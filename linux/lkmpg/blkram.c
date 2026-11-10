// SPDX-License-Identifier: GPL-2.0
/*
 * blkram.c - Tiny RAM-backed block device using blk-mq.
 *
 * Covers Linux 5.10 through 6.17+ by using version guards at the three
 * major block-layer API transitions:
 *
 *   5.10-5.14  alloc_disk() + blk_mq_init_queue() + blk_cleanup_queue()
 *   5.15-6.8   blk_mq_alloc_disk(set, queuedata) [2-arg macro]
 *   6.9+       blk_mq_alloc_disk(set, lim, queuedata) [3-arg macro]
 *
 * Teardown likewise varies: blk_cleanup_queue() was removed in 5.15,
 * blk_cleanup_disk() was removed in 5.18; modern kernels use
 * del_gendisk() + put_disk().
 */

#include <linux/blk-mq.h>
#include <linux/blkdev.h>
#include <linux/errno.h>
#include <linux/highmem.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/slab.h>
#include <linux/version.h>
#include <linux/vmalloc.h>

/* genhd.h was removed in 5.18; its content lives in blkdev.h since 5.17. */
#if LINUX_VERSION_CODE < KERNEL_VERSION(5, 18, 0)
#include <linux/genhd.h>
#endif

#define BLKRAM_SECTOR_SIZE 512
static unsigned long blkram_mb = 8;
module_param(blkram_mb, ulong, 0444);
MODULE_PARM_DESC(blkram_mb, "Size of the RAM disk in MB");

struct blkram_dev {
    struct blk_mq_tag_set tag_set;
    struct gendisk *disk;
    struct request_queue *queue;
    u8 *data;
    size_t size;
};

static struct blkram_dev *blkram;
static int blkram_major;

static blk_status_t blkram_transfer(struct blkram_dev *dev,
                                    struct request *rq) {
    struct req_iterator iter;
    struct bio_vec bvec;
    sector_t sector = blk_rq_pos(rq);
    unsigned long offset = (unsigned long)sector * BLKRAM_SECTOR_SIZE;

    rq_for_each_segment(bvec, rq, iter) {
        unsigned int len = bvec.bv_len;
        void *iobuf;

        if (offset + len > dev->size)
            return BLK_STS_IOERR;

/* kmap_local_page() appeared in 5.11; fall back to kmap_atomic() on 5.10. */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(5, 11, 0)
        iobuf = kmap_local_page(bvec.bv_page) + bvec.bv_offset;
#else
        iobuf = kmap_atomic(bvec.bv_page) + bvec.bv_offset;
#endif

        if (rq_data_dir(rq) == WRITE)
            memcpy(dev->data + offset, iobuf, len);
        else
            memcpy(iobuf, dev->data + offset, len);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(5, 11, 0)
        kunmap_local(iobuf);
#else
        kunmap_atomic(iobuf);
#endif
        offset += len;
    }

    return BLK_STS_OK;
}

static blk_status_t blkram_queue_rq(struct blk_mq_hw_ctx *hctx,
                                    const struct blk_mq_queue_data *bd) {
    struct request *rq = bd->rq;
    struct blkram_dev *dev = rq->q->queuedata;
    blk_status_t status;

    blk_mq_start_request(rq);

    if (blk_rq_is_passthrough(rq))
        status = BLK_STS_IOERR;
    else
        status = blkram_transfer(dev, rq);

    blk_mq_end_request(rq, status);

    return BLK_STS_OK;
}

static const struct blk_mq_ops blkram_mq_ops = {
    .queue_rq = blkram_queue_rq,
};

static const struct block_device_operations blkram_fops = {
    .owner = THIS_MODULE,
};

static int __init blkram_init(void) {
    unsigned long sectors;
    int ret;

    blkram_major = register_blkdev(0, "blkram");
    if (blkram_major < 0)
        return blkram_major;

    blkram = kzalloc(sizeof(*blkram), GFP_KERNEL);
    if (!blkram) {
        ret = -ENOMEM;
        goto err_unreg;
    }

    blkram->size = blkram_mb * 1024 * 1024;
    sectors = blkram->size / BLKRAM_SECTOR_SIZE;

    blkram->data = vzalloc(blkram->size);
    if (!blkram->data) {
        ret = -ENOMEM;
        goto err_free_dev;
    }

    blkram->tag_set.ops = &blkram_mq_ops;
    blkram->tag_set.nr_hw_queues = 1;
    blkram->tag_set.queue_depth = 64;
    blkram->tag_set.numa_node = NUMA_NO_NODE;
    blkram->tag_set.cmd_size = 0;
    /* BLK_MQ_F_SHOULD_MERGE was removed in 6.6+; merging is always on. */
#if LINUX_VERSION_CODE < KERNEL_VERSION(6, 6, 0)
    blkram->tag_set.flags = BLK_MQ_F_SHOULD_MERGE;
#else
    blkram->tag_set.flags = 0;
#endif
    blkram->tag_set.driver_data = blkram;

    ret = blk_mq_alloc_tag_set(&blkram->tag_set);
    if (ret)
        goto err_free_data;

/* Three eras of block-device creation:
 *   6.9+      blk_mq_alloc_disk(set, lim, queuedata)  -- 3-arg form.
 *   5.15-6.8  blk_mq_alloc_disk(set, queuedata)       -- 2-arg form.
 *   5.10-5.14 alloc_disk() + blk_mq_init_queue()      -- separate objects.
 */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(6, 9, 0)
    {
        struct queue_limits lim = {
            .logical_block_size = BLKRAM_SECTOR_SIZE,
        };
        blkram->disk = blk_mq_alloc_disk(&blkram->tag_set, &lim, blkram);
    }
    if (IS_ERR(blkram->disk)) {
        ret = PTR_ERR(blkram->disk);
        goto err_tag_set;
    }
    blkram->queue = blkram->disk->queue;
#elif LINUX_VERSION_CODE >= KERNEL_VERSION(5, 15, 0)
    blkram->disk = blk_mq_alloc_disk(&blkram->tag_set, blkram);
    if (IS_ERR(blkram->disk)) {
        ret = PTR_ERR(blkram->disk);
        goto err_tag_set;
    }
    blkram->queue = blkram->disk->queue;
#else
    blkram->queue = blk_mq_init_queue(&blkram->tag_set);
    if (IS_ERR(blkram->queue)) {
        ret = PTR_ERR(blkram->queue);
        goto err_tag_set;
    }

    blkram->disk = alloc_disk(1);
    if (!blkram->disk) {
        ret = -ENOMEM;
        goto err_cleanup_queue;
    }

    blkram->disk->queue = blkram->queue;
#endif

    blkram->queue->queuedata = blkram;
    blkram->disk->major = blkram_major;
    blkram->disk->first_minor = 0;
    blkram->disk->minors = 1;
    blkram->disk->fops = &blkram_fops;
    blkram->disk->private_data = blkram;

    snprintf(blkram->disk->disk_name, DISK_NAME_LEN, "blkram0");
#if LINUX_VERSION_CODE < KERNEL_VERSION(6, 9, 0)
    blk_queue_logical_block_size(blkram->queue, BLKRAM_SECTOR_SIZE);
#endif
    set_capacity(blkram->disk, sectors);

    /* add_disk() returns int since 5.16; earlier kernels return void. */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(5, 16, 0)
    ret = add_disk(blkram->disk);
    if (ret)
        goto err_put_disk;
#else
    add_disk(blkram->disk);
#endif

    pr_info("blkram: registered /dev/%s (%lu MiB)\n", blkram->disk->disk_name,
            blkram_mb);

    return 0;

#if LINUX_VERSION_CODE >= KERNEL_VERSION(5, 16, 0)
err_put_disk:
    put_disk(blkram->disk);
#endif
#if LINUX_VERSION_CODE < KERNEL_VERSION(5, 15, 0)
err_cleanup_queue:
    blk_cleanup_queue(blkram->queue);
#endif
err_tag_set:
    blk_mq_free_tag_set(&blkram->tag_set);
err_free_data:
    vfree(blkram->data);
err_free_dev:
    kfree(blkram);
err_unreg:
    unregister_blkdev(blkram_major, "blkram");
    return ret;
}

static void __exit blkram_exit(void) {
    del_gendisk(blkram->disk);
    put_disk(blkram->disk);
#if LINUX_VERSION_CODE < KERNEL_VERSION(5, 15, 0)
    blk_cleanup_queue(blkram->queue);
#endif
    blk_mq_free_tag_set(&blkram->tag_set);
    vfree(blkram->data);
    kfree(blkram);
    unregister_blkdev(blkram_major, "blkram");
}

module_init(blkram_init);
module_exit(blkram_exit);

MODULE_DESCRIPTION("LKMPG blk-mq RAM disk example");
MODULE_LICENSE("GPL");

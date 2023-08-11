#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/fs.h>  // the fops
#include <linux/init.h>
#include <linux/miscdevice.h>
#include <linux/mm.h>  // kvmalloc()
#include <linux/module.h>
#include <linux/sched.h>  // get_task_comm()
#include <linux/slab.h>	  // k[m|z]alloc(), k[z]free(), ...

// copy_[to|from]_user()
#include <linux/version.h>
#if LINUX_VERSION_CODE > KERNEL_VERSION(4, 11, 0)
#include <linux/uaccess.h>
#else
#include <asm/uaccess.h>
#endif

#include "../../convenient.h"

#define OURMODNAME "miscdrv_rdwr"
MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");
MODULE_VERSION("0.1");

struct drv_ctx {
	struct device *dev;
	int tx, rx, err, myword;
	u32 config1, config2;
	u64 config3;
#define MAXBYTES 128
	char oursecret[MAXBYTES];
};

// Use static to put context in a consistent memory location
static struct drv_ctx *ctx;

static int open_miscdrv_rdwr(struct inode *inode, struct file *filp) {
	struct device *dev = ctx->dev;
	char *buf = kzalloc(PATH_MAX, GFP_KERNEL);
	if (unlikely(!buf)) {
		return -ENOMEM;
	}
	PRINT_CTX();
	dev_info(dev, " opening \"%s\" now; wrt open file: f_flags = 0x%x\n",
			 file_path(filp, buf, PATH_MAX), filp->f_flags);
	kfree(buf);

	return nonseekable_open(inode, filp);
}

static ssize_t read_miscdrv_rdwr(struct file *filp, char __user *ubuf,
								 size_t count, loff_t *off)

{
	int ret = count;
	int secret_len = strnlen(ctx->oursecret, MAXBYTES);
	struct device *dev = ctx->dev;
	char tasknm[TASK_COMM_LEN];

	PRINT_CTX();
	dev_info(dev, "%s wants to read (upto) %zu bytes\n",
			 get_task_comm(tasknm, current), count);

	ret = -EINVAL;
	if (count < MAXBYTES) {
		dev_warn(dev,
				 "request # of bytes (%zu) is < required size"
				 " (%d), aborting read\n",
				 count, MAXBYTES);
		goto out_notok;
	}
	if (secret_len <= 0) {
		dev_warn(dev,
				 "whoops, something's wrong, the 'secret' isn't"
				 " available..; aborting read\n");
		goto out_notok;
	}

	ret = -EFAULT;
	// NOTE: This is the invoke point of the scheduler -> may go to sleep and
	// WAIT for system to finish IO
	if (copy_to_user(ubuf, ctx->oursecret, secret_len)) {
		dev_warn(dev, "copy_to_user() failed\n");
		goto out_notok;
	}

	ret = secret_len;
	// Update device stats
	ctx->tx += secret_len;
	dev_info(dev, " %d bytes read, returning... (stats: tx=%d, rx=%d)\n",
			 secret_len, ctx->tx, ctx->rx);

out_notok:
	return ret;
}

static ssize_t write_miscdrv_rdwr(struct file *filp, const char __user *ubuf,
								  size_t count, loff_t *off) {
	int ret = 0;
	void *kbuf = NULL;
	struct device *dev = ctx->dev;
	char tasknm[TASK_COMM_LEN];

	PRINT_CTX();
	if (unlikely(count > MAXBYTES)) { /* paranoia */
		dev_warn(dev,
				 "count %zu exceeds max # of bytes allowed, "
				 "aborting write\n",
				 count);
		goto out_nomem;
	}
	dev_info(dev, "%s wants to write %zu bytes\n",
			 get_task_comm(tasknm, current), count);

	ret = -ENOMEM;
	kbuf = kvmalloc(count, GFP_KERNEL);
	if (unlikely(!kbuf)) {
		goto out_nomem;
	}
	memset(kbuf, 0, count);

	ret = -EFAULT;
	// NOTE: This is the invoke point of the scheduler -> may go to sleep and
	// WAIT for system to finish IO
	if (copy_from_user(kbuf, ubuf, count)) {
		dev_warn(dev, "copy_from_user() failed\n");
		goto out_cfu;
	}

	strlcpy(ctx->oursecret, kbuf, (count > MAXBYTES ? MAXBYTES : count));

	ctx->rx += count;  // our 'receive' is wrt userspace

	ret = count;
	dev_info(dev, " %zu bytes written, returning... (stats: tx=%d, rx=%d)\n",
			 count, ctx->tx, ctx->rx);

out_cfu:
	kvfree(kbuf);
out_nomem:
	return ret;
}

static int close_miscdrv_rdwr(struct inode *inode, struct file *filp) {
	struct device *dev = ctx->dev;
	char *buf = kzalloc(PATH_MAX, GFP_KERNEL);
	if (unlikely(!buf)) {
		return -ENOMEM;
	}
	PRINT_CTX();
	dev_info(dev, " filename: \"%s\"\n", file_path(filp, buf, PATH_MAX));
	kfree(buf);
	return 0;
}

static const struct file_operations llkd_miscdev_fops = {
	.open = open_miscdrv_rdwr,
	.read = read_miscdrv_rdwr,
	.write = write_miscdrv_rdwr,
	.llseek = no_llseek,
	.release = close_miscdrv_rdwr,
};

static struct miscdevice llkd_miscdev = {
	.minor =
		MISC_DYNAMIC_MINOR,	 // Let kernel dynmaically allocate a monior number
	.name = "llkd_miscdev_rdwr",
	.mode = 0666,
	.fops = &llkd_miscdev_fops,
};

static int __init miscdrv_rdwr_init(void) {
	int ret = 0;
	pr_info("%s: inserted!\n", OURMODNAME);

	struct device *dev = NULL;
	ret = misc_register(&llkd_miscdev);
	if (ret) {
		pr_notice("%s: misc device registration failed, aborting\n",
				  OURMODNAME);
		return ret;
	}
	// Retrieve the device pointer for this device
	dev = llkd_miscdev.this_device;

	dev_info(dev,
			 "LLKD misc driver (major # 10) registered, minor# = %d,"
			 " dev node is /dev/%s\n",
			 llkd_miscdev.minor, llkd_miscdev.name);

	ctx = devm_kzalloc(dev, sizeof(struct drv_ctx), GFP_KERNEL);
	if (unlikely(!ctx)) {
		return -ENOMEM;
	}
	ctx->dev = dev;
	strlcpy(ctx->oursecret, "initmsg", 8);
	dev_dbg(ctx->dev, "A sample print via the dev_dbg(): driver initialized\n");

	return 0;
}

static void __exit miscdrv_rdwr_exit(void) {
	misc_deregister(&llkd_miscdev);
	pr_info("LLKD misc (rdwr) driver deregistered, bye\n");
}

module_init(miscdrv_rdwr_init);
module_exit(miscdrv_rdwr_exit);

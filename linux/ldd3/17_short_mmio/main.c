#include <linux/cdev.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/kernel.h>
#include <linux/list.h>
#include <linux/module.h>
#include <linux/slab.h>
#include <linux/uaccess.h>

#include <asm/io.h>

#include "main.h"

static int major = 0;

static unsigned long base = 0xFF000000;
unsigned long short_base = 0;
void __iomem *short_iomem;

int short_open(struct inode *inode, struct file *filp) { return 0; }

int short_release(struct inode *inode, struct file *filp) { return 0; }

ssize_t short_read(struct file *filp, char __user *buf, size_t count,
                   loff_t *f_pos) {
    char v;

    if (*f_pos > 0) {
        return 0;
    }

    v = ioread8(short_iomem);
    rmb();

    (*f_pos)++;

    if (copy_to_user(buf, &v, 1)) {
        return -EFAULT;
    }

    return 1;
}

ssize_t short_write(struct file *filp, const char __user *buf, size_t count,
                    loff_t *f_pos) {
    unsigned char *kbuf, *ptr;
    size_t cnt = count;

    kbuf = kmalloc(cnt, GFP_KERNEL);

    if (!kbuf)
        return -ENOMEM;
    if (copy_from_user(kbuf, buf, cnt))
        return -EFAULT;

    ptr = kbuf;

    while (cnt--) {
        pr_debug("mem=%p, val=%d(%#x)\n", short_iomem, *ptr, *ptr);
        iowrite8(*ptr++, short_iomem);
        wmb();
    }

    kfree(kbuf);

    return count;
}

static struct file_operations fops = {
    .owner = THIS_MODULE,
    .read = short_read,
    .write = short_write,
    .open = short_open,
    .release = short_release,
};

static int __init m_init(void) {
    int result = 0;
    short_base = base;

    pr_err("base = %#lx\n", short_base);
    if (!request_mem_region(short_base, 0x10, "short")) {
        pr_err("short_mmio: cannot get I/O mem address %#lx\n", short_base);
        result = -ENODEV;
        goto out;
    }
    // Call ioremap to make the allocated region accessible by the kernel
    short_iomem = ioremap(short_base, 0x10);
    pr_info("ioremap %#lx\n", (unsigned long)short_iomem);

    result = register_chrdev(major, MODULE_NAME, &fops);
    if (result < 0) {
        pr_err("cannot get major number!\n");
        goto unreg_region;
    }
    major = (major == 0) ? result : major;

    return 0;

unreg_region:
    iounmap(short_iomem);
    release_mem_region(short_base, 0x10);

out:
    return result;
}

static void __exit m_exit(void) {
    unregister_chrdev(major, MODULE_NAME);
    iounmap(short_iomem);
    release_mem_region(short_base, 0x10);
}

module_init(m_init);
module_exit(m_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("zuoxiang");
MODULE_DESCRIPTION("Basic mmio manipulation");

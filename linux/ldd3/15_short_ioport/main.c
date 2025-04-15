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

static unsigned long base = 0x200;
unsigned long short_base = 0;

int short_open(struct inode *inode, struct file *filp) { return 0; }

int short_release(struct inode *inode, struct file *filp) { return 0; }

ssize_t short_read(struct file *filp, char __user *buf, size_t count,
                   loff_t *f_pos) {
    unsigned long port = short_base;
    char v;

    if (*f_pos > 0) {
        return 0;
    }

    v = inb(port);
    rmb();

    (*f_pos)++;

    if (copy_to_user(buf, &v, 1)) {
        return -EFAULT;
    }

    return 1;
}

ssize_t short_write(struct file *filp, const char __user *buf, size_t count,
                    loff_t *f_pos) {
    unsigned long port = short_base;
    unsigned char *kbuf, *ptr;
    size_t cnt = count;

    kbuf = kmalloc(cnt, GFP_KERNEL);

    if (!kbuf)
        return -ENOMEM;
    if (copy_from_user(kbuf, buf, cnt))
        return -EFAULT;

    ptr = kbuf;

    while (cnt--) {
        pr_debug("port=%lu(%#lx), val=%d(%#x)\n", port, port, *ptr, *ptr);
        outb(*(ptr++), port);
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

    if (!request_region(short_base, SHORT_NR_PORTS, "short")) {
        pr_err("short_ioport: cannot get I/O port address %#lx\n", short_base);
        result = -ENODEV;
        goto out;
    }

    result = register_chrdev(major, MODULE_NAME, &fops);
    if (result < 0) {
        pr_err("cannot get major number!\n");
        goto unreg_region;
    }
    major = (major == 0) ? result : major;

    return 0;

unreg_region:
    release_region(short_base, SHORT_NR_PORTS);

out:
    return result;
}

static void __exit m_exit(void) {
    unregister_chrdev(major, MODULE_NAME);
    release_region(short_base, SHORT_NR_PORTS);
}

module_init(m_init);
module_exit(m_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("zuoxiang");
MODULE_DESCRIPTION("Basic io port manipulation based on Parallel Port");

#include <linux/cdev.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/platform_device.h> /* For platform devices */

MODULE_LICENSE("GPL");

static unsigned int major;
static struct class *dummy_class;
static struct cdev dummy_cdev;

int dummy_open(struct inode *inode, struct file *filp) {
    pr_info("Someone tried to open me\n");
    return 0;
}

int dummy_release(struct inode *inode, struct file *filp) {
    pr_info("Someone closed me\n");
    return 0;
}

ssize_t dummy_read(struct file *filp, char __user *buf, size_t count,
                   loff_t *offset) {
    pr_info("Nothing to read guy\n");
    return 0;
}

ssize_t dummy_write(struct file *filp, const char __user *buf, size_t count,
                    loff_t *offset) {
    pr_info("Can not accept any data guy\n");
    return count;
}

struct file_operations dummy_fops = {
    open : dummy_open,
    release : dummy_release,
    read : dummy_read,
    write : dummy_write,
};

static int my_pdrv_probe(struct platform_device *pdev) {
    struct device *dummy_device = NULL;
    int error;
    dev_t devt = 0;

    error = alloc_chrdev_region(&devt, 0, 1, "dummy_char");
    if (error < 0) {
        pr_err("Can not allocate dummy_char dev");
        return error;
    }
    major = MAJOR(devt);
    pr_info("dummy_char major number = %d\n", major);

    dummy_class = class_create("dummy_char_class");
    if (IS_ERR(dummy_class)) {
        pr_err("Error creating sdma test module class.\n");
        unregister_chrdev_region(MKDEV(major, 0), 1);
        return PTR_ERR(dummy_class);
    }

    cdev_init(&dummy_cdev, &dummy_fops);
    dummy_cdev.owner = THIS_MODULE;
    cdev_add(&dummy_cdev, devt, 1);

    dummy_device =
        device_create(dummy_class, &pdev->dev, devt, NULL, "dummy_char");
    if (IS_ERR(dummy_device)) {
        pr_err("Error creating sdma test class device.\n");
        class_destroy(dummy_class);
        unregister_chrdev_region(devt, 1);
        return -1;
    }

    pr_info("dummy char module loaded\n");
    return 0;
}

static int my_pdrv_remove(struct platform_device *pdev) {
    unregister_chrdev_region(MKDEV(major, 0), 1);
    device_destroy(dummy_class, MKDEV(major, 0));
    cdev_del(&dummy_cdev);
    class_destroy(dummy_class);

    pr_info("dummy char module unloaded.\n");
    return 0;
}

static struct platform_driver mypdrv = {
    .probe = my_pdrv_probe,
    .remove = my_pdrv_remove,
    .driver =
        {
            .name = "platform-dummy-char",
            .owner = THIS_MODULE,
        },
};

module_platform_driver(mypdrv);
MODULE_AUTHOR("Zuo Xiang");
MODULE_LICENSE("GPL");

#include <linux/cdev.h>
#include <linux/device.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/module.h>
#include <linux/version.h>

MODULE_DESCRIPTION("Dummy character driver");
MODULE_AUTHOR("Zuo Xiang <xianglinks@gmail.com>");
MODULE_LICENSE("GPL");

#define DUMMY_MAGIC_NUMBER 117

static unsigned int major; /* major number for device */
static struct class *dummy_class;
static struct cdev dummy_cdev;

// Per-device private data
// - magic_num: Just the magic data to copied from kernel to user space
// - op_counter: Used as a hard limitation of number of operations
struct dummy_private_data {
	struct cdev cdev;
	int magic_num;
	int op_counter;
};

// - inode: Refers to a file on the disk
// - filp: Refers to an open file with associated file descriptor
int dummy_open(struct inode *inode, struct file *filp) {
	struct dummy_private_data *pd = NULL;

	pd = container_of(inode->i_cdev, struct dummy_private_data, cdev);
	pd->magic_num = DUMMY_MAGIC_NUMBER;
	pd->op_counter = 0;
	filp->private_data = pd;

	pr_info("Someone tried to open me\n");

	return 0;
}

int dummy_release(struct inode *inode, struct file *filp) {
	filp->private_data = NULL;

	pr_info("Someone closed me\n");

	return 0;
}

/**
 * The read operation handler
 * In this dummy read operation, the device will return the magic_num in dummy_private_data only once.
 *
 * @param filp 
 * @param __user 
 * @param count 
 * @param offset 
 * @return 
 */
ssize_t dummy_read(struct file *filp, char __user *buf, size_t count,
				   loff_t *offset) {
	struct dummy_private_data *pd = NULL;

	if (*offset > DUMMY_MAGIC_NUMBER) {
		return 0;
	}
	if (*offset + count > DUMMY_MAGIC_NUMBER) {
		count = DUMMY_MAGIC_NUMBER - (*offset);
	}

	pd = filp->private_data;
	if (pd->op_counter < 1) {
		if (copy_to_user(buf, &(pd->magic_num), sizeof(int)) != 0) {
			return -EIO;
		}
		pd->op_counter += 1;
		pr_info("This is just a dummy read...\n");
		return sizeof(int);
	} else {
		pr_info("This is just a dummy read...\n");
		return 0;
	}
}

// It's not clear to me why count uses size_t, but the offset is passed with an
// pointer?
ssize_t dummy_write(struct file *filp, const char __user *buf, size_t count,
					loff_t *offset) {
	pr_info("This is just a dummy write...\n");

	// Write beyond the end of the file.
	if (*offset > DUMMY_MAGIC_NUMBER) {
		return -EINVAL;
	}
	if (*offset + count > DUMMY_MAGIC_NUMBER) {
		count = DUMMY_MAGIC_NUMBER - *offset;
	}

	return count;
}

// Register handlers for file operations
struct file_operations dummy_fops = {
	.open = dummy_open,
	.release = dummy_release,
	.read = dummy_read,
	.write = dummy_write,
};

static int __init dummy_char_init_module(void) {
	struct device *dummy_device;
	int error;
	dev_t devt = 0;

	/* Get a range of minor numbers (starting with 0) to work with */
	error = alloc_chrdev_region(&devt, 0, 1, "dummy_char");
	if (error < 0) {
		pr_err("Can't get major number\n");
		return error;
	}
	major = MAJOR(devt);
	pr_info("dummy_char major number = %d\n", major);

	/* Create device class, visible in /sys/class */
	dummy_class = class_create(THIS_MODULE, "dummy_char_class");
	if (IS_ERR(dummy_class)) {
		pr_err("Error creating dummy char class.\n");
		unregister_chrdev_region(MKDEV(major, 0), 1);
		return PTR_ERR(dummy_class);
	}

	/* Initialize the char device and tie a file_operations to it */
	cdev_init(&dummy_cdev, &dummy_fops);
	dummy_cdev.owner = THIS_MODULE;
	/* Now make the device live for the users to access */
	cdev_add(&dummy_cdev, devt,
			 1);  // Only 1 consecuitive minor number to this device.

	// Make the device physically visible under /dev directory with the given
	// name !!!
	dummy_device = device_create(dummy_class, NULL, /* no parent device */
								 devt,				/* associated dev_t */
								 NULL,				/* no additional data */
								 "dummy_char");		/* device name */

	if (IS_ERR(dummy_device)) {
		pr_err("Error creating dummy char device.\n");
		class_destroy(dummy_class);
		unregister_chrdev_region(devt, 1);
		return -1;
	}

	pr_info("dummy char module loaded\n");
	return 0;
}

static void __exit dummy_char_cleanup_module(void) {
	unregister_chrdev_region(MKDEV(major, 0), 1);
	device_destroy(dummy_class, MKDEV(major, 0));
	cdev_del(&dummy_cdev);
	class_destroy(dummy_class);

	pr_info("dummy char module Unloaded\n");
}

module_init(dummy_char_init_module);
module_exit(dummy_char_cleanup_module);

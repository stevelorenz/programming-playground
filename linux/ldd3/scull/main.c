#include <linux/init.h>
#include <linux/module.h>
#include <linux/moduleparam.h>

#include "scull.h"

MODULE_AUTHOR("Alessandro Rubini, Jonathan Corbet");
MODULE_LICENSE("Dual BSD/GPL");

int scull_major = SCULL_MAJOR;
int scull_minor = 0;
int scrull_nr_devs = SCULL_NR_DEVS;

struct file_operations scull_fops = {
	.owner = NULL,
	.read = NULL,
	.write = NULL,
	.open = NULL,
	.release = NULL,
};

static int scull_init_module(void) {
	int result;
	dev_t dev = 0;

	if (scull_major) {
		dev = MKDEV(scull_major, scull_minor);
		result = register_chrdev_region(dev, scrull_nr_devs, "scull");
	} else {
		result =
			alloc_chrdev_region(&dev, scull_minor, scrull_nr_devs, "scull");
		scull_major = MAJOR(dev);
	}

	if (result < 0) {
		printk(KERN_WARNING "scull: can't get major %d\n", scull_major);
		return result;
	}

	return 0;
}

void scull_cleanup_module(void) {}

module_init(scull_init_module);
module_exit(scull_cleanup_module);

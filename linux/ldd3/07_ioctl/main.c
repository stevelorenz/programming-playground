#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

#include <linux/cdev.h>
#include <linux/fs.h>
#include <linux/slab.h>
#include <linux/uaccess.h>

#include "main.h"

MODULE_AUTHOR("zuoxiang");
MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("An ioctl() example");

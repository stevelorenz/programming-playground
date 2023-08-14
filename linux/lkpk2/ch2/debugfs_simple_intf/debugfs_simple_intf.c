#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/debugfs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/version.h>
#if LINUX_VERSION_CODE >= KERNEL_VERSION(4, 11, 0)
#include <linux/sched/signal.h>
#include <linux/uaccess.h>
#else
#include <asm/uaccess.h>
#endif
#include "../../convenient.h"

#define OURMODNAME "dbgfs_simple_intf"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");
MODULE_VERSION("0.1");



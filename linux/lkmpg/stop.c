/*
 * stop.c - Illustration of multi filed modules
 */

#include <linux/module.h> /* Specifically, a module  */
#include <linux/printk.h>

void cleanup_module(void) { pr_info("Short is the life of a kernel module\n"); }

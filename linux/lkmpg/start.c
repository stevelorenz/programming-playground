/*
 * start.c - Illustration of multi filed modules
 */

#include <linux/module.h> /* Specifically, a module */
#include <linux/printk.h>

int init_module(void) {
    pr_info("Hello, world - this is the kernel speaking\n");
    return 0;
}

MODULE_LICENSE("GPL");

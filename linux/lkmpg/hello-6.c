/*
 * hello-6.c - Demonstrates module_param_cb() callbacks.
 */
#include <linux/init.h>
#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/printk.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("LKMPG");
MODULE_DESCRIPTION("A module_param_cb() example");

static int watched = 42;

static int watched_set(const char *val, const struct kernel_param *kp) {
    int ret;

    ret = param_set_int(val, kp);
    if (ret) {
        return ret;
    }

    pr_info("watched updated to %d\n", watched);

    return 0;
}

static int watched_get(char *buffer, const struct kernel_param *kp) {
    int ret;

    ret = param_get_int(buffer, kp);

    if (ret >= 0) {
        pr_info("watched was read\n");
    }
    return ret;
}

static const struct kernel_param_ops watched_ops = {
    .set = watched_set,
    .get = watched_get,
};

module_param_cb(watched, &watched_ops, &watched, 0644);
MODULE_PARM_DESC(watched, "An integer that logs every update");

static int __init hello_6_init(void) {
    pr_info("Hello, world 6\n");
    pr_info("watched starts at %d\n", watched);
    return 0;
}

static void __exit hello_6_exit(void) { pr_info("Goodbye, world 6\n"); }

module_init(hello_6_init);
module_exit(hello_6_exit);

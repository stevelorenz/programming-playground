#include <linux/init.h>
#include <linux/module.h>

MODULE_LICENSE("Dual BSD/GPL");

static int hello_init(void) {
    printk(KERN_ALERT "Hello world from zuo xiang!\n");
    return 0;
}

static void hello_exit(void) {
    printk(KERN_ALERT "Goodbye, world from zuoxiang.\n");
}

module_init(hello_init);
module_exit(hello_exit);

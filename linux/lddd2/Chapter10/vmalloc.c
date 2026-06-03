#include <linux/init.h>
#include <linux/module.h>
#include <linux/vmalloc.h>

static void *ptr = NULL;

static int my_vmalloc_init(void) {
    unsigned long size = 8192;
    ptr = vmalloc(size);

    if (!ptr) {
        pr_err("Memory allocation failed\n");
        return -ENOMEM;
    } else {
        pr_info("Memory allocated OK\n");
    }

    return 0;
}

static void my_vmalloc_exit(void) {
    vfree(ptr);
    pr_info("Memory freed\n");
}

module_init(my_vmalloc_init);
module_exit(my_vmalloc_exit);
MODULE_LICENSE("GPL");
MODULE_AUTHOR("john Madieu <john.madieu@gmail.com>");

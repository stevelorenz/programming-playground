#include <linux/init.h>
#include <linux/mm.h>
#include <linux/module.h>
#include <linux/slab.h>

static void *ptr;

static int alloc_init(void) {
    size_t size = 1024;
    ptr = kmalloc(size, GFP_KERNEL);

    if (!ptr) {
        pr_err("Failed to allocate memory\n");
        return -ENOMEM;
    } else {
        pr_info("Memory allocated fine\n");
    }

    return 0;
}

static void alloc_exit(void) {
    kfree(ptr);
    pr_info("Memory freed\n");
}

module_init(alloc_init);
module_exit(alloc_exit);
MODULE_LICENSE("GPL");
MODULE_AUTHOR("John Madieu <john.madieu@gmail.com>");

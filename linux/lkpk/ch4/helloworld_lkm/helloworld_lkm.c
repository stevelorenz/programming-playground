#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("LKP book:ch4/helloworld_lkm: hello, world, our first LKM");
MODULE_LICENSE("Dual MIT/GPL");
MODULE_VERSION("0.1");

static int __init helloworld_lkm_init(void) {
	pr_info("%s():%d: Hello World\n", __func__, __LINE__);
	return 0;
}

static void __exit helloworld_lkm_exit(void) {
    pr_info("Goodbye, world\n");
}

module_init(helloworld_lkm_init);
module_exit(helloworld_lkm_exit);

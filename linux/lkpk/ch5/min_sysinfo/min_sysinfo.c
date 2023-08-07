#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

#define OURMODNAME "min_sysinfo"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("Example to get system information");
MODULE_LICENSE("GPL");
MODULE_VERSION("0.1");

void sysinfo(void) {
    char msg[128];
	memset(msg, 0, strlen(msg));
    snprintf(msg, 48, "%s(): minimal Platform Info:\nCPU: ", __func__);

    #ifdef CONFIG_X86
    #if (BITS_PER_LONG == 32)
    	strncat(msg, "x86_32, ", 9);
    #else
    	strncat(msg, "x86_64, ", 9);
    #endif
    #endif
    
    #ifdef CONFIG_ARM64
        strncat(msg, "AArch64, ", 10);
    #endif

    #ifdef __BIG_ENDIAN
    	strncat(msg, "big-endian; ", 13);
    #else
    	strncat(msg, "little-endian; ", 16);
    #endif

    pr_info("%s", msg);

}
EXPORT_SYMBOL(sysinfo);

static int __init min_sys_init(void) {
	pr_info("%s installed\n", OURMODNAME);
    sysinfo();
	return 0;
}

static void __exit min_sys_exit(void) { pr_info("%s removed\n", OURMODNAME); }

module_init(min_sys_init);
module_exit(min_sys_exit);

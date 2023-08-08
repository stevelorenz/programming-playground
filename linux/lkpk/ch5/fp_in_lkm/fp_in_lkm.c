#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>
// This is NOT available for ARM CPUs on Linux 5.15.123
#include <asm/fpu/api.h>

#define MODNAME "fp_in_lkm"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION(
	"Simple example to show performing FP (Float Point) arithemetic in kernel "
	"mode");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

static double num = 22.0;
static double den = 7.0;
static double mypi;

static int __init fp_in_lkm_init(void) {
	pr_info("inserted\n");

    kernel_fpu_begin();
	mypi = num / den;
	kernel_fpu_end();

	return 0;
}

static void __exit fp_in_lkm_exit(void) { pr_info("removed\n"); }

module_init(fp_in_lkm_init);
module_exit(fp_in_lkm_exit);

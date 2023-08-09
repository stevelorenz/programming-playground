#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/module.h>

#include "../../klib_llkd.h"

#define MODNAME "lowlevel_mem"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

static const void *gptr1, *gptr2, *gptr3, *gptr4, *gptr5;
static int bsa_alloc_order = 3;
module_param_named(order, bsa_alloc_order, int, 0660);
MODULE_PARM_DESC(order, "order of the allocation (power-to-raise-2-to)");

static int bsa_alloc(void) {
	int stat = -ENOMEM;
	u64 numpg2alloc = 0;
	const struct page *pg_ptr1;

	pr_info(
		"%s: 0. Show identity mapping: RAM page frames : kernel virtual pages "
		":: 1:1\n",
		MODNAME);
	show_phy_pages((void *)PAGE_OFFSET, 5 * PAGE_SIZE, 1);

	// Allocate a single page
	gptr1 = (void *)__get_free_page(GFP_KERNEL);
	if (!gptr1) {
		pr_warn("%s: __get_free_page failed!\n", MODNAME);
		goto out1;
	}
	pr_info(
		"%s: 1. __get_free_page() alloc'ed 1 page from the BSA @ %pK (%px)\n",
		MODNAME, gptr1, gptr1);

	// Allocate 2^bsa_alloc_order pages

	numpg2alloc = powerof(2, bsa_alloc_order);
	gptr2 = (void *)__get_free_pages(GFP_KERNEL | __GFP_ZERO, bsa_alloc_order);
	if (!gptr2) {
		/* no error/warning printk now; see above comment */
		goto out2;
	}
	pr_info(
		"%s: 2. __get_free_pages() alloc'ed 2^%d = %lld page(s) = %lld bytes\n"
		" from the BSA @ %pK (%px)\n",
		MODNAME, bsa_alloc_order, powerof(2, bsa_alloc_order),
		numpg2alloc * PAGE_SIZE, gptr2, gptr2);
	pr_info(" (PAGE_SIZE = %ld bytes)\n", PAGE_SIZE);
	show_phy_pages(gptr2, numpg2alloc * PAGE_SIZE, 1);

out2:
	free_page(gptr1);

out1:
	return stat;
}

static int __init lowlevel_mem_init(void) {
	pr_info("inserted\n");
	bsa_alloc();
	return 0;
}

static void __exit lowlevel_mem_exit(void) {
	pr_info("gptr1: %pK", gptr1);
	free_pages(gptr2, bsa_alloc_order);
	free_page(gptr1);
	pr_info("removed\n");
}

module_init(lowlevel_mem_init);
module_exit(lowlevel_mem_exit);

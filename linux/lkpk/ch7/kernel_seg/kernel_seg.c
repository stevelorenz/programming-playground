#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <asm/fixmap.h>
#include <asm/pgtable.h>
#include <linux/highmem.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/version.h>
#include <linux/vmalloc.h>

#include "../../convenient.h"

#define MODNAME "kernel_seg"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

#define ELLPS \
	"|                           [ . . . ]                         |\n"

static void show_kernelseg_info(void) {
	pr_info(
		"\nSome Kernel Details [by decreasing address]\n"
		"+-------------------------------------------------------------+\n");
	/* kernel fixmap region */
	pr_info(ELLPS
			"|fixmap region:      "
			" %px - %px     | [%4ld MB]\n",
#ifdef CONFIG_ARM
			SHOW_DELTA_M(FIXADDR_START, FIXADDR_END));
#else
			SHOW_DELTA_M(FIXADDR_START, (FIXADDR_START + FIXADDR_SIZE)));
#endif
	/* kernel module region
	 * For the modules region, it's high in the kernel segment on typical 64-bit
	 * systems, but the other way around on many 32-bit systems (particularly
	 * ARM-32); so we rearrange the order in which it's shown depending on the
	 * arch, thus trying to maintain a 'by descending address' ordering.
	 */
#if (BITS_PER_LONG == 64)
	pr_info(
		"|module region:      "
		" %px - %px     | [%ld MB]\n",
		SHOW_DELTA_M(MODULES_VADDR, MODULES_END));
#endif

#ifdef CONFIG_KASAN	 // KASAN region: Kernel Address SANitizer
	pr_info(
		"|KASAN shadow:       "
		" %px - %px     | [%2ld GB]\n",
		SHOW_DELTA_G(KASAN_SHADOW_START, KASAN_SHADOW_END));
#endif

	/* vmalloc region */
	pr_info(
		"|vmalloc region:     "
		" %px - %px     | [%4ld MB = %2ld GB]\n",
		SHOW_DELTA_MG(VMALLOC_START, VMALLOC_END));

	/* lowmem region */
	pr_info(
		"|lowmem region:      "
		" %px - %px     | [%4ld MB = %2ld GB]\n"
#if (BITS_PER_LONG == 32)
		"|           (above:PAGE_OFFSET - highmem)                     |\n",
#else
		"|                (above:PAGE_OFFSET    -      highmem)        |\n",
#endif
		SHOW_DELTA_MG((unsigned long)PAGE_OFFSET, (unsigned long)high_memory));

	/* (possible) highmem region;  may be present on some 32-bit systems */
#ifdef CONFIG_HIGHMEM
	pr_info(
		"|HIGHMEM region:     "
		" %px - %px | [%4ld MB]\n",
		SHOW_DELTA_M(PKMAP_BASE, (PKMAP_BASE) + (LAST_PKMAP * PAGE_SIZE)));
#endif

	/*
	 * Symbols for kernel:
	 *   text begin/end (_text/_etext)
	 *   init begin/end (__init_begin, __init_end)
	 *   data begin/end (_sdata, _edata)
	 *   bss begin/end (__bss_start, __bss_stop)
	 * are only defined *within* (in-tree) and aren't available for modules;
	 * thus we don't attempt to print them.
	 */

#if (BITS_PER_LONG == 32) /* modules region: see the comment above reg this */
	pr_info(
		"|module region:      "
		" %px - %px | [%4ld MB]\n",
		SHOW_DELTA_M(MODULES_VADDR, MODULES_END));
#endif
	pr_info(ELLPS);
}

static int __init kernel_seg_init(void) {
	pr_info("inserted\n");
	show_kernelseg_info();
	return 0;
}

static void __exit kernel_seg_exit(void) { pr_info("removed\n"); }

module_init(kernel_seg_init);
module_exit(kernel_seg_exit);

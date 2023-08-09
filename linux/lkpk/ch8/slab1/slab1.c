#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/slab.h>

#define MODNAME "slab1"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

static char* gkptr = NULL;

struct myctx {
	u32 iarr[100];
	u64 uarr[100];
	char uname[128], passwd[16], config[16];
};
static struct myctx* ctx;

static int __init slab1_init(void) {
	pr_info("inserted\n");
	gkptr = kmalloc(1024, GFP_KERNEL);
	if (!gkptr) {
		WARN_ONCE(1, "%s: kmalloc() failed!\n", MODNAME);
		/* As mentioned earlier, there is really no need to print an
		 * error msg when a memory alloc fails; the situation
		 * "shouldn't" typically occur, and if it does, the kernel will
		 * emit a chain of messages in any case. Here, we use the WARN_ONCE()
		 * macro pedantically, and as this is a 'learning' program..
		 */
		goto out_fail1;
	}
	pr_info("kmalloc() succeeds, (actual KVA) ret value = %px\n", gkptr);
	/* We use the %px format specifier here to show the actual KVA; in
	 * production, Don't! */
	print_hex_dump_bytes("gkptr before memset: ", DUMP_PREFIX_OFFSET, gkptr,
						 32);
	memset(gkptr, 'm', 1024);
	print_hex_dump_bytes(" gkptr after memset: ", DUMP_PREFIX_OFFSET, gkptr,
						 32);

	ctx = kzalloc(sizeof(struct myctx), GFP_KERNEL);
	if (!ctx) {
		goto out_fail2;
	}

	pr_info(
		"%s: context struct alloc'ed and initialized (actual KVA ret = %px)\n",
		MODNAME, ctx);
	print_hex_dump_bytes("ctx: ", DUMP_PREFIX_OFFSET, ctx, 32);

	return 0;

out_fail2:
	kfree(gkptr);

out_fail1:
	return -ENOMEM;
}

static void __exit slab1_exit(void) {
	kfree(ctx);
	kfree(gkptr);
	pr_info("removed\n");
}

module_init(slab1_init);
module_exit(slab1_exit);

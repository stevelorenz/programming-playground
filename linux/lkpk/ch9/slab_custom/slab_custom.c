#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/version.h>

#define OURMODNAME "slab_custom"
#define OURCACHENAME "our_ctx"

static int use_ctor = 1;
module_param(use_ctor, uint, 0);
MODULE_PARM_DESC(
	use_ctor,
	"if set to 1 (default), our custom ctor routine"
	" will initialize slabmem; when 0, no custom constructor will run");

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

struct myctx {
	u32 iarr[10];
	u64 uarr[10];
	char uname[128], passwd[16], config[64];
};
static struct kmem_cache *gctx_cachep;

static int user_our_cache(void) {
	struct myctx *obj = NULL;

	obj = kmem_cache_alloc(gctx_cachep, GFP_KERNEL);
	if (!obj) {
		pr_warn("[Pedantic] kmem_cache_alloc failed!\n");
		return -ENOMEM;
	}

	pr_info(
		"Our cache object (@ %pK, actual=%px) size is %u bytes; actual "
		"ksize=%zu\n",
		obj, obj, kmem_cache_size(gctx_cachep), ksize(obj));
	print_hex_dump_bytes("obj: ", DUMP_PREFIX_OFFSET, obj,
						 sizeof(struct myctx));

	kmem_cache_free(gctx_cachep, obj);
    return 0;
}

static void our_ctor(void *new) {
	struct myctx *ctx = new;
	struct task_struct *p = current;

	pr_info("in ctor: just alloced mem object is @ 0x%px\n",
			ctx); /* %pK in production */
	memset(ctx, 0, sizeof(struct myctx));

	snprintf(ctx->config, 6 * sizeof(u64) + 5, "%d.%d,%ld.%ld,%ld,%ld", p->tgid,
			 p->pid, p->nvcsw, p->nivcsw, p->min_flt, p->maj_flt);
}

static int create_our_cache(void) {
	int ret = 0;
	void *ctor_fn = NULL;

	if (use_ctor == 1) {
		ctor_fn = our_ctor;
	};

	pr_info(
		"sizeof our ctx structure is %zu bytes\n"
		" using custom constructor routine? %s\n",
		sizeof(struct myctx), use_ctor == 1 ? "yes" : "no");

	gctx_cachep = kmem_cache_create(
		OURCACHENAME, sizeof(struct myctx), sizeof(long),
		SLAB_POISON | SLAB_RED_ZONE | SLAB_HWCACHE_ALIGN, ctor_fn);

	if (!gctx_cachep) {
		// Convert between pointer and error message
		if (IS_ERR(gctx_cachep)) {
			ret = PTR_ERR(gctx_cachep);
		}
	}

	return ret;
}

static int __init slab_custom_init(void) {
	pr_info("inserted\n");
	create_our_cache();
	return user_our_cache();
}

static void __exit slab_custom_exit(void) {
	kmem_cache_destroy(gctx_cachep);
	pr_info("removed\n");
}

module_init(slab_custom_init);
module_exit(slab_custom_exit);

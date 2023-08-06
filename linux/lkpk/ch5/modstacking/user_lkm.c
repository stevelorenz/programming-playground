#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>

#define MODNAME "user_lkm"
MODULE_LICENSE("GPL");

extern void llkd_sysinfo2(void);
extern long get_skey(int);
extern int exp_int;

static int __init user_lkm_init(void)
{
#define THE_ONE 0xfedface
    pr_info("inserted\n");
    u64 sk = get_skey(THE_ONE);
    pr_info("Called get_key(), ret = 0x%llx = %llu\n", sk, sk);
    pr_info("exp_int = %d\n", exp_int);

    llkd_sysinfo2();
    return 0;
}

static void __exit user_lkm_exit(void)
{
    pr_info("removed\n");
}

module_init(user_lkm_init);
module_exit(user_lkm_exit);

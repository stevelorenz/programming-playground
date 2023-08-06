/*
 * ch5/core_lkm/core_lkm.c
 */

#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

#define OURMODNAME "core_lkm"
#define THE_ONE 0xfedface

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

int exp_int = 200;
EXPORT_SYMBOL_GPL(exp_int);

/* llkd_sysinfo2 is expected to be called from other LKMs */
void llkd_sysinfo2(void) {
#define MSGLEN 128
	char msg[MSGLEN];

	memset(msg, 0, MSGLEN);
	snprintf(msg, 48, "%s(): minimal Platform Info:\nCPU: ", __func__);

	/* Strictly speaking, all this #if... is considered ugly and should be
	 * isolated as far as is possible
	 */
#ifdef CONFIG_X86
#if (BITS_PER_LONG == 32)
	strlcat(msg, "x86_32, ", MSGLEN);
#else
	strlcat(msg, "x86_64, ", MSGLEN);
#endif
#endif
#ifdef CONFIG_ARM
	strlcat(msg, "ARM-32, ", MSGLEN);
#endif
#ifdef CONFIG_ARM64
	strlcat(msg, "Aarch64, ", MSGLEN);
#endif
#ifdef CONFIG_MIPS
	strlcat(msg, "MIPS, ", MSGLEN);
#endif
#ifdef CONFIG_PPC
	strlcat(msg, "PowerPC, ", MSGLEN);
#endif
#ifdef CONFIG_S390
	strlcat(msg, "IBM S390, ", MSGLEN);
#endif

#ifdef __BIG_ENDIAN
	strlcat(msg, "big-endian; ", MSGLEN);
#else
	strlcat(msg, "little-endian; ", MSGLEN);
#endif

#if (BITS_PER_LONG == 32)
	strlcat(msg, "32-bit OS.\n", MSGLEN);
#elif (BITS_PER_LONG == 64)
	strlcat(msg, "64-bit OS.\n", MSGLEN);
#endif
	pr_info("%s", msg);
}
EXPORT_SYMBOL(llkd_sysinfo2);


#if (BITS_PER_LONG == 32)
u32 get_skey(int p)
#else				// 64-bit
u64 get_skey(int p)
#endif
{
#if (BITS_PER_LONG == 32)
	u32 secret = 0x567def;
#else				// 64-bit
	u64 secret = 0x123abc567def;
#endif
    pr_info("%s:%d: I've been called\n", __FILE__, __LINE__);
    if (p == THE_ONE ) {
        return secret;
    }
    return 0;
}
EXPORT_SYMBOL(get_skey);

static int __init core_lkm_init(void) {
	pr_info("inserted\n");
	return 0;
}

static void __exit core_lkm_exit(void) { pr_info("removed\n"); }

module_init(core_lkm_init);
module_exit(core_lkm_exit);

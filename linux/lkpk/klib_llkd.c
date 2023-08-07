#include "klib_llkd.h"

void llkd_minsysinfo(void) {
#define MSGLEN 128
	char msg[MSGLEN];

	memset(msg, 0, MSGLEN);
	snprintf(msg, 48, "%s(): minimal platform info:\nCPU: ", __func__);

	/* Strictly speaking, all this #if... is considered ugly and should be
	   isolated as far as is possible */
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

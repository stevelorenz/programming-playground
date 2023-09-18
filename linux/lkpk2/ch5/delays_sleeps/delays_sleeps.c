
#include <linux/delay.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/ktime.h>  // ktime_get_*() routines
#include <linux/module.h>

#define OURMODNAME "delays_sleeps"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

#define DILLY_DALLY(code_str, run_this)                                  \
	do {                                                                 \
		u64 t1, t2;                                                      \
		t1 = ktime_get_real_ns();                                        \
		run_this;                                                        \
		t2 = ktime_get_real_ns();                                        \
		pr_info(code_str "-> actual: %11llu ns = %7llu us = %4llu ms\n", \
				(t2 - t1), (t2 - t1) / 1000, (t2 - t1) / 1000000);       \
	} while (0)

static int __init delays_sleeps_init(void) {
	pr_info("%s: inserted\n", OURMODNAME);

	// Atomic busy-loops, no sleep
	DILLY_DALLY("ndelay() for         10 ns", ndelay(10));
	DILLY_DALLY("udelay() for     10,000 ns", udelay(10));
	DILLY_DALLY("mdelay() for 10,000,000 ns", mdelay(10));

	// Non-atomic sleeping, can cause schedule() to be invoked
	DILLY_DALLY("usleep_range(10,10) for 10,000 ns", usleep_range(10, 10));
	DILLY_DALLY("msleep(10) for      10,000,000 ns", msleep(10));
	DILLY_DALLY("msleep_interruptible(10)         ", msleep_interruptible(10));
	DILLY_DALLY("ssleep(1)                        ", ssleep(1));

	return 0;
}

static void __exit delays_sleeps_exit(void) {
	pr_info("%s: removed\n", OURMODNAME);
}

module_init(delays_sleeps_init);
module_exit(delays_sleeps_exit);

#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/timer.h>

#include "../../convenient.h"

#define OURMODNAME "timer_simple"
#define INITIAL_VALUE 3

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

static struct st_ctx {
	struct timer_list tmr;
	int data;
} ctx;
static unsigned long exp_ms = 420;

static void ding(struct timer_list *timer) {
	struct st_ctx *priv = from_timer(priv, timer, tmr);
	/* from_timer() is in fact a wrapper around the well known
	 * container_of() macro! This allows us to retrieve access to our
	 * 'parent' driver context structure
	 */
	// Whoops! Bugfix- decrement even if DEBUG is off...
	priv->data--;
	pr_debug("timed out... data=%d\n", priv->data);
	PRINT_CTX();

	/* until countdown done, fire it again! */
	if (priv->data) mod_timer(&priv->tmr, jiffies + msecs_to_jiffies(exp_ms));
}

static int __init timer_simple_init(void) {
	ctx.data = INITIAL_VALUE;

	/* Initialize our kernel timer */
	ctx.tmr.expires = jiffies + msecs_to_jiffies(exp_ms);
	ctx.tmr.flags = 0;
	timer_setup(&ctx.tmr, ding, 0);

	pr_info("timer set to expire in %ld ms\n", exp_ms);
	add_timer(&ctx.tmr); /* Arm it; lets get going! */

	return 0; /* success */
}

static void __exit timer_simple_exit(void) {
	del_timer_sync(&ctx.tmr);
	pr_info("removed\n");
}

module_init(timer_simple_init);
module_exit(timer_simple_exit);

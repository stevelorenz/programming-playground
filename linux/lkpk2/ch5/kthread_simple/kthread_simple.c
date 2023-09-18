#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <asm/atomic.h>
#include <linux/init.h>
#include <linux/kthread.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/sched/signal.h>	 // signal_pending()
#include <linux/sched/task.h>	 // {get,put}_task_struct()
#include <linux/signal.h>		 // allow_signal()

#include "../../convenient.h"

#define OURMODNAME "kthread_simple"
#define KTHREAD_NAME "kt_simple"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

// Every process is described by a task_struct
static struct task_struct *gkthrd_ts;

static int simple_kthread(void *arg) {
	PRINT_CTX();
	if (!current->mm) {
		pr_info("mm field NULL, we are a kernel thread!\n");
	};

	/*
	 * By default all signals are masked for the kthread; allow a couple
	 * so that we can 'manually' kill it
	 */
	allow_signal(SIGINT);
	allow_signal(SIGQUIT);

	while (!kthread_should_stop()) {
		pr_info("FYI, I, kernel thread PID %d, am going to sleep now...\n",
				current->pid);
		// This action is the key step to let the kthread run!
		set_current_state(TASK_INTERRUPTIBLE);
		schedule();	 // yield the processor, go to sleep...
		/* Aaaaaand we're back! Here, it's typically due to either the
		 * SIGINT or SIGQUIT signal hitting us, or due to the rmmod (or
		 * shutdown)
		 */
		if (signal_pending(current)) break;
	}

	// We've been (rudely) interrupted by a signal...
	set_current_state(TASK_RUNNING);
	pr_info(
		"FYI, I, kernel thread PID %d, have been rudely awoken; I shall"
		" now exit... Good day Sir!\n",
		current->pid);
	return 0;
}

static int kthread_simple_init(void) {
	int ret = 0;
	pr_info("Lets now create a kernel thread...\n");
	gkthrd_ts = kthread_run(simple_kthread, NULL, "llkd/%s", KTHREAD_NAME);
	// Convert between pointer to error number...
	if (IS_ERR(gkthrd_ts)) {
		ret = PTR_ERR(gkthrd_ts);  // it's usually -ENOMEM
		pr_err("kthread creation failed (%d)\n", ret);
		return ret;
	}

	get_task_struct(gkthrd_ts); /* increment the kthread task structure's
								 * reference count, marking it as being
								 * in use
								 */
	pr_info(
		"Initialized, kernel thread task ptr is 0x%pK (actual=0x%px)\n"
		"See the new kernel thread 'llkd/%s' with ps (and kill it with SIGINT "
		"or SIGQUIT)\n",
		gkthrd_ts, gkthrd_ts, KTHREAD_NAME);

	return 0;
}

static void kthread_simple_exit(void) {
	kthread_stop(gkthrd_ts);
	/* waits for our kthread to terminate; it also
	 * internally invokes the put_task_struct() to
	 * decrement task's reference count
	 */
	pr_info("kthread stopped, and LKM removed.\n");
}

module_init(kthread_simple_init);
module_exit(kthread_simple_exit);

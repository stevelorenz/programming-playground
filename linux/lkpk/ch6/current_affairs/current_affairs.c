#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/cred.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/preempt.h>
#include <linux/sched.h>
#include <linux/uidgid.h>

#define MODNAME "current_affairs"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

static void show_ctx(char *nm) {
	pr_info("%s:%s:%d: ", nm, __func__, __LINE__);

	unsigned int uid = from_kuid(&init_user_ns, current_uid());
	unsigned int euid = from_kuid(&init_user_ns, current_euid());

	if (likely(in_task())) {
		pr_info(
			"%s: in process context ::\n"
			" PID         : %6d\n"
			" TGID        : %6d\n"
			" UID         : %6u\n"
			" EUID        : %6u (%s root)\n"
			" name        : %s\n"
			" current (ptr to our process context's task_struct) :\n"
			"               0x%pK (0x%px)\n"
			" stack start : 0x%pK (0x%px)\n",
			nm,
			/* always better to use the helper methods provided */
			task_pid_nr(current), task_tgid_nr(current),
			/* ... rather than the 'usual' direct lookups:
			 * current->pid, current->tgid,
			 */
			uid, euid, (euid == 0 ? "have" : "don't have"), current->comm,
			current, current, current->stack, current->stack);
	} else {
		pr_alert("%s: in interrupt context [Should NOT happen here]!!!\n", nm);
	}
}

static int __init current_affairs_init(void) {
	pr_info("inserted\n");
	pr_info("size of struct task_struct=%zd\n", sizeof(struct task_struct));
	show_ctx(MODNAME);
	return 0;
}

static void __exit current_affairs_exit(void) { 
    show_ctx(MODNAME);
    pr_info("removed\n"); }

module_init(current_affairs_init);
module_exit(current_affairs_exit);

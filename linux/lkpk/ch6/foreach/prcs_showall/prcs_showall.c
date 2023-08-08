#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/cred.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/preempt.h>
#include <linux/sched.h>
#include <linux/uidgid.h>
#include <linux/version.h>
#if LINUX_VERSION_CODE > KERNEL_VERSION(4, 10, 0)
#include <linux/sched/signal.h> /* for_each_xxx(), ... */
#endif
#include <linux/fs.h> /* no_llseek() */
#include <linux/kallsyms.h>
#include <linux/slab.h>
#include <linux/uaccess.h> /* copy_to_user() */

#define MODNAME "prcs_showall"
#define TMP_LEN 128

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");	// or whatever
MODULE_VERSION("0.1");

static int show_prcs_in_tasklist(void) {
	struct task_struct *p;
	char tmp[TMP_LEN];
	int numread = 0, n = 0, total = 0;
	char hdr[] = "     Name       |  TGID  |   PID  |  RUID |  EUID";
	pr_info("%s\n", &hdr[0]);
	// task_list is global and require locking, but task_list_lock is not
	// available for LKMs. so, RCU read lock is indicated here.
	rcu_read_lock();
	for_each_process(p) {
		memset(tmp, 0, TMP_LEN);
		n = snprintf(tmp, TMP_LEN, "%-16s|%8d|%8d|%7u|%7u\n", p->comm, p->tgid,
					 p->pid, __kuid_val(p->cred->uid),
					 __kuid_val(p->cred->euid));
        numread += n;
        pr_info("%s", tmp);
        cond_resched();
		total++;
	}

	rcu_read_unlock();
	return total;
}

static int __init prcs_showall_init(void) {
	int total = 0;
	pr_info("inserted\n");
	total = show_prcs_in_tasklist();
	pr_info("%s: total # of processes on the system: %d\n", MODNAME, total);
	return 0;
}

static void __exit prcs_showall_exit(void) { pr_info("removed\n"); }

module_init(prcs_showall_init);
module_exit(prcs_showall_exit);

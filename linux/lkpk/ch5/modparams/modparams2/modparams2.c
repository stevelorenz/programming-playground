#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

#define MODNAME "modparams2"

MODULE_AUTHOR("Zuo Xiang");
MODULE_LICENSE("GPL");

/* Module parameters */
static int mp_debug_level = 0;
module_param(mp_debug_level, int, 0660);
MODULE_PARM_DESC(
	mp_debug_level,
	"Debug level [0-2]; 0 => no debug messages, 2 => high verbosity");

static char *mp_strparam = "My string param";
module_param(mp_strparam, charp, 0660);
MODULE_PARM_DESC(mp_strparam, "A demo string parameter");

static int control_freak;
module_param(control_freak, int, 0660);
MODULE_PARM_DESC(control_freak,
				 "Set to the project's control level [1-5]. MANDATORY");

static int __init modparams2_init(void) {
	pr_info("inserted\n");
	pr_info(
		"module parameters passed: mp_debug_level=%d, mp_strparam=%s, "
		"control_freak=%d\n",
		mp_debug_level, mp_strparam, control_freak);

	if ((control_freak < 1) || (control_freak > 5)) {
		pr_warn(
			"%s: Must pass along module parameter"
			"'control_freak', value in range [1-5]; aborting...\n");
		return -EINVAL;
	}

	return 0;
}

static void __exit modparams2_exit(void) { pr_info("removed\n"); }

module_init(modparams2_init);
module_exit(modparams2_exit);

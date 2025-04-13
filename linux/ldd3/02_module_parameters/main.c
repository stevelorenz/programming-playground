#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

static int howmany = 1;
static char *whom = "Mom";

module_param(howmany, int, S_IRUGO);
module_param(whom, charp, S_IRUGO);

static int m_init(void) {
    pr_debug("parameters test module is loaded\n");
    for (int i = 0; i < howmany; ++i) {
        pr_info("#%d hello, %s\n", i, whom);
    }
    return 0;
}

static void m_exit(void) { pr_debug("parameters test module is unloaded\n"); }

module_init(m_init);
module_exit(m_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("zuoxiang");
MODULE_DESCRIPTION("Module parameters test program");

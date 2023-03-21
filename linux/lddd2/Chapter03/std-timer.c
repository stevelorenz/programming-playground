#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/timer.h>

MODULE_LICENSE("GPL");

static struct timer_list my_timer;

void my_timer_callback(struct timer_list *t)
{
    // jiffies is a global variable
    pr_info("%s called (%ld).\n", __FUNCTION__, jiffies);
}

static int __init my_init(void)
{
    int retval;
    pr_info("Timer module loaded\n");

    timer_setup(&my_timer, my_timer_callback, 0);

    return 0;
}

module_init(my_init);

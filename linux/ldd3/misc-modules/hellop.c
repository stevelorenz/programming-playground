#include <linux/init.h>
#include <linux/module.h>
#include <linux/moduleparam.h>

MODULE_LICENSE("Dual BSD/GPL");

static int howmany = 1;
static char* whom = "world";
module_param(howmany, int, S_IRUGO);
module_param(whom, charp, S_IRUGO);

static int hello_init(void)
{
    int i;
    for (i = 0; i < howmany; ++i) {
        printk(KERN_ALERT "(%d) Hello, %s\n", i, whom);
    }
    return 0;
}

static void hello_exit(void)
{
    printk(KERN_ALERT "Goodbyte, %s\n", whom);
}

module_init(hello_init);
module_exit(hello_exit);

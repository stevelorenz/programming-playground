#include <linux/module.h>
#include <linux/platform_device.h>
#include <linux/types.h>

static struct platform_device *pdev = NULL;

static int __init platform_dummy_char_add(void) {
    int inst_id = 0;
    pdev = platform_device_alloc("fake-gpio-chip", inst_id);
    platform_device_add(pdev);
    pr_info("fake-gpio-chip added\n");
    return 0;
}

static void __exit platform_device_char_put(void) { platform_device_put(pdev); }

module_init(platform_dummy_char_add);
module_exit(platform_device_char_put);
MODULE_LICENSE("GPL");
MODULE_AUTHOR("John Madieu <john.madieu@gmail.com>");

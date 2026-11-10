/*
 * bottomhalf.c - Top and bottom half interrupt handling
 *
 * Based upon the RPi example by Stefan Wendler (devnull@kaltpost.de)
 * from:
 *    https://github.com/wendlers/rpi-kmod-samples
 *
 * Press one button to turn on an LED and another to turn it off
 */

#include <linux/delay.h>
#include <linux/gpio.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/module.h>
#include <linux/printk.h>
#include <linux/version.h>
#include <linux/workqueue.h>

static int button_irqs[] = {-1, -1};

/* Define GPIOs for LEDs.
 * TODO: Change the numbers for the GPIO on your board.
 */
#ifdef NO_GPIO_REQUEST_ARRAY
static unsigned int led_gpio = 4;
static unsigned int button_gpios[] = {17, 18};
#else
static struct gpio leds[] = {{4, GPIOF_OUT_INIT_LOW, "LED 1"}};

/* Define GPIOs for BUTTONS
 * TODO: Change the numbers for the GPIO on your board.
 */
static struct gpio buttons[] = {
    {17, GPIOF_IN, "LED 1 ON BUTTON"},
    {18, GPIOF_IN, "LED 1 OFF BUTTON"},
};
#endif

/* Workqueue function containing some non-trivial amount of processing */
static void bottomhalf_work_fn(struct work_struct *work) {
    pr_info("Bottom half workqueue starts\n");
    /* do something which takes a while */
    msleep(500);

    pr_info("Bottom half workqueue ends\n");
}

static DECLARE_WORK(bottomhalf_work, bottomhalf_work_fn);

/* interrupt function triggered when a button is pressed */
static irqreturn_t button_isr(int irq, void *data) {
    /* Do something quickly right now */
#ifdef NO_GPIO_REQUEST_ARRAY
    if (irq == button_irqs[0] && !gpio_get_value(led_gpio))
        gpio_set_value(led_gpio, 1);
    else if (irq == button_irqs[1] && gpio_get_value(led_gpio))
        gpio_set_value(led_gpio, 0);
#else
    if (irq == button_irqs[0] && !gpio_get_value(leds[0].gpio))
        gpio_set_value(leds[0].gpio, 1);
    else if (irq == button_irqs[1] && gpio_get_value(leds[0].gpio))
        gpio_set_value(leds[0].gpio, 0);
#endif

    /* Do the rest at leisure via the scheduler */
    schedule_work(&bottomhalf_work);
    return IRQ_HANDLED;
}

static int __init bottomhalf_init(void) {
    int ret = 0;

    pr_info("%s\n", __func__);

    /* register LED gpios */
#ifdef NO_GPIO_REQUEST_ARRAY
    ret = gpio_request(led_gpio, "LED 1");
#else
    ret = gpio_request_array(leds, ARRAY_SIZE(leds));
#endif

    if (ret) {
        pr_err("Unable to request GPIOs for LEDs: %d\n", ret);
        return ret;
    }

#ifdef NO_GPIO_REQUEST_ARRAY
    ret = gpio_direction_output(led_gpio, 0);

    if (ret) {
        pr_err("Unable to configure LED GPIO direction: %d\n", ret);
        goto fail2;
    }
#endif

    /* register BUTTON gpios */
#ifdef NO_GPIO_REQUEST_ARRAY
    ret = gpio_request(button_gpios[0], "LED 1 ON BUTTON");

    if (ret) {
        pr_err("Unable to request GPIOs for BUTTONs: %d\n", ret);
        goto fail2;
    }

    ret = gpio_request(button_gpios[1], "LED 1 OFF BUTTON");

    if (ret) {
        pr_err("Unable to request GPIOs for BUTTONs: %d\n", ret);
        goto fail3;
    }

    ret = gpio_direction_input(button_gpios[0]);

    if (ret) {
        pr_err("Unable to configure BUTTON1 GPIO direction: %d\n", ret);
        goto fail4;
    }

    ret = gpio_direction_input(button_gpios[1]);

    if (ret) {
        pr_err("Unable to configure BUTTON2 GPIO direction: %d\n", ret);
        goto fail4;
    }
#else
    ret = gpio_request_array(buttons, ARRAY_SIZE(buttons));

    if (ret) {
        pr_err("Unable to request GPIOs for BUTTONs: %d\n", ret);
        goto fail1;
    }
#endif

#ifdef NO_GPIO_REQUEST_ARRAY
    pr_info("Current button1 value: %d\n", gpio_get_value(button_gpios[0]));

    ret = gpio_to_irq(button_gpios[0]);
#else
    pr_info("Current button1 value: %d\n", gpio_get_value(buttons[0].gpio));

    ret = gpio_to_irq(buttons[0].gpio);
#endif

    if (ret < 0) {
        pr_err("Unable to request IRQ: %d\n", ret);
#ifdef NO_GPIO_REQUEST_ARRAY
        goto fail4;
#else
        goto fail2;
#endif
    }

    button_irqs[0] = ret;

    pr_info("Successfully requested BUTTON1 IRQ # %d\n", button_irqs[0]);

    ret = request_irq(button_irqs[0], button_isr,
                      IRQF_TRIGGER_RISING | IRQF_TRIGGER_FALLING,
                      "gpiomod#button1", NULL);

    if (ret) {
        pr_err("Unable to request IRQ: %d\n", ret);
#ifdef NO_GPIO_REQUEST_ARRAY
        goto fail4;
#else
        goto fail2;
#endif
    }

#ifdef NO_GPIO_REQUEST_ARRAY
    ret = gpio_to_irq(button_gpios[1]);
#else
    ret = gpio_to_irq(buttons[1].gpio);
#endif

    if (ret < 0) {
        pr_err("Unable to request IRQ: %d\n", ret);
#ifdef NO_GPIO_REQUEST_ARRAY
        goto fail5;
#else
        goto fail2;
#endif
    }

    button_irqs[1] = ret;

    pr_info("Successfully requested BUTTON2 IRQ # %d\n", button_irqs[1]);

    ret = request_irq(button_irqs[1], button_isr,
                      IRQF_TRIGGER_RISING | IRQF_TRIGGER_FALLING,
                      "gpiomod#button2", NULL);

    if (ret) {
        pr_err("Unable to request IRQ: %d\n", ret);
#ifdef NO_GPIO_REQUEST_ARRAY
        goto fail5;
#else
        goto fail3;
#endif
    }

    return 0;

/* cleanup what has been setup so far */
#ifdef NO_GPIO_REQUEST_ARRAY
fail5:
    free_irq(button_irqs[0], NULL);

fail4:
    gpio_free(button_gpios[1]);

fail3:
    gpio_free(button_gpios[0]);

fail2:
    gpio_free(led_gpio);
#else
fail3:
    free_irq(button_irqs[0], NULL);

fail2:
    gpio_free_array(buttons, ARRAY_SIZE(buttons));

fail1:
    gpio_free_array(leds, ARRAY_SIZE(leds));
#endif

    return ret;
}

static void __exit bottomhalf_exit(void) {
    pr_info("%s\n", __func__);

    /* free irqs */
    free_irq(button_irqs[0], NULL);
    free_irq(button_irqs[1], NULL);

    /* turn all LEDs off */
#ifdef NO_GPIO_REQUEST_ARRAY
    gpio_set_value(led_gpio, 0);
#else
    int i;
    for (i = 0; i < ARRAY_SIZE(leds); i++)
        gpio_set_value(leds[i].gpio, 0);
#endif

    /* unregister */
#ifdef NO_GPIO_REQUEST_ARRAY
    gpio_free(led_gpio);
    gpio_free(button_gpios[0]);
    gpio_free(button_gpios[1]);
#else
    gpio_free_array(leds, ARRAY_SIZE(leds));
    gpio_free_array(buttons, ARRAY_SIZE(buttons));
#endif
}

module_init(bottomhalf_init);
module_exit(bottomhalf_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("Interrupt with top and bottom half");

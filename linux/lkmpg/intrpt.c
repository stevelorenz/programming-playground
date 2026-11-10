#include <linux/gpio.h>
#include <linux/interrupt.h>
#include <linux/kernel.h> /* for ARRAY_SIZE() */
#include <linux/module.h>
#include <linux/printk.h>
#include <linux/version.h>

#if LINUX_VERSION_CODE >= KERNEL_VERSION(6, 10, 0)
#define NO_GPIO_REQUEST_ARRAY
#endif

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
static struct gpio buttons[] = {{17, GPIOF_IN, "LED 1 ON BUTTON"},
                                {18, GPIOF_IN, "LED 1 OFF BUTTON"}};
#endif

// Button Interrupt Service Routine (ISR)
static irqreturn_t button_isr(int irq, void *data) {
#ifdef NO_GPIO_REQUEST_ARRAY
    if (irq == button_irqs[0] && !gpio_get_value(led_gpio))
        gpio_set_value(led_gpio, 1);
    else if (irq == button_irqs[1] && gpio_get_value(led_gpio))
        gpio_set_value(led_gpio, 0);
#else
    /* first button */
    if (irq == button_irqs[0] && !gpio_get_value(leds[0].gpio))
        gpio_set_value(leds[0].gpio, 1);
    /* second button */
    else if (irq == button_irqs[1] && gpio_get_value(leds[0].gpio))
        gpio_set_value(leds[0].gpio, 0);
#endif
    return IRQ_HANDLED;
}

static int __init intrpt_init(void) {
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

    /* register BUTTON gpios */
#ifdef NO_GPIO_REQUEST_ARRAY
    ret = gpio_request(button_gpios[0], "LED 1 ON BUTTON");

    if (ret) {
        pr_err("Unable to request GPIOs for BUTTONs: %d\n", ret);
        goto fail1;
    }

    ret = gpio_request(button_gpios[1], "LED 1 OFF BUTTON");

    if (ret) {
        pr_err("Unable to request GPIOs for BUTTONs: %d\n", ret);
        goto fail2;
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
        goto fail3;
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
        goto fail3;
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
        goto fail4;
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
        goto fail4;
#else
        goto fail3;
#endif
    }

    return 0;

/* cleanup what has been setup so far */
#ifdef NO_GPIO_REQUEST_ARRAY
fail4:
    free_irq(button_irqs[0], NULL);

fail3:
    gpio_free(button_gpios[1]);

fail2:
    gpio_free(button_gpios[0]);

fail1:
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

static void __exit intrpt_exit(void) {
    pr_info("%s\n", __func__);
    free_irq(button_irqs[0], NULL);
    free_irq(button_irqs[1], NULL);
}

module_init(intrpt_init);
module_exit(intrpt_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("Handle some GPIO interrupts");

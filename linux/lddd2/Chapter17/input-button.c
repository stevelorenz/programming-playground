#include <linux/gpio/consumer.h> /* For GPIO Descriptor interface */
#include <linux/input.h>
#include <linux/interrupt.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/of.h>              /* For DT*/
#include <linux/platform_device.h> /* For platform devices */
#include <linux/slab.h>

struct btn_data {
    struct gpio_desc *btn_gpiod;
    struct input_dev *i_dev;
    struct platform_device *pdev;
    int irq;
};

static int btn_open(struct input_dev *i_dev) {
    pr_info("Input device opened\n");
    return 0;
}

static void btn_close(struct input_dev *i_dev) {
    pr_info("input device closed\n");
}

static irqreturn_t packt_btn_interrupt(int irq, void *dev_id) {
    struct btn_data *priv = dev_id;

    input_report_key(priv->i_dev, BTN_0, gpiod_get_value(priv->btn_gpiod) & 1);

    input_sync(priv->i_dev);

    return IRQ_HANDLED;
}

static const struct of_device_id btn_dt_ids[] = {
    {
        .compatible = "packt,input-button",
    },
    {/* sentinel */}};

static int btn_probe(struct platform_device *pdev) {

    struct btn_data *priv;
    struct gpio_desc *gpiod;
    struct input_dev *i_dev;
    int ret;

    priv = devm_kzalloc(&pdev->dev, sizeof(*priv), GFP_KERNEL);
    if (!priv) {
        return -ENOMEM;
    }

    i_dev = input_allocate_device();
    if (!i_dev) {
        return -ENOMEM;
    }

    i_dev->open = btn_open;
    i_dev->close = btn_close;
    i_dev->name = "Packt Btn";
    i_dev->dev.parent = &pdev->dev;

    priv->i_dev = i_dev;
    priv->pdev = pdev;

    set_bit(EV_KEY, i_dev->evbit);
    set_bit(BTN_0, i_dev->keybit);

    gpiod = gpiod_get(&pdev->dev, "button", GPIOD_IN);
    if (IS_ERR(gpiod)) {
        return -ENODEV;
    }

    priv->irq = gpiod_to_irq(priv->btn_gpiod);
    priv->btn_gpiod = gpiod;

    ret = input_register_device(priv->i_dev);
    if (ret) {
        pr_err("Failed to register input device\n");
        goto err_input;
    }

    ret = request_any_context_irq(priv->irq, packt_btn_interrupt,
                                  (IRQF_TRIGGER_FALLING | IRQF_TRIGGER_RISING),
                                  "packt-input-button", priv);

    if (ret < 0) {
        goto err_btn;
    }

    platform_set_drvdata(pdev, priv);
    return 0;

err_btn:
    gpiod_put(priv->btn_gpiod);
err_input:
    input_free_device(i_dev);
    return ret;
}

static int btn_remove(struct platform_device *pdev) { return 0; }

static struct platform_driver mypdr = {
    .probe = btn_probe,
    .remove = btn_remove,
    .driver =
        {
            .name = "input-button",
            .of_match_table = of_match_ptr(btn_dt_ids),
            .owner = THIS_MODULE,
        },
};
module_platform_driver(mypdr);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("John Madieu <john.madieu@gmail.com>");
MODULE_DESCRIPTION("Input device (IRQ based)");

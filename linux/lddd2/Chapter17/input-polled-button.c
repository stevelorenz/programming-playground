#include <linux/gpio/consumer.h>
#include <linux/input.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/of.h>
#include <linux/platform_device.h>
#include <linux/slab.h>

struct poll_btn_data {
    struct gpio_desc *btn_gpiod;
    struct input_dev *input;
};

static int polled_btn_open(struct input_dev *input) {
    pr_info("polled device opened()\n");
    return 0;
}

static void polled_btn_close(struct input_dev *input) {
    pr_info("polled device closed()\n");
}

static void polled_btn_poll(struct input_dev *input) {
    struct poll_btn_data *priv = input_get_drvdata(input);
    int value;

    value = gpiod_get_value_cansleep(priv->btn_gpiod);

    input_report_key(input, BTN_0, value & 1);
    input_sync(input);
}

static const struct of_device_id btn_dt_ids[] = {
    {.compatible = "packt,input-polled-button"}, {}};
MODULE_DEVICE_TABLE(of, btn_dt_ids);

static int polled_btn_probe(struct platform_device *pdev) {
    struct poll_btn_data *priv;
    struct input_dev *input;
    int ret;

    priv = devm_kzalloc(&pdev->dev, sizeof(*priv), GFP_KERNEL);
    if (!priv) {
        return -ENOMEM;
    }

    priv->btn_gpiod = devm_gpiod_get(&pdev->dev, "button", GPIOD_IN);
    if (IS_ERR(priv->btn_gpiod)) {
        return dev_err_probe(&pdev->dev, PTR_ERR(priv->btn_gpiod),
                             "failed to get button GPIO\n");
    }

    input = devm_input_allocate_device(&pdev->dev);
    if (!input) {
        return -ENOMEM;
    }

    priv->input = input;

    input->name = "Packt input polled Btn";
    input->dev.parent = &pdev->dev;
    input->open = polled_btn_open;
    input->close = polled_btn_close;

    input_set_drvdata(input, priv);
    input_set_capability(input, EV_KEY, BTN_0);

    ret = input_setup_polling(input, polled_btn_poll);
    if (ret) {
        return ret;
    }

    input_set_poll_interval(input, 200);

    ret = input_register_device(input);
    if (ret) {
        return ret;
    }

    platform_set_drvdata(pdev, priv);

    return 0;
}

static int polled_btn_remove(struct platform_device *pdev) { return 0; }

static struct platform_driver mypdrv = {
    .probe = polled_btn_probe,
    .remove = polled_btn_remove,
    .driver =
        {
            .name = "input-polled-button",
            .of_match_table = of_match_ptr(btn_dt_ids),
        },
};

module_platform_driver(mypdrv);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("John Madieu <john.madieu@gmail.com>");
MODULE_DESCRIPTION("Polled input device");

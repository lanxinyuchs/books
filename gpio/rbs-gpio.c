/*
 * RBS GPIO access interface
 *
 * Simple module that allows to access GPIO pins from user-space
 *
 * Copyright 2013 Ericsson AB
 * Andrey Panteleev <andrey.xx.panteleev@ericsson.com>
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/net.h>
#include <linux/slab.h>
#include <asm/uaccess.h>
#include <linux/types.h>
#include <linux/interrupt.h>
#include <linux/gpio.h>

#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-gpio.h>

#ifdef CONFIG_RBS_GPIO_MAXPINS
#define RBS_GPIO_MAXPINS	CONFIG_RBS_GPIO_MAXPINS
#else
#define RBS_GPIO_MAXPINS	32
#endif

#define RBS_GPIO_IRQSIZE	((RBS_GPIO_MAXPINS - 1) / BITS_PER_LONG + 1)

struct rbs_gpio;

#define RBS_GPIOPIN_IRQEN	0x1

struct rbs_gpio_pin {
	int pin;

	int irq;
	unsigned long irq_state;

	struct rbs_gpio *gpio;
};

struct rbs_gpio {
	struct mutex lock;
	struct rbs_gpio_pin *pins[RBS_GPIO_MAXPINS];
	unsigned long irq_active[RBS_GPIO_IRQSIZE];
	unsigned long irq_active_buf[RBS_GPIO_IRQSIZE];
	int irq_active_no;
};

static irqreturn_t rbs_gpio_handle_irq(int irq, void *dev)
{
	struct rbs_gpio_pin *pin = dev;

	if (test_and_clear_bit(RBS_GPIOPIN_IRQEN, &pin->irq_state))
		disable_irq_nosync(irq);

	mutex_lock(&pin->gpio->lock);
	if (!test_and_set_bit(pin->pin, pin->gpio->irq_active))
		pin->gpio->irq_active_no++;
	mutex_unlock(&pin->gpio->lock);

	rbs_proto_oobavail(RBS_PROTO_GPIO);
	return IRQ_HANDLED;
}

static int rbs_gpio_sock_init(struct rbs_sock *rsk, void *init_param)
{
	struct rbs_gpio *gpio;

	gpio = kzalloc(sizeof(*gpio), GFP_KERNEL);
	if (!gpio)
		return -ENOMEM;
	mutex_init(&gpio->lock);

	rsk->proto_data = gpio;
	return 0;
}

static void rbs_gpio_sock_done(struct rbs_sock *rsk)
{
	struct rbs_gpio *gpio;
	struct rbs_gpio_pin *pin;
	int i;

	gpio = rsk->proto_data;
	rsk->proto_data = NULL;

	/* free all reserved pins */
	for (i = 0; i < RBS_GPIO_MAXPINS; i++) {
		pin = gpio->pins[i];
		if (!pin)
			continue;

		if (pin->irq != NO_IRQ)
			free_irq(pin->irq, pin);

		gpio_free(pin->pin);
		kfree(pin);
	}

	kfree(gpio);
}

static int rbs_gpio_sock_oobavail(struct rbs_sock *rsk)
{
	struct rbs_gpio *gpio = rsk->proto_data;
	int irqs;

	/* Safe not to take the mutex here, worst possible
	 * scenario is that read old active data which will
	 * cause us to wake up when we don't have to.
	 */
	irqs = gpio->irq_active_no;

	return irqs ? 1 : 0;
}

static void *rbs_gpio_sock_oobrecv(struct rbs_sock *rsk, int *size)
{
	struct rbs_gpio *gpio = rsk->proto_data;

 	mutex_lock(&gpio->lock);
	if (!gpio->irq_active_no) {
		mutex_unlock(&gpio->lock);
		return NULL;
	}

	memcpy(gpio->irq_active_buf, gpio->irq_active,
			sizeof(gpio->irq_active));
	memset(gpio->irq_active, 0, sizeof(gpio->irq_active));
	gpio->irq_active_no = 0;
	mutex_unlock(&gpio->lock);

	*size = sizeof(gpio->irq_active);

	return &gpio->irq_active_buf;
}

static int rbs_gpio_ioctl_request(struct rbs_gpio *gpio,
		int idx, void __user *arg)
{
	struct rbs_gpio_pin *pin;

	if (gpio->pins[idx])
		return -EBUSY;

	pin = kzalloc(sizeof(*pin), GFP_KERNEL);
	if (!pin)
		return -ENOMEM;
	if (gpio_request(idx, "rbs_gpio")) {
		kfree(pin);
		return -EBUSY;
	}

	pin->irq = NO_IRQ;
	pin->gpio = gpio;
	pin->pin = idx;

	gpio->pins[idx] = pin;
	return 0;
}

static int rbs_gpio_ioctl_free(struct rbs_gpio *gpio,
		int idx, void __user *arg)
{
	struct rbs_gpio_pin *pin;

	pin = gpio->pins[idx];
	if (!pin)
		return 0;

	if (pin->irq != NO_IRQ)
		free_irq(pin->irq, pin);
	clear_bit(pin->pin, gpio->irq_active);
	gpio->pins[idx] = NULL;
	kfree(pin);
	gpio_free(idx);
	return 0;
}

static int rbs_gpio_ioctl_dir(struct rbs_gpio_pin *pin,
		void __user *arg)
{
	struct rbs_gpio_ioc_state ioc;

	if (!pin)
		return -EINVAL;
	if (copy_from_user(&ioc, arg, sizeof(ioc)))
		return  -EFAULT;

	switch (ioc.state) {
	case RBS_GPIOSTATE_IN:
		gpio_direction_input(pin->pin);
		break;
	case RBS_GPIOSTATE_OUT_LOW:
		gpio_direction_output(pin->pin, 0);
		break;
	case RBS_GPIOSTATE_OUT_HIGH:
		gpio_direction_output(pin->pin, 1);
		break;
	}
	return 0;
}

static int rbs_gpio_ioctl_set(struct rbs_gpio_pin *pin,
		void __user *arg)
{
	struct rbs_gpio_ioc_state ioc;

	if (!pin)
		return -EINVAL;
	if (copy_from_user(&ioc, arg, sizeof(ioc)))
		return -EFAULT;

	gpio_set_value_cansleep(pin->pin, ioc.state ? 1 : 0);
	return 0;
}

static int rbs_gpio_ioctl_get(struct rbs_gpio_pin *pin,
		void __user *arg)
{
	struct rbs_gpio_ioc_state ioc;

	if (!pin)
		return -EINVAL;
	if (copy_from_user(&ioc, arg, sizeof(ioc)))
		return -EFAULT;

	ioc.state = gpio_get_value_cansleep(pin->pin);

	if (copy_to_user(arg, &ioc, sizeof(ioc)))
		return -EFAULT;
	return 0;
}

static int rbs_gpio_ioctl_setupirq(struct rbs_gpio_pin *pin,
		void __user *arg)
{
	struct rbs_gpio_ioc_irq ioc;
	unsigned long irqflags = IRQF_ONESHOT;

	if (!pin)
		return -EINVAL;
	if (pin->irq != NO_IRQ)
		return -EALREADY;
	if (copy_from_user(&ioc, arg, sizeof(ioc)))
		return -EFAULT;

	switch (ioc.type) {
	case RBS_GPIOIRQ_LEVEL_LOW:
		irqflags |= IRQF_TRIGGER_LOW;
		break;
	case RBS_GPIOIRQ_LEVEL_HIGH:
		irqflags |= IRQF_TRIGGER_HIGH;
		break;
	case RBS_GPIOIRQ_EDGE_RISE:
		irqflags |= IRQF_TRIGGER_RISING;
		break;
	case RBS_GPIOIRQ_EDGE_FALL:
		irqflags |= IRQF_TRIGGER_FALLING;
		break;
	default:
		return -EINVAL;
	}

	pin->irq = gpio_to_irq(pin->pin);
	if (pin->irq < 0 || pin->irq == NO_IRQ) {
		pin->irq = NO_IRQ;
		return -ENXIO;
	}
	set_bit(RBS_GPIOPIN_IRQEN, &pin->irq_state);
	if (request_threaded_irq(pin->irq, NULL, rbs_gpio_handle_irq,
				irqflags, "gpio-irq", pin)) {
		pin->irq = NO_IRQ;
		return -ENXIO;
	}

	return 0;
}

static int rbs_gpio_ioctl_enableirq(struct rbs_gpio_pin *pin,
		void __user *arg)
{
	if (!pin || (pin->irq == NO_IRQ))
		return -EINVAL;
	if (!test_and_set_bit(RBS_GPIOPIN_IRQEN, &pin->irq_state))
		enable_irq(pin->irq);
	return 0;
}

static int rbs_gpio_ioctl_disableirq(struct rbs_gpio_pin *pin,
		void __user *arg)
{
	if (!pin || (pin->irq == NO_IRQ))
		return -EINVAL;
	if (test_and_clear_bit(RBS_GPIOPIN_IRQEN, &pin->irq_state))
		disable_irq(pin->irq);
	return 0;
}

static int rbs_gpio_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
		unsigned long arg)
{
	int ret;
	struct rbs_gpio *gpio = rsk->proto_data;
	struct rbs_gpio_pin *pin;
	int idx;
	void __user *param = (void __user *) arg;

	if (copy_from_user(&idx, param, sizeof(idx)))
		return -EFAULT;

	if (idx < 0 || idx >= RBS_GPIO_MAXPINS)
		return -EINVAL;

	mutex_lock(&gpio->lock);
	pin = gpio->pins[idx];
	switch (cmd) {
	case RBS_GPIOIOC_REQUEST:
		ret = rbs_gpio_ioctl_request(gpio, idx, param);
		break;
	case RBS_GPIOIOC_FREE:
		ret = rbs_gpio_ioctl_free(gpio, idx, param);
		break;
	case RBS_GPIOIOC_DIR:
		ret = rbs_gpio_ioctl_dir(pin, param);
		break;
	case RBS_GPIOIOC_SET:
		ret = rbs_gpio_ioctl_set(pin, param);
		break;
	case RBS_GPIOIOC_GET:
		ret = rbs_gpio_ioctl_get(pin, param);
		break;
	case RBS_GPIOIOC_SETUPIRQ:
		ret = rbs_gpio_ioctl_setupirq(pin, param);
		break;
	case RBS_GPIOIOC_ENABLEIRQ:
		ret = rbs_gpio_ioctl_enableirq(pin, param);
		break;
	case RBS_GPIOIOC_DISABLEIRQ:
		ret = rbs_gpio_ioctl_disableirq(pin, param);
		break;
	default:
		ret = -ENOSYS;
		break;
	}
	mutex_unlock(&gpio->lock);

	return ret;
}


static struct rbs_proto_ops gpio_proto_ops = {
	.init = rbs_gpio_sock_init,
	.done = rbs_gpio_sock_done,
	.oob_avail = rbs_gpio_sock_oobavail,
	.oob_recv = rbs_gpio_sock_oobrecv,
#if defined(CONFIG_COMPAT)
	.compat_ioctl = rbs_gpio_sock_ioctl,
#endif
	.ioctl = rbs_gpio_sock_ioctl,
	.mmap = NULL,
};

static int __init rbs_gpio_init(void)
{
	if (rbs_proto_register(RBS_PROTO_GPIO, &gpio_proto_ops, NULL)) {
		pr_err("%s: unable to register protocol\n", __func__);
		return -EBUSY;
	}

	return 0;
}

static void __exit rbs_gpio_exit(void)
{
	rbs_proto_unregister(RBS_PROTO_GPIO, &gpio_proto_ops);
}

module_init(rbs_gpio_init);
module_exit(rbs_gpio_exit);

MODULE_LICENSE("GPL");

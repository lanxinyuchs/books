/*
 * GPIO socket API
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

#ifndef UAPI_RBS_GPIO_H__
#define UAPI_RBS_GPIO_H__

#ifndef NO_IRQ
#define NO_IRQ                  ((unsigned int)(-1))
#endif

#define RBS_GPIOIOC_REQUEST	_IOR('g', 0, int)
#define RBS_GPIOIOC_FREE	_IOR('g', 1, int)

#define RBS_GPIOSTATE_IN	0x0
#define RBS_GPIOSTATE_OUT_LOW	0x1
#define RBS_GPIOSTATE_OUT_HIGH	0x2

struct rbs_gpio_ioc_state {
	int pin;
	int state;
};

#define RBS_GPIOIOC_DIR		_IOR('g', 2, struct rbs_gpio_ioc_state)
#define RBS_GPIOIOC_SET		_IOR('g', 3, struct rbs_gpio_ioc_state)
#define RBS_GPIOIOC_GET		_IOWR('g', 4, struct rbs_gpio_ioc_state)

#define RBS_GPIOIRQ_LEVEL_LOW	0x0
#define RBS_GPIOIRQ_LEVEL_HIGH	0x1
#define RBS_GPIOIRQ_EDGE_RISE	0x3
#define RBS_GPIOIRQ_EDGE_FALL	0x4

struct rbs_gpio_ioc_irq {
	int pin;
	int type;
};

#define RBS_GPIOIOC_SETUPIRQ	_IOR('g', 5, struct rbs_gpio_ioc_irq)
#define RBS_GPIOIOC_ENABLEIRQ	_IOR('g', 6, int)
#define RBS_GPIOIOC_DISABLEIRQ	_IOR('g', 7, int)

#endif /* UAPI_RBS_GPIO_H__ */


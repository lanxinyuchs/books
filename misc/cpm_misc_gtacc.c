/*
 * CPM1 misc
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
#include <linux/errno.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <linux/smp.h>

#if defined(__arm__)
static inline void CNTKCTL_write(u32 val)
{
	asm volatile("mcr p15, 0, %0, c14, c1, 0" : : "r" (val));
}

static inline u32 CNTKCTL_read(void)
{
	u32 val;

	asm volatile("mrc p15, 0, %0, c14, c1, 0" : "=r" (val));

	return val;
}
#elif defined(__aarch64__)
static inline void CNTKCTL_write(u32 val)
{
	asm volatile("msr cntkctl_el1, %0" : : "r" (val));
}

static inline u32 CNTKCTL_read(void)
{
	u32 val;

	asm volatile("mrs %0, cntkctl_el1" : "=r" (val));

	return val;
}
#else
#error "This driver currently only supports ARM/ARM64"
#endif
/* enable PL0 access to CNTPCT and CNTFRQ */
static void gtacc_enable(void *unused)
{
	u32 val;

	val = CNTKCTL_read();
	val |= 0x1;
	CNTKCTL_write(val);
}

/* disable PL0 access to CNTPCT and CNTFRQ */
static void gtacc_disable(void *unused)
{
	u32 val;

	val = CNTKCTL_read();
	val &= ~0x1;
	CNTKCTL_write(val);
}

static __init int cpm_misc_gtacc_init(void)
{
	return on_each_cpu(gtacc_enable, NULL, 1);
}

static void __exit cpm_misc_gtacc_exit(void)
{
	on_each_cpu(gtacc_disable, NULL, 1);
}

late_initcall(cpm_misc_gtacc_init);
module_exit(cpm_misc_gtacc_exit);

MODULE_LICENSE("GPL");


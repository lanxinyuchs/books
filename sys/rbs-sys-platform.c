/*
 * RBS system platform drivers for dusX3 boards
 *
 * Copyright 2016 Ericsson AB
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

#define pr_fmt(fmt) "RBS Sys: " fmt

#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/reboot.h>
#include <linux/kmsg_dump.h>
#include <linux/of_platform.h>
#include <linux/platform_device.h>
#if defined(CONFIG_PSTORE_RAM)
#include <linux/pstore_ram.h>
#endif
#ifdef CONFIG_RT_TRACK_BLOCKING_RTMUTEX
#include <../../../kernel/locking/rtmutex_common.h>
#endif
#if defined(CONFIG_CPM_MISC_SYSTEM_WATCHDOG)
#include <linux/rbs/cpm-wdt.h>
#endif
#include <linux/rbs/rbs-sys.h>
#include "rbs-sys-internal.h"
#include "rbs-sys-cpm.h"

#ifdef CONFIG_RT_TRACK_BLOCKING_RTMUTEX

#define FLAG_DUMP 1UL
#define FLAG_IS_DUMPED 2UL
static inline struct rt_mutex * get_blocking_rt_mutex(struct task_struct *task)
{
	unsigned long v = (unsigned long)task->blocking_rt_mutex;

	return (struct rt_mutex *)(v & ~FLAG_DUMP);
}

static void set_dumpflag(struct task_struct *task, unsigned long flag)
{
	unsigned long v = (unsigned long)task->blocking_rt_mutex;

	task->blocking_rt_mutex = (struct rt_mutex *)(v | flag);
}

static void clear_dumpflag(struct task_struct *task, unsigned long flag)
{
	unsigned long v = (unsigned long)task->blocking_rt_mutex;

	task->blocking_rt_mutex = (struct rt_mutex *)(v & ~flag);
}

static int has_dumpflag(struct task_struct *task, unsigned long flag)
{
	return (unsigned long)task->blocking_rt_mutex & flag;
}
#else
static void set_dumpflag(struct task_struct *task, unsigned long flag) {}
static void clear_dumpflag(struct task_struct *task, unsigned long flag) {}
static int has_dumpflag(struct task_struct *task, unsigned long flag)
{
	return 0;
}
#endif

static void panic_debug_dump(struct kmsg_dumper *dumper,
			     enum kmsg_dump_reason reason)
{
	static int once = 0;
	struct task_struct *g, *p;
	char Xcomm[TASK_COMM_LEN];

	/* One panic !  */
	if (once++ > 0)
		return;

	/* Take a snapshot of the Real Panic first */
	kmsg_dump(KMSG_DUMP_OOPS);

	printk("---- STARTING TO DUMP BT-ALL\n");
	rcu_read_lock();
#ifdef CONFIG_RT_TRACK_BLOCKING_RTMUTEX
	for_each_process(p) {
		if (get_blocking_rt_mutex(p)) {
			struct task_struct *owner;

			owner = rt_mutex_owner(get_blocking_rt_mutex(p));
			if (owner)
				set_dumpflag(owner, FLAG_DUMP);
			set_dumpflag(p, FLAG_DUMP);
		}
	}
#endif
	do_each_thread(g, p) {
		if (has_dumpflag(p, FLAG_DUMP) ||
		    ((p->flags & PF_KTHREAD) &&
		    (p->state & TASK_UNINTERRUPTIBLE))) {
			printk("U\n");
			clear_dumpflag(p, FLAG_DUMP);
			sched_show_task(p);
			set_dumpflag(p, FLAG_IS_DUMPED);
#if defined(CONFIG_CPM_MISC_SYSTEM_WATCHDOG)
			cpm_touch_syswd();
#endif
		}
	} while_each_thread(g, p);
	rcu_read_unlock();

	/* Take a snapshot of the uninterruptable,
	   next section could potentially obscure that info */
	kmsg_dump(KMSG_DUMP_OOPS);

	rcu_read_lock();
	do_each_thread(g, p) {
		if (!has_dumpflag(p, FLAG_IS_DUMPED) &&
		    (p->flags & PF_KTHREAD)) {

			/* All other processes should be stopped at this stage,
			   don't get_task_comm since it tries to take the
			   task lock, and panic has already done local_irq_disable()
			*/
			if (p->comm != NULL)
				strncpy(Xcomm, p->comm, TASK_COMM_LEN);
			Xcomm[TASK_COMM_LEN - 1] = 0;
			if (strstr(Xcomm, "irq")) {
				sched_show_task(p);
			}
#if defined(CONFIG_CPM_MISC_SYSTEM_WATCHDOG)
			cpm_touch_syswd();
#endif
		}
	} while_each_thread(g, p);

	rcu_read_unlock();
	printk("---- END DUMP BT-ALL\n");
}

/* Must be registered _before_ PSTORE */
static struct kmsg_dumper panic_debug = {
	.dump = panic_debug_dump,
	.max_reason = KMSG_DUMP_PANIC,
};

#if defined(CONFIG_BLK_DEV_PMEM)
static struct resource rbs_sys_pmem_resources[1];

static struct platform_device pmem_device = {
	.name			= "pmem",
	.id			= -1,
	.num_resources		= ARRAY_SIZE(rbs_sys_pmem_resources),
	.resource		= rbs_sys_pmem_resources
};

static int rbs_sys_pmem_devices = 0;

static void __init rbs_sys_pmem_set(phys_addr_t addr,
				    phys_addr_t len,
				    int vmalloc) {

	if (addr == 0)
		return;

	if (rbs_sys_pmem_devices == 1) {
		pr_warn("pmem: too many devices %d\n",
			rbs_sys_pmem_devices);
		return;
	}

	pmem_device.resource[rbs_sys_pmem_devices].start = addr;
	pmem_device.resource[rbs_sys_pmem_devices].end = addr + len - 1;
	pmem_device.resource[rbs_sys_pmem_devices].flags =
		IORESOURCE_MEM | (vmalloc & 0x1);
	rbs_sys_pmem_devices++;

	pr_info("pmem: memory block at [%#016llx-%#016llx]\n",
		(unsigned long long) addr,
		(unsigned long long) addr + len - 1);
}

static void __init rbs_sys_pmem_init(void)
{
	int i;
	phys_addr_t base = 0;
	phys_addr_t size = 0;

	if (rbs_sys_get_pmem_node_location("pmem", &base, &size))
		pr_err("pmem: dtb lookup failed\n");

	if (size) {
		rbs_sys_pmem_set(base, size, 1);
		for (i = 0; i < rbs_sys_pmem_devices; i++) {
			if (pmem_device.resource[i].start != 0)
				platform_device_register(&pmem_device);
		}
	} else {
		pr_err("pmem: memory area not defined in dtb\n");
	}
}

#endif

#if defined(CONFIG_PSTORE_RAM)
static struct ramoops_platform_data ramoops_pdata = {
	.mem_size = (unsigned long)0,
	.mem_address = (unsigned long)0,
	.record_size = 32768UL,
        .console_size = 614400UL,
	.ftrace_size = 4096UL,
	.dump_oops = 1,
	.ecc_info = {
		.block_size = 0,
		.ecc_size = 0,
		.symsize = 0,
		.poly = 0,
	}
};

static struct platform_device ramoops_plat_dev = {
        .name = "ramoops",
        .dev = {
                .platform_data = &ramoops_pdata,
        }
};

static int ramoops_setup(void) {
	phys_addr_t base;
	phys_addr_t size;
	int ret;

	if (rbs_sys_get_pmem_node_location("pstore", &base, &size))
		goto out_def;

	ramoops_pdata.mem_address = base;
	ramoops_pdata.mem_size = size;

	ret = platform_device_register(&ramoops_plat_dev);
	if (ret) {
		pr_warn("pstore: Could not create ramoops platform device: %d\n",
			ret);
		goto out_def;
	}

	return 0;

out_def:
	/* Something went wrong, graceful exit */
	pr_warn("pstore: No compatible  area found for ramoops");
	return 0;
}
#endif

int __init rbs_platform_init(void)
{
#if defined(CONFIG_BLK_DEV_PMEM)
	rbs_sys_pmem_init();
#endif
	kmsg_dump_register(&panic_debug);

#if defined(CONFIG_PSTORE_RAM)
	ramoops_setup();
#endif

	return 0;
}
device_initcall_sync(rbs_platform_init);

/*
 * RBS SYS internal API
 *
 * Copyright 2012 Ericsson AB
 * Kerstin Jonsson <kerstin.jonsson@ericsson.com>
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
#ifndef RBS_SYS_INTERNAL_H__
#define RBS_SYS_INTERNAL_H__

#include <linux/rbs/rbs-sys.h>

struct rbs_sys;

struct rbs_sys_bt_ops {
	int (*init)(struct rbs_sys *sys);
	void (*set_restart)(struct rbs_sys *sys,
				  struct rbs_sys_restart_data *data);
	int (*get_restart)(struct rbs_sys *sys,
				 struct rbs_sys_restart_data *data);
	int (*get_restart_reason)(struct rbs_sys *sys, unsigned int *reason);
	int (*get_fault)(struct rbs_sys *sys, unsigned long long *fault);
	int (*set_bootcount)(struct rbs_sys *sys, unsigned int count);
	int (*get_bootcount)(struct rbs_sys *sys, unsigned int *count);
	int (*mbox_map)(struct rbs_sys *sys, struct vm_area_struct *vma);
	int (*get_bootstarted)(struct rbs_sys *sys, unsigned int *bs);
	int (*get_pmem_node_location)(struct rbs_sys *sys, const char *node,
				      phys_addr_t *base, phys_addr_t *size);
	int (*set_upgrade)(struct rbs_sys *sys, int set);
	int (*get_wdtrace)(struct rbs_sys *sys, unsigned int *trace);
};

struct rbs_sys_bt_deps {
	struct rbs_sys_bt_ops *bt_ops;
	struct rbs_fn_id *bt_fn_depend;
};

struct rbs_sys_wd_ops {
	int (*set_cpu_mask)(struct cpumask *cpumask);
	int (*get_cpu_mask)(struct cpumask *cpumask);
	void (*touch_all)(void);
};

struct rbs_sys {
	struct mutex lock;
	int sys_state;
	unsigned int boot_count;
	struct rbs_sys_restart_data restart_data;

	void *board_data;
	struct rbs_sys_bt_ops *board;
	struct rbs_sys_wd_ops *wd;

	struct notifier_block sys_reboot_notifier;
	struct timer_list restart_delay;

	int api_ready;
	struct rbs_proto_mcgrp *mcgrp;

	uint32_t sys_cfg;
};

/**
 * rbs_sys_add_bt_ops
 * @dep: RBS board type ops and fn deps
 * @bt:  RBS board type ID
 */
extern void __init rbs_sys_add_bt_ops(struct rbs_sys_bt_deps *dep, enum rbs_sys_bt bt);

/**
 * rbs_sys_add_wd_ops
 */
extern void __init rbs_sys_add_wd_ops(struct rbs_sys_wd_ops *ops);
extern int rbs_sys_get_pmem_node_location(const char *node,
					  phys_addr_t *base,
					  phys_addr_t *size);

#endif

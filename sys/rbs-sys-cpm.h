/*
 * RBS system CPM1 common
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

#ifndef _RBS_SYS_CPM_H
#define _RBS_SYS_CPM_H

int rbs_sys_cpm_board_init(struct rbs_sys *sys);
void rbs_sys_cpm_board_set_restart(struct rbs_sys *sys,
				   struct rbs_sys_restart_data *data);
int rbs_sys_cpm_board_get_restart(struct rbs_sys *sys,
				  struct rbs_sys_restart_data *data);
int rbs_sys_cpm_board_get_restart_reason(struct rbs_sys *sys,
					 unsigned int *reason);
int rbs_sys_cpm_board_get_fault(struct rbs_sys *sys,
				unsigned long long *fault);
int rbs_sys_cpm_board_set_bootcount(struct rbs_sys *sys, unsigned int count);
int rbs_sys_cpm_board_get_bootcount(struct rbs_sys *sys, unsigned int *count);
int rbs_sys_cpm_board_get_bootstarted(struct rbs_sys *sys, unsigned int *bs);

int rbs_sys_cpm_board_mbox_map(struct rbs_sys *sys, struct vm_area_struct *vma);

int rbs_sys_cpm_board_get_pmem_node_location(struct rbs_sys *sys,
					     const char *node,
					     phys_addr_t *base,
					     phys_addr_t *size);
int rbs_sys_cpm_board_set_upgrade(struct rbs_sys *sys, int set);

extern struct rbs_sys_bt_ops rbs_sys_cpm_bt_ops;

#endif /* !_RBS_SYS_CPM_H */

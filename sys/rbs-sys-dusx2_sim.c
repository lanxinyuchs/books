/*
 * RBS system DUSx2 sim board
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

#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/reboot.h>
#include <linux/rbs/rbs-fn.h>
#include <linux/rbs/rbs-sys.h>

#include "rbs-sys-internal.h"

static struct rbs_fn_id rbs_sys_fn_deps[] = {
	RBS_FN_ID_NULL,
};

static int dusx2sim_board_init(struct rbs_sys *sys)
{
	return 0;
}

void dusx2sim_board_set_restart(struct rbs_sys *sys,
		struct rbs_sys_restart_data *data)
{
}

int dusx2sim_board_get_restart(struct rbs_sys *sys,
		struct rbs_sys_restart_data *data)
{
	return -1;
}

int dusx2sim_board_set_bootcount(struct rbs_sys *sys,
		unsigned int count)
{
	return 0;
}

int dusx2sim_board_get_bootcount(struct rbs_sys *sys,
		unsigned int *count)
{
	return -1;
}

struct rbs_sys_bt_ops dusx2sim_bt_ops = {
	.init = dusx2sim_board_init,
	.set_restart = dusx2sim_board_set_restart,
	.get_restart = dusx2sim_board_get_restart,
	.set_bootcount = dusx2sim_board_set_bootcount,
	.get_bootcount = dusx2sim_board_get_bootcount,
};

struct rbs_sys_bt_deps dusx2sim = {
	.bt_ops = &dusx2sim_bt_ops,
	.bt_fn_depend = rbs_sys_fn_deps,
};

static int __init dusx2sim_bt_init(void)
{
	rbs_sys_add_bt_ops(&dusx2sim, RBS_BT_DUSX2_SIM);
	return 0;
}
postcore_initcall_sync(dusx2sim_bt_init);

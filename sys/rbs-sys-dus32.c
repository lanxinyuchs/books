/*
 * RBS system DUS32 board
 *
 * Copyright 2013 Ericsson AB
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
#include "rbs-sys-cpm.h"

static struct rbs_fn_id rbs_sys_fn_deps[] = {
	RBS_FN_ID("icm", 0),
	RBS_FN_ID("rbs-time", 0),
	RBS_FN_ID_NULL,
};

static struct rbs_sys_bt_deps dus32_bt = {
	.bt_ops = &rbs_sys_cpm_bt_ops,
	.bt_fn_depend = rbs_sys_fn_deps,
};

static int __init dus32_bt_init(void)
{
	rbs_sys_add_bt_ops(&dus32_bt, RBS_BT_DUS32);
	return 0;
}
postcore_initcall_sync(dus32_bt_init);

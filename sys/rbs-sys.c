/*
 * RBS system X
 * (X is to be defined)
 *
 * Copyright 2012-2013 Ericsson AB
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

#define pr_fmt(fmt) "RBS Sys: " fmt

#include <linux/module.h>

#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/net.h>
#include <linux/slab.h>
#include <asm/uaccess.h>
#include <linux/skbuff.h>
#include <linux/types.h>
#include <linux/reboot.h>
#include <net/sock.h>
#include <linux/of.h>
#include <linux/of_fdt.h>

#include <linux/rbs/rbs-sys.h>
#include <linux/rbs/rbs-error.h>
#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-fn.h>

#include "rbs-sys-internal.h"
#if defined(CONFIG_COMPAT)
#include "rbs-sys-compat.h"
#endif

#define RCS_BT_NODEPATH	"/rcs_nodes/board_info"

#define RBS_SYSWDTRACE_UN       0

static int flat_dt_size = 0;

static struct rbs_sys *rbs_sys = NULL;
static enum rbs_sys_bt board_type = RBS_BT_MAX;
static struct rbs_sys_bt_deps *board[RBS_BT_MAX] = {0};
static struct rbs_sys_wd_ops *rbs_sys_wd = NULL;

/* Obs, definition of enum rbs_sys_bt in rbs-sys.h */
static const char *board_type_str[RBS_BT_MAX + 1] = {
	"tp1",
	"dus",
	"duw2",
	"dul",
	"dul_tb",
	"dustb",
	"tp5",
	"duw31",
	"dusx2_sim",
	"tcu03",
	"dus32",
	"dus52",
	"tcu04",
	"dus33",
	"dus53",
	"mdusx3",
	"c608",
	"dus52tk",
	"dus32tk",
	"unknown",
	"RBS_BT_UNSUPPORTED"
};

static char *rbs_sys_state2str(int state)
{
	switch (state) {
	case RBS_SYS_STARTING: return "STARTING";
	case RBS_SYS_OPERATIONAL: return "OPERATIONAL";
	case RBS_SYS_DEGRADED: return "DEGRADED";
	case RBS_SYS_FAULT: return "FAULT";
	case RBS_SYS_RESTARTING: return "RESTARTING";
	default:
		break;
	}
	return "UNKNOWN";
}

static void __rbs_sys_set_state(struct rbs_sys *sys, int state)
{
	struct rbs_sys_event *evt;
	struct sk_buff *skb;

	char *state_str = rbs_sys_state2str(state);

	sys->sys_state = state;

	pr_info("%s\n", state_str);
	rbs_error_t(RBS_ERRSRC_SYS, 2 /* XXX */, state_str);

	if (!sys->api_ready)
		return;

	/* notify socket users */
	skb = alloc_skb(sizeof(*evt), GFP_KERNEL);
	if (!skb) {
		WARN_ONCE(1, "unable to allocate skb\n");
		return;
	}
	evt = (struct rbs_sys_event *) skb_put(skb, sizeof(*evt));
	evt->event = RBS_SYSEVT_STATECHG;
	evt->d.sys_state = state;

	if (rbs_proto_mcgrp_send(sys->mcgrp, skb)) {
		WARN_ONCE(1, "unable to send skb\n");
	}
}

static void __rbs_sys_set_restart(struct rbs_sys *sys,
		struct rbs_sys_restart_data *data)
{
	if (rbs_sys->board->set_restart)
		rbs_sys->board->set_restart(sys, data);
	sys->restart_data = *data;
}

static void __rbs_sys_get_restart(struct rbs_sys *sys,
		struct rbs_sys_restart_data *data)
{
	if (!rbs_sys->board->get_restart ||
	    rbs_sys->board->get_restart(sys, &sys->restart_data)) {
		/* set defaults */
		memset(data, 0, sizeof(*data));
		data->type = RBS_SYSRESTART_COLD;
	}
}

static void __rbs_sys_restart_delay(unsigned long data)
{
	struct rbs_sys *sys = (struct rbs_sys *) data;

	if (sys->sys_state != RBS_SYS_RESTARTING)
		return;

	kernel_restart(NULL);
}

int rbs_sys_set_state(int state)
{
	mutex_lock(&rbs_sys->lock);

	if (rbs_sys->sys_state == state)
		return 0;

	switch (state) {
	case RBS_SYS_STARTING:
	case RBS_SYS_OPERATIONAL:
	case RBS_SYS_DEGRADED:
	case RBS_SYS_FAULT:
	case RBS_SYS_RESTARTING:
		break;
	default:
		mutex_unlock(&rbs_sys->lock);
		return -EINVAL;
	}

	__rbs_sys_set_state(rbs_sys, state);
	mutex_unlock(&rbs_sys->lock);

	return 0;
}
EXPORT_SYMBOL(rbs_sys_set_state);

int rbs_sys_get_state(void)
{
	return rbs_sys->sys_state;
}
EXPORT_SYMBOL(rbs_sys_get_state);

int rbs_sys_order_restart(int delay /* ms */,
		unsigned int type,
		unsigned int ecode,
		unsigned int extra1,
		unsigned int extra2)
{
	struct rbs_sys_restart_data rdata = {
		.type = type,
		.ecode = ecode,
		.extra1 = extra1,
		.extra2 = extra2,
	};

	mutex_lock(&rbs_sys->lock);
	if (rbs_sys->sys_state == RBS_SYS_RESTARTING) {
		mutex_unlock(&rbs_sys->lock);
		return -EALREADY;
	}

	__rbs_sys_set_restart(rbs_sys, &rdata);
	__rbs_sys_set_state(rbs_sys, RBS_SYS_RESTARTING);
	mutex_unlock(&rbs_sys->lock);

	if (rbs_sys->wd)
		rbs_sys->wd->touch_all();

	if (delay <= 0)
		kernel_restart(NULL);
	else {
		mod_timer(&rbs_sys->restart_delay,
				jiffies + msecs_to_jiffies(delay));

		printk(KERN_EMERG "Restart in %d ms\n", delay);
	}
	return 0;
}
EXPORT_SYMBOL(rbs_sys_order_restart);

void rbs_sys_get_restart(struct rbs_sys_restart_data *data)
{
	if (!data)
		return;

	mutex_lock(&rbs_sys->lock);
	*data = rbs_sys->restart_data;
	mutex_unlock(&rbs_sys->lock);
}
EXPORT_SYMBOL(rbs_sys_get_restart);

unsigned int rbs_sys_get_restart_reason(void)
{
	unsigned int reason;

	mutex_lock(&rbs_sys->lock);
	if (!rbs_sys->board->get_restart_reason ||
	    rbs_sys->board->get_restart_reason(rbs_sys, &reason))
		reason = 0;
	mutex_unlock(&rbs_sys->lock);
	return reason;
}
EXPORT_SYMBOL(rbs_sys_get_restart_reason);

unsigned long long rbs_sys_get_fault(void)
{
	unsigned long long fault;

	mutex_lock(&rbs_sys->lock);
	if (!rbs_sys->board->get_fault ||
	    rbs_sys->board->get_fault(rbs_sys, &fault))
		fault = 0;
	mutex_unlock(&rbs_sys->lock);
	return fault;
}
EXPORT_SYMBOL(rbs_sys_get_fault);

unsigned int rbs_sys_get_bootcount(void)
{
	unsigned int bc;

	mutex_lock(&rbs_sys->lock);
	if (rbs_sys->board->get_bootcount)
		rbs_sys->board->get_bootcount(rbs_sys, &rbs_sys->boot_count);
	bc = rbs_sys->boot_count;
	mutex_unlock(&rbs_sys->lock);

	return bc;
}
EXPORT_SYMBOL(rbs_sys_get_bootcount);

unsigned int rbs_sys_get_bootstarted(void)
{
	unsigned int bs;

	if (!rbs_sys->board->get_bootstarted ||
	    rbs_sys->board->get_bootstarted(rbs_sys, &bs))
		bs = RBS_SYSBOOT_UN;

	return bs;
}
EXPORT_SYMBOL(rbs_sys_get_bootstarted);

void rbs_sys_set_bootcount(unsigned int count)
{
	mutex_lock(&rbs_sys->lock);
	rbs_sys->boot_count = count;
	if (rbs_sys->board->set_bootcount)
		rbs_sys->board->set_bootcount(rbs_sys, count);
	mutex_unlock(&rbs_sys->lock);
}
EXPORT_SYMBOL(rbs_sys_set_bootcount);

int rbs_sys_get_pmem_node_location(const char *node,
				   phys_addr_t *base,
				   phys_addr_t *size)
{
	if (rbs_sys->board->get_pmem_node_location)
		return rbs_sys->board->get_pmem_node_location(rbs_sys,
							      node,
							      base,
							      size);
	return -ENOENT;
}
EXPORT_SYMBOL(rbs_sys_get_pmem_node_location);

void rbs_sys_set_upgrade(int set)
{
	mutex_lock(&rbs_sys->lock);
	if (rbs_sys->board->set_upgrade)
		rbs_sys->board->set_upgrade(rbs_sys, set);
	mutex_unlock(&rbs_sys->lock);
}
EXPORT_SYMBOL(rbs_sys_set_upgrade);

static int rbs_sys_restart_event(struct notifier_block *nb,
		unsigned long event,
		void *ptr)
{
	struct rbs_sys_restart_data rdata;
	struct rbs_sys *sys = container_of(nb, struct rbs_sys,
			sys_reboot_notifier);

	/* check if ordered by us */
	if (sys->sys_state == RBS_SYS_RESTARTING)
		return NOTIFY_DONE;

	printk(KERN_EMERG "restart ordered not via rbs_sys...\n");

	if (sys->wd)
		sys->wd->touch_all();

	rdata.type = RBS_SYSRESTART_COLD;
	rdata.ecode = 0x0; /* XXX */
	rdata.extra1 = 0x0;
	rdata.extra2 = 0x0;

	mutex_lock(&sys->lock);
	__rbs_sys_set_restart(sys, &rdata);
	__rbs_sys_set_state(sys, RBS_SYS_RESTARTING);
	mutex_unlock(&sys->lock);

	return NOTIFY_DONE;
}

static unsigned int rbs_sys_get_wdtrace(void)
{
	unsigned int trace;

	if (!rbs_sys->board->get_wdtrace ||
	    rbs_sys->board->get_wdtrace(rbs_sys, &trace))
		trace = RBS_SYSWDTRACE_UN;

	return trace;
}

void __init rbs_sys_add_bt_ops(struct rbs_sys_bt_deps *dep, enum rbs_sys_bt bt)
{
	pr_debug("%s: add board ops %d\n", __func__, bt);

	if (WARN_ON_ONCE(!dep || !dep->bt_ops || bt < 0 || bt >= RBS_BT_MAX)) {
		pr_warn("%s: invalid indata\n", __func__);
		return;
	}
	if (WARN_ON_ONCE(board[bt])) {
		pr_warn("%s: board type %d already set\n", __func__, bt);
		return;
	}
	board[bt] = dep;
}

void __init rbs_sys_add_wd_ops(struct rbs_sys_wd_ops *ops)
{
	pr_debug("%s: add watchdog ops\n", __func__);

	if (WARN_ON_ONCE(!ops)) {
		pr_warn("%s: invalid indata\n", __func__);
		return;
	}
	if (WARN_ON_ONCE(rbs_sys_wd)) {
		pr_warn("%s: wd already set\n", __func__);
		return;
	}
	rbs_sys_wd = ops;
}

static int __init rbs_sys_init(void)
{
	rbs_sys = kmalloc(sizeof(*rbs_sys), GFP_KERNEL);
	if (!rbs_sys)
		panic("Unable to allocate rbs_sys state area\n");

	if (board_type == RBS_BT_MAX || !board[board_type])
		panic("Board type not set/supported: board_type %d\n",
		      board_type);

	rbs_sys->board = board[board_type]->bt_ops;
	rbs_sys->wd = rbs_sys_wd;
	mutex_init(&rbs_sys->lock);
	rbs_sys->board_data = NULL;
	rbs_sys->api_ready = 0;
	rbs_sys->sys_state = RBS_SYS_STARTING;
	rbs_sys->sys_reboot_notifier.notifier_call = rbs_sys_restart_event;
	rbs_sys->sys_cfg = 0; /* default */

	init_timer(&rbs_sys->restart_delay);
	rbs_sys->restart_delay.function = __rbs_sys_restart_delay;
	rbs_sys->restart_delay.data = (unsigned long) rbs_sys;

	if (rbs_sys->board->init && rbs_sys->board->init(rbs_sys))
		panic("Board specific initialization failed\n");

	/* fetch restart information from HW/uboot */
	__rbs_sys_get_restart(rbs_sys, &rbs_sys->restart_data);
	if (!rbs_sys->board->get_bootcount ||
	    rbs_sys->board->get_bootcount(rbs_sys, &rbs_sys->boot_count))
		rbs_sys->boot_count = 0;

	/* for logging purpose */
	__rbs_sys_set_state(rbs_sys, RBS_SYS_STARTING);

	register_reboot_notifier(&rbs_sys->sys_reboot_notifier);

	return 0;
}
arch_initcall(rbs_sys_init);

static int rbs_sys_ioctl_getdtprop_body(char *node, char *pname,
					int *size, void __user *dst)
{
	struct rbs_sys_ioc_getdtprop req;
	struct device_node *np;
	struct property *prop;
	int psize, osize;

	node[sizeof(req.node_path) - 1] = 0;
	pname[sizeof(req.prop_name) - 1] = 0;

	if (node[0] != '/') {
		np = of_find_node_by_name(NULL, "aliases");
		if (!np)
			return -ENOENT;
		prop = of_find_property(np, node, NULL);
		if (!prop || !prop->value) {
			of_node_put(np);
			return -ENOENT;
		}
		strncpy(node, (char *) prop->value,
				sizeof(req.node_path));
		of_node_put(np);
	}

	np = of_find_node_by_path(node);
	if (!np)
		return -ENOENT;

	prop = of_find_property(np, pname, &psize);
	if (!prop) {
		of_node_put(np);
		return -ENOENT;
	}
	if (!prop->value) {
		of_node_put(np);
		return -ENODATA;
	}

	osize = *size;
	*size = psize;

	if (dst) {
		if (psize < osize)
			osize = psize;
		if (copy_to_user(dst, prop->value, osize)) {
			of_node_put(np);
			return -EFAULT;
		}
	}

	of_node_put(np);

	return 0;
}

static int rbs_sys_ioctl_getdt_body(void __user *ublob, int *size)
{
	unsigned long usize;

	usize = *size;
	*size = flat_dt_size;

	if (ublob) {
		if (usize > *size)
			usize = *size;
		if (copy_to_user(ublob,
				 initial_boot_params, usize))
			return -EFAULT;
	}

	return 0;
}

static int rbs_sys_ioctl_setwdcpumask_body(struct rbs_sys *sys,
					   void __user *src, int *size)
{
	cpumask_var_t cpumask;
	int ret;

	if (!alloc_cpumask_var(&cpumask, GFP_KERNEL))
		return -ENOMEM;

	if (*size < cpumask_size())
		cpumask_clear(cpumask);
	if (*size > cpumask_size())
		*size = cpumask_size();

	ret = copy_from_user(cpumask, src, *size);
	if (ret)
		goto done;

	mutex_lock(&sys->lock);

	ret = sys->wd->set_cpu_mask(cpumask);

	mutex_unlock(&sys->lock);
done:
	free_cpumask_var(cpumask);
	return ret;
}

static int rbs_sys_ioctl_getwdcpumask_body(struct rbs_sys *sys,
					   void __user *dst, int *size)
{
	cpumask_var_t cpumask;
	int ret;

	if (!alloc_cpumask_var(&cpumask, GFP_KERNEL))
		return -ENOMEM;

	mutex_lock(&sys->lock);

	ret = sys->wd->get_cpu_mask(cpumask);

	mutex_unlock(&sys->lock);

	if (*size > cpumask_size())
		*size = cpumask_size();

	ret = copy_to_user(dst, cpumask, *size);

	free_cpumask_var(cpumask);
	return ret;
}

static int rbs_sys_sock_init(struct rbs_sock *rsk, void *init_param)
{
	rsk->proto_data = init_param;
	return 0;
}

static void rbs_sys_sock_done(struct rbs_sock *rsk)
{
	struct rbs_sys *sys = rsk->proto_data;

	rbs_proto_mcgrp_remove(sys->mcgrp, rsk);
	rsk->proto_data = NULL;
}

static int rbs_sys_ioctl_getstate(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_state param;

	param.sys_state = rbs_sys_get_state();

	if (copy_to_user(arg, &param, sizeof(param)))
		return -EFAULT;

	return 0;
}

static int rbs_sys_ioctl_setstate(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_state param;

	if (copy_from_user(&param, arg, sizeof(param)))
		return -EFAULT;

	return rbs_sys_set_state(param.sys_state);
}

static int rbs_sys_ioctl_substate(struct rbs_sys *sys, struct rbs_sock *rsk,
		void __user *arg)
{
	int on, state;
	int ret = 0;

	if (copy_from_user(&on, arg, sizeof(on)))
		return -EFAULT;

	mutex_lock(&sys->lock);
	state = rbs_sys_get_state();
	if (on)
		ret = rbs_proto_mcgrp_add(sys->mcgrp, rsk);
	else
		rbs_proto_mcgrp_remove(sys->mcgrp, rsk);
	mutex_unlock(&sys->lock);

	if (!on)
		skb_queue_purge(&rsk->sk.sk_receive_queue);

	if (!ret && copy_to_user(arg, &state, sizeof(state)))
		return -EFAULT;

	return ret;
}

static int rbs_sys_ioctl_getrestartreason(struct rbs_sys *sys, void __user *arg)
{
	unsigned int reason;

	reason = rbs_sys_get_restart_reason();

	if (copy_to_user(arg, &reason, sizeof(reason)))
		return -EFAULT;

	return 0;
}

static int rbs_sys_ioctl_getfault(struct rbs_sys *sys, void __user *arg)
{
	unsigned long long fault;

	fault = rbs_sys_get_fault();

	if (copy_to_user(arg, &fault, sizeof(fault)))
		return -EFAULT;

	return 0;
}

static int rbs_sys_ioctl_getrestart(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_restart_data param;

	rbs_sys_get_restart(&param);

	if (copy_to_user(arg, &param, sizeof(param)))
		return -EFAULT;

	return 0;
}

static int rbs_sys_ioctl_orderrestart(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_rstorder param;
	char buf[TASK_COMM_LEN] = {0,};

	if (copy_from_user(&param, arg, sizeof(param)))
		return -EFAULT;

	/* XXX for debug purpose only, to be removed */
	get_task_comm(buf, current);
	pr_info("Restart ordered by %s(0x%x)\n", buf, current->pid);

	return rbs_sys_order_restart(param.delay,
			param.data.type, param.data.ecode,
			param.data.extra1, param.data.extra2);
}

static int rbs_sys_ioctl_setbootcount(struct rbs_sys *sys, void __user *arg)
{
	unsigned int count;

	if (copy_from_user(&count, arg, sizeof(count)))
		return -EFAULT;

	rbs_sys_set_bootcount(count);
	return 0;
}

static int rbs_sys_ioctl_getbootcount(struct rbs_sys *sys, void __user *arg)
{
	unsigned int count;

	count = rbs_sys_get_bootcount();

	if (copy_to_user(arg, &count, sizeof(count)))
		return -EFAULT;
	return 0;
}

static int rbs_sys_ioctl_getbootstarted(struct rbs_sys *sys, void __user *arg)
{
	unsigned int bs;

	bs = rbs_sys_get_bootstarted();

	if (copy_to_user(arg, &bs, sizeof(bs)))
		return -EFAULT;
	return 0;
}

static int rbs_sys_ioctl_setupgrade(struct rbs_sys *sys, void __user *arg)
{
	int set;

	if (copy_from_user(&set, arg, sizeof(set)))
		return -EFAULT;

	rbs_sys_set_upgrade(set);
	return 0;
}

static int rbs_sys_ioctl_getbt(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_getbt bt;

	bt.type = rbs_sys_board_type();

	if (copy_to_user(arg, &bt, sizeof(bt)))
		return -EFAULT;
	return 0;
}

static int rbs_sys_ioctl_getdtprop(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_getdtprop req;
	void __user *val = NULL;
	int ret;

	if (copy_from_user(&req, arg, sizeof(req)))
		return -EFAULT;

	if (req.size < 0)
		return -EFAULT;

	if (req.val) {
		val = (void __user *) req.val;
		if (!access_ok(VERIFY_WRITE, val, req.size))
			return -EFAULT;
	}

	/* Ok to proceed even if req.val == NULL, the function is called
	 * twice for each queried property, the first time to fetch the
	 * size of the property and then req.val _is_ NULL. The guard against
	 * dereferencing a NULL-pointer is done in rbs_sys_ioctl_getdtprop_body.
	 */

	ret = rbs_sys_ioctl_getdtprop_body(req.node_path, req.prop_name,
					   &req.size, val);
	if (!ret)
		ret = copy_to_user(arg, &req, sizeof(req));

	return ret;
}

static int rbs_sys_ioctl_getdt(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_getdt dt;
	void __user *ublob;
	int ret;

	if (copy_from_user(&dt, arg, sizeof(dt)))
		return -EFAULT;

	ublob = dt.blob;
	ret = rbs_sys_ioctl_getdt_body(ublob, &dt.size);

	if (!ret)
		ret = copy_to_user(arg, &dt, sizeof(dt));

	return ret;
}

static int rbs_sys_ioctl_setwdcpumask(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_wdcpumask ioc;
	int ret;

	if (!sys->wd)
		return -ENOSYS;

	ret = copy_from_user(&ioc, arg, sizeof(ioc));
	if (ret)
		return ret;

	return rbs_sys_ioctl_setwdcpumask_body(sys, (__user void *) ioc.cpu_set,
					       &ioc.size);
}

static int rbs_sys_ioctl_getwdcpumask(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_ioc_wdcpumask ioc;
	int ret;

	if (!sys->wd)
		return -ENOSYS;

	ret = copy_from_user(&ioc, arg, sizeof(ioc));
	if (ret)
		return ret;

	return rbs_sys_ioctl_getwdcpumask_body(sys, (void __user *) ioc.cpu_set,
					       &ioc.size);
}

static int rbs_sys_ioctl_setsyscfg(struct rbs_sys *sys, void __user *arg)
{
	uint32_t n_scfg;
	int ret;

	ret = copy_from_user(&n_scfg, arg, sizeof(n_scfg));
	if (ret)
		return ret;

	mutex_lock(&sys->lock);
	sys->sys_cfg = n_scfg;
	mutex_unlock(&sys->lock);

	return 0;
}

static int rbs_sys_ioctl_getsyscfg(struct rbs_sys *sys, void __user *arg)
{
	uint32_t scfg;
	int ret;

	mutex_lock(&sys->lock);
	scfg = sys->sys_cfg;
	mutex_unlock(&sys->lock);

	ret = copy_to_user(arg, &scfg, sizeof(scfg));
	if (ret)
		return ret;

	return 0;
}

#if defined(CONFIG_COMPAT)
static int rbs_sys_ioctl_compat_getdtprop(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_compat_getdtprop req;
	void __user *val = NULL;
	int ret;

	if (copy_from_user(&req, arg, sizeof(req)))
		return -EFAULT;

	if (req.size < 0)
		return -EFAULT;

	if (req.val) {
		val = compat_ptr(req.val);
		if (!access_ok(VERIFY_WRITE, val, req.size))
			return -EFAULT;
	}

	/* Ok to proceed even if req.val == NULL, the function is called
	 * twice for each queried property, the first time to fetch the
	 * size of the property and then req.val _is_ NULL. The guard against
	 * dereferencing a NULL-pointer is done in rbs_sys_ioctl_getdtprop_body.
	 */

	ret = rbs_sys_ioctl_getdtprop_body(req.node_path, req.prop_name,
					   &req.size, val);
	if (!ret)
		ret = copy_to_user(arg, &req, sizeof(req));

	return ret;
}

static int rbs_sys_ioctl_compat_getdt(struct rbs_sys *sys, void __user *arg)
{
	struct rbs_sys_compat_getdt dt;
	void __user *ublob;
	int ret;

	if (copy_from_user(&dt, arg, sizeof(dt)))
		return -EFAULT;

	ublob = compat_ptr(dt.blob);
	ret = rbs_sys_ioctl_getdt_body(ublob, &dt.size);

	if (!ret)
		ret = copy_to_user(arg, &dt, sizeof(dt));

	return ret;
}

static int rbs_sys_ioctl_compat_setwdcpumask(struct rbs_sys *sys,
					     void __user *arg)
{
	struct rbs_sys_compat_wdcpumask ioc;
	int ret;

	if (!sys->wd)
		return -ENOSYS;

	ret = copy_from_user(&ioc, arg, sizeof(ioc));
	if (ret)
		return ret;

	return rbs_sys_ioctl_setwdcpumask_body(sys, compat_ptr(ioc.cpu_set),
					       &ioc.size);
}

static int rbs_sys_ioctl_compat_getwdcpumask(struct rbs_sys *sys,
					     void __user *arg)
{
	struct rbs_sys_compat_wdcpumask ioc;
	int ret;

	if (!sys->wd)
		return -ENOSYS;

	ret = copy_from_user(&ioc, arg, sizeof(ioc));
	if (ret)
		return ret;

	return rbs_sys_ioctl_getwdcpumask_body(sys, compat_ptr(ioc.cpu_set),
					       &ioc.size);
}

static int rbs_sys_sock_compat_ioctl(struct rbs_sock *rsk, unsigned int cmd,
		unsigned long arg)
{
	struct rbs_sys *sys = rsk->proto_data;
	int ret = 0;
	void __user *argp = (void __user *)arg;

	switch (cmd) {
	case RBS_SYSIOC_SUBSCRIBESTATE:
		ret = rbs_sys_ioctl_substate(sys, rsk, argp);
		break;
	case RBS_SYSIOC_GETSTATE:
		ret = rbs_sys_ioctl_getstate(sys, argp);
		break;
	case RBS_SYSIOC_FORCESTATE:
		/* XXX permissions??? */
		ret = rbs_sys_ioctl_setstate(sys, argp);
		break;
	case RBS_SYSIOC_GETRESTART:
		ret = rbs_sys_ioctl_getrestart(sys, argp);
		break;
	case RBS_SYSIOC_GETRESTARTREASON:
		ret = rbs_sys_ioctl_getrestartreason(sys, argp);
		break;
	case RBS_SYSIOC_GETFAULT:
		ret = rbs_sys_ioctl_getfault(sys, argp);
		break;
	case RBS_SYSIOC_ORDERRESTART:
		/* XXX permissions??? */
		ret = rbs_sys_ioctl_orderrestart(sys, argp);
		break;
	case RBS_SYSIOC_SETBOOTCOUNT:
		ret = rbs_sys_ioctl_setbootcount(sys, argp);
		break;
	case RBS_SYSIOC_GETBOOTCOUNT:
		ret = rbs_sys_ioctl_getbootcount(sys, argp);
		break;
	case RBS_SYSIOC_GETBOOTSTARTED:
		ret = rbs_sys_ioctl_getbootstarted(sys, argp);
		break;
	case RBS_SYSIOC_SETUPGRADE:
		ret = rbs_sys_ioctl_setupgrade(sys, argp);
		break;
	case RBS_SYS_COMPAT_GETDTPROP:
		ret = rbs_sys_ioctl_compat_getdtprop(sys, argp);
		break;
	case RBS_SYSIOC_GETBT:
		ret = rbs_sys_ioctl_getbt(sys, argp);
		break;
	case RBS_SYS_COMPAT_GETDT:
		ret = rbs_sys_ioctl_compat_getdt(sys, argp);
		break;
	case RBS_SYS_COMPAT_SETWDCPUMASK:
		ret = rbs_sys_ioctl_compat_setwdcpumask(sys, argp);
		break;
	case RBS_SYS_COMPAT_GETWDCPUMASK:
		ret = rbs_sys_ioctl_compat_getwdcpumask(sys, argp);
		break;
	case RBS_SYSIOC_SETSYSCFG:
		ret = rbs_sys_ioctl_setsyscfg(sys, argp);
		break;
	case RBS_SYSIOC_GETSYSCFG:
		ret = rbs_sys_ioctl_getsyscfg(sys, argp);
		break;
	default:
		ret = -ENOSYS;
		break;
	}

	return ret;
}

#endif /* CONFIG_COMPAT */

static int rbs_sys_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
		unsigned long arg)
{
	struct rbs_sys *sys = rsk->proto_data;
	int ret = 0;
	void __user *argp = (void __user *) arg;

	switch (cmd) {
	case RBS_SYSIOC_SUBSCRIBESTATE:
		ret = rbs_sys_ioctl_substate(sys, rsk, argp);
		break;
	case RBS_SYSIOC_GETSTATE:
		ret = rbs_sys_ioctl_getstate(sys, argp);
		break;
	case RBS_SYSIOC_FORCESTATE:
		/* XXX permissions??? */
		ret = rbs_sys_ioctl_setstate(sys, argp);
		break;
	case RBS_SYSIOC_GETRESTART:
		ret = rbs_sys_ioctl_getrestart(sys, argp);
		break;
	case RBS_SYSIOC_ORDERRESTART:
		/* XXX permissions??? */
		ret = rbs_sys_ioctl_orderrestart(sys, argp);
		break;
	case RBS_SYSIOC_GETRESTARTREASON:
		ret = rbs_sys_ioctl_getrestartreason(sys, argp);
		break;
	case RBS_SYSIOC_GETFAULT:
		ret = rbs_sys_ioctl_getfault(sys, argp);
		break;
	case RBS_SYSIOC_SETBOOTCOUNT:
		ret = rbs_sys_ioctl_setbootcount(sys, argp);
		break;
	case RBS_SYSIOC_GETBOOTCOUNT:
		ret = rbs_sys_ioctl_getbootcount(sys, argp);
		break;
	case RBS_SYSIOC_GETBOOTSTARTED:
		ret = rbs_sys_ioctl_getbootstarted(sys, argp);
		break;
	case RBS_SYSIOC_SETUPGRADE:
		ret = rbs_sys_ioctl_setupgrade(sys, argp);
		break;
	case RBS_SYSIOC_GETDTPROP:
		ret = rbs_sys_ioctl_getdtprop(sys, argp);
		break;
	case RBS_SYSIOC_GETBT:
		ret = rbs_sys_ioctl_getbt(sys, argp);
		break;
	case RBS_SYSIOC_GETDT:
		ret = rbs_sys_ioctl_getdt(sys, argp);
		break;
	case RBS_SYSIOC_SETWDCPUMASK:
		ret = rbs_sys_ioctl_setwdcpumask(sys, argp);
		break;
	case RBS_SYSIOC_GETWDCPUMASK:
		ret = rbs_sys_ioctl_getwdcpumask(sys, argp);
		break;
	case RBS_SYSIOC_SETSYSCFG:
		ret = rbs_sys_ioctl_setsyscfg(sys, argp);
		break;
	case RBS_SYSIOC_GETSYSCFG:
		ret = rbs_sys_ioctl_getsyscfg(sys, argp);
		break;
	default:
		ret = -ENOSYS;
		break;
	}

	return ret;
}

static int rbs_sys_sock_mmap(struct rbs_sock *rsk,
		struct vm_area_struct *vma)
{
	struct rbs_sys *sys = rsk->proto_data;

	if (!sys->board || !sys->board->mbox_map)
		return -EOPNOTSUPP;

	return sys->board->mbox_map(sys, vma);
}

static struct rbs_proto_ops rbs_sys_proto_ops = {
	.init = rbs_sys_sock_init,
	.done = rbs_sys_sock_done,
#if defined(CONFIG_COMPAT)
	.compat_ioctl = rbs_sys_sock_compat_ioctl,
#endif
	.ioctl = rbs_sys_sock_ioctl,
	.mmap = rbs_sys_sock_mmap,
};

static int rbs_sys_fn_setup(struct rbs_fn *fn)
{
	mutex_lock(&rbs_sys->lock);
	switch (rbs_sys->sys_state) {
	case RBS_SYS_FAULT: /* XXX */
		break;
	case RBS_SYS_STARTING:
	case RBS_SYS_DEGRADED:
		__rbs_sys_set_state(rbs_sys, RBS_SYS_OPERATIONAL);
		break;
	case RBS_SYS_OPERATIONAL:
	case RBS_SYS_RESTARTING:
	default:
		break;
	}
	mutex_unlock(&rbs_sys->lock);
	return 0;
}

static void rbs_sys_fn_release(struct rbs_fn *fn)
{
	mutex_lock(&rbs_sys->lock);
	switch (rbs_sys->sys_state) {
	case RBS_SYS_OPERATIONAL:
		__rbs_sys_set_state(rbs_sys, RBS_SYS_DEGRADED);
		break;
	case RBS_SYS_FAULT: /* XXX */
		break;
	case RBS_SYS_STARTING:
	case RBS_SYS_DEGRADED:
	default:
		break;
	}
	mutex_unlock(&rbs_sys->lock);
}

static struct rbs_fn_ops rbs_sys_fn_ops = {
	.setup = rbs_sys_fn_setup,
	.release = rbs_sys_fn_release,
};
static ssize_t board_type_show(struct rbs_fn *fn,
			       struct rbs_fn_attribute *attr,
			       char *buf)
{
	return sprintf(buf, "%s\n", board_type_str[board_type]);
}
static ssize_t board_state_show(struct rbs_fn *fn,
				struct rbs_fn_attribute *attr,
				char *buf)
{
	return sprintf(buf, "%s\n", rbs_sys_state2str(rbs_sys_get_state()));
}
static ssize_t boot_count_show(struct rbs_fn *fn,
				struct rbs_fn_attribute *attr,
				char *buf)
{
	return sprintf(buf, "%u\n", rbs_sys_get_bootcount());
}
static ssize_t boot_started_show(struct rbs_fn *fn,
				struct rbs_fn_attribute *attr,
				char *buf)
{
	unsigned int bs = rbs_sys_get_bootstarted();

	switch (bs) {
	case RBS_SYSBOOT_CF:
		return sprintf(buf, "cf\n");
	case RBS_SYSBOOT_FB:
		return sprintf(buf, "fb\n");
	case RBS_SYSBOOT_NL:
		return sprintf(buf, "nl\n");
	case RBS_SYSBOOT_NL3:
		return sprintf(buf, "nl3\n");
	case RBS_SYSBOOT_UP:
		return sprintf(buf, "up\n");
	case RBS_SYSBOOT_UN:
	default:
		break;
	}

	return sprintf(buf, "un\n");
}
static ssize_t restart_type_show(struct rbs_fn *fn,
				struct rbs_fn_attribute *attr,
				char *buf)
{
	struct rbs_sys_restart_data param;
	rbs_sys_get_restart(&param);
	return sprintf(buf, "%u\n", (unsigned int) param.type);
}

static ssize_t restart_reason_show(struct rbs_fn *fn,
				  struct rbs_fn_attribute *attr,
				  char *buf)
{
	return sprintf(buf, "0x%x\n", rbs_sys_get_restart_reason());
}

static ssize_t fault_show(struct rbs_fn *fn,
			  struct rbs_fn_attribute *attr,
			  char *buf)
{
	return sprintf(buf, "0x%llx\n", rbs_sys_get_fault());
}

static ssize_t dumps_location_show(struct rbs_fn *fn,
				   struct rbs_fn_attribute *attr,
				   char *buf)
{
	phys_addr_t base;
	phys_addr_t size;
	int ret = rbs_sys_get_pmem_node_location("dumps", &base, &size);

	if (!ret)
		return sprintf(buf, "0x%llx %llu\n", base, size);

	return ret;
}

static ssize_t trace_location_show(struct rbs_fn *fn,
				   struct rbs_fn_attribute *attr,
				   char *buf)
{
	phys_addr_t base;
	phys_addr_t size;
	int ret = rbs_sys_get_pmem_node_location("trace", &base, &size);

	if (!ret)
		return sprintf(buf, "0x%llx %llu\n", base, size);

	return ret;
}

static ssize_t wdtrace_show(struct rbs_fn *fn,
			    struct rbs_fn_attribute *attr,
			    char *buf)
{
	return sprintf(buf, "%x\n", rbs_sys_get_wdtrace());
}

static struct rbs_fn_attribute rbs_sys_attr[] = {
	_RBS_FN_ATTR(board_type, S_IRUGO, board_type_show, NULL),
	_RBS_FN_ATTR(board_state, S_IRUGO, board_state_show, NULL),
	_RBS_FN_ATTR(boot_count, S_IRUGO, boot_count_show, NULL),
	_RBS_FN_ATTR(restart_type, S_IRUGO, restart_type_show, NULL),
	_RBS_FN_ATTR(restart_reason, S_IRUGO, restart_reason_show, NULL),
	_RBS_FN_ATTR(fault, S_IRUGO, fault_show, NULL),
	_RBS_FN_ATTR(boot_started, S_IRUGO, boot_started_show, NULL),
	_RBS_FN_ATTR(dumps_location, S_IRUGO, dumps_location_show, NULL),
	_RBS_FN_ATTR(trace_location, S_IRUGO, trace_location_show, NULL),
	_RBS_FN_ATTR(wdtrace, S_IRUGO, wdtrace_show, NULL),
	_RBS_FN_ATTR_NULL,
};

static struct rbs_fn rbs_sys_fn = {
	.name = "rbs-sys",
	.owner = THIS_MODULE,
	.parent = NULL,
	.ops = (struct rbs_fn_ops *) &rbs_sys_fn_ops,
	.attrs = rbs_sys_attr,
};

static int __init rbs_sys_lateinit(void)
{
	int ret;

	rbs_sys->mcgrp = rbs_proto_mcgrp_alloc(RBS_PROTO_SYS, -1);
	if (!rbs_sys->mcgrp)
		panic("%s: unable to allocate multicast group\n", __func__);

	ret = rbs_proto_register(RBS_PROTO_SYS, &rbs_sys_proto_ops,
			rbs_sys);
	if (ret)
		panic("%s: unable to register RBS_SYS protocol\n", __func__);

	rbs_sys->api_ready = 1;
	rbs_sys_fn.depend = board[board_type]->bt_fn_depend;

	ret = rbs_fn_add(&rbs_sys_fn);
	if (ret)
		panic("%s: unable to add RBS_SYS FN\n", __func__);

	return 0;
}
late_initcall(rbs_sys_lateinit);

enum rbs_sys_bt rbs_sys_board_type(void)
{
	return board_type;
}
EXPORT_SYMBOL(rbs_sys_board_type);

static __init int rbs_sys_bt_init(void)
{
	struct device_node *np;
	const char *bt;
	int ret;

	board_type = RBS_BT_UNKNOWN;

	np = of_find_node_by_path(RCS_BT_NODEPATH);
	if (!np) {
		pr_info("Board info node not found\n");
		return -EFAULT;
	}

	ret = of_property_read_string(np, "board_type", &bt);
	if (ret) {
		pr_info("Board type property not found\n");
		of_node_put(np);
		return -EFAULT;
	}

	flat_dt_size = of_get_flat_dt_size();

	if (!strcmp(bt, "tcu03")) {
		board_type = RBS_BT_TCU03;
	} else if (!strcmp(bt, "tcu04")) {
		board_type = RBS_BT_TCU04;
	} else if (!strcmp(bt, "c608")) {
		board_type = RBS_BT_C608;
	} else if (!strcmp(bt, "dus52")) {
		board_type = RBS_BT_DUS52;
	} else if (!strcmp(bt, "dus52tk")) {
		board_type = RBS_BT_DUS52_TK;
	} else if (!strcmp(bt, "dus32tk")) {
		board_type = RBS_BT_DUS32_TK;
	} else if (!strcmp(bt, "dus32")) {
		board_type = RBS_BT_DUS32;
	} else if (!strcmp(bt, "dus53")) {
		board_type = RBS_BT_DUS53;
	} else if (!strcmp(bt, "dus33")) {
		board_type = RBS_BT_DUS33;
	} else if (!strcmp(bt, "mdusx3")) {
		board_type = RBS_BT_MDUSX3;
	} else {
		pr_info("Unknown board type: %s\n", bt);
	}

	of_node_put(np);

	pr_info("Board type %d\n", board_type);

	return 0;
}
postcore_initcall(rbs_sys_bt_init);

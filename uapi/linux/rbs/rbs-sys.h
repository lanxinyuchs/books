/*
 * RBS SYS socket API
 *
 * Copyright 2012 Ericsson AB
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

#ifndef UAPI_RBS_SYS_H__
#define UAPI_RBS_SYS_H__

/* System state */
#define RBS_SYS_STARTING	0
#define RBS_SYS_OPERATIONAL	1
#define RBS_SYS_DEGRADED	2
#define RBS_SYS_FAULT		3
#define RBS_SYS_RESTARTING	4

/* System restart type */
#define RBS_SYSRESTART_COLD	0
#define RBS_SYSRESTART_COLD_WT	1
#define RBS_SYSRESTART_COLD_WET	2 /* extended test */
#define RBS_SYSRESTART_POWERON	3
#define RBS_SYSRESTART_WATCHDOG	4
#define RBS_SYSRESTART_MMIRESET	5
#define RBS_SYSRESTART_BPMSW	6
#define RBS_SYSRESTART_TEST	7
#define RBS_SYSRESTART_ECC	8 /* uncorrectable ECC error */

/* Boot pointer */
#define RBS_SYSBOOT_UN		0 /* unknown */
#define RBS_SYSBOOT_CF		1 /* configured */
#define RBS_SYSBOOT_FB		2 /* fallback */
#define RBS_SYSBOOT_NL		3 /* network loader (type2) */
#define RBS_SYSBOOT_UP          4 /* upgrade */
#define RBS_SYSBOOT_NL3		5 /* network loader (type3) */

/* */
struct rbs_sys_restart_data {
	unsigned int type;
	unsigned int ecode;
	unsigned int extra1;
	unsigned int extra2;
};

struct rbs_sys_event {
	int event;
	union {
		int sys_state;
	} d;
};

#define RBS_SYSEVT_STATECHG	0

/*
 * IOCTL
 */

struct rbs_sys_ioc_state {
	int sys_state;
};

#define RBS_SYSIOC_GETSTATE	_IOR('s', 0, struct rbs_sys_ioc_state)
#define RBS_SYSIOC_FORCESTATE	_IOW('s', 1, struct rbs_sys_ioc_state)
#define RBS_SYSIOC_SUBSCRIBESTATE	_IOWR('s', 2, int)

#define RBS_SYSIOC_GETRESTART	_IOR('s', 10, struct rbs_sys_restart_data)

struct rbs_sys_ioc_rstorder {
	int delay; /* ms */
	struct rbs_sys_restart_data data;
};
#define RBS_SYSIOC_ORDERRESTART	_IOW('s', 11, struct rbs_sys_ioc_rstorder)

#define RBS_SYSIOC_SETBOOTCOUNT	_IOW('s', 20, unsigned int)
#define RBS_SYSIOC_GETBOOTCOUNT	_IOR('s', 21, unsigned int)
#define RBS_SYSIOC_GETBOOTSTARTED	_IOR('s', 22, unsigned int)
#define RBS_SYSIOC_SETUPGRADE  _IOW('s', 23, int)

struct rbs_sys_ioc_getdtprop {
	char node_path[128]; /* in */
	char prop_name[32]; /* in */
	int size; /* in/out */
	void *val; /* in */
};
#define RBS_SYSIOC_GETDTPROP	_IOWR('s', 30, struct rbs_sys_ioc_getdtprop)

struct rbs_sys_ioc_getdt {
	unsigned int size; /* in/out */
	void *blob; /* in */
};
#define RBS_SYSIOC_GETDT	_IOWR('s', 31, struct rbs_sys_ioc_getdt)

/* Note, corresponding string definition of enum rbs_sys_bt in rbs-sys.c */
enum rbs_sys_bt {
	RBS_BT_TP1,
	RBS_BT_DUS41,
	RBS_BT_DUW2,
	RBS_BT_DUL,
	RBS_BT_DUL_TB,
	RBS_BT_DUS41_TB,
	RBS_BT_TP5,
	RBS_BT_DUW31,
	RBS_BT_DUSX2_SIM,
	RBS_BT_TCU03,
	RBS_BT_DUS32,
	RBS_BT_DUS52,
	RBS_BT_TCU04,
	RBS_BT_DUS33,
	RBS_BT_DUS53,
	RBS_BT_MDUSX3,
	RBS_BT_C608,
	RBS_BT_DUS52_TK,
	RBS_BT_DUS32_TK,
	RBS_BT_UNKNOWN,
	RBS_BT_MAX
};

struct rbs_sys_ioc_getbt {
	int type;
};
#define RBS_SYSIOC_GETBT	_IOR('s', 40, struct rbs_sys_ioc_getbt)

struct rbs_sys_ioc_wdcpumask {
	unsigned int size; /*in/out */
	void *cpu_set;    /*in */
};
#define RBS_SYSIOC_SETWDCPUMASK _IOWR('s', 50, struct rbs_sys_ioc_wdcpumask)
#define RBS_SYSIOC_GETWDCPUMASK _IOWR('s', 51, struct rbs_sys_ioc_wdcpumask)

#define RBS_SYSIOC_SETSYSCFG	_IOW('s', 60, uint32_t)
#define RBS_SYSIOC_GETSYSCFG	_IOR('s', 61, uint32_t)

#define RBS_SYSIOC_GETRESTARTREASON _IOR('s', 62, uint32_t)
#define RBS_SYSIOC_GETFAULT	    _IOR('s', 63, uint64_t)

#endif /* UAPI_RBS_SYS_H__ */

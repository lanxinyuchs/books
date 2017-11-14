#ifndef RBS_SYS_COMPAT_H__
#define RBS_SYS_COMPAT_H__

#include <asm/compat.h>

struct rbs_sys_compat_getdtprop {
	char node_path[128]; /* in */
	char prop_name[32]; /* in */
	int size; /* in/out */
	compat_uptr_t val; /* in */
};


struct rbs_sys_compat_getdt {
	unsigned int size; /* in/out */
	compat_uptr_t blob; /* in */
};

struct rbs_sys_compat_wdcpumask {
	unsigned int size; /*in/out */
	compat_uptr_t cpu_set;    /*in */
};

#define RBS_SYS_COMPAT_GETDTPROP _IOWR('s', 30, struct rbs_sys_compat_getdtprop)
#define RBS_SYS_COMPAT_GETDT _IOWR('s', 31, struct rbs_sys_compat_getdt)

#define RBS_SYS_COMPAT_SETWDCPUMASK _IOWR('s', 50, struct rbs_sys_compat_wdcpumask)
#define RBS_SYS_COMPAT_GETWDCPUMASK _IOWR('s', 51, struct rbs_sys_compat_wdcpumask)

#endif /* RBS_SYS_COMPAT_H__ */

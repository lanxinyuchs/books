#ifndef __CPM_WDT_DEFINES_H
#define __CPM_WDT_DEFINES_H

#define WATCHDOG_TIMEOUT     20 /* seconds */

#define WD0_TIMER_OFFSET     0x000A0 /* TIMER5_INT */
#define WD1_TIMER_OFFSET     0x000C0 /* TIMER6_INT */

/* XXX from drivers/clocksource/timer-sp.h */
#define TIMER_LOAD	0x00			/* ACVR rw */
#define TIMER_VALUE	0x04			/* ACVR ro */
#define TIMER_CTRL	0x08			/* ACVR rw */
#define TIMER_CTRL_ONESHOT	(1 << 0)	/*  CVR */
#define TIMER_CTRL_32BIT	(1 << 1)	/*  CVR */
#define TIMER_CTRL_DIV1		(0 << 2)	/* ACVR */
#define TIMER_CTRL_DIV16	(1 << 2)	/* ACVR */
#define TIMER_CTRL_DIV256	(2 << 2)	/* ACVR */
#define TIMER_CTRL_IE		(1 << 5)	/*   VR */
#define TIMER_CTRL_PERIODIC	(1 << 6)	/* ACVR */
#define TIMER_CTRL_ENABLE	(1 << 7)	/* ACVR */

#if defined(CONFIG_RBS_SYS_CPM1) 

#define SC_CRIT_WRITE_KEY    0x31000
#define SC_RESET_CONTROL     0x31008

#define CDBGRSTREQ_ENABLE    0x00800
#define WD0_RESET_ENABLE     0x00080
#define WD0_RESET_SELECT     0x00040

#elif defined(CONFIG_RBS_SYS_CPM2)

#define SC_CRIT_WRITE_KEY    0x2000
#define SC_RESET_CONTROL     0x2008

#define CDBGRSTREQ_ENABLE    0x4000000
#define WD1_RESET_ENABLE     0x0000200
#define WD0_RESET_ENABLE     0x0000100
#define WD1_RESET_SELECT     0x0000080
#define WD0_RESET_SELECT     0x0000040

#endif

#endif

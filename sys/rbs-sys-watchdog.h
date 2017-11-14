#ifndef __RBS_SYS_WATCHDOG_H
#define __RBS_SYS_WATCHDOG_H

#if defined(CONFIG_RBS_SYS_CPM1) || defined(CONFIG_RBS_SYS_CPM2)

#include <asm/arch_timer.h>

#define __get_cpu_ticks() arch_counter_get_cntvct()

#define CPU_TIME_SHIFT_US 8

static inline u64 __get_cpu_time_us(void)
{
	return __get_cpu_ticks() >> CPU_TIME_SHIFT_US;
}

#else

#warning   "RBS sys watchdog needs cpu tick read function"

#define __get_cpu_ticks() 0
#define __get_cpu_time_us() 0

#endif

#endif

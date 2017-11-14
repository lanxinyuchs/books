/*
 * Detect soft lockups on a system
 *
 * Copyright 2014 Ericsson AB
 * Kerstin Jonsson <kerstin.jonsson@ericsson.com>
 *
 * based on kernel/watchdog.c
 * - Add optional supervision of system watchdog
 * - Integrate with rbs sys and add api to modify
 *   affinity for lockup detector.
 * - Make thresholds configurable
 * - Removed hard lockup detector as it's not applicable in
 *   currently supported platforms
 * - Removed panic on soft lockup option, rely on the system WD
 *   to restart if lockup persists.
 *
 *  Released under the General Public License (GPL).
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
 *
 */

#define pr_fmt(fmt) "RBS Sys: " fmt

#include <linux/mm.h>
#include <linux/cpu.h>
#include <linux/nmi.h>
#include <linux/init.h>
#include <linux/delay.h>
#include <linux/freezer.h>
#include <linux/kthread.h>
#include <linux/lockdep.h>
#include <linux/notifier.h>
#include <linux/module.h>
#include <linux/sysctl.h>
#include <linux/smpboot.h>
#include <linux/sched/rt.h>
#include <linux/cpumask.h>
#include <linux/kmsg_dump.h>

#include <asm/irq_regs.h>
#include <linux/kvm_para.h>
#include <linux/perf_event.h>

#include "rbs-sys-internal.h"
#include "rbs-sys-watchdog.h"

static struct task_struct *sys_watchdog_task = NULL;
static int __read_mostly sys_watchdog_enabled = 0;
static pid_t __read_mostly sys_watchdog_pid = 0;
static int __read_mostly softlockup_thresh = CONFIG_RBS_SYS_SOFTLOCKUP_THRESHOLD;
static u64 __read_mostly sample_period = CONFIG_RBS_SYS_WATCHDOG_THRESHOLD * ((u64)NSEC_PER_SEC / 5);
static struct cpumask __read_mostly *watchdog_cpu_mask = cpu_all_mask;
static char watchdog_cpu_mask_str[(NR_CPUS * 2) + 1];

static DEFINE_PER_CPU(unsigned long, watchdog_touch_ts);
static DEFINE_PER_CPU(struct task_struct *, softlockup_watchdog);
static DEFINE_PER_CPU(struct hrtimer, watchdog_hrtimer);
static DEFINE_PER_CPU(bool, soft_watchdog_warn);
static DEFINE_PER_CPU(unsigned long, hrtimer_interrupts);
static DEFINE_PER_CPU(unsigned long, soft_lockup_hrtimer_cnt);

static DEFINE_PER_CPU(u64, sys_watchdog_countdown);
static DEFINE_PER_CPU(u64, sys_watchdog_ts);
static DEFINE_PER_CPU(unsigned long, sys_watchdog_touchdown);
static DEFINE_PER_CPU(unsigned long, sys_watchdog_touchdown_save);

static int watchdog_enabled = 1;

static int __init nowatchdog_setup(char *str)
{
	watchdog_enabled = 0;
	return 1;
}
__setup("nowatchdog", nowatchdog_setup);

/**
 * Returns seconds, approximately.  We don't need nanosecond
 * resolution, and we don't need to waste time with a big divide when
 * 2^30ns == 1.074s.
 */
static unsigned long get_timestamp(void)
{
	return local_clock() >> 30LL;  /* 2^30 ~= 10^9 */
}

/**
 * Commands for resetting the watchdog
 */
static void __touch_watchdog(void)
{
	__this_cpu_write(watchdog_touch_ts, get_timestamp());
}

static void rbs_sys_touch_all_softlockup_watchdogs(void)
{
	int cpu;

	for_each_online_cpu(cpu) {
		if (cpumask_test_cpu(cpu, watchdog_cpu_mask))
			per_cpu(watchdog_touch_ts, cpu) = 0;
	}
}

/**
 * System WD Interface
 */

void rbs_sys_touch_system_watchdog(void)
{
	int cpu;

	for_each_online_cpu(cpu) {
		if (cpumask_test_cpu(cpu, watchdog_cpu_mask))
			per_cpu(sys_watchdog_touchdown, cpu) += 1;
	}
	preempt_disable();
	__touch_watchdog();
	__this_cpu_write(sys_watchdog_ts, local_clock());
	preempt_enable();
}

void rbs_sys_enable_system_watchdog(struct task_struct *t)
{
	pr_info("System WD supervision started\n");

	if (t) {
		rcu_read_lock();
		get_task_struct(t);
		sys_watchdog_pid = t->pid;
		put_task_struct(t);
		rcu_read_unlock();
	}
	sys_watchdog_task = t;
	sys_watchdog_enabled = 1;
}

/**/

static int is_softlockup(unsigned long touch_ts)
{
	unsigned long now = get_timestamp();

	/* Warn about unreasonable delays: */
	if (time_after(now, touch_ts + softlockup_thresh))
		return now - touch_ts;

	return 0;
}

/**
* System WD kicker increments the touchdown counter
* by calling rbs_sys_touch_system_watchdog()
* If the counter has not been incremented for two consecutive
* sample periods (~2-4s with default settings)
* then the lockup detector debug-printout triggers in the
* watchdog timer function: watchdog_timer_fn()
*/
static s64 __is_syswd_lockup(u64 cnt_us)
{
	unsigned long touchdown = __this_cpu_read(sys_watchdog_touchdown);

	if (__this_cpu_read(sys_watchdog_touchdown_save) == touchdown) {
		u64 countdown = __this_cpu_read(sys_watchdog_countdown);

		if (countdown)
			return cnt_us - countdown;

		__this_cpu_write(sys_watchdog_countdown, cnt_us);
	} else {
		__this_cpu_write(sys_watchdog_countdown, 0);
		__this_cpu_write(sys_watchdog_touchdown_save, touchdown);
	}

	return 0;
}

static s64 is_syswd_lockup(u64 cnt_us)
{
	if (sys_watchdog_enabled)
		return __is_syswd_lockup(cnt_us);

	return 0;
}

static inline u64 syswd_ts(int *syswd_cpu)
{
	u64 ts = 0;
	int cpu;

	for_each_online_cpu(cpu) {
		u64 cpu_ts = per_cpu(sys_watchdog_ts, cpu);
		if ( cpu_ts > ts) {
			ts = cpu_ts;
			*syswd_cpu = cpu;
		}
	}

	return ts;
}

static void watchdog_interrupt_count(void)
{
	__this_cpu_inc(hrtimer_interrupts);
}

static enum hrtimer_restart watchdog_timer_fn(struct hrtimer *hrtimer)
{
	unsigned long touch_ts = __this_cpu_read(watchdog_touch_ts);
	u64 cnt_us = __get_cpu_time_us();
	struct pt_regs *regs = get_irq_regs();
	int duration;
	s64 syswd_duration = is_syswd_lockup(cnt_us);

	watchdog_interrupt_count();

	wake_up_process(__this_cpu_read(softlockup_watchdog));

	hrtimer_forward_now(hrtimer, ns_to_ktime(sample_period));

	if (touch_ts == 0 && syswd_duration == 0) {
		__touch_watchdog();
		return HRTIMER_RESTART;
	}

	duration = is_softlockup(touch_ts);
	if (unlikely(duration || syswd_duration)) {
		int syswd_cpu = -1;
		u64 syswd_ts_delay = local_clock() - syswd_ts(&syswd_cpu);
		char *syswd_affinity = (sys_watchdog_task ? watchdog_cpu_mask_str : NULL);

		if (__this_cpu_read(soft_watchdog_warn) != true) {
			__this_cpu_write(soft_watchdog_warn, true);

			pr_emerg("SW Watchdog soft lockup - CPU#%d stuck for %us! "
			       "syswd stuck for >= %lldus [%s:%d]\n"
			       "affinity %s, last reset on CPU#%d: delay %llu ns\n",
			       smp_processor_id(), duration, syswd_duration,
			       current->comm, task_pid_nr(current),
			       (syswd_affinity ? syswd_affinity : ""),
			       syswd_cpu, syswd_ts_delay);

			print_modules();
			print_irqtrace_events(current);
			if (regs)
				show_regs(regs);
			else
				dump_stack();
			kmsg_dump(KMSG_DUMP_OOPS);
		}
		/* Softlockup ~5 times in a row */
		/* Disclaimer: Since we dont have a proper NMI, we let the */
		/* SW WD trigger a reset since the HW WD clears the PMEM */
		/* and subsequently all information regarding what went wrong */

		if (syswd_duration >= 5000000) {
			unsigned int syswd_flags;
			int syswd_on_rq;
			int syswd_task_cpu;

			if (sys_watchdog_task) {
				rcu_read_lock();
				get_task_struct(sys_watchdog_task);

				syswd_flags = sys_watchdog_task->flags;
				syswd_on_rq = sys_watchdog_task->on_rq;
				syswd_task_cpu = task_cpu(sys_watchdog_task);

				put_task_struct(sys_watchdog_task);
				rcu_read_unlock();
			} else {
				syswd_flags = 0;
				syswd_on_rq = -1;
				syswd_task_cpu = -1;
			}

			panic("Unrecoverable SW Watchdog soft lockup - CPU#%d stuck for %us! "
			      "syswd stuck for >= %lldus [%s:%d]:\n"
			      "affinity %s, last reset on CPU#%d: delay %llu ns "
			      "flags 0x%8.8x On CPU#%d On rq %d\n",
			      smp_processor_id(), duration, syswd_duration,
			      current->comm, task_pid_nr(current),
			      (syswd_affinity ? syswd_affinity : ""),
			      syswd_cpu, syswd_ts_delay,
			      syswd_flags, syswd_on_rq, syswd_task_cpu);

		}
	} else
		__this_cpu_write(soft_watchdog_warn, false);

	return HRTIMER_RESTART;
}

static void watchdog_set_prio(unsigned int policy, unsigned int prio)
{
	struct sched_param param = { .sched_priority = prio };

	sched_setscheduler(current, policy, &param);
}

static void watchdog_enable(unsigned int cpu)
{
	struct hrtimer *hrtimer = raw_cpu_ptr(&watchdog_hrtimer);

	hrtimer_init(hrtimer, CLOCK_MONOTONIC, HRTIMER_MODE_REL);
	hrtimer->function = watchdog_timer_fn;
	hrtimer->irqsafe = 1;

	watchdog_set_prio(SCHED_FIFO, MAX_RT_PRIO - 1);

	if (!watchdog_enabled || !cpumask_test_cpu(cpu, watchdog_cpu_mask)) {
		pr_info("watchdog_enable, not enabling since cpu%d not in mask\n", cpu);
		return;
	}
	pr_info("watchdog_enable cpu %d\n", cpu);
	hrtimer_start(hrtimer, ns_to_ktime(sample_period),
		      HRTIMER_MODE_REL_PINNED);

	preempt_disable();
	__touch_watchdog();
	preempt_enable();
}

static void watchdog_disable(unsigned int cpu)
{
	struct hrtimer *hrtimer = raw_cpu_ptr(&watchdog_hrtimer);

	watchdog_set_prio(SCHED_NORMAL, 0);
	hrtimer_cancel(hrtimer);
	pr_info("watchdog_disable cpu %d\n", cpu);
}

static int watchdog_should_run(unsigned int cpu)
{
	return __this_cpu_read(hrtimer_interrupts) !=
		__this_cpu_read(soft_lockup_hrtimer_cnt);
}

static void watchdog(unsigned int cpu)
{
	preempt_disable();
	__this_cpu_write(soft_lockup_hrtimer_cnt,
			 __this_cpu_read(hrtimer_interrupts));
	__touch_watchdog();
	preempt_enable();
}

static struct smp_hotplug_thread watchdog_threads = {
	.store			= &softlockup_watchdog,
	.thread_should_run	= watchdog_should_run,
	.thread_fn		= watchdog,
	.thread_comm		= "watchdog/%u",
	.setup			= watchdog_enable,
	.park			= watchdog_disable,
	.unpark			= watchdog_enable,
};

/**
 * rbs-sys proto callbacks
 */
static int rbs_sys_watchdog_set_cpu_mask(struct cpumask *cpumask)
{
	struct cpumask old_mask;
	int cpu;
	int ret = 0;

	if (!watchdog_enabled)
		return -ENODEV;

	cpumask_copy(&old_mask, watchdog_cpu_mask);

	for_each_online_cpu(cpu) {
		if (cpumask_test_cpu(cpu, cpumask)) {
			if (!cpumask_test_and_set_cpu(cpu, watchdog_cpu_mask))
				kthread_unpark(per_cpu(softlockup_watchdog, cpu));
			continue;
		}
		if (cpumask_test_and_clear_cpu(cpu, watchdog_cpu_mask))
			kthread_park(per_cpu(softlockup_watchdog, cpu));
	}
	if (sys_watchdog_enabled && sys_watchdog_pid) {

		if (!cpumask_equal(&old_mask, watchdog_cpu_mask)) {
			cpumap_print_to_pagebuf(false, watchdog_cpu_mask_str,
						watchdog_cpu_mask);
			ret = sched_setaffinity(sys_watchdog_pid, watchdog_cpu_mask);
		}
		pr_info("System Watchdog affinity : %s\n",
			watchdog_cpu_mask_str);
	}
	return ret;
}

static int rbs_sys_watchdog_get_cpu_mask(struct cpumask *cpumask)
{
	if (!watchdog_enabled)
		return -ENODEV;

	memcpy(cpumask, watchdog_cpu_mask, cpumask_size());
	return 0;
}

struct rbs_sys_wd_ops rbs_sys_wd_ops = {
	.set_cpu_mask = rbs_sys_watchdog_set_cpu_mask,
	.get_cpu_mask = rbs_sys_watchdog_get_cpu_mask,
	.touch_all = rbs_sys_touch_all_softlockup_watchdogs,
};

static __init int rbs_sys_lockup_detector_init(void)
{
	int ret;

	pr_info("Setup soft lockup detector\n");

	rbs_sys_add_wd_ops(&rbs_sys_wd_ops);

        /* Pet dog early */
	preempt_disable();
	__touch_watchdog();
	preempt_enable();
	cpumap_print_to_pagebuf(false, watchdog_cpu_mask_str, watchdog_cpu_mask);

	ret = smpboot_register_percpu_thread(&watchdog_threads);
	if (ret) {
		pr_err("Failed to create watchdog threads, disabled\n");
		watchdog_enabled = 0;
	}
	return ret;
}
early_initcall(rbs_sys_lockup_detector_init);

/**
 * Watchdog timer for Intel axxia platforms
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
 */

#define pr_fmt(fmt) "RBS CPM: " fmt

#include <linux/clk.h>
#include <linux/clocksource.h>
#include <linux/io.h>
#include <linux/kthread.h>
#include <linux/module.h>
#include <linux/of.h>
#include <linux/of_address.h>
#include <linux/reboot.h>
#include <linux/rbs/cpm-wdt.h>
#include <linux/rbs/rbs-sys.h>
#include "cpm-wdt-defines.h"

/* XXX CONFIG_ARM_TIMER_SP804 is missing for ARM64 in kernel versions < 4.2 */

static const char *wd_timer_name = "cpm-timer5";
static void __iomem *syscon_base = NULL;
static void __iomem *syscon_reset_key = NULL;
static void __iomem *syscon_reset_ctrl = NULL;
static void __iomem *wd_timer_base = NULL;

static struct task_struct *watchdog_task;

static unsigned int timer_value = 0xffffffff;

static s32 cpm_syswd_disable = 0;
static s32 cpm_syswd_test = 0;
static s32 cpm_wd_enabled = 0;

notrace s32 __init early_cpm_wdt_off(char *p)
{
	cpm_syswd_disable = 1;
	return 0;
}
early_param("nowd", early_cpm_wdt_off);

notrace s32 __init early_cpm_wdt_test(char *p)
{
	cpm_syswd_test = 1;
	return 0;
}
early_param("testwd", early_cpm_wdt_test);

static long __init get_clock_rate(const char *name, struct clk *clk)
{
	long rate;
	int err;

	if (!clk) {
		clk = clk_get_sys("sp804", name);
		if (IS_ERR(clk)) {
			pr_err("sp804: %s clock not found: %d\n", name,
				(int)PTR_ERR(clk));
			return PTR_ERR(clk);
		}
	}

	err = clk_prepare(clk);
	if (err) {
		pr_err("%s clock failed to prepare: %d\n", name, err);
		clk_put(clk);
		return err;
	}

	err = clk_enable(clk);
	if (err) {
		pr_err("%s clock failed to enable: %d\n", name, err);
		clk_unprepare(clk);
		clk_put(clk);
		return err;
	}

	rate = clk_get_rate(clk);
	if (rate < 0) {
		pr_err("%s clock failed to get rate: %ld\n", name, rate);
		clk_disable(clk);
		clk_unprepare(clk);
		clk_put(clk);
	}

	return rate;
}

static inline void __kick_wd(void)
{
	if (cpm_wd_enabled) {
		writel(timer_value, wd_timer_base + TIMER_LOAD);
		rbs_sys_touch_system_watchdog();
	}
}

void cpm_touch_syswd(void)
{
	__kick_wd();
}

static int wdt_panic(struct notifier_block *this, unsigned long event, void *ptr)
{
	/* try not to restart int the middle of panic/Oops printout */
	__kick_wd();

	/* paranoid! panic stops schedule so should not be needed */
	cpm_wd_enabled = 0;
	return NOTIFY_DONE;
}

static struct notifier_block panic_block = {
	.notifier_call = wdt_panic,
};

static int wdt_reboot(struct notifier_block *this, unsigned long event, void *ptr)
{
	/* hopefully don't TMO in the middle of restart, when restart is ordered */
	__kick_wd();
	return NOTIFY_DONE;
}

static struct notifier_block reboot_block = {
	.notifier_call = wdt_reboot,
};

static int watchdog(void* dummy)
{
	ktime_t wait;
	const unsigned long timeout_ns = 250 * 1000 * 1000;
	struct sched_param param = {
		.sched_priority = MAX_USER_RT_PRIO - 1,
	};

	sched_setscheduler(current, SCHED_FIFO, &param);

	if (cpm_syswd_test)
		pr_info("System Watchdog test reset expected within the next %d s\n", WATCHDOG_TIMEOUT);

	do {
		wait = ns_to_ktime(timeout_ns);
		set_current_state(TASK_UNINTERRUPTIBLE);
		(void) schedule_hrtimeout(&wait, HRTIMER_MODE_REL);
		__kick_wd();
	}while (!cpm_syswd_test);
	return 0;
}

void __init cpm_enable_system_wd(void __iomem *syscon_base, void __iomem *timers, struct clk *clk)
{
	long rate;

	if (cpm_syswd_disable) {
		pr_info("System Watchdog is Disabled\n");
		return;
	}

	if (WARN_ON(timers == NULL))
		return;

	if (WARN_ON(syscon_base == NULL))
		return;

	rate = get_clock_rate(wd_timer_name, clk);
	if (rate < 0) {
		pr_warn("Get clk rate for %s fails, CPM System Watchdog is not started\n",
			wd_timer_name);
		return;
	}

	wd_timer_base = timers + WD0_TIMER_OFFSET;

	if (IS_ENABLED(CONFIG_CPM_KERNEL_SYSWD_SETUP)) {
		unsigned int ctrl;

		/* XXX verify on hardware */
		/* TimerXLoad  = (Interval x TIMCLKFREQ) / (TIMCLKENXDIV x PRESCALEDIV) */
		/* timer_value = rate * WATCHDOG_TIMEOUT */

		writel(0, wd_timer_base + TIMER_CTRL);
		writel(timer_value, wd_timer_base + TIMER_LOAD);
		writel(timer_value, wd_timer_base + TIMER_VALUE);
		writel(TIMER_CTRL_32BIT | TIMER_CTRL_ENABLE | TIMER_CTRL_PERIODIC | TIMER_CTRL_IE, wd_timer_base + TIMER_CTRL);

		/* Enable WD restart at wd timer TMO */
		syscon_reset_key = syscon_base + SC_CRIT_WRITE_KEY;
		syscon_reset_ctrl = syscon_base + SC_RESET_CONTROL;

		ctrl = readl(syscon_reset_ctrl);
		writel(0xab, syscon_reset_key);
#if defined(CONFIG_RBS_SYS_CPM1)
		writel(ctrl | WD0_RESET_ENABLE | WD0_RESET_SELECT, syscon_reset_ctrl);
#else /* CONFIG_RBS_SYS_CPM2 */
		writel(ctrl | WD0_RESET_ENABLE, syscon_reset_ctrl);
#endif
		writel(0x00, syscon_reset_key);

		pr_info("System Watchdog timeout set to %ld ms", timer_value / (rate / 1000));
	}
	cpm_wd_enabled = 1;

	/* Add initial reset here as it looks like we might be on the border already
	when uboot turn control over to kernel */
	writel(timer_value, wd_timer_base + TIMER_LOAD);
}

void __init cpm_start_syswd_poll(void)
{
	struct task_struct *t = NULL;

	if (cpm_wd_enabled) {
		BUG_ON(!wd_timer_base);
#if defined(CONFIG_CPM_KERNEL_SYSWD_SETUP)
		BUG_ON(!syscon_reset_key);
		BUG_ON(!syscon_reset_ctrl);
#endif

		watchdog_task = kthread_run(watchdog, NULL, "cpm_syswd");
		if (IS_ERR(watchdog_task))
			pr_warn("Unable to start CPM System Watchdog poll\n");
		else {
			pr_info("System Watchdog poll started\n");
			t = watchdog_task;
		}
	}
	else
		return;

	if(!cpm_syswd_test)
		rbs_sys_enable_system_watchdog(t);
	else
		rbs_sys_enable_system_watchdog(NULL);
}

/* XXX Fix early start even if CONFIG_ARM_TIMER_SP804 is set*/
static void __init cpm_dt_timer_init(struct device_node *np)
{
	void __iomem *base;
	struct clk *clk;

	struct device_node *syscon_np;
	syscon_np = of_find_compatible_node(NULL, NULL, "intel,axxia-syscon");
	if (!syscon_np) {
		pr_crit("could not find syscon node\n");
		return;
	}

	syscon_base = of_iomap(syscon_np, 0);
	if (!syscon_base) {
		pr_crit("could not remap syscon\n");
		return;
	}

	base = of_iomap(np, 0);
	if (WARN_ON(base == NULL))
		return;

	clk = of_clk_get(np, 0);
	if (WARN_ON(IS_ERR(clk)))
		return;

	cpm_enable_system_wd(syscon_base, base, clk);
}
CLOCKSOURCE_OF_DECLARE(cpm, "arm,sp804", cpm_dt_timer_init);

static s32 __init wdt_init(void)
{
  /* XXX Fix early start even if CONFIG_ARM_TIMER_SP804 is set*/
	/* cpm_dt_timer_init(NULL); */
	cpm_start_syswd_poll();

	if (!cpm_wd_enabled) {
		pr_info("System Watchdog is Disabled\n");
		return 0;
	}
	pr_info("System Watchdog driver loaded\n");

	atomic_notifier_chain_register(&panic_notifier_list, &panic_block);
	register_reboot_notifier(&reboot_block);

	return 0;
}
module_init(wdt_init);

MODULE_DESCRIPTION("RBS CPM watchdog driver");
MODULE_LICENSE("GPL");

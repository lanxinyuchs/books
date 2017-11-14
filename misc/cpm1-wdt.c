/**
 * Watchdog timer for LSI axxia platforms
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
 */

#define pr_fmt(fmt) KBUILD_MODNAME ": " fmt

#include <linux/module.h>
#include <linux/fs.h>
#include <linux/smp.h>
#include <linux/miscdevice.h>
#include <linux/notifier.h>
#include <linux/reboot.h>
#include <linux/io.h>
#include <linux/watchdog.h>
#include <linux/uaccess.h>
#include <linux/kthread.h>
#include <linux/delay.h>
#include <linux/clk.h>
#include <linux/sched/rt.h>
#include <linux/rbs/rbs-sys.h>

#include <asm/hardware/arm_timer.h>
#include <asm/mach/time.h>
#include <asm/div64.h>
#include <asm/axxia-wdt.h>

#define WD_TIMER_OFFSET      0x000a0
#define ACC_CTRL_OFFSET      0x31000
#define __CC_WRKEY_ENABLE       0xab
#define __CC_WRKEY_DISABLE         0

#define RESET_CTRL_OFFSET    0x31008
#define __RST_WFI_WAIT       0x80000
#define __RST_CDBG_ENABLE    0x00800
#define __RST_WD_ENABLE      0x00080
#define __RST_WD_SYS_RST     0x00040

static const char *wd_timer_name = "axxia-timer5";
static void __iomem *syscon_acc_ctrl = NULL;
static void __iomem *syscon_reset_ctrl = NULL;
static void __iomem *wd_timer_base = NULL;

static struct task_struct *watchdog_task;

static s32 axm_sys_wd_disable = 0;
static s32 axm_sys_wd_test = 0;
static s32 axxia_wd_enabled = 0;

notrace s32 __init early_axm_wdt_off(char *p)
{
	axm_sys_wd_disable = 1;
        return 0;
}
early_param("nowd", early_axm_wdt_off);

notrace s32 __init early_axm_wdt_test(char *p)
{
	axm_sys_wd_test = 1;
        return 0;
}
early_param("testwd", early_axm_wdt_test);

static long __init get_clock_rate(const char *name)
{
	struct clk *clk;
	long rate;
	int err;

	clk = clk_get_sys("sp804", name);
	if (IS_ERR(clk)) {
		pr_err("sp804: %s clock not found: %d\n", name,
			(int)PTR_ERR(clk));
		return PTR_ERR(clk);
	}

	err = clk_prepare(clk);
	if (err) {
		pr_err("sp804: %s clock failed to prepare: %d\n", name, err);
		clk_put(clk);
		return err;
	}

	err = clk_enable(clk);
	if (err) {
		pr_err("sp804: %s clock failed to enable: %d\n", name, err);
		clk_unprepare(clk);
		clk_put(clk);
		return err;
	}

	rate = clk_get_rate(clk);
	if (rate < 0) {
		pr_err("sp804: %s clock failed to get rate: %ld\n", name, rate);
		clk_disable(clk);
		clk_unprepare(clk);
		clk_put(clk);
	}

	return rate;
}
static inline void __kick_wd(void)
{
	if (axxia_wd_enabled) {
		writel(0xffffffff, wd_timer_base + TIMER_LOAD);
		rbs_sys_touch_system_watchdog();
	}
}

void cpm1_touch_syswd(void)
{
        __kick_wd();
}

static int wdt_panic(struct notifier_block *this, unsigned long event, void *ptr)
{
	/* try not to restart int the middle of panic/Oops printout */
        __kick_wd();
	/* paranoid! panic stops schedule so should not be needed */
	axxia_wd_enabled = 0;
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

static int watchdog(void *dummy)
{
	int test = 0;
	const unsigned long timeout_ns = 250 * 1000 * 1000;
	ktime_t wait;
	struct sched_param param = {
                .sched_priority = MAX_USER_RT_PRIO - 1,
        };
        sched_setscheduler(current, SCHED_FIFO, &param);

	if (axm_sys_wd_test)
		pr_info("axxia System Watchdog test reset expected within the next 20 s\n");

        for (;;) {
		wait = ns_to_ktime(timeout_ns);
		set_current_state(TASK_UNINTERRUPTIBLE);
		(void) schedule_hrtimeout(&wait, HRTIMER_MODE_REL);

		if (!test)
			__kick_wd();

		if (axm_sys_wd_test)
			test = 1;

        }
        return 0;
}

void __init axxia_enable_system_wd(void __iomem *syscon_base, void __iomem *timers)
{
	long rate;

	if (axm_sys_wd_disable) {
		pr_info("axxia System Watchdog is Disabled\n");
		return;
	}
	if (WARN_ON(timers == NULL))
                return;
	if (WARN_ON(syscon_base == NULL))
                return;
	wd_timer_base = timers + WD_TIMER_OFFSET;
	syscon_acc_ctrl = syscon_base + ACC_CTRL_OFFSET;
	syscon_reset_ctrl = syscon_base + RESET_CTRL_OFFSET;
	rate = get_clock_rate(wd_timer_name);
	if (rate < 0) {
		pr_warn("Get clk rate for %s fails, axxia System Watchdog is not started\n",
			wd_timer_name);
		return;
	}
#if defined(CONFIG_CPM1_KERNEL_SYSWD_SETUP)
	writel(0, wd_timer_base + TIMER_CTRL);
        writel(0xffffffff, wd_timer_base + TIMER_LOAD);
        writel(0xffffffff, wd_timer_base + TIMER_VALUE);
        writel(TIMER_CTRL_32BIT | TIMER_CTRL_ENABLE | TIMER_CTRL_PERIODIC | TIMER_CTRL_IE,
               wd_timer_base + TIMER_CTRL);

	/* Enable WD restart at wd timer TMO */
	writel(__CC_WRKEY_ENABLE, syscon_acc_ctrl);
	writel(__RST_WFI_WAIT|__RST_CDBG_ENABLE|__RST_WD_ENABLE|__RST_WD_SYS_RST, syscon_reset_ctrl);
	writel(__CC_WRKEY_DISABLE, syscon_acc_ctrl);
#endif
	axxia_wd_enabled = 1;
	/* Add initial reset here as it looks like we might be on the edge already
	   when uboot turn control over to kernel */
	writel(0xffffffff, wd_timer_base + TIMER_LOAD);
}

void __init axxia_start_syswd_poll(void)
{
	struct task_struct *t = NULL;

	if (axxia_wd_enabled) {
		BUG_ON(!wd_timer_base);
		BUG_ON(!syscon_acc_ctrl);
		BUG_ON(!syscon_reset_ctrl);
		watchdog_task = kthread_run(watchdog, NULL, "sys_wdd");
		if (IS_ERR(watchdog_task))
			pr_warn("Unable to start axxia System Watchdog poll\n");
		else {
			pr_info("axxia System Watchdog poll started\n");
			t = watchdog_task;
		}
	} else
		return;

	rbs_sys_enable_system_watchdog(t);
}

static s32 __init wdt_init(void)
{
	if (!axxia_wd_enabled) {
		pr_info("axxia System Watchdog is Disabled\n");
		return 0;
	}
	pr_info("axxia System Watchdog driver loaded\n");

	atomic_notifier_chain_register(&panic_notifier_list, &panic_block);
	register_reboot_notifier(&reboot_block);

	return 0;
}

module_init(wdt_init);

MODULE_DESCRIPTION("Axxia watchdog driver");
MODULE_LICENSE("GPL");

#ifndef __CPM_WDT_H
#define __CPM_WDT_H

struct clk;

void __init cpm_enable_system_wd(void __iomem *syscon, void __iomem *timers, struct clk *clk);
void __init cpm_start_syswd_poll(void);
void cpm_touch_syswd(void);

#endif

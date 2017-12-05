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

#ifndef RBS_SYS_H__
#define RBS_SYS_H__

#include <uapi/linux/rbs/rbs-sys.h>


extern int rbs_sys_order_restart(int delay /* ms */,
		unsigned int type, unsigned int ecode,
		unsigned int extra1, unsigned int extra2);
extern int rbs_sys_get_state(void);

/**
 * rbs_sys_board_type
 *
 * Board type is set at postcore_init
 * returns:
 * Board type if valid DU platform tag was found in dtb
 * or %RBS_BT_MAX if it wasn't
 */
extern enum rbs_sys_bt rbs_sys_board_type(void);

#if defined(CONFIG_RBS_SYS_LOCKUP_DETECTOR)

extern void rbs_sys_touch_system_watchdog(void);
extern void rbs_sys_enable_system_watchdog(struct task_struct *t);

#else

#define rbs_sys_touch_system_watchdog()
#define rbs_sys_enable_system_watchdog(t);

#endif

#endif /* RBS_SYS_H__ */

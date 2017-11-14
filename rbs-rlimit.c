/*
 * RBS Rate limit
 *
 * Copyright 2012 Ericsson AB
 * Andrey Panteleev <andrey.xx.panteleev@ericsson.com>
 *
 * Based on lib/ratelimit.c
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

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/jiffies.h>
#include <linux/timer.h>
#include <linux/rbs/rbs-rlimit.h>

#define RBS_RL_MASKED	1

static void rlimit_timer(unsigned long data)
{
	struct rbs_rlimit_state *rl = (struct rbs_rlimit_state *) data;

	clear_bit(RBS_RL_MASKED, &rl->flags);

	if (rl->unmask)
		rl->unmask(rl->id, rl->data);
}

void rbs_rlimit_init(struct rbs_rlimit_state *rl, int id, void *data,
                     int interval, int burst, void (*mask)(int, void *),
                     void (*unmask)(int, void *))
{
	if (!rl)
		return;
	raw_spin_lock_init(&rl->lock);
	rl->interval = interval;
	rl->burst = burst;
	if (!rl->burst)
		rl->burst = 1;
	rl->id = id;
	rl->cnt = 0;
	rl->begin = 0;
	rl->flags = 0;
        rl->data = data;

	init_timer(&rl->timer);
	rl->timer.function = rlimit_timer;
	rl->timer.data = (unsigned long) rl;

	rl->mask = mask;
	rl->unmask = unmask;
}
EXPORT_SYMBOL(rbs_rlimit_init);

void rbs_rlimit_destroy(struct rbs_rlimit_state *rl)
{
	if (!rl)
		return;
	del_timer_sync(&rl->timer);
}
EXPORT_SYMBOL(rbs_rlimit_destroy);

int rbs_rlimit(struct rbs_rlimit_state *rl)
{
	unsigned long flags;
	int ret;

	if (!rl || !rl->interval)
		return 1;

	if (test_bit(RBS_RL_MASKED, &rl->flags))
		return 0;

	raw_spin_lock_irqsave(&rl->lock, flags);
	if (!rl->begin)
		rl->begin = jiffies;

	if (time_is_before_jiffies(rl->begin + rl->interval)) {
		rl->begin = 0;
		rl->cnt = 0;
	}
	if (rl->burst > rl->cnt) {
		rl->cnt++;
		ret = 1;
	} else 
		ret = 0;
	raw_spin_unlock_irqrestore(&rl->lock, flags);

	if (!ret && !test_and_set_bit(RBS_RL_MASKED, &rl->flags)) {
		if (rl->mask)
			rl->mask(rl->id, rl->data);
		mod_timer(&rl->timer, rl->begin + rl->interval + 1);
	}

	return ret;
}
EXPORT_SYMBOL(rbs_rlimit);

int __rbs_rlimit(struct rbs_rlimit_state *rl)
{
	int ret;

	if (!rl || !rl->interval)
		return 1;

	if (test_bit(RBS_RL_MASKED, &rl->flags))
		return 0;

	if (!rl->begin)
		rl->begin = jiffies;

	if (time_is_before_jiffies(rl->begin + rl->interval)) {
		rl->begin = 0;
		rl->cnt = 0;
	}
	if (rl->burst > rl->cnt) {
		rl->cnt++;
		ret = 1;
	} else 
		ret = 0;

	if (!ret && !test_and_set_bit(RBS_RL_MASKED, &rl->flags)) {
		if (rl->mask)
			rl->mask(rl->id, rl->data);
		mod_timer(&rl->timer, rl->begin + rl->interval + 1);
	}

	return ret;
}
EXPORT_SYMBOL(__rbs_rlimit);

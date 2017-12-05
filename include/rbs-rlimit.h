/*
 * RBS Rate limit
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

#ifndef RBS_RLIMIT_H__
#define RBS_RLIMIT_H__

#include <linux/param.h>
#include <linux/spinlock.h>
#include <linux/timer.h>

struct rbs_rlimit_state {
	raw_spinlock_t lock;

	int interval;
	int burst;
	int cnt;
	unsigned long begin;
	unsigned long flags;
	struct timer_list timer;

	int id;
	void *data;
	void (*mask)(int, void *);
	void (*unmask)(int, void *);
};

/*
 * rbs_rlimit_init - init rate limit object
 * @rl: rbs_rlimit_state data
 * @id: user defined, passed unmodified to mask() and unmask() callbacks
 * @data: user defined, passed unmodified to mask() and unmask() callbacks
 * @interval: defines rate limit: not more than @burst in every @interval
 * @burst: see @interval
 */
extern void rbs_rlimit_init(struct rbs_rlimit_state *rl, int id, void *data,
                            int interval, int burst, void (*mask)(int, void *),
                            void (*unmask)(int, void *));

/*
 * rbs_rlimit_destroy - destroy rate limit object
 * @rl: rbs_rlimit_state data
 */
extern void rbs_rlimit_destroy(struct rbs_rlimit_state *rl);

/*
 * rbs_rlimit - rate limie
 * @rl: rbs_rlimit_state data
 *
 * Returns:
 *  0 - limit has been reached
 *  1 - limit has not been reached yet
 */
extern int rbs_rlimit(struct rbs_rlimit_state *rl);

/*
 * __rbs_rlimit - rate limie
 * @rl: rbs_rlimit_state data
 *
 * Returns:
 *  0 - limit has been reached
 *  1 - limit has not been reached yet
 *
 *  This is light-weight NON-THREAD-SAFE version of rbs_limit(). Can be
 *  used when rlimit is only called from one context (e.g. one kernel thread
 *  or one and only one interrupt handler).
 */
extern int __rbs_rlimit(struct rbs_rlimit_state *rl);

#endif /* !RBS_RLIMIT_H__ */

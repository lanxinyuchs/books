#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <errno.h>
#include <signal.h>

#include <ulh_dl_list.h>
#include "ulh_timer.h"

static inline uint64_t ulh_timer_gettime(void)
{
	struct timespec ts;

	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000ULL + ts.tv_nsec / 1000000;
}

void ulh_timerqueue_init(struct ulh_timerqueue *queue)
{
	dl_list_init(&queue->queue);
}

int ulh_timerqueue_schedule(struct ulh_timerqueue *queue)
{
	uint64_t now;
	struct ulh_timer *it;

	now = ulh_timer_gettime();
	while (!dl_list_empty(&queue->queue)) {
		it = dl_list_first_entry(&queue->queue, struct ulh_timer,
				link);
		if (now < it->exp_time)
			break;
		it->armed = 0;
		dl_list_remove(&it->link);
		it->cb(it->cb_param);
	}

	if (dl_list_empty(&queue->queue))
		return -1;

	it = dl_list_first_entry(&queue->queue, struct ulh_timer, link);
	return it->exp_time - now;
}

void ulh_timerqueue_destroy(struct ulh_timerqueue *queue)
{
	struct ulh_timer *it, *tmp;

	dl_list_foreach_safe(it, tmp, &queue->queue, link) {
		dl_list_remove(&it->link);
		it->armed = 0;
	}
}

void ulh_timer_init(struct ulh_timer *timer, void (*cb)(void *),
		void *cb_param)
{
	timer->cb = cb;
	timer->cb_param = cb_param;
	dl_list_init(&timer->link);
	timer->armed = 0;
}

int ulh_timer_arm(struct ulh_timer *timer, struct ulh_timerqueue *queue,
		uint32_t tmo)
{
	struct ulh_timer *it;

	if (!timer)
		return -EINVAL;

	if (!tmo)
		tmo = 1;
	if (timer->armed)
		ulh_timer_cancel(timer);

	timer->arm_time = ulh_timer_gettime();
	timer->exp_time = timer->arm_time + tmo;

	dl_list_foreach(it, &queue->queue, link) {
		if (timer->exp_time < it->exp_time) {
			dl_list_insert_before(&it->link, &timer->link);
			goto inserted;
		}
	}
	dl_list_insert_tail(&queue->queue, &timer->link);

inserted:
	timer->armed = 1;

	return 0;
}

int ulh_timer_cancel(struct ulh_timer *timer)
{
	if (!timer->armed)
		return 0;

	dl_list_remove(&timer->link);
	timer->armed = 0;

	return 0;
}

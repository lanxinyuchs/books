#ifndef ULH_TIMER_H__
#define ULH_TIMER_H__

#include <stdint.h>
#include <ulh_dl_list.h>

struct ulh_timerqueue {
	struct dl_list queue;
};

struct ulh_timer {
	struct dl_list link;
	int armed;
	uint64_t exp_time;
	uint64_t arm_time;

	void (*cb)(void *);
	void *cb_param;
};

void ulh_timerqueue_init(struct ulh_timerqueue *);
int ulh_timerqueue_schedule(struct ulh_timerqueue *);
void ulh_timerqueue_destroy(struct ulh_timerqueue *);

void ulh_timer_init(struct ulh_timer *, void (*)(void *), void *);
int ulh_timer_arm(struct ulh_timer *, struct ulh_timerqueue *, uint32_t);
int ulh_timer_cancel(struct ulh_timer *);

#endif /* ULH_TIMER_H__ */

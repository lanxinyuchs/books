#ifndef __STATE_H
#define __STATE_H

#include <errno.h>
#include <pthread.h>
#include <string.h>
#include <stdbool.h>

struct state;

#include "child.h"
#include "domain.h"
#include "cb.h"

struct state {
	struct child_head cglist;     /* list of all children */
	struct child_head cllist;     /* the launch list */
	struct child_head cslist;     /* the list for stopped children */
	struct domain_head dglist;    /* list of all domains */
	struct domain_head dslist;    /* the list for stopped domains */
	struct child *complete_ch;
	struct child *wait_ch;
	struct timespec shutdown_ts;
	bool shutdown;
	bool cmdshutdown;
	bool boot_done;
};

extern void state_init(struct state *s);

#define timestamp(ts) do {                                          \
	if (clock_gettime(CLOCK_MONOTONIC, ts) < 0)                     \
		reboot_fmt("Failed clock_gettime (%s)", strerror(errno));   \
} while (0)

#endif /* __STATE_H */

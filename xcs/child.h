#ifndef __CHILD_H
#define __CHILD_H

#include <time.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/queue.h>

TAILQ_HEAD(child_head, child);

typedef enum {
	CHILD_STATE_STOPPED = 0, /* The child is not running, and has been serviced */
	CHILD_STATE_STARTED,     /* The child has been started */
	CHILD_STATE_TERMINATED,  /* The child has been asked to terminate */
	CHILD_STATE_ABORTED,     /* The child has been asked to abort */
	CHILD_STATE_KILLED,      /* The child has been killed */
} child_state_t;

#include "state.h"
#include "domain.h"

struct child {
	char **argv;
	child_state_t state;
	bool cmdstopped;
	pid_t pid;
	int retry;
	int retries;
	int wait_timeout_ms;
	int retry_timeout_ms;
	int abort_timeout_ms;
	int alive_timeout_ms;
	int run_until_completion;
	int affinity;
	struct timespec wait_ts;
	struct timespec stopped_ts;
	struct timespec alive_ts;
	struct domain *domain;
	TAILQ_ENTRY(child) cglist_entry;  /* entry in the global child list */
	TAILQ_ENTRY(child) cdlist_entry;  /* entry in the domain list */
	TAILQ_ENTRY(child) cllist_entry;  /* entry in the launch list */
	TAILQ_ENTRY(child) cslist_entry;  /* entry in the stopped list */
};

struct child_config {
	int retries;
	int retry_timeout_ms;
	int abort_timeout_ms;
	int alive_timeout_ms;
	int wait_timeout_ms;
	int run_until_completion;
	int affinity;
};

#define child_first(list, s) TAILQ_FIRST(&(s)->list)
#define child_last(list, s) TAILQ_LAST(&(s)->list, child_head)
#define child_next(ch, list) TAILQ_NEXT(ch, list##_entry)
#define child_prev(ch, list) TAILQ_PREV(ch, child_head, list##_entry)
#define child_append(ch, list, s) TAILQ_INSERT_TAIL(&(s)->list, ch, list##_entry)
#define child_remove(ch, list, s) TAILQ_REMOVE(&(s)->list, ch, list##_entry)
#define child_remove_first(list, s) child_remove(child_first(list, s), list, s)
#define child_foreach(ch, list, s) TAILQ_FOREACH(ch, &(s)->list, list##_entry)

#define child_count(list, s) ({              \
	struct child *ch;                        \
	int cnt = 0;                             \
	child_foreach(ch, list, s) cnt++;        \
	cnt;                                     \
})

#define child_delete_all(list, s) do {       \
	struct child *next;                      \
	struct child *ch = child_first(list, s); \
	while (ch) {                             \
		next = child_next(ch, list);         \
		child_delete(ch);                    \
		ch = next;                           \
	}                                        \
} while (0)

#define child_last_running(list, s) ({               \
	struct child *_ch = child_last(list, s);         \
	while (_ch && _ch->state == CHILD_STATE_STOPPED) \
		_ch = child_prev(_ch, list);                 \
	_ch;                                             \
})

extern struct child * child_create(char **argv, struct domain *d,
                                   struct child_config *cfg);
extern void child_delete(struct child *ch);
extern void child_launch(struct child *ch);
extern void child_died(struct child *ch, struct state *s);
extern bool child_is_stopped(struct child *ch);
extern void child_set_state(struct child *ch, child_state_t state);
extern void child_escalate(struct child *ch, child_state_t state, struct state *s);
extern int child_get_timeout(struct child *ch);
extern struct child * child_get_from_pid(pid_t pid, struct state *s);

#endif /* __CHILD_H */

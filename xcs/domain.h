#ifndef __DOMAIN_H
#define __DOMAIN_H

#include <time.h>
#include <stdbool.h>
#include <string.h>
#include <sys/queue.h>

TAILQ_HEAD(domain_head, domain);

typedef enum {
	DOMAIN_STATE_STOPPED = 0,
	DOMAIN_STATE_STARTED,
	DOMAIN_STATE_TERMINATED,
	DOMAIN_STATE_ABORTED,
	DOMAIN_STATE_KILLED,
} domain_state_t;

#include "child.h"
#include "state.h"

struct domain {
	char *name;
	domain_state_t state;
	bool cmdstopped;
	int retry;
	int retries;
	int retry_timeout_ms;
	int abort_timeout_ms;
	struct timespec stopped_ts;
	struct child_head cdlist;          /* list of children in the domain */
	TAILQ_ENTRY(domain) dglist_entry;  /* entry in the global domain list */
	TAILQ_ENTRY(domain) dslist_entry;  /* entry in the stopped domain list */
};

struct domain_config {
	int retries;
	int retry_timeout_ms;
	int abort_timeout_ms;
};

#define domain_first(list, s) TAILQ_FIRST(&(s)->list)
#define domain_last(list, s) TAILQ_LAST(&(s)->list, domain_head)
#define domain_next(d, list) TAILQ_NEXT(d, list##_entry)
#define domain_prev(d, list) TAILQ_PREV(d, domain_head, list##_entry)
#define domain_append(d, list, s) TAILQ_INSERT_TAIL(&(s)->list, d, list##_entry)
#define domain_remove(d, list, s) TAILQ_REMOVE(&(s)->list, d, list##_entry)
#define domain_remove_first(list, s) domain_remove(domain_first(list, s), list, s)
#define domain_foreach(d, list, s) TAILQ_FOREACH(d, &(s)->list, list##_entry)


#define domain_delete_all(list, s) do {         \
	struct domain *next;                        \
	struct domain *d = domain_first(list, s);   \
	while (d) {                                 \
		next = domain_next(d, list);            \
		domain_delete(d);                       \
		d = next;                               \
	}                                           \
} while (0)

extern struct domain * domain_create(const char *name, struct domain_config *cfg);
extern void domain_delete(struct domain *d);
extern void domain_set_state(struct domain *d, domain_state_t state);
extern void domain_escalate(struct domain *d, domain_state_t state);
extern void domain_launch(struct domain *d, struct state *s);
extern bool domain_is_stopped(struct domain *d);
extern struct domain *domain_get_by_name(const char *name, struct state *s);
extern int domain_get_timeout(struct domain *d);
extern child_state_t domain_get_child_state(domain_state_t state);

#endif /* __DOMAIN_H */

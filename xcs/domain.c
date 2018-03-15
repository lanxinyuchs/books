#include <stdlib.h>

#include "domain.h"
#include "child.h"
#include "state.h"
#include "log.h"
#include "cb.h"

static const char *state_strings[] = {
	"stopped",
	"running",
	"terminated",
	"aborted",
	"killed",
};

struct domain * domain_create(const char *name, struct domain_config *cfg)
{
	struct domain *d = NULL;

	d = calloc(1, sizeof(struct domain));

	if (!d)
		return NULL;

	d->retries = cfg->retries;
	d->retry_timeout_ms = cfg->retry_timeout_ms;
	d->abort_timeout_ms = cfg->abort_timeout_ms;

	TAILQ_INIT(&d->cdlist);

	d->name = strdup(name);
	if (!d->name) {
		free(d);
		return NULL;
	}

	log_info("New domain %s added", d->name);

	return d;
}

void domain_delete(struct domain *d)
{
	if (!d) return;

	if (d->name)
		free(d->name);

	free(d);
}

void domain_set_state(struct domain *d, domain_state_t state)
{
	struct child *ch = child_last_running(cdlist, d);
	child_state_t cstate = domain_get_child_state(state);

	if (ch)
		child_set_state(ch, cstate);

	if (state == DOMAIN_STATE_TERMINATED)
		CB(domain_stop, d->name);

	timestamp(&d->stopped_ts);
	d->state = state;
}

void domain_escalate(struct domain *d, domain_state_t state)
{
	if (d->state == DOMAIN_STATE_KILLED) {
		reboot_fmt("domain %s escalated to death", d->name);
		return; /* Keep coverity happy */
	}

	/* Escalate to at least the given state */
	if (d->state + 1 > state)
		state = d->state + 1;

	log_info("escalating domain %s state to %s",
	         d->name, state_strings[state]);

	domain_set_state(d, state);
}

void domain_launch(struct domain *d, struct state *s)
{
	struct child *ch;

	child_foreach(ch, cdlist, d) {
		ch->retry = 0;
		child_append(ch, cllist, s);
	}

	memset(&d->stopped_ts, 0, sizeof(struct timespec));
}

bool domain_is_stopped(struct domain *d)
{
	struct child *ch;
	bool result = true;

	child_foreach(ch, cdlist, d) {
		if (!child_is_stopped(ch))
			result = false;
	}

	if (result)
		d->state = DOMAIN_STATE_STOPPED;

	return result;
}

struct domain *domain_get_by_name(const char *name, struct state *s)
{
	struct domain *d;

	domain_foreach(d, dglist, s) {
		if (strcmp(d->name, name) == 0)
			return d;
	}

	return 0;
}

int domain_get_timeout(struct domain *d)
{
	int timeout;

	switch (d->state) {
		case DOMAIN_STATE_TERMINATED:
			timeout = d->retry_timeout_ms;
			break;
		case DOMAIN_STATE_ABORTED:
			timeout = d->abort_timeout_ms;
			break;
		default:
			timeout = 0;
			break;
	}

	return timeout;
}

child_state_t domain_get_child_state(domain_state_t state)
{
	switch (state) {
		case DOMAIN_STATE_STOPPED:
			return CHILD_STATE_STOPPED;
		case DOMAIN_STATE_STARTED:
			return CHILD_STATE_STARTED;
		case DOMAIN_STATE_TERMINATED:
			return CHILD_STATE_TERMINATED;
		case DOMAIN_STATE_ABORTED:
			return CHILD_STATE_ABORTED;
		case DOMAIN_STATE_KILLED:
			return CHILD_STATE_KILLED;
	}

	return -1;
}

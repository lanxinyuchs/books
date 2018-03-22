#include <stdint.h>
#include <string.h>
#include <sys/queue.h>

#include "cb.h"
#include "state.h"

void state_init(struct state *s)
{
	memset(s, 0, sizeof(struct state));
	TAILQ_INIT(&s->cglist);
	TAILQ_INIT(&s->cllist);
	TAILQ_INIT(&s->cslist);

	TAILQ_INIT(&s->dglist);
	TAILQ_INIT(&s->dslist);
}

uint64_t state_get_new_id(struct state *s)
{
	if (s->id > UINT64_MAX - 1) {
		/* Overflow! */
		CB(reboot, "Overflow of mama child id");
	}

	return (s->id)++;
}

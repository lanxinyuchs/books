#include <string.h>
#include <sys/queue.h>

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

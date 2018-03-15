#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/queue.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sched.h>

#include "mama.h"
#include "domain.h"
#include "child.h"
#include "log.h"
#include "state.h"
#include "cb.h"

#ifdef __LTTNG__
static const char *state_strings[] = {
	"stopped",
	"started",
	"terminated",
	"aborted",
	"killed",
};
#endif

static void send_sig(struct child *ch, int sig)
{
	int ret = kill(ch->pid, sig);

	if (ret == 0) {
		log_info("sent signal %d to child %s with pid %d",
		         sig, basename(ch->argv[0]), ch->pid);
	}
	else if (ret == -1 && errno == ESRCH) {
		log_info("the child %s with pid %d had already quit",
		         basename(ch->argv[0]), ch->pid);
	}
	else {
		reboot_fmt("failed to send %d to child %s with pid %d (%s)",
		           sig, basename(ch->argv[0]), ch->pid, strerror(errno));
	}
}

static void set_affinity(struct child *ch)
{
	cpu_set_t s;

	CPU_ZERO(&s);
	CPU_SET(ch->affinity, &s);

	if (sched_setaffinity(ch->pid, sizeof(cpu_set_t), &s) < 0) {
		reboot_fmt("Failed to set affinity %d of %d (%s)", ch->affinity,
		           ch->pid, ch->argv[0]);
	}
}

struct child * child_create(char **argv, struct domain *d,
                                   struct child_config *cfg)
{
	struct child *ch = NULL;

	if (cfg->retries && !cfg->retry_timeout_ms) {
		log_err("retries specified, but no (mandatory) retry_timout_ms for %s",
		        basename(argv[0]));
		return NULL;
	}

	ch = calloc(1, sizeof(struct child));
	if (!ch)
		return NULL;

	ch->argv = argv;
	ch->retries = cfg->retries;
	ch->retry_timeout_ms = cfg->retry_timeout_ms;
	ch->abort_timeout_ms = cfg->abort_timeout_ms;
	ch->alive_timeout_ms = cfg->alive_timeout_ms;
	ch->wait_timeout_ms = cfg->wait_timeout_ms;
	ch->run_until_completion = cfg->run_until_completion;
	ch->affinity = cfg->affinity;

	if (d) {
		child_append(ch, cdlist, d);
		ch->domain = d;
		log_info("child %s added to domain %s", basename(ch->argv[0]), d->name);
	}
	else {
		log_info("child %s added with no domain", basename(ch->argv[0]));
	}

	return ch;
}

void child_delete(struct child *ch)
{
	if (!ch) return;

	if (ch->argv) {
		int i;
		const char *arg;

		for (i = 0, arg = ch->argv[0]; arg; i++, arg = ch->argv[i])
			free((void *) arg);
		free(ch->argv);
	}

	free(ch);
}

void child_launch(struct child *ch)
{
	struct domain *d = ch->domain;

	pid_t child_pid = fork();

	if (child_pid == -1) {
		CB(reboot, "Failed fork()");
	}
	else if (child_pid == 0) {
		/* Unblock signals for children as these are inherited */
		sigset_t mask;

		sigemptyset(&mask);
		sigaddset(&mask, SIGSYNC);
		sigaddset(&mask, SIGALIVE);
		sigaddset(&mask, SIGCHLD);
		sigaddset(&mask, SIGTERM);

		if (pthread_sigmask(SIG_UNBLOCK, &mask, NULL) == -1)
			CB(reboot, "sigprocmask failed");

		execv(ch->argv[0], ch->argv);
		/* If execv returns, there was an error */
		reboot_fmt("Execv failed (%s): %s", ch->argv[0], strerror(errno));
	}

	log_info("launched child %s with pid %d", ch->argv[0], child_pid);

	ch->pid = child_pid;
	child_set_state(ch, CHILD_STATE_STARTED);
	memset(&ch->stopped_ts, 0, sizeof(struct timespec));

	if (ch->affinity >= 0)
		set_affinity(ch);

	if (d && d->state == DOMAIN_STATE_STOPPED) {
		d->state = DOMAIN_STATE_STARTED;
		CB(domain_start, d->name);
	}

	CB(child_launch, ch->argv[0], child_pid);

	/* Initialize the alive timeout */
	timestamp(&ch->alive_ts);
}

void child_died(struct child *ch, struct state *s)
{
	struct domain *d = ch->domain;

	if (s->shutdown) {
		struct child * last_ch;

		child_set_state(ch, CHILD_STATE_STOPPED);
		last_ch = child_last_running(cglist, s);
		if (last_ch)
			child_set_state(last_ch, CHILD_STATE_TERMINATED);
	}
	else if (d && d->state != DOMAIN_STATE_STARTED) {
		child_set_state(ch, CHILD_STATE_STOPPED);
		if (domain_is_stopped(d)) {
			if (d->cmdstopped) {
				log_info("domain %s stopped by command", d->name);
			}
			else {
				domain_remove(d, dslist, s);
				log_info("domain %s relaunched for the %dth time of %d",
				         d->name, d->retry, d->retries);
				domain_launch(d, s);
			}
		}
		else {
			struct child * last_ch = child_last_running(cdlist, d);

			if (d->cmdstopped) {
				log_info("child %s with pid %d in domain %s died after "
				         "being stopped by command", basename(ch->argv[0]),
				         ch->pid, d->name);
			} else {
				log_info("child %s with pid %d in domain %s died because "
				         "domain is shutting down", basename(ch->argv[0]),
				         ch->pid, d->name);
			}

			if (last_ch) {
				child_state_t cstate = domain_get_child_state(d->state);
				child_set_state(last_ch, cstate);
			}
		}
	}
	else if (ch->cmdstopped) {
		child_set_state(ch, CHILD_STATE_STOPPED);
		log_info("child %s with pid %d died after being stopped by command",
		         basename(ch->argv[0]), ch->pid);
	}
	else if (ch->retry < ch->retries) {
		if (ch->state != CHILD_STATE_STARTED) {
			log_info("stopped child %s with pid %d died",
			         basename(ch->argv[0]), ch->pid);
			child_remove(ch, cslist, s);
		}

		child_set_state(ch, CHILD_STATE_STOPPED);
		child_launch(ch);
		ch->retry++;
		log_info("child %s relaunched for the %dth time of %d",
		         basename(ch->argv[0]), ch->retry, ch->retries);

		if (s->wait_ch == ch) {
			timestamp(&ch->wait_ts);
		}
	}
	else if (d && (d->retry < d->retries)) {
		child_set_state(ch, CHILD_STATE_STOPPED);
		d->retry++;
		log_info("restarting domain %s for the %dth time of %d",
				d->name, d->retry, d->retries);
		domain_set_state(d, DOMAIN_STATE_TERMINATED);
		domain_append(d, dslist, s);
	}
	else {
		CB(reboot, "child died, configuration exhausted");
	}
}

bool child_is_stopped(struct child *ch)
{
	int ret = kill(ch->pid, 0);

	if (ret == 0) {
		log_trace2("check result: the child %s with pid %d is still alive",
				   basename(ch->argv[0]), ch->pid);
		return false;
	}
	else if (ret == -1 && errno == ESRCH) {
		log_trace2("check result: the child %s with pid %d has quit",
				   basename(ch->argv[0]), ch->pid);
	}
	else {
		reboot_fmt("Failed to check if child %s with pid %d (%s) is alive",
				   basename(ch->argv[0]), ch->pid, strerror(errno));
	}

	return true;
}

void child_set_state(struct child *ch, child_state_t state)
{
	switch (state) {
		case CHILD_STATE_STOPPED:
			ch->state = CHILD_STATE_STOPPED;
			break;
		case CHILD_STATE_STARTED:
			ch->state = CHILD_STATE_STARTED;
			break;
		case CHILD_STATE_TERMINATED:
			if (!child_is_stopped(ch)) {
				send_sig(ch, SIGTERM);
				ch->state = CHILD_STATE_TERMINATED;
				timestamp(&ch->stopped_ts);
			}
			break;
		case CHILD_STATE_ABORTED:
			if (!child_is_stopped(ch)) {
				send_sig(ch, SIGABRT);
				ch->state = CHILD_STATE_ABORTED;
				timestamp(&ch->stopped_ts);
			}
			break;
		case CHILD_STATE_KILLED:
			if (!child_is_stopped(ch)) {
				send_sig(ch, SIGKILL);
				ch->state = CHILD_STATE_KILLED;
			}
			break;
		default:
			log_err("Unsupported state %d", state);
			return;
	}

	log_trace2("child %s with pid %d now in state %s",
	           basename(ch->argv[0]), ch->pid, state_strings[ch->state]);
}

void child_escalate(struct child *ch, child_state_t state, struct state *s)
{
	if (ch->state == CHILD_STATE_STARTED)
		child_append(ch, cslist, s);

	if (ch->state == CHILD_STATE_KILLED)
		reboot_fmt("child %s with pid %d escalated to death",
		           basename(ch->argv[0]), ch->pid);

	/* Escalate to at least the given state */
	if (ch->state + 1 > state)
		state = ch->state + 1;

	child_set_state(ch, state);
}

int child_get_timeout(struct child *ch)
{
	int timeout;

	switch (ch->state) {
		case CHILD_STATE_TERMINATED:
			timeout = ch->retry_timeout_ms;
			break;
		case CHILD_STATE_ABORTED:
			timeout = ch->abort_timeout_ms;
			break;
		default:
			timeout = 0;
			break;
	}

	return timeout;
}

struct child * child_get_from_pid(pid_t pid, struct state *s)
{
	struct child *ch;

	child_foreach(ch, cglist, s) {
		if (ch->pid == pid)
			return ch;
	}

	return 0;
}

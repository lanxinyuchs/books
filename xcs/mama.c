#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include <libgen.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/signalfd.h>

#include "libmama.h"
#include "mama.h"
#include "child.h"
#include "domain.h"
#include "config.h"
#include "state.h"
#include "log.h"
#include "cmd.h"
#include "cb.h"

#define DEFAULT_CFG_DIR "/etc/mama.d"

struct mama_callbacks cb;
void *cb_user_data;

#define GOT_TIMEOUT    0
#define GOT_SIGNAL     1
#define GOT_CONNECTION 2
#define GOT_SOCKET_CMD 3

struct mama_handle {
	char *cfg_file;
	char *cfg_dir;
	char **tags;
	int sfd;
};

static int timespec_diff(const struct timespec *start, const struct timespec *end)
{
    struct timespec diff;

    if ((end->tv_nsec - start->tv_nsec) < 0) {
        diff.tv_sec = end->tv_sec - start->tv_sec - 1;
        diff.tv_nsec = 1000000000l + end->tv_nsec - start->tv_nsec;
    } else {
        diff.tv_sec = end->tv_sec - start->tv_sec;
        diff.tv_nsec = end->tv_nsec - start->tv_nsec;
    }

    return diff.tv_sec * 1000l + diff.tv_nsec / 1000000l;
}

#define SETMAX(max, x) if ((x) > (max)) (max) = (x)

static int getsignal(int fd, int cmd_server_fd, int *cmd_client_fds,
                     int num_client_fds, struct signalfd_siginfo *siginfo,
                     int32_t timeout_ms, int *cmd_client_idx)
{
	struct timeval tv;
	struct timeval *tvp;
	fd_set set;
	int fd_max;
	int ret;

	if (timeout_ms < 0) {
		tvp = NULL;
	}
	else {
		tv.tv_sec = timeout_ms / 1000;
		tv.tv_usec = (timeout_ms % 1000) * 1000;
		tvp = &tv;
	}

	FD_ZERO(&set);
	FD_SET(fd, &set);
	fd_max = fd;

	FD_SET(cmd_server_fd, &set);
	SETMAX(fd_max, cmd_server_fd);

	for (int i = 0; i < num_client_fds; i++) {
		if (cmd_client_fds[i] >= 0) {
			FD_SET(cmd_client_fds[i], &set);
			SETMAX(fd_max, cmd_client_fds[i]);
		}
	}

	while ((ret = select(fd_max + 1, &set, NULL, NULL, tvp)) == -1 && errno == EINTR)
		continue;

	if (ret == -1)
		reboot_fmt("Failed to select on pipe (%s)", strerror(errno));

	if (FD_ISSET(fd, &set)) {
		ret = read(fd, siginfo, sizeof(struct signalfd_siginfo));
		if (ret != sizeof(struct signalfd_siginfo))
			reboot_fmt("Failed to read from signalfd (%s)", strerror(errno));

		return GOT_SIGNAL;
	}

	if (FD_ISSET(cmd_server_fd, &set))
		return GOT_CONNECTION;

	for (int i = 0; i < num_client_fds; i++) {
		if (cmd_client_fds[i] >= 0 && FD_ISSET(cmd_client_fds[i], &set)) {
			*cmd_client_idx = i;
			return GOT_SOCKET_CMD;
		}
	}

	return GOT_TIMEOUT;
}

static int calculate_diff(struct timespec *o, struct timespec *c,
                          int timeout_ms)
{
	if (timeout_ms && (o->tv_sec || o->tv_nsec)) {
		int current = timespec_diff(o, c);
		return timeout_ms - current;
	}

	return INT_MAX;
}

static void got_sync(pid_t pid, struct state *s)
{
	struct child *ch = child_get_from_pid(pid, s);

	if (!ch) {
		log_info("SIGSYNC recv from unmanaged pid %d", pid);
	}
	else if (s->wait_ch && s->wait_ch->pid == ch->pid) {
		timestamp(&s->wait_ch->alive_ts);
		log_info("Got SIGSYNC from child %s with pid %d, continuing...",
		         basename(ch->argv[0]), ch->pid);
		s->wait_ch = NULL;
	}
	else {
		log_trace2("SIGSYNC recv from unexpected child %s pid %d",
		           basename(ch->argv[0]), ch->pid);
	}
}

static void got_shutdown(pid_t pid, struct state *s, struct config *cfg)
{
	struct child *ch;

	if (cfg->only_managed_children_can_shut_down) {
		ch = child_get_from_pid(pid, s);
		if (!ch) {
			log_info("SIGTERM received from unmanaged pid %d", pid);
		}
	}
	else if (!s->shutdown) {
		log_info("shutting down all children");

		ch = child_last_running(cglist, s);

		if (ch)
			child_set_state(ch, CHILD_STATE_TERMINATED);

		timestamp(&s->shutdown_ts);
		s->shutdown = true;
	}
}

static void got_alive(pid_t pid, struct state *s)
{
	struct child *ch = child_get_from_pid(pid, s);

	if (!ch) {
		log_info("SIGALIVE recv from unmanaged pid %d", pid);
	}
	else if (ch->alive_timeout_ms) {
		timestamp(&ch->alive_ts);
	}
	else {
		log_trace2("SIGALIVE recv from unexpected child %s pid %d",
		           basename(ch->argv[0]), ch->pid);
	}
}

static void got_chld(struct state *s, struct config *cfg)
{
	/* Multiple children may have died, loop to make sure we get all */
	while (1) {
		int status;
		pid_t pid = waitpid(-1, &status, WNOHANG);
		if (pid <= 0)
			break;

		struct child *ch = child_get_from_pid(pid, s);
		if (ch) {
			ch->pid = 0;
			log_info("child %s with pid %d died", basename(ch->argv[0]), pid);

			if (ch == s->complete_ch &&
			    WIFEXITED(status) &&
			    WEXITSTATUS(status) == 0)
			{
				log_info("child %s with pid %d completed successfully",
				         basename(ch->argv[0]), pid);
				child_set_state(ch, CHILD_STATE_STOPPED);
				s->complete_ch = NULL;
				continue;
			}

			/*
			 * If a child is ordered to stop via a command, we don't call the
			 * exit callback.
			 */
			else if (!(ch->cmdstopped || ch->terminated_sockfd >= 0)) {
				CB(child_exit, ch->argv[0], pid, status);
			}

			if (cfg->require_initial_launch_success && !s->boot_done)
				CB(reboot, "initial launch not complete");
			else
				child_died(ch, s);
		}
		else {
			log_err("failed to get child from pid %d", pid);
		}
	}
}


static int shutdown_timeout(struct state *s, struct config *cfg)
{
	struct child *ch;
	struct timespec ts;
	bool stopped = true;
	int diff;

	child_foreach(ch, cglist, s) {
		if (!child_is_stopped(ch))
			stopped = false;
	}

	if (stopped) {
		log_info("shutdown of all children complete, exiting");
		CB(exit, s->cmdshutdown ?
		   MAMA_EXIT_REASON_COMMAND : MAMA_EXIT_REASON_ORDERED);
	}

	timestamp(&ts);
	diff = calculate_diff(&s->shutdown_ts, &ts, cfg->shutdown_timeout_ms);
	if (diff < 0 && !s->cmdshutdown) {
		log_info("shutdown timeout %dms exceeded, rebooting",
		         cfg->shutdown_timeout_ms);
		CB(exit, MAMA_EXIT_REASON_TIMEOUT);
	}

	return diff + 1;
}

static int check_and_calculate_timeout(struct state *s)
{
	struct domain *d;
	struct child *ch;
	struct timespec ts;
	int min = INT_MAX;
	int timeout;

	timestamp(&ts);

	/* Check if we have received pings in a timely fashion */
	child_foreach(ch, cglist, s) {
		/* If the child isn't launched yet, or is stopped, skip it */
		if (!ch->pid || ch->state != CHILD_STATE_STARTED) continue;
		if (!ch->alive_timeout_ms) continue;
		int diff = calculate_diff(&ch->alive_ts, &ts, ch->alive_timeout_ms);
		if (diff < 0) {
			log_info("child with pid %d exceeded alive timeout treshold %d",
			         ch->pid, ch->alive_timeout_ms);
			child_escalate(ch, CHILD_STATE_ABORTED, s);
		}
		else if (diff < min) min = diff;
	}

	/* Check if we have waited long enough for SIGSYNC */
	if (s->wait_ch) {
		int diff = calculate_diff(&s->wait_ch->wait_ts, &ts,
		                          s->wait_ch->wait_timeout_ms);
		if (diff < 0) {
			log_info("child wait timeout %dms exceeded",
			         s->wait_ch->wait_timeout_ms);
			child_escalate(s->wait_ch, CHILD_STATE_ABORTED, s);
		}
		else if (diff < min) min = diff;
	}

	/* Check if we have waited long enough after a child has been stopped */
	child_foreach(ch, cslist, s) {
		if (ch->state == CHILD_STATE_STARTED) continue;
		if (child_is_stopped(ch)) continue;

		timeout = child_get_timeout(ch);

		int diff = calculate_diff(&ch->stopped_ts, &ts, timeout);
		if (diff < 0) {
			log_info("child restart timeout %dms exceeded", timeout);
			child_escalate(ch, CHILD_STATE_ABORTED, s);
			diff = child_get_timeout(ch);
			if (diff && diff < min) min = diff;
		}
		else if (diff < min) min = diff;
	}

	/* Check if the domain stop has completed or we have waited too long */
	domain_foreach(d, dslist, s) {
		if (d->state == DOMAIN_STATE_STARTED) continue;

		timeout = domain_get_timeout(d);
		if (timeout) {
			int diff = calculate_diff(&d->stopped_ts, &ts, timeout);
			if (diff < 0) {
				log_info("domain restart timeout %dms exceeded", timeout);
				domain_escalate(d, DOMAIN_STATE_TERMINATED);
				diff = domain_get_timeout(d);
				if (diff && diff < min) min = diff;
			}
			else if (diff < min) min = diff;
		}
	}

	return min == INT_MAX ? -1 : min + 1;
}

static int write_int_to_file(int val, char *file)
{
	int status = -1;
	FILE *fp = NULL;
	char *string = NULL;

	fp = fopen(file, "w");
	if (fp == NULL) {
		log_err("Failed to open %s for writing\n", file);
		goto exit;
	}

	if (asprintf(&string, "%d", val) < 0) {
		log_err("Failed to generate value string\n");
		goto exit;
	}

	if (fwrite(string, strlen(string), 1, fp) != 1) {
		log_err("Failed to write value string %s to %s\n", string, file);
		goto exit;
	}

	log_info("Successfully wrote %d to %s", val, file);

	status = 0;

exit:
	if (string) free(string);
	if (fp) {
		if (fflush(fp) != 0)
			log_err("Failed to flush fp\n");
		if (fclose(fp) != 0)
			log_err("Failed to close fp\n");
	}

	return status;
}

static int read_int_from_file(int *val, char *file)
{
	int status = -1;
	FILE *fp = NULL;

	fp = fopen(file, "r");
	if (fp == NULL) {
		log_err("Failed to open %s for reading\n", file);
		goto exit;
	}

	if (fscanf(fp, "%d", val) != 1) {
		log_err("Failed to read integer from file %s\n", file);
		goto exit;
	}

	status = 0;

exit:
	if (fp && fclose(fp) != 0)
		log_err("Failed to close fp\n");

	return status;
}

static int set_rt_sched(struct config *cfg)
{
	int cur_period = 0;
	int cur_runtime = -2;
	int period, runtime;

	if (read_int_from_file(&cur_period,
	                       "/proc/sys/kernel/sched_rt_period_us"))
		return -1;

	if (read_int_from_file(&cur_runtime,
	                       "/proc/sys/kernel/sched_rt_runtime_us"))
		return -1;

	runtime = cfg->sched_rt_runtime_us != -2 ?
	             cfg->sched_rt_runtime_us : cur_runtime;

	period = cfg->sched_rt_period_us != 0 ?
	             cfg->sched_rt_period_us : cur_period;

	if (runtime > period) {
		log_err("sched_rt_runtime_us (%d) cannot exceed sched_rt_period_us (%d)",
		        runtime, period);
		return -1;
	}

	if (period < cur_runtime) {
		/*
		 * We won't be able to set the new period to less than current runtime,
		 * so we need to reverse the writes.
		 */
		if (runtime != cur_runtime &&
		    write_int_to_file(runtime, "/proc/sys/kernel/sched_rt_runtime_us"))
			return -1;

		if (period != cur_period &&
		    write_int_to_file(period, "/proc/sys/kernel/sched_rt_period_us"))
			    return -1;
	}
	else {
		if (period != cur_period &&
		    write_int_to_file(period, "/proc/sys/kernel/sched_rt_period_us"))
			    return -1;

		if (runtime != cur_runtime &&
		    write_int_to_file(runtime, "/proc/sys/kernel/sched_rt_runtime_us"))
			return -1;

	}

	return 0;
}

static mama_handle_t init(struct mama_callbacks *cbs, void *user_data)
{
	sigset_t mask;
	int sfd;
	struct mama_handle *mama;

	log_init();

	sigemptyset(&mask);
	sigaddset(&mask, SIGSYNC);
	sigaddset(&mask, SIGALIVE);
	sigaddset(&mask, SIGCHLD);
	sigaddset(&mask, SIGTERM);

	if (!cbs || !cbs->reboot) {
		log_err("A reboot callback must be supplied");
		exit(1);
	}

	cb = *cbs;
	cb_user_data = user_data;

	/* block these signals and use signalfd instead */
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1)
		CB(reboot, "sigprocmask failed");

	sfd = signalfd(-1, &mask, 0);
	if (sfd == -1)
		CB(reboot, "signalfd failed");

	mama = (struct mama_handle *) calloc(1, sizeof(struct mama_handle));
	if (!mama)
		CB(reboot, "memory allocation failed");
	else
		mama->sfd = sfd;

	return mama;
}

mama_handle_t mama_init(char *cfg_file, struct mama_callbacks *cbs,
                        void *user_data)
{
	mama_handle_t mama = init(cbs, user_data);
	if (!mama)
		return NULL;

	if (cfg_file)
		mama->cfg_file = strdup(cfg_file);
	else
		CB(reboot, "A config file must be supplied");

	return mama;
}

mama_handle_t mama_init_dir(char *cfg_dir, char **tags,
                            struct mama_callbacks *cbs, void *user_data)
{
	mama_handle_t mama = init(cbs, user_data);
	if (!mama)
		return NULL;

	if (tags)
		mama->tags = tags;

	if (cfg_dir)
		mama->cfg_dir = strdup(cfg_dir);
	else
		mama->cfg_dir = DEFAULT_CFG_DIR;

	return mama;
}

void mama_start(mama_handle_t mama)
{
	struct config cfg;
	struct state s;
	int cmd_server_fd;
	struct child *ch;
	int *cmd_client_fds;
	struct signalfd_siginfo fdsi;
	int cmd_client_idx;

	log_info("mama started");

	if (!mama) {
		log_err("mama_init() must be called before mama_start()");
		exit(1);
	}

	config_initialize(&cfg);
	state_init(&s);

	if (mama->cfg_file) {
		if (config_parse(mama->cfg_file, &s, &cfg))
			reboot_fmt("Failed to parse configuration file: %s",
			           mama->cfg_file);
	}
	else if (mama->cfg_dir) {
		if (config_dir_parse(mama->cfg_dir, mama->tags, &s, &cfg))
			reboot_fmt("Failed to parse configuration directory: %s",
			           mama->cfg_dir);
	}
	else {
		/* Shouldn't happen */
		CB(reboot, "Neither config file or directory supplied!");
	}

	cmd_client_fds = malloc(sizeof(void *) * cfg.max_mamacmd_connections);
	if (!cmd_client_fds) {
		cmd_client_fds = (void *) 1; /* For coverity, never used */
		reboot_fmt("Failed to malloc space for mamacmd client fds");
	}

	/* Initialize all client connection file descriptors to -1 */
	for (int i = 0; i < cfg.max_mamacmd_connections; i++)
		cmd_client_fds[i] = -1;

	if (set_rt_sched(&cfg))
		reboot_fmt("Failed to set global rt sched params");

	cmd_server_fd = open_cmd_server_socket();
	if (cmd_server_fd == -1)
		CB(reboot, "Failed to create server sockfd");

	/* Add all configured children to the launch list */
	child_foreach(ch, cglist, &s)
		child_append(ch, cllist, &s);

	/* coverity[no_escape] the 'escape' is through the reboot cb */
	while (1) {
		int timeout;

		if (s.shutdown) {
			/*
			 * Ignore all other timeouts but shutdown timeout
			 * during ordered shutdown.
			 */
			timeout = shutdown_timeout(&s, &cfg);
		}
		else {
			/* Check if timeouts have occurred, and calculate new timeout */
			timeout = check_and_calculate_timeout(&s);
		}

		/*
		 * Try to pop a new child every time, as the launch list can be
		 * updated from multiple places.
		 */
		ch = child_first(cllist, &s);

		if (ch && !s.shutdown) {
			if (!s.wait_ch && !s.complete_ch) {
				child_launch(ch, &s);
				if (ch->run_until_completion) {
					s.complete_ch = ch;
				}
				else if (ch->wait_timeout_ms) {
					s.wait_ch = ch;
					timestamp(&ch->wait_ts);
				}
				child_remove_first(cllist, &s);

				/*
				 * Only poll for signals to launch more children
				 * as soon as possible, in case 'wait' isn't set.
				 */
				timeout = 0;
			}
		}
		else {
			s.boot_done = true;
		}

		int ret = getsignal(mama->sfd, cmd_server_fd, cmd_client_fds,
		                    cfg.max_mamacmd_connections, &fdsi, timeout,
		                    &cmd_client_idx);

		switch (ret) {
		case GOT_TIMEOUT:
			break;
		case GOT_SIGNAL:
			if (fdsi.ssi_signo == SIGCHLD)
				got_chld(&s, &cfg);
			else if (fdsi.ssi_signo == SIGTERM)
				got_shutdown(fdsi.ssi_pid, &s, &cfg);
			else if (fdsi.ssi_signo == SIGALIVE)
				got_alive(fdsi.ssi_pid, &s);
			else if (fdsi.ssi_signo == SIGSYNC)
				got_sync(fdsi.ssi_pid, &s);
			else
				log_err("Received unsupported signal %s", fdsi.ssi_pid);
			break;
		case GOT_CONNECTION:
			if (accept_cmd_client(cmd_server_fd, cmd_client_fds,
			                      cfg.max_mamacmd_connections))
				log_info("Failed to accept client socket connection");
			break;
		case GOT_SOCKET_CMD:
			service_cmd_client(&cmd_client_fds[cmd_client_idx], &s);
			break;
		default:
			log_err("Should not happen - getsignal returned %d", ret);
			break;
		}
	}
}

void mama_exit(mama_handle_t mama)
{
	if (mama) {
		if (mama->cfg_file)
			free(mama->cfg_file);
		free(mama);
	}
}

void mama_run(char *cfg_file, struct mama_callbacks *cbs, void *user_data)
{
	mama_handle_t h = mama_init(cfg_file, cbs, user_data);
	if (h) {
		mama_start(h);
		/* mama_start() will never return, keep coverity happy */
		mama_exit(h);
	}
}

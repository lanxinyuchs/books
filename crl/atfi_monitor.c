#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>
#include <getopt.h>
#include <libgen.h>
#include <sys/inotify.h>
#include "log.h"

#define IN_BUFF_LEN	4096
#define PATH_BUFF_LEN	512

#define BIN_PATH		"/usr/bin/"
#define ATFI_DAEMON		"atfid"
#define ATFI_MINIMAL_DAEMON	"atfi_minimald"

#define ATFI_DAEMON_PATH BIN_PATH ATFI_DAEMON
#define ATFI_MINIMAL_DAEMON_PATH BIN_PATH ATFI_MINIMAL_DAEMON

static void server_running(pid_t *dpid, char *dpath)
{
	char buff[PATH_BUFF_LEN] = {0};

	snprintf(buff, PATH_BUFF_LEN, "/proc/%d/exe", *dpid);
	readlink(buff, dpath, PATH_BUFF_LEN);
	log_trace2("atfi daemon running: %s", dpath);
}

static int launch_server(pid_t *dpid, char *daemon, char **argv)
{
	*dpid = fork();

	switch(*dpid) {
	case -1:
		log_err("Unable to run server: %s, errno %d", daemon, errno);
		return -1;
	case 0:
		/* child */
		execv(daemon, argv);

		log_err("We shouldn't be here, exec failed, exiting.");
		return -2;
	default:
		/* parent */
		break;
	}

	return 0;
}

static int handle_ecb_event(const struct inotify_event *event, pid_t *dpid, char **argv)
{
	int status;
	char dpath[PATH_BUFF_LEN] = {0};

	switch (event->mask) {
	case IN_DELETE:
		log_trace2("Ignoring ecb device disconnected as "
			   "server can handle disconnected device");
		break;
	case IN_CREATE:
		server_running(dpid, dpath);

		if (!strcmp(dpath, ATFI_DAEMON_PATH)) {
			log_trace2("Ignoring ecb device connected, "
				   "ATFI server already running");
			break;
		}

		log_info("ATFI connected, shutting down minimal daemon. Waiting %d", *dpid);

		kill(*dpid, SIGINT);
		waitpid(*dpid, &status, 0);
		if (!WIFEXITED(status)) {
			log_info("ATFI minimal server exited ungracefully");
		}

		log_info("Launch ATFI full daemon.");
		argv[0] = ATFI_DAEMON;
		launch_server(dpid, ATFI_DAEMON_PATH, argv);
		break;
	default:
		break;
	}

	return 0;
}

static int run(__attribute__((unused)) int argc, char **argv, char *dev)
{
	int infd, wd;
	int nbytes;
	char buff[IN_BUFF_LEN];
	char *tbuff;
	struct inotify_event *event;
	unsigned int last_state;
	pid_t dpid = 0;
	char *devpth;
	char *devdir;
	char *devfile;

	devpth = strdup(dev);
	devfile = basename(devpth);
	devdir = dirname(devpth);

	infd = inotify_init();

	if (infd == -1) {
		log_err("inotify init failed.");
		return -1;
	}

	wd = inotify_add_watch(infd, devdir, IN_CREATE | IN_DELETE);

        if (wd == -1) {
		log_err("Unable to setup watching descriptor.");
		return NULL;
	}

	if (access(dev, R_OK | W_OK)) {
		argv[0] = ATFI_MINIMAL_DAEMON;
		launch_server(&dpid, ATFI_MINIMAL_DAEMON_PATH, argv);
		last_state = IN_DELETE;
	} else {
		argv[0] = ATFI_DAEMON;
		launch_server(&dpid, ATFI_DAEMON_PATH, argv);
		last_state = IN_CREATE;
	}

	for (;;) {
		nbytes = read(infd, buff, IN_BUFF_LEN);
		if (nbytes == -1)
			continue;

		for (tbuff = buff; tbuff < buff + nbytes;
		     tbuff += sizeof(struct inotify_event) + event->len) {
			event = (struct inotify_event *) tbuff;

			if (strcmp(devfile, event->name) == 0) {
				if (event->mask == last_state)
					continue;

				handle_ecb_event(event, &dpid, argv);
				last_state = event->mask;
			}
		}
	}

	return 0;
}

int main(int argc, char *argv[])
{
	int daemonize = 1;
	int c;
	char *dev = NULL;
	const char *const   sopt = "hd:";
	const struct option lopt[] = {
		{ "help", no_argument, NULL, 'h'},
		{ "device", required_argument, NULL, 'd'},
		{ 0, 0, 0, 0 }
	};

	while ((c = getopt_long(argc, argv, sopt, lopt, NULL)) != -1) {
		switch (c) {
		case 'h':
			printf("Usage: atfi_monitord [option]\n"
			       "where option is one of:\n"
			       "-h|--help                 "
			       "Display this help message\n"
			       "<atfid cli params> that are passed "
			       "to atfid server on start.\n");
			return 0;
		case 'd':
			dev = optarg;
			break;
		default:
			break;
		}
	}

	if (daemonize) {
		if (daemon(0,0)) {
			perror("daemon");
			return -1;
		}
		log_info("ATFI supervisor starting.");
	}

	run(argc, argv, dev);

	if (daemonize) {
		log_info("ATFI supervisor exiting.");
	}

	return 0;
}

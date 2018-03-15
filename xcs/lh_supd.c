/*
 * This program supervises O&M link handlers by subscribing to ITC link handler
 * events. Each supervised link handler has its path stored in one of two lists:
 *
 * list.down - Link handlers down (ie. not connected)
 * list.up   - Link handlers up (ie. connected)
 *
 * As ITC link handler events are received paths are moved between the two
 * lists and the instant of timeout is re-calculated when appropriate.
 *
 * Restart conditions are checked periodically and also as events are received.
 *
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <libgen.h>
#include <signal.h>
#include <sys/queue.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <stdbool.h>

#include <itc.h>
#include <itc_system.h>

#include <libreboot.h>

#define TRACEPOINT_PROVIDER com_ericsson_xcs_lh_sup
#include <tpt_create.h>
#include <tpt.h>


#define DAEMON_NAME        "lh_supd"
#define DAEMON_LOCK_PREFIX "/LCK.."
#define DAEMON_LOCK_PATH   "/var/lock"
#define DAEMON_LOCK_FILE   DAEMON_LOCK_PATH DAEMON_LOCK_PREFIX DAEMON_NAME
#define DAEMON_EXIT_SIGNAL 0xdeadbeef


union itc_msg {
	uint32_t               msgno;
	struct itc_lnh_added   itc_lnh_added;
	struct itc_lnh_removed itc_lnh_removed;
};

struct sup_lnh {
	LIST_ENTRY(sup_lnh)  ptr;
	const char          *path;
};

#ifdef __MACHINE_ZYNQMP_MIMO
static const char *paths[] = {
        "bp0/",
        "bp1/",
        "bp2/",
        "trxm0/",
        "trxm1/",
        "trxm2/",
        "trxm3/"
};
#else
static const char *paths[] = {
	"bp0/",
	"bp1/",
	"bp0_0/",
	"bp0_1/",
	"bp0_2/",
	"bp0_3/",
	"bp0_4/",
	"bp1_0/",
	"bp1_1/",
	"bp1_2/",
	"bp1_3/",
	"bp1_4/"
};
#endif

static int       n_paths = sizeof(paths) / sizeof(const char*);

static int             application = 0;
static int             lnh_up_once = 0;
static struct timespec instant_of_timeout = { 0, 0 };
static itc_mbox_id_t   mbox = ITC_NO_ID;
static volatile int    exit_flag = 0;
static int             return_code = 0;
static char           *program;
static int             no_reboot = 0;

static struct {
	LIST_HEAD(list_down, sup_lnh) down;
	LIST_HEAD(list_up, sup_lnh) up;
} list = {
	.down = LIST_HEAD_INITIALIZER(&down),
	.up = LIST_HEAD_INITIALIZER(&up)
};


static void sig_handler(__attribute__((unused)) int signo)
{
	union itc_msg *msg;

	exit_flag = 1;
	msg = itc_alloc(sizeof(uint32_t), DAEMON_EXIT_SIGNAL);
	itc_send(&msg, mbox, ITC_MY_MBOX);
}

static void calc_timeout(void)
{
	static const struct timespec tmo = {
		.tv_sec = 300, .tv_nsec = 0
	}; /* 300 seconds (5 minutes) */

	if (clock_gettime(CLOCK_MONOTONIC, &instant_of_timeout) != 0) {
		TPT_ERROR(STR("clock_gettime() failed, errno: %d", errno));
		memset(&instant_of_timeout, 0, sizeof(instant_of_timeout));
		return;
	}

	instant_of_timeout.tv_sec += tmo.tv_sec;
	instant_of_timeout.tv_nsec += tmo.tv_nsec;
	if (instant_of_timeout.tv_nsec >= 1000000000) {
		instant_of_timeout.tv_nsec -= 1000000000;
		instant_of_timeout.tv_sec++;
	}
}

static int past_timeout(void)
{
	struct timespec now;

	if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) {
		TPT_ERROR(STR("clock_gettime() failed, errno: %d", errno));
		return 1;
	}

	if (now.tv_sec < instant_of_timeout.tv_sec) {
		return 0;
	} else if (now.tv_sec == instant_of_timeout.tv_sec &&
	           now.tv_nsec < instant_of_timeout.tv_nsec) {
		return 0;
	}

	return 1;
}

static struct sup_lnh* lookup_lnh_down(const char *path)
{
	struct sup_lnh *sup_lnh;

	LIST_FOREACH(sup_lnh, &list.down, ptr) {
		if (strcmp(sup_lnh->path, path) == 0) {
			return sup_lnh;
		}
	}

	return NULL;
}

static struct sup_lnh* lookup_lnh_up(const char *path)
{
	struct sup_lnh *sup_lnh;

	LIST_FOREACH(sup_lnh, &list.up, ptr) {
		if (strcmp(sup_lnh->path, path) == 0) {
			return sup_lnh;
		}
	}

	return NULL;
}

static int init(const char *argv0)
{
	static const struct sigaction act = {
		.sa_handler = sig_handler,
		.sa_flags = 0
	};
	struct sup_lnh *sup_lnh;
	const char     *sys_mode;
	char           *str;
	FILE           *fp;
	int             idx, res, fd;

#ifdef __MACHINE_ZYNQMP_MIMO
	if(strcmp("TRXM", getenv("SYS_BOARD_TYPE")) == 0)
	{
		TPT_INFO("TRXM BOARD TYPE\n");
		n_paths = 1;
	}
	else if (strcmp("BP", getenv("SYS_BOARD_TYPE")) == 0)
	{
		TPT_INFO("BP BOARD TYPE\n");
		n_paths = 7;
	}
	else
	{
		TPT_ERROR("Wrong board type\n");
		return -1;
	}
#endif
	/* Add all supervised link handler paths to the down list */
	for (idx = 0; idx < n_paths; idx++) {
		sup_lnh = calloc(1, sizeof(struct sup_lnh));
		if (sup_lnh == NULL) {
			TPT_ERROR("calloc() failed");
			return -1;
		}
		LIST_INSERT_HEAD(&list.down, sup_lnh, ptr);
		sup_lnh->path = paths[idx];
	}

	/* Calculate the instant of timeout before link handler up once */
	calc_timeout();

	/* Fetch the SYS_MODE */
	sys_mode = getenv("SYS_MODE");
	if (sys_mode != NULL && strcmp(sys_mode, "AUBOOT") == 0) {
		application = 0;
	} else {
		application = 1;
	}

	/* Create, check and lock HDB UUCP lock file */
	fd = open(DAEMON_LOCK_FILE, O_WRONLY | O_CREAT, S_IWUSR);
	if (fd == -1) {
		return -1;
	}
	if (flock(fd, LOCK_EX | LOCK_NB) == -1) {
		TPT_ERROR(STR("flock(%s) failed, errno: %d (lock not obtained)",
		              DAEMON_LOCK_FILE, errno));
		goto err_lock;
	}
	fp = fdopen(fd, "w");
	if (!fp) {
		TPT_ERROR(STR("fdopen() failed, errno: %d", errno));
		goto err_lock;
	}
	fprintf(fp, "%10d\n", getpid());
	fflush(fp);

	/* Initiate ITC */
	res = itc_init(4, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if (res != 0) {
		TPT_ERROR(STR("itc_init() returned %d", res));
		goto err_itc;
	}

	/* Create an ITC mailbox */
	mbox = itc_create_mailbox("lh_sup", 0);
	if (mbox == ITC_NO_ID) {
		TPT_ERROR("itc_create_mailbox() failed");
		goto err_itc;
	}

	/* Subscribe for ITC link handler added and removed events */
	itc_subscribe_events(ITC_EVENT_LNH_ADDED | ITC_EVENT_LNH_REMOVED);

	/* Setup signal handler */
	if (sigaction(SIGTERM, &act, NULL) != 0) {
		TPT_ERROR(STR("sigaction() failed, errno: %d", errno));
		goto err_itc;
	}

	/* Extract name of program from argv[0] */
	str = strdup(argv0);
	if (str == NULL) {
		TPT_ERROR("strdup() failed");
		goto err_itc;
	}
	program = strdup(basename(str));
	free(str);
	if (program == NULL) {
		TPT_ERROR("strdup() failed");
		goto err_itc;
	}

	return 0;

err_itc:
	unlink(DAEMON_LOCK_FILE);
	fclose(fp);
err_lock:
	close(fd);
	return -1;
}

static void shutdown(void)
{
	if (mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}

	unlink(DAEMON_LOCK_FILE);

	free(program);
}

static void restart(char *reason)
{
	TPT_INFO(reason);

	if (no_reboot) {

		TPT_INFO("No reboot performed (program option -x)");
		printf("REBOOT\n");
		fflush(stdout); /* Try not to buffer this string */

	} else {

		if (reboot_with_slot(program, reason, -1) != 0) {
			reboot_immediate(program, reason);
		}

		TPT_ERROR("Returned from reboot function!!!");

		exit_flag = 1;
		return_code = -1;
	}
}

#ifdef __MACHINE_ZYNQMP_MIMO
static bool slave_link_all_down(void)
{
	struct sup_lnh *sup_lnh;

	LIST_FOREACH(sup_lnh, &list.up, ptr) {
		if (strncmp(sup_lnh->path, "bp", 2) == 0) {
			return false;
		}
	}

	return true;
}
#endif

static void check_restart_conditions(void)
{
#ifdef __MACHINE_ZYNQMP_MIMO
        TPT_TRACE(3,
                  STR("Checking restart conditions (%s:%s:%s)",
                      lnh_up_once ? "Link up once" : "Link never up",
                      application ? "AUAPP" : "AUBOOT",
                      slave_link_all_down() ? "Link down" : "Link up"));
#else
	TPT_TRACE(3,
	          STR("Checking restart conditions (%s:%s:%s)",
	              lnh_up_once ? "Link up once" : "Link never up",
	              application ? "AUAPP" : "AUBOOT",
	              LIST_EMPTY(&list.up) ? "Link down" : "Link up"));
#endif
	if (!lnh_up_once && application && past_timeout()) {
		restart("Restart ordered due to initial Link timeout");
	}

#ifdef __MACHINE_ZYNQMP_MIMO
	if (lnh_up_once && slave_link_all_down() && past_timeout())
#else
	if (lnh_up_once && LIST_EMPTY(&list.up) && past_timeout())
#endif
	{
		restart("Restart ordered due to Link timeout");
	}
}

static void handle_lnh_added(const char *path)
{
	struct sup_lnh *sup_lnh;

	sup_lnh = lookup_lnh_down(path);
	if (sup_lnh != NULL) {
		LIST_REMOVE(sup_lnh, ptr);
		LIST_INSERT_HEAD(&list.up, sup_lnh, ptr);
#ifdef __MACHINE_ZYNQMP_MIMO
                if (strncmp(sup_lnh->path, "bp", 2) == 0) {
			lnh_up_once = 1;
		}
#else
		lnh_up_once = 1;
#endif
	}
}

static void handle_lnh_removed(const char *path)
{
	struct sup_lnh *sup_lnh;

	sup_lnh = lookup_lnh_up(path);
	if (sup_lnh != NULL) {
		LIST_REMOVE(sup_lnh, ptr);
		LIST_INSERT_HEAD(&list.down, sup_lnh, ptr);
#ifdef __MACHINE_ZYNQMP_MIMO
		if (slave_link_all_down()) {
			TPT_INFO("All slave link down\n");
#else
		if (LIST_EMPTY(&list.up)) {
#endif
			calc_timeout();
		}
	}
}

static void receive_message(union itc_msg *msg)
{
	if (msg->msgno == ITC_LNH_ADDED) {
		TPT_INFO(STR("ITC event LNH Added: \"%s\"",
                                 msg->itc_lnh_added.lnhpath));
		handle_lnh_added(msg->itc_lnh_added.lnhpath);
	} else if (msg->msgno == ITC_LNH_REMOVED) {
		TPT_INFO(STR("ITC event LNH Removed: \"%s\"",
                                 msg->itc_lnh_removed.lnhpath));
		handle_lnh_removed(msg->itc_lnh_removed.lnhpath);
	} else if (msg->msgno == DAEMON_EXIT_SIGNAL) {
		TPT_TRACE(2, "Daemon exit signal");
	} else {
		TPT_INFO(STR("ABN: Unexpected message 0x%08x from 0x%08x",
		             msg->msgno, itc_sender(msg)));
	}
}

static void main_loop(void)
{
	union itc_msg *msg;

	while (!exit_flag) {
		msg = itc_receive(ITC_NOFILTER, 1000, ITC_FROM_ALL);
		if (msg != NULL) {
			receive_message(msg);
			itc_free(&msg);
		}
		if (!exit_flag) {
			check_restart_conditions();
		}
	}
}

static void help(void)
{
	printf("Usage: %s [options]\n\n"
	       "Options:\n"
	       "-h  Display usage information (this message)\n"
	       "-x  For testing purposes don't reboot,"
	       " but print \"REBOOT\\n\" to stdout\n"
	       "-d  Daemonize the program\n\n",
	       program);
}

int main(int argc, char *argv[])
{
	static const char short_options[] = "hxd";
	int               c, daemonize = 0;

	while ((c = getopt(argc, argv, short_options)) != -1) {
		switch (c) {
		case 'h':
			help();
			return 0;
		case 'x':
			no_reboot = 1;
			break;
		case 'd':
			daemonize = 1;
			break;
		default:
			help();
			return -1;
		}
	}

	if (!daemonize || !daemon(0, 1)) {
		TPT_INFO("Starting daemon");
		if (init(argv[0]) == 0) {
			TPT_INFO("Daemon running");
			main_loop();
			shutdown();
		} else {
			TPT_ERROR("Failed to initialize daemon");
			return -1;
		}
	} else {
		TPT_ERROR("Failed to start daemon");
		return -1;
	}

	return return_code;
}

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <libgen.h>
#include <getopt.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/inotify.h>
#include <time.h>
#include <string.h>
#include <pthread.h>
#include <limits.h>

#include <llog.h>
#include <libreboot_private.h>
#include <libmama.h>
#include <mama.h>

#define TRACEPOINT_PROVIDER com_ericsson_mamad
#include <tpt_create.h>
#include <tpt.h>

struct pmd {
	pid_t pid;
	pid_t ppid;
	char *program;
	int signal;
	time_t crashtime;
	char *workingdir;
	char *pmdfile;
};

static struct llog_info {
	char *program;
	char *pmd;
	pid_t pid;
	int signal;
} li;

#define COMMAND_EXIT_CODE 0xef

static pthread_mutex_t mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
static struct mama_callbacks cbs;
static int notify_pipe[2] = { -1, -1 };

static void exit_notify_thread(void)
{
	char dummy = 0;

	if (notify_pipe[1] > 0)
		(void) write(notify_pipe[1], &dummy, 1);
}

static void do_reboot(void *user_data, char *reason)
{
	reboot_crash(li.program, li.pid, li.signal, li.pmd);
}

static void do_exit(void *user_data, mama_exit_reason_t reason)
{
	switch (reason) {
		case MAMA_EXIT_REASON_ORDERED:
			cbs.reboot(user_data, "Mama ordered to exit");
			break;
		case MAMA_EXIT_REASON_TIMEOUT:
			cbs.reboot(user_data, "Mama ordered to exit but timed out");
			break;
		case MAMA_EXIT_REASON_COMMAND:
			exit_notify_thread();
			exit(COMMAND_EXIT_CODE);
			break;
	}
}

static void do_not_reboot(void *user_data, char *reason)
{
	TPT_ERROR(STR("reboot callback: turned off, reason: %s, program %s, "
	        "pid %d, signal %d, pmd: %s", reason, li.program, li.pid,
	        li.signal, li.pmd));
	exit_notify_thread();
	exit(1);
}

static void set_info(char *program, char *pmd, pid_t pid, int signal)
{
	llog_write("Program Crash", program, pid, NULL, signal,
	           pmd, getenv("SYS_LF_PID"));

	pthread_mutex_lock(&mutex);

	if (li.program)
		free(li.program);
	li.program = program;

	if (li.pmd)
		free(li.pmd);
	li.pmd = pmd;

	li.pid = pid;
	li.signal = signal;

	pthread_mutex_unlock(&mutex);
}

static int parse_pmd_notice(char *pmd_notice_file, struct pmd *pmd)
{
	int status = -1;
	FILE *f = NULL;

	memset(pmd, 0, sizeof(struct pmd));

	f = fopen(pmd_notice_file, "r");
	if (!f) {
		TPT_INFO(STR("Failed to open %s, cannot handle core dump",
		             pmd_notice_file));
		goto exit;
	}

	/* Exception from strict ANSI C99 to support %ms in fscanf */
	if (__extension__ fscanf(f, "%d %d %ms %d %ld %ms %ms", &pmd->pid,
	                                                        &pmd->ppid,
	                                                        &pmd->program,
	                                                        &pmd->signal,
	                                                        &pmd->crashtime,
	                                                        &pmd->workingdir,
	                                                        &pmd->pmdfile) != 7) {
		TPT_INFO(STR("The file %s is malformed, cannot handle core dump",
		             pmd_notice_file));
		goto exit;
	}

	status = 0;

exit:
	if (status) {
		if (pmd->program) free(pmd->program);
		if (pmd->workingdir) free(pmd->workingdir);
		if (pmd->pmdfile) free(pmd->pmdfile);
	}

	if (f)
		fclose(f);

	return status;
}

static void handle_core_dump(pid_t pid, char *program, int signal)
{
	char *basedir = getenv("PMDGEN_BASEDIR");
	struct pmd pmd;
	char *filename = NULL;
	int num_digits = 0;
	int tmp = pid;
	size_t len;

	/* Keep coverity happy, we will never fail here */
	if (!basedir)
		goto exit;

	for (; tmp != 0; tmp /= 10)
		num_digits++;

	len = strlen(basedir) + num_digits + 1 + sizeof(".pmd");

	filename = malloc(len);
	if (!filename) {
		TPT_INFO("Failed malloc");
		goto exit;
	}

	snprintf(filename, len, "%s/%d.pmd", basedir, pid);

	if (parse_pmd_notice(filename, &pmd))
		goto exit;

	if (pmd.signal != signal) {
		TPT_INFO(STR("mama says %s was killed by signal %d, pmdgen says by %d",
		             program, signal, pmd.signal));
	}

	TPT_INFO(STR("pmd pid: %d ppid: %d signal %d, time %ld, workdir %s, file %s",
	             pmd.pid, pmd.ppid, pmd.signal, pmd.crashtime,
	             pmd.workingdir, pmd.pmdfile));

	set_info(program, pmd.pmdfile, pid, signal);
	free(pmd.workingdir);
	free(pmd.program);

exit:
	if (filename)
		free(filename);
}

static void do_child_exit(void *user_data, char *name, pid_t pid, int status)
{
	int signal;
	char *program = basename(name);
	if (!program)
		program = name;
	program = strdup(program);

	if (WIFEXITED(status)) {
		TPT_ERROR(STR("exit callback: child %s (pid %d) exited with status %d",
		        program, pid, WEXITSTATUS(status)));
		set_info(program, NULL, pid, 0);
	}
	else if (WIFSIGNALED(status)) {
		signal = WTERMSIG(status);
		TPT_ERROR(STR("exit callback: child %s (pid %d) died of signal %d",
		        program, pid, signal));

		if (WCOREDUMP(status)) {
			TPT_ERROR(STR("exit callback: child %s (pid %d) generated core dump",
			        program, pid));
			handle_core_dump(pid, program, signal);
		}
		else {
			set_info(program, NULL, pid, signal);
		}
	}
	else {
		TPT_ERROR(STR("exit callback: unexpected status %d, doing best effort",
		              status));
		set_info(program, NULL, pid, 0);
	}
}

static struct mama_callbacks cbs = {
	NULL,
	do_child_exit,
	NULL,
	NULL,
	do_reboot,
	do_exit
};

static void reboot_with_info(char *program, char *pmd, pid_t pid,
                             int signal, char *reason)
{
	pthread_mutex_lock(&mutex);

	set_info(program, pmd, pid, signal);
	cbs.reboot(NULL, reason);

	pthread_mutex_unlock(&mutex);
}

static void mama_fail(char *reason)
{
	reboot_with_info("mamad", NULL, getpid(), 0, reason);
}

#define BUF_LEN (sizeof(struct inotify_event) + NAME_MAX + 1)

static int read_notifications(int fd)
{
	char *basedir = getenv("PMDGEN_BASEDIR");
	char buf[BUF_LEN] __attribute__ ((aligned(8)));
	struct inotify_event *e;
	struct pmd pmd;
	ssize_t num_bytes;
	uint32_t i = 0;
	char *ptr = buf;
	int status = -1;

	/* Keep coverity happy, we will never fail here */
	if (!basedir)
		goto exit;

	num_bytes = read(fd, buf, BUF_LEN);
	if (num_bytes == -1)
		goto exit;

	while (i < num_bytes) {
		e = (struct inotify_event *) ptr;
		if ( (e->mask & IN_CLOSE_WRITE) && !(e->mask & IN_ISDIR) ) {
			size_t len = strlen(basedir) + 1 + e->len + 1;
			char *filename = malloc(len);
			if (!filename)
				goto exit;
			sprintf(filename, "%s/%s", basedir, e->name);

			if (parse_pmd_notice(filename, &pmd) == 0) {
				if (pmd.ppid != getpid()) {
					/* The process which died was not managed by mama, reboot */
					reboot_with_info(pmd.program, pmd.pmdfile, pmd.pid, pmd.signal,
				                 "Process not managed by mama crashed");
				}

				TPT_INFO(STR("Process %d owned by mama and will be handled",
				             pmd.pid));
				/* The process is owned by mama and will be handled */
				free(pmd.program);
				free(pmd.pmdfile);
				free(pmd.workingdir);
			}
			free(filename);
		}

		ptr += (sizeof(struct inotify_event) + e->len);
		i += (sizeof(struct inotify_event) + e->len);
	}

	status = 0;

exit:
	return status;
}

static void * notify_thread_fxn(void *arg)
{
	int fd = (int) arg;
	int fd_max;
	fd_set set;
	sigset_t mask;
	bool quit = false;

	/*
	 * Since libmama relies on these signals, other threads need to
	 * block them.
	 */
	sigemptyset(&mask);
	sigaddset(&mask, SIGSYNC);
	sigaddset(&mask, SIGALIVE);
	sigaddset(&mask, SIGCHLD);
	sigaddset(&mask, SIGTERM);

	/* block these signals and use signalfd instead */
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1)
		mama_fail("pthread_sigmask failed");

	while (!quit) {
		FD_ZERO(&set);
		FD_SET(fd, &set);
		fd_max = fd;
		FD_SET(notify_pipe[0], &set);
		if (notify_pipe[0] > fd)
			fd_max = notify_pipe[0];

		int ret = select(fd_max + 1, &set, NULL, NULL, NULL);
		if (ret == -1)
			mama_fail("Select failed in notify thread");

		if (FD_ISSET(fd, &set)) {
			if (read_notifications(fd)) {
				mama_fail("Failed to read pmd notification");
				break; /* Keep coverity happy, we will never come here */
			}
		}

		if (FD_ISSET(notify_pipe[0], &set))
			quit = true;
	}

	return NULL;
}

static void usage(void)
{
	printf("Usage: mamad [options] <config file>\n\n"
	       "Options:\n"
	       "     -h | --help          Print usage information (this message)\n"
	       "          --no-reboot     Turns off the reboot functionality for\n"
	       "                          debugging purposes. Instead the daemon\n"
	       "                          will log and exit.\n"
	       "          --no-unmanaged  Don't monitor crashes from unmanaged\n"
	       "                          processes.\n"
	       "          --no-guard      Don't spawn a guard for mamad\n"
	       "\n");
}

enum {
	ARGID_HELP = 256,
	ARGID_NO_REBOOT,
	ARGID_NO_UNMANAGED,
	ARGID_NO_GUARD
};

int main(int argc, char *argv[])
{
	static char short_options[] = "h";
	static struct option long_options[] = {
		{ "help",         no_argument,       NULL, ARGID_HELP },
		{ "no-reboot",    no_argument,       NULL, ARGID_NO_REBOOT },
		{ "no-unmanaged", no_argument,       NULL, ARGID_NO_UNMANAGED },
		{ "no-guard",     no_argument,       NULL, ARGID_NO_GUARD },
		{ 0, 0, 0, 0 }
	};
	int argid, index;
	bool guard = true;
	bool unmanaged = true;
	pthread_t notify_thread;
	char *basedir;

	for (;;) {
		argid = getopt_long(argc, argv, short_options, long_options, &index);

		if (argid == -1)
			break;

		switch (argid) {
			case ARGID_HELP:
			case 'h':
				usage();
				exit(EXIT_SUCCESS);

			case ARGID_NO_REBOOT:
				cbs.reboot = do_not_reboot;
				break;

			case ARGID_NO_UNMANAGED:
				unmanaged = false;
				break;

			case ARGID_NO_GUARD:
				guard = false;
				break;

			default:
				usage();
				mama_fail("Wrong parameter to mamad");
		}
	}

	if (argc - optind != 1) {
		usage();
		mama_fail("No config file supplied to mamad");
	}

	basedir = getenv("PMDGEN_BASEDIR");
	if (!basedir)
		mama_fail("Mamad: PMDGEN_BASEDIR environment variable not set");

	if (guard) {
		pid_t mamad_pid = fork();

		if (mamad_pid == -1) {
			mama_fail("Mamad failed fork");
		}
		else if (mamad_pid != 0) {
			int arglen = strlen(argv[0]);
			int status;

			/* Modify the apperance in 'ps' as to not confuse */
			strncpy(argv[0], "mamaguard", arglen);
			for (int i = 1; i < argc; i++) {
				arglen = strlen(argv[i]);
				memset(argv[i], ' ', arglen);
			}

			/* Wait for mama to exit, which it should never do */
			pid_t pid = waitpid(mamad_pid, &status, 0);

			if (pid != mamad_pid) {
				TPT_ERROR(STR("Strange child pid exited %d, expected %d\n",
				        pid, mamad_pid));
				mama_fail("Mamad exited");
			}

			if (WIFEXITED(status) && WEXITSTATUS(status) == COMMAND_EXIT_CODE) {
				TPT_INFO("mamad was ordered by a command to exit, ignoring");
				exit(0);
			}
			else {
				TPT_ERROR(STR("Mamad exited with status %d\n", status));
				mama_fail("Mamad exited");
			}
		}
	}

	if (unmanaged) {
		if (pipe(notify_pipe)) {
			TPT_ERROR(STR("Failed to create pipe (%s)", strerror(errno)));
			mama_fail("Mamad failed to create pipe");
		}

		int fd = inotify_init();
		if (fd == -1) {
			TPT_ERROR("Failed to initialize inotify");
			mama_fail("Mamad failed to initialize inotify");
		}

		if (inotify_add_watch(fd, basedir, IN_CLOSE_WRITE) == -1) {
			TPT_ERROR(STR("Failed to add watch on directory %s", basedir));
			mama_fail("Mamad internal error");
		}

		/* Spawn the thread which monitors the pmd directory */
		if (pthread_create(&notify_thread, NULL, notify_thread_fxn, (void *)fd)) {
			TPT_ERROR("Failed to create notify thread");
			mama_fail("Mamad failed to create notify thread");
		}
	}
        /* Remove the LD_RPELOAD setting on mamad.sh script. Not needed for child processes*/
        unsetenv("LD_PRELOAD");
	mama_run(argv[argc-1], &cbs, NULL);
}

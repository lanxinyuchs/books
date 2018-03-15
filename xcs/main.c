#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <libgen.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "libmama.h"
#include "mama.h"
#include "test.h"

#define FUZZ_MS 100
#define MAX_CMD_SIZE 1024

static int test_status;

static inline int timestamp(struct timespec *ts)
{
	if (clock_gettime(CLOCK_MONOTONIC, ts) < 0) {
		fprintf(stderr, "Failed clock_gettime()\n");
		return -1;
	}

	return 0;
}

static int timespec_diff(const struct timespec *start,
                         const struct timespec *end)
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

static int check_delta(struct state *state)
{
	struct timespec now;
	int diff_ms;
	struct stage *s = &state->stages[state->stage];

	if (timestamp(&now) < 0)
		return -1;

	diff_ms = timespec_diff(&state->ts, &now);

	if (diff_ms < (s->delta_ms - FUZZ_MS)) {
		printf("Error: diff %dms is lower than minimum allowed %dms\n",
		       diff_ms, s->delta_ms - FUZZ_MS);
		return -1;
	}
	else if (diff_ms > (s->delta_ms + FUZZ_MS)) {
		printf("Error: diff %dms is higher than maximum allowed %dms\n",
		       diff_ms, s->delta_ms + FUZZ_MS);
		return -1;
	}

	return 0;
}

static void check_stage(struct state *state, stage_type_t stage_type, union arg *a)
{
	struct stage *s = &state->stages[state->stage];

	if (s->type != stage_type) {
		printf("Error: Expected stage %d, got %d\n", s->type, stage_type);
		test_status = -1;
	}

	switch (s->type) {
		case stage_type_child_launch:
		case stage_type_child_exit:
		case stage_type_domain_start:
		case stage_type_domain_stop:
			if (strcmp(s->a.name, a->name) != 0) {
				printf("Error: Expected name %s, got %s\n", s->a.name, a->name);
				test_status = -1;
			}
			break;
		case stage_type_exit:
			if (s->a.reason != a->reason) {
				printf("Error: Expected exit reason %d, got %d\n", s->a.reason,
				       a->reason);
				test_status = -1;
			}
			break;
		case stage_type_reboot:
			break;
	}

	if (check_delta(state) < 0)
		test_status = -1;
}

void do_reboot(void *arg, char *reason)
{
	struct state *s = (struct state *) arg;
	union arg a; /* Keep coverity happy */
	printf("reboot with reason: %s\n", reason);
	check_stage(s, stage_type_reboot, &a);
	s->stage++;
	fflush(stdout);
	exit(test_status);
}

void do_exit(void *arg, mama_exit_reason_t reason)
{
	struct state *s = (struct state *) arg;
	union arg a;
	printf("exiting with reason %d\n", reason);
	a.reason = reason;
	check_stage(s, stage_type_exit, &a);
	s->stage++;
	fflush(stdout);
	exit(test_status);
}

void do_child_exit(void *arg, char *name, pid_t pid, int status)
{
	struct state *s = (struct state *) arg;
	union arg a;

	if (WIFEXITED(status)) {
		printf("exit callback: child %s (pid %d) exited with status %d\n",
		        basename(name), pid, WEXITSTATUS(status));
	}
	else if (WIFSIGNALED(status)) {
		printf("exit callback: child %s (pid %d) died of signal %d\n",
		        basename(name), pid, WTERMSIG(status));

		if (WCOREDUMP(status)) {
			printf("exit callback: child %s (pid %d) generated a core dump\n",
			        basename(name), pid);
		}
	}

	a.name = basename(name);
	check_stage(s, stage_type_child_exit, &a);
	if (status != s->stages[s->stage].status) {
		printf("Error: Expected exit status %d, got %d\n",
		       s->stages[s->stage].status, status);
		test_status = -1;
	}

	s->stage++;
}

void do_child_launch(void *arg, char *name, pid_t pid)
{
	struct state *s = (struct state *) arg;
	union arg a;
	printf("Launched %s with pid %d\n", basename(name), pid);
	a.name = basename(name);
	check_stage(s, stage_type_child_launch, &a);
	s->stage++;
}

void do_domain_start(void *arg, char *name)
{
	struct state *s = (struct state *) arg;
	union arg a;
	printf("Started domain %s\n", name);
	a.name = name;
	check_stage(s, stage_type_domain_start, &a);
	s->stage++;
}

void do_domain_stop(void *arg, char *name)
{
	struct state *s = (struct state *) arg;
	union arg a;
	printf("Stopped domain %s\n", name);
	a.name = name;
	check_stage(s, stage_type_domain_stop, &a);
	s->stage++;
}

void child_fxn(struct state *st, char *config_file)
{
	struct mama_callbacks cbs = {
		do_child_launch,
		do_child_exit,
		do_domain_start,
		do_domain_stop,
		do_reboot,
		do_exit
	};

	timestamp(&st->ts);

	mama_run(config_file, &cbs, st);
}

void usage()
{
	printf("Usage: mama-test -tc <test case nbr> [config dir] [test app dir]\n\n"
	       "    Where <test case nbr> is one of:\n"
	       "    1:  Test child quit\n"
	       "    2:  Test alive timeout\n"
	       "    3:  Test a failed boot\n"
	       "    4:  Test sync timeout\n"
	       "    5:  Test command completion\n"
	       "    6:  Test command completion with error code\n"
	       "    7:  Test command completion with crash\n"
	       "    8:  Test crashing child\n"
	       "    9:  Test escalation of a hung child\n"
	       "    10: Test a failed mama shutdown\n"
	       "    11: Test a mama shutdown which succeeds\n"
	       "    12: Test a slow domain restart which gets aborted\n"
	       "    13: Test a slow domain restart which gets killed\n"
	       "    14: Test domains\n"
	       "    15: Test command pipe\n"
	       "    16: Test cpu core affinity (requires 2 cores)\n"
	       "    17: Test domain shutdown\n"
	       "    18: Test shutting down mama\n"
	       "    19: Test domain shutdown using command\n"
	       "    20: Test mama shutdown using command\n"
	       "\n"
	       "    The [config dir] points to where the test .cfg files are,\n"
	       "    the [test app dir] points to where the test-child app is.\n"
	       "    Both these directory parameters default to $PWD\n"
	       "\n");
}

int main(int argc, char *argv[])
{
	struct test *test;
	char *config_file;
	char cmd[MAX_CMD_SIZE];
	char *config_dir = NULL;
	char *test_dir = NULL;
	parent_fxn pfxn = NULL;
	int retval = 0;
	int fxnval = 0;
	pid_t pid;

	if (argc < 3 || strcmp(argv[1], "-tc") != 0) {
		usage();
		return -1;
	}

	if (argc > 3)
		config_dir = argv[3];
	else
		config_dir = getenv("PWD");

	if (argc > 4)
		test_dir = argv[4];
	else
		test_dir = getenv("PWD");

	if (!test_dir || !config_dir) {
		printf("Failed to get PWD\n");
		return -2;
	}

	switch (atoi(argv[2])) {
		case 1:
			test = &test_quit;
			break;
		case 2:
			test = &test_alive_timeout;
			break;
		case 3:
			test = &test_fail_boot_launch;
			break;
		case 4:
			test = &test_sync_timeout;
			break;
		case 5:
			test = &test_completion;
			break;
		case 6:
			test = &test_completion_code;
			break;
		case 7:
			test = &test_completion_crash;
			break;
		case 8:
			test = &test_crash;
			break;
		case 9:
			test = &test_escalation_killed;
			break;
		case 10:
			test = &test_shutdown_fail;
			break;
		case 11:
			test = &test_shutdown_success;
			break;
		case 12:
			test = &test_slow_domain_aborted;
			break;
		case 13:
			test = &test_slow_domain_killed;
			break;
		case 14:
			test = &test_domain;
			break;
		case 15:
			test = &test_cmd;
			pfxn = cmd_test_fxn;
			break;
		case 16:
			test = &test_affinity;
			pfxn = affinity_test_fxn;
			break;
		case 17:
			test = &test_domain_shutdown;
			break;
		case 18:
			test = &test_shutdown;
			break;
		case 19:
			test = &test_domain_shutdown_cmd;
			pfxn = domain_shutdown_cmd_test_fxn;
			break;
		case 20:
			test = &test_shutdown_cmd;
			pfxn = shutdown_cmd_test_fxn;
			break;
		default:
			printf("Unknown test case number %s\n", argv[2]);
			usage();
			return -2;
	}

	config_file = tmpnam(NULL);
	if (!config_file) {
		printf("Failed to create a temporary file name\n");
		return -2;
	}

	snprintf(cmd, MAX_CMD_SIZE, "sed s=__TEST_DIR__=%s= %s/%s > %s",
	         test_dir, config_dir, test->config_file, config_file);

	if (system(cmd) != 0) {
		printf("Failed to sed the test config file\n");
		return -2;
	}

	printf("Running test config file: %s\n", test->config_file);

	pid = fork();

	if (pid == -1) {
		printf("Failed fork\n");
		return -2;
	}

	if (pid == 0)
		child_fxn(&test->st, config_file);

	if (pfxn)
		fxnval = (*pfxn)(test_dir);

	wait(&retval);

	if (unlink(config_file)) {
		printf("Failed to delete temporary config file %s\n", config_file);
		return -1;
	}

	if (retval || fxnval)
		return -1;

	return 0;
}

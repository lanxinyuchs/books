#include <stdio.h>
#include <limits.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <libgen.h>
#include <inttypes.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "test.h"
#include "mamacmd.h"

int find_env(pid_t pid, char *var, char *val)
{
	FILE *f = NULL;
	char filename[PATH_MAX];
	int status = -1;

	if (sprintf(filename, "/proc/%d/environ", pid) < 0) {
		printf("Failed sprintf\n");
		goto exit;
	}

	f = fopen(filename, "r");
	if (!f) {
		printf("Failed fopen %s\n", filename);
		goto exit;
	}

	do {
		char *line = NULL;
		char *myval;
		size_t sz;

		if (getdelim(&line, &sz, 0, f) < 0)
			break;

		myval = strstr(line, "=");
		if (!myval) {
			free(line);
			goto exit;
		}
		*myval++ = '\0';

		if (strcmp(var, line) == 0) {
			if (strcmp(myval, val) == 0) {
				status = 0;
				free(line);
				break;
			}
			else {
				/* environment variable found, but with other value */
				free(line);
				break;
			}
		}
		free(line);
	} while (!feof(f));

exit:
	if (f) (void) fclose(f);
	return status;
}


static bool check_entry(struct mamacmd_status_entry *e, mamacmd_id_t id,
                        char *cmd, char *domain, bool child_stopped,
                        bool domain_stopped)
{
	int status = 0;

	if (id != e->id) {
		printf("Expected id %" PRIu64 " got %" PRIu64 "\n", id, e->id);
		status = -1;
	}

	if (cmd && strcmp(basename(cmd), basename(e->cmd)) != 0) {
		printf("Expected cmd %s got %s\n", cmd, e->cmd);
		status = -1;
	}

	if (domain && strcmp(domain, e->domain) != 0) {
		printf("Expected domain %s got %s\n", domain, e->domain);
		status = -1;
	}

	if (child_stopped != e->cmdstopped) {
		printf("Expected child stopped to be %d got %d\n", child_stopped,
		       e->cmdstopped);
		status = -1;
	}

	if (domain_stopped != e->domain_cmdstopped) {
		printf("Expected domain stopped to be %d got %d\n", domain_stopped,
		       e->domain_cmdstopped);
		status = -1;
	}

	return status;
}

static pid_t get_pid(char *cmd, struct mamacmd_status_entry *e, int n)
{
	for (int i = 0; i < n; i++) {
		if (strcmp(cmd, basename(e[i].cmd)) == 0)
			return e[i].pid;
	}

	return 0;
}

static int get_child_cpu(pid_t pid)
{
	char *filename = NULL;
	FILE *f = NULL;
	int num_digits = 0;
	int p = pid;
	size_t size;
	int status = -1;
	int cpu;

	while (p) {
		p /= 10;
		num_digits++;
	}

	size = strlen("/proc/") + num_digits + strlen("/stat");
	filename = malloc(size);
	if (!filename)
		goto exit;
	sprintf(filename, "/proc/%d/stat", pid);

	f = fopen(filename, "r");
	if (!f) {
		printf("Failed to open file %s for reading\n", filename);
		goto exit;
	}

	if (fscanf(f, "%*d %*s %*s %*d %*d %*d %*d %*d %*d %*d %*d %*d %*d %*d"
	              "%*d %*d %*d %*d %*d %*d %*d %*d %*d %*d %*d %*d %*d %*d"
	              "%*d %*d %*d %*d %*d %*d %*d %*d %*d %*d %u", &cpu) != 1) {
		printf("Failed to get last used cpu for %d\n", pid);
		goto exit;
	}

	status = cpu;

exit:
	if (filename) free(filename);
	if (f && fclose(f) != 0)
		printf("Failed to close fp\n");

	return status;
}

#define TEST_SUCCESS 0
#define TEST_FAILURE -1

#define EXPECT(x) if (!(x)) do {                                           \
    printf("Expected '" #x "' to be true at %s:%d\n", __FILE__, __LINE__); \
    result = TEST_FAILURE;                                                 \
    goto exit;                                                             \
} while (0)

#define ASIZE(x) (sizeof(x) / sizeof((x)[0]))

int cmd_test_fxn(char *test_dir)
{
	struct mamacmd_status_entry *entries = NULL;
	mamacmd_handle h = NULL;
	int result = TEST_SUCCESS;
	int num;
	mamacmd_id_t id;
	char * const args[] = {
		"--lifetime-ms",
		"4000",
	};
	char * argv[ASIZE(args) + 1 + 1];
	struct mamacmd_launch_config cfg = {
		sizeof(struct mamacmd_launch_config),
		0,
		0,
		0,
		0,
		0,
		1,
		0,
	};

	argv[0] = malloc(strlen(test_dir) + sizeof("1"));
	sprintf(argv[0], "%s/%s", test_dir, "1");
	for (int i = 0; i < ASIZE(args); i++)
		argv[i+1] = args[i];
	argv[ASIZE(args) + 1] = 0;

	sleep(1);

	h = mamacmd_connect();
	EXPECT(h);

	num = mamacmd_status(h, &entries);
	EXPECT(num >= 0);
	free(entries); entries = NULL;
	EXPECT(num == 3);

	sleep(1);

	EXPECT(mamacmd_child_stop(h, 0) == 0);

	num = mamacmd_status(h, &entries);
	EXPECT(num >= 0);

	EXPECT(check_entry(&entries[0], 0, NULL, NULL, true, false) == 0);
	free(entries); entries = NULL;

	sleep(1);

	EXPECT(mamacmd_child_start(h, 0) == 0);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num >= 0);

	EXPECT(check_entry(&entries[0], 0, NULL, NULL, false, false) == 0);
	free(entries); entries = NULL;

	sleep(1);

	EXPECT(mamacmd_launch(h, argv, NULL, "mydomain", &cfg, &id) == 0);
	EXPECT(id == 3u);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num >= 0);
	free(entries); entries = NULL;
	EXPECT(num == 4);

exit:
	if (entries) free(entries);

	if (h && mamacmd_disconnect(h))
		result = TEST_FAILURE;

	return result;
}

int affinity_test_fxn(char *test_dir)
{
	mamacmd_handle h = NULL;
	int result = TEST_SUCCESS;
	struct mamacmd_status_entry *entries = NULL;
	pid_t pid;
	int num;
	int cpu;

	sleep(1);

	h = mamacmd_connect();
	EXPECT(h);

	num = mamacmd_status(h, &entries);
	EXPECT(num >= 0);

	pid = get_pid("1", entries, num);
	EXPECT(pid > 0);
	cpu = get_child_cpu(pid);
	EXPECT(cpu == 0);

	pid = get_pid("2", entries, num);
	EXPECT(pid > 0);
	cpu = get_child_cpu(pid);
	EXPECT(cpu == 1);

exit:
	if (entries) free(entries);

	if (h && mamacmd_disconnect(h))
		result = TEST_FAILURE;

	return result;
}

int domain_shutdown_cmd_test_fxn(char *test_dir)
{
	mamacmd_handle h = NULL;
	int result = TEST_SUCCESS;
	struct mamacmd_status_entry *entries = NULL;
	int num;

	sleep(1);

	h = mamacmd_connect();
	EXPECT(h);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 4);

	for (int i = 0; i < 4; i++) {
		EXPECT(check_entry(&entries[i], i, NULL, "test_domain", false, false) == 0);
	}
	free(entries); entries = NULL;

	EXPECT(mamacmd_domain_stop(h, "test_domain") == 0);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 4);

	for (int i = 0; i < 4; i++) {
		EXPECT(check_entry(&entries[i], i, NULL, "test_domain", false, true) == 0);
	}
	free(entries); entries = NULL;

	sleep(1);

	EXPECT(mamacmd_domain_start(h, "test_domain") == 0);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 4);

	for (int i = 0; i < 4; i++) {
		EXPECT(check_entry(&entries[i], i, NULL, "test_domain", false, false) == 0);
	}

exit:
	if (entries) free(entries);

	if (h && mamacmd_disconnect(h))
		result = TEST_FAILURE;

	return result;
}

int shutdown_cmd_test_fxn(char *test_dir)
{
	mamacmd_handle h = NULL;
	int result = TEST_SUCCESS;

	sleep(1);

	h = mamacmd_connect();
	EXPECT(h);

	EXPECT(mamacmd_shutdown(h) == 0);

exit:
	if (h && mamacmd_disconnect(h))
		result = TEST_FAILURE;

	return result;
}

static int read_int_from_file(int *val, char *file)
{
	int status = -1;
	FILE *fp = NULL;

	fp = fopen(file, "r");
	if (fp == NULL) {
		printf("Failed to open %s for reading\n", file);
		goto exit;
	}

	if (fscanf(fp, "%d", val) != 1) {
		printf("Failed to read integer from file %s\n", file);
		goto exit;
	}

	status = 0;

exit:
	if (fp && fclose(fp) != 0)
		printf("Failed to close fp\n");

	return status;
}

int rtsched_test_fxn(char *test_dir)
{
	int result = -1;
	int val = 0;

	sleep(1);

	EXPECT(read_int_from_file(&val, "/proc/sys/kernel/sched_rt_period_us") == 0);
	EXPECT(val == 20000);

	EXPECT(read_int_from_file(&val, "/proc/sys/kernel/sched_rt_runtime_us") == 0);
	EXPECT(val == 10000);

	result = 0;

exit:
	return result;
}

int rtsched_reset_test_fxn(char *test_dir)
{
	int result = -1;
	int val = 0;

	sleep(1);

	EXPECT(read_int_from_file(&val, "/proc/sys/kernel/sched_rt_period_us") == 0);
	EXPECT(val == 1000000);

	EXPECT(read_int_from_file(&val, "/proc/sys/kernel/sched_rt_runtime_us") == 0);
	EXPECT(val == 950000);

	result = 0;

exit:
	return result;
}

int terminate_test_fxn(char *test_dir)
{
	mamacmd_handle h = NULL;
	struct mamacmd_status_entry *entries = NULL;
	int result = -1;
	int num;

	sleep(2);
	h = mamacmd_connect();
	EXPECT(h);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 3);
	free(entries); entries = NULL;

	EXPECT(mamacmd_terminate(h, 0) == 0);
	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 2);
	EXPECT(check_entry(&entries[0], 1, "test/test-child/2", NULL, false, false) == 0);
	EXPECT(check_entry(&entries[1], 2, "test/test-child/3", NULL, false, false) == 0);
	free(entries); entries = NULL;

	EXPECT(mamacmd_child_stop(h, 2) == 0);
	EXPECT(mamacmd_terminate(h, 2) == 0);
	num = mamacmd_status(h, &entries);
	EXPECT(num == 1);
	EXPECT(check_entry(&entries[0], 1, "test/test-child/2", NULL, false, false) == 0);

	result = 0;

exit:
	if (entries) free(entries);

	if (h && mamacmd_disconnect(h))
		result = TEST_FAILURE;

	return result;
}

int env_test_fxn(char *test_dir)
{
	struct mamacmd_status_entry *entries = NULL;
	mamacmd_handle h = NULL;
	mamacmd_id_t id;
	int result = -1;
	int num;
	pid_t pid;
	char * env[] = {
	  "trouble", "shoot this!",
	  "made_up", "variable",
	  "3bad", "^apples",
	  NULL
	};
	char * const args[] = {
		"--lifetime-ms",
		"-1",
	};
	char * argv[ASIZE(args) + 1 + 1];
	struct mamacmd_launch_config cfg = {
		sizeof(struct mamacmd_launch_config),
		0,
		0,
		0,
		0,
		0,
		1,
		0,
	};


	sleep(1);
	h = mamacmd_connect();
	EXPECT(h);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 2);

	pid = entries[0].pid;
	EXPECT(find_env(pid, "GBLVAR", "GBLVAL") == 0);
	EXPECT(find_env(pid, "VAR", "VAL") == 0);
	EXPECT(find_env(pid, "FOO", "BAR") == 0);

	pid = entries[1].pid;
	EXPECT(find_env(pid, "GBLVAR", "GBLVAL") == 0);
	EXPECT(find_env(pid, "VAR", "VAL") != 0);
	EXPECT(find_env(pid, "FOO", "BAR") != 0);

	free(entries); entries = NULL;

	argv[0] = malloc(strlen(test_dir) + sizeof("3"));
	sprintf(argv[0], "%s/%s", test_dir, "3");
	for (int i = 0; i < ASIZE(args); i++)
		argv[i+1] = args[i];
	argv[ASIZE(args) + 1] = 0;

	EXPECT(mamacmd_launch(h, argv, env, NULL, &cfg, &id) == 0);
	EXPECT(id == 2u);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 3);

	pid = entries[2].pid;
	EXPECT(find_env(pid, "GBLVAR", "GBLVAL") == 0);
	EXPECT(find_env(pid, "VAR", "OTHER VAL") == 0);
	EXPECT(find_env(pid, env[0], env[1]) == 0);
	EXPECT(find_env(pid, env[2], env[3]) == 0);
	EXPECT(find_env(pid, env[4], env[5]) == 0);

	result = 0;

exit:
	if (entries) free(entries);

	if (h && mamacmd_disconnect(h))
		result = TEST_FAILURE;

	return result;
}

int multiple_connections_test_fxn(char *test_dir)
{
	mamacmd_handle handles[3];
	int result = -1;

	sleep(1);

	handles[0] = mamacmd_connect();
	EXPECT(handles[0]);

	sleep(1);

	handles[1] = mamacmd_connect();
	EXPECT(handles[0]);

	EXPECT(mamacmd_child_stop(handles[0], 0) == 0);

	handles[2] = mamacmd_connect();
	EXPECT(handles[0]);

	EXPECT(mamacmd_child_stop(handles[1], 1) == 0);

	EXPECT(mamacmd_connect() == NULL);

	EXPECT(mamacmd_child_stop(handles[2], 2) == 0);

	result = 0;

exit:
	for (int i = 0; i < 3; i++) {
		if (handles[i] && mamacmd_disconnect(handles[i]))
			result = TEST_FAILURE;
	}

	return result;
}

int persistent_id_test_fxn(char *test_dir)
{
	mamacmd_handle h = NULL;
	struct mamacmd_status_entry *entries = NULL;
	int result = -1;
	int num;

	sleep(1);
	h = mamacmd_connect();
	EXPECT(h);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 4);
	free(entries); entries = NULL;

	EXPECT(mamacmd_terminate(h, 0) == 0);
	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 3);
	EXPECT(check_entry(&entries[0], 1, "test/test-child/2", NULL, false, false) == 0);
	EXPECT(check_entry(&entries[1], 2, "test/test-child/3", NULL, false, false) == 0);
	EXPECT(check_entry(&entries[2], 3, "test/test-child/4", NULL, false, false) == 0);
	free(entries); entries = NULL;

	EXPECT(mamacmd_terminate(h, 2) == 0);
	num = mamacmd_status(h, &entries);
	EXPECT(num == 2);
	EXPECT(check_entry(&entries[0], 1, "test/test-child/2", NULL, false, false) == 0);
	EXPECT(check_entry(&entries[1], 3, "test/test-child/4", NULL, false, false) == 0);

	result = 0;

exit:
	if (entries) free(entries);

	if (h && mamacmd_disconnect(h))
		result = TEST_FAILURE;

	return result;
}

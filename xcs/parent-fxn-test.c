#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <libgen.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "test.h"
#include "mamacmd.h"

static bool check_entry(struct mamacmd_status_entry *e, char *cmd, char *domain,
                        bool child_stopped, bool domain_stopped)
{
	int status = 0;

	if (cmd && strcmp(cmd, e->cmd) != 0) {
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
	if (f) fclose(f);

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
#define EXEC "1"

int cmd_test_fxn(char *test_dir)
{
	struct mamacmd_status_entry *entries = NULL;
	mamacmd_handle h = NULL;
	int result = TEST_SUCCESS;
	int num;
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
	};

	argv[0] = malloc(strlen(test_dir) + sizeof(EXEC));
	sprintf(argv[0], "%s/%s", test_dir, EXEC);
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

	EXPECT(check_entry(&entries[0], NULL, NULL, true, false) == 0);
	free(entries); entries = NULL;

	sleep(1);

	EXPECT(mamacmd_child_start(h, 0) == 0);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num >= 0);

	EXPECT(check_entry(&entries[0], NULL, NULL, false, false) == 0);
	free(entries); entries = NULL;

	sleep(1);

	EXPECT(mamacmd_launch(h, argv, "mydomain", &cfg) == 0);

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
		EXPECT(check_entry(&entries[i], NULL, "test_domain", false, false) == 0);
	}
	free(entries); entries = NULL;

	EXPECT(mamacmd_domain_stop(h, "test_domain") == 0);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 4);

	for (int i = 0; i < 4; i++) {
		EXPECT(check_entry(&entries[i], NULL, "test_domain", false, true) == 0);
	}
	free(entries); entries = NULL;

	sleep(1);

	EXPECT(mamacmd_domain_start(h, "test_domain") == 0);

	sleep(1);

	num = mamacmd_status(h, &entries);
	EXPECT(num == 4);

	for (int i = 0; i < 4; i++) {
		EXPECT(check_entry(&entries[i], NULL, "test_domain", false, false) == 0);
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

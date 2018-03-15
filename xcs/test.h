#ifndef __TEST_H
#define __TEST_H

#include <libmama.h>
#include <time.h>

#define MAX_STAGES 100

typedef enum {
	stage_type_child_launch = 0,
	stage_type_child_exit,
	stage_type_domain_start,
	stage_type_domain_stop,
	stage_type_reboot,
	stage_type_exit
} stage_type_t;

union arg {
	char *name;
	mama_exit_reason_t reason;
};

struct stage {
	stage_type_t type;
	union arg a;
	int delta_ms;
	int status;
};

struct state {
	int stage;
	struct timespec ts;
	struct stage stages[MAX_STAGES];
};

struct test {
	char *config_file;
	struct state st;
};

extern struct test test_domain;
extern struct test test_alive_timeout;
extern struct test test_quit;
extern struct test test_fail_boot_launch;
extern struct test test_shutdown_fail;
extern struct test test_shutdown_success;
extern struct test test_slow_domain_aborted;
extern struct test test_slow_domain_killed;
extern struct test test_sync_timeout;
extern struct test test_completion;
extern struct test test_completion_code;
extern struct test test_completion_crash;
extern struct test test_crash;
extern struct test test_escalation_killed;
extern struct test test_cmd;
extern struct test test_affinity;
extern struct test test_domain_shutdown;
extern struct test test_shutdown;
extern struct test test_domain_shutdown_cmd;
extern struct test test_shutdown_cmd;

typedef int (*parent_fxn)(char *test_dir);

extern int cmd_test_fxn(char *test_dir);
extern int affinity_test_fxn(char *test_dir);
extern int domain_shutdown_cmd_test_fxn(char *test_dir);
extern int shutdown_cmd_test_fxn(char *test_dir);

#endif /* __TEST_H */

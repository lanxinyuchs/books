#ifndef _MAMACMD_H
#define _MAMACMD_H

#include <unistd.h>
#include <stdbool.h>
#include <sys/types.h>

#define MAMACMD_MAX_ENTRY_STRING_SIZE 100

struct mamacmd_status_entry {
	char cmd[MAMACMD_MAX_ENTRY_STRING_SIZE];
	char domain[MAMACMD_MAX_ENTRY_STRING_SIZE];
	bool cmdstopped;
	bool domain_cmdstopped;
	bool in_domain;
	bool run_until_completion;
	int id;
	pid_t pid;
};

struct mamacmd_launch_config {
	int size;
	int retries;
	int retry_timeout_ms;
	int abort_timeout_ms;
	int alive_timeout_ms;
	int wait_timeout_ms;
	int run_until_completion;
	int affinity;
};

typedef struct mamacmd_instance * mamacmd_handle;

extern mamacmd_handle mamacmd_connect(void);
extern int mamacmd_child_start(mamacmd_handle h, int id);
extern int mamacmd_child_stop(mamacmd_handle h, int id);
extern int mamacmd_domain_start(mamacmd_handle h, char *domain);
extern int mamacmd_domain_stop(mamacmd_handle h, char *domain);
extern int mamacmd_status(mamacmd_handle h,
                          struct mamacmd_status_entry ** entries);
extern int mamacmd_launch(mamacmd_handle h, char * const argv[], char *domain,
                          struct mamacmd_launch_config *cfg);
extern int mamacmd_disconnect(mamacmd_handle h);
extern int mamacmd_shutdown(mamacmd_handle h);

#endif /* _MAMACMD_H */

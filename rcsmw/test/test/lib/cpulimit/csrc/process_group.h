#ifndef __PROCESS_GROUP_H

#define __PROCESS_GROUP_H

#include "process_iterator.h"

#include "list.h"

#define PIDHASH_SZ 1024
#define pid_hashfn(x) ((((x) >> 8) ^ (x)) & (PIDHASH_SZ - 1))

struct process_group
{
	//hashtable with all the processes (array of struct list of struct process)
	struct list *proctable[PIDHASH_SZ];
	struct list *proclist;
	pid_t target_pid;
	int include_children;
	struct timeval last_update;
};

int init_process_group(struct process_group *pgroup, int target_pid, int include_children);

void update_process_group(struct process_group *pgroup);

int close_process_group(struct process_group *pgroup);

int find_process_by_pid(pid_t pid);

int find_process_by_name(const char *process_name);

int remove_process(struct process_group *pgroup, int pid);

#endif

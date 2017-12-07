#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/time.h>
#include <signal.h>

#include <assert.h>

#include "process_iterator.h"
#include "process_group.h"
#include "list.h"

// look for a process by pid
// search_pid   : pid of the wanted process
// return:  pid of the found process, if successful
//          negative pid, if the process does not exist or if the signal fails
int find_process_by_pid(pid_t pid)
{
	return (kill(pid,0)==0) ? pid : -pid;
}

// look for a process with a given name
// process: the name of the wanted process. it can be an absolute path name to the executable file
//         or just the file name
// return:  pid of the found process, if it is found
//         0, if it's not found
//         negative pid, if it is found but it's not possible to control it
int find_process_by_name(const char *process_name)
{
	//whether the variable process_name is the absolute path or not
	int is_absolute_path = process_name[0] == '/';
	//flag indicating if the a process with given name was found
	int found = 0;
	pid_t pid = -1;

	//process iterator
	struct process_iterator it;
	struct process proc;
	struct process_filter filter;
	filter.pid = 0;
	filter.include_children = 0;
	init_process_iterator(&it, &filter);
	while (get_next_process(&it, &proc) != -1)
	{
		pid = proc.pid;
		int size = strlen(proc.command);

		found = 0;
		if (is_absolute_path && strncmp(proc.command, process_name, size)==0 && size==strlen(process_name)) {
			//process found
			found = 1;
		}
		else {
			//process found
			if (strncmp(proc.command + size - strlen(process_name), process_name, strlen(process_name))==0) {
				found = 1;
			}
		}
		if (found==1) {
			if (kill(pid,SIGCONT)==0) {
				//process is ok!
				break;
			}
			else {
				//we don't have permission to send signal to that process
				//so, don't exit from the loop and look for another one with the same name
				found = -1;
			}
		}
	}
	if (close_process_iterator(&it) != 1) exit(1);
	if (found == 1) {
		//ok, the process was found
		return pid;
	}
	else if (found == 0) {
		//no process found
		return 0;
	}
	else if (found == -1) {
		//the process was found, but we haven't permission to control it
		return -pid;
	}
	//this MUST NOT happen
	exit(2);
}

int init_process_group(struct process_group *pgroup, int target_pid, int include_children)
{
	//hashtable initialization
	memset(&pgroup->proctable, 0, sizeof(pgroup->proctable));
	pgroup->target_pid = target_pid;
	pgroup->include_children = include_children;
	pgroup->proclist = (struct list*)malloc(sizeof(struct list));
	init_list(pgroup->proclist, 4);
	memset(&pgroup->last_update, 0, sizeof(pgroup->last_update));
	update_process_group(pgroup);
	return 0;
}

int close_process_group(struct process_group *pgroup)
{
	int i;
	int size = sizeof(pgroup->proctable) / sizeof(struct process*);
	for (i=0; i<size; i++) {
		if (pgroup->proctable[i] != NULL) {
			//free() history for each process
			destroy_list(pgroup->proctable[i]);
			free(pgroup->proctable[i]);
			pgroup->proctable[i] = NULL;
		}
	}
	clear_list(pgroup->proclist);
	free(pgroup->proclist);
	pgroup->proclist = NULL;
	return 0;
}

void remove_terminated_processes(struct process_group *pgroup)
{
	//TODO
}

//return t1-t2 in microseconds (no overflow checks, so better watch out!)
static inline unsigned long timediff(const struct timeval *t1,const struct timeval *t2)
{
	return (t1->tv_sec - t2->tv_sec) * 1000000 + (t1->tv_usec - t2->tv_usec);
}

//parameter in range 0-1
#define ALFA 0.08
#define MIN_DT 20

void update_process_group(struct process_group *pgroup)
{
	struct process_iterator it;
	struct process tmp_process;
	struct process_filter filter;
	struct timeval now;
	gettimeofday(&now, NULL);
	//time elapsed from previous sample (in ms)
	long dt = timediff(&now, &pgroup->last_update) / 1000;
	filter.pid = pgroup->target_pid;
	filter.include_children = pgroup->include_children;
	init_process_iterator(&it, &filter);
	clear_list(pgroup->proclist);
	init_list(pgroup->proclist, 4);

	while (get_next_process(&it, &tmp_process) != -1)
	{
//		struct timeval t;
//		gettimeofday(&t, NULL);
//		printf("T=%ld.%ld PID=%d PPID=%d START=%d CPUTIME=%d\n", t.tv_sec, t.tv_usec, tmp_process.pid, tmp_process.ppid, tmp_process.starttime, tmp_process.cputime);
		int hashkey = pid_hashfn(tmp_process.pid);
		if (pgroup->proctable[hashkey] == NULL)
		{
			//empty bucket
			pgroup->proctable[hashkey] = malloc(sizeof(struct list));
			struct process *new_process = malloc(sizeof(struct process));
			tmp_process.cpu_usage = -1;
			memcpy(new_process, &tmp_process, sizeof(struct process));
			init_list(pgroup->proctable[hashkey], 4);
			add_elem(pgroup->proctable[hashkey], new_process);
			add_elem(pgroup->proclist, new_process);
		}
		else
		{
			//existing bucket
			struct process *p = (struct process*)locate_elem(pgroup->proctable[hashkey], &tmp_process);
			if (p == NULL)
			{
				//process is new. add it
				struct process *new_process = malloc(sizeof(struct process));
				tmp_process.cpu_usage = -1;
				memcpy(new_process, &tmp_process, sizeof(struct process));
				add_elem(pgroup->proctable[hashkey], new_process);
				add_elem(pgroup->proclist, new_process);
			}
			else
			{
				assert(tmp_process.pid == p->pid);
				assert(tmp_process.ppid == p->ppid);
				assert(tmp_process.starttime == p->starttime);
				add_elem(pgroup->proclist, p);
				if (dt < MIN_DT) continue;
				//process exists. update CPU usage
				double sample = 1.0 * (tmp_process.cputime - p->cputime) / dt;
				if (p->cpu_usage == -1) {
					//initialization
					p->cpu_usage = sample;
				}
				else {
					//usage adjustment
					p->cpu_usage = (1.0-ALFA) * p->cpu_usage + ALFA * sample;
				}
				p->cputime = tmp_process.cputime;
			}
		}
	}
	close_process_iterator(&it);
	if (dt < MIN_DT) return;
	pgroup->last_update = now;
}

int remove_process(struct process_group *pgroup, int pid)
{
	int hashkey = pid_hashfn(pid);
	if (pgroup->proctable[hashkey] == NULL) return 1; //nothing to delete
	struct list_node *node = (struct list_node*)locate_node(pgroup->proctable[hashkey], &pid);
	if (node == NULL) return 2;
	delete_node(pgroup->proctable[hashkey], node);
	return 0;
}
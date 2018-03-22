#ifndef __CONFIG_H
#define __CONFIG_H

#include "state.h"

struct config {
	int require_initial_launch_success;
	int exit_handler_timeout_ms;
	int shutdown_timeout_ms;
	int only_managed_children_can_shut_down;
	int sched_rt_period_us;
	int sched_rt_runtime_us;
	int max_mamacmd_connections;
};

extern void config_initialize(struct config *gcfg);
extern int config_parse(char *cfg_file, struct state *s, struct config *cfg);
extern int config_dir_parse(char *cfg_dir, char **tags, struct state *s,
                            struct config *gcfg);

#endif /* __CONFIG_H */

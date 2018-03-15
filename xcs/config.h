#ifndef __CONFIG_H
#define __CONFIG_H

#include "state.h"

struct config {
	int require_initial_launch_success;
	int exit_handler_timeout_ms;
	int shutdown_timeout_ms;
	int only_managed_children_can_shut_down;
};

extern int config_parse(char *cfg_file, struct state *s, struct config *cfg);

#endif /* __CONFIG_H */

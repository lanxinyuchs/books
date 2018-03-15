#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <sys/queue.h>

#include <libconfig.h>

#include "config.h"
#include "state.h"
#include "child.h"
#include "domain.h"
#include "log.h"

static int parse_domains(config_t *cfg, struct state *s)
{
	config_setting_t *domains;
	int cnt;

	domains = config_lookup(cfg, "mama.domains");
	if (!domains) {
		log_trace2("No domains in config file");
		return 0;
	}

	cnt = config_setting_length(domains);

	for (int i = 0; i < cnt; i++) {
		struct domain *d;
		config_setting_t *domain;
		const char *name;
		struct domain_config dcfg;

		domain = config_setting_get_elem(domains, i);
		if (!domain) {
			return -1;
		}

		if (!config_setting_lookup_string(domain, "name", &name))
			return -1;

		memset(&dcfg, 0, sizeof(dcfg));
		config_setting_lookup_int(domain, "retries", &dcfg.retries);
		config_setting_lookup_int(domain, "retry_timeout_ms", &dcfg.retry_timeout_ms);
		config_setting_lookup_int(domain, "abort_timeout_ms", &dcfg.abort_timeout_ms);

		d = domain_create(name, &dcfg);
		if (!d)
			return -1;
		domain_append(d, dglist, s);
	}

	return 0;
}

static void free_argv(char **argv)
{
	int i;
	const char *arg;

	for (i = 0, arg = argv[0]; arg; i++, arg = argv[i])
		free((void *) arg);
	free(argv);
}

static char ** get_argv(const char *cmd, config_setting_t *child)
{
	int status = -1;
	config_setting_t *args_setting;
	char ** argv = NULL;

	args_setting = config_setting_get_member(child, "args");
	if (args_setting) {
		int idx, num_args = config_setting_length(args_setting);

		argv = calloc((num_args + 1 + 1), sizeof(char *));
		if (!argv)
			goto exit;

		for (idx = 0; idx < num_args; idx++) {
			const char *arg = config_setting_get_string_elem(args_setting, idx);
			if (!arg)
				goto exit;

			argv[idx+1] = strdup(arg);
			if (!argv[idx+1])
				goto exit;
		}
		argv[idx+1] = 0;
	}
	else {
		argv = malloc((1 + 1) * sizeof(char *));
		if (!argv)
			goto exit;

		argv[1] = 0;
	}

	argv[0] = strdup(cmd);
	if (!argv[0])
		goto exit;

	status = 0;

exit:
	if (status) {
		if (argv)
			free_argv(argv);
		return NULL;
	}

	return argv;
}

static int parse_children(config_t *cfg, struct state *s)
{
	config_setting_t *cmds;
	int cnt;

	cmds = config_lookup(cfg, "mama.children");
	if (!cmds) {
		log_err("No children in config file");
		return -1;
	}

	cnt = config_setting_length(cmds);

	for (int i = 0; i < cnt; i++) {
		config_setting_t *child;
		const char *cmd;
		char **argv;
		const char *domain;
		struct child *ch;
		struct child_config ccfg;
		struct domain *d = NULL;

		child = config_setting_get_elem(cmds, i);
		if (!child)
			return -1;

		if (!config_setting_lookup_string(child, "command", &cmd))
			return -1;

		/* Retrieve the configuration */
		memset(&ccfg, 0, sizeof(ccfg));
		ccfg.affinity = -1;
		config_setting_lookup_int(child, "retries", &ccfg.retries);
		config_setting_lookup_int(child, "retry_timeout_ms", &ccfg.retry_timeout_ms);
		config_setting_lookup_int(child, "abort_timeout_ms", &ccfg.abort_timeout_ms);
		config_setting_lookup_int(child, "alive_timeout_ms", &ccfg.alive_timeout_ms);
		config_setting_lookup_int(child, "wait_timeout_ms", &ccfg.wait_timeout_ms);
		config_setting_lookup_bool(child, "run_until_completion", &ccfg.run_until_completion);
		config_setting_lookup_int(child, "affinity", &ccfg.affinity);

		/* Parse the command arguments */
		argv = get_argv(cmd, child);
		if (!argv)
			return -1;

		/* Parse the domain name */
		if (config_setting_lookup_string(child, "domain", &domain)) {
			d = domain_get_by_name(domain, s);

			if (!d) {
				log_info("The domain %s was not found", domain);
				free_argv(argv);
				return -1;
			}
		}

		/* Create the child and add it to the global child list */
		ch = child_create(argv, d, &ccfg);
		if (!ch) {
			free_argv(argv);
			return -1;
		}
		child_append(ch, cglist, s);
	}

	return 0;
}

static int parse_global(config_t *cfg, struct config *gcfg)
{
	config_lookup_bool(cfg, "mama.require_initial_launch_success",
	                   &gcfg->require_initial_launch_success);

	config_lookup_int(cfg, "mama.exit_handler_timeout_ms",
	                  &gcfg->exit_handler_timeout_ms);

	config_lookup_int(cfg, "mama.shutdown_timeout_ms",
	                  &gcfg->shutdown_timeout_ms);

	config_lookup_bool(cfg, "mama.only_managed_children_can_shut_down",
	                   &gcfg->only_managed_children_can_shut_down);

	return 0;
}

int config_parse(char *cfg_file, struct state *s, struct config *gcfg)
{
	config_t cfg;
	int status = -1;

	config_init(&cfg);

	log_info("Parsing configuration from file %s", cfg_file);

	if (!config_read_file(&cfg, cfg_file)) {
		log_err("%s:%d - %s\n", config_error_file(&cfg),
		        config_error_line(&cfg), config_error_text(&cfg));
		goto exit;
	}

	if (parse_global(&cfg, gcfg) < 0)
		goto exit;

	if (parse_domains(&cfg, s) < 0)
		goto exit;

	if (parse_children(&cfg, s) < 0)
		goto exit;

	status = 0;

exit:
	if (status < 0) {
		domain_delete_all(dglist, s);
		child_delete_all(cglist, s);
	}
	config_destroy(&cfg);
	return status;
}

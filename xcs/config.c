#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <dirent.h>
#include <limits.h>
#include <sys/queue.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

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

static void free_char_array(char **array)
{
	int i;
	const char *e;

	if (array) {
		for (i = 0, e = array[0]; e; i++, e = array[i])
			free((void *) e);
		free(array);
	}
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
		free_char_array(argv);
		return NULL;
	}

	return argv;
}

static char ** get_env(config_setting_t *env_setting)
{
	char **env = NULL;
	int status = -1;
	int idx = 0;
	int num_env = config_setting_length(env_setting);

	env = calloc(num_env * 2 + 1, sizeof(char *));
	if (!env)
		goto exit;

	for (int i = 0; i < num_env; i++) {
		const char *e = config_setting_get_string_elem(env_setting, i);
		if (!e)
			goto exit;

		char *p = strstr(e, "=");
		if (!p) {
			log_err("Environment must be provided as VARIABLE=VALUE");
			goto exit;
		}
		*p++ = '\0';

		env[idx++] = strdup(e);
		if (!env[idx-1])
			goto exit;

		env[idx++] = strdup(p);
		if (!env[idx-1])
			goto exit;
	}
	env[idx] = NULL;

	status = 0;

exit:
	if (status) {
		free_char_array(env);
		env = (void *) -1;
	}

	return env;
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
		config_setting_t *environment;
		const char *cmd;
		char **argv = NULL;
		char **env = NULL;
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

		/* Get the environment if available */
		environment = config_setting_get_member(child, "environment");
		if (environment) {
			env = get_env(environment);
			if (env == (void *) -1) {
				free_char_array(argv);
				return -1;
			}
		}

		/* Parse the domain name */
		if (config_setting_lookup_string(child, "domain", &domain)) {
			d = domain_get_by_name(domain, s);

			if (!d) {
				log_info("The domain %s was not found", domain);
				free_char_array(argv);
				free_char_array(env);
				return -1;
			}
		}

		/* Create the child and add it to the global child list */
		ch = child_create(argv, env, d, state_get_new_id(s), &ccfg);
		if (!ch) {
			free_char_array(argv);
			free_char_array(env);
			return -1;
		}
		child_append(ch, cglist, s);
	}

	return 0;
}

static int parse_global(config_t *cfg, struct state *s, struct config *gcfg)
{
	config_setting_t *environment;

	config_lookup_bool(cfg, "mama.require_initial_launch_success",
	                   &gcfg->require_initial_launch_success);

	config_lookup_int(cfg, "mama.exit_handler_timeout_ms",
	                  &gcfg->exit_handler_timeout_ms);

	config_lookup_int(cfg, "mama.shutdown_timeout_ms",
	                  &gcfg->shutdown_timeout_ms);

	config_lookup_bool(cfg, "mama.only_managed_children_can_shut_down",
	                   &gcfg->only_managed_children_can_shut_down);

	config_lookup_int(cfg, "mama.sched_rt_period_us",
	                  &gcfg->sched_rt_period_us);

	config_lookup_int(cfg, "mama.sched_rt_runtime_us",
	                  &gcfg->sched_rt_runtime_us);

	environment = config_lookup(cfg, "mama.environment");
	if (environment) {
		char **env = get_env(environment);
		if (env == (void *) -1)
			return -1;
		s->global_env = env;
	}

	config_lookup_int(cfg, "mama.max_mamacmd_connections",
	                  &gcfg->max_mamacmd_connections);

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

	if (parse_global(&cfg, s, gcfg) < 0)
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

static char * _config_dir;
static char **_tags;

bool tag_filter(const char *filename)
{
	char *tag;
	bool include_file = true;
	char *tmp = strdup(filename); /* Must copy since strtok modifies buffer */

	if (!tmp)
		return false;

	if (!strchr(tmp, '_'))
		/* No tags in file name, will always be included */
		goto exit;

	/* Skip until the first tag */
	(void) strtok(tmp, "_");
	tag = strtok(NULL, "_.");

	while (tag) {
		int idx = 0;

		while (_tags[idx]) {
			/* Any tag match will include the file */
			if (strcmp(tag, _tags[idx++]) == 0)
				goto exit;
		}

		tag = strtok(NULL, "_.");
	}

	include_file = false;

exit:
	if (tmp)
		free(tmp);

	return include_file;
}

static int filter_files(const struct dirent *e)
{
	struct stat st;
	char file_path[PATH_MAX];
	const char *f = e->d_name;
	char *ext;

	/* Minimum size of a valid file name */
	if (strlen(f) < strlen("999-x.cfg"))
		return 0;

	/* Needs to start with 3 digits and a '-' */
	if (!isdigit(f[0]) || !isdigit(f[1]) || !isdigit(f[2]) || f[3] != '-')
		return 0;

	/* Needs to have a file extension */
	ext = strrchr(f, '.');
	if (ext == 0)
		return 0;

	/* The file extension needs to be .cfg */
	if (strcmp(ext, ".cfg") != 0)
		return 0;

	if (_tags && !tag_filter(f))
		return 0;

	snprintf(file_path, PATH_MAX, "%s/%s", _config_dir, e->d_name);

	if (stat(file_path, &st)) {
		log_info("Failed to stat %s", file_path);
		return 0;
	}

	return S_ISREG(st.st_mode) || S_ISLNK(st.st_mode);
}

int config_dir_parse(char *cfg_dir, char **tags, struct state *s,
                     struct config *gcfg)
{
	struct dirent **file_list = NULL;
	char file_path[PATH_MAX];
	char real_path[PATH_MAX];
	int num_files;
	struct stat st;
	int status = -1;

	if (!realpath(cfg_dir, real_path)) {
		log_err("Failed to resolve the absolute path of %s (%s)",
		        cfg_dir, strerror(errno));
		return -1;
	}

	if (stat(real_path, &st)) {
		log_err("Failed to stat %s", real_path);
		return -1;
	}

	if (!S_ISDIR(st.st_mode)) {
		log_err("The config dir supplied %s is not a directory", cfg_dir);
		return -1;
	}

	_config_dir = real_path;
	_tags = tags;

	num_files = scandir(cfg_dir, &file_list, filter_files, alphasort);
	if (num_files < 0) {
		log_err("Failed scandir() [%s]", strerror(errno));
		return -1;
	}

	if (num_files == 0) {
		log_err("No configuration files found in %s", cfg_dir);
		return -1;
	}

	for (int i = 0; i < num_files; i++) {
		snprintf(file_path, PATH_MAX, "%s/%s", real_path, file_list[i]->d_name);

		if (config_parse(file_path, s, gcfg))
			goto exit;
	}

	status = 0;

exit:
	if (file_list)
		free(file_list);

	return status;
}

void config_initialize(struct config *gcfg)
{
	memset(gcfg, 0, sizeof(struct config));
	gcfg->sched_rt_runtime_us = -2;
	gcfg->max_mamacmd_connections = 50;
}

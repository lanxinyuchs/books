/*
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/queue.h>
#include <argz.h>
#include <unistd.h>

#include <libconfig.h>

#include "whitelist.h"

#define CFG_FILE "/etc/coli/whitelist.cfg"
#define COMMAND_CFG_ENTRY "ccoli.commands"

struct linux_command {
	char *name;
	char *usage;
	char *short_descr;
	char *descr;
	char **argv;
	TAILQ_ENTRY(linux_command) e;
};

static TAILQ_HEAD(cmd_head, linux_command) list;

void insert_sorted(struct linux_command *c)
{
	struct linux_command *lc;

	if (TAILQ_EMPTY(&list)) {
		TAILQ_INSERT_HEAD(&list, c, e);
		return;
	}

	TAILQ_FOREACH(lc, &list, e) {
		if (strcmp(c->name, lc->name) < 0) {
			TAILQ_INSERT_BEFORE(lc, c, e);
			return;
		}
	}

	TAILQ_INSERT_TAIL(&list, c, e);
}

int whitelist_init(void)
{
	config_setting_t *cmds;
	int status = -1;
	config_t cfg;
	int cnt;

	TAILQ_INIT(&list);
	config_init(&cfg);

	if (!config_read_file(&cfg, CFG_FILE)) {
		fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
		        config_error_line(&cfg), config_error_text(&cfg));
		goto exit;
	}

	cmds = config_lookup(&cfg, COMMAND_CFG_ENTRY);
	if (!cmds) {
		fprintf(stderr, "Failed to find %s in config file\n", COMMAND_CFG_ENTRY);
		goto exit;
	}

	cnt = config_setting_length(cmds);

	for (int i = 0; i < cnt; i++) {
		config_setting_t *cmd;
		const char *name, *usage, *short_descr, *descr;
		config_setting_t *argv_setting;
		struct linux_command *c;

		cmd = config_setting_get_elem(cmds, i);
		if (!cmd)
			goto exit;

		if (!config_setting_lookup_string(cmd, "name", &name))
			goto exit;

		c = calloc(1, sizeof(struct linux_command));
		if (!c)
			goto exit;

		c->name = malloc(strlen(name) + 1);
		if (!c->name) {
			free(c);
			goto exit;
		}
		strcpy(c->name, name);

		insert_sorted(c);

		if (config_setting_lookup_string(cmd, "usage", &usage)) {
			c->usage = malloc(strlen(usage) + 1);
			if (!c->usage)
				goto exit;
			strcpy(c->usage, usage);
		}

		if (config_setting_lookup_string(cmd, "short_descr", &short_descr)) {
			c->short_descr = malloc(strlen(short_descr) + 1);
			if (!c->short_descr)
				goto exit;
			strcpy(c->short_descr, short_descr);
		}

		if (config_setting_lookup_string(cmd, "descr", &descr)) {
			c->descr = malloc(strlen(descr) + 1);
			if (!c->descr)
				goto exit;
			strcpy(c->descr, descr);
		}

		argv_setting = config_setting_get_member(cmd, "argv");
		if (argv_setting) {
			int idx;
			int num_settings;

			if (!config_setting_is_array(argv_setting)) {
				fprintf(stderr, "The argv of \"%s\" is not an array\n", c->name);
				goto exit;
			}

			num_settings = config_setting_length(argv_setting);
			c->argv = malloc(num_settings * sizeof(char *) + 1);
			if (!c->argv)
				goto exit;

			for (idx = 0; idx < num_settings; idx++) {
				const char *arg =
				    config_setting_get_string_elem(argv_setting, idx);
				if (!arg)
					goto exit;
				c->argv[idx] = malloc(strlen(arg) + 1);
				if (!c->argv[idx])
					goto exit;
				strcpy(c->argv[idx], arg);
			}

			c->argv[idx] = '\0';
		}

		if (c->argv == NULL || c->argv[0] == NULL) {
			fprintf(stderr, "No argv supplied in configuration file for \"%s\"\n",
			        c->name);
			goto exit;
		}

		if (access(c->argv[0], F_OK | X_OK)) {
			fprintf(stderr, "Linux whitelist item \"%s\" is not executable (%s)\n",
			        c->name, strerror(errno));
			goto exit;
		}
	}

	status = 0;

exit:
	config_destroy(&cfg);

	if (status) {
		struct linux_command *c;
		TAILQ_FOREACH(c, &list, e) {
			if (c->name) free(c->name);
			if (c->usage) free(c->usage);
			if (c->short_descr) free(c->short_descr);
			if (c->descr) free(c->descr);
			if (c->argv) {
				for (int i = 0; c->argv[i]; i++)
					free(c->argv[i]);
				free(c->argv);
			}
		}
	}

	return status;
}

char ** whitelist_get_cmd(char *program)
{
	struct linux_command *c;
	int ret;

	TAILQ_FOREACH(c, &list, e) {
		ret = strcmp(c->name, program);

		if (ret == 0)
			return c->argv;

		if (ret > 0)
			return NULL;
	}

	return NULL;
}

char * whitelist_get_next(const char *expr, const char *prev)
{
	struct linux_command *c;

	TAILQ_FOREACH(c, &list, e) {
		if ( (strncmp(expr, c->name, strlen(expr)) == 0) &&
		     (strcmp(c->name, prev) > 0) ) {
			return c->name;
		}
	}

	return NULL;
}

char * whitelist_get_help(char *program)
{
	struct linux_command *c;

	TAILQ_FOREACH(c, &list, e) {
		if (strcmp(program, c->name) == 0)
			return c->short_descr;
	}

	return NULL;
}

#define MAX_USAGE_SIZE (10 * 1024)

static char * get_usage(struct linux_command *c)
{
	char buf[MAX_USAGE_SIZE];
	char *argz;
	size_t argz_len;
	FILE *f = NULL;
	char *usage = NULL;
	size_t len;

	if (argz_create(c->argv, &argz, &argz_len))
		goto exit;
	if (argz_append(&argz, &argz_len, "--help", strlen("--help") + 1))
		goto exit;
	argz_stringify(argz, argz_len, ' ');

	f = popen(argz, "r");
	if (!f)
		goto exit;

	len = fread(buf, 1, MAX_USAGE_SIZE, f);
	if (!len)
		goto exit;

	usage = malloc(len + 1);
	if (!usage)
		goto exit;

	memcpy(usage, buf, len);
	buf[len] = '\0';

exit:
	if (f)
		pclose(f);

	return usage;
}

char * whitelist_get_usage(char *program)
{
	struct linux_command *c;

	TAILQ_FOREACH(c, &list, e) {
		if (strcmp(program, c->name) == 0) {
			if (c->usage)
				return strdup(c->usage);
			else
				return get_usage(c);
		}
	}

	return NULL;
}

char * whitelist_get_descr(char *program)
{
	struct linux_command *c;

	TAILQ_FOREACH(c, &list, e) {
		if (strcmp(program, c->name) == 0)
			return c->descr;
	}

	return NULL;
}

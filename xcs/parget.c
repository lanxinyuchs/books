/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include <inttypes.h>
#include "itc.h"
#include "nvpi3.h"
#include "nvpi3_msg.h"
#include "parcmd.h"

#define PARGET_MAILBOX_NAME        "parget"
#define PAR_CMD_NODE_INDENT 2
#define PAR_CMD_LINE_LENGTH 81 /* Line length incuding '\n'. */

struct db_group_list {
	uint32_t num_of_groups;
	char *group_name[];
};

struct node_print_state {
	char *node_path;
	char *database_name;
	uint32_t depth;
};


union itc_msg {
	uint32_t msgno;
	NVPI3_MESSAGES;
};

static void print_usage(void)
{
	printf("NAME\n");
	printf("    parget - Reads parameters.\n");
	printf("\n");
	printf("SYNOPSIS:\n");
	printf("    parget [-h] [-g <group>] [-n <name>] [-a]\n");
	printf("\n");
	printf("DESCRIPTION\n");
	printf("    Command to read parameters.\n");
	printf("\n");
	printf("    -h prints this text and exits.\n");
	printf("    <group> is optional and defines which database group to read from.\n");
	printf("    If <group> is omitted then all groups are scanned.\n");
	printf("    <name> is optional and defines the names of the paramaters whose values\n");
	printf("    should be returned. Regular expressions using POSIX Extended Regular \n");
	printf("    Expression syntax are allowed in name.\n");
	printf("    If <name> is omitted then values for all parameters are returned.\n");
	printf("    -a is optional and returns all parameters matching <name>, including those\n");
	printf("    that are taggged, deleted default or that are duplicates.\n");
}

static void calc_value_to_str_data(uint32_t first_line, uint32_t line_length,
                                   nvpi3_key_type_t type, uint32_t size,
                                   uint32_t *hanging, uint32_t *num_of_values,
                                   uint32_t *num_of_values_first_line,
                                   uint32_t *num_of_values_per_line)
{
	uint32_t value_size, str_length;

	if (first_line > line_length) {
		first_line = line_length;
	}

	switch (type) {
	case NVPI3_KEY_TYPE_STR:
		str_length = sizeof("a")-1;
		value_size = sizeof(char);
		if (line_length > first_line) {
			first_line++;   /* Increase first for preceeding '"'*/
		}
		if (line_length - first_line < str_length) {
			/* No room to print values on first line. */
			(*hanging)++; /* Increase hanging for preceeding '"'*/
		}
		if (line_length > first_line) {
			(line_length)--; /* Decrease line_length for last '"'*/
		}
		break;
	case NVPI3_KEY_TYPE_U32:
		str_length = sizeof("0x00000000 ")-1;
		value_size = sizeof(uint32_t);
		break;
	case NVPI3_KEY_TYPE_U8:
	case NVPI3_KEY_TYPE_BIN:
	default:
		str_length = sizeof("0x00 ")-1;
		value_size = sizeof(uint8_t);
		break;
	};

	*num_of_values = size / value_size;
	*num_of_values_first_line = (line_length - first_line) / str_length;
	*num_of_values_per_line = (line_length - *hanging) / str_length;

	if (!*num_of_values_per_line) {
		*num_of_values_per_line = 1;
	}
}

static void print_db_and_node_path(struct node_print_state *state,
                                   const char *database_name,
                                   const char **key_name)
{
	const char *start;
	const char *end = state->node_path;

	state->depth = 1;

	if (state->database_name == NULL ||
	    strcmp(state->database_name, database_name)) {
		free(state->database_name);
		state->database_name = malloc(strlen(database_name) + 1);
		if (state->database_name == NULL) {
			printf("malloc of size %d failed, aborting\n",
			       strlen(database_name) + 1);
			return;
		}

		strcpy(state->database_name, database_name);
		printf("%s\n", state->database_name);
	}

	if (state->node_path != NULL) {
		const char *p = state->node_path;

		for (;;) {
			p = strchr(p, '/');
			if (p == NULL) {
				break;
			}

			p++;
			if (strncmp(state->node_path, *key_name,
			            p - state->node_path)) {
				break;
			}
			end = p;
			state->depth++;
		}
	}

	start = (*key_name) + (end - state->node_path);

	for (;;) {
		const char *p = strchr( start, '/');
		if (p == NULL) {
			break;
		}
		p++;
		printf("%*s%.*s\n", PAR_CMD_NODE_INDENT * state->depth, "",
		      p - start, start);
		state->depth++;
		start = p;
	}

	if (start != *key_name) {
		free(state->node_path);
		state->node_path = malloc(start - *key_name + 1);
		if (state->node_path == NULL) {
			printf("malloc of size %d failed, aborting\n",
			       start - *key_name + 1);
			return;
		}
		strncpy(state->node_path, *key_name, start - *key_name);
		state->node_path[start - *key_name] = '\0';
	}

	*key_name = start;
}

static nvpi3_result_t print_value_callback(void *user_data,
                                           struct nvpi3_read_ind **ind)
{
	uint32_t hanging, first_line, i, cursor, num_of_values,
		num_of_values_first_line, num_of_values_per_line;
	struct node_print_state *state = user_data;
	const char *name = (const char *) *ind + (*ind)->name_offset;

	print_db_and_node_path(state, (const char *) *ind +
	                       (*ind)->database_name_offset, &name);
	if ((*ind)->type == NVPI3_READ_IND_NODE) {
		return NVPI3_RESULT_SUCCESS;
	}

	first_line = PAR_CMD_NODE_INDENT * state->depth + strlen(name) +
		sizeof(" = ") - 1;
	hanging = PAR_CMD_NODE_INDENT * (state->depth + 1);
	calc_value_to_str_data(first_line, PAR_CMD_LINE_LENGTH,
	                       (*ind)->u.key.type, (*ind)->u.key.value_size,
	                       &hanging, &num_of_values,
	                       &num_of_values_first_line,
	                       &num_of_values_per_line);

	printf("%*s%s%s = ", PAR_CMD_NODE_INDENT * state->depth, "", name,
	       (const char *) *ind + (*ind)->u.key.type_str_offset);
	if (!num_of_values_first_line) {
		/* If string then decrease hanging for preceeding '"'*/
		printf("\n%*s",
		       ((*ind)->u.key.type == NVPI3_KEY_TYPE_STR && hanging) ?
		       hanging - 1 : hanging, "");
	}

	for (i = cursor = 0; i < num_of_values; i++, cursor++) {
		switch ((*ind)->u.key.type) {
		case NVPI3_KEY_TYPE_STR:
			if (!i) {
				printf("\"");
			}

			printf("%c", (*ind)->u.key.value.str[i]);

			if (i == num_of_values - 1) {
				printf("\"");
			}
			break;
		case NVPI3_KEY_TYPE_U8:
		case NVPI3_KEY_TYPE_BIN:
			if (i != num_of_values - 1) {
				printf("0x%.2x ",
				       (*ind)->u.key.value.u8_array[i]);
			} else {
				printf("0x%.2x",
				       (*ind)->u.key.value.u8_array[i]);
			}
			break;
		case NVPI3_KEY_TYPE_U32:
			if (i != num_of_values - 1) {
				printf("0x%.8x ",
				       (*ind)->u.key.value.u32_array[i]);
			} else {
				printf("0x%.8x",
				       (*ind)->u.key.value.u32_array[i]);
			}
		};

		if (i == num_of_values -1) {
			printf("\n");
		} else if (num_of_values_first_line) {
			num_of_values_first_line--;
			if (!num_of_values_first_line) {
				printf("\n%*s", hanging, "");
				cursor = 0;
			}
		} else if ((cursor && !(cursor % num_of_values_per_line)) ||
		           num_of_values_per_line == 1) {
			printf("\n%*s", hanging, "");
		}
	}
	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t list_db_groups_callback(
	void *user_data, uint32_t group_index, uint32_t num_of_groups,
	const char *name, uint32_t num_of_def,
	const struct nvpi3_db_definition def[])
{
	uint32_t size;
	struct db_group_list **list = user_data;

	if (*list == NULL) {
		size = offsetof(struct db_group_list, group_name) +
			num_of_groups * sizeof((*list)->group_name[0]);
		*list = malloc(size);
		if (*list == NULL) {
			printf("malloc of size %d failed, aborting\n", size);
			return NVPI3_RESULT_OTHER_ERROR;
		}
		memset(*list, 0, size);
		(*list)->num_of_groups = num_of_groups;
	}

	size = strlen(name) + 1;
	(*list)->group_name[group_index] = malloc(size);
	if ((*list)->group_name[group_index] == NULL) {
		printf("malloc of size %d failed, aborting\n", size);
		return NVPI3_RESULT_OTHER_ERROR;
	}

	memcpy((*list)->group_name[group_index], name, size);
	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t par_get(const struct parcmd_conn *conn,
                              struct db_group_list *list, const char *pattern,
                              uint32_t flags)
{
	uint32_t i;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	for (i = 0;
	     i < list->num_of_groups && result == NVPI3_RESULT_SUCCESS;
	     i++) {
		nvpi3_result_t result2;
		nvpi3_db_group_handle group_handle;
		struct node_print_state state = {NULL, NULL, 0};

		result = parcmd_open_db_group(conn, list->group_name[i],
		                              &group_handle);
		if (result != NVPI3_RESULT_SUCCESS) {
			break;
		}

		result = parcmd_read(conn, group_handle, flags, pattern,
		                     print_value_callback, &state);
		free(state.database_name);
		free(state.node_path);
		result2 = parcmd_close_db_group(conn, group_handle);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = result2;
		}
	}
	return result;
}

int main(int argc, char *argv[])
{
	struct parcmd_conn connection;
	int ret = -1, opt;
	uint32_t flags = 0;
	struct db_group_list *list = NULL;
	const char *pattern = "/";
	const char *group_name = NULL;

	while ((opt = getopt(argc, argv, "hg:n:a")) != -1) {
		switch (opt) {
		case 'h':
			print_usage();
			ret = 0;
			goto main_end;
		case 'g':
			group_name = optarg;
			break;
		case 'n':
			pattern = optarg;
			break;
		case 'a':
			flags = NVPI3_READ_REQ_FLAGS_ALL;
			break;
		case '?':
			if (optopt == 'g' || optopt == 'n') {
				printf("Option -%c requires an argument.\n",
				        optopt);
			} else if (isprint(optopt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				        optopt);
			}
			goto main_end;
		default:
			if (isprint(opt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				        opt);
			}
			goto main_end;
		}
	}

	if (optind < argc) {
		printf("Unknown non-option arguments: ");
		while (optind < argc) {
			printf("%s ", argv[optind++]);
		}
		printf("\n");
		goto main_end;
	}

	ret = parcmd_conn_establish(PARGET_MAILBOX_NAME, &connection);
	if (ret) {
		goto main_end;
	}

	if (group_name != NULL) {
		if (list_db_groups_callback(&list, 0, 1, group_name, 0, NULL) !=
		    NVPI3_RESULT_SUCCESS) {
			ret = -1;
			goto main_disconnect;
		}
	} else {
		if (parcmd_list_db_groups(&connection, list_db_groups_callback,
		                          &list) != NVPI3_RESULT_SUCCESS) {
			ret = -1;
			goto main_disconnect;
		}
	}


	if (list != NULL) {
		if (par_get(&connection, list, pattern, flags) !=
		    NVPI3_RESULT_SUCCESS) {
			ret = -1;
		}
	}

main_disconnect:
	if (parcmd_conn_disconnect(&connection)) {
		ret = -1;
	}


main_end:
	if (list != NULL) {
		uint32_t i;

		for (i = 0; i < list->num_of_groups; i++) {
			free(list->group_name[i]);
		}

		free(list);
	}

	exit(ret);
}

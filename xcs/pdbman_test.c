/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <arpa/inet.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <stdint.h>

#include <itc.h>
#include <string.h>

#include <log.h>


#include <pdb_test_nvpi3.h>
#include <pdb_test_misc.h>

#define TEST_MAX_NO_OF_MAILBOXES         4
#define TEST_MAILBOX_NAME               "paramdb_test"
#define TEST_TIMEOUT                     1000

enum supported_cmds {OPEN_DB_GROUP = 0,
                     CLOSE_DB_GROUP,
                     OPEN_NODE,
                     CLOSE_NODE,
                     GET_VALUE,
                     GET_VALUE_SIZE,
                     UNSUPPORTED_COMMAND
                    };

typedef struct {
	uint32_t specific_data;
} client_data_t;


static bool get_cmd(char *cmd, enum supported_cmds *cmd_is)
{
	FUNC;

	DBG("requested command=%s", cmd);

	if (strcmp(cmd, "odbg") == 0) {
		*cmd_is = OPEN_DB_GROUP;
		return true;
	}

	if (strcmp(cmd, "cdb") == 0) {
		*cmd_is = CLOSE_DB_GROUP;
		return true;
	}

	if (strcmp(cmd, "on") == 0) {
		*cmd_is = OPEN_NODE;
		return true;
	}

	if (strcmp(cmd, "cn") == 0) {
		*cmd_is = CLOSE_NODE;
		return true;
	}

	if (strcmp(cmd, "gv") == 0) {
		*cmd_is = GET_VALUE;
		return true;
	}

	if (strcmp(cmd, "gs") == 0) {
		*cmd_is = GET_VALUE_SIZE;
		return true;
	}

	return false;
}


static bool get_key_type(char *value_type, uint32_t *value_type_is,
                         uint32_t *value_type_size)
{

	FUNC;

	if (strstr(value_type, "str") != NULL) {
		*value_type_size = sizeof(char);
		*value_type_is = NVPI3_KEY_TYPE_STR;
		return true;
	}

	if (strstr(value_type, "u32") != NULL) {
		*value_type_size = sizeof(uint32_t);
		*value_type_is = NVPI3_KEY_TYPE_U32;
		return true;
	}

	if (strstr(value_type, "u8") != NULL) {
		*value_type_size = sizeof(uint8_t);
		*value_type_is = NVPI3_KEY_TYPE_U8;
		return true;
	}

	if (strstr(value_type, "bin") == NULL) {
		*value_type_size = sizeof(uint8_t);
		*value_type_is = NVPI3_KEY_TYPE_BIN;
		return true;
	}

	return false;
}

static void print_help_text(char *cmd)
{
	printf("command %s is not supported\n", cmd);
	printf("supported commands are: odb, cdb, on, cn, gv and gs\n");
	printf("odb <name-of-data-base> - opens a data base\n");
	printf("cdb - closes opened data base\n");
	printf("on <name-of-node> - opens a node\n");
	printf("cn - closes opened node\n");
	printf("gv <name-of-key, from node handle> <key-type> <size-of-value>"
	       "- reads a value\n");
	printf("gs <name-of-key, from node handle> <key-type>"
	       "- reads size of value\n");
}

static void print_value(char *key, int value_type, int value_size,
                        const void *value)
{
	uint32_t ix;
	char val[250];
	char *valp;
	const char *vp = value;

	FUNC;

	valp = &val[0];
	valp = valp + sprintf(valp, "%s = ", key);

	val[strlen(key) + 3 + value_size + 1] = '\0';

	switch (value_type) {
	case NVPI3_KEY_TYPE_STR:
		sprintf(valp, "%s", value);
		break;

	case NVPI3_KEY_TYPE_U32:
		for (ix = 0; ix < value_size / 4; ix++) {
			valp = valp + sprintf(valp, "0x%08x ", *(uint32_t *)vp);
			vp = vp + 4;
		}
		break;

	default: /* NVPI3_KEY_TYPE_BIN or U8 */
		for (ix = 0; ix < value_size; ix++) {
			valp = valp + sprintf(valp, "0x%02x ", *vp);
			vp++;
		}
		break;
	}

	printf("AND HERE IS THE RESULT:\n%s\n", val);
}


int main(int argc, char *argv[])
{
	uint32_t nvpi3_res = NVPI3_RESULT_SUCCESS;
	itc_mbox_id_t my_mbox_id = ITC_NO_ID;
	enum supported_cmds cmd;
	nvpi3_db_group_handle db_group_handle = NULL;
	nvpi3_node_handle node_handle = NULL;
	int value_type;
	size_t value_type_size;
	uint32_t value_size, read_value_size;
	uint32_t current_arg = 0;
	uint32_t previous_arg = 0;
	uint32_t parsed_arg = 0;
	uint32_t ix = 0;
	uint32_t argc_ix = argc;
	union nvpi3_key_value *value_buff;

	FUNC;

	if (argc == 1) {
		print_help_text("");
		exit(1);
	}

	DBG("no of arguments, argc=%d\n", argc_ix);
	for (ix = 0; ix < argc_ix; ix++)
		DBG("argv[%u]=%s", ix, argv[ix]);

	if(!test_init_mailbox_if(&my_mbox_id)) {
		printf("failure - initating mailbox interface");
		goto main_failure;
	}
	DBGNF("mailbox if initiated successfully");

	/* compensate for first command in argc which is "pdbman_test" */
	argc_ix--;

	while (argc_ix > 0) {
		previous_arg = current_arg;
		current_arg++; /* get next command */
		if (!get_cmd(argv[current_arg], &cmd)) {
			print_help_text(argv[current_arg]);
			exit(1);
		}

		switch (cmd) {
		case OPEN_DB_GROUP:
			/* command is followed by db_name or null */
			current_arg++;
			printf("opening data base db=%s", argv[current_arg]);
			db_group_handle = open_db_group(argv[current_arg],
			                                &nvpi3_res);
			if (nvpi3_res != NVPI3_RESULT_SUCCESS) {
				printf("failure opening data base result=%d\n",
				       nvpi3_res);
				goto main_failure;
			}

			DBG("success - OPEN_DB, db_group_handle = %d",
			    db_group_handle);
			printf("success - OPEN_DB\n");
			break;

		case CLOSE_DB_GROUP:
			printf("closing opened data base");
			DBG("db_group_handle=%d", db_group_handle);
			/* it is not possible to define which db_group_handle
			   to close */
			close_db_group(db_group_handle, &nvpi3_res);
			if (nvpi3_res != NVPI3_RESULT_SUCCESS) {
				printf("failure closing data base result=%d\n",
				       nvpi3_res);
				goto main_failure;
			}
			printf("sucess - CLOSE_DB_GROUP\n");
			break;

		case OPEN_NODE:
			/* command is follwed by node_name, db-handle is already
			   opened or NULL */
			current_arg++;

			printf("Opening node %s", argv[current_arg]);
			node_handle = open_node(db_group_handle,
			                        argv[current_arg],
			                        &nvpi3_res);
			if (nvpi3_res != NVPI3_RESULT_SUCCESS) {
				printf("failure opening node result=%d\n",
				       nvpi3_res);
				goto main_failure;
			}
			DBG("node_handle=%d", node_handle);
			printf("success - OPEN_NODE\n");
			break;

		case CLOSE_NODE:
			/* node_handle is either an already opened node or
			   NULL */

			printf("closing opened node\n");
			DBG("node_handle=%d", node_handle);
			close_node(node_handle, &nvpi3_res);
			if (nvpi3_res != NVPI3_RESULT_SUCCESS) {
				printf("failure closing node result=%d", nvpi3_res);
				goto main_failure;
			}
			printf("sucess - CLOSE_NODE");
			break;

		case GET_VALUE:
			/* command is followed by key, type and size of value,
			   node_id is already opened or NULL */
			current_arg++;
			printf("geting key value key=%s, key type=%s, value size=%s",
			       argv[current_arg], argv[current_arg + 1],
			       argv[current_arg + 2]);

			if (!get_key_type(argv[current_arg + 1], &value_type,
			                  &value_type_size)) {
				printf("failure - unsupported parameter values");
				goto main_failure;
			}

			value_size = (uint32_t)atoi(argv[current_arg + 2]);
			DBG("value_type=%d, value_type_size=%d, value_size=%d",
			    value_type, value_type_size, value_size);

			value_buff = (union nvpi3_key_value *)malloc(
			                     value_type_size * value_size);
			if (value_buff == NULL) {
				printf("failure allocating buffer to which key"
				       "value will be written\n");
				goto main_failure;
			}

			read_value_size = get_value(node_handle,
			                            argv[current_arg],
			                            value_type,
			                            value_size,
			                            value_buff,
			                            &nvpi3_res);
			if (nvpi3_res != NVPI3_RESULT_SUCCESS) {
				printf("failure reading key value result=%d\n",
				       nvpi3_res);
				goto main_failure;
			}

			print_value(argv[current_arg], value_type,
			            read_value_size, (void *)value_buff);
			printf("sucess reading value\n");
			current_arg = current_arg + 2;
			free(value_buff);
			break;

		case GET_VALUE_SIZE:
			/* command is followed by key and type,
			   db_group_handle and node_id are already opened or
			   NULL */
			current_arg++;
			printf("geting value size for key=%s, key type=%s",
			       argv[current_arg], argv[current_arg + 1]);

			if (!get_key_type(argv[current_arg + 1], &value_type,
			                  &value_type_size)) {
				printf("failure - unsupported parameter values");
				goto main_failure;
			}

			DBG("value_type=%d, value_type_size=%d",
			    value_type, value_type_size);

			read_value_size = get_value_size(node_handle,
			                                 argv[current_arg],
			                                 value_type, &nvpi3_res);

			if (nvpi3_res != NVPI3_RESULT_SUCCESS) {
				printf("failure reading value size result=%d\n",
				       nvpi3_res);
				goto main_failure;
			}

			printf("sucess reading value size\n");
			current_arg = current_arg + 1;
			break;

		default:
			goto main_failure;
			break;

		}

		parsed_arg = current_arg - previous_arg;
		argc_ix = argc_ix - parsed_arg;
	}

	DBG("delete mailbox=%u", my_mbox_id);
	itc_delete_mailbox(my_mbox_id);
	DBGNF("mailbox deleted");
	printf("sucsess - requested command/commands PASSED");
	exit(0);

main_failure:

	if (my_mbox_id != ITC_NO_ID)
		itc_delete_mailbox(my_mbox_id);

	exit(1);
}

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

#define PARSET_MAILBOX_NAME        "parset"

union itc_msg {
	uint32_t msgno;
	NVPI3_MESSAGES;
};

static void print_usage(void)
{
	printf("NAME\n");
	printf("    parset - Sets a parameter.\n");
	printf("\n");
	printf("SYNOPSIS:\n");
	printf("    parset [-h] -g <group> -n <name> -t <type> -v <value>[,<value>...]\n");
	printf("\n");
	printf("DESCRIPTION\n");
	printf("    Command to set a parameter.\n");
	printf("\n");
	printf("    -h prints this text and exits.\n");
	printf("    <group> defines which database group to set parameter value in.\n");
	printf("    <name> define the name of the paramaters whose value should be set.\n");
	printf("    <type> define the type of the value to set. Supported types are str, u8,\n");
	printf("    u32 and bin\n");
	printf("    str defines that <value> is a string\n");
	printf("    u8 defines that <value> is a byte. Multiple values are allowed.\n");
	printf("    u32 defines that <value> is a 32-bit word. Multiple values are allowed\n");
	printf("    bin defines that <value> is a byte. Multiple values are allowed\n");
	printf("    <value> defines the value(s)to set.\n");
}

static nvpi3_result_t str_to_type(const char *type_str, nvpi3_key_type_t *type)
{
	if (!strcmp(type_str,"str")) {
		*type = NVPI3_KEY_TYPE_STR;
	} else if (!strcmp(type_str,"u8")) {
		*type = NVPI3_KEY_TYPE_U8;
	} else if (!strcmp(type_str,"u32")) {
		*type = NVPI3_KEY_TYPE_U32;
	} else if (!strcmp(type_str,"bin")) {
		*type = NVPI3_KEY_TYPE_BIN;
	} else {
		return NVPI3_RESULT_INVALID_PARAM;
	}
	return NVPI3_RESULT_SUCCESS;
}

static uint32_t str_to_num_of_values(const char *value_str)
{
	uint32_t i = 0;

	if (value_str[0] == '\0') {
		goto str_to_num_of_values_end;
	}

	do {
		i++;
		value_str = strchr(value_str + 1, ',');
	} while (value_str != NULL);

str_to_num_of_values_end:
	return i;
}

static nvpi3_result_t alloc_set_value_req(const char *key_name,
                                          nvpi3_key_type_t type,
                                          uint32_t num_of_values,
                                          const char *value_str,
                                          struct nvpi3_set_value_req **req)
{
	uint32_t i, name_offset, size, req_size;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	char *p, *str = NULL;

	if (type == NVPI3_KEY_TYPE_STR) {
		/* Add space for '\0' */
		num_of_values++;
	}

	size = num_of_values;
	if (type == NVPI3_KEY_TYPE_U32) {
		size *= sizeof(uint32_t);
	}

	name_offset = offsetof(struct nvpi3_set_value_req, value) + size;
	req_size = name_offset + strlen(key_name) + 1;

	*req = (struct nvpi3_set_value_req *)
	      itc_alloc(req_size, NVPI3_SET_VALUE_REQ);
	(*req)->key_name_offset = name_offset;
	strcpy((char *) *req + name_offset, key_name);
	(*req)->type = type;
	(*req)->value_size = size;

	if (type == NVPI3_KEY_TYPE_STR) {
	  memcpy((*req)->value.str, value_str, size);
		goto alloc_set_value_req_end;
	}

	size = strlen(value_str) + 1;
	str = malloc(size);
	if (str == NULL) {
		printf("malloc of size %d failed, aborting\n", size);
		result = NVPI3_RESULT_OTHER_ERROR;
		goto alloc_set_value_req_end;
	}
	memcpy(str, value_str, size);

	p = strtok (str," ,");
	for (i = 0; p != NULL; i++) {
		unsigned long value;
		char* end;

		value = strtoul (p, &end, 0);
		if (end == p || *end != '\0') {
			printf("Invalid argument %s\n%*s\n", value_str,
			       sizeof("Invalid argument ")+ end - str, "^");
			result =  NVPI3_RESULT_INVALID_PARAM;
			goto alloc_set_value_req_end;
		}
		if (type == NVPI3_KEY_TYPE_U32) {
			(*req)->value.u32_array[i] = value;
		} else {
			if (value > UINT8_MAX) {
				printf("Invalid argument %s\n%*s\n", value_str,
				       p - str, "^");
				result =  NVPI3_RESULT_INVALID_PARAM;
				goto alloc_set_value_req_end;
			}

			(*req)->value.u8_array[i] = (uint8_t) value;
		}

		p = strtok (NULL, " ,");
       }

alloc_set_value_req_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		itc_free((union itc_msg **) req);
	}

	free(str);
	return result;
}

static nvpi3_result_t set_value(const struct parcmd_conn *conn,
                                nvpi3_db_group_handle group_handle,
                                struct nvpi3_set_value_req **req)
{
	union itc_msg *reply;
	itc_monitor_id_t monitor_id;
	int monitored;
	nvpi3_result_t result= NVPI3_RESULT_SUCCESS;
	static const uint32_t filter[] =
		{3, NVPI3_SET_VALUE_CFM, NVPI3_SET_VALUE_REJ,
		 ITC_MONITOR_DEFAULT_NO};

	monitor_id = itc_monitor(conn->server_mbox, NULL);
	monitored = 1;

	(*req)->connection_ref = conn->server_ref;
	(*req)->group_handle = group_handle;
	(*req)->transaction_handle = NULL;
	itc_send((union itc_msg **) req, conn->server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, conn->server_mbox);
	switch (reply->msgno) {
	case NVPI3_SET_VALUE_CFM:
		break;
	case NVPI3_SET_VALUE_REJ:
		printf("Received NVPI3_SET_VALUE_REJ (%" PRIu32
		       "), aborting\n", reply->nvpi3_set_value_rej.error);
		result = reply->nvpi3_set_value_rej.error;
		break;
	case ITC_MONITOR_DEFAULT_NO:
		printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
		result = NVPI3_RESULT_OTHER_ERROR;
		monitored = 0;
		break;
	}

	if (monitored) {
		itc_unmonitor(monitor_id);
	}

	itc_free(&reply);
	return result;
}

static nvpi3_result_t par_set(const struct parcmd_conn *conn,
                              const char *group_name, const char *key_name,
                              const char *type_str, const char *value_str)
{
	nvpi3_db_group_handle group_handle;
	nvpi3_key_type_t type;
	uint32_t num_of_values;
	nvpi3_result_t result, result2;
	struct nvpi3_set_value_req *req;

	result = parcmd_open_db_group(conn, group_name, &group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		return result;
	}

	result = str_to_type(type_str, &type);
	if (result != NVPI3_RESULT_SUCCESS) {
		printf("Invalid argument %s\n", type_str);
		goto par_set_end;
	}

	if (type == NVPI3_KEY_TYPE_STR) {
		num_of_values = strlen(value_str);
	} else {
		num_of_values = str_to_num_of_values(value_str);
	}

	if (!num_of_values) {
		printf("Invalid argument %s\n", value_str);
		result = NVPI3_RESULT_INVALID_PARAM;
		goto par_set_end;
	}

	result = alloc_set_value_req(key_name, type, num_of_values, value_str,
	                             &req);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto par_set_end;
	}

	result = set_value(conn, group_handle, &req);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto par_set_end;
	}

par_set_end:
	result2 = parcmd_close_db_group(conn, group_handle);
	if (result == NVPI3_RESULT_SUCCESS) {
		result = result2;
	}

	return result;
}

int main(int argc, char *argv[])
{
	struct parcmd_conn connection;
	int ret = 0, opt;
	const char *group_name = NULL, *key_name = NULL, *type_str = NULL,
		*value_str = NULL;

	while ((opt = getopt(argc, argv, "hg:n:t:v:")) != -1) {
		switch (opt) {
		case 'h':
			print_usage();
			goto main_end;
		case 'g':
			group_name = optarg;
			break;
		case 'n':
			key_name = optarg;
			break;
		case 't':
			type_str = optarg;
			break;
		case 'v':
			value_str = optarg;
			break;
		case '?':
			if (optopt == 'g' || optopt == 'n' || optopt == 't' ||
			    optopt == 'v') {
				printf("Option -%c requires an argument.\n",
				        optopt);
			} else if (isprint(optopt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				        optopt);
			}
			ret = -1;
			goto main_end;
		default:
			if (isprint(opt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				        opt);
			}
			ret = -1;
			goto main_end;
		}
	}

	if (group_name == NULL) {
		printf("Missing argument -g <group>\n");
		ret = -1;
	}

	if (key_name == NULL) {
		printf("Missing argument -n <name>\n");
		ret = -1;
	}

	if (type_str == NULL) {
		printf("Missing argument -t <type>\n");
		ret = -1;
	}

	if (value_str == NULL) {
		printf("Missing argument -v <value>[,<value>...]\n");
		ret = -1;
	}

	if (optind < argc) {
		printf("Unknown non-option arguments: ");
		while (optind < argc) {
			printf("%s ", argv[optind++]);
		}
		printf("\n");
		ret = -1;
	}

	if (ret) {
		goto main_end;
	}

	ret = parcmd_conn_establish(PARSET_MAILBOX_NAME, &connection);
	if (ret) {
		goto main_end;
	}

	if (par_set(&connection, group_name, key_name, type_str, value_str) !=
	            NVPI3_RESULT_SUCCESS) {
		ret = -1;
	}

	if (parcmd_conn_disconnect(&connection)) {
		ret = -1;
	}

main_end:
	exit(ret);
}

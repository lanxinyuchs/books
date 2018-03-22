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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <log.h>

#include <nvpi3.h>

#include <pdb_test_misc.h>
#include <pdb_test_nvpi3.h>


nvpi3_db_group_handle open_db_group(char *group_name, uint32_t *res)
{
	nvpi3_db_group_handle db_group_handle = NULL;

	FUNC;

	*res = nvpi3_open_db_group(group_name, &db_group_handle);
	if (*res != NVPI3_RESULT_SUCCESS)
		printf("failure opening data base, group_name=%s, res=%s",
		       group_name, test_nvpi3_res_to_str(*res));
	DBG("PDBTest: rcvd db_group_handle=%u", (uint32_t)db_group_handle);

	/* db_group_handle = NULL if failure, != NULL if success */
	return db_group_handle;
}


void close_db_group(nvpi3_db_group_handle db_group_handle, uint32_t *res)
{
	FUNC;

	DBG("close_db_groip: db_group_handle = %u", (uint32_t)db_group_handle);
	*res = nvpi3_close_db_group(db_group_handle);
	if (*res != NVPI3_RESULT_SUCCESS)
		printf("failure closing data base db_group_handle=%d, res=%s",
		       db_group_handle, test_nvpi3_res_to_str(*res));

	return;
}


nvpi3_node_handle open_node(nvpi3_db_group_handle db_group_handle,
                            char *node_name,
                            uint32_t *res)
{
	nvpi3_node_handle node_handle;

	FUNC;

	if (strcmp(node_name, "null") == 0) {
		DBGNF("default node is used = root node");
		node_name[0] = '\0';
	}

	DBG("db_group_handle=%d, node_name=%s", db_group_handle, node_name);

	*res = nvpi3_open_node(db_group_handle, node_name, &node_handle);
	if (*res != NVPI3_RESULT_SUCCESS)
		printf("failure opening node, db_group_handle=%d, node_name=%s, res=%s",
		       db_group_handle, node_name, test_nvpi3_res_to_str(*res));

	/* node_handle = NULL if failure, != NULL if success */
	return node_handle;
}

void close_node(nvpi3_node_handle node_handle, uint32_t *res)
{
	FUNC;

	DBG("node_handle=%d", node_handle);
	*res = nvpi3_close_node(node_handle);
	return;
}

uint32_t get_value(nvpi3_node_handle node_handle, char *key_name,
                   uint32_t value_type, uint32_t value_buff_size,
                   union nvpi3_key_value *value_buff, uint32_t *res)
{
	uint32_t key_name_len = 0;

	FUNC;

	*res = NVPI3_RESULT_SUCCESS;

	key_name_len = strlen(key_name) + 1; /* include null-char */

	*res = nvpi3_get_value(node_handle, key_name, value_type,
	                       &value_buff_size, value_buff);

	if (*res != NVPI3_RESULT_SUCCESS) {
		printf("failure - reading value for key=%s, res=%s",
		       key_name, test_nvpi3_res_to_str(*res));
	}

	return value_buff_size;
}


uint32_t get_value_size(nvpi3_node_handle node_handle, char *key_name,
                        uint32_t value_type, uint32_t *res)
{
	uint32_t key_name_len, value_size;

	FUNC;

	key_name_len = strlen(key_name) + 1; /* include null-char */

	DBG("node_handle=%d, key_name=%s, key_name_len=%d",
	    node_handle, key_name, key_name_len);

	*res = nvpi3_get_value_size(node_handle, key_name, value_type,
	                            &value_size);
	if (*res != NVPI3_RESULT_SUCCESS) {
		printf("failure - reading value size for key=%s, res=%s", key_name,
		       test_nvpi3_res_to_str(*res));
	}

	return value_size;
}

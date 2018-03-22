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

#include <nvpi3.h>

#include <log.h>

#include <pdb_test_nvpi3.h>
#include <pdb_test_misc.h>
#include <pdb_test_data.h>

#define MAX_KEY_NAME_LEN 60
#define MAX_VALUE_BUFF_SIZE 150

/*
 * get_value_sizes_one_node_handle
 *
 * Opens a data base, "sys_bpar", opens a node, gets keys' value sizes.
 * Gets the same size twice. Checks that value sizes are as expected. */
enum tc_result get_value_sizes_one_node_handle(void)
{
	nvpi3_db_group_handle db_group_handle;
	nvpi3_node_handle node_handle[2];
	uint32_t nvpi3_res, read_value_size, ix;
	char key_cpy[MAX_KEY_NAME_LEN];

	FUNC;

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	node_handle[0] = open_node(db_group_handle, root_a_node, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	node_handle[1] = open_node(db_group_handle, root_bbb_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	/* Key1 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", a0_str_sk );
	DBG("key=%s#str", key_cpy);
	read_value_size = get_value_size(node_handle[0], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");

	DBG("key=%s, expected size=%d, read_value_size=%d", key_cpy,
	    strlen(a0_str_v) + 1, read_value_size);

	if (read_value_size != strlen(a0_str_v) + 1) {
		printf("failure - get value size key=%s", key_cpy);
		return TC_FAILED;
	}

	/* Key2 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", a1_str_sk);
	DBG("key=%s#str", key_cpy);
	read_value_size = get_value_size(node_handle[0], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");

	DBG("key=%s, expected size=%d, read_value_size=%d", key_cpy,
	    strlen(a1_str_v) + 1, read_value_size);

	if (read_value_size != strlen(a1_str_v) + 1) {
		printf("failure - get value size key=%s", key_cpy);
		return TC_FAILED;
	}

	/* Key2, read same value size twice */
	DBG("key=%s#str", key_cpy);
	read_value_size = get_value_size(node_handle[0], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");

	DBG("key=%s, expected size=%d, read_value_size=%d", key_cpy,
	    strlen(a1_str_v) + 1, read_value_size);

	if (read_value_size != strlen(a1_str_v) + 1) {
		printf("failure - get value size key=%s", key_cpy);
		return TC_FAILED;
	}

	/* key3 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", bbb0_str_sk);
	DBG("key_cpy=%s#str", key_cpy);

	read_value_size = get_value_size(node_handle[1], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");

	DBG("key=%s, expected size=%d, read_value_size=%d", key_cpy,
	    strlen(bbb0_str_v) + 1, read_value_size);

	if (read_value_size != strlen(bbb0_str_v) + 1) {
		printf("failure - get value size key =%s", key_cpy);
		return TC_FAILED;
	}

	for (ix = 0; ix < 2; ix++) {
		close_node(node_handle[ix], &nvpi3_res);
		CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");
	}

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing data base");

	DBGNF("success - get_value_sizes_one_node_handle");
	return TC_PASSED;
}

/*
 * get_value_sizes_two_node_handles
 *
 * Opens a data base, "sys_bpar", opens two node, gets keys' value sizes in
 * different nodes. Checks that value sizes are as expected. */
enum tc_result get_value_sizes_two_node_handles(void)
{
	nvpi3_db_group_handle db_group_handle;
	nvpi3_node_handle node_handle[2];
	uint32_t nvpi3_res, read_value_size, ix;
	char key_cpy[MAX_KEY_NAME_LEN];

	FUNC;

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	node_handle[0] = open_node(db_group_handle, root_a_node, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	node_handle[1] = open_node(db_group_handle, root_b_node, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	/* key1 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", a1_str_sk);
	DBG("key=%s#str", key_cpy);
	read_value_size = get_value_size(node_handle[0], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");

	DBG("key=%s, expected size=%d, read_value_size=%d", key_cpy,
	    strlen(a1_str_v) + 1, read_value_size);

	if (read_value_size != strlen(a1_str_v) + 1) {
		printf("failure - get value size ke=%s", key_cpy);
		return TC_FAILED;
	}

	/* key2 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", b0_str_sk);
	DBG("key=%s#str", key_cpy);
	read_value_size = get_value_size(node_handle[1], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");

	DBG("key=%s, expected size=%d, read_value_size=%d", key_cpy,
	    strlen(b0_str_v) + 1, read_value_size);

	if (read_value_size != strlen(b0_str_v) + 1) {
		printf("failure - get value size key=%s", key_cpy);
		return TC_FAILED;
	}

	for (ix = 0; ix < 2; ix++) {
		close_node(node_handle[ix], &nvpi3_res);
		CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");
	}

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing data base");

	DBGNF("success - get_value_sizes_two_node_handles");
	return TC_PASSED;
}

/*
 * get_value_sizes_two_node_handles_loop
 *
 * Opens a data base, "sys_bpar", opens two node, gets keys' value sizes in
 * different nodes. Checks that value sizes are as expected. */
enum tc_result get_value_sizes_two_node_handles_loop(void)
{
	enum tc_result tc_res;
	uint32_t ix;

	for (ix = 0; ix < TEST_MAX_LOOPS; ix++) {
		tc_res = get_value_sizes_two_node_handles();
		if (tc_res != TC_PASSED) {
			printf("failure - geting value");
			return TC_FAILED;
		}
	}

	DBGNF("success - get_value_sizes_two_node_handles_loop");
	return TC_PASSED;
}

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
#include <string.h>

#include <nvpi3.h>

#include <log.h>

#include <pdb_test_nvpi3.h>
#include <pdb_test_misc.h>
#include <pdb_test_data.h>

#define MAX_KEY_NAME_LEN 40
#define MAX_VALUE_BUFF_SIZE 25

/*
 * get_values_one_node_handle
 *
 * Opens a data base, "sys_bpar", opens a node, gets keys' values. Gets the same
 * key twice. Checks that values are as expected. */
enum tc_result get_values_one_node_handle(void)
{
	nvpi3_db_group_handle db_group_handle;
	nvpi3_node_handle node_handle;
	uint32_t nvpi3_res, read_value_size;
	char key_cpy[MAX_KEY_NAME_LEN];
	char value_buff[MAX_VALUE_BUFF_SIZE];

	/* Get vallue for string "top_str0_k" */

	FUNC;

	value_buff[MAX_VALUE_BUFF_SIZE - 1] = '\0';

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	node_handle = open_node(db_group_handle, root_a_node, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	/* Key1 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", a0_str_sk );
	DBG("key_cpy=%s#str", key_cpy);

	read_value_size = get_value(node_handle, key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE,
	                            (union nvpi3_key_value *)value_buff,
	                            &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");
	DBG("key=%s#str, expected value=%s, read value=%s",
	    key_cpy, a0_str_v, value_buff);

	if (strcmp(value_buff, a0_str_v) != 0) {
		printf("failure - incorrect value key=%s#str", key_cpy);
		return TC_FAILED;
	}

	/* Key2 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", a1_str_sk);
	DBG("key_cpy=%s#str", key_cpy);

	read_value_size = get_value(node_handle, key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE,
	                            (union nvpi3_key_value *)value_buff,
	                            &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");
	DBG("key=%s#str, expected value=%s, read value=%s",
	    key_cpy, a1_str_v, value_buff);

	if (strcmp(value_buff, a1_str_v) != 0) {
		printf("failure - incorrect value key=%s#str", key_cpy);
		return TC_FAILED;
	}

	/* Key2, read same key twice */
	DBG("key_cpy=%s#str", key_cpy);

	read_value_size = get_value(node_handle, key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE,
	                            (union nvpi3_key_value *)value_buff,
	                            &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");
	DBG("key=%s#str, expected value=%s, read value=%s",
	    key_cpy, a1_str_v, value_buff);

	if (strcmp(value_buff, a1_str_v) != 0) {
		printf("failure reading same key twice - incorrect value"
		       " key=%s#str", key_cpy);
		return TC_FAILED;
	}

	close_node(node_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing data base");

	DBGNF("success - get_values_one_node_handle");
	return TC_PASSED;
}

/*
 * get_values_two_node_handles
 *
 * Opens a data base, "sys_bpar", opens two nodes, gets keys' values in opened
 * nodes. Checks that values are as expected.
 */
enum tc_result get_values_two_node_handles(void)
{
	nvpi3_db_group_handle db_group_handle;
	nvpi3_node_handle node_handle[2];
	uint32_t nvpi3_res, read_value_size, ix;
	char key_cpy[MAX_KEY_NAME_LEN];
	char value_buff[MAX_VALUE_BUFF_SIZE];

	/* Get vallue for string "top_str0_k" */

	FUNC;

	value_buff[MAX_VALUE_BUFF_SIZE - 1] = '\0';

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	node_handle[0] = open_node(db_group_handle, root_a_node, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	node_handle[1] = open_node(db_group_handle, root_bbb_node, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	/* key1, node1  */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", a0_str_sk);
	/* DBG("key_cpy=%s#str",key_cpy); */
	read_value_size = get_value(node_handle[0], key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE,
	                            (union nvpi3_key_value *)value_buff,
	                            &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");
	/* DBG("key=%s#str, expected value=%s, read value=%s",
	   key_cpy, a0_str_v, value_buff); */

	if (strcmp(value_buff, a0_str_v) != 0) {
		printf("failure - incorrect value key=%s#str", key_cpy);
		return TC_FAILED;
	}

	/* key2, node1  */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", a1_str_sk);
	/* DBG("key_cpy=%s#str",key_cpy); */
	read_value_size = get_value(node_handle[0], key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE,
	                            (union nvpi3_key_value *)value_buff,
	                            &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");
	/* DBG("key=%s#str, expected value=%s, read value=%s",
	   key_cpy, a1_str_v, value_buff); */

	if (strcmp(value_buff, a1_str_v) != 0) {
		printf("failure - incorrect value key=%s#str", key_cpy);
		return TC_FAILED;
	}

	/* key2, node2 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", bbb0_str_sk);
	/* DBG("key_cpy=%s#str",key_cpy); */
	read_value_size = get_value(node_handle[1], key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE,
	                            (union nvpi3_key_value *)value_buff,
	                            &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");
	/* DBG("key=%s#str, expected value=%s, read value=%s",
	   key_cpy, bbb0_str_v, value_buff); */

	if (strcmp(value_buff, bbb0_str_v) != 0) {
		printf("failure - incorrect value key=%s#str", key_cpy);
		return TC_FAILED;
	}

	for (ix = 0; ix < 2; ix++) {
		close_node(node_handle[ix], &nvpi3_res);
		CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");
	}

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing data base");

	DBGNF("success - get_values_two_node_handles");
	return TC_PASSED;
}

/*
 * get_values_two_node_handles_loop
 *
 * Opens a data base, "sys_bpar", opens two nodes, gets keys' values in opened
 * nodes. Checks that values are as expected.
 */
enum tc_result get_values_two_node_handles_loop(void)
{
	enum tc_result tc_res;
	uint32_t ix = 0;

	FUNC;

	for (ix = 0; ix < TEST_MAX_LOOPS; ix++) {
		tc_res = get_values_two_node_handles();
		if (tc_res != TC_PASSED) {
			printf("failure - tc:get_values_two_node_handles_loop");
			return TC_FAILED;
		}
	}

	DBGNF("success - get_values_two_node_handles_loop");
	return TC_PASSED;
}

/*
 * get_value_sizes_and_values_two_node_handles
 *
 * Opens a data base, "sys_bpar", opens two node, gets keys' value sizes
 * and then gets actual values. Checks that value is as expected.
 */
enum tc_result get_value_sizes_and_values_two_node_handles(void)
{
	nvpi3_db_group_handle db_group_handle;
	nvpi3_node_handle node_handle[2];
	uint32_t nvpi3_res, read_value_size, ix;
	char key_cpy[MAX_KEY_NAME_LEN];
	union nvpi3_key_value *value_buff;

	FUNC;

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	node_handle[0] = open_node(db_group_handle, root_ab_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	/* key1 */
	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", ab0_str_sk);
	DBG("key=%s#str", key_cpy);
	read_value_size = get_value_size(node_handle[0], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - geting value");

	if (read_value_size != strlen(ab0_str_v) + 1) {
		printf("failure - geting value size for key=%s#str", key_cpy);
		goto failure;
	}

	value_buff = (union nvpi3_key_value *)malloc(read_value_size * sizeof(
	                        union nvpi3_key_value));
	CHECK_MALLOC_RETURN_ON_ERROR(value_buff);

	read_value_size = get_value(node_handle[0], key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE, value_buff,
	                            &nvpi3_res);
	CHECK_RES_GOTO_ON_ERROR(nvpi3_res, "failure-getting value", failure);
	DBG(" expected value=%s, read value=%s", ab0_str_v, value_buff);

	if (strcmp(value_buff, ab0_str_v) != 0) {
		printf("failure - geting value for key=%s#str", key_cpy);
		goto failure;
	}

	free((union nvpi3_key_value *)value_buff);

	/* key2 */
	node_handle[1] = open_node(db_group_handle, root_top_node,
	                           &nvpi3_res);
	CHECK_RES_GOTO_ON_ERROR(nvpi3_res, "failure - opening node", failure);

	snprintf(key_cpy, MAX_KEY_NAME_LEN, "%s", top0_str_sk);
	DBG("key=%s#str", key_cpy);
	read_value_size = get_value_size(node_handle[1], key_cpy,
	                                 NVPI3_KEY_TYPE_STR, &nvpi3_res);
	CHECK_RES_GOTO_ON_ERROR(nvpi3_res, "failure - getting value size", failure);

	if (read_value_size != strlen(top0_str_v) + 1) {
		printf("failure - geting value size for key=%s#str", key_cpy);;
		goto failure;
	}

	value_buff = (union nvpi3_key_value *)malloc(read_value_size * sizeof(
	                        union nvpi3_key_value));
	CHECK_MALLOC_GOTO_ON_ERROR(value_buff, failure);

	read_value_size = get_value(node_handle[1], key_cpy, NVPI3_KEY_TYPE_STR,
	                            MAX_VALUE_BUFF_SIZE, value_buff,
	                            &nvpi3_res);
	CHECK_RES_GOTO_ON_ERROR(nvpi3_res, "failure - getting value", failure);
	DBG(" expected value=%s, read value=%s", top0_str_v, value_buff);

	if (strcmp(value_buff, top0_str_v) != 0) {
		printf("failure - geting value for key=%s#str", key_cpy);
		goto failure;
	}

	free((union nvpi3_key_value *)value_buff);

	for(ix = 0; ix < 2; ix++) {
		close_node(node_handle[ix], &nvpi3_res);
		CHECK_RES_GOTO_ON_ERROR(nvpi3_res, "failure - closing node", failure);
	}

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_GOTO_ON_ERROR(nvpi3_res, "failure - closing db", failure);

	DBGNF("success - get_value_sizes_and_values_two_node_handles");
	return TC_PASSED;

failure:
	if (value_buff != NULL)
		free((union nvpi3_key_value *)value_buff);

	DBGNF("failure - get_value_sizes_and_values_two_node_handles");
	return TC_FAILED;
}

/*
 * get_value_sizes_and_values_two_node_handles_loop
 *
 * Read keys values according to following loop:
 * Opens a data base, "sys_bpar", opens two node, gets keys' value sizes
 * and then gets actual values. Checks that value is as expected.
 */
enum tc_result get_value_sizes_and_values_two_node_handles_loop(void)
{
	enum tc_result tc_res, ix;

	for (ix = 0; ix < TEST_MAX_LOOPS; ix++) {
		tc_res = get_value_sizes_and_values_two_node_handles();
		if (tc_res != TC_PASSED) {
			printf("failure - tc:get_values_two_node_handles_loop");
			return TC_FAILED;
		}
	}

	DBGNF("success - get_value_sizes_and_values_two_node_handles_loop");
	return TC_PASSED;
}

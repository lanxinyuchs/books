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

#include <pdb_test_data.h>

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
 * open_close_nodes_two_dbs
 *
 * Opens data bases. e.g opens the same node twice.
 * Checks that there are different node_handles returned from NVPI3, one per
 * opened node. Closes the nodes in different order compared to the opening
 * order.FInally closes the opened data base.
 */
enum tc_result open_close_nodes_same_db(void)
{
	nvpi3_db_group_handle db_group_handle;
	nvpi3_node_handle node_handle[4], node_handle1;
	uint32_t nvpi3_res;

	FUNC;

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	node_handle[0] = open_node(db_group_handle, root_a_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	node_handle[1] = open_node(db_group_handle, root_a_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	if (node_handle[0] == node_handle[1]) {
		printf("failure - same node handle ID twice");
		return TC_FAILED;
	}

	node_handle[2] = open_node(db_group_handle, root_bbb_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	if ((node_handle[2] == node_handle[0]) ||
	    (node_handle[2] == node_handle[1])) {
		printf("failure - same node handle ID twice");
		return TC_FAILED;
	}

	node_handle[3] = open_node(db_group_handle, root_top_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");
	if ((node_handle[3] == node_handle[0]) ||
	    (node_handle[3] == node_handle[1]) ||
	    (node_handle[3] == node_handle[2])) {
		printf("failure - same node handle ID twice");
		return TC_FAILED;
	}

	close_node(node_handle[1], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_node(node_handle[0], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_node(node_handle[3], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_node(node_handle[2], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing db");

	DBGNF("success - open_close_nodes_same_db");
	return TC_PASSED;
}

/*
 * open_close_nodes_two_dbs
 *
 * Opens data two bases, both "sys_bpar" . Opens nodes, e.g.the same node in
 * each data base. * Checks that there are different node_handles returned
 * from NVPI3, one per opened node. Closes the nodes in different order
 * compared to the opening order. */
enum tc_result open_close_nodes_two_dbs(void)
{
	nvpi3_db_group_handle db_group_handle[2];
	nvpi3_node_handle node_handle[4];
	uint32_t nvpi3_res;

	FUNC;

	db_group_handle[0] = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	db_group_handle[1] = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");
	DBG("db_group_handle[1]=%d", db_group_handle[1]);

	node_handle[0] = open_node(db_group_handle[0], root_a_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	node_handle[1] = open_node(db_group_handle[1], root_a_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	if (node_handle[0] == node_handle[1]) {
		printf("failure - same node_handle ID twice");
		return TC_FAILED;
	}

	node_handle[2] = open_node(db_group_handle[1], root_bbb_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	if ((node_handle[2] == node_handle[0]) ||
	    (node_handle[2] == node_handle[1])) {
		printf("failure - same node_handle ID twice");
		return TC_FAILED;
	}

	node_handle[3] = open_node(db_group_handle[0], root_top_node,
	                           &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening node");

	if ((node_handle[3] == node_handle[0]) ||
	    (node_handle[3] == node_handle[1]) ||
	    (node_handle[3] == node_handle[2])) {
		printf("failure - same node_handle ID twice");
		return TC_FAILED;
	}

	close_node(node_handle[1], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_node(node_handle[0], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_node(node_handle[3], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_node(node_handle[2], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing node");

	close_db_group(db_group_handle[1], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing db");

	close_db_group(db_group_handle[0], &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing db");

	DBGNF("success - open_close_node_two_dbs");
	return TC_PASSED;
}

/*
 * open_close_nodes_two_dbs_loop
 *
 * Tests opening/closing of nodes. In each test loop:
 * Opens data two bases, both "sys_bpar". Opens the same node in each data base.
 * Checks that there are different node_handles returned from NVPI3, one per
 * opened node. Closes the nodes in different order compared to the opening
 * order. */
enum tc_result open_close_nodes_two_dbs_loop(void)
{
	uint32_t ix;
	enum tc_result tc_res;

	for (ix = 0; ix < TEST_MAX_LOOPS; ix++) {
		tc_res = open_close_nodes_two_dbs();
		if (tc_res != TC_PASSED) {
			printf("failure - opening/closing db");
			return TC_FAILED;
		}
	}

	return TC_PASSED;
}

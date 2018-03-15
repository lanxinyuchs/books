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
#include <errno.h>
#include <stdbool.h>
#include <string.h>

#include <itc.h>

#include <nvpi3_common.h>
#include <nvpi3.h>
#include <nvpi3_cfg.h>

#include <log.h>

#include <pdb_tcs_nvpi3_normal.h>
#include <pdb_test_nvpi3_cfg.h>
#include <pdb_test_misc.h>

#define RUN_TEST_EXIT_ON_ERROR(tc_func) \
	do{if (tc_func() != TC_PASSED) \
			exit (1);\
	} while(0)

#define CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, tc_res, str)   \
	do{if (tc_res != TC_PASSED){\
			fprintf(stdout, str);                   \
			fprintf(stdout, "\n");                   \
			test_remove_mailbox_if(my_mbox_id);     \
			exit(1);                        \
		} }while(0)

#define CHECK_TC_RES_RETURN_ON_ERROR(tc_res, str)\
	do{if (tc_res != TC_PASSED){\
			fprintf(stderr, str); \
			return TC_FAILED;\
		} }while(0)

#define TEST_MAX_NO_OF_MAILBOXES         4
#define TEST_MAILBOX_NAME               "paramdb_test"
#define TEST_TIMEOUT                     1000

typedef struct {
	uint32_t specific_data;
} client_data_t;

enum supported_cmds {TC0_N, TC1_N, TC2_N, TC3_N, TC4_N, TC5_N,
		     TC1_E,
		     TEST_ALL, TEST_UNSUPPORTED};

static uint32_t get_test_group(char *tc_group)
{
	FUNC;

	DBG("requested tc_group=%s", tc_group);

	if (strcmp(tc_group, "tc0_n") == 0)
		return TC0_N;

	if (strcmp(tc_group, "tc1_n") == 0)
		return TC1_N;

	if (strcmp(tc_group, "tc2_n") == 0)
		return TC2_N;

	if (strcmp(tc_group, "tc3_n") == 0)
		return TC3_N;

	if (strcmp(tc_group, "tc4_n") == 0)
		return TC4_N;

	if (strcmp(tc_group, "tc5_n") == 0)
		 return TC5_N;

	if (strcmp(tc_group, "tc1_e") == 0)
		return TC1_E;

	if (strcmp(tc_group, "tc_all") == 0)
		return TEST_ALL;

	printf("Unsupported test case\n");
	return TEST_UNSUPPORTED;

}

enum tc_result run_all_tests(void)
{
	FUNC;

	DBGNF("**create_destroy_db_group**\n");
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_group_db);

	DBGNF("**create_destroy_db_group_dbs**\n");
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_group_dbs);

	DBGNF("**create_open_destroy_db_groups**\n");
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_db);

	DBGNF("**create_destroy_db_groups_db_loop**\n");
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_db_loop);

	DBGNF("**create_open_destroy_db_groups_dbs**\n");
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_dbs);

	DBGNF("**create_destroy_db_groups_dbs_loop**\n");
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_dbs_loop);

	DBGNF("**create_open_destroy_db_group**\n");
	RUN_TEST_EXIT_ON_ERROR(create_open_destroy_db_group);

	DBGNF("**open_close_db_group**\n");
	RUN_TEST_EXIT_ON_ERROR(open_close_db_group);

	DBGNF("**open_close_dbs**\n");
	RUN_TEST_EXIT_ON_ERROR(open_close_dbs);

	DBGNF("**open_close_dbs_loop**\n");
	RUN_TEST_EXIT_ON_ERROR(open_close_dbs_loop);

	DBGNF("**open_close_node_same_db**\n");
	RUN_TEST_EXIT_ON_ERROR(open_close_nodes_same_db);

	DBGNF("**open_close_node_two_dbs**\n");
	RUN_TEST_EXIT_ON_ERROR(open_close_nodes_two_dbs);

	DBGNF("**open_close_node_two_dbs_loop**\n");
	RUN_TEST_EXIT_ON_ERROR(open_close_nodes_two_dbs_loop);

	DBGNF("**get_value_sizes_one_node_handle**\n");
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_one_node_handle);

	DBGNF("**get_value_sizes_two_node_handles**\n");
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_two_node_handles);

	DBGNF("**get_value_sizes_two_node_handles_loop**\n");
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_two_node_handles_loop);

	DBGNF("**get_values_one_node_handle**\n");
	RUN_TEST_EXIT_ON_ERROR(get_values_one_node_handle);

	DBGNF("**get_values_two_node_handles**\n");
	RUN_TEST_EXIT_ON_ERROR(get_values_two_node_handles);

	DBGNF("**get_values_two_node_handles_loop**");
	RUN_TEST_EXIT_ON_ERROR(get_values_two_node_handles_loop);

	DBGNF("**get_value_sizes_and_values_two_node_handles**\n");
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_and_values_two_node_handles);

	DBGNF("**get_value_sizes_and_values_two_node_handles_loo**p\n");
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_and_values_two_node_handles_loop);

	/* continue adding all tests here */
	DBGNF("SUCCESS - TC_ALL");
	printf("SUCCESS - TC_ALL\n");
	return TC_PASSED;
}

enum tc_result run_tc0_test(char *cmd)
{
	enum tc_result tc_res;
	char *res_str;

	FUNC;

	if (strcmp(cmd, "create_def_db") == 0) {
		DBGNF(" *** START TEST DO NOTHING *** ");
		return TC_PASSED;
	}

	/* Run single test case only */
	if (strcmp(cmd, "create_dbg_db") == 0) {
		DBGNF(" *** START TEST create_destroy_db_group ***");
		tc_res = create_destroy_db_group_db();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - create_dbg_db");
		return tc_res;
	}

	if (strcmp(cmd, "create_dbg_dbs") == 0) {
		DBGNF(" *** START TEST create_destroy_dbg_dbs ***");
		tc_res = create_destroy_db_group_dbs();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - create_dbg_dbs");
		return tc_res;
	}

	if (strcmp(cmd, "create_dbgs") == 0) {
		DBGNF(" *** START TEST create_destroy_db_groups ***");
		tc_res = create_destroy_db_groups_db();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - dbgs");
		return tc_res;
	}

	if (strcmp(cmd, "create_dbgs_loop") == 0) {
		DBGNF(" *** START TEST create_destroy_db_groups_loop ***");
		tc_res = create_destroy_db_groups_db_loop();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - create_dbgs_loop");
		return tc_res;
	}

	if (strcmp(cmd, "create_dbgs_dbs") == 0) {
		DBGNF(" *** START TEST create_destroy_db_groups_dbs ***");
		tc_res = create_destroy_db_groups_dbs();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - create_dbgs_dbs");
		return tc_res;
	}

	if (strcmp(cmd, "create_dbgs_dbs_loop") == 0) {
		DBGNF(" *** START TEST create_destroy_db_groups_dbs_loop ***");
		tc_res = create_destroy_db_groups_dbs_loop();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - create_dbs_dbs_loop");
		return tc_res;
	}

	if (strcmp(cmd, "create_open_dbg") == 0) {
		DBGNF(" *** START TEST create_open_destroy_db_group ***");
		tc_res = create_open_destroy_db_group();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - create_destroy_db_group");
		return tc_res;
	}

	if (strcmp(cmd, "open_dbg") == 0) {
		DBGNF(" *** START TEST open_close_db_group ***");
		tc_res = open_close_db_group();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - open_close_db_group");
		return tc_res;
	}

	if (strcmp(cmd, "open_dbgs") == 0) {
		DBGNF(" *** START TEST open_close_dbs ***");
		tc_res = open_close_dbs();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - open_close_dbs");
		return tc_res;
	}

	if (strcmp(cmd, "open_dbgs_loop") == 0) {
		DBGNF(" *** START TEST open_close_dbs ***");
		tc_res = open_close_dbs_loop();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n ");
		DBGNF("SUCCESS - open_close_dbs_loop");
		return tc_res;
	}

	if (strcmp(cmd, "open_nodes_same_db") == 0) {
		DBGNF(" *** START TEST open_close_nodes_same_db ***");
		tc_res = open_close_nodes_same_db();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS - open_close_nodes_same_db");
		return tc_res;
	}

	if (strcmp(cmd, "open_nodes_two_dbs") == 0) {
		DBGNF(" *** START TEST open_close_nodes_two_dbs ***");
		tc_res = open_close_nodes_two_dbs();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n ");
		DBGNF("SUCCESS - open_close_nodes_two_dbs");
		return tc_res;
	}

	if (strcmp(cmd, "open_nodes_two_dbs_loop") == 0) {
		DBGNF(" *** START TEST open_close_nodes_two_dbs ***");
		tc_res = open_close_nodes_two_dbs_loop();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS open_close_nodes_two_dbs_loop");
		return tc_res;
	}

	if (strcmp(cmd, "sizes_one_node") == 0) {
		DBGNF(" *** START TEST get_value_sizes_one_node_handle ***");
		tc_res = get_value_sizes_one_node_handle();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS get_value_sizes_one_node_handle tc_res=%s");
		return tc_res;
	}

	if (strcmp(cmd, "sizes_two_nodes") == 0) {
		DBGNF(" *** START TEST get_value_sizes_two_node_handles ***");
		tc_res = get_value_sizes_two_node_handles();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n ");
		DBGNF("SUCCESS get_value_sizes_two_node_handles");
		return tc_res;
	}

	if (strcmp(cmd, "sizes_two_nodes_loop") == 0) {
		DBGNF(" *** START TEST get_value_sizes_two_node_handles_loop ***");
		tc_res = get_value_sizes_two_node_handles_loop();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS get_value_sizes_two_node_handles_loop");
		return tc_res;
	}

	if (strcmp(cmd, "values_one_node") == 0) {
		DBGNF(" *** START TEST get_values_one_node ***");
		tc_res = get_values_one_node_handle();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n");
		DBGNF("SUCCESS get_values_one_node");
		return tc_res;
	}

	if (strcmp(cmd, "values_two_nodes") == 0) {
		uint32_t ix;
		DBGNF(" *** START TEST get_values_two_nodes ***");
		tc_res = get_values_two_node_handles();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n ");
		DBGNF("SUCCESS get_values_two_nodes");
		return tc_res;
	}

	if (strcmp(cmd, "values_two_nodes_loop") == 0) {
		DBGNF(" *** START TEST get_values_two_nodes_loop ***");
		tc_res = get_values_two_node_handles_loop();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n ");
		DBGNF("SUCCESS get_values_two_nodes_loop");
		return tc_res;
	}

	if (strcmp(cmd, "sizes_and_value_two_nodes") == 0) {
		DBGNF(" *** START TEST sizes_and_values_two_nodes ***");
		tc_res = get_value_sizes_and_values_two_node_handles();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n ");
		DBGNF("SUCCESS sizes_and_values_two_nodes");
		return tc_res;
	}

	if (strcmp(cmd, "sizes_and_value_two_nodes_loop") == 0) {
		DBGNF(" *** START TEST sizes_and_values_two_nodes ***");
		tc_res = get_value_sizes_and_values_two_node_handles_loop();
		CHECK_TC_RES_RETURN_ON_ERROR(tc_res, "failure - tc0_n ");
		DBGNF("SUCCESS sizes_and_values_two_nodes");
		return tc_res;
	}

	printf("FAILURE - unsupported tc");
	return TC_FAILED;
}

enum tc_result run_tc1_tests(void)
{
	FUNC;
	RUN_TEST_EXIT_ON_ERROR(open_close_db_group);
	RUN_TEST_EXIT_ON_ERROR(open_close_dbs);
	RUN_TEST_EXIT_ON_ERROR(open_close_dbs_loop);

	RUN_TEST_EXIT_ON_ERROR(open_close_nodes_same_db);
	RUN_TEST_EXIT_ON_ERROR(open_close_nodes_two_dbs);
	RUN_TEST_EXIT_ON_ERROR(open_close_nodes_two_dbs_loop);
	/* Continue adding normal test for opening/closing data base
	   and handle */
	DBGNF("SUCCESS - TC1_N");
	printf("SUCCESS - TC1_N\n");
	return TC_PASSED;
}

enum tc_result run_tc2_tests(void)
{
	FUNC;
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_one_node_handle);
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_two_node_handles);
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_two_node_handles_loop);
	/* Continue adding normal tests for geting value size */
	DBGNF("SUCCESS - TC2_N");
	printf("SUCCESS - TC2_N\n");
	return TC_PASSED;
}

enum tc_result run_tc3_tests(void)
{
	FUNC;
	RUN_TEST_EXIT_ON_ERROR(get_values_one_node_handle);
	RUN_TEST_EXIT_ON_ERROR(get_values_two_node_handles);
	RUN_TEST_EXIT_ON_ERROR(get_values_two_node_handles_loop);
	/* Continue adding normal tests for geting values */
	DBGNF("SUCCESS - TC3_N");
	printf("SUCCESS - TC3_N\n");
	return TC_PASSED;
}

enum tc_result run_tc4_tests(void)
{
	FUNC;
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_and_values_two_node_handles);
	RUN_TEST_EXIT_ON_ERROR(get_value_sizes_and_values_two_node_handles_loop);
	/* Continue adding normal tests for "geting value sizes and values" */
	DBGNF("SUCCESS - TC4_N");
	printf("SUCCESS - TC4_N\n");
	return TC_PASSED;
}

enum tc_result run_tc5_tests(void)
{
	FUNC;
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_group_db);
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_group_dbs);
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_db);
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_db_loop);
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_dbs);
	RUN_TEST_EXIT_ON_ERROR(create_destroy_db_groups_dbs_loop);
/*	RUN_TEST_EXIT_ON_ERROR(create_open_destroy_db_group); */
        /* Continue adding normal tests for "geting value sizes and values" */
	DBGNF("SUCCESS - TC5_N");
	printf("SUCCESS - TC5_N\n");
	return TC_PASSED;
}

enum tc_result run_tc1_error_tests(void)
{
	FUNC;
	/* continue adding error tests for opening/closing data base node */
	DBGNF("SUCCESS - tC1_E");
	printf("SUCCESS - TC1_E\n");
	return TC_PASSED;
}

static uint32_t create_sys_bpar(nvpi3_db_group_handle *created_dbg)
{
	struct nvpi3_db_definition definition[1];
	uint32_t no_of_defs = 1;
	nvpi3_result_t res;

	definition[0].format = NVPI3_DB_FORMAT_FTD;
	definition[0].permission = NVPI3_DB_PERMISSION_RO;
	definition[0].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[0].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[0].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[0].storage.file.name, "main.dtb");
	definition[0].commit.user_data = NULL;
	definition[0].commit.callback = NULL;

	*created_dbg = create_db_group("sys_bpar", no_of_defs,
				       definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating def_db");

	DBG("Pdbtest: created dbg_handle=%u", *created_dbg);
	return res;
}

static uint32_t destroy_def_db(nvpi3_db_group_handle created_dbg)
{
	nvpi3_result_t res;

	DBG("Pdbtest: destroy dbg_handle=%u", created_dbg);


	destroy_db_group(created_dbg, &res);
       	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying def_db");

	DBG("Pdbtest: def_db destryed = %d", created_dbg);
	return res;
}

int main(int argc, char *argv[])
{
	uint32_t res;
	uint32_t cmd_cnt = 1;

	itc_mbox_id_t my_mbox_id = ITC_NO_ID;
	enum supported_cmds tc;
	uint32_t ix = 0;
	nvpi3_db_group_handle created_dbg;

	FUNC;

	DBG("no of arguments, argc=%d", argc);
	for (ix = 0; ix < argc; ix++)
		DBG("argv[%u]=%s", ix, argv[ix]);

	if(!test_init_mailbox_if(&my_mbox_id)) {
		printf("initiaing mailbox failure");
		exit(1);
	}
	DBG("mailbox created, PDBtest: my_mbox_id=%d", my_mbox_id);

	/* create default db */
	res = create_sys_bpar(&created_dbg);
	if (res != NVPI3_RESULT_SUCCESS)
		exit(1);
	DBG("PDBtest: created dbg_handle=%u", created_dbg);

	while (cmd_cnt < argc) {
		tc = get_test_group(argv[cmd_cnt]);

		switch (tc) {
		case TC0_N:
			/* single test only */
			if (argc < 3) {
				printf("Too few parameters provided for test case");
				test_remove_mailbox_if(my_mbox_id);
				exit(1);
			}
			res = run_tc0_test(argv[cmd_cnt + 1]);
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC0_N");

			DBGNF(" ****** END OF TC0_N *****");
			res = destroy_def_db(created_dbg);
			if (res != NVPI3_RESULT_SUCCESS) {
				DBGNF("PDBtest: failure destroying db");
				exit(1);
			}
			test_remove_mailbox_if(my_mbox_id);
			printf("requested test/tests PASSED");
			DBGNF(" ****** END OF TC *****");
			exit(0);

		case TC1_N:
			res = run_tc1_tests();
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC1_N");
			break;

		case TC2_N:
			res = run_tc2_tests();
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC2_N");
			break;

		case TC3_N:
			res = run_tc3_tests();
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC3_N");
			break;

		case TC4_N:
			res = run_tc4_tests();
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC4_N");
			break;

		case TC5_N:
			res = run_tc5_tests();
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC5_N");
			break;

		case TC1_E:
			res = run_tc1_error_tests();
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC1_E");
			break;

		case TEST_ALL:
			res = run_all_tests();
			CHECK_TC_RES_EXIT_ON_ERROR(my_mbox_id, res,
			                           "FAILURE - TC1_E");
			res = destroy_def_db(created_dbg);
			test_remove_mailbox_if(my_mbox_id);
			fprintf(stdout, "TC_ALL PASSED\n");
			exit(0);


		case TEST_UNSUPPORTED:
			fprintf(stdout, "FAILURE - test isn't supported\n");
			res = destroy_def_db(created_dbg);
			test_remove_mailbox_if(my_mbox_id);
			DBGNF(" ****** END OF TC *****");
			exit(1);

		default: /* Shouldn't occurre */
			fprintf(stdout, "FAILURE - SW-error, cmd=%s",
				argv[cmd_cnt]);
			res = destroy_def_db(created_dbg);
			test_remove_mailbox_if(my_mbox_id);
			exit(1);
		}

		cmd_cnt++;
	}

	DBGNF(" ****** END OF TC *****");
	res = destroy_def_db(created_dbg);
	if (res != NVPI3_RESULT_SUCCESS) {
		DBGNF("PDBtest: failure destroying db");
		exit(1);

	}
	test_remove_mailbox_if(my_mbox_id);
	fprintf(stdout, "requested test/tests PASSED\n");
	exit(0);
}

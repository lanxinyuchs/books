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
#include <nvpi3_cfg.h>

#include <log.h>

#include <pdb_test_nvpi3.h>
#include <pdb_test_nvpi3_cfg.h>
#include <pdb_test_misc.h>
#include <pdb_test_data.h>

#define MAX_KEY_NAME_LEN 60
#define MAX_VALUE_BUFF_SIZE 150

#define db_name     "REMBRANDT"

/*
 * create the simpliest data base and open it.
 * Fianlly destroy it.
 */
enum tc_result create_open_destroy_db_group(void)
{
	struct nvpi3_db_definition definition[1];
	nvpi3_db_group_handle db_group_handle_create;
	nvpi3_db_group_handle db_group_handle_open;
	nvpi3_result_t res, no_of_defs;

	FUNC;

	no_of_defs = 1;
	definition[0].format = NVPI3_DB_FORMAT_FTD;
	definition[0].permission = NVPI3_DB_PERMISSION_RO;
	definition[0].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[0].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[0].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[0].storage.file.name, "main.dtb");
	definition[0].commit.user_data = NULL;
	definition[0].commit.callback = NULL;

	db_group_handle_create = create_db_group(db_name, no_of_defs,
						 definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating db");
	fprintf(stdout, "PDBtest:db_group_handle_create=%d\n",
		db_group_handle_create);

	db_group_handle_open = open_db_group(db_name, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - opening db");

	close_db_group(db_group_handle_open, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - opening db");

	destroy_db_group(db_group_handle_create, &res);
       	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying db");

	DBGNF("success - create, open and destroy db_group");
	return TC_PASSED;
}



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

/*
 * create and destroy the simpliest data base group, one data base only
 */
enum tc_result create_destroy_db_group_db(void)
{
	struct nvpi3_db_definition definition[1];
	nvpi3_db_group_handle db_group_handle;
	uint32_t res, no_of_defs;

	FUNC;

	fprintf(stdout, "Pdbtest: create_destroy_db_group 0\n");

	no_of_defs = 1;
	definition[0].format = NVPI3_DB_FORMAT_FTD;
	definition[0].permission = NVPI3_DB_PERMISSION_RO;
	definition[0].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[0].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[0].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[0].storage.file.name, "rembrandt.dtb");
	definition[0].commit.user_data = NULL;
	definition[0].commit.callback = NULL;

	db_group_handle = create_db_group("REMBRANDT", no_of_defs,
					  definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating REMBRANDT");
	fprintf(stdout, "Pdbtest: db_group_handle=%d\n", db_group_handle);

	res = nvpi3_destroy_db_group(db_group_handle);
       	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying REMBRANDT");

	DBGNF("success - create_destroy_db_group");
	return TC_PASSED;
}

enum tc_result create_destroy_db_group_dbs(void)
{
	struct nvpi3_db_definition definition[4];
	nvpi3_db_group_handle db_group_handle;
	uint32_t res, no_of_defs;

	FUNC;

	fprintf(stdout, "Pdbtest: create_destroy_db_group_dbs\n");

	no_of_defs = 1;
	definition[0].format = NVPI3_DB_FORMAT_FTD;
	definition[0].permission = NVPI3_DB_PERMISSION_RO;
	definition[0].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[0].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[0].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[0].storage.file.name, "rembrandt.dtb");
	definition[0].commit.user_data = NULL;
	definition[0].commit.callback = NULL;

	no_of_defs++;
	definition[1].format = NVPI3_DB_FORMAT_FTD;
	definition[1].permission = NVPI3_DB_PERMISSION_RW;
	definition[1].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[1].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[1].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[1].storage.file.name, "davinci.dtb");
	definition[1].commit.user_data = NULL;
	definition[1].commit.callback = NULL;

	no_of_defs++;
	definition[2].format = NVPI3_DB_FORMAT_FTD;
	definition[2].permission = NVPI3_DB_PERMISSION_WO;
	definition[2].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[2].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[2].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[2].storage.file.name, "michelangelo.dtb");
	definition[2].commit.user_data = NULL;
	definition[2].commit.callback = NULL;

	no_of_defs++;
	definition[3].format = NVPI3_DB_FORMAT_FTD;
	definition[3].permission = NVPI3_DB_PERMISSION_RO;
	definition[3].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[3].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[3].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[3].storage.file.name, "main.dtb");
	definition[3].commit.user_data = NULL;
	definition[3].commit.callback = NULL;

	db_group_handle = create_db_group("GREAT_ARTISTS", no_of_defs,
					  definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating GREAT_ARTISTS");
	fprintf(stdout, "Pdbtest: db_group_hande=%d\n", db_group_handle);

	fprintf(stdout, "Pdbtest: destroy db_group_hande=%d\n",
		db_group_handle);
	res = nvpi3_destroy_db_group(db_group_handle);
       	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying GREAT_ARTISTS");

	DBGNF("success - create_destroy_db_group");
	return TC_PASSED;
}


/*
 * create and destroy three different data base group, containing one data base
 * each
 */
enum tc_result create_destroy_db_groups_db(void)
{
	struct nvpi3_db_definition definition;
	nvpi3_db_group_handle db_group_handle[3];
	uint32_t res;
	uint32_t no_of_defs = 1;

	FUNC;

	fprintf(stdout, "Pdbtest: create_destroy_db_group 0\n");

	definition.format = NVPI3_DB_FORMAT_FTD;
	definition.permission = NVPI3_DB_PERMISSION_RO;
	definition.storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition.storage.file.offset = 0; /* There is no LM/LMC header */
	definition.storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition.storage.file.name, "rembrandt.dtb");
	definition.commit.user_data = NULL;
	definition.commit.callback = NULL;
	db_group_handle[0] = create_db_group("REMBRANDT", no_of_defs,
					     &definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating REMBRANDT");
	fprintf(stdout, "Pdbtest: db_group_handle[0]=%d\n", db_group_handle[0]);

	definition.format = NVPI3_DB_FORMAT_FTD;
	definition.permission = NVPI3_DB_PERMISSION_RW;
	definition.storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition.storage.file.offset = 0; /* There is no LM/LMC header */
	definition.storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition.storage.file.name, "davinci.dtb");
	definition.commit.user_data = NULL;
	definition.commit.callback = NULL;
	db_group_handle[1] = create_db_group("DAVINCI", no_of_defs,
					     &definition, &res);
	fprintf(stdout, "Pdbtest: db_group_handle[1]=%d\n", db_group_handle[1]);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating DAVINCI");

	if (db_group_handle[0] == db_group_handle[1])
		return TC_FAILED;

	definition.format = NVPI3_DB_FORMAT_FTD;
	definition.permission = NVPI3_DB_PERMISSION_WO;
	definition.storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition.storage.file.offset = 0; /* There is no LM/LMC header */
	definition.storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition.storage.file.name, "michelangelo.dtb");
	definition.commit.user_data = NULL;
	definition.commit.callback = NULL;
	db_group_handle[2] = create_db_group("MICHELANGELO", no_of_defs,
					     &definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating MICHELANGELO");
	fprintf(stdout, "Pdbtest: db_group_handle[2]=%d\n", db_group_handle[2]);

	if ((db_group_handle[2] == db_group_handle[0]) ||
	    (db_group_handle[2] == db_group_handle[1]))
		return TC_FAILED;

	fprintf(stdout, "Pdbtest: destroy db_group_handle[1]=%d\n",
		db_group_handle[1]);
	res = nvpi3_destroy_db_group(db_group_handle[1]);
    	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying DAVINCI");

	fprintf(stdout, "Pdbtest: destroy db_group_handle[0]=%d\n",
		db_group_handle[0]);
	res = nvpi3_destroy_db_group(db_group_handle[0]);
      	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying REMBRANDT");

	fprintf(stdout, "Pdbtest: destroy db_group_handle[2]=%d\n",
		db_group_handle[2]);
	res = nvpi3_destroy_db_group(db_group_handle[2]);
      	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying MICHELANGELO");

	DBGNF("success - create_destroy_db_groups");
	return TC_PASSED;
}


/*
 * create and destroy the simpliest data bases
*/
enum tc_result create_destroy_db_groups_db_loop(void)
{
	enum tc_result tc_res;
	uint32_t ix;

	FUNC;
	for (ix=0; ix < TEST_MAX_LOOPS; ix++){
		tc_res = create_destroy_db_groups_db();
		if (tc_res != TC_PASSED){
			printf("failure - creating/destroyng db group");
			return TC_FAILED;
		}
	}

	DBGNF("success - creating/destroyng db group");
	return TC_PASSED;
}
enum tc_result create_destroy_db_groups_dbs(void)
{
	struct nvpi3_db_definition definition[3];
	nvpi3_db_group_handle db_group_handle[3];
	uint32_t res;
	uint32_t no_of_defs;

	FUNC;

	fprintf(stdout, "Pdbtest: create_destroy_db_group 0\n");

	/* *********************************************** */
	no_of_defs=1;
	definition[0].format = NVPI3_DB_FORMAT_FTD;
	definition[0].permission = NVPI3_DB_PERMISSION_RO;
	definition[0].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[0].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[0].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[0].storage.file.name, "main.dtb");
	definition[0].commit.user_data = NULL;
	definition[0].commit.callback = NULL;

	no_of_defs++;
	definition[1].format = NVPI3_DB_FORMAT_FTD;
	definition[1].permission = NVPI3_DB_PERMISSION_WO;
	definition[1].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[1].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[1].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[1].storage.file.name, "rembrandt.dtb");
	definition[1].commit.user_data = NULL;
	definition[1].commit.callback = NULL;

	no_of_defs++;
	definition[2].format = NVPI3_DB_FORMAT_FTD;
	definition[2].permission = NVPI3_DB_PERMISSION_RW;
	definition[2].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[2].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[2].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[2].storage.file.name, "davinci.dtb");
	definition[2].commit.user_data = NULL;
	definition[2].commit.callback = NULL;

	db_group_handle[0] = create_db_group("REMBRANDT&DAVINCE", no_of_defs,
					     definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating GREAT_ARTISTS");
	fprintf(stdout, "Pdbtest: db_group_handle[0]=%d\n", db_group_handle[0]);

	/* *********************************************** */
	no_of_defs=1;
	definition[0].format = NVPI3_DB_FORMAT_FTD;
	definition[0].permission = NVPI3_DB_PERMISSION_RO;
	definition[0].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[0].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[0].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[0].storage.file.name, "main.dtb");
	definition[0].commit.user_data = NULL;
	definition[0].commit.callback = NULL;

	no_of_defs++;
	definition[1].format = NVPI3_DB_FORMAT_FTD;
	definition[1].permission = NVPI3_DB_PERMISSION_WO;
	definition[1].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[1].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[1].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[1].storage.file.name, "rembrandt.dtb");
	definition[1].commit.user_data = NULL;
	definition[1].commit.callback = NULL;

	no_of_defs++;
	definition[2].format = NVPI3_DB_FORMAT_FTD;
	definition[2].permission = NVPI3_DB_PERMISSION_RW;
	definition[2].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[2].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[2].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[2].storage.file.name, "michelangelo.dtb");
	definition[2].commit.user_data = NULL;
	definition[2].commit.callback = NULL;

	db_group_handle[1] = create_db_group("REMBRANDT&MICHELANGELO",
					     no_of_defs, definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating DAVINCI");
	fprintf(stdout, "Pdbtest: db_group_handle[1]=%d\n", db_group_handle[1]);

	if (db_group_handle[1] == db_group_handle[0])
		return TC_FAILED;

	/* *********************************************** */
	no_of_defs = 1;
	definition[0].format = NVPI3_DB_FORMAT_FTD;
	definition[0].permission = NVPI3_DB_PERMISSION_RO;
	definition[0].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[0].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[0].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[0].storage.file.name, "main.dtb");
	definition[0].commit.user_data = NULL;
	definition[0].commit.callback = NULL;

	no_of_defs++;
	definition[1].format = NVPI3_DB_FORMAT_FTD;
	definition[1].permission = NVPI3_DB_PERMISSION_WO;
	definition[1].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[1].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[1].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[1].storage.file.name, "davinci.dtb");
	definition[1].commit.user_data = NULL;
	definition[1].commit.callback = NULL;

	no_of_defs++;
	definition[2].format = NVPI3_DB_FORMAT_FTD;
	definition[2].permission = NVPI3_DB_PERMISSION_RW;
	definition[2].storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	definition[2].storage.file.offset = 0; /* There is no LM/LMC header */
	definition[2].storage.file.max_size = 0; /* unlimited db size */
	strcpy(definition[2].storage.file.name, "michelangelo.dtb");
	definition[2].commit.user_data = NULL;
	definition[2].commit.callback = NULL;

	db_group_handle[2] = create_db_group("DAVINCI&MICHELANGELO", no_of_defs,
					     definition, &res);
	CHECK_RES_RETURN_ON_ERROR(res, "failure - creating MICHELANGELO");
	fprintf(stdout, "Pdbtest: db_group_handle[2]=%d\n", db_group_handle[2]);

	if ((db_group_handle[2] == db_group_handle[0]) ||
	    (db_group_handle[2] == db_group_handle[1]))
		return TC_FAILED;

	/* *********************************************** */
	fprintf(stdout, "Pdbtest: destroy db_group_handle[1]=%d\n",
		db_group_handle[1]);
	res = nvpi3_destroy_db_group(db_group_handle[1]);
    	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying "
				  "REMBRANDT&MICHELANGELO");

	fprintf(stdout, "Pdbtest: destroy db_group_handle[0]=%d\n",
		db_group_handle[0]);
	res = nvpi3_destroy_db_group(db_group_handle[0]);
      	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying "
				  "REMBRANDT&DAVINCE");

	fprintf(stdout, "Pdbtest: destroy db_group_handle[2]=%d\n",
		db_group_handle[2]);
	res = nvpi3_destroy_db_group(db_group_handle[2]);
      	CHECK_RES_RETURN_ON_ERROR(res, "failure - destroying "
				  "DAVINCI&MICHELANGELO");

	DBGNF("success - create_destroy_db_groups");
	return TC_PASSED;
}


/*
 * create and destroy the simpliest data bases
*/
enum tc_result create_destroy_db_groups_dbs_loop(void)
{
	enum tc_result tc_res;
	uint32_t ix;

	FUNC;
	for (ix=0; ix < TEST_MAX_LOOPS; ix++){
		tc_res = create_destroy_db_groups_dbs();
		if (tc_res != TC_PASSED){
			printf("failure - creating/destroyng db groups dbs");
			return TC_FAILED;
		}
	}

	DBGNF("success - creating/destroyng db groups dbs");
	return TC_PASSED;
}

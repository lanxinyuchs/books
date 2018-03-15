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
 * open_close_db_group
 *
 * Opens one data base, "sys_bpar" and close it."null".
*/
enum tc_result open_close_db_group(void)
{
	nvpi3_db_group_handle db_group_handle;
	uint32_t nvpi3_res;

	FUNC;

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	DBG("PDBtests: res=%u", nvpi3_res);
	DBG("PDBtest db_group_handle=%u", (uint32_t)db_group_handle);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	DBG("PDBtest db_group_handle=%u", (uint32_t)db_group_handle);

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing def-db");

	DBGNF("success - open_close_db");
	return TC_PASSED;
}

/*
 * open_close_dbs
 *
 * Opens 3 data bases, as "db_def" and "null". Checks that there are different
 * bd_handles returned from NVPI3, one per data base. Closes the data bases in
 * different order copmered to the opening order.  */
enum tc_result open_close_dbs(void)
{
	nvpi3_db_group_handle db_group_handle, db_group_handle1,
	                      db_group_handle2;
	uint32_t nvpi3_res;

	FUNC;

	DBGNF("OPEN DB1");

	db_group_handle = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening def-db");

	DBGNF("OPEN DB2");
	db_group_handle1 = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - opening null-db");

	DBG("DB_GROUP_HANDLE=%d, DB_GROUP_HANDLE1=%d",
	    db_group_handle, db_group_handle1);
	if (db_group_handle == db_group_handle1) {
		printf("failure - same db_group_handle twice");
		return TC_FAILED;
	}

	DBGNF("OPEN DB3");

	db_group_handle2 = open_db_group("sys_bpar", &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res,
				  "failure - opening def-db a third time");

	DBG("DB_GROUP_HANDLE=%d, DB_GROUP_HANDLE1=%d, DB_GROUP_HANDLE2=%d",
	    db_group_handle, db_group_handle1, db_group_handle2);
	if ((db_group_handle2 == db_group_handle) ||
	    (db_group_handle2 == db_group_handle1)) {
		printf("failure - same db_group_handle twice");
		return TC_FAILED;
	}

	DBGNF("Separate DB-handles from NVPI3");

	close_db_group(db_group_handle1, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing data base");

	close_db_group(db_group_handle2, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing data base");

	close_db_group(db_group_handle, &nvpi3_res);
	CHECK_RES_RETURN_ON_ERROR(nvpi3_res, "failure - closing data base");

	DBGNF("success - open_close_dbs");
	return TC_PASSED;
}

/*
 * open_close_db_loop
 *
 * Opens/closes a data base several times, 10 times in total. */
enum tc_result open_close_dbs_loop(void)
{
	uint32_t ix;
	enum tc_result tc_res;

	for (ix = 0; ix < TEST_MAX_LOOPS; ix++) {
		tc_res = open_close_dbs();
		if (tc_res != TC_PASSED){
		printf("failure - opning/closing data base");
			return TC_FAILED;
		}
	}

	DBGNF("success - open_close_dbs_loop");
	return TC_PASSED;
}

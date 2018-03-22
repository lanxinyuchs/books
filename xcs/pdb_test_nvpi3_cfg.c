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
#include <nvpi3_cfg.h>

#include <pdb_test_misc.h>
#include <pdb_test_nvpi3.h>
#include <pdb_test_nvpi3_cfg.h>

nvpi3_db_group_handle create_db_group(
	const char *db_name, uint32_t no_of_defs,
	const struct nvpi3_db_definition definition[],
	nvpi3_result_t *res)
{
	nvpi3_db_group_handle db_group_handle = NULL;
	uint32_t ix = 0;

	FUNC;

	fprintf(stdout, "PDBtest:create_db_group 1\n");

	fprintf(stdout, "PDBtest:db_name=%s\n", db_name);
	fprintf(stdout, "PDBtest:no_of_defs=%u\n", no_of_defs);

	for (ix=0; ix<no_of_defs; ix++){
		fprintf(stdout, "PDBtest: definition.format=%u\n",
			definition->format);
		fprintf(stdout, "PDBtest definition.storage.file.type =%u\n",
			definition->storage.file.type);
		fprintf(stdout, "PDBtest: definition.storage.file.name=%s\n",
			definition->storage.file.name);
	}

	*res = nvpi3_create_db_group(db_name, no_of_defs, definition,
				     &db_group_handle);
	if (*res != NVPI3_RESULT_SUCCESS){
		fprintf(stdout, "pdbtest:create_db_group 2\n");
		printf("failure creating db, db_name =%s, res=%s",
		       db_name, test_nvpi3_res_to_str(*res));

	}
	fprintf(stdout, "pdbtest:create_db_group 3\n");

	/* db_group_handle = NULL if failure, != NULL if success */
	fprintf(stdout, "create_db_group SUCCESS\n");
	return db_group_handle;
}


void destroy_db_group(nvpi3_db_group_handle db_group_handle,
		      nvpi3_result_t *res)
{

	FUNC;

	DBG("db_group_handle = %d", db_group_handle);
	*res = nvpi3_destroy_db_group(db_group_handle);
	if (*res != NVPI3_RESULT_SUCCESS)
		printf("failure destroying db, db_group_handle=%d, res=%s",
		       db_group_handle, test_nvpi3_res_to_str(*res));

	return;
}

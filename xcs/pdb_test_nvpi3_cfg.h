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

#ifndef PARAMDB_TEST_NVPI3_CFG__
#define PARAMDB_TEST_NVPI3_CFG__

#include <nvpi3_cfg.h>
#include <nvpi3.h>

extern nvpi3_db_group_handle create_db_group(
	const char *db_name, uint32_t no_of_defs,
	const struct nvpi3_db_definition definition[],
	nvpi3_result_t *res);

extern void destroy_db_group(
	nvpi3_db_group_handle db_group_handle, nvpi3_result_t *res);

#endif /* PARAMDB_TEST_NVPI3_CFG__ */

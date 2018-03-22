/* > Description **************************************************************/
/**
 * @file nvpi3_cfg.h
 * @brief NVPI3 configuration interface
 *
 * This file defines the NVPI3 configuration interface.
 */
/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef NVPI3_CFG_H
#define NVPI3_CFG_H

/* > Includes *****************************************************************/
#include <stdint.h>
#include "nvpi3_common.h"
#include "nvpi3_cfg_common.h"

/* > Defines ******************************************************************/

/* > Type Declarations ********************************************************/

/* > Function Declarations ****************************************************/

/**
 * @brief Creates a database group.
 *
 * The database group can consist of a single database or several databases.
 * If database does not exist and it has write permission, i.e.
 * NVPI3_DB_PERMISSION_RW or NVPI3_DB_PERMISSION_WO, then an empty database file
 * is created at location defined by definition.storage.file.name
 *
 * @param[in]  name                Name of the database group.
 * @param[in]  num_of_definitions  Number of elements in definition array.
 * @param[in]  definition          Definitions for the databases included in the
 *                                 group. The priority in which the databases
 *                                 should be searched for nodes and keys is
 *                                 defined by their order in the array starting
 *                                 from definition[0].
 * @param[out] group_handle        Handle to created database group on success
 *                                 or NULL on failure.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded creating the database
 *                                         group.@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_ACCESS_DENIED  Database group already created.
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @post       The database group is created.
 */
nvpi3_result_t nvpi3_create_db_group(
	const char *name,
	uint32_t num_of_definitions,
	const struct nvpi3_db_definition definition[],
	nvpi3_db_group_handle *group_handle);

/**
 * @brief Destroys a database group
 *
 * @param[in] group_handle  Handle to the database group to destroy.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded destroying the database
 *                                        group.@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       The database group must have been created via
 *            nvpi3_create_db_group.
 * @post      The database group is destroyed.
 * @see       nvpi3_create_db_group
 */
nvpi3_result_t nvpi3_destroy_db_group(nvpi3_db_group_handle group_handle);

#endif /* NVPI3_CFG_H */

#ifdef __cplusplus
}
#endif

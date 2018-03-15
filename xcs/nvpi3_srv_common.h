/* > Description **************************************************************/
/**
 * @file nvpi3_srv_common.h
 * @brief NVPI3 server common header
 *
 * This file defines the NVPI3 common types.
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

#ifndef NVPI3_SRV_COMMON_H
#define NVPI3_SRV_COMMON_H

/* > Includes *****************************************************************/
#include <stdint.h>
#include "nvpi3_common.h"
#include "nvpi3_cfg_common.h"

/* > Defines ******************************************************************/

/* > Type Declarations ********************************************************/

/**
 * @brief Callback function that will be called for each node that
 *        nvpi3_srv_enum_nodes finds.
 *
 * @param[in] user_data      Application context information.
 * @param[in] database_name  Name of database.
 * @param[in] node_handle    Handle identifying found node.
 *
 * @return    NVPI3_RESULT_SUCCESS  Indicates success and that enumeration
 *                                  should continue.@n
 *            ...                   Other values indicates failure and that
 *                                  enumeration should stop.@n
 *
 * @pre       Called from nvpi3_srv_enum_nodes.
 * @see       nvpi3_srv_enum_nodes.
 */
typedef nvpi3_result_t (*nvpi3_srv_enum_nodes_callback)(
	void *user_data, const char *database_name,
	nvpi3_node_handle node_handle);

/**
 * @brief Callback function that will be called for each key that
 *        nvpi3_srv_enum_keys finds.
 *
 * @param[in] user_data      Application context information.
 * @param[in] database_name  Name of database.
 * @param[in] key_name       Full key of name starting from root node.
 * @param[in] type           Key type.
 * @param[in] type_str       String that is used to represents key type.
 * @param[in] size           Size of value in bytes.
 * @param[in] value          value.
 *
 * @return    NVPI3_RESULT_SUCCESS  Indicates success and that enumeration
 *                                  should continue.@n
 *            ...                   Other values indicates failure and that
 *                                  enumeration should stop.@n
 *
 * @pre       Called from nvpi3_srv_enum_keys.
 * @see       nvpi3_srv_enum_keys.
 */
typedef nvpi3_result_t (*nvpi3_srv_enum_keys_callback)(
	void *user_data, const char *database_name, const char *key_name,
	nvpi3_key_type_t type, const char *type_str, uint32_t size,
	const union nvpi3_key_value *value);

/**
 * @brief Callback function that will be called from for nvpi3_srv_get_value to
 *        return a key's value.
 *
 * @param[in] user_data  Application context information.
 * @param[in] size       Size of value in bytes.
 * @param[in] value      value.
 *
 * @return    NVPI3_RESULT_SUCCESS  Indicates success and that enumeration
 *                                  should continue.@n
 *            ...                   Other values indicates failure and that
 *                                  enumeration should stop.@n
 *
 * @pre       Called from nvpi3_srv_get_value.
 * @see       nvpi3_srv_get_value.
 */
typedef nvpi3_result_t (*nvpi3_srv_get_value_callback)(
	void *user_data, uint32_t size, const union nvpi3_key_value *value);

/**
 * @brief Callback function that will be called for each database group that
 *        nvpi3_srv_enum_db_groups finds.
 *
 * @param[in] user_data           Application context information.
 * @param[in] num_of_groups       The number of database groups.
 * @param[in] name                Name of the database group.
 * @param[in] num_of_definitions  Number of elements in definition array.
 * @param[in] definition          Definitions for the databases included in the
 *                                group. The priority in which the databases
 *                                should be searched for nodes and keys is
 *                                defined by their order in the array starting
 *                                from definition[0].
 *
 * @return    NVPI3_RESULT_SUCCESS  Indicates success and that enumeration
 *                                  should continue.@n
 *            ...                   Other values indicates failure and that
 *                                  enumeration should stop.@n
 *
 * @pre       Called from nvpi3_srv_enum_db_groups.
 * @see       nvpi3_srv_enum_db_groups.
 */
typedef nvpi3_result_t (*nvpi3_srv_enum_db_groups_callback)(
	void *user_data, uint32_t num_of_groups, const char *name,
	uint32_t num_of_definitions,
	const struct nvpi3_db_definition definition[]);

/* > Function Declarations ****************************************************/

#endif /* NVPI3_SRV_COMMON_H */

#ifdef __cplusplus
}
#endif

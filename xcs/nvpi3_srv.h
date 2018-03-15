/* > Description **************************************************************/
/**
 * @file nvpi3_srv.h
 * @brief NVPI3 server interface
 *
 * This file defines the NVPI3 server interface.
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

#ifndef NVPI3_SRV_H
#define NVPI3_SRV_H

/* > Includes *****************************************************************/
#include <stdint.h>
#include "nvpi3_common.h"
#include "nvpi3_cfg_common.h"
#include "nvpi3_srv_common.h"

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
 * @param[in]  client_ref          Client reference to associate to database
 *                                 group.
 * @param[in]  name                Name of the database group.
 * @param[in]  num_of_definitions  Number of elements in definition array.
 * @param[in]  definition          Definitions for the databases included in the
 *                                 group. The priority in which the databases
 *                                 should be searched for nodes and keys is
 *                                 defined by their order in the array starting
 *                                 from definition[0].
 * @param[out] cleared             Array indicating which databases that has
 *                                 been cleared due to being corrupt.
 *                                 The array must contain num_of_definitions
 *                                 elements. Only databases with
 *                                 nvpi3_db_definition::permission other than
 *                                 @ref NVPI3_DB_PERMISSION_RO can be cleared.
 * @param[out] group_handle        Handle to created database group on success
 *                                 or NULL on failure.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded creating the database
 *                                         group.@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_ACCESS_DENIED  Failed to create an empty database
 *                                         file at location defined by
 *                                         definition.storage.file.name.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @post       The database group is created.
 */
nvpi3_result_t nvpi3_srv_create_db_group(
	void *client_ref,
	const char *name,
	uint32_t num_of_definitions,
	const struct nvpi3_db_definition definition[],
	int cleared[],
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
nvpi3_result_t nvpi3_srv_destroy_db_group(nvpi3_db_group_handle group_handle);

/**
 * @brief Opens a database group.
 *
 * The database group can consist of a single database or several databases. The
 * databases are added to group via nvpi3_srv_create_db_group.
 *
 * @param[in]  client_ref    Client reference to associate to database group.
 * @param[in]  name          Name of the database group.
 * @param[out] group_handle  Handle to opened database group on success or NULL
 *                           on failure.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded opening the database group.@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_NOT_FOUND      database group not found.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre        The database group must have been created via
 *             nvpi3_srv_create_db_group.
 * @post       The database group is opened.
 * @see        nvpi3_srv_create_db_group
 */
nvpi3_result_t nvpi3_srv_open_db_group(void *client_ref, const char *name,
                                       nvpi3_db_group_handle *group_handle);


/**
 * @brief Closes a database group.
 *
 * @param[in] group_handle  Handle to the database group to close.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded closing the database group.@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.
 *
 * @pre       The database group must have been opened via
 *            nvpi3_srv_open_db_group.
 * @post      The database group is closed.
 * @see       nvpi3_srv_open_db_group
 */
nvpi3_result_t nvpi3_srv_close_db_group(nvpi3_db_group_handle group_handle);

/**
 * @brief Destroys all resorces created/opened by a client.
 *
 * @param[in] client_ref Client reference identifying client whose resources
 *                       should be destroyd.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded destroying resources.@n
 *            NVPI3_RESULT_NOT_FOUND      Clent not found.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       A database group must have been created/opened via
 *            nvpi3_srv_create_db_group/nvpi3_srv_open_db_group.
 * @post      All database groups created/opened by client are destroyed/closed.
 * @see       nvpi3_srv_create_db_group
 */
nvpi3_result_t nvpi3_srv_destroy(void *client_ref);

/**
 * @brief Opens a node in a database group.
 *
 * The priority in which the databases should be searched for the node is
 * defined when the group is created via nvpi3_srv_create_db_group. If node is
 * found a handle to it is returned. The node handle can be used to access
 * keys in that particular node or its sub nodes.
 *
 * @param[in]  group_handle  Handle to the database group.
 * @param[in]  node_name     Node name starting from root node. Node names
 *                           should be preceded by '/', e.g.
 *                           /node/sub-node1.../sub-nodeN. An empty string ("")
 *                           opens the root node.
 * @param[out] node_handle   Handle to opened node on success or NULL on
 *                           failure.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded opening node@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_NOT_FOUND      Node not found.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre        The database group must have been created/opened via
 *             nvpi3_srv_create_db_group/nvpi3_srv_open_db_group.
 * @post       The node is opened.
 * @see        nvpi3_srv_create_db_group@n
 *             nvpi3_srv_open_db_group
 */
nvpi3_result_t nvpi3_srv_open_node(nvpi3_db_group_handle group_handle,
                                   const char *node_name,
                                   nvpi3_node_handle *node_handle);

/**
 * @brief Closes a node.
 *
 * @param[in] node_handle  handle to the node to be closed.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded closing the node.@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.
 *
 * @pre       The node must have been opened via nvpi3_srv_open_node.
 * @post      The node is closed.
 * @see       nvpi3_srv_open_node
 */
nvpi3_result_t nvpi3_srv_close_node(nvpi3_node_handle node_handle);

/**
 * @brief Returns a key's value.
 *
 * The priority in which the databases should be searched for the key, is
 * defined when the group is created via nvpi3_srv_create_db_group. If the key
 * is found and caller supplied enough room in value buffer then its value is
 * returned.
 *
 * @param[in] node_handle  Handle identifying node to read from.
 * @param[in] key_name     Key name starting from node defined by node_handle.
 *                         Name may contain node names which should be preceded
 *                         by '/', e.g. /node/sub-node1.../sub-nodeN/key-name.
 * @param[in] type         Key type.
 * @param[in] callback     Callback function that will be called if key is
 *                         found. If set to NULL then only a check is if key
 *                         exist is made.
 * @param[in] user_data    Application context information to be passed back to
 *                         the callback function.
 *
 * @return    NVPI3_RESULT_SUCCESS           Succeeded reading value.@n
 *            NVPI3_RESULT_INVALID_PARAM     Invalid parameter supplied.@n
 *            NVPI3_RESULT_NOT_FOUND         Requested key is not found.@n
 *            NVPI3_RESULT_OTHER_ERROR       Other miscellaneous error.
 *
 * @pre       The database group must have been opened via
 *            nvpi3_srv_open_db_group.@n
 *            The node must have been opened via nvpi3_srv_open_node.
 * @see       nvpi3_srv_open_db_group@n
 *            nvpi3_srv_open_node
 */
nvpi3_result_t nvpi3_srv_get_value(nvpi3_node_handle node_handle,
                                   const char *key_name, nvpi3_key_type_t type,
                                   nvpi3_srv_get_value_callback callback,
                                   void *user_data);

/**
 * @brief Returns the key's value size in bytes.
 *
 * The priority in which the databases should be searched for the key, is
 * defined when the group is created via nvpi3_srv_create_db_group. If the key
 * is found then its value size in bytes is returned.
 *
 * @param[in]  node_handle  Handle identifying node to read from.
 * @param[in]  key_name     Key name starting from node defined by node_handle.
 *                          Name may contain node names which should be preceded
 *                          by '/', e.g. /node/sub-node1.../sub-nodeN/key-name.
 * @param[in]  type         Key type.
 * @param[out] size         Size of value in bytes.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded retrieving value size.
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_NOT_FOUND,     Requested key not found.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre        The database group must have been opened via
 *             nvpi3_srv_open_db_group.@n
 *             The node must have been opened via nvpi3_srv_open_node.
 * @see        nvpi3_srv_open_db_group@n
 *             nvpi3_srv_open_node
 */
nvpi3_result_t nvpi3_srv_get_value_size(nvpi3_node_handle node_handle,
                                        const char *key_name,
                                        nvpi3_key_type_t type,
                                        uint32_t *size);

/**
 * @brief Deletes a node.
 *
 * All keys in node and sub-nodes to node will also be deleted.
 * In which database the node will be deleted depends on the permission of the
 * database and its priority defined when the group was created via
 * nvpi3_srv_create_db_group.
 *
 * @param[in] group_handle        Handle to the database group.
 * @param[in] node_name           Node name starting from root node. Node names
 *                                should be preceded by '/', e.g.
 *                                /node/sub-node1.../sub-nodeN.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded deleting node@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *            NVPI3_RESULT_NOT_FOUND,     Requested node not found.@n
 *            NVPI3_RESULT_ACCESS_DENIED  The node exist in a data base that is
 *                                        write protected.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       The database group must have been opened via
 *            nvpi3_srv_open_db_group.@n
 * @post      The node is deleted.
 * @see       nvpi3_srv_open_db_group
 */
nvpi3_result_t nvpi3_srv_delete_node(nvpi3_db_group_handle group_handle,
                                     const char *node_name);
/**
 * @brief Sets a key's value.
 *
 * In which database the key's value will be set depends on the permission of
 * the database and its priority defined when the group was created via
 * nvpi3_srv_create_db_group. Parents nodes to the key will be automatically
 * created if they don't exist.
 *
 * @param[in] group_handle        Handle to the database group.
 * @param[in] key_name            Key name starting from root node. Name may
 *                                contain node names which should be preceded
 *                                by '/', e.g.
 *                                /node/sub-node1.../sub-nodeN/key-name.
 * @param[in] type                Key type.
 * @param[in] size                Size of value to set in bytes.
 * @param[in] value               Value to set.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded setting value.@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *            NVPI3_RESULT_ACCESS_DENIED  All databases in the group are write
 *                                        protected.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       The database group must have been opened via nvpi3_open_db_group.@n
 * @post      The key's value is set.
 * @see       nvpi3_srv_open_db_group
 */
nvpi3_result_t nvpi3_srv_set_value(nvpi3_db_group_handle group_handle,
                                   const char *key_name, nvpi3_key_type_t type,
                                   uint32_t size,
                                   const union nvpi3_key_value *value);

/**
 * @brief Deletes a key.
 *
 * In which database the key will be deleted depends on the permission of the
 * database and its priority defined when the group was created via
 * nvpi3_srv_create_db_group.
 *
 * @param[in] group_handle        Handle to the database group.
 * @param[in] key_name            Key name starting from root node. Name may
 *                                contain node names which should be preceded
 *                                by '/', e.g.
 *                                /node/sub-node1.../sub-nodeN/key-name.
 * @param[in] type                Key type.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded deleting key@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *            NVPI3_RESULT_NOT_FOUND,     Requested key not found.@n
 *            NVPI3_RESULT_ACCESS_DENIED  The key exist in a data base that is
 *                                        write protected.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       The database group must have been opened via
 *            nvpi3_srv_open_db_group.@n
 * @post      The key's value is set.
 * @see       nvpi3_srv_open_db_group
 */
nvpi3_result_t nvpi3_srv_delete_key(nvpi3_db_group_handle group_handle,
                                    const char *key_name,
                                    nvpi3_key_type_t type);

/**
 * @brief Enumerates nodes in a database group.
 *
 * @param[in] group_handle  Handle to the database group.
 * @param[in] node_name     Node name starting from root node. Node names should
 *                          be preceded by '/', e.g.
 *                          /node/sub-node1.../sub-nodeN.
 * @param[in] all           Non-zero if also nodes tagged deleted and default
 *                          should be enumerated.
 * @param[in] callback      Callback function that will be called for each found
 *                          node.
 * @param[in] user_data     Application context information to be passed back to
 *                          the callback function.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded opening node@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre        The database group must have been opened via
 *             nvpi3_srv_create_db_group.
 * @see        nvpi3_srv_create_db_group.
 */
nvpi3_result_t nvpi3_srv_enum_nodes(nvpi3_db_group_handle group_handle,
                                    const char *node_name, int all,
                                    nvpi3_srv_enum_nodes_callback callback,
                                    void *user_data);

/**
 * @brief Enumerates keys in a node.
 *
 * @param[in] node_handle  Handle identifying node to enumerate keys in.
 * @param[in] all          Non-zero if also keys tagged deleted and default
 *                         should be enumerated.
 * @param[in] callback     Callback function that will be called for each found
 *                         key.
 * @param[in] user_data    Application context information to be passed back to
 *                         the callback function.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded opening node@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre        Called from nvpi3_srv_enum_nodes_callback or the node must have
 *             been opened via nvpi3_srv_open_node.
 * @see        nvpi3_srv_enum_nodes_callback@n
 *             nvpi3_srv_open_node.
 */
nvpi3_result_t nvpi3_srv_enum_keys(nvpi3_node_handle node_handle, int all,
                                   nvpi3_srv_enum_keys_callback callback,
                                   void *user_data);

/**
 * @brief Returns name of a node.
 *
 * @param[in] node_handle  Handle identifying node whose name should be
 *                         returned.
 * @param[in] node_name    Pointer to node name starting from root node.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded opening node@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.
 *
 * @pre        The database group must have been opened via
 *             nvpi3_srv_open_db_group.@n
 *             The node must have been opened via nvpi3_srv_open_node.
 * @see        nvpi3_srv_open_db_group@n
 *             nvpi3_srv_open_node
 */
nvpi3_result_t nvpi3_srv_get_node_name(nvpi3_node_handle node_handle,
                                       const char **node_name);
/**
 * @brief Enumerates database groups.
 *
 * @param[in] callback     Callback function that will be called for each found
 *                         database group.
 * @param[in] user_data    Application context information to be passed back to
 *                         the callback function.
 *
 * @return     NVPI3_RESULT_SUCCESS      Succeeded to Enumerate database
 *                                       groups.@n
 *             NVPI3_RESULT_OTHER_ERROR  Other miscellaneous error.
 */
nvpi3_result_t nvpi3_srv_enum_db_groups(
	nvpi3_srv_enum_db_groups_callback callback, void *user_data);

#endif /* NVPI3_SRV_H */

#ifdef __cplusplus
}
#endif

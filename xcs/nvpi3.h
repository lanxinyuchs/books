/* > Description **************************************************************/
/**
 * @file nvpi3.h
 * @brief NVPI3 interface
 *
 * This file defines the NVPI3 interface.
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

#ifndef NVPI3_H
#define NVPI3_H

/* > Includes *****************************************************************/
#include <stdint.h>
#include "nvpi3_common.h"

/* > Defines ******************************************************************/

/* > Type Declarations ********************************************************/

/**
 * @brief Holds a transaction handle, to be used to group and commit a number
 *        of operations.
 */
typedef struct nvpi3_transaction_object *nvpi3_transaction_handle;

/* > Function Declarations ****************************************************/

/**
 * @brief Opens a database group.
 *
 * The database group can consist of a single database or several databases. The
 * databases are added to group via nvpi3_create_db_group.
 *
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
 *             nvpi3_create_db_group.
 * @post       The database group is opened.
 * @see        nvpi3_create_db_group
 */
nvpi3_result_t nvpi3_open_db_group(const char *name,
                                   nvpi3_db_group_handle *group_handle);


/**
 * @brief Closes a database group.
 *
 * @param[in] group_handle  Handle to the database group to close.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded closing the database group.@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.
 *
 * @pre       The database group must have been opened via nvpi3_open_db_group.
 * @post      The database group is closed.
 * @see       nvpi3_open_db_group
 */
nvpi3_result_t nvpi3_close_db_group(nvpi3_db_group_handle group_handle);

/**
 * @brief Opens a node in a database group.
 *
 * The priority in which the databases should be searched for the node is
 * defined when the group is created via nvpi3_create_db_group. If node is
 * found a handle to it is returned. The node handle can be used to access
 * keys in that particular node or its sub nodes.
 *
 * @param[in]  group_handle  Handle to the database group.
 * @param[in]  node_name     Node name starting from root node. Node names
 *                           should begin with '/' and each node/sub-node should
 *                           be separated by '/', e.g. "/node/sub-node1/".
 *                           "/" opens the root node.
 * @param[out] node_handle   Handle to opened node on success or NULL on
 *                           failure.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded opening node@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_NOT_FOUND      Node not found.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre        The database group must have been opened via nvpi3_open_db_group.
 * @post       The node is opened.
 * @see        nvpi3_open_db_group
 */
nvpi3_result_t nvpi3_open_node(nvpi3_db_group_handle group_handle,
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
 * @pre       The node must have been opened via nvpi3_open_node.
 * @post      The node is closed.
 * @see       nvpi3_open_node
 */
nvpi3_result_t nvpi3_close_node(nvpi3_node_handle node_handle);

/**
 * @brief Returns a key's value.
 *
 * The priority in which the databases should be searched for the key, is
 * defined when the group is created via nvpi3_create_db_group. If the key is
 * found and caller supplied enough room in value buffer then its value is
 * returned.
 *
 * @param[in]     node_handle  Handle identifying node to read from.
 * @param[in]     key_name     Key name starting from node defined by
 *                             node_handle. Name may contain node names which
 *                             should be separated by '/', e.g.
 *                             "node/sub-node1/key-name".
 * @param[in]     type         Key type.
 * @param[in,out] size         Pointer to variable that the caller should set to
 *                             the number of bytes that can be stored in value.
 *                             This function will set the variable to actual
 *                             number of bytes stored in value.
 * @param[out]    value        Read value.
 *
 * @return        NVPI3_RESULT_SUCCESS           Succeeded reading value.@n
 *                NVPI3_RESULT_INVALID_PARAM     Invalid parameter supplied.@n
 *                NVPI3_RESULT_NOT_FOUND         Requested key is not found.@n
 *                NVPI3_RESULT_BUFFER_TOO_SMALL  Read value is too big to fit in
 *                                               in memory value buffer provided
 *                                               by the caller. The size
 *                                               parameter contains the required
 *                                               size.@n
 *                NVPI3_RESULT_OTHER_ERROR       Other miscellaneous error.
 *
 * @pre           The database group must have been opened via
 *                nvpi3_open_db_group.@n
 *                The node must have been opened via nvpi3_open_node.
 * @see           nvpi3_open_db_group@n
 *                nvpi3_open_node
 */
nvpi3_result_t nvpi3_get_value(nvpi3_node_handle node_handle,
                               const char *key_name, nvpi3_key_type_t type,
                               uint32_t *size, union nvpi3_key_value *value);

/**
 * @brief Returns the key's value size in bytes.
 *
 * The priority in which the databases should be searched for the key, is
 * defined when the group is created via nvpi3_create_db_group. If the key is
 * found then its value size in bytes is returned.
 *
 * @param[in]  node_handle  Handle identifying node to read from.
 * @param[in]  key_name     Key name starting from node defined by node_handle.
 *                          Name may contain node names which should be
 *                          separated by '/', e.g."node/sub-node1/key-name".
 * @param[in]  type         Key type.
 * @param[out] size         Size of value in bytes.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded retrieving value size.
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_NOT_FOUND,     Requested key not found.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre        The database group must have been opened via nvpi3_open_db_group.@n
 *             The node must have been opened via nvpi3_open_node.
 * @see        nvpi3_open_db_group@n
 *             nvpi3_open_node
 */
nvpi3_result_t nvpi3_get_value_size(nvpi3_node_handle node_handle,
                                    const char *key_name, nvpi3_key_type_t type,
                                    uint32_t *size);

/**
 * @brief Deletes a node.
 *
 * All keys in node and sub-nodes to node will also be deleted.
 * In which database the node will be deleted depends on the permission of the
 * database and its priority defined when the group was created via
 * nvpi3_create_db_group.
 *
 * @param[in] group_handle        Handle to the database group.
 * @param[in] transaction_handle  Handle to a database transaction created via
 *                                nvpi3_create_transaction or NULL.
 *                                If set to NULL then operation is carried
 *                                out immediately otherwise it is carried
 *                                out when transaction is committed via
 *                                nvpi3_commit_transaction.
 * @param[in]  node_name          Node name starting from root node. Node names
 *                                should begin with '/' and each node/sub-node
 *                                should be separated by '/', e.g.
 *                                "/node/sub-node1/".
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded deleting node@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *            NVPI3_RESULT_NOT_FOUND,     Requested node not found.@n
 *            NVPI3_RESULT_ACCESS_DENIED  The node exist in a data base that is
 *                                        write protected.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       The database group must have been opened via nvpi3_open_db_group.@n
 *            If transaction_handle is supplied then it must have been created
 *            via nvpi3_create_transaction.
 * @post      The node is deleted.
 * @see       nvpi3_open_db_group@n
 *            nvpi3_create_transaction@n
 *            nvpi3_commit_transaction
 */
nvpi3_result_t nvpi3_delete_node(nvpi3_db_group_handle group_handle,
                                 nvpi3_transaction_handle transaction_handle,
                                 const char *node_name);
/**
 * @brief Sets a key's value.
 *
 * In which database the key's value will be set depends on the permission of
 * the database and its priority defined when the group was created via
 * nvpi3_create_db_group. Parents nodes to the key will be automatically
 * created if they don't exist.
 *
 * @param[in] group_handle        Handle to the database group.
 * @param[in] transaction_handle  Handle to a database transaction created via
 *                                nvpi3_create_transaction or NULL.
 *                                If set to NULL then operation is carried
 *                                out immediately otherwise it is carried
 *                                out when transaction is committed via
 *                                nvpi3_commit_transaction.
 * @param[in] key_name            Key name starting from root node. Name may
 *                                contain node names which should be separated
 *                                by '/', e.g."node/sub-node1/key-name".
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
 *            If transaction_handle is supplied then it must have been created
 *            via nvpi3_create_transaction.
 * @post      The key's value is set.
 * @see       nvpi3_open_db_group@n
 *            nvpi3_create_transaction@n
 *            nvpi3_commit_transaction
 */
nvpi3_result_t nvpi3_set_value(nvpi3_db_group_handle group_handle,
                               nvpi3_transaction_handle transaction_handle,
                               const char *key_name, nvpi3_key_type_t type,
                               uint32_t size,
                               const union nvpi3_key_value *value);

/**
 * @brief Deletes a key.
 *
 * In which database the key will be deleted depends on the permission of the
 * database and its priority defined when the group was created via
 * nvpi3_create_db_group.
 *
 * @param[in] group_handle        Handle to the database group.
 * @param[in] transaction_handle  Handle to a database transaction created via
 *                                nvpi3_create_transaction or NULL.
 *                                If set to NULL then operation is carried
 *                                out immediately otherwise it is carried
 *                                out when transaction is committed via
 *                                nvpi3_commit_transaction.
 * @param[in] key_name            Key name starting from root node. Name may
 *                                contain node names which should be separated
 *                                by '/', e.g."node/sub-node1/key-name".
 * @param[in] type                Key type.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded deleting key@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *            NVPI3_RESULT_NOT_FOUND,     Requested key not found.@n
 *            NVPI3_RESULT_ACCESS_DENIED  The key exist in a data base that is
 *                                        write protected.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       The database group must have been opened via nvpi3_open_db_group.@n
 *            If transaction_handle is supplied then it must have been created
 *            via nvpi3_create_transaction.
 * @post      The key's value is set.
 * @see       nvpi3_open_db_group@n
 *            nvpi3_create_transaction@n
 *            nvpi3_commit_transaction
 */
nvpi3_result_t nvpi3_delete_key(nvpi3_db_group_handle group_handle,
                                nvpi3_transaction_handle transaction_handle,
                                const char *key_name, nvpi3_key_type_t type);

/**
 * @brief Creates a database transaction.
 *
 * The database transaction is used to group a number of operations and commit
 * them as an atomic operation.
 *
 * @param[out] transaction_handle  Handle to created database transaction on
 *                                 success or NULL on failure.
 *
 * @return     NVPI3_RESULT_SUCCESS        Succeeded creating the database
 *                                         transaction.@n
 *             NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *             NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @post       The database transaction is created.
 */
nvpi3_result_t nvpi3_create_transaction(
	nvpi3_transaction_handle *transaction_handle);

/**
 * @brief Destroys a database transaction
 *
 * @param[in] transaction_handle  Handle to the database transaction to destroy.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded destroying the database
 *                                        transaction.@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.
 *
 * @pre       The database transaction must have been created via
 *            nvpi3_create_transaction.
 * @post      The database transaction is destroyed.
 * @see       nvpi3_create_transaction
 */
nvpi3_result_t nvpi3_destroy_transaction(
	nvpi3_transaction_handle transaction_handle);

/**
 * @brief Commits all operations that are part of transaction.
 *
 * If any operation fail the transaction will be rolled back.
 *
 * @param[in] transaction_handle  Handle to the database transaction to commit.
 *
 * @return    NVPI3_RESULT_SUCCESS        Succeeded committing the database
 *                                        transaction.@n
 *            NVPI3_RESULT_INVALID_PARAM  Invalid parameter supplied.@n
 *            NVPI3_RESULT_ACCESS_DENIED  The database group has been deleted or
 *                                        all databases in the group have been
 *                                        changed to write protected.@n
 *            NVPI3_RESULT_OTHER_ERROR    Other miscellaneous error.
 *
 * @pre       The database transaction must have been created via
 *            nvpi3_create_transaction.
 * @post      The database transaction is committed.
 * @see       nvpi3_create_transaction
 */
nvpi3_result_t nvpi3_commit_transaction(
	nvpi3_transaction_handle transaction_handle);

#endif /* NVPI3_H */

#ifdef __cplusplus
}
#endif

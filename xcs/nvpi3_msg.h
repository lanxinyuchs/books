/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#ifndef NVPI3_MSG_H
#define NVPI3_MSG_H

#include "conn-establish.h"
#include "nvpi3.h"
#include "nvpi3_cfg.h"

/**
 * @file nvpi3_msg.h
 * @brief The message interface towards the paramdb server.
 *
 * This header file describes the message interface towards the nvpi3 server.
 */

/**
 * @brief Defines the base number for messages to nvpi3 server
 *
 *        It's still an open issue which message base to use in NVPI3
 */
#define NVPI3_MSG_BASE 0x3c00

/** Supported nvpi3 message server interface version */
#define NVPI3_SERVER_VERSIONS              1

/** The name of nvpi3 server in mailbox interface, itc */
#define NVPI3_SERVER_NAME     "nvpi3_server"

/** Initiates a connection establish procedure towards nvpi3 server */
#define NVPI3_CONN_ESTABLISH_REQ  (NVPI3_MSG_BASE + 0)

/** Connecting to nvpi3 server succeeded */
#define NVPI3_CONN_ESTABLISH_CFM  (NVPI3_MSG_BASE + 1)

/** Connecting to nvpi3 server failed */
#define NVPI3_CONN_ESTABLISH_REJ  (NVPI3_MSG_BASE + 2)

/** Initiates a disconnect procedure towards nvpi3 server */
#define NVPI3_CONN_DISCONNECT_REQ (NVPI3_MSG_BASE + 3)

/** Disconnecting to nvpi3 server succeeded */
#define NVPI3_CONN_DISCONNECT_CFM (NVPI3_MSG_BASE + 4)

/** Disconnecting to nvpi3 server failed */
#define NVPI3_CONN_DISCONNECT_REJ (NVPI3_MSG_BASE + 5)

/** Used internally in conn establish helper to supervise a connection */
#define NVPI3_CONN_MONITOR_FWD    (NVPI3_MSG_BASE + 6)

/** Request to create a data base group. */
#define NVPI3_CREATE_DB_GROUP_REQ (NVPI3_MSG_BASE + 7)

/** Creating data base group succeeded. */
#define NVPI3_CREATE_DB_GROUP_CFM (NVPI3_MSG_BASE + 8)

/** Creating data base group failed. */
#define NVPI3_CREATE_DB_GROUP_REJ (NVPI3_MSG_BASE + 9)

/** Request to destroy a data base group. */
#define NVPI3_DESTROY_DB_GROUP_REQ (NVPI3_MSG_BASE + 10)

/** Destroying data base group succeeded, db handler is returned */
#define NVPI3_DESTROY_DB_GROUP_CFM (NVPI3_MSG_BASE + 11)

/** Destroying data base group failed */
#define NVPI3_DESTROY_DB_GROUP_REJ (NVPI3_MSG_BASE + 12)

/** Request to open a base group. */
#define NVPI3_OPEN_DB_GROUP_REQ (NVPI3_MSG_BASE + 13)

/** Opening data base group succeeded. */
#define NVPI3_OPEN_DB_GROUP_CFM (NVPI3_MSG_BASE + 14)

/** Opening data base group failed. */
#define NVPI3_OPEN_DB_GROUP_REJ (NVPI3_MSG_BASE + 15)

/** Request to close a data base group. */
#define NVPI3_CLOSE_DB_GROUP_REQ (NVPI3_MSG_BASE + 16)

/** Closing data base group succeeded. */
#define NVPI3_CLOSE_DB_GROUP_CFM (NVPI3_MSG_BASE + 17)

/** Closing data base group failed. */
#define NVPI3_CLOSE_DB_GROUP_REJ (NVPI3_MSG_BASE + 18)

/** Request to open a node. */
#define NVPI3_OPEN_NODE_REQ (NVPI3_MSG_BASE + 19)

/** Opening node succeeded. */
#define NVPI3_OPEN_NODE_CFM (NVPI3_MSG_BASE + 20)

/** Opening node failed. */
#define NVPI3_OPEN_NODE_REJ (NVPI3_MSG_BASE + 21)

/** Request to close a node. */
#define NVPI3_CLOSE_NODE_REQ (NVPI3_MSG_BASE + 22)

/** Closing node succeeded. */
#define NVPI3_CLOSE_NODE_CFM (NVPI3_MSG_BASE + 23)

/** Closing node failed. */
#define NVPI3_CLOSE_NODE_REJ (NVPI3_MSG_BASE + 24)

/** Request to return a key's value. */
#define NVPI3_GET_VALUE_REQ (NVPI3_MSG_BASE + 25)

/** Confirm containing the key's value. */
#define NVPI3_GET_VALUE_CFM (NVPI3_MSG_BASE + 26)

/** Failed retrieving value. */
#define NVPI3_GET_VALUE_REJ (NVPI3_MSG_BASE + 27)

/** Request to return a key's value size in bytes. */
#define NVPI3_GET_VALUE_SIZE_REQ (NVPI3_MSG_BASE + 28)

/** Confirm containing the key's value size. */
#define NVPI3_GET_VALUE_SIZE_CFM (NVPI3_MSG_BASE + 29)

/** Failed retrieving value size. */
#define NVPI3_GET_VALUE_SIZE_REJ (NVPI3_MSG_BASE + 30)

/** Request to create a transaction. */
#define NVPI3_CREATE_TRANSACTION_REQ (NVPI3_MSG_BASE + 31)

/** Creating transaction succeeded. */
#define NVPI3_CREATE_TRANSACTION_CFM (NVPI3_MSG_BASE + 32)

/** Creating transaction failed. */
#define NVPI3_CREATE_TRANSACTION_REJ (NVPI3_MSG_BASE + 33)

/** Request to destroy a transaction. */
#define NVPI3_DESTROY_TRANSACTION_REQ (NVPI3_MSG_BASE + 34)

/** Destroying transaction succeeded. */
#define NVPI3_DESTROY_TRANSACTION_CFM (NVPI3_MSG_BASE + 35)

/** Destroying transaction failed */
#define NVPI3_DESTROY_TRANSACTION_REJ (NVPI3_MSG_BASE + 36)

/** Request to commit a transaction. */
#define NVPI3_COMMIT_TRANSACTION_REQ (NVPI3_MSG_BASE + 37)

/** Commiting transaction succeeded. */
#define NVPI3_COMMIT_TRANSACTION_CFM (NVPI3_MSG_BASE + 38)

/**Commiting transaction failed. */
#define NVPI3_COMMIT_TRANSACTION_REJ (NVPI3_MSG_BASE + 39)

/** Request to delete a node. */
#define NVPI3_DELETE_NODE_REQ (NVPI3_MSG_BASE + 40)

/** Deleteting node succeeded. */
#define NVPI3_DELETE_NODE_CFM (NVPI3_MSG_BASE + 41)

/** Deleteting node failed. */
#define NVPI3_DELETE_NODE_REJ (NVPI3_MSG_BASE + 42)

/** Request to set the value of a key. */
#define NVPI3_SET_VALUE_REQ (NVPI3_MSG_BASE + 43)

/** Succeeded setting value. */
#define NVPI3_SET_VALUE_CFM (NVPI3_MSG_BASE + 44)

/** Failed setting value. */
#define NVPI3_SET_VALUE_REJ (NVPI3_MSG_BASE + 45)

/** Request to deletes a key. */
#define NVPI3_DELETE_KEY_REQ (NVPI3_MSG_BASE + 46)

/** Deleteting key succeeded. */
#define NVPI3_DELETE_KEY_CFM (NVPI3_MSG_BASE + 47)

/** Deleteting key failed. */
#define NVPI3_DELETE_KEY_REJ (NVPI3_MSG_BASE + 48)

/** Request client to commit database change. */
#define NVPI3_COMMIT_DB_RREQ (NVPI3_MSG_BASE + 49)

/** Commit succeeded. */
#define NVPI3_COMMIT_DB_RCFM (NVPI3_MSG_BASE + 50)

/** Commit failed. */
#define NVPI3_COMMIT_DB_RREJ (NVPI3_MSG_BASE + 51)

/** Initiates read of values of the keys matching search pattern. */
#define NVPI3_READ_REQ (NVPI3_MSG_BASE + 52)

/**
 * nvpi3_read_req::flags bit mask for read request of all nodes/keys,
 * i.e. including those that are taggged deleted and duplicates.
 */
#define NVPI3_READ_REQ_FLAGS_ALL  0x1

/** Initiating read procedure succeeded. */
#define NVPI3_READ_CFM (NVPI3_MSG_BASE + 53)

/** Initiating read procedure failed. */
#define NVPI3_READ_REJ (NVPI3_MSG_BASE + 54)

/** Indication containing node or value. */
#define NVPI3_READ_IND (NVPI3_MSG_BASE + 55)

/** Indicates end of read procedure. */
#define NVPI3_READ_END_IND (NVPI3_MSG_BASE + 56)

/** Initiates list of created database groups. */
#define NVPI3_LIST_DB_GROUPS_REQ (NVPI3_MSG_BASE + 57)

/** Initiating list procedure succeeded. */
#define NVPI3_LIST_DB_GROUPS_CFM (NVPI3_MSG_BASE + 58)

/** Initiating list procedure failed. */
#define NVPI3_LIST_DB_GROUPS_REJ (NVPI3_MSG_BASE + 59)

/** Indication containing database property. */
#define NVPI3_LIST_DB_GROUPS_IND (NVPI3_MSG_BASE + 60)

/** Indicates end of list procedure. */
#define NVPI3_LIST_DB_GROUPS_END_IND (NVPI3_MSG_BASE + 61)

/**
 * @brief Message structure containing all messages to be handled by
 *        conn_establish_helper and nvpi3 server
 */
#define NVPI3_MESSAGES                                                      \
	conn_any_msg_t                       any_msg;                       \
	conn_establish_req_t                 nvpi3_conn_establish_req;      \
	conn_establish_cfm_t                 nvpi3_conn_establish_cfm;      \
	conn_establish_rej_t                 nvpi3_conn_establish_rej;      \
	conn_disconnect_req_t                nvpi3_conn_disconnect_req;     \
	conn_disconnect_cfm_t                nvpi3_conn_disconnect_cfm;     \
	conn_disconnect_rej_t                nvpi3_conn_disconnect_rej;     \
	struct nvpi3_create_db_group_req     nvpi3_create_db_group_req;     \
	struct nvpi3_create_db_group_cfm     nvpi3_create_db_group_cfm;     \
	struct nvpi3_create_db_group_rej     nvpi3_create_db_group_rej;     \
	struct nvpi3_destroy_db_group_req    nvpi3_destroy_db_group_req;    \
	struct nvpi3_destroy_db_group_cfm    nvpi3_destroy_db_group_cfm;    \
	struct nvpi3_destroy_db_group_rej    nvpi3_destroy_db_group_rej;    \
	struct nvpi3_open_db_group_req       nvpi3_open_db_group_req;       \
	struct nvpi3_open_db_group_cfm       nvpi3_open_db_group_cfm;       \
	struct nvpi3_open_db_group_rej       nvpi3_open_db_group_rej;       \
	struct nvpi3_close_db_group_req      nvpi3_close_db_group_req;      \
	struct nvpi3_close_db_group_cfm      nvpi3_close_db_group_cfm;      \
	struct nvpi3_close_db_group_rej      nvpi3_close_db_group_rej;      \
	struct nvpi3_open_node_req           nvpi3_open_node_req;           \
	struct nvpi3_open_node_cfm           nvpi3_open_node_cfm;           \
	struct nvpi3_open_node_rej           nvpi3_open_node_rej;           \
	struct nvpi3_close_node_req          nvpi3_close_node_req;          \
	struct nvpi3_close_node_cfm          nvpi3_close_node_cfm;          \
	struct nvpi3_close_node_rej          nvpi3_close_node_rej;          \
	struct nvpi3_get_value_req           nvpi3_get_value_req;           \
	struct nvpi3_get_value_cfm           nvpi3_get_value_cfm;           \
	struct nvpi3_get_value_rej           nvpi3_get_value_rej;           \
	struct nvpi3_get_value_size_req      nvpi3_get_value_size_req;      \
	struct nvpi3_get_value_size_cfm      nvpi3_get_value_size_cfm;      \
	struct nvpi3_get_value_size_rej      nvpi3_get_value_size_rej;      \
	struct nvpi3_create_transaction_req  nvpi3_create_transaction_req;  \
	struct nvpi3_create_transaction_cfm  nvpi3_create_transaction_cfm;  \
	struct nvpi3_create_transaction_rej  nvpi3_create_transaction_rej;  \
	struct nvpi3_destroy_transaction_req nvpi3_destroy_transaction_req; \
	struct nvpi3_destroy_transaction_cfm nvpi3_destroy_transaction_cfm; \
	struct nvpi3_destroy_transaction_rej nvpi3_destroy_transaction_rej; \
	struct nvpi3_commit_transaction_req  nvpi3_commit_transaction_req;  \
	struct nvpi3_commit_transaction_cfm  nvpi3_commit_transaction_cfm;  \
	struct nvpi3_commit_transaction_rej  nvpi3_commit_transaction_rej;  \
	struct nvpi3_delete_node_req         nvpi3_delete_node_req;         \
	struct nvpi3_delete_node_cfm         nvpi3_delete_node_cfm;         \
	struct nvpi3_delete_node_rej         nvpi3_delete_node_rej;         \
	struct nvpi3_set_value_req           nvpi3_set_value_req;           \
	struct nvpi3_set_value_cfm           nvpi3_set_value_cfm;           \
	struct nvpi3_set_value_rej           nvpi3_set_value_rej;           \
	struct nvpi3_delete_key_req          nvpi3_delete_key_req;          \
	struct nvpi3_delete_key_cfm          nvpi3_delete_key_cfm;          \
	struct nvpi3_delete_key_rej          nvpi3_delete_key_rej;          \
	struct nvpi3_commit_db_rreq          nvpi3_commit_db_rreq;          \
	struct nvpi3_commit_db_rcfm          nvpi3_commit_db_rcfm;          \
	struct nvpi3_commit_db_rrej          nvpi3_commit_db_rrej;          \
	struct nvpi3_read_req                nvpi3_read_req;                \
	struct nvpi3_read_cfm                nvpi3_read_cfm;                \
	struct nvpi3_read_rej                nvpi3_read_rej;                \
	struct nvpi3_read_ind                nvpi3_read_ind;                \
	struct nvpi3_read_end_ind            nvpi3_read_end_ind;            \
	struct nvpi3_list_db_groups_req      nvpi3_list_db_groups_req;      \
	struct nvpi3_list_db_groups_cfm      nvpi3_list_db_groups_cfm;      \
	struct nvpi3_list_db_groups_rej      nvpi3_list_db_groups_rej;      \
	struct nvpi3_list_db_groups_ind      nvpi3_list_db_groups_ind;      \
	struct nvpi3_list_db_groups_end_ind  nvpi3_list_db_groups_end_ind

/**
 * @brief All messages must have a header consisting of a message number,
 *        a procedure reference identification and a connection reference
 *        identification
 */
#define NVPI3_MSG_HEADER                        \
	uint32_t msgno;                         \
	uint32_t procedure_ref;                 \
	uint32_t connection_ref

/**
 * @brief Holds information for NVPI3_CREATE_DB_GROUP_REQ message
 */
struct nvpi3_create_db_group_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Name of data base group */
	char name[NVPI3_MAX_STRING_LENGTH];
	/** Mbox Id of commit db thread */
	itc_mbox_id_t commit_db_mbox_id;
	/** Number of elements in definition array */
	uint32_t num_of_definitions;
	/** Definitions for the databases included in the group */
	struct nvpi3_db_definition definition[];
};

/**
 * @brief Holds information for NVPI3_CREATE_DB_GROUP_CFM message
 */
struct nvpi3_create_db_group_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to created database group */
	nvpi3_db_group_handle group_handle;
};

/**
 * @brief Holds information for NVPI3_CREATE_DB_GROUP_REJ message
 */
struct nvpi3_create_db_group_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_DESTROY_DB_GROUP_REQ message
 */
struct nvpi3_destroy_db_group_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database group to destroy */
	nvpi3_db_group_handle group_handle;
};

/**
 * @brief Holds information for NVPI3_DESTROY_DB_GROUP_CFM message
 */
struct nvpi3_destroy_db_group_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_DESTROY_DB_GROUP_REJ message
 */
struct nvpi3_destroy_db_group_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};


/**
 * @brief Holds information for NVPI3_OPEN_DB_GROUP_REQ message
 */
struct nvpi3_open_db_group_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Name of data base group */
	char name[];
};

/**
 * @brief Holds information for NVPI3_OPEN_DB_GROUP_CFM message
 */
struct nvpi3_open_db_group_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to opened database group */
	nvpi3_db_group_handle group_handle;
};

/**
 * @brief Holds information for NVPI3_OPEN_DB_GROUP_REJ message
 */
struct nvpi3_open_db_group_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_CLOSE_DB_GROUP_REQ message
 */
struct nvpi3_close_db_group_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database group to close. */
	nvpi3_db_group_handle group_handle;
};

/**
 * @brief Holds information for NVPI3_CLOSE_DB_GROUP_CFM message
 */
struct nvpi3_close_db_group_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_CLOSE_DB_GROUP_REJ message
 */
struct nvpi3_close_db_group_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};


/**
 * @brief Holds information for NVPI3_OPEN_NODE_REQ message
 */
struct nvpi3_open_node_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database group. */
	nvpi3_db_group_handle group_handle;
	/** name of node, null terminated */
	char node_name[];
};

/**
 * @brief Holds information for NVPI3_OPEN_NODE_CFM message
 */
struct nvpi3_open_node_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to opened node. */
	nvpi3_node_handle node_handle;
};

/**
 * @brief  Holds information for NVPI3_OPEN_NODE_REJ message
 */
struct nvpi3_open_node_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_CLOSE_NODE_REQ message
 */
struct nvpi3_close_node_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the node to close. */
	nvpi3_node_handle node_handle;
};

/**
 * @brief Holds information for NVPI3_CLOSE_NODE_CFM message
 */
struct nvpi3_close_node_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief  Holds information for NVPI3_CLOSE_NODE_REJ message
 */
struct nvpi3_close_node_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief  Holds information for NVPI3_GET_VALUE_REQ message
 */
struct nvpi3_get_value_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the node. */
	nvpi3_node_handle node_handle;
	/** key type */
	nvpi3_key_type_t type;
	/* name of key to read value from */
	char key_name[];
};

/**
 * @brief Holds information for NVPI3_GET_VALUE_CFM message
 */
struct nvpi3_get_value_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** size read value */
	uint32_t value_size;
	/** read value */
	union nvpi3_key_value value;
};

/**
 * @brief Holds information for NVPI3_GET_VALUE_REJ message
 */
struct nvpi3_get_value_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
        nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_GET_VALUE_SIZE_REQ message
 */
struct nvpi3_get_value_size_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the node. */
	nvpi3_node_handle node_handle;
	/** key type */
	nvpi3_key_type_t type;
	/** name of key to read value from */
	char key_name[];
};

/**
 * @brief Holds information for NVPI3_GET_VALUE_SIZE_CFM message
 */
struct nvpi3_get_value_size_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** size of value */
	uint32_t value_size;
};

/**
 * @brief Holds information for NVPI3_GET_VALUE_SIZE_REJ message
 */
struct nvpi3_get_value_size_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_CREATE_TRANSACTION_REQ message
 */
struct nvpi3_create_transaction_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_CREATE_TRANSACTION_CFM message
 */
struct nvpi3_create_transaction_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the created database transaction. */
	nvpi3_transaction_handle transaction_handle;
};

/**
 * @brief Holds information for NVPI3_CREATE_TRANSACTION_REJ message
 */
struct nvpi3_create_transaction_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_DESTROY_TRANSACTION_REQ message
 */
struct nvpi3_destroy_transaction_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database transaction to destroy. */
	nvpi3_transaction_handle transaction_handle;
};

/**
 * @brief Holds information for NVPI3_DESTROY_TRANSACTION_CFM message
 */
struct nvpi3_destroy_transaction_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_DESTROY_TRANSACTION_REJ message
 */
struct nvpi3_destroy_transaction_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_COMMIT_TRANSACTION_REQ message
 */
struct nvpi3_commit_transaction_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database transaction to commit. */
	nvpi3_transaction_handle transaction_handle;
};

/**
 * @brief Holds information for NVPI3_COMMIT_TRANSACTION_CFM message
 */
struct nvpi3_commit_transaction_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_COMMIT_TRANSACTION_REJ message
 */
struct nvpi3_commit_transaction_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_DELETE_NODE_REQ message
 */
struct nvpi3_delete_node_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database group. */
	nvpi3_db_group_handle group_handle;
	/** Handle to the database transaction. */
	nvpi3_transaction_handle transaction_handle;
	/** Name of node to delete starting from root node. */
	char node_name[];
};

/**
 * @brief Holds information for NVPI3_DELETE_NODE_CFM message
 */
struct nvpi3_delete_node_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;

};

/**
 * @brief Holds information for NVPI3_DELETE_NODE_REJ message
 */
struct nvpi3_delete_node_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_SET_REQ message
 */
struct nvpi3_set_value_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database group. */
	nvpi3_db_group_handle group_handle;
	/** Handle to the database transaction. */
	nvpi3_transaction_handle transaction_handle;
	/**
	 * Offset of name of key whose value should be set. Note that space is
	 * allocated within this message buffer but after value.
	 */
	uint32_t key_name_offset;
	/** key type */
	nvpi3_key_type_t type;
	/** size of value to set */
	uint32_t value_size;
	/** Value to set. */
	union nvpi3_key_value value;
};

/**
 * @brief Holds information for NVPI3_SET_CFM message
 */
struct nvpi3_set_value_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_SET_REJ message
 */
struct nvpi3_set_value_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_DELETE_KEY_REQ message
 */
struct nvpi3_delete_key_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database group. */
	nvpi3_db_group_handle group_handle;
	/** Handle to the database transaction. */
	nvpi3_transaction_handle transaction_handle;
	/** key type */
	nvpi3_key_type_t type;
	/** Name of key to delete starting from root node. */
	char key_name[];
};

/**
 * @brief Holds information for NVPI3_DELETE_KEY_CFM message
 */
struct nvpi3_delete_key_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;

};

/**
 * @brief Holds information for NVPI3_DELETE_KEY_REJ message
 */
struct nvpi3_delete_key_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_COMMIT_DB_RREQ message
 */
struct nvpi3_commit_db_rreq {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/* Commit callback function. */
	nvpi3_db_commit_callback_func callback;
	/* Application context information. */
	void *user_data;
	/* The number of bytes in database. */
	uint32_t size;
	/* Storage specification for database. */
	union nvpi3_db_storage storage;
};

/**
 * @brief Holds information for NVPI3_COMMIT_DB_RCFM message
 */
struct nvpi3_commit_db_rcfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_COMMIT_DB_RREJ message
 */
struct nvpi3_commit_db_rrej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
};

/**
 * @brief Holds information for NVPI3_READ_REQ message
 */
struct nvpi3_read_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Handle to the database group. */
	nvpi3_db_group_handle group_handle;
	/** Request flags:
	 * @ref NVPI3_READ_REQ_FLAGS_ALL,
	 */
	uint32_t flags;
	/** Search pattern, '*' and '?' allowed as wildcards. */
	char pattern[];
};

/**
 * @brief Holds information for NVPI3_READ_CFM message
 */
struct nvpi3_read_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_READ_REJ message
 */
struct nvpi3_read_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/** nvpi3_read_ind::type values for an XLF that contains an AU boot. */
/**
 * @brief nvpi3_read_ind::type values.
 */
enum nvpi3_read_ind_type {
	/** nvpi3_read_ind contains node info. */
	NVPI3_READ_IND_NODE,
	/** nvpi3_read_ind contains key info. */
	NVPI3_READ_IND_KEY
};

/**
 * @brief Holds information for NVPI3_READ_IND message
 */
struct nvpi3_read_ind {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/**
	 * Offset of name of database. Note that space is allocated within this
	 * message buffer at the end.
	 */
	uint32_t database_name_offset;
	/**
	 * Offset of name of node or key. Note that space is allocated within
	 * message buffer at the end
	 */
	uint32_t name_offset;
	/** @ref NVPI3_READ_IND_NODE or @ref NVPI3_READ_IND_KEY */
	uint32_t type;
	union {
		struct {
			char strings[1];
		} node;
		struct {
			/**
			 * Offset of type string. Note that space is allocated
			 *  within message buffer at the end
			 */
			uint32_t type_str_offset;
			/** key type */
			nvpi3_key_type_t type;
			/** size value */
			uint32_t value_size;
			/** value */
			union nvpi3_key_value value;
		} key;
	} u;
};

/**
 * @brief Holds information for NVPI3_READ_END_IND message
 */
struct nvpi3_read_end_ind {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Result. */
	nvpi3_result_t result;
};

/**
 * @brief Holds information for NVPI3_LIST_DB_GROUPS_REQ message
 */
struct nvpi3_list_db_groups_req {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_LIST_DB_GROUPS_CFM message
 */
struct nvpi3_list_db_groups_cfm {
	/** Message header information */
	NVPI3_MSG_HEADER;
};

/**
 * @brief Holds information for NVPI3_LIST_DB_GROUPS_REJ message
 */
struct nvpi3_list_db_groups_rej {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Reason for reject. */
	nvpi3_result_t error;
};

/**
 * @brief Holds information for NVPI3_LIST_DB_GROUPS_IND message
 */
struct nvpi3_list_db_groups_ind {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/**Number of groups. */
	uint32_t num_of_groups;
	/** Name of data base group */
	char name[NVPI3_MAX_STRING_LENGTH];
	/** Number of elements in definition array */
	uint32_t num_of_definitions;
	/** Definitions for the databases included in the group */
	struct nvpi3_db_definition definition[];
};

/**
 * @brief Holds information for NVPI3_LIST_DB_GROUPS_END_IND message
 */
struct nvpi3_list_db_groups_end_ind {
	/** Message header information */
	NVPI3_MSG_HEADER;
	/** Result. */
	nvpi3_result_t result;
};

#endif /* NVPI3_MSG_H */

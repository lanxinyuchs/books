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

#ifdef __cplusplus
extern "C" {
#endif

#ifndef PARCMD_H
#define PARCMD_H

#include "conn-establish-helper.h"
#include "nvpi3.h"

#define PARCMD_TIMEOUT             10000 /* 10 sec */

struct parcmd_conn {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
	struct conn_establish_msg_numbers *conn_messages;
};

int parcmd_conn_establish(const char *mbox_name,
                          struct parcmd_conn *connection);

int parcmd_conn_disconnect(const struct parcmd_conn *connection);

nvpi3_result_t parcmd_list_db_groups(
	const struct parcmd_conn *conn,
	nvpi3_result_t (*callback)(void *user_data, uint32_t group_index,
	                           uint32_t num_of_groups, const char *name,
	                           uint32_t num_of_def,
	                           const struct nvpi3_db_definition def[]),
	void *user_data);

nvpi3_result_t parcmd_open_db_group(const struct parcmd_conn *conn,
                                    const char *name,
                                    nvpi3_db_group_handle *group_handle);

nvpi3_result_t parcmd_close_db_group(const struct parcmd_conn *conn,
                                     nvpi3_db_group_handle group_handle);

nvpi3_result_t parcmd_read(
	const struct parcmd_conn *conn, nvpi3_db_group_handle group_handle,
	uint32_t flags, const char *pattern,
	nvpi3_result_t (*callback)(void *user_data,
	                           struct nvpi3_read_ind **ind),
	void *user_data);

#endif /* PARCMD_H */

#ifdef __cplusplus
}
#endif

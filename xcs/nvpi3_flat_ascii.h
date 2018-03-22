/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef NVPI3_FLAT_ASCII
#define NVPI3_FLAT_ASCII

#include <stdint.h>
#include "nvpi3_common.h"
#include "nvpi3_srv_common.h"

nvpi3_result_t nvpi3_flat_ascii_get_db_header_size(uint32_t *size);

nvpi3_result_t nvpi3_flat_ascii_get_db_size(void *header, uint32_t *size);

nvpi3_result_t nvpi3_flat_ascii_create_db(void *address, uint32_t size,
                                          int clear, char delimiter,
                                          void **h_db);

nvpi3_result_t nvpi3_flat_ascii_resize_db(void *h_db, void *address,
                                          uint32_t size);

nvpi3_result_t nvpi3_flat_ascii_destroy_db(void *h_db);

nvpi3_result_t nvpi3_flat_ascii_open_node(void *h_db, void *h_parent_node,
                                          uint32_t close_option,
                                          const char *name, uint32_t length,
                                          void **h_node);

nvpi3_result_t nvpi3_flat_ascii_close_node(void *h_node);

uint32_t nvpi3_flat_ascii_get_node_path_size(void *h_node);

nvpi3_result_t nvpi3_flat_ascii_get_node_path(void *h_node, char *path,
                                              uint32_t size);

nvpi3_result_t nvpi3_flat_ascii_get_value(void *h_node, const char *key_name,
                                          nvpi3_key_type_t type, uint32_t *size,
                                          nvpi3_srv_get_value_callback callback,
                                          void *user_data);

nvpi3_result_t nvpi3_flat_ascii_add_node(void *h_node, const char *name,
                                         uint32_t length, void **h_subnode);

nvpi3_result_t nvpi3_flat_ascii_delete_node(void *h_node);

nvpi3_result_t nvpi3_flat_ascii_set_value(void *h_db, const char *key_name,
                                          nvpi3_key_type_t type, uint32_t size,
                                          const union nvpi3_key_value *value);

nvpi3_result_t nvpi3_flat_ascii_delete_key(void *h_db, const char *key_name,
                                           nvpi3_key_type_t type);

nvpi3_result_t nvpi3_flat_ascii_enum_nodes(
	void *h_db, const char *node_name, int recursive,
	nvpi3_result_t (*callback) (void *user_data, void *h_node),
	void *user_data);

nvpi3_result_t nvpi3_flat_ascii_enum_keys(
	void *h_node,
	nvpi3_result_t (*callback)(void *user_data,
	                           const char *key_name,
	                           nvpi3_key_type_t type,
	                           const char *type_str,
	                           uint32_t size,
	                           const union nvpi3_key_value *value),
	void *user_data);

#endif /* NVPI3_FLAT_ASCII */

#ifdef __cplusplus
}
#endif

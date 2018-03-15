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

#ifndef PARAMDB_TEST_NVPI3__
#define PARAMDB_TEST_NVPI3__

#include <nvpi3_cfg.h>
#include <nvpi3.h>

extern nvpi3_db_group_handle open_db_group(char *db_group_name, uint32_t *res);

extern void close_db_group(nvpi3_db_group_handle db_group_handle,
                           uint32_t *res);

extern nvpi3_node_handle open_node(nvpi3_db_group_handle db_group_handle,
                                   char *node_name, uint32_t *res);

extern void close_node(nvpi3_node_handle node_handle, uint32_t *res);

extern uint32_t get_value(nvpi3_node_handle node_handle, char *key_name,
                          uint32_t value_type, uint32_t value__buff_size,
                          union nvpi3_key_value *value_buff, uint32_t *res);

extern uint32_t get_value_size(nvpi3_node_handle node_handle,
                               char *key_name, uint32_t value_type,
                               uint32_t *res);
#endif /* PARAMDB_TEST_NVPI3__ */

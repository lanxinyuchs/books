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

#ifndef PARAMDB_TCS_NVPI3_NORMAL__
#define PARAMDB_TCS_NVPI3_NORMAL__

extern enum tc_result create_destroy_db_group_db(void);
extern enum tc_result create_destroy_db_group_dbs(void);
extern enum tc_result create_destroy_db_groups_db(void);
extern enum tc_result create_destroy_db_groups_db_loop(void);
extern enum tc_result create_destroy_db_groups_dbs(void);
extern enum tc_result create_destroy_db_groups_dbs_loop(void);
extern enum tc_result create_open_destroy_db_group(void);

extern enum tc_result open_close_db_group(void);
extern enum tc_result open_close_dbs(void);
extern enum tc_result open_close_dbs_loop(void);

extern enum tc_result open_close_node_no_db(void);
extern enum tc_result open_close_nodes_same_db(void);
extern enum tc_result open_close_nodes_two_dbs(void);
extern enum tc_result open_close_nodes_two_dbs_loop(void);

extern enum tc_result get_values_no_node_handle(void);
extern enum tc_result get_values_one_node_handle(void);
extern enum tc_result get_values_two_node_handles(void);
extern enum tc_result get_values_two_node_handles_loop(void);
extern enum tc_result get_value_sizes_and_values_two_node_handles(void);
extern enum tc_result get_value_sizes_and_values_two_node_handles_loop(void);

extern enum tc_result get_value_sizes_no_node_handle(void);
extern enum tc_result get_value_sizes_one_node_handle(void);
extern enum tc_result get_value_sizes_two_node_handles(void);
extern enum tc_result get_value_sizes_two_node_handles_loop(void);

#endif /* PARAMDB_TCS_NVPI3_NORMAL__ */


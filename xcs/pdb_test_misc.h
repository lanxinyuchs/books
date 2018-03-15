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

#ifndef PARAMDB_TEST_MISC__
#define PARAMDB_TEST_MISC__

#include <stdbool.h>

#include <itc.h>
#include <nvpi3_common.h>
#include <nvpi3.h>
#include <nvpi3_cfg.h>

enum tc_result {
	TC_PASSED = 0,
	TC_FAILED = 1
};

#define TEST_MAX_NO_OF_MAILBOXES         4
#define TEST_MAILBOX_NAME               "paramdb_test"
#define TEST_TIMEOUT                     1000

#define TEST_MAX_LOOPS                   10

#define MAILBOX_CONNECTIONS              1

#ifdef  DEBUG
#    define FUNC  log_info("PDBTest: %s;%s;%d",__FILE__, __func__,__LINE__)
#    define DBG(format,args...) log_info("PDBTest %s;%s;%d: " format "\n",     \
                __FILE__, __func__,__LINE__, args)
#    define DBGNF(txt_str) log_info("PDBTest %s;%s;%d %s", __FILE__, __func__, __LINE__, txt_str)
#else
#    define FUNC
#    define DBG(format,args...)
#    define DBGNF(txt_str)
#endif

#define PARAMDB_LOG_INFO(format,args...) log_info("PDBTest %s;%s;%d: " format "\n",     \
                __FILE__, __func__,__LINE__, args)

#define PARAMDB_LOG_ERR(format,args...) log_info("PDBTest %s;%s;%d: " format "\n",     \
                __FILE__, __func__, __LINE__, args)

#define CHECK_RES_RETURN_ON_ERROR(res, str)\
	do{if (res != NVPI3_RESULT_SUCCESS){\
			fprintf(stderr, str);\
			fprintf(stderr, "\n");\
			return TC_FAILED;\
		} }while(0)

#define CHECK_RES_GOTO_ON_ERROR(res, str, goto_lable)   \
	do{if (res != NVPI3_RESULT_SUCCESS){\
			fprintf(stderr, str);\
			fprintf(stderr, "\n");\
			goto goto_lable;    \
		} }while(0)

#define CHECK_RES_EXIT_ON_ERROR(my_mbox_id, res, str)   \
	do{if (res != NVPI3_RESULT_SUCCESS){\
			fprintf(stderr, str);\
			fprintf(stderr, "\n");\
			test_remove_mailbox_if(my_mbox_id);     \
			exit(1);                        \
		} }while(0)

#define CHECK_MALLOC_RETURN_ON_ERROR(pointer) \
	do{if (pointer == NULL){                           \
			DBGNF("Memory allocation failed"); \
			return TC_FAILED; \
		} }while(0)

#define CHECK_MALLOC_GOTO_ON_ERROR(pointer, failure_lable) \
	do{ if(pointer == NULL){                                   \
			fprintf(stderr, "Memory allocation failed\n");    \
			goto failure_lable; \
		} }while(0)

extern char *test_nvpi3_res_to_str(uint32_t res);

extern void test_print_value(char *key_name, uint32_t key_name_len,
                             uint32_t value_type, uint32_t value_size,
                             union nvpi3_key_value *value);

extern bool test_init_mailbox_if(itc_mbox_id_t *my_mbox_id);

extern void test_remove_mailbox_if(itc_mbox_id_t my_mbox_id);

#endif /* PARAMDB_TEST_MISC__ */

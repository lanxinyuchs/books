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

#include <arpa/inet.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include <string.h>
#include <itc.h>

#include <nvpi3_common.h>
#include <nvpi3.h>
#include <nvpi3_cfg.h>
#include <nvpi3_msg.h>
#include <pdb_test_nvpi3_cfg.h>

#include <log.h>
#include <pdb_test_misc.h>
#include <pdb_test_nvpi3.h>

bool test_init_mailbox_if(itc_mbox_id_t *my_mbox_id)
{
	FUNC;

	itc_mbox_id_t tmp;

	*my_mbox_id = ITC_NO_ID;

	if (itc_init(TEST_MAX_NO_OF_MAILBOXES, ITC_MALLOC,
	             NULL, ITC_NO_NAMESPACE, 0))
		return false;

	tmp = itc_create_mailbox(TEST_MAILBOX_NAME, 0);
	if (tmp == ITC_NO_ID)
		return false;

	DBG("mailbox created, tmp my_mbox_id=%d", tmp);
	*my_mbox_id = tmp;
	DBG("mailbox created, my_mbox_id=%d", *my_mbox_id);


	return true;
}

void test_remove_mailbox_if(itc_mbox_id_t my_mbox_id)
{
	itc_delete_mailbox(my_mbox_id);
	DBGNF("mailbox deleted");
}

char *test_nvpi3_res_to_str(uint32_t res)
{
	FUNC;

	switch (res) {
	case NVPI3_RESULT_SUCCESS:
		return "NVPI3_RESULT_SUCCESS";
		break;

	case NVPI3_RESULT_INVALID_PARAM:
		return "NVPI3_RESULT_INVALID_PARAM";
		break;

	case NVPI3_RESULT_NOT_FOUND:
		return "NVPI3_RESULT_NOT_FOUND";
		break;

	case NVPI3_RESULT_BUFFER_TOO_SMALL:
		return "NVPI3_RESULT_BUFFER_TOO_SMALL";
		break;

	case NVPI3_RESULT_OTHER_ERROR:
		return "NVPI3_RESULT_OTHER_ERROR";
		break;

	default:
		printf("Unexpected result back from NVPI3, res=%s",
		       "NVPI3_RESULT_OTHER_ERROR");
		return "UNKNOWN NVPI3 RESULT";
		break;

	}
}


void test_print_value(char *key_name, uint32_t key_name_len,
                      uint32_t value_type,
                      uint32_t value_size, union nvpi3_key_value *value)
{
	uint8_t ix;
	union nvpi3_key_value *p = value;

	FUNC;

	DBGNF("\n");

	switch (value_type) {
	case NVPI3_KEY_TYPE_STR:
		printf("%s = %s\n", key_name, value->str);
		break;

	case NVPI3_KEY_TYPE_U32:
		printf("%s = ", key_name);
		for (ix = 0; ix < (value_size / 4); ix++)
			p = p + printf("0x%08x ",
			               (uint32_t)value->u32_array[ix]);
		printf("\n");
		break;

	default: /* Binary or u8 */
		printf("%s = ", key_name);
		for (ix = 0; ix < value_size; ix++)
			p = p + printf("0x%02x ", value->u8_array[ix]);
		printf("\n");
		break;
	}
}



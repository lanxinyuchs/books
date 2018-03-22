#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "itc.h"
#include "xpai_xhl_if.h"
#include "hwlog.h"

#define MAX_MAILBOX_NUM    1

static int init_itc(void)
{
	/* Initialize ITC and create mailbox */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if(itc_create_mailbox("hwlog_test", 0) == ITC_NO_ID) {
		printf("failed to create hwlog_test mailbox\n");
		return 1;
	}
	return 0;
}

static void delete_itc(void)
{
	itc_mbox_id_t mbox = itc_current_mbox();
	if( mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}
}

union itc_msg {
	HWLOG_SIGNAL_STRUCTS;
};

static itc_mbox_id_t _hwld_mbox = ITC_NO_ID;
static uint32_t _connection_reference;

static int connect_to_hwlog()
{
	union itc_msg *itc_msg, *reply;
	struct hwlog_connect_req *req;
	_hwld_mbox = itc_locate(HWLOG_MAILBOX);

	if (_hwld_mbox == ITC_NO_ID)
		return 0;

	itc_msg = itc_alloc(sizeof(struct hwlog_connect_req),
	                    HWLOG_CONNECT_REQ);

	req = &itc_msg->hwlog_connect_req;
	req->nbr_of_supported_protocol_revisions = 1;
	req->protocol_revision = HWLOG_PROTOCOL_REVISION;
	itc_send(&itc_msg, _hwld_mbox, ITC_MY_MBOX);
	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, _hwld_mbox);

	if (reply->hwlog_connect_cfm.msgno == HWLOG_CONNECT_CFM) {
		_connection_reference = reply->hwlog_connect_cfm.connection_reference;
		itc_free((union itc_msg **)&reply);
		return -1;
	}

	itc_free((union itc_msg **)&reply);
	return 0;
}

static int test_hwlog_clear(void)
{
	union itc_msg *itc_msg, *reply;
	struct hwlog_logerase_req *req;
	struct XPAI_HWLogS *log = NULL;
	U32 status = 0;

	if (_hwld_mbox == ITC_NO_ID) {
		if (!connect_to_hwlog())
			return status;
	}

	itc_msg = itc_alloc(sizeof(struct hwlog_logerase_req),
	                    HWLOG_LOGERASE_REQ);
	req = &itc_msg->hwlog_logerase_req;
	req->connection_reference = _connection_reference;
	itc_send(&itc_msg, _hwld_mbox, ITC_MY_MBOX);

	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, _hwld_mbox);
	if (reply->hwlog_logerase_cfm.msgno == HWLOG_LOGERASE_CFM) {
		status = -1;
	}

	itc_free((union itc_msg **)&reply);

	log = XPAI_HWLogRead(XPAI_XHL_AREA_REPAIR);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogRead returned %d\n", log->result);
		status = -1;
		goto exit;
	}

	if (log->noOfEntries != 0) {
		printf("Log is not empty (%d entries)\n", log->noOfEntries);
		status = -1;
		goto exit;
	}

exit:
	if (log != NULL) itc_free((union itc_msg **)&log);

	return status;
}

static int test_hwlog_write_conf(int test_no)
{
	struct XPAI_HWLogS *log = NULL;
	U32 i, result, status = 0, sub_test = 0;
	char *text_logged = "This text should be logged";
	char *text_logged_alt = "This text should also be logged";
	char *date = "151007 164155";

	printf("\t%d.%d: Write a configuration hwlog entry with id 000 and 002\n",
	       test_no, sub_test++);
	result = XPAI_HWLogWrite2(date, "000", text_logged);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}
	result = XPAI_HWLogWrite2(date, "002", text_logged);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Write same again (should be filtered)\n", test_no,
	       sub_test++);
	result = XPAI_HWLogWrite2(date, "000", text_logged);
	if (result != XPAI_XHL_FILTERED) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}
	result = XPAI_HWLogWrite2(date, "002", text_logged);
	if (result != XPAI_XHL_FILTERED) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Write a configuration hwlog entry w different text "
	       "(should be logged)\n", test_no, sub_test++);

	result = XPAI_HWLogWrite2(date, "000", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite returned %d\n", result);
		status = -1;
		goto exit;
	}
	result = XPAI_HWLogWrite2(date, "002", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Read back and see that we have four log entries\n",
	       test_no, sub_test++);
	log = XPAI_HWLogRead(XPAI_XHL_AREA_REPAIR);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogRead returned %d\n", log->result);
		status = -1;
		goto exit;
	}

	for (i = 0; i < log->noOfEntries; i++) {
		printf("\t     %s\n", log->list[i]);
	}

	if (log->noOfEntries != 4) {
		printf("ERROR: Invalid amount of entries %d, expected 2\n",
		       log->noOfEntries);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Check that entries is what we wrote\n",
	       test_no, sub_test++);
	if (strstr(log->list[0], text_logged) == NULL ||
	    strstr(log->list[0], "000") == NULL) {
		printf("ERROR: Entry 0 \"%s\" does not contain "
		       "what was written \"%s\"\n",
		       log->list[0], text_logged);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[1], text_logged) == NULL ||
	    strstr(log->list[1], "002") == NULL) {
		printf("ERROR: Entry 1 \"%s\" does not contain "
		       "what was written \"%s\"\n",
		       log->list[1], text_logged);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[2], text_logged_alt) == NULL ||
	    strstr(log->list[2], "000") == NULL) {
		printf("ERROR: Entry 2 \"%s\" does not contain "
		       "what was written \"%s\"\n",
		       log->list[2], text_logged_alt);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[3], text_logged_alt) == NULL ||
	    strstr(log->list[3], "002") == NULL) {
		printf("ERROR: Entry 3 \"%s\" does not contain "
		       "what was written \"%s\"\n",
		       log->list[3], text_logged_alt);
		status = -1;
		goto exit;
	}

exit:
	if (log != NULL) itc_free((union itc_msg **)&log);
	return status;
}

static int test_hwlog_write_op(int test_no)
{
	struct XPAI_HWLogS *log = NULL;
	U32 i, result, status = 0, sub_test = 0;
	char *text_logged = "This text should be logged";
	char *text_logged_filtered = "This text should NOT be logged";
	char *text_logged_filtered_alt = "This text should NOT be logged either";

	printf("\t%d.%d: Write a operator hwlog entry (ID 001)\n", test_no,
	       sub_test++);
	result = XPAI_HWLogWrite2("date", "001", text_logged);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Write same again (should be filtered)\n", test_no,
	       sub_test++);
	result = XPAI_HWLogWrite2("date", "001", text_logged_filtered);
	if (result != XPAI_XHL_FILTERED) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Write a configuration hwlog entry w different "
	       "text (should be filtered)\n", test_no, sub_test++);
	result = XPAI_HWLogWrite2("date", "001", text_logged_filtered_alt);
	if (result != XPAI_XHL_FILTERED) {
		printf("XPAI_HWLogWrite returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Read back and see that we have one log entry\n",
	       test_no, sub_test++);
	log = XPAI_HWLogRead(XPAI_XHL_AREA_REPAIR);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogRead returned %d\n", log->result);
		status = -1;
		goto exit;
	}

	for (i = 0; i < log->noOfEntries; i++) {
		printf("\t     %s\n", log->list[i]);
	}

	if (log->noOfEntries != 1) {
		printf("ERROR: Invalid amount of entries %d, expected 1\n",
		       log->noOfEntries);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Check that entries is what we wrote\n",
	       test_no, sub_test++);
	if (strstr(log->list[0], text_logged) == NULL) {
		printf("ERROR: Entry 0 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[0], text_logged);
		status = -1;
		goto exit;
	}

exit:
	if (log != NULL) itc_free((union itc_msg **)&log);
	return status;
}

static int test_hwlog_write_hwfault(int test_no)
{
	struct XPAI_HWLogS *log = NULL;
	U32 i, result, status = 0, sub_test = 0;
	char *text_logged = "This text should be logged";
	char *text_logged_alt = "This text should also be logged";
	char *text_logged_filtered = "This text should NOT be logged";

	printf("\t%d.%d: Write five hwfault hwlog entries (id 9ZZ), "
	       "one should be filtered out\n",
	       test_no, sub_test++);
	for (i = 0; i < 4; i++) {
		if(i % 2) {
			result = XPAI_HWLogWrite2("date", "9ZZ",
			                          text_logged_alt);
		} else {
			result = XPAI_HWLogWrite2("date", "9ZZ",
			                          text_logged);
		}
		if (result != XPAI_XHL_OK) {
			printf("XPAI_HWLogWrite2 returned %d\n", result);
			status = -1;
			goto exit;
		}
	}
	result = XPAI_HWLogWrite2("date", "9ZZ",
	                          text_logged_filtered);
	if (result != XPAI_XHL_FILTERED) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Write three more entries (ID 91Z, 9Z1, 011)\n",
	       test_no, sub_test++);
	result = XPAI_HWLogWrite2("date", "91Z", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d for ID 91Z\n", result);
		status = -1;
		goto exit;
	}
	result = XPAI_HWLogWrite2("date", "9Z1", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d for ID 9Z1\n", result);
		status = -1;
		goto exit;
	}
	result = XPAI_HWLogWrite2("date", "011", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d for ID 011\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Read back and see that we have four + three log entries\n",
	       test_no, sub_test++);
	log = XPAI_HWLogRead(XPAI_XHL_AREA_REPAIR);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogRead returned %d\n", log->result);
		status = -1;
		goto exit;
	}

	for (i = 0; i < log->noOfEntries; i++) {
		printf("\t     %s\n", log->list[i]);
	}

	if (log->noOfEntries != 7) {
		printf("ERROR: Invalid amount of entries %d, expected 4\n",
		       log->noOfEntries);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Check that entries is what we wrote\n",
	       test_no, sub_test++);
	if (strstr(log->list[0], text_logged) == NULL) {
		printf("ERROR: Entry 0 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[0], text_logged);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[1], text_logged_alt) == NULL) {
		printf("ERROR: Entry 1 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[1], text_logged_alt);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[2], text_logged) == NULL) {
		printf("ERROR: Entry 2 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[2], text_logged);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[3], text_logged_alt) == NULL) {
		printf("ERROR: Entry 3 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[3], text_logged_alt);
		status = -1;
		goto exit;
	}

	for (i = 0; i < 3; i++) {
		if (strstr(log->list[4 + i], text_logged_alt) == NULL) {
			printf("ERROR: Entry %d \"%s\" does not contain what was "
			       "written \"%s\"\n", 4 + i,
			       log->list[4 + i], text_logged_alt);
			status = -1;
			goto exit;
		}
	}

exit:
	if (log != NULL) itc_free((union itc_msg **)&log);
	return status;
}

static int test_hwlog_read_specific(int test_no)
{
	struct XPAI_HWLogS *log = NULL;
	U32 i, result, status = 0, sub_test = 0;
	char *text_logged = "This text should be logged";
	char *text_logged_alt = "This text should be also be logged";

	printf("\t%d.%d: Write one HW log (ID 0ZZ)\n", test_no, sub_test++);
	result = XPAI_HWLogWrite2("date", "0ZZ", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Write three hwfault hwlog entries (ID 9ZZ)\n",
	       test_no, sub_test++);
	for (i = 0; i < 3; i++) {
		result = XPAI_HWLogWrite2("date", "9ZZ", text_logged);
		if (result != XPAI_XHL_OK) {
			printf("XPAI_HWLogWrite2 returned %d\n", result);
			status = -1;
			goto exit;
		}
	}

	printf("\t%d.%d: Write some more HW logs (ID 8ZZ and 91Y)\n", test_no,
	       sub_test++);
	result = XPAI_HWLogWrite2("date", "8ZZ", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}
	result = XPAI_HWLogWrite2("date", "91Y", text_logged_alt);
	if (result != XPAI_XHL_OK) {
		printf("XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Read back and see that we have three log entries for 9ZZ\n",
	       test_no, sub_test++);
	log = XPAI_HWLogReadSpecifiedId("9ZZ");
	if (log == NULL || log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogReadSpecifiedId returned %d\n", result);
		status = -1;
		goto exit;
	}

	for (i = 0; i < log->noOfEntries; i++) {
		printf("\t     %s\n", log->list[i]);
	}

	if (log->noOfEntries != 3) {
		printf("ERROR: Invalid amount of entries %d, expected 3\n",
		       log->noOfEntries);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Check that entries is what we wrote\n",
	       test_no, sub_test++);
	if (strstr(log->list[0], text_logged) == NULL) {
		printf("ERROR: Entry 0 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[0], text_logged);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[1], text_logged) == NULL) {
		printf("ERROR: Entry 1 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[1], text_logged);
		status = -1;
		goto exit;
	}
	if (strstr(log->list[2], text_logged) == NULL) {
		printf("ERROR: Entry 2 \"%s\" does not contain what was "
		       "written \"%s\"\n",
		       log->list[2], text_logged);
		status = -1;
		goto exit;
	}

exit:
	if (log != NULL) itc_free((union itc_msg **)&log);
	return status;
}

static int test_hwlog_negative(int test_no)
{
	struct XPAI_HWLogS *log = NULL;
	U32 i, result, status = 0, sub_test = 0;
	char *text_logged = "This text should be not be logged";
	char text_logged_too_long[128] = {0};
	char *str_copy = NULL;

	printf("\t%d.%d: Write with invalid ID (0000, ???, A0Z and 0a0)\n", test_no,
	       sub_test++);
	result = XPAI_HWLogWrite2("date", "0000", text_logged);
	if (result != XPAI_XHL_NOT_OK) {
		printf("ERROR: Invalid ID 0000 accepted in write\n");
		status = -1;
		goto exit;
	}

	result = XPAI_HWLogWrite2("date", "???", text_logged);
	if (result != XPAI_XHL_NOT_OK) {
		printf("ERROR: Invalid ID ??? accepted in write\n");
		status = -1;
		goto exit;
	}

	result = XPAI_HWLogWrite2("date", "A0Z", text_logged);
	if (result != XPAI_XHL_NOT_OK) {
		printf("ERROR: Invalid ID A0Z accepted in write\n");
		status = -1;
		goto exit;
	}

	result = XPAI_HWLogWrite2("date", "0a0", text_logged);
	if (result != XPAI_XHL_NOT_OK) {
		printf("ERROR: Invalid ID 0a0 accepted in write\n");
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Read with invalid area (2)\n", test_no, sub_test++);
	log = XPAI_HWLogRead(XPAI_XHL_AREA_SYSTEM + 1);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_NOT_OK) {
		printf("XPAI_HWLogRead returned %d despite invalid area %d\n",
		       log->result, XPAI_XHL_AREA_SYSTEM + 1);
		status = -1;
		goto exit;
	}
	if (log != NULL) itc_free((union itc_msg **)&log);

	printf("\t%d.%d: Read log and see that we have no log entries\n",
	       test_no, sub_test++);
	log = XPAI_HWLogRead(XPAI_XHL_AREA_REPAIR);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogRead returned %d\n", log->result);
		status = -1;
		goto exit;
	}

	for (i = 0; i < log->noOfEntries; i++) {
		printf("\t     %s\n", log->list[i]);
	}

	if (log->noOfEntries != 0) {
		printf("ERROR: Invalid amount of entries %d, expected 0\n",
		       log->noOfEntries);
		status = -1;
		goto exit;
	}

	printf("\t%d.%d: Write entry with too long msg string, should be truncated\n",
	       test_no, sub_test++);
	for (i = 0; i < sizeof(text_logged_too_long) - 1; i++) {
		text_logged_too_long[i] = '0' + (i % 10);
	}

	str_copy = strdup(text_logged_too_long);
	if (str_copy == NULL) {
		printf("ERROR: Failed to strdup\n");
		status = -1;
		goto exit;
	}

	result = XPAI_HWLogWrite2("date", "9ZZ", text_logged_too_long);
	if (result == XPAI_XHL_NOT_OK) {
		printf("ERROR: XPAI_HWLogWrite2 returned %d\n", result);
		status = -1;
		goto exit;
	}

	if(strcmp(str_copy, text_logged_too_long) != 0) {
		printf("String is modified %s != %s\n", text_logged_too_long, str_copy);
		status = -1;
		goto exit;
	}

	printf("\t\t\t  Wrote: %s\n", text_logged_too_long);

	if (log != NULL) itc_free((union itc_msg **)&log);
	log = XPAI_HWLogRead(XPAI_XHL_AREA_REPAIR);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogRead returned %d\n", log->result);
		status = -1;
		goto exit;
	}

	for (i = 0; i < log->noOfEntries; i++) {
		printf("\t     %s\n", log->list[i]);
	}

	if (log->noOfEntries != 1) {
		printf("ERROR: Invalid amount of entries %d, expected 1\n",
		       log->noOfEntries);
		status = -1;
		goto exit;
	}

exit:
	if (str_copy != NULL) free(str_copy);
	if (log != NULL) itc_free((union itc_msg **)&log);
	return status;
}

static char *int_to_base36(int id)
{
	char base36[36] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	static char buffer[] = "000";
	unsigned int offset = sizeof(buffer);

	buffer[--offset] = '\0';
	do {
		buffer[--offset] = base36[id % 36];
	} while (id /= 36);

	return buffer;
}

static int test_hwlog_many_entries(int test_no)
{
	struct XPAI_HWLogS *log = NULL;
	U32 i, result, id, status = 0, sub_test = 0, iterations = 2050;
	char *text_logged = "This text should be be logged";

	printf("\t%d.%d: Write %d log entries\t", test_no,
			sub_test++, iterations);
	for (i = 0; i < iterations; i++) {
		id = i + 6;
		result = XPAI_HWLogWrite2("20151022 081800", int_to_base36(id), text_logged);
		if (i%10) {
			/* Indicate some progress, so user don't think we are hanging */
			printf("."); fflush(stdout);
		}
	}
	printf("\n");
	if (result != XPAI_XHL_FULL) {
		printf("ERROR: Last XPAI_HWLogWrite2 returned %d\n", result);
		fflush(stdout);
		status = -1;
		goto exit;
	}

	log = XPAI_HWLogRead(XPAI_XHL_AREA_REPAIR);
	if (log == NULL) {
		printf("XPAI_HWLogRead returned NULL\n");
		status = -1;
		goto exit;
	} else if (log->result != XPAI_XHL_OK) {
		printf("XPAI_HWLogRead returned %d\n", log->result);
		status = -1;
		goto exit;
	}

	for (i = 0; i < log->noOfEntries; i++) {
		printf("\t     %s\n", log->list[i]);
	}

	if (log->noOfEntries != XPAI_XHL_NO_SYSTEM_ENTRIES) {
		printf("ERROR: Invalid amount of entries %d, expected XPAI_XHL_NO_SYSTEM_ENTRIES\n",
		       log->noOfEntries);
		status = -1;
		goto exit;
	}

	exit:
	if (log != NULL) itc_free((union itc_msg **)&log);
	return status;
}

static void print_usage(char *pgm)
{
	printf("Usage: %s -tc <test case number>\n\n"
	       "\twhere <test case number> is one of:\n"
	       "\t1: Write/read configuration entries\n"
	       "\t2: Write/read operator entries\n"
	       "\t3: Write/read HW fault entries\n"
	       "\t4: Read specific entries\n"
	       "\t5: Negative tests\n"
	       "\t6: Many configuration entries\n",
		   pgm);
}

int main(int argc, char **argv)
{
	int status = EXIT_SUCCESS;
	int result = 0;
	int tc = 0;

	if (argc < 3) {
		print_usage(argv[0]);
		exit(-1);
	}

	if (argc >= 3) {
		if (strstr(argv[1], "-tc") != NULL) {
			tc = atoi(argv[2]);
		}
	}

	/* Set up */
	if(init_itc()) {
		return 1;
	}

	/* Tests */
	if (tc >= 0 && tc <= 6) {
		/* This test needs the hwlog to be cleared */
		printf("Clearing hwlog...");
		fflush(stdout);
		if (!test_hwlog_clear()) {
			printf("FAILED!\n");
			result = -1;
			goto exit;
		}
		printf("DONE!\n");
	}

	switch (tc) {
	case 1:
		printf("%d Test writing configuration entries:\n", tc);
		fflush(stdout);
		result = test_hwlog_write_conf(tc);
		break;
	case 2:
		printf("%d Test writing operator entries:\n", tc);
		fflush(stdout);
		result = test_hwlog_write_op(tc);
		break;
	case 3:
		printf("%d Test writing HW fault entries:\n", tc);
		fflush(stdout);
		result = test_hwlog_write_hwfault(tc);
		break;
	case 4:
		printf("%d Test read specific entries:\n", tc);
		fflush(stdout);
		result = test_hwlog_read_specific(tc);
		break;
	case 5:
		printf("%d Test negative tests:\n", tc);
		fflush(stdout);
		result = test_hwlog_negative(tc);
		break;
	case 6:
		printf("%d Test many configuration entries:\n", tc);
		fflush(stdout);
		result = test_hwlog_many_entries(tc);
		break;
	default:
		printf("Invalid tc number %d\n", tc);
		print_usage(argv[0]);
		result = -1;
		goto exit;
	}


exit:
	/* Tear down */
	delete_itc();

	if(result) {
		printf("ERROR!\n");
		status = EXIT_FAILURE;
	} else {
		printf("SUCCESS!\n");
	}

	return status;
}

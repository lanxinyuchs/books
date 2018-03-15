#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <itc.h>
#include "common.h"

/* XPP includes */
#include "xpai_xhl_if.h"

/* Logging */
#include "log_tpt.h"

/* CRL HWLOG */
#include "hwli.h"

#define _UNUSED_ __attribute__((__unused__))
#define XPAI_HWLOG_FILTER_CONF 1
#define XPAI_HWLOG_FILTER_OP 1
#define XPAI_HWLOG_FILTER_HWFAULT 4
#define XPAI_HWLOG_ID_LEN 3
#define XPAI_HWLOG_MSG_LEN 105 /* gived 106 with trailing \0 */

/* TBD: Open issues:
 * 3.  According to IWD, the user of write should be able to set date/time.
 *     crl-hwlog has no interface for that, so it is ignored.
 *     [Göran] The date is supplied in ELIB_BC_HWLOG_SET_REQ and CBCI_HW_LOG_WRITE_REQ which
 *     at least is defined as an valid signal from DU. So the consequence will be that date
 *     will be discarded for those. I think that this is OK because I can not see the use case
 *     where DU should write in Radios HW Log.
 * 4.  It has been decided(?) that XPAI_HWLogErase should not be implemented.
 *     Should it still be stubbed or removed completely?
 *     [Göran] For now keep it and return OK.
 *     I would like to remove it and that Radio SW returns
 *     SUCCESS to client but that needs to be further discussed.
 * 5.  Design rules says that entry no should start from 2. Now it starts from 0.
 *     Open
 * Closed issues:
 * 1.  According to IWD, write should be able to return "full".
 *     This is not implemented in crl-hwlog. Added as Bugzilla 2814
 * 2.  According to IWD, write should be able to return "filtered".
 *     This is not implemented in crl-hwlog. Added as Bugzilla 2814
 */

/**
 * hwlog_write
 */
static U32 hwlog_write(_UNUSED_ char *date_time_str,
                       char *log_id_str,
                       const char *log_str)
{
	uint32_t i, result;
	char *my_log_str;
	int hwli_result;

	/* Check parameters */
	if(log_id_str == NULL || log_str == NULL) {
		TPT_ERROR(STR("NUll pointer given "
		              "(log_id_str = %p, log_str = %p)",
		              (void *)log_id_str, log_str));
		return XPAI_XHL_NOT_OK;
	}

	if (strlen(log_id_str) != (HWLI_ID_SZ - 1)) {
		TPT_ERROR(STR("ID string %s is not valid!", log_id_str));
		return XPAI_XHL_NOT_OK;
	}

	for (i = 0; i < (HWLI_ID_SZ - 1); i++) {
		if (i == 0) {
			if (log_id_str[i] < '0' || log_id_str[i] > '9') {
				TPT_ERROR(STR("ID string %s is not valid!", log_id_str));
				return XPAI_XHL_NOT_OK;
			}
		} else {
			if ((log_id_str[i] < '0' || log_id_str[i] > '9') &&
			    (log_id_str[i] < 'A' || log_id_str[i] > 'Z')) {
				TPT_ERROR(STR("ID string %s is not valid!", log_id_str));
				return XPAI_XHL_NOT_OK;
			}
		}
	}

	/* Make a local copy, so we can truncate if needed */
	my_log_str = strdup(log_str);
	if(my_log_str == NULL) {
		TPT_ERROR("Failed to strdup!");
		return XPAI_XHL_NOT_OK;
	}

	if (strlen(my_log_str) > XPAI_HWLOG_MSG_LEN) {
		/* Too long, truncate */
		my_log_str[XPAI_HWLOG_MSG_LEN] = 0;
	}

	/* Write with filter based on ID */
	if (strncmp(log_id_str, "000", XPAI_HWLOG_ID_LEN) == 0
	    || strncmp(log_id_str, "002", XPAI_HWLOG_ID_LEN) == 0) {
		/* Configuration entry */
		TPT_TRACE(1, STR("Write configuration entry \"%s\" with ID \"%s\"",
		                 my_log_str, log_id_str));
		hwli_result = hwli_write(log_id_str,
		                         XPAI_HWLOG_FILTER_CONF /*filter*/,
		                         1 /*filter_on_msg*/, my_log_str);
	} else if (strncmp(log_id_str, "001", XPAI_HWLOG_ID_LEN) == 0) {
		/* Operator entry */
		TPT_TRACE(1, STR("Write operator entry \"%s\" with ID \"%s\"",
		                 my_log_str, log_id_str));
		hwli_result = hwli_write(log_id_str,
		                         XPAI_HWLOG_FILTER_OP /*filter*/,
		                         0 /*filter_on_msg*/, my_log_str);
	} else if (strncmp(log_id_str, "003", XPAI_HWLOG_ID_LEN) == 0
	           || strncmp(log_id_str, "004", XPAI_HWLOG_ID_LEN) == 0
	           || strncmp(log_id_str, "005", XPAI_HWLOG_ID_LEN) == 0) {
		/* ID 003 to 005 are reserved in crl-hwlog, so we can not use them
		 * in XPAI. Log error.
		 */
		TPT_INFO(STR("Unsupported ID %s was used, "
		             "nothing got logged", log_id_str));
		hwli_result = HWLI_ERROR;
	} else {
		/* HW fault entry */
		TPT_TRACE(1, STR("Write HW fault entry \"%s\" with ID \"%s\"",
		                 my_log_str, log_id_str));
		hwli_result = hwli_write(log_id_str,
		                         XPAI_HWLOG_FILTER_HWFAULT /*filter*/,
		                         0 /*filter_on_msg*/, my_log_str);
	}

	/* Check and convert result */
	if (hwli_result == HWLI_SUCCESS) {
		result = XPAI_XHL_OK;
	} else if (hwli_result == HWLI_FILTERED) {
		result = XPAI_XHL_FILTERED;
	} else if (hwli_result == HWLI_LOG_FULL) {
		result = XPAI_XHL_FULL;
	} else {
		/* Unexpected return from hwli */
		TPT_ERROR(STR("HWLI returned %d", hwli_result));
		result = XPAI_XHL_NOT_OK;
	}

	free(my_log_str);

	return result;
}

/**
 * XPAI_HWLogWrite2
 */
U32 XPAI_HWLogWrite2(char *dateTimeStr,
                     char *logIdStr,
                     char *logStr)
{
	return hwlog_write(dateTimeStr, logIdStr, logStr);
}

/**
 * format_hwlog
 */
static void format_hwlog(uint32_t entry_no, char *id, char *date_time,
                         char *msg, char *outbuf)
{
	char *byte_ptr = outbuf;
	size_t current_len, max_write_len;

	/* Format from 39/102 60-CSX 101 09 Uen
	 * <entry no> <date> <time> <log id>;<entry>*/

	/* Add entry number */
	snprintf(byte_ptr, XPAI_XHL_ENTRY_LENGTH, "%d", entry_no);
	byte_ptr += strlen(outbuf);

	/* Add space */
	*byte_ptr = ' ';
	byte_ptr++;

	/* Add date */
	memcpy(byte_ptr, date_time, 6);
	byte_ptr += 6;

	/* Add space */
	*byte_ptr = ' ';
	byte_ptr++;

	/* Add time */
	memcpy(byte_ptr, &date_time[6], 6);
	byte_ptr += 6;

	/* Add space */
	*byte_ptr = ' ';
	byte_ptr++;

	/* Add ID */
	memcpy(byte_ptr, id, HWLI_ID_SZ - 1);
	byte_ptr += HWLI_ID_SZ - 1;

	/* Add ; */
	*byte_ptr = ';';
	byte_ptr++;

	/* Check length */
	current_len = strlen(outbuf);
	max_write_len = XPAI_XHL_ENTRY_LENGTH - current_len;

	/* Add message */
	if(strlen(msg) > max_write_len) {
		memcpy(byte_ptr, msg, max_write_len);

	} else  {
		memcpy(byte_ptr, msg, strlen(msg));
	}
}

/**
 * hwlog_read
 */
static struct XPAI_HWLogS *hwlog_read(U32 area, int use_id, char *id)
{
	struct hwli_entry *entries;
	struct hwli_entry *current_entry;
	uint32_t size = 0;
	uint32_t offset = 0;
	struct XPAI_HWLogS *hwlog = NULL;
	uint32_t i;

	hwlog = (struct XPAI_HWLogS *)itc_alloc(sizeof(struct XPAI_HWLogS), 0);
	memset(hwlog, 0, sizeof(struct XPAI_HWLogS));

	/* Check parameters */
	if (area != XPAI_XHL_AREA_REPAIR &&
	    area != XPAI_XHL_AREA_SYSTEM) {
		TPT_ERROR(STR(" Unknown area %d", area));
		hwlog->result = XPAI_XHL_NOT_OK;
		return hwlog;
	}

	/* Read log */
	hwlog->result = hwli_readlog(&entries, &size);

	/* Convert result */
	if (hwlog->result == HWLI_SUCCESS) {
		hwlog->result = XPAI_XHL_OK;
	} else {
		size = 0; /* Don't copy any entries */
		hwlog->result = XPAI_XHL_NOT_OK;
	}

	/* If more entries than XPAI_XHL_NO_SYSTEM_ENTRIES, take the
	 * XPAI_XHL_NO_SYSTEM_ENTRIES latest entries */
	if(size > XPAI_XHL_NO_SYSTEM_ENTRIES) {
		offset = size - XPAI_XHL_NO_SYSTEM_ENTRIES;
		size = XPAI_XHL_NO_SYSTEM_ENTRIES;
	}

	/* Copy entries */
	for (i = 0; i < size; i++) {
		current_entry = entries + offset + i;
		if (!use_id ||
		    (use_id &&
		     (strncmp(id, current_entry->id, HWLI_ID_SZ - 1) == 0))) {
			/* Add entry */
			format_hwlog(offset + i, current_entry->id,
			             current_entry->time,
			             current_entry->msg,
			             hwlog->list[hwlog->noOfEntries]);
			hwlog->noOfEntries ++;
		}
	}

	free(entries);
	return hwlog;
}

/**
 * XPAI_HWLogRead
 */
struct XPAI_HWLogS *XPAI_HWLogRead(U32 area) /* !- FUNC -! */
{
	return hwlog_read(area, 0, 0);
}

/**
 * XPAI_HWLogReadSpecifiedId
 */
struct XPAI_HWLogS *XPAI_HWLogReadSpecifiedId(char *logId) /* !- FUNC -! */
{
	return hwlog_read(0, 1, logId);
}

/**
 * XPAI_HWLogErase
 */
U32 XPAI_HWLogErase(U32 _UNUSED_ area) /* !- FUNC -! */
{
	return 0;
}

/**
 * xpai_hwlog_init
 */
int32_t xpai_hwlog_init(void)
{
	return INIT_OK;
}

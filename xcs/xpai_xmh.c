/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <string.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>
#include <itc.h>
#include <pthread.h>
#include "lmc_server.h"
#include "xpai_xmh_if.h"
#include "xpai_xcbc_loadable_au_if.h"
#include "common.h"
#include "rhd-common.h"
#include "conn-establish-helper.h"
#include "log_tpt.h"

#define XMH_INIT_MBOX "xmh_init"
#define XMH_ADAPTER_MBOX "xmh_adapter"
#define _UNUSED_ __attribute__((__unused__))

static struct server_info lmc_conn;
static bool initialized = false;
static pthread_once_t once_control = PTHREAD_ONCE_INIT;
static pthread_mutex_t conn_complete_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t conn_complete;
static uint32_t client_ref = 1243;
static itc_mbox_id_t adapter_mbox = ITC_NO_ID;



union itc_msg {
	uint32_t msgno;
	LMC_SERVER_MESSAGES;
	struct XPAI_LoadFileDataIndS   data_ind;
	struct XPAI_LoadFileDeleteIndS delete_ind;
};

/****
 *
 *      Function XPAI_LoadFileList
 *
 *****/
struct XPAI_LoadFileListS *XPAI_LoadFileList(void)
{
	uint32_t i;
	struct XPAI_LoadFileListS *lmc_list = NULL;
	struct XPAI_LoadFileList2S *lmc_list2 = XPAI_LoadFileList2();

	TPT_TRACE(1, "XPAI_LoadFileList");
	if (lmc_list2 == NULL)
		return lmc_list;

	lmc_list = (struct XPAI_LoadFileListS *)
		   itc_alloc(sizeof(struct XPAI_LoadFileListS), 0);
	if (lmc_list == NULL) {
		TPT_ERROR("Malloc failed for lmc_list");
		goto load_file_list_end;
	}
	lmc_list->result = lmc_list2->result;

	for (i = 0; i < XPAI_XMH_MAX_LOADFILES; i++) {
		lmc_list->info[i].state = lmc_list2->info[i].state;
		lmc_list->info[i].isCurrentLoadFile =
			lmc_list2->info[i].isCurrentLoadFile;
		lmc_list->info[i].size = lmc_list2->info[i].size;
		lmc_list->info[i].lmc_size = lmc_list2->info[i].lmc_size;
		lmc_list->info[i].seqNo = lmc_list2->info[i].seqNo;
		lmc_list->info[i].fileType = lmc_list2->info[i].fileType;
		lmc_list->info[i].prodDateTime = lmc_list2->info[i].prodDateTime;
		lmc_list->info[i].noLoadSubFiles =
			lmc_list2->info[i].noLoadSubFiles;
		lmc_list->info[i].working = lmc_list2->info[i].working;
		memcpy(lmc_list->info[i].swPid,
		       lmc_list2->info[i].swPid,
		       XPAI_XMH_SWPID_LENGTH + 1);
	}

load_file_list_end:
	itc_free((union itc_msg **)&lmc_list2);
	return lmc_list;
}

/****
 *
 *      Function XPAI_LoadFileList2
 *
 *****/
struct XPAI_LoadFileList2S *XPAI_LoadFileList2(void)
{
	union itc_msg *msg;
	uint32_t i, lmc_count;
	struct XPAI_LoadFileList2S *lmc_list = NULL;

	static const uint32_t rx_filter[] = {1,
	                                     LMC_GET_LOAD_FILE_INFO_CFM
	                                    };

	TPT_TRACE(1, "XPAI_LoadFileList2");
	if (!initialized) {
		TPT_ERROR("Connection to server is not started.");
		return lmc_list;
	}
	/* send  request*/
	msg = itc_alloc(sizeof(struct lmc_get_load_file_info_req),
	                LMC_GET_LOAD_FILE_INFO_REQ);
	msg->get_load_file_info_req.connection_ref = lmc_conn.server_ref;
	msg->get_load_file_info_req.flags = 0;
	TPT_SEND_SIG(msg->msgno, lmc_conn.server_mbox,
	             "LMC_GET_LOAD_FILE_INFO_REQ");
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	TPT_REC_SIG(msg->msgno, "LMC_GET_LOAD_FILE_INFO_CFM");
	lmc_count = msg->get_load_file_info_cfm.lmc_count;
	lmc_list = (struct XPAI_LoadFileList2S *)
	           itc_alloc(sizeof(struct XPAI_LoadFileList2S), 0);
	if (lmc_list == NULL) {
		TPT_ERROR("Malloc failed for lmc_list");
		goto load_file_list2_end;
	}
	memset(lmc_list, 0, sizeof(struct XPAI_LoadFileList2S));
	lmc_list->result = XPAI_XMH_SUCCEED;

	for (i = 0; i < XPAI_XMH_MAX_LOADFILES; i++) {
		if (i >= lmc_count) {
			lmc_list->info[i].state = XPAI_XMH_STATE_NON_EXISTENT;
			continue;
		}
		TPT_TRACE(1, STR("info[%d]: state=%d isCurrentLoadFile=%d size=%d lmc_size=%d "
		          "seqNo=%d fileType=%d prodDateTime=%d swPid=%s "
		          "noLoadSubFiles=%d working=%d permissions=%d",
		          i,
		          msg->get_load_file_info_cfm.info[i].state,
		          msg->get_load_file_info_cfm.info[i].is_current,
		          msg->get_load_file_info_cfm.info[i].slot_size,
		          msg->get_load_file_info_cfm.info[i].lmc_size,
		          msg->get_load_file_info_cfm.info[i].seq_number,
		          msg->get_load_file_info_cfm.info[i].file_type,
		          msg->get_load_file_info_cfm.info[i].time,
		          msg->get_load_file_info_cfm.info[i].lmid,
		          msg->get_load_file_info_cfm.info[i].subfile_counter,
		          msg->get_load_file_info_cfm.info[i].working,
		          msg->get_load_file_info_cfm.info[i].permissions));
		switch (msg->get_load_file_info_cfm.info[i].state) {
		case LMC_STATE_VALID:
			lmc_list->info[i].state = XPAI_XMH_STATE_VALID;
			break;
		case LMC_STATE_ERASED:
			lmc_list->info[i].state = XPAI_XMH_STATE_ERASED;
			break;
		case LMC_STATE_ERPROG:
			lmc_list->info[i].state = XPAI_XMH_STATE_ERPROG;
			break;
		case LMC_STATE_WRPROG:
			lmc_list->info[i].state = XPAI_XMH_STATE_WRPROG;
			break;
		default:
			lmc_list->info[i].state = XPAI_XMH_STATE_ERROR;
			break;
		}
		if(lmc_list->info[i].state != XPAI_XMH_STATE_VALID) {
			continue;
		}
		switch (msg->get_load_file_info_cfm.info[i].file_type) {
		case XLF_IBOOT_HDR_TYPE_AU_BOOT:
			lmc_list->info[i].fileType = XPAI_XMH_FILETYPE_AUBOOT;
			break;
		case XLF_IBOOT_HDR_TYPE_AU_APPLIC:
			lmc_list->info[i].fileType = XPAI_XMH_FILETYPE_AUAPPLIC;
			break;
		default:
			TPT_ERROR(STR("Invalid file type %d",
			              msg->get_load_file_info_cfm.info[i].file_type));
			lmc_list->info[i].fileType = 0;
			break;
		}
		lmc_list->info[i].isCurrentLoadFile =
			msg->get_load_file_info_cfm.info[i].is_current;
		lmc_list->info[i].size =
			msg->get_load_file_info_cfm.info[i].slot_size;
                lmc_list->info[i].lmc_size =
			msg->get_load_file_info_cfm.info[i].lmc_size;
		lmc_list->info[i].seqNo =
			msg->get_load_file_info_cfm.info[i].seq_number;
		lmc_list->info[i].prodDateTime =
			msg->get_load_file_info_cfm.info[i].time;
		lmc_list->info[i].noLoadSubFiles =
			msg->get_load_file_info_cfm.info[i].subfile_counter;
		lmc_list->info[i].working =
			msg->get_load_file_info_cfm.info[i].working;
		lmc_list->info[i].locked = 0;
		if(msg->get_load_file_info_cfm.info[i].permissions &
		   LMC_LOAD_FILE_PERMISSION_LOCKED) {
			lmc_list->info[i].locked = 1;
		}
		memcpy(lmc_list->info[i].swPid,
			msg->get_load_file_info_cfm.info[i].lmid,
			XPAI_XMH_SWPID_LENGTH + 1);
	}

load_file_list2_end:

	itc_free(&msg);
	return lmc_list;
}

/****
 *
 *      Function XPAI_LoadSubFileList2
 *
 *****/
struct XPAI_SubFileListS *XPAI_LoadSubFileList2(U32 loadFileIx)
{
	union itc_msg *msg;
	uint32_t i, j, nof_load_subfiles;
	uint32_t magic;
	struct XPAI_SubFileListS *reply = NULL;

	static const uint32_t rx_filter[] = { 2,
	                                      LMC_GET_SUBFILE_INFO_CFM,
					      LMC_GET_SUBFILE_INFO_REJ };

	TPT_TRACE(1, "XPAI_LoadSubFileList2");
	if (!initialized) {
		TPT_ERROR("Connection to server is not started.");
		return reply;
	}
	msg = itc_alloc(sizeof(struct lmc_get_subfile_info_req),
	                LMC_GET_SUBFILE_INFO_REQ);
	msg->get_subfile_info_req.use_current = 0;
	msg->get_subfile_info_req.index = loadFileIx;
	msg->get_subfile_info_req.connection_ref = lmc_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, lmc_conn.server_mbox,
	             "LMC_GET_SUBFILE_INFO_REQ");
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);

	reply = (struct XPAI_SubFileListS *)
	        itc_alloc(sizeof(struct XPAI_SubFileListS), 0);
	memset(reply, 0, sizeof(struct XPAI_SubFileListS));
	if (msg->msgno == LMC_GET_SUBFILE_INFO_REJ) {
		reply->noLoadSubFiles = 0;
		switch (msg->get_subfile_info_rej.error_code) {
		case LMC_RESULT_INVALID_PARAM:
			reply->result = XPAI_XMH_ILLEGAL_INDEX;
			break;
		case LMC_RESULT_NOT_FOUND:
			reply->result = XPAI_XMH_NOT_VALID;
			break;
		default:
			reply->result = XPAI_XMH_FAIL;
			break;
		}
		goto lmlist_end;
	}
	nof_load_subfiles = msg->get_subfile_info_cfm.counter;

	for (i = 0, j = 0;
	     i < nof_load_subfiles && j < XPAI_XMH_MAX_SUBFILES;
	     i++) {
		magic = msg->get_subfile_info_cfm.list[i].magic;
		if (magic != XLF_IBOOT_WPR_MAGIC_BLOB) {
			reply->format[j] = XPAI_XMH_FORMAT_USERDATA;
			sprintf(reply->info[j], "%.3u %.3u %.3u %.3u",
			        (magic >> 24) & 0xff,
			        (magic >> 16) & 0xff,
			        (magic >> 8) & 0xff,
			        magic & 0xff);
			j++;
		}
	}
	reply->noLoadSubFiles = j;

lmlist_end:
	itc_free(&msg);
	return reply;
}

/****
 *
 *      Function XPAI_LoadSubFileOpen2
 *
 *****/
S32 XPAI_LoadSubFileOpen2(U32 loadFileIx, U32 loadSubFileIx)
{
	union itc_msg *msg;
	uint32_t i, j, nof_load_subfiles;
	int32_t result = XPAI_XMH_FAIL;
	static const uint32_t lm_filter[] = {2,
	                                     LMC_GET_SUBFILE_INFO_CFM,
					     LMC_GET_SUBFILE_INFO_REJ
					    };
	static const uint32_t rx_filter[] = {2,
	                                     LMC_LOAD_SUBFILE_OPEN_CFM,
	                                     LMC_LOAD_SUBFILE_OPEN_REJ
	                                    };
	TPT_TRACE(1, STR("XPAI_LoadSubFileOpen2(loadSubFileIx=%d)",
	                 loadSubFileIx));
	if (!initialized) {
		TPT_ERROR("Connection to server is not started.");
		return result;
	}
	/* Convert subfile index to server subfile list index */
	msg = itc_alloc(sizeof(struct lmc_get_subfile_info_req),
	                LMC_GET_SUBFILE_INFO_REQ);
	msg->get_subfile_info_req.use_current = 0;
	msg->get_subfile_info_req.index = loadFileIx;
	msg->get_subfile_info_req.connection_ref = lmc_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, lmc_conn.server_mbox,
	             "LMC_GET_SUBFILE_INFO_REQ");
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	msg = itc_receive(lm_filter, ITC_NO_TMO, ITC_FROM_ALL);
	if (msg->msgno == LMC_GET_SUBFILE_INFO_REJ) {
		switch (msg->get_subfile_info_rej.error_code) {
		case LMC_RESULT_INVALID_PARAM:
			result = XPAI_XMH_ILLEGAL_INDEX;
			break;
		case LMC_RESULT_NOT_FOUND:
			result = XPAI_XMH_NOT_VALID;
			break;
		default:
			result = XPAI_XMH_FAIL;
			break;
		}
		itc_free(&msg);
		return result;
	}
	nof_load_subfiles = msg->get_subfile_info_cfm.counter;
	for (i = 0, j = 0; i < nof_load_subfiles; i++) {
		if (msg->get_subfile_info_cfm.list[i].magic !=
		    XLF_IBOOT_WPR_MAGIC_BLOB) {
			if (j == loadSubFileIx)
				break;
			j++;
		}
	}
	itc_free(&msg);
	loadSubFileIx = i;

	msg = itc_alloc(sizeof(struct lmc_load_subfile_open_req),
	                LMC_LOAD_SUBFILE_OPEN_REQ);
	msg->load_subfile_open_req.lmc_index = loadFileIx;
	msg->load_subfile_open_req.lm_index = loadSubFileIx;
	msg->load_subfile_open_req.connection_ref = lmc_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, lmc_conn.server_mbox,
	             "LMC_LOAD_SUBFILE_OPEN_REQ");
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch(msg->msgno) {
	case LMC_LOAD_SUBFILE_OPEN_CFM:
		TPT_REC_SIG(msg->msgno, "LMC_LOAD_SUBFILE_OPEN_CFM");
		result = msg->load_subfile_open_cfm.handle;
		break;
	case LMC_LOAD_SUBFILE_OPEN_REJ:
		TPT_REC_SIG(msg->msgno, "LMC_LOAD_SUBFILE_OPEN_REJ");
		switch (msg->load_subfile_open_rej.error_code) {
		case LMC_RESULT_INVALID_PARAM:
			result = XPAI_XMH_ILLEGAL_INDEX;
			break;
		case LMC_RESULT_NOT_FOUND:
			result = XPAI_XMH_NOT_VALID;
			break;
		case LMC_RESULT_RESOURCE_SHORTAGE:
			result = XPAI_XMH_OUT_OF_HANDLES;
			break;
		case LMC_RESULT_OTHER_ERROR:
		default:
			result = XPAI_XMH_FAIL;
			break;
		}
		break;
	default:
		TPT_ERROR(STR("Unexpected msg 0x%x received from 0x%x",
			      msg->msgno, itc_sender(msg)));
	}

	itc_free(&msg);
	return result;
}

/****
 *
 *      Function XPAI_LoadSubFileRead2
 *
 *****/
S32 XPAI_LoadSubFileRead2(S32 handle, U8 *buffer, U32 pos, U32 size)
{
	union itc_msg *msg;
	int32_t  result = XPAI_XMH_FAIL;
	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_SUBFILE_READ_CFM,
	                                      LMC_LOAD_SUBFILE_READ_REJ
	                                    };
	TPT_TRACE(1, STR("XPAI_LoadSubFileRead2"
	                 "(handle=%d buffer=%p pos=%d size=%d)",
		         handle, buffer, pos, size));
	if (!initialized) {
		TPT_ERROR("Connection to server is not started.");
		return result;
	}

	msg = itc_alloc(sizeof(struct lmc_load_subfile_read_req),
	                LMC_LOAD_SUBFILE_READ_REQ);
	msg->load_subfile_read_req.handle = handle;
	msg->load_subfile_read_req.pos = pos;
	msg->load_subfile_read_req.size = size;
	msg->load_subfile_read_req.connection_ref = lmc_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, lmc_conn.server_mbox, "LMC_LOAD_SUBFILE_READ_REQ");
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch(msg->msgno) {
	case LMC_LOAD_SUBFILE_READ_CFM:
		TPT_REC_SIG(msg->msgno, "LMC_LOAD_SUBFILE_READ_CFM");
		result = msg->load_subfile_read_cfm.nof_read_bytes;
		memcpy(buffer, msg->load_subfile_read_cfm.buf, result);
		break;
	case LMC_LOAD_SUBFILE_READ_REJ:
		TPT_REC_SIG(msg->msgno, "LMC_LOAD_SUBFILE_READ_REJ");
		result = msg->load_subfile_read_rej.error_code;
		switch (result) {
		case LMC_RESULT_INVALID_PARAM:
			result = XPAI_XMH_ILLEGAL_HANDLE;
			break;
		case LMC_RESULT_NOT_FOUND:
			result = XPAI_XMH_NOT_VALID;
			break;
		case LMC_RESULT_OTHER_ERROR:
		default:
			result = XPAI_XMH_FAIL;
			break;
		}
		break;
	default:
		TPT_ERROR(STR("Unexpected msg %d received from 0x%x",
			      msg->msgno, itc_sender(msg)));
	}

	itc_free(&msg);
	return result;
}

/****
 *
 *      Function XPAI_LoadSubFileClose
 *
 *****/
S32 XPAI_LoadSubFileClose(S32 handle)
{
	union itc_msg *msg;
	int32_t  result = XPAI_XMH_FAIL;
	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_SUBFILE_CLOSE_CFM,
	                                      LMC_LOAD_SUBFILE_CLOSE_REJ
	                                    };
	TPT_TRACE(1, STR("XPAI_LoadSubFileClose(handle=%d)", handle));
	if (!initialized) {
		TPT_ERROR("Connection to server is not started.");
		return result;
	}

	msg = itc_alloc(sizeof(struct lmc_load_subfile_close_req),
	                LMC_LOAD_SUBFILE_CLOSE_REQ);
	msg->load_subfile_close_req.handle = handle;
	msg->load_subfile_close_req.connection_ref = lmc_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, lmc_conn.server_mbox,
	             "LMC_LOAD_SUBFILE_CLOSE_REQ");
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch(msg->msgno) {
	case LMC_LOAD_SUBFILE_CLOSE_CFM:
		TPT_REC_SIG(msg->msgno, "LMC_LOAD_SUBFILE_CLOSE_CFM");
		result = XPAI_XMH_SUCCEED;
		break;
	case LMC_LOAD_SUBFILE_CLOSE_REJ:
		TPT_REC_SIG(msg->msgno, "LMC_LOAD_SUBFILE_CLOSE_REJ");
		result = XPAI_XMH_ILLEGAL_HANDLE;
		break;
	default:
		TPT_ERROR(STR("Unexpected msg %d received from 0x%x",
			      msg->msgno, itc_sender(msg)));
	}

	itc_free(&msg);
	return result;
}

/****
 *
 *      Function XPAI_LoadFileInit
 *
 *****/
U32 XPAI_LoadFileInit(char *loadModule, U32 *maxBlockSize, U32 pid)
{
	union itc_msg *out_msg;
	union itc_msg *in_msg;
	char *empty_string = "";
	char *lmc;
	uint32_t lmc_len, max_len;
	uint32_t ret = XPAI_LOAD_FILE_INIT_OK;

	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_INIT_CFM,
	                                      LMC_LOAD_FILE_INIT_REJ
	                                    };

	if (!initialized) {
		TPT_ERROR("Connection to lmc server is not started.");
		return XPAI_LOAD_FILE_INIT_NOK_SERVER;
	}
	if (adapter_mbox == ITC_NO_ID) {
		TPT_ERROR("Adapter thread is not started.");
		return XPAI_LOAD_FILE_INIT_NOK_OTHER;
	}
	/* Check loadModule. */
	if (loadModule == NULL)
		lmc = empty_string;
	else
		lmc = loadModule;

	TPT_TRACE(1, STR("XPAI_LoadFileInit(loadModule=%s "
	                 "maxBlockSize=%p pid=0x%x)",
	                 lmc, (void *)maxBlockSize, pid));

	lmc_len = strlen(lmc);
	max_len = XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH - 1;
	if ((lmc_len == 0) || (lmc_len > max_len)) {
		TPT_ERROR(STR("XPAI_LoadFileInit() received"
		              " illegal pid length: %d.",
		              lmc_len));
		ret = XPAI_LOAD_FILE_INIT_NOK_WRONG_PARAM;
	}

	/* Send request. */
	out_msg = itc_alloc(sizeof(struct lmc_load_file_init_req) + lmc_len + 1,
	                    LMC_LOAD_FILE_INIT_REQ);
	strncpy(out_msg->load_file_init_req.loadmodule, lmc, lmc_len + 1);
	out_msg->load_file_init_req.procedure_ref = pid;
	out_msg->load_file_init_req.connection_ref = lmc_conn.server_ref;
	itc_send(&out_msg, adapter_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (in_msg->msgno) {
	case LMC_LOAD_FILE_INIT_CFM:
		*maxBlockSize = in_msg->load_file_init_cfm.max_block_size;
		break;
	case LMC_LOAD_FILE_INIT_REJ:
		ret = in_msg->load_file_init_rej.error_code;
		switch (ret) {
		case LMC_RESULT_INVALID_PARAM:
			ret = XPAI_LOAD_FILE_INIT_NOK_WRONG_PARAM;
			break;
		case LMC_RESULT_WRONG_STATE:
			ret = XPAI_LOAD_FILE_INIT_NOK_WRONG_STATE;
			break;
		case LMC_RESULT_WRONG_CONFIG_DATA:
			ret = XPAI_LOAD_FILE_INIT_NOK_WRONG_CONFIG_DATA;
			break;
		case LMC_RESULT_RESOURCE_SHORTAGE:
			ret = XPAI_LOAD_FILE_INIT_NOK_RESOURCE_SHORTAGE;
			break;
		default:
			ret = XPAI_LOAD_FILE_INIT_NOK_OTHER;
			break;
		}
		break;
	default:
		TPT_ERROR(STR("Unexpected signal (msgno=0x%X) received from 0x%X.",
		              in_msg->msgno, itc_sender(in_msg)));
		ret = XPAI_LOAD_FILE_INIT_NOK_OTHER;
		break;
	}
	itc_free(&in_msg);

	return ret;
}

/****
 *
 *      Function XPAI_LoadFileData
 *
 *****/
U32 XPAI_LoadFileData(U32 lmBlockSize, U16 lmSeqNr, char *lmBlock)
{
	union itc_msg *out_msg;
	union itc_msg *in_msg;
	uint32_t ret = XPAI_LOAD_FILE_DATA_OK;

	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_DATA_CFM,
	                                      LMC_LOAD_FILE_DATA_REJ
	                                    };

	if (!initialized) {
		TPT_ERROR("Connection to lmc server is not started.");
		return XPAI_LOAD_FILE_DATA_NOK_SERVER;
	}
	if (adapter_mbox == ITC_NO_ID) {
		TPT_ERROR("Adapter thread is not started.");
		return XPAI_LOAD_FILE_DATA_NOK_OTHER;
	}
	TPT_TRACE(1, STR("XPAI_LoadFileData"
	                 "(lmBlockSize=%d lmSeqNr=%d lmBlock=%p)",
	                 lmBlockSize, lmSeqNr, (void *)lmBlock));
	/* If wrong lmBlockSize. */
	if (lmBlockSize > XPAI_LOAD_FILE_MAX_LM_BLOCKSIZE) {
		TPT_ERROR(STR("XPAI_LoadFileData() received"
		              " illegal lmBlockSize: 0x%X",
		              lmBlockSize));
		ret = XPAI_LOAD_FILE_DATA_NOK_WRONG_PARAM;
		return ret;
	}

	/* Send request. */
	out_msg = itc_alloc(sizeof(struct lmc_load_file_data_req) + lmBlockSize,
	                    LMC_LOAD_FILE_DATA_REQ);
	out_msg->load_file_data_req.lm_block_size = lmBlockSize;
	out_msg->load_file_data_req.lm_seq_nr = lmSeqNr;
	out_msg->load_file_data_req.connection_ref = lmc_conn.server_ref;
	memcpy(out_msg->load_file_data_req.lm_block, lmBlock, lmBlockSize);
	itc_send(&out_msg, adapter_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (in_msg->msgno) {
	case LMC_LOAD_FILE_DATA_CFM:
		break;
	case LMC_LOAD_FILE_DATA_REJ:
		ret = in_msg->load_file_data_rej.error_code;
		switch (ret) {
		case LMC_RESULT_INVALID_PARAM:
			ret = XPAI_LOAD_FILE_DATA_NOK_WRONG_PARAM;
			break;
		case LMC_RESULT_WRONG_STATE:
			ret = XPAI_LOAD_FILE_DATA_NOK_WRONG_STATE;
			break;
                case LMC_RESULT_WRONG_SEQ_NR:
			ret = XPAI_LOAD_FILE_DATA_NOK_WRONG_SEQ_NR;
			break;
		default:
			ret = XPAI_LOAD_FILE_DATA_NOK_OTHER;
			break;
		}
		break;
	default:
		TPT_ERROR(STR("Unexpected signal (msgno=0x%X) received from 0x%X.",
		              in_msg->msgno, itc_sender(in_msg)));
		ret = XPAI_LOAD_FILE_DATA_NOK_OTHER;
		break;
	}
	itc_free(&in_msg);

	return ret;
}

/****
 *
 *      Function XPAI_LoadFileEnd
 *
 *****/
U32 XPAI_LoadFileEnd(U16 *loadResult)
{
	union itc_msg *out_msg;
	union itc_msg *in_msg;
	uint32_t ret;

	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_END_CFM,
	                                      LMC_LOAD_FILE_END_REJ
	                                    };

	if (!initialized) {
		TPT_ERROR("Connection to lmc server is not started.");
		return XPAI_LOAD_FILE_END_NOK_SERVER;
	}
	if (adapter_mbox == ITC_NO_ID) {
		TPT_ERROR("Adapter thread is not started.");
		return XPAI_LOAD_FILE_DATA_NOK_OTHER;
	}
	/* Send request. */
	out_msg = itc_alloc(sizeof(struct lmc_load_file_end_req),
	                    LMC_LOAD_FILE_END_REQ);
	out_msg->load_file_end_req.connection_ref = lmc_conn.server_ref;
	itc_send(&out_msg, adapter_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (in_msg->msgno) {
	case LMC_LOAD_FILE_END_CFM:
		ret = in_msg->load_file_end_cfm.result;
		if (ret == LMC_RESULT_SUCCESS)
			*loadResult = XPAI_LOAD_FILE_END_RESULT_LOAD_COMPLETED;
		else
			*loadResult = XPAI_LOAD_FILE_END_RESULT_LOAD_ABORTED;
		ret = XPAI_LOAD_FILE_END_OK;
		break;
	case LMC_LOAD_FILE_END_REJ:
		ret = in_msg->load_file_end_rej.error_code;
		switch (ret) {
		case LMC_RESULT_WRONG_STATE:
			ret = XPAI_LOAD_FILE_END_NOK_WRONG_STATE;
			break;
		default:
			ret = XPAI_LOAD_FILE_END_NOK_OTHER;
			break;
		}
		break;
	default:
		TPT_ERROR(STR("Unexpected signal (msgno=0x%X) received from 0x%X.",
		              in_msg->msgno, itc_sender(in_msg)));
		ret = XPAI_LOAD_FILE_END_NOK_OTHER;
		break;
	}
	itc_free(&in_msg);

	return ret;
}

/****
 *
 *      Function XPAI_LoadFileDelete
 *
 *****/
U32 XPAI_LoadFileDelete(char *loadModule, U32 pid)
{
	union itc_msg *out_msg;
	union itc_msg *in_msg;
	char *empty_string = "";
	char *lmc;
	uint32_t lmc_len, max_len;
	uint32_t ret = XPAI_LOAD_FILE_DELETE_OK;

	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_DELETE_CFM,
	                                      LMC_LOAD_FILE_DELETE_REJ
	                                    };

	if (!initialized) {
		TPT_ERROR("Connection to lmc server is not started.");
		return XPAI_LOAD_FILE_DELETE_NOK_SERVER;
	}
	if (adapter_mbox == ITC_NO_ID) {
		TPT_ERROR("Adapter thread is not started.");
		return XPAI_LOAD_FILE_DATA_NOK_OTHER;
	}
	/* Check loadModule. */
	if (loadModule == NULL)
		lmc = empty_string;
	else if (strlen(loadModule) == 0)
		lmc = empty_string;
	else
		lmc = loadModule;

	TPT_TRACE(1, STR("XPAI_LoadFileDelete(loadModule=%s pid=0x%x)",
	                 lmc, pid));

	lmc_len = strlen(lmc);
	max_len = XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH - 1;
	if (lmc_len > max_len) {
		TPT_ERROR(STR("XPAI_LoadFileDelete() received"
		              " illegal pid length: %d.",
		              lmc_len));
		ret = XPAI_LOAD_FILE_DELETE_NOK_WRONG_PARAM;
	}
	/* Send request. */
	out_msg = itc_alloc(sizeof(struct lmc_load_file_delete_req) + lmc_len + 1,
	                    LMC_LOAD_FILE_DELETE_REQ);
	strncpy(out_msg->load_file_delete_req.loadmodule, lmc, lmc_len + 1);
	out_msg->load_file_delete_req.procedure_ref = pid;
	out_msg->load_file_delete_req.connection_ref = lmc_conn.server_ref;
	itc_send(&out_msg, adapter_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (in_msg->msgno) {
	case LMC_LOAD_FILE_DELETE_CFM:
		break;
	case LMC_LOAD_FILE_DELETE_REJ:
		ret = in_msg->load_file_delete_rej.error_code;
		switch (ret) {
		case LMC_RESULT_WRONG_STATE:
			ret = XPAI_LOAD_FILE_DELETE_NOK_WRONG_STATE;
			break;
		case LMC_RESULT_ACCESS_DENIED:
			ret = XPAI_LOAD_FILE_DELETE_NOK_UNEXPECTED_PARAMETER_VALUE;
			break;
		case LMC_RESULT_INVALID_PARAM:
			ret = XPAI_LOAD_FILE_DELETE_NOK_WRONG_PARAM;
			break;
		case LMC_RESULT_OTHER_ERROR:
		default:
			ret = XPAI_LOAD_FILE_DELETE_NOK_OTHER;
			break;
		}
		break;
	default:
		TPT_ERROR(STR("Unexpected signal (msgno=0x%X) received from 0x%X.",
		              in_msg->msgno, itc_sender(in_msg)));
		ret = XPAI_LOAD_FILE_DELETE_NOK_OTHER;
		break;
	}
	itc_free(&in_msg);

	return ret;
}
S32 XPAI_ReadLoadFileData(U32 loadFileIx, /* !- FUNC -! */
			  U32 addrOffset,
                          U8 *buffer, 
                          U32 length)
{
	union itc_msg *out_msg;
	union itc_msg *in_msg;
	uint32_t ret = XPAI_READ_LOAD_FILE_DATA_OK;

        static const uint32_t rx_filter[] = { 2,
	                                      LMC_READ_LOAD_FILE_DATA_CFM,
	                                      LMC_READ_LOAD_FILE_DATA_REJ
	                                    };
	if (!initialized) {
		TPT_ERROR("Connection to lmc server is not started.");
		return XPAI_READ_LOAD_FILE_DATA_NOK_SERVER;
	}

	TPT_TRACE(1, STR("XPAI_ReadLoadFileData"
	                 "(loadFileIx=%d addrOffset=%d length=%d)",
	                 loadFileIx, addrOffset, length));
	/* If wrong loadFileIx. */
	if (loadFileIx > XPAI_XMH_MAX_LOADFILES) {
		TPT_ERROR(STR("XPAI_ReadLoadFileData() received"
		              " illegal loadFileIx: 0x%X",
		              loadFileIx));
		ret = XPAI_READ_LOAD_FILE_DATA_NOK_WRONG_PARAM;
		return ret;
	}
        /* Send request. */
        out_msg = itc_alloc(sizeof(struct lmc_read_load_file_data_req),
	                    LMC_READ_LOAD_FILE_DATA_REQ);
	out_msg->read_load_file_data_req.lmc_index = loadFileIx;
	out_msg->read_load_file_data_req.addroffset = addrOffset;
        out_msg->read_load_file_data_req.length = length;
	out_msg->read_load_file_data_req.connection_ref = lmc_conn.server_ref;

	itc_send(&out_msg, lmc_conn.server_mbox, ITC_MY_MBOX);
	/* Receive answer. */
	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (in_msg->msgno) {
        case LMC_READ_LOAD_FILE_DATA_CFM:
                TPT_REC_SIG(in_msg->msgno, "LMC_READ_LOAD_FILE_DATA_CFM");
                memcpy(buffer,in_msg->read_load_file_data_cfm.lmc_data,in_msg->read_load_file_data_cfm.nof_read_bytes);
		break;
        case LMC_READ_LOAD_FILE_DATA_REJ:
                TPT_REC_SIG(in_msg->msgno, "LMC_READ_LOAD_FILE_DATA_REJ");
		ret = in_msg->read_load_file_data_rej.error_code;
                break;
        default:
		TPT_ERROR(STR("Unexpected signal (msgno=0x%X) received from 0x%X.",
		              in_msg->msgno, itc_sender(in_msg)));
		ret = XPAI_READ_LOAD_FILE_DATA_NOK_OTHER;
		break;
        }
        itc_free(&in_msg);
        return ret;
}


U32 XPAI_LoadFileDataGetSeq(U32 *seqNr)
{
	union itc_msg *out_msg;
	union itc_msg *in_msg;
	uint32_t ret = XPAI_LOAD_FILE_DATA_GET_SEQ_OK;
    static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_DATA_GET_SEQ_CFM,
	                                      LMC_LOAD_FILE_DATA_GET_SEQ_REJ
	                                    };
    if (!initialized) {
            TPT_ERROR("Connection to lmc server is not started.");
            return XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_SERVER;
        }
        if (adapter_mbox == ITC_NO_ID) {
            TPT_ERROR("Adapter thread is not started.");
            return XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_OTHER;
        }
        /* Send request. */
        out_msg = itc_alloc(sizeof(struct lmc_load_file_data_get_seq_req),
                            LMC_LOAD_FILE_DATA_GET_SEQ_REQ);
        out_msg->load_file_data_get_seq_req.connection_ref = lmc_conn.server_ref;
        itc_send(&out_msg, adapter_mbox, ITC_MY_MBOX);
    
        /* Receive answer. */
        in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
        switch (in_msg->msgno) {
        case LMC_LOAD_FILE_DATA_GET_SEQ_CFM:
            *seqNr = in_msg->load_file_data_get_seq_cfm.result;
            ret = XPAI_LOAD_FILE_DATA_GET_SEQ_OK;
            break;
        case LMC_LOAD_FILE_END_REJ:
            ret = in_msg->load_file_data_get_seq_rej.error_code;
            switch (ret) {
            case LMC_RESULT_WRONG_STATE:
                ret = XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_WRONG_STATE;
                break;
            default:
                ret = XPAI_LOAD_FILE_END_NOK_OTHER;
                break;
            }
            break;
        default:
            TPT_ERROR(STR("Unexpected signal (msgno=0x%X) received from 0x%X.",
                          in_msg->msgno, itc_sender(in_msg)));
            ret = XPAI_LOAD_FILE_END_NOK_OTHER;
            break;
        }
        itc_free(&in_msg);
    
        return ret;

}



/****
 *
 *      Function handle_load_file_init_req
 *
 *****/
static void handle_load_file_init_req(union itc_msg **msg,
                                      itc_mbox_id_t *ind_receiver)
{
	union itc_msg *rej;
	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_INIT_CFM,
	                                      LMC_LOAD_FILE_INIT_REJ
	                                    };
	itc_mbox_id_t sender = itc_sender(*msg);

	if (*ind_receiver != ITC_NO_ID) {
		rej = itc_alloc(sizeof(struct lmc_load_file_init_rej),
		                LMC_LOAD_FILE_INIT_REJ);
		rej->load_file_init_rej.error_code = LMC_RESULT_WRONG_STATE;
		TPT_SEND_SIG(rej->msgno, sender,
		             "LMC_GET_LOAD_FILE_INIT_REJ");
		itc_send(&rej, sender, ITC_MY_MBOX);
		return;
	}

	*ind_receiver = (*msg)->load_file_init_req.procedure_ref;

	/* Forward request*/
	itc_send(msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive and forward answer. */
	*msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	if ((*msg)->msgno == LMC_LOAD_FILE_INIT_REJ)
		*ind_receiver = ITC_NO_ID;
	itc_send(msg, sender, ITC_MY_MBOX);
}

/****
 *
 *      Function handle_load_file_data_req
 *
 *****/
static void handle_load_file_data_req(union itc_msg **msg,
                                      itc_mbox_id_t ind_receiver)
{
	union itc_msg *rej;
	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_DATA_CFM,
	                                      LMC_LOAD_FILE_DATA_REJ
	                                    };

	itc_mbox_id_t sender = itc_sender(*msg);

	if (ind_receiver == ITC_NO_ID) {
		rej = itc_alloc(sizeof(struct lmc_load_file_data_rej),
		                LMC_LOAD_FILE_DATA_REJ);
		rej->load_file_data_rej.error_code = LMC_RESULT_WRONG_STATE;
		TPT_SEND_SIG(rej->msgno, sender,
		             "LMC_GET_LOAD_FILE_DATA_REJ");
		itc_send(&rej, sender, ITC_MY_MBOX);
		return;
	}

	/* Forward request*/
	(*msg)->load_file_data_req.procedure_ref = ind_receiver;
	itc_send(msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive and forward answer. */
	*msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	itc_send(msg, sender, ITC_MY_MBOX);
}

/****
 *
 *      Function handle_load_file_end_req
 *
 *****/
static void handle_load_file_end_req(union itc_msg **msg,
                                     itc_mbox_id_t *ind_receiver)
{
	union itc_msg *rej;
	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_END_CFM,
	                                      LMC_LOAD_FILE_END_REJ
	                                    };

	itc_mbox_id_t sender = itc_sender(*msg);

	if (*ind_receiver == ITC_NO_ID) {
		rej = itc_alloc(sizeof(struct lmc_load_file_end_rej),
		                LMC_LOAD_FILE_END_REJ);
		rej->load_file_end_rej.error_code = LMC_RESULT_WRONG_STATE;
		TPT_SEND_SIG(rej->msgno, sender,
		             "LMC_GET_LOAD_FILE_END_REJ");
		itc_send(&rej, sender, ITC_MY_MBOX);
		return;
	}

	/* Forward request*/
	itc_send(msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive and forward answer. */
	*msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	*ind_receiver = ITC_NO_ID;
	itc_send(msg, sender, ITC_MY_MBOX);
}

/****
 *
 *      Function handle_load_file_delete_req
 *
 *****/
static void handle_load_file_delete_req(union itc_msg **msg)
{
	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_DELETE_CFM,
	                                      LMC_LOAD_FILE_DELETE_REJ
	                                    };

	itc_mbox_id_t sender = itc_sender(*msg);

	/* Forward request*/
	itc_send(msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive and forward answer. */
	*msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	itc_send(msg, sender, ITC_MY_MBOX);
}

/****
 *
 *      Function handle_load_file_data_ind
 *
 *****/
static void handle_load_file_data_ind(union itc_msg **in_msg)
{
	union itc_msg *out_msg;
	itc_mbox_id_t mbox = (*in_msg)->load_file_data_ind.procedure_ref;

	out_msg = itc_alloc(sizeof(struct XPAI_LoadFileDataIndS),
	                    XPAI_LOAD_FILE_DATA_IND);
	out_msg->data_ind.lmSeqNr = (*in_msg)->load_file_data_ind.lm_seq_nr;
	itc_send(&out_msg, mbox, ITC_MY_MBOX);
}

/****
 *
 *      Function handle_load_file_delete_ind
 *
 *****/
static void handle_load_file_delete_ind(union itc_msg **in_msg)
{
	union itc_msg *out_msg;
	uint32_t result = (*in_msg)->load_file_delete_ind.result;
	itc_mbox_id_t mbox = (*in_msg)->load_file_delete_ind.procedure_ref;

	out_msg = itc_alloc(sizeof(struct XPAI_LoadFileDeleteIndS),
	                    XPAI_LOAD_FILE_DELETE_IND);
	switch (result) {
	case LMC_RESULT_SUCCESS:
		result = XPAI_LOAD_FILE_DELETE_RESULT_OK;
		break;
	case LMC_RESULT_INVALID_PARAM:
		result = XPAI_LOAD_FILE_DELETE_RESULT_UNEXPECTED_PARAMETER_VALUE;
		break;
	default:
		result = XPAI_LOAD_FILE_DELETE_RESULT_OTHER_ERROR;
		break;
	}
	out_msg->delete_ind.result = result;
	itc_send(&out_msg, mbox, ITC_MY_MBOX);
}

/****
 *
 *      Function handle_load_file_data_get_seq
 *
 *****/
static void handle_load_file_data_get_seq_req(union itc_msg **msg,
                                     itc_mbox_id_t *ind_receiver)
{
	union itc_msg *rej;
	static const uint32_t rx_filter[] = { 2,
	                                      LMC_LOAD_FILE_DATA_GET_SEQ_CFM,
	                                      LMC_LOAD_FILE_DATA_GET_SEQ_REJ
	                                    };

	itc_mbox_id_t sender = itc_sender(*msg);
    
    if (*ind_receiver == ITC_NO_ID) {
            rej = itc_alloc(sizeof(struct lmc_load_file_end_rej),
                            LMC_LOAD_FILE_END_REJ);
            rej->load_file_end_rej.error_code = LMC_RESULT_WRONG_STATE;
            TPT_SEND_SIG(rej->msgno, sender,
                         "LMC_GET_LOAD_FILE_END_REJ");
            itc_send(&rej, sender, ITC_MY_MBOX);
            return;
        }

	/* Forward request*/
	itc_send(msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	/* Receive and forward answer. */
	*msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	itc_send(msg, sender, ITC_MY_MBOX);
}

/****
 *
 *      Function do_conn_establish
 *
 *****/
static int do_conn_establish(void)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {LMC_SERVER_VERSIONS};
	uint32_t res;
	LMC_CONN_ESTABLISH_MESSAGES_STRUCT(lmc_conn_messages);

	/*call connection establish*/
	res = xpai_locate_mbox(LMC_SERVER_NAME, &lmc_conn.server_mbox);
	if (res != INIT_OK) {
		TPT_ERROR("Client:Cannot find lmc server mailbox");
		goto do_conn_establish_error;
	}

	res = conn_establish(
	              /*input parameters*/
	              lmc_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &lmc_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &lmc_conn.server_ref,
	              &lmc_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed (reason:0x%08x)",
		              res));
		goto do_conn_establish_error;
	}

	TPT_TRACE(1, STR("mailbox id for lmc server: %u\n \
                   server connection ref:%u\n \
                   selected version: %u\n",
	          lmc_conn.server_mbox,
	          lmc_conn.server_ref,
	          lmc_conn.selected_version));

	res = pthread_mutex_lock(&conn_complete_lock);
	if (res) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d", res));
		goto do_conn_establish_error;
	}
	initialized = true;

	res = pthread_cond_signal(&conn_complete);
	if (res) {
		TPT_ERROR(STR("pthread_cond_signal return error %d", res));
		(void) pthread_mutex_unlock(&conn_complete_lock);
		goto do_conn_establish_error;
	}

	res = pthread_mutex_unlock(&conn_complete_lock);
	if (res) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d", res));
		goto do_conn_establish_error;
	}

	return 0;
do_conn_establish_error:

	(void) pthread_cond_signal(&conn_complete);
	return -1;
}

/****
 *
 *      Function adapter
 *
 *****/
static void *adapter(void _UNUSED_ *arg)
{
	union itc_msg *msg;
	itc_mbox_id_t ind_receiver = ITC_NO_ID;

	adapter_mbox = itc_create_mailbox(XMH_ADAPTER_MBOX, 0);
	if (adapter_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("create_mailbox of %s failed", XMH_ADAPTER_MBOX));
		pthread_exit(NULL);
	}

	if (do_conn_establish()) {
		TPT_ERROR("Connection establishment failed");
		itc_delete_mailbox(adapter_mbox);
		pthread_exit(NULL);
	}

	for (;;) {
		msg = itc_receive(ITC_NOFILTER,
		                  ITC_NO_TMO,
		                  ITC_FROM_ALL);
		switch(msg->msgno) {
		case LMC_LOAD_FILE_INIT_REQ:
			handle_load_file_init_req(&msg, &ind_receiver);
			break;
		case LMC_LOAD_FILE_DATA_REQ:
			handle_load_file_data_req(&msg, ind_receiver);
			break;
		case LMC_LOAD_FILE_DATA_IND:
			handle_load_file_data_ind(&msg);
			break;
		case LMC_LOAD_FILE_END_REQ:
			handle_load_file_end_req(&msg, &ind_receiver);
			break;
		case LMC_LOAD_FILE_DELETE_REQ:
			handle_load_file_delete_req(&msg);
			break;
		case LMC_LOAD_FILE_DELETE_IND:
			handle_load_file_delete_ind(&msg);
			break;
        case LMC_LOAD_FILE_DATA_GET_SEQ_REQ:
            handle_load_file_data_get_seq_req(&msg, &ind_receiver);
            break;
		default:
			TPT_ERROR(STR("Ind thread receive unexpected message"
			              " 0x%x", msg->msgno));
			break;
		}
		if (msg != NULL)
			itc_free(&msg);
	}
}

/****
 *
 *      Function init_routine
 *
 *****/
static void init_routine(void)
{
	pthread_attr_t attr;
	pthread_t thread;
	int ret;
	pthread_condattr_t cattr;
	int locked = 0;

	ret = pthread_condattr_init(&cattr);
	if (ret) {
		TPT_ERROR(STR("pthread_condattr_init failed with error %d",
		              ret));
		return;
	}

	ret = pthread_cond_init(&conn_complete, &cattr);
	if (ret) {
		TPT_ERROR(STR("pthread_cond_init failed with error %d",
		              ret));
		return;
	}

	ret = pthread_attr_init(&attr);
	if (ret) {
		TPT_ERROR(STR("pthread_attr_init failed with error %d",
		              ret));
		return;
	}

	ret = pthread_mutex_lock(&conn_complete_lock);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              ret));
		goto init_routine_end;
	}

	locked = 1;

	ret = pthread_create(&thread, &attr, adapter, NULL);
	if (!ret) {
		ret = pthread_cond_wait(&conn_complete, &conn_complete_lock);
		if (!ret)
			goto init_routine_end;
		TPT_ERROR(STR("pthread_cond_wait failed with error %d",
		              ret));
	} else {
		TPT_ERROR(STR("pthread_create failed with error %d",
		              ret));
	}

init_routine_end:
	if (locked) {
		ret = pthread_mutex_unlock(&conn_complete_lock);
		if (ret)
			TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
			              ret));
	}
	return;
}

/****
 *
 *      Function lmc_init
 *
 *****/
int32_t xpai_lmc_init(void)
{
	pthread_once(&once_control, init_routine);

	if (!initialized) {
		TPT_ERROR(STR("Client:%d failed to intialize xpai lmc",
		              client_ref));
		return -1;
	} else {
		return 0;
	}
}

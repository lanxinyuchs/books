/* ----------------------------------------------------------------------
 * %CCaseFile:	appm_lmhi.c %
 * %CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/1 %
 * %CCaseDate:	2017-09-28 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * This file contains the implementation of the LMHI interface.
 * LMHI has two functions: one to get all Load Modules for a board and tag
 * and one function to release the buffers where the result was stored.
 * When the request is received, a socket is set up towards the server in CS
 * and the request is sent. When the answer is received, the message is
 * unpacked and strored in the result buffers.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2017 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R1A/2      2012-08-17   etxarnu     Created
 * R1A/3      2012-08-20   etxarnu     Added LMI_HOST
 * R2A/1      2012-11-28   etxarnu     Use CEC for comms to MW
 * R2A/5      2013-03-15   etxpeno     Added Lmhi_start_pgm and Lmhi_stop_pgm
 * R2A/16     2014-05-19   etxarnu     Increased timeout to 20 sec
 * R2A/17     2014-05-26   etxarnu     Set lmhi_result_buffer->lmdata=NULL
 * R2A/18     2014-08-09   uabesvi     Increased timeout to 40 sec
 * R3A/1      2014-11-06   etxarnu     Added tracepoints
 * R3A/2      2014-11-27   etxarnu     Added Lmhi_get_lms_2
 *                                     Added Lmhi_start_pgm_2
 * R3A/3      2014-12-08   etxarnu     Added boardrev to Lmhi_get_lms_2
 * R4A/1      2015-05-22   etxarnu     lmhi_result_buffer->lmdata = NULL after free()
 * R4A/3      2015-09-10   etxarnu     Made mem_base and mem_current thread safe
 * R4A/3      2015-09-21   etxarnu     more info in error printouts in Lmhi_get_lms/_2
 * R5A/1      2016-03-08   etxarnu     Added Lmhi_get_pids/Lmhi_free_pids
 * R6A/1      2016-04-25   etxpeno     remove compiler warnings
 * R6A/2      2016-06-16   etxberb     Added Lmhi_get_lms_3 (addition of
 *                                     hwcategory and hwmodel on top of
 *                                     Lmhi_get_lms_2).
 * R8A/1      2016-12-15   etxarnu     Added Lmhi_add_board/3
 * R9A/1      2017-02-20   etxpejn     Temp removed error printout in Lmhi_add_board/3
 * R10A/1     2017-05-30   uabesvi     Added Lmhi_ext_add_board/3
 * R11A/1     2017-09-06   etxarnu     Added Lmhi_start_pgm_3
 * ----------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <syslog.h>


#include "cec.h"
#include "lmi.pb-c.h"
#include "appm_lmhi.h"

#define TRACEPOINT_DEFINE
#include "appm_trace.h"

#define INFO( fmt,...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR( fmt,...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt,...) do {	\
  char *err_str;\
  asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, __VA_ARGS__); \
  tracepoint(com_ericsson_appm, type , err_str);\
  free(err_str);\
  } while(0)



static LmhiResultCode copy_msg_to_lmh_buffer(LmiGetLMsResult * ,
					     LmhiGetLMsResult *);

static LmhiResultCode copy_pids_to_lmh_buffer(LmiGetPidsResult * ,
					     LmhiGetPidsResult *);

static int calc_buffers_size(LmiGetLMsResult * ret_msg);
static void init_my_alloc(char *base);
static char* my_alloc(int size);
static int pad(char* str);
int asprintf(char **strp, const char *fmt, ...);


__thread char * mem_base = NULL;
__thread char * mem_current;
__thread char * pid_base = NULL;

//----------------------------------------------------------------------------
LmhiResultCode Lmhi_add_board(char *boardNo,  char *boardRev, 
			      uint32_t mboxId)
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;
  LmiMessage msg   = LMI_MESSAGE__INIT;
  LmiAddBoard addBoard = LMI_ADD_BOARD__INIT;
  LmiAddBoardRet * ret_msg ;
  int timeout = 43000;

  addBoard.handle = 0;
  addBoard.boardno = boardNo;
  addBoard.boardrev = boardRev;
  addBoard.mboxid = mboxId;
  msg.addboard = &addBoard;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Lmhi_add_board: Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Lmhi_add_board: Failed cec_send",0);
     result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }


  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("Lmhi_add_board: Failed cec_receive_w_tmeout",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  ret_msg = lmi_add_board_ret__unpack(NULL,
				      recv_packet.length,
				      (uint8_t *) recv_packet.data);

  free(recv_packet.data);

  if (ret_msg == NULL) {
    ERROR("Lmhi_add_board: Error unpacking incoming message ",0);
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK) {
    //ERROR("LMH server reports error",0);
    result = LMHI_ERR_PEER_ERROR;
  }

  if (ret_msg->result == LMI_RESULT_CODE__WAIT) {
    result = LMHI_WAIT;
  }

  lmi_add_board_ret__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;

}


//----------------------------------------------------------------------------
LmhiResultCode Lmhi_add_ext_board(char *boardNo,
				  char *boardRev, 
				  uint32_t mboxId)
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;
  LmiMessage msg   = LMI_MESSAGE__INIT;
  LmiAddExtBoard addExtBoard = LMI_ADD_EXT_BOARD__INIT;
  LmiAddExtBoardRet * ret_msg ;
  int timeout = 43000;

  addExtBoard.handle = 0;
  addExtBoard.boardno = boardNo;
  addExtBoard.boardrev = boardRev;
  addExtBoard.mboxid = mboxId;
  msg.addextboard = &addExtBoard;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Lmhi_add_ext_board: Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Lmhi_add_ext_board: Failed cec_send",0);
     result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }


  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("Lmhi_add_ext_board: Failed cec_receive_w_tmeout",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  ret_msg = lmi_add_ext_board_ret__unpack(NULL,
					  recv_packet.length,
					  (uint8_t *) recv_packet.data);
  
  free(recv_packet.data);

  if (ret_msg == NULL) {
    ERROR("Lmhi_add_ext_board: Error unpacking incoming message ",0);
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK) {
    //ERROR("LMH server reports error",0);
    result = LMHI_ERR_PEER_ERROR;
  }

  if (ret_msg->result == LMI_RESULT_CODE__WAIT) {
    result = LMHI_WAIT;
  }

  lmi_add_ext_board_ret__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;

}



//----------------------------------------------------------------------------
LmhiResultCode Lmhi_delete_ext_board(char *boardNo,
				     char *boardRev, 
				     uint32_t mboxId)
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;
  LmiMessage msg   = LMI_MESSAGE__INIT;
  LmiDeleteExtBoard deleteExtBoard = LMI_DELETE_EXT_BOARD__INIT;
  LmiDeleteExtBoardRet * ret_msg ;
  int timeout = 43000;

  deleteExtBoard.handle = 0;
  deleteExtBoard.boardno = boardNo;
  deleteExtBoard.boardrev = boardRev;
  deleteExtBoard.mboxid = mboxId;
  msg.deleteextboard = &deleteExtBoard;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Lmhi_delete_ext_board: Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Lmhi_delete_ext_board: Failed cec_send",0);
     result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }


  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("Lmhi_delete_ext_board: Failed cec_receive_w_tmeout",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  ret_msg = lmi_delete_ext_board_ret__unpack(NULL,
					  recv_packet.length,
					  (uint8_t *) recv_packet.data);
  
  free(recv_packet.data);

  if (ret_msg == NULL) {
    ERROR("Lmhi_delete_ext_board: Error unpacking incoming message ",0);
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK) {
    //ERROR("LMH server reports error",0);
    result = LMHI_ERR_PEER_ERROR;
  }

  if (ret_msg->result == LMI_RESULT_CODE__WAIT) {
    result = LMHI_WAIT;
  }

  lmi_delete_ext_board_ret__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;

}

//----------------------------------------------------------------------------

LmhiResultCode Lmhi_get_lms (char * boardtype,  char * tag,
			     LmhiGetLMsResult* lmhi_result_buffer )
{
  return Lmhi_get_lms_3(NULL, NULL,
			boardtype, NULL,
			tag, LMHI_PHASE_NORMAL,
			lmhi_result_buffer);
}

LmhiResultCode Lmhi_get_lms_2 (char * boardtype, char * boardrev,
			       char * tag, LmhiPhase phase,
			       LmhiGetLMsResult* lmhi_result_buffer )
{
  return Lmhi_get_lms_3(NULL, NULL,
			boardtype, boardrev,
			tag, phase,
			lmhi_result_buffer);
}

LmhiResultCode Lmhi_get_lms_3 (char * hwcategory, char * hwmodel,
			       char * boardtype, char * boardrev,
			       char * tag, LmhiPhase phase,
			       LmhiGetLMsResult* lmhi_result_buffer )
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;
  lmhi_result_buffer->lmdata = NULL;
  LmiMessage msg   = LMI_MESSAGE__INIT;
  LmiGetLMs getLms = LMI_GET_LMS__INIT;
  LmiGetLMsResult * ret_msg ;
  int timeout = 43000;


  //INFO( "HwCategory: %s, HwModel: %s, Boardtype: %s, Boardrev: %s, tag: %s, resbuf: %d", hwcategory, hwmodel, boardtype, boardrev, tag, (int)lmhi_result_buffer );

  getLms.handle = 0;
  getLms.hwcategory = hwcategory;
  getLms.hwmodel = hwmodel;
  getLms.boardtype = boardtype;
  getLms.boardrev = boardrev;
  getLms.tag = tag;
  getLms.phase = phase;
  msg.getlms = &getLms;

  // This is the calculated packing length
  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  // Pack msg, including submessages
  lmi_message__pack (&msg, (uint8_t *)send_packet.data );

  // See the length of message */
  /* fprintf(stderr,"Writing %d serialized bytes\n",send_packet.length); */
  /* hexdump(send_packet.data,send_packet.length,80); */

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Failed cec_open: HwCategory: %s, HwModel: %s, Boardtype: %s, Boardrev: %s, tag: %s, resbuf: %p",
	  hwcategory, hwmodel, boardtype, boardrev, tag, (void*)lmhi_result_buffer );
    result = LMHI_ERR_CEC_OPEN;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Failed cec_send: HwCategory: %s, HwModel: %s, Boardtype: %s, Boardrev: %s, tag: %s, resbuf: %p",
	  hwcategory, hwmodel, boardtype, boardrev, tag, (void*)lmhi_result_buffer );
    result = LMHI_ERR_CEC_SEND;
    goto cleanup1;
  }

  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("Failed cec_receive_w_timeout: HwCategory: %s, HwModel: %s, Boardtype: %s, Boardrev: %s, tag: %s, resbuf: %p",
	  hwcategory, hwmodel, boardtype, boardrev, tag, (void*)lmhi_result_buffer );
    result = LMHI_ERR_RECV_TMO;
    goto cleanup1;
  }

  /* fprintf(stderr,"result after sending = %d\n",res);  */
  /* fprintf(stderr,"received the following\n");  */
  /* hexdump(recv_packet.data,recv_packet.length,80); */

  /* Decoding of answer */
  ret_msg = lmi_get_lms_result__unpack(NULL,
				       recv_packet.length,
				       (uint8_t *) recv_packet.data);


  free(recv_packet.data); // Free the allocated serialized buffer

  if (ret_msg == NULL) {
    ERROR("Error unpacking incoming message : HwCategory: %s, HwModel: %s, Boardtype: %s, Boardrev: %s, tag: %s, resbuf: %p",
	  hwcategory, hwmodel, boardtype, boardrev, tag, (void*)lmhi_result_buffer );
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->n_lmdata == 0)  {
    //ERROR("No LM data returned",0);
    result = LMHI_ERR_ZERO_LENGTH;
    goto cleanup2;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK){
    ERROR("LMH server reports error: HwCategory: %s, HwModel: %s, Boardtype: %s, Boardrev: %s, tag: %s, resbuf: %p",
	  hwcategory, hwmodel, boardtype, boardrev, tag, (void*)lmhi_result_buffer );
    result = LMHI_ERR_NO_DATA;
    goto cleanup2;
  }

  /* print_msg_buf( ret_msg); */

  result = copy_msg_to_lmh_buffer(ret_msg, lmhi_result_buffer);

 cleanup2:
  lmi_get_lms_result__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;
}

//----------------------------------------------------------------------------
void Lmhi_free_buffers( LmhiGetLMsResult *lmhi_result_buffer) {
  free(lmhi_result_buffer->lmdata);
  lmhi_result_buffer->lmdata = NULL;
  mem_base = mem_current = NULL;
}


//----------------------------------------------------------------------------
LmhiResultCode
Lmhi_start_pgm(uint32_t duId, uint32_t cpuSet, char *lmId, char *pgmName,
	       char *const argv[], LmhiStartPgmResult *startPgmRes)
{
  return Lmhi_start_pgm_2( duId,  cpuSet, lmId, pgmName, NULL,
			   argv, startPgmRes);
}


LmhiResultCode
Lmhi_start_pgm_2(uint32_t duId, uint32_t cpuSet, char *lmId, char *pgmName, char *nameSpace,
	       char *const argv[], LmhiStartPgmResult *startPgmRes)
{
  return Lmhi_start_pgm_3( duId,  cpuSet, lmId, pgmName, nameSpace, 0,
			   argv, startPgmRes);
}

LmhiResultCode
Lmhi_start_pgm_3(uint32_t duId, uint32_t cpuSet, char *lmId, char *pgmName, char *nameSpace, uint32_t mboxId,
	       char *const argv[], LmhiStartPgmResult *startPgmRes)
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;

  size_t n_arg;
  char **arg;

  LmiMessage msg = LMI_MESSAGE__INIT;
  LmiStartPgm startPgm = LMI_START_PGM__INIT;
  LmiStartPgmRet *ret_msg ;

  int timeout = 90000; // 90 seconds. Must be longer than the timeout in appmServer

  for (n_arg = 0; argv[n_arg]; n_arg++);
  arg = malloc(n_arg *sizeof(*arg));
  startPgm.pid = getpid();
  startPgm.handle = 0;
  startPgm.pgmname = pgmName;
  startPgm.lmid = lmId;
  startPgm.duid = duId;
  startPgm.cpuset = cpuSet;
  startPgm.mboxid = mboxId;
  startPgm.n_arg = n_arg;
  startPgm.arg = arg;
  startPgm.ns = nameSpace;
  for (unsigned int i = 0; i < n_arg; i++)
    arg[i] = argv[i];

  msg.startpgm = &startPgm;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  free(arg);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("cec_send failed",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("cec_receive_w_tmeout failed",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  ret_msg = lmi_start_pgm_ret__unpack(NULL,
				      recv_packet.length,
				      (uint8_t *) recv_packet.data);

  free(recv_packet.data);

  if (ret_msg == NULL) {
    ERROR("Error unpacking incoming message",0);
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK) {
    ERROR("LMH server reports error",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup2;
  }

  startPgmRes->pgmId = ret_msg->pgmid;

 cleanup2:
  lmi_start_pgm_ret__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;
}

//----------------------------------------------------------------------------
LmhiResultCode
Lmhi_stop_pgm(uint32_t duId, uint32_t pgmId)
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;

  LmiMessage msg = LMI_MESSAGE__INIT;
  LmiStopPgm stopPgm = LMI_STOP_PGM__INIT;
  LmiStopPgmRet *ret_msg ;

  int timeout = 90000; // 90 seconds. Must be longer than the timeout in appmServer

  stopPgm.pid = getpid();
  stopPgm.handle = 0;
  stopPgm.pgmid = pgmId;
  stopPgm.duid = duId;
  msg.stoppgm = &stopPgm;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Failed cec_send",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("Failed cec_receive_w_tmeout",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  ret_msg = lmi_stop_pgm_ret__unpack(NULL,
				     recv_packet.length,
				     (uint8_t *) recv_packet.data);

  free(recv_packet.data);

  if (ret_msg == NULL) {
    ERROR("Error unpacking incoming message ",0);
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK) {
    ERROR("LMH server reports error",0);
    result = LMHI_ERR_PEER_ERROR;
  }

  lmi_stop_pgm_ret__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;
}

//----------------------------------------------------------------------------
LmhiResultCode
Lmhi_heartbeat(char *pgmName, char *lmId)
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet;
  LmhiResultCode result = LMHI_OK;

  LmiMessage msg = LMI_MESSAGE__INIT;
  LmiHeartbeat heartBeat = LMI_HEARTBEAT__INIT;

  heartBeat.pid = getpid();
  heartBeat.handle = 0;
  heartBeat.pgmname = pgmName;
  heartBeat.lmid = lmId;
  msg.heartbeat = &heartBeat;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Failed cec_send",0);
     result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;
}

//----------------------------------------------------------------------------
LmhiResultCode
Lmhi_signal_to_pgm(uint32_t duId, uint32_t pgmId, uint32_t sigNo)
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;

  LmiMessage msg = LMI_MESSAGE__INIT;
  LmiSignPgm signPgm = LMI_SIGN_PGM__INIT;
  LmiSignPgmRet *ret_msg ;

  switch (sigNo) {
  case SIGHUP:
  case SIGINT:
  case SIGQUIT:
  case SIGPIPE:
  case SIGTERM:
  case SIGUSR1:
  case SIGUSR2:
    break;
  default:
    if (sigNo >= (uint32_t)SIGRTMIN && sigNo <= (uint32_t)SIGRTMAX)
      break;
    return LMHI_ERR_SIG_NO;
  }


  int timeout = 90000; // 90 seconds. Must be longer than the timeout in appmServer

  signPgm.pid = getpid();
  signPgm.handle = 0;
  signPgm.pgmid = pgmId;
  signPgm.duid = duId;
  signPgm.signo = sigNo;
  msg.signpgm = &signPgm;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Failed cec_send",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("Failed cec_receive_w_tmeout",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  ret_msg = lmi_sign_pgm_ret__unpack(NULL,
				     recv_packet.length,
				     (uint8_t *) recv_packet.data);

  free(recv_packet.data);

  if (ret_msg == NULL) {
    ERROR("Error unpacking incoming message",0);
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK) {
    ERROR("LMH server reports error",0);
    result = LMHI_ERR_PEER_ERROR;
  }

  lmi_sign_pgm_ret__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;
}

//----------------------------------------------------------------------------
LmhiResultCode
Lmhi_get_pids(uint32_t duId, uint32_t pgmId, LmhiGetPidsResult* lmhi_result_buffer )
{
  char signature[] = {'L', 'M', 'I'} ;
  cec_handle_t *handle;
  cec_packet_t send_packet, recv_packet;
  LmhiResultCode result = LMHI_OK;

  LmiMessage msg = LMI_MESSAGE__INIT;
  LmiGetPids getPids = LMI_GET_PIDS__INIT ;
  LmiGetPidsResult *ret_msg ;

  /* syslog(LOG_INFO,"LMHI: get_pids: %u ", pgmId); */

  int timeout = 90000; // 90 seconds. Must be longer than the timeout in appmServer

  getPids.pid = getpid();
  getPids.handle = 0;
  getPids.pgmid = pgmId;
  getPids.duid = duId;
  msg.getpids = &getPids;

  send_packet.length = lmi_message__get_packed_size (&msg);
  send_packet.data = malloc(send_packet.length);
  lmi_message__pack (&msg, (uint8_t *)send_packet.data);

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    ERROR("Failed to open CEC port",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup0;
  }

  if (cec_send(handle, &send_packet) < 0) {
    ERROR("Failed cec_send",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  if (cec_receive_w_tmeout(handle, &recv_packet, timeout) < 0) {
    ERROR("Failed cec_receive_w_tmeout",0);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup1;
  }

  /* Decoding of answer */
  ret_msg = lmi_get_pids_result__unpack(NULL,
					recv_packet.length,
					(uint8_t *) recv_packet.data);

  free(recv_packet.data); // Free the allocated serialized buffer

  if (ret_msg == NULL) {
    ERROR("Error unpacking incoming message",0);
    result = LMHI_ERR_UNPACK_MSG;
    goto cleanup1;
  }

  if (ret_msg->n_piddata == 0)  {
    //ERROR("No pids data returned",0);
    result = LMHI_ERR_ZERO_LENGTH;
    goto cleanup2;
  }

  if (ret_msg->result == LMI_RESULT_CODE__NOK) {
    ERROR("LMH server reports error: appmId %d", pgmId);
    result = LMHI_ERR_PEER_ERROR;
    goto cleanup2;
  }
  result = copy_pids_to_lmh_buffer(ret_msg, lmhi_result_buffer);

 cleanup2:
  lmi_get_pids_result__free_unpacked(ret_msg, NULL);

 cleanup1:
  cec_close(handle);

 cleanup0:
  free(send_packet.data);

  return result;
}
//----------------------------------------------------------------------------
void Lmhi_free_pids( LmhiGetPidsResult *lmhi_result_buffer) {
  free(lmhi_result_buffer->pdata);
  pid_base =  NULL;
}




//---------------------------------------------------------------------------
static LmhiResultCode
copy_pids_to_lmh_buffer( LmiGetPidsResult * ret_msg,
			 LmhiGetPidsResult *lmhi_result_buffer) {
  int mem_size = ret_msg->n_piddata * 4;
  /* syslog(LOG_INFO,"LMHI: NoOfPids: %u",ret_msg->n_piddata ); */

  if ((pid_base = calloc(1,mem_size)) == NULL ) {
    ERROR("Data structure alloc failed",0);
    return LMHI_ERR_NO_MEMORY;
  }

  lmhi_result_buffer->n_pids = ret_msg->n_piddata;
  lmhi_result_buffer->pdata = (void*)pid_base;
  for (unsigned int i=0; i < ret_msg->n_piddata ; i++){
    /* syslog(LOG_INFO,"LMHI: Pid[%d]: %u",i,ret_msg->piddata[i] ); */
    lmhi_result_buffer->pdata[i] = ret_msg->piddata[i];
  }
  return LMHI_OK ;
}

//---------------------------------------------------------------------------
static LmhiResultCode
copy_msg_to_lmh_buffer( LmiGetLMsResult * ret_msg,
			LmhiGetLMsResult *lmhi_result_buffer) {

  int mem_size = calc_buffers_size(ret_msg);
  char * buff_base;
  if ((buff_base = calloc(1,mem_size)) == NULL ) {
    ERROR("Data structure alloc failed",0);
    return LMHI_ERR_NO_MEMORY;
  }

  init_my_alloc(buff_base);

  LmhiLmData *lms = (LmhiLmData *)my_alloc(sizeof(LmhiLmData)*
					   ret_msg->n_lmdata);
  lmhi_result_buffer->n_lmdata = ret_msg->n_lmdata;
  lmhi_result_buffer->lmdata   = lms;


  for (unsigned int i = 0; i < ret_msg->n_lmdata; i++) {

    char * lmname = my_alloc(pad(ret_msg->lmdata[i]->id->name));
    strcpy(lmname,ret_msg->lmdata[i]->id->name);

    char * lmid = my_alloc(pad(ret_msg->lmdata[i]->id->id));
    strcpy(lmid,ret_msg->lmdata[i]->id->id);

    char * lmrev = my_alloc(pad(ret_msg->lmdata[i]->id->rev));
    strcpy(lmrev,ret_msg->lmdata[i]->id->rev);

    LmhiFileInfo *files = (LmhiFileInfo *)my_alloc(sizeof(LmhiFileInfo)*
						   ret_msg->lmdata[i]->n_file);
    for (unsigned int j = 0; j < ret_msg->lmdata[i]->n_file; j++) {
      char *type = my_alloc(pad(ret_msg->lmdata[i]->file[j]->type) );
      strcpy(type,ret_msg->lmdata[i]->file[j]->type);
      char *path = my_alloc(pad(ret_msg->lmdata[i]->file[j]->path) );
      strcpy(path,ret_msg->lmdata[i]->file[j]->path);
      files[j].type = type;
      files[j].path = path;
    }

    lms[i].name = lmname;
    lms[i].id = lmid;
    lms[i].rev = lmrev;
    lms[i].file = files;
    lms[i].n_file = ret_msg->lmdata[i]->n_file;

  }
  /* hexdump((char *)lms, mem_size, 80);  */
  return LMHI_OK ;
}
//---------------------------------------------------------------------------

static int
calc_buffers_size(LmiGetLMsResult * ret_msg){

  int size = sizeof(LmhiLmData)* ret_msg->n_lmdata;

  for (unsigned int i = 0; i < ret_msg->n_lmdata; i++) {
    size += pad(ret_msg->lmdata[i]->id->name);
    size += pad(ret_msg->lmdata[i]->id->id);
    size += pad(ret_msg->lmdata[i]->id->rev);
    size += sizeof(LmhiFileInfo) * ret_msg->lmdata[i]->n_file;
    for (unsigned int j = 0; j < ret_msg->lmdata[i]->n_file; j++) {
      size += pad(ret_msg->lmdata[i]->file[j]->type);
      size += pad(ret_msg->lmdata[i]->file[j]->path);
    }
  }

  return size ;
}

//---------------------------------------------------------------------------
// Calculate size of buffer needed for string with room for \0 and word aligned
static int
pad(char* str){
  return ((strlen(str) + 1) & 0xfffffffc) + 4;
}

//---------------------------------------------------------------------------

static void
init_my_alloc(char *base) {
  mem_base = base;
  mem_current = base;
}

static char*
my_alloc(int size) {
  char * ret_val = mem_current;
  mem_current += size;
  return ret_val;
}

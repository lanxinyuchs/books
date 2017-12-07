/* ----------------------------------------------------------------------
 * %CCaseFile:	test_pri.c %
 * %CCaseRev:	/main/R2A/R4A/3 %
 * %CCaseDate:	2015-11-27 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Tests of PRI use this module.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
 * R4A/1      2015-11-25 erarafo     Using wrappers for TRI trace macros
 * R4A/2      2015-11-26 erarafo     APPLOG trace to understand failures
 * R4A/3      2015-11-27 erarafo     Calling restartOwnPiu from spawned process
 * ----------------------------------------------------------------------
 */

#define VERSION "%CCaseRev:	/main/R2A/R4A/3 %"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <time.h>

#include "master.h"
#include "cello_piu.h"
#include "cello_piu.sig"

#include "cello_te_trace.h"

#define CelloPri_initiateMemory_no           1
#define CelloPri_initiateService_no          2
#define CelloPri_internal_no                 3
#define CelloPri_freeMemory_no               4
#define CelloPri_terminateService_no         5
#define CelloPiu3_getHuntPath_no             6
#define CelloPiu10_getPid_no                 7
#define CelloPiu3_getOwnIdentity_no          8
#define CelloPri9_getOwnIdentity_no          9
#define CelloPri8_getOwnIdentity_no         10
#define CelloPri9_getPiuOrDeviceIdentity_no 11
#define CelloPri8_getPiuOrDeviceIdentity_no 12
#define CelloPiu4_restartOwnPiu_no          13
#define CelloPiu4_restartOtherPiu_no        14
#define CelloPiu3_getLinkHandlerName_no     15

extern bool TRI_trace_enabled;

union SIGNAL
{
  SIGSELECT sigNo;
  CelloPriInitiateServiceCfm celloPriInitiateServiceCfm;
  CelloPriInitiateServiceSus celloPriInitiateServiceSus;
  CelloPriInitiateServiceRej celloPriInitiateServiceRej;
  CelloPriTerminateServiceCfm celloPriTerminateServiceCfm;
  CelloPiu3GetHuntPathCfm celloPiu3GetHuntPathCfm;
  CelloPiu3GetLHNameCfm celloPiu3GetLHNameCfm;
  CelloPiu3GetOwnIdCfm celloPiu3GetOwnIdCfm;
  CelloPiu4RestartPiuCfm celloPiu4RestartPiuCfm;
  CelloPiu4RestartPiuRej celloPiu4RestartPiuRej;
  CelloPri8GetIdentityCfm celloPri8GetIdentityCfm;
  CelloPri9GetIdentityCfm celloPri9GetIdentityCfm;
  CelloPiu10OperationalPidCfm celloPiu10OperationalPidCfm;
};

static struct {
  void *memory;
  OSADDRESS proxyPointer;
  U32 rank;
  Boolean escalation;
  char *cause;
  U32 clientId;
} restartData;

extern void restartHelper(void);

ei_x_buff
send_sig_pri(void **priMemory_p, union SIGNAL *sig_p, int func,
	     ei_x_buff args) {
  CelloPriResult celloPriResult;
  ei_x_buff resp;

  QTRACE3(1, "send_sig_pri func=  %d \n", func);

  ei_x_new(&resp);

  switch (func) {

    /*-----------------------------------------------*/
  case CelloPri_initiateMemory_no:

    APPLOG("test_pri, %s, %s", "initiateMemory", VERSION);

    *priMemory_p = CelloPri_initiateMemory();

    ei_x_format(&resp, "{ok, memory_initiated}");
    return resp;
    /*-----------------------------------------------*/
  case CelloPri_initiateService_no:
    {
      U32 pvFirstWanted;
      U32 pvSecondWanted;
      U32 pvThirdWanted;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&pvFirstWanted);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&pvSecondWanted);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&pvThirdWanted);
      celloPriResult = CelloPri_initiateService(*priMemory_p,
						pvFirstWanted,
						pvSecondWanted,
						pvThirdWanted);

      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPri_internal_no:
    celloPriResult = CelloPri_internal(*priMemory_p, sig_p);
    //    free_buf(&sig_p);

    ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
    return resp;
    /*-----------------------------------------------*/
  case CelloPri_freeMemory_no:
    CelloPri_freeMemory(priMemory_p);

    ei_x_format(&resp, "{ok, memory_freed}");
    return resp;
    /*-----------------------------------------------*/
  case CelloPri_terminateService_no:
    celloPriResult = CelloPri_terminateService(*priMemory_p, NULL);

    ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
    return resp;
    /*-----------------------------------------------*/
  case CelloPiu3_getHuntPath_no:
    {
      U32   piuInstanceId;
      char *processName_p;
      U32   clientId;
      int type, size;
      QTRACE2(1, "getHuntPath");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&piuInstanceId);
      ei_get_type(args.buff, &args.index, &type, &size);
      processName_p = malloc(size+1);
      ei_decode_string(args.buff, &args.index, processName_p);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

      celloPriResult = CelloPiu3_getHuntPath(*priMemory_p,
					     piuInstanceId,
					     processName_p,
					     clientId);
      free(processName_p);

      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPiu10_getPid_no:
    {
      U32   piuInstanceId;
      U32   clientId;

      QTRACE2(1, "getPid10");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&piuInstanceId);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

      celloPriResult = CelloPiu10_getPid(*priMemory_p, piuInstanceId, clientId);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPiu3_getOwnIdentity_no:
    QTRACE2(1, "getOwnIdentity");
    celloPriResult = CelloPiu3_getOwnIdentity(*priMemory_p);
    ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
    return resp;
    /*-----------------------------------------------*/
  case CelloPri9_getOwnIdentity_no:
    {
      U32   clientId;

      QTRACE2(1, "getOwnIdentity9");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

     celloPriResult = CelloPri9_getOwnIdentity(*priMemory_p, clientId);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPri8_getOwnIdentity_no:
    {
      U32   clientId;

      QTRACE2(1, "getOwnIdentity8");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

      celloPriResult = CelloPri8_getOwnIdentity(*priMemory_p, clientId);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPri9_getPiuOrDeviceIdentity_no:
    {
      U32   piuOrDeviceId;
      U32   clientId;

      QTRACE2(1, "getPiuOrDeviceIdentity9");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&piuOrDeviceId);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

      celloPriResult = CelloPri9_getPiuOrDeviceIdentity(*priMemory_p,
							piuOrDeviceId,
							clientId);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPri8_getPiuOrDeviceIdentity_no:
    {
      U32   piuOrDeviceId;
      U32   clientId;

      QTRACE2(1, "getPiuOrDeviceIdentity8");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&piuOrDeviceId);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

      celloPriResult = CelloPri8_getPiuOrDeviceIdentity(*priMemory_p,
							piuOrDeviceId,
							clientId);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPiu4_restartOwnPiu_no:
    {
      U32     restartRank;
      Boolean restartEscalation;
      char    *restartCause;
      U32     clientId;
      int type, size;
      const char *request = "restartOwnPiu";

      APPLOG("test_pri: %s", request);
      QTRACE2(1, request);
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&restartRank);
      ei_decode_boolean(args.buff, &args.index, (int *)&restartEscalation);
      ei_get_type(args.buff, &args.index, &type, &size);
      restartCause = malloc(size+1);
      ei_decode_string(args.buff, &args.index, restartCause);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);
      APPLOG("test_pri: %s, "
          "message decoded, rank: %u, escalation: %d, cause: %s, client: %u",
          request,
          restartRank,
          restartEscalation,
          restartCause,
          clientId);

      restartData.memory = *priMemory_p;
      restartData.proxyPointer = get_envp(current_process(), "PRI_PROXY_MEM");
      restartData.rank = restartRank;
      restartData.escalation = restartEscalation;
      restartData.cause = restartCause;
      restartData.clientId = clientId;

      char *block_name;
      asprintf(&block_name, "ift_block_restart_helper_%u", clientId);
      PROCESS Block = create_block(block_name, 0, 0, 0, 0);
      APPLOG("test_pri: %s, block created: %s", request, block_name);
      free(block_name);
      char *client_name;

      asprintf(&client_name, "ift_client_restart_helper");
      PROCESS pid = create_process(OS_PRI_PROC,
          client_name,
          restartHelper,
          32768, 4, 0, Block,
          NULL, 0, 0);
      free(client_name);
      start(pid);
      APPLOG("test_pri: %s, helper started: %u", request, pid);

      // this process really doesn't know if the restart succeeded,
      // just hope for the best
      ei_x_format(&resp, "{ok, ~i}", CELLO_PRI_OK);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPiu4_restartOtherPiu_no:
    {
      U32     piuInstanceId;
      U32     restartRank;
      Boolean restartEscalation;
      char    *restartCause;
      U32     clientId;
      int type, size;

      QTRACE2(1, "restartOtherPiu");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&piuInstanceId);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&restartRank);
      ei_decode_boolean(args.buff, &args.index, (int *)&restartEscalation);
      ei_get_type(args.buff, &args.index, &type, &size);
      restartCause = malloc(size+1);
      ei_decode_string(args.buff, &args.index, restartCause);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

      celloPriResult = CelloPiu4_restartOtherPiu(*priMemory_p,
						 piuInstanceId,
						 restartRank,
						 restartEscalation,
						 restartCause,
						 clientId);

      free(restartCause);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
    /*-----------------------------------------------*/
  case CelloPiu3_getLinkHandlerName_no:
    {
      U32   piuInstanceId;
      U32   clientId;

      QTRACE2(1, "getLinkHandlerName3");
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
		      (long unsigned int *)&piuInstanceId);
      ei_decode_ulong(args.buff, &args.index, (long unsigned int *)&clientId);

      celloPriResult = CelloPiu3_getLinkHandlerName(*priMemory_p, piuInstanceId,
						    clientId);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloPriResult);
      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}

ei_x_buff
recv_sig_pri(void **priMemory_p, union SIGNAL *sig_p) {
  ei_x_buff resp;

  //    printf("==================test_pri:recv_sig_pri  %x\n", sig_p->sigNo);
  QTRACE3(1, "recv_sig_pri sigNo=  %x \n", sig_p->sigNo);

  ei_x_new(&resp);
  switch(sig_p->sigNo) {
    /* Interface Management Signals */
  case CELLO_PRI_INITIATE_SERVICE_CFM:
    ei_x_format(&resp, "{signal, {~i,~i,~i}}",
                (unsigned int) CELLO_PRI_INITIATE_SERVICE_CFM,
                (unsigned int) sig_p->celloPriInitiateServiceCfm.signalRevision
		,
                (unsigned int) sig_p->celloPriInitiateServiceCfm.selectedPV);
    break;
  case CELLO_PRI_INITIATE_SERVICE_SUS:
    ei_x_format(&resp, "{signal, {~i,~i,~i}}",
                (unsigned int) CELLO_PRI_INITIATE_SERVICE_SUS,
                (unsigned int) sig_p->celloPriInitiateServiceSus.signalRevision
		,
                (unsigned int) sig_p->celloPriInitiateServiceSus.highestPV);
    break;
  case CELLO_PRI_INITIATE_SERVICE_REJ:
    ei_x_format(&resp, "{signal, {~i, ~i, ~i}}",
                (unsigned int) CELLO_PRI_INITIATE_SERVICE_REJ,
                (unsigned int) sig_p->celloPriInitiateServiceRej.signalRevision
		,
                (unsigned int) sig_p->celloPriInitiateServiceRej.highestPV);
    break;
  case CELLO_PRI_SERVER_UP_IND:
    ei_x_format(&resp, "{signal, {~i}}",
                (unsigned int) CELLO_PRI_SERVER_UP_IND);
    break;
  case CELLO_PRI_SERVER_DOWN_IND:
    ei_x_format(&resp,
		"{signal, {~i}}",
                (unsigned int) CELLO_PRI_SERVER_DOWN_IND);
    break;
  case CELLO_PRI_SERVER_UNPUBLISH_IND:
    ei_x_format(&resp,
		"{signal, {~i}}",
                (unsigned int) CELLO_PRI_SERVER_UNPUBLISH_IND);
    break;
  case CELLO_PRI_TERMINATE_SERVICE_CFM:
    ei_x_format(&resp,
		"{signal, {~i}}",
                (unsigned int) CELLO_PRI_TERMINATE_SERVICE_CFM);
    break;
  case CELLO_PIU3_GET_HUNT_PATH_CFM:
    ei_x_format(&resp,
		"{signal, {~i, ~s, ~i, ~i}}",
		CELLO_PIU3_GET_HUNT_PATH_CFM,
		sig_p->celloPiu3GetHuntPathCfm.huntPath,
		sig_p->celloPiu3GetHuntPathCfm.result,
		sig_p->celloPiu3GetHuntPathCfm.clientId);
    break;
  case CELLO_PIU3_GET_LH_NAME_CFM:
    ei_x_format(&resp,
		"{signal, {~i, ~i, ~s, ~i}}",
		CELLO_PIU3_GET_LH_NAME_CFM,
		sig_p->celloPiu3GetLHNameCfm.result,
		sig_p->celloPiu3GetLHNameCfm.linkHandlerName,
		sig_p->celloPiu3GetLHNameCfm.clientId);
    break;
  case CELLO_PIU3_GET_OWN_ID_CFM:

    ei_x_format(&resp,
		"{signal, {~i, ~i, ~i}}",
		CELLO_PIU3_GET_OWN_ID_CFM,
		sig_p->celloPiu3GetOwnIdCfm.piuInstanceId,
		sig_p->celloPiu3GetOwnIdCfm.result);
    break;
  case CELLO_PIU4_RESTART_PIU_CFM:
    ei_x_format(&resp,
		"{signal, {~i, ~i, ~i}}",
		CELLO_PIU4_RESTART_PIU_CFM,
		sig_p->celloPiu4RestartPiuCfm.piuInstanceId,
		sig_p->celloPiu4RestartPiuCfm.clientId);
    break;
  case CELLO_PIU4_RESTART_PIU_REJ:
    ei_x_format(&resp,
		"{signal, {~i, ~i, ~i, ~i}}",
		CELLO_PIU4_RESTART_PIU_REJ,
		sig_p->celloPiu4RestartPiuRej.result,
		sig_p->celloPiu4RestartPiuRej.piuInstanceId,
		sig_p->celloPiu4RestartPiuRej.clientId);
    break;
  case CELLO_PRI8_GET_IDENTITY_CFM:
    ei_x_format(&resp,
		"{signal, {~i, ~i, ~i, ~i, ~i, ~i, ~i, ~s, ~i, ~i}}",
		CELLO_PRI8_GET_IDENTITY_CFM,
		sig_p->celloPri8GetIdentityCfm.piuInstanceId,
		sig_p->celloPri8GetIdentityCfm.piuOrDeviceId,
		sig_p->celloPri8GetIdentityCfm.smn,
		sig_p->celloPri8GetIdentityCfm.slotNumber,
		sig_p->celloPri8GetIdentityCfm.apn,
		sig_p->celloPri8GetIdentityCfm.deviceId,
		sig_p->celloPri8GetIdentityCfm.huntPathPrefix,
		sig_p->celloPri8GetIdentityCfm.result,
		sig_p->celloPri8GetIdentityCfm.clientId
		);
    break;
  case CELLO_PRI9_GET_IDENTITY_CFM:
    ei_x_format(&resp,
		"{signal, {~i, ~i, ~i, ~i, ~i, ~i, ~i, ~i, ~s, ~i, ~i}}",
		CELLO_PRI9_GET_IDENTITY_CFM,
		sig_p->celloPri9GetIdentityCfm.piuInstanceId,
		sig_p->celloPri9GetIdentityCfm.piuOrDeviceId,
		sig_p->celloPri9GetIdentityCfm.smn,
		sig_p->celloPri9GetIdentityCfm.slotNumber,
		sig_p->celloPri9GetIdentityCfm.apn,
		sig_p->celloPri9GetIdentityCfm.deviceId,
		sig_p->celloPri9GetIdentityCfm.subrackNumber,
		sig_p->celloPri9GetIdentityCfm.huntPathPrefix,
		sig_p->celloPri9GetIdentityCfm.result,
		sig_p->celloPri9GetIdentityCfm.clientId);
    break;
  case CELLO_PIU10_OPERATIONAL_PID_CFM:
    ei_x_format(&resp,
		"{signal, {~i, ~i, ~s, ~s, ~s, ~s, ~s, ~i, ~i}}",
		CELLO_PIU10_OPERATIONAL_PID_CFM,
                sig_p->celloPiu10OperationalPidCfm.piuInstanceId,
		sig_p->celloPiu10OperationalPidCfm.pidInHw_r.productNumber,
		sig_p->celloPiu10OperationalPidCfm.pidInHw_r.productRevision,
		sig_p->celloPiu10OperationalPidCfm.pidInHw_r.productName,
		sig_p->celloPiu10OperationalPidCfm.pidInHw_r.productDate,
		sig_p->celloPiu10OperationalPidCfm.pidInHw_r.serialNumber,
		sig_p->celloPiu10OperationalPidCfm.result,
		sig_p->celloPiu10OperationalPidCfm.clientId);
    break;
  default:
    ei_x_format(&resp, "{signal, ~i}", (unsigned int) sig_p->sigNo);
  }
  return resp;
}


/**
 * Calls CelloPiu4_restartOwnPiu. The call is expected to cause the
 * entire application to be terminated. Some initial delay is enforced
 * so that the parent process has plenty of time time to handle the
 * synchronous call from the test suite.
 */
OS_PROCESS(restartHelper) {
  PROCESS const pid = current_process();

  struct timespec timeSpec = {.tv_sec=2, .tv_nsec=0};
  nanosleep(&timeSpec, NULL);

  APPLOG("restartHelper: pid: %u, about to restore: %s", pid, "PRI_PROXY_MEM");
  set_envp(current_process(), "PRI_PROXY_MEM", restartData.proxyPointer);

  APPLOG("restartHelper: pid: %u, about to call: CelloPiu4_restartOwnPiu", pid);
  CelloPriResult result = CelloPiu4_restartOwnPiu(restartData.memory,
                                           restartData.rank,
                                           restartData.escalation,
                                           restartData.cause,
                                           restartData.clientId);
  free(restartData.cause);

  // it is unclear how much of these messages that will be seen
  APPLOG("restartHelper: result: %u", result);
  for (int count = 0; count < 15; count++) {
    nanosleep(&timeSpec, NULL);
    APPLOG("restartHelper: %s", "idle");
  }
}

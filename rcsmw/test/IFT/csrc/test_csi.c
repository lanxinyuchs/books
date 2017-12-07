/* ----------------------------------------------------------------------
 * %CCaseFile:	test_csi.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R8A/3 %
 * %CCaseDate:	2017-01-16 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Tests of CSI use functions defined here.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
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
 * R2A/3      2013-02-18 erarafo     Created
 * R4A/1      2015-04-13 etxberb     Added constants for new functions.
 * R4A/2      2015-05-19 etxberb     Added new functions.
 * R4A/3      2015-09-08 uabesvi     Added new functions for cluster restart.
 * R4A/4      2015-11-26 erarafo     Wrappers around TRI trace macros.
 * R8A/1      2015-01-15 erarafo     Fix use of ei_decode_ulong() for VRCS64
 * R8A/2      2017-01-16 erarafo     Eliminate duplication
 * R8A/3      2017-01-16 erarafo     ClearCase version embedded in executable
 * ----------------------------------------------------------------------
 */



#include "master.h"
#include "clh_csi.h"
#include "clh_csi.sig"

#define Csi_get_state_no        1   /* Deprecated */
#define Csi_subscribe_no        2   /* Deprecated */
#define Csi_unsubscribe_no      3   /* Deprecated */
#define Csi_get_role_no         4   /* Deprecated */
#define Csi_subscribeRole_no    5   /* Deprecated */
#define Csi_unsubscribeRole_no  6   /* Deprecated */
#define Csi_subscribeCoreState_no         7
#define Csi_unsubscribeCoreState_no       8
#define Csi_getOwnMpid_no                 9
#define Csi_getHuntPathPrefix_no         10
#define Csi_subscribeClusterRestart_no   18
#define Csi_unsubscribeClusterRestart_no 19
#define Csi_clusterRestartReply_no       20

union SIGNAL
{
  uint32_t              msgNo;
  CsiCoreStateChangeInd csiCoreStateChangeInd;
  CsiClusterRestartInd  csiClusterRestartInd;
};

char *test_csi_version = "test_csi.c %CCaseRev:	/main/R2A/R3A/R4A/R8A/3 %";

extern bool TRI_trace_enabled;

ei_x_buff
send_sig_csi(int func, ei_x_buff args) {
  ei_x_buff resp;

  QTRACE3(1, "send_sig_csi func=  %d \n", func);

  ei_x_new(&resp);

  switch (func) {
  case Csi_subscribeCoreState_no:
    {
      U32 mpId;
      CsiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);

      result = Csi_subscribeCoreState(mpId);

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Csi_unsubscribeCoreState_no:
    {
      U32 mpId;
      CsiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);

      result = Csi_unsubscribeCoreState(mpId);

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Csi_getOwnMpid_no:
    {
      U32 mpId;
      CsiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);

      result = Csi_getOwnMpid(&mpId);

      ei_x_format(&resp, "{ok, ~i, ~i}", result, mpId);

      return resp;
    }
  case Csi_getHuntPathPrefix_no:
    {
      U32 mpId;
      char huntPathPrefix[CSI_HUNT_PATH_SIZE];
      CsiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);

      result = Csi_getHuntPathPrefix(mpId, huntPathPrefix);

      ei_x_format(&resp, "{ok, ~i, ~s}", result, huntPathPrefix);

      return resp;
    }
  case Csi_subscribeClusterRestart_no:
    {
      CsiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);

      result = Csi_subscribeClusterRestart();

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Csi_unsubscribeClusterRestart_no:
    {
      CsiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);

      result = Csi_unsubscribeClusterRestart();

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Csi_clusterRestartReply_no:
    {
      CsiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);

      result = Csi_clusterRestartReply();

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}

ei_x_buff
recv_sig_csi(union SIGNAL *sig_p) {
  ei_x_buff resp;

  ei_x_new(&resp);
  switch (sig_p->msgNo) {
  case CSI_CORE_STATE_CHANGE_IND:
    QTRACE2(1, "CSI_CORE_STATE_CHANGE_IND");
    ei_x_format(&resp, "{signal, {~i,~i,~i}}",
                (unsigned int) CSI_CORE_STATE_CHANGE_IND,
                (unsigned int) sig_p->csiCoreStateChangeInd.mpId,
                (unsigned int) sig_p->csiCoreStateChangeInd.coreState);
    return resp;
  case CSI_CLUSTER_RESTART_IND:
    QTRACE2(1, "CSI_CLUSTER_RESTART_IND");
    ei_x_format(&resp, "{signal, {~i,~i}}",
                (unsigned int) CSI_CLUSTER_RESTART_IND,
                (unsigned int) sig_p->csiClusterRestartInd.restartType);
    return resp;
  default:
    ei_x_format(&resp, "{signal, ~i}", (unsigned int) sig_p->msgNo);
    return resp;
  }
}

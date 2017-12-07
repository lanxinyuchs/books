/* ----------------------------------------------------------------------
 * %CCaseFile:	test_chi.c %
 * %CCaseRev:	/main/R3A/R4A/R8A/2 %
 * %CCaseDate:	2017-01-16 %
 * %CCaseDocNo: %
 * Author:      etxpeno
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Tests of CHI use functions defined here.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
 * R3A/1      2014-12-15 etxpeno     Created
 * R4A/1      2015-04-13 etxberb     Added 'restartCluster' case and constants
 *                                   for new functions.
 * R4A/2      2015-04-24 etxberb     Added cases for Chi_associateMp,
 *                                   Chi_disassociateMp, Chi_subscribeOpState &
 *                                   Chi_unsubscribeOpState.
 * R4A/4      2015-05-19 etxberb     Added chiOpStateChangeInd.
 * R4A/5      2015-11-26 erarafo     Wrappers around TRI trace macros.
 * R8A/1      2017-01-16 erarafo     Fix use of ei_decode_ulong for VRCS64
 * R8A/2      2017-01-16 erarafo     ClearCase version embedded in executable
 * ----------------------------------------------------------------------
 */

#include "master.h"
#include "clh_chi.h"
#include "clh_chi.sig"

#define Chi_get_state_no    1   /* Deprecated */
#define Chi_subscribe_no    2   /* Deprecated */
#define Chi_unsubscribe_no  3   /* Deprecated */
#define Chi_subscribeOpState_no    4
#define Chi_unsubscribeOpState_no  5
#define Chi_associateMp_no         6
#define Chi_disassociateMp_no      7
#define Chi_restartCluster_no      8

union SIGNAL
{
  uint32_t msgNo;
  ChiOpStateChangeInd chiOpStateChangeInd;
};

char *test_chi_version = "test_chi.c %CCaseRev:	/main/R3A/R4A/R8A/2 %";

extern bool TRI_trace_enabled;

ei_x_buff
send_sig_chi(int func, ei_x_buff args) {
  ei_x_buff resp;

  QTRACE3(1, "send_sig_chi func = %d \n", func);

  ei_x_new(&resp);

  switch (func) {
  case Chi_associateMp_no:
    {
      uint32_t mpId;
      char *fruId;
      ChiRestartRank coreRank;
      ChiResult result;
      int type, size;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);
      ei_get_type(args.buff, &args.index, &type, &size);
      fruId = malloc(size + 1);
      ei_decode_string(args.buff, &args.index, fruId);
      coreRank = decodeToU32(&args);

      result = Chi_associateMp(mpId, fruId, coreRank);
      free(fruId);

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Chi_disassociateMp_no:
    {
      uint32_t mpId;
      ChiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);

      result = Chi_disassociateMp(mpId);

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Chi_subscribeOpState_no:
    {
      uint32_t mpId;
      ChiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);

      result = Chi_subscribeOpState(mpId);

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Chi_unsubscribeOpState_no:
    {
      uint32_t mpId;
      ChiResult result;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      mpId = decodeToU32(&args);

      result = Chi_unsubscribeOpState(mpId);

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  case Chi_restartCluster_no:
    {
      ChiRestartType restartType;
      ChiRestartRank restartRank;
      char *restartCause;
      ChiResult result;
      int type, size;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      restartType = decodeToU32(&args);
      restartRank = decodeToU32(&args);
      ei_get_type(args.buff, &args.index, &type, &size);
      restartCause = malloc(size + 1);
      ei_decode_string(args.buff, &args.index, restartCause);

      result = Chi_restartCluster(restartType, restartRank, restartCause);
      free(restartCause);

      ei_x_format(&resp, "{ok, ~i}", result);

      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}

ei_x_buff
recv_sig_chi(union SIGNAL *sig_p) {
  ei_x_buff resp;

  ei_x_new(&resp);
  switch (sig_p->msgNo) {
  case CHI_OP_STATE_CHANGE_IND:
    QTRACE2(1, "CHI_OP_STATE_CHANGE_IND");
    ei_x_format(&resp, "{signal, {~i, ~i, ~i}}",
                (unsigned int) CHI_OP_STATE_CHANGE_IND,
                (unsigned int) sig_p->chiOpStateChangeInd.mpId,
                (unsigned int) sig_p->chiOpStateChangeInd.operState);
    return resp;
  default:
    ei_x_format(&resp, "{signal, ~i}", (unsigned int) sig_p->msgNo);
    return resp;
  }
}

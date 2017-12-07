/* ----------------------------------------------------------------------
 * %CCaseFile:	test_buti.c %
 * %CCaseRev:	/main/R2A/R8A/5 %
 * %CCaseDate:	2017-01-18 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Support for BUTI tests.
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
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R8A/1      2017-01-15 erarafo     Fix use of ei_decode_ulong() for VRCS64
 * R8A/2      2017-01-16 etxpeno     correction of R8A/1 fix
 * R8A/3      2017-01-16 erarafo     Eliminate duplication
 * R8A/4      2017-01-16 erarafo     ClearCase version embedded in executable
 * R8A/5      2017-01-18 erarafo     Fix more cases of unsafe decoding
 * ----------------------------------------------------------------------
 */
#include "master.h"
#include "cello_buti.h"
#include "cello_buti.sig"

#define CelloButi_initiateMemory_no           1
#define CelloButi_initiateService_no          2
#define CelloButi_internal_no                 3
#define CelloButi_freeMemory_no               4
#define CelloButi_terminateService_no         5
#define CelloButi_subscribeButtonEvent_no     6
#define CelloButi_unsubscribeButtonEvent_no   7
#define CelloButi_changeFeedBackMode_no       8
union SIGNAL
{
  SIGSELECT sigNo;
  CelloButiInitiateServiceCfm        celloButiInitiateServiceCfm;
  CelloButiInitiateServiceRej        celloButiInitiateServiceRej;
  CelloButiSubscribeButtonEventCfm   celloButiSubscribeButtonEventCfm;
  CelloButiUnsubscribeButtonEventCfm celloButiUnsubscribeButtonEventCfm;
  CelloButiChangeFeedbackModeCfm     celloButiChangeFeedbackModeCfm;
  CelloButiChangeFeedbackModeRej     celloButiChangeFeedbackModeRej;
  CelloButiTerminateServiceCfm       celloButiTerminateServiceCfm;
  CelloButiEventInd                  celloButiEventInd;
};

char *test_buti_version = "test_buti.c %CCaseRev:	/main/R2A/R8A/5 %";

ei_x_buff
send_sig_buti(void **butiMemory_p, union SIGNAL *sig_p, int func,
	      ei_x_buff args) {
  CelloButiResult celloButiResult;
  ei_x_buff resp;

  ei_x_new(&resp);

  switch (func) {

  case CelloButi_initiateMemory_no:
    *butiMemory_p = CelloButi_initiateMemory();

    ei_x_format(&resp, "{ok, memory_initiated}");
    return resp;

  case CelloButi_initiateService_no:
    {
      U32 pvFirstWanted;
      U32 pvSecondWanted;
      U32 pvThirdWanted;
      U32 clientRef;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      pvFirstWanted = decodeToU32(&args);
      pvSecondWanted = decodeToU32(&args);
      pvThirdWanted = decodeToU32(&args);
      clientRef = decodeToU32(&args);

      celloButiResult = CelloButi_initiateService(*butiMemory_p,
						  pvFirstWanted,
						  pvSecondWanted,
						  pvThirdWanted,
						  clientRef);

      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloButiResult);
      return resp;
    }

  case CelloButi_internal_no:
    celloButiResult = CelloButi_internal(*butiMemory_p, sig_p);
    ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloButiResult);
    return resp;

  case CelloButi_freeMemory_no:
    celloButiResult = CelloButi_freeMemory(butiMemory_p);

    ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloButiResult);
    return resp;

  case CelloButi_terminateService_no:
    {
      U32 clientRef;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      clientRef = decodeToU32(&args);

      celloButiResult = CelloButi_terminateService(*butiMemory_p, clientRef);

      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloButiResult);
      return resp;
    }

  case CelloButi_subscribeButtonEvent_no:
    {
      U32 clientRef;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      clientRef = decodeToU32(&args);

      celloButiResult = CelloButi_subscribeButtonEvent(*butiMemory_p,
						       clientRef);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloButiResult);
      return resp;
    }

  case CelloButi_unsubscribeButtonEvent_no:
    {
      U32 clientRef;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      clientRef = decodeToU32(&args);

      celloButiResult = CelloButi_unsubscribeButtonEvent(*butiMemory_p,
							 clientRef);
      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloButiResult);
      return resp;
    }

  case CelloButi_changeFeedBackMode_no:
    {
      Boolean feedbackMode;
      U32 clientRef;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      feedbackMode = (Boolean)decodeToU32(&args);
      clientRef = decodeToU32(&args);

      celloButiResult = CelloButi_changeFeedBackMode(*butiMemory_p,
						     feedbackMode,
						     clientRef);

      ei_x_format(&resp, "{ok, ~i}", (unsigned int) celloButiResult);
      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}

ei_x_buff
recv_sig_buti(void **butiMemory_p, union SIGNAL *sig_p) {
  ei_x_buff resp;

  ei_x_new(&resp);

  switch(sig_p->sigNo) {
  case CELLO_BUTI_INITIATE_SERVICE_CFM:
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i}}",
                (unsigned int)CELLO_BUTI_INITIATE_SERVICE_CFM,
                (unsigned int)sig_p->celloButiInitiateServiceCfm.signalRevision,
                (unsigned int)sig_p->celloButiInitiateServiceCfm.selectedPV,
                (unsigned int)sig_p->celloButiInitiateServiceCfm.clientRef);
     break;
  case CELLO_BUTI_INITIATE_SERVICE_REJ:
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i,~i}}",
                (unsigned int)CELLO_BUTI_INITIATE_SERVICE_REJ,
                (unsigned int)sig_p->celloButiInitiateServiceRej.signalRevision,
                (unsigned int)sig_p->celloButiInitiateServiceRej.highestSupportedPV,
                (unsigned int)sig_p->celloButiInitiateServiceRej.rejectReason,
                (unsigned int)sig_p->celloButiInitiateServiceRej.clientRef);
    break;
  case CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM:
    ei_x_format(&resp, "{signal, {~i,~i}}",
                (unsigned int)CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM,
                (unsigned int)sig_p->celloButiSubscribeButtonEventCfm.clientRef);
    break;
  case CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM:
    ei_x_format(&resp, "{signal, {~i,~i}}",
                (unsigned int)CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM,
                (unsigned int)sig_p->celloButiUnsubscribeButtonEventCfm.clientRef);
    break;
  case CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM:
    ei_x_format(&resp, "{signal, {~i,~i,~i}}",
                (unsigned int)CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,
                (unsigned int)sig_p->celloButiChangeFeedbackModeCfm.clientRef,
                (unsigned int)sig_p->celloButiChangeFeedbackModeCfm.feedBackMode);
    break;
  case CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ:
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i}}",
                (unsigned int)CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ,
                (unsigned int)sig_p->celloButiChangeFeedbackModeRej.rejectReason,
                (unsigned int)sig_p->celloButiChangeFeedbackModeRej.clientRef,
                (unsigned int)sig_p->celloButiChangeFeedbackModeRej.feedBackMode);
    break;
  case CELLO_BUTI_TERMINATE_SERVICE_CFM:
    ei_x_format(&resp, "{signal, {~i,~i}}",
                (unsigned int)CELLO_BUTI_TERMINATE_SERVICE_CFM,
                (unsigned int)sig_p->celloButiTerminateServiceCfm.clientRef);
    break;
  case CELLO_BUTI_EVENT_IND:
    ei_x_format(&resp, "{signal, {~i,~i}}",
                (unsigned int)CELLO_BUTI_EVENT_IND,
                (unsigned int)sig_p->celloButiEventInd.buttonEventType);
    break;
  default:
    ei_x_format(&resp, "{signal, ~i}", (unsigned int) sig_p->sigNo);
  }
  return resp;
}

/* ----------------------------------------------------------------------
 * %CCaseFile:	test_cci.c %
 * %CCaseRev:
 * %CCaseDate:	2017-06-28 %
 * %CCaseDocNo: %
 * Author:	ekurnik
 * Author: Nikola Kurdija, <nikola.kurdija@ericsson.com>
 *
 * Short description:
 * Tests of CCI use functions defined here.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
 * R5A/1	 2016-02-19	 ekurnik	 Created
 * R10A/1    2017-06-27  estjako     Added CCI2_SUBSCRIBE signal
 * R10A/2    2017-06-27  enatdok     Minor fix in reading PV
 * R10A/4    2017-06-28  estjako     Added CCI2_NTP_STATE_IND signal
 */

#include <sys/types.h>
#include <unistd.h>
#include "master.h"

#include "cci.h"
#include "cci.sig"

union SIGNAL {
	SIGSELECT sigNo;
	CciSubscribeReq cciSubscribeReq;
	CciUnsubscribeReq cciUnsubscribeReq;
	Cci2SubscribeReq cci2SubscribeReq;
	CciTimeUpdateInd cciTimeUpdate;
	Cci2NtpStateInd cci2NtpStateInd;
	CciSubscribeCfm cciSubscribeCfm;
	CciSubscribeRej cciSubscribeRej;
	CciUnsubscribeCfm cciUnsubscribeCfm;
	CciUnsubscribeRej cciUnsubscribeRej;
	CciServerDownInd cciServerDownInd;
};

extern bool TRI_trace_enabled;

static long
decodeLong(const char* buffer, int* index);
static void
verifyTuple(const char* buffer, int* index, int expectedArity, char* tag);

ei_x_buff send_sig_cci (int func, ei_x_buff args) {

    QENTER2("send_sig_cci(), func: %d", func);

    ei_x_buff resp;
    ei_x_new(&resp);

    PROCESS cci_mbox;
    hunt(CCI_MBOX_NAME, 0, &cci_mbox, NULL);

	switch(func) {
	case CCI_SUBSCRIBE_REQ:
		QTRACE2(3, "send: CciSubscribeReq");

		union SIGNAL *subscribe_msg = alloc(sizeof(CciSubscribeReq), CCI_SUBSCRIBE_REQ);
		subscribe_msg->cciSubscribeReq.pid = getpid();
		send(&subscribe_msg, cci_mbox);

		ei_x_format(&resp, "{ok, ~i}", func);
		break;
	case CCI_UNSUBSCRIBE_REQ:
		QTRACE2(3, "send: CciUnsubscribeReq");

		union SIGNAL *unsubscribe_msg = alloc(sizeof(CciUnsubscribeReq), CCI_UNSUBSCRIBE_REQ);
		send(&unsubscribe_msg, cci_mbox);

		ei_x_format(&resp, "{ok, ~i}", func);
		break;
	case CCI2_SUBSCRIBE_REQ:
		QTRACE2(3, "send: Cci2SubscribeReq");

		union SIGNAL *subscribe2_msg = alloc(sizeof(Cci2SubscribeReq), CCI2_SUBSCRIBE_REQ);
		subscribe2_msg->cci2SubscribeReq.pid = getpid();
		verifyTuple(args.buff, &args.index, 1, "CCI2_SUBSCRIBE_REQ");
		subscribe2_msg->cci2SubscribeReq.PV = (CciProtocolVersion)decodeLong(args.buff, &args.index);
		send(&subscribe2_msg, cci_mbox);

		ei_x_format(&resp, "{ok, ~i}", func);
		break;
	default:
		QTRACE3(3, "unknown function code: %d", func);
		ei_x_format(&resp, "{nok, ~s. ~i}", "unknown function code", func);
	}

	return resp;
}

ei_x_buff recv_sig_cci(union SIGNAL *sig_p) {
	QENTER2("recv_sig_cci(), sigNo: %d", sig_p->sigNo);

	ei_x_buff resp;
	ei_x_new(&resp);

	switch(sig_p->sigNo) {
	case CCI_TIME_UPDATE_IND:
		QTRACE2(3, "got timestep indication");
		ei_x_format(&resp, "{signal, {ok, ~i, ~i, ~i}}", sig_p->sigNo, sig_p->cciTimeUpdate.reason, sig_p->cciTimeUpdate.timediff);
		break;
	case CCI2_NTP_STATE_IND:
		QTRACE2(3, "got ntp state indication");
		ei_x_format(&resp, "{signal, {ok, ~i, ~i, ~i}}", sig_p->sigNo, sig_p->cci2NtpStateInd.cci_ntp_state, sig_p->cci2NtpStateInd.offset);
		break;
	case CCI_SUBSCRIBE_CFM:
		QTRACE2(3, "got subscribe confirm");
		ei_x_format(&resp, "{signal, {ok, ~i}}", sig_p->sigNo);
		break;
	case CCI_SUBSCRIBE_REJ:
		QTRACE2(3, "got subscribe reject");
		ei_x_format(&resp, "{signal, {ok, ~i, ~i}}", sig_p->sigNo, sig_p->cciSubscribeRej.rejectReason);
		break;
	case CCI_UNSUBSCRIBE_CFM:
		QTRACE2(3, "got unsubscribe confirm");
		ei_x_format(&resp, "{signal, {ok, ~i}}", sig_p->sigNo);
		break;
	case CCI_UNSUBSCRIBE_REJ:
		QTRACE2(3, "got unsubscribe reject");
		ei_x_format(&resp, "{signal, {ok, ~i, ~i}}", sig_p->sigNo, sig_p->cciUnsubscribeRej.rejectReason);
		break;
	case CCI_SERVER_DOWN_IND:
		QTRACE2(3, "got server down indication");
		ei_x_format(&resp, "{signal, {ok, ~i}}", sig_p->sigNo);
		break;
	default:
		QTRACE3(3, "unknown sigNo: %d", sig_p->sigNo);
		ei_x_format(&resp, "{signal, {nok, ~s. ~i}}", "unknown sigNo", sig_p->sigNo);
	}

	return resp;
}

static long
decodeLong(const char* buffer, int* index) {
  long result;
  ei_decode_long(buffer, index, &result);
  return result;
}

static void
verifyTuple(const char* buffer, int* index, int expectedArity, char* tag) {
  int arity;
  ei_decode_tuple_header(buffer, index, &arity);
  if (arity != expectedArity) {
    QTRACE_ERROR4("tuple arity mismatch, expected: %d, actual: %d, tag: %s",
		    expectedArity,
		    arity,
		    tag);
  }
}





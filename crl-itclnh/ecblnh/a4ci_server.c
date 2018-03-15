/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#define _GNU_SOURCE /* pthread_setname_np() */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>
#include <syslog.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>
#include "a4ci.sig"
#include "ecb_lttng.h"
#include "ecb_trace.h"
#include "ecb_transport.h"


#define A4CI_PORT_MIN					1
#define A4CI_PORT_MAX					2
#define A4CI_HDLC_ADDR_MIN				1
#define A4CI_HDLC_ADDR_MAX				254
#define MAX_FRAME						((A4CI_MAX_DATA_SIZE + 5) * 2)

#define HDLC_UI							0x03
#define HDLC_PF    						0x10


/* PP unit should, if exist on the address, respond in this time after
   we sent it a POLL frame. Emtpy frame with FINAL bit set is
   included in this time. */
#define HDLC_TRIBUTARY_RESPONSE_MS    40

/* Timeout between two polls of control unit towards tributary unit. */
#define HDLC_CONTROL_DELAY_MS         2


union itc_msg {
	uint32_t                      	msgno;
	struct a4ci_connEstablishReqS 	a4ci_connReq;
	struct a4ci_connEstablishCfmS 	a4ci_connCfm;
	struct a4ci_connEstablishRejS 	a4ci_connRej;
	struct a4ci_dataFwdS          	a4ci_dataFwd;
	struct a4ci_dataReqS          	a4ci_dataReq;
	struct a4ci_dataCfmS          	a4ci_dataCfm;
	struct a4ci_dataRejS          	a4ci_dataRej;
	struct a4ci_data2ReqS         	a4ci_data2Req;
	struct a4ci_data2CfmS         	a4ci_data2Cfm;
	struct a4ci_data2RejS         	a4ci_data2Rej;
	struct a4ci_linkStatReqS	  	a4ci_linkReq;
	struct a4ci_linkStatCfmS	  	a4ci_linkCfm;
	struct a4ci_linkStatRejS	  	a4ci_linkRej;
	struct ulh_transmsg_data		a4ci_ecbData;
};


extern pthread_mutex_t ebus_lock;



static uint64_t get_tick(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000000ULL + ts.tv_nsec/1000;
}

static struct ser_port *get_port(uint16_t port_no)
{
	static struct ser_port *saup = NULL;
	struct ser_port *sp = g_port;

	switch (port_no) {
		case 0:
			A4C_TRACE("Bad port (0) requested. Defaults to 1");
			break;

		case 1:
			break;;

		case 2:
			saup = saup ? : sau_init();
			sp = saup;
			break;

		default:
			A4C_ERROR("Bad port (%d) requested. Defaults to 1", port_no);
			break;
	}

	if (!sp) {
		A4C_ERROR("Failed to open port %d", port_no);
	}
	return sp;
}

static uint16_t a4ci_transact(struct ser_port *sp, uint8_t hAddr,
		uint8_t *cmd, uint16_t length, uint8_t *dst, uint16_t *perr)
{
	uint32_t select[2] = {1, ULH_TRANSMSG_DATA};
	struct timespec ts = {0, HDLC_CONTROL_DELAY_MS*1000000};
	union itc_msg *msg = NULL;
	uint64_t current_time;
	uint64_t expired_time;
	uint16_t data_length = 0;
	uint32_t tmo_ms;
	int ret;

	*perr = A4CI_OTHER_ERROR;

	pthread_mutex_lock(&ebus_lock);
	ret = ecb_transmit(sp, hAddr, HDLC_UI | HDLC_PF, cmd, length);

	current_time = get_tick();
	expired_time = current_time + (HDLC_TRIBUTARY_RESPONSE_MS * 1000);

	if (ret) {
		A4C_ERROR("ecb_write failed: %d", ret);
		goto done;
	}

	for (;;) {
		current_time = get_tick();
		if (current_time >= expired_time) {
			ATOMIC_ADD(&sp->timeouts, 1);
			*perr = A4CI_TIME_OUT;
			goto done;
		}

		tmo_ms = (uint32_t)(expired_time - current_time)/1000 + 1;
		msg = itc_receive(select, tmo_ms, ITC_FROM_ALL);

		if (msg) {
			uint8_t *src = msg->a4ci_ecbData.data.data;
			uint8_t addr = *src++;
			uint8_t ctrl = *src++;

			if (addr == hAddr) {
				uint32_t dlen = msg->a4ci_ecbData.data.size - 2;

				if ((data_length + dlen) > A4CI_MAX_DATA_SIZE) {
					A4C_ERROR("bad data length: %u (%u)",
							data_length, msg->a4ci_ecbData.data.size);
					goto done;
				}
				memcpy(dst, src, dlen);
				data_length += dlen;
				dst += dlen;

				if (ctrl & HDLC_PF) {
					*perr = 0;
					goto done;
				}
			}
			ulh_tbuff_free(&msg->a4ci_ecbData.data);
			itc_free(&msg);
		}
	}

 done:
        if (msg)
        {
           ulh_tbuff_free(&msg->a4ci_ecbData.data);
           itc_free(&msg);
           clock_nanosleep(CLOCK_MONOTONIC, 0, &ts, NULL);
        }

	pthread_mutex_unlock(&ebus_lock);
	return data_length;
}

static void conn_req(struct a4ci_connEstablishReqS *req,
			       const itc_mbox_id_t mid)
{
	union itc_msg *msg;

	req->protocolRev = ntohs(req->protocolRev);
	A4C_TRACE("Received A4CI_CONN_ESTABLISH_REQ rev: %d",
							req->protocolRev);

	if (req->protocolRev <= A4CI_PROTOCOL_REV) {
		msg = itc_alloc(sizeof(struct a4ci_connEstablishCfmS),
				A4CI_CONN_ESTABLISH_CFM);
		A4C_TRACE("Sent A4CI_CONN_ESTABLISH_CFM rev: %d",
							req->protocolRev);
	} else {
		msg = itc_alloc(sizeof(struct a4ci_connEstablishRejS),
				A4CI_CONN_ESTABLISH_REJ);
		msg->a4ci_connRej.errorCode =
			htons(A4CI_UNEXPECTED_PARAMETER_VALUE);
		msg->a4ci_connRej.protocolRev =
			htons(A4CI_PROTOCOL_REV);

		A4C_TRACE("Sent A4CI_CONN_ESTABLISH_REJ rev: %d",
		       A4CI_PROTOCOL_REV);
	}

	itc_send(&msg, mid, ITC_MY_MBOX);
}

static void data_fwd(struct a4ci_dataFwdS *fwd)
{
	int ret;
	struct ser_port *sp = get_port(ntohs(fwd->port));

	if (!sp) {
		A4C_ERROR("data_fwd failed");
		return;
	}

	fwd->length = ntohs(fwd->length);
	A4C_TRACE("Received A4CI_DATA_FWD addr:  0x%02x", fwd->hdlcAddr);

	if (fwd->hdlcAddr < A4CI_HDLC_ADDR_MIN ||
	    fwd->hdlcAddr > A4CI_HDLC_ADDR_MAX ||
	    fwd->length == 0 ||
	    fwd->length > A4CI_MAX_DATA_SIZE) {
		A4C_ERROR("data_fwd discard frame length %d",(int)fwd->length);
		return;
	}

	pthread_mutex_lock(&ebus_lock);
	ret = ecb_transmit(sp, fwd->hdlcAddr, HDLC_UI, fwd->data, fwd->length);
	usleep(HDLC_CONTROL_DELAY_MS*1000);
	pthread_mutex_unlock(&ebus_lock);

	if (ret) {
		A4C_ERROR("ecb_transmit failed: %d", ret);
	}
}

static void data_req(struct a4ci_dataReqS *req, itc_mbox_id_t mid)
{
	union itc_msg *sig;
	uint16_t error,len;
	struct ser_port *sp;

	sig = itc_alloc(sizeof(struct a4ci_dataCfmS), A4CI_DATA_CFM);
	req->length = ntohs(req->length);

	A4C_DEBUG("Received A4CI_DATA_REQ addr", NULL, req->hdlcAddr);

	if (req->hdlcAddr < A4CI_HDLC_ADDR_MIN ||
	    req->hdlcAddr > A4CI_HDLC_ADDR_MAX ||
	    req->length > A4CI_MAX_DATA_SIZE) {

		if (req->length > A4CI_MAX_DATA_SIZE) {
			A4C_ERROR("invalid parameter length: %d", req->length);
		} else {
			A4C_ERROR("invalid parameter addr: 0x%02x", req->hdlcAddr);
		}
		error = A4CI_UNEXPECTED_PARAMETER_VALUE;
		goto error_exit;
	}

	sp = get_port(ntohs(req->port));
	if (!sp) {
		error = A4CI_OTHER_ERROR;
		goto error_exit;
	}

	len = a4ci_transact(sp, req->hdlcAddr, req->data, req->length,
							sig->a4ci_dataCfm.data, &error);
	if (!error) {
		sig->a4ci_dataCfm.port = req->port;
		sig->a4ci_dataCfm.hdlcAddr = req->hdlcAddr;
		sig->a4ci_dataCfm.length = htons(len);
		itc_send(&sig, mid, ITC_MY_MBOX);
		return;
	}

 error_exit:
	sig->a4ci_dataRej.sigNo = A4CI_DATA_REJ;
	sig->a4ci_dataRej.errorCode = htons(error);
	sig->a4ci_dataRej.port = req->port;
	sig->a4ci_dataRej.hdlcAddr = req->hdlcAddr;
	itc_send(&sig, mid, ITC_MY_MBOX);
}

static void data2_req(struct a4ci_data2ReqS *req, itc_mbox_id_t mid)
{
	union itc_msg *sig;
	uint16_t error,len;
	struct ser_port *sp;

	sig = itc_alloc(sizeof(struct a4ci_data2CfmS), A4CI_DATA2_CFM);
	req->length = ntohs(req->length);
	A4C_DEBUG("Received A4CI_DATA2_REQ addr", NULL, req->hdlcAddr);

	if (req->hdlcAddr < A4CI_HDLC_ADDR_MIN ||
	    req->hdlcAddr > A4CI_HDLC_ADDR_MAX ||
	    req->length > A4CI_MAX_DATA_SIZE) {

		if (req->length > A4CI_MAX_DATA_SIZE) {
			A4C_ERROR("invalid parameter length: %d", req->length);
		} else {
			A4C_ERROR("invalid parameter addr: 0x%02x", req->hdlcAddr);
		}
		error = A4CI_UNEXPECTED_PARAMETER_VALUE;
		goto error_exit;
	}

	sp = get_port(ntohs(req->port));
	if (!sp) {
		error = A4CI_OTHER_ERROR;
		goto error_exit;
	}

	len = a4ci_transact(sp, req->hdlcAddr, req->data, req->length,
							sig->a4ci_data2Cfm.data, &error);
	if (!error) {
		sig->a4ci_data2Cfm.clientRef = req->clientRef;
		sig->a4ci_data2Cfm.port = req->port;
		sig->a4ci_data2Cfm.length = htons(len);
		sig->a4ci_data2Cfm.hdlcAddr = req->hdlcAddr;
		itc_send(&sig, mid, ITC_MY_MBOX);
		return;
	}

 error_exit:
	sig->a4ci_data2Rej.sigNo = A4CI_DATA2_REJ;
	sig->a4ci_data2Rej.clientRef = req->clientRef;
	sig->a4ci_data2Rej.errorCode = htons(error);
	sig->a4ci_data2Rej.port = req->port;
	sig->a4ci_data2Rej.hdlcAddr = req->hdlcAddr;
	itc_send(&sig, mid, ITC_MY_MBOX);
}

static void link_req(struct a4ci_linkStatReqS *req, itc_mbox_id_t mid)
{
	union itc_msg *sig;
	struct ser_port *sp;
	struct rhai_ser_stats estat;
	int ret;

	A4C_DEBUG("Received A4CI_LINK_STAT_REQ port", NULL, ntohs(req->port));

	sp = get_port(ntohs(req->port));
	if (!sp) {
		sig = itc_alloc(sizeof(sig->a4ci_linkRej), A4CI_LINK_STAT_REJ);
		sig->a4ci_linkRej.clientRef = req->clientRef;
		sig->a4ci_linkRej.port = req->port;
		sig->a4ci_linkRej.errorCode = A4CI_OTHER_ERROR;
		goto done;
	}

	sig = itc_alloc(sizeof(sig->a4ci_linkCfm), A4CI_LINK_STAT_CFM);
	sig->a4ci_linkCfm.clientRef = req->clientRef;
	sig->a4ci_linkCfm.port = req->port;
	sig->a4ci_linkCfm.nrOfUartOverrunsIsValid = 0;

	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfReceivedOctets, &sp->rx_octets);
	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfTransmittedOctets, &sp->tx_octets);
	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfReceivedHDLCFrames, &sp->rx_frames);
	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfTransmittedHDLCFrames, &sp->tx_frames);
	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfFCSErrors, &sp->fcserrors);
	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfErrCtrlFields, &sp->ctrl_errs);
	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfErrAddFields, &sp->addr_errs);
	ATOMIC_CPY(&sig->a4ci_linkCfm.nrOfTimeouts, &sp->timeouts);

	ret = rhai_ser_get_stats(sp->handle, &estat);
	if (ret == 0) {
		sig->a4ci_linkCfm.nrOfUartOverruns = htonl(estat.rx_overrun);
		sig->a4ci_linkCfm.nrOfUartOverrunsIsValid =
						estat.valid & RHAI_SER_RX_OVERRUN_VALID;
	} else {
		A4C_ERROR("rhai_ser_stats: %d", ret);
	}

 done:
	itc_send(&sig, mid, ITC_MY_MBOX);
}

void *a4ci_thread(__attribute__((unused)) void *ctx)
{
	itc_mbox_id_t  mid;
	union itc_msg *msg;

	char mbname[64];

	mid = itc_create_mailbox(A4CI_PHYSICAL_THREAD, 0);
	if (mid == ITC_NO_ID) {
		A4C_ERROR("itc_create_mailbox failed", -errno);
		goto eout;
	}

	(void)pthread_setname_np(pthread_self(), "a4ci_server");

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		itc_get_name(itc_sender(msg), mbname, sizeof(mbname));

		switch (msg->msgno) {
		case ULH_TRANSMSG_DATA:
			ulh_tbuff_free(&msg->a4ci_ecbData.data);
			break;
		case A4CI_CONN_ESTABLISH_REQ:
			conn_req(&msg->a4ci_connReq, itc_sender(msg));
			atfi.mpa4ci_mbox = mid;
			break;
		case A4CI_DATA_FWD:
			data_fwd(&msg->a4ci_dataFwd);
			break;
		case A4CI_DATA_REQ:
			data_req(&msg->a4ci_dataReq, itc_sender(msg));
			break;
		case A4CI_DATA2_REQ:
			data2_req(&msg->a4ci_data2Req, itc_sender(msg));
			break;
		case A4CI_LINK_STAT_REQ:
			link_req(&msg->a4ci_linkReq, itc_sender(msg));
			break;
		default:
			A4C_TRACE("Recv unexpected msg: 0x%x", msg->msgno);
			break;
		}

		itc_free(&msg);
	}

 eout:
	itc_delete_mailbox(mid);
	pthread_exit(0);
	return NULL;
}

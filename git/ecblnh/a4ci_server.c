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
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>
#include <syslog.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>
#include <itc.h>
#include <rhai-sys.h>
#include <rhai-ecb.h>

#include "a4ci.sig"
#include "ecb_lttng.h"
#include "ecb_trace.h"


#define A4CI_PORT_MIN						1
#define A4CI_PORT_MAX						2
#define A4CI_HDLC_ADDR_MIN					1
#define A4CI_HDLC_ADDR_MAX					254
#define MAX_FRAME							((A4CI_MAX_DATA_SIZE + 5) * 2)


#define MHP_CHAR_T     						0x7e
#define MHP_CHAR_E     						0x7d
#define MHP_A4CI_UI    						0x03
#define MHP_A4CI_PF    						0x10


/* Escape macro. */
#define MHP_CHAR_ESCAPE(ptr, c)                         \
  do {                                                  \
    if ((c) == MHP_CHAR_T || (c) == MHP_CHAR_E) {       \
      *((ptr)++) = MHP_CHAR_E;                          \
      *((ptr)++) = (c) ^ 0x20;                          \
    } else {                                            \
      *((ptr)++) = (c);                                 \
    }                                                   \
  } while (0);

/* Frame Checksum constants and macro for CRC-16-CCITT. */
#define MHP_FCS(f, c) ((uint16_t)(((f) >> 8) ^ ftab[((f)^(c)) & 0xff]))
#define MHP_FCS_INIT    0xffff
#define MHP_FCS_GOOD    0xf0b8


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
};

struct a4ci_data {
	uint32_t						sigNo;
	uint32_t						clientRef;
	uint16_t						port;
	uint16_t						length;
	uint8_t							hdlcAddr;
	uint8_t							data[MAX_FRAME];
} __attribute__((packed));

static struct ebus_transact {
	struct a4ci_data	    		*rxBuf;
	pthread_cond_t  				wait_cond;
	uint8_t 						haddr;
	uint8_t 						crcok;
} args;

static struct {
	void							*handle;
	struct ebus_transact			*args_p;
	pthread_mutex_t 				wait_lock;
	struct a4ci_linkStatCfmS 		lstat;
} a4ci;



static const uint16_t ftab[] = {
  0x0000, 0x1189, 0x2312, 0x329b, 0x4624, 0x57ad, 0x6536, 0x74bf,
  0x8c48, 0x9dc1, 0xaf5a, 0xbed3, 0xca6c, 0xdbe5, 0xe97e, 0xf8f7,
  0x1081, 0x0108, 0x3393, 0x221a, 0x56a5, 0x472c, 0x75b7, 0x643e,
  0x9cc9, 0x8d40, 0xbfdb, 0xae52, 0xdaed, 0xcb64, 0xf9ff, 0xe876,
  0x2102, 0x308b, 0x0210, 0x1399, 0x6726, 0x76af, 0x4434, 0x55bd,
  0xad4a, 0xbcc3, 0x8e58, 0x9fd1, 0xeb6e, 0xfae7, 0xc87c, 0xd9f5,
  0x3183, 0x200a, 0x1291, 0x0318, 0x77a7, 0x662e, 0x54b5, 0x453c,
  0xbdcb, 0xac42, 0x9ed9, 0x8f50, 0xfbef, 0xea66, 0xd8fd, 0xc974,
  0x4204, 0x538d, 0x6116, 0x709f, 0x0420, 0x15a9, 0x2732, 0x36bb,
  0xce4c, 0xdfc5, 0xed5e, 0xfcd7, 0x8868, 0x99e1, 0xab7a, 0xbaf3,
  0x5285, 0x430c, 0x7197, 0x601e, 0x14a1, 0x0528, 0x37b3, 0x263a,
  0xdecd, 0xcf44, 0xfddf, 0xec56, 0x98e9, 0x8960, 0xbbfb, 0xaa72,
  0x6306, 0x728f, 0x4014, 0x519d, 0x2522, 0x34ab, 0x0630, 0x17b9,
  0xef4e, 0xfec7, 0xcc5c, 0xddd5, 0xa96a, 0xb8e3, 0x8a78, 0x9bf1,
  0x7387, 0x620e, 0x5095, 0x411c, 0x35a3, 0x242a, 0x16b1, 0x0738,
  0xffcf, 0xee46, 0xdcdd, 0xcd54, 0xb9eb, 0xa862, 0x9af9, 0x8b70,
  0x8408, 0x9581, 0xa71a, 0xb693, 0xc22c, 0xd3a5, 0xe13e, 0xf0b7,
  0x0840, 0x19c9, 0x2b52, 0x3adb, 0x4e64, 0x5fed, 0x6d76, 0x7cff,
  0x9489, 0x8500, 0xb79b, 0xa612, 0xd2ad, 0xc324, 0xf1bf, 0xe036,
  0x18c1, 0x0948, 0x3bd3, 0x2a5a, 0x5ee5, 0x4f6c, 0x7df7, 0x6c7e,
  0xa50a, 0xb483, 0x8618, 0x9791, 0xe32e, 0xf2a7, 0xc03c, 0xd1b5,
  0x2942, 0x38cb, 0x0a50, 0x1bd9, 0x6f66, 0x7eef, 0x4c74, 0x5dfd,
  0xb58b, 0xa402, 0x9699, 0x8710, 0xf3af, 0xe226, 0xd0bd, 0xc134,
  0x39c3, 0x284a, 0x1ad1, 0x0b58, 0x7fe7, 0x6e6e, 0x5cf5, 0x4d7c,
  0xc60c, 0xd785, 0xe51e, 0xf497, 0x8028, 0x91a1, 0xa33a, 0xb2b3,
  0x4a44, 0x5bcd, 0x6956, 0x78df, 0x0c60, 0x1de9, 0x2f72, 0x3efb,
  0xd68d, 0xc704, 0xf59f, 0xe416, 0x90a9, 0x8120, 0xb3bb, 0xa232,
  0x5ac5, 0x4b4c, 0x79d7, 0x685e, 0x1ce1, 0x0d68, 0x3ff3, 0x2e7a,
  0xe70e, 0xf687, 0xc41c, 0xd595, 0xa12a, 0xb0a3, 0x8238, 0x93b1,
  0x6b46, 0x7acf, 0x4854, 0x59dd, 0x2d62, 0x3ceb, 0x0e70, 0x1ff9,
  0xf78f, 0xe606, 0xd49d, 0xc514, 0xb1ab, 0xa022, 0x92b9, 0x8330,
  0x7bc7, 0x6a4e, 0x58d5, 0x495c, 0x3de3, 0x2c6a, 0x1ef1, 0x0f78
};


static uint8_t target_address = 0;
static uint64_t next_tx_time = 0;
struct a4ci_linkStatCfmS *g_stat = &a4ci.lstat;

struct rhai_sys_hwinfo prodinfo;

extern pthread_mutex_t ebus_lock;


static uint64_t get_tick(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000000ULL + ts.tv_nsec/1000;
}

static void tmo_ms(struct timespec* ts, uint32_t tmo)
{
    long int nsec;
    clock_gettime(CLOCK_MONOTONIC, ts);
    ts->tv_sec += (tmo/1000);
	tmo %= 1000;

    nsec = ts->tv_nsec + (tmo*1000000);
    ts->tv_sec += (nsec/1000000000);
    ts->tv_nsec =  nsec%1000000000;
}

static int make_frame(uint8_t addr, uint8_t ctrl,
					const void *txData, uint32_t size,
			   uint8_t* tx_buf, uint32_t *frame_size)
{
	uint8_t  *to, *from;
	uint16_t fcs = MHP_FCS_INIT;
	uint32_t i;

	#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
	uint8_t BigEndian = 0 ;
	#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
	uint8_t BigEndian = 1 ;
	#endif

	to    = (uint8_t*)tx_buf ;
	from  = (uint8_t*)txData;

	/*Create frame*/
	/*Append flag*/
	*to++ = 0x7e;

	/* Append start terminator. */
	*to++ = MHP_CHAR_T;

	/* Append address field. */
	fcs = MHP_FCS(fcs, addr);
	MHP_CHAR_ESCAPE(to, addr);

	/* Append control field. */
	fcs = MHP_FCS(fcs, ctrl);
	MHP_CHAR_ESCAPE(to, ctrl);

	/* Copy transmit signal, escape and calculate FCS. */
	for (i = size; i > 0; i--, from++) {
		fcs = MHP_FCS(fcs, *from);
		MHP_CHAR_ESCAPE(to, *from);
	}

	/* Append the FCS. */
	fcs ^= 0xffff;
	if (!BigEndian) {
		fcs = (uint16_t) (((fcs & 0xff00) >> 8) | ((fcs & 0x00ff) << 8));
                MHP_CHAR_ESCAPE(to, (uint8_t) (fcs >> 8));
                MHP_CHAR_ESCAPE(to, (uint8_t) (fcs & 0xff));

	}
        else { /* Big endian format */
                MHP_CHAR_ESCAPE(to, (uint8_t) (fcs & 0xff));
	        MHP_CHAR_ESCAPE(to, (uint8_t) (fcs >> 8));
        }

	/* Append the ending terminator. */
	*to++ = MHP_CHAR_T;

	/* Calculate frame size. */
	*frame_size = (uint32_t) (to - tx_buf);
	return 0;
}

static uint8_t check_frame(struct a4ci_data *p)
{
	uint16_t fcs_cache[4], idx, fcs, crc;
	unsigned char *src, *dst;
	int length = p->port;
	int i,j;

	src = p->data - 2;
	dst = p->data - 2;
	fcs = MHP_FCS_INIT;

	for (idx = 0, i = j = 0; i < length; i++, j++) {
		if (src[i] == MHP_CHAR_T)
			break;

		if (src[i] != MHP_CHAR_E) {
			dst[j] = src[i];
		} else
			dst[j] = src[++i] ^ 0x20;

		fcs = MHP_FCS(fcs, dst[j]);
		fcs_cache[idx] = fcs;
		idx = (idx + 1) & 3;
	}

	/* strip off FCS  */
	length = j - 2;
	p->port = (uint16_t)length;

	if (fcs == MHP_FCS_GOOD)
		return 1;

	/* Endianness issue */
	idx = (idx + 1) & 3;
	fcs = fcs_cache[idx] ^ 0xffff;
	if (memcmp(&fcs, &dst[length], 2) == 0)
		return 1;

	/* Defect FCU PPs issue */
	if (dst[0] != 0x2f)
		goto err_out;

	crc = ((uint16_t)dst[length] << 8) | ((uint16_t)dst[length + 1]);
	fcs = fcs_cache[idx];
	fcs = ((fcs << 8) | (fcs >> 8)) ^ 0xffff;

	if ((fcs | 0x2020) == (crc | 0x2020))
		return 1;

 err_out:
	return 0;
}

static void check_header(uint8_t addr, uint8_t ctrl)
{
	uint8_t ctl;

	if ((addr == 0) || (addr == 255)) {
		INC_STAT(nrOfErrAddFields, 1);
	}

	if ((ctrl & 3) != 3)
		return;

	ctl = ctrl & 0xE0;
	ctl = ((ctrl & 0x0C) >> 2) | (ctl >> 3);

	if ((uint32_t)0x31109 & (1 << ctl))
		return;

	INC_STAT(nrOfErrCtrlFields, 1);
}

static void *rx_thread(__attribute__((unused)) void *arguments)
{
	struct a4ci_data *p = NULL, *q = NULL;
	itc_mbox_id_t me;
	uint8_t addr = 0;
	int length;

	me = itc_create_mailbox("a4ci_rx", 0);
	if (me == ITC_NO_ID) {
		A4C_ERROR("itc_create_mailbox failed, %d", -errno);
		pthread_exit(0);
	}

	for (;;) {
		if (!p) {
			p = (struct a4ci_data*)itc_alloc(sizeof(*p), A4CI_DATA2_CFM);
			if (!p) {
				A4C_ERROR("itc_alloc errno, %d", -errno);
				usleep(10000);
				continue;
			}
		}

		for (length = 0; length < 5;) {
			length = rhai_ecb_read(a4ci.handle, p->data - 2, MAX_FRAME);

			if ((length > 2) && (length < 5)) {
				A4C_DEBUG("RX discarded (len)", p->data - 2, length);
			}

			if (length >= 2) {
				check_header(p->data[-2],p->data[-1]);
			} else if (length < 0) {
				A4C_ERROR("rhai_ecb_read errno, %d", -errno);
			}
		}

		INC_STAT(nrOfReceivedHDLCFrames, 1);
		INC_STAT(nrOfReceivedOctets, length);

		p->port = length;
		if (!check_frame(p)) {
			INC_STAT(nrOfFCSErrors, 1);
			A4C_DEBUG("RX discarded (crc)", p->data - 2, length);
			continue;
		}

		if (p->data[-2] != target_address) {
			A4C_DEBUG("RX discarded (addr)", p->data - 2, length);
			continue;
		}

		if ((p->data[-1] & MHP_A4CI_PF) == 0) {
			A4C_DEBUG("RX packet (partial)", p->data - 2, p->port);
			addr = p->data[-2];

			if (q)
				itc_free((union itc_msg**)&q);

			q = p;
			p = NULL;
			continue;
		}

		A4C_DEBUG("Rx final", p->data - 2, p->port);

		if (q == NULL) {
			addr = p->data[-2];
			q = p;
			p = NULL;
		}

		pthread_mutex_lock(&a4ci.wait_lock);
		if (a4ci.args_p) {
			if (addr == a4ci.args_p->haddr) {
				a4ci.args_p->rxBuf = q;
				a4ci.args_p->crcok = 1;
				pthread_cond_broadcast(&a4ci.args_p->wait_cond);
				a4ci.args_p = NULL;
				q = NULL;
			} else {
				A4C_TRACE("Packet dropped. Expecting addr: 0x%02x",
								a4ci.args_p->haddr);
			}
		} else {
			A4C_TRACE("Unsolicited packet dropped. Length %d", length);
		}
		pthread_mutex_unlock(&a4ci.wait_lock);
	}
	return NULL;
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
	uint8_t buf[MAX_FRAME];
	uint64_t current_time;
	uint32_t size;
	uint32_t len;

	fwd->length = ntohs(fwd->length);
	A4C_TRACE("Received A4CI_DATA_addr 0x%02x", fwd->hdlcAddr);

	if (fwd->hdlcAddr < A4CI_HDLC_ADDR_MIN ||
	    fwd->hdlcAddr > A4CI_HDLC_ADDR_MAX ||
	    fwd->length == 0 ||
	    fwd->length > A4CI_MAX_DATA_SIZE) {
		A4C_ERROR("data_fwd discard frame length %d",(int)fwd->length);
		return;
	}

	make_frame(fwd->hdlcAddr, MHP_A4CI_UI, fwd->data, fwd->length, buf, &len);
	pthread_mutex_lock(&ebus_lock);
	current_time = get_tick();

	if (current_time < next_tx_time) {
		/* 2 ms idle time between transactions is required */
		usleep((uint32_t)(next_tx_time - current_time));
	}
	size = (uint32_t)rhai_ecb_write(a4ci.handle, &buf[0], len);
	pthread_mutex_unlock(&ebus_lock);
	next_tx_time = get_tick() + 2000;

	INC_STAT(nrOfTransmittedHDLCFrames, 1);
	INC_STAT(nrOfTransmittedOctets, size);

	if (size != len) {
		A4C_ERROR("rhai_ecb_write failed: %d",(int)size);
	}
}

static struct a4ci_data *a4ci_transact(uint8_t hdlcAddr, uint8_t *cmd,
									uint16_t length, uint16_t *perr)
{
	uint8_t tx_buf[MAX_FRAME], crcok;
	uint32_t frame_size = 0;
	uint64_t current_time;
	struct a4ci_data *rxp;
	struct timespec ts;
	int ret;

	*perr = A4CI_OTHER_ERROR;

	make_frame(hdlcAddr, MHP_A4CI_UI | MHP_A4CI_PF, cmd,
				length, tx_buf, &frame_size);

	pthread_mutex_lock(&a4ci.wait_lock);

	/*
	** Now, when we have the mutex, set up data for the rx_thread, hdlc_address
	** and an initialized condition variable.
	**
	** Next, issue the command to the peripheral. Then wait for the specified
	** timeout period. If a response arrives the conditioned wait completes
	** with success and we simply pick up the a4ci_dataCfmS pointer.
	*/

	/* Stale data from a previous timedout transaction, simply discard it */
	if (args.rxBuf) {
		A4C_TRACE("Discarding stale data");
		itc_free((union itc_msg**)&args.rxBuf);
	}

	/* Init data for rx thread */
	args.haddr = hdlcAddr;
	a4ci.args_p = &args;

	/* Now rx- thread has all data it needs to pick up the response so ok to tx */
	A4C_DEBUG("TX command", &tx_buf[0], frame_size);
	current_time = get_tick();

	if (current_time < next_tx_time) {
		/* 2 ms idle time between transactions is required */
		usleep((uint32_t)(next_tx_time - current_time));
	}

	pthread_mutex_lock(&ebus_lock);
	target_address = hdlcAddr;

	ret = rhai_ecb_write(a4ci.handle, &tx_buf[0], frame_size);
	if (ret < frame_size) {
		A4C_ERROR("ecb write  failed, %d\n", ret);
		pthread_mutex_unlock(&a4ci.wait_lock);
		pthread_mutex_unlock(&ebus_lock);
		return NULL;
	}

	INC_STAT(nrOfTransmittedHDLCFrames, 1);
	INC_STAT(nrOfTransmittedOctets, ret);

	tmo_ms(&ts, 60);
	ret = pthread_cond_timedwait(&args.wait_cond, &a4ci.wait_lock, &ts);
	pthread_mutex_unlock(&ebus_lock);
	next_tx_time = get_tick() + 2000;
	target_address = 0;

	if (ret) {
		INC_STAT(nrOfTimeouts, 1);
		A4C_TRACE("timeout, no response in 60 ms");
		pthread_mutex_unlock(&a4ci.wait_lock);
		*perr = A4CI_TIME_OUT;
		return NULL;
	}


	/* Pick up response and initialize the remaining fields before returning it */
	rxp = args.rxBuf;
	args.rxBuf = NULL;
	crcok = args.crcok;
	pthread_mutex_unlock(&a4ci.wait_lock);

	if (rxp == NULL) {
		A4C_ERROR("BUG: NULL pointer received from rx. errno, %d", errno);
		return NULL;
	}

	/* The "port" field is used temporarily to store the frame length, beacuse
	** the "length" field holds the HDLC address and CTRL octets.
	*/
	frame_size = rxp->port - 2;
	rxp->length = frame_size;

	if (crcok && (frame_size <= A4CI_MAX_DATA_SIZE)) {
		A4C_DEBUG("Returning response, bytes", rxp->data, frame_size);
		return rxp;
	}

	A4C_ERROR("corrupt frame discarded length: %d", frame_size);
	itc_free((union itc_msg**)&rxp);
	return NULL;
}

static void data_req(struct a4ci_dataReqS *req, itc_mbox_id_t mid)
{
	struct a4ci_data *rxp;
	union itc_msg *sig;
	uint16_t error;

	req->length = ntohs(req->length);
	A4C_TRACE("Received A4CI_DATA_REQ addr: 0x%02x", req->hdlcAddr);

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

	rxp = a4ci_transact(req->hdlcAddr, req->data, req->length, &error);
	if (rxp) {
		sig = itc_alloc(sizeof(struct a4ci_dataCfmS), A4CI_DATA_CFM);
		sig->a4ci_dataCfm.port = req->port;
		sig->a4ci_dataCfm.hdlcAddr = req->hdlcAddr;
		memcpy(sig->a4ci_dataCfm.data, rxp->data, rxp->length);
		sig->a4ci_dataCfm.length = htons(rxp->length);

		itc_send(&sig, mid, ITC_MY_MBOX);
		itc_free((union itc_msg**)&rxp);
		return;
	}

 error_exit:
	sig = itc_alloc(sizeof(struct a4ci_dataRejS), A4CI_DATA_REJ);
	sig->a4ci_dataRej.errorCode = htons(error);
	sig->a4ci_dataRej.port = req->port;
	sig->a4ci_dataRej.hdlcAddr = req->hdlcAddr;
	itc_send(&sig, mid, ITC_MY_MBOX);
}

static void data2_req(struct a4ci_data2ReqS *req, itc_mbox_id_t mid)
{
	struct a4ci_data *rxp;
	union itc_msg *sig;
	uint16_t error;

	req->length = ntohs(req->length);
	A4C_TRACE("Received A4CI_DATA2_REQ addr: 0x%02x", req->hdlcAddr);

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

	rxp = a4ci_transact(req->hdlcAddr, req->data, req->length, &error);
	if (rxp) {
		rxp->clientRef 	= req->clientRef;
		rxp->port 		= req->port;
		rxp->length 	= htons(rxp->length);
		rxp->hdlcAddr 	= req->hdlcAddr;

		itc_send((union itc_msg**)&rxp, mid, ITC_MY_MBOX);
		return;
	}

 error_exit:
	sig = itc_alloc(sizeof(struct a4ci_data2RejS), A4CI_DATA2_REJ);
	sig->a4ci_data2Rej.clientRef = req->clientRef;
	sig->a4ci_data2Rej.errorCode = htons(error);
	sig->a4ci_data2Rej.port = req->port;
	sig->a4ci_data2Rej.hdlcAddr = req->hdlcAddr;
	itc_send(&sig, mid, ITC_MY_MBOX);
}

static void link_req(struct a4ci_linkStatReqS *req, itc_mbox_id_t mid)
{
	union itc_msg *sig;
	struct rhai_ecb_stats estat;
	uint32_t i, *p, *q = &a4ci.lstat.nrOfReceivedOctets;
	int ret;

	sig = itc_alloc(sizeof(sig->a4ci_linkCfm), A4CI_LINK_STAT_CFM);
	sig->a4ci_linkCfm.clientRef = req->clientRef;
	p = &sig->a4ci_linkCfm.nrOfReceivedOctets;
	memset(p, 0, 10*sizeof(uint32_t));

	for (i = 0; i < 10; i++, p++, q++) {
		if (*q) {
			*p = *q;
			*q = 0;
		}
	}
	
	ret = rhai_ecb_stats(a4ci.handle, &estat);
	if (ret == 0) {	
		sig->a4ci_linkCfm.nrOfUartOverruns = estat.rx_overrun;
		sig->a4ci_linkCfm.nrOfUartOverrunsIsValid =
						estat.valid & RHAI_ECB_RX_OVERRUN_VALID;
	} else {
		A4C_ERROR("rhai_ecb_stats: %d", ret);
	}

	sig->a4ci_linkCfm.port = req->port;
	itc_send(&sig, mid, ITC_MY_MBOX);
}

void *a4ci_thread(__attribute__((unused)) void *ctx)
{
	pthread_condattr_t cattr;
	pthread_t rxthread;
	itc_mbox_id_t  mid;
	union itc_msg *msg;
	int ret;

	memset(&args, 0, sizeof(args));
	memset(&a4ci, 0, sizeof(a4ci));

	ret = rhai_sys_hwinfo(&prodinfo);
	if (ret) {
		A4C_ERROR("rhai_sys_hwinfo failed", ret);
	}
	
	pthread_mutex_init(&a4ci.wait_lock, NULL);

	mid = itc_create_mailbox(A4CI_PHYSICAL_THREAD, 0);
	if (mid == ITC_NO_ID) {
		A4C_ERROR("itc_create_mailbox failed", -errno);
		goto eout;
	}

	ret = rhai_ecb_init((void **)&a4ci.handle);
	if (ret < 0) {
		A4C_ERROR("rhai_ecb_init failed %d\n",ret);
		goto eout;
	}

	ret = pthread_condattr_init(&cattr);
	if (ret < 0) {
		A4C_ERROR("pthread_condattr_init errno:%d\n", errno);
		goto eout;
	}

	ret = pthread_condattr_setclock(&cattr, CLOCK_MONOTONIC);
	if (ret < 0) {
		A4C_ERROR("pthread_condattr_setclock errno:%d\n", errno);
		goto eout;
	}

	ret = pthread_cond_init(&args.wait_cond, &cattr);
	if (ret < 0) {
		A4C_ERROR("pthread_cond_init errno:%d\n", errno);
		goto eout;
	}


	if (pthread_create(&rxthread, NULL, rx_thread, NULL)){
		A4C_ERROR("Rx thread creation failed, %d", -errno);
		rhai_ecb_shutdown(a4ci.handle);
		goto eout;
	}

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch (msg->msgno) {
		case A4CI_CONN_ESTABLISH_REQ:
			conn_req(&msg->a4ci_connReq, itc_sender(msg));
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

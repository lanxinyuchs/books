/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/*
 * Documents: 3GPP TS 25.462 version 12.0.0 Release 12
 *            3GPP TS 25.463 version 7.5.0 Release 7
 *            IWD A2CI Server 12/15519-CEH10169/1 C
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <arpa/inet.h>
#include <sys/param.h>
#include <pthread.h>

#include <itc.h>
#include <itc_system.h>

#include <ecb_dev.h>
#include <ecb_unc.h>
#include <ecb_mux.h>

#include "a2ci.h"
#include "a2ci.sig"
#include "atfi.h"

#include "atfi_au2.h"
#include "aisg.h"
#include "atfi_hdlc.h"

#include "log.h"

/*
*******************************************************************************
**  MACROS
*******************************************************************************
*/

/* Secondary Station Modem TXRX transition time [us] */
#define MODEM_RXTX_TIME            3000

/* One Secondary Station is polled once within this interval [us] */
#define HDLC_POLL_TMO            100000

/* The Secondary Station should be able to respond within this time [ms] */
#define HDLC_RESPONSE_9600_TMO      668
#define HDLC_RESPONSE_38400_TMO     175

/* Maximum number of retransmissions */
#define HDLC_MAX_RETRANSMISSIONS      2


/*
*******************************************************************************
**  TYPES
*******************************************************************************
*/

struct fragment {
	struct fragment         *next;
	uint32_t                 size;
	uint8_t                  data[A2CI_INFO_FIELD_MAX_SIZE];
};

struct connection {

	enum {
		MODE_NDM = 0, MODE_NRM = 1
	}                        mode;

	uint8_t                  addr;
	uint8_t                  va;
	uint8_t                  vs;
	uint8_t                  vr;
	uint32_t                 retransmission;

	int                      remote_rx_rdy;
	int                      bitrate_idx;

	struct {
		struct fragment *front;
		struct fragment *back;
		struct fragment *current;
	}                        tx;

	struct ecb_unc_stat      stat;

	int                      fix_atma_bad_sw;

	uint8_t                  device_type;
	uint8_t                  uid_size;
	uint8_t                  uid[ATFI_MAX_UNIQUE_HW_ID_LENGTH];

	au2_cb_conn              cb_conn;
	au2_cb_disc              cb_disc;

	itc_monitor_id_t         client_mon;
	itc_mbox_id_t            client_mbox;
	enum {
		ST_NOT_CONNECTED = 0, ST_CONNECTED = 1
	}                        state;

	itc_mbox_id_t            a2ci_mbox;
	char                     a2ci_name[ITC_NAME_MAXLEN + 1];
};

#define AU2_AISG_CREATE_REQ 101
struct au2_aisg_create_req {
	uint32_t           msgno;
	struct connection *c;
};

#define AU2_AISG_CREATE_CFM 102
struct au2_aisg_create_cfm {
	uint32_t           msgno;
};

#define AU2_AISG_DESTROY_REQ 103
struct au2_aisg_destroy_req {
	uint32_t           msgno;
	struct connection *c;
};

#define AU2_AISG_DESTROY_CFM 104
struct au2_aisg_destroy_cfm {
	uint32_t           msgno;
};

union itc_msg {
	uint32_t                      msgno;
	struct a2ci_connEstablishReqS a2ci_conn_establish_req;
	struct a2ci_connEstablishCfmS a2ci_conn_establish_cfm;
	struct a2ci_connEstablishRejS a2ci_conn_establish_rej;
	struct a2ci_dataFwdS          a2ci_data_fwd;
	struct a2ci_dataIndS          a2ci_data_ind;
	struct au2_aisg_create_req    au2_aisg_create_req;
	struct au2_aisg_create_cfm    au2_aisg_create_cfm;
	struct au2_aisg_destroy_req   au2_aisg_destroy_req;
	struct au2_aisg_destroy_cfm   au2_aisg_destroy_cfm;
};

static char *state_to_string[] = { "ST_NOT_CONNECTED",
                                   "ST_CONNECTED    "};


/*
*******************************************************************************
**  VARIABLES
*******************************************************************************
*/

#define MIN_BITRATE_IDX 0
#define MAX_BITRATE_IDX 1

static uint32_t bitrate[2] = {
	38400,
	9600
};

static uint32_t rsp_msec_tmo[2] = {
	HDLC_RESPONSE_38400_TMO,
	HDLC_RESPONSE_9600_TMO
};

static struct timespec rsp_nsec_tmo[2] = {
	{ .tv_sec = 0, .tv_nsec = HDLC_RESPONSE_38400_TMO * 1000000 },
	{ .tv_sec = 0, .tv_nsec = HDLC_RESPONSE_9600_TMO * 1000000 }
};

static struct {
	char              *mbox_name;
	void              *dev_handle;
	char              *dev_name;
	pthread_t          thread;
	itc_mbox_id_t      mbox;
	int                exit;
	struct connection *c[256];
} au2_aisg;

/*
*******************************************************************************
**  FUNCTIONS
*******************************************************************************
*/

static int unc_wait(struct connection *c,
                    uint8_t *rsp_ctrl, void *rsp_info, uint32_t *rsp_info_size,
                    struct timespec *rsp_tmo)
{
	uint8_t rsp_ctrl0;
	int     result;

	result = ecb_unc_wait(au2_aisg.dev_handle, &c->stat, c->addr,
	                      rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
	                      rsp_info, rsp_info_size, rsp_tmo);
	if (result == 1) {
		log_hdlc(c->addr, rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
		         rsp_info, rsp_info_size, "RX(%s)", c->a2ci_name);
	}

	return result;
}

static int unc_send(struct connection *c,
                    uint8_t cmd_ctrl, void *cmd_info, uint32_t cmd_info_size)
{
	log_hdlc(c->addr, &cmd_ctrl, cmd_info, &cmd_info_size,
	         "TX(%s)", c->a2ci_name);

	return ecb_unc_send(au2_aisg.dev_handle, &c->stat, c->addr,
	                    cmd_ctrl, cmd_info, cmd_info_size);
}

static uint16_t ltohs(uint16_t u16)
{
	if (htons(1) == 1) {
		return ((u16 >> 8) & 0x00ff) | ((u16 << 8) & 0xff00);
	}

	return u16;
}


static struct connection *find_a2ci_connection(itc_mbox_id_t mbox)
{
	int addr;

	for (addr = 0; addr < 256; addr++) {
		if (au2_aisg.c[addr] &&
		    au2_aisg.c[addr]->a2ci_mbox == mbox) {
			return au2_aisg.c[addr];
		}
	}

	return 0;
}

static struct connection *find_client_connection(itc_mbox_id_t mbox)
{
	int addr;

	for (addr = 0; addr < 256; addr++) {
		if (au2_aisg.c[addr] &&
		    au2_aisg.c[addr]->client_mbox == mbox) {
			return au2_aisg.c[addr];
		}
	}

	return 0;
}

static struct connection *find_next_connection(uint8_t *addr)
{
	uint8_t last = *addr;

	do {
		(*addr)++;
		if (au2_aisg.c[*addr]) {
			return au2_aisg.c[*addr];
		}
	} while (*addr != last);

	*addr = 0;
	return 0;
}

static void use_next_bitrate(struct connection *c)
{
	c->bitrate_idx++;
	if (c->bitrate_idx > MAX_BITRATE_IDX) {
		c->bitrate_idx = MIN_BITRATE_IDX;
	}

	ecb_dev_prog(au2_aisg.dev_handle,
	             ECB_DEV_PARAM_BITRATE,
	             &bitrate[c->bitrate_idx]);
}

static void fix_atma_sw_check(struct connection *c,
                              uint8_t *data, uint32_t size)
{
	const char faulty_sw_version[] = "CXC1728234/1 R1A";
	char       text_string[256];
	uint16_t   length, cnt, idx = 0;

	/* Device Type must be ATMA */
	if (c->device_type != 2) {
		return;
	}

	/* Data size sanity check */
	if (size < 4) {
		return;
	}

	/* Message code must be Get Information */
	if (data[idx++] != 0x05) {
		return;
	}

	/* Message length sanity check */
	memcpy(&length, &data[idx], sizeof(uint16_t));
	length = ltohs(length);
	idx += 2;
	if (length > A2CI_INFO_FIELD_MAX_SIZE) {
		return;
	}

	/* Return Code must be OK */
	if (data[idx++] != 0x00) {
		return;
	}

	/* Fourth string is the software version */
	cnt = 1;
	text_string[0] = '\0';
	while (idx < length) {
		if (cnt == 4) {
			memcpy(text_string, &data[idx + 1], data[idx]);
			text_string[data[idx]] = '\0';
			log_trace2("%s:Unit SW Version: \"%s\"",
			           c->a2ci_name, text_string);
			break;
		}
		idx += 1 + data[idx];
		cnt++;
	}

	/* SW version check */
	if (strstr(text_string, faulty_sw_version) != NULL) {
		log_info("%s:ATMA with faulty SW detected",
		         c->a2ci_name);
		c->fix_atma_bad_sw = 1;
	} else {
		c->fix_atma_bad_sw = 0;
	}
}

static int fix_atma_triggered(struct connection *c,
                              uint8_t cmd_ctrl, uint8_t rsp_ctrl)
{
	if (!c->fix_atma_bad_sw) {
		return 0; /* The ATMA SW must be bad */
	}

	if (!HDLC_I_FORMAT(cmd_ctrl)) {
		return 0; /* Command must been an I-frame */
	}

	if (c->tx.current->data[0] != 0x75) {
		return 0; /* Message code must been Get Device Data */
	}

	if (!HDLC_S_FORMAT(rsp_ctrl) || HDLC_S_FUNCTION(rsp_ctrl) != HDLC_RNR) {
		return 0; /* Response must been an RNR-frame */
	}

	return 1; /* Trigger workaround. */
}

static struct fragment *tx_alloc()
{
	struct fragment *f;

	f = malloc(sizeof(struct fragment));
	if (f) {
		f->next = 0;
		f->size = 0;
	}

	return f;
}

static void tx_free(struct fragment *front)
{
	struct fragment *f;

	while (front) {
		f = front;
		front = front->next;
		free(f);
	}
}

static void tx_flush(struct connection *c)
{
	tx_free(c->tx.front);
	c->tx.front = c->tx.back = 0;

	if (c->tx.current) {
		free(c->tx.current);
		c->tx.current = 0;
	}
}

static void tx_enqueue(struct connection *c, struct fragment *fragment)
{
	if (c->mode == MODE_NDM) {
		tx_free(fragment);
	} else {
		if (!c->tx.front) {
			c->tx.front = fragment;
		} else {
			c->tx.back->next = fragment;
		}
		c->tx.back = fragment;
	}
}

static struct fragment *tx_dequeue(struct connection *c)
{
	if (c->tx.front) {
		c->tx.current = c->tx.front;
		c->tx.front = c->tx.front->next;
		if (!c->tx.front) {
			c->tx.back = 0;
		}
		c->tx.current->next = 0;
	} else {
		c->tx.current = 0;
	}

	return c->tx.current;
}


static void nrm(struct connection *c)
{
	if (c->mode == MODE_NRM) {
		return;
	}

	log_info("%s:AISG AU2 unit %u was connected using bitrate %u bit/s",
	         c->a2ci_name, c->addr, bitrate[c->bitrate_idx]);

	c->mode = MODE_NRM;
	c->va = c->vs = c->vr = 0;
	c->retransmission = 0;
	c->remote_rx_rdy = 1;
	tx_flush(c);

	/* Create A2CI Server mailbox by cloning */
	c->a2ci_mbox = itc_clone_mailbox(au2_aisg.mbox, c->a2ci_name);
	if (c->a2ci_mbox == ITC_NO_ID) {
		log_err("%s:Failed to create mailbox", c->a2ci_name);
	}

	c->cb_conn(c);
}

static void ndm(struct connection *c, const char *txt)
{
	if (c->mode == MODE_NDM) {
		return;
	}

	log_info("%s:AISG AU2 unit %u was disconnected due to %s",
	         c->a2ci_name, c->addr, txt);

	c->mode = MODE_NDM;
	tx_flush(c);

	c->cb_disc(c);

	if (c->client_mbox != ITC_NO_ID) {
		itc_unmonitor(c->client_mon);
		c->client_mbox = ITC_NO_ID;
	}

	/* Destroy the A2CI Server mailbox */
	itc_delete_mailbox(c->a2ci_mbox);
}

static void update_window(struct connection *c, uint8_t rsp_ctrl)
{
	uint8_t nr = HDLC_NR(rsp_ctrl);

	/* Validate N(R). */
	if (c->va <= c->vs) {
		if (nr < c->va || nr > c->vs) {
			ndm(c, "invalid Nr");
			return;
		}
	} else {
		if (nr < c->va && nr > c->vs) {
			ndm(c, "invalid Nr");
			return;
		}
	}

	/* Update V(A) and discard acknowledged fragment. */
	if (c->va != nr) {
		tx_free(c->tx.current);
		c->tx.current = 0;
		c->va = (c->va + 1) & HDLC_WINDOW_MASK;
	}
}

static void deliver(struct connection *c, uint8_t *data, uint32_t size)
{
	union itc_msg *msg;

	fix_atma_sw_check(c, data, size);

	if (c->client_mbox != ITC_NO_ID) {
		msg = itc_alloc(sizeof(struct a2ci_dataIndS), A2CI_DATA_IND);
		msg->a2ci_data_ind.length = htons((uint16_t) size);
		memcpy(msg->a2ci_data_ind.data, data, size);
		itc_send(&msg, c->client_mbox, c->a2ci_mbox);
	}
}

static void handle_response(struct connection *c, uint8_t rsp_ctrl,
                            uint8_t *rsp_info, uint32_t rsp_info_size)
{
	c->retransmission = 0;
	if (HDLC_S_FORMAT(rsp_ctrl)) {
		if (HDLC_S_FUNCTION(rsp_ctrl) == HDLC_RR) {
			c->remote_rx_rdy = 1;
			update_window(c, rsp_ctrl);
		} else if (HDLC_S_FUNCTION(rsp_ctrl) == HDLC_RNR) {
			c->remote_rx_rdy = 0;
			update_window(c, rsp_ctrl);
		} else {
			ndm(c, "unknown S format");
		}
	} else if (HDLC_I_FORMAT(rsp_ctrl)) {
		/* If N(S) equals V(R) then deliver the INFO field. */
		if (HDLC_NS(rsp_ctrl) == c->vr) {
			c->vr = (c->vr + 1) & HDLC_WINDOW_MASK;
			deliver(c, rsp_info, rsp_info_size);
		}
		c->remote_rx_rdy = 1;
		update_window(c, rsp_ctrl);
	} else if (rsp_ctrl == (HDLC_DM | HDLC_PF)) {
		ndm(c, "DM response");
	} else if (rsp_ctrl == (HDLC_FRMR | HDLC_PF)) {
		log_info("FRMR 0x%02x 0x%02x 0x%02x",
		         rsp_info[0], rsp_info[1], rsp_info[2]);
		ndm(c, "FRMR response");
	} else {
		log_info("Illegal control field 0x%02x", rsp_ctrl);
		ndm(c, "unexpected response");
	}
}

static void fix_atma_workaround(struct connection *c)
{
	uint8_t         rsp_ctrl;
	uint8_t         rsp_info[A2CI_INFO_FIELD_MAX_SIZE];
	uint32_t        rsp_info_size = sizeof(rsp_info);
	struct timespec t, t1;
	int             res;

	/* Use workaround for the next 1.2 seconds, or until I response. */
	clock_gettime(CLOCK_MONOTONIC, &t1);
	t1.tv_sec += 1;
	t1.tv_nsec += 200000000;
	if (t1.tv_nsec >= 1000000000) {
		t1.tv_nsec -= 1000000000;
		t1.tv_sec++;
	}

	do {
		/* Send one RR command every 100ms. */
		usleep(100000);

		unc_send(c, HDLC_RR | HDLC_PF | HDLC_VR(c->vr), 0, 0);

		res = unc_wait(c, &rsp_ctrl, rsp_info, &rsp_info_size,
		               &rsp_nsec_tmo[c->bitrate_idx]);

		if (res == 1) {
			handle_response(c, rsp_ctrl, rsp_info, rsp_info_size);
			if (HDLC_I_FORMAT(rsp_ctrl)) {
				break; /* I-frame response received */
			}
		} else { /* Time-out or error */
			c->retransmission++;
			if (c->retransmission > HDLC_MAX_RETRANSMISSIONS) {
				ndm(c, "too many retransmissions");
			}
		}

		if (c->mode == MODE_NDM) {
			break;
		}

		clock_gettime(CLOCK_MONOTONIC, &t);

	} while (t.tv_sec < t1.tv_sec ||
	         (t.tv_sec == t1.tv_sec && t.tv_nsec < t1.tv_nsec));
}

static void poll_nrm(struct connection *c)
{
	uint8_t   cmd_ctrl, rsp_ctrl;
	uint8_t  *cmd_info, rsp_info[A2CI_INFO_FIELD_MAX_SIZE];
	uint32_t  cmd_info_size, rsp_info_size = sizeof(rsp_info);
	int       res;

	if (c->remote_rx_rdy && c->va != c->vs) {
		/* Information retransmission. */
		cmd_ctrl = HDLC_I | HDLC_PF | HDLC_VS_VR(c->va, c->vr);
		cmd_info = c->tx.current->data;
		cmd_info_size = c->tx.current->size;
	} else if (c->remote_rx_rdy && tx_dequeue(c)) {
		/* Information transmission. */
		cmd_ctrl = HDLC_I | HDLC_PF | HDLC_VS_VR(c->vs, c->vr);
		cmd_info = c->tx.current->data;
		cmd_info_size = c->tx.current->size;
		c->vs = (c->vs + 1) & HDLC_WINDOW_MASK;
	} else {
		/* None information transmission. */
		cmd_ctrl = HDLC_RR | HDLC_PF | HDLC_VR(c->vr);
		cmd_info = 0;
		cmd_info_size = 0;
	}

	res = ecb_dev_begin(au2_aisg.dev_handle);
	if (res != 0) {
		c->retransmission++;
		if (c->retransmission > HDLC_MAX_RETRANSMISSIONS) {
			ndm(c, "too many retransmissions");
		}
		return;
	}

	unc_send(c, cmd_ctrl, cmd_info, cmd_info_size);

	res = unc_wait(c, &rsp_ctrl, &rsp_info, &rsp_info_size,
	               &rsp_nsec_tmo[c->bitrate_idx]);

	if (res == 1) {
		handle_response(c, rsp_ctrl, rsp_info, rsp_info_size);
		if (fix_atma_triggered(c, cmd_ctrl, rsp_ctrl)) {
			fix_atma_workaround(c);
		}
	} else { /* Time-out or error */
		c->retransmission++;
		if (c->retransmission > HDLC_MAX_RETRANSMISSIONS) {
			ndm(c, "too many retransmissions");
		}
	}

	usleep(MODEM_RXTX_TIME);
	ecb_dev_end(au2_aisg.dev_handle);
}

static void poll_snrm(struct connection *c)
{
	uint8_t rsp_ctrl;
	int     res;

	log_trace2("%s:Connection attempt using bitrate %u bit/s",
	           c->a2ci_name, bitrate[c->bitrate_idx]);

	/* Reset Device */
	aisg_reset_device(c->a2ci_name, au2_aisg.dev_handle,
	                  rsp_msec_tmo[c->bitrate_idx],
	                  c->addr);

	sched_yield(); /* Yield to other users of the device */

	/* Address Assignment */
	res = aisg_address_assignment(c->a2ci_name, au2_aisg.dev_handle,
	                              rsp_msec_tmo[c->bitrate_idx],
	                              c->addr,
	                              c->uid,
	                              c->uid_size,
	                              c->device_type);
	if (res != 0) { /* Response time-out or failure. */
		use_next_bitrate(c);
		return;
	}

	sched_yield(); /* Yield to other users of the device */

	/* Set Normal Response Mode */
	res = ecb_dev_begin(au2_aisg.dev_handle);
	if (res == 0) {
		unc_send(c, HDLC_SNRM | HDLC_PF, 0, 0);
		res = unc_wait(c, &rsp_ctrl, 0, 0,
		               &rsp_nsec_tmo[c->bitrate_idx]);
		usleep(MODEM_RXTX_TIME);
		ecb_dev_end(au2_aisg.dev_handle);
	}

	if (res == 1 && rsp_ctrl == (HDLC_UA | HDLC_PF)) {
		nrm(c);
	} else {
		use_next_bitrate(c);
	}
}

static void poll_disc(struct connection *c)
{
	if (ecb_dev_begin(au2_aisg.dev_handle) == 0) {
		unc_send(c, HDLC_DISC | HDLC_PF, 0, 0);
		unc_wait(c, 0, 0, 0, &rsp_nsec_tmo[c->bitrate_idx]);
		usleep(MODEM_RXTX_TIME);
		ecb_dev_end(au2_aisg.dev_handle);

		sched_yield(); /* Yield to other users of the device */
	}

	/* This should make sure the unit enters NoAddress state. */
	aisg_reset_device(c->a2ci_name, au2_aisg.dev_handle,
	                  rsp_msec_tmo[c->bitrate_idx],
	                  c->addr);

	ndm(c, "requested disconnect");

	sched_yield(); /* Yield to other users of the device */
}

static void handle_a2ci_conn_establish_req(struct connection *c,
                                           union itc_msg *msg)
{
	itc_mbox_id_t  client_mbox = itc_sender(msg);
	union itc_msg *rsp;

	msg->a2ci_conn_establish_req.protocolRev =
		ntohs(msg->a2ci_conn_establish_req.protocolRev);

	log_trace2("%s:Received A2CI_CONN_ESTABLISH_REQ(pr:%u)",
	           c->a2ci_name, msg->a2ci_conn_establish_req.protocolRev);

	if (msg->a2ci_conn_establish_req.protocolRev != A2CI_PROTOCOL_REV) {
		rsp = itc_alloc(sizeof(struct a2ci_connEstablishRejS),
		                A2CI_CONN_ESTABLISH_REJ);
		rsp->a2ci_conn_establish_rej.errorCode =
			htons(A2CI_UNEXPECTED_PARAMETER_VALUE);
		rsp->a2ci_conn_establish_rej.protocolRev =
			htons(A2CI_PROTOCOL_REV);
		log_trace2("Sent A2CI_CONN_ESTABLISH_REJ");
	} else {
		if (c->client_mbox != ITC_NO_ID) {
			log_info("Maximum number of A2CI clients (1) exceeded");
		} else {
			c->client_mbox = client_mbox;
			c->client_mon = itc_monitor(client_mbox, NULL);
			c->state = ST_CONNECTED;
		}
		rsp = itc_alloc(sizeof(struct a2ci_connEstablishCfmS),
		                A2CI_CONN_ESTABLISH_CFM);
		log_trace2("Sent A2CI_CONN_ESTABLISH_CFM");
	}

	itc_send(&rsp, client_mbox, c->a2ci_mbox);
}

static void handle_a2ci_data_fwd(struct connection *c, union itc_msg *msg)
{
	struct fragment *fragment;
	uint16_t         size;

	size = ntohs(msg->a2ci_data_fwd.length);

	/* Data size sanity check */
	if (size < 1 || size > A2CI_INFO_FIELD_MAX_SIZE) {
		log_info("Discarded A2CI_DATA_FWD due to invalid length (%u)",
		         size);
		return;
	}

	fragment = tx_alloc();
	if (!fragment) {
		ndm(c, "transmit overflow");
	} else {
		fragment->size = size;
		memcpy(fragment->data, msg->a2ci_data_fwd.data, size);
		tx_enqueue(c, fragment);
	}
}

static void handle_a2ci_reset_fwd(struct connection *c)
{
	int res;

	log_trace2("%s:Received A2CI_RESET_FWD", c->a2ci_name);

	res = aisg_reset_device(c->a2ci_name, au2_aisg.dev_handle,
	                        rsp_msec_tmo[c->bitrate_idx],
	                        c->addr);
	if (res != 0) {
		log_info("Reset device failed!");
	}

	sched_yield(); /* Yield to other users of the device */
}

static void handle_a2ci_msg(struct connection *c, union itc_msg *msg)
{
	switch (msg->msgno) {
	case A2CI_CONN_ESTABLISH_REQ:
		handle_a2ci_conn_establish_req(c, msg);
		break;
	case A2CI_DATA_FWD:
		if (c->state == ST_CONNECTED) {
			handle_a2ci_data_fwd(c, msg);
		}
		break;
	case A2CI_RESET_FWD:
		if (c->state == ST_CONNECTED) {
			handle_a2ci_reset_fwd(c);
		}
		break;
	default:
		break;
	}
}

static void handle_au2_aisg_create_req(struct au2_aisg_create_req *req,
                                       itc_mbox_id_t mbox)
{
	struct connection *c = req->c;
	union itc_msg     *rsp;

	if (!au2_aisg.c[c->addr]) {

		log_info("%s:Starting Primary Station on %s for AISG AU2 unit %u",
		         c->a2ci_name, au2_aisg.dev_name, c->addr);

		au2_aisg.c[c->addr] = c;

	} else {
		log_info("%s:Already started AU2 unit %u",
		         c->a2ci_name, c->addr);
	}

	rsp = itc_alloc(sizeof(struct au2_aisg_create_cfm),
	                AU2_AISG_CREATE_CFM);
	itc_send(&rsp, mbox, ITC_MY_MBOX);
}

static void handle_au2_aisg_destroy_req(struct au2_aisg_destroy_req *req,
                                        itc_mbox_id_t mbox)
{
	struct connection *c = au2_aisg.c[req->c->addr];
	union itc_msg     *rsp;

	if (c) {

		uint8_t addr = c->addr;

		log_info("%s:Terminating Primary Station for AISG AU2 unit %u",
		       c->a2ci_name, c->addr);

		if (c->mode == MODE_NRM) {
			poll_disc(c);
		}

		free(c);
		au2_aisg.c[addr] = 0;

	} else {
		log_info("%s:Already terminated AU2 unit %u",
		         c->a2ci_name, c->addr);
	}

	rsp = itc_alloc(sizeof(struct au2_aisg_destroy_cfm),
	                AU2_AISG_DESTROY_CFM);
	itc_send(&rsp, mbox, ITC_MY_MBOX);
}

static void handle_monitor_default_no(itc_mbox_id_t mbox)
{
	struct connection *c = find_client_connection(mbox);

	if (c) {
		log_trace2("%s:Lost A2CI client", c->a2ci_name);
		c->client_mbox = ITC_NO_ID;
		c->state = ST_NOT_CONNECTED;
	}
}

static void handle_au2_aisg_msg(union itc_msg *msg, itc_mbox_id_t mbox)
{
	switch (msg->msgno) {
	case AU2_AISG_CREATE_REQ:
		handle_au2_aisg_create_req(&msg->au2_aisg_create_req, mbox);
		break;
	case AU2_AISG_DESTROY_REQ:
		handle_au2_aisg_destroy_req(&msg->au2_aisg_destroy_req, mbox);
		break;
	case ITC_MONITOR_DEFAULT_NO:
		handle_monitor_default_no(mbox);
		break;
	default:
		break;
	}
}

static void *au2_aisg_thread(void *co)
{
	struct connection *c;
	union itc_msg     *msg;
	itc_mbox_id_t      recv_mbox;
	uint8_t            addr = 0;

	(void) co;

	au2_aisg.mbox = itc_create_mailbox(au2_aisg.mbox_name, 0);
	if (au2_aisg.mbox == ITC_NO_ID) {
		log_err("Failed to create mailbox");
		exit(1);
	}

	while (!au2_aisg.exit) {

		/* Receive and process messages */
		while ((msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL))) {
			recv_mbox = itc_receiver(msg);
			if (recv_mbox == au2_aisg.mbox) {
				handle_au2_aisg_msg(msg, itc_sender(msg));
			} else {
				c = find_a2ci_connection(recv_mbox);
				if (c) {
					handle_a2ci_msg(c, msg);
				}
			}
			itc_free(&msg);
		}

		c = find_next_connection(&addr);
		if (c) {
			/* Poll unit on next connection */
			ecb_dev_prog(au2_aisg.dev_handle,
			             ECB_DEV_PARAM_BITRATE,
			             &bitrate[c->bitrate_idx]);
			if (c->mode == MODE_NRM) {
				poll_nrm(c);
			} else {
				poll_snrm(c);
			}
		}

		usleep(HDLC_POLL_TMO);
	}

	itc_delete_mailbox(au2_aisg.mbox);

	return NULL;
}


void *au2_aisg_create(const char *a2ci_name,
                      uint8_t addr, uint8_t device_type,
                      uint8_t *uid, uint8_t uid_size,
                      au2_cb_conn cb_conn, au2_cb_disc cb_disc)
{
	uint32_t           sel_loc[2] = { 1, ITC_LOCATE_DEFAULT_NO };
	uint32_t           sel[2] = { 1, AU2_AISG_CREATE_CFM };
	itc_mbox_id_t      mbox;
	union itc_msg     *msg;
	struct connection *c;

	itc_locate_async(au2_aisg.mbox_name, NULL, ITC_MY_MBOX);
	msg = itc_receive(sel_loc, ITC_NO_TMO, ITC_FROM_ALL);
	mbox = itc_sender(msg);
	itc_free(&msg);

	c = calloc(1, sizeof(struct connection));
	if (!c) {
		log_err("Failed to allocate memory");
		return NULL;
	}

	c->addr = addr;
	strcpy(c->a2ci_name, a2ci_name);
	c->device_type = device_type;
	c->uid_size = uid_size;
	memcpy(c->uid, uid, uid_size);
	c->cb_conn = cb_conn;
	c->cb_disc = cb_disc;
	c->client_mbox = ITC_NO_ID;
	c->bitrate_idx = MIN_BITRATE_IDX;

	msg = itc_alloc(sizeof(struct au2_aisg_create_req),
	                AU2_AISG_CREATE_REQ);
	msg->au2_aisg_create_req.c = (struct connection*) c;
	itc_send(&msg, mbox, ITC_MY_MBOX);

	msg = itc_receive(sel, ITC_NO_TMO, mbox);
	itc_free(&msg);

	return c;
}

int au2_aisg_destroy(void *connection)
{
	uint32_t       sel_loc[2] = { 1, ITC_LOCATE_DEFAULT_NO };
	uint32_t       sel[2] = { 1, AU2_AISG_DESTROY_CFM };
	itc_mbox_id_t  mbox;
	union itc_msg *msg;

	itc_locate_async(au2_aisg.mbox_name, NULL, ITC_MY_MBOX);
	msg = itc_receive(sel_loc, ITC_NO_TMO, ITC_FROM_ALL);
	mbox = itc_sender(msg);
	itc_free(&msg);

	msg = itc_alloc(sizeof(struct au2_aisg_destroy_req),
	                AU2_AISG_DESTROY_REQ);
	msg->au2_aisg_destroy_req.c = (struct connection*) connection;
	itc_send(&msg, mbox, ITC_MY_MBOX);

	msg = itc_receive(sel, ITC_NO_TMO, mbox);
	itc_free(&msg);

	return 0;
}

int au2_aisg_init(const char *mbox_name, const char *dev_name,
                  const char *profile, const char *muxlib, int channel)
{
	const uint32_t stop_bits = 1;

	memset(&au2_aisg, 0, sizeof(au2_aisg));

	au2_aisg.mbox_name = strdup(mbox_name);
	au2_aisg.dev_name = strdup(dev_name);

	if (ecb_dev_init(&au2_aisg.dev_handle, au2_aisg.dev_name) != 0) {
		log_err("Failed to initiate device");
		goto err_dev;
	}

	if (profile[0] && muxlib[0]) {
		if (ecb_mux_init(au2_aisg.dev_handle, muxlib) != 0) {
			log_err("Failed to initiate mux");
			goto err_create;
		}
		if (ecb_mux_prog(au2_aisg.dev_handle,
		                 profile,
		                 channel) != 0) {
			log_err("Failed to retrieve mux program");
			goto err_create;
		}
	}

	if (ecb_dev_prog(au2_aisg.dev_handle,
	                 ECB_DEV_PARAM_STOP_BITS,
	                 &stop_bits) != 0) {
		log_err("Failed to program device");
		goto err_create;
	}

	/* Create the AU2 AISG thread */
	if (pthread_create(&au2_aisg.thread, NULL, au2_aisg_thread, 0) != 0) {
		log_err("Failed to create thread");
		goto err_create;
	}

	return 0;

err_create:
	ecb_dev_shutdown(au2_aisg.dev_handle);
err_dev:
	return -1;
}


int au2_aisg_shutdown(void)
{
	/* Cancel the aisg thread */
	au2_aisg.exit = 1;
	pthread_join(au2_aisg.thread, NULL);

	/* Free allocated resources */
	ecb_dev_shutdown(au2_aisg.dev_handle);
	free(au2_aisg.mbox_name);
	free(au2_aisg.dev_name);

	return 0;
}

void au2_aisg_get_link_info(char** buf, int *buf_left, void* connection,
                            uint16_t pa, uint16_t la, const char* state,
                            int first, int reset, int detailed)
{
	struct connection *c = (struct connection*) connection;

	if (first) {
		strcpy(*buf,
		       "=============================="
		       "==============================\n"
		       "AU2_AISG: \npa  la  st                    "
		       "au2_st           mode name \n");
		*buf_left -= strlen(*buf);
		*buf += strlen(*buf);

	}

	snprintf(*buf,
		 *buf_left,
		 "%-3u %-3u %-21s %-16s %-4s %s \n",
		 pa, la, state,
	         state_to_string[c->state],
	         c->mode == MODE_NRM ? "NRM" : "NDM",
	         c->a2ci_name);

	*buf_left -= strlen(*buf);
	*buf += strlen(*buf);

	if(detailed) {

		snprintf(*buf,
		         *buf_left,
		         "n_tx......: %-u\n"
		         "n_rx......: %-u\n"
		         "err_addr..: %-u\n"
		         "err_ctrl..: %-u\n"
		         "err_crc...: %-u\n"
		         "err_size..: %-u\n",
		         c->stat.n_tx,
		         c->stat.n_rx,
		         c->stat.err_addr,
		         c->stat.err_ctrl,
		         c->stat.err_crc,
		         c->stat.err_size);

		*buf_left -= strlen(*buf);
		*buf += strlen(*buf);
	}

	if(reset)
		memset(&c->stat, 0, sizeof(c->stat));
}

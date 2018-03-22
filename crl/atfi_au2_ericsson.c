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

#include "atfi_au2.h"
#include "atfi_hdlc.h"

#include "log.h"

/*
*******************************************************************************
**  MACROS
*******************************************************************************
*/

/* Fragment Header bit definitions */
#define FRAG_SF_SIZE               3
#define FRAG_IL_SIZE               1
#define FRAG_S_HEADER           0xc0
#define FRAG_F_HEADER           0x80
#define FRAG_I_HEADER           0x00
#define FRAG_L_HEADER           0x40

/* Secondary Station Modem TXRX transition time [us] */
#define MODEM_RXTX_TIME         3000

/* The Secondary Station is at least polled once within this interval [ms] */
#define HDLC_ACTIVE_POLL_TMO       0 /* Be active during user data transfer */
#define HDLC_POLL_TMO            100 /* Be less active otherwise */

/* The SNRM command is sent with this interval [s] */
#define HDLC_SNRM_TMO              5

/* The Secondary Station should be able to respond within this time [ms] */
#define HDLC_RESPONSE1_TMO        70 /* 1:st time-out, send flag character */
#define HDLC_RESPONSE2_TMO        20 /* 2:nd time-out, no response */

/* Maximum number of retransmissions */
#define HDLC_MAX_RETRANSMISSIONS   2

/* Maximum information field size */
#define HDLC_MAX_INFO_SIZE        78


/*
*******************************************************************************
**  TYPES
*******************************************************************************
*/

#pragma pack(1)
union info {
	uint8_t                  header;
	struct {
		uint8_t          header;
		uint16_t         size;
		uint8_t          data[1];
	}                        first, solitary;
	struct {
		uint8_t          header;
		uint8_t          data[1];
	}                        intermediate, last;
	uint8_t                  frmr[3];
	uint8_t                  raw[HDLC_MAX_INFO_SIZE];
};
#pragma pack()

struct fragment {
	struct fragment         *next;
	uint32_t                 size;
	union info               info;
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
	int                      active;

	struct {
		struct fragment *front;
		struct fragment *back;
		struct fragment *current;
	}                        tx;

	struct {
		uint32_t         size;
		uint32_t         index;
		uint8_t         *data;
	}                        rx;

	void                    *handle;
	struct ecb_unc_stat      stat;

	itc_mbox_id_t            thread_mbox;
	char                     thread_name[ITC_NAME_MAXLEN + 1];
	pthread_t                thread;
	int                      exit;

	au2_cb_conn              cb_conn;
	au2_cb_conn              cb_disc;

	itc_mbox_id_t            au2_mbox;
	char                     au2_name[ITC_NAME_MAXLEN + 1];
};

union itc_msg {
	uint32_t msgno;
};


/*
*******************************************************************************
**  VARIABLES
*******************************************************************************
*/

static struct timespec rsp1_tmo = {
	.tv_sec = 0,
	.tv_nsec = HDLC_RESPONSE1_TMO * 1000000
};

static struct timespec rsp2_tmo = {
	.tv_sec = 0,
	.tv_nsec = HDLC_RESPONSE2_TMO * 1000000
};

static uint8_t flag = 0x7e;


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

	result = ecb_unc_wait(c->handle, &c->stat, c->addr,
	                      rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
	                      rsp_info, rsp_info_size, rsp_tmo);
	if (result == 1) {
		log_hdlc(c->addr, rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
		         rsp_info, rsp_info_size, "RX(%s)", c->au2_name);
	}

	return result;
}

static int unc_send(struct connection *c,
                    uint8_t cmd_ctrl, void *cmd_info, uint32_t cmd_info_size)
{
	log_hdlc(c->addr, &cmd_ctrl, cmd_info, &cmd_info_size,
	         "TX(%s)", c->au2_name);

	return ecb_unc_send(c->handle, &c->stat, c->addr,
	                    cmd_ctrl, cmd_info, cmd_info_size);
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

static void tx_enqueue(struct connection *c,
                       struct fragment *front, struct fragment *back)
{
	if (c->mode == MODE_NDM) {
		tx_free(front);
	} else {
		if (!c->tx.front) {
			c->tx.front = front;
		} else {
			c->tx.back->next = front;
		}
		c->tx.back = back;
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

static int transmit(struct connection *c, union itc_msg *msg)
{
	uint8_t         *from;
	struct fragment *front, *back;
	uint32_t         remaining, size;
	itc_mbox_id_t    client_mbox;
	uint16_t         u16;

	client_mbox = itc_sender(msg);
	msg->msgno = htonl(msg->msgno);
	from = (uint8_t*) msg;
	remaining = itc_size(msg);

	/* Build the front fragment. */
	size = MIN(remaining,
	           HDLC_MAX_INFO_SIZE - (FRAG_SF_SIZE + sizeof(uint32_t)));
	front = back = tx_alloc();
	if (!front) {
		return -1;
	}
	front->size = FRAG_SF_SIZE + sizeof(uint32_t) + size;
	front->info.header = FRAG_F_HEADER;
	u16 = htons((uint16_t) remaining);
	memcpy(&front->info.first.size, &u16, sizeof(uint16_t));
	memcpy(front->info.first.data, &client_mbox, sizeof(uint32_t));
	memcpy(&front->info.first.data[sizeof(uint32_t)], from, size);
	remaining -= size;

	/* Build any other fragments including the back fragment. */
	while (remaining > 0) {
		from += size;
		size = MIN(remaining, HDLC_MAX_INFO_SIZE - FRAG_IL_SIZE);
		back->next = tx_alloc();
		if (!back->next) {
			tx_free(front);
			return -1;
		}
		back = back->next;
		back->size = FRAG_IL_SIZE + size;
		back->info.header = FRAG_I_HEADER;
		memcpy(back->info.last.data, from, size);
		remaining -= size;
	}

	/* Set fragment last bit in the back fragment. */
	back->info.header |= FRAG_L_HEADER;

	/* Enqueue the fragments front to back. */
	tx_enqueue(c, front, back);

	return 0;
}

static void nrm(struct connection *c)
{
	if (c->mode == MODE_NRM) {
		return;
	}

	log_info("%s:AU2 unit %u was connected", c->au2_name, c->addr);

	c->mode = MODE_NRM;
	c->active = 0;
	c->va = c->vs = c->vr = 0;
	c->retransmission = 0;
	tx_flush(c);

	/* Create the AU2 Server mailbox. */
	c->au2_mbox = itc_clone_mailbox(c->thread_mbox, c->au2_name);
	if (c->au2_mbox == ITC_NO_ID) {
		log_err("Failed to create mailbox, sleeping forever");
		for (;;) sleep(3600);
	}

	c->cb_conn(c);
}

static void ndm(struct connection *c, const char *txt)
{
	if (c->mode == MODE_NDM) {
		return;
	}

	log_info("%s:AU2 unit %u was disconnected due to %s",
	       c->au2_name, c->addr, txt);

	c->mode = MODE_NDM;
	c->active = 0;
	tx_flush(c);

	if (c->rx.data) {
		free(c->rx.data);
		c->rx.data = NULL;
	}

	c->cb_disc(c);

	/* Destroy the AU2 Server mailbox. */
	itc_delete_mailbox(c->au2_mbox);
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

static int deliver(struct connection *c,
                   union info *info, const uint32_t i_size)
{
	struct itc_mbox_info *mbox_info;
	itc_mbox_id_t         mbox;
	union itc_msg        *msg;
	uint32_t              f_size;
	void                 *f_data;
	uint16_t              u16;

	if (i_size < MIN(FRAG_SF_SIZE, FRAG_IL_SIZE)) {
		log_err("Too small fragment");
		return 0;
	}

	if (info->header & FRAG_F_HEADER) {
		if (c->rx.data) {
			log_err("Unexpected %c fragment",
			       info->header & FRAG_L_HEADER ? 'S' : 'F');
			return 0;
		} else if (i_size <= FRAG_SF_SIZE) {
			log_err("Illegal size (%u <= %u)",
			       i_size, FRAG_SF_SIZE);
			return 0;
		}
		memcpy(&u16, &info->first.size, sizeof(uint16_t));
		c->rx.size = ntohs(u16);
		if (c->rx.size < 4) {
			log_err("Unexpected size %u", c->rx.size);
			return 0;
		}
		c->rx.index = 0;
		c->rx.data = malloc(c->rx.size);
		if (!c->rx.data) {
			log_err("Failed to allocate %u bytes",
			       c->rx.size);
			return 0;
		}
		f_data = info->first.data;
		f_size = i_size - FRAG_SF_SIZE;
	} else {
		if (!c->rx.data) {
			log_err("Unexpected %c fragment",
			       info->header & FRAG_L_HEADER ? 'L' : 'I');
			return 0;
		} else if (i_size <= FRAG_IL_SIZE) {
			log_err("Illegal size (%u <= %u)",
			       i_size, FRAG_IL_SIZE);
			return 0;
		}
		f_data = info->last.data;
		f_size = i_size - FRAG_IL_SIZE;
	}

	/* Copy fragment data. */
	if (c->rx.index + f_size > c->rx.size) {
		log_err("Size mismatch (%u > %u)",
		       c->rx.index + f_size, c->rx.size);
		return 0;
	} else {
		memcpy(c->rx.data + c->rx.index, f_data, f_size);
		c->rx.index += f_size;
	}

	/* Deliver data. */
	if (info->header & FRAG_L_HEADER) {
		if (c->rx.index != c->rx.size) {
			log_err("Size mismatch (%u != %u)",
			       c->rx.index, c->rx.size);
			return 0;
		} else {
			memcpy(&mbox, c->rx.data, sizeof(uint32_t));
			mbox_info = itc_get_mailbox_info(mbox);
			if (mbox_info) { /* Deliver only if mailbox exists. */
				itc_free((union itc_msg**)(void*) &mbox_info);
				msg = itc_alloc(c->rx.size - sizeof(uint32_t),
				                0);
				memcpy(msg, &c->rx.data[sizeof(uint32_t)],
				       c->rx.size - sizeof(uint32_t));
				msg->msgno = ntohl(msg->msgno);
				itc_send(&msg, mbox, c->au2_mbox);
			} else {
				log_info("%s:Mailbox 0x%x does not exist",
				       c->au2_name, mbox);
			}
			free(c->rx.data);
			c->rx.data = NULL;
		}
	}

	return 1;
}

static void poll_nrm(struct connection *c)
{
	uint8_t     cmd_ctrl, rsp_ctrl;
	union info *cmd_info, rsp_info;
	uint32_t    cmd_info_size, rsp_info_size = HDLC_MAX_INFO_SIZE;
	int         active, res;

	if (c->va != c->vs) { /* Information retransmission. */
		cmd_ctrl = HDLC_I | HDLC_PF | HDLC_VS_VR(c->va, c->vr);
		cmd_info = &c->tx.current->info;
		cmd_info_size = c->tx.current->size;
		active = 1;
	} else if (tx_dequeue(c)) { /* Information transmission. */
		cmd_ctrl = HDLC_I | HDLC_PF | HDLC_VS_VR(c->vs, c->vr);
		cmd_info = &c->tx.current->info;
		cmd_info_size = c->tx.current->size;
		c->vs = (c->vs + 1) & HDLC_WINDOW_MASK;
		active = 1;
	} else { /* None information transmission. */
		cmd_ctrl = HDLC_RR | HDLC_PF | HDLC_VR(c->vr);
		cmd_info = 0;
		cmd_info_size = 0;
		active = 0;
	}

	if (ecb_dev_begin(c->handle) != 0) {
		c->retransmission++;
		if (c->retransmission > HDLC_MAX_RETRANSMISSIONS) {
			ndm(c, "too many retransmissions");
		}
		return;
	}

	unc_send(c, cmd_ctrl, cmd_info, cmd_info_size);
	res = unc_wait(c, &rsp_ctrl, &rsp_info, &rsp_info_size, &rsp1_tmo);

	if (res == 0) { /* 1:st Time-out. */
		/* Transmit a flag character and wait for response. */
		ecb_dev_write(c->handle, &flag, sizeof(flag));
		res = unc_wait(c, &rsp_ctrl, &rsp_info, &rsp_info_size, &rsp2_tmo);
	}

	usleep(MODEM_RXTX_TIME);
	ecb_dev_end(c->handle);

	if (res == 1) { /* Response received. */
		c->retransmission = 0;
		if (HDLC_S_FORMAT(rsp_ctrl)) {
			if (HDLC_S_FUNCTION(rsp_ctrl) == HDLC_RR) {
				update_window(c, rsp_ctrl);
			} else {
				ndm(c, "unknown S format");
			}
		} else if (HDLC_I_FORMAT(rsp_ctrl)) {
			active = 1;
			/* If N(S) equals V(R) then deliver the INFO field. */
			if (HDLC_NS(rsp_ctrl) == c->vr) {
				c->vr = (c->vr + 1) & HDLC_WINDOW_MASK;
				if (!deliver(c, &rsp_info, rsp_info_size)) {
					ndm(c, "invalid info in response");
				}
			}
			update_window(c, rsp_ctrl);
		} else if (rsp_ctrl == (HDLC_DM | HDLC_PF)) {
			ndm(c, "DM response");
		} else if (rsp_ctrl == (HDLC_RNR | HDLC_PF)) {
			ndm(c, "RNR response");
		} else if (rsp_ctrl == (HDLC_FRMR | HDLC_PF)) {
			log_info(
			       "FRMR 0x%02x 0x%02x 0x%02x",
			       rsp_info.frmr[0],
			       rsp_info.frmr[1],
			       rsp_info.frmr[2]);
			ndm(c, "FRMR response");
		} else {
			log_info(
			       "Illegal control field 0x%02x", rsp_ctrl);
			ndm(c, "unexpected response");
		}

	} else { /* 2:nd Time-out or Error. */
		c->retransmission++;
		if (c->retransmission > HDLC_MAX_RETRANSMISSIONS) {
			ndm(c, "too many retransmissions");
		}
	}

	if (c->mode == MODE_NRM) {
		c->active = active;
	} else {
		c->active = 0;
	}
}

static void poll_snrm(struct connection *c)
{
	uint8_t rsp_ctrl;
	int     res;

	if (ecb_dev_begin(c->handle) != 0) {
		return;
	}

	unc_send(c, HDLC_SNRM | HDLC_PF, 0, 0);
	res = unc_wait(c, &rsp_ctrl, 0, 0, &rsp1_tmo);

	if (res == 0) { /* 1:st time-out. */
		/* Transmit a flag character and wait for response. */
		ecb_dev_write(c->handle, &flag, sizeof(flag));
		res = unc_wait(c, &rsp_ctrl, 0, 0, &rsp2_tmo);
	}

	usleep(MODEM_RXTX_TIME);
	ecb_dev_end(c->handle);

	if (res == 1 && rsp_ctrl == (HDLC_UA | HDLC_PF)) {
		nrm(c);
	}
}

static void poll_disc(struct connection *c)
{
	uint8_t rsp_ctrl;
	int     res;

	if (ecb_dev_begin(c->handle) != 0) {
		ndm(c, "requested disconnect");
		return;
	}

	unc_send(c, HDLC_DISC | HDLC_PF, 0, 0);
	res = unc_wait(c, &rsp_ctrl, 0, 0, &rsp1_tmo);

	if (res == 0) { /* 1:st time-out. */
		/* Transmit a flag character and wait for response. */
		ecb_dev_write(c->handle, &flag, sizeof(flag));
		res = unc_wait(c, &rsp_ctrl, 0, 0, &rsp2_tmo);
	}

	usleep(MODEM_RXTX_TIME);
	ecb_dev_end(c->handle);

	ndm(c, "requested disconnect");
}

static void *au2_ericsson_thread(void *co)
{
	struct connection *c = (struct connection*) co;
	union itc_msg     *msg;
	uint32_t           tmo;

	c->thread_mbox = itc_create_mailbox(c->thread_name, 0);
	if (c->thread_mbox == ITC_NO_ID) {
		log_err("Failed to create mailbox");
		exit(1);
	}

	while (!c->exit) {

		/* Receive AU2 message to transmit. */
		if (c->mode == MODE_NRM) {
			if (c->active) {
				tmo = HDLC_ACTIVE_POLL_TMO;
			} else {
				tmo = HDLC_POLL_TMO;
			}
			msg = itc_receive(ITC_NOFILTER, tmo, ITC_FROM_ALL);
			if (msg) {
				if (transmit(c, msg) != 0) {
					ndm(c, "transmit overflow");
				}
				itc_free(&msg);
			}
		}

		/* Poll AU2 in Normal Response Mode. */
		if (c->mode == MODE_NRM) {
			poll_nrm(c);
		}

		/* Poll AU2 in Normal Disconnected Mode. */
		if (c->mode == MODE_NDM) {
			poll_snrm(c);
			if (c->mode == MODE_NDM) {
				sleep(HDLC_SNRM_TMO);
			}
		}
	}

	if (c->mode == MODE_NRM) {
		poll_disc(c);
	}

	itc_delete_mailbox(c->thread_mbox);

	return NULL;
}

void *au2_ericsson_create(const char *au2_name, const char *dev_name,
                          uint8_t addr,
                          au2_cb_conn cb_conn, au2_cb_disc cb_disc,
                          const char *profile, const char *muxlib,
                          int channel)
{
	const uint32_t     stop_bits = 1;
	struct connection *c;

	log_info("%s:Starting Primary Station on %s for AU2 unit %u",
	         au2_name, dev_name, addr);

	c = calloc(1, sizeof(struct connection));
	if (!c) {
		log_err("Failed to allocate memory");
		goto err_mem;
	}

	c->addr = addr;
	c->cb_conn = cb_conn;
	c->cb_disc = cb_disc;
	strcpy(c->au2_name, au2_name);

	snprintf(c->thread_name, sizeof(c->thread_name),
	         "au2_eric_thread_%u", c->addr);

	if (ecb_dev_init(&c->handle, dev_name) != 0) {
		log_err("Failed to initiate device");
		goto err_dev;
	}

	if (profile[0] && muxlib[0]) {
		if (ecb_mux_init(c->handle, muxlib) != 0) {
			log_err("Failed to initiate mux");
			goto err_prog;
		}

		if (ecb_mux_prog(c->handle, profile, channel) != 0) {
			log_err("Failed to retrieve mux program");
			goto err_prog;
		}
	}

	if (ecb_dev_prog(c->handle, ECB_DEV_PARAM_STOP_BITS, &stop_bits) != 0) {
		log_err("Failed to program device");
		goto err_prog;
	}

	/* Create the AU2 Server thread. */
	if (pthread_create(&c->thread, NULL, au2_ericsson_thread, c) != 0) {
		log_err("Failed to create thread");
		goto err_create;
	}

	return c;

err_create:
err_prog:
	ecb_dev_shutdown(c->handle);
err_dev:
	free(c);
err_mem:
	return NULL;
}

int au2_ericsson_destroy(void *connection)
{
	struct connection *c = (struct connection*) connection;

	log_info("%s:Terminating Primary Station for AU2 unit %u",
	       c->au2_name, c->addr);

	/* Let the receive thread exit and join. */
	c->exit = 1;
	pthread_join(c->thread, NULL);

	ecb_dev_shutdown(c->handle);
	free(c);

	return 0;
}

void au2_ericsson_get_link_info(char** buf, int* buf_left, void* connection,
                                uint16_t pa, uint16_t la, const char* state,
                                int first, int reset, int detailed)
{
	struct connection *c = (struct connection*) connection;

	if (first) {
		strcpy(*buf,
		       "=============================="
		       "==============================\n"
		       "AU2_ERIC: \npa  la  st                    "
		       "mode name \n");
		*buf_left -= strlen(*buf);
		*buf += strlen(*buf);

	}

	snprintf(*buf,
	         *buf_left,
	         "%-3u %-3u %-21s %-4s %s \n",
	         pa, la, state,
	         c->mode == MODE_NRM ? "NRM" : "NDM",
	         c->au2_name);

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

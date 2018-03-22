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
#include <ulh_cm.h>
#include <ulh_timer.h>
#include <ulh_transport.h>

#include <ecb_dev.h>
#include <ecb_unc.h>
#include <ecb_mux.h>

#include "atfi.sig"
#include "atfi_cm.h"
#include "atfi_hdlc.h"

#include "log.h"


/*
*******************************************************************************
**  MACROS
*******************************************************************************
*/

/* Fragment Header bit definitions. */
#define FRAG_SF_SIZE              14
#define FRAG_IL_SIZE               2
#define FRAG_S_HEADER         0xc000
#define FRAG_F_HEADER         0x8000
#define FRAG_I_HEADER         0x0000
#define FRAG_L_HEADER         0x4000

/* Size of information in HDLC Frame Reject */
#define HDLC_FRMR_SIZE             3

/* The command time-out. */
#define HDLC_CMD_TMO            1000

/* Maximum information field size. */
#define HDLC_MAX_INFO_SIZE        78

/* Maximum frame size (with good margin) */
#define HDLC_MAX_FRAME_SIZE      256

/*
*******************************************************************************
**  TYPES
*******************************************************************************
*/

#pragma pack(1)
union info {
	uint16_t                 header;
	struct {
		uint16_t         header;
		uint32_t         src;
		uint32_t         dst;
		uint32_t         size;
		uint8_t          data[1];
	}                        first, solitary;
	struct {
		uint16_t         header;
		uint8_t          data[1];
	}                        intermediate, last;
	uint8_t                  frmr[HDLC_FRMR_SIZE];
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
	}                             mode;

	int                           accept_nrm;

	uint8_t                       addr;
	uint8_t                       va;
	uint8_t                       vs;
	uint8_t                       vr;

	struct {
		struct fragment      *front;
		struct fragment      *back;
		struct fragment      *current;
	}                             tx;

	struct {
		struct ulh_cm_msghdr  hdr;
		uint32_t              index;
		void                 *data;
	}                             rx;

	void                         *lo;
	struct ulh_cm_uc_ops          uc;

	void                         *handle;
	struct ecb_unc_stat           stat;

	struct ulh_timer              timer;
	struct ulh_timerqueue        *tqueue;

	pthread_t                     thread;
	int                           exit;
	unsigned long                 uref;
	itc_mbox_id_t                 mbox;

	char                          srv_name[ATFID_SRV_NAME_MAX_SIZE];
};

union itc_msg {
	uint32_t                 msgno;
	struct ulh_transmsg_data data;
};

static struct connection *conn[ATFI_MAX_NUMBER_OF_CONNECTIONS];


/*
*******************************************************************************
**  FUNCTIONS
*******************************************************************************
*/

static int unc_wait(struct connection *c,
                    uint8_t *cmd_ctrl, void *cmd_info, uint32_t *cmd_info_size,
                    struct timespec *rsp_tmo)
{
	int result;

	result = ecb_unc_wait(c->handle, &c->stat, c->addr,
	                      cmd_ctrl, cmd_info, cmd_info_size, rsp_tmo);
	if (result == 1) {
		log_hdlc(c->addr, cmd_ctrl, cmd_info, cmd_info_size,
		         "RX(%s)", c->srv_name);
	}

	return result;
}

static int unc_send(struct connection *c,
                    uint8_t rsp_ctrl, void *rsp_info, uint32_t rsp_info_size)
{
	log_hdlc(c->addr, &rsp_ctrl, rsp_info, &rsp_info_size,
	         "TX(%s)", c->srv_name);

	return ecb_unc_send(c->handle, &c->stat, c->addr,
	                    rsp_ctrl, rsp_info, rsp_info_size);
}

/* Transport layer receiver thread. */
static void* recv_thread(void *arg)
{
	struct timespec recv_tmo = {
		.tv_sec = 0,
		.tv_nsec = 100 * 1000000 /* 100 ms */
	};
	uint8_t           *cmd = 0;
	struct connection *c;
	int                res;
	const char         postfix[] = "_recv";
	char               mbox_name[ATFID_SRV_NAME_MAX_SIZE + sizeof(postfix)];
	itc_mbox_id_t      mbox;
	union itc_msg     *msg = NULL;

	c = (struct connection*) arg;

	/* Create a mailbox. */
	snprintf(mbox_name, sizeof(mbox_name), "%s%s", c->srv_name, postfix);
	mbox = itc_create_mailbox(mbox_name, 0);
	if (mbox == ITC_NO_ID) {
		log_err("Failed to create mailbox, sleeping forever");
		for (;;) sleep(3600);
	}

	/* Flush any data received up till now. */
	ecb_dev_flush(c->handle);

	/* The secondary station never shares the device file,
	   but it also needs to setup the HW. */
	(void) ecb_dev_begin(c->handle);
	(void) ecb_dev_end(c->handle);

	for (;;) {

		/* Allocate memory for command frame. */
		if (msg == NULL) {
			msg = itc_alloc(sizeof(struct ulh_transmsg_data),
			                ULH_TRANSMSG_DATA);
			res = 0;
			while (ulh_tbuff_alloc_def(&msg->data.data,
			                           HDLC_MAX_FRAME_SIZE) != 0) {
				if (c->exit) {
					goto recv_thread_done;
				} else if (res == 0) {
					log_info("Failed to allocate buffer");
				}
				res++;
				sleep(1);
			}
			cmd = ulh_tbuff_get(&msg->data.data);
		}

		/* Wait for command frame. */
		res = unc_wait(c, &cmd[1], &cmd[2],
		               &msg->data.data.size, &recv_tmo);

		if (c->exit) { /* Time to exit. */

			ulh_tbuff_free(&msg->data.data);
			break;

		} else if (res == 1) { /* Send command frame to ULH. */

			cmd[0] = c->addr;
			msg->data.cid = 0;
			msg->data.uref = c->uref;
			itc_send(&msg, c->mbox, ITC_MY_MBOX);

		} else { /* No frame was received, make sure MRU is correct. */

			msg->data.data.size = HDLC_MAX_FRAME_SIZE;

		}

	}

recv_thread_done:
	itc_free(&msg);
	itc_delete_mailbox(mbox);

	return NULL;
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


static void nrm(struct connection *c)
{
	if (c->mode == MODE_NRM) {
		return;
	}

	log_info("%s:DU unit %u was connected", c->srv_name, c->addr);

	c->mode = MODE_NRM;
	c->va = c->vs = c->vr = 0;
	tx_flush(c);

	c->uc.uc_connected(c->lo);
}

static void ndm(struct connection *c, char *txt)
{
	if (c->mode == MODE_NDM) {
		return;
	}

	log_info("%s:DU unit %u was disconnected due to %s",
	       c->srv_name, c->addr, txt);

	c->mode = MODE_NDM;
	tx_flush(c);

	if (c->rx.data) {
		itc_free((union itc_msg**) &c->rx.data);
	}

	ulh_timer_cancel(&c->timer);

	c->uc.uc_disconnected(c->lo);
}

static int handle_recv_seq_number(struct connection *c, uint8_t cmd_ctrl)
{
	uint8_t nr = HDLC_NR(cmd_ctrl);

	/* Validate N(R). */
	if (c->va <= c->vs) {
		if (nr < c->va || nr > c->vs) {
			return 1;
		}
	} else {
		if (nr < c->va && nr > c->vs) {
			return 1;
		}
	}

	/* Update V(A) and discard acknowledged fragment. */
	if (c->va != nr) {
		tx_free(c->tx.current);
		c->tx.current = 0;
		c->va = (c->va + 1) & HDLC_WINDOW_MASK;
	}

	if (c->va != c->vs) { /* Information retransmission. */
		unc_send(c, HDLC_I | HDLC_PF | HDLC_VS_VR(c->va, c->vr),
		         &c->tx.current->info, c->tx.current->size);
	} else if (tx_dequeue(c)) { /* Information transmission. */
		unc_send(c, HDLC_I | HDLC_PF | HDLC_VS_VR(c->vs, c->vr),
		         &c->tx.current->info, c->tx.current->size);
		c->vs = (c->vs + 1) & HDLC_WINDOW_MASK;
	} else { /* Supervision transmission. */
		unc_send(c, HDLC_RR | HDLC_PF | HDLC_VR(c->vr), 0, 0);
	}

	return 0;
}

static void reject_frame(struct connection *c, uint8_t ctrl)
{
	union info rsp_info = {
		.frmr = { ctrl, (c->vr << 5) | (c->vs << 1), 0 }
	};

	unc_send(c, HDLC_FRMR | HDLC_PF, &rsp_info, HDLC_FRMR_SIZE);
}

static int deliver(struct connection *c,
                   union info *info, const uint32_t i_size)
{
	uint32_t  f_size;
	void     *f_data;

	if (i_size < MIN(FRAG_SF_SIZE, FRAG_IL_SIZE)) {
		log_err("Too small fragment");
		return 0;
	}

	if (info->header & htons(FRAG_F_HEADER)) {
		if (c->rx.data) {
			log_err("Unexpected %c fragment",
			       info->header & htons(FRAG_L_HEADER) ? 'S' : 'F');
			return 0;
		} else if (i_size <= FRAG_SF_SIZE) {
			log_err("Illegal size (%u <= %u)",
			       i_size, FRAG_SF_SIZE);
			return 0;
		}
		memcpy(&c->rx.hdr.src, &info->first.src, sizeof(uint32_t));
		c->rx.hdr.src = ntohl(c->rx.hdr.src);
		memcpy(&c->rx.hdr.dst, &info->first.dst, sizeof(uint32_t));
		c->rx.hdr.dst = ntohl(c->rx.hdr.dst);
		memcpy(&c->rx.hdr.size, &info->first.size, sizeof(uint32_t));
		c->rx.hdr.size = ntohl(c->rx.hdr.size);
		c->rx.index = 0;
		c->rx.data = itc_alloc(c->rx.hdr.size, 0);
		f_data = info->first.data;
		f_size = i_size - FRAG_SF_SIZE;
	} else {
		if (!c->rx.data) {
			log_err("Unexpected %c fragment",
			       info->header & htons(FRAG_L_HEADER) ? 'L' : 'I');
			return 0;
		} else if (i_size <= FRAG_IL_SIZE) {
			log_err("Illegal size (%u <= %u)",
			       i_size, FRAG_IL_SIZE);
			return 0;
		}
		f_data = info->last.data;
		f_size = i_size - FRAG_IL_SIZE;
	}

	if (c->rx.index + f_size > c->rx.hdr.size) {
		log_err("Size mismatch (%u > %u)",
		       c->rx.index + f_size, c->rx.hdr.size);
		return 0;
	} else {
		memcpy(((uint8_t*) c->rx.data) + c->rx.index, f_data, f_size);
		c->rx.index += f_size;
	}

	if (info->header & htons(FRAG_L_HEADER)) {
		if (c->rx.index != c->rx.hdr.size) {
			log_err("Size mismatch (%u != %u)",
			       c->rx.index, c->rx.hdr.size);
			return 0;
		} else {
			c->uc.uc_delivery(c->lo, &c->rx.hdr, c->rx.data);
			c->rx.data = 0;
		}
	}

	return 1;
}

static void command_tmo_task(void *co)
{
	ndm((struct connection*) co, "command time-out");
}


static int dc_init(void *co,
                   struct ulh_cm_uc_ops *uc,
                   void *lo,
                   __attribute__((unused)) uint32_t prio)
{
	struct connection *c = (struct connection*) co;

	c->lo = lo;
	c->uc = *uc;

	return 0;
}

static int dc_finalize(__attribute__((unused)) void *co,
                       __attribute__((unused)) uint32_t prio)
{
	return 0;
}

static int dc_connect(void *co, __attribute__((unused)) uint32_t prio)
{
	((struct connection*) co)->accept_nrm = 1;

	return 0;
}

static int dc_disconnect(void *co, __attribute__((unused)) uint32_t prio)
{
	struct connection *c = (struct connection*) co;

	c->accept_nrm = 0;
	ndm(c, "requested disconnect");

	return 0;
}

static int dc_transmit(void *co,
                       __attribute__((unused)) uint32_t prio,
                       struct ulh_cm_msghdr *hdr, union itc_msg *msg)
{
	struct connection *c = (struct connection*) co;
	struct fragment   *front, *back;
	uint8_t           *from;
	uint32_t           remaining, size, u32;

	remaining = hdr->size;
	from = (uint8_t*) msg;

	/* Build the front fragment. */
	size = MIN(remaining, HDLC_MAX_INFO_SIZE - FRAG_SF_SIZE);
	front = back = tx_alloc();
	if (!front) {
		return -1;
	}
	front->size = FRAG_SF_SIZE + size;
	front->info.header = htons(FRAG_F_HEADER);
	u32 = htonl(hdr->src);
	memcpy(&front->info.first.src, &u32, sizeof(uint32_t));
	u32 = htonl(hdr->dst);
	memcpy(&front->info.first.dst, &u32, sizeof(uint32_t));
	u32 = htonl(hdr->size);
	memcpy(&front->info.first.size, &u32, sizeof(uint32_t));
	memcpy(front->info.first.data, from, size);
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
		back->info.header = htons(FRAG_I_HEADER);
		memcpy(back->info.last.data, from, size);
		remaining -= size;
	}

	/* Set fragment last bit in the back fragment. */
	back->info.header |= htons(FRAG_L_HEADER);

	/* Enqueue the fragments front to back. */
	tx_enqueue(c, front, back);

	return 0;
}

static void dc_receive(void *co,
                       __attribute__((unused)) uint32_t cid,
                       struct ulh_tbuff *tbuf)
{
	struct connection *c = (struct connection*) co;
	uint8_t            cmd_ctrl = tbuf->data[1];
	union info        *cmd_info = (union info*) &tbuf->data[2];
	uint32_t           cmd_info_size = tbuf->size;

	if (c->mode == MODE_NDM) {

		if (cmd_ctrl == (HDLC_SNRM | HDLC_PF)) {
			if (c->accept_nrm) {
				unc_send(c, HDLC_UA | HDLC_PF, 0, 0);
				nrm(c);
			}
		} else if (cmd_ctrl == (HDLC_DISC | HDLC_PF) ||
		           HDLC_I_FORMAT(cmd_ctrl) ||
		           (HDLC_S_FORMAT(cmd_ctrl) &&
		            HDLC_S_FUNCTION(cmd_ctrl) == HDLC_RR)) {
			unc_send(c, HDLC_DM | HDLC_PF, 0, 0);
		}

	} else if (HDLC_S_FORMAT(cmd_ctrl)) {

		if (HDLC_S_FUNCTION(cmd_ctrl) == HDLC_RR) {
			if (handle_recv_seq_number(c, cmd_ctrl) != 0) {
				reject_frame(c, cmd_ctrl);
				ndm(c, "invalid Nr");
			}
		} else {
			reject_frame(c, cmd_ctrl);
			ndm(c, "unknown S format");
		}

	} else if (HDLC_I_FORMAT(cmd_ctrl)) {

		/* If N(S) equals V(R) then deliver the INFO field. */
		if (HDLC_NS(cmd_ctrl) == c->vr) {
			c->vr = (c->vr + 1) & HDLC_WINDOW_MASK;
			if (!deliver(c, cmd_info, cmd_info_size)) {
				ndm(c, "invalid info in command");
			} else if (handle_recv_seq_number(c, cmd_ctrl) != 0) {
				reject_frame(c, cmd_ctrl);
				ndm(c, "invalid Nr");
			}
		} else if (handle_recv_seq_number(c, cmd_ctrl) != 0) {
			reject_frame(c, cmd_ctrl);
			ndm(c, "invalid Nr");
		}

	} else if (cmd_ctrl == (HDLC_DISC | HDLC_PF)) {

		unc_send(c, HDLC_UA | HDLC_PF, 0, 0);
		ndm(c, "DISC command");

	} else if (cmd_ctrl == (HDLC_SNRM | HDLC_PF)) {

		unc_send(c, HDLC_DM | HDLC_PF, 0, 0);
		ndm(c, "SNRM command in NRM");

	}

	ulh_tbuff_free(tbuf);

	/* If we are in Normal Response Mode, restart the command time-out. */
	if (c->mode == MODE_NRM) {
		ulh_timer_arm(&c->timer, c->tqueue, HDLC_CMD_TMO);
	}
}

int linx_ss_get_info(char** buf, int* buf_left, uint16_t pa, uint16_t la,
                    const char* state, int first, int reset, int detailed)
{
	struct connection *c = NULL;

	for (int i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (conn[i]) {
			if (conn[i]->addr == pa) {
				c = conn[i];
				break;
			}
		}
	}

	if (!c)
		return -1;

	if (first) {
		strcpy(*buf,
		       "=============================="
		       "==============================\n"
		       "AU1: \npa  la  st                    mode server station type\n");
		*buf_left -= strlen(*buf);
		*buf += strlen(*buf);
	}

	snprintf(*buf,
	         *buf_left,
	         "%-3u %-3u %-21s %-4s %-6s %-7s %s \n",
	         c->addr,
	         la,
	         state,
	         c->mode == MODE_NRM ? "NRM" : "NDM",
	         c->srv_name,
	         "S/UNC",
	         "DU_PRIMARY");

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

	return 0;
}

static int ecb_create_instance(__attribute__((unused)) void *priv,
                               __attribute__((unused)) const char *name,
                               struct ulh_cm_instance *inst,
                               struct ulh_cm_config *config,
                               struct ulh_timerqueue *tqueue)
{
	static struct ulh_cm_dc_ops ecb_cm_ops = {
		.dc_init       = dc_init,
		.dc_fini       = dc_finalize,
		.dc_connect    = dc_connect,
		.dc_disconnect = dc_disconnect,
		.dc_transmit   = dc_transmit,
		.dc_receive    = dc_receive,
	};
	struct ecb_cm_config  cfg;
	struct connection    *c;

	memcpy(&cfg, config, sizeof(struct ecb_cm_config));

	log_info("%s:Starting Secondary Station on %s for DU unit %u",
	       cfg.srv_name, cfg.device, cfg.addr);

	c = calloc(1, sizeof(struct connection));
	if (!c) {
		log_err("Failed to allocate memory");
		goto err_mem;
	}

	inst->instance = c;
	inst->ops = &ecb_cm_ops;

	c->addr = cfg.addr;
	strcpy(c->srv_name, cfg.srv_name);
	c->tqueue = tqueue;

	ulh_timer_init(&c->timer, command_tmo_task, c);

	if (ecb_dev_init(&c->handle, cfg.device) != 0) {
		log_err("Failed to initiate device");
		goto err_dev;
	}

	if (cfg.lib_mux[0] && cfg.profile_name[0]) {
		if (ecb_mux_init(c->handle, cfg.lib_mux) != 0) {
			log_err("Failed to initiate mux for linx ss");
			goto err_dev;
		}

		if (ecb_mux_prog(c->handle, cfg.profile_name, cfg.channel)
		                != 0) {
			log_err("Failed to retrieve mux program for linx ss");
			goto err_dev;
		}
	}

	c->uref = config->uref;
	c->mbox = config->mbox;

	/* Create the transport receive thread. */
	if (pthread_create(&c->thread, NULL, recv_thread, c) != 0) {
		log_err("Failed to create transport receive thread");
		goto err_create;
	}

	for (int i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (!conn[i]) {
			conn[i] = c;
			break;
		}
	}

	return 0;

err_create:
	ecb_dev_shutdown(c->handle);
err_dev:
	free(c);
err_mem:
	return -1;
}

static int ecb_destroy_instance(__attribute__((unused)) void *priv,
                                struct ulh_cm_instance *inst)
{
	struct connection *c = (struct connection*) inst->instance;

	log_info("%s:Terminating Secondary Station for DU unit %u",
	       c->srv_name, c->addr);

	/* Let the receive thread exit and join. */
	c->exit = 1;
	pthread_join(c->thread, NULL);

	ecb_dev_shutdown(c->handle);

	for (int i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (conn[i] == c) {
			conn[i] = 0;
			break;
		}
	}

	free(c);

	return 0;
}

int atfid_init_cm_linx_ss(const char *name)
{
	static struct ulh_cm_ops ecb_ops = {
		.create_instance  = ecb_create_instance,
		.destroy_instance = ecb_destroy_instance,
	};

	return ulh_cm_register(name, &ecb_ops, 0);
}

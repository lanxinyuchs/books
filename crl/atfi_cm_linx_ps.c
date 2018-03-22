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

#include <itc.h>
#include <ulh_cm.h>
#include <ulh_timer.h>

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

/* The Secondary Station is at least polled once within this interval. */
#define HDLC_ACTIVE_POLL_TMO       0 /* Be active during user data transfer. */
#define HDLC_POLL_TMO            100 /* Be less active otherwise. */

/* The SNRM command is sent with this interval. */
#define HDLC_SNRM_TMO           1000

/* The Secondary Station should be able to respond within this time. */
#define HDLC_RESPONSE_TMO         70

/* Maximum number of retransmissions. */
#define HDLC_MAX_RETRANSMISSIONS   2

/* Maximum information field size. */
#define HDLC_MAX_INFO_SIZE        78


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
	}                             mode;

	uint8_t                       addr;
	uint8_t                       va;
	uint8_t                       vs;
	uint8_t                       vr;
	uint32_t                      retransmission;
	int                           active;

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

	char                          srv_name[ATFID_SRV_NAME_MAX_SIZE];
};

union itc_msg {
	uint32_t                 msgno;
};


static struct connection *conn[ATFI_MAX_NUMBER_OF_CONNECTIONS];

/*
*******************************************************************************
**  VARIABLES
*******************************************************************************
*/

static struct timespec rsp_tmo = {
	.tv_sec = 0,
	.tv_nsec = HDLC_RESPONSE_TMO * 1000000
};


/*
*******************************************************************************
**  FUNCTIONS
*******************************************************************************
*/

static int unc_cmd_rsp(struct connection *c,
                       uint8_t cmd_ctrl, void *cmd_info, uint32_t cmd_info_size,
                       uint8_t *rsp_ctrl, void *rsp_info, uint32_t *rsp_info_size,
                       struct timespec *rsp_tmo)
{
	uint8_t rsp_ctrl0;
	int     result;

	log_hdlc(c->addr, &cmd_ctrl, cmd_info, &cmd_info_size,
	         "TX(%s)", c->srv_name);

	result = ecb_unc_cmd_rsp(c->handle, &c->stat, c->addr,
	                         cmd_ctrl, cmd_info, cmd_info_size,
	                         rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
	                         rsp_info, rsp_info_size, rsp_tmo);
	if (result == 1) {
		log_hdlc(c->addr,
		         rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
		         rsp_info, rsp_info_size,
		         "RX(%s)", c->srv_name);
	}

	return result;
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
	c->active = 0;
	c->va = c->vs = c->vr = 0;
	c->retransmission = 0;
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
	c->active = 0;
	tx_flush(c);

	if (c->rx.data) {
		itc_free((union itc_msg**) &c->rx.data);
	}

	c->uc.uc_disconnected(c->lo);
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

static void poll_nrm(struct connection *c)
{
	uint8_t     cmd_ctrl, rsp_ctrl;
	union info *cmd_info, rsp_info;
	uint32_t    cmd_info_size, rsp_info_size;
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
	} else { /* Supervision transmission. */
		cmd_ctrl = HDLC_RR | HDLC_PF | HDLC_VR(c->vr);
		cmd_info = 0;
		cmd_info_size = 0;
		active = 0;
	}

	rsp_info_size = HDLC_MAX_INFO_SIZE;
	res = unc_cmd_rsp(c,
	                  cmd_ctrl, cmd_info, cmd_info_size,
	                  &rsp_ctrl, &rsp_info, &rsp_info_size,
	                  &rsp_tmo);

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
		} else if (rsp_ctrl == (HDLC_FRMR | HDLC_PF)) {
			log_info("FRMR 0x%02x 0x%02x 0x%02x",
			         rsp_info.frmr[0],
			         rsp_info.frmr[1],
			         rsp_info.frmr[2]);
			ndm(c, "FRMR response");
		} else {
			log_info("Illegal control field 0x%02x", rsp_ctrl);
			ndm(c, "unexpected response");
		}
	} else { /* Time-out or error. */
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

	res = unc_cmd_rsp(c,
	                  HDLC_SNRM | HDLC_PF, 0, 0,
	                  &rsp_ctrl, 0, 0, &rsp_tmo);
	if (res == 1 && rsp_ctrl == (HDLC_UA | HDLC_PF)) {
		nrm(c);
	}
}

static void poll_disc(struct connection *c)
{
	if (c->mode == MODE_NRM) {
		(void) unc_cmd_rsp(c,
		                   HDLC_DISC | HDLC_PF, 0, 0,
		                   0, 0, 0, &rsp_tmo);
	}

	ndm(c, "requested disconnect");
}

static void poll_task(void *co)
{
	struct connection *c = (struct connection*) co;

	/* Poll the secondary station. */
	if (c->mode == MODE_NRM) {
		poll_nrm(c);
	} else {
		poll_snrm(c);
	}

	if (c->active) {
		ulh_timer_arm(&c->timer, c->tqueue,
		              HDLC_ACTIVE_POLL_TMO);
	} else if (c->mode == MODE_NRM) {
		ulh_timer_arm(&c->timer, c->tqueue,
		              HDLC_POLL_TMO);
	} else {
		ulh_timer_arm(&c->timer, c->tqueue,
		              HDLC_SNRM_TMO);
	}
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
	struct connection *c = (struct connection*) co;

	/* Start poll task. */
	ulh_timer_cancel(&c->timer);
	poll_task(co);

	return 0;
}

static int dc_disconnect(void *co, __attribute__((unused)) uint32_t prio)
{
	struct connection *c = (struct connection*) co;

	/* Poll with DISC. */
	ulh_timer_cancel(&c->timer);
	poll_disc(c);

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

static void dc_receive(__attribute__((unused)) void *co,
                       __attribute__((unused)) uint32_t cid,
                       __attribute__((unused)) struct ulh_tbuff *tbuf)
{
}

int linx_ps_get_info(char** buf, int* buf_left, uint16_t pa, uint16_t la,
                    const char* state, int first, int reset, int detailed)
{
	struct connection    *c = NULL;

	for (int i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (conn[i]) {
			if(conn[i]->addr == pa) {
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
		       "AU1: \npa  la  st                    "
		                "mode server station type\n");
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
	         "P/UNC",
	         "DU_SECONDARY");

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

	log_info("%s:Starting Primary Station on %s for DU unit %u",
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
	ulh_timer_init(&c->timer, poll_task, c);

	if (ecb_dev_init(&c->handle, cfg.device) != 0) {
		log_err("Failed to initiate device");
		goto err_dev;
	}

	if (cfg.lib_mux[0] && cfg.profile_name[0]) {
		if (ecb_mux_init(c->handle, cfg.lib_mux) != 0) {
			log_err("Failed to initiate mux for linx ps");
			goto err_dev;
		}

		if (ecb_mux_prog(c->handle, cfg.profile_name, cfg.channel)
		                != 0) {
			log_err("Failed to retrieve mux program for linx ps");
			goto err_dev;
		}
	}

	for (int i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (!conn[i]) {
			conn[i] = c;
			break;
		}
	}
	return 0;

err_dev:
	free(c);
err_mem:
	return -1;
}

static int ecb_destroy_instance(__attribute__((unused)) void *priv,
                                struct ulh_cm_instance *inst)
{
	struct connection *c = (struct connection*) inst->instance;

	log_info("%s:Terminating Primary Station for DU unit %u",
	         c->srv_name, c->addr);

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

int atfid_init_cm_linx_ps(const char *name)
{
	static struct ulh_cm_ops ecb_ops = {
		.create_instance  = ecb_create_instance,
		.destroy_instance = ecb_destroy_instance,
	};

	return ulh_cm_register(name, &ecb_ops, 0);
}

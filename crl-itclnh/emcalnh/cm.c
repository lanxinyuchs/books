/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <arpa/inet.h>
#include <itc.h>

#include "cm.h"
#include "mhp3_proto.h"
#include "rlnh_proto.h"
#include "lh_trace.h"


#ifdef LTTNG
#include "lh_lttng.h"
#endif


#define STATE_DOWN							0
#define STATE_HELLO							1
#define STATE_UP							2

#define RESEND_TIMEOUT						10
#define RESEND_COUNT						50

#define BUG() \
	printf("[HDLC] %s[%s:%u]: BUG\n", __func__, __FILE__, __LINE__);


union itc_msg {
	uint32_t msg_no;
};




#ifdef LTTNG
static const char *type2str(uint8_t type)
{
	switch (type) {
	case MHP_CONN:	return "CONN";
	case MHP_DISC:	return "DISC";
	case MHP_ACK:	return "ACK";
	case MHP_NACK:	return "NACK";
	case MHP_DATA:	return "DATA";
	default:
		break;
	}
	return "????";
}

static void mhp_print_header(char *name, const char *text,
                             char *data, uint32_t length)
{
	struct mhp_header   *hdr;
	struct mhp_conn     *chdr;
	struct mhp_disc     *dhdr;
	struct mhp_data     *data_hdr;
	struct mhp_data_hdr ddata_hdr;

	hdr = (struct mhp_header*)data;

	MHP_TRACE(mhp_hdr, (char*)name, (char *)text, length,
             (char*)type2str(hdr->type), hdr->ns, hdr->nr);

	switch(hdr->type) {
	case MHP_CONN:
		chdr = (struct mhp_conn *)hdr;
		MHP_TRACE(mhp_connect, chdr->local_cid, chdr->remote_cid);
		break;

	case MHP_DISC:
		dhdr = (struct mhp_disc *)hdr;
		MHP_TRACE(mhp_disconnect, dhdr->local_cid, dhdr->remote_cid);
		break;

	case MHP_DATA:
		data_hdr  = (struct mhp_data *)hdr;
		memcpy(&ddata_hdr, data_hdr + 1, sizeof(struct mhp_data_hdr));
		MHP_TRACE(mhp_data, data_hdr->size, data_hdr->frag,
                          ddata_hdr.src, ddata_hdr.dst,
                          ddata_hdr.extra);
		break;
	default:
		break;
	}
}

void lnh_print_header(char *name, char *prfx,
                       struct mhp_msghdr *chdr, uint8_t *buf)
{
	struct rlnh_header            *rhdr;
	struct rlnh_msg_init          *rihdr;
	struct rlnh_msg_initreply     *rirhdr;
	struct rlnh_msg_publish       *rphdr;
	struct rlnh_msg_unpublish     *ruhdr;
	struct rlnh_msg_unpublish_ack *ruahdr;
	struct rlnh_msg_queryname     *rqhdr;

	rhdr = (struct rlnh_header *)buf;
	switch (ntohs(rhdr->type)) {
	case RLNH_INIT:
		rihdr = (struct rlnh_msg_init *)rhdr;
		MHP_TRACE(	rlnh_init, name, prfx, rhdr->reserved, rhdr->type,
                          rihdr->version);
		break;
	case RLNH_INIT_REPLY:
		rirhdr = (struct rlnh_msg_initreply *)rhdr;
		MHP_TRACE(rlnh_init_reply, name, prfx, rhdr->reserved, rhdr->type,
                          rirhdr->status, "");
		break;
	case RLNH_PUBLISH:
		rphdr = (struct rlnh_msg_publish *)rhdr;
		MHP_TRACE(rlnh_publish, name, prfx, rhdr->reserved, rhdr->type,
                          rphdr->laddr, rphdr->name);
		break;
	case RLNH_QUERY_NAME:
		rqhdr = (struct rlnh_msg_queryname *)rhdr;
		MHP_TRACE(rlnh_query, name, prfx, rhdr->reserved, rhdr->type,
                          rqhdr->laddr, rqhdr->name);
		break;
	case RLNH_UNPUBLISH:
		ruhdr = (struct rlnh_msg_unpublish *)rhdr;
		MHP_TRACE(rlnh_unpublish, name, prfx, rhdr->reserved, rhdr->type,
                          ruhdr->laddr);
		break;
	case RLNH_UNPUBLISH_ACK:
		ruahdr = (struct rlnh_msg_unpublish_ack *)rhdr;
		MHP_TRACE(rlnh_unpublish_ack, name, prfx, rhdr->reserved, rhdr->type,
                          ruahdr->laddr);
		break;
	default:
		MHP_TRACE(rlnh_data, name, prfx, chdr->src, chdr->dst, chdr->size,
                          *((uint32_t *)buf));
		break;
	}
}

void mhp_trace_error(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_ulh, mhp_error, file, line, buffer);
}

void mhp_trace_info(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[512];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_ulh, mhp_info, file, line, buffer);
}

void mhp_trace_dbg(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[512];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_ulh, mhp_dbg, file, line, buffer);
}
#endif

#if 0
static void
print_data(uint8_t *data, uint32_t size, uint32_t noelem)
{
	char ascii[128];
	uint32_t k,j;
	for (k = 0, j = 0; k < size; k++, j++) {
		if (j == noelem) {
			ascii[j] = 0;
			printf("  \"%s\"\n", ascii);
			j = 0;
		}
		printf("%.2x ",data[k]);
		if (isprint((int)data[k]))
			ascii[j] = (char)data[k];
		else
			ascii[j] = '.';
	}
	if (j) {
		ascii[j] = 0;
		for (;j < noelem; j++)
			printf("   ");

		printf("  \"%s\"\n", ascii);
	}
	printf("\n");
}
#endif

static inline uint64_t mhp_gettime(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000ULL + ts.tv_nsec / 1000000;
}

struct tx_buff *tx_alloc(int length)
{
	struct tx_buff *tb = malloc(sizeof(*tb));
	int32_t ret;

	if (tb) {
		ret = rhai_mhp_tx_alloc(g.mhp_handle, length, (void**)&tb->data);
		if (ret == 0) {
			tb->size = length;
			return tb;
		}
		free(tb);
		MHP_ERROR("rhai_mhp_tx_alloc,  %d", ret);
	}
	return NULL;
}

void tx_free(struct tx_buff *tb)
{
	if (tb) {
		int32_t ret = rhai_mhp_tx_free(g.mhp_handle, tb->data);
		if (ret) {
			MHP_ERROR("%s::rhai_mhp_tx_free, %d",
				__func__, ret);
		}
		free(tb);
	}
}

void tx_send(struct mhp_link *co, struct tx_buff *tb)
{
	int32_t ret = rhai_mhp_pkt_tx(g.mhp_handle, co->connid,
								tb->data, tb->size, 0);
	if (ret) {
		MHP_ERROR("%s: rhai_mhp_pkt_tx, %d",
				co->name, ret);
		return;
	}
	co->rx_outstanding = 0;
	co->keep_alive = 50;
}

void tx_queue(struct tx_queue *que, struct tx_buff *tb)
{
	if (que->tail == NULL)
		que->head = tb;
	else
		que->tail->next = tb;

	que->tail = tb;
	tb->next = NULL;
}

struct tx_buff *tx_dequeue(struct tx_queue *que)
{
	struct tx_buff *p = que->head;

	if (p) {
		que->head = p->next;
		if (que->head == NULL)
			que->tail = NULL;
	}
	return p;
}

int ecom_init(struct mhp_link *co, struct eolc_g2hw_config *cfg)
{
	struct rhai_mhp_config conf;
	uint32_t pcep = cfg->emca_pcep;
	int32_t ret;

	if ((pcep | (cfg->emca_id << 4)) > 0x1000000)
		return -EINVAL;

	conf.cref = co;
	conf.emca_id = cfg->emca_id;
	conf.emca_pcep = pcep;
	memcpy(conf.emca_mac, cfg->emca_mac, 6);

	MHP_INFO("EMCA_ID: %u", conf.emca_id);
	MHP_INFO("EMCA_PCEP: 0x%x", conf.emca_pcep);
	MHP_INFO("MAC: %02x:%02x:%02x:%02x:%02x:%02x",
		conf.emca_mac[0],conf.emca_mac[1],conf.emca_mac[2],
		conf.emca_mac[3],conf.emca_mac[4],conf.emca_mac[5]);

	ret = rhai_mhp_add_conn(g.mhp_handle, &conf,
					&co->connid, &co->mtu);
	if (ret) {
		MHP_ERROR("rhai_mhp_add_link,  %d", ret);
		return -EFAULT;
	}
	return 0;
}

static void mhp_tx_ctrl(struct mhp_link *co, uint8_t type)
{
	struct mhp_conn *conn;
	uint32_t size = sizeof(*conn);
	int32_t ret;

	ret = rhai_mhp_tx_alloc(g.mhp_handle, size, (void**)&conn);
	if (ret) {
		MHP_ERROR("rhai_mhp_tx_alloc,  %d", ret);
		return;
	}

	conn->cmn.feature = 0;
	conn->cmn.prio = 0;
	conn->cmn.type = type;
	conn->cmn.ns = htons(co->tx_seq);
	conn->cmn.nr = htons(co->rx_seq);

	if ((type == MHP_CONN) || (type == MHP_DISC)) {
		conn->local_cid = htons(co->local_cid);
		conn->remote_cid = htons(co->remote_cid);
	}

	MHP_HEADER(co->name, "MHP tx", conn, size);

	ret = rhai_mhp_pkt_tx(g.mhp_handle, co->connid, conn, size, 1);
	if (ret) {
		MHP_ERROR("%s: rhai_mhp_pkt_tx, %d", __func__, ret);

		//rhai_mhp_tx_free(g.mhp_handle, conn);
		return;
	}

	co->rx_outstanding = 0;
	co->keep_alive = 50;
}

static void mhp_reset_link(struct mhp_link *co)
{
	struct tx_buff *tb;

	co->rx_seq = 0;
	co->tx_seq += 10000;
	co->tx_outstanding = 0;
	co->rx_outstanding = 0;
	co->tx_ack = co->tx_seq - 1;
	co->conn_alive = 300;
	co->keep_alive = 50;

	for (tb = tx_dequeue(&co->txq); tb; tb = tx_dequeue(&co->txq))
		tx_free(tb);

	for (tb = tx_dequeue(&co->axq); tb; tb = tx_dequeue(&co->axq))
		tx_free(tb);

	if (co->rx_msg)
		itc_free(&co->rx_msg);

	co->remote_cid = 0;
	if (!++co->local_cid)
		co->local_cid = 1;
}

static void mhp_tx(struct mhp_link *co)
{
	uint64_t tnow = mhp_gettime();
	struct mhp_header *hdr;
	struct tx_buff *tb;

	for (;co->tx_outstanding < co->tx_wnd;) {
		if ((tb = tx_dequeue(&co->txq)) == NULL)
			break;

		hdr = (struct mhp_header *)tb->data;
		hdr->ns = htons(++co->tx_seq);
		hdr->nr = htons(co->rx_seq);

		tx_queue(&co->axq, tb);
		tx_send(co, tb);
		tb->time = tnow + RESEND_TIMEOUT;
		tb->rcnt = 0;
		co->tx_outstanding++;
	}
}

static void mhp_retransmit(struct mhp_link *co, int force)
{
	struct mhp_header *hdr;
	struct tx_buff *tb;
	uint64_t tnow;
	uint16_t nrtx = 0;

	for (tnow = mhp_gettime(), tb = co->axq.head; tb; tb = tb->next) {
		if (tnow >= tb->time) {
			hdr = (struct mhp_header *)tb->data;
			hdr->nr = htons(co->rx_seq);
			tb->time = tnow + RESEND_TIMEOUT;

			if (tb->rcnt++ > RESEND_COUNT) {
				MHP_INFO("*** DISCONNECT [%s] (num rtx = %d) ***",
											co->name, RESEND_COUNT);
				mhp_disc(co);
				return;
			}
			nrtx++;
			tx_send(co, tb);

		} else if (force) {
			nrtx++;
			tx_send(co, tb);
		} else
			break;
	}

	if (nrtx) {
		uint16_t ns_end = ntohs(hdr->ns);
		uint16_t ns_start = ns_end - (uint16_t)co->tx_outstanding + 1;
		(void)ns_start;

		if (!force) {
			MHP_DBG("%s - %s ns: [%u...%u]  nr: %u out: %d  ack: %d force: %d"
						,co->name, __func__, ns_start, ns_end, co->rx_seq,
						co->tx_outstanding, co->tx_ack, force);
		}
	}
}

static void mhp_process_ack(struct mhp_link *co, uint16_t nr, int line)
{
	struct mhp_header *hdr;
	struct tx_buff *tb;

	if (nr == co->tx_ack)
		return;

	if ((uint16_t)(nr - co->tx_ack) > co->tx_outstanding) {
		MHP_DBG("%s BAD N(R) %u received. (ack: %u  out: %u  line: %d",
				co->name, nr, co->tx_ack, co->tx_outstanding, line);
		return;
	}

	for (;co->tx_ack != nr;) {
		if ((tb = tx_dequeue(&co->axq))) {
			hdr = (struct mhp_header*)tb->data;
			co->tx_ack = ntohs(hdr->ns);
			co->tx_outstanding--;
			tx_free(tb);
		} else
			break;
	}
	mhp_tx(co);
}

static void mhp_rx_conn(struct mhp_link *co, void *tb)
{
	struct mhp_conn *hdr = tb;
	uint16_t lcid = ntohs(hdr->local_cid);
	uint16_t rcid = ntohs(hdr->remote_cid);

	if (!lcid) {
		MHP_ERROR("%s: CONN with bad lconnid:%u", co->name, lcid);
		return;
	}
	switch (co->cm_state) {
	case STATE_DOWN:
	case STATE_HELLO:
		if (!rcid) {
			co->remote_cid = lcid;
			co->cm_state = STATE_HELLO;
			co->rx_seq = ntohs(hdr->cmn.ns);
			mhp_tx_ctrl(co, MHP_CONN);
			break;
		}
		if (rcid != co->local_cid)
			break;

		co->remote_cid = lcid;
		co->rx_seq = ntohs(hdr->cmn.ns);
		co->cm_state = STATE_UP;
		uc_connected(co);
		break;
	case STATE_UP:
		break;
	default:BUG();
		break;
	}
}

static void mhp_rx_disc(struct mhp_link *co, void *tb)
{
	struct mhp_disc *hdr = tb;
	uint16_t lcid = ntohs(hdr->local_cid);
	uint16_t rcid = ntohs(hdr->remote_cid);

	if (!rcid) {
		MHP_ERROR("%s: DISC with bad rconnid:%u", co->name, rcid);
		return;
	}

	if ((lcid != co->remote_cid) || (rcid != co->local_cid))
		return;

	switch (co->cm_state) {
	case STATE_DOWN:
	case STATE_HELLO:
		co->cm_state = STATE_DOWN;
		mhp_reset_link(co);
		break;
	case STATE_UP:
		mhp_disc(co);
		break;
	default:BUG();
		break;
	}
}

static void mhp_rx_ack(struct mhp_link *co, void *tb)
{
	struct mhp_header *hdr = tb;
	uint16_t nr = ntohs(hdr->nr);

	switch (co->cm_state) {
	case STATE_DOWN:
		break;
	case STATE_HELLO:
		if (nr != co->tx_seq)
			break;
		co->cm_state = STATE_UP;
		uc_connected(co);
	case STATE_UP:
		mhp_process_ack(co, nr, __LINE__);
		break;
	default:BUG();
		break;
	}
}

static void mhp_rx_nack(struct mhp_link *co, void *tb)
{
	struct mhp_header *hdr = tb;
	uint16_t nr = ntohs(hdr->nr);

	MHP_INFO("(***) %s %s  ns: %u  nr: %u",co->name,
		__func__, ntohs(hdr->ns), nr);

	switch (co->cm_state) {
	case STATE_DOWN:
		break;
	case STATE_HELLO:
		if (nr != co->tx_seq)
			break;
		co->cm_state = STATE_UP;
		uc_connected(co);
	case STATE_UP:
		mhp_process_ack(co, nr, __LINE__);
		mhp_retransmit(co, 1);
		break;
	default:BUG();
		break;
	}
}

static void mhp_rx_data(struct mhp_link *co, void *tb, int length)
{
	uint16_t nr, ns, size, frag, frag_no;
	uint32_t rsize, frag_max_size;
	union itc_msg *msg = NULL;
	struct mhp_data *hdr = tb;
	struct mhp_data_hdr *dhdr = (struct mhp_data_hdr *)(hdr + 1);
	uint8_t *payload = (uint8_t*)(dhdr + 1);

	if (length <= (sizeof(*hdr) + sizeof(*dhdr))) {
		MHP_ERROR("Too short frame (%d)...discaring", length);
		return;
	}

	nr = ntohs(hdr->cmn.nr);
	ns = ntohs(hdr->cmn.ns);
	size = ntohs(hdr->size);
	frag = ntohs(hdr->frag);
	frag_no = frag & MHP_DATA_FRAGNO_MASK;

	if (co->cm_state == STATE_HELLO) {
		if (nr == co->tx_seq) {
			uint16_t rseq = co->rx_seq;
			co->cm_state = STATE_UP;
			co->rx_seq += 1;
			uc_connected(co);
			co->rx_seq = rseq;
		}
	}

	if ((uint16_t)(co->rx_seq + 1) != ns) {
		int16_t delta = (int16_t)(co->rx_seq - ns);
		if (delta >= 0) {
			MHP_DBG("MHP: %s: duplicate packet dropped    "
				" (exp:%u ns:%u nr:%u ack:%u)", co->name, co->rx_seq + 1,
						ns, nr, co->tx_ack);
			if (delta < 4)
				mhp_tx_ctrl(co, MHP_ACK);
		} else if (co->nack_sent++ == 0) {
			MHP_INFO("MHP: %s: out of order packet NACKED  "
				" (exp:%u ns:%u nr:%u nack:%u)", co->name, co->rx_seq + 1,
						ns, nr, co->nack_sent);
			mhp_tx_ctrl(co, MHP_NACK);
		} else {
			MHP_INFO("MHP: %s: out of order packet DROPPED "
				" (exp:%u ns:%u nr:%u count:%u)", co->name, co->rx_seq + 1,
						ns, nr, co->nack_sent);
		}
		return;
	}

	co->nack_sent = 0;

	if (frag_no == 0) {
		if (frag & MHP_DATA_F_FRAG) {
			MHP_ERROR("MHP: %s unfragmented DATA with FRAG set", co->name);
			mhp_disc(co);
			return;
		}
		if (co->rx_msg) {
			MHP_ERROR("MHP: %s unexpected unfragmented DATA", co->name);
			mhp_disc(co);
			return;
		}

		co->rx_head.size = size;
		co->rx_head.dst = ntohl(dhdr->dst);
		co->rx_head.src = ntohl(dhdr->src);

                if (co->rx_head.size > co->max_itc_msg_size)
                {
                   MHP_ERROR("MHP: %s too big message size %d, allowed %d, dst %d, src %d",
                             co->name,
                             co->rx_head.size,
                             co->max_itc_msg_size,
                             co->rx_head.dst,
                             co->rx_head.src);
                   mhp_disc(co);
                   return;
                }

		msg = itc_alloc(size, 0);
		memcpy(msg, payload, size);
	} else {
		frag_max_size = co->mtu - sizeof(struct mhp_data) -
							sizeof(struct mhp_data_hdr);

		if (frag & MHP_DATA_F_FRAG) {
			if (co->rx_msg) {
				MHP_ERROR("MHP: %s unexpected 1st fragment", co->name);
				mhp_disc(co);
				return;
			}
			/* size = first_size + (num_fragmens - 1) * frag_max_size */
			co->rx_head.size = size + (frag_no - 1) * frag_max_size;
			co->rx_head.dst = ntohl(dhdr->dst);
			co->rx_head.src = ntohl(dhdr->src);

                        if (co->rx_head.size > co->max_itc_msg_size)
                        {
                           MHP_ERROR("MHP: %s too big message size frag %d, allowed %d, dst %d, src %d",
                                     co->name,
                                     co->rx_head.size,
                                     co->max_itc_msg_size,
                                     co->rx_head.dst,
                                     co->rx_head.src);
                           mhp_disc(co);
                           return;
                        }

			co->rx_msg = itc_alloc(co->rx_head.size, 0);
			co->rx_pos = (uint8_t *)co->rx_msg;
			co->rx_frag = frag_no;

		} else if (!co->rx_msg) {
			MHP_ERROR("MHP: %s unexpected non-1st frag", co->name);
			mhp_disc(co);
			return;
		}

		if (frag_no != co->rx_frag) {
			MHP_ERROR("MHP: %s unexpected FragNo (exp:%u rcv:%u)",
								co->name, co->rx_frag, frag_no);
			mhp_disc(co);
			return;
		}

		memcpy(co->rx_pos, payload, size);
		co->rx_pos += size;

		if (--co->rx_frag == 0) {
			rsize = co->rx_pos - (uint8_t *)co->rx_msg;
			msg = co->rx_msg;

			if (rsize != co->rx_head.size) {
				if ((rsize + 1) != co->rx_head.size) {
					MHP_ERROR("MHP: %s unexpected reassembly size %d (exp: %d)",
							co->name, rsize, co->rx_head.size);
					mhp_disc(co);
					return;
				}
				itc_setsize(msg, rsize);
				co->rx_head.size = rsize;
			}
		}
	}

	co->rx_seq += 1;

	if (++co->rx_outstanding > 3)
		mhp_tx_ctrl(co, MHP_ACK);

	mhp_process_ack(co, nr, __LINE__);

	if (msg) {
		co->rx_msg = NULL;
		LNH_HEADER(co->name, "RX", &co->rx_head, msg);
		uc_deliver(co, &co->rx_head, msg);
	}
}

int mhp_send(struct mhp_link *co, struct mhp_msghdr *hdr,
					union itc_msg *msg)
{
	struct mhp_data_hdr *ddhdr;
	struct mhp_data *dhdr;
	struct tx_buff *tb;
	uint32_t rsize = hdr->size;
	uint32_t hsize = sizeof(*ddhdr) + sizeof(*dhdr);
	uint32_t frag_size;
	uint32_t frag_max_size = co->mtu - hsize;
	uint32_t frags, cnt = 0;
	uint8_t *pos = (uint8_t*)msg;

	if (co->cm_state != STATE_UP) {
		MHP_INFO("MHP: %s: discarding dc_transmit in state %d",
					co->name, co->cm_state);
		return -EINVAL;
	}

	LNH_HEADER(co->name, "TX", hdr, msg);

	frags = (rsize - 1) / frag_max_size + 1;
	if (frags > MHP_DATA_MAXFRAGS) {
		MHP_ERROR("MHP: %s: packet is too big", co->name);
		return -EINVAL;
	}

	while (rsize) {
		frag_size = rsize;
		if (frags > 1) {
			if ((cnt == 0) && (rsize % frag_max_size))
				frag_size = (rsize % frag_max_size) + (rsize & 1);
			else if (frag_size > frag_max_size)
				frag_size = frag_max_size;
		}

		tb = tx_alloc(frag_size + hsize);
		if (tb == NULL) {
			MHP_ERROR("%s: unable to allocate packet", co->name);
			return -ENOMEM;
		}

		dhdr = (struct mhp_data *)tb->data;
		ddhdr = (struct mhp_data_hdr *) (dhdr + 1);

		memset(dhdr, 0, hsize);
		dhdr->cmn.type = MHP_DATA;
		dhdr->size = htons(frag_size);

		/* fill frag word if fragmented message */
		if (frags > 1)
			dhdr->frag = htons((frags-cnt)|(MHP_DATA_F_FRAG & (cnt-1)));

		if (cnt == 0) {
			ddhdr->src   = htonl(hdr->src);
			ddhdr->dst   = htonl(hdr->dst);
			ddhdr->extra = htonl(hdr->size);
		}

		memcpy(ddhdr + 1, pos, frag_size);
		pos += frag_size;
		tx_queue(&co->txq, tb);
		rsize -= frag_size;
		cnt++;
	}

	mhp_tx(co);
	return 0;
}

void mhp_tick(struct mhp_link *co)
{
	if (co->cm_state == STATE_UP) {
		co->keep_alive--;
		mhp_retransmit(co, 0);

		if (co->rx_outstanding || (co->keep_alive == 0))
			mhp_tx_ctrl(co, MHP_ACK);

		if (!--co->conn_alive) {
			MHP_ERROR("Alive counter hits zero (%d)... DISCONNECTING\n", 0);
			mhp_disc(co);
		}
		return;
	}

	switch (co->cm_state) {
	case STATE_DOWN:
		mhp_tx_ctrl(co, MHP_CONN);
		break;
	case STATE_HELLO:
		break;
	default:BUG();
		break;
	}
}

void mhp_disc(struct mhp_link *co)
{
	uc_disconnected(co);
	mhp_tx_ctrl(co, MHP_DISC);
	mhp_reset_link(co);
	co->cm_state = STATE_DOWN;
}

void mhp_exit(struct mhp_link *mhp)
{
	mhp_reset_link(mhp);
}

int mhp_init(struct mhp_link *mhp)
{
	mhp->tx_wnd   = 8;
	mhp_reset_link(mhp);
	mhp->cm_state = STATE_DOWN;
	return 0;
}

void rx_packet(void *data, uint16_t length, void *cref)
{
	struct mhp_header *hdr = (struct mhp_header *)data;
	struct mhp_link *co = cref;
	int len = (int)length;

	co->conn_alive = 3000;

	MHP_HEADER(co->name, "MHP RX", hdr, len);

	switch (hdr->type) {
		case MHP_DATA:
				mhp_rx_data(co, hdr, len);
				break;
		case MHP_CONN:
				if (len >= sizeof(struct mhp_conn))
					mhp_rx_conn(co, hdr);
				break;
		case MHP_DISC:
				if (len >= sizeof(struct mhp_disc))
					mhp_rx_disc(co, hdr);
				break;
		case MHP_ACK:
				mhp_rx_ack(co, hdr);
				break;
		case MHP_NACK:
				mhp_rx_nack(co, hdr);
				break;
		default:
				MHP_INFO("MHP: %s: unknown packet type 0x%x",
					co->name, hdr->type);
				break;
	}
}

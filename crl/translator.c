/* -*- c -*- ******************************************************************
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

/******************************************************************************
 *
 * Product name:
 *      Link Handler Translator
 *
 * File:
 *      translator.c
 *
 * Author:
 *      Peter Marchall (QPETMAR)
 *
 * Description:
 *      Translator between LINX and GLH Link Handler protocols.
 *
 *      LINX protocol version 1 is described in LINX Protocols revision 11:
 *      http://
 *      linx.sourceforge.net/linxdoc-1.4.1/doc/linxprotocol/linxprotocol.pdf
 *
 *      The GLH Protocol (from OSE5.6 BL540110) had to be reverse engineered.
 *
 */

/*
 * TRANSLATED MESSAGES
 * ===================
 *
 * RLNH_INIT & RLNH_INIT_REPLY
 * ---------------------------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |----- A0) RLNH_INIT ------------>|                                 |
 *  |                                 |                                 |
 *  |<---- A1) RLNH_INIT -------------|                                 |
 *  |                                 |                                 |
 *  |----- A2) RLNH_INIT_REPLY ------>|                                 |
 *  |                                 |                                 |
 *  |<---- A3) RLNH_INIT_REPLY -------|                                 |
 *  |                                 |                                 |
 *
 *
 * RLNH_PUBLISH
 * ------------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |----- B0) RLNH_PUBLISH --------->|                                 |
 *  |                                 |                                 |
 *  |                                 |----- B1) GLH_PUBLISH ---------->|
 *  |                                 |                                 |
 *  |                                 |<---- B2) GLH_PUBLISH_ACK -------|
 *  |                                 |                                 |
 *
 *
 * RLNH_QUERY_NAME
 * ---------------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |----- C0) RLNH_QUERY_NAME ------>|                                 |
 *  |                                 |                                 |
 *  |                                 |----- C1) GLH_QUERY_NAME ------->|
 *  |                                 |                                 |
 *
 *
 * RLNH_UNPUBLISH
 * --------------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |----- D0) RLNH_UNPUBLISH ------->|                                 |
 *  |                                 |                                 |
 *  |                                 |----- D1) GLH_UNPUBLISH -------->|
 *  |                                 |                                 |
 *  |<---- D2) RLNH_UNPUBLISH_ACK ----|                                 |
 *  |                                 |                                 |
 *
 *
 * GLH_PUBLISH
 * -----------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |                                 |<---- E0) GLH_PUBLISH -----------|
 *  |                                 |                                 |
 *  |                                 |----- E1) GLH_PUBLISH_ACK ------>|
 *  |                                 |                                 |
 *  |<---- E2) RLNH_PUBLISH ----------|                                 |
 *  |                                 |                                 |
 *
 *
 * GLH_QUERY_NAME
 * --------------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |                                 |<---- F0) GLH_QUERY_NAME --------|
 *  |                                 |                                 |
 *  |<---- F1) RLNH_QUERY_NAME -------|                                 |
 *  |                                 |                                 |
 *
 *
 * GLH_UNPUBLISH (LOCAL)
 * ---------------------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |                                 |<---- G0) GLH_UNPUBLISH ---------|
 *  |                                 |                                 |
 *  |<---- G1) RLNH_UNPUBLISH --------|                                 |
 *  |                                 |                                 |
 *  |----- G2) RLNH_UNPUBLISH_ACK --->|                                 |
 *  |                                 |                                 |
 *
 *
 * GLH_UNPUBLISH (REMOTE)
 * ----------------------
 *
 * LINX                               T                                GLH
 *  |                                 |                                 |
 *  |                                 |<---- H0) GLH_UNPUBLISH ---------|
 *  |                                 |                                 |
 *  |                                 |----- H1) GLH_PUBLISH ---------->|
 *  |                                 |                                 |
 *
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <arpa/inet.h>

#include "translator.h"


/* Macros for using ramlog, syslog, lttng or nothing. */

#if defined(T_DBG_RAMLOG)

#include <ramlog.h>
#define _DBG_PRINTF(...)                        \
	do {                                    \
		ramlog_printf(__VA_ARGS__);     \
	} while (0)

#elif defined(T_DBG_SYSLOG)

#include <syslog.h>
#define _DBG_PRINTF(...)                        \
	do {                                    \
		syslog(LOG_DEBUG, __VA_ARGS__);          \
	} while (0)

#elif defined(T_DBG_LTTNG)
#include <log.h>
#define _DBG_PRINTF(...)                        \
	do {                                    \
		log_trace2(__VA_ARGS__);          \
	} while (0)

#endif


/* Definitions of LINX Protocol related constants and messages. */

#define RLNH_VERSION            1

#define RLNH_NO_ADDR            0

#define RLNH_TYPE_QUERY_NAME    1
#define RLNH_TYPE_PUBLISH       2
#define RLNH_TYPE_UNPUBLISH     3
#define RLNH_TYPE_UNPUBLISH_ACK 4
#define RLNH_TYPE_INIT          5
#define RLNH_TYPE_INIT_REPLY    6

#pragma pack(1)

/*
 * Note: The reserved field has been included in the type field. This simplifies
 *       the code and works since the reserved field always contains zero.
 */

struct rlnh_init {
	uint32_t type;
	uint32_t version;
};

struct rlnh_init_reply {
	uint32_t type;
	uint32_t status;
};

struct rlnh_publish {
	uint32_t type;
	uint32_t addr;
	char     name[1];
};

struct rlnh_query_name {
	uint32_t type;
	uint32_t addr;
	char     name[1];
};

struct rlnh_unpublish {
	uint32_t type;
	uint32_t addr;
};

struct rlnh_unpublish_ack {
	uint32_t type;
	uint32_t addr;
};

union rlnh_message {
	uint32_t                  type;
	struct rlnh_init          init;
	struct rlnh_init_reply    init_reply;
	struct rlnh_publish       publish;
	struct rlnh_query_name    query_name;
	struct rlnh_unpublish     unpublish;
	struct rlnh_unpublish_ack unpublish_ack;
};

#pragma pack()


/* Definitions of GLH Protocol related constants and messages. */

#define GLH_NO_PID           0

#define GLH_TYPE_PUBLISH     0
#define GLH_TYPE_PUBLISH_ACK 1
#define GLH_TYPE_QUERY_NAME  2
#define GLH_TYPE_UNPUBLISH   3
#define GLH_TYPE_USER_MSG    4

#pragma pack(1)

/*
 * Note: It seems GLH fills the zero field with varying nonsense. The
 *       translator sets the zero field to 0 when transmitting GLH messages
 *       and ignores the content of the zero field in received GLH messages.
 */

struct glh_publish {
	uint16_t type;    /* GLH_TYPE_PUBLISH */
	uint16_t zero;
	uint32_t opid;    /* Original PID which is being published */
	char     name[1]; /* Name published */
};

struct glh_publish_ack {
	uint16_t type;    /* GLH_TYPE_PUBLISH_ACK */
	uint16_t zero;
	uint32_t opid;    /* Original PID */
	uint32_t cpid;    /* Clone PID */
};

struct glh_query_name {
	uint16_t type;    /* GLH_TYPE_QUERY_NAME */
	uint16_t zero;
	uint32_t user;    /* OSE User Number */
	uint32_t hpid;    /* Hunter PID on remote side. */
	char     name[1]; /* Name queried for */
};

struct glh_unpublish {
	uint16_t type;    /* GLH_TYPE_UNPUBLISH */
	uint16_t zero;
	uint32_t pid;     /* PID being unpublished */
	uint8_t  remote;  /* 0 for LEP otherwise REP */
};

struct glh_user_msg {
	uint16_t type;    /* GLH_TYPE_USER_MSG */
	uint16_t zero;
	uint32_t dpid;    /* Destination PID on remote side */
	uint32_t spid;    /* Source PID on remote side */
	uint8_t  data[1]; /* User data */
};

union glh_message {
	uint16_t               type;
	struct glh_publish     publish;
	struct glh_publish_ack publish_ack;
	struct glh_query_name  query_name;
	struct glh_unpublish   unpublish;
	struct glh_user_msg    user_msg;
};

#pragma pack()


/* Definition of internal data structures. */

struct endpoint {

	/* Process identifier. */
	uint32_t           pid;

        /* Name of the endpoint. */
	char              *name;
};

struct queued_msg {

	/* Queue pointers. */
	struct queued_msg *next;
	struct queued_msg *prev;

	/* Address to match with the Original PID in the GLH_PUBLISH_ACK. */
	uint32_t           addr;

	/* Type of RLNH message. */
	enum { QM_TYPE_UNPUBLISH, QM_TYPE_QUERY_NAME, QM_TYPE_USER_MSG } type;

	/* RLNH message. */
	union {
		struct {
			uint32_t addr;
		} unpublish;
		struct {
			uint32_t addr;
			char     name[1];
		} query_name;
		struct {
			uint32_t src;
			uint32_t dst;
			uint32_t size;
			uint8_t  data[1];
		} user_msg;
	} msg;
};

struct context {

	/* Object pointer associated with this context. */
	void            *obj;

	/* Callback function for local side reception. */
	t_loc_rx         loc_rx;

	/* Callback function for remote side transmission. */
	t_rem_tx         rem_tx;

	/* Maximum number of endpoints. */
	uint32_t         max_num_endpoints;

	/* Local endpoints published on the remote side. */
	struct endpoint *lep;

	/* Remote endpoints published on the local side. */
	struct endpoint *rep;

        /* Messages sent from not yet published local endpoints. */
	struct {
		struct queued_msg *front;
		struct queued_msg *back;
	} queue;
};


/*
*******************************************************************************
**                          PROTOTYPES FOR COMPILER
*******************************************************************************
*/

static int loc_tx_rlnh_query_name(struct context *ctx,
				  uint32_t addr, const char *name);

static int loc_tx_rlnh_user_msg(struct context *ctx,
				uint32_t src, uint32_t dst,
				uint32_t size, void *data);

static int loc_tx_rlnh_unpublish(struct context *ctx, uint32_t addr);


/*
*******************************************************************************
**                             LOGGING FUNCTIONS
*******************************************************************************
*/

#ifdef _DBG_PRINTF

static void dump_rlnh(const char *prefix, uint32_t src, uint32_t dst,
		      uint32_t size, const union rlnh_message *msg)
{
	if (src != RLNH_NO_ADDR || dst != RLNH_NO_ADDR) {

		_DBG_PRINTF("%sRLNH_USER_MSG(src:%u, dst:%u, size:%u)\n",
			    prefix, src, dst, size);

	} else if (msg->type == htonl(RLNH_TYPE_QUERY_NAME)) {

		_DBG_PRINTF("%sRLNH_QUERY_NAME(addr:%u, name:\"%s\")\n",
			    prefix,
			    ntohl(msg->query_name.addr),
			    msg->query_name.name);

	} else if (msg->type == htonl(RLNH_TYPE_PUBLISH)) {

		_DBG_PRINTF("%sRLNH_PUBLISH(addr:%u, name:\"%s\")\n",
			    prefix,
			    ntohl(msg->publish.addr),
			    msg->publish.name);

	} else if (msg->type == htonl(RLNH_TYPE_UNPUBLISH)) {

		_DBG_PRINTF("%sRLNH_UNPUBLISH(addr:%u)\n",
			    prefix,
			    ntohl(msg->unpublish.addr));

	} else if (msg->type == htonl(RLNH_TYPE_UNPUBLISH_ACK)) {

		_DBG_PRINTF("%sRLNH_UNPUBLISH_ACK(addr:%u)\n",
			    prefix,
			    ntohl(msg->unpublish.addr));

	} else if (msg->type == htonl(RLNH_TYPE_INIT)) {

		_DBG_PRINTF("%sRLNH_INIT(version:%u)\n",
			    prefix,
			    ntohl(msg->init.version));

	} else if (msg->type == htonl(RLNH_TYPE_INIT_REPLY)) {

		_DBG_PRINTF("%sRLNH_INIT_REPLY(status:%u)\n",
			    prefix,
			    ntohl(msg->init_reply.status));

	} else {

		_DBG_PRINTF("%sRLNH_UNKNOWN(%u)",
			    prefix,
			    ntohl(msg->type));
	}
}

static void dump_glh(const char *prefix,
		     uint32_t size, const union glh_message *msg)
{
	if (msg->type == htons(GLH_TYPE_PUBLISH)) {

		_DBG_PRINTF("%sGLH_PUBLISH(zero:0x%04x, "
			    "opid:0x%08x, name:\"%s\")\n",
			    prefix,
			    ntohs(msg->publish.zero),
			    ntohl(msg->publish.opid),
			    msg->publish.name);

	} else if (msg->type == htons(GLH_TYPE_PUBLISH_ACK)) {

		_DBG_PRINTF("%sGLH_PUBLISH_ACK(zero:0x%04x, "
			    "opid:0x%08x, cpid:0x%08x)\n",
			    prefix,
			    ntohs(msg->publish_ack.zero),
			    ntohl(msg->publish_ack.opid),
			    ntohl(msg->publish_ack.cpid));

	} else if (msg->type == htons(GLH_TYPE_QUERY_NAME)) {

		_DBG_PRINTF("%sGLH_QUERY_NAME(zero:0x%04x, "
			    "user:%u, hpid:0x%08x, name:\"%s\")\n",
			    prefix,
			    ntohs(msg->query_name.zero),
			    ntohl(msg->query_name.user),
			    ntohl(msg->query_name.hpid),
			    msg->query_name.name);

	} else if (msg->type == htons(GLH_TYPE_UNPUBLISH)) {

		_DBG_PRINTF("%sGLH_UNPUBLISH(zero:0x%04x, "
			    "pid:0x%08x, remote:%hhu)\n",
			    prefix,
			    ntohs(msg->unpublish.zero),
			    ntohl(msg->unpublish.pid),
			    msg->unpublish.remote);

	} else if (msg->type == htons(GLH_TYPE_USER_MSG)) {

		_DBG_PRINTF("%sGLH_USER_MSG(zero:0x%04x, "
			    "spid:0x%08x, dpid:0x%08x) [%u]\n",
			    prefix,
			    ntohs(msg->user_msg.zero),
			    ntohl(msg->user_msg.spid),
			    ntohl(msg->user_msg.dpid),
			    size);
	} else {

		_DBG_PRINTF("%sGLH_UNKNOWN(%u)\n",
			    prefix,
			    ntohs(msg->type));

	}
}

#else

#define dump_rlnh(...)
#define dump_glh(...)

#endif


/*
*******************************************************************************
**                             SUBFUNCTIONS
*******************************************************************************
*/

static int loc_rx(struct context *ctx,
		  uint32_t src, uint32_t dst, uint32_t size, void *data)
{
	dump_rlnh("LOC RX: ", src, dst, size, data);
	return ctx->loc_rx(ctx->obj, src, dst, size, data);
}

static int rem_tx(struct context *ctx, uint32_t size, void *data)
{
	dump_glh("REM TX: ", size, data);
	return ctx->rem_tx(ctx->obj, size, data);
}

static uint32_t rep_lookup_by_pid(struct context *ctx, uint32_t pid)
{
	uint32_t addr;

	for (addr = 1; addr <= ctx->max_num_endpoints; addr++) {
		if (ctx->rep[addr].pid == pid) {
			return addr;
		}
	}

	return RLNH_NO_ADDR;
}

static uint32_t rep_lookup_by_name(struct context *ctx, const char *name)
{
	uint32_t addr;

	for (addr = 1; addr <= ctx->max_num_endpoints; addr++) {
		if (ctx->rep[addr].name &&
		    strcmp(ctx->rep[addr].name, name) == 0) {
			return addr;
		}
	}

	return RLNH_NO_ADDR;
}

static struct queued_msg* enqueue(struct context *ctx, uint32_t size)
{
	struct queued_msg *qm = malloc(sizeof(struct queued_msg) + size);

	if (!qm) {
		return 0;
	}

	qm->next = 0;
	qm->prev = ctx->queue.back;
	if (ctx->queue.back) {
		ctx->queue.back->next = qm;
	} else {
		ctx->queue.front = qm;
	}
	ctx->queue.back = qm;

	return qm;
}

static int enqueue_query_name(struct context *ctx,
			      uint32_t addr, const char *name)
{
	struct queued_msg *qm = enqueue(ctx, strlen(name));

	if (!qm) {
		return -1;
	}

	qm->addr = addr;
	qm->type = QM_TYPE_QUERY_NAME;
	qm->msg.query_name.addr = addr;
	strcpy(qm->msg.query_name.name, name);

	return 0;
}

static int enqueue_unpublish(struct context *ctx, uint32_t addr)
{
	struct queued_msg *qm = enqueue(ctx, 0);

	if (!qm) {
		return -1;
	}

	qm->addr = addr;
	qm->type = QM_TYPE_UNPUBLISH;
	qm->msg.unpublish.addr = addr;

	return 0;
}

static int enqueue_user_msg(struct context *ctx,
			    uint32_t src, uint32_t dst, uint32_t size, void *data)
{
	struct queued_msg *qm = enqueue(ctx, size);

	if (!qm) {
		return -1;
	}

	qm->addr = src;
	qm->type = QM_TYPE_USER_MSG;
	qm->msg.user_msg.src = src;
	qm->msg.user_msg.dst = dst;
	qm->msg.user_msg.size = size;
	memcpy(qm->msg.user_msg.data, data, size);

	return 0;
}

static int dequeue_query_name(struct context *ctx, struct queued_msg *qm)
{
	return loc_tx_rlnh_query_name(ctx,
				      qm->msg.query_name.addr,
				      qm->msg.query_name.name);
}

static int dequeue_user_msg(struct context *ctx, struct queued_msg *qm)
{
	return loc_tx_rlnh_user_msg(ctx,
				    qm->msg.user_msg.src,
				    qm->msg.user_msg.dst,
				    qm->msg.user_msg.size,
				    qm->msg.user_msg.data);
}

static int dequeue_unpublish(struct context *ctx, struct queued_msg *qm)
{
	return loc_tx_rlnh_unpublish(ctx, qm->msg.unpublish.addr);
}

static int dequeue(struct context *ctx, uint32_t addr)
{
	struct queued_msg *qm, *qm_next;
	int                rv;

	qm = ctx->queue.front;
	while (qm) {

		qm_next = qm->next;

		if (qm->addr == addr) {

			/* Handle the queued message. */
			if (qm->type == QM_TYPE_QUERY_NAME) {
				rv = dequeue_query_name(ctx, qm);
			} else if (qm->type == QM_TYPE_USER_MSG) {
				rv = dequeue_user_msg(ctx, qm);
			} else if (qm->type == QM_TYPE_UNPUBLISH) {
				rv = dequeue_unpublish(ctx, qm);
			} else return -1;

			if (rv) {
				return -1;
			}

			/* Unlink and free the queued message. */
			if (qm->prev) {
				qm->prev->next = qm->next;
			} else {
				ctx->queue.front = qm->next;
			}
			if (qm->next) {
				qm->next->prev = qm->prev;
			} else {
				ctx->queue.back = qm->prev;
			}
			free(qm);
		}

		qm = qm_next;
	}

	return 0;
}


/*
*******************************************************************************
**           TRANSLATE MESSAGES TRANSMITTED FROM LOCAL SIDE (LINX)
*******************************************************************************
*/

/* A0) */
static int loc_tx_rlnh_init(struct context *ctx, uint32_t version)
{
	struct rlnh_init       init = {
		.type = htonl(RLNH_TYPE_INIT),
		.version = htonl(RLNH_VERSION)
	};
	struct rlnh_init_reply init_reply;

	/* A1) Receive an RLNH_INIT message. */
	if (loc_rx(ctx, RLNH_NO_ADDR, RLNH_NO_ADDR, sizeof(init), &init) != 0) {
		return -1;
	}

	/* A3) Receive an RLNH_INIT_REPLY message. */
	init_reply.type = htonl(RLNH_TYPE_INIT_REPLY);
	if (version == RLNH_VERSION) {
		init_reply.status = 0; /* Supported */
	} else {
		init_reply.status = htonl(1); /* Unsupported */
	}
	return loc_rx(ctx, RLNH_NO_ADDR, RLNH_NO_ADDR,
		      sizeof(init_reply), &init_reply);
}

static int loc_tx_rlnh_init_reply(struct context *ctx, uint32_t status)
{
	(void) ctx;
	(void) status;

	/* A2) The lowest version is 1 so there is nothing to do here. */

	return 0;
}

/* B0) */
static int loc_tx_rlnh_publish(struct context *ctx,
			       uint32_t addr, const char *name)
{
	struct glh_publish *publish;
	uint32_t            size;
	int                 rv;

	/* Store the name of the endpoint to be published. */
	ctx->lep[addr].name = calloc(sizeof(char), strlen(name) + 1);
	if (!ctx->lep[addr].name) {
		return -1;
	}
	strcpy(ctx->lep[addr].name, name);

	/* B1) Transmit a GLH_PUBLISH message. */
	size = sizeof(struct glh_publish) + strlen(name);
	publish = malloc(size);
	if (!publish) {
		return -1;
	}
	publish->type = htons(GLH_TYPE_PUBLISH);
	publish->zero = 0;
	publish->opid = htonl(addr);
	strcpy(publish->name, name);
	rv = rem_tx(ctx, size, publish);
	free(publish);

	return rv;
}

/* C0) */
static int loc_tx_rlnh_query_name(struct context *ctx,
				  uint32_t addr, const char *name)
{
	struct glh_query_name *query_name;
	uint32_t               size;
	int                    rv;

	/* If clone PID is not yet known then enqueue the RLNH message. */
	if (ctx->lep[addr].pid == GLH_NO_PID) {
		return enqueue_query_name(ctx, addr, name);
	}

	/* If the endpoint is already published then return. */
	if (rep_lookup_by_name(ctx, name) != RLNH_NO_ADDR) {
		return 0;
	}

	/* C1) Transmit a GLH_QUERY_NAME message. */
	size = sizeof(struct glh_query_name) + strlen(name);
	query_name = malloc(size);
	if (!query_name) {
		return -1;
	}
	query_name->type = htons(GLH_TYPE_QUERY_NAME);
	query_name->zero = 0;
	query_name->user = 0; /* Superuser number. */
	query_name->hpid = htonl(addr);
	strcpy(query_name->name, name);
	rv = rem_tx(ctx, size, query_name);
	free(query_name);

	return rv;
}

/* D0) */
static int loc_tx_rlnh_unpublish(struct context *ctx, uint32_t addr)
{
	struct glh_unpublish      unpublish = {
		.type = htons(GLH_TYPE_UNPUBLISH),
		.zero = 0,
		.pid = htonl(ctx->lep[addr].pid),
		.remote = 0
	};
	struct rlnh_unpublish_ack unpublish_ack = {
		.type = htonl(RLNH_TYPE_UNPUBLISH_ACK),
		.addr = htonl(addr)
	};

	/* If clone PID is not yet known then enqueue the RLNH message. */
	if (ctx->lep[addr].pid == GLH_NO_PID) {
		return enqueue_unpublish(ctx, addr);
	}

	/* Free the endpoint. */
	free(ctx->lep[addr].name);
	ctx->lep[addr].pid = GLH_NO_PID;
	ctx->lep[addr].name = 0;

	/* D1) Transmit a GLH_UNPUBLISH message. */
	if (rem_tx(ctx, sizeof(unpublish), &unpublish) != 0) {
		return -1;
	}

	/* D2) Receive an RLNH_UNPUBLISH message. */
	return loc_rx(ctx, RLNH_NO_ADDR, RLNH_NO_ADDR,
		      sizeof(unpublish_ack), &unpublish_ack);
}

/* G2) */
static int loc_tx_rlnh_unpublish_ack(struct context *ctx, uint32_t addr)
{
	/* Free the endpoint. */
	free(ctx->rep[addr].name);
	ctx->rep[addr].pid = GLH_NO_PID;
	ctx->rep[addr].name = 0;

	return 0;
}

static int loc_tx_rlnh_user_msg(struct context *ctx,
				uint32_t src, uint32_t dst,
				uint32_t size, void *data)
{
	struct glh_user_msg *user_msg;
	int                  rv;

	/* If clone PID is not yet known then enqueue the RLNH message. */
	if (ctx->lep[src].pid == GLH_NO_PID) {
		return enqueue_user_msg(ctx, src, dst, size, data);
	}

	/* Translate the user message. */
	user_msg = malloc(sizeof(struct glh_user_msg) - 1 + size);
	if (!user_msg) {
		return -1;
	}
	user_msg->type = htons(GLH_TYPE_USER_MSG);
	user_msg->zero = 0;
	user_msg->spid = htonl(ctx->lep[src].pid);
	user_msg->dpid = htonl(ctx->rep[dst].pid);
	memcpy(user_msg->data, data, size);
	rv = rem_tx(ctx, sizeof(struct glh_user_msg) - 1 + size, user_msg);
	free(user_msg);

	return rv;
}


/*
*******************************************************************************
**           TRANSLATE MESSAGES RECEIVED FROM THE REMOTE SIDE (GLH)
*******************************************************************************
*/

/* E0) */
static int rem_rx_glh_publish(struct context *ctx,
			      uint32_t opid, const char *name)
{
	struct glh_publish_ack  publish_ack;
	struct rlnh_publish    *publish;
	uint32_t                size, addr;
	int                     rv;

	/* First check if this remote endpoint is published already. */
	if (rep_lookup_by_name(ctx, name) != RLNH_NO_ADDR) {
		return 0;
	}

	/* Lookup a free remote endpoint structure. */
	addr = rep_lookup_by_pid(ctx, GLH_NO_PID);
	if (addr == RLNH_NO_ADDR) {
		return -1;
	}

	/* Store PID and name of remote endpoint. */
	ctx->rep[addr].pid = opid;
	ctx->rep[addr].name = calloc(sizeof(char), strlen(name) + 1);
	if (!ctx->rep[addr].name) {
		return -1;
	}
	strcpy(ctx->rep[addr].name, name);

	/* E1) Transmit a GLH_PUBLISH_ACK message. */
	publish_ack.type = htons(GLH_TYPE_PUBLISH_ACK);
	publish_ack.zero = 0;
	publish_ack.opid = htonl(opid);
	publish_ack.cpid = htonl(addr);
	if (rem_tx(ctx, sizeof(publish_ack), &publish_ack) != 0) {
		return -1;
	}

	/* E2) Receive an RLNH_PUBLISH message. */
	size = sizeof(struct rlnh_publish) + strlen(name);
	publish = malloc(size);
	if (!publish) {
		return -1;
	}
	publish->type = htonl(RLNH_TYPE_PUBLISH);
	publish->addr = htonl(addr);
	strcpy(publish->name, name);
	rv = loc_rx(ctx, RLNH_NO_ADDR, RLNH_NO_ADDR, size, publish);
	free(publish);

	return rv;
}

/* B2) */
static int rem_rx_glh_publish_ack(struct context *ctx,
				  uint32_t opid, uint32_t cpid)
{
	/* Store the clone PID. */
	ctx->lep[opid].pid = cpid;

	/* Now the clone PID is known so dequeue any enqueued RLNH messages. */
	return dequeue(ctx, opid);
}

/* F0) */
static int rem_rx_glh_query_name(struct context *ctx,
                                 uint32_t hpid, const char *name)
{
	struct rlnh_query_name *query_name;
	uint32_t                size;
	int                     rv;

	/* F1) Receive an RLNH_QUERY_NAME message. */
	size = sizeof(struct rlnh_query_name) + strlen(name);
	query_name = malloc(size);
	if (!query_name) {
		return -1;
	}
	query_name->type = htonl(RLNH_TYPE_QUERY_NAME);
	query_name->addr = htonl(rep_lookup_by_pid(ctx, hpid));
	strcpy(query_name->name, name);
	rv = loc_rx(ctx, RLNH_NO_ADDR, RLNH_NO_ADDR, size, query_name);
	free(query_name);

	return rv;
}

static int rem_rx_glh_unpublish(struct context *ctx,
				uint32_t pid, uint8_t remote)
{
	int rv;

	if (!remote) { /* G0) */

		struct rlnh_unpublish unpublish = {
			.type = htonl(RLNH_TYPE_UNPUBLISH),
			.addr = htonl(pid)
		};

		/* G1) Receive an RLNH_UNPUBLISH message. */
		rv = loc_rx(ctx, RLNH_NO_ADDR, RLNH_NO_ADDR,
			    sizeof(unpublish), &unpublish);

	} else { /* H0) */

		/*
		 * Note: In this case the clone was killed on the remote side.
		 *       So it has to be re-published as a new clone then.
		 */

		struct glh_publish *publish;
		uint32_t            size;

		/* H1) Transmit a GLH_PUBLISH message. */
		size = sizeof(struct glh_publish) + strlen(ctx->lep[pid].name);
		publish = malloc(size);
		if (!publish) {
			return -1;
		}
		publish->type = htons(GLH_TYPE_PUBLISH);
		publish->zero = 0;
		publish->opid = htonl(pid);
		strcpy(publish->name, ctx->lep[pid].name);
		rv = rem_tx(ctx, size, publish);
		free(publish);

	}

	return rv;
}

static int rem_rx_glh_user_msg(struct context *ctx,
			       uint32_t spid, uint32_t dpid,
			       uint32_t size, void *data)
{
	if (spid > ctx->max_num_endpoints || dpid > ctx->max_num_endpoints) {
		return -1;
	} else if (ctx->lep[dpid].pid == GLH_NO_PID) {
		return 0; /* OK, LEP has been unpublished. */
	}

	return loc_rx(ctx, spid, dpid, size, data);
}


/*
*******************************************************************************
**                           INTERFACE FUNCTIONS
*******************************************************************************
*/

int t_loc_tx(void *handle,
	     uint32_t src, uint32_t dst, uint32_t size, void *data)
{
	struct context     *ctx = handle;
	union rlnh_message *msg = data;
	int                 rv;

	dump_rlnh("LOC TX: ", src, dst, size, data);

	if (!ctx) {
		return -1; /* Sanity check */
	}

	if (src != RLNH_NO_ADDR || dst != RLNH_NO_ADDR) {

		rv = loc_tx_rlnh_user_msg(ctx, src, dst, size, data);

	} else if (msg->type == htonl(RLNH_TYPE_INIT)) {

		rv = loc_tx_rlnh_init(ctx,
				      ntohl(msg->init.version));

	} else if (msg->type == htonl(RLNH_TYPE_INIT_REPLY)) {

		rv = loc_tx_rlnh_init_reply(ctx,
					    ntohl(msg->init_reply.status));

	} else if (msg->type == htonl(RLNH_TYPE_PUBLISH)) {

		rv = loc_tx_rlnh_publish(ctx,
					 ntohl(msg->publish.addr),
					 msg->publish.name);

	} else if (msg->type == htonl(RLNH_TYPE_QUERY_NAME)) {

		rv = loc_tx_rlnh_query_name(ctx,
					    ntohl(msg->query_name.addr),
					    msg->query_name.name);

	} else if (msg->type == htonl(RLNH_TYPE_UNPUBLISH)) {

		rv = loc_tx_rlnh_unpublish(ctx,
					   ntohl(msg->unpublish.addr));

	} else if (msg->type == htonl(RLNH_TYPE_UNPUBLISH_ACK)) {

		rv = loc_tx_rlnh_unpublish_ack(ctx,
					       ntohl(msg->unpublish_ack.addr));

	} else return -1;

	return rv == 0 ? 0 : -1;
}

int t_rem_rx(void *handle, uint32_t size, void *data)
{
	struct context    *ctx = handle;
	union glh_message *msg = data;
	int                rv;

	dump_glh("REM RX: ", size, data);

	if (!ctx) {
		return -1; /* Sanity check */
	}

	if (msg->type == htons(GLH_TYPE_PUBLISH)) {

		rv = rem_rx_glh_publish(ctx,
					ntohl(msg->publish.opid),
					msg->publish.name);

	} else if (msg->type == htons(GLH_TYPE_PUBLISH_ACK)) {

		rv = rem_rx_glh_publish_ack(ctx,
					    ntohl(msg->publish_ack.opid),
					    ntohl(msg->publish_ack.cpid));

	} else if (msg->type == htons(GLH_TYPE_QUERY_NAME)) {

		rv = rem_rx_glh_query_name(ctx,
					   ntohl(msg->query_name.hpid),
					   msg->query_name.name);

	} else if (msg->type == htons(GLH_TYPE_UNPUBLISH)) {

		rv = rem_rx_glh_unpublish(ctx,
					  ntohl(msg->unpublish.pid),
					  msg->unpublish.remote);

	} else if (msg->type == htons(GLH_TYPE_USER_MSG)) {

		rv = rem_rx_glh_user_msg(ctx,
					 ntohl(msg->user_msg.spid),
					 ntohl(msg->user_msg.dpid),
					 size - 12, /* Size of data */
					 msg->user_msg.data);

	} else return -1;

	return rv == 0 ? 0 : -1;
}

int t_open(void **handle, void *obj, t_loc_rx rx, t_rem_tx tx,
	   uint32_t max_num_endpoints)
{
	struct context *ctx;

	/* Allocate space for context. */
	ctx = calloc(1, sizeof(struct context));
	if (!ctx) {
		goto err_mem_ctx;
	}
	ctx->obj = obj;
	ctx->loc_rx = rx;
	ctx->rem_tx = tx;
	ctx->max_num_endpoints = max_num_endpoints;

	/* Allocate space for local endpoints. */
	ctx->lep = calloc(max_num_endpoints + 1, sizeof(struct endpoint));
	if (!ctx->lep) {
		goto err_mem_lep;
	}

	/* Allocate space for remote endpoints. */
	ctx->rep = calloc(max_num_endpoints + 1, sizeof(struct endpoint));
	if (!ctx->rep) {
		goto err_mem_rep;
	}

	*handle = ctx;
	return 0;

err_mem_rep:
	free(ctx->lep);
err_mem_lep:
	free(ctx);
err_mem_ctx:
	*handle = 0;
	return -1;
}

int t_close(void **handle)
{
	struct context    *ctx = (struct context*) *handle;
	struct queued_msg *qm;
	uint32_t           addr;

	if (!ctx) {
		return -1;
	}

	/* Free endpoint information. */
	for (addr = 1; addr <= ctx->max_num_endpoints; addr++) {
		if (ctx->lep[addr].name) {
			free(ctx->lep[addr].name);
		}
		if (ctx->rep[addr].name) {
			free(ctx->rep[addr].name);
		}
	}
	free(ctx->lep);
	free(ctx->rep);

	/* Free enqueued messages. */
	while (ctx->queue.front) {
		qm = ctx->queue.front->next;
		free(ctx->queue.front);
		ctx->queue.front = qm;
	}

	/* Free the context. */
	free(ctx);

	*handle = 0;
	return 0;
}

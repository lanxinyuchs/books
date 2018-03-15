/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
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
#include <stdint.h>
#include <stdarg.h>
#include <arpa/inet.h>

#ifdef LTTNG

#include "ulh_hdlc_proto.h"
#include "ulh_cm.h"
#include "hdlc_lttng.h"
#include "trace.h"


static const char *hdlc_frame_type_tostr(int type)
{
	switch (type) {
	case 0:                 return "INFO";
	case HDLC_SFRAME_RR:    return "RR";
	case HDLC_SFRAME_RNR:   return "RNR";
	case HDLC_SFRAME_REJ:   return "REJ";
	case HDLC_SFRAME_SREJ:  return "SREJ";
	case HDLC_UFRAME_UI:    return "UI";
	case HDLC_UFRAME_SNRM:  return "SNRM";
	case HDLC_UFRAME_SABM:  return "SABM";
	case HDLC_UFRAME_SABME: return "SABME";
	case HDLC_UFRAME_SARME: return "SARME";
	case HDLC_UFRAME_SNRME: return "SNRME";
	case HDLC_UFRAME_SARM:  return "SARM/DM";
	case HDLC_UFRAME_DISC:  return "DISC";
	case HDLC_UFRAME_UA:    return "UA";
	case HDLC_UFRAME_FRMR:  return "FRMR";
	case HDLC_UFRAME_RESET: return "RESET";
	}
	return "????";
}

static void decode_iframe(const char *name, const char *pfx,
		struct hdlc_header *hdlc, int len)
{
	char buf[256];
	sprintf(buf, "%02X %02X %s %s len=%-3d PF=%d %5s N(R)=%d N(S)=%d",
			hdlc->hdlc_address, 
			hdlc->hdlc_control,
			name,
			pfx,
			len,
			hdlc_extract_pf(hdlc),
			hdlc_frame_type_tostr(0),
			hdlc_extract_receive_seqno(hdlc),
			hdlc_extract_iframe_send_seqno(hdlc));

	tracepoint(com_ericsson_hdlc, hdlc_dbg, buf);
}

static void decode_sframe(const char *name, const char *pfx,
		struct hdlc_header *hdlc, int len)
{
	char buf[256];
	sprintf(buf, "%02X %02X  %s %s len=%-3d PF=%d %5s N(R)=%d",
			hdlc->hdlc_address,
			hdlc->hdlc_control,
			name,
			pfx,
			len,
			hdlc_extract_pf(hdlc),
			hdlc_frame_type_tostr(hdlc_extract_sframe_type(hdlc)),
			hdlc_extract_receive_seqno(hdlc));

	tracepoint(com_ericsson_hdlc, hdlc_dbg, buf);
}

static void decode_uframe(const char *name, const char *pfx,
		struct hdlc_header *hdlc, int len)
{
	char buf[256];
	sprintf(buf, "%02X %02X %s %s len=%-3d PF=%d %5s",
			hdlc->hdlc_address,
			hdlc->hdlc_control,
			name,
			pfx,
			len,
			hdlc_extract_pf(hdlc),
			hdlc_frame_type_tostr(hdlc_extract_uframe_type(hdlc)));
	tracepoint(com_ericsson_hdlc, hdlc_dbg, buf);
}

void hdlc_print_header(char *name, char *pfx, uint8_t *p, int len)
{
	struct hdlc_header *hdlc = (struct hdlc_header*)p;
	switch (hdlc_extract_frame_type(hdlc)) {
	case HDLC_IFRAME:
		decode_iframe(name, pfx, hdlc, len);
		break;
	case HDLC_UFRAME:
		decode_uframe(name, pfx, hdlc, len);
		break;
	case HDLC_SFRAME:
		decode_sframe(name, pfx, hdlc, len);
		break;
	}
}

void hdlc_trace_dbg(const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_hdlc, hdlc_dbg, buffer);
}

void hdlc_trace_info(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_hdlc, hdlc_info, file, line, buffer);
}

void hdlc_trace_error(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_hdlc, hdlc_error, file, line, buffer);
}




#define RLNH_VERSION		1
#define RLNH_INIT			5
#define RLNH_INIT_REPLY		6
#define RLNH_PUBLISH		2
#define RLNH_QUERY_NAME		1
#define RLNH_UNPUBLISH		3
#define RLNH_UNPUBLISH_ACK	4
#define RLNH_PUBLISH_PEER	7

struct rlnh_header {
	uint16_t reserved;
	uint16_t type;
};

struct rlnh_msg_init {
	struct rlnh_header hdr;
	uint32_t version;
};

struct rlnh_msg_initreply {
	struct rlnh_header hdr;
	uint32_t status;
};

struct rlnh_msg_publish {
	struct rlnh_header hdr;
	uint32_t laddr;

	char name[1];
};

struct rlnh_msg_unpublish {
	struct rlnh_header hdr;
	uint32_t laddr;
};

struct rlnh_msg_unpublish_ack {
	struct rlnh_header hdr;
	uint32_t laddr;
};

struct rlnh_msg_queryname {
	struct rlnh_header hdr;
	uint32_t laddr;

	char name[1];
};

void lnh_print_header(char *name, char *prfx,
                       struct ulh_cm_msghdr *chdr, uint8_t *buf)
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
		HDLC_TRACE(	rlnh_init, name, prfx, rhdr->reserved, rhdr->type,
                          rihdr->version);
		break;
	case RLNH_INIT_REPLY:
		rirhdr = (struct rlnh_msg_initreply *)rhdr;
		HDLC_TRACE(rlnh_init_reply, name, prfx, rhdr->reserved, rhdr->type,
                          rirhdr->status, "");
		break;
	case RLNH_PUBLISH:
		rphdr = (struct rlnh_msg_publish *)rhdr;
		HDLC_TRACE(rlnh_publish, name, prfx, rhdr->reserved, rhdr->type,
                          rphdr->laddr, rphdr->name);
		break;
	case RLNH_QUERY_NAME:
		rqhdr = (struct rlnh_msg_queryname *)rhdr;
		HDLC_TRACE(rlnh_query, name, prfx, rhdr->reserved, rhdr->type,
                          rqhdr->laddr, rqhdr->name);
		break;
	case RLNH_UNPUBLISH:
		ruhdr = (struct rlnh_msg_unpublish *)rhdr;
		HDLC_TRACE(rlnh_unpublish, name, prfx, rhdr->reserved, rhdr->type,
                          ruhdr->laddr);
		break;
	case RLNH_UNPUBLISH_ACK:
		ruahdr = (struct rlnh_msg_unpublish_ack *)rhdr;
		HDLC_TRACE(rlnh_unpublish_ack, name, prfx, rhdr->reserved, rhdr->type,
                          ruahdr->laddr);
		break;
	default:
		HDLC_TRACE(rlnh_data, name, prfx, chdr->src, chdr->dst, chdr->size,
                          *((uint32_t *)buf));
		break;
	}
}
#endif

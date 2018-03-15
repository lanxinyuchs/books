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


#include <stdint.h>
#include <stdarg.h>

#include "ulh_transport.h"
#include "ecb_lttng.h"


struct hdlc_header {
	uint8_t hdlc_address;
	uint8_t hdlc_control;
};

/*
 * Types of frames.
 */
#define HDLC_IFRAME     		0
#define HDLC_SFRAME				1
#define HDLC_UFRAME     		3

/*
 * Types of S-frames.  The secondary transmits RR, REJ
 * and receives RR, RNR
 */
#define HDLC_SFRAME_RR          0x01
#define HDLC_SFRAME_RNR         0x05
#define HDLC_SFRAME_REJ         0x09
#define HDLC_SFRAME_SREJ        0x0d
/*
 * Types of U-frames.  The secondary transmits UA, DM, FRMR
 * and receives      SARM, DISC
 */
#define HDLC_UFRAME_UI          0x03
#define HDLC_UFRAME_SNRM        0x83
#define HDLC_UFRAME_SABM        0x2f
#define HDLC_UFRAME_SABME       0x6f
#define HDLC_UFRAME_SARME       0x4f
#define HDLC_UFRAME_SNRME       0xcf
#define HDLC_UFRAME_SARM        0x0f
#define HDLC_UFRAME_DISC        0x43
#define HDLC_UFRAME_UA          0x63
#define HDLC_UFRAME_DM          0x0f
#define HDLC_UFRAME_FRMR        0x87
#define HDLC_UFRAME_RESET       0x8f




static inline int hdlc_frame_type(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control & 1) ? (hdlc->hdlc_control & 3) : HDLC_IFRAME;
}

static inline int hdlc_pf(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control >> 4) & 1;
}

static inline int hdlc_sframe_type(struct hdlc_header* hdlc)
{
	return (hdlc->hdlc_control & 0x0f);
}

static inline int hdlc_uframe_type(struct hdlc_header* hdlc)
{
	return hdlc->hdlc_control & 0xef;
}

static inline int hdlc_iframe_send_seqno(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control >> 1) & 0x7;
}

static inline int hdlc_receive_seqno(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control >> 5) & 0x7;
}

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
			hdlc_pf(hdlc),
			hdlc_frame_type_tostr(0),
			hdlc_receive_seqno(hdlc),
			hdlc_iframe_send_seqno(hdlc));

	tracepoint(com_ericsson_ecb, ecblnh_dbg, buf);
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
			hdlc_pf(hdlc),
			hdlc_frame_type_tostr(hdlc_sframe_type(hdlc)),
			hdlc_receive_seqno(hdlc));

	tracepoint(com_ericsson_ecb, ecblnh_dbg, buf);
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
			hdlc_pf(hdlc),
			hdlc_frame_type_tostr(hdlc_uframe_type(hdlc)));
	tracepoint(com_ericsson_ecb, ecblnh_dbg, buf);
}


void hdlc_print_header(char *name, char *pfx, uint8_t *p, int len)
{
	struct hdlc_header *hdlc = (struct hdlc_header*)p;
	switch (hdlc_frame_type(hdlc)) {
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

void ecb_trace_dbg(const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_ecb, ecblnh_dbg, buffer);
}

void ecb_trace_info(const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_ecb, ecblnh_info, buffer);
}

void ecb_trace_error(const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_ecb, ecblnh_error, buffer);
}

void a4c_trace_debug(char *text, uint8_t *p, int length)
{
	char buf[1024], *q = buf;
	int cnt;

	q += sprintf(q, "%s: %d ", text, length);
	if (p && (length <=256)) {
		q += sprintf(q, "data: ");

		for (cnt = 0; cnt < length; cnt++)
			q += sprintf(q, "%02X ", p[cnt]);
	}
	tracepoint(com_ericsson_ecb, a4ci_dbg, buf);
}

void a4c_trace_info(const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_ecb, a4ci_info, buffer);
}

void a4c_trace_error(const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_ecb, a4ci_error, buffer);
}

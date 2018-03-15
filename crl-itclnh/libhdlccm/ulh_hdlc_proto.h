#ifndef ULH_HDLC_PROTO_H__
#define ULH_HDLC_PROTO_H__

#include <stdint.h>

struct hdlc_header {
	uint8_t hdlc_address;
	uint8_t hdlc_control;
};
#define HDLC_HLEN (sizeof(struct hdlc_header))
#define HDLC_HALIGN 0   /* not needed */

/*
 * Types of frames.
 */
#define HDLC_IFRAME     0
#define HDLC_SFRAME     1
#define HDLC_UFRAME     3

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

/* Types of UI frame data
 * Value 0 should not be used
*/
#define HDLC_UI_WAKE_UP        1
#define HDLC_UI_WAKE_UP_ACK    2
#define HDLC_UI_PORT_QUERY     3
#define HDLC_UI_PORT_QUERY_ACK 4

static inline void hdlc_set_pf(struct hdlc_header *hdlc)
{
	hdlc->hdlc_control |= (1 << 4);
}

static inline void hdlc_build_iframe(struct hdlc_header *hdlc,
		unsigned char address,
		unsigned ackno, unsigned seqno, int pf)
{
	hdlc->hdlc_address = address;
	hdlc->hdlc_control = (ackno << 5) | (seqno << 1) | HDLC_IFRAME;
	if (pf != 0)
		hdlc_set_pf(hdlc);
}

static inline void hdlc_build_sframe(struct hdlc_header *hdlc,
		unsigned char address,
		unsigned type, unsigned seqno, int pf)
{
	hdlc->hdlc_address = address;
	hdlc->hdlc_control = (seqno << 5) | type;
	if (pf != 0)
		hdlc_set_pf(hdlc);
}

static inline void hdlc_build_uframe(struct hdlc_header *hdlc, 
		unsigned char address,
		unsigned type, int pf)
{
	hdlc->hdlc_address = address;
	hdlc->hdlc_control = type;
	if (pf != 0)
		hdlc_set_pf(hdlc);
}

static inline int hdlc_extract_frame_type(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control & 1) ? (hdlc->hdlc_control & 3) : HDLC_IFRAME;
}

static inline int hdlc_extract_pf(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control >> 4) & 1;
}

static inline int hdlc_extract_sframe_type(struct hdlc_header* hdlc)
{
	return (hdlc->hdlc_control & 0x0f);
}

static inline int hdlc_extract_uframe_type(struct hdlc_header* hdlc)
{
	return hdlc->hdlc_control & 0xef;
}

static inline int hdlc_extract_iframe_send_seqno(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control >> 1) & 0x7;
}

static inline int hdlc_extract_receive_seqno(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control >> 5) & 0x7;
}

/*
 * The fragmentation header is the first thing sent in the info field of all I-frames.
 * The FRAG_PRIORITY bit is set in all fragments belonging to a high-priority
 * message.
 */
struct frag_header {
	uint16_t frag_bits;
#                       define FRAG_PRIORITY    0x0100
#                       define FRAG_LAST        0x4000
#                       define FRAG_FIRST       0x8000
};

/*
 * This header is prepended to the first (or only) fragment of a message.
 */
struct msg_header {
	uint32_t msg_src;
	uint32_t msg_dst;
	uint32_t msg_size;
};
#endif /* !ULD_HDCL_PROTO_H__ */

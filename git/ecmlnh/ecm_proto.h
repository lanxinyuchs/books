#include <arpa/inet.h>
#include <net/ethernet.h>

#define ECM_ETHTYPE 0x8911

#define ECM_VER_3 3

#define ECM_COREID
#define ECM_MAIN   0x0
#define ECM_CONN   0x1
#define ECM_UDATA  0x2
#define ECM_FRAG   0x3
#define ECM_ACK    0x4
#define ECM_NACK   0x5
#define ECM_LAST   0xF
#define ECM_NONE   0xF

#define ECM_SEQ_MASK 0xFFF

#define ECM_MAIN_SIZE sizeof(uint32_t)
#define ECM_MAIN_GET_NEXT(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 28) & 0xF)
#define ECM_MAIN_GET_VER(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 25) & 0x7)
#define ECM_MAIN_GET_CONNID(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 15) & 0xFF)
#define ECM_MAIN_GET_PKTSIZE(hdr) \
	(ntohl(*((uint32_t *)hdr)) & 0x3FFF)
#define ECM_MAIN_SET(hdr, next, ver, connid, pktsize) \
        *((uint32_t *)hdr) = htonl( next   << 28 |    \
                                    ver    << 25 |    \
                                    connid << 15 |    \
                                    pktsize)
#define ECM_MAIN_ADVANCE(hdr) (hdr = ((uint32_t *)hdr) + 1)

/* We need to check these values!!! */
#define ECM_CONNTYPE_RESET       1
#define ECM_CONNTYPE_CONNECT     2
#define ECM_CONNTYPE_CONNECT_ACK 3
#define ECM_CONNTYPE_ACK         4

#define ECM_CONN_SIZE(features) (4 * sizeof(uint32_t) + \
				 strlen(features) + 1)

#define ECM_DST_OFFSET      4
#define ECM_SRC_OFFSET      (4 + ETH_ALEN)
#define ECM_FEATURES_OFFSET (4 + 2 * ETH_ALEN)
#define ECM_CONN_GET_NEXT(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 28) & 0xF)
#define ECM_CONN_GET_TYPE(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 24) & 0xF)
#define ECM_CONN_GET_SIZE(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 21) & 0x7)
#define ECM_CONN_GET_WIN(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 17) & 0xF)
#define ECM_CONN_GET_CONNID(hdr) \
	(ntohl(*((uint32_t *)hdr)) & 0xFF)
#define ECM_CONN_GET_DST(hdr) \
	(void *)((uint8_t *)hdr + ECM_DST_OFFSET)
#define ECM_CONN_GET_SRC(hdr) \
	(void *)((uint8_t *)hdr + ECM_SRC_OFFSET)
#define ECM_CONN_GET_FEATURES(hdr) \
	(void *)((uint8_t *)hdr + ECM_FEATURES_OFFSET)

#define ECM_CONN_SET(hdr, next, type, size, win, connid) \
	*((uint32_t *)hdr) = htonl( next   << 28 |       \
                                    type   << 24 |       \
                                    size   << 21 |       \
                                    win    << 17 |       \
                                    connid)
#define ECM_CONN_SET_DST(hdr, mac) \
	memcpy(((uint8_t *)hdr + ECM_DST_OFFSET), mac, ETH_ALEN)
#define ECM_CONN_SET_SRC(hdr, mac) \
	memcpy(((uint8_t *)hdr + ECM_SRC_OFFSET), mac, ETH_ALEN)
#define ECM_CONN_SET_FEATURES(hdr, features) \
	strcpy(((char *)hdr + ECM_FEATURES_OFFSET), features)
#define ECM_CONN_ADVANCE(hdr) (hdr = ((uint32_t *)hdr) + 3)

#define ECM_ACK_SIZE (sizeof(uint32_t))
#define ECM_ACK_GET_NEXT(hdr)  \
	((ntohl(*((uint32_t *)hdr)) >> 28) & 0xF)
#define ECM_ACK_GET_R(hdr)     \
	((ntohl(*((uint32_t *)hdr)) >> 27) & 0x1)
#define ECM_ACK_GET_ACKNO(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 12) & ECM_SEQ_MASK)
#define ECM_ACK_GET_SEQNO(hdr) \
	(ntohl(*((uint32_t *)hdr)) & ECM_SEQ_MASK)

#define ECM_ACK_SET(hdr, next, r, ackno, seqno)		 \
	*((uint32_t *)hdr) = htonl( next   << 28 |       \
                                    r      << 27 |       \
                                    ackno  << 12 |       \
                                    seqno)
#define ECM_ACK_ADVANCE(hdr) (hdr = ((uint32_t *)hdr) + 1)

#define ECM_NACK_SIZE (ECM_MAIN_SIZE + sizeof(uint32_t))
#define ECM_NACK_GET_NEXT(hdr)  \
	((ntohl(*((uint32_t *)hdr)) >> 28) & 0xF)
#define ECM_NACK_GET_COUNT(hdr) \
	((ntohl(*((uint32_t *)hdr)) >> 16) & 0xFF)
#define ECM_NACK_GET_SEQNO(hdr) \
	(ntohl(*((uint32_t *)hdr)) & ECM_SEQ_MASK)

#define ECM_NACK_SET(hdr, next, count, seqno)	    \
	*((uint32_t *)hdr) = htonl( next   << 28 |  \
                                    count  << 16 |  \
                                    seqno)
#define ECM_NACK_ADVANCE(hdr) (hdr = ((uint32_t *)hdr) + 1)

#define ECM_UDATA_SIZE (3 * sizeof(uint32_t))
#define ECM_UDATA_GET_NEXT(hdr)  \
	((ntohl(*((uint32_t *)hdr)) >> 28) & 0xF)
#define ECM_UDATA_GET_O(hdr)  \
	((ntohl(*((uint32_t *)hdr)) >> 27) & 0x1)
#define ECM_UDATA_GET_M(hdr)  \
	((ntohl(*((uint32_t *)hdr)) >> 15) & 0x1)
#define ECM_UDATA_GET_FRAGNO(hdr)	\
	(ntohl(*((uint32_t *)hdr)) & 0x7FFF)
#define ECM_UDATA_GET_DST(hdr_p)	\
	ntohl(*((uint32_t *)hdr_p + 1))
#define ECM_UDATA_GET_SRC(hdr_p)	 \
	ntohl(*((uint32_t *)hdr_p + 2))

#define ECM_UDATA_SET(hdr, next, o, m, fragno)  	 \
	*((uint32_t *)hdr) = htonl( next   << 28 |       \
                                    o      << 27 |       \
                                    m      << 15 |       \
                                    fragno)

#define ECM_UDATA_SET_DST(hdr, dst)	 \
	*((uint32_t *)hdr + 1) = htonl(dst)
#define ECM_UDATA_SET_SRC(hdr, src)	 \
	*((uint32_t *)hdr + 2) = htonl(src)
#define ECM_UDATA_ADVANCE(hdr) (hdr = ((uint32_t *)hdr) + 3)

#define ECM_FRAG_SIZE (sizeof(uint32_t))
#define ECM_FRAG_GET_NEXT(hdr)  \
	((ntohl(*((uint32_t *)hdr)) >> 28) & 0xF)
#define ECM_FRAG_GET_M(hdr)  \
	((ntohl(*((uint32_t *)hdr)) >> 15) & 0x1)
#define ECM_FRAG_GET_FRAGNO(hdr)	\
	(ntohl(*((uint32_t *)hdr)) & 0x7FFF)

#define ECM_FRAG_SET(hdr, next, m, fragno)           \
	*((uint32_t *)hdr) = ntohl( next   << 28 |   \
                                    m      << 15 |   \
                                    fragno)
#define ECM_FRAG_ADVANCE(hdr) (hdr = ((uint32_t *)hdr) + 1)


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
#include <ctype.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdarg.h>
#include <syslog.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>
#include <linux/if_arp.h>
#include <linux/if.h>
#include <net/ethernet.h>
#include <ulh_dl_list.h>
#include <itc.h>
#include <ulh_ref.h>
#include <ulh_transport.h>
#include "ulh_trace.h"
#include "ulh_lttng.h"


/*******************************************************************************
**  #defines
*******************************************************************************/
#define ULH_EBCOM_ATYPE			0xeb
#define ULH_EBCOM_NOVLAN		0x1000
#define ULH_ETHERTYPE_802_1Q	0x8100
#define ULH_ETHERTYPE_EBCOM		0xebc0
#define ULH_EBCOM_VLAN			10
#define ULH_EBCOM_RMTU			1500

#define ULH_ETHER_QHDRSIZ		14
#define ULH_EBCOM_MPHDRSIZ		36
#define ULH_EBCOM_HDRSIZE		(ULH_ETHER_QHDRSIZ + ULH_EBCOM_MPHDRSIZ)



/*******************************************************************************
**  types
*******************************************************************************/
struct ulh_ether_hdr {
	uint8_t 				dst[6];
	uint8_t 				src[6];
	/*uint16_t 				type;
	uint16_t 				tci;*/
	uint16_t 				proto;
};

struct ulh_ebcom_hdr {
	struct ulh_ether_hdr	ethdr;
	uint8_t 				mphdr[ULH_EBCOM_MPHDRSIZ];
};

struct ulh_ebcom_addr {
	uint8_t 				mac[6];
	uint32_t 				lcep;
	uint8_t 				qix;
};

struct ulh_ebcom_conn {
	struct dl_list 			link;
	uint32_t 				cep;
	uint32_t 				cid;
	int 					mtu;

	struct sockaddr_ll 		saddr;
	struct ulh_ebcom_hdr	txhdr;
};

struct ulh_ebcom {
	pthread_mutex_t 		lock;
	struct dl_list 			conns;
	volatile int			running;
	int						sd;
	int						ifindex;
	pthread_t 				rx_thread;
	char 					name[32];
	uint8_t 				own_mac[8];
};


/*******************************************************************************
**  locals
*******************************************************************************/

#ifdef LTTNG
void ulh_trace_error(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_ulh, ulh_error, file, line, buffer);
}

void ulh_trace_info(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_ulh, ulh_info, file, line, buffer);
}
#endif



static inline void ebcom_set_rev(uint8_t *hdr, uint8_t rev)
{
	*hdr &= 0x0f;
	*hdr |= rev << 4;
}

static inline void ebcom_set_prim(uint8_t *hdr, uint8_t prim)
{
	*hdr &= 0xf0;
	*hdr |= prim & 0xf;
}

static inline void ebcom_set_ctrl(uint8_t *hdr, uint8_t ctrl)
{
	*(hdr + 1) = ctrl << 3;
}

static inline void ebcom_set_size(uint8_t *hdr, uint16_t size)
{
	*(((uint16_t *)hdr) + 1) = htons(size);
}

static inline void ebcom_set_seq(uint8_t *hdr, uint8_t seq)
{
	*(hdr + 4) = seq;
}

static inline uint32_t ebcom_get_cep(uint8_t *hdr)
{
	uint32_t cep = ntohl(*(((uint32_t *) hdr) + 3));
	return (cep >> 4) & 0xfffff;
}

static inline void ebcom_set_cep(uint8_t *hdr, uint32_t cep, uint8_t qix)
{
	uint32_t val = ntohl(*(((uint32_t *) hdr) + 3));
	val &= 0xff000000;
	val |= ((cep & 0xfffff) << 4) | (qix & 0xf);
	*(((uint32_t *) hdr) + 3) = htonl(val);
}

static inline void ebcom_set_vlan(uint8_t *hdr, uint16_t vlan)
{
	*(((uint16_t *)hdr) + 15) = htons(vlan);
}



/*     	The EBCOM header

  		  7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  		 +-------+-------+---------+-----+---------------+---------------+
  		 |  rev. | prim  | Control |rsv  |         EBCom Length          |
  	0	 |       |       |         |     |                               |  3
  		 +-------+-------+---------+-----+---------------+---------------+
		 |   sequence#   |                 RESERVED                      |
	4	 |               |                                               |  7
		 +---------------+---------------+---------------+---------------+
		 |             Sourec Specific Observability (Optional)          |
	8	 |                                                               |  11
		 +---------------+---------------+---------------+---------------+


           		                                          7 6 5 4 3 2 1 0
		 +---------------+---------------+---------------+-------+-------+
		 |   Control     |           xCEP Destination            | Qidx  |
   12	 |               |                                       |       |  15
		 +---------------+---------------+---------------+-------+-------+
		 |   Reserved    |   Trace Mask  |        Signal Number          |
   16	 |               |               |                               |  19
		 +---------------+---------------+---------------+---------------+


		The 16 byte General Purpose (GP) field is used to convey MAC/VLAN

		 +---------------+---------------+---------------+---------------+
		 |                    RESERVED                   |   peer_ID     |
   20	 |                                               |               |  23
		 +---------------+---------------+---------------+---------------+
		 |     MAC_1     |    MAC_2      |    MAC_3      |   MAC_4       |
   24	 |               |               |               |               |  27
		 +---------------+---------------+---------------+---------------+
		 |     MAC_5     |    MAC_6      |             VLAN              |
   28	 |               |               |                               |  31
		 +---------------+---------------+---------------+---------------+
		 |                           RESERVED                            |
   32	 |                                                               |  35
		 +---------------+---------------+---------------+---------------+

  */



static void build_txhdr(struct ulh_ebcom_conn *co, struct ulh_ebcom_addr *dst)
{
	memset(&co->saddr, 0 , sizeof(co->saddr));
	memset(&co->txhdr, 0 , sizeof(co->txhdr));

	memcpy(co->saddr.sll_addr, dst->mac, 6);
	memcpy(co->txhdr.ethdr.dst, dst->mac, 6);

    co->saddr.sll_family    = AF_PACKET;
    co->saddr.sll_halen     = ETHER_ADDR_LEN;
    co->saddr.sll_protocol  = htons(ETH_P_ALL);

	//co->txhdr.ethdr.type 	= htons(ULH_ETHERTYPE_802_1Q);
	co->txhdr.ethdr.proto 	= htons(ULH_ETHERTYPE_EBCOM);
	//co->txhdr.ethdr.tci 	= htons(ULH_EBCOM_VLAN);

	ebcom_set_rev (co->txhdr.mphdr, 0x0);
	ebcom_set_prim(co->txhdr.mphdr, 0x1); 	/* Prim Type 1 			*/
	ebcom_set_ctrl(co->txhdr.mphdr, 0x8); 	/* MP 					*/
	ebcom_set_size(co->txhdr.mphdr, 0x0); 	/* updated during TX 	*/
	ebcom_set_seq (co->txhdr.mphdr, 0x0);
	ebcom_set_vlan(co->txhdr.mphdr, 0xA);
	ebcom_set_cep(co->txhdr.mphdr, dst->lcep, dst->qix);
}

static void set_src_addr(struct ulh_ebcom *ebcom, struct ulh_ebcom_conn *co)
{
	if (ebcom->running) {
		memcpy(co->txhdr.ethdr.src, ebcom->own_mac, 6);
		memcpy(co->txhdr.mphdr +24, ebcom->own_mac, 6);
		co->saddr.sll_ifindex = ebcom->ifindex;
	}
}

static void ebcom_tbuff_free(struct ulh_ref *ref)
{
	struct ulh_tbuff_rbuf *dbuf = container_of(ref,
			struct ulh_tbuff_rbuf, ref);
	free(dbuf);
}

static int ebcom_alloc_buff(void *ebcom_r, void *conn_ref,
							uint32_t size, struct ulh_tbuff *tbuff)
{
	struct ulh_tbuff_rbuf *dbuf;
	(void)ebcom_r;
	(void)conn_ref;

	dbuf = malloc(sizeof(*dbuf) + size + ULH_EBCOM_HDRSIZE);
	if (!dbuf)
		return -ENOMEM;
	ulh_init_ref(&dbuf->ref, 1, ebcom_tbuff_free);

	dbuf->buf = (uint8_t *)(dbuf + 1);
	tbuff->rbuf = dbuf;
	tbuff->data = dbuf->buf + ULH_EBCOM_HDRSIZE;
	tbuff->size = size;
	return 0;
}

static int open_device(struct ulh_ebcom *ebcom)
{
    const char ifname[] = "vei0.10";
	struct sockaddr_ll saddr;
    struct ifreq ifr;
	int rc;
    
    if ((ebcom->sd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0)
        return -1;

    strncpy(ifr.ifr_name, ifname, sizeof(ifr.ifr_name));
    if ((rc = ioctl(ebcom->sd, SIOCGIFINDEX, &ifr)) < 0)
        goto err_out;

    saddr.sll_ifindex 	= ifr.ifr_ifindex;
    saddr.sll_family    = AF_PACKET;
    saddr.sll_halen     = ETHER_ADDR_LEN;
    saddr.sll_protocol  = htons(ETH_P_ALL);

    if ((rc = ioctl(ebcom->sd, SIOCGIFHWADDR, &ifr)) < 0)
        goto err_out;

	if (ifr.ifr_hwaddr.sa_family != ARPHRD_ETHER)
		goto err_out;

	ebcom->ifindex = saddr.sll_ifindex;
	memcpy(ebcom->own_mac, ifr.ifr_hwaddr.sa_data, 6);
	
    if (!bind(ebcom->sd, (const struct sockaddr *)&saddr, sizeof(saddr)))
        return 0;

  err_out:
    close(ebcom->sd);
    return rc;
}

/*
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
*/

static void *ebcom_rx_thread(void *param)
{
	struct ulh_ebcom *ebcom = param;
	struct ulh_ebcom_conn *conn;
	struct ulh_tbuff tb;
	itc_mbox_id_t me;
	int ret = -1;
	int cnt;
	
	me = itc_create_mailbox("ebcom_rx_thread", 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "%s mbox failure. Terminating thread", __func__);
		pthread_exit(0);
	}

	for (cnt = 0; ret; cnt++) {
		if ((ret = open_device(ebcom))) {
			if ((cnt <= 600) && ((cnt % 60) == 0)) {
				ULH_TRACE_INFO("Open vei0 failed, %d", errno);
			}
			sleep(1);
		}
	}

	ULH_TRACE_INFO("%s - Starting to recv from vei0", __func__);

	pthread_mutex_lock(&ebcom->lock);
	ebcom->running = 1;
	dl_list_foreach(conn, &ebcom->conns, link)
		set_src_addr(ebcom, conn);
	pthread_mutex_unlock(&ebcom->lock);
	
	for (; ebcom->running;) {
		if (ebcom_alloc_buff(0, 0, ULH_EBCOM_RMTU, &tb)) {
			sleep(1);
			continue;
		}

		ret = recvfrom(ebcom->sd, tb.data, tb.size, 0, NULL, NULL);

		if (ret > ULH_EBCOM_HDRSIZE) {
			struct ulh_ebcom_hdr *pkt = (struct ulh_ebcom_hdr *)tb.data;
			uint32_t cep, cid = ULH_TRANS_NOCONN;
			uint16_t proto = ntohs(pkt->ethdr.proto);
			uint8_t *ebh = (uint8_t*)&pkt->mphdr;
			int hlen = sizeof(*pkt);

			proto = ntohs(pkt->ethdr.proto);
			if (proto == ULH_ETHERTYPE_802_1Q) {
				uint16_t *proto_p = (uint16_t *)&pkt->mphdr;
				hlen += 4;
				ebh  += 4;
				proto = ntohs(proto_p[1]);
			}

			if (proto != ULH_ETHERTYPE_EBCOM) {	
				ULH_TRACE_ERROR("unexpected Ethertype:0x%x\n",pkt->ethdr.proto);
				ulh_tbuff_free(&tb);
				continue;
			}

			if (memcmp(pkt->ethdr.dst, ebcom->own_mac, 6)) {
				ULH_TRACE_INFO( "%s: %02x:%02x:%02x:%02x:%02x:%02x\n",
                     "Rx-packet sent to unknown MAC:",
                    pkt->ethdr.dst[0], pkt->ethdr.dst[1], pkt->ethdr.dst[2], 
					pkt->ethdr.dst[3], pkt->ethdr.dst[4], pkt->ethdr.dst[5]);
				ulh_tbuff_free(&tb);
				continue;
			}

			cep = ebcom_get_cep(ebh);
			ULH_TRACE(ebcom_cep, "RX", cep, 0);

			pthread_mutex_lock(&ebcom->lock);
			dl_list_foreach(conn, &ebcom->conns, link) {
				if (conn->cep == cep) {
					cid = conn->cid;
					break;
				}
			}

			if (cid != ULH_TRANS_NOCONN) {
				tb.size = ret;
				ulh_tbuff_pop(&tb, hlen);
				ulh_trans_deliver(cid, &tb);
			} else
				ulh_tbuff_free(&tb);

			pthread_mutex_unlock(&ebcom->lock);
		} else {
			ULH_TRACE_ERROR("too short Ethernet frame: %d\n", ret);
			ulh_tbuff_free(&tb);
		}
	}

	pthread_exit(0);
}


static int ebcom_create_conn(void *ebcom_r, uint32_t cid,
		struct ulh_trans_addr *src, struct ulh_trans_addr *dst,
		void **conn_ref)
{
	struct ulh_ebcom_addr *ea_src = (struct ulh_ebcom_addr *)src->data;
	struct ulh_ebcom_addr *ea_dst = (struct ulh_ebcom_addr *)dst->data;
	struct ulh_ebcom *ebcom = ebcom_r;
	struct ulh_ebcom_conn *co;

	co = malloc(sizeof(*co));
	if (!co)
		return -ENOMEM;


	build_txhdr(co, ea_dst); 

	co->cid = cid;
	co->cep = ea_src->lcep;
	co->mtu = ULH_EBCOM_RMTU - (ULH_EBCOM_HDRSIZE + 4);

	pthread_mutex_lock(&ebcom->lock);
	set_src_addr(ebcom, co);
	dl_list_insert_tail(&ebcom->conns, &co->link);
	pthread_mutex_unlock(&ebcom->lock);

	*conn_ref = co;
	return 0;
}

static int ebcom_destroy_conn(void *ebcom_r, void *conn_ref)
{
	struct ulh_ebcom_conn *co = conn_ref;
	struct ulh_ebcom *ebcom = ebcom_r;

	pthread_mutex_lock(&ebcom->lock);
	dl_list_remove(&co->link);
	pthread_mutex_unlock(&ebcom->lock);
	free(co);
	return 0;
}

static int ebcom_transmit(void *ebcom_r, void *conn_ref,
		struct ulh_tbuff *tb)
{
	struct ulh_ebcom_conn *co = conn_ref;
	struct ulh_ebcom *ebcom = ebcom_r;
	struct ulh_ebcom_hdr *hdr;
	int tx_len;

	ulh_tbuff_push(tb, sizeof(co->txhdr));
	hdr = (struct ulh_ebcom_hdr *)tb->data;
	memcpy(hdr, &co->txhdr, sizeof(*hdr));
	ebcom_set_size(hdr->mphdr,tb->size - sizeof(hdr->ethdr));

#ifdef LTTNG
	{
		struct ulh_ether_hdr *ehdr = (struct ulh_ether_hdr*)&co->txhdr.ethdr;
        uint32_t cep = ebcom_get_cep(co->txhdr.mphdr);

		ULH_TRACE(eth_hdr, "TX", ehdr->dst, ehdr->src, ntohs(ehdr->proto));
        ULH_TRACE(ebcom_cep, "TX", cep, 0);
	}
#endif

    tx_len = sendto(ebcom->sd, hdr, tb->size, 0,
					(struct sockaddr*)&co->saddr, sizeof(co->saddr));
	if (tx_len != tb->size) {
		ULH_TRACE_ERROR("sendto failed, (errno: %d) (tx: %d actual: %d)\n",
							errno, tb->size, tx_len);
	}

	ulh_tbuff_pop(tb, sizeof(co->txhdr));
	ulh_tbuff_free(tb);
	return 0;
}

static int ebcom_getmtu(void *ebcom_r, void *conn_ref)
{
	struct ulh_ebcom_conn *co = conn_ref;
	return co->mtu;
}

static void ebcom_destroy(void *ebcom_r)
{
	struct ulh_ebcom *ebcom = ebcom_r;

	if (!ebcom)
		return;

	ebcom->running = 0;
	pthread_join(ebcom->rx_thread, NULL);
	free(ebcom);
}

static struct ulh_trans_ops ebcom_ops = {
	.create_conn 	= ebcom_create_conn,
	.destroy_conn 	= ebcom_destroy_conn,
	.getmtu 		= ebcom_getmtu,
	.transmit 		= ebcom_transmit,
	.destroy 		= ebcom_destroy,
	.alloc_buff 	= ebcom_alloc_buff
};

/*******************************************************************************
**  globals
*******************************************************************************/
int ulh_ebcom_init(const char *name)
{
	struct ulh_ebcom *ebcom = malloc(sizeof(*ebcom));
	int ret;
	pthread_mutexattr_t attr;

	if (!ebcom)
		return -ENOMEM;

	strncpy(ebcom->name, name, sizeof(ebcom->name));
	dl_list_init(&ebcom->conns);

	ret = pthread_mutexattr_init(&attr);
	if(ret != 0) {
		ULH_TRACE_ERROR("pthread_mutexattr_init to failed, ret=%d %d\n",
				ret, errno);
		goto fail;
	}

	ret = pthread_mutexattr_settype(&attr,
					PTHREAD_MUTEX_ADAPTIVE_NP);
	if(ret != 0) {
		ULH_TRACE_ERROR("pthread_mutex_extattr_settype to ADAPTIVE failed, ret=%d %d\n",
				ret, errno);
	}

	ret = pthread_mutex_init(&ebcom->lock, &attr);
	if(ret != 0) {
		ULH_TRACE_ERROR("pthread_mutex_init to failed, ret=%d %d\n",
				ret, errno);
		goto fail;
	}

	if (ulh_trans_register(name, &ebcom_ops, ebcom)) {
		ULH_TRACE_ERROR("ulh_trans_register failed, %d\n", errno);
		goto fail;
	}

	if (pthread_create(&ebcom->rx_thread, NULL, ebcom_rx_thread, ebcom) == 0)
		return 0;

	ULH_TRACE_ERROR("pthread_create failed, %d\n", errno);
	ulh_trans_unregister(name);

fail:

	free(ebcom);
	return -EFAULT; 
}

int ulh_ebcom_shutdown(void)
{
	return 0;
}

int ulh_ebcom_makeaddr(uint8_t *mac,  uint32_t cep,
                       struct ulh_trans_addr *addr)
{
	struct ulh_ebcom_addr *ea = (struct ulh_ebcom_addr *)addr->data;
		
	if (cep >= 0x1000000)
		return -1;

	memset(addr, 0, sizeof(*addr));
	addr->type = ULH_EBCOM_ATYPE;
	addr->len = sizeof(*ea);
	
	memcpy(ea->mac, mac, 6);
	ea->lcep = cep >> 4;
	ea->qix = cep & 0xf;
	return 0;
}

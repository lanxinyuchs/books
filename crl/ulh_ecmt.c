/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file ulh_eth.c
 *
 *   [Enter a longer description about the file. More than one line
 *   can be used, but leave an empty line before the copyright line.]
 *
 *   [All text within [...] should be changed or removed.]
 *
 *   Copyright (C) 2010 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2014-10-03 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : First version.
 *
 *   Revised : 2015-11-26 Madhusudan Veladri
 *   Change  : Protected pthread_cond_signal with mutex lock
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>

#include <sys/socket.h>
#include <sys/select.h>
#include <netpacket/packet.h>
#include <net/if.h>
#include <net/if_arp.h>
#include <sys/ioctl.h>
#include <linux/sockios.h>

#include <errno.h>

#include <itc.h>

#include "ulh_transport.h"

#include "ulh_trace.h"
#ifdef LTTNG
#include "ulh_lttng.h"
#include "../libitclnh/ulh_rlnh.h"
#endif

#include "ecm_proto.h"
#include "ecm-link-api.h"
#include "ulh_ecmt.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define ULH_ECMT_ATYPE   	0xEC
#define ULH_ECMT_RMTU		1600

#define ULH_ETHERTYPE_802_1Q	0x8100

#define MAXINFO 256

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
struct ulh_ether_hdr {
	uint8_t dst[ETH_ALEN];
	uint8_t src[ETH_ALEN];
	uint16_t proto;
};

struct ulh_ether_q_hdr {
	uint16_t tci;
	uint16_t proto;
};

/* This struct has to fit within the struct ulh_trans_addr data field
   which has a size of ULH_TRANS_MAX_ADDR_LEN. The size of the devname
   that we can support is then ULH_TRANS_MAX_ADDR_LEN - the rest of the
   struct fields. This is hardcoded in now but should be improved to use
   some sort of sizeof preprocessor constructs. */
struct ulh_ecmt_addr {
	uint8_t mac[ETH_ALEN];
	uint16_t vlan;
	char devname[ULH_TRANS_MAX_ADDR_LEN - ETH_ALEN - 2];
};

struct ulh_ecmt_device {
	struct dl_list  link;
	char           *devname;
#ifndef PER_CONN_TXFD
        int             txfd;
#endif
	int             rxfd;
	int             ifindex;
	int             mtu;

	struct dl_list conns;
};

struct ulh_ecmt_conn {
	struct dl_list          link;
	uint8_t                 dst[ETH_ALEN];
	uint16_t                vlan;
	uint32_t                cid;
#ifdef PER_CONN_TXFD
	int                     txfd;
#endif
	struct ulh_ecmt_device *dev;

	/* pre-build header */
	uint32_t                tx_hdrsiz;
	uint8_t                 tx_hdr[64];
};

struct ulh_ecmt {
	pthread_t       rx_thread;
	itc_mbox_id_t   rx_mbox;
	int             fd;
	int             rx_shutdown;
	int             rx_started;
	pthread_mutex_t rx_lock;
	pthread_cond_t  rx_wait;

	struct dl_list  devices;
	char           *name;
	uint32_t        did;
};

#define ULH_ECMT_CONN_CTRL_MSG	0xebc0c
struct ulh_ecmt_conn_ctrl_msg {
	uint32_t msg_no;
	int add;
	struct ulh_ecmt_conn *co;
	/* local MAC and VLAN */
	int *complete;
	pthread_mutex_t *lock;
	pthread_cond_t *wait;
};

union itc_msg {
	uint32_t msg_no;
	struct ulh_ecmt_conn_ctrl_msg ctrl;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   [Enter a brief one-line function comment and leave next line empty.]
 *
 *   @param [param1    Description of param1. Use a hyphen (-) as param1
 *                     (and no description) if no parameters are used]
 *   [@param param2    Description of param2]
 *
 *   @return           [Comment the return of the function. Use a hyphen
 *                     (-) if no return value is used]
 *
 *   @par Globals:
 *                     [List of all globals variables that are used by
 *                     this function. Use two hyphens (--) if no global
 *                     variables are used]
 *
 *   [A longer multi-line and multi-sentence description of the function
 *   can be entered here. This way of tagging the comments is based
 *   on the function doxygen - A documentation system for generating
 *   descriptions from source code.]
 *
 *   [The short-line description is required, but the longer description
 *   is optional for simple functions. The intended audience is advanced,
 *   and no novice programmers.]
 *
 *   [Inside functions, use comments to structure the code and explain
 *   the meaning of instructions or groups of instructions. Do not
 *   explain what the code does, but what your intentions are and what
 *   the code (or code section) does to achieve that goal. Do not
 *   comment things that are obvious to anyone reading the source
 *   code.]
 *
 *   [Nevertheless: "When in doubt, comment it!"].
 */
/* ===================================================================== */
#ifdef LTTNG
void ulh_trace_error(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_ulh_ecm, ulh_error, file, line, buffer);
}

void ulh_trace_info(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_ulh_ecm, ulh_info, file, line, buffer);
}
#endif

static void __ether_construct_hdr(uint8_t  *buf,
				  uint32_t *size,
				  uint8_t  *dst,
				  uint8_t  *src,
				  uint16_t  vlan)
{
	struct ulh_ether_hdr *ehdr;
	struct ulh_ether_q_hdr *qhdr;

	ehdr = (struct ulh_ether_hdr *) buf;
	memcpy(ehdr->dst, dst, 6);
	memcpy(ehdr->src, src, 6);
	if (vlan == ULH_ECM_NOVLAN) {
		ehdr->proto = htons(ECM_ETHTYPE);
		*size = sizeof(struct ulh_ether_hdr);
	} else {
		ehdr->proto = htons(ULH_ETHERTYPE_802_1Q);
		qhdr = (struct ulh_ether_q_hdr *) (ehdr + 1);
		qhdr->proto = htons(ECM_ETHTYPE);
		qhdr->tci = htons(vlan);
		*size = sizeof(struct ulh_ether_hdr) +
			sizeof(struct ulh_ether_q_hdr);
	}
}

static struct ulh_ecmt_device *__ecmt_get_dev(struct ulh_ecmt *ecmt,
                                              char *dev)
{
	struct ulh_ecmt_device *it;

	dl_list_foreach(it, &ecmt->devices, link) {
		if (strcmp(it->devname, dev) == 0)
			return it;
	}

	return NULL;
}

static struct ulh_ecmt_conn *__ecmt_get_conn_by_dst(struct ulh_ecmt_device *dev,
						      uint8_t *mac, uint16_t vlan)
{
	struct ulh_ecmt_conn *it;

	dl_list_foreach(it, &dev->conns, link) {
		if ((it->vlan == vlan) &&
		    (memcmp(mac, it->dst, ETH_ALEN) == 0))
			return it;
	}

	return NULL;
}

static int open_rx_socket(char    *devname,
			  int     *ifindex,
			  int     *mtu)
{
	struct sockaddr_ll sa;
	struct ifreq ifr;
	int sd, flags, res;
        int sock_buf;
        socklen_t optlen;

        sd = socket(PF_PACKET, SOCK_RAW, htons(ECM_ETHTYPE));
	if(sd == -1) {
		ULH_TRACE_ERROR("socket call failed: %d(%s)",
				errno, strerror(errno));
		return -1;
	}

        /* Increment the receive socket buffer size from the default 208KB
         * to 10MB. This will let receive socket hold more no of incoming
         * packets before link handler thread serves.
         */
        sock_buf = 10 * 1024 * 1024;
        if (setsockopt(sd, SOL_SOCKET, SO_RCVBUF, &sock_buf, sizeof(sock_buf))) {
                ULH_TRACE_ERROR("setsockopt return error: %d(%s)",
                                errno, strerror(errno));
        }

        optlen = sizeof(sock_buf);
        res = getsockopt(sd, SOL_SOCKET, SO_RCVBUF, &sock_buf, &optlen);
        if (res) {
                ULH_TRACE_ERROR("getsockopt return error: %d(%s)",
                                errno, strerror(errno));
        }
        else {
                ULH_TRACE_INFO("RX SOCKET RCV BUF size : %d",sock_buf);
        }

        strcpy(ifr.ifr_name, devname);
        if(ioctl(sd, SIOCGIFMTU, &ifr) == -1) {
           ULH_TRACE_ERROR("ioctl SIOCGIFMTU failed: %d(%s)",
                 errno, strerror(errno));
           return -1;
        }
	*mtu = ifr.ifr_mtu;

	if(ioctl(sd, SIOCGIFINDEX, &ifr) == -1) {
		ULH_TRACE_ERROR("ioctl SIOCGIFINDEX failed: %d(%s)",
				errno, strerror(errno));
		return -1;
	}
	*ifindex = ifr.ifr_ifindex;

	/*RAW communication*/
	memset(&sa, 0, sizeof(struct sockaddr_ll));
	sa.sll_family   = PF_PACKET;
	sa.sll_protocol = htons(ECM_ETHTYPE);
	sa.sll_ifindex  = *ifindex;

	if(bind(sd, (struct sockaddr *)&sa, sizeof(sa)) == -1) {
		ULH_TRACE_ERROR("bind failed: %d(%s)",
				errno, strerror(errno));
		return -1;
	}

        flags = fcntl(sd, F_GETFL);
        fcntl(sd, F_SETFL, flags | O_NONBLOCK);
	return sd;
}

static void rx_ctrlmsg(struct ulh_ecmt *ecmt)
{
	union itc_msg *msg;
	struct ulh_ecmt_device *dev;
	struct ulh_ecmt_conn *co;

	msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
	if(msg == NULL) {
		return;
	} else if (msg->msg_no != ULH_ECMT_CONN_CTRL_MSG) {
		ULH_TRACE_ERROR("unexpected message 0x%x\n",
				msg->msg_no);
		itc_free(&msg);
		return;
	}

	co = msg->ctrl.co;
	dev = co->dev;

	ULH_TRACE(ecmt_rx_ctrlmsg, dev->devname, msg->ctrl.add,
		  co->dst, co->vlan, co->cid);

	/* remove? */
	if (!msg->ctrl.add) { 

#ifdef PER_CONN_TXFD
                if((co->txfd != -1) &&
			   (close(co->txfd) == -1)) {
				ULH_TRACE_ERROR("shutdown :close failed: %d(%s)",
						errno, strerror(errno));
               }
               co->txfd = -1;
#endif
		dl_list_remove(&msg->ctrl.co->link);
		if(dl_list_empty(&dev->conns)) {
			dl_list_remove(&dev->link);
			if((dev->rxfd != -1) &&
			    (close(dev->rxfd) == -1)) {
				ULH_TRACE_ERROR("close failed: %d(%s)",
						errno, strerror(errno));
			}
#ifndef PER_CONN_TXFD
                        if((dev->txfd != -1) &&
			    (close(dev->txfd) == -1)) {
				ULH_TRACE_ERROR("close failed: %d(%s)",
						errno, strerror(errno));
			}
			dev->txfd = -1;
#endif
			dev->rxfd = -1;
		}

		pthread_mutex_lock(msg->ctrl.lock);
		*msg->ctrl.complete = 1;
		pthread_cond_signal(msg->ctrl.wait);
		pthread_mutex_unlock(msg->ctrl.lock);
		itc_free(&msg);
		return;
	}
           

	if(dev->rxfd == -1) {
		dev->rxfd = open_rx_socket(dev->devname, &dev->ifindex,
				    &dev->mtu);
		if(dev->rxfd == -1) {
			pthread_cond_signal(msg->ctrl.wait);
			itc_free(&msg);
			free(dev);
			return;
		}
		dl_list_insert_tail(&ecmt->devices, &dev->link);
	}

        dl_list_insert_tail(&dev->conns, &co->link);
	pthread_mutex_lock(msg->ctrl.lock);
	*msg->ctrl.complete = 1;
	if(pthread_cond_signal(msg->ctrl.wait) != 0) {
		ULH_TRACE_ERROR("pthread_cond_signal failed: %d(%s)",
				errno, strerror(errno));
		pthread_mutex_unlock(msg->ctrl.lock);
		itc_free(&msg);
		return;
	}
	pthread_mutex_unlock(msg->ctrl.lock);

	itc_free(&msg);

	return;
}

static void ecmt_rx_packet(struct ulh_ecmt *ecmt,
			   struct ulh_ecmt_device *dev)
{
	struct ulh_tbuff         tb;
	struct ulh_ether_hdr    *ehdr;
	struct ulh_ether_q_hdr  *qhdr;
        struct ulh_ecmt_conn    *co;
        int                      pkt_size, size;
	uint16_t                 vlan = ULH_ECM_NOVLAN;
	uint16_t                 proto;
        uint32_t                 msgno, *hdr;

	while (1) {
	        ulh_tbuff_alloc_def(&tb, ULH_ECMT_RMTU);
		pkt_size = recv(dev->rxfd, tb.data, ULH_ECMT_RMTU, 0);
		if (pkt_size == -1) {
			/* No packet returned or error! */
			ulh_tbuff_free(&tb);
			if ((errno == EAGAIN) || (errno == EWOULDBLOCK)) {
				/* No packet returned or error! */
				return;
			}
			ULH_TRACE_ERROR("Recv errno: %d", -errno);
			return;
		}

		if(pkt_size < sizeof(struct ulh_ether_hdr)) {
			ULH_TRACE_ERROR("To small packet received: %d", pkt_size);
			ulh_tbuff_free(&tb);
			return;
		}

		/* Set the tbuff to the actual packet size. */
		tb.size = pkt_size;

		ehdr = (struct ulh_ether_hdr *) ulh_tbuff_get(&tb);
		size = pkt_size - sizeof(struct ulh_ether_hdr);
		proto = ntohs(ehdr->proto);
		if (proto == ULH_ETHERTYPE_802_1Q) {
			qhdr = (struct ulh_ether_q_hdr *) (ehdr + 1);
			proto = ntohs(qhdr->proto);
			vlan = ntohs(qhdr->tci) & 0x0fff;
			ulh_tbuff_pop(&tb, (sizeof(struct ulh_ether_hdr) +
				      sizeof(struct ulh_ether_q_hdr)));
			size -= sizeof(struct ulh_ether_q_hdr);
	                ULH_TRACE(eth_vlan_hdr, "ECMT RX", dev->devname, ehdr->dst,
				  ehdr->src, ehdr->proto, qhdr->tci, qhdr->proto);
		} else {
			vlan = ULH_ECM_NOVLAN;
			ulh_tbuff_pop(&tb, sizeof(struct ulh_ether_hdr));
	                ULH_TRACE(eth_hdr, "ECMT RX", dev->devname, ehdr->dst,
				  ehdr->src, ehdr->proto);
	       }

		/* find connection */
		co = __ecmt_get_conn_by_dst(dev, ehdr->src, vlan);
		if(co == NULL) {
			ULH_TRACE(ecmt_packet_not_found, dev->devname, ehdr->src, vlan);
			ulh_tbuff_free(&tb);
			return;
		}
                hdr = (uint32_t *)ulh_tbuff_get(&tb);
                if(ECM_GET_ACK_NEXT_FROM_MAIN(hdr) == ECM_UDATA) {
                        /* Getting msg no from hdr main *(uint32_t *)(hdr+5) */
                        msgno = ECM_GET_MSGNO_FROM_MAIN(hdr);
                        ULH_TRACE(ulh_trace_sig, __FILE__, __LINE__, msgno, __func__);
                }

		ulh_trans_deliver(co->cid, &tb);
	}
}

static void *ecmt_rx_thread(void *param)
{
	struct ulh_ecmt        *ecmt = param;
	struct ulh_ecmt_device *it;
        fd_set                  fdset;
	int                     max_fd, res;

	ecmt->rx_mbox = itc_create_mailbox(ecmt->name, 0);
	ecmt->fd = itc_get_fd();

	pthread_mutex_lock(&ecmt->rx_lock);
	ecmt->rx_started = 1;
	pthread_cond_signal(&ecmt->rx_wait);
	pthread_mutex_unlock(&ecmt->rx_lock);

	while (!ecmt->rx_shutdown) {
                FD_ZERO(&fdset);
                FD_SET(ecmt->fd, &fdset);
                max_fd = ecmt->fd + 1;
		dl_list_foreach(it, &ecmt->devices, link) {
			FD_SET(it->rxfd, &fdset);
			max_fd = (it->rxfd >= max_fd) ? (it->rxfd + 1) : max_fd;
		}
		res = select(max_fd, &fdset, NULL, NULL, NULL);
		if(res == -1) {
			continue;
		}
		if(FD_ISSET(ecmt->fd, &fdset)) {
			rx_ctrlmsg(ecmt);
		}
		dl_list_foreach(it, &ecmt->devices, link) {
			if(FD_ISSET(it->rxfd, &fdset)) {
				ecmt_rx_packet(ecmt, it);
			}
		}
	}

	pthread_exit(0);
}

static int ecmt_create_conn(void *ecmt_r, uint32_t cid,
			    struct ulh_trans_addr *src, struct ulh_trans_addr *dst,
			    void **conn_ref)
{
	struct ulh_ecmt *ecmt = ecmt_r;
	struct ulh_ecmt_device *dev;
	struct ulh_ecmt_conn *co;
	struct ulh_ecmt_addr *ea_src, *ea_dst;

	if (src->type != ULH_ECMT_ATYPE ||
	    dst->type != ULH_ECMT_ATYPE)
		return -EINVAL;

	ea_src = (struct ulh_ecmt_addr *) src->data;
	ea_dst = (struct ulh_ecmt_addr *) dst->data;

	if (ea_src->vlan != ea_dst->vlan)
		return -EINVAL;

	if(strcmp(ea_src->devname, ea_dst->devname) != 0)
		return -EINVAL;

	dev = __ecmt_get_dev(ecmt, ea_src->devname);
	if(!dev) {
		dev = malloc(sizeof(*dev));
		if(!dev)
			return -ENOMEM;
		memset(dev, 0, sizeof(*dev));

		dev->devname = strdup(ea_src->devname);
		if(!dev->devname) {
			free(dev);
			return -ENOMEM;
		}
#ifndef PER_CONN_TXFD
                dev->txfd = -1;
#endif
		dev->rxfd = -1;

		dl_list_init(&dev->conns);
		dl_list_init(&dev->link);
	}

	co = malloc(sizeof(*co));
	if (!co)
		return -ENOMEM;

	/* construct TX header */
	__ether_construct_hdr(co->tx_hdr, &co->tx_hdrsiz,
			      ea_dst->mac, ea_src->mac, ea_dst->vlan);
	memcpy(co->dst, ea_dst->mac, ETH_ALEN);
	co->vlan  = ea_dst->vlan;
	co->cid   = cid;
	co->dev   = dev;
#ifdef PER_CONN_TXFD
        co->txfd = -1;
#endif

#ifdef LTTNG
        if(co->tx_hdrsiz) {
                struct ulh_ether_hdr * ehdr = (struct ulh_ether_hdr *)co->tx_hdr;
                struct ulh_ether_q_hdr *qhdr = (struct ulh_ether_q_hdr *)
                                                (co->tx_hdr +
                                                 sizeof(struct ulh_ether_hdr));

                ULH_TRACE(eth_vlan_hdr, "ECMT create VLAN HDR", dev->devname,
                          ehdr->dst, ehdr->src, ehdr->proto,
                          qhdr->tci, qhdr->proto);
        } else {
                struct ulh_ether_hdr * ehdr = (struct ulh_ether_hdr *)co->tx_hdr;
                ULH_TRACE(eth_hdr, "ECMT create tx HDR", dev->devname,
                          ehdr->dst, ehdr->src, ehdr->proto);
        }
#endif

	*conn_ref = co;

	return 0;
}

static int ecmt_destroy_conn(void *ecmt_r, void *conn_ref)
{
	struct ulh_ecmt_conn *co = conn_ref;
	struct ulh_ecmt_device *dev = co->dev;

	if(dl_list_empty(&dev->conns)) {
		free(dev->devname);
		free(dev);
	}

	free(co);

	return 0;
}

static int ecmt_enable(void *ecmt_r, void *conn_ref)
{
	struct ulh_ecmt *ecmt = ecmt_r;
	struct ulh_ecmt_conn *co = conn_ref;
	union itc_msg *msg;
	pthread_mutex_t lock;
	pthread_cond_t wait;
	int complete = 0;

	pthread_mutex_init(&lock, NULL);
	pthread_cond_init(&wait, NULL);
	msg = itc_alloc(sizeof(struct ulh_ecmt_conn_ctrl_msg),
			ULH_ECMT_CONN_CTRL_MSG);
	msg->ctrl.add  = 1;
	msg->ctrl.co   = co;
	msg->ctrl.lock = &lock;
	msg->ctrl.wait = &wait;
	msg->ctrl.complete = &complete;
	itc_send(&msg, ecmt->rx_mbox, ITC_MY_MBOX);

	pthread_mutex_lock(&lock);
	while (!complete)
		pthread_cond_wait(&wait, &lock);
	pthread_mutex_unlock(&lock);

	return complete;
}

static int ecmt_disable(void *ecmt_r, void *conn_ref)
{
	struct ulh_ecmt *ecmt = ecmt_r;
	struct ulh_ecmt_conn *co = conn_ref;
	union itc_msg *msg;
	pthread_mutex_t lock;
	pthread_cond_t wait;
	int complete = 0;

	pthread_mutex_init(&lock, NULL);
	pthread_cond_init(&wait, NULL);
	msg = itc_alloc(sizeof(struct ulh_ecmt_conn_ctrl_msg),
			ULH_ECMT_CONN_CTRL_MSG);
	msg->ctrl.add = 0;
	msg->ctrl.co = co;
	msg->ctrl.lock = &lock;
	msg->ctrl.wait = &wait;
	msg->ctrl.complete = &complete;
	itc_send(&msg, ecmt->rx_mbox, ITC_MY_MBOX);

	pthread_mutex_lock(&lock);
	while (!complete)
		pthread_cond_wait(&wait, &lock);
	pthread_mutex_unlock(&lock);

	return 0;
}

static int ecmt_transmit(void *ecmt_r, void *conn_ref,
			 struct ulh_tbuff *buff)
{
	struct ulh_ecmt_conn *co = conn_ref;
	struct sockaddr_ll sa;
	uint8_t *txbuff;
	int txsize, res;
        int sock_buf;
        socklen_t optlen;
        uint32_t msgno, *hdr;
        uint32_t send_size;

#ifdef PER_CONN_TXFD
	if(co->txfd == -1) {
		co->txfd = socket(PF_PACKET, 
                                       SOCK_RAW, htons(ECM_ETHTYPE));
		if(co->txfd == -1) {
			ULH_TRACE_ERROR("socket call failed: %d(%s)",
					errno, strerror(errno));
			return -1;
		}
#else
        if(co->dev->txfd == -1) {
		co->dev->txfd = socket(PF_PACKET,
					SOCK_RAW, htons(ECM_ETHTYPE));
		if(co->dev->txfd == -1) {
			ULH_TRACE_ERROR("socket call failed: %d(%s)",
					errno, strerror(errno));
			return -1;
		}
#endif
                /* Increment the send socket buffer size from the default 208KB
                 * to 4MB.
                 */
                sock_buf = 4 * 1024 * 1024;
#ifdef PER_CONN_TXFD
                if (setsockopt(co->txfd, SOL_SOCKET, SO_SNDBUF, &sock_buf, sizeof(sock_buf))) {
#else 
                if (setsockopt(co->dev->txfd, SOL_SOCKET, SO_SNDBUF, &sock_buf, sizeof(sock_buf))) {
#endif
                        ULH_TRACE_ERROR("setsockopt return error: %d(%s)",
                                        errno, strerror(errno));
                }

                optlen = sizeof(sock_buf);
#ifdef PER_CONN_TXFD
                res = getsockopt(co->txfd, SOL_SOCKET, SO_SNDBUF, &sock_buf, &optlen);
#else
                res = getsockopt(co->dev->txfd, SOL_SOCKET, SO_SNDBUF, &sock_buf, &optlen);
#endif
                if (res) {
                        ULH_TRACE_ERROR("getsockopt return error: %d(%s)",
                                        errno, strerror(errno));
                }
                else {
                        ULH_TRACE_INFO("TX SOCKET SND BUF size : %d",sock_buf);
                }
	}

	if(ulh_tbuff_headroom(buff) < co->tx_hdrsiz) {
		ULH_TRACE_ERROR("Not enough headrom in buffer, "
				"headroom: %d  headersize: %d",
				ulh_tbuff_headroom(buff), co->tx_hdrsiz);
		ulh_tbuff_free(buff);
		return -1;
	}
	txbuff = ulh_tbuff_get(buff);
	txbuff -= co->tx_hdrsiz;
	memcpy(txbuff, co->tx_hdr, co->tx_hdrsiz);
	txsize = co->tx_hdrsiz + ulh_tbuff_len(buff);

#ifdef LTTNG
        if(co->tx_hdrsiz == sizeof(struct ulh_ether_hdr)) {
                struct ulh_ether_hdr * ehdr = (struct ulh_ether_hdr *)co->tx_hdr;
                ULH_TRACE(eth_hdr, "ECMT TX", co->dev->devname,
                          ehdr->dst, ehdr->src, ehdr->proto);
        } else {
                struct ulh_ether_hdr * ehdr = (struct ulh_ether_hdr *)co->tx_hdr;
                struct ulh_ether_q_hdr *qhdr = (struct ulh_ether_q_hdr *)
                                                (co->tx_hdr +
                                                 sizeof(struct ulh_ether_hdr));

                ULH_TRACE(eth_vlan_hdr, "ECMT TX VLAN", co->dev->devname,
                          ehdr->dst, ehdr->src, ehdr->proto,
                          qhdr->tci, qhdr->proto);
        }
#endif

        hdr = (uint32_t *) ulh_tbuff_get(buff);
        if(ECM_GET_ACK_NEXT_FROM_MAIN(hdr) == ECM_UDATA) {
                /* Getting msg no from hdr main *(uint32_t *)(hdr+5) */
                msgno = ECM_GET_MSGNO_FROM_MAIN(hdr);
                ULH_TRACE(ulh_trace_sig, __FILE__, __LINE__, msgno, __func__);
        }
        memset(&sa, 0, sizeof(sa));
        sa.sll_family   = AF_PACKET;
        sa.sll_ifindex  = co->dev->ifindex;
        sa.sll_halen    = ETH_ALEN;
        memcpy(sa.sll_addr, co->tx_hdr, ETH_ALEN);

#ifdef PER_CONN_TXFD
        if(sendto(co->txfd, txbuff, txsize,
                 0, (struct sockaddr *)&sa, sizeof(sa)) == -1) {
		/* Multi threaded ECM, the failed packet is successfully re-sent
                   after the socket is closed and re-opened.sendto failure is
                   not a ERROR in this case hence trace type is info */
                ULH_TRACE_INFO("sendto failed: %d(%s) len: %d",
                                 errno, strerror(errno), txsize);

                if (ioctl(co->txfd, SIOCOUTQ, &send_size) < 0) {
                        ULH_TRACE_ERROR("ioctl failed: %d(%s)",
                                        errno, strerror(errno));
                }
                else {
                        ULH_TRACE_INFO("Transmit socket current "
                                        "unsent data: %u", send_size);
                }
    
               if( close(co->txfd)) {
                        ULH_TRACE_ERROR("close failed: %d(%s)",
                                 errno, strerror(errno));

               }
               co->txfd = -1;
        }
#else

        if(sendto(co->dev->txfd, txbuff, txsize,
                 0, (struct sockaddr *)&sa, sizeof(sa)) == -1) {
		/* Multi threaded ECM, the failed packet is successfully re-sent
		  after the socket is closed and re-opened. sendto failure is
                  not a ERROR in this case hence trace type is info */
                ULH_TRACE_INFO("sendto failed: %d(%s) len: %d",
                                 errno, strerror(errno), txsize);

                if (ioctl(co->dev->txfd, SIOCOUTQ, &send_size) < 0) {
                        ULH_TRACE_ERROR("ioctl failed: %d(%s)",
                                        errno, strerror(errno));
                }
                else {
                        ULH_TRACE_INFO("Transmit socket current "
                                        "unsent data: %u", send_size);
                }
    
               if (close(co->dev->txfd)) {
                        ULH_TRACE_ERROR("close failed: %d(%s)",
                                        errno, strerror(errno));
               }
               co->dev->txfd = -1;
        }
#endif

        ulh_tbuff_free(buff);

        return 0;
}

static int ecmt_getmtu(void *ecmt_r, void *conn_ref)
{
	struct ulh_ecmt_conn *co = conn_ref;
	struct ifreq ifr;
	int mtu;
	int sd;

	sd = socket(PF_PACKET, SOCK_RAW, htons(ECM_ETHTYPE));
	if(sd == -1) {
		ULH_TRACE_ERROR("socket call failed: %d(%s)",
				errno, strerror(errno));
		return -1;
	}

	strcpy(ifr.ifr_name, co->dev->devname);
	if(ioctl(sd, SIOCGIFMTU, &ifr) == -1) {
		ULH_TRACE_ERROR("ioctl SIOCGIFMTU failed: %d(%s)",
				errno, strerror(errno));
		return -1;
	}
	mtu = ifr.ifr_mtu;

	if(close(sd) == -1) {
		ULH_TRACE_ERROR("close failed: %d(%s)",
				errno, strerror(errno));
		return -1;
	}

	return mtu;
}

static int ecmt_conn_info(void *ecmt_r, void *conn_ref,
			  ulh_trans_info lvl, char *text, int maxtextlen)
{
	struct ulh_ecmt_conn *co = conn_ref;
	char tmptext[MAXINFO];
	int avail, ret = 0;

	avail = (MAXINFO < maxtextlen) ? MAXINFO : maxtextlen;

	switch(lvl) {
	case TRANS_HEADING:
		ret = snprintf(tmptext, avail,
			       "devname    dst mac           vlan");
		break;
	case TRANS_SUMMARY:
		if(co->vlan == ULH_ECM_NOVLAN)
			ret = snprintf(tmptext, avail,
				       "%-10s %02x:%02x:%02x:%02x:%02x:%02x NOVLAN",
				       co->dev->devname, co->dst[0], co->dst[1], co->dst[2],
				       co->dst[3], co->dst[4], co->dst[5]);
		else
			ret = snprintf(tmptext, avail,
				       "%-10s %02x:%02x:%02x:%02x:%02x:%02x 0x%04x",
				       co->dev->devname, co->dst[0], co->dst[1], co->dst[2],
				       co->dst[3], co->dst[4], co->dst[5], co->vlan);
		break;
	case TRANS_DETAILED:
		if(co->vlan == ULH_ECM_NOVLAN)
			ret = snprintf(tmptext, avail,
				       "ecmt cid: %d\n"
				       "ecmt dev: %s\n"
				       "ecmt mac: 0x%02x:0x%02x:0x%02x:0x%02x:0x%02x:0x%02x\n",
				       co->cid, co->dev->devname, co->dst[0], co->dst[1], co->dst[2],
				       co->dst[3], co->dst[4], co->dst[5]);
		else
			ret = snprintf(tmptext, avail,
				       "ecmt cid: %d\n"
				       "ecmt dev: %s\n"
				       "ecmt mac: 0x%02x:0x%02x:0x%02x:0x%02x:0x%02x:0x%02x\n"
				       "ecmt vlan: 0x%04x\n",
				       co->cid, co->dev->devname, co->dst[0], co->dst[1], co->dst[2],
				       co->dst[3], co->dst[4], co->dst[5], co->vlan);
		break;
	default:
		return -EINVAL;
		break;
	}

	if(ret >= avail)
	    return -ERANGE;

	strcpy(text, tmptext);

	return 0;
}

static void ecmt_destroy(void *ecmt_r)
{
	struct ulh_ecmt *ecmt = ecmt_r;

	if (!ecmt)
		return;

	ULH_TRACE_ERROR("%s\n", __func__);

	ecmt->rx_shutdown = 1;
	pthread_join(ecmt->rx_thread, NULL);

	free(ecmt->name);
	free(ecmt);
}

static struct ulh_trans_ops ecmt_ops = {
	.create_conn = ecmt_create_conn,
	.destroy_conn = ecmt_destroy_conn,
	.enable = ecmt_enable,
	.disable = ecmt_disable,
	.getmtu = ecmt_getmtu,
	.transmit = ecmt_transmit,
	.conn_info = ecmt_conn_info,
	.destroy = ecmt_destroy,
};

int ulh_ecmt_create(const char *name)
{
	struct ulh_ecmt *ecmt;

	ecmt = malloc(sizeof(*ecmt));
	if (!ecmt)
		return -ENOMEM;

	ecmt->name = strdup(name);
	if (!ecmt->name) {
		free(ecmt);
		return -ENOMEM;
	}

	ecmt->rx_shutdown = 0;
	ecmt->rx_started = 0;
	pthread_mutex_init(&ecmt->rx_lock, NULL);
	pthread_cond_init(&ecmt->rx_wait, NULL);
	dl_list_init(&ecmt->devices);

        if (pthread_create(&ecmt->rx_thread, NULL, ecmt_rx_thread, ecmt))
                goto fail;

        /* Make it easier to find the ecmt receive thread when debugging. */
        if (pthread_setname_np(ecmt->rx_thread, "ecmt_rx") < 0) {
                ULH_TRACE_ERROR("pthread_setname_np failed:%d(%s)",
                                errno, strerror(errno));
        }

        if (ulh_trans_register(name, &ecmt_ops, ecmt)) {
                ecmt->rx_shutdown = 1;
                pthread_join(ecmt->rx_thread, NULL);
                goto fail;
        }
        pthread_mutex_lock(&ecmt->rx_lock);
        while (!ecmt->rx_started)
           pthread_cond_wait(&ecmt->rx_wait, &ecmt->rx_lock);
        pthread_mutex_unlock(&ecmt->rx_lock);

	if (ecmt->rx_started < 0)
		goto fail;

	return 0;

fail:
	if (ecmt) {
		if (ecmt->name)
			free(ecmt->name);
		free(ecmt);
	}

	return -EFAULT;
}

int ulh_ecmt_makeaddr(uint8_t *mac, int vlan, const char *devname,
		      struct ulh_trans_addr *addr)
{
	struct ulh_ecmt_addr *ea;

	/* See comment of how the ulh_ecmt_addr struct is
	   defined at the top of this file. */
	if(strlen(devname) >= (ULH_TRANS_MAX_ADDR_LEN - ETH_ALEN - 2))
		return -EINVAL;

	memset(addr, 0, sizeof(*addr));

	addr->type = ULH_ECMT_ATYPE;
	addr->len = sizeof(*ea);

	ea = (struct ulh_ecmt_addr *) addr->data;
	memcpy(ea->mac, mac, sizeof(uint8_t) * ETH_ALEN);
	if (vlan < 0)
		ea->vlan = ULH_ECM_NOVLAN;
	else if (vlan > 4096)
		return -1;
	else
		ea->vlan = (uint16_t) vlan;
	strcpy(ea->devname, devname);

	return 0;
}

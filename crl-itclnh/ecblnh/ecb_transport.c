/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#define _GNU_SOURCE /* pthread_setname_np() */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <syslog.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>
#include <itc.h>
#include <rhai-ser.h>

#include "atfi.h"
#include "a4ci.sig"
#include "ecb_transport.h"
#include "ecb_trace.h"



/* Frame Checksum constants and macro for CRC-16-CCITT. */
#define FCS_INIT    		0xffff
#define FCS_POLY 			0x8408
#define FCS_GOOD    		0xf0b8

#define FCS(fcs, ch) (crc16((uint16_t)fcs, (char)ch))


#define ENCAP(ptr, c)                                               \
  do {                                                              \
		if (((c) == CHAR_END) || ((c) == CHAR_ESC)) {				\
			*ptr++ = CHAR_ESC;										\
			*ptr++ = (c) ^ 0x20;									\
		} else {													\
			*ptr++ = (c);											\
		}															\
	} while (0)



union itc_msg {
	uint32_t						msgno;
	struct ulh_transmsg_data		a4ci;
};

struct ser_port *g_port;


static uint16_t crc16(uint16_t fcs, char ch)
{
	int j;
	uint16_t data = (uint16_t)ch;

	for (j = 0; j < 8; j++, data >>= 1) {
		if ((fcs ^ data) & 1)
			fcs = (fcs >> 1) ^ FCS_POLY;
		else
			fcs >>= 1;
	}
	return (fcs);
}

void print_data(char *data, int size)
{
	char ascii[128];
	uint32_t k,j, noelem = 16;

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

int check_frame(struct ulh_tbuff* tbuff)
{
    uint16_t fcs_cache[4], idx, fcs, crc;
    unsigned char *src, *dst;
    int length = tbuff->size - 1;
    int i = 0, j = 0;

	if (tbuff->size < ECB_HLEN)
		return 0;

    src = tbuff->data;
    dst = tbuff->data;

    for (fcs = FCS_INIT, idx = 0;  i < length; i++, j++) {
        if (src[i] != CHAR_ESC) {
            dst[j] = src[i];
        } else {
            dst[j] = src[++i] ^ 0x20;
        }

        fcs = FCS(fcs, dst[j]);
        fcs_cache[idx] = fcs;
        idx = (idx + 1) & 3;
    }

    /* strip off FCS */
    length = j - 2;
    tbuff->size = length;

    if (fcs == FCS_GOOD)
        return 1;

    /* Endianness issue */
    idx = (idx + 1) & 3;
    fcs = fcs_cache[idx] ^ 0xffff;
    if (memcmp(&fcs, &dst[length], 2) == 0)
        return 1;

    /* Defect FCU PPs issue */
    if (dst[0] != 0x2f)
        return 0;

    crc = ((uint16_t)dst[length] << 8) | ((uint16_t)dst[length + 1]);
    fcs = fcs_cache[idx];
    fcs = ((fcs << 8) | (fcs >> 8)) ^ 0xffff;

    if ((fcs | 0x2020) == (crc | 0x2020))
        return 1;

    return 0;
}

int ecb_transmit(struct ser_port *p, uint8_t addr, uint8_t ctrl,
											const void* buf, int len)
{
	uint8_t buffer[512], *dst = buffer;
    const uint8_t *src = buf;
    uint16_t fcs = FCS_INIT;

	p = p ? : g_port;
    *dst++ = CHAR_END;

    ENCAP(dst, addr);
    ENCAP(dst, ctrl);

    fcs = FCS(fcs, addr);
    fcs = FCS(fcs, ctrl);

    while (len--) {
        fcs = FCS(fcs,*src);
        ENCAP(dst,*src);
        src++;
    }

    fcs ^= 0xffff;
    ENCAP(dst,(uint8_t)(fcs & 0xff));
    ENCAP(dst,(uint8_t)(fcs >> 8));
    *dst++ = CHAR_END;

    len = (int)(dst - buffer);

	ATOMIC_ADD(&p->tx_frames, 1);
	ATOMIC_ADD(&p->tx_octets, len);

	A4C_DEBUG("++ TX [A4CI]", buffer, len);
	return rhai_ser_write(p->handle, buffer, len);
}

static int
ecb_create_conn(void *tref, uint32_t tcid, struct ulh_trans_addr *src,
		struct ulh_trans_addr *dst, void **cref)
{
	(void)tcid;
	(void)src;
	(void)dst;

	*cref = tref;
	return 0;
}

static int ecb_getmtu(void *param, void *conn_ref)
{
	(void)param;
	(void)conn_ref;
	return ECB_MTU;
}

static void ecb_free_buf(struct ulh_ref *ref)
{
	struct ulh_tbuff_rbuf *dbuf = container_of(ref,
			struct ulh_tbuff_rbuf, ref);
	free(dbuf);
}

static int 
ecb_alloc_buf(void *tr, void *cr, uint32_t size, struct ulh_tbuff *tbuff)
{
	struct ulh_tbuff_rbuf *dbuf;

	dbuf = malloc(sizeof(*dbuf) + size);
	if (!dbuf)
		return -ENOMEM;
	ulh_init_ref(&dbuf->ref, 1, ecb_free_buf);

	dbuf->buf = (uint8_t*)(dbuf + 1);
	tbuff->rbuf = dbuf;
	tbuff->data = dbuf->buf;
	tbuff->size = size;
	return 0;
}

#define HDLC_PF					0x10
#define HDLC_DM					0x0F

static void send_dm_response(uint8_t addr)
{
	if (check_for_dm(addr))
		ecb_transmit(NULL, addr, HDLC_DM | HDLC_PF, NULL, 0);
}


static int ecb_get_frame(struct ser_port *ser, void* _buf, int len)
{
	uint8_t *src, *buf = _buf, *dst = buf;
	int rlen, length = 0;

	if (ser->len) {
		rlen = ser->len < len ? ser->len : len;
		for (src = ser->data; rlen; rlen--, ser->len--) {
			if ((*dst++ = *src++) == CHAR_END) {
				ser->data = src;
				ser->len--;
				return (int)(dst - buf);
			}
		}
		if (ser->len) {
			ser->data += len;
			return len;
		}
	}
	for (rlen = len - (int)(dst - buf); rlen;) {
		if ((length = rhai_ser_read(ser->handle, dst, rlen, 100)) < 0)
			continue;

		A4C_DEBUG("-- RX [RAW ]", dst, length);

		for (rlen -= length; length; length--) {
			if (*dst++ == CHAR_END) {
				ser->data = ser->rx_data;
				ser->len = --length;
				memcpy(ser->data, dst, ser->len);
				return (int)(dst - buf);
			}
		}
	}
	return len;
}

static void *ser_reader(void *context)
{
	struct ser_port *ser = context;
	char mbox_name[24];
	struct ulh_tbuff tbuff;
	union itc_msg *msg;
	itc_mbox_id_t me;

	sprintf(mbox_name, "%s_rx_thread", ser->data);
	(void)pthread_setname_np(pthread_self(), mbox_name);

	me = itc_create_mailbox(mbox_name, 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "[ECBLNH] create mbox failure...");
		pthread_exit(context);
	}

	for (;;) {
		int ret = ecb_alloc_buf(ser, 0, EBUS_MTU, &tbuff);
		if (ret) {
			syslog(LOG_ERR, "ecb_alloc_buf, %d", ret);
			sleep(1);
			continue;
		}

		do {
			tbuff.size  = ecb_get_frame(ser, tbuff.data, EBUS_MTU);
			ATOMIC_ADD(&ser->rx_octets, tbuff.size);
		} while (tbuff.size < 3);

		ATOMIC_ADD(&ser->rx_frames, 1);

		if (check_frame(&tbuff)) {
			uint8_t addr = tbuff.data[0];
			struct atf_conn *p = &atfi.conns[atfi.conn_lookup[addr]];

			if (p->state != DISCONNECTED) {
				A4C_DEBUG("-- RX [LNH ]", tbuff.data, tbuff.size);
				ulh_trans_deliver(p->tran_id, &tbuff);
			} else if (atfi.mpa4ci_mbox != ITC_NO_ID){
				A4C_DEBUG("-- RX [A4CI]", tbuff.data, tbuff.size);
				msg = itc_alloc(sizeof(msg->a4ci),ULH_TRANSMSG_DATA);
				msg->a4ci.data = tbuff;
				itc_send(&msg, atfi.mpa4ci_mbox, ITC_MY_MBOX);
			} else {
				A4C_DEBUG("-- RX [DROP]", tbuff.data, tbuff.size);
				send_dm_response(addr);
				ulh_tbuff_free(&tbuff);
			}
		} else {
			A4C_DEBUG("-- RX [DISC]", tbuff.data, tbuff.size);
			ATOMIC_ADD(&ser->fcserrors, 1);
			ulh_tbuff_free(&tbuff);
		}
	}
    return context;
}

static struct ulh_trans_ops ecb_ops = {
	.create_conn 	= ecb_create_conn,
	.getmtu 		= ecb_getmtu,
	.alloc_buff		= ecb_alloc_buf
};


int ecb_init(void)
{
	pthread_t rtid;
	struct rhai_ser_if *handle;
	int ret = -EAGAIN;

	for (; ret; sleep(2)) {
		ret = rhai_ser_open(&handle, RHAI_SER_UNIT_ECB,
									RHAI_SER_UARTBAUD_115200);

		if (ret == 0)
			break;

		if (ret != -EAGAIN) {
			syslog(LOG_ERR, "rhai_ser_init: %d", ret);
			sleep(10);
		}
	}
	g_port = malloc(sizeof(*g_port));
	memset(g_port, 0, sizeof(*g_port));
	g_port->handle = handle;
	g_port->data = g_port->rx_data;
	strcpy((char*)g_port->data, "ecb");

	if ((ret = ulh_trans_register("ecb0", &ecb_ops, g_port)))
		goto eout0;

	if ((ret = pthread_create(&rtid, NULL, ser_reader, g_port)) == 0)
		return ret;

	ulh_trans_unregister("ecb0");

 eout0:
	rhai_ser_close(handle);
	free(g_port);
	g_port = NULL;
	return ret;
}


struct ser_port *sau_init(void)
{
	struct rhai_ser_if *handle;
	struct ser_port *p;
	pthread_t rtid;
	int ret;

	ret = rhai_ser_open(&handle, RHAI_SER_UNIT_SAU,
									RHAI_SER_UARTBAUD_115200);

	if (ret) {
		syslog(LOG_ERR, "rhai_ser_open(SAU) failed, %d\n", ret);
		return NULL;
	}

	p = malloc(sizeof(*p));
	memset(p, 0, sizeof(*p));
	p->handle = handle;
	p->data = p->rx_data;
	strcpy((char*)p->data, "sau");

	if ((ret = pthread_create(&rtid, NULL, ser_reader, p)) == 0)
		return p;

	syslog(LOG_ERR, "pthread_create failed, %d\n", errno);

	rhai_ser_close(handle);
	free(p);
	return NULL;
}

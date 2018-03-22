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
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <itc.h>

#include "ecb-link-api.h"
#include "atfi.sig"
#include "test.h"

#define CRC_INIT            0xffffffff
#define CRC_HIGH            0x80000000
#define CRC_POLY            0x04c11db7


#define TEST_DATA 			0xb
#define TEST_DATA_R 		0xc
#define CLOSE_TEST			0xd

struct testdata {
	uint32_t				msgno;
	uint32_t				crc32;
	uint32_t				seqno;
	char					data[1];
};

union itc_msg {
	uint32_t					msgno;
	struct ecb_link_event		lkind;
	struct testdata				test;
};


static uint64_t get_tick(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000ULL + ts.tv_nsec / 1000000;
}

static uint32_t crc32(void *q, int len)
{
	unsigned char* p = q;
    unsigned long i, j, c, bit;
    unsigned long crc = CRC_INIT;

    for (i = 0; i < len; i++)  {
        c = (unsigned long)*p++;

        for (j=0x80; j; j>>=1) {
            bit = crc & CRC_HIGH;
            crc<<= 1;

            if (c & j)
                bit ^= CRC_HIGH;
            if (bit)
                crc ^= CRC_POLY;
        }
    }
    return htonl(crc);
}

void *ping_thread(void *param)
{
	uint32_t select[3] = {2, ITC_LOCATE_DEFAULT_NO, ECB_LINK_EVENT_MSG};
	uint32_t sel[3]    = {2, TEST_DATA_R, ECB_LINK_EVENT_MSG};
	uint32_t fin[2]    = {1, CLOSE_TEST};
	struct thread_data *td = param;
	struct ecb_link_config cfg;
	void *handle = NULL;
	union itc_msg *msg;
	itc_mbox_id_t me, peer = 0;
	uint32_t count, size = 12, linkid = 0;
	int ret;
	uint64_t t0, tmax = 0;

	td->result = 1;
	me = itc_create_mailbox("ping_thread", 0);

	if (me == ITC_NO_ID)
		pthread_exit(NULL);

	ret = ecb_link_init(&handle);
	if (ret) {
		printf("ecb_link_init, %d\n", ret);
		td->result = 2;
		goto done;
	}

	cfg.address = td->address;
	cfg.station = ECB_STATION_PRIMARY;
	strcpy(cfg.name, "slave");

	ret = ecb_link_create(handle, &cfg, &linkid);
	if (ret) {
		printf("ecb_link_create, %d\n", ret);
		td->result = 3;
		goto done;
	}

	itc_locate_async("slave/pong_thread", NULL, ITC_MY_MBOX);

	for (count = 0; !peer && (count < 10); count++) {
		msg = itc_receive(select, 1000, ITC_FROM_ALL);
		if (msg) {
			if (msg->msgno == ITC_LOCATE_DEFAULT_NO)
				peer = itc_sender(msg);

			itc_free(&msg);
		}
	}

	if (!peer) {
		td->result = 4;
		goto done;
	}

	for (count = 0; count < td->count; count++) {
		uint32_t crcc, crcd;

		msg = itc_alloc(size,TEST_DATA);
		msg->test.seqno = count;
		msg->test.crc32 = crc32(&msg->test.seqno, size - 8);

		t0 = get_tick();
		itc_send(&msg, peer, ITC_MY_MBOX);
		msg = itc_receive(sel, 3000, ITC_FROM_ALL);

		if (msg == NULL) {
			printf("No response from spray slave after %u (out of %u)"
					 "signals. Quitting.\n", count, td->count);
			td->result = 5;
			goto done;
		}

		if (msg->msgno == ECB_LINK_EVENT_MSG) {
			printf("Unexpected LINK_EVENT: %d\n", msg->lkind.event);
			td->result = 6;
			goto done;
		}

		if ((t0 = (get_tick() - t0)) > tmax) {
			tmax = t0;
			printf("New max RTD: %u milliseconds\n", (uint32_t)tmax);
		}

		crcd = msg->test.crc32;
		crcc = crc32(&msg->test.seqno, itc_size(msg) - 8);
		itc_free(&msg);

		if (crcc != crcd) {
			printf("CRC error in %zu byte message\n", itc_size(msg) - 8);
			td->result = 7;
			goto done;
		}

		if ((count % 20) == 0)
			printf("Looped %d messages\n", count);

		if (++size > td->size)
			size = 12;
	}

	td->result = 0;

	msg = itc_alloc(sizeof(struct testdata), CLOSE_TEST);
	itc_send(&msg, peer, ITC_MY_MBOX);
	msg = itc_receive(fin, 1000, ITC_FROM_ALL);

	if (msg)
		itc_free(&msg);

	ecb_link_destroy(handle, linkid);
	ecb_link_shutdown(handle);
	usleep(5000);

 done:
	itc_delete_mailbox(me);
	pthread_exit(NULL);
	return NULL;
}



void *pong_thread(void *param)
{
	struct thread_data *td = param;
	union itc_msg *msg;
	itc_mbox_id_t me;
	uint32_t count = 0;
	int32_t rcv_tmo = ITC_NO_TMO;

	td->result = 1;
	me = itc_create_mailbox("pong_thread", 0);

	if (me == ITC_NO_ID)
		goto done;

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, rcv_tmo, ITC_FROM_ALL);
		if (msg == NULL) {
			printf("No messages from master in %u ms. Quitting.\n", rcv_tmo);
			goto done;
		}
		switch (msg->msgno) {
		case TEST_DATA:
			msg->msgno = TEST_DATA_R;
			itc_send(&msg, itc_sender(msg),ITC_MY_MBOX);
			rcv_tmo = 3000;
			count++;
			break;

		case CLOSE_TEST:
			printf("CLOSE_TEST received. %d messages looped\n", count);
			td->count = count;
			td->result = 0;
			itc_send(&msg, itc_sender(msg),ITC_MY_MBOX);
			usleep(10000);
			itc_delete_mailbox(me);
			goto done;

		default:
			printf("Received unknown message 0x%x\n", msg->msgno);
			itc_free(&msg);
			break;
		}
	}
 done:
	pthread_exit(NULL);
	return NULL;
}

/*
 *
 * Copyright 2013 Ericsson AB
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
  *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
	 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */


#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <pthread.h>
#include <sys/time.h>
#include <time.h>
#include "itc.h"
#include "ecb-link-api.h"


#define NANO_SECOND_MULTIPLIER  1000000  // 1 millisecond = 1,000,000

#define START_TEST	0xa
struct starttest {
	uint32_t	msgno;
};
#define TEST_DATA 0xb
struct testdata {
	uint32_t	msgno;
	uint32_t	crc32;
	uint32_t	count;
	char		data[1];
};

#define TEST_DATA_R 0xc
struct testr {
	uint32_t	msgno;
	uint32_t	crc32;
	uint32_t	count;
	char		data[1];
};
#define CLOSE_TEST	0xd
struct closetest {
	uint32_t	msgno;
};

static itc_mbox_id_t test = ITC_NO_ID;
static itc_mbox_id_t slave = ITC_NO_ID;


union itc_msg {
	uint32_t		msgno;
	struct ecb_link_event	lkind;
	struct testdata		test;
	struct starttest	start;
	struct closetest	close;
};


static itc_mbox_id_t
hunt_peer(char *peer_name) {
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	union itc_msg *loc;
	itc_mbox_id_t me, peer;
	me = itc_current_mbox();
	itc_locate_async(peer_name, NULL, me);
	for(me = itc_current_mbox();;) {
	while(1) {
		loc = itc_receive(select, 0, ITC_FROM_ALL);
		if (loc)
			itc_free(&loc);
		else
			break;
	}
	peer = itc_locate(peer_name);
	if(peer != ITC_NO_ID)
		break;

	sleep(1);
        }
	printf("Hunt for %s success\n",peer_name);
	return peer;
}

static char helptext[] =
      " prim [-a <cfg address> -n <no. of messages>] \n"
      "-a <cfg address>    Configuration address for link\n"
      "-n <no of messages> no of messages to be looped\n"
      "-f <multiply factor> multiply factor for calculting delay between sends\n"
      "-h                 Print help text\n";


static void printhelp()
{
   printf("%s", helptext);
} /* printhelp */


int
main(int argc, char *argv[]) {
	uint32_t select[2] = {1, ECB_LINK_EVENT_MSG};
	uint32_t sel[3] = {1, TEST_DATA_R,ECB_LINK_EVENT_MSG};
	union itc_msg *msg,*loc;
	struct ecb_link_config cfg;
	void *handle = NULL;
	uint32_t linkid = 0;
	int ret, count = 0;
        uint32_t addr = 0, factor = 0;
	struct timeval txTime;
	struct timeval rxTime;
        uint32_t dms,rec_count = 0;
        uint32_t noofsig = 0;
	long INTERVAL_MS;
	struct timespec sleepValue = {0};
	struct tm *tm;
	char s[20]; /* strlen("2009-08-10 18:17:54") + 1 */

	time_t rawtime;
        time( &rawtime );

	/*
	 *     * parse command-line options
	 */
	while ((ret = getopt(argc, argv, "h:a:n:f:")) != -1) {
		switch(ret) {
		case 'a':
			addr = atoi(optarg);
			break;
		case 'n':
			noofsig = atoi(optarg);
			break;
		case 'f':
			factor = atoi(optarg);
			break;
		case 'h':
		case '?':
			printhelp();
			exit(0);
		default:
			printhelp();
			exit(-1);
			break;
		}
	}

	INTERVAL_MS = factor * NANO_SECOND_MULTIPLIER;
	sleepValue.tv_nsec = INTERVAL_MS;

	ret = itc_init(4, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n",ret);
		return -1;
	}

	test = itc_create_mailbox("Primary", 0);
	if (test == ITC_NO_ID) {
		printf("created mbox failure: %d\n",ITC_MY_MBOX);
		itc_exit();
		return -1;
	}

	ret = ecb_link_init(&handle);
	if (ret) {
		printf("ecb_link_init, %d\n", ret);
		return -1;
	}

	cfg.address = addr;
	cfg.station = ECB_STATION_PRIMARY;
	strcpy(cfg.name, "primary");

	ret = ecb_link_create(handle, &cfg, &linkid);
	if (ret) {
		printf("ecb_link_create, %d\n", ret);
		return -1;
	}

	msg = itc_receive(select, 10000, ITC_FROM_ALL);
	if (msg) {
		printf("Event: %d received\n", msg->lkind.event);
		itc_free(&msg);
	}

	slave = hunt_peer("primary/Secondary");

	gettimeofday(&txTime, NULL);
	for(count = 0 ; count < noofsig ; count++) {
		loc = itc_alloc(sizeof(struct testdata),TEST_DATA);
		loc->test.count = count;
		itc_send(&loc, slave, test);
	//	printf("sent TEST_DATA %d\n",count);
		nanosleep(&sleepValue, NULL);
	}
	for(count = 0 ; count < noofsig ; count++) {
		loc = itc_receive(sel, 10000, ITC_FROM_ALL);
		if(loc) {
		//	printf("Received msg:%d from secondary\n",
		//			loc->msgno);
		if(loc->msgno == TEST_DATA_R) {
			rec_count++;
		}
		else if(loc->msgno == ECB_LINK_EVENT_MSG) {
			tm = localtime(&rawtime);
			strftime(s, 20, "%F %H:%M:%S", tm);
			if(msg->lkind.event == 0)
				printf("client:%s: LINK DOWN\n",s);
			if(msg->lkind.event == 1)
				printf("client:%s: LINK UP\n",s);
		}
		else {
			printf("Unknown message received %d\n",loc->msgno);
		}

		itc_free(&loc);
		printf("Received messages count %d\n",rec_count);
		}
	}
	gettimeofday(&rxTime,NULL);
	dms = (rxTime.tv_sec*1000 + rxTime.tv_usec/1000) -
			     (txTime.tv_sec*1000 + txTime.tv_usec/1000);

	printf("Test took %d ms for round trip\n",dms);
	loc = itc_alloc(sizeof(struct closetest),CLOSE_TEST);
	itc_send(&loc, slave, test);
	printf("sent close test\n");
	nanosleep(&sleepValue, NULL);

	ecb_link_shutdown(handle);
	return 0;

}

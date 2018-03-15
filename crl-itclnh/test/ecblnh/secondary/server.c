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
#include <time.h>
#include "itc.h"
#include "ecb-link-api.h"

#define NANO_SECOND_MULTIPLIER  1000000  // 1 millisecond = 1,000,000
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


union itc_msg {
	uint32_t		msgno;
	struct testdata		test;
	struct closetest	close;
	struct ecb_link_event	lkind;
};
static char helptext[] =
      " sec [-a <cfg address>  \n"
      "-a <cfg address>    Configuration address for link\n"
      "-h                 Print help text\n";


static void printhelp()
{
   printf("%s", helptext);
} /* printhelp */



int
main(int argc, char *argv[]) {
	int ret;
	itc_mbox_id_t server;
	struct ecb_link_config cfg;
	void *handle = NULL;
	uint32_t linkid = 0;
	uint32_t addr = 1;
	union itc_msg *msg,*msg1;
	uint32_t select[2] = {1, ECB_LINK_EVENT_MSG};
	uint32_t sel[] = {0};

	struct tm *tm;
	char s[20]; /* strlen("2009-08-10 18:17:54") + 1 */
	time_t rawtime;
        time( &rawtime );


	/*
	 *     * parse command-line options
	 */
	while ((ret = getopt(argc, argv, "h:a:")) != -1) {
		switch(ret) {
		case 'a':
			addr = atoi(optarg);
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



	ret = itc_init(4, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n",ret);
		return -1;
	}
	server = itc_create_mailbox("Secondary", 0);
	if (server == ITC_NO_ID) {
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
	cfg.station = ECB_STATION_SECONDARY;
	strcpy(cfg.name, "secondary");

	ret = ecb_link_create(handle, &cfg, &linkid);
	if (ret) {
		printf("ecb_link_create, %d\n", ret);
		return -1;
	}

	msg = itc_receive(select, 10000 , ITC_FROM_ALL);
	if (msg) {
		printf("Event: %d received\n", msg->lkind.event);
		itc_free(&msg);
	}

	for(;;) {
		msg = itc_receive(sel, ITC_NO_TMO, ITC_FROM_ALL);
		if(msg) {

			if(msg->msgno == TEST_DATA) {
		//	printf("Received %d times msg:%d from primary:%d\n",
		//			msg->test.count,msg->msgno,itc_sender(msg));
			msg1 = itc_alloc(sizeof(struct testr),TEST_DATA_R);
			itc_send(&msg1, itc_sender(msg),ITC_MY_MBOX);
			}

			else if(msg->msgno == CLOSE_TEST) {
			printf("Received shut down\n");
				ecb_link_shutdown(handle);
				exit(0);
			}

			else if(msg->msgno == ECB_LINK_EVENT_MSG){
				tm = localtime(&rawtime);
				strftime(s, 20, "%F %H:%M:%S", tm);
		                if(msg->lkind.event == 0)
						printf("server:%s: LINK DOWN\n",s);
				if(msg->lkind.event == 1)
						printf("server:%s: LINK UP\n",s);
			}

			else {
				printf("received unknown message\
						%d\n",msg->msgno);
			}

			itc_free(&msg);
		}
	}
return 0;
}

/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <itc.h>
#include <string.h>
#include <getopt.h>
#include <libecp.h>
#include <stdbool.h>
#include <rhami_lh_spec.h>
#include "rhd-common.h"
#include "conn-establish-helper.h"

#define LH_MAX_DEVICES          2
#define MAILBOX_SIZE 1
#define LHCMD_ECPSTAT_MBOX_NAME  "ecpstat"
#define CONN_TMO 50000

#define STRINGIFY(EXPR)         (#EXPR)
#define XSTRINGIFY(EXPR)        STRINGIFY(EXPR)

#define MAILBOX_NAME_LENGTH     (sizeof(RHAMI_LH_MAILBOX) + sizeof(XSTRINGIFY(LH_MAX_DEVICES)) + 1)

static uint32_t client_ref = 1111; /* dummy value */
static struct server_info lh_conn;
static uint32_t lh_max_devices = LH_MAX_DEVICES;
static uint32_t mailbox_name_length = MAILBOX_NAME_LENGTH;

union itc_msg {
	uint32_t         msgno;
	RHAMI_LH_STRUCTS
};

static struct conn_establish_msg_numbers lh_conn_messages = {
	RHAMI_LH_CONN_ESTABLISH_REQ,
	RHAMI_LH_CONN_ESTABLISH_CFM,
	RHAMI_LH_CONN_ESTABLISH_REJ,
	RHAMI_LH_CONN_DISCONNECT_REQ,
	RHAMI_LH_CONN_DISCONNECT_CFM,
	RHAMI_LH_CONN_DISCONNECT_REJ,
	RHAMI_LH_CONN_MONITOR_FWD
};

void get_server_mbox(itc_mbox_id_t *mbox, uint32_t lh_dev);
void print_usage();
void print_stats(uint32_t buff_idx);

void print_usage()
{
	printf("Usage: ecpstat -d <device> -b <buff_idx>\n");
}

void get_server_mbox(itc_mbox_id_t *mbox, uint32_t lh_dev)
{
	char mail_box[sizeof(RHAMI_LH_MAILBOX) + sizeof("-0xFFFFFFFF")];

	if (itc_current_mbox() == ITC_NO_ID) {
		printf("no mailbox is exist.\n");
		exit(1);
	}

        if (snprintf(mail_box, mailbox_name_length, "%s_%d", RHAMI_LH_MAILBOX,
                     lh_dev) < 0) {
                printf("Unable to initialize mailbox name %s_%d\n", RHAMI_LH_MAILBOX, lh_dev);
                exit(1);
        }

	*mbox = itc_locate(mail_box);
	if (*mbox == ITC_NO_ID) {
		printf("lh deamon id %d is invalid or mailbox %s is not existed\n",
		        lh_dev, mail_box);
		exit(1);
	}
}

void print_stats(uint32_t buff_idx)
{
	union itc_msg *reply, *request;
	static uint32_t rx_filter[] = {2,
	                               RHAMI_LH_ECPSTATS_CFM,
	                               RHAMI_LH_ECPSTATS_REJ
	                               };

	request = itc_alloc(sizeof(struct lh_ecpstats_req),
	                    RHAMI_LH_ECPSTATS_REQ);

	request->ecpstat_req.buff_idx = buff_idx;
	request->ecpstat_req.connection_ref = lh_conn.server_ref;
	itc_send(&request, lh_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(rx_filter, ITC_NO_TMO, lh_conn.server_mbox);

	if (reply->msgno == RHAMI_LH_ECPSTATS_CFM) {
		printf("%s", reply->ecpstat_cfm.buffer);
	}
	else if (reply->msgno == RHAMI_LH_ECPSTATS_REJ) {
		printf("ecp stats request is rejected\n");
	}

	itc_free(&reply);
}

int main(int argc, char *argv[])
{
	uint32_t buff_idx = 0;
	uint32_t lh_device = 0;
	bool buff_idx_valid = false;
	bool lh_device_valid = false;
	int c;
	itc_mbox_id_t my_mbox = ITC_NO_ID;
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {LH_SERVER_VERSIONS};
	uint32_t res;

#ifdef __MACHINE_ZYNQMP_MIMO
        if(strcmp("TRXM", getenv("SYS_BOARD_TYPE")) == 0)
        {
                lh_max_devices = 1;
        }
        else if (strcmp("BP", getenv("SYS_BOARD_TYPE")) == 0)
        {
                lh_max_devices = 7;
        }
        else
        {
		printf("Unknown board type\n");
                exit(1);
        }
	mailbox_name_length = sizeof(RHAMI_LH_MAILBOX) + sizeof(XSTRINGIFY(lh_max_devices)) + 1;
#endif
        while ((c = getopt(argc, argv, "d:b:h")) != -1) {
                switch (c) {
                default:
                case 'h':
                        print_usage();
			exit(1);
                case 'b':
                        buff_idx = atoi(optarg);
                        if (buff_idx >= ECP_MAX_NO_OF_BUFFS) {
                                printf("buffer index should be less than %d\n", ECP_MAX_NO_OF_BUFFS);
				exit(1);
                        }
			buff_idx_valid = true;
                        break;
                case 'd':
                        lh_device = atoi(optarg);
                        if (lh_device >= lh_max_devices) {
                                printf("ecp device index should be less than %d\n", lh_max_devices);
				exit(1);
                        }
			lh_device_valid = true;
                        break;
                }
        }

	if (!buff_idx_valid || !lh_device_valid) {
		print_usage();
		exit(1);
	}

	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Failed to initialize itc\n");
		exit(1);
	}

	if ((my_mbox = itc_create_mailbox(LHCMD_ECPSTAT_MBOX_NAME, 0)) == ITC_NO_ID) {
		printf("Failed to create itc mailbox\n");
		exit(1);
	}

	get_server_mbox(&lh_conn.server_mbox, lh_device);

	res = conn_establish(
	              /*input parameters*/
	              lh_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &lh_conn_messages,
	              CONN_TMO,
	              /*returned values*/
	              &lh_conn.server_ref,
	              &lh_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		printf("Client:Connection establish failed"
		       "(reason:0x%08x)\n", res);
		return res;
	}

	print_stats(buff_idx);

	res = conn_disconnect(lh_conn.server_mbox,
	                      ++procedure_ref,
	                      lh_conn.server_ref,
	                      &lh_conn_messages,
	                      CONN_TMO);
	if (res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection disconnect failed (reason:0x%08x)", res);
		return 1;
	}

	if (my_mbox != ITC_NO_ID)
		itc_delete_mailbox(my_mbox);

	return 0;

}

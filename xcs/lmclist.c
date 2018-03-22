/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <getopt.h>
#include <itc.h>
#include <unistd.h>
#include "lmc_server.h"

#define MAILBOX_SIZE 5
#define CONN_ESTABLISH_TMO 0

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	LMC_SERVER_MESSAGES;
};

static void print_usage(void)
{
	printf("Usage: lmclist <options>\n\n"
	       "List the lmc in flash\n"
	       "Options:\n"
	       "     -h  Display usage information (this message).\n"
	       "     -l  Print long format\n"
	       "     -a  Print all LMC:s in the flash\n");
}

int main(int argc, char **argv)
{
	int i;
	uint32_t long_mode = 0, all_mode = 0;
	const struct lmc_load_file_entry *lmc_list;
	union itc_msg *msg = NULL;
	int opt;

	char mailbox_name[50];
	itc_mbox_id_t my_mbox = ITC_NO_ID;
	itc_mbox_id_t server_mbox;
	uint32_t procedure_ref = 0;
	uint32_t client_ref = 1234;
	uint32_t requested_versions[] = {LMC_SERVER_VERSIONS};
	LMC_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	uint32_t server_ref, selected_version, res;
	int status = -1;

	while ((opt = getopt(argc, argv, "hla")) != -1) {
		switch (opt) {
		case 'l':
			long_mode = 1;
			break;
		case 'a':
			all_mode = 1;
			break;
		case 'h':
			status = 0;
			print_usage();
			goto exit;
		default:
			print_usage();
			goto exit;
		}
	}

	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Unable to inizalize ITC!\n");
		goto exit;
	}

	snprintf(mailbox_name, sizeof(mailbox_name), "%s-%d", argv[0],
	         getpid());
	my_mbox = itc_create_mailbox(mailbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		printf("Unable to create ITC mailbox!\n");
		goto exit;
	}

	server_mbox = itc_locate(LMC_SERVER_NAME);
	if(server_mbox == ITC_NO_ID) {
		printf("Cannot locate the server \"%s\"\n", LMC_SERVER_NAME);
		goto exit;
	}

	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) /
	              sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection establish failed (reason:0x%x)\n", res);
		goto exit;
	}

	msg = itc_alloc(sizeof(struct lmc_get_load_file_info_req),
	                LMC_GET_LOAD_FILE_INFO_REQ);
	msg->get_load_file_info_req.procedure_ref = ++procedure_ref;
	msg->get_load_file_info_req.connection_ref = server_ref;
	msg->get_load_file_info_req.flags = 0;
	if(all_mode) {
		msg->get_load_file_info_req.flags |=
		        LMC_GET_LOAD_FILE_INFO_REQ_FLAGS_ALL;
	}

	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, 15000, server_mbox);
	if(!msg) {
		printf("Timeout! Server did not reply within 15 seconds,\n");
		goto exit;
	}

	if(msg->any_msg.procedure_ref != procedure_ref) {
		printf("Server replied with invalid procedure_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       procedure_ref, msg->any_msg.procedure_ref);
		goto exit;
	}
	if(msg->any_msg.connection_ref != client_ref) {
		printf("Server replied with invalid connection_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       client_ref, msg->any_msg.connection_ref);
		goto exit;
	}

	if(msg->msgno != LMC_GET_LOAD_FILE_INFO_CFM) {
		printf("Server replied with unexpected message;"
		       "message number: 0x%08x\n",
		       msg->msgno);
		goto exit;
	}

	lmc_list = msg->get_load_file_info_cfm.info;

	if (all_mode) {
		printf("   %-13s%-9s%-11s%-11s%-5s%-17s%-4s%-9s%-4s%-16s%s\n",
		       "SlotProt", "State", "SlotSize", "LmcSize", "Curr",
		       "ProdDate", "Seq", "Type", "LMs", "Partition", "SwPid");
	} else if (long_mode) {
		printf("   %-9s%-9s%-11s%-11s%-5s%-17s%-4s%-7s%-4s%s\n",
		       "SlotProt", "State", "SlotSize", "LmcSize", "Curr",
		       "ProdDate", "Seq", "Type", "LMs", "SwPid");
	} else {
		printf("   %-9s%-9s%-11s%-5s%-17s%-4s%-7s%-4s%s\n",
		       "SlotProt", "State", "Size", "Curr", "ProdDate",
		       "Seq", "Type", "LMs", "SwPid");
	}

	for(i = 0; i < msg->get_load_file_info_cfm.lmc_count; i++) {
		if (lmc_list[i].state == LMC_STATE_NON_EXISTENT) {
			continue;
		}

		/* Print the index. */
		printf("%2d ", i);

		/* Print the slot permissions state. */
		if(all_mode) {
			if(lmc_list[i].permissions & LMC_LOAD_FILE_PERMISSION_RO) {
				printf("%-3s", "RO ");
			} else{
				printf("%-3s", "RW ");
			}

			if(lmc_list[i].permissions & LMC_LOAD_FILE_PERMISSION_UNLOCKED) {
				printf("%-*s", 13-3, "Unlocked");
			} else if(lmc_list[i].permissions & LMC_LOAD_FILE_PERMISSION_LOCKED) {
				printf("%-*s", 13-3, "Locked");
			} else {
				printf("%*s", 13-3, " ");
			}
		} else {
			if(lmc_list[i].permissions & LMC_LOAD_FILE_PERMISSION_LOCKED) {
				printf("%-9s", "Locked");
			} else {
				printf("%-9s", "Unlocked");
			}
		}
		/* Print the state */
		switch (lmc_list[i].state) {
		case LMC_STATE_VALID:
			if (lmc_list[i].working == 1) {
				printf("%-9s", "Working");
			} else {
				printf("%-9s", "Valid");
			}
			break;
		case LMC_STATE_ERPROG:
			printf("%-9s", "Erasing");
			break;
		case LMC_STATE_WRPROG:
			printf("%-9s", "Writing");
			break;
		case LMC_STATE_ERROR:
			printf("%-9s", "Faulty");
			break;
		case LMC_STATE_ERASED:
			printf("%-9s", "Erased");
			break;
		default:
			printf("%-9s", "?");
			break;
		}

		/* Print the slot size */
		printf("%-11d", lmc_list[i].slot_size);

		if(lmc_list[i].state != LMC_STATE_VALID) {
			if(all_mode) {
				printf("%-11s%-5s%-17s%-4s%-9s%-4s",
				       " ", " ", " ", " ", " ", " ");
				printf("%-16s", lmc_list[i].partition_name);
			}
			printf("\n");
			continue;
		}
		if(long_mode || all_mode) {
			/* Print the actual size of LMC */
			printf("%-11d", lmc_list[i].lmc_size);
		}

		/* Print current */
		if (lmc_list[i].is_current == 1) {
			printf("%-5s", "Yes");
		} else {
			printf("%-5s", "No");
		}

		/* Print production date, seqno and lmctype */
		printf("%04d-%02d-%02d %02d:00 ",
		       1900 + ((char *) & (lmc_list[i].time))[3], /* year */
		       1 + ((char *) & (lmc_list[i].time))[2],    /* month */
		       ((char *) & (lmc_list[i].time))[1],        /* day */
		       ((char *) & (lmc_list[i].time))[0]);       /* hour */
		printf("%-4d", lmc_list[i].seq_number);

		if(!all_mode) {
			if (lmc_list[i].file_type ==
			    XLF_IBOOT_HDR_TYPE_AU_BOOT) {
				printf("%-7s", "Boot");
			} else if (lmc_list[i].file_type ==
			           XLF_IBOOT_HDR_TYPE_AU_APPLIC) {
				printf("%-7s", "Applic");
			} else {
				printf("%-7s", "?");
			}
		} else {
			if (lmc_list[i].file_type ==
			    XLF_IBOOT_HDR_TYPE_AU_BOOT) {
				printf("%-9s", "AuBoot");
			} else if (lmc_list[i].file_type ==
			           XLF_IBOOT_HDR_TYPE_AU_APPLIC) {
				printf("%-9s", "AuApplic");
			} else if (lmc_list[i].file_type ==
			           XLF_IBOOT_HDR_TYPE_BOOT) {
				printf("%-9s", "Boot");
			} else if (lmc_list[i].file_type ==
			           XLF_IBOOT_HDR_TYPE_AU_ENV) {
				printf("%-9s", "AuEnv");
			} else if (lmc_list[i].file_type ==
			           XLF_IBOOT_HDR_TYPE_BOOT_ENV) {
				printf("%-9s", "BootEnv");
			} else {
				printf("%-9s", "?");
			}
		}

		/* Print no load modules. Print swpid last as it may be long */
		printf("%-4d", lmc_list[i].subfile_counter);

		if(all_mode) {
			printf("%-16s", lmc_list[i].partition_name);
		}
		printf("%s\n", lmc_list[i].lmid);
	}

	itc_free(&msg);
	res = conn_disconnect(server_mbox, ++procedure_ref,
	                      server_ref, &conn_messages, CONN_ESTABLISH_TMO);
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection disconnect failed (reason:0x%x)\n", res);
		goto exit;
	}

	status = 0;

exit:
	if (msg)
		itc_free(&msg);
	if (my_mbox != ITC_NO_ID)
		itc_delete_mailbox(my_mbox);

	return status;
}

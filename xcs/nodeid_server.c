/*
 * Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */
/*----------------------------  Include files  ------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <mtd/mtd-user.h>
#include <itc.h>

#include "nodeid_server.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_nodeid_server
#include <tpt_create.h>
#include <tpt.h>

#define NODEID_MAX_NOF_MBOX          1
#define NODEID_DAEMON_NAME           "nodeid_server"
#define NODEID_DEVICE_ENV            "sys_nodeid_dev"

#define EXIT_SIGNAL                  0xdeefbead

/* Node id area definitions.
 * 128*128=16 kB
 */
#define NODEID_NO_ENTRIES      128
#define NODEID_ENTRY_LEN       128

/*entry should begin with NID:*/
#define NODEID_PREFIX "NID:"
#define NODEID_PREFIX_LEN (sizeof(NODEID_PREFIX) - 1)
#define NODEID_MAX_SIZE (NODEID_ENTRY_LEN - NODEID_PREFIX_LEN)


static itc_mbox_id_t nodeid_server_mbox = ITC_NO_ID;

union itc_msg {
	uint32_t msgno;
	NODEID_STRUCTS
};

/* server global state */
static mtd_info_t mtd_info;
static int mtd_fd;

/**
 * Function print_usage
 */
static void print_usage()
{
	printf("Usage: nodeid_server <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	NODEID_CONN_ESTABLISH_MSG_STRUCT(nodeid_server_conn_messages);

	uint32_t supported_versions[] = {NODEID_SERVER_VERSIONS};
	int conn_result = conn_establish_server_init( &handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &nodeid_server_conn_messages, 0, NULL);

	if( conn_result != CONN_INIT_OK)
		return NULL;

	return handle;
}

static int init_mtd(char *dev, mtd_info_t *pinfo)
{
	int fd = open(dev, O_RDWR, O_SYNC | O_NONBLOCK);
	if (fd < 0)
		return -1;

	if (ioctl(fd, MEMGETINFO, pinfo)) {
		if (close(fd) == -1)
			TPT_ERROR(STR("close failed with errno %d", errno));
		return -1;
	}

	return fd;
}



static uint32_t mtd_write(const char *buf,
                     uint32_t size,
                     uint32_t offset)
{
	uint32_t ret = NODEID_RESULT_OTHER_ERROR;
	int32_t sys_ret;

	if((size + offset)  > mtd_info.size) {
		TPT_ERROR(STR("requested size 0x%x @ offset: 0x%x exceeds "
		              "partition size 0x%x",
		              size, offset, mtd_info.size));
		ret = NODEID_RESULT_INVALID_PARAM;
		goto mtd_write_end;
	}
	sys_ret = lseek(mtd_fd, offset, SEEK_SET);
	if (sys_ret == -1) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto mtd_write_end;
	} else if (sys_ret != offset) {
		TPT_ERROR(STR("lseek return %d is not as expected %d", ret, offset));
		goto mtd_write_end;
	}

	sys_ret = write(mtd_fd, buf, size);
	if (sys_ret == -1) {
		TPT_ERROR(STR("write failed with errno %d", errno));
		goto mtd_write_end;
	} else if (sys_ret != size) {
		TPT_ERROR(STR("warning:write %d bytes differs from "
				"expected %d bytes", sys_ret, size));
		goto mtd_write_end;
	}
	ret = NODEID_RESULT_SUCCESS;

mtd_write_end:
	return ret;
}

static uint32_t mtd_erase(void)
{
	erase_info_t erase_info;

	erase_info.length = mtd_info.erasesize;

	for (erase_info.start = 0;
	     erase_info.start < mtd_info.size;
	     erase_info.start += mtd_info.erasesize) {

		if (ioctl(mtd_fd, MEMERASE, &erase_info) == -1) {
			TPT_ERROR(STR("Erase (fd %d) failed with errno %d", mtd_fd, errno));
			return NODEID_RESULT_OTHER_ERROR;
		}

	}

	return NODEID_RESULT_SUCCESS;
}

static uint32_t find_last_entry(uint32_t *index, char entry[NODEID_ENTRY_LEN], uint32_t entry_len)
{
	ssize_t read_ret;
	uint32_t ret = NODEID_RESULT_ACCESS_DENIED;

	if (lseek(mtd_fd, 0, SEEK_SET)) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto find_last_entry_end;
	}

	/* Scan entries for first erased entry. */
	for (*index = 0; *index < NODEID_NO_ENTRIES; (*index)++) {
		read_ret = read(mtd_fd, entry, entry_len);
		if (read_ret == -1) {
			TPT_ERROR(STR("read failed with errno %d", errno));
			goto find_last_entry_end;
		} else if (read_ret != entry_len) {
			TPT_ERROR(STR("warning: read %d bytes while expected %d" "bytes", read_ret, entry_len));
			goto find_last_entry_end;
		}

		if (*entry == 0xFF) {
			/* Found erased entry. */
			break;
		}
	}

	if (!(*index)) {
		/* Empty area. */
		ret = NODEID_RESULT_NOT_FOUND;
		goto find_last_entry_end;
	}

	/* Reverse to last valid entry end re-read it. */
	(*index)--;
	if (lseek(mtd_fd, *index * entry_len, SEEK_SET)
	                != *index * entry_len) {
		TPT_ERROR(STR("lseek %d failed with errno %d", *index * entry_len, errno));
		goto find_last_entry_end;
	}

	read_ret = read(mtd_fd, entry, entry_len);
	if (read_ret == -1) {
		TPT_ERROR(STR("read failed with errno %d", errno));
		goto find_last_entry_end;
	} else if (read_ret != entry_len) {
		TPT_ERROR(STR("Read %d bytes while expected %d bytes", read_ret, entry_len));
		goto find_last_entry_end;
	}

	ret = NODEID_RESULT_SUCCESS;

find_last_entry_end:
	return ret;
}

static uint32_t write_node_id(uint32_t length, uint8_t *node_id_buf)
{
	char entry[NODEID_ENTRY_LEN], entry_mtd[NODEID_ENTRY_LEN];
	uint32_t  entry_no, i, ret;

	if ((length * 2) > NODEID_MAX_SIZE) {
		TPT_ERROR(STR("Requested size %d exceeds max %d", length,
		              NODEID_MAX_SIZE));
		ret = NODEID_RESULT_INVALID_PARAM;
		goto write_node_id_end;
	}
	/*
	 * Build nodeid string, "NID:<nodeid numbers in two digit hexadecimal>"
	 */
	memcpy(entry, NODEID_PREFIX, NODEID_PREFIX_LEN);
	for (i = 0; i < length; i++)
	{
		sprintf(&entry[NODEID_PREFIX_LEN + i*2], "%02x",
		        node_id_buf[i]);
	}

	ret = find_last_entry(&entry_no, entry_mtd, sizeof(entry_mtd));
	if (ret == NODEID_RESULT_SUCCESS) {
		if (entry_no == NODEID_NO_ENTRIES - 1) {
			ret = NODEID_RESULT_RESOURCE_SHORTAGE;
			goto write_node_id_end;
		}

		if (!strcmp((char *)entry, entry_mtd)) {
			/* Ignore request if new node id is equal to last. */

			goto write_node_id_end;
		}

		entry_no++; /* Advance to erased position */
	} else if (ret != NODEID_RESULT_NOT_FOUND) {
		goto write_node_id_end;
	}

	ret = mtd_write(entry, strlen(entry) + 1,
	                entry_no * NODEID_ENTRY_LEN);

write_node_id_end:
	return ret;
}

static uint32_t read_node_id(uint32_t *length, uint8_t *node_id_buf)
{
	char entry[NODEID_ENTRY_LEN], *src;
	uint32_t entry_no, i, ret;

	ret = find_last_entry(&entry_no, entry, sizeof(entry));
	if (ret != NODEID_RESULT_SUCCESS) {
		goto read_node_id_end;
	}

	/*
	 * Get nodeid 8b binary values from last nodeid string (skip 'NID:' at
	 * the beginning of string).
	 */
	if (*length > NODEID_MAX_SIZE) {
		*length = NODEID_MAX_SIZE;
	}

	for (i = 0, src = entry + NODEID_PREFIX_LEN;
	              i < *length && *src != '\0';
	              i++, src += 2) {
	if (sscanf(src, "%02hhx", &node_id_buf[i]) != 1) {
		TPT_INFO(STR("Found garabage %.2s at offset %d", src,
		              entry_no * NODEID_ENTRY_LEN));
		break;
		}
	}
	*length = i;

	ret = NODEID_RESULT_SUCCESS;
read_node_id_end:
	return ret;
}

static void handle_read_nodeid_req(union itc_msg *in_msg, uint32_t client_ref)
{
	int32_t ret_code = NODEID_RESULT_SUCCESS;
	union itc_msg *out_msg;
	uint8_t  node_id_buf[NODEID_MAX_SIZE];
	uint32_t length = sizeof(node_id_buf);

	itc_mbox_id_t client_mbox = itc_sender(in_msg);
	struct nodeid_read_req *req = &in_msg->nodeid_read_req;

	ret_code = read_node_id(&length, node_id_buf);

	if (ret_code == NODEID_RESULT_SUCCESS) 	{
		out_msg = itc_alloc(offsetof(struct nodeid_read_cfm, node_id) +
				             length, NODE_ID_READ_CFM);
		out_msg->nodeid_read_cfm.connection_ref = client_ref;
		out_msg->nodeid_read_cfm.procedure_ref = req->procedure_ref;
		out_msg->nodeid_read_cfm.length = length;
		memcpy(out_msg->nodeid_read_cfm.node_id, node_id_buf, length);
		TPT_SEND_SIG(out_msg->msgno, client_mbox, STR("NODE_ID_READ_CFM"));
	} else {
		out_msg = itc_alloc(sizeof(struct nodeid_read_rej), NODE_ID_READ_REJ);
		out_msg->nodeid_read_rej.connection_ref = client_ref;
		out_msg->nodeid_read_rej.procedure_ref = req->procedure_ref;
		out_msg->nodeid_read_rej.error_code = ret_code;
		TPT_SEND_SIG(out_msg->msgno, client_mbox,
		             STR("NODE_ID_READ_REJ: retCode=%d",
		                 out_msg->nodeid_read_rej.error_code));
	}
	itc_send(&out_msg, client_mbox, ITC_MY_MBOX);
}

static void handle_write_nodeid_req(union itc_msg *in_msg, uint32_t client_ref)
{
	uint32_t ret_code = NODEID_RESULT_SUCCESS;
	union itc_msg *out_msg;
	itc_mbox_id_t client_mbox = itc_sender(in_msg);
	struct nodeid_write_req *req = &in_msg->nodeid_write_req;

	TPT_REC_SIG(in_msg->msgno,
	            STR("handle_write_nodeid_req (length %u buffer %p)",
	                req->length,
	                req->node_id));

	ret_code = write_node_id(req->length, req->node_id);

	if (ret_code == NODEID_RESULT_SUCCESS) {
		out_msg = itc_alloc(sizeof(struct nodeid_write_cfm), NODE_ID_WRITE_CFM);
		out_msg->nodeid_write_cfm.connection_ref = client_ref;
		out_msg->nodeid_write_cfm.procedure_ref = req->procedure_ref;
		TPT_SEND_SIG(out_msg->msgno, client_mbox, "NODE_ID_WRITE_CFM");
	} else {
		out_msg = itc_alloc(sizeof(struct nodeid_write_rej), NODE_ID_WRITE_REJ);
		out_msg->nodeid_write_rej.connection_ref = client_ref;
		out_msg->nodeid_write_rej.procedure_ref = req->procedure_ref;
		out_msg->nodeid_write_rej.error_code = ret_code;
		TPT_SEND_SIG(out_msg->msgno, client_mbox,
	               STR("NODE_ID_WRITE_REJ: err:%d", out_msg->nodeid_write_rej.error_code));
	}
	itc_send(&out_msg, client_mbox, ITC_MY_MBOX);
}

static void
handle_erase_nodeid_req(union itc_msg *in_msg, uint32_t client_ref)
{
	int32_t ret_code;
	union itc_msg *out_msg;
	itc_mbox_id_t client_mbox = itc_sender(in_msg);
	struct nodeid_erase_req *req = &in_msg->nodeid_erase_req;

	TPT_REC_SIG(in_msg->msgno,
	            STR("handle_erase_nodeid_req"));

	ret_code = mtd_erase();

	if(ret_code == NODEID_RESULT_SUCCESS) {
		out_msg = itc_alloc(sizeof(struct nodeid_erase_cfm),
				                           NODE_ID_ERASE_CFM);
		out_msg->nodeid_erase_cfm.connection_ref = client_ref;
		out_msg->nodeid_erase_cfm.procedure_ref = req->procedure_ref;
		TPT_SEND_SIG(out_msg->msgno, client_mbox, "NODE_ID_ERASE_CFM");
	} else {
		out_msg = itc_alloc(sizeof(struct nodeid_erase_rej),
				                    NODE_ID_ERASE_REJ);
		out_msg->nodeid_erase_rej.connection_ref = client_ref;
		out_msg->nodeid_erase_rej.procedure_ref = req->procedure_ref;
		out_msg->nodeid_erase_rej.error_code = ret_code;
		TPT_SEND_SIG(out_msg->msgno, client_mbox, "NODE_ID_ERASE_REJ");
	}

	itc_send(&out_msg, client_mbox, ITC_MY_MBOX);
}

static void main_loop(conn_server_handle_t handle)
{
	union itc_msg *msg_p;
	struct conn_client_info client_info;

	while(1) {
		msg_p = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		if (msg_p) {

			if (msg_p->msgno == EXIT_SIGNAL) {
				TPT_INFO(STR("%s exiting as ordered", NODEID_DAEMON_NAME));
				itc_free(&msg_p);
				return;
			}
			/*Handle CONN_ESTABLISH... messages (and messages from
			 unknown clients.*/
			if (!conn_check_client(handle, &msg_p, &client_info))
				continue;

			switch (msg_p->msgno) {
				case NODE_ID_READ_REQ:
					handle_read_nodeid_req(msg_p, client_info.client_ref);
					break;

				case NODE_ID_WRITE_REQ:
					handle_write_nodeid_req(msg_p, client_info.client_ref);
					break;
				/*shell command option*/
				case NODE_ID_ERASE_REQ:
					handle_erase_nodeid_req(msg_p, client_info.client_ref);
					break;
				default:
					TPT_INFO(STR("Unexpected message received, "
					"msgno=0x%x, sender=0x%x",
					msg_p->msgno, itc_sender(msg_p)));
					break;
			}
			itc_free(&msg_p);
		}
	}

}

static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send((union itc_msg **)&msg, nodeid_server_mbox, ITC_MY_MBOX);
}

/**
 * Main function
 */
int main( int argc, char **argv )
{
	int daemonize = 0;
	void *handle = NULL;

	char *dev = getenv(NODEID_DEVICE_ENV);
	if (!dev) {
		TPT_ERROR(STR("The %s env variable must be set", NODEID_DEVICE_ENV));
		exit(1);
	}

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		}
		else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		}
		else {
			print_usage();
			exit(1);
		}
	}

	if (!daemonize || !daemon(0, 0)) {
		TPT_INFO(STR("Starting %s %s",
		              daemonize ? "daemon" : "foreground process",
		              NODEID_DAEMON_NAME));

		/* Initialize ITC */
		if (itc_init(NODEID_MAX_NOF_MBOX, ITC_MALLOC,
		             NULL, ITC_NO_NAMESPACE, 0)) {
			TPT_ERROR("Unable to initialize ITC!");
			return -1;
		}

		nodeid_server_mbox = itc_create_mailbox(NODEID_SERVER_NAME, 0);
		if (nodeid_server_mbox == ITC_NO_ID) {
			TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
			              NODEID_SERVER_NAME));
		}

		mtd_fd = init_mtd(dev, &mtd_info);
		if (mtd_fd == -1) {
			TPT_ERROR(STR("Failed to initialize i/o device: %s", dev));
			exit(1);
		}
		/* Initialize connection establish server */
		handle = conn_server_init();
		if (handle == NULL) {
			TPT_ERROR("Unable to initialize conn_establish server!");
			return -1;
		}
		/* Start processing ITC messages.
		 * No return.
		 */
		if (signal(SIGTERM, exit_handler) != SIG_ERR) {
			main_loop(handle);
		} else {
			TPT_ERROR("Failed to install signal exit handler");
			return -EFAULT;
		}

		if (mtd_fd >= 0 && close(mtd_fd) == -1) {
			TPT_ERROR(STR("close failed with errno %d", errno));
			return -1;
		}
	}

	if (daemonize) {
		TPT_INFO("daemon exiting");
	}
	return 0;
}


/**
 *   Link handler shell
 *   Link handler shell execution on remote target.
 *
 *   @file itc_lhsh.c
 *
 *   Copyright (C) 2014-2015 by Ericsson AB. All rights reserved. The
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
 *
 *   Revised : 2015-11-03 Fredrik Skog
 *   Change  : TR HU17668: Fixed buffer overruns.
 *             Added this header.
 *
 *   Revised : 2015-11-16 Fredrik Skog
 *   Change  : Fixed problem causing "END" to not be printed after command
 *             output.
 *
 * ========================================================================
 */
//
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/poll.h>
#include <sys/queue.h>
#include <arpa/inet.h>
#include <itc.h>
#include "itc_lhsh.sig"

/* Maximum size of command arguments. */
#define CMD_ARGV_MAX_SIZE 512

union itc_msg {
	uint32_t     sigNo;

	struct ShellInterfaceRequest	if_req;
	struct ShellInterfaceReply	if_reply;
	struct ShellCreateChild		create_child_req;
	struct ShellCreateChild		create_child_reply;
	struct ShellSetenvChildSender	setenv_child_s;
	struct ShellSetenvChild		setenv_child;
	struct ShellStartChild		start_child;
	struct ChildDone		child_done;
	struct FmCancel			fm_cancel_req;
	struct FmCancel			fm_cancel_reply;
	struct FmDupHandle		fm_dup_handle;
	struct FmInterfaceReply		fm_if_reply;

	struct FmWriteRequest		write_req;
	struct FmWriteReply		write_reply;
	struct FmReadRequest		read_req;
	struct FmReadReply		read_reply;
	struct FmCancel			FmCancelRequest;
	struct FmClose			fm_close;
	struct FmSubscribeSignal	fm_subscribe_sig;
	struct FmSetPos			fm_set_pos;
	struct FmSetOwnerProc   fm_setowner_req;
	struct FmSetOwnerProc   fm_setowner_reply;

	struct FmExamineTermRequest	fm_exam_term_req;
	struct FmExamineTermReply	fm_exam_term_reply;
};

struct lhsh_rcmd {
	itc_mbox_id_t shell_pid;

	uint32_t child_pid;
	uint32_t child_ref;
	itc_mbox_id_t owner_pid;

	char *argv;
};

static itc_mbox_id_t owner_pids[0x20];

static itc_mbox_id_t hunt_shelld(char *link)
{
	char name[128];
	union itc_msg *sig;
	itc_mbox_id_t spid;
   char str_shelld[] = {"ose_shelld"};

	if (link && strlen(link)) {
		snprintf(name, sizeof(name), "%s/%s", link, str_shelld);
   } else {
      snprintf(name, sizeof(name), "%s", str_shelld);
   }

	itc_locate_async(name, NULL, ITC_MY_MBOX);

	sig = itc_receive(ITC_NOFILTER, 3000, ITC_FROM_ALL);

	if (sig == NULL)
		return 0;

	if (sig->sigNo != ITC_LOCATE_DEFAULT_NO) {
		printf("unexpected signal 0x%x\n", sig->sigNo);
		return 0;
	}

	spid = itc_sender(sig);
	itc_free(&sig);

	return spid;
}


static inline void check_recv_signal(union itc_msg *sig,
		uint32_t expect)
{
	if (sig->sigNo == ITC_MONITOR_DEFAULT_NO) {
		printf("remote shell died\n");
		exit(-1);
	}
	if (sig->sigNo == expect)
		return;
	printf("unexpected signal 0x%x\n", sig->sigNo);
	exit(-1);
}

static inline int str_add(char *buf, int bufsize, char *str, int *off)
{
	int ret = *off;
	*off += snprintf(buf + *off, bufsize, "%s", str) + 1;
	return ret;
}

static int rcmd_create(char *name,
		char *argv,
		struct lhsh_rcmd *rcmd)
{
	union itc_msg *sig;
	int soff = 0;
   int remain_len = 0;

	/* SHELL_INTERFACE */
	sig = itc_alloc(sizeof(struct ShellInterfaceRequest),
			SHELL_INTERFACE_REQUEST);
	if (!sig)
		exit(-1);

	itc_send(&sig, rcmd->shell_pid, ITC_MY_MBOX);

	sig = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	check_recv_signal(sig, SHELL_INTERFACE_REPLY);

	if (ntohl(sig->if_reply.status) > 1) {
		printf("Warning: ifreq. status code %u ignored.\n",
				ntohl(sig->if_reply.status));
	}

	itc_free(&sig);

	/* SHELL_CREATE_CHILD */
	sig = itc_alloc(sizeof(struct ShellCreateChild) +
         CMD_ARGV_MAX_SIZE,
			SHELL_CREATE_CHILD_REQUEST);

   memset(sig->create_child_req.s, 0, CMD_ARGV_MAX_SIZE);

   remain_len = CMD_ARGV_MAX_SIZE;

	sig->create_child_req.status    = htonl(0);
	sig->create_child_req.child_pid = htonl(0);
	sig->create_child_req.att_ref   = htonl(0);
	sig->create_child_req.options   = htonl(0);
	sig->create_child_req.proc_type = htonl(64); /* PRI_PROC */
	sig->create_child_req.priority  = htonl(28);
	sig->create_child_req.user_id   = htonl(0);
	sig->create_child_req.cmd_name  = htonl(str_add(sig->create_child_req.s, remain_len - soff, name, &soff));
   sig->create_child_req.proc_name = htonl(str_add(sig->create_child_req.s, remain_len - soff, name, &soff));
   sig->create_child_req.argv      = htonl(str_add(sig->create_child_req.s, remain_len - soff, argv, &soff));
   sig->create_child_req.wdir      = htonl(str_add(sig->create_child_req.s, remain_len - soff, "/", &soff));
   sig->create_child_req.user_name = htonl(str_add(sig->create_child_req.s, remain_len - soff, "rbs", &soff));
   sig->create_child_req.terminal  = htonl(str_add(sig->create_child_req.s, remain_len - soff, "", &soff));

	itc_send(&sig, rcmd->shell_pid, ITC_MY_MBOX);

	sig = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	check_recv_signal(sig, SHELL_CREATE_CHILD_REPLY);

	if (sig->create_child_reply.status != 0) {
		printf("SHELL_CREATE_CHILD_REQUEST failed with %u\n",
				ntohl(sig->create_child_reply.status));
		exit(-1);
	}

	rcmd->child_pid = ntohl(sig->create_child_reply.child_pid);
	rcmd->child_ref = ntohl(sig->create_child_reply.att_ref);

	itc_free(&sig);

	return 0;
}

static void rcmd_env_set(struct lhsh_rcmd *rcmd, char *env, ...)
{
	union itc_msg *sig;
   char buf[CMD_ARGV_MAX_SIZE];
	va_list arg;
   int arg_len;

	va_start(arg, env);
   arg_len = vsnprintf(buf, CMD_ARGV_MAX_SIZE, env, arg);
	va_end(arg);

	sig = itc_alloc(sizeof(struct ShellSetenvChildSender) +
			arg_len + 1,
			SHELL_SETENV_CHILD_SENDER);
	sig->setenv_child_s.child_pid = htonl(rcmd->child_pid);
	snprintf(sig->setenv_child_s.assignment, arg_len + 1, "%s", buf);
	itc_send(&sig, rcmd->shell_pid, ITC_MY_MBOX);
}

static void rcmd_env_set_local(struct lhsh_rcmd *rcmd, char *env, ...)
{
	union itc_msg *sig;
   char buf[CMD_ARGV_MAX_SIZE];
	va_list arg;
   int arg_len;

	va_start(arg, env);
   arg_len = vsnprintf(buf, CMD_ARGV_MAX_SIZE, env, arg);
	va_end(arg);
   if (arg_len >= CMD_ARGV_MAX_SIZE) {
      printf("argv length exceeds buffer size; truncated.\n");
   }

	sig = itc_alloc(sizeof(struct ShellSetenvChildSender) +
			arg_len + 1,
			SHELL_SETENV_CHILD);
	sig->setenv_child_s.child_pid = htonl(rcmd->child_pid);
	snprintf(sig->setenv_child_s.assignment, arg_len + 1, "%s", buf);
	itc_send(&sig, rcmd->shell_pid, ITC_MY_MBOX);
}

static int rcmd_env_setup(struct lhsh_rcmd *rcmd, char* cmd_argv)
{

	/* FD<id>=0x<pid>,0x<handle>,0x<bufMode>,0x<bufSize,0x<append> */

	/* stdin */
	rcmd_env_set(rcmd, "FD0=0x%%lx,0x1,1,80,0x0");

	/* stdout */
	rcmd_env_set(rcmd, "FD1=0x%%lx,0x2,1,80,0x0");

	/* stderr */
	rcmd_env_set(rcmd, "FD2=0x%%lx,0x3,1,80,0x0");

	/* parent */
	rcmd_env_set(rcmd, "PARENT=0x%%lx");

	/* */
	rcmd_env_set_local(rcmd, "ARGV=%s", cmd_argv);

	return 0;
}

static int rcmd_start(struct lhsh_rcmd *rcmd)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct ShellStartChild),
			SHELL_START_CHILD);
	sig->start_child.child_pid = htonl(rcmd->child_pid);
	sig->start_child.att_ref = htonl(rcmd->child_ref);

	itc_send(&sig, rcmd->shell_pid, ITC_MY_MBOX);
	return 0;
}

static int rcmd_write_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	union itc_msg *rsig;

	write(1, sig->write_req.buffer, ntohl(sig->write_req.requested));
	rsig = itc_alloc(sizeof(struct FmWriteReply),FM_WRITE_REPLY);
	if (!rsig)
		exit(-1);

	rsig->write_reply.handle = sig->write_req.handle;
	rsig->write_reply.status = 0;
	rsig->write_reply.actual = sig->write_req.requested;

	itc_send(&rsig, itc_sender(sig), ITC_MY_MBOX);
	return 0;
}

static int rcmd_read_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	int ret;
	int size = ntohl(sig->read_req.requested);
	union itc_msg *rsig;

	rsig = itc_alloc(sizeof(struct FmReadReply) + size,FM_READ_REPLY);
	ret = read(0, rsig->read_reply.buffer, size);
	if (ret < 0)
		exit(-1);

	rsig->read_reply.handle = sig->read_req.handle;
	rsig->read_reply.actual = htonl(ret);
	rsig->read_reply.status = 0;

	itc_send(&rsig, itc_sender(sig), ITC_MY_MBOX);
	return 0;
}

static int rcmd_if_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	union itc_msg *rsig;
	uint32_t nsig, *psig, cnt = 0;
	uint32_t sigs[] = {
		0x7594, 0x7596, 0x7598, 0x759a, 0x759c, 0x759e, 0x75a0,
		0x75a2, 0x75a4, 0x75a6, 0x75a8, 0x75aa, 0x75ac, 0x75ae,
	       	0x75b0, 0x75b2, 0x75b4, 0x75b6, 0x75b8, 0x75ba, 0x75bc,
	        0x75be, 0x75c0, 0x75c2, 0x75c4, 0x75c6, 0x75c8, 0x75ca,
	       	0x75cc, 0x75ce, 0x75d0, 0x75d2, 0x75d4, 0x75d6, 0x75d8,
	       	0x75da, 0x75dc, 0x75de, 0x75e0, 0x75e2, 0x75e4, 0x75e6,
	       	0x75e8, 0x75ea, 0x75ec, 0x75ee, /* f0, f2 */ 0x75f4
	};
   char str_console[] = {"SHELL Linux console fm"};

	nsig = sizeof(sigs) / sizeof(uint32_t);
	rsig = itc_alloc(sizeof(struct FmInterfaceReply) +
			sizeof(sigs),
			FM_INTERFACE_REPLY);

	rsig->fm_if_reply.status = 0;
	rsig->fm_if_reply.biosHandle = 0;
   snprintf(rsig->fm_if_reply.whatStr, sizeof(rsig->fm_if_reply.whatStr),
            "%s", str_console);
	rsig->fm_if_reply.sigs[0] = htonl(nsig);

	for (psig = &rsig->fm_if_reply.sigs[1]; cnt < nsig; cnt++)
		*psig++ = htonl(sigs[cnt]);

	itc_send(&rsig, itc_sender(sig), ITC_MY_MBOX);
	rcmd->owner_pid = itc_sender(sig);
	return 0;
}

static int rcmd_dup_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	itc_mbox_id_t from_mbox = itc_sender(sig);
	union itc_msg *rsig;
	FmHandle old, new;

	if (rcmd->owner_pid)
		from_mbox = rcmd->owner_pid;

	old = ntohl(sig->fm_dup_handle.oldHandle);
	new = old + 0x10;

	rsig = itc_alloc(sizeof(struct FmDupHandle),FM_DUP_HANDLE_REPLY);

	rsig->fm_dup_handle.status = 0;
	rsig->fm_dup_handle.oldHandle = htonl(old);
	rsig->fm_dup_handle.newHandle = htonl(new);

	owner_pids[old] = from_mbox;
	owner_pids[new] = from_mbox;

	itc_send(&rsig, from_mbox, ITC_MY_MBOX);
	return 0;
}

static int rcmd_owner_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	itc_mbox_id_t to_mbox;
	union itc_msg *rsig;

	rsig = itc_alloc(sizeof(struct FmSetOwnerProc),
			FM_SET_OWNER_PROC_REPLY);

	rsig->fm_setowner_reply.status = 0;
	rsig->fm_setowner_reply.handle = sig->fm_setowner_req.handle;

	to_mbox = owner_pids[ntohl(sig->fm_setowner_req.handle)];
	owner_pids[ntohl(sig->fm_setowner_req.handle)] = itc_sender(sig);

	itc_send(&rsig, to_mbox, ITC_MY_MBOX);
	return 0;
}


static int rcmd_sub_sig_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	union itc_msg *rsig;

	rsig = itc_alloc(sizeof(struct FmSubscribeSignal),
			FM_SUBSCRIBE_SIGNAL_REPLY);

	rsig->fm_subscribe_sig.status  = 0;
	rsig->fm_subscribe_sig.handle  = sig->fm_subscribe_sig.handle;
	rsig->fm_subscribe_sig.signals = sig->fm_subscribe_sig.signals;

	itc_send(&rsig, itc_sender(sig), ITC_MY_MBOX);
	return 0;
}

static int rcmd_close_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	union itc_msg *rsig;

	rsig = itc_alloc(sizeof(struct FmClose),
			FM_CLOSE_REPLY);
	rsig->fm_close.status = 0;
	rsig->fm_close.handle = sig->fm_close.handle;
	rsig->fm_close.cancel = sig->fm_close.cancel;

	itc_send(&rsig, itc_sender(sig), ITC_MY_MBOX);
	return 0;
}

static int rcmd_set_pos_req(struct lhsh_rcmd *rcmd, union itc_msg *sig)
{
	union itc_msg *rsig;

	rsig = itc_alloc(sizeof(struct FmSetPos),FM_SET_POS_REPLY);
	rsig->fm_set_pos.status = 0;
	rsig->fm_set_pos.handle = sig->fm_set_pos.handle;
	rsig->fm_set_pos.whence = sig->fm_set_pos.whence;
	rsig->fm_set_pos.offset = sig->fm_set_pos.offset;
	rsig->fm_set_pos.position = 0;

	itc_send(&rsig, itc_sender(sig), ITC_MY_MBOX);
	return 0;
}


static int lhsh(char *link, char* proc_name, char* cmd_argv)
{
	struct lhsh_rcmd rcmd;
	struct pollfd fds[2];
	int nfds, linx_fd, ret;
	union itc_msg *sig;
	int has_in;
	union itc_msg *read_req;

	rcmd.owner_pid = 0;
	rcmd.shell_pid = hunt_shelld(link);
	if (!rcmd.shell_pid) {
		printf("unable to hunt %s/ose_shelld\n", link);
		return -1;
	}

	/* attach */
	itc_monitor(rcmd.shell_pid, NULL);

	if (rcmd_create(proc_name, cmd_argv, &rcmd)) {
		printf("unable to create remote process\n");
		return -1;
	}
	if (rcmd_env_setup(&rcmd, cmd_argv)) {
		printf("unable to setup env\n");
		return -1;
	}
	if (rcmd_start(&rcmd)) {
		printf("unable to start remote process\n");
		return -1;
	}

	linx_fd = itc_get_fd();
	has_in = 0;
	read_req = NULL;


	while (1) {
		nfds = 0;

		/* linx */
		fds[nfds].fd = linx_fd;
		fds[nfds].events = POLLIN;
		fds[nfds].revents = 0;
		nfds++;

		if (read_req) {
			fds[nfds].fd = 0;
			fds[nfds].events = POLLIN | POLLPRI;
			fds[nfds].revents = 0;
			nfds++;
		}

		ret = poll(fds, nfds, 1000);
		if (ret < 0) {
			perror(0);
			exit(-1);
		}
		if (!ret)
			continue;

		if (nfds > 1 && fds[1].revents)
			has_in = 1;

		do {
			if (!fds[0].revents)
				break;

			sig = itc_receive(ITC_NOFILTER, ITC_NO_TMO,
					ITC_FROM_ALL);

			switch (sig->sigNo) {
			case FM_INTERFACE_REQUEST:
				rcmd_if_req(&rcmd, sig);
				break;
			case FM_SUBSCRIBE_SIGNAL_REQUEST:
				rcmd_sub_sig_req(&rcmd, sig);
				break;
			case FM_DUP_HANDLE_REQUEST:
				rcmd_dup_req(&rcmd, sig);
				break;
			case FM_SET_OWNER_PROC_REQUEST:
				rcmd_owner_req(&rcmd, sig);
				break;
			case FM_CLOSE_REQUEST:
				rcmd_close_req(&rcmd, sig);
				break;
			case FM_WRITE_REQUEST:
				rcmd_write_req(&rcmd, sig);
				break;
			case FM_SET_POS_REQUEST:
				rcmd_set_pos_req(&rcmd, sig);
				break;
			case FM_READ_REQUEST:
				read_req = sig;
				sig = NULL;
				break;
			case CHILD_DONE:
				printf("END\n");
				return 0;
				break;
			case ITC_MONITOR_DEFAULT_NO:
				printf("END\n");
				return 0;
				break;

			default:
				break;
			}

			if (sig)
				itc_free(&sig);

		} while (0);

		if (has_in && read_req) {
			rcmd_read_req(&rcmd, read_req);
			itc_free(&read_req);
			has_in = 0;
		}
	}


	return 0;
}

int main(int argc, char **argv)
{
	char* proc_name;
	itc_mbox_id_t me;
   char cmd_argv[CMD_ARGV_MAX_SIZE] = "";
	char lhsh_name[32];
	int ret, i, /*len,*/ remain_len;

	ret = itc_init(32, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n", ret);
		return -1;
	}

	snprintf(lhsh_name, sizeof(lhsh_name), "lhsh_mbox_%p", (void*)getpid());

	me = itc_create_mailbox(lhsh_name, 0);
	if (me == ITC_NO_ID) {
		printf("create mailbox failure: %d\n", me);
		itc_exit();
		return -1;
	}

	if (argc < 2) {
		printf("Usage: %s <link_path>\n", argv[0]);
		return 0;
	}

	if (argc == 2){
		proc_name = "shell";
		snprintf(cmd_argv, strlen(cmd_argv), "%s", proc_name);
	}
	else {
		proc_name = argv[2];
      remain_len = sizeof(cmd_argv);
		for (i = 2; i < argc; i++){
         strncat(cmd_argv, argv[i], remain_len);
         remain_len -= strlen(argv[i]);
         strncat(cmd_argv, " ", remain_len);
         remain_len -= strlen(" ");
		}
	}

	lhsh(argv[1], proc_name, cmd_argv);
	itc_delete_mailbox(me);
	itc_exit();
	return 0;
}

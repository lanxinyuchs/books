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
#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <argz.h>

#include "mamacmd.h"
#include "cmd-socket.h"

static int read_response(int sockfd)
{
	struct cmd_socket_header cmdh;
	struct cmd_socket_rsp rsp;

	if (cmd_socket_read_buf(sockfd, &cmdh, sizeof(cmdh)) < 0)
		return -1;

	if (cmdh.msgno != CMD_SOCKET_RSP ||
	    cmdh.size < sizeof(cmdh) + sizeof(rsp))
		return -2;

	if (cmd_socket_read_buf(sockfd, &rsp, sizeof(rsp)) < 0)
		return -1;

	return rsp.result;
}

mamacmd_handle mamacmd_connect(void)
{
	struct sockaddr_un addr;
	char *socket_name = getenv(CMD_SOCKET_ENV);
	int fd;

	fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (fd < 0)
		return NULL;

	if (!socket_name)
		socket_name = CMD_SOCKET;

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, socket_name, sizeof(addr.sun_path) - 1);

	if (connect(fd, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
		close(fd);
		return NULL;
	}

	return (mamacmd_handle) ((intptr_t) fd + 1);
}

int mamacmd_child_start(mamacmd_handle h, int id)
{
	struct cmd_socket_start cmd;
	int sockfd = (intptr_t) h - 1;
	int result;

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_START, sizeof(cmd)) < 0)
		return -1;

	cmd.id = id;
	if (cmd_socket_write_buf(sockfd, &cmd, sizeof(cmd)) < 0)
		return -1;

	result = read_response(sockfd);
	if (result < 0)
		return -1;
	else if (result == 1)
		return -2;

	return 0;
}

int mamacmd_child_stop(mamacmd_handle h, int id)
{
	struct cmd_socket_stop cmd;
	int sockfd = (intptr_t) h - 1;
	int result;

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_STOP, sizeof(cmd)) < 0)
		return -1;

	cmd.id = id;
	if (cmd_socket_write_buf(sockfd, &cmd, sizeof(cmd)) < 0)
		return -1;

	result = read_response(sockfd);
	if (result < 0)
		return -1;
	else if (result == 1)
		return -2;

	return 0;
}

int mamacmd_domain_start(mamacmd_handle h, char *domain)
{
	struct cmd_socket_dstart *cmd;
	size_t size = offsetof(struct cmd_socket_dstart, name) + strlen(domain) + 1;
	int sockfd = (intptr_t) h - 1;
	int result;

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_DSTART, size) < 0)
		return -1;

	cmd = malloc(size);
	if (!cmd)
		return -1;

	strcpy(cmd->name, domain);

	if (cmd_socket_write_buf(sockfd, cmd, size) < 0) {
		free(cmd);
		return -1;
	}

	free(cmd);

	result = read_response(sockfd);
	if (result < 0)
		return -1;
	else if (result == 1)
		return -2;

	return 0;
}

int mamacmd_domain_stop(mamacmd_handle h, char *domain)
{
	struct cmd_socket_dstop *cmd;
	size_t size = offsetof(struct cmd_socket_dstop, name) + strlen(domain) + 1;
	int sockfd = (intptr_t) h - 1;
	int result;

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_DSTOP, size) < 0)
		return -1;

	cmd = malloc(size);
	if (!cmd)
		return -1;

	strcpy(cmd->name, domain);

	if (cmd_socket_write_buf(sockfd, cmd, size) < 0) {
		free(cmd);
		return -1;
	}

	free(cmd);

	result = read_response(sockfd);
	if (result < 0)
		return -1;
	else if (result == 1)
		return -2;

	return 0;
}


int mamacmd_status(mamacmd_handle h, struct mamacmd_status_entry **entries)
{
	struct cmd_socket_status_rsp rsp;
	struct mamacmd_status_entry *e = NULL;
	int sockfd = (intptr_t) h - 1;
	int result;
	int status = -1;

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_STATUS, 0) < 0)
		goto exit;

	result = read_response(sockfd);
	if (result < 0)
		goto exit;
	else if (result == 1) {
		status = -2;
		goto exit;
	}

	if (cmd_socket_read_buf(sockfd, &rsp, sizeof(rsp)) < 0)
		goto exit;

	e = malloc(sizeof(struct mamacmd_status_entry) * (rsp.nbr + 1));
	if (!e)
		goto exit;

	for (int i = 0; i < rsp.nbr; i++) {
		struct cmd_socket_status_entry_header eh;
		size_t size;
		int msgno;

		msgno = cmd_socket_read_header(sockfd, &size);
		if (msgno != CMD_SOCKET_STATUS_ENTRY)
			goto exit;

		if (size < sizeof(struct cmd_socket_header) + sizeof(eh))
			goto exit;

		if (cmd_socket_read_buf(sockfd, &eh, sizeof(eh)) < 0)
			goto exit;

		size_t toread = size - sizeof(struct cmd_socket_header) - sizeof(eh);
		if (toread <= 0)
			goto exit;

		char *buf = malloc(toread);
		if (!buf)
			goto exit;

		if (cmd_socket_read_buf(sockfd, buf, toread) < 0) {
			free(buf);
			goto exit;
		}

		strncpy(e[i].cmd, buf, MAMACMD_MAX_ENTRY_STRING_SIZE);

		if (eh.in_domain) {
			char *p = buf + strlen(buf) + 1;
			strncpy(e[i].domain, p, MAMACMD_MAX_ENTRY_STRING_SIZE);
		}

		free(buf);

		e[i].cmdstopped = eh.cmdstopped;
		e[i].domain_cmdstopped = eh.domain_cmdstopped;
		e[i].in_domain = eh.in_domain;
		e[i].run_until_completion = eh.run_until_completion;
		e[i].id = eh.id;
		e[i].pid = eh.pid;
	}

	/* Make sure last entry is 0 to terminate */
	memset(&e[rsp.nbr], 0, sizeof(e[rsp.nbr]));

	*entries = e;
	status = rsp.nbr;

exit:
	if (status < 0 && e)
		free(e);

	return status;
}

/*
 * By using the first element 'size' we don't need to rev all clients
 * using this interface when a new configuration option is added.
 */
#define ADD(cfg, n, cmd, field) do {                                        \
	if ((cfg)->size >= sizeof(int) * (n))                                   \
		(cmd)->field = (cfg)->field;                                        \
} while (0)

int mamacmd_launch(mamacmd_handle h, char * const argv[], char *domain,
                   struct mamacmd_launch_config *cfg)
{
	struct cmd_socket_launch *cmd = NULL;
	int sockfd = (intptr_t) h - 1;
	char *argz = NULL;
	size_t argz_len;
	size_t size;
	int status = -1;
	int result;
	char *ptr;

	if (argz_create(argv, &argz, &argz_len) != 0)
		goto exit;
	argz_stringify(argz, argz_len, '&');

	size = sizeof(struct cmd_socket_launch) + argz_len;
	if (domain)
		size += strlen(domain) + 1;

	cmd = malloc(size);
	if (!cmd)
		goto exit;

	ADD(cfg, 2, cmd, retries);
	ADD(cfg, 3, cmd, retry_timeout_ms);
	ADD(cfg, 4, cmd, abort_timeout_ms);
	ADD(cfg, 5, cmd, alive_timeout_ms);
	ADD(cfg, 6, cmd, wait_timeout_ms);
	ADD(cfg, 7, cmd, run_until_completion);
	ADD(cfg, 8, cmd, affinity);

	/* Step over the struct */
	ptr = (char *) (cmd + 1);
	strcpy(ptr, argz);

	if (domain) {
		ptr += argz_len;
		strcpy(ptr, domain);
	}

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_LAUNCH, size) < 0)
		goto exit;

	if (cmd_socket_write_buf(sockfd, cmd, size) < 0)
		goto exit;

	result = read_response(sockfd);
	if (result < 0)
		goto exit;
	else if (result == 1) {
		status = -2;
		goto exit;
	}

	status = 0;

exit:
	if (cmd)
		free(cmd);

	if (argz)
		free(argz);

	return status;
}

int mamacmd_shutdown(mamacmd_handle h)
{
	int sockfd = (intptr_t) h - 1;
	int result;

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_SHUTDOWN, 0) < 0)
		return -1;

	result = read_response(sockfd);

	if (result < 0)
		return -1;
	else if (result == 1)
		return -1;

	return 0;
}

int mamacmd_disconnect(mamacmd_handle h)
{
	int sockfd = (intptr_t) h - 1;

	if (cmd_socket_write_header(sockfd, CMD_SOCKET_CLOSE, 0) < 0)
		return -1;

	close(sockfd);

	return 0;
}

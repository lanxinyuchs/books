#ifndef __CMD_SOCKET_H
#define __CMD_SOCKET_H

#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>

#define CMD_SOCKET_ENV "MAMACMD_SOCKET"
#define CMD_SOCKET "/tmp/mamactrl"

/* reponse values */
#define CMD_SOCKET_RSP_SUCCESS  0
#define CMD_SOCKET_RSP_FAILURE  1

/* msgno */
#define CMD_SOCKET_RSP          0xee
#define CMD_SOCKET_CLOSE        0xff
#define CMD_SOCKET_STATUS       0x01
#define CMD_SOCKET_STATUS_ENTRY 0x02
#define CMD_SOCKET_START        0x10
#define CMD_SOCKET_STOP         0x20
#define CMD_SOCKET_DSTART       0x30
#define CMD_SOCKET_DSTOP        0x40
#define CMD_SOCKET_LAUNCH       0x50
#define CMD_SOCKET_SHUTDOWN     0x60

/* Every command and response starts with this header */
struct cmd_socket_header {
	uint32_t msgno;
	size_t size;
};

/* A generic response */
struct cmd_socket_rsp {
	int result;
};

/* Follows the generic response for a status reponse */
struct cmd_socket_status_rsp {
	int nbr;
};

/*
 * Followed by nullterminated string with the command name, in turn followed by
 * a nullterminated string containing the domain name if applicable.
 * The total size of the message is given in cmd_socket_header.size.
 */
struct cmd_socket_status_entry_header {
	bool cmdstopped;
	bool domain_cmdstopped;
	bool in_domain;
	bool run_until_completion;
	int id;
	pid_t pid;
};

/* A start command */
struct cmd_socket_start {
	int id;
};

/* A stop command */
struct cmd_socket_stop {
	int id;
};

/* A start command */
struct cmd_socket_dstart {
	char name[1];
};

/* A stop command */
struct cmd_socket_dstop {
	char name[1];
};

/*
 * Followed by a nullterminated string with the argv as a string and the
 * domain name if applicable.
 * The total size of the message is given in cmd_socket_header.size.
 */
struct cmd_socket_launch {
	int retries;
	int retry_timeout_ms;
	int abort_timeout_ms;
	int alive_timeout_ms;
	int wait_timeout_ms;
	int run_until_completion;
	int affinity;
};

extern int cmd_socket_write_buf(int sockfd, void *buf, size_t len);
extern int cmd_socket_read_buf(int sockfd, void *buf, size_t len);
extern int cmd_socket_write_header(int sockfd, int msgno, size_t size);
extern int cmd_socket_read_header(int sockfd, size_t *sizep);

#endif /* __CMD_SOCKET_H */

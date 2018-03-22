#include <stdint.h>
#include <unistd.h>

#include "cmd-socket.h"

int cmd_socket_write_buf(int sockfd, void *buf, size_t len)
{
	while (len) {
		ssize_t cnt = write(sockfd, buf, len);
		if (cnt < 0)
			return -1;

		len -= cnt;
	}

	return 0;
}

int cmd_socket_read_buf(int sockfd, void *buf, size_t len)
{
	char *b = (char *) buf;

	while (len) {
		ssize_t cnt = read(sockfd, b, len);
		if (cnt <= 0)
			return -1;

		len -= cnt;
		b += cnt;
	}

	return 0;
}

int cmd_socket_write_header(int sockfd, int msgno, size_t append_size)
{
	struct cmd_socket_header cmdh;

	cmdh.msgno = msgno;
	cmdh.size = sizeof(cmdh) + append_size;
	if (cmd_socket_write_buf(sockfd, &cmdh, sizeof(cmdh)) < 0)
		return -1;

	return 0;
}

int cmd_socket_read_header(int sockfd, size_t *sizep)
{
	struct cmd_socket_header cmdh;

	if (cmd_socket_read_buf(sockfd, &cmdh, sizeof(cmdh)) < 0)
		return -1;

	if (cmdh.size < sizeof(cmdh))
		return -1;

	*sizep = cmdh.size;
	return cmdh.msgno;
}

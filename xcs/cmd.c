#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <argz.h>

#include "cmd-socket.h"
#include "cmd.h"
#include "log.h"
#include "child.h"
#include "domain.h"

static int send_response(int sockfd, int result, size_t append_size)
{
	struct cmd_socket_rsp rsp;

	append_size += sizeof(rsp);
	if (cmd_socket_write_header(sockfd, CMD_SOCKET_RSP, append_size) < 0) {
		log_info("Failed to write header to socket");
		return -1;
	}

	rsp.result = result;
	if (cmd_socket_write_buf(sockfd, &rsp, sizeof(rsp)) < 0) {
		log_info("Failed to write response to socket");
		return -1;
	}

	return 0;
}

static struct child * find_child_from_id(int id, struct state *s)
{
	struct child *ch;
	int cnt = 0;

	child_foreach(ch, cglist, s) {
		if (cnt++ == id)
			return ch;
	}

	return NULL;
}

int check_affinity(int affinity)
{
	FILE *fp = NULL;
	int num_cores;
	int status = -1;

	fp = popen("nproc", "r");
	if (!fp) {
		log_info("Failed to execute nproc (%s)", strerror(errno));
		goto exit;
	}

	if (fscanf(fp, "%d", &num_cores) != 1) {
		log_info("Failed to parse nproc output\n");
		goto exit;
	}

	if (affinity + 1 > num_cores) {
		log_info("Tried to set affinity %d, but only %d cores available",
		         affinity, num_cores);
		goto exit;
	}

	status = 0;

exit:
	if (fp) pclose(fp);

	return status;
}

static void status(int sockfd, struct state *s, size_t size)
{
	struct cmd_socket_status_rsp rsp;
	struct child *ch;
	int id = 0;

	if (size < sizeof(struct cmd_socket_header)) {
		log_info("Size of status command too small");
		return;
	}

	if (send_response(sockfd, CMD_SOCKET_RSP_SUCCESS,
	                  sizeof(struct cmd_socket_status_rsp)) < 0)
		return;

	rsp.nbr = child_count(cglist, s);
	if (cmd_socket_write_buf(sockfd, &rsp, sizeof(rsp)) < 0) {
		log_info("Failed to write status response to socket");
		return;
	}

	child_foreach(ch, cglist, s) {
		struct cmd_socket_status_entry_header entry_header;
		struct domain *d = ch->domain;
		size_t append_size;

		entry_header.pid = ch->pid;
		entry_header.cmdstopped = ch->cmdstopped;
		if (d)
			entry_header.domain_cmdstopped = d->cmdstopped;
		else
			entry_header.domain_cmdstopped = false;
		entry_header.id = id++;
		entry_header.run_until_completion = ch->run_until_completion;
		append_size = sizeof(entry_header) + strlen(ch->argv[0]) + 1;

		if (d) {
			append_size += strlen(d->name) + 1;
			entry_header.in_domain = true;
		}
		else {
			entry_header.in_domain = false;
		}

		if (cmd_socket_write_header(sockfd, CMD_SOCKET_STATUS_ENTRY,
		                          append_size) < 0) {
			log_info("Failed to write header to socket");
			return;
		}

		if (cmd_socket_write_buf(sockfd, &entry_header, sizeof(entry_header)) < 0) {
			log_info("Failed to write entry header to socket");
			return;
		}

		if (cmd_socket_write_buf(sockfd, ch->argv[0],
		                       strlen(ch->argv[0]) + 1) < 0) {
			log_info("Failed to write status entry command to socket");
			return;
		}

		if (d) {
			if (cmd_socket_write_buf(sockfd, d->name, strlen(d->name) + 1) < 0) {
				log_info("Failed to write status entry domain to socket");
				return;
			}
		}
	}
}

static void start_child(int sockfd, struct state *s, size_t size)
{
	struct cmd_socket_start cmd;
	struct child *ch;

	if (size < sizeof(struct cmd_socket_header) + sizeof(cmd)) {
		log_info("Size of start command too small");
		return;
	}

	if (cmd_socket_read_buf(sockfd, &cmd, sizeof(cmd)) < 0) {
		log_info("Failed to read start cmd from socket");
		return;
	}

	ch = find_child_from_id(cmd.id, s);
	if (!ch) {
		log_info("Failed to locate child with id %d", cmd.id);
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
		return;
	}

	if (!child_is_stopped(ch))
		return;

	child_append(ch, cllist, s);
	ch->cmdstopped = false;

	send_response(sockfd, CMD_SOCKET_RSP_SUCCESS, 0);
}

static void stop_child(int sockfd, struct state *s, size_t size)
{
	struct cmd_socket_stop cmd;
	struct child *ch;

	if (size < sizeof(struct cmd_socket_header) + sizeof(cmd)) {
		log_info("Size of stop command too small");
		return;
	}

	if (cmd_socket_read_buf(sockfd, &cmd, sizeof(cmd)) < 0) {
		log_info("Failed to read stop cmd from socket");
		return;
	}

	ch = find_child_from_id(cmd.id, s);
	if (!ch) {
		log_info("Failed to locate child with id %d", cmd.id);
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
		return;
	}

	child_set_state(ch, CHILD_STATE_TERMINATED);
	ch->cmdstopped = true;

	send_response(sockfd, CMD_SOCKET_RSP_SUCCESS, 0);
}

static void start_domain(int sockfd, struct state *s, size_t size)
{
	struct cmd_socket_dstart *cmd = NULL;
	struct domain *d;

	if (size < sizeof(struct cmd_socket_header) +
	           offsetof(struct cmd_socket_dstart, name)) {
		log_info("Size of domain start command too small");
		goto error;
	}

	size -= sizeof(struct cmd_socket_header);

	cmd = malloc(size);
	if (!cmd)
		goto error;

	if (cmd_socket_read_buf(sockfd, cmd, size) < 0) {
		log_info("Failed to read stop cmd from socket");
		goto error;
	}

	d = domain_get_by_name(cmd->name, s);
	if (!d) {
		log_info("Failed to locate domain %s", cmd->name);
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
		goto error;
	}

	if (!d->cmdstopped) {
		log_info("stop cmd of domain %s failed as domain isn't stopped");
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
		goto error;
	}

	domain_launch(d, s);
	d->cmdstopped = false;

	send_response(sockfd, CMD_SOCKET_RSP_SUCCESS, 0);

error:
	if (cmd) free(cmd);
}

static void stop_domain(int sockfd, struct state *s, size_t size)
{
	struct cmd_socket_dstop *cmd = NULL;
	struct domain *d;

	if (size < sizeof(struct cmd_socket_header) +
	           offsetof(struct cmd_socket_dstop, name)) {
		log_info("Size of domain stop command too small");
		goto error;
	}

	size -= sizeof(struct cmd_socket_header);

	cmd = malloc(size);
	if (!cmd)
		goto error;

	if (cmd_socket_read_buf(sockfd, cmd, size) < 0) {
		log_info("Failed to read stop cmd from socket");
		goto error;
	}

	d = domain_get_by_name(cmd->name, s);
	if (!d) {
		log_info("Failed to locate domain %s", cmd->name);
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
		goto error;
	}

	if (d->state != DOMAIN_STATE_STARTED) {
		log_info("stop cmd of domain %s failed as domain is shut down");
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
		goto error;
	}

	domain_set_state(d, DOMAIN_STATE_TERMINATED);
	d->cmdstopped = true;

	send_response(sockfd, CMD_SOCKET_RSP_SUCCESS, 0);

error:
	if (cmd) free(cmd);
}

static void launch(int sockfd, struct state *s, size_t size)
{
	struct cmd_socket_launch *cmd = NULL;
	struct child *ch;
	char *ptr;
	char *argz = NULL;
	size_t argz_len;
	char **argv = NULL;
	struct child_config cfg;
	struct domain *d = NULL;
	char *argz_entry = NULL;
	int status = -1;
	int argc;
	int i = 0;

	if (size < sizeof(struct cmd_socket_header) +
	           sizeof(struct cmd_socket_launch)) {
		log_info("Size of launch command too small");
		goto error;
	}

	size -= sizeof(struct cmd_socket_header);

	cmd = malloc(size);
	if (!cmd)
		goto error;

	if (cmd_socket_read_buf(sockfd, cmd, size) < 0) {
		log_info("Failed to read stop cmd from socket");
		goto error;
	}

	ptr = (char *) (cmd + 1);
	argz_create_sep(ptr, '&', &argz, &argz_len);

	if (size > sizeof(struct cmd_socket_launch) + strlen(ptr) + 1) {
		ptr += strlen(ptr) + 1;
		d = domain_get_by_name(ptr, s);
		if (!d) {
			log_info("domain %s does not exist", ptr);
			goto error;
		}
	}

	argc = argz_count(argz, argz_len);
	argv = malloc((argc + 1) * sizeof(char *));
	if (!argv)
		goto error;

	/* Allocate the args */
	for (i = 0; i < argc; i++) {
		argz_entry = argz_next(argz, argz_len, argz_entry);
		if (!argz_entry) break; /* Keep coverity happy, we don't need this */
		argv[i] = strdup(argz_entry);
		if (!argv[i])
			goto error;
	}
	argv[i] = 0;

	if (access(argv[0], X_OK) < 0) {
		log_info("%s is not a valid executable file", argv[0]);
		goto error;
	}

	cfg.retries = cmd->retries;
	cfg.retry_timeout_ms = cmd->retry_timeout_ms;
	cfg.abort_timeout_ms = cmd->abort_timeout_ms;
	cfg.alive_timeout_ms = cmd->alive_timeout_ms;
	cfg.wait_timeout_ms = cmd->wait_timeout_ms;
	cfg.run_until_completion = cmd->run_until_completion;
	cfg.affinity = cmd->affinity;

	if (cfg.affinity >= 0) {
		if (check_affinity(cfg.affinity) < 0)
			goto error;
	}

	ch = child_create(argv, d, &cfg);
	if (!ch)
		goto error;

	/* Add child to global and launch lists */
	child_append(ch, cglist, s);
	child_append(ch, cllist, s);

	status = 0;

error:
	if (status < 0) {
		if (argv) {
			for (i = i - 1; i >= 0; i--)
				free(argv[i]);
			free(argv);
		}
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
	}
	else
		send_response(sockfd, CMD_SOCKET_RSP_SUCCESS, 0);

	if (cmd) free(cmd);
	if (argz) free(argz);
}

static void _shutdown(int sockfd, struct state *s, size_t size)
{
	struct child *ch;

	if (size < sizeof(struct cmd_socket_header)) {
		log_info("Size of launch command too small");
		send_response(sockfd, CMD_SOCKET_RSP_FAILURE, 0);
	}

	ch = child_last_running(cglist, s);

	if (ch)
		child_set_state(ch, CHILD_STATE_TERMINATED);

	timestamp(&s->shutdown_ts);

	s->shutdown = true;
	s->cmdshutdown = true;

	send_response(sockfd, CMD_SOCKET_RSP_SUCCESS, 0);
}

int service_cmd_client(int *client_cmd_sockfdp, struct state *s)
{
	uint32_t msgno;
	size_t size;

	msgno = cmd_socket_read_header(*client_cmd_sockfdp, &size);

	switch (msgno) {
		case CMD_SOCKET_START:
			log_info("Received 'start child' command");
			start_child(*client_cmd_sockfdp, s, size);
			break;
		case CMD_SOCKET_STOP:
			log_info("Received 'stop child' command");
			stop_child(*client_cmd_sockfdp, s, size);
			break;
		case CMD_SOCKET_DSTART:
			log_info("Received 'start domain' command");
			start_domain(*client_cmd_sockfdp, s, size);
			break;
		case CMD_SOCKET_DSTOP:
			log_info("Received 'stop domain' command");
			stop_domain(*client_cmd_sockfdp, s, size);
			break;
		case CMD_SOCKET_STATUS:
			log_info("Received 'status' command");
			status(*client_cmd_sockfdp, s, size);
			break;
		case CMD_SOCKET_LAUNCH:
			log_info("Received 'launch' command");
			launch(*client_cmd_sockfdp, s, size);
			break;
		case CMD_SOCKET_SHUTDOWN:
			log_info("Received 'shutdown' command");
			_shutdown(*client_cmd_sockfdp, s, size);
			break;
		case CMD_SOCKET_CLOSE:
			log_info("Received 'close' command");
			close(*client_cmd_sockfdp);
			*client_cmd_sockfdp = -1;
			break;
		default:
			return -1;
	}

	return 0;
}

int accept_cmd_client(int server_cmd_sockfd)
{
	return accept(server_cmd_sockfd, NULL, NULL);
}

int open_cmd_server_socket()
{
	struct sockaddr_un addr;
	int result = -1;
	char *socket_name = getenv(CMD_SOCKET_ENV);
	int fd;

	fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (fd < 0) {
		log_err("failed to create socket");
		goto exit;
	}

	if (!socket_name)
		socket_name = CMD_SOCKET;

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, socket_name, sizeof(addr.sun_path) - 1);

	if (access(socket_name, W_OK) == -1) {
		if (errno != ENOENT) {
			log_err("Error accessing %s (errno %d)\n", socket_name, errno);
			goto exit;
		}
	}
	else {
		if (unlink(socket_name) < 0) {
			log_err("Failed to unlink %s", socket_name);
			goto exit;
		}
	}

	if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
		log_err("Failed to bind socket");
		goto exit;
	}

	if (listen(fd, 1) == -1) {
		log_err("Failed to listen on socket");
		goto exit;
	}

	result = 0;

exit:
	if (result < 0 && fd >= 0) {
		close(fd);
		return result;
	}

	return fd;
}

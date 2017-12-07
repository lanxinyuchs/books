/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <errno.h>

#include "ComtEComm_1.h"
#include "ComtEUtils_1.h"

#define BERT_HEADER_LENGTH 4
#define MAX_QUEUED_CONNECTIONS 5



MafReturnT comte_connect(char* host, char *port, comte_con_handle_t* con_handle) {
    struct linger so_linger;
    struct addrinfo hints;
    struct addrinfo *result = NULL;

    memset (&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
    hints.ai_flags = AI_NUMERICSERV;


    int gai_ret = getaddrinfo(host, port, &hints, &result);
    if (gai_ret != 0) {
        if (!con_handle->quiet) {
            ERROR("ERROR, getting address of host: %s",
                  gai_strerror(gai_ret));
        }
        return MafFailure;
    }

    con_handle->sockfd = socket(result->ai_family,
                                result->ai_socktype,
                                result->ai_protocol);
    if (con_handle->sockfd < 0) {
        if (!con_handle->quiet) {
            ERROR("ERROR opening socket: %s", strerror(errno));
        }
        freeaddrinfo(result);
        return MafFailure;
    }

    int rv;
    do {
        rv = connect(con_handle->sockfd,
                     result->ai_addr,
                     result->ai_addrlen);
    } while (rv < 0 && errno == EINTR);

    freeaddrinfo(result);

    if (rv < 0) {
        if (! con_handle->quiet) {
            ERROR("ERROR connecting: %s", strerror(errno));
        }
        return MafFailure;
    }

    /* This linger trick is not pretty and you can read about it in
     * a comment in comte_bert_server:cast_to_com/3.
     */
    so_linger.l_onoff = !0;
    so_linger.l_linger = 0;
    rv = setsockopt(con_handle->sockfd, SOL_SOCKET, SO_LINGER,
                    &so_linger, sizeof(so_linger));
    if (rv < 0) {
        if (! con_handle->quiet) {
            ERROR("ERROR setsockopt SOL_SOCKET:SO_LINGER: %s",
                  strerror(errno));
        }
        return MafFailure;
    }
    return MafOk;

}


MafReturnT comte_send(comte_buff_t* buff, comte_con_handle_t* con_handle) {
	ssize_t n;
	uint32_t netlen;
	struct iovec outv[2];
	int k, outv_size;

	netlen = htonl(buff->size);
	outv[0].iov_base = &netlen;
	outv[0].iov_len = BERT_HEADER_LENGTH;
	outv[1].iov_base = buff->buff;
	outv[1].iov_len = buff->size;
	outv_size = 2;

	for (k = 0;  k < outv_size;) {
		n = writev(con_handle->sockfd, outv + k, outv_size - k);
		if (n < 0) {
			if (errno == EINTR) continue;
			if (! con_handle->quiet) ERROR("ERROR writing to socket: %s",
                                                       strerror(errno));
			return MafFailure;
		}
		while (k < outv_size) {
			if (n < outv[k].iov_len) {
				outv[k].iov_len -= n;
				outv[k].iov_base =
                    ((char *) (outv[k].iov_base)) + n;
				break;
			}
			else {
				n -= outv[k].iov_len;
				k++;
			}
		}
	}
	return MafOk;

}

static int comte_read_length
(void* buff, uint32_t length, comte_con_handle_t *con_handle) {
	ssize_t n, tot_read = 0;
	do {

		do {
			n = read
                (con_handle->sockfd, ((char *) buff) + tot_read,
                 length - tot_read);
		} while (n < 0 && errno == EINTR);

		if (n < 0) {

                    if (! con_handle->quiet) ERROR("ERROR reading: %s",
                                                   strerror(errno));
                    return n;

		}

		if (n == 0) {
			if (! con_handle->quiet) {
                            ERROR("Remote side closed unexpectedly when reading: %s",
                                  strerror(errno));
			}
			return n;
		}
		tot_read += n;
	} while (tot_read != length);
	return tot_read;
}


MafReturnT comte_recv(comte_buff_t* buff, comte_con_handle_t* con_handle) {
	ssize_t n;
	n = comte_read_length(&buff->size, BERT_HEADER_LENGTH, con_handle);

	if (n <= 0) return MafFailure;

	buff->size = ntohl(buff->size);

	buff->buff = comte_malloc(sizeof(uint8_t) * (buff->size));
	if (buff->buff == NULL) return MafFailure;

	n = comte_read_length(buff->buff, buff->size, con_handle);

	if (n <= 0) {
		comte_free(buff->buff);
		buff->buff = NULL;
		return MafFailure;
	}

	return MafOk;
}


void comte_recv_close(comte_con_handle_t* con_handle) {
    int n;
    do {
	char b;
	n = read(con_handle->sockfd, &b, 1);
    } while (n < 0 && errno == EINTR);

    if (n == 0) return;
    if (n < 0 && errno == ECONNRESET) return;

    ERROR("Did not read EOF");
}

MafReturnT comte_send_recv(comte_buff_t* buff, comte_con_handle_t* con_handle) {
	MafReturnT res = comte_send(buff, con_handle);
	comte_free(buff->buff);
	buff->buff = NULL;
	if (res != MafOk)
		return res;

	return comte_recv(buff, con_handle);
}

MafReturnT comte_disconnect(comte_con_handle_t* con_handle) {
	int rv;
	do {
		rv = close(con_handle->sockfd);
	} while (rv < 0 && errno == EINTR);
	return MafOk;
}


MafReturnT comte_connect_send_recv
(char* hostname, char *portno, comte_buff_t* buff, int quiet) {
	MafReturnT res;
	comte_con_handle_t* connection;

	res = MafOk;
	connection = comte_malloc(sizeof(comte_con_handle_t));
	connection->quiet = quiet;

        res = comte_connect(hostname, portno, connection);
	if (res != MafOk) {
		if (! connection->quiet) ERROR("comte_connect failed");
                comte_free(connection);
		return res;
	}

	res = comte_send_recv(buff, connection);
	if (res != MafOk) {
            if (! connection->quiet) ERROR("comte_send_recv failed");
        }

	comte_disconnect(connection);
	comte_free(connection);
	return res;
}

MafReturnT comte_listen(char *addr, char *portno, comte_con_handle_t *listen_handle) {
        struct addrinfo hints;
        struct addrinfo *result, *rp;

        memset(&hints, 0, sizeof(struct addrinfo));
        /* Use IPv4 only for now */
        hints.ai_family = AF_INET;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_protocol = IPPROTO_TCP;
        hints.ai_flags = AI_NUMERICSERV;

        int gai_ret = getaddrinfo(addr, portno, &hints, &result);
        if (gai_ret != 0) {
            ERROR("getaddrinfo failed: %s\n", gai_strerror(gai_ret));
            return MafFailure;
        }

        int sock_ret = -1;
        for (rp = result; rp != NULL; rp = rp->ai_next) {
            sock_ret = socket(rp->ai_family, rp->ai_socktype,
                              rp->ai_protocol);
            if(sock_ret == -1)
                continue;

            /* Socket options */
            int on = 1;
            if (setsockopt(sock_ret, SOL_SOCKET,
                           SO_REUSEADDR, &on, sizeof(on) ) < 0) {
                if (! listen_handle->quiet)
                    ERROR("Error setting SO_REUSEADDR: %s",
                          strerror(errno));

                close(sock_ret);
                continue;
            }

            if(bind(sock_ret, rp->ai_addr, rp->ai_addrlen) == 0){
                /* Bind OK */
                listen_handle->sockfd = sock_ret;
                break;
            }

            ERROR("ERROR on bind: %s", strerror(errno));
            close(sock_ret);
        }

        freeaddrinfo(result);

        if(rp == NULL){
            if (! listen_handle->quiet)
                ERROR("ERROR opening socket: %s",
                      strerror(errno));
            return MafFailure;
        }

      	if (listen(listen_handle->sockfd, MAX_QUEUED_CONNECTIONS) < 0) {
            if (! listen_handle->quiet)
                ERROR("Failed to start listening: %s",
                      strerror(errno));
            return MafFailure;
	}
	return MafOk;

}


MafReturnT comte_accept(comte_con_handle_t* listen_handle,
		comte_con_handle_t* con_handle) {

	fd_set fdset;
	FD_ZERO(&fdset);
	FD_SET(listen_handle->sockfd, &fdset);

	struct timeval timeout;
	timeout.tv_sec = 1;
	timeout.tv_usec = 0;

	int select_res;
	do {
		select_res = select(listen_handle->sockfd + 1, &fdset, &fdset, &fdset,
				&timeout);
	} while (select_res < 0 && errno == EINTR);

	if (select_res == 0)
		return MafTimeOut;
	else if (select_res < 0)
		return MafFailure;

	struct sockaddr_in cli_addr;
	unsigned int clilen = sizeof(cli_addr);

	do {
		con_handle->sockfd = accept(listen_handle->sockfd,
				(struct sockaddr *) &cli_addr, &clilen);
	} while (con_handle->sockfd < 0 && errno == EINTR);

	if (con_handle->sockfd < 0) {
            if (! con_handle->quiet) ERROR("ERROR on accept: %s",
                                           strerror(errno));
		return MafFailure;

	}
	return MafOk;
}

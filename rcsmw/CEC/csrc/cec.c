/* ----------------------------------------------------------------------
 * %CCaseFile:	cec.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/R8A/1 %
 * %CCaseDate:	2016-11-10 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Implementation of the C side of the CEC TCP connection facility.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and disseminations to the receivers employees shall
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R2A/10     2013-02-20 erarafo     Created
 * R4A/2      2015-10-12 etxberb     TR HU24243: Changed from variable length
 *                                   of signature msgs to fixed length (16).
 * R5A/1      2016-03-04 etxarnu     Changed ERROR to INFO in send_/recv_fill
 * R5A/2      2016-03-08 erarafo     Fixed compiler warnings
 * R8A/1      2016-11-10 etxpeno     improve tracing
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <sys/time.h>

#include "cec.h"

#define TRACEPOINT_DEFINE
#include "cec_trace.h"

#define ENTER() tracepoint(com_ericsson_rcs_cec, enter, __func__)
#define ERROR_TRACE(arg) tracepoint(com_ericsson_rcs_cec, error_trace, \
				    __FILE__, __LINE__, __func__, arg)
#define ERROR_TRACE_W_INT_ARG(arg, int) tracepoint(com_ericsson_rcs_cec,  \
						   error_trace_w_int_arg, \
						   __FILE__, __LINE__,    \
						   __func__, arg, int)
#define ERROR_TRACE_W_STR_ARG(arg, str) tracepoint(com_ericsson_rcs_cec,  \
						   error_trace_w_str_arg, \
						   __FILE__, __LINE__,    \
						   __func__, arg, str)
#define WARNING_TRACE(arg) tracepoint(com_ericsson_rcs_cec, warning_trace, \
				      __FILE__, __LINE__, __func__, arg)
#define WARNING_TRACE_W_STR_ARG(arg, str) tracepoint(com_ericsson_rcs_cec,    \
						     warning_trace_w_str_arg, \
						     __FILE__, __LINE__,      \
						     __func__, arg, str)

static int cec_receive_internal(cec_handle_t *handle, cec_packet_t *packet,
				unsigned int timeout);
static int cec_send_fill(int socket, const void *data, size_t size);
static int cec_recv_fill(int socket, void *data, size_t size);
static char *cec_port();
static char *cec_host();

#define CEC_HOST_KEY ("CEC_HOST")
#define CEC_PORT_KEY ("CEC_PORT")
#define CEC_PORT_DEFAULT ("9803")

cec_handle_t *
cec_open(void *name, size_t size) {

  cec_handle_t *handle = NULL;

  ENTER();

  if (name == NULL) {
    ERROR_TRACE("name == NULL");
    return NULL;
  }

  if (size > 16) {
    ERROR_TRACE_W_INT_ARG("Too long signature", size);
    return NULL;
  }

  int socketfd = socket(AF_INET, SOCK_STREAM, 0);
  if (socketfd < 0) {
    ERROR_TRACE_W_STR_ARG("Failed to create socket", strerror(errno));
    return NULL;
  }

  char *node = cec_host();
  char *service = cec_port();
  struct addrinfo hints = {
    .ai_family = AF_INET,
    .ai_socktype = SOCK_STREAM,
    .ai_flags = AI_NUMERICSERV
  };
  struct addrinfo *addrs;
  int result = getaddrinfo(node, service, &hints, &addrs);
  if (result != 0) {
    ERROR_TRACE_W_STR_ARG("Failed to get address info", strerror(errno));
    goto error;
  }

  /*
   * Search the resulting addresses until a usable
   * entry is found. If no usable entry is found
   * the value of r will remain negative.
   */
  int r1 = -1;
  for (struct addrinfo *addr = addrs;
       addr != NULL && r1 < 0;
       addr = addr->ai_next) {
    do {
      r1 = connect(socketfd, addrs->ai_addr, addrs->ai_addrlen);
    } while (r1 < 0 && errno == EINTR);
  }

  if (addrs != NULL)
    freeaddrinfo(addrs);

  if (r1 < 0) {
    ERROR_TRACE("Could not connect");
    goto error;
  }

  handle = malloc(sizeof(*handle));
  if (handle == NULL) {
    ERROR_TRACE_W_STR_ARG("could not create CEC handle", strerror(errno));
    goto error;
  }

  handle->socket = socketfd;

  int option_value = 1;
  socklen_t option_len = sizeof(option_value);

  if (setsockopt(socketfd, IPPROTO_TCP, TCP_NODELAY,
                 &option_value, option_len) != 0) {
    ERROR_TRACE_W_STR_ARG("failed to set socket options", strerror(errno));
    goto error;
  }

  char signature[16] ;
  memset(signature, 0, 16);
  memcpy(signature, name, size);
  int r2 = -1;
  r2 = cec_send_fill(handle->socket, signature, 16);
  if (r2 == -1) {
    ERROR_TRACE("failed to send signature");
    goto error;
  }

  return handle;

 error:
  free(handle);
  close(socketfd);
  return NULL;
}

int
cec_close(cec_handle_t *handle) {
  ENTER();

  if (handle == NULL) {
    ERROR_TRACE("handle == NULL");
    return -1;
  }

  close(handle->socket);
  free(handle);
  return 0;
}

int
cec_send(cec_handle_t *handle, cec_packet_t *packet) {
  ENTER();

  if (handle == NULL) {
    ERROR_TRACE("handle == NULL");
    return -1;
  }

  if (packet == NULL) {
    ERROR_TRACE("packet == NULL");
    return -1;
  }

  size_t packet_size = htonl(packet->length);
  if (cec_send_fill(handle->socket, &packet_size, sizeof(packet->length)) < 0)
    return -1;

  if (cec_send_fill(handle->socket, packet->data, packet->length) < 0)
    return -1;

  return packet->length;
}

int
cec_send_with_pid(cec_handle_t *handle, cec_packet_t *packet) {
  ENTER();

  if (handle == NULL) {
    ERROR_TRACE("handle == NULL");
    return -1;
  }

  if (packet == NULL) {
    ERROR_TRACE("packet == NULL");
    return -1;
  }

  size_t packet_size = htonl(packet->length + sizeof(uint32_t));
  if (cec_send_fill(handle->socket, &packet_size, sizeof(packet->length)) < 0)
    return -1;

  pid_t pid = getpid();

  if (cec_send_fill(handle->socket, (uint32_t*)&pid, sizeof(uint32_t)) < 0)
    return -1;

  if (cec_send_fill(handle->socket, packet->data, packet->length) < 0)
    return -1;

  return packet->length;
}

int
cec_receive(cec_handle_t *handle, cec_packet_t *packet) {
  ENTER();

  return cec_receive_internal(handle, packet, 0);
}

int
cec_receive_w_tmeout(cec_handle_t *handle, cec_packet_t *packet,
		     unsigned int timeout) {
  ENTER();

  return cec_receive_internal(handle, packet, timeout);
}

static int
cec_receive_internal(cec_handle_t *handle, cec_packet_t *packet,
		     unsigned int timeout) {

  if (packet == NULL) {
    ERROR_TRACE("packet == NULL");
    return -1;
  }

  if (handle == NULL) {
    ERROR_TRACE("handle == NULL");
    goto ERROR;
  }

  if (timeout != 0) {
    struct timeval tv;
    tv.tv_sec = timeout / 1000;
    tv.tv_usec = 1000 * (timeout % 1000);

    int r = setsockopt(handle->socket, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv);
    if (r != 0) {
     ERROR_TRACE_W_STR_ARG("failed to set socket options", strerror(errno));
     goto ERROR;
    }
  }

  if (cec_recv_fill(handle->socket, &packet->length,
		    sizeof(packet->length)) < 0)
    goto ERROR;

  packet->length = ntohl(packet->length);
  packet->data = malloc(packet->length);

  if (packet->data == NULL) {
    ERROR_TRACE("Could not allocate memory");
    goto ERROR;
  }

  if (cec_recv_fill(handle->socket, packet->data, packet->length) < 0) {
    free(packet->data);
    goto ERROR;
  }

  return packet->length;

 ERROR:
  packet->data = NULL;
  packet->length = 0;
  return -1;
}

static int
cec_send_fill(int socket, const void *data, size_t size) {
  int n;
  size_t i = 0;

  while (i < size) {

    do {
      n = sendto(socket, ((unsigned char *)data) + i, size - i, MSG_NOSIGNAL,
		 NULL, 0);
    } while (n < 0 && errno == EINTR);

    if (n < 0) {
      WARNING_TRACE_W_STR_ARG("failed to send", strerror(errno));
      return n;
    } else if (n == 0) {
      WARNING_TRACE("failed to send, 0 bytes sent");
      return -1;
    }

    i += n;
  }

  return size;
}

static int
cec_recv_fill(int socket, void *data, size_t size) {
  int n = 0;
  size_t i = 0;

  while (i < size) {
    do {
      n = recv(socket, ((unsigned char *)(data)) + i, size - i, 0);
    } while (n < 0 && errno == EINTR);

    if (n < 0) {
      WARNING_TRACE_W_STR_ARG("failed to receive", strerror(errno));
      return n;
    } else if (n == 0) {
      WARNING_TRACE("failed to receive, got 0 bytes");
      return -1;
    }

    i += n;
  }
  return n;
}

/**
 * Returns the value of the environment variable
 * CEC_PORT, or a default value if the environment
 * variable is not set. In either case the returned
 * value should be a string containing a decimal
 * integer.
 */
static char *
cec_port()
{
  char *value = getenv(CEC_PORT_KEY);

  if (value == NULL) {
    return CEC_PORT_DEFAULT;
  }

  return value;
}

/**
 * Returns the value of the environment variable
 * CEC_HOST, or NULL if no value is set. The value
 * NULL is valid as the 1st argument of getaddrinfo,
 * implying the loopback address.
 */
static char *
cec_host() {
  return getenv(CEC_HOST_KEY);
}

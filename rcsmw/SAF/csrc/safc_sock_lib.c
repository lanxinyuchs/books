/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2012. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *  Purpose : SAFC socket related Functions
 * ----------------------------------------------------------------------
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>

#include <saAis.h>
#include "safc_trace.h"
#include "safc_sock_lib.h"

#define SAFSRV_HEADER_LENGTH 4
#define MAX_QUEUED_CONNECTIONS 5

int safc_socket_trace_level = 1;

int safc_connect(char* host, int port)
{
   struct sockaddr_in serv_addr;
   struct hostent *server;
   int rv;
   int sockfd;

   sockfd = socket(AF_INET, SOCK_STREAM, 0);
   if (sockfd < 0) {
      TRACE_ERROR(("ERROR opening socket\n"));
      return sockfd;
   }

   server = gethostbyname(host);
   if (server == NULL) {
     TRACE_ERROR(("ERROR, no such host!\n"));
      return -1;
   }
   memset((char *) &serv_addr, 0, sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   memmove((char *) &serv_addr.sin_addr.s_addr, (char *) server->h_addr_list[0],
	   server->h_length);
   serv_addr.sin_port = htons(port);

   do {
      rv = connect(sockfd,
		   (const struct sockaddr *) &serv_addr,
		   sizeof(serv_addr));
   } while (rv < 0 && errno == EINTR);
   if (rv < 0) {
     TRACE_ERROR(("ERROR connecting!\n"));
      return rv;
   }

   return sockfd;
}

int safc_send(int sockfd, safc_buf_t* buf)
{
   int n, netlen;
   SaUint8T *header_and_data = malloc(sizeof(SaUint8T)*(buf->size+SAFSRV_HEADER_LENGTH));

   netlen = htonl(buf->size);
   memcpy(header_and_data, &netlen, SAFSRV_HEADER_LENGTH);
   memcpy(header_and_data + SAFSRV_HEADER_LENGTH, buf->buf, buf->size);

   do {
      n = send(sockfd,
	       header_and_data,
	       buf->size + SAFSRV_HEADER_LENGTH,
	       MSG_NOSIGNAL);
   } while (n < 0 && errno == EINTR);

   if (n < 0) {
     TRACE_ERROR(("ERROR writing to socket\n"));
      free(header_and_data);
      return SA_AIS_ERR_MESSAGE_ERROR;
   }
   free(header_and_data);
   return SA_AIS_OK;
}

int safc_read_length(int sockfd, void* buf, SaUint32T length)
{
   int n, tot_read = 0;
   do {
      do {
	 n = read(sockfd, (void*)((char*) buf + tot_read), length - tot_read);
      } while (n < 0 && errno == EINTR);

      if (n < 0) {
	TRACE_ERROR(("ERROR reading!\n"));
	 return n;
      }
      if (n == 0) {
	TRACE_ERROR(("Remote side closed unexpectedly when reading!\n"));
	 return n;
      }
      tot_read += n;
   } while (tot_read != length);
   return tot_read;
}

int safc_recv(int sockfd, safc_buf_t* buf)
{
   int n;
   n = safc_read_length(sockfd, &buf->size, SAFSRV_HEADER_LENGTH);

   if (n <= 0)
      return SA_AIS_ERR_MESSAGE_ERROR;

   TRACE2(("Length Read!\n"));

   buf->size = ntohl(buf->size);

   buf->buf = malloc(sizeof(SaUint8T) * (buf->size));
   TRACE2(("Buf allocated!\n"));
   n = safc_read_length(sockfd, buf->buf, buf->size);
   TRACE2(("Msg Read: %d!\n", n));

   if (n <= 0) {
      return SA_AIS_ERR_MESSAGE_ERROR;
   }

   return SA_AIS_OK;
}

int safc_send_recv(int sockfd, safc_buf_t* buf)
{
   int res = safc_send(sockfd, buf);

   if (res != SA_AIS_OK)
      return res;

   free(buf->buf);
   buf->buf = NULL;

   return safc_recv(sockfd, buf);
}

int safc_disconnect(int sockfd)
{
   int rv;

   do {
      rv = close(sockfd);
   } while (rv < 0 && errno == EINTR);

   return SA_AIS_OK;
}

int safc_connect_send_recv(char* hostname, int port, safc_buf_t* buf)
{

  int sockfd = 0;
  int res;

  sockfd = safc_connect(hostname, port);
  if (sockfd < 0)
    {
      TRACE_ERROR(("safc_connect failed\n"));
      return sockfd;
    }

  res = safc_send_recv(sockfd, buf);
  if (res != SA_AIS_OK)
    {
      TRACE_ERROR(("safc_send_recv failed\n"));
      safc_disconnect(sockfd);
      return res;
    }

  safc_disconnect(sockfd);

  return res;
}

in_port_t safc_protobuf_port(const char *env_var, in_port_t default_port)
{
  in_port_t port;
  char *value;

  value = getenv(env_var);

  if (value == NULL)
    return default_port;

  port = strtol(value, (char**)NULL, 10);

  if (port > 0 && port < 65536)
    return port;

  return default_port;
}


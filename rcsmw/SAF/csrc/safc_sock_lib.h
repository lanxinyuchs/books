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
 *  Purpose : SAF Common Library
 * ----------------------------------------------------------------------
 *
 */
#ifndef _SAFC_SOCK_LIB_H
#define _SAFC_SOCK_LIB_H
#include <netinet/in.h>
#include <saAis.h>

typedef struct safc_buf_t
{
  int size;
  char* buf;
} safc_buf_t;


extern int safc_connect(char* host, int port);
extern int safc_send(int sockfd, safc_buf_t* buf);
extern int safc_read_length(int sockfd, void* buf, SaUint32T length);
extern int safc_recv(int sockfd, safc_buf_t* buf);
extern int safc_send_recv(int sockfd, safc_buf_t* buf);
extern int safc_disconnect(int sockfd);
extern int safc_connect_send_recv(char* hostname, int port, safc_buf_t* buf);
extern in_port_t safc_protobuf_port(const char *env_var, in_port_t default_port);

#endif

/* ----------------------------------------------------------------------
 * %CCaseFile:	pgh_restart_board.c %
 * %CCaseRev:	/main/R2A/5 %
 * %CCaseDate:	2014-10-07 %
 * %CCaseDocNo: %
 * Author: Per Norberg
 *
 * Short description:
 * This programs restarts the board
 * pgh_restart_board [-t] [-p pghd_port] [-r reason] [-e]
 * If -t is set, a cold restart with test is executed, otherwise a
 * cold restart is executed
 * If -e is set, the bootcounter in EE is incremented.
 * 
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014 All rights reserved.
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
  ****************************************************************************
 * Description:
  *
 * Rev        Date         Name        What
 * -----      -------      --------    ------------------------
 * R2A/1      2013-08-29   etxpeno     First version
 * R2A/5      2014-10-07   etxarnu     Added -e option for escalation
 * ----------------------------------------------------------------------
 */
/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB 2013-2014 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#define _XOPEN_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <strings.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#define BUFLEN (7*sizeof(uint32_t))
#define MSGLEN (6*sizeof(uint32_t))

static int send_fill(int socket, const void *data, size_t size);

int
main(int argc, char *argv[])
{
  int sockfd;
  struct sockaddr_in servaddr;
  int opt;
  int tcp_port = 56789; /* port number on target */
  pid_t pid;
  int hwtest = 0;
  int escalate = 0;
  char *reason = NULL;
  size_t reason_len = 0;
  char *buf;
  char *ptr;
  uint32_t type = 50; /* PGH_IF_MSG_RESTARTBRD */
  int seq = 0;
  int reserved = 0;
  int option_value = 1;
  socklen_t option_len = sizeof(option_value);
  size_t buf_len = BUFLEN;
  uint32_t msg_len = MSGLEN;

  while ((opt = getopt(argc, argv, "p:ter:")) != -1) {
    switch (opt) {
    case 'p':
      tcp_port = atoi(optarg);
      break;
    case 't':
      hwtest = 1;
      break;
    case 'e':
      escalate = 1;
      break;
    case 'r':
      reason = optarg;
      reason_len = strlen(reason)+1;
      buf_len += reason_len;
      msg_len += reason_len;
      break;
    }
  }

  buf = malloc(buf_len);
  if (buf == NULL)
    exit(EXIT_FAILURE);

  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror("socket");
    exit(EXIT_FAILURE);
  }

  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(tcp_port);
  inet_pton(AF_INET, "127.0.0.1", &servaddr.sin_addr);

  if (connect(sockfd, (const struct sockaddr *)&servaddr, sizeof(servaddr)) < 0) {
    perror("connect");
    exit(EXIT_FAILURE);
  }

  if (setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY,
                 &option_value, option_len) < 0) {
    perror("setsockopt");
    exit(EXIT_FAILURE);
  }

  pid = htonl(getpid());
  if (send_fill(sockfd, &pid, sizeof(pid)) < 0)
    exit(EXIT_FAILURE);

  ptr = buf;

  *(uint32_t*)ptr = htonl(msg_len);
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = type;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = msg_len;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = seq;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = reserved;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = hwtest;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = escalate;
  ptr += sizeof(uint32_t);

  if (reason != NULL)
    strcpy(ptr, reason);

  if (send_fill(sockfd, buf, buf_len) < 0) {
    exit(EXIT_FAILURE);
  }

  if (close(sockfd) < 0) {
    exit(EXIT_FAILURE);
  }

  return 0;
}

static int
send_fill(int socket, const void *data, size_t size) {
  int n;
  size_t i = 0;

  while (i < size) {
    do {
      n = sendto(socket, ((unsigned char *)data) + i, size - i, 0, NULL, 0);
    } while (n < 0 && errno == EINTR);

    if (n < 0) {
      perror("sendto");
      return n;
    } else if (n == 0) {
      perror("sendto");
      return -1;
    }

    i += n;
  }

  return size;
}

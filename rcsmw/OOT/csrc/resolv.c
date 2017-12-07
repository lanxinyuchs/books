/* ----------------------------------------------------------------------
 * %CCaseFile:	resolv.c %
 * %CCaseRev:	/main/R3A/R4A/R6A/R12A/1 %
 * %CCaseDate:	2017-10-26 %
 * %CCaseDocNo: %
 * Author:
 * Author: Lars Carlsson, <lars.carlsson@ericsson.com>
 *
 * Short description:
 * Started in relevant network namespace (through APPM), receives requests
 * for lookup (getaddrinfo(...)), returns the result to ootResolver.
 * UDP is used between this program and ootResolver.
 * usage: resolv [-d ] -n <Namespace> -p <Port>
 * <Port> is the UDP portnumber where  ootResolver listens
 * <Namespace> is the string that the program uses to "check-in" on UDP
 * -d will just cause a lot of printing, useful during test (not with APPM)
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
 * R3A/1      2015-02-09 etxlg       Created
 * R4A/1      2015-11-18 etxlg       IPv6 and more chrome
 * R6A/1      2016-08-26 etxpeno     Coverity fix
 * R12A/1     2017-10-24 etxlg       SP338? return all addresses
 * ----------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>	/*duh*/
#include <netdb.h>
#include <string.h>	/*memset*/
#include <arpa/inet.h>	/*inet_ntop*/
#include <errno.h>

#define DEBUG_FILE "/tmp/resolv_debug.txt"

/* corresponds to what is in ootResolver.erl */
enum	{ DUMMY = 0,
	  REPLY_NS = 1,
	  RES_REP_4 = 2,
	  RES_REP_6 = 3,
	  RES_REP_ALL_4 = 4,
	  RES_REP_ALL_6 = 5,
	  ERR_NO_RET = 128,
	  ERR_RETURN = 129
};

int debug_fd = -1;
int debug = 0;

void
fdebug(const char *format, ...)
{
  va_list arg;
  char write_buf[1000];
  int len;

  if(! debug) return;

  va_start(arg, format);
  vfprintf(stderr, format, arg);
  if(debug_fd != -1){
    if(debug == 1){
      write(debug_fd, "File closed\n", strlen("File closed\n"));
      close(debug_fd);
      debug_fd = -1;
    }else{
      debug--;
      len = vsnprintf(write_buf, 1000, format, arg);
      len = len > 1000 ? 1000 : len;
      if (len >= 0) write(debug_fd, write_buf, len);
    }
  }
  va_end(arg);
}

int err_to_eai_macro(char * buf, int error)
{
  char *s;

  switch(error){
  case EAI_BADFLAGS:	/* invalid ai_flags */
    s = "EAI_BADFLAGS";
    break;
  case EAI_NONAME:	/*  name (or service) unknown */
    s = "EAI_NONAME";
    break;
  case EAI_AGAIN:	/* temporary failure */
    s = "EAI_AGAIN";
    break;
  case EAI_FAIL:	/* non-recoverable failure */
    s = "EAI_FAIL";
    break;
  case EAI_FAMILY:	/* unsupported ai_family */
    s = "EAI_FAMILY";
    break;
  case EAI_SOCKTYPE:	/* unsupported ai_socktype */
    s = "EAI_SOCKTYPE";
    break;
  case EAI_SERVICE:	/* unsupported service for ai_socktype */
    s = "EAI_SERVICE";
    break;
  case EAI_MEMORY:	/* mem alloc fail */
    s = "EAI_MEMORY";
    break;
  case EAI_SYSTEM:	/* system err (in errno) */
    s = "EAI_SYSTEM";
    break;
  case EAI_OVERFLOW:	/* arg buffer overflow */
    s = "EAI_OVERFLOW";
    break;
  default:
    s = "EAI_UNKNOWN";
    /*
      and a bunch more that are __USE_GNU
    */
  }
  fdebug("getaddrinfo: gai_strerror(%d): %s, %s\n",
	 error, s, gai_strerror(error));
  memcpy(buf, s, strlen(s)); /* not including '\0' */
  return strlen(s);
}

void print_ai(int count, struct addrinfo *ap)
{
  /*
    struct sockaddr_in *si;
    uint32_t ipadr;
  */
  char buf[1024];
  void *address;

  fdebug("Addrinfo: %d\n", count);
  fdebug("ai_flags: %d\n", ap->ai_flags);
  fdebug("ai_family: %d\n", ap->ai_family);
  fdebug("ai_socktype: %d\n", ap->ai_socktype);
  fdebug("ai_protocol: %d\n", ap->ai_protocol);
  fdebug("ai_addrlen: %d\n", ap->ai_addrlen);
  if(ap->ai_canonname)
    fdebug("ai_canonname: %s\n", ap->ai_canonname);
  else
    fdebug("ai_canonname: %s\n", "NULL pointer");

  switch (ap->ai_family){
  case AF_INET:
    fdebug("This is IPv4\n");
    address = &((struct sockaddr_in*)ap->ai_addr)->sin_addr;
    break;
  case AF_INET6:
    fdebug("This is IPv6\n");
    address = &((struct sockaddr_in6*)ap->ai_addr)->sin6_addr;
    break;
  default:
    fdebug("unexpected, ai_family: %d\n", ap->ai_family);
    return;
    break;
  }

  if(inet_ntop(ap->ai_family,
	       address,
	       /* &si->sin_addr.s_addr, */
	       buf, sizeof buf) != NULL){
    fdebug("Address as string: %s\n", buf);
  }else{
    perror("inet_ntop");
  }
}

int
open_udp(struct sockaddr_in *dest, char *nns)
{
  int sock, len;
  int ret = -1;
  socklen_t useless = sizeof(struct sockaddr_in);
  struct sockaddr_in src;
  char *buf;


  len = strlen(nns) + 1;
  buf = malloc(len);
  if(!buf){
    perror("malloc");
    return -1;
  }
  buf[0] = REPLY_NS;
  memcpy(&buf[1], nns, len - 1);

  memset(&src, 0, sizeof src);
  src.sin_family = AF_INET;
  src.sin_port = 0;
  src.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if(sock == -1){
    perror("socket");
    goto errout;
  }
  ret = bind(sock, (struct sockaddr*)&src, sizeof src);
  if(ret == -1){
    perror("bind");
    close(sock);
    goto errout;
  }
  ret = getsockname(sock, (struct sockaddr*)&src, &useless);
  if(ret == -1){
    perror("getsockname");
    close(sock);
    goto errout;
  }
  fdebug("Socket bound to port: %hu\n",
	 ntohs(src.sin_port));
  ret = sendto(sock, buf, len, 0,
	       (struct sockaddr*)dest,
	       sizeof(struct sockaddr_in));
  if(ret == -1){
    perror("sendto");
    close(sock);
    goto errout;
  }

  ret = sock;

 errout:
  free(buf);
  return ret;
}

void
respond(int sock, struct sockaddr_in *dest, char *buf, int len)
{
  int ret;
  ret = sendto(sock, buf, len, 0,
	       (struct sockaddr*)dest,
	       sizeof(struct sockaddr_in));
  if(ret == -1){
    perror("respond: sendto");
  }
}

int
main(int argc, char **argv)
{
  struct sockaddr_in dest;
  struct addrinfo *ares, hints, *ap;
  int sock;
  int ret, i, len ;
  uint32_t port = 0;
  uint32_t  seq_no;
  char *nns = NULL;
  char buf[1024], *bufp;

  for(i = 1; i < argc; i++){
    if(argv[i][0] == '-' && argv[i][1] == 'd'){
      debug = 1000;
      debug_fd = open(DEBUG_FILE, O_CREAT | O_TRUNC | O_WRONLY,
		      S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH);
      if(debug_fd == -1) {
	perror("open");
	fdebug("debug is active but write to file disabled\n");
      }else{
	fdebug("debug is active, write to file: %s\n",
	       DEBUG_FILE);
      }
      continue;
    }
    if(argv[i][0] == '-' && argv[i][1] == 'p' && (i + 1) < argc){
      port = (uint32_t)atoi(argv[i + 1]);
      i++;
      fdebug("Server is at port: %hu\n", port);
      continue;
    }
    if(argv[i][0] == '-' && argv[i][1] == 'n' && (i + 1) < argc){
      nns = argv[i + 1];
      fdebug("My net_ns is: %s\n", nns);
      i++;
      continue;
    }
  }

  if(!port || !nns){
    fdebug("need -p <Port> and -n <Net_ns>\n");
    return -1;
  }

  dest.sin_family = AF_INET;
  dest.sin_port = htons((uint16_t)port);
  dest.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

  sock = open_udp(&dest, nns);
  if(sock == -1){
    fdebug("Failed doing open_udp - exiting\n");
    return -2;
  }

  memset(&hints, 0, sizeof hints);
  hints.ai_flags = AI_CANONNAME;
  hints.ai_family = AF_INET;
  hints.ai_socktype = 0;
  hints.ai_protocol = 0;

  while(1){
    fdebug("Waiting in recv\n");
    ret = recv(sock, &buf, sizeof(buf), 0);
    fdebug("recv: %d\n", ret);
    if(ret < 6){
      fdebug("Short[%d] resolve jobb received - exiting\n", ret);
      return -3;
    }
    if((unsigned)(ret + 1) > sizeof buf){
      fdebug("Impossible length[%d] resolve job received - exiting\n",
	     ret);
      return -4;
    }
    fdebug("Type: %hu\n", (unsigned char)buf[0]);
    memcpy(&seq_no, &buf[1], sizeof seq_no);
    fdebug("JobId: %u\n", seq_no);
    buf[ret] = '\0';
    fdebug("Hostname to resolve: %s\n", &buf[5]);
    if((unsigned char)buf[0] == RES_REP_4 ||
       (unsigned char)buf[0] == RES_REP_ALL_4){
      hints.ai_family = AF_INET;
      fdebug("Resolving IPv4, hints.ai_family: %u\n",
	     hints.ai_family);
    }else{
      if((unsigned char)buf[0] == RES_REP_6 ||
	 (unsigned char)buf[0] == RES_REP_ALL_6){
	hints.ai_family = AF_INET6;
	fdebug("Resolving IPv6, hints.ai_family: %u\n",
	       hints.ai_family);
      }else{
	fdebug("badness: %hu\n, exiting...\n",
	       (unsigned char) buf[0]);
	return -5;
      }
    }

    ret = getaddrinfo(&buf[5], NULL, &hints, &ares);
    if(ret != 0){
      fdebug("getaddrinfo: %d, %s\n", ret, strerror(errno));
      buf[0] = (unsigned char)ERR_RETURN;
      memcpy(&buf[1], &seq_no, 4);
      len = err_to_eai_macro(&buf[5], ret);
      respond(sock, &dest, buf, 5 + len);
      /*freeaddrinfo(ares); */
      continue;
    }
    if(ares == NULL){
      fdebug("no results from getaddrinfo\n");
      buf[0] = (unsigned char)ERR_NO_RET;
      memcpy(&buf[1], &seq_no, sizeof seq_no);
      respond(sock, &dest, buf, 5);
      freeaddrinfo(ares);
      continue;
    }
    if(debug){
      for(i = 0, ap = ares; ap != NULL; ap = ap->ai_next, i++){
	print_ai(i, ap);
      }
    }
    memcpy(&buf[1], &seq_no, sizeof seq_no);
    /* switch(hints.ai_family){ */
    switch(buf[0]){
    /* case AF_INET:
      buf[0] = RES_REP_4; */
    case RES_REP_4:
      fdebug("Answering with IPv4\n");
      memcpy(&buf[5], &((struct sockaddr_in*) ares->ai_addr)->sin_addr.s_addr, 4);
      respond(sock, &dest, buf, 9);
      break;
    /* case AF_INET6:
      buf[0] = RES_REP_6; */
    case RES_REP_6:
      fdebug("Answering with IPv6\n");
      memcpy(&buf[5], &((struct sockaddr_in6*)ares->ai_addr)->sin6_addr, 16);
      respond(sock, &dest, buf, 21);
      break;
    case RES_REP_ALL_4:
      fdebug("Answering with ALL IPv4\n");
      for(bufp = &buf[5], ap = ares;
	  ap != NULL &&  (bufp - buf) < (sizeof buf - 4);
	  ap = ap->ai_next, bufp += 4){
	  memcpy(bufp, &((struct sockaddr_in*)ap->ai_addr)->sin_addr.s_addr, 4);
      }
      respond(sock, &dest, buf, bufp - buf);
      break;
    case RES_REP_ALL_6:
      fdebug("Answering with ALL IPv6\n");
      for(bufp = &buf[5], ap = ares;
	  ap != NULL &&  (bufp - buf) < (sizeof buf  - 16);
	  ap = ap->ai_next, bufp += 16){
	  memcpy(bufp, &((struct sockaddr_in6*)ap->ai_addr)->sin6_addr, 16);
      }
      respond(sock, &dest, buf, bufp - buf);
      break;
    default:
      break;
    }
    freeaddrinfo(ares);
    fdebug("loop back\n");
  } /*end of infinite loop*/
  return 0; /*not reached*/
}

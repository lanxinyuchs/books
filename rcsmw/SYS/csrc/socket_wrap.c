/*---------------------------------------------------------------------------
 * %CCaseFile:	socket_wrap.c %
 * %CCaseRev:	/main/R2A/1 %
 * %CCaseDate:	2014-05-07 %
 * %CCaseDocNo:	69/190 55-CNA 113 171 Ux %
 * Author: Sebastian Strollo <seb@erix.ericsson.se>
 */
/*
 * This was derived from setuid_socket_wrap.c as used by the SBG, which in
 * turn is a slightly modified version of the setuid_socket_wrap.c file 
 * included in the Erlang/OTP distribution.
 * It has been modified to be directly run by user root (i.e. not using setuid
 * file permissions) and do setuid to unprivilegued user based on cmd-line
 * arguments.
 * Name changed to socket_wrap.c
 * etxlg
 */

/* Short description: 
 * Program  executed as root. It extract args as below, opens the sockets
 * removes the args and add new ones containing the fd # of the just opened
 * socket(s). It then execs the EXEC_PROGRAM with UID taken from -u argument.
 * setuid_socket_wrap.c
 *
 * ./a.out -u <user name> [-s [tag,][addr]:[port]]* [-d [tag,][addr]:[port]]* 
 *         [-r [tag,]proto]* -- program args
 *
 * Where: -s = stream socket, -d datagram socket and -r means raw socket.
 *
 *-----------------------------------------------------------------------------
 * Template Id: ETX/B 
 * 
 * Copyright (C) 1996 by Ericsson Telecom AB
 * S - 125 26 STOCKHOLM
 * SWEDEN, tel int + 46 8 719 0000
 *
 * This program may be used and/or copied only with the written permission
 * from Ericsson Telecom AB, or in accordance with the terms and 
 * conditions stipulated in the agreement/contract under which the program
 * has been supplied.
 *
 * All rights reserved.
 *
 * Copyright (c) Ericsson AB 2014 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 *-----------------------------------------------------------------------------
 *
 * Rev	   Date	     Name    What
 * ---	   --------  ----    ----
 * 0       20061103  etxlg   Copied from CPO R1A/4
 * 00      20140506  etxlg   Modified to run as root with -u
 *-----------------------------------------------------------------------------
 */

#ifndef EXEC_PROGRAM


/* This wrapper has been removed, we go back to calling erl direcly   *
 * The only function of the wrapper was to do ulimit -c unlimited     *
 * currently ulimits are set elsewhere (more limiting than unlimited) *
 * and trying to reset it causes an annoying error printout           *
 #  define EXEC_PROGRAM "/usr/sbin/AXD301_erl_wrap"                   */
#define EXEC_PROGRAM "erl"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pwd.h>
#include <errno.h>


#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif

struct sock_list {
    struct sock_list *next;
    int fd;
    int type;
    int protocol;
    struct sockaddr_in addr;
    char *arg;
};

int parse_addr(addr, str)
    struct sockaddr_in *addr;
    char *str;
{
    int port = 0;
    char *cp;
    struct hostent *hp;
    struct servent *se;

    if ((cp = strrchr(str, (int)':')) != NULL)
        *cp++ = '\0';
    if (cp) {
        if (!isdigit((int)cp[0])) {
            if ((se = getservbyname(cp, "tcp")) != NULL) {
                port = ntohs(se->s_port);
	    } else {
		fprintf(stderr, "unknown port %s\n", cp);
		return -1;
	    }
        } else {
            port = atoi(cp);
        }
    }
    if (port < 0 || port > 0xffff) {
	fprintf(stderr, "bad port number %d\n", port);
        return -1;
    }
    
    bzero(addr, sizeof(*addr));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);
    if (*str == '\000') {
	addr->sin_addr.s_addr = INADDR_ANY;
    } else {
	if ((addr->sin_addr.s_addr = inet_addr(str)) == INADDR_NONE) {
	    if ((hp = gethostbyname(str)) == NULL) {
		fprintf(stderr, "\"%s\" unknown host or address!\n", str);
		return -1;
	    } else {
		bcopy(hp->h_addr_list[0], &addr->sin_addr.s_addr,hp->h_length);
	    }
	}
    }
    return 0;
}

int parse_protocol(char *argstr)
{
  int protocol;
  struct protoent *pe;

  if (!isdigit((int)argstr[0])) {
      if ((pe = getprotobyname(argstr)) != NULL) {
          protocol = pe->p_proto;
      } else {
 	 fprintf(stderr, "Unknown protocol: %s\n", argstr);
         return -1;
      }
  } else {
      protocol = atoi(argstr);
  }

  if (protocol < 0 || protocol > 0xff) {
      fprintf(stderr, "bad protocol number %d\n", protocol);
      return -1;
  }

  return protocol;
}

struct sock_list *new_entry(type, argstr)
    int type;
    char *argstr;
{
    struct sock_list *sle;
    char *cp;
    
    sle = (struct sock_list *)malloc(sizeof(struct sock_list));
    if (!sle)
	return NULL;
    sle->next = NULL;
    sle->fd = -1;

    if ((cp = strchr(argstr, (int)',')) != NULL) {
	*cp++ = '\0';
	sle->arg = argstr;
	argstr = cp;
    } else {
	sle->arg = "-fd";
    }
    sle->type = type;
    switch (type) {
        case SOCK_RAW: {
	    int protocol;

	    if ((protocol = parse_protocol(argstr)) < 0) {
	        free(sle);
	        return NULL;
	    }
	    sle->protocol = protocol;
	    break;
	}
        case SOCK_STREAM:
        case SOCK_DGRAM:
	    sle->protocol = 0;
	    if (parse_addr(&sle->addr, argstr) < 0) {
		free(sle);
		return NULL;
	    }
	    break;
    }
    return sle;
}

int open_socket(sle)
    struct sock_list *sle;
{
    int		one = 1;
     
    sle->fd = socket(AF_INET, sle->type, sle->protocol);
    if (sle->fd < 0) {
	perror("socket");
	return -1;
    }
    if (sle->type != SOCK_RAW) {
#if 0
	printf("binding fd %d to %s:%d\n", sle->fd,
	       inet_ntoa(sle->addr.sin_addr), ntohs(sle->addr.sin_port));
#endif
	/* Set REUSE flag to enable Erlang restart */
	if (setsockopt(sle->fd, SOL_SOCKET, SO_REUSEADDR,
		       (char*)&one, sizeof(one)) == -1)
	{
	     perror("setsockopt");
	     close(sle->fd);
	     return -1;
	}
	
	if (bind(sle->fd, (struct sockaddr *)&sle->addr, sizeof(sle->addr))<0){
	    perror("bind");
	    close(sle->fd);
	    return -1;
	}
    }
    return sle->fd;
}

int main(argc, argv)
    int argc;
    char *argv[];
{
    struct sock_list *sl = NULL, *sltmp = NULL;
    int count = 0;
    int c;
    char *user = NULL;
    struct passwd *pwentry;


    while ((c = getopt(argc, argv, "s:d:r:u:")) != EOF)
        switch (c) {
	case 's':
	    sltmp = new_entry(SOCK_STREAM, optarg);
	    if (!sltmp) {
		exit(1);
	    }
	    sltmp->next = sl;
	    sl = sltmp;
	    count++;
	    break;
	case 'd':
	    sltmp = new_entry(SOCK_DGRAM, optarg);
	    if (!sltmp) {
		exit(1);
	    }
	    sltmp->next = sl;
	    sl = sltmp;
	    count++;
	    break;
	case 'r':
	    sltmp = new_entry(SOCK_RAW, optarg);
	    if (!sltmp) {
		exit(1);
	    }
	    sltmp->next = sl;
	    sl = sltmp;
	    count++;
	    break;
        case 'u':
            user = optarg;
            break;
	    count = count + 2;
	default:
	    exit(1);
	}
    argc -= optind;
    argv += optind;

    if(user == NULL){
        fprintf(stderr, "arg: -u <User> is required\n");
        exit(1);
    }
    errno = 0;
    pwentry = getpwnam(user);
    if(pwentry == NULL){
	if(errno == 0){
	    fprintf(stderr, "No such user: %s\n", user);
	    exit(1);
	}
        perror("getpwname");
        exit(1);
    }

    for(sltmp = sl; sltmp != NULL; sltmp = sltmp->next)
	if (open_socket(sltmp) < 0) {
	    fprintf(stderr, "failed to create socket!\n");
	    exit(1);
	}

    /* setuid(getuid()); */
    setuid(pwentry->pw_uid);
    
    {
	int i;
	char **newargv;
	char *run_prog = EXEC_PROGRAM;
	char *run_prog_name;

	newargv = (char **)malloc((1 + 2*count + argc + 1) * sizeof(char*));

	if ((run_prog_name = strrchr(run_prog, (int)'/')) == NULL)
	    run_prog_name = run_prog;
	else
	    run_prog_name++;

	i = 0;
	newargv[i++] = run_prog_name;

	for (; argc; argc--, argv++, i++)
	    newargv[i] = *argv;

	for(sltmp = sl; sltmp != NULL; ) {
	    char *fd_str = (char *)malloc(8);
	    if (!fd_str) exit(1);
	    sprintf(fd_str, "%d", sltmp->fd);
	    if (sltmp->arg && *(sltmp->arg))
		newargv[i++] = sltmp->arg;
	    newargv[i++] = fd_str;
	    sl = sltmp;
	    sltmp = sltmp->next;
	    free(sl);
	}
	newargv[i] = (char *)NULL;
	execvp(run_prog, newargv);
	perror("exec");
	exit(1);
    }
    exit(0);
}


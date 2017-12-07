/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB 2012-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 ****************************************************************************
 * Description:
 * This works as a middle-man between an arbitrary program that reads/writes
 * on stdin/stdout/stderr and an erlang port.
 * The program to run is given on the command line, it is forked with its
 * stdin and stdout/err connected to pipes back to the parent.
 * The parent program reads and writes the pipes, using poll (kernel) to ensure
 * it never blocks, forwarding only so much data to the port as is requested.
 * The requesting of additional data, as well as a few other commands are
 * handled out-of-band on an UDP socket connected back to erlang process
 * that owns the port.
 * 
 * This program solves two main problems:
 * It introduces flow control to the port, the external program can no longer
 * break the emulator by generating enormous amounts of data.
 * It can do "half-close", ensuring that the forked program read EOF and is
 * able to do an ordered flush of it's stdout before exiting.
 * 
 * Rev        Date         Name        What
 * -----      -------      --------    ------------------------
 * Rx         2014-10-11   etxlg       Copied from omc and adapted
 * R2A/2      2014-10-14   etxlg       Close stuff when exit due to port close
 * R3A/1      2015-03-23   etxlg       ASCII: 3 -> SIGINT
 * R3A/2      2015-04-10   etxlg       sighandler for SIGINT set to DEFAULT
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <signal.h>

#define BUFSIZE 4096
#define CHUNK 1024
#define INTCHAR 3

int child_done = 0;
int read_done = 0;
int debug=0;
int last=0;

struct {
    int afd;
    int bfd;
    int cfd;
    int dfd;
    int controlfd;
    int acount;
    int bcount;
    int requested;
    char abuf[BUFSIZE];
    char bbuf[BUFSIZE];
    char *awp;
    char *bwp;
} movit;

void
usage(char *name)
{
    fprintf(stderr, "%s\t[-d] -p <portnumber> -- prog <prog args>\n"
                "\t-d - print debug\n"
                "\t-p - udp port to connect for control\n",
                name);
}

void
do_debug(char *fmt, ...)
{
static FILE *file = NULL;
va_list ap;

    if(!debug) return;

    if(file == NULL)
	file = fopen("/tmp/port_proxy.dbg", "w+");
    if(file == NULL) {
	perror("fopen");
	return;
    }
    va_start(ap, fmt);
    vfprintf(file, fmt, ap);
    fflush(file);
    va_end(ap);
}


void
print_globals()
{
	do_debug( "%d: acount: %d\n", getpid(), movit.acount);
	do_debug( "%d: bcount: %d\n", getpid(), movit.bcount);
	do_debug( "%d: requested: %d\n", getpid(), movit.requested);
	do_debug( "%d: read_done: %d child_done: %d\n",
			getpid(), read_done, child_done);
	do_debug( "%d: last: %d\n", getpid(), last);
}

int
open_udp_port(int portnumber)
{
    struct sockaddr_in laddress;
    int fda;
    socklen_t addrlen;

    memset(&laddress, 0, sizeof laddress);
    laddress.sin_family = AF_INET;
    laddress.sin_port = 0;
    laddress.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    fda = socket(AF_INET, SOCK_DGRAM, 0);
    if(fda == -1){
        perror("socket");
        fprintf(stderr, "Failed to open udp control socket\n");
        return -1;
    }
    do_debug( "UDP control socket opened with fd: %d\n", fda);

    if(bind(fda, (struct sockaddr *)&laddress, sizeof laddress) == -1){
        perror("bind");
        fprintf(stderr, "Failed to bind udp control socket\n");
        close(fda);
        return -1;
    }
    do_debug( "UDP control socket bound to loopback\n");

    laddress.sin_port = htons(portnumber);
    if(connect(fda, (struct sockaddr *)&laddress, sizeof laddress) == -1){
        perror("connect");
        fprintf(stderr, "Failed to connect udp control socket\n");
        close(fda);
        return -1;
    }
    do_debug( "UDP control socket connected to port: %d\n",
			portnumber);
    addrlen = sizeof laddress;
    if(getsockname(fda, (struct sockaddr *)&laddress, &addrlen) == -1) {
	perror("getsockname");
        fprintf(stderr, "Failed to read local portnumber on control socket\n");
	close(fda);
	return -1;
    }
    do_debug( "UDP control socket local portnumber: %d\n",
			ntohs(laddress.sin_port));
    if(write(fda, &laddress.sin_port, sizeof laddress.sin_port) == -1){
	perror("write");
        fprintf(stderr, "Failed to write local portnumber on control socket\n");
	close(fda);
	return -1;
    }
    return fda;
}

void
adjust_poll(int epfd)
{
    struct epoll_event new_poll;

    /*afd, stdin, reading */
    new_poll.data.fd = movit.afd;
    if(movit.acount){
	do_debug( "%d: adjust: afd/stdin poll: 0\n", getpid());
	new_poll.events = 0;
	epoll_ctl(epfd, EPOLL_CTL_MOD, movit.afd, &new_poll);
    }else{
	do_debug( "%d: adjust: afd/stdin poll: EPOLLIN\n",
			  getpid());
	new_poll.events = EPOLLIN;
	epoll_ctl(epfd, EPOLL_CTL_MOD, movit.afd, &new_poll);
    }
    /*bfd, stdout, writing */
    new_poll.data.fd = movit.bfd;
    if(movit.bcount){
	do_debug( "%d: adjust: bfd/stdout poll: EPOLLOUT\n",
			  getpid());
	new_poll.events = EPOLLOUT;
	epoll_ctl(epfd, EPOLL_CTL_MOD, movit.bfd, &new_poll);
    }else{
	do_debug( "%d: adjust: bfd/stdout poll: 0\n",
			  getpid());
	new_poll.events = 0;
	epoll_ctl(epfd, EPOLL_CTL_MOD, movit.bfd, &new_poll);
    }
    /*cfd, output pipe, writing */
    if(!child_done){
	new_poll.data.fd = movit.cfd;
	if(movit.acount){
	    do_debug("%d: adjust: cfd/out pipe poll: ADD EPOLLOUT\n",
			      getpid());
	    new_poll.events = EPOLLOUT;
	    epoll_ctl(epfd, EPOLL_CTL_ADD, movit.cfd, &new_poll);
	}else{
	    do_debug("%d: adjust: cfd/out pipe poll: DEL\n",
			      getpid());
	    epoll_ctl(epfd, EPOLL_CTL_DEL, movit.cfd, NULL);
	}
    }
    /*dfd, input pipe, reading */
    if(!read_done){
	new_poll.data.fd = movit.dfd;
	if(movit.bcount || !movit.requested){
	    do_debug("%d: adjust: dfd/in pipe poll: DEL\n",
			      getpid());
	    epoll_ctl(epfd, EPOLL_CTL_DEL, movit.dfd, NULL);
	}else{
	    do_debug("%d: adjust: dfd/in pipe poll: ADD EPOLLIN\n",
			      getpid());
	    new_poll.events = EPOLLIN;
	    epoll_ctl(epfd, EPOLL_CTL_ADD, movit.dfd, &new_poll);
	}
    }
}

void
process_control_data(char data, int epfd)
{

    do_debug( "%d: Control character: %d\n", getpid(),
		      (int)data);
    switch(data){
    case 1:
	print_globals();
	/* hmmm
	movit.requested += CHUNK;
	*/
	movit.requested = (movit.requested + CHUNK) > BUFSIZE 
			  ? BUFSIZE
			  : movit.requested + CHUNK;
/*
	if(!movit.requested){
	    if(debug) fprintf(stderr, "%d: Adding chunk to requested\n",
			      getpid());
	    movit.requested = CHUNK;
	}else{
	    if(debug) fprintf(stderr, "%d: No more chunks - busy\n", getpid());
	}
*/
    break;
    case 2:
	do_debug( "%d: Closing ouput pipe\n", getpid());
	epoll_ctl(epfd, EPOLL_CTL_DEL, movit.cfd, NULL);
	close(movit.cfd);
	child_done++;
    break;
    case 9:
	print_globals();
    break;
    default:
	fprintf(stderr, "%d: Urecognized data on upd: %d\n",
		getpid(),(int)data);
    break;
	} /* end of switch */
}

int
process(int epfd, struct epoll_event *ev)
{
/* strategy to somewhat simplify the code here
 * if a buffer contains anything, we disable reading, i.e. once the buffer
 * holds data it will not be written to again until it becomes empty
 * new schema using pipes
 * afd(stdin) -> read -> abuf/acount -> write -> cfd(out_pipe)
 * dfd(in_pipe) -> read -> bbuf/bcount -> write -> bfd(stdout)
 */
    int ret;
    char data;
    char *interrupt_char;


     if(ev->data.fd == movit.afd){  /* stdin */
	do_debug( "%d: Polled: stdin\n", getpid());
        if(ev->events & (EPOLLHUP | EPOLLERR)){
	    do_debug( "%d: EPOLLHUP|ERR on afd - STDIN\n",
			      getpid());
	    return -2;
	}
	if(ev->events & EPOLLIN && movit.acount == 0){
	    ret = read(movit.afd, movit.abuf, BUFSIZE);
	    if(ret == 0){
 		do_debug( "%d: read 0 on afd - STDIN\n",
				  getpid()); 
		return -2;
	    }
	    if(ret < 0){
		perror("read from stdin");
		return -2;
	    }
	    movit.acount = ret;
	}
	return 1;
    }else if(ev->data.fd == movit.bfd){ /* stdout */
	do_debug( "%d: Polled: stdout\n", getpid());
        if(ev->events & (EPOLLHUP | EPOLLERR)){
	    do_debug( "%d: EPOLLHUP|ERR on bfd - STDOUT\n",
			      getpid());
	    return -2;
	}
	if(ev->events & EPOLLOUT && movit.bcount){
	    ret = write(movit.bfd, movit.bwp, movit.bcount);
	    if(ret < 0){
		perror("write to stdout");
		return -2;
	    }
	    movit.bcount -= ret;
	    movit.bwp += ret;
	    if(movit.bcount == 0) movit.bwp = &movit.bbuf[0];
	}
	return 1;
    }else if(ev->data.fd == movit.cfd){ /* output pipe for writing to child */
	do_debug( "%d: Polled: output pipe\n", getpid());
	if(ev->events & (EPOLLHUP | EPOLLERR)){
	    do_debug( "%d: EPOLLHUP|ERR on cfd - out pipe\n",
			      getpid()); 
	    child_done++;
	    last = 2;
	    epoll_ctl(epfd, EPOLL_CTL_DEL, movit.cfd, NULL);
	return 1;
    	}
	if(ev->events & EPOLLOUT && movit.acount){
	    /* add CTRL-C -> SIGINT */
	    interrupt_char = memchr(movit.awp, INTCHAR, movit.acount);
	    if(interrupt_char){
	 	do_debug( "found INTCHAR at position: %d\n",
				  interrupt_char - movit.awp);
		if(*movit.awp == INTCHAR){
		    movit.acount--;
		    movit.awp ++;
		    if(movit.acount == 0) movit.awp = &movit.abuf[0];
		    return -3;
		}
		/* after this the interrupt character may be first in buf */
	        ret = write(movit.cfd, movit.awp, interrupt_char - movit.awp);
	    }else{
	        ret = write(movit.cfd, movit.awp, movit.acount);
	    }
	    if(ret < 0){
		perror("write to child pipe");
		return -2;
	    }
	    movit.acount -= ret;
	    movit.awp += ret;
	    if(movit.acount == 0) movit.awp = &movit.abuf[0];
	}
	return 1;
    }else if(ev->data.fd == movit.dfd){ /* input pipe for reading from child */
	do_debug( "%d: Polled: input pipe\n", getpid());
	if(ev->events & EPOLLIN){
	    if(movit.bcount == 0 && movit.requested){
		ret = read(movit.dfd, movit.bbuf,
			movit.requested < BUFSIZE ? movit.requested : BUFSIZE);
		if(ret == 0) {
		    do_debug( "%d: read 0 on dfd\n", getpid()); 
		    read_done++;
		}else if(ret < 0){
		    perror("read from child pipe");
		    return -2;
		}
		movit.requested -= ret;
		movit.bcount = ret;
	    }
	    return 1;
	}else if(ev->events & (EPOLLHUP | EPOLLERR)){
	    do_debug( "%d: EPOLLHUP|ERR on dfd - in pipe "
				      "removing dfd from poll\n",
			      getpid()); 
	    epoll_ctl(epfd, EPOLL_CTL_DEL, movit.dfd, NULL);
	    read_done++;
	    return 1;
	}
    }else if(ev->data.fd == movit.controlfd){
	do_debug("%d: Polled: Data on control fd, reading it\n",
			getpid());
	ret = read(movit.controlfd, &data, 1);
	if(ret != 1) {
	    perror("read udp control socket");
	    exit(99);
	}
	process_control_data(data, epfd);
	return 1;
    }else{
	fprintf(stderr, "%d: port_proxy: Internal error\n", getpid());
	exit(9);
    }
return 1;  /* shouldn't reach this, every branch return separately */
}


int
main(int argc, char **argv)
{
    pid_t child;
    int epfd, nfds, ret, opt, count;
    int portnumber = 0;  /* this must be set, -p is required */
    struct epoll_event ev[5];
    int pipe_one[2], pipe_two[2];
    struct sigaction act;

    memset(&movit, 0, sizeof movit);
    memset(ev, 0, sizeof ev);
    memset(&act, 0, sizeof act);

    act.sa_handler = SIG_DFL;

    while((opt = getopt(argc, argv, "dp:")) != -1){
	switch(opt){
	case 'd':
	    debug++;
	    do_debug("Debug printing is on\n");
	break;
	case 'p':
	    portnumber = atoi(optarg);
	break;
	case '?':
	default:
	    fprintf(stderr, "Unrecognized option\n");
	    usage(argv[0]);
	    exit(1);
	break;
	}
   }
   do_debug( "UDP portnumber: %d\n", portnumber);
   do_debug( "argc: %d, optind: %d, arg at optind: %s\n",
			argc, optind, argv[optind]);


    if((argc - optind) < 1){
	usage(argv[0]);
        exit (1);
    }
    if(pipe(pipe_one) == -1 || pipe(pipe_two) == -1){
	perror("pipe");
	exit(2);
    }

    child = fork();

    switch(child){
        case -1:
            perror("fork");
            exit (9);
            break;
        case 0:  /* child on slave pty */
	    close(pipe_one[1]); /* close write end */
	    close(pipe_two[0]); /* close read end */
	    if(dup2(pipe_one[0], STDIN_FILENO) != STDIN_FILENO){
		perror("dup stdin");
		exit(3);
	    }
	    if(dup2(pipe_two[1], STDOUT_FILENO) != STDOUT_FILENO){
		perror("dup stdout");
		exit(4);
	    }
	    if(dup2(pipe_two[1], STDERR_FILENO) != STDERR_FILENO){
		perror("dup stderr");
		exit(5);
	    }
	    close(pipe_one[0]);
	    close(pipe_two[1]);

	    do_debug( "exec: %s\n", argv[optind]);
	    sigaction(SIGINT, &act, NULL);
	    execv(argv[optind], &argv[optind]);
            perror("execl");
            exit (12);
            break;
        default: /* parent */
	    close(pipe_one[0]); /* close read end */
	    close(pipe_two[1]); /* close write end */
	    if((epfd = epoll_create(5)) == -1) {
		perror("epoll_create");
		exit(13);
	    }

	    movit.afd =  STDIN_FILENO;
	    movit.bfd =  STDOUT_FILENO;
	    movit.cfd =  pipe_one[1];
	    movit.dfd =  pipe_two[0];
	    movit.controlfd = open_udp_port(portnumber);
	    movit.acount = 0;
	    movit.bcount = 0;
	    movit.awp = &movit.abuf[0];
	    movit.bwp = &movit.bbuf[0];

	    if(movit.controlfd == -1) exit(99);

	    ev[0].data.fd = movit.afd; /*stdin*/
	    ev[0].events = EPOLLIN;
	    ev[1].data.fd = movit.bfd; /*stdout*/
	    ev[1].events = 0;
	    ev[2].data.fd = movit.dfd; /*read pipe*/
	    ev[2].events = 0; /* no reading before we have requested */
	    ev[3].data.fd = movit.cfd; /*write pipe*/
	    ev[3].events = 0;
	    ev[4].data.fd = movit.controlfd;
	    ev[4].events = EPOLLIN;



	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, STDIN_FILENO, &ev[0]) != 0) {
		perror("epoll_ctl stdin");
		exit(14);
	    }
	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, STDOUT_FILENO, &ev[1]) != 0) {
		perror("epoll_ctl stdout");
		exit(15);
	    }
/*
 * These two are handled with CTL_ADD/DEL and it doesn't work to ADD an
 * fd with new events if it is _already_ there...
 * need to DEL them since othervise there would be a nasty bunch of
 * EPOLLHUP when the child exits before we finish reading alll the data on the
 * pipe...
	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, pipe_two[0], &ev[2]) != 0) {
		perror("epoll_ctl read end of pipe_two");
		exit(16);
	    }
	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, pipe_one[1], &ev[3]) != 0) {
		perror("epoll_ctl write end of pipe_one");
		exit(17);
	    }
*/
	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, movit.controlfd, &ev[4]) != 0) {
		perror("epoll_ctl control socket");
		exit(18);
	    }
 
            while(1){
		/* if((count = epoll_wait(epfd, ev, 5, 10000)) == -1) { */
		if((count = epoll_wait(epfd, ev, 5, -1)) == -1) {
		    if(errno == EINTR) continue;
		    perror("epoll_wait");
		    exit(19);
		}
		do_debug( "%d: epoll_wait: %d\n", getpid(), count);
/* epoll_wait() timeout 
		if(count == 0){
		    print_globals();
		    continue;
		}
*/
		for(nfds = 0; nfds < count; nfds++){
		    ret = process(epfd, &ev[nfds]);
		    switch(ret){
		    case -3: /* found the interrupt character */
			do_debug("sending SIGINT to: %d\n", child);
			kill(child, SIGINT);
		    case 1: /* normal case, check if done*/
			if (read_done && movit.bcount == 0){
			    do_debug(
					      "%d: -1 due to read_done and "
					      "empty bcount\n",
					      getpid()); 
			    do_debug( "Waiting for child\n");
			    close(movit.cfd);
			    close(movit.dfd);
			    wait(&ret);
			    if(WIFEXITED(ret)){
				do_debug(
						  "Child exit status: %d\n",
						  WEXITSTATUS(ret));
			        exit(WEXITSTATUS(ret));
			    }
			    exit(91);
			}
		    break;
		    case -2: /* the port exited, we are a loose end */
			/* unfortunately it seems that not all commands will
			 * will exit just for SIGHUP, try closing fds too */
			close(movit.afd);
			close(movit.bfd);
			close(movit.cfd);
			close(movit.dfd);
			kill(child, SIGHUP);
			wait(&ret);
			exit(0);
		    break;
		    default:
		    break;
		    } /* end of switch(ret) */
		}
		adjust_poll(epfd);
            } /* end of while(1) */
            break;
    } /* end of switch(child) */
return 0;
}

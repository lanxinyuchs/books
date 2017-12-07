/* ----------------------------------------------------------------------
 * %CCaseFile:	pipe_pty.c %
 * %CCaseRev:	/main/R2A/R3A/R5A/1 %
 * %CCaseDate:	2016-04-15 %
 * %CCaseDocNo: %
 * Author:      
 * Author: Lars Carlsson, lars.carlsson@ericsson.com
 *
 * Short description:
 * forkpty and exec the program given on the cmdline (arg1) in order to act
 * as middleman betwen a cmdline program expecting ptys and the erlang
 * port which read and write on a pipe.
 * This should possibly be scrapped soonish since I believe OTP:SSH is adding
 * support for pty-s natively.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
 * Revision history:
 *
 * Rev        Date         Name        What
 * -----      -------      --------    ------------------------
 * R2A/1      2014-01-17   etxlg       Copied from sys, fixed C90 warning
 * R2A/2      2014-03-31   etxlg       Fixed stupidity by adding more of same
 * R2A/3      2014-04-02   etxlg       Rewrite for unix98 pty
 * R2A/4      2014-04-09   etxlg       Support windowsize (only at start)
 * R2A/5      2014-06-12   etxlg       Try to solve issue with looping cliss
 * R3A/1      2015-01-28   etxlg       CTRL-C into Kill-line, no setsid() and
 *                                     ISIG must be on the pty
 * R5A/1      2016-04-15   etxarnu     CTRL-C fix also for sim and vRCS
*/

#define _SVID_SOURCE
#define _XOPEN_SOURCE 600

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <pty.h>
#include <sys/epoll.h>
#include <string.h>
#include <libgen.h>
#include <errno.h>
#include <termios.h>
#include <sys/wait.h>
#include <signal.h>

static pid_t child;

#define BUFSIZE 4096

int do_exit = 0;
int stat_a=0;
int stat_b=0;
int debug=0;

struct {
    int afd;
    int bfd;
    int cfd;
    int acount;
    int bcount;
    char abuf[BUFSIZE];
    char bbuf[BUFSIZE];
    char *awp;
    char *bwp;
} movit;

/*
void
donothing(int sig)
{
}
*/

void
usage(char *name)
{
    fprintf(stderr, "%s\t[-c <columns>] [-r <rows>] [-d] -- prog <prog args>\n"
                "\t-d - print debug\n"
                "\t-c - columns, set the number of columns on the pty\n"
                "\t-r - rows, set the number of rows on the pty\n",
                name);
}

int
process(int epfd, struct epoll_event *ev)
{
/* strategy to somewhat simplify the code here
 * if a buffer contains anything, we disable reading, i.e. once the buffer
 * holds data it will not be written to again until it becomes empty
   afd(stdin) -> read -> abuf/acount -> write -> cfd(master)
   cfd(master) -> read -> bbuf/bcount -> write -> bfd(stdout)
 */
    int ret;
    struct epoll_event new_poll;

    
    if(ev->data.fd == movit.afd){
        if(ev->events & EPOLLHUP){
	    if(debug) fprintf(stderr, "EPOLLHUP on afd - STDIN\n");
	    /* assume this is == broken connection exit everything by force
	    do_exit = 1;
	     */
	    return -2;
	}
	if(ev->events & EPOLLIN && movit.acount == 0){
	    ret = read(movit.afd, movit.abuf, BUFSIZE);
	    if(ret == 0){
 		if(debug) fprintf(stderr, "read 0 on afd -STDIN"); 
		do_exit = 1;}
	    if(ret < 0){
		perror("read from stdin");
		return -1;
	    }
	    stat_a += ret;
	    movit.acount = ret;
	}
    }else if(ev->data.fd == movit.bfd){
	if(ev->events & EPOLLOUT && movit.bcount){
	    ret = write(movit.bfd, movit.bwp, movit.bcount);
	    if(ret < 0){
		perror("write to stdout");
		return -1;
	    }
	    movit.bcount -= ret;
	    movit.bwp += ret;
	    if(movit.bcount == 0) movit.bwp = &movit.bbuf[0];
	}
    }else if(ev->data.fd == movit.cfd){
    if(ev->events & EPOLLHUP){
	if(debug) fprintf(stderr, "EPOLLHUP on cfd - master pty\n"); 
	return -1;
    }
	if(ev->events & EPOLLIN && movit.bcount == 0){
	    ret = read(movit.cfd, movit.bbuf, BUFSIZE);
	    if(ret == 0) {
		if(debug) fprintf(stderr, "read 0 on cfd\n"); 
		return -1;
	    }
	    if(ret < 0){
		perror("read from pty");
		return -1;
	    }
	    movit.bcount = ret;
	}
	if(ev->events & EPOLLOUT && movit.acount){
	    ret = write(movit.cfd, movit.awp, movit.acount);
	    if(ret < 0){
		perror("write to pty");
		return -1;
	    }
	    movit.acount -= ret;
	    movit.awp += ret;
	    stat_b += ret;
	    if(movit.acount == 0) movit.awp = &movit.abuf[0];
	}
    }else{
	fprintf(stderr, "pipe_pty: Internal error\n");
	exit(9);
    }

    new_poll.data.fd = movit.afd;
    new_poll.events = 0;
    if(movit.acount == 0 && ! do_exit) new_poll.events = EPOLLIN;
    epoll_ctl(epfd, EPOLL_CTL_MOD, movit.afd, &new_poll);

    new_poll.data.fd = movit.bfd;
    new_poll.events = 0;
    if(movit.bcount) new_poll.events = EPOLLOUT;
    epoll_ctl(epfd, EPOLL_CTL_MOD, movit.bfd, &new_poll);
 
    new_poll.data.fd = movit.cfd;
    new_poll.events = 0;
    if(movit.bcount == 0) new_poll.events |= EPOLLIN;
    if(movit.acount) new_poll.events |= EPOLLOUT;
    epoll_ctl(epfd, EPOLL_CTL_MOD, movit.cfd, &new_poll);

if (do_exit && movit.acount == 0 && movit.bcount ==0){
    if(debug) fprintf(stderr, "-1 due to do_exit and all counts\n"); 
    return -1;
}

return 1;
}


static void SIGINThandler(int sig)
{
   signal(SIGINT, SIGINThandler);
   if(debug) fprintf(stderr, "SIGINT, sending to: %d\n", (int)child); 
   kill(child, SIGINT);
}


int
main(int argc, char **argv)
{
    int master, slave, epfd, nfds, ret, opt, count;
    int rows = 24;
    int cols = 80;
    struct epoll_event ev[3];
    struct termios raw_term;
    struct winsize winsize;

    signal(SIGINT, SIGINThandler);

    memset(&raw_term, 0, sizeof raw_term);
    memset(&winsize, 0, sizeof winsize);


    while((opt = getopt(argc, argv, "dc:r:")) != -1){
	switch(opt){
	case 'd':
	    debug++;
	    fprintf(stderr, "Debug printing is on\n");
	break;
	case 'r':
	    rows = atoi(optarg);
	break;
	case 'c':
	    cols = atoi(optarg);
	break;
	case '?':
	default:
	    fprintf(stderr, "Unrecognized option\n");
	    usage(argv[0]);
	    exit(1);
	break;
	}
   }
   if(debug) fprintf(stderr, "Rows: %d Columns: %d\n", rows, cols);
   if(debug) fprintf(stderr, "argc: %d, optind: %d, arg at optind: %s\n",
			argc, optind, argv[optind]);


    if((argc - optind) < 1){
	usage(argv[0]);
        exit (1);
    }

    if((master = posix_openpt(O_RDWR)) < 0){
	perror("posix_openpt");
	exit(2);
    }
    if(grantpt(master) != 0){
	perror("grantpt");
	exit(3);
    }
    if(unlockpt(master) != 0){
	perror("unlockpt");
	exit(4);
    }
    if((slave = open(ptsname(master), O_RDWR)) < 0){
	perror("open pts slave");
	exit(5);
    }
    if(tcgetattr(slave, &raw_term) != 0){
	perror("tcgetattr");
	exit(6);
    }
/* this stuff is carefully determined through empirical testing, I have no idea
 * what is going on but unless this is right really strange things will happen
 * when larger amounts are piped/pasted into the terminal. It looks like
 * characters are lost...
 */
    raw_term.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
			| IXON);
/*
    raw_term.c_oflag &= ~OPOST;   <== diff against using "raw"
*/
/*
    raw_term.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    this was proven to work without strangeness - unfortunately cliss really
    wants to have signals translated so I will now try to add back ISIG
*/
    raw_term.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN);
    raw_term.c_cflag &=  ~(CSIZE | PARENB);
    raw_term.c_cflag |= CS8;
    if(tcsetattr(slave, TCSANOW, &raw_term) != 0){
	perror("tcsetattr");
	exit(7);
    }
    /* set the col/row values on the terminal, corresponding to default in CLISS
       or from cmdline to match the initial windowsize, size change is not
     * supported
     */
    winsize.ws_row = rows;
    winsize.ws_col = cols;
    if(ioctl(master, TIOCSWINSZ, &winsize) != 0){
	perror("ioctl: TIOCSWINSZ");
	exit(8);
    }

    child = fork();

    switch(child){
        case -1:
            perror("fork");
            exit (9);
            break;
        case 0:  /* child on slave pty */
	    close(master);
	    close(STDIN_FILENO);
	    close(STDOUT_FILENO);
	    close(STDERR_FILENO);
	    dup(slave); dup(slave);
	    if((ret = dup(slave)) != STDERR_FILENO){
		fprintf(stderr, "third dup returned: %d\n", ret);
		exit(10);
	    }
	    close(slave);
/*
This one caused a lot of grief...
couldn't get SIGINT -> KILL_LINE processing to work
	    if(setsid() == -1){
		perror("setsid");
		exit(11);
	    }
*/
	    if(debug) fprintf(stderr, "exec: %s\n", argv[optind]);
	    execv(argv[optind], &argv[optind]);
            perror("execl");
            exit (12);
            break;
        default:
	    close(slave);
	    if((epfd = epoll_create(3)) == -1) {
		perror("epoll_create");
		exit(13);
	    }
	    ev[0].data.fd = STDIN_FILENO;
	    ev[0].events = EPOLLIN;
	    ev[1].data.fd = STDOUT_FILENO;
	    ev[1].events = 0;
	    ev[2].data.fd = master;
	    ev[2].events = EPOLLIN;

	    movit.afd =  STDIN_FILENO;
	    movit.bfd =  STDOUT_FILENO;
	    movit.cfd =  master;
	    movit.acount = 0;
	    movit.bcount = 0;
	    movit.awp = &movit.abuf[0];
	    movit.bwp = &movit.bbuf[0];
	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, STDIN_FILENO, &ev[0]) != 0) {
		perror("epoll_ctl stdin");
		exit(14);
	    }
	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, STDOUT_FILENO, &ev[1]) != 0) {
		perror("epoll_ctl stdout");
		exit(15);
	    }
	    if(epoll_ctl(epfd, EPOLL_CTL_ADD, master, &ev[2]) != 0) {
		perror("epoll_ctl master");
		exit(16);
	    }
            while(1){
		if((count = epoll_wait(epfd, ev, 3, -1)) == -1) {
		    if(errno == EINTR) continue;
		    perror("epoll_wait");
		    exit(17);
		}
		for(nfds = 0; nfds < count; nfds++){
		    ret = process(epfd, &ev[nfds]);
		    switch(ret){
		    case -1:
			close(master);
			/* fprintf(stderr, "Waiting for child\n"); */
			wait(&ret);
			/* fprintf(stderr, "Child exit status: %d\n",
				WEXITSTATUS(ret)); */
			/*ensure cliss goes down*/
			/* kill(child, SIGTERM); */
/* fprintf(stderr, "stat_a: %u, stat_b: %u\n", stat_a, stat_b); */
			exit(0);
		    break;
		    case -2:
			kill(child, SIGHUP);
			wait(&ret);
			exit(0);
		    break;
		    default:
		    break;
		    }
		}
            } /* end of while(1) */
            break;
    } /* end of switch(child) */
return 0;
}

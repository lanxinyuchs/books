/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB 2011-2013 All rights reserved.
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
 * This is used when executing an erlang portprogram as part of a command pipe.
 * It tries to solve the problem of how to signal to the external program an
 * EOF. It works by reading the input stream looking for a character sequence that
 * when found indicates that EOF happened (comp. inband signalling).
 * Usage: std_of_proxy [-t <marker>] <program> [<args...>]
 * -t <marker> will set the string that indicates EOF to "marker", it is
 *  othervise set by default to "#inband-eof-marker#".
 *  program is the full path to the program to execute, args are passed along.
 *
 * Rev        Date         Name        What
 * -----      -------      --------    ------------------------
 * R2A/1      2013-07-05   etxlg       Handle child exit

 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h> /*memset*/
#include <signal.h>
#include <sys/wait.h>

volatile sig_atomic_t child_exited = 0;

void
child_exit(int sig)
{
    (void) sig; /* silence compiler warning */
    child_exited++;
}

int
main(int argc, char **argv)
{
    pid_t child;
    int pipefds[2];
    int prog = 1;

    char *marker = "#inband-eof-marker#";
    char *current = marker;
    int character;
    struct sigaction action;
    int status;

    if(argc < 2){
        fprintf(stderr, "Usage: %s [-t <termination string>] <program to fork> [args...]\n",
                argv[0]);
        return  -1;
    }
    if(argc >= 4){
        if(argv[1][0] == '-' && argv[1][1] == 't'){
            marker = argv[2];
            prog = 3;
        }
    }

    if(pipe(pipefds) != 0){
            perror("std_eof_proxy: pipe():");
            return -1;
    }
    memset(&action, 0, sizeof action);
    action.sa_handler = child_exit;
    if(sigaction(SIGCHLD, &action, NULL) == -1){
	perror("sigaction");
	return -1;
    }

    child = fork();
    switch(child){
        case -1:
            perror("std_eof_proxy: fork():");
            return -1;
            break;
        case 0: /* the child executes this */
            close(pipefds[1]); /*write end not needed here*/
            if(dup2(pipefds[0], STDIN_FILENO) == -1){;
                perror("std_eof_proxy: dup2(stdin):");
                return -1;
            }
            execv(argv[prog], &argv[prog]);
            perror("std_eof_proxy: execv()");
            return -1;
            break;
        default: /* parent */
            close(pipefds[0]); /*read end not needed here*/
            if(dup2(pipefds[1], STDOUT_FILENO) == -1){;
                perror("std_eof_proxy: dup2(stdout):");
                return -1;
            }
            while(1){
		if(child_exited){
		    wait(&status);
	    		//fprintf(stderr, "EOF proxy exit: %d waited at the top of the loop\n", status);
			if(WIFEXITED(status)) return WEXITSTATUS(status);
			return -1; /*hopfully not likely */
		    return status;
		}
                character = getc(stdin);
                if(character == EOF){
		    if(child_exited){
			wait(&status);
	    		//fprintf(stderr, "EOF proxy exit: %d waited due to stdin EOF\n", status);
			if(WIFEXITED(status)) return WEXITSTATUS(status);
			return -1; /*hopfully not likely */
		    }
		    /*do we need to make sure that the child exits?*/
		   /* kill(child, SIGTERM); */
                    break;
                }
                if(*current == (char)character){
                    current++;
                    if(*current == '\0'){
	    		//fprintf(stderr, "EOF proxy exit: 0 found eof-marker\n");
                        return 0;
                    }
                }else{
                    if(current != marker){
                        fwrite(marker, sizeof(char), current - marker, stdout);
                        current = marker;
                    }
                    putc(character, stdout);
                }
            } /* end of while(1), i.e. nothing more to read on stdin */
            if(current != marker){
                fwrite(marker, sizeof(char), current - marker, stdout);
            }
	    //fprintf(stderr, "EOF proxy exit: 0 after while-loop\n");
            return 0;
            break;
    }
}

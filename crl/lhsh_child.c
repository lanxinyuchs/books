/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <pty.h>
#include <sys/prctl.h>
#include <syscall.h>
#ifndef PR_SET_NAME
#define PR_SET_NAME 15
#endif
#include <sys/wait.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <termios.h>

#include <itc.h>
#include "lhsh_shelld.sig"
#include "lhsh_child.h"
#include "log.h"

#ifndef SHELL_BASENAME
#define SHELL_BASENAME "sh"
#endif

union itc_msg {
        uint32_t sigNo;

        struct FmWriteRequest FmWriteRequest;
        struct FmWriteReply   FmWriteReply;
        struct FmReadRequest  FmReadRequest;
        struct FmReadReply    FmReadReply;
        struct FmCancel       FmCancelRequest;
        struct FmCancel       FmCancelReply;
        struct FmClose        FmCloseRequest;
        struct FmClose        FmCloseReply;
        struct ChildDone      t_child_done;
};

struct io_data {
        itc_mbox_id_t mbox;
        char name[17]; /* PR_SET_NAME is limited to 16 bytes + '\0' */

        struct {
                itc_monitor_id_t parent;
                itc_monitor_id_t in;
                itc_monitor_id_t out;
                itc_monitor_id_t err;
        } attRef;
        struct child_data *cd;
        int pty;
};
static char *static_board_name[] = {
	"bp",
	"trxm0",
	"trxm1",
	"trxm2",
	"trxm3"
};
static unsigned num_of_boards = 5;
static const char control_c = 0x03;

/******************************************************************************/

static void __attribute__((unused)) log_buffer(const char *msg, char *buf,
                                               unsigned size)
{
        log_trace2("Buffer[%u] \"%s\": %s", size, msg, buf);
#if 0
        unsigned i;

        for (i = 0; i < size; i++) {
                if (i % 8 == 0) {
                        printf("\n  ");
                }
                printf(" 0x%02x", buf[i]);
        }
        printf("\n-\n");
#endif
}

/******************************************************************************/

static void
add_path_prefix(const char *prefix)
{
        if (prefix != NULL) {
                const char *envName = "PATH";
                const char *oldPath = getenv(envName);
                if (!oldPath)
                        return;

                /* Add 2 bytes for ':' and termination. */
                char *path = malloc(strlen(prefix) + strlen(oldPath) + 2);

                sprintf(path, "%s:%s", prefix, oldPath);
                /* coverity[tainted_string] there seems to be no escape from this! */
                setenv(envName, path, 1);
                free(path);
        }
}

/******************************************************************************/

static void remote_close(itc_mbox_id_t mbox, const struct ose_file *f)
{
        union itc_msg *sig;
        uint32_t rx_filter[] = {1, FM_CLOSE_REPLY};
        uint32_t status;

        sig = itc_alloc(sizeof(struct FmClose), FM_CLOSE_REQUEST);
        sig->FmCloseRequest.handle = htonl(f->handle);
        sig->FmCloseRequest.cancel = htonl(1);

        log_trace2("sent FM_CLOSE_REQUEST");
        itc_send(&sig, f->spid, mbox);

        sig = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
        log_trace2("got FM_CLOSE_REPLY");

        status = ntohl(sig->FmCloseReply.status);
        if (status != 0) {
                log_trace2("FM_CLOSE(%u) failed. status = %u", f->handle, status);
        }

        itc_free(&sig);
}

/******************************************************************************/

static void send_fm_read_req(struct io_data *io)
{
        union itc_msg *sig = itc_alloc(sizeof(struct FmReadRequest),
                                                       FM_READ_REQUEST);
        sig->FmReadRequest.handle    = htonl(io->cd->in.handle);
        sig->FmReadRequest.position  = 0;
        sig->FmReadRequest.block     = htonl(1);
        sig->FmReadRequest.requested = htonl(16);

        log_trace2("sent FM_READ_REQUEST");
        itc_send(&sig, io->cd->in.spid, ITC_MY_MBOX);
}

/******************************************************************************/

static int
handle_fdw(struct io_data *io, int pty, const struct ose_file *f)
{
        union itc_msg *sig = itc_receive(NULL, ITC_NO_TMO, ITC_FROM_ALL);
        itc_mbox_id_t sender = ITC_NO_ID;
        int status;
        int result = 0;

        switch (sig->sigNo) {
        case ITC_MONITOR_DEFAULT_NO:
                sender = itc_sender(sig);
                log_trace2("got ITC_MONITOR_DEFAULT_NO from 0x%08x", sender);
                if (sender == io->cd->in.spid) {
                        io->attRef.in = ITC_NO_ID;
                } else if (sender == io->cd->out.spid) {
                        io->attRef.out = ITC_NO_ID;
                } else if (sender == io->cd->err.spid) {
                        io->attRef.err = ITC_NO_ID;
                } else if (sender == io->cd->parent) {
                        io->attRef.parent = ITC_NO_ID;
                } else {
                        log_err("Received attach signal from unrecognized process 0x%08x",
                               sender);
                }
                result = 2;
                break;

        case FM_READ_REPLY:
                log_trace2("got FM_READ_REPLY from 0x%08x", itc_sender(sig));
                status = ntohl(sig->FmReadReply.status);
                if (status != 0) {
                        log_trace2("Received FM_READ_REPLY with status %u", status);
                        result = 1;
                        break;
                }
                write(pty, sig->FmReadReply.buffer, ntohl(sig->FmReadReply.actual));
                send_fm_read_req(io);
                break;

        case SHELL_KILL_CHILD:
                log_trace2("Sending SIGINT");
                write(pty, &control_c, sizeof(control_c));
                break;

        default:
                log_err("Unexpected signal (0x%08x) received from 0x%08x", sig->sigNo,
                        itc_sender(sig));
                result = 1;
                break;
        }

        itc_free(&sig);
        return result;
}

/******************************************************************************/

static int
handle_fdr(struct io_data *io, fd_set *fdSet, int *fd,
           const struct ose_file *f)
{
        union itc_msg *sig;
        int rCnt;
        uint32_t rx_filter[] = {2, ITC_MONITOR_DEFAULT_NO, FM_WRITE_REPLY};
        itc_mbox_id_t sender = ITC_NO_ID;
        int result = 0;

        /* Initially set buffer size as dictated by the client side
           and later resize it if needed */
        sig = itc_alloc(sizeof(struct FmWriteRequest) + f->size + 1,
                        FM_WRITE_REQUEST);

        rCnt = read(*fd, &sig->FmWriteRequest.buffer, f->size);

        if (rCnt <= 0) {
                log_trace2("read(%d) returned %d. (%s)",
                       *fd, rCnt, strerror(errno));

                /* This happens when the child terminal has exited
                   Setting fd to -1 tells the loop around handle_fdr
                   to exit */
                *fd = -1;

                itc_free(&sig);
                return 0;
        }

        sig->FmWriteRequest.buffer[rCnt] = 0;

        log_trace2("Read from command output \'%s\' (%d)",
                  sig->FmWriteRequest.buffer, rCnt);

        if (f->append)
                sig->FmWriteRequest.position  = 0xffffffff;
        else
                sig->FmWriteRequest.position  = -1;
        sig->FmWriteRequest.handle    = htonl(f->handle);
        sig->FmWriteRequest.requested = htonl(rCnt);

        if (rCnt < f->size) {
                if (!itc_setsize(sig, sizeof(struct FmWriteRequest) + rCnt + 1)) {
                        log_trace2("resized payload of FM_WRITE_REQUEST from %d to a new size %d failed",
                                    f->size, rCnt);
                }
                else {
                        log_trace2("resized payload of FM_WRITE_REQUEST from %d to a new size %d",
                                    f->size, rCnt);
                }
        }

        itc_send(&sig, f->spid, ITC_MY_MBOX);
        log_trace2("sent FM_WRITE_REQUEST");

        /* BUG WARNING!
         * This code does not properly handle the case that 'actual'
         * written bytes does not match 'requested'. In that case a new
         * signal should probably be sent with the missing bytes until
         * 'requested' equals 'actual'! */
        sig = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
        switch (sig->sigNo) {
        case ITC_MONITOR_DEFAULT_NO:
                sender = itc_sender(sig);
                log_trace2("got ITC_MONITOR_DEFAULT_NO from 0x%08x", sender);
                if (sender == io->cd->in.spid) {
                        io->attRef.in = ITC_NO_ID;
                } else if (sender == io->cd->out.spid) {
                        io->attRef.out = ITC_NO_ID;
                } else if (sender == io->cd->err.spid) {
                        io->attRef.err = ITC_NO_ID;
                } else if (sender == io->cd->parent) {
                        io->attRef.parent = ITC_NO_ID;
                } else {
                        log_err("Received attach signal from unrecognized process 0x%08x",
                               sender);
                }
                result = 2;
                break;

        case FM_WRITE_REPLY:
                log_trace2("got FM_WRITE_REPLY from 0x%08x", itc_sender(sig));
                if (ntohl(sig->FmWriteReply.actual) < 0) {
                        log_err("fmWrite() failed. Got %d bytes, expected %d",
                                        ntohl(sig->FmWriteReply.actual), rCnt);
                        result = 1;
                        break;
                }
                break;

        default:
                log_err("Unexpected signal (0x%08x) received from 0x%08x",
                        sig->sigNo, itc_sender(sig));
                result = 1;
                break;
        }

        itc_free(&sig);
        return result;
}

/******************************************************************************/

static void *child_cont(struct io_data *io)
{
        int pty = io->pty;
        int fd_itc;
        sigset_t sigmask;
        union itc_msg *reply;

        snprintf(io->name, sizeof(io->name), "io-%ld", syscall(__NR_gettid));
        prctl(PR_SET_NAME, (unsigned long)io->name, 0UL, 0UL, 0UL);

        /* Create our temporary mailbox. */
        io->mbox = itc_create_mailbox(io->name, 0);
        if (io->mbox == ITC_NO_ID)
                return NULL;

        io->attRef.parent = itc_monitor(io->cd->parent, NULL);
        io->attRef.in = itc_monitor(io->cd->in.spid, NULL);
        io->attRef.out = itc_monitor(io->cd->out.spid, NULL);
        io->attRef.err = itc_monitor(io->cd->err.spid, NULL);

        fd_itc = itc_get_fd();

        if (sigprocmask(0, NULL, &sigmask) != 0) {
                perror("sigprocmask");
                _exit(-1);
        }

        send_fm_read_req(io);

        io->cd->child = io->mbox;

        /********** Main loop **********/

        while (pty >= 0) {
                fd_set fdsR;
                int maxFd = fd_itc;

                /***** Preparation *****/

                FD_ZERO(&fdsR);
                FD_SET(fd_itc, &fdsR);

                if (pty >= 0) {
                        FD_SET(pty, &fdsR);
                        if (pty > maxFd) {
                                maxFd = pty;
                        }
                }

                /***** select *****/
                if (pselect(maxFd + 1, &fdsR, NULL, NULL, NULL, &sigmask) < 0) {
                        if (errno == EINTR)
                                continue;
                        perror("pselect");
                        abort();
                }

                /***** Evaluate *****/
                if (FD_ISSET(fd_itc, &fdsR)) {
                        int ret;
                        /* stdin */
                        ret = handle_fdw(io, pty, &io->cd->in);
                        if (ret) {
                                if (ret > 1)
                                        /* remote process died; no point trying to connect to it! */
                                        goto out;
                                break;
                        }
                }

                if (pty >= 0 && FD_ISSET(pty, &fdsR)) {
                        int ret;

                        /* stdout */
                        ret = handle_fdr(io, &fdsR, &pty, &io->cd->out);
                        if (ret) {
                                if (ret > 1)
                                        /* remote process died; no point trying to connect to it! */
                                        goto out;
                                break;
                        }
                }
        }

        /* Cleanup */
        remote_close(io->mbox, &io->cd->in);
        remote_close(io->mbox, &io->cd->out);
        remote_close(io->mbox, &io->cd->err);

out:
        if (io->attRef.parent != ITC_NO_ID)
                itc_unmonitor(io->attRef.parent);
        if (io->attRef.in != ITC_NO_ID)
                itc_unmonitor(io->attRef.in);
        if (io->attRef.out != ITC_NO_ID)
                itc_unmonitor(io->attRef.out);
        if (io->attRef.err != ITC_NO_ID)
                itc_unmonitor(io->attRef.err);

        /* Send CHILD_DONE */
        reply = itc_alloc(sizeof(struct ChildDone), CHILD_DONE);
        reply->t_child_done.status = 0;
        reply->t_child_done.error = 0;
        itc_send(&reply, io->cd->parent, ITC_MY_MBOX);
        log_trace2("sent CHILD_DONE to %x", io->cd->parent);

        /* Delete "child mailbox" created by shelld
        * This to send ATTATCH message to other side */
        itc_delete_mailbox(io->cd->mbox);
        /* Delete normal child mailbox */
        itc_delete_mailbox(io->mbox);

        return NULL;
}

/******************************************************************************/

static void __create_child(struct child_data *cd)
{
        pid_t pid;
        int pty;
        struct io_data io;
        int status;
	char tempstr[128] = "***********************************************************\
					\n*************************************************************\n";
        /* Whoo, this cannot really be possible! */
        if (cd->cmd_str == NULL)
                return;

        log_info("Child created");
        log_trace2("%s():", __func__);
        log_trace2("   path_prefix : '%s'",      cd->path_prefix);
        log_trace2("   parent      : 0x%08x",    cd->parent);
        log_trace2("   cmd_str     : '%s'",      cd->cmd_str);
        log_trace2("   in spid     : 0x%08x:%d", cd->in.spid,  cd->in.handle);
        log_trace2("   out spid    : 0x%08x:%d", cd->out.spid, cd->out.handle);
        log_trace2("   err spid    : 0x%08x:%d", cd->err.spid, cd->err.handle);

        pid = forkpty(&pty, NULL, NULL, NULL);
        if (0 == pid) {
                const char *cmd_str = cd->cmd_str;
                char *cmd_str2 = calloc(1, strlen(cd->cmd_str) +
                                        strlen(SHELL_BASENAME " -c ''") + 16);

                strcat(cmd_str2, SHELL_BASENAME);
                if (!strncmp(cmd_str, "shell ", 6) && strlen(cmd_str) > 6)
                        cmd_str += 6;
                if (strcmp(cmd_str, "shell")) {
                        strcat(cmd_str2, " -c '");
                        strcat(cmd_str2, cmd_str);
                        strcat(cmd_str2, "'");
                }

                /* Use default action for SIGINT to enable Control-C */
                signal(SIGINT, SIG_DFL);

                add_path_prefix(cd->path_prefix);

                /* Set the prompt */
                if (cd->prompt)
                        setenv("PS1", cd->prompt, 1);
                else
                        setenv("PS1", "$ ", 1);

                /* Set some useful entries in the environment */
                setenv("LHSH_TTY", ttyname(0), 1);

                /* Disable local echo on tty */
                system("stty -echo");

                /* We need to disable post processing in order to stream binary
                   data over this tty */
                system("stty -opost");

                /* Generate signal for Control-C */
                system("stty isig");

                /* Child environment setup completed. Now execute the request. */
                log_trace2("Command %s started", cmd_str2);


		if(cd->num_of_trxm != 0 && cd->linkhandler_name != NULL){
			num_of_boards = cd->num_of_trxm + 1; //1 means bp board
			char linkhandler[128];
			char lhsh_trxm_cmd[128] = "";
			char cmd_str4[128] ="";
			uint32_t i =0 ,j = 0,remain_len;
			uint32_t cmdtype = 0;
			char * sep_cmd[32];
			char cmd_str3[128] = "";
			strcat(cmd_str3, cmd_str);

			sep_cmd[j] = strtok(cmd_str3," ");
			while(sep_cmd[j]) {
				log_trace2("sep_cmd %d value is %s",j, sep_cmd[j]);
				j = j +1;
				sep_cmd[j] = strtok(NULL," ");
			}
			if(j > 1) {
				remain_len = sizeof(cmd_str4);

				for(i = 0;i < j-1;i++) {
					strncat(cmd_str4,sep_cmd[i],remain_len);
					remain_len -= strlen(sep_cmd[i]);
					strncat(cmd_str4, " ", remain_len);
         				remain_len -= strlen(" ");
					log_trace2("cmd_str4 value is %s,sep_cmd %d is %s", cmd_str4, i,sep_cmd[i]);
				}

				for(i = 0;i < num_of_boards;i++) {
					if(strcmp(sep_cmd[j -1] ,static_board_name[i] ) == 0) {
						cmdtype =  1;
						//BP
						if (i == 0) {
							memset(cmd_str3, 0, 128);
                					strcat(cmd_str3, SHELL_BASENAME);
                        				strcat(cmd_str3, " -c '");
                        				strcat(cmd_str3, cmd_str4);
                        				strcat(cmd_str3, "'");

							system(cmd_str3);
						} else
						{
							//this part need to confirm with the detail name of trxm link
							snprintf(lhsh_trxm_cmd, 128, "/usr/bin/lhsh  %s  %s",sep_cmd[j -1], cmd_str4);
							log_info("lhsh_trxm_cmd is %s", lhsh_trxm_cmd);;
							system(lhsh_trxm_cmd);
						}
						break;
					}
				}
			}
			else {
				cmdtype = 0;
			}

			if(cmdtype == 0) {
				system(cmd_str2);
				write(pty,tempstr,128);
				for( i =0 ;i < num_of_boards - 1;i++) {
					memset(linkhandler, 0, 128);
					//this part need to confirm with the detail name of trxm link
					snprintf(linkhandler, 128, "%s%d", cd->linkhandler_name, i);
					snprintf(lhsh_trxm_cmd, 128, "/usr/bin/lhsh  %s  %s", linkhandler, cmd_str);
					log_info("lhsh_trxm_cmd is %s",lhsh_trxm_cmd);
					write(1,tempstr,128);
					write(1,linkhandler,128);
					write(1,"\n",1);
					system(lhsh_trxm_cmd);
					write(1,tempstr,128);
				}
			}

		} else
		       system(cmd_str2);

                log_trace2("Command %s done", cmd_str2);
                log_trace2("_exit");
                _exit(0);
                /* End of child */
        } else if (pid < 0) {
                /* forkpty() failed */
                perror("forkpty");
                return;
        }

        /* Parent */
        io.pty = pty;
        io.cd = cd;

        /* Continue in main loop */
        child_cont(&io);

        if (pty >= 0)
                if (close(pty)) {
                        if (errno == EBADF)
                        abort();
                }
        log_trace2("IO completed");

        waitpid(pid, &status, 0);
        if (status != 0)
            log_trace2("child completed with nonzero exit status. (%d)", status);
}

/******************************************************************************/

void
create_child(struct child_data *cd)
{
        __create_child(cd);
}

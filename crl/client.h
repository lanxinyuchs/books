/* ---------------------------------------------------------------------------
 *
 * @copyright Ericsson AB 2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef CLIENT_H
#define CLIENT_H

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <itc.h>
#include <stdarg.h>
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif
#include "lhsh_shelld.sig"
#include "test_sigs/lhsh_shelld_test.sig"
#include "lhsh_child.h"

union itc_msg {
        uint32_t sigNo;

        struct ShellInterfaceRequest  t_shell_interface_req;
        struct ShellInterfaceReply    t_shell_interface_reply;
        struct ShellCreateChild       t_shell_creat_child_req;
        struct ShellCreateChild       t_shell_creat_child_reply;
        struct ShellSetenvChildSender t_shell_setenv_child_sender;
        struct ShellSetenvChild       t_shell_setenv_child;
        struct ShellStartChild        t_shell_start_child;
        struct ShellKillChild         t_shell_kill_child;
        struct ChildDone              t_child_done;
        struct FmCancel               t_fm_cancel_req;
        struct FmCancel               t_fm_cancel_reply;

        struct FmWriteRequest         fm_write_request;
        struct FmWriteReply           fm_write_reply;
        struct FmCancel               fm_cancel_request;
        struct FmReadRequest          fm_read_request;
        struct FmReadReply            fm_read_reply;
        struct FmClose                fm_close_request;

        /*Test oriented structures!*/
        struct SimpleRemoteCmd         simple_remote_command;
        struct SimpleRemoteCmdReply    simple_remote_command_reply;
        struct SimpleRemoteCmdDone     simple_remote_command_child_done;

};

struct lhsh_rcmd {
        itc_mbox_id_t shell_pid;

        uint32_t child_pid;
        uint32_t child_ref;
        itc_mbox_id_t owner_pid;

        char *argv;
};

typedef enum {
        Client_ITC = 0,
        Client_RUMA,
        Client_RCMD,
        CLIENT_NUM
} ClientMode;


static inline int str_add(char *buf, char *str, int *off)
{
        int ret = *off;
        *off += sprintf(buf + *off, "%s", str) + 1;
        return ret;
}

void handle_shell_interface_response(union itc_msg *sig);

void handle_shell_create_child_response(itc_mbox_id_t replyTo,
                                        union itc_msg *sig,
                                        struct lhsh_rcmd *rcmd);

int send_interface_request(itc_mbox_id_t spid, itc_mbox_id_t myBox);

int send_create_child_request(itc_mbox_id_t myBox, char *c_argv,
                              struct lhsh_rcmd *rcmd);

int send_start_child_request(itc_mbox_id_t spid, itc_mbox_id_t myBox,
                             struct lhsh_rcmd *rcmd);

void send_setenv_child(struct lhsh_rcmd *rcmd, itc_mbox_id_t spid,
                       itc_mbox_id_t myBox, int local, char *env, ...);

int rcmd_write_req(itc_mbox_id_t spid, union itc_msg *sig, char *buffer,
                   int *bytes);

int rcmd_close_req(itc_mbox_id_t spid, union itc_msg *sig);

int send_rcmd(char *c_argv, itc_mbox_id_t myBox, itc_mbox_id_t spid,
              itc_mbox_id_t tpid, int bufsize, int bufmode, int append);


#endif /* CLIENT_H */

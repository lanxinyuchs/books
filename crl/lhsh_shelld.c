/* ---------------------------------------------------------------------------
 *
 * й Ericsson AB 2014 All rights reserved.
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
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <itc.h>
#include <itc_system.h>
#include "lhsh_shelld.sig"
#include "lhsh_child.h"
#include "log.h"

#define DEFAULT_LHSH_PATH ""

/******************************************************************************/

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
        struct FmCancel               t_fm_cancel_req;
        struct FmCancel               t_fm_cancel_reply;

        struct FmWriteRequest         FmWriteRequest;
        struct FmCancel               FmCancelRequest;
        struct FmReadRequest          FmReadRequest;
        struct FmClose                FmCloseRequest;
};

/******************************************************************************/

struct child_descriptor {
        struct child_descriptor *next;
        pid_t pid;
        struct child_data data;
};

static struct child_descriptor *child_list = NULL;
static const char *path_prefix = DEFAULT_LHSH_PATH;
static itc_mbox_id_t server_mbox = ITC_NO_ID;
static pthread_mutex_t list_mutex = PTHREAD_MUTEX_INITIALIZER;

static uint32_t num_of_trxm = 0;
static const char *linkhandler_name = "trxm";

/******************************************************************************/

void help_print(void)
{
        printf("Place the te binary file into /mnt/flash/lhsh.\n");
        printf("Start  the lhsh_shelld application with > бн/lhsh_shelld -p "
               "/mnt/flash/lhsh.\n");
        printf("'/mnt/flash/lhsh' is a path prefix. It will be added to the "
               "beginning of the PATH \n");
        printf("variable, which is used to locate the te binary.\n");
        printf("On the OBIF side, after the link is established, type 'lhsh "
               "port_<x>_dev_<x> te'\n");
        printf("to run the 'te' command. You can also type linux command instead "
               "of  'te'\n");
        printf(".e.g. lhsh port_1_dev_1 ls\n");
}

/******************************************************************************/

static void print_create_child_request(struct ShellCreateChild *req)
{
        log_trace2("CHILD_CREATE_REQUEST:");
        log_trace2("  status is    : %ld", (long)req->status);
        log_trace2("  child_pid is : %ld", (long)req->child_pid);
        log_trace2("  user_id is   : %ld", (long)req->user_id);
        log_trace2("  user_name is : %ld", (long)req->user_name);
        log_trace2("  proc_name is : %ld", (long)req->proc_name);
        log_trace2("  proc_type is : %ld", (long)req->proc_type);
        log_trace2("  priority is  : %ld", (long)req->priority);
        log_trace2("  argv is      : %ld", (long)req->argv);
        log_trace2("  wdir is      : %ld", (long)req->wdir);
        log_trace2("  s is         : %s",  req->s);
}


/******************************************************************************/

static void handle_shell_interface_request(itc_mbox_id_t replyTo)
{
        union itc_msg *reply;

        reply = itc_alloc(sizeof(struct ShellInterfaceReply),
                          SHELL_INTERFACE_REPLY);

        reply->t_shell_interface_reply.status     = 0;
        reply->t_shell_interface_reply.biosHandle = 0;
        sprintf(reply->t_shell_interface_reply.whatStr, "comba shelld");
        reply->t_shell_interface_reply.sigs[0] = 0;

        itc_send(&reply, replyTo, ITC_MY_MBOX);
        log_trace2("sent SHELL_INTERFACE_REPLY to 0x%08x",
               replyTo);
}

/******************************************************************************/

static void
handle_shell_create_child_request(itc_mbox_id_t replyTo,
                                  struct ShellCreateChild *req)
{
        struct child_descriptor *child;
        union itc_msg *reply;

        print_create_child_request(req);

        child = calloc(1, sizeof(struct child_descriptor));
        child->pid = 0;
        child->data.cmd_str = strdup(&req->s[ntohl(req->argv)]);
        child->data.path_prefix = path_prefix;
        child->data.child = ITC_NO_ID;
	child -> data.num_of_trxm = num_of_trxm;
	child -> data.linkhandler_name = linkhandler_name;
	log_trace2("num of trxm is %d, child linkhandler name is %s, and linkhanler is %s",
		child -> data.num_of_trxm,child -> data.linkhandler_name,linkhandler_name);
        pthread_mutex_lock(&list_mutex);
        child->next = child_list;
        child_list = child;
        pthread_mutex_unlock(&list_mutex);

        /*
         * This can get set in a weird way if the client sends a PA= env var,
         * but what the hell is that???
         */
        child->data.parent = replyTo;

        /* Create a cloned "child mailbox"
         * Other side may use this for ATTATCH signals */
        child->data.mbox = itc_clone_mailbox(ITC_MY_MBOX, child->data.cmd_str);

        /* Send reply */
        reply = itc_alloc(sizeof(struct ShellCreateChild),
                          SHELL_CREATE_CHILD_REPLY);

        memset(&(reply->t_shell_creat_child_req.status), 0,
               (sizeof(struct ShellCreateChild) - sizeof(reply->sigNo)));

        reply->t_shell_creat_child_req.att_ref   = 0;
        /* Wooh, this is a real hack warning! The child_pid is really a uint32_t
         * but sent as a pointer! This will break on 64-bit platforms! */
        reply->t_shell_creat_child_req.child_pid = htonl((uint32_t)child);
        /* Send back reply from "child mailbox" */
        itc_send(&reply, replyTo, child->data.mbox);
        log_trace2("sent SHELL_CREATE_CHILD_REPLY to %x",
                replyTo);
}

/******************************************************************************/

static struct child_descriptor *find_child_descriptor(uint32_t pid)
{
        struct child_descriptor *tmp;

        pthread_mutex_lock(&list_mutex);
        tmp = child_list;
        while (tmp) {
                if (tmp == (struct child_descriptor *)pid) {
                        pthread_mutex_unlock(&list_mutex);
                        return tmp;
                }
                tmp = tmp->next;
        }
        pthread_mutex_unlock(&list_mutex);
        return NULL;
}

/******************************************************************************/

static struct child_descriptor *
unlink_child_descriptor(struct child_descriptor *cd)
{
        struct child_descriptor **tmp;

        pthread_mutex_lock(&list_mutex);
        tmp = &child_list;

        while (*tmp) {
                if (*tmp == cd) {
                        (*tmp) = cd->next;
                        pthread_mutex_unlock(&list_mutex);
                        return cd;
                }
                tmp = &(*tmp)->next;
        }
        pthread_mutex_unlock(&list_mutex);
        log_trace2("unlink_child_descriptor(0x%08x) failed. Descriptor not found.",
               (unsigned)cd);
        return NULL;
}

/******************************************************************************/

static void
handle_shell_setenv_child_sender(itc_mbox_id_t sender,
                                 struct ShellSetenvChildSender *req)
{
        char name[3] = {0};
        struct child_descriptor *child;

        log_trace2("got SHELL_SETENV_CHILD_SENDER from 0x%08x", sender);

        child = find_child_descriptor(ntohl(req->child_pid));
        if (child == NULL) {
                log_err("Invalid child 0x%08x", ntohl(req->child_pid));
                return;
        }
        sprintf(name, "%.2s", req->assignment);

        if (strcmp(name, "FD") == 0) {
                int remote_fd = -1;
                uint32_t handle = 0;
                uint32_t bufMode = 0;
                uint32_t bufSize = 0;
                uint32_t append = 0;
                char o[10];

                sscanf(req->assignment, "FD%d=%[^','],0x%x,%u,%u,0x%x",
                       &remote_fd, o, &handle, &bufMode, &bufSize, &append);
                log_trace2("assignment \'%s\'.", req->assignment);

                if (bufSize == 0) {
                	/* RUMA sets this. But we need at least
                	 * some bytes to store data in, so set it
                	 * to 80. */
                	bufSize = 80;
                }

                /* NOTE! This implementation does not implement FM_BUFF_LINE or FM_BUFF_FULL
                 * Only FM_BUF_NONE. bufSize will only give max read from fd and then send
                 * back signal unbuffered */

				if (remote_fd == 0) {
                        child->data.in.handle = handle;
                        child->data.in.spid = sender;
                        child->data.in.mode = bufMode;
                        child->data.in.size = bufSize;
                        child->data.in.append = append;
                } else if (remote_fd == 1) {
                        child->data.out.handle = handle;
                        child->data.out.spid = sender;
                        child->data.out.mode = bufMode;
                        child->data.out.size = bufSize;
                        child->data.out.append = append;
                } else if (remote_fd == 2) {
                        child->data.err.handle = handle;
                        child->data.err.spid = sender;
                        child->data.err.mode = bufMode;
                        child->data.err.size = bufSize;
                        child->data.err.append = append;
                } else {
                        log_err("Unexpected file descriptor (%d)", remote_fd);
                }
        } else if (strcmp(name, "PA") == 0) {
                child->data.parent = sender;
                log_trace2("Parent is 0x%x", sender);
        }
}

/******************************************************************************/

static int
lhsh_shelld_itc_errorhandler(const char *itc_call, const char *buffer, uint32_t flags,
                             const char *file, int line)
{
    const char *basename __attribute__((unused)) = file;

    /* If itc is not compiled with file and line support we insert this function's
       file and line in order to get some idea of where to start digging. */
    if (!basename || line == 0) {
        line = __LINE__;
        file = basename = __FILE__;
    }

    while (*file) {
        if (*file++ == '/')
            basename = file;
    }

    log_err(
        "%s:%d ERROR: %s in %s with %x flags (errno = %d)",
        basename, line, buffer, itc_call, flags, errno);

    return 1;
}

/******************************************************************************/

static void
handle_shell_setenv_child(itc_mbox_id_t sender,
                          struct ShellSetenvChildSender *req)
{
        struct child_descriptor *child;

        child = find_child_descriptor(ntohl(req->child_pid));
        if (child == NULL) {
                log_err("Invalid child 0x%08x", ntohl(req->child_pid));
                return;
        }

        if (!strncmp(req->assignment, "PROMPT=", 7))
                child->data.prompt = strdup(strstr(req->assignment, "=") + 1);
        else
                child->data.prompt = NULL;
}

/******************************************************************************/

static void *
__handle_shell_start_child(void *data)
{
        struct ShellStartChild *req = data;
        struct child_descriptor *child;

        child = find_child_descriptor(ntohl(req->child_pid));

        if (child == NULL) {
                log_err("Request to start invalid child (0x%08x)", ntohl(req->child_pid));
                free(req);
                pthread_exit(NULL);
        }

        create_child(&child->data);


        /* Unlink and free child descriptor */
        if (child->data.cmd_str)
                free((char *)child->data.cmd_str);
        if (child->data.prompt)
                free((char *)child->data.prompt);
        free(unlink_child_descriptor(child)); /* free(NULL) is ok (NOP) */

        free(req);
        pthread_exit(NULL);
}

static void
handle_shell_start_child(struct ShellStartChild *req)
{
        pthread_t tid;
        pthread_attr_t tattr;

        void *data = malloc(sizeof(struct ShellStartChild));
        if (data) {
                memcpy(data, req, sizeof(struct ShellStartChild));

                /* Continue in a new thread since we need to create a new
                 * dedicated ITC mailbox. This will also make is possible to
                 * process requests in parallel. Create the thread in detached
                 * state since we do not wish to block on join here! */
                pthread_attr_init(&tattr);
                pthread_attr_setdetachstate(&tattr, PTHREAD_CREATE_DETACHED);
                if (pthread_create(&tid, &tattr, __handle_shell_start_child, data))
                        perror("pthread_create");
        }
}

static void
handle_shell_kill_child(union itc_msg *sig)
{
        struct child_descriptor *child;

        child = find_child_descriptor(ntohl(sig->t_shell_kill_child.child_pid));

        if (child != NULL && child->data.child != ITC_NO_ID) {
                log_trace2("Forwarding SHELL_KILL_CHILD to child");
                itc_send(&sig, child->data.child, ITC_MY_MBOX);
        } else {
                log_info("Ignored SHELL_KILL_CHILD");
                itc_free(&sig);
        }
}

/******************************************************************************/

int shelld_main(void)
{
        log_info("ose_shelld server started. PATH prefix is: %s", path_prefix);

        /* Initialize ITC */
        itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

#ifdef HAVE_SYSLOG_H
        /* Initialize syslog */
        setlogmask(LOG_UPTO(LOG_INFO));
        openlog("lhsh_shelld", LOG_CONS | LOG_PID, LOG_USER);
#endif

        /* Register itc error handler */
        itc_register_errorhandler(&lhsh_shelld_itc_errorhandler);

        /* Create our mailbox. */
        server_mbox = itc_create_mailbox("ose_shelld", 0);
        if (server_mbox == ITC_NO_ID)
                return -1;

        while (1) {
                union itc_msg *sig;
                itc_mbox_id_t sender;

                sig = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

                sender = itc_sender(sig);

                switch (sig->sigNo) {
                case SHELL_INTERFACE_REQUEST:
                        log_trace2("SHELL_INTERFACE_REQUEST from 0x%08x", sender);
                        handle_shell_interface_request(sender);
                        break;

                case SHELL_CREATE_CHILD_REQUEST:
                        log_trace2("SHELL_CREATE_CHILD_REQUEST from 0x%08x", sender);
                        handle_shell_create_child_request(sender,
                                                          &sig->t_shell_creat_child_req);
                        break;

                case SHELL_SETENV_CHILD_SENDER:
                        log_trace2("SHELL_SETENV_CHILD_SENDER from 0x%08x", sender);
                        handle_shell_setenv_child_sender(sender,
                                                         &sig->t_shell_setenv_child_sender);
                        break;

                case SHELL_SETENV_CHILD:
                        log_trace2("SHELL_SETENV_CHILD from 0x%08x", sender);
                        handle_shell_setenv_child(sender, &sig->t_shell_setenv_child_sender);
                        break;

                case SHELL_START_CHILD:
                        log_trace2("SHELL_START_CHILD from 0x%08x", sender);
                        handle_shell_start_child(&sig->t_shell_start_child);
                        break;

                case SHELL_KILL_CHILD:
                        log_trace2("SHELL_KILL_CHILD(0x%08x) from 0x%08x",
                                   htonl(sig->t_shell_kill_child.child_pid), sender);
                        handle_shell_kill_child(sig);
                        continue;

                default:
                        log_err("Unrecognized signal 0x%08x from 0x%08x", sig->sigNo, sender);
                }

                itc_free(&sig);
        }

        itc_delete_mailbox(server_mbox);
        itc_exit();

        exit(1);
}

/******************************************************************************/

int main(int argc, char *argv[])
{
        int opt;
	char * board;
	num_of_trxm = 0;

        for (opterr = 0; (opt = getopt(argc, argv, "p:n:l:")) != -1;) {
                switch (opt) {
                case 'p':
                        path_prefix = optarg;
                        break;
		case 'n':
			num_of_trxm = atoi(optarg);
			break;
		case 'l':
			linkhandler_name = optarg;
			break;
                default:
                        help_print();
                        return 0;
                }
        }
	if ((board = getenv("SYS_BOARD_TYPE")) == NULL) {
		log_err("SYS_BOARD_TYPE not found, aborting");
			return -EFAULT;
        }
	log_info("SYS_BOARD_TYPE is %s",board);
	/*trxm  board */
	if (strcmp(board, "TRXM") == 0) {
		num_of_trxm = 0;
	}
        shelld_main();
        return 0;
}

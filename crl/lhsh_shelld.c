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
#include <signal.h>
#include <arpa/inet.h>
#include <itc.h>
#include <itc_system.h>
#include "lhsh_shelld.sig"
#include "lhsh_child.h"
#include "log.h"
#include "time.h"
#include <sys/queue.h>

#define DEFAULT_LHSH_PATH ""
#define MAX_ITC_MAILBOXES 32
#define MAX_LHSH_SESSIONS ((MAX_ITC_MAILBOXES/2)-1)

/*****************************************************************************/

struct pid_node {
        int child_pid;
        STAILQ_ENTRY(pid_node) entries; /*Queue for storing available child pids*/
};

STAILQ_HEAD(child_pids_head, pid_node) head = STAILQ_HEAD_INITIALIZER(head);

/******************************************************************************/

union itc_msg {
        uint32_t sigNo;

        struct ShellInterfaceRequest  t_shell_interface_req;
        struct ShellInterfaceReply    t_shell_interface_reply;
        struct ShellCreateChild       t_shell_creat_child_req;
        struct ShellCreateChild       t_shell_creat_child_reply;
        struct ShellSetenvChildSender t_shell_setenv_child_sender;
        struct ShellSetenvChild       t_shell_setenv_child;
        struct ShellGetenvChildRequest t_shell_getenv_child_req;
        struct ShellGetenvChildReply  t_shell_getenv_child_reply;
        struct ShellStartChild        t_shell_start_child;
        struct ShellKillChild         t_shell_kill_child;
	struct ChildDone              t_child_done;
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
static uint32_t num_of_trxm = 0;
static const char *linkhandler_name = "trxm";
/* Number of children that are currently existing in the system.
 * This number should not exceed the MAX_LHSH_SESSIONS number */
static int child_count = 0;
pthread_mutex_t list_mutex = PTHREAD_MUTEX_INITIALIZER;

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

static const uint32_t sigs_handled[] = {
        SHELL_INTERFACE_REQUEST,
        SHELL_CREATE_CHILD_REQUEST,
        SHELL_SETENV_CHILD,
        SHELL_SETENV_CHILD_SENDER,
        SHELL_START_CHILD,
        SHELL_KILL_CHILD,
        SHELL_GETENV_CHILD_REQUEST
};

static int nsigs_handled = sizeof(sigs_handled) / sizeof(sigs_handled[0]);

static void handle_shell_interface_request(itc_mbox_id_t replyTo)
{
        union itc_msg *reply;
        int i;

        reply = itc_alloc(sizeof(struct ShellInterfaceReply) + sizeof(sigs_handled),
                          SHELL_INTERFACE_REPLY);

        reply->t_shell_interface_reply.status     = 0;
        reply->t_shell_interface_reply.biosHandle = 0;
        sprintf(reply->t_shell_interface_reply.whatStr, "comba shelld");
        reply->t_shell_interface_reply.sigs[0] = htonl(nsigs_handled);
        /* Added to avoid delay of 100 ms on the client side. Delay happens
         * when we send an empty shell interface reply to the client as it then thinks that
         * SHELL_GETENV_CHILD_REQUEST is unsupported. Support for the mentioned signal
         * added in this commit. */
        for (i = 0; i < nsigs_handled; i++)
                reply->t_shell_interface_reply.sigs[i + 1] = htonl(sigs_handled[i]);

        itc_send(&reply, replyTo, ITC_MY_MBOX);
        log_trace2("sent SHELL_INTERFACE_REPLY to 0x%08x",
                   replyTo);
}

/******************************************************************************/

static void
handle_shell_create_child_request(itc_mbox_id_t replyTo,
                                  struct ShellCreateChild *req)
{
        struct child_descriptor *child = NULL;
        union itc_msg *reply;
        struct pid_node *temp;
        char box_name[16];

        print_create_child_request(req);
        /* Send reply */
        reply = itc_alloc(sizeof(struct ShellCreateChild),
                          SHELL_CREATE_CHILD_REPLY);

        memset(&(reply->t_shell_creat_child_req.status), 0,
               (sizeof(struct ShellCreateChild) - sizeof(reply->sigNo)));
        reply->t_shell_creat_child_req.att_ref   = 0;
        pthread_mutex_lock(&list_mutex);

        if (child_count < MAX_LHSH_SESSIONS) {

                child = calloc(1, sizeof(struct child_descriptor));
                if (child == NULL) {
                        log_err("Unable to alocate place for child. SHELL_CREATE_CHILD_REQUEST aborted.");
                        itc_free(&reply);
                        pthread_mutex_unlock(&list_mutex);
                        return;
                }
                child->data.cmd_str = strdup(&req->s[ntohl(req->argv)]);
                child->data.path_prefix = path_prefix;
                child->data.child = ITC_NO_ID;
		child -> data.num_of_trxm = num_of_trxm;
       		child -> data.linkhandler_name = linkhandler_name;
        	log_trace2("num of trxm is %d, child linkhandler name is %s, and linkhanler is %s",
                child -> data.num_of_trxm,child -> data.linkhandler_name,linkhandler_name);
                child->next = child_list;
                child_list = child;

                /* Access the queue of available child_pids and assign
                 * one to the child */

                temp = STAILQ_FIRST(&head);
                if (temp != NULL) {
                        STAILQ_REMOVE_HEAD(&head, entries);
                        child->pid = temp->child_pid;
                        free(temp);
                } else {
                        /* something went wrong - assign pid 0 */
                        child->pid = 0;
                }

                log_trace2("Child was assigned with the pid: %d and will execute cmd: %s",
                           child->pid, child->data.cmd_str);

                /* Increase the number of created children */
                child_count += 1;
                log_trace2("CHILD COUNT = %d", child_count);

                /*
                 * This can get set in a weird way if the client sends a PA= env var,
                 * but what the hell is that???
                 */
                child->data.parent = replyTo;
                log_trace2("Child %d will send his response to client %x", child->pid,
                           replyTo);

                /* Create a cloned "child mailbox"
                 * Other side may use this for ATTATCH signals */
                sprintf(box_name, "mbox_%d", child->pid);
                child->data.mbox = itc_clone_mailbox(ITC_MY_MBOX, box_name);
                child->data.mbox_name = strdup(box_name);

                child->data.clientRef = itc_monitor(replyTo,
                                                    NULL); /* let us know when sender (client) dies */

                /* We assign pid [1..31] from the queue of availabe pids to child,
                 * notify client with the assigned pid */
                reply->t_shell_creat_child_req.child_pid = htonl(child->pid);

                log_trace2("Mailbox with id %d [%s] assigned to child %d", child->data.mbox,
                           box_name, child->pid);

                /* sending back response to client */

                itc_send(&reply, replyTo, child->data.mbox);

        } else {
                log_info("Maximum number of lhsh sessions already reached.");
                reply->t_shell_creat_child_req.child_pid = 0;
                reply->t_shell_creat_child_req.status = -2;
                /* sending back response to client, no more space */
                itc_send(&reply, replyTo, ITC_MY_MBOX);
        }

        /* we are done with modifying cd so we can release our mutex */
        pthread_mutex_unlock(&list_mutex);
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
                if (tmp->pid == pid) {
                        pthread_mutex_unlock(&list_mutex);
                        return tmp;
                }
                tmp = tmp->next;
        }

        pthread_mutex_unlock(&list_mutex);

        return NULL;
}

/******************************************************************************/

/**
 * @brief
 *
 * Finds and executes child by pid.
 *
 * @param pid Pid of the child that we want to start
 *
 * @return Child descriptor or NULL if child is already unlinked
 *
 */

static struct child_descriptor *find_and_execute_child(uint32_t pid)
{
        struct child_descriptor *tmp;

        log_trace2("Child to execute: %d", pid);

        pthread_mutex_lock(&list_mutex);

        tmp = child_list;
        while (tmp) {
                if ((tmp->pid == pid)) {
                        /* start child under mutex, we must not end up with
                         * currupted data in child process in case someone
                         * starts to clean our child data right before we fork,
                         * that happens from the mother thread when the
                         * client crashes right after sending start
                         * child requst -> cleanup procedure begins so we
                         * end up with junk in data */
                        log_trace2("About to start child %d.", pid);
                        create_child(&tmp->data);
                        return tmp;
                }
                tmp = tmp->next;
        }
        log_trace2("Request to start an invalid child: %d, it seems that child is already unlinked.",
                   pid);
        pthread_mutex_unlock(&list_mutex);
        return NULL;
}

/******************************************************************************/

/**
 * @brief
 *
 * Deletes child descriptor data
 *
 * @param cd Child descriptor that we want to free
 *
 */

static void free_child_data(struct child_descriptor *cd)
{
        log_trace2("Deleting mailbox: %s.", cd->data.mbox_name);

        itc_delete_mailbox(cd->data.mbox);

        log_trace2("Cleaning child descriptor of: %d.",
                   cd->data.mbox);

        if (cd->data.mbox_name)
                free((char *)cd->data.mbox_name);
        if (cd->data.cmd_str)
                free((char *)cd->data.cmd_str);
        if (cd->data.prompt)
                free((char *)cd->data.prompt);

        free(cd);
}

/******************************************************************************/

/**
 * @brief
 *
 * Returnes pid of unlinked child to the queue of available pids
 *
 * @param child_pid pid that we want to return to the queue
 *
 */

static void return_pid_to_queue(pid_t child_pid)
{
        struct pid_node *temp;

        /* return child pid to the queue
         * of availble pids */
        temp = malloc(sizeof(struct pid_node));
        if (temp != NULL) {
                temp->child_pid = child_pid;
                STAILQ_INSERT_TAIL(&head, temp, entries);
                log_trace2("Returned pid: %d to the queue of available pids.",
                           child_pid);
        } else {
                log_err("Unable to return pid: %d to the queue of available pids.",
                        child_pid);
        }
}

/******************************************************************************/

/**
 * @brief
 *
 * Removes the child descriptor from the linked list of descriptors as
 * the child is obviously done here. Pid of the child is returned to the
 * queue of available pids and its descriptor freed.
 *
 * @param child_mbox Mbox id of the child that is finished
 *
 */

static void
unlink_child_descriptor_child (itc_mbox_id_t child_mbox)
{
        struct child_descriptor **tmp;
        struct child_descriptor *cd;

        pthread_mutex_lock(&list_mutex);
        tmp = &child_list;

        while (*tmp) {
                if ((*tmp)->data.child == child_mbox) {
                        /* return child pid to queue */
                        log_trace2("Pid: %d will be returned to queue from unlink_child.",
                                   (*tmp)->pid);
                        return_pid_to_queue((*tmp)->pid);

                        cd = (*tmp);
                        (*tmp) = (*tmp)->next;
                        child_count -= 1;

                        free_child_data(cd);

                        log_trace2("Cleaning child descriptor from unlink_child - OK");
                        pthread_mutex_unlock(&list_mutex);
                        return;
                }
                if (*tmp)
                        tmp = &(*tmp)->next;
        }

        pthread_mutex_unlock(&list_mutex);

        log_trace2("Unlinking of child descriptor finished for child 0x%08x.",
                   (unsigned)child_mbox);
        return;
}

/******************************************************************************/

/**
 * @brief
 *
 * Unlinks all child descriptors that are associated with the dead client.
 * All their pids are here returned to the list of available pids.
 *
 * @param parent Id of client mailbox (dead client)
 *
 */

static void
unlink_child_descriptor_client (itc_mbox_id_t parent)
{
        struct child_descriptor **tmp;
        struct child_descriptor *cd;

        pthread_mutex_lock(&list_mutex);
        tmp = &child_list;

        while (*tmp) {
                if ((*tmp)->data.parent == parent) {
                        /* return child pid to the queue
                         * of availble pids */
                        log_trace2("Pid: %d will be returned to queue from unlink_client.",
                                   (*tmp)->pid);
                        return_pid_to_queue((*tmp)->pid);

                        cd = (*tmp);
                        (*tmp) = (*tmp)->next;
                        child_count -= 1;

                        log_trace2("About to clean child descriptor: %d of client: %x. Client died unexpectedly.",
                                   cd->data.mbox, parent);

                        free_child_data(cd);
                        cd = NULL;
                        log_trace2("Cleaning child descriptors for: %x from unlink_client, removed child - OK",
                                   parent);

                }
                if (*tmp != NULL)
                        tmp = &(*tmp)->next;
        }

        pthread_mutex_unlock(&list_mutex);

        log_trace2("Finished with unlinking of child descriptors for client 0x%08x.",
                   (unsigned)parent);
        return;
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
                        /* Misebehaving client sets this to 0. But we need at least
                         * some bytes to store data in, so set it
                         * to 4096. */
                        bufSize = 4096;
                }

                if (remote_fd == 0){
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
lhsh_shelld_itc_errorhandler(const char *itc_call, const char *buffer,
                             uint32_t flags,
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

static void
handle_client_done(itc_mbox_id_t sender)
{
        /* client has crashed, unlink and clean all his child descriptors*/
        unlink_child_descriptor_client(sender);
}

/******************************************************************************/

/*
 * Clients can call this repeatedly until the value returned is non-empty.
 * This is to avoid a race problem related to unacknowledged SHELL_SETENV_CHILD_SENDER
 * signals (IIRC).  Otherwise, if the INTERFACE_REPLY says SHELL_GETENV_CHILD_REQUEST
 * is not supported, the client can pause for ~100ms per command, which wastes time.
 * The client only ever asks for FD<n> so that's all we'll implement.
 */
static void
handle_shell_getenv_child(itc_mbox_id_t sender,
                          struct ShellGetenvChildRequest *req)
{
        static const int smax = 64;
        struct child_descriptor *child;
        struct ose_file *f = NULL;
        union itc_msg *reply;
        int fd;

        child = find_child_descriptor(ntohl(req->child_pid));
        if (child == NULL) {
                log_err("Invalid child 0x%08x", ntohl(req->child_pid));
                goto reply;
        }

        if (strncmp(req->name, "FD", 2) != 0)
                goto reply;
        fd = atoi(req->name + 2);

        switch (fd) {
        case 0:
                f = &child->data.in;
                break;
        case 1:
                f = &child->data.out;
                break;
        case 2:
                f = &child->data.err;
                break;
        default:
                goto reply;
        }

reply:

        reply = itc_alloc(sizeof(struct ShellGetenvChildReply) + smax,
                          SHELL_GETENV_CHILD_REPLY);
        reply->t_shell_getenv_child_reply.child_pid = req->child_pid;
        if (f && f->spid != 0) {
                reply->t_shell_getenv_child_reply.success = 1;
                snprintf(reply->t_shell_getenv_child_reply.assignment, smax,
                         "%s=0x%x,0x%x,%d,%d,0x%x",
                         req->name, f->spid, f->handle, f->mode, f->size, f->append);
        } else {
                reply->t_shell_getenv_child_reply.success = 0;
                snprintf(reply->t_shell_getenv_child_reply.assignment, smax, "%s=",
                         req->name);
        }
        itc_send(&reply, sender, ITC_MY_MBOX);
}

/******************************************************************************/

static void
handle_child_done(struct ChildDone *done)
{
        unlink_child_descriptor_child(done->mbox);
}

/******************************************************************************/

static void *
__handle_shell_start_child(void *data)
{
        struct ShellStartChild *req = data;
        struct child_descriptor *child;
        union itc_msg *notifyme;

        child = find_and_execute_child(ntohl(req->child_pid));

        /* Message used to notify server of child exit. */
        notifyme = itc_alloc(sizeof(struct ChildDone), CHILD_DONE);
        notifyme->t_child_done.status = 0;
        notifyme->t_child_done.error = 0;

        if (child) {
                notifyme->t_child_done.mbox = child->data.child;
                itc_send(&notifyme, server_mbox, server_mbox);
        } else {
                log_trace2("Child data already cleaned, child_done won't be executed.");
                itc_free(&notifyme);
        }

        free(req);

        pthread_exit(NULL);

}

static void
handle_shell_start_child(struct ShellStartChild *req)
{
        pthread_t tid;
        pthread_attr_t tattr;
        struct child_descriptor *child_p;

        void *data = malloc(sizeof(struct ShellStartChild));
        if (data == NULL) {
                log_err("Unable to allocate child data.");
                return;
        }

        memcpy(data, req, sizeof(struct ShellStartChild));

        child_p = find_child_descriptor(ntohl(req->child_pid));
        if (child_p == NULL) {
                log_err("Unable to locate child descriptor.");
                free(data);
                return;
        }
        itc_unmonitor(child_p->data.clientRef);
        /* Continue in a new thread since we need to create a new
         * dedicated ITC mailbox. This will also make is possible to
         * process requests in parallel. Create the thread in detached
         * state since we do not wish to block on join here! */
        pthread_attr_init(&tattr);
        pthread_attr_setdetachstate(&tattr, PTHREAD_CREATE_DETACHED);
        if (pthread_create(&tid, &tattr, __handle_shell_start_child, data))
                perror("pthread_create");
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

static void handle_exit_signal()
{
        union itc_msg *msg;
        uint32_t rx_filter[] = {1, CHILD_DONE};
        struct child_descriptor *tmp = NULL;

        pthread_mutex_lock(&list_mutex);
        if (child_list) {
                tmp = child_list;
                while (tmp != NULL && tmp->data.child != ITC_NO_ID) {
                        msg = itc_alloc(sizeof(uint32_t), LHSH_EXIT_SIGNAL);
                        itc_send(&msg, tmp->data.child, ITC_MY_MBOX);
                        log_trace2("Sending exit signal to child: 0x%08x", (uint32_t)tmp);
                        tmp = tmp->next;
                }
                pthread_mutex_unlock(&list_mutex);
                while (1) {
                        msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
                        log_trace2("Child done received.");
                        pthread_mutex_lock(&list_mutex);
                        if (child_list == NULL) {
                                pthread_mutex_unlock(&list_mutex);
                                log_trace2("All children cleaned. Exiting as ordered.");
                                break;
                        } else {
                                pthread_mutex_unlock(&list_mutex);
                        }
                        itc_free(&msg);
                }
        } else {
                pthread_mutex_unlock(&list_mutex);
                log_trace2("No children to clean. Exiting as ordered.");
        }
}

static void exit_handler(int sig)
{
        union itc_msg *msg;
        log_info("Received signal 0x%X, terminating", sig);
        msg = itc_alloc(sizeof(uint32_t), LHSH_EXIT_SIGNAL);
        itc_send(&msg, server_mbox, ITC_MY_MBOX);
}

/******************************************************************************/

int shelld_main(void)
{
        int res = 0;
        struct pid_node *temp;  /* we use this to manipulate child_pid queue */
        int i;

        log_info("ose_shelld server started. PATH prefix is: %s", path_prefix);

        /* Initialize ITC */
        res = itc_init(MAX_ITC_MAILBOXES, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
        if (res != 0) {
                log_err("itc_init failed with error %d", res);
                return res;
        }

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

        /* Install SIGTERM signal. */
        if (signal(SIGTERM, exit_handler) == SIG_ERR) {
                log_err("Failed to install signal exit handler");
                exit(-EFAULT);
        }

        /* Initialize queue for the available child pids */

        STAILQ_INIT(&head);

        /* Insert head pid to queue */
        struct pid_node *head_node = malloc(sizeof(struct pid_node));

        if (head_node == NULL) {
                log_err("Unable to allocate child pid queue");
                exit(-EFAULT);
        }

        head_node->child_pid = 1;
        STAILQ_INSERT_HEAD(&head, head_node, entries);

        /* We have 2*MAX_LHSH_SESSIONS available pids */

        /* Why? Now when we are using queue for pids,
         * we must be carefull. For instance, if the monitor
         * for child 7 arrives before START_CHILD for 7,
         * 7 is returned to the queue and START_CHILD signal still
         * waits to be processed. If some other kid later gets
         * pid 7 everything will explode because we will try to
         * start new child - 7 with an old signal. Child 7 has
         * no env set yet! To avoid described situation we use
         * 2 * MAX_SESSIONS queue as only MAX_SESSIONS kids
         * can be active at the same time */

        for (i = 1; i <= (MAX_LHSH_SESSIONS * 2); i++) {
                temp = malloc(sizeof(struct pid_node));
                if (temp == NULL) {
                        log_err("Unable to allocate place for child pid's queue.");
                        exit(-EFAULT);
                }
                temp->child_pid = i + 1;
                STAILQ_INSERT_TAIL(&head, temp, entries);
        }

        while (1) {
                union itc_msg *sig;
                itc_mbox_id_t sender;

                sig = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

                if (sig->sigNo == LHSH_EXIT_SIGNAL) {
                        log_trace2("Started exit procedure");
                        itc_free(&sig);
                        handle_exit_signal();
                        break;
                }

                sender = itc_sender(sig);

                switch (sig->sigNo) {
                case ITC_MONITOR_DEFAULT_NO:
                        log_trace2("received ITC_MONITOR_DEFAULT_NO from %x",
                                   sender);
                        handle_client_done(sender);
                        break;
                case CHILD_DONE:
                        log_trace2("Received child done.");
                        handle_child_done(&sig->t_child_done);
                        break;
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

                case SHELL_GETENV_CHILD_REQUEST:
                        log_trace2("SHELL_GETENV_CHILD_REQUEST from 0x%08x", sender);
                        handle_shell_getenv_child(sender, &sig->t_shell_getenv_child_req);
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

        /* we are done, clean the queue of child pids */

        while (!STAILQ_EMPTY(&head)) {
                temp = STAILQ_FIRST(&head);
                STAILQ_REMOVE_HEAD(&head, entries);
                free(temp);
        }

        exit(0);
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

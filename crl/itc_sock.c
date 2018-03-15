/**
 *   ITC socket transport implementation.
 *
 *   @file itc_sock.c
 *
 *   The socket implementation is used to "connect" to itcworld so that
 *   ITC gets its world ID and also so that ITC will be notified when
 *   this process dies.
 *
 *   Copyright (C) 2010 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : [2010-02-01 Lars Magnus Ericsson]
 *   Change  : First version.
 *
 *   Revised : 2014-01-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for multiple itcworld instances.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support in ITC to work after a fork.
 *             Removed init of itci_create_ep to NULL.
 *             Improved keeping of itc_sock data transport functions
 *             whithin a compile time definition.
 *
 *   Revised : 2014-02-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for determining max message size.
 *             Removed unused variable world_transport in init_alloc.
 *
 *   Revised : 2014-09-29 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed sockets so that they do not use an "abstract
 *             socket address" to allow ITC to be used between
 *             network namespaces.
 *
 *   Revised : 2015-09-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed the internal itc_threads to have scheduling policy
 *             and priority set by environment variables CRL_SCHED_POLICY
 *             and CRL_SCHED_PRIO. If no environment variables are set the
 *             threads will defaul to sheeduling polisy SCHED_OTHER
 *             priority 0.
 *
 *   Revised : 2015-10-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed handling of locate world when world can not
 *             accept any more ITC "clients" so that itc_init fails.
 *
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>

#include "pthread.h"

#include "search.h"

#include "itc.h"
#include "itc_system.h"
#include "itc_impl.h"
#include "itc_messages.h"
#include "itci_internal.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define RXLEN 1024


#define ITC_SOCK_BASE_MSG_NO (ITC_MSG_BASE + 0x210)
#define SOCK_MSG           (ITC_SOCK_BASE_MSG_NO + 0)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
#ifdef ITC_SOCK_DATA
#define TX_MSG   (ITC_SOCK_BASE_MSG_NO + 1)
#define FREE_MSG (ITC_SOCK_BASE_MSG_NO + 2)
struct tx_msg {
        uint32_t      msgno;
        pid_t         pid;
        itc_mbox_id_t mbox_id;
        int           size;
        void         *addr;
};

struct sock_target {
        itc_mbox_id_t mbox_id;

        struct sockaddr_un addr;
};
#endif

struct sock_instance {
        int                sd;

        bool               loc_world_run;

        char              *rundir_path;

#ifdef ITC_SOCK_DATA
        void              *sock_target_tree;
        pthread_rwlock_t   sock_target_rwlock;

        pthread_mutex_t    tx_m;
        int                my_tx_sd;

        pid_t              pid;

        struct sockaddr_un my_rx_addr;
        int                my_rx_sd;

        itc_mbox_id_t      mbox_id;
        itc_mbox_id_t      world_mask;
#endif
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static struct sock_instance sock_inst;

#ifdef ITC_SOCK_DATA
static __thread int mytxsd = 0;
#endif

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
#ifdef ITC_SOCK_DATA
static void *sock_rx_thread(void *data);
#endif

static bool sock_locate_world(itc_mbox_id_t  *my_world_id,
                              itc_mbox_id_t  *world_mask,
                              itc_mbox_id_t  *world_mbox_id);

static int sock_init(itc_mbox_id_t  my_world_id,
                     itc_mbox_id_t  world_mask,
                     int            mbox_count,
                     uint32_t       flags);

static int sock_exit(void);

#ifdef ITC_SOCK_DATA /* The below implementation is kept but removed from build to
                      * keep it present if it ever is needed again. */
static int sock_create_mailbox(struct itc_mailbox *mbox, uint32_t flags);

static int sock_delete_mailbox(struct itc_mailbox *mbox);

static int sock_send(struct itc_mailbox *mbox, struct itc_message *message,
                     itc_mbox_id_t to, itc_mbox_id_t from);
#endif

struct itci_transport_funcs sock_itc_funcs = { sock_locate_world,
                                               sock_init,
                                               sock_exit,
#ifdef ITC_SOCK_DATA
                                               sock_create_mailbox,
                                               sock_delete_mailbox,
                                               sock_send,
#else
                                               NULL,
                                               NULL,
                                               NULL,
#endif
                                               NULL,
                                               NULL,
                                               NULL
};

/* ===================================================================== */
/**
 *   sock_locate_world
 *
 *   @param my_world_id    Pointer to my_world_id, that is set by the
 *                         function.
 *
 *   @param world_mask     Pointer to world mask, that is set by the
 *                         function.
 *
 *   @param world_mbox_id  Pointer to mailbox id of ITC world, that is
 *                         set by the function.
 *
 *   @return               true at success.
 *
 *   @par Globals:         sock_inst
 *                         ITC socket instance global structure.
 *
 *   Function to locate ITC world, used if sockets are used for the ITC world
 *   initial transport.
 */
/* ===================================================================== */
static bool sock_locate_world(itc_mbox_id_t  *my_world_id,
                              itc_mbox_id_t  *world_mask,
                              itc_mbox_id_t  *world_mbox_id)
{
        int sd, res, len;
        struct sockaddr_un world_addr;
        struct itc_locate_world_repl *itc_locate_world_repl;

        sock_inst.rundir_path = itc_get_rundir();
        if(sock_inst.rundir_path == NULL) {
                ITC_TRACE_ERROR("Failed to get rundir path", 0);
                return false;
        }

        sd = socket(AF_LOCAL, SOCK_STREAM, 0);
        if (sd < 0) {
                ITC_TRACE_ERROR("socket setup failed: %d", sd);
                return false;
        }

        memset(&world_addr, 0, sizeof(struct sockaddr_un));
        world_addr.sun_family = AF_LOCAL;

        strcpy(world_addr.sun_path, sock_inst.rundir_path);
        strcat(world_addr.sun_path, ITC_WORLD_LOC_NAME);
        res = connect(sd, (struct sockaddr *)&world_addr, sizeof(world_addr));
        if (res < 0) {
                ITC_TRACE_ERROR("Connect to world failed: %d %s",
				errno, strerror(errno));
                return false;
        }

        itc_locate_world_repl = malloc(RXLEN);
        if(itc_locate_world_repl == NULL) {
                ITC_TRACE_ERROR("Out of memory", 0);
                return false;
        }

        len = recv(sd, itc_locate_world_repl, RXLEN, 0);
        if (len < sizeof(struct itc_locate_world_repl)) {
                ITC_TRACE_ERROR("ITC locate world reply recv error: %d", len);
                free(itc_locate_world_repl);
                return false;
        }

        res = close(sd);

        if(itc_locate_world_repl->msgno == ITC_LOCATE_WORLD_REPL &&
	   itc_locate_world_repl->my_id != ITC_NO_ID) {
                *my_world_id   = itc_locate_world_repl->my_id;
                *world_mask    = itc_locate_world_repl->world_mask;
                *world_mbox_id = itc_locate_world_repl->world_mbox_id;

                sock_inst.loc_world_run = true;
	} else if(itc_locate_world_repl->msgno == ITC_LOCATE_WORLD_REPL &&
		  itc_locate_world_repl->my_id == ITC_NO_ID) {
	    ITC_TRACE_ERROR("No more ITC programs possible (max %d), locate to world rejected\n",
			    SUPPORTED_PROCESSES);
                free(itc_locate_world_repl);
                return false;
        } else {
                ITC_TRACE_ERROR("Incorrect response from world: 0x%08x",
                                itc_locate_world_repl->msgno);
                free(itc_locate_world_repl);
                return false;
        }

        free(itc_locate_world_repl);

        return true;
}

/* ===================================================================== */
/**
 *   sock_init
 *
 *   @param my_world_id  My world id, from world.
 *
 *   @param world_mask   World mask.
 *
 *   @param mbox_count   How many local mailboxes shall be supported.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     sock_inst
 *                     ITC socket instance global structure.
 *
 *   Init function of the ITC by socket functionality, called by itc_init.
 */
/* ===================================================================== */
static int sock_init(itc_mbox_id_t  my_world_id,
                     itc_mbox_id_t  world_mask,
                     int            mbox_count,
                     uint32_t       flags)
{
        int sd, res;
        struct sockaddr_un world_addr;

        /* The sock_init can always be rerun so we do not bother to check
           if it has already been run since this is already dealt with
           in itc.c. */

        if(sock_inst.loc_world_run) {

                sd = socket(AF_LOCAL, SOCK_STREAM, 0);
                if (sd < 0) {
                        ITC_TRACE_ERROR("socket setup failed: %d", sd);
                        return ITC_EINTERNAL_ERROR;
                }

                memset(&world_addr, 0, sizeof(struct sockaddr_un));
                world_addr.sun_family = AF_LOCAL;
                sprintf(world_addr.sun_path, "%s%s_0x%08x",
                        sock_inst.rundir_path, ITC_WORLD_NAME, my_world_id);

                res = connect(sd, (struct sockaddr *)&world_addr, sizeof(world_addr));
                if (res < 0) {
                        ITC_TRACE_ERROR("Connect to world failed: %d", res);
                        return ITC_EINTERNAL_ERROR;
                }

                sock_inst.sd         = sd;
        }

#ifdef ITC_SOCK_DATA
        sock_inst.pid        = getpid();
        sock_inst.world_mask = world_mask;

        if(pthread_mutex_init(&sock_inst.tx_m, NULL) != 0) {
                ITC_TRACE_ERROR("Mutex init failed", 0);
                return ITC_EINTERNAL_ERROR;
        }

        if(pthread_rwlock_init(&sock_inst.sock_target_rwlock, NULL) != 0) {
                ITC_TRACE_ERROR("RW lock init failed", 0);
                return ITC_EINTERNAL_ERROR;
        }

        sock_inst.my_tx_sd = socket(AF_LOCAL, SOCK_DGRAM, 0);
        if(sock_inst.my_tx_sd < 0) {
                ITC_TRACE_ERROR("TX socket call failed: %d",
                                sock_inst.my_tx_sd);
                return ITC_EINTERNAL_ERROR;
        }

        if(add_itcthread(sock_rx_thread, (void *)(unsigned long)my_world_id,
			 true, NULL) < 0) {
                return ITC_EINTERNAL_ERROR;
        }
#endif

        return 0;
}

/* ===================================================================== */
/**
 *   sock_exit
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     sock_inst
 *                     ITC socket instance global structure.
 *
 *   Exit function of the ITC by socket functionality, called by itc_exit.
 */
/* ===================================================================== */
static int sock_exit(void)
{
        int res;

        if(sock_inst.loc_world_run) {

                res = close(sock_inst.sd);
                if (res < 0) {
                        ITC_TRACE_ERROR("Close world socket failed: %d", res);
                        return -1;
                }
        }

#ifdef ITC_SOCK_DATA
        res = close(sock_inst.my_tx_sd);
        if (res < 0) {
                ITC_TRACE_ERROR("Close tx socket failed: %d", res);
                return -1;
        }

        if(pthread_mutex_destroy(&sock_inst.tx_m) != 0) {
                ITC_TRACE_ERROR("mutex destroy failed", 0);
                return -1;
        }

        if(pthread_rwlock_destroy(&sock_inst.sock_target_rwlock) != 0) {
                ITC_TRACE_ERROR("RW destroy init failed", 0);
                return -1;
        }
#endif

        memset(&sock_inst, 0, sizeof(struct sock_instance));

        return 0;
}

#if ITC_SOCK_DATA /* The below implementation is kept but removed from build to
                   * keep it present if it ever is needed again. */
/* ===================================================================== */
/**
 *   compare_mbox
 *
 *   @param pa         Pointer to target A.
 *
 *   @param pb         Pointer to target B.
 *
 *   @return           0 if mailbox id is equal.
 *                     negative if mailbox id A less than B.
 *                     positive if mailbox id A larger than B.
 *
 *   @par Globals:     --
 *
 *   Compare function of 2 mailbox ids, used together with the
 *   tree functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_mbox(const void *pa, const void *pb)
{
        const struct sock_target *ta, *tb;
        int res;

        ta = pa;
        tb = pb;

        if(ta->mbox_id < tb->mbox_id)
                res = -1;
        else if(ta->mbox_id > tb->mbox_id)
                res = 1;
        else
                res = 0;

        return res;
}

/* ===================================================================== */
/**
 *   add_sock_target
 *
 *   @param mbox_id    Mailbox id to add.
 *
 *   @return           0 if mailbox successfully added.
 *
 *   @par Globals:     sock_inst
 *                         ITC socket instance global structure.
 *
 *   Add a new socket traget.
 */
/* ===================================================================== */
static int add_sock_target(itc_mbox_id_t mbox_id)
{
        itc_mbox_id_t new_id;
        struct sock_target *st, **tmp;

        new_id = mbox_id & sock_inst.world_mask;

        st = (struct sock_target *)itc_alloc(sizeof(struct sock_target), 0);
        memset(st, 0, sizeof(struct sock_target));
        st->mbox_id = new_id;
        st->addr.sun_family = AF_LOCAL;
        sprintf(st->addr.sun_path,
                "itc_rx_sock_0x%08x", new_id);

        pthread_rwlock_rdlock(&sock_inst.sock_target_rwlock);
        tmp = tfind(st, &sock_inst.sock_target_tree, compare_mbox);
        pthread_rwlock_unlock(&sock_inst.sock_target_rwlock);

        if(tmp != NULL) {
                itc_free((union itc_msg **)&st);
                return -1;
        } else {
                pthread_rwlock_wrlock(&sock_inst.sock_target_rwlock);
                tsearch(st, &sock_inst.sock_target_tree, compare_mbox);
                pthread_rwlock_unlock(&sock_inst.sock_target_rwlock);
        }

        return 0;
}

/* ===================================================================== */
/**
 *   get_sock_target
 *
 *   @param mbox_id    Mailbox id to get.
 *
 *   @return           Pointer to socket target.
 *
 *   @par Globals:     sock_inst
 *                         ITC socket instance global structure.
 *
 *   Get a socket target struct.
 */
/* ===================================================================== */
static struct sock_target *get_sock_target(itc_mbox_id_t mbox_id)
{
        struct sock_target st, **tmp;

        st.mbox_id = (mbox_id &sock_inst.world_mask);


        pthread_rwlock_rdlock(&sock_inst.sock_target_rwlock);
        tmp = tfind(&st, &sock_inst.sock_target_tree, compare_mbox);
        pthread_rwlock_unlock(&sock_inst.sock_target_rwlock);

        while(tmp == NULL) {
                add_sock_target(mbox_id);
                pthread_rwlock_rdlock(&sock_inst.sock_target_rwlock);
                tmp = tfind(&st, &sock_inst.sock_target_tree, compare_mbox);
                pthread_rwlock_unlock(&sock_inst.sock_target_rwlock);
       }

        return *tmp;
}

/* ===================================================================== */
/**
 *   sock_create_mailbox
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     sock_inst
 *                     ITC socket instance global structure.
 *
 *   Create mailbox function of the ITC by socket functionality, called
 *   by itc_create_mailbox.
 */
/* ===================================================================== */
static int sock_create_mailbox(struct itc_mailbox *mbox, uint32_t flags)
{
        int sd;

        sd = socket(AF_LOCAL, SOCK_DGRAM, 0);
        if(sd < 0) {
                ITC_TRACE_ERROR("TX socket call failed: %d",
                                sock_inst.my_tx_sd);
                return -1;
        }

        mytxsd = sd;

        return 0;
}

/* ===================================================================== */
/**
 *   sock_delete_mailbox
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     linx_inst
 *                     ITC Linx instance global structure.
 *
 *   Delete mailbox function of the ITC by socket functionality, called
 *   by itc_delete_mailbox.
 */
/* ===================================================================== */
static int sock_delete_mailbox(struct itc_mailbox *mbox)
{
        int res;

        if(mytxsd == 0) {
                ITC_TRACE_ERROR("No socket mailbox configured for 0x%08x",
                                mbox->mbox_id);
                return -1;
        }

        res = close(mytxsd);
        if(res < 0) {
                ITC_TRACE_ERROR("Close mailbox TX socket failed: %d", res);
                return -1;
        }

        mytxsd = 0;

        return 0;
}

/* ===================================================================== */
/**
 *   sock_send
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @param message    Pointer to message to be sent.
 *
 *   @param to         Mailbox id of message receipient.
 *
 *   @param from       Mailbox id of message sender.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     sock_inst
 *                     ITC socket instance global structure.
 *
 *   Send message function of the ITC by socket functionality, called
 *   by itc_send.
 */
/* ===================================================================== */
static int sock_send(struct itc_mailbox *mbox, struct itc_message *message,
                     itc_mbox_id_t to, itc_mbox_id_t from)
{
        struct sock_target *st;
        int res = -1;
        static struct tx_msg txmsg;

//        if(mytxsd == 0) {
//                ITC_TRACE_ERROR("Send from a not setup mailbox", 0);
//                return -1;
//        }

        st = get_sock_target(to);
        if(st == NULL) {
                ITC_TRACE_ERROR("Failed to add socket target 0x%08x", to);
                return -1;
        }

        txmsg.msgno   = TX_MSG;
        txmsg.pid     = sock_inst.pid;
        txmsg.mbox_id = mbox->mbox_id;
        txmsg.size    = message->size;
        txmsg.addr    = message;

        MUTEX_LOCK(&sock_inst.tx_m);
        while(res == -1) {
                res = sendto(sock_inst.my_tx_sd, &txmsg, sizeof(struct tx_msg), 0,
                             (struct sockaddr *)&st->addr, sizeof(struct sockaddr_un));

                if(res == -1 &&
                   errno != 111) {
                        break;
                }

                if(res == -1 &&
                   errno == 111) {
                        printf("Resend ECONNREFUSED\n");
                }

        }
        MUTEX_UNLOCK(&sock_inst.tx_m);

        if(res < 0) {
                ITC_TRACE_ERROR("sendto failed: %d (%d,%s)",
                                res, errno, strerror(errno));
        }

        return res;
}

/* ===================================================================== */
/**
 *   deliver_packet
 *
 *   @param buff       Pointer to packet buffer.
 *
 *   @param len        Packet length.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Deliver receive packet to recipients mailbox.
 */
/* ===================================================================== */
static int send_free_req(itc_mbox_id_t mbox_id, void *addr)
{
        struct sock_target *st;
        static struct tx_msg txmsg;
        int res = -1;

        st = get_sock_target(mbox_id);
        if(st == NULL) {
                return -1;
        }

        txmsg.msgno = FREE_MSG;
        txmsg.pid   = 0;
        txmsg.size  = 0;
        txmsg.addr  = addr;

        MUTEX_LOCK(&sock_inst.tx_m);
        while(res == -1) {
                res = sendto(sock_inst.my_tx_sd, &txmsg, sizeof(struct tx_msg), 0,
                             (struct sockaddr *)&st->addr, sizeof(struct sockaddr_un));

                if(res == -1 &&
                   errno != 111) {
                        break;
                }

                if(res == -1 &&
                   errno == 111) {
                        printf("Resend ECONNREFUSED\n");
                }

        }
        MUTEX_UNLOCK(&sock_inst.tx_m);

        if(res < 0) {
                ITC_TRACE_ERROR("sendto failed: %d (%d,%s)",
                                res, errno, strerror(errno));
        }

        return res;
}

/* ===================================================================== */
/**
 *   deliver_packet
 *
 *   @param buff       Pointer to packet buffer.
 *
 *   @param len        Packet length.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Deliver receive packet to recipients mailbox.
 */
/* ===================================================================== */
static int deliver_packet(char *buff, int len)
{
        struct itc_message *message;
        union itc_msg *msg;
        struct tx_msg *rxmsg;
        static struct iovec fromvec, tovec;
        int res;

        rxmsg = (struct tx_msg *)buff;
        if(rxmsg->msgno == TX_MSG) {
                msg = itc_alloc(rxmsg->size, 0);
                message = MSG_TO_MESSAGE(msg);

                fromvec.iov_base = rxmsg->addr;
                fromvec.iov_len  = rxmsg->size + ITC_MSG_ADM_OFFSET;
                tovec.iov_base   = message;
                tovec.iov_len    = rxmsg->size + ITC_MSG_ADM_OFFSET;

                res = process_vm_readv(rxmsg->pid, &tovec, 1,
                                       &fromvec, 1, 0);
                if(res == -1) {
                        ITC_TRACE_ERROR("Copy from precess %d failed (%d:%s)",
                                        rxmsg->pid, errno, strerror(errno));
                        return -1;
                } else if (res != (rxmsg->size + ITC_MSG_ADM_OFFSET)) {
                        ITC_TRACE_ERROR("Not all of message copied from process got %d expected %d",
                                        res, (rxmsg->size + ITC_MSG_ADM_OFFSET));
                        return -1;
                }

                itc_send(&msg, message->receiver, message->sender);

                if(send_free_req(rxmsg->mbox_id, rxmsg->addr) < 0) {
                        return -1;
                }
        } else if(rxmsg->msgno == FREE_MSG) {
                message = rxmsg->addr;
                msg = MESSAGE_TO_MSG(message);
                itc_free(&msg);
        } else {
                return -1;
        }

        return 0;
}

/* ===================================================================== */
/**
 *   sock_rx_thread
 *
 *   @param data       Thread arguments, not used.
 *
 *   @return           Will never return.
 *
 *   @par Globals:     --
 *
 *   This is the thread which receives all messages sent from another
 *   process.
 */
/* ===================================================================== */
static void *sock_rx_thread(void *data)
{
        itc_mbox_id_t my_world_id = (itc_mbox_id_t)(unsigned long)data;
        int res, rx_len;
        char *rx_buffer;

        memset(&sock_inst.my_rx_addr, 0, sizeof(struct sockaddr_un));
        sock_inst.my_rx_addr.sun_family = AF_LOCAL;
        sprintf(sock_inst.my_rx_addr.sun_path,
                "itc_rx_sock_0x%08x", my_world_id);

        sock_inst.my_rx_sd = socket(AF_LOCAL, SOCK_DGRAM, 0);

        sock_inst.mbox_id = itc_create_mailbox(sock_inst.my_rx_addr.sun_path, 0);

        res = bind(sock_inst.my_rx_sd, (struct sockaddr *) &sock_inst.my_rx_addr,
                   sizeof(struct sockaddr_un));
        if(res < 0) {
                ITC_TRACE_ERROR("bind error: %d (%d:%s)",
                                res, errno, strerror(errno));
                ITC_ERROR_VERBOSE("sock_rx_thread", "Bind failed in socket RX thread",
                                  ITC_ERROR_FATAL, __FILE__, __LINE__);
        }

        rx_buffer = malloc(65536);
        if(rx_buffer == NULL) {
                ITC_ERROR_VERBOSE("sock_rx_thread", "Out of memory error",
                                  ITC_ERROR_FATAL, __FILE__, __LINE__);
        }

        for(;;) {
                rx_len = recv(sock_inst.my_rx_sd, rx_buffer, RXLEN, 0);
                if(rx_len < 0) {
                        ITC_TRACE_ERROR("RX socket receive failed, res: %d", rx_len);
                        ITC_ERROR_VERBOSE("sock_rx_thread", "RX socket receive failed",
                                          ITC_ERROR_FATAL, __FILE__, __LINE__);
                }
                res = deliver_packet(rx_buffer, rx_len);
                if(res != 0) {
                        ITC_TRACE_ERROR("RX deliver_packet failed, res; %d", res);
                        ITC_ERROR_VERBOSE("sock_rx_thread", "RX deliver_packet failed",
                                          ITC_ERROR_FATAL, __FILE__, __LINE__);
                }
        }

        /* Will never get here */
        return NULL;
}
#endif

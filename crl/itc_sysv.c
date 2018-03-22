/**
 *   ITC SYSV message transport implementation.
 *
 *   @file itc_sysv.c
 *
 *   ITC implementation for using SYSV message queues as transport
 *   between processes.
 *
 *   Copyright (C) 2013-2014  by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-02-20 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Updated error handling for errors from msgrcv.
 *
 *   Revised : 2014-02-21 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support in ITC to work after a fork.
 *             Removed init of itci_create_ep to NULL.
 *             Added support for determining max message size.
 *             Improved error messages in ITC_ERROR.
 *             Added error codes for itc_init.
 *             Removed unused variable world_transport in init_alloc.
 *
 *   Revised : 2014-09-29 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected msgsnd handling of EINTR and removed the
 *             ITC_CROSSMEMORY implementation.
 *
 *   Revised : 2014-11-04 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected receive buffer to adapt to the largets message
 *             size supported by the SYSV message queue implementation.
 *
 *   Revised : 2015-02-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Set thread name for the itc_rx_sysv thread.
 *
 *   Revised : 2015-03-30 Henrik Wallin henrik.b.wallin@ericsson.com
 *   Change  : Make sure the space for msgrcv is handled correctly.
 *             Sending side adjusts for the extra sizeof(long), but the
 *             receive side was missing the special treatment. This is
 *             needed as the mtype "field" is used to pass data.
 *
 *   Revised : 2015-09-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed the internal itc_threads to have scheduling policy
 *             and priority set by environment variables CRL_SCHED_POLICY
 *             and CRL_SCHED_PRIO. If no environment variables are set the
 *             threads will defaul to sheeduling polisy SCHED_OTHER
 *             priority 0.
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

#include <sys/uio.h>
#include <sys/prctl.h>

#include <errno.h>

#include <sys/types.h>
#include <sys/msg.h>
#include <sys/ipc.h>

#include <search.h>

#include "itc.h"
#include "itc_impl.h"
#include "itci_internal.h"

#ifdef ITC_LTTNG
/*
 * The header containing our TRACEPOINT_EVENTs.
 */
#define TRACEPOINT_DEFINE
#include "itc_sysv_lttng.h"
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define MIN(a, b) a < b ? a : b

#define ITC_SYSV_BASE_MSG_NO (ITC_MSG_BASE + 0x200)

#ifdef ITC_LTTNG

#define ITC_SYSV_TRACE(trc, ...)                                                          \
              tracepoint(com_ericsson_itc_sysv, trc, __VA_ARGS__);                  \

#else /* ITC_LTTNG */

#define ITC_SYSV_TRACE(trc, ...)

#endif /* ITC_LTTNG */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
#define TX_MSG   (ITC_SYSV_BASE_MSG_NO + 1)
#define FREE_MSG (ITC_SYSV_BASE_MSG_NO + 2)
struct sysv_msg {
        long          msgno;
        pid_t         pid;
        itc_mbox_id_t mbox_id;
        int           size;
        void         *addr;
};

#define SYSV_MSGTYPE ((void *)ITC_SYSV_BASE_MSG_NO + 3)

struct sysv_target {
        int mbox_id;
        int mqid;
};

struct sysv_instance {
        itc_mbox_id_t      mbox_id;
        int                mqid;

        pid_t              pid;

        pthread_mutex_t    thread_lock_m;

        itc_mbox_id_t      world_mask;
        itc_mbox_id_t      world_shift;
        itc_mbox_id_t      my_world_id;

        pthread_key_t      destruct_key;
        int                killed;

        int                max_msgsize;

        char              *rundir_path;
        char              *msgq_file;
        char              *rx_buffer;

        struct sysv_target st[SUPPORTED_PROCESSES];
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static struct sysv_instance si;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static void *sysv_rx_thread(void *data);

static int sysv_init(itc_mbox_id_t  my_world_id,
                     itc_mbox_id_t  world_mask,
                     int            mbox_count,
                     uint32_t       flags);

static int sysv_exit(void);

static int sysv_send(struct itc_mailbox *mbox, struct itc_message *message,
                     itc_mbox_id_t to, itc_mbox_id_t from);

static int sysv_get_maxmsgsize(void);

struct itci_transport_funcs sysv_itc_funcs = { NULL,
                                               sysv_init,
                                               sysv_exit,
                                               NULL,
                                               NULL,
                                               sysv_send,
                                               NULL,
                                               NULL,
                                               sysv_get_maxmsgsize
};

/* ===================================================================== */
/**
 *   find_target
 *
 *   @param mbox_id    Mailbox id of target.
 *
 *   @return           Pointer to struct sysv_target.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Find target structure. If incorrect mailbox ID enterred fatal
 *   error triggered.
 */
/* ===================================================================== */
static struct sysv_target *find_target(itc_mbox_id_t mbox_id)
{
        itc_mbox_id_t pi;

        pi = (mbox_id & si.world_mask) >> si.world_shift;
        if(pi == 0 ||
           pi >= SUPPORTED_PROCESSES) {
		return NULL;
        }

        return &(si.st[pi]);
}

/* ===================================================================== */
/**
 *   get_msgq
 *
 *   @param mbox_id    Mailbox id to add.
 *
 *   @return           Added mqid or -1 if fatal error and -2 if msgq
 *                     has been removed due to target demise and message
 *                     shall be dropped.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Get a new sysv message queue identifier.
 */
/* ===================================================================== */
static int get_msgq(itc_mbox_id_t mbox_id)
{
        itc_mbox_id_t new_id;
        key_t key;
        int mqid;

        new_id = mbox_id & si.world_mask;

        key = ftok(si.msgq_file, (new_id >> 20));
        if(key == -1) {
		ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
				  ITC_ERROR_FATAL,
				  "Failed to generate key: %d(%s)",
				  errno, strerror(errno));
		/* Will not return here! */
        }

        mqid = msgget(key, 0);
        if(mqid == -1) {
                if(errno == ENOENT) {
                        return -1;
                } else {
			ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
					  ITC_ERROR_FATAL,
					  "Failed to get message queue for TX to 0x%08x, error: %d(%s)",
					  mbox_id, errno, strerror(errno));
			/* Will not return here! */
                }
        }

        return mqid;
}

/* ===================================================================== */
/**
 *   add_sysv_target
 *
 *   @param mbox_id    Mailbox id to add.
 *
 *   @return           Pointer to added sysv target or NULL at failure.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Add a new socket target.
 */
/* ===================================================================== */
static void add_sysv_target(struct sysv_target *st,
			    itc_mbox_id_t mbox_id)
{
        int mqid;

        mqid = get_msgq(mbox_id);
        if(mqid != -1) {
		st->mbox_id = (mbox_id & si.world_mask);
		st->mqid = mqid;
	}
}

/* ===================================================================== */
/**
 *   rm_sysv_target
 *
 *   @param mbox_id    Mailbox id to remove.
 *
 *   @return           -
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Remove a socket target.
 */
/* ===================================================================== */
static void rm_sysv_target(itc_mbox_id_t mbox_id)
{
        struct sysv_target *st;

        st = find_target(mbox_id);
        st->mbox_id = 0;
        st->mqid    = 0;
}

/* ===================================================================== */
/**
 *   get_sysv_target
 *
 *   @param mbox_id    Mailbox id to get.
 *
 *   @return           Pointer to sysv target or NULL if it does not
 *                     exist.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Get a sysv target struct.
 */
/* ===================================================================== */
static struct sysv_target *get_sysv_target(itc_mbox_id_t mbox_id)
{
        struct sysv_target *st;

        st = find_target(mbox_id);
	if(st == NULL) {
		return NULL;
	}

        if(st->mbox_id == 0) {
                add_sysv_target(st, mbox_id);
        }

        return st;
}

/* ===================================================================== */
/**
 *   sysv_thread_death
 *
 *   @param data       Unused.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function added as destructor to the sysv_rx_thread to be run when
 *   it terminates to remove the sysv message queue.
 */
/* ===================================================================== */
static void sysv_thread_death(void *data)
{
        (void)data;

        if(si.mqid != -1) {
                si.killed = 1;
                if(msgctl(si.mqid, IPC_RMID, NULL) == -1) {
                        ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                          ITC_ERROR_FATAL,
                                          "Failed to get msg q info: %d(%s)",
                                          errno, strerror(errno));
                }
                si.mqid = -1;
        }

	free(si.rx_buffer);
}

/* ===================================================================== */
/**
 *   free_sysv_resources
 *
 *   @return           -
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Function that frees all of the ITC sysv transports resources.
 *   Used after fork.
 */
/* ===================================================================== */
static int free_sysv_resources(void)
{
        free(si.msgq_file);

        /* Delete destructor key */
        if(pthread_key_delete(si.destruct_key) != 0) {
                ITC_TRACE_ERROR("Pthread key delete error %d(%s)",
                                errno, strerror(errno));
                return -1;
        }

        memset(&si, 0, sizeof(struct sysv_instance));

        return 0;
}

/* ===================================================================== */
/**
 *   sysv_init
 *
 *   @param my_world_id  My world id, from world.
 *
 *   @param world_mask   World mask.
 *
 *   @param mbox_count   How many local mailboxes shall be supported.
 *
 *   @return           0 at success and -1 at failure.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Init function of the ITC by sysv functionality, called by itc_init.
 */
/* ===================================================================== */
static int sysv_init(itc_mbox_id_t  my_world_id,
                     itc_mbox_id_t  world_mask,
                     int            mbox_count,
                     uint32_t       flags)
{
        int tmp, tmpmask;

        if(si.rundir_path != NULL) {
                if(flags & ITC_INIT_FORCE_REDO) {
                        if(free_sysv_resources() != 0) {
                                return ITC_EINTERNAL_ERROR;
                        }
                        sysv_get_maxmsgsize();
                } else {
                        return ITC_EALREADY_INITIALISED;
                }
        }

        si.rundir_path = itc_get_rundir();
        if(si.rundir_path == NULL) {
                ITC_TRACE_ERROR("Failed to get rundir path", 0);
                return -1;
        }

        si.msgq_file = malloc(strlen(si.rundir_path) + strlen(ITC_MSGQ_FILE) + 1);
        if(si.msgq_file == NULL) {
                ITC_TRACE_ERROR("Out of memory", 0);
                return ITC_EINTERNAL_ERROR;
        }

        strcpy(si.msgq_file, si.rundir_path);
        strcat(si.msgq_file, ITC_MSGQ_FILE);

        tmp = 0;
        tmpmask = world_mask;
        while(!(tmpmask & 1)) {
                tmpmask = tmpmask >> 1;
                tmp++;
        }

        si.mqid        = -1;
        si.pid         = getpid();
        si.world_mask  = world_mask;
        si.world_shift = tmp;
        si.my_world_id = my_world_id;

        /* Initialise destructor key */
        if(pthread_key_create(&si.destruct_key, sysv_thread_death) != 0) {
                ITC_TRACE_ERROR("Pthread key create error: %d(%s)",
                                errno, strerror(errno));
                return ITC_EINTERNAL_ERROR;
        }

        if(pthread_mutex_init(&si.thread_lock_m, NULL) != 0) {
                ITC_TRACE_ERROR("Pthread mutex init error %d(%s)",
                                errno, strerror(errno));
                return ITC_EINTERNAL_ERROR;
        }


        if(add_itcthread(sysv_rx_thread, NULL,
			 true, &si.thread_lock_m) < 0) {
                return ITC_EINTERNAL_ERROR;
        }

        return 0;
}

/* ===================================================================== */
/**
 *   sysv_exit
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           0 at successand -1 at failure.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Exit function of the ITC by sysv functionality, called by itc_exit.
 */
/* ===================================================================== */
static int sysv_exit(void)
{
        free(si.msgq_file);

        if(pthread_mutex_destroy(&si.thread_lock_m) != 0) {
                ITC_TRACE_ERROR("Pthread mutex destroy error %d(%s)",
                                errno, strerror(errno));
                return -1;
        }

        /* Delete destructor key */
        if(pthread_key_delete(si.destruct_key) != 0) {
                ITC_TRACE_ERROR("Pthread key delete error %d(%s)",
                                errno, strerror(errno));
                return -1;
        }

        memset(&si, 0, sizeof(struct sysv_instance));

        return 0;
}

/* ===================================================================== */
/**
 *   sysv_send
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @param message    Pointer to message to be sent.
 *
 *   @param to         Mailbox id of message receipient.
 *
 *   @param from       Mailbox id of message sender.
 *
 *   @return           0 at success and -1 at failure.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Send message function of the ITC by sysv functionality, called
 *   by itc_send.
 */
/* ===================================================================== */
static int sysv_send(struct itc_mailbox *mbox, struct itc_message *message,
                     itc_mbox_id_t to, itc_mbox_id_t from)
{
        union itc_msg *msg;
        struct sysv_target *st;
        int size;

        st = get_sysv_target(to);
        if(st == NULL) {
                return 1;
        } else if(st->mbox_id == 0) {
		/* Target mailbox could not be added, message dropped. */
		return 0;
	}

        message->next = SYSV_MSGTYPE;
        message->prev = NULL;
        size = message->size + ITC_MSG_ADM_OFFSET;
        *((uint32_t *)message) = TX_MSG;
        while(msgsnd(st->mqid, message, (size - sizeof(long)), MSG_NOERROR) == -1) {
		if(errno == EINTR) {
			continue;
		} else if(errno == EINVAL ||
			  errno == EIDRM) {
			rm_sysv_target(to);
			add_sysv_target(st, to);
			if(st->mbox_id == 0)
				return 0;
		} else {
                        ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                          ITC_ERROR_FATAL,
                                          "msgsnd failed on q %d, error %d(%s)",
                                          st->mqid, errno, strerror(errno));
			/* Will not return here! */
		}
        }

        msg = MESSAGE_TO_MSG(message);
        itc_free(&msg);

        return 0;
}
/* ===================================================================== */
/**
 *   sysv_get_maxmsgsize
 *
 *   @return           Max message size supported by the SYSV
 *                     message queues.
 *
 *   @par Globals:     si
 *                     ITC sysv instance global structure.
 *
 *   Return the largest message size supported by the SYSV
 *   message queues.
 */
/* ===================================================================== */
static int sysv_get_maxmsgsize(void)
{
        struct msginfo info;

        if(si.max_msgsize != 0) {
                return si.max_msgsize;
        }

        if(msgctl(0, IPC_INFO, (struct msqid_ds *)&info) == -1) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "msgctl failed IPC_INFO: %d(%s)",
                                  errno, strerror(errno));
        }

        si.max_msgsize = MIN(info.msgmax, info.msgmnb);

        return si.max_msgsize;
}


/* ===================================================================== */
/**
 *   deliver_packet
 *
 *   @param buff       Pointer to packet buffer.
 *
 *   @param len        Packet length.
 *
 *   @return           0 at success and -1 at failure.
 *
 *   @par Globals:     --
 *
 *   Deliver receive packet to recipients mailbox.
 */
/* ===================================================================== */
static int deliver_packet(char *buff, int len, int mqid)
{
        struct itc_message *message, *rxmsg;
        union itc_msg *msg;
        uint16_t flags, buffsize;

        rxmsg = (struct itc_message *)buff;

#if 0
        ITC_SYSV_TRACE(itc_sysv_deliver_packet,
                       mqid, si.my_world_id, rxmsg->pid,
                       rxmsg->mbox_id, rxmsg->size,
                       (long)rxmsg->addr);
#endif

        msg = itc_alloc(rxmsg->size, 0);
        message = MSG_TO_MESSAGE(msg);

        flags    = message->flags;
        buffsize = message->buffsize;

        memcpy(message, rxmsg, (rxmsg->size + ITC_MSG_ADM_OFFSET));

        message->flags    = flags;
        message->buffsize = buffsize;

        itc_send(&msg, message->receiver, message->sender);

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
static void *sysv_rx_thread(void *data)
{
        key_t key;
        char name[30];
        int rx_len, res, reperr = 0;
        struct msqid_ds msqinfo;

	if(prctl(PR_SET_NAME, "itc_rx_sysv", 0, 0, 0) == -1) {
                ITC_TRACE_ERROR("prctl failed, error %d(%s)",
                                errno, strerror(errno));
                return (void *) -1;
	}

        sprintf(name, "itc_rx_sysv_0x%08x", si.my_world_id);
        si.mbox_id = itc_create_mailbox(name, ITC_MBOX_NO_NS);

        /* Add destructor to key */
        if(pthread_setspecific(si.destruct_key, (void *)(unsigned long)si.mbox_id) != 0) {
                ITC_TRACE_ERROR("Pthread set destructor error %d(%s)",
                                errno, strerror(errno));
                return (void *) -1;
        }

        key = ftok(si.msgq_file, (si.my_world_id >> 20));
        if(key == -1) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "Failed to create key: %d (%s)",
                                  errno, strerror(errno));
        }

        si.mqid = msgget(key, IPC_CREAT | 0666);
        if(si.mqid == -1) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "Failed to open msg q error: %d (%s)",
                                  errno, strerror(errno));
        }

        if(msgctl(si.mqid, IPC_STAT, &msqinfo) == -1) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "Failed to get msg q info: %d (%s)",
                                  errno, strerror(errno));
        }

        if(msqinfo.msg_qnum != 0) {
                /* MSG queue should be empty, if not remove and reopen */
                if(msgctl(si.mqid, IPC_RMID, &msqinfo) == -1) {
                        ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                          ITC_ERROR_FATAL,
                                          "Failed to get msg q info: %d (%s)",
                                          errno, strerror(errno));
                }

                si.mqid = msgget(key, IPC_CREAT | 0666);
                if(si.mqid == -1) {
                        ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                          ITC_ERROR_FATAL,
                                          "Failed to open msg q error: %d (%s)",
                                          errno, strerror(errno));
                }
        }

	/* To ensure that max_msgsize is set in si run sysv_get_maxmsgsize here! */
	(void)sysv_get_maxmsgsize();
        si.rx_buffer = malloc(si.max_msgsize);
        if(si.rx_buffer == NULL) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "Out of memory error",
                                  0);
        }
        memset(si.rx_buffer, 0, si.max_msgsize);

        MUTEX_UNLOCK(&si.thread_lock_m);

        for(;;) {
                rx_len = msgrcv(si.mqid, si.rx_buffer, si.max_msgsize - sizeof(long), 0, 0);
                if(si.killed) {
                        break;
                }
                if(rx_len < 0) {
                        if((errno == EIDRM ||
                            errno == EINVAL) &&
                           !reperr) {
                                /* The message queue was removed, we might be exiting
                                   so for now wait 10ms and then retry. If the problem
                                   still exists call error. */
                                usleep(10000);
                                reperr = 1;
                                continue;
                        } else if(errno == EINTR) {
                                /* Recevive interrupted by a signal.
                                   Reenter msgrcv. */
                                continue;
                        }
                        ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                          ITC_ERROR_FATAL,
                                          "RX msgrcv receive failed(q:%d), res: %d error: %d (%s)",
                                          si.mqid, rx_len, errno, strerror(errno));
                }
                res = deliver_packet(si.rx_buffer, rx_len + sizeof(long), si.mqid);
                if(res < 0) {
                        ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                          ITC_ERROR_FATAL,
                                          "RX deliver_packet failed",
                                          0);
                }
                reperr = 0;
        }

        return NULL;
}

/**
 *   Internal ITC main include file.
 *
 *   @file itc_impl.h
 *
 *   ITC header file used for declarations and definitions that is
 *   used globally in the ITC implementation.
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
 *   Revised : 2013-02-04 Magnus Lindberg
 *   Change  : First version.
 *
 *   Revised : 2014-01-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for multiple itcworld instances.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Updated to support file and line correctly.
 *             Added support in ITC to work after a fork.
 *             Major overhaul of how monitors are implemented.
 *             Improved error messages in ITC_ERROR.
 *             Added support for determining max message size.
 *
 *   Revised : 2014-09-29 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected SUPPORTED_PROCESSES definition.
 *
 *   Revised : 2014-09-29 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed ITC_MAX_MSGSIZE to (1024 * 1024) bytes.
 *             Will be decreased by transports.
 *
 *   Revised : 2015-02-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added thread id (tid) in itc_mailbox struct.
 *
 *   Revised : 2015-09-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed the internal itc_threads to have scheduling policy
 *             and priority set by environment variables CRL_SCHED_POLICY
 *             and CRL_SCHED_PRIO. If no environment variables are set the
 *             threads will defaul to sheeduling polisy SCHED_OTHER
 *             priority 0.
 *
 *   Revised : 2015-11-04 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed default high scheduling priority to 40.
 *
 *   Revised : 2016-01-29 Hans Beckerus hans.xx.beckerus@ericsson.com
 *   Change  : Seize monitor resources in segments
 *
 * ========================================================================
 */

#ifndef __ITCIMPL_H
#define __ITCIMPL_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>
#include <stdbool.h>

#include "pthread.h"

#include "itc.h"
#include "itc_system.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define ITC_MAX_MSGSIZE (1024 * 1024)
#define ITC_MSG_ADM_OFFSET 32

/* Flags that ITC can add to the init call towards the
   itc transport functions. */
#define ITC_INIT_FLAG_I_AM_WORLD 0x00000001
#define ITC_INIT_FORCE_REDO      0x00000100

#define ITC_WORLD_NAME "itc_world"

#define ITC_WORLD_LOC_NAME "itc_world_locator"

#define ITC_MSGQ_FILE     "itcmsgq"
#define ITC_RUN_DIR       "/var/run/"
#define ITC_INSTANCE_DEFAULT   "itcworld/"
#define ITC_INSTANCE_PREFIX    "itc_"

/* itc_create_mailbox flags are not allowed to clash with
   itc_create_mailbox flags in itc.h */
#define ITC_CREATE_CLONE 0x00001000

#define ITC_MON_EXT_FLAG 1

#define ITC_USED_MBOXES 2

#define SUPPORTED_PROCESSES 255

#define WORLD_MASK 0xFFF00000

#define ITC_MSG_INRX 0x0001

#define ITC_HIGH_PRIORITY 40

#define MSG_TO_MESSAGE(msg) ((struct itc_message *)((unsigned long)msg - \
                                                    ITC_MSG_ADM_OFFSET))
#define MESSAGE_TO_MSG(message) ((union itc_msg *)(&message->msgno))

#define MBOX_ALIGNMENT 16
#define CVT_MBOX_ID(mid) (mid == ITC_MY_MBOX ? mymailbox->mbox_id : mid)
#define GET_MBOX(mbox_id) ((struct itc_mailbox *)                       \
                           ((char *)itc_inst.mboxes +                   \
                            ((mbox_id & itc_inst.mbox_id_mask) *        \
                             itc_inst.mbox_offset)))

#define __ITC_ALIGN_MASK(x,mask)  (((x)+(mask))&~(mask))
#define ITC_ALIGN(x,a)            __ITC_ALIGN_MASK(x,(typeof(x))(a)-1)
#define ITC_ALIGN_PTR(ptr, align) (typeof(ptr))ITC_ALIGN((uintptr_t)(ptr), \
                                                         align)

#define FFS(val) __builtin_clz(val)

#define ITC_ERROR(flags, msg, ...)                     \
        itc_error(__func__, file, line, flags, msg, __VA_ARGS__)

#define ITC_ERROR_VERBOSE(call, file, line, flags, msg, ...)     \
        itc_error(call, file, line, flags, msg, __VA_ARGS__)


#define MUTEX_LOCK(lock)                                                \
        do {                                                            \
                int mutex_res;                                          \
                mutex_res = pthread_mutex_lock(lock);                   \
                if(mutex_res != 0) {                                    \
                        itc_error(__func__, __FILE__, __LINE__,         \
                                  ITC_ERROR_FATAL,                      \
                                  "mutex lock error: %d(%s)",           \
                                  mutex_res, strerror(mutex_res));      \
                }                                                       \
        } while(0)

#define MUTEX_UNLOCK(lock)                                              \
        do {                                                            \
                int mutex_res;                                          \
                mutex_res = pthread_mutex_unlock(lock);                 \
                if(mutex_res != 0) {                                    \
                        itc_error(__func__, __FILE__, __LINE__,         \
                                  ITC_ERROR_FATAL,                      \
                                  "mutex unlock error: %d(%s)",         \
                                  mutex_res, strerror(mutex_res));      \
                }                                                       \
        } while(0)


#ifdef ITC_STATS
#define INC_STAT(mbox, element) if(mbox != NULL) \
                                   atomic_inc(&mbox->stats.element)
#define DEC_STAT(mbox, element) if(mbox != NULL) \
                                   atomic_dec(&mbox->stats.element)
#else
#define INC_STAT(mbox, element)
#define DEC_STAT(mbox, element)
#endif

#define MONITOR_SEG_SIZE 64
#define MONITOR_SEG_MAX  1024
#define MONITOR_MAX      (MONITOR_SEG_SIZE * MONITOR_SEG_MAX)
#define MONITOR_EXT_FLAG 0x80000000
#define MONITOR_ID_MASK  0x0000FFFF
#define MONITOR_AGE_MASK 0x00FF0000
#define MONITOR_MAGIC    0x3A000000
#define MONITOR_REC_FROM_ID(id) (id & MONITOR_ID_MASK)
#define MONITOR_SET_ID(id) (MONITOR_MAGIC | (id & MONITOR_ID_MASK))
#define MONITOR_INCR_AGE(id)                                    \
        do {                                                    \
                uint32_t age;                                   \
                age = (id & MONITOR_AGE_MASK) >> 16;            \
                age++;                                          \
                id = (MONITOR_MAGIC                    |        \
                      ((age << 16) & MONITOR_AGE_MASK) |        \
                      (id & MONITOR_ID_MASK));                  \
         } while(0)
#define EXT_MONITOR_INCR_AGE(id)                                \
        do {                                                    \
                uint32_t age;                                   \
                age = (id & MONITOR_AGE_MASK) >> 16;            \
                age++;                                          \
                id = (MONITOR_EXT_FLAG                 |        \
                      MONITOR_MAGIC                    |        \
                      ((age << 16) & MONITOR_AGE_MASK) |        \
                      (id & MONITOR_ID_MASK));                  \
         } while(0)

#ifdef ITC_STAT_MBOXID
#define MAX_STAT_ID 64
#endif

#ifdef ITC_LTTNG

void itc_trace_error(char *file, int line, const char *format, ...);

#define ITC_TRACE(trc, ...)                                                          \
        do {                                                                         \
           if(mymailbox != NULL)                                                     \
              tracepoint(com_ericsson_itc_if, trc, mymailbox->mbox_id, __VA_ARGS__); \
           else                                                                      \
              tracepoint(com_ericsson_itc_if, trc, 0, __VA_ARGS__);                  \
        } while (0)


#define ITC_COORD_TRACE(...) tracepoint(com_ericsson_itc_if, __VA_ARGS__)
#define ITC_TRACE_ERROR(txt, ...) itc_trace_error(__FILE__, __LINE__, txt, __VA_ARGS__)

#else /* ITC_LTTNG */

#define ITC_TRACE(...)
#define ITC_COORD_TRACE(...)
#define ITC_TRACE_ERROR(...)

#endif /* ITC_LTTNG */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef enum {
        ITC_TRANSPORT_LOCAL = 0,
        ITC_TRANSPORT_SOCK,
        ITC_TRANSPORT_SYSV,
        ITC_TRANSPORT_LINX,
        ITC_NUM_TRANSPORTS
} itc_transports;

typedef enum {
        MONITOR_UNUSED,
        MONITOR_INUSE,
        MONITOR_DELETED,
} monitor_state;


struct mbox_cnt_pair {
        volatile struct itc_mailbox *ptr;
        volatile unsigned long int   aba;
};

struct mbox_lfq {
        struct mbox_cnt_pair enqueue;
        struct mbox_cnt_pair dequeue;

        unsigned long int aba;
};

struct llqueue {
        struct itc_message *head;
        struct itc_message *tail;

        struct itc_message *search;
};

struct mboxqueue {
        struct itc_mailbox *head;
        struct itc_mailbox *tail;

        struct itc_mailbox *search;
};

struct monitor_record {
        struct monitor_record      *next;
        struct monitor_record      *prev;
        itc_monitor_id_t            mon_id;
        uint32_t                    flags;

        pthread_mutex_t             lock_m;

        itc_mbox_id_t               target_mbox_id;
        struct itc_mailbox         *target_mbox;
        struct monitor_mbox_record *target_entry;

        struct itc_mailbox         *monitor_mbox;
        struct monitor_mbox_record *monitor_entry;

        union itc_msg              *msg;

        monitor_state               state;
};

struct monitor_mbox_record {
        struct monitor_mbox_record *next;
        struct monitor_mbox_record *prev;
        uint32_t                    flags;

        struct monitor_record      *rec;
};

struct errh_record {
        struct errh_record         *next;
        struct errh_record         *prev;
        uint32_t                    flags;

        itc_errorhandler            errh;
};

struct itc_threads {
        struct itc_threads *next;

        void               *(*worker)(void *);
        void               *arg;
        pthread_t           tid;

	bool                highprio;
        pthread_mutex_t    *start_m;
};

struct itc_message {
#ifdef __LP64__
        struct itc_message *next;       /* Pointer to next element in whichever
                                           queue this message currently belongs to */
        struct itc_message *prev;       /* Pointer to next element in whichever
                                           queue this message currently belongs to */
#else
        struct itc_message *next;       /* Pointer to next element in whichever
                                           queue this message currently belongs to */
        struct itc_message *prev;       /* Pointer to next element in whichever
                                           queue this message currently belongs to */
        uint32_t            reserved[2];
#endif

        uint16_t            flags;
        uint16_t            buffsize;

        /* DO NOT change anything after this point - will break the
           ITC system                                                            */
        itc_mbox_id_t       receiver;   /* receiver mailbox                      */
        itc_mbox_id_t       sender;     /* sending mailbox                       */
        int32_t             size;       /* size of message                       */

        /* this is start of message as the user sees it                          */
        uint32_t            msgno;      /* message number                        */
        char                data[1];    /* actual data - where user space starts */
};

struct mbox_rxdata {
        pthread_mutexattr_t         rxq_attr;              /* Attributes for RXQ mutex     */
        pthread_mutex_t             rxq_m;                 /* protect the queue            */
        pthread_cond_t              rxq_c;                 /* wait on queue                */

        int                         rxfd;                  /* FDs used for enabling select */
        int                         has_fd;                /* True is FDs are created      */
        long                        rx_qlen;
        uint32_t                    in_rx;
        uint32_t                   *filter;
};

struct itc_mailbox {
        struct itc_mailbox         *next;
        unsigned long int           aba;                  /* Used for the lock free mbox q   */

        uint32_t                    flags;                /* Flags from itc_create_mailbox   */

        struct mbox_rxdata          rxq_data;             /* Data about the RX queue */
        struct mbox_rxdata         *rxq;                  /* Pointer to data about the RX queue */

        struct itc_mailbox         *parent_mbox;          /* Parent mailbox for clones else NULL */

        struct itc_message         *pending_locate;
        pthread_mutex_t             locate_lock;

        pthread_mutex_t             monitors_m;            /* protect the queue               */
        struct monitor_mbox_record *monitors;              /* linkage of mboxes monitoring me */

        pthread_mutex_t             mymonitors_m;          /* protect the queue               */
        struct monitor_mbox_record *mymonitors;            /* linkage of mboxes I am monitoring */

#ifdef ITC_STATS
        struct itc_stats            stats;
#endif

        uint32_t                    mbox_id;               /* mailbox id                      */
        uint32_t                    aging;

        mbox_state                  state;
	pid_t                       tid;
        char                        name[ITC_NAME_MAXLEN]; /* name of mailbox                 */
};

struct itc_instance {
        struct itcq           *freemboxes;

        itc_mbox_id_t          my_world_id;
        itc_mbox_id_t          world_mask;
        itc_mbox_id_t          world_mbox_id;
        itc_transports         world_transport;

        itc_mbox_id_t          local_coord_mbox_id;
        pthread_mutex_t        local_coord_sync;
        pthread_t              local_coord_tid;

        pthread_mutex_t        local_locate_tree_m;
        void                  *local_locate_tree;

        pthread_mutex_t        free_rec_m;
        struct monitor_record *free_rec;
        void                  *monitor_free;
        struct monitor_record *monitor_list;
        int                    monitor_size;

        pthread_mutex_t        list_m;
        struct errh_record    *errh_list;
        struct itc_threads    *thread_list;

        pthread_key_t          destruct_key;

        itc_hook              *txhook;
        void                  *txuser;
        itc_hook              *rxhook;
        void                  *rxuser;

        pid_t                  pid;

        int32_t                mailbox_count;
        uint32_t               mbox_id_mask;
        int                    age_shift;
        int                    mbox_offset;
        uint8_t               *mboxes_free;
        struct itc_mailbox    *mboxes;

        char                  *name_space;

#ifdef ITC_STAT_MBOXID
        struct itc_mailbox    *statmboxarray[MAX_STAT_ID];
#endif
        char                   inst_path[ITC_NAME_MAXLEN]; /* name of ITC instance         */
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
int add_itcthread(void *(*worker)(void *), void *arg,
		  bool highprio, pthread_mutex_t *start_m);

void itc_error(const char *itc_call,
               const char *file,
               int         line,
               uint32_t    flags,
               const char *format,
               ...);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ITCIMPL_H */

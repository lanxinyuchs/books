/**
 *   Main file of the ITC implementation.
 *
 *   @file itc.c
 *
 *   See longer description in itc.h.
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
 *   Revised : [2013-01-14 Magnus Lindberg, magnus.k.lindberg@ericsson.com]
 *   Change  : First version.
 *
 *   Revised : 2014-01-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for multiple itcworld instances.
 *
 *   Revised : 2014-01-21 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected static id handling in itc_delete_mailbox and
 *             itc_get_name. Added close of the eventfd for a mailbox in
 *             itc_delete_mailbox
 *
 *   Revised : 2014-01-28 Magnus Lindberg
 *   Change  : Implemented itc_get_name between processes.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Updated to support file and line correctly.
 *             Added support in ITC to work after a fork.
 *             Major overhaul of how monitors are implemented.
 *             Removed calls to itci_create_ep.
 *             Collected queue handling into itc_q.c.
 *             Removed some warnings.
 *             Improved error messages in ITC_ERROR.
 *             Added support for determining max message size.
 *             Added error codes for itc_init.
 *
 *   Revised : 2014-09-29 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed send to disconnected trace from error to debug.
 *
 *   Revised : 2014-10-28 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added itc_events implementation. Corrected memory leak of
 *             monitor structures at create and delete of mailboxes.
 *
 *   Revised : 2014-11-07 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected a race condition in __itc_monitor.
 *
 *   Revised : 2014-11-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected error in __itc_monitor introduced in previous
 *             release.
 *
 *   Revised : 2014-11-27 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected error in mailbox id aging update and improved
 *             error output at send to mailbox id 0.
 *
 *   Revised : 2014-12-17 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed pthread condition variables to use CLOCK_MONOTONIC
 *             to avoid receives with timeout to get hanging when for
 *             instance ntp moves the clock.
 *
 *   Revised : 2015-02-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added thread id to itc_get_mailbox and friends.
 *             Removed calc_abs_time from receives with timout 0.
 *             Set thread name for the itc_local_coordinator.
 *
 *   Revised : 2015-03-31 Henrik Wallin henrik.b.wallin@ericsson.com
 *   Change  : Fixed memory leak in trigger_monitor.
 *
 *   Revised : 2015-08-17 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Fixed locking when get_mailbox_info is called from
 *             the itc_local_coordinator thread.
 *
 *   Revised : 2015-09-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed the internal itc_threads to have scheduling policy
 *             and priority set by environment variables CRL_SCHED_POLICY
 *             and CRL_SCHED_PRIO. If no environment variables are set the
 *             threads will defaul to sheeduling polisy SCHED_OTHER
 *             priority 0.
 *
 *   Revised : 2015-09-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected handling of the sysv transmit when the receiver
 *             has dissappeared.
 *
 *   Revised : 2015-10-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Correction to locking a mailbox rx queue when changing its
 *             state to deleted. This to avoid getting SEGV in itc_send.
 *
 *   Revised : 2015-11-04 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected setting of scheduling priority and added
 *             environment variable to be able to set the "high"
 *             scheduling priority. Corrected deletion of cloned mailboxes
 *             to not remove the mailbox fd. Allowed shared and cloned
 *             mailboxes to be linkhandlers.
 *
 *   Revised : 2015-11-10 Padmaiah U
 *   Change  : Modified __itc_clone_mailbox() such that a race condition
 *             between inserting cloned mailbox details to local locate tree
 *             and locating the cloned mailbox is corrected. This is to
 *             avoid SEGV in itc_monitor.
 *
 *   Revised : 2015-11-26 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected a hole in the implementation of the rxq locks
 *             for cloned mailboxes. This could lead to crashes in
 *             itc_send when a clone was deleted while someone was
 *             sending to it.
 *
 *   Revised : 2016-01-29 Hans Beckerus hans.xx.beckerus@ericsson.com
 *   Change  : Seize monitor resources in segments
 *
 *   Revised : 2016-02-05 Miroslav Krizanic
 *   Change  : Removed priority limitation and changed default priority
 *             for the real time scheduling policies.
 *
 *   Revised : 2016-09-23 Jaipaul Cheernam
 *   Change  : Updated to __itc_alloc to always allocate atleast 4 bytes to
 *             incorprate msgno
 *
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include <pthread.h>
/*#include <linux/time.h>*/

#include <errno.h>

#include <sys/eventfd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <search.h>

#include "itc.h"
#include "itc_system.h"

#include "itc_impl.h"
#include "itci_internal.h"
#include "itci_alloc.h"
#include "itc_messages.h"
#include "itc_list.h"
#include "itc_q.h"

#ifdef ITC_LTTNG
/*
 * The header containing our TRACEPOINT_EVENTs.
 */
#define TRACEPOINT_DEFINE
#include "itc_lttng.h"

#include "stdarg.h"
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
union itc_msg {
        uint32_t                       msgno;

        struct itc_add_rem_mailbox     itc_add_rem_mailbox;
        struct itc_locate              itc_locate;
        struct itc_locate_repl         itc_locate_repl;
        struct itc_locate_async        itc_locate_async;
        struct itc_locate_async_repl   itc_locate_async_repl;
        struct itc_monitor             itc_monitor;
        struct itc_unmonitor           itc_unmonitor;
        struct itc_trig_monitor        itc_trig_monitor;
        struct itc_assign_lnh          itc_assign_lnh;
        struct itc_get_all_mboxes      itc_get_all_mboxes;
        struct itc_get_mbox_info       itc_get_mbox_info;
        struct itc_get_mbox_info_repl  itc_get_mbox_info_repl;
        struct itc_get_alloc_info_repl itc_get_alloc_info_repl;
        struct itc_set_namespace       itc_set_namespace;
        struct itc_get_name            itc_get_name;
        struct itc_get_name_repl       itc_get_name_repl;
	struct itc_subscribe_events    itc_subscribe_events;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
/** @brief Global itc instance variable that holds the administrative data for
    ITC within a Linux process. */
struct itc_instance itc_inst;

static struct itci_transport_funcs transp_funcs[ITC_NUM_TRANSPORTS];
static struct itci_alloc_funcs alloc_funcs;

/** @brief mymailbox is a thread local variable that points to this threads
    mailbox. */
static __thread struct itc_mailbox *mymailbox = NULL;

/* External declarations of structs with function pointers.
   This will have to be cleaned up later */
extern struct itci_transport_funcs local_itc_funcs;
extern struct itci_transport_funcs sock_itc_funcs;
extern struct itci_transport_funcs sysv_itc_funcs;

extern struct itci_alloc_funcs malloc_funcs;
extern struct itci_alloc_funcs pool_funcs;
extern struct itci_alloc_funcs poolflex_funcs;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

#ifdef ITC_LTTNG
/* ===================================================================== */
/**
 *   itc_trace_error
 *
 *   @param file       File where error occured.
 *
 *   @param line       Line number in file.
 *
 *   @param format     Printf style message with additional parameters
 *                     for potetial dumped variables.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   ITC lttng trace error function.
 */
/* ===================================================================== */
void itc_trace_error(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        if(mymailbox != NULL) {
                tracepoint(com_ericsson_itc_if, itc_error, mymailbox->mbox_id, file, line, buffer);
        } else {
                tracepoint(com_ericsson_itc_if, itc_error, 0, file, line, buffer);
        }
}
#endif /* ITC_LTTNG */

/* ===================================================================== */
/**
 *   gettid
 *
 *   @return           Thread id of current thread.
 *
 *   @par Globals:     --
 *
 *   Get thread id of current thread.
 */
/* ===================================================================== */
static pid_t gettid(void)
{
    return (pid_t)syscall(SYS_gettid);
}

/* ===================================================================== */
/**
 *   atomic_inc
 *
 *   @return           New counter value.
 *
 *   @par Globals:     --
 *
 *   Atomic increase counter.
 */
/* ===================================================================== */
static inline long int atomic_inc(long int *cnt)
{
        long int res;

        res = __sync_add_and_fetch(cnt, 1);

        return res;
}

/* ===================================================================== */
/**
 *   atomic_dec
 *
 *   @return           New counter value.
 *
 *   @par Globals:     --
 *
 *   Atomic increase counter.
 */
/* ===================================================================== */
static inline void atomic_dec(long int *cnt)
{
        __sync_sub_and_fetch(cnt, 1);
}

/* ===================================================================== */
/**
 *   find_mbox
 *
 *   @param mbox_id    Mailbox id.
 *
 *   @return           Pointer to found mailbox entry or NULL if not a
 *                     local mailbox.
 *
 *   @par Globals:     --
 *
 *   Find local mailbox, if the mailbox is not local NULL will be
 *   returned.
 */
/* ===================================================================== */
struct itc_mailbox *find_mbox(itc_mbox_id_t mbox_id)
{
	if(mbox_id == ITC_MY_MBOX)
		return mymailbox;
#ifdef ITC_STAT_MBOXID
        if((mbox_id & itc_inst.world_mask) == 0) {
                if(mbox_id >= MAX_STAT_ID) {
                        return NULL;
                }
                return itc_inst.statmboxarray[mbox_id];
        }
#endif

        if((mbox_id & itc_inst.world_mask) == itc_inst.my_world_id) {
		uint32_t mbox_index = (uint32_t)(mbox_id & itc_inst.mbox_id_mask);
		if (mbox_index < (uint32_t)itc_inst.mailbox_count) {
					return GET_MBOX(mbox_id);
		}
        }

        return NULL;
}

/* ===================================================================== */
/**
 *   compare_mbox_name
 *
 *   @param pa         Pointer to mailbox name.
 *
 *   @param pb         Pointer to mailbox.
 *
 *   @return           String compare result from the mailbox names.
 *
 *   @par Globals:     --
 *
 *   Mailbox name compare function. It is used together with the binary
 *   tree function tfind.
 */
/* ===================================================================== */
static int compare_mbox_name(const void *pa, const void *pb)
{
        const char                *name = pa;
        const struct itc_mailbox  *mbox = pb;

        return strcmp(name, mbox->name);
}

/* ===================================================================== */
/**
 *   compare_mbox_name_add
 *
 *   @param pa         Pointer to mailbox A.
 *
 *   @param pb         Pointer to mailbox B.
 *
 *   @return           String compare result from the mailbox names.
 *
 *   @par Globals:     --
 *
 *   Mailbox name compare function. It is used together with the binary
 *   tree functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_mbox_name_add(const void *pa, const void *pb)
{
        const struct itc_mailbox  *mbox1 = pa;
        const struct itc_mailbox  *mbox2 = pb;

        return strcmp(mbox1->name, mbox2->name);
}

/* ===================================================================== */
/**
 *   do_nothing
 *
 *   @param tmp        Pointer to mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function used by tdestroy that does nothing on the entries.
 */
/* ===================================================================== */
static void do_nothing(void *tmp)
{
        (void)tmp;
}

/* ===================================================================== */
/**
 *   insert_locate
 *
 *   @param tree       Pointer to tree.
 *
 *   @param tree_m     Pointer to tree protection mutex.
 *
 *   @param mbox       Mailbox to add to tree.
 *
 *   @return           0 if mailbox entry added to tree.
 *
 *   @par Globals:     --
 *
 *   Used to add a mailbox to the local locate tree.
 */
/* ===================================================================== */
static int insert_locate(void **tree,
                         pthread_mutex_t *tree_m,
                         struct itc_mailbox *mbox)
{
        struct itc_mailbox **tmp;
        int searchresult = 0;


        MUTEX_LOCK(tree_m);

        /* Check if entry exists in tree it should not */
        tmp = tfind(mbox, tree, compare_mbox_name_add);
        if(tmp != NULL) {
                searchresult = -1;
        } else {
                tsearch(mbox, tree, compare_mbox_name_add);
        }

        MUTEX_UNLOCK(tree_m);

        return searchresult;
}

/* ===================================================================== */
/**
 *   remove_locate
 *
 *   @param tree       Pointer to tree.
 *
 *   @param tree_m     Pointer to tree protection mutex.
 *
 *   @param mbox       Mailbox to delete from tree.
 *
 *   @return           0 if mailbox entry deleted from tree.
 *
 *   @par Globals:     --
 *
 *   Used to delete a mailbox from the local locate tree.
 */
/* ===================================================================== */
static int remove_locate(void **tree,
                         pthread_mutex_t *tree_m,
                         struct itc_mailbox *mbox)
{
        struct itc_mailbox *tmp;
        int searchresult = 0;

        MUTEX_LOCK(tree_m);

        /* Check if entry exists in tree it should */
        tmp = tfind(mbox, tree, compare_mbox_name_add);
        if(tmp == NULL) {
                searchresult = -1;
        } else {
                tdelete(mbox, tree, compare_mbox_name_add);
        }

        MUTEX_UNLOCK(tree_m);

        return searchresult;
}

/* ===================================================================== */
/**
 *   get_locate
 *
 *   @param name       Mailbox name.
 *
 *   @return           Pointer to mailbox or NULL if no mailbox found.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   Get mailbox with the given name from local locate tree.
 */
/* ===================================================================== */
struct itc_mailbox *get_locate(const char *name)
{
        struct itc_mailbox **tmp;
        void *tree = itc_inst.local_locate_tree;

        MUTEX_LOCK(&itc_inst.local_locate_tree_m);

        /* Check if entry exists in tree it should not */
        tmp = tfind(name, &tree, compare_mbox_name);

        MUTEX_UNLOCK(&itc_inst.local_locate_tree_m);

        return (tmp == NULL) ? NULL : *tmp;
}

/* ===================================================================== */
/**
 *   conf_run_dir
 *
 *   @return           0 at success and -1 at failure.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   Function to get information about a mailbox and store in in an
 *   itc_mbox_info structure.
 */
/* ===================================================================== */
static int conf_run_dir(void)
{
        char *tmp;

        if(strcmp(itc_inst.inst_path, "") != 0) {
                return 0;
        }

        tmp = getenv(ITC_ENV_RUNDIR_PATH);
        if(tmp == NULL) {
                strcpy(itc_inst.inst_path, ITC_RUN_DIR);
        } else {
                if(strlen(tmp) >= ITC_ENV_MAXLEN) {
                        ITC_TRACE_ERROR("Instance name enviroment variable too long, length %d",
                                        strlen(tmp));
                        return -1;
                }
                strcpy(itc_inst.inst_path, tmp);
                if(itc_inst.inst_path[strlen(itc_inst.inst_path) - 1] != '/') {
                        strcat(itc_inst.inst_path, "/");
                }
        }

        tmp = getenv(ITC_ENV_INSTANCE_NAME);
        if(tmp == NULL) {
                strcat(itc_inst.inst_path, ITC_INSTANCE_DEFAULT);
        } else {
                if(strlen(tmp) >= ITC_ENV_MAXLEN) {
                        ITC_TRACE_ERROR("Instance name enviroment variable too long, length %d",
                                        strlen(tmp));
                        return -1;
                }
                strcat(itc_inst.inst_path, ITC_INSTANCE_PREFIX);
                strcat(itc_inst.inst_path, tmp);
                if(itc_inst.inst_path[strlen(itc_inst.inst_path) - 1] != '/') {
                        strcat(itc_inst.inst_path, "/");
                }
        }

        return 0;
}

/* ===================================================================== */
/**
 *   get_mailbox_info
 *
 *   @param mbox          Pointer to mailbox.
 *
 *   @param mbox_info     Pointer to mailbox information structure.
 *
 *   @param withnamespace If true namespace shall be added to mailbox
 *                        name and if false namespace shall be omitted
 *                        in mailbox name.
 *
 *   @return              -
 *
 *   @par Globals:        --
 *
 *   Function to get information about a mailbox and store in in an
 *   itc_mbox_info structure.
 */
/* ===================================================================== */
static void get_mailbox_info(struct itc_mailbox *mbox,
                             struct itc_mbox_info *mbox_info,
                             bool withnamespace)
{
        int n_filter, i;
	pthread_mutex_t *rxq_m;

#ifdef ITC_STATS
        mbox_info->stats = mbox->stats;
#else
        memset(&mbox_info->stats, 0, sizeof(struct itc_stats));
#endif

	rxq_m = &(mbox->rxq->rxq_m);
        if(mbox->state == MBOX_INUSE  ||
           mbox->state == MBOX_CLONE) {
                MUTEX_LOCK(rxq_m);
		/* Recheck state with rxq mutex locked! */
		if(mbox->state != MBOX_INUSE  &&
		   mbox->state != MBOX_CLONE) {
			MUTEX_UNLOCK(rxq_m);
		} else {
			mbox_info->mbox_id = mbox->mbox_id;
			mbox_info->state   = mbox->state;
			mbox_info->tid     = mbox->tid;

			if(withnamespace                 &&
			   itc_inst.name_space != NULL   &&
			   !(mbox->flags & ITC_MBOX_NO_NS)) {
				strcpy(mbox_info->name, itc_inst.name_space);
				strcat(mbox_info->name, mbox->name);
			} else {
				strcpy(mbox_info->name, mbox->name);
			}

			mbox_info->rx_qlen = mbox->rxq->rx_qlen;
			mbox_info->in_rx   = mbox->rxq->in_rx;
			if(mbox->rxq->filter == NULL) {
				mbox_info->filter[0] = 0;
			} else {
				n_filter = abs(mbox->rxq->filter[0]);
				if(n_filter > 4) {
					n_filter = 4;
				}

				mbox_info->filter[0] = mbox->rxq->filter[0];
				for(i=1 ; i<=n_filter ; i++) {
					mbox_info->filter[i] = mbox->rxq->filter[i];
				}
			}
			if(mbox->rxq->has_fd) {
				mbox_info->rxfd      = mbox->rxq->rxfd;
			} else {
				mbox_info->rxfd      = -1;
			}
			if(mbox->state == MBOX_CLONE) {
				mbox_info->parent_id = mbox->parent_mbox->mbox_id;
			} else {
				mbox_info->parent_id = ITC_NO_ID;
			}
			MUTEX_UNLOCK(rxq_m);

			return;
		}
        }

        mbox_info->mbox_id = mbox->mbox_id;
        mbox_info->state   = mbox->state;
        mbox_info->tid     = 0;
	strcpy(mbox_info->name, "");
	mbox_info->rx_qlen   = 0;
	mbox_info->in_rx     = 0;
	mbox_info->filter[0] = 0;
	mbox_info->rxfd      = -1;
	mbox_info->parent_id = ITC_NO_ID;
}

/* ===================================================================== */
/**
 *   mailbox_thread_death
 *
 *   @param data       Pointer to deleted mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function added as destructor to the pthread where a mailbox is
 *   created. It is run at termination of the thread deleting the mailbox.
 *
 *   Please note that it is only run when a mailbox is terminated by
 *   pthread means (pthread_cancel) or if you return from the thread
 *   function.
 */
/* ===================================================================== */
static void mailbox_thread_death(void *data)
{
        struct itc_mailbox *mbox = data;

        if(itc_inst.mboxes != NULL &&
           mymailbox == mbox       &&
           (mbox->state == MBOX_INUSE ||
            mbox->state == MBOX_CLONE)) {
                itc_delete_mailbox(mbox->mbox_id);
        }
}

/* ===================================================================== */
/**
 *   calc_abs_time
 *
 *   @param ts         Pointer to timespec that shall be adapted to
 *                     timeout.
 *
 *   @param tmo        Timeout in milli seconds.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function that sets up a struct timespec to reflect a timeout a
 *   number of milli seconds.
 */
/* ===================================================================== */
static void calc_abs_time(struct timespec *ts, unsigned long tmo)
{
        clock_gettime(CLOCK_MONOTONIC, ts);

        ts->tv_sec += tmo / 1000;
        ts->tv_nsec += (tmo % 1000) * 1000000;

        if (ts->tv_nsec >= 1000000000) {
                ts->tv_sec += 1;
                ts->tv_nsec -= 1000000000;
        }
}

/* ===================================================================== */
/**
 *   world_settings
 *
 *   @param world_mask   Pointer to world mask to be configured.
 *
 *   @param world_shift  Pointer to world shift to be configured.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function that sets up the settings for ITC world when the ITC world
 *   is running. This needs to be the same as the one in itc_world.c.
 *
 *   TODO: I should figure out a better solution for this.
 */
/* ===================================================================== */
void world_settings(uint32_t *world_mask, uint32_t *world_shift)
{
        uint32_t tmp_mask, tmp_shift;

        tmp_mask = WORLD_MASK;
        tmp_shift = 32;
        while(tmp_mask & 0x80000000) {
                tmp_mask = tmp_mask << 1;
                tmp_shift--;
        }

        *world_mask  = WORLD_MASK;
        *world_shift = tmp_shift;
}

/* ===================================================================== */
/**
 *   createandstart
 *
 *   @param worker     Pointer to fuction to be run as pthread.
 *
 *   @param arg        Argument to the above function.
 *
 *   @param t_id       Pointer to thread ID of the create thread.
 *
 *   @return           0 when thread is created.
 *
 *   @par Globals:     --
 *
 *   Create and start a pthread.
 */
/* ===================================================================== */
static int
createandstart(void *(*worker)(void *), void *arg, pthread_t *t_id,
	       int policy, int prio)
{
        int res;
        pthread_attr_t t_attr;
	struct sched_param sh_par;

        /* we will have them detached */
        if ((res = pthread_attr_init(&t_attr)) != 0) {
                return -1;
        }

	if(pthread_attr_setschedpolicy(&t_attr, policy) != 0) {
		return -1;
	}

	sh_par.sched_priority = prio;
	if(pthread_attr_setschedparam(&t_attr, &sh_par) != 0) {
		return -1;
	}

	if (pthread_attr_setinheritsched(&t_attr, PTHREAD_EXPLICIT_SCHED) != 0) {
		return -1;
	}

        if ((res = pthread_create(t_id, &t_attr, worker, arg)) != 0) {
                return -1;
        }

        return 0;
}


/* ===================================================================== */
/**
 *   Check if priority for given scheduling policy is in valid range.
 *
 *   @param policy        Scheduling policy.
 *   @param highpriority  High scheduling priority.
 *   @param priority      Scheduling priority.
 *
 *   @return              0 in case of success, -1 in case of failure.
 *
 *   @par Globals:      --
 *
 */
/* ===================================================================== */
static int
check_sched_params(int policy,int highpriority,int priority)
{

   int max_policy_prio = sched_get_priority_max(policy);
   int min_policy_prio = sched_get_priority_min(policy);

   if (priority < min_policy_prio ||
       priority > max_policy_prio ||
       highpriority < min_policy_prio ||
       highpriority > max_policy_prio)
   {
      /* invalid priority for this policy */
      return -1 ;
   }

   return 0;
}

/* ===================================================================== */
/**
 *   Get scheduling policy and priority from environments variables.
 *
 *   @param policy        Out: scheduling policy.
 *   @param highpriority  Out: High scheduling priority.
 *   @param priority      Out: scheduling priority.
 *
 *   @return            0 in case of success, -1 in case of failure.
 *
 *   @par Globals:      --
 *
 *   @note Default to SCHED_OTHER if no policy given.
 *         SCHED_OTHER always has priority 0 (zero).
 *         Default priority for real time scheduling policies
 *         is the minimum allowed priority and default highpriority is 40.
 */
/* ===================================================================== */
static int
getschedparams(int *policy, int *highpriority, int *priority)
{
	char *env_policy = NULL;
	char *env_prio = NULL;

        /* SCHED_OTHER always has priority 0 (zero). So both
          calls to get max or min priority will return 0. */
        *policy = SCHED_OTHER;
	*highpriority = sched_get_priority_max(SCHED_OTHER);
	*priority = sched_get_priority_min(SCHED_OTHER);

	env_policy = getenv("CRL_SCHED_POLICY");
	if (env_policy) {
		if (strcasecmp(env_policy, "FIFO") == 0) {
                         *policy = SCHED_FIFO;
		} else if (strcasecmp(env_policy, "RR") == 0) {
			*policy = SCHED_RR;
		} else if (strcasecmp(env_policy, "OTHER") == 0) {
			return 0;  /* SCHED_OTHER, already set */
		} else {
			return -1; /* Invalid policy. */
		}
	} else {
		return 0; /* SCHED_OTHER, already set */
	}

	env_prio = getenv("CRL_SCHED_PRIO");
	if (env_prio) {
		errno = 0;
		*priority = (int)strtol(env_prio, NULL, 10);
		if (errno != 0) {
			return -1;
		}
	} else {
            /* default priroity */
           *priority = sched_get_priority_min(*policy);
	}

	env_prio = getenv("CRL_SCHED_HIGH_PRIO");
	if (env_prio) {
		errno = 0;
		*highpriority = (int)strtol(env_prio, NULL, 10);
		if (errno != 0) {
			return -1;
		}
	} else {
           /* it is on user to make sure that high priority is
              bigger than regular priority  */
           *highpriority = ITC_HIGH_PRIORITY;
	}

	return check_sched_params(*policy,*highpriority,*priority);
}

/* ===================================================================== */
/**
 *   add_itcthread
 *
 *   @param worker     Pointer to fuction to be run as pthread.
 *
 *   @param arg        Argument to the above function.
 *
 *   @param highprio   Set to true if the thread needs some realtime
 *                     behaviour for realtime scheduling policies.
 *
 *   @param start_m    Startup synchronisation mutex, if no
 *                     synchronisation should be performed set to NULL.
 *
 *   @return           0 when thread is added.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   Add a pthread to be run by ITC.
 */
/* ===================================================================== */
int
add_itcthread(void *(*worker)(void *), void *arg,
	      bool highprio, pthread_mutex_t *start_m)
{
        struct itc_threads *thr;

        thr = malloc(sizeof(struct itc_threads));
        if(thr == NULL) {
                return -1;
        }

        thr->worker   = worker;
        thr->arg      = arg;
        thr->start_m  = start_m;
	thr->highprio = highprio;

        MUTEX_LOCK(&itc_inst.list_m);
        thr->next = itc_inst.thread_list;
        itc_inst.thread_list = thr;
        MUTEX_UNLOCK(&itc_inst.list_m);

        return 0;
}


/* ===================================================================== */
/**
 *   start_itcthreads
 *
 *   @return           0 when all threads successfully created.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   Add a pthread to be run by ITC.
 */
/* ===================================================================== */
static int
start_itcthreads(void)
{
        struct itc_threads *thr;
        int policy, highprio, prio, result = 0;

	if(getschedparams(&policy, &highprio, &prio) != 0) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "Get scheduling parameters failed", 0);
                /* Will not return here! */
	}

        MUTEX_LOCK(&itc_inst.list_m);
        thr = itc_inst.thread_list;

        while(thr != NULL) {
                if(thr->start_m != NULL) {
                        MUTEX_LOCK(thr->start_m);
                }
                result = createandstart(thr->worker, thr->arg,
                                        &thr->tid,
					policy,
					(thr->highprio ?
					 highprio : prio));
                if(result != 0) {
                        break;
                }
                if(thr->start_m != NULL) {
                        /* This is done to stop execution until the created
                           thread has managed to initialised itself. So above
                           you take the mutex and after you have started the
                           thread you take the mutex again to make yourself
                           go to "sleep". The created thread will then release
                           the mutex when appropriate and we wake up. We then
                           realease to be able to delete it at itc_exit.*/
                        MUTEX_LOCK(thr->start_m);
                        MUTEX_UNLOCK(thr->start_m);
                }
                thr = thr->next;
        }

        MUTEX_UNLOCK(&itc_inst.list_m);

        return result;
}

/* ===================================================================== */
/**
 *   start_itcthreads
 *
 *   @return           0 when all threads successfully created.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   Add a pthread to be run by ITC.
 */
/* ===================================================================== */
static int
kill_itcthreads(void)
{
        struct itc_threads *thr, *tmp;
        int result = 0;

        MUTEX_LOCK(&itc_inst.list_m);
        thr = itc_inst.thread_list;
        while(thr != NULL) {
                result = pthread_cancel(thr->tid);
                if(result != 0) {
                        break;
                }
                result = pthread_join(thr->tid, NULL);
                if(result != 0) {
                        break;
                }
                tmp = thr;
                thr = thr->next;
                free(tmp);
        }
        MUTEX_UNLOCK(&itc_inst.list_m);

        return result;
}

/* ===================================================================== */
/**
 *   locate_async_repl
 *
 *   @param msg        Pointer to locate_async_repl message.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function in ITC local coordinator that handles the locate_async_repl
 *   message.
 */
/* ===================================================================== */
static void locate_async_repl(union itc_msg *msg)
{
        struct itc_mailbox *mbox;
        struct itc_message *message, *prev;
        union itc_msg *loc_msg;

#if 0
        ITC_COORD_TRACE(itc_coord_ITC_LOCATE_ASYNC_REPL,
                        msg->msgno, mymailbox->mbox_id,
                        msg->itc_locate_async_repl.from_mbox,
                        (unsigned long)msg->itc_locate_async_repl.data,
                        msg->itc_locate_async_repl.mbox_id,
                        msg->itc_locate_async_repl.transport,
                        msg->itc_locate_async_repl.mbox_name);
#endif

        mbox = find_mbox(msg->itc_locate_async_repl.from_mbox);
        if(mbox == NULL ||
           mbox->state == MBOX_UNUSED) {
                /* MBOX never created, error */
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__, 0,
                                  "Could not find located mailbox(0x08%x)",
                                  msg->itc_locate_async_repl.from_mbox);
        } else if(mbox->state == MBOX_DELETED ||
                  mbox->mbox_id != msg->itc_locate_async_repl.from_mbox) {
                ITC_TRACE_ERROR("Mbox deleted mbox:0x%08x(0x%08x) state: %d",
                                mbox->mbox_id, msg->itc_locate_async_repl.from_mbox,
                                mbox->state);
                return;
        }

        MUTEX_LOCK(&mbox->locate_lock);

        prev = NULL;
        message = mbox->pending_locate;
        while(message != NULL) {
                if(message == msg->itc_locate_async_repl.data) {
                        if(prev == NULL) {
                                mbox->pending_locate = message->next;
                        } else {
                                prev->next = message->next;
                        }
                        DEC_STAT(mbox, pend_locates);

                        loc_msg = MESSAGE_TO_MSG(message);
                        message = message->next;

                        ITC_COORD_TRACE(itc_coord_send_locate_found,
                                        msg->itc_locate_async_repl.mbox_id,
                                        msg->itc_locate_async_repl.from_mbox,
                                        (unsigned long)loc_msg, loc_msg->msgno);

                        itc_send(&loc_msg, msg->itc_locate_async_repl.from_mbox,
                                 msg->itc_locate_async_repl.mbox_id);
                        break;
                } else {
                        prev    = message;
                        message = message->next;
                }
        }

        MUTEX_UNLOCK(&mbox->locate_lock);
}

/* ===================================================================== */
/**
 *   trigger_monitor
 *
 *   @param msg        Pointer to itc_trig_monitor message.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function in ITC local coordinator that handles the itc_trig_monitor
 *   message. This message is sent from itcworld when a remote mailbox
 *   that has been monitored from this process has been removed. One
 *   message is sent for each monitor.
 */
/* ===================================================================== */
static void trigger_monitor(union itc_msg *msg)
{
        union itc_msg *mon_msg;
        struct monitor_record *rec;
        itc_monitor_id_t mon_id;
        struct itc_mailbox *mon_mbox;
        struct monitor_mbox_record *mon_rec;

        mon_id = msg->itc_trig_monitor.mon_id;
        rec = &(itc_inst.monitor_list[MONITOR_REC_FROM_ID(mon_id)]);

        MUTEX_LOCK(&rec->lock_m);

        if(rec->state != MONITOR_INUSE ||
           rec->mon_id != mon_id ||
           rec->monitor_mbox->mbox_id != msg->itc_trig_monitor.from_mbox_id) {
                MUTEX_UNLOCK(&rec->lock_m);
                return;
        }

        if(rec->target_entry != NULL) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "ITC local coordinator trigger for non remote monitor", 0);
                /* Will not return here! */
        }

        mon_rec  = rec->monitor_entry;
        mon_mbox = rec->monitor_mbox;
        mon_msg  = rec->msg;

        if(mon_rec != NULL) {
                MUTEX_LOCK(&mon_mbox->mymonitors_m);
                DEC_STAT(mon_mbox, pend_mymonitors);
                LL_OUT(struct monitor_mbox_record, mon_rec);
                mon_rec->rec = NULL;
                MUTEX_UNLOCK(&mon_mbox->mymonitors_m);
        }

        rec->target_mbox   = NULL;
        rec->target_entry  = NULL;
        rec->monitor_mbox  = NULL;
        rec->monitor_entry = NULL;
        rec->msg           = NULL;
        rec->state         = MONITOR_DELETED;
        MONITOR_INCR_AGE(rec->mon_id);

        MUTEX_UNLOCK(&rec->lock_m);

        if(mon_rec != NULL) {
                itc_free((union itc_msg **) &mon_rec);
        }

        if(mon_msg == NULL) {
                mon_msg = itc_alloc(sizeof(uint32_t),
                                    ITC_MONITOR_DEFAULT_NO);
        }
        itc_send(&mon_msg, mon_mbox->mbox_id,
                 msg->itc_trig_monitor.target_mbox_id);

        if(itc_inst.free_rec != NULL) {
                MUTEX_LOCK(&itc_inst.free_rec_m);
                LL_AT_FRONT(struct monitor_record, itc_inst.free_rec, rec);
                MUTEX_UNLOCK(&itc_inst.free_rec_m);
        }
}

/* ===================================================================== */
/**
 *   handle_get_mbox_info
 *
 *   @param msg        Pointer to itc_get_mbox_info message.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function in ITC local coordinator that handles the ITC_GET_MBOX_INFO
 *   message.
 */
/* ===================================================================== */
static void handle_get_mbox_info(union itc_msg *msg)
{
        struct itc_mailbox *mbox;
        union itc_msg *info_msg;

	if(msg->itc_get_mbox_info.mbox_id == ITC_MY_MBOX)
		mbox = NULL;
	else
		mbox = find_mbox(msg->itc_get_mbox_info.mbox_id);
        if(mbox != NULL) {
                info_msg = itc_alloc(sizeof(struct itc_get_mbox_info_repl) +
                                      ITC_NAME_MAXLEN,
                                     ITC_GET_MBOX_INFO_REPL);
                info_msg->itc_get_mbox_info_repl.mbox_id = msg->itc_get_mbox_info.mbox_id;

                get_mailbox_info(mbox,
                                 &info_msg->itc_get_mbox_info_repl.mbox_info,
                                 true);
                info_msg->itc_get_mbox_info_repl.mbox_found = 1;
        } else {
                info_msg = itc_alloc(sizeof(struct itc_get_mbox_info_repl),
                                     ITC_GET_MBOX_INFO_REPL);
                info_msg->itc_get_mbox_info_repl.mbox_found = 0;
        }

        itc_send(&info_msg, itc_sender(msg), ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   handle_get_alloc_info
 *
 *   @param msg        Pointer to itc_get_alloc_info message.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function in ITC local coordinator that handles the ITC_GET_ALLOC_INFO
 *   message.
 */
/* ===================================================================== */
static void handle_get_alloc_info(union itc_msg *msg)
{
        union itc_msg *info_msg;
        struct itc_alloc_info *ai;

        info_msg = itc_alloc(sizeof(struct itc_get_alloc_info_repl),
                             ITC_GET_ALLOC_INFO_REPL);

        if(alloc_funcs.itci_free != NULL) {
                ai = alloc_funcs.itci_info();
                memcpy(&(info_msg->itc_get_alloc_info_repl.alloc_info),
                       ai, sizeof(struct itc_alloc_info));
                itc_free((union itc_msg **)&ai);
        } else {
                info_msg->itc_get_alloc_info_repl.alloc_info.scheme =
                        ITC_NUM_SCHEMES;
        }

        itc_send(&info_msg, itc_sender(msg), ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   itc_local_coordinator
 *
 *   @param data       Input argument to thread, unused.
 *
 *   @return           Never returns
 *
 *   @par Globals:     --
 *
 *   This is the ITC local coordinator thread which is started by
 *   itc_init and might by terminated by itc_exit.
 */
/* ===================================================================== */
static void *itc_local_coordinator(void *data)
{
        union itc_msg      *msg;
        uint32_t            any[] = { 0 };
        char                name[ITC_NAME_MAXLEN];

	if(prctl(PR_SET_NAME, "itc_coordinator", 0, 0, 0) == -1) {
                ITC_TRACE_ERROR("prctl failed, error %d(%s)",
                                errno, strerror(errno));
                abort();
	}

        if(pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL) != 0) {
                ITC_TRACE_ERROR("Failed to set pthread cancel state", 0);
                abort();
        }

        sprintf(name, "itc_local_coord_0x%08x", itc_inst.my_world_id);
        itc_inst.local_coord_mbox_id = itc_create_mailbox(name,
                                                          ITC_MBOX_NO_NS);
        if(itc_inst.local_coord_mbox_id == ITC_NO_ID) {
                /* How to error handle this ??? */
                ITC_TRACE_ERROR("Create local coordinator mailbox failed", 0);
                abort();
        }

        /* If a namespace is set we have to inform world here! */
        if(itc_inst.name_space != NULL) {
                union itc_msg *msg;

                /* Report name space to world! */
                msg = itc_alloc((sizeof(struct itc_set_namespace) +
                                 strlen(itc_inst.name_space)),
                                ITC_SET_NAMESPACE);
                msg->itc_set_namespace.world_id = itc_inst.my_world_id;
                strcpy(msg->itc_set_namespace.name_space,
                       itc_inst.name_space);
                itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
        }

        /* Release itc_init call */
        MUTEX_UNLOCK(&itc_inst.local_coord_sync);


        for(;;) {
                msg = itc_receive(any, ITC_NO_TMO, ITC_FROM_ALL);

                switch(msg->msgno) {
                case ITC_LOCATE_ASYNC_REPL:
                        locate_async_repl(msg);
                        break;
                case ITC_TRIG_MONITOR:
                        trigger_monitor(msg);
                        break;
                case ITC_GET_MBOX_INFO:
                        handle_get_mbox_info(msg);
                        break;
                case ITC_GET_ALLOC_INFO:
                        handle_get_alloc_info(msg);
                        break;
                default:
                        /* ERROR trace here */
                        ITC_TRACE_ERROR("Unexpected message received: 0x%x",
                                        msg->msgno);
                        break;
                }

                itc_free(&msg);
        }
}

/* ===================================================================== */
/**
 *   return_all_monitors
 *
 *   @param mbox       Mailbox to return monitors for.
 *
 *   @return           A list with all monitor messages that shall be
 *                     sent.
 *
 *   @par Globals:     --
 *
 *   This function triggers all monitors that a mailbox has when it is
 *   deleted. It also removes all the monitors that the deleted mailbox
 *   has created.
 */
/* ===================================================================== */
static struct itc_message *return_all_monitors(struct itc_mailbox *mbox)
{
        union itc_msg *msg;
        struct monitor_mbox_record *tar_rec, *tmp_rec, *mon_rec;
        struct monitor_record *rec;
        itc_mbox_id_t target, monitor;
        struct itc_message *mon_messages = NULL, *tmp;

        while(1) {
                MUTEX_LOCK(&mbox->monitors_m);
                tar_rec = LL_FIRST(struct monitor_mbox_record,
                                   mbox->monitors);

                if(tar_rec == NULL ||
                   tar_rec->rec == NULL) {
                        MUTEX_UNLOCK(&mbox->monitors_m);
                        break;
                }
                rec = tar_rec->rec;
                MUTEX_UNLOCK(&mbox->monitors_m);

                MUTEX_LOCK(&rec->lock_m);
//                tar_rec->rec = NULL;

                MUTEX_LOCK(&mbox->monitors_m);
                LL_GETFIRST(struct monitor_mbox_record,
                            mbox->monitors,
                            tmp_rec);

                if(tar_rec != tmp_rec) {
                        if(tmp_rec != NULL) {
                                LL_AT_FRONT(struct monitor_mbox_record,
                                            mbox->monitors,
                                            tmp_rec);
                                MUTEX_UNLOCK(&mbox->monitors_m);
                                MUTEX_UNLOCK(&rec->lock_m);
                                continue;
                        } else {
                                MUTEX_UNLOCK(&mbox->monitors_m);
                                MUTEX_UNLOCK(&rec->lock_m);
                                break;
                        }
                }

                DEC_STAT(mbox, monitors);
                MUTEX_UNLOCK(&mbox->monitors_m);

                if(rec->state != MONITOR_INUSE) {
                        MUTEX_UNLOCK(&rec->lock_m);
                        continue;
                }
                mon_rec = rec->monitor_entry;
                msg     = rec->msg;
                target  = rec->target_mbox->mbox_id;
                monitor = rec->monitor_mbox->mbox_id;

                if(mon_rec != NULL) {
                        MUTEX_LOCK(&rec->monitor_mbox->mymonitors_m);
                        DEC_STAT(rec->monitor_mbox, pend_mymonitors);
                        LL_OUT(struct monitor_mbox_record, mon_rec);
                        mon_rec->rec = NULL;
                        MUTEX_UNLOCK(&rec->monitor_mbox->mymonitors_m);
                }

                rec->target_mbox   = NULL;
                rec->target_entry  = NULL;
                rec->monitor_mbox  = NULL;
                rec->monitor_entry = NULL;
                rec->msg           = NULL;
                rec->state         = MONITOR_DELETED;
                MONITOR_INCR_AGE(rec->mon_id);

                MUTEX_UNLOCK(&rec->lock_m);

                itc_free((union itc_msg **) &tar_rec);
                if(mon_rec != NULL) {
                        itc_free((union itc_msg **) &mon_rec);
                }

                if(msg == NULL) {
                        msg = itc_alloc(sizeof(uint32_t),
                                        ITC_MONITOR_DEFAULT_NO);
                }
                tmp = MSG_TO_MESSAGE(msg);
                tmp->sender   = target;
                tmp->receiver = monitor;
                tmp->next = mon_messages;
                mon_messages = tmp;

                if(itc_inst.free_rec != NULL) {
                        MUTEX_LOCK(&itc_inst.free_rec_m);
                        LL_AT_FRONT(struct monitor_record, itc_inst.free_rec, rec);
                        MUTEX_UNLOCK(&itc_inst.free_rec_m);
                }
        }
        if(mbox->mymonitors != NULL) {
			itc_monitor_id_t mtor;
			while (1) {
				MUTEX_LOCK(&mbox->mymonitors_m);
				mon_rec = LL_FIRST(struct monitor_mbox_record, mbox->mymonitors);
				if (mon_rec == NULL) {
					MUTEX_UNLOCK(&mbox->mymonitors_m);
					break;
				}
				mtor = mon_rec->rec->mon_id;
				MUTEX_UNLOCK(&mbox->mymonitors_m);
				itc_unmonitor(mtor);
			}
        }
        return mon_messages;
}

/* ===================================================================== */
/**
 *   free_rec_exit
 *
 *   @return           0 at success, negative if an error has occurred.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   This function frees all monitor records and asociated items.
 */
/* ===================================================================== */
static int free_rec_exit(void)
{
        struct monitor_record *rec;
        int i, ret;

        MUTEX_LOCK(&itc_inst.free_rec_m);
        for(i=0 ; i<itc_inst.monitor_size ; i++) {
                rec = &(itc_inst.monitor_list[i]);
                if(rec->target_entry != NULL) {
                        itc_free((union itc_msg **)&rec->target_entry);
                }

                if(rec->monitor_entry != NULL) {
                        itc_free((union itc_msg **)&rec->monitor_entry);
                }

                ret = pthread_mutex_destroy(&rec->lock_m);
                if(ret != 0) {
                        ITC_TRACE_ERROR("Pthread mutex destroy error errno: %d(%s)",
                                        ret, strerror(ret));
                        return -1;
                }
        }
        MUTEX_UNLOCK(&itc_inst.free_rec_m);

        ret = pthread_mutex_destroy(&itc_inst.free_rec_m);
        if(ret != 0) {
                        ITC_TRACE_ERROR("Pthread mutex destroy error errno: %d(%s)",
                                        ret, strerror(ret));
                return -1;
        }

        free(itc_inst.free_rec);
        itc_inst.free_rec = NULL;
        free(itc_inst.monitor_free);
        itc_inst.monitor_free = NULL;

        return 0;
}

/* ===================================================================== */
/**
 *   free_errh_exit
 *
 *   @return           0 at success, negative if an error has occurred.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   This function frees the errorhandler list.
 */
/* ===================================================================== */
static int free_errh_exit(void)
{
        struct errh_record *errh;

        MUTEX_LOCK(&itc_inst.list_m);
        LL_GETFIRST(struct errh_record, itc_inst.errh_list, errh);
        MUTEX_UNLOCK(&itc_inst.list_m);
        while(errh != NULL) {
                free(errh);

                MUTEX_LOCK(&itc_inst.list_m);
                LL_GETFIRST(struct errh_record, itc_inst.errh_list, errh);
                MUTEX_UNLOCK(&itc_inst.list_m);
        }

        if(pthread_mutex_destroy(&itc_inst.list_m) != 0) {
                return -1;
        }

        free(itc_inst.errh_list);
        itc_inst.errh_list = NULL;

        return 0;
}

/* ===================================================================== */
/**
 *   itc_default_errh
 *
 *   @param itc_call   ITC fucntion call in progress.
 *
 *   @param msg        Error message.
 *
 *   @param flags      Error flags.
 *
 *   @param file       File where error occurred.
 *
 *   @param line       Line in file.
 *
 *   @return           Will not reurn since abort will be called.
 *
 *   @par Globals:     --
 *
 *   This is the default error handler added by ITC. If the execution
 *   reaches this error handler the program wll be aborted.
 */
/* ===================================================================== */
static int itc_default_errh(const char *itc_call,
                            const char *msg,
                            uint32_t    flags,
                            const char *file,
                            int         line)
{
        ITC_TRACE_ERROR("ITC ERROR", 0);
        ITC_TRACE_ERROR("Call:    %s", itc_call);
        ITC_TRACE_ERROR("Message: %s", msg);
        ITC_TRACE_ERROR("Flags:   0x%08x", flags);
        ITC_TRACE_ERROR("File:    %s", file);
        ITC_TRACE_ERROR("Line:    %d", line);

#if 1
        printf("ITC ERROR\n");
        printf("Call:    %s\n", itc_call);
        printf("Message: %s\n", msg);
        printf("Flags:   0x%08x\n", flags);
        printf("File:    %s(%d)\n", file, line);
#endif
        /* Restart process here, use abort to get a core dump */
        abort();
}

/* ===================================================================== */
/**
 *   itc_error
 *
 *   @param itc_call   ITC fucntion call in progress.
 *
 *   @param msg        Error message.
 *
 *   @param flags      Error flags.
 *
 *   @param file       File where error occurred.
 *
 *   @param line       Line in file.
 *
 *   @return           Will not reurn since abort will be called.
 *
 *   @par Globals:     --
 *
 *   ITC error function. When ITC encounters an error it will call this
 *   function.
 */
/* ===================================================================== */
void itc_error(const char *itc_call,
               const char *file,
               int         line,
               uint32_t    flags,
               const char *format,
               ...)
{
        va_list args;
        char buffer[256];
        struct errh_record *rec;
        int ret;

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

#ifdef ITC_LTTNG
        if(mymailbox != NULL) {
                tracepoint(com_ericsson_itc_if, itc_error, mymailbox->mbox_id,
                           (char *)file, line, buffer);
        } else {
                tracepoint(com_ericsson_itc_if, itc_error, 0,
                           (char *)file, line, buffer);
        }
#endif

        MUTEX_LOCK(&itc_inst.list_m);

        rec = LL_FIRST(struct errh_record, itc_inst.errh_list);
        if(rec == NULL) {
                /* ITC INIT not run, call default error handler */
                itc_default_errh(itc_call, buffer, flags, file, line);
        }

        while(rec != NULL) {
                ret = rec->errh(itc_call, buffer, flags, file, line);
                if(ret == 0 &&
                   (flags & ITC_ERROR_FATAL) == 0) {
                        break;
                }

                LL_NEXT(struct errh_record, rec);
        }

        MUTEX_UNLOCK(&itc_inst.list_m);
}

/* ===================================================================== */
/**
 *   free_itc_resources
 *
 *   @return           -
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 *   Function that frees all of ITCs resources.
 *   Used after fork.
 */
/* ===================================================================== */
static void free_itc_resources(void)
{

        /* Delete destructor key */
        if(pthread_key_delete(itc_inst.destruct_key) != 0) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
                                  ITC_ERROR_FATAL,
                                  "Pthread key delete error", 0);
                return;
        }

        tdestroy(itc_inst.local_locate_tree, do_nothing);

        free(itc_inst.free_rec);
        free(itc_inst.monitor_free);
        free(itc_inst.errh_list);

        if(itc_inst.name_space) {
                free(itc_inst.name_space);
        }

        free(itc_inst.mboxes);
        memset(&itc_inst, 0, sizeof(struct itc_instance));

        mymailbox = NULL;
}

/* ===================================================================== */
/**
 *   Deletes a mailbox
 *
 *   @param mbox       Pointer to mailbox to be deleted.
 *
 *   @param file       Pointer to file of ITC API call.
 *
 *   @param line       Line of ITC API call.
 *
 *   @return           List of monitor messages to be sent at the
 *                     end of the itc_delete_mailbox call.
 *
 *   @par Globals:     --
 *
 *   The function that deletes the mailbox structures from ITC.
 *   All santy checks are run prior to this function.
 */
/* ===================================================================== */
static struct itc_message *delete_mailbox(struct itc_mailbox *mbox,
					  const char *file, int line)
{
	struct itc_message *mon_messages;
	union itc_msg *msg;
	int i;

#ifdef ITC_STAT_MBOXID
	for(i=0 ; i<MAX_STAT_ID ; i++) {
		struct itc_mailbox *tmp;

		tmp = itc_inst.statmboxarray[i];
		if(tmp != NULL &&
		   tmp->mbox_id == mbox->mbox_id) {
			itc_inst.statmboxarray[i] = NULL;
		}
	}
#endif

	/* Remove mailbox from locate tree */
        if(remove_locate(&itc_inst.local_locate_tree,
                         &itc_inst.local_locate_tree_m, mbox) != 0) {
//                ITC_TRACE_ERROR("Remove locate not present in tree", 0);
        }

        /* Notify world of my demise */
        /* If I am not world send notification */
        if(itc_inst.my_world_id != (itc_inst.world_mbox_id & itc_inst.world_mask)) {
                uint32_t flags = (mbox->state == MBOX_CLONE ?
				  mbox->parent_mbox->flags : mbox->flags);

                if(itc_inst.name_space == NULL ||
                   flags & ITC_MBOX_NO_NS) {
                        msg = itc_alloc((sizeof(struct itc_add_rem_mailbox) + strlen(mbox->name)),
                                        ITC_REM_MBOX);
                        msg->itc_add_rem_mailbox.mbox_id = mbox->mbox_id;
                        strcpy(msg->itc_add_rem_mailbox.mbox_name, mbox->name);
                        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
                } else {
                        msg = itc_alloc((sizeof(struct itc_add_rem_mailbox) +
                                         strlen(itc_inst.name_space) +
                                         strlen(mbox->name)),
                                        ITC_REM_MBOX);
                        msg->itc_add_rem_mailbox.mbox_id = mbox->mbox_id;
                        strcpy(msg->itc_add_rem_mailbox.mbox_name, itc_inst.name_space);
                        strcat(msg->itc_add_rem_mailbox.mbox_name, mbox->name);
                        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
                }
        }

        /* Resolve all monitors */
        mon_messages = return_all_monitors(mbox);
	itc_free((union itc_msg **)&mbox->monitors);
	if(mbox->mymonitors != NULL) {
		itc_free((union itc_msg **)&mbox->mymonitors);
	}

        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_delete_mailbox != NULL) {
                        if(transp_funcs[i].itci_delete_mailbox(mbox) < 0) {
                                ITC_ERROR(0, "delete mailbox gets error from transport type %d", i);
                                return mon_messages;
                        }
                }
        }

        mbox->rxq = NULL;

        /* Build new mbox_id to be used next time */
        mbox->aging++;
        mbox->mbox_id = (itc_inst.my_world_id |
                         ((mbox->aging << itc_inst.age_shift) &
			  ~itc_inst.world_mask) |
			 (mbox->mbox_id & itc_inst.mbox_id_mask));
        strcpy(mbox->name, "");
	mbox->tid = 0;

#ifdef ITC_STATS
        /* Zero the mailbox statistic counters */
        memset(&mbox->stats, 0, sizeof(struct itc_stats));
#endif

        q_enqueue(itc_inst.freemboxes, mbox);

	return mon_messages;
}

/* ===================================================================== */
/**
 *   Register an error handler for ITC
 *
 *   @param errh       Error handler function that shall be invoked at
 *                     error in ITC.
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void register_errorhandler(itc_errorhandler errh)
{
        struct errh_record *rec;

        rec = malloc(sizeof(struct errh_record));
        memset(rec, 0, sizeof(struct errh_record));
        rec->errh = errh;

        MUTEX_LOCK(&itc_inst.list_m);
        LL_AT_FRONT(struct errh_record, itc_inst.errh_list, rec);
        MUTEX_UNLOCK(&itc_inst.list_m);

        return;
}

/* ===================================================================== */
/**
 *   __add_monitors
 *
 *   Add a new segement of monitors to the list.
 *
 *   Note that this function expects the monitor records free list lock
 *   to be taken before being called.
 *
 *   @return               0 at success, at failure:
 *                         ITC_EOUT_OF_RESOURCES    not enough resources.
 *                         ITC_EINTERNAL_ERROR      internal ITC error.
 *
 *   @par Globals:         --
 *
 */
/* ===================================================================== */
static int __add_monitors()
{
        int prev_size;
        int add_size;
        int i;

        prev_size = itc_inst.monitor_size;
        if (prev_size >= MONITOR_MAX)
                return ITC_EOUT_OF_RESOURCES;

        add_size = (MONITOR_MAX - prev_size) < MONITOR_SEG_SIZE
                ? MONITOR_MAX - prev_size
                : MONITOR_SEG_SIZE;

        memset((char *)itc_inst.monitor_list +
               (prev_size * sizeof(struct monitor_record)),
               0,
               (add_size * sizeof(struct monitor_record)));

        for(i=prev_size; i<(prev_size + add_size) ; i++) {
                itc_inst.monitor_list[i].mon_id = MONITOR_SET_ID(i);

                if(pthread_mutex_init(&(itc_inst.monitor_list[i].lock_m), NULL) < 0) {
                        ITC_TRACE_ERROR("Pthread mutex init error", 0);
                        return ITC_EINTERNAL_ERROR;
                }

                LL_AT_FRONT(struct monitor_record,
                            itc_inst.free_rec,
                            &(itc_inst.monitor_list[i]));

                /* Since this function might exit prematurely due to the mutex
                 * init error above, the resource counter should be incremented
                 * for each successful iteration to keep it in sync.
                 */
                itc_inst.monitor_size++;
        }

        return 0;
}

/* ===================================================================== */
/**
 *   Initialises ITC for the current process.
 *
 *   @param mailbox_count  Max number of mailboxes used by this process.
 *
 *   @param alloc_scheme   Which scheme shall be used by
 *                         itc_alloc/itc_free.
 *
 *   @param scheme         Union specifying configuration parameters for
 *                         the allocation scheme.
 *
 *   @param name_space     Set ITC namespace of this linux process.
 *
 *   @param flags          Flags field controlling the behavior of
 *                         itc_init. There are currently no flags defined
 *                         bit to allow future extensions it shall be set
 *                         to 0.
 *
 *   @return               0 at success, at failure:
 *                         ITC_EINTERNAL_ERROR      internal ITC error.
 *                         ITC_EALREADY_INITIALISED itc_init already
 *                                                  run.
 *                         ITC_ENS_TO_LONG          Namespace to long.
 *                         ITC_EOUT_OF_MEMORY       itc_init not enough
 *                                                  memory available.
 *                         ITC_ENO_WORLD            itc_init could not
 *                                                  locate world.
 *                         ITC_EILLEGAL_ALLOC_CFG   Illegal alloc
 *                                                  configuration.
 *
 *   @par Globals:         --
 *
 */
/* ===================================================================== */
int __itc_init(int32_t mailbox_count,
                itc_alloc_scheme alloc_scheme,
                union itc_scheme *scheme,
                char *name_space,
                uint32_t flags,
                const char *file,
                int line)
{
        int ret, namespace_len, i = 0;
        char *ns = NULL;
        struct itc_mailbox *tmp;
        uint32_t init_flags = 0;
        int max_msgsize = ITC_MAX_MSGSIZE;
	pthread_condattr_t condattr;

        if(itc_inst.mboxes != NULL) {
                if(getpid() == itc_inst.pid) {
                        ITC_TRACE_ERROR("itc_init already run", 0);
                        return ITC_EALREADY_INITIALISED;
                } else {
                        if(alloc_funcs.itci_exit_alloc != NULL) {
                                alloc_funcs.itci_exit_alloc();
                        }
                        free_itc_resources();
                        init_flags = ITC_INIT_FORCE_REDO;
                }
        }
        itc_inst.pid = getpid();

        if(conf_run_dir() < 0) {
                return ITC_EINTERNAL_ERROR;
        }

        if(name_space != NULL ||
           (ns = getenv(ITC_ENV_NAMESPACE)) != NULL) {
                if(name_space != NULL) {
                        ns = name_space;
                }
                ITC_TRACE(itc_init_wname, mailbox_count,
                          alloc_scheme, ns, flags);
                namespace_len = strlen(ns);
                if(namespace_len > ITC_NAME_MAXLEN) {
                        ITC_TRACE_ERROR("Name space to long, %d characters",
                                        namespace_len);
                        return ITC_ENS_TO_LONG;
                }
                /* We need to add space for trailing '\0' and potentially
                   that we might need to add the '/' character at the end
                   of the namespace. */
                itc_inst.name_space = malloc(namespace_len + 2);
                if(itc_inst.name_space == NULL) {
                        ITC_TRACE_ERROR("Out of memory error", 0);
                        return ITC_EOUT_OF_MEMORY;
                }
                strcpy(itc_inst.name_space, ns);
                if(itc_inst.name_space[namespace_len - 1] != '/') {
                        strcat(itc_inst.name_space, "/");
                }
        } else {
                ITC_TRACE(itc_init, mailbox_count, alloc_scheme, flags);
        }

        /* Initialise errorhandler list */
        if(pthread_mutex_init(&itc_inst.list_m, NULL) != 0) {
                ITC_TRACE_ERROR("Pthread mutex init error", 0);
                return ITC_EINTERNAL_ERROR;
        }

        itc_inst.errh_list = malloc(sizeof(struct errh_record));
        if(itc_inst.errh_list == NULL) {
                ITC_TRACE_ERROR("Out of memory error", 0);
                return ITC_EOUT_OF_MEMORY;
        }
        memset(itc_inst.errh_list, 0, sizeof(struct errh_record));
        LL_NEW(struct errh_record, itc_inst.errh_list);

        register_errorhandler(itc_default_errh);


        /* We need three extra mailbox for ITC internal use. */
        mailbox_count += 3;

        /* Hardcoded setup of the different transport and allocation
           functions. This will have to be done in a nicer way later */
        transp_funcs[ITC_TRANSPORT_LOCAL] = local_itc_funcs;
        transp_funcs[ITC_TRANSPORT_SOCK]  = sock_itc_funcs;
        transp_funcs[ITC_TRANSPORT_SYSV]  = sysv_itc_funcs;

        if(alloc_scheme == ITC_MALLOC) {
                alloc_funcs = malloc_funcs;
        } else if(alloc_scheme == ITC_POOL) {
                alloc_funcs = pool_funcs;
        } else if(alloc_scheme == ITC_POOL_FLEX) {
                alloc_funcs = poolflex_funcs;
        } else if(alloc_scheme == ITC_USER_DEFINED) {
                alloc_funcs.itci_init_alloc = NULL;
                alloc_funcs.itci_exit_alloc = NULL;

                alloc_funcs.itci_alloc      =
                        (itci_alloc *)scheme->user_defined_parameters.alloc;

                alloc_funcs.itci_free       =
                        (itci_free *)scheme->user_defined_parameters.free;

                alloc_funcs.itci_info       =
                        (itci_info *)scheme->user_defined_parameters.info;
        } else {
                ITC_TRACE_ERROR("Illegal alloc scheme : %d", alloc_scheme);
                return ITC_EILLEGAL_ALLOC_CFG;
        }

        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_get_maxmsgsize != NULL ) {
                        int tmp;

                        tmp = transp_funcs[i].itci_get_maxmsgsize();
                        if(tmp < max_msgsize) {
                                max_msgsize = tmp;
                        }
                }
        }

        /* Initialise the memory allocation functions */
        if(alloc_funcs.itci_init_alloc != NULL) {
                ret = alloc_funcs.itci_init_alloc(scheme, max_msgsize);
                if(ret != 0) {
                        ITC_TRACE_ERROR("Alloc init function failed, scheme: %d", scheme);
                        return ret;
                }
        }

        if(flags & ITC_INIT_FLAG_I_AM_WORLD) {
                uint32_t tmp_shift;

                world_settings(&itc_inst.world_mask, &tmp_shift);
                itc_inst.my_world_id   = 1 << tmp_shift;
                itc_inst.world_mbox_id = 1 << tmp_shift | 1;
        } else {
                for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                        if(transp_funcs[i].itci_locate_world != NULL ) {
                                if(transp_funcs[i].itci_locate_world(&itc_inst.my_world_id,
                                                                     &itc_inst.world_mask,
                                                                     &itc_inst.world_mbox_id)) {
                                        break;
                                }
                        }
                }

                if(i == ITC_NUM_TRANSPORTS) {
                        ITC_TRACE_ERROR("Failed to locate world", 0);
                        return ITC_ENO_WORLD;
                }
        }

        /* To be able to have the free mbox list lock free we need to align all mailboxes
           to MBOX_ALIGNMENT. So we have to take this into account dirung allocation and
           configuration.*/
        itc_inst.mbox_offset = (((sizeof(struct itc_mailbox) + MBOX_ALIGNMENT - 1) /
                                 MBOX_ALIGNMENT) *
                                MBOX_ALIGNMENT);
        itc_inst.mboxes_free = malloc((mailbox_count * itc_inst.mbox_offset) + MBOX_ALIGNMENT);
        if(itc_inst.mboxes_free == NULL) {
                ITC_TRACE_ERROR("Out of memory error", 0);
                return ITC_EOUT_OF_MEMORY;
        }
        memset(itc_inst.mboxes_free, 0, ((mailbox_count * itc_inst.mbox_offset) + MBOX_ALIGNMENT));
        itc_inst.mboxes = (void *)ITC_ALIGN_PTR(itc_inst.mboxes_free, MBOX_ALIGNMENT);

        itc_inst.mbox_id_mask = 0xFFFFFFFF >> FFS(mailbox_count);
        itc_inst.age_shift    = 32 - FFS(mailbox_count);

        itc_inst.mailbox_count = mailbox_count;

        for(i=0 ; i<itc_inst.mailbox_count ; i++) {
                tmp = GET_MBOX(i);
                tmp->mbox_id = itc_inst.my_world_id | i;

                if(pthread_mutexattr_init(&(tmp->rxq_data.rxq_attr)) != 0) {
                        ITC_TRACE_ERROR("Pthread mutex settype error errno: %d(%s)",
                                        errno, strerror(errno));
                        return ITC_EINTERNAL_ERROR;
                }
                if(pthread_mutexattr_settype(&(tmp->rxq_data.rxq_attr),
                                             PTHREAD_MUTEX_ERRORCHECK) != 0) {
                        ITC_TRACE_ERROR("Pthread mutex settype error errno: %d(%s)",
                                        errno, strerror(errno));
                        return ITC_EINTERNAL_ERROR;
                }
                if(pthread_mutex_init(&(tmp->rxq_data.rxq_m),
                                      &(tmp->rxq_data.rxq_attr)) != 0) {
                        /* Mutex init failure, report error here */
                        ITC_TRACE_ERROR("Pthread mutex init error errno: %d(%s)",
                                        errno, strerror(errno));
                        return ITC_EINTERNAL_ERROR;
                }

		if(pthread_condattr_init(&condattr) != 0) {
                        /* Condition init failure, report error here */
                        ITC_TRACE_ERROR("Pthread condition attribute init error, errno: %d(%s)",
                                        errno, strerror(errno));
                        return ITC_EINTERNAL_ERROR;
                }
		if(pthread_condattr_setclock(&condattr, CLOCK_MONOTONIC) != 0) {
                        /* Condition init failure, report error here */
                        ITC_TRACE_ERROR("Pthread condition set clockattr error, errno: %d(%s)",
                                        errno, strerror(errno));
                        return ITC_EINTERNAL_ERROR;
                }
                if(pthread_cond_init(&(tmp->rxq_data.rxq_c), &condattr) != 0) {
                        /* Condition init failure, report error here */
                        ITC_TRACE_ERROR("Pthread condition var init error, errno: %d(%s)",
                                        errno, strerror(errno));
                        return ITC_EINTERNAL_ERROR;
                }

                if(i == 0) {
                        itc_inst.freemboxes = q_new(tmp);
                } else {
                        q_enqueue(itc_inst.freemboxes, tmp);
                }
        }


        /* Initialise destructor key */
        if(pthread_key_create(&itc_inst.destruct_key, mailbox_thread_death) != 0) {
                ITC_TRACE_ERROR("Pthread key create error", 0);
                return ITC_EINTERNAL_ERROR;
        }

        /* Initialise locate trees */
        if(pthread_mutex_init(&itc_inst.local_locate_tree_m, NULL) != 0) {
                ITC_TRACE_ERROR("Pthread mutex init error", 0);
                return ITC_EINTERNAL_ERROR;
        }

        /* Initialise monitor records free list */
        if(pthread_mutex_init(&itc_inst.free_rec_m, NULL) != 0) {
                ITC_TRACE_ERROR("Pthread mutex init error", 0);
                return ITC_EINTERNAL_ERROR;
        }

        itc_inst.free_rec = malloc(sizeof(struct monitor_record));
        if(itc_inst.free_rec == NULL) {
                /* Out of memory, report error here */
                ITC_TRACE_ERROR("Out of memory error", 0);
                return -1;
        }
        memset(itc_inst.free_rec, 0, sizeof(struct monitor_record));
        LL_NEW(struct monitor_record, itc_inst.free_rec);

        itc_inst.monitor_size = 0;
        itc_inst.monitor_free = malloc(MONITOR_MAX * sizeof(struct monitor_record) +
                                              sysconf(_SC_PAGESIZE));
        if (itc_inst.monitor_free == NULL) {
                ITC_TRACE_ERROR("Out of memory error", 0);
                return ITC_EOUT_OF_MEMORY;
        }
        itc_inst.monitor_list = (void *)ITC_ALIGN_PTR(itc_inst.monitor_free,
                                                      sysconf(_SC_PAGESIZE));
	MUTEX_LOCK(&itc_inst.free_rec_m);
        ret = __add_monitors();
	MUTEX_UNLOCK(&itc_inst.free_rec_m);
        if (ret) {
                ITC_TRACE_ERROR("Failed to add monitor resources error", 0);
                return ret;
        }

        /* Start ITC instance coordination pthread */
        if(pthread_mutex_init(&itc_inst.local_coord_sync, NULL) != 0) {
                ITC_TRACE_ERROR("Pthread mutex init error", 0);
                return ITC_EINTERNAL_ERROR;
        }

        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_init != NULL) {
                        if(transp_funcs[i].itci_init(itc_inst.my_world_id,
                                                     itc_inst.world_mask,
                                                     mailbox_count,
                                                     init_flags) != 0) {

                                ITC_TRACE_ERROR("Failed in init funtion(%d)", i);
                                return ITC_EINTERNAL_ERROR;
                        }
                }
        }

        if(add_itcthread(itc_local_coordinator, NULL, false, &itc_inst.local_coord_sync)) {
                ITC_TRACE_ERROR("Failed to add ITC local coord thread",
                                0);
                return ITC_EINTERNAL_ERROR;
        }

        if(start_itcthreads() != 0) {
                ITC_TRACE_ERROR("Failed to start ITC threads", 0);
                return ITC_EINTERNAL_ERROR;
        }

        return 0;
}

/* ===================================================================== */
/**
 *   Exits ITC for this process
 *
 *   @return         -
 *
 *   @par Globals:   --
 */
/* ===================================================================== */
void __itc_exit(const char *file, int line)
{
        struct itc_mailbox *mbox;
        int i, ret, open_mboxes = 0;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        ITC_TRACE(itc_exit, 0);

        for(i=0 ; i<itc_inst.mailbox_count ; i++) {
                mbox = GET_MBOX(i);
		MUTEX_LOCK(&(mbox->rxq_data.rxq_m));
                if(mbox->state == MBOX_INUSE) {
                        open_mboxes++;
                }
		MUTEX_UNLOCK(&(mbox->rxq_data.rxq_m));

                /* ITC opens 2 mailboxes, they are still open */
                if(open_mboxes > ITC_USED_MBOXES) {
                        ITC_ERROR(0, "itc_exit still has mailboxes error", 0);
                        return;
                }
        }

        /* Cancel local coordinator thread and delete corresponding data */
        ret = pthread_mutex_destroy(&itc_inst.local_coord_sync);
        if(ret != 0) {
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread mutex destroy error: %d(%s)",
                          ret, strerror(ret));
                return;
        }

        if(kill_itcthreads() != 0) {
                ITC_ERROR(ITC_ERROR_FATAL, "Kill ITC threads failed", 0);
                return;
        }

        for(i=0 ; i<itc_inst.mailbox_count ; i++) {
                mbox = GET_MBOX(i);
                ret = pthread_cond_destroy(&(mbox->rxq_data.rxq_c));
                if(ret != 0) {
                        ITC_ERROR(ITC_ERROR_FATAL, "pthread cond variable destroy error: %d(%s)",
                                  ret, strerror(ret));
                        return;
                }
                ret = pthread_mutex_destroy(&(mbox->rxq_data.rxq_m));
                if(ret != 0) {
                        ITC_ERROR(ITC_ERROR_FATAL, "pthread mutex destroy error: %d(%s)",
                                ret, strerror(ret));
                        return;
                }
                ret = pthread_mutexattr_destroy(&(mbox->rxq_data.rxq_attr));
                if(ret != 0) {
                        ITC_ERROR(ITC_ERROR_FATAL, "pthread mutexattr destroy error: %d(%s)",
                                  ret, strerror(ret));
                        return;
                }
        }

        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_exit != NULL ) {
                        if(transp_funcs[i].itci_exit() != 0) {
                                ITC_ERROR(0, "Failed in exit funtion(%d)", i);
                                return;
                        }
                }
        }

        /* Delete destructor key */
        ret = pthread_key_delete(itc_inst.destruct_key);
        if(ret != 0) {
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread key delete error: %d(%s)",
                          ret, strerror(ret));
                return;
        }

        /* Destroy locate tree */
        MUTEX_LOCK(&itc_inst.local_locate_tree_m);
        tdestroy(itc_inst.local_locate_tree, do_nothing);
        MUTEX_UNLOCK(&itc_inst.local_locate_tree_m);

        ret = pthread_mutex_destroy(&itc_inst.local_locate_tree_m);
        if(ret != 0) {
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread mutex destroy error: %d(%s)",
                          ret, strerror(ret));
                return;
        }

        /* Free all monitor records */
        if(free_rec_exit() != 0) {
                ITC_ERROR(ITC_ERROR_FATAL, "Free monitor records failed", 0);
                return;
        }

        /* Free errorhandler list */
        if(free_errh_exit() != 0) {
                ITC_ERROR(ITC_ERROR_FATAL, "Free errorhandlers failed", 0);
                return;
        }

        if(itc_inst.name_space) {
                free(itc_inst.name_space);
        }

        if(alloc_funcs.itci_exit_alloc != NULL) {
                alloc_funcs.itci_exit_alloc();
        }

        free(itc_inst.mboxes_free);
	q_destroy(itc_inst.freemboxes);
        memset(&itc_inst, 0, sizeof(struct itc_instance));
}

/* ===================================================================== */
/**
 *   Allocate an ITC message.
 *
 *   @param size       Size of the message to allocate.
 *
 *   @param msgno     Message number of the allocated message.
 *
 *   @return           Pointer to allocated message, will always return a
 *                     valid message pointer, if memory can not be
 *                     allocated the error handler will be called.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
union itc_msg *__itc_alloc(size_t size,
                           uint32_t msgno,
                           const char *file,
                           int line)
{
        struct itc_message *message;
        char *endp;
        if(size < sizeof(msgno)) {
                size = sizeof(msgno);
        }

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        message = alloc_funcs.itci_alloc(size + ITC_MSG_ADM_OFFSET + 1);
        if(message == NULL) {
                ITC_ERROR(0, "Out of memory error", 0);
                return NULL;
        }
        message->msgno    = msgno;
        if(mymailbox != NULL) {
                message->sender   = mymailbox->mbox_id;
        } else {
                message->sender   = 0;
        }
        message->receiver = 0;
        message->size     = size;
        message->flags    = 0;
        endp = (char *)((unsigned long)&message->msgno + size);
        *endp = ENDP_ID;

        INC_STAT(mymailbox, allocs);

        ITC_TRACE(itc_alloc, size, msgno,
                  (unsigned long)&(message->msgno));

        return (union itc_msg *)&(message->msgno);
}

/* ===================================================================== */
/**
 *   Free ITC message.
 *
 *   @param msg        Pointer to ITC message pointer.
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_free(union itc_msg **msg, const char *file, int line)
{
        struct itc_message *message;
        char *endp;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if( msg == NULL ||
           *msg == NULL) {
                ITC_ERROR(0, "Trying to free NULL pointer", 0);
                return;
        }

        ITC_TRACE(itc_free, (unsigned long)*msg);

        message = MSG_TO_MESSAGE(*msg);
        endp = (char *)((unsigned long)&message->msgno + message->size);



        if(message->flags & ITC_MSG_INRX) {
                ITC_ERROR(0, "Trying to free message in RX queue msg %p flags 0x%04x",
                          *msg, message->flags);
                return;
        } else if(*endp != ENDP_ID) {
                ITC_ERROR(0, "Message endp overwritten msg %p endp: 0x%02x",
                          *msg, *endp);
                return;
        }


        alloc_funcs.itci_free(message);

        INC_STAT(mymailbox, frees);

        *msg = NULL;
}

/* ===================================================================== */
/**
 *   Get sender of an ITC message.
 *
 *   @param msg        Pointer to the ITC message to get sender for.
 *
 *   @return           The sender of the message.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
itc_mbox_id_t __itc_sender(union itc_msg *msg, const char *file, int line)
{
        struct itc_message *tmp;

        tmp = MSG_TO_MESSAGE(msg);

        ITC_TRACE(itc_sender, (unsigned long)msg, tmp->sender);

        return tmp->sender;
}

/* ===================================================================== */
/**
 *   Get receiver of an ITC message.
 *
 *   @param msg        Pointer to the ITC message to get receiver for.
 *
 *   @return           The receiver of the message.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
itc_mbox_id_t __itc_receiver(union itc_msg *msg, const char *file, int line)
{
        struct itc_message *tmp;

        tmp = MSG_TO_MESSAGE(msg);

        ITC_TRACE(itc_receiver, (unsigned long)msg, tmp->receiver);

        return tmp->receiver;
}

/* ===================================================================== */
/**
 *   Get size of ITC message.
 *
 *   @param msg        Pointer to the ITC message to get sender for.
 *
 *   @return           Size of the message.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
size_t __itc_size(union itc_msg *msg, const char *file, int line)
{
        struct itc_message *tmp;

        tmp = MSG_TO_MESSAGE(msg);

        ITC_TRACE(itc_size, (unsigned long)msg, tmp->size);

        return tmp->size;
}

/* ===================================================================== */
/**
 *   Set size of ITC message.
 *
 *   @param msg        Pointer to the ITC message to get sender for.
 *
 *   @param size       New message size.
 *
 *   @return           Non zero if message size successfully set,
 *                     0 if message size not updated.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
int32_t __itc_setsize(union itc_msg *msg,
                      int32_t newsize,
                      const char *file,
                      int line)
{
        struct itc_message *tmp;
        char *endp;

        tmp = MSG_TO_MESSAGE(msg);

        if(newsize > tmp->size) {
                return 0;
        }

        tmp->size = newsize;
        endp = (char *)((unsigned long)&tmp->msgno + tmp->size);
        *endp = ENDP_ID;

        return 1;
}

/* ===================================================================== */
/**
 *   Create ITC mailbox for current thread.
 *
 *   @param name       Name of the ITC mailbox. Can be max
 *                     ITC_NAME_MAXLEN characters.
 *
 *   @param flags      Flags field controlling the behavior of
 *                     itc_create_mailbox. Currently no flags are defined
 *                     but to allow future extensions the flags field
 *                     needs to be set to 0.
 *
 *   @return           Mail box ID of the created mailbox.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
itc_mbox_id_t __itc_create_mailbox(const char *name,
                                   uint32_t flags,
                                   const char *file,
                                   int line)
{
        struct itc_mailbox *new_mbox;
        int i, ret;
        union itc_msg *msg;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mymailbox != NULL &&
           !(flags & ITC_MBOX_SHARED)) {
                ITC_ERROR(0, "Thread already has mailbox", 0);
                return ITC_NO_ID;
        }

        ITC_TRACE(itc_create_mailbox_enter, name, flags);

        new_mbox = q_dequeue(itc_inst.freemboxes);
        if(new_mbox == NULL) {
                /* Out of mailboxes return error */
                ITC_ERROR(0, "Out of mailboxes", 0);
                return ITC_NO_ID;
        }

        if(strlen(name) > (ITC_NAME_MAXLEN)) {
                /* To long mbox name, return error */
                ITC_ERROR(0, "Mailbox name too long", 0);
                return ITC_NO_ID;
        }

#ifndef ITC_STAT_MBOXID
        if(flags & ITC_MBOX_STAT_ID_FLAG) {
                ITC_ERROR(0, "Static mbox ids not configured for ITC", 0);
                return ITC_NO_ID;
        }
#endif

        new_mbox->next        = NULL;
        new_mbox->parent_mbox = NULL;
        strcpy(new_mbox->name, name);

        new_mbox->rxq = &new_mbox->rxq_data;

        new_mbox->rxq->rx_qlen = 0;
        new_mbox->rxq->in_rx   = 0;
        new_mbox->rxq->filter  = NULL;
        new_mbox->flags        = flags;

        MUTEX_LOCK(&(new_mbox->rxq->rxq_m));

        if(flags == ITC_MBOX_SHARED) {
                new_mbox->state       = MBOX_SHARED;
		new_mbox->tid         = 0;
        } else {
                new_mbox->state       = MBOX_INUSE;
		new_mbox->tid         = gettid();
        }

#ifdef ITC_STATS
        /* Zero the mailbox statistic counters */
        memset(&new_mbox->stats, 0, sizeof(struct itc_stats));
#endif
        ret = pthread_mutex_init(&new_mbox->locate_lock, NULL);
        if(ret != 0) {
                /* Mutex init failure, report error here */
                MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread mutex init error: %d(%s)",
                          ret, strerror(ret));
                return ITC_NO_ID;
        }

        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_create_mailbox != NULL) {
                        if(transp_funcs[i].itci_create_mailbox(new_mbox, flags) < 0) {
                                MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));
                                ITC_ERROR(0, "create mailbox gets error from transport type %d", i);
                                return ITC_NO_ID;
                        }
                }
        }

        /* Add destructor to key */
        ret = pthread_setspecific(itc_inst.destruct_key, new_mbox);
        if(ret != 0) {
                MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread set destructor error",
                          ret, strerror(ret));
                return ITC_NO_ID;
        }

        /* Initialise monitor structures */
        new_mbox->monitors = (struct monitor_mbox_record *)
                             itc_alloc(sizeof(struct monitor_mbox_record), 0);
        memset(new_mbox->monitors, 0, sizeof(struct monitor_mbox_record));
        LL_NEW(struct monitor_mbox_record, new_mbox->monitors);
        ret = pthread_mutex_init(&new_mbox->monitors_m, NULL);
        if(ret != 0) {
                MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread mutex init error",
                          ret, strerror(ret));
                return ITC_NO_ID;
        }

        new_mbox->mymonitors = (struct monitor_mbox_record *)
                               itc_alloc(sizeof(struct monitor_mbox_record), 0);
        memset(new_mbox->mymonitors, 0, sizeof(struct monitor_mbox_record));
        LL_NEW(struct monitor_mbox_record, new_mbox->mymonitors);
        ret = pthread_mutex_init(&new_mbox->mymonitors_m, NULL);
        if(ret != 0) {
                MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread mutex init error",
                          ret, strerror(ret));
                return ITC_NO_ID;
        }

        if(insert_locate(&itc_inst.local_locate_tree,
                         &itc_inst.local_locate_tree_m, new_mbox) != 0) {
                ITC_TRACE_ERROR("Locate already exists in tree", 0);
//                return ITC_NO_ID;
        }

        if(new_mbox->state == MBOX_INUSE) {
                mymailbox = new_mbox;
        }

#ifdef ITC_STAT_MBOXID
        if(flags & ITC_MBOX_STAT_ID_FLAG) {
                struct itc_mailbox *tmp;

                tmp = itc_inst.statmboxarray[ITC_MBOX_GET_STAT_ID(flags)];
                if(tmp != NULL) {
                        ITC_ERROR(0, "Static id already busy", 0);
                        return ITC_NO_ID;
                }
                itc_inst.statmboxarray[ITC_MBOX_GET_STAT_ID(flags)] = new_mbox;
        }
#endif

        MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));

        /* If I am not world send notification */
        if(itc_inst.my_world_id != (itc_inst.world_mbox_id & itc_inst.world_mask)) {
                /* Notify world of my existance */
                if(itc_inst.name_space == NULL ||
                        flags & ITC_MBOX_NO_NS) {
                        msg = itc_alloc((sizeof(struct itc_add_rem_mailbox) + strlen(name)),
                                        ITC_ADD_MBOX);
                        msg->itc_add_rem_mailbox.mbox_id = new_mbox->mbox_id;
                        strcpy(msg->itc_add_rem_mailbox.mbox_name, name);
                        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
                } else {
                        msg = itc_alloc((sizeof(struct itc_add_rem_mailbox) +
                                         strlen(itc_inst.name_space) +
                                         strlen(name)),
                                        ITC_ADD_MBOX);
                        msg->itc_add_rem_mailbox.mbox_id = new_mbox->mbox_id;
                        strcpy(msg->itc_add_rem_mailbox.mbox_name, itc_inst.name_space);
                        strcat(msg->itc_add_rem_mailbox.mbox_name, name);
                        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
                }
        }


        ITC_TRACE(itc_create_mailbox_exit, new_mbox->name, flags,
                  new_mbox->mbox_id);

        return new_mbox->mbox_id;
}

/* ===================================================================== */
/**
 *   Clone ITC mailbox.
 *
 *   @param mbos_id    Mailbox ID that shall be cloned.
 *
 *   @param name       Name of the cloned mailbox.
 *
 *   @return           Mailbox id of the new mailbox clone.
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
itc_mbox_id_t __itc_clone_mailbox(itc_mbox_id_t mbox_id,
                                  const char *name,
                                  const char *file,
                                  int line)
{
        struct itc_mailbox *new_mbox, *tmp, *parent_mbox;
        int i, ret;
        union itc_msg *msg;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        parent_mbox = find_mbox(mbox_id);
	mbox_id = CVT_MBOX_ID(mbox_id);
        if(parent_mbox == NULL) {
                ITC_ERROR(0, "Failed to find parent mailbox: 0x%x", mbox_id);
                return ITC_NO_ID;
        } else if(parent_mbox->state == MBOX_CLONE ||
		parent_mbox->state == MBOX_DELETED ||
		mbox_id != parent_mbox->mbox_id) {
                ITC_ERROR(0, "Parent mailbox has incorrect state, id: 0x%x state: %d",
			  mbox_id, parent_mbox->state);
                return ITC_NO_ID;
        }

        new_mbox = q_dequeue(itc_inst.freemboxes);
        if(new_mbox == NULL) {
                /* Out of mailboxes return error */
                ITC_ERROR(0, "Out of mailboxes", 0);
                return ITC_NO_ID;
        }

        if(strlen(name) > (ITC_NAME_MAXLEN)) {
                /* To long mbox name, return error */
                ITC_ERROR(0, "Mailbox name too long", 0);
                return ITC_NO_ID;
        }

        strcpy(new_mbox->name, name);

#ifdef ITC_STATS
        /* Zero the mailbox statistic counters */
        memset(&new_mbox->stats, 0, sizeof(struct itc_stats));
#endif

        new_mbox->rxq = &parent_mbox->rxq_data;

        /* Insert mailbox clone in list in threads mailbox */
        tmp = parent_mbox;
        while(tmp->next != NULL) {
                tmp = tmp->next;
        }
        new_mbox->next = NULL;
        tmp->next = new_mbox;

        MUTEX_LOCK(&(new_mbox->rxq->rxq_m));

        new_mbox->parent_mbox = parent_mbox;
        new_mbox->state = MBOX_CLONE;

        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_create_mailbox != NULL) {
                        if(transp_funcs[i].itci_create_mailbox(new_mbox, ITC_CREATE_CLONE) < 0) {
                                MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));
                                ITC_ERROR(0, "create mailbox gets error from transport type %d", i);
                                return ITC_NO_ID;
                        }
                }
        }

        /* Initialise monitor structures, only monitord from others needed
           because a clone will never initiate any monitors */
        new_mbox->monitors = (struct monitor_mbox_record *)
                             itc_alloc(sizeof(struct monitor_mbox_record), 0);
        memset(new_mbox->monitors, 0, sizeof(struct monitor_mbox_record));
        LL_NEW(struct monitor_mbox_record, new_mbox->monitors);
        ret = pthread_mutex_init(&new_mbox->monitors_m, NULL);
        if(ret != 0) {
                MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));
                ITC_ERROR(ITC_ERROR_FATAL, "Pthread mutex init error",
                          ret, strerror(ret));
                return ITC_NO_ID;
        }
        new_mbox->mymonitors = NULL;

        if(insert_locate(&itc_inst.local_locate_tree,
                         &itc_inst.local_locate_tree_m, new_mbox) != 0) {
                ITC_TRACE_ERROR("Locate already exists in tree", 0);
//                return ITC_NO_ID;
        }

        MUTEX_UNLOCK(&(new_mbox->rxq->rxq_m));

        /* If I am not world send notification */
        if(itc_inst.my_world_id != (itc_inst.world_mbox_id & itc_inst.world_mask)) {
                /* Notify world of my existance */
                if(itc_inst.name_space == NULL ||
                   parent_mbox->flags) {
                        msg = itc_alloc((sizeof(struct itc_add_rem_mailbox) + strlen(name)),
                                        ITC_ADD_MBOX);
                        msg->itc_add_rem_mailbox.mbox_id = new_mbox->mbox_id;
                        strcpy(msg->itc_add_rem_mailbox.mbox_name, name);
                        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
                } else {
                        msg = itc_alloc((sizeof(struct itc_add_rem_mailbox) +
                                         strlen(itc_inst.name_space) +
                                         strlen(name)),
                                        ITC_ADD_MBOX);
                        msg->itc_add_rem_mailbox.mbox_id = new_mbox->mbox_id;
                        strcpy(msg->itc_add_rem_mailbox.mbox_name, itc_inst.name_space);
                        strcat(msg->itc_add_rem_mailbox.mbox_name, name);
                        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
                }
        }

        ITC_TRACE(itc_clone_mailbox, name, new_mbox->mbox_id);

        return new_mbox->mbox_id;
}

/* ===================================================================== */
/**
 *   Delete ITC mailbox.
 *
 *   @param mbox_id    Mail box id of the mailbox to be deleted.
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_delete_mailbox(itc_mbox_id_t mbox_id, const char *file, int line)
{
        struct itc_mailbox *mbox;
        struct itc_message *mon_messages;
        union itc_msg *msg;
        int res;
        mbox_state mb_state;
        bool mbox_clone  = false;
        bool mbox_shared = false;
	pthread_mutex_t *rxq_m;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        ITC_TRACE(itc_delete_mailbox, mbox_id);

        mbox = find_mbox(mbox_id);
	mbox_id = CVT_MBOX_ID(mbox_id);
        if(mbox == NULL) {
                ITC_ERROR(0, "Delete of non local mailbox", 0);
                return;
        }

	/* We need to lock the monitor mutex while changing state on
	   the mailbox to deleted to ensure the consistency of the
	   monitor list.*/
	MUTEX_LOCK(&mbox->monitors_m);
	rxq_m = &(mbox->rxq->rxq_m);
	res = pthread_mutex_lock(rxq_m);
	if(res != 0 &&
	   res != EDEADLK) {
		ITC_ERROR(ITC_ERROR_FATAL,
			  "Pthread_mutex_lock failed %d, errno: %d(%s)",
			  res, errno, strerror(errno));
		return;
	}
        /* Start to delete mailbox */
        mb_state = mbox->state;
	if(mb_state == MBOX_CLONE) {
		/* For cloned mailboxes we need to take both the cloned rxq
		   mutex and the parent rxq mutex. To avoid deadlocks with
		   send we need to take them in the appropriate order hence
		   we release the parent rxq mutex first.*/
		MUTEX_UNLOCK(rxq_m);
		MUTEX_LOCK(&(mbox->rxq_data.rxq_m));
		MUTEX_LOCK(rxq_m);
		mbox->state = MBOX_DELETED;
		MUTEX_UNLOCK(&(mbox->rxq_data.rxq_m));
	} else {
		mbox->state = MBOX_DELETED;
	}
	MUTEX_UNLOCK(rxq_m);
	MUTEX_UNLOCK(&mbox->monitors_m);

        if(mymailbox == NULL) {
                ITC_ERROR(0, "Delete mailbox has no mailbox", 0);
                return;
        } else if(mb_state == MBOX_INUSE &&
                  mbox->mbox_id != mymailbox->mbox_id) {
                ITC_ERROR(0, "Delete of anothers mailbox", 0);
                mbox->state = mb_state;
                return;
#ifdef ITC_STAT_MBOXID
        } else if(mb_state == MBOX_UNUSED ||
                  mb_state == MBOX_DELETED ||
                  (mbox_id >= MAX_STAT_ID &&
                   mbox->mbox_id != mbox_id)) {
#else
        } else if(mb_state == MBOX_UNUSED ||
                  mb_state == MBOX_DELETED ||
                  mbox->mbox_id != mbox_id) {
#endif
                ITC_ERROR(0, "Mbox deleted or not created", 0);
                mbox->state = mb_state;
                return;
        } else if(mb_state == MBOX_CLONE) {
                mbox_clone = true;
        } else if(mb_state == MBOX_SHARED) {
                mbox_shared = true;
        }

	if(!mbox_clone &&
	   mbox->rxq->has_fd) {
		res = close(mbox->rxq->rxfd);
		if(res == -1) {
			ITC_ERROR(ITC_ERROR_FATAL, "close error errno: %d(%s)",
				  errno, strerror(errno));
		}
		mbox->rxq->has_fd = 0;
	}

	MUTEX_LOCK(rxq_m);

        if(mbox_clone) {
                struct itc_mailbox *prev = NULL;
                struct itc_mailbox *tmp = mbox->parent_mbox->next;

                while(tmp != NULL) {
                        if(tmp->mbox_id == mbox_id) {
                                if(prev == NULL) {
                                        mbox->parent_mbox->next = tmp->next;
                                } else {
                                        prev->next = tmp->next;
                                }
                                break;
                        }
                        prev = tmp;
                        tmp = tmp->next;
                }
                mbox->next = NULL;
        } else {
                struct itc_mailbox *tmp, *next;
		struct itc_message *tmpmessage;
		union itc_msg *tmpmsg;

		/* Delete all cloned mailboxes */
		tmp = mbox->next;
		while(tmp != NULL) {
			next = tmp->next;

			/* We need to lock the monitor mutex while changing state on
			   the mailbox to deleted to ensure the consistency of the
			   monitor list.*/
			MUTEX_LOCK(&tmp->monitors_m);
			tmp->state = MBOX_DELETED;
			MUTEX_UNLOCK(&tmp->monitors_m);
			tmpmessage = delete_mailbox(tmp, file, line);
			while(tmpmessage != NULL) {
				tmpmsg = MESSAGE_TO_MSG(tmpmessage);
				tmpmessage = tmpmessage->next;
				itc_send(&tmpmsg, itc_receiver(tmpmsg),
					 itc_sender(tmpmsg));
			}
                        tmp = next;
                }
                mbox->next = NULL;
        }

	mon_messages = delete_mailbox(mbox, file, line);

	MUTEX_UNLOCK(rxq_m);

        if(!mbox_clone &&
           !mbox_shared) {
                mymailbox = NULL;
        }

        while(mon_messages != NULL) {
                msg = MESSAGE_TO_MSG(mon_messages);
                mon_messages = mon_messages->next;
                itc_send(&msg, itc_receiver(msg), itc_sender(msg));
        }

        return;
}

/* ===================================================================== */
/**
 *   Get current ITC mailbox.
 *
 *   @return           Mailbox id of the current threads mailbox or
 *                     ITC_NO_ID if the current thread has no mailbox.
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
itc_mbox_id_t __itc_current_mbox(const char *file,
                                 int line)
{
        if(mymailbox == NULL) {
                return ITC_NO_ID;
        }

        return mymailbox->mbox_id;
}

/* ===================================================================== */
/**
 *   Add name to mailbox
 *
 *   @param mbox_id    Mailbox ID that shall have name added to it.
 *
 *   @param name       Name to be added to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
int __itc_add_name(itc_mbox_id_t mbox_id,
                   const char *name,
                   const char *file,
                   int line)

{
        union itc_msg *msg;
        itc_mbox_id_t tmp_id;

        ITC_TRACE(itc_add_name, mbox_id, name);

        if(mbox_id == ITC_MY_MBOX) {
                if(mymailbox == NULL) {
                        ITC_ERROR(0, "Thread is missing mailbox", 0);
                        return -1;
                } else {
                        tmp_id = mymailbox->mbox_id;
                }
        } else {
                tmp_id = mbox_id;
        }

        /* Notify world of the name */
        msg = itc_alloc((sizeof(struct itc_add_rem_mailbox) + strlen(name)),
                        ITC_ADD_NAME);
        msg->itc_add_rem_mailbox.mbox_id = tmp_id;
        strcpy(msg->itc_add_rem_mailbox.mbox_name, name);
        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);

        return 0;
}

/* ===================================================================== */
/**
 *   Locate ITC mailbox.
 *
 *   @param name       Name of the mailbox to be located.
 *
 *   @return           Mailbox id of the located mailbox or ITC_NO_ID if
 *                     no mailbox has been found.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
itc_mbox_id_t __itc_locate(const char *name, const char *file, int line)
{
        itc_mbox_id_t mbox_id = ITC_NO_ID;
        struct itc_mailbox *mbox;
        union itc_msg *msg;
        uint32_t locate_repl[] = { 1, ITC_LOCATE_REPL };

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        mbox = get_locate(name);

        if(mbox != NULL) {
                return mbox->mbox_id;
        }

        /* Not found locally send on to world */
        msg = itc_alloc((sizeof(struct itc_locate) + strlen(name)),
                        ITC_LOCATE);
        strcpy(msg->itc_locate.mbox_name, name);
        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(locate_repl, ITC_NO_TMO, ITC_FROM_ALL);
        mbox_id = msg->itc_locate_repl.mbox_id;

        itc_free(&msg);

        INC_STAT(mymailbox, locates);

        ITC_TRACE(itc_locate, name, mbox_id);

        return mbox_id;
}

/* ===================================================================== */
/**
 *   Locate ITC mailbox asynchronously.
 *
 *   @param name       Name of the mailbox to be located.
 *
 *   @param msg        Pointer to message to be returned when the mailbox
 *                     is found. If msg is NULL a message with message
 *                     number ITC_LOCATE_DEFAULT_NO will be returned.
 *
 *   @param from       Mailbox id that shall receive the locate message
 *                     when the mailbox has been located. Set from to
 *                     ITC_MY_MBOX to get it to your own mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_locate_async(const char *name,
                        union itc_msg **msg,
                        itc_mbox_id_t from,
                        const char *file,
                        int line)
{
        struct itc_mailbox *mbox, *from_mbox;
        union itc_msg *tx_msg, *loc_msg;
        struct itc_message *message;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        ITC_TRACE(itc_locate_async, name,
                  (unsigned long)(msg ? *msg : NULL));

        if(mymailbox == NULL ||
           mymailbox->state != MBOX_INUSE) {
                ITC_ERROR(0, "Own mailbox not created error", 0);
                return;
        }

	from_mbox = find_mbox(from);
	from = CVT_MBOX_ID(from);
        if(from_mbox == NULL) {
                ITC_ERROR(0, "itc_locate_async from illegal mailbox ID, mailbox in another process?",
                          0);
                return;
        } else if (from_mbox->state != MBOX_INUSE  &&
                   from_mbox->state != MBOX_CLONE  &&
                   from_mbox->state != MBOX_SHARED &&
                   from_mbox->mbox_id != from) {
                ITC_ERROR(0, "itc_locate_async from mailbox which is in an incorrect state",
                          0);
                return;
        }

        INC_STAT(from_mbox, locates);

        if(msg == NULL) {
                loc_msg = itc_alloc(sizeof(uint32_t), ITC_LOCATE_DEFAULT_NO);
        } else {
                loc_msg = *msg;
                *msg = NULL;
        }

        mbox = get_locate(name);

        if(mbox != NULL) {
                ITC_TRACE(itc_send_locate_found, mbox->mbox_id,
                          from_mbox->mbox_id, (unsigned long)loc_msg,
                          loc_msg->msgno);
                itc_send(&loc_msg, from_mbox->mbox_id, mbox->mbox_id);
                return;
        }

        /* Not found locally send on to world */
        message = MSG_TO_MESSAGE(loc_msg);

        MUTEX_LOCK(&from_mbox->locate_lock);
        message->next = from_mbox->pending_locate;
        from_mbox->pending_locate = message;
        INC_STAT(from_mbox, pend_locates);
        MUTEX_UNLOCK(&from_mbox->locate_lock);

        tx_msg = itc_alloc((sizeof(struct itc_locate_async) + strlen(name)),
                           ITC_LOCATE_ASYNC);
        tx_msg->itc_locate_async.from_mbox = from_mbox->mbox_id;
        tx_msg->itc_locate_async.data      = message;
        strcpy(tx_msg->itc_locate_async.mbox_name, name);
        itc_send(&tx_msg, itc_inst.world_mbox_id, itc_inst.local_coord_mbox_id);

        return;
}

/* ===================================================================== */
/**
 *   Get file descriptor of ITC mailbox.
 *
 *   @return           File descriptor of the current threads mailbox.
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
int __itc_get_fd(const char *file, int line)
{
        struct itc_mailbox *mbox = mymailbox;
        uint64_t one = 1;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mbox == NULL) {
                ITC_ERROR(0, "No local mailbox created in itc_get_fd", 0);
                return -1;
        }

        if(!mbox->rxq->has_fd) {
                mbox->rxq->rxfd = eventfd(0, 0);
                if(mbox->rxq->rxfd == -1) {
                        ITC_ERROR(ITC_ERROR_FATAL, "eventfd returns failure: %d(%s)",
                                  errno, strerror(errno));
                        return -1;
                }
                mbox->rxq->has_fd = 1;
                if(mbox->rxq->rx_qlen != 0) {
                        if(write(mbox->rxq->rxfd, &one, 8) < 0) {
                                ITC_ERROR(ITC_ERROR_FATAL, "Failed to write to eventfd: %d(%s)",
                                          errno, strerror(errno));
                                return -1;
                        }
                }
        }

        return mbox->rxq->rxfd;
}

/* ===================================================================== */
/**
 *   Get name of ITC mailbox.
 *
 *   @param mbox_id      Mailbox id to get name for.
 *
 *   @param name         Pointer to string where mailbox name shall
 *                       be stored. The returned name will be '\0'
 *                       terminated.
 *
 *   @param name_len     How many bytes can be stored in the name string.
 *                       If there is not sufficient room the name will be
 *                       cut and only the beginning of the name present.
 *
 *   @return             true (non 0) if the name for the mailbox has
 *                       been found and false (0) otherwise.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
int32_t __itc_get_name(itc_mbox_id_t mbox_id,
                       char *name,
                       uint32_t name_len,
                       const char *file,
                       int line)
{
        struct itc_mailbox *mbox;
        int32_t found = 0;
        union itc_msg *msg;
        uint32_t get_name_repl[] = { 1, ITC_GET_NAME_REPL };

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        mbox = find_mbox(mbox_id);
	mbox_id = CVT_MBOX_ID(mbox_id);

#ifdef ITC_STAT_MBOXID
        if(mbox != NULL &&
           (mbox_id < MAX_STAT_ID ||
            mbox->mbox_id == mbox_id)) {
#else
        if(mbox != NULL &&
           mbox->mbox_id == mbox_id) {
#endif
                if(mbox->state == MBOX_INUSE  ||
                   mbox->state == MBOX_SHARED ||
                   mbox->state == MBOX_CLONE) {
                        if(strlen(mbox->name) >= name_len) {
                                strncpy(name, mbox->name, name_len);
                                name[name_len - 1] = '\0';
                                found = 1;
                        } else {
                                strcpy(name, mbox->name);
                                found = 1;
                        }
                } else {
                        strcpy(name, "");
                }
        } else {
                msg = itc_alloc(sizeof(struct itc_get_name),
                                ITC_GET_NAME);
                msg->itc_get_name.mbox_id = mbox_id;
                itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
                msg = itc_receive(get_name_repl, ITC_NO_TMO, ITC_FROM_ALL);
                found = msg->itc_get_name_repl.found;
                if(found) {
                        if(strlen(msg->itc_get_name_repl.mbox_name) >= name_len) {
	                        strncpy(name, msg->itc_get_name_repl.mbox_name,
	                                name_len);
                                name[name_len - 1] = '\0';
                        } else {
	                        strcpy(name, msg->itc_get_name_repl.mbox_name);
                        }
                } else {
                        strcpy(name, "");
                }
                itc_free(&msg);
        }

        ITC_TRACE(itc_getname, mbox_id, name, name_len, found);

        return found;
}

/* ===================================================================== */
/**
 *   Get real mailbox id for static mailbox id.
 *
 *   @param mbox_id      Mailbox id to get real id for.
 *
 *   @return             Real mailbox id.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
itc_mbox_id_t __itc_get_real_mbox(itc_mbox_id_t mbox_id,
                                  const char *file,
                                  int line)
{
#ifdef ITC_STAT_MBOXID
        itc_mbox_id_t ret_id;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mbox_id == 0 ||
           mbox_id >= MAX_STAT_ID) {
                ITC_ERROR(0, "Incorrect static ID given in itc_get_real_mailbox", 0);
        }

        if(itc_inst.statmboxarray[mbox_id] != NULL) {
                ret_id = itc_inst.statmboxarray[mbox_id]->mbox_id;
        } else {
                ret_id = ITC_NO_ID;
        }

        return ret_id;
#else
        ITC_ERROR(0, "Trying to get real mailbox id for static id when not supported by itc", 0);
        return ITC_NO_ID;
#endif
}

/* ===================================================================== */
/**
 *   Send ITC message
 *
 *   @param msg        Pointer to ITC message pointer that shall be sent.
 *
 *   @param to         Mailbox that message shall be sent to.
 *
 *   @param from       Mailbox id that shall be set as ITC message sender.
 *                     For my own mailbox set ITC_MY_MBOX as from.
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_send(union itc_msg **msg,
                itc_mbox_id_t to,
                itc_mbox_id_t from,
                const char *file,
                int line)
{
        struct itc_message *message;
        struct itc_mailbox *from_mbox, *to_mbox;
        uint64_t one = 1;
        int i, result;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if( msg == NULL ||
           *msg == NULL) {
                ITC_ERROR(0, "Trying to send NULL pointer", 0);
                return;
        }

	if(to == 0 ||
	   from == 0) {
                ITC_ERROR(0, "Trying to send %s mailbox id 0",
			  (to == 0) ? "to" : "from");
                return;
        }

        ITC_TRACE(itc_send, (unsigned long)*msg, (*msg)->msgno, to, from);

        message = MSG_TO_MESSAGE(*msg);
	from_mbox = find_mbox(from);
        if((from == ITC_MY_MBOX) && (mymailbox == NULL)) {
                ITC_ERROR(0, "Own mailbox not created error", 0);
                return;
        }
	from = CVT_MBOX_ID(from);
        /* When we use from in send we try to locate the local from mailbox
           and use it to send. If the mailbox is not local we need to use the
           thread mailbox own mymailbox and set the sender as from. */

	if(from_mbox == NULL) {
		if(mymailbox != NULL) {
			from_mbox = mymailbox;
		} else {
			ITC_ERROR(0, "From mailbox not local and mymailbox not created error", 0);
			return;
		}
#ifdef ITC_STAT_MBOXID
		if(from < MAX_STAT_ID) {
			ITC_ERROR(0, "Send from not configured static ID", 0);
			return;
		}
	} else {
		if(from < MAX_STAT_ID) {
			from = from_mbox->mbox_id;
		}
#endif
	}
	message->sender = from;

        if((to_mbox = find_mbox(to)) != NULL) {
		to = CVT_MBOX_ID(to);
                if(to_mbox->state == MBOX_UNUSED) {
                        ITC_ERROR(0, "Send to unused mailbox", 0);
                        return;
#ifdef ITC_STAT_MBOXID
                } else if(to_mbox->state == MBOX_DELETED ||
                          (to >= MAX_STAT_ID &&
                           to != to_mbox->mbox_id)) {
#else
                } else if(to_mbox->state == MBOX_DELETED ||
                          to != to_mbox->mbox_id) {
#endif
                        ITC_TRACE(itc_send_to_deleted, from, to);
			itc_free(msg);
                        return;
                }

                MUTEX_LOCK(&(to_mbox->rxq_data.rxq_m));
                if(to_mbox->state == MBOX_INUSE ||
                   to_mbox->state == MBOX_SHARED) {
                } else if (to_mbox->state == MBOX_CLONE) {
                        MUTEX_LOCK(&(to_mbox->rxq->rxq_m));
                        MUTEX_UNLOCK(&(to_mbox->rxq_data.rxq_m));
                } else if(to_mbox->state == MBOX_DELETED) {
                        MUTEX_UNLOCK(&(to_mbox->rxq_data.rxq_m));
			itc_free(msg);
                        return;
                } else {
                        MUTEX_UNLOCK(&(to_mbox->rxq_data.rxq_m));
                        ITC_ERROR(0, "send on unused mailbox", 0);
                }
        } else {
#ifdef ITC_STAT_MBOXID
		if(to < MAX_STAT_ID) {
                        ITC_ERROR(0, "Send to not configured static ID", 0);
			return;
		}
#endif
	}

#ifdef ITC_STAT_MBOXID
        /* If a static ID is used we need to update to to
           reflect the real mailbox id of the to mailbox. */
        if(to_mbox != NULL &&
	   to < MAX_STAT_ID) {
                to = to_mbox->mbox_id;
        }
#endif
	message->receiver = to;

        if(itc_inst.txhook != NULL) {
                if(itc_inst.txhook(itc_inst.txuser, from, to,
                                   message->size, *msg) == ITC_DROP) {
                        if(to_mbox != NULL) {
                                MUTEX_UNLOCK(&(to_mbox->rxq->rxq_m));
                        }
                        return;
                }
        }

        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_send != NULL) {
                        result = transp_funcs[i].itci_send(from_mbox, message,
                                                           to, from);
                        if(result == 0) {
                                break;
                        } else if(result < 0) {
                                /* Send failed in transport function */
                                if(to_mbox != NULL) {
                                        MUTEX_UNLOCK(&(to_mbox->rxq->rxq_m));
                                }
                                ITC_ERROR(0, "Send failed in transport function %d", i);
                        }
                }
        }

        if(i == ITC_NUM_TRANSPORTS) {
                if(to_mbox != NULL) {
                        MUTEX_UNLOCK(&(to_mbox->rxq->rxq_m));
                }
                ITC_ERROR(0, "Message not sent by any transport function from: 0x%08x to: 0x%08x",
			  from, to);
        }

        INC_STAT(from_mbox, tx_msg);

        if(to_mbox != NULL) {
                if(to_mbox->rxq != NULL &&
		   to_mbox->rxq->has_fd &&
                   to_mbox->rxq->rx_qlen == 0) {
                        if(write(to_mbox->rxq->rxfd, &one, 8) < 0) {
                                ITC_ERROR(ITC_ERROR_FATAL, "Failed to write to pipe",
                                          errno, strerror(errno));
                                /* Will not return here */
                        }
                }
                to_mbox->rxq->rx_qlen++;
                pthread_cond_signal(&(to_mbox->rxq->rxq_c));
                MUTEX_UNLOCK(&(to_mbox->rxq->rxq_m));
        }

        *msg = NULL;

        return;
}

/* ===================================================================== */
/**
 *   Receive messages from your ITC mailbox.
 *
 *   @param filter     Which message numbers you wish to receive.
 *                     Set to ITC_NOFILTER to get all messages.
 *
 *   @param tmo        How many milliseconds the itc_receive shall block
 *                     and wait for messages. Use 0 to get a non blocking
 *                     call and ITC_NO_TMO to block forever.
 *
 *   @param from       Mailbox id that messages shall be received from,
 *                     use ITC_FROM_ALL to receive all incoming messages.
 *
 *   @return           Pointer to received message from ITC mailbox or
 *                     NULL if no message was received.
 */
/* ===================================================================== */
union itc_msg *__itc_receive(const uint32_t *filter,
                             int32_t tmo,
                             itc_mbox_id_t from,
                             const char *file,
                             int line)
{
        struct itc_message *message = NULL;
        struct itc_mailbox *mbox = mymailbox;
        int i, ret;
        bool recursive_rx = false;
	struct timespec ts;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

#ifdef ITC_LTTNG
        if(filter == NULL) {
                ITC_TRACE(itc_receive_enter, 0, tmo, from);
        } else {
                ITC_TRACE(itc_receive_enter, filter[0], tmo, from);
        }
#endif
        if(mbox == NULL ||
           mbox->state != MBOX_INUSE) {
                ITC_ERROR(0, "Own mailbox not created error", 0);
                return NULL;
        }

	if((tmo != ITC_NO_TMO) &&
	    (tmo != 0)) {
		calc_abs_time(&ts, tmo);
	}

        do {
                MUTEX_LOCK(&(mbox->rxq->rxq_m));

                mbox->rxq->in_rx = 1;
                mbox->rxq->filter = (uint32_t *)filter;

                for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                        if(transp_funcs[i].itci_receive != NULL) {
                                message = transp_funcs[i].itci_receive(mbox,
                                                                       filter, tmo, from,
                                                                       recursive_rx);
                        }
                }

                if(message == NULL) {
                        if(tmo == 0) {
                                mbox->rxq->in_rx = 0;
                                mbox->rxq->filter = NULL;
                                MUTEX_UNLOCK(&(mbox->rxq->rxq_m));
                                break;
                        } else if(tmo == ITC_NO_TMO) {

                                ret = pthread_cond_wait(&(mbox->rxq->rxq_c),
                                                        &(mbox->rxq->rxq_m));
                                if(ret != 0) {
                                        /* Condition variable failure, ERROR here */
                                        ITC_ERROR(ITC_ERROR_FATAL,
                                                  "Pthread cond wait error: %d(%s)",
                                                  ret, strerror(ret));
                                        /* Will not return here restarted
                                           by fatal ITC_ERROR*/
                                }
                        } else {
                                ret = pthread_cond_timedwait(&(mbox->rxq->rxq_c),
                                                             &(mbox->rxq->rxq_m),
                                                             &ts);
                                if(ret == ETIMEDOUT) {
                                        mbox->rxq->in_rx = 0;
                                        mbox->rxq->filter = NULL;
                                        MUTEX_UNLOCK(&(mbox->rxq->rxq_m));
                                        break;
                                } else if(ret != 0) {
                                        /* Condition variable failure, ERROR here */
                                        ITC_ERROR(ITC_ERROR_FATAL,
                                                  "Pthread cond timedwait error: %d(%s)",
                                                  ret, strerror(ret));
                                        /* Will not return here restarted
                                           by fatal ITC_ERROR*/
                                }
                        }
                } else {
                        mbox->rxq->rx_qlen--;
                        if(mbox->rxq->has_fd &&
                           mbox->rxq->rx_qlen == 0) {
                                char buf[8];

                                if(read(mbox->rxq->rxfd, &buf, 8) < 0) {
                                        ITC_ERROR(ITC_ERROR_FATAL,
                                                  "Read from pipi failed",
                                                  errno, strerror(errno));
                                        /* Will not return here restarted
                                           by fatal ITC_ERROR*/
                                }
                        }
                }

                mbox->rxq->in_rx = 0;
                mbox->rxq->filter = NULL;

                MUTEX_UNLOCK(&(mbox->rxq->rxq_m));

                recursive_rx = true;

                if(message != NULL &&
                   itc_inst.rxhook != NULL) {
                        union itc_msg *msg = MESSAGE_TO_MSG(message);
                        if(itc_inst.rxhook(itc_inst.rxuser, itc_sender(msg),
                                           itc_receiver(msg), itc_size(msg),
                                           msg) == ITC_DROP) {
                                message = NULL;
                        }
                }

        } while(message == NULL);

#ifdef ITC_STATS
        INC_STAT(mbox, rx_msg);
        if(message == NULL) {
                INC_STAT(mbox, rx_tmo);
        } else {
                INC_STAT(mbox, rx_wmsg);
        }
#endif

#ifdef ITC_LTTNG
        if(message == NULL) {
                ITC_TRACE(itc_receive_exit_empty, tmo,
                          from, (unsigned long)NULL);
        } else {
                union itc_msg *msg = MESSAGE_TO_MSG(message);
                ITC_TRACE(itc_receive_exit, tmo, from,
                          (unsigned long)msg, msg->msgno);
        }
#endif

        return (union itc_msg *)((message == NULL) ? NULL : MESSAGE_TO_MSG(message));
}

/* ===================================================================== */
/**
 *   Receive messages from specified ITC mailbox.
 *
 *   @param mbox_id    Mailbox ID of the shared receive queue.
 *
 *   @param filter     Which message numbers you wish to receive.
 *                     Set to ITC_NOFILTER to get all messages.
 *
 *   @param tmo        How many milliseconds the itc_receive shall block
 *                     and wait for messages. Use 0 to get a non blocking
 *                     call and ITC_NO_TMO to block forever.
 *
 *   @return           Pointer to received message from ITC mailbox or
 *                     NULL if no message was received.
 */
/* ===================================================================== */
union itc_msg *__itc_receive_mbox(itc_mbox_id_t mbox_id,
                                  const uint32_t *filter,
                                  int32_t tmo,
                                  const char *file,
                                  int line)
{
        struct itc_message *message = NULL;
        struct itc_mailbox *mbox;
        int i, ret;
        bool recursive_rx = false;
	struct timespec ts;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

#ifdef ITC_LTTNG
        if(filter == NULL) {
                ITC_TRACE(itc_receive_mbox_enter, mbox_id, 0, tmo);
        } else {
                ITC_TRACE(itc_receive_enter, mbox_id, filter[0], tmo);
        }
#endif
        mbox = find_mbox(mbox_id);
	mbox_id = CVT_MBOX_ID(mbox_id);
        if(mbox == NULL               ||
           mbox->state != MBOX_SHARED ||
           mbox->mbox_id != mbox_id) {
                ITC_ERROR(0, "Mailbox not created or not shared", 0);
                return NULL;
        }

	if((tmo != ITC_NO_TMO) &&
	   (tmo != 0)) {
		calc_abs_time(&ts, tmo);
	}

        do {
                MUTEX_LOCK(&(mbox->rxq->rxq_m));

                mbox->rxq->in_rx = 1;
                mbox->rxq->filter = (uint32_t *)filter;

                for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                        if(transp_funcs[i].itci_receive != NULL) {
                                message = transp_funcs[i].itci_receive(mbox,
                                                                       filter, tmo, ITC_FROM_ALL,
                                                                       recursive_rx);
                        }
                }

                if(message == NULL) {
                        if(tmo == 0) {
                                mbox->rxq->in_rx = 0;
                                mbox->rxq->filter = NULL;
                                MUTEX_UNLOCK(&(mbox->rxq->rxq_m));
                                break;
                        } else if(tmo == ITC_NO_TMO) {
                                ret = pthread_cond_wait(&(mbox->rxq->rxq_c),
                                                        &(mbox->rxq->rxq_m));
                                if(ret != 0) {
                                        /* Condition variable failure, ERROR here */
                                        ITC_ERROR(ITC_ERROR_FATAL,
                                                  "Pthread cond wait error",
                                                  ret, strerror(ret));
                                        /* Will not return here restarted
                                           by fatal ITC_ERROR*/
                               }
                        } else {
                                ret = pthread_cond_timedwait(&(mbox->rxq->rxq_c),
                                                                &(mbox->rxq->rxq_m),
                                                                &ts);
                                if(ret == ETIMEDOUT) {
                                        mbox->rxq->in_rx = 0;
                                        mbox->rxq->filter = NULL;
                                        MUTEX_UNLOCK(&(mbox->rxq->rxq_m));
                                        break;
                                } else if(ret != 0) {
                                        /* Condition variable failure, ERROR here */
                                        ITC_ERROR(ITC_ERROR_FATAL,
                                                  "Pthread cond timedwait error",
                                                  ret, strerror(ret));
                                        /* Will not return here restarted
                                           by fatal ITC_ERROR*/
                                }
                        }
                } else {
                        mbox->rxq->rx_qlen--;
                        if(mbox->rxq->has_fd &&
                           mbox->rxq->rx_qlen == 0) {
                                char buf[8];

                                if(read(mbox->rxq->rxfd, &buf, 8) < 0) {
                                        ITC_ERROR(ITC_ERROR_FATAL,
                                                  "Read from pipi failed",
                                                  errno, strerror(errno));
                                        /* Will not return here restarted
                                           by fatal ITC_ERROR*/
                                }
                        }
                }

                mbox->rxq->in_rx = 0;
                mbox->rxq->filter = NULL;

                MUTEX_UNLOCK(&(mbox->rxq->rxq_m));

                recursive_rx = true;

                if(message != NULL &&
                   itc_inst.rxhook != NULL) {
                        union itc_msg *msg = MESSAGE_TO_MSG(message);
                        if(itc_inst.rxhook(itc_inst.rxuser, itc_sender(msg),
                                           itc_receiver(msg), itc_size(msg),
                                           msg) == ITC_DROP) {
                                message = NULL;
                        }
                }

        } while(message == NULL);

#ifdef ITC_STATS
        INC_STAT(mbox, rx_msg);
        if(message == NULL) {
                INC_STAT(mbox, rx_tmo);
        } else {
                INC_STAT(mbox, rx_wmsg);
        }
#endif

#ifdef ITC_LTTNG
        if(message == NULL) {
                ITC_TRACE(itc_receive_mbox_exit_empty, mbox_id, tmo,
                          (unsigned long)NULL);
        } else {
                union itc_msg *msg = MESSAGE_TO_MSG(message);
                ITC_TRACE(itc_receive_mbox_exit, mbox_id, tmo,
                          (unsigned long)msg, msg->msgno);
        }
#endif

        return (union itc_msg *)((message == NULL) ? NULL : MESSAGE_TO_MSG(message));
}

/* ===================================================================== */
/**
 *   Remove ITC message
 *
 *   @param mbox_id    Mailbox that message shall be removed from.
 *
 *   @param msg        Pointer to ITC message pointer that shall
 *                     be removed.
 *
 *   @return           Message pointer that has been removed or NULL if
 *                     message not found in RX queue.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
union itc_msg *__itc_remove(itc_mbox_id_t mbox_id,
                             union itc_msg *msg,
                             const char *file,
                             int line)
{
        struct itc_mailbox *mbox;
        struct itc_message *message, *tmp = NULL;
        int i;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(msg == NULL) {
                ITC_ERROR(0, "Trying to remove a NULL pointer from a queue", 0);
                return NULL;
        }

        if(mbox_id == ITC_MY_MBOX) {
                if(mymailbox == NULL) {
                        ITC_ERROR(0, "Thread is missing mailbox", 0);
                        return NULL;
                } else {
                        mbox = mymailbox;
                }
        } else {
                mbox = find_mbox(mbox_id);
                if(mbox == NULL) {
                        ITC_ERROR(0, "Failed to find mailbox", 0);
                        return NULL;
                } else if(mbox->mbox_id != mbox_id) {
                        ITC_ERROR(0, "Trying to use a recycled mailbox", 0);
                        return NULL;
                }

        }

        if(mbox->state == MBOX_UNUSED ||
           mbox->state == MBOX_DELETED) {
                ITC_ERROR(0, "Trying to remove message from mailbox in incorrect state", 0);
                return NULL;
        }

        message = MSG_TO_MESSAGE(msg);

        MUTEX_LOCK(&(mbox->rxq->rxq_m));
        for(i=0 ; i<ITC_NUM_TRANSPORTS ; i++) {
                if(transp_funcs[i].itci_remove != NULL) {
                        tmp = transp_funcs[i].itci_remove(mbox, message);
                        if(tmp != 0) {
                                break;
                        }
                }
        }

        if(tmp != NULL) {
                mbox->rxq->rx_qlen--;
                if(mbox->rxq->has_fd &&
                   mbox->rxq->rx_qlen == 0) {
                        char buf[8];

                        if(read(mbox->rxq->rxfd, &buf, 8) < 0) {
                                ITC_ERROR(ITC_ERROR_FATAL,
                                          "Read from pipe failed",
                                          errno, strerror(errno));
                                /* Will not return here restarted
                                   by fatal ITC_ERROR*/
                        }
                }
        }
        MUTEX_UNLOCK(&(mbox->rxq->rxq_m));

        return (union itc_msg *)((tmp == NULL) ? NULL : MESSAGE_TO_MSG(tmp));
}

/* ===================================================================== */
/**
 *   Monitor the existence of a mailbox.
 *
 *   @param who        Mailbox id of the mailbox you wish to monitor.
 *
 *   @param msg        Message that you wish to get when the mailbox is
 *                     deleted. If msg is set to NULL you will get a
 *                     message with message number ITC_MONITOR_DEFAULT_NO
 *                     when the monitored mailbox is deleted.
 *
 *   @return           A monitor ID reference that can be used to
 *                     "unmonitor" the mailbox.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
itc_monitor_id_t __itc_monitor(itc_mbox_id_t who,
                               union itc_msg **msg,
                               const char *file,
                               int line)
{
        struct itc_mailbox *mbox;
        struct monitor_record *rec;
        struct monitor_mbox_record *mon_rec, *tar_rec;
        itc_monitor_id_t mon_id = ITC_NO_ID;
        union itc_msg *monmsg, *tmp;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mymailbox == NULL ||
           mymailbox->state != MBOX_INUSE) {
                ITC_ERROR(0, "Own mailbox not created error", 0);
                return ITC_NO_ID;
        }

        INC_STAT(mymailbox, mymonitors);

	if (who == ITC_MY_MBOX) {
                ITC_ERROR(ITC_ERROR_FATAL, "Attempt to monitor own mailbox", 0);
                /* Above error is fatal, will never return here! */
	}

        mbox = find_mbox(who);

        MUTEX_LOCK(&itc_inst.free_rec_m);
        LL_GETLAST(struct monitor_record,
                   itc_inst.free_rec, rec);
        if(rec == NULL) {
                (void)__add_monitors();
                LL_GETLAST(struct monitor_record,
                           itc_inst.free_rec, rec);
                MUTEX_UNLOCK(&itc_inst.free_rec_m);
                if (rec == NULL) {
                        ITC_ERROR(0, "Out of monitors error", 0);
                        return ITC_NO_ID;
                }
        } else {
                MUTEX_UNLOCK(&itc_inst.free_rec_m);
        }

        MUTEX_LOCK(&rec->lock_m);

        mon_rec = (struct monitor_mbox_record *)
                itc_alloc(sizeof(struct monitor_mbox_record), 0);
        memset(mon_rec, 0, sizeof(struct monitor_mbox_record));

        rec->monitor_entry  = mon_rec;
        rec->monitor_mbox   = mymailbox;
        if(msg == NULL) {
                rec->msg    = NULL;
        } else {
                rec->msg    = *msg;
        }
        rec->state          = MONITOR_INUSE;

        if(mbox != NULL) {
                MUTEX_LOCK(&mbox->monitors_m);
#ifdef ITC_STAT_MBOXID
                if(mbox->state == MBOX_UNUSED  ||
                   mbox->state == MBOX_DELETED ||
                   (who >= MAX_STAT_ID &&
                    mbox->mbox_id != who))
#else
                if(mbox->state == MBOX_UNUSED  ||
                   mbox->state == MBOX_DELETED ||
                   mbox->mbox_id != who)
#endif
                {
			MUTEX_UNLOCK(&mbox->monitors_m);

			MUTEX_UNLOCK(&rec->lock_m);

			MONITOR_INCR_AGE(rec->mon_id);
			MUTEX_LOCK(&itc_inst.free_rec_m);
			LL_AT_FRONT(struct monitor_record, itc_inst.free_rec, rec);
			MUTEX_UNLOCK(&itc_inst.free_rec_m);

			if(msg == NULL) {
                                tmp = itc_alloc(sizeof(uint32_t),
                                                ITC_MONITOR_DEFAULT_NO);
                                itc_send(&tmp, mymailbox->mbox_id, who);
                        } else {
                                itc_send(msg, mymailbox->mbox_id, who);
                        }

                        return ITC_NO_ID;
                }

		tar_rec = (struct monitor_mbox_record *)
                        itc_alloc(sizeof(struct monitor_mbox_record), 0);
                memset(tar_rec, 0, sizeof(struct monitor_mbox_record));
                tar_rec->rec = rec;

		rec->target_entry   = tar_rec;
		rec->target_mbox    = mbox;
		rec->target_mbox_id = who;

		INC_STAT(mbox, monitors);
                LL_AT_FRONT(struct monitor_mbox_record,
                            mbox->monitors, tar_rec);
                MUTEX_UNLOCK(&mbox->monitors_m);
	} else {
		rec->target_entry   = NULL;
		rec->target_mbox    = NULL;
		rec->target_mbox_id = who;
	}

        mon_rec->rec = rec;
        MUTEX_LOCK(&mymailbox->mymonitors_m);
        INC_STAT(mymailbox, pend_mymonitors);
        LL_AT_FRONT(struct monitor_mbox_record,
                    mymailbox->mymonitors, mon_rec);
        MUTEX_UNLOCK(&mymailbox->mymonitors_m);

        MUTEX_UNLOCK(&rec->lock_m);

        if(mbox == NULL) {
                monmsg = itc_alloc(sizeof(struct itc_monitor), ITC_MONITOR);
                monmsg->itc_monitor.from_mbox_id   = mymailbox->mbox_id;
                monmsg->itc_monitor.target_mbox_id = who;
                monmsg->itc_monitor.coord_mbox_id  = itc_inst.local_coord_mbox_id;
                monmsg->itc_monitor.mon_id         = rec->mon_id;
                itc_send(&monmsg, itc_inst.world_mbox_id, ITC_MY_MBOX);
        }

        mon_id = rec->mon_id;
	if(msg != NULL) {
                *msg = NULL;
	}

        ITC_TRACE(itc_monitor, who, (unsigned long)msg, (unsigned long)mon_id);

        return mon_id;
}

/* ===================================================================== */
/**
 *   Unmonitor (cancel) the monitoring of a mailbox.
 *
 *   @param monitor_id The ID returned by itc_monitor that you wish
 *                     to "cancel".
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_unmonitor(itc_monitor_id_t monitor_id,
                     const char *file,
                     int line)
{
        struct monitor_record *rec;
        struct monitor_mbox_record *mon_rec, *tar_rec;
        struct itc_mailbox *tar_mbox, *mon_mbox;
        union itc_msg *msg;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        ITC_TRACE(itc_unmonitor, (unsigned long)monitor_id);

        if(monitor_id == ITC_NO_ID) {
                /* Temporary solution, itc_monitor returns ITC_NO_ID as
                   monitor_id when a monitor message is returned
                   directly in the itc_monitor call. */
                return;
        }

        rec = &(itc_inst.monitor_list[MONITOR_REC_FROM_ID(monitor_id)]);

        MUTEX_LOCK(&rec->lock_m);

        if(rec->mon_id != monitor_id ||
           rec->state != MONITOR_INUSE)
        {
                MUTEX_UNLOCK(&rec->lock_m);
                return;
        }
        tar_rec  = rec->target_entry;
        tar_mbox = rec->target_mbox;
        mon_rec  = rec->monitor_entry;
        mon_mbox = rec->monitor_mbox;
        msg      = rec->msg;

        if(tar_rec != NULL) {
                MUTEX_LOCK(&tar_mbox->monitors_m);
                if(tar_rec->rec == rec) {
                        tar_rec->rec = NULL;
                        LL_OUT(struct monitor_mbox_record, tar_rec);
                        DEC_STAT(tar_mbox, monitors);
                }
                MUTEX_UNLOCK(&tar_mbox->monitors_m);
                itc_free((union itc_msg **)&tar_rec);
        }

        MUTEX_LOCK(&mon_mbox->mymonitors_m);
        if(mon_rec->rec == rec) {
                DEC_STAT(mon_mbox, pend_mymonitors);
                mon_rec->rec = NULL;
                LL_OUT(struct monitor_mbox_record, mon_rec);
        }
        MUTEX_UNLOCK(&mon_mbox->mymonitors_m);
        itc_free((union itc_msg **)&mon_rec);

        if(msg != NULL) {
                itc_free(&msg);
        }

        if(tar_rec == NULL) {
                msg = itc_alloc(sizeof(struct itc_unmonitor), ITC_UNMONITOR);
                msg->itc_unmonitor.from_mbox_id   = mon_mbox->mbox_id;
                msg->itc_unmonitor.target_mbox_id = rec->target_mbox_id;
                msg->itc_unmonitor.mon_id         = rec->mon_id;
                itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
        }

        rec->target_mbox   = NULL;
        rec->target_entry  = NULL;
        rec->monitor_mbox  = NULL;
        rec->monitor_entry = NULL;
        rec->msg           = NULL;
        rec->state         = MONITOR_DELETED;

        MUTEX_UNLOCK(&rec->lock_m);

        MONITOR_INCR_AGE(rec->mon_id);
        MUTEX_LOCK(&itc_inst.free_rec_m);
        LL_AT_FRONT(struct monitor_record, itc_inst.free_rec, rec);
        MUTEX_UNLOCK(&itc_inst.free_rec_m);

        return;
}

/* ===================================================================== */
/**
 *   Register an error handler for ITC
 *
 *   @param errh       Error handler function that shall be invoked at
 *                     error in ITC.
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_register_errorhandler(itc_errorhandler errh,
                                 const char *file,
                                 int line)
{
        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        ITC_TRACE(itc_register_errorhandler, (unsigned long)errh);

        register_errorhandler(errh);

        return;
}

/* ===================================================================== */
/**
 *   RegisterAssign a linkhandler to ITC
 *
 *   @param lnhpath    Linkhandler path.
 *
 *   @param mbox_id    Linkhandler mailbox ID.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
void __itc_assign_linkhandler(char *lnhpath,
                              itc_mbox_id_t mbox_id,
                              const char *file,
                              int line)
{
        union itc_msg *msg;
        struct itc_mailbox *mbox;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mbox_id == ITC_MY_MBOX) {
                if(mymailbox == NULL) {
                        ITC_ERROR(0, "Trying to assign LNH without having a mailbox", 0);
                        return;
                } else {
                        mbox = mymailbox;
                }
        } else {
                mbox = find_mbox(mbox_id);
                if(mbox == NULL) {
                        ITC_ERROR(0, "Trying to assign LNH to remote mailbox", 0);
                        return;
                }
                if(mbox->mbox_id != mbox_id) {
                        ITC_ERROR(0, "Trying to assign LNH to recycled mailbox", 0);
                        return;
                }
        }

        if(mbox->state != MBOX_INUSE &&
	   mbox->state != MBOX_CLONE &&
	   mbox->state != MBOX_SHARED) {
                ITC_ERROR(0, "Trying to assign LNH to not setup mailbox(0x%08x)",
                          mbox_id);
                return;
        }

        msg = itc_alloc((sizeof(struct itc_assign_lnh) + strlen(lnhpath) + 1),
                        ITC_ASSIGN_LNH);
        msg->itc_assign_lnh.mbox_id = mbox->mbox_id;
        strcpy(msg->itc_assign_lnh.lnhpath, lnhpath);
        if(msg->itc_assign_lnh.lnhpath[strlen(msg->itc_assign_lnh.lnhpath) - 1] != '/') {
                strcat(msg->itc_assign_lnh.lnhpath, "/");
        }
        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   Unregister/Deassign a linkhandler to ITC
 *
 *   @param lnhpath    Linkhandler path.
 *
 *   @param mbox_id    Linkhandler mailbox ID.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
void __itc_deassign_linkhandler(char *lnhpath,
                                itc_mbox_id_t mbox_id,
                                const char *file,
                                int line)
{
        union itc_msg *msg;
        struct itc_mailbox *mbox;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mbox_id == ITC_MY_MBOX) {
                if(mymailbox == NULL) {
                        ITC_ERROR(0, "Trying to assign LNH without having a mailbox", 0);
                        return;
                } else {
                        mbox = mymailbox;
                }
        } else {
                mbox = find_mbox(mbox_id);
                if(mbox == NULL) {
                        ITC_ERROR(0, "Trying to assign LNH to remote mailbox", 0);
                        return;
                }
                if(mbox->mbox_id != mbox_id) {
                        ITC_ERROR(0, "Trying to assign LNH to recycled mailbox", 0);
                        return;
                }
        }

        if(mbox->state != MBOX_INUSE) {
                ITC_ERROR(0, "Trying to deassign LNH from not setup mailbox(0x%08x)",
                          mbox_id);
                return;
        }

        msg = itc_alloc((sizeof(struct itc_assign_lnh) + strlen(lnhpath)),
                        ITC_DEASSIGN_LNH);
        msg->itc_assign_lnh.mbox_id = mbox_id;
        strcpy(msg->itc_assign_lnh.lnhpath, lnhpath);
        if(msg->itc_assign_lnh.lnhpath[strlen(msg->itc_assign_lnh.lnhpath) - 1] != '/') {
                strcat(msg->itc_assign_lnh.lnhpath, "/");
        }
        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   Install send and receive hooks to ITC.
 *
 *   @param send_hook  Send hook function that is called at ITC send.
 *
 *   @param send_user  User supplied field that will be forwarded in the
 *                     send hook function call.
 *
 *   @param recv_hook  Receive hook function that is called at ITC receive.
 *
 *   @param recv_user  User supplied field that will be forwarded in the
 *                     receive hook function call.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_install_hooks(itc_hook    send_hook,
                         void       *send_user,
                         itc_hook    recv_hook,
                         void       *recv_user,
                         const char *file,
                         int         line)

{
        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(send_hook != NULL) {
                itc_inst.txhook = send_hook;
                itc_inst.txuser = send_user;
        }

        if(recv_hook != NULL) {
                itc_inst.rxhook = recv_hook;
                itc_inst.rxuser = recv_user;
        }

        if(send_hook == NULL &&
           recv_hook == NULL) {
                itc_inst.txhook = NULL;
                itc_inst.txuser = NULL;
                itc_inst.rxhook = NULL;
                itc_inst.rxuser = NULL;
        }
}

/* ===================================================================== */
/**
 *   Get all ITC mailboxes
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void __itc_get_mboxes(const char *file, int line)
{
        union itc_msg *msg;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mymailbox == NULL ||
           mymailbox->state != MBOX_INUSE) {
                ITC_ERROR(0, "Own mailbox not created error", 0);
                return;
        }

        msg = itc_alloc(sizeof(struct itc_get_all_mboxes),
                        ITC_GET_ALL_MBOXES);
        msg->itc_get_all_mboxes.mbox_id = mymailbox->mbox_id;
        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   Get information about maibox
 *
 *   @param mbox_id    Mailbox id.
 *
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
struct itc_mbox_info *__itc_get_mailbox_info(itc_mbox_id_t mbox_id,
					     const char   *file,
					     int           line)
{
        struct itc_mailbox   *mbox;
        struct itc_mbox_info *mi = NULL;
        union itc_msg        *msg;
        uint32_t              get_info_repl[] = { 2, ITC_GET_MBOX_INFO_REPL,
                                                  ITC_MONITOR_DEFAULT_NO };
        itc_mbox_id_t         coord_id;
        itc_monitor_id_t      mon_id;
        char                  name[ITC_NAME_MAXLEN];

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        mbox = find_mbox(mbox_id);
	mbox_id = CVT_MBOX_ID(mbox_id);

        if(mbox != NULL) {
                mi = (struct itc_mbox_info *)
                     itc_alloc((sizeof(struct itc_mbox_info) +
                                ITC_NAME_MAXLEN), 0);

                get_mailbox_info(mbox, mi, false);
        } else {
                sprintf(name, "itc_local_coord_0x%08x",
                        (mbox_id & itc_inst.world_mask));
                coord_id = itc_locate(name);
                if(coord_id != ITC_NO_ID) {
                        mon_id = itc_monitor(coord_id, NULL);
                        msg = itc_alloc(sizeof(struct itc_get_mbox_info),
                                        ITC_GET_MBOX_INFO);
                        msg->itc_get_mbox_info.mbox_id = mbox_id;
                        itc_send(&msg, coord_id, ITC_MY_MBOX);

                        msg = itc_receive(get_info_repl,
                                          ITC_NO_TMO, ITC_FROM_ALL);
                        if(msg->msgno == ITC_GET_MBOX_INFO_REPL) {
                                if(msg->itc_get_mbox_info_repl.mbox_found) {
                                        mi = (struct itc_mbox_info *)
                                                itc_alloc((sizeof(struct itc_mbox_info) +
                                                           strlen(msg->itc_get_mbox_info_repl.mbox_info.name)), 0);

                                        *mi = msg->itc_get_mbox_info_repl.mbox_info;
                                        strcpy(mi->name, msg->itc_get_mbox_info_repl.mbox_info.name);
                                }
                                itc_unmonitor(mon_id);
                        }
                        /* If it is the monitor that triggers do nothing but
                           free the monitor message */
                        itc_free(&msg);
                }
        }

        return mi;
}

/* ===================================================================== */
/**
 *   Get information about ITC allocations.
 *
 *   @param mbox_id    Mailbox id of any mailbox in the process tou wish
 *                     to get alloc information about.
 *
 *   @return           Pointer to structure with allocation information.
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
struct itc_alloc_info *__itc_get_alloc_info(itc_mbox_id_t mbox_id,
                                            const char   *file,
                                            int           line)
{
        struct itc_alloc_info *ai = NULL;
        char                   name[ITC_NAME_MAXLEN];
        itc_mbox_id_t          coord_id;
        itc_monitor_id_t      mon_id;
        uint32_t               get_alloc_repl[] = { 2, ITC_GET_ALLOC_INFO_REPL,
                                                    ITC_MONITOR_DEFAULT_NO };
        union itc_msg         *msg;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR(ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mbox_id == ITC_MY_MBOX ||
           (mbox_id & itc_inst.world_mask) == itc_inst.my_world_id) {
                if(alloc_funcs.itci_free != NULL) {
                        ai = alloc_funcs.itci_info();
                }
        } else {
                sprintf(name, "itc_local_coord_0x%08x",
                        (mbox_id & itc_inst.world_mask));
                coord_id = itc_locate(name);
                if(coord_id != ITC_NO_ID) {
                        mon_id = itc_monitor(coord_id, NULL);
                        msg = itc_alloc(sizeof(struct itc_get_alloc_info),
                                        ITC_GET_ALLOC_INFO);
                        itc_send(&msg, coord_id, ITC_MY_MBOX);

                        msg = itc_receive(get_alloc_repl,
                                          ITC_NO_TMO, ITC_FROM_ALL);

                        if(msg->msgno == ITC_GET_ALLOC_INFO_REPL) {
                                ai = (struct itc_alloc_info *)
                                        itc_alloc(sizeof(struct itc_alloc_info), 0);
                                memcpy(ai, &(msg->itc_get_alloc_info_repl.alloc_info),
                                       sizeof(struct itc_alloc_info));

                                itc_unmonitor(mon_id);
                        }
                        /* If it is the monitor that triggers do nothing
                           but free the monitor message */
                        itc_free(&msg);
                }
        }

        return ai;
}

/* ===================================================================== */
/**
 *   Get path to itc run directory
 *
 *   @return           Pointer to path used for itc run directory.
 *
 *   @par Globals:     itc_inst
 *                     Global ITC instance structure.
 *
 */
/* ===================================================================== */
char *itc_get_rundir(void)
{
        conf_run_dir();

        return itc_inst.inst_path;
}

/* ===================================================================== */
/**
 *   Call ITC error handler
 *
 *   @param errtext    Error message character string.
 *
 *   @param flags       Error flags.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Make ITC call the ITC error handler.
 *
 */
/* ===================================================================== */
void __itc_call_errh(char *errtext,
                     uint32_t flags,
                     const char *file,
                     int line)

{
        ITC_ERROR(flags, errtext, 0);
}
/* ===================================================================== */
/**
 *   Subscribe to ITC events
 *
 *   @param events     Flag field of which events to subscribe to.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
void itc_subscribe_events(uint32_t events)
{
        union itc_msg *msg;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
				  ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
				  0);
                /* Above error is fatal, will never return here! */
        }

        if(mymailbox == NULL ||
           mymailbox->state != MBOX_INUSE) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
				  0, "Own mailbox not created error", 0);
                return;
        }

        msg = itc_alloc(sizeof(struct itc_subscribe_events),
                        ITC_SUBSCRIBE_EVENTS);
        msg->itc_subscribe_events.mbox_id = mymailbox->mbox_id;
        msg->itc_subscribe_events.events = events;
        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   Unsubscribe to ITC events
 *
 *   @param events     Flag field of which events to subscribe to.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
void itc_unsubscribe_events(uint32_t events)
{
        union itc_msg *msg;

        if(itc_inst.mboxes == NULL) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
				  ITC_ERROR_FATAL, "ITC not initialised (itc_init not run)",
                          0);
                /* Above error is fatal, will never return here! */
        }

        if(mymailbox == NULL ||
           mymailbox->state != MBOX_INUSE) {
                ITC_ERROR_VERBOSE(__FUNCTION__, __FILE__, __LINE__,
				  0, "Own mailbox not created error", 0);
                return;
        }

        msg = itc_alloc(sizeof(struct itc_subscribe_events),
                        ITC_UNSUBSCRIBE_EVENTS);
        msg->itc_subscribe_events.mbox_id = mymailbox->mbox_id;
        msg->itc_subscribe_events.events = events;
        itc_send(&msg, itc_inst.world_mbox_id, ITC_MY_MBOX);
}

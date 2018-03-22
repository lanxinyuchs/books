/**
 *   Local data and definitions for LITS.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2011-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-09-10 Ravineet Singh
 *   Change  : Added support for user define:able lits_sigrt_base.
 *
 *   Revised : 2015-03-16 Ravineet Singh EAB/FJP/HB
 *   Change  : Moved tmo signals and tmo funtion definitions to tmoserver.h
 *
 *   Revised : 2015-02-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Renamed lits_tmo_cleanup ->lits_tmo_remove
 *             Added a new funtion; lits_tmo_cleanup
 *             Added LITS_TMO_WRAP signal.
 *
 *   Revised : 2014-05-08 Stanislav Vovk
 *   Change  : Defined lits_error as macro, now it can handle variable
 *             arguments
 *
 *   Revised : 2014-04-15 Ravineet Singh EAB/FJP/HB
 *   Change  : Added THREAD_INTERRUPT_SIGNAL to interrupt blocking syscall,
 *             for LINX usecase only.
 *
 *   Revised : 2014-03-18 Ravineet Singh EAB/FJP/HB
 *   Change  : Added THREAD_KILL_SIGNAL and THREAD_STOP_SIGNAL.
 *
 *   Revised : 2014-02-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected definition of UNKNOWN_PRIORITY.
 *
 *   Revised : 2014-02-21 Ravineet Singh EAB/FJP/HB
 *   Change  : Added LITS_ZOMBIE_COLLECT signal definition.
 *
 *   Revised : 2014-02-10 Ravineet Singh EAB/FJP/HB
 *   Change  : Added LITS internal print function, lits_print_message(),
 *             that uses less stack than fprintf()
 *
 *   Revised : 2014-02-06 Lars Jönsson EAB/FJP/TB
 *   Change  : Added global timeout functions.
 *
 *   Revised : 2014-01-23 Stanislav Vovk
 *   Change  : Moved RT prio constants here for use
 *             in priotest
 *
 *   Revised : 2013-09-25 Stanislav Vovk
 *   Change  : Added ITC types to tcb
 *
 *   Revised : 2012-05-08 Lars Jönsson EAB/FJP/TB
 *   Change  : Pthread ID is now stored in tcb in order to be retrieved by
 *             other threads.
 *
 *   Revised : 2012-04-23 Hans Beckerus
 *   Change  : Added "fast" semaphore handle.
 *
 *   Revised : 2012-03-02 Zabihullah Bayat & Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for interrupt processes.
 *
 *   Revised : 2012-02-15 Lars Jönsson EAB/FJP/TB
 *   Change  : Made lits_local_process() more robust by using const
 *             argument.
 *
 *   Revised : 2011-12-15 Lars Jönsson EAB/FJP/TB
 *   Change  : Added redirection table process type to TCB.
 *
 *   Revised : 2011-04-26 Lars Jönsson EAB/FJP/TB
 *   Change  : Added environment to TCB.
 *
 *   Revised : 2011-03-14 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __LITS_H
#define __LITS_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>

#include <itc.h>
#include <itc_system.h>
#include <stdlib.h>
#include <ose.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define MAX_PROC_NAME_LEN	255

/* default max processes */
#define MAX_PROCS_DEFAULT       512

/*
** This maps OSE scheduler priority to Linux priority
** - OSE Prioritized process:
**   OSE prio 0-31 uses Linux prio 41-10 (SCHED_FIFO)
** - OSE Interrupt process:
**   OSE prio 0-31 uses Linux prio 91-60 (SCHED_FIFO)
** - OSE Background process: SCHED_OTHER (nice prios)
** - OSE Phantom process: Linux prio 92 (SCHED_FIFO)
*/
#define UNKNOWN_PRIORITY	255

#define OSE_PRIORITY_LEVELS	32	/* For PRI_PROC and INT_PROC */
#define	PRIO_BASE_PRI_PROC	10	/* Linux prio 41-20 for PRI_PROC */
#define	PRIO_BASE_INT_PROC	60	/* Linux prio 91-60 for INT_PROC */
#define	PRIO_BASE_BG_PROC	0
#define	PRIO_BASE_PHANTOM	92
#define	MAX_USED_RT_PRIO	PRIO_BASE_PHANTOM

#define DEFAULT_ITC_PRIO 22     /* Which prio to use for ITC threads */
/*
** Lits rt signal base.
** possible to change via env variable LITS_SIGRT_BASE.
*/
extern uint32_t lits_sigrt_base;
#define LITS_SIGRT_BASE_DEFAULT  (SIGRTMIN)
#define LITS_NR_SIGRT_USED       (2)
#define THREAD_STOP_SIGNAL       (lits_sigrt_base)
#define THREAD_TMO_SIGNAL        (lits_sigrt_base + 1)

/*
** Internal error code that is used when a string is supplied in the extra
** parameter.
*/
#define	ECODE_INTERNAL		0x87273045

/* Prints an error and calls error handler function */
#define lits_error(__fmt, args...)                      \
    do                                                  \
    {                                                   \
        lits_print_message(__fmt, ## args);             \
        lits_error_handler(0, ECODE_INTERNAL, 0);	\
    } while (0)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef struct tcb_t_
{
    OSENTRYPOINT          *entrypoint;
    itc_mbox_id_t         mb;
    PROCESS               pid;
    pthread_mutex_t       mutex; /* For init, start/stop, and kill handling */
    pthread_cond_t        init_cond;
    pthread_cond_t        suspend_cond;
    uint32_t              stop_cnt;
    OSERRH                *error_handler;
    void*                 envp;
    struct OS_redir_entry *redir_table;
    enum PROCESS_TYPE     proc_type;
    int                   current_event;	/* Only used by OS_INT_PROC */
    sem_t			*fsem;
    int			fsem_pipe[2];	/* Only used by OS_INT_PROC */
    pthread_t             tid;
    char                  name[MAX_PROC_NAME_LEN + 1];
} tcb_t;

#define LITS_SIGBASE       (76543210)
#define LITS_TMO_SIGBASE   (LITS_SIGBASE + 100)


/** ==================================================================== */
/** @struct z_collect
 *
 *  Collect zombies. Send from zz_kill_proc to main thread.
 *
 *  @param tid       Thead to collect
 */
/* ===================================================================== */

#define LITS_ZOMBIE_COLLECT   (LITS_SIGBASE + 1) /*!- SIGNO(struct lits_z_collect) -!*/
#define LITS_ZOMBIE_COLLECT_R (LITS_SIGBASE + 2) /*!- SIGNO(struct lits_z_collect) -!*/

struct lits_z_collect
{
    SIGSELECT sig_no;
    tcb_t*    tcb;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
extern __thread  tcb_t  *lits_data;

/** ==================================================================== */
/**
 *   Local error handler.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zeor if
 *                     called via error() and error2().
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           Not used
 *
 *   @par Globals:
 *                     --
 *
 *   This error handler first calls the process error handler (if it
 *   exists) and then the block error handler (if it exists) and then the
 *   system error handler (if it exists) and finally the default error
 *   handler. Any of the error handler may never return.
 */
/* ===================================================================== */
extern OSADDRESS
lits_error_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra);

/** ==================================================================== */
/**
 *   Prints the message (currently to stderr).
 *
 *   @param fmt    Format.
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int lits_print_message(const char *fmt, ...);

/** ==================================================================== */
/**
 *   Serach for a process in the current program.
 *
 *   @param name       Name of the process
 *
 *   @return           PID of the process or 0 if not found
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern PROCESS lits_local_process(const char *name);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */

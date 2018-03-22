/**
 *   Process handling functions.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2011-2016 by Ericsson AB. All rights reserved. The
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
 *
 *   Revised : 2016-05-23 Mikael Zwahlen
 *   Change  : current_process() will now return 0 for a non-lits thread.
 *
 *   Revised : 2016-02-12 Anette Sch�tt
 *   Change  : Updates of the 'removal of setenv usage' implementation,
 *             because Lits must support both osemain.c files using/
 *             not using setenv (client might not be rebuilt with updated
 *             osemain.c).
 *
 *   Revised : 2016-02-05 Anette Sch�tt
 *   Change  : Updates to not use setenv in osemain.c.
 *
 *   Revised : 2015-12-16 Magnus Lindberg
 *   Change  : Removed thread cleanup handler. ITC ensures that the mailbox
 *             is removed at pthread_cancel or pthread_exit. Added log of
 *             error code when itc_init fails.
 *
 *   Revised : 2015-09-10 Ravineet Singh
 *   Change  : Added support for user define:able lits_sigrt_base.
 *
 *   Revised : 2015-06-15 Ravineet Singh
 *   Change  : Added support for SCHED_RR scheduling policy.
 *
 *   Revised : 2015-03-30 Ravineet Singh
 *   Change  : Added impl. of ose_cpu_id and ose_num_cpus system calls
 *
 *   Revised : 2015-03-27 Ravineet Singh
 *   Change  : Removed LINX dependency. Removed trailing whitespaces.
 *
 *   Revised : 2015-03-16 Ravineet Singh EAB/FJP/HB
 *   Change  : Included tmoserver.h
 *             Removed call to lits_tmo_cleanup()
 *
 *   Revised : 2015-03-05 Henrik Wallin
 *   Change  : Change zzget_pri to not always call pthread_getschedparam().
 *             Make sure it only is called on valid processes. If called
 *             on a zombie pid, it can fail and get stuck in assert().
 *
 *   Revised : 2015-02-20 Henrik Wallin
 *   Change  : Change lits_error_handler to check fatal condition also
 *             after one of the installed error handlers returns.
 *             Fold default_error_handler into lits_error_handler.
 *
 *   Revised : 2015-02-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Added cleanup of timers in pthread cancellation
 *             handler (thread_kill)
 *
 *   Revised : 2015-01-22 Ravineet Singh EAB/FJP/HB
 *   Change  : Extended stack for the daemon thread. If linked with
 *             application that uses a fair amount of shared libs,
 *             pthread_cancel uses more stack.
 *
 *   Revised : 2014-10-30 Henrik Wallin
 *   Change  : Add missing itc_delete_mailbox call.
 *             Also make sure tcb->mb is set to mbox id and not pid.
 *
 *   Revised : 2014-10-28 Henrik Wallin
 *   Change  : Make the body of thread_interrupt empty. The delay(0) call
 *             was a cancellation point in iteself, causing the thread
 *             to end inside the signal handler. That is not good, as
 *             that can basicly be "anywere".
 *
 *   Revised : 2014-10-15 Ravineet Singh EAB/FJP/HB
 *   Change  : lits_print_message() now also writes to syslog, in case
 *             used by a daemonized process.
 *
 *   Revised : 2014-10-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Rewritten zzget_pcb to be human readable.
 *             zzget_pri now handles all process types.
 *             block_data->tid is now set.
 *
 *   Revised : 2014-04-11 Ravineet Singh EAB/FJP/HB
 *   Change  : Changed macro LITS_DISABLE_RTSCHED to LITS_ENABLE_RTSCHED
 *
 *   Revised : 2014-09-18 Henrik Wallin
 *   Change  : Fix problem in kill handling. Changes done for how the
 *             mutex is handled for the conditional variable.
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Fix memory leak in create_process.
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Cleanup tcb->pid usage.
 *             Initialize it fully in thread_start and init_os, so it
 *             can be used directory instead of open coding the itc / linx
 *             #ifdefs for each usage.
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Add assert(lits_data).
 *             lits functions must be called from a lits created thread.
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Move prctl calls earlier to aid debugging
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Make sure linx_close is called from correct thread.
 *             It should be called from the same thread that did linx_open.
 *             A cancellation hok is installed that will be triggered when the
 *             thread is cancelled.
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Add helper function (wait_for_start).
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Reworked the tcb mutex and conditional variable
 *             synchronization during create_process and kill_proc.
 *             Replace the "cond_*" macros with real conditional variables
 *             and rename and reuse the suspend_mutex as the mutex guarding the
 *             cond variable.
 *             Move the mutex grabbing around to make sure that the create_process
 *             thread and the new thread doesn't race. (Old code had a risk to get
 *             stuck as found by helgrind).
 *             For kill handling - move all "real kill code" into lits_daemon thread.
 *             Now the kill_proc code only sends the signal and wait for reply.
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Fix logical error (&& -> ||)
 *             Check pointers before dereferencing
 *             Add structure to union SIGNAL to avoid explicit casts
 *
 *   Revised : 2014-08-11 Stanislav Vovk
 *   Change  : Removed support for '-c' option
 *
 *   Revised : 2014-07-08 Daniel Nilsson
 *   Change  : If init_os is run with SCHED_FIFO scheduling class then
 *             drop priority to a lower priority before calling itc_init.
 *             This ensured that the threads started within itc_init are
 *             are not given highest priority in the system.
 *
 *   Revised : 2014-05-16 Stanislav Vovk
 *   Change  : Replaced assert() with lits_error() in collect_zombie.
 *             assert() uses fprintf which might segfault if thread
 *             stack is small. lits_error() is now a macro which can
 *             accept variabel args.
 *
 *   Revised : 2014-04-11 Ravineet Singh EAB/FJP/HB
 *   Change  : Changed the procedure of killing a thread. Instead of
 *             using pthread_exit() in a signal handler
 *             (while potentially holding a lock), a daemon is created
 *             that terminated the thread by using pthread_cancel().
 *             THREAD_INTERRUPT_SIGNAL is used by the daemon, only when
 *             using Linx (ITC LINX and ITC_OVER_LINX), to interrupt
 *             blocking system calls.
 *
 *  Revised :  2014-04-09 Ravineet Singh EAB/FJP/HB
 *  Change  :  Make it possible to set min stack size for all threads by
 *             setting UNIX env variable or osemain.con BLOCK_VARIABLE
 *             'LITS_MIN_STACK_SIZE'.
 *
 *   Revised : 2014-03-27 Lars Jönsson EAB/FJP/TB
 *   Change  : Replaced "cpucore" via an option with environment variable.
 *             The command line option is kept during the migration to
 *             use environment variable in the application.
 *
 *   Revised : 2014-03-20 Ravineet Singh EAB/FJP/HB
 *   Change  : Corrected get_pcb, shall not call error handler for
 *             illegal pid.
 *
 *   Revised : 2014-03-18 Ravineet Singh EAB/FJP/HB
 *   Change  : Fixed deadlocks when a process is to be killed, while it
 *             holds a tcb mutex.
 *             sighand now uses lits_data instead of calling get_tcb().
 *             THREAD_KILL_SIGNAL now blocks THREAD_STOP_SIGNAL, i.e not
 *             interrupted
 *             Moved THREAD_KILL_SIGNAL to lits_internal.h
 *             Renamed SIGRTMIN to THREAD_STOP_SIGNAL
 *
 *   Revised : 2014-03-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected set_pri() for SCHED_FIFO disabled.
 *
 *   Revised : 2014-02-24 Ravineet Singh EAB/FJP/HB
 *   Change  : Make sure that the main thread receives the tcb pointer
 *             before the current thread is terminated
 *
 *   Revised : 2014-02-24 Ravineet Singh EAB/FJP/HB
 *   Change  : Fixed race condition where the mailbox is removed after
 *             tcb removal. Also only letting the block thread reap threads
 *             killing themselves.
 *
 *   Revised : 2014-02-21 Ravineet Singh EAB/FJP/HB
 *   Change  : Fixed memory leak @ kill_proc(current_process()) by letting
 *             main thread clean up after the zombie thread.
 *             Freeing the tcb is now done after thread cleanup in main thread.
 *             Minor cleanup in comments.
 *             Cleanup of signals in zzos_main.
 *
 *   Revised : 2014-02-10 Ravineet Singh EAB/FJP/HB
 *   Change  : Added LITS internal print function, lits_print_message(),
 *             that uses less stack than fprintf()
 *             Release mutex in zzkill_proc
 *
 *   Revised : 2014-02-06 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed compiler warnings.
 *
 *   Revised : 2014-02-03 Lars Jönsson EAB/FJP/TB
 *   Change  : Return value of itc_init() is now checked.
 *
 *   Revised : 2014-01-23 Stanislav Vovk
 *   Change  : Moved rlimit not set warning message to priotest,
 *             Moved RT prio constants to lits_internal.h for use
 *             in priotest
 *
 *   Revised : 2013-12-20 Marcus Ahlberg
 *   Change  : Allow OS_BG_PROC to use static pid
 *
 *   Revised : 2013-12-09 Stanislav Vovk
 *   Change  : real to static pid conversion support
 *
 *   Revised : 2013-12-06 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected priorty calculation in get_pcb(), when real-time
 *             priorities are disabled,
 *
 *   Revised : 2013-12-04 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected the test that checks if real-time priorities can
 *             be used. It was not updated when usage of real-time
 *             priorities was changed.
 *
 *   Revised : 2013-12-02 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for background processes.
 *
 *   Revised : 2013-11-28 Lars Jönsson EAB/FJP/TB
 *   Change  : Changed usage of real-time priorities.
 *             - OSE Prioritized process:
 *               OSE prio 0-31 uses Linux prio 41-10 (SCHED_FIFO)
 *             - OSE Interrupt process:
 *               OSE prio 0-31 uses Linux prio 91-60 (SCHED_FIFO)
 *             - OSE Background process: SCHED_OTHER (nice prios)
 *               (not implementet yet)
 *             - OSE Phantom process: Linux prio 92 (SCHED_FIFO)
 *
 *   Revised : 2013-11-21 Stanislav Vovk
 *   Change  : Fix: get_ptype returns correct type for unknown pid
 *
 *   Revised : 2013-11-13 Stanislav Vovk
 *   Change  : Added static pid support for create_process
 *
 *   Revised : 2013-11-12 Stanislav Vovk
 *   Change  : Fix: get_env_list always returns an allocated buffer
 *
 *   Revised : 2013-10-31 Stanislav Vovk
 *   Change  : Added s_create_pool
 *
 *   Revised : 2013-10-31 Stanislav Vovk
 *   Change  : Added get_env_list
 *
 *   Revised : 2013-10-25 Ravineet Singh EAB/FJP/HB
 *   Change  : Fix: zzset_envp and zzset_envp are now 64 bit compatible
 *
 *   Revised : 2013-10-22 Stanislav Vovk
 *   Change  : Fix: get_pcb returns correct name
 *
 *   Revised : 2013-10-14 Stanislav Vovk
 *   Change  : Changed ITC initialization
 *
 *   Revised : 2013-09-26 Stanislav Vovk
 *   Revised : 2013-09-25 Stanislav Vovk
 *   Change  : Bugfixes for ITC version.
 *             If mailbox has been deleted the type will be detected as OS_ZOOMBIE
 *
 *   Revised : 2013-09-01 Stanislav Vovk
 *   Change  : Added ITC support
 *
 *   Revised : 2013-08-30 Hans Beckerus
 *   Change  : Updated according to new libuio interface.
 *             Only affecting code built with -DENABLE_INT_PROC.
 *
 *   Revised : 2013-08-28 Stanislav Vovk
 *   Change  : Using SIGUSR1 for kill_proc
 *             When SIGTERM is sent to all programs at system shutdown
 *             the sighandler would be executed in context of any existing
 *             thread. Meaning threads linx endpoint will close and send attach signal
 *             to zzos_main() which in turn would generate an error and call
 *              abort().
 *
 *   Revised : 2013-08-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Adjust stack size according to requested size and
 *             glibc alignment.
 *
 *   Revised : 2013-08-26 Ravineet Singh EAB/FJP/HB
 *   Change  : default_error_handler now calls abort.
 *
 *   Revised : 2013-08-27 Stanislav Vovk
 *   Change  : In OSE get_pcb() returns comlete link name. Changed
 *             to same behaviour.
 *
 *   Revised : 2013-02-01 Lars Jönsson EAB/FJP/TB
 *   Change  : Improved robustness when killing a stopped process.
 *
 *   Revised : 2012-10-16 Lars Carlsson EAB/FJP/HO
 *   Change  : linx_info_get_type() and linx_info_get_state()
 *             implementation depend on macro ENABLE_USEL_LINX.
 *
 *   Revised : 2012-10-12 Lars Carlsson EAB/FJP/HO
 *   Change  : Implementation of linx_info_get_type() and
 *             linx_info_get_state() moved to liblinx.
 *
 *   Revised : 2012-05-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Killing of a process is now more robust and kill_proc()
 *             will not return until the pthread has exited, i.e. totally
 *             removed.
 *
 *   Revised : 2012-04-24 Hans Beckerus
 *   Change  : Fixed default value of fsemvalue and added dummy stack
 *             to zzget_pcb().
 *
 *   Revised : 2012-04-24 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected memory leakage at killing of processes.
 *             Corrected problem with no fast semaphore on the block.
 *             Added support for OS_BLOCK to get_ptype().
 *
 *   Revised : 2012-04-23 Hans Beckerus
 *   Change  : Added implementation of wait_fsem(), signal_fsem(),
 *             get_fsem() and set_fsem().
 *             Fixed bug for which interrupt processes could start too
 *             early without having its tcb stored in the database.
 *
 *   Revised : 2012-04-19 Hans Beckerus
 *   Change  : Fixed 'wanted' bug in zzget_pcb().
 *
 *   Revised : 2012-04-04 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed temporary code.
 *
 *   Revised : 2012-04-04 Lars Jönsson EAB/FJP/TB
 *   Change  : Made default error handler more robust.
 *
 *   Revised : 2012-03-02 Zabihullah Bayat & Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for interrupt processes.
 *
 *   Revised : 2012-02-15 Lars Jönsson EAB/FJP/TB
 *   Change  : Made lits_local_process() more robust by using const
 *             argument. Added prefix to all debug printouts.
 *
 *   Revised : 2012-02-15 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected a bug in get_ptype() that was introduced when
 *             phantom processes were added.
 *
 *   Revised : 2012-02-09 Lars Jönsson EAB/FJP/TB
 *   Change  : Fixed problem with small stack sizes. Previously default
 *             stack size (around a couple of MB) was used when specified
 *             stack size was smaller than PTHREAD_STACK_MIN.
 *
 *   Revised : 2012-01-17 Lars Jönsson EAB/FJP/TB
 *   Change  : Moved time handling to a separate file [time.c].
 *
 *   Revised : 2012-01-09 Lars Jönsson EAB/FJP/TB
 *   Change  : Phantom processes can now be created without any redirection
 *             table.
 *
 *   Revised : 2011-12-15 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for phantom processes.
 *
 *   Revised : 2011-12-14 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for set/get_envp().
 *
 *   Revised : 2011-12-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for request_tmo_sig(). Added const declaration
 *             of some parameters in function calls.
 *             Corrected request_tmo(), i.e. return vaule from Linx call is
 *             checked.
 *
 *   Revised : 2011-05-20 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected stop() to use its own linx endpoint when
 *             retrieving the owner (Linux thread) of the specified
 *             process. The previous implementation did not work with
 *             linx 2.5.0, because linx now checks if the socket is
 *             already in use.
 *
 *   Revised : 2011-04-27 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for inheriting the Linux enviroment to the
 *             block environment.
 *
 *   Revised : 2011-04-26 Lars Jönsson EAB/FJP/TB
 *   Change  : Rewritten environment variables handling.
 *
 *   Revised : 2011-04-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected error handling in get_env() and added set_env().
 *
 *   Revised : 2011-03-29 Lars Jönsson EAB/FJP/TE
 *   Change  : Added support for changing process and block error handlers.
 *
 *   Revised : 2011-03-28 Lars Jönsson EAB/FJP/TE
 *   Change  : Added pluggable error handlers process, block and system.
 *
 *   Revised : 2011-03-28 Lars Jönsson EAB/FJP/TE
 *   Change  : Added support for a Block, i.e. use the "starter thread" as
 *             as the block.
 *
 *   Revised : 2011-03-21 Lars Jönsson EAB/FJP/TE
 *   Change  : Corrected startup of threads, i.e. scheduling policy is now
 *             set as expected.
 *
 *   Revised : 2011-03-21 Lars Jönsson EAB/FJP/TE
 *   Change  : Cleanup of error handling.
 *
 *   Revised : 2011-02-07 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/*
** TODO: - Update handling of cond_signal() or migrate it with suspend
**         mutex handling. Any new system calls, like get my "name" (get_iid()
**       - Add block linkhandler. Is prepending "<name>/" to the process name
**         sufficient?
**
** TODO for new structure:
**       - Prios
**         - Handle SCHED_OTHER in zzget_pri()
*/

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <libgen.h>
#include <pthread.h>
#include <limits.h>
#include <errno.h>
#include <signal.h>
#include <lits_internal.h>
#include <osetypes.h>
#include <tcb.h>
#include <stdarg.h>
#include <env.h>
#include <sys/ioctl.h>
#include <sys/prctl.h>
#include <sys/syscall.h>   /* For SYS_xxx definitions */
#include <syslog.h>
#include <semaphore.h>
#include <stdbool.h>
#include <sys/resource.h>
#include <tmoserver.h>
#include <unistd.h>

/* UIO helper */
#ifdef ENABLE_INT_PROC
#include "uio_helper.h"
#endif

#include <itc.h>
#include <itc_system.h>
#include <ose_i.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#if 0
#define	DEBUG
#endif

#ifdef	DEBUG
#define	DBG_PRINT(...)		printf("DBG: "); printf(__VA_ARGS__)
#else
#define	DBG_PRINT(...)
#endif

#define ALIGN(x,a)              __ALIGN_MASK(x,(__typeof__(x))(a)-1)
#define __ALIGN_MASK(x,mask)    (((x)+(mask))&~(mask))

/* Forward declarations */
static void lits_daemon(void);

#define ENV_SIZE (32)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/*
** Temporary storage of environment variables for the main thread.
** These variables shall be set to main thread of the application
** when it's tcb is alloctaed.
*/
char *__lits_environ[ENV_SIZE] = {0};

union SIGNAL
{
    SIGSELECT   sig_no;
    struct lits_z_collect lits_z_collect;
};

/*
** Thread local pointer to tcb
*/
__thread  tcb_t  *lits_data = NULL;

/*
** Pointer to tcb for the Block
*/
static tcb_t  *block_data = NULL;

/*
 * Process ID for the LITS daemon
*/
static PROCESS daemon_pid = OSE_ILLEGAL_PROCESS;

/*
** System error handler. May only be changed by changed
** at compile time configuration.
*/
OSERRH *lits_system_error_handler = NULL;

/*
** Lits rt signal base (user overrideable).
*/
uint32_t lits_sigrt_base;

/*
** OSE Kernel User's Guide specifies maximum buffer size to be 65535.
*/
const int MAX_SIGSIZE = 65535;

/*
** If highest needed RT Scheduling priority (OSE prio 0) cannot be used due
** to permission problems, SCHED_OTHER is used for all threads
*/
#ifdef LITS_SCHED_FIFO
static int lits_sched_policy = SCHED_FIFO;
#elif defined LITS_SCHED_ROUND_ROBIN
static int lits_sched_policy = SCHED_RR;
#else
static int lits_sched_policy = SCHED_OTHER;
#endif /* LITS_ENABLE_RTSCHED */

/*
** Used for seting core affinity
*/
#define UNDEFINED_CPU_CORE 0xFFFF
static unsigned int assigned_cpu_core = UNDEFINED_CPU_CORE;


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Common channel to print messages.
 *   Message is printed both to syslog and strerr.
 *
 *   @param fmt
 *                     Message to be printed.
 *
 *   @return           Nr of bytes written.
 *
 *   @par Globals:
 *                     --
 *
 */
/* ===================================================================== */
int
lits_print_message(const char *fmt, ...)
{
    va_list args;
    int tmp;
    char *buffer;
    int ret;

    va_start(args,fmt);
    ret = vasprintf(&buffer, fmt, args);
    assert(ret != -1);
    va_end(args);

    openlog ("liblits", LOG_CONS | LOG_PID | LOG_NDELAY, LOG_USER);
    syslog (LOG_ERR, "%s", buffer);
    closelog ();

    ret = write(STDERR_FILENO, buffer, strlen(buffer));
    assert(-1 != ret);
    tmp = (int)ret;
    ret = write(STDERR_FILENO, "\n", strlen("\n"));
    assert(-1 != ret);
    tmp += (int)ret;
    free(buffer);
    return tmp;
}


/* ========================================================================
 *   Global Error Handling Functions
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Local error handler.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zero if
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
 *   system error handler (if it exists).
 *   Any of the error handler may never return.
 */
/* ===================================================================== */
OSADDRESS
lits_error_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra)
{
    int is_fatal = (user_called == 0) && (ecode & 0x80000000);
    OSADDRESS res = 0;

    /* Try each error handler in turn.
     * After one of them returns 1, no more handlers are called.
     */
    if (lits_data && lits_data->error_handler)
    {
        res = lits_data->error_handler(user_called, ecode, extra);
    }
    if (!res && block_data && block_data->error_handler)
    {
        res = block_data->error_handler(user_called, ecode, extra);
    }
    if (!res && lits_system_error_handler)
    {
        res = lits_system_error_handler(user_called, ecode, extra);
    }

    /* Only return if the error is non-fatal and one of the
     * handlers returned 1.
     */
    if (res && !is_fatal)
    {
        return res;
    }

    /* We are going to abort. Print some nice message.
     */
    {
        PROCESS pid = 0;
        char *name = NULL;

        if (lits_data)
        {
            pid = lits_data->pid;
            name = lits_data->name;
        }

        lits_print_message("%s in \"%s\" (0x%x)\n"
                           "- ecode: 0x%08lx, extra: 0x%08lx, user_called: %d\n",
                           is_fatal ? "Fatal Error" : "Error",
                           name ? name : "(unknown process)",
                           pid, ecode, extra, user_called);
    }

    /* The error is fatal or no handler available that could handle it.
     * Cause abnormal process termination hence generating a core dump.
     */
    abort();

    return 0;
}

/** ==================================================================== */
/**
 *   Error handler.
 *
 *   @param ecode      Error code
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzerror(OSERRCODE ecode)
{
    lits_error_handler(1, ecode, 0);
}

/** ==================================================================== */
/**
 *   Error handler.
 *
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzerror2(OSERRCODE ecode, OSERRCODE extra)
{
    lits_error_handler(1, ecode, extra);
}

/** ==================================================================== */
/**
 *   Error handler.
 *
 *   @param id         Process or Block ID
 *   @param entry_point
 *                     Entry point for the new Error handler
 *   @param stack_size Stack size for the Error handler (Not used)
 *
 *   @return           Entry point of previous error handler or NULL
 *
 *   @par Globals:
 *                     tcb for the Process or Block
 */
/* ===================================================================== */
OSERRH*
zzcreate_error_handler(PROCESS id, OSERRH *entrypoint, OSADDRESS stack_size)
{
    OSERRH *old;
    tcb_t  *tcb;

    (void)stack_size;

    if ( (tcb = get_tcb(id)) == NULL )
    {
        lits_error_handler(0, 0x32, id);
        return NULL;
    }

    old = tcb->error_handler;
    tcb->error_handler = entrypoint;

    return old;
}

/* ========================================================================
 *   Internal Process Handling Functions
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Returns the IPC name of the specified process.
 *
 *   @param pid        Process ID
 *
 *   @return           IPCs name or NULL
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static char*
ipc_get_name(PROCESS pid)
{
    char *name = NULL;

    name = (char*)zzalloc(ITC_NAME_MAXLEN, 0);
    (void)itc_get_name(pid, name, ITC_NAME_MAXLEN);
    return name;
}

/** ==================================================================== */
/**
 *   Free IPC name, retreived by ipc_get_name.
 *
 *   @param name       name buffer to free
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
ipc_free_name(char * name)
{
    assert(name);
    zzfree_buf( (union SIGNAL **)&name );
}

/** ==================================================================== */
/**
 *   Get IPC state.
 *
 *   @param pid        Process ID
 *
 *   @return           state
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
ipc_get_state(PROCESS pid)
{
    return 0;
}

/** ==================================================================== */
/**
 *   Converts OSE priority process priority level to something that can be
 *   used by pthread_setschedprio().
 *
 *   @param ose_prio   OSE priority
 *
 *   @return           Native thread priority
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
priority_ose_to_linux(OSPRIORITY ose_prio, enum PROCESS_TYPE proc_type)
{
    int prio;

    switch (proc_type)
    {
        case OS_PRI_PROC:
            prio = (PRIO_BASE_PRI_PROC + OSE_PRIORITY_LEVELS - 1) - ose_prio;
            break;
#ifdef ENABLE_INT_PROC
        case OS_INT_PROC:
            prio = (PRIO_BASE_INT_PROC + OSE_PRIORITY_LEVELS - 1) - ose_prio;
            break;

        case OS_PHANTOM:
            prio = PRIO_BASE_PHANTOM;
            break;
#else
        case OS_PHANTOM:
            /* lower the priority of phantom processes if we
               don't use interrupt processes.*/
            prio = PRIO_BASE_PRI_PROC + OSE_PRIORITY_LEVELS;
            break;
#endif

        case OS_BG_PROC:
            prio = 0;
            break;

        default:
            prio = 0;
            break;
    }

    return prio;
}

/** ==================================================================== */
/**
 *   Converts current scheduler priority to closest matching OSE priority
 *   process priority level.
 *
 *   @param linux_prio Native thread priority
 *
 *   @return           OSE priority
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
priority_linux_to_ose(int linux_prio, enum PROCESS_TYPE proc_type)
{
    int prio;

    switch (proc_type)
    {
        case OS_PRI_PROC:
            prio = (PRIO_BASE_PRI_PROC + OSE_PRIORITY_LEVELS - 1) - linux_prio;
            break;

        case OS_INT_PROC:
            prio = (PRIO_BASE_INT_PROC + OSE_PRIORITY_LEVELS - 1) - linux_prio;
            break;

        case OS_PHANTOM:
            prio = 0;
            break;

        case OS_BG_PROC:
            prio = 0;
            break;

        default:
            prio = 0;
            break;
    }

    return prio;
}

#ifdef ENABLE_INT_PROC
/** ==================================================================== */
/**
 *   Wait for events to interrupt processes (User Mode drivers).
 *
 *   @param uio_fd    Socket decriptor for User Mode IO
 *   @param itc_fd    File decriptor for ITC mailbox
 *
 *   @return           Event detected
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
wait_event(int uio_fd, int itc_fd)
{
    int nfsd = 0;
    fd_set rd;
    int retv = 0;
    int event = HW_INTERRUPT;
    tcb_t *tcb;
    int fsem_fd;

    tcb = lits_data;
    fsem_fd = tcb->fsem_pipe[0];	/* read end of the pipe */

    if (uio_fd > 0)
        nfsd = uio_fd + 1;
    if (itc_fd + 1 > nfsd)
        nfsd = itc_fd + 1;
    if (fsem_fd + 1 > nfsd)
        nfsd = fsem_fd + 1;

    do
    {
        FD_ZERO(&rd);
        FD_SET(uio_fd, &rd);
        FD_SET(itc_fd, &rd);
        FD_SET(fsem_fd, &rd);

        retv = select(nfsd, &rd, NULL, NULL, NULL);

        if (retv == -1 && errno != EINTR)
            lits_error("Failed to handle UIO/ITC event");

    }
    while (retv == -1);

    if ( retv == 0 )
        lits_error("Timeout when handling UIO/ITC event");

    if (FD_ISSET(uio_fd, &rd))
    {
        uint32_t tmp = 0;
        /* Read the interrupt */
        (void)read(uio_fd, &tmp, 4);
        event = HW_INTERRUPT;
    }
    else if (FD_ISSET(itc_fd, &rd))
    {
        event = RECEIVE_SIGNAL;
    }
    else if (FD_ISSET(fsem_fd, &rd))
    {
        char dummy = 0;
        /* Drain the fast semaphore pipe */
        (void)read(fsem_fd, &dummy, 1);
        event = FSEM_CALL;
    }
    else
    {
        lits_error("Unknown UIO/ITC event");
    }

    return event;
}

/** ==================================================================== */
/**
 *   Acknowledge interrupt events to User Mode drivers.
 *
 *
 *   @param uio_fd     Socket decriptor for User Mode IO
 *   @param event      Event type
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
event_handled(int uio_fd, int event)
{
    uint32_t tmp = 1;
    if (event == HW_INTERRUPT)
    {
        write(uio_fd, &tmp, 4);
    }
    return;
}

/** ==================================================================== */
/**
 *   Interrupt process function
 *
 *   @param func       Interrupt process function.
 *
 *   @return           -
 *
 *   @par Globals:
 *                     lits_data
 */
/* ===================================================================== */
static void
int_thread(OSENTRYPOINT *func)
{
    uint32_t tmp = 1;
    int uio_fd;
    int itc_fd;
    UIO_HANDLE_ uio_hdl;

    itc_fd = itc_get_fd();

    if ((uio_hdl = uio_open(lits_data->name)) == (UIO_HANDLE_)-1)
    {
        lits_error("Failed to obtain UIO handle for %s\n", lits_data->name);
    }
    uio_fd = uio_getfd(uio_hdl);
    write(uio_fd, &tmp, 4);
    while(1)
    {
        lits_data->current_event = wait_event(uio_fd, itc_fd);
        (func)();
        event_handled(uio_fd, lits_data->current_event);
    }
}
#endif /* ENABLE_INT_PROC */

/** ==================================================================== */
/**
 *   Phantom process function
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
phantom_thread(void)
{
    struct OS_redir_entry *entry = lits_data->redir_table;
    SIGSELECT             selAll[] = {0};
    union SIGNAL          *sig;
    int                   i;

    while(1)
    {
        sig = receive(selAll);

        if ( entry == NULL )
        {
            free_buf(&sig);
            continue;
        }

        for (i = 1; i < entry[0].sig; i++)
        {
            if ( sig->sig_no == entry[i].sig )
            {
                send_w_s(&sig, sender(&sig), entry[i].pid);

                break;
            }
        }

        if ( i >= entry[0].sig )
        {
            if ( entry[0].pid )
                send_w_s(&sig, sender(&sig), entry[0].pid);
            else
                free_buf(&sig);
        }
    }
}

/** ==================================================================== */
/**
 *   Help function to wait for start condition.
 *
 *   @param arg        Pointer to the TCB
 *
 *   @return           Always NULL
 *
 *   @par Globals:
 *                     --
 *
 *   Waits on the suspend condition. It will be signalled when starting a process
 *   after it is created or put to stopped state.
 */
/* ===================================================================== */
static void
wait_for_start(tcb_t *tcb)
{
    pthread_mutex_lock(&tcb->mutex);
    while (tcb->stop_cnt > 0)
    {
        pthread_cond_wait(&tcb->suspend_cond, &tcb->mutex);
    }
    pthread_mutex_unlock(&tcb->mutex);
    DBG_PRINT("Resuming: \"%s\" [%u]\n", tcb->name, tcb->pid);
}

/** ==================================================================== */
/**
 *   Starter function for threads.
 *
 *   @param arg        Pointer to the TCB
 *
 *   @return           Always NULL
 *
 *   @par Globals:
 *                     --
 *
 *   Creates a ITC mailbox with the current process name and waits for
 *   for start() before the actual process code is called. Phantom
 *   processes do not wait for start().
 *
 *   @verbatim
 *
 *   TODO: - Cleanup of after abnormal exit
 *
 *   @endverbatim
 */
/* ===================================================================== */
static void *
thread_start(void *arg)
{
    tcb_t         *tcb = arg;
    OSENTRYPOINT  *func = tcb->entrypoint;
    int r;

    pthread_mutex_lock(&tcb->mutex);

    lits_data = tcb;

    prctl(PR_SET_NAME, tcb->name, 0, 0, 0);

    bool static_pid_is_set = false;

    if ((tcb->proc_type == OS_PRI_PROC || tcb->proc_type == OS_BG_PROC) &&
            tcb->redir_table != NULL)
    {
        /* Go through the process attributes */
        for (int i = 1; i < tcb->redir_table[0].pid + 1; ++i)
        {
            if (PROC_ATTR_PID == tcb->redir_table[i].sig)
            {
                if (static_pid_is_set)
                    lits_error("Multiple definitions of attribute PROC_ATTR_PID");
                if (tcb->redir_table[i].pid < 1 || tcb->redir_table[i].pid > 63)
                    lits_error("User defined pid is out of range, allowed range 1-63");
                tcb->mb = itc_create_mailbox(tcb->name,
                                             ITC_MBOX_SET_STAT_ID(tcb->redir_table[i].pid));
                tcb->pid = tcb->redir_table[i].pid;
                static_pid_is_set = true;
            }
            else if (PROC_ATTR_POOLID == tcb->redir_table[i].sig)
            {
                /* Not implemented */
            }
            else
            {
                lits_error("Unknown process attribute");
            }
        }
    }

    /* Set dynamic pid if static pid is not already set */
    if (false == static_pid_is_set)
    {
        tcb->mb = itc_create_mailbox(tcb->name, 0);
        tcb->pid = tcb->mb;
    }

    if (tcb->mb == ITC_NO_ID)
        lits_error("Cannot create itc mailbox");

    tcb->stop_cnt = 1;
    DBG_PRINT("Inside Posix thread \"%s\": %u\n", tcb->name, current_process());

    if ((r = add_tcb(tcb)) != 0 )
        lits_error("Failed to add TCB entry, %d", r);

    /*
    ** Signal to the creator thread that tcb is ready
    */
    pthread_cond_signal(&tcb->init_cond);
    pthread_mutex_unlock(&tcb->mutex);

    if (tcb->proc_type == OS_PRI_PROC || tcb->proc_type == OS_BG_PROC)
    {
        wait_for_start(tcb);
    }

#ifdef ENABLE_INT_PROC
    if ( tcb->proc_type == OS_INT_PROC )
        int_thread(func);
    else
#endif
    {
        (func)();
        /* A cancellation point when a stopped thread is being cancelled. */
        delay(1);
    }
    /*
    ** Add functionality for processes that terminates abnormally
    */
    lits_error_handler(0, 0x107, tcb->pid);

    kill_proc(tcb->pid);

    return NULL;
}

/** ==================================================================== */
/**
 *   Signal handler that is called when a process/thread shall be stopped
 *
 *   @param signo      Signal number
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
sighand(int signo)
{
    tcb_t    *tcb = lits_data;

    assert(NULL != tcb);
    wait_for_start(tcb);
}

/* ========================================================================
 *   LITS Internal Process Handling Functions
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Search for a process in the current program.
 *
 *   @param name       Name of the process
 *
 *   @return           PID of the process or 0 if not found
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
PROCESS
lits_local_process(const char *name)
{
    tcb_t  *tcb;

    tcb = get_tcb_by_name(name);

    return (tcb != NULL ? tcb->pid : 0);
}

/* ========================================================================
 *   Global Process Handling Functions
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Returns the block ID or the specified process
 *
 *   @param pid        Process ID
 *
 *   @return           Block ID or 0 if the process is not found and the
 *                     error is ignored by the error handler
 *
 *   @par Globals:
 *                     --
 *
 *   This is mostly used to get environmental variables from current
 *   process. As it's not possible to access environmental variables of
 *   other processes with standard POSIX functions, the implementation
 *   of this function does not
 *
 *   get_env(get_bid(current_process()), "varname")
 */
/* ===================================================================== */
PROCESS
zzget_bid(PROCESS pid)
{
    tcb_t  *tcb;

    assert(lits_data);

    if ( (tcb = get_tcb(pid)) == NULL )
    {
        lits_error_handler(0, 0x32, pid);
        return 0;
    }

    // This is usually used to get environmental variables
    return block_data->pid;
}


/** ==================================================================== */
/**
 *   Returns the process ID of the current process.
 *
 *   @param            -
 *
 *   @return           Process ID if calling thread is a lits-thread,
 *                     otherwise 0.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
PROCESS
zzcurrent_process(void)
{
    if (lits_data)
        return lits_data->pid;

    return 0;
}


/** ==================================================================== */
/**
 *   Get an environment variable for a specific process or block.
 *
 *   @param id         Process or Block ID
 *   @param name       Name of environment variable
 *
 *   @return           Pointer to the environment variable or NULL
 *                     on error
 *
 *   @par Globals:
 *                     --
 *
 *   This is mostly used to get environmental variables from current
 *   process. As it's not possible to access environmental variables of
 *   other processes with standard POSIX functions, the implementation
 *   of this function does not
 *
 *   get_env(get_bid(current_process()), "varname")
 */
/* ===================================================================== */
char *
zzget_env(PROCESS id, const char *name)
{
    tcb_t  *tcb;
    char* result;

    assert(lits_data);

    if ( (tcb = get_tcb(id)) == NULL )
    {
        lits_error_handler(0, 0x32, id);
        return NULL;
    }

    // We can only access the environment of this process in POSIX by default.
    if (get_bid(id) != block_data->pid)
        lits_error("Cannot get environment variables outside this block");

    const char* value = find_env(&tcb->envp, name);

    if (value == NULL)
    {
        return NULL;
    }

    size_t length = strlen(value);

    result = (char*)zzalloc(length + 1, 0);

    // Environmental variable can change between calls to strlen and strncpy.
    strncpy(result, value, length);
    result[length] = 0;

    return result;
}

/** ==================================================================== */
/**
 *   List the environment variables available in the specified process or block
 *
 *   @param id         Process or Block ID
 *   @param name       A pointer to an environment variable name
 *
 *   @return           Returns a buffer obtained from the caller's
 *                     pool containing the names of the environment variables
 *                     available in the specified process or block.
 *
 *                     Names are separated by a single space and the entire
 *                     string is null-terminated
 *
 *                     The buffer must be freed by the caller when information
 *                     has been extracted.
 *
 *                     Returns an empty string "" if the process or block has
 *                     terminated or if no variables were found
 *
 *   @par Globals:
 *                     --
 *
 */
/* ===================================================================== */
char *
zzget_env_list(PROCESS id, const char *first_name)
{
    tcb_t *tcb;
    char *buf, *item, *bufp = NULL, *tmp_bufp = "", *ptr;
    int buf_len;

    assert(lits_data);

    if ( (tcb = get_tcb(id)) == NULL )
    {
        lits_error_handler(0, 0x32, id);
        goto out;
    }

    if (get_bid(id) != block_data->pid)
        lits_error("Cannot get environment variables outside this block");

    bufp = env_get_list(tcb->envp);
    if (bufp == NULL)
        goto out;

    if (first_name == NULL)
    {
        tmp_bufp = bufp;
        goto out;
    }

    /* search complete env list for first name */
    ptr = bufp;
    while ((item = strchr(ptr, ' ')) != NULL)
    {
        *item = 0;
        if (strcmp(ptr, first_name) == 0)
        {
            *item = ' ';
            /* Found fisrt name, save pointer */
            tmp_bufp = item + 1;
            goto out;
        }
        *item = ' ';
        ptr = ++item;
    }

out:
    buf_len = strlen(tmp_bufp);
    if ((buf_len + 1) > MAX_SIGSIZE)
    {
        tmp_bufp[MAX_SIGSIZE] = 0;
        ptr = strrchr(tmp_bufp, ' ');
        *ptr = 0;
        buf_len = strlen(tmp_bufp);
    }
    buf = (char *) zzalloc(buf_len + 1, 0);
    strcpy(buf, tmp_bufp);

    if (bufp != NULL)
        free(bufp);

    return buf;
}

/** ==================================================================== */
/**
 *   Set an environment variable for a specific process or block.
 *
 *   @param id         Process or Block ID
 *   @param name       Name of environment variable
 *   @param value      Value of environment variable. The variables is
 *                     removed if value is NULL
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSBOOLEAN
zzset_env(PROCESS id, const char *name, const char *value)
{
    tcb_t  *tcb;

    assert(lits_data);

    if ( (tcb = get_tcb(id)) == NULL )
    {
        lits_error_handler(0, 0x32, id);
        return -1;
    }

    // We can only access the environment of this process in POSIX by default.
    if (get_bid(id) != block_data->pid)
        lits_error("Cannot set environment variables outside this block");

    if (value == NULL)
        return delete_env(&tcb->envp, name);

    return add_env(&tcb->envp, name, value);
}

/** ==================================================================== */
/**
 *   Get a pointer stored in an environment variable for a
 *   specific process or block.
 *
 *   @param id         Process or Block ID. O means current process
 *   @param name       Name of environment variable
 *
 *   @return           The pointer or 0 if not found
 *
 *   @par Globals:
 *                     --
 *
 *   This is mostly used to get environmental variables from current
 *   process. As it's not possible to access environmental variables of
 *   other processes with standard POSIX functions, the implementation
 *   of this function does not
 *
 *   get_env(get_bid(current_process()), "varname")
 */
/* ===================================================================== */
OSADDRESS
zzget_envp(PROCESS id, const char *name)
{
    OSADDRESS  result = 0;
    char       *str;

    assert(lits_data);

    if ( id == 0 )
        id = current_process();

    if ( (str = zzget_env(id, name)) == NULL )
        return 0;

    if ( strlen(str) == sizeof(OSADDRESS)*2)
        result = strtoul(str, NULL, 16);

    free_buf((union SIGNAL **)&str);

    return result;
}

/** ==================================================================== */
/**
 *   Store a pointer in an environment variable for a specific
 *   process or block.
 *
 *   @param id         Process or Block ID. O means current process
 *   @param name       Name of environment variable
 *   @param value      The pointer to store
 *
 *   @return           0 on error, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSBOOLEAN
zzset_envp(PROCESS id, const char *name, OSADDRESS value)
{
    char  str[sizeof(OSADDRESS)*2+1];
    char  format[8];

    assert(lits_data);

    if ( id == 0 )
        id = current_process();

    sprintf(format, "%%0%dlx", (int)sizeof(OSADDRESS)*2);
    sprintf(str, format, value);

    return zzset_env(id, name, str) == 0 ? 1 : 0;
}

/** ==================================================================== */
/**
 *   Get priority of the specified process.
 *
 *   @param pid        Process ID
 *
 *   @return           Priority or 0 when process is not found
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSPRIORITY
zzget_pri(PROCESS pid)
{
    tcb_t *tcb;
    int policy;
    struct sched_param param;
    OSPRIORITY ret = UNKNOWN_PRIORITY;
    enum PROCESS_TYPE ptype;
    int rv;

    assert(lits_data);

    tcb = get_tcb(pid);
    if ( tcb == NULL )
        return ret;

    ptype = tcb->proc_type;
    switch (ptype)
    {
        case OS_BG_PROC:
        case OS_PHANTOM:
            rv = pthread_getschedparam(tcb->tid, &policy, &param);
            assert(0 == rv);
            ret = priority_linux_to_ose(param.sched_priority, ptype);
            break;

        case OS_PRI_PROC:
        case OS_INT_PROC:
        case OS_TI_PROC:
            rv = pthread_getschedparam(tcb->tid, &policy, &param);
            assert(0 == rv);
            if(policy != SCHED_OTHER)
                ret = priority_linux_to_ose(param.sched_priority, ptype);
            break;
        case OS_BLOCK:
        case OS_ZOOMBIE:
            break;

        default:
            lits_error("Internal error: unknown process type:%d", ptype);
            break;
    }

    return ret;
}


/** ==================================================================== */
/**
 *   Set priority of the current process.
 *
 *   @param newpri     Priority to set
 *
 *   @return           The new priority
 *
 *   @par Globals:
 *                     --
 *
 *   Old priority is returned if setting new priority fails.
 */
/* ===================================================================== */
OSPRIORITY
zzset_pri(OSPRIORITY newpri)
{
    struct sched_param param;
    int                policy;
    int                result;
    enum PROCESS_TYPE  ptype = get_ptype(current_process());

    assert(lits_data);

    if (ptype != OS_PRI_PROC)
        return UNKNOWN_PRIORITY;

    pthread_setschedprio(pthread_self(),
                         priority_ose_to_linux(newpri, ptype));
    result = pthread_getschedparam(pthread_self(), &policy, &param);

    if ( result != 0 )
        lits_error("Cannot get the spcified priority");

    if (policy == SCHED_OTHER)
        return UNKNOWN_PRIORITY;

    return priority_linux_to_ose(param.sched_priority, ptype);
}

/** ==================================================================== */
/**
 *   Retrieve the Porcess Control Block (PCB) of the specified process.
 *
 *   @param pid        Process ID
 *
 *   @return           Pointer to the PCB. Must be freed after use, by
 *                     using free_buf()
 *
 *   @par Globals:
 *                     --
 *
 *   @verbatim
 *
 *   This is used in following way:
 *
 *     pcb = get_pcb(current_process();
 *     foo(pcb->strings[pcb->name])
 *
 *   @endverbatim
 */
/* ===================================================================== */
struct OS_pcb *
zzget_pcb (PROCESS pid)
{
    static unsigned long dummy_stack[4] =
    {0xeeeeeeee, 0xeeeeeeee, 0xeeeeeeee, 0xeeeeeeee};
    struct OS_pcb* process_info;
    char* name;
    int name_length_aligned = 0;

    assert(lits_data);
    name = ipc_get_name(pid);
    if(name)
    {
        name_length_aligned = strlen(name);
        /* make sure 'wanted' field after process name gets properly aligned */
        name_length_aligned = ALIGN(name_length_aligned, sizeof(SIGSELECT));
    }
    process_info = (struct OS_pcb *)zzalloc(sizeof(struct OS_pcb)
                                            + name_length_aligned
                                            + sizeof(SIGSELECT), 0);
    /* Mostly unimplemented. */
    memset(process_info, 0, sizeof(struct OS_pcb));
    /* Process name is at position 0. */
    process_info->name = 0;

    process_info->stack_top = (OSADDRESS) &dummy_stack[1];
    process_info->stack_limit = (OSADDRESS) dummy_stack;
    process_info->max_sigsize = MAX_SIGSIZE;
    /*
     * TODO
     * Hopefully there are no different sized sigselects on remote
     * machines if this ever supports them.
     */
    process_info->sigsel_size = sizeof(SIGSELECT);

    if(name)
    {
        /* Copy and truncate name.*/
        size_t len = strlen(name);

        strncpy(process_info->strings, name, len);
        process_info->strings[len] = 0;
        /* Make sure first (and only) SIGSELECT entry in buffer is 0 until support
        for 'wanted' is added. */
        memset(&process_info->strings[name_length_aligned], 0, sizeof(SIGSELECT));
        ipc_free_name(name);
    }

    process_info->status = (OSADDRESS)ipc_get_state(pid);
    process_info->type =   (OSADDRESS)zzget_ptype(pid);
    process_info->fsemvalue = zzget_fsem(pid);
    process_info->priority =  zzget_pri(pid);
    process_info->remote_server = pid;
    /* point to the aligned 'wanted' field*/
    process_info->cpuregs = name_length_aligned;
    /*  point to the aligned 'wanted' field */
    process_info->wanted = name_length_aligned;

    return process_info;
}

/** ==================================================================== */
/**
 *   Get process type of the specified process.
 *
 *   @param pid        Process ID
 *
 *   @return           Process type
 *
 *   @par Globals:
 *                     --
 *
 *   All local processes (Linux endpoints) in other programs are
 *   assumed to be prioritized processes.
 */
/* ===================================================================== */
enum PROCESS_TYPE
zzget_ptype(PROCESS pid)
{
    tcb_t *tcb;
    struct itc_mbox_info *info;
    enum PROCESS_TYPE t;

    assert(lits_data);
    t = OS_ILLEGAL;

    if ( pid == block_data->pid )
        return OS_BLOCK;

    if ( (tcb = get_tcb(pid)) != NULL )
        return tcb->proc_type;

    info = itc_get_mailbox_info(pid);
    if (info)
    {
        switch (info->state)
        {
            case MBOX_DELETED:
                t = OS_ZOOMBIE;
                break;
            case MBOX_INUSE:
            case MBOX_CLONE:
            case MBOX_SHARED:
                t = OS_PRI_PROC;
                break;
            case MBOX_UNUSED:
            default:
                t = OS_ILLEGAL;
                break;
        }
        itc_free((union itc_msg **)&info);
    }
    return t;
}

/** ==================================================================== */
/**
 *   Kill the specified process.
 *
 *   @param pid        Process ID
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzkill_proc(PROCESS pid)
{
    union SIGNAL *sig;
    tcb_t     *tcb;
    SIGSELECT sel_z[] = {1, LITS_ZOMBIE_COLLECT_R};
    PROCESS my_pid;

    assert(lits_data);

    if ( (tcb = get_tcb(pid)) == NULL )
    {
        lits_error_handler(0, 0x32, pid);
        return;
    }

    if (pid == block_data->pid )
    {
        lits_print_message("Killing static block 0x%08x (%d).\n",
                           pid, pid);
        lits_error_handler(0, 0x80000048, pid);
        return;
    }

    my_pid = lits_data->pid;

    /* Let the daemon cleanup. */
    sig = zzalloc(sizeof(struct lits_z_collect), LITS_ZOMBIE_COLLECT);
    sig->lits_z_collect.tcb = tcb;
    send(&sig, daemon_pid);

    sig = receive(sel_z);
    free_buf(&sig);

    /* Shall not get here if commiting suiside, */
    if(pid == my_pid)
    {
        lits_error("Fatal: Thread cancel failed (%x).\n", pid);
    }
}

/** ==================================================================== */
/**
 *   A stub for create_block, not used by LITS.
 *
 *   @param name
 *   @param user
 *   @param use_remote_calls
 *   @param remote_call_server
 *   @param supervisor_mode
 *
 *   @return           1
 *
 *   @par Globals:
 *                     --
 *
 *   @verbatim
 *
 *   @endverbatim
 */
/* ===================================================================== */
PROCESS
zzcreate_block(char *name,
               OSUSER user,
               OSBOOLEAN use_remote_calls,
               PROCESS remote_call_server,
               OSBOOLEAN supervisor_mode)
{
    assert(lits_data);
    return 1;
}

/** ==================================================================== */
/**
 *   A stub for s_create_pool
 *
 *   @param poolid
 *   @param base
 *   @param size
 *   @param sigsizes
 *
 *   @return           returns a dummy pool value
 *
 *   @par Globals:
 *                     --
 *
 *   @verbatim
 *
 *   @endverbatim
 */
/* ===================================================================== */
OSPOOLID
zzs_create_pool(OSPOOLID poolid,
                OSADDRESS base,
                OSADDRESS size,
                OSBUFSIZE const *sigsizes)
{
    assert(lits_data);
    return (OSPOOLID) 1024;
}

/** ==================================================================== */
/**
 *   Creates a process, i.e. starts a new posix thread.
 *
 *   @param proc_type  Not used
 *   @param name       Name of the process
 *   @param entrypoint Entry point
 *   @param stack_size Stack sizs
 *   @param priority   Priority (mapped to pthread prio)
 *   @param timeslice  Not used
 *   @param block      Not used
 *   @param router_table
 *                     Redirection tables (only for phantom processes)
 *   @param vector     Not used
 *   @param user       Not used
 *
 *   @return           PID of the created process
 *
 *   @par Globals:
 *                     --
 *
 *   Creates a new threads and creates a new TCB table entry, which is
 *   inserted into the TCB table when the process is initalized. IPC
 *   handle is used as PID (Process ID).
 *
 *   @verbatim
 *
 *   TODO: - Handle non-prioritized processes
 *
 *   @endverbatim
 */
/* ===================================================================== */
PROCESS
zzcreate_process(enum PROCESS_TYPE proc_type,
                 const char *name,
                 OSENTRYPOINT *entrypoint,
                 OSADDRESS stack_size,
                 OSPRIORITY priority,
                 OSTIME timeslice,
                 PROCESS block,
                 struct OS_redir_entry *router_table,
                 OSVECTOR vector,
                 OSUSER user)
{
    pthread_attr_t      attr;
    struct sched_param  param;
    tcb_t               *tcb;
    PROCESS             pid;
    OSENTRYPOINT        *entry = entrypoint;
    int                 policy = lits_sched_policy;
    OSADDRESS           stack;
    cpu_set_t           cpuSet;
    long psz;
    unsigned int        min_stack = 0;
    char                *env;

    assert(lits_data);

    (void) timeslice;
    (void) block;
    (void) vector;
    (void) user;

    switch ( proc_type )
    {
        case OS_PRI_PROC:
            if ( router_table != NULL && router_table[0].sig > 1 )
                lits_error("Redirection is not supported on prioritized processes");
            break;

#ifdef ENABLE_INT_PROC
        case OS_INT_PROC:
            break;
#endif

        case OS_PHANTOM:
            if ( priority != 0 )
                lits_error("Only priority 0 is supported on phantom processes");

            entry = phantom_thread;
            break;

        case OS_BG_PROC:
            if ( router_table != NULL && router_table[0].sig > 1 )
                lits_error("Redirection is not supported on background processes");

            if ( priority != 0 )
                lits_error("Only priority 0 is supported on background processes");

            policy = SCHED_OTHER;
            break;

        default:
            lits_error("Process type not supported");
    }

    if ( (tcb = malloc(sizeof(tcb_t))) == 0 )
        lits_error("Cannot allocate memory for task control block");

    /* Clear the entire tcb block */
    memset(tcb, 0, sizeof(tcb_t));

    tcb->entrypoint = entry;
    tcb->proc_type  = proc_type;
    tcb->current_event = RECEIVE_SIGNAL;

    /* Add a core prefix to the process name if core is specified */
    if(assigned_cpu_core== UNDEFINED_CPU_CORE)
    {
        strncpy(tcb->name, name, MAX_PROC_NAME_LEN);
    }
    else
    {
        snprintf(tcb->name, MAX_PROC_NAME_LEN,"c%i_%s",  assigned_cpu_core, name);
    }
    tcb->name[MAX_PROC_NAME_LEN] = '\0';

    tcb->fsem = malloc(sizeof(sem_t));
    if (!tcb->fsem)
        lits_error("Failed to allocate fast semaphore");
    if (sem_init(tcb->fsem, 0, 0) == -1)
        lits_error("Failed to initialize fast semaphore");
#ifdef ENABLE_INT_PROC
    if (pipe(tcb->fsem_pipe) == -1)
        lits_error("Failed to create pipe for fast semaphore");
#endif

    if ( router_table )
    {
        size_t  size;

        if (OS_PRI_PROC == tcb->proc_type || OS_BG_PROC == tcb->proc_type)
        {
            size = router_table[0].pid + 1;
        }
        else     /* OS_PHANTOM */
        {
            size = router_table[0].sig;
        }

        if ( size < 1 )
            lits_error("Redirection do not have any entries");

        size = size * sizeof(struct OS_redir_entry);

        if ( (tcb->redir_table = malloc(size)) == NULL )
            lits_error("Cannot allocate memory for redirection table");

        memcpy(tcb->redir_table, router_table, size);
    }

    /*
     * Allow applications to define minimum stack size.
     */
#ifdef LITS_MIN_STACK_SIZE
    min_stack = LITS_MIN_STACK_SIZE;
#endif
    env = zzget_env(block_data->pid, "LITS_MIN_STACK_SIZE");
    if (env != NULL)
    {
        errno=0;
        min_stack = (unsigned int) strtoul(env, NULL, 10);
        if (errno)
        {
            lits_error("strtoul failed (%s)", strerror(errno));
        }
        free_buf((union SIGNAL **)&env);
    }

    if(min_stack)
    {
        stack_size = stack_size < min_stack ? min_stack : stack_size;
        DBG_PRINT("Min stack size set to=%d\n", min_stack);
    }
    /*
     * glibc stack allocation can return less stack than requested.
     * The requested stack size is;
     * - Aligned *down* to page size
     * - TLS (thread local storage) size is subtracted
     *
     * This is reported as a BUG report to WR but it might take a long time to be fixed.
     * For the time being fix this here;
     */

    psz = sysconf(_SC_PAGESIZE);
    assert (psz != -1);

    /* Static TLS size varies between platforms, but should not be
       larger than 128 bytes. */
    stack_size += 128;

    /* We have signal handler, reserve stack for that and align to page size. */
    stack_size+= SIGSTKSZ;
    stack_size = ALIGN(stack_size, psz);
    stack = stack_size < PTHREAD_STACK_MIN ? PTHREAD_STACK_MIN : stack_size;

    pthread_attr_init(&attr);
    pthread_attr_setschedpolicy(&attr, policy);

    if ( pthread_attr_setstacksize(&attr, stack) != 0 )
        lits_error("Failed to set stack size");

    param.sched_priority = priority_ose_to_linux(priority, proc_type);
    pthread_attr_setschedparam(&attr, &param);

    /* Core Affinity */
    if(assigned_cpu_core != UNDEFINED_CPU_CORE )
    {
        CPU_ZERO(&cpuSet);
        CPU_SET(assigned_cpu_core,&cpuSet);
        if (pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpuSet))
        {
            lits_error("Failed to set affinity ");
        }
    }

    if ( lits_sched_policy != SCHED_OTHER )
        pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED);

    pthread_cond_init(&tcb->init_cond, NULL);
    pthread_cond_init(&tcb->suspend_cond, NULL);
    pthread_mutex_init(&tcb->mutex, NULL);
    pthread_mutex_lock(&tcb->mutex);

    if ( pthread_create(&tcb->tid, &attr, thread_start, tcb) != 0 )
        lits_error("Failed to create a new process");

    /* Wait for thread initialization */
    pthread_cond_wait(&tcb->init_cond, &tcb->mutex);
    pthread_mutex_unlock(&tcb->mutex);
    pthread_cond_destroy(&tcb->init_cond);

    pid = tcb->pid;

    DBG_PRINT("Thread \"%s\" is created: %u\n", tcb->name, pid);

    return pid;
}

/** ==================================================================== */
/**
 *   Starts the process with the specified PID.
 *
 *   @param id         Process ID (PID)
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Silently ignored on processes other than prioritized.
 */
/* ===================================================================== */
void
zzstart(PROCESS id)
{
    tcb_t  *tcb;

    assert(lits_data);

    if ( (tcb = get_tcb(id)) == NULL )
    {
        lits_error_handler(0, 0x32, id);
        return;
    }

    if ((tcb->proc_type != OS_PRI_PROC) && (tcb->proc_type != OS_BG_PROC))
        return;

    pthread_mutex_lock(&tcb->mutex);

    if ( tcb->stop_cnt )
    {
        DBG_PRINT("Changing stop count %u --> %u for process \"%s\" [%u]\n",
                  tcb->stop_cnt, tcb->stop_cnt - 1, tcb->name, id);

        if ( --tcb->stop_cnt == 0 )
        {
            DBG_PRINT("Starting: \"%s\" [%u]\n", tcb->name, id);
            pthread_cond_signal(&tcb->suspend_cond);
        }
    }
    else
        lits_error_handler(0, 0x5b, id);

    pthread_mutex_unlock(&tcb->mutex);
}

/** ==================================================================== */
/**
 *   Stops the process with the specified PID.
 *
 *   @param id         Process ID (PID)
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Silently ignored on processes other than prioritized.
 */
/* ===================================================================== */
void
zzstop(PROCESS id)
{
    tcb_t  *tcb;

    assert(lits_data);

    if ( (tcb = get_tcb(id)) == NULL )
    {
        lits_error_handler(0, 0x32, id);
        return;
    }

    if ((tcb->proc_type != OS_PRI_PROC) && (tcb->proc_type != OS_BG_PROC))
        return;

    pthread_mutex_lock(&tcb->mutex);

    /* Check overflow */
    if ( ++tcb->stop_cnt == 0 )
        tcb->stop_cnt--;

    pthread_mutex_unlock(&tcb->mutex);

    DBG_PRINT("Changing stop count %u --> %u for process \"%s\" [%u]\n",
              tcb->stop_cnt - 1, tcb->stop_cnt, tcb->name, id);

    if ( tcb->stop_cnt == 1 )
    {
        DBG_PRINT("Stopping: \"%s\" [%u]\n", tcb->name, id);
        pthread_kill(tcb->tid, THREAD_STOP_SIGNAL);
    }
}

/** ==================================================================== */
/**
 *   Returns reason "wake up" of an interrupt process.
 *
 *   @param            -
 *
 *   @return           Wake up event: 0 - Interrupt, 1 - Signal recevied
 *
 *   @par Globals:
 *                     lits_data
 *
 *   NOTE! Will always return 1 if called from non-interrupt processes.
 */
/* ===================================================================== */
int
zzwake_up(void)
{
    assert(lits_data);
    return lits_data->current_event;
}

/** ==================================================================== */
/**
 *   Interrogates the fast semaphore of the specified process.
 *
 *   @param pid        Process ID (PID)
 *
 *   @return           Returns the current value of the fast semaphore.
 *
 *   @par Globals:
 *                     --
 *
 *   BUGS/LIMITATIONS:
 *
 *   Limited interrupt process semantic.
 */
/* ===================================================================== */
OSFSEMVAL
zzget_fsem(PROCESS pid)
{
    /* For remote processes this is a NOP */
    int value = -1;
    tcb_t *tcb;

    assert(lits_data);

    tcb = get_tcb(pid);
    if (tcb && tcb->fsem)
    {
        /* Examining the fast semaphore of an interrupt or
         * timer-interrupt process, or a process without a fast
         * semaphore, yields garbage. */
        if (tcb->proc_type != OS_INT_PROC)
        {
            if (sem_getvalue(tcb->fsem, &value) == -1)
            {
                value = -1;
                lits_error("Failed to get value of fast semaphore");
            }
        }
    }
    return (OSFSEMVAL) value;
}

/** ==================================================================== */
/**
 *   Initializes a fast semaphore for subsequent use.
 *
 *   @param value      The initial value
 *   @param pid        Process ID (PID)
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   BUGS/LIMITATIONS:
 *
 *   Limited interrupt process semantic.
 */
/* ===================================================================== */
void
zzset_fsem(OSFSEMVAL value, PROCESS pid)
{
    tcb_t *tcb;

    assert(lits_data);

    /* For remote processes this is a NOP */
    tcb = get_tcb(pid);
    if (tcb && tcb->fsem)
    {
        if ( tcb->proc_type == OS_INT_PROC )
        {
            lits_error("The set_fsem call is not available to interrupt processes");
            return;
        }
        /* It is illegal to set a fast semaphore to a negative value */
        if (value < 0)
        {
            lits_error("Illegal value for fast semaphore");
            return;
        }
        if (sem_init(tcb->fsem, 0, value) == -1)
        {
            value = 0;
            lits_error("Failed to set value of fast semaphore");
        }
    }
}

/** ==================================================================== */
/**
 *   Waits at the fast semaphore associated with the caller.
 *   The semaphore value is decreased by the specified amount. If the
 *   result is negative, the caller is suspended until some other
 *   process has issued enough signal_fsem calls for the value to
 *   become non-negative.
 *
 *   @param count      The number to decrease the fast semaphore with
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   BUGS/LIMITATIONS:
 *
 *   Limited interrupt process semantic.
 */
/* ===================================================================== */
void
zzwait_fsem(OSFSEMVAL count)
{
    /* For remote processes this is a NOP */
    tcb_t *tcb = lits_data;

    assert(lits_data);

    if (tcb && tcb->fsem)
    {
        if ( tcb->proc_type == OS_INT_PROC )
        {
            lits_error("The wait_fsem call is not available to interrupt processes");
            return;
        }
        while (count > 0)
        {
            while (sem_wait(tcb->fsem) == -1)
            {
                if (EINTR != errno)
                {
                    lits_error("Failed to wait on fast semaphore");
                    break;
                }
            }
            --count;
        }
    }
}

/** ==================================================================== */
/**
 *   Increments the value of the fast semaphore associated with the
 *   specified process.
 *   If the result is zero, the specified process is made ready if it
 *   had done a wait_fsem.
 *   If the process is an interrupt process, a wake_up invokation of the
 *   processed is scheduled.
 *
 *   @param pid        Process ID (PID)
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   BUGS/LIMITATIONS:
 *
 *   Limited interrupt process semantic.
 */
/* ===================================================================== */
void
zzsignal_fsem(PROCESS pid)
{
    /* For remote processes this is a NOP */
    tcb_t *tcb;

    assert(lits_data);

    tcb = get_tcb(pid);
    if (tcb && tcb->fsem)
    {
        if (tcb->proc_type != OS_INT_PROC)
        {
            int value = 0;
            if (sem_getvalue(tcb->fsem, &value) == -1)
                lits_error("Failed to get value of fast semaphore");
            /* It is illegal to cause a fast semaphore to wrap from high
             * positive numbers to a negative value for all processes but
             * interrupt processes (these are allowed to wrap since the
             * fast semaphore is only used to issue a wake_up call). */
            if (value == INT_MAX)
            {
                lits_error("Illegal signal of fast semaphore");
                return;
            }
        }
        if (sem_post(tcb->fsem) == -1)
            lits_error("Failed to signal fast semaphore");
        if (tcb->proc_type == OS_INT_PROC)
        {
            char dummy = 1;
            size_t ret = write(tcb->fsem_pipe[1], &dummy, 1);
            assert(-1 != ret);
        }
    }
}

/* ========================================================================
 *   Main Init and Start Functions
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Copies the environment strings from the provided strings to the
 *   Lits environment.
 *
 *   @param envp       Indirect pointer to environment
 *   @param strings    Indirect pointer to environemnt strings to add
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
inherit_env(void **envp, char **strings)
{
    char **ptr;
    char *val;

    for (ptr = strings; *ptr != NULL; ptr++)
    {
        if ( (val = strchr(*ptr, '=')) != NULL )
        {
            *val = '\0';
            add_env(envp, *ptr, val+1);
            *val = '=';
        }
    }
}

/** ==================================================================== */
/**
 *   Inherits environment to the Lits environment for the main thread.
 *   For backward compaitble reasons, environment are copied both from
 *   Linux environment and, if exists, the strings set in the Lits global
 *   data (__lits_environ).
 *
 *   @param envp       Indirect pointer to main thread environment
 *
 *   @return           -
 *
 *   @par Globals:
 *                    environ, __lits_environ
 */
/* ===================================================================== */
static void
inherit_to_lits_env(void **envp)
{
    if (__lits_environ[0] != NULL)
    {
        inherit_env(envp, __lits_environ);
    }

    inherit_env(envp, environ);
}

/** ==================================================================== */
/**
 *   Gets  environment variable set in osemain.con. The environment
 *   variable is searched for in the the Lits global data, __lits_environ.
 *   If Lits global data is empty, the Linux environment is searched for
 *   the varaibale.
 *   Format:
 *   variable name>=<variable value>'\0'<variable name>=<variable value>'\0'
 *
 *
 *   @param var_name   Environment variable name to get
 *   @param var_name   If found, indirect pointer to the variable value.
 *                     If not found, NULL is returned.
 *
 *   @return           -
 *
 *   @par Globals:
 *                     __lits_environ
 */
/* ===================================================================== */
static void
lits_get_env(const char *var_name, char **var_value)
{
    char *ptr;
    int size = strlen(var_name);
    int i;

    if ( __lits_environ[0] == NULL)
    {
        *var_value = getenv(var_name);
        return;
    }
    else
    {
        for (i = 0; __lits_environ[i] != NULL; i++)
        {
            ptr = __lits_environ[i];

            if ((strncmp(ptr, var_name, size) == 0) && (*(ptr + size) == '='))
            {
                *var_value = ptr + size + 1;
                return;
            }
        }
        *var_value = NULL;
    }
    return;
}

/** ==================================================================== */
/**
 *   Checks if cpu core usage should be restricted.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     assigned_cpu_core
 */
/* ===================================================================== */
static void
check_core_setting(void)
{
    char *val;

    val = get_env(lits_data->pid, "cpucore");

    if ( val == NULL )
        return;

    assigned_cpu_core = atoi(val);
    free_buf((union SIGNAL**) &val);

    if((assigned_cpu_core >= sysconf(_SC_NPROCESSORS_CONF)) &&
            (assigned_cpu_core != UNDEFINED_CPU_CORE))
    {
        lits_print_message("Unsupported, can only run on core 0 to %ld \n",
                           sysconf(_SC_NPROCESSORS_CONF)-1);
        exit(1);
    }
}

/** ==================================================================== */
/**
 *   Sets lits_sigrt_base.
 *        Reads the evn variable LITS_SIGRT_BASE, if set use it
 *        otherwise use LITS_SIGRT_BASE_DEFAULT.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     lits_sigrt_base
 */
/* ===================================================================== */
static void
set_lits_sigrt_base(void)
{
    char *val;

    lits_sigrt_base = LITS_SIGRT_BASE_DEFAULT;
    val = get_env(lits_data->pid, "LITS_SIGRT_BASE");

    if ( val == NULL )
        return;

    lits_sigrt_base = atoi(val);
    free_buf((union SIGNAL**) &val);

    if( (lits_sigrt_base > (SIGRTMAX - LITS_NR_SIGRT_USED +1)) ||
        (lits_sigrt_base < SIGRTMIN) )
    {
        lits_print_message("LITS_SIGRT_BASE: Illegal value(%d), supported"
                           " range: %d - %d \n",
                           lits_sigrt_base, SIGRTMIN,
                           SIGRTMAX - LITS_NR_SIGRT_USED +1);
        exit(1);
    }
}

/** ==================================================================== */
/**
 *   Get assigned CPU core
 *
 *   @param coreNr_p  Pointer to location where to store result
 *
 *   @return          true if core affinity is set, else false
 *
 *   @par Globals:
 *                    assigned_cpu_core
 */
/* ===================================================================== */
bool lits_get_assigned_core(uint32_t* const coreNr_p)
{
    bool cpu_core_is_defined;

    if(UNDEFINED_CPU_CORE==assigned_cpu_core)
    {
        cpu_core_is_defined = false;
    }
    else
    {
        cpu_core_is_defined = true;
        *coreNr_p = assigned_cpu_core;
    }
    return cpu_core_is_defined;
}

/** ==================================================================== */
/**
 *   Get number of CPUs
 *   *Note* This returns nr of CPUs online.
 *
 *   @param           -
 *
 *   @return          nr of CPUs online
 *
 *   @par Globals:
 *                    -
 */
/* ===================================================================== */
cpuid_t
zzose_num_cpus(void)
{
    long ncpus;

    ncpus = sysconf( _SC_NPROCESSORS_ONLN );
    assert(-1 != ncpus);
    return (cpuid_t)ncpus;
}

/** ==================================================================== */
/**
 *   Get CPUid
 *
 *   @param           -
 *
 *   @return          CPUid
 *
 *   @par Globals:
 *                    -
 */
/* ===================================================================== */
cpuid_t
zzose_cpu_id()
{
    cpuid_t cpu;

    cpu = (cpuid_t)sched_getcpu();
    assert(-1 != cpu);
    return cpu;
}

/** ==================================================================== */
/**
 *   Get real pid for a static pid
 *
 *   @param stpid     static pid
 *
 *   @return          Real pid
 *
 *   @par Globals:
 *                    --
 */
/* ===================================================================== */
PROCESS lits_get_real_pid(PROCESS stpid)
{
    return itc_get_real_mbox(stpid);
}

/** ==================================================================== */
/**
 *   Initiate ITC
 *
 *   @param -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void lits_init_itc()
{
    int32_t proc_max = MAX_PROCS_DEFAULT;
    itc_alloc_scheme aschm = ITC_MALLOC;
    union itc_scheme *schm;
    char *name_space = ITC_NO_NAMESPACE;
    uint32_t fl = 0;
    char *env, *token;
    unsigned int pool_sz = 0, i = 0;
    unsigned int lits_itc_prio = DEFAULT_ITC_PRIO;
    int sched_prio;
    int ret;
    struct sched_param sched_parameters;
    uint32_t sig_szs[8] = {0};

    /* Get values from environment */
    lits_get_env("MAX_PROCS", &env);
    if (env != NULL)
    {
        errno = 0;
        proc_max = (int32_t) strtol(env, NULL, 10);
        if (errno)
            lits_error("Failed to read MAX_PROCS environment");
        DBG_PRINT("MAX_PROCS=%d\n", proc_max);
    }
    lits_get_env("OSE_LM_POOL_SIZE", &env);
    if (env != NULL)
    {
        errno = 0;
        pool_sz = (unsigned int) strtol(env, NULL, 10);
        if (errno)
            lits_error("Failed to read OSE_LM_POOL_SIZE environment");
        aschm = ITC_POOL;
        DBG_PRINT("OSE_LM_POOL_SIZE=%d\n", pool_sz);
    }
    lits_get_env("OSE_LM_SIGNAL_SIZES", &env);
    if (env != NULL)
    {
        token = strtok(env, ",");
        while (token != NULL)
        {
            errno = 0;
            sig_szs[i++] = (uint32_t) strtol(token, NULL, 10);
            if (errno)
                lits_error("Failed to read OSE_LM_SIGNAL_SIZES environment");
            if (i >= 8)
                break;
            token = strtok(NULL, ",");
        }
        DBG_PRINT("OSE_LM_SIGNAL_SIZES=%d,%d,%d,%d,%d,%d,%d,%d\n",
                  sig_szs[0], sig_szs[1], sig_szs[2], sig_szs[3],
                  sig_szs[4], sig_szs[5], sig_szs[6], sig_szs[7]);
        aschm = ITC_POOL_FLEX;
    }

    /* Set itc scheme */
    switch (aschm)
    {
        case ITC_MALLOC:
            schm = NULL;
            break;
        case ITC_POOL:
            schm = malloc(sizeof(struct itc_pool_parameters));
            schm->pool_parameters.size = pool_sz;
            break;
        case ITC_POOL_FLEX:
            if (pool_sz == 0)
                lits_error("ITC_POOL not defined");
            schm = malloc(sizeof(struct itc_pool_flex_parameters));
            schm->pool_flex_parameters.size = pool_sz;
            for (i = 0; i < 8; i++)
                schm->pool_flex_parameters.msg_sizes[i] = sig_szs[i];
            break;
        default:
            lits_error("ITC allocation scheme not supported");
    }

    /* If we are running this init function with realtine prio's we need
     * to adjust the prio before calling itc_init
     */

    ret = pthread_getschedparam(pthread_self(), &sched_prio, &sched_parameters);
    if (ret)
    {
        lits_error("Failed to get scheduler parameters");
    }
    else
    {
        if (sched_prio != SCHED_OTHER)
        {
            /* We are running with realtime prio, so drop prio before itc_init
             * is called to lower the priority of the threads created in itc_init
             */
            int default_sched_prio = sched_parameters.sched_priority;

            /* Prio level of ITC threads can be optionally configured using an
             * environment variable
             */
            env = getenv("LITS_ITC_PRIO");
            if (env != NULL)
            {
                errno = 0;
                lits_itc_prio = (unsigned int) strtol(env, NULL, 10);
                if (errno)
                {
                    lits_error("Failed to read LITS_ITC_PRIO environment variable");
                }
                if ((lits_itc_prio < 0) || (lits_itc_prio > OSE_PRIORITY_LEVELS))
                {
                    lits_error("LITS_ITC_PRIO set in environment variable out of range");
                }
                DBG_PRINT("LITS_ITC_PRIO=%d\n", lits_itc_prio);
            }

            /* Drop priority level */
            memset(&sched_parameters, 0, sizeof(struct sched_param));
            sched_parameters.sched_priority = priority_ose_to_linux(lits_itc_prio,
                                              OS_PRI_PROC);
            ret = pthread_setschedparam(pthread_self(), sched_prio, &sched_parameters);
            if (ret)
            {
                lits_error("Failed to set scheduler parameters when dropping priority");
            }
            DBG_PRINT("Dropped prio level to (LITS level) %d before call to itc_init\n",
                      lits_itc_prio);
	    ret = itc_init(proc_max, aschm, schm, name_space, fl);
            if (ret != 0)
                lits_error("ITC init failed (%d). More information included in traces (if enabled).", ret);

            /* Restore priority level */
            sched_parameters.sched_priority = default_sched_prio;
            ret = pthread_setschedparam(pthread_self(), sched_prio, &sched_parameters);
            if (ret)
            {
                lits_error("Failed to set scheduler parameters when raising priority");
            }
            DBG_PRINT("Restored prio level after call to itc_init\n");

        }
        else
        {
            /* We are not running with realtime prio, so just call itc_init without
             * changing any priorities
             */
	    ret = itc_init(proc_max, aschm, schm, name_space, fl);
            if (ret != 0)
                lits_error("ITC init failed (%d). More information included in traces (if enabled).", ret);
        }
    }
}

/** ==================================================================== */
/**
 *   Create a daemon that reaps dead threads.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     daemon_pid
 *
 *   Should be called prior to zzos_main().
 */
/* ===================================================================== */
static void
create_lits_daemon(void)
{
    char name[80];

    sprintf(name, "lits_daemon_%d", getpid());
    daemon_pid = zzcreate_process(OS_PRI_PROC, name, lits_daemon, 65536, 30,
                                  (OSTIME)0,0,0,0,0);

    zzattach(NULL, daemon_pid);
    zzstart(daemon_pid);
}

/** ==================================================================== */
/**
 *   Check if the user wants to override the scheduling policy.
 *   Also if user has system permissions to run RT.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     lits_sched_policy
 *
 */
/* ===================================================================== */
static void
update_schedular(void)
{
    char* env;
    int policy;
    struct sched_param param;

    policy = lits_sched_policy;
    /*
     * We are called by the main thread, the mailbox is not created yet and
     * the tcb has not been inserted into the tree yet. Use Linux env
     */
    env = getenv("LITS_SCHED_POLICY");
    if (env != NULL)
    {
        if(0 == strcmp(env, "RR") || 0 == strcmp(env, "rr"))
        {
            policy = SCHED_RR;
        }
        else if(0 == strcmp(env, "FF") || 0 == strcmp(env, "ff"))
        {
            policy = SCHED_FIFO;
        }
        else if(0 == strcmp(env, "OTHER") || 0 == strcmp(env, "other"))
        {
            policy = SCHED_OTHER;
        }
        else
        {
            lits_error("Unkown sched policy:%s\n", env);
        }
    }

    if ( policy != SCHED_OTHER )
    {
        struct rlimit rlim;
        int ret;

        ret = getrlimit(RLIMIT_RTPRIO, &rlim);
        if ( ret == 0 && (rlim.rlim_cur >= MAX_USED_RT_PRIO))
        {
            param.sched_priority = priority_ose_to_linux(0, OS_PRI_PROC);
            ret = pthread_setschedparam(pthread_self(), policy, &param);
            if ( ret == EPERM )
            {
                policy = SCHED_OTHER;
            }
        }
        else
        {
            policy = SCHED_OTHER;
        }
    }

    lits_sched_policy = policy;
}
/** ==================================================================== */
/**
 *   Init of the Legacy IPC and Task support layer.
 *
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 *
 *   Should be called prior to zzos_main().
 */
/* ===================================================================== */
int
zzinit_os(int argc, char **argv)
{
    struct sigaction   actions;
    tcb_t              *tcb;

    if ( (tcb = malloc(sizeof(tcb_t))) == 0 )
        return 0;

    /* Clear the entire tcb block --> proc_type=OS_PRI_PROC */
    memset(tcb, 0, sizeof(tcb_t));

    lits_data = tcb;
    block_data = tcb;

    tcb->tid = pthread_self();

    strncpy(tcb->name, basename(argv[0]), MAX_PROC_NAME_LEN);
    tcb->name[MAX_PROC_NAME_LEN] = '\0';

    prctl(PR_SET_NAME, tcb->name, 0, 0, 0);

    update_schedular();
    lits_init_itc();

    if ((tcb->mb = itc_create_mailbox(tcb->name, 0)) == ITC_NO_ID)
        lits_error("Cannot create itc mailbox");
    tcb->pid = tcb->mb;

    /* tcb must be added before get_env/set_env can be called */
    if ( add_tcb(tcb) != 0 )
        lits_error("Failed to add TCB entry");

    inherit_to_lits_env(&tcb->envp);

    check_core_setting();
    set_lits_sigrt_base();

    memset(&actions, 0, sizeof(actions));
    sigemptyset(&actions.sa_mask);
    actions.sa_flags = 0;
    actions.sa_handler = sighand;

    if ( sigaction(THREAD_STOP_SIGNAL, &actions, NULL) != 0 )
    {
        lits_print_message("Error setting signal action\n");
        return -1;
    }

    /* Initiating time out functionality */
    lits_tmo_init();

    create_lits_daemon();
    return 0;
}

/** ==================================================================== */
/**
 *   Collect and cleanup the zombie thread.
 *
 *   @param sig       Signal containing tcb pointer
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   tcb is freed here, to guarantee that the memory is
 *   freed after thread cleanup.
 */
/* ===================================================================== */
static void
collect_zombie(union SIGNAL * sig)
{
    union SIGNAL *reply;
    tcb_t* tcb;
    tcb_t* mtcb;
    int ret;

    if (sig == NULL || (tcb = sig->lits_z_collect.tcb) == NULL)
        lits_error("Corrupt signal(%p) or tcb(%p)", sig, sig ? tcb : NULL);

    /*
     * Avoid race condition with simultanious kill_proc()
     */
    mtcb = get_tcb(tcb->pid);
    if(NULL == mtcb || mtcb->proc_type == OS_ZOOMBIE)
    {
        goto z_send_reply;
    }

    /* We do a lock here, only to ensure that the thread is not holding it.
     * When succeeding to take the lock we know the thread is running normally,
     * or is blocking in wait_for_start.
     */
    pthread_mutex_lock(&tcb->mutex);

    /* Reaping has begun. */
    tcb->proc_type = OS_ZOOMBIE;

    ret = pthread_cancel(tcb->tid) ;
    if (ret != 0)
        lits_error("pthread_cancel failed, err:%d", ret);

    /* cond_wait is a cancellation point, so this should not be needed.
     * But it can be that there is a stop signal pending while the
     * cancellation signal is sent and taken. I.e the cancellation handler get
     * interrupted by the stop handler that will block in cond_wait...
     * So we have to release that one so cancellation handler will run.
     */
    if ( tcb->stop_cnt != 0 )
    {
        tcb->stop_cnt = 0;
        pthread_cond_signal(&tcb->suspend_cond);
    }

    /* In case the thread is blocking in cond_wait, it will
     * try to grab the mutex before handling the cancel.
     * As we are holding the mutex, we must unlock the mutex here.
     * Otherwise the thread will never end and this code will get stuck
     * in the loop as the pthread_kill never returns ESRCH.
     * After this unlock, the mutex is in locked state by the thread
     * so it can not be used any more.
     * We could have a cleanup handler installed that unlocks the mutex, but
     * as we will destroy it anyway, we don't need to.
     */
    pthread_mutex_unlock(&tcb->mutex);

    /*
     * Note: There is a race condition here:
     * If a caller has claimed a tcb via get_tcb() and access's
     * the same after we free the tcb, a seg fault will occur!
     */
    ret = remove_tcb(tcb);
    if (ret != 0)
    {
        lits_error("remove_tcb failed(%d) for %s", ret, tcb->name);
    }

    ret = pthread_join(tcb->tid, NULL) ;
    if (ret != 0)
        lits_error("pthread_join failed, err:%d", ret);

    /* Cleanup and free tcb. */

    ret = destroy_env_tab(&tcb->envp);
    if (ret != 0)
        lits_error("destroy_env_tab failed, err:%d", ret);

    if (tcb->redir_table) free(tcb->redir_table);
    if (tcb->fsem) free(tcb->fsem);

    pthread_cond_destroy(&tcb->suspend_cond);
    pthread_mutex_destroy(&tcb->mutex);

    free(tcb);

z_send_reply:
    /* Reaping done, send sync signal, contents are not used/checked. */
    reply = zzalloc(sizeof(struct lits_z_collect), LITS_ZOMBIE_COLLECT_R);
    send(&reply, sender(&sig));
}


/** ==================================================================== */
/**
 *   LITS daemon
 *
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
lits_daemon(void)
{
    union SIGNAL     *sig;
    SIGSELECT        selAll[] = {0};

    while(1)
    {
        sig = receive(selAll);

        switch (sig->sig_no)
        {
            case LITS_ZOMBIE_COLLECT:
                collect_zombie(sig);
                break;
            default:
                lits_print_message("%s Unknown signal [%d] from process %d\n",
                                   __FUNCTION__, sig->sig_no, sender(&sig));
                break;
        }

        free_buf(&sig);
    }
}

/** ==================================================================== */
/**
 *   Main entry point
 *
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int
zzos_main(int argc, char **argv)
{
    union SIGNAL     *sig;
    SIGSELECT        selAll[] = {0};

    assert(lits_data);

    while(1)
    {
        sig = receive(selAll);

        switch (sig->sig_no)
        {
            case OS_ATTACH_SIG:
                lits_print_message("The static process 0x%08x (%d) terminated\n",
                                   sender(&sig), sender(&sig));
                lits_error_handler(0, 0x80000048, sender(&sig));
                return -1;
                break;
            default:
                lits_print_message("Unknown signal [%d] from process %d\n",
                                   sig->sig_no, sender(&sig));
                break;
        }

        free_buf(&sig);
    }

    stop(current_process());

    destroy_tcb_tab();

    return 0;
}

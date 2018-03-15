/**
 *   Inter process communication functions. Mainly wrappers around
 *   ITC.
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
 *   Revised : 2015-03-27 Ravineet Singh
 *   Change  : Removed LINX dependency. Removed trailing whitespaces.
 *
 *   Revised : 2015-03-16 Ravineet Singh EAB/FJP/HB
 *   Change  : Included tmoserver.h
 *
 *   Revised : 2015-02-24 Ravineet Singh EAB/FJP/HB
 *   Change  : Adapted receive[_xx] calls to lits_tmo_remove api change.
 *
 *   Revised : 2015-02-16 Ravineet Singh EAB/FJP/HB
 *   Change  : Added loop to all receive calls, in case the signal was a
 *             deleted timeout signal.
 *             Made lits local hunt default, i.e. is always used.
 *
 *   Revised : 2015-02-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Adapted to the change of lits_tmo_cleanup ->lits_tmo_remove.
 *
 *   Revised : 2014-10-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Fixed copy/paste error in zzreceive_from
 *
 *   Revised : 2014-10-03 Ravineet Singh EAB/FJP/HB
 *   Change  : Fixed copy/paste error in zzreceive
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Add assert(lits_data).
 *             lits functions must be called from a lits created thread.
 *
 *   Revised : 2014-02-14 Ravineet Singh EAB/FJP/HB
 *   Change  : Renamed the define LITS_TMP_TIME to ITC_OVER_LINX
 *
 *   Revised : 2014-03-20 Ravineet Singh EAB/FJP/HB
 *   Change  : Impl. hunt_from for itc only.
 *
 *   Revised : 2014-02-21 Lars Jönsson EAB/FJP/TB
 *   Change  : Updated after change of SIGNAL definition.
 *
 *   Revised : 2014-02-18 Daniel Nilsson EAB/FJB
 *   Change  : Changed implementation of hunt() when using ITC:
 *             - Hunt now return false when hunting remote processes and the
 *               application is listening for the hunt signal response.
 *               This avoids using "hunt_helper" towards remote processes.
 *
 *   Revised : 2014-02-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Changed implementation of hunt() when using ITC:
 *             - No local lookup is done when running real ITC, i.e. when
 *               ITC is not just a wrapper around Linx. ITC already checks
 *               local mailboxes first.
 *             - Hunt with signal is slighly improved. The signal is now
 *               sent directly from ITC. Synchronous ITC locate is used
 *               for checking if there is a direct hit.
 *
 *   Revised : 2014-02-06 Lars Jönsson EAB/FJP/TB
 *   Change  : Timeout entries are now cleaned up when receiving the
 *             corresponding timeout signal.
 *
 *   Revised : 2013-10-31 Stanislav Vovk
 *   Change  : Added s_alloc, wrapper towards zzalloc;
 *             s_alloc_nil, wrapper towards zzalloc;
 *             set_sigsize
 *
 *   Revised : 2013-10-28 Ravineet Singh EAB/FJP/HB
 *   Change  : Added zzalloc_nil, wrapper towards zzalloc
 *             Added zzrestore, stub.
 *             Added zzaddressee, ITC impl only.
 *
 *   Revised : 2013-09-26 Stanislav Vovk
 *   Change  : Added check that ITC types are included
 *
 *   Revised : 2013-09-25 Stanislav Vovk
 *   Change  : Added ITC support
 *
 *   Revised : 2012-02-09 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected problem in hunt(), where the zzhunt_helper
 *             endpoint was not closed.
 *
 *   Revised : 2011-12-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Added const declaration of some parameters in function
 *             calls.
 *
 *   Revised : 2011-10-20 Lars Jönsson EAB/FJP/TB
 *   Change  : Error codes from linx_send() and linx_send_w_s() are now
 *             handled. It is not an error to send a signal to a process
 *             that has died.
 *
 *   Revised : 2011-09-27 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected hunt(), because it crashed when hunting for a
 *             process in another program and name_ was set to NULL.
 *
 *   Revised : 2011-03-31 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <itc.h>
#include <tmoserver.h>
#include <string.h>
#include <osetypes.h>
#include <lits_internal.h>
#include <lits_extended.h>

#if !defined(_LITS_IS_ITC_OSE_)
#error "ipc needs OSE types to be wrapped around ITC types. \
Install header files provided in ipc instead of using the original \
OSE header files."
#endif

/* ========================================================================
*   DEFINITIONS
* ========================================================================
*/

static lits_alloc_hook lits_alloc_hook_func_p = NULL;
static lits_alloc_hook lits_free_hook_func_p = NULL;

/* ========================================================================
*   TYPE DEFINITIONS
* ========================================================================
*/

/* ========================================================================
*   DATA DECLARATIONS
* ========================================================================
*/

/* ========================================================================
*   FUNCTION PROTOTYPES
* ========================================================================
*/
/** ==================================================================== */
/**
*   Allocates a signal buffer and sets the signal number.
*
*   @param size       Size of buffer, including signal number
*   @param signo      Signal number to be set in the signal
*
*   @return           Pointer to the signal
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
union SIGNAL *
        zzalloc(OSBUFSIZE size, SIGSELECT signo)
{
    assert(lits_data);

    union SIGNAL *sig = (union SIGNAL *)itc_alloc(size, signo);

    if ( sig == NULL )
        lits_error_handler(0, 0x20, 0);

    if (NULL != lits_alloc_hook_func_p)
    {
        (*lits_alloc_hook_func_p)(0, sig);
    }

    return sig;
}

/** ==================================================================== */
/**
*   Allocates a signal buffer and sets the signal number, in OSE may
*   return nil. In Lits, just a wrapper to zzalloc.
*
*   @param size       Size of buffer, including signal number
*   @param signo      Signal number to be set in the signal
*
*   @return           Pointer to the signal
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
union SIGNAL *
        zzalloc_nil(OSBUFSIZE size, SIGSELECT signo)
{
    return zzalloc(size, signo);
}

/** ==================================================================== */
/**
*   Allocates a signal
*
*   @param pool       ignored
*   @param size       Size of buffer, including signal number
*   @param signo      Signal number to be set in the signal
*
*   @return           Pointer to the signal
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
union SIGNAL *
        zzs_alloc(OSPOOLID pool, OSBUFSIZE size, SIGSELECT signo)
{
    (void) pool;
    return zzalloc(size, signo);
}

/** ==================================================================== */
/**
*   Allocates a signal buffer and sets the signal number, in OSE may
*   return nil. In Lits, just a wrapper to zzalloc.
*
*   @param pool       ignored
*   @param size       Size of buffer, including signal number
*   @param signo      Signal number to be set in the signal
*
*   @return           Pointer to the signal
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
union SIGNAL *
        zzs_alloc_nil(OSPOOLID pool, OSBUFSIZE size, SIGSELECT signo)
{
    (void) pool;
    return zzalloc(size, signo);
}

/** ==================================================================== */
/**
*   Restore, i.e. steal a signal buffer.
*
*
*   @param sig        Signal to steal ownership of
*
*   @return           --
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
void
zzrestore(union SIGNAL *sig)
{
    assert(lits_data);

    /* ITC does not check ownership, discard. */
    (void)sig;
}

/** ==================================================================== */
/**
*   Attaches to a process.
*
*   @param sig        Signal to be sent when the attached process is
*                     killed
*   @param id         Process ID to attach to
*
*   @return           Attach reference to be used when detach() is called
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
OSATTREF
zzattach(union SIGNAL **sig, PROCESS id)
{
    assert(lits_data);

    if (sig == NULL) {
        union SIGNAL *s;
        s = alloc(sizeof(SIGSELECT), OS_ATTACH_SIG);
        return itc_monitor(id, (union itc_msg **)&s);
    } else {
        return itc_monitor(id, (union itc_msg **)sig);
    }
}

/** ==================================================================== */
/**
*   Removes a previous attach to a process.
*
*   @param attRef     Attach reference returned from attach()
*
*   @return           -
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
void
zzdetach(OSATTREF *attRef)
{
    assert(lits_data);
    itc_unmonitor(*attRef);
}

/** ==================================================================== */
/**
*   Frees a signal buffer.
*
*   @param sig        Signal buffer
*
*   @return           -
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
void
zzfree_buf(union SIGNAL **sig)
{
    assert(lits_data);
    if (NULL != lits_free_hook_func_p)
    {
        (*lits_free_hook_func_p)(0, *sig);
    }
    itc_free((union itc_msg **)sig);
}

/** ==================================================================== */
/**
*   Hunts for named path.
*
*   @param name       Process name
*   @param user       Not used
*   @param name_      Pointer to PID result. Only updated if not NULL
*   @param hint_sid   Signal to send when the process is found
*
*   @return           True if the process is found. Otherwise False
*
*   @par Globals:
*                     --
*
*   NOTE! Is this still true???
*   ---------------------------
*   This does not check if name has a valid link name (or any link at
*   all) so if the target process is not found, this will block until
*   such process comes online.
*/
/* ===================================================================== */
OSBOOLEAN
zzhunt(const char *name, OSUSER user, PROCESS *name_, union SIGNAL **hunt_sig)
{
    PROCESS pid;

    assert(lits_data);

    if ( user != 0 )
        lits_error("Hunt failed. Only user=0 is supported");

    if (name_ != NULL)
        *name_ = (PROCESS) 0;

    pid = lits_local_process(name);
    if ( pid != 0 )
    {
        if (name_ != NULL)
            *name_ = pid;

        if (hunt_sig != NULL)
            send_w_s(hunt_sig, pid, current_process());

        return True;
    }

    /*
    ** Search in all blocks
    */
    if (hunt_sig != NULL) {
        itc_locate_async(name, (union itc_msg **)hunt_sig, ITC_MY_MBOX);
    }

    pid = (PROCESS)itc_locate(name);

    if (pid == (PROCESS)ITC_NO_ID)
        return False;

    if (name_ != NULL)
        *name_ = pid;

    return True;
}

/** ==================================================================== */
/**
*   Hunts for remote named path.
*
*   @param link         itc link name
*   @param name      Process name
*   @param pid         Pointer to PID result. Only updated if not NULL
*
*   @return           True if the process is found. Otherwise False
*
*   @par Globals:
*                     --
*
*   NOTE! Is this still true???
*   ---------------------------
*   This does not check if name has a valid link name (or any link at
*   all) so if the target process is not found, this will block until
*   such process comes online.
*/
/* ===================================================================== */
OSBOOLEAN zzhunt_remote(const char *link, const char *name, PROCESS *pid)
{
    union SIGNAL {
        uint32_t sigNo;
    };
    char path[128];
    union SIGNAL *inSig;
    union itc_msg *locate_msg = NULL;
    PROCESS spid;
    struct timespec time;
    uint32_t msg_id;
    uint32_t sig_filter[] = {1, 0};

    assert(lits_data);

    if(link == NULL){
        lits_error("string link is NULL");
        return False;
    }

    if(name == NULL){
        lits_error("string name is NULL");
        return False;
    }

    if (pid != NULL)
        *pid = (PROCESS) 0;

    clock_gettime(CLOCK_REALTIME, &time);
    msg_id = ((time.tv_sec)%100)*1000000 + (time.tv_nsec)/1000; //get an unique msg_id in 100s time slot.
    sig_filter[1] = msg_id;

    snprintf(path, sizeof(path), "%s/%s", link, name);
    locate_msg = itc_alloc(sizeof(uint32_t), msg_id);
    itc_locate_async( path, &locate_msg, ITC_MY_MBOX );
    inSig = (union SIGNAL *)itc_receive(sig_filter,
                                        3000,
                                        ITC_FROM_ALL);

    if (inSig == NULL)
    {
        return False;
    }

    if (inSig->sigNo != msg_id) {
        lits_error("unexpected signal %d, expect to receive %d\n", inSig->sigNo, msg_id);
        return False;
    }

    spid = (PROCESS)itc_sender((union itc_msg *)inSig);
    itc_free((union itc_msg **)&inSig);

    if (spid == (PROCESS)ITC_NO_ID)
    {
        return False;
    }

    if (pid != NULL)
    {
        *pid = spid;
    }

    return True;
}

/** ==================================================================== */
/**
*   Hunt using an alias. This is a system call intended mainly
*   for link handlers and other network software.
*
*   @param name       Process name
*   @param user       Not used
*   @param name_      Pointer to PID result. Only updated if not NULL
*   @param hunt_sid   Signal to send when the process is found
*   @param from       The ID of the process specified as hunter
*
*   @return           True if the process is found. Otherwise False
*
*   @par Globals:
*                     --
*
*/
/* ===================================================================== */
OSBOOLEAN zzhunt_from(char *name,
                      OSUSER user,
                      PROCESS *name_,
                      union SIGNAL **hunt_sig,
                      PROCESS from)
{
    assert(lits_data);
    itc_locate_async(name, (union itc_msg **)hunt_sig, from);
    return True;
}

/** ==================================================================== */
/**
*   Receives a signal.
*
*   @param sigsel     Signal selection
*
*   @return           The signal
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
union SIGNAL *
        zzreceive(const SIGSELECT *sigsel)
{
    assert(lits_data);

    union SIGNAL *sig;
    int removed = 0;

    do{
        sig = (union SIGNAL *)itc_receive(sigsel, ITC_NO_TMO, ITC_FROM_ALL);
        removed = lits_tmo_remove(&sig);
    }while(removed);
    return sig;
}

/** ==================================================================== */
/**
*   Receives a signal from a specific process.
*
*   @param timeout    Timeout in ms
*   @param sigsel     Signal selection
*   @param from       Process ID of process to receive from
*
*   @return           The signal or NIL if no signal arrived within
*                     the specified timeout
*
*   @par Globals:
*                     --
*
*   NOTE! The timeout handling is currently not working correctly
*         If a (canceled) timeout signal is fetched, it will be
*         removed and the original timeout will be retriggered.
*/
/* ===================================================================== */
union SIGNAL *
        zzreceive_from(OSTIME timeout, const SIGSELECT *sigsel, PROCESS from)
{
    assert(lits_data);

    int removed = 0;
    union SIGNAL *sig;
    do {
        sig = (union SIGNAL *)itc_receive(sigsel, timeout, from);
        removed = 0;
        if ( sig)
        {
            removed = lits_tmo_remove(&sig);
        }
    }while(removed);

    return sig;
}

/** ==================================================================== */
/**
*   Receive a signal within a specified time.
*
*   @param timeout    Timeout in ms
*   @param sigsel     Signal selection
*
*   @return           The signal or NIL if no signal arrived within
*                     the specified timeout
*
*   @par Globals:
*                     --
*
*   NOTE! The timeout handling is currently not working correctly
*         If a (canceled) timeout signal is fetched, it will be
*         removedand the original timeout will be retriggered.
*/
/* ===================================================================== */
union SIGNAL *
        zzreceive_w_tmo(OSTIME timeout, const SIGSELECT *sigsel)
{
    assert(lits_data);

    union SIGNAL *sig;
    int removed;
    do {
        sig = (union SIGNAL *)itc_receive(sigsel, timeout, ITC_FROM_ALL);
        removed = 0;
        if ( sig )
        {
            removed = lits_tmo_remove(&sig);
        }
    }while(removed);

    return sig;
}

/** ==================================================================== */
/**
*   Sends a signal.
*
*   @param sig        Signal to send
*   @param to         Process ID of the receiving process
*
*   @return           -
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
void
zzsend(union SIGNAL **sig, PROCESS to)
{
    assert(lits_data);
    itc_send((union itc_msg **)sig, to, ITC_MY_MBOX);
}

/** ==================================================================== */
/**
*   Sends a signal with the specified addressee.
*
*   @param sig        Signal to send
*   @param from       Process ID of the "sending" process
*   @param to         Process ID of the receiving process
*
*   @return           -
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
void
zzsend_w_s(union SIGNAL **sig, PROCESS from, PROCESS to)
{
    assert(lits_data);
    itc_send((union itc_msg **)sig, to, from);
}

/** ==================================================================== */
/**
*   Retrieves the sender of a signal.
*
*   @param sig        The signal
*
*   @return           Process ID of the sender
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
PROCESS
zzsender(union SIGNAL **sig)
{
    assert(lits_data);
    return itc_sender((union itc_msg *)*sig);
}

/** ==================================================================== */
/**
*   Retrieves the size of a signal.
*
*   @param sig        The signal
*
*   @return           Signal size
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
OSBUFSIZE
zzsigsize(union SIGNAL **sig)
{
    assert(lits_data);
    return itc_size((union itc_msg *)*sig);
}

/** ==================================================================== */
/**
*   Retrieves the addressee of a signal.
*
*   @param sig        The signal
*
*   @return           Process ID of the addressee
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
PROCESS
zzaddressee(union SIGNAL **sig)
{
    assert(lits_data);
    return itc_receiver((union itc_msg *)*sig);
}

/** ==================================================================== */
/**
*   Resize a signal.
*
*   @param sig        The signal
*   @param newsize    new size of the signal
*
*   @return           0 for success or non-zero for failure
*
*   @par Globals:
*                     --
*/
/* ===================================================================== */
OSBOOLEAN
zzset_sigsize(union SIGNAL **sig, OSBUFSIZE newsize)
{
    assert(lits_data);
    return itc_setsize((union itc_msg *)*sig, newsize);
}

/** ==================================================================== */
/**
*   Register hook function for alloc().
*
*   @param hook_func_p Function pointer of hook function to execute
*
*   @par Globals:      lits_alloc_hook_func_p
*
*/
/* ===================================================================== */
void lits_register_alloc_hook(const lits_alloc_hook hook_func_p)
{
    lits_alloc_hook_func_p = hook_func_p;
}

/** ==================================================================== */
/**
*   Register hook function for free().
*
*   @param hook_func_p Function pointer of hook function to execute
*
*   @par Globals:      lits_alloc_hook_func_p
*
*/
/* ===================================================================== */
void lits_register_free_hook(const lits_alloc_hook hook_func_p)
{
    lits_free_hook_func_p = hook_func_p;
}

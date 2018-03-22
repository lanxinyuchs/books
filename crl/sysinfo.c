/**
 *   sysinfo handling functions
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2013-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-02-14 Lars Jönsson EAB/FJP/TB
 *   Change  : Updated after change of SIGNAL definition.
 *
 *   Revised : 2013-11-14 Stanislav Vovk
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "lits_internal.h"
#include "tcb.h"
#include "ose_sysinfo.h"

/** ==================================================================== */
/**
 *   Get process name
 *
 *   @param pid           process id
 *   @param size          buffer size
 *   @param name          pointer to buffer where name will be copied
 *
 *   @return              SYSINFO_NO_ERR       if successful
 *                        SYSINFO_EILLEGAL_PID if pid is illegal
 *
 *   @par Globals:
 *                     --
 *
 *   SYSINFO_EVALUE_TRUNCATED not supported
 */
/* ===================================================================== */
int zzsysinfo_proc_name(PROCESS pid, unsigned int size, char *name)
{
    unsigned int sz = size;
    int ret;

    if (sz > ITC_NAME_MAXLEN)
        sz = ITC_NAME_MAXLEN;
    ret = itc_get_name(pid, name, sz);

    if (!ret)
        return SYSINFO_EILLEGAL_PID;

    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Get process state
 *
 *   @param pid           process id
 *   @param state         pointer to state
 *   @param substate      not used
 *
 *   @return              SYSINFO_NO_ERR if successful
 *                        SYSINFO_EILLEGAL_PID if pid is illegal
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_proc_state(PROCESS pid,
                         unsigned int *state,
                         unsigned int *substate)
{
    tcb_t *tcb;
    (void) substate;

    if ((tcb = get_tcb(pid)) == NULL)
        return SYSINFO_EILLEGAL_PID;

    if (tcb->stop_cnt == 0)
        *state = SYSINFO_STATE_RUNNING;
    else
        *state = SYSINFO_STATE_STOPPED;

    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Get process prio
 *
 *   @param pid           process id
 *   @param prio          pointer to where to return priority
 *
 *   @return              SYSINFO_NO_ERR if successful
 *                        SYSINFO_EILLEGAL_PID if pid is illegal
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_proc_prio(PROCESS pid, OSPRIORITY *prio)
{
    tcb_t *tcb;

    if ((tcb = get_tcb(pid)) == NULL)
        return SYSINFO_EILLEGAL_PID;

    *prio = zzget_pri(pid);
    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Retrieves as much information as possible
 *   from a given attach reference
 *   Just a stub.
 *
 *   @param attref        attach reference
 *   @param info          Pointer to a sysinfo_system_attref_info
 *                        struct where the information is stored
 *
 *   @return              SYSINFO_NO_ERR if successful
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_system_attref_info(OSATTREF attref,
                                 struct sysinfo_system_attref_info *info)
{
    (void) attref;
    /* storing a dummy value */
    info->state = SYSINFO_ATTREF_UNAVAILABLE;
    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Get signal size
 *
 *   @param sigbuf        pointer to signal
 *   @param size          Pointer to where size is stored
 *
 *   @return              SYSINFO_NO_ERR
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_signal_size(const union SIGNAL *sigbuf,
                          OSBUFSIZE *size)
{
    *size = (OSBUFSIZE) itc_size((union itc_msg *)sigbuf);
    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Retrieves the unused pool space for a given pool ID.
 *   Just a stub.
 *
 *   @param poolid        pool id
 *   @param unused        Pointer to where unused pool is stored
 *
 *   @return              SYSINFO_NO_ERR if successful
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_pool_unused(OSPOOLID poolid, OSADDRESS *unused)
{
    (void) poolid;
    /* storing a dummy value */
    *unused = 0;
    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Retrieves the used pool space for a given pool ID.
 *   Just a stub.
 *
 *   @param poolid        pool id
 *   @param used          Pointer to where used pool is stored
 *
 *   @return              SYSINFO_NO_ERR if successful
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_pool_used(OSPOOLID poolid, OSADDRESS *used)
{
    (void) poolid;
    /* storing a dummy value */
    *used = 0;
    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Retrieves the pool ID for a given signal
 *   Just a stub.
 *
 *   @param sigbuf        signal
 *   @param poolid        pointer to pool id
 *
 *   @return              SYSINFO_NO_ERR if successful
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_signal_poolid(const union SIGNAL *sigbuf,
                            OSPOOLID *poolid)
{
    (void) sigbuf;
    /* storing a dummy value */
    *poolid = 0;
    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Retrieves the buffer size index for a given signal
 *   Just a stub.
 *
 *   @param sigbuf        signal
 *   @param index         pointer to index
 *
 *   @return              SYSINFO_NO_ERR if successful
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_signal_size_index(const union SIGNAL *sigbuf,
                                unsigned int *index)
{
    (void) sigbuf;
    /* storing a dummy value */
    *index = 0;
    return SYSINFO_NO_ERR;
}

/** ==================================================================== */
/**
 *   Retrieves the configured buffer sizes for a given pool ID
 *   Just a stub.
 *
 *   @param poolid        pointer to pool id
 *   @param sizes         Array to where the pool buffer sizes are copied
 *
 *   @return              SYSINFO_NO_ERR if successful
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int zzsysinfo_pool_buffer_sizes(OSPOOLID poolid, OSBUFSIZE *sizes)
{
    (void) poolid;
    (void) sizes;
    return SYSINFO_NO_ERR;
}

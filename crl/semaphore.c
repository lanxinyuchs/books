/**
 *   Semaphore handling functions.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2012 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-10-22 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <semaphore.h>
#include <errno.h>
#include <stdlib.h>
#include "lits_internal.h"


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

struct semaphore
{
     sem_t sem;
};

/** ==================================================================== */
/**
 *   Create a semaphore.
 *
 *   @param initial_val   initial value for the newly created semaphore
 *
 *   @return              pointer to new semaphore
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
SEMAPHORE*
zzcreate_sem(OSSEMVAL initial_val)
{
    sem_t* _sem;
    int rv;

    _sem = malloc(sizeof(sem_t));
    assert(_sem != 0);

    rv = sem_init(_sem, 0, initial_val);
    assert(rv == 0);
    return (SEMAPHORE*)_sem;
}

/** ==================================================================== */
/**
 *   Get the semaphore value.
 *
 *   @param sem   semaphore pointer
 *
 *   @return      The value of the semaphore
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSSEMVAL
zzget_sem(SEMAPHORE *sem)
{
    int rv;
    int sval;

    rv = sem_getvalue((sem_t*)sem, &sval);
    assert(rv == 0);
    return (OSSEMVAL)sval;
}

/** ==================================================================== */
/**
 *   Signal a semaphore.
 *
 *   @param sem   semaphore pointer
 *
 *   @return            --
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzsignal_sem(SEMAPHORE *sem)
{
    int rv;

    rv = sem_post((sem_t*)sem);
    assert(rv == 0);
}

/** ==================================================================== */
/**
 *   Wait for a semaphore.
 *
 *   @param sem   semaphore pointer
 *
 *   @return            --
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzwait_sem(SEMAPHORE *sem)
{
    while (sem_wait((sem_t*)sem) == -1)
    {
        if (EINTR != errno)
        {
            lits_error("Failed to wait on fast semaphore");
            break;
        }
    }
}

/** ==================================================================== */
/**
 *   Kill the semaphore.
 *
 *   @param sem   semaphore pointer
 *
 *   @return            --
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzkill_sem(SEMAPHORE *sem)
{
    int rv;

    rv = sem_destroy((sem_t*)sem);
    assert(rv == 0);
    free(sem);
}

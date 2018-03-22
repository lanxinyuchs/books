/**
 *   LITS tmoserver signal and function interface.
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
 *   Revised : 2014-04-11 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 *
 *   Revised : 2016-03-01 Magnus Lindberg
 *   Change  : Updated the tmo_wrap signal structure to allow for a wider
 *             magic pattern and a more unused sigsize.
 * ========================================================================
 */

#ifndef __TMOSERVER_H
#define __TMOSERVER_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <ose.h>
#include <lits_internal.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/** ==================================================================== */
/** @struct tmo_wrap
 *
 *  Wrap tmo sig to client to be able to detect in clients queue.
 *
 *  @param signo    signal no.
 *  @param ref      tmo ref
 */
/* ===================================================================== */
#define LITS_TMO_MAGIC    ("TmOsErVeR")
#define MAGIC_LEN         (10)
struct __attribute__ ((__packed__)) tmo_wrap
{
    SIGSELECT signo;
    OSTMOREF  ref;
    char      magic[MAGIC_LEN];
};

/** ==================================================================== */
/** @struct tmo_request
 *
 *  Request a tmo from tmoserver.
 *
 *  @param signo    signal no.
 *  @param timeout  timeout
 *  @param status   status; 0=ok, !=0 otherwise
 *  @param tmosigno tmosigno
 *  @param tmosigno tmosig
 *  @param ref      tmo ref
 */
/* ===================================================================== */
#define LITS_TMO_REQUEST   (LITS_TMO_SIGBASE + 1) /*!- SIGNO(struct tmo_request) -!*/
#define LITS_TMO_REQUEST_R (LITS_TMO_SIGBASE + 2) /*!- SIGNO(struct tmo_request) -!*/

struct tmo_request
{
    SIGSELECT    signo;
    int          status;
    OSTIME       timeout;
    SIGSELECT    tmosigno;
    union SIGNAL **tmosig;
    OSTMOREF     ref;
};

/** ==================================================================== */
/** @struct tmo_restart
 *
 *  Restart a tmo.
 *
 *  @param signo    signal no.
 *  @param status   status; 0=ok, !=0 otherwise
 *  @param ref      tmo ref
 *  @param timeout  timeout
 */
/* ===================================================================== */
#define LITS_TMO_RESTART   (LITS_TMO_SIGBASE + 3) /*!- SIGNO(struct tmo_restart) -!*/
#define LITS_TMO_RESTART_R (LITS_TMO_SIGBASE + 4) /*!- SIGNO(struct tmo_restart) -!*/

struct tmo_restart
{
    SIGSELECT    signo;
    int          status;
    OSTMOREF     ref;
    OSTIME       timeout;
};

/** ==================================================================== */
/** @struct tmo_cancel
 *
 *  Cancel a tmo.
 *
 *  @param signo    signal no.
 *  @param status   status; 0=ok, !=0 otherwise
 *  @param ref      tmo ref
 *  @param usersig  user signal
 */
/* ===================================================================== */
#define LITS_TMO_CANCEL   (LITS_TMO_SIGBASE + 5) /*!- SIGNO(struct tmo_cancel) -!*/
#define LITS_TMO_CANCEL_R (LITS_TMO_SIGBASE + 6) /*!- SIGNO(struct tmo_cancel) -!*/

struct tmo_cancel
{
    SIGSELECT    signo;
    int          status;
    OSTMOREF     ref;
    union SIGNAL *usersig;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Initiate tmo server funtionality.
 *   This funtion needs to be called prior to calling any tmo server
 *   functions.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern void lits_tmo_init(void);

/** ==================================================================== */
/**
 *   lits_tmo_remove: Called by receive[_w_tmo|_from] functions to remove
 *   canceled tmo's.
 *
 *   @param sig        Pointer pointer to signal
 *
 *   @return           If signal was removed
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int lits_tmo_remove(union SIGNAL **sig);

/** ==================================================================== */
/**
 *   Request a timer from tmo_server
 *
 *   @param       tmeout
 *
 *   @param       tmosigno
 *
 *   @param       tmosig
 *
 *   @return      tmo ref
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern OSTMOREF
tmo_server_request(OSTIME timeout, SIGSELECT tmosigno, union SIGNAL **tmosig);

/** ==================================================================== */
/**
 *   Request a timer restart from tmo_server
 *
 *   @param       tmoref
 *
 *   @param       timeout    new timeout
 *
 *   @return      new tmo ref
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern OSTMOREF tmo_server_restart(OSTMOREF tmoref, OSTIME timeout);

/** ==================================================================== */
/**
 *   Request a timer cancellation from tmo_server
 *
 *   @param       tmoref
 *
 *   @return      tmo signal
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern union SIGNAL* tmo_server_cancel(OSTMOREF tmoref);

/** ==================================================================== */
/**
 *   Get tmo server PID
 *
 *   @param       -
 *
 *   @return      Valid PID of tmo server
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
PROCESS
get_tmo_server_pid(void);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __TMOSERVER_H */

/**
 *   Extended API for LITS to handle functionality not part of OSE.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-11-19 Daniel Nilsson EAB/FJL/BLC
 *   Change  : First version.
 * ========================================================================
 */

#ifndef _LITS_EXTENDED_H
#define _LITS_EXTENDED_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef void (*lits_alloc_hook)(const OSPOOLID poolId,
                                union SIGNAL *const sig_p);


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

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
extern void lits_register_alloc_hook(const lits_alloc_hook hook_func_p);

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
extern void lits_register_free_hook(const lits_alloc_hook hook_func_p);

/** ==================================================================== */
/**
 *   Get real pid for a static pid
 *
 *   @param stpid     static pid
 *
 *   @return          Real pid
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
extern PROCESS lits_get_real_pid(PROCESS stpid);

#ifdef __cplusplus
}
#endif

#endif /* _LITS_EXTENDED_H */

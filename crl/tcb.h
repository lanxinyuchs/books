/**
 *   Routines for handling search trees for the task control blocks.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2011-2012 by Ericsson AB. All rights reserved. The
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
 *   Change  : Removed unused func, delete_tcb
 *
 *   Revised : 2012-05-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Added remove_tcb().
 *
 *   Revised : 2012-02-20 Lars Jönsson EAB/FJP/TB
 *   Change  : Made get_tcb_by_name() more robust by using const argument.
 *
 *   Revised : 2011-03-14 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __TCB_H
#define __TCB_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <lits_internal.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Destroys the pid and name based binary trees.
 *
 *   @param            -
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     tcb_name_tab, tcb_tab
 */
/* ===================================================================== */
extern int destroy_tcb_tab(void);

/** ==================================================================== */
/**
 *   Adds a new process/thread to the pid and named based binary trees.
 *
 *   @param tcb        Pointer to the new element
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     tcb_name_tab, tcb_tab
 */
/* ===================================================================== */
extern int add_tcb(tcb_t *tcb);

/** ==================================================================== */
/**
 *   Seraches for a process/thread with the specified pid.
 *
 *   @param pid        Process ID
 *
 *   @return           Pointer to the TCB or NULL is not found
 *
 *   @par Globals:
 *                     tcb_tab
 */
/* ===================================================================== */
extern tcb_t *get_tcb(PROCESS pid);

/** ==================================================================== */
/**
 *   Seraches for a process/thread with the specified name.
 *
 *   @param name       Process name
 *
 *   @return           Pointer to the TCB or 0 is not found
 *
 *   @par Globals:
 *                     tcb_tab
 */
/* ===================================================================== */
extern tcb_t *get_tcb_by_name(const char *name);

/** ==================================================================== */
/**
 *   Removes a process/thread from the pid and named based binary trees.
 *
 *   @param tcb        Pointer to the element to be removed
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     tcb_name_tab, tcb_tab
 */
/* ===================================================================== */
extern int remove_tcb(tcb_t *tcb);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */

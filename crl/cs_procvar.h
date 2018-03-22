/**
 *
 *   @file
 *   @version @(#) ClearCase ID: 
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
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

#ifndef CLS_PROCVAR_H
#define CLS_PROCVAR_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include "ose.h"
#include "osetypes.h"

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

/************************************************************************
 *
 *  Function name : Cs_addProcVar_i
 *
 *  Description   : Adds a global or static variable to the context of a
 *                  process or to the context of all processes within a block.
 *                  After this call the global variable will always contain a
 *                  value that is private for the process(es).
 *
 *  Arguments     : pid       The identity of the process or block.
 *		    var_pU32  Pointer to the variable.
 *                
 *  Return        : Zero if operation succedded, otherwise it returns a value
 *                  other than zero.
 *
 ************************************************************************/
int Cs_addProcVar_i(PROCESS pid,U32 *var_pU32, uint32_t multicore);


/************************************************************************
 *
 *  Function name : Cs_getProcVar_i()
 *
 *  Description   : Gets the private value of a global or static variable
 *                  for a process.
 *
 *  Arguments     : pid         The identity of the process.
 *		    value_pU32  The returned private value
 *                
 *  Return        : Zero if operation succedded, otherwise it returns a value
 *                  other than zero.
 *
 ************************************************************************/
int Cs_getProcVar_i(PROCESS pid, U32 *value_pU32);


/************************************************************************
 *
 *  Function name : Cs_setProcVar_i
 *
 *  Description   : Sets the private value of a global or static variable
 *                  for a process or for all processes within a block.
 *
 *  Arguments     : pid        The identity of the process or block.
 *		    value_U32  The private value
 *                
 *  Return        : Zero if operation succedded, otherwise it returns a value
 *                  other than zero.
 *
 ************************************************************************/
int Cs_setProcVar_i(PROCESS pid, U32 value_U32);



#endif /* CLS_PROCVAR_H */

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */

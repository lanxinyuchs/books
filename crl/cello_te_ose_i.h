/**
 *   This is a wrapper around the ose_i.h file that enables the
 *   Trace & Error Handling for interrupt and timer interrupt
 *   processes. The inclusions of this file shall replace the
 *   inclusion of the ose_i.h file.
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
 *   Revised : -
 *   Change  : And serveral very old comments.
 *
 *   Revised : 1997-09-17
 *   Change  : Updated according to the new rules for free_buf and FREE_BUF.
 *            FREE_BUF shall handle that a NIL pointer is passed to it.
 *            free_buf shall have the original OSE Delta semantics, although
 *            during a transition period we will accept NIL pointers to    
 *            free_buf as well, although it will generate an error message.
 *
 *   Revised : 1997-11-06
 *   Change  : Removed the temporary redefinition of free_buf. Now we are back
 *             to the OSE Delta original semantics of free_buf.
 *
 *   Revised : 2010-09-23 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

#ifndef CELLO_TE_OSE_H
#define CELLO_TE_OSE_H
 
/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <ose_i.h>               /* Original ose_i.h         */
#include "cello_te_handlers.h"   /* Internal T & E handlers  */

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/**
 * Macro: FREE_BUF()
 * Description:
 *   This macro is modified verion of the OSE Delta system call free_buf.
 *   The difference is that this macro can handle that a NIL pointer is
 *   passed to it. This can be very useful when writing the clean up
 *   code in the PROC_ERR section.
 */

#define FREE_BUF(sig)\
	do { if (*(sig)) free_buf(sig); } while(0)


/**
 * Redefine the OS_PROCESS macro to call a special handler
 * to make this process part of the Trace & Error Handling.
 */
#undef OS_PROCESS
#ifdef TRI_MULTICORE
#define OS_PROCESS(NAME)\
	extern void NAME ## __(void);\
	void NAME(void)\
	{\
	  static void *procInfo_p = 0;\
          OMCSF_runIntProcMc(NAME ## __, &procInfo_p);	\
	}\
	void NAME ## __(void)
#else
#define OS_PROCESS(NAME)\
	extern void NAME ## __(void);\
	void NAME(void)\
	{\
	  static void *procInfo_p = 0;\
          OMCSF_runIntProc(NAME ## __, &procInfo_p);	\
	}\
	void NAME ## __(void)
#endif

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

#ifdef __cplusplus
}
#endif

#endif /* CELLO_TE_OSE_H */

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */

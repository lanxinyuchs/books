/**
 *   This is a wrapper around the ose.h file that enables the
 *   Trace & Error Handling for prioritized and background
 *   processes. The inclusion of this file shall replace the
 *   inclusion of the ose.h file.
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
 *   Revised : 97/09/17
 *   Change  : Updated according to the new rules for free_buf and FREE_BUF.
*              FREE_BUF shall handle that a NIL pointer is passed to it.
 *             free_buf shall have the original OSE Delta semantics, although
 *             during a transition period we will accept NIL pointers to 
 *             free_buf as well, although it will generate an error message.
 *
 *   Revised : 1997-11-06
 *   Change  : Removed the temporary redefinition of free_buf. Now we are back
 *             to the OSE Delta original semantics of free_buf.
 *
 *   Revised : 2010-09-23 Anette Schött
 *   Change  : Updated according to template and added macros for Linux.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 *
 *   Revised : 2015-09-25 Anette Schött
 *   Change  : Add NO_LITS ifdef to remove LITS dependency for Linux.
 * ========================================================================
 */

#ifndef CELLO_TE_OSE_H
#define CELLO_TE_OSE_H
 
/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#ifndef NO_LITS
#include <ose.h>                 /* Original ose.h           */
#endif
#include "cello_te_handlers.h"   /* Internal T & E handlers  */

#ifdef __cplusplus
extern "C" {
#endif

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

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

#if ( defined(__cplusplus) && defined(TRI_MULTICORE) )
#define OS_PROCESS(NAME) \
	static void NAME ## __(void);\
	extern "C" void NAME(void)\
        {\
	  OMCSF_initPriProcMc();\
	  NAME ## __();\
	}\
	static void NAME ## __(void)
#elif (defined(__cplusplus) && !defined(TRI_MULTICORE) )
#define OS_PROCESS(NAME) \
	static void NAME ## __(void);\
	extern "C" void NAME(void)\
        {\
	  OMCSF_initPriProc();\
	  NAME ## __();\
	}\
	static void NAME ## __(void)
#endif

#if ( !defined(__cplusplus) && defined(TRI_MULTICORE) )
#define OS_PROCESS(NAME)\
	static void NAME ## __(void);\
	void NAME(void)\
        {\
	  OMCSF_initPriProcMc();\
	  NAME ## __();\
	}\
	static void NAME ## __(void)
#elif (!defined(__cplusplus) && !defined(TRI_MULTICORE) )
#define OS_PROCESS(NAME)\
	static void NAME ## __(void);\
	void NAME(void)\
        {\
	  OMCSF_initPriProc();\
	  NAME ## __();\
	}\
	static void NAME ## __(void)
#endif

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ===================================================================== */
#else    /* Linux implementation */
/* ===================================================================== */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include "itc.h"
#include "tri_proxy.h"    /* Declaration of TRI proxy functions   */

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
        do { if (*(sig)) itc_free(sig); } while(0)

/**
 * Redefine the OS_PROCESS macro to call a special handler
 * to make this process part of the Trace & Error Handling.
 */
#undef OS_PROCESS
#ifdef __cplusplus
#define OS_PROCESS(NAME) \
	static void NAME ## __(void);\
	extern "C" void NAME(void)\
        {\
	  initPriProc(#NAME);\
	  NAME ## __();\
	}\
	static void NAME ## __(void)
#else
#define OS_PROCESS(NAME)\
	static void NAME ## __(void);\
	void NAME(void)\
        {\
	  initPriProc(#NAME);\
	  NAME ## __();\
	}\
	static void NAME ## __(void)
#endif

/**
 *  Register a name for the current thread.
 *  Optional. This is similar to the process name in LITS/ITS. If you don't use
 *  this function then the name and group mask from the "default object" will be
 *  used when tracing.
 *
 *  @param  thread_name
 *           The name you want to give this thread
 *
 *  @return -
 *
 */
void tri_register_thread_name(const char *procName);


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */

#ifdef __cplusplus
}
#endif

#endif /* CELLO_TE_OSE_H */

/**
 *   The contents of this file declares the macros for error handling.
 * 
 *   Important: Since some of the macros consists of multiple statements
 *   and/or if statements without else clause, it is important that a
 *   macro only is used in compound statements, i.e. inside braces.
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
 *
 *   Revised : 1997-06-24
 *   Change  : Added __ERROR__ macro when compiling for host.
 *
 *   Revised : 1997-06-30
 *   Change  : Added the COMP_CHECK macro.
 *
 *   Revised : 1997-08-08
 *   Change  : Changed to CHECK and ERROR macro to always jump to the PROC_ERR
 *             section even if the trace groups CHECK and ERROR are disabled.
 *             This is to ensure that the semantics of the CHECK and ERROR macro
 *              is kept even if the actual logging is not performed.
 *
 *   Revised : 1997-08-15 
 *   Change  : Added the ERROR_UNEXPECTED_SIG macro.
 *
 *   Revised : 2009-09-30 xcssamo
 *   Change  : WRNae57637 - Added SET_STR_FLAG in ERROR trace primitive 
 *             to set the 24-bit in admFlags which means that STR
 *             macro is used inside these trace macros.
 *
 *   Revised : 2009-10-21 xcssadh
 *   Change  : UABtr75501 - Removed GCC4 warning in C++ environment.
 *
 *   Revised : 2009-12-18 xcssamo
 *   Change  : WRNae70553 - Removed usage of SET_STR_FLAG in ERROR trace primitive
 *             to avoid warnings.
 *
 *   Revised : 2010-11-11xpadupp 
 *   Change  : Override the declaration of __ERROR__ for CPP TARGET (Non  SIM)
 *             as same as SIM to avoid the linking errors in CPP9.
 *
 *   Revised : 2010-12-08 xpadupp
 *   Change  : Modified declaration of _ERROR_ so that same single declaration
 *             gets called for both TARGET and SIM .So that we can avoid pclint
 *             warnings for simcello built.
 *
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added macros for Linux.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 * ========================================================================
 */

#ifndef CELLO_TE_ERROR_H
#define CELLO_TE_ERROR_H
 

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include "cello_te_group.h"       /* Internal trace group declarations */
#include "cello_te_handlers.h"    /* Internal T & E handlers           */

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
 * Macro: PROC_LOOP
 * Description:
 *   Marks the beginning of the process loop.
 */ 
#define PROC_LOOP\
        PROC_LOOP_BEGIN: while (1)


/**
 * Macro: PROC_ERR
 * Description:
 *   Marks the beginning of the error handling part of a function.
 *
 *   Note: This macro calls __ERROR__ which is recognized by the Diab
 *         compiler to generate compile time error whenever execution
 *         for some reason continues into the PROC_ERR section.
 */ 
#define PROC_ERR\
        __ERROR__("Execution continues into the PROC_ERR section", 1);\
	ERROR_LABEL


/**
 * Macro: RESUME
 * Description:
 *   Clears the error status and resumes to the beginning of the
 *   process loop. Shall only be used int the PROC_ERR section of
 *   of the function that implements the main process loop.
 *
 */ 
#define RESUME\
	CLEAR_ERROR_STATUS;\
	goto PROC_LOOP_BEGIN
 

/**
 * Macro: CLEAR_ERROR_STATUS
 * Description:
 *   Clears the error status for the current process.
 *
 */ 
#ifdef TRI_MULTICORE
#define CLEAR_ERROR_STATUS\
  (((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p->statusMask = 0)
#else
#define CLEAR_ERROR_STATUS\
  (OMCSF_procInfo_p->statusMask = 0)
#endif

/**
 * Macro: CHECK_ERROR_STATUS
 * Description:
 *   Checks whether error status indicates any errors.
 */
#ifdef TRI_MULTICORE
#define CHECK_ERROR_STATUS \
        if (((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p->statusMask)\
        {\
          OMCSF_raiseStatusMc(OMCSF_GROUP_CHECK,\
                              __FILE__, __LINE__, "ERROR STATUS");	\
          goto ERROR_LABEL;\
        }
#else
#define CHECK_ERROR_STATUS \
  if (OMCSF_procInfo_p->statusMask)	\
        {\
          OMCSF_raiseStatus(OMCSF_GROUP_CHECK,\
                            __FILE__, __LINE__, "ERROR STATUS");	\
          goto ERROR_LABEL;\
        }
#endif

/**
 * Macro: DO()
 * Description:
 *   This macro shall be used for function calls to ensure that
 *   error recovery is performed whenever an error is detected.
 *
 */ 
#define DO(FUNC)  /* I: Function to call */\
	(FUNC); CHECK_ERROR_STATUS


/**
 * Macro: CHECK()
 * Description:
 *   General error checking. Used for checking that a condition is
 *   fulfilled. If the condition is not fulfilled, an error is reported
 *   and a jump is made to PROC_ERR.
 */
#ifdef TRI_MULTICORE
#define CHECK(CONDITION)  /* I: Condition to check */\
        if (!(CONDITION))\
        {\
	  OMCSF_raiseStatusMc(OMCSF_GROUP_CHECK,\
			      __FILE__, __LINE__, #CONDITION);	\
	  goto ERROR_LABEL;\
	}
#else
#define CHECK(CONDITION)  /* I: Condition to check */\
        if (!(CONDITION))\
        {\
	  OMCSF_raiseStatus(OMCSF_GROUP_CHECK,\
			    __FILE__, __LINE__, #CONDITION);	\
	  goto ERROR_LABEL;\
	}
#endif
/**
 * Macro: COMP_CHECK()
 * Description:
 *   This macro is the compile time version of the CHECK macro.
 *   It can for example be used for checking that two different
 *   structures have the same size:
 *
 *     COMP_CHECK(sizeof(struct a) == sizeof(struct b));
 **
 *   Note: This macro calls __ERROR__ which is recognized by the Diab
 *         compiler to generate compile time error whenever CONDITION
 *         evaluates to false.
 */
#define COMP_CHECK(CONDITION)  /* I: Condition to check during compile time */\
	if (!(CONDITION)) \
	{\
	   __ERROR__("Compile time check failed: " #CONDITION, 1);\
	}


/**
 * Macro: ERROR()
 * Description:
 *   Reports an error and jumps to PROC_ERR.
 */
#ifdef TRI_MULTICORE
#define ERROR(MSG)   /* I: Message string */\
        OMCSF_raiseStatusMc(OMCSF_GROUP_ERROR, __FILE__, __LINE__, MSG);	\
	goto ERROR_LABEL
#else
#define ERROR(MSG)   /* I: Message string */\
        OMCSF_raiseStatus(OMCSF_GROUP_ERROR, __FILE__, __LINE__, MSG);	\
	goto ERROR_LABEL
#endif

/**
 * Macro: ERROR_UNEXPECTED_SIG()
 * Description:
 *   This is a special version of the ERROR macro that reports that
 *   an unexpected signal has been received. The actual logged message
 *   contains the signal number and the sender of the signal. When
 *   this message has been logged, a jump is made to PROC_ERR in the
 *   same way as ERROR does.
 */
#ifdef TRI_MULTICORE
#define ERROR_UNEXPECTED_SIG(SIG)   /* I: Pointer to received signal */\
        OMCSF_raiseUnexpectedSigMc(OMCSF_GROUP_ERROR, __FILE__, __LINE__, SIG); \
	goto ERROR_LABEL
#else
#define ERROR_UNEXPECTED_SIG(SIG)   /* I: Pointer to received signal */\
        OMCSF_raiseUnexpectedSig(OMCSF_GROUP_ERROR, __FILE__, __LINE__, SIG); \
	goto ERROR_LABEL
#endif


/**
 * Macro: __ERROR__() 
 * Description:
 *   Whenever the Diab compiler is not used, just define __ERROR__
 *   as ERROR to call the run time error handling instead.
 *
 */
#define __ERROR__(MSG, LEVEL) ERROR(MSG)


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ===================================================================== */
#else    /* LINUX implementation */
/* ===================================================================== */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include "tri_proxy.h"    /* Declaration of TRI proxy functions   */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/**
 * Macro: PROC_LOOP
 * Description:
 *   Marks the beginning of the process loop.
 */ 
#define PROC_LOOP\
        PROC_LOOP_BEGIN: while (1)


/**
 * Macro: PROC_ERR
 * Description:
 *   Marks the beginning of the error handling part of a function.
 *
 *   Note: This macro calls __ERROR__ which is recognized by the Diab
 *         compiler to generate compile time error whenever execution
 *         for some reason continues into the PROC_ERR section.
 */ 
#define PROC_ERR\
/*lint -e527*/\
        __ERROR__("Execution continues into the PROC_ERR section", 1);\
/*lint +e527*/\
	ERROR_LABEL


/**
 * Macro: RESUME
 * Description:
 *   Clears the error status and resumes to the beginning of the
 *   process loop. Shall only be used int the PROC_ERR section of
 *   of the function that implements the main process loop.
 *
 */ 
#define RESUME\
	CLEAR_ERROR_STATUS;\
	goto PROC_LOOP_BEGIN


/**
 * Macro: CLEAR_ERROR_STATUS
 * Description:
 *   Clears the error status for the current process.
 *
 */ 
#define CLEAR_ERROR_STATUS\
	(procInfo->statusMask = 0)


/**
 * Macro: CHECK_ERROR_STATUS
 * Description:
 *   Checks whether error status indicates any errors.
 */ 
#define CHECK_ERROR_STATUS \
	if (procInfo->statusMask)\
        {\
          triRaiseStatus(GROUP_CHECK,\
                      __SHORT_FILE__, __LINE__, "ERROR STATUS");      \
	  goto ERROR_LABEL;\
	}


/**
 * Macro: DO()
 * Description:
 *   This macro shall be used for function calls to ensure that
 *   error recovery is performed whenever an error is detected.
 *
 */ 
#define DO(FUNC)  /* I: Function to call */\
	(FUNC); CHECK_ERROR_STATUS


/**
 * Macro: CHECK()
 * Description:
 *   General error checking. Used for checking that a condition is
 *   fulfilled. If the condition is not fulfilled, an error is reported
 *   and a jump is made to PROC_ERR.
 */ 
#define CHECK(CONDITION)  /* I: Condition to check */\
        if (!(CONDITION))\
        {\
	  triRaiseStatus(GROUP_CHECK,\
                         __SHORT_FILE__, __LINE__, #CONDITION);       \
	  goto ERROR_LABEL;\
	}


/**
 * Macro: COMP_CHECK()
 * Description:
 *   This macro is the compile time version of the CHECK macro.
 *   It can for example be used for checking that two different
 *   structures have the same size:
 *
 *     COMP_CHECK(sizeof(struct a) == sizeof(struct b));
 *
 *   Note: This macro calls __ERROR__ which is recognized by the Diab
 *         compiler to generate compile time error whenever CONDITION
 *         evaluates to false.
 */
#define COMP_CHECK(CONDITION)  /* I: Condition to check during compile time */\
	if (!(CONDITION)) \
	{\
	   __ERROR__("Compile time check failed: " #CONDITION, 1);\
	}


/**
 * Macro: ERROR()
 * Description:
 *   Reports an error and jumps to PROC_ERR.
 */ 
#define ERROR(MSG)   /* I: Message string */\
        triRaiseStatus(GROUP_ERROR, __SHORT_FILE__, __LINE__, MSG);\
	goto ERROR_LABEL


/**
 *  Macro: ERROR_UNEXPECTED_SIG()
 * Description:
 *   This is a special version of the ERROR macro that reports that
 *   an unexpected signal has been received. The actual logged message
 *   contains the signal number and the sender of the signal. When
 *   this message has been logged, a jump is made to PROC_ERR in the
 *   same way as ERROR does.
 */ 
#define ERROR_UNEXPECTED_SIG(SIG)   /* I: Pointer to received signal */\
        triRaiseUnexpectedSig(GROUP_ERROR, __SHORT_FILE__, __LINE__, SIG);\
	goto ERROR_LABEL


/**
 * Macro: __ERROR__() 
 * Description:
 *   Whenever the Diab compiler is not used, just define __ERROR__
 *   as ERROR to call the run time error handling instead.
 *
 */
#define __ERROR__(MSG, LEVEL) ERROR(MSG)


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

#endif /* CELLO_TE_ERROR_H */

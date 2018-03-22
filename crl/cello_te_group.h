/**
 *   This file containt internal trace group definitions for the
 *   Trace & Error handling.
 *
 *   @file
 * 
 *   Copyright (C) 2013-2015 by Ericsson AB. All rights reserved. The
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
 *   Revised : 1998-09-26
 *   Change  : Added the IF_GROUP_IS_ENABLED macro.
 *
 *   Revised : 1998-10-01
 *   Change  : Merging the new Interface Trace functionality.
 *
 *   Revised : 1998-10-09
 *   Change  : Added the new trace group OMCSF_GROUP_INTERFACE.
 *             Changed the default trace group mask to include
 *             this new trace group.
 *
 *   Revised : 1998-10-28
 *   Change  : Added the new generic trace group concept, i.e. a generalization
 *             of the interface trace concept.
 *
 *   Revised : 1998-10-28
 *   Change  : The previous checkin comment was misleading. The new generic
 *             interface object concept was added, which is a generalization of
 *             the interface trace concept, i.e. the interface trace concept
 *             is only a special case of the interface object concept.
 *
 *   Revised : 2011-03-28 Sangita Mohanty
 *   Change  : Changed the datatype of OMCSF_groupE from enum to U32.
 *
 *   Revised : 2012-04-12 Padmaiah U
 *   Change  : Added defined(SPARC) so that stdint.h header file gets included
 *             when build is done using bs -sparc command.
 *
 *   Revised : 2012-07-12 Padmaiah U
 *   Change  : HQ10046: Modified _builtin_expect declaration
 *             so that compiler do effective code optimization.
 *
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added macros for Linux.
 *
 *   Revised : 2013-12-13 M. Winberg
 *   Change  : Change: Redefined OMCSF groups to their Linux equivalents
 *             in Linux part of the file.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 *
 *   Revised : 2015-01-07 Anette Schött
 *   Change  : Update OBJ_GROUP_IS_ENABLED to use legacy name for trace
 *             object pointer.
 * ========================================================================
 */

#ifndef CELLO_TE_GROUP_H
#define CELLO_TE_GROUP_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#if defined(__linux__) && !defined(OSE_DELTA)
#define LNX 1
#endif


/* Conditional stdint.h file inclusion */
#if defined(PPC) || defined(I686) || defined(OSE_DELTA) || defined(SPARC) || defined(LNX)
#include <stdint.h>
#endif

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
 * Macro: GROUP_IS_ENABLED()
 * Description:
 *   This macro is used for check if the specified GROUP is
 *   enabled in the groupMask for the currently executing
 *   process.
 *
 */
#ifdef TRI_MULTICORE
#define GROUP_IS_ENABLED(GROUP) /* I: Group number to check */\
        ( __builtin_expect(!!(((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].groupMask & (1 << (int)(GROUP))), 0) )
#else
#define GROUP_IS_ENABLED(GROUP) /* I: Group number to check */\
  ( __builtin_expect(!!(OMCSF_procInfo_p->groupMask & (1 << (int)(GROUP))), 0) )
#endif

/**
 * Macro: IF_GROUP_IS_ENABLED()
 * Description:
 *   This macro is used for check if the specified GROUP is
 *   enabled in the groupMask for the interface currently
 *   being executed disregarding which process executes.
 *
 */
#define IF_GROUP_IS_ENABLED(GROUP) /* I: Group number to check */\
  ( __builtin_expect(!!(OMCSF_interfaceInfo_p->groupMask & (1 << (int)(GROUP))), 0) )


/**
 * Macro: OBJ_GROUP_IS_ENABLED()
 * Description:
 *   This macro is used for check if the specified GROUP is
 *   enabled in the groupMask for the trace object, wherefor which a trace macro
 *   currently is beeing  executed, disregarding which process it executes in 
 *   or in which interface (source code module) code is executed.
 *
 */
#define OBJ_GROUP_IS_ENABLED(TRACEOBJ, /* I: Name of trace object to check */\
                             GROUP)   /* I: Group number to check */\
  ( __builtin_expect(!!(OMCSF_traceObjInfo ## TRACEOBJ ## _p->groupMask & (1 << (int)(GROUP))), 0) )


/**
 * Macro: RAISE_STATUS()
 * Description:
 *   This macro is used for raising the specified STATUS in
 *   the statusMask for the currently executing process.
 *
 */
#ifdef TRI_MULTICORE
#define RAISE_STATUS(STATUS) /* I: Status number to raise */\
        (((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p->statusMask |= (1 << (int)STATUS))
#else
#define RAISE_STATUS(STATUS) /* I: Status number to raise */\
  (OMCSF_procInfo_p->statusMask |= (1 << (int)STATUS))
#endif

/**
 * Macro: STATUS_IS_RAISED()
 * Description:
 *   This macro is used for checking if the specified STATUS is
 *   raised in the statusMask for the currently executing
 *   process.
 *
 */
#ifdef TRI_MULTICORE
#define STATUS_IS_RAISED(STATUS) /* I: Status number to check */\
        (((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p->statusMask & (1 << (int)(STATUS)))
#else
#define STATUS_IS_RAISED(STATUS) /* I: Status number to check */\
  (OMCSF_procInfo_p->statusMask & (1 << (int)(STATUS)))
#endif

/**
 * Default trace group mask
 */
#define DEFAULT_GROUP_MASK  ((1 << (int)OMCSF_GROUP_CHECK) |\
                             (1 << (int)OMCSF_GROUP_ERROR) |\
                             (1 << (int)OMCSF_GROUP_INFO) |\
                             (1 << (int)OMCSF_GROUP_INTERFACE) | \
                             (1 << (int)OMCSF_GROUP_TRACE_OBJ))

/**
 * OMCSF_groupE values, see description below
 */
#define OMCSF_GROUP_CHECK        (0)  /* Check failed               */ 
#define OMCSF_GROUP_ERROR        (1)  /* Error raised               */
#define OMCSF_GROUP_ENTER        (2)  /* Enter trace group          */
#define OMCSF_GROUP_RETURN       (3)  /* Return trace group         */
#define OMCSF_GROUP_INFO         (4)  /* Information message        */
#define OMCSF_GROUP_TRACE1       (5)  /* General trace group 1      */
#define OMCSF_GROUP_TRACE2       (6)  /* General trace group 2      */
#define OMCSF_GROUP_TRACE3       (7)  /* General trace group 3      */
#define OMCSF_GROUP_TRACE4       (8)  /* General trace group 4      */
#define OMCSF_GROUP_TRACE5       (9)  /* General trace group 5      */
#define OMCSF_GROUP_TRACE6       (10) /* General trace group 6      */
#define OMCSF_GROUP_TRACE7       (11) /* General trace group 7      */
#define OMCSF_GROUP_TRACE8       (12) /* General trace group 8      */
#define OMCSF_GROUP_TRACE9       (13) /* General trace group 9      */
#define OMCSF_GROUP_STATE_CHANGE (14) /* State changes              */
#define OMCSF_GROUP_BUS_SEND     (15) /* Bus message sent           */
#define OMCSF_GROUP_BUS_RECEIVE  (16) /* Bus message received       */
#define OMCSF_GROUP_REC_SIG      (17) /* Signal received            */
#define OMCSF_GROUP_SEND_SIG     (18) /* Signal received            */
#define OMCSF_GROUP_PARAM        (19) /* Signal received            */
#define OMCSF_GROUP_INTERFACE    (20) /* Interface traces           */
#define OMCSF_GROUP_TRACE_OBJ    (21) /* Trace object traces        */
#define OMCSF_GROUP_CONTINUATION (22) /* Continuation of large      */ 
                                      /* BUS_SEND/BUS_RECEIVE       */
#define OMCSF_GROUP_RESERVED1    (23) /* Reserved for future use... */                
#define OMCSF_GROUP_RESERVED2    (24) /* Reserved for future use... */
#define OMCSF_GROUP_RESERVED3    (25) /* Reserved for future use... */
#define OMCSF_GROUP_RESERVED4    (26) /* Reserved for future use... */
#define OMCSF_GROUP_RESERVED5    (27) /* Reserved for future use... */

/** 
 * User defined errors and trace groups. Since trace and error
 * handling is performed on a per process basis, these groups may
 * be redefined and used freely in any user process.
 */
#define OMCSF_GROUP_USER1        (28) /* User defined group 1       */
#define OMCSF_GROUP_USER2        (29) /* User defined group 2       */
#define OMCSF_GROUP_USER3        (30) /* User defined group 3       */
#define OMCSF_GROUP_USER4        (31) /* User defined group 4       */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/**
 * OMCSF_groupE
 * Description:
 *   Trace groups and assertions used by the Trace & Error handling.
 */
typedef uint32_t OMCSF_groupE;


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ===================================================================== */
#else    /* Linux implementation */
/* ===================================================================== */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/**
 *  Macro: GROUP_IS_ENABLED()
 * Description:
 *   This macro is used for check if the specified GROUP is
 *   enabled in the groupMask for the currently executing
 *   process.
 *
 */
#define GROUP_IS_ENABLED(GROUP) /* I: Group number to check */\
   ( __builtin_expect(!!(procInfo->groupMask & (1 << (int)(GROUP))), 0) )



/**
 *  Macro: IF_GROUP_IS_ENABLED()
 * Description:
 *   This macro is used for check if the specified GROUP is
 *   enabled in the groupMask for the interface currently
 *   being executed disregarding which process executes.
 *
 */
#define IF_GROUP_IS_ENABLED(GROUP) /* I: Group number to check */\
  ( __builtin_expect(!!(interfaceInfo->groupMask & (1 << (int)(GROUP))), 0) )


/**
 *  Macro: OBJ_GROUP_IS_ENABLED()
 * Description:
 *   This macro is used for check if the specified GROUP is
 *   enabled in the groupMask for the trace object, wherefor which a trace macro
 *   currently is beeing  executed, disregarding which process it executes in 
 *   or in which interface (source code module) code is executed.
 *
 */
#define OBJ_GROUP_IS_ENABLED(TRACEOBJ, /* I: Name of trace object to check */\
                             GROUP)   /* I: Group number to check */\
  ( __builtin_expect(!!(OMCSF_traceObjInfo ## TRACEOBJ ## _p->groupMask & (1 << (int)(GROUP))), 0) )


/**
 *  Macro: RAISE_STATUS()
 * Description:
 *   This macro is used for raising the specified STATUS in
 *   the statusMask for the currently executing process.
 *
 */
#define RAISE_STATUS(STATUS) /* I: Status number to raise */\
        (procInfo->statusMask |= (1 << (int)STATUS))


/**
 *  Macro: STATUS_IS_RAISED()
 * Description:
 *   This macro is used for checking if the specified STATUS is
 *   raised in the statusMask for the currently executing
 *   process.
 *
 */
#define STATUS_IS_RAISED(STATUS) /* I: Status number to check */\
        (procInfo->statusMask & (1 << (int)(STATUS)))


/*
 * Default trace group mask
 */
#ifndef TRI_DEFAULT_GROUP_MASK
#define TRI_DEFAULT_GROUP_MASK  ((1 << (int)GROUP_CHECK) |\
                                 (1 << (int)GROUP_ERROR) |      \
                                 (1 << (int)GROUP_INFO) |       \
                                 (1 << (int)GROUP_INTERFACE) |  \
                                 (1 << (int)GROUP_TRACE_OBJ))

#define DEFAULT_GROUP_MASK TRI_DEFAULT_GROUP_MASK
#endif

/**
 * OMCSF_groupE values, see description below
 */
#define GROUP_CHECK        (0)  /* Check failed               */ 
#define GROUP_ERROR        (1)  /* Error raised               */
#define GROUP_ENTER        (2)  /* Enter trace group          */
#define GROUP_RETURN       (3)  /* Return trace group         */
#define GROUP_INFO         (4)  /* Information message        */
#define GROUP_TRACE1       (5)  /* General trace group 1      */
#define GROUP_TRACE2       (6)  /* General trace group 2      */
#define GROUP_TRACE3       (7)  /* General trace group 3      */
#define GROUP_TRACE4       (8)  /* General trace group 4      */
#define GROUP_TRACE5       (9)  /* General trace group 5      */
#define GROUP_TRACE6       (10) /* General trace group 6      */
#define GROUP_TRACE7       (11) /* General trace group 7      */
#define GROUP_TRACE8       (12) /* General trace group 8      */
#define GROUP_TRACE9       (13) /* General trace group 9      */
#define GROUP_STATE_CHANGE (14) /* State changes              */
#define GROUP_BUS_SEND     (15) /* Bus message sent           */
#define GROUP_BUS_RECEIVE  (16) /* Bus message received       */
#define GROUP_REC_SIG      (17) /* Signal received            */
#define GROUP_SEND_SIG     (18) /* Signal received            */
#define GROUP_PARAM        (19) /* Signal received            */
#define GROUP_INTERFACE    (20) /* Interface traces           */
#define GROUP_TRACE_OBJ    (21) /* Trace object traces        */
#define GROUP_CONTINUATION (22) /* Continuation of large      */ 
                                      /* BUS_SEND/BUS_RECEIVE */
#define GROUP_RESERVED1    (23) /* Reserved for future use... */                
#define GROUP_RESERVED2    (24) /* Reserved for future use... */
#define GROUP_RESERVED3    (25) /* Reserved for future use... */
#define GROUP_RESERVED4    (26) /* Reserved for future use... */
#define GROUP_RESERVED5    (27) /* Reserved for future use... */

/** 
 * User defined errors and trace groups. Since trace and error
 * handling is performed on a per process basis, these groups may
 * be redefined and used freely in any user process.
 */
#define GROUP_USER1        (28) /* User defined group 1       */
#define GROUP_USER2        (29) /* User defined group 2       */
#define GROUP_USER3        (30) /* User defined group 3       */
#define GROUP_USER4        (31) /* User defined group 4       */


/* Re-map OSE defines to Linux */  
#define OMCSF_GROUP_CHECK           GROUP_CHECK 
#define OMCSF_GROUP_ERROR           GROUP_ERROR
#define OMCSF_GROUP_ENTER           GROUP_ENTER
#define OMCSF_GROUP_RETURN          GROUP_RETURN
#define OMCSF_GROUP_INFO            GROUP_INFO
#define OMCSF_GROUP_TRACE1          GROUP_TRACE1
#define OMCSF_GROUP_TRACE2          GROUP_TRACE2
#define OMCSF_GROUP_TRACE3          GROUP_TRACE3
#define OMCSF_GROUP_TRACE4          GROUP_TRACE4
#define OMCSF_GROUP_TRACE5          GROUP_TRACE5
#define OMCSF_GROUP_TRACE6          GROUP_TRACE6
#define OMCSF_GROUP_TRACE7          GROUP_TRACE7
#define OMCSF_GROUP_TRACE8          GROUP_TRACE8
#define OMCSF_GROUP_TRACE9          GROUP_TRACE9
#define OMCSF_GROUP_STATE_CHANGE    GROUP_STATE_CHANGE
#define OMCSF_GROUP_BUS_SEND        GROUP_BUS_SEND
#define OMCSF_GROUP_BUS_RECEIVE     GROUP_BUS_RECEIVE
#define OMCSF_GROUP_SEND_SIG        GROUP_SEND_SIG
#define OMCSF_GROUP_REC_SIG         GROUP_REC_SIG
#define OMCSF_GROUP_PARAM           GROUP_PARAM
#define OMCSF_GROUP_INTERFACE       GROUP_INTERFACE
#define OMCSF_GROUP_TRACE_OBJ       GROUP_TRACE_OBJ
#define OMCSF_GROUP_CONTINUATION    GROUP_CONTINUATION
#define OMCSF_GROUP_RESERVED1       GROUP_RESERVED1
#define OMCSF_GROUP_RESERVED2       GROUP_RESERVED2
#define OMCSF_GROUP_RESERVED3       GROUP_RESERVED3
#define OMCSF_GROUP_RESERVED4       GROUP_RESERVED4
#define OMCSF_GROUP_RESERVED5       GROUP_RESERVED5
#define OMCSF_GROUP_USER1           GROUP_USER1
#define OMCSF_GROUP_USER2           GROUP_USER2
#define OMCSF_GROUP_USER3           GROUP_USER3
#define OMCSF_GROUP_USER4           GROUP_USER4

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
/* ===================================================================== */

typedef uint32_t OMCSF_groupE;

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

#endif /* CELLO_TE_GROUP_H */
    

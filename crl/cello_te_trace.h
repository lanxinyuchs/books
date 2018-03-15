/**
 *   The contents of this file declares the macros for trace handling.
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
 *   Revised : -
 *   Change  : And serveral very old comments.
 *
 *   Revised : 1997-06-30
 *   Change  : Added the TRACE_ERROR macro.
 *
 *   Revised : 1997-07-29
 *   Change  : Updated comments to the STR macro.
 *
 *   Revised : 1997-08-03
 *   Change  : Changed names of everything with event to group.
 *
 *   Revised : 1997-11-03
 *   Change  : Clarified the comment regarding the STR macro.
 *
 *   Revised : 1998-09-26
 *   Change  : Minor changes due to new Interface Trace functionality.
 *
 *   Revised : 1998-10-01
 *   Change  : Merging the new Interface Trace functionality.
 *
 *   Revised : 2009-09-30 xcssamo
 *   Change  : WRNae57637 - Added SET_STR_FLAG in all trace  macros
 *             to set the 24-bit in admFlags which means that STR
 *             macro is used inside these trace macros.
 *
 *   Revised : 2009-12-18 xcssamo 
 *   Change  : WRNae70553 - Removed the usage of SET_STR_FLAG to avoid
 *   warnings. (Moved revision history for WRNae57637 up). 
 *
 *   Revised : 2010-06-15 xcssamo 
 *   Change  : UABtr80492 - Added new trace macro TRACE_UTS to provide option
 *             to the user to input its own timestamp, process and file info
 *             when generating a trace log entry.
 *
 *   Revised : 2010-12-07 xnarang
 *   Change  : HM65206 - Removed verification of trace group enabled check for 
 *             TRACE_UTS macro. This change enables TRACE_UTS macro to generate 
 *             specific group trace even the trace group gets disabled for that
 *             process.
 *
 *   Revised : 2010-09-23 Anette Schött
 *   Change  : Updated according to template and added macros for Linux.
 *
 *   Revised : 2013-09-22 Satish Muchala
 *   Change  : Implementation for Selective Trace function feature.
 *  
 *   Revised : 2014-01-20 M. Winberg
 *   Change  : Moved extern declaration of procInfo to cello_te_handlers.h
 *             in Linux part.
 *
 *   Revised : 2014-01-23 Naresh Angala
 *   Change  : WCDMA00017891 : CR to improve the robustness of STR
 *             macro so that compilation errors or warnings are seen
 *             when STR is called with wrong number of arguments.
 *
 *   Revised : 2014-02-28 Niranjan Kumar
 *   Change  : TRACE macros contains 'if' condition, this leads to
 *             compilation errors when trace macros are used inside
 *             conditional operator (ex: (var==1)?TRACE1:TRACE2.
 *             Removed the if condition.
 *
 *   Revised : 2014-03-05 Anette Schött
 *   Change  : Correct parameters to lttngTriTrace() for Linux RETURN 
 *             macro.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 * ========================================================================
 */
 
#ifndef CELLO_TE_TRACE_H
#define CELLO_TE_TRACE_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <string.h>
#include <stdio.h>
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


/* >   Macro: DECLARE_SEL_TRACE()
**
** Description:
**   This macro registers user provided selective trace funtion.
**
*/
#define DECLARE_SEL_TRACE(FUNC)  /* Pointer to selective trace function */\
void OMCSF_registerSelTraceFunc(void)\
{\
   selTraceFunc = FUNC; \
}

/**
 * Macro: ENTER()
 * Description:
 *   Used for tracing a call to a function. The message string
 *   is the name of the called function.
 */
#ifndef ENTER
#ifdef TRI_MULTICORE
#define ENTER(MSG)  /* I: Message string */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_ENTER) &&\
               OMCSF_logTrace(OMCSF_GROUP_ENTER, __FILE__, __LINE__, MSG,\
                              ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else
#define ENTER(MSG)  /* I: Message string */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_ENTER) &&\
               OMCSF_logTrace(OMCSF_GROUP_ENTER, __FILE__, __LINE__, MSG,\
                              OMCSF_procInfo_p))
#endif
#endif

/**
 * Macro: RETURN
 * Description:
 *   Used for tracing a return from a function. Semantically a return
 *   is also made from the function.
 */
#ifndef RETURN
#ifdef TRI_MULTICORE
#define RETURN\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_RETURN) &&\
               OMCSF_logTrace(OMCSF_GROUP_RETURN, __FILE__, __LINE__, 0,\
                              ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p));\
        return
#else
#define RETURN\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_RETURN) &&\
               OMCSF_logTrace(OMCSF_GROUP_RETURN, __FILE__, __LINE__, 0,\
                              OMCSF_procInfo_p));\
        return
#endif
#endif


/**
 * Macro: INFO()
 * Description:
 *   Used for tracing informational messages. A call to INFO always
 *   generates a logged message, and thus it should be used with care.
 *
 */ 
#ifdef TRI_MULTICORE
#define INFO(MSG)  /* I: Message string */\
            (void)(GROUP_IS_ENABLED(OMCSF_GROUP_INFO) &&\
                   OMCSF_logTrace(OMCSF_GROUP_INFO, __FILE__, __LINE__, MSG,\
                                  ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else
#define INFO(MSG)  /* I: Message string */\
            (void)(GROUP_IS_ENABLED(OMCSF_GROUP_INFO) &&\
                   OMCSF_logTrace(OMCSF_GROUP_INFO, __FILE__, __LINE__, MSG,\
                                  OMCSF_procInfo_p))
#endif

/**
 * Macro: TRACE()
 * Description:
 *   This is the general trace macro. Used for tracing other kinds of
 *   messages that needs to be switched on or off. The GROUP parameter
 *   indicates which trace group this trace belongs to.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
              MSG)    /* I: Message string                                  */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
               OMCSF_logTrace(OMCSF_GROUP_TRACE ## GROUP,\
                              __FILE__, __LINE__, MSG,\
                              ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else
    #define TRACE(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
                  MSG)    /* I: Message string                                  */\
            (void)(GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
                   OMCSF_logTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                  __FILE__, __LINE__, MSG,\
                                  OMCSF_procInfo_p))
#endif

/**
 * Macro: TRACE_STATE()
 * Description:
 *   Used for tracing state changes in finite state machines. Preferably
 *   the message shall state what the actual state is that has occured.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_STATE(MSG) /* I: Message string */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_STATE_CHANGE) &&\
               OMCSF_logTrace(OMCSF_GROUP_STATE_CHANGE,\
                              __FILE__, __LINE__, MSG,\
                              ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else
#define TRACE_STATE(MSG) /* I: Message string */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_STATE_CHANGE) &&\
               OMCSF_logTrace(OMCSF_GROUP_STATE_CHANGE,\
               __FILE__, __LINE__, MSG,\
               OMCSF_procInfo_p))
#endif

/**
 * Macro: TRACE_BUS_SEND()
 * Description:
 *   Used for tracing sent messages on a bus.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_BUS_SEND(MSG,       /* I: Message string     */\
                       DATA,      /* I: Bus data           */\
                       LEN)       /* I: Length of bus data */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_BUS_SEND) &&\
               OMCSF_logBusTrace(((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                 OMCSF_GROUP_BUS_SEND,\
                                 __FILE__, __LINE__, MSG, DATA, LEN)) 
#else
#define TRACE_BUS_SEND(MSG,       /* I: Message string     */\
                       DATA,      /* I: Bus data           */\
                       LEN)       /* I: Length of bus data */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_BUS_SEND) &&\
               OMCSF_logBusTrace(OMCSF_procInfo_p,\
                                 OMCSF_GROUP_BUS_SEND,\
                                 __FILE__, __LINE__, MSG, DATA, LEN))
#endif

/**
 * Macro: TRACE_BUS_RECEIVE()
 * Description:
 *   Used for tracing received messages on a bus.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_BUS_RECEIVE(MSG,      /* I: Message string     */\
                          DATA,     /* I: Bus data           */\
                          LEN)      /* I: Length of bus data */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_BUS_RECEIVE) &&\
               OMCSF_logBusTrace(((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                 OMCSF_GROUP_BUS_RECEIVE,\
                                 __FILE__, __LINE__, MSG, DATA, LEN))
#else
#define TRACE_BUS_RECEIVE(MSG,      /* I: Message string     */\
                          DATA,     /* I: Bus data           */\
                          LEN)      /* I: Length of bus data */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_BUS_RECEIVE) &&\
               OMCSF_logBusTrace(OMCSF_procInfo_p,\
                                 OMCSF_GROUP_BUS_RECEIVE,\
                                 __FILE__, __LINE__, MSG, DATA, LEN))
#endif

/**
 * Macro: TRACE_SEND_SIG()
 * Description:
 *   Used for tracing of sent signals.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_SEND_SIG(SIG,    /* I: Sent signal pointer      */\
                       RECPID, /* I: Receiver pid             */\
                       MSG)    /* I: Message string           */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_SEND_SIG) &&\
               OMCSF_sendLogTrace(__FILE__, __LINE__, *((U32*)SIG),\
                                  (U32)RECPID, MSG,\
                                  ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p)) 
#else
#define TRACE_SEND_SIG(SIG,    /* I: Sent signal pointer      */\
                       RECPID, /* I: Receiver pid             */\
                       MSG)    /* I: Message string           */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_SEND_SIG) &&\
               OMCSF_sendLogTrace(__FILE__, __LINE__, *((U32*)SIG),\
                                  (U32)RECPID, MSG, \
                                  OMCSF_procInfo_p))

#endif

/**
 * Macro: TRACE_REC_SIG()
 * Description:
 *   Used for tracing of received signals.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_REC_SIG(SIG,   /* I: Received signal pointer   */\
                      MSG)   /* I: Message string            */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_REC_SIG) &&\
               OMCSF_recLogTrace(__FILE__, __LINE__,\
                                 SIG, MSG,\
                                 ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else
#define TRACE_REC_SIG(SIG,   /* I: Received signal pointer   */\
                      MSG)   /* I: Message string            */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_REC_SIG) &&\
               OMCSF_recLogTrace(__FILE__, __LINE__,\
                                 SIG, MSG, \
                                 OMCSF_procInfo_p))
#endif

/**
 * Macro: TRACE_PARAM()
 * Description:
 *   Used for tracing of formatted parameters.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_PARAM(MSG)     /* I: Message string    */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_PARAM) &&\
               OMCSF_logTrace(OMCSF_GROUP_PARAM,\
                              __FILE__, __LINE__, MSG,\
                              ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else 
#define TRACE_PARAM(MSG)     /* I: Message string    */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_PARAM) &&\
               OMCSF_logTrace(OMCSF_GROUP_PARAM,\
                             __FILE__, __LINE__, MSG,\
                             OMCSF_procInfo_p))

#endif


/**
 * Macro: TRACE_ERROR()
 * Description:
 *   Used for tracing errors. This trace logs in the same trace group
 *   as the ERROR macro, but without the semantics regarding error
 *   handling as the ERROR macro has.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_ERROR(MSG)  /* I: Message string */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_ERROR) &&\
               OMCSF_logTrace(OMCSF_GROUP_ERROR, __FILE__, __LINE__, MSG,\
                              ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else
#define TRACE_ERROR(MSG)  /* I: Message string */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_ERROR) &&\
               OMCSF_logTrace(OMCSF_GROUP_ERROR, __FILE__, __LINE__, MSG,\
                              OMCSF_procInfo_p))
#endif

/**
 * Macro: TRACE_UTS()
 * Description:
 *   Used for tracing any trace group, but it gives an option
 *   to theuser to input its own timestamp ( seconds and microsecs),
 *   process name, file and line information.
 *
 */
#ifndef TRACE_UTS
#ifdef TRI_MULTICORE
#define TRACE_UTS(GROUP,\
                  TSEC, \
                  TUSEC, \
                  FILE,\
                  LINE,\
                  PROCNAME, \
                  MSG,\
                  BINDATA, \
                  DATALENGTH) \
        (void)(OMCSF_logUserTrace(GROUP,TSEC,TUSEC,FILE,LINE, \
                                  PROCNAME, MSG,BINDATA,DATALENGTH,\
                                  ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))
#else
#define TRACE_UTS(GROUP,\
                  TSEC, \
                  TUSEC, \
                  FILE,\
                  LINE,\
                  PROCNAME, \
                  MSG,\
                  BINDATA, \
                  DATALENGTH) \
        (void)(OMCSF_logUserTrace(GROUP,TSEC,TUSEC,FILE,LINE, \
                                  PROCNAME, MSG,BINDATA,DATALENGTH, \
                                  OMCSF_procInfo_p))

#endif
#endif

/**
 * Macro: TRACE_SEL()
 * Description:
 *   Used for selective tracing. Traces are logged only when selective trace
 *   function (if any registered) returns True.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_SEL(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
                  DATA,   /* I: Data pointer                                    */\
                  MSG)    /* I: Message string                                  */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
               selTraceFunc(DATA) &&\
               OMCSF_logSelectiveTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                       __FILE__, __LINE__, MSG,\
                                       ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p))

#else
#define TRACE_SEL(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
                  DATA,   /* I: Data pointer                                    */\
                  MSG)    /* I: Message string                                  */\
        (void)(GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
               selTraceFunc(DATA) &&\
               OMCSF_logSelectiveTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                       __FILE__, __LINE__, MSG,\
                                       OMCSF_procInfo_p))
#endif

/**
 * Macro: STR()
 * Description:
 *   STR(char *format, ...)
 *
 *   The format string has the same syntax as in the sprintf function,
 *   There are however some limitations in the maximum length of the
 *   formatted string as well as in the conversion specifications. If
 *   any of these limitations are too limited then other means of
 *   formatting a string can and should be used. In all normal cases
 *   these limitation should however be of no concern.
 *
 *   A conversion specification must have the following syntax:
 *
 *      %[<width>][.<precision>]<conversion character>
 *
 *      ([] specifies an optional item)
 *
 *   The conversion character must be one of:
 *
 *      d   Signed decimal
 *      u   Unsigned decimal
 *      x   Hexadecimal (uses the characters 'abcdef')
 *      X   Hexadecimal (uses the characters 'ABCDEF')
 *      f   Float (Limited formatting capability. Use with care.)
 *      s   String
 *      c   Character
 *      %   The '%' character itself
 *
 * Example:
 *   STR("Decimal value = %d", value)
 *   STR("Hex value 1 = %x, Hex value 2 = %X", hex1, hex2)
 *   STR("The string = %s", str)
 */ 
#ifdef TRI_STR_CHECK
#define STR(FORMAT, ...) (OMCSF_formatStr(FORMAT, __VA_ARGS__))
#else
#define STR (OMCSF_formatStr)
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

#include "tri_proxy.h"    /* Declaration of TRI proxy functions   */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/**
 * Macro: ENTER()
 * Description:
 *   Used for tracing a call to a function. The message string
 *   is the name of the called function.
 */
#ifndef ENTER
#define ENTER(MSG)  /* I: Message string */                         \
        (void)(GROUP_IS_ENABLED(GROUP_ENTER) &&                     \
               lttngTriTrace(GROUP_ENTER, __SHORT_FILE__, __LINE__, \
                             MSG, procInfo));
#endif


/**
 * Macro: RETURN
 * Description:
 *   Used for tracing a return from a function. Semantically a return
 *   is also made from the function.
 */
#ifndef RETURN
#define RETURN                                                      \
        (void)(GROUP_IS_ENABLED(GROUP_RETURN) &&                    \
               lttngTriTrace(GROUP_RETURN, __SHORT_FILE__,          \
                             __LINE__, 0, procInfo));            \
        return
#endif


/**
 * Macro: INFO()
 * Description:
 *   Used for tracing informational messages. A call to INFO always
 *   generates a logged message, and thus it should be used with care.
 *
 */ 
#define INFO(MSG)  /* I: Message string */\
           (void)(GROUP_IS_ENABLED(GROUP_INFO) &&                   \
                  lttngTriTrace(GROUP_INFO, __SHORT_FILE__,         \
                                 __LINE__, MSG, procInfo))



/**
 * Macro: TRACE()
 * Description:
 *   This is the general trace macro. Used for tracing other kinds of
 *   messages that needs to be switched on or off. The GROUP parameter
 *   indicates which trace group this trace belongs to.
 *
 */
#define TRACE(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
              MSG)    /* I: Message string                                  */\
        (void)(GROUP_IS_ENABLED(GROUP_TRACE ## GROUP) &&                      \
               lttngTriTrace(GROUP_TRACE ## GROUP, __SHORT_FILE__,            \
                             __LINE__, MSG, procInfo))


/**
 * Macro: TRACE_STATE()
 * Description:
 *   Used for tracing state changes in finite state machines. Preferably
 *   the message shall state what the actual state is that has occured.
 *
 */ 
#define TRACE_STATE(MSG) /* I: Message string */                    \
        (void)(GROUP_IS_ENABLED(GROUP_STATE_CHANGE) &&              \
               lttngTriTrace(GROUP_STATE_CHANGE, __SHORT_FILE__,    \
                             __LINE__, MSG, procInfo))


/**
 * Macro: TRACE_BUS_SEND()
 * Description:
 *   Used for tracing sent messages on a bus.
 *
 */
#define TRACE_BUS_SEND(MSG,        /* I: Message string     */         \
                       DATA,        /* I: Bus data           */         \
                       LEN)        /* I: Length of bus data */         \
        (void)(GROUP_IS_ENABLED(GROUP_BUS_SEND)  &&                 \
               lttngTriBusTrace(procInfo, GROUP_BUS_SEND,           \
                                __SHORT_FILE__, __LINE__,           \
                                 MSG, DATA, LEN))

/**
 * Macro: TRACE_BUS_RECEIVE()
 * Description:
 *   Used for tracing received messages on a bus.
 *
 */ 
#define TRACE_BUS_RECEIVE(MSG,  /* I: Message string     */         \
                          DATA,        /* I: Bus data           */         \
                          LEN)        /* I: Length of bus data */         \
        (void)(GROUP_IS_ENABLED(GROUP_BUS_RECEIVE) &&               \
               lttngTriBusTrace(procInfo, GROUP_BUS_RECEIVE,        \
                                __SHORT_FILE__, __LINE__,           \
                                MSG, DATA, LEN))

/**
 * Macro: TRACE_SEND_SIG()
 * Description:
 *   Used for tracing of sent signals.
 *
 */ 
#define TRACE_SEND_SIG(SIG,    /* I: Sent signal pointer*/          \
                         RECPID, /* I: Receiver pid       */          \
                       MSG)    /* I: Message string         */          \
        (void)(GROUP_IS_ENABLED(GROUP_SEND_SIG) &&                  \
               lttngTriSendSigTrace(__SHORT_FILE__, __LINE__,       \
                                    *((uint32_t*)SIG),              \
                                    (uint32_t)RECPID, MSG, procInfo))

/**
 * Macro: TRACE_REC_SIG()
 * Description:
 *   Used for tracing of received signals.
 *
 */ 
#define TRACE_REC_SIG(SIG,   /* I: Received signal pointer */       \
                      MSG)   /* I: Message string            */       \
        (void)(GROUP_IS_ENABLED(GROUP_REC_SIG) &&                   \
               lttngTriRecSigTrace(__SHORT_FILE__, __LINE__,        \
                                   (uint32_t*)SIG, MSG, procInfo))

/**
 * Macro: TRACE_PARAM()
 * Description:
 *   Used for tracing of formatted parameters.
 *
 */ 
#define TRACE_PARAM(MSG)     /* I: Message string */                \
        (void)(GROUP_IS_ENABLED(GROUP_PARAM) &&                     \
               lttngTriTrace(GROUP_PARAM, __SHORT_FILE__,           \
                             __LINE__, MSG, procInfo))

/**
 * Macro: TRACE_ERROR()
 * Description:
 *   Used for tracing errors. This trace logs in the same trace group
 *   as the ERROR macro, but without the semantics regarding error
 *   handling as the ERROR macro has.
 *
 */
#define TRACE_ERROR(MSG)  /* I: Message string */                   \
        (void)(GROUP_IS_ENABLED(GROUP_ERROR) &&                     \
               lttngTriTrace(GROUP_ERROR, __SHORT_FILE__,           \
                             __LINE__, MSG, procInfo))

/**
 * Macro: TRACE_UTS()
 * Description:
 *   Used for tracing any trace group, but it gives an option
 *   to theuser to input its own timestamp ( seconds and microsecs),
 *   process name, file and line information.
 *
 */
#ifndef TRACE_UTS
#define TRACE_UTS(GROUP,                                            \
                  TSEC,                                             \
                  TUSEC,                                            \
                  FILE,                                             \
                  LINE,                                             \
                  PROCNAME,                                         \
                  MSG,                                              \
                  BINDATA,                                          \
                  DATALENGTH)                                       \
        (void)(lttngTriUserTrace(GROUP, TSEC, TUSEC, FILE, LINE,    \
                                 PROCNAME, MSG, BINDATA,            \
                                 DATALENGTH, procInfo))

#endif

/**
 * Macro: STR()
 * Description:
 *   STR(char *format, ...)
 *
 *   The format string has the same syntax as in the sprintf function,
 *   There are however some limitations in the maximum length of the
 *   formatted string as well as in the conversion specifications. If
 *   any of these limitations are too limited then other means of
 *   formatting a string can and should be used. In all normal cases
 *   these limitation should however be of no concern.
 *
 *   A conversion specification must have the following syntax:
 *
 *      %[<width>][.<precision>]<conversion character>
 *
 *      ([] specifies an optional item)
 *
 *   The conversion character must be one of:
 *
 *      d   Signed decimal
 *      u   Unsigned decimal
 *      x   Hexadecimal (uses the characters 'abcdef')
 *      X   Hexadecimal (uses the characters 'ABCDEF')
 *      f   Float (Limited formatting capability. Use with care.)
 *      s   String
 *      c   Character
 *      %   The '%' character itself
 *
 * Example:
 *   STR("Decimal value = %d", value)
 *   STR("Hex value 1 = %x, Hex value 2 = %X", hex1, hex2)
 *   STR("The string = %s", str)
 */
#ifdef TRI_STR_CHECK
#define STR(FORMAT, ...) (formatStr(FORMAT, __VA_ARGS__))
#else
#define STR (formatStr)
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
#endif    /* OSE implementation */
/* ===================================================================== */

#ifdef __cplusplus
}
#endif

#endif /* CELLO_TE_TRACE_H */

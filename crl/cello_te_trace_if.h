/**
 *   The contents of this file declares the macros for trace handling
 *   in interfaces, i.e. traces not managed on process basis but on
 *   interface basis. This allows traces to be generated from an interface
 *   disregarding which process executes the interface functions.
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
 *   Revised : 1998-10-09
 *   Change  : Corrected the check for TRACE_IF_BUS_RECEIVE. The check shall be
 *             made for the interface trace mask, not the process.
 *
 *   Revised : 2009-09-30 xcssamo 
 *   Change  : WRNae57637 - Added SET_STR_FLAG in all trace  macros
 *             to set the 24-bit in admFlags which means that STR
 *             macro is used inside these trace macros.
 *
 *   Revised: 2009-12-25 Niranjan (XCSNIRD)
 *   Change : Trace Overload Protection CR implementation (WRNae15217)
 *
 *   Revised: 2010-06-15 xcssamo
 *   Change : UABtr80492 : Added new trace macro TRACE_UTS to provide
 *            the user an option to input its own timestamp, process
 *            and file/line information when writing a trace entry.
 *
 *   Revised: 2010-12-07 xnarang
 *   Change : HM65206 - Removed verification of trace group enabled check for 
 *            TRACE_UTS macro. This change enables TRACE_UTS macro to generate 
 *            specific group trace even the trace group gets disabled for that
 *            process.
 *
 *   Revised : 2013-03-23 Anette Schött
 *   Change  : Updated according to template and added macros for Linux.
 * 
 *   Revised : 2013-09-22 Satish Muchala
 *   Change  : Implementation for Selective Trace function feature.
 *
 *   Revised : 2014-02-27 Anette Schött
 *   Change  : Add deRegisterIfObj as a public function.
 *
 *   Revised : 2014-02-28 Niranjan Kumar
 *   Change  : TRACE macros contains 'if' condition, this leads to
 *             compilation errors when trace macros are used inside
 *             conditional operator (ex: (var==1)?TRACE1:TRACE2.
 *             Hence removed the if condition.
 *
 *   Revised : 2014-06-10 Anette Schött
 *   Change  : Move deRegisterIfObj to te_handlers.h.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 *
 *   Revised : 2015-10-28 Anette Schött
 *   Change  : Updated with low level TRI additions.
 * ========================================================================
 */

#ifndef CELLO_TE_TRACE_IF_H
#define CELLO_TE_TRACE_IF_H

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

/** Macro: DECLARE_INTERFACE()
 *
 * Description:
 *   This macro declares an interface that should have the possibility
 *   to enable and disable its own traces.
 *
 */
#define DECLARE_INTERFACE(IFNAME)  /* I: The name of the interface */\
static struct OMCSF_procInfoS *OMCSF_interfaceInfo_p = &OMCSF_defaultProcInfo;\
void OMCSF_registerInterface_ ## IFNAME(void)\
{\
   OMCSF_registerInterface(#IFNAME, &OMCSF_interfaceInfo_p);\
}

/** Macro: TRI_IF_INFO_PTR
 *
 * Description:
 *   Pointer to the trace item (interface) information structure.
 *
 */
#define TRI_IF_INFO_PTR OMCSF_interfaceInfo_p

/* >   Macro: DECLARE_SEL_TRACE_IF()
**
** Description:
**   This macro registers user provided selective trace funtion.
**
*/
#define DECLARE_SEL_TRACE_IF(FUNC)  /* Pointer to selective trace function */\
void OMCSF_registerSelTraceIfFunc(void)\
{\
   selTraceIfFunc = FUNC; \
}

/** Macro: ENTER()
 *
 * Description:
 *   Used for tracing a call to a function. The message string
 *   is the name of the called function. This macro overrides the standard
 *   ENTER macro to make it possible to enable it both via the process
 *   and via the interface.
 */
#ifdef ENTER
#undef ENTER
#endif

#ifdef TRI_MULTICORE
#define ENTER(MSG)  /* I: Message string */\
       (void)((GROUP_IS_ENABLED(OMCSF_GROUP_ENTER) ||\
               IF_GROUP_IS_ENABLED(OMCSF_GROUP_ENTER)) &&\
              OMCSF_logObjTrace(OMCSF_GROUP_ENTER, __FILE__, __LINE__, MSG,\
                                ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                OMCSF_interfaceInfo_p))
#else
#define ENTER(MSG)  /* I: Message string */\
        (void)((GROUP_IS_ENABLED(OMCSF_GROUP_ENTER) ||\
                IF_GROUP_IS_ENABLED(OMCSF_GROUP_ENTER)) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_ENTER, __FILE__, __LINE__, MSG,\
                                 OMCSF_procInfo_p,\
                                 OMCSF_interfaceInfo_p))
#endif

/** Macro: TRACE_UTS()
 *
 * Description:
 *   Used for tracing any trace group with interfaces , but it gives an option
 *   to theuser to input its own timestamp ( seconds and microsecs),
 *   process name, file and line information.
 *
 */
#ifdef TRACE_UTS
#undef TRACE_UTS
#endif

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
        (void)(OMCSF_logObjUserTrace(GROUP,TSEC,TUSEC,FILE,LINE, \
                                     PROCNAME, MSG,BINDATA,DATALENGTH, \
                                     ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p, \
                                     OMCSF_interfaceInfo_p))
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
        (void)(OMCSF_logObjUserTrace(GROUP,TSEC,TUSEC,FILE,LINE, \
                                     PROCNAME, MSG,BINDATA,DATALENGTH, \
                                     OMCSF_procInfo_p,\
                                     OMCSF_interfaceInfo_p))

#endif


/** Macro: RETURN
 *
 * Description:
 *   Used for tracing a return from a function. Semantically a return
 *   is also made from the function. This macro overrides the standard
 *   RETURN macro to make it possible to enable it both via the process
 *   and via the interface.
 *   
 */
#ifdef RETURN
#undef RETURN
#endif

#ifdef TRI_MULTICORE
#define RETURN\
        (void)((GROUP_IS_ENABLED(OMCSF_GROUP_RETURN) ||\
                IF_GROUP_IS_ENABLED(OMCSF_GROUP_RETURN)) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_RETURN, __FILE__, __LINE__, 0,\
                                 ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                 OMCSF_interfaceInfo_p)); \
        return
#else
#define RETURN\
        (void)((GROUP_IS_ENABLED(OMCSF_GROUP_RETURN) ||\
                IF_GROUP_IS_ENABLED(OMCSF_GROUP_RETURN)) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_RETURN, __FILE__, __LINE__, 0,\
                                 OMCSF_procInfo_p,\
                                 OMCSF_interfaceInfo_p)); \
        return
#endif


/** Macro: TRACE_IF()
 *
 * Description:
 *   This is the general trace macro. Used for tracing other kinds of
 *   messages that needs to be switched on or off. The GROUP parameter
 *   indicates which trace group this trace belongs to.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_IF(GROUP, /* I: Which group (1 - 9) this trace belongs to */\
                 MSG)   /* I: Message string                            */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                 __FILE__, __LINE__, MSG,\
                                 ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                 OMCSF_interfaceInfo_p ))
#else
#define TRACE_IF(GROUP, /* I: Which group (1 - 9) this trace belongs to */\
                 MSG)   /* I: Message string                            */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                 __FILE__, __LINE__, MSG, \
                                 OMCSF_procInfo_p,\
                                 OMCSF_interfaceInfo_p ))
#endif

/** Macro: TRACE_IF_STATE()
 *
 * Description:
 *   Used for tracing state changes in finite state machines. Preferably
 *   the message shall state what the actual state is that has occured.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_IF_STATE(MSG) /* I: Message string */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_STATE_CHANGE) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_STATE_CHANGE,\
                                 __FILE__, __LINE__, MSG,\
                                 ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                 OMCSF_interfaceInfo_p))

#else
#define TRACE_IF_STATE(MSG) /* I: Message string */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_STATE_CHANGE) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_STATE_CHANGE,\
                                 __FILE__, __LINE__, MSG, \
                                 OMCSF_procInfo_p,\
                                 OMCSF_interfaceInfo_p))
#endif

/** Macro: TRACE_IF_BUS_SEND()
 *
 * Description:
 *   Used for tracing sent messages on a bus.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_IF_BUS_SEND(MSG,  /* I: Message string     */\
                          DATA, /* I: Bus data           */\
                          LEN)  /* I: Length of bus data */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_BUS_SEND) &&\
               OMCSF_logObjBusTrace(((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                    OMCSF_interfaceInfo_p,\
                                    OMCSF_GROUP_BUS_SEND,\
                                    __FILE__, __LINE__, MSG, DATA, LEN ))
#else
#define TRACE_IF_BUS_SEND(MSG,  /* I: Message string     */\
                          DATA, /* I: Bus data           */\
                          LEN)  /* I: Length of bus data */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_BUS_SEND) &&\
               OMCSF_logObjBusTrace(OMCSF_procInfo_p,\
                                    OMCSF_interfaceInfo_p,\
                                    OMCSF_GROUP_BUS_SEND,\
                                    __FILE__, __LINE__, MSG, DATA, LEN ))
#endif

/** Macro: TRACE_IF_BUS_RECEIVE()
 *
 * Description:
 *   Used for tracing received messages on a bus.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_IF_BUS_RECEIVE(MSG,   /* I: Message string     */\
                             DATA,  /* I: Bus data           */\
                             LEN)   /* I: Length of bus data */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_BUS_RECEIVE) &&\
               OMCSF_logObjBusTrace(((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                    OMCSF_interfaceInfo_p,\
                                    OMCSF_GROUP_BUS_RECEIVE,\
                                    __FILE__, __LINE__, MSG, DATA, LEN))
#else
#define TRACE_IF_BUS_RECEIVE(MSG,   /* I: Message string     */\
                             DATA,  /* I: Bus data           */\
                             LEN)   /* I: Length of bus data */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_BUS_RECEIVE) &&\
               OMCSF_logObjBusTrace(OMCSF_procInfo_p,\
                                    OMCSF_interfaceInfo_p,\
                                    OMCSF_GROUP_BUS_RECEIVE,\
                                    __FILE__, __LINE__, MSG, DATA, LEN))
#endif 

/** Macro: TRACE_IF_SEND_SIG()
 *
 * Description:
 *   Used for tracing of sent signals.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_IF_SEND_SIG(SIG,    /* I: Sent signal pointer      */\
                          RECPID, /* I: Receiver pid             */\
                          MSG)    /* I: Message string           */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_SEND_SIG) &&\
               OMCSF_sendObjLogTrace(__FILE__, __LINE__, *((U32*)SIG),\
                                     (U32)RECPID, MSG,\
                                     ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                     OMCSF_interfaceInfo_p))
#else 
#define TRACE_IF_SEND_SIG(SIG,    /* I: Sent signal pointer      */\
                          RECPID, /* I: Receiver pid             */\
                          MSG)    /* I: Message string           */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_SEND_SIG) &&\
               OMCSF_sendObjLogTrace(__FILE__, __LINE__, *((U32*)SIG),\
                                     (U32)RECPID, MSG, \
                                     OMCSF_procInfo_p,\
                                     OMCSF_interfaceInfo_p))
#endif

/** Macro: TRACE_IF_REC_SIG()
 *
 * Description:
 *   Used for tracing of received signals.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_IF_REC_SIG(SIG,   /* I: Received signal pointer   */\
                         MSG)   /* I: Message string            */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_REC_SIG) &&\
               OMCSF_recObjLogTrace(__FILE__, __LINE__,\
                                    SIG, MSG,\
                                    ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                    OMCSF_interfaceInfo_p))
#else
#define TRACE_IF_REC_SIG(SIG,   /* I: Received signal pointer   */\
                         MSG)   /* I: Message string            */\
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_REC_SIG) &&\
               OMCSF_recObjLogTrace(__FILE__, __LINE__,\
                                    SIG, MSG, \
                                    OMCSF_procInfo_p,\
                                    OMCSF_interfaceInfo_p))
#endif

/**
 * Macro: TRACE_IF_SEL()
 * Description:
 *   Used for selective tracing. Traces are logged only when selective trace
 *   function (if any registered) returns True.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_IF_SEL(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
                     DATA,   /* I: Data Pointer                                    */\
                     MSG)    /* I: Message string                                  */ \
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
               selTraceIfFunc(DATA) &&\
               OMCSF_logObjSelectiveTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                          __FILE__, __LINE__, MSG,\
                                          ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                          OMCSF_interfaceInfo_p))
#else
#define TRACE_IF_SEL(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
                     DATA,   /* I: Data Pointer                                    */\
                     MSG)    /* I: Message string                                  */ \
        (void)(IF_GROUP_IS_ENABLED(OMCSF_GROUP_TRACE ## GROUP) &&\
               selTraceIfFunc(DATA) &&\
               OMCSF_logObjSelectiveTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                          __FILE__, __LINE__, MSG,\
                                          OMCSF_procInfo_p, \
                                          OMCSF_interfaceInfo_p))

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

#include <stdint.h>
#include "tri_proxy.h"    /* Declaration of TRI proxy functions   */

extern __thread struct ItemInfo *procInfo;

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/** Macro: DECLARE_INTERFACE()
 *
 * Description:
 *   This macro declares an interface that should have the possibility
 *   to enable and disable its own traces.
 *
 */
#define DECLARE_INTERFACE(IFNAME)  /* I: The name of the interface */ \
static struct ItemInfo *interfaceInfo = &defaultItemInfo;             \
void OMCSF_registerInterface_ ## IFNAME(void)                         \
{                                                                     \
  registerIfObj(#IFNAME, &interfaceInfo);                             \
}

/** Macro: TRI_IF_INFO_PTR
 *
 * Description:
 * Pointer to the trace item (interface) information structure.
 */
#define TRI_IF_INFO_PTR interfaceInfo

/** Macro: ENTER()
 *
 * Description:
 *   Used for tracing a call to a function. The message string
 *   is the name of the called function. This macro overrides the standard
 *   ENTER macro to make it possible to enable it both via the process
 *   and via the interface.
 */
#ifdef ENTER
#undef ENTER
#endif
#define ENTER(MSG)  /* I: Message string */                           \
        (void)((GROUP_IS_ENABLED(GROUP_ENTER) ||                      \
                IF_GROUP_IS_ENABLED(GROUP_ENTER)) &&                  \
               lttngTriIfObjTrace(GROUP_ENTER, __SHORT_FILE__,        \
                                  __LINE__, MSG, procInfo,            \
                                  interfaceInfo))

/** Macro: TRACE_UTS()
 *
 * Description:
 *   Used for tracing any trace group with interfaces , but it gives an option
 *   to theuser to input its own timestamp ( seconds and microsecs),
 *   process name, file and line information.
 *
 */

#ifdef TRACE_UTS
#undef TRACE_UTS
#endif

#define TRACE_UTS(GROUP,                                              \
                  TSEC,                                               \
                  TUSEC,                                              \
                  FILE,                                               \
                  LINE,                                               \
                  PROCNAME,                                           \
                  MSG,                                                \
                  BINDATA,                                            \
                  DATALENGTH)                                         \
        (void)(lttngTriIfObjUserTrace(GROUP, TSEC, TUSEC, FILE, LINE, \
                                PROCNAME, MSG, BINDATA, DATALENGTH,   \
                                procInfo, interfaceInfo))


/** Macro: RETURN
 *
 * Description:
 *   Used for tracing a return from a function. Semantically a return
 *   is also made from the function. This macro overrides the standard
 *   RETURN macro to make it possible to enable it both via the process
 *   and via the interface.
 *   
 */
#ifdef RETURN
#undef RETURN
#endif
#define RETURN                                                        \
        (void)((GROUP_IS_ENABLED(GROUP_RETURN) ||                     \
                IF_GROUP_IS_ENABLED(GROUP_RETURN)) &&                 \
               lttngTriIfObjTrace(GROUP_RETURN, __SHORT_FILE__,       \
                                  __LINE__, 0, procInfo,              \
                                  interfaceInfo));                    \
        return

/** Macro: TRACE_IF()
 *
 * Description:
 *   This is the general trace macro. Used for tracing other kinds of
 *   messages that needs to be switched on or off. The GROUP parameter
 *   indicates which trace group this trace belongs to.
 *
 */
#define TRACE_IF(GROUP, /* I: Which group (1 - 9) this trace belongs to */  \
		 MSG)   /* I: Message string                            */  \
        (void)(IF_GROUP_IS_ENABLED(GROUP_TRACE ## GROUP) &&                 \
	       lttngTriIfObjTrace(GROUP_TRACE ## GROUP,                     \
                                  __SHORT_FILE__, __LINE__,                 \
                                 MSG, procInfo, interfaceInfo))

/** Macro: TRACE_IF_STATE()
 *
 * Description:
 *   Used for tracing state changes in finite state machines. Preferably
 *   the message shall state what the actual state is that has occured.
 *
 */ 
#define TRACE_IF_STATE(MSG) /* I: Message string */                   \
        (void)(IF_GROUP_IS_ENABLED(GROUP_STATE_CHANGE) &&             \
	       lttngTriIfObjTrace(GROUP_STATE_CHANGE,                 \
                                  __SHORT_FILE__, __LINE__,           \
                                  MSG, procInfo, interfaceInfo))

/** Macro: TRACE_IF_BUS_SEND()
 *
 * Description:
 *   Used for tracing sent messages on a bus.
 *
 */
#define TRACE_IF_BUS_SEND(MSG,  /* I: Message string     */           \
			  DATA, /* I: Bus data           */           \
			  LEN)  /* I: Length of bus data */           \
        (void)(IF_GROUP_IS_ENABLED(GROUP_BUS_SEND) &&                 \
	       lttngTriIfObjBusTrace(procInfo, interfaceInfo,         \
                                     GROUP_BUS_SEND,                  \
                                     __SHORT_FILE__, __LINE__,        \
                                     MSG, DATA, LEN))

/** Macro: TRACE_IF_BUS_RECEIVE()
 *
 * Description:
 *   Used for tracing received messages on a bus.
 *
 */ 
#define TRACE_IF_BUS_RECEIVE(MSG,   /* I: Message string     */       \
			     DATA,  /* I: Bus data           */       \
			     LEN)   /* I: Length of bus data */       \
        (void)(IF_GROUP_IS_ENABLED(GROUP_BUS_RECEIVE) &&              \
	       lttngTriIfObjBusTrace(procInfo, interfaceInfo,         \
                                     GROUP_BUS_RECEIVE,               \
                                     __SHORT_FILE__, __LINE__,        \
                                     MSG, DATA, LEN))

/** Macro: TRACE_IF_SEND_SIG()
 *
 * Description:
 *   Used for tracing of sent signals.
 *
 */ 
#define TRACE_IF_SEND_SIG(SIG,    /* I: Sent signal pointer  */       \
  		       RECPID, /* I: Receiver pid            */       \
		       MSG)    /* I: Message string 	     */       \
        (void)(IF_GROUP_IS_ENABLED(GROUP_SEND_SIG) &&                 \
	       lttngTriIfObjSendSigTrace(__SHORT_FILE__, __LINE__,    \
                                         *((uint32_t*)SIG),           \
                                         (uint32_t)RECPID, MSG,       \
                                         procInfo, interfaceInfo))

/** Macro: TRACE_IF_REC_SIG()
 *
 * Description:
 *   Used for tracing of received signals.
 *
 */ 
#define TRACE_IF_REC_SIG(SIG,   /* I: Received signal pointer */      \
		      MSG)   /* I: Message string 	      */      \
        (void)(IF_GROUP_IS_ENABLED(GROUP_REC_SIG) &&                  \
	       lttngTriIfObjRecSigTrace(__SHORT_FILE__, __LINE__,     \
                                        (uint32_t*)SIG, MSG,          \
                                        procInfo, interfaceInfo))
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

#endif /* CELLO_TE_TRACE_IF_H */

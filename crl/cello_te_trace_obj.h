/**
 *   The contents of this file declares the macros for trace handling
 *   of generic trace objects, i.e. traces not managed on process basis but
 *   on a so called trace object basis. This allows traces to be generated
 *   from a so called trace object disregarding which process executes the
 *   traces for this trace object.
 *
 *   Important: Since some of the macros consists of multiple statements
 *   and/or if statements without else clause, it is important that a
 *   macro only is used in compound statements, i.e. inside braces.
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
 *   Revised : 2009-12-18 xcssamo 
 *   Change  : WRNae70553 - Removed the usage of SET_STR_FLAG to avoid 
 *             warnings.
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
 *   Revised : 2013-09-17 Satish Muchhala
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
 *   Change  : Move deRegisterIfObj to te_handlers.h. Added missing
 *             extern "C". 
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 *
 *   Revised : 2015.01-07 Anette Schött
 *   Change  : Addd support for OMCSF_traceObjInfo ## OBJNAME ## _p in Linux.
 *
 *   Revised : 2015-01-26 Anette Schött
 *   Change  : Add support for both traceObjInfo ## OBJNAME and
 *             OMCSF_traceObjInfo ## OBJNAME ## _p reference name in
 *             DECLARE_TRACE_OBJ.
 *
 *   Revised : 2015-02-13 Anette Schött
 *   Change  : Add lint actions to avoid 752 warning for IMPORT_TRACE_OBJ
 *             macro.
 *
 *   Revised : 2015-10-28 Anette Schött
 *   Change  : Updated with low level TRI additions.
 *
 *   Revised : 2015-10-06 Anette Schött
 *   Change  : Add DECLARE_TRACE_OBJ_PTR(), tri_register_obj and
 *             tri_deregister_obj.
 * ========================================================================
 */

#ifndef CELLO_TE_TRACE_OBJ_H
#define CELLO_TE_TRACE_OBJ_H

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
 * Macro: DECLARE_TRACE_OBJ()
 * Description:
 *   This macro declares a generic trace object that should have the
 *   possibility to enable and disable its own traces.
 *
 */
#define DECLARE_TRACE_OBJ(OBJNAME)  /* I: Name of trace object to declare */\
struct OMCSF_procInfoS *OMCSF_traceObjInfo ## OBJNAME ## _p = \
  &OMCSF_defaultProcInfo;\
void OMCSF_registerTraceObj_ ## OBJNAME(void)\
{\
   OMCSF_registerInterface(#OBJNAME, &OMCSF_traceObjInfo ## OBJNAME ## _p);\
}

/**
 * Macro: TRI_OBJ_INFO_PTR()
 * Description:
 *   Pointer to the trace item (object) information structure.
 *
 */
#define TRI_OBJ_INFO_PTR(OBJNAME) OMCSF_traceObjInfo ## OBJNAME ## _p

/*
 * Macro: IMPORT_TRACE_OBJ()
 * Description:
 *   This macro imports a generic trace object that have been declared
 *   somewhere else.
 *
 */
#define IMPORT_TRACE_OBJ(OBJNAME)  /* I: Name of the trace object to import */\
extern struct OMCSF_procInfoS *OMCSF_traceObjInfo ## OBJNAME ## _p;

/**
 * Macro: DECLARE_OBJ_SEL_TRACE()
 * Description:
 *   This macro registers an function to be used while generation
 *   selective tracing
 *
 */
#define DECLARE_SEL_TRACE_OBJ(FUNC) /* I: The name of the interface */\
void OMCSF_registerSelTraceObjFunc(void)\
{\
     selTraceObjFunc = FUNC; \
}

/**
 * Macro: IMPORT_SEL_TRACE()
 * Description:
 *   This macro imports an function to be used while generation
 *   selective tracing
 *
 */
#define IMPORT_SEL_TRACE_OBJ() /* I: The name of the Object */ \
extern Boolean (*selTraceObjFunc)(void *);


/*
 * Macro: TRACE_OBJ()
 * Description:
 *   This is the general trace macro. Used for tracing other kinds of
 *   messages that needs to be switched on or off. The GROUP parameter
 *   indicates which trace group this trace belongs to.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                  TRACEOBJ, /* I: The trace object to control this trace    */\
                  MSG)      /* I: Message string                            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_TRACE ## GROUP, __FILE__,\
                                 __LINE__, MSG,\
                                 ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                 OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#else
#define TRACE_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                  TRACEOBJ, /* I: The trace object to control this trace    */\
                  MSG)      /* I: Message string                            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_TRACE ## GROUP, __FILE__,\
                                 __LINE__, MSG, \
                                 OMCSF_procInfo_p,\
                                 OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#endif


/*
 * Macro: TRACE_OBJ_SEND_SIG()
 * Description:
 *   Used for tracing of sent signals.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_OBJ_SEND_SIG(TRACEOBJ, /* I: Trace object control   */\
                           SIG,      /* I: Sent signal pointer    */\
                           RECPID,   /* I: Receiver pid           */\
                           MSG)      /* I: Message string         */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_SEND_SIG) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ)&&\
               OMCSF_sendObjLogTrace(__FILE__, __LINE__,*((U32*)SIG),\
                                     RECPID, MSG,\
                                     ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                     OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#else
#define TRACE_OBJ_SEND_SIG(TRACEOBJ, /* I: Trace object control   */\
                           SIG,    /* I: Sent signal pointer      */\
                           RECPID, /* I: Receiver pid             */\
                           MSG)    /* I: Message string           */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_SEND_SIG) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ)&&\
               OMCSF_sendObjLogTrace(__FILE__, __LINE__,*((U32*)SIG),\
                                     RECPID, MSG, \
                                     OMCSF_procInfo_p,\
                                     OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#endif

/*
 * Macro: TRACE_OBJ_REC_SIG()
 * Description:
 *   Used for tracing of received signals.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_OBJ_REC_SIG(TRACEOBJ, /* I: The trace object control  */\
                          SIG,      /* I: Received signal pointer   */\
                          MSG)      /* I: Message string            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_REC_SIG) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_recObjLogTrace(__FILE__, __LINE__, SIG, MSG,\
                                    ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                    OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#else
#define TRACE_OBJ_REC_SIG(TRACEOBJ, /* I: The trace object control  */\
                          SIG,      /* I: Received signal pointer   */\
                          MSG)      /* I: Message string            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_REC_SIG) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_recObjLogTrace(__FILE__, __LINE__, SIG, MSG,\
                                    OMCSF_procInfo_p,\
                                    OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#endif

/*
 * Macro: TRACE_OBJ_PARAM()
 * Description:
 *   Used for tracing of formatted parameters.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_OBJ_PARAM(TRACEOBJ, /* I: The trace object control    */\
                        MSG)      /* I: Message string              */\
       (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_PARAM) &&\
              GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
              OMCSF_logObjTrace(OMCSF_GROUP_PARAM, __FILE__, __LINE__, MSG,\
                                ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#else
#define TRACE_OBJ_PARAM(TRACEOBJ, /* I: The trace object control    */\
                        MSG)      /* I: Message string              */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_PARAM) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_PARAM, __FILE__, __LINE__, MSG,\
                                 OMCSF_procInfo_p,\
                                 OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#endif

/*
 * Macro: TRACE_OBJ_STATE()
 * Description:
 *   Used for tracing state changes in finite state machines. Preferably
 *   the message shall state what the actual state is that has occured.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_OBJ_STATE(TRACEOBJ, /* I: Trace object to control this trace */\
                        MSG)      /* I: Message string                     */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_STATE_CHANGE) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_STATE_CHANGE, __FILE__,__LINE__,\
                                 MSG,\
                                 ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                 OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#else
#define TRACE_OBJ_STATE(TRACEOBJ, /* I: Trace object to control this trace */\
                        MSG)      /* I: Message string                     */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_STATE_CHANGE) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjTrace(OMCSF_GROUP_STATE_CHANGE, __FILE__,__LINE__,\
                                 MSG, OMCSF_procInfo_p,			\
                                 OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#endif

/*
 * Macro: TRACE_OBJ_BUS_SEND()
 * Description:
 *   Used for tracing sent messages on a bus.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_OBJ_BUS_SEND(TRACEOBJ, /* I: Trace object to control trace */\
                           MSG,      /* I: Message string                */\
                           DATA,     /* I: Bus data                      */\
                           LEN)      /* I: Length of bus data            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_BUS_SEND) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjBusTrace(((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                    OMCSF_traceObjInfo ## TRACEOBJ ## _p,\
                                    OMCSF_GROUP_BUS_SEND,\
                                    __FILE__, __LINE__, MSG, DATA, LEN))
#else
#define TRACE_OBJ_BUS_SEND(TRACEOBJ, /* I: Trace object to control trace */\
                           MSG,      /* I: Message string                */\
                           DATA,     /* I: Bus data                      */\
                           LEN)      /* I: Length of bus data            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_BUS_SEND) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjBusTrace(OMCSF_procInfo_p,\
                                    OMCSF_traceObjInfo ## TRACEOBJ ## _p,\
                                    OMCSF_GROUP_BUS_SEND,\
                                    __FILE__, __LINE__, MSG, DATA, LEN))
#endif

/*
 * Macro: TRACE_OBJ_BUS_RECEIVE()
 * Description:
 *   Used for tracing received messages on a bus.
 *
 */ 
#ifdef TRI_MULTICORE
#define TRACE_OBJ_BUS_RECEIVE(TRACEOBJ, /* I: Trace object to control trace */\
                              MSG,      /* I: Message string                */\
                              DATA,     /* I: Bus data                      */\
                              LEN)      /* I: Length of bus data            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_BUS_RECEIVE) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjBusTrace(((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                    OMCSF_traceObjInfo ## TRACEOBJ ## _p,\
                                    OMCSF_GROUP_BUS_RECEIVE,\
                                    __FILE__, __LINE__, MSG, DATA, LEN))
#else
#define TRACE_OBJ_BUS_RECEIVE(TRACEOBJ, /* I: Trace object to control trace */\
                              MSG,      /* I: Message string                */\
                              DATA,     /* I: Bus data                      */\
                              LEN)      /* I: Length of bus data            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_BUS_RECEIVE) &&\
               GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
               OMCSF_logObjBusTrace(OMCSF_procInfo_p,\
                                    OMCSF_traceObjInfo ## TRACEOBJ ## _p,\
                                    OMCSF_GROUP_BUS_RECEIVE,\
                                    __FILE__, __LINE__, MSG, DATA, LEN))
#endif
/**
 * Macro: TRACE_OBJ_SEL()
 * Description:
 *   This is the general trace macro. Used for selective trace
 *   messages that needs to be switched on or off. The GROUP parameter
 *   indicates which trace group this trace belongs to. selectiveFunction
 *   invokes the application defined function.
 *
 */
#ifdef TRI_MULTICORE
#define TRACE_OBJ_SEL(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
                      TRACEOBJ, /* I: The trace object to control this trace    */\
                      DATA,     /* I: Data Pointer          */\
                      MSG)      /* I: Message string                                */\
       (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
              GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
              selTraceObjFunc(DATA) &&\
              OMCSF_logObjSelectiveTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                         __FILE__, __LINE__, MSG,\
                                         ((struct ProcInfo *)OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p,\
                                         OMCSF_traceObjInfo ## TRACEOBJ ## _p))
#else
#define TRACE_OBJ_SEL(GROUP,  /* I: Which trace group (1 - 9) this trace belongs to */\
                      TRACEOBJ, /* I: The trace object to control this trace    */\
                      DATA,     /* I: Data Pointer          */\
                      MSG)      /* I: Message string                                */\
       (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
              GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) &&\
              selTraceObjFunc(DATA) &&\
              OMCSF_logObjSelectiveTrace(OMCSF_GROUP_TRACE ## GROUP,\
                                         __FILE__, __LINE__, MSG,\
                                         OMCSF_procInfo_p,\
                                         OMCSF_traceObjInfo ## TRACEOBJ ## _p))

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

#include <stddef.h>
#include <stdint.h>
#include "tri_proxy.h"    /* Declaration of TRI proxy functions   */

extern __thread struct ItemInfo *procInfo;

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/*
 * Macro: DECLARE_TRACE_OBJ()
 * Description:
 *   This macro declares a generic trace object that should have the
 *   possibility to enable and disable its own traces. Both the Linux
 *   reference name and the legacy name are defined.
 *
 */
#define DECLARE_TRACE_OBJ(OBJNAME)  /* I: Name of trace object to declare */\
struct ItemInfo *traceObjInfo ## OBJNAME = &defaultItemInfo;\
struct ItemInfo *OMCSF_traceObjInfo ## OBJNAME ## _p = &defaultItemInfo;\
void OMCSF_registerTraceObj_ ## OBJNAME(void)\
{\
        registerIfObj(#OBJNAME, NULL, &traceObjInfo ## OBJNAME,         \
                      &OMCSF_traceObjInfo ## OBJNAME ## _p);\
}

/**
 * Macro: TRI_OBJ_INFO_PTR()
 * Description:
 *   Pointer to the trace item (object) information structure.
 *
 */
#define TRI_OBJ_INFO_PTR(OBJNAME) traceObjInfo ## OBJNAME

/*
 * Macro: IMPORT_TRACE_OBJ()
 * Description:
 *   This macro imports a generic trace object that have been declared
 *   somewhere else. Both the Linux reference name and the legacy name
 *   are exported.
 *
 */
/*lint -esym(752,traceObjInfo*) */
/*lint -esym(752,OMCSF_traceObjInfo*_p) */
/*local declarator 'Symbol' (Location) not referenced */
#define IMPORT_TRACE_OBJ(OBJNAME)\
   extern struct ItemInfo *traceObjInfo ## OBJNAME; \
   extern struct OMCSF_procInfoS *OMCSF_traceObjInfo ## OBJNAME ## _p;


/**
 *   DECLARE_TRACE_OBJ_PTR()
 *
 *   @param   OBJNAME   Name og the trace object
 *
 *
 *   This macro is a is used to declare trace object pointers.
 *   To be used in conjunction with tri_register_obj and/or
 *   tri_deregister_obj.
 */
#define DECLARE_TRACE_OBJ_PTR(OBJNAME) /* I: Name of trace object to declare */\
struct ItemInfo *traceObjInfo ## OBJNAME = &defaultItemInfo;\
struct ItemInfo *OMCSF_traceObjInfo ## OBJNAME ## _p = &defaultItemInfo;


/**
 *   Register a named trace object.
 *   This allocates an "object" to use with trace macros ending in _OBJ.
 *
 *   @param  itemName            Name of the trace item to be registered
 *
 *   @param  ref                 A pointer to where to store the allocated
 *                               "object"
 *
 *   @return TRI_OK              Success
 *           TRI_INVALID_PARAM   obj was null or *obj already points to a
 *                               trace object, or trace_name was NULL or
 *                               trace_name too long
 *           TRI_NOT_INITIALIZED You need to call tri_init before using
 *                               this function
 *           TRI_INTERNAL_ERROR  Failed to locate trace mask handling thread
 *
 */
#define tri_register_obj registerIfObj

extern void registerIfObj(const char *itemName,
                          struct ItemInfo **ref,
                          ...);

/**
 *  Remove registration of a trace object.
 *
 *  @param  ref                  A pointer to where the "object"
 *                               to deallocate is stored.
 *
 *  @return TRI_OK              Success
 *          TRI_INVALID_PARAM    obj was invalid
 *          TRI_NOT_INITIALIZED  You need to call tri_init before
 *                               using this function
 *          TRI_INTERNAL_ERROR   Failed to find the trace mask handling
 *                               thread
 *
 */
#define tri_deregister_obj deRegisterIfObj

extern void deRegisterIfObj(struct ItemInfo **ref);


/*
 * Macro: TRACE_OBJ()
 * Description:
 *   This is the general trace macro. Used for tracing other kinds of
 *   messages that needs to be switched on or off. The GROUP parameter
 *   indicates which trace group this trace belongs to.
 *
 */
#define TRACE_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                  TRACEOBJ, /* I: The trace object to control this trace    */\
                  MSG)      /* I: Message string                            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, GROUP_TRACE ## GROUP) &&        \
               GROUP_IS_ENABLED(GROUP_TRACE_OBJ) &&                           \
               lttngTriIfObjTrace(GROUP_TRACE ## GROUP, __SHORT_FILE__,       \
                                  __LINE__, MSG,                              \
                                  procInfo, OMCSF_traceObjInfo ## TRACEOBJ ## _p))


/*
 * Macro: TRACE_OBJ_SEND_SIG()
 * Description:
 *   Used for tracing of sent signals.
 *
 */ 
#define TRACE_OBJ_SEND_SIG(TRACEOBJ, /* I: Trace object control */          \
                           SIG,    /* I: Sent signal pointer    */          \
                           RECPID, /* I: Receiver pid           */          \
                           MSG)    /* I: Message string         */          \
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, GROUP_SEND_SIG) &&            \
               GROUP_IS_ENABLED(GROUP_TRACE_OBJ) &&                         \
               lttngTriIfObjSendSigTrace(__SHORT_FILE__, __LINE__,          \
                                         *((uint32_t*)SIG),                 \
                                         (uint32_t)RECPID, MSG, procInfo,   \
                                         OMCSF_traceObjInfo ## TRACEOBJ ## _p))


/*
 * Macro: TRACE_OBJ_REC_SIG()
 * Description:
 *   Used for tracing of received signals.
 *
 */ 
#define TRACE_OBJ_REC_SIG(TRACEOBJ, /* I: The trace object control  */     \
                          SIG,      /* I: Received signal pointer   */     \
                          MSG)      /* I: Message string            */     \
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, GROUP_REC_SIG) &&            \
               GROUP_IS_ENABLED(GROUP_TRACE_OBJ) &&                        \
               lttngTriIfObjRecSigTrace(__SHORT_FILE__, __LINE__,          \
                                        (uint32_t*)SIG, MSG, procInfo,     \
                                        OMCSF_traceObjInfo ## TRACEOBJ ## _p))


/*
 * Macro: TRACE_OBJ_PARAM()
 * Description:
 *   Used for tracing of formatted parameters.
 *
 */ 
#define TRACE_OBJ_PARAM(TRACEOBJ, /* I: The trace object control */        \
                        MSG)      /* I: Message string           */        \
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, GROUP_PARAM) &&              \
               GROUP_IS_ENABLED(GROUP_TRACE_OBJ) &&                        \
               lttngTriIfObjTrace(GROUP_PARAM, __SHORT_FILE__,             \
                                  __LINE__, MSG,                           \
                                  procInfo, OMCSF_traceObjInfo ## TRACEOBJ ## _p))


/*
 * Macro: TRACE_OBJ_STATE()
 * Description:
 *   Used for tracing state changes in finite state machines. Preferably
 *   the message shall state what the actual state is that has occured.
 *
 */ 
#define TRACE_OBJ_STATE(TRACEOBJ, /* I: Trace object to control this trace */\
                        MSG)      /* I: Message string                     */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, GROUP_STATE_CHANGE) &&         \
               GROUP_IS_ENABLED(GROUP_TRACE_OBJ) &&                          \
               lttngTriIfObjTrace(GROUP_STATE_CHANGE, __SHORT_FILE__,        \
                                  __LINE__, MSG,                             \
                                  procInfo, OMCSF_traceObjInfo ## TRACEOBJ ## _p))

/*
 * Macro: TRACE_OBJ_BUS_SEND()
 * Description:
 *   Used for tracing sent messages on a bus.
 *
 */
#define TRACE_OBJ_BUS_SEND(TRACEOBJ, /* I: Trace object to control trace */\
                           MSG,      /* I: Message string                */\
                           DATA,     /* I: Bus data                      */\
                           LEN)      /* I: Length of bus data            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, GROUP_BUS_SEND) &&           \
               GROUP_IS_ENABLED(GROUP_TRACE_OBJ) &&                        \
               lttngTriIfObjBusTrace(procInfo, OMCSF_traceObjInfo ## TRACEOBJ ## _p,\
                                     GROUP_BUS_SEND, __SHORT_FILE__,       \
                                     __LINE__, MSG, DATA, LEN))


/*
 * Macro: TRACE_OBJ_BUS_RECEIVE()
 * Description:
 *   Used for tracing received messages on a bus.
 *
 */ 
#define TRACE_OBJ_BUS_RECEIVE(TRACEOBJ, /* I: Trace object to control trace */\
                              MSG,      /* I: Message string                */\
                              DATA,     /* I: Bus data                      */\
                              LEN)      /* I: Length of bus data            */\
        (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, GROUP_BUS_RECEIVE) &&           \
               GROUP_IS_ENABLED(GROUP_TRACE_OBJ) &&                           \
               lttngTriIfObjBusTrace(procInfo, OMCSF_traceObjInfo ## TRACEOBJ ## _p,\
                                     GROUP_BUS_RECEIVE, __SHORT_FILE__,       \
                                     __LINE__, MSG, DATA, LEN))


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

#endif /* CELLO_TE_TRACE_OBJ_H */

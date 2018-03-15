/**
 *   Client header definitions.
 *
 *   @file
 *
 *   Copyright (C) 2014-2015 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-12-17 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2015-01-26 Anette Schött
 *   Change  : Add TRACE_LINUX_OBJ macro.
 * ========================================================================
 */

#ifndef __CLIENT_HEADER_H
#define __CLIENT_HEADER_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <cello_te_group.h>
#include <cello_te_trace_obj.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/*
 * Define a client implemented macro (not allowed but done anyway).
 * Use OSE syntax for pointer name in the macro.
 */
#define TRACE_OMCSF_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                        TRACEOBJ, /* I: The trace object to control this trace    */ \
                        MSG)      /* I: Message string                            */ \
                                (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
                                GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) && \
                                OMCSF_logObjTrace(OMCSF_GROUP_TRACE ## GROUP, __SHORT_FILE__,\
                                                  __LINE__, MSG,        \
                                                  procInfo, OMCSF_traceObjInfo ## TRACEOBJ ## _p))

/*
 * Define a client implemented macro (not allowed but done anyway).
 * Use Linux syntax for pointer name in the macro.
 */
#define TRACE_LINUX_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                        TRACEOBJ, /* I: The trace object to control this trace    */ \
                        MSG)      /* I: Message string                            */ \
                                (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
                                GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) && \
                                lttngTriIfObjTrace(OMCSF_GROUP_TRACE ## GROUP, __SHORT_FILE__,\
                                                   __LINE__, MSG,       \
                                                   procInfo, traceObjInfo ## TRACEOBJ))

/*
 * Define a client implemented macro (not allowed but done anyway).
 * Use a mix of OSE and Linux syntax for pointer name and log function in the macro.
 */
#define TRACE_MIX1_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                       TRACEOBJ, /* I: The trace object to control this trace    */ \
                       MSG)      /* I: Message string                            */ \
                                 (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
                                 GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) && \
                                 lttngTriIfObjTrace(OMCSF_GROUP_TRACE ## GROUP, __SHORT_FILE__,\
                                                    __LINE__, MSG,      \
                                                    procInfo, traceObjInfo ## TRACEOBJ))

/*
 * Define a client implemented macro (not allowed but done anyway).
 * Use a mix of OSE and Linux syntax for pointer name and log function in the macro.
 */
#define TRACE_MIX2_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                       TRACEOBJ, /* I: The trace object to control this trace    */ \
                       MSG)      /* I: Message string                            */ \
                                 (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) &&\
                                 GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) && \
                                 OMCSF_logObjTrace(OMCSF_GROUP_TRACE ## GROUP, __SHORT_FILE__,\
                                                    __LINE__, MSG,      \
                                                    procInfo, traceObjInfo ## TRACEOBJ))

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

#endif   /* ifndef __CLIENT_HEADER_H */

/**
 *   Header file for tri test.
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
 *   Revised : 2013-09-30 Niranjan Kumar D
 *   Change  : First version.
 *
 *   Revised : 2013-12-12 M. Winberg
 *   Change  : Added deregisterIfObj extern declaration.
 *
 *   Revised : 2014-02-27 Anette Schött
 *   Change  : Corrected some incorrect parameters and changed
 *             deregisterIfObj to deRegisterIfObj.
 *
 *   Revised : 2014-03-04 Niranjan Kumar D
 *   Change  : Cleanup of unwanted declarations and include files.
 *
 *   Revised : 2015-01-26 Anette Schött
 *   Change  : Add client macros used to verify differnt combinations of
 *             OMCSF_xxx and Linux usage.
 *
 *   Revised : 2015-04-02 Anette Schött
 *   Change: : Added extern declarations for general functions to spawn
 *             a thread of different kind.
 *
 *   Revised : 2015-10-29 Ranganadh
 *   Change  : Added extern delcaration for general functions to start an
 *             object.
 *
 *   Revised : 2015-11-03 Anette Schött
 *   Change  : Change TEST_TE_LOG_PATH from /var/log/volatile to /tmp to
 *             make it to work in CRL test fram work.
 *
 * ========================================================================
 */
/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#ifndef TRI_TEST_H
#define TRI_TEST_H

#include <cello_te_ose.h>
#include <cello_te_trace_obj.h>
#include <stdio.h>
#include <stdlib.h>
#include <osetypes.h>
#include <gtest/gtest.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define TEST_TE_LOG_PATH "/tmp/test_te_log"
#define TEST_TE_SESSION "s5"

#define TEST_THREAD_NAME "testThread"

/*
 * Define a client implemented macro (not allowed but done anyway).
 * Use OSE syntax for pointer name and trace logging in the macro.
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
 * Use Linux syntax for pointer name and trace logging in the macro.
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
 * Use mix of OSE and Linux syntax for pointer name and trace logging
 * in the macro.
 */
#define TRACE_OMCSF_LINUX_OBJ(GROUP,    /* I: Which group (1 - 9) this trace belongs to */\
                              TRACEOBJ, /* I: The trace object to control this trace    */ \
                              MSG)      /* I: Message string                            */ \
                                     (void)(OBJ_GROUP_IS_ENABLED(TRACEOBJ, OMCSF_GROUP_TRACE ## GROUP) && \
                                     GROUP_IS_ENABLED(OMCSF_GROUP_TRACE_OBJ) && \
                                     lttngTriIfObjTrace(OMCSF_GROUP_TRACE ## GROUP, __SHORT_FILE__, \
                                                        __LINE__, MSG,  \
                                                        procInfo, OMCSF_traceObjInfo ## TRACEOBJ ## _p))

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Pointer to thread specific data */
extern struct ItemInfo *testProcInfo;

/* Holds process id of tri server */
extern PROCESS triPid;

/* Holds process id of the test thread */
extern PROCESS testPid;

/* Thread identities */
extern pthread_t daemonForwardTid;
extern pthread_t serverStubTid;
extern pthread_t daemonStubTid;

/* Mail boxes */
extern itc_mbox_id_t daemonMbox;
extern itc_mbox_id_t daemonForwardMbox;
extern itc_mbox_id_t serverStubMbox;
extern itc_mbox_id_t testBox;

/*Extern declaration for tri server functions */
extern "C" void triServer();
extern "C" void spawntriServerProcs(void);

/*Extern declaration for tri deamon functions */
extern "C" void createTriDaemon(void);

/* Extern declaration for function creating tri test thread */
extern "C" void spawnTestThread(void);
extern "C" void spawnTestObject(void);

/*Extern declaration for tri server stub */
extern "C" void spawnServerStubThread(void);

/*Extern declaration for tri deamon forwarding thread */
extern "C" void spawnDaemonForwardThread(void);

/*Extern declaration for tri daemon stub */
extern "C" void spawnDaemonStubThread(void);

/* Extern declaration for function to locate the tri daemon */
extern "C" void locateTriDaemon(void);

#endif /* TRI_TEST_H */


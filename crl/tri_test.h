/**
 *   Header file for tri test.
 * 
 *   Copyright (C) 2013-2016 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-12-10 Anette Schött
 *   Change  : Add some extern declarations.
 *
 *   Revised : 2016-02-26 Anette Schött
 *   Change  : Added waitForTracingDone.
 *
 *   Revised : 2015-04-01 Miroslav Krizanic
 *   Change  : Added extern declarations for scheduling policy test.
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
#include <gtest/gtest.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define TEST_TE_LOG_PATH "/tmp/test_te_log"
#define TEST_TE_SESSION "s5"

#define TEST_PFS_PATH "/tmp"
#define TEST_TE_SAVE_PATH "/tmp/tri"
#define TEST_TE_SAVE_FILE_NAME "te_save.txt"

#define TEST_THREAD_NAME "testThread"
#define TEST_PTHREAD_NAME "testPthread"

#define MAX_NAME_LEN 16

#define TRI_TEST_MSG (0xFFFFFFFF)

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

typedef struct
{
   uint32_t            msgNo;
   uint32_t            sched_policy;
   uint32_t            sched_priority;
   uint32_t            child_pid;
} TriTestMsg;

/**
 * Failure to test
 */
typedef enum {
   TEST_ABORT,
   TEST_EXIT_SUCCESS,
   TEST_EXIT_FAILURE,
   TEST_SEGV
} failure_cause;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Pointer to thread specific data */
extern struct ItemInfo *testProcInfo;

/* Holds process id of tri server */
extern pid_t triServerPid;

/* Holds process id of the lits test thread */
extern PROCESS testPid;

/* Holds non-lits process id */
extern pid_t noLitsTestPid;

/* Thread identities */
extern pthread_t daemonForwardTid;
extern pthread_t serverStubTid;
extern pthread_t daemonStubTid;

/* Mail boxes */
extern itc_mbox_id_t daemonForwardMbox;
extern itc_mbox_id_t serverStubMbox;
extern itc_mbox_id_t testBox;

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

/*Extern declaration for tri server functions */
extern "C" void triServer();
extern "C" void spawntriServerProcs(void);

/* Extern declaration for function creating tri test thread */
extern "C" void spawnTestThread(void);
extern "C" void spawnTestProcess(char *name, failure_cause fail_cause);
extern "C" void spawnTestObject(void);

/*Extern declaration for tri server stub */
extern "C" void spawnServerStubThread(void);

/*Extern declaration for tri deamon forwarding thread */
extern "C" void spawnDaemonForwardThread(void);

/*Extern declaration for tri daemon stub */
extern "C" void spawnDaemonStubThread(void);

/* Extern declaration for function to locate the tri daemon */
extern "C" itc_mbox_id_t locateTriDaemon(void);

/* Extern declaration of function which waits until tracing is done */
extern void waitForTracingDone(void);

/* Extern declaration for function to remove saved trace groups file */
extern "C" int removeSaveTraceGroupFile(int *error_num);

extern "C" void confSchedulingPolicyEnv(int policy, int priority) ;

extern "C" void getTriDaemonSchedParams(int* policy, int* priority);

extern "C"  pid_t getProcessLatestTask(pid_t process_id);

extern "C" void sendTriDeamonSchedParams(itc_mbox_id_t parent_mboxid);

#endif /* TRI_TEST_H */


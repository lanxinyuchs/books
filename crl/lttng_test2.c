/**
 *   Test client which is NOT using Lits library.

 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <cello_te_ose.h>
#include <cello_te_trace.h>
#include <cello_te_trace_if.h>
#include <cello_te_trace_obj.h>

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <syslog.h>
#include <itc.h>
#include <errno.h>
#include <getopt.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "lttng_test.h"
#include "lttng_test.sig"

#define TRACEPOINT_DEFINE
#include "com_ericsson_plf_lttng_test.h"

#define TRACEPOINT_DEFINE
#include "com_ericsson_plf_lttng_test1.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define MAX_TRACES_PER_SECOND 5000
#define TRACE_OVERLOAD_LOOPS  10
#define DEFAULT_WAIT_IN_USEC  100000 /* 100 msec */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg {
        uint32_t              msgno;
        struct LttngTestStart start;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

DECLARE_INTERFACE(LTTNG_TRI_IF);
DECLARE_TRACE_OBJ(LTTNG_TRI_OBJ);
DECLARE_TRACE_OBJ(LTTNG_TRI_OBJ_1);

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void testReturn(void)
{
   RETURN;
}

/* ===================================================================== */
/**
 *   Generates traces via TRI from thread or process context.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_tri_trace(union itc_msg *msg)
{
   char msg_name[20]       = "LTTNG_TEST_START";
   char smallData[100];
   char bigData[2048];
   int i, j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      INFO("Test of INFO");
      TRACE_ERROR("Test of ERROR");
      TRACE(1, "Test of TRACE1");
      TRACE(2, "Test of TRACE2");
      TRACE(3, "Test of TRACE3");
      TRACE(4, "Test of TRACE4");
      TRACE(5, "Test of TRACE5");
      TRACE(6, "Test of TRACE6");
      TRACE(7, "Test of TRACE7");
      TRACE(8, "Test of TRACE8");
      TRACE(9, "Test of TRACE9");
      TRACE_PARAM("Test of PARAM");
      TRACE_STATE("Test of STATE");
      ENTER("Test of ENTER");
      testReturn();
      TRACE_REC_SIG(msg, "Test of REC_SIG");
      TRACE_SEND_SIG(msg, itc_sender(msg), "Test of SEND_SIG");
      INFO(STR("Test of STR: Signal name %s, Signal no %d",
               msg_name, itc_sender(msg) ));
      /* Generate TRI binary traces */
      for( i = 0; i < 100; i++ )
         smallData[i] = (char)i;
      for( i = 0; i < 2048; i++ )
         bigData[i] = (char)i;

      TRACE_BUS_SEND("Test of BUS_SEND with 100 byte data",
                     (uint8_t *)smallData, 100);
      TRACE_BUS_RECEIVE("Test of BUS_RECEIVE with 2048 byte data",
                        (uint8_t *)bigData, 2048);
      /* Generate TRI trace UTS */
      /*TRACE_UTS(1, 100, 500,
        "lttng_test.c", 102, "lttng2_test",
        "test of TRACE_UTS", smallData, 100);*/
   }
}

/* ===================================================================== */
/**
 *   Generates traces via TRI from interface context.
 *
 *   @param     msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_tri_trace_if(union itc_msg *msg)
{
   char smallData[100];
   char bigData[2048];
   int i, j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      TRACE_IF(1, "Test of TRACE1");
      TRACE_IF(2, "Test of TRACE2");
      TRACE_IF(3, "Test of TRACE3");
      TRACE_IF(4, "Test of TRACE4");
      TRACE_IF(5, "Test of TRACE5");
      TRACE_IF(6, "Test of TRACE6");
      TRACE_IF(7, "Test of TRACE7");
      TRACE_IF(8, "Test of TRACE8");
      TRACE_IF(9, "Test of TRACE9");
      TRACE_IF_STATE("Test of STATE");
      ENTER("Test of ENTER");
      testReturn();
      TRACE_IF_REC_SIG(msg, "Test of REC_SIG");
      TRACE_IF_SEND_SIG(msg, itc_sender(msg), "Test of SEND_SIG");
      /*  Generate interface binary traces */
      for( i = 0; i < 100; i++ )
         smallData[i] = (char)i;
      for( i = 0; i < 2048; i++ )
         bigData[i] = (char)i;

      TRACE_IF_BUS_SEND("Test of BUS_SEND with 100 byte data",
                        (uint8_t *)smallData, 100);
      TRACE_IF_BUS_RECEIVE("Test of BUS_RECEIVE with 2048 byte data",
                           (uint8_t *)bigData, 2048);
   }
}

/* ===================================================================== */
/**
 *   Generates LTTng traces.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_lttng_trace(union itc_msg *msg)
{
   int j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      tracepoint(com_ericsson_plf_lttng_test, EMERG_Test,   "Test of EMERG");
      tracepoint(com_ericsson_plf_lttng_test, ALERT_Test,   "Test of ALERT");
      tracepoint(com_ericsson_plf_lttng_test, CRIT_Test,    "Test of CRIT");
      tracepoint(com_ericsson_plf_lttng_test, ERR_Test,     "Test of ERR");
      tracepoint(com_ericsson_plf_lttng_test, WARNING_Test, "Test of WARNING");
      tracepoint(com_ericsson_plf_lttng_test, NOTICE_Test,  "Test of NOTICE");
      tracepoint(com_ericsson_plf_lttng_test, INFO_Test,    "Test of INFO");
      tracepoint(com_ericsson_plf_lttng_test, DEBUG_Test,   "Test of DEBUG");
      tracepoint(com_ericsson_plf_lttng_test1, EMERG_Test,   "Test of EMERG");
      tracepoint(com_ericsson_plf_lttng_test1, ALERT_Test,   "Test of ALERT");
      tracepoint(com_ericsson_plf_lttng_test1, CRIT_Test,    "Test of CRIT");
      tracepoint(com_ericsson_plf_lttng_test1, ERR_Test,     "Test of ERR");
      tracepoint(com_ericsson_plf_lttng_test1, WARNING_Test, "Test of WARNING");
      tracepoint(com_ericsson_plf_lttng_test1, NOTICE_Test,  "Test of NOTICE");
      tracepoint(com_ericsson_plf_lttng_test1, INFO_Test,    "Test of INFO");
      tracepoint(com_ericsson_plf_lttng_test1, DEBUG_Test,   "Test of DEBUG");
   }
}

/* ===================================================================== */
/**
 *   Generates LTTng traces with high through put.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void perf_lttng_trace(union itc_msg *msg)
{
   unsigned int i = 1, j;
   char string[128];
   int max_traces_per_sec = MAX_TRACES_PER_SECOND;
   int trace_overload_loops = TRACE_OVERLOAD_LOOPS;

   if (msg->start.argc > 1)
   {
      trace_overload_loops =  msg->start.arguments[i];
      i++;
   }

   if (msg->start.argc > 2)
   {
      max_traces_per_sec =  msg->start.arguments[i];
   }

   /* The number of microseconds to sleep between each trace */
   useconds_t us = (1000 * 1000) /  max_traces_per_sec;

   for(i = 0; i < trace_overload_loops; i++)
   {
      for(j = 0; j < max_traces_per_sec; j++)
      {
         sprintf(string, "Trace overload test, iteration %d/%d, loop %d/%d",
                 i+1, TRACE_OVERLOAD_LOOPS, j+1, max_traces_per_sec);
         tracepoint(com_ericsson_plf_lttng_test, INFO_Test, string);

         usleep(us);
      }
   }
}

/* ===================================================================== */
/**
 *   Generates traces via TRI with high through put.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void perf_tri_trace(union itc_msg *msg)
{
   unsigned int i = 0, j;
   int max_traces_per_sec = MAX_TRACES_PER_SECOND;
   int trace_overload_loops = TRACE_OVERLOAD_LOOPS;

   if (msg->start.argc > 1)
   {
      trace_overload_loops =  msg->start.arguments[i];
      i++;
   }

   if (msg->start.argc > 2)
   {
      max_traces_per_sec =  msg->start.arguments[i];
   }

   /* The number of microseconds to sleep between each trace */
   useconds_t us = (1000 * 1000) /  max_traces_per_sec;

   for(i = 0; i < trace_overload_loops; i++)
   {
      for(j = 0; j < max_traces_per_sec; j++)
      {
         INFO(STR("Trace overload test, iteration %d/%d, loop %d/%d",
                  i+1, trace_overload_loops, j+1,
                  max_traces_per_sec));
         usleep(us);
      }
   }
}

/* ===================================================================== */
/**
 *   Generates traces via TRI from object context.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_tri_trace_obj(union itc_msg *msg)
{
   char smallData[100];
   int j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(3, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(4, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(5, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(6, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(7, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(8, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(9, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ_SEND_SIG(LTTNG_TRI_OBJ, msg, itc_sender(msg),
                         "Test of SEND_SIG for TRACE OBJ");
      TRACE_OBJ_REC_SIG(LTTNG_TRI_OBJ, msg,
                        "Test of REC_SIG for TRACE OBJ");
      TRACE_OBJ_PARAM(LTTNG_TRI_OBJ, "Test of PARAM for TRACE OBJ");
      TRACE_OBJ_STATE(LTTNG_TRI_OBJ, "Test of STATE for TRACE OBJ");
      TRACE_OBJ_BUS_SEND(LTTNG_TRI_OBJ,
                         "Test of BUS_SEND with 100 byte data for TRACE OBJ",
                         (uint8_t *)smallData, 100);
      TRACE_OBJ_BUS_RECEIVE(LTTNG_TRI_OBJ,
                            "Test of BUS_RECEIVE with 100 byte data for TRACE OBJ",
                            (uint8_t *)smallData, 100);
   }
}

/* ===================================================================== */
/**
 *   Generates LTTng binary traces.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_lttng_binary_trace(union itc_msg *msg)
{
   char smallData[100];
   char bigData[2048];
   int i, j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      for( i = 0; i < 100; i++ )
         smallData[i] = (char)i;
      for( i = 0; i < 2048; i++ )
         bigData[i] = (char)i;

      tracepoint(com_ericsson_plf_lttng_test, Binary_Test,
                 "Test of binary trace with 100 byte data", smallData, 100);
      tracepoint(com_ericsson_plf_lttng_test, Binary_Test,
                 "Test of binary trace with 2048 byte data", bigData, 2048);
   }
}

/* ===================================================================== */
/**
 *   Generates large traces via TRI from thread context.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_tri_trace_multi_segment(union itc_msg *msg)
{
   int j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      INFO(MULTI_SEG_TRACE_1);
      INFO(MULTI_SEG_TRACE_2);
      INFO(MULTI_SEG_TRACE_3);
      INFO(MULTI_SEG_TRACE_4);
      INFO(MULTI_SEG_TRACE_5);
   }
}

/* ===================================================================== */
/**
 *   Generates large LTTng traces.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_lttng_trace_multi_segment(union itc_msg *msg)
{
   int j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {   tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_1);
      tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_2);
      tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_3);
      tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_4);
      tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_5);
   }
}

/* ===================================================================== */
/**
 *   Generates LTTng traces at high rate from multiple threads.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_lttng_trace_high_rate_mutliple_threads(union itc_msg *msg)
{
   char buf[16] = {0};
   pthread_t tid[12];
   int i, ret;
   int j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      for (i = 0; i < 12; i++) {
         switch(i) {
            case 0:
            case 1:
            case 2:
               sprintf(buf, "high_trace_%d", i);
               ret = pthread_create(&tid[i], NULL, high_traceA, (void*)buf);
               if (ret != 0) {
                  printf("Failed to create thread %s, ret=%d", buf, ret);
                  abort();
               }
               break;
            case 3:
            case 4:
            case 5:
               sprintf(buf, "high_trace_%d", i);
               ret = pthread_create(&tid[i], NULL, high_traceB, (void*)buf);
               if (ret != 0) {
                  printf("Failed to create thread %s, ret=%d", buf, ret);
                  abort();
               }
               break;
            case 6:
            case 7:
            case 8:
               sprintf(buf, "high_trace_%d", i);
               ret = pthread_create(&tid[i], NULL, high_trace_defprocA, (void*)buf);
               if (ret != 0) {
                  printf("Failed to create thread %s, ret=%d", buf, ret);
                  abort();
               }
               break;
            case 9:
            case 10:
            case 11:
               sprintf(buf, "high_trace_%d", i);
               ret = pthread_create(&tid[i], NULL, high_trace_defprocB, (void*)buf);
               if (ret != 0) {
                  printf("Failed to create thread %s, ret=%d", buf, ret);
                  abort();
               }
               break;
         }

         sleep(1);
      }

      sleep(5);
   }
}

/* ===================================================================== */
/**
 *   Generates LTTng traces form several cores.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_lttng_multi_core_trace(union itc_msg *msg)
{
   ; //TBD
}

/* ===================================================================== */
/**
 *   Testing that multiple tri_init doesn't cause two TRI daemons are
 *   spawn for this process.
 *
 *   @param      msg  Test signal from shell.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void test_of_multiple_tri_init(union itc_msg *msg)
{
   pthread_t tid;
   int ret;
   int j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      if ((ret = pthread_create(&tid, NULL, lttng2_tri_test_2, NULL)) != 0 ) {

         printf("%s: Failed to create lttng2_tri_test_2, ret=%d",
                __func__, ret);

         pthread_exit(&ret);
      }
      pthread_join(tid, NULL);

      //TBD, check number of TRI daemons
   }
}

/* ===================================================================== */
/**
 *   Starts thread and waits until it dies.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void create_and_kill_threads(union itc_msg *msg)
{
   pthread_t tid;
   int ret;
   int i = 1, j = 0;
   int wait_in_usec = DEFAULT_WAIT_IN_USEC;
   int loops = TRACE_OVERLOAD_LOOPS;

   if (msg->start.argc > 1)
   {
      loops =  msg->start.arguments[i];
      i++;
   }

   if (msg->start.argc > 2)
   {
      wait_in_usec =  1000 * msg->start.arguments[i];
   }

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      if ((ret = pthread_create(&tid, NULL, lttng2_tri_kill_test, NULL)) != 0 )
      {

         printf("%s: Failed to create lttng2_tri_kill_test, ret=%d",
                __func__, ret);

         return;
      }
      if ((ret = pthread_join(tid, NULL)) != 0)
      {
         printf("%s: pthread_join failed, ret=%d", __func__, ret);
      }
      usleep(wait_in_usec);
   }
}

/* ===================================================================== */
/**
 *   Registers and deregisters trace object.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void register_deregister_object(union itc_msg *msg)
{
   int i = 1, j = 0;
   int wait_in_usec = DEFAULT_WAIT_IN_USEC;
   int loops = TRACE_OVERLOAD_LOOPS;

   if (msg->start.argc > 1)
   {
      loops =  msg->start.arguments[i];
      i++;
   }

   if (msg->start.argc > 2)
   {
      wait_in_usec =  1000 * msg->start.arguments[i];
   }

   while (j++ != loops)
   {
      deRegisterIfObj(&traceObjInfoLTTNG_TRI_OBJ_1);

      sleep(2);

      registerIfObj("LTTNG_TRI_OBJ_1", &traceObjInfoLTTNG_TRI_OBJ_1);

      usleep(wait_in_usec);
   }
}

/* ===================================================================== */
/**
 *   Genetares traces via TRI Low Level functions for thread, interface and
 *   object.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_tri_low_level_traces(union itc_msg *msg)
{
   int j = 0, loops;

   loops =  msg->start.arguments[1];

   while (j++ != loops)
   {
      (void)OMCSF_logTrace(4,
                           __SHORT_FILE__,
                           __LINE__,
                           STR("My process string with val=%d", 444),
                           TRI_PROC_INFO_PTR);

      (void)OMCSF_logObjTrace(4,
                              __SHORT_FILE__,
                              __LINE__,
                              STR("My interface string with val=%d", 444),
                              TRI_PROC_INFO_PTR,
                              TRI_IF_INFO_PTR);

      (void)OMCSF_logObjTrace(4,
                              __SHORT_FILE__,
                              __LINE__,
                              STR("My object string with val=%d", 444),
                              TRI_PROC_INFO_PTR,
                              TRI_OBJ_INFO_PTR(LTTNG_TRI_OBJ));
   }
}

/* ===================================================================== */
/**
 *   Function running all test cases.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void lttng2_tri_test(void)
{
   char name[32];
   itc_mbox_id_t mbox;
   union itc_msg *msg;
   uint32_t rxFilter[] = {0};
   int ret;

   openlog("LTTNG2_TRI_TEST", 0, LOG_DAEMON);

   ret = itc_init(2, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
   if (ret != 0)
   {
      printf("itc_init failed, ret=%d", ret);
      abort();
   }

   snprintf(name, sizeof(name), "%s_%u", "test_mbox", getpid());
   mbox = itc_create_mailbox(name, 0);

   /* Do init of TRI for this process */
   tri_init();

   msg = itc_receive(rxFilter, ITC_NO_TMO, ITC_FROM_ALL);

   switch(msg->start.arguments[0])
   {
      case 1:
      {
         printf("Generating TRI traces\n");
         generate_tri_trace(msg);
         break;
      }
      case 2:
      {
         printf("Generating TRI IF traces\n");
         generate_tri_trace_if(msg);
         break;
      }
      case 3:
      {
         printf("Generating LTTng traces\n");
         generate_lttng_trace(msg);
         break;
      }
      case 4:
      {
         printf("Generating %d traces per second for %d seconds...\n",
                msg->start.arguments[1], msg->start.arguments[2]);
         perf_lttng_trace(msg);
         break;
      }
      case 5:
      {
         /*
           Characteristics requirement PECA_100:
           The SW platform shall allow all applications sharing
           one HW-thread to produce 5000 traces per second.
         */
         printf("Generating %d traces per second for %d seconds...\n",
                msg->start.arguments[1], msg->start.arguments[2]);
         perf_tri_trace(msg);
         break;
      }
      case 7:
      {
         printf("Generating TRI OBJ traces\n");
         generate_tri_trace_obj(msg);
         break;
      }
      case 8:
      {
         printf("Generating LTTng binary traces\n");
         generate_lttng_binary_trace(msg);
         break;
      }
      case 9:
      {
         printf("Generating Multi segment TRI traces\n");
         generate_tri_trace_multi_segment(msg);
         break;
      }
      case 10:
      {
         printf("Generating Multi segment LTTng traces\n");
         generate_lttng_trace_multi_segment(msg);
         break;
      }
      case 11:
      {
         printf("Generating LTTng traces from multiple threads at a high rate\n");
         generate_lttng_trace_high_rate_mutliple_threads(msg);
         break;
      }
      case 12:
      {
         printf("Generate LTTng multi core traces\n");
                generate_lttng_multi_core_trace(msg);
         break;
      }
      case 13:
      {
         printf("(TBD)\n");
         break;
      }
      case 14:
      {
         printf("Generate traces with identity\n");
         test_of_multiple_tri_init(msg);
         break;
      }
      case 15:
      {
         printf("Create and kill threads\n");
         create_and_kill_threads(msg);
         break;
      }
      case 16:
      {
         printf("Register and deregister object\n");
         register_deregister_object(msg);
         break;
      }
      case 17:
      {
         printf("Generate traces with Low Level TRI functions\n");
         generate_tri_low_level_traces(msg);
         break;
      }
      default:
      {
         printf("Internal error tc=%d\n", msg->start.tc);
         break;
      }
   }

   itc_free(&msg);
   itc_delete_mailbox(mbox);

   closelog();
   return;
}

/* ===================================================================== */
/**
 *   Print usage of lttng_test2 command.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void printUsage()
{
   printf("LTTng test command for native Linux application test\n");
   printf("Usage: lttngtc2 <-tc n> [argc1] [args2] ... [args10]\n");
   printf("\tIf nothing else is specified for a tc:\n");
   printf("\targ1 = Number of loops (-1 for infinit looping)\n");
   printf("\targ2-arg10 = For future usage\n");
   printf("\tWhere tc is one of:\n");
   printf("\t1:  Generate TRI traces\n");
   printf("\t2:  Generate TRI interface traces\n");
   printf("\t3:  Generate LTTng traces\n");
   printf("\t4:  LTTng trace overload.\n");
   printf("\t    arg1=Number of loops (default: 10)\n");
   printf("\t    arg2=Max traces/sec (default: 5000)\n");
   printf("\t5:  TRI trace overload\n");
   printf("\t    arg1=Number of loops (default: 10)\n");
   printf("\t    arg2=Max traces/sec (default: 5000)\n");
   printf("\t6:  Streaming traces (TBD)\n");
   printf("\t7:  Generate TRI trace objects\n");
   printf("\t8:  Generate LTTng binary traces\n");
   printf("\t9:  Generate TRI multi segment traces\n");
   printf("\t10: Generate LTTng multi segment traces\n");
   printf("\t11: Generate traces from multiple threads at a high rate\n");
   printf("\t12: Generate LTTng multi core traces (TBD)\n");
   printf("\t13: Generate max streamng traces (TBD)\n");
   printf("\t14: Test of multiple tri_init()\n");
   printf("\t15: Create and kill thread\n");
   printf("\t    arg1=Number of loops (default: 10)\n");
   printf("\t    arg2=Delay in msec between loops (default: 100 msec)\n");
   printf("\t16: Register and deregister object\n");
   printf("\t    arg1=Number of loops (default: 10)\n");
   printf("\t    arg2=Delay in mssec between loops (default: 100 msec)\n");
   printf("\t17: Generate traces with Low Level TRI functions\n");
}

/* ===================================================================== */
/**
 *   Main thread for test client.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
int main (int argc, char *argv[])
{
   char name[32];
   pid_t pid;
   itc_mbox_id_t my_mbox, test_mbox;
   union itc_msg *msg;
   int i = 0, ret, opt;

   if (argc < 3)
   {
      printUsage();
      return 0;
   }

   ret = itc_init(2, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
   if (ret != 0)
   {
      printf("%s: itc_init failed, ret=%d\n", __func__, ret);
      abort();
   }

   snprintf(name, sizeof(name), "%s_%u", "lttng_test2", getpid());
   my_mbox = itc_create_mailbox(name, 0);

   /* Start process which will handle the test case */
   if ((pid = fork()) == 0)
   {
      lttng2_tri_test();
      printf("\nTest finished!\n");
   }
   else
   {
      msg = itc_alloc(sizeof(struct LttngTestStart), LTTNG_TEST_START );

      /* Set number of loops to one as default */
      msg->start.arguments[1] = 1;

      while (((opt = getopt(argc, argv, "t:")) != -1))
      {
         switch (opt)
         {
            case 't':
            {
               if (argc > 5)
               {
                  printUsage();
                  return 1;
               }
               while (optind < argc)
               {
                  msg->start.arguments[i++] = atoi(argv[optind]);
                  optind++;
               }
               msg->start.argc = i;
               break;
            }
            default: /* '?' */
            {
               printUsage();
               return 1;
            }
         }
      }

      snprintf(name, sizeof(name), "%s_%u", "test_mbox", pid);
      while((test_mbox = itc_locate(name)) == ITC_NO_ID)
      {
         sleep(5);
      }

      itc_send(&msg, test_mbox, my_mbox);

      itc_delete_mailbox(my_mbox);
   }

   exit(EXIT_SUCCESS);
}

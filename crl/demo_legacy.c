/**
 *   TRI test program for interface trace item macros.
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
 *   Revised : 2013-06-05 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-08-16 Anette Schött
 *   Change  : Updates for bus send/receive
 *
 *   Revised : 2015-04-24 Anette Schött
 *   Change  : Added testThread process which is started and killed twice.
 *             Can be used to test for memory leakage when running Valgrind.
 *
 *   Revised : 2015-04-28 Anette Schött
 *   Change  : Updated so that all error macros are executed.
 *
 *   Revised : 2015-05-25 Anette Schött
 *   Change  : Corrected the TRACE_SEND/REC and error macro testing.
 *
 *   Revised : 2015-06-25 Anette Schött
 *   Change  : Increaded number of iterations for killing a thread.
 *
 *   Revised : 2015-10-30 Anette Schött
 *   Change  : Remove compiler warnings.
 * ========================================================================
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdarg.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <syslog.h>
#include <cello_te_ose.h>
#include <cello_te_trace.h>
#include <cello_te_error.h>
#include <cello_te_group.h>

#define CPP_SIGNAL (0xababa)
typedef struct
{
   uint32_t     signo;
}CppSignal;

union SIGNAL
{
   uint32_t     signo;
   CppSignal    cppSig;
};

static int 
myFunc(void)
{
   RAISE_STATUS(GROUP_ERROR);
   if (STATUS_IS_RAISED(GROUP_ERROR))
   {
      INFO("STATUS_IS_RAISED is raised");
   }
   CLEAR_ERROR_STATUS;
   RETURN 0;
}

OS_PROCESS(testThread)
{
   int i = 0;

   while(1)
   {
      INFO(STR("Test thread %d", i++));
      sleep(2);
   }
}


/* ========================================================================*/

OS_PROCESS(test3)
{
   int errCalls = 0;
   char text[10] = "test";
   int delay = 0;
   union SIGNAL *sig;
   static SIGSELECT sigSelAll[] = {0};
   uint8_t smallData[100];
   uint8_t bigData[2048];
   int i;
   PROCESS testPid;
   int loop = 0;

   /* Initialize the data arrays */
   for( i = 0; i < 100; i++ )
      smallData[i] = (char)i;
   for( i = 0; i < 2048; i++ )
      bigData[i] = (char)i;
      
   /* Open syslog for logging */
   openlog("TRI_DEMO:", 0, LOG_DAEMON);

   syslog(LOG_INFO, "Demo program test3 starting.");

   sleep(delay);

   PROC_LOOP
   {
      /*
       * Create and kill a thread. Allocated memory shall be freed and
       * can be investigated with Valgrind.
       */
      if (loop < 100)
      {
         testPid = create_process(OS_PRI_PROC,
                                  "testThread1",
                                  testThread,
                                  2048,
                                  30,
                                  (OSTIME)0,
                                  0,
                                  NULL,
                                  0, 0);
         start(testPid);
      }
      loop++;

      sleep(5);

      INFO(STR("test3 started test thread pid=0x%x", testPid));
      INFO(STR("procInfo->clientPtr: 0x%x ", procInfo->clientPtr));
      TRACE_ERROR("This is a ERROR from test3");
      ENTER("This is an ENTER from test");
      INFO("This is a INFO from test3");
      INFO(STR("This is a INFO with STR from test3 %d", 123));
      TRACE_UTS(1, 333, 2222, "demo_legacy.c", 123, "test2",  
                "This is a TRACE_UTS from test3", 
                smallData, strlen((char*)smallData));
      TRACE_UTS(GROUP_INFO, 222222, 3333333, __FILE__, __LINE__, "myTestName", 
                STR("This is an %d from test3", GROUP_INFO), (uint8_t *)text, strlen(text));
      TRACE(1, "This is a TRACE_1 from test3");
      TRACE(2, "This is a TRACE_2 from test3");
      TRACE(3, "This is a TRACE_3 from test3");
      TRACE(4, "This is a TRACE_4 from test3");
      TRACE(5, "This is a TRACE_5 from test3");
      TRACE(6, "This is a TRACE_6 from test3");
      TRACE(7, "This is a TRACE_7 from test3");
      TRACE(8, "This is a TRACE_8 from test3");
      TRACE(9, "This is a TRACE_9 from test3");
      TRACE_STATE("This is a TRACE_STATE from test3");
      TRACE_BUS_SEND("This is a BUS_SEND with 100 byte data from test3", smallData, 100);
      TRACE_BUS_SEND("This is a BUS_SEND with 1024 byte data from test3", bigData, 1024);
      TRACE_BUS_RECEIVE("This is a BUS_SEND with 100 byte data from test3", smallData, 100);
      TRACE_BUS_RECEIVE("This is a BUS_SEND with 1024 byte data from test3", bigData, 1024);
      TRACE_PARAM("This is a TRACE_PARAM from test3");


      if (loop < 100)
      {
         INFO(STR("test3 killed test thread pid=0x%x", testPid));
         kill_proc(testPid);
      }

      /*
       * Iterate between the different ways of macros.
       */
      if (loop % 4 == 0)
      {
         CLEAR_ERROR_STATUS;
         sig = alloc(sizeof(CppSignal), CPP_SIGNAL);
         TRACE_SEND_SIG(sig, 0xfffff, "This is a TRACE_SEND_SIG from test3");
         send(&sig, current_process());

         sig = receive(sigSelAll);
         TRACE_REC_SIG(sig, "This is a TRACE_REC_SIG from test3");
         ERROR_UNEXPECTED_SIG((union itc_msg *)sig);
      }

      if (loop % 4 == 1)
      {
         sig = alloc(sizeof(CppSignal), CPP_SIGNAL);
         TRACE_SEND_SIG(sig, 0xfffff, "This is a TRACE_SEND_SIG from test3");
         send(&sig, current_process());

         sig = receive(sigSelAll);
         TRACE_REC_SIG(sig, "This is a TRACE_REC_SIG from test3");
         FREE_BUF((union itc_msg **)&sig);
      }

      if (loop % 4 == 2)
      {
         CHECK(0);
      }
      //COMP_CHECK(0);

      if (loop % 4 == 3)
      {
         RAISE_STATUS(GROUP_ERROR);
         if (STATUS_IS_RAISED(GROUP_ERROR))
         {
            INFO("STATUS_IS_RAISED is raised");
            CLEAR_ERROR_STATUS;
         }
      }

      DO(myFunc());                     /* Calls CHECK_ERROR_STATUS */
  }
   closelog();

PROC_ERR:
   INFO(STR("This is PROC_ERR called for %d time", ++errCalls));
   RESUME;                            /* Calls CLEAR_ERROR_STATUS */
}

/* ========================================================================*/

OS_PROCESS(test4)
{
   int errCalls = 0;
   char text[10] = "test";
   int delay = 0;
   union SIGNAL *sig;
   uint8_t binData[512]= {0xff};
   uint16_t dataLength = strlen((char*)binData);
   static SIGSELECT sigSelAll[] = {0};
   uint8_t smallData[100];
   uint8_t bigData[2048];
   int i;
   PROCESS testPid;
   int loop = 0;

   /* Initialize the data arrays */
   for( i = 0; i < 100; i++ )
      smallData[i] = (char)i;
   for( i = 0; i < 2048; i++ )
      bigData[i] = (char)i;

   /* Open syslog for logging */
   openlog("TRI_DEMO:", 0, LOG_DAEMON);

   syslog(LOG_INFO, "Demo program test4 starting.");

   sleep(delay);

   PROC_LOOP
   {
      /*
       * Create and kill a thread. Allocated memory shall be freed and
       * can be investigated with Valgrind.
       */
      if (loop < 100)
      {
         testPid = create_process(OS_PRI_PROC,
                                  "testThread2",
                                  testThread,
                                  2048,
                                  30,
                                  (OSTIME)0,
                                  0,
                                  NULL,
                                  0, 0);
         start(testPid);
      }
      loop++;

      sleep(5);

      INFO(STR("test4 started test thread pid=0x%x", testPid));
      INFO(STR("procInfo->clientPtr: 0x%x ", procInfo->clientPtr));
      TRACE_ERROR("This is a ERROR from test4");
      ENTER("This is an ENTER from test");
      INFO("This is a INFO from test4");
      INFO(STR("This is a INFO with STR from test4 %d", 123));
      TRACE_UTS(1, 333, 2222, "demo_legacy.c", 123, "test4",  
                "This is a TRACE_UTS from test4", 
                binData, dataLength);
      TRACE_UTS(GROUP_INFO, 222222, 3333333, __FILE__, __LINE__, "myTestName", 
                STR("This is an %d from test4", GROUP_INFO), (uint8_t *)text, strlen(text));
      TRACE(1, "This is a TRACE_1 from test4");
      TRACE(2, "This is a TRACE_2 from test4");
      TRACE(3, "This is a TRACE_3 from test4");
      TRACE(4, "This is a TRACE_4 from test4");
      TRACE(5, "This is a TRACE_5 from test4");
      TRACE(6, "This is a TRACE_6 from test4");
      TRACE(7, "This is a TRACE_7 from test4");
      TRACE(8, "This is a TRACE_8 from test4");
      TRACE(9, "This is a TRACE_9 from test4");
      TRACE_STATE("This is a TRACE_STATE from test4");
      TRACE_BUS_SEND("This is a BUS_SEND with 100 byte data from test4", smallData, 100);
      TRACE_BUS_SEND("This is a BUS_SEND with 1024 byte data from test4", bigData, 1024);
      TRACE_BUS_RECEIVE("This is a BUS_SEND with 100 byte data from test4", smallData, 100);
      TRACE_BUS_RECEIVE("This is a BUS_SEND with 1024 byte data from test4", bigData, 1024);
      TRACE_PARAM("This is a TRACE_PARAM from test4");

      if (loop < 100)
      {
         INFO(STR("test4 killed test thread pid=0x%x", testPid));
         kill_proc(testPid);
      }
      /*
       * Iterate between the different ways of error macros.
       */
      if (loop % 4 == 0)
      {
         CLEAR_ERROR_STATUS;
         sig = alloc(sizeof(CppSignal), CPP_SIGNAL);
         TRACE_SEND_SIG(sig, 0xfffff, "This is a TRACE_SEND_SIG from test4");
         send(&sig, current_process());

         sig = receive(sigSelAll);
         TRACE_REC_SIG(sig, "This is a TRACE_REC_SIG from test4");
         ERROR_UNEXPECTED_SIG((union itc_msg *)sig);
      }

      if (loop % 4 == 1)
      {
         sig = alloc(sizeof(CppSignal), CPP_SIGNAL);
         TRACE_SEND_SIG(sig, 0xfffff, "This is a TRACE_SEND_SIG from test4");
         send(&sig, current_process());

         sig = receive(sigSelAll);
         TRACE_REC_SIG(sig, "This is a TRACE_REC_SIG from test4");
         FREE_BUF((union itc_msg **)&sig);
      }

      if (loop % 4 == 2)
      {
         CHECK(0);
      }

      if (loop % 4 == 3)
      {
         RAISE_STATUS(GROUP_ERROR);
         if (STATUS_IS_RAISED(GROUP_ERROR))
         {
            INFO("STATUS_IS_RAISED is raised");
            CLEAR_ERROR_STATUS;
         }
      }

      DO(myFunc());                     /* Calls CHECK_ERROR_STATUS */
   }
   closelog();

 PROC_ERR:
   INFO(STR("This is PROC_ERR called for %d time", ++errCalls));
   RESUME;                            /* Calls CLEAR_ERROR_STATUS */
}

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
 *   Revised : 2015-01-07 Anette Schött
 *   Change  : Added test for client implemented macro using OSE trace
 *             object pointer.
 *
 *   Revised : 2015-01-26 Anette Schött
 *   Change  : Add test for client TRACE_LINUX_OBJ macro.
 *
 *   Revised : 2015-02-13 Anette Schött
 *   Change  : Remove closelog() as never reached.
 *
 *   Revised : 2015-02-10 Anette Schött
 *   Change  : Add tests for client declared macro.
 *
 *   Revised : 2015-06-12 Anette Schött
 *   Change  : Minor correction of the test.
 *
 *   Revised : 2015-10-30 Anette Schött
 *   Change  : Remove compiler warnings.
 *
 * ========================================================================
 */

#include <stdio.h>
#include <unistd.h>
#include <syslog.h>
#include <cello_te_ose.h>
#include <cello_te_trace.h>
#include <cello_te_trace_obj.h>
#include <client_header.h>

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

/* ========================================================================*/
IMPORT_TRACE_OBJ(CELLO_OBJ)
IMPORT_TRACE_OBJ(CELLO_OBJ_1)

OS_PROCESS(test2)
{
   union SIGNAL *sig;
   uint8_t smallData[100];
   uint8_t bigData[2048];
   int i;


   /* Initialize the data arrays */
   for( i = 0; i < 100; i++ )
      smallData[i] = (char)i;
   for( i = 0; i < 2048; i++ )
      bigData[i] = (char)i;

   /* Open syslog for logging */
   openlog("TRI_DEMO_OBJ:", 0, LOG_DAEMON);

   syslog(LOG_INFO, "Demo program test2 starting.");

   while(1)
   {      
      sig = alloc(sizeof(CppSignal), CPP_SIGNAL);
      
      INFO("This is a INFO from test2");
      TRACE_OBJ(1, CELLO_OBJ, "This is a TRACE_OBJ_1 from test2");
      TRACE_OBJ(1, CELLO_OBJ, STR("This is a TRACE_OBJ_1 from test%d", 2));
      TRACE_OBJ(2, CELLO_OBJ, "This is a TRACE_OBJ_2 from test2");
      TRACE_OBJ(3, CELLO_OBJ, "This is a TRACE_OBJ_3 from test2");
      TRACE_OBJ(4, CELLO_OBJ, "This is a TRACE_OBJ_4 from test2");
      TRACE_OBJ(5, CELLO_OBJ, "This is a TRACE_OBJ_5 from test2");
      TRACE_OBJ(6, CELLO_OBJ, "This is a TRACE_OBJ_6 from test2");
      TRACE_OBJ(7, CELLO_OBJ, "This is a TRACE_OBJ_7 from test2");
      TRACE_OBJ(8, CELLO_OBJ, "This is a TRACE_OBJ_8 from test2");
      TRACE_OBJ(9, CELLO_OBJ, "This is a TRACE_OBJ_9 from test2");
      TRACE_OBJ_STATE(CELLO_OBJ, "This is a TRACE_OBJ_STATE from test2");
      TRACE_OBJ_PARAM(CELLO_OBJ, "This is a TRACE_OBJ_PARAM from test2");
      TRACE_OBJ_PARAM(CELLO_OBJ, STR("This is a TRACE_OBJ_PARAM from test%d", 2));
      TRACE_OBJ_BUS_SEND(CELLO_OBJ, "This is a BUS_SEND with 100 byte data from test2",
                     smallData, 100);
      TRACE_OBJ_BUS_SEND(CELLO_OBJ, "This is a BUS_SEND with 1024 byte data from test2",
                     bigData, 1024);
      TRACE_OBJ_BUS_RECEIVE(CELLO_OBJ, "This is a BUS_SEND with 100 byte data from test2",
                        smallData, 100);
      TRACE_OBJ_BUS_RECEIVE(CELLO_OBJ, "This is a BUS_SEND with 1024 byte data from test2",
                        bigData, 1024);
      TRACE_OBJ_REC_SIG(CELLO_OBJ, sig, "This is a TRACE_OBJ_REC_SIG from test2");
      TRACE_OBJ_SEND_SIG(CELLO_OBJ, sig, 0xfffff, "This is a TRACE_OBJ_SEND_SIG from test2");


      TRACE_OBJ(1, CELLO_OBJ_1, "This is a TRACE_OBJ_1 from test2");
      TRACE_OBJ(1, CELLO_OBJ_1, STR("This is a TRACE_OBJ_1 from test%d", 2));
      TRACE_OBJ(2, CELLO_OBJ_1, "This is a TRACE_OBJ_2 from test2");
      TRACE_OBJ(3, CELLO_OBJ_1, "This is a TRACE_OBJ_3 from test2");
      TRACE_OBJ(4, CELLO_OBJ_1, "This is a TRACE_OBJ_4 from test2");
      TRACE_OBJ(5, CELLO_OBJ_1, "This is a TRACE_OBJ_5 from test2");
      TRACE_OBJ(6, CELLO_OBJ_1, "This is a TRACE_OBJ_6 from test2");
      TRACE_OBJ(7, CELLO_OBJ_1, "This is a TRACE_OBJ_7 from test2");
      TRACE_OBJ(8, CELLO_OBJ_1, "This is a TRACE_OBJ_8 from test2");
      TRACE_OBJ(9, CELLO_OBJ_1, "This is a TRACE_OBJ_9 from test2");
      TRACE_OBJ_STATE(CELLO_OBJ_1, "This is a TRACE_OBJ_STATE from test2");
      TRACE_OBJ_PARAM(CELLO_OBJ_1, "This is a TRACE_OBJ_PARAM from test2");
      TRACE_OBJ_PARAM(CELLO_OBJ_1, STR("This is a TRACE_OBJ_PARAM from test%d", 2));
      TRACE_OBJ_BUS_SEND(CELLO_OBJ_1, "This is a BUS_SEND with 100 byte data from test2",
                     smallData, 100);
      TRACE_OBJ_BUS_SEND(CELLO_OBJ_1, "This is a BUS_SEND with 1024 byte data from test2",
                     bigData, 1024);
      TRACE_OBJ_BUS_RECEIVE(CELLO_OBJ_1, "This is a BUS_SEND with 100 byte data from test2",
                        smallData, 100);
      TRACE_OBJ_BUS_RECEIVE(CELLO_OBJ_1, "This is a BUS_SEND with 1024 byte data from test2",
                        bigData, 1024);
      TRACE_OBJ_REC_SIG(CELLO_OBJ_1, sig, "This is a TRACE_OBJ_REC_SIG from test2");
      TRACE_OBJ_SEND_SIG(CELLO_OBJ_1, sig, 0xfffff, "This is a TRACE_OBJ_SEND_SIG from test2");


      TRACE_OMCSF_OBJ(1, CELLO_OBJ, "This is a client implemented TRACE_OBJ_1 macro using "
                      "OSE trace object pointer from test2");
      TRACE_LINUX_OBJ(1, CELLO_OBJ, "This is a client implemented TRACE_OBJ_1 macro using "
                      "Linux trace object pointer from test2");

      TRACE_MIX1_OBJ(1, CELLO_OBJ, "This is a client implemented TRACE_OBJ_1 macro using "
                     "mix 1 of OSE and Linux syntax and functions from test2");
      TRACE_MIX2_OBJ(1, CELLO_OBJ, "This is a client implemented TRACE_OBJ_1 macro using "
                     "mix 2 of OSE and Linux syntax and functions from test2");

      free_buf(&sig);
      sleep(5);
   }
}

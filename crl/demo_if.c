/**
 *   TRI test program for interface trace item macros.
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
 *   Revised : 2013-06-05 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-08-16 Anette Schött
 *   Change  : Updates for bus send/receive
 *
 *   Revised : 2015-10-30 Anette Schött
 *   Change  : Remove compiler warnings.
 * ========================================================================
 */

#include <stdio.h>
#include <string.h>
#include <cello_te_ose.h>
#include <cello_te_error.h>
#include <cello_te_group.h>
#include <cello_te_trace_if.h>
#include <cello_te_trace.h>
#include "demo_if.h"


DECLARE_INTERFACE(CELLO_IF) 


union SIGNAL
{
   uint32_t     signo;
   CppSignal    cppSig;
};


static 
int myFunc(void)
{
   RAISE_STATUS(GROUP_ERROR);
   if (STATUS_IS_RAISED(GROUP_ERROR))
   {
      TRACE_IF(1, "STATUS_IS_RAISED is raised");
   }
   CLEAR_ERROR_STATUS;
   RETURN 0;
}

void  myIfFunc(void)
{
   char text[10] = "test";
   static SIGSELECT sigSelAll[] = {0};
   union SIGNAL *sig;
   uint8_t smallData[100];
   uint8_t bigData[2048];
   int i;


   /* Initialize the data arrays */
   for( i = 0; i < 100; i++ )
      smallData[i] = (char)i;
   for( i = 0; i < 2048; i++ )
      bigData[i] = (char)i;
   
   TRACE_ERROR("This is a ERROR from test5");            
   ENTER("This is an ENTER from test5");
   INFO("This is a INFO from test5");
   INFO(STR("This is a INFO with STR from test5 %d", 123));
   TRACE_UTS(1, 333, 2222, "demo_if.c", 123, "test5",  
             "This is a TRACE_UTS from test5", 
             smallData, strlen((char*)smallData));
   TRACE_UTS(GROUP_INFO, 222222, 3333333, __FILE__, __LINE__, "myIfProcName", 
             STR("This is an %d from myIfFunc", GROUP_INFO), (uint8_t*)text, strlen(text));
   TRACE_IF(1, "This is a TRACE_IF_1");
   TRACE_IF(2, "This is a TRACE_IF_2");
   TRACE_IF(3, "This is a TRACE_IF_3");
   TRACE_IF(4, "This is a TRACE_IF_4");
   TRACE_IF(5, "This is a TRACE_IF_5");
   TRACE_IF(6, "This is a TRACE_IF_6");
   TRACE_IF(7, "This is a TRACE_IF_7");
   TRACE_IF(8, "This is a TRACE_IF_8");
   TRACE_IF(9, "This is a TRACE_IF_9");
   TRACE_IF_STATE("This is a TRACE_IF_STATE");
   TRACE_IF_BUS_SEND("This is a BUS_SEND with 100 byte data from test3", smallData, 100);
   TRACE_IF_BUS_SEND("This is a BUS_SEND with 1024 byte data from test3", bigData, 1024);
   TRACE_IF_BUS_RECEIVE("This is a BUS_RECEIVE with 100 byte data from test3", smallData, 100);
   TRACE_IF_BUS_RECEIVE("This is a BUS_RECEIVE with 1024 byte data from test3", bigData, 1024);

   myFunc();

   CLEAR_ERROR_STATUS;
   sig = receive(sigSelAll);
   TRACE_IF_REC_SIG(sig, "This is a TRACE_IF_REC_SIG");
   FREE_BUF((union itc_msg**)&sig);
}

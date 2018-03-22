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
 *   Revised : 2015-10-30 Anette Schött
 *   Change  : Remove compiler warnings.
 * ========================================================================
 */

#include <stdio.h>
#include <unistd.h>
#include <syslog.h>
#include <cello_te_ose.h>
#include <cello_te_group.h>
#include <cello_te_error.h>
#include <cello_te_trace.h>
#include "demo_if.h"

OS_PROCESS(test5)
{
   union SIGNAL *sig;
   int i = 0;

   /* Open syslog for logging */
   openlog("TRI_DEMO_IF:", 0, LOG_DAEMON);

   syslog(LOG_INFO, "Demo test interface process starting.");
   
   PROC_LOOP
   {
      
      /* Send a signal to be received in the interface function */
      sig = alloc(sizeof(CppSignal), CPP_SIGNAL);

      TRACE_SEND_SIG(sig, 0xfffff, "This is a TRACE_IF_SEND_SIG");

      send(&sig, current_process());

      myIfFunc();
      
      i++;
      sleep(5);

      if (i%3 == 0)
      {
         break;
      }
   }

   RESUME;

   closelog();
}

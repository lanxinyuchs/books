/**
 *   This file handles ts shell commands targeted for the Java Trace server.
 *
 *   Most of the tasks are forwarded to the 'te' Java trace handler.
 *
 *
 *   @file ts_java_trace_handler.c
 *
 *
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-01-28 Daniel Lefwerth
 *   Change  : First version.
 *
 *   Revised : 2016-07-06 Fredrik Skog
 *   Change  : Moved command timeout to trace_cmd.h.
 *
 * ========================================================================
 */

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <itc.h>

#include "ts_internal.h"
#include "te_internal.h"
#include "java_trace_server.h"
#include "jth.h"
#include "trace_cmd.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

union itc_msg
{
   uint32_t msg_no;
   JtsStatusReq statusReq;
   JtsStatusInd statusInd;
   JtsGenericRsp genericRsp;
};


/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */


/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

static itc_mbox_id_t jts_mb = ITC_NO_ID;

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */


/* ========================================================================= */
/**
 *   Get the Java Trace Server mail box.
 *
 *   @return 0 if successful, -1 otherwise
 */
/* ========================================================================= */
static int
locateServer(void)
{
   if ((jts_mb = itc_locate(JTS_MBOX_NAME)) == ITC_NO_ID)
   {
      ERR("Failed to locate %s", JTS_MBOX_NAME);
      return -1;
   }
   return 0;
}

/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
cmd_status(const Session *session, const char *provider, bool *isSessionFound)
{
   union itc_msg *msg;
   uint32_t filter[] = {2, JTS_GENERIC_RSP, JTS_STATUS_IND};
   bool isDone = false;
   bool isFirstLine = true;
   int maxNameLength = 0;
   int count = 0;
   int i;
   int ret = CMD_SUCCESS;

   (void)session;
   (void)isSessionFound;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   /* Request status from JTS */
   msg = itc_alloc(sizeof(JtsStatusReq), JTS_STATUS_REQ);
   msg->statusReq.type = 0;
   strncpy(msg->statusReq.traceItem, provider, JTS_MAX_TRACE_ITEM_LEN);
   msg->statusReq.traceItem[JTS_MAX_TRACE_ITEM_LEN - 1] = 0;
   itc_send(&msg, jts_mb, ITC_MY_MBOX);

   while (!isDone) {
      msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
      if (msg == NULL) {
         ERR("Command timeout");
         return CMD_ERROR;
      }

      switch(msg->msg_no) {
      case JTS_GENERIC_RSP:
         isDone = true;
         break;

      case JTS_STATUS_IND:
         maxNameLength = 8;

         /* Count number of providers with non-default masks */
         for (i = 0; i < msg->statusInd.nTraceItems; i++)
         {
            if (msg->statusInd.info[i].mask == DEFAULT_MASK)
            {
               continue;
            }

            count++;
            if (strlen(msg->statusInd.info[i].traceItem) > maxNameLength) {
               maxNameLength = strlen(msg->statusInd.info[i].traceItem);
            }
         }

         /* Print header (first time only) */
         if (isFirstLine && (count > 0)) {
            MSG("--------------------------------------------------------------------------");
            MSG("Java Trace events on all sessions which have other than default group mask");
            MSG("%8s name%*senabled groups", "pid", maxNameLength, "");
            isFirstLine = false;
         }

         /* Print enabled trace groups for providers with non-default masks */
         for (i = 0; i < msg->statusInd.nTraceItems; i++) {
            if (msg->statusInd.info[i].mask != DEFAULT_MASK)
            {
               if (msg->statusInd.info[i].id != 0) {
                  _MSG("%08x", msg->statusInd.info[i].id);
               }
               else {
                  _MSG("%8c", '-');
               }
               _MSG(" %s", msg->statusInd.info[i].traceItem);
               _MSG("%*s",
                    (int)(maxNameLength + 4 - strlen(msg->statusInd.info[i].traceItem)),
                    " ");
               jth_printTraceGroups(msg->statusInd.info[i].mask);
               MSG(""); /* new line */
            }
         }
         break;
      }
   }

   /* If all providers have default masks, print it */
   if (isFirstLine) {
      MSG("-------------------------------------------------");
      MSG("All the Java trace events have default group mask");
   }
   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
cmd_enable(const Session *session,
           const char *provider,
           int nEvents, const char *events[])
{
   (void)session;
   return getJavaTraceHandler()->cmd_enable(provider, nEvents, events);
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
cmd_disable(const Session *session,
            const char *provider,
            int nEvents, const char *events[])
{
   (void)session;
   return getJavaTraceHandler()->cmd_disable(provider, nEvents, events);
}


/* ========================================================================= */
/**
 * See definition of TsTraceHandler.
 */
/* ========================================================================= */
static int
cmd_default(const Session *session,
            const char *provider,
	    Scope scope)
{
   (void)session;
   return getJavaTraceHandler()->cmd_default(provider, scope);
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
cmd_save(const Session *session, const char *provider)
{
   (void)session;
   return getJavaTraceHandler()->cmd_save(provider);
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
cmd_filter(const Session *session,
           int type, const char *traceFilter, const char *event)
{
   (void)session;
   return getJavaTraceHandler()->cmd_filter(type, traceFilter, event);
}



static TsTraceHandler javaHandler = {
   "JAVA",
   JTE_HANDLER,
   cmd_status,
   cmd_enable,
   cmd_disable,
   cmd_default,
   cmd_save,
   cmd_filter
};


/* ========================================================================= */
/**
 *   Register the Java handler as a command handler to the 'ts' command.
 */
/* ========================================================================= */
static __attribute__((constructor)) void
registerJavaHandler(void)
{
   ts_registerHandler(&javaHandler);
}


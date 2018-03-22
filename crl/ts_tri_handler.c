/**
 *   This file handles 'ts' shell commands targeted for the TRI server.
 *
 *   Most of the tasks are forwarded to the 'te' TRI handler.
 *
 *
 *   @file ts_tri_handler.c
 *
 *
 *   Copyright (C) 2014-2016 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-11-12 Ranganath
 *   Change  : Bugzilla:1873 fix: te status shows only requested info.
 *             Also added additional command validations.
 *
 *   Revised : 2014-11-26 Ranganath
 *   Change  : DO:Support for te filter
 *
 *   Revised : 2014-11-28 Ranganadh
 *   Change  : DO:Introduced warning for te  enable/disable commands
 *
 *   Revised : 2014-12-22 Ranganadh
 *   Change  : DO:Introduced support to distinguish te and ts commands
 *
 *   Revised : 2015-02-05 Daniel Lefwerth
 *   Change  : DO:Introduced separation of TED and TRI handlers.
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
#include <tri_server_cmd.h>
#include <tri.h>

#include "ts_internal.h"
#include "te_internal.h"
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
   uint32_t               msg_no;
   SetDefaultGroupMaskReq def_mask_req;
   SetDefaultGroupMaskCfm def_mask_cfm;
   StatusReq              status_req;
   StatusCfm              status_cfm;
   StatusInfoInd          status_ind;
   SaveGroupMaskReq       save_mask_req;
   SaveGroupMaskCfm       save_mask_cfm;
   GroupMaskReq           mask_req;
   GroupMaskCfm           mask_cfm;
   PresetGroupMaskReq     presetGroupMaskReq;
   PresetGroupMaskCfm     presetGroupMaskCfm;
   FilterSetReq           filterSetReq;
   FilterSetRsp           filterSetRsp;
};


/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */


/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

static itc_mbox_id_t tri_mb = ITC_NO_ID;

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */

/* ========================================================================= */
/**
 *   Get the TRI server mail box.
 *
 *   @return 0 if successful, -1 otherwise
 *
 */
/* ========================================================================= */
static int
locateServer(void)
{
   if (((tri_mb = itc_locate(TRI_SERVER_MAILBOX_NAME)) == ITC_NO_ID) &&
       ((tri_mb = itc_locate(TRI_SERVER_NAME)) == ITC_NO_ID))
   {
      ERR("Failed to locate tri");
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
tri_status(const Session *session, const char *provider, bool *isSessionFound)
{
   union itc_msg *msg;
   uint32_t filter[] = {2, STATUS_CFM, STATUS_INFO_IND};
   bool isDone = false;
   bool isFirstLine = true;
   int maxProviderLen = 0;
   int count = 0;
   int i;
   int ret = CMD_SUCCESS;

   (void)session;
   (void)isSessionFound;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   /* Request status from TRI */
   msg = itc_alloc(sizeof(StatusReq), STATUS_REQ);
   strncpy(msg->status_req.procName, provider, TRI_MAX_PROC_NAME_LEN);
   msg->status_req.procName[TRI_MAX_PROC_NAME_LEN - 1] = 0;
   itc_send(&msg, tri_mb, ITC_MY_MBOX);

   while (!isDone) {
      msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
      if (msg == NULL) {
         ERR("Command timeout");
         return CMD_ERROR;
      }

      switch(msg->msg_no) {
      case STATUS_CFM:
         isDone = true;
         break;

      case STATUS_INFO_IND:
         maxProviderLen = 8;

         /* Count number of providers with non-default masks */
         for (i = 0; i < msg->status_ind.noOfProc; i++)
         {
            if (msg->status_ind.status[i].groupMask == TRI_DEFAULT_GROUP_MASK)
            {
               continue;
            }

            count++;
            if (strlen(msg->status_ind.status[i].procName) > maxProviderLen)
            {
               maxProviderLen = strlen(msg->status_ind.status[i].procName);
            }
         }

         /* Print header (first time only) */
         if (isFirstLine && (count > 0)) {
            MSG("--------------------------------------------------------------------------");
            MSG("Common TRI events on all sessions which have other than default group mask");
            MSG("%8s name%*senabled groups", "pid", maxProviderLen, "");
            isFirstLine = false;
         }

         /* Print enabled trace groups for providers with non-default masks */
         for (i = 0; i < msg->status_ind.noOfProc; i++) {
            if (msg->status_ind.status[i].groupMask != TRI_DEFAULT_GROUP_MASK)
            {
               if (msg->status_ind.status[i].mbox != ITC_NO_ID)
                  _MSG("%08x", msg->status_ind.status[i].mbox);
               else
                  _MSG("%8c", '-');

               _MSG(" %s", msg->status_ind.status[i].procName);
               _MSG("%*s",
                    (int)(maxProviderLen + 4 - strlen(msg->status_ind.status[i].procName)),
                    " ");
               tri_printTraceGroups(msg->status_ind.status[i].groupMask);
               MSG(""); /* new line */
            }
         }
         break;
      }
   }

   /* If all providers have default masks, print it */
   if (isFirstLine) {
      MSG("------------------------------------------");
      MSG("All the TRI events have default group mask");
   }
   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
tri_enable(const Session *session,
               const char *provider,
               int nEvents, const char *events[])
{
   (void)session;
   return getTeTriHandler()->cmd_enable(provider, nEvents, events);
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
tri_disable(const Session *session,
            const char *provider,
            int nEvents, const char *events[])
{
   (void)session;
   return getTeTriHandler()->cmd_disable(provider, nEvents, events);
}


/* ========================================================================= */
/**
 * See definition of TsTraceHandler.
 */
/* ========================================================================= */
static int
tri_default(const Session *session,
            const char *provider,
	    Scope scope)
{
   (void)session;
   return getTeTriHandler()->cmd_default(provider, scope);
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
tri_save(const Session *session, const char *provider)
{
   (void)session;
   return getTeTriHandler()->cmd_save(provider);
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
tri_filter(const Session *session,
           int type, const char *traceFilter, const char *event)
{
   (void)session;
   return getTeTriHandler()->cmd_filter(type, traceFilter, event);
}



static TsTraceHandler triHandler = {
   "TRI",
   TRI_HANDLER,
   tri_status,
   tri_enable,
   tri_disable,
   tri_default,
   tri_save,
   tri_filter
};


/* ========================================================================= */
/**
 *   Register the TRI handler as a command handler to the 'ts' command.
 */
/* ========================================================================= */
static __attribute__((constructor)) void
registerTriHandler(void)
{
   ts_registerHandler(&triHandler);
}


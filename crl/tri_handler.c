/**
 *   This file handles te shell commands targeted for the TRI server.
 *
 *   @file tri_handler.c
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
 *   Revised : 2016-02-10 Anette Schött
 *   Change  : Moved get_scope to ted_util.c.
 *
 *   Revised : 2016-02-26 Anette Schött
 *   Change  : Change so 'te save *' will not print "Specified process
 *             not found."
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

#include "ted.h"
#include "tri.h"
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
 *   Support functionality to find group type
 *
 *   @param      gr group type pointer
 *               m  group mask correcponding to gr will be returned
 *
 *   @return
 */
/* ========================================================================= */
static int
updateGroupMask(const char *gr, uint32_t *m)
{
   uint32_t g_type;

   if (strcmp(gr, "all") == 0) {
      *m = 0xFFFFFFFF;
   }
   else {
      if ((g_type = tri_getGroupType(gr)) == GROUP_RESERVED2) {
         return -1;
      }
      *m |= (1 << g_type);
   }
   return 0;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
tri_status(const char *provider, int scope, bool isProgramSpecified)
{
   union itc_msg *msg;
   uint32_t filter[] = {2, STATUS_CFM, STATUS_INFO_IND};
   int i;
   bool isFirstLine = true;
   bool isDone = false;
   int max_pname_len = 0;

   (void)isProgramSpecified;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   if (scope == RESTART) {
      provider = "-restart";
   }
   else if (scope == PRESET) {
      provider = "-preset";
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
         max_pname_len = 8;
         for (i = 0; i < msg->status_ind.noOfProc; i++) {
            if (strlen(msg->status_ind.status[i].procName) > max_pname_len)
               max_pname_len = strlen(msg->status_ind.status[i].procName);
         }
         if (isFirstLine && msg->status_ind.noOfProc) {
            MSG("TRI status:"); /* new line */
            MSG("%8s name%*senabled groups", "pid", max_pname_len, "");
            isFirstLine = false;
         }
         for (i = 0; i < msg->status_ind.noOfProc; i++) {
            if (msg->status_ind.status[i].mbox != ITC_NO_ID)
               _MSG("%08x", msg->status_ind.status[i].mbox);
            else
               _MSG("%8c", '-');

            _MSG(" %s", msg->status_ind.status[i].procName);
            _MSG("%*s",
                 (int)(max_pname_len + 4 - strlen(msg->status_ind.status[i].procName)),
                 " ");
            tri_printTraceGroups(msg->status_ind.status[i].groupMask);
            MSG(""); /* new line */
         }
         break;

      default:
         ERR("Unknown msg 0x%x received", msg->msg_no);
         itc_free(&msg);
         return CMD_ERROR;
      }
      itc_free(&msg);
   }

   if (isFirstLine) {
      return CMD_IGNORED;
   }
   return CMD_SUCCESS;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
tri_enable(const char *provider, int nEvents, const char *events[])
{
   union itc_msg *msg;
   uint32_t filter[] = {1, GROUP_MASK_CFM};
   uint32_t groupMask = 0;
   int ret = 0;
   int i;

   /* Validate the trace groups */
   for (i = 0; i < nEvents; i++) {
      if (updateGroupMask(events[i], &groupMask) < 0) {
         /* If the provider is a wildcard, then assume that the command
          * should be handled by another handler
          */
         if (strcmp(provider, "*") == 0) {
            return CMD_SUCCESS;
         }
         ERR("Unknown trace group, %s\n%s", events[i], GROUP_SYNTAX);
         return CMD_ERROR;
      }
   }

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(GroupMaskReq), GROUP_MASK_REQ);
   msg->mask_req.change = TE_MASK_ENABLE;
   msg->mask_req.groupMask = groupMask;
   strncpy(msg->mask_req.procName, provider, TRI_MAX_PROC_NAME_LEN);
   msg->mask_req.procName[TRI_MAX_PROC_NAME_LEN - 1] = 0;
   itc_send(&msg, tri_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }

   if (msg->mask_cfm.result == TE_CMD_RESULT_SYNTAX_ERROR) {
      ERR("Failed to enable requested trace groups for this process");
      ret = CMD_ERROR;
   }
   else if (msg->mask_cfm.result == TE_CMD_RESULT_PROCESS_NOT_FOUND) {
      ret = CMD_IGNORED;
   }
   else {
      if (msg->mask_cfm.result != TE_CMD_RESULT_OK) {
         ret = CMD_ERROR;
      }
      else {
         ret = 0;
      }
   }
   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
tri_disable(const char *provider, int nEvents, const char *events[])
{
   int           ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t      filter[] = {1, GROUP_MASK_CFM};
   int           i;
   uint32_t      groupMask = 0;

   for (i = 0; i < nEvents; i++) {
      if (updateGroupMask(events[i], &groupMask) < 0) {
         /* If the provider is a wildcard, then assume that the command
          * should be handled by another handler
          */
         if (strcmp(provider, "*") == 0) {
            return CMD_SUCCESS;
         }
         ERR("Unknown trace group, %s\n%s", events[i], GROUP_SYNTAX);
         return CMD_ERROR;
      }
   }

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(GroupMaskReq), GROUP_MASK_REQ);
   msg->mask_req.change = TE_MASK_DISABLE;
   msg->mask_req.groupMask = groupMask;
   strncpy(msg->mask_req.procName, provider, TRI_MAX_PROC_NAME_LEN);
   msg->mask_req.procName[TRI_MAX_PROC_NAME_LEN - 1] = 0;
   itc_send(&msg, tri_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }

   if (msg->mask_cfm.result == TE_CMD_RESULT_SYNTAX_ERROR) {
      ERR("Failed to enable requested trace groups for this process");
      ret = CMD_ERROR;
   }
   else if (msg->mask_cfm.result == TE_CMD_RESULT_PROCESS_NOT_FOUND) {
      ret = CMD_IGNORED;
   }
   else if (msg->mask_cfm.result != TE_CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }
   itc_free(&msg);

   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
tri_preset_config(const char *provider, int nEvents, const char **events,
                  bool savePreset)
{
   int           ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t      filter[] = {1, PRESET_GROUP_MASK_CFM};
   uint32_t      groupMask;
   uint32_t      groupType;
   int           i;
   bool          isEnabled;

   groupMask = TRI_DEFAULT_GROUP_MASK;

   /* Construct the group mask and check the event names */
   for (i = 0; i < nEvents; i++)
   {
      /*
       * Check if the trace group shall be enabled or disabled,
       * i.e. if '+' or a '-' is specified before the name of the
       * trace group. The default is to enable.
       */
      isEnabled = true;
      if (events[i][0] == '+') {
         isEnabled = true;
         events[i]++;     /* Skip the '+' */
      }
      else if (events[i][0] == '-') {
         isEnabled = false;
         events[i]++;     /* Skip the '-' */
      }
      if (strcmp(events[i], "all") == 0) {
         /*
          * Enable or disable 'all' trace groups...
          */
         groupMask = isEnabled ? 0xFFFFFFFF : 0x00000000;
      }
      else {
         groupType = tri_getGroupType(events[i]);
         if (groupType == OMCSF_GROUP_RESERVED2)
         {
            /* If the provider is a wildcard, then assume that the command
             * should be handled by another handler
             */
            if (strcmp(provider, "*") == 0) {
               return CMD_SUCCESS;
            }
            ERR("Unknown trace group '%s'\n%s", events[i], GROUP_SYNTAX);
            return CMD_ERROR;
         }
         if (isEnabled) {
            groupMask |= (1 << groupType);
         }
         else {
            groupMask &= ~(1 << groupType);
         }
      }
   }

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(PresetGroupMaskReq), PRESET_GROUP_MASK_REQ);
   msg->presetGroupMaskReq.groupMask = groupMask;
   strncpy(msg->presetGroupMaskReq.procName, provider,
           TRI_MAX_PROC_NAME_LEN);
   msg->presetGroupMaskReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   msg->presetGroupMaskReq.savePreset = savePreset;
   itc_send(&msg, tri_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Failed to receive response for preset request");
      return CMD_ERROR;
   }
   if (msg->presetGroupMaskCfm.result != TE_CMD_RESULT_OK) {
      ERR("Failed to enable requested trace groups for this process");
      ret = CMD_ERROR;
   }
   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
tri_default(const char *provider, Scope scope)
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, SET_DEFAULT_GROUP_MASK_CFM};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(SetDefaultGroupMaskReq),
                   SET_DEFAULT_GROUP_MASK_REQ);
   strncpy(msg->def_mask_req.procName, provider, TRI_MAX_PROC_NAME_LEN);
   msg->def_mask_req.procName[TRI_MAX_PROC_NAME_LEN - 1] = 0;
   msg->def_mask_req.scope = get_scope(scope);
   itc_send(&msg, tri_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      return CMD_ERROR;
   }

   if (msg->def_mask_cfm.result == TE_CMD_RESULT_PROCESS_NOT_FOUND) {
      ERR("Specified process not found");
      ret = CMD_ERROR;
   }
   else if (msg->def_mask_cfm.result != TE_CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

   itc_free(&msg);

   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
tri_save(const char *provider)
{
   int ret = 0;
   union itc_msg *msg;
   uint32_t filter[] = {1, SAVE_GROUP_MASK_CFM};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(SaveGroupMaskReq), SAVE_GROUP_MASK_REQ);
   strncpy(msg->save_mask_req.procName, provider, TRI_MAX_PROC_NAME_LEN);
   msg->save_mask_req.procName[TRI_MAX_PROC_NAME_LEN - 1] = 0;
   itc_send(&msg, tri_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if ((msg->save_mask_cfm.result == TE_CMD_RESULT_PROCESS_NOT_FOUND)
       && strcmp(provider, "*")) {
      MSG("Specified process not found.");
   }
   else if (msg->save_mask_cfm.result == TE_CMD_RESULT_PRESET_OVERFLOW) {
      MSG("Too many events were saved");
   }
   else if (msg->save_mask_cfm.result != TE_CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }
   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
tri_filter(int type, const char *traceFilter, const char *event)
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, FILTER_SET_CFM};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(FilterSetReq), FILTER_SET_REQ);
   if (type == 0) {
      strncpy(msg->filterSetReq.filterExpr, traceFilter, MAX_FILTER_LEN);
      msg->filterSetReq.filterExpr[MAX_FILTER_LEN - 1] = '\0';
      strcpy(msg->filterSetReq.procName, event);
      msg->filterSetReq.type = 0;
   }
   else {
      strcpy(msg->filterSetReq.procName, event);
      msg->filterSetReq.type = 1;
   }
   itc_send(&msg, tri_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->filterSetRsp.result == TE_CMD_FILTER_UNSUPPORTED) {
      ERR("Filter syntax was unsupported");
      ret = CMD_ERROR;
   }
   else if (msg->filterSetRsp.result == TE_CMD_RESULT_PROCESS_NOT_FOUND) {
      ERR("Specified process not found");
      ret = CMD_ERROR;
   }
   else if (msg->filterSetRsp.result != TE_CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }
   itc_free(&msg);

   return ret;
}



static TeTraceHandler triHandler = {
   "TRI",
   TRI_HANDLER,
   tri_status,
   tri_enable,
   tri_disable,
   tri_preset_config,
   tri_default,
   tri_save,
   tri_filter,
};



/* ========================================================================= */
/**
 *   Register the TRI handler as a command handler to the 'te' command.
 *
 *   As this file is used by the 'ts' command as well, the registration
 *   is included only if TE_BINARY is defined.
 */
/* ========================================================================= */
#ifdef TE_BINARY
static __attribute__((constructor)) void
registerTriHandler(void)
{
   te_registerHandler(&triHandler);
}
#endif




/* ========================================================================= */
/**
 *   Returns the TRI handler.
 */
/* ========================================================================= */
TeTraceHandler*
getTeTriHandler(void)
{
   return &triHandler;
}

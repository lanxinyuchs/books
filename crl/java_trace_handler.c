/**
 *   This file handles te shell commands targeted for the Java Trace Server.
 *
 *   @file java_trace_handler.c
 *
 *
 *   Copyright (C) 2015-2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ============================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2015-01-28 Daniel Lefwerth
 *   Change  : First version.
 *
 *   Revised : 2016-21-10 Anette Schött
 *   Change  : Moved get_scope to ted_util.c.
 *
 *   Revised : 2016-07-06 Fredrik Skog
 *   Change  : Moved command timeout to trace_cmd.h.
 *
 * ============================================================================
 */

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <itc.h>
#include <sys/queue.h>

#include "ted.h"
#include "trace_cmd.h"
#include "te_internal.h"
#include "java_trace_server.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */


typedef struct {
   const char *name;
   uint32_t mask;
} Group;


union itc_msg {
   uint32_t msgNo;

   JtsChangeMaskReq changeMaskReq;
   JtsSetMaskReq setMaskReq;
   JtsSaveMaskReq saveMaskReq;
   JtsStatusReq statusReq;
   JtsStatusInd statusInd;
   JtsGenericRsp genericRsp;
};

/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

static const Group groupMasks[] =
{
   {"check",        GROUP_CHECK},
   {"error",        GROUP_ERROR},
   {"enter",        GROUP_ENTER},
   {"return",       GROUP_RETURN},
   {"info",         GROUP_INFO},
   {"trace1",       GROUP_TRACE1},
   {"trace2",       GROUP_TRACE2},
   {"trace3",       GROUP_TRACE3},
   {"trace4",       GROUP_TRACE4},
   {"trace5",       GROUP_TRACE5},
   {"trace6",       GROUP_TRACE6},
   {"trace7",       GROUP_TRACE7},
   {"trace8",       GROUP_TRACE8},
   {"trace9",       GROUP_TRACE9},
   {"state_change", GROUP_STATE_CHANGE},
   {"rec_sig",      GROUP_REC_SIG},
   {"send_sig",     GROUP_SEND_SIG},
   {"param",        GROUP_PARAM},
   {NULL,           GROUP_INVALID}
};


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
 * Print the trace group names that are set by the groupMask.
 *
 * @param groupMask - the group bitmask
 */
/* ========================================================================= */
void
jth_printTraceGroups(uint32_t groupMask)
{
   int i;
   bool isFirst = true;

   for (i = 0; groupMasks[i].name; i++) {
      if (groupMask & groupMasks[i].mask) {
         if (!isFirst)
            _MSG(" ");
         else
            isFirst = false;
         _MSG(groupMasks[i].name);
      }
   }
}


/* ========================================================================= */
/**
 * Get the group mask for the given group name.
 *
 * @param group - trace group name
 *
 * @return the corresponding bitmask, or GROUP_INVALID if group name is invalid
 */
/* ========================================================================= */
static uint32_t
getGroupMask(const char *group)
{
   size_t i;

   if (strcmp(group, "all") == 0) {
      return 0xffffffffU;
   }

   for (i = 0; groupMasks[i].name != NULL; i++) {
      if (strcmp(group, groupMasks[i].name) == 0) {
         return groupMasks[i].mask;
      }
   }
   return GROUP_INVALID;
}


/* ========================================================================= */
/**
 *   Get the Java Trace Server mail box.
 *
 *   @return 0 if successful, -1 otherwise
 *
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
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
cmd_status(const char *provider, int scope, bool isProgramSpecified)
{
   union itc_msg *msg;
   uint32_t filter[] = {2, JTS_GENERIC_RSP, JTS_STATUS_IND};
   int i;
   bool isDone = false;
   bool isFirstLine = true;
   size_t maxNameLength = 0;

   (void)isProgramSpecified;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   /* Request status from JTS */
   msg = itc_alloc(sizeof(JtsStatusReq), JTS_STATUS_REQ);
   msg->statusReq.type = scope;
   strncpy(msg->statusReq.traceItem, provider, JTS_MAX_TRACE_ITEM_LEN);
   msg->statusReq.traceItem[JTS_MAX_TRACE_ITEM_LEN - 1] = 0;
   itc_send(&msg, jts_mb, ITC_MY_MBOX);

   while (!isDone) {
      msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
      if (msg == NULL) {
         ERR("Command timeout");
         return CMD_ERROR;
      }

      switch (msg->msgNo) {
      case JTS_GENERIC_RSP:
         isDone = true;
         break;

      case JTS_STATUS_IND:
         maxNameLength = 8;
         for (i = 0; i < msg->statusInd.nTraceItems; i++) {
            if (strlen(msg->statusInd.info[i].traceItem) > maxNameLength)
               maxNameLength = strlen(msg->statusInd.info[i].traceItem);
         }
         if (isFirstLine) {
            MSG("Java Trace status:"); /* new line */
            MSG("%8s name%*senabled groups", "pid", (int)maxNameLength, "");
            isFirstLine = false;
         }

         for (i = 0; i < msg->statusInd.nTraceItems; i++) {
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
         break;
      }
      itc_free(&msg);
   }

   if (isFirstLine) {
      /* If no matching thread was found, and wildcard wasn't specified,
       * return CMD_IGNORED.
       */
      if (provider[0] != '*') {
         return CMD_IGNORED;
      }
      MSG("Java Trace status:");
      MSG("All Java trace items have default configuration");
      MSG("");
   }
   return CMD_SUCCESS;
}


/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 */
/* ========================================================================= */
static int
cmd_enable(const char *provider, int nEvents, const char *events[])
{
   union itc_msg *msg;
   uint32_t filter[] = {1, JTS_GENERIC_RSP};
   uint32_t mask = 0;
   int ret = CMD_SUCCESS;
   int i;

   for (i = 0; i < nEvents; i++) {
      uint32_t thisMask = getGroupMask(events[i]);
      if (thisMask == GROUP_INVALID) {
         /* If the provider is a wildcard, then assume that the command
          * should be handled by another handler
          */
         if (strcmp(provider, "*") == 0) {
            return CMD_SUCCESS;
         }
         ERR("Unknown trace group: %s", events[i]);
         return CMD_ERROR;
      }
      mask |= thisMask;
   }

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(JtsChangeMaskReq), JTS_CHANGE_MASK_REQ);
   msg->changeMaskReq.change = JTS_MASK_ENABLE;
   msg->changeMaskReq.mask = mask;
   msg->changeMaskReq.scope = JTS_RUNNING;
   strncpy(msg->changeMaskReq.traceItem, provider, JTS_MAX_TRACE_ITEM_LEN);
   msg->changeMaskReq.traceItem[JTS_MAX_TRACE_ITEM_LEN - 1] = 0;
   itc_send(&msg, jts_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->genericRsp.result != JTS_RESULT_OK) {
      ERR("%s", msg->genericRsp.info);
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
cmd_disable(const char *provider, int nEvents, const char *events[])
{
   union itc_msg *msg;
   uint32_t filter[] = {1, JTS_GENERIC_RSP};
   uint32_t mask = 0;
   int ret = CMD_SUCCESS;
   int i;

   for (i = 0; i < nEvents; i++) {
      uint32_t thisMask = getGroupMask(events[i]);
      if (thisMask == GROUP_INVALID) {
         /* If the provider is a wildcard, then assume that the command
          * should be handled by another handler
          */
         if (strcmp(provider, "*") == 0) {
            return CMD_SUCCESS;
         }
         ERR("Unknown trace group: %s\n", events[i]);
         return CMD_ERROR;
      }
      mask |= thisMask;
   }

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(JtsChangeMaskReq), JTS_CHANGE_MASK_REQ);
   msg->changeMaskReq.change = JTS_MASK_DISABLE;
   msg->changeMaskReq.mask = mask;
   msg->changeMaskReq.scope = JTS_RUNNING;
   strncpy(msg->changeMaskReq.traceItem, provider, JTS_MAX_TRACE_ITEM_LEN);
   msg->changeMaskReq.traceItem[JTS_MAX_TRACE_ITEM_LEN - 1] = 0;
   itc_send(&msg, jts_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->genericRsp.result != JTS_RESULT_OK) {
      ERR("%s", msg->genericRsp.info);
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
cmd_preset_config(const char *provider, int nEvents, const char **events,
                  bool savePreset)
{
   union itc_msg *msg;
   uint32_t mask = DEFAULT_MASK;
   uint32_t filter[] = {1, JTS_GENERIC_RSP};
   int ret = CMD_SUCCESS;
   int i;

   for (i = 0; i < nEvents; i++) {
      uint32_t thisMask;
      bool isEnabled = true;
      const char *group = events[i];

      if (*group == '+') {
         group++;
      }
      else if (*group == '-') {
         group++;
         isEnabled = false;
      }

      thisMask = getGroupMask(group);
      if (thisMask == GROUP_INVALID) {
         /* If the provider is a wildcard, then assume that the command
          * should be handled by another handler
          */
         if (strcmp(provider, "*") == 0) {
            return CMD_SUCCESS;
         }
         ERR("Unknown group mask: %s\n", events[i]);
         return CMD_ERROR;
      }

      if (isEnabled) {
         mask |= thisMask;
      }
      else {
         mask &= ~thisMask;
      }
   }

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(JtsSetMaskReq), JTS_SET_MASK_REQ);
   msg->setMaskReq.mask = mask;
   msg->setMaskReq.save = savePreset ? 1 : 0;
   strncpy(msg->setMaskReq.traceItem, provider, JTS_MAX_TRACE_ITEM_LEN);
   msg->setMaskReq.traceItem[JTS_MAX_TRACE_ITEM_LEN - 1] = 0;
   itc_send(&msg, jts_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->genericRsp.result != JTS_RESULT_OK) {
      ERR("%s", msg->genericRsp.info);
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
cmd_default(const char *provider, Scope scope)
{
   union itc_msg *msg;
   uint32_t filter[] = {1, JTS_GENERIC_RSP};
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(JtsChangeMaskReq), JTS_CHANGE_MASK_REQ);
   msg->changeMaskReq.change = JTS_MASK_DEFAULT;
   msg->changeMaskReq.mask = DEFAULT_MASK;
   msg->changeMaskReq.scope = get_scope(scope);
   strncpy(msg->changeMaskReq.traceItem, provider, JTS_MAX_TRACE_ITEM_LEN);
   msg->changeMaskReq.traceItem[JTS_MAX_TRACE_ITEM_LEN - 1] = 0;
   itc_send(&msg, jts_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->genericRsp.result != JTS_RESULT_OK) {
      ERR("%s", msg->genericRsp.info);
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
cmd_save(const char *provider)
{
   union itc_msg *msg;
   uint32_t filter[] = {1, JTS_GENERIC_RSP};
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(JtsSaveMaskReq), JTS_SAVE_MASK_REQ);
   strncpy(msg->saveMaskReq.traceItem, provider, JTS_MAX_TRACE_ITEM_LEN);
   msg->saveMaskReq.traceItem[JTS_MAX_TRACE_ITEM_LEN - 1] = 0;
   itc_send(&msg, jts_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->genericRsp.result != JTS_RESULT_OK) {
      ERR("%s", msg->genericRsp.info);
      ret = CMD_ERROR;
   }
   itc_free(&msg);

   return ret;
}



/* ========================================================================= */
/**
 *   See definiton of TeTraceHandler.
 *
 *   The Java handler does not implement the filter function currently.
 */
/* ========================================================================= */
static int
cmd_filter(int type, const char *traceFilter, const char *event)
{
   (void)type;
   (void)traceFilter;
   (void)event;

   return CMD_SUCCESS;
}


static TeTraceHandler javaHandler = {
   "JAVA",
   JTE_HANDLER,
   cmd_status,
   cmd_enable,
   cmd_disable,
   cmd_preset_config,
   cmd_default,
   cmd_save,
   cmd_filter
};


/* ========================================================================= */
/**
 *   Register the Java trace handler as a command handler to the 'te' command.
 *
 *   As this file is used by the 'ts' command as well, the registration
 *   is included only if TE_BINARY is defined.
 */
/* ========================================================================= */
#ifdef TE_BINARY
static __attribute__((constructor)) void
registerJavaHandler(void)
{
   te_registerHandler(&javaHandler);
}
#endif


/* ========================================================================= */
/**
 *   Returns the Java trace handler.
 */
/* ========================================================================= */
TeTraceHandler*
getJavaTraceHandler(void)
{
   return &javaHandler;
}

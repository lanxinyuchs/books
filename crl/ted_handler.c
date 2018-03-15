/**
 *   This file handles te shell commands targeted for the TED server.
 *
 *   @file ted_handler.c
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
/* ========================================================================
 *   History of development:
 *   -----------------------
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
#include <itc.h>
#include <tri_server_cmd.h>
#include "trace_cmd.h"
#include "te_internal.h"
#include "ted.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* TRI provider names */
#define TRI_PROVIDER_NAME_1 "com_ericsson_trithread"
#define TRI_PROVIDER_NAME_2 "com_ericsson_triobjif"




/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

union itc_msg
{
   uint32_t               msg_no;
   struct ted_status_req  statusReq;
   struct ted_status_rsp  statusRsp;
   struct ted_ctrl_req    ctrlReq;
   struct ted_ctrl_rsp    ctrlRsp;
};

/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

static const char *indent4 = "    ";

/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

static itc_mbox_id_t ted_mb = ITC_NO_ID;

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */

#if 0
static int
validateTraceItem(const char *traceItem)
{
   char *wptr = NULL;

   /*
    * Wild card in the middle of traceItem name is not allowed.
    */
   if (strncmp((char *)traceItem, LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) == 0)
   {
      /* Check if * is used in the provider name */
      if ((wptr = strstr(traceItem, "*")) && (*(wptr + 1) != '\0')) {
         ERR("Wildcards only allowed at the end of lttng provider name");
         return INVALID_ITEM;
      }
      return EXCLUSIVE_ITEM;
   }
   else if (strcmp(traceItem, "*") == 0) {
      return VALID_ITEM;
   }

   return INVALID_ITEM;
}
#endif

/* ========================================================================= */
/**
 *   Locate the TED server.
 *
 *   @return 0 if successful, -1 otherwise
 *
 *   @global ted_mb
 */
/* ========================================================================= */
static int
locateServer(void)
{
   ted_mb = itc_locate(TED_NAME);
   if (ted_mb == ITC_NO_ID) {
      ERR("Failed to locate ted");
      return -1;
   }

   return 0;
}


/* ========================================================================= */
/**
 * Get the length of the specified string up to the last wildcard.
 * The returned length can be used in a call to strncmp() to check
 * if the string matches another string.
 *
 * The string is assumed to have only one wildcard character.
 *
 * @param string -
 *
 * @return length of the string for usage with strncmp
 */
/* ========================================================================= */
static int
getLength(const char *string)
{
   int i;

   i = strlen(string);

   if (string[i-1] == '*')
   {
      i--;
   }
   else
   {
      /* Include null terminator */
      i++;
   }
   return i;
}

/* ========================================================================= */
/**
 *   Check if the event name is a filtered event.
 *
 *   @param name - the event name (provider[:event])
 *
 *   @return true if the event should be filtered, false otherwise
 */
/* ========================================================================= */
static bool
isFilteredEvent(const char *name)
{
   if (strstr(name, TRI_PROVIDER_NAME_1)) {
      return true;
   }
   else if (strstr(name, TRI_PROVIDER_NAME_2)) {
      return true;
   }
   return false;
}

/* ========================================================================= */
/**
 * Prints the recieved enabled event data in msg to stdout.
 *
 * @param msg        Received status message to print.
 * @param isFirstLine True if first first line is printed, otherwise false.
 *
 * @return True if all data is printed, false data to print remains.
 */
/* ========================================================================= */
static bool
handle_status_enabled_scope(union itc_msg *msg,
                            const char *provider,
                            int len,
                            bool isProgramSpecified,
                            bool *isFirstLine)
{
   bool isDone = false;
   char *indexPtr;
   int i;

   if (*isFirstLine && msg->statusRsp.data_size.no_of_events) {
      if (isProgramSpecified) {
         MSG("Registered LTTng user events for '%s':", provider);
         /* FIXME: msg->statusRsp.data_size.no_of_events can never be 0 here,
          * see above */
         if (msg->statusRsp.data_size.no_of_events == 0) {
            MSG("%sNo registered events", indent4);
         }
      }
      else {
         MSG("");
         MSG("Enabled LTTng events:");
      }
      *isFirstLine = false;
   }

   for (i = 0; i < msg->statusRsp.data_size.no_of_events; i++)
   {
      /* Skip filtered events */
      if (isFilteredEvent(msg->statusRsp.data_type.event_list[i].name))
         continue;

      /* Check event names having the format <provider> */
      indexPtr = strchr(msg->statusRsp.data_type.event_list[i].name, ':');
      if (indexPtr == NULL) {
         if (!strncmp(provider, msg->statusRsp.data_type.event_list[i].name, len))
         {
            ted_printEvent(&msg->statusRsp.data_type.event_list[i]);
         }
         continue;
      }

      /* Check event names having the format <provider>:<event> */
      if (!strncmp(provider, msg->statusRsp.data_type.event_list[i].name, len) ||
          isProgramSpecified)
      {
         ted_printEvent(&msg->statusRsp.data_type.event_list[i]);
      }
   }
   if (msg->statusRsp.last) {
      isDone = true;
   }
   return isDone;
}

/* ========================================================================= */
/**
 * Prints the recieved preset/restart data in msg to stdout.
 *
 * @param msg         Received status message to print.
 * @param isFirstLine True if first first line is printed, otherwise false.
 *
 * @return True if all data is printed, false data to print remains.
 */
/* ========================================================================= */
static bool
handle_status_restart_preset_scope(union itc_msg *msg,
                                   bool scope,
                                   bool *isFirstLine)
{
   bool isDone = false;
   int maxProviderLen = 0;
   int i;

   maxProviderLen = 14;
   for (i = 0; i < msg->statusRsp.data_size.no_of_events; i++) {
      if (strlen(msg->statusRsp.data_type.event_list[i].name) > maxProviderLen)
      {
         maxProviderLen = strlen(msg->statusRsp.data_type.event_list[i].name);
      }
   }
   if (*isFirstLine && msg->statusRsp.data_size.no_of_events) {
      MSG(" ");
      if (scope == RESTART) {
         MSG("Saved LTTng events List:");
      }
      else {
         MSG("Preset LTTng events List:");
      }
      MSG("provider:event%*s enabled/disabled", maxProviderLen - 10, " ");
      *isFirstLine = false;
   }
   for (i = 0; i < msg->statusRsp.data_size.no_of_events; i++) {
      if (isFilteredEvent(msg->statusRsp.data_type.event_list[i].name))
         continue;
      _MSG("%s", msg->statusRsp.data_type.event_list[i].name);
      _MSG("%*s",
           (int)(maxProviderLen + 4 - strlen(msg->statusRsp.data_type.event_list[i].name)),
           " ");
      _MSG(" %s", msg->statusRsp.data_type.event_list[i].enabled ? "enabled":"disabled");
      MSG(""); /* new line */
   }
   if (msg->statusRsp.last) {
      isDone = true;
   }
   return isDone;
}

/* ========================================================================= */
/**
 * Prints the recieved user ids in msg to stdout.
 *
 * @param msg User identities to print.
 * @param isFirstLine True if first first line is printed, otherwise false.
 *
 * @return True if all data is printed, false data to print remains.
 */
/* ========================================================================= */
static bool
handle_status_uid_scope(union itc_msg *msg, bool *isFirstLine)
{
  int i;
  char *data_pos = &(msg->statusRsp.data_type.elem[0]);

  if (msg->statusRsp.data_size.no_of_elems) {
    MSG("Available UIDs used for tracing:");

    for (i = 0; i < msg->statusRsp.data_size.no_of_elems; i++) {
      _MSG("  %s", data_pos);
      data_pos += strlen(data_pos) + 1;
    }
  }
  else {
    _MSG("\tNo UIDs available");
  }

  MSG("");

  *isFirstLine = false;
  return true;
}

/* ========================================================================= */
/**
 * See definition of TeTraceHandler.
 */
/* ========================================================================= */
static int
ted_status(const char *provider, int scope, bool isProgramSpecified)
{
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_STATUS_RSP};
   bool isDone = false;
   bool isFirstLine = true;
   int len;

   len = getLength(provider);

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   if (scope == RESTART) {
      provider = "-restart";
   }
   else if (scope == PRESET) {
      provider = "-preset";
   }
   else if (scope == UID) {
      provider = "-uid";
   }

   /* Request status from TED */
   msg = itc_alloc(sizeof(struct ted_status_req) + strlen(provider) + 1,
                   TED_STATUS_REQ);
   if (isProgramSpecified) {
      msg->statusReq.type = TE_STATUS_LIST;
   }
   else {
      msg->statusReq.type = TE_STATUS;
   }
   /* FIXME: else is missing? */
   msg->statusReq.handler = CMD_TE_HANDLER;
   strcpy(msg->statusReq.data, provider);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   while (!isDone) {
      msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
      if (msg == NULL) {
         ERR("Command timeout");
         return CMD_ERROR;
      }

      if (!msg->statusRsp.success) {
         itc_free(&msg);
         return CMD_FAILED;
      }

      if ((scope == RUNNING) || (scope == LIST)) {

         isDone = handle_status_enabled_scope(msg,
                                              provider, len,
                                              isProgramSpecified,
                                              &isFirstLine);
      }
      else if ((scope == RESTART) || (scope == PRESET)) {

         isDone = handle_status_restart_preset_scope(msg, scope, &isFirstLine);
      }
      else {

         isDone = handle_status_uid_scope(msg, &isFirstLine);
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
 * See definition of TeTraceHandler.
 */
/* ========================================================================= */
static int
ted_enable(const char *provider, int nEvents, const char *events[])
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t      filter[] = {1, TED_CTRL_RSP};
   int           size = 0;
   char          *eventFormat = NULL;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   /* Group the lttng events to be sent to TED */
   size = ted_groupLttngEvents(nEvents, events, &eventFormat);
   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size, TED_CTRL_REQ);
   msg->ctrlReq.type = TE_ENABLE;
   msg->ctrlReq.size = nEvents + 1;
   msg->ctrlReq.handler = CMD_TE_HANDLER;
   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   snprintf(msg->ctrlReq.data, size + 1, "%s", eventFormat);
   free(eventFormat);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Failed to enable requested event(%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_EVENT_NOT_FOUND) {
      ret = CMD_IGNORED;
   }
   else {
      if (msg->ctrlRsp.result != CMD_RESULT_OK) {
         ret = CMD_ERROR;
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
ted_disable(const char *provider, int nEvents, const char *events[])
{
   int           ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t      filter[] = {1, TED_CTRL_RSP};
   int           size = 0;
   char          *eventFormat = NULL;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   /*
    * Group the lttng events to be sent to TED.
    */
   size = ted_groupLttngEvents(nEvents, events, &eventFormat);
   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size, TED_CTRL_REQ);
   msg->ctrlReq.type = TE_DISABLE;
   msg->ctrlReq.size = nEvents + 1;
   msg->ctrlReq.handler = CMD_TE_HANDLER;
   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   snprintf(msg->ctrlReq.data, size + 1, "%s", eventFormat);
   free(eventFormat);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Failed to enable requested event(%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_EVENT_NOT_FOUND) {
      ret = CMD_IGNORED;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
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
ted_preset_config(const char *provider, int nEvents, const char **events,
                  bool savePreset)
{
   int           ret = 0;
   int           size = 0;
   union itc_msg *msg;
   uint32_t      filter[] = {1, TED_CTRL_RSP};
   char          *eventFormat = NULL;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   size = ted_groupLttngEvents(nEvents, events, &eventFormat);
   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size, TED_CTRL_REQ);
   if (savePreset) {
      msg->ctrlReq.type = TE_CONFIG;
   }
   else {
      msg->ctrlReq.type = TE_PRESET;
   }
   msg->ctrlReq.size = nEvents + 1;
   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   snprintf(msg->ctrlReq.data, size + 1, "%s", eventFormat);
   free(eventFormat);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Failed to receive response for preset request");
      return CMD_ERROR;
   }
   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ret &= CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_PRESET_OVERFLOW) {
      ERR("Too many trace events are saved.");
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }
   else {
      ret = 0;
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
ted_default(const char *provider, Scope scope)
{
   int ret = 0;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }


   msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
   msg->ctrlReq.type = TE_DEFAULT;
   msg->ctrlReq.handler = CMD_TE_HANDLER;
   msg->ctrlReq.scope = get_scope(scope);

   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
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
ted_save(const char *provider)
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req) + strlen(provider) + 1,
                   TED_CTRL_REQ);
   msg->ctrlReq.type = TE_SAVE;
   msg->ctrlReq.handler = CMD_TE_HANDLER;
   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_PRESET_OVERFLOW) {
      ERR("Too many trace events are saved.");
   }
   else if (msg->ctrlRsp.result == CMD_EVENT_NOT_FOUND) {
      ERR("Provider not found");
   }
   else if (msg->ctrlRsp.result == CMD_SESSION_NOT_FOUND) {
      MSG("No session available to save");
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
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
ted_filter(int type, const char *traceFilter, const char *event)
{
   int ret = 0;
   int size;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   size = strlen(traceFilter) + strlen(event) + 2;
   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size, TED_CTRL_REQ);
   msg->ctrlReq.handler = CMD_TE_HANDLER;
   msg->ctrlReq.type = type == 0 ? TE_FILTER_SET : TE_FILTER_RESET;
   if (type == 0) {
      strcpy(msg->ctrlReq.data, traceFilter);
      strcpy(&msg->ctrlReq.data[strlen(traceFilter) + 2], event);
      msg->ctrlReq.offset = strlen(traceFilter) + 2;
   }
   else {
      strcpy(msg->ctrlReq.data, event);
   }
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }
   itc_free(&msg);
   return ret;
}


/** ======================================================================== */
/**
 *  Read the log using babeltrace.
 *
 *  @param msg  - Response signal from ted
 *  @param info - Read arguments and options from command
 *
 *  @return command result
 */
/** ======================================================================== */
static int
read_trace(union itc_msg *msg, TedInfo *info)
{
   char *cmd;
   char *path = NULL;
   const char *fc = "";
   const char *level = "";
   const char *brief = "";
   int r, options;

   options = msg->ctrlRsp.extra | info->options;

   if (options & TED_LOG_FCORRELATE)
      fc = "--clock-force-correlate";
   if (options & TED_LOG_LEVEL)
      level = "-f loglevel";
   if (options & TED_LOG_BRIEF)
      brief = "--no-delta -n none -f trace:procname";
   if (options & TED_LOG_UID) {
      /*
       * Only read traces for the specified user id when uid is set,
       * i.e another path in the snapshot directory is used.
       */
      if (asprintf(&path, "%s/*/%s/%d",
                   msg->ctrlRsp.data, TED_LOG_SUB_DIR, info->uid) < 0)
         return CMD_ERROR;
   }
   else {
      if (asprintf(&path, "%s", msg->ctrlRsp.data) < 0)
         return CMD_ERROR;
   }

   if (asprintf(&cmd, "babeltrace --clock-date %s %s %s %s",
                fc, level, brief, path) < 0)
   {
      return CMD_ERROR;
   }
   r = run_shell_cmd(cmd);
   free(path);
   free(cmd);
   return r;
}


/** ======================================================================== */
/**
 *  Read or clear the log.
 *
 *  @param info    - Information structure between te command and ted
 *
 *  @return command result
 */
/** ======================================================================== */
int
ted_log(TedInfo *info)
{
   int ret = 0;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
   msg->ctrlReq.type = info->type == 0 ? TE_LOG_READ : TE_LOG_CLEAR;
   msg->ctrlReq.uid = info->uid ;
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   switch (msg->ctrlRsp.type) {
   case TE_LOG_READ:
      if (msg->ctrlRsp.result) {
         ret = read_trace(msg, info);
      } else {
         ERR("Failed to read log");
         ret = CMD_ERROR;
      }
      break;
   case TE_LOG_CLEAR:
      if (!msg->ctrlRsp.result) {
         ERR("Failed to clear log");
         ret = CMD_ERROR;
      }
      break;
   default:
      ERR("Unknown command type");
      ret = CMD_ERROR;
   }
   return ret;
}



/* ========================================================================= */
/**
 * Send the session request to the TED server.
 *
 * @return command result
 */
/* ========================================================================= */
int
ted_restart(void)
{
   uint32_t filter[] = {1, TED_CTRL_RSP};
   union itc_msg *msg;
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

        msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
        msg->ctrlReq.type = TE_RESTART;
        msg->ctrlReq.handler = CMD_TE_HANDLER;
        itc_send(&msg, ted_mb, ITC_MY_MBOX);

        msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
        if (msg == NULL) {
                ERR("Command timeout.");
                return CMD_ERROR;
        }

        if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
                ERR("Command failed(%s)", msg->ctrlRsp.data);
                ret = CMD_ERROR;
        }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

        itc_free(&msg);

   return ret;
}


static TeTraceHandler tedHandler = {
   "TED",
   TED_HANDLER,
   ted_status,
   ted_enable,
   ted_disable,
   ted_preset_config,
   ted_default,
   ted_save,
   ted_filter,
};



/* ========================================================================= */
/**
 *   Register the TED handler as a command handler to the 'te' command.
 */
/* ========================================================================= */
#ifdef TE_BINARY
static __attribute__((constructor)) void
registerTedHandler(void)
{
   te_registerHandler(&tedHandler);
}
#endif



/* ========================================================================= */
/**
 *   Returns the TED handler which is the mandatory command handler.
 */
/* ========================================================================= */
TeTraceHandler*
getTedHandler(void)
{
   return &tedHandler;
}

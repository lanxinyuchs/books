/**
 *   This file handles te shell commands targeted for the TED server.
 *
 *   @file ted_util.c
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
 *   Revised : 2016-04-20 Fredrik Skog
 *   Change  : Moved function run_shell_cmd() to this file.
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

#include "te_internal.h"
#include "ted.h"
#include "trace_cmd.h" /* CMD_ERROR */


/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */


/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */


/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

static const char *indent4 = "    ";

/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */

/* ===================================================================== */
/**
 *   Returns the string representation of the provided loglevel.
 *
 *   @param      value takes any integer defined in LTTng log levels
 *
 *   @return     Character pointer corresponding to Log value
 *
 */
/* ===================================================================== */
static const char *
loglevelToStr(int value)
{
   switch (value) {
   case -1:
      return "";
   case LTTNG_LOGLEVEL_EMERG:
      return "TRACE_EMERG";
   case LTTNG_LOGLEVEL_ALERT:
      return "TRACE_ALERT";
   case LTTNG_LOGLEVEL_CRIT:
      return "TRACE_CRIT";
   case LTTNG_LOGLEVEL_ERR:
      return "TRACE_ERR";
   case LTTNG_LOGLEVEL_WARNING:
      return "TRACE_WARNING";
   case LTTNG_LOGLEVEL_NOTICE:
      return "TRACE_NOTICE";
   case LTTNG_LOGLEVEL_INFO:
      return "TRACE_INFO";
   case LTTNG_LOGLEVEL_DEBUG_SYSTEM:
      return "TRACE_DEBUG_SYSTEM";
   case LTTNG_LOGLEVEL_DEBUG_PROGRAM:
      return "TRACE_DEBUG_PROGRAM";
   case LTTNG_LOGLEVEL_DEBUG_PROCESS:
      return "TRACE_DEBUG_PROCESS";
   case LTTNG_LOGLEVEL_DEBUG_MODULE:
      return "TRACE_DEBUG_MODULE";
   case LTTNG_LOGLEVEL_DEBUG_UNIT:
      return "TRACE_DEBUG_UNIT";
   case LTTNG_LOGLEVEL_DEBUG_FUNCTION:
      return "TRACE_DEBUG_FUNCTION";
   case LTTNG_LOGLEVEL_DEBUG_LINE:
      return "TRACE_DEBUG_LINE";
   case LTTNG_LOGLEVEL_DEBUG:
      return "TRACE_DEBUG";
   default:
      return "<<UNKNOWN>>";
   }
}


/* ===================================================================== */
/**
 *   Support functionality to check if event was enabled with filter
 *
 *
 *   @param      value takes any interger (if filter enabled value = 1)
 *
 *   @return     Character pointer corresponding to value
 *
 */
/* ===================================================================== */
static const char *
filterString(int value)
{
   switch (value) {
   case 1:        return " [with filter]";
   default:       return "";
   }
}


/** ======================================================================== */
/**
 *  Print status of an LTTng event.
 *
 *  @param event - event which holds the data of a LTTng event
 */
/* ========================================================================= */
void
ted_printEvent(const struct lttng_event *event)
{
   if (event->enabled) {
      if (event->loglevel != -1) {
         MSG("%s%s (loglevel: %s) %s",
             indent4,
             event->name,
             loglevelToStr(event->loglevel),
             filterString(event->filter));
      }
      else {
         MSG("%s%s %s",
             indent4,
             event->name,
             filterString(event->filter));
      }
   }
}


/** ==================================================================== */
/**
 *  Creates lttng events string from command arguments
 *      ex: event1 event2 ....eventN
 *
 *  @param  nEvents - Number of events
 *          events - Array of event names
 *          eventFormat - output string contains group of lttng events.
 *
 *  @return size         length of the created lttng format string
 *
 *  Caller of the function need to free the 'eventFormat' with
 *  free() call.
 */
/* ===================================================================== */
int
ted_groupLttngEvents(int nEvents, const char *events[], char **eventFormat)
{
   int len = 0;
   int i;

   for (i = 0; i < nEvents; i++) {
      /*
       * lttng format 'event01 event02 event03 ... eventN'
       */
      len += strlen(events[i]) + 1;
   }
   *eventFormat = (char *)malloc(len);
   for (i = 0; i < nEvents; i ++) {
      if (i != 0) {
         strcat(*eventFormat, " ");
         strcat(*eventFormat, (char *)events[i]);
      }
      else {
         strcpy(*eventFormat, (char *)events[i]);
      }
   }

   /*
    * Returning length of the string data excluding
    * null termination.
    */
   return (len - 1);
}

/* ========================================================================= */
/**
 *   This function maps the definitions of default scope with corresponding
 *   values defined in its handler.
 *   Ex: RUNNING->TED_RUNNING
 *
 *   @return corresponding mapping if successful, TED_RUNNING otherwise
 *
 */
/* ========================================================================= */
uint32_t
get_scope(Scope var)
{
   switch(var) {
      case RUNNING:
           return (uint32_t)(TED_RUNNING);
      case RESTART:
           return (uint32_t)(TED_RESTART);
      case PRESET:
           return (uint32_t)(TED_PRESET);
      case UID:
           return (uint32_t)(TED_UID);
      case LIST:
           return (uint32_t)(TED_LIST);
   }
   return (uint32_t)(TED_RUNNING);
}

/** ==================================================================== */
/**
 *  Run shell command.
 *
 *  @param  cmd - Command to run, including options.
 *
 *  @return 0 for success or CMD_ERROR for failure.
 *
 */
/* ===================================================================== */
int
run_shell_cmd(const char *cmd)
{
   char buf[256];
   FILE *ptr;
   int status;

   ptr = popen(cmd, "r");
   if ( ptr == NULL) {
      ERR("Failed to run '%s'", cmd);
      return CMD_ERROR;
   }
   while (fgets(buf, 256, ptr) != NULL) {
      (void) fprintf(stdout, "%s", buf);
   }
   status = pclose(ptr);
   if (!WIFEXITED(status))
      return CMD_ERROR;

   status = WEXITSTATUS(status);
   return (status == 0 ? status : CMD_ERROR);
}

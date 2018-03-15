/**
 *  This file interprets the shell command 'ts' and forwards the requests to
 *  the concerned shell command handler.
 *
 *   @file ts.c
 *
 *   An trace enable request on a session will look like:
 *     "ts enable mysession ev1 ev2 tp1"
 *     Here ev1, ev2 are called trace events.
 *          tp1 is called as trace point provider.
 *          mysession is the session on which above trace points
 *                    needs to be enabled.
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

/* ============================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2014-12-05 Ranganath & Shreyas
 *   Change  : First revision. Command suppport for "ts"
 *
 *   Revised : 2015-01-23 Ranganath
 *   Change  : Improved default and status commands
 *
 *   Revised : 2015-01-27 Nils Carlson <nils.carlson@ericsson.com>
 *   Change  : Add ts restart cmd.
 *
 *   Revised : 2015-02-05 Daniel Lefwerth
 *   Change  : Refactored ts command.
 *
 *   Revised : 2015-07-23 Ranganadh
 *   Change  : Added checking of session name /session ID before executing
 *             a command in case of ts.
 *
 *   Revised : 2015-09-29 Niklas Damberg
 *   Change  : Add optional parameters to the ts ip command.
 *             The parameters that can be used to alter performance are:
 *             Sub buffer size - Size of trace buffers.
 *             Number of sub buffers - Number of trace buffers
 *             Switch timer interval - Time interval for switching
 *             TR: HT71981
 *
 *   Revised : 2015-11-04 Niklas Damberg
 *   Change  : WP4903: Add subcommand 'correlate enable|disable' to
 *             the ts command. When 'ts restart' is executed after ntp sync
 *             this setting decides if a restart is performed, default it is
 *             disabled. A new option '--if-correlate-enabled is added to
 *             'ts restart' for this.
 *
 *   Revised : 2016-04-18 Anette Schött
 *   Change  : Update of command description to clarify syntax for IPv4 and
 *             IPv6 addresses for option ip.
 *
 *   Revised : 2016-07-06 Fredrik Skog
 *   Change  : Removed RECEIVE_TMO define which was not used.

 * ============================================================================
 */

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <itc.h>

#include "trace_cmd.h"
#include "ts_internal.h"
#include "ted.h"
#include <tri_server_cmd.h>

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

#define TS_MBOX_NAME "ts_cmd_mb"

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

TAILQ_HEAD(TsTraceHandlerList, TsTraceHandler);
typedef struct TsTraceHandlerList TsTraceHandlerList;


/* This structure represents a sub-command handler.
 *
 * A sub-command handler is called with the shell command ('ts') and
 * the sub-command arguments removed.
 */
struct cmd_struct {
   const char *name;
   int (*func)(int argc, const char **argv);
};

/* ============================================================================
 *   FUNCTION PROTOTYPES
 * ============================================================================
 */

/* The sub-command handlers */
static int ts_session_create(int argc, const char **argv);
static int ts_session_delete(int argc, const char **argv);
static int ts_status(int argc,const char **argv);
static int ts_enable(int argc, const char **argv);
static int ts_disable(int argc, const char **argv);
static int ts_default(int argc, const char **argv);
static int ts_save(int argc, const char **argv);
static int ts_filter(int argc, const char **argv);
static int ts_restart(int argc, const char **argv);
static int ts_echo(int argc, const char **argv);
static int ts_correlate(int argc, const char **argv);

/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

static const struct cmd_struct commands[] = {
   {"ip",        ts_session_create},
   {"destroy",   ts_session_delete},
   {"status",    ts_status},
   {"s",         ts_status},
   {"enable",    ts_enable},
   {"e",         ts_enable},
   {"disable",   ts_disable},
   {"d",         ts_disable},
   {"filter",    ts_filter},
   {"save",      ts_save},
   {"default",   ts_default},
   {"restart",   ts_restart},
   {"echo",      ts_echo},
   {"correlate", ts_correlate},
   {NULL, NULL}
};

/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

static itc_mbox_id_t my_mb = ITC_NO_ID;
static TsTraceHandlerList handlers;

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */

/* ========================================================================= */
/*
 * Prints the 'ts' command usage help.
 */
/* ========================================================================= */
static void
usage(void)
{
   fprintf(
      stderr,
      "Usage: ts <cmd> <param> ...\n"
      "Commands <cmd>:\n"
      "    ip {<ipv4>|<ipv6 enclosed by []>|<hostname>[:PORT1[:PORT2]} [<sessionName>]\n"
      "        [-s <subbuffer size>] [-n <no of subbuffers>]\n"
      "        [-i <switch interval>] [-f <data flush interval>]\n"
      "        Create an LTTng trace streaming session.\n"
      "        This will use the default network transport layer which is\n"
      "        TCP for both control (PORT1) data port (PORT2).\n"
      "        The default ports are 5342 and 5343, respectively.\n"
      "        If not sub buffer size, number of subbuffers,\n"
      "        switch interval or data flush interval is not set\n"
      "        the default values are used\n"
      "\n"
      "    enable {<sessionName>|<sessionId>} {<event>[<event2>, ..] <provider>}\n"
      "        Enable LTTng events on the specified session.\n"
      "\n"
      "    disable {<sessionName>|<sessionId>} {<event>[<event2>, ..] <provider>}\n"
      "        Disable LTTng events on the specified session.\n"
      "\n"
      "    status [<sessionName>|<sessionId>]\n"
      "        Print the streaming sessions details.\n"
      "\n"
      "    default {<sessionId>|<sessionName>|-restart} [<provider>]\n"
      "        Reset all enabled tracing events to default log level for \n"
      "        the given session.\n"
      "        '-restart' removes all saved session details.\n"
      "\n"
      "    destroy [<sessionId>|<sessionName>]\n"
      "        Destroy LTTng trace streaming session(s).\n"
      "\n"
      "    filter set {<sessionName>|<sessionId>} <filter> <event/provider>\n"
      "        Activate a filter on a LTTng event.\n"
      "        Filter expression examples:\n"
      "        LTTng format:\n"
      "            'intfield > 500 && intfield < 503'\n"
      "            '(strfield == \"test\" || intfield != 10) && intfield > 33'\n"
      "\n"
      "    filter reset {<sessionName>|<sessionId>} <event>\n"
      "        Deactivate a filter on the specified session.\n"
      "\n"
      "    save {<sessionName>|<sessionId>|'*'}\n"
      "        Save currently enabled trace groups/LTTng events.\n"
      "        Specifying '*' instead of session name will also save currently \n"
      "        enabled LTTng events for all sessions created through ts commands.\n"
      "\n"
      "    restart\n"
      "        Restart all ts traces, used for time updated.\n"
      "\n"
      "    echo {<sessionName>|<sessionId>} <trace string>\n"
      "        Write a string to the given session.\n"
      "\n"
      "    correlate enable|disable\n"
      "        Enable/disable restart of trace session, to correlate time stamps,\n"
      "        after ntp sync.\n"
      "\n"
      "    Note: 1) To deal with bash extensions encapsulate wildcard into \"\" or ''\n"
      "          2) The TRI events that are enabled/disabled/saved using ts\n"
      "             will stay even after the session is destroyed\n"
      "\n"
      "Command Examples:\n"
      "        Create an LTTng trace streaming session.\n"
      "        ts ip 127.0.0.1\n"
      "        ts ip 127.0.0.1 mysession\n"
      "        ts ip [0:0:0:0.0:0:0:1] mysession\n"
      "        ts ip [0:0:0:0.0:0:0:1]:3229:3230 mysession\n"
      "        ts ip myhost.com:3229:3230\n"
      "        ts ip hostname \n"
      "    Enable LTTng event(s) using session id:\n"
      "        ts enable 2 myevent01 myevent02 com_ericsson_myprov\n"
      "        ts enable 2 * com_ericsson_myprov\n"
      "        ts e 2 myevent01 com_*\n"
      "    Enable LTTng event(s) using session name:\n"
      "        ts enable myses0_423 myevent01 com_ericsson_*\n"
      "        ts enable myses0_423 myevent01 com_ericsson_myprov\n"
      "        ts enable myses0_423 myevent01 com_*\n"
      "        ts enable myses0_423 myevent01 *\n"
      "    Disable LTTng event(s) using seesion id.:\n"
      "        ts disable 3 myevent01 myevent02 com_ericsson_myprov\n"
      "        ts disable 5 * com_ericsson_myprov\n"
      "        ts disable 7 myevent01 com_*\n"
      "    Disable LTTng event(s) using seesion name.:\n"
      "        ts disable myses0_423 myevent01 com_ericsson_*\n"
      "        ts disable myses0_423 myevent01 com_ericsson_myprov\n"
      "        ts disable myses0_423 myevent01 com_*\n"
      "        ts d myses0_423 myevent01 *\n"
      "    Status of LTTng trace streaming sessions and enabled events on the sessions:\n"
      "        ts status myses0_423\n"
      "        ts status 1\n"
      "    Default LTTng events(s) on the specified session using session name:\n"
      "        ts default myses0_423\n"
      "        ts default myses0_423 com_ericsson*\n"
      "    Default LTTng events(s) on the specified session using session id:\n"
      "        ts default 1\n"
      "        ts default 1 com_ericsson_*\n"
      "    Activate a filter:\n"
      "        ts filter set myses0_423 'intf < 20 && intf > 50' com_ericsson_myprov:myevent\n"
      "        ts filter set 3 'LEN<30 AND ([4]=10 OR [14]>=$FE)' myevent\n"
      "    Deactivate a filter:\n"
      "        ts filter reset myses0_423 com_ericsson_myprov:myevent\n"
      "        ts filter reset 3 com_ericsson\n"
      "    Destroy  LTTng streaming session(s)\n"
      "        ts destroy myses2_354  \n"
      "        ts destroy 3    \n"
      "    Save enabled events on session(s)\n"
      "        ts save 1\n"
      "        ts save mysession\n"
      "        ts save *\n"
      "        ts save myses*\n"
      "    Write an event into on session(s)\n"
      "        ts echo 1 'Test starts here'\n"
      "\n");
}

/* ========================================================================= */
/**
 *   Get the type of handlers that are concerned about the specified
 *   provider.
 *
 *   This function only analyses the trace provider name.
 *
 *   @param provider -
 *
 *   @return
 */
/* ========================================================================= */
static HandlerType
getHandlerType(const char *provider)
{
   char *wptr = NULL;
   HandlerType handlerType = NO_HANDLER;

   /*
    * Command:
    * a) 'te enable event01 event02... eventN com_*'
    *    A traceItem starting with 'com_' is considered as a request for LTTng.
    *    HandlerType will be TED_HANDLER.
    *
    * b) 'te enable trace1 "*"'
    *     A wild card "*" is considered as common request for all
    *     trace handlers. HandlerType will contain all handlers.
    *
    * c) 'te enable trace2 se.ericsson:*'
    *    A traceItem containing a ':' is considered as a request for Java Trace.
    *    HandlerType will be JTE_HANDLER.
    *
    * d) 'te enable trace1 ose_ns'
    *    Other traceItem names is considered as a request for TRI.
    *    HandlerType will be TRI_HANDLER.
    */

   if (strncmp(provider, LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) == 0)
   {
      /* Wild card in the middle of traceItem name is not allowed */
      if ((wptr = strchr(provider, '*')) && (*(wptr + 1) != '\0'))
      {
         ERR("Wildcards only allowed at the end of LTTng provider name");
         return NO_HANDLER;
      }
      handlerType = TED_HANDLER;
   }
   else if (strcmp(provider, "*") == 0) {
      handlerType |= TED_HANDLER | TRI_HANDLER | JTE_HANDLER;
   }
   else if (strchr(provider, ':') != NULL) {
      handlerType = JTE_HANDLER;
   }
   else {
      handlerType = TRI_HANDLER;
   }
   return handlerType;
}

/* ========================================================================= */
/**
 *  Validate that the events are specified correctly.
 *
 *  @param argc - argument count, including provider
 *  @param argv - argument vector, including events and provider
 *
 *  @return command result
 */
/* ========================================================================= */
static int
validateEvents(int argc, const char *argv[])
{
   const char *wild = NULL;
   bool isWildcardInProvider = strchr(argv[argc-1], '*') != NULL;
   int i;

   for (i = 0; i < argc - 1; i++)
   {
      /* Check for wild card in middle of event name */
      if ((wild = strchr(argv[i], '*')) && (wild[1] != '\0')) {
         ERR("Wildcards only allowed at the end of event name");
         return CMD_ERROR;
      }

      if (isWildcardInProvider) {
         if (strcmp(argv[i], "all") == 0) {
            ERR("* in process name used with 'all' is not allowed");
            return CMD_ERROR;
         }

         if (wild != NULL) {
            ERR("Wildcards for both the event and provider is not allowed");
            return CMD_ERROR;
         }
      }
   }
   return CMD_SUCCESS;
}

/* ========================================================================= */
/**
 *  Register a command handler to the 'ts' command.
 *
 *  @param handler - the handler to register
 *
 *  @globals handlers
 */
/* ========================================================================= */
void
ts_registerHandler(TsTraceHandler *handler)
{
   static bool isListInitialized = false;

   if (!isListInitialized) {
      TAILQ_INIT(&handlers);
      isListInitialized = true;
   }
   TAILQ_INSERT_TAIL(&handlers, handler, entry);
}


/* ========================================================================= */
/**
 *  Initializes the te shell command by installing the trace handlers
 *  and initializing ITC.
 *
 *  @return true if succesful
 *
 *  @globals my_mb
 */
/* ========================================================================= */
static bool
ts_init(void)
{
   itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

   if (my_mb == ITC_NO_ID) {
      my_mb = itc_create_mailbox(TS_MBOX_NAME, 0);
   }
   if (my_mb == ITC_NO_ID) {
      return false;
   }
   return true;

}

/* ========================================================================= */
/**
 *   Fucntionality to destroy mail box created for "ts"
 *
 *   @globals my_mb
 */
/* ========================================================================= */
static void
ts_destroy(void)
{
   if (my_mb != ITC_NO_ID)
      itc_delete_mailbox(my_mb);
   itc_exit();
}


/* ========================================================================= */
/**
 *   Initialize a session object.
 *
 *   @param session - session to initialize
 *   @param input - string representation of session
 */
/* ========================================================================= */
static void
session_init(Session *session, const char *input)
{
   char *endptr = NULL;

   /* Check if given argument was a session ID or a session name */
   session->id = (uint32_t)strtoul(input, &endptr, 0);

   if (*endptr) {
      strncpy(session->name, input, sizeof(session->name));
      session->name[sizeof(session->name) - 1] = '\0';
      session->id = 0;
   }
   else {
      session->name[0] = '\0';
   }
}


/* ========================================================================= */
/**
 *   Starts the live streaming session towards remote host.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 *
 *   Syntax: ts ip {<ip address>|<hostname>[:<port 1>[:<port 2>]\
 *           [-s <subbuffer size>] [-n <no of subbuffers>] \
 *           [-i <switch interval>] [-f <data flush interval>]
 */
/* ========================================================================= */
static int
ts_session_create(int argc, const char **argv)
{
   Session session;
   int c, no_arg;
   char *ip;
   const char *session_name;
   uint64_t subbuf_size = 0, no_of_subbuf = 0;
   unsigned int switch_interval = 0, flush_interval = 0;

   while ((c = getopt(argc, (char *const *)argv, "s:n:i:f:")) != -1) {
      switch (c) {
      case 's':
	      if ((subbuf_size = strtoull(optarg, NULL, 10)) == 0) {
		      MSG("Option -s not valid argument\n");
		      return CMD_ERROR;
	      }
	      break;
      case 'n':
	      if ((no_of_subbuf = strtoull(optarg, NULL, 10)) == 0) {
		      MSG("Option -n not valid argument\n");
		      return CMD_ERROR;
	      }
	      /* Check if no of subbur is power of 2 */
	      if (((no_of_subbuf-1)&no_of_subbuf) != 0) {
		      MSG("Option -n, argument not power of 2\n");
		      return CMD_ERROR;
	      }
	      break;
      case 'i':
	      if ((switch_interval = (unsigned int)strtoul(optarg, NULL, 10)) == 0) {
		      MSG("Option -i not valid argument\n");
		      return CMD_ERROR;
	      }
	      break;
      case 'f':
	      if ((flush_interval = (unsigned int)strtoul(optarg, NULL, 10)) == 0) {
		      MSG("Option -f not valid argument\n");
		      return CMD_ERROR;
	      }
	      break;
      case '?':
	 if (optopt == 'c') {
	    MSG("Option -%c requires an argument.\n", optopt);
	 } else if (isprint (optopt)) {
	    MSG("Unknown option `-%c'.\n", optopt);
	 } else {
	    MSG("Unknown option character `\\x%x'.\n",
		optopt);
	    return CMD_ERROR;
	 }
      default:
	 MSG("Error while parsing command\n");
	 return CMD_ERROR;
      }
   }
   ip = (char*)argv[0];
   no_arg = argc-optind;
   if ( no_arg > 1 ) {
      MSG("Invalid number of arguments.");
      return CMD_ERROR;
   }
   if ( no_arg == 1) {
	   session_name = argv[optind];
	   if (strchr(session_name, '*') != NULL) {
		   MSG("Wild card is not supported in session name.");
		   return CMD_ERROR;
	   }
	   session_init(&session, session_name);
	   if (session.name[0] == '\0') {
		   MSG("Number is not allowed as name of the session");
		   return CMD_ERROR;
	   }
   } else {
	   /* session name was not specified it will be generated */
	   session_init(&session, "");
   }
   return ts_ted_session_create(&session, ip, subbuf_size, no_of_subbuf,
				switch_interval, flush_interval);
}


/* ========================================================================= */
/**
 *   Sends the request to stop the streaming on the specified session
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 *
 */
/* ========================================================================= */
static int
ts_session_delete(int argc, const char **argv)
{
   Session session;

   if (argc > 1) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   session_init(&session, argc > 0 ? argv[0] : "*");

   return ts_ted_session_delete(&session);
}


/* ========================================================================= */
/**
 *   Sends the status request to the ted server. Session details and
 *   all the enabled events are printed.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_status(int argc, const char **argv)
{
   const char *provider = "*";
   Session session;
   int ret;
   bool isSessionFound = false;
   HandlerType handlerType;
   TsTraceHandler *handler;

   if (argc > 1) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   session_init(&session, argc > 0 ? argv[0] : "*");

   /* Check the primary handler first */
   ret = getTsTedHandler()->cmd_status(&session, provider, &isSessionFound);

   if (ret != CMD_SUCCESS || !isSessionFound) {
      return ret;
   }

   /* Delegate to the optional trace handlers if session was found */
   handlerType = getHandlerType(provider);
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if ((handlerType & handler->type) && (handler->type != TED_HANDLER))
      {
         ret = handler->cmd_status(&session, provider, &isSessionFound);
         if (ret != CMD_SUCCESS) return ret;
      }
   }

   return ret;
}


/* ========================================================================= */
/**
 *   Sends the enable request for the specified events
 *   on the specified session.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_enable(int argc, const char **argv)
{
   int ret = CMD_SUCCESS;
   Session session;
   HandlerType handlerType;
   TsTraceHandler *handler;
   const char *provider = argv[argc-1];
   int handlerResult;
   bool isIgnored = true;

   if (argc < 3) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   session_init(&session, argv[0]);

   /* Skip session argument */
   argv++;
   argc--;

   if (ts_ted_session_validate(&session) != CMD_SUCCESS) {
           return CMD_ERROR;
   }
   handlerType = getHandlerType(provider);
    /* Validate the event arguments */
   if (validateEvents(argc, argv) != CMD_SUCCESS) {
      return CMD_ERROR;
   }
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         handlerResult = handler->cmd_enable(&session, provider, argc-1, argv);
         if (handlerResult == CMD_ERROR) {
            ret = CMD_ERROR;
         }
         if (handlerResult != CMD_IGNORED) {
            isIgnored = false;
         }
      }
   }

   if (isIgnored) {
      MSG("Specified provider not found");
   }

   return ret;
}


/* ========================================================================= */
/**
 *   Sends the disable request for the specified events
 *   on the specified session.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 *
 */
/* ========================================================================= */
static int
ts_disable(int argc, const char **argv)
{
   int ret = CMD_SUCCESS;
   Session session;
   HandlerType handlerType;
   TsTraceHandler *handler;
   const char *provider = argv[argc-1];
   int handlerResult;
   bool isIgnored = true;

   if (argc < 3){
      ERR("Invalid number of arguments.");
      return CMD_ERROR;
   }

   session_init(&session, argv[0]);

   /* Skip session argument */
   argv++;
   argc--;
   if (ts_ted_session_validate(&session) != CMD_SUCCESS) {
           return CMD_ERROR;
   }

   /* Delegate to the concerned trace handlers */
   handlerType = getHandlerType(provider);
   /* Validate the event arguments */
   if (validateEvents(argc, argv) != CMD_SUCCESS) {
      return CMD_ERROR;
   }
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         handlerResult = handler->cmd_disable(&session, provider, argc-1, argv);
         if (handlerResult == CMD_ERROR) {
            ret = CMD_ERROR;
         }
         if (handlerResult != CMD_IGNORED) {
            isIgnored = false;
         }
      }
   }
   if (isIgnored) {
      MSG("Specified provider not found");
   }

   return ret;
}


/* ========================================================================= */
/**
 *   Sends the default request for a particular session.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_default(int argc, const char **argv)
{
   int ret = CMD_SUCCESS;
   Session session;
   Scope scope = RUNNING;
   const char *provider = "*";
   HandlerType handlerType;
   TsTraceHandler *handler;

   if ((argc < 1) || (argc > 2)) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   if (strchr(argv[0], '*') != NULL) {
      MSG("Wild card is not supported in session name.");
      return CMD_ERROR;
   }
   if (strcmp(argv[0], "-restart") == 0) {
      scope = RESTART;
   } else {
      scope = RUNNING;
   }
   session_init(&session, argv[0]);

   if (argc == 2) {
      provider = argv[1];
   }
   if (ts_ted_session_validate(&session) != CMD_SUCCESS) {
           return CMD_ERROR;
   }

   /* Delegate to the concerned trace handlers */
   handlerType = getHandlerType(provider);
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         if (handler->cmd_default(&session, provider, scope) == CMD_ERROR)
         {
            ret = CMD_ERROR;
         }
      }
   }
   return ret;
}


/* ========================================================================= */
/**
 *   Sends the session save request to the ted server
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_save(int argc, const char **argv)
{
   int ret = CMD_SUCCESS;
   char *provider = "*";
   Session session;
   TsTraceHandler *handler;
   HandlerType handlerType;

   if ((argc < 1) || (argc > 2)) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   session_init(&session, argv[0]);

   if (argc >= 2) {
      provider = (char *) argv[1];
   }

   /* Delegate to the concerned trace handlers */
   handlerType = getHandlerType(provider);
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         if (handler->cmd_save(&session, provider) == CMD_ERROR)
         {
            ret = CMD_ERROR;
         }
      }
   }
   return ret;
}


/* ========================================================================= */
/**
 *   Sends the filter enable request to the ted server to the specified
 *   events of the sessions.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_filter(int argc, const char **argv)
{
   int ret = CMD_SUCCESS;
   int size = 0;
   int type;
   const char *filter = "";
   const char *event;
   Session session;
   HandlerType handlerType;
   TsTraceHandler *handler;

   if (argc < 2) {
      ERR("Invalid number of arguments.");
      return CMD_ERROR;
   }

   /* Check sub command */
   if (strcmp(argv[0], "set") == 0) {
      if (argc < 4) {
         ERR("No filter or event specified");
         return CMD_ERROR;
      }

      type = 0;
      session_init(&session, argv[1]);
      filter = argv[2];
      size = strlen(filter) + 1;
      event = argv[3];
      size += strlen(event) + 1;
   }
   else if (strcmp(argv[0], "reset") == 0) {
      if (argc < 3) {
         ERR("No event specified");
         return CMD_ERROR;
      }

      type = 1;
      session_init(&session, argv[1]);
      event = argv[2];
      size = strlen(event) + 1;
   }
   else {
      ERR("Unsupported sub-command");
      return CMD_ERROR;
   }
   if (ts_ted_session_validate(&session) != CMD_SUCCESS) {
           return CMD_ERROR;
   }

   /* Delegate to the concerned trace handlers */
   handlerType = getHandlerType(event);
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         if (handler->cmd_filter(&session, type, filter, event) == CMD_ERROR)
         {
            ret = CMD_ERROR;
         }
      }
   }

   return ret;
}


/* ========================================================================= */
/**
 *   Sends the session restart request to the ted server.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_restart(int argc, const char **argv)
{
   int force = 1;

   if (argc > 1) {
      ERR("Invalid number of arguments");
      return 1;
   }
   if (argc == 1) {
      if (strcmp(argv[0], "--if-correlate-enabled")) {
	 ERR("Invalid argument");
         return 1;
      }
      force = 0;
   }
   return ts_ted_restart(force);
}


/* ========================================================================= */
/**
 *   Sends the echo command to the specified session.
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_echo(int argc, const char **argv)
{
   Session session;

   if (argc != 2) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   session_init(&session, argv[0]);

   return ts_ted_echo(&session, argv[1]);
}


/* ========================================================================= */
/**
 *   Enable or disable time correlate for ts
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
ts_correlate(int argc, const char **argv)
{
   if (argc != 1) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }
   if (!strcmp(argv[0], "enable")) {
      return ts_ted_set_correlate(1);
   } else if (!strcmp(argv[0], "disable")) {
      return ts_ted_set_correlate(0);
   } else {
      ERR("Invalid argument");
      return 1;
   }
}


/* ========================================================================= */
/**
 *   Handles command from the console and send it to command handler
 *
 *   @param argc  argument count
 *   @param argv  argument vector
 *
 *   @return command result
 */
/* ========================================================================= */
static int
handle_cmd(int argc, const char **argv)
{
   int i = 0;
   const struct cmd_struct *cmd;

   cmd = &commands[i];
   while (cmd->func != NULL) {
      if (strcmp(cmd->name, argv[0]) == 0) {
         argc--;
         argv++;
         return cmd->func(argc, argv);
      }
      cmd = &commands[++i];
   }
   ERR("Command unsupported");
   return CMD_ERROR;
}

/** ==================================================================== */
/**
 *   This function parses the ts commands which were given on console
 *   and forwards it to the ts command handler.
 *
 *   @param           argc  argument count
 *                    argv  argument vector
 *
 *   @return          0 on Succesful execution
 *                    nonZero on failure
 *
 */
/* ========================================================================= */
int
parse_args(int argc, const char **argv)
{
   int ret;

   if (argc < 2) {
      usage();
      return -1;
   }

   argc--;
   argv++;

   ret = handle_cmd(argc, argv);
   switch (ret) {
   case CMD_ERROR:
      break;
   case CMD_FAILED:
      ERR("Command failed");
      break;
   case -1:
      usage();
      break;
   case CMD_SUCCESS:
      break;
   default:
      break;
   };
   return ret;
}

/* ========================================================================= */
/**
 *   This is the starting point and ending point of the exection of any
 *   ts command.
 *
 *   @param           argc  argument count
 *                    argv  argument vector
 *
 *   @return          0 on successful command execution.
 *                    exits if non successful
 */
/* ========================================================================= */
int
main(int argc, char *argv[])
{
   int ret;

   if (!ts_init()) {
      ERR("Command failed");
      return 0;
   }

   ret = parse_args(argc, (const char **) argv);
   if (ret != 0)
      exit(EXIT_FAILURE);

   ts_destroy();
   return 0;
}

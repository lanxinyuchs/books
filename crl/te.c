/**
 *   This file interprets the shell command 'te' and forwards the request to 
 *   the concerned shell command handler. The TED handler is the primary
 *   and mandatory command handler; the other handlers are optional
 *   and it shoud be possible to build the te command without them.
 *
 *   
 *
 *   @file te.c
 *
 *   An trace request will look like:
 *     "te enable ev1 ev2 tp1"
 *     Here ev1, ev2 are called trace events.
 *          tp1 is called as trace point provider.
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
 *   Revised : 2015-01-23 Ranganath
 *   Change  : Improved default and status commands
 *
 *   Revised : 2015-01-27 Nils Carlson <nils.carlson@ericsson.com>
 *   Change  : Add te restart command
 *
 *   Revised : 2015-02-06 Daniel Lefwerth
 *   Change  : Added Java Trace Handler (JTH).
 *
 *   Revised : 2015-03-17 Daniel Lefwerth
 *   Change  : Prevent usage of event 'all' in combination with wildcard
 *             in provider name.
 *
 *   Revised : 2015-06-08 Daniel Lefwerth
 *   Change  : Added options -l and -b to 'te log read'.
 *
 *   Revised : 2016-01-04 Anette Schött
 *   Change  : Added option -u and changed 'te log' command line reading to use
 *             getopt.
 *
 *   Revised : 2016-02-26 Anette Schött
 *   Change  : Add missing support for 'te s'.
 * ============================================================================
 */

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/queue.h>
#include <itc.h>
#include <getopt.h>

#include "ted.h"
#include "te_internal.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

#define TE_MBOX_NAME "te_cmd_mb"

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

TAILQ_HEAD(TeTraceHandlerList, TeTraceHandler);
typedef struct TeTraceHandlerList TeTraceHandlerList;


/*
 * This structure groups a 'te' sub-command name and its handler.
 *
 * A sub-command handler is called without the arguments <te> and <subcommand>
 * in the argument vector.
 */
struct cmd_struct {
   const char *name;
   int (*func)(int argc, const char **argv);
};


/* ============================================================================
 *   FUNCTION PROTOTYPES
 * ============================================================================
 */

/*
 * The te sub command functions.
 */
static int te_enable(int argc, const char **argv);
static int te_disable(int argc, const char **argv);
static int te_preset(int argc, const char **argv);
static int te_config(int argc, const char **argv);
static int te_default(int argc, const char **argv);
static int te_save(int argc, const char **argv);
static int te_filter(int argc, const char **argv);
static int te_restart(int argc, const char **argv);


/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */


static const struct cmd_struct commands[] = {
   {"enable", te_enable},
   {"e", te_enable},
   {"disable", te_disable},
   {"d", te_disable},
   {"preset", te_preset},
   {"config", te_config},
   {"default", te_default},
   {"save", te_save},
   {"filter", te_filter},
   {"restart", te_restart},
   {NULL, NULL}
};


/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

static itc_mbox_id_t my_mb = ITC_NO_ID;
static TeTraceHandlerList handlers;

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */

/* ========================================================================= */
/*
 * Prints the 'te' command usage help.
 */
/* ========================================================================= */
static void 
usage(void)
{
   fprintf(
      stderr, 
      "Shell command to manage the T&E session. The te command interacts with\n"
      "trace points created via LTTng and with traces created via TRI interface.\n"
      "Generated traces are saved in buffers, separate buffers for each CPU and\n"
      "user group of program generating the trace. The traces are displayed from\n"
      "all buffers and printed in chronological order. Each of the buffers are\n"
      "wrap around buffers, which means that oldest traces will disappear when\n"
      "respectively buffer is full.\n" 
      "\n"
      "Usage: te <cmd> <param> ...\n"
      "Commands <cmd>:\n"
      "    enable <group/event> ... [<process/provider>]\n"
      "        Enable trace groups/LTTng events.\n"
      "\n"
      "    disable <group/event> .. [<process/provider>]\n"
      "        Disable trace groups/LTTng events.\n"
      "\n"
      "    status [-restart|-preset|-u] [<process/provider>] [-l <program>]\n"
      "        Display tracing status.\n"
      "        '-l' lists registered LTTng events for the specifed program.\n"
      "        '-restart' prints the saved Preset list.\n"
      "        '-preset'  prints the Preset list.\n"
      "        '-u'  prints the available user groups to use when reading T&E.\n"
      "\n"
      "    default [-preset|-restart][<process/provider>|'*']\n"
      "        Set trace groups/LTTng events back to default.\n"
      "        '-restart' removes all the saved preset list.\n"
      "        '-preset'  removes only the preset list.\n"
      "\n"
      "    save {<process/provider>|'*'}\n"
      "        Save currently enabled trace groups/LTTng events.\n"
      "        Specifying '*' instead of process will also save currently enabled\n"
      "        LTTng events.\n"
      "\n"
      "    log read [-l] [-b] [-u <user group>]\n"
      "        Read T&E log.\n"
      "        '-l' prints the log level for each event\n"
      "        '-b' prints the output in brief format\n"
      "        '-u' prints the traces for the specified user group\n"
      "\n"
      "    log clear\n"
      "        Clear T&E log.\n"
      "\n"
      "    preset [+|-]<group/event> ... <process/provider>\n"
      "        Enable/disable LTTng events for specified items if they exists.\n"
      "        Presets the trace group mask for the specified process\n"
      "        during interception of the process when they are created.\n"
      "        +|-    Specify whether the trace group is to be enabled (+)\n"
      "               or disabled (-).\n"
      "               Enabled is default, if the trace group name only is specified.\n"
      "\n"
      "    config [+|-]<group/event> ... <process/provider>\n"
      "        Configure a trace group mask or event to be saved for specified items.\n"
      "        This is similar to 'te preset' where saved preset survives a restart.\n"
      "\n"
      "    filter set <filter> <event/process>\n"
      "        Activate a filter on a LTTng event/process.\n"
      "        Filter expression examples:\n"
      "        LTTng format :\n"
      "            'intfield > 500 && intfield < 503'\n"
      "            '(strfield == \"test\" || intfield != 10) && intfield > 33'\n"
      "            'doublefield > 1.1 && intfield < 5.3'\n"
      "        TRI format:\n"
      "            'LEN < 30'\n"
      "            '[35:2..6] = 3'\n"
      "            'LEN<30 AND ([4]=10 OR [14]>=$FE)'\n"
      "\n"
      "    filter reset <event>\n"
      "        Deactivate a filter on a LTTng event.\n"
      "\n"
      "    restart\n"
      "        Restart the te trace, used for time updated.\n"
      "\n"
      "Supported TRI trace group names <group>:\n"
      "    " GROUP_SYNTAX
      "\n"
      "NOTES:\n"
      "    - <process>  = Name of process. Wildcard '*' can be used at end/instead of name\n"
      "                  (Not applicable for all commands)\n"
      "    - <program>  = Name of Linux program/process. No wildcards '*' allowed\n"
      "    - <process> is not applicable when dealing with LTTng events\n"
      "    - <provider> = Name of LTTng tracepoint provider\n"
      "    - <event>    = LTTng event name.\n"
      "    - Mix of LTTng event names and TRI group names is allowed on LTTng tracepoint provider\n"
      "    - For LTTng event names wildcard(*) is only allowed at the end of the name\n"
      "    - For LTTng event names 'all' is not allowed\n"
      "\n"
      "Command Examples:\n"
      "    Enable LTTng event(s):\n"
      "        te enable myevent01 myevent02 com_ericsson_myprov\n"
      "        te enable * com_ericsson_myprov\n"
      "        te enable myevent01 com_*\n"
      "        te enable myevent01 com_ericsson_*\n"
      "        te enable myevent01 trace1 com_ericsson_myprov\n"
      "        te enable myevent01 trace1 com_*\n"
      "        te enable myevent01 trace1 *\n"
      "    List registered LTTng events for 'myprog':\n"
      "        te status -l myprog\n"
      "    Activate a filter:\n"
      "        te filter set 'intf < 20 && intf > 50' com_ericsson_myprov:myevent\n"
      "        te filter set 'LEN<30 AND ([4]=10 OR [14]>=$FE)' myprocess\n"
      "    Deactivate a filter:\n"
      "        te filter reset com_ericsson_myprov:myevent\n"
      "        te filter reset myprocess\n"
      "    Save currently enabled trace groups and events:\n"
      "        te save '*'\n"
      "    Presets the trace groups/events specfied\n"
      "        te preset +trace1 myprocess\n"
      "        te preset +trace1 -info myproc*\n"
      "        te preset +trace1 *\n"
      "        te preset myevent01 trace1 com_*\n"
      "        te preset myevent01 trace1 *\n"
      "    Configures the trace groups/events specfied\n"
      "        te config +trace1 myprocess\n"
      "        te config +trace1 -info myproc*\n"
      "        te config +trace1 *\n"
      "        te config +all myprocess\n"
      "        te config myevent01 trace1 com_*\n"
      "        te config myevent01 -trace1 *\n"
      "\n");
}


/* ========================================================================= */
/**
 *  Register a command handler to the 'te' command.
 *
 *  @param handler - the handler to register
 *
 *  @globals handlers
 */
/* ========================================================================= */
void 
te_registerHandler(TeTraceHandler *handler)
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
te_init(void)
{
   itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

   /* Create the mailbox */
   if (my_mb == ITC_NO_ID) {
      my_mb = itc_create_mailbox(TE_MBOX_NAME, 0);
   }
   if (my_mb == ITC_NO_ID) {
      return false;
   }
   return true;
}


/* ========================================================================= */
/**
 *  Cleanup after te shell command.
 *
 *  @globals my_mb
 */
/* ========================================================================= */
static void 
te_destroy(void)
{
   if (my_mb != ITC_NO_ID) {
      itc_delete_mailbox(my_mb);
   }
   itc_exit();
}


/* ========================================================================= */
/**
 *  Get the type of handlers that are concerned about the specified provider.
 *  This function only analyses the trace provider name. 
 *
 *  Currently this function aggregates the information of all handlers;
 *  ideally this information should be distributed as well.
 *
 *  @param provider - 
 *
 *  @return a bitmask containing the concerned handler types
 */
/* ========================================================================= */
static HandlerType
getHandlerType(const char *provider, int scope)
{
   char *wptr = NULL;
   HandlerType handlerType = NO_HANDLER;

   /*
    * Some scope is only applicable for ted, only provider is not
    * possible to use.
    */
   if ((scope == UID) || (scope == LIST)) {
      return TED_HANDLER;
   }

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

   /*
    * Wild card in the middle of traceItem name is not allowed.
    */
   if (strncmp(provider, LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) == 0)
   {
      /* Check if * is used in the provider name */
      if ((wptr = strchr(provider, '*')) && (*(wptr + 1) != '\0'))
      {
         ERR("Wildcards only allowed at the end of lttng provider name");
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
#ifdef TE_MCT
   return TED_HANDLER;
#else
   return handlerType;
#endif
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
 *  Requests the status information from the servers that are interested 
 *  in the specified provider.
 *
 *  Ex:
 *  te status ose_ns         (TRI event)
 *  te status com_*          (Lttng event)
 *  te status com.ericsson:* (Java event)
 *  te status *              (common event sent to all handlers)
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_status(int argc, char * const argv[])
{
   const char *provider = "*";
   int ret = CMD_SUCCESS;
   int scope = RUNNING;
   int handlerType = ALL_HANDLERS;
   bool isIgnored = true;
   bool isProgramSpecified = false;
   TeTraceHandler *handler;
   int handlerResult;
   int opt;

   while (((opt = getopt(argc, argv, "r::p::l:u")) != -1) &&
          (scope == RUNNING)) {

     switch (opt) {
     case 'r':
       scope = RESTART;
       /* te status -restart <provider> */
       if (optind < argc) {
               provider = argv[optind];
               optind++;
       }
       break;
     case 'p':
       scope = PRESET;
       /* te status -preset <provider> */
       if (optind < argc) {
               provider = argv[optind];
               optind ++;
       }
       break;
     case 'l':
       scope = LIST;
       if (optarg) {
	 provider = optarg;
         isProgramSpecified = true;
       }
       break;
     case 'u':
       scope = UID;
       break;
     default: /* '?' */
       return CMD_ERROR;
     }
   }

   /*
    * 'te status' can at most have zero or one option.
    */ 
   if ((optind != 1) && (optind < argc)) {
      ERR("Unexpected argument after option");
      return CMD_ERROR;
   }

   /* Handles 'te status <provider>' */
   if ((optind == 1) && (argc > 1)) {
      provider = argv[optind];
   }

   handlerType = getHandlerType(provider, scope);
   if (handlerType == NO_HANDLER) {
      ERR("Invalid trace item provided (%s)", provider);
      return CMD_ERROR;
   }

   /* Delegate to the concerned handlers */
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         handlerResult = handler->cmd_status(provider,
                                             scope,
                                             isProgramSpecified);
         if (handlerResult == CMD_ERROR) {
            ret = CMD_ERROR;
         }
         if (handlerResult != CMD_IGNORED) {
            isIgnored = false;
         }
      }
   }

   if (isIgnored) {
      if (strcmp(provider, "*") == 0) {
         MSG("All providers have default configuration");
      }
      else {
         MSG("Specified provider not found");
      }
   }

   return ret;
}


/* ========================================================================= */
/**
 *  Sends the enable request for events to the corresponding servers
 *  according to the specified provider argument.
 *
 *  Ex:
 *  te enable info ose_ns           (TRI event)
 *  te enable ntpDebug com_*        (Lttng event)
 *  te enable trace1 com.ericsson:* (Java event)
 *  te enable trace1 *              (common event sent to all handlers)
 *
 *  @param argc - argument count
 *  @param argv - argument vector, starting with the events
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_enable(int argc, const char **argv)
{
   const char *provider = argv[argc - 1];
   int ret = CMD_SUCCESS;
   TeTraceHandler *handler;
   int handlerType;
   int handlerResult;
   bool isIgnored = true;

   if (argc < 2) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   /* Find out which handlers that are concerned with this provider */
   handlerType = getHandlerType(provider, RUNNING);
   if (handlerType == NO_HANDLER) {
      ERR("Invalid trace item provided");
      return CMD_ERROR;
   }

   /* Validate the event arguments */
   if (validateEvents(argc, argv) != CMD_SUCCESS) {
      return CMD_ERROR;
   }

   /* Delegate to the concerned handlers */
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         handlerResult = handler->cmd_enable(provider, argc-1, argv);
         if (handlerResult == CMD_ERROR)
         {
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
 *  Sends the disable request for events to the corresponding servers
 *  according to the specified provider argument.
 *
 *  Ex:
 *  te disable info ose_ns           (TRI event)
 *  te disable ntpDebug com_*        (Lttng event)
 *  te disable trace1 com.ericsson:* (Java event)
 *  te disable trace1 *              (common event sent to all handlers)
 *
 *  @param argc - argument count
 *  @param argv - argument vector, starting with the events
 *
 *  @return command result
 *
 */
/* ========================================================================= */
static int 
te_disable(int argc, const char **argv)
{
   const char *provider = argv[argc - 1];
   int ret = CMD_SUCCESS;
   TeTraceHandler *handler;
   int handlerType;
   int handlerResult;
   bool isIgnored = true;

   if (argc < 2) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   /* Find out which handlers that are concerned with this provider */
   handlerType = getHandlerType(provider, RUNNING);
   if (handlerType == NO_HANDLER) {
      ERR("Invalid trace item provided");
      return CMD_ERROR;
   }

   /* Validate the event arguments */
   if (validateEvents(argc, argv) != CMD_SUCCESS) {
      return CMD_ERROR;
   }

   /* Delegate to the concerned handlers */
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         handlerResult = handler->cmd_disable(provider, argc-1, argv);
         if (handlerResult == CMD_ERROR)
         {
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
 *  Sends the specified preset/config trace group request to the corresponding
 *  server (tri/Lttng) depending on the trace events provided.
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *  @param savePreset - true if the configuration should survive a restart
 *                      false otherwise
 *
 *  @return CMD_ERROR (if command failed)
 *          CMD_SUCCESS(if command succeded)
 */
/* ========================================================================= */
int 
handlePresetConfig(int argc, const char **argv, bool savePreset)
{
   int ret = CMD_SUCCESS;
   TeTraceHandler *handler;
   HandlerType handlerType;
   const char *provider = argv[argc - 1];

   if ((argc > 0) && strcmp(argv[0], "-run") == 0) {
      argv++;
      argc--;
   }

   if (argc < 2) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   /* Find out which handlers that are concerned with this provider */
   handlerType = getHandlerType(provider, RUNNING);
   if (handlerType == NO_HANDLER) {
      ERR("Invalid trace item provided");
      return CMD_ERROR;
   }

   /* Validate the event arguments */
   if (validateEvents(argc, argv) != CMD_SUCCESS) {
      return CMD_ERROR;
   }

   /* Delegate to the concerned handlers */
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         if (handler->cmd_preset_config(provider, argc-1, argv, savePreset) == CMD_ERROR)
         {
            ret = CMD_ERROR;
         }
      }
   }
   return ret;
}


/* ========================================================================= */
/**
 *  Handles the 'te config' sub-command.
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_config(int argc, const char **argv)
{
   return handlePresetConfig(argc, argv, true);
}

/* ========================================================================= */
/**
 *  Handles the 'te preset' sub-command.
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_preset(int argc, const char **argv)
{
   return handlePresetConfig(argc, argv, false);
}


/* ========================================================================= */
/**
 *  Sends the default group mask request to the trace handlers that are 
 *  interested in the specified provider.
 *
 *  te default os_ns       (sent to TRI handler)
 *  te default *           (sent to all handlers)
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_default(int argc, const char **argv)
{
   int ret = CMD_SUCCESS;
   const char *provider = "*";
   TeTraceHandler *handler;
   HandlerType handlerType;
   Scope scope = RUNNING;
   int i = 0;

   if (argc > 2) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   if (argc > 0) {
      /* Parse the optional option argument  */
      if (strcmp(argv[i], "-restart") == 0) {
         scope = RESTART;
         i++;
      }
      else if (strcmp(argv[i], "-preset") == 0)
      {
         scope = PRESET;
         i++;
      }
      else
      {
         if (argc == 2) {
                ERR("Invalid arguments");
                return CMD_ERROR;
         }
      }
   }

   /* Parse the provider (if any) */
   if (i < argc)
   {
      provider = argv[i++];
   }

   if (i != argc) {
      ERR("Too many arguments");
      return CMD_ERROR;
   }

   /* Delegate to the concerned handlers */
   handlerType = getHandlerType(provider, scope);
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         if (handler->cmd_default(provider, scope) == CMD_ERROR)
         {
            ret = CMD_ERROR;
         }
      }
   }
   return ret;
}


/* ========================================================================= */
/**
 *  Sends the trace configuration save request to the trace handlers 
 *  that are interested in the specified provider.
 *
 *  Ex:
 *  te save os_ns       (sent to TRI handler)
 *  te save *           (sent to all handlers)
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_save(int argc, const char **argv)
{
   int ret = 0;
   TeTraceHandler *handler;
   int handlerType;
   const char *provider;

   if (argc != 1) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   provider = argv[0];
   handlerType = getHandlerType(provider, RUNNING);

   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         if (handler->cmd_save(provider) == CMD_ERROR)
         {
            ret = CMD_ERROR;
         }
      }
   }

   return ret;
}


/* ========================================================================= */
/**
 *  Set a trace filter
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_filter(int argc, const char **argv)
{
   int ret = 0;
   int type;
   const char *filter = "";
   const char *provider;
   TeTraceHandler *handler;
   HandlerType handlerType;

   if (argc < 1) {
      ERR("Invalid number of arguments");
      return CMD_ERROR;
   }

   /* Analyse sub command */
   if (strcmp(argv[0], "set") == 0) {
      if (argc < 3) {
         ERR("No filter or event specified");
         return CMD_ERROR;
      }

      type = 0;
      filter = argv[1];
      provider = argv[2];
   }
   else if (strcmp(argv[0], "reset") == 0) {
      if (argc < 2) {
         ERR("No event specified");
         return CMD_ERROR;
      }

      type = 1;
      provider = argv[1];
   }
   else {
      ERR("Unsupported sub-command");
      return CMD_ERROR;
   }

   /* Delegate to the concerned handlers */
   handlerType = getHandlerType(provider, RUNNING);
   TAILQ_FOREACH(handler, &handlers, entry)
   {
      if (handlerType & handler->type) {
         if (handler->cmd_filter(type, filter, provider) == CMD_ERROR)
         {
            ret = CMD_ERROR;
         }
      }
   }
   return ret;
}


/* ========================================================================= */
/**
 *  Handles the 'te log read' or 'te log clear' request.
 *
 *  This command is only sent to the LTTng handler.
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
static int
te_log(int argc, char * const argv[])
{
   TedInfo info;
   char *endptr;
   int opt;

   info.type = 0;
   info.options = 0;
   info.uid = -1;

   /* Skip 'log' argument */
   argc--;
   argv++;

   if (argc < 1) {
      ERR("Sub-command read or clear is missing\n");
      return CMD_ERROR;
   }

   if((strcmp(argv[0], "read") != 0) &&
      (strcmp(argv[0], "clear") != 0)) {
      ERR("Unsupported sub-command (%s)\n", argv[0]);
      return CMD_ERROR;
   }

   if (!strcmp(argv[0], "clear") && (argc > 1)) {
      ERR("Too many arguments\n");
      return CMD_ERROR;
   }

   if (!strcmp(argv[0], "read") && (argc > 3)) {
      ERR("Too many arguments\n");
      return CMD_ERROR;
   }

   if (!strcmp(argv[0], "clear")) {
      info.type = 1;
   }

   /* Parse  the arguments */
   while ((opt = getopt(argc, argv, "lbu:")) != -1) {

      switch (opt) {
      case 'b':
         info.options |=  TED_LOG_BRIEF;
         break;
      case 'l':
        info.options |=  TED_LOG_LEVEL;
        break;
     case 'u':
        info.options |= TED_LOG_UID;
        info.uid = strtol(optarg, &endptr, 10);
        if (endptr != NULL && *endptr != '\0') {
          ERR("User id must be a number\n");
          return CMD_ERROR;
        }
        if (info.uid < 0) {
          ERR("User id must be a positive number\n");
          return CMD_ERROR;
        }
        break;
      default: /* '?' */
         return CMD_ERROR;
      }
   }

   if (optind > (argc + 1)) {
      ERR("Invalid number of arguments\n");
      return CMD_ERROR;
   }

   /* 'te log' is only handled by TED */
   return ted_log(&info);
}


/* ========================================================================= */
/**
 *   Sends the session restart request to the ted server.
 *
 *  @param argc - argument count
 *  @param argv - argument vector, starting with subcommand
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
te_restart(int argc, const char **argv)
{
   (void)argc;
   (void)argv;

   return ted_restart();
}


/* ========================================================================= */
/**
 *  Handles command from the console and calls the subcommand handler.
 *
 *  @param argc - argument count
 *  @param argv - argument vector, starting with subcommand
 *
 *  @return command result
 */
/* ========================================================================= */
static int 
handle_cmd(int argc, const char **argv)
{
   const struct cmd_struct *cmd;

   for (cmd = commands; cmd->func != NULL; cmd++) {
      if (strcmp(cmd->name, argv[0]) == 0) {
         /* Skip subcommand argument  */
         argc--;
         argv++;
         return cmd->func(argc, argv);
      }
   }
   ERR("Command unsupported");
   return CMD_ERROR;
}

/* ========================================================================= */
/**
 *  This function parses the 'te' command which were given on console
 *  and forwards it to the te subcommand handler.
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return command result
 */
/* ========================================================================= */
int
parse_args(int argc, const char **argv)
{
   int ret = -99;

   /* Skip 'te' argument */
   argc--;
   argv++;

   /*
    * Handle 'log' and 'status' sub command separate as it uses getopt to
    * check arguments. I should suggest that all sub commands should be changed
    * to use this as well and then this different handling can be removed.
    */
   if (strcmp(argv[0], "log") == 0) {
           ret = te_log(argc, (char * const*)argv);
   }
   else if ((strcmp(argv[0], "status") == 0) ||
            (strcmp(argv[0], "s") == 0)) {
      ret = te_status(argc, (char * const*)argv);
   }

   if (ret == -99) {
      ret = handle_cmd(argc, argv);
   }

   switch(ret) {
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
 *  This is the starting point and ending point of the exection of any
 *  'te' shell command.
 *
 *  @param argc - argument count
 *  @param argv - argument vector
 *
 *  @return 0 if successful, exits if non successful
 */
/* ========================================================================= */
int 
main(int argc, char *argv[])
{
   int ret;

   if (!te_init()) {
      ERR("Command failed");
      return 0;
   }

   if (argc < 2) {
      usage();
      exit(EXIT_FAILURE);
   }

   ret = parse_args(argc, (const char **) argv);
   if (ret != CMD_SUCCESS)
      exit(EXIT_FAILURE);

   te_destroy();
   return 0;
}

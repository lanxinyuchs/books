/**
 *   This file contains te command handler support.
 *
 *   @file te_internal.h
 *
 *   Copyright (C) 2014-2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef TE_INTERNAL_H
#define TE_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdint.h>
#include <stdbool.h>
#include <sys/queue.h>

#include "trace_cmd.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/*
 ** Define syntax for the different parameters
 */
#define GROUP_SYNTAX                                                    \
        "<group> = check | error | enter | return | info | trace1 .. trace9 |\n\
              state_change | bus_send | bus_receive | rec_sig | send_sig |\n\
              param | user1 .. user4 | all\n"



/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

/* ========================================================================= */
/**
 * A trace handler handles shell commands targeted for a specific 
 * trace server. A trace handler need to implement the functions 
 * specified by this type.
 *
 * A trace handler must register to the 'te' command using 
 * the function te_registerHandler().
 */
/* ========================================================================= */
typedef struct TeTraceHandler {
   const char *name;
   HandlerType type;

   /**
    * Get the status of the specified provider.
    *
    * @param provider - provider name
    * @param scope - TRANSIENT to get the running status, 
    *                RESTART to get the status of the saved events,
    *                PRESET to get the status of the preset events
    * @param isProgramSpecified - 
    *
    * @return CMD_SUCCESS, CMD_ERROR, CMD_IGNORED
    */
   int (*cmd_status)(const char *provider, int scope, bool isProgramSpecified);

   /**
    * Enable events for the specified provider.
    *
    * @param provider - provider name, might end with '*'
    * @param nEvents - number of events
    * @param events - the event names
    *
    * @return CMD_SUCCESS, CMD_ERROR, CMD_IGNORED
    */
   int (*cmd_enable)(const char *provider, int nEvents, const char *events[]);

   /**
    * Disable events for the specified provider.
    *
    * @param provider - provider name, might end with '*'
    * @param nEvents - number of events
    * @param events - the event names
    *
    * @return CMD_SUCCESS, CMD_ERROR, CMD_IGNORED
    */
   int (*cmd_disable)(const char *provider, int nEvents, const char *events[]);

   /**
    * Preset or configure events for the specified provider.
    *
    * @param provider - provider name, might end with '*'
    * @param nEvents - number of events
    * @param events - the event names
    * @param savePreset - true if configuration should survive restart (config)
    *                     false otherwise (preset)
    */
   int (*cmd_preset_config)(const char *provider, 
                            int nEvents, const char *events[], 
                            bool savePreset);

   /**
    * Set the default trace configuration for the specified provider.
    *
    * @param provider - provider name, might end with '*'
    * @param scope - TRANSIENT to set default of the transient configuration,
    *                RESTART to set the default of saved events,
    *                PRESET to set the default of preset events'
    */
   int (*cmd_default)(const char *provider, Scope scope);

   /*
    * Save the configuration for the specified provider.
    *
    * @param provider - provider name
    */
   int (*cmd_save)(const char *provider);

   /**
    * Activate/de-activate a filter on the specified event.
    *
    * @param type - 0 for activate (set), 1 for de-activate (reset)
    * @param traceFilter - 
    * @param event - event name
    */
   int (*cmd_filter)(int type, const char *traceFilter, const char *event);

   /* List head of trace handlers */
   TAILQ_ENTRY(TeTraceHandler) entry;
} TeTraceHandler;


/* ============================================================================
 *   FUNCTION PROTOTYPES
 * ============================================================================
 */

/* ========================================================================= */
/**
 *  Register a command handler to the 'te' command.
 *
 *  @param handler - the handler to register
 */
/* ========================================================================= */
extern void te_registerHandler(TeTraceHandler *handler);


#ifdef __cplusplus
}
#endif

#endif /* TE_INTERNAL_H */

/**
 *   This file contains 'ts' command handler support.
 *
 *   @file ts_internal.h
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
#ifndef TS_INTERNAL_H
#define TS_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdint.h>
#include <lttng/lttng.h>
#include <sys/queue.h>
#include "trace_cmd.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

/* ========================================================================= */
/*
 * A session object contains either a session id or a session name.
 * If session name is valid, the id is 0.
 * If session id is valid, the name is an empty string.
 */
/* ========================================================================= */
typedef struct Session {
   uint32_t id;
   char name[LTTNG_SYMBOL_NAME_LEN];
} Session;


/* ========================================================================= */
/**
 * A trace handler handles shell commands targeted for a specific 
 * trace server.
 *
 * A trace handler must register to the 'ts' command using 
 * the function ts_registerHandler().
 */
/* ========================================================================= */
typedef struct TsTraceHandler {
   const char *name;
   HandlerType type;

   /**
    * Get the status of the specified provider.
    *
    * @param session - 
    * @param provider - 
    * @param isSessionFound - for TED handler to set if a session was found
    */
   int (*cmd_status)(const Session *session, const char *provider, 
                     bool *isSessionFound);

   /**
    * Enable events for the specified provider.
    *
    * @param session - 
    * @param provider - provider name, might end with '*'
    * @param nEvents - number of events
    * @param events - the event names
    */
   int (*cmd_enable)(const Session *session, const char *provider,
                     int nEvents, const char *events[]);

   /**
    * Disable events for the specified provider.
    *
    * @param session - 
    * @param provider - provider name, might end with '*'
    * @param nEvents - number of events
    * @param events - the event names
    */
   int (*cmd_disable)(const Session *session, const char *provider,
                      int nEvents, const char *events[]);

   /**
    * Set the default trace configuration for the specified provider.
    *
    * @param session - 
    * @param provider - provider name, might end with '*'
    * @param scope    - Scope of running default command
    */
   int (*cmd_default)(const Session *session, const char *provider,
                                              Scope scope);

   /*
    * Save the configuration for the specified provider.
    *
    * @param session - 
    * @param provider - provider name
    */
   int (*cmd_save)(const Session *session, const char *provider);

   /*
    * @param session - 
    * @param type - 0 for set, 1 for reset
    * @param traceFilter - 
    * @param event - 
    */
   int (*cmd_filter)(const Session *session, 
                     int type, const char *traceFilter, const char *event);

   /* List head of trace handlers */
   TAILQ_ENTRY(TsTraceHandler) entry;
} TsTraceHandler;


/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

/* ============================================================================
 *   FUNCTION PROTOTYPES
 * ============================================================================
 */

/* ========================================================================= */
/**
 * Create a new streaming session.
 *
 * @param session         - session name to use
 * @param ip              - ip string of observer
 * @param subbuf_size     - size in bytes of subbuffer
 *                          if 0 is given default value 32 k is used
 * @param no of subbuf    - number of subbuffers, (must be multiple of 2)
 *                          if 0 is given default value is used
 * @param switch_interval - switch timer interval in usec
 *                          if 0 is given default value 2 sec is used
 * @param flush_interval -  data flush timer interval in usec
 *                          if 0 is given default value 1 sec is used
 *
 * @return command result
 */
/* ========================================================================= */
extern int ts_ted_session_create(const Session *session, const char *ip,
				 uint64_t subbuf_size, uint64_t no_of_subbuf,
				 unsigned int switch_interval,
				 unsigned int flush_interval);

/* ========================================================================= */
/**
 * Delete a streaming session.
 *
 * @param session - the session to delete
 *
 * @return command result
 */
/* ========================================================================= */
extern int ts_ted_session_delete(const Session *session);

/* ========================================================================= */
/**
 * Send the session request to the TED server.
 *
 * @param force - if 0 first check if correlate is enabled,
 *                otherwise restart ts directly
 *
 * @return command result
 */
/* ========================================================================= */
extern int ts_ted_restart(unsigned int force);

/* ========================================================================= */
/**
 * Enable or disable correlate, i e if ts session shall be restarted after
 * NTP synchronization
 *
 * @param val - if 0 disable, otherwise enable. Default is disabled.
 *
 * @return command result
 */
/* ========================================================================= */
extern int ts_ted_set_correlate(unsigned int val);

/* ========================================================================= */
/**
 * 
 *
 * @return command result
 */
/* ========================================================================= */
extern int ts_ted_echo(const Session *session, const char *provider);

/* ========================================================================= */
/**
 *  Register a command handler to the 'ts' command.
 *
 *  @param handler - the handler to register
 */
/* ========================================================================= */
extern void ts_registerHandler(TsTraceHandler *handler);


/* ========================================================================= */
/**
 *   Returns the TED handler which is the mandatory command handler.
 */
/* ========================================================================= */
extern TsTraceHandler* getTsTedHandler(void);

/* ========================================================================= */
/**
 * 
 *
 * @return if session was available.
 */
/* ========================================================================= */
extern int ts_ted_session_validate(const Session *session);


#ifdef __cplusplus
}
#endif

#endif /* TS_INTERNAL_H */

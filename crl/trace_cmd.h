/**
 *   This file contains generic trace command handler support.
 *
 *   @file trace_cmd.h
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
 *   Change  : Moved command timeout to this file and increased it from 10
 *             to 15 s.
 *
 * ========================================================================
 */

#ifndef TRACE_CMD_H
#define TRACE_CMD_H

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdint.h>
#include <stdbool.h>

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

#define INFO(fmt, args...)                                \
        fprintf(stderr, "Info: " fmt "\n", ## args)
#define ERR(fmt, args...)                                \
        fprintf(stderr, "Error: " fmt "\n", ## args)
#define WARN(fmt, args...)                                \
        fprintf(stderr, "Warning: " fmt "\n", ## args)
#define DBG(fmt, args...)                                \
        fprintf(stderr, "DBG: " fmt "\n", ## args)
#define MSG(fmt, args...)                        \
        fprintf(stdout, fmt "\n", ## args)
#define _MSG(fmt, args...)                        \
        fprintf(stdout, fmt, ## args)

/* LTTNG wild card provider */
#define LTTNG_PROVIDER "com_"

/* Receive timeout for commands */
#define RECEIVE_TMO (15000) /* ms */

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

/*
 * The handler type specified which trace handler that should handle
 * a certain command which depends on the command arguments.
 *
 * Note that the values need to be a power of 2 in order to be
 * used as bitmasks.
 */
typedef enum HandlerType {
   NO_HANDLER  = 0,
   TED_HANDLER = 1,
   TRI_HANDLER = 2,
   JTE_HANDLER = 4,
} HandlerType;
#define ALL_HANDLERS (TED_HANDLER|TRI_HANDLER|JTE_HANDLER)


/*
 * The Scope specifies if the status or default command
 * should impact the transient, the restart (saved), the
 * preset configuration or user id or lttng event listing.
 *
 */
typedef enum  {
   RUNNING  = 0,
   RESTART  = 1,
   PRESET   = 2,
   UID      = 3,
   LIST     = 4
} Scope;


/*
 * Generic return codes when calling the command functions.
 *
 * CMD_SUCCESS - command was successful
 * CMD_ERROR - an error occured
 * CMD_FAILED -
 * CMD_IGNORED - the command could not be handled by the handler, or
 *               no provider matched the provider name (no error)
 */
enum cmd_error_code {
   CMD_SUCCESS = 0,
   CMD_ERROR,
   CMD_FAILED,
   CMD_IGNORED
};


/* ============================================================================
 *   FUNCTION PROTOTYPES
 * ============================================================================
 */


#ifdef __cplusplus
}
#endif

#endif /* TRACE_CMD_H */

/**
 *  This file contains some common definitions used by the TRI trace handlers.
 *
 *  @file tri.h
 *
 *  Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *  information in this document is the property of Ericsson. Except
 *  as specifically authorized in writing by Ericsson, the receiver
 *  of this document shall keep the information contained herein
 *  confidential and shall protect the same in whole or in part from
 *  disclosure and dissemination to third parties. Disclosure and
 *  disseminations to the receiver's employees shall only be made on
 *  a strict need to know basis.
 */

#ifndef TRI_H
#define TRI_H

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdint.h>
#include <trace_cmd.h>
#include <te_internal.h>

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

/* ============================================================================
 *   FUNCTION PROTOTYPES
 * ============================================================================
 */

extern void tri_printTraceGroups(uint32_t groupMask);
extern uint32_t tri_getGroupType(const char *group);

/* ========================================================================= */
/**
 *   Returns the TRI handler.
 */
/* ========================================================================= */
extern TeTraceHandler* getTeTriHandler(void);


#ifdef __cplusplus
}
#endif

#endif /* TRI_H */

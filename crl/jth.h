/**
 * This file contains some common definitions used by the Java trace handlers.
 *
 * @file jth.h
 *
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#ifndef JTH_H
#define JTH_H

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

extern void jth_printTraceGroups(uint32_t groupMask);

/* ========================================================================= */
/**
 *   Returns the Java trace handler.
 */
/* ========================================================================= */
extern TeTraceHandler* getJavaTraceHandler(void);

#ifdef __cplusplus
}
#endif

#endif /* JTH_H */

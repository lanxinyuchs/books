/**
 *   This is the header file for utility macros for LTTng tracing.
 *
 *   @file trace_macros.h
 *
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef __TRACE_MACROS_H
#define __TRACE_MACROS_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdio.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define STR(FORMAT, ...) (traceUtil_formatStr(FORMAT, __VA_ARGS__))

#define  TRACE_UTIL_MAX_FORMAT_STR_LEN (512)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Used by thread to store formatted string */
static __thread char fmt_str[TRACE_UTIL_MAX_FORMAT_STR_LEN] = {0};

/* ===================================================================== */
/**
 *   This function builds a formatted string and stores in the
 *   process information structure for the current process.
 *
 *   @param format    String containing format information
  *
 *   @param ...        Variable number of parameters according to format
 *
 *   @return           A pointer to the formatted string.
 *
 *   @par Globals:
 *                     fmt_str
 */
/* ===================================================================== */
static inline char *
traceUtil_formatStr(const char *format, ...)
{
   va_list args;  /* Argument list */

   va_start(args, format);
   vsnprintf(fmt_str, TRACE_UTIL_MAX_FORMAT_STR_LEN, format, args);
   va_end(args);

   return fmt_str;
}


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __TRACE_MACROS_H */

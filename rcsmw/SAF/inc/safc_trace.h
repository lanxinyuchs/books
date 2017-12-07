/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2012-2012. All Rights Reserved.
 * 
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 * 
 * %CopyrightEnd%
 * 
 * ----------------------------------------------------------------------
 *  Purpose : SAF Common Library
 * ----------------------------------------------------------------------
 * 
 */

extern int safc_trace_level;

#define TRACE(level, proto ) do if (safc_trace_level > level) { printf proto; } while (0)
#define TRACE_ERROR( proto ) TRACE(0, proto)
#define TRACE1( proto ) TRACE(1, proto)
#define TRACE2( proto ) TRACE(2, proto)
#define TRACE3( proto ) TRACE(3, proto)

#define TRACE_ENTER() _safc_trace(__FILE__, __LINE__,  __func__, "Entered\n")
#define TRACE_LEAVE() _safc_trace(__FILE__, __LINE__,  __func__, "Leaved\n")

extern void _safc_trace(const char *file, unsigned int line, const char *function, const char *text);
extern void safcTraceLevel(int level);

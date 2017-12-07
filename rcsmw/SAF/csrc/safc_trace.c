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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "safc_trace.h"

int safc_trace_level = 0;

void _safc_trace(const char *file, unsigned int line, const char *function, const char *text)
{
   if (safc_trace_level > 0) {
      printf("%s(%d):%s %s\n", file, line, function, text);
   }
}

void safcTraceLevel(int level) 
{
   /* Should probably have a mutex */

   safc_trace_level = level;
}


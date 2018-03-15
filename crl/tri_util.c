/**
 *   This file contains common TRI utilities.
 *
 *   @file tri_util.c
 *
 *
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-01-27 Daniel Lefwerth
 *   Change  : First version.
 * ========================================================================
 */

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <tri_server_cmd.h>
#include <tri.h>
#include "trace_cmd.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

typedef struct Group
{
   const char *name;
   uint32_t type;
} Group;


/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

const Group groups[] = {
        {"check",        GROUP_CHECK},
        {"error",        GROUP_ERROR},
        {"enter",        GROUP_ENTER},
        {"return",       GROUP_RETURN},
        {"info",         GROUP_INFO},
        {"trace1",       GROUP_TRACE1},
        {"trace2",       GROUP_TRACE2},
        {"trace3",       GROUP_TRACE3},
        {"trace4",       GROUP_TRACE4},
        {"trace5",       GROUP_TRACE5},
        {"trace6",       GROUP_TRACE6},
        {"trace7",       GROUP_TRACE7},
        {"trace8",       GROUP_TRACE8},
        {"trace9",       GROUP_TRACE9},
        {"state_change", GROUP_STATE_CHANGE},
        {"bus_send",     GROUP_BUS_SEND},
        {"bus_receive",  GROUP_BUS_RECEIVE},
        {"rec_sig",      GROUP_REC_SIG},
        {"send_sig",     GROUP_SEND_SIG},
        {"param",        GROUP_PARAM},
        {"interface",    GROUP_INTERFACE},
        {"object",       GROUP_TRACE_OBJ},
        {"user1",        GROUP_USER1},
        {"user2",        GROUP_USER2},
        {"user3",        GROUP_USER3},
        {"user4",        GROUP_USER4},
        {NULL,           GROUP_RESERVED2},
        {NULL,           GROUP_RESERVED3},
        {NULL,           GROUP_RESERVED4},
        {NULL,           GROUP_RESERVED5},
};

/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */

/* ========================================================================= */
/**
 *   Get the group type given a trace group string.
 *
 *   @param group - a trace group
 *
 *   @return Group mask value corresponding to the trace group
 *
 */
/* ========================================================================= */
uint32_t 
tri_getGroupType(const char *group)
{
   int i;
   for (i = 0; groups[i].name; i++) {
      if (strcmp(group, groups[i].name) == 0)
         return groups[i].type;
   }
   return GROUP_RESERVED2;
}


/* ========================================================================= */
/**
 *   Support functionality to print groups corresponding to the
 *   group mask requested
 *
 *   @param      groupMask : group mask of a process
 *
 */
/* ========================================================================= */
void 
tri_printTraceGroups(uint32_t groupMask)
{
   int i, first = 1;

   for (i = 0; groups[i].name; i++) {
      if (groupMask & (1 << groups[i].type)) {
         if (!first)
            _MSG(" ");
         else
            first = 0;
         _MSG(groups[i].name);
      }
   }
}


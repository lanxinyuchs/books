/**
 *   Handling of local commands.
 * 
 *   @file
 * 
 *   This file is a part of the COLI shell.
 * 
 *   Copyright (C) 2011 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-10-15 Ravineet Singh EAB/FJP/HB
 *   Change  : Remove inclusion of exit.h.
 *
 *   Revised : 2014-06-05 Ravineet Singh EAB/FJP/HB
 *   Change  : Added command 'coli-help' for integrated shell env.
 *
 *   Revised : 2011-11-22 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <string.h>
#include <local_cmd.h>
#include <help.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 
typedef struct
{
   char  *name;
   int (* func)(int argc, char **argv);
   char *usage;
   char *short_descr;
   char *descr;
} local_cmd_t;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 
extern int  cmd_help(int argc, char **argv);

/* Update new commands in alphabetic order */

const local_cmd_t  local_cmds[] = 
{
   { "coli-help", cmd_help, HELP_USAGE, HELP_SHORT, HELP_DESCR }
};

#define NUM_CMDS (sizeof(local_cmds)/sizeof(local_cmd_t))

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */ 

/** ==================================================================== */
/** 
 *   Checks if "name" is a local command.
 * 
 *   @param name       Command name
 * 
 *   @return           1 if found, otherwise 0
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
int
local_command(char *name)
{
   int i;

   for (i = 0; i < NUM_CMDS; i++)
      if ( strcmp(name, local_cmds[i].name) == 0 )
	 return 1;

   return 0;
}

/** ==================================================================== */
/** 
 *   Returns the next local command where the name starts with expr.
 * 
 *   @param expr       Start of command name
 *   @param prev       Previous command name
 * 
 *   @return           Pointer to the next command, NULL if not found.
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
char *
next_local_command(const char *expr, const char *prev)
{
   int i;

   for (i = 0; i < NUM_CMDS; i++)
      if ( (strncmp(expr, local_cmds[i].name, strlen(expr)) == 0) &&
	   (strcmp(local_cmds[i].name, prev) > 0) )
	 return local_cmds[i].name;

   return NULL;
}

/** ==================================================================== */
/** 
 *   Runs a local command.
 * 
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 * 
 *   @return           0 if OK, otherwise non-zero (result from command)
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
int
run_local_command(int argc, char **argv)
{
   int i;

   if ( argc < 1 )
      return -1;

   for (i = 0; i < NUM_CMDS; i++)
      if ( strcmp(argv[0], local_cmds[i].name) == 0 )
	 return local_cmds[i].func(argc, argv);

   return -1;
}

/** ==================================================================== */
/** 
 *   Return the short help string for the specified command.
 * 
 *   @param name       Command name
 * 
 *   @return           The help string if found, otherwise NULL
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
char *
local_command_help(char *name)
{
   int i;

   for (i = 0; i < NUM_CMDS; i++)
      if ( strcmp(name, local_cmds[i].name) == 0 )
	 return local_cmds[i].short_descr;

   return NULL;
}

/** ==================================================================== */
/** 
 *   Return the usage string for the specified command.
 * 
 *   @param name       Command name
 * 
 *   @return           The usage string if found, otherwise NULL
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
char *
local_command_usage(char *name)
{
   int i;

   for (i = 0; i < NUM_CMDS; i++)
      if ( strcmp(name, local_cmds[i].name) == 0 )
	 return local_cmds[i].usage;

   return NULL;
}

/** ==================================================================== */
/** 
 *   Return the help description string for the specified command.
 * 
 *   @param name       Command name
 * 
 *   @return           The description string if found, otherwise NULL
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
char *
local_command_descr(char *name)
{
   int i;

   for (i = 0; i < NUM_CMDS; i++)
      if ( strcmp(name, local_cmds[i].name) == 0 )
	 return local_cmds[i].descr;

   return NULL;
}

/**
 *   Definitions for local commands.
 * 
 *   @file
 * 
 *   This file is a part of the COLI command server.
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
 *   Revised : 2011-11-22 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __LOCAL_CMD_H
#define __LOCAL_CMD_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   DATA DECLARATIONS
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
extern int local_command(char *name);

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
extern char *next_local_command(const char *expr, const char *prev);

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
extern int run_local_command(int argc, char **argv);

/** ==================================================================== */
/** 
 *   Return the short help string for the specified command.
 * 
 *   @param name       Command name
 *   @param help       The short help string
 * 
 *   @return           1 if found, otherwise 0
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
extern char *local_command_help(char *name);

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
extern char *local_command_usage(char *name);

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
extern char *local_command_descr(char *name);


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */

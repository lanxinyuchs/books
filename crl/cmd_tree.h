/**
 *   Routines for handling search trees for the commands.
 * 
 *   @file
 * 
 *   This file implements binary tree funtions in COLI. 
 *   Current clients are colid and COLI lib.
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
 *   Revised : 2014-12-10 Henrik Wallin
 *   Change  : Added argument to callback function in walk_cmds.
 *
 *   Revised : 2014-01-13 Ravineet Singh EAB/FJP/HB
 *   Change  : Merged colid/cmd_tree.h and lib/cmd_tree.h into this file.
 *
 *   Revised : 2011-10-24 Lars Jönsson EAB/FJP/TB
 *   Change  : Move to dynamical sizes of strings and removed obsolete
 *             definitions.
 *
 *   Revised : 2011-09-28 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __CMD_TREE_H
#define __CMD_TREE_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <ose.h>
#include <shell.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 
typedef struct cmd_t_ 
{
   PROCESS pid;
   char       *name;
   char       *usage;
   char       *description;
   cmd_func_t func;
} cmd_t;


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 

/** ==================================================================== */
/** 
 *   Destroys the command binary tree.
 * 
 *   @param cmd_tree   command tree root
 * 
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
extern int destroy_cmd_tab(void **cmd_tree);

/** ==================================================================== */
/** 
 *   Adds a new command to the binary tree.
 * 
 *   @param cmd_tree   command tree root
 *   @param cmd        Pointer to the new element
 * 
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
extern int add_cmd(void **cmd_tree, cmd_t *cmd);

/** ==================================================================== */
/** 
 *   Searches for a command with the specified name.
 * 
 *   @param cmd_tree   command tree root
 *   @param name       Command name
 * 
 *   @return           Pointer to the CMD or 0 is not found
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
extern cmd_t *get_cmd(void **cmd_tree, char *name);

/** ==================================================================== */
/** 
 *   Delete a command from the binary tree.
 * 
 *   @param cmd_tree   command tree root
 *   @param name       Command name
 * 
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
extern int delete_cmd(void **cmd_tree, char *name);

/** ==================================================================== */
/** 
 *   Finds the next command that starts with the specified command name.
 * 
 *   @param cmd_tree   command tree root
 *   @param name       Command name or start of a command name
 *   @param prev_name  Previous command name found. NULL if this is the
 *                     first search
 * 
 *   @return           Pointer to the CMD or 0 is not found
 *
 *   @par Globals:     
 *                     previous_name
 *
 *   This function is used for searching for all commands that starts
 *   with the specified name. One (or none) command is retrieved at each
 *   call. prev_name should be set to NULL the first time. It should be
 *   set to the previuos found command name on all other calls.
 */
/* ===================================================================== */
extern cmd_t *get_cmd_partial(void **cmd_tree, char *name, char *prev_name);



/** ==================================================================== */
/** 
 *   Walks through all commands and calls the the specified function for
 *   each command.
 * 
 *   @param cmd_tree   command tree root
 *   @param func       Callback function, name is the command name
 *   @param arg        Callback argument. Passed to func when called.
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
extern void walk_cmds(void **cmd_tree, void (* func)(cmd_t *, void *), void *arg);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __CMD_TREE_H */

/**
 *   Routines for handling search trees for the commands.
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
 *   Revised : 2014-12-10 Henrik Wallin
 *   Change  : Added argument to callback function in walk_cmds.
 *
 *   Revised : 2014-01-13 Ravineet Singh EAB/FJP/HB
 *   Change  : Merged colid/cmd_tree.c and lib/cmd_tree.c into this file.
 *
 *   Revised : 2011-11-30 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected get_cmd_partial(), which previously missed to
 *             find all commands. 
 *
 *   Revised : 2011-11-25 Lars Jönsson EAB/FJP/TB
 *   Change  : Search for parrtial commands names now supports "empty"
 *             expression (i.e. search for all commands).
 *
 *   Revised : 2011-10-21 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for retrieving all commands.
 *
 *   Revised : 2011-10-15 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for searching partial command names.
 *
 *   Revised : 2011-09-28 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#ifndef _GNU_SOURCE
  #define _GNU_SOURCE
#endif
#include <cmd_tree.h>
#include <pthread.h>
#include <search.h>
#include <stdlib.h>
#include <string.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 
/*
**  Macros for protecting critical regions when modifying the tree.
**  Currently unused, because the functions are only used by a single
**  threaded application.
*/
#define	protect_tree()		pthread_mutex_lock(&cmd_mutex)
#define	unprotect_tree()	pthread_mutex_unlock(&cmd_mutex)


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 
/*
**  Mutex for protecting critical regions when modifying the
**  command tree
*/
static pthread_mutex_t cmd_mutex = PTHREAD_MUTEX_INITIALIZER;
static char *previous_name = NULL;
static cmd_t *cmd_next;
static void (* walk_func)(cmd_t *cmd, void *arg);
static void *walk_arg;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */ 

/** ==================================================================== */
/** 
 *   Compare algorithm for binary tree search.
 * 
 *   @param pa         Element A
 *   @param pb         Element B
 * 
 *   @return           -1 if A < B, 0 if A = B, 1 A > B
 *
 *   @par Globals:     
 *                     --
 * 
 *   This algorthm compares the name of commands.
 */
/* ===================================================================== */
static int
compare_cmd(const void *pa, const void *pb)
{
  const cmd_t  *cmd1 = pa;
  const cmd_t  *cmd2 = pb;

  return strcmp(cmd1->name, cmd2->name);
}

/** ==================================================================== */
/** 
 *   Compare algorithm for binary tree search.
 * 
 *   @param pa         Element A
 *   @param pb         Element B
 * 
 *   @return           -1 if A < B, 1 if A = B, 1 A > B
 *
 *   @par Globals:     
 *                     previous_name, cmd_next
 * 
 *   This algorthm compares partial names of commands. If previuos_name
 *   is not set (NULL), the first command that matches the partial name
 *   is a hit. Otherwise the first command after previous_name that
 *   matches the partial name is a hit. It there is a hit, cmd_next is set
 *   to it. Otherwise it will remain set to NULL.
 */
/* ===================================================================== */
static int
compare_cmd_partial(const void *pa, const void *pb)
{
  const cmd_t  *cmd1 = pa;
  const cmd_t  *cmd2 = pb;
  int          len = strlen(cmd1->name);
  int          res;

  if ( (res = strncmp(cmd1->name, cmd2->name, len)) != 0 )
     return res;

  if ( !previous_name )
  {
     cmd_next = (cmd_t *)cmd2;
     return -1;
  }

  if ( (res = strcmp(previous_name, cmd2->name)) >= 0 )
     return 1;

  cmd_next = (cmd_t *)cmd2;

  return res;
}

/** ==================================================================== */
/** 
 *   Compare algorithm for binary tree search, when searching for the
 *   next command in the tree.
 * 
 *   @param pa         Element A
 *   @param pb         Element B
 * 
 *   @return           -1 if A < B, 1 if A = B, 1 A > B
 *
 *   @par Globals:     
 *                     cmd_next
 * 
 *   This algorthm compares name of previous command in cmd1 with the
 *   current command name (cmd2) in the search tree. cmd_next is set to
 *   the next command in the tree, if found. Otherwise it will remain
 *   set to NULL.
 */
/* ===================================================================== */
static int
compare_cmd_next(const void *pa, const void *pb)
{
  const cmd_t  *cmd1 = pa;
  const cmd_t  *cmd2 = pb;
  int          res;

  if ( (res = strcmp(cmd1->name, cmd2->name)) >= 0)
     return 1;
  
  if ( !cmd_next || (strcmp(cmd2->name, cmd_next->name) < 0) )
     cmd_next = (cmd_t *)cmd2;
  
  return -1;
}

/** ==================================================================== */
/** 
 *   Destroys the command binary tree.
 * 
 *   @param            -
 * 
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
int
destroy_cmd_tab(void **cmd_tree)
{
  tdestroy(*cmd_tree, free);
  *cmd_tree = NULL;

  return 0;
}

/** ==================================================================== */
/** 
 *   Adds a new command to the binary tree.
 * 
 *   @param cmd        Pointer to the new element
 * 
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
int
add_cmd(void **cmd_tree, cmd_t *cmd)
{
  cmd_t  **res;

  protect_tree();
  res = tsearch(cmd, cmd_tree, compare_cmd);
  unprotect_tree();

  if ( (res == 0) || (*res != cmd) )
     return -1;

  return 0;
}

/** ==================================================================== */
/** 
 *   Searches for a command with the specified name.
 * 
 *   @param name       Command name
 * 
 *   @return           Pointer to the CMD or 0 is not found
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
cmd_t *
get_cmd(void **cmd_tree, char *name)
{
  cmd_t  cmd;
  cmd_t  **res;

  if ( (cmd.name = malloc(strlen(name)+1)) == 0 )
    return NULL;

  strcpy(cmd.name, name);

  protect_tree();
  res = tfind(&cmd, cmd_tree, compare_cmd);
  unprotect_tree();

  free(cmd.name);

  return res ? *res : NULL;
}

/** ==================================================================== */
/** 
 *   Delete a command from the binary tree.
 * 
 *   @param name       Command name
 * 
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
int
delete_cmd(void **cmd_tree, char *name)
{
  cmd_t  *cmd;
  cmd_t  **res;

  if ( (cmd = get_cmd(cmd_tree, name)) == NULL )
    return -1;

  protect_tree();
  res = tdelete(cmd, cmd_tree, compare_cmd);
  unprotect_tree();

  if ( res == 0 )
     return -2;

  if (cmd->name) free(cmd->name);
  if (cmd->usage) free(cmd->usage);
  if (cmd->description) free(cmd->description);
  free(cmd);

  return 0;
}

/** ==================================================================== */
/** 
 *   Finds the next command after previous_name.
 * 
 *   @param prev_name  Previous command name found. NULL if this is the
 *                     first search
 * 
 *   @return           Pointer to the CMD or 0 is not found
 *
 *   @par Globals:     
 *                     previous_name
 *
 *   This function is used for seraching for all commands. One (or none)
 *   command is retrieved at each call. prev_name should be set to NULL
 *   the first time. It should be set to the previuos found command name
 *   on all other calls.
 */
/* ===================================================================== */
static cmd_t *
get_next_cmd(void **cmd_tree, char *prev_name)
{
  cmd_t  cmd;
  char   *name = "";

  if ( prev_name )
     name = prev_name;

  if ( (cmd.name = malloc(strlen(name)+1)) == 0 )
    return NULL;

  strcpy(cmd.name, name);

  protect_tree();
  cmd_next = NULL;
  previous_name = NULL;
  tfind(&cmd, cmd_tree, compare_cmd_next);
  unprotect_tree();

  free(cmd.name);

  return cmd_next;
}

/** ==================================================================== */
/** 
 *   Finds the next command that starts with the specified command name.
 * 
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
cmd_t *
get_cmd_partial(void **cmd_tree, char *name, char *prev_name)
{
  cmd_t  cmd;

  if ( strlen(name) == 0 )
     return get_next_cmd(cmd_tree, prev_name);

  if ( (cmd.name = malloc(strlen(name)+1)) == 0 )
    return NULL;

  strcpy(cmd.name, name);

  protect_tree();
  cmd_next = NULL;
  previous_name = prev_name;
  tfind(&cmd, cmd_tree, compare_cmd_partial);
  unprotect_tree();

  free(cmd.name);

  return cmd_next;
}

/** ==================================================================== */
/** 
 *   Called from the tree walk function at each node.
 * 
 *   @param nodep      Pointer to the current node
 *   @param which      When the node is reached
 *   @param depth      Current tree depth
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
static void
walk_action(const void *nodep, const VISIT which, const int depth)
{
  cmd_t  *cmd = *(cmd_t **)nodep;
  (void) depth;

  switch (which) {
  case preorder:
    break;
  case postorder:
    walk_func(cmd, walk_arg);
    break;
  case endorder:
    break;
  case leaf:
    walk_func(cmd, walk_arg);
    break;
  }
}

/** ==================================================================== */
/** 
 *   Walks through all commands and calls the the specified function for
 *   each command.
 * 
 *   @param func       Callback function, name is the command name
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     -
 */
/* ===================================================================== */
void
walk_cmds(void **cmd_tree, void (* func)(cmd_t *, void *), void *arg)
{
  protect_tree();
  walk_func = func;
  walk_arg = arg;
  twalk(*cmd_tree, walk_action);
  unprotect_tree();
}

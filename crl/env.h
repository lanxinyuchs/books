/**
 *   Routines for handling search trees for the environment varibles.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
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
 *   Revised : 2013-10-31 Stanislav Vovk
 *   Change  : Added env_get_list declaration
 *
 *   Revised : 2011-12-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Added const declaration of some parameters in function
 *             calls.
 *
 *   Revised : 2011-04-26 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __ENV_H
#define __ENV_H

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
 *   Destroys the binary tree.
 *
 *   @param envp       Indirect pointer to binary tree
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int destroy_env_tab(void **envp);

/** ==================================================================== */
/**
 *   Adds a new environment variable to the binary tree.
 *
 *   @param envp       Indirect pointer to binary tree
 *   @param name       Name of environment variable
 *   @param value      Value string for the environment variable
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int add_env(void **envp, const char *name, const char *value);

/** ==================================================================== */
/**
 *   Searches for an environment variable.
 *
 *   @param envp       Indirect pointer to binary tree
 *   @param name       Name of environment variable
 *
 *   @return           Pointer to the value string for the environment
 *                     variable or NULL is not found
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern char *find_env(void **envp, const char *name);

/** ==================================================================== */
/**
 *   Delete an environment variable from the binary trees.
 *
 *   @param envp       Indirect pointer to binary tree
 *   @param name       Name of environment variable
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int delete_env(void **envp, const char *name);

/** ==================================================================== */
/**
 *   Get a complete list of environment variable's names sorted in order
 *
 *   @param envp       pointer to binary tree
 *
 *   @return           returns a list or NULL if no btree exists
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern char * env_get_list(void *);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */

/**
 *   Routines for handling search trees for the environment varibles.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2011-2012 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-02-10 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed a warning.
 *
 *   Revised : 2013-11-12 Stanislav Vovk
 *   Change  : Calling lits_error if malloc/realloc fails
 *
 *   Revised : 2013-10-31 Stanislav Vovk
 *   Change  : Added functions to build a list with names from btree
 *
 *   Revised : 2012-02-20 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed compiler warnings.
 *
 *   Revised : 2011-12-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Added const declaration of some parameters in function
 *             calls.
 *
 *   Revised : 2011-04-26 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <lits_internal.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/*
**  Macros for protecting critical regions when modifying the
**  ENV tables
*/
#define	protect_env()		pthread_mutex_lock(&env_mutex)
#define	unprotect_env()		pthread_mutex_unlock(&env_mutex)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef struct env_t_
{
    char  *name;
    char  *val;
} env_t;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
/*
**  Mutex for protecting critical regions when modifying the
**  ENV tables
*/
static pthread_mutex_t env_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Pointer to dynamic list of names, names are added while traversing btree */
static char *list_env = NULL;

/* size of dynamic list of names */
static size_t list_size = 0;

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
 *   This algorthm compares the name of processes/threads.
 */
/* ===================================================================== */
static int
compare_env(const void *pa, const void *pb)
{
    const env_t  *env1 = pa;
    const env_t  *env2 = pb;

    return strcmp(env1->name, env2->name);
}

/** ==================================================================== */
/**
 *   Compare algorithm for binary tree search.
 *
 *   @param pa         Element A
 *   @param pb         Element B
 *
 *   @return           -1 if A < B, 1 A >= B
 *
 *   @par Globals:
 *                     --
 *
 *   This algorthm compares the name of processes/threads and is used
 *   when inserting elements into the name based tree.
 */
/* ===================================================================== */
static int
insert_env(const void *pa, const void *pb)
{
    const env_t  *env1 = pa;
    const env_t  *env2 = pb;
    int          res;

    res = strcmp(env1->name, env2->name);

    return (res != 0 ? res : 1);
}

/** ==================================================================== */
/**
 *   Removes the data element of a node in the binary tree.
 *
 *   @param nodep      Pointer to the element
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
free_node(void *nodep)
{
    env_t *env = nodep;

    if ( env->name ) free(env->name);
    if ( env->val  ) free(env->val);
    free(env);
}

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
int
destroy_env_tab(void **envp)
{
    tdestroy(*envp, free_node);
    envp = NULL;

    return 0;
}

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
int
add_env(void **envp, const char *name, const char *value)
{
    env_t  **res;
    env_t  *env;

    if ( (env = malloc(sizeof(env_t))) == 0 )
        return -1;

    /* Clear the entire env block */
    memset(env, 0, sizeof(env_t));

    env->name = malloc(strlen(name)+1);
    env->val  = malloc(strlen(value)+1);

    if ( (env->name == NULL) || (env->val == NULL) )
    {
        free_node(env);
        return -2;
    }

    strcpy(env->name, name);
    strcpy(env->val, value);

    protect_env();

    if ( (res = tfind(env, envp, compare_env)) != NULL )
    {
        ((env_t *)*res)->val = env->val;
        env->val = NULL;
        free_node(env);
        env = *res;
    }
    else
    {
        res = tsearch(env, envp, insert_env);
    }

    unprotect_env();

    if ( (res == 0) || (*res != env) )
        return -3;

    return 0;
}

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
char *
find_env(void **envp, const char *name)
{
    env_t  env;
    env_t  **res;

    env.name = (char *)name;

    protect_env();
    res = tfind(&env, envp, compare_env);
    unprotect_env();

    return res ? ((env_t *)*res)->val : NULL;
}

/** ==================================================================== */
/**
 *   Build a list of names while traversing the tree
 *
 *   @param env        pointer to a node
 *
 *   @return           --
 *
 *   @par Globals:
 *                     list_size  size of the list
 *                     list_env   pointer dynamic list with names
 */
/* ===================================================================== */
static void build_env_array(env_t *env)
{
    size_t add_size;
    char *tmp;

    add_size = strlen(env->name) + 1;
    tmp = realloc(list_env, list_size + add_size + 1);
    if (tmp == NULL)
        lits_error("Failed to allocate memory, realloc");
    list_env = tmp;
    memcpy(list_env + list_size, env->name, strlen(env->name));
    list_size += add_size;
    list_env[list_size - 1] = ' ';
    list_env[list_size] = 0;
}

/** ==================================================================== */
/**
 *   Function executed at every node and leaf in btree, in order
 *
 *   @param nodep      pointer to node
 *   @param which      order
 *   @param depth      btree depth
 *
 *   @return           --
 *
 *   @par Globals:
 */
/* ===================================================================== */
static void
list_env_btree(const void *nodep, const VISIT which, const int depth)
{
    env_t *env;

    switch(which)
    {
        case preorder:
            break;
        case postorder:
            env = *(env_t **) nodep;
            build_env_array(env);
            break;
        case endorder:
            break;
        case leaf:
            env = *(env_t **) nodep;
            build_env_array(env);
            break;
    }
}

/** ==================================================================== */
/**
 *   Get a complete list of environment variable's names sorted in order
 *
 *   @param envp       pointer to binary tree
 *
 *   @return           returns a list or NULL if no btree exists
 *
 *   @par Globals:
 *                     list_size  size of the list
 *                     list_env   pointer dynamic list with names
 */
/* ===================================================================== */
char * env_get_list(void *envp)
{
    char *tmp_bufp;

    if (envp == NULL)
        return NULL;

    protect_env();
    twalk(envp, list_env_btree);

    /* overwrite space in the end */
    list_env[list_size - 1] = 0;
    list_size = 0;

    tmp_bufp = malloc(strlen(list_env) + 1);
    if (tmp_bufp == NULL)
        lits_error("Failed to allocate memory, malloc");
    memcpy(tmp_bufp, list_env, strlen(list_env) + 1);

    free(list_env);
    list_env = NULL;
    unprotect_env();
    return tmp_bufp;
}

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
int
delete_env(void **envp, const char *name)
{
    env_t  env;
    env_t  *elem;
    env_t  **res;

    env.name = (char *)name;

    protect_env();
    res = tfind(&env, envp, compare_env);
    unprotect_env();

    if ( res == NULL )
        return -1;

    elem = (env_t *)*res;

    protect_env();
    res = tdelete(&env, envp, compare_env);
    unprotect_env();

    if ( res == NULL )
        return -2;

    free_node(elem);

    return 0;
}

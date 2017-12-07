/**
 *   Start and configuration of a liblits application.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2011-2016 by Ericsson AB. All rights reserved. The
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
 *
 *   Revised : 2016-02-12 Anette Sch�tt
 *   Change  : Updates of the 'removal of setenv usage' implementation,
 *             because Lits must support both osemain.c files using/
 *             not using setenv (client might not be rebuilt with updated
 *             osemain.c).
 *
 *   Revised : 2016-02-05 Anette Sch�tt
 *   Change  : Updates to not use setenv.
 *
 *   Revised : 2014-05-16 Stanislav Vovk
 *   Change  : Fixed two Coverity errors/warnings
 *
 *   Revised : 2013-12-02 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for static background processes.
 *
 *   Revised : 2013-10-30 Lars Jönsson EAB/FJP/TB
 *   Change  : Minor cleanup of comments.
 *
 *   Revised : 2013-10-24 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected handling of USER_START().
 *
 *   Revised : 2013-10-14 Stanislav Vovk
 *   Change  : Changed ITC initialization
 *
 *   Revised : 2013-09-26 Stanislav Vovk
 *   Change  : Removed LITS_ITC check
 *
 *   Revised : 2013-09-25 Stanislav Vovk
 *   Change  : User can now initiate ITC from osemain.con
 *
 *   Revised : 2012-04-19 Lars Jönsson EAB/FJP/TB
 *   Change  : Changed startup of main() function.
 *
 *   Revised : 2012-03-02 Zabihullah Bayat & Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for interrupt processes.
 *
 *   Revised : 2012-01-17 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed compiler warnings when redirection tables are not
 *             used.
 *
 *   Revised : 2012-01-09 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for static phantom processes, including
 *             redirection tables.
 *
 *   Revised : 2011-12-08 Lars Jönsson EAB/FJP/TB
 *   Change  : Corrected implemetation of static environment variables.
 *
 *   Revised : 2011-04-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for static environment variables.
 *
 *   Revised : 2011-04-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for static error handlers.
 *
 *   Revised : 2011-03-31 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <libgen.h>
#include <ose.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define	DEFAULT		0
#define	DEFAULT_	DEFAULT

/* Number of enironment variables which can be saved */
#define ENV_SIZE        32

/*
** Default settings for all configuration parameters, incl that
** the ones that are not handled
*/
#define SEPARATELY_LINKED(val)
#define START_OSE_HOOK1(name)
#define START_OSE_HOOK2(name)
#define REDIR_TAB_START(x)
#define REDIR_ENTRY(no, name)
#define REDIR_TAB_END(default)
#define PHT_PROC(name, block, redir)
#define PRI_PROC(name, func, stack, prio, block, time, redir)
#define BG_PROC(name, func, stack, block, time, redir)
#define INT_PROC(name, func, stack, prio, block, irq)
#define USER_START(val)
#define BLOCK_ERROR_HANDLER(name, func, stack)
#define PROC_ERROR_HANDLER(name, func, stack)
#define BLOCK_VARIABLE(name, var, value)
#define PROC_VARIABLE(name, var, value)
#define OSE_LM_SIGNAL_SIZES(_x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7)
#define OSE_LM_POOL_SIZE(_sz)
#define MAX_PROCS(_no)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
/*
** Pid variables for all static processes
*/
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir) \
  PROCESS name ## _;
#include "osemain.con"
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir)

#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir) \
  PROCESS name ## _;
#include "osemain.con"
#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir)

#undef INT_PROC
#define INT_PROC(name, func, stack, prio, block, irq) \
  PROCESS name ## _;
#include "osemain.con"
#undef INT_PROC
#define INT_PROC(name, func, stack, prio, block, irq)

#undef PHT_PROC
#define PHT_PROC(name, block, redir) \
  PROCESS name ## _;
#include "osemain.con"
#undef PHT_PROC
#define PHT_PROC(name, block, redir)

/*
** Declaration of all redirection tables
*/
#undef REDIR_TAB_START
#define REDIR_TAB_START(x) \
   static struct OS_redir_entry *x;
#include <osemain.con>
#undef REDIR_TAB_START
#define REDIR_TAB_START(x)

/*
** Environment to be set for main thread.
*/
extern char *__lits_environ[ENV_SIZE];

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
/*
** External declaration of all start hook 1
*/
#undef START_OSE_HOOK1
#define START_OSE_HOOK1(name) \
  extern void name(void);
#include "osemain.con"
#undef START_OSE_HOOK1
#define START_OSE_HOOK1(name)

/*
** External declaration of all start hook 2
*/
#undef START_OSE_HOOK2
#define START_OSE_HOOK2(name) \
  extern void name(void);
#include "osemain.con"
#undef START_OSE_HOOK2
#define START_OSE_HOOK2(name)

/*
** External declaration of all static processes
*/
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir) \
   extern OS_PROCESS(func);
#include "osemain.con"
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir)

#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir) \
   extern OS_PROCESS(func);
#include "osemain.con"
#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir)

#undef INT_PROC
#define INT_PROC(name, func, stack, prio, block, irq) \
   extern OS_PROCESS(func);
#include "osemain.con"
#undef INT_PROC
#define INT_PROC(name, func, stack, prio, block, irq)

/*
** External declaration of all static block error handlers
*/
#undef BLOCK_ERROR_HANDLER
#define BLOCK_ERROR_HANDLER(name, func, stack) \
   extern OSADDRESS func(OSBOOLEAN, OSERRCODE, OSERRCODE);
#include "osemain.con"
#undef BLOCK_ERROR_HANDLER
#define BLOCK_ERROR_HANDLER(name, func, stack)

/*
** External declaration of all static process error handlers
*/
#undef PROC_ERROR_HANDLER
#define PROC_ERROR_HANDLER(name, func, stack) \
   extern OSADDRESS func(OSBOOLEAN, OSERRCODE, OSERRCODE);
#include "osemain.con"
#undef PROC_ERROR_HANDLER
#define PROC_ERROR_HANDLER(name, func, stack)

/*
** The main() function is wrapped by using the linker option --wrap. This
** means that the declared function main() is renamed to __real_main() by
** the linker and a call to __read_main() ends up in a call to the
** function that is declared as main(). A call to main() ends up in a call
** to the function that is called __wrap_main(), which is the actual entry
** to this configuration and starter file.
**
** The reason for all this, is that liblits has to be initialized before
** the application code is started, even if it has a main() function, i.e.
** USER_START (YES) is set in the configuration file.
*/
extern int __real_main(int argc, char **argv);

extern int zzinit_os(int argc, char **argv);
extern int zzos_main(int argc, char **argv);


/** ==================================================================== */
/**
 *  Add the provided environment to be stored in the provided string
 *  alloctaed by yhe caller. The environment is copied to the tcb of the
 *  main thread.
 *
 *   @param var_name  Name of the environment variable.
 *   @param var_name  Value of the environment variable.
 *   @param string    Indirect pointer to string to copy environment to.
 *
 *   @return           0 at success, -1 at failure.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static int
add_to_env(char *var_name, char *var_value, char **string)
{
    char *env = NULL;
    char **ptr = string;
    int size = strlen(var_name) + strlen(var_value) + 2;
    int i = 0;

    if ((env = malloc(size)) == 0)
       return -1;

    memset(env, 0, size);

    snprintf(env, size, "%s=%s", var_name, var_value);
    env[size -1] = '\0';

    while(*ptr != 0)
    {
        ptr++;
        i++;
    }

    /*
    ** Last element in array shall not be used to be able to stop
    ** searching in array.
    */
    if (i >= (ENV_SIZE -1))
    {
        free(env);
        return -1;
    }

    *ptr = env;
    return 0;
}

/** ==================================================================== */
/**
 *   Frees the strings indirectly pointed to by envp.
 *
 *   @param envp       Indirect pointer to environment strings.
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
free_env_string(char **envp)
{
    char **ptr;

    for (ptr = envp; *ptr != NULL; ptr++)
    {
        free(*ptr);
        *ptr = NULL;
    }
}

/** ==================================================================== */
/**
 *   Runs all start hook 1.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: START_OSE_HOOK1 ( <functionname> )
 */
/* ===================================================================== */
static void
run_hook1(void)
{
#undef START_OSE_HOOK1
#define START_OSE_HOOK1(name) \
   name();
#include "osemain.con"
#undef START_OSE_HOOK1
#define START_OSE_HOOK1(name)
}

/** ==================================================================== */
/**
 *   Runs all start hook 2.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   @verbatim
 *
 *   Prototype: START_OSE_HOOK2 ( <functionname> )
 */
/* ===================================================================== */
static void
run_hook2(void)
{
#undef START_OSE_HOOK2
#define START_OSE_HOOK2(name) \
   name();
#include "osemain.con"
#undef START_OSE_HOOK2
#define START_OSE_HOOK2(name)
}

/** ==================================================================== */
/**
 *   Creates static redirection tables.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: REDIR_TAB_START(<redirection_table_name>)
 *              REDIR_ENTRY(<signal_no>, <process_name>)
 *              REDIR_TAB_END(<default_process_name> | DEFAULT)
 *
 *   NOTE!  Static redirection tables are only supported for phantom
 *          processes and a signal may not be redirected to a static
 *          phantom process.
 */
/* ===================================================================== */
static void
create_redir_tabs(void)
{
    int i;
    int *num;
    struct OS_redir_entry *tab;

    /*
    **  Avoid compiler warnings when no redirection tables are used!
    */
    (void)i;
    (void)num;
    (void)tab;

    /*
    **  Calculate number of entries in each table
    */
#undef REDIR_TAB_START
#undef REDIR_ENTRY
#define REDIR_TAB_START(x) \
   static int x ## _entries; \
   num = &x ## _entries; \
   *num = 1;
#define REDIR_ENTRY(no, name) \
   (*num)++;
#include <osemain.con>

    /*
    **  Allocate space for and populate the tables
    */
#undef REDIR_TAB_START
#undef REDIR_ENTRY
#undef REDIR_TAB_END
#define REDIR_TAB_START(x) \
   x = malloc(sizeof(struct OS_redir_entry) * x ## _entries); \
   tab = x; \
   i = 1;
#define REDIR_ENTRY(no, name) \
   tab[i].sig = no; \
   tab[i++].pid = name ## _;
#define REDIR_TAB_END(default) \
   tab[0].sig = i; \
   tab[0].pid = default ## _;
#include <osemain.con>
#undef REDIR_TAB_START
#undef REDIR_ENTRY
#undef REDIR_TAB_END
#define REDIR_TAB_START(x)
#define REDIR_ENTRY(no, name)
#define REDIR_TAB_END(default)
}

/** ==================================================================== */
/**
 *   ITC init user values goes to environment
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     __lits_environ
 *
 *   Prototype:
 *	MAX_PROCS(<max processes>)
 * 	OSE_LM_POOL_SIZE(<pool size>)
 * 	OSE_LM_SIGNAL_SIZES(<msg_size1>, <msg_size2>, <msg_size3>, <msg_size4>,
 * 			    <msg_size5>, <msg_size6>, <msg_size7>, <msg_size8>)
 *
 */
/* ===================================================================== */
static int lits_itc()
{
#undef MAX_PROCS
#define MAX_PROCS(_no)				\
    char __buf_mp[10] = {0};                    \
    snprintf(__buf_mp, 10, "%d", _no);		\
    if (add_to_env("MAX_PROCS", __buf_mp, &(__lits_environ[0])) == -1)	\
	return -1;
#include <osemain.con>
#undef MAX_PROCS
#define MAX_PROCS(_no)

#undef OSE_LM_POOL_SIZE
#define OSE_LM_POOL_SIZE(_sz)			      \
    char __buf_ps[10] = {0};                          \
    snprintf(__buf_ps, 10, "%d", _sz);		      \
    if (add_to_env("OSE_LM_POOL_SIZE", __buf_ps, &(__lits_environ[0])) == -1) \
	return -1;
#include <osemain.con>
#undef OSE_LM_POOL_SIZE
#define OSE_LM_POOL_SIZE(_sz)

#undef OSE_LM_SIGNAL_SIZES
#define OSE_LM_SIGNAL_SIZES(_x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7)	\
    char __sig_szs[128] = {0};                                          \
    snprintf(__sig_szs, 128, "%d,%d,%d,%d,%d,%d,%d,%d",			\
	    _x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7);			\
    if (add_to_env("OSE_LM_SIGNAL_SIZES", __sig_szs, &(__lits_environ[0])) == -1) \
	return -1;
#include <osemain.con>
#undef OSE_LM_SIGNAL_SIZES
#define OSE_LM_SIGNAL_SIZES(_x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7)

    return 0;
}

/** ==================================================================== */
/**
 *   Creates all static processes.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: PRI_PROC ( <processname>,<entrypoint>,<stacksize>,<priority>,
 *                         [<blockname> | DEFAULT], <timeslice>,
 *                         [<redirection_table_name> | NULL] )
 */
/* ===================================================================== */
static void
create_static_procs(void)
{
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir) \
   name ## _ = create_process(OS_PRI_PROC, #name, func, stack, prio, time, block, redir, 0, 0); \
   attach(NULL, name ## _);
#include "osemain.con"
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir)
}

/** ==================================================================== */
/**
 *   Creates all static background processes.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: BG_PROC ( <processname>,<entrypoint>,<stacksize>,
 *                        [<blockname> | DEFAULT], <timeslice>,
 *                        [<redirection_table_name> | NULL] )
 */
/* ===================================================================== */
static void
create_static_bg_procs(void)
{
#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir) \
   name ## _ = create_process(OS_BG_PROC, #name, func, stack, 0, time, block, redir, 0, 0); \
   attach(NULL, name ## _);
#include "osemain.con"
#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir)
}

/** ==================================================================== */
/**
 *   Creates all static interrupt processes.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: INT_PROC ( <processname>,<entrypoint>,<stacksize>,<priority>,
 *                         [<blockname> | DEFAULT], <irq> ))
 */
/* ===================================================================== */
static void
create_static_int_procs(void)
{
#undef INT_PROC
#define INT_PROC(name, func, stack, prio, block, irq) \
   name ## _ = create_process(OS_INT_PROC, #name, func, stack, prio, irq, block, 0, 0, 0); \
   attach(NULL, name ## _);
#include "osemain.con"
#undef INT_PROC
#define INT_PROC(name, func, stack, prio, block, irq)
}

/** ==================================================================== */
/**
 *   Creates all static phantom processes.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *              PHT_PROC (<processname>, <blockname> | DEFAULT,
 *                        <redirection_table> | NULL)
 */
/* ===================================================================== */
static void
create_static_phantoms(void)
{
#undef PHT_PROC
#define PHT_PROC(name, block, redir) \
   name ## _ = create_process(OS_PHANTOM, #name, NULL, 0, 0, 0, block, redir, 0, 0); \
   attach(NULL, name ## _);
#include "osemain.con"
#undef PHT_PROC
#define PHT_PROC(name, block, redir)
}

/** ==================================================================== */
/**
 *   Starts all static processes.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: PRI_PROC ( <processname>,<entrypoint>,<stacksize>,<priority>,
 *                         [<blockname> | DEFAULT], <timeslice>,
 *                         [<redirection_table_name> | NULL] )
 */
/* ===================================================================== */
static void
start_static_procs(void)
{
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir) \
   start(name ## _);
#include "osemain.con"
#undef PRI_PROC
#define PRI_PROC(name, func, stack, prio, block, time, redir)
}

/** ==================================================================== */
/**
 *   Starts all static background processes.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: BG_PROC ( <processname>,<entrypoint>,<stacksize>,
 *                        [<blockname> | DEFAULT], <timeslice>,
 *                        [<redirection_table_name> | NULL] )
 */
/* ===================================================================== */
static void
start_static_bg_procs(void)
{
#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir) \
   start(name ## _);
#include "osemain.con"
#undef BG_PROC
#define BG_PROC(name, func, stack, block, time, redir)
}

/** ==================================================================== */
/**
 *   Creates static block error handler(s).
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Only one block is available in the program which means that the
 *   block name parameter is ignored. The block ID of the one and only
 *   block is always used.
 *
 *   Prototype: BLOCK_ERROR_HANDLER(<blockname> | DEFAULT, <entrypoint>,
 *                                  <stacksize>)
 */
/* ===================================================================== */
void
create_static_block_errhs(void)
{
#undef BLOCK_ERROR_HANDLER
#define BLOCK_ERROR_HANDLER(name, func, stack) \
   create_error_handler(get_bid(current_process()), func, stack);
#include "osemain.con"
#undef BLOCK_ERROR_HANDLER
#define BLOCK_ERROR_HANDLER(name, func, stack)
}

/** ==================================================================== */
/**
 *   Creates static process error handler(s).
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: PROC_ERROR_HANDLER(<processname>, <entrypoint>,
 *                                 <stacksize>)
 */
/* ===================================================================== */
void
create_static_proc_errhs(void)
{
#undef PROC_ERROR_HANDLER
#define PROC_ERROR_HANDLER(name, func, stack) \
   create_error_handler(name ## _, func, stack);
#include "osemain.con"
#undef PROC_ERROR_HANDLER
#define PROC_ERROR_HANDLER(name, func, stack)
}

/** ==================================================================== */
/**
 *   Creates static block variables.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Only one block is available in the program which means that the
 *   block name parameter is ignored. The block ID of the one and only
 *   block is always used.
 *
 *   Prototype: BLOCK_VARIABLE(<blockname> | DEFAULT, <name>, <value>)
 */
/* ===================================================================== */
void
create_static_block_vars(void)
{
#undef BLOCK_VARIABLE
#define BLOCK_VARIABLE(name, var, value) \
  set_env(get_bid(current_process()), #var, value);
#include "osemain.con"
#undef BLOCK_VARIABLE
#define BLOCK_VARIABLE(name, var, value)
}

/** ==================================================================== */
/**
 *   Creates static process variables.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Prototype: PROC_VARIABLE(<processname>, <name>, <value>)
 */
/* ===================================================================== */
void
create_static_proc_vars(void)
{
#undef PROC_VARIABLE
#define PROC_VARIABLE(name, var, value) \
  set_env(name ## _, #var, value);
#include "osemain.con"
#undef PROC_VARIABLE
#define PROC_VARIABLE(name, var, value)
}

/** ==================================================================== */
/**
 *   Run hooks, creates static error handler, sets static environment
 *   variables and starts static processes.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   If USER_START is set to YES, the main() function of the application
 *   may call start_OSE() to start the static processes etc.
 *
 *   NOTE!  start_OSE() is not declared in any header file and is only
 *          provided for backwards compatibility.
 */
/* ===================================================================== */
void
start_OSE(void)
{
    int argc = 0;
    char **argv = NULL;
    void *ptr;

    if ( (ptr = get_env(get_bid(current_process()), "ARGC")) != NULL )
        argc = strtoul(ptr, NULL, 0);

    if ( (ptr = get_env(get_bid(current_process()), "ARGV")) != NULL )
        argv = (char **)strtoul(ptr, NULL, 0);

    create_static_procs();
    create_static_bg_procs();
    create_static_int_procs();
    create_redir_tabs();
    create_static_phantoms();
    create_static_block_errhs();
    create_static_proc_errhs();
    create_static_block_vars();
    create_static_proc_vars();
    run_hook2();
    start_static_procs();
    start_static_bg_procs();

    exit(zzos_main(argc, argv));
}

/** ==================================================================== */
/**
 *   Main entry point
 *
 *   @param argc       Standard main argc
 *   @param argv       Standard main argv
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     __lits_environ
 *
 *   see comment at __real_main() declaration in this file
 *
 *   Calls start_OSE() to start all static processes etc. If USER_START is
 *   set to YES, the main() function of the application is called instead,
 *   which may call start_OSE() to start the static processes etc.
 *
 *   NOTE!  start_OSE() is not declared in any header file and is only
 *          provided for backwards compatibility.
 *
 *   Prototype: USER_START ( YES | NO )
 */
/* ===================================================================== */
int
__wrap_main(int argc, char **argv)
{
    char __str[2*sizeof(unsigned long) + 3];

    lits_itc();
    if ( zzinit_os(argc, argv) != 0 )
        return -1;

    run_hook1();

    /*
    ** Save argc and argv for later use in start_OSE()
    */
    snprintf(__str, sizeof(__str), "0x%lx", (unsigned long)argc);
    if (add_to_env("ARGC", __str, &(__lits_environ[0])) == -1)
       return -1;
    snprintf(__str, sizeof(__str), "0x%lx", (unsigned long)argv);
    if (add_to_env("ARGV", __str, &(__lits_environ[0])) == -1)
       return -1;

#undef USER_START
#define USER_START(val) val
#define	YES exit(__real_main(argc, argv));
#define	NO  start_OSE();
#include "osemain.con"
#undef USER_START
#undef YES
#undef NO
#define USER_START(val)

    /* Clean up */
    free_env_string(__lits_environ);

    return 0;
}

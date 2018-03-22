/**
 *   Test of environment variables.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
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
 *   Change  : Added tests for get_env_list
 *
 *   Revised : 2011-12-14 Lars Jönsson EAB/FJP/TB
 *   Change  : Added tests storing pointers in environment variables.
 *
 *   Revised : 2011-04-28 Lars Jönsson EAB/FJP/TB
 *   Change  : Added tests for inherited Linux environment.
 *
 *   Revised : 2011-04-26 Lars Jönsson EAB/FJP/TB
 *   Change  : Updated tests for separate environment areas for each
 *             process and block.
 *
 *   Revised : 2011-04-13 Lars Jönsson EAB/FJP/TB
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
#include <ose.h>

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
extern	PROCESS	worker_;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Checks the result of environment variables handling
 *
 *   @param str        String to precede the result string
 *   @param id         Process or Block ID to the variable
 *   @param name       Name of variable to check
 *   @param expected   Expected value of the variable, NULL if it should
 *                     not exist
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_env_var_handling(char *str, PROCESS id, char *name, char *expected)
{
    int  passed = 1;
    char *value;

    value = get_env(id, name);

    if ( expected == NULL )
        passed &= value == expected;
    else if ( value != NULL )
        passed &= strcmp(expected, value) == 0;
    else
        passed = 0;

    printf("%s [%s=%s]: %s\n", str, name, value, passed ? "OK" : "Fail");

    if ( value != NULL )
        free_buf((union SIGNAL **)&value);

    return passed;
}

/** ==================================================================== */
/**
 *   Checks the result of environment pointer variables handling
 *
 *   @param str        String to precede the result string
 *   @param id         Process or Block ID to the variable
 *   @param name       Name of variable to check
 *   @param expected   Expected value of the variable, NULL if it should
 *                     not exist
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_envp_var_handling(char *str, PROCESS id, char *name, char *expected)
{
    int  passed = 1;
    char *value;

    value = (char *)get_envp(id, name);

    if ( expected == NULL )
        passed &= value == expected;
    else if ( value != NULL )
        passed &= strcmp(expected, value) == 0;
    else
        passed = 0;

    printf("%s [%s=%s]: %s\n", str, name, value, passed ? "OK" : "Fail");

    return passed;
}

/** ==================================================================== */
/**
 *   Checks the result of environment variables handling
 *
 *   @param str        String to precede the result string
 *   @param id         Process or Block ID to the variable
 *   @param name       Name of variable to check
 *   @param expected   Expected value of the variable, NULL if it should
 *                     not exist
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_env_list_handling(char *str, PROCESS id, char *fname, char *exp_list)
{
    int  passed = 1;
    char *list;

    list = get_env_list(id, fname);

    if (strcmp(exp_list, list) != 0)
        passed = 0;

    printf("%s:\n - Expected List: %s\n - Fetched  List: %s\n - Result: %s\n",
           str, exp_list, list, passed ? "OK" : "Fail");

    free_buf((union SIGNAL **) &list);
    return passed;
}

/** ==================================================================== */
/**
 *   Worker process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(worker)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;

    while(1)
    {
        sig = receive(selAll);
        send(&sig, sender(&sig));
    }
}

/** ==================================================================== */
/**
 *   Main test process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   All environment variables are actually set on block level and share
 *   among all processes.
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    SIGSELECT     sel_attach[] = {1, OS_ATTACH_SIG};
    union SIGNAL  *sig;
    int           passed = 1;
    char          *name, *name2, *name3;
    char          *val;
    PROCESS       id;
    char          exp_list[96] = {0};

    /*
    **  Tests:
    **  - Check inherited Linux environment
    **  - Check static process and block variables
    **  - Replace static process and block variables
    **  - Remove static process and block variables
    **  - Add dynamic process and block variables
    */

    /*
    **  The Linux environment variable is set in the Makefile
    */
    name = "LITS";
    val  = "Linux";
    id   = get_bid(current_process());

    passed &= check_env_var_handling("Check inherited Linux environment variable from block",
                                     id, name, val);

    name = "LITS";
    val  = NULL;
    id   = current_process();

    passed &= check_env_var_handling("Check inherited Linux environment variable from process",
                                     id, name, val);

    name = "static-block";
    val  = "statval-block";
    id   = get_bid(current_process());

    passed &= check_env_var_handling("Check static block variable", id, name, val);

    name = "static-proc";
    val  = "statval-proc";
    id   = current_process();

    passed &= check_env_var_handling("Check static process variable", id, name,
                                     val);

    name = "static-block";
    val  = "dynval-block";
    id   = get_bid(current_process());

    set_env(id, name, val);
    passed &= check_env_var_handling("Replace static block variable", id, name,
                                     val);

    name = "static-proc";
    val  = "dynval-proc";
    id   = current_process();

    set_env(id, name, val);
    passed &= check_env_var_handling("Replace static process variable", id, name,
                                     val);

    name = "static-block";
    val  = NULL;
    id   = get_bid(current_process());

    set_env(id, name, val);
    passed &= check_env_var_handling("Remove static block variable", id, name, val);

    name = "static-proc";
    val  = NULL;
    id   = current_process();

    set_env(id, name, val);
    passed &= check_env_var_handling("Remove static process variable", id, name,
                                     val);

    name = "dynamic-block";
    val  = "dynval-block";
    id   = get_bid(current_process());

    set_env(id, name, val);
    passed &= check_env_var_handling("Create dynamic block variable", id, name,
                                     val);

    name = "dynamic-proc";
    val  = "dynval-proc";
    id   = current_process();

    set_env(id, name, val);
    passed &= check_env_var_handling("Create dynamic process variable", id, name,
                                     val);

    name = "dynamic-block";
    val  = NULL;
    id   = current_process();

    passed &= check_env_var_handling("Check block variable from process", id, name,
                                     val);

    name = "dynamic-proc";
    val  = NULL;
    id   = get_bid(current_process());

    passed &= check_env_var_handling("Check process variable from block", id, name,
                                     val);

    name = "dynamic-proc";
    val  = NULL;
    id   = worker_;

    passed &= check_env_var_handling("Check process variable from another process",
                                     id, name, val);

    name = "temp-proc";
    val  = "temp-val";
    name2 = "temp-proc2";
    name3 = "temp-proc3";
    id   = create_process(OS_PRI_PROC, "temp_proc", worker, 2000, 15, (OSTIME)0, 0,
                          0, 0, 0);

    attach(NULL, id);
    start(id);

    passed &= check_env_list_handling("Checking returned env list in process without "
                                      "any variables [process]",
                                      id, NULL, "");

    set_env(id, name, val);
    set_env(id, name2, val);
    set_env(id, name3, val);
    passed &= check_env_var_handling("Check dynamic temporary process", id, name,
                                     val);

    sprintf(exp_list, "%s %s %s", name, name2, name3);
    passed &= check_env_list_handling("Checking returned complete env list [process]",
                                      id, NULL, exp_list);

    /*
    **  Just verifying that killing a process with environment variables
    **  does not generate any fatal errors
    */
    kill_proc(id);
    sig = receive_w_tmo(1000, sel_attach);
    printf("Killing temporary process with enviroment variables: %s\n",
           sig != NIL ? "OK" : "Fail");
    passed &= sig != NIL,

              name = "pointer-var";
    val  = "pointer variable string";
    id   = current_process();
    set_envp(0, name, (OSADDRESS)val);
    passed &= check_envp_var_handling("Check pointer variable", id, name, val);

    name = "non-pointer-var";
    val  = "non-pointer variable string";
    id   = current_process();
    set_env(id, name, val);
    passed &= check_envp_var_handling("Check non-pointer variable", id, name, NULL);


    name = "static-block1";
    val  = "dynval-block1";
    id   = get_bid(current_process());
    set_env(id, name, val);

    name2 = "static-block2";
    val  = "dynval-block2";
    set_env(id, name2, val);

    name3 = "static-block3";
    val  = "dynval-block3";
    set_env(id, name3, val);

    sprintf(exp_list, "%s %s", name2, name3);
    passed &= check_env_list_handling("Checking returned list [block]", id, name,
                                      exp_list);
    passed &= check_env_list_handling("Checking non-existing variable [block]", id,
                                      "static-block31", "");
    passed &= check_env_list_handling("Checking list for last element [block]",
                                      id, name3, "");

    name = "static-proc1";
    val  = "dynval-proc1";
    id   = current_process();
    set_env(id, name, val);

    name2 = "static-proc2";
    val  = "dynval-proc2";
    set_env(id, name2, val);

    sprintf(exp_list, "%s", name2);
    passed &= check_env_list_handling("Checking returned list [process]", id, name,
                                      exp_list);
    passed &= check_env_list_handling("Checking non-existing variable [process]",
                                      id,
                                      "static-proc31", "");
    passed &= check_env_list_handling("Checking list for last element [process]",
                                      id, name2, "");

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

/**
 *   Test of warning free free_buf_type system call.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2013-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-12-10 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
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

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */


/** ==================================================================== */
/**
 *   Main test process
 *   Very simple test, since we have -Wall and -Werror, this file
 *   won't compile if there is a warning. Also verifies that
 *   free_buf_type is actually implemented.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     order[]
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    char *ne;
    char *e;
    char *n = "hellos___";
    PROCESS bl;

    bl = get_bid(current_process());

    ne = get_env(bl, n);
    set_env(bl, n, "hi");
    e = get_env(bl, n);

    if(ne != NULL)
    {
        free_buf_type( char *, &ne);
    }

    if(e != NULL)
    {
        free_buf_type( char *, &e);
    }

    exit(0);
}

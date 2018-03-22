/**
 *   Test server.
 *
 *   @file
 *
 *   This file is a part of the test programs for the lits (Legacy IPC and
 *   Task Support) lib.
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
 *   Revised : 2015-03-27 Ravineet Singh
 *   Change  : Removed LINX dependency. Removed trailing whitespaces.
 *
 *   Revised : 2011-04-05 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ose.h>
#include "client_server.sig"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define SERVER_NAME "server"

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
 *   Server process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(server_proc)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    PROCESS       pid;

    pid = current_process();
    printf("Server PID: %u (0x%08x)\n", pid, pid);

    while (1)
    {
        sig = receive(selAll);
        pid = sender(&sig);
        free_buf(&sig);
        sig = alloc(sizeof(struct pong_sig), CL_SER_PONG);
        send(&sig, pid);
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
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    start(create_process(OS_PRI_PROC, SERVER_NAME, server_proc, 2000, 15,
                         (OSTIME)0, 0,0, 0, 0));

    stop(current_process());
}

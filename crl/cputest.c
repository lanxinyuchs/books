/**
 *   Test of CPU calls handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
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
 *   Revised : 2015-03-30 Ravineet Singh EAB/FJP/HB
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

/** ==================================================================== */
/**
 *   Main test process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    cpuid_t cpuid;
    cpuid_t nrcpus;
    int passed = 0;

    nrcpus = zzose_num_cpus();
    cpuid = zzose_cpu_id();
    printf("# of CPU : %d\n", nrcpus);
    printf("Running on core %d\n", cpuid);

    if(nrcpus > 0 && cpuid < nrcpus)
    {
        passed = 1;
    }

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

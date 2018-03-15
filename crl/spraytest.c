/**
 *   Spray test.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2011-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-04-011 Ravineet Singh  EAB/FJP/HB
 *   Change  : Added header and template.
 *             Renamed litsspray-> straytest.
 *
 *   Revised : 2014-03-04 Sridhar Karnam  EAB/FJP/HO
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "ose.h"
#include "osetypes.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "l_spray.sig"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define	RET_SUCCESS	0
#define	RET_WARNING	1

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
union SIGNAL
{
    SIGSELECT sigNo;
    struct litsSprayTest litsSprayTest;
    struct litsSprayTestR litsSprayTestR;
    struct litsSprayTestStart litsSprayTestStart;
    struct litsSprayTestReady litsSprayTestReady;
};


OSENTRYPOINT lits_sprayMaster_proc;
OSENTRYPOINT lits_spraySlave_proc;

static const char sprayUsage_z[] =
    "litsspray [c <count>] [d <delay>] [l <length>]";

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */


int
main (int argc, char **argv)
{
    char string_ac[100];
    int looptime;
    U32 loops_U32 = 1000;
    U32 dataSize_U32 = 0;
    OSTIME delayTime = 0;
    union SIGNAL *sigSend_pn, *sigRec_pn;
    SIGSELECT litsSprayTestReady_a[] = {2, LITS_SPRAY_TEST_READY, OS_ATTACH_SIG};
    PROCESS slavePid, masterPid;
    OSATTREF attRef;
    const char spraySlave_z[] = "lits_spraySlave_proc";
    FILE *output = stdout;
    int i;
    for (i = 1; i < argc; i++)
    {
        if(strcmp(argv[i],"l")== 0)
        {
            if (i < argc-1)
            {
                dataSize_U32 = strtol(argv[++i],NULL,0);
                if (dataSize_U32 > 65000)
                {
                    fprintf(output, "Max size = 65000.\n");
                    return RET_WARNING;
                }
            }
            else
            {
                fprintf(output, "Value missing for option: %s \n", argv[i]);
                fprintf(output, "Usage: %s \n", sprayUsage_z);
                return RET_WARNING;
            }
        }
        else if(strcmp(argv[i],"c")== 0)
        {
            if (i < argc-1)
            {
                loops_U32 = strtol(argv[++i],NULL,0);
            }
            else
            {
                fprintf(output, "Value missing for option: %s \n", argv[i]);
                fprintf(output, "Usage: %s \n", sprayUsage_z);
                return RET_WARNING;
            }
        }
        else if(strcmp(argv[i],"d")== 0)
        {
            if (i < argc-1)
            {
                delayTime = strtol(argv[++i],NULL,0);
            }
            else
            {
                fprintf(output, "Value missing for option: %s \n", argv[i]);
                fprintf(output, "Usage: %s \n", sprayUsage_z);
                return RET_WARNING;
            }
        }
        else
        {
            fprintf(output, "Unknown option: %s \n", argv[i]);
            fprintf(output, "Usage: %s \n", sprayUsage_z);
            return RET_WARNING;
        }
    }

    slavePid = create_process(OS_PRI_PROC,
                              "lits_spraySlave_proc",
                              lits_spraySlave_proc,
                              (OSADDRESS) 1000,
                              (OSPRIORITY) 16,
                              (OSTIME) 0,
                              (PROCESS) 0,
                              (struct OS_redir_entry *) 0,
                              (OSVECTOR) 0,
                              (OSUSER) 0);
    start(slavePid);
    /* local processor */
    (void) strcpy(string_ac, spraySlave_z);
    hunt(string_ac, 0, &slavePid, NULL);
    if (slavePid == 0)
    {
        fprintf(output,"Error: process %s could not be reached.\n",
                string_ac);
        return RET_WARNING;
    }
    attRef = attach(NULL, slavePid);
    /* send request to start measure */
    sigSend_pn = alloc(sizeof(struct litsSprayTestStart),
                       LITS_SPRAY_TEST_START);
    sigSend_pn->litsSprayTestStart.loops_U32 = loops_U32;
    sigSend_pn->litsSprayTestStart.slavePid = slavePid;
    sigSend_pn->litsSprayTestStart.dataSize_U32 = dataSize_U32;
    sigSend_pn->litsSprayTestStart.delayTime = delayTime;

    fprintf(output,"OS signal loop test starting.\n");
    fprintf(output,"loops = %d \n" ,loops_U32);
    fprintf(output,"data size = %d bytes\n" ,dataSize_U32);
    fprintf(output,"delay in loop = %d ms\n", delayTime);
    fprintf(output,"signal loops in process: %s\n", string_ac);

    masterPid = create_process(OS_PRI_PROC,
                               "lits_sprayMaster_proc",
                               lits_sprayMaster_proc,
                               (OSADDRESS) 2048,
                               (OSPRIORITY) 23,
                               (OSTIME) 0,
                               (PROCESS) 0,
                               (struct OS_redir_entry *) 0,
                               (OSVECTOR) 0,
                               (OSUSER) 0);

    start(masterPid);

    /* order start of test */
    send(&sigSend_pn,masterPid);

    /* Wait for result */
    sigRec_pn = receive(litsSprayTestReady_a);

    switch (sigRec_pn->sigNo)
    {

        case OS_ATTACH_SIG:
            free_buf(&sigRec_pn);
            kill_proc(masterPid);
            fprintf(output, "Lost contact with process %s, test aborted\n",
                    spraySlave_z);
            return RET_WARNING;

        case LITS_SPRAY_TEST_READY:
            /* print result */
            fprintf(output,"The test took %lu milliseconds.\n",
                    sigRec_pn->litsSprayTestReady.time / 1000);
            looptime = sigRec_pn->litsSprayTestReady.time / loops_U32;
            fprintf(output,"Signalling loop time is then %d microseconds.\n",
                    looptime);
            kill_proc(masterPid);
            free_buf(&sigRec_pn);
            detach(&attRef);
            return RET_SUCCESS;

        default:
            free_buf(&sigRec_pn);
            break;
    } /* switch */

    return RET_SUCCESS;

}



OS_PROCESS(lits_sprayMaster_proc)
{
    union SIGNAL *sigSend_pn, *sigRec_pn, *sigSendTest_pn, *sigRecTest_pn;
    static SIGSELECT litsSprayTestStart_a[] = {1, LITS_SPRAY_TEST_START};
    static SIGSELECT litsSprayTestR_a[] = {2, LITS_SPRAY_TEST_R, OS_ATTACH_SIG};
    OSTICK startTick, stopTick;
    OSTICK startMicro, stopMicro;
    OSTIME delayTime;
    OSBUFSIZE size;
    int loop,i;
    PROCESS pid;


    /* Outer loop to receive command to start perfmeter */
    for (;;)
    {
        sigRec_pn = receive(litsSprayTestStart_a);

        /* attach to our main (the spray command) */
        attach(NULL, sender(&sigRec_pn));

        loop =  (int) sigRec_pn->litsSprayTestStart.loops_U32;
        size = (OSBUFSIZE) sigRec_pn->litsSprayTestStart.dataSize_U32 +
               sizeof(struct litsSprayTest);
        pid = sigRec_pn->litsSprayTestStart.slavePid;
        delayTime = sigRec_pn->litsSprayTestStart.delayTime;

        if ( delayTime == 0)
        {
            /* use a version of inner loop without any delay system calls */
            startTick = get_systime(&startMicro);
            for (i = 0; i < loop; i++)
            {
                sigSendTest_pn = alloc(size,LITS_SPRAY_TEST );
                sigSendTest_pn->litsSprayTest.sigSize_U32 = size;
                send(&sigSendTest_pn, pid);
                sigRecTest_pn = receive(litsSprayTestR_a);

                if (sigRecTest_pn->sigNo == OS_ATTACH_SIG)
                    kill_proc(current_process());

                free_buf(&sigRecTest_pn);
            }
            stopTick = get_systime(&stopMicro);
        }
        else
        {
            /* inner loop with delay system call */
            startTick = get_systime(&startMicro);
            for (i = 0; i < loop; i++)
            {
                /* pid = sender(&sigRec_pn); */

                sigSendTest_pn = alloc(size,LITS_SPRAY_TEST );
                sigSendTest_pn->litsSprayTest.sigSize_U32 = size;
                send(&sigSendTest_pn, pid);
                sigRecTest_pn = receive(litsSprayTestR_a);

                if (sigRecTest_pn->sigNo == OS_ATTACH_SIG)
                    kill_proc(current_process());

                free_buf(&sigRecTest_pn);
                delay(delayTime);
            }
            stopTick = get_systime(&stopMicro);
        }

        sigSend_pn = alloc(sizeof(struct litsSprayTestReady),
                           LITS_SPRAY_TEST_READY);
        if (stopTick >= startTick)
        {
            sigSend_pn->litsSprayTestReady.time =
                (stopTick - startTick) * system_tick() + stopMicro - startMicro;
        }
        else /* Wrap around */
        {
            sigSend_pn->litsSprayTestReady.time =
                (stopTick + (0xffffffff - startTick + 1)) * system_tick()
                + stopMicro - startMicro;
        }
        send(&sigSend_pn, sender(&sigRec_pn));
        free_buf(&sigRec_pn);

    }

}

OS_PROCESS(lits_spraySlave_proc)
{
    union SIGNAL *sigSendTest_pn, *sigRecTest_pn;
    SIGSELECT litsSprayTest_a[] = {1, LITS_SPRAY_TEST};
    PROCESS pid;

    for (;;)
    {
        sigRecTest_pn = receive(litsSprayTest_a);
        pid = sender(&sigRecTest_pn);
        sigSendTest_pn = alloc(sigRecTest_pn->litsSprayTest.sigSize_U32,
                               LITS_SPRAY_TEST_R);
        free_buf(&sigRecTest_pn);
        send(&sigSendTest_pn, pid);
    }
}

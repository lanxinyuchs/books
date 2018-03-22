/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file
 *   @version @(#) ClearCase ID: /vobs/cpp/src/babs_ss/epb1_bl/epb1_jpre_swu/src/lmspray.c
 *				 /main/cppdev/0 #
 *
 *   [Enter a longer description about the file. More than one line
 *   can be used, but leave an empty line before the copyright line.]
 *
 *
 *   Copyright (C) 2010 by Ericsson AB. All rights reserved. The
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
 *   Revised : [2011-01-14 Krishna Vadrevu]
 *   Change  : First version based on
 *	       /vobs/cello/babs/BABS_CRX901142_1/JPRETEST_CNX9010682/src/
 *	       swu/lmspray.c@@/main/cppdev/1
 *
 *   Revised : [2013-01-15 Magnus Lindberg]
 *   Change  : lmspray adapted to use itc, hence it is now itcspray
 *
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "ose.h"
#include "shell.h"

#include "itc.h"
/*
 ****************************************************************************
 * 3  SIGNAL INCLUDE FILES
 ****************************************************************************
 */

#include "itcspray.sig"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
  uint32_t sigNo;
  struct ItcSprayPrio ItcSprayPrio;
  struct ItcSprayTest ItcSprayTest;
  struct ItcSprayTestR ItcSprayTestR;
  struct ItcSprayTestStart ItcSprayTestStart;
  struct ItcSprayTestReady ItcSprayTestReady;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

int itcsprayCmd_i(int argc,char ** argv);

OSENTRYPOINT Itc_sprayMaster_proc;
static const char sprayUsage_z[] = "itcspray [-p <linkhandler>] [-c <count>] [-d <delay>] [-l <length>] [-b <burst>] [-m <mprio>] [-s <sprio>]";

/** ==================================================================== */
/**
 *  Adds the command itcspray to the OSE shell.
 *  A call to this functin should be placed in a  START_OSE_HOOK2.
 *
 *  @param    : As specified in shell documentation
 *
 *  @return
 *
 */
/* ===================================================================== */

void
addShellCmdItcSpray_v(void)
{

 shell_add_cmd("itcspray","itcspray [-p <linkhandler>]       Performance in OS signalling\n           [-c <count>] [-d <delay>]\n           [-l <length>] [-b <burst>] [-m <mprio>] [-s <sprio>]" ,
               "", itcsprayCmd_i);
}

/** ==================================================================== */
/**
 *  Shell command that implements perfomance messurements.
 *
 *  @param    - As specified in shell documentation
 *
 *  @return   - Integer as defined in shell documentation
 *
 */
/* ===================================================================== */

int
itcsprayCmd_i(int argc,
             char ** argv)
{
   char *linkHandler_pc      = NULL;  /* -p option */
   uint32_t dataSize_U32     = 0;     /* -l option */
   uint32_t loops_U32        = 1000;  /* -c option */
   OSTIME delayTime          = 0;     /* -d option */
   uint32_t burst_U32        = 1;     /* -b option */
   uint32_t masterPrio_U32   = 25;    /* -m option */
   uint32_t slavePrio_U32    = 25;    /* -s option */

   char string_ac[100], tmp_str[100];
   int i, looptime;
   union itc_msg *sigSend_pn, *sigRec_pn, *sigHunt_pn;
   uint32_t itcloc[] = {1, ITC_SPRAY_TEST};
   uint32_t ItcSprayTestReady_a[] = {2, ITC_SPRAY_TEST_READY, ITC_MONITOR_DEFAULT_NO};
   itc_id_t slave_mbox, master_mbox, my_mbox_id, tmp_mbox_id;
   PROCESS masterPid;
   itc_monitor_id_t monRef;

   const char spraySlave_z[] = "Itc_spraySlave_proc";

   FILE *output = stdout;

  /*
   * Check number of arguments.
   */
  if (argc > 15)
  {
    fprintf(output, "Usage: %s \n", sprayUsage_z);
    return  RET_WARNING;
  }

  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-')
	{
	  switch (argv[i][1])
	    {
	    case 'p':
	      {
		/* Make sure there is a value supplied for the option */

		if (i < argc-1) /* i.e. option is not last */
	          linkHandler_pc = argv[++i];
		else
		  {
		    fprintf(output, "Value missing for option: %s \n", argv[i]);
		    fprintf(output, "Usage: %s \n", sprayUsage_z);
		    return RET_WARNING;
		  }
		break;
	      }
	    case 'l':
	      {
		/* Make sure there is a value supplied for the option */

		if (i < argc-1) /* i.e. option is not last */
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
		break;
	      }
	    case 'c':
	      {
		/* Make sure there is a value supplied for the option */

		if (i < argc-1) /* i.e. option is not last */
	          loops_U32 = strtol(argv[++i],NULL,0);
		else
		  {
		    fprintf(output, "Value missing for option: %s \n", argv[i]);
		    fprintf(output, "Usage: %s \n", sprayUsage_z);
		    return RET_WARNING;
		  }
		break;
	      }
	    case 'd':
	      {
		/* Make sure there is a value supplied for the option */

		if (i < argc-1) /* i.e. option is not last */
	          delayTime = strtol(argv[++i],NULL,0);
		else
		  {
		    fprintf(output, "Value missing for option: %s \n", argv[i]);
		    fprintf(output, "Usage: %s \n", sprayUsage_z);
		    return RET_WARNING;
		  }
		break;
	      }
	    case 'b':
	      {
		/* Make sure there is a value supplied for the option */

		if (i < argc-1) /* i.e. option is not last */
	          burst_U32 = strtol(argv[++i],NULL,0);
		else
		  {
		    fprintf(output, "Value missing for option: %s \n", argv[i]);
		    fprintf(output, "Usage: %s \n", sprayUsage_z);
		    return RET_WARNING;
		  }
		break;
	      }
	    case 'm':
	      {
		/* Make sure there is a value supplied for the option */

		if (i < argc-1) /* i.e. option is not last */
                {
                   masterPrio_U32 = strtol(argv[++i],NULL,0);
                   if (masterPrio_U32 > 31)
                   {
                      fprintf(output, "allowed prio = [0..31].\n");
                      return RET_WARNING;
                   }
                }
		else
		  {
		    fprintf(output, "Value missing for option: %s \n", argv[i]);
		    fprintf(output, "Usage: %s \n", sprayUsage_z);
		    return RET_WARNING;
		  }
		break;
	      }
	    case 's':
	      {
		/* Make sure there is a value supplied for the option */

		if (i < argc-1) /* i.e. option is not last */
                {
                   slavePrio_U32 = strtol(argv[++i],NULL,0);
                   if (slavePrio_U32 > 31)
                   {
                      fprintf(output, "allowed prio = [0..31].\n");
                      return RET_WARNING;
                   }
                }
		else
		  {
		    fprintf(output, "Value missing for option: %s \n", argv[i]);
		    fprintf(output, "Usage: %s \n", sprayUsage_z);
		    return RET_WARNING;
		  }
		break;
	      }
	    default:
	      {
		/* Unknown option */
		fprintf(output, "Unknown option: %s \n", argv[i]);
		fprintf(output, "Usage: %s \n", sprayUsage_z);
		return RET_WARNING;
	      }
	    }
	}
      else
	{
	  fprintf(output, "Usage: %s \n", sprayUsage_z);
	  return  RET_WARNING;
	}
    }


  if ( (loops_U32 % burst_U32) != 0 )
  {
     fprintf(output,"Error: loop count must be a multiple of burst size.\n");
     return RET_WARNING;
  }

  if (linkHandler_pc == NULL)
  {
      /* local processor */
      (void) strcpy(string_ac, spraySlave_z);
  }
  else
  {
     /* get remote pid */
     (void) strcpy(string_ac, linkHandler_pc);
     (void) strcat(string_ac, "/");
     (void) strcat(string_ac, spraySlave_z);
  }


  /* Find and create a new unique mbox id */
  i=0;
  do
  {
     i++;
     sprintf(tmp_str, "itcspray_cmd_%d", i);
     tmp_mbox_id = itc_locate(tmp_str);
  } while(tmp_mbox_id != ITC_NO_ID);

  my_mbox_id = itc_create_mailbox(tmp_str, 0);
  if(my_mbox_id == ITC_NO_ID)
  {
     printf("Can not create my own mailbox\n");
     return RET_WARNING;
  }

  /* hunt for slave process */
  sigSend_pn = itc_alloc(sizeof( struct ItcSprayTest ),ITC_SPRAY_TEST);
  itc_locate_async(string_ac, &sigSend_pn, ITC_MY_MBOX);
  sigSend_pn = itc_receive(itcloc, 15000, ITC_FROM_ALL);
  if (!sigSend_pn)
  {
     fprintf(output,"Error: process %s could not be reached.\n",
             string_ac);
     return RET_WARNING;
  }
  slave_mbox = itc_sender(sigSend_pn);
  itc_free(&sigSend_pn);

  monRef = itc_monitor(slave_mbox, NULL);

  /* set prio for slave process */
  sigSend_pn = itc_alloc(sizeof(struct ItcSprayPrio), ITC_SPRAY_PRIO);
  sigSend_pn->ItcSprayPrio.slavePrio_U32 = slavePrio_U32;
  itc_send(&sigSend_pn, slave_mbox, ITC_MY_MBOX);

  /* send request to start measure */
  sigSend_pn = itc_alloc(sizeof(struct ItcSprayTestStart), ITC_SPRAY_TEST_START);
  sigSend_pn->ItcSprayTestStart.loops_U32 = loops_U32;
  sigSend_pn->ItcSprayTestStart.slave_mbox = slave_mbox;
  sigSend_pn->ItcSprayTestStart.dataSize_U32 = dataSize_U32;
  sigSend_pn->ItcSprayTestStart.delayTime = delayTime;
  sigSend_pn->ItcSprayTestStart.burst_U32 = burst_U32;


  fprintf(output,"OS signal loop test starting.\n");
  fprintf(output,"spray master prio = %d \n" ,masterPrio_U32);
  fprintf(output,"spray slave  prio = %d \n" ,slavePrio_U32);
  fprintf(output,"loops = %d \n" ,loops_U32);
  fprintf(output,"data size = %d bytes\n" ,dataSize_U32);
  fprintf(output,"burst = %d \n" ,burst_U32);
  fprintf(output,"delay in loop = %d ms\n", delayTime);
  fprintf(output,"signal loops in process: %s\n", string_ac);


  masterPid = create_process(OS_PRI_PROC,
			     "Itc_sprayMaster_proc",
			     Itc_sprayMaster_proc,
			     (OSADDRESS) 2048,
			     (OSPRIORITY) masterPrio_U32,
			     (OSTIME) 0,
			     (PROCESS) 0,
			     (struct OS_redir_entry *) 0,
			     (OSVECTOR) 0,
			     (OSUSER) 0);
  start(masterPid);

  sigHunt_pn = itc_alloc(sizeof( struct ItcSprayTest ),ITC_SPRAY_TEST);
  itc_locate_async("Itc_sprayMaster_proc", &sigHunt_pn, ITC_MY_MBOX);
  sigHunt_pn = itc_receive(itcloc, ITC_NO_TMO, ITC_FROM_ALL);
  master_mbox = itc_sender(sigHunt_pn);
  itc_free(&sigHunt_pn);

  /* order start of test */
  itc_send(&sigSend_pn, master_mbox, ITC_MY_MBOX);

  /* Wait for result */
  sigRec_pn = itc_receive(ItcSprayTestReady_a, ITC_NO_TMO, ITC_FROM_ALL);

  switch (sigRec_pn->sigNo)
    {
    case ITC_MONITOR_DEFAULT_NO:
      {
	/* Lost connection with remote Error Manager */
	itc_free(&sigRec_pn);
	kill_proc(masterPid);
	fprintf(output, "Lost contact with process %s, test aborted\n", spraySlave_z);
	return RET_WARNING;
      }

    case ITC_SPRAY_TEST_READY:
      {
	/* print result */
	fprintf(output,"The test took %d milliseconds.\n",
		sigRec_pn->ItcSprayTestReady.time / 1000);
	looptime = sigRec_pn->ItcSprayTestReady.time / loops_U32;
	fprintf(output,"Signalling loop time is then %d microseconds.\n",
		looptime);

        if (linkHandler_pc == NULL) /* if board local ITCspray */
        {
           /* *This will only work for legacy applications where process pid
              and itc mbox id are equal */
           if ( get_bid(masterPid) == get_bid(slave_mbox) )
           {
              fprintf(output,
                      "\n !!! ITCspray was done within same LM (same bid).\n\n");
           }
           else
           {
              fprintf(output,
                      "\n !!! ITCspray was done between two load modules.\n\n");
           }
        }
	kill_proc(masterPid);
	itc_free(&sigRec_pn);
	itc_unmonitor(monRef);
	return RET_SUCCESS;
      }

    default:
      {
	break;
      }
    } /* switch */
  return RET_SUCCESS;
}

/** ==================================================================== */
/**
 *   On demand this process starts a sequenze of
 *   alloc - send - recieve- free_buf.
 *   It can also send a report including the number
 *   microseconds the test took.
 *
 *   @param        -
 *
 *   @return       -
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */

OS_PROCESS(Itc_sprayMaster_proc)
{
   union itc_msg *sigSend_pn, *sigRec_pn, *sigSendTest_pn, *sigRecTest_pn;
   static uint32_t ItcSprayTestStart_a[] = {1, ITC_SPRAY_TEST_START};
   static uint32_t ItcSprayTestR_a[] = {1, ITC_SPRAY_TEST_R};
   OSTICK startTick, stopTick;
   OSTICK startMicro, stopMicro;
   OSTIME delayTime;
   OSBUFSIZE size;
   int loop,i,j,burst;
   itc_id_t mid;
   itc_id_t my_mbox;

   my_mbox = itc_create_mailbox("Itc_sprayMaster_proc");

   /* Outer loop to receive command to start perfmeter */
   for (;;)
   {
      sigRec_pn = itc_receive(ItcSprayTestStart_a, ITC_NO_TMO, ITC_FROM_ALL);

      loop =  (int) sigRec_pn->ItcSprayTestStart.loops_U32;
      burst =  (int) sigRec_pn->ItcSprayTestStart.burst_U32;
      size = (OSBUFSIZE) sigRec_pn->ItcSprayTestStart.dataSize_U32 +
         sizeof(struct ItcSprayTest);
      mid = sigRec_pn->ItcSprayTestStart.slave_mbox;
      delayTime = sigRec_pn->ItcSprayTestStart.delayTime;

      if ( delayTime == 0)
      {
         /* use a version of inner loop without any delay system calls */
         startTick = get_systime(&startMicro);
         for (i = 0; i < loop; i=i+burst)
         {
            for (j=0; j<burst; j++)
            {
               sigSendTest_pn = itc_alloc(size, ITC_SPRAY_TEST);
               sigSendTest_pn->ItcSprayTest.sigSize_U32 = size;
               itc_send(&sigSendTest_pn, mid, ITC_MY_MBOX);
            }
            for (j=0; j<burst; j++)
            {
               sigRecTest_pn = itc_receive(ItcSprayTestR_a, ITC_NO_TMO, ITC_FROM_ALL);
               itc_free(&sigRecTest_pn);
            }
         }
         stopTick = get_systime(&stopMicro);
      }
      else
      {
         /* inner loop with delay system call */
         startTick = get_systime(&startMicro);
         for (i = 0; i < loop; i=i+burst)
         {
            for (j=0; j<burst; j++)
            {
               sigSendTest_pn = itc_alloc(size, ITC_SPRAY_TEST);
               sigSendTest_pn->ItcSprayTest.sigSize_U32 = size;
               itc_send(&sigSendTest_pn, mid, ITC_MY_MBOX);
            }
            for (j=0; j<burst; j++)
            {
               sigRecTest_pn = itc_receive(ItcSprayTestR_a, ITC_NO_TMO, ITC_FROM_ALL);
               itc_free(&sigRecTest_pn);
            }
            delay(delayTime);
         }
         stopTick = get_systime(&stopMicro);
      }

      sigSend_pn = itc_alloc(sizeof(struct ItcSprayTestReady), ITC_SPRAY_TEST_READY);
      if (stopTick >= startTick)
      {
         sigSend_pn->ItcSprayTestReady.time =
            (stopTick - startTick) * system_tick() + stopMicro - startMicro;
      }
      else /* Wrap around */
      {
         sigSend_pn->ItcSprayTestReady.time =
            (stopTick + (0xffffffff - startTick + 1)) * system_tick()
            + stopMicro - startMicro;
      }
      itc_send(&sigSend_pn, itc_sender(sigRec_pn), ITC_MY_MBOX);
      itc_free(&sigRec_pn);
   }
}

/** ==================================================================== */
/**
 *   Itc_spraySlave_proc
 *
 *   @param        -
 *
 *   @return       -
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */

OS_PROCESS(Itc_spraySlave_proc)
{
   union itc_msg *sigSendTest_pn, *sigRecTest_pn;
   uint32_t ItcSprayTest_a[] = {2, ITC_SPRAY_TEST, ITC_SPRAY_PRIO};
   itc_id_t my_mbox, mid;

   my_mbox = itc_create_mailbox("Itc_spraySlave_proc");

   for (;;)
   {
      sigRecTest_pn = itc_receive(ItcSprayTest_a, ITC_NO_TMO, ITC_FROM_ALL);
      switch (sigRecTest_pn->sigNo)
      {
         case ITC_SPRAY_PRIO:
         {
            set_pri((OSPRIORITY)sigRecTest_pn->ItcSprayPrio.slavePrio_U32);
            itc_free(&sigRecTest_pn);
            break;
         }

         case ITC_SPRAY_TEST:
         {
            mid = itc_sender(sigRecTest_pn);
            sigSendTest_pn = itc_alloc(sigRecTest_pn->ItcSprayTest.sigSize_U32,
                                       ITC_SPRAY_TEST_R);
            itc_free(&sigRecTest_pn);
            itc_send(&sigSendTest_pn, mid, ITC_MY_MBOX);
         }
      }
   }
}

/** ==================================================================== */
/**
 *   Itc_spray_proc
 *
 *   @param        -
 *
 *   @return       -
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */

OS_PROCESS(Itc_spray_proc)
{
   itc_id_t slave_id;
   itc_id_t my_mbox;

   my_mbox = itc_create_mailbox("Itc_spray_proc");

   slave_id = itc_locate("Itc_spraySlave_proc");
   if(slave_id != ITC_NO_ID)
   {
      printf("ITCspray slave process already exist, do not create..\n");
   }
   else
   {
      start(create_process(OS_PRI_PROC,
                           "Itc_spraySlave_proc",
                           Itc_spraySlave_proc,
                           (OSADDRESS) 2048,
                           (OSPRIORITY) 2,
                           (OSTIME) 0,
                           (PROCESS) 0,
                           (struct OS_redir_entry *) 0,
                           (OSVECTOR) 0,
                           (OSUSER) 0));
   }

   while (1)
      delay(20000);
}

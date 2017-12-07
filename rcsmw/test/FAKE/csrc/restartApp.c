/* ----------------------------------------------------------------------
 * %CCaseFile:	restartApp.c %
 * %CCaseRev:	/main/R2A/3 %
 * %CCaseDate:	2014-01-22 %
 * %CCaseDocNo: %
 * Author:	eransbn
 * Author: Niklas Boljang, <niklas.boljang@ericsson.com>
 *
 * Short description:
 * Simple test application to test eri and pri restart.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2014 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R1A/2      2013-12-02   eransbn     Created
 * R2A/2      2013-07-03   eransbn     Added CRI calls 
 * ----------------------------------------------------------------------
 */
#include <unistd.h> /* needed by sleep */
#include <cello_te_ose.h>

#include "cello_piu.h"		        //C-function interface for protocol version 3 to 10.
#include "cello_piu.sig"		//OSE signal interface for protocol version 3 to 10.
#include "cello_piu_types.h"            //PIU data types.
#include "cello_control_commontypes.h"	//CPP common types.
#include <eri_ng.h>
#include <signal.h>
#include <osetypes.h>
#include <cello_te_trace.h>
#include "shell.h" 
//#include <ose.h>   
/* #define CELLO_PRI_RESTART_WARM           0 */
/* #define CELLO_PRI_RESTART_REFRESH        1 */
/* #define CELLO_PRI_RESTART_COLD           2 */
/* #define CELLO_PRI_RESTART_COLD_WITH_TEST 3 */

void MyProgram_restartMyself();
static int RESTART_restartCommand(int argc, char **argv);
void initiatePriService(void);
void MyCelloPri_internal(void);
int test_cmd(int argc, char **argv);
static int cont = 1;
void *priMemory_p;
/* typedef struct */
/* { */
/* 	SIGSELECT        sigNo; */
/* } CelloPriServerUpInd; */
union SIGNAL
{
  SIGSELECT sigNo;
  CelloPriInitiateServiceCfm  celloPriInitiateServiceCfm;
  CelloPriServerUpInd celloPriServerUpInd;
  CelloPiu4RestartPiuRej restartReject;

};
int test_cmd(int argc, char **argv) {
  if (argc != 0) {
    printf("test does not accept any arguments\n");
    return RET_ERROR;
  }

  printf("ok\n");
  return RET_SUCCESS;
  }


/*
 *   Constants
 */           
static const char longTraceAndErrorUsage[] =
  "restart <cmd>\n"                          
  "where <cmd> is one of:\n"                
  "help - this help\n" 
  "pri cold|warm|refresh|cold_hw_test\n"
  "eri cold|warm|refresh|cold_hw_test\n"                   
  "end - end the restart application\n\n";

OS_PROCESS(restart)
{
  ENTER("create_restart");
  
  /* SIGSELECT anySig[] = {0}; */
  /* union SIGNAL *sigRec = 0; */
  // printf("cont %d",cont);
  
  while (cont) {
    sleep(1);
   
  }
  RETURN;
} 
  

void initiatePriService()
{
  ENTER("test restart: Initiating  Cello PRI service ...");
  U32 result;
  
  priMemory_p = CelloPri_initiateMemory();
  if(!priMemory_p)
    {
      return;
    }
  result = CelloPri_initiateService(priMemory_p, 
                                    CELLO_PRI_PV11,
                                    CELLO_PRI_PV10 ,
                                    CELLO_PRI_PV9);
  /*Result values:
    CELLO_PRI_MEMORY_NOT_INITIATED
    CELLO_PRI_ERROR_SERVER_NOT_AVAILABLE
    CELLO_PRI_OK
  */
  switch(result)
    {
    case CELLO_PRI_OK :
      ENTER("Cello initiateService OK");
      break;
    default:
      printf("Cello initiateService %d",result);
      fflush(stdout);
      break;
    }

  return;
}


void RESTART_addRestartCmd()
{
  shell_add_cmd("restart", "restart - Handling of restart application",
                "restart help | <cmd>", RESTART_restartCommand);
}

static int
RESTART_restartCommand(int argc, char **argv) {
  /*
   * Skip command name
   */
  argv++;
  argc--;

  if (argc < 2)
    {
      printf("Usage: %s", longTraceAndErrorUsage);
      return RET_ERROR;
    }
  if (strcmp(argv[0], "pri") == 0)
    {
      TRACE(1,"in pri ...");
    
      if(strcmp(argv[1], "warm") == 0)
	{
	  MyProgram_restartMyself(CELLO_PRI_RESTART_WARM);
	  return RET_SUCCESS;

	}
      if(strcmp(argv[1], "cold") == 0)
	{
	  MyProgram_restartMyself(CELLO_PRI_RESTART_COLD);
	  return RET_SUCCESS;
	}
      return RET_SUCCESS;
    }
  if (strcmp(argv[0], "eri") == 0)
    {
      TRACE(1,"in eri ...");
    
      if(strcmp(argv[1], "warm") == 0)
      	{
      	  eri_ng_restart_request(ERI_NG_WARM,
      				 "restart_app warm restart", NULL, 0);
      	  return RET_SUCCESS;

      	}
      if(strcmp(argv[1], "cold") == 0)
      	{
      	  eri_ng_restart_request(ERI_NG_COLD,
      				 "restart_app cold restart", NULL, 0);
      	  return RET_SUCCESS;
      	}
      return RET_SUCCESS;
    }
  if (strcmp(argv[0], "help") == 0)
    {
      printf("Usage: %s", longTraceAndErrorUsage);
      return RET_SUCCESS;
    }

  if (strcmp(argv[0], "end") == 0)
    {
      cont = 0;
      return RET_SUCCESS;
    }


  printf("Usage: %s", longTraceAndErrorUsage);
  return RET_ERROR;
}

void
MyProgram_restartMyself(U32 restartCause )
{
  initiatePriService();
  MyCelloPri_internal();
                       
  if (CelloPiu4_restartOwnPiu(&priMemory_p,                             
			      restartCause,	       
			      False,
			      "Internal error",
			      0) != CELLO_PRI_OK )
    {
      printf( "Restart own PIU rejected\n" );
      fflush(stdout);      
    } 
  else 
    {
      printf("ok");
      fflush(stdout);
    }
  
}
void MyCelloPri_internal(){
 typedef struct celloPriInitiateServiceCfm
  {
    SIGSELECT    	sigNo ;
    U32	 		signalRevision;
    U32          	selectedPV ;
  } CelloPriInitiateServiceCfm;



  union SIGNAL* sigRec  = alloc(sizeof(struct celloPriInitiateServiceCfm), CELLO_PRI_INITIATE_SERVICE_CFM);
  sigRec->celloPriInitiateServiceCfm.sigNo =  CELLO_PRI_INITIATE_SERVICE_CFM;
  sigRec->celloPriInitiateServiceCfm.signalRevision = 10;
  sigRec->celloPriInitiateServiceCfm.selectedPV = 10;

 
  CelloPriResult resultInternal;
  resultInternal = CelloPri_internal(priMemory_p, sigRec);

  // printf("after send CelloPri_internal resultInternal %d", resultInternal);
  //fflush(stdout);
  return;
}

/* ----------------------------------------------------------------------
 * %CCaseFile:	pgh_start.c %
 * %CCaseRev:	/main/R2A/R3A/R11A/1 %
 * %CCaseDate:	2017-09-28 %
 * %CCaseDocNo: %
 * Author: Per Norberg, per.norberg@ericsson.com
 *
 * Short description:
 * This file contains a short command line interface to Lmhi_start_pgm()
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
 * R2A/1      2013-03-25 etxpeno     Created
 * R11A/1     2017-09-27 etxarnu     Added Lmhi_start_pgm_3
 * ----------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "itc.h"
#include "appm_lmhi.h"
#include "appm_lmhi.sig"

struct alloc_scheme {
        itc_alloc_scheme alloc_scheme;
        union itc_scheme *scheme_par;
};

static void usage(void);

int
main(int argc, char *argv[])
{
  int option;
  int i;

  LmhiResultCode Result;
  uint32_t duId = 0;
  uint32_t cpuSet = 0;
  uint32_t mboxid = 0;
  struct alloc_scheme alloc_scheme = { ITC_MALLOC, NULL };  
  char *lmId = NULL;
  char *pgmName = NULL;
  char *ns = NULL;
  char **lmhi_argv;
  LmhiStartPgmResult startPgmRes;

  while((option = getopt(argc, argv, "d:c:l:p:mn:h")) != -1) {
    switch (option) {
    case 'd':
      duId = atoi(optarg);
      break;
    case 'c':
      cpuSet = atoi(optarg);
      break;
    case 'l':
      lmId = strdup(optarg);
      break;
    case 'p':
      pgmName = strdup(optarg);
      break;
    case 'm':
      mboxid = 1;
      break;
    case 'n':
      ns = strdup(optarg);
      break;
    case 'h':
      usage();
      exit(EXIT_SUCCESS);
    default:
      usage();
      exit(EXIT_FAILURE);
    }
  }

  if (lmId == NULL || pgmName == NULL) {
    usage();
    exit(EXIT_FAILURE);
  }

  if (mboxid == 1) {
    if(itc_init(1, alloc_scheme.alloc_scheme,
		alloc_scheme.scheme_par, 
		ITC_NO_NAMESPACE, 0) != 0) {
      printf("ITC init failed, exiting\n");
      exit(-1);
    }
   mboxid = itc_create_mailbox("ADD_BOARD",0);  
  }



  lmhi_argv = malloc((1+argc-optind)* sizeof(*lmhi_argv));
  for(i = optind; i < argc; i++)
    lmhi_argv[i-optind] = strdup(argv[i]);
  lmhi_argv[i-optind] = NULL;

  Result = Lmhi_start_pgm_3(duId, cpuSet, lmId, pgmName, ns, mboxid, lmhi_argv, &startPgmRes);

  for(i = optind; i < argc; i++)
    free(lmhi_argv[i-optind]);
  free(lmhi_argv);
  free(pgmName);
  free(lmId);
  free(ns);

  printf("%u %u\n", Result, startPgmRes.pgmId);
  if (mboxid != 0) {
    printf(" Waiting for program termination .....\n");
    union itc_msg *msg;
    msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
    switch(msg->msgno) {
    case LMHI_PGM_TERM_IND:
      printf("LMHI_PGM_TERM_IND received with\n");
      printf("Reason = %u \n",msg->LmhiPgmTermInd.reason);
      break;
    case LMHI_PGM_CRASH_IND:
      printf("LMHI_PGM_CRASH_IND received\n");
      break;
      
    default:
      printf("Unexpected message received : %d\n",msg->msgno );
      break;
    }
    itc_free(&msg);
  }

  return 0;
}

static void
usage(void)
{
  printf("usage: pgh_start -h\n");
  printf("usage: pgh_start -p name -l id [-c cpuset] [-d duid] [-n namespace] [-m] -- <args> \n");
}

/* ----------------------------------------------------------------------
 * %CCaseFile:	pgh_pids.c %
 * %CCaseRev:	/main/R5A/1 %
 * %CCaseDate:	2016-03-10 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin arto.nummelin@ericsson.com
 *
 * Short description:
 * This file contains a short command line interface to Lmhi_get_pids()
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2013 All rights reserved.
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
 * R5A/1      2016-03-10 etxarnu     Created
 * ----------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "appm_lmhi.h"

static void usage(void);

int
main(int argc, char *argv[])
{
  int option;
  int pgmIdFl = 0;
  uint32_t i;
  LmhiGetPidsResult pids;
  LmhiResultCode Result;
  uint32_t duId = 1;
  uint32_t pgmId = 0;

  while((option = getopt(argc, argv, "d:p:h")) != -1) {
    switch (option) {
    case 'd':
      duId = atoi(optarg);
      break;
    case 'p':
      pgmId = atoi(optarg);
      pgmIdFl = 1;
      break;
    case 'h':
      usage();
      exit(EXIT_SUCCESS);
    default:
      usage();
      exit(EXIT_FAILURE);
    }
  }

  if (!pgmIdFl) {
    usage();
    exit(EXIT_FAILURE);
  }

  Result = Lmhi_get_pids(duId, pgmId, &pids);
  printf ("\nResult from get_pids:\n");
  printf("Result= %u\n", Result);
  printf("NoOfPids= %u\n",pids.n_pids );
  if (Result == LMHI_OK) 
    for ( i = 0; i < pids.n_pids; i++) 
      printf ("Pid: %u\n",pids.pdata[i] ); 

  Lmhi_free_pids( &pids);
  return 0;
}

static void
usage(void)
{
  printf("usage: pgh_pids -h\n");
  printf("usage: pgh_pids -p pgmId [-d duid]\n");
}

/* ----------------------------------------------------------------------
 * %CCaseFile:	pgh_signal.c %
 * %CCaseRev:	/main/R2A/1 %
 * %CCaseDate:	2014-01-14 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin
 *
 * Short description:
 * This file contains a short command line interface to Lmhi_signal_to_pgm()
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
 * R2A/1      2014-01-14 etxarnu     Created
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
  int sigNoFl = 0;

  LmhiResultCode Result;
  uint32_t duId = 0;
  uint32_t pgmId = 0;
  uint32_t sigNo = 0;

  while((option = getopt(argc, argv, "d:p:s:h")) != -1) {
    switch (option) {
    case 'd':
      duId = atoi(optarg);
      break;
    case 'p':
      pgmId = atoi(optarg);
      pgmIdFl = 1;
      break;
    case 's':
      sigNo = atoi(optarg);
      sigNoFl = 1;
      break;
     case 'h':
      usage();
      exit(EXIT_SUCCESS);
    default:
      usage();
      exit(EXIT_FAILURE);
    }
  }

  if ( !(pgmIdFl && sigNoFl) ) {
    usage();
    exit(EXIT_FAILURE);
  }

  Result = Lmhi_signal_to_pgm(duId, pgmId, sigNo);

  printf("%u\n", Result);

  return 0;
}

static void
usage(void)
{
  printf("usage: pgh_signal -h\n");
  printf("usage: pgh_signal -p pgmId -s sigNo [-d duid]\n");
}

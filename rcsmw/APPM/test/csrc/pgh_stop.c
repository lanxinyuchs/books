/* ----------------------------------------------------------------------
 * %CCaseFile:	pgh_stop.c %
 * %CCaseRev:	/main/R2A/1 %
 * %CCaseDate:	2013-03-25 %
 * %CCaseDocNo: %
 * Author: Per Norberg, per.norberg@ericsson.com
 *
 * Short description:
 * This file contains a short command line interface to Lmhi_stop_pgm()
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
 * R2A/1      2013-03-25 etxpeno     Created
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

  LmhiResultCode Result;
  uint32_t duId = 0;
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

  Result = Lmhi_stop_pgm(duId, pgmId);

  printf("%u\n", Result);

  return 0;
}

static void
usage(void)
{
  printf("usage: pgh_stop -h\n");
  printf("usage: pgh_stop -p pgmId [-d duid]\n");
}

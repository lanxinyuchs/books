/* ----------------------------------------------------------------------
 * %CCaseFile:	test_lmhi.c %
 * %CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R7A/1 %
 * %CCaseDate:	2016-10-05 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * This file contains ta short command line interface to the LMHI interface
 * usage:  rcsapp.sh test_lmhi [BoardType] Tag NoOfReqs
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
 * R1A/3      2012-08-17   etxarnu     Created
 * R4A/1      2015-11-27   etxarnu     Corrected bug
 * ----------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <string.h>

#include "appm_lmhi.h"
/*--------------------------------------------------------------------------*/
int64_t timespecDiff(struct timespec *timeA_p, struct timespec *timeB_p)
{
  return ((timeA_p->tv_sec * 1000000000) + timeA_p->tv_nsec) -
           ((timeB_p->tv_sec * 1000000000) + timeB_p->tv_nsec);
}

static void usage(void);

/*--------------------------------------------------------------------------*/
int main (int argc,  char * argv[])
{
  int option;
  char * boardtype = NULL;
  char * boardrev = NULL;
  char * hwcategory = NULL;
  char * hwmodel = NULL;
  char * tag = NULL;
  int preload = 0;
  int loops = 1;
  LmhiGetLMsResult lms;
  LmhiResultCode res;

  while((option = getopt(argc, argv, "y:l:b:t:r:c:ph")) != -1) {
    switch (option) {
    case 'y':
      hwcategory = strdup(optarg);
      break;
    case 'l':
      hwmodel = strdup(optarg);
      break;
    case 'b':
      boardtype = strdup(optarg);
      break;
    case 'r':
      boardrev = strdup(optarg);
      break;
    case 't':
      tag = strdup(optarg);
      break;
    case 'p':
      preload=1;
      break;
    case 'c':
      loops = atoi(optarg);
      break;
    case 'h':
      usage();
      exit(EXIT_SUCCESS);
    default:
      usage();
      exit(EXIT_FAILURE);
    }
  }
  if (tag == NULL) {
    usage();
    exit(EXIT_FAILURE);
  }

  if (boardtype == NULL) {
    printf ("Calling get_lms %d times with: NULL %s  %d\n",
	    loops,   tag,  preload);
  } else {
    printf ("Calling get_lms %d times with: %s  %s %s  %d\n",
	    loops,  boardtype, boardtype,  tag,  preload);
  }
  struct timespec start, end;
  clock_gettime(CLOCK_MONOTONIC, &start);

  unsigned int i,j;
  for ( i=0; i<loops; i++) {
    res = Lmhi_get_lms_3 (hwcategory, hwmodel, boardtype, boardrev,  tag, preload, &lms );

    if (res != LMHI_OK) {
      fprintf(stderr, "LMHI error, result= %d\n",
	      res);
      exit(1);
    }
  }

  clock_gettime(CLOCK_MONOTONIC, &end);
  int64_t timeElapsed = timespecDiff(&end, &start);
  fprintf(stdout,"\tElapsed CPU time test:   %ld  usec\n",
	  (long int)timeElapsed/1000);
  fprintf(stdout,"\tMean CPU time:   %ld  usec\n",
	  (long int)timeElapsed/1000/loops);


  printf ("\nResult from get_lms:\n");
  for ( i = 0; i < lms.n_lmdata; i++) {
    printf ("LM: %s %s %s\n",
  	    lms.lmdata[i].name,
  	    lms.lmdata[i].id,
  	    lms.lmdata[i].rev );
    for ( j = 0; j < lms.lmdata[i].n_file; j++)
      printf ("   File:[%d,%d] type=%s, path=%s\n",i,j,
  	      lms.lmdata[i].file[j].type,
  	      lms.lmdata[i].file[j].path);
  }
  Lmhi_free_buffers(&lms);
  free(boardtype);
  free(tag);
    return 0;
}

static void
usage(void)
{
  printf("usage: test_lmhi -h\n");
  printf("usage: test_lmhi -t tag  [-y hwcategory] [-l hwmodel] [-b boardtype] [-r boardrev] [-p (=preload) ] [-c loops]  [-h (=help)] \n");
}

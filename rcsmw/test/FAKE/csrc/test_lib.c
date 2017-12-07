/* ----------------------------------------------------------------------
 * %CCaseFile:	test_lib.c %
 * %CCaseRev:	/main/R1A/R2A/R3A/1 %
 * %CCaseDate:	2014-11-20 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 * Author: Arto Nummelin, <arto.nummelin@ericsson.com>
 *
 * Short description:
 * Super simple test library to test APPM block in RCS.
 * Will do a printout when called .
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2014 All rights reserved.
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
 * R1A/1      2012-06-08   etxarnu     Created
 * R2A/1      2013-02-28   etxarnu     Updated since DBI and TRANI removed
 * ----------------------------------------------------------------------
 */

#include <stdio.h> 
#include <stdlib.h> 


void print_env_vars (int i) {
  int cec,log;


  if (getenv("CEC_PORT") != NULL ) { // if running in RCS
    cec = atoi(getenv("CEC_PORT"));
    log = atoi(getenv("LOG_PORT"));
  }
  else { // if running standalone
    cec = 0;
    log = 0;
  }
 
  printf("\nHello RCS World in test_lib - %d\n",i);
  printf("CEC %d, LOG %d \n", cec,log);
  printf("TEST_APP_DEBUG=  %s \n", getenv("TEST_APP_DEBUG"));
  if (i%60 == 0) { 
    if (getenv("LD_LIBRARY_PATH") != NULL ) { // if running in RCS
      printf("LD_LIBRARY_PATH=  %s \n", getenv("LD_LIBRARY_PATH"));
    }
  }

  fflush(stdout);

 
}



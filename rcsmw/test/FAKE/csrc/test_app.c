/* ----------------------------------------------------------------------
 * %CCaseFile:	test_app.c %
 * %CCaseRev:	/main/R1A/R2A/6 %
 * %CCaseDate:	2014-04-22 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 * Author: Arto Nummelin, <arto.nummelin@ericsson.com>
 *
 * Short description:
 * Super simple test application to test APPM block in RCS.
 * Will do a printout every 10 seconds which is handled as an
 * incoming signal on Erlang side. This can be viewed if turning
 * on debug in appmServer with appmServer:debug(true).
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
 * R1A/2      2012-03-13   etxarnu     Created
 * R2A/2      2013-07-03   etxarnu     Added ERI calls 
 * R2A/4      2014-04-22   etxberb     Added Heartbeat.
 * ----------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <signal.h>
#include <eri_ng.h>
#include <appm_lmhi.h>

#define SIG_RST_CNT     10
#define SIG_WARM        12
#define SIG_COLD        18
#define SIG_COLD_W_TEST 19

extern void print_env_vars(int);

extern int test_imm_om();

void sleep(int);
int i=0;

// Define the function to be called when  signal 10 is sent to process
void
signal_callback_handler(int signum)
{
  printf("Caught signal %d\n",signum);

  switch ( signum ) {
  case SIG_RST_CNT:
    printf("Resetting counter to 0\n");
    fflush(stdout);
    i = 0;
    break;

  case SIG_WARM:
    eri_ng_restart_request(ERI_NG_WARM,
			   "test_app warm restart", NULL, 0);
    break;

 case SIG_COLD:
    eri_ng_restart_request(ERI_NG_COLD,
			   "test_app cold restart", NULL, 0);
    break;

  case SIG_COLD_W_TEST:
    eri_ng_restart_request(ERI_NG_COLD_WITH_TEST,
			   "test_app cold_w_test restart", NULL, 0);
    break;
  }
}



int main(void) {
  // Register signal and signal handler
  signal(SIG_RST_CNT, signal_callback_handler);
  signal(SIG_WARM, signal_callback_handler);
  signal(SIG_COLD, signal_callback_handler);
  signal(SIG_COLD_W_TEST, signal_callback_handler);

  char *lmId = getenv("CXC_NO");
  char *pgmName = getenv("CXC_NAME");

  if (test_imm_om() != 0)
    {
      printf("test_imm_om() failed\n");
    }

  while (1) {
    print_env_vars (i); //call to shared library function
    sleep(10);
    Lmhi_heartbeat(pgmName, lmId);
    i += 10;

  }

}



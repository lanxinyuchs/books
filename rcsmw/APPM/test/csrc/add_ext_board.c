/* ----------------------------------------------------------------------
 * %CCaseFile:	add_ext_board.c %
 * %CCaseRev:	/main/R10A/1 %
 * %CCaseDate:	2017-05-30 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin
 *
 * Short description:
 * This file contains a short command line interface to Lmhi_add_board()
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
 * R8A/1      2016-12-15 etxarnu     Created
 * ----------------------------------------------------------------------
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "itc.h"
#include "appm_lmhi.h"
#include "appm_lmhi.sig"

struct alloc_scheme {
        itc_alloc_scheme alloc_scheme;
        union itc_scheme *scheme_par;
};

void usage(void)
{
  printf("usage: add_ext_board -b boardtype -r boardrev\n");
}



int main(int argc, char *argv[])
{
  int option;
  char * boardtype = NULL;
  char * boardrev = NULL;
  uint32_t mboxid;
  struct alloc_scheme alloc_scheme = { ITC_MALLOC, NULL };  
  LmhiResultCode Result;

  while((option = getopt(argc, argv, "b:r:")) != -1) {
    switch (option) {
    case 'b':
      boardtype = strdup(optarg);
      break;
    case 'r':
      boardrev = strdup(optarg);
      break;
    default:
      usage();
      exit(EXIT_FAILURE);
    }
  }

  if (boardtype == NULL || boardrev == NULL){
    usage();
    exit(EXIT_FAILURE);
  }


  if(itc_init(1, alloc_scheme.alloc_scheme,
	      alloc_scheme.scheme_par, 
	      ITC_NO_NAMESPACE, 0) != 0) {
    printf("ITC init failed, exiting\n");
    exit(-1);
  }

  mboxid = itc_create_mailbox("ADD_EXT_BOARD",0);

  Result = Lmhi_add_ext_board(boardtype, boardrev, mboxid);

  printf("%u\n", Result);
  
  if (Result == LMHI_WAIT) {
    union itc_msg *msg;
    printf("Waiting for response...\n");
    msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
    switch(msg->msgno) {
    case LMHI_ADD_EXT_BOARD_RSP:
      printf("LMHI_ADD_EXT_BOARD_RSP received with\n");
      printf("BoardNo = %s \n",msg->LmhiAddBoardRsp.BoardNo);
      printf("BoardRev = %s\n",msg->LmhiAddBoardRsp.BoardRev);
      break;
      
    default:
      printf("Unexpected message received : %d\n",msg->msgno );
      break;
    }
    itc_free(&msg);
    
  }

  return 0;
}


/* ----------------------------------------------------------------------
 * %CCaseFile:	set_bootptr.c %
 * %CCaseRev:	/main/R2A/1 %
 * %CCaseDate:	2014-03-27 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin
 *
 * Short description:
 * This file contains the command to call RHAI sys_set_bootptr
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
 * R2A/1      2014-03-27 etxarnu     Created (copy from derb)
 * ----------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "rhai-sys.h"



int usage(char **argv) {
  printf("Usage %s bootptr    where bootptr = configured | fallback | nl\n", argv[0]);
  return 0;
}


int setptr(uint32_t restarttype) {
   if (rhai_sys_setbootptr(restarttype) == 0) {
     printf("bootptr set to %d\n",restarttype);
     return 0;
   }
   else {
     printf("set_bootptr got error from rhai_sys_setbootptr\n");
     return -1;
   }
}

int main(int argc, char **argv) {
   uint32_t type;
   if ( argc !=2 ) {
      usage(argv);      
      return -1;
    }
   if(strcmp(argv[1], "configured") == 0) 
      type = RHAI_SYSBOOT_CF;
   else if(strcmp(argv[1], "fallback") == 0) 
      type = RHAI_SYSBOOT_FB;
   else if(strcmp(argv[1], "nl") == 0) 
      type = RHAI_SYSBOOT_NL;
   else
     return -1;
   return setptr(type); 
}


/**********************************************************************
 * %CCaseFile:	bootcounter.c %
 * %CCaseRev:	/main/R2A/2 %
 * %CCaseDate:	2014-04-07 %
 * %CCaseDocNo:  %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014 All rights reserved.
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
 * **********************************************************************
 * Rev      Date       Name        What
 * -----    -------    --------    --------------------------
 * R2A/1-2  20140407   etxderb    Created
 * **********************************************************************/

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <rhai-sys.h>

int usage(char **argv) {
  printf("usage: %s get | set [value]\n", argv[0]);
  return 0;
}


int getbootcounter() {
   uint32_t bootcount;
   if (rhai_sys_getbootcounter(&bootcount) == 0) {
      printf("%d\n", bootcount);
      return 0;
   }
   return -1;
}


int main(int argc, char **argv) {
        if (argc < 2) {
           usage(argv);
           return 1;
        }
        if (strcmp(argv[1], "set") == 0)
           if (argc == 3) {
              uint32_t counter = atoi(argv[2]);
              return rhai_sys_setbootcounter(counter);
              }
        if (strcmp(argv[1], "get") == 0) 
           if (argc == 2) 
              return getbootcounter();
        usage(argv);
        return 1;
} 


/* ----------------------------------------------------------------------
 * %CCaseFile:	sysread.c %
 * %CCaseRev:	/main/R2A/R3A/1 %
 * %CCaseDate:	2015-04-14 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin
 *
 * Short description:
 * This file contains the command to read bootptr, bootcount, restart_type
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
 * R2A/4      2014-04-22 etxarnu     Changed restart_type to return string
 * R2A/5      2015-04-14 etxarnu     Added more values to restart_type
 * ----------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <rhai-sys.h>

int usage(char **argv) {
  printf("Usage %s all | restart_type | bootcount | bootptr\n", argv[0]);
  return 0;
}

int bootptr() {
   uint32_t bootptr;
   if (rhai_sys_getbootptr(&bootptr) == 0) {
      printf("bootptr=%d\n", bootptr);
      return 0;
   }
   return -1;
}

int restart_type() {
   uint32_t restart_type;
   if (rhai_sys_getrestarttype(&restart_type) == 0) {
     switch (restart_type ) {
     case  RHAI_SYSRESTART_COLD:
       printf("COLD");
      return 0;
     case  RHAI_SYSRESTART_COLDTEST:
       printf("COLDTEST");
      return 0;
     case  RHAI_SYSRESTART_COLDEXTEST:
       printf("COLDEXTEST");
      return 0;
     case  RHAI_SYSRESTART_POWERON:
       printf("POWERON");
      return 0;
     case  RHAI_SYSRESTART_WATCHDOG:
       printf("WATCHDOG");
      return 0;
     case  RHAI_SYSRESTART_MMI_RESET:
       printf("MMI_RESET");
      return 0;
     case  RHAI_SYSRESTART_BPM:
       printf("BPM");
      return 0;
     case  RHAI_SYSRESTART_TEST:
       printf("TEST");
      return 0;
     case  RHAI_SYSRESTART_ECC:
       printf("ECC");
      return 0;
     }

     printf("UNKNOWN=%d", restart_type);
     return -1;
   }
   return -1;
}


int bootcount() {
   uint32_t bootcount;
   if (rhai_sys_getbootcounter(&bootcount) == 0) {
      printf("boot counter=%d\n", bootcount);
      return 0;
   }
   return -1;
}



int main(int argc, char **argv) {

  //        printf("called as %s %s\n", argv[0], argv[1]);
        
        if ( argc !=2 ) {
          usage(argv);      
          return -1;
        }
        if (strcmp(argv[1], "restart_type") == 0) {
          return restart_type();
        }
        if (strcmp(argv[1], "bootcount") == 0) {
          return bootcount();
        }
        if (strcmp(argv[1], "bootptr") == 0) {
          return bootptr();
        }
        if (strcmp(argv[1], "all") == 0) {
	  restart_type();
	  bootcount();
	  return bootptr();
        }
        usage(argv);      
        return 1; 
       
}


/* ----------------------------------------------------------------------
 * %CCaseFile:	bootptr.c %
 * %CCaseRev:	/main/R2A/R4A/1 %
 * %CCaseDate:	2015-08-27 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin
 *
 * Short description:
 * This file contains the command to read and write bootptr
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
 * R2A/1-2      2014-04-28 etxderb     Created
 * R4A/1        2015-08-27 etxarnu     Added NL2 and NL3 
 * ----------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <rhai-sys.h>

int usage() {
  printf("usage: bootptr get | set [configured|fallback|nl|nl2|nl3|upgrade|configured_once]\n");
  return 0;
}





char *from_rhai(int rhai) {
  char *res;
  switch(rhai) {
  case RHAI_SYSBOOT_CF:      res = "configured";     break;
  case RHAI_SYSBOOT_FB:      res = "fallback";       break;
  case RHAI_SYSBOOT_NL2:     res = "nl2";            break;
  case RHAI_SYSBOOT_NL3:     res = "nl3";            break;
  case RHAI_SYSBOOT_UP:      res = "upgrade";        break; /* remove ? */
  case RHAI_SYSBOOT_CF_ONCE: res = "configured_once";break; /* remove ? */
  default:
    printf("bootptr: Unknown bootptr received from RHAI %d\n", rhai);
    exit(-1);
  }
  return res;
}


int to_rhai(char *bootptr) {
  if (strcmp(bootptr, "configured") == 0)
    return RHAI_SYSBOOT_CF;
  if (strcmp(bootptr, "fallback") == 0)
    return RHAI_SYSBOOT_FB;
  if (strcmp(bootptr, "nl") == 0)
    return RHAI_SYSBOOT_NL;
  if (strcmp(bootptr, "nl2") == 0)
    return RHAI_SYSBOOT_NL2;
  if (strcmp(bootptr, "nl3") == 0)
    return RHAI_SYSBOOT_NL3;
  if (strcmp(bootptr, "upgrade") == 0)
    return RHAI_SYSBOOT_UP;
  if (strcmp(bootptr, "configured_once") == 0)
    return RHAI_SYSBOOT_CF_ONCE;
  usage();
  exit(-1);
}

int get_bootptr() {
  uint32_t bootptr;
  if (rhai_sys_getbootptr(&bootptr) == 0) {
    printf("%s\n", from_rhai(bootptr));
    return 0;
  }
   return -1;
}

int set_bootptr(char *bootptr) {
  return rhai_sys_setbootptr(to_rhai(bootptr));
}


int main(int argc, char **argv) {

  if (argc < 2) {
    usage();
    return 1;
  }
  if (strcmp(argv[1], "set") == 0)
    if (argc == 3)
      return set_bootptr(argv[2]);
  if (strcmp(argv[1], "get") == 0) 
    if (argc == 2) 
      return get_bootptr();
  usage();
  return 1;
}




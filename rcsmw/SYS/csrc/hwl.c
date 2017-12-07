/* ----------------------------------------------------------------------
 * %CCaseFile:	hwl.c %
 * %CCaseRev:	/main/R5A/R11A/1 %
 * %CCaseDate:	2017-10-03 %
 * %CCaseDocNo: %
 * Author: Petra Jansson
 *
 * Short description:
 * This file contains the command to read and write bootptr
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
 * R5A/1      2016-03-14 etxpejn     Created
 * R11A/1     2017-10-02 etxpejn     Updated with logId
 * ----------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <rhai-hwl.h>
#include <syslog.h>

int usage() {
  printf("usage: hwl log [UP version]\n");
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    usage();
    return 1;
  }
  if (strcmp(argv[1], "log") == 0) { 
    if (argc == 4) {
      int r;
      r = rhai_hwl_logentry(argv[2], RHAI_HWL_FILTER_MSG(1, RHAI_HWL_FILTER_COMPLETE_MSG),
			    argv[3], 1);
      return r;
      }
  }
  usage();
  return 1;
}




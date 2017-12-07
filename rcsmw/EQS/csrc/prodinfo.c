/* ----------------------------------------------------------------------
 * %CCaseFile:	prodinfo.c %
 * %CCaseRev:	/main/R3A/2 %
 * %CCaseDate:	2015-03-03 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin
 *
 * Short description:
 * This file contains the command to read product information
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
 * R3A/1      2014-10-20 etxarnu     Created 
 * ----------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <rhai-sys.h>
static struct rhai_sys_hwinfo hwinfo ;

int usage(char **argv) {
  printf("Usage %s -long | -short \n", argv[0]);
  return 0;
}

int prodinfo() {
   if (rhai_sys_hwinfo(&hwinfo) == 0) {
      return 0;
   }
   return -1;
}

void print_hwinfo() {
  printf("productname=%s\n",   hwinfo.productname);
  printf("marketname=%s\n",   hwinfo.marketname);
  printf("productnumber=%s\n", hwinfo.productnumber);
  printf("productrevision=%s\n", hwinfo.productrevision);
  printf("productdate=%s\n", hwinfo.productdate);
  printf("serialnumber=%s\n", hwinfo.serialnumber);
}

void print_hwinfo_short() {
  printf("%s\n", hwinfo.productname);
  printf("%s\n", hwinfo.marketname);
  printf("%s\n", hwinfo.productnumber);
  printf("%s\n", hwinfo.productrevision);
  printf("%s\n", hwinfo.productdate);
  printf("%s\n", hwinfo.serialnumber);
}

int main(int argc, char **argv) {

        if ( argc !=2 ) {
          usage(argv);      
          return -1;
        }
	if (rhai_sys_hwinfo(&hwinfo) != 0) {
	  printf("Error reading hwinfo \n");
	  return -1;
	}
        if (strcmp(argv[1], "-long") == 0) {
	    print_hwinfo();
	    return 0;
	  }
	else if (strcmp(argv[1], "-short") == 0) {
	    print_hwinfo_short();
	    return 0;
        }
	else {
	  usage(argv);      
	  return 1;
	}       
}


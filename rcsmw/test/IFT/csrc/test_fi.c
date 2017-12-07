/* ----------------------------------------------------------------------
 * %CCaseFile:	test_fi.c %
 * %CCaseRev:	/main/R3A/R4A/R6A/R7A/R10A/1 %
 * %CCaseDate:	2017-06-05 %
 * %CCaseDocNo: %
 * Author:	etxivri
 * Author: Ivan Ribrant, <ivan.ribrant@ericsson.com>
 *
 * Short description:
 * Super simple test LICI block in RCS using legacy interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
 *b
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R3A/1      2014-08-28 etxpejn     Created
 * R3A/4      2015-01-09 erarafo     Support for files in ramdisk
 * R3A/5      2015-01-14 erarafo     Set parameters for speedy tests
 * R3A/6      2015-01-22 erarafo     Parameter setting by COLI; cleanup
 * R4A/1      2015-11-26 erarafo     Wrappers around TRI trace macros
 * R6A/1      2016-07-11 etxpeno     Coverity fix
 * R7A/1      2016-10-17 etxpejn     Added FiImportFile_no_2 & FiExportFile_no_2
 * R10A/1     2017-05-29 ejinfeg     Added FiImportFile_no_3
* ----------------------------------------------------------------------
 */

#include "master.h"
#include "fi.h"

#include "osens.sig"

#define _GNU_SOURCE

#include <stdio.h>

#define FiImportFile_no   0
#define FiExportFile_no   1
#define FiOpenWrite_no    2
#define FiImportFile_no_2 3
#define FiExportFile_no_2 4
#define FiImportFile_no_3 5

extern bool TRI_trace_enabled;

static ei_x_buff send_sig_fi3(int func, ei_x_buff args);
static ei_x_buff send_sig_fi5(int func, ei_x_buff args);

// hidden function; this declaration must match code in fi.c
extern int fiSetParams(
    uint32_t persistence,
    uint32_t spaceLimit,
    uint32_t staleMillis,
    uint32_t timeout);

ei_x_buff resp;
int type, size2;
char *password;
char *uri;
char *dir;
FiResultT r;

#define RECEIVE_TMO (5000) /* The number of ms to wait for a response in the tests. */

ei_x_buff
send_sig_fi(int function, ei_x_buff args) {

  int arity;

  ei_decode_tuple_header(args.buff, &args.index, &arity);

  switch (arity) {
  case 3:
    return send_sig_fi3(function, args);
  case 5:
    return send_sig_fi5(function, args);
  default:
    {
      ei_x_buff resp;
      ei_x_format(&resp, "{ok, ~s, ~i}", "arity not supported", arity);
      return resp;
    }
  }
}


static ei_x_buff
send_sig_fi3(int function, ei_x_buff args) {

   ei_x_new(&resp);

   switch (function) {
   case FiExportFile_no:
     QINFO1("FiExportFile_no");

     ei_get_type(args.buff, &args.index, &type, &size2);
     password = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, password);

     ei_get_type(args.buff, &args.index, &type, &size2);
     uri = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, uri);

     ei_get_type(args.buff, &args.index, &type, &size2);
     dir = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, dir);

     r = fiExportFile(password, uri, dir);

     ei_x_format(&resp, "{ok, ~u}", r);
     return resp;

   case FiExportFile_no_2:
     QINFO1("FiExportFile_no_2");
     
     ei_get_type(args.buff, &args.index, &type, &size2);
     password = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, password);

     ei_get_type(args.buff, &args.index, &type, &size2);
     uri = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, uri);

     ei_get_type(args.buff, &args.index, &type, &size2);
     dir = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, dir);

     r = fiExportFile2(password, uri, dir);

     ei_x_format(&resp, "{ok, ~u}", r);
     return resp;

   case FiImportFile_no:
     QINFO1("FiImportFile_no");

     ei_get_type(args.buff, &args.index, &type, &size2);
     password = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, password);

     ei_get_type(args.buff, &args.index, &type, &size2);
     uri = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, uri);

     ei_get_type(args.buff, &args.index, &type, &size2);
     dir = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, dir);

     r = fiImportFile(password, uri, dir);

     ei_x_format(&resp, "{ok, ~u}", r);
     return resp;

   case FiImportFile_no_2:
     QINFO1("FiImportFile_no_2");

     ei_get_type(args.buff, &args.index, &type, &size2);
     password = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, password);

     ei_get_type(args.buff, &args.index, &type, &size2);
     uri = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, uri);

     ei_get_type(args.buff, &args.index, &type, &size2);
     dir = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, dir);

     r = fiImportFile2(password, uri, dir);

     ei_x_format(&resp, "{ok, ~u}", r);
     return resp;

   case FiOpenWrite_no:
     QINFO1("FiOpenWrite_no");
     // arg1: the "dir" component
     // arg2: the "name" component
     // arg3: the size of the file to be written

     char *p1 = decodeString(&args);
     char *p2 = decodeString(&args);
     long long m = decodeInteger(&args);

     FILE* fp;
     FiResultT r = fiOpenWrite(FI_VOLATILE, p1, p2, &fp);

     if (r != FI_OK) {
       ei_x_format(&resp, "{error, ~u}", r);
     }
     else {
       for (long long k = 0; k < m; k++) {
         fputc('A', fp);
       }
       fclose(fp);
       ei_x_format(&resp, "{ok, ~u}", r);
     }
     free(p1);
     free(p2);
     return resp;
   }

   ei_x_format(&resp, "{ok, function_does_not_exist}");
   return resp;
}

static ei_x_buff
send_sig_fi5(int function, ei_x_buff args) {

   ei_x_new(&resp);

   switch (function) {
   case FiImportFile_no_3:
     QINFO1("FiImportFile_no_3");

     ei_get_type(args.buff, &args.index, &type, &size2);
     password = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, password);

     ei_get_type(args.buff, &args.index, &type, &size2);
     uri = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, uri);

     ei_get_type(args.buff, &args.index, &type, &size2);
     dir = malloc(size2+1);
     ei_decode_string(args.buff, &args.index, dir);

     uint64_t fileSize;
     ei_decode_ulonglong(args.buff, &args.index, &fileSize);

     uint8_t encrpted;
     ei_decode_char(args.buff, &args.index, &encrpted);

     r = fiImportFile3(password, uri, dir, fileSize, encrpted);

     ei_x_format(&resp, "{ok, ~u}", r);
     return resp;
   }

   ei_x_format(&resp, "{ok, function_does_not_exist}");
   return resp;
}

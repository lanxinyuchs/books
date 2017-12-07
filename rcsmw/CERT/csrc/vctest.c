/*
 * vctest.c
 *
 *  Created on: Dec 16, 2015
 *      Author: ehsake
 */
/* ----------------------------------------------------------------------
 * %CCaseFile:  vctest.c %
 * %CCaseRev:   /main/R3A/1 %
 * %CCaseDate:  2016-01-25 %
 * %CCaseDocNo: %
 * Author:      ehsake
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB %CCaseTemplateCopyrightYear% All rights reserved.
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
 * Rev        Date        Name        What
 * -----      ----------  --------    --------------------------
 *            20160125    ehsake      SECI update for eIDL encryption
 * ----------------------------------------------------------------------
 */


/*
 * Simple test program for reading VC, not included in delivered LM
 * Checkout and update SECRET_PREFIX before building
 */


#include <stdio.h>
#include <string.h>

#include <cert_seci.h>
#include <cert_file_vc.h>
#include "cert_vc.h"

char*
seciErrToString(SeciResultT result) {

  switch (result) {
  case SECI_OK: return "SECI_OK";
  case SECI_NOT_FOUND: return "SECI_NOT_FOUND";
  case SECI_ERROR: return "SECI_ERROR";
  case SECI_INVALID_PARAMETER_RESULTP: return "SECI_INVALID_PARAMETER_RESULTP";
  case SECI_SEND_ERROR: return "SECI_SEND_ERROR";
  case SECI_RECEIVE_ERROR: return "SECI_RECEIVE_ERROR";
  case SECI_DELETE_FAILED_WRONG_ID: return "SECI_DELETE_FAILED_WRONG_ID";
  case SECI_UNINITIALIZED_SUB: return "SECI_UNINITIALIZED_SUB";
  case SECI_VERIFY_PEER_VALID: return "SECI_VERIFY_PEER_VALID";
  case SECI_VERIFY_PEER_NOT_VALID: return "SECI_VERIFY_PEER_NOT_VALID";
  case SECI_VERIFY_PEER_UNKNOWN: return "SECI_VERIFY_PEER_UNKNOWN";
  default:
    return "unknown code";
  }

}

void
get_vc_from_seci(char* secretP)
{
  SeciResultT result;
  char* resultP = NULL;

  printf("call seci_get_vc\n");
  result = seci_get_vc(secretP, &resultP);
  printf("Result = %d and data = %s\n ",result,resultP);

  seci_free(resultP);

}

void
get_cert(char* secretP)
{
  SeciResultT result;
  char* resultP = NULL;
  printf("call seci_get_cert\n");

  result = seci_get_cert(secretP,&resultP);
  printf("Result = %d \n ",result);
  seci_free(resultP);
  resultP = NULL;

}


void
get_vcert()
{
  SeciResultT result;
  char* resultP = NULL;
  printf("call seci_get_vcert\n");

  result = seci_get_vcert(&resultP);
  printf("Result = %d and data = %s\n ",result,resultP);
  seci_free(resultP);
  resultP = NULL;

}

void test_verify_own_vc()
{
  SeciResultT result;
  char* resultP = NULL;
  printf("call seci_get_vcert\n");

  result = seci_get_vcert(&resultP);
  if(result == SECI_OK) {
    printf("got vc, testing verifying it \n");
    result = seci_verify_peer_vc(resultP);
     printf("Verify result = %s \n",seciErrToString(result));
  }
  seci_free(resultP);


}

void test_encode() {
  SeciResultT result;
  char* peerVcP = NULL;
  char* encodedData = NULL;
  char* decodedData = NULL;
  char* secret = strdup("thereisnospoon");

  result = seci_get_vcert(&peerVcP);
    if(result != SECI_OK) {
      printf("could not get vc:  %s \n",seciErrToString(result));
      goto free;
    }

   printf("test to encode the string thereisnospoon\n");
   result = seci_encode(peerVcP,strlen(secret),secret,&encodedData);
   printf("encode result = %s data = %s \n",seciErrToString(result),encodedData);


   if(result != SECI_OK)
     goto free;

   result = seci_decode(peerVcP,strlen(encodedData),encodedData,&decodedData);
   printf("decode result = %s data = %s \n",seciErrToString(result),decodedData);





  free:
       seci_free(peerVcP);
       seci_free(encodedData);
       seci_free(decodedData);
}


void usage()
{
  printf("Usage: vctest get_cert <secret>|get_vc <secret>|get_vcert|verify_vcert|encode\n");
}

int main(int argc, char **argv) {

  if(argc < 2) {
    usage();
    return 0;
  }

  if(strcmp("get_cert",argv[1]) == 0) {
    get_cert(argv[2]);
  } else if(strcmp("get_vc",argv[1]) == 0) {
    get_vc_from_seci(argv[2]);
  } else if (strcmp("get_vcert",argv[1]) == 0) {
    get_vcert();
  }else if (strcmp("verify_vcert",argv[1]) == 0) {
    test_verify_own_vc();
  }
  else if (strcmp("encode",argv[1]) == 0) {
      test_encode();
    }
  else {
    usage();
  }

  return 0;

}




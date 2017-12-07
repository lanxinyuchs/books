/* ----------------------------------------------------------------------
 * %CCaseFile:  cert_file_vc.h %
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
 *            20160713    enenteo     Coverity warnings updates
 *            20161012    ehsake      Read keyfile twice. Corrected
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE /* required to remove warning for asprintf */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <cert_seci.h>
#include <cert_trace.h>


#define VC_CERT "/rcs/cert/vc/vc.crt"
#define VC_KEY  "/rcs/cert/vc/vc.key"


static char*
get_rcs_root()
{

  if(getenv("RCS_ROOT") == NULL)
  {
    return "";
  } else  {
    return getenv("RCS_ROOT");
  }
}

static long
get_file_size(char* fileName)
{
  long size = 0;
  FILE *fp = fopen(fileName, "r");

  if (fp != NULL) {
    if (fseek(fp, 0L, SEEK_END) == 0) {
      size = ftell(fp);
    }

    fclose(fp);
  }
  return size;

}

static size_t
read_file (char* fileName,long bytesToRead, char** outP)
{

  char *source = NULL;
  size_t readLength = 0;
  FILE *fp = fopen(fileName, "r");
  if (fp != NULL)
    {
      source = malloc(sizeof(char) * (bytesToRead));
      readLength = fread(source, sizeof(char), bytesToRead, fp);
      if (readLength == 0) {
	INFO("Error reading file %s", fileName);
	free(source);
      } else {
	*outP = source;
      }
      fclose(fp);
    }
  else {
    INFO("Could not open %s errno = %d",fileName,errno);
  }
  return readLength;
}


SeciResultT
get_vc_from_file(char** resultP)
{

  SeciResultT retCode;
  char* certData = NULL;
  char* keyData = NULL;
  long certSize, keySize = 0;
  char certFileName[255],keyFileName[255];
  char* ptr;

  DEBUG("get_vc_from_file%s","");

  sprintf(certFileName,"%s%s", get_rcs_root(), VC_CERT);
  certSize = get_file_size(certFileName);
  
  sprintf(keyFileName,"%s%s",get_rcs_root(),VC_KEY);
  keySize = get_file_size(keyFileName);

  if((certSize>0)&&(keySize>0)) 
    {
      read_file(certFileName,certSize,&certData);
      read_file(keyFileName,keySize,&keyData);
    }
  else
    {
      DEBUG("cert or key file size is not > 0, certSize=%ld, keySize=%ld",
           	    certSize,
	            keySize);
    }
  
  if(certData != NULL && keyData != NULL)
    {
      DEBUG("cert and key read, total %ld bytes",certSize+keySize);
      *resultP = (char*)malloc(certSize+keySize+1);
      if(*resultP != NULL)
	{
	  ptr = *resultP;
	  /* adding one byte for null termination */
	  memset(*resultP,0,certSize+keySize+1);
	  /*copy key first to have same order as in real vc */
	  memcpy(*resultP,keyData,keySize);
	  ptr+=keySize;
	  memcpy(ptr,certData,certSize);
	  retCode = SECI_OK;
	} else {
	DEBUG("malloc failed%s","");
	retCode = SECI_ERROR;
      }

    }
  else
    {
      retCode = SECI_NOT_FOUND;
    }


  free(certData);
  free(keyData);

  DEBUG("Leaving get_vc result=%d",retCode);

  return retCode;
}








/* ----------------------------------------------------------------------
 * %CCaseFile:	fi.c %
 * %CCaseRev:	/main/R3A/R6A/R7A/R10A/R11A/1 %
 * %CCaseDate:	2017-08-10 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Implementation of the File interface.
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
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R3A/7      2014-09-17 etxpejn
 * R3A/8      2015-01-07 erarafo     fiOpenWrite(), work in progress
 * R3A/9      2015-01-09 erarafo     fiOpenWrite(), further work in progress
 * R3A/10     2015-01-12 erarafo     Parameters settable for testing
 * R3A/11     2015-01-14 erarafo     Support symlink creation by sysFi
 * R3A/12     2015-01-22 erarafo     Cleanup after COLI parameter setting
 * R6A/1      2016-08-09 erarafo     Avoiding a coverity warning
 * R7A/1      2016-10-17 etxpejn     Added fiExportFile2 & fiImportFile2
 * R10A/1     2017-05-29 ejinfeg     Added fiImportFile3
*  R10A/2     2017-06-03 ejinfeg     Added fiImportFile3, fix crash issue 
*  R11A/1     2017-08-10 etxpejn     HW13739 Prolong CEC_TIMEOUT_MILLIS to avoid timeout 
* ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "cec.h"
#include "fi.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdbool.h>
#include <time.h>

#define FI_IMPORT_FILE      0
#define FI_EXPORT_FILE      1
#define FI_SPACE_AVAILABLE        2
#define FI_IMPORT_FILE_2      3
#define FI_EXPORT_FILE_2      4
#define FI_IMPORT_FILE_3      5

// for tests only
#define FI_SET_PARAMS     101

#define CEC_TIMEOUT_MILLIS 300000

typedef struct
{
  cec_handle_t *handle;
} FiProxyData;

static char signature[] = {'F', 'I'};


FiResultT
fiExportFile(const char *Password, const char *Uri, const char *Fullpath)
{
  FiProxyData *fiProxyP = NULL;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;
  int send_length;
  char *ptr;
  FiResultT result = FI_OK;
  /* milliseconds*/


  int pwSize = strlen(Password);
  int uriSize = strlen(Uri);
  int fullpathSize = strlen(Fullpath);

  fiProxyP = malloc(sizeof *fiProxyP);
  fiProxyP->handle = cec_open(signature, sizeof(signature));
   
  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t) + pwSize + uriSize + fullpathSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = FI_EXPORT_FILE;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = pwSize;
  ptr += sizeof(uint32_t);
 
  *(uint32_t*)ptr = uriSize;
  ptr += sizeof(uint32_t);

  memcpy(ptr, Password, pwSize);

  ptr += pwSize;

  memcpy(ptr, Uri, uriSize);

  ptr += uriSize;
  memcpy(ptr, Fullpath, fullpathSize);

  /* Send the request and wait for the reply */
  if (cec_send(fiProxyP->handle, &send_packet) == -1) 
    {
      result = FI_SEND_ERROR;
    }
  else 
    {
      if (cec_receive_w_tmeout(fiProxyP->handle, &recv_packet, CEC_TIMEOUT_MILLIS) == -1)
	{
	  result = FI_RECEIVE_ERROR;
	}
      else 
	{
	  ptr = recv_packet.data;
	  result = *(uint32_t *)ptr;
	  free(recv_packet.data);
	}
    }
  free(send_packet.data);

  cec_close(fiProxyP->handle);
  free(fiProxyP);
    
  return result;
}

FiResultT
fiExportFile2(const char *EncryptPassword, const char *Uri, const char *Fullpath)
{
  FiProxyData *fiProxyP = NULL;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;
  int send_length;
  char *ptr;
  FiResultT result = FI_OK;
  /* milliseconds*/


  int pwSize = strlen(EncryptPassword);
  int uriSize = strlen(Uri);
  int fullpathSize = strlen(Fullpath);

  fiProxyP = malloc(sizeof *fiProxyP);
  fiProxyP->handle = cec_open(signature, sizeof(signature));
   
  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t) + pwSize + uriSize + fullpathSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = FI_EXPORT_FILE_2;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = pwSize;
  ptr += sizeof(uint32_t);
 
  *(uint32_t*)ptr = uriSize;
  ptr += sizeof(uint32_t);

  memcpy(ptr, EncryptPassword, pwSize);

  ptr += pwSize;

  memcpy(ptr, Uri, uriSize);

  ptr += uriSize;
  memcpy(ptr, Fullpath, fullpathSize);

  /* Send the request and wait for the reply */
  if (cec_send(fiProxyP->handle, &send_packet) == -1) 
    {
      result = FI_SEND_ERROR;
    }
  else 
    {
      if (cec_receive_w_tmeout(fiProxyP->handle, &recv_packet, CEC_TIMEOUT_MILLIS) == -1)
	{
	  result = FI_RECEIVE_ERROR;
	}
      else 
	{
	  ptr = recv_packet.data;
	  result = *(uint32_t *)ptr;
	  free(recv_packet.data);
	}
    }
  free(send_packet.data);

  cec_close(fiProxyP->handle);
  free(fiProxyP);
    
  return result;
}


FiResultT
fiImportFile(const char *Password, const char *Uri, const char *File)
{
  FiProxyData *fiProxyP = NULL;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;
  int send_length;
  char *ptr;
  FiResultT result = FI_OK;  // FIXME, this assignment has no effect


  int pwSize = strlen(Password);
  int uriSize = strlen(Uri);
  int fileSize = strlen(File);

  fiProxyP = malloc(sizeof *fiProxyP);
  fiProxyP->handle = cec_open(signature, sizeof(signature));
   
  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t) + pwSize + uriSize + fileSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = FI_IMPORT_FILE;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = pwSize;
  ptr += sizeof(uint32_t);
 
  *(uint32_t*)ptr = uriSize;
  ptr += sizeof(uint32_t);

  memcpy(ptr, Password, pwSize);

  ptr += pwSize;

  memcpy(ptr, Uri, uriSize);

  ptr += uriSize;
  memcpy(ptr, File, fileSize);

  /* Send the request and wait for the reply */
  if (cec_send(fiProxyP->handle, &send_packet) == -1) 
    {
      result = FI_SEND_ERROR;
    }
  else 
    {
      if (cec_receive_w_tmeout(fiProxyP->handle, &recv_packet, CEC_TIMEOUT_MILLIS) == -1)
	{
	  result = FI_RECEIVE_ERROR;
	}
      else 
	{
	  ptr = recv_packet.data;
	  result = *(uint32_t *)ptr;
	  free(recv_packet.data);
	}
    }
  free(send_packet.data);

  cec_close(fiProxyP->handle);
  free(fiProxyP);
    
  return result;
}

FiResultT
fiImportFile2(const char *EncryptPassword, const char *Uri, const char *File)
{

  FiProxyData *fiProxyP = NULL;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;
  int send_length;
  char *ptr;
  FiResultT result = FI_OK;  // FIXME, this assignment has no effect


  int pwSize = strlen(EncryptPassword);
  int uriSize = strlen(Uri);
  int fileSize = strlen(File);

  fiProxyP = malloc(sizeof *fiProxyP);
  fiProxyP->handle = cec_open(signature, sizeof(signature));
   
  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t) + pwSize + uriSize + fileSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = FI_IMPORT_FILE_2;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = pwSize;
  ptr += sizeof(uint32_t);
 
  *(uint32_t*)ptr = uriSize;
  ptr += sizeof(uint32_t);

  memcpy(ptr, EncryptPassword, pwSize);

  ptr += pwSize;

  memcpy(ptr, Uri, uriSize);

  ptr += uriSize;
  memcpy(ptr, File, fileSize);

  /* Send the request and wait for the reply */
  if (cec_send(fiProxyP->handle, &send_packet) == -1) 
    {
      result = FI_SEND_ERROR;
    }
  else 
    {
      if (cec_receive_w_tmeout(fiProxyP->handle, &recv_packet, CEC_TIMEOUT_MILLIS) == -1)
	{
	  result = FI_RECEIVE_ERROR;
	}
      else 
	{
	  ptr = recv_packet.data;
	  result = *(uint32_t *)ptr;
	  free(recv_packet.data);
	}
    }
  free(send_packet.data);

  cec_close(fiProxyP->handle);
  free(fiProxyP);
    
  return result;
}

FiResultT
fiImportFile3(const char *Password, const char *Uri, const char *File, uint64_t MaxSize, uint8_t EncryptedPwd)
{
  FiProxyData *fiProxyP = NULL;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;
  int send_length;
  char *ptr;
  FiResultT result = FI_OK;  // FIXME, this assignment has no effect


  int pwSize = strlen(Password);
  int uriSize = strlen(Uri);
  int fileSize = strlen(File);

  fiProxyP = malloc(sizeof *fiProxyP);
  fiProxyP->handle = cec_open(signature, sizeof(signature));
   
  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t)  + sizeof(uint32_t) + pwSize + uriSize + fileSize + sizeof(uint64_t) + sizeof(uint8_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = FI_IMPORT_FILE_3;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = pwSize;
  ptr += sizeof(uint32_t);
 
  *(uint32_t*)ptr = uriSize;
  ptr += sizeof(uint32_t);

  *(uint32_t*)ptr = fileSize;
  ptr += sizeof(uint32_t);

  memcpy(ptr, Password, pwSize);
  ptr += pwSize;

  memcpy(ptr, Uri, uriSize);
  ptr += uriSize;

  memcpy(ptr, File, fileSize);
  ptr += fileSize;

  memcpy(ptr, &MaxSize, sizeof(MaxSize));
  ptr += sizeof(uint64_t);

  memcpy(ptr, &EncryptedPwd, sizeof(EncryptedPwd));

  /* Send the request and wait for the reply */
  if (cec_send(fiProxyP->handle, &send_packet) == -1) 
    {
      result = FI_SEND_ERROR;
    }
  else 
    {
      if (cec_receive_w_tmeout(fiProxyP->handle, &recv_packet, CEC_TIMEOUT_MILLIS) == -1)
	{
	  result = FI_RECEIVE_ERROR;
	}
      else 
	{
	  ptr = recv_packet.data;
	  result = *(uint32_t *)ptr;
	  free(recv_packet.data);
	}
    }
  free(send_packet.data);

  cec_close(fiProxyP->handle);
  free(fiProxyP);

  return result;
}

void
fiFree(void* p)
{

  free(p);

  return;
}


// TODO, remove when finalized
//
//static void log_s(const char *logfile, const char *s) {
//  FILE *f = fopen(logfile, "a"); fprintf(f, "%s\n", s); fclose(f);
//}
//
//static void log_ss(const char *logfile, const char *s1, const char *s2) {
//  FILE *f = fopen(logfile, "a"); fprintf(f, "%s %s\n", s1, s2); fclose(f);
//}
//
//static void log_sd(const char *logfile, const char *s, const int j) {
//  FILE *f = fopen(logfile, "a"); fprintf(f, "%s %d\n", s, j); fclose(f);
//}
//
//static void log_su(const char *logfile, const char *s, const unsigned int j) {
//  FILE *f = fopen(logfile, "a"); fprintf(f, "%s %u\n", s, j); fclose(f);
//}


/**
 * Creates any number of directory levels under the given 'prefix'
 * directory. The prefix path must not end with a "/" (thus it cannot
 * be the "/" directory). The null-terminated components array specifies
 * slash-free subdirectory names.
 *
 * The prefix parameter may be given as NULL, in which case the
 * directories are constructed starting from the leftmost component.
 *
 * Directories are created as necessary, similar to the `mkdir -p'
 * command. The returned value is true if all directory levels were
 * created/verified and false otherwise.
 */
static bool makeDir(const char *prefix, const char **components) {
  if (*components == NULL) {
    return true;
  }
  else {
    // create the next pathname
    const unsigned int nextDirSize =
        prefix == NULL ?
            strlen(*components) :
            strlen(prefix) + 1 + strlen(*components);
    char nextDir[nextDirSize + 1];
    if (prefix == NULL) {
      strcpy(nextDir, *components);
    }
    else {
      strcpy(nextDir, prefix);
      strcat(nextDir, "/");
      strcat(nextDir, *components);
    }

    // check directory status
    struct stat statBuffer;
    int s = stat(nextDir, &statBuffer);
    if (s != 0 && errno == ENOENT) {
      //try to create the directory
      //log_ss(logfile, "try to create:", nextDir);
      int t = mkdir(nextDir, S_IRWXU|S_IRWXG|S_IRWXO);
      if (t != 0) {
        //result = errno;
        //log_sd(logfile, "failed to create:", errno);
        return false;
      }
      else {
        //log_s(logfile, "successful create!");
        return makeDir(nextDir, components+1);
      }
    }
    else if (s != 0) {
      // other error; give up
      //log_sd(logfile, "other error:", errno);
      return false;
    }
    else if (S_ISDIR(statBuffer.st_mode)) {
      // exists and is a directory
      return makeDir(nextDir, components+1);
    }
    else {
      // exists but is not a directory, give up
      //log_ss(logfile, "exists but is not a directory:", nextDir);
      //result = ENOTDIR;
      return false;
    }
  }
}


/**
 * Obtains the current filespace available for the given persistence type,
 * and the associated root directory path. Also pass the directory name
 * so that sysFi can check it and set up a symlink from the SFTP area.
 *
 * A value of zero for spaceAvail indicates that no more files may be opened
 * for writing.
 *
 * In case the return value was FI_OK the caller must deallocate the
 * buffer pointed to by *root.
 */
static FiResultT
spaceAvailable(
    FiPersistencyT persistence,
    const char *dir,
    uint32_t *spaceAvail,
    char **root) {
  cec_handle_t *h = cec_open(signature, sizeof(signature));
  if (h == NULL) {
    return FI_OPEN_ERROR;
  }
  else {
    unsigned int bufferSize = 2*sizeof(uint32_t) + strlen(dir);
    char buffer[bufferSize];
    uint32_t *p = (uint32_t *)buffer;
    *p = FI_SPACE_AVAILABLE;
    ++p;
    *p = persistence;
    ++p;
    strncpy((char *)p, dir, strlen(dir));
    cec_packet_t send_packet = { bufferSize, (void *)buffer };
    int cecSendResult = cec_send(h, &send_packet);
    if (cecSendResult == -1) {
      cec_close(h);
      return FI_SEND_ERROR;
    }
    else {
      cec_packet_t recv_packet;
      int rr = cec_receive_w_tmeout(h, &recv_packet, CEC_TIMEOUT_MILLIS);
      cec_close(h);
      if (rr == -1) {
        return FI_RECEIVE_ERROR;
      }
      else {
        uint32_t *q = (uint32_t *)recv_packet.data;
        *spaceAvail = *q;
        q++;
        FiResultT dirValid;
        dirValid = *q;
        q++;
        if (dirValid != FI_OK) {
          free(recv_packet.data);
          return dirValid;
        }
        else {
          uint32_t rootSize = *q;
          q++;
          *root = strndup((const char *)q, (size_t)rootSize);
          free(recv_packet.data);
          return FI_OK;
        }
      }
    }
  }
}


FiResultT
fiOpenWrite(
    FiPersistencyT persistence,
    const char *dir,
    const char *name,
    FILE **fp) {

//  TODO, remove when this feature is finalized ---------
//  char *logfile;
//  asprintf(&logfile, "%s/%s", getenv("LOG_DIR"), "fi_log.txt");
//  FILE *lf = fopen(logfile, "w");
//  fclose(lf);
// ------------------------------------------------------

  if (persistence == FI_PERSISTENT) {
    return FI_NOT_SUPPORTED;
  }
  else if (persistence != FI_VOLATILE) {
    return FI_INVALID_PARAMETER;
  }
  else {
    uint32_t spaceAvail;
    char *rootResult;
    FiResultT r = spaceAvailable(persistence, dir, &spaceAvail, &rootResult);
    if (r != FI_OK) {
      return r;
    }
    char root[strlen(rootResult)+1];
    strcpy(root, rootResult);
    free(rootResult);

//  log_su(logfile, "space available:", spaceAvail);
//  log_ss(logfile, "root:", root);

    if (spaceAvail == 0) {
      return FI_SPACE_FULL;
    }
    else {
      const char *components[] = {root, dir, NULL};
      bool b = makeDir(NULL, components);
      if (!b) {
        return FI_FAILED_TO_OBTAIN_DIRECTORY;
      }
      unsigned int absPathSize = 0;
      for (char **p = (char **)components; *p != NULL; p++) {
        absPathSize += (p == (char **)components ? 0 : 1) + strlen(*p);
      }
      absPathSize += 1 + strlen(name);
      char absPath[absPathSize + 1];
      strcpy(absPath, "");
      for (char **p = (char **)components; *p != NULL; p++) {
        if (p != (char **)components) {
          strcat(absPath, "/");
        }
        strcat(absPath, *p);
      }
      strcat(absPath, "/");
      strcat(absPath, name);
      FILE *f = fopen(absPath, "w");
      if (f == NULL) {
        return FI_FAILED_TO_OPEN_FILE;
      }
      else {
        *fp = f;
        return FI_OK;
      }
    }
  }
}

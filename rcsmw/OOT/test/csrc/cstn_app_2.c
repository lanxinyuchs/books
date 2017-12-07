/* ----------------------------------------------------------------------
 * %CCaseFile:	cstn_app_2.c %
 * %CCaseRev:	/main/R8A/R9A/3 %
 * %CCaseDate:	2017-01-25 %
 * %CCaseDocNo: %
 * Author:	eolaand
 *
 * Short description:
 * A simple application that tests the CSTN C interface
 *
 * Compile command (cd to OOT directory): 
 *   cm 
 *   cm cstn_app_2
 *
 * Execute commands:
 *   cd /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/OOT/OOT_CNX9013316/OOT_CAX1033675/out/tgt_i686/bin
 *   rcsapp.sh -i /repo/etxuser/sim ./cstn_app_2
 * 
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2017 All rights reserved.
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
 * R8A/1      2017-01-14 uabesvi     Created
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>
#include <sys/select.h>

#include <pthread.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

#include "cstnn.h"


#define VERBOSE true
#define VERBOSE_FILE stdout
#define LEASHING false

enum {
  INIT_1_ERROR = 10,
  INIT_2_ERROR = 20,
  UPDATE_ERROR = 30
};


void print_f(char *fmt) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt); }
void print_fs(char *fmt, char *s) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s); }
void print_fd(char *fmt, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, d); }
void print_fss(char *fmt, char *s, char *t) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, t); }
void print_fsd(char *fmt, char *s, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, d); }


CsTnResult cbOamDNtoNsNameAndIp(char *ldn, CsTnInfoT *cstnInfo) {
  char *kurt;
  char *ipAddr;


  fprintf(stderr, " cbOamDNtoNsNameAndIp   SLEEEEEEEPING %d\n", 4);

  sleep(10);

  kurt = "allan wallan";
  strcpy(cstnInfo->NsName, kurt); 

  ipAddr = "134.122.101.2";
  strcpy(cstnInfo->IpAddress, ipAddr); 

  return CSTN_OK;
}


CsTnResult sendUpdate(  CsTnHandleT handle, int scenarioLength)
{
  CsTnInfoT cstnInfo;

  bool more = true;
  int saveerr = 0;
  int r;
  int result;

  
  /* 
   * Send update
   */
  char *kurt;
  char *ipAddr;
  char *ldn = calloc(30, 8);
  
  char *me = "ManEl=1,SF=1,Oam=2";
  strcpy(ldn, me); 
  
  memset(cstnInfo.NsName, 0, CSTN_NSNAME_STRLEN);
  memset(cstnInfo.IpAddress, 0, INET6_ADDRSTRLEN);

  kurt = "kalle kurt";
  strcpy(cstnInfo.NsName, kurt); 

  ipAddr = "134.122.1.222";
  strcpy(cstnInfo.IpAddress, ipAddr); 


  result = CsTnUpdate(ldn, &cstnInfo);
  if (result != CSTN_OK) {
    fprintf(stderr, "failed to send update. Error: %d\n", result);
    free(ldn);
    return UPDATE_ERROR;
  }
  

  me = "ManEl=1,SF=1,Oam=3";
  strcpy(ldn, me); 

  memset(cstnInfo.NsName, 0, CSTN_NSNAME_STRLEN);
  memset(cstnInfo.IpAddress, 0, INET6_ADDRSTRLEN);

  kurt = "sally cally";
  strcpy(cstnInfo.NsName, kurt); 

  ipAddr = "134.122.1.111";
  strcpy(cstnInfo.IpAddress, ipAddr); 

  result = CsTnUpdate(ldn, &cstnInfo);
  if (result != CSTN_OK) {
    fprintf(stderr, "failed to send update. Error: %d\n", result);
    free(ldn);
    return UPDATE_ERROR;
  }


  me = "ManEl=1,SF=1,Oam=44";
  strcpy(ldn, me); 

  memset(cstnInfo.NsName, 0, CSTN_NSNAME_STRLEN);
  memset(cstnInfo.IpAddress, 0, INET6_ADDRSTRLEN);

  kurt = "helly belly";
  strcpy(cstnInfo.NsName, kurt); 

  ipAddr = "134.122.1189.189";
  strcpy(cstnInfo.IpAddress, ipAddr); 

  result = CsTnUpdate(ldn, &cstnInfo);
  if (result != CSTN_OK) {
    fprintf(stderr, "failed to send update. Error: %d\n", result);
    free(ldn);
    return UPDATE_ERROR;
  }

  free(ldn);
  
  time_t beginning2 = time(NULL);

  while (more && (time(NULL) - beginning2) < scenarioLength) {

    fprintf(stderr, "while loop %d\n", handle);

    /* 
     * Normally we should use infinity here but we need to check scenariolength
     */
    struct timeval timeout = {.tv_sec = 1};

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(handle, &rfds);

    r = select(handle + 1, &rfds, NULL, NULL, &timeout);
    if (r < 0) {
      saveerr = errno;
      fprintf(stderr, "error number: %d, name: %s\n", saveerr, strerror(saveerr));
      perror("select()\n");
      more = false;
    }
    else if (r == 0) {
      fprintf(stderr, "select timeout\n");
    }
    else if (r == 1) {
      int x = FD_ISSET(handle, &rfds);

      fprintf(stderr, "ISSET %d\n", x);

      fprintf(stderr, "data on socket, time to dispatch\n");
      result = CsTnDispatch();
      fprintf(stderr, "data on socket, dispatch  %d\n", result);
      if (result != CSTN_OK) {
	/*
	 * This is probably an unexpected tcp close
	 */
	fprintf(stderr, "failed to dispatch %d\n", result);
	/*
	 * Don't exit here even if socket is closed. 
	 * Finalize is needed to free allocated memory.
	 */
	more = false;
      }
    }
    else {
      fprintf(stderr, "confused!!!\n");
    }
  }

  return result;

}









int main(int argc, char *argv[]) 
{
  CsTnHandleT handle;
  CsTnCallbacks2T callback;


  bool more = true;
  int saveerr = 0;
  int r;
  int result;

  time_t beginning = time(NULL);

  int scenarioLength; 
  if (argc > 1) 
    scenarioLength = atoi(argv[1]);
  else
    scenarioLength = 10;

  /* 
   * Initialize the session
   */

  callback.OamDNtoNsNameAndIp = cbOamDNtoNsNameAndIp;

  handle = CsTnInitialize2(&callback);
  if (handle == CSTN_FAIL) {
    fprintf(stderr, "failed to initialize CSTN: %d\n", handle);
    return INIT_2_ERROR;
  }
  
  while (more && (time(NULL) - beginning) < scenarioLength) {

    fprintf(stderr, "while loop %d\n", handle);

    /* 
     * Normally we should use infinity here but we need to check scenariolength
     */
    struct timeval timeout = {.tv_sec = 1};

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(handle, &rfds);



    r = select(handle + 1, &rfds, NULL, NULL, &timeout);
    if (r < 0) {
      saveerr = errno;
      fprintf(stderr, "error number: %d, name: %s\n", saveerr, strerror(saveerr));
      perror("select()\n");
      more = false;
    }
    else if (r == 0) {
      fprintf(stderr, "select timeout\n");
    }
    else if (r == 1) {
      int x = FD_ISSET(handle, &rfds);

      fprintf(stderr, "ISSET %d\n", x);

      fprintf(stderr, "data on socket, time to dispatch\n");
      result = CsTnDispatch();
      fprintf(stderr, "data on socket, dispatch  %d\n", result);
      if (result != CSTN_OK) {
	/*
	 * This is probably an unexpected tcp close
	 */
	fprintf(stderr, "failed to dispatch %d\n", result);
	/*
	 * Don't exit here even if socket is closed. 
	 * Finalize is needed to free allocated memory.
	 */
	more = false;
      }
    }
    else {
      fprintf(stderr, "confused!!!\n");
    }
  }



  fprintf(stderr, "sleep before sending update: %d\n", 2);
  sleep(2);

  sendUpdate(handle, scenarioLength);






  while (more && (time(NULL) - beginning) < scenarioLength) {

    fprintf(stderr, "while loop %d\n", handle);

    /* 
     * Normally we should use infinity here but we need to check scenariolength
     */
    struct timeval timeout = {.tv_sec = 1};

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(handle, &rfds);



    r = select(handle + 1, &rfds, NULL, NULL, &timeout);
    if (r < 0) {
      saveerr = errno;
      fprintf(stderr, "error number: %d, name: %s\n", saveerr, strerror(saveerr));
      perror("select()\n");
      more = false;
    }
    else if (r == 0) {
      fprintf(stderr, "select timeout\n");
    }
    else if (r == 1) {
      int x = FD_ISSET(handle, &rfds);

      fprintf(stderr, "ISSET %d\n", x);

      fprintf(stderr, "data on socket, time to dispatch\n");
      result = CsTnDispatch();
      fprintf(stderr, "data on socket, dispatch  %d\n", result);
      if (result != CSTN_OK) {
	/*
	 * This is probably an unexpected tcp close
	 */
	fprintf(stderr, "failed to dispatch %d\n", result);
	/*
	 * Don't exit here even if socket is closed. 
	 * Finalize is needed to free allocated memory.
	 */
	more = false;
      }
    }
    else {
      fprintf(stderr, "confused!!!\n");
    }
  }



  fprintf(stderr, "sleep before sending update: %d\n", 2);
  sleep(2);

  sendUpdate(handle, scenarioLength);

  
  fprintf(stderr, "stopped\n");




  return 0;
}



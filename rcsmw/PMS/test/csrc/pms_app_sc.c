/* ----------------------------------------------------------------------
 * %CCaseFile:	pms_app_sc.c %
 * %CCaseRev:	/main/R2A/R3A/1 %
 * %CCaseDate:	2015-01-19 %
 * %CCaseDocNo: %
 * Author:	eolaand
 *
 * Short description:
 * A simple application that tests the PM C interface, including show counters.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
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
 * R2A/1      2014-04-24 eolaand     Created
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

#include "pms_pmi.h"


#define VERBOSE true
#define VERBOSE_FILE stdout
#define LEASHING false


void print_f(char *fmt) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt); }
void print_fs(char *fmt, char *s) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s); }
void print_fd(char *fmt, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, d); }
void print_fss(char *fmt, char *s, char *t) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, t); }
void print_fsd(char *fmt, char *s, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, d); }


static bool isSubscribed = false;

static PmiHandleT handle = NULL;

void
subscribe(PmiHandleT handle, unsigned int GP, CounterSpecT **counterSpecs) {

  (void) handle;
  (void) GP;
  (void) counterSpecs;
  printf("CALLBACK: subscribe\n");

  print_fd("gran period: %d\n", GP);

  int mm = 0;
  for (CounterSpecT **p = counterSpecs; *p != NULL; p++) {
    mm++;
  }
  print_fd("n of groups seen by app: %d\n", mm); 
  
  for (CounterSpecT **p = counterSpecs; *p != NULL; p++) {
    
    print_fs("group: %s\n", (char *) (*p)->pmGroup);
    
    for (char **q = (char **) (*p)->measurementTypes; *q != NULL; q++) {
      print_fs("type: %s\n", *q);
    }
  }
  
  isSubscribed = true;
  printf("subscribe ready!\n");
}


void
report(PmiHandleT handle, unsigned int granularityPeriod, time_t timeSpec, time_t deadline) {

  (void) deadline;

  printf("CALLBACK: report\n");

  print_fd("GP: %d\n", granularityPeriod);
  print_fd("timeSpec: %d\n", timeSpec);
  print_fd("deadline: %d\n", deadline);
  print_fd("(deadline - timeSpec): %d\n", deadline-timeSpec);
  printf("send data\n");

  long long type1Counts[] = {452, 407, 411};
  MeasurementValueT type1Meas = {"Type1", 3, type1Counts};
  
  long long type2Count[] = {9};
  MeasurementValueT type2Meas = {"Type2", 1, type2Count};
  
  long long type3Counts[] = {776332, 795113};
  MeasurementValueT type3Meas = {"Type3", 1, type3Counts};
  
  MeasurementValueT *group1Meas[] = {&type1Meas, &type2Meas, NULL};
  
  ValueBundleT group1 = {"Group1", group1Meas};
  
  MeasurementValueT *group2Meas[] = {&type3Meas, NULL};
  
  ValueBundleT group2 = {"Group2", group2Meas};
  
  ValueBundleT *bundles[] = {&group1, &group2, NULL};
  
  if (pmiData(handle, granularityPeriod, timeSpec, "ME=1,PC=1", bundles) != PMI_OK) {
    print_fs("%sfailed to send data\n", "");
  }
  printf("report ready!\n");
}

void
report_sc(PmiHandleT handle, 
	  const unsigned int requestId, 
	  const char *moInstLdn,
	  const unsigned int maxTime) {
  
  (void) maxTime;
  (void) moInstLdn;

  printf("CALLBACK: reportShowCounters\n");

  print_fd("Req Id: %d\n", requestId);
  print_fs("MO instance LDN: %s\n", (char *) moInstLdn);
  print_fd("Max Report time: %d\n", maxTime);
  printf("send ShowCounters data\n");

  
  unsigned int result = 0;

  char * errorstr = "";

  long long type1Counts[] = {452, 407, 411};
  MeasurementValueT type1Meas = {"Type1", 3, type1Counts};
  
  long long type2Count[] = {9};
  MeasurementValueT type2Meas = {"Type2", 1, type2Count};

  MeasurementValueT *bundles[] = {&type1Meas, &type2Meas, NULL};


  if (pmiDataShowCounters(handle, requestId, result, errorstr, bundles) 
      != PMI_OK) {
    print_fs("%sfailed to send data\n", "");
  }
}


int main(int argc, char *argv[]) 
{  
  // Everything is done in the main thread.

  int scenarioLength; 
  if (argc > 1) 
    scenarioLength = atoi(argv[1]);
  else
    scenarioLength = 20;

  print_fd("scenario length: %d\n", scenarioLength);
  
  
  PmiCallbacksT_2 callbacks = {
    .pmiSubscribeCallback = &subscribe,
    .pmiReportCallback = &report,
    .pmiReportShowCountersCallback = &report_sc};
  
  printf("initialize\n");

  const char *topMoLdn = "ManagedElement=1,Transport=1";
  const char *pmGroups[] = {"Group1", "Group2", NULL};
  
  PmiResultT res;

  res = pmiInitialize_2(&handle, &callbacks, pmGroups, topMoLdn);
  if (res != PMI_OK) {
    printf("failed to initialize PMI: %d\n", res);
    return EXIT_FAILURE;
  }
  
  time_t beginning = time(NULL);
  
  printf("done initialize\n");

  PmiSelectionObjectT selectionObject;


  if (pmiSelectionObjectGet(handle, &selectionObject) != PMI_OK) {
    printf("failed to get selection object\n");
    return EXIT_FAILURE;
  }
  print_fd("got selection object: %d\n", selectionObject);

  bool more = true;
  int saveerr = 0;

  while (more && (time(NULL) - beginning) < scenarioLength) {
    // Normally we should use infinity here but we need to check scenariolength
    struct timeval timeout = {.tv_sec = 1};
    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(selectionObject, &rfds);
    int r = select(selectionObject+1, &rfds, NULL, NULL, &timeout);
    if (r < 0) {
      saveerr = errno;
      printf("error number: %d, name: %s\n", saveerr, strerror(saveerr));
      perror("select()\n");
      more = false;
    }
    else if (r == 0) {
      printf("select timeout\n");
    }
    else if (r == 1) {
      printf("data on socket, time to dispatch\n");
      if (pmiDispatch(handle) != PMI_OK) {
	// This is probably an unexpected tcp close
	printf("failed to dispatch\n");
	// Don't exit here even if socket is closed. 
	// Finalize is needed to free allocated memory.
	more = false;
      }
    }
    else {
      printf("confused!!!\n");
    }
  }
  
  printf("finalize\n");

  if (pmiFinalize(handle) != PMI_OK) {
    // This can happen if the socket is closed but it's ok.
    // The memory is free'd and we still get an error return due to the 
    // closed socket.
    printf("failed to finalize PMI\n");
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}




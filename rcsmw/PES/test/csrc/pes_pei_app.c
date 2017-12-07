/* ----------------------------------------------------------------------
 * %CCaseFile:	pes_pei_app.c %
 * %CCaseRev:	/main/R3A/9 %
 * %CCaseDate:	2014-12-03 %
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
 * Copyright (c) Ericsson AB 2014 All rights reserved.
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
 * R3A/1      2014-11-14 uabesvi     Created
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

#include "pes_pei.h"


#define VERBOSE true
#define VERBOSE_FILE stdout
#define LEASHING false


void print_f(char *fmt) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt); }
void print_fs(char *fmt, char *s) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s); }
void print_fd(char *fmt, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, d); }
void print_fss(char *fmt, char *s, char *t) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, t); }
void print_fsd(char *fmt, char *s, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, d); }



static PeiHandleT handle = NULL;

void
event_job(PeiHandleT handle,
	  char *eventProducerId,
	  char *eventJobId,
	  uint32_t requestedJobState,
	  PeiEventTypeT const *eventTypes,
	  PeiEventFilterT const * const *eventFilters,
	  PeiFileControlT *fileControl,
	  PeiStreamControlT *streamControl,
	  void *userData) {
  
  (void) handle;

  printf("CALLBACK: event job request\n");
  print_fs("Produer id: %s\n", eventProducerId);
  print_fs("Job id: %s\n", eventJobId);

  print_fd("Job state: %d\n", requestedJobState);

  if (eventTypes == NULL) 
    print_f("Event types NULL\n");
  else {
    print_fd("Noof event types: %d\n", eventTypes->noofTypeAliases);
    for (uint32_t i = 0; i < eventTypes->noofTypeAliases; i++) {
      print_fd("Type: %d\n", eventTypes->typeAliases[i]);
    }
  }


  if (*eventFilters == NULL) 
    print_f("Event filters NULL\n");
  else
    for (PeiEventFilterT const * const *p = eventFilters; *p != NULL; p++) {
      print_fs("Filter name: %s\n", (*p)->name);
      print_fs("Filter value: %s\n", (*p)->value);
    }
   

  if (fileControl == NULL)
    print_f("File control NULL\n");
  else {
      print_fd("RP: %d\n", fileControl->reportingPeriod);
      if (fileControl->compressionType == NULL) 
	print_f("File Compression type: NULL\n");
      else
	print_fd("File Compression type: %d\n", 
		 fileControl->compressionType->value);
  }
  

  if (streamControl == NULL)
    print_f("Stream control NULL\n");
  else {
      if (streamControl->compressionType == NULL) 
	print_f("Stream Compression type: NULL\n");
      else
	print_fd("Stream Compression type: %d\n", 
		 streamControl->compressionType->value);

      print_fs("IP Address: %s\n", streamControl->destinationIpAddress);
      print_fd("Port: %d\n", streamControl->destinationPort);
  }
  

  print_fs("userData: %s\n", (char *)userData);
  printf("Event job request received!\n");
}



void
me_attr_update(PeiHandleT handle,
	       char *userLabel,
	       char *networkManagedElementId,
	       void *userData) {
  
  (void) handle;
  
  printf("CALLBACK: ME ATTRIBUTE UPDATE\n");
  print_fs("User Label: %s\n", userLabel);
  print_fs("networkManagedElementId: %s\n", networkManagedElementId);
  print_fs("User Data: %s\n", userData);

}


int main(int argc, char *argv[]) 
{  
  // Everything is done in the main thread.

  char userDataEventJob[] = "event job user data"; 
  char userDataMeAttrUpdate[] = "me attr user data"; 

  int scenarioLength; 
  if (argc > 1) 
    scenarioLength = atoi(argv[1]);
  else
    scenarioLength = 15;

  print_fd("scenario length: %d\n", scenarioLength);
  

  PeiEventJobCallbackSpecT jobCbSpec = {
    .eventJobCallback = (PeiEventJobCallbackT) &event_job,
    .userData = &userDataEventJob};

  
  PeiMEAttrUpdateCallbackSpecT attrCbSpec = {
    .meAttrUpdateCallback = (PeiMEAttrUpdateCallbackT) &me_attr_update,
    .userData = &userDataMeAttrUpdate};

  
  PeiCallbacksT callbacks = {
    .eventJobCallbackSpec = &jobCbSpec,
    .meAttrUpdateCallbackSpec = &attrCbSpec};
 
  char *eventMapId = "fake";

  printf("initialize\n");

  PeiResultT res;

  /* 
   * Initialize the session
   */

  res = peiInitialize(&handle, eventMapId, &callbacks);
  if (res != PEI_OK) {
    printf("failed to initialize PEI: %d\n", res);
    return EXIT_FAILURE;
  }


  time_t beginning = time(NULL);
  
  printf("done initialize\n");

  PeiSelectionObjectT selectionObject;

  if (peiSelectionObjectGet(handle, &selectionObject) != PEI_OK) {
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
      int result = peiDispatch(handle);
      if (result != PEI_OK) {
	// This is probably an unexpected tcp close
	printf("failed to dispatch %d\n", result);
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

  if (peiFinalize(handle) != PEI_OK) {
    // This can happen if the socket is closed but it's ok.
    // The memory is free'd and we still get an error return due to the 
    // closed socket.
    printf("failed to finalize PEI\n");
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}



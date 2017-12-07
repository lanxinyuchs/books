/* ----------------------------------------------------------------------
 * %CCaseFile:	pms_pmi2_app.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/R6A/R9A/1 %
 * %CCaseDate:	2017-02-21 %
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
#include <stdint.h>

#include "pms_pmi2.h"


#define VERBOSE true
#define VERBOSE_FILE stdout
#define LEASHING false


void print_f(char *fmt) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt); }
void print_fs(char *fmt, char *s) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s); }
void print_fd(char *fmt, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, d); }
void print_fss(char *fmt, char *s, char *t) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, t); }
void print_fsd(char *fmt, char *s, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, d); }


static bool isSubscribed = false;

static Pmi2HandleT handle = NULL;

void
subscribe_rop(Pmi2HandleT handle, uint32_t GP, Pmi2SubscribeRopSpecT const * const *subscrSpecs, void *userData) {

  (void) handle;

  printf("CALLBACK: subscribe rop\n");
  print_fd("granularity period: %d\n", GP);

  int mm = 0;
  for (Pmi2SubscribeRopSpecT const * const *p = subscrSpecs; *p != NULL; p++) {
    mm++;
  }
  print_fd("number of subscribe specs: %d\n", mm); 

  for (Pmi2SubscribeRopSpecT const * const *p = subscrSpecs; *p != NULL; p++) {
    
    print_fd("groupIdAlias: %d\n", (*p)->pmGroupIdAlias);

    uint32_t noofMtIdAliases = (*p)->noofMeasurementTypeIdAliases;
    print_fd("noofMeasurementTypeIdAliases: %d\n", noofMtIdAliases);
    
    uint32_t *mtPtr = (uint32_t *) (*p)->measurementTypeIdAliases;
    
    for (uint32_t i = 0; i < noofMtIdAliases; i++) {
      uint32_t mtIdA = *(uint32_t *)mtPtr;
      printf("mtIdA %d:  %d\n", i, mtIdA);
      mtPtr+= sizeof(uint32_t);
    }

  }
  
  isSubscribed = true;

  print_fs("userData: %s\n", (char *)userData);
  printf("subscribe rop ready!\n");
}




void
report_rop(Pmi2HandleT handle, uint32_t granularityPeriod, uint32_t reportId, uint32_t maxReportingTime, void *userData) {

  (void) userData;
  printf("CALLBACK: report\n");

  print_fd("GP: %d\n", granularityPeriod);
  print_fd("reportId: %d\n", reportId);
  print_fd("maxReportingTime: %d\n", maxReportingTime);
  printf("send report rop data\n");

  /*
   * Elements
   */
  int64_t type1Elements[] = {4, 2, 3};
/*   Pmi2MeasurementValueT type1MeasV = {1, 3, type1Elements}; */
  Pmi2MeasurementValueT type1MeasV = {1, 0, NULL};
  
  int64_t type2Elements[] = {999};
  Pmi2MeasurementValueT type2MeasV = {2, 1, type2Elements};
  
  int64_t type3Elements[] = {776332, 795113};
  Pmi2MeasurementValueT type3MeasV = {1, 2, type3Elements};
  
  /*
   * MeasValues
   */
  Pmi2MeasurementValueT const *inst1MeasV[] = {&type1MeasV, &type2MeasV, NULL};
  Pmi2MeasurementValueT const *inst2MeasV[] = {&type3MeasV, NULL};

  /*
   * MoInstances
   */
  Pmi2MoInstanceBundleT instBundle1 = {30, inst1MeasV};
  Pmi2MoInstanceBundleT instBundle2 = {29, inst2MeasV};

  Pmi2MoInstanceBundleT const *inst1Bundle[] = {&instBundle1, NULL};
  Pmi2MoInstanceBundleT const *inst2Bundle[] = {&instBundle2, NULL};
  
  /*
   * ValueBundles
   */
  Pmi2ValueBundleT group1 = {200, inst1Bundle};
  Pmi2ValueBundleT group2 = {100, inst2Bundle};
  

  Pmi2ValueBundleT const *bundles[] = {&group1, &group2, NULL};
  printf("invoke pmi2DataRop \n");
  
  if (pmi2DataRop(handle, granularityPeriod, reportId, bundles, true) != PMI2_OK) {
    print_fs("%sfailed to send report rop data\n", "");
  }
  printf("report ready!\n");
}

void
report_show_counters(Pmi2HandleT handle, 
		     const uint32_t reportId, 
		     const uint32_t moInstLdn,
		     Pmi2ShowCountersSpecT const * const *scSpecs, 
		     const uint32_t maxReportTime,
		     void *userData) {
  
  (void) userData;

  printf("CALLBACK: reportShowCounters\n");

  print_fd("Report Id: %d\n", reportId);
  print_fd("MO instance LDN: %d\n", moInstLdn);

  int mm = 0;
  for (Pmi2ShowCountersSpecT const * const *p = scSpecs; *p != NULL; p++) {
    mm++;
  }
  print_fd("number of show counters specs: %d\n", mm); 

  for (Pmi2ShowCountersSpecT const * const *p = scSpecs; *p != NULL; p++) {
    
    print_fd("groupIdAlias: %d\n", (*p)->pmGroupIdAlias);

    uint32_t noofMtIdAliases = (*p)->noofMeasurementTypeIdAliases;
    print_fd("noofMeasurementTypeIdAliases: %d\n", noofMtIdAliases);
    
    uint32_t *mtPtr = (uint32_t *) (*p)->measurementTypeIdAliases;
    
    for (uint32_t i = 0; i < noofMtIdAliases; i++) {
      uint32_t mtIdA = *(uint32_t *)mtPtr;
      printf("mtIdA %d:  %d\n", i, mtIdA);
      mtPtr+= sizeof(uint32_t);
    }
  }

  print_fd("Max Report time: %d\n", maxReportTime);
  printf("send ShowCounters data\n");
  
  uint32_t result = 0;

  char * errorstr = "";

  /*
   * Elements
   */
  int64_t type1Elements[] = {1};
  Pmi2MeasurementValueT type1MeasV = {1, 1, type1Elements};
  
  /*
   * MeasValues
   */
  Pmi2MeasurementValueT const *inst1MeasV[] = {&type1MeasV, NULL};

  /*
   * MoInstances
   */
  Pmi2MoInstanceBundleT instBundle1 = {3, inst1MeasV};

  Pmi2MoInstanceBundleT const *inst1Bundle[] = {&instBundle1, NULL};
  
  /*
   * ValueBundles
   */
  Pmi2ValueBundleT group1 = {100, inst1Bundle};




  int64_t type1Elements2[] = {7, -1, 34};
  Pmi2MeasurementValueT type1MeasV2 = {2, 3, type1Elements2};
  
  Pmi2MeasurementValueT const *inst1MeasV2[] = {&type1MeasV2, NULL};

  Pmi2MoInstanceBundleT instBundle2 = {3, inst1MeasV2};

  Pmi2MoInstanceBundleT const *inst1Bundle2[] = {&instBundle2, NULL};
  
  Pmi2ValueBundleT group2 = {200, inst1Bundle2};






  

  Pmi2ValueBundleT const *bundles[] = {&group1, &group2, NULL};
  printf("invoke pmi2DataShowCounters \n");
  

  /*
   * Elements
   */
  // int64_t type1Elements[] = {4, 2, 3};
  // Pmi2MeasurementValueT type1MeasV = {11, 3, type1Elements};
  
  // int64_t type2Elements[] = {999};
  // Pmi2MeasurementValueT type2MeasV = {22, 1, type2Elements};
  
  
  /*
   * MeasValues
   */
  // Pmi2MeasurementValueT const * const inst1MeasV[] = {&type1MeasV, &type2MeasV, NULL};


  //  if (pmi2DataShowCounters(handle, reportId, result, errorstr, inst1MeasV) 
  if (pmi2DataShowCounters(handle, reportId, result, errorstr, bundles) 
      != PMI2_OK) {
    print_fs("%sfailed to send data\n", "");
  }
}


int main(int argc, char *argv[]) 
{  
  // Everything is done in the main thread.

  char userDataSubscr[] = "subscribe user data"; 
  char userDataRop[] = "rop user data"; 
  char userDataSc[] = "show counters user data"; 

  int scenarioLength; 
  if (argc > 1) 
    scenarioLength = atoi(argv[1]);
  else
    scenarioLength = 10;

  print_fd("scenario length: %d\n", scenarioLength);
  

  Pmi2SubscribeRopCallbackSpecT subscrCbSpec = {
    .subscribeRopCallback = (Pmi2SubscribeRopCallbackT) &subscribe_rop,
    .userData = &userDataSubscr};

  Pmi2ReportRopCallbackSpecT ropCbSpec = {
    .reportRopCallback = (Pmi2ReportRopCallbackT) &report_rop,
    .userData = &userDataRop};

  Pmi2ReportShowCountersCallbackSpecT scCbSpec = {
    .reportShowCountersCallback = (Pmi2ReportShowCountersCallbackT) &report_show_counters,
    .userData = &userDataSc};

  Pmi2CallbacksT callbacks = {
    .subscribeRopCallbackSpec = &subscrCbSpec,
    .reportRopCallbackSpec = &ropCbSpec,
    .reportShowCountersCallbackSpec = &scCbSpec};
  
  printf("initialize\n");


  Pmi2ResultT res;

  /* 
   * Initialize the session
   */

/*   char aliasFile[] = "fake"; */
/*   res = pmi2Initialize(&handle, aliasFile, &callbacks); */
  res = pmi2Initialize(&handle, NULL, &callbacks);
  if (res != PMI2_OK) {
    printf("failed to initialize PMI2: %d\n", res);
    return EXIT_FAILURE;
  }

  /* 
   * Send counter maps
   */

  Pmi2MeasurementTypeMapT mtMapT1 = {
    .measurementTypeId = "Type1",
    .measurementTypeIdAlias = 1
  };

  Pmi2MeasurementTypeMapT const * const mtMaps1[] = {
    &mtMapT1,
    NULL,
   };

  Pmi2CounterMapT counterMapG1 = {
    .pmGroupId = "Group1",
    .pmGroupIdAlias = 100,
    .measurementTypes = mtMaps1
  };

  Pmi2MeasurementTypeMapT mtMapT2 = {
    .measurementTypeId = "Type2",
    .measurementTypeIdAlias = 1
  };

  Pmi2MeasurementTypeMapT mtMapT3 = {
    .measurementTypeId = "Type3",
    .measurementTypeIdAlias = 2
  };

  Pmi2MeasurementTypeMapT const * const mtMaps2[] = {
    &mtMapT2,
    &mtMapT3,
    NULL,
   };

  Pmi2CounterMapT counterMapG2 = {
    .pmGroupId = "Group2",
    .pmGroupIdAlias = 200,
    .measurementTypes = mtMaps2
  };
  
  Pmi2CounterMapT counterMapG3 = {
    .pmGroupId = "Group3",
    .pmGroupIdAlias = 300,
    .measurementTypes = NULL
  };
  
  Pmi2CounterMapT const * const counterMaps[] = {
    &counterMapG1,
    &counterMapG2,
    &counterMapG3,
    NULL,
   };

  res = pmi2CounterMap(handle, NULL);
  if (res != PMI2_OK) {
    printf("failed to send pmi2CounterMap NULL PMI2: %d\n", res);
    return EXIT_FAILURE;
  }

  printf("sent pmi2Countermap NULL PMI2: %d\n", res);

  res = pmi2CounterMap(handle, counterMaps);
  if (res != PMI2_OK) {
    printf("failed to send pmi2CounterMap PMI2: %d\n", res);
    return EXIT_FAILURE;
  }
  
  time_t beginning = time(NULL);
  
  printf("done initialize\n");

  Pmi2SelectionObjectT selectionObject;

  if (pmi2SelectionObjectGet(handle, &selectionObject) != PMI2_OK) {
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
      int result = pmi2Dispatch(handle);
      if (result != PMI2_OK) {
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

  if (pmi2Finalize(handle) != PMI2_OK) {
    // This can happen if the socket is closed but it's ok.
    // The memory is free'd and we still get an error return due to the 
    // closed socket.
    printf("failed to finalize PMI2\n");
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}



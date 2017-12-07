/* ----------------------------------------------------------------------
 * %CCaseFile:	test_trace.c %
 * %CCaseRev:	/main/R2A/3 %
 * %CCaseDate:	2014-02-19 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: A program that demonstrates tracing via LTTng.
 * This program is declared as a dynamic program in test_trace_*.xml.
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
 * R2A/1      2014-02-18 erarafo     Created
 * R2A/2      2014-02-18 erarafo     Going idle instead of exiting
 * R2A/3      2014-02-19 erarafo     Reporting probe version
 * ----------------------------------------------------------------------
 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdbool.h>
#include <unistd.h>
#include <stdio.h>
//#include <string.h>
#include <stdlib.h>

#include <dlfcn.h>

#define TRACEPOINT_DEFINE
#define TRACEPOINT_PROBE_DYNAMIC_LINKAGE
#include "test_trace.h"




#define LOG "test_trace.txt"

/**
 * Writes a progress message.
 */
static void
appLog(const char *mode, const char *text, const int number) {
  char *filename;
  asprintf(&filename, "%s/%s", getenv("LOG_DIR"), LOG);
  FILE *stream = fopen(filename, mode);
  free(filename);
  fprintf(stream, "%s: %d\n", text, number);
  fclose(stream);
}

/**
 * Writes a progress message; string argument
 */
static void
appLogS(const char *mode, const char *text, const char *message) {
  char *filename;
  asprintf(&filename, "%s/%s", getenv("LOG_DIR"), LOG);
  FILE *stream = fopen(filename, mode);
  free(filename);
  fprintf(stream, "%s: %s\n", text, message);
  fclose(stream);
}

/**
 * Does nothing more.
 */
static void
idle() {
  int lap = 0;
  while (true) {
    lap++;
    sleep(10);
    tracepoint(com_ericsson_rcs_test, idle, 77777);
    appLog("a", "being idle", lap);
  }
}


/**
 * Execution starts here.
 *
 * Progress messages are written to test_trace.txt in the
 * rcs/applicationlogs directory.
 */
int
main() {
  appLog("w", "started", 0);
  char *provider=getenv("TRACEPOINT_PROVIDER");
  if (provider == NULL) {
    appLog("a", "TRACEPOINT_PROVIDER undefined", 0);
    return 1;
  }
  else {
    appLogS("a", "TRACEPOINT_PROVIDER", provider);
  }

  void *lib=dlopen(provider, RTLD_LAZY);
  if (lib == NULL) {
    appLog("a", "dlopen failed", 0);
    return 2;
  }

  const char *(*funPointer)();
  dlerror();
  funPointer = (const char *(*)())dlsym(lib, "testTraceProbeVersion");
  char *lookupError = dlerror();
  if (lookupError != NULL) {
    appLogS("a", "lookup error", lookupError);
  }
  else {
    const char *versionString = (*funPointer)();
    appLogS("a", "test_trace_probe funPointer", versionString);
  }

  tracepoint(com_ericsson_rcs_test, starting, 35353);
  appLog("a", "done trace, going idle", 0);

  idle();
}

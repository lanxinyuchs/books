/* ----------------------------------------------------------------------
 * %CCaseFile:	pms_app.c %
 * %CCaseRev:	/main/R2A/10 %
 * %CCaseDate:	2013-02-13 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: erarafo
 *
 * Short description:
 * A simple application that tests the PM C interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013 All rights reserved.
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
 * R2A/2      2013-02-03   erarafo     Created
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
#include "pms_hashmap.h"

#include "seqdiagr.h"


#define VERBOSE true
#define VERBOSE_FILE stdout
#define LEASHING false


static int leashCount = 1;


void print_f(char *fmt) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt); }
void print_fs(char *fmt, char *s) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s); }
void print_fd(char *fmt, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, d); }
void print_fss(char *fmt, char *s, char *t) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, t); }
void print_fsd(char *fmt, char *s, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, d); }





/**
 * Hangs and waits for creation of files named /dev/shm/SIGNUM/leash-N,
 * where N starts from 1 and is stepped by 1. A small utility script
 * can be used to create the files easily. The purpose is to allow
 * stepping through the program for demo purposes.
 */
void
leash() {
	if (LEASHING) {
		char *fileName;
		asprintf(&fileName, "/dev/shm/%s/leash-%d", getenv("LOGNAME"), leashCount);
		leashCount++;

		int onLeash = 1;
		while (onLeash) {
			FILE *f = fopen(fileName, "r");
			if (f == NULL) {
				sleep(1);
			}
			else {
				onLeash = 0;
			}
		}
	}
}

static bool isSubscribed = false;

static PmiHandleT handle = NULL;

PmiResultT
subscribe(PmiHandleT handle, unsigned int GP, CounterSpecT **counterSpecs) {

	print_fs("%sCALLBACK: subscribe\n", spPlus());

	print_fsd("%sgran period: %d\n", spPlus(), GP);


	int mm = 0;
	for (CounterSpecT **p = counterSpecs; *p != NULL; p++) {
			mm++;
	}
	print_fsd("%sn of groups seen by app: %d\n", spPlus(), mm);



	for (CounterSpecT **p = counterSpecs; *p != NULL; p++) {

		print_fss("%s  group: %s\n", spPlus(), (*p)->pmGroup);

		for (char **q = (*p)->measurementTypes; *q != NULL; q++) {
			print_fss("%s    type: %s\n", spPlus(), *q);
		}

	}



	//printEvent(stdout, arrow(LEFT, RED, "subscribe"), spaces());
	printEvent(stdout, spaces(), arrow(RIGHT, RED, "subscribe"));
	isSubscribed = true;
	return PMI_OK;
}


/*
PmiResultT
unsubscribe() {
	printEvent(stdout, arrow(LEFT, RED, "unsubscribe"), spaces());
	isSubscribed = false;
	return PMI_OK;
}
*/


void
report(PmiHandleT handle, unsigned int granularityPeriod, time_t timeSpec, time_t deadline) {

	printEvent(stdout, spaces(), arrow(RIGHT, RED, "report"));
	print_fs("%sCALLBACK: report\n", spPlus());
	print_fsd("%sGP: %d\n", spPlus(), granularityPeriod);
	print_fsd("%stimeSpec: %d\n", spPlus(), timeSpec);
	print_fsd("%sdeadline: %d\n", spPlus(), deadline);
	print_fsd("%s(deadline - timeSpec): %d\n", spPlus(), deadline-timeSpec);

	printEvent(stdout, spaces(), arrow(LEFT, BLUE, "data"));

	int type1Counts[] = {452, 407, 411};
	MeasurementValueT type1Meas = {"Type1", 3, type1Counts};

	int type2Count[] = {9};
	MeasurementValueT type2Meas = {"Type2", 1, type2Count};

	int type3Counts[] = {776332, 795113};
	MeasurementValueT type3Meas = {"Type3", 1, type3Counts};

	MeasurementValueT *group1Meas[] = {&type1Meas, &type2Meas, NULL};

	ValueBundleT group1 = {"Group1", group1Meas};

	MeasurementValueT *group2Meas[] = {&type3Meas, NULL};

	ValueBundleT group2 = {"Group2", group2Meas};


	ValueBundleT *bundles[] = {&group1, &group2, NULL};

	if (pmiData(handle, granularityPeriod, timeSpec, "ME=1,PC=1", bundles) != PMI_OK) {
		print_fs("%sfailed to send data\n", spPlus());
	}
}


/**
 * This function runs in a thread of its own, performing the
 * dispatch calls.
 */
void *
dispatcher(void *singleArgument) {

	PmiSelectionObjectT selectionObject = *((PmiSelectionObjectT *)singleArgument);

	bool more = true;

	// don't bother timing out; just die when the Unix process does.

	while (more) {
		struct timeval timeout = {.tv_sec = 9999};
		fd_set rfds;
		FD_ZERO(&rfds);
		FD_SET(selectionObject, &rfds);
		int r = select(selectionObject+1, &rfds, NULL, NULL, &timeout);
		if (r < 0) {
			// maybe socket closed gets us here
			printf("error number: %d, name: %s\n", errno, strerror(errno));
			perror("select()\n");
			more = false;
		}
		else if (r == 0) {
			printEvent(stdout, spaces(), arrow(NEITHER, GREEN, "select timeout"));
		}
		else if (r == 1) {
			//printf("data on socket, time to dispatch\n");
			printEvent(stdout, spaces(), arrow(LEFT, BLUE, "dispatch"));
			if (pmiDispatch(handle) != PMI_OK) {
				print_fs("%sfailed to dispatch\n", spPlus());
				exit(EXIT_FAILURE);
			}
		}
		else {
			print_fs("%sconfused!!!\n", spPlus());
		}
	}
	return NULL;
}


int
main() {

	int scenarioLength = atoi(getenv("SCENARIO_LENGTH"));
	print_fd("scenario length: %d\n", scenarioLength);


	PmiCallbacksT callbacks = {
			.pmiSubscribeCallback = &subscribe,
			.pmiReportCallback = &report};

	leash();

	//printEvent(stdout, arrow(RIGHT, BLUE, "initialize"), spaces());
	printEvent(stdout, spaces(), arrow(LEFT, BLUE, "initialize"));
	char *pmGroups[] = {"Group1", "Group2", NULL};
	if (pmiInitialize(&handle, &callbacks, pmGroups) != PMI_OK) {
		print_fs("%sfailed to initialize PMI\n", spPlus());
		return EXIT_FAILURE;
	}

	time_t beginning = time(NULL);


	print_fs("%sdone initialize\n", spPlus());
	print_fss("%sgot handle: %s\n", spPlus(), handle);


	PmiSelectionObjectT selectionObject;

	leash();

	if (pmiSelectionObjectGet(handle, &selectionObject) != PMI_OK) {
		print_fs("%sfailed to get selection object\n", spPlus());
		return EXIT_FAILURE;
	}

	//printEvent(stdout, arrow(RIGHT, BLUE, "selectionObjGet"), spaces());
	printEvent(stdout, spaces(), arrow(LEFT, BLUE, "selectionObjGet"));


	print_fsd("%sgot selection object: %d\n", spPlus(), selectionObject);

	// spawn a separate thread for the dispatching

	pthread_t dispatchingThread;
	int r = pthread_create(&dispatchingThread, NULL, &dispatcher, (void *)&selectionObject);
	if (r != 0) {
		print_fd("thread creation failed, code: %d\n", r);
		return EXIT_FAILURE;
	}

	// wait for scenario to end
	while ((time(NULL) - beginning) < scenarioLength) {
		printEvent(stdout, spaces(), spaces());
		sleep(1);
	}

	printEvent(stdout, spaces(), arrow(LEFT, BLUE, "finalize"));

	if (pmiFinalize(handle) != PMI_OK) {
		print_fs("%sfailed to finalize PMI\n", spPlus());
		return EXIT_FAILURE;
	}

	// don't bother canceling the dispatcher thread; it could be done here

	return EXIT_SUCCESS;
}




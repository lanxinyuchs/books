/* ----------------------------------------------------------------------
 * %CCaseFile:	alarm_example_app.c %
 * %CCaseRev:	/main/R2A/R3A/2 %
 * %CCaseDate:	2015-01-30 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: An alarms example program. This program is used as
 * a code example in the alarms IWD.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
 * R2A/1      2013-11-21 erarafo     First version
 * R2A/3      2014-08-20 erarafo     Notification object DN corrected
 * R3A/1      2014-11-04 erarafo     Finalize session in case of error
 * R3A/2      2015-01-20 erarafo     Adjusted for alarm number change
 * ----------------------------------------------------------------------
 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdbool.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <saAis.h>
#include <saNtf.h>

#define VENDOR_ID_ERICSSON 193
#define MAJOR 140
#define MINOR_ALARM_65519 65519
#define MINOR_ALERT_65524 65524

#define MO_INST "testClass1Id=1,TESTMOMtestRootId=1"
#define LOG "alarm_example_app.txt"

/**
 * Writes a progress message.
 */
static void
appLog(const char *mode, const char *text, const int number) {
  char *filename;
  asprintf(&filename, "%s/%s", getenv("LOG_DIR"), LOG);
  FILE *stream = fopen(filename, mode);
  free(filename);
  fprintf(stream, "%s, %d\n", text, number);
  fclose(stream);
}

/**
 * Does nothing more.
 */
static void
idle() {
  while (true) {
    sleep(60);
  }
}

/**
 * Finalizes the session and goes idle.
 */
static void
finalizeAndIdle(SaNtfHandleT handle) {
  SaAisErrorT r = saNtfFinalize(handle);
  if (r != SA_AIS_OK) {
    appLog("a", "failed to finalize", r);
  }
  else {
    appLog("a", "finalized", r);
  }
  idle();
}

/**
 * Sends a notification.
 */
static SaAisErrorT
sendAlarm(SaNtfHandleT handle,
    SaUint16T minorId,
    SaNtfSeverityT severity,
    const char *additionalText) {

  SaNtfAlarmNotificationT notif;

  SaAisErrorT r1 =
      saNtfAlarmNotificationAllocate(
          handle,
          &notif,
          0,
          strlen(additionalText) + 1,
          0,
          0,
          0,
          0,
          SA_NTF_ALLOC_SYSTEM_LIMIT);

  if (r1 != SA_AIS_OK) {
    appLog("a", "failed to allocate", r1);
    return r1;
  }

  *(notif.notificationHeader.eventType) = SA_NTF_TYPE_ALARM;
  *(notif.notificationHeader.eventTime) = SA_TIME_UNKNOWN;

  memcpy(notif.notificationHeader.notificationObject->value,
      MO_INST,
      strlen(MO_INST));
  notif.notificationHeader.notificationObject->length = strlen(MO_INST);

  notif.notificationHeader.notificationClassId->vendorId = VENDOR_ID_ERICSSON;
  notif.notificationHeader.notificationClassId->majorId = MAJOR;
  notif.notificationHeader.notificationClassId->minorId = minorId;

  *(notif.perceivedSeverity) = severity;
  *(notif.trend) = SA_NTF_TREND_NO_CHANGE;
  *(notif.probableCause) = SA_NTF_UNSPECIFIED_REASON;

  strcpy(notif.notificationHeader.additionalText, additionalText);

  SaAisErrorT r2 = saNtfNotificationSend(notif.notificationHandle);
  if (r2 != SA_AIS_OK) {
    appLog("a", "failed to send notification", r2);
    return r2;
  }

  SaAisErrorT r3 = saNtfNotificationFree(notif.notificationHandle);
  if (r3 != SA_AIS_OK) {
    appLog("a", "failed to free", r3);
    return r3;
  }

  return r3;
}

/**
 * Execution starts here. The program opens an NTF session, raises an
 * alarm, clears it, sends an alert, and closes the session.
 *
 * The 4 seconds pause before clearing the alarm ensures that the
 * transient filter in COM does not kick in.
 *
 * Progress messages are written to alarm_example_app.txt in the
 * rcs/applicationlogs directory.
 */
int
main() {
  appLog("w", "started", 0);

  SaNtfHandleT handle;
  SaVersionT version = {'A', 0x01, 0x01};
  SaAisErrorT r1 = saNtfInitialize(&handle, NULL, &version);
  if (r1 != SA_AIS_OK) {
    appLog("a", "failed to initialize", r1);
    idle();
  }
  else {
    appLog("a", "initialized", r1);
  }

  SaAisErrorT r2 = sendAlarm(
      handle,
      MINOR_ALARM_65519,
      SA_NTF_SEVERITY_MAJOR,
      "");
  if (r2 != SA_AIS_OK) {
    appLog("a", "failed to send alarm", r2);
    finalizeAndIdle(handle);
  }
  else {
    appLog("a", "alarm sent", r2);
  }

  sleep(4);

  SaAisErrorT r3 = sendAlarm(
      handle,
      MINOR_ALARM_65519,
      SA_NTF_SEVERITY_CLEARED,
      "");
  if (r3 != SA_AIS_OK) {
    appLog("a", "failed to clear alarm", r3);
    finalizeAndIdle(handle);
  }
  else {
    appLog("a", "alarm cleared", r3);
  }

  sleep(4);

  SaAisErrorT r4 = sendAlarm(
      handle,
      MINOR_ALERT_65524,
      SA_NTF_SEVERITY_WARNING,
      "");
    if (r4 != SA_AIS_OK) {
      appLog("a", "failed to send alert", r4);
      finalizeAndIdle(handle);
    }
    else {
      appLog("a", "alert sent", r4);
    }

  SaAisErrorT r5 = saNtfFinalize(handle);
  if (r5 != SA_AIS_OK) {
    appLog("a", "failed to finalize", r5);
  }
  else {
    appLog("a", "finalized", r5);
  }

  idle();
}

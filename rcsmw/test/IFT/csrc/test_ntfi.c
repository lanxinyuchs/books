/* ----------------------------------------------------------------------
 * %CCaseFile:	test_ntfi.c %
 * %CCaseRev:	/main/R2A/R3A/R6A/R11A/R12A/1 %
 * %CCaseDate:	2017-11-07 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Adapter onto the SAF NTF interface for exercising alarms from
 * Erlang test suites.
 *
 * Test suites using this module are:
 *   $RCT_TOP/test/suites/SAF/ntf_alarm_via_ift_SUITE
 *   $RCS_TOP/COMSA/COMSA_CNX9012610/test/suites/status_led_SUITE
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
 * R2A/1      2013-11-07 erarafo     First version.
 * R2A/2      2013-11-08 erarafo     Comments elaborated.
 * R2A/3      2013-11-25 erarafo     Added start/stop of alarms example
 * R2A/5      2013-12-04 erarafo     Increased logging; handle 64-bits result
 * R2A/6      2013-12-06 erarafo     Log output added
 * R2A/7      2014-08-19 erarafo     Providing correct CXC no for ALAEXA
 * R2A/8      2014-09-19 erarafo     Collecting on-board traps
 * R2A/9      2014-09-22 erarafo     Adapted to changes in test_common
 * R2A/10     2014-09-22 erarafo     Cleanup
 * R2A/11     2014-09-23 erarafo     Cleanup
 * R3A/1      2014-09-25 erarafo     Steps towards additional info handling
 * R3A/2      2014-09-26 erarafo     Exercising all Additional Info data types
 * R3A/3      2014-09-26 erarafo     Optional 4th arg in getTailLines()
 * R3A/4      2014-09-30 erarafo     Refactored setting of integer values
 * R3A/5      2014-10-01 erarafo     Enforce big-endian storage in ARRAY data
 * R3A/6      2014-10-29 erarafo     Second Opinion adjustments
 * R3A/7      2014-11-25 erarafo     Dynamic Additional Info string
 * R3A/8      2014-12-18 erarafo     Added handleAlarmRaiseClear
 * R3A/9      2014-12-18 erarafo     Support both raise-clear and clear-raise
 * R3A/11     2015-03-23 erarafo     Support single raise and single clear too
 * R6A/1      2016-07-08 etxpeno     Coverity fixes
 * R11A/1     2017-08-29 elarrun     Added test for SP539 Alarm correlation,
 *                                   test mapping from NTF infoId to name
 * R12A/1     2017-08-29 elarrun     (Single RAT restart) Support for
 *                                   specifying program group in
 *                                   an alarm notification
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "master.h"

#include "appm_lmhi.h"

#include <stdio.h>
#include <stdint.h>
#include <float.h>
#include <endian.h>
#include <time.h>

#include <saAis.h>
#include <saNtf.h>

// Test suites must define these constants too
#define NTFI_INITIALIZE             1
#define NTFI_FINALIZE               2
#define NTFI_ALARM                  3
#define NTFI_ALARM_RAISE_CLEAR      4
#define NTFI_ALARM_CLEAR_RAISE      5
#define NTFI_ALARM_RAISE            6
#define NTFI_ALARM_CLEAR            7
#define NTFI_START_ALARMS_EXAMPLE  91
#define NTFI_STOP_ALARMS_EXAMPLE   92

#define NTFI_COUNT_LINES          101
#define NTFI_GET_TAIL_LINES       102

#define NTFI_EXPAND_STRING        901

#define VENDOR_ID_ERICSSON 193


#define ADD_INFO_NONE      0
//#define ADD_INFO_PRIMITIVE 0x0001
#define ADD_INFO_STRING    0x0002
//#define ADD_INFO_NAME      0x0004
//#define ADD_INFO_IPADDRESS 0x0008
//#define ADD_INFO_BINARY    0x0010
//#define ADD_INFO_ARRAY     0x0020
#define ADD_INFO_CORR_INFO 0x0040
#define ADD_INFO_ALARM_ID  0x0080
#define ADD_INFO_UUID      0x0100
#define ADD_INFO_DN3       0x0200
#define ADD_INFO_INFOID0   0x0400

// Specifies that an id+string should be
// parsed from the request
#define ADD_INFO_DYN_STRING     0x0800


// The following values, used for Additional Info,
// are checked by ntf_alarm_via_ift_SUITE.

//#define TESTVALUE_UINT8_1      250
//#define TESTVALUE_INT8_1       -100
//#define TESTVALUE_UINT16_1     65000
//#define TESTVALUE_INT16_1      -30000
//#define TESTVALUE_UINT32_1     4000000000
//#define TESTVALUE_INT32_1      -2000000000
//#define TESTVALUE_UINT64_1     18000000000000000000ull
//#define TESTVALUE_INT64_1      -9000000000000000000ll
//#define TESTVALUE_FLOAT_1      3.14e0f
//#define TESTVALUE_DOUBLE_1     DBL_MAX
#define TESTVALUE_STRING_1     "please get us some ladders real soon"
//#define TESTVALUE_IPADDRESS_1  "128.30.52.45"
//#define TESTVALUE_BINARY_1     {126, 127, 128, 129}
//#define TESTVALUE_NAME_1       "Oscar=2,Gustaf=5"
#define TESTVALUE_CORR_INFO_1  "CI={\"C\":\"191f0558-94bd-49d1-9851-ac99ceac95c2\",\"n\": \"RadioNode\"},\"P\": \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"}"
#define TESTVALUE_ALARM_ID_1   "alarmId=4711"
#define TESTVALUE_UUID_1       "UUID=191f0558-94bd-49d1-9851-ac99ceac95c2"
#define TESTVALUE_DN3_1        "DN3=SubNetwork=LteNetwork,ManagedElement=11300_RadioNode12,ENodeBFunction=1"
#define TESTVALUE_INFOID0_1    "ThereIs=EqualsInTheString"
#define TESTVALUE_INFOID0_2    "ThereIsNoEqualsInTheString"
#define TESTVALUE_INFOID0_3    "ThereCanBe=Multiple=Equals=In=The=String"

#define RAISE        3
#define CLEAR        2
#define RAISE_CLEAR  1
#define CLEAR_RAISE  0


typedef struct {
  unsigned short nameLength;
  char name[10];
  unsigned short atomicNumber;
  unsigned short massNumber;
} AtomInfo;

static ei_x_buff
handleInitialize(int nArgs, ei_x_buff ignoredRequest, ei_x_buff response) {
  SaNtfHandleT handle;
  SaVersionT version = {'A', 0x01, 0x01};
  SaAisErrorT r = saNtfInitialize(&handle, NULL, &version);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~i}", r);
    return response;
  }
  else {
    APPLOG(
        "NTF session opened, test_ntfi.c version: %s, handle: %llu",
        "%CCaseRev:	/main/R2A/R3A/R6A/R11A/R12A/1 %",
        handle);

    unsigned int highBits = handle >> 32;
    unsigned int lowBits = handle & 0xffffffffLL;

    ei_x_format(&response, "{ok, ~u, ~u}", highBits, lowBits);
    return response;
  }
}


static ei_x_buff
handleFinalize(int nArgs, ei_x_buff request, ei_x_buff response) {
  SaNtfHandleT handle = (SaNtfHandleT)decodeInteger(&request);
  SaAisErrorT r = saNtfFinalize(handle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~i}", r);
    return response;
  }
  else {
    APPLOG("NTF session closed, handle: %llu", handle);

    ei_x_format(&response, "{ok}");
    return response;
  }
}


/**
 * Encodes the given SaNameT with the given string
 * and returns a SaNameT pointer.
 */
static SaNameT *
setSaNameT(SaNameT *name, const char *chars) {
  unsigned int length = strlen(chars);
  for (unsigned int j = 0; j < length; j++) {
    name->value[j] = (SaUint8T)chars[j];
  }
  name->length = length;
  return name;
}


/**
 * The request to be decoded holds:
 *
 * {Handle, Major, Minor, MoInstance, Severity, AdditionalText}
 *
 * The relation between Major. Minor and the "minorType" attribute
 * in metadata is:
 *
 *     minorType == (2^16)*Major + Minor
 *
 * The preparation of the SaNtfAlarmNotificationT struct is based
 * on the example in Appendix A of SAI-AIS-NTF-A.01.01.
 *
 * The AdditionalText that the test case sends may be an empty
 * string, or the special strings "EMPTY_TEXT" or "NO_TEXT", or
 * any non-empty string (there is a length limit in SAF that we
 * don't bother about yet).
 *
 * The "EMPTY_TEXT" is equivalent to an actual empty string.
 * The "NO_TEXT" case causes a 0/NULL encoding of lengthAdditionalText
 * and additionalText; is this OK with the SAF NTF implementation?
 */
static ei_x_buff
handleAlarm(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs < 6) {
    ei_x_format(&response, "{error, {too_few_arguments, ~i}}", nArgs);
    return response;
  }

  SaNtfHandleT handle = (SaNtfHandleT)decodeInteger(&request); // arg 1

  APPLOG("handleAlarm: handle: %llu", handle);

  SaUint16T major = (SaUint16T)decodeInteger(&request); // arg 2
  SaUint16T minor = (SaUint16T)decodeInteger(&request); // arg 3

  int moInstL = stringLength(&request);
  if (moInstL < 0) {
    ei_x_format(&response, "{error, {bad_parameter_mo_instance, ~i}}", moInstL);
    return response;
  }
  char moInstS[moInstL+1];
  if (moInstL == 0) {
    strcpy(moInstS, "");
    ei_skip_term(request.buff, &(request.index));
  }
  else {
    ei_decode_string(request.buff, &(request.index), moInstS); // arg 4
  }

  SaNtfSeverityT severity = (unsigned int)decodeInteger(&request); // arg 5

  int addTextRawL = stringLength(&request);
  if (addTextRawL < 0) {
    ei_x_format(&response, "{error, {bad_parameter_additional_text, ~i}}", addTextRawL);
    return response;
  }

  char *addText;
  unsigned int lenAddText;

  if (addTextRawL == 0) {
    // test case sent empty string
    ei_skip_term(request.buff, &(request.index));
    addText = calloc(1, sizeof(*addText));
    lenAddText = 1;
  }
  else {
    // test case sent non-empty string; parse it
    char addTextRaw[addTextRawL+1];
    ei_decode_string(request.buff, &(request.index), addTextRaw); // arg 6
    if (strcmp(addTextRaw, "NO_TEXT") == 0) {
      addText = NULL;
      lenAddText = 0;
    }
    else if (strcmp(addTextRaw, "EMPTY_TEXT") == 0) {
      // really the same as a true empty string from test case
      addText = calloc(1, sizeof(*addText));
      lenAddText = 1;
    }
    else {
      // any other non-empty string
      addText = strdup(addTextRaw);
      lenAddText = addTextRawL+1;
    }
  }

  unsigned int addInfo =
      nArgs == 6 ? ADD_INFO_NONE : (unsigned int)decodeInteger(&request); // arg 7

  // ================= end of request parsing =============

  // If the DYN_STRING bit is set then treat arguments 8
  // and 9 as a single id+string to be provided as additional info.

  unsigned int numAddInfo =
      //(addInfo & ADD_INFO_PRIMITIVE ? 10 : 0) +  // PRIMITIVE is 10 values
      (addInfo & ADD_INFO_STRING ? 1 : 0) +
      //(addInfo & ADD_INFO_NAME ? 1 : 0) +
      //(addInfo & ADD_INFO_IPADDRESS ? 1 : 0) +
      //(addInfo & ADD_INFO_BINARY ? 1 : 0) +
      //(addInfo & ADD_INFO_ARRAY ? 1 : 0) +
      (addInfo & ADD_INFO_CORR_INFO ? 1 : 0) +
      (addInfo & ADD_INFO_ALARM_ID ? 1 : 0) +
      (addInfo & ADD_INFO_UUID ? 1 : 0) +
      (addInfo & ADD_INFO_DN3 ? 1 : 0) +
      (addInfo & ADD_INFO_INFOID0 ? 4 : 0) +
      (addInfo & ADD_INFO_DYN_STRING ? 1 : 0);

  SaNtfElementIdT aiStrId = 0;
  char *aiStrS = NULL;
  if (addInfo & ADD_INFO_DYN_STRING) {

    aiStrId = (SaUint16T)decodeInteger(&request); // arg 8

    int aiStrL = stringLength(&request);
    if (aiStrL < 0) {
      free(addText);
      ei_x_format(&response, "{error, {bad_parameter_a_i_string, ~i}}", aiStrL);
      return response;
    }
    aiStrS = (char *)calloc(aiStrL+1, sizeof(char));
    if (aiStrL == 0) {
      strcpy(aiStrS, "");
      ei_skip_term(request.buff, &(request.index));
    }
    else {
      ei_decode_string(request.buff, &(request.index), aiStrS); // arg 9
    }
  }


  SaNtfAlarmNotificationT notif;

  SaAisErrorT r1 =
      saNtfAlarmNotificationAllocate(
          handle,
          &notif,
          0,
          lenAddText,
          numAddInfo,
          0,
          0,
          0,
          SA_NTF_ALLOC_SYSTEM_LIMIT);

  if (r1 != SA_AIS_OK) {
    free(addText);
    free(aiStrS);
    ei_x_format(&response, "{error, {saNtfAlarmNotificationAllocate, ~i}}", r1);
    return response;
  }

  unsigned int j = 0;

  if (addInfo & ADD_INFO_DYN_STRING) {
    notif.notificationHeader.additionalInfo[j].infoId = aiStrId;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
    char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        strlen(aiStrS) + 1,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      free(aiStrS);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "single string", status);
      return response;
    }
    strcpy(reservedMemory, aiStrS);
    free(aiStrS);
  }

  /*
  if (addInfo & ADD_INFO_PRIMITIVE) {

    notif.notificationHeader.additionalInfo[j].infoId = 100;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_UINT8;
    SaUint8T uint8 = TESTVALUE_UINT8_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.uint8Val = uint8;

    notif.notificationHeader.additionalInfo[j].infoId = 101;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_INT8;
    SaInt8T int8 = TESTVALUE_INT8_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.int8Val = int8;

    notif.notificationHeader.additionalInfo[j].infoId = 102;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_UINT16;
    SaUint16T uint16 = TESTVALUE_UINT16_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.uint16Val = uint16;

    notif.notificationHeader.additionalInfo[j].infoId = 103;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_INT16;
    SaInt16T int16 = TESTVALUE_INT16_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.int16Val = int16;

    notif.notificationHeader.additionalInfo[j].infoId = 104;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_UINT32;
    SaUint32T uint32 = TESTVALUE_UINT32_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.uint32Val = uint32;

    notif.notificationHeader.additionalInfo[j].infoId = 105;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_INT32;
    SaInt32T int32 = TESTVALUE_INT32_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.int32Val = int32;

    notif.notificationHeader.additionalInfo[j].infoId = 106;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_UINT64;
    SaUint64T uint64 = TESTVALUE_UINT64_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.uint64Val = uint64;

    notif.notificationHeader.additionalInfo[j].infoId = 107;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_INT64;
    SaInt64T int64 = TESTVALUE_INT64_1;
    notif.notificationHeader.additionalInfo[j++].infoValue.int64Val = int64;

    notif.notificationHeader.additionalInfo[j].infoId = 108;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_FLOAT;
    notif.notificationHeader.additionalInfo[j++].infoValue.floatVal = TESTVALUE_FLOAT_1;

    notif.notificationHeader.additionalInfo[j].infoId = 109;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_DOUBLE;
    notif.notificationHeader.additionalInfo[j++].infoValue.doubleVal = TESTVALUE_DOUBLE_1;
  }
  */

  if (addInfo & ADD_INFO_STRING) {
    char *aiString = TESTVALUE_STRING_1;
    notif.notificationHeader.additionalInfo[j].infoId = 110;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
    char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        strlen(aiString) + 1,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "string", status);
      return response;
    }
    strcpy(reservedMemory, aiString);
  }

  /*
  if (addInfo & ADD_INFO_IPADDRESS) {
    char *aiIpAddress = TESTVALUE_IPADDRESS_1;
    notif.notificationHeader.additionalInfo[j].infoId = 111;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_IPADDRESS;
    char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        strlen(aiIpAddress) + 1,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "ipAddress", status);
      return response;
    }
    strcpy(reservedMemory, aiIpAddress);
  }

  if (addInfo & ADD_INFO_BINARY) {
    unsigned char aiBinary[] = TESTVALUE_BINARY_1;
    unsigned int aiBinarySize = 4;
    notif.notificationHeader.additionalInfo[j].infoId = 112;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_BINARY;
    unsigned char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        aiBinarySize,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "binary", status);
      return response;
    }
    for (unsigned int k = 0; k < aiBinarySize; k++) {
      reservedMemory[k] = aiBinary[k];
    }
  }

  if (addInfo & ADD_INFO_NAME) {
    SaNameT aiName;
    setSaNameT(&aiName, TESTVALUE_NAME_1);
    notif.notificationHeader.additionalInfo[j].infoId = 113;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_LDAP_NAME;
    SaNameT *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        sizeof(SaNameT),
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "name", status);
      return response;
    }
    *reservedMemory = aiName;
  }

  if (addInfo & ADD_INFO_ARRAY) {
    AtomInfo a1 =
       {.name={'H', 'y', 'd', 'r', 'o', 'g', 'e', 'n', '@', '@'},
        .nameLength=htobe16(8),
        .atomicNumber=htobe16(1),
        .massNumber=htobe16(1)};
    AtomInfo a2 =
       {.name={'H', 'e', 'l', 'i', 'u', 'm', '@', '@', '@', '@'},
        .nameLength=htobe16(6),
        .atomicNumber=htobe16(2),
        .massNumber=htobe16(4)};
    notif.notificationHeader.additionalInfo[j].infoId = 114;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_ARRAY;
    AtomInfo *reservedMemory;
    SaAisErrorT status = saNtfArrayValAllocate(
        notif.notificationHandle,
        2,
        sizeof(AtomInfo),
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "atomInfo", status);
      return response;
    }
    reservedMemory[0] = a1;
    reservedMemory[1] = a2;
  }
  */

  if (addInfo & ADD_INFO_CORR_INFO) {
    char *aiString = TESTVALUE_CORR_INFO_1;
    notif.notificationHeader.additionalInfo[j].infoId = 1000;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
    char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        strlen(aiString) + 1,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "CI", status);
      return response;
    }
    strcpy(reservedMemory, aiString);
  }


  if (addInfo & ADD_INFO_ALARM_ID) {
    char *aiString = TESTVALUE_ALARM_ID_1;
    notif.notificationHeader.additionalInfo[j].infoId = 1001;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
    char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        strlen(aiString) + 1,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "UUID", status);
      return response;
    }
    strcpy(reservedMemory, aiString);
  }

  if (addInfo & ADD_INFO_UUID) {
    char *aiString = TESTVALUE_UUID_1;
    notif.notificationHeader.additionalInfo[j].infoId = 1002;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
    char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        strlen(aiString) + 1,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "UUID", status);
      return response;
    }
    strcpy(reservedMemory, aiString);
  }

  if (addInfo & ADD_INFO_DN3) {
    char *aiString = TESTVALUE_DN3_1;
    notif.notificationHeader.additionalInfo[j].infoId = 1100 + 3;
    notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
    char *reservedMemory;
    SaAisErrorT status = saNtfPtrValAllocate(
        notif.notificationHandle,
        strlen(aiString) + 1,
        (void **)&reservedMemory,
        &(notif.notificationHeader.additionalInfo[j++].infoValue));
    if (status != SA_AIS_OK) {
      free(addText);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "DNx", status);
      return response;
    }
    strcpy(reservedMemory, aiString);
  }

  if (addInfo & ADD_INFO_INFOID0) {
    {
      char *aiString = TESTVALUE_INFOID0_1;
      notif.notificationHeader.additionalInfo[j].infoId = 0;
      notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
      char *reservedMemory;
      SaAisErrorT status = saNtfPtrValAllocate(
          notif.notificationHandle,
          strlen(aiString) + 1,
          (void **)&reservedMemory,
          &(notif.notificationHeader.additionalInfo[j++].infoValue));
      if (status == SA_AIS_OK) {
        strcpy(reservedMemory, aiString);
      } else {
        free(addText);
        ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "infoId0", status);
        return response;
      }
    }

    {
      char *aiString = TESTVALUE_INFOID0_2;
      notif.notificationHeader.additionalInfo[j].infoId = 0;
      notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
      char *reservedMemory;
      SaAisErrorT status = saNtfPtrValAllocate(
          notif.notificationHandle,
          strlen(aiString) + 1,
          (void **)&reservedMemory,
          &(notif.notificationHeader.additionalInfo[j++].infoValue));
      if (status == SA_AIS_OK) {
        strcpy(reservedMemory, aiString);
      } else {
        free(addText);
        ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "infoId0", status);
        return response;
      }
    }

    {
      char *aiString = TESTVALUE_INFOID0_3;
      notif.notificationHeader.additionalInfo[j].infoId = 0;
      notif.notificationHeader.additionalInfo[j].infoType = SA_NTF_VALUE_STRING;
      char *reservedMemory;
      SaAisErrorT status = saNtfPtrValAllocate(
          notif.notificationHandle,
          strlen(aiString) + 1,
          (void **)&reservedMemory,
          &(notif.notificationHeader.additionalInfo[j++].infoValue));
      if (status == SA_AIS_OK) {
        strcpy(reservedMemory, aiString);
      } else {
        free(addText);
        ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~a, ~i}}", "infoId0", status);
        return response;
      }
    }
  }

  *(notif.notificationHeader.eventType) = SA_NTF_ALARM_COMMUNICATION;
  *(notif.notificationHeader.eventTime) = SA_TIME_UNKNOWN;

  SaNameT moInst;
  setSaNameT(&moInst, moInstS);
  notif.notificationHeader.notificationObject->length = moInst.length;
  memcpy(
      notif.notificationHeader.notificationObject->value,
      moInst.value,
      moInst.length);

  notif.notificationHeader.notificationClassId->vendorId = VENDOR_ID_ERICSSON;
  notif.notificationHeader.notificationClassId->majorId = major;
  notif.notificationHeader.notificationClassId->minorId = minor;

  *(notif.perceivedSeverity) = severity;
  *(notif.trend) = SA_NTF_TREND_NO_CHANGE;
  *(notif.probableCause) = SA_NTF_UNSPECIFIED_REASON;

  if (addText != NULL) {
    strcpy(notif.notificationHeader.additionalText, addText);
    free(addText);
  }

  SaAisErrorT r2 = saNtfNotificationSend(notif.notificationHandle);
  if (r2 != SA_AIS_OK) {
    ei_x_format(&response, "{error, {saNtfNotificationSend, ~i}}", r2);
    return response;
  }

  SaNtfIdentifierT notifId = *(notif.notificationHeader.notificationId);

  SaAisErrorT r3 = saNtfNotificationFree(notif.notificationHandle);
  if (r3 != SA_AIS_OK) {
    ei_x_format(&response, "{error, {saNtfNotificationFree, ~i}}", r3);
    return response;
  }
  else {
    ei_x_format(&response, "{ok, ~i}", notifId);
    return response;
  }
}


/**
 * Holds execution for the given number of milliseconds. The
 * return value is 0 if successful, -1 otherwise.
 */
static int holdMillis(unsigned int millis) {
  const struct timespec nap = {
      .tv_sec=millis/1000,
      .tv_nsec=(millis % 1000)*1000000};
  return nanosleep(&nap, NULL);
}


/**
 * The request to be decoded holds:
 *
 * {Handle, Major, Minor, MoInstance, Severity, AdditionalText, Separation}
 *       1      2      3           4         5               6           7
 *
 * Two notifications are sent Separation ms apart. The second notification
 * is a 'clear'.
 *
 * For the interpretation of arguments, see the handleAlarm function.
 * Additional Info is not supported here.
 *
 * The 'scenario' argument can one of RAISE, CLEAR, RAISE_CLEAR or CLEAR_RAISE.
 * In case of RAISE and CLEAR the Separation value is ignored; no delay is
 * performed.
 */
static ei_x_buff
handleAlarmRaiseClear(
    int nArgs,
    ei_x_buff request,
    ei_x_buff response,
    int scenario) {

  if (nArgs < 7) {
    ei_x_format(&response, "{error, {too_few_arguments, ~i}}", nArgs);
    return response;
  }

  SaNtfHandleT handle = (SaNtfHandleT)decodeInteger(&request); // arg 1

  APPLOG("handleAlarmTransient: handle: %llu", handle);

  SaUint16T major = (SaUint16T)decodeInteger(&request); // arg 2
  SaUint16T minor = (SaUint16T)decodeInteger(&request); // arg 3

  int moInstL = stringLength(&request);
  if (moInstL < 0) {
    ei_x_format(&response, "{error, {bad_parameter_mo_instance, ~i}}", moInstL);
    return response;
  }
  char moInstS[moInstL+1];
  if (moInstL == 0) {
    strcpy(moInstS, "");
    ei_skip_term(request.buff, &(request.index));
  }
  else {
    ei_decode_string(request.buff, &(request.index), moInstS); // arg 4
  }

  SaNtfSeverityT severity = (unsigned int)decodeInteger(&request); // arg 5

  int addTextRawL = stringLength(&request);
  if (addTextRawL < 0) {
    ei_x_format(&response, "{error, {bad_parameter_additional_text, ~i}}", addTextRawL);
    return response;
  }

  char *addText;
  unsigned int lenAddText;

  if (addTextRawL == 0) {
    // test case sent empty string
    ei_skip_term(request.buff, &(request.index));
    addText = calloc(1, sizeof(*addText));
    lenAddText = 1;
  }
  else {
    // test case sent non-empty string; parse it
    char addTextRaw[addTextRawL+1];
    ei_decode_string(request.buff, &(request.index), addTextRaw); // arg 6
    if (strcmp(addTextRaw, "NO_TEXT") == 0) {
      addText = NULL;
      lenAddText = 0;
    }
    else if (strcmp(addTextRaw, "EMPTY_TEXT") == 0) {
      // really the same as a true empty string from test case
      addText = calloc(1, sizeof(*addText));
      lenAddText = 1;
    }
    else {
      // any other non-empty string
      addText = strdup(addTextRaw);
      lenAddText = addTextRawL+1;
    }
  }

  unsigned int separation = (unsigned int)decodeInteger(&request); // arg 7

  // handling the optional argument "program group"
  SaUint16T numProposedRepairActions = 0;
  char *progGrp = NULL;
  if (nArgs > 7) {
    int progGrpL = stringLength(&request);
    if (progGrpL < 0) {
      ei_x_format(&response, "{error, {bad_parameter_program_group, ~i}}",
		  progGrpL);
      return response;
    }

    if (progGrpL == 0)
      ei_skip_term(request.buff, &(request.index));
    else {
      progGrp = calloc(progGrpL+1,1);
      ei_decode_string(request.buff, &(request.index), progGrp); // arg 8
      numProposedRepairActions = 1;
    }
  }


  // ================= end of request parsing =============

  // allocate a notification
  SaNtfAlarmNotificationT notif;
  SaAisErrorT r1 =
      saNtfAlarmNotificationAllocate(
          handle,
          &notif,
          0,
          lenAddText,
          0,
          0,
          0,
          numProposedRepairActions,
          SA_NTF_ALLOC_SYSTEM_LIMIT);
  if (r1 != SA_AIS_OK) {
    free(addText);
    ei_x_format(&response, "{error, {saNtfAlarmNotificationAllocate, ~i}}", r1);
    return response;
  }

  // fill out header info
  *(notif.notificationHeader.eventType) = SA_NTF_ALARM_COMMUNICATION;
  *(notif.notificationHeader.eventTime) = SA_TIME_UNKNOWN;
  SaNameT moInst;
  setSaNameT(&moInst, moInstS);
  notif.notificationHeader.notificationObject->length = moInst.length;
  memcpy(notif.notificationHeader.notificationObject->value,
	 moInst.value,
	 moInst.length);
  notif.notificationHeader.notificationClassId->vendorId = VENDOR_ID_ERICSSON;
  notif.notificationHeader.notificationClassId->majorId = major;
  notif.notificationHeader.notificationClassId->minorId = minor;
  if (addText) {
    strcpy(notif.notificationHeader.additionalText, addText);
    free(addText);
  }

  // fill out other notification fields
  *(notif.trend) = SA_NTF_TREND_NO_CHANGE;
  *(notif.probableCause) = SA_NTF_UNSPECIFIED_REASON;

  // set program group if needed */
  if (numProposedRepairActions == 1) {
    notif.proposedRepairActions[0].actionId = 0;
    notif.proposedRepairActions[0].actionValueType = SA_NTF_VALUE_STRING;

    SaStringT destPtr = NULL;
    r1 = saNtfPtrValAllocate(notif.notificationHandle,
			     strlen(progGrp) + 1,
			     (void**) &destPtr,
			     &(notif.proposedRepairActions[0].actionValue));
    if (r1 != SA_AIS_OK) {
      saNtfNotificationFree(notif.notificationHandle);
      free(progGrp);
      ei_x_format(&response, "{error, {saNtfPtrValAllocate, ~i}}", r1);
      return response;
    }
    strcpy(destPtr, progGrp);
    free(progGrp);
  }

  // send the notification and clear it after certain time
  SaNtfIdentifierT notifId1 = 999;
  SaNtfIdentifierT notifId2 = 999;
  int holdResult = -2;
  for (
      // first lap severity
      SaNtfSeverityT s = (scenario == RAISE_CLEAR || scenario == RAISE) ? severity : SA_NTF_SEVERITY_CLEARED;

      // don't terminate here; use breaks
      true;

      // toggle the severity for the second lap
      s = (s == severity ? SA_NTF_SEVERITY_CLEARED : severity)) {

    *(notif.perceivedSeverity) = s;

    SaAisErrorT r2 = saNtfNotificationSend(notif.notificationHandle);
    if (r2 != SA_AIS_OK) {
      ei_x_format(&response, "{error, {saNtfNotificationSend, ~i, ~i}}", s, r2);
      return response;
    }

    if ((scenario == RAISE_CLEAR && s == severity) ||
        (scenario == CLEAR_RAISE && s == SA_NTF_SEVERITY_CLEARED) ||
        (scenario == RAISE && s == severity) ||
        (scenario == CLEAR && s == SA_NTF_SEVERITY_CLEARED)) {
      // end of first lap
      notifId1 = *(notif.notificationHeader.notificationId);
      if (scenario == RAISE || scenario == CLEAR) {
        break;
      }
      holdResult = holdMillis(separation);
      if (holdResult != 0) {
        ei_x_format(&response, "{error, {badHoldResult, ~i}}", holdResult);
        return response;
      }
    }
    else {
      // end of 2nd lap
      notifId2 = *(notif.notificationHeader.notificationId);
      break;
    }
  }

  SaAisErrorT r3 = saNtfNotificationFree(notif.notificationHandle);
  if (r3 != SA_AIS_OK) {
    ei_x_format(&response, "{error, {saNtfNotificationFree, ~i}}", r3);
    return response;
  }

  char *s1;
  asprintf(&s1, "%llu", notifId1);
  char *s2;
  asprintf(&s2, "%llu", notifId2);

  ei_x_format(&response, "{ok, {~s, ~s}}", s1, s2);
  free(s1);
  free(s2);
  return response;
}


static LmhiStartPgmResult startPgmRes;

static ei_x_buff
handleCountLines(int nArgs, ei_x_buff request, ei_x_buff response) {
  if (nArgs != 2) {
    ei_x_format(&response, "{error, {argcount, ~i}}", nArgs);
    return response;
  }
  else {
    char *dirname = decodeString(&request);
    char *resolvedDirname = envExpand(dirname);
    free(dirname);
    char *filename = decodeString(&request);
    unsigned int nLines;
    int errorNumber;
    int result = countLines(resolvedDirname, filename, &nLines, &errorNumber);
    free(filename);
    free(resolvedDirname);
    if (result != TEXTFILE_OK) {
      ei_x_format(&response, "{error, {~i, ~i}}", result, (int)errorNumber);
      return response;
    }
    else {
      ei_x_format(&response, "{ok, ~i}", (int)nLines);
      return response;
    }
  }
}


static ei_x_buff
handleGetTailLines(int nArgs, ei_x_buff request, ei_x_buff response) {
  if (nArgs < 3 || nArgs > 4) {
    ei_x_format(&response, "{error, {argcount, ~i}}", nArgs);
    return response;
  }
  else {
    char *dirname = decodeString(&request);                              // arg 1
    char *resolvedDirname = envExpand(dirname);
    free(dirname);
    char *filename = decodeString(&request);                             // arg 2
    unsigned int skipLines = (unsigned int)decodeInteger(&request);      // arg 3
    int maxLines = (nArgs == 4) ? (int)decodeInteger(&request) : -1;     // arg 4 (optional)

    char *ffname;
    asprintf(&ffname, "%s/%s", getenv("LOG_DIR"), "debug.txt");
    FILE *ff = fopen(ffname, "a");
    free(ffname);
    fprintf(ff, "maxLines = %d\n", maxLines);
    fclose(ff);

    LineList *lines;
    int errorNumber;
    int result =
        getTailLines(
            resolvedDirname,
            filename,
            skipLines,
            maxLines,
            &lines,
            &errorNumber);
    free(filename);
    free(resolvedDirname);
    if (result != TEXTFILE_OK) {
      ei_x_format(&response, "{error, {~i, ~i}}", result, (int)errorNumber);
      return response;
    }
    else {
      ei_x_encode_version(&response);
      ei_x_encode_tuple_header(&response, 2);
      ei_x_encode_atom(&response, "ok");
      if (lines == NULL) {
        ei_x_encode_empty_list(&response);
      }
      else {
        ei_x_encode_list_header(&response, (long int)lineListSize(lines));
        for (LineList *p = lines; p != NULL; p = p->next) {
          ei_x_encode_string(&response, p->line);
        }
        ei_x_encode_empty_list(&response);
      }
      freeLines(lines, true);
      return response;
    }
  }
}


static ei_x_buff
handleExpandString(int nArgs, ei_x_buff request, ei_x_buff response) {
  if (nArgs != 1) {
    ei_x_format(&response, "{error, {argcount, ~i}}", nArgs);
    return response;
  }
  else {
    char *string = decodeString(&request);
    char *expanded = envExpand(string);
    free(string);
    ei_x_format(&response, "{ok, ~s}", expanded);
    free(expanded);
    return response;
  }
}


/**
 * Handles a call from the test suite towards the CS.
 */
ei_x_buff
send_sig_ntfi(int function, ei_x_buff request) {

  APPLOG("send_sig_ntfi, function: %d", function);

  int nArgs;
  ei_decode_tuple_header(request.buff, &request.index, &nArgs);

  ei_x_buff response;
  ei_x_new(&response);

  switch (function) {
  case NTFI_INITIALIZE:
    return handleInitialize(nArgs, request, response);

  case NTFI_FINALIZE:
    return handleFinalize(nArgs, request, response);

  case NTFI_ALARM:
    return handleAlarm(nArgs, request, response);

  case NTFI_ALARM_RAISE:
    return handleAlarmRaiseClear(nArgs, request, response, RAISE);

  case NTFI_ALARM_CLEAR:
    return handleAlarmRaiseClear(nArgs, request, response, CLEAR);

  case NTFI_ALARM_RAISE_CLEAR:
    return handleAlarmRaiseClear(nArgs, request, response, RAISE_CLEAR);

  case NTFI_ALARM_CLEAR_RAISE:
    return handleAlarmRaiseClear(nArgs, request, response, CLEAR_RAISE);

  case NTFI_START_ALARMS_EXAMPLE:
  {
    uint32_t duId = 0;
    uint32_t cpuSet = 0;
    char *lmId = getenv("ALAEXA_CXC_NO");
    if (lmId == NULL) {
      ei_x_format(&response, "{error, missing_cxc_no}");
      return response;
    }
    char *pgmName = "ALAEXA";
    char *const lmhi_argv[] = {"alarms_example_app", NULL};
    LmhiResultCode r1 = Lmhi_start_pgm(duId, cpuSet, lmId, pgmName, lmhi_argv, &startPgmRes);
    if (r1 != LMHI_OK) {
      ei_x_format(&response, "{error, ~i}", r1);
      return response;
    }
    else {
      ei_x_format(&response, "{ok, ~i}", startPgmRes.pgmId);
      return response;
    }
  }

  case NTFI_STOP_ALARMS_EXAMPLE:
  {
    uint32_t duId = 0;
    LmhiResultCode r2 = Lmhi_stop_pgm(duId, startPgmRes.pgmId);
    if (r2 != LMHI_OK) {
      ei_x_format(&response, "{error, ~i}", r2);
      return response;
    }
    else {
      ei_x_format(&response, "{ok}");
      return response;
    }
  }

  case NTFI_COUNT_LINES:
    return handleCountLines(nArgs, request, response);

  case NTFI_GET_TAIL_LINES:
    return handleGetTailLines(nArgs, request, response);

  case NTFI_EXPAND_STRING:
    return handleExpandString(nArgs, request, response);

  default:
    ei_x_format(&response, "{error, {unknown_function, ~i}}", function);
    return response;
  }
}

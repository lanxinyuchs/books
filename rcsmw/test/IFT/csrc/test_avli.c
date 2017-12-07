/* ----------------------------------------------------------------------
 * %CCaseFile:	test_avli.c %
 * %CCaseRev:	/main/R2A/R4A/R5A/R6A/1 %
 * %CCaseDate:	2016-08-24 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Tests of AVLI use functions defined here.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
 * R2A/2      2013-02-18 erarafo     Created
 * R2A/3      2014-01-31 erarafo     Function rename, static declarations
 * R2A/4      2014-04-10 etxberb     Added AVLI4_WRITE_NODE_EVENT.
 * R4A/1      2015-11-25 erarafo     Wrappers around TRI macro calls.
 * R5A/1      2015-12-11 etomist     Added AVLI ITC support
 * R5A/2      2015-12-29 etomist     Removed AVLI ITC support
 * ----------------------------------------------------------------------
 */


#include "master.h"

#include "cello_avli.h"
#include "cello_avli.sig"

/* must match definitions in test_avli.hrl */
#define AVLI_INITIATE_MEMORY            1
#define AVLI_INTERNAL                   3
#define AVLI2_FREE_MEMORY               9
#define AVLI2_INITIATE_SERVICE         10
#define AVLI2_WRITE_NODE_EVENT         11
#define AVLI2_WRITE_PIU_EVENT          12
#define AVLI2_WRITE_HW_EVENT           13
#define AVLI2_WRITE_SERVICE_EVENT      14
#define AVLI2_WRITE_OTHER_EVENT        15
#define AVLI3_WRITE_PGM_EVENT          16
#define AVLI4_WRITE_NODE_EVENT         17
#define AVLI5_WRITE_HW_EVENT           18


#define AVLI_ITC_MBOX "avliItcMbox"


/* must reflect entire content of cello_avli.sig */
union SIGNAL
{
  SIGSELECT                     sigNo;
  CelloAvliWriteCfm             celloAvliWriteCfm;
  CelloAvliHuntNsInd            celloAvliHuntNsInd;
  CelloAvliServerUpInd          celloAvliServerUpInd;
  CelloAvliServerDownInd        celloAvliServerDownInd;
  CelloAvliAttachNsInd          celloAvliAttachNsInd;
  CelloAvliUnpublishInd         celloAvliUnpublishInd;
  CelloAvli2InitiateServiceCfm  celloAvli2InitiateServiceCfm;
  CelloAvli2InitiateServiceRej  celloAvli2InitiateServiceRej;
  CelloAvli2InitiateServiceSus  celloAvli2InitiateServiceSus;
};


extern bool TRI_trace_enabled;


/* TODO, remove
char*
getString(char* string, char* defaultString) {
  QENTER1("getString");

  char* result;
  if (string == NULL) {
    result = defaultString;
  }
  else {
    result = string;
  }
  QRETURN1(result);
}
*/


static char *
envValue(char* name, char* defaultValue) {
  char* result = getenv(name);
  if (result == NULL) {
    return defaultValue;
  }
  else {
    return result;
  }
}


static void
verifyTuple(const char* buffer, int* index, int expectedArity, char* tag) {
  int arity;
  ei_decode_tuple_header(buffer, index, &arity);
  if (arity != expectedArity) {
    QTRACE_ERROR4("tuple arity mismatch, expected: %d, actual: %d, tag: %s",
		    expectedArity,
		    arity,
		    tag);
  }
}


static char *
decodeWrappedString(const char* buffer, int* index) {
  verifyTuple(buffer, index, 2, "ill-formed wrapped string");
  unsigned long length;
  ei_decode_ulong(buffer, index, &length);
  char* result = malloc(length+1);
  ei_decode_string(buffer, index, result);
  return result;
}


static char *
decodeLimitedString(const char* buffer, int* index, unsigned long maxLength, char* tag) {
  verifyTuple(buffer, index, 2, "ill-formed wrapped limited string");
  unsigned long length;
  ei_decode_ulong(buffer, index, &length);

  if (length > maxLength) {
    QTRACE_ERROR4("string length exceeded, max length: %d, actual: %d, tag: %s", maxLength, length, tag);
    char* decodedString = malloc(length+1);
    char* result = malloc(maxLength+1);
    ei_decode_string(buffer, index, decodedString);
    strncpy(result, decodedString, maxLength);
    free(decodedString);
    *(result+maxLength) = '\0';
    return result;
  }
  else {
    char* result = malloc(length+1);
    ei_decode_string(buffer, index, result);
    return result;
  }
}


    // from $RCS_TOP/EQS/EQS_CXA11446/inc/cello_control_commontypes.h
    // code examples in 10/1553-CXA 110 3180 - User Guide for Equipment Resource Interface
    // the byte arrays are intended to accomodate null-terminated strings


static void
decodeHwPid(const char* buffer, int* index, Cello_PidInHW* hwPid, char* tag) {

  verifyTuple(buffer, index, 5, tag);

  char* s;
  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_NUMBER_LEN-1, "productNumber");
  strcpy((char* restrict)hwPid->productNumber, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_REVISION_LEN-1, "productRevision");
  strcpy((char* restrict)hwPid->productRevision, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_NAME_LEN-1, "productName");
  strcpy((char* restrict)hwPid->productName, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_DATE_LEN-1, "productDate");
  strcpy((char* restrict)hwPid->productDate, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_SERIAL_NUMBER_LEN-1, "serialNumber");
  strcpy((char* restrict)hwPid->serialNumber, s);
  free(s);

  QTRACE8(3, "tag: %s, prodNo: '%s', prodRev: '%s', prodname: '%s', prodDate: '%s', serial: '%s'",
	       tag,
	       hwPid->productNumber,
	       hwPid->productRevision,
	       hwPid->productName,
	       hwPid->productDate,
	       hwPid->serialNumber);

  QRETURN;
}

static void
decodeHw5Pid(const char* buffer, int* index, Cello_Avli_PidInHW* hwPid, char* tag) {

  verifyTuple(buffer, index, 5, tag);

  char* s;
  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_NUMBER_LEN-1, "productNumber");
  strcpy((char* restrict)hwPid->productNumber, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_REVISION_LEN-1, "productRevision");
  strcpy((char* restrict)hwPid->productRevision, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_AVLI_MAX_PRODUCT_NAME_LEN-1, "productName");
  strcpy((char* restrict)hwPid->productName, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_DATE_LEN-1, "productDate");
  strcpy((char* restrict)hwPid->productDate, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_SERIAL_NUMBER_LEN-1, "serialNumber");
  strcpy((char* restrict)hwPid->serialNumber, s);
  free(s);

  QTRACE8(3, "tag: %s, prodNo: '%s', prodRev: '%s', prodname: '%s', prodDate: '%s', serial: '%s'",
	       tag,
	       hwPid->productNumber,
	       hwPid->productRevision,
	       hwPid->productName,
	       hwPid->productDate,
	       hwPid->serialNumber);

  QRETURN;
}

static void
decodeSwPid(const char* buffer, int* index, Cello_PidInSW* swPid, char* tag) {

  verifyTuple(buffer, index, 4, tag);

  char* s;
  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_NUMBER_LEN-1, "productNumber");
  strcpy((char* restrict)swPid->productNumber, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_REVISION_LEN-1, "productRevision");
  strcpy((char* restrict)swPid->productRevision, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_NAME_LEN-1, "productName");
  strcpy((char* restrict)swPid->productName, s);
  free(s);

  s = decodeLimitedString(buffer, index, CELLO_MAX_PRODUCT_DATE_LEN-1, "productDate");
  strcpy((char* restrict)swPid->productDate, s);
  free(s);

  QTRACE7(3, "tag: %s, prodNo: '%s', prodRev: '%s', prodname: '%s', prodDate: '%s'",
	       tag,
	       swPid->productNumber,
	       swPid->productRevision,
	       swPid->productName,
	       swPid->productDate);

  QRETURN;
}



static unsigned long
decodeUlong(const char* buffer, int* index) {
  unsigned long result;
  ei_decode_ulong(buffer, index, &result);
  return result;
}

static long
decodeLong(const char* buffer, int* index) {
  long result;
  ei_decode_long(buffer, index, &result);
  return result;
}



static union SIGNAL* avliBufferP; // assume just one active client for now




ei_x_buff
send_sig_avli(int func, ei_x_buff args) {

  QENTER2("send_sig_avli(), func: %d", func);

  ei_x_buff resp;
  ei_x_new_with_version(&resp);

  if (func == AVLI_INITIATE_MEMORY) {

    QTRACE2(3, "call: Cello_Avli_initiateMemory()");
    avliBufferP = Cello_Avli_initiateMemory();
    ei_x_format_wo_ver(&resp, "{ok}");

  } else if (func == AVLI2_INITIATE_SERVICE) {

    QTRACE3(3, "CEC_PORT: %s", envValue("CEC_PORT", "undefined"));
    QTRACE2(3, "call: CelloAvli2_initiateService()");
    Cello_AvliResult result = CelloAvli2_initiateService(avliBufferP, 4, 3, 0);
    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else if (func == AVLI2_FREE_MEMORY) {

    QTRACE2(3, "call: CelloAvli2_freeMemory()");
    CelloAvli2_freeMemory(&avliBufferP);
    ei_x_format_wo_ver(&resp, "{ok}");

  } else if (func == AVLI2_WRITE_NODE_EVENT) {

    verifyTuple(args.buff, &args.index, 5, "AVLI2_WRITE_NODE_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);
    CelloAvliAddInfo info = decodeWrappedString(args.buff, &args.index);

    QTRACE7(3, "call: CelloAvli2_writeNodeEvent(), client: %d, ts: %x, status: %d, reason: %d, info: %s",
		 clientRef, timestamp, serviceStatus, reason, info);

    Cello_AvliResult result = CelloAvli2_writeNodeEvent(avliBufferP,
							timestamp,
							serviceStatus,
							reason,
							info,
							clientRef);

    free(info);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else if (func == AVLI4_WRITE_NODE_EVENT) {

    verifyTuple(args.buff, &args.index, 6, "AVLI4_WRITE_NODE_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);
    U32 eventId = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliAddInfo info = decodeWrappedString(args.buff, &args.index);

    QTRACE8(3, "call: CelloAvli4_writeNodeEvent(), client: %d, ts: %x, status: %d, reason: %d, eventId: %x, info: %s",
		 clientRef, timestamp, serviceStatus, reason, eventId, info);

    Cello_AvliResult result = CelloAvli4_writeNodeEvent(avliBufferP,
							timestamp,
							serviceStatus,
							reason,
							eventId,
							info,
							clientRef);

    free(info);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else if (func == AVLI2_WRITE_PIU_EVENT) {

    verifyTuple(args.buff, &args.index, 8, "AVLI2_WRITE_PIU_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);
    CelloAvliPiuType piuType = (CelloAvliPiuType)decodeUlong(args.buff, &args.index);
    verifyTuple(args.buff, &args.index, 3, "AVLI2_WRITE_PIU_EVENT (piuHwAddr)");

    CelloPiuHwAddr piuHwAddr;
    piuHwAddr.smn = decodeUlong(args.buff, &args.index);
    piuHwAddr.apn = decodeUlong(args.buff, &args.index);
    piuHwAddr.ern = decodeUlong(args.buff, &args.index);

    QTRACE5(3, "piuHwAddr.smn: %d, .apn: %d, .ern: %d", piuHwAddr.smn, piuHwAddr.apn, piuHwAddr.ern);

    Cello_PidInHW hwPid;
    decodeHwPid(args.buff, &args.index, &hwPid, "AVLI2_WRITE_PIU_EVENT");

    CelloAvliAddInfo info = decodeWrappedString(args.buff, &args.index);

    QTRACE8(3, "call CelloAvli2_writePiuEvent(), client: %d, ts: %x, status: %d, reason: %d, piuType: %d, info: %s",
		 clientRef, timestamp, serviceStatus, reason, piuType, info);

    Cello_AvliResult result =
      CelloAvli2_writePiuEvent(avliBufferP,
			       timestamp,
			       serviceStatus,
			       reason,
			       piuType,
			       &piuHwAddr,
			       &hwPid,
			       info,
			       clientRef);

    free(info);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else if (func == AVLI2_WRITE_HW_EVENT) {

    verifyTuple(args.buff, &args.index, 8, "AVLI2_WRITE_HW_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);
    CelloAvliHwType hwType = decodeWrappedString(args.buff, &args.index);
    CelloAvliHwAddress hwAddress = decodeWrappedString(args.buff, &args.index);

    Cello_PidInHW hwPid;
    decodeHwPid(args.buff, &args.index, &hwPid, "AVLI2_WRITE_HW_EVENT");

    CelloAvliAddInfo info = decodeWrappedString(args.buff, &args.index);

    QTRACE9(3, "call CelloAvli2_writeHwEvent(), client: %d, ts: %x, status: %d, reason: %d, hwType: %s, hwAddress: %s, info: %s",
		 clientRef, timestamp, serviceStatus, reason, hwType, hwAddress, info);

    Cello_AvliResult result =
      CelloAvli2_writeHwEvent(avliBufferP,
			      timestamp,
			      serviceStatus,
			      reason,
			      hwType,
			      hwAddress,
			      &hwPid,
			      info,
			      clientRef);

    free(hwType);
    free(hwAddress);
    free(info);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else if (func == AVLI5_WRITE_HW_EVENT) {

    verifyTuple(args.buff, &args.index, 8, "AVLI5_WRITE_HW_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);
    CelloAvliHwType hwType = decodeWrappedString(args.buff, &args.index);
    CelloAvliHwAddress hwAddress = decodeWrappedString(args.buff, &args.index);

    Cello_Avli_PidInHW hwPid;
    decodeHw5Pid(args.buff, &args.index, &hwPid, "AVLI5_WRITE_HW_EVENT");

    CelloAvliAddInfo info = decodeWrappedString(args.buff, &args.index);

    QTRACE9(3, "call CelloAvli5_writeHwEvent(), client: %d, ts: %x, status: %d, reason: %d, hwType: %s, hwAddress: %s, info: %s",
		 clientRef, timestamp, serviceStatus, reason, hwType, hwAddress, info);

    Cello_AvliResult result =
      CelloAvli5_writeHwEvent(avliBufferP,
			      timestamp,
			      serviceStatus,
			      reason,
			      hwType,
			      hwAddress,
			      &hwPid,
			      info,
			      clientRef);

    free(hwType);
    free(hwAddress);
    free(info);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

   } else if (func == AVLI2_WRITE_SERVICE_EVENT) {

    verifyTuple(args.buff, &args.index, 7, "AVLI2_WRITE_SERVICE_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);
    CelloAvliServiceType serviceType = decodeWrappedString(args.buff, &args.index);
    CelloAvliServiceInstance serviceInstance = decodeWrappedString(args.buff, &args.index);
    CelloAvliAddInfo info = decodeWrappedString(args.buff, &args.index);

    QTRACE9(3, "call CelloAvli2_writeServiceEvent(), client: %d, ts: %x, status: %d, reason: %d, serviceType: %s, serviceInstance: %s, info: %s",
		 clientRef, timestamp, serviceStatus, reason, serviceType, serviceInstance, info);

    Cello_AvliResult result =
      CelloAvli2_writeServiceEvent(avliBufferP,
			      timestamp,
			      serviceStatus,
			      reason,
			      serviceType,
			      serviceInstance,
			      info,
			      clientRef);

    free(serviceType);
    free(serviceInstance);
    free(info);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else if (func == AVLI2_WRITE_OTHER_EVENT) {

    verifyTuple(args.buff, &args.index, 5, "AVLI2_WRITE_OTHER_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);
    CelloAvliAvailabilityInfo avInfo = decodeWrappedString(args.buff, &args.index);

    QTRACE7(3, "call CelloAvli2_writeOtherEvent(), client: %d, ts: %x, status: %d, reason: %d, availabilityInfo: %s",
		 clientRef, timestamp, serviceStatus, reason, avInfo);

    Cello_AvliResult result =
      CelloAvli2_writeOtherEvent(avliBufferP,
				 timestamp,
				 serviceStatus,
				 reason,
				 avInfo,
				 clientRef);
    free(avInfo);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else if (func == AVLI3_WRITE_PGM_EVENT) {

    verifyTuple(args.buff, &args.index, 8, "AVLI3_WRITE_PGM_EVENT");
    CelloAvliClientRef clientRef = (CelloAvliClientRef)decodeUlong(args.buff, &args.index);
    U32 timestamp = (U32)decodeUlong(args.buff, &args.index);
    CelloAvliServiceStatus serviceStatus = (CelloAvliServiceStatus)decodeLong(args.buff, &args.index);
    CelloAvliReason reason = (CelloAvliReason)decodeLong(args.buff, &args.index);

    CelloAvliPiuType piuType = (CelloAvliPiuType)decodeUlong(args.buff, &args.index);


    verifyTuple(args.buff, &args.index, 3, "AVLI3_WRITE_PGM_EVENT (piuHwAddr)");

    CelloPiuHwAddr piuHwAddr;
    piuHwAddr.smn = decodeUlong(args.buff, &args.index);
    piuHwAddr.apn = decodeUlong(args.buff, &args.index);
    piuHwAddr.ern = decodeUlong(args.buff, &args.index);

    QTRACE5(3, "piuHwAddr.smn: %d, .apn: %d, .ern: %d", piuHwAddr.smn, piuHwAddr.apn, piuHwAddr.ern);



    Cello_PidInSW swPid;
    decodeSwPid(args.buff, &args.index, &swPid, "AVLI3_WRITE_PGM_EVENT");

    CelloAvliAddInfo info = decodeWrappedString(args.buff, &args.index);

    QTRACE8(3, "call CelloAvli3_writePgmEvent(), client: %d, ts: %x, status: %d, reason: %d, piuType: %d, info: %s",
		 clientRef, timestamp, serviceStatus, reason, piuType, info);

    Cello_AvliResult result =
      CelloAvli3_writePgmEvent(avliBufferP,
			       timestamp,
			       serviceStatus,
			       reason,
			       piuType,
			       &piuHwAddr,
			       &swPid,
			       info,
			       clientRef);

    free(info);

    ei_x_format_wo_ver(&resp, "{ok, ~i}", result);

  } else {

    ei_x_format_wo_ver(&resp, "{error, ~s. ~i}", "unknown function code", func);

  }

  QRETURN1(resp);
}

ei_x_buff
recv_sig_avli(union SIGNAL *sig_p) {
  ei_x_buff resp;

  QENTER2("recv_sig_pri(), sigNo: %d", sig_p->sigNo);

  ei_x_new(&resp);

  Cello_AvliResult result;

  switch(sig_p->sigNo) {

  case CELLO_AVLI_SERVER_UP_IND:
    QTRACE2(3, "got server up indication");
    result = Cello_Avli_internal(avliBufferP, sig_p);
    QTRACE2(3, "got server up indication, sent 'internal'");
    free_buf(&sig_p);
    QTRACE2(3, "got server up indication, freed memory");

    ei_x_format(&resp, "{signal, {ok, ~i, ~i}}", CELLO_AVLI_SERVER_UP_IND, result);
    // latestSignalP = sig_p;
    break;

  case CELLO_AVLI2_INITIATE_SERVICE_CFM:
    QTRACE2(3, "got svc cfm indication");
    result = Cello_Avli_internal(avliBufferP, sig_p);
    QTRACE2(3, "got svc cfm indication, sent 'internal'");
    free_buf(&sig_p);
    QTRACE2(3, "got svc cfm indication, freed memory");

    ei_x_format(&resp, "{signal, {ok, ~i, ~i}}", CELLO_AVLI2_INITIATE_SERVICE_CFM, result);
    //latestSignalP = sig_p;
    break;

  case CELLO_AVLI_WRITE_CFM:
    ei_x_format(&resp, "{signal, {ok, ~i}}", CELLO_AVLI_WRITE_CFM);
    free_buf(&sig_p);
    break;

  default:
    ei_x_format(&resp, "{signal, ~i}", (unsigned int) sig_p->sigNo);
    break;
  }

  QRETURN1(resp);
}

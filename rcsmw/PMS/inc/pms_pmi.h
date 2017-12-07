#ifndef PMS_PMI_H_
#define PMS_PMI_H_
/* ----------------------------------------------------------------------
 * %CCaseFile:	pms_pmi.h %
 * %CCaseRev:	/main/R2A/R3A/R8A/1 %
 * %CCaseDate:	2017-01-11 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Definition of the C interface to PM.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
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
 * R2A/2      2013-02-04 erarafo     Created
 * R2A/7      2013-04-08 erarafo     64-bit signed int for measurement values
 * R2A/8      2013-04-09 erarafo     Improved descriptions in comments
 * R2A/9      2013-04-10 erarafo     Added 'const' specifiers
 * R2A/10     2013-04-10 erarafo     Removed impractical 'const' specifiers
 * R2A/11     2013-04-11 erarafo     Renamed a parameter for uniformity
 * R2A/14     2013-05-03 erarafo     Elaborated failure codes
 * R2A/15     2013-11-28 eolaand     Add c++ def
 * R2A/16     2014-01-20 erarafo     Using 'const char'
 * R2A/17     2014-04-10 uabesvi     Added functions for show counters
 * R2A/20     2014-05-14 uabesvi     Added error string to show counters data
 * R8A/1      2017-01-11 erarafo     More PMI_LOCATION_nn macros added
 * ----------------------------------------------------------------------
 */

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef const void *PmiHandleT;


typedef int PmiSelectionObjectT;


enum {
  PMI_CS_OK = 0,
  PMI_CS_NOT_EXIST = 1,
  PMI_CS_INTERNAL_ERROR = 2
};

enum {
  PMI_MAX_SESSIONS = 100
};

enum {
  PMI_INITIALIZE = 10000,
  PMI_SELECTION_OBJECT_GET = 20000,
  PMI_DISPATCH = 30000,
  PMI_SUBSCRIBE = 40000,
  PMI_REPORT = 50000,
  PMI_DATA = 60000,
  PMI_FINALIZE = 70000,
  PMI_SC_REPORT = 80000,
  PMI_SC_DATA = 90000
};

enum {
  PMI_LOCATION_1 = 100,
  PMI_LOCATION_2 = 200,
  PMI_LOCATION_3 = 300,
  PMI_LOCATION_4 = 400,
  PMI_LOCATION_5 = 500,
  PMI_LOCATION_6 = 600,
  PMI_LOCATION_7 = 700,
  PMI_LOCATION_8 = 800,
  PMI_LOCATION_9 = 900,
  PMI_LOCATION_10 = 1000,
  PMI_LOCATION_11 = 1100,
  PMI_LOCATION_12 = 1200,
  PMI_LOCATION_13 = 1300,
  PMI_LOCATION_14 = 1400,
  PMI_LOCATION_15 = 1500,
  PMI_LOCATION_16 = 1600,
  PMI_LOCATION_17 = 1700,
  PMI_LOCATION_18 = 1800,
  PMI_LOCATION_19 = 1900,
  PMI_LOCATION_20 = 2000,
  PMI_LOCATION_21 = 2100,
  PMI_LOCATION_22 = 2200,
  PMI_LOCATION_23 = 2300,
  PMI_LOCATION_24 = 2400,
  PMI_LOCATION_25 = 2500,
  PMI_LOCATION_26 = 2600,
  PMI_LOCATION_27 = 2700,
  PMI_LOCATION_28 = 2800,
  PMI_LOCATION_29 = 2900,
  PMI_LOCATION_30 = 3000,
  PMI_LOCATION_31 = 3100,
  PMI_LOCATION_32 = 3200,
  PMI_LOCATION_33 = 3300,
  PMI_LOCATION_34 = 3400,
  PMI_LOCATION_35 = 3500,
  PMI_LOCATION_36 = 3600,
  PMI_LOCATION_37 = 3700,
  PMI_LOCATION_38 = 3800,
  PMI_LOCATION_39 = 3900,
  PMI_LOCATION_40 = 4000,
  PMI_LOCATION_41 = 4100,
  PMI_LOCATION_42 = 4200,
  PMI_LOCATION_43 = 4300,
  PMI_LOCATION_44 = 4400,
  PMI_LOCATION_45 = 4500,
  PMI_LOCATION_46 = 4600,
  PMI_LOCATION_47 = 4700,
  PMI_LOCATION_48 = 4800,
  PMI_LOCATION_49 = 4900,
  PMI_LOCATION_50 = 5000,
  PMI_LOCATION_51 = 5100,
  PMI_LOCATION_52 = 5200,
  PMI_LOCATION_53 = 5300,
  PMI_LOCATION_54 = 5400,
  PMI_LOCATION_55 = 5500,
  PMI_LOCATION_56 = 5600,
  PMI_LOCATION_57 = 5700,
  PMI_LOCATION_58 = 5800,
  PMI_LOCATION_59 = 5900,
  PMI_LOCATION_60 = 6000,
  PMI_LOCATION_61 = 6100,
  PMI_LOCATION_62 = 6200,
  PMI_LOCATION_63 = 6300,
  PMI_LOCATION_64 = 6400,
  PMI_LOCATION_65 = 6500,
  PMI_LOCATION_66 = 6600,
  PMI_LOCATION_67 = 6700,
  PMI_LOCATION_68 = 6800,
  PMI_LOCATION_69 = 6900,
  PMI_LOCATION_70 = 7000,
  PMI_LOCATION_71 = 7100,
  PMI_LOCATION_72 = 7200,
  PMI_LOCATION_73 = 7300
};

enum {
  PMI_OK = 1,
  PMI_UNKNOWN_HANDLE = 2,
  PMI_INTERNAL_HASHMAP_FAILURE = 11,
  PMI_INTERNAL_OUT_OF_MEMORY = 12,
  PMI_INTERNAL_ALLOCATION_FAILURE = 13,
  PMI_INTERNAL_VERSION_MISMATCH = 14,
  PMI_INTERNAL_BAD_ARITY = 15,
  PMI_INTERNAL_UNKNOWN_FUNCTION = 16,
  PMI_INTERNAL_DECODE_FAILURE = 17,
  PMI_INTERNAL_ENCODE_FAILURE = 18,
  PMI_INTERNAL_CEC_OPEN_FAILURE = 19,
  PMI_INTERNAL_CEC_SEND_FAILURE = 20,
  PMI_INTERNAL_CEC_RECEIVE_FAILURE = 21,
  PMI_INTERNAL_CEC_CLOSE_FAILURE = 22,
  PMI_INTERNAL_MAX_HANDLES_EXCEEDED = 23,
  PMI_INTERNAL_HASHMAP_VALUE_OVERWRITE = 24,
  PMI_INTERNAL_HASHMAP_OUT_OF_MEMORY = 25,
  PMI_INTERNAL_HASHMAP_INTERFACE_ERROR = 26,
  PMI_INTERNAL_HASHMAP_OVERWRITE = 27,
  PMI_INTERNAL_LIMITS_INCONSISTENCY = 28
};


/**
 * A PmiResultT value is the sum of an optional function
 * code, an optional location code and a diagnostic
 * code picked from the enums above.
 *
 * The value PMI_OK (with no function and location code)
 * indicates successful execution.
 */
typedef int PmiResultT;


/**
 * A CounterSpecT wraps a pointer to a PM group name and a pointer
 * to a null-terminated array of pointers to measurement type names.
 * Each name is represented as a C-style string (a null-terminated
 * array of bytes).
 */
typedef struct {
	const char *pmGroup;
	const char **measurementTypes;
} CounterSpecT;


/**
 * A MeasurementValueT represents a counter value for a specific
 * measurement type. The counter value has a multiplicity which
 * is 1 or higher. The counter value elements are held in an array
 * of size equal to the multiplicity. The type of an array element
 * is "long long", which is 64 bits signed integer in the target and
 * simulator C programming environments.
 */
typedef struct {
	const char *measurementType;
	unsigned int multiplicity;
	const long long *elements;
} MeasurementValueT;


/**
 * A ValueBundleT represents counter values for a collection of
 * measurement types belonging to a common PM group. The second
 * field is a pointer to a null-terminated array of pointers to
 * MeasurementValueT instances.
 */
typedef struct {
	const char *pmGroup;
	MeasurementValueT **measurementValues;
} ValueBundleT;



/**
 * A PmiSubscribeCallbackT is a pointer to a function that takes
 * a PM session handle, a granularity period and a null-terminated
 * array of pointers to CounterSpecT as arguments. Such a function
 * must be implemented by the application. Invocation of the function
 * is a message to the application that collection of PM data for the
 * implied PM groups and measurement types shall be started.
 */
typedef void (* PmiSubscribeCallbackT) (
		PmiHandleT pmiHandle,
		unsigned int granularityPeriod,
		CounterSpecT **counterSpecs);


/**
 * A PmiReportCallbackT is a pointer to a function that takes a PM
 * session handle, a granularity period and two wall clock times as
 * arguments. Such a function must be implemented by the application.
 * Invocation of the function is a message to the application that
 * reporting of PM data as available at the "timestamp" point in time
 * shall be done. Reports will be accepted only up to the "deadline"
 * point in time; late-arriving reports will be ignored.
 */
typedef void (* PmiReportCallbackT) (
		PmiHandleT pmiHandle,
		unsigned int granularityPeriod,
		time_t timestamp,
		time_t deadline);


/**
 * A PmiReportShowCountersCallbackT is a pointer to a function that takes
 * the following arguments
 * - PM session handle - received from pmiInitialize_2
 * - requestId         - to be returned unchanged in pmiDataShowCounters
 * - moInstancLdn      - string defining the instance for which counters
 *                       are to be fetched
 * - maxResponseTime   - time in seconds within which pmiDataShowCounters should be sent
 * Such a function must be implemented by the application.
 * Invocation of the function is a message to the application to
 * report all the current counter values. 
 */
typedef void (* PmiReportShowCountersCallbackT) (
		PmiHandleT pmiHandle,
		const unsigned int requestId,
		const char *moInstanceLdn,
		const unsigned int maxResponseTime);


/**
 * A PmiCallbacksT is a wrapper for one PmiSubscribeCallbackT and one
 * PmiReportCallbackT.
 */
typedef struct {
	PmiSubscribeCallbackT pmiSubscribeCallback;
	PmiReportCallbackT pmiReportCallback;
} PmiCallbacksT;


/**
 * A PmiCallbacksT_2 is a wrapper for one PmiSubscribeCallbackT and one
 * PmiReportCallbackT.
 */
typedef struct {
  PmiSubscribeCallbackT pmiSubscribeCallback;
  PmiReportCallbackT pmiReportCallback;
  PmiReportShowCountersCallbackT pmiReportShowCountersCallback;
} PmiCallbacksT_2;



/**
 * The groupSpec argument is a null-terminated array of
 * pointers to PmGroup names.
 */
PmiResultT
pmiInitialize(PmiHandleT *pmiHandle,
	      const PmiCallbacksT *pmiCallbacks,
	      const char **groups);
  

PmiResultT
pmiInitialize_2(PmiHandleT *pmiHandle,
		const PmiCallbacksT_2 *pmiCallbacks,
		const char **groups,
		const char *topMoLdn);

PmiResultT
pmiSelectionObjectGet(PmiHandleT pmiHandle,
		      PmiSelectionObjectT *selectionObject);


/**
 * Only the DISPATCH_ONE pattern of [1] is supported,
 * hence no dispatchFlag argument is used here.
 */
PmiResultT
pmiDispatch(PmiHandleT pmiHandle);



PmiResultT
pmiData(PmiHandleT pmiHandle,
	unsigned int granularityPeriod,
	time_t timestamp,
	const char *measObjLdn,
	ValueBundleT **bundles);

PmiResultT
pmiDataShowCounters(PmiHandleT pmiHandle,
		    unsigned int requestId,
		    unsigned int result,
		    char *errorString,
		    MeasurementValueT **bundles);
  

PmiResultT
pmiFinalize(PmiHandleT pmiHandle);

#ifdef __cplusplus
}
#endif

#endif

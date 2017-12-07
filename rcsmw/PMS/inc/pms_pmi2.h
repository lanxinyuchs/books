#ifndef PMS_PMI2_H_
#define PMS_PMI2_H_
/* ----------------------------------------------------------------------
 * %CCaseFile:	pms_pmi2.h %
 * %CCaseRev:	/main/R2A/R3A/R5A/1 %
 * %CCaseDate:	2016-03-17 %
 * %CCaseDocNo: %
 * Author:	
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Definition of the C interface to PM.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
 * R1A/1      2014-08-14 uabesvi     Created
 * R1A/10     2014-10-16 uabesvi     updated for show counters
 * ----------------------------------------------------------------------
 */


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

enum {
  PMI2_CS_OK = 0,
  PMI2_CS_NOT_EXIST = 1,
  PMI2_CS_INTERNAL_ERROR = 2
};

enum {
  PMI2_MAX_SESSIONS = 100
};

enum {
  PMI2_INITIALIZE = 10000,
  PMI2_COUNTER_MAP = 20000,
  PMI2_SELECTION_OBJECT_GET = 30000,
  PMI2_DISPATCH = 40000,
  PMI2_SUBSCRIBE = 50000,
  PMI2_REPORT = 60000,
  PMI2_ROP_DATA = 70000,
  PMI2_FINALIZE = 80000,
  PMI2_SC_REPORT = 90000,
  PMI2_SC_DATA = 100000
};

enum {
  PMI2_LOCATION_1 = 100,
  PMI2_LOCATION_2 = 200,
  PMI2_LOCATION_3 = 300,
  PMI2_LOCATION_4 = 400,
  PMI2_LOCATION_5 = 500,
  PMI2_LOCATION_6 = 600,
  PMI2_LOCATION_7 = 700,
  PMI2_LOCATION_8 = 800,
  PMI2_LOCATION_9 = 900,
  PMI2_LOCATION_10 = 1000,
  PMI2_LOCATION_11 = 1100,
  PMI2_LOCATION_12 = 1200,
  PMI2_LOCATION_13 = 1300,
  PMI2_LOCATION_14 = 1400,
  PMI2_LOCATION_15 = 1500,
  PMI2_LOCATION_16 = 1600,
  PMI2_LOCATION_17 = 1700
};

enum {
  PMI2_OK = 1,
  PMI2_UNKNOWN_HANDLE = 2,
  PMI2_UNKNOWN_COUNTER_MAP_ID = 3,
  PMI2_BAD_PARAMETER = 4,
  PMI2_INTERNAL_ERROR = 5,
  PMI2_INTERNAL_HASHMAP_FAILURE = 11,
  PMI2_INTERNAL_OUT_OF_MEMORY = 12,
  PMI2_INTERNAL_ALLOCATION_FAILURE = 13,
  PMI2_INTERNAL_VERSION_MISMATCH = 14,
  PMI2_INTERNAL_BAD_ARITY = 15,
  PMI2_INTERNAL_UNKNOWN_FUNCTION = 16,
  PMI2_INTERNAL_DECODE_FAILURE = 17,
  PMI2_INTERNAL_ENCODE_FAILURE = 18,
  PMI2_INTERNAL_CEC_OPEN_FAILURE = 19,
  PMI2_INTERNAL_CEC_SEND_FAILURE = 20,
  PMI2_INTERNAL_CEC_RECEIVE_FAILURE = 21,
  PMI2_INTERNAL_CEC_CLOSE_FAILURE = 22,
  PMI2_INTERNAL_MAX_HANDLES_EXCEEDED = 23,
  PMI2_INTERNAL_HASHMAP_VALUE_OVERWRITE = 24,
  PMI2_INTERNAL_HASHMAP_OUT_OF_MEMORY = 25,
  PMI2_INTERNAL_HASHMAP_INTERFACE_ERROR = 26,
  PMI2_INTERNAL_HASHMAP_OVERWRITE = 27,
  PMI2_INTERNAL_LIMITS_INCONSISTENCY = 28,
};


enum {
  TEN_SECONDS    = 10,
  THIRTY_SECONDS = 30,
  ONE_MIN        = 60,
  FIVE_MIN       = 60*5,
  FIFTEEN_MIN    = 60*15,
  THIRTY_MIN     = 60*30,
  ONE_HOUR       = 60*60,
  TWELVE_HOUR    = 60*60*12,
  ONE_DAY        = 60*60*24
};

typedef const void *Pmi2HandleT;


typedef uint32_t Pmi2SelectionObjectT;


/**
 * A Pmi2ResultT value is the sum of an optional function
 * code, an optional location code and a diagnostic
 * code picked from the enums above.
 *
 * The value PMI2_OK (with no function, location or diagnostic codes)
 * indicates successful execution.
 */
typedef uint32_t Pmi2ResultT;




/**
 * A Pmi2SubscribeRopSpecT specifies the currently active counters
 * for a PM Group.
 * - pmGroupIdAlias               - The alias for the PM Group
 * - noofMeasurementTypeIdAliases - Number of measurement type aliases
 *                                  in the array
 * - measurementTypeIdAliases     - An array of counters
 *                                  that are to be measured
 */
typedef struct {
        uint32_t pmGroupIdAlias;
        uint32_t noofMeasurementTypeIdAliases;
        uint32_t *measurementTypeIdAliases;
} Pmi2SubscribeRopSpecT;

/**
 * A Pmi2SubscribeRopCallbackT is a pointer to a function that takes 
 * the following arguments:
 * - pmi2Handle        - Received from pmi2Initialize
 * - granularityPeriod - The GP that the subscribe is valid for
 * - subscribeRopSpecs - A null-terminated array of pointers 
 *                       to Pmi2SubscribeRopSpecT 
 * - userData          - A pointer to user data, as received in pmi2Initialize
 * 
 * Such a function must be implemented by the application if performance 
 * measurement is to be collected. 
 * Invocation of the function is a message to the application that 
 * collection of PM data for the implied PM groups and measurement 
 * types shall be started.
 */
typedef void (* Pmi2SubscribeRopCallbackT) (
                Pmi2HandleT pmi2Handle,
                uint32_t granularityPeriod,
                Pmi2SubscribeRopSpecT const * const *subscribeRopSpecs,
                void *userData);


/**
 * A Pmi2ReportRopCallbackT is a pointer to a function that takes 
 * the following arguments: 
 * - pmi2Handle        - Received from pmi2Initialize
 * - granularityPeriod - The GP that the measurement data is requested for
 * - reportId          - To be returned unchanged in pmi2ReportRopData.
 * - maxReportingTime  - Time in seconds within which pmi2ReportRopData 
 *                       should be sent
 * - userData          - A pointer to user data, as received in pmi2Initialize
 * 
 * Such a function must be implemented by the application if performance 
 * measurement is to be collected. 
 * Invocation of the function is a message to the application that reporting
 * of PM data as available at the reception of the call shall be done.
 * The reports should be sent latest at "maximum reporting time"
 * point in time; late-arriving reports will be ignored.
 */
typedef void (* Pmi2ReportRopCallbackT) (
                Pmi2HandleT pmi2Handle,
                uint32_t granularityPeriod,
                uint32_t reportId,
                uint32_t maxReportingTime,
                void * userData);


/**
 * A Pmi2ShowCountersSpecT specifies the requested counters for
 * show counter command.
 * - pmGroupIdAlias               - The alias for the PM Group
 * - noofMeasurementTypeIdAliases - Number of measurement type aliases
 *                                  in the array
 * - measurementTypeIdAliases     - An array of counters
 *                                  that are to be measured
 */
typedef struct {
        uint32_t pmGroupIdAlias;
        uint32_t noofMeasurementTypeIdAliases;
        uint32_t *measurementTypeIdAliases;
} Pmi2ShowCountersSpecT;


/**
 * A Pmi2ReportShowCountersCallbackT is a pointer to a function that takes
 * the following arguments
 * - pmi2Handle        - Received from pmi2Initialize
 * - reportId          - To be returned unchanged in pmi2ShowCountersData
 * - moInstancLdnAlias - Alias for the instance for which counters
 *                       are to be returned
 * - showCountersSpecs - A null-terminated array of pointers 
 *                       to Pmi2ShowCountersSpecT 
 * - maxReportingTime  - Time in seconds within which pmi2DataShowCounters 
 *                       should be sent
 * - userData          - A pointer to user data, as received in pmi2Initialize
 *
 * Such a function must be implemented by the application if show counters
 * functionality is to be supported.
 * Invocation of the function is a message to the application to
 * report all the current counter values for the requested MO instance. 
 */
typedef void (* Pmi2ReportShowCountersCallbackT) (
                Pmi2HandleT pmi2Handle,
                uint32_t reportId,
                uint32_t moInstanceLdnAlias,
                Pmi2ShowCountersSpecT const * const *showCountersSpecs,
                uint32_t maxReportingTime,
                void *userData);


/**
 * A Pmi2SubscribeRopCallbackSpecT defines the subscribe rop  
 * callback function.
 * It is possible to define a pointer to user data, which will be
 * returned in the callback function.
 */
typedef struct {
  Pmi2SubscribeRopCallbackT subscribeRopCallback;
  void *userData;
} Pmi2SubscribeRopCallbackSpecT;

/**
 * A Pmi2ReportRopCallbackSpecT defines the report rop  
 * callback function.
 * It is possible to define a pointer to user data, which will be
 * returned in the callback function.
 */
typedef struct {
  Pmi2ReportRopCallbackT reportRopCallback;
  void *userData;
} Pmi2ReportRopCallbackSpecT;

/**
 * A Pmi2ReportShowCountersCallbackSpecT defines the report show counters  
 * callback function.
 * It is possible to define a pointer to user data, which will be
 * returned in the callback function.
 */
typedef struct {
  Pmi2ReportShowCountersCallbackT reportShowCountersCallback;
  void *userData;
} Pmi2ReportShowCountersCallbackSpecT;



/**
 * A Pmi2CallbacksT is a wrapper for the PMI2 callback specs.
 */
typedef struct {
  Pmi2SubscribeRopCallbackSpecT *subscribeRopCallbackSpec;
  Pmi2ReportRopCallbackSpecT *reportRopCallbackSpec;
  Pmi2ReportShowCountersCallbackSpecT *reportShowCountersCallbackSpec;
} Pmi2CallbacksT;



/**
 * A Pmi2MeasurementTypeMapT creates an alias for a Measurement Type
 * instance. The alias is used i the subsequent function calls for ROP and
 * and show counters.
 */
typedef struct {
        const char *measurementTypeId;
        uint32_t measurementTypeIdAlias;
} Pmi2MeasurementTypeMapT;



/**
 * A Pmi2CounterMapT creates an alias for a PM Group instances.
 * It also defines the Measurement Types belonging to the PM Group
 * instance by defining a null-terminated array of pointers to
 * the Pmi2MeasurementMapT instances.
 */
typedef struct {
        const char *pmGroupId;
        uint32_t pmGroupIdAlias;
        Pmi2MeasurementTypeMapT const * const *measurementTypes;
} Pmi2CounterMapT;



/**
 * A Pmi2MeasurementValueT represents a counter value for a specific
 * measurement type. The counter value has a multiplicity which
 * is 1 or higher. The counter value elements are held in a null-terminated 
 * array. The type of an array element is "int64_t", which is 64 bits 
 * signed integer in the target and simulator C programming environments.
 */
typedef struct {
        uint32_t measurementTypeIdAlias;
        uint32_t multiplicity;
        const int64_t *elements;
} Pmi2MeasurementValueT;


/**
 * A Pmi2MoInstanceBundleT is a container for an MO instance and
 * its measurement values.
 * - moInstanceLdnAlias - The alias for the MO instance
 * - measurementValues  - A null-terminated list of measurement values
 */
typedef struct {
        uint32_t moInstanceLdnAlias;
        Pmi2MeasurementValueT const * const *measurementValues;
} Pmi2MoInstanceBundleT;


/**
 * A Pmi2ValueBundleT represents counter values for a collection of
 * measurement types belonging to a common PM group. 
 * - pmGroupIdAlias - The PM group for which the counter values are valid
 * - instanceBudles - A null-terminated array of instance bundles
 */
typedef struct {
        uint32_t pmGroupIdAlias;
        Pmi2MoInstanceBundleT const * const *instanceBundles;
} Pmi2ValueBundleT;





/*
 * Initialie a PMI2 session
 *
 * If counterMapId is set to to the counter alias appdata file then
 * the pmi2CounterMap function calls will be ignored.
 * If counterMapId is set to NULL then the counter aliases must be
 * defined by invoking pmi2CounterMap function.
 */
Pmi2ResultT
pmi2Initialize(Pmi2HandleT *pmi2Handle,
	       const char *counterMapId,
               const Pmi2CallbacksT *pmi2Callbacks);
  


/*
 * Get a selection object. Used for polling of messages sent from PMS.
 * 
 */
Pmi2ResultT
pmi2SelectionObjectGet(Pmi2HandleT pmi2Handle,
                       Pmi2SelectionObjectT *selectionObject);


/*
 * Only the DISPATCH_ONE pattern of [1] is supported,
 * hence no dispatchFlag argument is used here.
 */
Pmi2ResultT
pmi2Dispatch(Pmi2HandleT pmi2Handle);


/*
 * Attach counters to a PMI2 session 
 * 
 */
Pmi2ResultT
pmi2CounterMap(Pmi2HandleT pmi2Handle,
               Pmi2CounterMapT const * const *counterMap);


/*
 * Send the measurument values for a measurement period
 */
Pmi2ResultT
pmi2DataRop(Pmi2HandleT pmi2Handle,
            uint32_t granularityPeriod,
            uint32_t reportId,
            Pmi2ValueBundleT const * const *bundles,
            const uint32_t finalFragment);

/*
 * Send the counter values for a show counters request
 * 
 */
Pmi2ResultT
pmi2DataShowCounters(Pmi2HandleT pmi2Handle,
                     uint32_t reportId,
                     uint32_t result,
                     const char *errorString,
                     Pmi2ValueBundleT const * const *bundles);
  

/*
 * Close a PMI2 session
 */
Pmi2ResultT
pmi2Finalize(Pmi2HandleT pmi2Handle);



#ifdef __cplusplus
}
#endif

#endif

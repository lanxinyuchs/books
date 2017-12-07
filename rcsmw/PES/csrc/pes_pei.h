#ifndef PES_PEI_H_
#define PES_PEI_H_
/* ----------------------------------------------------------------------
 * %CCaseFile:	pes_pei.h %
 * %CCaseRev:	/main/R3A/15 %
 * %CCaseDate:	2014-12-09 %
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
 * R1A/1      2014-09-26 uabesvi     Created
 * ----------------------------------------------------------------------
 */


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

enum {
  PEI_CS_OK = 0,
  PEI_CS_NOT_EXIST = 1,
  PEI_CS_INTERNAL_ERROR = 2
};

enum {
  PEI_MAX_SESSIONS = 100
};

enum {
  PEI_INITIALIZE = 10000,
  PEI_SELECTION_OBJECT_GET = 20000,
  PEI_DISPATCH = 30000,
  PEI_ME_ATTR_UPDATE = 40000,
  PEI_EVENT_JOB = 50000,
  PEI_FINALIZE = 60000
};

enum {
  PEI_LOCATION_1 = 100,
  PEI_LOCATION_2 = 200,
  PEI_LOCATION_3 = 300,
  PEI_LOCATION_4 = 400,
  PEI_LOCATION_5 = 500,
  PEI_LOCATION_6 = 600,
  PEI_LOCATION_7 = 700,
  PEI_LOCATION_8 = 800,
  PEI_LOCATION_9 = 900,
  PEI_LOCATION_10 = 1000,
  PEI_LOCATION_11 = 1100,
  PEI_LOCATION_12 = 1200,
  PEI_LOCATION_13 = 1300,
  PEI_LOCATION_14 = 1400,
  PEI_LOCATION_15 = 1500,
  PEI_LOCATION_16 = 1600,
  PEI_LOCATION_17 = 1700
};

enum {
  PEI_OK = 1,
  PEI_UNKNOWN_HANDLE = 2,
  PEI_UNKNOWN_EVENT_MAP_ID = 3,
  PEI_BAD_PARAMETER = 4,
  PEI_INTERNAL_ERROR = 5,
  PEI_INTERNAL_HASHMAP_FAILURE = 11,
  PEI_INTERNAL_OUT_OF_MEMORY = 12,
  PEI_INTERNAL_ALLOCATION_FAILURE = 13,
  PEI_INTERNAL_VERSION_MISMATCH = 14,
  PEI_INTERNAL_BAD_ARITY = 15,
  PEI_INTERNAL_UNKNOWN_FUNCTION = 16,
  PEI_INTERNAL_DECODE_FAILURE = 17,
  PEI_INTERNAL_ENCODE_FAILURE = 18,
  PEI_INTERNAL_CEC_OPEN_FAILURE = 19,
  PEI_INTERNAL_CEC_SEND_FAILURE = 20,
  PEI_INTERNAL_CEC_RECEIVE_FAILURE = 21,
  PEI_INTERNAL_CEC_CLOSE_FAILURE = 22,
  PEI_INTERNAL_MAX_HANDLES_EXCEEDED = 23,
  PEI_INTERNAL_HASHMAP_VALUE_OVERWRITE = 24,
  PEI_INTERNAL_HASHMAP_OUT_OF_MEMORY = 25,
  PEI_INTERNAL_HASHMAP_INTERFACE_ERROR = 26,
  PEI_INTERNAL_HASHMAP_OVERWRITE = 27,
  PEI_INTERNAL_LIMITS_INCONSISTENCY = 28
};


typedef const void *PeiHandleT;


typedef uint32_t PeiSelectionObjectT;


/**
 * A PeiResultT value is the sum of an optional function
 * code, an optional location code and a diagnostic
 * code picked from the enums above.
 *
 * The value PEI_OK (with no function, location or diagnostic codes)
 * indicates successful execution.
 */
typedef uint32_t PeiResultT;

/**
 * A PeiEventFilterT 
 * 
 * Event filter name and value.
 */
typedef struct {
  char *name;
  char *value;
} PeiEventFilterT;


/**
 * PeiCompressionTypeT 
 * 
 * Compression Type used in Stream and File Control.
 */
typedef struct {
  uint32_t value;
} PeiCompressionTypeT;


/**
 * PeiStreamControlT 
 * 
 * Attributes needed for streaming the events
 */
typedef struct {
  PeiCompressionTypeT *compressionType;
  char *destinationIpAddress;
  uint32_t destinationPort;
} PeiStreamControlT;


/**
 * PeiFileControlT 
 * 
 * Attributes needed for ROP files
 */
typedef struct {
  uint32_t reportingPeriod;
  PeiCompressionTypeT *compressionType;
} PeiFileControlT;


/**
 * PeiEventTypeT 
 * 
 * Active Event Types 
 */
typedef struct {
  uint32_t noofTypeAliases;
  uint32_t *typeAliases;
} PeiEventTypeT;


/**
 * A PeiEventJobCallbackT is a pointer to a function that takes
 * the following arguments
 * - peiHandle         - Received from peiInitialize
 * - eventProducerId   - Identity of the event producer 
 * - eventJobId        - Identity of the event job
 * - requestedJobState - Active | Stopped
 * - eventTypes        - A pointer a struct defining the active Event Types
 * - eventFilterIds    - A null-terminated array of pointers to eventFilter
 * - fileControl       - Attributes needed for ROP files
 * - streamControl     - Attributes needed for streaming the events
 * - userData          - A pointer to user data, as received in peiInitialize
 *
 */
typedef void (* PeiEventJobCallbackT) (
                PeiHandleT peiHandle,
		char *eventProducerId,
		char *eventJobId,
                uint32_t requestedJobState,
		PeiEventTypeT const *eventTypes,
		PeiEventFilterT const * const *eventFilters,
		PeiFileControlT *fileControl,
		PeiStreamControlT *streamControl,
                void *userData);


/**
 * A PeiEventJobCallbackSpecT defines the event job  
 * callback function.
 * It is possible to define a pointer to user data, which will be
 * returned in the callback function.
 */
typedef struct {
  PeiEventJobCallbackT eventJobCallback;
  void *userData;
} PeiEventJobCallbackSpecT;


/**
 * A PeiMEAttrUpdateCallbackT is a pointer to a function that takes
 * the following arguments
 * - peiHandle               - Received from peiInitialize
 * - userLabel               - User label from ManagedElement
 * - networkManagedElementId - User label from ManagedElement
 * - userData                - A pointer to user data, as received in 
 *                             peiInitialize
 *
 */
typedef void (* PeiMEAttrUpdateCallbackT) (
                PeiHandleT peiHandle,
		char *userLabel,
		char *networkManagedElementId,
                void *userData);



/**
 * A PeiMEAttrUpdateCallbackSpecT defines the ManagedElement Attribute Update  
 * callback function.
 * It is possible to define a pointer to user data, which will be
 * returned in the callback function.
 */
typedef struct {
  PeiMEAttrUpdateCallbackT meAttrUpdateCallback;
  void *userData;
} PeiMEAttrUpdateCallbackSpecT;



/**
 * A PeiCallbacksT is a wrapper for the PEI callback specs.
 */
typedef struct {
  PeiEventJobCallbackSpecT *eventJobCallbackSpec;
  PeiMEAttrUpdateCallbackSpecT *meAttrUpdateCallbackSpec;
} PeiCallbacksT;



/*
 * Initialie a PEI session
 *
 * eventMapId - specifies which event map appdata file is handled by this 
 *              session eventMap must map to the peiSession tag in 
 *              the appdata file
 * peiCallbacks - defines the callback functions this session supports
 */
PeiResultT
peiInitialize(PeiHandleT *peiHandle,
	      char *eventMapId,
              const PeiCallbacksT *peiCallbacks);
  


/*
 * Get a selection object. Used for polling of messages sent from PES.
 * 
 */
PeiResultT
peiSelectionObjectGet(PeiHandleT peiHandle,
                      PeiSelectionObjectT *selectionObject);


/*
 * Only the DISPATCH_ONE pattern of [1] is supported,
 * hence no dispatchFlag argument is used here.
 */
PeiResultT
peiDispatch(PeiHandleT peiHandle);



/*
 * Close a PEI session
 */
PeiResultT
peiFinalize(PeiHandleT peiHandle);



#ifdef __cplusplus
}
#endif

#endif

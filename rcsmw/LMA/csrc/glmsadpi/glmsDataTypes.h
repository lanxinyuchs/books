/*
 * Copyright (c) Ericsson AB 2017 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

/**
 * @file
 * @brief Data type definitions for the GLMS Adapter interface.
 *
 * This headerfile holds the data type definitions and macros that are used
 * by the signals in the glms_adpi.sig file.
 */

#ifndef GLMS_DATA_TYPES_H
#define GLMS_DATA_TYPES_H

#include <time.h>
#include <stdint.h>
#include <itc.h>

#include "timetFix.h"

/** @defgroup adpidata Adapter Interface Data Types
 * @{
 */

/** Max string length of a resultInfo variable. */
#define GLMS_RESULT_INFO_LEN                (200)

/** Max string length of an MO action name. */
#define GLMS_ASYNC_ACTION_NAME_LEN          (20)

/** Max string length of an MO action additional info. */
#define GLMS_ASYNC_ACTION_ADDINFO_LEN       (100)

/** Max string length of an MO action progress info. */
#define GLMS_ASYNC_ACTION_PROGRESSINFO_LEN  (100)

/** Max string length of an MO action result info.  */
#define GLMS_ASYNC_ACTION_RESULTINFO_LEN    (100)

/** Max string length of a fingerprint. */
#define GLMS_FINGERPRINT_LEN                (256)

/** Max string length of a product type. */
#define GLMS_PRODUCT_TYPE_LEN               (128)

/** Max string length of a Key File location. */
#define GLMS_KEYFILE_LOCATION_LEN           (512)

/** Max string length of a Key File URI. */
#define GLMS_KEYFILE_URI_LEN                (GLMS_KEYFILE_LOCATION_LEN)

/** Max string length of a Key File SFTP password. */
#define GLMS_PASSWORD_LEN                   (128)

/** Max string length of an MO additional info.  */
#define GLMS_ADDITIONAL_INFO_LEN            (256)

/** Max string length of a key Id. */
#define GLMS_KEY_ID_LEN                     (24)

/** Max string length of a MO Id. */
#define GLMS_MO_KEY_LEN                     (GLMS_KEY_ID_LEN + 10)

/** Max string length of a key name. */
#define GLMS_KEY_NAME_LEN                   (128)

/** Max string length of a description. */
#define GLMS_DESCRIPTION_LEN                (128)

/** Max string length of a capacity unit specifier.
    GLMS_CAPACITY_UNIT_LEN is MAX_SIZE_OF_LCCI_LICENSE_DATA_S_CAPACITY_UNIT + 1.
    The reason for the plus one is that some clients use 30 characters for
    the capacity unit. */
#define GLMS_CAPACITY_UNIT_LEN              (31)

/** Max string length of a table name use by Persistent and
    Software Parameters. */
#define GLMS_TABLE_NAME_LEN                 (80)

/** Max string length of an area id. */
#define GLMS_AREA_ID_LEN                    (256)

/** GlmsAdpi protocol definition version 1. */
#define GLMS_ADPI_PROTOCOL_VERSION_1        (1)


/** License Key expiration date with noStop date. */
#define GLMS_NO_STOP_DATE                   ((time_t32) -1)


/** GLMS Bool definition. */
typedef uint32_t GlmsBool;
#define GLMS_FALSE (0) /**< False */
#define GLMS_TRUE  (1) /**< True */

/** Log level used by GLMS, higher setting means more logs. */
typedef uint32_t GlmsLogLevel;
#define GLMS_LOG_DISABLED  (0)
#define GLMS_LOG_LOW       (4)
#define GLMS_LOG_MEDIUM    (7)
#define GLMS_LOG_HIGH      (10)

/** Activation state of various modes (EU, IU, AM, etc). */
typedef uint32_t GlmsActivationState;
#define GLMS_ACTIVATION_STATE_INACTIVE            (0)
#define GLMS_ACTIVATION_STATE_ACTIVATED           (1)
#define GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING  (2)
#define GLMS_ACTIVATION_STATE_EXPIRED             (3)
#define GLMS_ACTIVATION_STATE_DEACTIVATED         (4)

/** Result of a request between GLMS and adapter. */
typedef uint32_t GlmsResult;
#define GLMS_NOK (0)
#define GLMS_OK  (1)

/** The LM state. */
typedef uint32_t GlmsLmState;
#define GLMS_LOCKED             (0)
#define GLMS_NORMAL             (1)
#define GLMS_EMERGENCY_UNLOCK   (2)
#define GLMS_INTEGRATION_UNLOCK (3)
#define GLMS_AUTONOMOUS_MODE    (4)

/** Result of an asynchronous action. */
typedef uint32_t GlmsAsyncActionResult;
#define GLMS_FAILURE        (0)
#define GLMS_SUCCESS        (1)
#define GLMS_NOT_AVAILABLE  (2)

/** State of an asynchronous action. */
typedef uint32_t GlmsAsyncActionState;
#define GLMS_CANCELLING  (0)
#define GLMS_RUNNING     (1)
#define GLMS_FINISHED    (2)
#define GLMS_CANCELLED   (3)

/** featureState value of FeatureState MO. */
typedef uint32_t GlmsFeatureState;
#define GLMS_FEATURESTATE_DEACTIVATED (0)
#define GLMS_FEATURESTATE_ACTIVATED   (1)

/** licenseState value of FeatureState and CapacityState MO:s. */
typedef uint32_t GlmsLicenseState;
#define GLMS_LICENSESTATE_DISABLED    (0)
#define GLMS_LICENSESTATE_ENABLED     (1)

/** serviceState value of FeatureState MO. */
typedef uint32_t GlmsServiceState;
#define GLMS_SERVICESTATE_INOPERABLE  (0)
#define GLMS_SERVICESTATE_OPERABLE    (1)

/** The type of a License Key. */
typedef uint32_t GlmsLicenseType;
#define GLMS_FEATURE_KEY  (0)
#define GLMS_CAPACITY_KEY (1)
#define GLMS_EMERGENCY_RESET_KEY (2)

/** The state of alarms. */
typedef uint32_t GlmsAlarmState;
#define GLMS_ALARM_CEASED     (0)
#define GLMS_ALARM_ACTIVATED  (1)

/** The severity of an activated alarm. */
typedef uint32_t GlmsAlarmSeverity;
#define GLMS_ALARM_NONE     (0)
#define GLMS_ALARM_MINOR    (1)
#define GLMS_ALARM_WARNING  (2)
#define GLMS_ALARM_MAJOR    (3)

/** The reason why an alarm was activated. */
typedef uint32_t GlmsAlarmReason;
#define GLMS_ALARM_REASON_CEASED               (0)
#define GLMS_ALARM_REASON_KEY_NOT_AVAILABLE    (1)
#define GLMS_ALARM_REASON_KEY_EXPIRED          (2)

/** The source of a License Key. */
typedef GlmsBool GlmsKeyType;
#define GLMS_KEY_TYPE_NODE         (GLMS_FALSE)
#define GLMS_KEY_TYPE_CENTRALIZED  (GLMS_TRUE)

/**
  The result of an installAreaLicenseKeys action.

  GlmsInstallAreaLicenseKeyResult codes:

  GLMS_ALKF_INSTALL_SUCCESS -
    Success.

  GLMS_ALKF_INSTALL_NOT_READABLE -
    Failed to read keys string. This can be a badly formatted keys string
    or a failure because of keys string cannot be decrypted correctly.

  GLMS_ALKF_INSTALL_UNSUPPORTED_VERSION -
    The version of the keys string is not supported by
    current version of GLMS.

  GLMS_ALKF_INSTALL_LICENSE_AREA_ID_MISMATCH -
    The area id hash is incorrect or the area id does not match the
    area id of the node.

  GLMS_ALKF_INSTALL_INVALID_SEQUENCE_NUMBER -
    The sequence number in the keys string is lower than
    the sequence number on the node.

  GLMS_ALKF_INSTALL_OTHER_FAILURE -
     Failure not covered by the other codes.
*/
typedef uint32_t GlmsInstallAreaLicenseKeyResult;
#define GLMS_ALKF_INSTALL_SUCCESS                   (0)
#define GLMS_ALKF_INSTALL_NOT_READABLE              (1)
#define GLMS_ALKF_INSTALL_UNSUPPORTED_VERSION       (2)
#define GLMS_ALKF_INSTALL_LICENSE_AREA_ID_MISMATCH  (3)
#define GLMS_ALKF_INSTALL_INVALID_SEQUENCE_NUMBER   (4)
#define GLMS_ALKF_INSTALL_OTHER_FAILURE             (5)

/** Struct with variables according to ECIM AsyncAction. */
typedef struct
{
   time_t32               timeActionCompleted;
   time_t32               timeActionStarted;
   time_t32               timeOfLastStatusUpdate;
   GlmsAsyncActionResult  result;
   GlmsAsyncActionState   state;
   uint32_t               actionId;
   uint32_t               progressPercentage;
   char                   actionName[GLMS_ASYNC_ACTION_NAME_LEN];
   char                   additionalInfo[GLMS_ASYNC_ACTION_ADDINFO_LEN];
   char                   progressInfo[GLMS_ASYNC_ACTION_PROGRESSINFO_LEN];
   char                   resultInfo[GLMS_ASYNC_ACTION_RESULTINFO_LEN];
} GlmsAsyncAction;

/** Capacity value of a CapacityKey. */
typedef struct
{
   GlmsBool    noLimit;
   int32_t     value;
} GlmsCapacityValue;

/** Attributes for a GracePeriod MO. */
typedef struct
{
   time_t32    gracePeriodExpiration;
   int32_t     gpConfiguredLength;
   int32_t     gpConfiguredResetThreshold;
   int32_t     gpConfiguredActivationThreshold;
   GlmsActivationState  gracePeriodState;
   char        gracePeriodId[GLMS_MO_KEY_LEN]; /* graceperiodId is same as capacityStateId*/
} GlmsGracePeriod;

/** Feature configuration data. */
typedef struct
{
   char keyId[GLMS_KEY_ID_LEN];
} GlmsFeatureConfigurationData;

/**
 * @}
 */

#endif /* GLMS_DATA_TYPES_H */

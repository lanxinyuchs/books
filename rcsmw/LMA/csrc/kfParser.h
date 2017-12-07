/**
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2013-06-29 Ramakrushna Mishra
 *   Change  : First version.
 *
 * ========================================================================
 */
#ifndef GLMS_KF_PARSER_H
#define GLMS_KF_PARSER_H

#include "glmsadpi/glmsDataTypes.h"
#include "timetFix.h"
#include "lihi/licenseCapacityControlICommon.h"

/*
 * DEFINITIONS
 */

#define GLMS_SWLT_LEN             (20)
#define GLMS_FORMAT_VERSION_LEN   (16)


#define FINGER_PRINT_METHOD_1   (1)
#define FINGER_PRINT_METHOD_3   (3)
#define FINGER_PRINT_METHOD_11 (11)
#define PKI                     (5)
#define LICENSE_KEY_FILE  "/license/license.xml"
/*
 * DATA STRUCTURES
 */

typedef struct FeatureKeyInstallData_s FeatureKeyInstallData;
typedef struct FeatureKeyInstanceInstallData_s FeatureKeyInstanceInstallData;
typedef struct CapacityKeyInstallData_s CapacityKeyInstallData;
typedef struct CapacityKeyInstanceInstallData_s CapacityKeyInstanceInstallData;

typedef enum
{
 LKF_SUCCESS=0,
 LKF_ERROR,
 FS_ERROR,
 MEM_ERROR,
 PROG_ERROR,
 PKI_ERROR
} ParseResult;

typedef struct
{
   GlmsBool resetKeyAvailable;
   time_t32 startDate;
   time_t32 stopDate;
} EmergencyResetKeyInstallData;

struct CapacityKeyInstanceInstallData_s
{
   time_t32  startDate;
   time_t32  stopDate;
   GlmsCapacityValue  capacity;
   GlmsCapacityValue  hardLimit;
   uint32_t  notContractuallyLimited;
   char hwacString[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING];
   struct CapacityKeyInstanceInstallData_s  *nextInstance;
};

struct CapacityKeyInstallData_s
{
   char   keyId[GLMS_KEY_ID_LEN];
   CapacityKeyInstanceInstallData   *instanceList;
   struct CapacityKeyInstallData_s  *nextCapacityKey;
};


struct FeatureKeyInstanceInstallData_s
{
   time_t32 startDate;
   time_t32 stopDate;
   struct   FeatureKeyInstanceInstallData_s  *nextInstance;
};

struct FeatureKeyInstallData_s
{
   char   keyId[GLMS_KEY_ID_LEN];
   FeatureKeyInstanceInstallData   *instanceList;
   struct FeatureKeyInstallData_s  *nextFeatureKey;
};

typedef struct
{
   uint32_t signatureType;
   uint32_t sequenceNumber;
   uint32_t fingerprintMethod;
   time_t32 generated;
   char     swlt[GLMS_SWLT_LEN];
   char     productType[GLMS_PRODUCT_TYPE_LEN];
   char     formatVersion[GLMS_FORMAT_VERSION_LEN];
   char     fingerprint[GLMS_FINGERPRINT_LEN];
   EmergencyResetKeyInstallData   emergencyKey;
   FeatureKeyInstallData         *featureKeyList;
   CapacityKeyInstallData        *capacityKeyList;
} KeyFileInstallData;




/*
 * FUNCTION PROTOTYPES
 */
ParseResult
parseKeyFile(const char *keyFileLocation,
             KeyFileInstallData **keyFileInstallData);

void
freeKeyFileInstallData(KeyFileInstallData **keyFileInstallData);

void
formatKeyId (char *keyId_p, uint32_t len);

#endif /* GLMS_KF_PARSER_H */

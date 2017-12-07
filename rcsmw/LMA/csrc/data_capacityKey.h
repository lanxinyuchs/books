#ifndef GLMS_CAPACITY_KEY_DATA_H
#define GLMS_CAPACITY_KEY_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"
#include "lihi/licenseCapacityControlICommon.h"
#include "data_capacityState.h"


struct CapacityKey_s
{
   char         capacityKeyId[GLMS_MO_KEY_LEN];        /* Persistent Parameter */
   /* keyId        - stored in parent CapacityState */
   /* name         - stored in parent CapacityState */
   /* productType  - stored in parent CapacityState */
   /* capacityUnit - stored in parent CapacityState */
   /* grantedCapacityLevel - stored in parent CapacityState */
   /* licensedCapacityLimitReached 
                           - stored in parent CapacityState */
   time_t32          validFrom;                        /* Persistent Parameter */
   time_t32          expiration;                       /* Persistent Parameter */
   GlmsCapacityValue licensedCapacityValue;            /* Persistent Parameter */
   uint32_t          notContractuallyLimited;          /* Persistent Parameter */
   char              hwacString[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING];

   /* Internal Data */
   GlmsBool     markedAtKfParsing;
   GlmsBool     pendingUpdates[2];
   uint32_t     tableIndex;

   struct CapacityState_s   *parentCapacityState;
   struct CapacityKey_s     *nextCapacityKey;
   struct CapacityKey_s     *prevCapacityKey;
};
typedef struct CapacityKey_s CapacityKey;

typedef struct
{
   uint32_t    readIndexesFromTable[2];
   uint32_t    indexesInTable[2];
   uint32_t    *tableIndex[2];
   int32_t     readStatus[2];
   uint32_t    tableIndexPool;
} CapacityKeyData;


/*
 * Capacity Key function prototypes
 *
 */
void capacityKeyDataInit();
void capacityKeyDataClear();

void             capacityKey_storeParameters(CapacityKey *ckElement);
void             capacityKey_fetchParameters();
int32_t          capacityKey_parseStoredParameters(union itc_msg *sig);


GlmsBool         capacityKey_isCapacityKeyLast(CapacityKey *ckElement);
CapacityKey      *capacityKey_getNext(CapacityKey *ckElement);
CapacityKey      *capacityKey_getPrev(CapacityKey *ckElement);
CapacityKey      *capacityKey_findByCapacityKeyId(char *capacityKeyId);
void             capacityKey_clearAllKeys(CapacityKey *ckElement);
void             capacityKey_clearKey(CapacityKey *ckElement);
void             capacityKey_deleteKeyAndMo(CapacityKey *ckElement);
CapacityKey      *capacityKey_createKeyWithMoIndication(char *keyId,
                                                        time_t32 validFrom,
                                                        time_t32 expiration,
                                                        GlmsCapacityValue licensedCapacityValue,
                                                        uint32_t notContractuallyLimited,
                                                        char *hwacString);

struct CapacityState_s    *capacityKey_getParentCapacityState(CapacityKey *ckElement);
char*            capacityKey_getCapacityKeyId(CapacityKey *ckElement);
char*            capacityKey_getKeyId(CapacityKey *ckElement);
char*            capacityKey_getName(CapacityKey *ckElement);
char*            capacityKey_getCapacityUnit(CapacityKey *ckElement);
void             capacityKey_sendMoUpdateCapacityKeyNameInd(CapacityKey *ckElement);
void             capacityKey_sendMoUpdateCapacityKeyCapacityUnitInd(CapacityKey *ckElement);
char*            capacityKey_getProductType(CapacityKey *ckElement);
time_t32         capacityKey_getValidFrom(CapacityKey *ckElement);
time_t32         capacityKey_getExpiration(CapacityKey *ckElement);
uint32_t         capacityKey_getTableIndex(CapacityKey *ckElement);
uint32_t         capacityKey_getGrantedCapacityLevel(CapacityKey *ckElement);
GlmsCapacityValue capacityKey_getLicensedCapacityValue(CapacityKey *ckElement);
GlmsBool         capacityKey_isNotContractuallyLimited(CapacityKey *ckElement);
GlmsBool         capacityKey_getLicensedCapacityLimitReached(CapacityKey *ckElement);
char *           capacityKey_getHwacString(CapacityKey *ckElement);

void             capacityKey_resetKfParsingMark(CapacityKey *ckElement);
void             capacityKey_markAtKfParsing(CapacityKey *ckElement);
GlmsBool         capacityKey_isMarkedAtKfParsing(CapacityKey *ckElement);

#endif /* GLMS_CAPACITY_KEY_DATA_H */

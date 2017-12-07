#ifndef GLMS_FEATURE_KEY_DATA_H
#define GLMS_FEATURE_KEY_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"
#include "lihi/licenseFeatureControlICommon.h"
#include "data_featureState.h"

struct FeatureKey_s
{
   char         featureKeyId[GLMS_MO_KEY_LEN];        /* Persistent Parameter */
   /* keyId        - stored in parent FeatureState */
   /* name         - stored in parent FeatureState */
   /* productType  - stored in parent FeatureState */
   time_t32     validFrom;                            /* Persistent Parameter */
   time_t32     expiration;                           /* Persistent Parameter */

   /* Internal Data */
   GlmsKeyType  keyType;                              /* Persistent Parameter */
   GlmsBool     markedAtKfParsing;
   GlmsBool     pendingUpdates[2];
   uint32_t     tableIndex;

   struct FeatureState_s   *parentFeatureState;
   struct FeatureKey_s     *nextFeatureKey;
   struct FeatureKey_s     *prevFeatureKey;
};
typedef struct FeatureKey_s FeatureKey;

struct LatentFeatureKey_s
{
   char         keyId[GLMS_KEY_ID_LEN]; /* Persistent Parameter */
   time_t32     validFrom;              /* Persistent Parameter */
   time_t32     expiration;             /* Persistent Parameter */
   GlmsKeyType  keyType;                /* Persistent Parameter */
   GlmsBool     markedAtKfParsing;
   struct LatentFeatureKey_s *nextFeatureKey;
};
typedef struct LatentFeatureKey_s LatentFeatureKey;

typedef struct
{
   LatentFeatureKey *latentKeyList;

   uint32_t    readIndexesFromTable[2];
   uint32_t    indexesInTable[2];
   uint32_t    *tableIndex[2];
   int32_t     readStatus[2];
   uint32_t    tableIndexPool;
} FeatureKeyData;


/*
 * Feature Key function prototypes
 *
 */
void featureKeyDataInit();
void featureKeyDataClear();

void             featureKey_storeParameters(FeatureKey *fkElement);
void             featureKey_fetchParameters();
int32_t          featureKey_parseStoredParameters(union itc_msg *sig);


GlmsBool         featureKey_isFeatureKeyLast(FeatureKey *fkElement);
FeatureKey      *featureKey_getNext(FeatureKey *fkElement);
FeatureKey      *featureKey_getPrev(FeatureKey *fkElement);
FeatureKey      *featureKey_findByFeatureKeyId(const char *featureKeyId);
void             featureKey_clearKeys(FeatureKey *fk, GlmsKeyType keyType);
void             featureKey_clearKey(FeatureKey *fkElement);
void             featureKey_deleteKeyAndMo(FeatureKey *fkElement);
FeatureKey      *featureKey_createKeyWithMoIndication(const char *keyId,
                                                      time_t32 validFrom,
                                                      time_t32 expiration,
                                                      GlmsKeyType keyType);

struct FeatureState_s    *featureKey_getParentFeatureState(FeatureKey *fkElement);
char*            featureKey_getFeatureKeyId(FeatureKey *fkElement);
char*            featureKey_getKeyId(FeatureKey *fkElement);
char*            featureKey_getName(FeatureKey *fkElement);
void             featureKey_sendMoUpdateFeatureKeyNameInd(FeatureKey *fkElement);
char*            featureKey_getProductType(FeatureKey *fkElement);
time_t32         featureKey_getValidFrom(FeatureKey *fkElement);
time_t32         featureKey_getExpiration(FeatureKey *fkElement);
GlmsKeyType      featureKey_getKeyType(FeatureKey *fkElement);
uint32_t         featureKey_getTableIndex(FeatureKey *fkElement);

void             featureKey_resetKfParsingMark(FeatureKey *fkElement, GlmsKeyType keyType);
void             featureKey_markAtKfParsing(FeatureKey *fkElement);
GlmsBool         featureKey_isMarkedAtKfParsing(FeatureKey *fkElement);

/* Latent keys are keys that are currently not used. They are not used
   because they do not have a current FeatureState MO. The reason no
   FeatureState MO exist can be that there is no subscriber for the
   feature or that it doesn't exist in configuration data.
   Once a FeatureState MO is created the latent key shall become
   an ordinary key.
*/
LatentFeatureKey *featureKey_findLatentKey(const char *keyId);
LatentFeatureKey *featureKey_findUniqueLatentKey(const char *keyId,
                                                 time_t32 validFrom,
                                                 time_t32 expiration,
                                                 GlmsKeyType keyType);
LatentFeatureKey *featureKey_addLatentKey(const char *keyId,
                                          time_t32 validFrom,
                                          time_t32 expiration,
                                          GlmsKeyType keyType);
void              featureKey_resetLatentKeysMark(GlmsKeyType keyType);
void              featureKey_markLatentKey(LatentFeatureKey *key);
void              featureKey_clearUnmarkedLatentKeys(GlmsKeyType keyType);
void              featureKey_clearLatentKeysOfKeyType(GlmsKeyType keyType);
void              featureKey_clearLatentKeys(void);
void              featureKey_clearLatentKey(LatentFeatureKey *key);
/* void              featureKey_storeLatentKeys(void); TODO: Do this later */


#endif /* GLMS_FEATURE_KEY_DATA_H */

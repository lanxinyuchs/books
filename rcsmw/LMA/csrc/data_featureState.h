#ifndef GLMS_FEATURE_STATE_DATA_H
#define GLMS_FEATURE_STATE_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"
#include "lihi/licenseFeatureControlICommon.h"
#include "data_featureKey.h"
#include "glms_main.h"

struct FeatureState_s
{
   
   
   char      featureStateId[GLMS_MO_KEY_LEN];   /* Software Parameter */
   char      name[GLMS_KEY_NAME_LEN];           /* Software Parameter - Used by FeatureKey MO*/
   char      productType[GLMS_PRODUCT_TYPE_LEN];/* Software Parameter - Used by FeatureKey MO*/
   char      description[GLMS_DESCRIPTION_LEN]; /* Software Parameter */
   char      keyId[GLMS_KEY_ID_LEN];            /* Software Parameter */
   char      activeFeatureKeyId[GLMS_MO_KEY_LEN];
   GlmsFeatureState featureState;               /* Software Parameter */
   GlmsLicenseState licenseState;
   GlmsLicenseState licenseStateAccordingToKf;

   /* Internal Data */
   time_t32     markedForDeletion;              /* Software Parameter */
   time_t32     expiryTime;
   GlmsBool     pendingUpdates[2];
   GlmsBool     sendMoUpdateInd;
   GlmsBool     licenseKeyNotAvailableAlarmActive;
   ExpirationAlarmStates licenseKeyExpirationAlarmState;
   uint32_t     lknaAlarmEventId;
   uint32_t     sentAlarmCorrelationEventId;
   uint32_t     tableIndex;

   struct FeatureKey_s     *keyList;
   struct LfciSubscriber_s *lfciSubList;
   struct FeatureState_s   *nextFeatureState;
};
typedef struct FeatureState_s FeatureState;

typedef struct
{
   uint32_t     readIndexesFromTable[2];
   uint32_t     indexesInTable[2];
   uint32_t     *tableIndex[2];
   int32_t      readStatus[2];
   uint32_t     tableIndexPool;
   FeatureState *firstFeatureState;
   FeatureState *lastFeatureState;
} FeatureStateData;

struct LfciSubscriber_s
{
   itc_mbox_id_t   clientMid;
   int32_t         serverRef;
   uint32_t        clientRef;
   struct LfciSubscriber_s   *nextSub;
};
typedef struct LfciSubscriber_s LfciSubscriber;


/*
 * Feature State function prototypes
 *
 */
void featureStateDataInit();
void featureStateDataClear();

void     featureState_handleLicenseKeyCloseToExpirationAlarm(FeatureState *fsElement);
void     featureState_handleLicenseKeyNotAvailableAlarm(FeatureState *fsElement);
void     handleGlmsAdpiDumpFeatureStateDataReq(union itc_msg *sigRec);

void     featureState_storeParameters(FeatureState *fsElement);
void     featureState_storeParametersOfAllFeatureStatesAndKeys();
void     featureState_fetchParameters();
int32_t  featureState_parseStoredParameters(union itc_msg *sig);
void     featureState_sendMoUpdateFeatureStateInd(FeatureState *fsElement);

FeatureState *featureState_getFirst();
FeatureState *featureState_getLast();
FeatureState *featureState_getNext(FeatureState *fsElement);
FeatureState *featureState_getPrevious(FeatureState *fsElement);
FeatureState *featureState_findByFeatureStateId(const char *keyId);
FeatureState *featureState_findByKeyId(const char *keyId);
FeatureState *featureState_findByTableIndex(uint32_t index);
uint32_t      featureState_getNrOfFeatureStateMos();

void          featureState_clearFeatureState(FeatureState *fsElement);
void          featureState_deleteFeatureStateAndMo(FeatureState *fsElement);
FeatureState *featureState_createFeatureState(const char *description,
                                              const char *keyId,
                                              const char *name,
                                              const char *productType,
                                              GlmsFeatureState featureState);

char*            featureState_getFeatureStateId(FeatureState *fsElement);
char*            featureState_getKeyId(FeatureState *fsElement);
char*            featureState_getName(FeatureState *fsElement);
void             featureState_setName(FeatureState *fsElement, const char *newName);
char*            featureState_getProductType(FeatureState *fsElement);
void             featureState_setProductType(FeatureState *fsElement, const char *newProductType);
char*            featureState_getDescription(FeatureState *fsElement);
void             featureState_setDescription(FeatureState *fsElement, const char *newDesc);
time_t32         featureState_getMarkedForDeletion(FeatureState *fsElement);
void             featureState_setMarkedForDeletion(FeatureState *fsElement,
                                                   time_t32 newMarkedForDeletion);
GlmsBool         featureState_isMoDeleteTimeoutReached(FeatureState *fsElement);
GlmsFeatureState featureState_getFeatureState(FeatureState *fsElement);
void             featureState_setFeatureState(FeatureState *fsElement,
                                              GlmsFeatureState newFeatureState);
GlmsLicenseState featureState_getLicenseState(FeatureState *fsElement);
void             featureState_setLicenseState(FeatureState *fsElement,
                                              GlmsLicenseState newLicenseState);
GlmsServiceState featureState_getServiceState(FeatureState *fsElement);

void             featureState_setLicenseStateAccordingToKfForAll();
GlmsLicenseState featureState_getLicenseStateAccordingToKf(FeatureState *fsElement);
void             featureState_setLicenseStateAccordingToKf(FeatureState *fsElement,
                                                           GlmsLicenseState newLicenseState);

void             featureState_resetAllKfParsingMarks(GlmsKeyType keyType);
GlmsBool         featureState_fillLfciLicenseInfoByKeyId(LfciLicenseInfoS *dstLicInfo,
                                                         const char *licenseKeyId);
GlmsBool         featureState_fillLfciLicenseInfo2ByKeyId(LfciLicenseInfo2S *dstLicInfo,
                                                          const char *licenseKeyId);
uint32_t         featureState_getTableIndex(FeatureState *fsElement);
char*            featureState_getActiveFeatureKeyId(FeatureState *fs);
void             featureState_updateActiveFeatureKey(FeatureState *fs, struct FeatureKey_s *fk);

void             featureState_doMoDeletionsAtKfParsing(struct timespec currentTime,
                                                       GlmsKeyType keyType);

size_t           featureState_fillExpirationAlarmAddInfo(char *fillStr);
uint32_t         featureState_getNrOfExpiringKeys(uint32_t *totalActivated);
void             featureState_deleteFeatureKeyMosAndLatentKeys(GlmsKeyType keyType);
uint32_t         featureState_getNrOfFeatureKeysOfType(FeatureState *fs,
                                                       GlmsKeyType keyType);
/*
 * FeatureKey Instance function prototypes
 *
 */
void                 featureState_clearFeatureKeyList(FeatureState *fsElement); 
void                 featureState_setFirstFeatureKey(FeatureState *fsElement, 
                                                     struct FeatureKey_s *fkElement);
struct FeatureKey_s *featureState_getFirstFeatureKey(FeatureState *fsElement);
struct FeatureKey_s *featureState_findFeatureKeyByStartDateExpirationAndKeyType(
                                                       FeatureState *fsElement,
                                                       time_t32 startDate,
                                                       time_t32 expiration,
                                                       GlmsKeyType keyType);
uint32_t             featureState_updateKeyState(FeatureState *fsElement);
uint32_t             featureState_getNrOfFeatureKeyMos();
GlmsBool             featureState_haveAnInstalledLicenseKey(FeatureState *fsElement);
void                 featureState_addFeatureKey(FeatureState *fsElement,
                                                struct FeatureKey_s *fkElement); 

/*
 * Feature Key Subscriber function prototypes
 *
 */
void             featureState_clearSubscriptionsByMidAndServerRef(itc_mbox_id_t mid,
                                                                  int32_t serverRef);
GlmsBool         featureState_clearSubscriptionsByKeyIdMidAndServerRef(char *keyId,
                                                                       itc_mbox_id_t mid,
                                                                       int32_t serverRef);
void             featureState_clearSubscriberList(FeatureState *fsElement);
void             featureState_addSubscriber(const char *keyId,
                                            const char *name,
                                            const char *description,
                                            itc_mbox_id_t clientMid,
                                            int32_t serverRef,
                                            uint32_t clientRef);
LfciSubscriber  *featureState_getFirstSub(FeatureState *fsElement);
LfciSubscriber  *featureState_getNextSub(LfciSubscriber *subscriber);
itc_mbox_id_t    featureState_getSubMid(LfciSubscriber *subscriber);
uint32_t         featureState_getSubClientRef(LfciSubscriber *subscriber);
int32_t          featureState_getSubServerRef(LfciSubscriber *subscriber);
void             featureState_sendLfciChangeIndToAllSubscribers(FeatureState *fsElement,
                                                                uint32_t indForPv);
void             featureState_sendLfciChangeIndToSubscriber(LfciSubscriber *subscriber,
                                                            FeatureState *fk,
                                                            uint32_t indForPv);


#endif /* GLMS_FEATURE_STATE_DATA_H */

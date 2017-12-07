#ifndef GLMS_CAPACITY_STATE_DATA_H
#define GLMS_CAPACITY_STATE_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"
#include "lihi/licenseCapacityControlICommon.h"
#include "data_capacityKey.h"


struct CapacityState_s
{
   char      capacityStateId[GLMS_MO_KEY_LEN];   /* Software Parameter */
   char      name[GLMS_KEY_NAME_LEN];            /* Software Parameter - Used by CapacityKey MO*/
   char      productType[GLMS_PRODUCT_TYPE_LEN]; /* Software Parameter - Used by CapacityKey MO*/
   char      description[GLMS_DESCRIPTION_LEN];  /* Software Parameter */
   char      capacityKeyId[GLMS_KEY_ID_LEN];     /* Software Parameter */   
   char      capacityUnit[GLMS_CAPACITY_UNIT_LEN];/* Software Parameter - Used by CapacityKey MO*/
   char      activeCapacityKeyId[GLMS_MO_KEY_LEN];
   
   GlmsCapacityValue  currentCapacityValue;
   char               currentHwacString[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING];
   GlmsGracePeriod    gracePeriod;
   GlmsLicenseState   licenseState;
   GlmsLicenseState   licenseStateAccordingToKf;
   uint16_t           isGracePeriodControlled;
   GlmsBool           isNotContractuallyLimited;
   GlmsBool           isGracePeriodMoCreated;    /* Software Parameter */
   uint32_t           gracePeriodActivationValue;
   uint32_t           sentGpAvailableState;
   uint32_t           gpTimerId;

   /* Internal Data */
   time_t32     markedForDeletion;               /* Software Parameter */
   time_t32     expiryTime;
   GlmsBool     pendingUpdates[2];
   GlmsBool     sendMoUpdateInd;
   GlmsBool     sendGpMoUpdateInd;
   GlmsBool     licenseKeyNotAvailableAlarmActive;
   ExpirationAlarmStates licenseKeyExpirationAlarmState;
   uint32_t     tableIndex;

   struct CapacityKey_s     *keyList;
   struct LcciSubscriber_s *lcciSubList;
   struct CapacityState_s   *nextCapacityState;
};
typedef struct CapacityState_s CapacityState;

typedef struct
{
   uint32_t     readIndexesFromTable[2];
   uint32_t     indexesInTable[2];
   uint32_t     *tableIndex[2];
   int32_t      readStatus[2];
   uint32_t     tableIndexPool;
   CapacityState *firstCapacityState;
   CapacityState *lastCapacityState;
} CapacityStateData;

struct LcciSubscriber_s
{
   itc_mbox_id_t   clientMid;
   int32_t         serverRef;
   uint32_t        clientRef;
   uint32_t        timerId;
   struct LcciSubscriber_s   *nextSub;
};
typedef struct LcciSubscriber_s LcciSubscriber;


/*
 * Capacity State function prototypes
 *
 */
void capacityStateDataInit();
void capacityStateDataClear();

void             capacityState_handleLicenseKeyNotAvailableAlarm(CapacityState *csElement);
void             handleGlmsAdpiDumpCapacityStateDataReq(union itc_msg *sigRec);
void             capacityState_handleLicenseKeyCloseToExpirationAlarm(CapacityState *csElement);

void             capacityState_storeParameters(CapacityState *csElement);
void             capacityState_storeParametersOfAllCapacityStatesAndKeys();
void             capacityState_fetchParameters();
int32_t          capacityState_parseStoredParameters(union itc_msg *sig);
void             capacityState_sendMoUpdateCapacityStateInd(CapacityState *csElement);
void             capacityState_sendMoUpdateGracePeriodInd(CapacityState *csElement);

CapacityState   *capacityState_getFirst();
CapacityState   *capacityState_getLast();
CapacityState   *capacityState_getNext(CapacityState *csElement);
CapacityState   *capacityState_getPrevious(CapacityState *csElement);
CapacityState   *capacityState_findByCapacityStateId(char *keyId);
CapacityState   *capacityState_findByKeyId(char *keyId);
CapacityState   *capacityState_findByGpTimerId(uint32_t timerId);
void             capacityState_stopGpTimer(CapacityState *cs);
CapacityState   *capacityState_findByTableIndex(uint32_t index);
uint32_t         capacityState_getNrOfCapacityStateMos();

void             capacityState_clearCapacityState(CapacityState *csElement);
void             capacityState_deleteCapacityStateAndMo(CapacityState *csElement);
CapacityState   *capacityState_createCapacityState(char *description,
                                                   char *keyId,
                                                   char *name,
                                                   char *capacityUnit,
                                                   char *productType,
                                                   time_t32 markedForDeletion,
                                                   uint16_t isGracePeriodControlled);

char*            capacityState_getCapacityStateId(CapacityState *csElement);
char*            capacityState_getCapacityKeyId(CapacityState *csElement);
char*            capacityState_getName(CapacityState *csElement);
void             capacityState_setName(CapacityState *csElement, char *newName);
void             capacityState_setcapacityUnit(CapacityState *csElement,
                                               char *newCapacityUnit);
char*            capacityState_getProductType(CapacityState *csElement);
void             capacityState_setProductType(CapacityState *csElement, char *newProductType);
char*            capacityState_getDescription(CapacityState *csElement);
void             capacityState_setDescription(CapacityState *csElement, char *newDesc);
time_t32         capacityState_getMarkedForDeletion(CapacityState *csElement);
void             capacityState_setMarkedForDeletion(CapacityState *csElement,
                                                    time_t32 newMarkedForDeletion);
GlmsBool         capacityState_isMoDeleteTimeoutReached(CapacityState *csElement);
GlmsLicenseState capacityState_getLicenseState(CapacityState *csElement);
void             capacityState_setLicenseState(CapacityState *csElement,
                                               GlmsLicenseState newLicenseState);

char*              capacityState_getCapacityUnit(CapacityState *csElement);
int32_t            capacityState_getGrantedCapacityLevel(CapacityState *csElement);
GlmsBool           capacityState_getLicensedCapacityLimitReached(CapacityState *csElement);
GlmsBool           capacityState_isNotContractuallyLimited(CapacityState *csElement);
GlmsBool           capacityState_isGracePeriodMoCreated(CapacityState *csElement);
void               capacityState_copyGracePeriod(CapacityState *csElement, GlmsGracePeriod *gpDest);
char *             capacityState_getGracePeriodId(CapacityState *csElement);
uint16_t           capacityState_getIsGpControlled(CapacityState *csElement);
uint32_t           capacityState_getGracePeriodAvailable(CapacityState *csElement);
void               capacityState_setGracePeriodActivationValue(CapacityState *csElement, uint32_t value);
uint32_t           capacityState_getGracePeriodActivationValue(CapacityState *csElement);
void               capacityState_setGpConfigurableAttributes(CapacityState *csElement,
                                                             int32_t  activationThreshold,
                                                             int32_t  resetThreshold,
                                                             int32_t length);
GlmsActivationState capacityState_getGracePeriodState(CapacityState *csElement);
GlmsBool           capacityState_isGpActive(CapacityState *csElement);
void               capacityState_setGracePeriodState(CapacityState *csElement,
                                                     GlmsActivationState gpState);
time_t32           capacityState_getGracePeriodExpiration(CapacityState *csElement);
uint32_t           capacityState_getGpConfiguredLength(CapacityState *csElement);
time_t32           capacityState_getGpLength(CapacityState *csElement);
void               capacityState_setGracePeriodExpirationAndState(CapacityState *csElement,
                                                                  time_t32 gpExpiration);
int32_t            capacityState_getGracePeriodResetThreshold(CapacityState *csElement);
int32_t            capacityState_getGracePeriodActivationThreshold(CapacityState *csElement);
int32_t            capacityState_getGpResetCapacityValue(CapacityState *csElement);
GlmsBool           capacityState_activateGp(CapacityState *csElement);
void               capacityState_handleGpWarningTimer(uint32_t timerId);
void               capacityState_handleGpExpiryTimer(uint32_t timerId);
void               capacityState_sendGpAlarmInd(CapacityState *csElement,
                                                GlmsAlarmState alarmState,
                                                GlmsAlarmSeverity alarmSeverit);
void               capacityState_calculateGpState();
void               capacityState_handleGlmsRestart();
GlmsGracePeriod    capacityState_getDefaultGracePeriod();
GlmsCapacityValue  capacityState_getCurrentCapacityValue(CapacityState *csElement);
GlmsCapacityValue  capacityState_getDefaultCapacityValue();

void             capacityState_setLicenseStateAccordingToKfForAll();
GlmsLicenseState capacityState_getLicenseStateAccordingToKf(CapacityState *csElement);
void             capacityState_setLicenseStateAccordingToKf(CapacityState *csElement,
                                                            GlmsLicenseState newLicenseState);

void             capacityState_resetAllKfParsingMarks();
GlmsBool         capacityState_fillLcciLicenseInfoByKeyId(LcciLicenseInfoS *dstLicInfo,
                                                          char *licenseKeyId);
GlmsBool         capacityState_fillLcciLicenseInfo2ByKeyId(LcciLicenseInfo2S *dstLicInfo,
                                                          char *licenseKeyId);
uint32_t         capacityState_getTableIndex(CapacityState *csElement);
char*            capacityState_getActiveCapacityKeyId(CapacityState *cs);
void             capacityState_updateActiveCapacityKey(CapacityState *cs, struct CapacityKey_s *ck);
 
void             capacityState_doMoDeletionsAtKfParsing(struct timespec currentTime);

size_t           capacityState_fillExpirationAlarmAddInfo(char *fillStr);
uint32_t         capacityState_getNrOfExpiringKeys(uint32_t *totalActivated);

/*
 * CapacityKey Instance function prototypes
 *
 */
void                 capacityState_clearCapacityKeyList(CapacityState *csElement); 
void                 capacityState_setFirstCapacityKey(CapacityState *csElement, 
                                                       struct CapacityKey_s *ckElement);
struct CapacityKey_s *capacityState_getFirstCapacityKey(CapacityState *csElement);
struct CapacityKey_s *capacityState_findCapacityKeyByStartDateAndExpirationAndCapacityValue
                                                       (CapacityState *csElement,
                                                        time_t32 startDate,
                                                        time_t32 expiration,
                                                        GlmsCapacityValue glmsCapacityValue,
                                                        char *hwacString);
uint32_t             capacityState_updateKeyState(CapacityState *csElement);
uint32_t             capacityState_getNrOfCapacityKeyMos();
GlmsBool             capacityState_haveAnInstalledLicenseKey(CapacityState *csElement);
void                 capacityState_addCapacityKey(CapacityState *csElement,
                                                  struct CapacityKey_s *ckElement); 

/*
 * Capacity Key Subscriber function prototypes
 *
 */
void             capacityState_clearSubscriptionsByMidAndServerRef(itc_mbox_id_t mid,
                                                                   int32_t serverRef);
GlmsBool         capacityState_clearSubscriptionsByKeyIdMidAndServerRef(char *keyId,
                                                                        itc_mbox_id_t mid,
                                                                        int32_t serverRef);
void             capacityState_clearSubscriberList(CapacityState *csElement);
void             capacityState_addSubscriber(char *keyId,
                                             char *name,
                                             char* capacityUnit,
                                             char *description,
                                             uint16_t isGracePeriodControlled,
                                             itc_mbox_id_t clientMid,
                                             int32_t serverRef,
                                             uint32_t clientRef);
LcciSubscriber  *capacityState_getFirstSub(CapacityState *csElement);
LcciSubscriber  *capacityState_getNextSub(LcciSubscriber *subscriber);
itc_mbox_id_t    capacityState_getSubMid(LcciSubscriber *subscriber);
uint32_t         capacityState_getSubClientRef(LcciSubscriber *subscriber);
int32_t          capacityState_getSubServerRef(LcciSubscriber *subscriber);
void             capacityState_sendLcciChangeIndToAllSubscribers(CapacityState *csElement);
void             capacityState_sendLcciChangeIndToSubscriber(LcciSubscriber *subscriber,
                                                             CapacityState *ck);


#endif /* GLMS_CAPACITY_STATE_DATA_H */

#ifndef GLMS_LM_DATA_H
#define GLMS_LM_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"

typedef struct
{
   char         fingerprint[GLMS_FINGERPRINT_LEN];
   GlmsLmState  lmState;
   time_t32     lastInventoryChange;
   time_t32     lastLicenseInventoryRefresh;
   char         referenceToLicenseServer;

   /* Internal Data */
   GlmsBool     ppPendingUpdates;
   GlmsBool     spPendingUpdates;
   GlmsBool     sendMoUpdateInd;
   int32_t      readStatus[2]; /* read status of stored parameters at startup */
   GlmsFeatureConfigurationData* featureConfList; /* List of supported features, a.k.a "Appdata" */
   uint32_t                      featureConfListLen;
} LmData;

/*
 * License Manager Data function prototypes
 *
 */
void             lmDataInit();
char*            lm_getFingerprint();
void             lm_setFingerprint(char *newFingerprint);
GlmsBool         lm_getFingerprintUpdatable();
GlmsLmState      lm_getLmState();
void             lm_setLmState(GlmsLmState newState);
void             lm_calculateLmState();
time_t32         lm_getLastInventoryChange();
void             lm_setLastInventoryChange(time_t32 lastInventoryChange);
time_t32         lm_getLastInventoryRefresh();
void             lm_setLastInventoryRefresh(time_t32 lastInventoryRefresh);
char             lm_getReferenceToLicenseServer();
void             lm_validateLicenses();
void             lm_storeParameters();
void             lm_fetchParameters();
int32_t          lm_parseStoredParameters(union itc_msg *sig);
void             lm_sendMoUpdateLmInd();
void             lm_setFeatureConfList(const GlmsFeatureConfigurationData* featureConfList,
                                       uint32_t featureConfListLen);
const GlmsFeatureConfigurationData* lm_getFeatureConfData(const char* keyId);

/* Get internal data for dumping */
GlmsBool         lm_getPpPendingUpdates();
GlmsBool         lm_getSpPendingUpdates();
GlmsBool         lm_getSendMoUpdateInd();
int32_t          lm_getStartupReadStatus(uint32_t elem);

#endif /* GLMS_LM_DATA_H */

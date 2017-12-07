#ifndef GLMS_UNLOCKS_DATA_H
#define GLMS_UNLOCKS_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"

typedef struct
{
   GlmsActivationState activationState;
   time_t32  expiration;
   uint32_t  activationsLeft;

   /* Internal Data */
   GlmsBool     ppPendingUpdates;
   GlmsBool     sendMoUpdateInd;
   int32_t      readStatus[2];
   uint32_t     euWarningTimerId;
   uint32_t     euExpiryTimerId;
} EmergencyUnlockState;

typedef struct
{
   GlmsActivationState activationState;
   time_t32  expiration;
   uint32_t  activationsLeft;

   /* Internal Data */
   GlmsBool     ppPendingUpdates;
   GlmsBool     sendMoUpdateInd;
   int32_t      readStatus[2];
   uint32_t     iuWarningTimerId;
   uint32_t     iuExpiryTimerId;
} IntegrationUnlockState;

typedef struct
{
   GlmsActivationState activationState;
   time_t32  expiration;
   uint32_t  activationsLeft;
   uint32_t  puDeactivated;

   /* Internal Data */
   GlmsBool     ppPendingUpdates;
   int32_t      readStatus[2];
   uint32_t     puExpiryTimerId;
} ProductionUnlockState;

typedef struct
{
   time_t32     expiration;

   /* Internal Data */
   GlmsBool     ppPendingUpdates;
   GlmsBool     sendMoUpdateInd;
   int32_t      readStatus[2];
   uint32_t     amExpiryTimerId;
} AutonomousModeData;


/*
 * Function prototypes for EU data.
 *
 */
void                 euDataInit();
GlmsActivationState  eu_getActivationState();
void                 eu_setActivationState(GlmsActivationState newState);
time_t32             eu_getExpiration();
void                 eu_setExpiration(time_t32 newExpiration);
uint32_t             eu_getActivationsLeft();
void                 eu_setActivationsLeft(uint32_t newActivationsLeft);
GlmsBool             eu_isEuActive();
void                 eu_storeParameters();
void                 eu_fetchParameters();
int32_t              eu_parseStoredParameters(union itc_msg *sig);
void                 eu_sendMoUpdateEuInd();
GlmsBool             eu_activateEu();
void                 eu_handleEuExpiryWarningTimer();
void                 eu_handleEuExpiryTimer();
void                 eu_sendEuAlarmInd(GlmsAlarmState alarmState,
                                       GlmsAlarmSeverity alarmSeverity);
void                 eu_calculateActivationState();
void                 eu_handleGlmsRestart();
void                 eu_reset();

/* Get internal data for dumping */
GlmsBool             eu_getPpPendingUpdates();
GlmsBool             eu_getSendMoUpdateInd();
int32_t              eu_getStartupReadStatus(uint32_t elem);
uint32_t             eu_getWarningTimerId();
uint32_t             eu_getExpiryTimerId();




/*
 * Function prototypes for IU data.
 *
 */
void                 iuDataInit();
GlmsActivationState  iu_getActivationState();
void                 iu_setActivationState(GlmsActivationState newState);
time_t32             iu_getExpiration();
void                 iu_setExpiration(time_t32 newExpiration);
uint32_t             iu_getActivationsLeft();
void                 iu_setActivationsLeft(uint32_t newActivationsLeft);
GlmsBool             iu_isIuActive();
void                 iu_storeParameters();
void                 iu_fetchParameters();
int32_t              iu_parseStoredParameters(union itc_msg *sig);
void                 iu_sendMoUpdateIuInd();
GlmsBool             iu_activateIu();
void                 iu_handleIuExpiryWarningTimer();
void                 iu_handleIuExpiryTimer();
void                 iu_calculateActivationState();
void                 iu_handleGlmsRestart();
void                 iu_reset();

/* Get internal data for dumping */
GlmsBool             iu_getPpPendingUpdates();
GlmsBool             iu_getSendMoUpdateInd();
int32_t              iu_getStartupReadStatus(uint32_t elem);
uint32_t             iu_getWarningTimerId();
uint32_t             iu_getExpiryTimerId();



/*
 * Function prototypes for PU data.
 *
 */
void                 puDataInit();
GlmsActivationState  pu_getActivationState();
void                 pu_setActivationState(GlmsActivationState newState);
time_t32             pu_getExpiration();
void                 pu_setExpiration(time_t32 newExpiration);
uint32_t             pu_getActivationsLeft();
void                 pu_setActivationsLeft(uint32_t newActivationsLeft);
uint32_t             pu_getPuDeactivated();
void                 pu_setPuDeactivated(uint32_t newPuDeactivated);
GlmsBool             pu_isPuActive();
void                 pu_storeParameters();
void                 pu_fetchParameters();
int32_t              pu_parseStoredParameters(union itc_msg *sig);
GlmsBool             pu_activatePu();
GlmsBool             pu_deactivatePu();
void                 pu_handlePuExpiryWarningTimer();
void                 pu_handlePuExpiryTimer();
void                 pu_calculateActivationState();
void                 pu_handleGlmsRestart();
void                 pu_reset();

/* Get internal data for dumping */
GlmsBool             pu_getPpPendingUpdates();
int32_t              pu_getStartupReadStatus(uint32_t elem);
uint32_t             pu_getExpiryTimerId();



/*
 * Function prototypes for AM data.
 *
 */
void                 amDataInit();
GlmsActivationState  am_getActivationState();
void                 am_activateAutonomousMode();
void                 am_ceaseAutonomousMode();
void                 am_sendMoUpdateEuInd();
void                 am_storeParameters();
void                 am_fetchParameters();
int32_t              am_parseStoredParameters(union itc_msg *sig);
void                 am_handleAmExpiryTimer();
void                 am_handleGlmsRestart();
void                 am_sendAmAlarmInd(GlmsAlarmState alarmState);

/* Get internal data for dumping */
time_t32             am_getExpiration();
GlmsActivationState  am_getActivationState();
GlmsBool             am_getPpPendingUpdates();
GlmsBool             am_getSendMoUpdateInd();
int32_t              am_getStartupReadStatus(uint32_t elem);
uint32_t             am_getExpiryTimerId();



#endif /* GLMS_UNLOCKS_DATA_H */

#ifndef GLMS_MAIN_H
#define GLMS_MAIN_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"
#include "timetFix.h"


#define   GLMS_PENDING_MO_DELETE_TIME            (60*24*60*60)
#define   GLMS_LICENSEKEY_EXPIRATION_ALARM_TIME  (14*24*60*60)
#define   GLMS_MAX_DUMPSTR_LEN                   (2000)


extern uint32_t  GlmsDumpSize[]; /* Sizes are set in glms_main.c */
#define   GLMS_LAST_DUMP_SIZE_ELEMENT  (4) /* The number of elements in
                                              GlmsDumpSize minus 1 */

typedef enum  expirationAlarmStates
{
   ALARM_CEASED_PENDING,
   ALARM_CEASED,
   ALARM_ACTIVE_PENDING,
   ALARM_ACTIVE
} ExpirationAlarmStates;

typedef enum adpiActivationStates
{
   ADPI_INITIATING,
   ADPI_ACTIVATED,
   ADPI_DEACTIVATED
} AdpiActivationState;

typedef struct
{
   itc_mbox_id_t       adapterMid;
   itc_monitor_id_t    monitorId;
   GlmsBool            isLicenseRefreshRunning;
   GlmsBool            isSubscribedForMoUpdateInd;
   time_t32            timeScaling;
   time_t32            warningPeriod;
   GlmsLogLevel        logLevel;
   char                *restrictedLicenses;
   AdpiActivationState adpiActivationState;
} AdapterData;

void           adapter_init(void);
void           adapter_setMid(itc_mbox_id_t newMid);
itc_mbox_id_t  adapter_getMid(void);
void           adapter_subscribeForMoUpdateInd(void);
GlmsBool       adapter_isSubscribedForMoUpdateInd(void);
void           adapter_setLogLevel(GlmsLogLevel logLevel);
GlmsLogLevel   adapter_getLogLevel(void);
char *         adapter_getRestrictedLicenses(void);
void           sendMsgToAdapter(union itc_msg *sig);
void           handleLicenseKeyCloseToExpirationAlarm(void);

time_t32       glms_getTimeScaling(void);
time_t32       glms_getWarningPeriod(void);

void                   adapter_setActivationState(AdpiActivationState adpiState);
AdpiActivationState    adapter_getActivationState(void);
GlmsBool               adapter_isAdpiActivated(void);
GlmsBool               adapter_isLicenseRefreshRunning(void);
void                   adapter_startLicenseRefresh(void);
void                   adapter_sendRspAndEndLicenseRefresh(GlmsResult result);

uint32_t               isLihiPublished(void);

#endif /* GLMS_MAIN_H */

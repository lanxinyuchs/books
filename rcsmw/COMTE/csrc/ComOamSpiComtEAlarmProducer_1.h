#ifndef ComOamSpiComtEEventProducer_1_h
#define ComOamSpiComtEEventProducer_1_h

#include <MafOamSpiEvent_1.h>
#include "bert.h"

#define ALARM_PRODUCER "alarm_producer"
#define NSEC 1000000000

#define POPULATE_ALARM_DEF(alarm, params)           \
    alarm->dn = params->dn;                         \
    alarm->majorType = params->majorType;           \
    alarm->minorType = params->minorType;           \
    struct timespec t_spec;                         \
    clock_gettime(CLOCK_REALTIME, &t_spec);                             \
    alarm->eventTime = ((uint64_t)t_spec.tv_sec * NSEC)+t_spec.tv_nsec; \
    alarm->severity = params->severity;             \
    alarm->additionalText = params->addText;

#define FREE_ALARM_DEF(alarm) \
    if(alarm->additionalText)                                    \
        comte_free(alarm->additionalText);                       \
    comte_free(alarm->dn);                                       \
    comte_free(alarm);

#define DECODE_ALARM_OPTS_DEF(bert_alarm_opts, params)          \
    decode_alarm_severity(bert_alarm_opts->value, ap);          \
    decode_alarm_add_text(bert_alarm_opts->next->value, ap);


#ifndef COM_FM_H
#define COM_FM_H 2
#endif

/* --------------------------------------------------------------- */
/* FM 4                                                            */
/* --------------------------------------------------------------- */
#if COM_FM_H >= 4
#include <MafOamSpiNotificationFm_4.h>

#define ComtENotificationFmEventType MafOamSpiNotificationFmEventType_4
typedef MafOamSpiNotificationFmStruct_4T ComtENotificationFmStructT;
typedef MafOamSpiFmAdditionalInfoContainerT ComtENotificationFmAddInfoContT;

#define FREE_ALARM(alarm)                                               \
    int i = 0;                                                          \
    for(i = 0; i < alarm->additionalInfo.size; i++){                    \
        comte_free(alarm->additionalInfo.additionalInfoArr[i].name);    \
        comte_free(alarm->additionalInfo.additionalInfoArr[i].value);   \
    }                                                                   \
    if(alarm->additionalInfo.size > 0)                                  \
        comte_free(alarm->additionalInfo.additionalInfoArr);            \
    FREE_ALARM_DEF(alarm);

#define POPULATE_ALARM(alarm, params)                   \
    POPULATE_ALARM_DEF(alarm, params);                  \
    alarm->additionalInfo = params->addInfo;

#define DECODE_ALARM_OPTS(bert_alarm_opts, params)                      \
    DECODE_ALARM_OPTS_DEF(bert_alarm_opts, params);                     \
    decode_alarm_add_info(bert_alarm_opts->next->next->value, params);

#else
/* --------------------------------------------------------------- */
/* FM 2                                                            */
/* --------------------------------------------------------------- */
#include <MafOamSpiNotificationFm_2.h>

#define ComtENotificationFmEventType MafOamSpiNotificationFmEventType_2
typedef MafOamSpiNotificationFmStruct_2T ComtENotificationFmStructT;
typedef void* ComtENotificationFmAddInfoContT;

#define FREE_ALARM(alarm)                       \
    FREE_ALARM_DEF(alarm);

#define POPULATE_ALARM(alarm, params)           \
    POPULATE_ALARM_DEF(alarm, params);

#define DECODE_ALARM_OPTS(bert_alarm_opts, params)      \
    DECODE_ALARM_OPTS_DEF(bert_alarm_opts, params);

#endif



typedef struct fm_alarm_params {
    char *dn;
    uint32_t majorType;
    uint32_t minorType;
    MafOamSpiNotificationFmSeverityT severity;
    char *addText;
    /* Only applicable in COM 4.0+ */
    ComtENotificationFmAddInfoContT addInfo;

} fm_alarm_params_t;




typedef struct comte_alarm_producer comte_alarm_producer_t;

#include "ComOamSpiComtEComponent_1.h"

struct comte_alarm_producer {
    MafOamSpiEventProducer_1T base;
    MafOamSpiEventProducerHandleT handle;
    MafOamSpiEventRouter_1T* router;
};

MafReturnT comte_alarm_producer_create(comte_oam_component_t* component);
MafReturnT comte_alarm_producer_destroy(comte_oam_component_t* component);
MafReturnT comte_register_alarm_producer();

MafReturnT comte_notify_alarm(MafOamSpiEventConsumerHandleT consumerId,
                              const char* eventType,
                              MafNameValuePairT** filterAddr,
                              fm_alarm_params_t *ap);

void decode_alarm_add_info(bert_term_t* add_info,
                           fm_alarm_params_t *ap);

MafReturnT populate_alarm_params(fm_alarm_params_t *ap, bert_list_t* bert_alarm_args);



#endif

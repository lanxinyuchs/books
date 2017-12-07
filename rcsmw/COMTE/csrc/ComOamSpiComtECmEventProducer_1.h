#ifndef ComOamSpiComtECmEventProducer_1_h
#define ComOamSpiComtECmEventProducer_1_h

#include <MafOamSpiServiceIdentities_1.h>
#include <MafOamSpiManagedObject_3.h>
#include <MafOamSpiEvent_1.h>
#include <MafOamSpiCmEvent_1.h>

typedef struct comte_cm_event_producer comte_cm_event_producer_t;

#include "ComOamSpiComtEConverter_1.h"
#include "ComtEUtils_1.h"
#include "ComOamSpiComtEUtils_1.h"
#include "bert.h"

#define CM_EVENT_PRODUCER "cm_event_producer"

struct comte_cm_event_producer {
    MafOamSpiEventProducer_1T base;
    MafOamSpiEventProducerHandleT handle;
    MafOamSpiEventRouter_1T* router;
};

MafReturnT comte_cm_event_producer_create(comte_oam_component_t* component);
MafReturnT comte_cm_event_producer_destroy(comte_oam_component_t* component);

MafReturnT comte_register_cm_event_producer();
MafOamSpiCmEvent_Notification_1T* create_cm_notification(bert_list_t* arg_list);
void free_cm_notification(MafOamSpiCmEvent_Notification_1T* cm_ntf);
MafReturnT comte_notify_cm(MafOamSpiEventConsumerHandleT consumerId,
                           const char* eventType,
                           MafNameValuePairT** filterAddr,
                           MafOamSpiCmEvent_Notification_1T* cm_ntf);

#endif


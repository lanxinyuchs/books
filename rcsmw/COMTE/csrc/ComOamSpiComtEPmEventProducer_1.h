#ifndef ComOamSpiComtEPmEventProducer_1_h
#define ComOamSpiComtEPmEventProducer_1_h

#include <MafOamSpiServiceIdentities_1.h>
#include <MafOamSpiManagedObject_3.h>
#include <MafOamSpiEvent_1.h>
#include <ComOamSpiPm_2.h>

typedef struct comte_pm_event_producer comte_pm_event_producer_t;

#include "ComOamSpiComtEComponent_1.h"
#include "ComOamSpiComtEConverter_1.h"
#include "ComtEUtils_1.h"
#include "ComOamSpiComtEUtils_1.h"
#include "bert.h"

#define PM_EVENT_PRODUCER "pm_event_producer"

struct comte_pm_event_producer {
    MafOamSpiEventProducer_1T base;
    MafOamSpiEventProducerHandleT handle;
    MafOamSpiEventRouter_1T* router;
};

MafReturnT comte_pm_event_producer_create(comte_oam_component_t* component);
MafReturnT comte_pm_event_producer_destroy(comte_oam_component_t* component);
void free_pm_notification(ComOamSpiPmEventValue_2T* pm_ntf);
MafReturnT comte_register_pm_event_producer();
MafReturnT comte_notify_pm(MafOamSpiEventConsumerHandleT consumerId,
                           const char* eventType,
                           MafNameValuePairT** filterAddr,
                           ComOamSpiPmEventValue_2T* pm_ntf);

#endif


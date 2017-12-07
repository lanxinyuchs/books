#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ComOamSpiComtEPmEventProducer_1.h"

comte_oam_component_t* oam_comp;

MafReturnT comte_notify_pm(MafOamSpiEventConsumerHandleT consumerId,
                           const char* eventType,
                           MafNameValuePairT** filterAddr,
                           ComOamSpiPmEventValue_2T* pm_ntf) {
    ENTER();
    MafReturnT ntf_res = MafOk;

    if (oam_comp->pm_event_producer->router == NULL) {
    ERROR("Could not send PM event before registering producer!");
    return MafFailure;
    }

    /* Notify consumer */
    ntf_res = oam_comp->pm_event_producer->router->notify(
        oam_comp->pm_event_producer->handle,
        consumerId,
        eventType,
        filterAddr,
        pm_ntf);


    if(ntf_res == MafOk){
        INFO("Notified consumer %lu", consumerId);
    } else {
        ERROR("Could not notify consumer %lu", consumerId);
    }

    LEAVE();
    return ntf_res;

}


void free_pm_notification(ComOamSpiPmEventValue_2T* pm_ntf){
    ENTER();
    comte_free(pm_ntf->jobId);
    comte_free(pm_ntf);
    LEAVE();
}


 /**
  * addFilter is called by the event router whenever the producer needs
  * to be updated about what events are subscribed for.
  *
  * @param consumerId [in] identifier of the consumer
  *
  * @param eventType [in] the type of event.
  *
  * @param filter [in] the filter specifies in more detail what it is the
  *        consumer will be notified for filter is a null terminated array
  *        and must always have a value even if it contains an empty array.
  *        The consumer must ensure that the instance of the filter
  *        exists during the time that the consumer is
  *        subscribing for the filtered events.
  *
  * @return MafOk, or @n
  * MafInvalidArgument if a parameter is not valid, or @n
  * MafFailure if an internal error occurred.
  */
static MafReturnT addFilter(MafOamSpiEventConsumerHandleT consumerId,
                     const char * eventType, MafNameValuePairT ** filter){
    ENTER();

    if( eventType == NULL ) {
        ERROR( "Could not add filter with an eventType that is NULL!" );
        return MafInvalidArgument;
    }

    // filter can not be empty
    if( filter == NULL ) {
        ERROR( "Could not add filter if filter is NULL!" );
        return MafInvalidArgument;
    }

    // Com pm event producer actually didnt define any filter 
    INFO("Adding event type %s from consumer %lu",
         eventType, consumerId);
    INFO("Filter addr: %p", filter);

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_registerEventConsumer(PM_EVENT_PRODUCER,
                                                  consumerId,
                                                  eventType,
                                                  filter,
                                                  buff),
                     decode_registerEventConsumer(buff));

}


/**
 * removeFilter is called by the event service whenever the producer
 * needs to be updated about events that are no longer subscribed for
 *
 * @param consumerId [in] consumerId identifier of the consumer
 *
 * @param eventType [in] the type of event
 *
 * @param filter [in] the filter instance that was previously added.
 *
 * @return MafOk, or @n
 * MafInvalidArgument if a parameter is not valid, or @n
 * MafFailure if an internal error occurred.
 */

static MafReturnT removeFilter(MafOamSpiEventConsumerHandleT consumerId,
        const char * eventType,
        MafNameValuePairT ** filter){
    ENTER();

    if( eventType == NULL ) {
        ERROR( "Could not remove filter with an eventType that is NULL!" );
        return MafInvalidArgument;
    }

    // filter can not be empty
    if( filter == NULL ) {
        ERROR( "Could not remove filter if filter is NULL!" );
        return MafInvalidArgument;
    }

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_unregisterEventConsumer(PM_EVENT_PRODUCER,
                                                    consumerId,
                                                    eventType,
                                                    buff),
                     decode_unregisterEventConsumer(buff));

}


/**
 * clearAll can be called by the Event Router to restart from a known state.
 * When called the producer will discard all filters.
 *
 * @return MafOk, or @n
 * MafInvalidArgument if a parameter is not valid, or @n
 * MafFailure if an internal error occurred.
 */
static MafReturnT clearAll(){
    ENTER();

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_unregisterAllEventConsumers(PM_EVENT_PRODUCER,
                                                        buff),
                     decode_unregisterAllEventConsumers(buff));

}


/**
 * doneWithValue, Event router calls this function
 * to notify the Producer that the value is not needed anymore.
 * The producer will know how to delete the value.
 *
 * @param eventType [in] the type of event. It will help the
 *                  producer how to cast the value before deleting it.
 *
 * @param value [in] the value that is no longer needed.
 *
 * @return MafOk, or @n
 * MafInvalidArgument if a parameter is not valid, or @n
 * MafFailure if an internal error occurred.
 */
static MafReturnT doneWithValue(const char * eventType, void * value){
    ENTER();
    if (strncmp(eventType, ComOamSpiPmEventTypeGpReady_2,
            strlen(ComOamSpiPmEventTypeGpReady_2)) == 0){
        ComOamSpiPmEventValue_2T* pm_ntf =
             (ComOamSpiPmEventValue_2T*) value;
        INFO("Free notification!");
        free_pm_notification(pm_ntf);
    }

    /* Clean up eventType */
    char* et = (char*) eventType;
    comte_free(et);

    LEAVE();
    return MafOk;
}


MafReturnT comte_pm_event_producer_create(comte_oam_component_t* comp){

    oam_comp = comp;

    oam_comp->pm_event_producer =
        comte_malloc(sizeof(comte_pm_event_producer_t));
    comte_pm_event_producer_t* cep = comp->pm_event_producer;

    cep->base.addFilter = addFilter;
    cep->base.clearAll = clearAll;
    cep->base.doneWithValue = doneWithValue;
    cep->base.removeFilter = removeFilter;
    cep->router = NULL;

    return MafOk;
}

MafReturnT comte_pm_event_producer_destroy(comte_oam_component_t* comp){
    comte_free(comp->pm_event_producer);
    return MafOk;
}

MafReturnT comte_register_pm_event_producer(){
    ENTER();

    MafReturnT res = MafOk;

        /* Get event interface */
    res =
            oam_comp->portal->getInterface(
                MafOamSpiEventService_1Id,
                (MafMgmtSpiInterface_1T**) &oam_comp->pm_event_producer->router);
    if (res != MafOk)
        return res;

        /* Register producer */
    res =
            oam_comp->pm_event_producer->router->registerProducer(
                &oam_comp->pm_event_producer->base,
                &oam_comp->pm_event_producer->handle);
        if (res != MafOk){
            ERROR("Could not register as event producer, res: %d", res);
            return res;
        }

        INFO("Registered pm event handle: %lu",
             oam_comp->pm_event_producer->handle);

        /* Add type of events the producer will send*/
    res =
            oam_comp->pm_event_producer->router->addProducerEvent(
                oam_comp->pm_event_producer->handle,
                ComOamSpiPmEventTypeGpReady_2);
        if (res != MafOk)
            return res;


    LEAVE();
    return res;
}

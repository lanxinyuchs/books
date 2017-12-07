#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ComOamSpiComtECmEventProducer_1.h"

comte_oam_component_t* oam_comp;

/* Forward declarations */
MafReturnT insert_event(MafOamSpiCmEvent_1T* cmEvent,
                         bert_stuple_t* transEvent);

MafReturnT comte_notify_cm(MafOamSpiEventConsumerHandleT consumerId,
                           const char* eventType,
                           MafNameValuePairT** filterAddr,
                           MafOamSpiCmEvent_Notification_1T* cm_ntf) {
    ENTER();
    MafReturnT ntf_res = MafOk;

    if (oam_comp->cm_event_producer->router == NULL) {
	ERROR("Could not send CM event before registering producer!");
	return MafFailure;
    }

    /* Notify consumer */
    ntf_res = oam_comp->cm_event_producer->router->notify(
        oam_comp->cm_event_producer->handle,
        consumerId,
        eventType,
        filterAddr,
        cm_ntf);


    if(ntf_res == MafOk){
        INFO("Notified consumer %lu", consumerId);
    } else {
        ERROR("Could not notify consumer %lu", consumerId);
    }

    LEAVE();
    return ntf_res;

}


MafOamSpiCmEvent_Notification_1T* create_cm_notification(bert_list_t* arg_list){

    MafReturnT res = MafOk;
    int i, numEvents;
    bert_list_t* event_list;
    bert_stuple_t curr_event;
    MafOamSpiCmEvent_Notification_1T* cm_ntf;

    cm_ntf = (MafOamSpiCmEvent_Notification_1T*)
        comte_malloc(sizeof(MafOamSpiCmEvent_Notification_1T));

    /* Trans id */
    switch (arg_list->value->type){
    case BERT_SMALL_INTEGER:
        cm_ntf->txHandle = arg_list->value->sint;
        break;
    case BERT_INTEGER:
        cm_ntf->txHandle = arg_list->value->lint;
        break;
    case BERT_SMALL_BIG:
        cm_ntf->txHandle = arg_list->value->sbig;
        break;
    case BERT_UINT64:
        cm_ntf->txHandle = arg_list->value->uint64;
        break;
    default:
        ERROR("Bad encoding of transaction id: %d", arg_list->next->next->value->type);
        comte_free(cm_ntf);
        return NULL;
    }

    /* eventTime */
    struct timespec t_spec;
    clock_gettime(CLOCK_REALTIME, &t_spec);
    cm_ntf->eventTime = ((uint64_t)t_spec.tv_sec * 1000000000) + t_spec.tv_nsec;

    /* sourceIndicator*/
    cm_ntf->sourceIndicator  = arg_list->next->value->sint;

    /* Event list, at least one */
    event_list = arg_list->next->next->value->list;
    numEvents = bert_list_length(event_list);
    curr_event = event_list->value->stuple;

    /* NULL term array of event pointers */
    cm_ntf->events =
        comte_malloc(sizeof(MafOamSpiCmEvent_1T*) * (numEvents+1) );
    cm_ntf->events[numEvents] = NULL;

    /* Create events */
    for(i = 0; i < numEvents; i++){

        /* Alloc event struct*/
        cm_ntf->events[i] = comte_malloc(sizeof(MafOamSpiCmEvent_1T));

        /* Insert event data */
        res = insert_event(cm_ntf->events[i], &curr_event);
        /* Return if bad event */
        if(res != MafOk){
            ERROR("ERROR at event insertion(%i/%i)", i, numEvents);
            break;
        }

        /* Move to next event */
        if(event_list->next != NULL){
            event_list = event_list->next;
            curr_event = event_list->value->stuple;
        }
    }

    if(res != MafOk){
        ERROR("Creation of notification failed: %i", res);
        free_cm_notification(cm_ntf);
        return NULL;
    }

    return cm_ntf;
}



MafReturnT insert_event(MafOamSpiCmEvent_1T* ev,
                        bert_stuple_t* transEvent){
    MafReturnT res = MafOk;
    int numAttrs = 0;
    bert_binary_t bert_dn = transEvent->values[1]->binary;
    uint8_t event_type = transEvent->values[2]->sint;
    bert_term_t* bert_attr_values = transEvent->values[3];



    /* MO distinguished name in 3GPP format */
    char *dn= comte_malloc(sizeof(char) * (bert_dn.length + 1));
    memcpy(dn, bert_dn.value, bert_dn.length);
    dn[bert_dn.length] = 0;
    ev->dn = dn;
    INFO("DN: %s", ev->dn);

    /* Event type */
    ev->eventType = event_type;
    INFO("EventType: %i", ev->eventType);

    switch(event_type){
    case MafOamSpiCmEvent_MoDeleted_1:
    case MafOamSpiCmEvent_Overflow_1:
        ev->attributes = NULL;
        break;
    default:
        /* Created or AVC */
        if(bert_attr_values->type == BERT_NIL){
            /* Only objectCreated */
            numAttrs = 0;
            ev->attributes = NULL;
        }
        else {
            numAttrs = bert_list_length(bert_attr_values->list);
            /* Alloc for pointers to named attributes */
            ev->attributes = (MafMoNamedAttributeValueContainer_3T**)
                comte_malloc(sizeof(MafMoNamedAttributeValueContainer_3T*) * (numAttrs+1));
            ev->attributes[numAttrs] = NULL;

            /* Decode attributes */
            res = decode_moNamedAttributes_term(bert_attr_values,
                                                ev->attributes);
            if(res != MafOk){
                ERROR("Returned error: %i", res);
		break;
            }
        }

        INFO("Insert event with type %i - num attrs %i", event_type, numAttrs);
        break;
    }

    return res;
}

void free_cm_notification(MafOamSpiCmEvent_Notification_1T* cm_ntf){
    ENTER();
    int i = 0, j;
    char *param;

    /* Cleanup events */
    while(cm_ntf->events[i] != NULL){
        /* Event - Free DN */
        param = (char *)cm_ntf->events[i]->dn;
        comte_free(param);
        /* Event - Free attributes */
        if(cm_ntf->events[i]->attributes != NULL){
            j = 0;
            while(cm_ntf->events[i]->attributes[j] != NULL){
                destroy_navc(cm_ntf->events[i]->attributes[j]);
                j++;
            }
        }
        comte_free(cm_ntf->events[i]->attributes);
        /* Free event - MafOamSpiCmEvent_1T */
        comte_free(cm_ntf->events[i]);
        i++;
    }
    comte_free(cm_ntf->events);
    comte_free(cm_ntf);
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
MafReturnT addFilter(MafOamSpiEventConsumerHandleT consumerId,
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

    // at least one filter must be defined
    if( filter[0] == NULL ) {
        ERROR( "Could not add filter if no filter is defined!" );
        return MafInvalidArgument;
    }

    // the filter must be named
    if( filter[0]->name == NULL ) {
        ERROR( "Could not add filter, the filter must be named!" );
        return MafInvalidArgument;
    }

    INFO("Adding event type %s from consumer %lu",
         eventType, consumerId);

    int i = 0;
    while(filter[i] != NULL){
        INFO("FilterType: %s", filter[i]->name);
        INFO("FilterValue: %s", filter[i]->value);
        i++;
    }

    INFO("Filter addr: %p", filter);

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_registerEventConsumer(CM_EVENT_PRODUCER,
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

MafReturnT removeFilter(MafOamSpiEventConsumerHandleT consumerId,
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

    // at least one filter must be defined
    if( filter[0] == NULL ) {
        ERROR( "Could not remove filter if no filter is defined!" );
        return MafInvalidArgument;
    }

    // the filter must be named
    if( filter[0]->name == NULL ) {
        ERROR( "Could not remove filter, the filter must be named!" );
        return MafInvalidArgument;
    }

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_unregisterEventConsumer(CM_EVENT_PRODUCER,
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
MafReturnT clearAll(){
    ENTER();

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_unregisterAllEventConsumers(CM_EVENT_PRODUCER,
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
MafReturnT doneWithValue(const char * eventType, void * value){
    ENTER();
    if (strncmp(eventType, MafOamSpiCmEvent_Notification_1, 31) == 0){
        MafOamSpiCmEvent_Notification_1T* cm_ntf =
             (MafOamSpiCmEvent_Notification_1T*) value;
        INFO("Free notification!");
        free_cm_notification(cm_ntf);
    }

    /* Clean up eventType */
    char* et = (char*) eventType;
    comte_free(et);

    LEAVE();
    return MafOk;
}



MafReturnT comte_cm_event_producer_create(comte_oam_component_t* comp){

    oam_comp = comp;

    oam_comp->cm_event_producer =
        comte_malloc(sizeof(comte_cm_event_producer_t));
    comte_cm_event_producer_t* cep = comp->cm_event_producer;

    cep->base.addFilter = addFilter;
    cep->base.clearAll = clearAll;
    cep->base.doneWithValue = doneWithValue;
    cep->base.removeFilter = removeFilter;
    cep->router = NULL;

    return MafOk;
}

MafReturnT comte_cm_event_producer_destroy(comte_oam_component_t* comp){
    comte_free(comp->cm_event_producer);
    return MafOk;
}

MafReturnT comte_register_cm_event_producer(){
	ENTER();

	MafReturnT res = MafOk;

        /* Get event interface */
	res =
            oam_comp->portal->getInterface(
                MafOamSpiEventService_1Id,
                (MafMgmtSpiInterface_1T**) &oam_comp->cm_event_producer->router);
	if (res != MafOk)
		return res;

        /* Register producer */
	res =
            oam_comp->cm_event_producer->router->registerProducer(
                &oam_comp->cm_event_producer->base,
                &oam_comp->cm_event_producer->handle);
        if (res != MafOk){
            ERROR("Could not register as event producer, res: %d", res);
            return res;
        }

        INFO("Registered cm event handle: %lu",
             oam_comp->cm_event_producer->handle);

        /* Add type of events the producer will send*/
	res =
            oam_comp->cm_event_producer->router->addProducerEvent(
                oam_comp->cm_event_producer->handle,
                MafOamSpiCmEvent_Notification_1);
        if (res != MafOk)
            return res;


	LEAVE();
	return res;
}


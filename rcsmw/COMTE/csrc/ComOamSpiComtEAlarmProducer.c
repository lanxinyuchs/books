/**
 * @author Lukas Larsson
 * @created 2011-05-23
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <pthread.h>
#include <MafOamSpiServiceIdentities_1.h>

#include "ComOamSpiComtEAlarmProducer_1.h"

/* Globals */
comte_oam_component_t* ap_component;

#if COM_FM_H >= 4
void decode_alarm_add_info(bert_term_t* add_info,
                           fm_alarm_params_t *ap){

    ap->addInfo.additionalInfoArr = NULL;
    ap->addInfo.size = 0;

    switch(add_info->type){
    case BERT_NIL: {
        INFO("No additionalInfo");
        break;
    }

    case BERT_LIST: {
        bert_list_t* info_list = add_info->list;
        uint32_t numInfo = bert_list_length(info_list);

        MafOamSpiNotificationFmAdditionalInfoT* addInfoArr =
            comte_malloc(sizeof(MafOamSpiNotificationFmAdditionalInfoT) *
                         (numInfo+1));
	if (addInfoArr == NULL) {
	    ERROR("comte_malloc for additionalInfo failed");
	    return;
	}
        memset(addInfoArr, 0, numInfo+1);

        MafOamSpiNotificationFmAdditionalInfoT* addInfoArrPtr =
            addInfoArr;

        for(info_list=add_info->list;
            info_list != NULL; info_list = info_list->next){
            bert_stuple_t* info_tuple =
                &info_list->value->stuple;

            addInfoArrPtr->name =
                copy_bert_binary(&info_tuple->values[0]->binary);
            addInfoArrPtr->value =
                copy_bert_binary(&info_tuple->values[1]->binary);

            addInfoArrPtr++;
        }

        ap->addInfo.additionalInfoArr = addInfoArr;
        ap->addInfo.size = numInfo;

        break;
    }
    default:
        ERROR("Bad additionalInfo type: %d", add_info->type);
    } // End switch
}
#endif


void decode_alarm_severity(bert_term_t* severity,
                           fm_alarm_params_t *ap){

    switch(severity->type){
    case BERT_NIL:
        ap->severity =
            MafOamSpiNotificationFmSeverityIndeterminate;
        break;
    case BERT_SMALL_INTEGER:
        ap->severity = (MafOamSpiNotificationFmSeverityT)
            severity->sint;
        break;
    default:
        ERROR("Bad severity type: %d", severity->type);
        ap->severity =
            MafOamSpiNotificationFmSeverityIndeterminate;
    }
}

void decode_alarm_add_text(bert_term_t* add_text,
                           fm_alarm_params_t *ap){

    switch(add_text->type){
    case BERT_BINARY:
        ap->addText = copy_bert_binary(&add_text->binary);
        break;
    case BERT_NIL:
        ap->addText = NULL;
        break;
    default: {
        ERROR("Bad additionalText type: %d", add_text->type);
        ap->addText = NULL;
    }
    }
}


static MafReturnT decode_alarm_opts
(bert_term_t* alarm_opts, fm_alarm_params_t *ap) {
    MafReturnT res = MafOk;

    switch(alarm_opts->type){
    case BERT_NIL: {
        /* No opts */
        break;
    }
    case BERT_LIST: {
        /* [Severity, AddText, [AddInfoTuple]] */
        DECODE_ALARM_OPTS(alarm_opts->list, ap);
        break;
    }
    default:
        ERROR("Bad opts type: %d", alarm_opts->type);
        res = MafFailure;
    }

    return res;
}

MafReturnT populate_alarm_params
(fm_alarm_params_t *ap, bert_list_t* bert_alarm_params) {
    MafReturnT res = MafFailure;

    bert_binary_t* bert_dn = &bert_alarm_params->value->binary;
    bert_term_t* bert_majorType = bert_alarm_params->next->value;
    bert_term_t* bert_minorType = bert_alarm_params->next->next->value;
    bert_term_t* bert_opts = bert_alarm_params->next->next->next->value;

    ap->majorType = (bert_majorType->type == BERT_SMALL_INTEGER) ?
        bert_majorType->sint :
        (uint32_t) bert_majorType->lint;

    ap->minorType = (bert_minorType->type == BERT_SMALL_INTEGER) ?
        bert_minorType->sint :
        (uint32_t) bert_minorType->lint;

    res = decode_alarm_opts(bert_opts, ap);

    if (res == MafOk) {
        /* DN */
        ap->dn = copy_bert_binary(bert_dn);
        if(!ap->dn)
            return res;
    }

    return res;
}

MafReturnT comte_notify_alarm(MafOamSpiEventConsumerHandleT consumerId,
                              const char* eventType,
                              MafNameValuePairT** filterAddr,
                              fm_alarm_params_t *ap) {
	ENTER();

	MafReturnT res = MafFailure;
	comte_alarm_producer_t *alarm_p = ap_component->alarm_producer;

	if (alarm_p->router == NULL) {
		ERROR("Could not send alarm before registering producer!");
		return res;
	}

	/* The type is a redef of the MAF type */
	ComtENotificationFmStructT* alarm =
		comte_malloc(sizeof(ComtENotificationFmStructT));
	if (alarm == NULL) {
		ERROR("comte_malloc for ComtENotificationFmStructT failed");
		return MafFailure;
	}

	POPULATE_ALARM(alarm, ap);

    res = alarm_p->router->notify
        (alarm_p->handle, consumerId, eventType, filterAddr, alarm);
    if(res != MafOk){
        ERROR("Failed to send alarm: %i", res);
        FREE_ALARM(alarm);
    }

    LEAVE();
    return res;
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
 *        consumer will be notified for. filter is a null terminated array and must allways
 *        have a value even if it contains an empty array. The
 *        consumer must ensure that the instance of the filter
 *        exists during the time that the consumer is
 *        subscribing for the filtered events.
 *
 * @return MafOk, or @n
 * MafInvalidArgument if a parameter is not valid, or @n
 * MafFailure if an internal error occurred.
 */
static MafReturnT addFilter(MafOamSpiEventConsumerHandleT consumerId,
                            const char * eventType,
                            MafNameValuePairT ** filter) {
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
	    if (strncmp(filter[i]->name, 
			"MafOamSpiNotificationFmFilterTypeDateTime",
			strlen("MafOamSpiNotificationFmFilterTypeDateTime"))
		!= 0) {
		INFO("FilterValue: %s", filter[i]->value);
	    }
	    else {
		INFO("FilterValue: %" PRIu64,
		     *((uint64_t*)(filter[i]->value)));
	    }
            i++;
        }
        INFO("Filter addr: %p", filter);

        GENERIC_BERT_RPC(ap_component->config,
                         encode_registerEventConsumer(ALARM_PRODUCER,
                                                      consumerId,
                                                      eventType,
                                                      filter,
                                                      buff),
                         decode_registerEventConsumer(buff));

	LEAVE();
	return MafOk;
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
                               MafNameValuePairT ** filter) {
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

        GENERIC_BERT_RPC(ap_component->config,
                         encode_unregisterEventConsumer(ALARM_PRODUCER,
                                                        consumerId,
                                                        eventType,
                                                        buff),
                         decode_unregisterEventConsumer(buff));

	LEAVE();
	return MafOk;
}

/**
 * clearAll can be called by the Event Router to restart from a known state.
 * When called the producer will discard all filters.
 *
 * @return MafOk, or @n
 * MafInvalidArgument if a parameter is not valid, or @n
 * MafFailure if an internal error occurred.
 */
static MafReturnT clearAll() {
	ENTER();

        GENERIC_BERT_RPC(ap_component->config,
                         encode_unregisterAllEventConsumers(ALARM_PRODUCER,
                                                            buff),
                         decode_unregisterAllEventConsumers(buff));

	LEAVE();
	return MafOk;
}

/**
 * doneWithValue, Event router calls this function to notify the Producer that the
 * the value is not needed anymore. The producer will know how to delete the value.
 *
 * @param eventType [in] the type of event. It will help the producer how to cast the
 *                  value before deleting it.
 *
 * @param value [in] the value that is no longer needed.
 *
 * @return MafOk, or @n
 * MafInvalidArgument if a parameter is not valid, or @n
 * MafFailure if an internal error occurred.
 */
static MafReturnT doneWithValue(const char *eventType, void *value) {
	ENTER();

        ComtENotificationFmStructT* alarm =
            (ComtENotificationFmStructT*) value;
        FREE_ALARM(alarm);

        /* Clean up eventType */
        char* et = (char*) eventType;
        comte_free(et);

	LEAVE();
	return MafOk;
}

MafReturnT comte_register_alarm_producer() {
	ENTER();

	MafReturnT res = MafOk;

	res =
            ap_component->portal->getInterface(
                MafOamSpiEventService_1Id,
                (MafMgmtSpiInterface_1T**) &ap_component->alarm_producer->router);

	if (res != MafOk)
		return res;

        /* Register as alarm producer */
	res =
            ap_component->alarm_producer->router->registerProducer(
                &ap_component->alarm_producer->base,
                &ap_component->alarm_producer->handle);
        if(res != MafOk){
            ERROR("Could not register as producer, res: %d", res);
            return res;
        }

        INFO("Registering alarm type: %s", ComtENotificationFmEventType);
	res =
            ap_component->alarm_producer->router->addProducerEvent(
                ap_component->alarm_producer->handle,
                ComtENotificationFmEventType);
        if(res != MafOk)
            ERROR("Could not register alarm type, res: %d", res);

	LEAVE();
	return res;
}


MafReturnT comte_alarm_producer_create(comte_oam_component_t* comp) {
	ap_component = comp;

	ap_component->alarm_producer =
		comte_malloc(sizeof(comte_alarm_producer_t));
	comte_alarm_producer_t* ap = comp->alarm_producer;

	ap->base.addFilter = addFilter;
	ap->base.clearAll = clearAll;
	ap->base.doneWithValue = doneWithValue;
	ap->base.removeFilter = removeFilter;
	ap->router = NULL;

        return MafOk;
}

MafReturnT comte_alarm_producer_destroy(comte_oam_component_t* component) {
	MafReturnT res = MafOk;

	comte_free(component->alarm_producer);

	return res;
}

/**
 * @author Lukas Larsson
 * @created 2011-05-24
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <pthread.h>

#include "ComtEBertServer_1.h"
#include "ComtEComm_1.h"
#include "ComOamSpiComtEAlarmProducer_1.h"
#include "ComOamSpiComtEPmEventProducer_1.h"
#include "ComMwSpiComtEAvailabilityController_1.h"
#include "ComMwSpiComtECryptoUtil_1.h"
#include "ComtEUtils_1.h"



static pthread_t thread;
static pthread_barrier_t start_barrier;
static uint8_t bert_server_run = true;
static MafReturnT bert_server_start_ret = MafOk;

/* Forward declarations */
static bert_term_t* handle_cm_notification(bert_term_t* bert_consumerId,
                                         bert_binary_t* bert_eventType,
                                         bert_binary_t* bert_filterAddr,
                                         bert_list_t* bert_events);

static bert_term_t* handle_pm_gp_notification(bert_term_t* bert_consumerId,
                                         bert_binary_t* bert_eventType,
                                         bert_binary_t* bert_filterAddr,
                                         bert_stuple_t bert_pm_gp_args);

static bert_term_t* handle_alarm( bert_term_t* bert_consumerId,
                                bert_binary_t* bert_eventType,
                                bert_binary_t* bert_filterAddr,
                                bert_list_t* bert_alarm_args );

static MafReturnT comte_com_log(bert_term_t *bert_file,
                                bert_term_t *bert_func,
                                bert_term_t *bert_line,
                                bert_term_t *bert_severity,
                                bert_term_t *bert_facility,
                                bert_term_t *bert_msg);

bert_term_t* bert_handle_cast(bert_term_t* cast) {

    if (cast->stuple.values[2]->type == BERT_ATOM) {
        if (strncmp(cast->stuple.values[2]->atom, "notify_alarm", 12) == 0
            && cast->stuple.values[3]->type == BERT_SMALL_TUPLE) {
            bert_stuple_t params_tuple = cast->stuple.values[3]->stuple;
            bert_term_t* consumerId = params_tuple.values[0];
            bert_term_t* eventType = params_tuple.values[1];
            bert_term_t* filterAddr = params_tuple.values[2];
            bert_list_t* alarm_args = params_tuple.values[3]->list;


            if (bert_list_length(alarm_args) != 4){
                bert_term_t* err[] = { catom("error"),
                                       catom("bad_argument_list") };
                return ctuple(2, err);
            }

            return handle_alarm
                (consumerId, &eventType->binary,
                 &filterAddr->binary, alarm_args);


        } else if(strncmp(cast->stuple.values[2]->atom, "comte_log", 9) == 0) {

            bert_stuple_t params_tuple = cast->stuple.values[3]->stuple;
            bert_term_t* bert_file = params_tuple.values[0];
            bert_term_t* bert_func = params_tuple.values[1];
            bert_term_t* bert_line = params_tuple.values[2];
            bert_term_t* bert_severity = params_tuple.values[3];
            bert_term_t* bert_facility = params_tuple.values[4];
            bert_term_t* bert_entry = params_tuple.values[5];

            MafReturnT res = comte_com_log(bert_file, bert_func, bert_line,
                                           bert_severity, bert_facility,
                                           bert_entry);
            if(res != MafOk){
                bert_term_t* err[] = { catom("error"),
                                       catom("could_not_write_to_log") };
                return ctuple(2, err);
            }

            return catom("ok");

        } else if(strncmp(cast->stuple.values[2]->atom, "notify_cm_1", 11) == 0
                  && cast->stuple.values[3]->type == BERT_SMALL_TUPLE) {

            bert_stuple_t params_tuple = cast->stuple.values[3]->stuple;
            bert_term_t* consumerId = params_tuple.values[0];
            bert_term_t* eventType = params_tuple.values[1];
            bert_term_t* filterAddr = params_tuple.values[2];
            bert_list_t* events = params_tuple.values[3]->list;

            return handle_cm_notification
                (consumerId, &eventType->binary,
                 &filterAddr->binary, events);

        } else if(strncmp(cast->stuple.values[2]->atom, "pm_gp_ready_1", 13) == 0
                  && cast->stuple.values[3]->type == BERT_SMALL_TUPLE) {

            bert_stuple_t params_tuple = cast->stuple.values[3]->stuple;
            bert_term_t* consumerId = params_tuple.values[0];
            bert_term_t* eventType = params_tuple.values[1];
            bert_term_t* filterAddr = params_tuple.values[2];
            bert_stuple_t gpPm = params_tuple.values[3]->stuple;

            return handle_pm_gp_notification
                (consumerId, &eventType->binary, &filterAddr->binary, gpPm);

        } else if (strncmp(cast->stuple.values[2]->atom, "setHaMode", 9) == 0
                   && cast->stuple.values[3]->type == BERT_BYTE_LIST) {
            bert_byte_list_t args_list = cast->stuple.values[3]->byte_list;
            if (args_list.length != 2)
                return catom("error");
            MafReturnT res = comte_setHaMode(args_list.value[0],
                                             args_list.value[1]);
            if (res != MafOk)
                return catom("error");

            return catom("ok");
        } else if (strncmp(cast->stuple.values[2]->atom, "healthCheck", 11) == 0
                   && cast->stuple.values[3]->type == BERT_NIL) {

            MafReturnT res = comte_healthCheck();
            if (res != MafOk)
                return catom("error");

            return catom("ok");
        } else if (strncmp(cast->stuple.values[2]->atom, "prepareTermination", 18) == 0
                   && cast->stuple.values[3]->type == BERT_NIL) {

            MafReturnT res = comte_prepareTermination();
            if (res != MafOk)
                return catom("error");

            return catom("ok");
        } else if (strncmp(cast->stuple.values[2]->atom, "terminateProcess", 16) == 0
                   && cast->stuple.values[3]->type == BERT_NIL) {

            MafReturnT res = comte_terminateProcess();
            if (res != MafOk)
                return catom("error");

            return catom("ok");
        } else if(strncmp(cast->stuple.values[2]->atom, "comteDecryptTest", 16) == 0
                  && cast->stuple.values[3]->type == BERT_LIST) {
            bert_list_t *args_list = cast->stuple.values[3]->list;

            /* Copy decoded binary */
            bert_binary_t bstring = args_list->value->binary;
            char *encString = comte_malloc(sizeof(char*) * bstring.length+1);
            memset(encString, '\0', bstring.length+1);
            memcpy(encString, bstring.value, bstring.length);

            /* Decrypt string and create result */
            char **result = comte_malloc(sizeof(char*));
            MafReturnT res = comte_decrypt(encString, result);
            if (res != MafOk){
                bert_term_t *tup[] = {catom("error"), csint((uint8_t)res)};
                return ctuple(2, tup);
            }
            bert_term_t* reply = cstring(*result);

            /* Dealloc */
            comte_free(encString);
            comte_free(*result);
            comte_free(result);

            return reply;
        }
    }

    return catom("error");

}

MafReturnT bert_server_loop(comte_con_handle_t* listen_handle) {
	MafReturnT res = MafOk;

	while (bert_server_run) {
		comte_con_handle_t con_handle;

		con_handle.quiet = listen_handle->quiet;
		res = comte_accept(listen_handle, &con_handle);

		if (res == MafTimeOut)
			continue;
		else if (res != MafOk)
			return res;

		comte_buff_t buff;
		if(comte_recv(&buff, &con_handle) != MafOk){
                    ERROR("Unexpected input on socket, cancelling decode.");
                    continue;
                }

                bert_buff_t* bbuff = comte_malloc(sizeof(bert_buff_t));
		bbuff->buff = buff.buff;
		bbuff->length = buff.size;

		bert_term_t* command = bert_decode(bbuff);

		comte_free(bbuff->buff);
		comte_free(bbuff);

		bert_term_t* reply;

		if (command == NULL || command->type != BERT_SMALL_TUPLE
                    || command->stuple.values[0]->type != BERT_ATOM) {
                    reply = catom("error");
		} else if (strncmp(command->stuple.values[0]->atom, "cast", 4) == 0
                           && command->stuple.size == 4) {
                    reply = bert_handle_cast(command);
		} else {
                    reply = catom("error");
		}

		bert_free(command);

		bert_buff_t* enc_buff = bert_encode(reply);

		buff.buff = enc_buff->buff;
		buff.size = enc_buff->length;

		comte_free(enc_buff);

		comte_send(&buff, &con_handle);

		bert_free(reply);

		comte_free(buff.buff);

		comte_recv_close(&con_handle);
		comte_disconnect(&con_handle);
	}

	return res;
}

static void* init_bert_server(void* context) {
	comte_con_handle_t listen_handle;
	comte_config_t* config = (comte_config_t*) context;

	listen_handle.quiet = 0;
	bert_server_start_ret =
            comte_listen(config->com_ip, config->com_port, &listen_handle);
	/* Release main thread */
	pthread_barrier_wait(&start_barrier);

	if (bert_server_start_ret == MafOk) {
		bert_server_loop(&listen_handle);
		comte_disconnect(&listen_handle);

	}
	return context;
}

MafReturnT comte_stop_bert_server() {

	bert_server_run = false;
	pthread_join(thread, NULL);

	return MafOk;
}

MafReturnT comte_start_bert_server(comte_config_t *config) {
    /* Init barrier for caller and bert server thread = 2 */
    pthread_barrier_init(&start_barrier, NULL, 2);
    /* Create thread */
    pthread_create(&thread, NULL, init_bert_server, config);
    /* Caller wait */
    pthread_barrier_wait(&start_barrier);

    return bert_server_start_ret;

}


/*
 * ------------------------------
 * INTERNAL FUNCTIONS
 * ------------------------------
 */

MafOamSpiEventConsumerHandleT decode_consumerId(bert_term_t *consumerId){
    switch (consumerId->type){
    case BERT_SMALL_INTEGER:
        return consumerId->sint;
    case BERT_INTEGER:
        return consumerId->lint;
    case BERT_SMALL_BIG:
        return consumerId->sbig;
    case BERT_UINT64:
        return consumerId->uint64;
    default:
        return -1;
    }
}

static MafNameValuePairT** decode_filter_addr(bert_binary_t* bert_filterAddr){
    MafNameValuePairT** filter;

    if (bert_filterAddr->length != sizeof(void *)) {
        ERROR("Filter Addr length: %lu",
              (unsigned long) bert_filterAddr->length);
        return NULL;
    }
    filter = *((void **) bert_filterAddr->value);
    INFO("Filter Addr: %p", filter);
    return filter;
}



static bert_term_t* handle_alarm
(bert_term_t* bert_consumerId, bert_binary_t* bert_eventType,
 bert_binary_t* bert_filterAddr, bert_list_t* bert_alarm_params ) {
    ENTER();

    /* Consumer ID */
    MafOamSpiEventConsumerHandleT consumerId =
        decode_consumerId(bert_consumerId);

    /* Event Type */
    char *eventType = copy_bert_binary(bert_eventType);
    INFO("EventType: %s", eventType);

    /* Filter Addr  */
    MafNameValuePairT** filterAddr =
        decode_filter_addr(bert_filterAddr);
    if (filterAddr == NULL) {
        comte_free(eventType);
        bert_term_t* err[] = { catom("error"), catom("badarg") };
        bert_term_t* error_tuple = ctuple(2, err);
        LEAVE();
        return error_tuple;
    }

    fm_alarm_params_t ap;
    MafReturnT res = populate_alarm_params(&ap, bert_alarm_params);
    if(res == MafOk){
        res = comte_notify_alarm(consumerId, eventType, filterAddr, &ap);
        /* eventType is freed by doneWithValue() */
    }
    else {
        comte_free(eventType);
        bert_term_t* err[] = { catom("error"), catom("not_ready") };
        bert_term_t* error_tuple = ctuple(2, err);
        LEAVE();
        return error_tuple;
    }

    bert_term_t* ok = catom("ok");
    LEAVE();
    return ok;
}


static bert_term_t* handle_cm_notification
(bert_term_t* bert_consumerId, bert_binary_t* bert_eventType,
 bert_binary_t* bert_filterAddr, bert_list_t* bert_event_params) {
    ENTER();

    /* Tuples as events - check event type */
    bert_list_t* event_list =
        bert_event_params->next->next->value->list;
    bert_stuple_t curr_event;
    curr_event = event_list->value->stuple;


    if (strncmp(curr_event.values[0]->atom, "cm_event_1", 10) == 0) {
        MafOamSpiCmEvent_Notification_1T* cm_ntf =
            create_cm_notification(bert_event_params);
        /* Notification is later freed in  */
        /* ComOamSpiComtECmEventProducer:doneWithValue */

        if(cm_ntf != NULL) {
            /* Consumer ID */
            MafOamSpiEventConsumerHandleT consumerId =
                decode_consumerId(bert_consumerId);
            INFO("ConsumerId: %lu", consumerId);

            /* Event Type - needs to survive until doneWithValue */
            char *eventType = copy_bert_binary(bert_eventType);
            INFO("EventType: %s", eventType);

            /* Filter Addr */
            MafNameValuePairT** filterAddr =
                decode_filter_addr(bert_filterAddr);
            if (filterAddr == NULL) {
                bert_term_t* err[] = { catom("error"), catom("badarg") };
                bert_term_t* error_tuple = ctuple(2, err);
                LEAVE();
                return error_tuple;
            }

            /* Notify consumer */
            MafReturnT res = comte_notify_cm
                (consumerId, (const char*) eventType, filterAddr, cm_ntf);

            /* Cleanup */
            if(res != MafOk){
                ERROR("Error at sending event: %i", res);
                comte_free(eventType);
                free_cm_notification(cm_ntf);

                bert_term_t* err[] = { catom("error"), catom("not_ready") };
                bert_term_t* error_tuple = ctuple(2, err);
                LEAVE();
                return error_tuple;
            }

        }
        else {
            ERROR("Error create_cm_notification returned NULL");
            bert_term_t* err[] = { catom("error"), catom("create_failed") };
            bert_term_t* error_tuple = ctuple(2, err);
            LEAVE();
            return error_tuple;
        }
    }
    else {
        ERROR("No such event: %s", curr_event.values[0]->atom);
        bert_term_t* err2[] =
            { catom("no_such_event"), catom(curr_event.values[0]->atom) };
        bert_term_t* err[] = { catom("error"), ctuple(2, err2) };
        bert_term_t* error_tuple = ctuple(2, err);
        LEAVE();
        return error_tuple;
    }

    bert_term_t* ok = catom("ok");
    LEAVE();
    return ok;
}

static uint64_t getBertValue(bert_term_t * bert) {
   switch(bert->type){
        case BERT_UINT64:
            return  bert->uint64;
        case BERT_SMALL_BIG:
            return (uint64_t)bert->sbig;
            break;
        case BERT_SMALL_INTEGER:
            return (uint64_t)bert->sint;
        case BERT_INTEGER:
            return (uint64_t)bert->lint;
        default:
            ERROR("Bad bertValue type: %i", bert->type);
            return 0;
     }
}

static bert_term_t* handle_pm_gp_notification
(bert_term_t* bert_consumerId, bert_binary_t* bert_eventType,
 bert_binary_t* bert_filterAddr, bert_stuple_t bert_pm_gp_params) {
    ENTER();

    /* Consumer ID */
    MafOamSpiEventConsumerHandleT consumerId = decode_consumerId(
            bert_consumerId);
    INFO("ConsumerId: %lu", consumerId);

    /* Event Type - needs to survive until doneWithValue */
    char *eventType = copy_bert_binary(bert_eventType);
    INFO("EventType: %s", eventType);

    /* Filter Addr */
    MafNameValuePairT** filterAddr = decode_filter_addr(bert_filterAddr);
    if (filterAddr == NULL) {
        bert_term_t* err[] = { catom("error"), catom("badarg") };
        bert_term_t* error_tuple = ctuple(2, err);
        LEAVE();
        return error_tuple;
    }

    // create ComOamSpiPmEventValue_2T (pm gp notification)
    if (bert_pm_gp_params.size != 4) {
        ERROR("Wrong tuple size in bert_pm_gp_params: %d",
                bert_pm_gp_params.size);
        bert_term_t* err[] = { catom("error"), catom("badarg") };
        bert_term_t* error_tuple = ctuple(2, err);
        LEAVE();
        return error_tuple;
    }

    ComOamSpiPmEventValue_2T * pm_ntf = (ComOamSpiPmEventValue_2T*)comte_malloc(sizeof(ComOamSpiPmEventValue_2T));
    pm_ntf->gpId = getBertValue(bert_pm_gp_params.values[0]);
    bert_term_t * bert_job = bert_pm_gp_params.values[1] ;
    switch ( bert_job->type ) {
        case BERT_BINARY: {
            bert_binary_t bert_jobId = bert_job->binary;
            char *jobId= comte_malloc(sizeof(char) * (bert_jobId.length + 1));
            memcpy(jobId, bert_jobId.value, bert_jobId.length);
            jobId[bert_jobId.length] = 0;
            pm_ntf->jobId = jobId;
            break;
        }
        case BERT_BYTE_LIST: {
            uint16_t count = bert_job->byte_list.length;
            uint8_t * bytes = bert_job->byte_list.value;
            char * jobId = comte_malloc(sizeof(char*) * (count+1));
            memcpy(jobId, bytes, count);
            jobId[count] = 0;
            pm_ntf->jobId = jobId;
            break;
        }
        default:
            ERROR("Error at decoding pm_ntf");
            bert_term_t* err[] = { catom("error"), catom("badarg") };
            bert_term_t* error_tuple = ctuple(2, err);
            return error_tuple;
            break;
    }
    pm_ntf->gpStartTimestampInNanoSeconds = getBertValue(bert_pm_gp_params.values[2]);
    pm_ntf->gpEndTimestampInNanoSeconds = getBertValue(bert_pm_gp_params.values[3]);

    /* Notify consumer */
    MafReturnT res = comte_notify_pm
        (consumerId, (const char*) eventType, filterAddr, pm_ntf);

    /* Cleanup */
    if(res != MafOk){
        ERROR("Error at sending event: %i", res);
        comte_free(eventType);
        free_pm_notification(pm_ntf);

        bert_term_t* err[] = { catom("error"), catom("not_ready") };
        bert_term_t* error_tuple = ctuple(2, err);
        LEAVE();
        return error_tuple;
    }

    bert_term_t* ok = catom("ok");
    LEAVE();
    return ok;
}


static MafReturnT comte_com_log(bert_term_t *bert_file,
                                bert_term_t *bert_func,
                                bert_term_t *bert_line,
                                bert_term_t *bert_severity,
                                bert_term_t *bert_facility,
                                bert_term_t *bert_msg){

    MafReturnT res = MafOk;
    char *file = NULL;
    char *func = NULL;
    char *msg = NULL;

    switch(bert_file->type){
    case BERT_ATOM:
        file = strndup(bert_file->atom, strlen(bert_file->atom));
        break;
    case BERT_BINARY:
        file = copy_bert_binary(&bert_file->binary);
        break;
    default:
        res = MafFailure;
        goto log_error;
    }

    if(!file){
        res = MafFailure;
        goto log_error;
    }


    /* char *func = NULL; */
    switch(bert_func->type){
    case BERT_ATOM:
        func = strndup(bert_func->atom, strlen(bert_func->atom));
        break;
    case BERT_BINARY:
        func = copy_bert_binary(&bert_func->binary);
        break;
    default:
        res = MafFailure;
    }

    if(!func){
        res = MafFailure;
        goto log_error;
    }


    uint16_t line = 0;
    switch(bert_line->type){
    case BERT_SMALL_INTEGER:
        line = bert_line->sint;
        break;
    case BERT_INTEGER:
        line = bert_line->lint;
        break;
    default:
        line = 0;
    }

    MwSpiSeverityT severity = (MwSpiSeverityT)bert_severity->sint;
    MwSpiFacilityT facility = (MwSpiFacilityT)bert_facility->sint;

    msg = copy_bert_binary(&bert_msg->binary);
    if(!msg){
        res = MafFailure;
        goto log_error;
    }

    res = comte_log_printf(file, func, line, severity, facility, "%s", msg);

log_error:

    comte_cond_free(file);
    comte_cond_free(func);
    comte_cond_free(msg);

    return res;
}

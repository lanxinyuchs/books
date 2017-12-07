/**
 * @author Magnus Lidén
 * @created 2014-04-12
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <MafOamSpiServiceIdentities_1.h>
#include <ComOamSpiServiceIdentities_1.h>

#include "ComOamSpiComtEPmMeasurement_1.h"
#include "ComOamSpiComtEConverter_1.h"
#include "ComtEUtils_1.h"

comte_oam_component_t* oam_comp;

#if !((COM_PM_MEAS_H+0) >= 2)
#warning "Using legacy interface ComOamSpiPmMeasurement_1"
#endif


/* ----------------------------------------------------
*  COM PM Measurements
*  ----------------------------------------------------
*/


static MafReturnT decode_pm_value_type
(bert_term_t* t, ComOamSpiPmMeasurementValueType_nT *vt) {
    bert_stuple_t *meas_val;

    if (t->type != BERT_SMALL_TUPLE || t->stuple.size != 2) {
        return MafFailure;
    }
    meas_val = &t->stuple;
#if ((COM_PM_MEAS_H+0) >= 2)
     /* {4,Value} | {{4,Value},_} */
    if (meas_val->values[0]->type == BERT_SMALL_TUPLE &&
        meas_val->values[0]->stuple.size == 2) {
        meas_val = &meas_val->values[0]->stuple; /* {4,Value} */
    }
#endif
    switch (meas_val->values[0]->sint) {
    case MafOamSpiMoAttributeType_3_INT64:
        *vt = ComOamSpiPmMeasurementValueType_n_INT;
        break;
    case MafOamSpiMoAttributeType_3_DECIMAL64:
		*vt = ComOamSpiPmMeasurementValueType_n_FLOAT;
        break;
    default:
        return MafFailure;
    }
    return MafOk;
}

static MafReturnT decode_pm_value
(bert_term_t *t, ComOamSpiPmMeasurement_nT *m, uint32_t n) {
    ComOamSpiPmMeasurementValueType_nT valueType;
    bert_stuple_t *pm_tuple;
    bert_term_t *pm_int;
    int64_t *int_val;
    MafReturnT ret;
    
    ret = decode_pm_value_type(t, &valueType);
    if (ret != MafOk) return ret;
    if (valueType != m->valueType) return MafFailure;

    pm_tuple = &t->stuple;
#if ((COM_PM_MEAS_H+0) >= 2)
    /* pm_tuple :: {4,Value} | {{4,Value},_} */
    if (pm_tuple->values[0]->type == BERT_SMALL_TUPLE) {
        if (pm_tuple->values[1]->type != BERT_BOOLEAN) return MafFailure;
        /* pm_tuple :: {{4,Value},bool()} */
        m->value[n].isSuspect = pm_tuple->values[1]->boolean;
        pm_tuple = &pm_tuple->values[0]->stuple; /* {4,Value} */
    }
    else {
        m->value[n].isSuspect = false;
    }
#if ((COM_PM_MEAS_H+0) >= 3)
    int_val = &m->value[n].value.intVal;
#else
    int_val = &m->value[n].intVal;
#endif
#else
    int_val = &m->value.intVal[n];
#endif
    pm_int = pm_tuple->values[1];

    switch(pm_int->type){
    case BERT_SMALL_INTEGER:
        *int_val = (int64_t) pm_int->sint;
        break;
    case BERT_INTEGER:
        *int_val = (int64_t) pm_int->lint;
        break;
    case BERT_SMALL_BIG:
        *int_val = (int64_t) pm_int->sbig;
        break;
    case BERT_UINT64:
        *int_val = (int64_t) pm_int->uint64;
        break;
    default:
        ERROR("Unknown PM value type: %d",
              pm_int->type);
        return MafFailure;
    }
    /* INFO("PM value %li", *int_val); */

    return MafOk;
}


#if ((COM_PM_MEAS_H+0) >= 2)
static bool decode_uint32(bert_term_t *t, uint32_t *p) {
	int64_t i = 0;

	switch (t->type) {
	case BERT_SMALL_INTEGER:
		i = (int64_t) t->sint;
		break;
	case BERT_INTEGER:
		i = (int64_t) t->lint;
		break;
	case BERT_SMALL_BIG:
		i = (int64_t) t->sbig;
		break;
	default:
		ERROR("Not a uint32: %d", t->type);
		return false;
	}
	if (i < 0 || i != (int64_t) ((uint32_t) i)) {
		ERROR("Bad uint32: %lld", (long long int) i);
		return false;
	}
	*p = (uint32_t) i;
	return true;
}
#endif

static MafReturnT decode_pm_meas_values
(ComOamSpiPmMeasurement_nT* curr_meas, bert_term_t* meas_vals) {
    //[Value,...]}]
    MafReturnT res;

    res = MafOk;
    if(meas_vals->type == BERT_NIL){
        curr_meas->nrOfValues = 0;
    }
    else if(meas_vals->type == BERT_LIST){
        bert_list_t* meas_list;
        uint32_t num_vals;

        meas_list = meas_vals->list;
        num_vals = bert_list_length(meas_list);
        curr_meas->nrOfValues = num_vals;
        if (num_vals == 0) goto res; /* BERT will not cause this? */

        /* Check the type of the first value */
        res = decode_pm_value_type
            (meas_list->value, &curr_meas->valueType);
        if (res != MafOk) {
            ERROR("Invalied PM value type: %d", meas_list->value->type);
            goto res;
        }

        switch (curr_meas->valueType) {
        case ComOamSpiPmMeasurementValueType_n_INT: {
            uint32_t i;
            
#if ((COM_PM_MEAS_H+0) >= 2)
            curr_meas->value =
                comte_malloc(sizeof(* curr_meas->value) * num_vals);
#else
            /* Alloc for integer values */
            curr_meas->value.intVal =
                comte_malloc(sizeof(int64_t) * num_vals);
#endif
            for (i = 0; i < num_vals; i++) {
                res = decode_pm_value(meas_list->value, curr_meas, i);
                if (res != MafOk) goto res;
                /* Values will be freed by releaseMeasurements() */
		meas_list = meas_list->next;
            }
            break;
        }
        default:
            /* FLOATS not handled */
            res = MafAborted;
            break;
        } /* switch () */
    }
    else {
        ERROR("Invalid measurements type: %d", meas_vals->type);
        res = MafFailure;
    }

 res:
    return res;
}

static MafReturnT decode_pm_meas_opts
(ComOamSpiPmMeasurement_nT* curr_meas, bert_term_t* bert_opts) {
    int res;

    res = MafOk;
    switch(bert_opts->type){
    case BERT_NIL: {
        /* The empty list */
        break;
    }
    case BERT_LIST: {
        bert_list_t *opts_list;
        for (opts_list = bert_opts->list;
             opts_list != NULL;
             opts_list = opts_list->next) {
            bert_stuple_t *prop;

            if (opts_list->value->type != BERT_SMALL_TUPLE) {
                ERROR("Invalid PM property type: %d",
                      opts_list->value->type);
                res = MafFailure;
                break;
            }
            prop = &opts_list->value->stuple;
            if (prop->size != 2) {
                ERROR("Invalid PM property arity: %d", prop->size);
                res = MafFailure;
                break;
            }

            if (strncmp(prop->values[0]->atom, "info", 4) == 0 &&
				prop->values[1]->type == BERT_BINARY) {
#if !((COM_PM_MEAS_H+0) >= 2)
                /* Copy description */
                char* desc =
                    copy_bert_binary(&prop->values[1]->binary);
                INFO("Meas desc: %s", desc);
                curr_meas->description = desc;
#endif
            }
#if ((COM_PM_MEAS_H+0) >= 2)
            else if (strncmp(prop->values[0]->atom, "job_id", 6) == 0 &&
					 prop->values[1]->type == BERT_BINARY) {
                /* Copy jobId */
                char* jobId =
                    copy_bert_binary(&prop->values[1]->binary);
                INFO("Meas jobId: %s", jobId);
                curr_meas->jobId = jobId;
			}
            else if
				(strncmp(prop->values[0]->atom, "error_information", 17) == 0
				 &&
				 prop->values[1]->type == BERT_BINARY) {
                /* Copy errorInformation */
                char* errorInformation =
                    copy_bert_binary(&prop->values[1]->binary);
                INFO("Meas errInf: %s", errorInformation);
                curr_meas->errorInformation = errorInformation;
			}
			else if
				(strncmp(prop->values[0]->atom, "gp_in_seconds", 13) == 0
				 &&
				 decode_uint32(prop->values[1], &curr_meas->gpInSeconds) ) {
			}
#endif
            else {
                /* Unknown property */
                ERROR("Unknown PM property: %s", prop->values[0]->atom);
                res = MafFailure;
                break;
            }
        } /* for () */
        break;
    }
    default: {
        ERROR("Invalid PM option list type: %d", bert_opts->type);
        res = MafFailure;
    }
    } // End of switch
    return res;
}



static void releaseMeasurements(ComOamSpiPmMeasuredObject_nT* meas_obj){
    ENTER();

    if(meas_obj->measurements == NULL){
        LEAVE();
        return;
    }

    int i;
    for (i = 0;
		 i < ComOamSpiPmMeasuredObject__nrOfMeasurements(meas_obj);
		 i++) {
        /* Free name and description */
        if(meas_obj->measurements[i].name != NULL){
            comte_free((char*)meas_obj->measurements[i].name);
        }

#if ((COM_PM_MEAS_H+0) >= 2)
        /* Optional jobId */
        if(meas_obj->measurements[i].jobId != NULL){
            comte_free((char*)meas_obj->measurements[i].jobId);
        }
        /* Optional errorInformation */
        if(meas_obj->measurements[i].errorInformation != NULL){
            comte_free((char*)meas_obj->measurements[i].errorInformation);
        }
#else
        /* Optional description */
        if(meas_obj->measurements[i].description != NULL){
            comte_free((char*)meas_obj->measurements[i].description);
        }
#endif

        /* Free measurement values */
        if (meas_obj->measurements[i].nrOfValues > 0) {
#if ((COM_PM_MEAS_H+0) >= 2)
            comte_free(meas_obj->measurements[i].value);
#else
            switch (meas_obj->measurements[i].valueType) {
            case ComOamSpiPmMeasurementValueType_n_INT:
                comte_free(meas_obj->measurements[i].value.intVal);
                break;
            case ComOamSpiPmMeasurementValueType_n_FLOAT:
                /* Not implemented */
                comte_free(meas_obj->measurements[i].value.floatVal);
                break;
            }
#endif
        }
    }

    comte_free(meas_obj->measurements);
    LEAVE();
}

static MafReturnT decode_pm_measurements
(bert_list_t* bert_meas_list, ComOamSpiPmMeasuredObject_nT * measuredObject) {
    ComOamSpiPmMeasurement_nT *meas_arr, *curr_meas;
    bert_list_t *curr_list;
    MafReturnT res;

	ComOamSpiPmMeasuredObject__nrOfMeasurements(measuredObject) =
		bert_list_length(bert_meas_list);
    /* Create measurement */
    meas_arr =
        comte_malloc
		(sizeof(ComOamSpiPmMeasurement_nT) *
		 ComOamSpiPmMeasuredObject__nrOfMeasurements(measuredObject));
    measuredObject->measurements = curr_meas = meas_arr;

    res = MafOk;
    for (curr_list = bert_meas_list;
         curr_list != NULL;
         curr_list = curr_list->next) {
        bert_stuple_t* bert_meas;
        MafReturnT r;

        curr_meas->name = NULL;
        curr_meas->nrOfValues = 0;
#if ((COM_PM_MEAS_H+0) >= 2)
		curr_meas->jobId = NULL;
		curr_meas->errorInformation = NULL;
		curr_meas->gpInSeconds = 0;
#else
		curr_meas->description = NULL;
#endif
        if (curr_list->value->type != BERT_SMALL_TUPLE) {
            ERROR("Decode of measurement wrong type: %d",
                  curr_list->value->type);
            res = MafFailure;
            continue;
        }
        bert_meas = &curr_list->value->stuple;
        if (bert_meas->size < 2 || 3 < bert_meas->size) {
            ERROR("Decode of measurement wrong arity: %d",
                  bert_meas->size);
            res = MafFailure;
            continue;
        }

        if (bert_meas->values[0]->type != BERT_BINARY) {
            ERROR("Decode of measurement name wrong type: %d",
                  bert_meas->values[0]->type);
            res = MafFailure;
            continue;
        }
        /* Copy meas name */
        curr_meas->name =
            copy_bert_binary(&bert_meas->values[0]->binary);
        INFO("Meas name: %s", curr_meas->name);

        /* Meas vals */
        /* [{?INT64, 234432},...] */
        r = decode_pm_meas_values(curr_meas, bert_meas->values[1]);
        if(r != MafOk){
            ERROR("Decode of measurement values failed: %d", res);
            res = r;
            continue;
        }

        /* [{info, <<"Descriptions">>}, ...] */
        /* Copy measurement optionals */
        if (bert_meas->size >= 3) {
            r = decode_pm_meas_opts(curr_meas, bert_meas->values[2]);
            if (r != MafOk) {
                res = r;
                continue;
            }
        }

        /* Move to next measurement */
        curr_meas++;
    }

    if (res != MafOk) releaseMeasurements(measuredObject);
    return res;
}

static MafReturnT decode_getPmMeasurements
(comte_buff_t* buff, ComOamSpiPmMeasuredObject_nT * measuredObject) {
    MafReturnT res = MafFailure;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);

    /* Default values */
    measuredObject->measurements = NULL;
	ComOamSpiPmMeasuredObject__nrOfMeasurements(measuredObject) = 0;
    measuredObject->release = NULL;

    if(res == MafOk){

        switch(response->type){
        case BERT_NIL: {
            /* [] */
            measuredObject->release = releaseMeasurements;
            break;
        }
        case BERT_LIST: {
            /* [{<<"MyCounter">>,[Value,...],[Opt,...]}] */
            res = decode_pm_measurements(response->list, measuredObject);
            if (res == MafOk) {
                measuredObject->release = releaseMeasurements;
            }
            break;
        }
        default: {
            ERROR("Bad response type: %i", response->type);
            res = MafFailure;
            break;
        }
        }
    }
    else {
        ERROR("Bad PM measure response: %d", res);
    }

    /* Free response */
    bert_free((bert_term_t*) response);
    return res;
}


static MafReturnT encode_getPmMeasurements
(const char * dn,
#if ((COM_PM_MEAS_H+0) >= 2)
 const ComOamSpiPmMeasurementFilter_nT *filter,
#endif
 comte_buff_t* buff) {
    bert_term_t *b_dn;
#if ((COM_PM_MEAS_H+0) >= 2)
    bert_term_t *b_names, *b_opts, *opts[2], *tup[2];
    const char **mt;

    mt = filter->measurementTypes;
    if (mt == NULL || *mt == NULL) {
        b_names = clist(0, NULL);
    }
    else {
        bert_list_t *names, *n;

        names = n = bert_alloc(sizeof(*n));
        for (;;) {
            n->value = cstring(*mt);
            if (*(++mt) == NULL) break;
            n->next = bert_alloc(sizeof(*n));
            n = n->next;
        }
        n->next = NULL;
        b_names = bert_alloc(sizeof(*b_names));
        b_names->type = BERT_LIST;
        b_names->list = names;
    }

    tup[0] = catom("verbose");
    tup[1] = cbool((uint8_t) filter->isVerbose);
    opts[0] = ctuple(2, tup);
    if (filter->jobId == NULL) {
        b_opts = clist(1, opts);
    }
    else {
        tup[0] = catom("job_id");
        tup[1] = cstring(filter->jobId);
        opts[1] = ctuple(2, tup);
        b_opts = clist(2, opts);
    }
#endif

    b_dn = cstring(dn);
    {
#if ((COM_PM_MEAS_H+0) >= 2)
        bert_term_t * args[3] = { b_dn, b_names, b_opts };
        return encode_rpc("getPmMeasurements", clist(3, args), buff);
#else
        bert_term_t * args[1] = { b_dn };
        return encode_rpc("getPmMeasurements", clist(1, args), buff);
#endif
    }
}


/**
 * This function returns PM measurement names and values associated to a measured object.
 *
 * @param [in] dn MO distinguished name in 3GPP format. This is the DN of the MO providing the values. Example: "ManagedElement=1,MoWithCounters=2"
 *
 * @param [in] filter
 *          A filter for filtering of which data to be fetched.
 *
 * @param [in|out] measuredObject
 *
 *         The result container contains:
 *             An array of measurements associated to this measured object
 *             The current value of these measurements.
 *
 *         The result container should be allocated by the caller.
 *         The content of the container is allocated by the callee.
 *         The allocated memory for the content is released via the release function in the container.
 *         The container itself shall be released by the caller.
 *         The caller shall invoke the release function if set.
 *         The release function shall be provided by the SPI implementation.
 *         The implementer may set the function pointer to NULL to indicate that there is nothing to release.
 *
 * @return MafOk if the call was successful
 *         MafNotExist if a measured object does not exist
 *         MafFailure if an internal error occurred
 */
static MafReturnT getMeasurementValues
(const char * dn,
#if ((COM_PM_MEAS_H+0) >= 2)
 const ComOamSpiPmMeasurementFilter_nT *filter,
#endif
 ComOamSpiPmMeasuredObject_nT * measuredObject) {
    ENTER();

    MafReturnT res = MafOk;
    comte_con_handle_t connection;

    connection.quiet = 0;
    res = comte_connect(oam_comp->config->comte_ip,
                        oam_comp->config->comte_port,
                        &connection);
    if (res != MafOk) {
        ERROR("connect failed");
        return res;
    }

    comte_buff_t buff;
#if ((COM_PM_MEAS_H+0) >= 2)
    res = encode_getPmMeasurements(dn, filter, &buff);
#else
    res = encode_getPmMeasurements(dn, &buff);
#endif
    if (res != MafOk) {
        ERROR("encode failed");
        comte_disconnect(&connection);
        return res;
    }

    res = comte_send_recv(&buff, &connection);
    if (res != MafOk) {
        return res;
    }

    res = decode_getPmMeasurements(&buff, measuredObject);
    if (res != MafOk) {
        comte_free(buff.buff);
        comte_disconnect(&connection);
        return res;
    }

    comte_free(buff.buff);
    comte_disconnect(&connection);
    LEAVE();
    return res;
}



static MafReturnT encode_getPmMeasurementNames(const char * dn,
                                        comte_buff_t* buff){
    bert_term_t * args[] = { cstring(dn) };
    return encode_rpc("getPmMeasurementNames", clist(1, args), buff);
}




static void releaseMeasurementNames
(struct ComOamSpiPmMeasuredObjectMeasurementNames_n* measNames) {
    ENTER();

    ComOamSpiPmMeasuredObjectMeasurementNames_nT* names =
            (ComOamSpiPmMeasuredObjectMeasurementNames_nT*) measNames;

    if(names->definitions){
        int i;
        for(i = 0; i < names->nrOfValues; i++){
            /* Counter name */
            if(names->definitions[i].name){
                comte_free((char*) names->definitions[i].name);
            }
            /* Optional description */
            if(names->definitions[i].description){
                comte_free((char*) names->definitions[i].description);
            }
        }
        comte_free(names->definitions);
    }

    LEAVE();
}

static MafReturnT decode_pm_meas_names
(bert_list_t* bert_name_list,
 ComOamSpiPmMeasuredObjectMeasurementNames_nT* measNames) {
    MafReturnT res = MafOk;
    int i = 0;

    measNames->nrOfValues = bert_list_length(bert_name_list);

    measNames->definitions = (ComOamSpiPmMeasurementDefinition_nT*)
        comte_malloc(sizeof(ComOamSpiPmMeasurementDefinition_nT) *
                     measNames->nrOfValues);

    /* [{<<"MyCounter">>, <<"CounterDescription">>}, ...] */
    for(i = 0; i < measNames->nrOfValues; i++){
        measNames->definitions[i].description = NULL;
        switch(bert_name_list->value->type){
        case BERT_SMALL_TUPLE: {
            bert_stuple_t* bert_def_tuple = &bert_name_list->value->stuple;

            if(bert_def_tuple->size != 2 ||
               bert_def_tuple->values[0]->type != BERT_BINARY ||
	       ! (bert_def_tuple->values[1]->type == BERT_LIST ||
		  bert_def_tuple->values[1]->type == BERT_NIL)){
                measNames->definitions[i].name = NULL;
                ERROR("Unknown PM name type: %d",
                      bert_def_tuple->values[0]->type);
                res = MafFailure;
                break;
            }
            /* Measurement name */
            char* name = copy_bert_binary(&bert_def_tuple->values[0]->binary);
            measNames->definitions[i].name = (const char*)name;
            INFO("PM Measurement name: %s", name);

            /* Measurement optionals */
            /* TODO */
            /* Re-use when ComOamSpiPmMeasurementDefinition_nT is used widely */
	    if(bert_def_tuple->values[1]->type == BERT_NIL) break;
            bert_list_t* bert_opts_list = bert_def_tuple->values[1]->list;
            while(bert_opts_list != NULL){
                if(bert_opts_list->value->type == BERT_SMALL_TUPLE &&
                   bert_opts_list->value->stuple.size == 2 &&
                   bert_opts_list->value->stuple.values[0]->type == BERT_ATOM){
                    bert_stuple_t* prop = &bert_opts_list->value->stuple;
                    if(strncmp(prop->values[0]->atom, "info", 4) == 0 &&
                       prop->values[1]->type == BERT_BINARY){
                        /* Copy description */
                        char* desc = copy_bert_binary(&prop->values[1]->binary);
                        INFO("PM Measurement desc: %s", desc);
                        measNames->definitions[i].description = desc;
                    }
                    else {
                        /* Unknown property */
                        ERROR("Unknown PM property: %s", prop->values[0]->atom);
                        res = MafFailure;
                        break;
                    }
                }
                else {
                    ERROR("Unknown PM property type: %d",
                          bert_opts_list->value->type);
                    res = MafFailure;
                    break;
                }
                bert_opts_list = bert_opts_list->next;
            }
            break;
        }
        case BERT_BINARY: {
            char* name = copy_bert_binary(&bert_name_list->value->binary);
            measNames->definitions[i].name = (const char*)name;
            INFO("PM Measurement name: %s", name);
            break;
        }
        default: {
            measNames->definitions[i].name = NULL;
            ERROR("Bad measurement name type: %d", bert_name_list->value->type);
            res = MafFailure;
            break;
        }
        }
        if(measNames->definitions[i].description == NULL){
            /* com error if not created */
            char* desc = comte_malloc(sizeof(char) * 1);
            desc[0] = '\0';
            measNames->definitions[i].description = (const char*) desc;
        }
        /* Move on */
        bert_name_list = bert_name_list->next;
    }
    if(res != MafOk){
        releaseMeasurementNames(measNames);
    }
    return res;
}

static MafReturnT decode_getPmMeasurementNames
(comte_buff_t* buff,
 ComOamSpiPmMeasuredObjectMeasurementNames_nT* measNames) {

    MafReturnT res = MafFailure;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);

    /* Default values */
    measNames->definitions = NULL;
    measNames->nrOfValues = 0;
    measNames->release = NULL;

    if(res == MafOk) {

        switch(response->type){

        case BERT_NIL: {
            measNames->release = releaseMeasurementNames;
            break;
        }

        case BERT_LIST: {
            /* Response: [<<"MeasNames">>,...] */
            res = decode_pm_meas_names(response->list, measNames);
            if(res == MafOk) {
                measNames->release = releaseMeasurementNames;
            }
            break;
        }

        default:
            ERROR("Bad response type: %i", response->type);
            res = MafFailure;
            break;
        }
    }
    else {
        ERROR("Bad PM measure names response: %i", res);
    }

    /* Free response */
    bert_free((bert_term_t*) response);
    return res;
}


/**
 * This function returns all possible PM measurement names associated to a measured object.
 *
 * @param [in] dn MO distinguished name in 3GPP format. Example: "ManagedElement=1,MoWithCounters=2"
 *
 * @param [in|out] measurementNames
 *
 *         The result container contains:
 *             An array of all possible measurement names associated to this measured object
 *
 *         The result container should be allocated by the caller.
 *         The content of the container is allocated by the callee.
 *         The allocated memory for the content is released via the release function in the container.
 *         The container itself shall be released by the caller.
 *         The caller shall invoke the release function if set.
 *         The release function shall be provided by the SPI implementation.
 *         The implementer may set the function pointer to NULL to indicate that there is nothing to release.
 *
 * @return MafOk if the call was successful
 *         MafNotExist if a measured object does not exist
 *         MafFailure if an internal error occurred
 */
static MafReturnT getMeasurementNames
(const char * dn,
 ComOamSpiPmMeasuredObjectMeasurementNames_nT * measurementNames) {
    ENTER();
    MafReturnT res = MafOk;
    comte_con_handle_t connection;

    connection.quiet = 0;
    res = comte_connect(oam_comp->config->comte_ip,
                        oam_comp->config->comte_port,
                        &connection);
    if (res != MafOk) {
        ERROR("connect failed");
        return res;
    }

    comte_buff_t buff;
    res = encode_getPmMeasurementNames(dn, &buff);
    if (res != MafOk) {
        ERROR("encode failed");
        comte_disconnect(&connection);
        return res;
    }

    res = comte_send_recv(&buff, &connection);
    if (res != MafOk) {
        return res;
    }

    res = decode_getPmMeasurementNames(&buff, measurementNames);
    if (res != MafOk) {
        comte_free(buff.buff);
        comte_disconnect(&connection);
        return res;
    }

    comte_free(buff.buff);
    comte_disconnect(&connection);
    LEAVE();
    return res;
}



#if ((COM_PM_MEAS_H+0) >= 2)

static MafReturnT encode_getPmJobIds(const char * dn, comte_buff_t* buff){
    bert_term_t * args[] = { cstring(dn) };
    return encode_rpc("getPmJobIds", clist(1, args), buff);
}

static void releaseJobIds(ComOamSpiPmMeasuredObjectJobIds_nT *measJobIds) {
    ENTER();
    if (measJobIds->ids != NULL) {
        const char **ids;
        for (ids = measJobIds->ids;  *ids != NULL;  ids++) {
            comte_free((void *) *ids);
        }
        comte_free(measJobIds->ids);
    }
    LEAVE();
}

static MafReturnT decode_getPmJobIds
(comte_buff_t* buff,
 ComOamSpiPmMeasuredObjectJobIds_nT* measJobIds) {
    MafReturnT res;
    bert_term_t* response;

    ENTER();
    response = NULL;
    if ((res = decode_rpc(buff, &response)) != MafOk) {
        ERROR("Bad PM measure names response: %i", res);
        goto free;
    }
    measJobIds->ids = NULL;
    measJobIds->release = NULL;
    switch (response->type) {
    case BERT_NIL:
        measJobIds->release = releaseJobIds;
        goto free;
    case BERT_LIST: {
        bert_list_t *l;
        const char **ids;
        /* Response: [<<"JobId">>,...] */
        l = response->list;
        ids = comte_malloc(sizeof(*ids) * (bert_list_length(l)+1));
        measJobIds->ids = ids;
        for (;  l != NULL;  l = l->next, ids++) {
            if (l->value->type == BERT_BINARY) {
                *ids = copy_bert_binary(&l->value->binary);
                INFO("PM JobId: %s", *ids);
            }
            else {
                ERROR("Bad JobId type: %i", l->value->type);
                *ids = NULL;
                releaseJobIds(measJobIds);
                res = MafFailure;
                goto free;
            }
        }
        *ids = NULL;
        measJobIds->release = releaseJobIds;
        break;
    }
    default:
        ERROR("Bad response type: %i", response->type);
        res = MafFailure;
        break;
    }
 free:
    bert_free((bert_term_t*) response);
    LEAVE();
    return res;
}

/**
 * This function returns all PM measurement job ids associated to a measured object.
 *
 * @param [in] dn MO distinguished name in 3GPP format. Example: "ManagedElement=1,MoWithCounters=2"
 *
 * @param [in|out] measurementJobIds
 *
 *         The result container contains:
 *             An array of all PM job IDs associated to this measured object
 *
 *         The result container should be allocated by the caller.
 *         The content of the container is allocated by the callee.
 *         The allocated memory for the content is released via the release function in the container.
 *         The container itself shall be released by the caller.
 *         The caller shall invoke the release function if set.
 *         The release function shall be provided by the SPI implementation.
 *         The implementer may set the function pointer to NULL to indicate that there is nothing to release.
 *
 * @return MafOk if the call was successful
 *         MafNotExist if a measured object does not exist
 *         MafFailure if an internal error occurred
 */
static MafReturnT getPmJobIds
(const char * dn,
 ComOamSpiPmMeasuredObjectJobIds_nT* measurementJobIds) {
    MafReturnT res;
    comte_con_handle_t connection;
    comte_buff_t buff;

    ENTER();
    connection.quiet = 0;
    if ((res = comte_connect
         (oam_comp->config->comte_ip,
          oam_comp->config->comte_port,
          &connection)) != MafOk) {
        ERROR("connect failed");
        goto res;
    }
    if ((res = encode_getPmJobIds(dn, &buff)) != MafOk) {
        ERROR("encode failed");
        goto disconnect;
    }
    if ((res = comte_send_recv(&buff, &connection)) != MafOk) {
        goto disconnect;
    }
    if ((res = decode_getPmJobIds(&buff, measurementJobIds)) != MafOk) {
        ERROR("decode failed");
    }
    comte_free(buff.buff);
 disconnect:
    comte_disconnect(&connection);
 res:
    LEAVE();
    return res;
}

#endif


MafReturnT comte_pm_meas_create(comte_oam_component_t* comp) {
	oam_comp = comp;

	oam_comp->pm_meas =
		comte_malloc(sizeof(comte_pm_meas_t));
	comte_pm_meas_t* pm = comp->pm_meas;

        pm->base.base.componentName = OAM_COMPONENT_NAME;
        pm->base.base.interfaceName =
            ComOamSpiPmMeasurementsInterface_nId.interfaceName;
         pm->base.base.interfaceVersion=
            ComOamSpiPmMeasurementsInterface_nId.interfaceVersion;

	pm->base.getMeasurementValues = getMeasurementValues;
	pm->base.getMeasurementNames = getMeasurementNames;
#if ((COM_PM_MEAS_H+0) >= 2)
	pm->base.getPmJobIds = getPmJobIds;
#endif

	return MafOk;
}

MafReturnT comte_pm_meas_destroy(comte_oam_component_t* comp) {
	MafReturnT res = MafOk;

	comte_free(comp->pm_meas);

	return res;
}

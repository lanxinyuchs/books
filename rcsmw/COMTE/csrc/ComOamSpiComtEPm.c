/**
 * @author Kresimir Baksa
 * @created 2017-02-12
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <MafOamSpiServiceIdentities_1.h>
#include <ComOamSpiServiceIdentities_1.h>

#include "ComOamSpiComtEPm_1.h"
#include "ComOamSpiComtEPmMap_1.h"
#include "ComOamSpiComtEConverter_1.h"
#include "ComtEUtils_1.h"

comte_oam_component_t* oam_comp;
ComOamSpiComtEPmMap_T* pmMap;


/* ----------------------------------------------------
*  COM PM Measurements
*  ----------------------------------------------------
*/

/*
 * This method is used to create rpc callback for bert server in Erlang.
 */
static ComReturnT encode_getGp(ComOamSpiPmGpId_2T pmGpId, comte_buff_t* buff){
    // create list of arguments that are going to be sent Erlang bert server
    bert_term_t * args[] = { create_uint64(pmGpId) };

    MafReturnT encode_result = encode_rpc("getGp", clist(1, args), buff);
    if (encode_result != MafOk){
        return ComFailure;
    }
    return ComOk;
}

static MafReturnT decode_gp_aggregated_value
(bert_stuple_t bertDataTuple,
 ComOamSpiPmAggregatedValue_2T* values,
 uint32_t valuesIndex) {
    MafReturnT res = MafOk;

    if (bertDataTuple.size != 4){
        ERROR("Wrong tuple size in gpAggregatedValue: %d",
                bertDataTuple.size);
        return MafFailure;
    }

    /*
     * Check the gpAggregatedValue tuple types
     */
    bert_term_t** gpAggregatedValueTupleValues = bertDataTuple.values;
    if ((gpAggregatedValueTupleValues[0]->type != BERT_BINARY)&&(gpAggregatedValueTupleValues[0]->type != BERT_ATOM)) {
        ERROR("Wrong data type in gpAggregatedValue"
                "for measType. Expected binary, got: %d",
                gpAggregatedValueTupleValues[0]->type);
        return MafFailure;
    }

    if (gpAggregatedValueTupleValues[1]->type != BERT_LIST &&
        gpAggregatedValueTupleValues[1]->type != BERT_UINT64 &&
        gpAggregatedValueTupleValues[1]->type != BERT_SMALL_INTEGER &&
        gpAggregatedValueTupleValues[1]->type != BERT_INTEGER &&
        gpAggregatedValueTupleValues[1]->type != BERT_FLOAT) {
        ERROR("Wrong data type in gpAggregatedValue"
                "for value. Expected list or number, got: %d",
                gpAggregatedValueTupleValues[1]->type);
        return MafFailure;
    }

    ComOamSpiPmValueType_2T valueType = ComOamSpiPmValueType_2_NIL;
    switch ( gpAggregatedValueTupleValues[2]->type ) {
        case BERT_UINT64:
		valueType = gpAggregatedValueTupleValues[2]->uint64;
		break;
        case BERT_SMALL_INTEGER:
             valueType = gpAggregatedValueTupleValues[2]->sint;
             break;
	 default:
             ERROR("Wrong data type in gpAggregatedValue"
                "for valueType. Expected unsigned integer, got: %d",
                gpAggregatedValueTupleValues[2]->type);
             return MafFailure;
    }
    // if (gpAggregatedValueTupleValues[2]->type != BERT_UINT64) {
    //    ERROR("Wrong data type in gpAggregatedValue"
    //            "for valueType. Expected unsigned integer, got: %d",
    //            gpAggregatedValueTupleValues[2]->type);
    //    return MafFailure;
    //}

    if (gpAggregatedValueTupleValues[3]->type != BERT_BOOLEAN) {
        ERROR("Wrong data type in gpAggregatedValue"
                "for isSuspect. Expected boolean, got: %d",
                gpAggregatedValueTupleValues[3]->type);
        return MafFailure;
    }

    //uint32_t gpAggregatedValueTupleValuesSize =
    //        gpAggregatedValueTupleValues[0]->binary.length;
    bert_term_t * measType = gpAggregatedValueTupleValues[0];
    char *string = NULL;
    if (measType->type == BERT_BINARY)
    {
       string = copy_bert_binary (&measType->binary);
    }
    else if (measType->type == BERT_ATOM)
    {
       string = copy_bert_atom (measType);
    }
    INFO ("decode_ComOamSpiPmAggregatedValue %s", string);

    /*
     * Set gpAggregatedValue structure members
     */

    // Set gpAggregatedValue.measType
    ComOamSpiPmAggregatedValue_2T gpAggregatedValue;
    //char* measType = (char *) comte_malloc(
    //        (gpAggregatedValueTupleValuesSize + 1) * sizeof(char));
    //memset(measType, 0, gpAggregatedValueTupleValuesSize + 1);
    //memcpy(measType, gpAggregatedValueTupleValues[0], //TODO is this ok
    //        gpAggregatedValueTupleValuesSize);

    gpAggregatedValue.measType = string;

    // Count values list elements
    bert_list_t* listIterator;

    if ( gpAggregatedValueTupleValues[1]->type == BERT_LIST ) {
        uint32_t listSize = 0;
        for (listIterator = gpAggregatedValueTupleValues[1]->list;
            listIterator != NULL;
            listIterator = listIterator->next){
            listSize++;
        }

        // Set gpAggregatedValue.valueSize
        gpAggregatedValue.valueSize = listSize;

    } else {
        gpAggregatedValue.valueSize = 1;
    }
    // Set gpAggregatedValue.isSuspect
    gpAggregatedValue.isSuspect = gpAggregatedValueTupleValues[3]->boolean;

    // If list size is 0 than value is NULL and type is NIL
    if (gpAggregatedValue.valueSize == 0) {
        gpAggregatedValue.value.intArr = NULL; //TODO is this ok
        gpAggregatedValue.valueType = ComOamSpiPmValueType_2_NIL;
        values[valuesIndex] = gpAggregatedValue;
        return MafOk;
    }

    // Set gpAggregatedValue.valueType and gpAggregatedValue.value
    ComOamSpiPmValue_2T value;
    int valueTypeIsInt = 0;
    int valueTypeIsFloat = 0;
    int valueTypeIsIntArr = 0;
    int valueTypeIsFloatArr = 0;
    switch (valueType) {
        case ComOamSpiPmValueType_2_INT:
            gpAggregatedValue.valueType = ComOamSpiPmValueType_2_INT;
            valueTypeIsInt = 1;
            break;
        case ComOamSpiPmValueType_2_FLOAT:
            gpAggregatedValue.valueType = ComOamSpiPmValueType_2_FLOAT;
            valueTypeIsFloat = 1;
            break;
        case ComOamSpiPmValueType_2_INTARR:
            gpAggregatedValue.valueType = ComOamSpiPmValueType_2_INTARR;
            valueTypeIsIntArr = 1;
            break;
        case ComOamSpiPmValueType_2_FLOATARR:
            gpAggregatedValue.valueType = ComOamSpiPmValueType_2_FLOATARR;
            valueTypeIsFloatArr = 1;
            break;
        case ComOamSpiPmValueType_2_NIL:
            gpAggregatedValue.valueType = ComOamSpiPmValueType_2_NIL;
            gpAggregatedValue.value.intArr = NULL; //TODO check
            values[valuesIndex] = gpAggregatedValue;
            return MafOk;
            break;
        default:
            ERROR("Data type %ld not supported"
                    "for valueType in ComOamSpiPmAggregatedValue_2T."
                    "Expecting INTARR, FLOATARR or NIL",
                    gpAggregatedValueTupleValues[2]->uint64);
            free((char *)gpAggregatedValue.measType);
            return MafFailure;
            break;
    }

    // Set ComOamSpiPmValue_2T array
    if (valueTypeIsInt) {
        switch (gpAggregatedValueTupleValues[1]->type) {
            case BERT_UINT64:
                value.intVal = (int64_t) gpAggregatedValueTupleValues[1]->uint64;
                break;
            case BERT_SMALL_INTEGER:
                value.intVal = (int64_t) gpAggregatedValueTupleValues[1]->sint;
                break;
            case BERT_INTEGER:
                value.intVal = (int64_t) gpAggregatedValueTupleValues[1]->lint;
                break;
            default:
                ERROR("Unsupported data type in "
                        "gpAggregatedValueTupleValues bert term got %d",
                        gpAggregatedValueTupleValues[1]->type);
                return MafFailure;
                break;
        }
    }
    else if (valueTypeIsFloat) {
        value.floatVal = (double) gpAggregatedValueTupleValues[1]->sfloat;
    }
    else if (valueTypeIsIntArr) {
        value.intArr = (int64_t *) comte_malloc(
                gpAggregatedValue.valueSize * sizeof(int64_t));
    } else if (valueTypeIsFloatArr) {
        value.floatArr = (double *) comte_malloc(
                gpAggregatedValue.valueSize * sizeof(double));
    } else {
        free((char *)gpAggregatedValue.measType);
        ERROR("Data type for valueType in ComOamSpiPmAggregatedValue_2T"
                "must be intArr or floatArr.");
        return MafFailure;
    }

    if (valueTypeIsIntArr || valueTypeIsFloatArr){
        // Iterate through value array and store the values to gpAggregatedValue
        uint32_t index = 0;
        for (listIterator = gpAggregatedValueTupleValues[1]->list;
             listIterator != NULL;
             listIterator = listIterator->next){
            if (valueTypeIsIntArr) {
                value.intArr[index] = (int64_t) listIterator->value->lint;
            } else {
                value.floatArr[index] = (double) listIterator->value->sfloat;
            }
            index++;
        }
    }
    gpAggregatedValue.value = value;
    values[valuesIndex] = gpAggregatedValue;
    return res;
}

static MafReturnT decode_gp_instance
(bert_stuple_t bertDataTuple,
 ComOamSpiPmInstance_2T* instances,
 uint32_t instancesIndex) {
    MafReturnT res = MafOk;

    if (bertDataTuple.size != 2){
        ERROR("Wrong tuple size in gpInstance: %d",
                bertDataTuple.size);
        return MafFailure;
    }

    /*
     * Check the gpInstance tuple types
     */
    bert_term_t** gpInstanceTupleValues = bertDataTuple.values;

    if ((gpInstanceTupleValues[0]->type != BERT_BINARY) && (gpInstanceTupleValues[0]->type != BERT_ATOM)) {
        ERROR("Wrong data type in gpInstance"
                "for measObjLdn. Expected binary, got: %d",
                gpInstanceTupleValues[0]->type);
        return MafFailure;
    }

    if (gpInstanceTupleValues[1]->type != BERT_LIST) {
        ERROR("Wrong data type in gpInstance"
                "for values. Expected list, got: %d",
                gpInstanceTupleValues[1]->type);
        return MafFailure;
    }

    //uint32_t gpInstanceTupleValuesSize =
    //        gpInstanceTupleValues[0]->binary.length;

    bert_term_t * measObjLDN = gpInstanceTupleValues[0];
    char *string = NULL;
    if (measObjLDN->type == BERT_BINARY)
    {
        string = copy_bert_binary (&measObjLDN->binary);
    }
    else if (measObjLDN->type == BERT_ATOM)
    {
        string = copy_bert_atom (measObjLDN);
    }
    INFO ("decode_ComOamSpiPmInstance %s", string);

    /*
     * Set the gpInstance values
     */
    ComOamSpiPmInstance_2T gpInstance;

    // Set measObjLdn string
    //char* measObjLdn = (char *) comte_malloc(
    //        (gpInstanceTupleValuesSize + 1) * sizeof(char));
    //memset(measObjLdn, 0, gpInstanceTupleValuesSize + 1);
    //memcpy(measObjLdn, gpInstanceTupleValues[0],
    //        gpInstanceTupleValuesSize);

    gpInstance.measObjLDN = string;

    // Count values list elements
    uint32_t listSize = 0;
    bert_list_t* listIterator;

    for (listIterator = gpInstanceTupleValues[1]->list;
         listIterator != NULL;
         listIterator = listIterator->next){
        listSize++;
    }

    // Set values array size
    gpInstance.size = listSize;

    gpInstance.values = (ComOamSpiPmAggregatedValue_2T*)comte_malloc(
            listSize * sizeof(ComOamSpiPmAggregatedValue_2T));
    if (gpInstance.values == NULL) {
        ERROR("Could not allocate memory for gpInstance.values array");
        free((char *)gpInstance.measObjLDN);
        return MafFailure;
    }

    // Set values array
    uint32_t index = 0;
    for (listIterator = gpInstanceTupleValues[1]->list;
         listIterator != NULL;
         listIterator = listIterator->next){
        res = decode_gp_aggregated_value(listIterator->value->stuple,
                gpInstance.values,
                index);
        if (res != MafOk) {
            free(gpInstance.values);
            free((char *)gpInstance.measObjLDN);
            return res;
        }
        index++;
    }
    instances[instancesIndex] = gpInstance;
    return res;
}

static MafReturnT decode_gp_meas_obj_class
(bert_stuple_t bertDataTuple,
 ComOamSpiPmMeasObjClass_2T* measObjClasses,
 uint32_t measObjClassesIndex) {
    MafReturnT res = MafOk;

    if (bertDataTuple.size != 2){
        ERROR("Wrong tuple size in gpMeasObjClass: %d",
                bertDataTuple.size);
        return MafFailure;
    }

    /*
     * Check the gpMeasObjClass tuple types
     */
    bert_term_t** gpMeasObjClassTupleValues = bertDataTuple.values;

    if ((gpMeasObjClassTupleValues[0]->type != BERT_BINARY) && (gpMeasObjClassTupleValues[0]->type != BERT_ATOM)){
       ERROR("Wrong data type in gpMeasObjClass"
               "for measObjClass. Expected binary, got: %d",
              gpMeasObjClassTupleValues[0]->type);
       return MafFailure;
    }

    if (gpMeasObjClassTupleValues[1]->type != BERT_LIST){
        ERROR("Wrong data type in gpMeasObjClass"
                "for instances. Expected list, got: %d",
                gpMeasObjClassTupleValues[1]->type);
        return MafFailure;
    }


    //uint32_t gpMeasObjClassTupleValuesSize =
    //        gpMeasObjClassTupleValues[0]->binary.length;

    bert_term_t * measObjClass = gpMeasObjClassTupleValues[0];
    char *string = NULL;
    if (measObjClass->type == BERT_BINARY)
    {
        string = copy_bert_binary (&measObjClass->binary);
    }
    else if (measObjClass->type == BERT_ATOM)
    {
        string = copy_bert_atom (measObjClass);
    }
    INFO ("decode_ComOamSpiPmMeasObjClass   %s", string);

    /*
     * Set the gpMeasObjClass values
     */
    ComOamSpiPmMeasObjClass_2T gpMeasObjClass;

    // Set measObjClass string
    //char* measObjClass = (char *)comte_malloc(
    //        (gpMeasObjClassTupleValuesSize + 1)*sizeof(char));
    //memset(measObjClass, 0, gpMeasObjClassTupleValuesSize + 1);
    //memcpy(measObjClass, gpMeasObjClassTupleValues[0],
    //        gpMeasObjClassTupleValuesSize);

    gpMeasObjClass.measObjClass = string;

    // Count instances length
    uint32_t listSize = 0;
    bert_list_t* listIterator;

    for (listIterator = gpMeasObjClassTupleValues[1]->list;
         listIterator != NULL;
         listIterator = listIterator->next){
        listSize++;
    }

    // Set instances array size
    gpMeasObjClass.size = listSize;
    gpMeasObjClass.instances = (ComOamSpiPmInstance_2T*)comte_malloc(
            sizeof(ComOamSpiPmInstance_2T) * listSize);
    if (gpMeasObjClass.instances == NULL) {
        ERROR("Could not allocate memory for measObjClasses array");
        free((char *)gpMeasObjClass.measObjClass);
        return MafFailure;
    }

    // Set instances array
    uint32_t index = 0;
    for (listIterator = gpMeasObjClassTupleValues[1]->list;
         listIterator != NULL;
         listIterator = listIterator->next){
        res = decode_gp_instance(listIterator->value->stuple,
                gpMeasObjClass.instances,
                index);
        if (res != MafOk) {
            free(gpMeasObjClass.instances);
            free((char *)gpMeasObjClass.measObjClass);
            return res;
        }
        index++;
    }

    measObjClasses[measObjClassesIndex] = gpMeasObjClass;
    return res;
}

static MafReturnT decode_gp_data
(bert_stuple_t bertDataTuple, ComOamSpiPmGpData_2T *gpData) {
    MafReturnT res = MafOk;
    /*
     * Check the gpData tuple size.
     * It always consist of two elements:
     * list of measurement classes
     * suspect flag
     * {[MeasClass], IsSuspect}
     */
    if (bertDataTuple.size != 2){
        ERROR("Wrong tuple size in gpData: %d",
                    bertDataTuple.size);
        return MafFailure;
    }

    /*
     * Check the gpData tuple values
     */
    bert_term_t** gpDataTupleValues = bertDataTuple.values;

    // Check types
    if (gpDataTupleValues[0]->type != BERT_LIST &&
            gpDataTupleValues[0]->type != BERT_NIL) {
        ERROR("Wrong data type in gpData "
              "for MeasObjClasses. Expected list or nil, got: %d",
              gpDataTupleValues[0]->type);
        return MafFailure;
    }
    if (gpDataTupleValues[1]->type != BERT_BOOLEAN) {
        ERROR("Wrong data type in gpData "
              "for IsSuspect flag. Expected boolean, got: %d",
              gpDataTupleValues[1]->type);
        return MafFailure;
    }

    /*
     * Empty list is given. No need to decode measObjClasses.
     */
    if (gpDataTupleValues[0]->type == BERT_NIL) {
        gpData->size = 0;
        gpData->measObjClasses = NULL;
        gpData->isSuspect = gpDataTupleValues[1]->boolean;
        return res;
    }

    /*
     * Count the elements in the measObjClasses list
     */
    uint32_t valueListSize = 0;
    bert_list_t* listIterator;

    res = MafOk;
    for (listIterator = gpDataTupleValues[0]->list;
         listIterator != NULL;
         listIterator = listIterator->next){
        valueListSize++;
    }
    /*
     * Allocate memory for measObjClasses array
     */
    ComOamSpiPmMeasObjClass_2T* measObjClasses = (ComOamSpiPmMeasObjClass_2T*)
            comte_malloc(sizeof(ComOamSpiPmMeasObjClass_2T) * valueListSize);
    if (measObjClasses == NULL) {
        ERROR("Could not allocate memory for measObjClasses array");
        return MafFailure;
    }
    /*
     * Decode measObjClasses list
     */
    uint32_t index = 0;
    for (listIterator = gpDataTupleValues[0]->list;
         listIterator != NULL;
         listIterator = listIterator->next){
        res = decode_gp_meas_obj_class(
                listIterator->value->stuple,
                measObjClasses, index);
        if(res != MafOk){
            free(measObjClasses);
            return res;
        }
        index++;
    }

    gpData->size = valueListSize;
    gpData->measObjClasses = measObjClasses;
    gpData->isSuspect = gpDataTupleValues[1]->boolean;
    return res;
}

/*
 * This method is used for decoding Erlang response from bert server.
 */
static ComReturnT decode_getGp
(comte_buff_t* buff, ComOamSpiPmGpData_2T **gpData){
    // gpData = (ComOamSpiPmGpData_2T **)comte_malloc(sizeof(ComOamSpiPmGpData_2T *));
    MafReturnT res = MafFailure;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);

    /* Default values */
    *gpData = (ComOamSpiPmGpData_2T *)comte_malloc(sizeof(ComOamSpiPmGpData_2T));
    if (gpData == NULL) {
        return MafFailure;
    }

    ComOamSpiPmGpData_2T *gpDataResult = *gpData;

    // init values for gp data
    gpDataResult->measObjClasses = NULL;
    gpDataResult->size = 0;
    gpDataResult->isSuspect = false;

    if(res == MafOk){

        switch(response->type){
        case BERT_NIL: {
            /* [] */
            // Return NULL
            // gpDataResult->measObjClasses = NULL;
            // gpDataResult->size = 0;
            // gpDataResult->isSuspect = false;
            // gp data is already set
            break;
        }
        case BERT_SMALL_TUPLE: {
            /* {[{<<"MeasObjClass">>,
             *      [{<<"MeasObjLdn">>,
             *          [{<<"MeasType">>,
             *              100,
             *              ComOamSpiPmValueType_2_INT,
             *              false}]}]}],
             *    false} */
            res = decode_gp_data(response->stuple, gpDataResult);
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

/*
*This method is used to retrieve GP data (ComOamSpiPmGpData_2T) from the implementor of the SPI Note: It is not allowed to free the gpData pointer until releaseGp is called.
*
*
*@Parameters:
*	[in] 	pmGpId 	Identifier of the GP data that is to be retrieved
*	[out] 	gpData 	Pointer to the GP data
*@Returns:
*	ComOk, or ComFailure if an internal error occurred. ComNotExists if the pmGpId does not exist or it's lifetime has expired.
*/
static ComReturnT getGp
(ComOamSpiPmGpId_2T pmGpId, ComOamSpiPmGpData_2T **gpData){
    ENTER();
    ComReturnT res = ComOk;

    // connection handler
    comte_con_handle_t connection;

    connection.quiet = 0;
    res = comte_connect(oam_comp->config->comte_ip,
                        oam_comp->config->comte_port,
                        &connection);
    if (res != ComOk) {
        ERROR("connect failed");
        return res;
    }

    comte_buff_t buff;
    res = encode_getGp(pmGpId, &buff);
    if (res != ComOk) {
        ERROR("encode failed");
        comte_disconnect(&connection);
        return res;
    }

    res = comte_send_recv(&buff, &connection);
    if (res != ComOk) {
        return res;
    }

    res = decode_getGp(&buff, gpData);
    if (res != ComOk) {
        ERROR("decode failed");
        comte_free(buff.buff);
        comte_disconnect(&connection);
        return res;
    }

    insert(pmGpId, *gpData, pmMap);

    comte_free(buff.buff);
    comte_disconnect(&connection);
    LEAVE();
    INFO("Leave getGp");
    return res;
}


/*
*This method should be called to release the allocated memory for the gpData pointer retrieved with getGp method.
*
*Each registered consumer of the event "ComOamSpiPmEventTypeGpReady_2" have to call releaseGp for each ComOamSpiPmGpId_2T in those events, regardless if getGp() was called or not.
*
*@Parameters:
*	pmGpId 	[in] Identifier of the Gp value that can be deleted
*@Returns:
*	ComOk, or ComFailure if an internal error occurred. ComNotExists if the pmGpId does not exist.
*
*/
static ComReturnT releaseGp
(ComOamSpiPmGpId_2T pmGpId){
	ComReturnT res = ComOk;
	INFO("Enter releaseGp");
	removePm(pmGpId, pmMap);
	return res;
}


MafReturnT comte_pm_create(comte_oam_component_t* comp) {
	oam_comp = comp;

	oam_comp->pm =
		comte_malloc(sizeof(comte_pm_t));
	comte_pm_t* pm = comp->pm;

        pm->base.base.componentName = OAM_COMPONENT_NAME;
        pm->base.base.interfaceName =
            ComOamSpiPmInterface_2Id.interfaceName;
        pm->base.base.interfaceVersion=
            ComOamSpiPmInterface_2Id.interfaceVersion;

	pm->base.getGp = getGp;
	pm->base.releaseGp = releaseGp;

	pmMap = newMap();

	return MafOk;
}

MafReturnT comte_pm_destroy(comte_oam_component_t* comp) {
	MafReturnT res = MafOk;

	comte_free(comp->pm);

	clear(pmMap);
	comte_free(pmMap);

	return res;
}


/**
 * @author Kresimir Baksa
 * @created 2017-05-11
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "ComOamSpiComtEPmMap_1.h"

static ComOamSpiComtEPmMapElement_T* newElement(ComOamSpiPmGpId_2T id,
        ComOamSpiPmGpData_2T* data) {
    ComOamSpiComtEPmMapElement_T* new_element = (struct ComOamSpiComtEPmMapElement_T*)
            comte_malloc(sizeof(struct ComOamSpiComtEPmMapElement_T));
    if (new_element != NULL) {
        new_element->id = id;
        new_element->data = data;
        new_element->next = NULL;
        new_element->prev = NULL;
    }
    return new_element;
}

ComOamSpiComtEPmMap_T* newMap(){
    ComOamSpiComtEPmMap_T* map = (struct ComOamSpiComtEPmMap_T*)
            comte_malloc(sizeof(struct ComOamSpiComtEPmMap_T));
    if (map == NULL){
        return map;
    }
    map->size = 0;
    map->head = NULL;
    map->tail = NULL;
    return map;
}

int insert(ComOamSpiPmGpId_2T key, ComOamSpiPmGpData_2T* data,
        ComOamSpiComtEPmMap_T* pmap) {
    if (pmap == NULL) {
        return 0;
    }
    ComOamSpiComtEPmMapElement_T* new_element = newElement(key, data);
    if (new_element == NULL) {
        return 0;
    }

    if (pmap->size == 0){
        pmap->head = new_element;
        pmap->tail = new_element;
        new_element->prev = NULL;
        new_element->next = NULL;
        pmap->size += 1;
        return 1;
    }

    new_element->prev = pmap->tail;
    new_element->next = NULL;

    pmap->tail->next = new_element;
    pmap->tail = new_element;
    pmap->size += 1;
    return 1;
}

static void freeValues(int size, ComOamSpiPmAggregatedValue_2T* values) {
    if (values == NULL){
        return;
    }
    int i = 0;
    ComOamSpiPmAggregatedValue_2T* elem = NULL;
    while (size-- > 0) {
        elem = &values[i++];
        if (elem == NULL){
            continue;
        }
        if (elem->measType != NULL){
            comte_free((char*)elem->measType);
        }
        ComOamSpiPmValueType_2T type = elem->valueType;
        if (type == ComOamSpiPmValueType_2_INTARR) {
            comte_free(elem->value.intArr);
        }
        if (type == ComOamSpiPmValueType_2_FLOATARR) {
            comte_free(elem->value.floatArr);
        }
    }
}

static void freeInstances(int size, ComOamSpiPmInstance_2T* instances) {
    if (instances == NULL){
        return;
    }
    int i = 0;
    ComOamSpiPmInstance_2T* elem = NULL;
    while (size-- > 0) {
        elem = &instances[i++];
        if (elem == NULL){
            continue;
        }
        if (elem->measObjLDN != NULL){
            comte_free((char*)elem->measObjLDN);
        }
        freeValues(elem->size, elem->values);
        if (elem->values != NULL){
            comte_free(elem->values);
        }
    }
}

static void freeData(ComOamSpiPmGpData_2T* data) {
    if (data == NULL){
        return;
    }
    if (data->measObjClasses == NULL){
        comte_free(data);
        return;
    }
    int sizeMeasObjClasses = data->size;
    int i = 0;
    ComOamSpiPmMeasObjClass_2T* measObjClasses = data->measObjClasses;
    ComOamSpiPmMeasObjClass_2T* elem = NULL;
    while(sizeMeasObjClasses-- > 0) {
        elem = &measObjClasses[i++];
        if (elem == NULL){
            continue;
        }
        if (elem->measObjClass != NULL){
            comte_free((char*)elem->measObjClass);
        }
        freeInstances(elem->size, elem->instances);
        if (elem->instances != NULL){
            comte_free(elem->instances);
        }
    }
    comte_free(data->measObjClasses);
    comte_free(data);
}

static void freeElement(ComOamSpiComtEPmMapElement_T* data) {
    if (data == NULL){
        return;
    }
    if (data->data == NULL){
        comte_free(data);
        return;
    }
    freeData(data->data);
    comte_free(data);
}

void clear(ComOamSpiComtEPmMap_T* map) {
    if (map == NULL || map->size == 0){
        return;
    }

    ComOamSpiComtEPmMapElement_T* iterator = map->head;
    ComOamSpiComtEPmMapElement_T* next_iterator;
    while(iterator != NULL) {
        next_iterator = iterator->next;
        freeElement(iterator);
        iterator = next_iterator;
    }
    map->head = NULL;
    map->tail = NULL;
    map->size = 0;
}

int removePm(ComOamSpiPmGpId_2T key, ComOamSpiComtEPmMap_T* pmap) {
    if (pmap == NULL || pmap->size == 0) {
        return 0;
    }

    ComOamSpiComtEPmMapElement_T* iterator;

    if (pmap->size == 1){
        if (pmap->head->id == key){
            iterator = pmap->head;
            pmap->head = NULL;
            pmap->tail = NULL;
            pmap->size = 0;
            freeElement(iterator);
            return 1;
        }else{
            return 0;
        }
    }

    // head element needs to be removed from the list
    if (pmap->head->id == key){
        iterator = pmap->head;
        pmap->head = pmap->head->next;

        pmap->head->prev = NULL;
        pmap->size -= 1;
        freeElement(iterator);
        return 1;
    }
    // tail element needs to be removed from the list
    if (pmap->tail->id == key){
        iterator = pmap->tail;
        pmap->tail = pmap->tail->prev;

        pmap->tail->next = NULL;
        pmap->size -= 1;
        freeElement(iterator);
        return 1;
    }

    // inside node need to be removed
    iterator = pmap->head->next;
    while (iterator != NULL && iterator->next != NULL){
        if (iterator->id == key){
            iterator->prev->next = iterator->next;
            iterator->next->prev = iterator->prev;
            pmap->size -= 1;
            freeElement(iterator);
            return 1;
        }
        iterator = iterator->next;
    }

    return 0;
}

int containsKey(ComOamSpiComtEPmMap_T* map, ComOamSpiPmGpId_2T key) {
    if (map == NULL){
        return 0;
    }
    ComOamSpiComtEPmMapElement_T* iterator = map->head;

    while (iterator != NULL){
        if (iterator->id == key){
            return 1;
        }
        iterator = iterator->next;
    }
    return 0;
}

int isEmpty(ComOamSpiComtEPmMap_T* map) {
    if (map == NULL || map->size == 0) {
        return 1;
    }
    return 0;
}

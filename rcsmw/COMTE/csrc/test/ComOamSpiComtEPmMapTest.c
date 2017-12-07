
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "../ComtEUtils_1.h"
#include "../ComOamSpiComtEPmMap_1.h"

#include "ComOamSpiComtEPmMapTest.h"

ComOamSpiPmGpData_2T* createGpData() {
    ComOamSpiPmGpData_2T * data = (ComOamSpiPmGpData_2T*)
            malloc(sizeof(ComOamSpiPmGpData_2T));

    if (data == NULL) {
        return data;
    }
    data->measObjClasses = NULL;
    data->size = 0;
    return data;
}

void TestCollectionNew(CuTest* tc) {
    ComOamSpiComtEPmMap_T* pmMap = newMap();
    CuAssertTrue(tc, pmMap != NULL);
    CuAssertTrue(tc, pmMap->size == 0);
    CuAssertTrue(tc, pmMap->head == NULL);
    CuAssertTrue(tc, pmMap->tail == NULL);
    free(pmMap);
}

void TestSingleCollectionInsert(CuTest* tc) {
    ComOamSpiComtEPmMap_T* pmMap = newMap();
    CuAssertTrue(tc, pmMap != NULL);

    ComOamSpiPmGpId_2T pmGpId = 3;
    CuAssertTrue(tc, insert(pmGpId, createGpData(), pmMap) != 0);
    CuAssertTrue(tc, pmMap->size == 1);
    CuAssertTrue(tc, containsKey(pmMap, pmGpId) != 0);

    clear(pmMap);
    free(pmMap);
}

void TestMultipleCollectionInsert(CuTest* tc) {
    ComOamSpiComtEPmMap_T* pmMap = newMap();
    CuAssertTrue(tc, pmMap != NULL);

    ComOamSpiPmGpId_2T pmGpId;
    unsigned int size = 0;
    for(pmGpId = 0; pmGpId < 1000; pmGpId++){
        CuAssertTrue(tc, insert(pmGpId, createGpData(), pmMap) != 0);
        size ++;
        CuAssertTrue(tc, pmMap->size == size);
        CuAssertTrue(tc, containsKey(pmMap, pmGpId) != 0);
    }

    clear(pmMap);
    free(pmMap);
}

void TestSingleCollectionInsertAndRemove(CuTest* tc) {
    ComOamSpiComtEPmMap_T* pmMap = newMap();
    CuAssertTrue(tc, pmMap != NULL);

    ComOamSpiPmGpId_2T pmGpId = 3;
    CuAssertTrue(tc, insert(pmGpId, createGpData(), pmMap) != 0);
    CuAssertTrue(tc, pmMap->size == 1);
    CuAssertTrue(tc, containsKey(pmMap, pmGpId) != 0);

    CuAssertTrue(tc, removePm(pmGpId, pmMap) != 0);
    CuAssertTrue(tc, pmMap->size == 0);
    CuAssertTrue(tc, containsKey(pmMap, pmGpId) == 0);

    clear(pmMap);
    free(pmMap);
}

void TestMultipleCollectionInsertAndRemove(CuTest* tc) {
    ComOamSpiComtEPmMap_T* pmMap = newMap();
    CuAssertTrue(tc, pmMap != NULL);

    ComOamSpiPmGpId_2T pmGpId;
    unsigned int size = 0;
    for(pmGpId = 0; pmGpId <= 1000; pmGpId++){
        CuAssertTrue(tc, insert(pmGpId, createGpData(), pmMap) != 0);
        size ++;
        CuAssertTrue(tc, pmMap->size == size);
        CuAssertTrue(tc, containsKey(pmMap, pmGpId) != 0);
    }

    for(pmGpId = 0; pmGpId <= 1000; pmGpId++){
        CuAssertTrue(tc, removePm(pmGpId, pmMap) != 0);
        size --;
        CuAssertTrue(tc, pmMap->size == size);
        CuAssertTrue(tc, containsKey(pmMap, pmGpId) == 0);
    }

    clear(pmMap);
    free(pmMap);
}

void TestCollectionClear(CuTest* tc) {
    ComOamSpiComtEPmMap_T* pmMap = newMap();
    CuAssertTrue(tc, pmMap != NULL);

    ComOamSpiPmGpId_2T pmGpId;
    unsigned int size = 0;
    for(pmGpId = 0; pmGpId <= 1000; pmGpId++){
        CuAssertTrue(tc, insert(pmGpId, createGpData(), pmMap) != 0);
        size ++;
        CuAssertTrue(tc, pmMap->size == size);
        CuAssertTrue(tc, containsKey(pmMap, pmGpId) != 0);
    }

    clear(pmMap);
    CuAssertTrue(tc, isEmpty(pmMap) != 0);
    free(pmMap);
}

CuSuite* getComOamSpiComtEPmMapSuite(){
    CuSuite* suite = CuSuiteNew();

    SUITE_ADD_TEST(suite, TestCollectionNew);
    SUITE_ADD_TEST(suite, TestSingleCollectionInsert);
    SUITE_ADD_TEST(suite, TestMultipleCollectionInsert);
    SUITE_ADD_TEST(suite, TestSingleCollectionInsertAndRemove);
    SUITE_ADD_TEST(suite, TestMultipleCollectionInsertAndRemove);
    SUITE_ADD_TEST(suite, TestCollectionClear);

    return suite;
}

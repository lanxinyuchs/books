#ifndef ComOamSpiComtEPmMap_h
#define ComOamSpiComtEPmMap_h


#include "ComOamSpiComtEPm_1.h"


typedef struct ComOamSpiComtEPmMapElement_T ComOamSpiComtEPmMapElement_T;

struct ComOamSpiComtEPmMapElement_T {
    ComOamSpiPmGpId_2T id;
    ComOamSpiPmGpData_2T * data;
    ComOamSpiComtEPmMapElement_T * next;
    ComOamSpiComtEPmMapElement_T * prev;
};

typedef struct ComOamSpiComtEPmMap_T ComOamSpiComtEPmMap_T;
struct ComOamSpiComtEPmMap_T {
    unsigned int size;
    ComOamSpiComtEPmMapElement_T* head;
    ComOamSpiComtEPmMapElement_T* tail;
    //ComOamSpiComtEPmMap* parent;
    //ComOamSpiComtEPmMap* leftChild;
    //ComOamSpiComtEPmMap* rightChild;
};

ComOamSpiComtEPmMap_T* newMap();

void clear(ComOamSpiComtEPmMap_T* map);

int insert
(ComOamSpiPmGpId_2T key,
 ComOamSpiPmGpData_2T * data,
 ComOamSpiComtEPmMap_T* map);

int removePm
(ComOamSpiPmGpId_2T key,
 ComOamSpiComtEPmMap_T* map);

int containsKey
(ComOamSpiComtEPmMap_T* map,
 ComOamSpiPmGpId_2T key);

int isEmpty(ComOamSpiComtEPmMap_T* map);


#endif

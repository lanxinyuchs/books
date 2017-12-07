#include <string.h>
#include "ComOamSpiComtEUtils_1.h"
#include "ComtEUtils_1.h"

/* Attribute Value Container */
MafMoAttributeValueContainer_3T* create_avc(MafOamSpiMoAttributeType_3T type,
                                            uint16_t nrOfValues) {
    MafMoAttributeValueContainer_3T* container = comte_malloc(
        sizeof(MafMoAttributeValueContainer_3T));
    container->type = type;
    container->nrOfValues = nrOfValues;

    container->values = (nrOfValues > 0) ?
        comte_malloc(sizeof(MafMoAttributeValue_3T) * nrOfValues) :
        NULL;

    return container;
}

/* Named Attribute Value Container */
MafMoNamedAttributeValueContainer_3T* create_navc(uint8_t* attr_name,
                                                  uint32_t attr_name_length,
                                                  uint8_t type,
                                                  uint16_t nrOfValues) {
    MafMoNamedAttributeValueContainer_3T* container = comte_malloc(
        sizeof(MafMoNamedAttributeValueContainer_3T));

    /* Container name */
    char* name = comte_malloc(sizeof(char) * attr_name_length+1);
    strncpy(name, (char*)attr_name, attr_name_length);
    name[attr_name_length] = '\0';
    container->name = name;

    if(nrOfValues > 0){
        /* Attribute-value container */
        container->value.type = type;
        container->value.nrOfValues = nrOfValues;
        container->value.values = comte_malloc(sizeof(MafMoAttributeValue_3T) * nrOfValues);
    } else {
        container->value.nrOfValues = 0;
        container->value.type = 0;
        container->value.values = NULL;
    }
    return container;
}


void destroy_avc_opt(MafMoAttributeValueContainer_3T* container, bool freeContainer) {
    if (container->type == MafOamSpiMoAttributeType_3_STRING
	    || container->type == MafOamSpiMoAttributeType_3_REFERENCE
	    || container->type == MafOamSpiMoAttributeType_3_STRUCT) {

	int i;
	for (i = 0; i < container->nrOfValues; i++) {
	    if (container->type == MafOamSpiMoAttributeType_3_STRING){

                if(container->values[i].value.theString != NULL){
                    char* ths = (char *) container->values[i].value.theString;
                    comte_free(ths);
                }

            }
	    if (container->type == MafOamSpiMoAttributeType_3_REFERENCE){
                if(container->values[i].value.moRef != NULL){
                    char* mor = (char *) container->values[i].value.moRef;
                    comte_free(mor);
                }
            }

	    if (container->type == MafOamSpiMoAttributeType_3_STRUCT) {
		struct MafMoAttributeValueStructMember_3 *curr =
			container->values[i].value.structMember, *prev;
		while (curr != NULL ) {
		    comte_free(curr->memberName);
		    destroy_avc(curr->memberValue);
		    prev = curr;
		    curr = curr->next;
		    comte_free(prev);
		}
	    }
	}
    }

    if(container->values != NULL)
        comte_free(container->values);

    /* NOT a pointer in a named container */
    if(freeContainer)
        comte_free(container);


}

void destroy_avc(MafMoAttributeValueContainer_3T* container) {
    destroy_avc_opt(container, true);
}

void destroy_navc(MafMoNamedAttributeValueContainer_3T* namedContainer) {

    /* Free container name */
    char* name = (char *)namedContainer->name;
    comte_free(name);

    /* Free AVC container */
    destroy_avc_opt(&namedContainer->value, false);

    /* Free named container */
    comte_free(namedContainer);

}

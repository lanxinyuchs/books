/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "ComMwSpiComtEConverter_1.h"
#include "ComtEUtils_1.h"
#include "bert.h"



MafReturnT encode_getRoles(const char *user, comte_buff_t* buff) {
	bert_term_t* args[] = { cstring(user) };
	return encode_rpc("getRoles", clist(1, args), buff);
}

MafReturnT decode_getRoles(comte_buff_t* buff,
                           MafMwSpiAccessManagementRoleT** roles) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk){

            switch(response->type){

            case BERT_LIST: {
                int num_roles = bert_list_length(response->list), i;
                *roles =
                    comte_malloc(sizeof(MafMwSpiAccessManagementRoleT*)
                                 * (num_roles+1));

                if(*roles){

                    bert_list_t* item = response->list;
                    char **curr_role = (char**)(*roles);

                    for (i = 0; i < num_roles; i++) {
                        if (item->value->type != BERT_BINARY) {
                            comte_free(*roles);
                            res = MafFailure;
                            break;
                        }
                        char *role = copy_bert_binary(&item->value->binary);
                        *curr_role = role;
                        curr_role++;
                        item = item->next;
                    }
                    *((*roles)+num_roles) = NULL;
                }
                else {
                    ERROR("Allocation error!");
                    res = MafFailure;
                }
                break;
            }

            default:
                ERROR("Bad response type: %i", response->type);
                res = MafFailure;
                break;
            }
        }

        bert_free(response);
	return res;
}

/**
 * Availability Controller encode/decode
 */


MafReturnT encode_acInitialize(comte_buff_t *buff) {
    bert_term_t *args[] = { csbig(getpid()) };
    return encode_rpc("acInitialize", clist(1, args), buff);
}

MafReturnT decode_acInitialize(comte_buff_t *buff) {
    return decode_ok_response(buff);
}

MafReturnT encode_healthCheckReport(MafReturnT healthReport,
                                    MafMwSpiRecommendedRecoveryT recommendedRecovery,
                                    comte_buff_t *buff) {
    bert_term_t* args[] = { csbig(healthReport), csint(recommendedRecovery) };
    return encode_rpc("healthCheckReport", clist(2, args), buff);
}

MafReturnT decode_healthCheckReport(comte_buff_t *buff) {
    return decode_ok_response(buff);
}

MafReturnT encode_haModeAssumed(MafReturnT error, comte_buff_t *buff) {
    bert_term_t* args[] = { csbig(error) };
    return encode_rpc("haModeAssumed", clist(1, args), buff);
}

MafReturnT decode_haModeAssumed(comte_buff_t *buff) {
    return decode_ok_response(buff);
}

MafReturnT encode_prepareTerminationResponse(MafReturnT error,
                                             comte_buff_t *buff) {
    bert_term_t* args[] = { csbig(error) };
    return encode_rpc("prepareTerminationResponse", clist(1, args), buff);
}
MafReturnT decode_prepareTerminationResponse(comte_buff_t *buff) {
    return decode_ok_response(buff);
}

/**
 * Replicated list encode/decode
 */
typedef struct buff_size_list buff_size_list_t;

struct buff_size_list {
	uint32_t buffSize;
	MafMwSpiListNameT* listInstanceName;
	buff_size_list_t* next;
};

static buff_size_list_t* buffSizeList = NULL;

uint32_t comte_rl_getBuffSize(const MafMwSpiListNameT* listInstanceName) {
	buff_size_list_t* curr = buffSizeList;

	while(curr) {
		if(curr->listInstanceName->length == listInstanceName->length &&
				memcmp(curr->listInstanceName->value,
                                       listInstanceName->value,
                                       listInstanceName->length) == 0)
			break;
		curr = curr->next;
	}

	if (!curr)
            return 0; // Fetch from ComtE
	else
            return curr->buffSize;
}

void comte_rl_addBuffSize(const MafMwSpiListNameT* listInstanceName, uint32_t size) {
	buff_size_list_t* new = comte_malloc(sizeof(buff_size_list_t));

	new->buffSize = size;
	new->listInstanceName = malloc(sizeof(MafMwSpiListNameT));
	new->listInstanceName->length = listInstanceName->length;
	memcpy(new->listInstanceName->value, listInstanceName->value, listInstanceName->length);
	new->next = buffSizeList;
	buffSizeList = new;
}

MafReturnT comte_rl_deleteBuffSize(const MafMwSpiListNameT* listInstanceName) {
	buff_size_list_t* curr = buffSizeList, *prev = NULL;

	while(curr) {
	    if(curr->listInstanceName->length == listInstanceName->length &&
				memcmp(curr->listInstanceName->value,
                                       listInstanceName->value,listInstanceName->length) == 0)
			break;
	    prev = curr;
	    curr = curr->next;
	}

	if (!curr)
	    return 0; // Could not find list?!?
	else {
	    prev->next = curr->next;
	    comte_free(curr->listInstanceName);
	    comte_free(curr);
	    return 1;
	}
}

MafReturnT decode_listOperation(comte_buff_t* buff) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

	if (res == MafOk) {
            switch(response->type){

            case BERT_ATOM: {
		if (strcmp(response->atom, "eexist") == 0){
                    res = MafAlreadyExist;
                }
		else if (strcmp(response->atom, "enoent") == 0){
                    res = MafNotExist;
                }
                else if (strcmp(response->atom, "ok") == 0){
                    /* Leave as MafOk */
                }
                else {
                    ERROR("Bad response: %s", response->atom);
                    res = MafFailure;
                }
                break;
            }
            default:
                ERROR("Bad response type: %i", response->type);
                res = MafFailure;
                break;
            }
	}

	bert_free(response);
	return res;
}

MafReturnT encode_listOperation(char* op,
		const MafMwSpiListNameT* listInstanceName, comte_buff_t* buff) {
	bert_term_t* args[] = { cbinary(listInstanceName->length-1,
			listInstanceName->value) };
	return encode_rpc(op, clist(1, args), buff);
}

MafReturnT encode_listCreate(const MafMwSpiListNameT* listInstanceName,
		uint32_t dataBufferSize, comte_buff_t* buff) {
	comte_rl_addBuffSize(listInstanceName, dataBufferSize);
	bert_term_t* args[] = { cbinary(listInstanceName->length-1,
			listInstanceName->value), csbig(dataBufferSize) };
	return encode_rpc("listCreate", clist(2, args), buff);
}

MafReturnT decode_listCreate(comte_buff_t* buff) {
	return decode_listOperation(buff);
}

MafReturnT encode_listDelete(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff) {
	return encode_listOperation("listDelete", listInstanceName, buff);
}

MafReturnT decode_listDelete(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff) {
	MafReturnT res = decode_listOperation(buff);
	if (res == MafOk) {
            if (!comte_rl_deleteBuffSize(listInstanceName)) return MafOk;
            else return MafFailure;
	} else
	    return res;
}

MafReturnT encode_listClear(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff) {
	return encode_listOperation("listClear", listInstanceName, buff);
}

MafReturnT decode_listClear(comte_buff_t* buff) {
	return decode_listOperation(buff);
}

MafReturnT encode_listGetSize(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff) {
	return encode_listOperation("listGetSize", listInstanceName, buff);
}

MafReturnT decode_listGetSize(comte_buff_t* buff, uint32_t* listSize) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk){
            switch (response->type) {
            case BERT_SMALL_INTEGER: {
		*listSize = (uint32_t) response->sint;
		break;
            }
            case BERT_INTEGER: {
		*listSize = response->lint;
		break;
            }
            case BERT_ATOM: {
		if (strcmp(response->atom, "enoent") == 0){
                    res = MafNotExist;
                }
                else {
                    ERROR("Bad response: %s", response->atom);
                    res = MafFailure;
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

	bert_free(response);
	return res;
}

MafReturnT encode_listIsEmpty(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff) {
	return encode_listOperation("listIsEmpty", listInstanceName, buff);
}

MafReturnT decode_listIsEmpty(comte_buff_t* buff, bool* listEmpty) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk){
            switch(response->type){

            case BERT_BOOLEAN: {
                *listEmpty = response->boolean;
                break;
            }

            case BERT_ATOM: {
                if(strcmp(response->atom, "enoent") == 0)
                    res = MafNotExist;
                else {
                    ERROR("Bad response: %s", response->atom);
                    res = MafFailure;
                }
                break;
            }

            default:
                ERROR("Bad response type: %i", response->type);
                res = MafFailure;
                break;
            }

        }

        bert_free(response);
        return res;
}

MafReturnT encode_listGetFrontRef(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff) {
	return encode_listOperation("listGetFrontRef", listInstanceName, buff);
}

MafReturnT decode_listGetFrontRef(comte_buff_t* buff,
                                  comte_replicated_list_t* list) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        list->first = NULL;
        list->curr = NULL;

        if(res == MafOk){
            switch(response->type){

            case BERT_NIL: {
                /* Leave list fields as NULL */
                break;
            }

            case BERT_ATOM: {
                if (strcmp(response->atom, "enoent") == 0){
                    res = MafNotExist;
                }
                else {
                    ERROR("Bad response: %s", response->atom);
                    res = MafFailure;
                }
                break;
            }

            case BERT_LIST: {
                list->curr = response->list;
                list->first = response->list;
                response->list = NULL;
                break;
            }

            default:
                ERROR("Bad response type: %i", response->type);
                res = MafFailure;
                break;
            }
        }

	bert_free(response);
	return res;
}

MafReturnT encode_listPushBack(const MafMwSpiListNameT* listInstanceName,
                               void* newItemDataBuffer, comte_buff_t* buff) {
	uint32_t buffSize = comte_rl_getBuffSize(listInstanceName);
	bert_term_t* args[] = { cbinary(listInstanceName->length-1,
			listInstanceName->value), cbinary(buffSize, newItemDataBuffer) };
	return encode_rpc("listPushBack", clist(2, args), buff);
}

MafReturnT decode_listPushBack(comte_buff_t* buff,
                               MafMwSpiListItemNameT* listItemName) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk) {
            switch(response->type){

            case BERT_ATOM: {
                if(strcmp(response->atom, "enoent") == 0){
                    res = MafNotExist;
                }
                else {
                    ERROR("Bad response: %s", response->atom);
                    res = MafFailure;
                }
                break;
            }

            case BERT_BINARY: {
                listItemName->length = (uint16_t)response->binary.length;
                memcpy(listItemName->value,
                       response->binary.value,
                       response->binary.length);
                break;
            }

            default:
                ERROR("Bad response type: %i", response->type);
                res = MafFailure;
                break;
            }

        }


	bert_free(response);
	return res;
}

MafReturnT encode_listPopBack(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff) {
	return encode_listOperation("listPopBack", listInstanceName, buff);
}

MafReturnT decode_listPopBack(comte_buff_t* buff) {
	return decode_listOperation(buff);
}

MafReturnT encode_listEraseItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, comte_buff_t* buff) {
	bert_term_t* args[] = { cbinary(listInstanceName->length-1,
			listInstanceName->value), cbinary(listItemName->length,
			listItemName->value) };
	return encode_rpc("listEraseItem", clist(2, args), buff);
}

MafReturnT decode_listEraseItem(comte_buff_t* buff) {
	return decode_listOperation(buff);
}

MafReturnT encode_listFindItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, comte_buff_t* buff) {
	bert_term_t* args[] = { cbinary(listInstanceName->length-1,
			listInstanceName->value), cbinary(listItemName->length,
			listItemName->value) };
	return encode_rpc("listFindItem", clist(2, args), buff);

}

MafReturnT decode_listFindItem(comte_buff_t* buff, void* copyOfItemData) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk) {
            switch(response->type){

            case BERT_ATOM: {
                if(strcmp(response->atom, "enoent") == 0){
                    res = MafNotExist;
                }
                else {
                    ERROR("Bad response: %s", response->atom);
                    res = MafFailure;
                }
                break;
            }

            case BERT_BINARY: {
                memcpy(copyOfItemData, response->binary.value,
                       response->binary.length);
                break;
            }

            default:
                ERROR("Bad response: %i", response->type);
                res = MafFailure;
                break;
            }

        }

	bert_free(response);
	return res;
}

MafReturnT encode_listReplaceItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, const void* replaceItemData,
		comte_buff_t* buff) {
	uint32_t buffSize = comte_rl_getBuffSize(listInstanceName);
	bert_term_t* args[] = { cbinary(listInstanceName->length-1,
			listInstanceName->value), cbinary(listItemName->length,
			listItemName->value), cbinary(buffSize, replaceItemData) };
	return encode_rpc("listReplaceItem", clist(3, args), buff);
}

MafReturnT decode_listReplaceItem(comte_buff_t* buff) {
	return decode_listOperation(buff);
}

MafReturnT encode_listNumberOfListInstances(comte_buff_t* buff) {
	bert_term_t *nil = comte_malloc(sizeof(bert_term_t));
	nil->type = BERT_NIL;
	return encode_rpc("listNumberOfListInstances", nil, buff);
}

MafReturnT decode_listNumberOfListInstances(comte_buff_t* buff,
		uint32_t* numberOfLinkListInstances) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk){
            switch (response->type) {

            case BERT_SMALL_INTEGER: {
		*numberOfLinkListInstances = (uint32_t) response->sint;
		break;
            }

            case BERT_INTEGER: {
		*numberOfLinkListInstances = response->lint;
		break;
            }

            default: {
                ERROR("Bad response type: %i", response->type);
		res = MafFailure;
                break;
            }
            }
        }

	bert_free(response);
	return res;
}

MafReturnT encode_listMemoryUsage(comte_buff_t* buff) {
	bert_term_t *nil = comte_malloc(sizeof(bert_term_t));
	nil->type = BERT_NIL;
	return encode_rpc("listMemporyUsage", nil, buff);
}

MafReturnT decode_listMemoryUsage(comte_buff_t* buff, uint32_t* memoryUsed,
		uint32_t* totalMemoryAvailable) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk){
            if(response->type == BERT_SMALL_TUPLE){
                switch (response->stuple.values[0]->type) {
                case BERT_SMALL_INTEGER: {
                    *memoryUsed = (uint32_t) response->stuple.values[0]->sint;
                    break;
                }
                case BERT_INTEGER: {
                    *memoryUsed = response->stuple.values[0]->lint;
                    break;
                }
                default: {
                    ERROR("Bad response type: %i", response->stuple.values[0]->type);
                    res = MafFailure;
                    break;
                }
                }
            }
            else {
                ERROR("Bad response type: %i", response->type);
                res = MafFailure;
            }
        }


	bert_free(response);
	return res;
}

/* Crypto SPI */

MafReturnT encode_encrypt(const char *string, comte_buff_t* buff) {
    bert_term_t* args[] = { cstring(string) };
    return encode_rpc("encrypt", clist(1, args), buff);
}

MafReturnT decode_encrypt(comte_buff_t* buff, char** encryptedString) {
    MafReturnT res = MafOk;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);


    if(res == MafOk){
        if(response->type == BERT_BINARY){
            *encryptedString = copy_bert_binary(&response->binary);
        }
        else {
            ERROR("Bad response type: %i", response->type);
            res = MafFailure;
        }
    }

    bert_free(response);
    return res;
}


MafReturnT encode_decrypt(const char *encryptedString, comte_buff_t* buff) {
    bert_term_t* args[] = { cstring(encryptedString) };
    return encode_rpc("decrypt", clist(1, args), buff);
}

MafReturnT decode_decrypt(comte_buff_t* buff, char** string) {
    MafReturnT res = MafOk;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);

    if(res == MafOk){
        if(response->type == BERT_BINARY){
            *string = copy_bert_binary(&response->binary);
        }
        else {
            ERROR("Bad response type: %i", response->type);
            res = MafFailure;
        }
    }

    bert_free(response);
    return res;
}



MafReturnT encode_logWrite(uint32_t eventId, MwSpiSeverityT severity,
		MwSpiFacilityT facility, const char *databuffer, comte_buff_t *buff) {
	bert_term_t *args[4];

	args[0] = csbig(eventId);
	args[1] = clint(severity);
	args[2] = clint(facility);
	args[3] = cstring(databuffer);
	return encode_rpc("logWrite", clist(4, args), buff);
}

MafReturnT decode_logWrite(comte_buff_t *buff) {
    return decode_ok_response(buff);
}

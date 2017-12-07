/**
 * @author Lukas Larsson
 * @created 2011-04-13
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <MafMwSpiServiceIdentities_1.h>

#include "ComMwSpiComtEComponent_1.h"
#include "ComMwSpiComtEReplicatedList_1.h"
#include "ComMwSpiComtEConverter_1.h"
#include "ComtEUtils_1.h"

comte_mw_component_t* rl_component = NULL;

MafReturnT comte_rl_free_list(comte_replicated_list_t* list) {
	// Inorder to use bert_free we have to create the surrounding bert_term_t
	bert_term_t* term = comte_malloc(sizeof(bert_term_t));
	term->type = BERT_LIST;
	term->list = list->first;
	bert_free(term);

	comte_free(list);
	return MafOk;
}

/**
 * Creates a new list instance associated with a cluster wide unique name.
 *
 * @param[in] listInstanceName Pointer to a parameter which associate the created instance of
 * the link list with a cluster wide unique name.
 *
 * @param[in] dataBufferSize The size in bytes for each item in the created link list
 * instance.
 *
 * @return MafOk, or @n
 * MafAlreadyExist if the linked list instance name already exists, or @n
 * MafNoResources if no memory is available and there is a reasonable
 * chance that COM can continue execution.
 */
static MafReturnT listCreate(const MafMwSpiListNameT* listInstanceName,
		uint32_t dataBufferSize) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listCreate(listInstanceName, dataBufferSize, buff),
			decode_listCreate(buff));
}

/**
 * Removes all linked list items, deletes the complete link list instance and free all used memory.
 * The listInstanceName associated with the link list instance is also removed.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @return MafOk, or MafNotExist if the linked list instance name does not exist.
 */
static MafReturnT listDelete(const MafMwSpiListNameT* listInstanceName) {
	GENERIC_BERT_RPC(rl_component->config,
			 encode_listDelete(listInstanceName, buff),
			 decode_listDelete(listInstanceName, buff));
}

/**
 * Removes all items in the linked list instance.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @return MafOk, or MafNotExist if the linked list instance name does not exist.
 */
static MafReturnT listClear(const MafMwSpiListNameT* listInstanceName) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listClear(listInstanceName, buff), decode_listClear(buff));
}

/**
 * Returns the number of items in the linked list instance.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[out] listSize Pointer to the size data, that is the number of items in the link list instance.
 *
 * @return MafOk, or MafNotExist if the linked list instance name does not exist.
 */
static MafReturnT listGetSize(const MafMwSpiListNameT* listInstanceName,
		uint32_t* listSize) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listGetSize(listInstanceName, buff),
			decode_listGetSize(buff,listSize));
}

/**
 * Verifies if the linked list instance contains items.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[out] listEmpty Pointer to a boolean parameter which is true if the link list instance contains no items.
 *
 * @return MafOk, or MafNotExist if the linked list instance name does not exist.
 */
static MafReturnT listIsEmpty(const MafMwSpiListNameT* listInstanceName,
		bool* listEmpty) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listIsEmpty(listInstanceName, buff),
			decode_listIsEmpty(buff,listEmpty));
}

// List Data Access methods

/**
 * Adds a new item to the end of the link list instance.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[out] listItemName Pointer to a local linked list instance wide unique item name.
 *
 * @param[in] newItemDataBuffer Pointer to a data buffer containing the new data item.
 *
 * @return MafOk, or @n
 * MafNotExist if the linked list instance name does not exist, or @n
 * MafNoResources if no memory is available and there is a reasonable
 * chance that COM can continue execution.
 */
static MafReturnT listPushBack(const MafMwSpiListNameT* listInstanceName,
		MafMwSpiListItemNameT* listItemName, void* newItemDataBuffer) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listPushBack(listInstanceName, newItemDataBuffer, buff),
			decode_listPushBack(buff,listItemName));
}

/**
 * Removes the last item in the linked list instance.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @return MafOk, or MafNotExist if the linked list instance name does not exists.
 */
static MafReturnT listPopBack(const MafMwSpiListNameT* listInstanceName) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listPopBack(listInstanceName, buff),
			decode_listPopBack(buff));
}

/**
 * Removes the item in the linked list instance pointed out by listItemName.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[in] listItemName Pointer to a local link list instance wide unique item name.
 *
 * @return MafOk, or MafNotExist if the linked list instance name or the item name does not exist.
 */
static MafReturnT listEraseItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listEraseItem(listInstanceName, listItemName, buff),
			decode_listEraseItem(buff));
}

/**
 * Returns a reference to the first item in the link list instance.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[out] listInstanceFrontRef Pointer to the first item in the link list instance. If
 * no item exist, null is returned.
 *
 * @return MafOk, or MafNotExist if the linked list instance name does not exist.
 */
static MafReturnT listGetFrontRef(const MafMwSpiListNameT* listInstanceName,
		MafMwSpiListItemRefT* listInstanceFrontRef) {
	ENTER();
	comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
	MafReturnT res = encode_listGetFrontRef(listInstanceName, buff);
	if (res != MafOk) {
		ERROR("encode failed!");
		*listInstanceFrontRef = NULL;
		return res;
	}

	res = comte_connect_send_recv
	    (rl_component->config->comte_ip,
	     rl_component->config->comte_port, buff, 0);
	if (res != MafOk) {
            ERROR("comte_rl_send_recv failed!");
            *listInstanceFrontRef = NULL;
            return res;
	}
	comte_replicated_list_t* list = comte_malloc(
			sizeof(comte_replicated_list_t));
	res = decode_listGetFrontRef(buff, list);
	if(res != MafOk || list->curr == NULL) {
		*listInstanceFrontRef = NULL;
		comte_free(list);
	}
	else
		*listInstanceFrontRef = (MafMwSpiListItemRefT*) list;
	comte_free(buff->buff);
	comte_free(buff);
	LEAVE();
	return res;
}

/**
 * Retrieves a copy a linked list instance data item pointed out by currentItemRef.
 * The output value of currentItemRef points to the next item in the linked list instance.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[in,out] currentItemRef Reference to an item in the linked list instance.
 *
 * @li In value is pointing to the current item in the linked list instance.
 * @li Out value is pointing to the next item in the linked list instance. If the value is null, the end of the list is reached.
 * @li The initial value of this parameter must be set by the function @em mwSpiListGetFrontRef().
 *
 * @param[out] listItemName Pointer to the name of the copied data item. The name
 * is unique within the linked list.
 *
 * @param[out] copyOfItemData Pointer to a data buffer containing a copy of the
 * linked list instance data item pointed out by the input value of currentItemRef.
 * The buffer is provided by the interface client.
 *
 * @return MafOk, or MafNotExist if the linked list instance name does not exist.
 */
static MafReturnT listGetNextItemFront(
		const MafMwSpiListNameT* listInstanceName,
		MafMwSpiListItemRefT* currentItemRef,
		MafMwSpiListItemNameT* listItemName, void* copyOfItemData) {
	ENTER();
	comte_replicated_list_t* list = (comte_replicated_list_t*) *currentItemRef;

	if (list->curr->value->type != BERT_SMALL_TUPLE)
		return MafFailure;

	bert_stuple_t tuple = list->curr->value->stuple;
	if (tuple.values[0]->type != BERT_BINARY
			|| tuple.values[1]->type != BERT_BINARY)
		return MafFailure;

	memcpy(listItemName->value, tuple.values[0]->binary.value,
			tuple.values[0]->binary.length);
	listItemName->length = tuple.values[0]->binary.length;

	memcpy(copyOfItemData, tuple.values[1]->binary.value,
			tuple.values[1]->binary.length);

	list->curr = list->curr->next;

	if (list->curr == NULL) {

		comte_rl_free_list(list);
		*currentItemRef = NULL;
	}

	LEAVE();
	return MafOk;
}

/**
 * Removes the memory associated with currentItemRef. This function should
 * be called after the last call to listGetNextItemFront if the end of the
 * list was not reached, that is if the currentItemRef is not NULL.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 * @param[in] currentItemRef Reference to an item in the linked list instance.
 *
 * @return MafOk, or MafNotExists if the linked list instance name or currentItemRef does not exist.
 */
static MafReturnT listGetFinalize(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemRefT currentItemRef) {
	ENTER();
	comte_rl_free_list((comte_replicated_list_t*) currentItemRef);
	LEAVE();
	return MafOk;
}

/**
 * Traverses the list and retrieves a copy of a linked list instance data item pointed out by listItemName.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[in] listItemName Pointer to the local link list instance wide unique item
 * name for the searched data item.
 *
 * @param[out] copyOfItemData Pointer to a data buffer containing a copy of the
 * linked list instance data item pointed out by listItemName.
 * The buffer is provided by the interface client.
 *
 * @return MafOk, or MafNotExist if the linked list instance name or listItemName does not exist.
 */
static MafReturnT listFindItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, void* copyOfItemData) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listFindItem(listInstanceName, listItemName, buff),
			decode_listFindItem(buff, copyOfItemData));
}

/**
 * Traverses the list and replaces a linked list instance data item pointed out by listItemName.
 * The old item is no longer available.
 *
 * @param[in] listInstanceName Pointer to a unique name for the linked list instance.
 *
 * @param[in] listItemName Pointer to the local link list instance wide unique item
 * name for the searched data item.
 *
 * @param[in] replaceItemData Pointer to a data buffer containing the data item used
 * to replace the existing data item. The buffer is provided by the interface client.
 *
 * @return MafOk, or MafNotExist if the linked list instance name or listItemName does not exist.
 */
static MafReturnT listReplaceItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, const void* replaceItemData) {
	GENERIC_BERT_RPC(
			rl_component->config,
			encode_listReplaceItem(listInstanceName, listItemName, replaceItemData, buff),
			decode_listReplaceItem(buff));
}

// List Utility methods

/**
 * Returns the current number of link lists.
 *
 * @param[out] numberOfLinkListInstances Pointer to the parameter containing the current
 * number of linked list instances.
 *
 * @return In principle always MafOk.
 */
static MafReturnT listNumberOfListInstances(uint32_t* numberOfLinkListInstances) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listNumberOfListInstances(buff),
			decode_listNumberOfListInstances(buff, numberOfLinkListInstances));
}

/**
 * Returns the memoryUsed (in per cent) and the totalMemoryAvailable (in bytes).
 *
 * @param[out] memoryUsed Pointer to the parameter containing the current memory
 * usage (in per cent).
 *
 * @param[out] totalMemoryAvailable Pointer to the parameter containing the total memory
 * available (in bytes) for the Replica List provider.
 *
 * @return MafOk if memory is available, or MafOutOfMemory if no memory is available.
 */
static MafReturnT listMemoryUsage(uint32_t* memoryUsed,
		uint32_t* totalMemoryAvailable) {
	GENERIC_BERT_RPC(rl_component->config,
			encode_listMemoryUsage(buff),
			decode_listMemoryUsage(buff, memoryUsed, totalMemoryAvailable));
}

MafReturnT comte_replicated_list_create(comte_mw_component_t* comp) {

	MafReturnT res = MafOk;

	rl_component = comp;
	rl_component->rl = comte_malloc(sizeof(comte_replicated_list_interface_t));
	comte_replicated_list_interface_t* rl = rl_component->rl;

	rl->base.base.componentName = MW_COMPONENT_NAME;
	rl->base.base.interfaceName = MafMwSpiReplicatedList_1Id.interfaceName;
	rl->base.base.interfaceVersion = MafMwSpiReplicatedList_1Id.interfaceVersion;

	rl->base.listClear = listClear;
	rl->base.listCreate = listCreate;
	rl->base.listDelete = listDelete;
	rl->base.listClear = listClear;
	rl->base.listGetSize = listGetSize;
	rl->base.listIsEmpty = listIsEmpty;
	rl->base.listGetFrontRef = listGetFrontRef;
	rl->base.listPushBack = listPushBack;
	rl->base.listPopBack = listPopBack;
	rl->base.listEraseItem = listEraseItem;
	rl->base.listGetNextItemFront = listGetNextItemFront;
	rl->base.listGetFinalize = listGetFinalize;
	rl->base.listFindItem = listFindItem;
	rl->base.listReplaceItem = listReplaceItem;
	rl->base.listNumberOfListInstances = listNumberOfListInstances;
	rl->base.listMemoryUsage = listMemoryUsage;

	return res;
}

MafReturnT comte_replicated_list_destroy(comte_mw_component_t* comp) {

	MafReturnT res = MafOk;
	comte_free(comp->rl);
	return res;
}

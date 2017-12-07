#ifndef ComMwSpiComtEConverter_1_h
#define ComMwSpiComtEConverter_1_h

#include <MafMwSpiAccessManagement_1.h>
#include <MafMwSpiReplicatedList_1.h>
#include <MafMwSpiAvailabilityController_1.h>
#include "ComMwSpiComtEReplicatedList_1.h"
#include "ComtEComm_1.h"

// Access Management
MafReturnT encode_getRoles(const char *user, comte_buff_t* buff);
MafReturnT decode_getRoles(comte_buff_t* buff,
		MafMwSpiAccessManagementRoleT** roles);

// Availablilty Controller
MafReturnT encode_acInitialize(comte_buff_t *buff);
MafReturnT decode_acInitialize(comte_buff_t *buff);
MafReturnT encode_healthCheckReport(MafReturnT healthReport,
		MafMwSpiRecommendedRecoveryT recommendedRecovery, comte_buff_t *buff);
MafReturnT decode_healthCheckReport(comte_buff_t *buff);
MafReturnT encode_haModeAssumed(MafReturnT error, comte_buff_t *buff);
MafReturnT decode_haModeAssumed(comte_buff_t *buff);
MafReturnT encode_prepareTerminationResponse(MafReturnT error,
		comte_buff_t *buff);
MafReturnT decode_prepareTerminationResponse(comte_buff_t *buff);

// Replicated List
MafReturnT encode_listCreate(const MafMwSpiListNameT* listInstanceName,
		uint32_t dataBufferSize, comte_buff_t* buff);
MafReturnT decode_listCreate(comte_buff_t* buff);

MafReturnT encode_listDelete(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff);
MafReturnT decode_listDelete(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff);

MafReturnT encode_listClear(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff);
MafReturnT decode_listClear(comte_buff_t* buff);

MafReturnT encode_listGetSize(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff);
MafReturnT decode_listGetSize(comte_buff_t* buff, uint32_t* listSize);

MafReturnT encode_listIsEmpty(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff);
MafReturnT decode_listIsEmpty(comte_buff_t* buff, bool* listEmpty);

MafReturnT encode_listGetFrontRef(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff);
MafReturnT decode_listGetFrontRef(comte_buff_t* buff,
		comte_replicated_list_t* list);

MafReturnT encode_listPushBack(const MafMwSpiListNameT* listInstanceName,
		void* newItemDataBuffer, comte_buff_t* buff);
MafReturnT decode_listPushBack(comte_buff_t* buff,
		MafMwSpiListItemNameT* listItemName);

MafReturnT encode_listPopBack(const MafMwSpiListNameT* listInstanceName,
		comte_buff_t* buff);
MafReturnT decode_listPopBack(comte_buff_t* buff);

MafReturnT encode_listEraseItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, comte_buff_t* buff);
MafReturnT decode_listEraseItem(comte_buff_t* buff);

MafReturnT encode_listFindItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, comte_buff_t* buff);
MafReturnT decode_listFindItem(comte_buff_t* buff, void* copyOfItemData);

MafReturnT encode_listReplaceItem(const MafMwSpiListNameT* listInstanceName,
		const MafMwSpiListItemNameT* listItemName, const void* replaceItemData,
		comte_buff_t* buff);
MafReturnT decode_listReplaceItem(comte_buff_t* buff);

MafReturnT encode_listNumberOfListInstances(comte_buff_t* buff);
MafReturnT decode_listNumberOfListInstances(comte_buff_t* buff,
		uint32_t* numberOfLinkListInstances);

MafReturnT encode_listMemoryUsage(comte_buff_t* buff);
MafReturnT decode_listMemoryUsage(comte_buff_t* buff, uint32_t* memoryUsed,
		uint32_t* totalMemoryAvailable);

MafReturnT encode_encrypt(const char *string, comte_buff_t* buff);
MafReturnT decode_encrypt(comte_buff_t* buff, char** encryptedString);

MafReturnT encode_decrypt(const char *encryptedString, comte_buff_t* buff);
MafReturnT decode_decrypt(comte_buff_t* buff, char** string);

MafReturnT encode_logWrite(uint32_t eventId, MwSpiSeverityT severity,
		MwSpiFacilityT facility, const char *databuffer, comte_buff_t *buff);
MafReturnT decode_logWrite(comte_buff_t *buff);

#endif

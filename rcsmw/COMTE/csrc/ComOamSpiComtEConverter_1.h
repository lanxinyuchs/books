#ifndef ComOamSpiComtEConverter_1_h
#define ComOamSpiComtEConverter_1_h

#include <MafOamSpiServiceIdentities_1.h>
#include <MafOamSpiManagedObject_3.h>
#include <MafOamSpiCmEvent_1.h>
#include <MafMgmtSpiCommon.h>
#include "ComtEComm_1.h"
#include "bert.h"

MafReturnT encode_join(unsigned long transId, comte_buff_t* buff);
MafReturnT encode_prepare(unsigned long transId, comte_buff_t* buff);
MafReturnT encode_commit(unsigned long transId, comte_buff_t* buff);
MafReturnT encode_validate(unsigned long transId, comte_buff_t* buff);
MafReturnT encode_abort_transaction(unsigned long transId, comte_buff_t* buff);
MafReturnT encode_finish(unsigned long transId, comte_buff_t* buff);

MafReturnT decode_join(comte_buff_t* buff);
MafReturnT decode_prepare(comte_buff_t* buff);
MafReturnT decode_commit(comte_buff_t* buff);
MafReturnT decode_validate(comte_buff_t* buff, bool* result);
MafReturnT decode_abort_transaction(comte_buff_t* buff);
MafReturnT decode_finish(comte_buff_t* buff);

MafReturnT encode_setMoAttribute(unsigned long transId, const char* dn,
		const char* attributeName,
		const MafMoAttributeValueContainer_3T* attributeValue,
		comte_buff_t* buff);
MafReturnT decode_setMoAttribute(comte_buff_t* buff);

MafReturnT encode_setMoAttributes(unsigned long transId, const char* dn,
                                  MafMoNamedAttributeValueContainer_3T** attributes,
                                  comte_buff_t* buff);
MafReturnT decode_setMoAttributes(comte_buff_t* buff);

MafReturnT encode_getMoAttribute(unsigned long transId, const char* dn,
		const char* attributeName, comte_buff_t* buff);
MafReturnT decode_getMoAttribute(comte_buff_t* buff,
                                 MafMoAttributeValueResult_3T* result);

MafReturnT encode_getMoAttributes(unsigned long transId, const char* dn,
                                  const char** attributeNames, comte_buff_t* buff);
MafReturnT decode_getMoAttributes(comte_buff_t* buff,
                                  MafMoAttributeValuesResult_3T* result);

MafReturnT decode_moNamedAttributes_term(bert_term_t* response,
                                         MafMoNamedAttributeValueContainer_3T **named_arr);

MafReturnT encode_getMoIterator(unsigned long transId, const char* dn,
		const char* className, comte_buff_t* buff);
MafReturnT decode_getMoIterator(comte_buff_t* buff, bert_list_t** handle);


MafReturnT encode_createMo(unsigned long transId, const char* parentDn,
                           const char* className, const char* keyAttributeName,
                           const char* keyAttributeValue,
                           MafMoNamedAttributeValueContainer_3T ** initialAttributes,
                           comte_buff_t* buff);
MafReturnT decode_createMo(comte_buff_t* buff);

MafReturnT encode_deleteMo(unsigned long transId, const char* dn, comte_buff_t* buff);
MafReturnT decode_deleteMo(comte_buff_t* buff);

MafReturnT encode_existsMo(unsigned long transId, const char* dn, comte_buff_t* buff);
MafReturnT decode_existsMo(comte_buff_t* buff, bool* result);

MafReturnT encode_countMoChildren(unsigned long transId, const char* dn,
                                   const char* className, comte_buff_t* buff);
MafReturnT decode_countMoChildren(comte_buff_t* buff, uint64_t* result);

MafReturnT encode_action(unsigned long transId, const char* dn, const char* name,
                         MafMoNamedAttributeValueContainer_3T** parameters, comte_buff_t* buff);
MafReturnT decode_action(comte_buff_t* buff, MafMoAttributeValueResult_3T* result);

MafReturnT encode_registerClass(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                                MafMgmtSpiInterface_1T transactionalResourceId,
                                const char* mocPath,
                                comte_buff_t* buff);
MafReturnT decode_registerClass(comte_buff_t* buff);

MafReturnT encode_unregisterClass(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                                MafMgmtSpiInterface_1T transactionalResourceId,
                                const char* mocPath,
                                comte_buff_t* buff);
MafReturnT decode_unregisterClass(comte_buff_t* buff);

MafReturnT encode_registerDn(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                             MafMgmtSpiInterface_1T transactionalResourceId,
                             const char* dn,
                             comte_buff_t* buff);
MafReturnT decode_registerDn(comte_buff_t* buff);

MafReturnT encode_unregisterDn(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                             MafMgmtSpiInterface_1T transactionalResourceId,
                             const char* dn,
                             comte_buff_t* buff);
MafReturnT decode_unregisterDn(comte_buff_t* buff);


/* Comte Consumer calls */
MafReturnT encode_registerEventConsumer(char *producer,
                                        MafOamSpiEventConsumerHandleT consumerId,
                                        const char * eventType,
                                        MafNameValuePairT ** filter,
                                        comte_buff_t* buff);
MafReturnT decode_registerEventConsumer(comte_buff_t* buff);

MafReturnT encode_unregisterEventConsumer(char *producer,
                                          MafOamSpiEventConsumerHandleT consumerId,
                                          const char * eventType,
                                          comte_buff_t* buff);
MafReturnT decode_unregisterEventConsumer(comte_buff_t* buff);


MafReturnT encode_unregisterAllEventConsumers(char *producer,
                                              comte_buff_t* buff);
MafReturnT decode_unregisterAllEventConsumers(comte_buff_t* buff);





// Decoding events
MafReturnT decode_attribute_term(bert_term_t* response,
                                 MafMoAttributeValueContainer_3T** com_result);

MafReturnT decode_getMoAttribute_tuple(MafMoAttributeValue_3T* attr,
                                       bert_term_t* tuple);

// Release callbacks
void release_moAttribute(struct MafMoAttributeValueContainer_3* container);
void release_moAttributes(struct MafMoAttributeValueContainer_3** containers);



#endif

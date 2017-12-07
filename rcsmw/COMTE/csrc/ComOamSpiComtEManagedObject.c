/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <MafOamSpiServiceIdentities_1.h>
#include <MafMwSpiServiceIdentities_1.h>
#include <MafMwSpiLog_1.h>

#include "ComOamSpiComtEManagedObject_1.h"
#include "ComOamSpiComtEConverter_1.h"
#include "bert.h"

comte_oam_component_t* mo_component = NULL;

typedef struct comte_iterator {
    bert_list_t* iterator_list;
    bert_list_t* next;
} comte_iterator_t;

MafReturnT setMoAttribute(MafOamSpiTransactionHandleT txHandle,
			  const char * dn, const char * attributeName,
			  const MafMoAttributeValueContainer_3T * attributeValue) {
    INFO("Enter: %s, %s", dn, attributeName);
    MafReturnT res = MafFailure;

    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
	ERROR("transaction with id %lu was not found!", txHandle);
	return res;
    }


    /* ============================= OI ============================== */
    MafReturnT oi_res = MafOk;
    comte_oi_key_t* oi;
    if(get_oi(dn, &oi) == MafOk){
	/* This DN has COM OI */

	if(oi_join(oi, txHandle) == MafOk){
	    MafOamSpiManagedObject_3T* moIf = get_mo_if(oi->moId);
	    INFO("Calling OI '%s' to setMoAttribute %s %s",
		 moIf->base.componentName, dn, attributeName);

	    oi_res = moIf->setMoAttribute(txHandle, dn, attributeName, attributeValue);

	    if(oi_res != MafOk){
		ERROR("Object Implementer refused: %d", oi_res);
		return oi_res;
	    }
	}
	else {
	    ERROR("OI Join failed: %d,dn=%s,attr=%s", res, dn, attributeName);
	}
    }
    /* ============================= OI ============================== */

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_setMoAttribute((unsigned long) txHandle, dn,
				attributeName, attributeValue, buff);
    if (res != MafOk) {
	comte_free(buff);
	return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
	comte_free(buff);
	return res;
    }

    res = decode_setMoAttribute(buff);
    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;
}


static MafReturnT handle_oi_vsn(comte_oi_key_t* oi,
				MafOamSpiTransactionHandleT txHandle,
				const char * dn,
				MafMoNamedAttributeValueContainer_3T ** attributes){

    MafReturnT oi_res = MafOk;
    if(is_mo_version(oi->moId, "3_1")){
        MafOamSpiManagedObject_3_1T* moIf = get_mo_ext_if(oi->moId);
        oi_res = moIf->setMoAttributes(txHandle, dn, attributes);

    }
    else {
        MafOamSpiManagedObject_3T* moIf = get_mo_if(oi->moId);
        MafMoNamedAttributeValueContainer_3T** attr_pp = attributes;
        MafMoNamedAttributeValueContainer_3T* attr_p = *attr_pp++;

        while(attr_p != NULL){
            INFO("Calling OI '%s' to setMoAttribute %s %s",
                 moIf->base.componentName, dn, attr_p->name);

	    oi_res = moIf->setMoAttribute(txHandle,dn,attr_p->name, &attr_p->value);

            if(oi_res != MafOk)
                break;

            attr_p = *attr_pp++;
        }
    }

    if(oi_res != MafOk)
        ERROR("Object Implementer refused: %d", oi_res);

    return oi_res;
}

MafReturnT setMoAttributes(MafOamSpiTransactionHandleT txHandle,
                           const char * dn,
                           MafMoNamedAttributeValueContainer_3T ** attributes){

    ENTER();
    MafReturnT res = MafFailure;

    comte_client_con_t* client_con =
        find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return res;
    }


    /* ============================= OI ============================== */
    MafReturnT oi_res = MafOk;
    comte_oi_key_t* oi;
    if(get_oi(dn, &oi) == MafOk){
        /* This DN has COM OI */
        if(oi_join(oi, txHandle) == MafOk){

            /* The OI might implement lower versions */
            oi_res = handle_oi_vsn(oi, txHandle, dn, attributes);
            if(oi_res != MafOk)
                return oi_res;

        }
        else {
	    ERROR("OI Join failed: %d,dn=%s", res, dn);
        }
    }
    /* ============================= OI ============================== */

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_setMoAttributes((unsigned long) txHandle, dn,
                                 attributes, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);

    if (res != MafOk) {
        comte_free(buff);
        return res;
    }
    res = decode_setMoAttributes(buff);

    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;
}

MafReturnT getMoAttribute(MafOamSpiTransactionHandleT txHandle,
			  const char* dn, const char* attributeName,
			  MafMoAttributeValueResult_3T* result) {

    MafReturnT res = MafNotExist;

    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return MafFailure;
    }

    /* ============================= OI ============================== */
    MafReturnT oi_res = MafOk;
    comte_oi_key_t* oi;
    if(get_oi(dn, &oi) == MafOk){

        /* This DN has a registered COM OI */
        if(is_readonly_attr(attributeName, oi)){

            oi_res = oi_join(oi, txHandle);
            if(oi_res == MafOk){
		MafOamSpiManagedObject_3T* moIf = get_mo_if(oi->moId);
		INFO("Calling OI '%s' for %s %s",
		     moIf->base.componentName,dn,attributeName);

		oi_res = moIf->getMoAttribute(txHandle,dn,attributeName,result);

		/* Check result from the OI, not always behaving... */
		if(oi_res == MafOk && result->container && result->container->nrOfValues){
		    INFO("Get attribute '%s' from OI %s successfully.",
			 attributeName, moIf->base.componentName);
		    return oi_res;
		}
		else {
		    INFO("Get attribute '%s'from OI %s failed: %d, try reading from MW",
			 attributeName, moIf->base.componentName, oi_res);
		}
            }
            else {
		ERROR("OI Join failed: %d,dn=%s,attr=%s", oi_res, dn, attributeName);
            }
        }
    }
    /* ============================= OI ============================== */

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_getMoAttribute((unsigned long) txHandle, dn, attributeName, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = decode_getMoAttribute(buff, result);
    if (res != MafOk && res != MafNotExist) {
        ERROR("getMoAttribute on transaction %lu for %s,%s returned %d",
              (unsigned long) txHandle, dn, attributeName, (int) res);
    }

    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;

}

typedef struct comte_mo_attr_ {
    const char* name;
    MafMoAttributeValueContainer_3T* avc;
    bool is_ro;
} comte_mo_attr_t;


static void getMoAttributes_ERROR_res
(MafOamSpiTransactionHandleT txHandle,
 const char * dn,
 const char ** attributeNames,
 int numAttrs,
 MafReturnT res) {
    if (attributeNames[0] == NULL) {
        ERROR("getMoAttributes on transaction %lu for %s, returned %d",
              (unsigned long) txHandle, dn, (int) res);
    }
    else if (attributeNames[1] == NULL) {
        ERROR("getMoAttributes on transaction %lu for %s,%s returned %d",
              (unsigned long) txHandle, dn, attributeNames[0], (int) res);
    }
    else {
        ERROR("getMoAttributes on transaction %lu for "
              "%s,%s..(%d) returned %d",
              (unsigned long) txHandle,
              dn, attributeNames[0], numAttrs-1, (int) res);
    }
}

MafReturnT getMoAttributes(MafOamSpiTransactionHandleT txHandle,
			   const char * dn,
			   const char ** attributeNames,
			   MafMoAttributeValuesResult_3T * result){
    ENTER();
    MafReturnT res = MafNotExist;
    int i = 0;
    int numAttrs = 0;

    /* Check transaction connection */
    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return MafFailure;
    }

    /* Check number of attributes */
    for (numAttrs = 0;  attributeNames[numAttrs] != NULL;  numAttrs++);

    /* ============================= OI ============================== */
    MafReturnT oi_res = MafOk;
    comte_oi_key_t* oi;
    int numRO = 0;

    if(get_oi(dn, &oi) == MafOk){
        /* This DN has a COM Object Implementer */
	/* Create a new array of comte_mo_attrs, */
	/* This will be used as base for result  */
	comte_mo_attr_t* new_attrs =
	    comte_malloc(sizeof(comte_mo_attr_t) * (numAttrs+1));
	memset(new_attrs, 0, sizeof(comte_mo_attr_t) * (numAttrs+1));


	/* Traverse attr names array and tag items in */
        /* the new array if they are RO. */
	for(i = 0; attributeNames[i] != NULL; i++){
	    new_attrs[i].name = attributeNames[i];
	    if(is_readonly_attr(attributeNames[i], oi)){
		/* Step number of readonly attrs, used to
		   allocate array later */
		numRO++;
		new_attrs[i].is_ro = true;
	    }
	    else {
		/* MW attrs */
		new_attrs[i].is_ro = false;
	    }
	}


	if(numRO > 0){
	    /* We've found some readonly attributes,
	       let's check with the OI */

	    /* Create attr array for ROs */
	    const char** ro_attrs = comte_malloc(sizeof(char*) * (numRO+1));
            memset(ro_attrs, 0, (sizeof(char*) * (numRO+1)) );


	    /* Use the attributes to the RO array */
	    comte_mo_attr_t* new_attr_p = new_attrs;
	    for(i = 0; i < numRO; i++){
		while(!new_attr_p->is_ro){
		    new_attr_p++;
		}
		ro_attrs[i] = new_attr_p->name;
		new_attr_p++;
	    }

	    /* Call OI */
            MafOamSpiManagedObject_3T* moIf = get_mo_if(oi->moId);
	    INFO("Calling OI '%s' for %s, multiple attrs(%i)",
		 moIf->base.componentName, dn, numRO);

	    MafMoAttributeValuesResult_3T oi_result;
            oi_res = moIf->getMoAttributes(txHandle, dn,
                                           ro_attrs,
                                           &oi_result);

            if(oi_res == MafOk && oi_result.containers &&
	       oi_result.containers[0]->nrOfValues){

		ERROR("Get attributes from OI %s was unexpectedly successful.",
		      moIf->base.componentName);

		/* The only OI in COM, SecurityManagement only acts     */
		/*     as validator, ie calling SecurityManagement for  */
		/*     getMoAttributes is pointless. At time of writing */
		/*     there is no other OI in COM planned but we can't */
                /*     be sure. The following code is left for future   */
                /*     work.                                            */

		comte_free(new_attrs);
		comte_free(ro_attrs);
		return MafFailure;

		if(0){
		    /* The result is to be copied into the resulting array
		       and MW is to be called with the rest of the attributes */
		    MafMoAttributeValueContainer_3T** cont_pp =
			(MafMoAttributeValueContainer_3T**)result->containers;
		    new_attr_p = new_attrs;

		    while(*cont_pp){
			if(new_attr_p->is_ro){
			    /* Copy the content from OI result */
			    MafMoAttributeValueContainer_3T* cont_p = *cont_pp;
			    new_attr_p->avc = create_avc(cont_p->type,
							 cont_p->nrOfValues);
			    new_attr_p->avc->type = cont_p->type;
			    new_attr_p->avc->nrOfValues = cont_p->nrOfValues;
			    /* Values */
			    for(i = 0; i < cont_p->nrOfValues; i++){
				new_attr_p->avc->values[i] = cont_p->values[i];
			    }
			}

			cont_pp++;
			new_attr_p++;
		    }

		    /* Task 2: Create attr array for MW */
		    int numMW = numAttrs - numRO;
		    if(numMW > 0){
			INFO("Num MW attrs %i", numRO);
			const char** mw_attrs = comte_malloc(sizeof(char*) * (numMW+1));
			memset(mw_attrs, 0, (sizeof(char*) * (numMW+1)) );

			/* Encode attrs */
			comte_buff_t *buff = comte_malloc(sizeof(comte_buff_t));
			res = encode_getMoAttributes(txHandle, dn,
						     mw_attrs, buff);
			if (res != MafOk) {
			    ERROR("Attr encoding failed: %i", res);
			    comte_free(new_attrs);
			    comte_free(ro_attrs);
			    comte_free(mw_attrs);
			    comte_free(buff);
			    return res;
			}

			/* Task 3: Call MW with attr array */
			res = comte_send_recv(buff, &client_con->connection);
			if (res != MafOk) {
			    ERROR("Send-Recv failed: %i", res);
			    comte_free(new_attrs);
			    comte_free(ro_attrs);
			    comte_free(mw_attrs);
			    comte_free(buff);
			    return res;
			}

			/* Decode values from MW */
			MafMoAttributeValuesResult_3T mw_result;
			res = decode_getMoAttributes(buff, &mw_result);
            if (res != MafOk && res != MafNotExist) {
                getMoAttributes_ERROR_res
                    (txHandle, dn, attributeNames, numAttrs, res);
            }

			/* Task 4: Merge MW values into resulting array */
			/* TODO */

		    }
		} // if(0)
	    }
	    else {
            INFO("Get attributes from OI, failed, try reading from MW instead");
            comte_free(ro_attrs);
            comte_free(new_attrs);
	    }
	}
	else {
	    //INFO("MO had implementer(OI) but no read-only attributes");
	    comte_free(new_attrs);
	}

    }
    /* ============================= OI ============================== */

    /* Encode all attributeNames for MW */
    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_getMoAttributes((unsigned long) txHandle, dn,
				 attributeNames, buff);


    /* Check encoding result */
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
	return res;
    }

    res = decode_getMoAttributes(buff, result);
    if (res != MafOk && res != MafNotExist) {
        getMoAttributes_ERROR_res
            (txHandle, dn, attributeNames, numAttrs, res);
    }

    comte_free(buff->buff);
    comte_free(buff);

    return res;
}



MafReturnT newMoIterator(MafOamSpiTransactionHandleT txHandle,
			 const char * dn, const char * className,
			 MafOamSpiMoIteratorHandle_3T *result) {
    ENTER();
    MafReturnT res = MafNotExist;

    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return MafFailure;
    }

    //INFO("Creating iterator for tx %d with %s, %s",txHandle, dn, className);
    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_getMoIterator((unsigned long) txHandle, dn, className, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }


    comte_iterator_t* iter = comte_malloc(sizeof(comte_iterator_t));

    /* The following will not compile if *result is
     * too small to hold the pointer iter
     */
    enum { CHECK_HANDLE_SIZE_1 = 1 /
	   ((sizeof(*result) >= sizeof(iter)) ? 1 : 0) };

    *result = (MafOamSpiMoIteratorHandle_3T)(uintptr_t)iter;
    res = decode_getMoIterator(buff, &iter->iterator_list);
    if (res != MafOk) {
        comte_free(iter);
        comte_free(buff->buff);
        comte_free(buff);
        return res;
    }
    iter->next = iter->iterator_list;
    comte_free(buff->buff);
    comte_free(buff);

    // have to wrap in bert term so that bert_free works
    bert_term_t* list = comte_malloc(sizeof(bert_term_t));
    list->type = BERT_LIST;
    list->list = iter->iterator_list;
    comte_mem_list_push(txHandle, BERT, (void*) list);
    comte_mem_list_push(txHandle, VOID, (void*) iter);

    LEAVE();
    return res;
}

MafReturnT finalizeMoIterator(MafOamSpiMoIteratorHandle_3T itHandle){
    /* The iterator is dealloced when the transaction is aborted/finished */
    return MafOk;
}

MafReturnT nextMo(MafOamSpiMoIteratorHandle_3T itHandle, char **result) {
    ENTER();
    MafReturnT res = MafOk;
    comte_iterator_t* iter = (comte_iterator_t*)(uintptr_t)itHandle;
    if (iter->next == NULL) {
        *result = NULL;
    } else {
        if (iter->next->value->type != BERT_BINARY)
            return MafFailure;
        bert_binary_t* string = &iter->next->value->binary;
        string->length++;
        uint8_t* data = string->value;
        string->value = comte_malloc(sizeof(uint8_t*) * string->length);
        memcpy(string->value, data, string->length - 1);
        string->value[string->length - 1] = 0;
        comte_free(data);
        *result = (char*) string->value;
        iter->next = iter->next->next;
    }
    LEAVE();
    return res;
}

MafReturnT createMo(MafOamSpiTransactionHandleT txHandle,
		    const char * parentDn, const char * className,
		    const char * keyAttributeName, const char * keyAttributeValue,
		    MafMoNamedAttributeValueContainer_3T ** initialAttributes) {
    INFO("Enter: %s, %s, %s", parentDn, className, keyAttributeValue);
    MafReturnT res = MafFailure;

    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
	ERROR("transaction with id %lu was not found!", txHandle);
	return res;
    }


    /* ============================= OI ============================== */
    MafReturnT oi_res = MafFailure;
    comte_oi_key_t* oi;
    if(get_oi_by_class(className, &oi) == MafOk){
	/* This DN has COM OI */
	if(oi_join(oi, txHandle) == MafOk){
	    MafOamSpiManagedObject_3T* moIf = get_mo_if(oi->moId);
	    INFO("Asking OI '%s' to create MO of class %s",
		 moIf->base.componentName, className);

	    oi_res = moIf->createMo(txHandle, parentDn, className,
				    keyAttributeName, keyAttributeValue,
				    initialAttributes);
	    /* If OI returns error, return immediately
	       otherwise continue with MW */
	    if(oi_res != MafOk){
		ERROR("Object Implementer refusation: %d", oi_res);
		return oi_res;
	    }
	}
	else {
	    ERROR("OI Join failed: %d,parentDn=%s,className=%s",res,parentDn,className);
	}
    }
    /* ============================= OI ============================== */


    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_createMo((unsigned long) txHandle, parentDn, className,
			  keyAttributeName, keyAttributeValue,
			  initialAttributes, buff);
    if (res != MafOk) {
	comte_free(buff);
	return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
	comte_free(buff);
	return res;
    }

    res = decode_createMo(buff);
    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;
}

MafReturnT deleteMo(MafOamSpiTransactionHandleT txHandle,
		    const char * dn) {
    INFO("Enter: %s", dn);
    MafReturnT res = MafFailure;

    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return res;
    }

    /* ============================= OI ============================== */
    MafReturnT oi_res = MafOk;
    comte_oi_key_t* oi;
    if(get_oi(dn, &oi) == MafOk){
        /* This DN has COM OI */
        if(oi_join(oi, txHandle) == MafOk) {
	    // Actually, we need to check the bloody version
	    MafOamSpiManagedObject_3T* moIf = get_mo_if(oi->moId);
	    INFO("Calling OI '%s' to delete %s", moIf->base.componentName, dn);

	    oi_res = moIf->deleteMo(txHandle, dn);
	    if(oi_res != MafOk){
		ERROR("Object Implementer refused: %d", oi_res);
		return oi_res;
	    }
        }
        else {
	    ERROR("OI Join failed: %d,dn=%s",res,dn);
        }
    }
    /* ============================= OI ============================== */

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_deleteMo((unsigned long) txHandle, dn, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = decode_deleteMo(buff);
    comte_free(buff->buff);
    comte_free(buff);
    LEAVE();
    return res;
}


MafReturnT existsMo(MafOamSpiTransactionHandleT txHandle,
                    const char * dn,
                    bool * result) {

    ENTER();
    MafReturnT res = MafFailure;

    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return res;
    }

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_existsMo((unsigned long) txHandle, dn, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = decode_existsMo(buff, result);
    INFO("existsMo: %s -> %d", dn, *result);

    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;
}

MafReturnT countMoChildren(MafOamSpiTransactionHandleT txHandle,
                           const char * dn,
                           const char * className,
                           uint64_t * result){
    ENTER();
    MafReturnT res = MafFailure;


    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);

    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return res;
    }

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_countMoChildren((unsigned long) txHandle, dn, className, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = decode_countMoChildren(buff, result);

    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;
}

MafReturnT action(MafOamSpiTransactionHandleT txHandle,
		  const char * dn,
		  const char * name,
		  MafMoNamedAttributeValueContainer_3T **parameters,
		  MafMoAttributeValueResult_3T * result) {

    INFO("Enter: %s, %s", dn, name);
    MafReturnT res = MafFailure;

    comte_client_con_t* client_con =
	find_client_con(mo_component->trans, txHandle);
    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return MafFailure;
    }


    /* ============================= OI ============================== */
    MafReturnT oi_res = MafOk;
    comte_oi_key_t* oi;
    if(get_oi(dn, &oi) == MafOk){
        /* This DN has COM OI */
        if(oi_join(oi, txHandle) != MafOk){

	    MafOamSpiManagedObject_3T* moIf = get_mo_if(oi->moId);

	    INFO("Calling OI '%s' action %s",moIf->base.componentName, name);
	    oi_res = moIf->action(txHandle, dn, name,parameters, result);

	    if(oi_res != MafOk){
		ERROR("Object Implementer refused: %d", oi_res);
		return oi_res;
	    }
        }
        else{
	    ERROR("OI Join failed: %d", res);
        }
    }
    /* ============================= OI ============================== */

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_action((unsigned long) txHandle, dn, name, parameters, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = decode_action(buff, result);

    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;
}

MafReturnT comte_managed_object_create(comte_oam_component_t* comp) {
    MafReturnT res = MafOk;

    mo_component = comp;

    mo_component->mo = comte_malloc(sizeof(comte_mo_t));
    comte_mo_t* mo = comp->mo;

    /* Registration of ManagedObject_3 SPI */
    mo->base.base.componentName = OAM_COMPONENT_NAME;
    mo->base.base.interfaceName = MafOamSpiCmRouterService_3Id.interfaceName;
    mo->base.base.interfaceVersion = MafOamSpiCmRouterService_3Id.interfaceVersion;

    mo->base.action = action;
    mo->base.createMo = createMo;
    mo->base.deleteMo = deleteMo;
    mo->base.getMoAttribute = getMoAttribute;
    mo->base.getMoAttributes = getMoAttributes;
    mo->base.newMoIterator = newMoIterator;
    mo->base.nextMo = nextMo;
    mo->base.setMoAttribute = setMoAttribute;
    mo->base.finalizeMoIterator = finalizeMoIterator;
    mo->base.existsMo = existsMo;
    mo->base.countMoChildren = countMoChildren;

    /* Registration of ManagedObject_3_1 SPI extension */
    mo->base_ext.base.componentName = OAM_COMPONENT_NAME;
    mo->base_ext.base.interfaceName = MafOamSpiCmRouterService_3_1Id.interfaceName;
    mo->base_ext.base.interfaceVersion = MafOamSpiCmRouterService_3_1Id.interfaceVersion;

    mo->base_ext.setMoAttributes = setMoAttributes;

    return res;
}

MafReturnT comte_managed_object_destroy(comte_oam_component_t* component) {
    MafReturnT res = MafOk;
    comte_free(mo_component->mo);
    return res;
}



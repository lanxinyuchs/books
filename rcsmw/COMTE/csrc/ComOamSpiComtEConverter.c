/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include "ComOamSpiComtEConverter_1.h"
#include "ComtEUtils_1.h"
#include "bert.h"



MafReturnT encode_transaction_rpc(unsigned long transId, char* name, comte_buff_t* buff) {
	bert_term_t* args[] = { create_uint64(transId) };
	return encode_rpc(name, clist(1, args), buff);
}

MafReturnT encode_join(unsigned long transId, comte_buff_t* buff) {
	return encode_transaction_rpc(transId, "join", buff);
}

MafReturnT encode_prepare(unsigned long transId, comte_buff_t* buff) {
	return encode_transaction_rpc(transId, "prepare", buff);
}

MafReturnT encode_commit(unsigned long transId, comte_buff_t* buff) {
	return encode_transaction_rpc(transId, "commit", buff);
}

MafReturnT encode_validate(unsigned long transId, comte_buff_t* buff) {
	return encode_transaction_rpc(transId, "validate", buff);
}

MafReturnT encode_abort_transaction(unsigned long transId, comte_buff_t* buff) {
	return encode_transaction_rpc(transId, "abort_transaction", buff);
}

MafReturnT encode_finish(unsigned long transId, comte_buff_t* buff) {
	return encode_transaction_rpc(transId, "finish", buff);
}

MafReturnT decode_join(comte_buff_t* buff) {
	return decode_ok_response(buff);
}

MafReturnT decode_prepare(comte_buff_t* buff) {
	return decode_ok_response(buff);
}

MafReturnT decode_commit(comte_buff_t* buff) {
	return decode_ok_response(buff);
}

MafReturnT decode_validate(comte_buff_t* buff, bool* validate_result) {
	MafReturnT res = MafFailure;
	bool result = 0;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);
        if(res != MafOk){
            goto done;
        }

	if (response->type == BERT_ATOM
		&& strcmp(response->atom, "ok") == 0) {
		result = !0; res = MafOk; goto done; /* ok */
	}

	if (response->type != BERT_SMALL_TUPLE) {
		ERROR("validate -> not a  tuple");
		/*abort();*/
		goto done; /* not tuple */
	}
	if (response->stuple.size <= 1) {
		ERROR("validate -> invalid tuple_size");
		goto done; /* tuple_size =< 1 */
	}

	bert_term_t* tag = response->stuple.values[0];
	if (tag->type != BERT_ATOM || strcmp(tag->atom, "ok") != 0) {
		ERROR("validate -> invalid tuple tag");
		goto done; /* {not ok, ...} */
	}

	bert_term_t* r = response->stuple.values[1];
	if (r->type != BERT_BOOLEAN) {
		ERROR("validate -> result is not boolean");
		goto done; /* {ok, not atom ...} */
	}
	res = MafOk; /* {ok, boolean() ...} */
	if (response->stuple.size >= 3) {
		/* {ok, boolean(), [<<"reason">> ...] | <<"reason">>} */
		res = comte_add_nbi_bert_messages(response->stuple.values[2]);
	}
	if (res == MafOk) result = !! r->boolean;

 done:
	bert_free(response);
	*validate_result = result;
	return res;
}

MafReturnT decode_abort_transaction(comte_buff_t* buff) {
	return decode_ok_response(buff);
}

MafReturnT decode_finish(comte_buff_t* buff) {
	return decode_ok_response(buff);
}

bert_term_t* encode_setMoAttr_uint64(uint8_t type, uint64_t value) {
        bert_term_t* type_tag[] = { csint(type), create_uint64(value) };
        return ctuple(2, type_tag);
}

bert_term_t* encode_setMoAttr_sbig(uint8_t type, int64_t value) {
        bert_term_t* type_tag[] = { csint(type), csbig(value) };
        return ctuple(2, type_tag);
}

bert_term_t* encode_setMoAttr_lint(uint8_t type, int32_t value) {
        bert_term_t* type_tag[] = { csint(type), clint(value) };
        return ctuple(2, type_tag);
}

/* DECIMAL not implemented in COM 3.3*/
/* bert_term_t* encode_setMoAttr_decimal64(uint8_t type, double value) { */
/* 	bert_term_t* type_tag[] = { csint(type), cdec64(value) }; */
/* 	return ctuple(2, type_tag); */
/* } */

bert_term_t* encode_setMoAttribute_switch(
		const MafMoAttributeValueContainer_3T * attributeValue) {

        /* nrOfValues is zero if we are to unset an attribute */
	if (attributeValue->nrOfValues == 0) {
	    return catom("undefined");
	}
       	/* Create a list of values to be set, the erlang side will
	   trim the list if there is only one element. */
	int i;
	bert_list_t* curr = comte_malloc(sizeof(bert_list_t));

	curr->next = NULL;
	for (i = attributeValue->nrOfValues - 1; i >= 0; i--) {

		switch (attributeValue->type) {
		case MafOamSpiMoAttributeType_3_UINT8: {
			curr->value =
					encode_setMoAttr_lint(attributeValue->type,
							(int32_t) attributeValue->values[i].value.u8);
			break;
		}
		case MafOamSpiMoAttributeType_3_UINT16: {
			curr->value =
					encode_setMoAttr_lint(attributeValue->type,
							(int32_t) attributeValue->values[i].value.u16);
			break;
		}
		case MafOamSpiMoAttributeType_3_UINT32: {
			curr->value =
					encode_setMoAttr_sbig(attributeValue->type,
							(int64_t) attributeValue->values[i].value.u32);
			break;
		}
		case MafOamSpiMoAttributeType_3_UINT64: {
			curr->value =
					encode_setMoAttr_uint64(attributeValue->type,
							attributeValue->values[i].value.u64);
			break;
		}
		case MafOamSpiMoAttributeType_3_INT8: {
			curr->value =
					encode_setMoAttr_lint(attributeValue->type,
							(int32_t) attributeValue->values[i].value.i8);
			break;
		}
		case MafOamSpiMoAttributeType_3_INT16: {
			curr->value =
                                        encode_setMoAttr_lint(attributeValue->type,
                                                        (int32_t) attributeValue->values[i].value.i16);
			break;
		}
		case MafOamSpiMoAttributeType_3_INT32: {
			curr->value =
					encode_setMoAttr_lint(attributeValue->type,
							(int32_t) attributeValue->values[i].value.i32);
			break;
		}
		case MafOamSpiMoAttributeType_3_INT64: {
			curr->value =
					encode_setMoAttr_sbig(attributeValue->type,
							(int64_t) attributeValue->values[i].value.i64);
			break;
		}
		case MafOamSpiMoAttributeType_3_ENUM: {
			curr->value =
                                        encode_setMoAttr_sbig(attributeValue->type,
                                                     (int64_t) attributeValue->values[i].value.theEnum);
			break;
		}
		case MafOamSpiMoAttributeType_3_BOOL: {
			bert_term_t* type_tag[] = { csint(attributeValue->type), cbool(
					attributeValue->values[i].value.theBool) };
			curr->value = ctuple(2, type_tag);
			break;
		}
		case MafOamSpiMoAttributeType_3_STRING: {
			bert_term_t * type_tag[] =
					{
                                            csint(attributeValue->type),
                                            cstring(attributeValue->values[i].value.theString)
                                        };
			curr->value = ctuple(2, type_tag);
			break;
		}
		case MafOamSpiMoAttributeType_3_REFERENCE: {
			bert_term_t * type_tag[] =
					{
                                            csint(attributeValue->type),
                                            cstring(attributeValue->values[i].value.moRef)
                                        };
			curr->value = ctuple(2, type_tag);
			break;
		}
		case MafOamSpiMoAttributeType_3_STRUCT: {

			MafMoAttributeValueStructMember_3T* struct_curr =
					attributeValue->values[i].value.structMember;

			if (struct_curr == NULL) {
				ERROR("Invalid struct! Segfault incoming!");
			}

			bert_list_t* struct_member = comte_malloc(sizeof(bert_list_t));
			struct_member->next = NULL;
			bert_term_t* struct_member_list = comte_malloc(sizeof(bert_term_t));
			struct_member_list->type = BERT_LIST;
			struct_member_list->list = struct_member;

			while (struct_curr != NULL) {
				bert_term_t* member = encode_setMoAttribute_switch(
						struct_curr->memberValue);
				bert_term_t* type_tag[] = { cstring(struct_curr->memberName),
						member };
				struct_member->value = ctuple(2, type_tag);
				if (struct_curr->next != NULL) {
					struct_member->next = comte_malloc(sizeof(bert_list_t));
					struct_member = struct_member->next;
					struct_member->next = NULL;
				}
				struct_curr = struct_curr->next;
			}

			bert_term_t* type_tag[] = { csint(attributeValue->type),
					struct_member_list };
			curr->value = ctuple(2, type_tag);
			break;
		}

                    /* Decimal not implemented in COM 3.3 */
                    /* case MafOamSpiMoAttributeType_3_DECIMAL64: { */
                    /*     curr->value = */
                    /*         encode_setMoAttr_decimal64(attributeValue->type, */
                    /*                                    attributeValue->values[i].value.decimal64); */
                    /*     break; */
                    /* } */
		default: {
			bert_term_t *term = comte_malloc(sizeof(bert_term_t));
			term->type = BERT_LIST;
			term->list = curr;
			bert_free(term);
			return NULL;
		}
		}
		if (i != 0) {
			bert_list_t* tmp = comte_malloc(sizeof(bert_list_t));
			tmp->next = curr;
			curr = tmp;
		}
	}
	bert_term_t* term;
	// Use the list if this is a sequence, only send the tuple if it is not.
	if (attributeValue->nrOfValues != 1) {
		term = comte_malloc(sizeof(bert_term_t));
		term->type = BERT_LIST;
		term->list = curr;
	} else {
		term = curr->value;
		curr->value = NULL;
		comte_free(curr);
	}
	return term;
}


bert_term_t* encode_moNamedAttribute(MafMoNamedAttributeValueContainer_3T* namedAttributeValue){
    bert_term_t* named_attr[] = {cstring(namedAttributeValue->name),
                                 encode_setMoAttribute_switch(&namedAttributeValue->value)};
    return ctuple(2, named_attr);


}

bert_term_t* encode_moNamedAttributes(MafMoNamedAttributeValueContainer_3T** parameters){
    bert_term_t* params = comte_malloc(sizeof(bert_term_t));
    if (*parameters == NULL)
        params->type = BERT_NIL;
    else {
        bert_list_t* curr = comte_malloc(sizeof(bert_list_t));
        MafMoNamedAttributeValueContainer_3T** curr_named_param = parameters;

        params->type = BERT_LIST;
        params->list = curr;
        while(true){
            curr->value = encode_moNamedAttribute(*curr_named_param);
            curr_named_param++;
            if (*curr_named_param == NULL) {
                curr->next = NULL;
                break;
            }
            curr->next = comte_malloc(sizeof(bert_list_t));
            curr = curr->next;
        }
    }
    return params;
}


MafReturnT decode_moNamedAttributes_term
(bert_term_t* response,
 MafMoNamedAttributeValueContainer_3T **named_arr) {
	MafReturnT res = MafFailure;
	int i,j;

	/* getMoAttribute responses are (lists with tagged tuples
	   {Type,Value}) or (a tagged tuple {Type,Value} or (undefined) */

	switch (response->type) {
	case BERT_NIL: {
	    res = MafNotExist;
	    named_arr[0] = NULL;
	    break;
	}
	case BERT_ATOM: {
		if (strcmp(response->atom, "undefined") == 0) {
			res = MafNotExist;
		}
		named_arr[0] = NULL;
		break;
	}
	case BERT_LIST: {
		bert_list_t* curr = response->list;
		uint16_t num_cont = bert_list_length(curr);

		for (i = 0;  i < num_cont;  i++) {
			if (curr->value->type != BERT_SMALL_TUPLE) {
				ERROR("Unexpected BERT type: %d", curr->value->type);
			fail:
				named_arr[i] = NULL;
				res = MafFailure;
				break;
			}
			bert_stuple_t *named_cont = &curr->value->stuple;
			if (named_cont->size != 2) {
				ERROR("Unexpected arity: %d", named_cont->size);
				goto fail;
			}
			bert_term_t *attr_name = named_cont->values[0];
			if (attr_name->type != BERT_BINARY) {
				ERROR("Unexpected BERT type: %d", attr_name->type);
				goto fail;
			}
			bert_term_t *attr_val = named_cont->values[1];

			switch(attr_val->type) {
			case BERT_NIL: {
				/* {<<"attr_name">>, []}
				 * No value set for named attribute,
				 * just create an empty container */
				named_arr[i] = create_navc
					(attr_name->binary.value,
					 attr_name->binary.length,
					 0,
					 0);
				res = MafOk;
				break;
			}
			case BERT_ATOM: {
				if (strcmp(attr_val->atom, "undefined") != 0) {
					ERROR("Unexpected atom: %s", attr_val->atom);
					goto fail;
				}
				/* {<<"attr_name">>, undefined}
				 * Undefined value set for named attribute,
				 * just create an empty container */
				named_arr[i] = create_navc
					(attr_name->binary.value,
					 attr_name->binary.length,
					 0,
					 0);
				res = MafOk;
				break;
			}
			case BERT_LIST: {
				bert_list_t* avc_list = attr_val->list;

				if (avc_list->value->type != BERT_SMALL_TUPLE) {
					ERROR("Unexpected BERT type: %d", avc_list->value->type);
					goto fail;
				}
				if (avc_list->value->stuple.size != 2) {
					ERROR("Unexpected arity: %d",
						  avc_list->value->stuple.size);
					goto fail;
				}
				if (avc_list->value->stuple.values[0]->type !=
					BERT_SMALL_INTEGER) {
					ERROR("Unexpected BERT type: %d",
						  avc_list->value->stuple.values[0]->type);
					goto fail;
				}
				/* {<<"attr_name">>, [{_Type,_Val},...]} */
				uint16_t avc_length = bert_list_length(avc_list);

				/* Create named containers */
				named_arr[i] = create_navc
					(attr_name->binary.value,
					 attr_name->binary.length,
					 avc_list->value->stuple.values[0]->sint,
					 avc_length);

				/* Decode attributes */
				MafMoAttributeValue_3T *values_ptr =
					named_arr[i]->value.values;
				for (j = 0; j < avc_length; j++) {
					res = decode_getMoAttribute_tuple
						(values_ptr, avc_list->value);

					if (res != MafOk) {
						named_arr[i+1] = NULL;
						break;
					}
					avc_list = avc_list->next;
					values_ptr++;
				}
				break;
			}
			default: {
				ERROR("Unexpected BERT type: %d", attr_val->type);
				named_arr[i] = NULL;
				res = MafFailure;
			}
			} /* case */

			/* Break if error, cleanup is performed later */
			if (res != MafOk) break;

			/* Move to next named avc */
			curr = curr->next;
		} /* for i */
		break;
	}
	default:
		ERROR("Decode failure, unknown type: %i", response->type);
		named_arr[0] = NULL;
		break;
	} /* case */
	return res;
}


MafReturnT encode_setMoAttribute(unsigned long transId, const char * dn,
		const char * attributeName,
		const MafMoAttributeValueContainer_3T * attributeValue,
		comte_buff_t* buff) {

	bert_term_t* args[] =
		{ create_uint64(transId), cstring(dn), cstring(attributeName),
		  encode_setMoAttribute_switch(attributeValue) };
	return encode_rpc("setMoAttribute", clist(4, args), buff);

}

MafReturnT decode_setMoAttribute(comte_buff_t* buff) {
    return decode_ok_response(buff);
}

MafReturnT encode_setMoAttributes(unsigned long transId, const char * dn,
                                  MafMoNamedAttributeValueContainer_3T ** attributes,
                                  comte_buff_t* buff) {

    bert_term_t* attrs = encode_moNamedAttributes(attributes);
    bert_term_t* args[] =
        { create_uint64(transId), cstring(dn), attrs};
    return encode_rpc("setMoAttributes", clist(3, args), buff);

}

MafReturnT decode_setMoAttributes(comte_buff_t* buff) {
    return decode_ok_response(buff);
}


MafReturnT encode_getMoAttribute(unsigned long transId, const char* dn,
                                 const char* attributeName, comte_buff_t* buff) {
	bert_term_t* args[] =
			{ create_uint64(transId), cstring(dn), cstring(attributeName) };

	return encode_rpc("getMoAttribute", clist(3, args), buff);
}

MafReturnT encode_getMoAttributes(unsigned long transId, const char* dn,
				  const char** attributeNames, comte_buff_t* buff) {
    ENTER();

    bert_term_t* attrs = comte_malloc(sizeof(bert_term_t));
    if(attrs == NULL)
	return MafFailure;

    //check if the first element of attributeNames is empty
    if(*attributeNames == NULL){
      attrs->type=BERT_NIL;
    }
    else{
      attrs->type = BERT_LIST;

      bert_list_t* curr = comte_malloc(sizeof(bert_list_t));
      char** curr_attr = (char**)attributeNames;
      attrs->list = curr;

      while(true){
	curr->value = cstring(*curr_attr);

	curr_attr++;
	if(*curr_attr == NULL){
	  curr->next = NULL;
	  break;
	}

	curr->next = comte_malloc(sizeof(bert_list_t));
	curr = curr->next;
      }
    }

    bert_term_t* args[] =
        { create_uint64(transId), cstring(dn), attrs };

    return encode_rpc("getMoAttributes", clist(3, args), buff);
}

MafReturnT decode_getMoAttribute_term(bert_term_t* response,
                                      MafMoAttributeValueContainer_3T **result);




static MafReturnT decode_struct
(MafMoAttributeValue_3T* attr, bert_term_t* value) {
    MafReturnT res = MafFailure;
    bert_list_t* curr;
    int name_len = 0;
    MafMoAttributeValueStructMember_3T
        *struct_first, *struct_prev, *struct_curr;

    struct_first = struct_prev = struct_curr = NULL;
    for (curr = value->list;  curr != NULL;  curr = curr->next) {

		if (curr->value->type != BERT_SMALL_TUPLE) {
			ERROR("Unexpected BERT type: %d", curr->value->type);
		fail:
			res = MafFailure;
			break;
		}
		bert_stuple_t *tuple = &curr->value->stuple;
		if (tuple->size != 2) {
			ERROR("Unexpected arity: %d", tuple->size);
			goto fail;
		}
		if (tuple->values[0]->type != BERT_BINARY) {
			ERROR("Unexpected BERT type: %d", tuple->values[0]->type);
			goto fail;
		}

		struct_curr = comte_malloc(sizeof(MafMoAttributeValueStructMember_3T));

        res = decode_getMoAttribute_term
			(tuple->values[1], &struct_curr->memberValue);

        if(res == MafOk){
            /* Insert memberName */
            name_len = tuple->values[0]->binary.length;
            struct_curr->memberName =
                comte_malloc(sizeof(char) * (name_len + 1));
            memcpy
                (struct_curr->memberName,
                 tuple->values[0]->binary.value,
                 name_len);
            struct_curr->memberName[name_len] = 0;
        }
        else if (res == MafNotExist) {
            /* There is no value present, container empty
               Populate struct member with just a memberName and
               an empty container */
            name_len = tuple->values[0]->binary.length;
            struct_curr->memberName =
                comte_malloc(sizeof(char) * (name_len + 1));
            memcpy
                (struct_curr->memberName,
                 tuple->values[0]->binary.value,
                 name_len);
            struct_curr->memberName[name_len] = 0;

            struct_curr->memberValue = create_avc(0, 0);
            res = MafOk;
        }
        else {
			comte_free(struct_curr);
            break;
        } /* else if */

		struct_curr->next = NULL;
		if (struct_first == NULL) struct_first = struct_curr;
		else struct_prev->next = struct_curr;
		struct_prev = struct_curr;
    }

    if (res == MafOk) {
        attr->value.structMember = struct_first;
    }
    else {
        /* Failure - free all allocated struct members */
        for (struct_curr = struct_first;
             struct_curr != NULL;
             (struct_prev = struct_curr,
			  struct_curr = struct_curr->next,
			  comte_free(struct_prev))  ) {
            comte_free(struct_curr->memberName);
			destroy_avc(struct_curr->memberValue);
        }
    }

    return res;
}



MafReturnT decode_getMoAttribute_value
(MafMoAttributeValue_3T *attr, int type, bert_term_t *value) {
	MafReturnT res = MafFailure;

	switch (type) {
	case MafOamSpiMoAttributeType_3_STRING: {
		if (value->type != BERT_BINARY){
			attr->value.theString = NULL;
			break;
		}
		char* string = copy_bert_binary(&value->binary);
		attr->value.theString = string;
		res = MafOk;
		break;
	}
	case MafOamSpiMoAttributeType_3_REFERENCE: {
		if (value->type != BERT_BINARY){
			attr->value.moRef = NULL;
			break;
		}
		char* reference = copy_bert_binary(&value->binary);
		attr->value.moRef = reference;
		res = MafOk;
		break;
	}
	case MafOamSpiMoAttributeType_3_ENUM: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.theEnum = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.theEnum = value->lint;
			res = MafOk;
		}
		else if (value->type == BERT_SMALL_BIG) {
			attr->value.theEnum = value->sbig;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_BOOL: {
		if (value->type != BERT_BOOLEAN)
			break;

		attr->value.theBool = value->boolean;
		res = MafOk;
		break;
	}
	case MafOamSpiMoAttributeType_3_INT8: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.i8 = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.i8 = value->lint;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_INT16: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.i16 = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.i16 = value->lint;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_INT32: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.i32 = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.i32 = value->lint;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_INT64: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.i64 = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.i64 = value->lint;
			res = MafOk;
		} else if (value->type == BERT_SMALL_BIG) {
			attr->value.i64 = value->sbig;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_UINT8: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.u8 = value->sint;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_UINT16: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.u16 = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.u16 = value->lint;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_UINT32: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.u32 = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.u32 = value->lint;
			res = MafOk;
		} else if (value->type == BERT_SMALL_BIG) {
			attr->value.u32 = value->sbig;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_UINT64: {
		if (value->type == BERT_SMALL_INTEGER) {
			attr->value.u64 = value->sint;
			res = MafOk;
		} else if (value->type == BERT_INTEGER) {
			attr->value.u64 = value->lint;
			res = MafOk;
		} else if (value->type == BERT_SMALL_BIG) {
			attr->value.u64 = value->sbig;
			res = MafOk;
		} else if (value->type == BERT_UINT64) {
			attr->value.u64 = value->uint64;
			res = MafOk;
		}
		break;
	}
	case MafOamSpiMoAttributeType_3_STRUCT: {
		if (value->type != BERT_LIST) {
			attr->value.structMember = NULL;
			break;
		}
		res = decode_struct(attr, value);
		break;
	}
    } /* case */

    if (res == MafFailure) {
        ERROR("Bad attribute tag: %i for BERT type: %i",
              type, (int) value->type);
    }
    return res;
}


MafReturnT decode_getMoAttribute_tuple
(MafMoAttributeValue_3T* attr, bert_term_t* tuple) {
	MafReturnT res = MafFailure;

	if (tuple->type != BERT_SMALL_TUPLE) {
		ERROR("Unexpected BERT type: %d", tuple->type);
		return res;
	}
	if (tuple->stuple.size != 2) {
		ERROR("Unexpected arity: %d", tuple->stuple.size);
		return res;
	}
	if (tuple->stuple.values[0]->type != BERT_SMALL_INTEGER) {
		ERROR("Unexpected BERT type: %d", tuple->stuple.values[0]->type);
		return res;
	}

    return decode_getMoAttribute_value
        (attr, (int) tuple->stuple.values[0]->sint, tuple->stuple.values[1]);
}


MafReturnT decode_getMoAttribute_term
(bert_term_t* response,
 MafMoAttributeValueContainer_3T **container) {
	MafReturnT res = MafFailure;
	/* getMoAttribute responses are (lists with tagged tuples {Type,Value})
	   or (a tagged tuple {Type,Value} or (undefined) */
	switch (response->type) {

	case BERT_NIL: {
	    res = MafNotExist;
		*container = NULL;
		break;
	}
	case BERT_ATOM: {
		if (strcmp(response->atom, "undefined") == 0) {
			res = MafOk;
			/* Create a container with zero values */
			/* This seems to generate logs in com.log about
			 * unexpected type in COM 5.0 since the integer
			 * type is hard coded. Earlier, COM did not care
			 * about the empty containers */
		MafMoAttributeValueContainer_3T* com_res =
		    create_avc(MafOamSpiMoAttributeType_3_INT8, 0);
		*container = com_res;
		}
		else {
			ERROR("Bad response: %s", response->atom);
			*container = NULL;
			res = MafFailure;
		}
		break;
	}
    case BERT_BYTE_LIST: {
        uint8_t *bytes;
        uint16_t count;
		MafMoAttributeValueContainer_3T* com_res;
        int i;
        bert_term_t t;

        count = response->byte_list.length;
        bytes = response->byte_list.value;
        com_res = create_avc(bytes[0], count-1);
        *container = com_res;
        res = MafOk;
        t.type = BERT_SMALL_INTEGER;
        for (i = 1;  i < count;  i++) {
            t.sint = bytes[i];
            INFO("decode_getMoAttribute_value");
            res = decode_getMoAttribute_value
                (&com_res->values[i-1], bytes[0], &t);
            INFO("decode_getMoAttribute_value returned");
            if (res != MafOk) {
                destroy_avc(com_res);
                break;
            }
        }
        break;
    }
	case BERT_LIST: {
		bert_term_t *first;
		bert_list_t *curr;
		uint16_t count;
		MafMoAttributeValueContainer_3T* com_res;
		int i;

		// If the response is a sequence
		first = response->list->value;
        if (first->type == BERT_SMALL_INTEGER) {
            // Sequence in [Tag|Vals] format
            curr = response->list->next;
            count = bert_list_length(curr);
            com_res = create_avc(first->sint, count);
            *container = com_res;
            for (i = 0;  i < count;  i++) {
                res = decode_getMoAttribute_value
                    (com_res->values + i, first->sint, curr->value);
                if (res != MafOk) {
                    destroy_avc(com_res);
                    break;
                }
                curr = curr->next;
            }
            break;
        }

		if (first->type != BERT_SMALL_TUPLE) {
			ERROR("Unexpected BERT type: %d", first->type);
			return res;
		}
		if (first->stuple.size != 2) {
			ERROR("Unexpected arity: %d", first->stuple.size);
			return res;
	    }
		if (first->stuple.values[0]->type != BERT_SMALL_INTEGER) {
			ERROR("Unexpected BERT type: %d", first->stuple.values[0]->type);
			return res;
		}

		count = bert_list_length(response->list);
		com_res = create_avc(first->stuple.values[0]->sint, count);
		*container = com_res;

		curr = response->list;
		for (i = 0; i < count; i++) {
			res = decode_getMoAttribute_tuple(com_res->values + i, curr->value);
			if (res != MafOk) {
				destroy_avc(com_res);
				break;
			}
			curr = curr->next;
		}

		break;
	}
	case BERT_SMALL_TUPLE: {
		MafMoAttributeValueContainer_3T* com_res;

		// If the response is a single value
		if (response->stuple.size != 2 ||
			response->stuple.values[0]->type != BERT_SMALL_INTEGER) {
			ERROR("Tuple type: %i, res: %i",
				  response->stuple.values[0]->type, res);
			*container = NULL;
			return res;
		}
		com_res = create_avc(response->stuple.values[0]->sint, 1);
		*container = com_res;

		res = decode_getMoAttribute_tuple(com_res->values, response);
		if (res != MafOk){
			destroy_avc(com_res);
		}
		break;
	}
	default:
            ERROR("Bad response type: %i", response->type);
            break;
	}
	return res;
}

MafReturnT decode_getMoAttribute(comte_buff_t* buff,
                                 MafMoAttributeValueResult_3T* result) {
	MafReturnT res = MafFailure;
        bert_term_t* response = NULL;
        /* Error code will be set, as well as potential  */
        /* error string. */
	res = decode_rpc(buff, &response);

        result->release = NULL;
        result->container = NULL;

        if(res == MafOk){
            /* Decode response */
            res = decode_getMoAttribute_term(response,
                                             &result->container);
            if(res == MafOk){
                result->release = release_moAttribute;
            }
        }

     	bert_free(response);
	LEAVE();
	return res;
}

void release_moAttribute(struct MafMoAttributeValueContainer_3* container){
    destroy_avc((MafMoAttributeValueContainer_3T*) container);
}

void release_moAttributes(struct MafMoAttributeValueContainer_3** containers){
    MafMoAttributeValueContainer_3T** cont_ptr = containers;
    while(*cont_ptr){
        destroy_avc(*cont_ptr);
        cont_ptr++;
    }
    comte_free(containers);

}

MafReturnT decode_getMoAttributes(comte_buff_t* buff,
                                  MafMoAttributeValuesResult_3T* result) {
    ENTER();
    MafReturnT res = MafFailure;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);

    result->release = NULL;
    result->containers = NULL;

    if(res == MafOk){
	switch(response->type){

	case BERT_NIL: {
	    res = MafNotExist;
	    goto done;
	}

	case BERT_LIST: {
	    int i;
	    MafMoAttributeValueContainer_3T **containers;
	    bert_list_t *attr_list;

	    i = bert_list_length(response->list);
	    containers = comte_malloc(sizeof(*containers) * (i+1));

	    for(attr_list=response->list, i = 0;
		attr_list != NULL;
		attr_list=attr_list->next, i++) {

		res = decode_getMoAttribute_term
		    (attr_list->value, containers+i);
		if(res != MafOk) {
		    containers[i] = NULL;
		    release_moAttributes(containers);
		    goto done;
		}
	    }
	    containers[i] = NULL;
	    result->containers = containers;
	    result->release = release_moAttributes;
	    goto done;
	}

	default: {
	    ERROR("Bad response type: %i", response->type);
	    res = MafFailure;
	    goto done;
	}
	}
    }

 done:
    bert_free(response);
    LEAVE();
    return res;
}


MafReturnT encode_getMoIterator(unsigned long transId, const char* dn,
                                const char* className, comte_buff_t* buff) {
	bert_term_t* args[] =
		{ create_uint64(transId), cstring(dn), cstring(className) };

	return encode_rpc("getMoIterator", clist(3, args), buff);
}
MafReturnT decode_getMoIterator(comte_buff_t* buff, bert_list_t** handle) {
	MafReturnT res = MafOk;
	bert_term_t* response = NULL;

        res = decode_rpc(buff, &response);

        if(res == MafOk){
            switch(response->type){

            case BERT_NIL: {
                (*handle) = NULL;
                break;
            }

            case BERT_LIST: {
                bert_list_t* curr = response->list;
                // Collapse type tagging of iterator keys
                while (curr) {
                    bert_term_t* container = curr->value;

                    if (container->type != BERT_SMALL_TUPLE
                        || container->stuple.values[0]->type != BERT_SMALL_INTEGER
                        || container->stuple.values[1]->type != BERT_BINARY){
                        res = MafFailure;
                        break;
                    }

                    curr->value = container->stuple.values[1];
                    container->stuple.values[1] = NULL;
                    bert_free(container);
                    curr = curr->next;
                }

                (*handle) = response->list;
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

MafReturnT encode_createMo(unsigned long transId, const char* parentDn,
                           const char* className, const char* keyAttributeName,
                           const char* keyAttributeValue,
                           MafMoNamedAttributeValueContainer_3T ** initialAttrs,
                           comte_buff_t* buff) {

    bert_term_t* params = encode_moNamedAttributes(initialAttrs);
    bert_term_t * args[] =
        { create_uint64(transId), cstring(parentDn),
          cstring(className), cstring(keyAttributeName),
          cstring(keyAttributeValue), params  };

    return encode_rpc("createMo", clist(6, args), buff);
}

MafReturnT decode_createMo(comte_buff_t* buff) {
    return decode_ok_response(buff);
}

MafReturnT encode_deleteMo(unsigned long transId, const char* dn, comte_buff_t* buff) {
    bert_term_t * args[] = { create_uint64(transId), cstring(dn) };
    return encode_rpc("deleteMo", clist(2, args), buff);
}

MafReturnT decode_deleteMo(comte_buff_t* buff) {
    return decode_ok_response(buff);
}

MafReturnT encode_existsMo(unsigned long transId, const char* dn, comte_buff_t* buff) {
    bert_term_t * args[] = { create_uint64(transId), cstring(dn) };
    return encode_rpc("existsMo", clist(2, args), buff);
}

MafReturnT decode_existsMo(comte_buff_t* buff, bool* result) {
    MafReturnT res = MafFailure;
    bert_term_t *response = NULL;

    res = decode_rpc(buff, &response);

    if( res == MafOk ) {
        if( response->type == BERT_BOOLEAN ){
            *result = response->boolean;
        }
        else {
            ERROR("Bad response type: %i", response->type);
            res = MafFailure;
        }
    }

    bert_free((bert_term_t*) response);
    return res;
}


MafReturnT encode_countMoChildren(unsigned long transId, const char* dn,
                                  const char* className, comte_buff_t* buff) {

    bert_term_t * args[] = { create_uint64(transId), cstring(dn), cstring(className) };
    return encode_rpc("countMoChildren", clist(3, args), buff);
}

MafReturnT decode_countMoChildren(comte_buff_t* buff, uint64_t* result){
    MafReturnT res = MafFailure;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);

    /* Change return code only when result differs */
    /* from an integer */
    if(res == MafOk){
        switch(response->type){
        case BERT_UINT64:
            *result = response->uint64;
            break;
        case BERT_SMALL_BIG:
            *result = (uint64_t)response->sbig;
            break;
        case BERT_SMALL_INTEGER:
            *result = (uint64_t)response->sint;
            break;
        case BERT_INTEGER:
            *result = (uint64_t)response->lint;
            break;
        default:
            ERROR("Bad response type: %i", response->type);
            res = MafFailure;
        }
    }

    bert_free((bert_term_t*) response);
    return res;
}




MafReturnT encode_action(unsigned long transId,
                         const char* dn,
                         const char* name,
                         MafMoNamedAttributeValueContainer_3T** parameters,
                         comte_buff_t* buff) {

    bert_term_t* params = encode_moNamedAttributes(parameters);
    bert_term_t * args[] =
        { create_uint64(transId), cstring(dn), cstring(name), params };

    return encode_rpc("action", clist(4, args), buff);
}




MafReturnT decode_action(comte_buff_t* buff, MafMoAttributeValueResult_3T* result) {
    MafReturnT res = MafFailure;
    bert_term_t* response = NULL;

    res = decode_rpc(buff, &response);

    result->release = NULL;
    result->container = NULL;

    if(res == MafOk){
        /* Check action return values */
        switch (response->type) {
        case BERT_NIL: {
            /* leave container as NULL for empty sequence*/
            break;
        }
        case BERT_ATOM: {
	    if((strcmp(response->atom, "undefined") != 0) &&
	       (strcmp(response->atom, "ok") != 0)){
		ERROR("Bad response type: %s", response->atom);
                res = MafFailure;
	    }
	    break;
        }
        case BERT_LIST:
        case BERT_SMALL_TUPLE: {
            /* BERT_LIST or BERT_SMALL_TUPLE */
            res = decode_getMoAttribute_term(response,
                                             &result->container);
            if(result->container != NULL)
                result->release = release_moAttribute;
            break;
        }

        default:
            ERROR("Bad response type: %i", response->type);
            res = MafFailure;
            break;
        }
    }

    bert_free((bert_term_t*) response);
    return res;

}



bert_term_t* create_oi_interface(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                                 MafMgmtSpiInterface_1T transactionalResourceId) {
    bert_term_t* moi_id[] = { catom("comte_oi_if"),
                              cstring(managedObjectInterfaceId.componentName),
                              cstring(managedObjectInterfaceId.interfaceName),
                              cstring(managedObjectInterfaceId.interfaceVersion)};
    bert_term_t* moi_tuple = create_stuple(4, (bert_term_t**)moi_id);

    bert_term_t* tri_id[] = { catom("comte_oi_if"),
                              cstring(transactionalResourceId.componentName),
                              cstring(transactionalResourceId.interfaceName),
                              cstring(transactionalResourceId.interfaceVersion)};
    bert_term_t* tri_tuple = create_stuple(4, (bert_term_t**)tri_id);

    bert_term_t* comte_oi_tuple[] = {catom("comte_oi_reg"), moi_tuple, tri_tuple};
    bert_term_t* comte_oi = create_stuple(3, (bert_term_t**) comte_oi_tuple);
    return comte_oi;
}


MafReturnT encode_register_oi(char* func,
                              MafMgmtSpiInterface_1T managedObjectInterfaceId,
                              MafMgmtSpiInterface_1T transactionalResourceId,
                              const char* val,
                              comte_buff_t* buff){


    bert_term_t* comte_oi = create_oi_interface(managedObjectInterfaceId,
                                                transactionalResourceId);
    bert_term_t* args[] = {comte_oi, cstring(val) };
    return encode_rpc(func, clist(2, args), buff);
}

MafReturnT encode_registerClass(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                                MafMgmtSpiInterface_1T transactionalResourceId,
                                const char* mocPath,
                                comte_buff_t* buff){
    return encode_register_oi("registerClass",
                              managedObjectInterfaceId,
                              transactionalResourceId,
                              mocPath,
                              buff);
}


MafReturnT decode_registerClass(comte_buff_t* buff){
    return decode_ok_response(buff);
}

MafReturnT encode_unregisterClass(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                                MafMgmtSpiInterface_1T transactionalResourceId,
                                const char* mocPath,
                                comte_buff_t* buff){
    return encode_register_oi("unregisterClass",
                              managedObjectInterfaceId,
                              transactionalResourceId,
                              mocPath,
                              buff);
}

MafReturnT decode_unregisterClass(comte_buff_t* buff){
    return decode_ok_response(buff);
}



MafReturnT encode_registerDn(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                             MafMgmtSpiInterface_1T transactionalResourceId,
                             const char* dn,
                             comte_buff_t* buff){
    return encode_register_oi("registerDn",
                              managedObjectInterfaceId,
                              transactionalResourceId,
                              dn,
                              buff);
}

MafReturnT decode_registerDn(comte_buff_t* buff){
    return decode_ok_response(buff);
}

MafReturnT encode_unregisterDn(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                               MafMgmtSpiInterface_1T transactionalResourceId,
                               const char* dn,
                               comte_buff_t* buff){
    return encode_register_oi("unregisterDn",
                              managedObjectInterfaceId,
                              transactionalResourceId,
                              dn,
                              buff);
}

MafReturnT decode_unregisterDn(comte_buff_t* buff){
    return decode_ok_response(buff);
}

bert_term_t* encode_nameValuePair(MafNameValuePairT* pair){
    bert_term_t* name_pair[2];
    name_pair[0] = cstring(pair->name);
    if (strncmp(pair->name, "MafOamSpiNotificationFmFilterTypeDateTime", \
        strlen("MafOamSpiNotificationFmFilterTypeDateTime")) != 0) {
        name_pair[1] = cstring(pair->value);
    }
    else {
        /* TODO This specific handling should not be needed, and it's not */
        /* clear for how long either */
        char timestamp[64]={0x0};
        sprintf(timestamp, "%" PRIu64,*((uint64_t*)(pair->value)));
        name_pair[1] = cstring(timestamp);
    }

    return ctuple(2, name_pair);
}

bert_term_t* encode_eventFilters(MafNameValuePairT** filter){
    bert_term_t* comte_filters = comte_malloc(sizeof(bert_term_t));
    if (*filter == NULL)
        comte_filters->type = BERT_NIL;
    else {
        bert_list_t* curr = comte_malloc(sizeof(bert_list_t));
        MafNameValuePairT** curr_name_pair = filter;

        comte_filters->type = BERT_LIST;
        comte_filters->list = curr;
        while(true){
            curr->value = encode_nameValuePair(*curr_name_pair);
            /* Continue to next pair */
            curr_name_pair++;
            if (*curr_name_pair == NULL) {
                curr->next = NULL;
                break;
            }
            /* Prepare for next pair */
            curr->next = comte_malloc(sizeof(bert_list_t));
            curr = curr->next;
        }
    }
    return comte_filters;
}

MafReturnT encode_registerEventConsumer(char *producer,
                                        MafOamSpiEventConsumerHandleT consumerId,
                                        const char * eventType,
                                        MafNameValuePairT ** filter,
                                        comte_buff_t* buff){

    bert_term_t* comte_producer = create_atom(producer);
    bert_term_t* comte_consumer = create_uint64((uint64_t) consumerId);
    bert_term_t* comte_event_type = cstring(eventType);
    bert_term_t* comte_filter_addr = cbinary(sizeof(filter), (void *) &filter);
    bert_term_t* comte_filters = encode_eventFilters(filter);

    bert_term_t* args[] = {comte_producer,
                           comte_consumer,
                           comte_event_type,
                           comte_filter_addr,
                           comte_filters};

    return encode_rpc("registerEventConsumer", clist(5, args), buff);
}

MafReturnT decode_registerEventConsumer(comte_buff_t* buff) {
    return decode_ok_response(buff);
}

MafReturnT encode_unregisterEventConsumer(char *producer,
                                          MafOamSpiEventConsumerHandleT consumerId,
                                          const char * eventType,
                                          comte_buff_t* buff){
    bert_term_t* comte_producer = create_atom(producer);
    bert_term_t* comte_consumer = create_uint64((uint64_t) consumerId);
    bert_term_t* comte_event_type = cstring(eventType);

    bert_term_t* args[] = {comte_producer,
                           comte_consumer,
                           comte_event_type};

    return encode_rpc("unregisterEventConsumer", clist(3, args), buff);

}

MafReturnT decode_unregisterEventConsumer(comte_buff_t* buff) {
    return decode_ok_response(buff);
}


MafReturnT encode_unregisterAllEventConsumers(char *producer,
                                           comte_buff_t* buff){
    bert_term_t* args[] = { catom(producer), catom("all") };
    return encode_rpc("unregisterEventConsumer", clist(2, args), buff);
}

MafReturnT decode_unregisterAllEventConsumers(comte_buff_t* buff) {
    return decode_ok_response(buff);
}





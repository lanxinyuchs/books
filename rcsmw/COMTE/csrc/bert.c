/**
 * @author Lukas Larsson
 * @created 2011-03-03
 */

#include <stdlib.h>
#include <string.h>
#include <endian.h>
#include <netinet/in.h>
#include "bert.h"

// These put_int/get_int macros are signedness agnostic
//
#define put_int_type_conv(t, i, type, conv)								\
	do {																\
		type tmp = (type) conv(i);										\
		memcpy((t), &tmp, sizeof(type));								\
	} while (0);
#define get_int_type_conv(i, s, type, conv)								\
	do {																\
		memcpy((i), (s), sizeof(type));									\
		*(i) = (type) conv(*(i));										\
	} while (0);
//
#define put_int16(t, i) put_int_type_conv((t), (i), int16_t, htons)
#define put_int32(t, i) put_int_type_conv((t), (i), int32_t, htonl)
#define put_int64(t, i) put_int_type_conv((t), (i), int64_t, htole64)
//
#define get_int16(i, s) get_int_type_conv((i), (s), int16_t, ntohs)
#define get_int32(i, s) get_int_type_conv((i), (s), int32_t, ntohl)
#define get_int64(i, s) get_int_type_conv((i), (s), int64_t, le64toh)

#define bmalloc malloc
#define bcalloc calloc
#define bfree free

int encode_term(const bert_term_t* term, bert_buff_t* buff);
bert_term_t* decode_term(uint8_t* buff, uint32_t length, uint32_t* read);

int append_buff(bert_buff_t* buff, const uint8_t* data, const uint32_t length) {
	uint32_t new_length = buff->length + length;
	uint8_t* new_buff = bmalloc(sizeof(uint8_t) * new_length);
	if(!new_buff)
            return 0;

        if (buff->length != 0)
		memcpy(new_buff, buff->buff, buff->length);
	memcpy(new_buff + buff->length, data, length);
	bfree(buff->buff);
	buff->buff = new_buff;
	buff->length = new_length;
	return 1;
}

int prepend_buff(bert_buff_t* buff, const uint8_t* data, const uint32_t length) {
	uint32_t new_length = buff->length + length;
	uint8_t* new_buff = bmalloc(sizeof(uint8_t) * new_length);
        if(!new_buff)
            return 0;

	memcpy(new_buff, data, length);
	if (buff->length != 0)
		memcpy(new_buff + length, buff->buff, buff->length);
	bfree(buff->buff);
	buff->buff = new_buff;
	buff->length = new_length;
	return 1;
}

int encode_sint(const bert_term_t* term, bert_buff_t* buff) {

	buff->length = 2;
	buff->buff = bmalloc(sizeof(uint8_t) * 2);
        if(!buff->buff)
            return 0;

	*(buff->buff) = BERT_SMALL_INTEGER;
	*(buff->buff + 1) = term->sint;

	return 1;
}

int encode_lint(const bert_term_t* term, bert_buff_t* buff) {

	buff->length = 5;
	buff->buff = bmalloc(sizeof(int32_t) + sizeof(uint8_t));
        if(!buff->buff)
            return 0;

	*(buff->buff) = BERT_INTEGER;
	put_int32(buff->buff+1, term->lint);

	return 1;
}

int encode_sbig(const bert_term_t* term, bert_buff_t* buff) {
	int64_t val = term->sbig;
	int sign = 0;
	if (val < 0) {
		val *= -1;
		sign = 1;
	}

	buff->length = 11;
	buff->buff = bmalloc(3 * sizeof(uint8_t) + sizeof(int64_t));
        if(!buff->buff)
            return 0;

	buff->buff[0] = BERT_SMALL_BIG;
	buff->buff[1] = 8;
	buff->buff[2] = sign;
	put_int64(buff->buff+3, val);

	return 1;
}

int encode_uint64(const bert_term_t* bt, bert_buff_t* buff) {
	buff->length = 3*sizeof(*buff->buff) + sizeof(bt->uint64);
	buff->buff = bmalloc(buff->length);
        if(!buff->buff)
            return 0;

	buff->buff[0] = BERT_SMALL_BIG;
	buff->buff[1] = sizeof(bt->uint64);
	buff->buff[2] = 0; // Sign
	put_int64(buff->buff+3, bt->uint64);
	return 1;
}

int encode_stuple(const bert_term_t* term, bert_buff_t* buff) {
	bert_buff_t* temp_buff = bmalloc(sizeof(bert_buff_t));
        if(!temp_buff)
            return 0;

	int i, res = 1;
	for (i = 0; i < term->stuple.size; i++) {
		temp_buff->length = 0;
		temp_buff->buff = NULL;
		if (res != 0 && !encode_term(*(term->stuple.values + i), temp_buff)) {
			res = 0;
			break;
		}
		if (res != 0 && !append_buff(buff, temp_buff->buff, temp_buff->length)) {
			res = 0;
			bfree(temp_buff->buff);
			break;
		}
		bfree(temp_buff->buff);
	}

	uint8_t head[] = { BERT_SMALL_TUPLE, term->stuple.size };
	if (res != 0 && !prepend_buff(buff, head, 2))
		res = 0;

	bfree(temp_buff);
	return res;
}

int encode_atom(const bert_term_t* atom, bert_buff_t* buff) {
	uint8_t len = strlen(atom->atom);
	if (len > 255)
		return 0;
	buff->length = 3 + len;
	buff->buff = bmalloc(sizeof(uint8_t) * buff->length);
        if(!buff->buff)
            return 0;

	*(buff->buff) = BERT_ATOM;
	*(buff->buff + 1) = 0;
	memcpy((buff->buff + 2), &len, sizeof(uint8_t));
	strncpy((char *) (buff->buff + 3), atom->atom, sizeof(uint8_t) * len);
	return 1;
}

int encode_list(const bert_term_t* list, bert_buff_t* buff) {
	bert_list_t* curr = list->list;
	bert_buff_t* temp_buff = bmalloc(sizeof(bert_buff_t));
        if(!temp_buff)
            return 0;

	uint32_t count = 0;
	while (curr != NULL) {
		temp_buff->length = 0;
		temp_buff->buff = NULL;
		count++;
		if (!encode_term(curr->value, temp_buff))
			return 0;
		if (!append_buff(buff, temp_buff->buff, temp_buff->length)) {
			bfree(temp_buff->buff);
			return 0;
		}
		bfree(temp_buff->buff);
		curr = curr->next;
	}
	uint8_t tail[] = { BERT_NIL };
	if (!append_buff(buff, tail, sizeof(uint8_t)))
		return 0;

	uint32_t network_order_count = 0;
	put_int32(&network_order_count, count);
	if (!prepend_buff(buff, (uint8_t*) &network_order_count, sizeof(uint32_t)))
		return 0;
	uint8_t head[] = { BERT_LIST };
	if (!prepend_buff(buff, head, sizeof(uint8_t)))
		return 0;

	bfree(temp_buff);
	return 1;
}

int encode_byte_list(const bert_term_t* term, bert_buff_t* buff) {
	buff->buff = bmalloc(sizeof(uint8_t) * (term->byte_list.length + 3));
        if(!buff->buff)
            return 0;

	*(buff->buff) = BERT_BYTE_LIST;
	put_int16(buff->buff + 1, term->byte_list.length);
	memcpy(buff->buff + 3, term->byte_list.value,
			sizeof(uint8_t) * term->byte_list.length);
	buff->length = term->byte_list.length + 3;
	return 1;
}

int encode_boolean(const bert_term_t* bool, bert_buff_t* buff) {

	bert_term_t* bert = create_atom("bert");
	bert_term_t* value;
	if (bool->boolean)
		value = create_atom("true");
	else
		value = create_atom("false");
	bert_term_t* values[] = { bert, value };
	bert_term_t* tuple = create_stuple(2, values);

	int res = encode_term(tuple, buff);
	bert_free(tuple);
	return res;
}

int encode_nil(const bert_term_t* nil, bert_buff_t* buff) {

	bert_term_t* bert = create_atom("bert");
	bert_term_t* value = create_atom("nil");
	bert_term_t* values[] = { bert, value };
	bert_term_t* tuple = create_stuple(2, values);

	int res = encode_term(tuple, buff);
	bert_free(tuple);
	return res;
}

int encode_binary(const bert_term_t* term, bert_buff_t* buff) {
	buff->buff =
            bmalloc(sizeof(uint8_t) + sizeof(uint32_t)
                    + sizeof(uint8_t) * term->binary.length);
        if(!buff->buff)
            return 0;

	*(buff->buff) = BERT_BINARY;
	put_int32(buff->buff + 1, term->binary.length);
	memcpy(buff->buff + 5, term->binary.value, term->binary.length);
	buff->length = term->binary.length + 5;
	return 1;
}

int encode_term(const bert_term_t* term, bert_buff_t* buff) {
	switch (term->type) {
	case BERT_UINT64:
		return encode_uint64(term, buff);
	case BERT_SMALL_INTEGER:
		return encode_sint(term, buff);
	case BERT_INTEGER:
		return encode_lint(term, buff);
	case BERT_SMALL_BIG:
		return encode_sbig(term, buff);
	case BERT_SMALL_TUPLE:
		return encode_stuple(term, buff);
	case BERT_ATOM:
		return encode_atom(term, buff);
	case BERT_LIST:
		return encode_list(term, buff);
	case BERT_BYTE_LIST:
		return encode_byte_list(term, buff);
	case BERT_BOOLEAN:
		return encode_boolean(term, buff);
	case BERT_NIL:
		return encode_nil(term, buff);
	case BERT_BINARY:
		return encode_binary(term, buff);
	default:
		return 0;
	}
}

bert_buff_t* bert_encode(const bert_term_t* term) {
	bert_buff_t* buff = bmalloc(sizeof(bert_buff_t));
	if(!buff)
		return NULL;

	buff->length = 0;
	buff->buff = NULL;
	if (!encode_term(term, buff)) {
		bfree(buff);
		return NULL;
	}

	uint8_t pre = BERT_EXT_TERM;
	if (!prepend_buff(buff, (uint8_t*) &pre, 1)) {
		bfree(buff);
		return NULL;
	}
	return buff;
}

bert_term_t* decode_sint(uint8_t* buff, uint32_t length, uint32_t* read) {
	bert_term_t* sint = bmalloc(sizeof(bert_term_t));
        if(!sint)
            return NULL;

	sint->type = BERT_SMALL_INTEGER;
	sint->sint = *buff;
	(*read)++;
	return sint;
}

bert_term_t* decode_lint(uint8_t* buff, uint32_t length, uint32_t* read) {
	bert_term_t* lint = bmalloc(sizeof(bert_term_t));
        if(!lint)
            return NULL;

	lint->type = BERT_INTEGER;
	get_int32(&lint->lint, buff);
	(*read) += 4;
	return lint;
}

bert_term_t* decode_big_value(uint8_t* buff, uint32_t n, int sign) {
	bert_term_t* bt = bmalloc(sizeof(*bt));
        if(!bt)
            return NULL;

	bt->type = BERT_SMALL_BIG;
	if (n == 0) {
		bt->sbig = 0;
		return bt;
	}

	// Find real length - skip big end zeros
	do {
		if (buff[n-1] != 0) break;
	} while (--n != 0);

	if (n > 8) {
		// Out of range whether postitive or negative
		goto fail;
	}
	else if (n == 8 ) {
		if (sign == 0) { // Positive
			if (buff[7] >= 128) {
				// > SINT64_MAX
				bt->type = BERT_UINT64;
				get_int64(&bt->uint64, buff);
				goto done;
			}
		}
		else { // Negative
			if (buff[7] > 128) goto fail; // < SINT64_MIN
			if (buff[7] == 128) {
				if ((buff[6] | buff[5] | buff[4] |
					 buff[3] | buff[2] | buff[1] | buff[0]) == 0) {
					// == SINT64_MIN, negation would overflow but
					// positive bit pattern is the same as the negative
					get_int64(&bt->sbig, buff);
					goto done;
				}
				else { // < SINT64_MIN
					goto fail;
				}
				// > SINT64_MIN
			} // else (buff[9] < 128)
		}
	} // else (n < 8)
	// 64-bit arithmetic will not overflow

	{ // Convert the least significant n bytes
		register int64_t v;
		for (v = 0;  n != 0;) v = (v << 8) | buff[--n];
		if (sign != 0) v = -v;
		bt->sbig = v;
	}

 done:
	return bt;

 fail:
	bfree(bt);
	return NULL;
}

bert_term_t* decode_sbig(uint8_t* buff, uint32_t length, uint32_t* read) {
	bert_term_t* bt;
	uint32_t len;
	int sign;

	if (length < 2) return NULL;
	len = buff[0];
	sign = buff[1];
	if (len + 2 > length) return NULL;

	bt = decode_big_value(buff + 2, len, sign);
	if (bt != NULL) *read += len + 2;
	return bt;
}

// We should not get a long big fitting in int64_t/uint64_t from Erlang,
// but just in case and since it is allowed...
bert_term_t* decode_lbig(uint8_t* buff, uint32_t length, uint32_t* read) {
	bert_term_t* bt;
	uint32_t len;
	int sign;

	if (length < 5) return NULL;
	get_int32(&len, buff + 1);
	sign = buff[4];
	if (len + 5 > length) return NULL;

	bt = decode_big_value(buff + 5, len, sign);
	if (bt != NULL) *read += len + 5;
	return bt;
}

bert_term_t* decode_stuple(uint8_t* buff, uint32_t length, uint32_t* read) {
	// Read size
	uint8_t size = *buff;
	(*read)++;

	// Create tuple
	bert_term_t* stuple = bmalloc(sizeof(bert_term_t));
        if(!stuple)
            return NULL;

	stuple->type = BERT_SMALL_TUPLE;
	stuple->stuple.size = size;
	stuple->stuple.values = bcalloc(1, sizeof(bert_term_t*) * size);

	// Read subdata
	int i;
	for (i = 0; i < size; i++) {
		uint32_t r = 0;
		stuple->stuple.values[i] = decode_term(buff + (*read), length - 1, &r);
		if (stuple->stuple.values[i] == NULL) {
			bert_free(stuple);
			return NULL;
		}
		(*read) += r;
	}
	return stuple;
}

bert_term_t* decode_atom(uint8_t* buff, uint32_t length, uint32_t* read) {
	bert_term_t* atom = bmalloc(sizeof(bert_term_t));
        if(!atom)
            return NULL;

	atom->type = BERT_ATOM;
	uint8_t strlen = *(buff + 1);
	atom->atom = bmalloc(sizeof(char) * strlen + 1);
        if(!atom->atom){
            bfree(atom);
            return NULL;
        }
	memcpy(atom->atom, buff + 2, strlen);
	atom->atom[strlen] = 0;
	(*read) += (2 + strlen);
	return atom;
}

bert_term_t* decode_list(uint8_t* buff, uint32_t length, uint32_t* read) {
	uint32_t listlen = 0;
	get_int32(&listlen, buff);
	bert_term_t* list = bmalloc(sizeof(bert_term_t));
	if(!list)
            return NULL;

        list->list = bmalloc(sizeof(bert_list_t));
        if(!list->list){
            bert_free(list);
            return NULL;
        }

	bert_list_t* curr = list->list;
	list->type = BERT_LIST;
	uint8_t* curr_buff = buff + 4;
	(*read) += 4;
	int i;
	uint32_t r;
	for (i = 0; i < listlen; i++) {
		r = 0;
		curr->value = decode_term(curr_buff, length - (curr_buff - buff), &r);
		if (curr->value == NULL) {
			curr->next = NULL;
			bert_free(list);
			return NULL;
		}
		curr_buff += r;
		(*read) += r;
		if (i != (listlen - 1)) {
			curr->next = bmalloc(sizeof(bert_list_t));
                        if(!curr->next){
                            break;
                        }
			curr = curr->next;
		} else {
			curr->next = NULL;
		}
	}
	if (*curr_buff != BERT_NIL)
		return NULL;
	(*read)++;
	return list;
}

bert_term_t* decode_byte_list(uint8_t* buff, uint32_t length, uint32_t* read) {
	bert_term_t* list = bmalloc(sizeof(bert_term_t));
        if(!list)
            return NULL;

	list->type = BERT_BYTE_LIST;

	get_int16(&list->byte_list.length, buff);
	list->byte_list.value = bmalloc(sizeof(uint8_t) * list->byte_list.length);
        if(!list->byte_list.value){
            bfree(list);
            return NULL;
        }
	memcpy(list->byte_list.value, buff + 2,
			sizeof(uint8_t) * list->byte_list.length);
	*read += list->byte_list.length + 2;
	return list;
}

bert_term_t* decode_binary(uint8_t* buff, uint32_t length, uint32_t* read) {
	bert_term_t* term = bmalloc(sizeof(bert_term_t));
        if(!term)
            return NULL;

	term->type = BERT_BINARY;
	get_int32(&term->binary.length, buff);
	term->binary.value = bmalloc(sizeof(uint8_t) * term->binary.length);
        if(!term->binary.value){
            bfree(term);
            return NULL;
        }
	memcpy(term->binary.value, buff + 4, sizeof(uint8_t) * term->binary.length);
	*read += term->binary.length + 4;
	return term;
}

bert_term_t* decode_bert_term(bert_term_t* term) {
	switch (term->type) {
	case BERT_ATOM: {
		bert_term_t* atom = (bert_term_t*) term;
		if (strcmp(atom->atom, "nil") == 0) {
			bert_term_t* nil = bmalloc(sizeof(bert_term_t));
                        if(!nil)
                            return NULL;

                        nil->type = BERT_NIL;
			return (bert_term_t*) nil;
		} else if (strcmp(atom->atom, "true") == 0) {
			bert_term_t* bool = bmalloc(sizeof(bert_term_t));
                        if(!bool)
                            return NULL;

                        bool->type = BERT_BOOLEAN;
			bool->boolean = 1;
			return (bert_term_t*) bool;
		} else if (strcmp(atom->atom, "false") == 0) {
			bert_term_t* bool = bmalloc(sizeof(bert_term_t));
                        if(!bool)
                            return NULL;

                        bool->type = BERT_BOOLEAN;
			bool->boolean = 0;
			return (bert_term_t*) bool;
		}
		break;
	}
	default:
		return NULL;
	}
	return NULL;
}

bert_term_t* decode_term(uint8_t* buff, uint32_t length, uint32_t* read) {
	uint32_t i = 0;
	bert_term_t* res;
	switch (*buff) {
	case BERT_SMALL_INTEGER:
		res = decode_sint(buff + 1, length - 1, &i);
		break;
	case BERT_INTEGER:
		res = decode_lint(buff + 1, length - 1, &i);
		break;
	case BERT_SMALL_BIG:
		res = decode_sbig(buff + 1, length - 1, &i);
		break;
	case BERT_LARGE_BIG:
		res = decode_lbig(buff + 1, length - 1, &i);
		break;
	case BERT_SMALL_TUPLE: {
		bert_term_t* stuple = decode_stuple(buff + 1, length - 1, &i);
		res = stuple;
		if (stuple != NULL && stuple->stuple.size == 2
				&& stuple->stuple.values[0]->type == BERT_ATOM
				&& strcmp((stuple->stuple.values[0])->atom, "bert") == 0) {
			res = decode_bert_term(stuple->stuple.values[1]);
			bert_free(stuple);
		}
		break;
	}
	case BERT_ATOM:
		res = decode_atom(buff + 1, length - 1, &i);
		break;
	case BERT_LIST:
		res = decode_list(buff + 1, length - 1, &i);
		break;
	case BERT_BYTE_LIST:
		res = decode_byte_list(buff + 1, length - 1, &i);
		break;
	case BERT_BINARY:
		res = decode_binary(buff + 1, length - 1, &i);
		break;
	default:
		res = NULL;
		break;
	}
	(*read) += i;
	(*read)++;
	return res;
}

bert_term_t* bert_decode(const bert_buff_t* buff) {
	if (*buff->buff != 131)
		return NULL;
	uint32_t read = 0;
	bert_term_t* res = decode_term(buff->buff + 1, buff->length - 1, &read);
	if (res == NULL || read != buff->length - 1) {
		bert_free(res);
		return NULL;
	}
	return res;
}

bert_term_t* create_sint(uint8_t value) {
	bert_term_t* sint = bmalloc(sizeof(bert_term_t));
        if(!sint)
            return NULL;

	sint->type = BERT_SMALL_INTEGER;
	sint->sint = value;
	return sint;
}

bert_term_t* create_lint(int32_t value) {
	bert_term_t* lint = bmalloc(sizeof(bert_term_t));
        if(!lint)
            return NULL;

	lint->type = BERT_INTEGER;
	lint->lint = value;
	return lint;
}

bert_term_t* create_sbig(int64_t value) {
	bert_term_t* sbig = bmalloc(sizeof(bert_term_t));
        if(!sbig)
            return NULL;

	sbig->type = BERT_SMALL_BIG;
	sbig->sbig = value;
	return sbig;
}

bert_term_t* create_uint64(uint64_t value) {
	bert_term_t* bt = bmalloc(sizeof(bert_term_t));
        if(!bt)
            return NULL;

	bt->type = BERT_UINT64;
	bt->uint64 = value;
	return bt;
}

bert_term_t* create_stuple(uint8_t size, bert_term_t** values) {
	bert_term_t* tuple = bmalloc(sizeof(bert_term_t));
        if(!tuple)
            return NULL;

	tuple->type = BERT_SMALL_TUPLE;
	tuple->stuple.size = size;
	tuple->stuple.values = bmalloc(sizeof(bert_term_t*) * size);
        if(!tuple->stuple.values){
            bfree(tuple);
            return NULL;
        }
	int i;
	for (i = 0; i < size; i++) {
		tuple->stuple.values[i] = *(values + i);
	}
	return tuple;
}

bert_term_t* create_atom(char* value) {
	bert_term_t* atom = bmalloc(sizeof(bert_term_t));
        if(!atom)
            return NULL;

	atom->type = BERT_ATOM;
	atom->atom = strdup(value);
	return atom;
}

bert_term_t* create_list(uint8_t length, bert_term_t** values) {
	bert_term_t* list = bmalloc(sizeof(bert_term_t));
        if(!list)
            return NULL;

	list->type = BERT_LIST;
	if (length == 0) {
		list->list = NULL;
		return list;
	}
	list->list = bmalloc(sizeof(bert_list_t));
        if(!list->list){
            bfree(list);
            return NULL;
        }

	bert_list_t* curr = list->list;
	curr->value = values[0];
	curr->next = NULL;

	int i;
	for (i = 1; i < length; i++) {
		curr->next = bmalloc(sizeof(bert_list_t));
                if(!curr->next){
                    break;
                }
		curr = curr->next;
		curr->value = values[i];
		curr->next = NULL;
	}

	return list;
}

bert_term_t* create_byte_list(uint16_t length, uint8_t* data) {
	bert_term_t* list = bmalloc(sizeof(bert_term_t));
        if(!list)
            return NULL;

	list->type = BERT_BYTE_LIST;
	list->byte_list.length = length;
	list->byte_list.value = bmalloc(sizeof(uint8_t) * length);
        if(!list->byte_list.value){
            bfree(list);
            return NULL;
        }
	memcpy(list->byte_list.value, data, length);
	return list;
}

bert_term_t* create_binary(uint32_t length, uint8_t* data) {
	bert_term_t* term = bmalloc(sizeof(bert_term_t));
        if(!term)
            return NULL;

	term->type = BERT_BINARY;
	term->binary.length = length;
	term->binary.value = bmalloc(sizeof(uint8_t) * length);
        if(!term->binary.value){
            bfree(term);
            return NULL;
        }
	memcpy(term->binary.value, data, length);
	return term;
}

bert_term_t* create_boolean(uint8_t value) {
	bert_term_t* boolean = bmalloc(sizeof(bert_term_t));
        if(!boolean)
            return NULL;

       	boolean->type = BERT_BOOLEAN;
	boolean->boolean = value;
	return boolean;
}

void *bert_alloc(size_t size) {
    return bmalloc(size);
}

void bert_free(bert_term_t* term) {
	if (term == NULL)
		return;
	switch (term->type) {
	case BERT_ATOM: {
		bfree(term->atom);
		bfree(term);
		return;
	}
	case BERT_SMALL_TUPLE: {
		int i;
		for (i = 0; i < term->stuple.size; i++) {
			bert_free(term->stuple.values[i]);
		}
		bfree(term->stuple.values);
		bfree(term);
		return;
	}
	case BERT_LIST: {
		bert_list_t* list = term->list;
		while (list != NULL) {
			bert_free(list->value);
			bert_list_t* to_free = list;
			list = list->next;
			bfree(to_free);
		}
		bfree(term);
		return;
	}
	case BERT_BYTE_LIST: {
		bfree(term->byte_list.value);
		bfree(term);
		return;
	}
	case BERT_BINARY: {
		bfree(term->binary.value);
		bfree(term);
		return;
	}
	default:
		free(term);
		return;
	}
}

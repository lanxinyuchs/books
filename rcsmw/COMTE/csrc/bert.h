#ifndef BERT_H
#define BERT_H

#include <stdint.h>

typedef enum bert_type_t_ {
	BERT_UINT64 = (111+256), // XXX dirty workaround, could be any value > 255
	BERT_SMALL_INTEGER = 97, // 0 -> 255
	BERT_INTEGER = 98, // -2147483648 (2^31*-1) -> 2147483647 (2^31-1)
	BERT_FLOAT = 99,
	BERT_ATOM = 100,
	BERT_SMALL_TUPLE = 104,
	BERT_LARGE_TUPLE = 105,
	BERT_NIL = 106,
	BERT_BYTE_LIST = 107,
	BERT_LIST = 108,
	BERT_BINARY = 109,
	BERT_SMALL_BIG = 110, // 2^2047*-1 -> 2^2047-1
	BERT_LARGE_BIG = 111, // Crazily small to Crazily Big
	BERT_EXT_TERM = 131,
	BERT_BOOLEAN = 1
} bert_type_t;

typedef struct bert_term bert_term_t;

typedef struct bert_stuple {
	uint8_t size;
	bert_term_t** values;
} bert_stuple_t;

typedef struct bert_ltuple {
	uint32_t size;
	bert_term_t** values;
} bert_ltuple_t;

typedef struct bert_byte_list {
	uint16_t length;
	uint8_t* value;
} bert_byte_list_t;

typedef struct bert_list bert_list_t;
struct bert_list {
	bert_term_t* value;
	bert_list_t* next;
};

typedef struct bert_binary {
	uint32_t length;
	uint8_t* value;
} bert_binary_t;

// typedef struct bert_lbig {
// } bert_lbig_t;

struct bert_term {
	bert_type_t type;
	union {
		uint64_t uint64; // XXX Workaround
		uint8_t sint;
		int32_t lint;
		int64_t sbig;
		float sfloat;
		char* atom;
		bert_stuple_t stuple;
		bert_ltuple_t ltuple;
		bert_byte_list_t byte_list;
		bert_list_t* list;
		bert_binary_t binary;
		uint8_t boolean;
		// bert_lbig_t lbig;
		int64_t lbig;
	};
};

typedef struct bert_buff {
	uint32_t length;
	uint8_t* buff;
} bert_buff_t;

bert_buff_t* bert_encode(const bert_term_t* term);
bert_term_t* bert_decode(const bert_buff_t* buff);
bert_term_t* create_sint(uint8_t value);
bert_term_t* create_lint(int32_t value);
bert_term_t* create_sbig(int64_t value);
bert_term_t* create_stuple(uint8_t size, bert_term_t** values);
bert_term_t* create_atom(char* name);
bert_term_t* create_list(uint8_t length, bert_term_t** values);
bert_term_t* create_byte_list(uint16_t length, uint8_t* data);
bert_term_t* create_binary(uint32_t length, uint8_t* data);
bert_term_t* create_boolean(uint8_t value);

bert_term_t* create_uint64(uint64_t value);

void *bert_alloc(size_t size);
void bert_free(bert_term_t* term);

#endif

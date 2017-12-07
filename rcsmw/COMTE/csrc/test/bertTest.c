/* Comment
 * Unit test for bert.c
 * Copied out of bert.c
 */

#include <stdlib.h>
#include <string.h>
#include <endian.h>
#include <netinet/in.h>

#include "bertTest.h"
#include "../bert.h"

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

void TestSmallIntEncode(CuTest *tc) {

	bert_term_t* term = create_sint(1);
	const uint8_t expected[] = {131, 97, 1};

	bert_buff_t* buff = bert_encode(term);
	bert_free(term);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 3 == buff->length);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 3));
	bfree(buff);
}

void TestSmallIntDecode(CuTest *tc) {
	const uint8_t data[] = {131, 97, 1};
	const bert_buff_t buff = {3, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_SMALL_INTEGER);
	CuAssertTrue(tc, term->sint == 1);
	bert_free(term);
}

void TestLargeIntEncode(CuTest *tc) {

	bert_term_t* term = create_lint(-128);
	const uint8_t expected[] = {131, 98, 255, 255, 255, 128};

	bert_buff_t* buff = bert_encode(term);
	bert_free(term);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 6 == buff->length);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 6));
	bfree(buff);
}

void TestLargeIntDecode(CuTest *tc) {
	const uint8_t data[] = {131, 98, 255, 255, 255, 128};
	const bert_buff_t buff = {6, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_INTEGER);
	bert_term_t* lint = term;
	CuAssertTrue(tc, lint->lint == -128);
	bert_free(term);
}

void TestSmallBigPosEncode(CuTest *tc) {
	bert_term_t* term = create_sbig(2147483648ul);
	const uint8_t expected[] = {131, 110, 8, 0, 0, 0, 0, 128, 0, 0, 0, 0};

	bert_buff_t* buff = bert_encode(term);
	bert_free(term);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 12 == buff->length);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 12));
	bfree(buff);
}

void TestSmallBigNegEncode(CuTest *tc) {
	bert_term_t* term = create_sbig(-9223372036854775807ull);
	const uint8_t expected[] = {131, 110, 8, 1, 255, 255, 255, 255, 255, 255,
		255, 127};

	bert_buff_t* buff = bert_encode(term);
	bert_free(term);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 12 == buff->length);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 12));
	bfree(buff);
}

void TestSmallBigPosDecode(CuTest *tc) {
	const uint8_t data[] = {131, 110, 4, 0, 0, 0, 0, 128};
	const bert_buff_t buff = {8, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_SMALL_BIG);
	CuAssertTrue(tc, term->sbig == (int64_t)2147483648ul);
	bert_free(term);
}

void TestSmallBigNegDecode(CuTest *tc) {
	const uint8_t data[] = {131, 110, 8, 1, 255, 255, 255, 255, 255, 255, 255,
		127};
	const bert_buff_t buff = {12, (uint8_t*) data};

	bert_term_t* term = bert_decode( &buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_SMALL_BIG);
	CuAssertTrue(tc, term->sbig == (int64_t)-9223372036854775807ull);
	bert_free(term);
}

void TestSmallBigTooSmallDecode(CuTest *tc) {
	// -9223372036854775809
	const uint8_t data[] = {131, 110, 8, 1, 1, 0, 0, 0, 0, 0, 0,
		128};
	const bert_buff_t buff = {12, (uint8_t*) data};

	bert_term_t* term = bert_decode( &buff);
	CuAssertTrue(tc, term == NULL);
}

void TestTinyTupleEncode(CuTest *tc) {
	bert_term_t* integer[] = {create_sint(1)};

	bert_term_t* tuple = create_stuple(1, (bert_term_t**) integer);

	const uint8_t expected[] = {131, 104, 1, 97, 1};

	bert_buff_t* buff = bert_encode(tuple);
	bert_free(tuple);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 5));
	bfree(buff);
}

void TestTupleEncode(CuTest *tc) {
	bert_term_t** integers = bmalloc(sizeof(bert_term_t*) * 50);
	int i;
	for (i = 0; i < 50; i++) {
		*(integers + i) = create_sint(i);
	}
	bert_term_t* tuple = create_stuple(50, integers);

	uint8_t* expected = bmalloc(sizeof(uint8_t) * 103);
	*expected = 131;
	*(expected + 1) = 104;
	*(expected + 2) = 50;
	for (i = 0; i < 50; i++) {
		const uint8_t data[] = {97, i};
		memcpy(expected + 3 + i * 2, data, sizeof(uint8_t) * 2);
	}

	bert_buff_t* buff = bert_encode(tuple);
	bert_free(tuple);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 103));
	bfree(buff);

}

void TestTinyTupleDecode(CuTest *tc) {
	const uint8_t data[] = {131, 104, 1, 97, 1};
	const bert_buff_t buff = {5, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_SMALL_TUPLE);
	CuAssertTrue(tc, term->stuple.size == 1);
	CuAssertTrue(tc, ((*term->stuple.values))->type == BERT_SMALL_INTEGER);
	bert_term_t* sint = (*term->stuple.values);
	CuAssertTrue(tc, sint->sint == 1);

	bert_free(term);
}

void TestTupleDecode(CuTest *tc) {
	int i;
	uint8_t* data = bmalloc(sizeof(uint8_t) * 103);
	*data = 131;
	*(data + 1) = 104;
	*(data + 2) = 50;
	for (i = 0; i < 50; i++) {
		const uint8_t sint_data[] = {97, i};
		memcpy(data + 3 + i * 2, sint_data, sizeof(uint8_t) * 2);
	}

	const bert_buff_t buff = {103, (uint8_t*) data};
	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_SMALL_TUPLE);

	CuAssertTrue(tc, term->stuple.size == 50);

	for (i = 0; i < term->stuple.size; i++) {
		CuAssertTrue(tc, ((*(term->stuple.values+i)))->type == BERT_SMALL_INTEGER);
		bert_term_t* sint = (*(term->stuple.values + i));
		CuAssertTrue(tc, sint->sint == i);
	}
	bert_free(term);
	bfree(data);
}

void TestTupleNestedEncode(CuTest *tc) {
	bert_term_t* integers[] = {create_sint(1)};

	bert_term_t* nested_tuple[] = {create_stuple(1, (bert_term_t**) integers)};

	bert_term_t* tuple = create_stuple(1, (bert_term_t**) nested_tuple);

	const uint8_t expected[] = {131, 104, 1, 104, 1, 97, 1};

	bert_buff_t* buff = bert_encode(tuple);
	bert_free(tuple);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, buff->length == 7);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 7));
	bfree(buff);
}

void TestTupleNestedDecode(CuTest *tc) {
	const uint8_t data[] = {131, 104, 1, 104, 1, 97, 1};
	const bert_buff_t buff = {7, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_SMALL_TUPLE);
	bert_term_t* stuple = term;
	CuAssertTrue(tc, term->stuple.size == 1);
	CuAssertTrue(tc, ((*term->stuple.values))->type == BERT_SMALL_TUPLE);

	stuple = (*term->stuple.values);
	CuAssertTrue(tc, stuple->stuple.size == 1);

	CuAssertTrue(tc, ((*stuple->stuple.values))->type == BERT_SMALL_INTEGER);
	bert_term_t* sint = (*stuple->stuple.values);
	CuAssertTrue(tc, sint->sint == 1);

	bert_free(term);
}

void TestAtomEncode(CuTest *tc) {
	bert_term_t* term = create_atom("a");
	const uint8_t expected[] = {131, 100, 0, 1, 97};

	bert_buff_t* buff = bert_encode(term);
	bert_free(term);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 5 == buff->length);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 5));
	bfree(buff);
}

void TestAtomDecode(CuTest *tc) {
	const uint8_t data[] = {131, 100, 0, 1, 97};
	const bert_buff_t buff = {5, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_ATOM);
	bert_term_t* atom = term;
	CuAssertTrue(tc, strcmp(atom->atom,"a") == 0);
	bert_free(term);
}

void TestTinyListEncode(CuTest *tc) {
	bert_term_t* atoms[] = {create_atom("a")};

	bert_term_t* list = create_list(1, (bert_term_t**) atoms);

	const uint8_t expected[] = {131, 108, 0, 0, 0, 1, 100, 0, 1, 97, 106};

	bert_buff_t* buff = bert_encode(list);
	bert_free(list);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 5));
	bfree(buff);
}

void TestTinyListDecode(CuTest *tc) {
	const uint8_t data[] = {131, 108, 0, 0, 0, 1, 100, 0, 1, 97, 106};
	const bert_buff_t buff = {11, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_LIST);
	CuAssertTrue(tc, term->list != NULL);
	bert_term_t* atom = term->list->value;
	CuAssertTrue(tc, atom->type == BERT_ATOM);
	CuAssertTrue(tc, strcmp(atom->atom,"a") == 0);
	CuAssertTrue(tc, term->list->next == NULL);
	bert_free(term);
}

void TestListEncode(CuTest *tc) {
	bert_term_t* terms[] = {create_atom("a"), create_sint(1)};
	bert_term_t* list = create_list(2, terms);

	const uint8_t expected[] =
	{	131, 108, 0, 0, 0, 2, 100, 0, 1, 97, 97, 1, 106};

	bert_buff_t* buff = bert_encode(list);
	bert_free(list);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 5));
	bfree(buff);
}

void TestListDecode(CuTest *tc) {
	const uint8_t data[] = {131, 108, 0, 0, 0, 2, 100, 0, 1, 97, 97, 1, 106};
	const bert_buff_t buff = {13, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_LIST);
	bert_list_t* list = term->list;
	CuAssertTrue(tc, list != NULL);

	CuAssertTrue(tc, list->value->type == BERT_ATOM);
	CuAssertTrue(tc, strcmp(list->value->atom,"a") == 0);
	CuAssertTrue(tc, list->next != NULL);
	list = list->next;
	CuAssertTrue(tc, list->value != NULL);
	CuAssertTrue(tc, list->value->type == BERT_SMALL_INTEGER);
	CuAssertTrue(tc, list->value->sint == 1);
	CuAssertTrue(tc, list->next == NULL);
	bert_free(term);
}

void TestByteListEncode(CuTest *tc) {
	uint8_t sints[] = {1, 2};

	bert_term_t* list = create_byte_list(2, sints);

	const uint8_t expected[] = {131, 107, 0, 2, 1, 2};

	bert_buff_t* buff = bert_encode(list);
	bert_free(list);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 5));
	bfree(buff);
}

void TestByteListDecode(CuTest *tc) {
	const uint8_t data[] = {131, 107, 0, 2, 1, 2};
	const bert_buff_t buff = {6, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_BYTE_LIST);
	bert_byte_list_t list = term->byte_list;
	CuAssertTrue(tc, list.value != NULL);
	CuAssertTrue(tc, list.length == 2);
	CuAssertTrue(tc, *(list.value) == 1);
	CuAssertTrue(tc, *(list.value+1) == 2);
	bert_free(term);
}

void TestBooleanTrueEncode(CuTest *tc) {

	bert_term_t* term = create_boolean(1);
	const uint8_t expected[] = {131, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100,
		0, 4, 116, 114, 117, 101};

	bert_buff_t* buff = bert_encode(term);
	bert_free(term);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 17 == buff->length);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 3));
	bfree(buff);
}

void TestBooleanTrueDecode(CuTest *tc) {
	const uint8_t data[] = {131, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0,
		4, 116, 114, 117, 101};
	const bert_buff_t buff = {17, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_BOOLEAN);
	CuAssertTrue(tc, term->boolean);
	bert_free(term);
}

void TestBooleanFalseEncode(CuTest *tc) {

	bert_term_t* term = create_boolean(0);
	const uint8_t expected[] = {131, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100,
		0, 5, 102, 97, 108, 115, 101};

	bert_buff_t* buff = bert_encode(term);
	bert_free(term);
	CuAssertTrue(tc, buff != NULL );
	CuAssertTrue(tc, 18 == buff->length);
	CuAssertTrue(tc, 0 == memcmp(buff->buff, expected, 3));
	bfree(buff);
}

void TestBooleanFalseDecode(CuTest *tc) {
	const uint8_t data[] = {131, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0,
		5, 102, 97, 108, 115, 101};
	const bert_buff_t buff = {18, (uint8_t*) data};

	bert_term_t* term = bert_decode(&buff);
	CuAssertTrue(tc, term != NULL);

	CuAssertTrue(tc, term->type == BERT_BOOLEAN);
	CuAssertTrue(tc, !term->boolean);
	bert_free(term);
}

CuSuite* getBertSuite() {
	CuSuite* suite = CuSuiteNew();
	// Integers
	SUITE_ADD_TEST(suite, TestSmallIntEncode);
	SUITE_ADD_TEST(suite, TestSmallIntDecode);
	SUITE_ADD_TEST(suite, TestLargeIntEncode);
	SUITE_ADD_TEST(suite, TestLargeIntDecode);

	// Bigs
	SUITE_ADD_TEST(suite, TestSmallBigPosEncode);
	SUITE_ADD_TEST(suite, TestSmallBigNegEncode);
	SUITE_ADD_TEST(suite, TestSmallBigPosDecode);
	SUITE_ADD_TEST(suite, TestSmallBigNegDecode);
	SUITE_ADD_TEST(suite, TestSmallBigTooSmallDecode);

	// Tuples
	SUITE_ADD_TEST(suite, TestTinyTupleEncode);
	SUITE_ADD_TEST(suite, TestTupleEncode);
	SUITE_ADD_TEST(suite, TestTinyTupleDecode);
	SUITE_ADD_TEST(suite, TestTupleDecode);
	SUITE_ADD_TEST(suite, TestTupleNestedEncode);
	SUITE_ADD_TEST(suite, TestTupleNestedDecode);

	// Atoms
	SUITE_ADD_TEST(suite, TestAtomEncode);
	SUITE_ADD_TEST(suite, TestAtomDecode);

	// Lists
	SUITE_ADD_TEST(suite, TestTinyListEncode);
	SUITE_ADD_TEST(suite, TestTinyListDecode);
	SUITE_ADD_TEST(suite, TestListEncode);
	SUITE_ADD_TEST(suite, TestListDecode);

	// Byte lists
	SUITE_ADD_TEST(suite, TestByteListEncode);
	SUITE_ADD_TEST(suite, TestByteListDecode);

	// Boolean
	SUITE_ADD_TEST(suite, TestBooleanTrueEncode);
	SUITE_ADD_TEST(suite, TestBooleanTrueDecode);
	SUITE_ADD_TEST(suite, TestBooleanFalseEncode);
	SUITE_ADD_TEST(suite, TestBooleanFalseDecode);

	return suite;
}

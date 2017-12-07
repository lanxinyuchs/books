/**
 * @author Karl Gäfvert
 * @author Rasmus Linusson
 */

#include "ComtEUtilsTest.h"
#include "../ComtEUtils_1.h"


void TestStrToUint32(CuTest *tc) {
	CuAssertIntEquals(tc, 55, str_to_uint32("55",2));
	CuAssertIntEquals(tc, 2, str_to_uint32(NULL,2));
}

CuSuite* getComtEUtilsSuite() {
	CuSuite* suite = CuSuiteNew();

	SUITE_ADD_TEST(suite, TestStrToUint32);

	return suite;
}

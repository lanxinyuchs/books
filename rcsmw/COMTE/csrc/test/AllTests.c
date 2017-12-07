#include <stdio.h>
#include "CuTest.h"

// Test headers
#include "bertTest.h"
#include "ComtEUtilsTest.h"
#include "ComOamSpiComtEPmMapTest.h"

void RunAllTests(void) {
	CuString *output = CuStringNew();
	CuSuite* suite = CuSuiteNew();

	CuSuiteAddSuite(suite, getBertSuite());
	CuSuiteAddSuite(suite, getComtEUtilsSuite());
	CuSuiteAddSuite(suite, getComOamSpiComtEPmMapSuite());

	CuSuiteRun(suite);
	CuSuiteSummary(suite, output);
	CuSuiteDetails(suite, output);
	printf("%s\n", output->buffer);
}

int main(void) {
	RunAllTests();
	return 0;
}

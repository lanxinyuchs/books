/**
 * @author Karl Gäfvert
 * @author Rasmus Linusson
 */

#ifndef COMTEUTILSTEST_H
#define COMTEUTILSTEST_H

#include <stdint.h>
#include "CuTest.h"

uint32_t str_to_uint32(char*, uint32_t);

CuSuite* getComtEUtilsSuite();

#endif

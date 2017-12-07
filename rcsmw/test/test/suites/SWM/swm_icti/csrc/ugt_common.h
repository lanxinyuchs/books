/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_common.h %
 * %CCaseRev:	/main/R2A/11 %
 * %CCaseDate:	2013-10-28 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: This file contains both typical header file
 * content and C functions. Several applications may include this file;
 * some duplication of binary code will result but this is not
 * considered a problem.
 *
 * The code is memory-leaky; do not expect it to run endlessly. This is
 * of no importance in intended test scenarios.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R2A/2      2013-08-20 erarafo     Added notOk/4 convenience function.
 * R2A/4      2013-08-21 erarafo     Refactoring, more functions added
 * R2A/5      2013-08-22 erarafo     Added function delaySeconds()
 * R2A/6      2013-08-26 erarafo     Minor refactoring
 * R2A/7      2013-08-26 erarafo     Common functions separately compiled
 * R2A/8      2013-08-27 erarafo     Functionality added
 * R2A/9      2013-10-10 erarafo     Removed CPP legacy
 * R2A/10     2013-10-10 erarafo     Fixed compiler warning
 * R2A/11     2013-10-28 erarafo     Uplift to ICTI IWD PB1
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif


#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/stat.h>
#include <errno.h>
#include <limits.h>
#include <string.h>

#include "safcImmCt.h"

#define STREQ(A, B) (strcmp(A, B) == 0)

#define NO_HANDLE 123456789
#define UNDEFINED 123456789




extern SaUint32T
getVersion(SafcImmCtSchemaVersionT_2 *v);

extern SaUint32T
getRelease(SafcImmCtSchemaVersionT_2 *v);

extern SaUint32T
getCorrection(SafcImmCtSchemaVersionT_2 *v);



extern char
*saDecode(int r);


extern bool
assertEqualsSaUInt32(
    SaUint32T expected,
    SaUint32T actual,
    FILE *log,
    const char *message);


extern char *
stringConcat(const char *a, const char *b);



extern SaImmClassNameT *
oneClassName(const char *name);




extern SaImmClassNameT *
twoClassNames(const char *name0, const char *name1);


// Returns a pointer to a SaStringT (which is a pointer to pointer
// to char).

extern SaStringT *
unDeref(SaStringT s);




// Returns a SaInt32T attribute value. If no attribute matches
// the given attribute name then INT_MIN (-2147483648) is returned.

extern SaInt32T
getAttribute(SafcImmCtInstanceT *inst, const char *attrName);


// TODO, clean up

extern SaStringT
getStringAttribute(SafcImmCtInstanceT *inst, const char *attrName);




// Fully general constructor for single- or multivalued
// attribute

extern SaImmAttrValuesT_2 *
createAttrValue(
    SaImmAttrNameT attrName,
    SaImmValueTypeT type,
    SaUint32T mult,
    SaImmAttrValueT *values);


// Takes a 'void *' value and puts it in a length-1 array.
// A pointer to the allocated array is returned.

extern void **
singletArray(void *x);


// Takes two 'void *' values and puts them in a length-2 array.
// A pointer to the allocated array is returned.

extern void **
doubletArray(void *x, void *y);



// Returns a location that points to an array of
// char.

extern void *
wrapString(char *str);



// Returns an RDN attribute. The given rdnName must
// match the class declaration.

extern SaImmAttrValuesT_2 *
rdn(SaImmAttrNameT rdnName, SaStringT rdnValue);



// Returns the length of the given attribute list.

extern int
nAttrValues(SaImmAttrValuesT_2 **attrValues);



// Returns an empty attribute list.

extern SaImmAttrValuesT_2 **
emptyAttrList();



// Returns a list containing one int32-valued attribute.

extern SaImmAttrValuesT_2 **
oneInt32Attr(SaImmAttrNameT name, SaInt32T value);



// Returns a list containing two int32-valued attributes, each
// having multiplicity 1.

extern SaImmAttrValuesT_2 **
twoInt32Attrs(
    SaImmAttrNameT name0, SaInt32T value0,
    SaImmAttrNameT name1, SaInt32T value1);


// Returns a list containing two simple string-valued attributes.

extern SaImmAttrValuesT_2 **
twoStringAttrs(SaImmAttrNameT name0, SaStringT value0, SaImmAttrNameT name1, SaStringT value1);



// Returns a list containing one string-valued attribute of
// multiplicity 2.

extern SaImmAttrValuesT_2 **
oneStrings2Attr(
		SaImmAttrNameT name, SaStringT value0, SaStringT value1);


extern SaImmAttrValuesT_2 **
appendAttr(SaImmAttrValuesT_2 ** attrs, SaImmAttrNameT attrName, SaInt32T attrValue);






// Returns the length of the given list.

extern int
nInstances(SafcImmCtInstanceT **insts);




// Returns the length of the given list.

extern int
nInstGroups(SafcImmCtInstanceGroupT **groups);



// Returns a SaNameT containing the given text. Truncation
// to 256 characters is enforced if necessary.

extern SaNameT *
wrapName(char *name);



// Returns an instance created from the given
// parameters. The parent name must be in leaf-first
// order, or NULL in case of a root instance.

extern SafcImmCtInstanceT *
inst(char *rdnName,
    char *rdnValue,
    SaNameT *parentName,
     SaImmAttrValuesT_2 **attrValues);



// Returns a null-terminated list of one instance.

extern SafcImmCtInstanceT **
oneInstance(SafcImmCtInstanceT *inst);



// Returns a null-terminated list of two instances.

extern SafcImmCtInstanceT **
twoInstances(SafcImmCtInstanceT *inst0, SafcImmCtInstanceT *inst1);




// Returns an instance group with no instances.

extern SafcImmCtInstanceGroupT *
emptyInstGroup(const char *class);




// Returns an instance group with the given instances.

extern SafcImmCtInstanceGroupT *
instGroup(SaImmClassNameT class, SafcImmCtInstanceT **insts);



// Returns a list containing one instance group.

extern SafcImmCtInstanceGroupT **
oneInstGroup(SafcImmCtInstanceGroupT *instGroup);



// Returns a list containing two instance groups.

extern SafcImmCtInstanceGroupT **
twoInstGroups(
    SafcImmCtInstanceGroupT *instGroup0,
    SafcImmCtInstanceGroupT *instGroup1);



// Returns a list containing three instance groups.

extern SafcImmCtInstanceGroupT **
threeInstGroups(
    SafcImmCtInstanceGroupT *instGroup0,
    SafcImmCtInstanceGroupT *instGroup1,
    SafcImmCtInstanceGroupT *instGroup2);




// Opens a new uniquely named log file.

extern FILE *openLog(char *name);



// Returns false on the first invocation and true
// on every subsequent invocation. Uses the file
// system for persistence.

extern bool hasMark(char *name);



// Call this function to check the result of an ICTI
// interface call. Returns true if the given code r
// is SA_AIS_OK.

extern bool
isOk(
    SaAisErrorT r,
    SafcImmCtHandleT handle,
    FILE *log,
    const char *traceMessage);


// Call this function to check the result of an ICTI
// interface call. Returns true if the given code r
// is not SA_AIS_OK.

extern bool
notOk(
    SaAisErrorT r,
    SafcImmCtHandleT handle,
    FILE *log,
    const char *traceMessage);


// Sleeps for approximately the given number of
// seconds. By passing secondsSleep as 0 the sleep
// period will be 100 ms.

extern void
delaySeconds(time_t secondsSleep);



// Repeatedly sleeps for approximately the given
// number of seconds. By passing secondsSleep as 0
// the sleep period will be 100 ms. A log message
// is written before each sleep period.

extern void
idle(FILE *log, time_t secondsSleep);

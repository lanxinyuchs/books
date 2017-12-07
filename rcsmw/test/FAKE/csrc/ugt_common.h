/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_common.h %
 * %CCaseRev:	/main/R2A/R3A/R4A/R8A/1 %
 * %CCaseDate:	2016-11-25 %
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
 * Used by: ugt_s0, ugt_s1v1, ugt_s1v2, ugt_s2 and noble_icti.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
 * R2A/3      2014-01-18 erarafo     Removed run-once logic
 * R2A/4      2014-01-20 erarafo     New functions added
 * R2A/5      2014-01-20 erarafo     Removed unused functions
 * R2A/6      2014-03-24 erarafo     Support for incremental AUE model
 * R2A/7      2014-03-31 erarafo     Refactoring
 * R2A/9      2014-06-11 erarafo     Added a print utility
 * R3A/1      2015-02-18 erarafo     Added timestamped logging
 * R4A/1      2015-04-27 erarafo     Function for creating uint32 attribute
 * R4A/2      2015-08-20 erarafo     Added a function
 * R8A/1      2016-11-25 erarafo     printInstanceGroups arity
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
#include <signal.h>

#include "safcImmCt.h"
#include "appm_lmhi.h"

#define STREQ(A, B) (A != NULL && B != NULL && strcmp(A, B) == 0)

#define NO_HANDLE 123456789
#define UNDEFINED 123456789

typedef SafcImmCtHandleT Handle;
typedef SaStringT String;

typedef SafcImmCtSchemaVersionT_2 VersionStruct;
typedef VersionStruct *Version;

typedef SafcImmCtInstanceGroupT GroupStruct;
typedef GroupStruct **GroupList;
typedef GroupStruct *Group;

typedef SafcImmCtInstanceT **InstList;
typedef SafcImmCtInstanceT *Inst;
typedef SaImmAttrValuesT_2 **AttrList;
typedef SaImmAttrValuesT_2 *Attr;
typedef SaImmAttrValueT *ValueList;
typedef SaImmAttrValueT Value;

typedef SaImmClassNameT ClassName;
typedef SaNameT *ParentName;             // TODO, the defined type name should be
                                         // more general!
typedef SaImmAttrNameT AttrName;
typedef SaImmValueTypeT AttrType;
typedef SaUint32T Mult;


extern GroupList
consg(Group group, GroupList groupList);

extern unsigned int
sizegl(GroupList groupList);

extern Group
createGroup(ClassName className, InstList instList);


extern InstList
consi(Inst inst, InstList instList);

extern unsigned int
sizeil(InstList instList);

extern Inst
createInst(ParentName parentName, AttrList attrList);


extern AttrList
consa(Attr attr, AttrList attrList);

extern unsigned int
sizeal(AttrList attrList);

extern Attr
createAttr(AttrName attrName, AttrType attrType, Mult mult, ValueList valueList);


extern ValueList
consv(Value value, ValueList valueList);

extern unsigned int
sizevl(ValueList valueList);

extern Value
createValueString(char *string);


extern Group zeroGroups[];
extern Inst zeroInsts[];
extern Attr zeroAttrs[];
extern Value zeroValues[];


extern bool
verLessEq(const VersionStruct *x, const VersionStruct *y);

extern bool
verEq(const VersionStruct *x, const VersionStruct *y);


extern Handle
initialize(char *log);

extern void
finalize(char *log, Handle handle);


extern void
readSchemaVersions(Handle handle, String schema, Version from, Version to);


extern GroupList
readInsts(Handle handle, ClassName className);

extern void
writeInsts(Handle handle, GroupList groupList);





extern char
*saDecode(int r);


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


// Creates a single-valued string-type attribute

extern SaImmAttrValuesT_2 *
stringAttr(
    SaImmAttrNameT attrName,
    SaStringT value);


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



// Returns a list containing one moRef-valued attribute.

extern SaImmAttrValuesT_2 **
oneMoRefAttr(SaImmAttrNameT name, SaStringT value);



// Returns a list containing one int32-valued attribute.

extern SaImmAttrValuesT_2 **
oneInt32Attr(SaImmAttrNameT name, SaInt32T value);



// Returns a list containing two int32-valued attributes, each
// having multiplicity 1.

extern SaImmAttrValuesT_2 **
twoInt32Attrs(
    SaImmAttrNameT name0, SaInt32T value0,
    SaImmAttrNameT name1, SaInt32T value1);



// Returns a list containing one uint32-valued attribute.

extern SaImmAttrValuesT_2 **
oneUint32Attr(SaImmAttrNameT name, SaUint32T value);



// Returns a list containing two simple string-valued attributes.

extern SaImmAttrValuesT_2 **
twoStringAttrs(SaImmAttrNameT name0, SaStringT value0, SaImmAttrNameT name1, SaStringT value1);



// Returns a list containing one string-valued attribute of
// multiplicity 2.

extern SaImmAttrValuesT_2 **
oneStrings2Attr(
		SaImmAttrNameT name, SaStringT value0, SaStringT value1);



// Returns a list containing one moref-valued attribute of
// multiplicity 2.

extern SaImmAttrValuesT_2 **
oneMorefs2Attr(SaImmAttrNameT name, char *ref1, char *ref2);




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



// Prints the given instance groups.

extern void printInstanceGroups(FILE *file, GroupList instanceGroups, bool includeAll);



/**
 * Macro for writing log entries to a file. Use it in fprintf-like
 * style, except that the first argument defines the log name.
 *
 *   APPLOG("my_program", "things are fine");
 *   APPLOG("my_program", "status: %s, count: %d", status, count);
 *
 * The log can be displayed in real time in a shell session:
 *
 *   tail -f rcs/applicationlogs/CXPDIR/my_program.log
 *
 * The use of ##__VA_ARGS__ read is explained here:
 * http://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html
 */
#define APPLOG(NAME, FORMAT, ...) \
  { FILE *APP_LOG_STREAM = appLogAppend(NAME); \
    fprintf(APP_LOG_STREAM, FORMAT "\n", ##__VA_ARGS__); \
    fclose(APP_LOG_STREAM); \
  }




/**
 * Similar to APPLOG, except that each log entry will be
 * timestamped.
 */
#define APPLOG_TIMESTAMP_SIZE 24

#define APPLOG_T(NAME, FORMAT, ...) \
  { FILE *APP_LOG_STREAM = appLogAppend(NAME); \
    char ts[APPLOG_TIMESTAMP_SIZE]; \
    appLogTimestamp(ts); \
    fprintf(APP_LOG_STREAM, "%s " FORMAT "\n", ts, ##__VA_ARGS__); \
    fclose(APP_LOG_STREAM); \
  }



extern FILE *
appLogAppend(char *name);






// Opens a new uniquely named log file.

extern FILE *openLog(char *name);


// Call this function to log the result of an ICTI
// interface operation. Returns true if the given code r
// is SA_AIS_OK.

extern bool
ok(const char *operation, SaAisErrorT r, FILE *log);


// Call this function to log the result of an ICTI
// interface operation. Returns true if the given code r
// is ANYTHING ELSE than SA_AIS_OK.

extern bool
nok(const char *operation, SaAisErrorT r, FILE *log);


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



typedef enum {
   RUN_CONVERTER_OK = 1,
   RUN_CONVERTER_NOT_FOUND = 2,
   RUN_CONVERTER_AMBIGUOUS = 3,
   RUN_CONVERTER_LOOKUP_FAILURE = 4,
   RUN_CONVERTER_SIGNAL_HANDLING_SETUP_FAILURE = 5,
   RUN_CONVERTER_SIGNAL_HANDLING_RESTORE_FAILURE = 6,
   RUN_CONVERTER_START_PROGRAM_FAILURE = 7,
   RUN_CONVERTER_STOP_PROGRAM_FAILURE = 8
} RunConverterErrorT;



/**
 * Runs the specified converter. If log is non-NULL progress
 * messages will be written using APPLOG with the given name.
 * On successful execution RUN_CONVERTER_OK is returned.
 *
 * This function must not be called from more than one thread
 * at a time.
 */
extern RunConverterErrorT
runConverter(const char *name, const char *log);


extern void appLogTimestamp(char *buffer);

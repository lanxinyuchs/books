/* ----------------------------------------------------------------------
 * %CCaseFile:	test_common.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R6A/R8A/1 %
 * %CCaseDate:	2017-01-16 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Functions that are called from multiple modules. Move functions here
 * to avoid duplication of code.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
 * R2A/1      2013-11-07 erarafo     First version.
 * R2A/2      2013-11-19 erarafo     Simpler handling of ERL_SMALL_INTEGER_EXT
 * R2A/4      2014-01-31 erarafo     Added decodeString function
 * R2A/5      2014-02-10 erarafo     Fixed compiler warning
 * R2A/6      2014-09-19 erarafo     Support for tail of text files
 * R2A/7      2014-09-22 erarafo     Added function envExpand()
 * R3A/1      2014-09-26 erarafo     Added maxLines in getTailLines()
 * R4A/1      2015-11-26 erarafo     Wrappers around TRI trace macros
 * R6A/1      2016-06-20 erarafo     Comments only
 * R8A/1      2017-01-16 erarafo     Add function decodeToU32()
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "master.h"
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>

extern bool TRI_trace_enabled;

/**
 * Decodes a number in the range [-2^31 .. (2^31-1)]
 * from the given ei_x_buff. Handles the case where the
 * number is encoded as a single byte (it is then trusted
 * that the 8 bits represent a number 0..255, as verified by
 * helpful people on the Erlang Questions list).
 *
 * If the Erlang type is not the expected one then a failure
 * is indicated, by returning a value that is 2^31 plus the
 * unknown type code. Type codes are in the range 65..126
 * as defined in ei.h (not all of those codes are used).
 * Invokers of this function must test against this kind
 * of failure.
 */
long long
decodeInteger(ei_x_buff *buffer) {
  int type;
  int size;
  ei_get_type(buffer->buff, &(buffer->index), &type, &size);
  if (type == ERL_SMALL_INTEGER_EXT) {
    // non-negative integer that fits in 8 bits
    char bits8;
    ei_decode_char(buffer->buff, &(buffer->index), &bits8);
    return bits8 >= 0 ? (long long)bits8 : (long long)bits8 + 256LL;
  }
  else if (type == ERL_INTEGER_EXT) {
    // other integer in 32-bits range
    long value;
    ei_decode_long(buffer->buff, &(buffer->index), &value);
    long long valueL = (long long)value;
    return valueL;
  }
  else {
    // unknown type; return 2^31 plus type
    long long resultL = TWO_TO_31 + (long long)type;
    return resultL;
  }
}

/**
 * See master.h for a description.
 */
U32
decodeToU32(ei_x_buff *buffer) {
  unsigned long u;
  ei_decode_ulong(buffer->buff, &(buffer->index), &u);
  return (U32)u;
}

/**
 * Returns string length, or a negative value in case of error. This
 * value must be checked by the caller.
 *
 * An empty string causes a zero result to be returned. Note that
 * this case must then be decoded specially. See the decodeString
 * function for an example.
 */
int
stringLength(ei_x_buff *buffer) {
  int type;
  int stringLength;
  if (ei_get_type(buffer->buff, &(buffer->index), &type, &stringLength) == -1) {
    QTRACE_ERROR1("cannot determine string length");
    return -2000000;
  }
  else if (type == ERL_NIL_EXT) {
    return 0;
  }
  else if (type != ERL_STRING_EXT && type > 0) {
    QTRACE_ERROR2("cannot determine string length: type is: %d", type);
    return -type;
  }
  else if (type != ERL_STRING_EXT) {
    QTRACE_ERROR2("cannot determine string length: type is: %d", type);
    return -1000000 + type;
  }
  else {
    return stringLength;
  }
}

/**
 * Decodes a string from the given buffer. The case of an empty
 * string is supported.
 *
 * The returned value is a pointer to a buffer which may be freed
 * when no longer used.
 */
char *decodeString(ei_x_buff *buffer) {
  const int length = stringLength(buffer);
  if (length < 0) {
    char *result;
    asprintf(&result, "###_error_when_decoding_string_###");
    return result;
  }
  else {
    char *result = (char *)calloc(length+1, sizeof(char));
    if (length == 0) {
      ei_skip_term(buffer->buff, &(buffer->index));
    }
    else {
      ei_decode_string(buffer->buff, &(buffer->index), result);
    }
    return result;
  }
}

/**
 * Decodes an SA Forum session handle. The given buffer is trusted
 * to contain a string consisting of decimal digits, forming a
 * non-negative integer in the unsigned long long range. The returned
 * value is the parsed integer.
 *
 * SA Forum interfaces use handles which are effectively unsigned
 * long long; encoding them as strings makes it safe and easy to
 * pass handles back and forth to test scripts.
 */
unsigned long long decodeHandle(ei_x_buff *buffer) {
  char *numeral = decodeString(buffer);
  unsigned long long result = strtoull(numeral, NULL, 0);
  free(numeral);
  return result;
}


#define IS_NAMECHAR(C) \
  ((C >= 'A' && C <= 'Z') || \
      (C >= 'a' && C <= 'z') || \
      (C >= '0' && C <= '9') || \
      C == '_')


/**
 * Expands the given string which may contain environment
 * variables. For example, "$LOG_DIR/tmp/" might expand to
 * /rcs/applicationlogs/DUMMY-ARM_CXP9021691_3/tmp/.
 *
 * The caller should free the allocated string when no
 * longer needed.
 */
char *envExpand(char *string) {
  // Parse the line, collect resolved variable values
  int growth = 0;
  LineList *varValues = NULL;
  LineList *varValuesTail = NULL;
  for (unsigned int i = 0; string[i] != '\0';) {
    if (string[i] != '$') {
      i++;
    }
    else {
      unsigned int z = i;
      for (unsigned int j = i+1; IS_NAMECHAR(string[j]); j++) {
        z = j;
      }
      unsigned int varNameLen = z-i;
      char *varValue;
      if (varNameLen == 0) {
        varValue = "$";
      }
      else {
        char *varName = (char *)calloc(varNameLen+1, sizeof(char));
        for (unsigned int k = 0; k < varNameLen; k++) {
          varName[k] = string[i+k+1];
        }
        varValue = getenv(varName);
        free(varName);
        varValue = varValue == NULL ? "" : varValue;
      }
      growth += strlen(varValue) - varNameLen - 1;
      if (varValues == NULL) {
        varValues = (LineList *)calloc(1, sizeof(LineList));
        varValuesTail = varValues;
      }
      else {
        varValuesTail->next = (LineList *)calloc(1, sizeof(LineList));
        varValuesTail = varValuesTail->next;
      }
      varValuesTail->line = varValue;
      i = z+1;
    }
  }

  // Build the expanded line
  char *result = (char *)calloc(strlen(string)+growth+1, sizeof(char));
  LineList *nextValue = varValues;
  unsigned int q = 0;
  for (unsigned int i = 0; string[i] != '\0';) {
    if (string[i] != '$') {
      result[q] = string[i];
      i++;
      q++;
    }
    else {
      for (unsigned int j = 0; nextValue->line[j] != '\0'; j++) {
        result[q] = nextValue->line[j];
        q++;
      }
      nextValue = nextValue->next;
      for (i++; IS_NAMECHAR(string[i]); i++) {
      }
    }
  }

  freeLines(varValues, false);
  return result;
}



int countLines(
    char *dirname,
    char *filename,
    unsigned int *result,
    int* errorNumber) {
  *errorNumber = 0;
  char *pathName;
  if (dirname == NULL) {
    asprintf(&pathName, "%s/%s", getenv("LOG_DIR"), filename);
  }
  else {
    asprintf(&pathName, "%s/%s", dirname, filename);
  }
  FILE *instream = fopen(pathName, "r");
  free(pathName);
  if (instream == NULL) {
    *errorNumber = errno;
    *result = 0;
    return TEXTFILE_CANNOT_READ;
  }
  for (unsigned int count = 0; true;) {
    int c = fgetc(instream);
    if (c == EOF) {
      fclose(instream);
      *result = count;
      break;
    }
    else if (c == '\n') {
      count++;
    }
  }
  return TEXTFILE_OK;
}

int getTailLines(
    char *dirname,
    char *filename,
    unsigned int skipLines,
    int maxLines,
    LineList **result,
    int* errorNumber) {
  *errorNumber = 0;
  char *pathName;
  if (dirname == NULL) {
    asprintf(&pathName, "%s/%s", getenv("LOG_DIR"), filename);
  }
  else {
    asprintf(&pathName, "%s/%s", dirname, filename);
  }
  FILE *instream = fopen(pathName, "r");
  free(pathName);
  if (instream == NULL) {
    *errorNumber = errno;
    *result = 0;
    return 1;
  }
  LineList *r = NULL;
  LineList *s = NULL;
  unsigned int resultLinesCount = 0;
  for (unsigned int count = 0; true; ) {

    // read next well-formed line, or return on end-of-file.

    if (maxLines >= 0 && resultLinesCount == maxLines) {
      fclose(instream);
      *result = r;
      break;
    }

    char *line;
    int c = fgetc(instream);
    if (c == EOF) {
      // well-behaved return
      fclose(instream);
      *result = r;
      break;
    }
    else if (c == '\n') {
      line = (char *)calloc(1, sizeof(char));
      count++;
    }
    else {
      int y = ungetc(c, instream);
      if (y == EOF) {
        fclose(instream);
        *result = r;
        return TEXTFILE_CANNOT_UNGETC;
      }
      // at this point we know we have a non-empty line ahead
      int x = fscanf(instream, "%m[^\n]", &line);
      if (x == 1) {
        count++;
        // Consume the EOL char. No error expected.
        int n = fgetc(instream);
        if (n == EOF) {
          fclose(instream);
          *result = r;
          return TEXTFILE_UNEXPECTED_ERROR_A;
        }
      }
      else if (errno != 0) {
        *errorNumber = errno;
        fclose(instream);
        *result = r;
        return TEXTFILE_READ_ERROR;
      }
      else {
        fclose(instream);
        *result = r;
        return TEXTFILE_UNEXPECTED_ERROR_B;
      }
    }

    // collect the string that was read
    if (count > skipLines && r == NULL) {
      LineList *q = calloc(1, sizeof(LineList));
      q->line = line;
      q->next = NULL;
      r = q;
      s = q;
    } else if (count > skipLines) {
      s->next = calloc(1, sizeof(LineList));
      s = s->next;
      s->line = line;
      s->next = NULL;
    }
    resultLinesCount++;
  }
  return TEXTFILE_OK;
}

unsigned int lineListSize(LineList *list) {
  unsigned int count = 0;
  for (LineList *p = list; p != NULL; p = p->next) {
    count++;
  }
  return count;
}

void freeLines(LineList *lines, bool all) {
  for (LineList *p = lines; p != NULL; ) {
    LineList *q = p->next;
    if (all) {
      free(p->line);
    }
    free(p);
    p = q;
  }
}

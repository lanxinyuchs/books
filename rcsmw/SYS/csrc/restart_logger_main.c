/* ----------------------------------------------------------------------
 * %CCaseFile:	restart_logger_main.c %
 * %CCaseRev:	/main/R2A/4 %
 * %CCaseDate:	2014-08-19 %
 * %CCaseDocNo: %
 * Author:	erarafo
 *
 * Short description: Main program of restart_logger
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014 All rights reserved.
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
 * R2A/1      2014-03-17 erarafo     Pieced together from previous code
 * R2A/2      2014-03-20 erarafo     Cleanup
 * R2A/3      2014-08-19 erarafo     Using CXA11481
 * R2A/4      2014-08-19 erarafo     Compiler warnings fixed
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>

#define TRACEPOINT_DEFINE
#include "com_ericsson_system_start.h"

#define PROCESS_NAME "restart_logger"
#define DEBUG_LOG_FILENAME "restart_logger.log"

/**
 * Macro for debug printouts to file. For example,
 *
 *   DEBUGLOG("things are fine");
 *   DEBUGLOG("status: %s, count: %d", status, count);
 *
 * The use of ##__VA_ARGS__ read is explained here:
 * http://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html
 */
#define DEBUGLOG(FORMAT, ...) \
  { FILE *STREAM = logStream(); \
    if (STREAM != NULL) { \
      fprintf(STREAM, FORMAT "\n", ##__VA_ARGS__); \
      fclose(STREAM); \
    } \
  }


/**
 * If the log directory is provided, return a stream,
 * otherwise return NULL.
 */
FILE *logStream() {
  const char *logDir = getenv("DEBUG_LOG_DIR");
  if (logDir == NULL) {
    return NULL;
  }
  else {
    char *logFile = NULL;
    asprintf(&logFile, "%s/%s", logDir, DEBUG_LOG_FILENAME);
    FILE *result = fopen(logFile, "a");
    free(logFile);
    return result;
  }
}


static size_t readExact(unsigned char *buffer, size_t length) {
  DEBUGLOG("enter readExact, length: %u", length);
  size_t bytesGot = 0;
  do {
    const int i = read(STDIN_FILENO, buffer+bytesGot, length-bytesGot);
    DEBUGLOG("read, result: %d", i);
    if (i == 0) {
      DEBUGLOG("readExact, EOF, returning 0");
      return 0;
    }
    else if (i < 0) {
      DEBUGLOG("readExact, ERROR, i: %d, returning: 0", i);
      return 0;
    }
    else {
      bytesGot += i;
    }
  } while (bytesGot < length);
  DEBUGLOG("readExact returing: %u", length);
  return length;
}


/**
 * Reads a command from standard input. The return value is
 * -1 in case of end of file, otherwise it is the size of
 * the read data.
 *
 * The allocated buffer holds a terminating zero byte. The
 * caller should deallocate the buffer when no more needed.
 */
static int readCommand(unsigned char **bufferPointer) {
  DEBUGLOG("readCmd");
  unsigned char sizeBuffer[2];
  if (readExact(sizeBuffer, 2) != 2) {
    DEBUGLOG("readCmd returning: -1");
    return -1;
  }
  else {
    uint16_t *sizeNt = (uint16_t *)sizeBuffer;
    size_t size = (size_t)ntohs(*sizeNt);
    *bufferPointer = (unsigned char *)calloc(size+1, sizeof(unsigned char));
    size_t result = readExact(*bufferPointer, size);
    DEBUGLOG("readCmd returning: %u", result);
    return (int)result;
  }
}


/**
 * Writes an exact number of bytes to standard output.
 * The given length (number of bytes) is trusted to be
 * 1 or more.
 */
static int writeExact(unsigned char *buffer, int length) {
  DEBUGLOG("writeExact, length: %d", length);
  int bytesWritten = 0;
  do {
    const int i = write(STDOUT_FILENO, buffer+bytesWritten, length-bytesWritten);
    if (i == 0) {
      // nothing written, consider this an error
      return 0;
    }
    else if (i < 0) {
      // error
      return i;
    }
    else {
      bytesWritten += i;
    }
  } while (bytesWritten < length);
  return length;
}


static int writeCommand(unsigned char *buffer, int length) {
  DEBUGLOG("writeCommand, length: %d", length);

  unsigned char twoChar[2];
  uint16_t *sizeNt;
  sizeNt = (uint16_t *)twoChar;
  *sizeNt = htons((uint16_t)length);
  writeExact(twoChar, 2);
  return writeExact(buffer, length);
}


/**
 * Prohibit this program from leaving core dumps.
 */
static void limits() {
  struct rlimit rlimitCore = {.rlim_max=0, .rlim_cur=0};
  int r = setrlimit(RLIMIT_CORE, &rlimitCore);
  if (r != 0) {
    DEBUGLOG("failed to zero RLIMIT_CORE: %s", strerror(errno));
  }
  else {
    DEBUGLOG("RLIMIT_CORE set to zero");
  }
}


typedef struct {
  char *head;
  char *tail;
} cons;


static cons *decodeString(const char *buffer) {
  char length[4];
  strncpy(length, buffer, 3);
  length[3] = (char)0;
  const int m = atoi(length);
  char *r = (char *)calloc(m+1, sizeof(char));
  strncpy(r, buffer+3, m);
  cons *result = (cons *)calloc(1, sizeof(cons));
  result->head = r;
  result->tail = (char *)buffer+3+m;
  return result;
}


/**
 * Execution begins here.
 */
int main() {
  DEBUGLOG("===== %s starting", PROCESS_NAME);
  limits();
  for (bool readMore = true; readMore; ) {
    unsigned char *inBuffer = NULL;
    const int r = readCommand(&inBuffer);
    if (r < 0) {
      readMore = false;
    }
    else {
      DEBUGLOG("request received");
      for (size_t k = 0; k < r; k++) {
        DEBUGLOG("buffer[%d] = %d", k, inBuffer[k]);
      }
      cons *fileAndLine = decodeString((char *)inBuffer);
      cons *msg = decodeString(fileAndLine->tail);
      free(inBuffer);

      char *message;
      asprintf(&message, "%s, %s, %s", PROCESS_NAME, fileAndLine->head, msg->head);
      event_system_start(message);
      free(message);

      free(msg->head);
      free(msg);
      free(fileAndLine->head);
      free(fileAndLine);
      unsigned char buffer[2] = {'o', 'k'};
      writeCommand(buffer, 2);
    }
  }
  DEBUGLOG("===== %s terminating", PROCESS_NAME);
}

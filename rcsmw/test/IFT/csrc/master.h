/* ----------------------------------------------------------------------
 * %CCaseFile:	master.h %
 * %CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R8A/R10A/R11A/4 %
 * %CCaseDocNo: %
 * Author:	etxivri
 * Author: Ivan Ribrant, <ivan.ribrant@ericsson.com>
 *
 * Short description:
 * master.h .
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
 * R1A/2      2012-08-31 etxivri     Created
 * R2A/12     2013-11-07 erarafo     Adjustments for fighting code duplication
 * R2A/13     2013-12-06 erarafo     APPLOG macro added
 * R2A/14     2014-01-28 erarafo     Support for AVC tests
 * R2A/15     2014-01-30 erarafo     Added 'decodeHandle' function
 * R2A/16     2014-01-31 erarafo     Added 'decodeString' function
 * R2A/17     2014-02-18 etxpejn     Support for LIHI tests
 * R2A/22     2014-08-26 erarafo     Support for simple self-test
 * R3A/1      2014-09-05 etxpejn     Added FI
 * R3A/1      2014-09-15 eolaand     Added PMI2
 * R3A/3      2014-09-19 erarafo     Merged additions from R2
 * R3A/4      2014-09-22 erarafo     Merged changes from R2A/24
 * R3A/5      2014-09-26 erarafo     Added maxLines in getTailLines()
 * R3A/6      2014-11-20 etxasta     Added SECI
 * R3A/7      2014-11-26 etxasta     Added PEI
 * R3A/8      2014-12-15 etxpeno     Added CHI
 * R3A/9      2015-01-29 erarafo     Timestamps in APPLOG entries
 * R3A/10     2015-01-30 erarafo     Revert to R3A/8
 * R3A/10     2015-01-30 erarafo     Resurrect R3A/9
 * R3A/12     2015-03-21 erarafo     Added IMM
 * R3A/13     2015-03-30 eolaand     Change dispatch from void to int and add
 *                                   dispatch result macros
 * R4A/2      2015-09-02 erarafo     Added TZII
 * R4A/3      2015-11-16 erarafo     Support for sharing proxy pointers
 * R4A/4      2015-11-20 erarafo     Macro wrappers for TRI and stdout trace
 * R4A/5      2015-11-25 erarafo     Macro wrappers for TRI and stdout trace
 * R4A/6      2015-11-25 erarafo     Macro wrappers for TRI and stdout trace
 * R4A/7      2015-11-26 erarafo     Macro wrappers for TRI and stdout trace
 * R5A/1      2015-12-02 erarafo     Macros for tracing of function calls
 * R5A/2      2015-12-02 erarafo     Cleanup
 * R5A/3      2015-12-11 etomist     Added AVLI_ITC
 * R5A/4      2015-12-13 erarafo     APPLOG log levels, separate queues
 * R5A/5      2015-12-17 erarafo     PROTOCOL_NAME macro added
 * R5A/6      2015-12-29 etomist     Removed AVLI_ITC
 * R5A/7      2016-02-19 ekurnik     Added CCI
 * R8A/1      2017-01-16 erarafo     Add function decodeToU32
 * R10A/1     2017-05-08 uabhten     Add CSTN
 * R11A/4     2017-09-13 enekdav     Added SEC_CREDU
 * ----------------------------------------------------------------------
 */


#include <stdbool.h>
#include <osetypes.h>
#include <cello_te_ose.h>
#include <cello_te_trace.h>
#include "socket.h"
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>

#define TWO_TO_31 0x80000000LL

#define ZERO(expr) assert(0 == (expr))
#define LOCK_INIT(lock) ZERO(pthread_rwlock_init(lock, NULL))
#define LOCK_RDLOCK(lock) ZERO(pthread_rwlock_rdlock(lock))
#define LOCK_WRLOCK(lock) ZERO(pthread_rwlock_wrlock(lock))
#define LOCK_UNLOCK(lock) ZERO(pthread_rwlock_unlock(lock))
#define LOCK_DESTROY(lock) ZERO(pthread_rwlock_destroy(lock))

#define MAX_ENDPOINTS 129
#define MAX_ASSOCIATIONS 513
#define START 0
#define SEND 0
#define STOP 0

#define DIE  0
#define LICI 1
#define PRI  2
#define CSI  3
#define VII  4
#define DNTI 5
#define AVLI 6
#define PMI  7
#define OEI  8
#define LMHI 9
#define CMSI 10
#define NTFI 11
#define AVC  12
#define LIHI 13
#define LTTNG 14
#define BUTI 15
#define SELF 16
#define FI   17
#define PMI2 18
#define SECI 19
#define PEI  20
#define CHI  21
#define IMM  22
#define TZII 23
#define CCI  24
#define CSTN 25
#define SEC_CREDU 26

#define PROTOCOL_NAME(P)            \
    P == DIE ? "DIE" :              \
    P == LICI ? "LICI" :            \
    P == PRI ? "PRI" :              \
    P == CSI ? "CSI" :              \
    P == VII ? "VII" :              \
    P == DNTI ? "DNTI" :            \
    P == AVLI ? "AVLI" :            \
    P == PMI ? "PMI" :              \
    P == OEI ? "OEI" :              \
    P == LMHI ? "LMHI" :            \
    P == CMSI ? "CMSI" :            \
    P == NTFI ? "NTFI" :            \
    P == AVC ? "AVC" :              \
    P == LIHI ? "LIHI" :            \
    P == LTTNG ? "LTTNG" :          \
    P == BUTI ? "BUTI" :            \
    P == SELF ? "SELF" :            \
    P == FI ? "FI" :                \
    P == PMI2 ? "PMI2" :            \
    P == SECI ? "SECI" :            \
    P == PEI ? "PEI" :              \
    P == CHI ? "CHI" :              \
    P == IMM ? "IMM" :              \
    P == TZII ? "TZII" :            \
    P == CCI ? "CCI" :              \
    P == CSTN ? "CSTN" :            \
    P == SEC_CREDU ? "SEC_CREDU" :  \
    "???"


#define DISPATCH_OK 1
#define DISPATCH_FAILED -1


// Macros for use within the IFT application. Log entries are appended
// to the APPLOG_BASENAME file in the rcs/applicationlogs/DUMMY*/
// directory. The implementation is thread-safe.
//
// Macros APPLOG_E(), APPLOG_W(), APPLOG_I(), APPLOG_D() are provided
// for severities ERROR, WARNING, INFO and DEBUG respectively. APPLOG()
// can be used as shorthand for APPLOG_I().
//
// APPLOG_LEVEL is A_INFO by default.
//
// Macro arguments should be given as if using printf(), except that no
// '\n' is needed at the end of the format string. For example,
//
// APPLOG_W("resource usage > 80%, count: %d, limit: %d", count, limit);
//
// The log can be displayed in real time in a shell session:
//
//   tail -f rcs/applicationlogs/DUMMY*/ift_app.log
//
// The rct_logging hook knows about the logfile pathname.
// See [WikiRef] TODO for an example.

#define APPLOG_BASENAME "ift_app.log"

#define APPLOG_LEVEL_DEFAULT A_INFO
extern volatile int applogLevel;


#define A_ERROR    1
#define A_WARNING  2
#define A_INFO     3
#define A_DEBUG    4


// The use of ##__VA_ARGS__ read is explained here:
// http://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html
#define APPLOG(FORMAT, ...)   APPLOG_X(A_INFO,    "INFO",    FORMAT, ##__VA_ARGS__)
#define APPLOG_E(FORMAT, ...) APPLOG_X(A_ERROR,   "ERROR",   FORMAT, ##__VA_ARGS__)
#define APPLOG_W(FORMAT, ...) APPLOG_X(A_WARNING, "WARNING", FORMAT, ##__VA_ARGS__)
#define APPLOG_I(FORMAT, ...) APPLOG_X(A_INFO,    "INFO",    FORMAT, ##__VA_ARGS__)
#define APPLOG_D(FORMAT, ...) APPLOG_X(A_DEBUG,   "DEBUG",   FORMAT, ##__VA_ARGS__)

#define APPLOG_X(SEVERITY, SEVERITY_NAME, FORMAT, ...)            \
  if (SEVERITY <= applogLevel) {                                  \
    FILE *APPLOG_STREAM = appLogAppend();                         \
    appLogTimestamp(APPLOG_STREAM);                               \
    fprintf(APPLOG_STREAM, "%s:%d %s " FORMAT "\n",               \
            __FILE__, __LINE__, SEVERITY_NAME, ##__VA_ARGS__);    \
    appLogClose(APPLOG_STREAM);                                   \
  }


/**
 * Macro for tracing a function call that is expected
 * to return orderly. This macro may be useful for function
 * calls that go into libraries and may blow up in there.
 */
#define CALL(FUNCALL, FMT, ...)                    \
  if (applogLevel >= A_DEBUG) {                    \
    long long int tag = makeTag();                 \
    char *s;                                       \
    asprintf(&s, FMT, ##__VA_ARGS__);              \
    APPLOG_D("%12llx--. %s", tag, s);              \
    free(s);                                       \
    FUNCALL;                                       \
    APPLOG_D("%12llx--'", tag);                    \
  }                                                \
  else {                                           \
    FUNCALL;                                       \
}

/**
 * Same as above, with assignment to the provided
 * result variable.
 */
#define RCALL(RESULT, FUNCALL, FMT, ...)           \
  CALL(RESULT = FUNCALL, FMT, ##__VA_ARGS__)


/**
 * Same as above, with declaration of and assignment to
 * the provided result variable.
 */
#define DRCALL(TYPE, RESULT, FUNCALL, FMT, ...)    \
  TYPE RESULT;                                     \
  RCALL(RESULT, FUNCALL, FMT, ##__VA_ARGS__)


/**
 * Macros for making it possible to silence trace messages
 * altogether (just replace right-hand expressions with a
 * single semicolon).
 */
#define QINFO1(S)                  if (TRI_trace_enabled) {INFO(S); }
#define QINFO2(S, V)               if (TRI_trace_enabled) {INFO(STR(S, V)); }
#define QENTER1(S)                 if (TRI_trace_enabled) {ENTER(S); }
#define QENTER2(A, B)              if (TRI_trace_enabled) {ENTER(STR(A, B)); }
#define QENTER3(A, B, C)           if (TRI_trace_enabled) {ENTER(STR(A, B, C)); }
#define QTRACE_ERROR1(A)           if (TRI_trace_enabled) {TRACE_ERROR(A); }
#define QTRACE_ERROR2(A, B)        if (TRI_trace_enabled) {TRACE_ERROR(STR(A, B)); }
#define QTRACE_ERROR3(A, B, C)     if (TRI_trace_enabled) {TRACE_ERROR(STR(A, B, C)); }
#define QTRACE_ERROR4(A, B, C, D)  if (TRI_trace_enabled) {TRACE_ERROR(STR(A, B, C, D)); }
#define QTRACE2(K, A)              if (TRI_trace_enabled) {TRACE(K, A); }
#define QTRACE3(K, A, B)           if (TRI_trace_enabled) {TRACE(K, STR(A, B)); }
#define QTRACE4(K, A, B, C)        if (TRI_trace_enabled) {TRACE(K, STR(A, B, C)); }
#define QTRACE5(K, A, B, C, D)     if (TRI_trace_enabled) {TRACE(K, STR(A, B, C, D)); }
#define QTRACE6(K, A, B, C, D, E) \
    if (TRI_trace_enabled) {TRACE(K, STR(A, B, C, D, E)); }
#define QTRACE7(K, A, B, C, D, E, F) \
    if (TRI_trace_enabled) {TRACE(K, STR(A, B, C, D, E, F)); }
#define QTRACE8(K, A, B, C, D, E, F, G) \
    if (TRI_trace_enabled) {TRACE(K, STR(A, B, C, D, E, F, G)); }
#define QTRACE9(K, A, B, C, D, E, F, G, H) \
    if (TRI_trace_enabled) {TRACE(K, STR(A, B, C, D, E, F, G, H)); }

#define QRETURN1(R)                if (TRI_trace_enabled) {RETURN R; } else {return R; }
#define QRETURN                    if (TRI_trace_enabled) {RETURN; } else {return; }


#define QPRINTF2(A, B)     printf(A, B)
#define QPRINTF3(A, B, C)  printf(A, B, C)
#define QFLUSH1(A)         fflush(A)
#define QPUTS1(A)          puts(A)
#define QPUTSF1(A)         { puts(A); fflush(stdout); }
#define QPRINTFF2(A, B)    { printf(A, B); fflush(stdout); }
#define QPRINTFF3(A, B, C) { printf(A, B, C); fflush(stdout); }

#define UPSTREAM_RESPONSE        "response"
#define UPSTREAM_RESPONSE_MASTER "response"
#define UPSTREAM_SIGNAL          "signal"
#define UPSTREAM_CALLBACK        "callback result"


/**
 * Represents a downstream or upstream message. A shared
 * collection of instances is maintained by the master and
 * child processes.
 */
struct Monitor_flag
{
  PROCESS frompid;
  PROCESS topid;
  erlang_pid from;
  uint32_t protocol;
  uint32_t func;
  ei_x_buff args;
  ei_x_buff childdata;
  struct Monitor_flag *next;
};

/**
 * Represents a downstream message. A shared
 * collection of instances is maintained by the master
 * process.
 */
typedef struct MsgData {
  PROCESS cDest;
  erlang_pid ePid;
  uint32_t prot;
  uint32_t func;
  ei_x_buff buf;
  struct MsgData *next;
} MsgDataT;


/**
 * Represents an upstream message. A shared
 * collection of instances is maintained by the master
 * process.
 */
typedef struct MsgUp {
  PROCESS cSrc;
  const char *clientId;
  erlang_pid eDest;
  ei_x_buff buf;
  const char *context;
  struct MsgUp *next;
} MsgUpT;




/**
 * Pointer to a function that takes a handle (representing
 * a connection between a C interface and the CS). The purpose
 * of the function, which has to be implemented by application
 * code, is to call a dispatch function in the C interface.
 * The PMI interface makes uses of such a scheme.
 */
//typedef void (*DispatcherT)(char *handle);
typedef int (*DispatcherT)(char *handle);

/**
 * Adds a schedule item that specifies that a socket is to be
 * periodically polled for data indicating that a callback
 * should be handled. The PMI interface uses this function.
 */
void addPollScheduleItem(char *handle, int socketFd, DispatcherT dispatcher);

/**
 * Sets the deathMark and socketClose flags. In the next lap
 * of the child loop the thread will be killed, after closing
 * the socket (if the socketClose flag is true).
 */
int setDeathMark(char *handle, bool socketClose);

/**
 * Removes a schedule item. The PMI interface uses this function.
 */
void removePollScheduleItem(char *handle);

/**
 * Sends a callback message to the test suite, to be
 * picked up by a rct_proxy:receive_proxy() call. The
 * PMI interface uses this function.
 */
void sendCallback(ei_x_buff message);


/**
 * Represents a child process (a client towards RBS CS).
 */
struct Client
{
  char *clientId;       // unique client name
  PROCESS pid;          // unique pid
  uint32_t prot;        // protocol
  erlang_pid eParent;   // Erlang parent process pid
  void *proxy_p;  // optional, used by TZII tests
};


/**
 * Acquires the logfile lock, opens the ift_app.log file in
 * append mode and returns a FILE pointer. The caller may
 * use the FILE pointer in write operations, using e g fprintf(),
 * and must then call the appLogClose() function. The time
 * between the two calls must be short and the in-between
 * execution path must be crash-safe.
 */
extern FILE *appLogAppend();


/**
 * To be called promtply after using the appLogAppend
 * function.
 */
extern void appLogClose(FILE *);


/**
 * Writes a space-terminated timestamp to the given
 * output stream.
 */
extern void appLogTimestamp(FILE *stream);


/**
 * Decodes a number in the range [-2^31 .. (2^31-1)]
 * from the given ei_x_buff. Handles the case where the
 * number is in the 0-255 range and encoded as a single byte.
 *
 * If the Erlang type is not the expected one then a failure
 * is indicated, by returning a value that is 2^31 plus the
 * unknown type code. Type codes are in the range 65..126
 * as defined in ei.h (not all of those codes are used).
 * Invokers of this function must test against this kind
 * of failure.
 */
extern long long decodeInteger(ei_x_buff *buffer);

/**
 * Decodes a number from the given ei_x_buff buffer and
 * returns it as a U32 number.
 */
extern U32 decodeToU32(ei_x_buff *buffer);

/**
 * Returns string length, or a negative value in case of error. This
 * value must be checked by the caller.
 *
 * An empty string causes a zero result to be returned. Note that
 * this case must then be decoded specially. See the implementation
 * of decodeString for an example.
 */
extern int stringLength(ei_x_buff *buffer);

/**
 * Decodes a string from the given buffer. The case of an empty
 * string is supported.
 *
 * The returned value is a pointer to a buffer which may be freed
 * when no longer used.
 */
extern char *decodeString(ei_x_buff *buffer);

/**
 * Decodes an SA Forum session handle. The given buffer is expected
 * to contain a string consisting of decimal digits, forming a
 * non-negative integer in the unsigned long long range. The returned
 * value is the parsed integer.
 *
 * SA Forum interfaces use handles which are effectively unsigned
 * long long; encoding them as strings makes it safe and easy to
 * pass handles back and forth to test scripts.
 */
unsigned long long decodeHandle(ei_x_buff *buffer);


// Simple functions for reading text files into memory

#define TEXTFILE_OK 0
#define TEXTFILE_CANNOT_READ 1
#define TEXTFILE_CANNOT_UNGETC 2
#define TEXTFILE_READ_ERROR 3
#define TEXTFILE_UNEXPECTED_ERROR_A 4
#define TEXTFILE_UNEXPECTED_ERROR_B 5

/**
 * Represents a sequence of null-terminated strings.
 */
typedef struct LineListT {
  char *line;
  struct LineListT *next;
} LineList;


/**
 * Returns the number of proper lines in a text file.
 * A trailing line that is not '\n' terminated is not
 * counted. The return value is one of the TEXTFILE_*
 * codes.
 *
 * If the given directory name is NULL then the LOG_DIR
 * is assumed.
 */
extern int countLines(
    char *dirname,
    char *filename,
    unsigned int *result,
    int* errorNumber);

/**
 * Returns the tail part of the content of a text file.
 * Leading lines are skipped as specified. The return value
 * is one of the TEXTFILE_* codes. It is the responsibility
 * of the caller to free the allocated memory.
 *
 * If the given directory name is NULL then the LOG_DIR
 * is assumed.
 *
 * The maxLines argument can be used to limit the size of
 * the returned list. Using -1 means no limit.
 */
extern int getTailLines(
    char *dirname,
    char *filename,
    unsigned int skipLines,
    int maxLines,
    LineList **result,
    int* errorNumber);

/**
 * Copies the given string to a newly allocated null-terminated
 * buffer. Limited bash-style variable expansion is performed,
 * for example /home/$USER/foo will be expanded to something like
 * /home/eqwerty/foo. An undefined variable expands to zero
 * characters. A single '$' that is not the start of a variable
 * name is copied literally.
 *
 * The caller is responsible for freeing the buffer.
 */
extern char *envExpand(char *string);

/**
 * Returns the size of a line list.
 */
extern unsigned int lineListSize(LineList *list);

/**
 * Frees the memory that a line list binds. If the second
 * argument is true then  referred strings are freed too,
 * otherwise only the chain of LineList instances is freed.
 */
extern void freeLines(LineList *lines, bool all);

/**
 * Returns the proxy pointer of the indicated client,
 * or NULL if the lookup was not successful.
 */
extern void *getPeerProxyPointer(PROCESS pid);

/**
 * Returns the number of nanoseconds since start of execution,
 * which is believed to be useful as a reasonably unique
 * tag value for messages.
 */
extern long long int makeTag();


/**
 * Represents a CEC connection between the application and the CS
 * that should be polled for callbacks. If a callback is pending
 * the dispatcher function is to be executed.
 */
typedef struct PollScheduleItem {
        PROCESS pid;
        char *handle;
        int socketFd;
        DispatcherT dispatcher;
        bool deathMark;
        bool closeSocket;
        struct PollScheduleItem *next;
} PollScheduleItemT;



/**
 * Represents a client.
 */
typedef struct ClientData {
  char *id;                 // unique client name
  PROCESS pid;              // unique pid
  uint32_t protocol;        // protocol
  erlang_pid epid;          // Erlang parent process pid

  void *mem_p;              // memory pointer, used by some protocols


  PollScheduleItemT *sched; // schedule for callbacks polling
  struct ClientData *next;

  pthread_rwlock_t schedLock;
} ClientDataT;





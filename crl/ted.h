/**
 *   This file contains two interfaces:
 *   - the interface to the ted server
 *   - the interface to the ted handler
 *
 *   @file ted.h
 *
 *   Copyright (C) 2014-2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef TED_H
#define TED_H

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdint.h>
#include <lttng/lttng.h>
#include <lttng/snapshot.h>
#include <trace_cmd.h>
#include <te_internal.h>

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

#define TED_NAME "ted-supvr"
#define TED_MSGBASE (0x32130)

/* LTTNG wild card provider */
#define LTTNG_PROVIDER "com_"

/* Scope of default command issued */
#define   TED_RUNNING  (0)
#define   TED_RESTART  (1)
#define   TED_PRESET   (2)
#define   TED_UID      (3)
#define   TED_LIST     (4)


/* Options to 'te log read'
 *
 * Note that the values need to be a power of 2 in order to be
 * used as bitmasks.
 */
#define TED_LOG_FCORRELATE (1<<0)
#define TED_LOG_LEVEL      (1<<1)
#define TED_LOG_BRIEF      (1<<2)
#define TED_LOG_UID        (1<<4)

/* Sub directories to the user id directories for an LTTng snapshot */
#define TED_LOG_SUB_DIR   "ust/uid"

/*  Information structure between te command and ted
 *  @param type    - 0 for reading, 1 for clearing
 *  @param options - a bitmask containing specifies command line options:
 *                   TED_LOG_LEVEL, TED_LOG_SHORT
 *  @param uid     - User identity to read log for, -1 read all available uid.
*/
typedef struct {
   int type;
   int options;
   int uid;
} TedInfo;

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

/*
 * Type of command
 */
enum cmd_type {
   TE_ENABLE = 0,
   TE_DISABLE,
   TE_LOG_READ,
   TE_LOG_CLEAR,
   TE_DEFAULT,
   TE_FILTER_SET,
   TE_FILTER_RESET,
   TE_STATUS,
   TE_STATUS_LIST,
   TE_SAVE,
   TE_CONFIG,
   TE_PRESET,
   TE_SESSION_CREATE,
   TE_SESSION_DELETE,
   TE_RESTART,
   TE_ECHO,
   TE_SESSION_VALIDATE,
   TE_SET_CORRELATE
};


enum cmd_handler {
   CMD_TE_HANDLER = 0,
   CMD_TS_HANDLER
};

enum cmd_result {
   CMD_RESULT_OK = 0,         /* If command successfully executed */
   CMD_EVENT_ACTION_FAILED,   /* If action performed is failed */
   CMD_EVENT_NOT_FOUND,       /* If event itself is not found */
   CMD_PRESET_OVERFLOW,       /* If overflow occured when saving presets */
   CMD_SESSION_NOT_FOUND,     /* Requested session not found */
   CMD_SESSION_LIMIT_REACHED, /* If sessions created reached limit */
   CMD_SESSION_OVERFLOW       /* If saved sessions count reached limit */
};


/* ============================================================================
 *   MESSAGE DEFINITIONS
 * ============================================================================
 */

struct session_details {
   uint32_t session_id;
   char time_of_creation[80];
   bool saved;
   struct lttng_session session_list;
};

struct session_info
{
   char session_name[LTTNG_SYMBOL_NAME_LEN];
   int no_of_channels;
   struct Channel_info{
      char name[LTTNG_SYMBOL_NAME_LEN];
      int no_of_events;
      struct lttng_event event_list[1];
   } channel_info[1];
};

#define TED_STATUS_REQ (TED_MSGBASE)
struct ted_status_req {
   uint32_t msg_no;
   enum cmd_type type;
   enum cmd_handler handler;
   char session_name[LTTNG_SYMBOL_NAME_LEN];
   uint32_t session_id;
   char data[1];
};

#define TED_STATUS_RSP (TED_MSGBASE + 1)
struct ted_status_rsp {
   uint32_t msg_no;
   uint16_t success;
   uint16_t last;
   uint32_t session_count;
   struct session_details current_session;
   /* Type of respond per status signal */
   union Data_size{
           uint32_t no_of_events;
           uint32_t no_of_elems;
   } data_size;
   union Data_type{
           struct lttng_event event_list[1];
           char elem[1];
   } data_type;
};

#define TED_CTRL_REQ (TED_MSGBASE + 2)
struct ted_ctrl_req {
   uint32_t msg_no;
   enum cmd_type type;
   int32_t uid;
   uint32_t size;
   uint32_t offset;
   uint32_t scope;
   uint32_t session_id;
   char provider[LTTNG_SYMBOL_NAME_LEN];
   enum cmd_handler handler;
   char session_name[LTTNG_SYMBOL_NAME_LEN];
   char data[1];
};

#define TED_CTRL_RSP (TED_MSGBASE + 3)
struct ted_ctrl_rsp {
   uint32_t msg_no;
   enum cmd_type type;
   uint16_t result;
   uint16_t extra;
   char data[1];
};

/* ============================================================================
 *   FUNCTION PROTOTYPES
 * ============================================================================
 */

/** ======================================================================== */
/**
 *  Print status of an LTTng event.
 *
 *  @param event - event which holds the data of a LTTng event
 */
/* ========================================================================= */
extern void ted_printEvent(const struct lttng_event *event);

/** ======================================================================== */
/**
 *   Creates lttng events string from command arguments
 *       ex: event1 event2 ....eventN
 *
 *   @param  argc         Number of command argument
 *           argv         Command arguments list
 *           eventFormat  output string that contains group of lttng events.
 *                        Need to be freed by caller
 *
 *   @return size         length of the created lttng format string
 *
 *   Caller of the function need to free the 'eventFormat' with
 *   free() call.
 */
/* ========================================================================= */
extern int ted_groupLttngEvents(int argc, const char **argv,
                                char **eventFormat);

/** ======================================================================== */
/**
 *  Read or clear the log.
 *
 *  @param info    - Information structure between te command and ted
 *
 *  @return command result
 */
/** ======================================================================== */
extern int ted_log(TedInfo *info);


/** ======================================================================== */
/**
 *  Send a session restart request to the TED server.
 *
 *  @return command result
 */
/** ======================================================================== */
extern int ted_restart(void);


/* ========================================================================= */
/**
 *   Returns the TED handler which is the mandatory command handler.
 */
/* ========================================================================= */
extern TeTraceHandler* getTedHandler(void);

/* ========================================================================= */
/**
 *   This function maps the definitions of default scope with corresponding
 *   values defined in its handler.
 *   Ex: RUNNING->TED_RUNNING
 *
 *   @return corresponding mapping if successful, TED_RUNNING otherwise
 *
 */
/* ========================================================================= */
extern uint32_t get_scope(Scope scope);

/** ==================================================================== */
/**
 *  Run shell command.
 *
 *  @param  cmd - Command to run, including options.
 *
 *  @return 0 for success or CMD_ERROR for failure.
 *
 */
/* ===================================================================== */
extern int run_shell_cmd(const char *cmd);


#ifdef __cplusplus
}
#endif

#endif /* TED_H */

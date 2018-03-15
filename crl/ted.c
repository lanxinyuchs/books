/**
 *   Trace and error deamon functionality for handling enable, disable,
 *   status req etc on lttng tracepoint providers.
 *
 *   @file ted.c
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
/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2014-12-05 Ranganath & Shreyas
 *   Change  : First revision. Command suppport for "ts"
 *
 *   Revised : 2015-01-23 Ranganath
 *   Change  : Improved default functionality
 *
 *   Revised : 2014-01-26 Nils Carlson <nils.carlson@ericsson.com>
 *   Change  : Add support for te and ts restart commands
 *
 *   Revised : 2014-01-30 Nils Carlson <nils.carlson@ericsson.com>
 *   Change  : Move delay loop to before lttng commands
 *
 *   Revised : 2014-02-03 Ranganadh
 *   Change  : Workaround to enable saved trace sessions during restart.
 *             If interface is not ready before ted is started, an retry
 *             is done for at least 60 seconds.
 *
 *   Revised : 2015-02-05 Daniel Lefwerth
 *   Change  : Refactored te and ts commands.
 *
 *   Revised : 2015-03-17 Nils Carlson
 *   Change  : Improve te log read handling of te restarts
 *
 *   Revised : 2015-03-20 Niranjan Kumar
 *   Change  : tracefile_size is removed. This refers to the maximum
 *             size of trace files on host. Default value is zero.
 *             Setting this leads to wrong value for events_discarded
 *             (A limitation in lttng enable-channel)
 *
 *   Revised : 2015-06-02 Niranjan Kumar
 *   Change  : Number of sub buffers and buffer size is fine tuned for
 *             live streaming sessions to obtain better throughput with
 *             minimum trace loss.
 *
 *   Revised : 2015-06-29 Anette Schött
 *   Change  : HT81939, HT80325, continue to retry to set up saved ts session,
 *             not only for LTTNG_ERR_RELAYD_CONNECT_FAIL error.
 *
 *   Revised : 2015-06-29 Anette Schött
 *   Change  : HT75879, move the creation of ted mailbox as early as
 *             possible.
 *
 *   Revised : 2015-07-23 Ranganadh
 *   Change  : Added checking of session name /session ID before executing
 *             a command in case of ts.
 *
 *   Revised : 2015-08-27 Ranganadh
 *   Change  : Fixed a bug introduced in previous delivery.
 *
 *   Revised : 2015-08-27 Anette Schött
 *   Change  : HT78799, Correction in enable_filter, where return value from
 *             lttng_list_event was incorrect used.
 *
 *   Revised : 2015-08-27 Anette Schött
 *   Change  : HT59474, add execute permission to the SAVED_SESSIONS_PATH
 *             directory and checking errno when doing access() for this
 *             directory.
 *
 *   Revised : 2015-09-17 Anette Schött
 *   Change  : HU16489, setting trace group to default and saving doesn't
 *             survive reboot.
 *
 *   Revised : 2015-09-29 Niklas Damberg
 *   Change  : Add optional parameters to the ts ip command.
 *             The parameters that can be used to alter performance are:
 *             Sub buffer size           - Size of trace buffers.
 *             Number of sub buffers     - Number of trace buffers
 *             Switch timer interval     - Time interval for switching
 *             Data flush timer interval - Time interval for data flushing
 *             TR: HT71981
 *
 *   Revised : 2015-10-14 Anette Schött
 *   Change  : Add inclusion of com_ericsson_system_start.h as now ted is
 *             registering this provider to LTTng and is printing the log
 *             entry that startuplog is ready.
 *
 *   Revised : 2015-10-19 Fredrik Skog
 *   Change  : Changed false ERROR syslog message to INFO.
 *             It was causing problems in the CI testloops.
 *
 *   Revised : 2015-10-30 Fredrik Skog
 *   Change  : Added retries for all lttng_enable_channel calls.
 *
 *   Revised : 2015-11-03 Fredrik Skog
 *   Change  : Increased retry interval from 500 us to 50 ms for
 *             lttng_enable_channel() calls to improve stability.
 *
 *   Revised : 2015-11-11 Niklas Damberg
 *   Change  : Introduce correlate option to ts command.
 *             New argument to restart message added that tells if
 *             correlate settings should be acted upon.
 *             New signal added to set correlate enabled/disabled.
 *
 *   Revised : 2015-11-13 Fredrik Skog
 *   Change  : Increased no. of retries when creating the live stream in
 *             an attempt to fix problems in CI test for secure board.
 *             Added syslog entry when enable channel fails.
 *
 *   Revised : 2015-12-16 Fredrik Skog
 *   Change  : Fixed and secured enable_ts_saved_events().
 *
 *   Revised : 2016-02-10 Anette Schött
 *   Change  : Added functionality to parse snapshot directory structure
 *             to be able to get used user groups. Corrected the usage of
 *             strtok in int_env when DEF_LTTNG_SESSION is used instead
 *             for TE_LTTNG_SESSIONS.
 *
 *   Revised : 2016-03-14 Fredrik Skog
 *   Change  : Replaced len = strlen(lttng_strerror(ret)), which could
 *             cause a segmentation fault, with snprintf.
 *
 *   Revised : 2016-04-18 Anette Schött
 *   Change  : Corrected formatting of IPv4 and IPv6 addresses in
 *             create_live_stream().
 * ========================================================================
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ftw.h>
#include <sys/types.h>
#include <errno.h>
#include <dirent.h>
#include <itc.h>
#include <syslog.h>
#include <sys/queue.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <time.h>
#include "te_internal.h"
#include "com_ericsson_plf_trace_util.h"
#include <sys/time.h>
#include <signal.h>
#include <pthread.h>
#include "ted.h"
#include "trace_cmd.h"
#include <assert.h>

#define MSG_ARRAY_SIZE 10
#define DEF_LTTNG_SESSION "lttng_trace"
#define SNAPSHOT_PATH "/var/volatile/log/lttng"
#define SAVED_EVENTS_FILE "lttng/.saved-events.lttng"
#define SAVED_SESSIONS_PATH "lttng/.live_stream/"

#define LIVE_STREAM_SUB_BUF_SIZE            (32*1024)
#define LIVE_STREAM_NUM_OF_SUB_BUFFERS      (32)

/* Maximum allowed sessions to start */
#define DEFAULT_TS_MAX_NUM_DIAG_STREAM_SESSION 1

/* Maximum preset count which can be saved */
#define MAX_SAVED_PRESET_COUNT         (500)

/* Maximum preset count overall */
#define MAX_PRESET_COUNT               (3000)

/* Filter lttng event handling errors */
#define LTTNG_RSP_ALREADY_ENABLED      (-55)
#define SAMPLE_INTERVAL 100
#define MAX_ADDITIONAL_INFO_LEN 256
#define TRI_PROVIDER_NAME1 "trithread"
#define TRI_PROVIDER_NAME2 "triobjif"

#define MAX_TS_CREATE_RETRIES          (600)
#ifdef ERR
#undef ERR
#endif
#define ERR(fmt, args...)                                                \
        do {                                                                \
                syslog(LOG_ERR, "Error[TED]:%s(): " fmt, __func__, ## args); \
        } while (0)

#ifdef INFO
#undef INFO
#endif
#define INFO(fmt, args...)                                                \
        do {                                                                \
                syslog(LOG_INFO, "Info[TED]:%s(): " fmt, __func__, ## args); \
        } while (0)

enum ss_reboot {
    SS_RB_CONF = 1,
    SS_RB_DONE
};

enum ss_time_adjusted {
    SS_TIME_AFTER_BOOT = 1,
    SS_TIME_ADJUSTED,
    SS_TIME_RESTARTED_TRACES_READ
};

union itc_msg {
        uint32_t msg_no;
        struct ted_status_req te_st_req;
        struct ted_status_rsp te_st_rsp;
        struct ted_ctrl_req te_ctrl_req;
        struct ted_ctrl_rsp te_ctrl_rsp;
};

struct te_sessions {
        char         *name;
        char         *path;
        uint64_t     subbuf_size;     /* bytes */
        uint64_t     no_of_subbuf;    /* power of 2 */
        unsigned int switch_interval; /* usec */
        unsigned int flush_interval;  /* usec */
};

struct presetListElement {
   /* List head of trace items */
   TAILQ_ENTRY(presetListElement) list;
   char *itemInfo;
   bool saved;
   int enable;
};
TAILQ_HEAD(presetListHead, presetListElement);
struct presetListHead teSessionPresetList;

/* Element which holds the data of a session created
 */
struct session_list_element {
   /* List head of session items */
   TAILQ_ENTRY(session_list_element) list;
   /* This structure contains name and ip address
    * of a created session
    */
   struct te_sessions te_session;
   /* This linked list contains all the saved events
    * on this particular session
    */
    struct presetListHead saved_events;
   /* Contains Session Identity allocated to it
    */
   uint16_t session_id;
   bool saved;
   /* This contains time of creation for a session
    */
   char time_of_creation[80];
};

TAILQ_HEAD(session_list_head, session_list_element);
struct session_list_head sessionList;

/*
 * total trace stream sessions available
 */
static int total_session_count = 0;

/*
 * total saved sessions counter
 */
static int saved_session_counter = 0;

/* Array of known sessions to ted, first element is assumed
 * to be the default snapshot session
 */
static struct te_sessions *lttng_sessions = NULL;

/*
 * Flag to know if user has configured to take snapshot at reboot
 */
static enum ss_reboot ted_ss_reboot = SS_RB_CONF;

/*
 * Flag to know if the time has already been adjusted once
 */
static enum ss_time_adjusted ted_ss_time_adjusted = SS_TIME_AFTER_BOOT;
/*
 * ts and te restart counters
 */
static int te_restart_count = 0;
static int ts_restart_count = 0;
static int ts_correlate_enabled = 0;

/*
 * Path to file with saved events
 */
static char *ted_saved_ev_file = NULL;
/*
 * Path to file where live events are available
 */
static char *ted_live_stream_ev_file = NULL;

/* To capture events defined in start up script file */
static struct session_info *te_session_info_at_restart = NULL;

static struct session_info *ts_session_info_at_restart = NULL;

/* Holds the maximum allowed live sessions that can be created
 */
static int maxNumDiagStreamSession = 0;

static int *session_id_vector;

static int ts_create_session_retries = 0;

static int last_lttng_err = 0;

/*
 * Data used to retrieve the user id diretory names
 *  for 'te status -u'
*/
static int dir_names_size;
static int no_of_dirs;
static char *msg_data_pos;
static char *msg_data_start;

static void handle_ts_create_session_retry_tmo(union sigval val);

static int
getlength(const char *string)
{
   int i;

   i = strlen(string);

   if (string[i-1] == '*')
   {
      i--;
   }
   else
   {
      /* Include nullchar */
      i++;
   }
   return i;
}

static int
filterLttngResponse(int responseCode)
{
        int result = 0;
        if (responseCode == LTTNG_RSP_ALREADY_ENABLED) {
                result = 0;
        } else {
                result = responseCode;
        }
        return result;
}

static void init_env()
{
        char *sessions, *tok, *ptr, *live_session_count;
        int count = 0, i;
        char *snapshot_path;

        /* see if alternative path is set */
        if ((snapshot_path = getenv("TE_LTTNG_SNAPSHOT_PATH")) == NULL) {
                snapshot_path = SNAPSHOT_PATH;
        }
        /* Get session name */
        if ((sessions = getenv("TE_LTTNG_SESSIONS")) == NULL) {
                sessions = DEF_LTTNG_SESSION;
        }

        if ((live_session_count = getenv("TS_MAX_NUM_DIAG_STREAM_SESSION")) == NULL) {
                maxNumDiagStreamSession = DEFAULT_TS_MAX_NUM_DIAG_STREAM_SESSION;
        } else {
                maxNumDiagStreamSession = atoi(live_session_count);
        }
        /* Create a interger array to keep track of session id value. Dont free this
         * allocated memory
         */
        session_id_vector = (int *)calloc(maxNumDiagStreamSession, sizeof(int));

        /* Create and intialise the group mask saved list */
        TAILQ_INIT(&teSessionPresetList);
        TAILQ_INIT(&sessionList);

        /* Get number of sessions. */
        ptr = malloc(strlen(sessions) + 1);
        strcpy(ptr, sessions);
        /* NB: strtok returns ptr if no colon is available in the string. */
        tok = strtok(ptr, ":");
        while (tok != NULL) {
                count++;
                tok = strtok(NULL, ":");
        }
        free(ptr);

        /* Get session names and put in list. */
        lttng_sessions = (struct te_sessions *) calloc(count + 1,
                          sizeof(struct te_sessions));
        ptr = malloc(strlen(sessions) + 1);
        strcpy(ptr, sessions);
        tok = strtok(ptr, ":");
        for (i = 0; i < count; i++) {
                asprintf(&lttng_sessions[i].name, "%s", tok);
                tok = strtok(NULL, ":");
        }
        free(ptr);
        /* path to snapshot */
        asprintf(&lttng_sessions[0].path, "%s", snapshot_path);

        /* Mark end of array */
        lttng_sessions[count].name = NULL;
}

/* ===================================================================== */
/**
 *   This sets the first ocuurence of zero in session_id_vector to one and
 *   returns the next index (This is to ensure session id starts with 1).
 *
 *   @param               -
 *
 *   @return              - A integer value
 *
 *   @par Globals: session_id_vector,total_session_count
 *
 */
/* ===================================================================== */
static int get_session_id()
{
        int i = 0;

        for (i = 0; i < maxNumDiagStreamSession; i++) {
                if (*(session_id_vector + i) == 0) {
                        *(session_id_vector + i) = 1;
                        total_session_count ++;
                        return (i + 1);
                }
        }
        return (-1);
}

/* ===================================================================== */
/**
 *   This is a support functionality to extract a provider and event name
 *   from item_name. The string might include wild card for the provider
 *   or for the event (com_ericsson_plf*, com_ericsson_plf_ntp:ntp*).
 *   Ex: com_ericsson_plf_ntp:ntpDebug (com_ericsson_plf_ntp is the provier
 *   name and ntpDebug is the event name).
 *
 *   @param               item_name    : lttng event whose provider needs
 *                                       to be extracted.
 *                        provider     : character array where extracted
 *                                       provider name is copied into.
 *                        provider_len : size of the provider array.
 *                        event        : character array where extracted
 *                                       event name is copied into.
 *                        event_len    : size of the event array.
 *
 *   @return              0 on successful execution
 *
 *   @par Globals:        -
 *
 */
/* ===================================================================== */
static int extract_provider_event(char *item_name,
                                  char *provider,
                                  int provider_len,
                                  char *event,
                                  int event_len)
{
        char *index_ptr;
        char *provider_ptr = NULL;
        char *event_ptr = NULL;
        int ret;
        int result = 0;

        if ((provider_len > LTTNG_SYMBOL_NAME_LEN) ||
            (event_len > LTTNG_SYMBOL_NAME_LEN)) {
                 result = -1;
                 goto respond;
        }

        provider_ptr = strdup(item_name);
        event_ptr = index_ptr = strchr(provider_ptr, ':');
        if (event_ptr == NULL) {
                /* Only provider provided */
                strcpy(event, "*");
        }
        else {
                /* Provide and event are provided */
                event_ptr++;
                ret = snprintf(event, event_len, "%s", event_ptr);
                if ((ret < 0) || ((size_t) ret >= event_len))
                {
                         result = -1;
                         goto respond;
                }
                index_ptr[0] = '\0';
        }

        ret = snprintf(provider, provider_len, "%s", provider_ptr);
        if ((ret < 0) || ((size_t) ret >= provider_len))
        {
                 result = -1;
        }

 respond:

        if (provider_ptr) {
                 free(provider_ptr);
        }

        return result;
}


/* ===================================================================== */
/**
 *  Adds session details to local linked list
 *
 *   @param               name pointer to session name
 *   @param               url pointer to handle ip address
 *   @param               subbuf size
 *   @param               no of subbufers
 *   @param               buffer switch interval time
 *   @param               data flush interval time
 *   @param               saved bool value to specify if saved event.
 *
 *   @return              0 on successful execution
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static uint16_t
addTsSessionDetails(char *name, char *url,
                    uint64_t subbuf_size, uint64_t no_of_subbuf,
                    unsigned int switch_interval, unsigned int flush_interval,
                    bool saved)
{
        struct session_list_element *elem;
        int ret = 0;
        int session_id;
        time_t now;
        struct tm ts;
        char buf[80];

        /* Get current time of the day*/
        time(&now);

        /* Format time, "ddd yyyy-mm-dd hh:mm:ss zzz" */
        ts = *localtime(&now);
        strftime(buf, sizeof(buf), "%a %Y-%m-%d %H:%M:%S %Z", &ts);

        elem = (struct session_list_element*)malloc(sizeof(struct session_list_element));
        asprintf(&elem->te_session.name, "%s", name);
        asprintf(&elem->te_session.path, "%s", url);
        elem->te_session.subbuf_size = subbuf_size;
        elem->te_session.no_of_subbuf = no_of_subbuf;
        elem->te_session.switch_interval = switch_interval;
        elem->te_session.flush_interval = flush_interval;
        snprintf(elem->time_of_creation,
                 sizeof(elem->time_of_creation), "%s", buf);
        elem->saved = saved;
        session_id = get_session_id();
        elem->session_id = session_id;
        TAILQ_INIT(&elem->saved_events);
        TAILQ_INSERT_HEAD(&sessionList, elem, list);
        ret = session_id;
        return ret;
}

/* ===================================================================== */
/**
 *   This function resets the value in the session_id_vector to the
 *   corresponding value passed.
 *
 *   @param               j : index value in array to zero the value.
 *
 *   @return              -
 *
 *   @par Globals: session_id_vector,total_session_count
 *
 */
/* ===================================================================== */
static void
reset_session_id(int j)
{
        if (j <= maxNumDiagStreamSession) {
                *(session_id_vector + j - 1) = 0;
                total_session_count --;
        } else {
                ERR("Unidentified session id %d was passed.Maximum session id %d",
                     j, maxNumDiagStreamSession);
        }
}

/* ===================================================================== */
/**
 *   Removes the trace stream details from local linked list,
 *   and also tracing will also be stopped if it is registered.
 *
 *   @param               name pointer to session name
 *
 *   @return              0 on successful removing.
 *                        non zero on failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int removeTsSessionDetails(char *name)
{
        struct session_list_element *sessionItem, *elem;
        int length, ret = 0, atLeastOneSession = 0;

        length = getlength(name);

        /*
        ** Walk through the list of sessions and try to locate an already
        ** existing session with the same name as the one about to be removed.
        */
        for(sessionItem = TAILQ_FIRST(&sessionList); sessionItem != NULL ;
            sessionItem = elem) {

                elem = TAILQ_NEXT(sessionItem, list);
                if  ((strncmp(name, sessionItem->te_session.name, length) == 0)) {
                        ret = lttng_destroy_session(sessionItem->te_session.name);
                        if (ret < 0) {
                                ERR("Failed to destroy session. reason:%s",
                                     lttng_strerror(ret));
                                return ret;
                        }
                        reset_session_id(sessionItem->session_id);
                        atLeastOneSession ++;
                        free(sessionItem->te_session.name);
                        free(sessionItem->te_session.path);
                        TAILQ_REMOVE(&sessionList, sessionItem, list);
                        free(sessionItem);
                }
        }
        if (atLeastOneSession == 0) {
                ret = -LTTNG_ERR_NO_SESSION;
        }
        return ret;
}
/* ===================================================================== */
/**
 *  Updates the save flag in the session list.
 *
 *   @param              name:  Name of the session name
 *                       saved: Boolean value that will be saved
 *
 *   @return             -
 *
 *   @par Globals:
 *               saved_session_counter
 */
/* ===================================================================== */
static void update_saved_sessions_data(char *name, bool saved)
{
        struct session_list_element *sessionItem, *elem;
        int length;

        length = getlength(name);
        /*
        ** Walk through the list of sessions and try to locate an already
        ** existing session with the same name as the one about to be removed.
        */
        for (sessionItem = TAILQ_LAST(&sessionList, session_list_head);
                               sessionItem != NULL; sessionItem = elem) {
                elem = TAILQ_PREV(sessionItem, session_list_head, list);

                if  ((strncmp(name, sessionItem->te_session.name, length) == 0) &&
                     (sessionItem->saved != saved)) {
                        sessionItem->saved = saved;
                        if (saved) {
                                saved_session_counter ++;
                        } else {
                                saved_session_counter --;
                        }
                }
        }

}

/* ===================================================================== */
/**
 *  Checks with local linked list if was actually created through ts
 *   command.
 *
 *   @param              session_name : session name of quiried session
 *                       session_detials: Sessions details of requested
 *                                         will be copied into it.
 *
 *   @return             1 if successful
 *                       0 if failed
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int isActiveTsSession(char *session_name,
                             struct session_details *session_details)
{
        struct session_list_element *saveItem = NULL;
        struct session_list_element *tmp_elem1;

         /* Walk through the session list and check if the requested
          * session name exists.
          */
        for (saveItem = TAILQ_LAST(&sessionList, session_list_head);
                                saveItem != NULL; saveItem = tmp_elem1) {
                tmp_elem1 = TAILQ_PREV(saveItem, session_list_head, list);
                if (!strcmp(session_name, saveItem->te_session.name)) {
                        /* If exists copy the requested session details
                        */
                        strncpy(session_details->time_of_creation,
                                saveItem->time_of_creation,
                                sizeof(session_details->time_of_creation));
                        session_details->session_id = saveItem->session_id;
                        session_details->saved = saveItem->saved;
                        return 1;
                }
        }
        return 0;
}

/* ===================================================================== */
/**
 *  Checks with local linked list if was actually created through ts
 *   command.
 *
 *   @param              session_name : session name of quiried session
 *
 *   @return             1 if successful
 *                       0 if failed
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int is_registered_ts_session(char *session_name)
{
        struct session_list_element *saveItem = NULL;
        struct session_list_element *tmp_elem1;

         /* Walk through the session list and check if the requested
          * session name exists.
          */
        for (saveItem = TAILQ_LAST(&sessionList, session_list_head);
                                saveItem != NULL; saveItem = tmp_elem1) {
                tmp_elem1 = TAILQ_PREV(saveItem, session_list_head, list);
                if (!strcmp(session_name, saveItem->te_session.name)) {
                        return 1;
                }
        }
        return 0;
}

/* ===================================================================== */
/**
 *   This function removes all the files in a directory
 *
 *   @param               path: Directory where files are
 *                              required to be removed
 *
 *   @return              -
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static void
removeFilesInDirectory(char *path)
{
        struct dirent *next_file;
        DIR *dirPath;
        char filepath[256];
        int ret = 0;

        dirPath = opendir(path);

        if (dirPath) {
                while ((next_file = readdir(dirPath)) != NULL)
                {
                    if (strncmp(next_file->d_name, "ts_stream", strlen("ts_stream")) != 0) {
                            continue;
                    }
                    snprintf(filepath, sizeof(filepath), "%s/%s",
                             path, next_file->d_name);
                    ret = remove(filepath);
                    if (ret < 0) {
                            ERR("Failed to remove file %s from %s directory, reason(%d)",
                                                    next_file->d_name, path, errno);
                    }
                }
        }
}

/* ===================================================================== */
/**
 *   This function returns a session name to corresponding session Id.
 *
 *   @param               session_id :session ID of the requested session
 *                        session_name : pointer where session name is
 *                                      returned
 *                        size : size of parameter "session_name" which
 *                               was passed
 *
 *   @return              Zero on success, non zero at failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int
getSessionNameFromId(uint16_t session_id, char *session_name, int size)
{
        struct session_list_element *elem;
        int ret = -1;
        TAILQ_FOREACH(elem, &sessionList, list) {
                if (elem->session_id == session_id) {
                        strncpy(session_name, elem->te_session.name,
                                size);
                        session_name[size - 1] = '\0';
                        ret = 0;
                        break;
                }
        }

        return ret;
}

/* ===================================================================== */
/**
 *   This function returns preset list head and the url of the requested
 *   session.
 *
 *   @param               session_name: session name of the requested
 *                                      session.
 *                        list_head : pointer to hold the session
 *                                      preset list
 *                        url :pointer to hold session ip.
 *                        (Note: memory pointed by url should be freed
 *                          when it is no longer required)
 *   @return              Zero on success, non zero at failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int
getSessionPresetList(char *session_name,
                     struct presetListHead **list_head, char **url,
                     uint64_t *subbuf_size, uint64_t *no_of_subbuf,
                     unsigned int *switch_interval,
                     unsigned int *flush_interval)
{
        struct session_list_element *elem;
        int ret = -1;
        TAILQ_FOREACH(elem, &sessionList, list) {
                if (!(strncmp(elem->te_session.name,session_name,
                    LTTNG_SYMBOL_NAME_LEN))){
                        *(list_head) = &elem->saved_events;
                        asprintf(&(*url), "%s", elem->te_session.path);
                        *subbuf_size = elem->te_session.subbuf_size;
                        *no_of_subbuf = elem->te_session.no_of_subbuf;
                        *switch_interval = elem->te_session.switch_interval;
                        *flush_interval = elem->te_session.flush_interval;
                        ret = 0;
                        break;
                }
        }
        return ret;

}

/* ===================================================================== */
/**
 *   This function returns overflow condition for a requested session
 *   preset list
 *
 *   @param               list_head : Which holds the preset head of
 *                                      a session list.
 *                        presetCount : preset count events of the session
 *                        savedPresetCount : saved preset count of the
 *                                           session.
 *
 *   @return              false if save events count less than
 *                        MAX_SAVED_PRESET_COUNT other wise true.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static bool
checkPresetOverFlow(struct presetListHead *list_head,
               int *presetCount,
               int *savedPresetCount)
{
        struct presetListElement *elem;
        bool saveOverFlow = true;
        *presetCount = 0;
        *savedPresetCount =0;

        TAILQ_FOREACH(elem, list_head, list)
        {
                (*presetCount)++;
                if (elem->saved == true) {
                        (*savedPresetCount)++;
                }
        }
        if (*savedPresetCount < MAX_SAVED_PRESET_COUNT)
                saveOverFlow = false;

        return saveOverFlow;
}

/* ===================================================================== */
/**
 *  This function gets the enabled events for a session.
 *
 *   @param              session : Requested session name
 *
 *   @return             session_info structure which contains data of the
 *                       enabled events for that session if successful,
 *                       NULL if failed
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static struct session_info * get_session_details(char *session)
{
        int count, size, tmpsize, n;
        struct session_info *session_info = NULL;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_event *event_list = NULL;
        struct lttng_channel *channels = NULL;
        int grand_no_of_events = 0;


        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        handle = lttng_create_handle(session, &domain);
        if (handle == NULL) {

                ERR("lttng_create_handle failed for ses:%s",
                             session);
                        goto err;
        }

        /* Get all channels for the session.
         */
         count = lttng_list_channels(handle, &channels);
         if (count < 0) {
                ERR("lttng_list_channels failed,reason:%s(%d)",
                                  lttng_strerror(count), count);
                goto err;
        } else if (count == 0) {
                INFO("No channels found on the session %s", session);
                goto err;
        }

       /* channel walk for one session */
       for (n = 0; n < count; n++) {
                /* get events for channel */
                size = lttng_list_events(handle, channels[n].name,
                                        &event_list);
                if (size < 0) {
                      ERR("lttng_list_events, %s",
                           lttng_strerror(size));
                     goto err;
                }
                grand_no_of_events += size;
        }

        /* Here allocate size for a session which is
         * summation of no.of channels x no. of corresponding events
         */
         session_info = (struct session_info *)
                        malloc(sizeof(struct session_info) +
                               grand_no_of_events *
                               sizeof(struct lttng_event) +
                               count * sizeof(struct Channel_info));

        tmpsize = sizeof(session_info->session_name);
        strncpy(session_info->session_name, session, tmpsize);
        session_info->session_name[tmpsize - 1] = '\0';

        session_info->no_of_channels = count;

        /* Itereate through each channel and copy all the events
         * list individually
         */
        for (n = 0; n < count; n++) {
                /* get events for channel */
                size = lttng_list_events(handle, channels[n].name,
                                         &event_list);
                if (size < 0) {
                       ERR("lttng_list_events, %s",
                            lttng_strerror(size));
                      goto err;
                 }

                tmpsize = sizeof(session_info->channel_info[n].name);
                strncpy(session_info->channel_info[n].name,
                        channels[n].name, tmpsize);

                session_info->channel_info[n].name[tmpsize - 1] = '\0';

                session_info->channel_info[n].no_of_events = size;

                /* Copy events for a channel */
                memcpy(session_info->channel_info[n].event_list,
                       event_list,
                       size * sizeof(struct lttng_event));

                if (event_list) {
                        free(event_list);
                        event_list = NULL;
                }
        }
        if (channels) {
                free(channels);
                channels = NULL;
        }

        lttng_destroy_handle(handle);
        handle = NULL;

        return session_info;

 err:
        if (channels)
                free(channels);
        if (event_list)
                free(event_list);
        if (session_info)
                free(session_info);
        if (handle)
                lttng_destroy_handle(handle);

        return NULL;
}

static int
updateEventsOnSingleProvider(struct lttng_handle *handle,
                             char *provider,
                             char *eventGroup,
                             char *channelName,
                             bool enable) {

        int                ret = 0;
        char               *ev_name;
        struct lttng_event lttngFormatEvent;

        /*
         * Not valid to enable/disable events on provider
         * starting with 'com_ericsson_tri'.
         */
        if ((strstr(provider, TRI_PROVIDER_NAME1) != NULL) ||
            (strstr(provider, TRI_PROVIDER_NAME2) != NULL)) {
                return 0;
        }
        memset(&lttngFormatEvent, 0, sizeof(lttngFormatEvent));

        /*
         * Enable/Disable all the events on the given provider.
         * Multipled events are separated by a space" ".
         */
        ev_name = strtok(eventGroup, " ");
        while (ev_name != NULL) {
               snprintf(lttngFormatEvent.name,
                        sizeof(lttngFormatEvent.name),
                        "%s:%s",
                        provider,
                        ev_name);
               if (enable) {
                       lttngFormatEvent.type = LTTNG_EVENT_TRACEPOINT;
                       ret = lttng_enable_event(handle, &lttngFormatEvent, channelName);
                       ret = filterLttngResponse(ret);
                       if (ret < 0) {
                              ERR("Failed to enable event, %s", lttngFormatEvent.name);
                              goto err;
                       }
                } else {
                       ret = lttng_disable_event(handle, lttngFormatEvent.name, channelName);
                       if (ret < 0) {
                             ERR("Failed to disable event, %s", lttngFormatEvent.name);
                             goto err;
                       }
                }
                ev_name = strtok(NULL, " ");
         }

 err:
         return ret;
}

static int
updateEventsOnMultiProviders(struct lttng_handle *handle,
                             char *channelName,
                             char *cmdProvider,
                             char *cmdEvents,
                             bool enable)
{

        int                ret = 0;
        int                size = 0;
        int                index = 0;
        int                len;
        int                provLen;
        int                count = 0;
        char               provider[LTTNG_SYMBOL_NAME_LEN];
        char               eventStr[LTTNG_SYMBOL_NAME_LEN];
        char               *eventIndexPtr = NULL;
        char               *ev_name = NULL;
        char               *eventList = NULL;


        struct lttng_event *registeredEvents = NULL;

        /*
         * Get the available tracepoints of UST lttng domain
         * lttng_list_tracepoints() gives the registered events in
         * the format, tracepointprovider:event1 and so on...
         */
        size = lttng_list_tracepoints(handle, &registeredEvents);
        if (size < 0) {
                ret = size;
                ERR("lttng_list_tracepoints, %s", lttng_strerror(ret));
                goto err;
        }

        /*
         * Enable/Disable on multiple tracepoints of UST lttng domain
         * as per the given provider and events.
         * ex: com_* : Enable/Disable events on matching providers
         *             starting with 'com_' and configured with given event.
         */
        len = strlen(cmdProvider);
        for (index = 0; index < size; index++) {
                /*
                 * Find the matching provider from the available tracepoints of
                 * UST lttng domain. Check for matching provider excluding
                 * wild card.
                 */
                if (!strncmp(registeredEvents[index].name, cmdProvider, len - 1)) {
                       eventIndexPtr = strchr(registeredEvents[index].name, ':');
                       if (eventIndexPtr == NULL)
                               continue;

                       provLen = eventIndexPtr - registeredEvents[index].name;
                       if (provLen >= LTTNG_SYMBOL_NAME_LEN) {
                               provLen = LTTNG_SYMBOL_NAME_LEN - 1;
                       }
                       strncpy(provider, registeredEvents[index].name, provLen);
                       provider[provLen] = '\0';
                       eventIndexPtr ++;
                       strcpy(eventStr, eventIndexPtr);

                       /*
                        * Parse on events group to find the tracepoints with specific
                        * event.For all matching tracepoints with given event, lttng
                        * enable/disable request is sent.
                        */
                       eventList = (char *)malloc(strlen(cmdEvents) + 1);
                       strcpy(eventList, cmdEvents);

                       /* enable event(s) in the first channel */
                       ev_name = strtok(eventList, " ");
                       while (ev_name != NULL) {
                               if (!strcmp(eventStr, ev_name)) {
                                       /*
                                        * Enable/Disable the event on single provider.
                                        */
                                       ret = updateEventsOnSingleProvider(handle,
                                                                          provider,
                                                                          eventStr,
                                                                          channelName,
                                                                          enable);
                                       count ++;
                                       if (ret < 0) {
                                               ERR("Failed to update events %s for provider %s",
                                                       eventStr, provider);
                                               goto err;
                                       }
                                       break;
                               }
                               ev_name = strtok(NULL, " ");
                       }
                       free(eventList);
                       eventList = NULL;
                }
        }
        ret = count;

 err:
        if (registeredEvents)
                free(registeredEvents);

        if (eventList)
                free(eventList);

        return ret;
}

static int enable_events(union itc_msg *rmsg)
{
        int count, ret = 0, len;
        union itc_msg *msg;
        char *eventGroup = NULL;
        char session_name[LTTNG_SYMBOL_NAME_LEN];
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER) {
                if(rmsg->te_ctrl_req.session_name[0] == '\0') {
                      ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                                 session_name,
                                                 sizeof(session_name));
                      if (ret < 0) {
                            ret = -LTTNG_ERR_NO_SESSION;
                            goto send_rsp;
                      }
                }
                else {
                       if (!is_registered_ts_session(rmsg->te_ctrl_req.session_name)) {
                             ret = 1;
                             goto send_rsp;
                       }
                       snprintf(session_name, LTTNG_SYMBOL_NAME_LEN, "%s",
                                rmsg->te_ctrl_req.session_name);
                }
        } else {
                snprintf(session_name, LTTNG_SYMBOL_NAME_LEN, "%s", lttng_sessions[0].name);
        }

        handle = lttng_create_handle(session_name, &domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", session_name);
                goto send_rsp;
        }

        /* get all channels */
        count = lttng_list_channels(handle, &channels);
        if (count < 0) {
                ret = count;
                ERR("lttng_list_channels failed,reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto send_rsp;
        }

        /* enable events on first channel */
        if (count > 0 && &channels[0] != NULL) {
                /* Check if com_* or * is given to decided whether the request is
                 * for single or multi tracepoint providers.
                 */
                if ((!strncmp((char *)rmsg->te_ctrl_req.provider,
                              LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) &&
                     strchr((char *)rmsg->te_ctrl_req.provider, '*')) ||
                    (!strcmp((char *)rmsg->te_ctrl_req.provider, "*"))) {
                        /*
                         * Wild card is given, enable the events for all
                         * matching providers.
                         */
                        ret = updateEventsOnMultiProviders(handle,
                                                           channels[0].name,
                                                           (char *)rmsg->te_ctrl_req.provider,
                                                           (char *)rmsg->te_ctrl_req.data,
                                                           true);
                        if (ret == 0) {
                                /* No events were enabled */
                                ret = 2;
                        } else {
                                /* Masking the return value as success since more than one
                                 * trace event was enabled
                                 */
                                ret = 0;
                        }
                } else {
                        eventGroup = (char *)malloc(strlen(rmsg->te_ctrl_req.data) + 1);
                        strcpy(eventGroup, rmsg->te_ctrl_req.data);
                        /*
                         * Enable the all given events on single provider.
                         */
                        ret = updateEventsOnSingleProvider(handle,
                                                           rmsg->te_ctrl_req.provider,
                                                           eventGroup,
                                                           channels[0].name,
                                                           true);
                        if (ret < 0) {
                                ERR("Failed to enable events %s for provider %s",
                                    eventGroup, rmsg->te_ctrl_req.provider);
                                goto send_rsp;
                        }
                        free(eventGroup);
                        eventGroup = NULL;
                }
        } else {
                ret = -1;
                ERR("No channels for session %s", lttng_sessions[0].name);
        }

 send_rsp:
        /* free up resources */
        if (channels)
                free(channels);
        if (handle)
                lttng_destroy_handle(handle);
        if (eventGroup)
                free(eventGroup);

        /* Send response */
        if (ret < 0) {
                len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len, TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
                snprintf(msg->te_ctrl_rsp.data, len, "%s", lttng_strerror(ret));
        } else if (ret == 1) {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
        } else if (ret == 2){
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_NOT_FOUND;
        } else {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        }
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}

static void
savePresetList (char *sessionPath,
                struct presetListHead *list_head,
                char *url,
                uint64_t subbuf_size,
                uint64_t no_of_subbuf,
                unsigned int switch_interval,
                unsigned int flush_interval)
{
        char *p = NULL, *ptr;
        FILE *fp = NULL;
        int num = 0;
        struct presetListElement *elem;

        /* check that full path exists */
        p = strdup(sessionPath);
        ptr = strchr(p, '/');
        while(ptr != NULL) {
                *ptr = 0;
                /* skip first '/' */
                if (strlen(p) != 0) {
                        if (access(p, F_OK) == -1 &&
                            mkdir(p, 0755)) {
                                ERR("mkdir(%s) failed, %m", p);
                                goto finish;
                        }
                }
                *ptr = '/';
                ptr = strchr(ptr + 1, '/');
        }
        unlink(sessionPath);

        /* create file */
        if ((fp = fopen(sessionPath, "w+")) == NULL) {
                ERR("Failed to open(or create) %s", sessionPath);
                goto finish;
        }
        if (url != NULL) {
                fprintf(fp,"%s %llu %llu %u %u\n", url,
                        (unsigned long long) subbuf_size,
                        (unsigned long long) no_of_subbuf,
                        switch_interval, flush_interval);
        }

         /* Now add the requested item to the new temporary save file */
        TAILQ_FOREACH(elem, list_head, list)
        {
                if (elem->saved == true)
                {
                        fprintf(fp,"%s %d\n",elem->itemInfo, elem->enable);
                        if (num < 0)
                        {
                                 ERR("Failed to write %s (wrote only %dB)",
                                     elem->itemInfo, num);
                        }
                }
        }
finish:

if (fp)
    fclose(fp);

if (p)
    free(p);

}

static int
removePreset(char *name, bool saveFlag,struct presetListHead *list_head)
{
   struct presetListElement *saveItem, *elem;
   int ret = -1;
   char provider[LTTNG_SYMBOL_NAME_LEN];
   char event[LTTNG_SYMBOL_NAME_LEN];
   char saved_provider[LTTNG_SYMBOL_NAME_LEN];
   char saved_event[LTTNG_SYMBOL_NAME_LEN];

   if (extract_provider_event(name,
                              provider, sizeof(provider),
                              event, sizeof(event))) {
         ERR("Failed to extract provider and/or event "
             "name from input %s", name);
         return ret;
   }
   /*
   ** Walk through the list of presets and try to locate an already
   ** existing preset with the same name as the one about to be removed.
   */
   for(saveItem = TAILQ_FIRST(list_head); saveItem != NULL;
                                                 saveItem = elem) {

       elem = TAILQ_NEXT(saveItem, list);
       if (extract_provider_event(saveItem->itemInfo,
                                  saved_provider, sizeof(saved_provider),
                                  saved_event, sizeof(saved_event))) {
         ERR("Failed to extract provider and/or event"
             " name from list element %s", name);
         continue;
       }

       /* All events for this provider shall be removed. */
       if (strcmp(event, "*") == 0) {
         strcpy(saved_event, "*");
       }
       /* All events for this provider shall be removed. */
       if (strcmp(provider, "*") == 0) {
         strcpy(saved_provider, "*");
       }

       if ((saveItem->saved == saveFlag) &&
           (strcmp(saved_provider, provider) == 0) &&
           (strcmp(saved_event, event) == 0)) {
         free(saveItem->itemInfo);
         TAILQ_REMOVE(list_head, saveItem, list);
         free(saveItem);
         ret = 0;
       }
   }
   return ret;
}

static bool
addPreset(char      *name,
          int       enable,
          bool      saved,
          struct presetListHead *list_head)

{
        int length = 0;
        struct presetListElement *elem, *saveItem;
        length = getlength(name);
        bool elemFound = false;
        bool saveOverFlow;
        int savedPresetCount;
        int presetCount;
        /*
         * Walk through the list to check if the requsted group trace
         * is already in the list
         */
        TAILQ_FOREACH(saveItem, list_head, list) {
        if (!strncmp(saveItem->itemInfo,
                  name, length)) {
             elemFound = true;
             break;
           }
        }
        saveOverFlow = checkPresetOverFlow(list_head, &presetCount,
                                           &savedPresetCount);

        if(elemFound) {
             if((TAILQ_PREV(saveItem, presetListHead, list) != NULL)) {

                /*
                 ** This is not the first element, but it is the present some where
                 ** in preset list. Move it to the first position in the list
                 */
                elem = (struct presetListElement*)malloc(sizeof(struct presetListElement));
                elem->saved = saved;
                asprintf(&elem->itemInfo, "%s", name);
                elem->enable = enable;
                TAILQ_INSERT_HEAD(list_head, elem,list);
                free(saveItem->itemInfo);
                TAILQ_REMOVE(list_head, saveItem, list);
                free(saveItem);
                saveItem = TAILQ_FIRST(list_head);
             }
             else {
             /* This is already first element */
                 saveItem->enable = enable;
                 saveItem->saved = saved;
             }
             /*
              * Check if this preset is change to or from a saved one and increase
              * or decrease the saved presets counter accordingly if needed...
              */
             if (!saved && saveItem->saved) {
               /*
                * Previously this Preset group was saved and now it was requested to change to
                * a non saved preset item.This use case usally appears when te config is given
                * on a process and then te preset was again applied on the same process.
                */
                  saved = true;
             }
             else if (saved && ! saveItem->saved) {
                /*
                 * Previousy this was a non saved Preset item. Now it was requested to change to
                 * a saved preset item. This case happens usually when te preset is applied on
                 * a process and then te config/save is applied on the same process. But we dont
                 * change if we already maximum limit for saving presets.
                 */
                if (savedPresetCount < MAX_SAVED_PRESET_COUNT) {
                        savedPresetCount ++;
                }
                else {
                   saved = 0;
                   saveOverFlow = true;
                }
              }

             /*
              * Update the save flag of that preset
              */
             saveItem->saved = saved;
        }
        else if(!elemFound && (presetCount < MAX_PRESET_COUNT)) {
              /*
              ** Allocate and initiate a new preset entry in the preset list
              */
              saveItem = (struct presetListElement*)malloc(sizeof(struct presetListElement));
              asprintf(&saveItem->itemInfo, "%s", name);
              saveItem->enable = enable;
              saveItem ->saved = false; /* Initialising with Default value */
              /*
               * Check if this preset is a saved one and increase
               * the saved presets counter if needed...
               */
               if (saved) {
                 /* This preset was requested to be saved. Check if we reached limit
                  */
                 if (savedPresetCount < MAX_SAVED_PRESET_COUNT) {
                    saveItem ->saved = true;
                    savedPresetCount ++;
                 }
                 else {
                    /* Since saved Preset count limit was already reached we will not save the
                     * preset but just adds the preset
                     */
                    saveItem ->saved = false;
                    saveOverFlow = true;
                 }
              }
              /* Increment total preset count as we are requested for a new preset.
               */
              presetCount ++;
              /*
              ** Extend the preset list with this new entry
              */
              if (TAILQ_EMPTY(list_head)) {
                 /*
                 ** This is the first entry to be put in the preset list
                 */
                 TAILQ_INSERT_TAIL(list_head, saveItem, list);
              }
              else {
                 /*
                 ** There already exist another preset first in the list.
                 ** Put this new one before that first preset...
                 */
                 TAILQ_INSERT_HEAD(list_head, saveItem, list);
              }
        }

        return saveOverFlow;

}

static int disable_events(union itc_msg *rmsg)
{
        int count, ret = 0, len;
        union itc_msg *msg;
        char          *eventGroup = NULL;
        char session_name[LTTNG_SYMBOL_NAME_LEN];
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;


        if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER) {
                if(rmsg->te_ctrl_req.session_name[0] == '\0') {
                      ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                                 session_name,
                                                 sizeof(session_name));
                      if (ret < 0) {
                             ret = -LTTNG_ERR_NO_SESSION;
                             goto send_rsp;
                      }
                }
                else {
                       if (!is_registered_ts_session(rmsg->te_ctrl_req.session_name)) {
                               ret = 1;
                               goto send_rsp;
                       }
                       snprintf(session_name, LTTNG_SYMBOL_NAME_LEN, "%s",
                                          rmsg->te_ctrl_req.session_name);
                }
        } else {
                snprintf(session_name, LTTNG_SYMBOL_NAME_LEN, "%s",
                                           lttng_sessions[0].name);
        }

        handle = lttng_create_handle(session_name, &domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", session_name);
                goto send_rsp;
        }

        /* get all channels */
        count = lttng_list_channels(handle, &channels);
        if (count < 0) {
                ret = count;
                ERR("lttng_list_channels failed,reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto send_rsp;
        }
        /* disable events on first channel */
        if (count > 0 && &channels[0] != NULL) {
                /* Check if com_* or * is given to decided whether the request is
                 * for single or multi tracepoint providers.
                 */
                if ((!strncmp((char *)rmsg->te_ctrl_req.provider,
                              LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) &&
                     strchr((char *)rmsg->te_ctrl_req.provider, '*')) ||
                    (!strcmp((char *)rmsg->te_ctrl_req.provider, "*"))) {
                        /*
                         * Wild card is given, disable the events for all
                         * matching providers.
                         */
                        ret = updateEventsOnMultiProviders(handle,
                                                           channels[0].name,
                                                           (char *)rmsg->te_ctrl_req.provider,
                                                           (char *)rmsg->te_ctrl_req.data,
                                                           false);
                        if (ret == 0) {
                              /* No events were found */
                                ret = 2;
                        } else {
                              /* Masking the return value as success since more than one
                               * trace event was disabled
                               */
                              ret = 0;
                        }
                } else {
                        eventGroup = (char *)malloc(strlen(rmsg->te_ctrl_req.data) + 1);
                        strcpy(eventGroup, rmsg->te_ctrl_req.data);
                        /*
                         * Disable all events on single provider.
                         */
                        ret = updateEventsOnSingleProvider(handle,
                                                           rmsg->te_ctrl_req.provider,
                                                           eventGroup,
                                                           channels[0].name,
                                                           false);
                        if (ret < 0) {
                                ERR("Failed to disable events %s for provider %s",
                                           eventGroup, rmsg->te_ctrl_req.provider);
                                goto send_rsp;
                        }
                        free(eventGroup);
                        eventGroup = NULL;
                }
        } else {
                ret = -1;
                ERR("No channels for session %s", lttng_sessions[0].name);
        }

 send_rsp:
        /* free up resources */
        if (channels)
                free(channels);
        if (handle)
                lttng_destroy_handle(handle);
        if (eventGroup)
                free(eventGroup);

        /* Send response */
        if (ret < 0) {
                len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len, TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
                snprintf(msg->te_ctrl_rsp.data, len, "%s", lttng_strerror(ret));
        } else if (ret == 1) {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
        } else if (ret == 2){
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_NOT_FOUND;
        } else {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        }
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}


static int mychmod(const char *path, const struct stat *sb,
                   int flag, struct FTW *ftwbuf) {

        int rc;
        struct stat st;

        if (ftwbuf->level == 0)
                return 0;

        if (stat(path, &st) < 0)
                return -1;

        if ((rc = chmod(path, st.st_mode | S_IROTH | S_IXOTH)) < 0) {
                INFO("Permissions change failed: %s, err: %s\n",
                     path, strerror(errno));
        }
        return rc;
}

static int myrm(const char *path, const struct stat *sb,
                int flag, struct FTW *ftwbuf) {

        int (*rm_func)(const char *);
        int rc;

        if (ftwbuf->level == 0)
                return 0;

        switch(flag) {
        case FTW_DP:
                rm_func = rmdir;
                break;
        default:
                rm_func = unlink;
        }
        if ((rc = rm_func(path)) != 0) {
                INFO("Not removed: %s\n", path);
        }
        return rc;
}

static void healthcheck(void)
{
   int ret = 0;
   char *pfs, *cmd = NULL;

   /* Get output path for health check log. */
   if ((pfs = getenv("PFS_PATH")) == NULL) {
      ERR("Failed to get environment variable PFS_PATH\n");
      /* Fallback to using default path. */
      asprintf(&cmd, "/sbin/lttng_healthchk -s");
   } else {
      asprintf(&cmd, "/sbin/lttng_healthchk -s -o %s", pfs);
   }

   /* Execute lttng-healthchk with single-shot check. */
   ret = run_shell_cmd(cmd);
   if (ret == CMD_ERROR) {
      ERR("Failed to execute %s\n", cmd);
   }

   free(cmd);
}


static int log_read(union itc_msg *rmsg)
{
        struct lttng_snapshot_output *lttng_out;
        union itc_msg *msg;
        int sz, ret = 0;
        uint16_t extra = 0;

        healthcheck();

        if (ted_ss_reboot == SS_RB_CONF || ted_ss_time_adjusted == SS_TIME_ADJUSTED) {
                /* Only read snapshot from last reboot or traces restarted once */
                if (ted_ss_reboot == SS_RB_CONF)
                        ted_ss_reboot = SS_RB_DONE;
                if (ted_ss_time_adjusted == SS_TIME_ADJUSTED)
                        ted_ss_time_adjusted = SS_TIME_RESTARTED_TRACES_READ;
                extra = TED_LOG_FCORRELATE;
        } else {
                /* Remove all previous snapshots from output folder
                 * This way filesystem will not be flooded by snapshots
                 * taken every time user reads the log. OBS, All dirs under
                 * path will be deleted.
                 */
                if (nftw(lttng_sessions[0].path, myrm, FOPEN_MAX, FTW_DEPTH) < 0) {
                        ERR("Failed to clean path, %s", lttng_sessions[0].path);
                        goto send_rsp;
                }
        }

        /*
         * Take a snapshot of lttng buffers  for a given session
         */
        /* Create a output handle */
        if ((lttng_out = lttng_snapshot_output_create()) == NULL) {
                ERR("lttng_snapshot_output_create failed");
                ret = -1;
                goto send_rsp;
        }

        /* Take a snapshot, output path used is same as configured for the session */
        ret = lttng_snapshot_record(lttng_sessions[0].name, lttng_out, 0);
        if (ret < 0) {
                ERR("lttng_snapshot_record failed, ses:%s, %s",
                    lttng_sessions[0].name, lttng_strerror(ret));
                ret = -1;
                goto send_rsp;
        }


        lttng_snapshot_output_destroy(lttng_out);

        /*
         * snapshot folder created by root lttng-sessiond and all its
         * subfolders have permissions set to 770, meaning non-privileged
         * user cant read/write from/to that folder. A regular user should
         * be able to read traces generated by root sessiond.
         * Changing permissions to this file tree.
         */
        nftw(lttng_sessions[0].path, mychmod, FOPEN_MAX, FTW_DEPTH);

 send_rsp:
        /* Send response */
        if (ret < 0) {
                sz = 0;
        } else {
                sz = strlen(lttng_sessions[0].path) + 1;
        }
        msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + sz, TED_CTRL_RSP);
        if (ret < 0) {
                msg->te_ctrl_rsp.result = 0;
        } else {
                msg->te_ctrl_rsp.result = 1;
                memcpy(msg->te_ctrl_rsp.data, lttng_sessions[0].path, sz);
        }
        msg->te_ctrl_rsp.type = TE_LOG_READ;
        msg->te_ctrl_rsp.extra = extra;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);
        return ret;
}

/* ===================================================================== */
/**
 *  Retry lttng_enable_channel, if needed.
 *
 *   @param handle   LTTng session handle
 *   @param channel  LTTng session channel
 *   @param retries  Number of times to retry, zero or greater.
 *
 *   @return         0 on successful execution, or LTTng error.
 *
 *   @par Globals:   --
 *
 *   @note lttng_enable_channel sometimes fails to begin with, but if
 *         one tries again it will succeed after some time.
 *         The root cause is not known, so this is a work-around.
 */
/* ===================================================================== */
static int retry_lttng_enable_channel(struct lttng_handle *handle,
                                      struct lttng_channel *channel,
                                      int retries)
{
   int i, loops, ret = 0;

   assert(retries >= 0);

   /* Run the loop at least once if the caller request no retries. */
   loops = (retries == 0) ? 1 : retries;

   for (i = 1; i <= loops; i++) {
      ret = lttng_enable_channel(handle, channel);
      if (ret == 0) {
         break;
      }
      else {
         INFO("Trying to enable channel %s, attempt %d of %d: %s (%d).",
              channel->name, i, loops, lttng_strerror(ret), ret);
         if (i == loops) {
            return ret;
         }
         else {
            /* Progressive delay. */
            usleep(50000 * i);
            continue;
         }
      }
   }

   return ret;
}


static int log_clear(union itc_msg *rmsg)
{
        union itc_msg *msg;
        int ret = 0, no_ses;
        int i, n, m, no_events;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_event *event_list = NULL;
        struct lttng_channel *channels = NULL;
        struct lttng_session *sessions = NULL;
        char *path = SNAPSHOT_PATH;

        healthcheck();

        /* Save enabled events for snapshot session */
        /* destroy and create sessions, and enable saved events */


        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        if ((ret = lttng_list_sessions(&sessions)) < 0) {
                ERR("lttng_list_sessions failed");
                goto send_rsp;
        }
        no_ses = ret;

        /* Session walk */
        for (i = 0; lttng_sessions[i].name != NULL; i++) {

                for (n = 0; n < no_ses; n++) {
                        if (strcmp(lttng_sessions[i].name, sessions[n].name) == 0)
                                break;
                        if (n + 1 == no_ses) {
                                ERR("Session %s does not exist", lttng_sessions[i].name);
                                ret = -1;
                                goto send_rsp;
                        }
                }
                handle = lttng_create_handle(lttng_sessions[i].name, &domain);
                if (handle == NULL) {
                        ret = -1;
                        ERR("lttng_create_handle failed for ses:%s", sessions[i].name);
                        goto send_rsp;
                }

                /* get all channels */
                ret = lttng_list_channels(handle, &channels);
                if (ret < 0) {
                        ERR("lttng_list_channels failed,reason:%s(%d)",
                            lttng_strerror(ret), ret);
                        goto send_rsp;
                }

                /* get events for first channel only */
                ret = lttng_list_events(handle, channels[0].name, &event_list);
                if (ret < 0) {
                        ERR("lttng_list_events, %s", lttng_strerror(ret));
                        goto send_rsp;
                }
                no_events = ret;

                /* destroy session */
                ret = lttng_destroy_session(lttng_sessions[i].name);
                if (ret < 0) {
                        ERR("lttng_destroy_session, %s", lttng_strerror(ret));
                        goto send_rsp;
                }
                lttng_destroy_handle(handle);

                /* delete all snapshots/lttng files */
                if (lttng_sessions[i].path != NULL) {
                        path = lttng_sessions[i].path;
                } else {
                        if (!sessions[n].snapshot_mode)
                                path = sessions[n].path;
                }
                if (nftw(path, myrm, FOPEN_MAX, FTW_DEPTH) < 0) {
                        ERR("Failed to clean path, %s", path);
                }

                /* Re-create session with enabled events */
                if (sessions[n].snapshot_mode) {
                        ret = lttng_create_session_snapshot(lttng_sessions[i].name,
                                                            lttng_sessions[i].path);
                } else {
                        ret = lttng_create_session(lttng_sessions[i].name, sessions[n].path);
                }
                if (ret < 0) {
                        ERR("Failed to re-create session(%s), %s",
                            lttng_sessions[i].name, lttng_strerror(ret));
                        goto send_rsp;
                }

                handle = lttng_create_handle(lttng_sessions[i].name, &domain);
                if (handle == NULL) {
                        ret = -1;
                        ERR("Second lttng_create_handle failed for ses:%s", sessions[i].name);
                        goto send_rsp;
                }

                ret = retry_lttng_enable_channel(handle, &channels[0], 5);
                if (ret < 0) {
                        ERR("lttng_enable_channel, %s", lttng_strerror(ret));
                        goto send_rsp;
                }

                /* enable events */
                for (m = 0; m < no_events; m++) {
                        if (event_list[m].enabled)
                                ret = lttng_enable_event(handle,
                                                         &event_list[m],
                                                         channels[0].name);
                }

                /* activate session */
                ret = lttng_start_tracing(lttng_sessions[i].name);
                if (ret < 0) {
                        ERR("lttng_start_tracing, %s", lttng_strerror(ret));
                        goto send_rsp;
                }

                /* Free up resources */
                free(channels);
                channels = NULL;
                free(event_list);
                event_list = NULL;
                lttng_destroy_handle(handle);
                handle = NULL;
        }

 send_rsp:
        if (channels)
                free(channels);
        if (event_list)
                free(event_list);
        if (sessions)
                free(sessions);
        if (handle)
                lttng_destroy_handle(handle);

        msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
        if (ret < 0) {
                msg->te_ctrl_rsp.result = 0;
        } else {
                msg->te_ctrl_rsp.result = 1;
        }
        msg->te_ctrl_rsp.type = TE_LOG_CLEAR;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);
        return ret;
}

/* ===================================================================== */
/**
 *  Handle a te restart request, takes a snapshot, then recreates
 *  the trace session.
 *
 *   @param               rmsg, the itc message with the restart request
 *
 *   @return              0 on successful execution
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static void handle_te_restart(union itc_msg *rmsg)
{
        union itc_msg *msg;
        int ret = 0, no_ses, len;
        int i, n, m, no_events;
        struct lttng_snapshot_output *lttng_out = NULL;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_event *event_list = NULL;
        struct lttng_channel *channels = NULL;
        struct lttng_session *sessions = NULL;

        healthcheck();

        /* Save enabled events for snapshot session */
        /* destroy and create sessions, and enable saved events */

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        if ((ret = lttng_list_sessions(&sessions)) < 0) {
                ERR("lttng_list_sessions failed");
                goto send_rsp;
        }
        no_ses = ret;

        /*
         * If the time has not been adjusted once aldready,
         * do not clean reboot traces.
         */
        if (ted_ss_time_adjusted == SS_TIME_AFTER_BOOT) {
                ted_ss_time_adjusted = SS_TIME_ADJUSTED;
        } else {
                /* Remove all previous snapshots from output folder
                 * This way filesystem will not be flooded by snapshots
                 * taken every time the clock is adjusted. OBS, All dirs under
                 * path will be deleted.
                 */
                if (nftw(lttng_sessions[0].path, myrm, FOPEN_MAX, FTW_DEPTH) < 0) {
                        ERR("Failed to clean path, %s", lttng_sessions[0].path);
                        goto send_rsp;
                }
                /*
                 * Force the te log read command to not delete all old snapshots
                 * before reading.
                 */
                ted_ss_reboot = SS_RB_CONF;
        }

        /* Session walk */
        for (i = 0; lttng_sessions[i].name != NULL; i++) {

                for (n = 0; n < no_ses; n++) {
                        if (strcmp(lttng_sessions[i].name, sessions[n].name) == 0)
                                break;
                        if (n + 1 == no_ses) {
                                ERR("Session %s does not exist", lttng_sessions[i].name);
                                ret = -1;
                                goto send_rsp;
                        }
                }

                /*
                 * Take a snapshot in order to preserve old data from before te restart
                 */

                /* Create an output handle */
                if ((lttng_out = lttng_snapshot_output_create()) == NULL) {
                        ERR("lttng_snapshot_output_create failed");
                        ret = -1;
                        goto send_rsp;
                }

                /* Take a snapshot, output path used is same as configured for the session */
                ret = lttng_snapshot_record(lttng_sessions[i].name, lttng_out, 0);
                if (ret < 0) {
                        ERR("lttng_snapshot_record failed, ses:%s, %s",
                            lttng_sessions[0].name, lttng_strerror(ret));
                        ret = -1;
                        goto send_rsp;
                }

                lttng_snapshot_output_destroy(lttng_out);
                lttng_out = NULL;

                handle = lttng_create_handle(lttng_sessions[i].name, &domain);
                if (handle == NULL) {
                        ret = -1;
                        ERR("lttng_create_handle failed for ses:%s", sessions[i].name);
                        goto send_rsp;
                }

                /* get all channels */
                ret = lttng_list_channels(handle, &channels);
                if (ret < 0) {
                        ERR("lttng_list_channels failed, reason:%s(%d)",
                            lttng_strerror(ret), ret);
                        goto send_rsp;
                }

                /* get events for first channel only */
                ret = lttng_list_events(handle, channels[0].name, &event_list);
                if (ret < 0) {
                        ERR("lttng_list_events, %s", lttng_strerror(ret));
                        goto send_rsp;
                }
                no_events = ret;

                /* destroy session */
                ret = lttng_destroy_session(lttng_sessions[i].name);
                if (ret < 0) {
                        ERR("lttng_destroy_session, %s", lttng_strerror(ret));
                        goto send_rsp;
                }

                lttng_destroy_handle(handle);
                handle = NULL;

                /* Re-create session with enabled events */
                if (sessions[n].snapshot_mode) {
                        ret = lttng_create_session_snapshot(lttng_sessions[i].name,
                                                            lttng_sessions[i].path);
                } else {
                        ret = lttng_create_session(lttng_sessions[i].name, sessions[n].path);
                }
                if (ret < 0) {
                        ERR("Failed to re-create session(%s), %s",
                            lttng_sessions[i].name, lttng_strerror(ret));
                        goto send_rsp;
                }

                handle = lttng_create_handle(lttng_sessions[i].name, &domain);
                if (handle == NULL) {
                        ret = -1;
                        ERR("Second lttng_create_handle failed for ses:%s", sessions[i].name);
                        goto send_rsp;
                }

                ret = retry_lttng_enable_channel(handle, &channels[0], 5);
                if (ret < 0) {
                        ERR("lttng_enable_channel, %s", lttng_strerror(ret));
                        goto send_rsp;
                }

                /* enable events */
                for (m = 0; m < no_events; m++) {
                        if (event_list[m].enabled)
                                ret = lttng_enable_event(handle,
                                                         &event_list[m],
                                                         channels[0].name);
                }

                /* activate session */
                ret = lttng_start_tracing(lttng_sessions[i].name);
                if (ret < 0) {
                        ERR("lttng_start_tracing, %s", lttng_strerror(ret));
                        goto send_rsp;
                }

                /* Free up resources */
                free(channels);
                channels = NULL;
                free(event_list);
                event_list = NULL;
                lttng_destroy_handle(handle);
                handle = NULL;
        }

 send_rsp:
        if (lttng_out)
                lttng_snapshot_output_destroy(lttng_out);

        if (channels)
                free(channels);
        if (event_list)
                free(event_list);
        if (sessions)
                free(sessions);
        if (handle)
                lttng_destroy_handle(handle);

        if (ret < 0) {
                len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len,
                                TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
                snprintf(msg->te_ctrl_rsp.data, len, "%s",
                         lttng_strerror(ret));
        } else {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        }
        msg->te_ctrl_rsp.type = TE_RESTART;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        tracepoint(com_ericsson_plf_trace_util, teRestart, ++te_restart_count);

        return;
}


static int skip_event(char *ev)
{
        if (strcmp(ev, "*") == 0)
                return 1;

        if ((strstr(ev, TRI_PROVIDER_NAME1) != NULL) ||
            (strstr(ev, TRI_PROVIDER_NAME2) != NULL)) {
                return 1;
        }
        return 0;
}

/* ===================================================================== */
/**
 *  This function verifies if the event passed is one of the default
 *  event of the session
 *
 *   @param              event : Name of the event
 *                       cmd_type: To determine type of the session
 *
 *   @return             true  :if event is a default event
 *                       false :if event is not a default event
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static bool
is_one_of_default_event(char *event,
                        int cmd_type)
{
        int  cnt, cnt1,len;
        struct session_info *default_session_details;

        len = getlength(event);
        if (cmd_type == CMD_TE_HANDLER) {
                default_session_details = te_session_info_at_restart;
        } else {
                default_session_details = ts_session_info_at_restart;
        }
        if (default_session_details == NULL) {
                return true;
        }

        for (cnt = 0;
             cnt < default_session_details->no_of_channels;
             cnt ++) {

                if (!strcmp(default_session_details->channel_info[cnt].name,
                           "metadata"))
                        continue;
                for (cnt1 = 0;
                     cnt1 < default_session_details->channel_info[cnt].no_of_events;
                     cnt1 ++) {
                        if (!strncmp(default_session_details->channel_info[cnt].
                                     event_list[cnt1].name,
                                     event, len)) {
                                return true;
                        }
                }
        }
        return false;
}

/* ===================================================================== */
/**
 *  This function enables the default events on the session requested
 *
 *   @param              session_name: Name of the session
 *                       channel_name: Name of the channel
 *                       handle      : pointer which holds the channel
 *                       cmd_type    : To determine type of the session
 *
 *   @return             Zero     : if succesful
 *                       non-zero : if unsuccesful
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
set_default_events(char *session_name,
                   char *channel_name,
                   struct lttng_handle *handle,
                   int cmd_type)
{
        int  ret = 0, cnt, cnt1;
        struct lttng_event lttngFormatEvent;
        struct session_info *default_session_details;

        if (cmd_type == CMD_TE_HANDLER) {
                default_session_details = te_session_info_at_restart;
        } else {
                default_session_details = ts_session_info_at_restart;
        }
        if (default_session_details == NULL) {
                goto finished;
        }

        for (cnt = 0; cnt < default_session_details->no_of_channels; cnt ++) {

                if(!strcmp(default_session_details->channel_info[cnt].name,
                          "metadata")) {
                        continue;
                }

                for (cnt1 = 0;
                     cnt1 < default_session_details->channel_info[cnt].no_of_events;
                     cnt1 ++) {
                        memset(&lttngFormatEvent, 0, sizeof(lttngFormatEvent));
                        snprintf(lttngFormatEvent.name,
                                 sizeof(lttngFormatEvent.name),
                                 "%s",
                                 default_session_details->channel_info[cnt].event_list[cnt1].name);
                        lttngFormatEvent.type =
                          default_session_details->channel_info[cnt].event_list[cnt1].type;
                        lttngFormatEvent.loglevel =
                          default_session_details->channel_info[cnt].event_list[cnt1].loglevel;
                        lttngFormatEvent.loglevel_type =
                          default_session_details->channel_info[cnt].event_list[cnt1].loglevel_type;
                        ret = lttng_enable_event(handle,
                                                 &lttngFormatEvent,
                                                 default_session_details->channel_info[cnt].name);
                        ret = filterLttngResponse(ret);
                        if (ret < 0) {
                             ERR("Failed to enable event, %s (%s)",
                                 lttngFormatEvent.name, lttng_strerror(ret));
                             goto finished;
                        }
                }
        }
finished:
        return ret;
}

static int set_default(union itc_msg *rmsg)
{
        int count, size, i, ret = 0, len;
        int at_least_one_event_found = 0;
        union itc_msg *msg;
        char session_name[LTTNG_SYMBOL_NAME_LEN];
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;
        struct lttng_event *event_list = NULL;

        char provider[LTTNG_SYMBOL_NAME_LEN];
        char *indexPtr;
        int provLen = 0;

        len = getlength(rmsg->te_ctrl_req.provider);
        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER) {
                if(rmsg->te_ctrl_req.session_name[0] == '\0') {
                        ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                                   session_name,
                                                   sizeof(session_name));
                        if (ret < 0) {
                                ret = -LTTNG_ERR_NO_SESSION;
                                goto send_rsp;
                        }
                } else {
                        if (rmsg->te_ctrl_req.scope == TED_RESTART) {
                                /* Removing all the saved event files.Since -restart
                                 * was given we will only remove saved files but wont
                                 * destroy sessions.
                                 */
                                removeFilesInDirectory(ted_live_stream_ev_file);
                                update_saved_sessions_data("*", false);
                                saved_session_counter = 0;
                                at_least_one_event_found = 1;
                                goto send_rsp;
                        }
                        if (!is_registered_ts_session(rmsg->te_ctrl_req.session_name)) {
                                ret = 1;
                                goto send_rsp;
                        }
                        snprintf(session_name, sizeof(session_name), "%s",
                                 rmsg->te_ctrl_req.session_name);
                }
        } else {
                if(rmsg->te_ctrl_req.scope == TED_RESTART) {
                         (void)removePreset(rmsg->te_ctrl_req.provider, true, &teSessionPresetList);
                         savePresetList(ted_saved_ev_file, &teSessionPresetList, NULL, 0, 0, 0, 0);
                         at_least_one_event_found = 1;
                         ret = CMD_RESULT_OK;
                         goto send_rsp;
                }
                else if(rmsg->te_ctrl_req.scope == TED_PRESET) {
                         (void)removePreset(rmsg->te_ctrl_req.provider, false, &teSessionPresetList);
                         at_least_one_event_found = 1;
                         ret = CMD_RESULT_OK;
                         goto send_rsp;
                }
                snprintf(session_name, sizeof(session_name),
                         "%s", lttng_sessions[0].name);
        }

        handle = lttng_create_handle(session_name, &domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", session_name);
                goto send_rsp;
        }

        /* get all channels */
        count = lttng_list_channels(handle, &channels);
        if (count < 0) {
                ret = count;
                ERR("lttng_list_channels failed,reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto send_rsp;
        }
        if (count > 0 && &channels[0] != NULL) {

                size = lttng_list_events(handle, channels[0].name, &event_list);
                if (size < 0) {
                        ret = size;
                        ERR("lttng_list_events, %s", lttng_strerror(ret));
                        goto send_rsp;
                }
                for (i = 0; i < size; i++) {
                        if (skip_event(event_list[i].name))
                                continue;
                        indexPtr = strchr(event_list[i].name, ':');
                        if (indexPtr == NULL) {
                                provLen = LTTNG_SYMBOL_NAME_LEN - 1;
                                /*continue;
                                */
                        } else {
                                provLen = indexPtr - event_list[i].name;
                        }
                        if (provLen >= LTTNG_SYMBOL_NAME_LEN) {
                                 provLen = LTTNG_SYMBOL_NAME_LEN - 1;
                        }
                        strncpy(provider, event_list[i].name, provLen);
                        provider[provLen] = '\0';
                        if ((event_list[i].enabled) &&
                            (!strncmp(rmsg->te_ctrl_req.provider, provider, len))) {
                                ret = lttng_disable_event(handle,
                                                          event_list[i].name,
                                                          channels[0].name);
                                if (ret < 0) {
                                        ERR("Failed to disable event, %s (%s)",
                                            event_list[i].name,
                                            lttng_strerror(ret));
                                        goto send_rsp;
                                }
                                at_least_one_event_found = 1;
                        }

                }
                /* Now all the tracing is back to default. Enable the events
                 * that are from the ted start up script.
                 */
                 if (is_one_of_default_event(rmsg->te_ctrl_req.provider,
                                              rmsg->te_ctrl_req.handler)) {
                        ret = set_default_events(session_name,
                                                 channels[0].name,
                                                 handle,
                                                 rmsg->te_ctrl_req.handler);
                        at_least_one_event_found = 1;
                }

        } else {
                ret = -1;
                ERR("No channels for session %s", lttng_sessions[0].name);
        }

send_rsp:
        /* free up resources */
        if (event_list)
                free(event_list);
        if (channels)
                free(channels);
        if (handle)
                lttng_destroy_handle(handle);

        /* Send response */
        if (ret < 0) {
                len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len, TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
                snprintf(msg->te_ctrl_rsp.data, len, "%s", lttng_strerror(ret));
        } else if (ret == 1) {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
        } else {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                if (at_least_one_event_found) {
                        msg->te_ctrl_rsp.result = CMD_RESULT_OK;
                } else {
                        msg->te_ctrl_rsp.result = CMD_EVENT_NOT_FOUND;
                }
        }
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}


static int enable_filter(struct lttng_event *event, char *filter,
                         struct lttng_domain *domain, char *session_name)
{
        int ret = 0, size, i;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;
        struct lttng_event *event_list = NULL;

        handle = lttng_create_handle(session_name, domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", session_name);
                goto out;
        }
        /* get all channels */
        ret = lttng_list_channels(handle, &channels);
        if (ret < 0) {
                ERR("lttng_list_channels failed, reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto out;
        }

        /* check if event is already enabled,
         * if yes disable it to avoid duplicated enebled events
         */
        size = lttng_list_events(handle, channels[0].name, &event_list);
        if (size < 0) {
                ret = size;
                ERR("lttng_list_events, %s", lttng_strerror(ret));
                goto out;
        }
        for (i = 0; i < size; i++) {
                if (!strcmp(event_list[i].name, event->name) && event_list[i].enabled) {
                        if (!event_list[i].filter) {
                                ret = lttng_disable_event(handle, event->name,
                                                          channels[0].name);
                                if (ret < 0) {
                                        ERR("lttng_disable_event, %s", lttng_strerror(ret));
                                        goto out;
                                }
                        } else {
                                ERR("Filter already set, %s", event->name);
                                ret = -1;
                                goto out;
                        }
                }
        }
        ret = lttng_enable_event_with_filter(handle, event, channels[0].name, filter);
        if (ret < 0) {
                switch (-ret) {
                case LTTNG_ERR_FILTER_EXIST:
                        ERR("Filter on all events is already enabled"
                             " (channel %s, session %s)",
                             channels[0].name, session_name);
                        break;
                default:
                        ERR("All events: %s (channel %s, session %s, filter \'%s\')",
                            lttng_strerror(ret),
                            channels[0].name, session_name, filter);
                        break;
                }
        }
 out:
        if (event_list)
                free(event_list);
        if (channels)
                free(channels);
        if (handle)
                lttng_destroy_handle(handle);
        return ret;
}

static int set_filter(union itc_msg *rmsg)
{
        int ret = 0;
        union itc_msg *msg;

        struct lttng_domain domain;
        struct lttng_event event;
        char *filter = NULL;
        char session_name[LTTNG_SYMBOL_NAME_LEN];

        memset(&domain, 0, sizeof(domain));
        memset(&event, 0, sizeof(event));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER) {
                if(rmsg->te_ctrl_req.session_name[0] == '\0') {
                        ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                                   session_name,
                                                   sizeof(session_name));
                        if (ret < 0) {
                              ret = -LTTNG_ERR_NO_SESSION;
                              goto send_rsp;
                        }
                }
                else {
                        if (!is_registered_ts_session(rmsg->te_ctrl_req.session_name)) {
                               ret = 1;
                               goto send_rsp;
                        }
                        snprintf(session_name, sizeof(session_name),
                                 "%s", rmsg->te_ctrl_req.session_name);
                }
        } else {
                snprintf(session_name, sizeof(session_name),
                         "%s", lttng_sessions[0].name);
        }
        strncpy(event.name, &rmsg->te_ctrl_req.data[rmsg->te_ctrl_req.offset],
                LTTNG_SYMBOL_NAME_LEN);
        event.name[LTTNG_SYMBOL_NAME_LEN - 1] = 0;
        event.type = LTTNG_EVENT_TRACEPOINT;
        asprintf(&filter, "%s", rmsg->te_ctrl_req.data);
        ret = enable_filter(&event, filter, &domain, session_name);

        free(filter);

send_rsp:
        /* Send response */
        msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
        if (ret < 0)
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
        else if (ret == 1)
                msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
        else
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}

#if 0
/* saves events and destroys session*/
static int destroy_ses_with_events(char *ses_name,
                                   struct lttng_event **event_list,
                                   int *no_events,
                                   struct lttng_domain *domain)
{
        int ret = 0;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;

        handle = lttng_create_handle(ses_name, domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", ses_name);
                goto out;
        }

        /* get all channels */
        ret = lttng_list_channels(handle, &channels);
        if (ret < 0) {
                ERR("lttng_list_channels failed, reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto out;
        }

        /* get events for first channel only */
        ret = lttng_list_events(handle, channels[0].name, event_list);
        if (ret < 0) {
                ERR("lttng_list_events, %s", lttng_strerror(ret));
                goto out;
        }
        *no_events = ret;

        /* destroy session */
        ret = lttng_destroy_session(ses_name);
        if (ret < 0) {
                ERR("lttng_destroy_session, %s", lttng_strerror(ret));
                goto out;
        }
 out:
        if (channels)
                free(channels);
        if (handle)
                lttng_destroy_handle(handle);

        return ret;
}

static int create_ses_with_events(struct te_sessions *ses,
                                  struct lttng_event *event_list,
                                  int no_events,
                                  struct lttng_domain *domain)
{
        int ret, m;
        struct lttng_handle *handle = NULL;

        ret = lttng_create_session_snapshot(ses->name, ses->path);
        if (ret < 0) {
                ERR("Failed to re-create session(%s), %s",
                    ses->name, lttng_strerror(ret));
                goto out;
        }

        handle = lttng_create_handle(ses->name, domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", ses->name);
                goto out;
        }

        /* enable events */
        for (m = 0; m < no_events; m++) {
                if (event_list[m].enabled)
                        ret = lttng_enable_event(handle, &event_list[m],
                                                 NULL);
        }

        /* activate session */
        ret = lttng_start_tracing(ses->name);
        if (ret < 0)
                ERR("lttng_start_tracing, %s", lttng_strerror(ret));
 out:
        if (handle)
                lttng_destroy_handle(handle);

        return ret;
}
#endif

static int reset_filter(union itc_msg *rmsg)
{
        int ret, count;
        char *ev_name;
        union itc_msg *msg;
        struct lttng_domain domain;
        struct lttng_event event;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;
        char session_name[LTTNG_SYMBOL_NAME_LEN];

        memset(&event, 0, sizeof(event));
        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER) {
                if(rmsg->te_ctrl_req.session_name[0] == '\0') {
                        ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                                   session_name,
                                                   sizeof(session_name));
                        if (ret < 0) {
                              ret = -LTTNG_ERR_NO_SESSION;
                             goto send_rsp;
                        }
                } else {
                        if (!is_registered_ts_session(rmsg->te_ctrl_req.session_name)) {
                                ret = 1;
                                goto send_rsp;
                        }
                        snprintf(session_name, sizeof(session_name),
                                "%s", rmsg->te_ctrl_req.session_name);
                }
        } else {
                  snprintf(session_name, sizeof(session_name), "%s",
                           lttng_sessions[0].name);
        }

        handle = lttng_create_handle(session_name, &domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s",session_name );
                goto send_rsp;
        }

        /* get all channels */
        count = lttng_list_channels(handle, &channels);
        if (count < 0) {
                ret = count;
                ERR("lttng_list_channels failed, reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto send_rsp;
        }
        if (count > 0 && &channels[0] != NULL) {

                /* disable event in the first channel */
                ev_name = rmsg->te_ctrl_req.data;
                ret = lttng_disable_event(handle, ev_name, channels[0].name);
                if (ret < 0) {
                        ERR("Failed to disable event, %s (%s)", ev_name,
                            lttng_strerror(ret));
                        goto send_rsp;
                }

                /* enable same event, will be enabled without any filter */
                strncpy(event.name, ev_name, LTTNG_SYMBOL_NAME_LEN);
                event.name[LTTNG_SYMBOL_NAME_LEN - 1] = 0;
                event.type = LTTNG_EVENT_TRACEPOINT;
                ret = lttng_enable_event(handle, &event, channels[0].name);
                if (ret < 0) {
                        ERR("Failed to enable event, %s", event.name);
                        goto send_rsp;
                }
        } else {
                ret = -1;
                ERR("No channels for session %s", lttng_sessions[0].name);
        }

#if 0
        /* save events and destroy session */
        ret = destroy_ses_with_events(lttng_sessions[0].name, &event_list,
                                      &no_events, &domain);
        if (ret < 0) {
                ERR("Failed to save events and destroy session");
                goto send_rsp;
        }

        /* Re-create session with saved events, this will clear all filters */
        ret = create_ses_with_events(&lttng_sessions[0], event_list,
                                     no_events, &domain);
        if (ret < 0)
                ERR("Failed to re-create session and enable saved events");
#endif

 send_rsp:
        if (handle)
                lttng_destroy_handle(handle);
        if (channels)
                free(channels);

        /* Send response */
        msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
        if (ret < 0)
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
        else if (ret == 1)
                msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
        else
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}

static void formatString(char *source, char *dest)
{
        if(dest[0] == '\0') {
             strcpy(dest,source);
        }
        else {
             strcat(dest," ");
             strcat(dest,source);
        }
}

static int  preset_events(union itc_msg *rmsg, bool saved)
{
        int    count, ret = 0;
        union  itc_msg *msg;
        char   *ev_name;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;
        char   *preset_event;
        char   *enable_save;
        char   *disable_save;
        char   *command_data;
        bool saveOverFlow = false;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        /*
         * Tokenising the give data into following format to add to preset list
         */
         enable_save = (char*)malloc(strlen(rmsg->te_ctrl_req.data) + 1);
         disable_save = (char*)malloc(strlen(rmsg->te_ctrl_req.data) + 1);
         command_data = (char *)malloc(strlen(rmsg->te_ctrl_req.data) + 1);
         enable_save[0] = '\0';
         disable_save[0] = '\0';
         strcpy(command_data, rmsg->te_ctrl_req.data);

        ev_name = strtok(command_data, " ");

        /* Iterate through the list and prepare two event lists.
         * One list containing events that are to be enabled and
         * other list containing events that are to be disabled.
         */
        while (ev_name != NULL) {
                if (ev_name[0] == '-') {
                     ev_name ++;
                     asprintf(&preset_event,"%s:%s",
                               rmsg->te_ctrl_req.provider, ev_name);
                     saveOverFlow = addPreset(preset_event, 0,
                                              saved, &teSessionPresetList);
                     formatString(ev_name, disable_save);
                }
                else {
                     if (ev_name[0] == '+')
                     ev_name ++;
                     asprintf(&preset_event,"%s:%s",
                              rmsg->te_ctrl_req.provider, ev_name);
                     saveOverFlow = addPreset(preset_event, 1,
                                              saved, &teSessionPresetList);
                     formatString(ev_name, enable_save);
                }
                free(preset_event);
                ev_name = strtok(NULL, " ");
        }
        handle = lttng_create_handle(lttng_sessions[0].name, &domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s",
                     lttng_sessions[0].name);
                goto send_rsp;
        }
        /* get all channels */
        count = lttng_list_channels(handle, &channels);
        if (count < 0) {
                ret = count;
                ERR("lttng_list_channels failed, reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto send_rsp;
        }
        /* get all channels */
        count = lttng_list_channels(handle, &channels);
        if (count < 0) {
                ret = count;
                ERR("lttng_list_channels failed, reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto send_rsp;
        }

        if (count > 0 && &channels[0] != NULL) {
                 /*
                  * Enable all the corresponding events
                  */
                 if(enable_save[0] != '\0') {
                           if ((!strncmp((char *)rmsg->te_ctrl_req.provider,
                                         LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) &&
                                strchr((char *)rmsg->te_ctrl_req.provider, '*')) ||
                               (!strcmp((char *)rmsg->te_ctrl_req.provider, "*"))) {
                                  /*
                                   * Wild card is given, enable the events for all
                                   * matching providers.
                                   */
                                  ret = updateEventsOnMultiProviders(handle,
                                                                     channels[0].name,
                                                                     (char *)rmsg->te_ctrl_req.provider,
                                                                     (char *)enable_save,
                                                                     true);
                           } else {
                                  ret = updateEventsOnSingleProvider(handle,
                                                                     rmsg->te_ctrl_req.provider,
                                                                     enable_save,
                                                                     channels[0].name,
                                                                     true);
                                  if (ret < 0) {
                                          ERR("Failed to enable events %s for provider %s",
                                              enable_save, rmsg->te_ctrl_req.provider);
                                          goto send_rsp;
                                  }
                          }
                }
                if(disable_save[0] != '\0') {
                           if ((!strncmp((char *)rmsg->te_ctrl_req.provider,
                                         LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) &&
                                strchr((char *)rmsg->te_ctrl_req.provider, '*')) ||
                               (!strcmp((char *)rmsg->te_ctrl_req.provider, "*"))) {
                                  /*
                                   * Wild card is given, enable the events for all
                                   * matching providers.
                                   */
                                  ret = updateEventsOnMultiProviders(handle,
                                                                     channels[0].name,
                                                                     (char *)rmsg->te_ctrl_req.provider,
                                                                     (char *)disable_save,
                                                                     false);
                           } else {
                                  ret = updateEventsOnSingleProvider(handle,
                                                                     rmsg->te_ctrl_req.provider,
                                                                     disable_save,
                                                                     channels[0].name,
                                                                     false);
                                  if (ret < 0) {
                                          ERR("Failed to enable events %s for provider %s",
                                              disable_save, rmsg->te_ctrl_req.provider);
                                          goto send_rsp;
                                  }
                          }
                }
        } else {
                ret = -1;
                ERR("No channels for session %s", lttng_sessions[0].name);
        }

send_rsp:
        if (saved) {
            /*
               * Save the list in the file if the requested trace group
               * needs to be saved
               */
            savePresetList(ted_saved_ev_file, &teSessionPresetList, NULL, 0, 0, 0, 0);
        }

        if (enable_save != NULL)
               free(enable_save);

        if (disable_save != NULL)
               free(disable_save);

        if(command_data  != NULL)
               free(command_data);

        if (channels)
                free(channels);

        if (handle)
                lttng_destroy_handle(handle);

        /* Build the signal */
        msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);

        if (ret < 0) {
             msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
        }
        else {
            if (saveOverFlow) {
                msg->te_ctrl_rsp.result = CMD_PRESET_OVERFLOW;
            }
            else {
              msg->te_ctrl_rsp.result = CMD_RESULT_OK;
            }
        }
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}


static int save_events(union itc_msg *rmsg)
{
        int    ret = 0, i, j, len, req_ses_len;
        union  itc_msg *msg;
        char   *ev_name;
        int    no_events, ses_cnt = 0;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_event *event_list = NULL;
        struct lttng_session *sessions = NULL;
        struct lttng_channel *channels = NULL;
        struct presetListHead *list_head = NULL;
        struct session_details tmp_elem1;
        char provider[LTTNG_SYMBOL_NAME_LEN];
        char sessionName[LTTNG_SYMBOL_NAME_LEN];
        char req_session_name[LTTNG_SYMBOL_NAME_LEN];
        char *sessionPath = NULL;
        char *indexPtr = NULL;
        int atleast_one_found = 0;
        char *url = NULL;
        int provLen = 0;
        bool saveOverFlow = false;
        bool atleast_one_overflow = false;
        uint64_t subbuf_size, no_of_subbuf;
        unsigned int switch_interval, flush_interval;
        int result;

        len = getlength(rmsg->te_ctrl_req.provider);
        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        /* Opening all sessions
         */
        ses_cnt = lttng_list_sessions(&sessions);
        if (ses_cnt < 0) {
                ret = ses_cnt;
                goto send_rsp;
        } else if (ses_cnt == 0) {
                ret = 2;
                goto send_rsp;
        }


        if ((rmsg->te_ctrl_req.handler) == CMD_TE_HANDLER) {
                 /* Copy the s1 session (corresponding to te) details.
                  */
                 snprintf(sessionName, sizeof(sessionName), "%s",
                          lttng_sessions[0].name);
        } else {
                 if(rmsg->te_ctrl_req.session_name[0] == '\0') {
                         ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                                    sessionName,
                                                    sizeof(sessionName));
                         if (ret < 0) {
                               sessionName[0] = '\0';
                               ret = -LTTNG_ERR_NO_SESSION;
                               goto send_rsp;
                         }
                  } else {
                         snprintf(sessionName, sizeof(sessionName),
                                "%s", rmsg->te_ctrl_req.session_name);
                         if (!strcmp(sessionName, "*")) {
                                 /* We will resave every thing
                                 */
                                 update_saved_sessions_data("*", false);
                         }
                  }
        }
        req_ses_len = getlength(sessionName);

        for (j = ses_cnt - 1; j >= 0; j--) {

               /* Check it was a request from te/ts and if it is a request from ts
                * continue to look for ts sessions.
                */
               if ((!isActiveTsSession(sessions[j].name, &tmp_elem1))
                    && (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER)){
                              continue;
               }
               /* Skip if is not the session requested
                */
               if (!(strncmp(sessionName, sessions[j].name, req_ses_len) == 0)) {
                       continue;
               }
               snprintf(req_session_name, sizeof(req_session_name),
                                           "%s", sessions[j].name);

               /* Copying the session path according to the request as well as
                * preset list head of the session
                */
               if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER){

                        ret = getSessionPresetList(req_session_name,
                                                   &list_head, &url,
                                                   &subbuf_size, &no_of_subbuf,
                                                   &switch_interval,
                                                   &flush_interval);
                        if (ret < 0) {
                                ret = -LTTNG_ERR_NO_SESSION;
                                goto send_rsp;
                        }

                        asprintf(&sessionPath, "%s" "/"
                                       "ts_stream-" "%s" ".txt",
                                       ted_live_stream_ev_file, req_session_name);
               } else {
                        list_head = &teSessionPresetList;
                        asprintf(&sessionPath, "%s", ted_saved_ev_file);
               }

               handle = lttng_create_handle(req_session_name, &domain);
               if (handle == NULL) {
                     ret = -1;
                     ERR("lttng_create_handle failed for ses:%s", sessionName);
                     goto send_rsp;
               }

               /* get all channels */
               ret = lttng_list_channels(handle, &channels);
               if (ret < 0) {
                     ERR("lttng_list_channels failed,reason:%s(%d)",
                     lttng_strerror(ret), ret);
                     goto send_rsp;
               }

               /* get events for first channel only */
               ret = lttng_list_events(handle, channels[0].name, &event_list);
               if (ret < 0) {
                      ERR("lttng_list_events, %s", lttng_strerror(ret));
                      goto send_rsp;
               }
               no_events = ret;
               /* Masking return value as success
                */
               ret  = 0;

               /* List current session details and write enabled events to file */
               for (i = 0; i < no_events; i++) {
                          indexPtr = strchr(event_list[i].name, ':');
                          if (indexPtr == NULL) {
                                 if(!strncmp(rmsg->te_ctrl_req.provider, event_list[i].name, len)) {
                                         atleast_one_found = 1;
                                 }
                                 continue;
                          }
                          provLen = indexPtr - event_list[i].name;
                          if (provLen >= LTTNG_SYMBOL_NAME_LEN) {
                                  provLen = LTTNG_SYMBOL_NAME_LEN - 1;
                          }
                          strncpy(provider, event_list[i].name, provLen);
                          provider[provLen] = '\0';
                          ev_name = event_list[i].name;

                          if(!strncmp(rmsg->te_ctrl_req.provider, provider, len)) {
                                 if (!event_list[i].enabled) {
                                         result = removePreset(ev_name, true, list_head);
                                         if(result == -1) {
                                                  /* The event is not a saved event in the
                                                   * saved preset list. So, remove it it was
                                                   * a nonsaved event from preset list.
                                                   */
                                           (void)removePreset(ev_name, false,
                                                              list_head);
                                         }
                                         continue;
                                  }
                                   /* skip default enabled events */
                                  if (skip_event(ev_name))
                                       continue;

                                  /* Save all the trace groups which were enabled
                                   */
                                  saveOverFlow = addPreset(ev_name, 1, true, list_head);
                                  /* Even if one sessions got overflowed while
                                   * adding presets, we will show as preset overflow
                                   */
                                  if (saveOverFlow) {
                                            atleast_one_overflow = true;
                                  }
                                  atleast_one_found = 1;
                         }
              }

              if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER) {
                      /* Checking if the session was already saved.
                       * Then no need to update counter value or flag
                       */
                      if (!tmp_elem1.saved) {
                                /* Checking saved session count if limit already
                                 * reached we dont save session details
                                 */
                                if (saved_session_counter < maxNumDiagStreamSession) {
                                        update_saved_sessions_data(sessions[j].name, true);
                                } else {
                                        /* This is just to make sure that saved file
                                         * was not stored previously
                                         */
                                        unlink(sessionPath);
                                        ret = 1;
                                        goto send_rsp;
                                }
                      }
              }
              /* Send the file path and preset pointer list to save the events
               */
              savePresetList(sessionPath, list_head, url, subbuf_size,
                             no_of_subbuf, switch_interval, flush_interval);
              if (sessionPath) {
                      free(sessionPath);
                      sessionPath = NULL;
              }
              if (url) {
                      free(url);
                      url = NULL;
              }
       }

send_rsp:

        if (event_list)
                free(event_list);
        if (channels)
                free(channels);
        if (handle)
                lttng_destroy_handle(handle);
        if (sessionPath)
                free(sessionPath);
        if (url)
                free(url);

        /* Send response */

        if (ret < 0) {
            ERR("Failed to list ts session(s)");
            len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
            msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len, TED_CTRL_RSP);
            msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
            snprintf(msg->te_ctrl_rsp.data, len, "%s", lttng_strerror(ret));
        } else if (ret == 1) {
           msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
           msg->te_ctrl_rsp.result = CMD_SESSION_OVERFLOW;
           msg->te_ctrl_rsp.extra = maxNumDiagStreamSession;
        } else if (ret == 2) {
           /* No session available. */
           msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
           msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
           msg->te_ctrl_rsp.extra = 0;
        } else {
            msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
            if (atleast_one_overflow) {
                msg->te_ctrl_rsp.result = CMD_PRESET_OVERFLOW;
            } else {
                if (atleast_one_found) {
                        msg->te_ctrl_rsp.result = CMD_RESULT_OK;
                } else {
                        msg->te_ctrl_rsp.result = CMD_EVENT_NOT_FOUND;
                }
            }
        }

        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);
        return ret;
}

/* ===================================================================== */
/**
 *   Creates an active trace stream session to a requested ip/hostname
 *   with requested session name.
 *
 *   @param               session_name     session name of the live stream
 *                        formatted_url    ip/hostname of the target
 *
 *   @return              0 on successful trace stream creation.
 *                        non zero on failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int create_live_stream(char         *session_name,
                              char         *formatted_url,
                              uint64_t     subbuf_size,
                              uint64_t     no_of_subbuf,
                              unsigned int switch_interval,
                              unsigned int flush_interval,
                              char         *additional_info,
                              int          max_err_msg_len)
{
        int createSuccesful = 0;
        struct lttng_handle *handle = NULL;
        struct lttng_channel channel;
        struct lttng_domain domain;
        struct lttng_event event;
        struct lttng_event_context ctx;
        int ret = 0;

        memset(&event, 0, sizeof(event));
        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        /* Create live streaming session.
         * 100000 microsecs is the time between each flush to network
         */
        if (flush_interval != 0) {
           ret = lttng_create_session_live(session_name, formatted_url, flush_interval);
        } else {
           ret = lttng_create_session_live(session_name, formatted_url, 100000);
        }
        if (ret < 0) {
                snprintf(additional_info, max_err_msg_len,
                         "Failed to create s:%s (%s); %s",
                         session_name, formatted_url,
                         lttng_strerror(ret));
                goto sendrsp;
        }
        createSuccesful = 1;

        handle = lttng_create_handle(session_name, &domain);
        if (handle == NULL) {
                ret = -1;
                snprintf(additional_info, max_err_msg_len,
                         "Failed to create handle for s:%s", session_name);
                goto sendrsp;
        }

        /* Channel for periodical flush of trace data */
        memset(&channel, 0, sizeof(channel));
        lttng_channel_set_default_attr(&domain, &channel.attr);
        if (subbuf_size != 0) {
           channel.attr.subbuf_size = subbuf_size;
        } else {
           channel.attr.subbuf_size = LIVE_STREAM_SUB_BUF_SIZE;
        }
        if (no_of_subbuf != 0) {
           channel.attr.num_subbuf = no_of_subbuf;
        } else {
         channel.attr.num_subbuf = LIVE_STREAM_NUM_OF_SUB_BUFFERS;
        }
        if (switch_interval != 0) {
           channel.attr.switch_timer_interval = switch_interval;
        } else {
           channel.attr.switch_timer_interval = 2000000; /* 2 sec */
        }
        snprintf(channel.name, LTTNG_SYMBOL_NAME_LEN, "ch-1");
        ret = retry_lttng_enable_channel(handle, &channel, 6);
        if (ret < 0) {
                snprintf(additional_info, max_err_msg_len,
                         "Failed to enable channel for s:%s, ch:%s; %s",
                         session_name, channel.name, lttng_strerror(ret));
                goto sendrsp;
        }

        /* Add context, procname */
        memset(&ctx, 0, sizeof(ctx));
        ctx.ctx = LTTNG_EVENT_CONTEXT_PROCNAME;
        ret = lttng_add_context(handle, &ctx, NULL, channel.name);
        if (ret < 0) {
                snprintf(additional_info, max_err_msg_len,
                        "Failed to add context to s:%s, ch:%s; %s",
                        session_name, channel.name, lttng_strerror(ret));
                goto sendrsp;
        }

        /* Enable events */
        snprintf(event.name, LTTNG_SYMBOL_NAME_LEN, "com_ericsson_tri*");
        event.type = LTTNG_EVENT_TRACEPOINT;
        event.loglevel = LTTNG_LOGLEVEL_DEBUG;
        event.loglevel_type = LTTNG_EVENT_LOGLEVEL_SINGLE;
        ret = lttng_enable_event(handle, &event, channel.name);
        if (ret < 0) {
                snprintf(additional_info, max_err_msg_len,
                         "Failed to enable event %s for s:%s ch:%s; %s",
                         event.name, session_name,
                         channel.name, lttng_strerror(ret));
                goto sendrsp;
        }
        memset(&event, 0, sizeof(event));
        snprintf(event.name, LTTNG_SYMBOL_NAME_LEN,
                 "com_ericsson_plf_trace_util*");
        event.type = LTTNG_EVENT_TRACEPOINT;
        event.loglevel = LTTNG_LOGLEVEL_INFO;
        event.loglevel_type = LTTNG_EVENT_LOGLEVEL_SINGLE;
        ret = lttng_enable_event(handle, &event, channel.name);
        if (ret < 0) {
           snprintf(additional_info, max_err_msg_len,
                    "Failed to enable event %s for s:%s ch:%s; %s",
                    event.name, session_name,
                    channel.name, lttng_strerror(ret));
           goto sendrsp;
        }
        memset(&event, 0, sizeof(event));
        snprintf(event.name, LTTNG_SYMBOL_NAME_LEN, "*");
        event.type = LTTNG_EVENT_TRACEPOINT;
        event.loglevel = LTTNG_LOGLEVEL_INFO;
        event.loglevel_type = LTTNG_EVENT_LOGLEVEL_RANGE;
        ret = lttng_enable_event(handle, &event, channel.name);
        if (ret < 0) {
           snprintf(additional_info, max_err_msg_len,
                    "Failed to enable event %s for s:%s ch:%s; %s",
                    event.name, session_name,
                    channel.name, lttng_strerror(ret));
           goto sendrsp;
        }

        ret = lttng_start_tracing(session_name);
        if (ret < 0) {
                snprintf(additional_info, max_err_msg_len,
                         "Failed to start s:%s; %s",
                         session_name, lttng_strerror(ret));
                goto sendrsp;
        }

        if (ts_session_info_at_restart == NULL) {
                ts_session_info_at_restart = get_session_details(session_name);
        }


sendrsp :

        if (handle)
                lttng_destroy_handle(handle);

        if ((ret < 0) && createSuccesful) {
                lttng_destroy_session(session_name);
        }

        return ret;
}

/* ===================================================================== */
/**
 *   This function removes any active sessions which was created by
 *   "ts" commands and but later destroyed by lttng commands. Since ted
 *   will be unaware of such actions ,we will remove the data from ted.
 *
 *   @param               -
 *
 *   @return              -
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static void
remove_inactive_live_sessions(void)
{
        struct session_list_element *sessionItem, *elem;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;
        int count = 0;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;
        /*
        ** Walk through the list of sessions and try to locate already
        ** destroyed session(using lttng calls) but present in the list.
        */
        for(sessionItem = TAILQ_FIRST(&sessionList); sessionItem != NULL ;
            sessionItem = elem) {

                elem = TAILQ_NEXT(sessionItem, list);
                handle = lttng_create_handle(sessionItem->te_session.name, &domain);
                if (handle == NULL)
                        return;
                count = lttng_list_channels(handle, &channels);
                if  (count <= 0) {
                        reset_session_id(sessionItem->session_id);
                        free(sessionItem->te_session.name);
                        free(sessionItem->te_session.path);
                        TAILQ_REMOVE(&sessionList, sessionItem, list);
                        free(sessionItem);
                } else {
                        if (handle)
                                lttng_destroy_handle(handle);
                        if (channels)
                                free(channels);
                }
        }
}

/* ===================================================================== */
/**
 *   Restarts sessions created by the ts ip command
 *
 *   @return              0 on successful removing.
 *                        non zero on failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static void handle_ts_restart(union itc_msg *rmsg)
{
        struct session_list_element *sessionItem, *elem;
        int len, ret = 0;
        struct lttng_domain domain = { 0 };
        struct lttng_handle *handle = NULL;
        struct lttng_event *event_list = NULL;
        struct lttng_channel *channels = NULL;
        union itc_msg *msg;
        char *name, *path;
        uint64_t subbuf_size, no_of_subbuf;
        unsigned int switch_interval, flush_interval;
        int no_events, m;
        char additional_info[MAX_ADDITIONAL_INFO_LEN];
        unsigned int force = 1;

        healthcheck();

        sscanf(rmsg->te_ctrl_req.data, "%u", &force);
        if (!force && !ts_correlate_enabled) {
                goto send_rsp;
        }

        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        /*
        ** Walk through the list of sessions and restart them one-by-one
        */
        for(sessionItem = TAILQ_FIRST(&sessionList); sessionItem != NULL ;
            sessionItem = elem) {

                elem = TAILQ_NEXT(sessionItem, list);

                name = sessionItem->te_session.name;
                path = sessionItem->te_session.path;
                subbuf_size = sessionItem->te_session.subbuf_size;
                no_of_subbuf = sessionItem->te_session.no_of_subbuf;
                switch_interval = sessionItem->te_session.switch_interval;
                flush_interval = sessionItem->te_session.flush_interval;

                handle = lttng_create_handle(name, &domain);
                if (handle == NULL) {
                        ret = -1;
                        ERR("lttng_create_handle failed for session:%s",
                            name);
                        goto send_rsp;
                }

                /* get all channels */
                ret = lttng_list_channels(handle, &channels);
                if (ret < 0) {
                        ERR("lttng_list_channels failed, reason:%s(%d)",
                            lttng_strerror(ret), ret);
                        goto send_rsp;
                }

                /* get events for first channel only */
                ret = lttng_list_events(handle, channels[0].name, &event_list);
                if (ret < 0) {
                        ERR("lttng_list_events, %s", lttng_strerror(ret));
                        goto send_rsp;
                }
                no_events = ret;

                /* Destroy the handle and channels from the first connection for this session */
                lttng_destroy_handle(handle);
                handle = NULL;

                free(channels);
                channels = NULL;

                ret = lttng_destroy_session(name);
                if (ret < 0) {
                        ERR("Failed to destroy session %s. reason:%s",
                            name, lttng_strerror(ret));
                        goto send_rsp;
                }
                ret = create_live_stream(name, path, subbuf_size, no_of_subbuf,
                                         switch_interval, flush_interval,
                                         additional_info,
                                         sizeof(additional_info));
                if (ret < 0) {
                        ERR("Failed to recreate session %s.%s",
                            name, additional_info);
                        /* Remove session if re-creation failed. */
                        removeTsSessionDetails(name);
                        goto send_rsp;
                }

                handle = lttng_create_handle(name, &domain);
                if (handle == NULL) {
                        ERR("lttng_create_handle failed for ses:%s", name);
                        goto send_rsp;
                }

                /* get all channels */
                ret = lttng_list_channels(handle, &channels);
                if (ret < 0) {
                        ERR("lttng_list_channels failed, reason:%s(%d)",
                            lttng_strerror(ret), ret);
                        goto send_rsp;
                }

                for (m = 0; m < no_events; m++) {
                        if (event_list[m].enabled) {
                                ret = lttng_enable_event(handle,
                                                         &event_list[m],
                                                         channels[0].name);
                        }
                }

                lttng_destroy_handle(handle);
                handle = NULL;

                free(channels);
                channels = NULL;

                free(event_list);
                event_list = NULL;
        }

        tracepoint(com_ericsson_plf_trace_util, tsRestart, ++ts_restart_count);

send_rsp:
        if (handle)
                lttng_destroy_handle(handle);

        if (channels)
                free(channels);

        if (event_list)
                free(event_list);

        if (ret < 0) {
                len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len,
                                TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
                snprintf(msg->te_ctrl_rsp.data, len, "%s",
                         lttng_strerror(ret));
        } else {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        }
        msg->te_ctrl_rsp.type = TE_RESTART;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return;

}

/* ===================================================================== */
/**
 *   This function sets the value of the correlate option specified
 *   by the user with the command 'ts correlate enable|disable'.
 *   Default is disabled.
 *
 *   @param               rmsg: The incoming message
 *
 *   @return              -
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static void
set_correlate(union itc_msg *rmsg)
{
        union itc_msg *msg;
	unsigned int val = 0;

	msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
        msg->te_ctrl_rsp.type = TE_RESTART;
	sscanf(rmsg->te_ctrl_req.data, "%u", &val);
	ts_correlate_enabled = val;
	msg->te_ctrl_rsp.result = CMD_RESULT_OK;
	itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   This function handles ts ip request and starts an active trace
 *   stream session to a requested ip/hostname.Session will be created
 *   only if session count dont cross maxNumDiagStreamSession.
 *
 *   @param               rmsg pointer to handle signal
 *
 *   @return              0 on successful trace stream creation.
 *                        non zero on failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int handle_ts_ip_request(union itc_msg *rmsg)
{

        char session_name[LTTNG_SYMBOL_NAME_LEN];
        int randomNumber;
        int session_id = 0;
        union itc_msg *msg;
        int ret = 0, len = 0;
        char *tmp;
        char additional_info[MAX_ADDITIONAL_INFO_LEN];
        char *session_error = "Session Limit already reached.";
        char *internal_error = "Session Id could not be retrieved due to internal error";
        char url[INET6_ADDRSTRLEN];
        uint64_t subbuf_size, no_of_subbuf;
        unsigned int switch_interval, flush_interval;

        /* Remove any inactive live sessions that were there
         * from ted database
         */
        remove_inactive_live_sessions();
        if (total_session_count == maxNumDiagStreamSession) {
                /* Session cannot be created because limit reached
                 */
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) +
                                strlen(session_error) + 1, TED_CTRL_RSP);
                snprintf(msg->te_ctrl_rsp.data, strlen(session_error) + 1,
                                                    "%s", session_error);
                msg->te_ctrl_rsp.result = CMD_SESSION_LIMIT_REACHED;
                ret = -1;
        } else {

                /*Generate random number */
                srand((unsigned int) time(0));

                /* If session name is not give, generate an random session name.*/
                if(rmsg->te_ctrl_req.session_name[0] == '\0') {
                        randomNumber = (rand()%9999 + 1);
                        snprintf(session_name, sizeof(session_name),
                           "ts_session%d_%d",total_session_count, randomNumber);
                } else {
                        strcpy(session_name, rmsg->te_ctrl_req.session_name);
                }
                sscanf(rmsg->te_ctrl_req.data, "%s %llu %llu %u %u", url,
                       (unsigned long long*)&subbuf_size,
                       (unsigned long long*)&no_of_subbuf,
                       &switch_interval, &flush_interval);
                ret = create_live_stream(session_name, url, subbuf_size,
                                         no_of_subbuf, switch_interval,
                                         flush_interval, additional_info,
                                         sizeof(additional_info));
                if (ret < 0) {
                        /* Session Cannot be created because of an lttng error
                         */
                        ERR("%s", additional_info);
                        len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
                        msg = itc_alloc(sizeof(struct ted_ctrl_rsp) +
                                        len, TED_CTRL_RSP);
                        snprintf(msg->te_ctrl_rsp.data, len,
                                 "%s", lttng_strerror(ret));
                        msg->te_ctrl_rsp.result = ret;
                } else {
                        /* Successfully started new session, so add it to the list */
                        session_id = addTsSessionDetails(session_name, url,
                                                         subbuf_size,
                                                         no_of_subbuf,
                                                         switch_interval,
                                                         flush_interval,
                                                         false);
                        if (session_id > 0) {
                                asprintf(&tmp, "session_name : %s sessionId : %d",
                                                        session_name, session_id);
                                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) +
                                                strlen(tmp) + 1 , TED_CTRL_RSP);
                                snprintf(msg->te_ctrl_rsp.data, strlen(tmp) + 1, "%s", tmp);
                                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
                                free(tmp);
                        } else {
                                /* Hitting this piece of code is highly unlikely
                                 * just to remove coverity errors.
                                 */
                                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) +
                                               strlen(internal_error) + 1, TED_CTRL_RSP);
                                snprintf(msg->te_ctrl_rsp.data, strlen(internal_error) + 1,
                                                    "%s", internal_error);
                                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
                        }
                }
        }
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);
        return ret;
}

/* ===================================================================== */
/**
 *   Destroys requested trace stream sessions.
 *
 *   @param               rmsg pointer to handle signal
 *
 *   @return              0 on successful destroy of the session
 *                        non zero on failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int destroy_ts_session(union itc_msg *rmsg)
{
        char session_name[LTTNG_SYMBOL_NAME_LEN];
        union itc_msg *msg;
        int ret = 0, len = 0;

        if (rmsg->te_ctrl_req.session_name[0] == '\0') {
                ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                           session_name,
                                           sizeof(session_name));
                if (ret < 0) {
                        ret = -LTTNG_ERR_NO_SESSION;
                        goto send_rsp;
               }
        } else {
                  snprintf(session_name, sizeof(session_name),
                         "%s", rmsg->te_ctrl_req.session_name);
        }

        ret = removeTsSessionDetails(session_name);

send_rsp:
        msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
        if (ret < 0) {
                len = snprintf(NULL, 0, "%s", lttng_strerror(ret)) + 1;
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len,
                                TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
                snprintf(msg->te_ctrl_rsp.data, len, "%s",
                         lttng_strerror(ret));
        } else {
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        }
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}

/* ===================================================================== */
/**
 *   Write a trace to requested trace stream session.
 *
 *   @param               rmsg pointer to handle signal
 *
 *   @return              0 on successful, non zero on failure.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int write_trace_to_session(char *session_name,
                                  char *provider,
                                  char *trace_event,
                                  char *data)
{
        int ret = 0, count = 0;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        handle = lttng_create_handle(session_name, &domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", session_name);
                goto finish;
        }

        /* get all channels */
        count = lttng_list_channels(handle, &channels);
        if (count < 0) {
                ret = count;
                ERR("lttng_list_channels failed,reason:%s(%d)",
                    lttng_strerror(ret), ret);
                goto finish;
        }

        /* enable events on first channel */
        if (count > 0 && &channels[0] != NULL) {
                ret = updateEventsOnSingleProvider(handle,
                                                   provider,
                                                   trace_event,
                                                   channels[0].name,
                                                   true);
                if (ret < 0) {
                        ERR("Failed to enable events %s for provider %s", trace_event, provider);
                        goto finish;
                }
                tracepoint(com_ericsson_plf_trace_util, testLog, data);
                ret = updateEventsOnSingleProvider(handle,
                                                   provider,
                                                   trace_event,
                                                   channels[0].name,
                                                   false);
                if (ret < 0) {
                        ERR("Failed to disable events %s for provider %s", trace_event, provider);
                        goto finish;
                }
        }
finish:
        return ret;

}

static int handle_ts_echo(union itc_msg *rmsg)
{
        char session_name[LTTNG_SYMBOL_NAME_LEN];
        union itc_msg *msg;
        int ret = 0, len;
        char provider[LTTNG_SYMBOL_NAME_LEN];
        char trace_event[LTTNG_SYMBOL_NAME_LEN];

        if (rmsg->te_ctrl_req.session_name[0] == '\0') {
                ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                           session_name,
                                           sizeof(session_name));
                if (ret < 0) {
                        ret = -LTTNG_ERR_NO_SESSION;
                        goto send_rsp;
               }
        } else {
                if (!is_registered_ts_session(rmsg->te_ctrl_req.session_name)) {
                      ret = 1;
                      goto send_rsp;
                }
                snprintf(session_name, sizeof(session_name),
                       "%s", rmsg->te_ctrl_req.session_name);
        }

        strncpy(provider, "com_ericsson_plf_trace_util" , sizeof(provider));
        provider[sizeof(provider) - 1] = '\0';
        strncpy(trace_event, "testLog", sizeof(trace_event));
        trace_event[sizeof(trace_event) - 1] = '\0';

        ret = write_trace_to_session(session_name, provider, trace_event,
                                     rmsg->te_ctrl_req.data);

send_rsp:

        if (ret < 0) {
                len = snprintf(NULL, 0 , "%s", lttng_strerror(ret)) + 1;
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp) + len, TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_EVENT_ACTION_FAILED;
                snprintf(msg->te_ctrl_rsp.data, len, "%s", lttng_strerror(ret));
        } else if (ret == 1){
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_SESSION_NOT_FOUND;
        } else {
                msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
                msg->te_ctrl_rsp.result = CMD_RESULT_OK;
        }
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}

static int handle_session_validation(union itc_msg *rmsg)
{
        char session_name[LTTNG_SYMBOL_NAME_LEN];
        union itc_msg *msg;
        int ret = 0;

        if (rmsg->te_ctrl_req.session_name[0] == '\0') {
                ret = getSessionNameFromId(rmsg->te_ctrl_req.session_id,
                                           session_name,
                                           sizeof(session_name));
                if (ret < 0) {
                        ret = CMD_SESSION_NOT_FOUND;
                } else {
                        ret = CMD_SUCCESS;
                }
        } else {
                if (!is_registered_ts_session(rmsg->te_ctrl_req.session_name)) {
                      if (rmsg->te_ctrl_req.session_name[0] == '-') {
                            /* Additional command was issued instead of
                             * session name
                             */
                            ret = CMD_SUCCESS;
                      } else {
                            ret = CMD_SESSION_NOT_FOUND;
                      }
                } else {
                        ret = CMD_SUCCESS;
                }
        }
        msg = itc_alloc(sizeof(struct ted_ctrl_rsp), TED_CTRL_RSP);
        msg->te_ctrl_rsp.result = ret;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);
        return ret;
}

static int handle_ctrl(union itc_msg *rmsg)
{
        switch (rmsg->te_ctrl_req.type) {
        case TE_ENABLE:
                enable_events(rmsg);
                break;
        case TE_DISABLE:
                disable_events(rmsg);
                break;
        case TE_LOG_READ:
                log_read(rmsg);
                break;
        case TE_LOG_CLEAR:
                log_clear(rmsg);
                break;
        case TE_DEFAULT:
                set_default(rmsg);
                break;
        case TE_FILTER_SET:
                set_filter(rmsg);
                break;
        case TE_FILTER_RESET:
                reset_filter(rmsg);
                break;
        case TE_SAVE:
                save_events(rmsg);
                break;
        case TE_CONFIG:
                preset_events(rmsg, true);
                break;
        case TE_PRESET:
                preset_events(rmsg, false);
                break;
        case TE_SESSION_CREATE:
                handle_ts_ip_request(rmsg);
                break;
        case TE_SESSION_DELETE:
                destroy_ts_session(rmsg);
                break;
        case TE_RESTART:
                if (rmsg->te_ctrl_req.handler == CMD_TS_HANDLER) {
                        handle_ts_restart(rmsg);
                }
                else {
                        handle_te_restart(rmsg);
                }
                break;
        case TE_ECHO:
                handle_ts_echo(rmsg);
                break;
        case TE_SESSION_VALIDATE:
                handle_session_validation(rmsg);
                break;
        case TE_SET_CORRELATE:
                set_correlate(rmsg);
                break;
        default:
                break;
        }
        return 0;
}

static void
status_msg(int sz, struct lttng_event *event_list,
           itc_mbox_id_t mb, int last_msg)
{
        union itc_msg *msg;
        int al_sz = MSG_ARRAY_SIZE;
        int size = sz;


        if (size == 0) {
                msg = itc_alloc(sizeof(struct ted_status_rsp), TED_STATUS_RSP);
                msg->te_st_rsp.data_size.no_of_events = size;
                msg->te_st_rsp.last = 1;
                msg->te_st_rsp.success = 1;
                itc_send(&msg, mb, ITC_MY_MBOX);
                return;
        }

        while(size > 0) {
                if (size < al_sz)
                        al_sz = size;

                msg = itc_alloc(sizeof(struct ted_status_rsp) +
                                al_sz * sizeof(struct lttng_event),
                                TED_STATUS_RSP);
                memcpy(msg->te_st_rsp.data_type.event_list, event_list,
                       al_sz * sizeof(struct lttng_event));
                event_list += al_sz;

                msg->te_st_rsp.data_size.no_of_events = al_sz;
                if (last_msg && al_sz == size)
                        msg->te_st_rsp.last = 1;
                msg->te_st_rsp.success = 1;
                itc_send(&msg, mb, ITC_MY_MBOX);

                size -= al_sz;
        }
        return;
}

/* ===================================================================== */
/**
 *   Sends the status reply to the ts mail box. All the enabled events
 *   of the sessions will be sent.
 *
 *   @param         sz  : no of events to be sent
 *                  event_list : pointer which holds the events data
 *                  mb   :mailbox to which reply has to be sent
 *                  status : pointer which holds the status of the sesion
 *                  session_cnt : which hold the no of remained sessions
 *
 *   @return     --
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */

static void
send_ts_status_msg(int sz, struct lttng_event *event_list,
                   itc_mbox_id_t mb, int last_msg,
                   struct session_details *status,
                   int session_cnt)
{
        union itc_msg *msg;
        int al_sz = MSG_ARRAY_SIZE;
        int size = sz;


        if (size == 0) {
                msg = itc_alloc(sizeof(struct ted_status_rsp), TED_STATUS_RSP);
                msg->te_st_rsp.data_size.no_of_events = size;
                msg->te_st_rsp.current_session.session_list = status->session_list;
                snprintf(msg->te_st_rsp.current_session.time_of_creation,
                         sizeof(msg->te_st_rsp.current_session.time_of_creation),
                         "%s", status->time_of_creation);
                msg->te_st_rsp.last = 1;
                msg->te_st_rsp.success = 1;
                itc_send(&msg, mb, ITC_MY_MBOX);
                return;
        }

        while(size > 0) {
                if (size < al_sz)
                        al_sz = size;

                msg = itc_alloc(sizeof(struct ted_status_rsp) +
                                al_sz * sizeof(struct lttng_event),
                                TED_STATUS_RSP);
                memcpy(msg->te_st_rsp.data_type.event_list, event_list,
                       al_sz * sizeof(struct lttng_event));
                msg->te_st_rsp.current_session.session_list = status->session_list;
                msg->te_st_rsp.session_count = session_cnt;
                msg->te_st_rsp.current_session.time_of_creation[0] = '\0';
                snprintf(msg->te_st_rsp.current_session.time_of_creation,
                         sizeof(msg->te_st_rsp.current_session.time_of_creation),
                         "%s", status->time_of_creation);
                msg->te_st_rsp.current_session.session_id = status->session_id;
                event_list += al_sz;

                msg->te_st_rsp.data_size.no_of_events = al_sz;
                if (last_msg && al_sz == size)
                        msg->te_st_rsp.last = 1;
                msg->te_st_rsp.success = 1;
                itc_send(&msg, mb, ITC_MY_MBOX);

                size -= al_sz;
        }
        return;
}

static char *get_cmdline_by_pid(pid_t pid)
{
        int ret;
        FILE *fp;
        char *cmdline;
        char path[24];

        snprintf(path, sizeof(path), "/proc/%d/cmdline", pid);
        fp = fopen(path, "r");
        if (fp == NULL) {
                return NULL;
        }

        cmdline = malloc(PATH_MAX);
        ret = fread(cmdline, 1, PATH_MAX, fp);
        if (ret < 0) {
                ERR("Failed fread proc list");
                free(cmdline);
                return NULL;
        }
        fclose(fp);

        return cmdline;
}

static int
copy_events(int i, pid_t cur_pid, struct lttng_event *event_list,
            struct lttng_event **el)
{
        int n, cnt = 0, l = -1;

        /* count no of elem with same pid */
        for (n = i; event_list[n].pid == cur_pid; n++) {
                cnt++;
        }

        if (!(*el = calloc(cnt, sizeof(struct lttng_event))))
                goto out;

        for (n = i, l = 0; event_list[n].pid == cur_pid; n++) {
                (*el)[l] = event_list[n];
                l++;
        }
 out:
        return l;
}

static int handle_status_all(union itc_msg *rmsg)
{
        int ret = 0, last_msg = 0;
        int count, size, ses_cnt, i, n;
        union itc_msg *msg;
        itc_mbox_id_t mb = itc_sender(rmsg);

        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_event *event_list = NULL, *el = NULL;
        struct lttng_channel *channels = NULL;
        struct lttng_session *sessions = NULL;
        pid_t cur_pid = 0;
        char *cmdline = NULL;
        int no_ev_to_send = -1;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        ses_cnt = lttng_list_sessions(&sessions);
        if (ses_cnt < 0) {
                ret = ses_cnt;
                ERR("Failed to list sessions");
                return -1;
        }


        /* Session walk */
        for (i = 0; i < ses_cnt; i++) {
                /* Only list events for default session for now */

                if (strcmp(sessions[i].name, lttng_sessions[0].name))
                        continue;

                handle = lttng_create_handle(sessions[i].name, &domain);
                if (handle == NULL) {
                        ret = -1;
                        ERR("lttng_create_handle failed for ses:%s",
                             sessions[i].name);
                        goto err;
                }

                if (rmsg->te_st_req.type == TE_STATUS_LIST) {
                        size = lttng_list_tracepoints(handle, &event_list);
                        if (size < 0) {
                                ret = size;
                                ERR("lttng_list_tracepoints, %s",
                                     lttng_strerror(ret));
                                goto err;
                        }
                        for (n = 0; n < size; n++) {
                                if (cur_pid != event_list[n].pid) {
                                        cur_pid = event_list[n].pid;
                                        if (!(cmdline = get_cmdline_by_pid(cur_pid))) {
                                                ERR("get_cmdline_by_pid failed, pid %d",
                                                     cur_pid);
                                                goto err;
                                        }
                                        if (!strcasecmp(basename(cmdline),
                                                        rmsg->te_st_req.data)) {
                                                free(cmdline);
                                                no_ev_to_send = copy_events(n,
                                                                            cur_pid,
                                                                            event_list,
                                                                            &el);
                                                break;
                                        }
                                        free(cmdline);
                                }
                        }
                        if (event_list)
                                free(event_list);
                        event_list = NULL;
                        if (no_ev_to_send >= 0) {
                                status_msg(no_ev_to_send, el, mb, 1);
                                if (el) free(el);
                        } else
                                goto err;
                } else {

                        /* get all channels */
                        count = lttng_list_channels(handle, &channels);
                        if (count < 0) {
                                ret = count;
                                ERR("lttng_list_channels failed,reason:%s(%d)",
                                    lttng_strerror(ret), ret);
                                goto err;
                        }

                        /* channel walk for one session */
                        for (n = 0; n < count; n++) {
                                if (count - 1 == n)
                                        last_msg = 1;

                                /* get events for channel */
                                size = lttng_list_events(handle, channels[n].name,
                                                        &event_list);
                                if (size < 0) {
                                        ret = size;
                                        ERR("lttng_list_events, %s",
                                             lttng_strerror(ret));
                                        goto err;
                                }
                                status_msg(size, event_list, mb, last_msg);

                                if (event_list)
                                        free(event_list);
                                event_list = NULL;
                        }
                        if (channels)
                                free(channels);
                        channels = NULL;
                }
                lttng_destroy_handle(handle);
                handle = NULL;
        }
        return ret;

 err:
        if (channels)
                free(channels);
        if (event_list)
                free(event_list);
        if (sessions)
                free(sessions);
        if (handle)
                lttng_destroy_handle(handle);

        /* Send response */
        msg = itc_alloc(sizeof(struct ted_status_rsp), TED_STATUS_RSP);
        msg->te_st_rsp.data_size.no_of_events = 0;
        msg->te_st_rsp.success = 0;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}

static int handle_preset_restart_status(union itc_msg *rmsg)
{
        int count = 0;
        int leftOver = 0;
        int preset_count = 0;
        int preset = 0;
        union itc_msg *msg;
        bool itemFound = false;
        struct presetListElement *saveItem = NULL;
        struct presetListElement *tmp_elem1;
        itc_mbox_id_t mb = itc_sender(rmsg);
        int totalPresetCount = 0;
        int totalSavedPresetCount = 0;

        msg = itc_alloc(sizeof(struct ted_status_rsp) +
                        MSG_ARRAY_SIZE* sizeof(struct lttng_event),
                        TED_STATUS_RSP);
        msg->te_st_rsp.data_size.no_of_events = 0;
        msg->te_st_rsp.success = 1;

        (void)checkPresetOverFlow(&teSessionPresetList,
                                  &totalPresetCount,
                                  &totalSavedPresetCount);

        if (!strcmp(rmsg->te_st_req.data, "-restart")) {
                preset_count = totalSavedPresetCount;
        }
        else {
                preset_count = totalPresetCount;
                preset = 1;
        }

        for (saveItem = TAILQ_LAST(&teSessionPresetList, presetListHead);
                        saveItem != NULL; saveItem = tmp_elem1) {
             tmp_elem1 = TAILQ_PREV(saveItem, presetListHead, list);
             if (saveItem->saved || preset) {
                strcpy(msg->te_st_rsp.data_type.event_list[leftOver].name,
                       saveItem->itemInfo);
                msg->te_st_rsp.data_type.event_list[leftOver].enabled = saveItem->enable;
                leftOver++;
                msg->te_st_rsp.data_size.no_of_events = leftOver;
                count++;
                itemFound = true;
                /*
                 * Check it the message is filled up. If so se status indication
                 * and allocate a new message when there is more to send.
                 */
                if (leftOver == MSG_ARRAY_SIZE) {

                        if (count == preset_count) {
                             msg->te_st_rsp.last = 1;
                        }
                        else {
                             msg->te_st_rsp.last = 0;
                        }
                        itc_send(&msg, mb, ITC_MY_MBOX);
                        leftOver = 0;
                        msg = itc_alloc(sizeof(struct ted_status_rsp) +
                                MSG_ARRAY_SIZE* sizeof(struct lttng_event),
                                TED_STATUS_RSP);

                }
             }
        }
        /*
         * Send the last item/items if the status indication was not filled up..
         */
        if (leftOver || !itemFound){
              msg->te_st_rsp.last = 1;
              itc_send(&msg, mb, ITC_MY_MBOX);
        }
        return count;
}

/* ===================================================================== */
/**
 *  Counts the number of user id directories and the size of the user id
 *  names.
 *
 *   @param      See ftw.h
 *
 *   @return     Zero at success.
 *
 *   @par Globals:
 *               dir_names_size, no_of_dirs
 */
/* ===================================================================== */
static int
check_uid_dirs(const char *path, const struct stat *sb,
               int flag, struct FTW *ftwbuf)
{
        char *endptr;
        char *dir_name;

        switch(flag) {
        case FTW_D:
                dir_name = basename(path);
                (void)strtol(dir_name, &endptr, 10);

                if (endptr != NULL && *endptr != '\0') {
                        return FTW_CONTINUE;
                }
                else if (strcmp(endptr, "") == 0) {
                          /* New uid number, copy name to respond signal */
                          dir_names_size += strlen(dir_name) + 1;
                          no_of_dirs++;
                }
                break;
        default:
                break;
        }

        return FTW_CONTINUE;
}

/* ===================================================================== */
/**
 *  Copies the found user id directory names to the position msg_data_pos
 *  in the status respond signal. As several snapshot directories might be
 *  traversed, duplicates of user id directory names might be read. Those
 *  duplictes are not to be copied (.../ust/uid/<user id name>).
 *
 *   @param      See ftw.h
 *
 *   @return     Zero at success.
 *
 *   @par Globals:
 *               msg_data_start, msg_data_pos, dir_names_size, no_of_dirs
 */
/* ===================================================================== */
static int
collect_uid_dirs(const char *path, const struct stat *sb,
                 int flag, struct FTW *ftwbuf)
{
        char *endptr;
        char *dir_name;
        char *pos;
        char *end_pos = msg_data_start + dir_names_size;
        int len;

        switch(flag) {
        case FTW_D:
                dir_name = basename(path);
                (void)strtol(dir_name, &endptr, 10);

                /* Directory name is not a number */
                if (endptr != NULL && *endptr != '\0') {
                        return FTW_CONTINUE;
                }
                else if (strcmp(endptr, "") == 0)  {
                        /*
                         * Directory name is a number, check against the
                         * copied numbers and do not copy if it's already
                         * copied.
                         */
                        pos = msg_data_start;
                        while (pos < end_pos) {
                                if (strcmp(pos, dir_name) == 0) {
                                        no_of_dirs--;
                                        return FTW_CONTINUE;
                                }
                                len = strlen(pos) + 1;
                                while (len-- > 0) pos++;
                        }
                        snprintf(msg_data_pos, strlen(dir_name) + 1,
                                 "%s", dir_name);
                        msg_data_pos += strlen(dir_name) + 1;
                }
                break;
        default:
                fprintf(stderr, "File %s is not a directory\n", path);
                break;
        }

        return FTW_CONTINUE;
}

/* ===================================================================== */
/**
 *   Sends the status reply with all available user id which LTTng is
 *   tracing.
 *
 *   @param      --       rmsg message pointer to handle request signal
 *
 *   @return     --
 *
 *   @par Globals:
 *               dir_names_size, no_of_dirs, msg_data_start, msg_data_pos
 */
/* ===================================================================== */
static int
handle_status_uid(union itc_msg *rmsg)
{
        union itc_msg *msg;
        char *path = lttng_sessions[0].path;
        char string[] = "-1";

        /*
         * Count number of uid directories and the size of these
         * directory names.
         */
        dir_names_size = 0;
        no_of_dirs = 0;
        nftw(path, check_uid_dirs, FOPEN_MAX, 0);

        msg = itc_alloc(sizeof(struct ted_status_rsp) + dir_names_size,
                        TED_STATUS_RSP);

        /* Update respond signal with the uid directory names. */
        msg_data_start = msg_data_pos = (&msg->te_st_rsp.data_type.elem[0]);
        memcpy(&(msg->te_st_rsp.data_type.elem[0]), string, dir_names_size);

        nftw(path, collect_uid_dirs, FOPEN_MAX, 0);

        /* Send respond signal */
        msg->te_st_rsp.data_size.no_of_elems = no_of_dirs;
        msg->te_st_rsp.success = 1;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return 1;
}

/* ===================================================================== */
/**
 *   Sends the status reply.All the enabled events
 *   of the sessions will be sent.
 *
 *   @param      --       rmsg message pointer to handle request signal
 *
 *   @return     --
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int handle_ts_status(union itc_msg *rmsg)
{
        struct session_details tmp_elem1, empty_elem;
        itc_mbox_id_t mb = itc_sender(rmsg);
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_event *event_list = NULL;
        struct lttng_channel *channels = NULL;
        struct lttng_session *sessions = NULL;
        union itc_msg *msg;
        int atleast_one_sent = 0;
        int ses_cnt = 0, count =0, n;
        char session_name[LTTNG_SYMBOL_NAME_LEN];
        int ret = 0, last_msg = 0, size, i, len;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;
        empty_elem.time_of_creation[0] = '\0';

        if(rmsg->te_st_req.session_name[0] == '\0') {
               ret = getSessionNameFromId(rmsg->te_st_req.session_id,
                                          session_name,
                                          sizeof(session_name));
               if (ret < 0) {
                        session_name[0] = '\0';
               }
        } else {
                snprintf(session_name, sizeof(session_name),
                         "%s",rmsg->te_st_req.session_name);
        }
        len = getlength(session_name);
        /* get the session details from lttng
         */
        ses_cnt = lttng_list_sessions(&sessions);
        if (ses_cnt < 0) {
                ret = ses_cnt;
                ERR("Failed to list sessions");
                goto err;
        }

        /* Session walk */
        for (i = 0; i < ses_cnt; i++) {
                /* list events for every session
                 */

                /* Skip if it was a default session, i.e session created for
                 * trace and error.
                 */
                if (!strcmp(sessions[i].name, lttng_sessions[0].name))
                        continue;

                /* skip it was not a active trace stream session.
                */
                if (!isActiveTsSession(sessions[i].name, &tmp_elem1)) {
                        continue;
                }

                /* Skip if is not the session requested
                 */
                if (!(strncmp(session_name, sessions[i].name, len) == 0)) {
                        continue;
                }


                handle = lttng_create_handle(sessions[i].name, &domain);
                if (handle == NULL) {
                        ret = -1;
                        ERR("lttng_create_handle failed for ses:%s", sessions[i].name);
                        goto err;
                }

                /* get all channels */
                count = lttng_list_channels(handle, &channels);
                if (count < 0) {
                        ret = count;
                        ERR("lttng_list_channels failed, reason:%s(%d)",
                            lttng_strerror(ret), ret);
                        goto err;
                }

                /* channel walk for one session */
                for (n = 0; n < count; n++) {
                        if (ses_cnt - 1 == i && count - 1 == n)
                                last_msg = 1;

                        /* get events for channel */
                        size = lttng_list_events(handle, channels[n].name, &event_list);
                        if (size < 0) {
                                ret = size;
                                ERR("lttng_list_events, %s", lttng_strerror(ret));
                                goto err;
                        }
                        atleast_one_sent = 1;
                        /* Copy all the session details
                         */
                        tmp_elem1.session_list =  sessions[i];
                        /* send the particular session details
                         */
                        send_ts_status_msg(size, event_list, mb, last_msg,
                                           &tmp_elem1, ses_cnt - i);

                        if (event_list)
                                free(event_list);
                        event_list = NULL;
                }
                if (channels)
                        free(channels);
                channels = NULL;

                lttng_destroy_handle(handle);
                handle = NULL;
        }
        /* sending an empty signal if last message flag was not set or if there are no
         * active streaming sesions
         */
        if (atleast_one_sent == 0 || last_msg == 0) {
                send_ts_status_msg(0, event_list, mb, 1, &empty_elem, 0);
        }
        return ret;

 err:
        if (channels)
                free(channels);
        if (event_list)
                free(event_list);
        if (sessions)
                free(sessions);
        if (handle)
                lttng_destroy_handle(handle);

        /* Send response */
        msg = itc_alloc(sizeof(struct ted_status_rsp), TED_STATUS_RSP);
        msg->te_st_rsp.data_size.no_of_events = 0;
        msg->te_st_rsp.success = 0;
        itc_send(&msg, itc_sender(rmsg), ITC_MY_MBOX);

        return ret;
}

static int handle_status(union itc_msg *rmsg)
{
        int ret;

        if (rmsg->te_st_req.handler == CMD_TS_HANDLER) {
                ret = handle_ts_status(rmsg);
        }
        else {
                if (!strcmp(rmsg->te_st_req.data, "-restart") ||
                    !strcmp(rmsg->te_st_req.data, "-preset")) {
                        ret = handle_preset_restart_status(rmsg);
                }
                else if (!strcmp(rmsg->te_st_req.data, "-uid")) {
                        ret = handle_status_uid(rmsg);
                }
                else {
                        ret = handle_status_all(rmsg);
                }
        }
        return ret;
}

static int
enable_events_on_session(char *session_name,
                         struct presetListHead *list_head,
                         FILE *fp)
{
        int ret = 0;
        int provLen = 0;
        int isEnable = 0;
        char buf[LTTNG_SYMBOL_NAME_LEN] = {0};
        char provider[LTTNG_SYMBOL_NAME_LEN];
        char eventStr[LTTNG_SYMBOL_NAME_LEN];
        char *eventIndexPtr;
        struct lttng_domain domain;
        struct lttng_handle *handle = NULL;
        struct lttng_channel *channels = NULL;
        struct presetListElement *elem;
        int totalSavedPresetCount;
        int totalPresetCount;

        memset(&domain, 0, sizeof(domain));
        domain.type = LTTNG_DOMAIN_UST;
        domain.buf_type = LTTNG_BUFFER_PER_UID;

        handle = lttng_create_handle(session_name, &domain);
        if (handle == NULL) {
                ret = -1;
                ERR("lttng_create_handle failed for ses:%s", lttng_sessions[0].name);
                goto out;
        }

        /* Get all channels. If channels are not created by the time ted is started,
         * wait till atleast one channel is available.
         */
        do {
              ret = lttng_list_channels(handle, &channels);
              if (ret <= 0) {
                 usleep(5000);
              }
        } while (ret <= 0);

        /* Just to make sure the count
         */
        (void)checkPresetOverFlow(list_head, &totalPresetCount,
                                  &totalSavedPresetCount);

        /* read file and enable events */
        while(fscanf(fp, "%s %d",buf ,&isEnable) > 0) {
                /* Skip blank lines */
                if (buf[0] == '\n' || buf[0] == 0)
                        continue;

                /* Write the preset list from the lttng saved file
                 */

                if (totalSavedPresetCount < MAX_SAVED_PRESET_COUNT) {
                       elem = (struct presetListElement*)malloc(sizeof(struct presetListElement));
                       asprintf(&elem->itemInfo,"%s",buf);
                       elem->enable = isEnable;
                       elem->saved = true;
                       TAILQ_INSERT_TAIL(list_head, elem, list);
                       totalSavedPresetCount ++;
                       totalPresetCount ++;
                }
                /* Now enable the corresponding trace groups
                 */
                eventIndexPtr = strchr(buf, ':');
                if (eventIndexPtr == NULL)
                       continue;
                provLen = eventIndexPtr - buf;
                if (provLen >= LTTNG_SYMBOL_NAME_LEN) {
                        provLen = LTTNG_SYMBOL_NAME_LEN - 1;
                }
                strncpy(provider, buf, provLen);
                provider[provLen] = '\0';
                eventIndexPtr ++;
                strcpy(eventStr, eventIndexPtr);
                if ((!strncmp(provider, LTTNG_PROVIDER, strlen(LTTNG_PROVIDER)) &&
                     strchr(provider, '*')) ||
                    (!strcmp(provider, "*"))) {
                        /*
                         * Wild card is given, enable the events for all
                         * matching providers.
                         */
                        ret = updateEventsOnMultiProviders(handle,
                                                           channels[0].name,
                                                           provider,
                                                           eventStr,
                                                           isEnable);
                } else {
                        /*
                         * Enable the all given events on single provider.
                         */
                        ret = updateEventsOnSingleProvider(handle,
                                                           provider,
                                                           eventStr,
                                                           channels[0].name,
                                                           isEnable);
                }
        }
out:
        if (handle)
                lttng_destroy_handle(handle);
        if (channels)
                free(channels);
        return ret;
}

/* ===================================================================== */
/**
 *  This function enables the saved events on the s1 session which is
 *  created for trace-error purpose.
 *
 *   @param              -
 *
 *   @return             0 on Success
 *                       negative value on Failure
 *
 *   @par Globals:
 *               ted_saved_ev_file
 */
/* ===================================================================== */
static int enable_te_saved_events()
{
        FILE *fp = NULL;
        int ret = 0;
        char *path, *pfs;

        /* trace pramfs path */
        if ((pfs = getenv("PFS_PATH")) == NULL) {
                return -1;
        }

        while(access(pfs, F_OK)){
           usleep(50000);
        }

        /* pfs path is available now.
         * Enable all te session saved events
         */
        asprintf(&path, "%s" "/" SAVED_EVENTS_FILE, pfs);
        /* Here ted_saved_ev_file contains the saved file path for
         * te session (s1). Hence it should not be freed it will be
         * used in later point of time when ever te save is called.
         */
        ted_saved_ev_file = path;

        /* Open te session saved file if exists.
         */
        if (access(path, F_OK) == 0){

              fp = fopen(path, "r");
              if (fp) {
                     /* Enabling saved events on te session
                      */
                     ret = enable_events_on_session(lttng_sessions[0].name,
                                                   &teSessionPresetList, fp);
                     fclose(fp);
              }
              else
                     ERR("Failed to open %s errno:%d", path, errno);
        }

        return ret;
}

/* ===================================================================== */
/**
 *  This function creates the saved trace stream sessions and enables
 *  saved events.
 *
 *   @param              -
 *
 *   @return             0 on success
 *                       negative value on Failure
 *
 *   @par Globals:
 *               ted_live_stream_ev_file
 *               ts_create_session_retries
 */
/* ===================================================================== */
static int enable_ts_saved_events()
{
        FILE *fp = NULL;
        int ret = 0;
        char *path = NULL, *sessionfile = NULL, *filepath = NULL, *pfs = NULL;
        DIR *d = NULL;
        struct dirent *dir = NULL;
        int fileOffset = 0;
        char *sessionName = NULL;
        char url[INET6_ADDRSTRLEN] = {0};
        uint64_t subbuf_size = 0, no_of_subbuf = 0;
        unsigned int switch_interval = 0, flush_interval = 0;
        char *get_url = NULL;
        char *filePtr = NULL;
        char additional_info[MAX_ADDITIONAL_INFO_LEN] = {0};
        struct presetListHead *list_head = NULL;

        /* Enabling saved ts sessions only if the host daemon is alive.
         */
        if (ted_live_stream_ev_file == NULL) {
                /* trace pramfs path */
                if ((pfs = getenv("PFS_PATH")) == NULL) {
                        return -1;
                }
                asprintf(&path, "%s" "/" SAVED_SESSIONS_PATH, pfs);
               /* Here ted_live_stream_ev_file contains the path of the
                * saved events of the live sessions created by ts command.
                * This pointer will be used at later pointer of time when
                * ts save is called.
                */
               ted_live_stream_ev_file = path;
        }
        path = ted_live_stream_ev_file;

        /*
         * Check that the user has permissions, if not respond back with error
         * to make this visible.
         */
        if (access(path, R_OK | X_OK) == -1) {
                /* no saved events */
                if (errno == ENOENT) {
                        return 0;
                } else {
                        ERR("No saved event: %s", strerror(errno));
                        return -1;
                }
        }

        d = opendir(ted_live_stream_ev_file);
        if (d) {
                while ((dir = readdir(d)) != NULL) {

                        if (dir->d_type == DT_REG) {
                              asprintf(&sessionfile, "%s", dir->d_name );
                        }
                        else {
                              continue;
                        }
                        /* Only process files starting with ts_stream */
                        if (strncmp(sessionfile, "ts_stream", strlen("ts_stream")) != 0) {
                                goto cleanup;
                        }
                        else {
                                /* Example file name :ts_stream-session_name.txt
                                 * "-" is wild card introduced to find session name
                                 */
                                filePtr = strchr(sessionfile, '-');
                                if (filePtr == NULL) {
                                        goto cleanup;
                                }
                                fileOffset = filePtr - sessionfile + 1;
                                /* session length will be totalfile length -
                                 * strlen("ts_stream-") - (strlen(".txt"))
                                 */
                                sessionName = strndup(sessionfile + fileOffset,
                                              strlen(sessionfile) -
                                              fileOffset - 4);
                        }
                        asprintf(&filepath,"%s" "%s", path, sessionfile);
                        fp = fopen(filepath , "r");
                        if (!fp){
                                ERR("Failed to open %s: %s",
                                    filepath, strerror(errno));
                                free(filepath);
                                free(sessionfile);
                                free(sessionName);
                                return -1;
                        }
                        ret = fscanf(fp, "%s %llu %llu %u %u", url,
                                     (unsigned long long*)&subbuf_size,
                                     (unsigned long long*)&no_of_subbuf,
                                     &switch_interval, &flush_interval);
                        if ((ret == EOF) || (ret == 0)) {
                           ERR("Failed to parse parameters <%s>: %s",
                               url, strerror(errno));
                           free(filepath);
                           free(sessionfile);
                           free(sessionName);
                           return -1;
                        }
                        ret = create_live_stream(sessionName, url, subbuf_size,
                                                 no_of_subbuf, switch_interval,
                                                 flush_interval,
                                                 additional_info,
                                                 sizeof(additional_info));
                        if (ret < 0) {
                                /* Write in the log only once to avoid
                                 * flooding the log.
                                 */
/*
                                if (ts_create_session_retries == 0) {
*/
                                        INFO("%s", additional_info);
/*
                                }
*/
                                goto cleanup;
                        }
                        /* Successfully started new session, add it to list */
                        if (total_session_count < maxNumDiagStreamSession) {
                                (void)addTsSessionDetails(sessionName, url,
                                                          subbuf_size,
                                                          no_of_subbuf,
                                                          switch_interval,
                                                          flush_interval,
                                                          false);
                                 update_saved_sessions_data(sessionName, true);
                        } else {
                                ERR("Maximum allowed live sessions limit reached");
                        }

                        ret = getSessionPresetList(sessionName, &list_head,
                                                   &get_url, &subbuf_size,
                                                   &no_of_subbuf,
                                                   &switch_interval,
                                                   &flush_interval);
                        /* Enable events only if session creation is succesful
                         */
                        if (ret == 0) {
                                (void)enable_events_on_session(sessionName, list_head, fp);
                        }

cleanup:
                        if (fp) {
                                fclose(fp);
                        }
                        free(sessionName);
                        sessionName = NULL;
                        free(filepath);
                        filepath = NULL;
                        free(sessionfile);
                        sessionfile = NULL;
                        free(get_url);
                        get_url = NULL;
              } /* while */
        }
        else {
                ERR("Failed to open directory %s, errno:%d",
                    ted_live_stream_ev_file, errno);
        }
        if (ret == 0) {
                INFO("Re-created saved ts session(s), %d retries performed.",
                     ts_create_session_retries);
        }
        return ret;
}

/* ===================================================================== */
/**
 *  This function starts the single shot timer for 100sec which notifies
 *  sigev_notify_function.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       -
 */
/* ===================================================================== */
static void
start_ts_create_retry_timer(void)
{
        struct sigevent sig;
        timer_t timerid;
        struct itimerspec in;
        pthread_attr_t attr;
        struct sched_param parm;

        pthread_attr_init( &attr );
        parm.sched_priority = 255;
        pthread_attr_setschedparam(&attr, &parm);

        sig.sigev_notify = SIGEV_THREAD;
        sig.sigev_signo = 0;
        sig.sigev_notify_function = handle_ts_create_session_retry_tmo;
        sig.sigev_notify_attributes = &attr;

        if (!timer_create(CLOCK_REALTIME, &sig, &timerid)) {

                in.it_value.tv_sec = 0;
                in.it_value.tv_nsec = SAMPLE_INTERVAL * 1000000;
                in.it_interval.tv_sec = 0;
                in.it_interval.tv_nsec = 0;
                if (timer_settime(timerid, 0, &in, NULL) == -1) {
                        ERR("Setting up of timer failed");
                }
        }
        else {
                ERR("Setting up of timer failed");
        }
}

/* ===================================================================== */
/**
 *  This function handles sigevent from the timer on expiration of the
 *  time specified.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       -
 */
/* ===================================================================== */
static void handle_ts_create_session_retry_tmo(union sigval val)
{
        int ret;

        if (ts_create_session_retries == MAX_TS_CREATE_RETRIES) {
                ERR("Saved TS sessions are not restored event after %d retries",
                     ts_create_session_retries);
        } else {
                ts_create_session_retries ++;
                if ((ret = enable_ts_saved_events()) < 0) {

                   if (ret != last_lttng_err) {
                      INFO("Could not setup ts session (%d retries): %s",
                           ts_create_session_retries, lttng_strerror(ret));
                           last_lttng_err = ret;
                   }
                   start_ts_create_retry_timer();
                }
        }

}

/* Exit gracefully at SIGTERM. It is done mostly for getting GCOV data during
 * the component tests execution. */
void term(int signum)
{
   exit(0);
}

int main()
{
        itc_mbox_id_t my_mb;
        union itc_msg *msg;
        uint32_t fltr[] = {0};
        int i, ret;

        struct sigaction action;
        memset(&action, 0, sizeof(struct sigaction));
        action.sa_handler = term;
        sigaction(SIGTERM, &action, NULL);

        openlog("ted", LOG_PID | LOG_ODELAY, LOG_DAEMON);

        /* read initial env vars */
        init_env();

        /* Initiate itc */
        if (itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) < 0) {
                ERR("Failed to init ITC");
                return CMD_ERROR;
        }

        /* Create itc mailbox */
        my_mb = itc_create_mailbox(TED_NAME, 0);
        if (my_mb == ITC_NO_ID) {
                ERR("Failed to create mailbox");
                return CMD_ERROR;
        }

        /* wait for sessiond */
        for (i = 0; i < 100; i++) {
                if (lttng_session_daemon_alive())
                        break;
                if (i + 1 == 100) {
                        ERR("lttng session daemon is dead");
                }
                usleep(500 * i);
        }

        /* Capture the enabled events at start up.
         * These events will be the events defined in the startup
         * script file. It will be reused when "te default or
         * ts default" is issued. Do not free this allocation.
         */
        te_session_info_at_restart = get_session_details(lttng_sessions[0].name);

        /* enable saved events if any */
        if (enable_te_saved_events() < 0)
                /* continue anyway */
                ERR("Failed to enable saved events");

        if ((ret = enable_ts_saved_events()) < 0) {
#if 0
                /* This is a work around to create live sessions.
                 * We will retry every 100 msec for every failure.
                 */
#endif
                INFO("Could not re-create saved ts session(s), %d retries: %s",
                     ts_create_session_retries, lttng_strerror(ret));
#if 0
                ts_create_session_retries++;
                start_ts_create_retry_timer();
#endif
        }

        /* Set default umask */
        umask(0022);

        while (1) {
                msg = itc_receive(fltr, ITC_NO_TMO, ITC_FROM_ALL);

                switch(msg->msg_no) {
                case TED_STATUS_REQ:
                        handle_status(msg);
                        break;
                case TED_CTRL_REQ:
                        handle_ctrl(msg);
                        break;
                default:
                        break;
                }
                itc_free(&msg);
        }
        return 0;
}

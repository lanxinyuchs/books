/**
 *   This file handles 'ts' shell commands targeted for the TED server.
 *
 *   @file ts_ted_handler.c
 *
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
 *   Revised : 2016-07-06 Fredrik Skog
 *   Change  : Moved command timeout to trace_cmd.h.
 *
 * ========================================================================
 */
/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <itc.h>

#include "trace_cmd.h"
#include "ts_internal.h"
#include "te_internal.h"
#include "ted.h"

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* TRI provider names */
#define TRI_PROVIDER_NAME_1 "com_ericsson_trithread"
#define TRI_PROVIDER_NAME_2 "com_ericsson_triobjif"

#define MAX_ERR_INFO_LEN (256)
#define MAX_URL_LEN      (256)

/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

union itc_msg
{
   uint32_t                msg_no;
   struct ted_status_req   statusReq;
   struct ted_status_rsp   statusRsp;
   struct ted_ctrl_req     ctrlReq;
   struct ted_ctrl_rsp     ctrlRsp;
};

/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

static itc_mbox_id_t ted_mb = ITC_NO_ID;

/* ============================================================================
 *   FUNCTIONS
 * ============================================================================
 */

/* ========================================================================= */
/**
 *   Locate the TED server.
 *
 *   @return 0 if successful, -1 otherwise
 *
 *   @global ted_mb
 */
/* ========================================================================= */
static int
locateServer(void)
{
   ted_mb = itc_locate(TED_NAME);
   if (ted_mb == ITC_NO_ID) {
      ERR("Failed to locate ted");
      return -1;
   }

   return 0;
}


/* ========================================================================= */
/**
 *   Print session status.
 *
 *   @param msg - message pointer to handle signal.
 */
/* ========================================================================= */
void
printSessionStatus(union itc_msg *msg)
{
   if (msg->statusRsp.current_session.time_of_creation[0] != '\0')
   {
      MSG("---------------------------------------------");
      MSG("Lttng Session   :%s",
          msg->statusRsp.current_session.session_list.name);
      MSG("Time of creation:%s",
          msg->statusRsp.current_session.time_of_creation);
      MSG("Session ID      :%d",
          msg->statusRsp.current_session.session_id);
      MSG("Status          :%s",
          msg->statusRsp.current_session.session_list.enabled ?
          "active  ":"inactive");
      MSG(" ");
   }
}

/* ========================================================================= */
/**
 *   Support functionality to print status output
 *
 *
 *   @param     msg         message pointer to handle signal.
 *   @param     first_line  To distingush status output.
 *
 *   @return     --
 *
 */
/* ========================================================================= */
static void
printStatus(union itc_msg *msg, bool isFirstLine)
{
   int i;

   if (isFirstLine &&
       msg->statusRsp.current_session.time_of_creation[0] != '\0')
   {
      /* Adding new line at end of session details */
      MSG(" ");
      printSessionStatus(msg);

      if (msg->statusRsp.data_size.no_of_events) {
         MSG("Enabled events: ");
      }
      else {
         MSG("No events enabled:");
      }
   }

   for (i = 0; i < msg->statusRsp.data_size.no_of_events; i++) {
      ted_printEvent(&msg->statusRsp.data_type.event_list[i]);
   }

}


/* ===================================================================== */
/**
 *   Parses the input for an IPv4 or IPv6 address. If LTTng ports are
 *   included, these are removed from the string.
 *
 *   @param               string        String to parse.
 *
 *   @param               addr          IPv4/IPv6 of the target, if found.
 *
 *   @return              0 on successful execution, or  -1 on error.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int parse_addr(char *string, char **addr)
{
        char *addr_start = NULL, *addr_end = NULL;
        int ret;

        if (strncmp(string, "localhost", 9) == 0) {
           *addr = string;
           return 0;
        }

        /* Check for IPv6 synatx, [fe80::f66d:4ff:fe53:d220]:<port>:<port> */
        addr_start = strstr(string, "[");
        if (addr_start) {
                addr_start++;
                addr_end = strstr(addr_start, "]");
                if (addr_end) {
                        /* End the string before the ports */
                        addr_end[0] = '\0';
                        *addr = addr_start;
                        /* Don't allow any dot in address */
                        ret = (strstr(*addr, ".") == NULL ? 0 : 1);
                        /* Address shall include colon */
                        return -(ret || strstr(*addr, ":") == NULL ? 1 : 0);
                } else {
                        return -1;
                }
        }

        /* Check for IPv4 synatx, 134.138.176.6:<port>:<port> */
        *addr = string;
        addr_end = strstr(string, ":");
        if ((addr_start == NULL) && addr_end) {
                /* End the string before the ports */
                addr_end[0] = '\0';
        }
        /* Don't allow any colon in address */
        ret = strstr(*addr, ":") == NULL ? 0 : 1;
        /* Address shall include dot */
        return -(ret || (strstr(*addr, ".") == NULL ? 1 : 0));
}

/* ===================================================================== */
/**
 *   Gets the IP family from the provided string.
 *
 *   @param               addr          IP/hostname
 *
 *   @param               inet_family   IP address family.
 *
 *   @return              0 on successful execution, or error code on error.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int get_addr_family(const char *addr, int *inet_family)
{
        struct addrinfo hint, *info = NULL;
        int ret;

        if (strncmp(addr, "localhost", 9) == 0) {
                *inet_family = AF_INET;
        } else {
                memset(&hint, 0, sizeof(hint));
                hint.ai_family = AF_UNSPEC;

                ret = getaddrinfo(addr, 0, &hint, &info);
                if (info == NULL) {
                   return ret;
                }
                if (ret) {
                   freeaddrinfo(info);
                   return ret;
                }

                *inet_family = info->ai_family;
                freeaddrinfo(info);
        }
        return 0;
}

/* ===================================================================== */
/**
 *   Formats an URL for the provided string and IP address family.
 *
 *   @param               string           String with IP address and
 *                                         optional LTTng ports.
 *
 *   @param               inet_family      IP address family.
 *
 *   @param               formatted_url    The URL for the address family.
 *
 *   @param               max_formatted_url_len  MAx length of the formatted
 *                                               URL.
 *
 *   @return              0 on successful execution, or  -1 on error.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int format_url(const char *string,
                      int inet_family,
                      char *formatted_url,
                      int max_formatted_ur_len)
{
        if (inet_family == AF_INET) {
                snprintf(formatted_url, max_formatted_ur_len,
                         "net://%s", string);
        }
        else if (inet_family == AF_INET6) {
                snprintf(formatted_url, max_formatted_ur_len,
                         "net6://%s", string);
        }
        else {
                formatted_url = NULL;
                return -1;
        }
        return 0;
}

/* ===================================================================== */
/**
 *   Creates an URL for the specified IP address and LTTng ports.
 *
 *   @param               string        String to parse.
 *
 *   @param               formatted_url The formatted URL.
 *
 *   @param               formatted_url_len  Max length of the formatted
 *                                           string.
 *
 *   @param               err_info      Error information, at failure.
 *
 *   @param               err_info_len  Max length of error information.
 *
 *   @return              0 on successful execution, or  -1 on error.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static int create_formatted_url(const char *string,
                                char *formatted_url,
                                int formatted_url_len,
                                char *err_info,
                                int err_info_len)
{
        int ret, inet_family = -1;
        char *addr, *string_start;

        ret = asprintf(&string_start, string);
        if (ret == -1) {
                  snprintf(err_info, err_info_len,
                           "Memory allocation failure");
                  return -1;
        }

        ret = parse_addr(string_start, &addr);
        if (ret < 0) {
                snprintf(err_info, err_info_len,
                         "No valid address syntax: %s", string_start);
                free(string_start);
                return -1;
        }

        ret = get_addr_family(addr, &inet_family);
        if (ret != 0) {
                snprintf(err_info, err_info_len,
                         "Failed to determine IP address family addr: %s ret=%d",
                         addr, ret);
                free(string_start);
                return -1;
        }

        /* String is ok to use, format the URL. */
        ret = format_url(string, inet_family,
                         formatted_url, formatted_url_len);
        if (ret < 0) {
                snprintf(err_info, err_info_len,
                         "No valid IP address family: %d (%s)",
                         inet_family, addr);
                free(string_start);
                return -1;
        }

        free(string_start);
        return ret;
}

/* ========================================================================= */
/**
 * Create a new streaming session.
 *
 * @param session         - session name to use
 * @param ip              - ip string of observer
 * @param subbuf_size     - size in bytes of subbuffer
 *                          if 0 is given default value is used
 * @param no of subbuf    - number of subbuffers, (must be multiple of 2)
 *                          if 0 is given default value 1 is used
 * @param switch_interval - switch timer interval in usec
 *                          if 0 is given default value is used
 * @param flush_interval -  data flush timer interval in usec
 *                          if 0 is given default value 1 sec is used
 *
 * @return command result
 */
/* ========================================================================= */
int
ts_ted_session_create(const Session *session, const char *ip,
                      uint64_t subbuf_size, uint64_t no_of_subbuf,
                      unsigned int switch_interval, unsigned int flush_interval)
{
   union itc_msg *msg;
   uint32_t createRsp[] = {1, TED_CTRL_RSP};
   int size;
   char formatted_url[MAX_URL_LEN];
   char err_info[MAX_ERR_INFO_LEN];
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   ret = create_formatted_url(ip,
                              formatted_url, MAX_URL_LEN,
                              err_info, MAX_ERR_INFO_LEN);
   if (ret < 0) {
           ERR("%s", err_info);
           return CMD_ERROR;
   }

   size = snprintf(NULL, 0, "%s %llu %llu %u %u",
                   formatted_url,
                   (unsigned long long)subbuf_size,
                   (unsigned long long)no_of_subbuf,
                   switch_interval, flush_interval)+1;
   /* + 1 for NULL termination.*/
   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size,
                   TED_CTRL_REQ);
   snprintf(msg->ctrlReq.data, size, "%s %llu %llu %u %u",
            formatted_url,
            (unsigned long long)subbuf_size,
            (unsigned long long)no_of_subbuf,
            switch_interval, flush_interval);
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name), "%s", session->name);
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   msg->ctrlReq.type = TE_SESSION_CREATE;
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(createRsp, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Failed to receive response for create request.");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      MSG("%s", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else {
      MSG("%s", msg->ctrlRsp.data);
      ret = CMD_SUCCESS;
   }

   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 * Delete a streaming session.
 *
 * @param session - the session to delete
 *
 * @return command result
 */
/* ========================================================================= */
int
ts_ted_session_delete(const Session *session)
{
   union itc_msg *msg;
   uint32_t filter[] = {2, TED_CTRL_RSP, TED_STATUS_RSP};
   int sessionCount = 0;
   int ret = CMD_SUCCESS;
   bool isFirstLine = true;
   bool isFirstSignal = true;
   bool isDone = false;
   char line[10], ch1;
   bool isAtLeastOneSession = false;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   if (strcmp(session->name, "*") == 0)
   {
      /* No session or '*' was specified so send a status request
       * to print all the sessions data and get user acknowledgement
       * to delete all sessions.
       */
      msg = itc_alloc(sizeof(struct ted_status_req), TED_STATUS_REQ);
      msg->statusReq.handler = CMD_TS_HANDLER;
      snprintf(msg->statusReq.session_name,
               sizeof(msg->statusReq.session_name),
               "%s", session->name);
      itc_send(&msg, ted_mb, ITC_MY_MBOX);

      while (!isDone) {

         msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
         if (msg == NULL) {
            ERR("Command timeout");
            return CMD_ERROR;
         }

         switch (msg->msg_no) {
         case TED_STATUS_RSP:
            if (!msg->statusRsp.success) {
               ERR("Status request failed (%d)", msg->statusRsp.success);
               return CMD_FAILED;
            }
            if (msg->statusRsp.data_size.no_of_events && isFirstSignal) {
               isAtLeastOneSession = true;
               MSG("Lttng Live sessions:");
            }
            if (sessionCount != msg->statusRsp.session_count) {
               sessionCount = msg->statusRsp.session_count;
            }
            if (isFirstLine) {
               printSessionStatus(msg);
            }

            isFirstLine = false;
            isFirstSignal = false;
            if (msg->statusRsp.last) {
               isDone = true;
            }

            break;
         default:
            WARN("Unknown msg received");
            itc_free(&msg);
            return CMD_ERROR;
         }
         itc_free(&msg);
      }
      if (isAtLeastOneSession) {
         fprintf(stdout, "Do you want to remove all the sessions specified. "
                 "Please enter y or n ? ");
         fflush(stdout);
         (void)fgets(line, 10, stdin);
         ch1 = line[0];
         if (!((ch1 == 'y') || (ch1 == 'Y'))) {
            return CMD_SUCCESS;
         }
      }
      else {
         MSG("No session found");
         return CMD_SUCCESS;
      }
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
   msg->ctrlReq.type = TE_SESSION_DELETE;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   msg->ctrlReq.session_id = session->id;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name), "%s", session->name);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->msg_no == TED_CTRL_RSP) {
      if (msg->ctrlRsp.result != CMD_RESULT_OK) {
         MSG("Command failed (%s)", msg->ctrlRsp.data);
         ret = CMD_ERROR;
      }
   }
   itc_free(&msg);
   return ret;

}


/* ========================================================================= */
/**
 * See definition of TsTraceHandler.
 */
/* ========================================================================= */
static int
ts_ted_default(const Session *session,
               const char *provider,
	       Scope scope)
{
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
   msg->ctrlReq.type = TE_DEFAULT;
   msg->ctrlReq.session_id = session->id;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name),
            "%s", session->name);
   msg->ctrlReq.scope = get_scope(scope);

   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout.");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Command failed (%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_SESSION_NOT_FOUND) {
      ERR("Not a live session registered by ts");
   }
   else if (msg->ctrlRsp.result == CMD_EVENT_NOT_FOUND) {
      MSG("Specified provider not found");
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 * See definition of TsTraceHandler.
 */
/* ========================================================================= */
static int
ts_ted_enable(const Session *session,
              const char *provider,
              int nEvents, const char *events[])
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};
   int size = 0;
   char *eventFormat = NULL;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   /* Group the lttng events to be sent to TED. */
   size = ted_groupLttngEvents(nEvents, events, &eventFormat);
   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size, TED_CTRL_REQ);
   msg->ctrlReq.type = TE_ENABLE;
   msg->ctrlReq.size = nEvents + 1;
   msg->ctrlReq.session_id = session->id;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name), "%s", session->name);
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   snprintf(msg->ctrlReq.data, size + 1, "%s", eventFormat);
   free(eventFormat);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Failed to enable requested event (%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_SESSION_NOT_FOUND) {
      ERR("Not a live session registered by ts");
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_EVENT_NOT_FOUND) {
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 * See definition of TsTraceHandler.
 */
/* ========================================================================= */
static int
ts_ted_disable(const Session *session,
               const char *provider,
               int nEvents, const char *events[])
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};
   int size = 0;
   char *eventFormat = NULL;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   /*
    * Group the lttng events to be sent to TED.
    */
   size = ted_groupLttngEvents(nEvents, events, &eventFormat);
   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size, TED_CTRL_REQ);
   msg->ctrlReq.type = TE_DISABLE;
   msg->ctrlReq.size = nEvents + 1;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   msg->ctrlReq.session_id = session->id;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name), "%s",
            session->name);
   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s",
            provider);
   snprintf(msg->ctrlReq.data, size + 1, "%s", eventFormat);
   free(eventFormat);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Failed to disable requested event(%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_EVENT_NOT_FOUND) {
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }
   itc_free(&msg);
   return ret;
}



/* ========================================================================= */
/**
 * See definition of TsTraceHandler.
 */
/* ========================================================================= */
static int
ts_ted_status(const Session *session,
              const char *provider,
              bool *isSessionFound)
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_STATUS_RSP};
   bool isDone = false;
   bool isFirstSignal = true;
   bool isFirstLine = true;
   int sessionCount = 0;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_status_req), TED_STATUS_REQ);
   msg->statusReq.handler = CMD_TS_HANDLER;
   msg->statusReq.session_id = session->id;
   snprintf(msg->statusReq.session_name, sizeof(msg->statusReq.session_name),
            "%s", session->name);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);


   while (!isDone) {
      msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
      if (msg == NULL) {
         ERR("Command timeout");
         return CMD_ERROR;
      }

      if (!msg->statusRsp.success) {
         ERR("Status request failed (%d)", msg->statusRsp.success);
         return CMD_FAILED;
      }
      if (msg->statusRsp.data_size.no_of_events && isFirstSignal) {
         *isSessionFound = true;
         MSG("Lttng Live sessions:");
      }
      if (sessionCount != msg->statusRsp.session_count) {
         isFirstLine = 1;
         sessionCount = msg->statusRsp.session_count;
      }
      printStatus(msg, isFirstLine);
      isFirstLine = false;
      isFirstSignal = false;

      if (msg->statusRsp.last) {
         isDone = true;
      }

      itc_free(&msg);
   }
   if (!(*isSessionFound)) {
      MSG("Invalid Session/No Sessions Found");
   }

   return ret;
}


/* ========================================================================= */
/**
 *   See definiton of TsTraceHandler.
 */
/* ========================================================================= */
static int
ts_ted_save(const Session *session, const char *provider)
{
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
   msg->ctrlReq.type = TE_SAVE;
   msg->ctrlReq.session_id = session->id;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name),
            "%s", session->name);
   snprintf(msg->ctrlReq.provider,
            sizeof(msg->ctrlReq.provider), "%s", provider);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout.");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Command failed (%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_PRESET_OVERFLOW) {
      ERR("Too many trace events are saved.");
   }
   else if (msg->ctrlRsp.result == CMD_EVENT_NOT_FOUND) {
      if (strcmp(session->name, "*") != 0) {
         ERR("Session not found");
      }
   }
   else if (msg->ctrlRsp.result == CMD_SESSION_OVERFLOW) {
      MSG("Saved session limit reached");
      MSG("Maximum sessions that can be saved: %d", msg->ctrlRsp.extra);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_SESSION_NOT_FOUND) {
      MSG("No session available to save");
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 * See definition of TsTraceHandler.
 */
/* ========================================================================= */
static int
ts_ted_filter(const Session *session,
              int type, const char *traceFilter, const char *event)

{
   int ret = CMD_SUCCESS;
   int size;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   size = strlen(traceFilter) + strlen(event) + 2;

   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size, TED_CTRL_REQ);
   msg->ctrlReq.type = type == 0 ? TE_FILTER_SET : TE_FILTER_RESET;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name),
            "%s", session->name);
   msg->ctrlReq.session_id = session->id;
   if (type == 0) {
      strcpy(msg->ctrlReq.data, traceFilter);
      /* FIXME why +2 here and not +1 */
      strcpy(&msg->ctrlReq.data[strlen(traceFilter) + 2], event);
      msg->ctrlReq.offset = strlen(traceFilter) + 2;
   }
   else {
      strcpy(msg->ctrlReq.data, event);
   }
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }
   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Failed to set/reset filter (%d)", msg->ctrlRsp.result);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

   itc_free(&msg);
   return ret;
}


/* ========================================================================= */
/**
 * Send the session request to the TED server.
 *
 * @return command result
 */
/* ========================================================================= */
int
ts_ted_restart(unsigned int force)
{
   uint32_t filter[] = {1, TED_CTRL_RSP};
   union itc_msg *msg;
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
   msg->ctrlReq.type = TE_RESTART;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   snprintf(msg->ctrlReq.data, 2, "%d", force);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout.");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Command failed (%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   } else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

   itc_free(&msg);

   return ret;
}


/* ========================================================================= */
/**
 * Send set correlate message
 *
 * @return command result
 */
/* ========================================================================= */
int
ts_ted_set_correlate(unsigned int val)
{
   uint32_t filter[] = {1, TED_CTRL_RSP};
   union itc_msg *msg;
   int ret = CMD_SUCCESS;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req), TED_CTRL_REQ);
   msg->ctrlReq.type = TE_SET_CORRELATE;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   snprintf(msg->ctrlReq.data, 2, "%d", val);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);

   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout.");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Command failed (%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   } else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   }

   itc_free(&msg);

   return ret;
}


/* ========================================================================= */
/**
 * Write a string into a session.
 *
 * @param session - the session to write to
 * @param data - the message string
 *
 * @return command result
 */
/* ========================================================================= */
int
ts_ted_echo(const Session *session, const char *data)
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};
   int size;

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   size = strlen(data);

   msg = itc_alloc(sizeof(struct ted_ctrl_req) + size + 1,
                   TED_CTRL_REQ);
   msg->ctrlReq.type = TE_ECHO;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   msg->ctrlReq.session_id = session->id;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name), "%s",
            session->name);
   snprintf(msg->ctrlReq.data, size + 1, "%s", data);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);


   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Command failed (%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_SESSION_NOT_FOUND) {
      ERR("Not a live session registered by ts");
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   } else {
      ret = CMD_SUCCESS;
   }

   itc_free(&msg);

   return ret;
}

/* ========================================================================= */
/**
 * Write a string into a session.
 *
 * @param session - the session to write to
 *
 * @return command result
 */
/* ========================================================================= */
int
ts_ted_session_validate(const Session *session)
{
   int ret = CMD_SUCCESS;
   union itc_msg *msg;
   uint32_t filter[] = {1, TED_CTRL_RSP};

   if (locateServer() < 0) {
      return CMD_ERROR;
   }

   msg = itc_alloc(sizeof(struct ted_ctrl_req),
                   TED_CTRL_REQ);
   msg->ctrlReq.type = TE_SESSION_VALIDATE;
   msg->ctrlReq.handler = CMD_TS_HANDLER;
   msg->ctrlReq.session_id = session->id;
   snprintf(msg->ctrlReq.session_name,
            sizeof(msg->ctrlReq.session_name),
            "%s", session->name);
   itc_send(&msg, ted_mb, ITC_MY_MBOX);


   msg = itc_receive(filter, RECEIVE_TMO, ITC_FROM_ALL);
   if (msg == NULL) {
      ERR("Command timeout");
      return CMD_ERROR;
   }

   if (msg->ctrlRsp.result == CMD_EVENT_ACTION_FAILED) {
      ERR("Command failed (%s)", msg->ctrlRsp.data);
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result == CMD_SESSION_NOT_FOUND) {
      ERR("Invalid Session/No Sessions Found");
      ret = CMD_ERROR;
   }
   else if (msg->ctrlRsp.result != CMD_RESULT_OK) {
      ret = CMD_ERROR;
   } else {
      ret = CMD_SUCCESS;
   }

   itc_free(&msg);

   return ret;
}


static TsTraceHandler tedHandler = {
   "TED",
   TED_HANDLER,
   ts_ted_status,
   ts_ted_enable,
   ts_ted_disable,
   ts_ted_default,
   ts_ted_save,
   ts_ted_filter
};


/* ========================================================================= */
/**
 *   Register the TED handler as a command handler to the 'ts' command.
 */
/* ========================================================================= */
static __attribute__((constructor)) void
registerTedHandler(void)
{
   ts_registerHandler(&tedHandler);
}


/* ========================================================================= */
/**
 *   Returns the TED handler which is the mandatory command handler.
 */
/* ========================================================================= */
TsTraceHandler*
getTsTedHandler(void)
{
   return &tedHandler;
}

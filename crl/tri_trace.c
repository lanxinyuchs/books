/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <syslog.h>
#include <errno.h>
#include "tri_proxy.h"
#include "tri_daemon.h"
#include "itc.h"
#include "cello_te_handlers.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t                      msgNo;
   TriDaemonItemMonitorInd       triDaemonItemMonitorInd;
};

struct threadCleanupData
{
  struct ItemInfo *mallocRef;
  itc_mbox_id_t daemonMbox;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Key for the thread-specific clean-up */
static pthread_key_t tri_cleanup_key = 0;

/* Multi tridaemon thread protection */
static int tri_dmn_run = 0;
static pthread_mutex_t tri_dmn_lock;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   This function is run when a thread exit. It will do the needed
 *   cleanup.
 *
 *   @param param      Pointer to structure with thread specific data.
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
static void
handleItemThreadDestroy(void* param)
{
   itc_mbox_id_t mbox;
   itc_mbox_id_t daemonMbox = ITC_NO_ID;
   int is_lits_mb = 0;
   struct ItemInfo *mallocRef;
   union itc_msg *msg;

   /* Create an ITC mailbox */
   if ((mbox = itc_current_mbox()) == ITC_NO_ID) {
       mbox = itc_create_mailbox(TRI_PROXY_MAILBOX_NAME, 0);
   } else {
      is_lits_mb = 1;
   }

   mallocRef = ((struct threadCleanupData *)param)->mallocRef;
   daemonMbox = ((struct threadCleanupData *)param)->daemonMbox;

   /* Send indication to TRI server */
   msg = itc_alloc(sizeof(TriDaemonItemMonitorInd),
                   TRI_DAEMON_ITEM_MONITOR_IND);
   msg->triDaemonItemMonitorInd.mallocRef = mallocRef;
   itc_send(&msg, daemonMbox, mbox);

   if (!is_lits_mb) {
      itc_delete_mailbox(mbox);
   }

   if (param) {
      free(param);
   }

   pthread_setspecific(tri_cleanup_key, NULL);
}

/* ===================================================================== */
/**
 *   Create a thread-specific data key.
 *
 *   @param -
 *
 *   @return           -
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static void tri_create_key(void)
{
  if (pthread_key_create(&tri_cleanup_key, &handleItemThreadDestroy)) {
    syslog(LOG_ERR, "pthread_key_create failed, errno=%d.", errno);
  }
}

/* ===================================================================== */
/**
 *   Get scheduling policy and priority from environments variables.
 *
 *   @param policy      Out: scheduling policy.
 *   @param priority    Out: scheduling priority.
 *
 *   @return            0 in case of success, 1 in case of failure.
 *
 *   @par Globals:      --
 *
 *   @note Default to SCHED_OTHER if no policy given.
 *         SCHED_OTHER always has priority 0 (zero).
 *         Default to priority 15 if no priority given for any other policy.
 */
/* ===================================================================== */
static int getschedparams(int *policy, int *priority)
{
   char *env_policy = NULL;
   char *env_prio = NULL;

   *priority = 0; /* SCHED_OTHER always has priority 0 (zero). */

   env_policy = getenv("CRL_SCHED_POLICY");
   if (env_policy) {
      if (strcasecmp(env_policy, "FIFO") == 0) {
         *policy = SCHED_FIFO;
      } else if (strcasecmp(env_policy, "RR") == 0) {
         *policy = SCHED_RR;
      } else if (strcasecmp(env_policy, "OTHER") == 0) {
         *policy = SCHED_OTHER;
         return 0;
      } else {
         *policy = SCHED_OTHER;
         return 1; /* Invalid policy. */
      }
   } else {
      *policy = SCHED_OTHER;
      return 0;
   }

   env_prio = getenv("CRL_SCHED_PRIO");
   if (env_prio) {
      errno = 0;
      *priority = (int)strtol(env_prio, NULL, 10);
      if (errno != 0) {
         return 1;
      }
      if ((*priority < 0) || (*priority > 99)) {
         return 1; /* Invalid priority. */
      }
   } else {
      *priority = 15;
   }

   return 0;
}

/* ===================================================================== */
/**
 *   Spawn the TRI daemon thread with correct environment. TRI daemon thread
 *   shall run as one instance for each application.
 *
 *   @param   -
 *
 *   @return -
 *
 */
/* ===================================================================== */
static void spawnTriDaemon(void)
{
   pthread_t tid;
   pthread_attr_t attr;
   int policy;
   struct sched_param sched_parameters;
   int ret;
   extern void *triDaemon();

   if (tri_dmn_run)
      return;

   tri_create_key();

   if (pthread_attr_init(&attr) != 0) {
      abort();
   }
   /* Get environment variables. */
   if (getschedparams(&policy, &sched_parameters.sched_priority) != 0) {
      syslog(LOG_ERR, "createTriDaemon: Invalid environment variable values");
      abort();
   }

   /* Set scheduling policy and priority. */
   if ((pthread_attr_setschedpolicy(&attr, policy) != 0) ||
       (pthread_attr_setschedparam(&attr, &sched_parameters) != 0) ||
       (pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED) != 0)) {
         syslog(LOG_ERR,
                "createTriDaemon: Failed to set sched: policy %d, prio %d.",
                policy, sched_parameters.sched_priority);
         abort();
   }

   ret = pthread_create(&tid, &attr, triDaemon, NULL);
   if (ret != 0) {
      syslog(LOG_ERR,
             "createTriDaemon: Failed to create TRI daemon, ret=%d", ret);
      abort();
   } else {
      tri_dmn_run = 1;
   }

   (void)pthread_attr_destroy(&attr);

   return;
}

/* ===================================================================== */
/**
 *   Constructor fot the TRI library.
 *
 *   @param   -
 *
 *   @return -
 *
 */
/* ===================================================================== */
static void __attribute__ ((constructor)) tri_lib_init(void)
{
   int ret;

   /* Open syslog for logging */
   openlog("TRI_TRACE", 0, LOG_DAEMON);

   /*
    * Need to have mutex lock if several tri_init calls from same process.
    * The first call will cretae deamon and set tri_dmn_run flag to avoid
    * several creations.
    */
   if ((ret = pthread_mutex_init(&tri_dmn_lock, NULL)) != 0)
   {
      syslog(LOG_ERR, "Mutex init failed, ret=%d\n", ret);
      closelog();
      abort();
   }
}

/* ===================================================================== */
/**
 *   Allocated thread specific key and assign a cleanup function
 *   which shall do the necessary cleanup if thread disappear.
 *
 *
 *   @param      Pointer to structure with thread specific data.
 *
 *   @return           TRI_OK at success otherwise TRI_INTERNAL_ERROR.
 *
 *   @par Globals:
 *                tri_cleanup_key
 */
/* ===================================================================== */
int triAllocAndSetThreadCleanupData(struct ItemInfo *procInfo,
                                    itc_mbox_id_t mbox)
{
   struct threadCleanupData *threadCleanupData;

   threadCleanupData = (struct threadCleanupData*)malloc(sizeof(struct threadCleanupData));
   threadCleanupData->mallocRef = procInfo;
   threadCleanupData->daemonMbox = mbox;

   if (pthread_setspecific(tri_cleanup_key, threadCleanupData))
   {
      syslog(LOG_ERR, "TRI proxy %s %s: %p pthread_setspecific failed, errno=%d.",
             __FUNCTION__, procInfo->procName, threadCleanupData, errno);
      return TRI_INTERNAL_ERROR;
   }
   return TRI_OK;
}

/* ===================================================================== */
/**
 *   Initiates the thread handling for TRI with no LITS dependency.
 *
 *   @param   -
 *
 *   @return TRI_OK                  Success.
 *           TRI_INTERNAL_ERROR      At failure.
 *
 *   This is a mandatory function to be the first TRI function to call.
 */
/* ===================================================================== */
int tri_init(void)
{
   /* Open syslog for logging */
   openlog("TRI_TRACE", 0, LOG_DAEMON);

   pthread_mutex_lock(&tri_dmn_lock);

   spawnTriDaemon();

   pthread_mutex_unlock(&tri_dmn_lock);

   return TRI_OK;
}

/* ===================================================================== */
/**
 *   OSE Start hook to be used by LITS depenedent clients to start the
 *   TRI daemon.
 *
 *   @param   -
 *
 *   @return -
 *
 */
/* ===================================================================== */
void createTriDaemon(void)
{
   (void)tri_init();

   return;
}

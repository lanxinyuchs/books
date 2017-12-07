/**
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-05-29 Ramakrushna Mishra
 *   Change  : First version.
 *
 * ========================================================================
 */


/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <sys/timerfd.h>
#include <sys/select.h>
#include <sys/time.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "unistd.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <itc.h>

#include "com_ericsson_glms.h"

#include "glmsUtils.h"
#include "glms_main.h"
#include "glms_timi.h"
#include "glms_internal.sig"


/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

static pthread_t timer_thread;
static uint32_t timer_thread_created = 0;
uint32_t nextTimerId = 1;

/* wakeUpPipe and timerList is used by both main thread and timer thread */
int wakeUpPipe[2] = {0,0};
struct Timer *timerList  = NULL;

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
union itc_msg
{
   uint32_t  sigNo;

   GlmsTimerExpiredInd       glmsTimerExpiredInd;
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

void *timerfd_handler_thread(void *arg);

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
wakeUpHandler()
{
   /* Wake up timer handler proc */
   char wakeUpStr[2] = "w";
   ssize_t writtenData;
   errno = 0;
   if((writtenData = write(wakeUpPipe[1], wakeUpStr, 1)) != 1)
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "Failed to write to wakeUpPipe. writtenData = %jd, errno = %d. %s",
                    (intmax_t)writtenData,
                    errno,
                    strerror(errno));
   }
}

GlmsBool
deleteTimer(uint32_t timerId)
{
   struct Timer *currTimer, *prevTimer;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "deleteTimer", timerId);

   prevTimer = NULL;

   for(currTimer = timerList;
       currTimer != NULL;
       prevTimer = currTimer, currTimer = currTimer->nextTimer)
   {
      if(currTimer->timerId == timerId)
      {
         if(currTimer == timerList)
         {
            timerList = currTimer->nextTimer;
         }

         if(prevTimer != NULL)
         {
            prevTimer->nextTimer = currTimer->nextTimer;
         }

	 /*  Stop the timer */
	 currTimer->timerLength.it_value.tv_sec   = 0;
         currTimer->timerLength.it_value.tv_nsec  = 0;
         currTimer->timerLength.it_interval.tv_nsec  = 0;
         currTimer->timerLength.it_interval.tv_sec   = 0;

	 if(timerfd_settime(currTimer->fd,
                            0,
                            &currTimer->timerLength,
                            NULL) == -1)
         {
            tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                       "Failed to call timerfd_settime to delete timer", errno);
            return(GLMS_FALSE);
         }

         free(currTimer);
	 wakeUpHandler();
	 break; /* We found the timer so we can break the loop */
      }
   }

   if(currTimer != NULL)
      return(GLMS_TRUE);

   return(GLMS_FALSE);
}



void
deleteAllTimers()
{
   struct Timer *currTimer, *nextTimer;

   tracepoint(com_ericsson_glms, call_to_function,
              "deleteAllTimers");

   nextTimer = NULL;

   for(currTimer = timerList;
       currTimer != NULL;
       currTimer = nextTimer)
   {
      nextTimer = currTimer->nextTimer;

      /*  Stop the timer */
      currTimer->timerLength.it_value.tv_sec   = 0;
      currTimer->timerLength.it_value.tv_nsec  = 0;
      currTimer->timerLength.it_interval.tv_nsec  = 0;
      currTimer->timerLength.it_interval.tv_sec   = 0;

      if(timerfd_settime(currTimer->fd,
                         0,
                         &currTimer->timerLength,
                         NULL) == -1)
      {
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Error: Failed to call timerfd_settime when deleting"
                    " all timers. Errno %d\n", errno);
      }

      free(currTimer);
   }

   timerList = NULL;
}


GlmsBool
isTimerActive(uint32_t clientId)
{
   struct Timer *timer;
   timer = timerList;

   while(timer != NULL)
   {
      if(timer->clientId == clientId)
         return(GLMS_TRUE);

      timer = timer->nextTimer;
   }

   return(GLMS_FALSE);
}

uint32_t
requestTimer(time_t32 timerLength, GlmsBool periodic, uint32_t clientId)
{
   struct Timer *timer, *tmpTimerList;
   struct timespec currentTime;

   clock_gettime(CLOCK_REALTIME, &currentTime);

   /* Create new timer */
   timer = (struct Timer *) malloc(sizeof(struct Timer));

   timer->timerId     = nextTimerId++;
   timer->clientId    = clientId;
   timer->clientMid   = itc_current_mbox();
   timer->nextTimer   = NULL;
   timer->periodic    = periodic;

   tracepoint(com_ericsson_glms, requestTimer,
              timerLength, periodic, clientId, timer->timerId);

#ifdef GLMS_TEST_RUN
   timer->timerLength.it_value.tv_sec   = timerLength + currentTime.tv_sec;
#else
   timer->timerLength.it_value.tv_sec   = timerLength;
#endif
   timer->timerLength.it_value.tv_nsec  = 0;
   timer->timerLength.it_interval.tv_nsec  = 0;
   if(periodic)
      timer->timerLength.it_interval.tv_sec   = timerLength;
   else
      timer->timerLength.it_interval.tv_sec   = 0;

   /* For testing we use realtime clock since we can manipulate those timers.
      For periodic timers it is however not possible to manipulate. The behaviour
      will be as if the timer is a monotonic timer. For those situations we
      will send a timer indication to GLMS from the test framework. Only
      the 24h daily validation timer is a periodic timer and that timer is
      not expected to interfere with the component tests. */
#ifdef GLMS_TEST_RUN
   timer->fd          = timerfd_create(CLOCK_REALTIME, 0);
#else
   timer->fd          = timerfd_create(CLOCK_MONOTONIC, 0);
#endif

   if(timer->fd == -1)
   {
      free(timer);
      return -1;
   }

   /* Start the timer */
   if(timerfd_settime(timer->fd,
#ifdef GLMS_TEST_RUN
                      TFD_TIMER_ABSTIME,
#else
                      0,
#endif
                      &timer->timerLength,
                      NULL) == -1)
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Failed to call timerfd_settime", errno);
      free(timer);
      return -1;
   }

   /* Add timer to timer list */
   if(timerList == NULL)
   {
      timerList = timer;
   }
   else
   {
      tmpTimerList = timerList;
      while(tmpTimerList->nextTimer != NULL)
      {
         tmpTimerList = tmpTimerList->nextTimer;
      }
      tmpTimerList->nextTimer = timer;
   }

   /* Wake up timer handler proc */
   wakeUpHandler();

   return timer->timerId;
}


void
terminateTimerThread()
{
   if(timer_thread_created != 0)
   {
      pthread_cancel(timer_thread);
      timer_thread_created = 0;

      if(wakeUpPipe[0] != 0)
      {
         close(wakeUpPipe[0]);
         wakeUpPipe[0] = 0;
      }

      if(wakeUpPipe[1] != 0)
      {
         close(wakeUpPipe[1]);
         wakeUpPipe[1] = 0;
      }
   }
}

void
initTimers()
{
   itc_mbox_id_t timerHandlerMid;

   tracepoint(com_ericsson_glms, call_to_function,
              "initTimers");

   timerHandlerMid = itc_locate("glms_timerfd_handler");
   if(timerHandlerMid != ITC_NO_ID)
   {
      terminateTimerThread();
   }

   if(pipe(wakeUpPipe) == -1)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Failed to open timi pipe");
      return;
   }

   pthread_create(&timer_thread, NULL, timerfd_handler_thread, NULL);
   timer_thread_created = 1;
}



void *timerfd_handler_thread(void *arg)
{
   struct Timer *timer, *nextTimer;
   struct Timer timercpy;
   union itc_msg *sig;
   int max_fd, select_status, oldval;
   fd_set timerSet;
   unsigned long long missed_timers;
   char wakeUpBuf[1];
   itc_mbox_id_t ownMid;

   (void)arg;

   ownMid = itc_create_mailbox("glms_timerfd_handler", 0);

   tracepoint(com_ericsson_glms, debug_trace_w_hex_arg,
              "timerfd_handler_thread created with mailbox id", ownMid);

   (void)pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &oldval);
   (void)pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &oldval);

   while(1)
   {
      /* timerSet attribute is used both as input and output in select
         so we need to set the file descriptors every time we call select. */
      FD_ZERO(&timerSet);
      FD_SET(wakeUpPipe[0], &timerSet); /* Add the wake up FD */
      max_fd = wakeUpPipe[0] + 1;
      timer = timerList;
      while(timer != NULL)
      {
         FD_SET(timer->fd, &timerSet);

         if(timer->fd >= max_fd)
            max_fd = timer->fd + 1;

         timer = timer->nextTimer;
      }

      select_status = select(max_fd, &timerSet, NULL, NULL, NULL);
      if(select_status == -1)
      {
         /* select return -1 for error, 0 if timeout or a positive number
            with the number of fds with changed state*/
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Select failed", errno);
      }
      else if(select_status == 0)
      {
         tracepoint(com_ericsson_glms, error_trace,
                    "Select timeout");
      }
      else /* Read the FD's to see which timer has triggered */
      {
         if(FD_ISSET(wakeUpPipe[0], &timerSet))
         {
            /* Block the FD */
            read(wakeUpPipe[0], wakeUpBuf, 1);
         }

         timer = timerList;
         while(timer != NULL)
         {
            /* We get the next timer here because deleteTimer() may
               delete the current timer. */
            nextTimer = timer->nextTimer;

            if(FD_ISSET(timer->fd, &timerSet))
            {
               /* When the alarm triggers a number will be written to the file descriptor.
                  This number needs to be read from the file descriptor otherwise select
                  will trigger immediately again. */
               /* missed_timers contains the number of times the timer has triggered since the
                  file descriptor was read. */
               read(timer->fd, &missed_timers, sizeof(missed_timers));

               timercpy = *timer;
               if(!(timer->periodic))
               {
                  deleteTimer(timer->timerId);
               }

               sig = itc_alloc(sizeof(GlmsTimerExpiredInd), GLMS_TIMER_EXPIRED_IND);
               sig->glmsTimerExpiredInd.clientId  = timercpy.clientId;
               sig->glmsTimerExpiredInd.timerId   = timercpy.timerId;
               itc_send(&sig, timercpy.clientMid, ITC_MY_MBOX);
            }

            timer = nextTimer;
         }
      }
   }
}

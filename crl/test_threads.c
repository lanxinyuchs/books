/**
 *   Thread bodies and utility functions for test.

 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <errno.h>
#include <cello_te_ose.h>
#include <cello_te_trace.h>
#include <cello_te_trace_obj.h>

#define MAX_NAME_LEN 16


IMPORT_TRACE_OBJ(LTTNG_TRI_OBJ);

/* ===================================================================== */
/**
 *   Sets a thread name and register the name to TRI.
 *
 *   @param      name
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void set_and_reg_thread(char *thread_name)
{
   char name[MAX_NAME_LEN];
   int ret = TRI_OK;

   snprintf(name, MAX_NAME_LEN, "%s", thread_name);

   if ((ret = prctl(PR_SET_NAME, (unsigned long)name, 0, 0, 0)) != 0)
   {
      syslog(LOG_INFO, "prctl failed to set name=%s ret=%d",
             thread_name, ret);
   }
   tri_register_thread_name(thread_name);

   return;
}

/* ===================================================================== */
/**
 *   Thread used for testing that multiple tri_init doesn't cause two
 *   TRI daemons are spawn for this process.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void *lttng2_tri_test_2(void *arg)
{
   int ret = TRI_OK;

   if ((ret = tri_init()) != TRI_OK) {
      syslog(LOG_ERR, "%s: tri_init failed for lttng2_tri_test_2, ret=%d",
             __func__, ret);
      pthread_exit(&ret);
   }
   INFO("lttng2_tri_test_2 called tri_init");

   set_and_reg_thread("lttng2_tri_test_2");

   INFO(STR("lttng2_tri_test_2 started"));

   while (1)
   {
      sleep(5);
   }
   exit(0);
}

/* ===================================================================== */
/**
 *   Thread used to test registration and deregistration.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void *lttng2_tri_kill_test(void *arg)
{
   set_and_reg_thread("lttng2_tri_kill_test");

   syslog(LOG_INFO, "lttng2_tri_kill_test go to sleep");
   sleep(1);
   syslog(LOG_INFO, "lttng2_tri_kill_test going for pthread_exit");

   pthread_exit(NULL);
}

/* ===================================================================== */
/**
 *   Generate high rate traces via TRI from object context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                smallData, bigData
 */
/* ===================================================================== */
void *high_traceA(void *arg)
{
   int ret;

   if ((ret = prctl(PR_SET_NAME, (unsigned long)arg, 0, 0, 0)) != 0) {
      printf("Failed to set thread name %s, ret=%d errno=%d",
             (char*)arg, ret, errno);
      abort();
   }

   printf("Start high tracing process: thread=%s id=%u, pid=%u\n",
          (char*)arg, (unsigned int)pthread_self(), getpid());

   while(1) {
      INFO(STR("Thread1 current tid:%d", (unsigned int)pthread_self()));
      TRACE_ERROR(STR("Thread1 current tid:%d", (unsigned int)pthread_self()));
      TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      usleep(80);
   }
}

/* ===================================================================== */
/**
 *   Generate high rate traces via TRI from object context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void *high_traceB(void *arg)
{
   int ret;

   if ((ret = prctl(PR_SET_NAME, (unsigned long)arg, 0, 0, 0)) != 0) {
      printf("Failed to set thread name %s, ret=%d errno=%d",
             (char*)arg, ret, errno);
      abort();
   }

   printf("Start high tracing process: thread=%s id=%u, pid=%u\n",
          (char*)arg, (unsigned int)pthread_self(), getpid());

   while(1) {
      INFO(STR("Thread1 current tid:%d, longer string", (unsigned int)pthread_self()));
      TRACE_ERROR(STR("Thread1 current tid:%d, longer string", (unsigned int)pthread_self()));
      TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ, longer string"));
      TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ, longer string"));
      usleep(90);
   }
}

/* ===================================================================== */
/**
 *   Generate high rate traces via TRI from object context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void *high_trace_defprocA(void *arg)
{
   int c = 0;
   int ret;

   if ((ret = prctl(PR_SET_NAME, (unsigned long)arg, 0, 0, 0)) != 0) {
      printf("Failed to set thread name %s, ret=%d errno=%d",
             (char*)arg, ret, errno);
      abort();
   }

   printf("Start high tracing process, default process: thread=%s id=%u, pid=%u\n",
          (char*)arg, (unsigned int)pthread_self(), getpid());

   while(1) {
      c++;
      INFO(STR("current tid:%d, cnt:%d", (unsigned int)pthread_self(), c));
      TRACE_ERROR(STR("current tid:%d", (unsigned int)pthread_self()));
      TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
      usleep(80);
   }
}

/* ===================================================================== */
/**
 *   Generate high rate traces via TRI from object context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void *high_trace_defprocB(void *arg)
{
   int c = 0;
   int ret;

   if ((ret = prctl(PR_SET_NAME, (unsigned long)arg, 0, 0, 0)) != 0) {
      printf("Failed to set thread name %s, ret=%d errno=%d",
             (char*)arg, ret, errno);
      abort();
   }

   printf("Start high tracing process, default process: thread=%s id=%u, pid=%u\n",
          (char*)arg, (unsigned int)pthread_self(), getpid());

   while(1) {
      c++;
      INFO(STR("current tid:%d, cnt:%d, longer string", (unsigned int)pthread_self(), c));
      TRACE_ERROR(STR("current tid:%d, longer string", (unsigned int)pthread_self()));
      TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ, longer string"));
      TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ, longer string"));
      usleep(90);
   }
}

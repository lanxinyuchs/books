/**
 *   Trace logger utility
 *
 *   @file tracelogger.c
 *
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
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
 *   ----------------------------------------------------------------------
 *   Revised : 2015-01-27 Nils Carlson <nils.carlson@ericsson.com>
 *   Change  : First revision. Can log a simple text string.
 *
 *   Revised : 2015-02-17 Niranjan Kumar (qnkudev)
 *   Change  : traceLogger is updated to generate bursts of test traces
 *             for given duration. Also added performance test case.
 *
 *   Revised : 2015-03-05 Niranjan Kumar (qnkudev)
 *   Change  : traceLogger is updated to generate bursts of data stream
 *             for given duration.
 *
 *   Revised : 2015-05-15 Niranjan Kumar (qnkudev)
 *   Change  : Added -c (core mask option). This will enable to generate
 *             traces on specified cores simoltaneously.
 *   ----------------------------------------------------------------------
 */
#include <stdio.h>
#include <unistd.h>
#include <sched.h>
#include <time.h>
#include <signal.h>
#include <pthread.h>
#include <lttng/lttng.h>
#include "com_ericsson_plf_trace_util.h"


#define TRACE_LOGGER_MAX_PERIOD         1000 /*milli seconds*/
#define TRACE_LOGGER_MIN_PERIOD         10   /*milli seconds*/
#define TRACE_LOGGER_DEFAULT_PERIOD     100  /*milli seconds*/

#define TRACE_LOGGER_MAX_DURATION       10*24*60*60  /*seconds*/
#define TRACE_LOGGER_MIN_DURATION       1            /*seconds*/
#define TRACE_LOGGER_DEFAULT_DURATION   10           /*seconds*/

#define TRACE_LOGGER_MIN_BURST          1
#define TRACE_LOGGER_MAX_BURST          10000000
#define TRACE_LOGGER_DEFAULT_BURST      500

#define TRACE_LOGGER_STRING_MSG         0
#define TRACE_LOGGER_DATA_MSG           1

#define LTTNG_EVENT_ALREADY_ENABLED     (-55)


typedef struct {
  uint32_t period;
  uint32_t duration;
  uint32_t burst;
  uint32_t msgType;
  uint32_t cpuMask;
  uint32_t cpu;
  char sessionName[256];
} TraceLogParams;

/* Counter used to illustrate number of traces sent */
static uint32_t *traceCounter = NULL;


/* ===================================================================== */
/**
 *   Prints the usage of tracelogger command.
 *   @param               -
 *
 *   @return              -
 *
 *   @par Globals:
 *                        -
 */
/* ===================================================================== */
static void
usage(void)
{
   fprintf(stderr, "\n\nShell command to generate bursts of LTTNG traces.\n"
                   "Usage: tracelogger <cmd> <param> ...\n\n"
                   "Commands <cmd>:\n"
                   "    generate <-s sessionName> [-t <type>] [-p <period>] [-b <burst>] [-d <duration>] [-c <cpumask>]\n"
                   "             Generates required number of traces for specified duration.\n"
                   "    perf     <-s sessionName> [-b <burst>] [-c <cpumask>]\n"
                   "             Measures time taken to generate specified number of traces.\n"
                   "\n"
                   "options:\n"
                   "        -s      Session name\n"
                   "        -t      Payload type. \n"
                   "                  0 = String message (default)\n"
                   "                  1 = Data stream\n"
                   "        -c      cpu mask. Each bit represents one cpu.\n"
                   "                  0x1 = Generate trace burst on cpu 0 (default)\n"
                   "                  0x3 = Generate trace burst on cpu 0 and cpu 1\n"
                   "        -p      Period in milli seconds to send # burst. Suggested:range 10-999. Default is 100\n"
                   "        -b      Number of traces to generate in a period. Default is 500\n"
                   "        -d      Total duration in seconds for trace generaion. Default is 10 seconds\n"
                   "                NOTE: If the specified burst is not  generated in requested period then\n"
                   "                      the duration will be more than requested. This occurs when too\n"
                   "                      much burst is requested in too little period\n"
                   "\n Sample diagram illustrating period,burst,duration\n\n"
                   "   |------------------------|-----------------------|-----------------------|\n"
                   "   <--       period       --><--    period      ---><--     period      --->\n"
                   "        (generate burst)         (generate burst)       (generate burst)    \n"
                   "   <------------------------------duration--------------------------------->\n");
}

/* ===================================================================== */
/**
 *   An empty handler used in timer function.
 *   @param               -
 *
 *   @return              -
 *
 *   @par Globals:
 *                        -
 */
/* ===================================================================== */
static void
handler(int sig, siginfo_t *si, void *uc)
{
   /* Empty handler */
}

/* ===================================================================== */
/**
 *   The function disables the specified event on the given session.
 *
 *   @param sessionName    Name of the session.
 *   @param eventName      Name of the event.
 *
 *   @return               zero on success, non zero on failure.
 *
 *   @par Globals:
 *                  -
 */
/* ===================================================================== */
static int
enableLttngEvent(char *sessionName,
                 char *eventName)
{
    struct lttng_domain domain;
    struct lttng_event event;
    struct lttng_handle *handle = NULL;
    struct lttng_channel *channels = NULL;
    int count = 0;
    int ret = 0;

    memset(&domain, 0, sizeof(domain));
    domain.type = LTTNG_DOMAIN_UST;
    domain.buf_type = LTTNG_BUFFER_PER_UID;

    /* create handle for the session */
    handle = lttng_create_handle(sessionName, &domain);
    if (handle == NULL) {
          ret = -1;
          printf("lttng_create_handle failed for ses:%s", sessionName);
          goto finish;
    }

    /* get all channels */
    count = lttng_list_channels(handle, &channels);
    if (count < 0) {
          ret = count;
          printf("lttng_list_channels failed,reason:%s(%d)",
                 lttng_strerror(ret), ret);
          goto finish;
    }

    /* Generate traces on first channel */
    if (count > 0 && &channels[0] != NULL) {

          /* Enable the testLog event */
          memset(&event, 0, sizeof(event));
          strncpy(event.name, eventName, LTTNG_SYMBOL_NAME_LEN);
          event.name[LTTNG_SYMBOL_NAME_LEN - 1] = 0;
          event.type = LTTNG_EVENT_TRACEPOINT;
          ret = lttng_enable_event(handle, &event, channels[0].name);

          if ((ret < 0) && (ret != LTTNG_EVENT_ALREADY_ENABLED)) {
               printf("Failed to enable event, %s\n", event.name);
               goto finish;
          }
    }

finish:
     return ret;

}

/* ===================================================================== */
/**
 *   The function disables the specified event on the given session.
 *
 *   @param sessionName    Name of the session.
 *   @param eventName      Name of the event.
 *
 *   @return               zero on success, non zero on failure.
 *
 *   @par Globals:
 *                  -
 */
/* ===================================================================== */
static int
disableLttngEvent(char *sessionName,
                  char *eventName)
{
    struct lttng_domain domain;
    struct lttng_event event;
    struct lttng_handle *handle = NULL;
    struct lttng_channel *channels = NULL;
    int count = 0;
    int ret = 0;

    memset(&domain, 0, sizeof(domain));
    domain.type = LTTNG_DOMAIN_UST;
    domain.buf_type = LTTNG_BUFFER_PER_UID;

    /* create handle for the session */
    handle = lttng_create_handle(sessionName, &domain);
    if (handle == NULL) {
          ret = -1;
          printf("lttng_create_handle failed for ses:%s", sessionName);
          goto finish;
    }

    /* get all channels */
    count = lttng_list_channels(handle, &channels);
    if (count < 0) {
          ret = count;
          printf("lttng_list_channels failed,reason:%s(%d)",
                 lttng_strerror(ret), ret);
          goto finish;
    }

    /* Generate traces on first channel */
    if (count > 0 && &channels[0] != NULL) {

              /* Disable the testLog event */
          memset(&event, 0, sizeof(event));
          strncpy(event.name, eventName, LTTNG_SYMBOL_NAME_LEN);
          event.name[LTTNG_SYMBOL_NAME_LEN - 1] = 0;
          event.type = LTTNG_EVENT_TRACEPOINT;
          ret = lttng_disable_event(handle, event.name, channels[0].name);

          if (ret < 0) {
               printf("Failed to disable event, %s\n", event.name);
               goto finish;
          }
    }

finish:
     return ret;
}
   

/* ===================================================================== */
/**
 *   The function generates the specified number of traces on given
 *   session.
 *
 *   @param sessionName    Name of the session.
 *   @param burst          Number of traces.
 *   @param msgType        Message type, string message or data stream
 *
 *   @return               zero on success, non zero on failure.
 *
 *   @par Globals:
 *                         traceCounter
 */
/* ===================================================================== */
static void *
generateLttngBurst(void *params1)
{

   int i = 0;
   uint32_t cpu = sched_getcpu();
   char logMsg[512];
   char data[256];
   TraceLogParams *params = (TraceLogParams *)params1;

    /* Initialize data stream */
    for (i = 0; i < 256; i++)    {
      data[i] = i;
    }

    for (i = 0 ; i < params->burst ; i ++) {
      snprintf(logMsg, sizeof(logMsg), "%s : trace_counter=%d",
               "LTTng trace logger: test trace written",
               ++traceCounter[cpu]);

      if (params->msgType == TRACE_LOGGER_DATA_MSG) {
        tracepoint(com_ericsson_plf_trace_util,
                   tsDataStream,
                   logMsg,
                   data, sizeof(data));
      }
      else {
        tracepoint(com_ericsson_plf_trace_util, testLog, logMsg);
      }
    }
    return NULL;
}

/* ===================================================================== */
/**
 *   The function starts single shot timer.
 *
 *   @param interval       Timer interval in milli seconds.
 *   @param hanlder        Handler function that is called on timer expiry.
 *
 *   @return               timer id on success.
 *
 *   @par Globals:
 *                         -
 */
/* ===================================================================== */
static timer_t
startOneShotTimer(uint64_t interval,
                  void (*handler)(int sig, siginfo_t *si, void *uc))
{
   timer_t timerid;
   struct itimerspec in;
   struct sigevent sev;
   struct sigaction sa;


   sa.sa_flags = SA_SIGINFO;
   sa.sa_sigaction = handler;
   sigemptyset(&sa.sa_mask);
   if (sigaction(SIGRTMIN, &sa, NULL) == -1) {
         printf("sigaction failed\n");
         exit(EXIT_FAILURE);
   }

   /* Create the timer */
   sev.sigev_notify = SIGEV_SIGNAL;
   sev.sigev_signo = SIGRTMIN;
   sev.sigev_value.sival_ptr = NULL;

   if (timer_create(CLOCK_REALTIME, &sev, &timerid) == -1) {
         printf("create timer failed\n");
         exit(EXIT_FAILURE);
   }

   in.it_value.tv_sec = 0;
   in.it_value.tv_nsec = interval * 1000000;
   in.it_interval.tv_sec = 0;
   in.it_interval.tv_nsec = 0;

   if (timer_settime(timerid, 0, &in, NULL) == -1)   {
         printf("Setting up of timer failed.\n");
         exit(EXIT_FAILURE);
   }
   return timerid;
}

/* ===================================================================== */
/**
 *   Parses the given arguments.
 *   @param argc       argument count
 *   @param argv       argument vector
 *   @param params     output param that contains parsed parameter values.
 *
 *   @return           Zero on success, non zero on failure..
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
int parseArgs(int argc, char **argv, TraceLogParams *params)
{
   char *endptr = NULL;
   int ret = 0;
   int opt;

   /* Store the default values to start with */
   params->period = TRACE_LOGGER_DEFAULT_PERIOD;
   params->burst = TRACE_LOGGER_DEFAULT_BURST;
   params->duration = TRACE_LOGGER_DEFAULT_DURATION;
   params->msgType = TRACE_LOGGER_STRING_MSG;
   params->cpuMask = 0x1;
   params->sessionName[0] = '\0';

   while ((opt = getopt(argc, argv, "p:b:d:t:s:c:")) != -1) {
       switch (opt) {
          case 's':
              snprintf(params->sessionName, sizeof(params->sessionName),"%s", optarg);
              break;
          case 'p':
              params->period = (uint32_t)strtoul(optarg, &endptr, 0);

              if (*endptr || (params->period < TRACE_LOGGER_MIN_PERIOD ||
                  params->period >= TRACE_LOGGER_MAX_PERIOD )) {

                    printf("Invalid input:period shall be between %d-%d\n",
                           TRACE_LOGGER_MIN_PERIOD, TRACE_LOGGER_MAX_PERIOD);
                    return -1;
              }
              break;
          case 'b':
              params->burst = (uint32_t)strtoul(optarg, &endptr, 0);

              if (*endptr || (params->burst < TRACE_LOGGER_MIN_BURST ||
                  params->burst >= TRACE_LOGGER_MAX_BURST )) {

                    printf("Invalid input:burst shall be between %d-%d\n",
                           TRACE_LOGGER_MIN_BURST, TRACE_LOGGER_MAX_BURST);
                    return -1;
              }
              break;
          case 'd':
              params->duration = (uint32_t)strtoul(optarg, &endptr, 0);

              if (*endptr || (params->duration < TRACE_LOGGER_MIN_DURATION ||
                  params->duration >= TRACE_LOGGER_MAX_DURATION )) {

                    printf("Invalid input:duration shall be between %d-%d\n",
                           TRACE_LOGGER_MIN_DURATION, TRACE_LOGGER_MAX_DURATION);
                    return -1;
              }
              break;
          case 't':
              params->msgType = (uint32_t)strtoul(optarg, &endptr, 0);

              if (*endptr) {
                    printf("Invalid input:type shall be 0 or 1.\n");
                   return -1;
              }
              break;
          case 'c':
              params->cpuMask = (uint32_t)strtoul(optarg, &endptr, 0);

              if (*endptr) {
                    printf("Invalid input:coremask shall be greater than or equal to zero.\n");
                   return -1;
              }
              break;
          default: /* '?' */
              printf("Invalid syntax: Check the arguments.\n");
              return -1;
          }
    }
   return ret;
}

/* ===================================================================== */
/**
 *   Handle function for 'generate' command.
 *   @param argc       argument count
 *   @param argv       argument vector
 *
 *   @return           Zero on success, non zero on failure..
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
int handleGenerateCommand(int argc, char *argv[])
{
   uint32_t  noOfShots;
   TraceLogParams params;
   timer_t timerId;
   struct itimerspec currVal;
   int i,j, ret;
   pthread_t thread_id;
   cpu_set_t cpuSet;
   pthread_attr_t attr;

   /* Parse the args and capture the parameters into 'params' structure. */
   ret = parseArgs(argc,argv, &params);

   if (ret != 0) {
     printf("Command parsing failed\n");
     usage();
     return ret;
   }

   /* Argument parsing done. Check if session name is provided.*/
   if (params.sessionName[0] == 0)
   {
      printf("Session name is not provided. Use -s to provide session name. \n");
      return -1;
   }

   enableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:tsDataStream");
   enableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:testLog");

   /* Calculate the number of shots required according to the
    * given period and duration.
    */
   noOfShots = (params.duration * 1000)/(params.period);


   /* For each shot, 'burst' number of traces are generated.
    * If still time is left in 'period' then sleep for remaining
    * time of 'period'.
    */ 
   for (i = 0; i < noOfShots ; i ++) {
      /* Starting timer */
      timerId = startOneShotTimer(params.period, handler);

      for (j = 0; j < sysconf(_SC_NPROCESSORS_ONLN); j ++) {

          if (!(params.cpuMask & (1 << j)))
            continue;

          pthread_attr_init(&attr);
          CPU_ZERO(&cpuSet);
          CPU_SET(j, &cpuSet);
          if (pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpuSet)) {
              printf("Setting scheduler affinitiy failed \n");
              ret = -1;
              goto finished;
          }
          pthread_create(&thread_id, &attr, &generateLttngBurst, &params);
      }
      pthread_join(thread_id, NULL);


      /* Check if there is still some time left in 'period',
       * if so, sleep for remaing time of 'period'.
       */
      timer_gettime(timerId, &currVal);
      usleep(currVal.it_value.tv_nsec/100);

      /* Deleting timer*/
      if (timerId)
         timer_delete(timerId);
   }

 finished:
   disableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:tsDataStream");
   disableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:testLog");

   return ret;
}

/* ===================================================================== */
/**
 *   Handle function for 'perf' command.
 *   @param argc       argument count
 *   @param argv       argument vector
 *
 *   @return           Zero on success, non zero on failure..
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
int handlePerfCommand(int argc, char *argv[])
{
   TraceLogParams params;
   int ret;
   struct timespec startTp, endTp;
   int diffSeconds, diffMicros, i;
   pthread_t thread_id;
   cpu_set_t cpuSet;
   pthread_attr_t attr;


   /* Parse the args and capture the parameters into 'params' structure. */
   ret = parseArgs(argc,argv, &params);

   if (ret != 0){
     printf("Command parsing failed\n");
     usage();
     return ret;
   }
   /* Argument parsing done. Check if session name is provided.*/
   if (params.sessionName[0] == 0) {
      printf("Session name is not provided. "
             "Use -s to provide session name. \n");
      return -1;
   }

   enableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:tsDataStream");
   enableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:testLog");

   /* Capture start time of the test */
   if ( clock_gettime(CLOCK_MONOTONIC, &startTp) != 0 )
      printf("Failed to get system time\n");
   

   for (i = 0; i < sysconf(_SC_NPROCESSORS_ONLN); i ++) {

     if (!(params.cpuMask & (1 << i)))
         continue;

     pthread_attr_init(&attr);
     CPU_ZERO(&cpuSet);
     CPU_SET(i, &cpuSet);
     if (pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpuSet)) {
        ret = -1;
        goto finished;
     }
     pthread_create(&thread_id, &attr, &generateLttngBurst, &params);
   }
   pthread_join(thread_id, NULL);


   /* Capture end time */
   if ( clock_gettime(CLOCK_MONOTONIC, &endTp) != 0 )
      printf("Failed to get system time\n");


   /* Calcualte time taken to generate traces. */
   diffSeconds = endTp.tv_sec - startTp.tv_sec;
   diffMicros = (endTp.tv_nsec - startTp.tv_nsec)/1000;

   if (diffMicros < 0 && diffSeconds > 0)  {
        diffSeconds = diffSeconds - 1;
        diffMicros  = diffMicros + 1000000;
   }
   else if (diffMicros > 0 && diffSeconds < 0)      {
        diffSeconds = diffSeconds + 1;
        diffMicros  = diffMicros - 1000000;
   }

   printf("Performance test: Total time taken for %u traces "
          "is %d seconds and %d microseconds \n",
          params.burst, diffSeconds, diffMicros);

 finished:

   disableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:tsDataStream");
   disableLttngEvent(params.sessionName, "com_ericsson_plf_trace_util:testLog");


   return ret;
}


/* ===================================================================== */
/**
 *   This is the starting point and ending point of the exection of any
 *   'tracelogger' command.
 *
 *   @param           argc  argument count
 *                    argv  argument vector
 *
 *   @return          0 on successful command execution.
 *                    Non zero on failure.
 *
 *   @par Globals:
 *                    traceCounter
 */
/* ===================================================================== */
int main(int argc, char *argv[])
{
   int ret = 0;
   uint32_t noOfCores = sysconf(_SC_NPROCESSORS_ONLN);

   if (argc == 1) {
      usage();
      return 0;
   }

   /* Initialize the trace counter*/
   traceCounter = (uint32_t *)malloc(noOfCores * sizeof(uint32_t));
   memset(traceCounter, 0, noOfCores * sizeof(uint32_t));

   if (!strcmp(argv[1], "generate")) {
     ret = handleGenerateCommand(argc - 1, argv + 1);
   }
   else if(!strcmp(argv[1], "perf")) {
     ret = handlePerfCommand(argc -1, argv + 1);
   }
   else {
     printf("Invalid command:%s\n", argv[1]);
     ret = -1;
   }
   
   if ( ret != 0) {
     printf("Command failed.\n");
   }
   return ret;
}

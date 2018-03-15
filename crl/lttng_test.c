#include <cello_te_ose.h>
#include <cello_te_trace.h>
#include <cello_te_trace_if.h>
#include <cello_te_trace_obj.h>

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <shell.h>
#include <unistd.h>
#include <pthread.h>

#include "lttng_test.sig"
#include "lttng_test.h"

#define TRACEPOINT_DEFINE
#include "com_ericsson_plf_lttng_test.h"

#define MAX_TRACES_PER_SECOND 5000
#define TRACE_OVERLOAD_LOOPS  10

union SIGNAL {
     SIGSELECT sigNo;
     struct LttngTestStart start;
};

DECLARE_INTERFACE(LTTNG_TRI_IF);
DECLARE_TRACE_OBJ(LTTNG_TRI_OBJ);
DECLARE_TRACE_OBJ(LTTNG_TRI_OBJ_1);

OSENTRYPOINT(high_traceA);
OSENTRYPOINT(high_traceB);
OSENTRYPOINT(high_trace_defprocA);
OSENTRYPOINT(high_trace_defprocB);

void testReturn(void)
{
RETURN;
}

OS_PROCESS(lttng_tri_test)
{
     static SIGSELECT         anySig[] = {0};
     union SIGNAL            *sig_p;
     char sig_name[20]       = "LTTNG_TEST_START";
     char buf[32] = {0};
     uint32_t i;
     uint8_t smallData[100];
     uint8_t bigData[2048];
     PROCESS pid[12];

     while(1)
     {
          sig_p = receive(anySig);
          if( sig_p->sigNo != LTTNG_TEST_START )
          {
               TRACE_ERROR(STR("Received unknown signal 0x%x from sender 0x%x",
                               sig_p->sigNo, sender(&sig_p)));
               free_buf( &sig_p );
               continue;
          }

          switch( sig_p->start.tc )
          {
          case 1:
          {
               printf("Generating TRI traces\n");
               INFO("Test of INFO");
               TRACE_ERROR("Test of ERROR");
               TRACE(1, "Test of TRACE1");
               TRACE(2, "Test of TRACE2");
               TRACE(3, "Test of TRACE3");
               TRACE(4, "Test of TRACE4");
               TRACE(5, "Test of TRACE5");
               TRACE(6, "Test of TRACE6");
               TRACE(7, "Test of TRACE7");
               TRACE(8, "Test of TRACE8");
               TRACE(9, "Test of TRACE9");
               TRACE_PARAM("Test of PARAM");
               TRACE_STATE("Test of STATE");
               ENTER("Test of ENTER");
               testReturn();
               TRACE_REC_SIG(sig_p, "Test of REC_SIG");
               TRACE_SEND_SIG(sig_p, sender(&sig_p), "Test of SEND_SIG");
               INFO(STR("Test of STR: Signal name %s, Sianal No %d",
                        sig_name, sender(&sig_p) ));
            /* Generate TRI binary traces */
               for( i = 0; i < 100; i++ )
                    smallData[i] = (char)i;
               for( i = 0; i < 2048; i++ )
                    bigData[i] = (char)i;

               TRACE_BUS_SEND("Test of BUS_SEND with 100 byte data", smallData, 100);
               TRACE_BUS_RECEIVE("Test of BUS_RECEIVE with 2048 byte data", bigData, 2048);
           /* Generate TRI trace UTS */
               /*TRACE_UTS(1, 100, 500,
                           "lttng_test.c", 102, "lttng_test",
                           "test of TRACE_UTS", smallData, 100);*/

               (void)OMCSF_logTrace(4,
                                    __SHORT_FILE__,
                                    __LINE__,
                                    STR("My process string with val=%d", 444),
                                    TRI_PROC_INFO_PTR);
               break;
          }
          case 2:
          {
               char smallData[100];
               char bigData[2048];

               printf("Generating TRI IF traces\n");
               TRACE_IF(1, "Test of TRACE1");
               TRACE_IF(2, "Test of TRACE2");
               TRACE_IF(3, "Test of TRACE3");
               TRACE_IF(4, "Test of TRACE4");
               TRACE_IF(5, "Test of TRACE5");
               TRACE_IF(6, "Test of TRACE6");
               TRACE_IF(7, "Test of TRACE7");
               TRACE_IF(8, "Test of TRACE8");
               TRACE_IF(9, "Test of TRACE9");
               TRACE_IF_STATE("Test of STATE");
               ENTER("Test of ENTER");
               testReturn();
               TRACE_IF_REC_SIG(sig_p, "Test of REC_SIG");
               TRACE_IF_SEND_SIG(sig_p, sender(&sig_p), "Test of SEND_SIG");
           /*  Generate interface binary traces */
               for( i = 0; i < 100; i++ )
                    smallData[i] = (char)i;
               for( i = 0; i < 2048; i++ )
                    bigData[i] = (char)i;

               TRACE_IF_BUS_SEND("Test of BUS_SEND with 100 byte data",
                                 (uint8_t *) smallData, 100);
               TRACE_IF_BUS_RECEIVE("Test of BUS_RECEIVE with 2048 byte data",
                                    (uint8_t *) bigData, 2048);

               (void)OMCSF_logObjTrace(4,
                                       __SHORT_FILE__,
                                       __LINE__,
                                       STR("My interface string with val=%d", 444),
                                       TRI_PROC_INFO_PTR,
                                       TRI_IF_INFO_PTR);
               break;
          }
          case 5:
          {
               /*
                 Characteristics requirement PECA_100:
                 The SW platform shall allow all applications sharing
                 one HW-thread to produce 5000 traces per second.
               */
               unsigned int i, j;

               /* The number of microseconds to sleep between each trace */
               useconds_t us = (1000 * 1000) /  MAX_TRACES_PER_SECOND;

               printf("Generating %d traces per second for %d seconds...\n",
                      MAX_TRACES_PER_SECOND, TRACE_OVERLOAD_LOOPS);
               for(i = 0; i < TRACE_OVERLOAD_LOOPS; i++)
               {
                    for(j = 0; j < MAX_TRACES_PER_SECOND; j++)
                    {
                         INFO(STR("Trace overload test, iteration %d/%d, loop %d/%d",
                                  i+1, TRACE_OVERLOAD_LOOPS, j+1,
                                  MAX_TRACES_PER_SECOND));
                         usleep(us);
                    }
               }
               printf("Done\n");
               break;
          }
          case 7:
          {
              /* test trace obj */
               TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(3, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(4, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(5, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(6, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(7, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(8, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ(9, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
               TRACE_OBJ_SEND_SIG(LTTNG_TRI_OBJ, sig_p, sender(&sig_p),
                                  "Test of SEND_SIG for TRACE OBJ");
               TRACE_OBJ_REC_SIG(LTTNG_TRI_OBJ,sig_p, "Test of REC_SIG for TRACE OBJ");
               TRACE_OBJ_PARAM(LTTNG_TRI_OBJ, "Test of PARAM for TRACE OBJ");
               TRACE_OBJ_STATE(LTTNG_TRI_OBJ, "Test of STATE for TRACE OBJ");
               TRACE_OBJ_BUS_SEND(LTTNG_TRI_OBJ,
                     "Test of BUS_SEND with 100 byte data for TRACE OBJ", smallData, 100);
               TRACE_OBJ_BUS_RECEIVE(LTTNG_TRI_OBJ,
                     "Test of BUS_RECEIVE with 100 byte data for TRACE OBJ", smallData, 100);

              /* test trace obj_1 */
               TRACE_OBJ(1, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(2, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(3, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(4, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(5, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(6, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(7, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(8, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ(9, LTTNG_TRI_OBJ_1, STR("Test of TRACE_OBJ_1"));
               TRACE_OBJ_SEND_SIG(LTTNG_TRI_OBJ_1, sig_p, sender(&sig_p),
                                  "Test of SEND_SIG for TRACE OBJ_1");
               TRACE_OBJ_REC_SIG(LTTNG_TRI_OBJ_1, sig_p, "Test of REC_SIG for TRACE OBJ_1");
               TRACE_OBJ_PARAM(LTTNG_TRI_OBJ_1, "Test of PARAM for TRACE OBJ_1");
               TRACE_OBJ_STATE(LTTNG_TRI_OBJ_1, "Test of STATE for TRACE OBJ_1");
               TRACE_OBJ_BUS_SEND(LTTNG_TRI_OBJ_1,
                     "Test of BUS_SEND with 100 byte data for TRACE OBJ_1", smallData, 100);
               TRACE_OBJ_BUS_RECEIVE(LTTNG_TRI_OBJ_1,
                     "Test of BUS_RECEIVE with 100 byte data for TRACE OBJ_1", smallData, 100);

               (void)OMCSF_logObjTrace(4,
                                       __SHORT_FILE__,
                                       __LINE__,
                                       STR("My object string with val=%d", 444),
                                       TRI_PROC_INFO_PTR,
                                       TRI_OBJ_INFO_PTR(LTTNG_TRI_OBJ));
               break;
          }
          case 9:
          {
               INFO(MULTI_SEG_TRACE_1);
               INFO(MULTI_SEG_TRACE_2);
               INFO(MULTI_SEG_TRACE_3);
               INFO(MULTI_SEG_TRACE_4);
               INFO(MULTI_SEG_TRACE_5);
               break;
          }
          case 11:
               for (i = 0; i < 12; i++) {
                    switch(i) {
                    case 0:
                    case 1:
                    case 2:
                         sprintf(buf, "high_trace_%d", i);
                         pid[i] = create_process(OS_PRI_PROC, buf, high_traceA, 1000,
                                                 (OSPRIORITY) 16, (OSTIME) 0, (PROCESS) 0,
                                                 (struct OS_redir_entry *) NULL, (OSVECTOR) 0,
                                                 (OSUSER) 0);
                         break;
                    case 3:
                    case 4:
                    case 5:
                         sprintf(buf, "high_trace_%d", i);
                         pid[i] = create_process(OS_PRI_PROC, buf, high_traceB, 1000,
                                                 (OSPRIORITY) 16, (OSTIME) 0, (PROCESS) 0,
                                                 (struct OS_redir_entry *) NULL, (OSVECTOR) 0,
                                                 (OSUSER) 0);
                         break;
                    case 6:
                    case 7:
                    case 8:
                         sprintf(buf, "high_trace_def_%d", i);
                         pid[i] = create_process(OS_PRI_PROC, buf, high_trace_defprocA, 1000,
                                                 (OSPRIORITY) 16, (OSTIME) 0, (PROCESS) 0,
                                                 (struct OS_redir_entry *) NULL, (OSVECTOR) 0,
                                                 (OSUSER) 0);
                         break;
                    case 9:
                    case 10:
                    case 11:
                         sprintf(buf, "high_trace_def_%d", i);
                         pid[i] = create_process(OS_PRI_PROC, buf, high_trace_defprocB, 1000,
                                                 (OSPRIORITY) 16, (OSTIME) 0, (PROCESS) 0,
                                                 (struct OS_redir_entry *) NULL, (OSVECTOR) 0,
                                                 (OSUSER) 0);
                         break;
                    }
                    start(pid[i]);
                    delay(1);
               }
               sleep(5);
               for (i = 0; i < 12; i++) {
                    printf("kill pid: %d\n", pid[i]);
                    kill_proc(pid[i]);
               }
               printf("Done\n");
               break;
          default:
               printf("Internal error\n");
               break;
          }
          free_buf( &sig_p );
     }
}

OS_PROCESS(lttng_trace_test)
{
     static SIGSELECT         anySig[] = {0};
     union SIGNAL            *sig_p;

     while(1)
     {
          sig_p = receive(anySig);
          if( sig_p->sigNo != LTTNG_TEST_START )
          {
               TRACE_ERROR(STR("Received unknown signal 0x%x from sender 0x%x",
                               sig_p->sigNo, sender(&sig_p)));
               free_buf( &sig_p );
               continue;
          }

          switch( sig_p->start.tc )
          {
          case 3:
          {
               printf("Generating LTTng traces\n");
               tracepoint(com_ericsson_plf_lttng_test, EMERG_Test,   "Test of EMERG");
               tracepoint(com_ericsson_plf_lttng_test, ALERT_Test,   "Test of ALERT");
               tracepoint(com_ericsson_plf_lttng_test, CRIT_Test,    "Test of CRIT");
               tracepoint(com_ericsson_plf_lttng_test, ERR_Test,     "Test of ERR");
               tracepoint(com_ericsson_plf_lttng_test, WARNING_Test, "Test of WARNING");
               tracepoint(com_ericsson_plf_lttng_test, NOTICE_Test,  "Test of NOTICE");
               tracepoint(com_ericsson_plf_lttng_test, INFO_Test,    "Test of INFO");
               tracepoint(com_ericsson_plf_lttng_test, DEBUG_Test,   "Test of DEBUG");
               break;
          }
          case 4:
          {
               /*
                 Characteristics requirement PECA_100:
                 The SW platform shall allow all applications
                 sharing one HW-thread to produce 5000 traces per second.
               */
               unsigned int i, j;
               /* The number of microseconds to sleep between each trace */
               useconds_t us = (1000 * 1000) /  MAX_TRACES_PER_SECOND;
               printf("Generating %d traces per second for %d seconds...\n",
                      MAX_TRACES_PER_SECOND, TRACE_OVERLOAD_LOOPS);
               for(i = 0; i < TRACE_OVERLOAD_LOOPS; i++)
               {
                    for(j = 0; j < MAX_TRACES_PER_SECOND; j++)
                    {
                         tracepoint(com_ericsson_plf_lttng_test, Overload_Test,
                                    "Trace overload test", i+1, TRACE_OVERLOAD_LOOPS,
                                    j+1, MAX_TRACES_PER_SECOND);
                         usleep(us);
                    }
               }
               printf("Done\n");
               break;
          }
          case 8:
          {
               char smallData[100];
               char bigData[2048];
               int i;

               for( i = 0; i < 100; i++ )
                    smallData[i] = (char)i;
               for( i = 0; i < 2048; i++ )
                    bigData[i] = (char)i;

               tracepoint(com_ericsson_plf_lttng_test, Binary_Test,
                          "Test of binary trace with 100 byte data", smallData, 100);
               tracepoint(com_ericsson_plf_lttng_test, Binary_Test,
                          "Test of binary trace with 2048 byte data", bigData, 2048);
               break;
          }
          case 10:
          {
               tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_1);
               tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_2);
               tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_3);
               tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_4);
               tracepoint(com_ericsson_plf_lttng_test, Multi_Seg_Test, MULTI_SEG_TRACE_5);
               break;
          }
          default:
               printf("Internal error\n");
               break;
          }

          free_buf( &sig_p );
     }
}


void printUsage()
{
     printf("LTTng test command\n");
     printf("Usage: lttng_test <-tc n>\n");
     printf("Where tc is one of:\n");
     printf("\t1:  Generate TRI traces\n");
     printf("\t2:  Generate TRI interface traces\n");
     printf("\t3:  Generate LTTng traces\n");
     printf("\t4:  LTTng trace overload\n");
     printf("\t5:  TRI trace overload\n");
     printf("\t6:  Streaming traces\n");
     printf("\t7:  Generate TRI trace objects\n");
     printf("\t8:  Generate LTTng binary traces\n");
     printf("\t9:  Generate TRI multi segment traces\n");
     printf("\t10: Generate LTTng multi segment traces\n");
     printf("\t11: Generate traces from multiple threads at a high rate\n");
     printf("\t12: Generate LTTng multi core traces\n");
     printf("\t13: Generate max streamng traces\n");
     printf("\t14: Generate traces with identity\n");
}

int lttng_test_cmd(int argc, char **argv)
{
     int tc;
     union SIGNAL *sig_p;
     PROCESS pid;

     if( argc != 3 )
     {
          printUsage();
          return -1;
     }

     if( argv[1][0] == '-' && argv[1][1] == 't' && argv[1][2] == 'c' )
     {
          tc = atoi(argv[2]);
          if( tc < 1 || tc > 15 )
          {
               printUsage();
               return -1;
          }

          sig_p = alloc( sizeof( struct LttngTestStart ), LTTNG_TEST_START );
          sig_p->start.tc = tc;

          switch( tc )
          {
          case 1:   /* Generic TRI TC fallthrough */
          case 2:
          case 5:
          case 7:
          case 9:
          case 11:
               hunt( "lttng_tri_test", 0, &pid, NULL );
               send( &sig_p, pid );
               break;
          case 3:   /* Generic LTTng TC fallthrough */
          case 4:
          case 8:
          case 10:
               hunt( "lttng_trace_test", 0, &pid, NULL );
               send( &sig_p, pid );
               break;

          default:
               printf("TC not implemented yet\n");
               free_buf( &sig_p );
               return -1;
          }
     }
     else
     {
          printUsage();
          return -1;
     }
     return 0;
}

OS_PROCESS(high_traceA)
{
     printf("Start high tracing process, thread id : %u, pid %u\n",
            (unsigned int)pthread_self(), getpid());
     while(1) {
          INFO(STR("Thread1 current process:%d", current_process()));
          TRACE_ERROR(STR("Thread1 current process:%d", current_process()));
          TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
          TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ"));
          usleep(80);
     }
}

OS_PROCESS(high_traceB)
{
     printf("Start high tracing process, thread id : %u, pid %u\n",
            (unsigned int)pthread_self(), getpid());
     while(1) {
          INFO(STR("Thread1 current process:%d, longer string", current_process()));
          TRACE_ERROR(STR("Thread1 current process:%d, longer string", current_process()));
          TRACE_OBJ(1, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ, longer string"));
          TRACE_OBJ(2, LTTNG_TRI_OBJ, STR("Test of TRACE_OBJ, longer string"));
          usleep(90);
     }
}

OS_PROCESS(lttng_test)
{
     PROCESS pid;

     shell_add_cmd("lttng_test",
                   "lttng_test <-tc n>",
                   "LTTng test command", lttng_test_cmd);

     pid = create_process(OS_PRI_PROC,
                          "lttng_tri_test",
                          lttng_tri_test,
                          10000,
                          (OSPRIORITY) 16,
                          (OSTIME) 0,
                          (PROCESS) 0,
                          (struct OS_redir_entry *) NULL, 
                          (OSVECTOR) 0,
                          (OSUSER) 0);

     start( pid );

     pid = create_process(OS_PRI_PROC,
                          "lttng_tri_no2_test",
                          lttng_tri_test,
                          10000,
                          (OSPRIORITY) 16,
                          (OSTIME) 0,
                          (PROCESS) 0,
                          (struct OS_redir_entry *) NULL, 
                          (OSVECTOR) 0,
                          (OSUSER) 0);

     start( pid );

     pid = create_process(OS_PRI_PROC,
                          "lttng_trace_test",
                          lttng_trace_test,
                          10000,
                          (OSPRIORITY) 16,
                          (OSTIME) 0,
                          (PROCESS) 0,
                          (struct OS_redir_entry *) NULL, 
                          (OSVECTOR) 0,
                          (OSUSER) 0);

     start( pid );

     stop(current_process());
}

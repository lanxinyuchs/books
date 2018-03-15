#include <cello_te_ose.h>
#include <cello_te_trace.h>
#include <cello_te_trace_if.h>
#include <cello_te_trace_obj.h>

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <syslog.h>
#include <itc.h>
#include <errno.h>

#include "lttng_test.h"
#include "lttng_test.sig"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define MAX_TRACES_PER_SECOND 5000
#define TRACE_OVERLOAD_LOOPS  10

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg {
        uint32_t              msgno;
        struct LttngTestStart start;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

DECLARE_INTERFACE(LTTNG2_TRI_IF);
DECLARE_TRACE_OBJ(LTTNG2_TRI_OBJ);

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void testReturn(void)
{
        RETURN;
}

/* ===================================================================== */
/**
 *   Generate traces via TRI from thread context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                smallData, bigData
 */
/* ===================================================================== */
void generate_tri_trace(void)
{
        char msg_name[20]       = "LTTNG_TEST_START";
        union itc_msg *msg;
        char smallData[100];
        char bigData[2048];
        int i;

        msg = itc_alloc(sizeof( struct LttngTestStart ), LTTNG_TEST_START);
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
        TRACE_REC_SIG(msg, "Test of REC_SIG");
        TRACE_SEND_SIG(msg, itc_sender(msg), "Test of SEND_SIG");
        INFO(STR("Test of STR: Signal name %s, Signal no %d",
                 msg_name, itc_sender(msg) ));
        /* Generate TRI binary traces */
        for( i = 0; i < 100; i++ )
           smallData[i] = (char)i;
        for( i = 0; i < 2048; i++ )
           bigData[i] = (char)i;
        
        TRACE_BUS_SEND("Test of BUS_SEND with 100 byte data",
                       (uint8_t *)smallData, 100);
        TRACE_BUS_RECEIVE("Test of BUS_RECEIVE with 2048 byte data",
                          (uint8_t *)bigData, 2048);
        /* Generate TRI trace UTS */
        /*TRACE_UTS(1, 100, 500,
          "lttng_test.c", 102, "lttng2_test",
          "test of TRACE_UTS", smallData, 100);*/

        itc_free(&msg);
}

/* ===================================================================== */
/**
 *   Generate traces via TRI from interface context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_tri_trace_if(void)
{
        union itc_msg *msg;
        char smallData[100];
        char bigData[2048];
        int i;
        
        msg = itc_alloc(sizeof( struct LttngTestStart ), LTTNG_TEST_START);
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
        TRACE_IF_REC_SIG(msg, "Test of REC_SIG");
        TRACE_IF_SEND_SIG(msg, itc_sender(msg), "Test of SEND_SIG");
        /*  Generate interface binary traces */
        for( i = 0; i < 100; i++ )
           smallData[i] = (char)i;
        for( i = 0; i < 2048; i++ )
           bigData[i] = (char)i;
        
        TRACE_IF_BUS_SEND("Test of BUS_SEND with 100 byte data",
                          (uint8_t *)smallData, 100);
        TRACE_IF_BUS_RECEIVE("Test of BUS_RECEIVE with 2048 byte data",
                             (uint8_t *)bigData, 2048);
        itc_free(&msg);
}

/* ===================================================================== */
/**
 *   Generate traces via TRI with high through put.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                smallData
 */
/* ===================================================================== */
void perf_tri_trace(void)
{
        unsigned int i, j;

        /* The number of microseconds to sleep between each trace */
        useconds_t us = (1000 * 1000) /  MAX_TRACES_PER_SECOND;
        
        syslog(LOG_INFO, "Generating %d traces per second for %d seconds...",
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
}

/* ===================================================================== */
/**
 *   Generate traces via TRI from object context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                smallData
 */
/* ===================================================================== */
void generate_tri_trace_obj(void)
{
        union itc_msg *msg;
        char smallData[100];

        msg = itc_alloc(sizeof( struct LttngTestStart ), LTTNG_TEST_START);
        TRACE_OBJ(1, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(2, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(3, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(4, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(5, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(6, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(7, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(8, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ(9, LTTNG2_TRI_OBJ, STR("Test of TRACE_OBJ"));
        TRACE_OBJ_SEND_SIG(LTTNG2_TRI_OBJ, msg, itc_sender(msg),
                           "Test of SEND_SIG for TRACE OBJ");
        TRACE_OBJ_REC_SIG(LTTNG2_TRI_OBJ, msg,
                          "Test of REC_SIG for TRACE OBJ");
        TRACE_OBJ_PARAM(LTTNG2_TRI_OBJ, "Test of PARAM for TRACE OBJ");
        TRACE_OBJ_STATE(LTTNG2_TRI_OBJ, "Test of STATE for TRACE OBJ");
        TRACE_OBJ_BUS_SEND(LTTNG2_TRI_OBJ,
                           "Test of BUS_SEND with 100 byte data for TRACE OBJ",
                           (uint8_t *)smallData, 100);
        TRACE_OBJ_BUS_RECEIVE(LTTNG2_TRI_OBJ,
                              "Test of BUS_RECEIVE with 100 byte data for TRACE OBJ",
                              (uint8_t *)smallData, 100);
        itc_free(&msg);
}

/* ===================================================================== */
/**
 *   Generate large traces via TRI from thread context.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void generate_tri_trace_multi_segment(void)
{
        INFO(MULTI_SEG_TRACE_1);
        INFO(MULTI_SEG_TRACE_2);
        INFO(MULTI_SEG_TRACE_3);
        INFO(MULTI_SEG_TRACE_4);
        INFO(MULTI_SEG_TRACE_5);
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
        tri_register_thread_name("lttng2_tri_kill_test");
        syslog(LOG_INFO, "lttng2_tri_kill_test go to sleep");
        sleep(5);
        syslog(LOG_INFO, "lttng2_tri_kill_test going for pthread_exit");
        pthread_exit(NULL);
}

/* ===================================================================== */
/**
 *   Thread used to test repetetive registration and deregistration.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void *lttng2_tri_parent_test(void *arg)
{
        pthread_t tid;
        int ret;

        tri_register_thread_name("lttng2_tri_parent_test");

        while(1) {
          if ((ret = pthread_create(&tid, NULL, lttng2_tri_kill_test, NULL)) != 0 ) {
             syslog(LOG_ERR, "%s: Failed to create lttng2_tri_kill_test", __func__);
             pthread_exit(&ret);
           }
          pthread_join(tid, NULL);

           INFO(STR("%s: lttng2_tri_kill_test tid=0x%x is killed",__func__, tid));

           sleep(1);
        }
        exit(0);
}

/* ===================================================================== */
/**
 *   Thread running all test cases.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
void *lttng2_tri_test_1(void *arg)
{
        pthread_t tid;
        int tc = 1;
        int ret;

        /* Open syslog for logging */
        openlog("LTTNG2_TRI_TEST", 0, LOG_DAEMON);

        if ((ret = tri_init()) != TRI_OK) {
          syslog(LOG_ERR, "%s: tri_init failed for lttng2_tri_test_1, ret=%d",
                 __func__, ret);
        }
        INFO("lttng2_tri_test_1 called tri_init");

        tri_register_thread_name("lttng2_tri_test_1");

        if ( pthread_create(&tid, NULL, lttng2_tri_parent_test, NULL) != 0 ) {
           syslog(LOG_ERR, "%s: Failed to create lttng2_tri_parent_test",__func__);
        }

        for(;;) 
        {
          switch( tc )
          {
          case 1:
          {
               syslog(LOG_INFO, "Generating TRI traces");
               generate_tri_trace();
               break;
          }
          case 2:
          {
               syslog(LOG_INFO, "Generating TRI IF traces");
               generate_tri_trace_if();
               break;
          }
          case 3:
          {
               /*
                 Characteristics requirement PECA_100:
                 The SW platform shall allow all applications sharing
                 one HW-thread to produce 5000 traces per second.
               */
               perf_tri_trace();
               break;
          }
          case 4:
          {
               syslog(LOG_INFO, "Generating TRI OBJ traces");
               generate_tri_trace_obj();

               break;
          }
          case 5:
          {
               syslog(LOG_INFO, "Generating Multi segment traces");
               generate_tri_trace_multi_segment();
               break;
          }
          case 6:
          {
               /* Dummy test case to restart the test loops */
               tc = 0;
               sleep(5);
               break;
          }
          default:
            syslog(LOG_ERR, "Internal error");
               break;
          }
          /* Increment to next test case */ 
          tc++;
        }
        closelog();
        exit(0);
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

        tri_register_thread_name("lttng2_tri_test_2");

        INFO(STR("lttng2_tri_test_2 started"));

        while (1)
        {
           sleep(5);
        }
        exit(0);
}

/* ===================================================================== */
/**
 *   Main thread for test client.
 *
 *   @param      -
 *
 *   @return           -
 *
 *   @par Globals:
 *                -
 */
/* ===================================================================== */
int main(int argc, char **argv)
{
        pthread_t tid1, tid2;
        int ret;

        /* TRI client must initialize ITC before any usage of TRI. */
        ret = itc_init(32, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
        if ((ret != 0) && (ret != ITC_EALREADY_INITIALISED)) {
           syslog(LOG_ERR, "itc_init failed, ret=%d", ret);
           abort();
        }

        if ( pthread_create(&tid1, NULL, lttng2_tri_test_1, NULL) != 0 ) {
           syslog(LOG_ERR, "%s: Failed to create lttng2_tri_test", __func__);
           abort();
        }

        if ( pthread_create(&tid2, NULL, lttng2_tri_test_2, NULL) != 0 ) {
           syslog(LOG_ERR, "%s: Failed to create lttng2_tri_test", __func__);
           abort();
        }

        pthread_join(tid1, NULL);
        pthread_join(tid2, NULL);

        return 0;
}

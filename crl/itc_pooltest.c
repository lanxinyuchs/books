#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <pthread.h>

#include "itc.h"

#define MAX_TEST_SIZES 10

#define CONC_CONTEXTS 5
#define CONC_LOOPS 10

union itc_msg {
        uint32_t msgno;
};

struct thr_test {
        int loops;
        int testno;
};

struct alloc_settings {
        int size;
        int nr;
};

struct pool_test {
        int nr_settings;
        struct alloc_settings settings[MAX_TEST_SIZES];
};

static struct pool_test test1 = { 8, { {     4, 2000 },
                                       {    30,  100 },
                                       {    50,  100 },
                                       {   100,  100 },
                                       {   500,   50 },
                                       {  1000,   10 },
                                       {  4000,    5 },
                                       {  8000,    5 } } };

/* ===================================================================== */
/**
 *   createandstart
 *
 *   @param worker     Pointer to fuction to be run as pthread.
 *
 *   @param arg        Argument to the above function.
 *
 *   @param t_id       Pointer to thread ID of the create thread.
 *
 *   @return           0 when thread is created.
 *
 *   @par Globals:     --
 *
 *   Create and start a pthread.
 */
/* ===================================================================== */
static int
createandstart(void *(*worker)(void *), void *arg, pthread_t *t_id)
{
        int res;
        pthread_attr_t t_attr;

        /* we will have them detached */
        if ((res = pthread_attr_init(&t_attr)) != 0){
                return -__LINE__;
        }

        if ((res = pthread_create(t_id, &t_attr, worker, arg)) != 0){
                return -__LINE__;
        }

        return 0;
}

int test_pool(struct pool_test *test)
{
        int i, j, k = 0;
        union itc_msg **itc_ptrs;
        int nr_ptrs = 0;

        for(i=0 ; i<test->nr_settings ; i++) {
                nr_ptrs += test->settings[i].nr;
        }

        itc_ptrs = malloc(nr_ptrs * sizeof(union itc_msg *));
        if(itc_ptrs == NULL) {
                return -__LINE__;
        }

        for(i=0 ; i<test->nr_settings ; i++) {
                for(j=0 ; j<test->settings[i].nr ; j++) {
                        itc_ptrs[k++] = itc_alloc(test->settings[i].size, j);
                }
        }

        for(i=0 ; i<nr_ptrs ; i++) {
                itc_free(&(itc_ptrs[i]));
                if(itc_ptrs[i] != NULL) {
                        return -__LINE__;
                }
        }

        free(itc_ptrs);

        return 0;
}

static itc_mbox_id_t init_malloc(int mboxes)
{
        if(itc_init(mboxes, ITC_MALLOC,
                    NULL, ITC_NO_NAMESPACE, 0) != 0) {
                return ITC_NO_ID;
        }

        return itc_create_mailbox("malloctest", 0);
}

static itc_mbox_id_t init_pool(int mboxes, int size)
{
        struct itc_pool_parameters pool_par;

        pool_par.size = size;
        if(itc_init(mboxes, ITC_POOL, (union itc_scheme *)&pool_par,
                    ITC_NO_NAMESPACE, 0) != 0) {
                return ITC_NO_ID;
        }

        return itc_create_mailbox("pooltest", 0);
}

static itc_mbox_id_t init_poolflex(int mboxes, int poolsize,
                                   int msgsize0, int msgsize1, int msgsize2, int msgsize3,
                                   int msgsize4, int msgsize5, int msgsize6, int msgsize7)
{
        struct itc_pool_flex_parameters pool_par;

        pool_par.size = poolsize;
        pool_par.msg_sizes[0] = msgsize0;
        pool_par.msg_sizes[1] = msgsize1;
        pool_par.msg_sizes[2] = msgsize2;
        pool_par.msg_sizes[3] = msgsize3;
        pool_par.msg_sizes[4] = msgsize4;
        pool_par.msg_sizes[5] = msgsize5;
        pool_par.msg_sizes[6] = msgsize6;
        pool_par.msg_sizes[7] = msgsize7;
        if(itc_init(mboxes, ITC_POOL_FLEX, (union itc_scheme *)&pool_par,
                    ITC_NO_NAMESPACE, 0) != 0) {
                return ITC_NO_ID;
        }

        return itc_create_mailbox("pooltest", 0);
}

void *userdefined_alloc(size_t size)
{
        void *tmp;

        tmp = malloc(size);
        if(tmp == NULL) {
                abort();
        }

        return tmp;
}

void userdefined_free(void *message)
{
        free(message);
}

static itc_mbox_id_t init_userdef(int mboxes)
{
        struct itc_user_defined_parameters userdef_par;

        userdef_par.alloc = userdefined_alloc;
        userdef_par.free  = userdefined_free;

        if(itc_init(mboxes, ITC_USER_DEFINED,
                    (union itc_scheme *)&userdef_par,
                    ITC_NO_NAMESPACE, 0) != 0) {
                return ITC_NO_ID;
        }

        return itc_create_mailbox("userdef_test", 0);

}

static void exit_itc(itc_mbox_id_t mbox_id)
{
        itc_delete_mailbox(mbox_id);
        itc_exit();
}

static void *pool_test_thread(void *data)
{
        struct thr_test *tst = data;
        itc_mbox_id_t mbox_id;
        char name[20];
        int i;
        long result = 0;

        sprintf(name, "pool_test_%d", tst->testno);

        mbox_id = itc_create_mailbox(name, 0);
        if(mbox_id == ITC_NO_ID) {
                result = -__LINE__;
        } else {

                for(i=0 ; i<tst->loops ; i++) {
                        result = test_pool(&test1);
                        if(result != 0) {
                                break;
                        }
                }

                if(i != tst->loops) {
                        result = -__LINE__;
                }
                itc_delete_mailbox(mbox_id);
        }

        return (void *)result;
}

static int run_conc_test(int contexts, int loops)
{
        struct thr_test *tst;
        int i, result = 0;
        pthread_t *tids;
        void *res;

        tids = malloc(contexts * sizeof(pthread_t));
        if(tids == NULL) {
                return -__LINE__;
        }

        tst = malloc(contexts * sizeof(struct thr_test));
        if(tst == NULL) {
                free(tids);
                return -__LINE__;
        }

        for(i=0 ; i<contexts ; i++) {
                tst[i].loops = loops;
                tst[i].testno = i;

                createandstart(pool_test_thread, &tst[i], &tids[i]);
        }

        for(i=0 ; i<contexts ; i++) {
                if(pthread_join(tids[i], &res) < 0) {
                        abort();
                }
                if(res != NULL) {
                        result = -1;
                }
        }

        free(tst);
        free(tids);

        return result;
}

static int concurrent_test(int contexts, int loops)
{
        itc_mbox_id_t mbox_id;
        int result;

        /* Test Malloc allocation scheme */
        mbox_id = init_malloc(contexts + 1);
        if(mbox_id == ITC_NO_ID) {
                return -__LINE__;
        }

        result = run_conc_test(contexts, loops);

        exit_itc(mbox_id);

        if(result != 0) {
                return result;
        }

        /* Test pool allocation scheme */
        mbox_id = init_pool((contexts + 1), (20*1024*1024));
        if(mbox_id == ITC_NO_ID) {
                return -__LINE__;
        }

        result = run_conc_test(contexts, loops);

        exit_itc(mbox_id);

        return result;
}

static int simple_test(void)
{
        itc_mbox_id_t mbox_id;
        int result;

        /* Test Malloc allocation scheme */
        mbox_id = init_malloc(1);
        if(mbox_id == ITC_NO_ID) {
                return -__LINE__;
        }

        result = test_pool(&test1);
        if(result != 0) {
                return result;
        }

        exit_itc(mbox_id);


        /* Test pool allocation scheme */
        mbox_id = init_pool(1, (2*1024*1024));
        if(mbox_id == ITC_NO_ID) {
                return -__LINE__;
        }

        result = test_pool(&test1);

        exit_itc(mbox_id);


        /* Test pool allocation scheme */
        mbox_id = init_poolflex(1, (2*1024*1024),
                                  16,   64,  128,  256,
                                1024, 2048, 4196, 8159);
        if(mbox_id == ITC_NO_ID) {
                return -__LINE__;
        }

        result = test_pool(&test1);

        exit_itc(mbox_id);


        /* Test user defined allocation scheme */
        mbox_id = init_userdef(1);
        if(mbox_id == ITC_NO_ID) {
                return -__LINE__;
        }

        result = test_pool(&test1);

        exit_itc(mbox_id);


        return result;
}

int msgsize_test(void)
{
        itc_mbox_id_t mbox_id;
        union itc_msg *msg;
        int result = 0;

        mbox_id = init_malloc(1);
        if(mbox_id == ITC_NO_ID) {
                return -__LINE__;
        }

        msg = itc_alloc(356, 0);

        if(itc_size(msg) != 356) {
                result = -2;
                goto out;
        }

        if(!itc_setsize(msg, 200)) {
                result = -3;
                goto out;
        }

        if(itc_size(msg) != 200) {
                result = -4;
                goto out;
        }

        if(itc_setsize(msg, 400)) {
                result = -5;
                goto out;
        }

        if(itc_size(msg) != 200) {
                result = -6;
                goto out;
        }

        if(itc_setsize(msg, 356)) {
                result = -7;
                goto out;
        }

        if(itc_size(msg) != 200) {
                result = -8;
                goto out;
        }

        if(!itc_setsize(msg, 100)) {
                result = -9;
                goto out;
        }

        if(itc_size(msg) != 100) {
                result = -10;
                goto out;
        }

out:
        exit_itc(mbox_id);

        return result;
}

int run_pooltest(void)
{
        int result;

        result = simple_test();
        if(result != 0) {
                return -result -1000;
        }

        result = concurrent_test(CONC_CONTEXTS, CONC_LOOPS);
        if(result != 0) {
                return -result -2000;
        }

        result = msgsize_test();
        if(result != 0) {
                return -result -3000;
        }

        return 0;
}

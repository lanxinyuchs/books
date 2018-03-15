#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>

#include "itc.h"
#include "itc_system.h"

#include "itc_ct.h"
#include "itc_loopthreads.h"

#define REC_TMO       15000

union itc_msg {
        uint32_t                 msgno;

        struct create_loop       create_loop;
        struct create_loop_repl  create_loop_repl;
        struct stop_loop         stop_loop;
        struct stop_loop_repl    stop_loop_repl;
        struct kill_loop         kill_loop;
        struct kill_loop_repl    kill_loop_repl;
        struct cancel_loop       cancel_loop;
        struct cancel_loop_repl  cancel_loop_repl;
        struct create_clone      create_clone;
        struct create_clone_repl create_clone_repl;
        struct delete_clone      delete_clone;
        struct delete_clone_repl delete_clone_repl;
        struct add_name          add_name;
        struct add_name_repl     add_name_repl;
        struct rem_locate        rem_locate;
};

static void create_clone(union itc_msg *msg)
{
        union itc_msg *replmsg;
        itc_mbox_id_t clone_id;
        bool success = true;

        clone_id = itc_clone_mailbox(ITC_MY_MBOX, msg->create_clone.name);
        if(clone_id == ITC_NO_ID) {
                success = false;
        }

         if(msg->create_clone.send_repl) {
                replmsg = itc_alloc(sizeof(struct create_clone_repl), CREATE_CLONE_REPL);
                replmsg->create_clone_repl.success  = success;
                replmsg->create_clone_repl.clone_id = clone_id;
                itc_send(&replmsg, itc_sender(msg), ITC_MY_MBOX);
        }
}

static void delete_clone(union itc_msg *msg)
{
        union itc_msg *replmsg;

        itc_delete_mailbox(msg->delete_clone.clone_id);

        if(msg->delete_clone.send_repl) {
                replmsg = itc_alloc(sizeof(struct delete_clone_repl), DELETE_CLONE_REPL);
                replmsg->create_clone_repl.success  = true;
                itc_send(&replmsg, itc_sender(msg), ITC_MY_MBOX);
        }
}

static void add_name(union itc_msg *msg)
{
        union itc_msg *replmsg;

        itc_add_name(itc_current_mbox(),
                     msg->add_name.name);

        if(msg->add_name.send_repl) {
                replmsg = itc_alloc(sizeof(struct add_name_repl), ADD_NAME_REPL);
                replmsg->add_name_repl.success  = true;
                itc_send(&replmsg, itc_sender(msg), ITC_MY_MBOX);
        }
}

static void remote_locate(union itc_msg *msg)
{
        msg->rem_locate.loc_mbox = itc_sender(msg);
        msg->rem_locate.rx_mbox = itc_receiver(msg);
        itc_send(&msg, msg->rem_locate.from_mbox, ITC_MY_MBOX);
}

void *loopback_thread(void *data)
{
        union itc_msg *msg;
        itc_mbox_id_t mbox_id;
        bool cont_thread = true;
        struct thread_data *thr_data = data;

        mbox_id = itc_create_mailbox(thr_data->name, 0);

        if(thr_data->have_mutex) {
                pthread_mutex_unlock(&thr_data->lock);
        } else {
                free(thr_data);
        }

        while(cont_thread) {
                msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                switch(msg->msgno) {
                case CREATE_CLONE:
                        create_clone(msg);
                        itc_free(&msg);
                        break;
                case DELETE_CLONE:
                        delete_clone(msg);
                        itc_free(&msg);
                        break;
                case ADD_NAME:
                        add_name(msg);
                        itc_free(&msg);
                        break;
                case REM_LOCATE:
                        remote_locate(msg);
                        break;
                case STOP_THREAD:
                        cont_thread = false;
                        itc_free(&msg);
                        break;
                case KILL_THREAD:
                        return NULL;
                        break;
                default:
                        itc_send(&msg, itc_sender(msg), itc_receiver(msg));
                        break;
                }
        }

        itc_delete_mailbox(mbox_id);

        return NULL;
}

int create_loopback(char *name, pthread_t *tid, itc_mbox_id_t *mbox_id)
{
        union itc_msg *msg;
        struct thread_data *thr_data;
        uint32_t loc_repl[] = { 1, LOC_BASE };

        thr_data = malloc(sizeof(struct thread_data));

        strcpy(thr_data->name, name);
        thr_data->have_mutex = false;

        pthread_create(tid, NULL, loopback_thread, thr_data);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(name, &msg, ITC_MY_MBOX);

        msg = itc_receive(loc_repl, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }

        *mbox_id = itc_sender(msg);
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(*mbox_id, &msg);

        return 0;
}

static void create_loopthread(union itc_msg *msg)
{
        union itc_msg *locmsg, *replmsg;
        struct thread_data *thr_data;
        pthread_t tid;
        bool success = true;

        thr_data = malloc(sizeof(struct thread_data));

        strcpy(thr_data->name, msg->create_loop.name);
        thr_data->have_mutex = false;

        pthread_create(&tid, NULL, loopback_thread, thr_data);

        locmsg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(msg->create_loop.name, &locmsg, ITC_MY_MBOX);
        if(locmsg != NULL) {
                success = false;
        }

        if(success) {
                locmsg = itc_receive(ITC_NOFILTER, 2000, ITC_FROM_ALL);
                if(locmsg == NULL) {
                        success = false;
                }
        }

        if(msg->create_loop.send_repl) {
                replmsg = itc_alloc(sizeof(struct create_loop_repl), CREATE_LOOP_REPL);
                replmsg->create_loop_repl.success = success;
                replmsg->create_loop_repl.tid     = tid;
                if(success) {
                        replmsg->create_loop_repl.mbox_id = itc_sender(locmsg);
                } else {
                        replmsg->create_loop_repl.mbox_id = ITC_NO_ID;
                }
                itc_send(&replmsg, itc_sender(msg), ITC_MY_MBOX);
        }

        if(success) {
                itc_free(&locmsg);
        }
}

static void stop_loopthread(union itc_msg *msg)
{
        union itc_msg *txmsg, *replmsg;
        bool success = true;

        txmsg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&txmsg, msg->stop_loop.mbox_id, ITC_MY_MBOX);

        if(msg->stop_loop.send_repl) {
                replmsg = itc_alloc(sizeof(struct stop_loop_repl), STOP_LOOP_REPL);
                replmsg->stop_loop_repl.success = success;
                itc_send(&replmsg, itc_sender(msg), ITC_MY_MBOX);
        }
}

static void kill_loopthread(union itc_msg *msg)
{
        union itc_msg *txmsg, *replmsg;
        bool success = true;

        txmsg = itc_alloc(sizeof(uint32_t), KILL_THREAD);
        itc_send(&txmsg, msg->kill_loop.mbox_id, ITC_MY_MBOX);

        if(msg->kill_loop.send_repl) {
                replmsg = itc_alloc(sizeof(struct kill_loop_repl), KILL_LOOP_REPL);
                replmsg->kill_loop_repl.success = success;
                itc_send(&replmsg, itc_sender(msg), ITC_MY_MBOX);
        }
}

static void cancel_loopthread(union itc_msg *msg)
{
        union itc_msg *replmsg;
        bool success = true;

        if(pthread_cancel(msg->cancel_loop.tid) < 0) {
                success = false;
        }

        if(msg->cancel_loop.send_repl) {
                replmsg = itc_alloc(sizeof(struct cancel_loop_repl), CANCEL_LOOP_REPL);
                replmsg->cancel_loop_repl.success = success;
                itc_send(&replmsg, itc_sender(msg), ITC_MY_MBOX);
        }
}

int procthread(char *namespace, char *name)
{
        union itc_msg *msg;
        itc_mbox_id_t mbox_id;
        bool contproc = true;

        if(itc_init(100, ITC_MALLOC, NULL, namespace, 0) != 0) {
                return -__LINE__;
        }

        mbox_id = itc_create_mailbox(name, 0);

        while(contproc) {
                msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                switch(msg->msgno) {
                case CREATE_LOOP:
                        create_loopthread(msg);
                        break;
                case STOP_LOOP:
                        stop_loopthread(msg);
                        break;
                case KILL_LOOP:
                        kill_loopthread(msg);
                        break;
                case CANCEL_LOOP:
                        cancel_loopthread(msg);
                        break;
                case KILL_PROC:
                        exit(0);
                        break;
                case LEAVE_PROC:
                        contproc = false;
                        break;
                default:
                        break;
                }
                itc_free(&msg);
        }

        itc_delete_mailbox(mbox_id);

        itc_exit();

        return 0;
}

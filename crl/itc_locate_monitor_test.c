#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>

#include <sys/types.h>
#include <signal.h>

#include "itc.h"
#include "itc_system.h"

#include "itc_ct.h"
#include "itc_loopthreads.h"

#define REC_TMO       15000
#define REC_SHORT_TMO  1000

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

static int ping_thread(itc_mbox_id_t mbox_id)
{
        union itc_msg *msg;
        int result = 0;
        msg = itc_alloc(sizeof(uint32_t), PING_THREAD);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != PING_THREAD) {
                result = -__LINE__;
        } else if(itc_sender(msg) != mbox_id) {
                result = -__LINE__;
        }
        itc_free(&msg);
        if(msg != NULL) {
                result = -__LINE__;
        }

        return result;
}

static int simple_loc_mon_test(char *name, bool locate_first, locmon_tcs tc)
{
        pthread_t tid;
        union itc_msg *msg;
        itc_mbox_id_t tar_mbox_id;
        struct thread_data *thr_data;
        static char tmp_name[ITC_NAME_MAXLEN];

        thr_data = malloc(sizeof(struct thread_data));
        if(thr_data == NULL) {
                return -__LINE__;
        }

        if(locate_first) {
                msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
                itc_locate_async(name, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        return -__LINE__;
                }
        }

        strcpy(thr_data->name, name);
        thr_data->have_mutex = true;
        pthread_mutex_init(&thr_data->lock, NULL);

        pthread_mutex_lock(&thr_data->lock);
        pthread_create(&tid, NULL, loopback_thread, thr_data);
        pthread_mutex_lock(&thr_data->lock);

        free(thr_data);

        if(!locate_first) {
                msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
                itc_locate_async(name, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        return -__LINE__;
                }
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        tar_mbox_id = itc_sender(msg);
        itc_free(&msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(tar_mbox_id, &msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        if(!itc_get_name(tar_mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                return -__LINE__;
        } else if(strcmp(name, tmp_name) != 0) {
                return -__LINE__;
        }

        if(tc == TC_MB_DELETE) {
                msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
                itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);
        } else if(tc == TC_THREAD_RETURNS) {
                msg = itc_alloc(sizeof(uint32_t), KILL_THREAD);
                itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);
        } else if(tc == TC_THREAD_CANCEL) {
                pthread_cancel(tid);
        } else {
                return -__LINE__;
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        if(itc_get_name(tar_mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                return -__LINE__;
        }

        if(pthread_join(tid, NULL) != 0) {
                return -__LINE__;
        }

        return 0;
}

static int between_proc_loc_mon_test(bool extprog,
                                     char *procname,
                                     char *threadname,
                                     bool locate_first,
                                     locmon_tcs tc)
{
        pid_t pid;
        pthread_t thread_tid;
        union itc_msg *msg;
        itc_mbox_id_t proc_mbox_id, thread_mbox_id;
        bool proc_running = false;
        int result = 0;
        static char tmp_name[ITC_NAME_MAXLEN];

        if(locate_first) {
                msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
                itc_locate_async(procname, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        result = -__LINE__;
                        goto error_out;
                }
        }

        pid = fork();

        if(pid == -1) {
                result = -__LINE__;
                goto error_out;
        } else if(pid == 0) {
                if(extprog) {
                        execlp("itcmonproc", "itcmonproc", procname, NULL);
                        printf("itcmonproc exit\n");
                } else {
                        procthread(ITC_NO_NAMESPACE, procname);
                }
                exit(0);

        }

        if(!locate_first) {
                msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
                itc_locate_async(procname, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        result = -__LINE__;
                        goto error_out;
                }
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                result = -__LINE__;
                goto error_out;
        }
        proc_mbox_id = itc_sender(msg);
        itc_free(&msg);

        proc_running = true;

        msg = itc_alloc(sizeof(struct create_loop) + strlen(threadname), CREATE_LOOP);
        msg->create_loop.send_repl = true;
        strcpy(msg->create_loop.name, threadname);
        itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != CREATE_LOOP_REPL) {
                result = -__LINE__;
                goto error_out;
        }
        if(!msg->create_loop_repl.success) {
                itc_free(&msg);
                result = -__LINE__;
                goto error_out;
        }
        thread_mbox_id = msg->create_loop_repl.mbox_id;
        thread_tid     = msg->create_loop_repl.tid;
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(thread_mbox_id, &msg);
        if(msg != NULL) {
                result = -__LINE__;
                goto error_out;
        }

        if(!itc_get_name(proc_mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                return -__LINE__;
        } else if(strcmp(procname, tmp_name) != 0) {
                return -__LINE__;
        }

        if(!itc_get_name(thread_mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                return -__LINE__;
        } else if(strcmp(threadname, tmp_name) != 0) {
                return -__LINE__;
        }

        if(tc == TC_MB_DELETE) {
                msg = itc_alloc(sizeof(struct stop_loop), STOP_LOOP);
                msg->stop_loop.send_repl = false;
                msg->stop_loop.mbox_id = thread_mbox_id;
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        } else if(tc == TC_THREAD_RETURNS) {
                msg = itc_alloc(sizeof(struct kill_loop), KILL_LOOP);
                msg->kill_loop.send_repl = false;
                msg->kill_loop.mbox_id = thread_mbox_id;
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        } else if(tc == TC_THREAD_CANCEL) {
                msg = itc_alloc(sizeof(struct cancel_loop), CANCEL_LOOP);
                msg->cancel_loop.send_repl = false;
                msg->cancel_loop.tid       = thread_tid;
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        } else if(tc == TC_KILL_PROC) {
                msg = itc_alloc(sizeof(uint32_t), KILL_PROC);
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);
                proc_running = false;

        } else if(tc == TC_LEAVE_PROC) {
                msg = itc_alloc(sizeof(uint32_t), LEAVE_PROC);
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);
                proc_running = false;

        } else {
                result = -__LINE__;
                goto error_out;
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                result = -__LINE__;
                goto error_out;
        }
        itc_free(&msg);
        if(msg != NULL) {
                result = -__LINE__;
        }


error_out:

        if(proc_running) {
                itc_monitor(proc_mbox_id, NULL);

                msg = itc_alloc(sizeof(uint32_t), LEAVE_PROC);
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

                msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL ||
                   msg->msgno != ITC_MONITOR_DEFAULT_NO) {
                        result = -__LINE__;
                }
        }

        return result;
}

static int several_new_procs_test(int howmany)
{
	int i, res = 0;

	for(i=0 ; (i<howmany) && (res == 0) ; i++) {
		res = between_proc_loc_mon_test(false, "many_remote", "target", false,
						TC_THREAD_RETURNS);
	}

	return res;
}

static int between_proc_wns_loc_mon_test(bool extprog,
                                         char *namespace,
                                         char *procname,
                                         char *threadname,
                                         bool locate_first,
                                         locmon_tcs tc)
{
        pid_t pid;
        pthread_t thread_tid;
        union itc_msg *msg;
        itc_mbox_id_t proc_mbox_id, thread_mbox_id;
        bool proc_running = false;
        int result = 0;
        char *name;
        static char tmp_name[ITC_NAME_MAXLEN];

        name = malloc(strlen(namespace) + 1 + strlen(procname) + 1);
        if(name == NULL) {
                result = -__LINE__;
                goto error_out;
        }

        strcpy(name, namespace);
        if(namespace[strlen(namespace) - 1] != '/') {
                strcat(name, "/");
        }
        strcat(name, procname);

        if(locate_first) {
                msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
                itc_locate_async(procname, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        result = -__LINE__;
                        goto error_out;
                }

                msg = itc_alloc(sizeof(uint32_t), (LOC_BASE + 1));
                itc_locate_async(name, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        result = -__LINE__;
                        goto error_out;
                }
        }

        pid = fork();

        if(pid == -1) {
                result = -__LINE__;
                goto error_out;
        } else if(pid == 0) {
                if(extprog) {
                        execlp("itcmonproc", "itcmonproc", namespace, procname, NULL);
                        printf("itcmonproc exit\n");
                } else {
                        procthread(namespace, procname);
                }
                exit(0);
        }

        if(!locate_first) {
                msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
                itc_locate_async(procname, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        result = -__LINE__;
                        goto error_out;
                }

                msg = itc_alloc(sizeof(uint32_t), (LOC_BASE + 1));
                itc_locate_async(name, &msg, ITC_MY_MBOX);
                if(msg != NULL) {
                        result = -__LINE__;
                        goto error_out;
                }
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != (LOC_BASE + 1)) {
                result = -__LINE__;
                goto error_out;
        }
        proc_mbox_id = itc_sender(msg);
        itc_free(&msg);

        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                result = -__LINE__;
                itc_free(&msg);
                goto error_out;
        }

        proc_running = true;

        if(!itc_get_name(proc_mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                return -__LINE__;
        } else if(strcmp(name, tmp_name) != 0) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(struct create_loop) + strlen(threadname), CREATE_LOOP);
        msg->create_loop.send_repl = true;
        strcpy(msg->create_loop.name, threadname);
        itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != CREATE_LOOP_REPL) {
                result = -__LINE__;
                goto error_out;
        }
        if(!msg->create_loop_repl.success) {
                itc_free(&msg);
                result = -__LINE__;
                goto error_out;
        }
        thread_mbox_id = msg->create_loop_repl.mbox_id;
        thread_tid     = msg->create_loop_repl.tid;
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(thread_mbox_id, &msg);
        if(msg != NULL) {
                result = -__LINE__;
                goto error_out;
        }

        if(tc == TC_MB_DELETE) {
                msg = itc_alloc(sizeof(struct stop_loop), STOP_LOOP);
                msg->stop_loop.send_repl = false;
                msg->stop_loop.mbox_id = thread_mbox_id;
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        } else if(tc == TC_THREAD_RETURNS) {
                msg = itc_alloc(sizeof(struct kill_loop), KILL_LOOP);
                msg->kill_loop.send_repl = false;
                msg->kill_loop.mbox_id = thread_mbox_id;
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        } else if(tc == TC_THREAD_CANCEL) {
                msg = itc_alloc(sizeof(struct cancel_loop), CANCEL_LOOP);
                msg->cancel_loop.send_repl = false;
                msg->cancel_loop.tid       = thread_tid;
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

        } else if(tc == TC_KILL_PROC) {
                msg = itc_alloc(sizeof(uint32_t), KILL_PROC);
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);
                proc_running = false;

        } else if(tc == TC_LEAVE_PROC) {
                msg = itc_alloc(sizeof(uint32_t), LEAVE_PROC);
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);
                proc_running = false;

        } else {
                result = -__LINE__;
                goto error_out;
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                result = -__LINE__;
                goto error_out;
        }
        itc_free(&msg);
        if(msg != NULL) {
                result = -__LINE__;
        }

error_out:

        if(proc_running) {
                itc_monitor(proc_mbox_id, NULL);

                msg = itc_alloc(sizeof(uint32_t), LEAVE_PROC);
                itc_send(&msg, proc_mbox_id, ITC_MY_MBOX);

                msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL ||
                   msg->msgno != ITC_MONITOR_DEFAULT_NO) {
                        result = -__LINE__;
                }

                if(itc_get_name(proc_mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                        return -__LINE__;
                }
        }

        return result;
}

static int simple_clone_test(char *mboxname, char *clonename)
{
        union itc_msg *msg;
        uint32_t del_clone_repl[] = { 1, DELETE_CLONE_REPL };
        uint32_t thread_monmsg[] = { 1, MON_BASE };
        uint32_t clone_monmsg[] = { 1, (MON_BASE + 1) };
        pthread_t tid;
        itc_mbox_id_t tar_mbox_id, clone_mbox_id;
        struct thread_data *thr_data;

        thr_data = malloc(sizeof(struct thread_data));
        if(thr_data == NULL) {
                return -__LINE__;
        }

        strcpy(thr_data->name, mboxname);
        thr_data->have_mutex = false;

        pthread_create(&tid, NULL, loopback_thread, thr_data);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname, &msg, ITC_MY_MBOX);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        tar_mbox_id = itc_sender(msg);
        itc_free(&msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(tar_mbox_id, &msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(struct create_clone) + strlen(clonename), CREATE_CLONE);
        msg->create_clone.send_repl = true;
        strcpy(msg->create_clone.name, clonename);
        itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != CREATE_CLONE_REPL) {
                return -__LINE__;
        }
        if(!msg->create_clone_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
        clone_mbox_id = msg->create_clone_repl.clone_id;
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), (MON_BASE + 1));
        itc_monitor(clone_mbox_id, &msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        if(ping_thread(tar_mbox_id) < 0) {
                return -__LINE__;
        }

        if(ping_thread(clone_mbox_id) < 0) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(struct delete_clone), DELETE_CLONE);
        msg->delete_clone.send_repl = true;
        msg->delete_clone.clone_id = clone_mbox_id;
        itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(del_clone_repl, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        if(!msg->delete_clone_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }

        msg = itc_receive(clone_monmsg, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        itc_free(&msg);


        msg = itc_alloc(sizeof(struct create_clone) + strlen(clonename), CREATE_CLONE);
        msg->create_clone.send_repl = true;
        strcpy(msg->create_clone.name, clonename);
        itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != CREATE_CLONE_REPL) {
                return -__LINE__;
        }
        if(!msg->create_clone_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
        clone_mbox_id = msg->create_clone_repl.clone_id;
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), (MON_BASE + 1));
        itc_monitor(clone_mbox_id, &msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        if(ping_thread(clone_mbox_id) < 0) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(thread_monmsg, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_receive(clone_monmsg, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        itc_free(&msg);

        if(pthread_join(tid, NULL) != 0) {
                return -__LINE__;
        }

        return 0;
}

static int simple_addname_test(char *mboxname, char *addname)
{
        union itc_msg *msg;
        uint32_t thread_monmsg[] = { 1, MON_BASE };
        uint32_t loc_msg[] = { 1, LOC_BASE };
        uint32_t addrepl_msg[] = { 1, ADD_NAME_REPL };
        pthread_t tid;
        itc_mbox_id_t mbox_id, ret_id;
        struct thread_data *thr_data;
        static char tmp_name[ITC_NAME_MAXLEN];

        thr_data = malloc(sizeof(struct thread_data));
        if(thr_data == NULL) {
                return -__LINE__;
        }

        strcpy(thr_data->name, mboxname);
        thr_data->have_mutex = false;

        pthread_create(&tid, NULL, loopback_thread, thr_data);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname, &msg, ITC_MY_MBOX);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        mbox_id = itc_sender(msg);
        itc_free(&msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(mbox_id, &msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        if(!itc_get_name(mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                return -__LINE__;
        } else if(strcmp(mboxname, tmp_name) != 0) {
                return -__LINE__;
        }

        ret_id = itc_locate(addname);
        if(ret_id != ITC_NO_ID) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(addname, &msg, ITC_MY_MBOX);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_receive(loc_msg, 100, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(struct add_name) + strlen(addname), ADD_NAME);
        msg->add_name.send_repl = true;
        strcpy(msg->add_name.name, addname);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        msg = itc_receive(addrepl_msg, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        if(!msg->add_name_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_receive(loc_msg, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        if(itc_sender(msg) != mbox_id) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);

        ret_id = itc_locate(addname);
        if(ret_id != mbox_id) {
                return -__LINE__;
        }

        if(ping_thread(mbox_id) < 0) {
                return -__LINE__;
        }

        /* Check that get name still returns the original mailbox name */
        if(!itc_get_name(mbox_id, tmp_name, ITC_NAME_MAXLEN)) {
                return -__LINE__;
        } else if(strcmp(mboxname, tmp_name) != 0) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        msg = itc_receive(thread_monmsg, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        itc_free(&msg);

        ret_id = itc_locate(addname);
        if(ret_id != ITC_NO_ID) {
                return -__LINE__;
        }

        return 0;
}

static int create_loopb_in_procthread(char *name,
				      itc_mbox_id_t proc_id,
				      itc_mbox_id_t *mbox_id,
				      pthread_t *tid)
{
	union itc_msg *msg;

        msg = itc_alloc(sizeof(struct create_loop) + strlen(name), CREATE_LOOP);
        msg->create_loop.send_repl = true;
        strcpy(msg->create_loop.name, name);
        itc_send(&msg, proc_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != CREATE_LOOP_REPL) {
		return -__LINE__;
        }
        if(!msg->create_loop_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
	if(mbox_id != NULL)
		*mbox_id = msg->create_loop_repl.mbox_id;
	if(tid != NULL)
		*tid     = msg->create_loop_repl.tid;
        itc_free(&msg);

	return 0;
}

static int add_name(itc_mbox_id_t mbox_id, char *name)
{
	union itc_msg *msg;
        uint32_t addrepl_msg[] = { 1, ADD_NAME_REPL };

        msg = itc_alloc(sizeof(struct add_name) + strlen(name), ADD_NAME);
        msg->add_name.send_repl = true;
        strcpy(msg->add_name.name, name);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        msg = itc_receive(addrepl_msg, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        if(!msg->add_name_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);

	return 0;
}

static itc_mbox_id_t async_locate(char *name)
{
	union itc_msg *msg;
	uint32_t locmsg[] = { 1, ITC_LOCATE_DEFAULT_NO };
	itc_mbox_id_t target;

	itc_locate_async(name, NULL, ITC_MY_MBOX);
	msg = itc_receive(locmsg, REC_SHORT_TMO, ITC_FROM_ALL);
	if(msg == NULL)
		return ITC_NO_ID;
	target = itc_sender(msg);
	itc_free(&msg);

	return target;
}

static int many_addname_test(char *mboxname, char *addname, int nr)
{
        union itc_msg *msg;
	itc_mbox_id_t proc_id, loop_id, tmp_id;
	char tmpname[ITC_NAME_MAXLEN], procname[] = "testproc";
	int i, result = 0;
	pid_t pid = 0;

        pid = fork();

        if(pid == -1) {
                result = -__LINE__;
                goto error_out;
        } else if(pid == 0) {
		procthread(ITC_NO_NAMESPACE, procname);
                exit(0);
        }

	msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
	itc_locate_async(procname, &msg, ITC_MY_MBOX);
	if(msg != NULL) {
		result = -__LINE__;
		goto error_out;
	}
        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
		result = -__LINE__;
		goto error_out;
        }
	proc_id = itc_sender(msg);
	itc_free(&msg);

	result = create_loopb_in_procthread(mboxname, proc_id,
					    &loop_id, NULL);
	if(result < 0) {
		result = -__LINE__;
		goto error_out;
        }

	for(i=0 ; i<nr ; i++) {
		sprintf(tmpname, "%s_%d", addname, i);
		result = add_name(loop_id, tmpname);
		if(result < 0) {
			result = -__LINE__;
			goto error_out;
		}
	}

	tmp_id = async_locate(mboxname);
	if(tmp_id != loop_id) {
		result = -__LINE__;
		goto error_out;
	}

	for(i=0 ; i<nr ; i++) {
		sprintf(tmpname, "%s_%d", addname, i);
		tmp_id = async_locate(tmpname);
		if(tmp_id != loop_id) {
			result = -__LINE__;
			goto error_out;
		}
	}

	(void)itc_monitor(proc_id, NULL);
	kill(pid, SIGKILL);
	pid =  0;
	msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
	if(msg == NULL) {
		result = -__LINE__;
		goto error_out;
	}
	itc_free(&msg);

	tmp_id = itc_locate(mboxname);
	if(tmp_id != ITC_NO_ID) {
		result = -__LINE__;
		goto error_out;
	}

	for(i=0 ; i<nr ; i++) {
		sprintf(tmpname, "%s_%d", addname, i);
		tmp_id = itc_locate(tmpname);
		if(tmp_id != ITC_NO_ID) {
			result = -__LINE__;
			goto error_out;
		}
	}

error_out:
	if(pid != 0)
		kill(pid, SIGKILL);

        return result;
}

static int dualsame_addname_test(char *mboxname, char *addname)
{
        union itc_msg *msg;
	itc_mbox_id_t proc_id, loop_id, tmp_id;
	char procname[] = "testproc";
	int result = 0;
	pid_t pid = 0;

        pid = fork();

        if(pid == -1) {
                result = -__LINE__;
                goto error_out;
        } else if(pid == 0) {
		procthread(ITC_NO_NAMESPACE, procname);
                exit(0);
        }

	msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
	itc_locate_async(procname, &msg, ITC_MY_MBOX);
	if(msg != NULL) {
		result = -__LINE__;
		goto error_out;
	}
        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
		result = -__LINE__;
		goto error_out;
        }
	proc_id = itc_sender(msg);
	itc_free(&msg);

	result = create_loopb_in_procthread(mboxname, proc_id,
					    &loop_id, NULL);
	if(result < 0) {
		result = -__LINE__;
		goto error_out;
        }

	result = add_name(loop_id, addname);
	if(result < 0) {
		result = -__LINE__;
		goto error_out;
        }

	result = add_name(loop_id, addname);
	if(result < 0) {
		result = -__LINE__;
		goto error_out;
        }

	tmp_id = async_locate(mboxname);
	if(tmp_id != loop_id) {
		result = -__LINE__;
		goto error_out;
        }

	tmp_id = async_locate(addname);
	if(tmp_id != loop_id) {
		result = -__LINE__;
		goto error_out;
        }

	(void)itc_monitor(proc_id, NULL);
	kill(pid, SIGKILL);
	pid =  0;
	msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
	if(msg == NULL) {
		result = -__LINE__;
		goto error_out;
	}
	itc_free(&msg);

	tmp_id = async_locate(addname);
	if(tmp_id != ITC_NO_ID) {
		result = -__LINE__;
		goto error_out;
        }

error_out:
	if(pid != 0)
		kill(pid, SIGKILL);

        return result;
}

static int simple_monitor_test(char *mboxname)
{
        union itc_msg *msg;
        pthread_t tid;
        itc_mbox_id_t tar_mbox_id;
        struct thread_data *thr_data1, *thr_data2;
        itc_monitor_id_t mon_id;

        thr_data1 = malloc(sizeof(struct thread_data));
        if(thr_data1 == NULL) {
                return -__LINE__;
        }

        strcpy(thr_data1->name, mboxname);
        thr_data1->have_mutex = false;

        pthread_create(&tid, NULL, loopback_thread, thr_data1);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        tar_mbox_id = itc_sender(msg);
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        mon_id = itc_monitor(tar_mbox_id, &msg);
        if(msg != NULL) {
                return -__LINE__;
        }
        itc_unmonitor(mon_id);

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        thr_data2 = malloc(sizeof(struct thread_data));
        if(thr_data2 == NULL) {
                return -__LINE__;
        }
        strcpy(thr_data2->name, mboxname);
        thr_data2->have_mutex = false;

        pthread_create(&tid, NULL, loopback_thread, thr_data2);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        tar_mbox_id = itc_sender(msg);
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        mon_id = itc_monitor(tar_mbox_id, &msg);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, tar_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);
        itc_unmonitor(mon_id);

        if(pthread_join(tid, NULL) != 0) {
                return -__LINE__;
        }

        return 0;
}

static int simple_duplmboxname_test(char *mboxname)
{
        union itc_msg *msg;
        pthread_t tid1, tid2;
        itc_mbox_id_t mbox_id1, mbox_id2;
        struct thread_data *thr_data1, *thr_data2;

        thr_data1 = malloc(sizeof(struct thread_data));
        if(thr_data1 == NULL) {
                return -__LINE__;
        }

        strcpy(thr_data1->name, mboxname);
        thr_data1->have_mutex = false;

        pthread_create(&tid1, NULL, loopback_thread, thr_data1);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        mbox_id1 = itc_sender(msg);
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(mbox_id1, &msg);

        thr_data2 = malloc(sizeof(struct thread_data));
        if(thr_data2 == NULL) {
                return -__LINE__;
        }

        strcpy(thr_data2->name, mboxname);
        thr_data2->have_mutex = false;

        pthread_create(&tid2, NULL, loopback_thread, thr_data2);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        if(itc_sender(msg) != mbox_id1) {
                return -__LINE__;
                itc_free(&msg);
        }
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }
        mbox_id2 = itc_sender(msg);
        if(mbox_id2 == mbox_id1) {
                return -__LINE__;
                itc_free(&msg);
        }
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(mbox_id2, &msg);

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id2, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);

        return 0;
}

static int simple_locatefrom_test(char *mboxname, char *locname, char *clonename)
{
        union itc_msg *msg;
        pthread_t tid, loctid;
        itc_mbox_id_t mbox_id, loc_id, clone_id;

        if(create_loopback(mboxname, &tid, &mbox_id) < 0) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(struct rem_locate), REM_LOCATE);
        msg->rem_locate.loc_mbox  = ITC_NO_ID;
        msg->rem_locate.rx_mbox   = ITC_NO_ID;
        msg->rem_locate.from_mbox = itc_current_mbox();
        itc_locate_async(locname, &msg, mbox_id);

        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        if(create_loopback(locname, &loctid, &loc_id) < 0) {
                return -__LINE__;
        }

        msg = itc_receive(ITC_NOFILTER, 100, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != REM_LOCATE) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.loc_mbox != loc_id) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.rx_mbox != mbox_id) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_alloc(sizeof(struct rem_locate), REM_LOCATE);
        msg->rem_locate.loc_mbox  = ITC_NO_ID;
        msg->rem_locate.rx_mbox   = ITC_NO_ID;
        msg->rem_locate.from_mbox = itc_current_mbox();
        itc_locate_async(locname, &msg, mbox_id);

        msg = itc_receive(ITC_NOFILTER, 10, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != REM_LOCATE) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.loc_mbox != loc_id) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.rx_mbox != mbox_id) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, loc_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);

        /* Test locate from cloned mailbox */
        msg = itc_alloc(sizeof(struct create_clone) + strlen(clonename), CREATE_CLONE);
        msg->create_clone.send_repl = true;
        strcpy(msg->create_clone.name, clonename);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != CREATE_CLONE_REPL) {
                return -__LINE__;
        }
        if(!msg->create_clone_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
        clone_id = msg->create_clone_repl.clone_id;
        itc_free(&msg);

        msg = itc_alloc(sizeof(struct rem_locate), REM_LOCATE);
        msg->rem_locate.loc_mbox  = ITC_NO_ID;
        msg->rem_locate.rx_mbox   = ITC_NO_ID;
        msg->rem_locate.from_mbox = itc_current_mbox();
        itc_locate_async(locname, &msg, clone_id);

        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        if(create_loopback(locname, &loctid, &loc_id) < 0) {
                return -__LINE__;
        }

        msg = itc_receive(ITC_NOFILTER, 10, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != REM_LOCATE) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.loc_mbox != loc_id) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.rx_mbox != clone_id) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_alloc(sizeof(struct rem_locate), REM_LOCATE);
        msg->rem_locate.loc_mbox  = ITC_NO_ID;
        msg->rem_locate.rx_mbox   = ITC_NO_ID;
        msg->rem_locate.from_mbox = itc_current_mbox();
        itc_locate_async(locname, &msg, clone_id);

        msg = itc_receive(ITC_NOFILTER, 10, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != REM_LOCATE) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.loc_mbox != loc_id) {
                itc_free(&msg);
                return -__LINE__;
        } else if(msg->rem_locate.rx_mbox != clone_id) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);


        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, loc_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);

        return 0;
}

static int simple_dupladdname_test(char *mboxname1,
                                   char *mboxname2,
                                   char *addname)
{
        union itc_msg *msg;
        pthread_t tid1, tid2;
        itc_mbox_id_t mbox_id1, mbox_id2, ret_id;
        struct thread_data *thr_data1, *thr_data2;

        thr_data1 = malloc(sizeof(struct thread_data));
        if(thr_data1 == NULL) {
                return -__LINE__;
        }

        strcpy(thr_data1->name, mboxname1);
        thr_data1->have_mutex = false;

        pthread_create(&tid1, NULL, loopback_thread, thr_data1);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname1, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        mbox_id1 = itc_sender(msg);
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), MON_BASE);
        itc_monitor(mbox_id1, &msg);


        thr_data2 = malloc(sizeof(struct thread_data));
        if(thr_data2 == NULL) {
                return -__LINE__;
        }
        strcpy(thr_data2->name, mboxname2);
        thr_data2->have_mutex = false;

        pthread_create(&tid2, NULL, loopback_thread, thr_data2);

        msg = itc_alloc(sizeof(uint32_t), LOC_BASE);
        itc_locate_async(mboxname2, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != LOC_BASE) {
                return -__LINE__;
        }

        mbox_id2 = itc_sender(msg);
        itc_free(&msg);

        msg = itc_alloc(sizeof(uint32_t), (MON_BASE + 1));
        itc_monitor(mbox_id2, &msg);


        msg = itc_alloc(sizeof(struct add_name) + strlen(addname), ADD_NAME);
        msg->add_name.send_repl = true;
        strcpy(msg->add_name.name, addname);
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != ADD_NAME_REPL) {
                return -__LINE__;
        }
        if(!msg->add_name_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);


        msg = itc_alloc(sizeof(struct add_name) + strlen(addname), ADD_NAME);
        msg->add_name.send_repl = true;
        strcpy(msg->add_name.name, addname);
        itc_send(&msg, mbox_id2, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != ADD_NAME_REPL) {
                return -__LINE__;
        }
        if(!msg->add_name_repl.success) {
                itc_free(&msg);
                return -__LINE__;
        }
        itc_free(&msg);


        ret_id = itc_locate(addname);
        if(ret_id != mbox_id1) {
                return -__LINE__;
        }


        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != MON_BASE) {
                return -__LINE__;
        }
        itc_free(&msg);


        ret_id = itc_locate(addname);
        if(ret_id != mbox_id2) {
                return -__LINE__;
        }


        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id2, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != (MON_BASE + 1)) {
                return -__LINE__;
        }
        itc_free(&msg);


        ret_id = itc_locate(addname);
        if(ret_id != ITC_NO_ID) {
                return -__LINE__;
        }

        return 0;
}

int loc_mon_test(void)
{
        int res;
        itc_mbox_id_t mbox_id;

        if(itc_init(4, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) != 0) {
                return -1;
        }

        mbox_id = itc_create_mailbox("locmon_test", 0);

        res = simple_loc_mon_test("locmon_target", true, TC_MB_DELETE);
        if(res != 0) {
                return res - 1000;
        }
        res = simple_loc_mon_test("locmon_target", false, TC_MB_DELETE);
        if(res != 0) {
                return res - 2000;
        }
        res = simple_loc_mon_test("locmon_target", true, TC_THREAD_RETURNS);
        if(res != 0) {
                return res - 3000;
        }
        res = simple_loc_mon_test("locmon_target", false, TC_THREAD_RETURNS);
        if(res != 0) {
                return res - 4000;
        }
        res = simple_loc_mon_test("locmon_target", true, TC_THREAD_CANCEL);
        if(res != 0) {
                return res - 5000;
        }
        res = simple_loc_mon_test("locmon_target", false, TC_THREAD_CANCEL);
        if(res != 0) {
                return res - 6000;
        }

        res = between_proc_loc_mon_test(true, "rem_proc", "remmon_target",
                                        true, TC_MB_DELETE);
        if(res != 0) {
                return res - 7000;
        }
        res = between_proc_loc_mon_test(true, "rem_proc", "remmon_target",
                                        false, TC_MB_DELETE);
        if(res != 0) {
                return res - 8000;
        }
        res = between_proc_loc_mon_test(true, "rem_proc", "remmon_target",
                                        true, TC_THREAD_RETURNS);
        if(res != 0) {
                return res - 9000;
        }
        res = between_proc_loc_mon_test(true, "rem_proc", "remmon_target",
                                        false, TC_THREAD_RETURNS);
        if(res != 0) {
                return res - 10000;
        }
        res = between_proc_loc_mon_test(true, "rem_proc", "remmon_target",
                                        true, TC_THREAD_CANCEL);
        if(res != 0) {
                return res - 11000;
        }
        res = between_proc_loc_mon_test(true, "rem_proc", "remmon_target",
                                        false, TC_THREAD_CANCEL);
        if(res != 0) {
                return res - 12000;
        }

	/* Test repeatedly creating new itc programs! */



        res = between_proc_loc_mon_test(false, "rem_proc", "remmon_target",
                                        true, TC_MB_DELETE);
        if(res != 0) {
                return res - 13000;
        }
        res = between_proc_loc_mon_test(false, "rem_proc", "remmon_target",
                                        false, TC_MB_DELETE);
        if(res != 0) {
                return res - 14000;
        }
        res = between_proc_loc_mon_test(false, "rem_proc", "remmon_target",
                                        true, TC_THREAD_RETURNS);
        if(res != 0) {
                return res - 15000;
        }
        res = between_proc_loc_mon_test(false, "rem_proc", "remmon_target",
                                        false, TC_THREAD_RETURNS);
        if(res != 0) {
                return res - 16000;
        }
        res = between_proc_loc_mon_test(false, "rem_proc", "remmon_target",
                                        true, TC_THREAD_CANCEL);
        if(res != 0) {
                return res - 17000;
        }
        res = between_proc_loc_mon_test(false, "rem_proc", "remmon_target",
                                        false, TC_THREAD_CANCEL);
        if(res != 0) {
                return res - 18000;
        }


        res = between_proc_wns_loc_mon_test(true, "itc_proc", "rem_ns_proc", "remmon_target",
                                            true, TC_MB_DELETE);
        if(res != 0) {
                return res - 19000;
        }
        res = between_proc_wns_loc_mon_test(true, "itc_proc", "rem_ns_proc", "remmon_target",
                                            false, TC_MB_DELETE);
        if(res != 0) {
                return res - 20000;
        }

        res = between_proc_wns_loc_mon_test(false, "itc_proc", "rem_ns_proc", "remmon_target",
                                            true, TC_MB_DELETE);
        if(res != 0) {
                return res - 21000;
        }
        res = between_proc_wns_loc_mon_test(false, "itc_proc", "rem_ns_proc", "remmon_target",
                                            false, TC_MB_DELETE);
        if(res != 0) {
                return res - 22000;
        }

	res = several_new_procs_test(500);
        if(res != 0) {
                return res - 23000;
        }

        res = simple_clone_test("main_mbox", "cloned_mbox");
        if(res != 0) {
                return res - 24000;
        }

        res = simple_addname_test("main_mbox", "added_name");
        if(res != 0) {
                return res - 25000;
        }

	res = many_addname_test("main_mbox2", "added_manyname", 10);
        if(res != 0) {
                return res - 26000;
        }

        res = dualsame_addname_test("main_mbox2", "added_name2");
        if(res != 0) {
                return res - 27000;
        }

	res = simple_monitor_test("monitored_mbox");
        if(res != 0) {
                return res - 28000;
        }

        res = simple_duplmboxname_test("duplicated_mbox");
        if(res != 0) {
                return res - 29000;
        }

        res = simple_dupladdname_test("mbox1", "mbox2", "duplicated_name");
        if(res != 0) {
                return res - 30000;
        }

        res = simple_locatefrom_test("mbox", "locatedmbox", "clonedmbox");
        if(res != 0) {
                return res - 31000;
        }

        itc_delete_mailbox(mbox_id);

        itc_exit();

        return 0;
}

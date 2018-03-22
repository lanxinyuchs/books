#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>

#include "itc.h"
#include "itc_system.h"
#include "itc_ct.h"

#define REC_TMO       15000
#define REC_SHORT_TMO  1000

#define LNH_THREAD_NAME "lnh_thread"

union itc_msg {
        uint32_t                    msgno;

        struct ass_lnh              ass_lnh;
        struct lnh_addrem_mbox      lnh_addrem_mbox;
        struct lnh_repl             lnh_repl;
        struct lnh_testmsg          lnh_testmsg;

        struct itc_locate_lnh       itc_locate_lnh;
        struct itc_locate_lnh_reply itc_locate_lnh_reply;

	struct itc_lnh_added        itc_lnh_added;
	struct itc_lnh_removed      itc_lnh_removed;
};

struct lnh_mbox {
        struct lnh_mbox *next;

        struct lnh      *lnh;
        itc_mbox_id_t    mbox_id;
        char             name[1];
};

struct lnh {
        struct lnh      *next;
        struct lnh_mbox *list_mboxes;
        char             lnhname[1];
};

struct unres_mbox {
        struct unres_mbox *next;

        itc_mbox_id_t      resp_mbox;
        itc_mbox_id_t      from;
        char               name[1];
};

struct lnh_inst {
        struct lnh        *list_lnh;
        struct unres_mbox *list_unresmboxes;
        itc_mbox_id_t      mbox_id;
};

struct testinst {
        char            *name;
        itc_mbox_id_t    mbox_id;
        itc_monitor_id_t mon_id;
};

static struct lnh_inst *li = NULL;

static void assign_lnh(char *lnhname, itc_mbox_id_t rsp_mbox)
{
        struct lnh *lnh;
        union itc_msg *msg;
        int result = 0;

        lnh = malloc(sizeof(struct lnh) + strlen(lnhname));
        if(lnh == NULL) {
                result = -__LINE__;
                goto out;
        }
        lnh->list_mboxes = NULL;
        strcpy(lnh->lnhname, lnhname);

        itc_assign_linkhandler(lnhname, li->mbox_id);

        lnh->next = li->list_lnh;
        li->list_lnh = lnh;

out:
        msg = itc_alloc(sizeof(struct lnh_repl), ASS_LNH_REPL);
        msg->lnh_repl.result = result;
        itc_send(&msg, rsp_mbox, ITC_MY_MBOX);
}

static void deassign_lnh(char *lnhname, itc_mbox_id_t rsp_mbox)
{
        struct lnh *lnh, *prev = NULL;
        union itc_msg *msg;
        int result = 0;

        for(lnh = li->list_lnh ;
            lnh != NULL        ;
            lnh = lnh->next) {
                if(strcmp(lnhname, lnh->lnhname) == 0) {
                        break;
                }
                prev = lnh;
        }
        if(lnh == NULL) {
                result = -__LINE__;
                goto out;
        }

        if(prev == NULL) {
                li->list_lnh = lnh->next;
        } else {
                prev->next = lnh->next;
        }
        free(lnh);

        itc_deassign_linkhandler(lnhname, li->mbox_id);

out:
        msg = itc_alloc(sizeof(struct lnh_repl), DEASS_LNH_REPL);
        msg->lnh_repl.result = result;
        itc_send(&msg, rsp_mbox, ITC_MY_MBOX);
}

static void add_lnh_mbox(char *name, itc_mbox_id_t rsp_mbox)
{
        struct lnh *lnh;
        struct lnh_mbox *lnh_mbox;
        struct unres_mbox *unres_mbox, *prev=NULL, *tofree;
        char *tmp;
        union itc_msg *msg;
        int result = 0;

        tmp = strchr(name, '/');
        if(tmp == NULL) {
                result = -__LINE__;
                goto out;
        }
        *tmp = '\0';

        for(lnh = li->list_lnh ; lnh != NULL ; lnh = lnh->next) {
                if(strcmp(lnh->lnhname, name) == 0) {
                        break;
                }
        }
        *tmp = '/';

        if(lnh == NULL) {
                result = -__LINE__;
                goto out;
        }

        lnh_mbox = malloc(sizeof(struct lnh_mbox) + strlen(name));
        if(lnh_mbox == NULL) {
                result = -__LINE__;
                goto out;
        }
        lnh_mbox->lnh = lnh;
        lnh_mbox->mbox_id = ITC_NO_ID;
        strcpy(lnh_mbox->name, name);
        lnh_mbox->next = lnh->list_mboxes;
        lnh->list_mboxes = lnh_mbox;

        unres_mbox = li->list_unresmboxes;

        while(unres_mbox != NULL) {
                if(strcmp(unres_mbox->name, name) == 0) {
                        tofree = NULL;

                        if(lnh_mbox->mbox_id == ITC_NO_ID) {
                                lnh_mbox->mbox_id = itc_clone_mailbox(ITC_MY_MBOX,
                                                                      name);
                                /* Send response on locate request */
                                msg = itc_alloc((sizeof(struct itc_locate_lnh_reply) + strlen(name)),
                                                ITC_LOCATE_LNH_REPLY);
                                msg->itc_locate_lnh_reply.mbox_id = lnh_mbox->mbox_id;
                                msg->itc_locate_lnh_reply.found = true;
                                strcpy(msg->itc_locate_lnh_reply.name, name);
                                itc_send(&msg, unres_mbox->resp_mbox, ITC_MY_MBOX);

                                tofree = unres_mbox;
                        }

                        if(prev == NULL) {
                                li->list_unresmboxes = unres_mbox->next;
                        } else {
                                prev->next = unres_mbox->next;
                        }

                        unres_mbox = unres_mbox->next;

                        if(tofree != NULL) {
                                free(tofree);
                        }
                } else {
                        prev = unres_mbox;
                        unres_mbox = unres_mbox->next;
                }
        }

out:
        msg = itc_alloc(sizeof(struct lnh_repl), ADD_LNH_MBOX_REPL);
        msg->lnh_repl.result = result;
        itc_send(&msg, rsp_mbox, ITC_MY_MBOX);
}

static void rem_lnh_mbox(char *name, itc_mbox_id_t rsp_mbox)
{
        struct lnh *lnh;
        struct lnh_mbox *lnh_mbox, *prev = NULL;
        union itc_msg *msg;
        char *tmp;
        int result = 0;

        tmp = strchr(name, '/');
        if(tmp == NULL) {
                result = -__LINE__;
                goto out;
        }
        *tmp = '\0';

        for(lnh = li->list_lnh ; lnh != NULL ; lnh = lnh->next) {
                if(strcmp(lnh->lnhname, name) == 0) {
                        break;
                }
        }

        if(lnh == NULL) {
                result = -__LINE__;
                goto out;
        }

        *tmp = '/';
        for(lnh_mbox = lnh->list_mboxes ;
            lnh_mbox != NULL            ;
            lnh_mbox = lnh_mbox->next) {
                if(strcmp(lnh_mbox->name, name) == 0) {
                        break;
                }
                prev = lnh_mbox;
        }

        if(lnh_mbox == NULL) {
                result = -__LINE__;
                goto out;
        }

        if(lnh_mbox->mbox_id != ITC_NO_ID) {
                itc_delete_mailbox(lnh_mbox->mbox_id);
        }
        if(prev != NULL) {
                prev->next = lnh_mbox->next;
        } else {
                lnh->list_mboxes = lnh_mbox->next;
        }

        free(lnh_mbox);

out:
        msg = itc_alloc(sizeof(struct lnh_repl), REM_LNH_MBOX_REPL);
        msg->lnh_repl.result = result;
        itc_send(&msg, rsp_mbox, ITC_MY_MBOX);
}

static void locate_lnh(char *lnhname,
                       itc_mbox_id_t from,
                       itc_mbox_id_t resp_mbox)
{
        struct lnh *lnh;
        struct lnh_mbox *lnh_mbox;
        char *tmp;
        union itc_msg *msg;

        tmp = strchr(lnhname, '/');
        if(tmp == NULL) {
                return;
        }
        *tmp = '\0';

        for(lnh = li->list_lnh ; lnh != NULL ; lnh = lnh->next) {
                if(strcmp(lnh->lnhname, lnhname) == 0) {
                        break;
                }
        }
        *tmp = '/';

        if(lnh == NULL) {
                return;
        }

        for(lnh_mbox = lnh->list_mboxes ;
            lnh_mbox != NULL            ;
            lnh_mbox = lnh_mbox->next) {
                if(strcmp(lnh_mbox->name, lnhname) == 0) {
                        break;
                }
        }

        if(lnh_mbox == NULL) {
                struct unres_mbox *unres_mbox;

                unres_mbox = malloc(sizeof(struct unres_mbox) + strlen(lnhname));
                if(unres_mbox == NULL) {
                        return;
                }
                strcpy(unres_mbox->name, lnhname);
                unres_mbox->resp_mbox = resp_mbox;
                unres_mbox->from = from;
                unres_mbox->next = li->list_unresmboxes;
                li->list_unresmboxes = unres_mbox;
        } else {
                if(lnh_mbox->mbox_id == ITC_NO_ID) {
                        lnh_mbox->mbox_id = itc_clone_mailbox(ITC_MY_MBOX, lnhname);
                }

                msg = itc_alloc((sizeof(struct itc_locate_lnh_reply) + strlen(lnhname)),
                                ITC_LOCATE_LNH_REPLY);
                msg->itc_locate_lnh_reply.mbox_id = lnh_mbox->mbox_id;
                msg->itc_locate_lnh_reply.found = true;
                strcpy(msg->itc_locate_lnh_reply.name, lnhname);
                itc_send(&msg, resp_mbox, ITC_MY_MBOX);
       }
}

static void *lnh_thread(void *data)
{
        union itc_msg *msg;
        char *name = (char *)data;

        li = malloc(sizeof(struct lnh_inst) + strlen(name));
        if(li == NULL) {
                return NULL;
        }
        memset(li, 0, (sizeof(struct lnh_inst) + strlen(name)));
        li->mbox_id = itc_create_mailbox(name, 0);
        li->list_lnh = NULL;

        for(;;) {
                msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                if(itc_receiver(msg) == li->mbox_id) {
                        switch(msg->msgno) {
                                case ASS_LNH:
                                        assign_lnh(msg->ass_lnh.lnhname,
                                                   itc_sender(msg));
                                        break;
                                case DEASS_LNH:
                                        deassign_lnh(msg->ass_lnh.lnhname,
                                                     itc_sender(msg));
                                        break;
                                case ADD_LNH_MBOX:
                                        add_lnh_mbox(msg->lnh_addrem_mbox.name,
                                                     itc_sender(msg));
                                        break;
                                case REM_LNH_MBOX:
                                        rem_lnh_mbox(msg->lnh_addrem_mbox.name,
                                                     itc_sender(msg));
                                        break;
                                case ITC_LOCATE_LNH:
                                        locate_lnh(msg->itc_locate_lnh.name,
                                                   msg->itc_locate_lnh.from,
                                                   itc_sender(msg));
                                        break;
                                default:

                                        break;
                        }
                        itc_free(&msg);
                } else {
                        itc_send(&msg, itc_sender(msg), itc_receiver(msg));
                }
        }
}

static int add_lnh_tombox(itc_mbox_id_t lnh_mbox_id, char *lnhpath)
{
        union itc_msg *msg;
        uint32_t resp_msg[] = { 1, ASS_LNH_REPL };
        int result;

        msg = itc_alloc((sizeof(struct ass_lnh) + strlen(lnhpath)),
                        ASS_LNH);
        strcpy(msg->ass_lnh.lnhname, lnhpath);
        itc_send(&msg, lnh_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(resp_msg, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        result = msg->lnh_repl.result;

        itc_free(&msg);
        return result;
}

static int rem_lnh_frommbox(itc_mbox_id_t lnh_mbox_id, char *lnhpath)
{
        union itc_msg *msg;
        uint32_t resp_msg[] = { 1, DEASS_LNH_REPL };
        int result;

        msg = itc_alloc((sizeof(struct ass_lnh) + strlen(lnhpath)),
                        DEASS_LNH);
        strcpy(msg->ass_lnh.lnhname, lnhpath);
        itc_send(&msg, lnh_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(resp_msg, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        result = msg->lnh_repl.result;
        itc_free(&msg);

        return result;
}

static int add_mbox_tolnh(itc_mbox_id_t lnh_mbox_id, char *name)
{
        union itc_msg *msg;
        uint32_t resp_msg[] = { 1, ADD_LNH_MBOX_REPL };
        int result;

        msg = itc_alloc((sizeof(struct lnh_addrem_mbox) + strlen(name)),
                        ADD_LNH_MBOX);
        strcpy(msg->lnh_addrem_mbox.name, name);
        itc_send(&msg, lnh_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(resp_msg, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        result = msg->lnh_repl.result;
        itc_free(&msg);

        return result;
}

static int rem_mbox_fromlnh(itc_mbox_id_t lnh_mbox_id, char *name)
{
        union itc_msg *msg;
        uint32_t resp_msg[] = { 1, REM_LNH_MBOX_REPL };
        int result;

        msg = itc_alloc((sizeof(struct lnh_addrem_mbox) + strlen(name)),
                        REM_LNH_MBOX);
        strcpy(msg->lnh_addrem_mbox.name, name);
        itc_send(&msg, lnh_mbox_id, ITC_MY_MBOX);

        msg = itc_receive(resp_msg, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        result = msg->lnh_repl.result;
        itc_free(&msg);

        return result;
}

static int basic_lnh_test(itc_mbox_id_t  lnh_mbox_id,
                          char          *name,
                          int            nr_links,
                          int            nr_msg)
{
        union itc_msg *msg;
        int i, j, result = 0;
        struct testinst *ti;

        ti = malloc(nr_links * sizeof(struct testinst));
        if(ti == NULL) {
                return -__LINE__;
        }
        memset(ti, 0, (nr_links * sizeof(struct testinst)));

        for(i=0 ; i<nr_links ; i++) {
                ti[i].name = malloc(strlen(name) + 4);
                if(ti[i].name == NULL) {
                        result = -__LINE__;
                        goto out;
                }
                sprintf(ti[i].name, "%s_%02d", name, i);
                result = add_mbox_tolnh(lnh_mbox_id, ti[i].name);
                if(result != 0) {
                        result = -__LINE__;
                        goto out;
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                itc_locate_async(ti[i].name, NULL, ITC_MY_MBOX);
                msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        result = -__LINE__;
                        goto out;
                } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                        itc_free(&msg);
                        result = -__LINE__;
                        goto out;
                }
                ti[i].mbox_id = itc_sender(msg);
                itc_free(&msg);

                itc_monitor(ti[i].mbox_id, NULL);
        }

        for(i=0 ; i<nr_links ; i++) {
                for(j=0 ; j<nr_msg ; j++) {
                        msg = itc_alloc(sizeof(uint32_t), LNH_TESTMSG);
                        msg->lnh_testmsg.seq_no = j;
                        itc_send(&msg, ti[i].mbox_id, ITC_MY_MBOX);
                }
                for(j=0 ; j<nr_msg ; j++) {
                        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                        if(msg == NULL) {
                                result = -__LINE__;
                                goto out;
                        } else if(msg->msgno != LNH_TESTMSG    ||
                                  msg->lnh_testmsg.seq_no != j ||
                                  itc_sender(msg) != ti[i].mbox_id) {
                                itc_free(&msg);
                                result = -__LINE__;
                                goto out;
                        }
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                result = rem_mbox_fromlnh(lnh_mbox_id, ti[i].name);
                if(result != 0) {
                        result = -__LINE__;
                        goto out;
                }

                msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        result = -__LINE__;
                        goto out;
                } else if(msg->msgno != ITC_MONITOR_DEFAULT_NO) {
                        itc_free(&msg);
                        result = -__LINE__;
                        goto out;
                }
                itc_free(&msg);
        }

out:
        for(i=0 ; i<nr_links ; i++) {
                if(ti[i].name != NULL) {
                        free(ti[i].name);
                }
        }

        free(ti);

        return result;
}

static int lnh_test_loc_add_nomon(itc_mbox_id_t  lnh_mbox_id,
                                  char          *name,
                                  int            nr_links,
                                  int            nr_msg)
{
        union itc_msg *msg;
        int i, j, result = 0;
        struct testinst *ti;

        ti = malloc(nr_links * sizeof(struct testinst));
        if(ti == NULL) {
                return -__LINE__;
        }
        memset(ti, 0, (nr_links * sizeof(struct testinst)));

        for(i=0 ; i<nr_links ; i++) {
                ti[i].name = malloc(strlen(name) + 4);
                if(ti[i].name == NULL) {
                        result = -__LINE__;
                        goto out;
                }
                sprintf(ti[i].name, "%s_%02d", name, i);

                itc_locate_async(ti[i].name, NULL, ITC_MY_MBOX);

                result = add_mbox_tolnh(lnh_mbox_id, ti[i].name);
                if(result != 0) {
                        result = -__LINE__;
                        goto out;
                }

                msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        result = -__LINE__;
                        goto out;
                } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                        itc_free(&msg);
                        result = -__LINE__;
                        goto out;
                }
                ti[i].mbox_id = itc_sender(msg);
                itc_free(&msg);
        }


        for(i=0 ; i<nr_links ; i++) {
                ti[i].mon_id = itc_monitor(ti[i].mbox_id, NULL);
        }

        for(i=0 ; i<nr_links ; i++) {
                for(j=0 ; j<nr_msg ; j++) {
                        msg = itc_alloc(sizeof(uint32_t), LNH_TESTMSG);
                        msg->lnh_testmsg.seq_no = j;
                        itc_send(&msg, ti[i].mbox_id, ITC_MY_MBOX);
                }
        }
        for(i=0 ; i<nr_links ; i++) {
                for(j=0 ; j<nr_msg ; j++) {
                        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                        if(msg == NULL) {
                                result = -__LINE__;
                                goto out;
                        } else if(msg->msgno != LNH_TESTMSG    ||
                                  msg->lnh_testmsg.seq_no != j ||
                                  itc_sender(msg) != ti[i].mbox_id) {
                                itc_free(&msg);
                                result = -__LINE__;
                                goto out;
                        }
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                itc_unmonitor(ti[i].mon_id);
        }

        for(i=0 ; i<nr_links ; i++) {
                result = rem_mbox_fromlnh(lnh_mbox_id, ti[i].name);
                if(result != 0) {
                        result = -__LINE__;
                        goto out;
                }
        }

        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                result = -__LINE__;
                goto out;
        }

out:
        for(i=0 ; i<nr_links ; i++) {
                if(ti[i].name != NULL) {
                        free(ti[i].name);
                }
        }

        free(ti);

        return result;
}

static int lnh_test_multloc_add_mon(itc_mbox_id_t  lnh_mbox_id,
                                    char          *name,
                                    int            nr_links,
                                    int            nr_msg,
                                    int            nr_loc)
{
        union itc_msg *msg;
        int i, j, result = 0;
        struct testinst *ti;

        ti = malloc(nr_links * sizeof(struct testinst));
        if(ti == NULL) {
                return -__LINE__;
        }
        memset(ti, 0, (nr_links * sizeof(struct testinst)));

        for(i=0 ; i<nr_links ; i++) {
                ti[i].name = malloc(strlen(name) + 4);
                if(ti[i].name == NULL) {
                        result = -__LINE__;
                        goto out;
                }
                sprintf(ti[i].name, "%s_%02d", name, i);

                for(j=0 ; j<nr_loc ; j++) {
                        itc_locate_async(ti[i].name, NULL, ITC_MY_MBOX);
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                result = add_mbox_tolnh(lnh_mbox_id, ti[i].name);
                if(result != 0) {
                        result = -__LINE__;
                        goto out;
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                for(j=0 ; j<nr_loc ; j++) {
                        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                        if(msg == NULL) {
                                result = -__LINE__;
                                goto out;
                        } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                                itc_free(&msg);
                                result = -__LINE__;
                                goto out;
                        }
                        ti[i].mbox_id = itc_sender(msg);
                        itc_free(&msg);
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                ti[i].mon_id = itc_monitor(ti[i].mbox_id, NULL);
                itc_unmonitor(ti[i].mon_id);
                ti[i].mon_id = itc_monitor(ti[i].mbox_id, NULL);
        }

        for(i=0 ; i<nr_links ; i++) {
                for(j=0 ; j<nr_msg ; j++) {
                        msg = itc_alloc(sizeof(uint32_t), LNH_TESTMSG);
                        msg->lnh_testmsg.seq_no = j;
                        itc_send(&msg, ti[i].mbox_id, ITC_MY_MBOX);
                }
        }
        for(i=0 ; i<nr_links ; i++) {
                for(j=0 ; j<nr_msg ; j++) {
                        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                        if(msg == NULL) {
                                result = -__LINE__;
                                goto out;
                        } else if(msg->msgno != LNH_TESTMSG    ||
                                  msg->lnh_testmsg.seq_no != j ||
                                  itc_sender(msg) != ti[i].mbox_id) {
                                itc_free(&msg);
                                result = -__LINE__;
                                goto out;
                        }
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                result = rem_mbox_fromlnh(lnh_mbox_id, ti[i].name);
                if(result != 0) {
                        result = -__LINE__;
                        goto out;
                }
        }

        for(i=0 ; i<nr_links ; i++) {
                msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        result = -__LINE__;
                        goto out;
                } else if(msg->msgno != ITC_MONITOR_DEFAULT_NO) {
                        itc_free(&msg);
                        result = -__LINE__;
                        goto out;
                }
                itc_free(&msg);
        }

        for(i=0 ; i<nr_links ; i++) {
                itc_unmonitor(ti[i].mon_id);
        }

out:
        for(i=0 ; i<nr_links ; i++) {
                if(ti[i].name != NULL) {
                        free(ti[i].name);
                }
        }

        free(ti);

        return result;
}

int lnhtest(void)
{
        int result;
        pthread_t tid;
        itc_mbox_id_t lnh_mbox_id;
        union itc_msg *msg;

        if(pthread_create(&tid, NULL, lnh_thread,
                          (void *)LNH_THREAD_NAME) != 0) {
                return -__LINE__;
        }

        itc_locate_async(LNH_THREAD_NAME, NULL, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                itc_free(&msg);
                return -__LINE__;
        }
        lnh_mbox_id = itc_sender(msg);
        itc_free(&msg);

        result = add_lnh_tombox(lnh_mbox_id, "lnh");
        if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	result = lnh_test_loc_add_nomon(lnh_mbox_id, "lnh/lnh_test_loc_add_nomon", 5, 10);
	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	result = lnh_test_multloc_add_mon(lnh_mbox_id, "lnh/lnh_test_multloc_add_mon", 5, 1, 5);
	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

        /* Add a second LNH and redo tests */
        result = add_lnh_tombox(lnh_mbox_id, "lnh_01");
        if(result != 0) {
                result = -__LINE__;
		goto out;
                result -= 5000;
        }

	result = basic_lnh_test(lnh_mbox_id, "lnh_01/basic_lnh_test", 3, 5);
	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	result = lnh_test_loc_add_nomon(lnh_mbox_id, "lnh_01/lnh_test_loc_add_nomon", 5, 10);
	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	result = lnh_test_multloc_add_mon(lnh_mbox_id, "lnh_01/lnh_test_multloc_add_mon", 1, 1, 20);
	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	result = rem_lnh_frommbox(lnh_mbox_id, "lnh");
	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

out:
        pthread_cancel(tid);
	pthread_join(tid, NULL);

	return result;
}

static int prelnhtest(void)
{
	union itc_msg *msg;
	itc_mbox_id_t lnh_mbox_id;
	pthread_t tid;
	int result;

	if(pthread_create(&tid, NULL, lnh_thread,
                          (void *)"prelnhthread") != 0) {
                return -__LINE__;
        }

        itc_locate_async("prelnhthread", NULL, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                itc_free(&msg);
                return -__LINE__;
        }
        lnh_mbox_id = itc_sender(msg);
        itc_free(&msg);

	itc_locate_async("prelnh/mbox_1", NULL, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg != NULL) {
		result = -__LINE__;
		itc_free(&msg);
		goto out;
	}

        result = add_lnh_tombox(lnh_mbox_id, "prelnh");
        if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	result = add_mbox_tolnh(lnh_mbox_id, "prelnh/mbox_1");
	if(result != 0) {
		result = -__LINE__;
		goto out;
	}

        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                result = -__LINE__;
		goto out;
        } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                itc_free(&msg);
                result = -__LINE__;
		goto out;
        }
        itc_free(&msg);

out:
        pthread_cancel(tid);
	pthread_join(tid, NULL);

	return result;
}

static int lnheventtest(void)
{
	uint32_t lnh_added[] = { 1, ITC_LNH_ADDED };
	uint32_t lnh_removed[] = { 1, ITC_LNH_REMOVED };
	union itc_msg *msg;
	itc_mbox_id_t lnh_mbox_id;
	pthread_t tid;
	int result;

	itc_subscribe_events(ITC_EVENT_LNH_ADDED |
			     ITC_EVENT_LNH_REMOVED);

	msg = itc_receive(lnh_added, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
	}
	if(strcmp(msg->itc_lnh_added.lnhpath, "") != 0) {
                return -__LINE__;
	}
	if(msg->itc_lnh_added.mbox_id != ITC_NO_ID) {
                return -__LINE__;
	}
	if(!msg->itc_lnh_added.last) {
                return -__LINE__;
	}
        itc_free(&msg);

        if(pthread_create(&tid, NULL, lnh_thread,
                          (void *)LNH_THREAD_NAME) != 0) {
                return -__LINE__;
        }

        itc_locate_async(LNH_THREAD_NAME, NULL, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                itc_free(&msg);
                return -__LINE__;
        }
        lnh_mbox_id = itc_sender(msg);
        itc_free(&msg);

        result = add_lnh_tombox(lnh_mbox_id, "prelnh");
        if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	msg = itc_receive(lnh_added, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
	}
	if(strcmp(msg->itc_lnh_added.lnhpath, "prelnh/") != 0) {
                return -__LINE__;
	}
        itc_free(&msg);

	result = rem_lnh_frommbox(lnh_mbox_id, "prelnh");
	if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	msg = itc_receive(lnh_removed, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
	}
	if(strcmp(msg->itc_lnh_removed.lnhpath, "prelnh/") != 0) {
                return -__LINE__;
	}
        itc_free(&msg);

	itc_unsubscribe_events(ITC_EVENT_ALL);

        pthread_cancel(tid);
	pthread_join(tid, NULL);

        if(pthread_create(&tid, NULL, lnh_thread,
                          (void *)LNH_THREAD_NAME) != 0) {
                return -__LINE__;
        }

        itc_locate_async(LNH_THREAD_NAME, NULL, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        } else if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                itc_free(&msg);
                return -__LINE__;
        }
        lnh_mbox_id = itc_sender(msg);
        itc_free(&msg);

	result = add_lnh_tombox(lnh_mbox_id, "eventlnh");
        if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	itc_subscribe_events(ITC_EVENT_LNH_ADDED |
			     ITC_EVENT_LNH_REMOVED);

	msg = itc_receive(lnh_added, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
	}
	if(strcmp(msg->itc_lnh_added.lnhpath, "eventlnh/") != 0) {
                return -__LINE__;
	}
	if(!msg->itc_lnh_added.last) {
                return -__LINE__;
	}
        itc_free(&msg);

        result = add_lnh_tombox(lnh_mbox_id, "prelnh");
        if(result != 0) {
                result = -__LINE__;
		goto out;
        }

	msg = itc_receive(lnh_added, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
	}
	if(strcmp(msg->itc_lnh_added.lnhpath, "prelnh/") != 0) {
                return -__LINE__;
	}
        itc_free(&msg);

        pthread_cancel(tid);
	pthread_join(tid, NULL);

	msg = itc_receive(lnh_removed, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
	}
	if(strcmp(msg->itc_lnh_removed.lnhpath, "prelnh/") != 0 &&
	   strcmp(msg->itc_lnh_removed.lnhpath, "eventlnh/") != 0) {
                return -__LINE__;
	}
        itc_free(&msg);

	msg = itc_receive(lnh_removed, REC_SHORT_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
	}
	if(strcmp(msg->itc_lnh_removed.lnhpath, "prelnh/") != 0 &&
	   strcmp(msg->itc_lnh_removed.lnhpath, "eventlnh/") != 0) {
                return -__LINE__;
	}
        itc_free(&msg);

	itc_unsubscribe_events(ITC_EVENT_ALL);

	return 0;

out:
        pthread_cancel(tid);
	pthread_join(tid, NULL);

	return result;
}

int run_lnhtest(void)
{
	itc_mbox_id_t mbox_id;
	int res = 0;

        /* Use a large amount of mailboxes here, no range check in the testcase */
        if(itc_init(100, ITC_MALLOC, NULL,
                    ITC_NO_NAMESPACE, 0) != 0) {
                return -__LINE__;
        }

        mbox_id = itc_create_mailbox("lnh_test", 0);

	res = lnhtest();
	if(res != 0) {
		res -= 10000;
		goto out;
	}

	res = prelnhtest();
	if(res != 0) {
		res -= 10000;
		goto out;
	}

	res = lnheventtest();
	if(res != 0) {
		res -= 20000;
		goto out;
	}

out:
        itc_delete_mailbox(mbox_id);
        itc_exit();

       return res;
}

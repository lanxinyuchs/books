/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file itc_linx.c
 *
 *   [Enter a longer description about the file. More than one line
 *   can be used, but leave an empty line before the copyright line.]
 *
 *   [All text within [...] should be changed or removed.]
 *
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
 *   Revised : 2013-08-29 Magnus Lindberg
 *   Change  : First version.
 *
 *   Revised : 2014-01-29 Magnus Lindberg
 *   Change  : Corrected allocation of ITC_LOCATE_LNH_REPLY and dual
 *             outstanding locates when a clone is created.
 *
 *   Revised : 2014-11-27 Magnus Lindberg
 *   Change  : Improved error logs at abort calls,changed from using
 *             select to poll. Updated EINTR handling so that retries
 *             are made.
 *
 *   Revised : 2015-03-04 Madhusudan Veladri
 *   Change  : Added workaround to ignore duplicate monitor message for
 *             mbox removal.
 *
 *   Revised : 2015-03-30 Magnus Lindberg
 *   Change  : Corrected an error where a peer mailbox was used at sender
 *             in send.
 *
 *   Revised : 2015-04-09 Madhusudan Veladri
 *   Change  : Added shutdown handling and discarding of msg in certain use
 *             cases rather than a restart.
 *
 *   Revised : 2015-11-10 Padmaiah U
 *   Change  : Modified send_on_linx function such that program abort
 *             code (incase of sending mailbox details not found) is removed.
 *
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <syslog.h>

#include <sys/socket.h>
#include <sys/select.h>
#include <sys/poll.h>

#include <errno.h>

#include "pthread.h"

#include "search.h"

#include "linx.h"

#include "itc.h"
#include "itc_system.h"

#include "itc_linx.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define LINXLNH_FWD_NAME "linxlnh_fwd"

#define REM_LINX 0x17c900
#define LNH_RESTART(errorcode)		       \
	do {				       \
		if(errorcode)		       \
			perror("LNH restart"); \
	        abort();		       \
	} while(0)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
#define LINX_HUNT 0x17c800
struct linx_hunt {
        LINX_SIGSELECT   signo;
        itc_mbox_id_t    from;
        char             name[1];
};

union LINX_SIGNAL {
        LINX_SIGSELECT               signo;
        struct linx_hunt             linx_hunt;
};

union itc_msg {
        uint32_t                     msgno;

        struct itc_locate_lnh        itc_locate_lnh;
        struct itc_locate_lnh_reply  itc_locate_lnh_reply;
        struct itc_mbox_added        itc_mbox_added;

        struct add_linx_lnh          add_linx_lnh;
};

/* Important the first field of the linx_mbox_data and linx_spid_data
 * structures have to be mbox_id. Otherwise the tree compare functions
 * will stop working. */
struct linx_mbox_data {
        itc_mbox_id_t          mbox_id;
        LINX_SPID              spid;

        struct linx_mbox_data *next;

        LINX                  *handle;
        int                    fd;
        itc_monitor_id_t       monid;
};

struct linx_spid_data {
        itc_mbox_id_t          mbox_id;
        LINX_SPID              spid;

        LINX_OSATTREF          attref;
};

struct cfg_lnh {
        struct cfg_lnh *next;
        char            lnhpath[1];
};

struct linx_instance {

        itc_mbox_id_t          mbox_id;
	int                    mbox_fd;
        LINX                  *handle;
	int                    linx_fd;

        void                  *mbox_tree;

        void                  *rem_mbox_tree;
        void                  *rem_spid_tree;

	struct pollfd         *poll_fds;
	int                    num_fds;

        struct cfg_lnh        *lnhlist;
        struct linx_mbox_data *mboxlist;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static struct linx_instance li;
static bool lnh_shutdown = 0;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   compare_mbox_add
 *
 *   @param pa         Pointer to mailbox A.
 *
 *   @param pb         Pointer to mailbox B.
 *
 *   @return           0 if mailbox id is equal.
 *                     negative if mailbox id A less than B.
 *                     positive if mailbox id A larger than B.
 *
 *   @par Globals:     --
 *
 *   Compare function of 2 mailbox ids, used together with the
 *   tree functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_mbox_add(const void *pa, const void *pb)
{
        const struct linx_mbox_data *mb_a, *mb_b;
        int res;

        mb_a = pa;
        mb_b = pb;

        if(mb_a->mbox_id < mb_b->mbox_id)
                res = -1;
        else if(mb_a->mbox_id > mb_b->mbox_id)
                res = 1;
        else
                res = 0;

        return res;
}

/* ===================================================================== */
/**
 *   compare_mbox
 *
 *   @param pa         Mailbox ID to find.
 *
 *   @param pb         Pointer to mailbox B.
 *
 *   @return           0 if mailbox id is equal.
 *                     negative if mailbox id A less than B.
 *                     positive if mailbox id A larger than B.
 *
 *   @par Globals:     --
 *
 *   Compare function of 2 mailbox ids, used together with the
 *   tree functions tfind.
 */
/* ===================================================================== */
static int compare_mbox(const void *pa, const void *pb)
{
        const struct linx_mbox_data *mb;
        const itc_mbox_id_t *mbox_id;
        int res;

        mbox_id = pa;
        mb = pb;

        if(*mbox_id < mb->mbox_id)
                res = -1;
        else if(*mbox_id > mb->mbox_id)
                res = 1;
        else
                res = 0;

        return res;
}
/* ===================================================================== */
/**
 *   compare_mbox_add
 *
 *   @param pa         Pointer to spid A.
 *
 *   @param pb         Pointer to spid B.
 *
 *   @return           0 if spid is equal.
 *                     negative if spid A less than B.
 *                     positive if spid A larger than B.
 *
 *   @par Globals:     --
 *
 *   Compare function of 2 spids, used together with the
 *   tree functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_spid_add(const void *pa, const void *pb)
{
        const struct linx_spid_data *sd_a, *sd_b;
        int res;

        sd_a = pa;
        sd_b = pb;

        if(sd_a->spid < sd_b->spid)
                res = -1;
        else if(sd_a->spid > sd_b->spid)
                res = 1;
        else
                res = 0;

        return res;
}

/* ===================================================================== */
/**
 *   compare_spid
 *
 *   @param pa         Linx spid of "mailbox" to find.
 *
 *   @param pb         Pointer to spid.
 *
 *   @return           0 if spid is equal.
 *                     negative if spid A less than B.
 *                     positive if spid A larger than B.
 *
 *   @par Globals:     --
 *
 *   Compare function of 2 spids, used together with the
 *   tree function tfind.
 */
/* ===================================================================== */
static int compare_spid(const void *pa, const void *pb)
{
        const struct linx_spid_data *sd;
        const LINX_SPID *spid;
        int res;

        spid = pa;
        sd = pb;

        if(*spid < sd->spid)
                res = -1;
        else if(*spid > sd->spid)
                res = 1;
        else
                res = 0;

        return res;
}


/* ===================================================================== */
/**
 *   add_rem_mbox
 *
 *   @param spid       Linx spid to add.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Add an itc mailbox for a Linx remote endpoint.
 */
/* ===================================================================== */
static void update_pollfds(void)
{
	struct linx_mbox_data *mboxlist;
	int fdp, num_fds = 2; /* 2 FDs needed for the linkhandlers
				 ITC and Linx endpoints*/

	if(li.poll_fds != NULL) {
		free(li.poll_fds);
	}

	for(mboxlist = li.mboxlist     ;
	    mboxlist != NULL           ;
	    mboxlist = mboxlist->next) {
		num_fds++;
	}

	li.poll_fds = malloc(num_fds * sizeof(struct pollfd));
	if(li.poll_fds == NULL) {
		LNH_RESTART(false);
	}

	li.num_fds = num_fds;

	fdp = 0;
	li.poll_fds[fdp].events  = POLLIN;
	li.poll_fds[fdp].revents = 0;
	li.poll_fds[fdp++].fd    = li.mbox_fd;

	li.poll_fds[fdp].events  = POLLIN;
	li.poll_fds[fdp].revents = 0;
	li.poll_fds[fdp++].fd    = li.linx_fd;

	for(mboxlist = li.mboxlist     ;
	    mboxlist != NULL           ;
	    mboxlist = mboxlist->next) {
		li.poll_fds[fdp].events  = POLLIN;
		li.poll_fds[fdp].revents = 0;
		li.poll_fds[fdp++].fd    = mboxlist->fd;
	}
}

/* ===================================================================== */
/**
 *   add_rem_mbox
 *
 *   @param spid       Linx spid to add.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Add an itc mailbox for a Linx remote endpoint.
 */
/* ===================================================================== */
static struct linx_spid_data *add_rem_mbox(LINX_SPID spid)
{
        struct linx_spid_data *sd, **res;
        struct linx_mbox_data *mbox;
	union LINX_SIGNAL *sig;
        char *name = NULL;
        int result;

        res = tfind(&spid, &li.rem_spid_tree, compare_spid);
        if(res != NULL) {
                /* Linx spid already present.
                   This is due to a dual locate before the
                   SPID is created so just do nothing.*/
                return *res;
        }

        result = linx_get_name(li.handle, spid, &name);
        if(result != 0   ||
           name == NULL) {
                /* The sender is terminated meanwhile and should be ok
                 *  to discard the message */
                if(errno == ECONNRESET) {
                        return NULL;
                }
                LNH_RESTART(true);
        }

        sd = malloc(sizeof(struct linx_spid_data));
	if(sd == NULL) {
		LNH_RESTART(false);
	}
        sd->spid    = spid;
        sd->mbox_id = itc_clone_mailbox(ITC_MY_MBOX, name);
        sig         = linx_alloc(li.handle, sizeof(LINX_SIGSELECT), REM_LINX);
        sd->attref  = linx_attach(li.handle, &sig, spid);
        linx_free_name(li.handle, &name);

        tsearch(sd, &li.rem_spid_tree, compare_spid_add);

        res = tfind(&(sd->mbox_id), &li.rem_mbox_tree, compare_mbox);
        if(res != NULL) {
                LNH_RESTART(false);
        }
        tsearch(sd, &li.rem_mbox_tree, compare_mbox_add);

	/* We add an entry for the cloned mailboxes in the mbox tree
	   to allow them to be the sender of messages over the linkhandler.
	   We do not add it to the mailbox list since this is not needed. */
        mbox = malloc(sizeof(struct linx_mbox_data));
        if(mbox == NULL) {
                LNH_RESTART(false);
        }
        mbox->mbox_id = sd->mbox_id;
	mbox->handle  = NULL;
        mbox->spid    = spid;
        mbox->fd      = -1;
        res = tfind(mbox, &li.mbox_tree, compare_mbox_add);
        if(res != NULL) {
                LNH_RESTART(false);
        }
        tsearch(mbox, &li.mbox_tree, compare_mbox_add);

        return sd;
}

/* ===================================================================== */
/**
 *   rem_rem_mbox
 *
 *   @param spid       Linx spid to add.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Remove an itc mailbox for a Linx remote endpoint.
 */
/* ===================================================================== */
static int rem_rem_mbox(LINX_SPID spid)
{
        struct linx_spid_data *sd, **res;
        struct linx_mbox_data *mbox, **mboxres;

        res = tfind(&spid, &li.rem_spid_tree, compare_spid);
        if(res == NULL) {
                LNH_RESTART(false);
        }
        sd = *res;

        res = tfind(&(sd->mbox_id), &li.rem_mbox_tree, compare_mbox);
        if(res == NULL) {
                LNH_RESTART(false);
        }

        mboxres = tfind(&(sd->mbox_id), &li.mbox_tree, compare_mbox);
        if(res != NULL) {
	    mbox = *mboxres;
	    tdelete(mbox, &li.mbox_tree, compare_mbox);
	    free(mbox);
	}

        itc_delete_mailbox(sd->mbox_id);

        if(tdelete(sd, &li.rem_spid_tree, compare_spid_add) == NULL) {
                LNH_RESTART(false);
        }
        if(tdelete(sd, &li.rem_mbox_tree, compare_mbox_add) == NULL) {
                LNH_RESTART(false);
        }

        free(sd);

        return 0;
}

/* ===================================================================== */
/**
 *   add_mbox
 *
 *   @param mbox_id    Mailbox id to add.
 *
 *   @param mbox_name  Mailbox name to add.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Add a Linx endpoint for a created mailbox.
 */
/* ===================================================================== */
static int add_mbox(itc_mbox_id_t mbox_id, char *mbox_name)
{
        struct linx_mbox_data *mbox, **res;
        struct cfg_lnh *lnh;
        char *tmp;

	/* Check it this is your own mailbox, if so ignore it
	   since it will be handled specifically. */
	if(mbox_id == li.mbox_id) {
		return 0;
	}

       /* Check if this is an LNH clone created by me,
           if so do not add the mailbox */
        for(lnh = li.lnhlist ; lnh != NULL ; lnh = lnh->next) {
                tmp = strchr(mbox_name, '/');
                if(tmp != NULL) {
                        *tmp = '\0';
                        if(strcmp(lnh->lnhpath, mbox_name) == 0) {
                                *tmp = '/';
                                return 0;
                        }
                        *tmp = '/';
                }
        }

        mbox = malloc(sizeof(struct linx_mbox_data));
        if(mbox == NULL) {
                LNH_RESTART(false);
        }
        mbox->mbox_id = mbox_id;
        mbox->handle  = linx_open(mbox_name, 0, NULL);
	if(mbox->handle == NULL) {
		LNH_RESTART(true);
	}
        mbox->spid    = linx_get_spid(mbox->handle);
        mbox->fd      = linx_get_descriptor(mbox->handle);

        mbox->next    = li.mboxlist;
        li.mboxlist  = mbox;

	update_pollfds();

        mbox->monid = itc_monitor(mbox->mbox_id, NULL);

        res = tfind(mbox, &li.mbox_tree, compare_mbox_add);
        if(res != NULL) {
                LNH_RESTART(false);
        }
        tsearch(mbox, &li.mbox_tree, compare_mbox_add);

        return 0;
}

/* ===================================================================== */
/**
 *   rem_mbox
 *
 *   @param mbox_id    Mailbox id to remove.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Add a Linx endpoint for a created mailbox.
 */
/* ===================================================================== */
static int rem_mbox(itc_mbox_id_t mbox_id)
{
        struct linx_mbox_data *mbox, **res, *mboxlist, *prev;

        res = tfind(&mbox_id, &li.mbox_tree, compare_mbox);
        if(res == NULL) {
            /* FIXME: Workaround for multiple monitor messages on mbox removal */
            syslog(LOG_WARNING, "itc_linx: Ignored duplicate rem mbox monitor message. "
                   "mbox: 0x%08x", mbox_id);
            return 0;
        }
        mbox = *res;

        tdelete(mbox, &li.mbox_tree, compare_mbox);

        prev = NULL;
        for(mboxlist = li.mboxlist     ;
            mboxlist != NULL           ;
            mboxlist = mboxlist->next) {
                if(mboxlist == mbox) {
                        break;
                }
                prev = mboxlist;
        }
        if(mboxlist == NULL) {
                LNH_RESTART(false);
        }
        if(prev == NULL) {
                li.mboxlist = mbox->next;
        } else {
                prev->next = mbox->next;
        }
        linx_close(mbox->handle);
        free(mbox);

	update_pollfds();

        return 0;
}

/* ===================================================================== */
/**
 *   send_on_linx
 *
 *   @param handle     Linx handle of current context.
 *
 *   @param msg        itc_msg to be forwarded to Linx.
 *
 *   @return           -
 *
 *   @par Globals:     linx_inst
 *                     ITC Linx instance global structure.
 *
 *   Forwards an itc message to a remote peer over Linx.
 */
/* ===================================================================== */
static void send_on_linx(LINX *handle, union itc_msg *msg)
{
        union LINX_SIGNAL *sig;
        itc_mbox_id_t tmp_id;
        void **res;
        struct linx_spid_data *sd;
        struct linx_mbox_data *from_mbox;

        tmp_id = itc_receiver(msg);
        res = tfind(&tmp_id, &li.rem_mbox_tree, compare_mbox);
        if(res == NULL) {
                /* Discard the msg when destination mbox is not found as 
                 * that might have died or link is lost */
                return;
        }
        sd = *res;

        tmp_id = itc_sender(msg);
        res = tfind(&tmp_id, &li.mbox_tree, compare_mbox);
        while (res == NULL) {
                union itc_msg *msg_loop;
                uint32_t addmsg[] = { 1, ITC_MBOX_ADDED };

                msg_loop = itc_receive(addmsg, 100, ITC_FROM_ALL);
                if (msg_loop == NULL) {
                        syslog(LOG_ERR, "LNH mailbox: 0x%08x not found for msg: 0x%08x."
                               "Message is discarded.", tmp_id, msg->msgno);
                        return;
                }

                add_mbox(msg_loop->itc_mbox_added.mbox_id,
                         msg_loop->itc_mbox_added.mbox_name);
                itc_free(&msg_loop);

                res = tfind(&tmp_id, &li.mbox_tree, compare_mbox);
        }
        from_mbox = *res;

        sig = linx_alloc(handle, itc_size(msg), 0);
        memcpy(sig, msg, itc_size(msg));
        linx_send_w_s(handle, &sig, from_mbox->spid, sd->spid);
}

/* ===================================================================== */
/**
 *   add_lnh
 *
 *   @param lnhpath    Lnhpath to add towards itc.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Add a Linx endpoint for a created mailbox.
 */
/* ===================================================================== */
static int add_lnh(char *lnhpath)
{
        struct cfg_lnh *lnh;

        lnh = malloc(sizeof(struct cfg_lnh) + strlen(lnhpath));
        if(lnh == NULL) {
                LNH_RESTART(true);
        }

        strcpy(lnh->lnhpath, lnhpath);
        lnh->next = li.lnhlist;
        li.lnhlist = lnh;

        itc_assign_linkhandler(lnhpath, ITC_MY_MBOX);

        return 0;
}

/* ===================================================================== */
/**
 *   add_lnh
 *
 *   @param lnhpath    Lnhpath to add towards itc.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Add a Linx endpoint for a created mailbox.
 */
/* ===================================================================== */
static int locate_spid(itc_mbox_id_t from, char *name)
{
        union LINX_SIGNAL *sig;

        sig = linx_alloc(li.handle,
                         (sizeof(struct linx_hunt) + strlen(name)),
                         LINX_HUNT);
        sig->linx_hunt.from = from;
        strcpy(sig->linx_hunt.name, name);
        if(linx_hunt(li.handle, name, &sig) != 0) {
                LNH_RESTART(true);
        }

        return 0;
}

/* ===================================================================== */
/**
 *   data_from_receiver
 *
 *   @param linx_rx    Receive context which has data.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   The data_from_receiver function delivers signals received from
 *   processor external Linx processes.
 */
/* ===================================================================== */
static int mbox_config()
{
        union itc_msg *msg;
        int result = 0;

        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg == NULL) {
                return -1;
        }

        if(itc_receiver(msg) == li.mbox_id) {
                switch(msg->msgno) {
                case ITC_MBOX_ADDED:
                        result = add_mbox(msg->itc_mbox_added.mbox_id,
                                          msg->itc_mbox_added.mbox_name);
                        break;

                case ITC_MONITOR_DEFAULT_NO:
                        result = rem_mbox(itc_sender(msg));
                        break;

                case ADD_LINX_LNH:
                        add_lnh(msg->add_linx_lnh.lnhpath);
                        break;

                case ITC_LOCATE_LNH:
                        locate_spid(itc_sender(msg),
                                    msg->itc_locate_lnh.name);
                        break;

                default:
                        result = -1;
                        break;
                }
        } else {
                send_on_linx(li.handle, msg);
        }

        itc_free(&msg);

        return result;
}

/* ===================================================================== */
/**
 *   data_from_receiver
 *
 *   @param linx_rx    Receive context which has data.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   The data_from_receiver function delivers signals received from
 *   processor external Linx processes.
 */
/* ===================================================================== */
static int linx_config(void)
{
        union itc_msg *msg;
        union LINX_SIGNAL *sig;
        LINX_SIGSELECT any[] = { 0 };
        struct linx_spid_data *sd;
        int result;

        result = linx_receive_w_tmo(li.handle, &sig, 0, any);
        if(result < 0) {
                LNH_RESTART(true);
        }
        if(sig == NULL) {
                LNH_RESTART(true);
        }

        result = 0;
        switch(sig->signo) {
                case REM_LINX:
                        rem_rem_mbox(linx_sender(li.handle, &sig));
                        break;

                case LINX_HUNT:
                        sd = add_rem_mbox(linx_sender(li.handle, &sig));
                        if(sd == NULL)
                                LNH_RESTART(true);
                        msg = itc_alloc((sizeof(struct itc_locate_lnh_reply) +
                                         strlen(sig->linx_hunt.name)),
                                        ITC_LOCATE_LNH_REPLY);
                        msg->itc_locate_lnh_reply.mbox_id = sd->mbox_id;
                        msg->itc_locate_lnh_reply.found = 1;
                        strcpy(msg->itc_locate_lnh_reply.name, sig->linx_hunt.name);
                        itc_send(&msg, sig->linx_hunt.from, ITC_MY_MBOX);
                        break;

                default:
                        result = -1;
                        break;
        }

        result = linx_free_buf(li.handle, &sig);
        if(result < 0) {
                LNH_RESTART(true);
        }

        return result;
}

/* ===================================================================== */
/**
 *   data_from_receiver
 *
 *   @param linx_rx    Receive context which has data.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   The data_from_receiver function delivers signals received from
 *   processor external Linx processes.
 */
/* ===================================================================== */
static int send_to_mbox(struct linx_mbox_data *mbox)
{
        union itc_msg *msg;
        union LINX_SIGNAL *sig;
        LINX_SIGSELECT any[] = { 0 };
        struct linx_spid_data *sender_sd, **res;
        LINX_SPID tmp_spid;
        int result;


        result = linx_receive_w_tmo(mbox->handle, &sig, 0, any);
        if(result < 0) {
                LNH_RESTART(true);
        }
        if(sig == NULL) {
                LNH_RESTART(false);
        }

        tmp_spid = linx_sender(mbox->handle, &sig);
        res = tfind(&tmp_spid, &li.rem_spid_tree, compare_spid);
        if(res == NULL) {
                sender_sd = add_rem_mbox(linx_sender(mbox->handle, &sig));
        } else {
                sender_sd = *res;
        }

        /* Discard the msg if failed to add rem mbox. It can happen when sender
         * terminates in meanwhile */
        if(sender_sd != NULL) {

                msg = itc_alloc(linx_sigsize(mbox->handle, &sig), 0);
                memcpy(msg, sig, linx_sigsize(mbox->handle, &sig));
                itc_send(&msg, mbox->mbox_id, sender_sd->mbox_id);
        }

        result = linx_free_buf(mbox->handle, &sig);
        if(result < 0) {
                LNH_RESTART(true);
        }

        return 0;
}

/* ===================================================================== */
/**
 *   Shutdown link handler 
 *
 *   @param            --
 *
 *   @return           --
 *
 *   @par Globals:     lnh_shutdown
 *   
 *   Invoked during shutdown (from atexit or signal handler) to make sure
 *   that lnh is stopped from processing messages.
 */
/* ===================================================================== */
void lnh_exit(void)
{
        lnh_shutdown = 1;
}

/* ===================================================================== */
/**
 *   linx_forwarder
 *
 *   @param data       Used as name for the internal itc_mailbox.
 *
 *   @return           Never returns.
 *
 *   @par Globals:     li
 *                     ITC Linx instance global structure.
 *
 *   Thread that forwards messages from remote Linx endpoints to the
 *   processor local itc mailboxes.
 */
/* ===================================================================== */
void *linxlnh_main(void *data)
{
        char *name = data;
        int i, handled, res;
        struct linx_mbox_data *mboxlist;

        li.mbox_id = itc_create_mailbox(name, 0);
        li.mbox_fd = itc_get_fd();
	li.handle  = linx_open(name, 0, NULL);
	li.linx_fd = linx_get_descriptor(li.handle);

	update_pollfds();

        itc_get_mboxes();

        for(;;) {
		while(1) {
			res = poll(li.poll_fds, li.num_fds, -1);
			if(res > 0) {
				break;
			} else if(res == -1 && errno == EINTR) {
			        continue;
			} else {
				LNH_RESTART(true);
			}
		}

                if(lnh_shutdown) {
                        /* Wait for restart */
                        while(1)
                                sleep(1);
                }

		handled = 0;
		if(li.poll_fds[0].revents & POLLIN) {
			mbox_config();
			handled++;
			li.poll_fds[0].revents = 0;
		}

		if(li.poll_fds[1].revents & POLLIN) {
			linx_config();
			handled++;
			li.poll_fds[1].revents = 0;
		}

		mboxlist = li.mboxlist;
                for(i=2 ; i<li.num_fds && handled < res; i++) {
			if(li.poll_fds[i].revents & POLLIN) {
				send_to_mbox(mboxlist);
				handled++;
				li.poll_fds[i].revents = 0;
			}
			mboxlist = mboxlist->next;
                }
        }
}

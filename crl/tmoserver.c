/**
 *   Timeout server implementation.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   A user timeout with its metadata are stored in the structure
 *   'entry'. All entry(ies) are places into a linked list, 'tmo'
 *   in the tmo_server thread.
 *
 *   A tmo entry and a Linux timer are created at a request_tmo[_sig
 *   request and both entities are removed at cancel_tmo[_sig] or
 *   when the client receives the tmo signal.
 *   At restart_tmo, the current tmo entry and the Linux timer are
 *   removed and new tmo entry (with old user data and new tmo ref)
 *   are created.
 *
 *   The tmo signal is wrapped into an internal LITS signal for two
 *   reasons;
 *   1. The user provided signal is not touched.
 *   2. This makes receive calls faster.
 *
 *   A tmo is "alive" until the user cancels the tmo or receives the
 *   tmo signal, meaning that tmo entry and the Linux timer are kept
 *   until that point.
 *
 *   If a tmo ref is found in the tmo linked list, it has not been
 *   cancelled or received by the client.
  *
 *   Copyright (C) 2012-2016 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-12-16 Magnus Lindberg
 *   Change  : Added attach to tmo server clients to avoid the need for a
 *             pthread cleanup handler for all lits threads.
 *
 *   Revised : 2015-09-10 Ravineet Singh
 *   Change  : Added support for user define:able lits_sigrt_base.
 *             Changed TMO_SIGNAL -> THREAD_TMO_SIGNAL
 *
 *   Revised : 2015-04-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Temporary work around for UM/ITC fd/select bug
 *
 *   Revised : 2015-04-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Wrapped tmo signal has same signo as original tmo signal.
 *             This so that the application can select only on requested
 *             tmo signal number.
 *
 *   Revised : 2015-03-16 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 *             Tmo server funtionality extracted from time.c into this file.
 *
 *   Revised : 2016-03-09 Magnus Lindberg
 *   Change  : Updated the tmo_wrap signal structure to allow for a wider
 *             magic pattern and a more unused sigsize. Also ensured that
 *             magic pattern is cleared before the tmo_wrap signale is
 *             freed.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <sys/signalfd.h>
#include <sys/syscall.h>   /* For SYS_xxx definitions */
#include <sys/types.h>
#include <time.h>
#include <tmoserver.h>
#include <unistd.h>

#include "lits_internal.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef struct tmo_entry_t_
{
    PROCESS              client;
    OSTMOREF		ref;
    SIGSELECT		signo;
    timer_t		timer_id;
    union SIGNAL		*wrap;
    union SIGNAL		*usersig;
    struct tmo_entry_t_	*next;
} tmo_entry_t;

typedef struct attaches_t_
{
    PROCESS             client;
    OSATTREF            attref;
    struct attaches_t_ *next;
} attaches_t;

typedef struct tmo_t_
{
    unsigned int	 last_ref;
    uint32_t             tot;
    attaches_t          *attaches;
    tmo_entry_t		*head;
    tmo_entry_t		*tail;
} tmo_t;

struct tmo_sig
{
    SIGSELECT		signo;
    OSTMOREF		ref;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static PROCESS tmo_server_pid = 0;

/* Thread local indicator in case timers are used. */
__thread  uint32_t  lits_tmo_user = 0;


union SIGNAL
{
    SIGSELECT          signo;
    struct tmo_sig     tmo_sig;
    struct tmo_wrap    tmo_wrap;
    struct tmo_request tmo_request;
    struct tmo_restart tmo_restart;
    struct tmo_cancel  tmo_cancel;
};

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Find timeout entry by ref
 *
 *   @param tmo        tmo list
 *
 *   @param ref         timeout reference
 *
 *   @return            entry if found, NULL otherwise
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static tmo_entry_t *
find_tmo_entry_ref(tmo_t *tmo, OSTMOREF ref)
{
    tmo_entry_t *entry;
    tmo_entry_t *ret = NULL;

    for (entry = tmo->head; entry; entry=entry->next)
    {
        if ( entry->ref != ref )
            continue;
        ret = entry;
        break;
    }
    return ret;
}

/** ==================================================================== */
/**
 *   Find timeout entry by pid
 *
 *   @param tmo        tmo list
 *
 *   @param client     client PID
 *
 *   @return           entry if found, NULL otherwise
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static tmo_entry_t *
find_tmo_entry_pid(tmo_t *tmo, PROCESS client)
{
    tmo_entry_t *entry;
    tmo_entry_t *ret = NULL;

    for (entry = tmo->head; entry; entry=entry->next)
    {
        if ( entry->client != client )
        {
            continue;
        }
        ret = entry;
        break;
    }

    return ret;
}

/** ==================================================================== */
/**
 *   Does pid have any timeout entry
 *
 *   @param tmo        tmo list
 *
 *   @param client     client PID
 *
 *   @return           true if pid has entry
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static bool
pid_has_tmo_entry(tmo_t *tmo, PROCESS client)
{
    return (find_tmo_entry_pid(tmo, client) != NULL);
}

/** ==================================================================== */
/**
 *   Add timeout entry
 *
 *   @param tmo        tmo list
 *
 *   @param entry      tmo entry
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static void
add_tmo_entry(tmo_t *tmo, tmo_entry_t *entry)
{
    if ( tmo->head == NULL )
    {
        tmo->head = entry;
    }
    else
    {
        tmo->tail->next = entry;
    }
    tmo->tail = entry;
    tmo->tot++;
}


/** ==================================================================== */
/**
 *   Remove timeout entry
 *
 *   @param tmo        tmo list
 *
 *   @param e          tmo entry
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static void
remove_tmo_entry(tmo_t *tmo, tmo_entry_t *e)
{
    tmo_entry_t *entry;
    tmo_entry_t *prev = NULL;

    for (entry = tmo->head; entry; prev = entry, entry = entry->next)
    {
        if ( entry != e )
            continue;

        if (prev)
            prev->next = entry->next;
        else
            tmo->head = entry->next;

        if (!entry->next)
            tmo->tail = prev;

        free(entry);
        tmo->tot--;
        return;
    }

    abort();
}

/** ==================================================================== */
/**
 *   Start a timeout
 *
 *   @param timeoout   Timeout time in milliseconds
 *   @param ref        Reference to the timeout entry
 *
 *   @return           Timer id
 *
 *   @par Globals:
 *                     tmo
 */
/* ===================================================================== */
static void
start_tmo(OSTIME timeout, timer_t timer_id)
{
    struct itimerspec its;

    if (timeout)
    {
        its.it_value.tv_sec = timeout / 1000;
        its.it_value.tv_nsec = (timeout % 1000) * 1000000;
    }
    else
    {
        /* 0(zero) will stop the timer, set it to a small value. */
        its.it_value.tv_sec = 0;
        its.it_value.tv_nsec = 10;
    }
    its.it_interval.tv_sec = 0;
    its.it_interval.tv_nsec = 0;

    if ( timer_settime(timer_id, 0, &its, NULL) == -1 )
        lits_error("start_tmo, timer_settime failed, %s(%d)",
                   strerror(errno), errno);
    return;
}

/** ==================================================================== */
/**
 *   Creates a new timeout entry
 *
 *   @param client     Client
 *
 *   @param sn         Signal number
 *
 *   @param tmosigno   User supplied signal.
 *
 *   @param ref        tmo ref
 *
 *   @return           Timeout entry
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static tmo_entry_t *
create_tmo_entry(PROCESS client, SIGSELECT sn, union SIGNAL **tmosig,
                 OSTMOREF ref)
{
    tmo_entry_t     *entry;
    union SIGNAL    *usersig;
    struct sigevent sigev;
    union SIGNAL    *wrap;
    SIGSELECT       signo;
    timer_t         timer_id;
    int             ret;

    usersig = NULL;
    signo = sn;
    if (tmosig)
    {
        if(*tmosig)
        {
            signo = (*tmosig)->signo;
        }
        usersig = *tmosig;
        *tmosig = NIL;
    }

    entry = calloc(1, sizeof(tmo_entry_t));
    assert(NULL != entry);

    /*
     * Wrap user signal (with original sigal nr.),
       i.e. easy to find when removing from users queue.
    */
    wrap = alloc(sizeof(struct tmo_wrap), signo);
    entry->wrap = wrap;
    wrap->tmo_wrap.ref = ref;
    strcpy(wrap->tmo_wrap.magic, LITS_TMO_MAGIC);
    entry->usersig = usersig;

    entry->ref = ref;

    memset(&sigev, 0, sizeof(sigev));
    sigev.sigev_notify = SIGEV_THREAD_ID | SIGEV_SIGNAL;
    sigev.sigev_signo = THREAD_TMO_SIGNAL;
    sigev._sigev_un._tid = syscall(SYS_gettid);
    sigev.sigev_value.sival_int = entry->ref;

    do
    {
        errno = 0;
        ret = timer_create(CLOCK_MONOTONIC, &sigev, &timer_id);
        if ( ret == -1 && errno != EINTR)
        {
            lits_error("timer_create failed, %s(%d)", strerror(errno), errno);
        }
    }
    while(errno == EINTR);

    entry->timer_id = timer_id;
    entry->signo = sn;
    entry->client = client;
    return entry;
}

/** ==================================================================== */
/**
 * Removes a timeout and frees wrap signal (if not sent)
 *
 *   @param tmoref     Timeout reference
 *   @param get_signal Non-zero if the timeout signal should be returned
 *
 *   @return           NIL or the timeout signal
 *
 *   @par Globals:
 *                     tmo
 */
/* ===================================================================== */
static union SIGNAL *
        remove_tmo(tmo_entry_t *entry, int get_signal)
{
    union SIGNAL *sig = NIL;
    int ret;

    assert(entry->timer_id);
    ret = timer_delete(entry->timer_id);
    if ( ret == -1 )
    {
        lits_error("timer_delete failed, %s(%d)", strerror(errno), errno);
    }

    if (get_signal)
    {
        sig = entry->usersig;
        if (NULL == sig)
        {
            sig = alloc(sizeof(struct tmo_sig), entry->signo);
        }
    }

    /* The signal may be sent. */
    if(entry->wrap)
    {
        free_buf(&entry->wrap);
    }
    return sig;
}

/** ==================================================================== */
/**
 * add attach of a client and add it to list
 *
 *   @param tmo        tmo list
 *
 *   @param client     client PID
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static void
add_attach(tmo_t *tmo, PROCESS client)
{
    attaches_t *new_attach;

    if((new_attach = malloc(sizeof(attaches_t))) == NULL)
    {
	lits_error("Cannot allocate memory for tmo server attach");
    }
    new_attach->client = client;
    new_attach->attref = attach(NULL, client);
    new_attach->next   = tmo->attaches;
    tmo->attaches      = new_attach;
}

/** ==================================================================== */
/**
 * find an attach to a client
 *
 *   @param tmo        tmo list
 *
 *   @param client     client PID
 *
 *   @return           attach entry from list or NULL if no entry
 *                     exists for this client.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static attaches_t *
find_attach(tmo_t *tmo, PROCESS client)
{
    attaches_t *att_entry;

    for (att_entry = tmo->attaches; att_entry; att_entry = att_entry->next)
    {
	if ( att_entry->client == client )
	    break;
    }

    return att_entry;
}

/** ==================================================================== */
/**
 * remove attach of a client and add it to list
 *
 *   @param tmo        tmo list
 *
 *   @param client     client PID
 *
 *   @param do_detach  Should detach be called or not
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static void
remove_attach(tmo_t *tmo, PROCESS client, bool do_detach)
{
    attaches_t *att_entry, **prev_entry;

    prev_entry = &tmo->attaches;

    for (att_entry = tmo->attaches; att_entry; att_entry = att_entry->next)
    {
	if ( att_entry->client == client )
	{
	    *prev_entry = att_entry->next;
	    break;
	}
	prev_entry = &att_entry->next;
    }

    if ( att_entry != NULL )
    {
	if ( do_detach )
	    detach(&att_entry->attref);

	free(att_entry);
    }
}

/** ==================================================================== */
/**
 * attach to a client if no attach already present
 *
 *   @param tmo        tmo list
 *
 *   @param client     client PID
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static void
attach_client(tmo_t *tmo, PROCESS client)
{
    if (find_attach(tmo, client) == NULL)
	add_attach(tmo, client);
}

/** ==================================================================== */
/**
 * detach from client if no more tmo entries is left
 *
 *   @param tmo        tmo list
 *
 *   @param client     client PID
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static void
detach_client(tmo_t *tmo, PROCESS client)
{
    if ( !pid_has_tmo_entry(tmo, client) )
	remove_attach(tmo, client, true);
}

/** ==================================================================== */
/**
 *   Timeout server process.
 *   Main thread for tmo handling.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(tmo_server)
{
    sigset_t    mask;
    int         maxsd, itcfd, sigfd;
    int         ret;
    fd_set      fds;
    tmo_t       tmo = {0, 0, NULL, NULL, NULL};
    tmo_entry_t  *entry;

    /* Block THREAD_TMO_SIGNAL and fetch THREAD_TMO_SIGNALs manually. */
    sigemptyset (&mask);
    sigaddset (&mask, THREAD_TMO_SIGNAL);
    ret = sigprocmask(SIG_BLOCK, &mask, NULL);
    assert(0 == ret);

    /* Get fd for ITC mailbox and UNIX signal. */
    sigemptyset (&mask);
    sigaddset (&mask, THREAD_TMO_SIGNAL);
    sigfd = signalfd(-1, &mask, 0);
    assert(sigfd != -1);
    itcfd = itc_get_fd();

    while(1)
    {
        FD_ZERO(&fds);
        FD_SET(itcfd, &fds);
        FD_SET(sigfd, &fds);
        maxsd = sigfd > itcfd ? sigfd : itcfd;
        /* Upper limit for select is 1024. */
        assert(maxsd < 1024);
        ret = select( maxsd + 1 , &fds , NULL , NULL , NULL);
        if(ret < 0)
        {
            if(errno ==EINTR)
            {
                continue;
            }
            lits_error("select error: %s", strerror(errno));
        }

        /* Tmo has expired. */
        if(FD_ISSET(sigfd, &fds))
        {
            struct signalfd_siginfo si;
            OSTMOREF ref;

            ret = read(sigfd, &si, sizeof(si));
            assert(ret == sizeof(si));
            assert(si.ssi_signo == THREAD_TMO_SIGNAL);
            ref = (OSTMOREF)si.ssi_int;
            entry = find_tmo_entry_ref(&tmo, ref);
            if(entry)
            {
                assert(entry->wrap);
                send_w_s(&entry->wrap, entry->client, entry->client);
                entry->wrap = NULL;
            }
        }
        /* ITC message has arrived. */
        else if(FD_ISSET(itcfd, &fds))
        {
            union SIGNAL *sig;
            SIGSELECT    selAll[] = {0};
            PROCESS      client;
#if 0
            sig = zzreceive(selAll);
#else
            /*
             * FIXME: temporary fix:
             * Due to a (probable) bug in um/itc select can return without
             * any data present.
             */
            sig = zzreceive_w_tmo(1, selAll);
            if(NULL == sig)
            {
                continue;
            }

#endif
            client = zzsender(&sig);
            switch(sig->signo)
            {
                case LITS_TMO_REQUEST:
                    entry = create_tmo_entry(client,
                                             sig->tmo_request.tmosigno,
                                             sig->tmo_request.tmosig,
                                             ++tmo.last_ref);
                    add_tmo_entry(&tmo, entry);
		    attach_client(&tmo, client);
                    start_tmo(sig->tmo_request.timeout, entry->timer_id);
                    sig->tmo_request.ref = entry->ref;
                    sig->tmo_request.status = 0;
                    sig->signo = LITS_TMO_REQUEST_R;
                    send(&sig, client);
                    break;

                case LITS_TMO_CANCEL:
                    /**
                     * *Note* Even though the OSE reference manual clearly states
                     *  that it is an error to cancel a timeout that has been expired,
                     *  it still allows it.
                     */
                    entry = find_tmo_entry_ref(&tmo, sig->tmo_cancel.ref);
                    if(NULL != entry)
                    {
                        sig->tmo_cancel.usersig = remove_tmo(entry, 1);
                        sig->tmo_cancel.status = 0;
                        remove_tmo_entry(&tmo, entry);
			detach_client(&tmo, client);
                    }
                    else
                    {
                        sig->tmo_cancel.status = -1;
                        sig->tmo_cancel.usersig = NULL;
                    }
                    sig->signo = LITS_TMO_CANCEL_R;
                    send(&sig, client);
                    break;

                case LITS_TMO_RESTART:
                    sig->tmo_restart.status = -1;
                    entry = find_tmo_entry_ref(&tmo, sig->tmo_restart.ref);
                    if(NULL != entry)
                    {
                        /* Create new timer from old values but new tmo ref. */
                        tmo_entry_t *new = create_tmo_entry(client,
                                                            entry->signo,
                                                            &entry->usersig,
                                                            ++tmo.last_ref);
                        /* Remove old timer. */
                        (void)remove_tmo(entry, 0);
                        remove_tmo_entry(&tmo, entry);
                        /* Add new timer. */
                        add_tmo_entry(&tmo, new);
                        start_tmo(sig->tmo_restart.timeout, new->timer_id);
                        sig->tmo_restart.ref = new->ref;
                        sig->tmo_restart.status = 0;
                    }
                    sig->signo = LITS_TMO_RESTART_R;
                    send(&sig, client);
                    break;

                case OS_ATTACH_SIG:
                    do
                    {
                        entry = find_tmo_entry_pid(&tmo, client);
                        if(NULL != entry)
                        {
                            (void)remove_tmo(entry, 0);
                            remove_tmo_entry(&tmo, entry);
                        }
                    }
                    while(entry);
		    remove_attach(&tmo, client, false);
                    free_buf(&sig);
                    break;

                default:
                    lits_print_message("LITS %s Unknown sig(%x) received, ignoring!",
                                       __FUNCTION__, sig->signo);
                    free_buf(&sig);
                    break;
            }
        }
        else
        {
            lits_error("LITS Internal error: %s, %d", __FILE__, __LINE__);
        }
    }
}

/* ==================================================================== */
/* Exported functions, please see tmoserver.h for details  */
/* ===================================================================== */
void
lits_tmo_init(void)
{
    tmo_server_pid = create_process(OS_PRI_PROC, "lits-tmo-server",
                                    tmo_server, 20000, 0, 0, 0, 0, 0, 0);
    (void)attach(NULL, tmo_server_pid);
    start(tmo_server_pid);
}

inline int
lits_tmo_remove(union SIGNAL **sig)
{
    union SIGNAL *usersig;
    OSTMOREF     ref;

    if( 0 == lits_tmo_user                      ||
            sigsize(sig) != sizeof(struct tmo_wrap) ||
	    strcmp((*sig)->tmo_wrap.magic, LITS_TMO_MAGIC) != 0)
    {
        return 0;
    }

    ref = (*sig)->tmo_wrap.ref;
    usersig = tmo_server_cancel(ref);
    /* Free wrap. Zero it so that the magic word does not get reused
       by a signal that does not reinitialise all data. */
    memset(*sig, 0, sizeof(struct tmo_wrap));
    free_buf(sig);

    *sig = usersig;
    return *sig ? 0 : 1;
}

OSTMOREF
tmo_server_request(OSTIME timeout, SIGSELECT tmosigno, union SIGNAL **tmosig)
{
    OSTMOREF     ref;
    SIGSELECT    msel[] = {1, LITS_TMO_REQUEST_R};
    union SIGNAL *sig;

    lits_tmo_user = 1;
    assert(0 != tmo_server_pid);
    sig = zzalloc(sizeof(struct tmo_request), LITS_TMO_REQUEST);
    sig->tmo_request.timeout = timeout;
    sig->tmo_request.tmosigno = tmosigno;
    sig->tmo_request.tmosig = tmosig;
    send(&sig, tmo_server_pid);
    sig = receive(msel);
    assert(0 == sig->tmo_request.status);
    ref = sig->tmo_request.ref;
    free_buf(&sig);

    return ref;
}

OSTMOREF
tmo_server_restart(OSTMOREF tmoref, OSTIME timeout)
{
    OSTMOREF     ref;
    SIGSELECT    msel[] = {1, LITS_TMO_RESTART_R};
    union SIGNAL *sig;

    assert(0 != tmo_server_pid);
    sig = zzalloc(sizeof(struct tmo_restart), LITS_TMO_RESTART);
    sig->tmo_restart.timeout = timeout;
    sig->tmo_restart.ref = tmoref;
    send(&sig, tmo_server_pid);
    sig = receive(msel);
    if (0 != sig->tmo_restart.status)
    {
        lits_error("Illegal tmoref (0x%x)", tmoref);
    }
    ref = sig->tmo_restart.ref;
    free_buf(&sig);

    return ref;
}

union SIGNAL*
        tmo_server_cancel(OSTMOREF tmoref)
{
    SIGSELECT    msel[] = {1, LITS_TMO_CANCEL_R};
    union SIGNAL *sig;
    union SIGNAL *ret;

    assert(0 != tmo_server_pid);
    sig = zzalloc(sizeof(struct tmo_cancel), LITS_TMO_CANCEL);
    sig->tmo_cancel.ref = tmoref;
    send(&sig, tmo_server_pid);
    sig = receive(msel);
    ret = sig->tmo_cancel.usersig;
    free_buf(&sig);

    return ret;
}

PROCESS
get_tmo_server_pid(void)
{
    assert(0 != tmo_server_pid);
    return tmo_server_pid;
}

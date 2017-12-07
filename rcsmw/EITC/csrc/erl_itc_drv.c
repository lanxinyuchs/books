/* -*- C -*-
** %EricssonCopyright%
** %CopyrightBegin%
**
** Copyright Ericsson AB 2013-2016. All Rights Reserved.
**
** The program may be used and/or copied only with the written permission from
** Ericsson AB, or in accordance with the terms and conditions stipulated in
** the agreement/contract under which the program has been supplied.
**
** %CopyrightEnd%
*/
#include <stdio.h>
#include <string.h>

#include "itc.h"
#include "itc_system.h"
#include "erl_driver.h"

#define OTP_ITC_MESSAGE_NR_RANGE_BASE 0x01900200
#define OTP_ITC_MESSAGE_NR_RANGE_SIZE 0x100

/* Stack size in kiloWords, -1 gives OS default */
#ifndef ITC_DRV_STACK_SIZE
#define ITC_DRV_STACK_SIZE -1
#endif

/* itc_receive() timeout (ms) for poll of drop_dead. XXX BAD CLUDGE */
#define ITC_DRV_RECEIVE_TIMEOUT 1000

#define FALSE 0
#define TRUE (! FALSE)

/* About the implementation
** ========================
**
** The driver has ERL_DRV_FLAG_USE_PORT_LOCKING set.
** Therefore can only one scheduler thread at the time be inside
** any port callback e.g port_control so no locking of the
** ItcDrvState fields is needed.
**
** A handler thread running itc_drv_handler_thread is used for
** each ITC mailbox since only threads can own mailboxes
** and the scheduler threads can not own mailboxes because
** those would be impossible to delete when needed.
**
** All operations except create and stop are acknowledged from
** the handler thread to the calling process  using erl_drv_send_term().
**
** This acknowledge is essential or else there is a potential causality
** problem when one erlang process e.g calls itc_drv_control_send
** that sends a message to the handler_thread for final send,
** returns to the scheduler, gets scheduled out and then in on another
** sheduler thread (which is very unlikely) and calls itc_drv_control_send
** again.  In this case there is no guarantee that the order between
** the first and the second message will be kept since the sends are
** from different threads.  That guarantee is enforced by the
** erlang code waiting for an ack from the handler thread.
**
** This schedule out and in scenario is also the reason why the original
** design choice of sending from one mailbox per scheduler thread using
** faked sender was abandoned in favor of sending all messages to be sent
** to the handler_thread for actual sending.  Then the order is guaranteed.
**
** Another design choice that had to be changed was to not reserve specific
** internal ITC message numbers.  That proved impossible since when the
** handler_thread has to receive from all mailboxes and if the end user
** specifies a filter list that filter list has to be augumented with the
** internal message codes that may accidentally collide with real application
** message codes.  The internal messages were intended to be recognized by
** having a particular sender mailbox.  But if a received message would
** turn out to be not internal since not from the particular sender mailbox,
** and not on the filter list can not be unreceived.  The solution is
** to reserve ITC message numbers for internal driver use.
**
**
** The implementation
** ==================
**
** When a port is created it starts an itc_drv_handler_thread that creates
** the mailbox that receives ITC messages.  This thread mostly sits in
** itc_receive and awaits messages or commands in the guise of control
** messages i.e messages with a reserved message no ITC_DRV_MSG.
**
** This handler_thread sends received ITC messages directly to the
** destination Erlang process, which is only possible in the SMP VM.
** It sends other acknowledge messages and results to the caller
** Erlang process.  The messages MSG_LISTEN and MSG_UNLISTEN changes
** the filter list to use in the next itc_receive.
**
** The handler_thread acknowledges most control messages back to
** the calling erlang process by sending directly to it.
** Discussion in the previous section.
**
** Whenever a VM thread wants to send an ITC message it is sent with
** a fake sender (the receiver) to the handler_thread that resends it
** to the real destination.  Discussion in the previous section.
**
** Before calling itc_exit() this driver relies on the erlang code
** first blocking creation of new mailboxes and then killing all
** ITC mailboxes.  These are registered in the driver and reported
** up to erlang in preparation of exit i.e itc_drv_control_exiting.
** Thereafter itc_exit() can succede.
*/

enum itc_drv_msgnos {
    /* This is the only msgno used but it has submessages */
    ITC_DRV_MSG = (OTP_ITC_MESSAGE_NR_RANGE_BASE + 1)
};
/* These are for the subfield emsgno */
enum itc_drv_emsgnos {
    MSG_STOP = 1,
    MSG_LOCATE,
    MSG_MONITOR,
    MSG_SEND,
    MSG_LISTEN,
    MSG_UNLISTEN,
    MSG_UNLOCATE,
    MSG_UNMONITOR,
    MSG_LOCATED,
    MSG_MONITOR_DOWN,
    MSG_GET_NAME
};

/* The _*_ fields below are placeholders and not intended
** to be used from that struct.  E.g msgno is intended to be used
** from msg->msgno and not from msg->locate.msgno, and emsgno
** should be used as msg->hdr.emsgno but not as msg->locate.emsgno.
*/

struct msg_empty { /* Used for application messages to get the size */
    uint32_t _msgno_;
};

struct msg_generic { /* Used for application messages */
  uint32_t _msgno_;
  char data[];
};

struct msg_hdr {
    uint32_t _msgno_;  /* ITC_DRV_MSG */
    /**/
    uint32_t emsgno; /* MSG_STOP | MSG_UNLOCATE | MSG_UNMONITOR */
    ErlDrvTermData caller;
    uint32_t ref;
};

struct msg_locate {
    uint32_t _msgno_;  /* ITC_DRV_MSG */
    uint32_t _emsgno_; /* MSG_LOCATE | MSG_LOCATED */
    ErlDrvTermData _caller_;
    uint32_t _ref_;
    /**/
    char name[];
};

struct msg_monitor {
    uint32_t _msgno_;  /* ITC_DRV_MSG */
    uint32_t _emsgno_; /* MSG_MONITOR | MSG_MONITOR_DOWN */
    ErlDrvTermData _caller_;
    uint32_t _ref_;
    /**/
    itc_mbox_id_t mbox_id;
};

struct msg_send {
    uint32_t _msgno_;  /* ITC_DRV_MSG */
    uint32_t _emsgno_; /* MSG_SEND */
    ErlDrvTermData _caller_;
    uint32_t _ref_;
    /**/
    union itc_msg *msg;
    itc_mbox_id_t to;
    itc_mbox_id_t from;
};

struct msg_listen {
    uint32_t _msgno_;  /* ITC_DRV_MSG */
    uint32_t _emsgno_; /* MSG_LISTEN | MSG_UNLISTEN */
    ErlDrvTermData _caller_;
    uint32_t _ref_;
    /**/
    uint32_t num_msgnos;
    uint32_t msgnos[];
};

struct msg_get_name {
    uint32_t _msgno_;  /* ITC_DRV_MSG */
    uint32_t _emsgno_; /* MSG_GET_NAME */
    ErlDrvTermData _caller_;
    uint32_t _ref_;
    /**/
    itc_mbox_id_t mbox_id;
};

union itc_msg {
    uint32_t msgno;
    struct msg_empty empty;
    struct msg_generic generic;
    struct msg_hdr hdr;
    struct msg_locate locate;
    struct msg_monitor monitor;
    struct msg_send send;
    struct msg_listen listen;
    struct msg_get_name get_name;
};


/* Common for handler thread and VM scheduler thread */
typedef struct {
    ErlDrvTid tid;
    ErlDrvMutex *lock; /* Locks the below */
    itc_mbox_id_t mbox_id;
    ErlDrvCond *cond;
    int drop_dead;
} ItcDrvHandler;

typedef struct itc_drv_state_struct {
    /* These are set at port start then read only */
    ErlDrvPort port;
    ErlDrvTermData self; /* port as port_id() */

    /* Set by itc_drv_control_create.
     * When set the rest of the fields below are initialized. */
    char *name;

    uint32_t flags;
    ErlDrvTermData destination; /* pid() that gets the {message,...} */
    uint32_t ref;

    ItcDrvHandler handler;

    /* Locked by itc_drv_lock */
    struct itc_drv_state_struct *next;
    struct itc_drv_state_struct *prev;
} ItcDrvState;

static void itc_drv_ref_init(uint32_t *ref) {
    *ref = 3037000499; /* (1<<32)/scrt(2) just for fun */
}
static void itc_drv_ref_next(uint32_t *ref) {
    (*ref) += 2147483647; /* ((1<<31) - 1) - a prime number */
}


static ErlDrvThreadOpts *itc_drv_thread_opts;

/* This global lock locks the immediately following fields and function */
static ErlDrvMutex *itc_drv_lock;
static enum {
    ITC_DRV_EXITED = 0,
    ITC_DRV_INITIALIZED,
    ITC_DRV_EXITING
} itc_drv_state;
static ItcDrvState itc_drv_ring; /* Port ring sentinel */

static void itc_drv_ring_splice_next(ItcDrvState *a, ItcDrvState *b) {
    ItcDrvState *c, *d;

    c = a->next;
    a->next = d = b->next;
    b->next = c;
    a = c->prev;
    c->prev = d->prev;
    d->prev = a;
}


/* *************************************************************************
** Small helpers
*/

#ifdef DEBUG
#define ASSERT(X)							\
    do {								\
	if (! (X)) {							\
	    fprintf							\
		(stderr,						\
		 "Assertion (%s) failed at %s:%d\r\n",			\
		 #X, __FILE__, __LINE__ );				\
	    abort();							\
	}								\
    } while (FALSE)
static int debug_alert(int x, char *expr, char *file, int line) {
    if (x) {
	fprintf
	    (stderr, "Alert (%s) == %d at %s:%d\r\n",
	     expr, x, file, line);
    }
    return x;
}
/* Pass through int argument and debug print if nonzero */
#define ALERT(X) debug_alert((X), #X, __FILE__, __LINE__)
#define DPRINTF(...) ((void) fprintf(stderr, __VA_ARGS__))
#else
#define ASSERT(X) do { (void) (X); } while (FALSE)
#define ALERT(X) ((int) (X))
#define DPRINTF(...) ((void) 0)
#endif

/*
** Malloc wrappers - "safe" i.e do not return NULL but abort()
*/

static void *itc_drv_alloc(ErlDrvSizeT size)
{
  void *ptr;

  if ((ptr = driver_alloc(size)) == NULL) {
    fprintf
      (stderr,
       DRIVER_NAME_STRING
       ": Could not allocate %lu bytes of memory!\n", (unsigned long) size);
    abort();
  }
  return ptr;
}

static ErlDrvBinary *itc_drv_alloc_binary(ErlDrvSizeT size)
{
  ErlDrvBinary *ptr;

  if ((ptr = driver_alloc_binary(size)) == NULL) {
    fprintf
      (stderr,
       DRIVER_NAME_STRING
       ": Could not allocate a binary of %lu bytes!\n", (unsigned long) size);
    abort();
  }
  return ptr;
}

#define ALLOC(size) itc_drv_alloc(size)
#define ALLOC_BINARY(size) itc_drv_alloc_binary(size)
#define FREE(ptr) driver_free(ptr)

static union itc_msg *itc_drv_alloc_msg(size_t size, uint32_t emsgno)
{
  union itc_msg *msg;
  msg = itc_alloc(size, ITC_DRV_MSG);
  msg->hdr.emsgno = emsgno;
  return msg;
}


static uint32_t get_uint32(char *p)
{
    unsigned char *up;

    up = (unsigned char *) p;
    return
	(((uint32_t) up[0]) << 24) |
	(((uint32_t) up[1]) << 16) |
	(((uint32_t) up[2]) << 8) |
	((uint32_t) up[3]);
}

static void put_uint32(char *p, uint32_t v)
{
    unsigned char *up;

    up = (unsigned char *) p;
    up[0] = (unsigned char) (v >> 24);
    up[1] = (unsigned char) (v >> 16);
    up[2] = (unsigned char) (v >> 8);
    up[3] = (unsigned char) v;
}


/* Atom handling */

#define ATOM(atom) atom_ ## atom
#define ATOM_STR(atom) atom_str_ ## atom
#define DECL_ATOM(atom)				\
    static ErlDrvTermData ATOM(atom);		\
    static char ATOM_STR(atom)[] = # atom
#define INIT_ATOM(atom)						\
    do { ATOM(atom) = driver_mk_atom(ATOM_STR(atom)); } while (FALSE)
#define LEN_ATOM_STR(atom) (sizeof(ATOM_STR(atom)) - 1)

#define return_ATOM_STR(atom, rbuf, rlen)				\
    do {								\
	char *ptr;							\
	ErlDrvSizeT alen;						\
	alen = LEN_ATOM_STR(atom);					\
	ptr = itc_drv_control_ensure_rbuf((rbuf), (rlen), 1+alen);	\
	ptr[0] = 'a';							\
	memcpy(ptr+1, ATOM_STR(atom), alen);				\
	return 1+alen;							\
    } while (FALSE)

#define return_ref_ATOM_STR(atom, ref, rbuf, rlen)			\
    do {								\
	char *ptr;							\
	ErlDrvSizeT alen;						\
	alen = LEN_ATOM_STR(atom);					\
	ptr =								\
	    itc_drv_control_ensure_rbuf					\
	    ((rbuf), (rlen), 1+sizeof(uint32_t)+alen);			\
	ptr[0] = 'r';							\
	put_uint32(ptr+1, (ref));					\
	memcpy(ptr+1+sizeof(uint32_t), ATOM_STR(atom), alen);		\
	return 1+sizeof(uint32_t)+alen;					\
    } while (FALSE)


static void itc_drv_send(union itc_msg ** msg, itc_mbox_id_t to) {
    itc_send(msg, to, to);
}


/* *************************************************************************
** Helpers and workers
*/

/* These must match the INIT_ATOM(*)s in itc_drv_init() further down */
DECL_ATOM(message);
DECL_ATOM(mailbox_up);
DECL_ATOM(mailbox_down);
DECL_ATOM(locate_ref);
DECL_ATOM(monitor_ref);
DECL_ATOM(get_name_ref);
DECL_ATOM(send_ref);
DECL_ATOM(listen_ref);
DECL_ATOM(unlisten_ref);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(undefined);
DECL_ATOM(already_initialized);
DECL_ATOM(already_exited);
DECL_ATOM(already_exiting);
DECL_ATOM(exiting);
DECL_ATOM(exiting_ref);

/* ITC mailbox handling thread (owns a mailbox) */
static void *itc_drv_handler_thread(void *arg)
{
    ItcDrvState *s;
    ErlDrvTermData message[17];
    ErlDrvTermData mailbox[16];
    ErlDrvTermData ack[8];
    ErlDrvTermData get_name[13];
    uint32_t *filter;
    char *namebuf;
    struct locate_item {
	struct locate_item *next;
	uint32_t ref;
    } *locate_list, *locate_list_tail;
    struct monitor_item {
	struct monitor_item *next;
	uint32_t ref;
	itc_monitor_id_t monitor_id;
    } *monitor_list, *monitor_list_tail;

    s = (ItcDrvState *) arg;

    /* {message,Port,{Sender,Receiver,Msgno,Data}} */
    /* Start ERL_DRV_TUPLE 3 */
    message[0] = ERL_DRV_ATOM;
    message[1] = ATOM(message);
    /**/
    message[2] = ERL_DRV_PORT;
    message[3] = s->self;
    /**/
    /* Start ERL_DRV_TUPLE 4 */
    message[4] = ERL_DRV_UINT;
    /* message[5] = SenderId */
    /**/
    message[6] = ERL_DRV_UINT;
    /* message[7] = ReceiverId */
    /**/
    message[8] = ERL_DRV_UINT;
    /*message[9] = SigNo */
    /**/
    message[10] = ERL_DRV_BUF2BINARY;
    /* message[11] = buf */
    /* message[12] = size */
    /**/
    message[13] = ERL_DRV_TUPLE;
    message[14] = 4;
    /**/
    message[15] = ERL_DRV_TUPLE;
    message[16] = 3;

    /* {mailbox_up|mailbox_down, Port, {monitor_ref,Port,Ref}, MboxId|true|false} */
    /* Start ERL_DRV_TUPLE 4 */
    mailbox[0] = ERL_DRV_ATOM;
    /* mailbox[1] = ATOM(mailbox_up) | ATOM(mailbox_down); */
    /**/
    mailbox[2] = ERL_DRV_PORT;
    mailbox[3] = s->self;
    /**/
    /* Start ERL_DRV_TUPLE 3 */
    mailbox[4] = ERL_DRV_ATOM;
    /* mailbox[5] = ATOM(locate_ref) | ATOM(monitor_ref); */
    /**/
    mailbox[6] = ERL_DRV_PORT;
    mailbox[7] = s->self;
    /**/
    mailbox[8] = ERL_DRV_UINT;
    /* mailbox[9] = LocateRef | MonitorRef */
    /**/
    mailbox[10] = ERL_DRV_TUPLE;
    mailbox[11] = 3;
    /**/
    /* mailbox[12] = ERL_DRV_UINT | ERL_DRV_ATOM; */
    /* mailbox[13] = MboxId | ('true' | 'false') */
    /**/
    mailbox[14] = ERL_DRV_TUPLE;
    mailbox[15] = 4;

    /* {some_ref, Port, Ref} */
    ack[0] = ERL_DRV_ATOM;
    /* ack[1] = ATOM(some_ref) */
    /**/
    ack[2] = ERL_DRV_PORT;
    ack[3] = s->self;
    /**/
    ack[4] = ERL_DRV_UINT;
    /* ack[5] = SomeRef */
    /**/
    ack[6] = ERL_DRV_TUPLE;
    ack[7] = 3;
#define SEND_ACK(caller, atom, ref)		\
    do {					\
        ack[1] = (atom);			\
	ack[5] = (ref);				\
	ASSERT					\
	  (erl_drv_send_term			\
	   (s->self, (caller),			\
	    ack, sizeof(ack) / sizeof (*ack))	\
	   >= 0);				\
    } while (FALSE)

    /* {{get_name_ref,Port,Ref}, Name :: binary() | string()} */
    /* Start ERL_DRV_TUPLE 2 */
    /* Start ERL_DRV_TUPLE 3 */
    get_name[0] = ERL_DRV_ATOM;
    get_name[1] = ATOM(get_name_ref);
    /**/
    get_name[2] = ERL_DRV_PORT;
    get_name[3] = s->self;
    /**/
    get_name[4] = ERL_DRV_UINT;
    /* get_name[5] = GetNameRef */
    /**/
    get_name[6] = ERL_DRV_TUPLE;
    get_name[7] = 3;
    /**/
    /* get_name[8] = ERL_DRV_BUF2BINARY | ERL_DRV_STRING; */
    /* get_name[9] = buf */
    /* get_name[10] = size */
    /**/
    get_name[11] = ERL_DRV_TUPLE;
    get_name[12] = 2;
    
    filter = ALLOC((1 + 1) * sizeof(*filter));
    filter[0] = 1;
    filter[1] = ITC_DRV_MSG;
    locate_list = locate_list_tail = NULL;
    monitor_list = monitor_list_tail = NULL;
    namebuf = ALLOC(ITC_NAME_MAXLEN + 1);

    erl_drv_mutex_lock(s->handler.lock);
    s->handler.mbox_id = itc_create_mailbox(s->name, s->flags);
    erl_drv_cond_signal(s->handler.cond);
    erl_drv_mutex_unlock(s->handler.lock);

    for (;;) {
	union itc_msg *msg;
	size_t size;

	msg = itc_receive(filter, ITC_DRV_RECEIVE_TIMEOUT, ITC_FROM_ALL);
	if (msg == NULL) {
	    erl_drv_mutex_lock(s->handler.lock);
	    if (s->handler.drop_dead) {
	        erl_drv_mutex_unlock(s->handler.lock);
		goto done;
	    }
	    erl_drv_mutex_unlock(s->handler.lock);
	    continue;
	}
	size = itc_size(msg);
	if (size < sizeof(msg->empty)) {
	    DPRINTF
		("itc_drv_handler_thread() "
		 "size:%lu < sizeof(msg->msgno):%lu\r\n",
		 (unsigned long) size, (unsigned long) sizeof(msg->msgno));
	    /* This is not possible - silently ignore broken message */
	    itc_free(&msg);
	    continue;
	}
	if (msg->msgno != ITC_DRV_MSG) {
	    message[5] = itc_sender(msg);
	    message[7] = itc_receiver(msg);
	    message[9] = msg->msgno;
	    message[11] = (ErlDrvTermData) msg->generic.data;
	    message[12] = size - sizeof(msg->empty);
	    ASSERT
		(erl_drv_send_term
		 (s->self, s->destination,
		  message, sizeof(message) / sizeof(*message))
		 >= 0);
	    itc_free(&msg);
	    continue;
	}
	if (size < sizeof(msg->hdr)) {
	    DPRINTF
		("itc_drv_handler_thread() "
		 "size:%lu < sizeof(msg->hdr):%lu\r\n",
		 (unsigned long) size, (unsigned long) sizeof(msg->hdr));
	    /* Too short control message - silently ignore */
	    itc_free(&msg);
	    continue;
	}

	switch (msg->hdr.emsgno) {
	case MSG_STOP: {
	    itc_free(&msg);
	    goto done;
	}
	case MSG_GET_NAME: {
	    if (itc_get_name
		(msg->get_name.mbox_id, namebuf, ITC_NAME_MAXLEN)) {
	        /* Name found */
		get_name[5] = msg->hdr.ref;
		get_name[8] = ERL_DRV_BUF2BINARY;
		get_name[9] = (ErlDrvTermData) namebuf;
		get_name[10] = strnlen(namebuf, ITC_NAME_MAXLEN);
	    }
	    else {
	        /* Name not found */
		get_name[5] = msg->hdr.ref;
	        get_name[8] = ERL_DRV_STRING;
		get_name[9] = (ErlDrvTermData) ATOM_STR(undefined);
		get_name[10] = LEN_ATOM_STR(undefined);
	    }
	    ASSERT
	        (erl_drv_send_term
		 (s->self, msg->hdr.caller,
		  get_name, sizeof(get_name) / sizeof (*get_name))
		 >= 0);
	    itc_free(&msg);
	    continue;
	}
	case MSG_LOCATE: {
	    struct locate_item *i;
	    ErlDrvTermData caller;
	    
	    /* Create and insert a new locate item */
	    i = ALLOC(sizeof(*i));
	    i->next = NULL;
	    i->ref = msg->hdr.ref;
	    if (locate_list == NULL) locate_list = locate_list_tail = i;
	    else locate_list_tail = locate_list_tail->next = i;

	    caller = msg->hdr.caller;
	    msg->hdr.emsgno = MSG_LOCATED;
	    itc_locate_async(msg->locate.name, &msg, ITC_MY_MBOX);

	    SEND_ACK(caller, ATOM(locate_ref), i->ref);
	    continue;
	}
	case MSG_UNLOCATE: {
	    struct locate_item *i, *p;
	    
	    /* Find msg->hdr.ref in linked list */
	    for (i = locate_list, p = NULL;
		 i != NULL;
		 p = i, i = i->next) {
		if (i->ref == msg->hdr.ref) break;
	    }
	    if (i != NULL) {
		/* Lift out locate_item from linked list */
		if (i == locate_list) locate_list = i->next;
		else p->next = i->next;
		if (i == locate_list_tail) locate_list_tail = p;
		FREE(i);
	    }
	    
	    /* Send a special internal {mailbox_up,...} */
	    mailbox[1] = ATOM(mailbox_up);
	    mailbox[5] = ATOM(locate_ref);
	    mailbox[9] = msg->hdr.ref;
	    mailbox[12] = ERL_DRV_ATOM;
	    /* Report if the locate was found hence had not fired */
	    mailbox[13] = (i != NULL) ? ATOM(true) : ATOM(false);
	    ASSERT
		(erl_drv_send_term
		 (s->self, msg->hdr.caller,
		  mailbox, sizeof(mailbox) / sizeof (*mailbox))
		 >= 0);
	    itc_free(&msg);
	    continue;
	}
	case MSG_LOCATED: {
	    struct locate_item *i, *p;

	    /* Find msg->hdr.ref in linked list */
	    for (i = locate_list, p = NULL;  i != NULL;  p = i, i = i->next) {
		if (i->ref == msg->hdr.ref) break;
	    }
	    if (i == NULL) {
		/* Locate ref not found - ignore message */
		itc_free(&msg);
		continue;
	    }

	    /* Lift out locate_item from linked list */
	    if (i == locate_list) locate_list = i->next;
	    else p->next = i->next;
	    if (i == locate_list_tail) locate_list_tail = p;
	    FREE(i);
	    
	    mailbox[1] = ATOM(mailbox_up);
	    mailbox[5] = ATOM(locate_ref);
	    mailbox[9] = msg->hdr.ref;
	    mailbox[12] = ERL_DRV_UINT;
	    mailbox[13] = itc_sender(msg);
	    ASSERT
		(erl_drv_send_term
		 (s->self, msg->hdr.caller,
		  mailbox, sizeof(mailbox) / sizeof (*mailbox))
		 >= 0);
	    itc_free(&msg);
	    continue;
	}
	case MSG_MONITOR: {
	    struct monitor_item *i;
	    ErlDrvTermData caller;
	    
	    /* Create and insert a new locate item */
	    i = ALLOC(sizeof(*i));
	    i->next = NULL;
	    i->ref = msg->hdr.ref;
	    if (monitor_list == NULL) monitor_list = monitor_list_tail = i;
	    else monitor_list_tail = monitor_list_tail->next = i;
	    
	    caller = msg->hdr.caller;
	    msg->hdr.emsgno = MSG_MONITOR_DOWN;
	    i->monitor_id = itc_monitor(msg->monitor.mbox_id, &msg);

	    SEND_ACK(caller, ATOM(monitor_ref), i->ref);
	    continue;
	}
	case MSG_UNMONITOR:{
	    struct monitor_item *i, *p;
	    
	    /* Find msg->hdr.ref in linked list */
	    for (i = monitor_list, p = NULL;
		 i != NULL;
		 p = i, i = i->next) {
		if (i->ref == msg->hdr.ref) break;
	    }
	    if (i != NULL) {
		/* Lift out locate_item from linked list */
		if (i == monitor_list) monitor_list = i->next;
		else p->next = i->next;
		if (i == monitor_list_tail) monitor_list_tail = p;
		/* Cancel the monitor */
		itc_unmonitor(i->monitor_id);
		FREE(i);
	    }
	    
	    /* Send a special internal {mailbox_down,...} */
	    mailbox[1] = ATOM(mailbox_down);
	    mailbox[5] = ATOM(monitor_ref);
	    mailbox[9] = msg->hdr.ref;
	    mailbox[12] = ERL_DRV_ATOM;
	    /* Report if the locate was found hence had not fired */
	    mailbox[13] = (i != NULL) ? ATOM(true) : ATOM(false);
	    ASSERT
		(erl_drv_send_term
		 (s->self, msg->hdr.caller,
		  mailbox, sizeof(mailbox) / sizeof (*mailbox))
		 >= 0);
	    itc_free(&msg);
	    continue;
	}
	case MSG_MONITOR_DOWN: {
	    struct monitor_item *i, *p;

	    /* Find msg->hdr.ref in linked list */
	    for (i = monitor_list, p = NULL;  i != NULL;  p = i, i = i->next) {
		if (i->ref == msg->hdr.ref) break;
	    }
	    if (i == NULL) {
		/* Monitor ref not found - ignore message */
		itc_free(&msg);
		continue;
	    }

	    /* Lift out monitor_item from linked list */
	    if (i == monitor_list) monitor_list = i->next;
	    else p->next = i->next;
	    if (i == monitor_list_tail) monitor_list_tail = p;
	    FREE(i);
	    
	    mailbox[1] = ATOM(mailbox_down);
	    mailbox[5] = ATOM(monitor_ref);
	    mailbox[9] = msg->hdr.ref;
	    mailbox[12] = ERL_DRV_UINT;
	    mailbox[13] = itc_sender(msg);
	    ASSERT
		(erl_drv_send_term
		 (s->self, msg->hdr.caller,
		  mailbox, sizeof(mailbox) / sizeof (*mailbox))
		 >= 0);
	    itc_free(&msg);
	    continue;
	}
	case MSG_UNLISTEN: {
	    size_t i, j;
	    ErlDrvTermData caller;
	    uint32_t ref;

	    caller = msg->hdr.caller;
	    ref = msg->hdr.ref;

	    FREE(filter);
	    filter = ALLOC((1 + msg->listen.num_msgnos) * sizeof(*filter));
	    for (i = 0, j = 1;  i < msg->listen.num_msgnos;  i++) {
	      /* Do not filter out control messages */
	      if (msg->listen.msgnos[i] != ITC_DRV_MSG) {
		filter[j++] = msg->listen.msgnos[i];
	      }
	    }
	    filter[0] = (uint32_t) (- ((int32_t) (j - 1)));
	    itc_free(&msg);

	    SEND_ACK(caller, ATOM(unlisten_ref), ref);
	    continue;
	}
	case MSG_LISTEN: {
	    size_t i, j;
	    ErlDrvTermData caller;
	    uint32_t ref;

	    caller = msg->hdr.caller;
	    ref = msg->hdr.ref;

	    FREE(filter);
	    filter =
		ALLOC((1 + 1 + msg->listen.num_msgnos) * sizeof(*filter));
	    for (i = 0, j = 2;  i < msg->listen.num_msgnos;  i++) {
	      /* Do not duplicate control message filter */
	      if (msg->listen.msgnos[i] != ITC_DRV_MSG) {
		filter[j++] = msg->listen.msgnos[i];
	      }
	    }
	    filter[1] = ITC_DRV_MSG;
	    filter[0] = j - 1;
	    itc_free(&msg);

	    SEND_ACK(caller, ATOM(listen_ref), ref);
	    continue;
	}
	case MSG_SEND: {
	    ErlDrvTermData caller;
	    uint32_t ref;

	    caller = msg->hdr.caller;
	    ref = msg->hdr.ref;
	    itc_send(&msg->send.msg, msg->send.to, msg->send.from);
	    itc_free(&msg);

	    SEND_ACK(caller, ATOM(send_ref), ref);
	    continue;
	}
	default:
	    message[5] = itc_sender(msg);
	    message[7] = itc_receiver(msg);
	    message[9] = msg->msgno;
	    message[11] = (ErlDrvTermData) msg->generic.data;
	    message[12] = size - sizeof(msg->empty);
	    ASSERT
		(erl_drv_send_term
		 (s->self, s->destination,
		  message, sizeof(message) / sizeof(*message))
		 >= 0);
	    itc_free(&msg);
	    continue;
	}
    }

 done:
    FREE(namebuf);
    FREE(filter);
    itc_delete_mailbox(s->handler.mbox_id);
    return NULL;
}



static char *itc_drv_control_ensure_rbuf
(char **rbuf, ErlDrvSizeT rlen, ErlDrvSizeT size)
{
    if (rlen < size) {
	ErlDrvBinary *bin;

	bin = ALLOC_BINARY(size);
	(*rbuf) = (char *) bin;
	return bin->orig_bytes;
    }
    else {
	return *rbuf;
    }
}

/* Dummy memory allocation scheme info function */
static void *itc_drv_alloc_info(void)
{
    return NULL;
}



/* *************************************************************************
** Driver callbacks
*/

/* erl_ddll:load_driver */
static int itc_drv_init(void)
{
    ErlDrvSysInfo sys_info;

    DPRINTF("itc_drv_init()\r\n");

    /* Check if we are friends with the Erlang VM */
    driver_system_info(&sys_info, sizeof(sys_info));
    if (ALERT((! sys_info.smp_support) || (! sys_info.thread_support))) {
	return -1;
    }

    itc_drv_state = ITC_DRV_EXITED;
    itc_drv_lock = erl_drv_mutex_create("itc_drv");
    if (ALERT(itc_drv_lock == NULL)) return -1;
    itc_drv_thread_opts = erl_drv_thread_opts_create(DRIVER_NAME_STRING);
    if (itc_drv_thread_opts != NULL) {
	itc_drv_thread_opts->suggested_stack_size = ITC_DRV_STACK_SIZE;
    }

    /* These must match DECL_ATOM(*) before itc_drv_handler_thread above */
    INIT_ATOM(message);
    INIT_ATOM(mailbox_up);
    INIT_ATOM(mailbox_down);
    INIT_ATOM(locate_ref);
    INIT_ATOM(monitor_ref);
    INIT_ATOM(get_name_ref);
    INIT_ATOM(send_ref);
    INIT_ATOM(listen_ref);
    INIT_ATOM(unlisten_ref);
    INIT_ATOM(true);
    INIT_ATOM(false);
    INIT_ATOM(undefined);
    INIT_ATOM(already_initialized);
    INIT_ATOM(already_exited);
    INIT_ATOM(already_exiting);
    INIT_ATOM(exiting);
    INIT_ATOM(exiting_ref);

    return 0;
}

/* This one can only be called before a port has been opened
 * since itc_drv_start() will lock the driver into the VM
 */
/* erl_ddll:unload_driver */
static void itc_drv_finish(void)
{
    DPRINTF("itc_drv_finish()\r\n");

    if (itc_drv_lock != NULL) {
	erl_drv_mutex_destroy(itc_drv_lock);
	itc_drv_lock = NULL;
    }
    if (itc_drv_thread_opts != NULL) {
	erl_drv_thread_opts_destroy(itc_drv_thread_opts);
	itc_drv_thread_opts = NULL;
    }
}

/* erlang:open_port/2 */
static ErlDrvData itc_drv_start(ErlDrvPort port, char *c)
{
    ItcDrvState *s;

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    s = ALLOC(sizeof(*s));
    s->port = port;
    s->self = driver_mk_port(port);
    s->name = NULL;
    itc_drv_ref_init(&(s->ref));
    s->next = s;
    s->prev = s;

    *c = '\0'; /* Just to avoid warning about unused parameter */
    return (ErlDrvData) s;
}

/* erlang:port_close */
static void itc_drv_stop(ErlDrvData drv_data)
{
    ItcDrvState *s;

    s = (ItcDrvState *) drv_data;
    if (s->name != NULL) {
	union itc_msg *msg;

	erl_drv_mutex_lock(itc_drv_lock);

	FREE(s->name);
	s->name = NULL;
	
	/* Tell the receiver to stop */
	erl_drv_mutex_lock(s->handler.lock);
	s->handler.drop_dead = TRUE; /* Fallback side channel */
	erl_drv_mutex_unlock(s->handler.lock);
	msg = itc_drv_alloc_msg(sizeof(msg->hdr), MSG_STOP);
	msg->hdr.caller = ERL_DRV_NIL; /* Not used */
	msg->hdr.ref = 0;              /* Not used */
	itc_drv_send(&msg, s->handler.mbox_id);

	/* Wait for the receiver to stop */
	ASSERT(erl_drv_thread_join(s->handler.tid, NULL) == 0);
	erl_drv_mutex_destroy(s->handler.lock);
	erl_drv_cond_destroy(s->handler.cond);

	itc_drv_ring_splice_next(s->prev, s); /* Unlink */
	erl_drv_mutex_unlock(itc_drv_lock);
    }
    FREE(s);
}

/* ********************** */
/* port_control callbacks */
/* ********************** */

/* This is a separate operation that does not affect this particular port.
 * It has to be done before any itc_drv_control_create can be done.
 */
static ErlDrvSSizeT itc_drv_control_init
(ItcDrvState *s, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    int32_t mbox_count;
    itc_alloc_scheme alloc_scheme;
    union itc_scheme scheme;
    char *name_space, c;
    int r;
	
    /* Parse parameters */
    if (ALERT(len < sizeof(uint32_t) + 1)) goto fail;
    mbox_count = (int32_t) get_uint32(buf);
    buf += sizeof(uint32_t);
    len -= sizeof(uint32_t) + 1;
    switch ((c = (buf++)[0])) { /* alloc_scheme */
    case 'm':
	if (ALERT(len)) goto fail;
	alloc_scheme = ITC_MALLOC;
	memset(&scheme, 0, sizeof(scheme));
	break;
    case 'p':
	if (ALERT(len < sizeof(uint32_t))) goto fail;
	alloc_scheme = ITC_POOL;
	scheme.pool_parameters.size =
	    get_uint32(buf);
	buf += sizeof(uint32_t);
	len -= sizeof(uint32_t);
	break;
    case 'f': {
	int i;

	if (ALERT(len < sizeof(uint32_t) * 9)) goto fail;
	alloc_scheme = ITC_POOL_FLEX;
	scheme.pool_flex_parameters.size =
	    get_uint32(buf);
	buf += sizeof(uint32_t);
	len -= sizeof(uint32_t);
	for (i = 0;  i < 8;  i++) {
	    scheme.pool_flex_parameters.msg_sizes[i] =
		get_uint32(buf);
	    buf += sizeof(uint32_t);
	    len -= sizeof(uint32_t);
	}
    } break;
    case 'd':
	if (ALERT(len)) goto fail;
	alloc_scheme = ITC_USER_DEFINED;
	scheme.user_defined_parameters.alloc = &driver_alloc;
	scheme.user_defined_parameters.free = &driver_free;
	scheme.user_defined_parameters.info = &itc_drv_alloc_info;
	break;
    default:
	DPRINTF
	    ("itc_drv_control_init() unknown alloc_scheme: %i\r\n",
	     (int) c);
	goto fail;
    }

    /* Check if itc_init has been called */
    erl_drv_mutex_lock(itc_drv_lock);

    if (itc_drv_state != ITC_DRV_EXITED) {
	erl_drv_mutex_unlock(itc_drv_lock);
	return_ATOM_STR(already_initialized, rbuf, rlen);
    }

    name_space = (len == 0) ? ITC_NO_NAMESPACE : buf;
    r = itc_init(mbox_count, alloc_scheme, &scheme, name_space, 0);
    if (r != 0) {
        DPRINTF("itc_init() failed: %d\r\n", r);
	erl_drv_mutex_unlock(itc_drv_lock);
	goto fail;
    }
    DPRINTF("itc_init()\r\n");
    /* Lock this driver into the VM */
    if (driver_lock_driver(s->port) < 0) {
	itc_exit();
	DPRINTF("itc_exit()\r\n");
	erl_drv_mutex_unlock(itc_drv_lock);
	goto fail;
    }

    itc_drv_state = ITC_DRV_INITIALIZED;
    itc_drv_ring.next = &itc_drv_ring;
    itc_drv_ring.prev = &itc_drv_ring;

    erl_drv_mutex_unlock(itc_drv_lock);
    return 0;

 fail:
    return -1;
}

static ErlDrvSSizeT itc_drv_control_exit
(char **rbuf, ErlDrvSizeT rlen)
{
    erl_drv_mutex_lock(itc_drv_lock);

    switch (itc_drv_state) {
    case ITC_DRV_EXITED:
	erl_drv_mutex_unlock(itc_drv_lock);
	return_ATOM_STR(already_exited, rbuf, rlen);
    case ITC_DRV_INITIALIZED:
	erl_drv_mutex_unlock(itc_drv_lock);
	return_ATOM_STR(already_initialized, rbuf, rlen);
    case ITC_DRV_EXITING: {
        itc_exit();
	DPRINTF("itc_exit()\r\n");

	itc_drv_state = ITC_DRV_EXITED;
	erl_drv_mutex_unlock(itc_drv_lock);
	return 0;
    }
    }
    erl_drv_mutex_unlock(itc_drv_lock);
    return -1;
}

static ErlDrvSSizeT itc_drv_control_exiting
(ItcDrvState *s, char **rbuf, ErlDrvSizeT rlen)
{
    erl_drv_mutex_lock(itc_drv_lock);

    switch (itc_drv_state) {
    case ITC_DRV_EXITING:
	erl_drv_mutex_unlock(itc_drv_lock);
	return_ATOM_STR(already_exiting, rbuf, rlen);
    case ITC_DRV_EXITED:
	erl_drv_mutex_unlock(itc_drv_lock);
	return_ATOM_STR(already_exited, rbuf, rlen);
    case ITC_DRV_INITIALIZED: {
	ItcDrvState *p;
	int n, m;
	ErlDrvTermData *exiting;

	/* Count the number of ports in the ring */
	n = 0;
	for (p = itc_drv_ring.next;
	     p != &itc_drv_ring;
	     p = p->next) n++;

	DPRINTF("itc_drv_control_exiting(): %d ports\r\n", n);

	itc_drv_ref_next(&s->ref);
	/* Build the result term containing a list of open ports */
	exiting = ALLOC(sizeof(*exiting) * (17 + 2*n));
	m = 0;
	/* {exiting, Port, {exiting_ref,Port,Ref}, [Ports...]} */
	/* Start ERL_DRV_TUPLE 4 */
	exiting[m++] = ERL_DRV_ATOM;
	exiting[m++] = ATOM(exiting);
	exiting[m++] = ERL_DRV_PORT;
	exiting[m++] = s->self;
	/* Start ERL_DRV_TUPLE 3 */
	exiting[m++] = ERL_DRV_ATOM;
	exiting[m++] = ATOM(exiting_ref);
	exiting[m++] = ERL_DRV_PORT;
	exiting[m++] = s->self;
	exiting[m++] = ERL_DRV_UINT;
	exiting[m++] = s->ref;
	exiting[m++] = ERL_DRV_TUPLE;
	exiting[m++] = 3;
	/**/
	for (p = itc_drv_ring.next;
	     p != &itc_drv_ring;
	     p = p->next) {
	    exiting[m++] = ERL_DRV_PORT;
	    exiting[m++] = p->self;
	}
	exiting[m++] = ERL_DRV_NIL;
	exiting[m++] = ERL_DRV_LIST;
	exiting[m++] = n + 1;
	exiting[m++] = ERL_DRV_TUPLE;
	exiting[m++] = 4;
	/**/
	ASSERT(m == 17 + 2*n);

	itc_drv_state = ITC_DRV_EXITING;
	erl_drv_mutex_unlock(itc_drv_lock);

	ASSERT
	    (erl_drv_send_term
	     (s->self, driver_caller(s->port), exiting, m));
	FREE(exiting);
	return_ref_ATOM_STR(exiting_ref, s->ref, rbuf, rlen);
    }
    }
    erl_drv_mutex_unlock(itc_drv_lock);
    return -1;
}

static ErlDrvSSizeT itc_drv_control_create
(ItcDrvState *s, char *buf, ErlDrvSizeT len)
{
    if (s->name != NULL) return -1; /* Wrong state */
    if (ALERT(len > ITC_NAME_MAXLEN)) return -1;

    /* Check if itc_init has been called */
    erl_drv_mutex_lock(itc_drv_lock);
    if (itc_drv_state != ITC_DRV_INITIALIZED) goto fail;

    s->name = ALLOC(len + 1);
    memcpy(s->name, buf, len);
    s->name[len] = '\0';
    s->destination = driver_caller(s->port);
    s->flags = 0;
    s->handler.lock = erl_drv_mutex_create("itc_receiver");
    s->handler.mbox_id = ITC_NO_ID;
    s->handler.cond = erl_drv_cond_create("itc_receiver");
    s->handler.drop_dead = FALSE;

    if (0 != erl_drv_thread_create
	("itc_receiver", &(s->handler.tid),
	 &itc_drv_handler_thread, s, itc_drv_thread_opts)) {
	FREE(s->name);
	s->name = NULL;
	erl_drv_mutex_destroy(s->handler.lock);
	erl_drv_cond_destroy(s->handler.cond);
	DPRINTF("itc_drv_control_create() thread create failed\r\n");
	goto fail;
    }

    /* Wait for receiver to start */
    erl_drv_mutex_lock(s->handler.lock);
    while (s->handler.mbox_id == ITC_NO_ID)
	erl_drv_cond_wait(s->handler.cond, s->handler.lock);
    erl_drv_mutex_unlock(s->handler.lock);

    itc_drv_ring_splice_next(&itc_drv_ring, s); /* Link */
    erl_drv_mutex_unlock(itc_drv_lock);
    return 0;

 fail:
    erl_drv_mutex_unlock(itc_drv_lock);
    return -1;
}

static ErlDrvSSizeT itc_drv_control_get_id
(ItcDrvState *s, char **rbuf, ErlDrvSizeT rlen)
{
    char *ptr;

    if (s->name == NULL) return -1; /* Wrong state */

    ptr = itc_drv_control_ensure_rbuf(rbuf, rlen, 1 + sizeof(uint32_t));
    ptr[0] = 'u';
    put_uint32(ptr+1, s->handler.mbox_id);

    return 1 + sizeof(uint32_t);
}

static ErlDrvSSizeT itc_drv_control_get_name
(ItcDrvState *s, char *buf, ErlDrvSizeT len,
 char **rbuf, ErlDrvSizeT rlen)
{
    union itc_msg *msg;

    if (s->name == NULL)  return -1; /* Wrong state */
    if (ALERT(len != sizeof(uint32_t))) return -1;

    itc_drv_ref_next(&s->ref);

    msg = itc_drv_alloc_msg(sizeof(msg->get_name), MSG_GET_NAME);
    msg->get_name.mbox_id = (itc_mbox_id_t) get_uint32(buf);
    msg->hdr.caller = driver_caller(s->port);
    msg->hdr.ref = s->ref;

    itc_drv_send(&msg, s->handler.mbox_id);

    return_ref_ATOM_STR(get_name_ref, s->ref, rbuf, rlen);
}

static ErlDrvSSizeT itc_drv_control_locate
(ItcDrvState *s, char *buf, ErlDrvSizeT len,
 char **rbuf, ErlDrvSizeT rlen)
{
    union itc_msg *msg;

    if (s->name == NULL)  return -1; /* Wrong state */
    if (ALERT(len > ITC_NAME_MAXLEN)) return -1;

    itc_drv_ref_next(&s->ref);

    msg = itc_drv_alloc_msg(sizeof(msg->locate) + len + 1, MSG_LOCATE);
    msg->hdr.caller = driver_caller(s->port);
    msg->hdr.ref = s->ref;
    memcpy(msg->locate.name, buf, len);
    msg->locate.name[len] = '\0';

    itc_drv_send(&msg, s->handler.mbox_id);

    return_ref_ATOM_STR(locate_ref, s->ref, rbuf, rlen);
}

static ErlDrvSSizeT itc_drv_control_unlocate
(ItcDrvState *s, char *buf, ErlDrvSizeT len)
{
    union itc_msg *msg;

    if (s->name == NULL)  return -1; /* Wrong state */
    if (ALERT(len != sizeof(uint32_t))) return -1;

    msg = itc_drv_alloc_msg(sizeof(msg->hdr), MSG_UNLOCATE);
    msg->hdr.ref = get_uint32(buf);
    msg->hdr.caller = driver_caller(s->port);

    itc_drv_send(&msg, s->handler.mbox_id);

    return 0;
}

static ErlDrvSSizeT itc_drv_control_monitor
(ItcDrvState *s, char *buf, ErlDrvSizeT len,
 char **rbuf, ErlDrvSizeT rlen)
{
    union itc_msg *msg;

    if (s->name == NULL)  return -1; /* Wrong state */
    if (ALERT(len != sizeof(uint32_t))) return -1;

    itc_drv_ref_next(&s->ref);
    msg = itc_drv_alloc_msg(sizeof(msg->monitor), MSG_MONITOR);
    msg->hdr.ref = s->ref;
    msg->hdr.caller = driver_caller(s->port);
    msg->monitor.mbox_id = (itc_mbox_id_t) get_uint32(buf);

    itc_drv_send(&msg, s->handler.mbox_id);

    return_ref_ATOM_STR(monitor_ref, s->ref, rbuf, rlen);
}

static ErlDrvSSizeT itc_drv_control_unmonitor
(ItcDrvState *s, char *buf, ErlDrvSizeT len)
{
    union itc_msg *msg;

    if (s->name == NULL)  return -1; /* Wrong state */
    if (ALERT(len != sizeof(uint32_t))) return -1;

    msg = itc_drv_alloc_msg(sizeof(msg->hdr), MSG_UNMONITOR);
    msg->hdr.ref = get_uint32(buf);
    msg->hdr.caller = driver_caller(s->port);

    itc_drv_send(&msg, s->handler.mbox_id);

    return 0;
}

static ErlDrvSSizeT itc_drv_control_send
(ItcDrvState *s, unsigned int command, char *buf, ErlDrvSizeT len,
 char **rbuf, ErlDrvSizeT rlen)
{
    itc_mbox_id_t to, from;
    union itc_msg *msg;

    if (s->name == NULL) return -1; /* Wrong state */

    switch (command) {
    case 's':
	if (ALERT(len < 2 * sizeof(uint32_t))) return -1;
	to = (itc_mbox_id_t) get_uint32(buf);
	from = s->handler.mbox_id;
	buf += sizeof(uint32_t);
	len -= sizeof(uint32_t);
	break;
    case 'S':
	if (ALERT(len < 2 * sizeof(uint32_t))) return -1;
	to = (itc_mbox_id_t) get_uint32(buf);
	from = (itc_mbox_id_t) get_uint32(buf);
	buf += 2 * sizeof(uint32_t);
	len -= 2 * sizeof(uint32_t);
	break;
    default:
	ASSERT(FALSE);
	return -1;
    }

    itc_drv_ref_next(&s->ref);

    msg = itc_drv_alloc_msg(sizeof(msg->send), MSG_SEND); /* Envelope */
    msg->hdr.caller = driver_caller(s->port);
    msg->hdr.ref = s->ref;
    msg->send.to = to;
    msg->send.from = from;
    msg->send.msg = itc_alloc(len, /*msgno*/ get_uint32(buf)); /* Message */
    buf += sizeof(uint32_t);
    len -= sizeof(uint32_t);
    memcpy(msg->send.msg->generic.data, buf, len);

    itc_drv_send(&msg, s->handler.mbox_id);

    return_ref_ATOM_STR(send_ref, s->ref, rbuf, rlen);
}

static ErlDrvSSizeT itc_drv_control_listen
(ItcDrvState *s, unsigned int command, char *buf, ErlDrvSizeT len,
 char **rbuf, ErlDrvSizeT rlen)
{
    union itc_msg *msg;
    uint32_t msgno;
    ErlDrvSizeT i, n;

    if (s->name == NULL) return -1; /* Wrong state */
    if (ALERT(len % sizeof(uint32_t))) return -1;
    n = len / sizeof(uint32_t);
    /* n + num possible msgnos shall not overflow int32_t; is this code fine? */
    if (ALERT(n >= 0x7fffffff)) {
	return -1;
    }
    switch (command) {
    case 't': msgno = MSG_LISTEN; break;
    case 'T': msgno = MSG_UNLISTEN; break;
    default: ASSERT(FALSE); return -1;
    }

    itc_drv_ref_next(&s->ref);

    msg =
	itc_drv_alloc_msg
	(sizeof(struct msg_listen) + (1+n)*sizeof(msg->listen.msgnos[0]),
	 msgno);
    msg->hdr.caller = driver_caller(s->port);
    msg->hdr.ref = s->ref;
    msg->listen.num_msgnos = n;
    for (i = 0;  i < n;  i++ ) {
	msg->listen.msgnos[i] = get_uint32(buf);
	buf += sizeof(uint32_t);
	len -= sizeof(uint32_t);
    }

    itc_drv_send(&msg, s->handler.mbox_id);

    switch (command) {
    case 't': return_ref_ATOM_STR(listen_ref, s->ref, rbuf, rlen); break;
    case 'T': return_ref_ATOM_STR(unlisten_ref, s->ref, rbuf, rlen); break;
    default: ASSERT(FALSE); return -1;
    }
}

/* erlang:port_control */
static ErlDrvSSizeT itc_drv_control
(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len,
 char **rbuf, ErlDrvSizeT rlen)
{
    ItcDrvState *state;

    state = (ItcDrvState *) drv_data;
    switch (command) {
    case 'i':
	return itc_drv_control_init(state, buf, len, rbuf, rlen);
    case 'x':
	return itc_drv_control_exit(rbuf, rlen);
    case 'X':
	return itc_drv_control_exiting(state, rbuf, rlen);
    case 'c':
	return itc_drv_control_create(state, buf, len);
    case 'g':
	return itc_drv_control_get_id(state, rbuf, rlen);
    case 'n':
	return itc_drv_control_get_name(state, buf, len, rbuf, rlen);
    case 'l':
	return itc_drv_control_locate(state, buf, len, rbuf, rlen);
    case 'L':
	return itc_drv_control_unlocate(state, buf, len);
    case 'm':
	return itc_drv_control_monitor(state, buf, len, rbuf, rlen);
    case 'M':
	return itc_drv_control_unmonitor(state, buf, len);
    case 's':
    case 'S':
	return itc_drv_control_send(state, command, buf, len, rbuf, rlen);
    case 't':
    case 'T':
	return itc_drv_control_listen(state, command, buf, len, rbuf, rlen);
    default:
	DPRINTF("itc_drv_control() unknown command: %u\r\n", command);
	return -1;
    }
}

/* ********************************************************************** */

static ErlDrvEntry itc_drv_entry = {
    itc_drv_init, /* erl_ddll:load_driver */
    itc_drv_start, /* erlang:open_port */
    itc_drv_stop, /* erlang:close_port */
    NULL, /* output: Port ! {self(),{command,Data}}, port_command/2 */
    NULL, /* ready_input */
    NULL, /* ready_output */
    DRIVER_NAME_STRING,
    itc_drv_finish, /* erl_ddll:unload_driver */
    NULL, /* handle */
    itc_drv_control, /* erlang:port_control */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING, /* driver_flags */
    NULL, /* handle2 */
    NULL, /* process_exit */
    NULL, /* stop_select */
    NULL /* emergency_close */
};

DRIVER_INIT(DRIVER_NAME)
{
    DPRINTF("DRIVER_INIT(DRIVER_NAME)\r\n");
    return &itc_drv_entry;
}

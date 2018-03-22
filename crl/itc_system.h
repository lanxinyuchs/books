/**
 *   itc_system.h specifies the system setup of ITC API
 *
 *   @file itc_system.h
 *
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 *
 *
 *   1. ITC Overview
 *   ---------------
 *
 *   Inter Thread Communication (ITC) is an implementation that allows
 *   passing messages between threads, processes and boards.
 *
 *   The main interface for regular ITC users is specified in itc.h.
 *   itc_system.h contains functionality for more system type functions:
 *   * Cloning mailboxes.
 *   * Removing a message from a receive queue.
 *   * Support functions for linkhandlers.
 *   * The possibility to install send and receive hooks.
 *
 *
 *   2. Cloning mailboxes
 *   --------------------
 *
 *   To clone a mailbox is to assign a new name and id to an existing
 *   mailbox. The cloned mailbox will have the same receive queue as the
 *   original mailbox. You can locate and monitor the cloned mailbox in the
 *   same way as you do a regular mailbox.
 *
 *   When a message is sent to a cloned mailbox the receiver of the message
 *   will be set to the id of the mailbox clone. To determine which mailbox
 *   a message was sent to you need to call itc_receiver after you have
 *   received the message.
 *
 *   To delete a cloned mailbox you use itc_delete_mailbox the same way you
 *   do for a regular mailbox. If you delete a mailbox that have open clones
 *   the errorhandler will be called. If the errorhandler returns 0 to allow
 *   this the original mailbox and all of the clones will be deleted.
 *
 *   You can also add only another name to an existing mailbox. The new name
 *   only adds the possibility to locate the mailbox using this new name. You
 *   can not remove an added name from a mailbox.
 *
 *
 *   2. Remove a message from mailbox
 *   --------------------------------
 *
 *   It is possible to remove a message identified by a message pointer from
 *   a receive queue using itc_remove. itc_remove takes a mailbox id and the
 *   message pointer to be removed as parameters. If the message has been
 *   successfully removed the message pointer is returned by itc_remove and
 *   if the message was not found in the receive queue then NULL is returned.
 *
 *
 *   3 Assigning linkhandlers
 *   ------------------------
 *
 *   Assign linkhandlers are used if you want to add a linkhandler connection
 *   to the running ITC. The linkhandler is assigned with a namespace (lnhpath)
 *   and the mailbox id of the linkhandler thread.
 *
 *   After the linkhandler is assigned ITC will forward all unresolved locate
 *   requests to the linkhandler namespace to the linkhandler mailbox id using
 *   message ITC_LOCATE_LNH. The linkhandler then needs to try to resolve the
 *   locate. If it is successfull it shall clone its mailbox to a new one with
 *   the name locate request. Then it replies to ITC with message
 *   ITC_LOCATE_LNH_REPLY.
 *
 *   Please note that only asynchronous locate requests will be forwarded to
 *   linkhandlers.
 *
 *   One linkhandler mailbox can service many ITC namespaces.
 *
 *   An assigned linkahnder connection can be removed from ITC using
 *   itc_deassign with the appropriate mailbox id and linkhandler path.
 *
 *
 *   4 ITC send and receive hook
 *   ---------------------------
 *
 *   It is possible to install hook functions in ITC for itc_send and
 *   itc_receive. This can be used to implement for instance message traces,
 *   endian converters or flow control mechanisms.
 *
 *   The send hook is called in the itc_send context before the message is
 *   added to the receivers mailbox queue. If the hook returns ITC_DROP
 *   the message will not be added to the receivers mailbox and ownership
 *   of the message goes to the hook function.
 *
 *   The receive hook is called in the itc_receive context after the message
 *   has been removed from the mailbox queue and before it is delivered
 *   by itc_receive. If the hook returns ITC_DROP the message will not be
 *   delivered and ownership of the message goes to the hook function.
 *
 *   Only one receive and one send hook can be active at any given time.
 *   If a new hook function is installed it will overwrite the previous hook
 *   function. If the hook function is NULL at installation that hook is
 *   ignored and a previous hook function is retained. If both the RX hook
 *   and TX hook function pointers are set to NULL then both the hook
 *   functions will be removed.
 *
 *
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2013-01-10 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : First version.
 *
 *   Revised : 2014-01-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for multiple itcworld instances.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Updated to support file and line correctly.
 *
 *   Revised : 2014-10-28 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added itc_events implementation and corrected
 *             itc_deassign_linkhandler
 * ========================================================================
 */

#ifndef __ITCSYSTEM_H
#define __ITCSYSTEM_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>
#include <stddef.h>

#include "itc.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/**
 * ITC event definitions, the events are defined as single bits making it
 * possible to or bits together to subscribe to several events at the
 * same time.
 */
#define ITC_EVENT_MBOXES_ADDED       0x00000001
#define ITC_EVENT_MBOXES_REMOVED     0x00000002
#define ITC_EVENT_LNH_ADDED          0x00000004
#define ITC_EVENT_LNH_REMOVED        0x00000008
#define ITC_EVENT_LOCATE_UNRESOLVED  0x00000010
#define ITC_EVENT_ALL                0xFFFFFFFF

/**
 * ITC_DROP definition used in as return from itc_hook function to signify
 * that a message shall be dropped in receive.
 */
#define ITC_DROP -2

/**
 * Definition of environment variables used by ITC for various
 * configurations.
 */
#define ITC_ENV_RUNDIR_PATH   "ITC_RUNDIR_PATH"
#define ITC_ENV_INSTANCE_NAME "ITC_INSTANCE_NAME"
#define ITC_ENV_NAMESPACE     "ITC_NAMESPACE"
#define ITC_ENV_MAXLEN        96

/* ===================================================================== */
/**
 *   Message ITC_LOCATE_LNH
 *
 *   @param from       Mailbox that is responsible for the ITC locate
 *                     request.
 *
 *   @param name       Name of the mailbox that is beeing hunted.
 *
 *   The ITC_LOCATE_LNH message is used between linkhandlers and ITC to
 *   forward locate requests.
 *
 *   If the origination of the request is ITC the lnhpath is a part of
 *   the name.
 *
 *   If the origination of the request is a linkhandle the lnhpath from
 *   the peer is stripped from the name.
 *
 */
/* ===================================================================== */
#define ITC_LOCATE_LNH (ITC_MSG_BASE + 0x10)
struct itc_locate_lnh
{
        uint32_t      msgno;
        itc_mbox_id_t from;
        char          name[1];
};

/* ===================================================================== */
/**
 *   Message ITC_LOCATE_LNH_REPLY
 *
 *   @param mbox_id    Mailbox id of the located mailbox.
 *
 *   @param found      True if the mailbox has been found.
 *
 *   @param name       Name of the located mailbox.
 *
 *   The ITC_LOCATE_LNH_REPLY is used when location request has been
 *   completed. If the mailbosx has been found, found is set to true and
 *   the mailbox id is set in mbox_id.
 *
 *   To be able to support multiple outstanding locate requests name is
 *   returned in the reply sinal so it can be matched with the locate
 *   request.
 *
 */
/* ===================================================================== */
#define ITC_LOCATE_LNH_REPLY (ITC_MSG_BASE + 0x11)
struct itc_locate_lnh_reply
{
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        int32_t       found;
        char          name[1];
};

/* ===================================================================== */
/**
 *   Message ITC_MBOX_ADDED
 *
 *   @param mbox_id    Mailbox id of the added mailbox.
 *
 *   @param last       Last of the mailboxes created prior to the event
 *                     subscribe call.
 *
 *   @param mbox_name  Name of the added mailbox.
 *
 *   The ITC_MBOX_ADDED message is used to indicate that a mailbox has been
 *   added by ITC. You can get a corresponding remove message to be informed
 *   of the removal of mailboxes but in most cases it is better to use
 *   itc_monitor to detect the removal of mailboxes.
 *
 */
/* ===================================================================== */
#define ITC_MBOX_ADDED (ITC_MSG_BASE + 0x12)
struct itc_mbox_added {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        int32_t       last;
        char          mbox_name[1];
};

/* ===================================================================== */
/**
 *   Message ITC_MBOX_REMOVED
 *
 *   @param mbox_id    Mailbox id of the removed mailbox.
 *
 *   @param mbox_name  Name of the removed mailbox.
 *
 *   The ITC_MBOX_REMOVED message is used to indicate that a mailbox has been
 *   removed by ITC.
 *
 */
/* ===================================================================== */
#define ITC_MBOX_REMOVED (ITC_MSG_BASE + 0x13)
struct itc_mbox_removed {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        char          mbox_name[1];
};

/* ===================================================================== */
/**
 *   Message ITC_LNH_ADDED
 *
 *   @param mbox_id    Mailbox id of the mailbox that handles this
 *                     linkhandler.
 *
 *   @param last       Last of the linkhandlers created prior to the event
 *                     subscribe call.
 *
 *   @param lnhpath    Linkhandler path of the added linkhandler.
 *
 *   The ITC_LNH_ADDED message is used to indicate that a linkhandler
 *   has been added by ITC.
 *
 */
/* ===================================================================== */
#define ITC_LNH_ADDED (ITC_MSG_BASE + 0x14)
struct itc_lnh_added {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        int32_t       last;
        char          lnhpath[1];
};

/* ===================================================================== */
/**
 *   Message ITC_LNH_REMOVED
 *
 *   @param mbox_id    Mailbox id of the mailbox that handled this
 *                     linkhandler.
 *
 *   @param lnhpath    Linkhandler path of the removed linkhandler.
 *
 *   The ITC_LNH_REMOVED message is used to indicate that a linkhandler
 *   has been removed by ITC.
 *
 */
/* ===================================================================== */
#define ITC_LNH_REMOVED (ITC_MSG_BASE + 0x15)
struct itc_lnh_removed {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        char          lnhpath[1];
};


/* ===================================================================== */
/**
 *   Message ITC_LOCATE_UNRESOLVED
 *
 *   @param mbox_id    Mailbox id of the requesting mailbox.
 *
 *   @param mbox_name  Name of the mailbox to be located.
 *
 *   The ITC_LOCATE_UNRESOLVED message is used to indicate that a locate
 *   request has been done for a mailbox name that does not exist at
 *   this time.
 *
 */
/* ===================================================================== */
#define ITC_LOCATE_UNRESOLVED (ITC_MSG_BASE + 0x16)
struct itc_locate_unresolved {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        char          mbox_name[1];
};

/* ===================================================================== */
/**
 *   Get path to itc run directory
 *
 *   @return           Pointer to path to find itc run directory.
 *
 *   @par Globals:     --
 *
 *   Gets the path to the runtime directory that ITC uses to for SYSV
 *   message queues and af_local sockets.
 *
 *   NOTE: this call should only be used by itcworld.
 *
 */
/* ===================================================================== */
extern char *itc_get_rundir(void);

/* ===================================================================== */
/**
 *   Typedefinition of ITC hook function.
 *
 *   @param user       User supllied data that comes from the
 *                     itc_install_hooks call.
 *
 *   @param from       Mailbox ID of sending mailbox.
 *
 *   @param to         Mailbox ID of receiving mailbox.
 *
 *   @param size       Message data length.
 *
 *   @param msg        Pointer to message.
 *
 *   @return           0 at success and ITC_DROP if the massage shall be
 *                     dropped atfer the hook has returned.
 *
 *   @par Globals:     --
 *
 *   Typedefinition of the hook function that can be installed as send
 *   or receive hook.
 *
 */
/* ===================================================================== */
typedef int (itc_hook)(void          *user,
                       itc_mbox_id_t  from,
                       itc_mbox_id_t  to,
                       size_t         size,
                       union itc_msg *msg);

/* ===================================================================== */
/**
 *   Install send and receive hooks to ITC.
 *
 *   @param send_hook  Send hook function that is called at ITC send.
 *
 *   @param send_user  User supplied field that will be forwarded in the
 *                     send hook function call.
 *
 *   @param recv_hook  Receive hook function that is called at ITC receive.
 *
 *   @param recv_user  User supplied field that will be forwarded in the
 *                     receive hook function call.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Installs send and receive hook functions that will be called by ITC at
 *   itc_send and itc_receive.
 *
 *   The send hook will be called in the context of the ITC send call
 *   after sender and receiver has been set but before the message is placed
 *   into the receipients mailbox.
 *
 *   The receive hook will be called in the context of the itc_receive call
 *   after the message has been retrived from the mailbox.
 *
 *   Please not that only one send and one receive hook can be installed.
 *   If you add a second hook it will overwrite the previous hook silently.
 *   This will not happen if the hook function pointer is NULL.
 */
/* ===================================================================== */
extern void itc_install_hooks(itc_hook  send_hook,
                              void     *send_user,
                              itc_hook  recv_hook,
                              void     *recv_user);

/* ===================================================================== */
/**
 *   Register/Assign a linkhandler to ITC
 *
 *   @param lnhpath    Linkhandler path.
 *
 *   @param mbox_id    Linkhandler mailbox ID.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Assigns a linkhandler with the path given by lnhpath. All hunts
 *   performed henceforth which starts with "<lnhpath>/" will be forwarded
 *   with itc message ITC_LNH_HUNT to the mbox id given in the
 *   itc_assign_linkhandler call.
 *
 */
/* ===================================================================== */
extern void itc_assign_linkhandler(char *lnhpath, itc_mbox_id_t mbox_id);

/* ===================================================================== */
/**
 *   Unregister/Deassign a linkhandler to ITC
 *
 *   @param lnhpath    Linkhandler path.
 *
 *   @param mbox_id    Linkhandler mailbox ID.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Deassigns a linkhandler with the path given by lnhpath.
 *
 */
/* ===================================================================== */
extern void itc_deassign_linkhandler(char *lnhpath, itc_mbox_id_t mbox_id);

/* ===================================================================== */
/**
 *   Clone ITC mailbox.
 *
 *   @param mbos_id    Mailbox ID that shall be cloned.
 *
 *   @param name       Name of the cloned mailbox.
 *
 *   @return           Mailbox id of the new mailbox clone.
 *
 *   @par Globals:     --
 *
 *   This clones an existing mailbox. That it is cloned is really that
 *   you give an existing mailbox queue a new name and it is allocated
 *   an additional mailbox id. So when you do an itc_receive you will
 *   receive packets sent to both mailboxes. To know which mailbox a
 *   message was sent to you do itc_receiver on the message after it
 *   has been received.
 *
 *   to send from a cloned mailbox you have to set the cloned mailbox id
 *   as sender in the itc_send call.
 *
 *   A cloned mailbox counts as a mailbox and you will have to take
 *   cloned mailboxes into account when you set max mailboxes in itc_init.
 *
 */
/* ===================================================================== */
extern itc_mbox_id_t itc_clone_mailbox(itc_mbox_id_t mbox_id, const char *name);

/* ===================================================================== */
/**
 *   Add name to mailbox
 *
 *   @param mbox_id    Mailbox ID that shall have name added to it.
 *
 *   @param name       Name to be added to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Adds a name to an existing mailbox. You can add as many names as you
 *   like to the mailbox. Please note that if you do itc_get_name on a
 *   mailbox you will get the name used in itc_create_mailbox and no other.
 *
 */
/* ===================================================================== */
extern int itc_add_name(itc_mbox_id_t mbox_id, const char *name);

/* ===================================================================== */
/**
 *   Receive messages from specified ITC mailbox.
 *
 *   @param mbox_id    Mailbox ID of the shared receive queue.
 *
 *   @param filter     Which message numbers you wish to receive.
 *                     Set to ITC_NOFILTER to get all messages.
 *
 *   @param tmo        How many milliseconds the itc_receive shall block
 *                     and wait for messages. Use 0 to get a non blocking
 *                     call and ITC_NO_TMO to block forever.
 *
 *   @return           Pointer to received message from ITC mailbox or
 *                     NULL if no message was received.
 *
 *   Receives messages from a shared mailbox. Which shared mailbox is
 *   given in the mbox_id.
 */
/* ===================================================================== */
union itc_msg *itc_receive_mbox(itc_mbox_id_t mbox_id,
                                const uint32_t *filter,
                                int32_t tmo);

/* ===================================================================== */
/**
 *   Remove ITC message
 *
 *   @param mbox_id    Mailbox that message shall be removed from.
 *
 *   @param msg        Pointer to ITC message pointer that shall
 *                     be removed.
 *
 *   @return           Message pointer that has been removed or NULL if
 *                     message not found in RX queue.
 *
 *   @par Globals:     --
 *
 *   Removes a message from the receive queue of a mailbox. The message
 *   to be removed is specified with its union itc_msg message pointer.
 *   If the message is found and removed from the queue the message
 *   pointer is returned, if the message is not found NULL is returned.
 */
/* ===================================================================== */
union itc_msg *itc_remove(itc_mbox_id_t mbox_id, union itc_msg *msg);

/* ===================================================================== */
/**
 *   Subscribe to ITC events
 *
 *   @param events     Flag field of which events to subscribe to.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Subscribe to ITC events. The events field is a flag field with
 *   one event per bit. The calls are cummulative so that you can
 *   subscribe to one event in the first call and then another in a
 *   new call you will subscribe to both events. To stop a subscription
 *   use itc_unsubscribe_events.
 *
 *   Supported events are:
 *   ITC_EVENT_MBOXES_ADDED: You get notification of all mboxes that
 *                           are create with message ITC_MBOX_ADDED.
 *                           At the time of subscribe all existing
 *                           mailboxes will be notified in the same way.
 *
 *   ITC_EVENT_MBOXES_REMOVED: You get notification of all mboxes that
 *                             are deleted with message ITC_MBOX_REMOVED.
 *
 *   ITC_EVENT_LNH_ADDED: You get notification of all linkhandkers that
 *                        are create with message ITC_LNH_ADDED.
 *                        At the time of subscribe all existing
 *                        linkhandlers will be notified in the same way.
 *
 *   ITC_EVENT_LNH_REMOVED: You get notification of all linkhandkers that
 *                          are removed with message ITC_LNH_REMOVED.
 *
 *   ITC_EVENT_UNRESOLVED_LOCATES: You get notification of all locates
 *                                 that fail to be resolved autommatically
 *                                 with message ITC_LOCATE_UNRESOLVED.
 *
 */
/* ===================================================================== */
void itc_subscribe_events(uint32_t events);

/* ===================================================================== */
/**
 *   Unsubscribe to ITC events
 *
 *   @param events     Flag field of which events to subscribe to.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Unsubscribe to ITC events. The events field is a flag field with
 *   one event per bit. The calls are cumulative so that you can
 *   unsubscribe to one event in the first call and then another in a
 *   new call you will subscribe to both events.
 *
 *   The possible events are described above at itc_subscribe_events.
 */
/* ===================================================================== */
void itc_unsubscribe_events(uint32_t events);

/* ===================================================================== */
/**
 *   Get all ITC mailboxes
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   To be used to be informed of all created ITC mailboxes. You will
 *   get all currently existing mailboxes as well as all mailboxes that
 *   are created sent to you through ITC_MBOX_ADDED message. To be
 *   informed when a mailbox has been removed you should monitor the
 *   added mailboxes.
 */
/* ===================================================================== */
void itc_get_mboxes(void);

/* ===================================================================== */
/**
 *   Get information about maibox
 *
 *   @param mbox_id    Mailbox id.
 *
 *   @return           Pointer to structure with mailbox information or
 *                     NULL if mailbox not found.
 *
 *   @par Globals:     --
 *
 *   Get information about a mailbox, it will return a pointer to
 *   struct itc_mbox_info if the mailbox is found or NULL if it is not
 *   found. The pointer returned is a buffer allocated with itc_alloc
 *   that needs to be freed by the caller.
 */
/* ===================================================================== */
extern struct itc_mbox_info *itc_get_mailbox_info(itc_mbox_id_t mbox_id);

/* ===================================================================== */
/**
 *   Get information about ITC allocations.
 *
 *   @param mbox_id    Mailbox id of any mailbox in the process tou wish
 *                     to get alloc information about.
 *
 *   @return           Pointer to structure with allocation information.
 *
 *   @par Globals:     --
 *
 *   Get information about the ITC allocations, it will return a pointer
 *   to struct itc_alloc_info or NULL if no information is available.
 *   The pointer returned is a buffer allocated  with itc_alloc that
 *   needs to be freed by the caller.
 */
/* ===================================================================== */
extern struct itc_alloc_info *itc_get_alloc_info(itc_mbox_id_t mbox_id);

/* ===================================================================== */
/**
 *   Call ITC error handler
 *
 *   @param errtext    Error message character string.
 *
 *   @param flags       Error flags.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Make ITC call the ITC error handler.
 *
 */
/* ===================================================================== */
extern void itc_call_errh(char *errtext, uint32_t flags);



/* ===================================================================== */
/**
 *   Function macros and prototypes for file and line information
 *
 *   The rest of itc.h contains the macros and function prototypes used
 *   when building ITC with ITC_FILEANDLINE defined. When you build with
 *   ITC_FILEANDLINE file and line information is passed into each ITC
 *   interface function so that it can be passed into the registered
 *   error handlers to make trouble shooting easier.
 *
 */
/* ===================================================================== */
extern void __itc_install_hooks(itc_hook    send_hook,
                                void       *send_user,
                                itc_hook    recv_hook,
                                void       *recv_user,
                                const char *file,
                                int         line);
#ifdef ITC_FILEANDLINE
#define itc_install_hooks(send_hook, send_user, recv_hook, recv_user) \
        __itc_install_hooks((send_hook), (send_user), (recv_hook), (recv_user), __FILE__, __LINE__)
#else
#define itc_install_hooks(send_hook, send_user, recv_hook, recv_user) \
        __itc_install_hooks((send_hook), (send_user), (recv_hook), (recv_user), "", 0)
#endif


extern void __itc_assign_linkhandler(char *lnhpath,
                                     itc_mbox_id_t mbox_id,
                                     const char *file,
                                     int line);
#ifdef ITC_FILEANDLINE
#define itc_assign_linkhandler(lnhpath, mbox_id) __itc_assign_linkhandler((lnhpath), (mbox_id), __FILE__, __LINE__)
#else
#define itc_assign_linkhandler(lnhpath, mbox_id) __itc_assign_linkhandler((lnhpath), (mbox_id), "", 0)
#endif


extern void __itc_deassign_linkhandler(char *lnhpath,
                                       itc_mbox_id_t mbox_id,
                                       const char *file,
                                       int line);
#ifdef ITC_FILEANDLINE
#define itc_deassign_linkhandler(lnhpath, mbox_id) __itc_deassign_linkhandler((lnhpath), (mbox_id), __FILE__, __LINE__)
#else
#define itc_deassign_linkhandler(lnhpath, mbox_id) __itc_deassign_linkhandler((lnhpath), (mbox_id), "", 0)
#endif


extern itc_mbox_id_t __itc_clone_mailbox(itc_mbox_id_t mbox_id,
                                         const char *name,
                                         const char *file,
                                         int line);
#ifdef ITC_FILEANDLINE
#define itc_clone_mailbox(mbox_id, name) __itc_clone_mailbox((mbox_id), (name), __FILE__, __LINE__)
#else
#define itc_clone_mailbox(mbox_id, name) __itc_clone_mailbox((mbox_id), (name), "", 0)
#endif



extern int __itc_add_name(itc_mbox_id_t mbox_id,
                          const char *name,
                          const char *file,
                          int line);
#ifdef ITC_FILEANDLINE
#define itc_add_name(mbox_id, name) __itc_add_name((mbox_id), (name), __FILE__, __LINE__)
#else
#define itc_add_name(mbox_id, name) __itc_add_name((mbox_id), (name), "", 0)
#endif


union itc_msg *__itc_receive_mbox(itc_mbox_id_t mbox_id,
                                  const uint32_t *filter,
                                  int32_t tmo,
                                  const char *file,
                                  int line);
#ifdef ITC_FILEANDLINE
#define itc_receive_mbox(mbox_id, flt, tmo) __itc_receive_mbox((mbox_id), (flt), (tmo), __FILE__, __LINE__)
#else
#define itc_receive_mbox(mbox_id, flt, tmo) __itc_receive_mbox((mbox_id), (flt), (tmo), "", 0)
#endif


union itc_msg *__itc_remove(itc_mbox_id_t mbox_id,
                            union itc_msg *msg,
                            const char *file,
                            int line);
#ifdef ITC_FILEANDLINE
#define itc_remove(mbox_id, msg) __itc_remove((mbox_id), (msg), __FILE__, __LINE__)
#else
#define itc_remove(mbox_id, msg) __itc_remove((mbox_id), (msg), "", 0)
#endif


void __itc_get_mboxes(const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_get_mboxes() __itc_get_mboxes(__FILE__, __LINE__)
#else
#define itc_get_mboxes() __itc_get_mboxes("", 0)
#endif


extern struct itc_mbox_info *__itc_get_mailbox_info(itc_mbox_id_t mbox_id,
                                                    const char   *file,
                                                    int           line);
#ifdef ITC_FILEANDLINE
#define itc_get_mailbox_info(mbox_id) __itc_get_mailbox_info((mbox_id), __FILE__, __LINE__)
#else
#define itc_get_mailbox_info(mbox_id) __itc_get_mailbox_info((mbox_id), "", 0)
#endif


extern struct itc_alloc_info *__itc_get_alloc_info(itc_mbox_id_t mbox_id,
                                                   const char   *file,
                                                   int           line);
#ifdef ITC_FILEANDLINE
#define itc_get_alloc_info(mbox_id) __itc_get_alloc_info((mbox_id), __FILE__, __LINE__)
#else
#define itc_get_alloc_info(mbox_id) __itc_get_alloc_info((mbox_id), "", 0)
#endif


extern void __itc_call_errh(char *errtext,
                            uint32_t flags,
                            const char *file,
                            int line);
#ifdef ITC_FILEANDLINE
#define itc_call_errh(errtest, flags) __itc_call_errh((errtest), (flags), __FILE__, __LINE__)
#else
#define itc_call_errh(errtest, flags) __itc_call_errh((errtest), (flags), "", 0)
#endif


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ITC_H */

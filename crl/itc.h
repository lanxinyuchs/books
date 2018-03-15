/**
 *   itc.h specifies the ITC API
 *
 *   @file itc.h
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
 *   1. ITC Overview
 *   ---------------
 *
 *   Inter Thread Communication (ITC) is an implementation that allows
 *   passing messages between threads, processes and boards. The messages
 *   are passed into the receivers mailbox that consists of a receive queue
 *   for incoming messages.
 *
 *   The terminology board, process and threads will be used in the
 *   remainder of the file with the below meaning.
 *
 *   A processor is an execution unit that is handled by one instance of an
 *   operating system. It can have multiple cores and several simultaneously
 *   executing contexts.
 *
 *   A process has its own memory map and can have several running threads.
 *   If you compare this to OSE this is a loadmodule.
 *
 *   A thread is a single execution context that can coexist with other
 *   threads within a process. The threads in a process shares memory map.
 *   For OSE a thread is an OSE process.
 *
 *   ITC needs to be initialised with itc_init before used.
 *
 *
 *   2. ITC initialisation
 *   ---------------------
 *
 *   The init sets up the maximum number of mailboxes used in the process.
 *   If you exceed this number the errorhandler will be called. Each mailbox
 *   uses approximately 200 bytes which will be statically allocated in
 *   itc_init for all mailboxes.
 *
 *   Initialisation also setup how ITC will allocate message buffers.
 *   ITC provides 3 allocation schemes defined in the itc_alloc_scheme
 *   enumeration:
 *   * ITC_MALLOC uses malloc() as allocation mechanism in each itc_alloc.
 *     This can lead to that itc_alloc and free can be put to sleep.
 *     ITC_MALLOC has no more configuration parameters in the itc_scheme
 *     union.
 *
 *   * ITC_POOL pre allocates an area used by ITC for the ITC
 *     messages. The total size of this area is configured in the
 *     pool_parameters struct in the itc_scheme union. The ITC_POOL
 *     allocation works with the following fixed sizes 32, 224, 992,
 *     4064, 16352 and 65504 bytes. The sizes are selected to get an
 *     efficient allocation mechanism. When a message has been given
 *     a specific size it will retain that size until itc_exit is
 *     called or the process terminates.
 *
 *   * ITC_POOL_FLEX pre allocates an area used by ITC for the ITC
 *     messages. The total size of this area is configured in the
 *     pool_flex_parameters struct in the itc_scheme union. For this
 *     allocation mechanism you can also configure 8 different messages
 *     sizes that will be used when allocating messages. This is also
 *     configured in the pool_flex_parameters struct. When a message
 *     has been given a specific size it will retain that size until
 *     itc_exit is called or the process terminates.
 *
 *     To terminate the use of ITC in a process you call itc_exit.
 *     For this to complete without errors  all mailboxes has to be
 *     deleted before you call itc_exit. If all mailboxes are not deleted
 *     the error handler will be called.
 *
 *   The initalisation can also enter an optional namespace for this
 *   process. For more information about how the namespace is used see
 *   below in namespaces and linkhandlers.
 *
 *
 *   3. Mailbox creation, name and ID
 *   --------------------------------
 *
 *   For a thread to use ITC it needs to create a mailbox with a name.
 *   The name of the mailbox will be used as identifier when locating
 *   mailboxes. It is allowed to create several mailboxes with the same
 *   name but the behaviour is undefined when you try to locate them, you
 *   will get one of them but you do not know which. The recommendation is
 *   to use unique names for all mailboxes in the namespace.
 *
 *   When the mailbox is created it returns the mailbox id. This ID
 *   identifies sender and receiver of messages. The ID will be returned
 *   by locate when a mailbox is found.
 *
 *   The definition ITC_MY_MBOX can be used to identify the current threads
 *   mailbox.
 *
 *
 *   3.1 Namespaces and linkhandlers
 *
 *   To be able to support communication between boards and also to support
 *   name separation of processes the concept of namespace is introduced.
 *   Together with the namespace a separator is used, the separator in ITC
 *   is '/'. To locate a mailbox in another namespace you look for the name:
 *   <name space>/<mailbox name>
 *   For instance if you want to locate mailbox server_1 in namespace board_3
 *   you need to locate name "board_3/server_1".
 *
 *   Name spaces are most commonly used for mailboxes residing on a remote
 *   processor. Then you say that you have a linkhandler handling the
 *   communication between the boards. The link in the linkhandler has a name
 *   which is the same as the name space. Please note that the link name often
 *   differs in the 2 directions of a link.
 *
 *   When you create a process you have the possibility to separate all threads
 *   within the process in a separate namespace. The namespace is entered into
 *   the itc_init call. If you give the process a namespace then a thread
 *   outside the process will have to locate the mailbox with
 *   namespace/mailbox. To locate mailboxes outside of the process from threads
 *   within the process no namespace has to be used.
 *
 *
 *   4. Message send and receive
 *   ---------------------------
 *
 *   For the communication between mailboxes ITC uses messages. If a message
 *   is sent between 2 mailboxes within the same process the message is kept
 *   at the same memory location and only the message pointer is entered into
 *   the receiving mailbox. If a message is sent to a mailbox in another
 *   process the message needs to be copied into the memory of the receiving
 *   process.
 *
 *
 *   4.1 ITC messages and the itc_msg union
 *
 *   The ITC messages must comply to a specific format. An ITC message always
 *   begins with an ITC message number which has type uint32_t.
 *   The most common implemetation of the ITC messages is to define the messages
 *   as structs which start with the message number and then contain all the
 *   other needed information. All the messages used within a c file or entire
 *   application are then put into the itc_msg union. The itc_msg union also
 *   contains an entry for only the uint32_t msgno.
 *
 *   See the below example:
 *
 *   #define MSGNO_1 0x11
 *   struct message_1 {
 *           uint32_t msgno;
 *           int val1;
 *           unsigned int reserved;
 *   };
 *
 *   #define MSGNO_2 0x22
 *   struct message_2 {
 *           uint32_t msgno;
 *           int val;
 *   };
 *
 *   union itc_msg {
 *           uint32_t         msgno;
 *           struct message_1 message_1;
 *           struct message_2 message_2;
 *   };
 *
 *   You can use the following as a message reception loop:
 *   void message_loop(void)
 *   {
 *           union itc_msg *msg, *respmsg;
 *
 *           for(;;) {
 *                   msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO. ITC_FROM_ALL);
 *                   switch(msg->msgno) {
 *                   case MSGNO_1:
 *                           Handle message and send response
 *                           respmsg = itc_alloc(sizeof(struct message_2),
 *                                               MSGNO_2);
 *                           respmsg->message_2.val = 0;
 *                           itc_send(&respmsg, itc_sender(msg), ITC_MY_MBOX);
 *                           break;
 *                   default:
 *                           Unexpected message
 *                           break;
 *                   }
 *                   itc_free(&msg);
 *           }
 *   }
 *
 *   4.2 Messages and endians
 *
 *   ITC does not ordinarily handle any endian conversion, the only exception
 *   is the message number. The message number will always be converted by ITC
 *   to the endianess of the running system.
 *
 *
 *   4.3 Receive filter
 *
 *   In itc_receive you have the possibility to add a receive filter to
 *   only receive certain specified messages. It is more efficient to receive
 *   all messages and if you need to use the filter try to limit the number of
 *   message numbers that you filter on.
 *
 *   The filter parameter in itc_receive is defined as a pointer uint32_t.
 *   This is really an array where the first entry is how many
 *   message numbers you wish to filter for. Then you enter the message
 *   numbers into the coming entries in the array. If you want to receive
 *   all messages you can either set the filter pointer to NULL or
 *   ITC_NOFILTER or you can set filter[0] to 0.
 *
 *   Example of a 2 message number filter:
 *   uint32_t rx_filter[] = { 2, MSGNO_1, MSGNO_2 };
 *
 *
 *   5. Locate and monitor
 *   ---------------------
 *
 *   To communicate with an ITC mailbox you need to know its mailbox
 *   identity. To get a mailbox identity for a mailbox name you need to
 *   locate it. A mailbox can be located in 2 different ways. One
 *   synchronous and one asynchronous.
 *
 *   5.1 Synchronous locate
 *
 *   itc_locate takes a mailbox name as parameter. It will block for the
 *   time it takes to search through the existing mailboxes on the processor.
 *   If the mailbox is found the mailbox id is returned. If the mailbox is not
 *   found itc_locate returns ITC_NO_ID.
 *
 *   The itc_locate call might need to search in another context than that of
 *   the calling thread so it might sleep in the itc_locate call.
 *
 *
 *   5.2 Asynchronous locate
 *
 *   itc_locate_async takes a mailbox name and a message
 *   pointer as parameters. The message pointer is the message you wish
 *   to have returned when the mailbox is found. If you set the message
 *   pointer to NULL you will get a message with message number set to
 *   ITC_LOCATE_DEFAULT_NO returned to you.
 *
 *   If the mailbox exists when itc_locate_async is called the message is
 *   returned into the calling threads mailbox imediately with the sender
 *   of the message set to the id of the mailbox you were locating. If
 *   the mailbox does not exist ITC will keep the locate message and if
 *   the mailbox is created later the message will be sent to the mailbox
 *   of the calling thread. The sender of the message will be set to the
 *   id of the mailbox that were located.
 *
 *
 *   5.3 Monitor
 *
 *   When you have the ID of a mailbox you can monitor its continued
 *   existance with itc_monitor. itc_monitor takes a mailbox ID and
 *   a message pointer as parameters. The message pointer is the message
 *   you wish to have sent to you when the mailbox is terminated. If the
 *   message pointer is NULL then you will get a message with message number
 *   ITC_MONITOR_DEFAULT_NO returned. If you try to monitor a mailbox that
 *   has already terminated you will get the monitor message returned to you
 *   immediately.
 *
 *   itc_monitor returns a monitor ID that can be used to cancel the monitor
 *   request using itc_unmonitor. Please note that you can still get the
 *   monitor message returned to you even after you call itc_unmonitor if
 *   the message was sent prior to the itc_unmonitor call.
 *
 *
 *   6. Errorhandlers
 *   ----------------
 *
 *   ITC calls errorhandlers when it encounters an error. Errorhandlers can
 *   be registered with itc_register_errorhandler. The registered function
 *   pointer will be called when an error is triggered.
 *
 *   More than one errorhandler can be registered. All registered
 *   errorhandlers will run sequentially at the time of error. If an error
 *   handler takes care of the error and wishes that execution resumes it
 *   shall return 0, and no more error handlers will be executed. The running
 *   ITC function will return and execution will resume.
 *
 *   ITC will always install a default error handler which will be executed
 *   last of the errorhandlers. If this errorhandler is run it will terminate
 *   the running process.
 *
 *   If ITC determines that an error is unrecoverable it will call the
 *   errorhandlers with flag ITC_ERROR_FATAL set. All errorhandlers will
 *   then be executed irregardles of what they return. The default errorhandler
 *   will terminate the process.
 *
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2013-01-10 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : First version.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Updated to support file and line correctly.
 *
 *   Revised : 2014-02-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added error codes for itc_init.
 *
 *   Revised : 2015-02-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added thread is to mailbox information struct.
 * ========================================================================
 */

#ifndef __ITC_H
#define __ITC_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>
#include <stddef.h>

#include <sys/types.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/**
 * ITC error codes, returned by itc_init
 */
/**
 * An internal ITC error occured, for instance errors from pthread calls.
 */
#define ITC_EINTERNAL_ERROR      -1
/**
 * itc_init already run in this process.
 */
#define ITC_EALREADY_INITIALISED -2
/**
 * Namespace string too long.
 */
#define ITC_ENS_TO_LONG          -3
/**
 * Not enough memory available in the system.
 */
#define ITC_EOUT_OF_MEMORY       -4
/**
 * itcworld not found, an itcworld has to be started prior to strting
 * an itc client process.
 */
#define ITC_ENO_WORLD            -5
/**
 * An illegal configuration of the allocation scheme was entered.
 * Probably configuring message sizes larger than the system can support.
 */
#define ITC_EILLEGAL_ALLOC_CFG   -6

/**
 * Not enough internal resources available to serve the request.
 */
#define ITC_EOUT_OF_RESOURCES    -7

/**
 * ITC message number base for messages used by the ITC implementation
 * The ITC message numbers has been reserved in the CRL message number
 * range which can be found in document 3/10260-CSX10109. Reservation
 * of CRL message number are kept in the CPP control git repository.
 */
#define ITC_MSG_BASE            0x01901000

/**
 * Max length of mailbox name (excluding terminating '\0')
 */
#define ITC_NAME_MAXLEN              255

/**
 * Max number mailboxes allowed within a process.
 */
#define ITC_MAX_MAILBOXES          65534

/**
 * Used to signify that an itc_mbox_id_t is not set or is missing.
 * 0 will never be used as a valid mailbox ID so we use it
 * to identify no ID.
 */
#define ITC_NO_ID                      0xFFFFFFFF

/**
 * Set itc_mbox_id_t as ITC_MY_MBOX
 */
#define ITC_MY_MBOX                    0xFFF00000

/**
 * Use ITC_FROM_ALL to get messages from all senders
 */
#define ITC_FROM_ALL                   0xFFF11111

/**
 * Use ITC_NO_TMO as tmo to get itc_receive to block forever
 * If no messages are received.
 */
#define ITC_NO_TMO                    -1

/**
 * If no message is used for locate asynchronously you will get a message
 * with this message number back.
 */
#define ITC_LOCATE_DEFAULT_NO   (ITC_MSG_BASE + 0)

/**
 * If no message is used for monitor you will get a message
 * with this message numer back.
 */
#define ITC_MONITOR_DEFAULT_NO  (ITC_MSG_BASE + 1)

/**
 * Identifier to use if you want to have no filter when you do itc_receive.
 */
#define ITC_NOFILTER NULL

/**
 * Identifier to use if you don't want a separaten namespace for your
 * ITC process. Used in itc_init.
 */
#define ITC_NO_NAMESPACE NULL

/**
 * ITC create mailbox flags and MACROS.
 */

/**
 * Some mailbox flags require an accompanying value, this will then be
 * the LSB 8 bits in the flag field.
 */
#define ITC_MBOX_VALUE_MASK   0x000000FF

/**
 * Assign a static ID to this mailbox. Allowed static IDs are 0 - 64.
 * The wanted static ID shall be set in the value bits.
 * For this to be allowed ITC needs to be built with --enable-static_ids.
 */
#define ITC_MBOX_STAT_ID_FLAG 0x00000100

/**
 * This mailbox shall be a shared mailbox.
 */
#define ITC_MBOX_SHARED       0x00000200

/**
 * If a namespace is given in itc_init this flags sets the mailbox to
 * be created outside of that namespace.
 */
#define ITC_MBOX_NO_NS        0x00000400

/**
 * These bits are reserved for ITC internal useage.
 */
#define ITC_MBOX_RESERVED     0x0000F000

/**
 * Macros to set and get the static ID in flags.
 */
#define ITC_MBOX_SET_STAT_ID(id) (ITC_MBOX_STAT_ID_FLAG | (ITC_MBOX_VALUE_MASK & id))
#define ITC_MBOX_GET_STAT_ID(flags) (flags & ITC_MBOX_VALUE_MASK)

/**
 * ITC fatal error flag.
 */
#define ITC_ERROR_FATAL 0x80000000


/**
 * Number of buffer size values for the ITC_POOL allocation scheme.
 */
#define ITC_POOL_BUFF_VALS 6

/**
 * Number of buffer size values for the ITC_POOL_FLEX allocation scheme.
 */
#define ITC_POOLFLEX_BUFF_VALS 8

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
/**
 * Mailbox identifier type.
 */
typedef uint32_t itc_mbox_id_t;

/**
 * Monitor ID type, used to unmonitor (cancel) a monitor request.
 */
typedef uint32_t itc_monitor_id_t;

/**
 * Mailbox state enumeration.
 */
typedef enum {
        MBOX_UNUSED,
        MBOX_INUSE,
        MBOX_CLONE,
        MBOX_SHARED,
        MBOX_DELETED
} mbox_state;

/**
 * Initialisation parameters for when itc_alloc uses malloc, since this
 * does not need any special parameters the structue only include a
 * reserved field.
 */
struct itc_malloc_parameters {
        unsigned int reserved;
};

/**
 * Information structure about the ITC_MALLOC allocation scheme.
 * Will be returned by itc_get_alloc_info.
 */
struct itc_malloc_info {
        long tot_malloc;
};

/**
 * Initialisation parameters for when itc_alloc uses its own pool
 * allocation scheme. The only neccessary parameters are the pool size.
 * This area will be allocated in the itc_init call and then split into
 * buffers as they are needed. To make the buffer handling efficient it
 * will only use the buffer sizes 32, 224, 992, 4064, 16352 and 65504 bytes.
 * Once memory is broken from free memory and assigned a buffer size it will
 * keep that buffer size until the process is restarted or itc_exit is called.
 */
struct itc_pool_parameters {
        unsigned int size;
};

/**
 * Information structure about the ITC_POOL allocation scheme.
 * Will be returned by itc_get_alloc_info.
 */
struct itc_pool_info {
        unsigned long totsize;
        unsigned long totfree;

        unsigned int size[ITC_POOL_BUFF_VALS];
        unsigned int allocated[ITC_POOL_BUFF_VALS];
        unsigned int free[ITC_POOL_BUFF_VALS];
};

/**
 * Initialisation parameters for when itc_alloc uses its own flexible pool
 * allocation scheme. The parameters are the pool size and the 8 message sizes
 * that shall be used by the allocation scheme.
 *
 * Once memory is broken from free memory and assigned a buffer size it will
 * keep that buffer size until the process is restarted or itc_exit is called.
 */
struct itc_pool_flex_parameters {
        unsigned int size;
        uint32_t msg_sizes[8];
};

/**
 * Information structure about the ITC_POOL_FLEX allocation scheme.
 * Will be returned by itc_get_alloc_info.
 */
struct itc_pool_flex_info {
        unsigned long totsize;
        unsigned long totfree;

        unsigned int size[ITC_POOLFLEX_BUFF_VALS];
        unsigned int allocated[ITC_POOLFLEX_BUFF_VALS];
        unsigned int free[ITC_POOLFLEX_BUFF_VALS];
};

/**
 * Initialisation parameters for when you wish to use your own allocation and
 * free functions on the backside itc_alloc and itc_free. Below are  typedefs
 * for the allocation and free function and a struct for entering them into
 * the itc_init call.
 */
typedef void *(userdef_alloc)(size_t size);
typedef void (userdef_free)(void *message);
typedef void *(userdef_info)(void);

struct itc_user_defined_parameters {
        userdef_alloc *alloc;
        userdef_free  *free;
        userdef_info  *info;
};

/**
 * Enumeration for all available itc_alloc allocation schemes.
 */
typedef enum {
        ITC_MALLOC = 0,
        ITC_POOL,
        ITC_POOL_FLEX,
        ITC_USER_DEFINED,
        ITC_NUM_SCHEMES
} itc_alloc_scheme;

union itc_scheme {
        struct itc_malloc_parameters       resvd;
        struct itc_pool_parameters         pool_parameters;
        struct itc_pool_flex_parameters    pool_flex_parameters;
        struct itc_user_defined_parameters user_defined_parameters;
};

struct itc_alloc_info {
        itc_alloc_scheme scheme;

        union {
                struct itc_malloc_info       malloc_info;
                struct itc_pool_info         pool_info;
                struct itc_pool_flex_info    pool_flex_info;
        } info;
};

struct itc_stats {
        long tx_msg;
        long rx_msg;
        long rx_wmsg;
        long rx_tmo;

        long allocs;
        long frees;

        long locates;
        long pend_locates;

        long monitors;

        long mymonitors;
        long pend_mymonitors;
};

struct itc_mbox_info {
        struct itc_stats   stats;

        long               rx_qlen;
        uint32_t           in_rx;
        uint32_t           filter[5]; /* We will store a maximum of 4
                                         filtered message numbers. */
        int                rxfd;
        itc_mbox_id_t      parent_id;

        uint32_t           mbox_id;
        mbox_state         state;
	pid_t              tid;
        char               name[1];
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   Initialises ITC for the current process.
 *
 *   @param mailbox_count  Max number of mailboxes used by this process.
 *
 *   @param alloc_scheme   Which scheme shall be used by
 *                         itc_alloc/itc_free.
 *
 *   @param scheme         Union specifying configuration parameters for
 *                         the allocation scheme.
 *
 *   @param name_space     Set ITC namespace of this linux process.
 *
 *   @param flags          Flags field controlling the behavior of
 *                         itc_init. There are currently no flags defined
 *                         bit to allow future extensions it shall be set
 *                         to 0.
 *
 *   @return               0 at success, at failure:
 *                         ITC_EINTERNAL_ERROR      internal ITC error.
 *                         ITC_EALREADY_INITIALISED itc_init already
 *                                                  run.
 *                         ITC_ENS_TO_LONG          Namespace to long.
 *                         ITC_EOUT_OF_MEMORY       itc_init not enough
 *                                                  memory available.
 *                         ITC_ENO_WORLD            itc_init could not
 *                                                  locate world.
 *                         ITC_EILLEGAL_ALLOC_CFG   Illegal alloc
 *                                                  configuration.
 *
 *   @par Globals:         --
 *
 *   This initialises the use of ITC for the current process. You need
 *   to call itc_init first in the process before any other ITC calls.
 *
 *   The initialisation consists of specifying how many mailboxes. For
 *   each of the number of mailboxes specified an entry in an mailbox
 *   array is allocated which will cost about 100-200 bytes per mailbox.
 *
 *   You also initialise which allocation scheme to use for ITC messages
 *   within this process. This is done with the alloc_scheme enum and the
 *   scheme union. Each possible scheme has a value in the enum and a
 *   corresponding struct in the enum. For more information about the
 *   schemes and initialisation parameters see the description of the
 *   itc_alloc_scheme enum and itc_scheme union.
 *
 *   With the namespace parameter you can give ITC in this linux process
 *   its own namespace for itc_locate purposes. If you do not want a
 *   separate namespace you set this to ITC_NO_NAMESPACE. If you
 *   give a name space all ITC mailboxes outside of this linux process
 *   will have to use "<namespace>/<mailbox name>" to locate a mailbox
 *   in this linux process. Within this linux process you do not have to
 *   use the namespace to locate mailboxes on the "outside".
 *
 */
/* ===================================================================== */
extern int itc_init(int32_t mailbox_count,
                    itc_alloc_scheme alloc_scheme,
                    union itc_scheme *scheme,
                    char *name_space,
                    uint32_t flags);

/* ===================================================================== */
/**
 *   Exits ITC for this process
 *
 *   @return         -
 *
 *   @par Globals:   --
 *
 *   This exits the usage of ITC from the running process. This is only
 *   allowed if there are no mailboxes currently active in the process.
 *   The process will call the error handlers if this criteria is not met.
 *
 */
/* ===================================================================== */
extern void itc_exit(void);

/* ===================================================================== */
/**
 *   Allocate an ITC message.
 *
 *   @param size       Size of the message to allocate.
 *
 *   @param msgno     Message number of the allocated message.
 *
 *   @return           Pointer to allocated message, will always return a
 *                     valid message pointer, if memory can not be
 *                     allocated the error handler will be called.
 *
 *   @par Globals:     --
 *
 *   Used to allocate an ITC message according to the allocation scheme
 *   set in itc_init. If you use the ITC_MALLOC allocation scheme you
 *   might be blocked if there is no memory readily available.
 *
 *   All other allocation schemes allocate the needed memory in itc_init
 *   and if ITC runs out of memory the error handler is called.
 *
 */
/* ===================================================================== */
extern union itc_msg *itc_alloc(size_t size, uint32_t msgno);

/* ===================================================================== */
/**
 *   Free ITC message.
 *
 *   @param msg        Pointer to ITC message pointer.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Frees ITC message. The inparameter msg is a pointer to a ITC message
 *   pointer when the itc_free function returns the ITC message pointer
 *   will be set to NULL. Trying to free an incorrect ITC message pointer
 *   will mean the the process exitserror handler is called.
 *
 */
/* ===================================================================== */
extern void itc_free(union itc_msg **msg);

/* ===================================================================== */
/**
 *   Get sender of an ITC message.
 *
 *   @param msg        Pointer to the ITC message to get sender for.
 *
 *   @return           The sender of the message.
 *
 *   @par Globals:     --
 *
 *   Get sender ITC message. If you enter an incorrect ITC message
 *   pointer the error handler will be called.
 *
 */
/* ===================================================================== */
extern itc_mbox_id_t itc_sender(union itc_msg *msg);

/* ===================================================================== */
/**
 *   Get receiver of an ITC message.
 *
 *   @param msg        Pointer to the ITC message to get receiver for.
 *
 *   @return           The receiver of the message.
 *
 *   @par Globals:     --
 *
 *   Get receiver of an ITC message. If you enter an incorrect ITC
 *   message pointer the error handler will be called.
 *
 */
/* ===================================================================== */
extern itc_mbox_id_t itc_receiver(union itc_msg *msg);

/* ===================================================================== */
/**
 *   Get size of ITC message.
 *
 *   @param msg        Pointer to the ITC message to get sender for.
 *
 *   @return           Size of the message.
 *
 *   @par Globals:     --
 *
 *   Get size of ITC message. If you enter an incorrect ITC message
 *   pointer the error handler will be called.
 *
 */
/* ===================================================================== */
extern size_t itc_size(union itc_msg *msg);

/* ===================================================================== */
/**
 *   Set size of ITC message.
 *
 *   @param msg        Pointer to the ITC message to get sender for.
 *
 *   @param size       New message size.
 *
 *   @return           Non zero if message size successfully set,
 *                     0 if message size not updated.
 *
 *   @par Globals:     --
 *
 *   Set size of ITC message. This can only be done if there is enough
 *   space in the message for the new size. If the message size is updated
 *   itc_setsize returns true (non zero).
 *
 */
/* ===================================================================== */
extern int32_t itc_setsize(union itc_msg *msg, int32_t newsize);

/* ===================================================================== */
/**
 *   Create ITC mailbox for current thread.
 *
 *   @param name       Name of the ITC mailbox. Can be max
 *                     ITC_NAME_MAXLEN characters.
 *
 *   @param flags      Flags field controlling the behavior of
 *                     itc_create_mailbox. Currently no flags are defined
 *                     but to allow future extensions the flags field
 *                     needs to be set to 0.
 *
 *   @return           Mail box ID of the created mailbox.
 *
 *   @par Globals:     --
 *
 *   Create a mailbox for use by the current thread. The mailbox will get
 *   its name from the inparameter. That name can be used by other
 *   mailboxes to locate this mailbox.
 *
 */
/* ===================================================================== */
extern itc_mbox_id_t itc_create_mailbox(const char *name,
                                        uint32_t flags);

/* ===================================================================== */
/**
 *   Delete ITC mailbox.
 *
 *   @param mbox_id    Mail box id of the mailbox to be deleted.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Delete the mailbox of the given mbox id. This is also used to delete
 *   cloned mailboxes. You are only allowed to delete cloned mailboxes or
 *   your own mailbox, failure to adhere to this will result in a call
 *   to the error handler.
 *
 */
/* ===================================================================== */
extern void itc_delete_mailbox(itc_mbox_id_t mbox_id);

/* ===================================================================== */
/**
 *   Get current ITC mailbox.
 *
 *   @return           Mailbox id of the current threads mailbox or
 *                     ITC_NO_ID if the current thread has no mailbox.
 *
 *   @par Globals:     --
 *
 *   Gets the ID of the current threads ITC mailbox. If no mailbox is
 *   setup for this thread the function returne ITC_NO_ID.
 *
 */
/* ===================================================================== */
extern itc_mbox_id_t itc_current_mbox(void);

/* ===================================================================== */
/**
 *   Locate ITC mailbox.
 *
 *   @param name       Name of the mailbox to be located.
 *
 *   @return           Mailbox id of the located mailbox or ITC_NO_ID if
 *                     no mailbox has been found.
 *
 *   @par Globals:     --
 *
 *   Locates the mailbox for the given name. It will search through all
 *   mailboxes currently known by ITC in the current namespace. If no
 *   mailbox is found the call will return ITC_NO_ID.
 *
 */
/* ===================================================================== */
extern itc_mbox_id_t itc_locate(const char *name);

/* ===================================================================== */
/**
 *   Locate ITC mailbox asynchronously.
 *
 *   @param name       Name of the mailbox to be located.
 *
 *   @param msg        Pointer to message to be returned when the mailbox
 *                     is found. If msg is NULL a message with message
 *                     number ITC_LOCATE_DEFAULT_NO will be returned.
 *
 *   @param from       Mailbox id that shall receive the locate message
 *                     when the mailbox has been located. Set from to
 *                     ITC_MY_MBOX to get it to your own mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Locates the mailbox for the given name. When found the message in
 *   msg will be returned to the calling thread. If msg is NULL a
 *   message with message number ITC_LOCATE_DEFAULT_NO will be returned.
 *
 *   The message will be kept by ITC in wait for the creation of
 *   the requested mailbox. msg will be set to NULL before
 *   itc_locate_async returns.
 *
 *   It is possible to get the response message to another mailbox than
 *   your own by setting from to something alse than ITC_MY_MBOX.
 *
 */
/* ===================================================================== */
extern void itc_locate_async(const char *name,
                             union itc_msg **msg,
                             itc_mbox_id_t from);

/* ===================================================================== */
/**
 *   Get file descriptor of ITC mailbox.
 *
 *   @return           File descriptor of the current threads mailbox.
 *
 *   @par Globals:     --
 *
 *   Gets a filedescriptor for the ITC mailbox in the current thread that
 *   can be used by select to determine if there is any receive messages
 *   in the mailbox.
 *
 *   The first use of itc_getfd creates the file descriptor and returns
 *   it to the caller. There will be data in the fiel descriptor if there
 *   are any receive messages in the ITC mailbox.
 *
 */
/* ===================================================================== */
extern int itc_get_fd(void);

/* ===================================================================== */
/**
 *   Get name of ITC mailbox.
 *
 *   @param mbox_id      Mailbox id to get name for.
 *
 *   @param name         Pointer to string where mailbox name shall
 *                       be stored. The returned name will be '\0'
 *                       terminated.
 *
 *   @param name_len     How many bytes can be stored in the name string.
 *                       If there is not sufficient room the name will be
 *                       cut and only the beginning of the name present.
 *
 *   @return             true if the name for the mailbox has been found
 *                       and false otherwise.
 *
 *   @par Globals:     --
 *
 *   Gets you the name of an existing mailbox. The name is stored in the
 *   name string. If there is not sufficient room for the mailbox name
 *   only the characters that fits into the name string is entered. The
 *   name string will always be '\0' terminated.
 *
 *   If the mailbox id does not exist the function returns false
 *   otherwise it returns true.
 *
 */
/* ===================================================================== */
extern int32_t itc_get_name(itc_mbox_id_t mbox_id,
                            char *name,
                            uint32_t name_len);

/* ===================================================================== */
/**
 *   Get real mailbox id for static mailbox id.
 *
 *   @param mbox_id      Mailbox id to get real id for.
 *
 *   @return             Real mailbox id.
 *
 *   @par Globals:     --
 *
 *   Gets the real mailbox id for a statically assigned mailbox id
 *   through itc_create_mailbox.
 *
 *   Only supported when ITC built with --enable-static_ids.
 *
 */
/* ===================================================================== */
extern itc_mbox_id_t itc_get_real_mbox(itc_mbox_id_t mbox_id);

/* ===================================================================== */
/**
 *   Send ITC message
 *
 *   @param msg        Pointer to ITC message pointer that shall be sent.
 *
 *   @param to         Mailbox that message shall be sent to.
 *
 *   @param from       Mailbox id that shall be set as ITC message sender.
 *                     For my own mailbox set ITC_MY_MBOX as from.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Sends ITC message to the mailbox set by to. As sender of the ITC
 *   message the mailbox ID given in from will be used. To set your own
 *   mailbox as sender use ITC_MY_MBOX as from.
 *
 *   When itc_send returns *msg will be set to NULL to avoid inadvertent
 *   reuse of the message pointer.
 *
 */
/* ===================================================================== */
extern void itc_send(union itc_msg **msg,
                     itc_mbox_id_t to,
                     itc_mbox_id_t from);

/* ===================================================================== */
/**
 *   Receive messages from your ITC mailbox.
 *
 *   @param filter     Which message numbers you wish to receive.
 *                     Set to ITC_NOFILTER to get all messages.
 *
 *   @param tmo        How many milliseconds the itc_receive shall block
 *                     and wait for messages. Use 0 to get a non blocking
 *                     call and ITC_NO_TMO to block forever.
 *
 *   @param from       Mailbox id that messages shall be received from,
 *                     use ITC_FROM_ALL to receive all incoming messages.
 *
 *   @return           Pointer to received message from ITC mailbox or
 *                     NULL if no message was received.
 *
 *   @par Globals:     --
 *
 *   Receive messages from your ITC mailbox.
 *
 *   You have the possibility to filter which messages to receive using
 *   the filter parameter. The filter parameter is a pointer to an array
 *   with message numbers. Entry 0 in the array tells ITC how many message
 *   numbers you wish to receive. The rest of the array contains the
 *   message numbers that shall be received. If you set the number of
 *   messages to receive to 0 all messages will be received. If you set the
 *   number of messages you wish to receive as negative you receive all
 *   message numbers but the ones given in the arrray. Please note that
 *   using the filter to selectively receive messages induces linnear
 *   searches that can cost quite a lot so if you use this you should
 *   filter for as few message numbers as possible.
 *
 *   The timeout is set in milliseconds where 0 means do not wait for
 *   messages and ITC_NO_TMO means wait forever for messages.
 *
 *   You can also receive messages sent by only one specific mailbox,
 *   this is done by the from parameter. To get messages from all
 *   mailboxes use ITC_FROM_ALL.
 *
 */
/* ===================================================================== */
extern union itc_msg *itc_receive(const uint32_t *filter,
                                  int32_t tmo,
                                  itc_mbox_id_t from);

/* ===================================================================== */
/**
 *   Monitor the existence of a mailbox.
 *
 *   @param who        Mailbox id of the mailbox you wish to monitor.
 *
 *   @param msg        Message that you wish to get when the mailbox is
 *                     deleted. If msg is set to NULL you will get a
 *                     message with message number ITC_MONITOR_DEFAULT_NO
 *                     when the monitored mailbox is deleted.
 *
 *   @return           A monitor ID reference that can be used to
 *                     "unmonitor" the mailbox.
 *
 *   @par Globals:     --
 *
 *   Monitor the continued existance of a mailbox. The mailbox you monitor
 *   is set by the who parameter.
 *
 *   The notification when a monitored mailbox is deleted or when its
 *   thread dissappears is sent with an ITC message. The message sent is
 *   the one given to the itc_monitor call in the msg parameter, the
 *   sender of the messag is set to the dissappeared mailbox id. If you
 *   use NULL as msg you will get an ITC message with message number
 *   ITC_MONITOR_DEFAULT_NO sent to you when the mailbox dissappears.
 *
 *   itc_monitor returns an itc_monitor_id_t identifier that can be used
 *   to unmonitor to a mailbox. If you try to monitor a mailbox which does
 *   not exist the monitor will trigger at once, meaning that you will get
 *   a valid monitor id returned and then the monitor message is sent to
 *   you.
 *
 */
/* ===================================================================== */
extern itc_monitor_id_t itc_monitor(itc_mbox_id_t who, union itc_msg **msg);

/* ===================================================================== */
/**
 *   Unmonitor (cancel) the monitoring of a mailbox.
 *
 *   @param monitor_id The ID returned by itc_monitor that you wish
 *                     to "cancel".
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Unmonitor (cancel) a requested monitor of a mailbox. The monitor_id
 *   identifies which monitor request that shall be removed. It is allowed
 *   to do unmonitor on a monitor id which has already been triggered.
 *
 *   If you unmonitor a mailbox which has dissappeared and the monitor
 *   signal is already in the mailbox inqueue that message is not deleted
 *   and will be delivered even after the itc_unmonitor call has been made.
 *
 */
/* ===================================================================== */
extern void itc_unmonitor(itc_monitor_id_t monitor_id);

/* ===================================================================== */
/**
 *   Type definition of the itc_errorhandler function
 *
 *   @param itc_call   ITC call that is in progress when the error
 *                     occured.
 *
 *   @param msg        Error message.
 *
 *   @param flags      Error flags. So far the only defined flag is
 *                     ITC_ERROR_FATAL which will be set the error is
 *                     unrecoverable.
 *
 *   @param file       File where error occured.
 *
 *   @param line       Line where error occured.
 *
 *   @return           0 if error sucessfully dealt with by error handler.
 *
 *   @par Globals:     --
 *
 *   The itc_errorhandler is a type definition of the error handler
 *   function that can be registered to ITC. This function will then be
 *   called when ITC encounters an error. If the error handler has handled
 *   the error it should return 0 and execution might continue without
 *   further interruption.
 *
 *   The file and line contains information about where the error occurred.
 *   Please note that this information will only be available if the
 *   ITC library has been built with compile time flag ITC_FILEANDLINE set.
 *   If the flag is not set file will be an empty string and line will be
 *   set to 0.
 *
 */
/* ===================================================================== */
typedef int (*itc_errorhandler)(const char *itc_call,
                                const char *msg,
                                uint32_t    flags,
                                const char *file,
                                int         line);

/* ===================================================================== */
/**
 *   Register an error handler for ITC
 *
 *   @param errh       Error handler function that shall be invoked at
 *                     error in ITC.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Registers an error handler to be invoked when ITC encounters an error.
 *   The error handler takes a message from ITC that identifies which error
 *   it is and also file and line information. If the error handler is able
 *   to resolve the error it returns 0 execution will resume unless the
 *   ITC_ERROR_FATAL flag is set, if not the process will exit.
 *
 *   You are allowed to register multiple error handlers from a process.
 *   The errorhandlers will be run sequentially. Each one can say that it
 *   dealt with the error by returning 0. Once one error handler has
 *   returned 0 the execution is resumed without calling the remaining
 *   error handlers.
 *
 *   ITC will always "register" a default error handler which will be run
 *   last. If no other error handler has handled an error the default error
 *   handler will cause the process to exit.
 *
 */
/* ===================================================================== */
extern void itc_register_errorhandler(itc_errorhandler errh);

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
extern int __itc_init(int32_t mailbox_count,
                      itc_alloc_scheme alloc_scheme,
                      union itc_scheme *scheme,
                      char *name_space,
                      uint32_t flags,
                      const char *file,
                      int line);

#ifdef ITC_FILEANDLINE
#define itc_init(mbcnt, asch, sch, ns, fl) __itc_init((mbcnt), (asch), (sch), (ns), (fl), __FILE__, __LINE__)
#else
#define itc_init(mbcnt, asch, sch, ns, fl) __itc_init((mbcnt), (asch), (sch), (ns), (fl), "", 0)
#endif


extern void __itc_exit (const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_exit() __itc_exit(__FILE__, __LINE__)
#else
#define itc_exit() __itc_exit("", 0)
#endif


extern union itc_msg *__itc_alloc(size_t size,
                                  uint32_t msgno,
                                  const char *file,
                                  int line);
#ifdef ITC_FILEANDLINE
#define itc_alloc(size, msgno) __itc_alloc((size), (msgno), __FILE__, __LINE__)
#else
#define itc_alloc(size, msgno) __itc_alloc((size), (msgno), "", 0)
#endif


extern void __itc_free(union itc_msg **msg, const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_free(msg) __itc_free((msg), __FILE__, __LINE__)
#else
#define itc_free(msg) __itc_free((msg), "", 0)
#endif


extern itc_mbox_id_t __itc_sender(union itc_msg *msg, const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_sender(msg) __itc_sender((msg), __FILE__, __LINE__)
#else
#define itc_sender(msg) __itc_sender((msg), "", 0)
#endif


extern itc_mbox_id_t __itc_receiver(union itc_msg *msg, const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_receiver(msg) __itc_receiver((msg), __FILE__, __LINE__)
#else
#define itc_receiver(msg) __itc_receiver((msg), "", 0)
#endif


extern size_t __itc_size(union itc_msg *msg, const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_size(msg) __itc_size((msg), __FILE__, __LINE__)
#else
#define itc_size(msg) __itc_size((msg), "", 0)
#endif


extern int32_t __itc_setsize(union itc_msg *msg,
                             int32_t newsize,
                             const char *file,
                             int line);
#ifdef ITC_FILEANDLINE
#define itc_setsize(msg, newsize) __itc_setsize((msg), (newsize), __FILE__, __LINE__)
#else
#define itc_setsize(msg, newsize) __itc_setsize((msg), (newsize), "", 0)
#endif

extern itc_mbox_id_t __itc_create_mailbox(const char *name,
                                          uint32_t flags,
                                          const char *file,
                                          int line);
#ifdef ITC_FILEANDLINE
#define itc_create_mailbox(name, flags) __itc_create_mailbox(name, flags, __FILE__, __LINE__)
#else
#define itc_create_mailbox(name, flags) __itc_create_mailbox(name, flags, "", 0)
#endif


extern void __itc_delete_mailbox(itc_mbox_id_t mbox_id, const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_delete_mailbox(mbox_id) __itc_delete_mailbox((mbox_id), __FILE__, __LINE__)
#else
#define itc_delete_mailbox(mbox_id) __itc_delete_mailbox((mbox_id), "", 0)
#endif

extern itc_mbox_id_t __itc_current_mbox(const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_current_mbox() __itc_current_mbox(__FILE__, __LINE__)
#else
#define itc_current_mbox() __itc_current_mbox("", 0)
#endif

extern itc_mbox_id_t __itc_locate(const char *name, const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_locate(name) __itc_locate((name), __FILE__, __LINE__)
#else
#define itc_locate(name) __itc_locate((name), "", 0)
#endif

extern void __itc_locate_async(const char *name,
                               union itc_msg **msg,
                               itc_mbox_id_t from,
                               const char *file,
                               int line);
#ifdef ITC_FILEANDLINE
#define itc_locate_async(name, msg, from) __itc_locate_async((name), (msg), (from), __FILE__, __LINE__)
#else
#define itc_locate_async(name, msg, from) __itc_locate_async((name), (msg), (from), "", 0)
#endif

extern int __itc_get_fd(const char *file, int line);
#ifdef ITC_FILEANDLINE
#define itc_get_fd() __itc_get_fd(__FILE__, __LINE__)
#else
#define itc_get_fd() __itc_get_fd("", 0)
#endif


extern int32_t __itc_get_name(itc_mbox_id_t mbox_id,
                              char *name,
                              uint32_t name_len,
                              const char *file,
                              int line);
#ifdef ITC_FILEANDLINE
#define itc_get_name(mbox_id, name, mnl) __itc_get_name((mbox_id), (name), (mnl), __FILE__, __LINE__)
#else
#define itc_get_name(mbox_id, name, mnl) __itc_get_name((mbox_id), (name), (mnl), "", 0)
#endif


extern itc_mbox_id_t __itc_get_real_mbox(itc_mbox_id_t mbox_id,
                                         const char *file,
                                         int line);
#ifdef ITC_FILEANDLINE
#define itc_get_real_mbox(mbox_id) __itc_get_real_mbox((mbox_id), __FILE__, __LINE__)
#else
#define itc_get_real_mbox(mbox_id) __itc_get_real_mbox((mbox_id), "", 0)
#endif


extern void __itc_send(union itc_msg **msg,
                       itc_mbox_id_t to,
                       itc_mbox_id_t from,
                       const char *file,
                       int line);
#ifdef ITC_FILEANDLINE
#define itc_send(msg, to, from) __itc_send((msg), (to), (from), __FILE__, __LINE__)
#else
#define itc_send(msg, to, from) __itc_send((msg), (to), (from), "", 0)
#endif


extern union itc_msg *__itc_receive(const uint32_t *filter,
                                    int32_t tmo,
                                    itc_mbox_id_t from,
                                    const char *file,
                                    int line);
#ifdef ITC_FILEANDLINE
#define itc_receive(flt, tmo, from) __itc_receive((flt), (tmo), (from), __FILE__, __LINE__)
#else
#define itc_receive(flt, tmo, from) __itc_receive((flt), (tmo), (from), "", 0)
#endif


extern itc_monitor_id_t __itc_monitor(itc_mbox_id_t who,
                                      union itc_msg **msg,
                                      const char *file,
                                      int line);
#ifdef ITC_FILEANDLINE
#define itc_monitor(who, msg) __itc_monitor((who), (msg), __FILE__, __LINE__)
#else
#define itc_monitor(who, msg) __itc_monitor((who), (msg), "", 0)
#endif


extern void __itc_unmonitor(itc_monitor_id_t monitor_id,
                            const char *file,
                            int line);
#ifdef ITC_FILEANDLINE
#define itc_unmonitor(mbox_id) __itc_unmonitor((mbox_id), __FILE__, __LINE__)
#else
#define itc_unmonitor(mbox_id) __itc_unmonitor((mbox_id), "", 0)
#endif

extern void __itc_register_errorhandler(itc_errorhandler errh,
                                        const char *file,
                                        int line);
#ifdef ITC_FILEANDLINE
#define itc_register_errorhandler(errh) __itc_register_errorhandler((errh), __FILE__, __LINE__)
#else
#define itc_register_errorhandler(errh) __itc_register_errorhandler((errh), "", 0)
#endif

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ITC_H */

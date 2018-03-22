/**
 * @copyright
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#ifndef CONN_ESTABLISH_HELPER_H
#define CONN_ESTABLISH_HELPER_H

#include <stdint.h>
#include <stdbool.h>
#include <itc.h>
#include "conn-establish.h"

/**
   @file conn-establish-helper.h
   @brief Message interface for the connection establish mechanism

   ### General ###

   This header file describes the function interface for the connection
   establish support lib.

   The purpose of the lib is to facilitate the implementation of
   the connection establish mechanism according to the design rule
   5/10260-HRB105700.

   It only implements the protocol/interface version negotiation.
   (Capability negotiation is NOT part of this.)

   The use of this function interface is mandatory on the server side.
   Use of the client side functions is sort of optional. If you want to
   expose a pure message interface, that's OK, but then the client side
   functionality for connecting and disconnecting needs to be
   (re-)implemented by the client.

   The lib enforces these design rules:
   MRDR_SIG:6
   MRDR_SIG:7
   MRDR_SIG:31
   MRDR_SIG:33 (for messages concerning the connection establish mechanism)
   MRDR_SIG:50 (partly, freeing client_data and resources not part of the
                actual connection establish mechanism is up to the you who
                implement the server)
   MRDR_SIG:51

   ### Message numbers and Message header. ###

   See conn-establish.h for further information.

   ### Server side ###

   #### Initialization ####

   The server side needs to be initialized using conn_establish_server_init().
   You need to provide a list of supported versions and the a list of message
   numbers to be used by the connection establish mechanism. Also you can limit
   the number of simultaneous clients if you like.
   In return you get a handle to be used in all subsequent server side functions.
   (The handle is in reality a pointer to an internal data structure, so don't
   mess with it.)


   #### Message checking/filtering ####

   All messages received needs to be handled/filtered by the function
   conn_check_client(). It handles the connection establish, disconnect, and
   monitoring of the client. It also checks that the received message is from
   a valid client and maps it to the correct client_ref and returns any
   connection-specific client_data. \n
   Messages from any unknown client and all connection establish/disconnect
   messages are consumed.

   If conn_check_client() returns false then the message have been consumed
   and you can go back to the top of the receive loop. client_info will then
   contain no usable information.

   #### Callbacks ####
   You can provide four optional callbacks in the "conn_event_callbacks"
   struct when initializing the server using conn_establish_server_init().
   Callbacks you don't want to use can be set disabled by setting them to NULL
   in the conn_event_callbacks struct. (Initializing the server with NULL in
   place of a pointer to conn_event_callbacks struct disables all callbacks)\n
   If you're going to use "client_data" using conn_set_client_data() then
   "client_disconnecting_cb" and "client_died_cb" are required.\n\n

   "client_connecting_cb" is called when conn_check_client() is about to allow
   a client to connect. In this callback you might e.g. want to allocate data
   used for this client\n.
   If you for some reason don't want to connect this client you can return
   a non-zero error code that will be returned as reason in the conn_establish_rej
   message. To avoid ambigous error codes it is advisable to use other error
   codes than the result/error codes defined in conn-establish.h. If you chose
   to return an error code it's your responsibility to do the logging. \n\n

   "client_disconnecting_cb" is called when connected client has disconnected
   using conn_disconnect_req. In this callback you might want free the
   "client_data", if used, and do some other cleaning up. (You don't need to
   call conn_clear_client_data(), but you need to free your "client_data".) \n\n

   "client_died_cb" is called when connected client has died or removed its
   mailbox. In this callback you might want free the client_data, if used,
   and do some other cleaning up. (You don't need to call
   conn_clear_client_data(), but you need to free your "client_data".)\n\n

   "dropped_msg_cb" is called whenever conn_check_client() has decided to
   drop/consume an invalid message. In this callback you have a chance to
   handle or act on the situation. If you do, then free the message, or
   conn_check_client() will still log it as a dropped message from an
   invalid client.\n\n

   The callbacks "client_disconnecting_cb" and "client_died_cb" is very
   similar and you might wand to handle them in the same way. Then use the
   same callback for both an look at "state" in the "client_info" to see
   which one it was.


   #### Storing client-specific data ####

   Function conn_set_client_data() : \n
   If you need some data associated with a specific client you can make
   conn_check_client() return it to you when a message is received from that
   client.
   You just need to allocate your data and give the pointer to your data
   and the "server_ref" (connection_ref of the received message) to
   conn_set_client_data() and it will store it for you.
   You can only set this once.

   Function conn_clear_client_data(): \n
   Is used to remove the client-specific data you stored using
   conn_set_client_data().\n
   NOTE: You must free the allocated data yourself.


   ### Client side ###

   Using the client side functions is optional, if you want to expose a pure
   message interface. The drawback is then that the client need to
   (re-)implement the conn_establish() and conn_disconnect() functionality
   itself.

   If the interface towards the client would be a function based interface
   (or mixed interface) there are the functions conn_establish() and
   conn_disconnect() to help you. You probably don't want to expose them
   directly to the client as is,  but wrap them. Either into explicit connect
   and disconnect functions or use them implicitly when the client calls a
   function of your interface.

   Before you can start sending your own signals to the server you must set up
   the connection by calling conn_establish(), or sending the message directly.
   Any messages sent to the server prior to having an established connection
   will be discarded. (You won't get a response.)

   When you are done talking to the server you should disconnect from it using
   conn_disconnect(), or sending a disconnect message. This tells the server
   to forget you and free any resources allocated to this connection.

   Depending on the purpose/characteristics of the server, you might allow an
   established connection to stay up for ever, or require it to be disconnected
   immediately. The implementation of the lib have no opinion about that.

*/

/**
   A pointer to this struct is used as an argument to
   conn_establish_server_init(), conn_establish()
   and conn_disconnect() to tell them what set of message
   numbers to use.

   The message numbers in this struct __must never change__ !\n
   If it does, then the interface negotiation will break.

   A suggestion defining a macro like the one below in your header file
   describing the interface between the client and the server, to
   ensure that both sides use the same set of message numbers.\n

   All message numbers are mandatory and need to be unique.

   @verbatim
   #define EXAMPLE_CONN_ESTABLISH_MESSAGES_STRUCT(name)              \
        struct conn_establish_msg_numbers  name =                 \
        {                                                         \
                EXAMPLE_CONN_ESTABLISH_REQ,                       \
                EXAMPLE_CONN_ESTABLISH_CFM,                       \
                EXAMPLE_CONN_ESTABLISH_REJ,                       \
                EXAMPLE_CONN_DISCONNECT_REQ,                      \
                EXAMPLE_CONN_DISCONNECT_CFM,                      \
                EXAMPLE_CONN_DISCONNECT_REJ,                      \
                EXAMPLE_CONN_MONITOR_FWD
        }
   @endverbatim
 */
struct conn_establish_msg_numbers {
	uint32_t establish_req;
	uint32_t establish_cfm;
	uint32_t establish_rej;
	uint32_t disconnect_req;
	uint32_t disconnect_cfm;
	uint32_t disconnect_rej;
	uint32_t monitor_fwd;
};

/**
   Enum defining possible state of a client. (used in struct conn_client_info)
 */
enum conn_client_state {
	CONN_ESTABLISH_STATUS_CONNECTED = 1,
	CONN_ESTABLISH_STATUS_CONNECTING,
	CONN_ESTABLISH_STATUS_DISCONNECTING,
	CONN_ESTABLISH_STATUS_DEAD
};

/**
   @typedef conn_server_handle_t
   The handle that conn_establish_server_init() returns that must be used
   by all subsequent calls to the server.
 */
typedef struct conn_server_data_internal *conn_server_handle_t;

/**
   A pointer to a conn_client_info struct should be provided to
   conn_check_client(). The struct will upon return be filled with
   the information concerning the client that sent the message to
   the server.
*/
struct conn_client_info {
	uint32_t client_ref;          /**< The client_ref to use in any reply
	                                message sent back to the client.*/
	uint32_t server_ref;          /**< The server_ref used to set or clear
	                                 client_data */
	uint32_t procedure_ref;       /**< The procedure_ref from the received
	                                 message. */
	itc_mbox_id_t sender;         /**< The sender of the received message */
	itc_mbox_id_t connected_mailbox; /**< The mailbox the client used when
	                                    connectiion was established */
	enum conn_client_state state; /**< The state of the client (mostly used
	                                 in callbacks)*/
	uint32_t protocol_version;    /**< The protocol version that was
	                                 negotiated when the connection was
	                                 established.*/
	conn_server_handle_t server_handle; /**< The handle that identifies this
	                                       server instance.*/
	void *client_data;            /**< A pointer to any client related data,
	                                 if set using conn_set_client_data(),
	                                 else NULL.*/
};

/**
   @typedef conn_event_connect_cb
   A callback of this type will be called when a client connects.
 */
typedef uint32_t (*conn_event_connect_cb)(struct conn_client_info *client_info);
/**
   @typedef conn_event_disconnect_cb
   A callback of this type will be called when a client disconnects or dies.
 */
typedef void (*conn_event_disconnect_cb)(struct conn_client_info *client_info);
/**
   @typedef conn_event_dropped_msg_cb
   A callback of this type will be called whenever a message is
   about to be dropped.
 */
typedef void (*conn_event_dropped_msg_cb)(union itc_msg **msg);


/**
   This struct is used as an argument to conn_establish_server_init().
   It lists the callbacks to be called by conn_check_client().
   Any unused callback can be set to NULL.
   "client_disconnecting_cb" and "client_died_cb" are required if you're
   going to use conn_set_client_data().
 */
struct conn_event_callbacks {
	conn_event_connect_cb     client_connecting_cb; /**< Called whenever a
	                                                   valid client is about
	                                                   to connect*/
	conn_event_disconnect_cb  client_disconnecting_cb; /**< Called whenever
	                                                      a client disconnects*/
	conn_event_disconnect_cb  client_died_cb;         /**< Called whenever
	                                                     a client dies */
	conn_event_dropped_msg_cb dropped_msg_cb; /**< Called whenever a message
	                                             is about to be dropped*/
};


/***************************/
/*  Client side functions  */
/***************************/

/**
 * @func conn_establish() (client side function)
 *
 * Sends a conn establish request to the server and negotiates
 * the interface/protocol version.
 *
 * @return
 *     CONN_ESTABLISH_SUCCESS or error code.
 *
 * @param[in] server_mailbox
 *     The mailbox id of the server
 *
 * @param[in] procedure_ref
 *     The procedure reference to use for this transaction.
 *     This is opaque to the server, it is used by the client to
 *     map responses to a specific request.
 *
 * @param[in] client_ref
 *     The client reference. A number identifying the client,
 *     will be sent back as connection_ref in all subsequent
 *     messages from the server.
 *
 * @param[in] requested_version_count
 *     The number of element s in the array "requested_versions"
 *
 * @param[in] requested_versions
 *     A pointer to a __variable length array__ with "requested_version_count"
 *     elements. \n
 *     The list is prioritized in the way that the first matching protocol
 *     will be used. I.e. not necessary the highest protocol number.\n
 *     So be sure to put the most preferred version first in the list.
 *
 * @param[in] msg_numbers
 *     A pointer to a conn_establish_msg_numbers struct containing the message
 *     numbers to be used by the conn establish mechanism
 *
 * @param[in] time_out
 *     The number of milliseconds to wait for the server to answer
 *     or 0 for infinite wait.
 *
 * @param[out] server_ref
 *     The server reference to be used as connection_ref in all subsequent
 *     messages sent to the server, or 0 if connection could not be established.
 *
 * @param[out] selected_version
 *     The first compatible protocol/interface version, or 0 if connection
 *     could not be established.
 */

uint32_t conn_establish(
        /*input parameters*/
        itc_mbox_id_t server_mailbox,
        uint32_t procedure_ref,
        uint32_t client_ref,
        uint32_t requested_version_count,
        uint32_t *requested_versions,
        struct conn_establish_msg_numbers *msg_numbers,
        uint32_t time_out,
        /*returned values*/
        uint32_t *server_ref,
        uint32_t *selected_version);


/**
 * @func conn_disconnect() (client side function)
 *
 * Sends a disconnect request to the server.
 *
 * @return
 *     CONN_ESTABLISH_SUCCESS or error code.
 *
 * @param[in] server_mailbox
 *     The mailbox id of the server
 *
 * @param[in] procedure_ref
 *     The procedure reference to use for this transaction.
 *     This is opaque to the server, it is used by the client to
 *     map responses to a specific request.
 *
 * @param[in] server_ref
 *     The server reference that was returned by conn_establish()
 *
 * @param[in] msg_numbers
 *     A pointer to a conn_establish_msg_numbers struct containing the message
 *     numbers to be used by the conn establish mechanism
 *
 * @param[in] time_out
 *     The number of milliseconds to wait for the server to answer
 *     or 0 for infinite wait.
 */
uint32_t conn_disconnect(
        /*input parameters*/
        itc_mbox_id_t server_mailbox,
        uint32_t procedure_ref,
        uint32_t server_ref,
        struct conn_establish_msg_numbers *msg_numbers,
        uint32_t time_out);


/***************************/
/*  Server side functions  */
/***************************/

/**
 * @def CONN_INIT_OK
 *      Connection establish server was successfully initalized.
 * @def CONN_INIT_INVALID_PARAMETER
 *      There was a problem with one or more of the paramters in the
 *      call to conn_establish_server_init()
 */
#define CONN_INIT_OK                 0
#define CONN_INIT_INVALID_PARAMETER  1

/**
 * @func conn_establish_server_init() (Server side function)
 *
 *    Initializes the connection establish mechanism.\n
 *    This function must only be called once.
 *
 * @return
 *    If succeded : CONN_INIT_OK
 *    If failed   : CONN_INIT_INVALID_PARAMETER
 *
 * @param [out] handle
 *    A a pointer to the handle to be used in subsequent function calls.
 *
 * @param[in] supported_version_count
 *    The number of element s in the array "supported_versions"
 *
 * @param[in] supported_versions
 *    A pointer to a __variable length array__ with "supported_version_count"
 *    elements, containing the versions that this server supports.
 *
 * @param[in] msg_numbers
 *    A pointer to a conn_establish_msg_numbers struct containing the message
 *    numbers to be used by the conn establish mechanism
 *
 * @param[in] max_client_count
 *    In case you want to limit the number of simultaneous clients that
 *    this server can handle you can specify it here.\n
 *    Set to 0 to allow any number of clients.
 *
 * @param[in] callbacks
 *    A pointer to a struct contining callbacks to called by conn_check_client()
 *    in different situations. (NULL will disable all callbacks)
 *    See conn_event_callbacks and the "Callbacks" section in the overall
 *    description of this file for more info.
 */
int conn_establish_server_init( conn_server_handle_t *handle,
                                uint32_t supported_version_count,
                                uint32_t *supported_versions,
                                struct conn_establish_msg_numbers *msg_numbers,
                                uint32_t max_client_count,
                                struct conn_event_callbacks *callbacks);

/**
 * @func conn_check_client() (Server side function)
 *
 * This is the main function of the lib. It handles connection establish,
 * disconnects (intentional or not), checks that received messages comes
 * from connected clients and filter out (consumes) those that don't.
 *
 * For more info on how to use this function please have a look at the example
 * application.
 *
 *
 * @return
 *     true:
 *         If there is a message that needs to be handled by the server.\n
 *     false:
 *         If the the message was consumed. I.e was from an unknown client or
 *         a CONN_ESTABLISH_REQ message.
 *
 * @param[in] handle
 *         The handle returned in conn_establish_server_init().
 *
 * @param[in,out] message
 *         [in]  A pointer the received message.\n
 *         [out] Either a pointer to the actual received message or
 *         a conn_destroy_fwd message (if the client have died or disconnected)
 *         or NULL (if the message has been consumed).\n
 *         If the message returned is a conn_destroy_fwd then it's up to
 *         you to free all the of the client's resources including
 *         the client_data, if set.\n
 *         The message original message is consumed when false is returned.
 *
 * @param[out] client_info
 *         If the message is from an existing client then
 *         client_info will contain the client_ref to use in
 *         the reply, the protocol/interface version selected
 *         in the connection establish phase and the pointer
 *         to the client_data (if it has been set; see
 *         conn_set_client_data()).
 *         If message was consumed then content of client_info is cleared.
 *
 ********************************************************************/
extern bool conn_check_client(conn_server_handle_t handle,
                              union itc_msg **message,
                              struct conn_client_info *client_info);
/**
 * @def CONN_ESTABLISH_CLIENT_DATA_OK
 *      Client data was set or cleared as requested.
 * @def CONN_ESTABLISH_CLIENT_INFO_OK
 *      The client info was found and returned as requested.
 * @def CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET
 *      Setting of client data failed. \n
 *      You need to clear it before you set it again.
 * @def CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST
 *      You tried to set or clear client data for a non-existing client.
 * @def CONN_ESTABLISH_CLIENT_MISSING_CALLBACKS
 *      You tried to set client data without having the callbacks to
 *      clean up if the client dies or disconnects.
 */
#define CONN_ESTABLISH_CLIENT_DATA_OK           0
#define CONN_ESTABLISH_CLIENT_INFO_OK           0
#define CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET  1
#define CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST    2
#define CONN_ESTABLISH_CLIENT_MISSING_CALLBACKS 3
/**
 * @func conn_set_client_data() (Server side function)
 *
 * Attaches a pointer to some connection/client specific data to the
 * internal client information struct.
 * The pointer is returned in client_info whenever function conn_check_client()
 * returns a message to process.
 *
 * @return
 *     CONN_ESTABLISH_CLIENT_DATA_OK:
 *         The client_data was set correctly. \n
 *     CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET:
 *         The client_data was already set for this client.
 *         You need to clear the client_data before setting it again.\n
 *     CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST:
 *         You tried set the client data for a non-existing client.
 *
 * @param[in] handle
 *         The handle returned in conn_establish_server_init().
 *
 * @param[in] server_ref
 *         Is the connection_ref that the client sent to the server.
 *         It is used to associate the *client_data to a specific client.
 *
 * @param[in] *client_data
 *         A pointer to be stored in internal client information struct.
 */
extern int conn_set_client_data(conn_server_handle_t handle,
                                uint32_t server_ref,
                                void *client_data);


/**
 * @func conn_clear_client_data() (Server side function)
 *
 * Removes a pointer to the connection/client specific data from the
 * internal client information struct and returns it in **client_data.
 *
 * @return
 *     CONN_ESTABLISH_CLIENT_DATA_OK:
 *         The client_data was removed and returned in **client_data.
 *         Now it's up to you to free it.\n
 *     CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST:
 *         You tried to clear the client data of a non-existing client.
 *
 * @param[in] handle
 *         The handle returned in conn_establish_server_init().
 *
 * @param[out] client_data
 *         Either a pointer to the client data or NULL (if client data
 *         was not set or client did not exist).
 ********************************************************************/
extern int conn_clear_client_data(conn_server_handle_t handle,
                                  uint32_t server_ref,
                                  void **client_data);

/**
 * @func conn_get_client_info() (Server side function)
 *
 * Get the client_info struct for a specific server_ref.
 *
 * @return
 *     CONN_ESTABLISH_CLIENT_DATA_OK:
 *         The client_info was found and returned in *client_info.\n
 *     CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST:
 *         You tried to get client_info of a non-existing client.
 *
 * @param[in] handle
 *         The handle returned in conn_establish_server_init().
 *
 * @param[out] client_info
 *         The client_info for the requested client, or unchanged
 *         if the client does not exist.
 ********************************************************************/
extern int conn_get_client_info(conn_server_handle_t handle,
                                uint32_t server_ref,
                                struct conn_client_info *client_info);

#endif

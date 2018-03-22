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

#ifndef CONN_ESTABLISH_H
#define CONN_ESTABLISH_H

#include <stdint.h>
/**
   @file conn-establish.h
   @brief Message interface for the connection establish mechanism

   ### General ###

   This header file describes the message interface for the connection
   establish support lib.
   This file needs to be included in any message based public interface.

   The purpose of the lib is to facilitate the implementation of
   the connection establish mechanism according to the design rule
   5/10260-HRB105700.

   It only implements the protocol/interface version negotiation.
   (Capabilitity negotiation is NOT part of this.)


   ### Message numbers. ###

   The message numbers that is needed by the conn establish mechanism needs
   to be defined in your interface header file.\n
   __THESE MESSAGE NUMBERS MUST NEVER CHANGE!__\n
   All versions of the interface MUST have the same connection establish
   message numbers for all eternity, or the interface negotiation will break.
   A sugestion is therefore to put them in the beginning of your message
   number range, i.e suggesting omitting `<YOUR_OFFSET>` in the example below.

   The message numbers need to be unique and all of them are mandatory. \n
   @verbatim
   #define <YOUR_IF>_CONN_ESTABLISH_REQ  (<YOUR_IF>_MSG_BASE + <YOUR_OFFSET> + 1)
   #define <YOUR_IF>_CONN_ESTABLISH_CFM  (<YOUR_IF>_MSG_BASE + <YOUR_OFFSET> + 2)
   #define <YOUR_IF>_CONN_ESTABLISH_REJ  (<YOUR_IF>_MSG_BASE + <YOUR_OFFSET> + 3)
   #define <YOUR_IF>_CONN_DISCONNECT_REQ (<YOUR_IF>_MSG_BASE + <YOUR_OFFSET> + 4)
   #define <YOUR_IF>_CONN_DISCONNECT_CFM (<YOUR_IF>_MSG_BASE + <YOUR_OFFSET> + 5)
   #define <YOUR_IF>_CONN_DISCONNECT_REJ (<YOUR_IF>_MSG_BASE + <YOUR_OFFSET> + 6)
   #define <YOUR_IF>_CONN_MONITOR_FWD    (<YOUR_IF>_MSG_BASE + <YOUR_OFFSET> + 7)
   @endverbatim

   ### Message structs type definitions ###

   According to the design rule 5/10260-HRB105700 rev A, MRDR_SIG:31 and
   MRDR_SIG:28 __all message structs MUST start with__ :

   - `uint32_t msgno;`\n
   Well... It's the number identifying this type of message.

   - `uint32_t procedure_ref;`\n
   A procedure reference of your own choice. Each message sent
   can/should have a uniqe procedure reference, the same number must
   be returned in the reply. This makes it possible to have multiple
   outstanding request towards the server and map the replies to
   the requests. (Or if you're not going use multiple outstanding
   request, your client can just pick a number and stick to that.)

   - `uint32_t connection_ref;`\n
   Every message sent to the server must have the connection referens
   returned in the conn_establish_cfm when setting up the connection.\n
   Every message sent to the client must have the connection referens
   provided in the conn_establish_req when setting up the connection.\n
   (The only exception to this is the actual conn_establish_req/rej
   where it's the other way around).

   NOTE! According to the design rule (MRDR_SIG:33) all message payload must
   be in sent in network byte order (i.e. big endian).\n
   All struct members __except the above__ must be in network byte order,
   endianess conversion of msgno will be handled by the link handler.
   `connection_ref` and `procedure_ref` are just opaque handles, so their
   endianess does not matter, as long as they are treated as such.
   But all other struct members are in big endian.\n
   (The connection establish mechanism don't care about endianess of rest of
   the rest of your interface.)
*/


/*
 * Result/error codes returned from the server:
 * --------------------------------------------
 */

/** @def CONN_ESTABLISH_SUCCESS
 *      What you have requested has successfully been done.*/
#define CONN_ESTABLISH_SUCCESS                 0

/** @def CONN_ESTABLISH_REJ_TIME_OUT
 *      Server did not anwer in time. (Only returned by helper functions.)
 */
#define CONN_ESTABLISH_REJ_TIME_OUT            10

/** @def CONN_ESTABLISH_REJ_ALREADY_CONNECTED
 *      A client tried to connect using connection reference and mailbox id
 *      as existing connection.
 */
#define CONN_ESTABLISH_REJ_ALREADY_CONNECTED   11

/** @def CONN_ESTABLISH_REJ_UNSUPPORTED_VERSION
 *      The server don't support any of the requested versions.
 */
#define CONN_ESTABLISH_REJ_UNSUPPORTED_VERSION 12

/** @def CONN_ESTABLISH_REJ_NOT_A_CLIENT
 *      The client that try to disconect was not a client.
 */
#define CONN_ESTABLISH_REJ_NOT_A_CLIENT        13

/** @def CONN_ESTABLISH_REJ_INVALID_PARAMETER
 *      One or more paramter does not make sense
 */
#define CONN_ESTABLISH_REJ_INVALID_PARAMETER   14

/** @def CONN_ESTABLISH_REJ_MAX_CLIENTS_REACHED
 *      The maximum number (configurable in server) has been reached
 */
#define CONN_ESTABLISH_REJ_MAX_CLIENTS_REACHED 15

/** @def CONN_ESTABLISH_REJ_OUT_OF_MEMORY
 *      Allocation of memory failed
 */
#define CONN_ESTABLISH_REJ_OUT_OF_MEMORY 16


/**
 *  @brief General message struct.
 *
 *  A helper type to use when message has no payload or when message payload
 *  is uninteresting, e.g. when manupilating values in the header.
 */
typedef struct conn_any_msg {
	uint32_t msgno;          /**< The message number. */
	uint32_t procedure_ref;  /**< A number/handle to associate the request
	                            and it's responses to eachother.\n
	                            When sent to the server \- just any number.\n
	                            When sent to the client \- must be the same
	                            number as in the request.*/
	uint32_t connection_ref; /**< When sent to server \- the connection
	                            reference returned in conn_establish_cfm_t \n
	                            When sent to client \- the connection
	                            reference supplied in conn_establish_req_t*/
} conn_any_msg_t;

/**
 * @brief Connection establish request struct
 *
 * Sent to the server whenever a client want to connect.
 */
typedef struct conn_establish_req {
	uint32_t msgno;          /**< The message number. */
	uint32_t procedure_ref;  /**< See conn_any_msg_t */
	uint32_t connection_ref; /**< The connection reference the client want
	                            to use to identify this connection. \n
	                            The server returns this in all subsequent
	                            messages sent to the client.
	                            (Except in the responses to this request.
	                            I.e. in conn_establish_cfm_t)*/
	uint32_t protocol_count; /**< The number of elements in
	                            #protocol_versions array. */
	uint32_t protocol_versions[1];/**< A __varible length array__ with
	                                 #protocol_count elements. (Note: You need to
	                                 allocate a struct of appropriate size) \n
	                                 The list is prioritized in the way that
	                                 the first matching protocol will be used.
	                                 I.e. not necessary the highest
	                                 protocol number. \n
	                                 So be sure to put the most preferred
	                                 version first in the list.*/
} conn_establish_req_t;

/**
 * @brief Connection establish confirm struct
 *
 * Is returned to the client when the client has been accepted by the server.
 */
typedef struct conn_establish_cfm {
	uint32_t msgno;           /**< The message number. */
	uint32_t procedure_ref;   /**< See conn_any_msg_t */
	uint32_t connection_ref;  /**< The connection reference the server want
	                               the client to use to identify in all
	                               subsequent messages sent to the server. */
	uint32_t selected_protocol_version;/**< This is the first matching protocol
	                                      version in the list provided in
	                                      the request.*/
} conn_establish_cfm_t;

/**
 * @brief Connection establish reject struct
 *
 * Is returned to the client when the server has rejected the clients
 * request to connect.
 * The reason why, can be found in #reason
 */
typedef struct conn_establish_rej {
	uint32_t msgno;          /**< The message number. */
	uint32_t procedure_ref;  /**< See conn_any_msg_t */
	uint32_t connection_ref; /**< Not applicable - no connection was created */
	uint32_t reason;         /**< The reason the connection request failed */
	uint32_t supported_protocol_count; /**< The number of protocol versions
	                                      the server supports*/
	uint32_t supported_protocol_versions[1]; /**< The list of supported protocol versions.
	                                            (Array is of variable size and contains
	                                            #supported_protocol_count elements.)*/
} conn_establish_rej_t;


/* Disconnection structs */
/**
 *  @brief Disconnection request
 *
 *  Sent to the server when the client wants to end the connection.
 */
typedef struct conn_disconnect_req {
	uint32_t msgno;          /**< The message number */
	uint32_t procedure_ref;  /**< See conn_any_msg_t */
	uint32_t connection_ref; /**< See conn_any_msg_t */
} conn_disconnect_req_t;

/**
 *  @brief Disconnection confirm
 *
 *  Sent back the client when client has been disconnected,
 *  as a result of request to disconnect, see conn_disconnect_req_t.
 */
typedef struct conn_disconnect_cfm {
	uint32_t msgno;          /**< The message number */
	uint32_t procedure_ref;  /**< See conn_any_msg_t */
	uint32_t connection_ref; /**< See conn_any_msg_t */
} conn_disconnect_cfm_t;

/**
 *  @brief Disconnection reject
 *
 *  Sent back the client when client has __not__ been disconnected,
 *  as a result of request to disconnect, see conn_disconnect_req_t.
 *  The reason why, can be found in #reason
 */
typedef struct conn_disconnect_rej {
	uint32_t msgno;          /**< The message number */
	uint32_t procedure_ref;  /**< See conn_any_msg_t */
	uint32_t connection_ref; /**< See conn_any_msg_t */
	uint32_t reason;         /**< The reason the server could not
                                    disconnect the client. */
} conn_disconnect_rej_t;

#endif

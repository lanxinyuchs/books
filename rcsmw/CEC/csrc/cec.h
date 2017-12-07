/* ----------------------------------------------------------------------
 * %CCaseFile:	cec.h %
 * %CCaseRev:	/main/R2A/R3A/1 %
 * %CCaseDate:	2015-01-08 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Header file for CEC.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  cec.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and disseminations to the receivers employees shall
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R3A/1      2015-01-08 erarafo     Copyright update; resolving size_t
 * ----------------------------------------------------------------------
 */
#ifndef __CEC_H__
#define __CEC_H__

#ifdef __cplusplus
extern "C" {        /* Use C declarations for C++ */
#endif              /* __cplusplus                */

#include <stdlib.h>

  typedef struct {
    int socket;
  } cec_handle_t;

  typedef struct {
    unsigned int length; /* always unencoded in interface */
    void *       data;
  } cec_packet_t;

  /**
   * Opens a TCP connection to a CS service and returns a handle
   * to the connection. The given 'name' is a pointer to a byte
   * array. The array size is given by 'size'. The byte sequence
   * identifies a service known by the CS.
   *
   * The return value points to a cec_handle_t which contains a
   * socket. In case of failure NULL is returned.
   *
   * The cec_handle_t is allocated in heap memory and should be
   * freed when no longer needed. This action can be performed
   * by calling the cec_close function.
   *
   * The CS service is located by the CEC_HOST and CEC_PORT
   * environment variable values. CEC_HOST may be specified
   * as a hostname or an IPv4 dotted decimal value. CEC_PORT
   * may be specified as a decimal integer.
   *
   * If CEC_HOST is not set the loopback address will be used.
   * IF CEC_PORT is not set a default value encoded in cec.h will
   * be used.
   */
  cec_handle_t *cec_open(void *name, size_t size);

  /**
   * Sends the data specified by the given packet using the
   * socket specified by the given handle.
   *
   * The returned value is packet->length, or -1 in case of
   * failure.
   */
  int cec_send(cec_handle_t *handle, cec_packet_t *packet);

  /**
   * Sends the data specified by the given packet using the
   * socket specified by the given handle. This function sends the pid
   * of the calling process before the data in packet->data as 4 octets.
   *
   * The returned value is packet->length, or -1 in case of
   * failure.
   */
  int cec_send_with_pid(cec_handle_t *handle, cec_packet_t *packet);

  /**
   * Performs a cec_receive_w_tmeout() with zero timeout.
   */
  int cec_receive(cec_handle_t *handle, cec_packet_t *packet);

  /**
   * Receives a cec_packet_t from the socket specified by the
   * given handle. The second argument points to a cec_packet_t
   * whose .data field must be NULL.
   *
   * The returned value is packet->length, or -1 in case
   * of failure.
   *
   * In case of success packet->data points to a buffer of
   * size packet->length. This buffer should be freed by the
   * caller when no longer needed.
   *
   * In case of failure a buffer may or may not have been
   * allocated. If packet->data is NULL then it can be trusted
   * that no buffer was allocated. Otherwise the buffer pointed
   * to must be freed.
   */
  int cec_receive_w_tmeout(cec_handle_t *handle, cec_packet_t *packet,
			   unsigned int timeout);

  /**
   * Closes the given socket and frees the cec_handle_t.
   */
  int cec_close(cec_handle_t *handle);

#ifdef __cplusplus
}                   /* extern "C" { */
#endif              /* __cplusplus  */

#endif

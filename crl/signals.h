/******************************************************************************
 * Copyright (c) Ericsson AB 2014 All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and dissemination to the receivers employees shall
 * only be made on a strict need to know basis.
 */

#ifndef __SIGNALS_H
#define __SIGNALS_H

#include <itc.h>

#include "hwli.h"

#define CONFIRM   0
#define REJECT    1
#define TIMEOUT   2

itc_mbox_id_t init_itc(uint32_t *connection_reference);

void send_connect_req(itc_mbox_id_t dst,
                      uint32_t procedure_reference,
                      uint32_t connection_reference,
                      uint32_t nbr_of_supported_protocol_revisions,
                      uint32_t *protocol_revisions,
                      itc_mbox_id_t src);

void send_logread_req(itc_mbox_id_t dst,
                      uint32_t procedure_reference,
                      uint32_t connection_reference,
	                  uint32_t data_per_ack,
                      itc_mbox_id_t src);

void send_write_req(itc_mbox_id_t dst,
                    uint32_t procedure_reference,
                    uint32_t connection_reference,
                    char *id,
                    uint32_t filter,
                    uint32_t filter_on_msg,
                    char *text,
                    itc_mbox_id_t src);

void send_logerase_req(itc_mbox_id_t dst,
                       uint32_t procedure_reference,
                       uint32_t connection_reference,
                       itc_mbox_id_t src);

void send_logread_ack(itc_mbox_id_t dst,
                      uint32_t procedure_reference,
                      uint32_t connection_reference,
                      int32_t sequence_number,
                      itc_mbox_id_t src);

int get_connect_reply(itc_mbox_id_t src,
                      uint32_t *procedure_reference,
                      uint32_t *connection_reference,
                      uint32_t *protocol_revision,
                      int32_t timeout);

int get_logread_reply(itc_mbox_id_t src,
                    uint32_t *procedure_reference,
                    uint32_t *connection_reference,
                    uint32_t *len,
                    int32_t timeout);

int get_write_reply(itc_mbox_id_t src,
                    uint32_t *procedure_reference,
                    uint32_t *connection_reference,
                    int32_t timeout);

int get_logerase_reply(itc_mbox_id_t src,
                       uint32_t *procedure_reference,
                       uint32_t *connection_reference,
                       int32_t timeout);

int get_logread_data(itc_mbox_id_t src,
                     uint32_t *procedure_reference,
                     uint32_t *connection_reference,
                     int32_t  *sequence_number,
                     struct hwli_entry *entry,
                     int32_t timeout);

#endif /* __SIGNALS_H */

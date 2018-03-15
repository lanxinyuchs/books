/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

#ifndef ATFI_AU2_H_
#define ATFI_AU2_H_

#include <stdint.h>

/* Callback functions for link state notification. */
typedef void (*au2_cb_conn)(void *obj);
typedef void (*au2_cb_disc)(void *obj);

/* For Ericsson AU2. */
void *au2_ericsson_create(const char *au2_name, const char *dev_name,
                          uint8_t addr,
                          au2_cb_conn cb_conn, au2_cb_disc cb_disc,
                          const char *profile, const char *muxlib,
                          int channel);

int au2_ericsson_destroy(void *handle);

void au2_ericsson_get_link_info(char **buf,
                                int *buf_left,
                                void* connection,
                                uint16_t pa,
                                uint16_t la,
                                const char* state,
                                int first,
                                int reset,
                                int detailed);


/* For AISG AU2. */
void *au2_aisg_create(const char *srv_name,
                      uint8_t addr, uint8_t device_type,
                      uint8_t *uid, uint8_t uid_size,
                      au2_cb_conn cb_conn, au2_cb_disc cb_disc);

int au2_aisg_destroy(void *handle);

int au2_aisg_init(const char *mbox_name,
                  const char *dev_name,
                  const char *profile,
                  const char *muxlib,
                  int channel);

int au2_aisg_shutdown(void);

void au2_aisg_get_link_info(char **buf,
                            int *buf_left,
                            void* connection,
                            uint16_t pa,
                            uint16_t la,
                            const char* state,
                            int first,
                            int reset,
                            int detailed);

#endif

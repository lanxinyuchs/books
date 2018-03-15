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

#ifndef ATFI_CM_H_
#define ATFI_CM_H_

#include <stdint.h>

#include <ulh_cm.h>

#define ATFID_CM_LINX_PS    "linx_ps"
#define ATFID_CM_LINX_SS    "linx_ss"
#define ATFID_CM_GLH_PS     "glh_ps"

#define ATFID_DEV_NAME_MAX_SIZE 128
#define ATFID_SRV_NAME_MAX_SIZE  32


int atfid_init_cm_linx_ps(const char *name);
int atfid_init_cm_linx_ss(const char *name);
int atfid_init_cm_glh_ps(const char *name);

int linx_ps_get_info(char** buf, int* buf_left, uint16_t pa, uint16_t la,
                     const char* state, int first, int reset, int detailed);

int linx_ss_get_info(char** buf, int* buf_left, uint16_t pa, uint16_t la,
                     const char* state, int first, int reset, int detailed);

int glh_ps_get_info(char** buf, int* buf_left, uint16_t pa, uint16_t la,
                    const char* state, int first, int reset, int detailed);

struct ecb_cm_config {
	struct ulh_cm_config  cmn;
	uint8_t               addr;
	char                  device[ATFID_DEV_NAME_MAX_SIZE];
	char                  srv_name[ATFID_SRV_NAME_MAX_SIZE];
	char                  lib_mux[ATFID_DEV_NAME_MAX_SIZE];
	char                  profile_name[ATFID_DEV_NAME_MAX_SIZE];
	int                   channel;
};

#endif

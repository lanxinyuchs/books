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

#ifndef ECB_STAT_H_
#define ECB_STAT_H_

/**
 *
 * @file     ecb_stat.h
 *
 * @brief    Interface to link statistics shared between different processes.
 */
#include "ecb_dev.h"

struct ecb_stat_info;

struct ecb_stat_info * ecb_stat_create(const char *dev_fname);
int ecb_stat_destroy(struct ecb_stat_info *info);
int ecb_stat_read(struct ecb_stat_info *info, struct ecb_dev_stat_hdlc *stat);
int ecb_stat_update(struct ecb_stat_info *info, struct ecb_dev_stat_hdlc *stat);
int ecb_stat_reset(struct ecb_stat_info *info);

#endif /* ECB_STAT_H_ */

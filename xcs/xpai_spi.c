/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <string.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>
#include <pthread.h>
#include <stdbool.h>
#include <itc.h>
#include <conn-establish-helper.h>
#include "rhd-common.h"

#include "common.h"
#include "log_tpt.h"
#include "xpai_xsp_if.h"

#define  XPAI_XSP_SPI_MAX_PORT_NUM  2
#ifndef __MACHINE_ZYNQMP_MIMO
#define  XPAI_SPI_MAX_SLAVE_NUM     96
#define SPI_SLAVES_PER_DEV          16  /* The actual number slaves per device     */
#else
#define  XPAI_SPI_MAX_SLAVE_NUM     6
#define SPI_SLAVES_PER_DEV          3 

#endif

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_SPI_STRUCTS
};

static struct server_info spi_conn[SPI_NOF_CONTROLLERS];
static bool initialized[SPI_NOF_CONTROLLERS];
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static uint32_t spi_client_ref[SPI_NOF_CONTROLLERS];

/* Fix me: in this file, the log should be replaced with united trace solution */

/**
 * locate the mail box.
 */
static int32_t get_mbox(itc_mbox_id_t *spi_mbox, uint32_t spid_index)
{
	char mailbox[sizeof(RHD_SPI_MAILBOX) + sizeof("-0xFFFFFFFF")];

	if (snprintf(mailbox, sizeof(mailbox), "%s_%d", RHD_SPI_MAILBOX,
	             spid_index) < 0) {
		TPT_ERROR("snprintf error");
		return INIT_OTHER_ERROR;
	}

	return xpai_locate_mbox(mailbox, spi_mbox);
}

/**
 * establish connection.
 */
int32_t xpai_spi_slave_init(uint32_t spid_index, uint32_t client_ref)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {SPI_SERVER_VERSIONS};
	SPI_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	int32_t ret = INIT_OK;
	int pthread_ret = 1;

	if (spid_index >= SPI_NOF_CONTROLLERS) {
		TPT_ERROR(STR("SPI slave index %u is not valid.\n",
		        spid_index));
		ret = INIT_OTHER_ERROR;
		goto spi_init_end;
	}

	if (initialized[spid_index]) {
		ret = INIT_OK;
		goto spi_init_end;
	}

	pthread_ret = pthread_mutex_lock(&mutex);
	if(pthread_ret) {
		TPT_ERROR(STR("(0x%x) pthread_mutex_lock failed with error %d.\n",
		          spid_index, pthread_ret));
		ret = INIT_OTHER_ERROR;
		goto spi_init_end;
	}

	spi_client_ref[spid_index] = client_ref;

	/*call connection establish*/
	ret = get_mbox(&spi_conn[spid_index].server_mbox, spid_index);
	if (ret != INIT_OK) {
		TPT_ERROR(STR("Mailbox error %u\n", spid_index));
		goto spi_init_end;
	}


	ret = conn_establish(
	              /*input parameters*/
	              spi_conn[spid_index].server_mbox,
	              ++procedure_ref,
	              spi_client_ref[spid_index],
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &spi_conn[spid_index].server_ref,
	              &spi_conn[spid_index].selected_version);

	if (ret != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Connection establish failed on SPI daemon %u \
                         (reason:0x%08x)", spid_index, ret));
		ret = INIT_SERVER_NOK;
		goto spi_init_end;
	}

	TPT_TRACE(1, STR("mailbox id for spi%d: %u\n \
                   server connection ref:%u\n \
                   selected version: %u\n",
	          spid_index,
	          spi_conn[spid_index].server_mbox,
	          spi_conn[spid_index].server_ref,
	          spi_conn[spid_index].selected_version));

	initialized[spid_index] = true;
	ret = INIT_OK;

spi_init_end:
	if (pthread_ret == 0) {
		pthread_ret = pthread_mutex_unlock(&mutex);
		if(pthread_ret) {
			TPT_ERROR(STR("(0x%x) pthread_mutex_unlock \
				 failed with error %d.\n",
			     spid_index, pthread_ret));
			ret = INIT_OTHER_ERROR;
		}
	}
	return ret;
}

int32_t xpai_spi_init(uint32_t client_ref)
{
	int i;
	int32_t res;

	/* For now we initialize all controllers defined by SPI_NOF_CONTROLLERS.
	 * Should probably be handled better so that only SPI controllers
	 * actually started by mama are initialized.
	 */
	for (i = 0; i < SPI_NOF_CONTROLLERS; i++) {
		res = xpai_spi_slave_init(i, client_ref);
		if (res != INIT_OK) {
			return res;
		}
	}

	return INIT_OK;
}

/**
 * configure the spi.
 */
uint32_t XPAI_ConfSPI(uint32_t slave,
                      struct XPAI_ConfigSPI_S *spi_config)
{
	union itc_msg *send_msg = NULL;
	union itc_msg *receive_msg = NULL;
	uint32_t rx_filter[] = {2, RHD_SPI_CFG_CFM,  RHD_SPI_CFG_REJ};
	int32_t ret = 0;
	uint32_t spid_index = 0;
	uint32_t slave_index = 0;

	if (slave >= XPAI_SPI_MAX_SLAVE_NUM) {
		TPT_ERROR(STR("SPI slave index %u is not valid", slave));
		return  XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}
	spid_index = slave / SPI_SLAVES_PER_DEV;
	slave_index = slave % SPI_SLAVES_PER_DEV;

	if (!initialized[spid_index]) {
		TPT_ERROR(STR("XPAI_ConfSPI: SPI %d is not initiated", spid_index));
		return XPAI_XSP_SPI_OTHER_ERROR;
	}

	if(spi_config == NULL) {
		TPT_ERROR("Illegal parameter value, spi_config=NULL");
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

	TPT_TRACE(1, STR("XPAI_ConfSPI("
	         "spid_index=0x%x, "
	         "clock polarity=0x%X, "
	         "clock phase=0x%X, "
	         "mode=0x%X, "
	         "LSBFirst=0x%X, "
	         "biDir=0x%X, "
	         "rate=0x%X, "
	         "idle=0x%X, "
	         "port=0x%X, "
	         "slave_select_polarity=0x%X, "
	         "bus_width=0x%X) ",
	         spid_index, spi_config->clockPolarity, spi_config->clockPhase,
	         spi_config->mode, spi_config->lsbFirst, spi_config->biDir,
	         spi_config->rate, spi_config->idle, spi_config->port,
	         spi_config->ssPolarity, spi_config->busWidth));

	/*
	 * Check parameters in spi_config
	 */

	if(spi_config->clockPolarity != XPAI_XSP_SPI_CLOCK_POLARITY_LOW &&
	    spi_config->clockPolarity != XPAI_XSP_SPI_CLOCK_POLARITY_HIGH) {
		TPT_ERROR(STR("Illegal parameter value, clockPolarity=%d",
		        spi_config->clockPolarity));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

	if(spi_config->clockPhase != XPAI_XSP_SPI_CLOCK_PHASE_LEADING_EDGE &&
	    spi_config->clockPhase != XPAI_XSP_SPI_CLOCK_PHASE_TRAILING_EDGE &&
	    spi_config->clockPhase != XPAI_XSP_SPI_CLOCK_PHASE_LEADING_WRITE_TRAILING_READ
	    &&
	    spi_config->clockPhase !=
	    XPAI_XSP_SPI_CLOCK_PHASE_TRAILING_WRITE_LEADING_READ) {
		TPT_ERROR(STR("Illegal parameter value, clockPhase=%d", spi_config->clockPhase));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}
#ifndef __MACHINE_ZYNQMP_MIMO
	if(spi_config->mode != XPAI_XSP_SPI_MODE_NO_AUTO &&
	    spi_config->mode != XPAI_XSP_SPI_MODE_AUTO &&
	    spi_config->mode != XPAI_XSP_SPI_MODE_STROBE) {
		TPT_ERROR(STR("Illegal parameter value, mode=%d", spi_config->mode));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}
	
	if(spi_config->biDir != XPAI_XSP_SPI_BIDIR_NORMAL &&
	    spi_config->biDir != XPAI_XSP_SPI_BIDIR_SWITCH) {
		TPT_ERROR(STR("Illegal parameter value, biDir=%d", spi_config->biDir));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

	if(spi_config->idle != XPAI_XSP_SPI_IDLE_ONE_HALF &&
	    spi_config->idle != XPAI_XSP_SPI_IDLE_TWO_HALF) {
		TPT_ERROR(STR("Illegal parameter value,idle=%d", spi_config->idle));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

	if(spi_config->port > XPAI_XSP_SPI_MAX_PORT_NUM) {
		TPT_ERROR(STR("Illegal parameter value, port=%d", spi_config->port));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}
#endif
	if(spi_config->lsbFirst != XPAI_XSP_SPI_MSB_FIRST &&
	    spi_config->lsbFirst != XPAI_XSP_SPI_LSB_FIRST) {
		TPT_ERROR(STR("Illegal parameter value, lsbFirst=%d", spi_config->lsbFirst));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

	if(spi_config->ssPolarity != XPAI_XSP_SPI_SSPOLARIY_LOW &&
	    spi_config->ssPolarity != XPAI_XSP_SPI_SSPOLARIY_HIGH) {
		TPT_ERROR(STR("Illegal parameter value, slave_select_polarity=%d",
		        spi_config->ssPolarity));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

	if(spi_config->busWidth != XPAI_XSP_SPI_BUS_WIDTH_3WIRE &&
	    spi_config->busWidth != XPAI_XSP_SPI_BUS_WIDTH_4WIRE) {
		TPT_ERROR(STR("Illegal parameter value, busWidth=%d", spi_config->busWidth));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

	send_msg = itc_alloc(sizeof(struct  spi_conf_req), RHD_SPI_CFG_REQ);
	send_msg->spi_config_req.connection_ref = spi_conn[spid_index].server_ref;
	send_msg->spi_config_req.slave = slave_index;
	memcpy(&send_msg->spi_config_req.spi_config, spi_config,
	       sizeof(struct XPAI_ConfigSPI_S));
	TPT_SEND_SIG(send_msg->msgno, spi_conn[spid_index].server_mbox, "CFG REQ");
	itc_send(&send_msg, spi_conn[spid_index].server_mbox, ITC_MY_MBOX);

	/* Wait for CONF Cfm/Rej */
	receive_msg = itc_receive(rx_filter, ITC_NO_TMO,
	                          spi_conn[spid_index].server_mbox);

	if(receive_msg->any_msg.connection_ref != spi_client_ref[spid_index]) {
		TPT_ERROR(STR("Server replied with invalid connection_ref; "
		        "expected 0x%08x, received 0x%08x\n",
		        spi_client_ref[spid_index], receive_msg->any_msg.connection_ref));
		itc_free(&receive_msg);
		return XPAI_XSP_SPI_OTHER_ERROR;
	}

	switch(receive_msg->msgno) {
	case RHD_SPI_CFG_CFM:
		TPT_REC_SIG(receive_msg->msgno,
				STR("RHD_SPI_CFG_CFM from: %d\n", spi_conn[spid_index].server_mbox));
		ret = XPAI_XSP_SPI_OK;
		break;
	case RHD_SPI_CFG_REJ:
		TPT_REC_SIG(receive_msg->msgno,
				STR("RHD_SPI_CFG_REJ(Error code: %d) from: %d\n",
		          receive_msg->spi_config_rej.error_code, spi_conn[spid_index].server_mbox));
		ret = receive_msg->spi_config_rej.error_code;
		break;

	}
	itc_free(&receive_msg);
	return ret;

}
/**
 * send or receive data from spi bus.
 */
uint32_t XPAI_SendReceiveSPI( uint32_t slave,
                              uint32_t command_mode, uint32_t *send_buf,
                              uint32_t length, uint32_t length_dir)
{
	union itc_msg *send_msg = NULL;
	union itc_msg *receive_msg = NULL;
	uint32_t rx_filter[] = {2, RHD_SPI_SENDRECEIVE_CFM,  RHD_SPI_SENDRECEIVE_REJ};
	int32_t ret = 0;
#ifndef __MACHINE_ZYNQMP_MIMO	
	uint32_t wr_len_in_words = 0;
#endif
	uint32_t spid_index = 0;
	uint32_t slave_index = 0;

	if (slave >= XPAI_SPI_MAX_SLAVE_NUM) {
		TPT_ERROR(STR("SPI slave index %u is not valid.\n", slave));
		return  XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}
	spid_index = slave / SPI_SLAVES_PER_DEV;
	slave_index = slave % SPI_SLAVES_PER_DEV;

	if (!initialized[spid_index]) {
		TPT_ERROR(STR("XPAI_SendReceiveSPI: SPI %d is not initiated.\n", spid_index));
		return XPAI_XSP_SPI_OTHER_ERROR;
	}

	if(command_mode != XPAI_XSP_SPI_CMD_MODE_WRITE &&
	    command_mode != XPAI_XSP_SPI_CMD_MODE_READ &&
	    command_mode != XPAI_XSP_SPI_CMD_MODE_BIDIR &&
	    command_mode != XPAI_XSP_SPI_CMD_MODE_DUPLEX) {
		TPT_ERROR(STR("Illegal parameter value, command_mode=%d", command_mode));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}
#ifndef __MACHINE_ZYNQMP_MIMO
	if(length < 1 || length > 256) {
		TPT_ERROR(STR("Illegal parameter value, length=%d", length));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}

#else
	if(length < 1 || length > 32) {
		TPT_ERROR(STR("Illegal parameter value, length=%d", length));
		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
	}
	send_msg = itc_alloc(sizeof(struct	spi_send_receive_req),
					 RHD_SPI_SENDRECEIVE_REQ);
#endif

#ifndef __MACHINE_ZYNQMP_MIMO
	if(command_mode == XPAI_XSP_SPI_CMD_MODE_BIDIR) {
		if(length_dir < 1 || length_dir > 256) {
			TPT_ERROR(STR("Illegal parameter value, length_dir=%d", length_dir));
			return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
		}

		if(length_dir >= length) {
			TPT_ERROR(STR("Invalid parameter values. LengthDir >= length (%d >= %d)",
			        length_dir, length));
			return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
		}
	}

	/* Get length to write. 'length' and 'length_dir' are in bits, convert to bytes and round up. */
	if(command_mode == XPAI_XSP_SPI_CMD_MODE_READ) {
		/* Nothing to write */
		wr_len_in_words = 0;
	} else if(command_mode == XPAI_XSP_SPI_CMD_MODE_BIDIR) {
		/* In BiDir Mode number of bits to write is 'length_dir' */
		wr_len_in_words = length_dir / 32;
		if(length_dir % 32) {
			/* Round up if necessary */
			wr_len_in_words++;
		}
	} else {
		/* In Write and Duplex Mode number of bits to write is 'length' */
		wr_len_in_words = length / 32;
		if(length % 32) {
			/* Round up if necessary */
			wr_len_in_words++;
		}
	}

	/* Send REQ */
	send_msg = itc_alloc(sizeof(struct  spi_send_receive_req) +
	                     (wr_len_in_words * 4),
	                     RHD_SPI_SENDRECEIVE_REQ);	
#endif

	/* Send REQ */

	send_msg->spi_send_receive_req.connection_ref =
	        spi_conn[spid_index].server_ref;
	send_msg->spi_send_receive_req.slave = slave_index;
	send_msg->spi_send_receive_req.command_mode = command_mode;
	send_msg->spi_send_receive_req.length = length;
	send_msg->spi_send_receive_req.lengthDir = length_dir;
#ifndef __MACHINE_ZYNQMP_MIMO
	if(wr_len_in_words > 0) {
		if(NULL == send_buf) {
			TPT_ERROR("Illegal parameter value, send_buf is a null pointer");
			itc_free(&send_msg);
			return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
		}
		memcpy(&send_msg->spi_send_receive_req.send_buffer[0], send_buf,
		       wr_len_in_words * 4);
	}
#else
    	if(NULL == send_buf) {
    		TPT_ERROR("Illegal parameter value, send_buf is a null pointer");
    		itc_free(&send_msg);
    		return XPAI_XSP_SPI_ILLEGAL_PARAMETER;
    	}
    	memcpy(&send_msg->spi_send_receive_req.send_buffer[0], send_buf,
    		   (length/8));

#endif

	TPT_SEND_SIG(send_msg->msgno, spi_conn[spid_index].server_mbox,
			"RHD_SPI_SENDRECEIVE_REQ");
	itc_send(&send_msg, spi_conn[spid_index].server_mbox, ITC_MY_MBOX);

	/* Receive Cfm/Rej */
	receive_msg = itc_receive(rx_filter, ITC_NO_TMO,
	                          spi_conn[spid_index].server_mbox);

	if(receive_msg->any_msg.connection_ref != spi_client_ref[spid_index]) {
		TPT_ERROR(STR("Server replied with invalid connection_ref; "
		        "expected 0x%08x, received 0x%08x",
		        spi_client_ref[spid_index], receive_msg->any_msg.connection_ref));
		itc_free(&receive_msg);
		return XPAI_XSP_SPI_OTHER_ERROR;
	}

	switch(receive_msg->msgno) {
	case RHD_SPI_SENDRECEIVE_CFM:
		TPT_REC_SIG(receive_msg->msgno, STR("RHD_SPI_SENDRECEIVE_CFM from: %d\n",
		          spi_conn[spid_index].server_mbox));
		ret = XPAI_XSP_SPI_OK;
		break;
	case RHD_SPI_SENDRECEIVE_REJ:
		TPT_REC_SIG(receive_msg->msgno,
				STR("RHD_SPI_SENDRECEIVE_REJ(Error code: %d) from: %d\n",
		          receive_msg->spi_send_receive_rej.error_code,
		          spi_conn[spid_index].server_mbox));
		ret = receive_msg->spi_send_receive_rej.error_code;
		break;

	}

	itc_free(&receive_msg);
	return ret;
}

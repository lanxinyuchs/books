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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <itc.h>
#include <uio_helper.h>
#include <pthread.h>
#include <conn-establish-helper.h>


#include "rhd-common.h"
#include "rhd-spid-if.h"
#ifndef __MACHINE_ZYNQMP_MIMO
#include "libspi.h"
#else
#include "libspi_mimo.h"
#endif


#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_spid
#include "tpt_create.h"
#include "tpt.h"

#define EXIT_SIGNAL             0xdeadbeef
#define MAX_MAILBOX_NUM         32

#define UIO_DEV_SPI             "spi"
#define DAEMON_NAME             "rhd-spid"

#define _STRINGIFY(s)           #s
#define STRINGIFY(s)            _STRINGIFY(s)

#define UIO_DEV_NAME_LEN   sizeof(UIO_DEV_SPI) + \
	sizeof(STRINGIFY(SPI_NOF_CONTROLLERS))           /*e.g "spi0"*/
#define MAILBOX_NAME_LEN   sizeof(RHD_SPI_MAILBOX) + \
	sizeof(STRINGIFY(SPI_NOF_CONTROLLERS)) + 1
#define DAEMON_NAME_LEN         sizeof(DAEMON_NAME) + \
	sizeof(STRINGIFY(SPI_NOF_CONTROLLERS)) + 1       /* e.g "rhd-spid-0" */

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_SPI_STRUCTS
};

/* Offset of SPI data in XPAI_SENDRECEIVE_SPI_IND signal. */
#define SPI_SENDRECEIVE_IND_DATA_OFFSET         offsetof(struct spi_send_receive_ind, buffer)

struct uio_info {
	UIO_HANDLE_ handle;
	void *mmap_base;
	uint32_t nof_users;
};

struct spid_context {
	struct spi_send_receive_ind *ind;
	struct uio_info uio;
#ifndef __MACHINE_ZYNQMP_MIMO	
	void *master;
#else
	pthread_mutex_t mutex;
#endif
	uint32_t spid_idx;
	
	struct {
		/* Non-zero if device is opened */
		bool is_opened;
		void *handle;

		struct {
			/*
			 * Values for clock phase.
			 * 0=Sampling on leading edges for read, write, duplex and bidir mode.
			 * 1=Sampling on traling edges for read, write, duplex and bidir mode.
			 * 2=Sampling on leading edges for write traling edge for read.
			 *   (value not allowed in duplex or bidir mode).
			 * 3=Sampling on traling edges for write leading edge for read
			 *   (value not allowed in duplex or bidir mode).
			 for ngr_lft_lp platform ,just 0 or 1
			 */
			uint32_t clock_phase;
			/*
			 * Slave Select mode.
			 * 0=No automatic slave select.
			 * 1=Automatic slave select.
			 * 2=Late positive slave select pulse. The time from last serial rising edge
			 *   of SPI clock to rising edge of pulse should be greater than 10 ns.
			 *   The pulse width should be greater than 30 ns
			 */
			uint32_t mode;
#ifdef __MACHINE_ZYNQMP_MIMO
			uint32_t clockPolarity;
			uint32_t lsbFirst;
			uint32_t rate;
			uint32_t ssPolarity;
			uint32_t busWidth;
#endif			
			
		} cfg;
	} slave[SPI_SLAVES_PER_DEV];
};
struct server_ref {
	uint32_t server_reference;
	LIST_ENTRY(server_ref) server_refs;      /* List. */
};

/*
 * Local Function Declaration
 */
static void handle_config_signal(struct spid_context *spi,
                                 union itc_msg *rx_msg, struct conn_client_info client_info);
static void handle_tx_rx_signal(struct spid_context *pDC,
                                union itc_msg *rx_msg, uint32_t client_ref);
static void read_messages(conn_server_handle_t handle);
#ifdef __MACHINE_ZYNQMP_MIMO
static int32_t spi_read(struct spid_context *pDC,uint32_t slave_index,uint32_t readCmd, uint32_t *data, uint32_t bitLength);
static int32_t spi_write(struct spid_context *pDC,uint32_t slave_index,uint32_t data,uint32_t bitLength);
#else
static void spid_int_notifier(void *data);
#endif
/*
 * Local Variable Definition
 */
static char *daemon_name;
static struct spid_context spid_context;
static itc_mbox_id_t spi_mbox = ITC_NO_ID;

LIST_HEAD(server_ref_head, server_ref) head =
        LIST_HEAD_INITIALIZER(head);
/*
 *  print usage
 */
static void print_usage(void)
{
	printf("Usage: rhd-spid [options] <spi controller>\n\n"
	       "Where <spi controller> is less than %d\n\n"
	       "Options:\n"
	       "    -h        Display usage information (this message).\n"
	       "    -d        Daemonize the program.\n\n", SPI_NOF_CONTROLLERS);
}


/**
 * To send configure confirm signal.
 */
static void send_config_cfm_sig(union itc_msg *rx_msg, uint32_t client_ref)
{
	union itc_msg *out_msg = NULL;

	out_msg = itc_alloc(sizeof(struct spi_conf_cfm), RHD_SPI_CFG_CFM);

	out_msg->any_msg.connection_ref = client_ref;
	out_msg->any_msg.procedure_ref = rx_msg->any_msg.procedure_ref;
	itc_send(&out_msg, itc_sender(rx_msg), ITC_MY_MBOX);

}
/**
 * To send configure reject signal.
 */
static void send_config_rej_sig(union itc_msg *rx_msg, uint32_t client_ref,
                                uint32_t err_code)
{
	union itc_msg *out_msg = NULL;

	out_msg = itc_alloc(sizeof(struct spi_conf_rej), RHD_SPI_CFG_REJ);

	if(out_msg != NULL) {
		out_msg->spi_config_rej.error_code = err_code;
	}
	out_msg->any_msg.connection_ref = client_ref;
	out_msg->any_msg.procedure_ref = rx_msg->any_msg.procedure_ref;
	itc_send(&out_msg, itc_sender(rx_msg), ITC_MY_MBOX);
}
/**
 * send tx/rx confirm signal
 */
static void send_tx_rx_cfm_sig(union itc_msg *rx_msg, uint32_t client_ref)
{
	union itc_msg *out_msg = NULL;

	out_msg = itc_alloc(sizeof(struct spi_send_receive_cfm),
	                    RHD_SPI_SENDRECEIVE_CFM);
	out_msg->any_msg.connection_ref = client_ref;
	out_msg->any_msg.procedure_ref = rx_msg->any_msg.procedure_ref;
	itc_send(&out_msg, itc_sender(rx_msg), ITC_MY_MBOX);

}

/**
 * Send tx/rx reject signal
 */
static void send_tx_rx_rej_sig(union itc_msg *rx_msg, uint32_t client_ref,
                               uint32_t err_code)
{
	union itc_msg *out_msg = NULL;

	out_msg = itc_alloc(sizeof(struct spi_send_receive_rej),
	                    RHD_SPI_SENDRECEIVE_REJ);
	if(out_msg != NULL) {
		out_msg->spi_send_receive_rej.error_code = err_code;
	}
	out_msg->any_msg.connection_ref = client_ref;
	out_msg->any_msg.procedure_ref = rx_msg->any_msg.procedure_ref;
	itc_send(&out_msg, itc_sender(rx_msg), ITC_MY_MBOX);

}

/**
 * Open a device
 */
 
#ifndef __MACHINE_ZYNQMP_MIMO
static int32_t open_device(struct spid_context *pDC, uint32_t slave)
{
	if(pDC == NULL) {
		return SPI_EINVALID_PARA;
	}

	if (!pDC->slave[slave].is_opened) {
		/* Open the device driver*/
		pDC->slave[slave].handle = spi_open(pDC->master, slave);
		if (pDC->slave[slave].handle == NULL) {
			TPT_ERROR(STR("spi_open returned error, slave = 0x%x",
			              slave));
			return SPI_EINVALID_PARA;
		}
		pDC->slave[slave].is_opened = true;
	}
	return SPI_SUCCESS;
}

/**
 * Map command to spi_phase according to the clock_phase
 */
static bool get_spi_ctrl_cpha_val(uint32_t command_mode, uint32_t clock_phase,
                                  spi_phase *spi_phase)
{
	/*
	 * clock_phase has four possible values:
	 * 0 = sampling on leading edges for read, write, duplex and bidir mode
	 * 1 = sampling on trailing edges for read, write, duplex and bidir mode
	 * 2 = sampling on leading edges for write and on trailing edges for read.
	 *     Not allowed in duplex and bidir.
	 * 3 = sampling on trailing edges for write and on leading edges for read.
	 *     Not allowed in duplex and bidir.
	 *
	 * SPI_CTRL_CPHA has two possible values:
	 * 0 = Sampling on leading edges
	 * 1 = Sampling on trailing edges
	 *
	 * command_mode (read, write, duplex or bidir) together with clock_phase
	 * decides what to write to SPI_CTRL_CPHA register.
	 */

	if(command_mode == SPI_CMD_MODE_WRITE) {
		/* Write mode */
		if(clock_phase == SPI_CLOCK_PHASE_LEADING_EDGE ||
		    clock_phase == SPI_CLOCK_PHASE_LEADING_WRITE_TRAILING_READ) {
			*spi_phase = SPI_PHASE_LEADING;
		} else {
			*spi_phase = SPI_PHASE_TRAILING;
		}
	} else if(command_mode == SPI_CMD_MODE_READ) {
		/* Read mode */
		if(clock_phase == SPI_CLOCK_PHASE_LEADING_EDGE ||
		    clock_phase ==
		    SPI_CLOCK_PHASE_TRAILING_WRITE_LEADING_READ) {
			*spi_phase = SPI_PHASE_LEADING;
		} else {
			*spi_phase = SPI_PHASE_TRAILING;
		}
	} else if(command_mode == SPI_CMD_MODE_BIDIR ||
	          command_mode == SPI_CMD_MODE_DUPLEX) {
		/* Bidir or Duplex mode*/
		if(clock_phase == SPI_CLOCK_PHASE_LEADING_EDGE) {
			*spi_phase = SPI_PHASE_LEADING;
		} else if(clock_phase == SPI_CLOCK_PHASE_TRAILING_EDGE) {
			*spi_phase = SPI_PHASE_TRAILING;
		} else {
			TPT_ERROR(STR("clock_phase (%d) is not allowed "
			              "in command_mode (%d)",
			              clock_phase, command_mode));
			return false;
		}
	}

	return true;
}

static int set_uio_map_irq(void)
{
	uint32_t spid_index;
	struct spid_context *pDC = &spid_context;
	char uio_dev_name[UIO_DEV_NAME_LEN] = {0};

	spid_index = pDC->spid_idx;

	snprintf(uio_dev_name, sizeof(uio_dev_name), "%s%d", UIO_DEV_SPI,
	         spid_index);
	pDC->uio.handle = uio_open(uio_dev_name);
	if (pDC->uio.handle == UIO_OPEN_FAILED) {
		TPT_ERROR("Failed to open uio");
		return -1;
	}

	pDC->uio.mmap_base = uio_mmap(pDC->uio.handle);
	if (pDC->uio.mmap_base == MAP_FAILED) {
		TPT_ERROR("Failed to peform UIO memory mapping");
		goto set_uio_map_irq_exit;
	}

	if (spi_init_regs(pDC->master, pDC->uio.mmap_base) != SPI_SUCCESS)
		goto set_uio_map_irq_exit;

	uio_disable_irq(pDC->uio.handle);

	/**
	 * Set the notifier for the interrupts.
	 */
	if (uio_irq_set_notifier(pDC->uio.handle, spid_int_notifier,
	                         pDC->master)) {
		TPT_ERROR("Failed to set UIO interrupt notifier");
		goto set_uio_map_irq_exit;
	}
	TPT_TRACE(1, "UIO interrupt notifier set");

	/* start interrupt handler */
	if (uio_bind_irq(pDC->uio.handle)) {
		TPT_ERROR("Unable to start UIO interrupt handler");
		goto set_uio_map_irq_exit;
	}
	TPT_TRACE(1, "UIO interrupt handler started");

	uio_enable_irq(pDC->uio.handle);

	return 0;

set_uio_map_irq_exit:
	uio_close(pDC->uio.handle);

	return -1;
}
#endif
static void remove_client_from_list(uint32_t server_reference,
                                    bool remove_all)
{
	struct server_ref *ref, *next_ref;
	struct spid_context *pDC = &spid_context;

	for (ref = LIST_FIRST(&head); ref != NULL; ref = next_ref) {
		next_ref = LIST_NEXT(ref, server_refs);
		if(remove_all == true || server_reference == ref->server_reference) {
			LIST_REMOVE(ref, server_refs);
			free(ref);

			pDC->uio.nof_users--;		
			if(pDC->uio.nof_users == 0) {
#ifndef __MACHINE_ZYNQMP_MIMO					
				uio_munmap (pDC->uio.handle);
				uio_close(pDC->uio.handle);
#endif				
				break;
			}
			
		}
	}

}

static bool is_server_ref_exist_in_list(uint32_t server_reference)
{
	struct server_ref *ref;

	LIST_FOREACH(ref, &head, server_refs) {
		if(server_reference == ref->server_reference) {
			return true;
		}
	}
	return false;
}

static bool add_server_ref_into_list(uint32_t server_refer,
                                     uint32_t *num_of_users)
{
	struct server_ref *ref;
	if(is_server_ref_exist_in_list(server_refer) == false) {
		ref = calloc(1, sizeof(struct server_ref));
		if(ref == NULL) {
			TPT_ERROR(STR("calloc ref failed %d",
			              sizeof(struct server_ref)));
			return false;
		}
		ref->server_reference = server_refer;
		LIST_INSERT_HEAD(&head, ref, server_refs);
		(*num_of_users)++;
	}
	return true;
}

/**
 * Handle open spi uio device
 */
 
#ifndef __MACHINE_ZYNQMP_MIMO
static int32_t open_uio_device(struct conn_client_info client_info)
{
	struct spid_context *pDC = &spid_context;

	if(pDC->uio.nof_users == 0) {
		if(set_uio_map_irq())
			return -1;
	}

	if(add_server_ref_into_list(client_info.server_ref,
	                            &pDC->uio.nof_users) == true)
		return 0;
	else
		return -1;

}
#endif
/**
 * Handle configure signal
 */
static void handle_config_signal(struct spid_context *pDC,
                                 union itc_msg *rx_msg, struct conn_client_info client_info)
{
	struct spi_config *spi_config = &rx_msg->spi_config_req.spi_config;
	uint32_t err_code = SPI_OK;
	uint32_t slave_index = 0;
#ifndef __MACHINE_ZYNQMP_MIMO 	
	uint32_t dev_err = SPI_SUCCESS;
	int32_t cfg[19] = {0};
#endif
	if(itc_size(rx_msg) != sizeof(struct spi_conf_req)) {
		TPT_ERROR(STR("Receive  message(0x%x) size(0x%x), "
		              "while the expected size is 0x%x",
		              rx_msg->msgno, itc_size(rx_msg),
		              sizeof(struct spi_conf_req)));
		err_code = SPI_OTHER_ERROR;
		goto handle_config_sig_exit;
	}
	slave_index = rx_msg->spi_config_req.slave;

	/* Treat Slave > 16 as error  */
	if(slave_index >= SPI_SLAVES_PER_DEV) {
		TPT_ERROR(STR("Invalid slave (%d)", slave_index));
		err_code = SPI_ILLEGAL_PARAMETER;
		goto handle_config_sig_exit;
	}

#ifdef __MACHINE_ZYNQMP_MIMO
    pDC->slave[slave_index].cfg.clockPolarity = (spi_config->clockPolarity) ?
	                                              SPI_POLARITY_HIGH : SPI_POLARITY_LOW;
	pDC->slave[slave_index].cfg.clock_phase = (spi_config->clockPhase)?
		                                       SPI_PHASE_TRAILING : SPI_PHASE_LEADING;
	pDC->slave[slave_index].cfg.lsbFirst = (spi_config->lsbFirst) ?
	                                         SPI_SEND_FIRST_LSB : SPI_SEND_FIRST_MSB;
	pDC->slave[slave_index].cfg.rate = spi_config->rate;
	pDC->slave[slave_index].cfg.busWidth = (spi_config->busWidth) ? SPI_WIRE_4 : SPI_WIRE_3;
	pDC->slave[slave_index].cfg.ssPolarity = (spi_config->ssPolarity) ?
	                                           SPI_SS_POL_HIGH : SPI_SS_POL_LOW;

	TPT_TRACE(3, STR("slave=%d,clockPolarity=%d,clock_phase=%d,lsbFirst=%d,rate=%d,busWidth=%d,ssPolarity=%d",
			 rx_msg->spi_config_req.slave, spi_config->clockPolarity,spi_config->clockPhase, spi_config->lsbFirst, spi_config->rate,spi_config->busWidth, spi_config->ssPolarity));
			 
	if(add_server_ref_into_list(client_info.server_ref,
                            &pDC->uio.nof_users) != true)
    	{
		err_code = SPI_OTHER_ERROR;
	}
	
    goto	handle_config_sig_exit;
#else	

	if(open_uio_device(client_info)) {
		TPT_ERROR("failed to open uio device");
		err_code = SPI_OTHER_ERROR;
		goto handle_config_sig_exit;
	}

	dev_err = open_device(pDC, slave_index);
	if (dev_err != SPI_SUCCESS) {
		err_code = SPI_OTHER_ERROR;
		goto handle_config_sig_exit;
	}

	cfg[0] = SPI_TAGC_POLARITY;
	cfg[1] = (spi_config->clockPolarity) ?
	         SPI_POLARITY_HIGH : SPI_POLARITY_LOW;
	cfg[2] = SPI_TAGC_SEND_FIRST;
	cfg[3] = (spi_config->lsbFirst) ?
	         SPI_SEND_FIRST_LSB : SPI_SEND_FIRST_MSB;
	cfg[4] = SPI_TAGC_SS_BIDIR;
	cfg[5] = (spi_config->biDir) ?
	         SPI_SS_BIDIR_SWITCH : SPI_SS_BIDIR_NORMAL;
	cfg[6] = SPI_TAGC_BITRATE;
	cfg[7] = SPI_CLK_IN_HZ / (2 * (spi_config->rate + 1));
	cfg[8] = SPI_TAGC_RX_DELAY;
	cfg[9] = (spi_config->idle) ?
	         SPI_RX_DELAY_ONE_CYCLE : SPI_RX_DELAY_HALF_CYCLE;
	cfg[10] = SPI_TAGC_PORT;
	cfg[11] = spi_config->port;
	cfg[12] = SPI_TAGC_SS_POL;
	cfg[13] = (spi_config->ssPolarity) ?
	          SPI_SS_POL_HIGH : SPI_SS_POL_LOW;
	cfg[14] = SPI_TAGC_WIRE;
	cfg[15] = (spi_config->busWidth) ? SPI_WIRE_4 : SPI_WIRE_3;
	cfg[16] = SPI_TAGC_INVERT_CLK;
	cfg[17] = SPI_INVERT_CLK_OFF;
	cfg[18] = SPI_TAGC_TAGEND;

	TPT_TRACE(3, STR("slave=%d, SPI_POLARITY=%d, "
			 "SPI_SEND_FIRST=%d,SPI_SS_BIDIR=%d,"
			 " SPI_BITRATE=%d, 0x%x, SPI_RX_DELAY=%d, SPI_PORT=%d,"
			 " SPI_SS_POL=%d, SPI_WIRE=%d, SPI_INVERT_CLK=%d",
			 rx_msg->spi_config_req.slave, cfg[1],
			 cfg[3], cfg[5], cfg[7],
			 spi_config->rate, cfg[9],
			 cfg[11], cfg[13], cfg[15], cfg[17]));

	dev_err = spi_set_conf(pDC->slave[slave_index].handle, cfg);
	if (dev_err != SPI_SUCCESS) {
		TPT_ERROR(STR("spi_set_conf returned error 0x%x", dev_err));
		err_code = SPI_OTHER_ERROR;
		goto handle_config_sig_exit;
	}

	/*
	 * Store config variables that are will be configured when
	 * XPP_XSP_SENDRECEIVE_SPI_REQ is executed.
	 */
	pDC->slave[slave_index].cfg.clock_phase = spi_config->clockPhase;
	pDC->slave[slave_index].cfg.mode = spi_config->mode;
#endif
handle_config_sig_exit:
	if (err_code == SPI_OK) {
		send_config_cfm_sig(rx_msg, client_info.client_ref);
	} else {
		send_config_rej_sig(rx_msg, client_info.client_ref, err_code);
	}

}

#ifdef __MACHINE_ZYNQMP_MIMO 
static int32_t spi_write(struct spid_context *pDC,uint32_t slave_index,uint32_t data,uint32_t bitLength)
{
	uint8_t tx_buf[4] = {0x00, 0x00, 0x00, 0x00};
	uint32_t i= 0;
	int ret;
	void *handle = NULL;
	    
	if((bitLength%8) != 0)
	{		
		TPT_ERROR(STR("Failed to send data, length %d erro", bitLength));
		return false;
	}
	else
	{
		for(i= 0 ; i < (bitLength/8) ; i++)
		{
		//this convert because arm ngr use little endian
		//from the lsb package the data
		if (pDC->slave[slave_index].cfg.lsbFirst)
			tx_buf[i] = (uint8_t)((data>>(i*8)) & 0xff);
        else
			tx_buf[i] = (uint8_t)((data>>(bitLength - (i + 1)*8)) & 0xff);	
		}
	}
	struct lib_spi_config cfg;

	//this is fixed 8bit, spi controller transfer min unit
	//todo,check this parameter
	cfg.bits_per_word = 8;
	//need set mode follow this :
	//define SPI_MODE_MASK		(SPI_CPHA | SPI_CPOL | SPI_CS_HIGH 
	//			| SPI_LSB_FIRST | SPI_3WIRE | SPI_LOOP 
	//			| SPI_NO_CS | SPI_READY)
	cfg.mode = (pDC->slave[slave_index].cfg.clock_phase<<0) | (pDC->slave[slave_index].cfg.clockPolarity<<1) | \
	       (pDC->slave[slave_index].cfg.ssPolarity<<2) | \
	       ((1 - pDC->slave[slave_index].cfg.busWidth)<<4);
	cfg.speed_hz = pDC->slave[slave_index].cfg.rate;

    	handle = libspi_config(pDC->spid_idx, slave_index, &cfg);
	if (NULL == handle)
	{
		TPT_ERROR(STR("configure err,bus num=%d,cs_num=%d,mode=%d,speed=%d", pDC->spid_idx,slave_index,cfg.mode,cfg.speed_hz));
		return false;
	}

	struct spi_ioc_transfer xmit[1] =
	{
		{
			/*
			* According to https://www.kernel.org/doc/Documentation/spi/spidev:
			*
			*  FULL DUPLEX CHARACTER DEVICE API
			*  ================================
			*  ...
			*  To make a full duplex request, provide both rx_buf and tx_buf for
			*  the same transfer.  It's even OK if those are the same buffer.
			* ...
			*/
			.tx_buf = (unsigned long) tx_buf, .rx_buf = (unsigned long) tx_buf, /* share buffer */
			.len = (bitLength/8), .speed_hz = cfg.speed_hz,
			.delay_usecs = 0,
			.bits_per_word = cfg.bits_per_word,
			.cs_change = 0,
		},
	};
		
	ret = libspi_transfer(handle, 1, xmit);
    	libspi_release_slave (handle);
	if (ret)
	{		
		TPT_ERROR(STR("slave=%d, value=%d, lenth=%d, error id=%d",slave_index,data,bitLength,ret));
		return false;
	}
	return true;
}

static int32_t spi_read(struct spid_context *pDC,uint32_t slave_index,uint32_t readCmd, uint32_t *data, uint32_t bitLength)
{
	uint8_t tx_buf[4] = {0x00, 0x00, 0x00, 0x00};
	uint8_t rx_buf[4] = {0x00, 0x00, 0x00, 0x00};
	uint32_t recive_data;
	uint32_t i= 0;
	int32_t ret = 0;
	void *handle = NULL;

	*data = 0;
	
	if((bitLength%8) != 0)
	{		
		TPT_ERROR(STR("Failed to send data, length %d erro", bitLength));
		return false;
	}
	else
	{
		for(i= 0 ; i < (bitLength/8) ; i++)
		{
			if (pDC->slave[slave_index].cfg.lsbFirst)
			{		
			//maybe not convert becuase not find lsb or msb mode in zynq manual?
			//this convert because arm ngr use little endian
			//from the lsb package the data
				tx_buf[i] = (uint8_t)((readCmd>>(i*8)) & 0xff);
			}
			else
			{
				tx_buf[i] = (uint8_t)((readCmd>>(bitLength - (i + 1)*8)) & 0xff);				
			}
		}
	}
    
	struct lib_spi_config cfg;
	//this is fixed 8bit, spi controller transfer min unit
	cfg.bits_per_word = 8;
	//need set mode follow this :
	//define SPI_MODE_MASK 	(SPI_CPHA | SPI_CPOL | SPI_CS_HIGH 
	//			| SPI_LSB_FIRST | SPI_3WIRE | SPI_LOOP 
	//			| SPI_NO_CS | SPI_READY)
	cfg.mode = (pDC->slave[slave_index].cfg.clock_phase<<0) | (pDC->slave[slave_index].cfg.clockPolarity<<1) | \
			(pDC->slave[slave_index].cfg.ssPolarity<<2) | (pDC->slave[slave_index].cfg.lsbFirst<<3) | \
			((1 - pDC->slave[slave_index].cfg.busWidth)<<4);
	cfg.speed_hz = pDC->slave[slave_index].cfg.rate;
    
    	handle = libspi_config(pDC->spid_idx, slave_index, &cfg);
	
	if (NULL == handle)
	{
		TPT_ERROR(STR("configure err,bus num=%d,cs_num=%d,mode=%d,speed=%d", pDC->spid_idx,slave_index,cfg.mode,cfg.speed_hz));
		return false;
	}

	struct spi_ioc_transfer xmit[1] =
	{
		{
			/*
			 * According to https://www.kernel.org/doc/Documentation/spi/spidev:
			 *
			 *	FULL DUPLEX CHARACTER DEVICE API
			 *	================================
			 */
			.tx_buf = (unsigned long) tx_buf,
			.rx_buf = (unsigned long) rx_buf, /* share buffer */
			.len = (bitLength/8),
			.speed_hz = cfg.speed_hz,
			.delay_usecs = 0,
			.bits_per_word = cfg.bits_per_word,
			.cs_change = 0,
		},
	};

	ret = libspi_transfer(handle, 1, xmit);
	libspi_release_slave (handle);
		
	if (ret)
	{
		TPT_ERROR(STR("slave=%d, value=%d, lenth=%d, error id=%d",slave_index,readCmd,bitLength,ret));
		return false;
	}

	else
	{
		for(i= 0 ; i < (bitLength/8) ; i++)
		{
			//this convert because arm ngr use little endian
			//from the lsb package the data
			//maybe not convert becuase not find lsb or msb mode in zynq manual?
			recive_data = (uint32_t)rx_buf[i];
			//todo,yan this revert maybe change
			if (pDC->slave[slave_index].cfg.lsbFirst)
			{
				*data |= (recive_data<<i*8);
			}
			else
			{
				*data |= (recive_data<<(bitLength - (i + 1)*8));
			}
		}

	}
	return true;
}
#endif
/**
 * Handle tx/rx signal
 */
static void handle_tx_rx_signal(struct spid_context *pDC,
                                union itc_msg *rx_msg, uint32_t client_ref)
{
	struct spi_send_receive_req *tx_rx_req = &rx_msg->spi_send_receive_req;
	uint32_t err_code = SPI_OK;
	uint32_t slave_index = 0;
#ifndef __MACHINE_ZYNQMP_MIMO
	static int32_t spi_ss_high[] = {SPI_TAGC_SS, SPI_SS_HIGH};
	static int32_t spi_ss_low[] = {SPI_TAGC_SS, SPI_SS_LOW};
	static int32_t spi_tx_end = SPI_TAGC_TAGEND;
	uint32_t count = 0, ind_length_in_bits = 0;
	spi_phase spi_phase = {0};
	struct spi_buffer_des vector[5] = {{0, 0}};
	uint32_t dev_err = SPI_SUCCESS;
	int32_t cfg[5] = {0};
	struct spi_buffer_des wr_vector[2] = {{0, 0}};
	struct spi_buffer_des rd_vector[2] = {{0, 0}};

	struct {
		int32_t tag;
		struct spi_xfer header;
	} command;
#endif 
	if(itc_size(rx_msg) < sizeof(struct spi_send_receive_req)) {
		TPT_ERROR(STR("Receive message(0x%x) size(0x%x) while \
			       the minimum size is 0x%x",
		              rx_msg->msgno,
		              itc_size(rx_msg),
		              sizeof(struct spi_send_receive_req)));
		err_code = SPI_OTHER_ERROR;
		goto handle_tx_rx_sig_exit;
	}
	slave_index = tx_rx_req->slave;

	/* Treat Slave > 16 as error  */
	if(slave_index >= SPI_SLAVES_PER_DEV) {
		TPT_ERROR(STR("Invalid slave (%d)", slave_index));
		err_code = SPI_ILLEGAL_PARAMETER;
		goto handle_tx_rx_sig_exit;
	}

	pDC->ind = NULL;

#ifdef __MACHINE_ZYNQMP_MIMO
	int pthread_ret;
	uint32_t ind_length_in_bits = 0;
	switch (tx_rx_req->command_mode) {
	case SPI_CMD_MODE_WRITE:
		pthread_ret = pthread_mutex_lock(&pDC->mutex);
		if(pthread_ret != 0) {
			TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
						pthread_ret));
			abort();
		}
		TPT_TRACE(2, STR("slave=%d, value=%d, lenth=%d",tx_rx_req->slave, tx_rx_req->send_buffer[0], tx_rx_req->length));

		if(spi_write(pDC, slave_index, tx_rx_req->send_buffer[0], tx_rx_req->length) != true)
		{
			err_code = SPI_OTHER_ERROR;
			TPT_ERROR(STR("Spi failed to write data"));
		}

		pthread_ret = pthread_mutex_unlock(&pDC->mutex);

		if(pthread_ret != 0) {
			TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
						pthread_ret));
			abort();
		}			
		break;

	case SPI_CMD_MODE_READ:				
		ind_length_in_bits = tx_rx_req->length;
		pDC->ind = (struct spi_send_receive_ind *)itc_alloc(
			SPI_SENDRECEIVE_IND_DATA_OFFSET + (((ind_length_in_bits + 31) & ~31) >> 3),
			RHD_SPI_SENDRECEIVE_IND);
		pDC->ind->length = ind_length_in_bits;

		pthread_ret = pthread_mutex_lock(&pDC->mutex);
		if(pthread_ret != 0) {
			TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
						pthread_ret));
			abort();
		}	
		if(spi_read(pDC,slave_index,tx_rx_req->send_buffer[0],&pDC->ind->buffer[0],tx_rx_req->length) != true)
		{  
			err_code = SPI_OTHER_ERROR;
			TPT_ERROR(STR("Spi failed to read data"));
		}

		pthread_ret = pthread_mutex_unlock(&pDC->mutex);

		if(pthread_ret != 0) {
			TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
						pthread_ret));
			abort();
		}

		TPT_TRACE(2, STR("read,slave=%d, readcmd=%d,value=%d,lenth=%d",
					tx_rx_req->slave, tx_rx_req->send_buffer[0],pDC->ind->buffer[0], tx_rx_req->length));

		break;

	default:
		TPT_ERROR(STR("Invalid command mode %d",
					tx_rx_req->command_mode));
		err_code = SPI_ILLEGAL_PARAMETER;
	}
	if ( (pDC->ind == NULL) && (err_code == SPI_OK) ) {
		pDC->ind = (struct spi_send_receive_ind *)itc_alloc(
                           SPI_SENDRECEIVE_IND_DATA_OFFSET +
                           (((ind_length_in_bits + 31) & ~31) >> 3),
                           RHD_SPI_SENDRECEIVE_IND);
		pDC->ind->length = ind_length_in_bits;
	}

	goto handle_tx_rx_sig_exit;

#else	

	if (!get_spi_ctrl_cpha_val(tx_rx_req->command_mode,
	                           pDC->slave[slave_index].cfg.clock_phase,
	                           &spi_phase)) {
		err_code = SPI_ILLEGAL_PARAMETER;
		goto handle_tx_rx_sig_exit;
	}

	/* Bidir mode is incompatible with manual slave select mode */
	if((tx_rx_req->command_mode == SPI_CMD_MODE_BIDIR) &&
	    pDC->slave[slave_index].cfg.mode != SPI_MODE_AUTO) {
		TPT_ERROR("Bidir mode is incompatible with "
		          "manual slave select");
		err_code = SPI_ILLEGAL_PARAMETER;
		goto handle_tx_rx_sig_exit;
	}

	command.tag = SPI_TAGC_TRANSFER;
	command.header.length_in_bits = tx_rx_req->length;
	command.header.length_bidir_in_bits = tx_rx_req->lengthDir;

	count = 0;
	vector[count].base_addr = &command;
	vector[count].len = sizeof(command);
	count++;

	ind_length_in_bits = 0;
	switch (tx_rx_req->command_mode) {
	case SPI_CMD_MODE_WRITE:
		command.header.mode = SPI_MODE_WRITE;
		wr_vector[0].base_addr = tx_rx_req->send_buffer;
		wr_vector[0].len =
		        ((command.header.length_in_bits + 7) & ~7) >> 3;
		command.header.wr_vector = wr_vector;
		command.header.wr_vector_count = 1;
		command.header.rd_vector = NULL;
		command.header.rd_vector_count = 0;
		break;
	case SPI_CMD_MODE_READ:
		command.header.wr_vector = NULL;
		command.header.wr_vector_count = 0;
		command.header.mode = SPI_MODE_READ;
		ind_length_in_bits = command.header.length_in_bits;
		break;
	case SPI_CMD_MODE_BIDIR:
		command.header.mode = SPI_MODE_BIDIR;
		ind_length_in_bits = command.header.length_in_bits -
		                     command.header.length_bidir_in_bits;
		wr_vector[0].base_addr = tx_rx_req->send_buffer;
		wr_vector[0].len =
		        ((command.header.length_bidir_in_bits + 7) & ~7) >> 3;
		command.header.wr_vector = wr_vector;
		command.header.wr_vector_count = 1;
		break;
	case SPI_CMD_MODE_DUPLEX:
		command.header.mode = SPI_MODE_DUPLEX;
		ind_length_in_bits = command.header.length_in_bits;
		wr_vector[0].base_addr = tx_rx_req->send_buffer;
		wr_vector[0].len =
		        ((command.header.length_in_bits + 7) & ~7) >> 3;
		command.header.wr_vector = wr_vector;
		command.header.wr_vector_count = 1;
		break;
	default:
		TPT_ERROR(STR("Invalid command mode %d",
		              tx_rx_req->command_mode));
		err_code = SPI_ILLEGAL_PARAMETER;
		goto handle_tx_rx_sig_exit;
		break;
	}
	pDC->ind = (struct spi_send_receive_ind *)itc_alloc(
	                   SPI_SENDRECEIVE_IND_DATA_OFFSET +
	                   (((ind_length_in_bits + 31) & ~31) >> 3),
	                   RHD_SPI_SENDRECEIVE_IND);
	pDC->ind->length = ind_length_in_bits;

	if(ind_length_in_bits) {
		rd_vector[0].base_addr =
		        (void *)((intptr_t)pDC->ind +
		                 SPI_SENDRECEIVE_IND_DATA_OFFSET);
		rd_vector[0].len = ((ind_length_in_bits + 31) & ~31) >> 3;
		command.header.rd_vector = rd_vector;
		command.header.rd_vector_count = 1;
	}

	if(pDC->slave[slave_index].cfg.mode == SPI_MODE_STROBE) {
		/* generate SS strobe */
		vector[count].base_addr = &spi_ss_high;
		vector[count].len = sizeof(spi_ss_high);
		count++;
		vector[count].base_addr = &spi_ss_low;
		vector[count].len = sizeof(spi_ss_low);
		count++;
		TPT_TRACE(2, STR("Write: slave=%d, mode=%d, %d, "
		                 "length=%d, lengthDir=%d, ss=%d, %d",
		                 tx_rx_req->slave, command.header.mode,
		                 tx_rx_req->command_mode,
		                 command.header.length_in_bits,
		                 command.header.length_bidir_in_bits,
		                 spi_ss_high[1], spi_ss_low[1]));
	} else {
		TPT_TRACE(2, STR("Write: slave=%d, mode=%d, %d,"
		                 " length=%d, lengthDir=%d",
		                 tx_rx_req->slave, command.header.mode,
		                 tx_rx_req->command_mode,
		                 command.header.length_in_bits,
		                 command.header.length_bidir_in_bits));
	}

	vector[count].base_addr = (void *) &spi_tx_end;
	vector[count].len = sizeof(spi_tx_end);
	count++;

	cfg[0] = SPI_TAGC_PHASE;
	cfg[1] = spi_phase;
	cfg[2] = SPI_TAGC_AUTO_SS;
	cfg[3] = (pDC->slave[slave_index].cfg.mode != SPI_MODE_AUTO) ?
	         SPI_AUTO_SS_OFF : SPI_AUTO_SS_ON;
	cfg[4] = SPI_TAGC_TAGEND;

	TPT_TRACE(2, STR("slave=%d, SPI_PHASE=%d, %d, %d, SPI_AUTO_SS=%d",
	                 tx_rx_req->slave, cfg[1], tx_rx_req->command_mode,
	                 pDC->slave[slave_index].cfg.clock_phase, cfg[3]));

	dev_err = spi_set_conf(pDC->slave[slave_index].handle, cfg);
	if (dev_err != SPI_SUCCESS) {
		TPT_ERROR(STR("spi_set_conf returned error 0x%x", dev_err));
		err_code = SPI_OTHER_ERROR;
		goto handle_tx_rx_sig_exit;
	}

	/*
	  FIXME:
	  Should replace TRACE_BUS_SEND with suitable funtion
	 */
	/*
	  if (sendRecReq->commandMode != SPI_CMD_MODE_READ)
	  {
	          TRACE_BUS_SEND(STR("slave=%d", sendRecReq->slave),
	                         vector[1].base_addr, (U16) vector[1].len);
	  }
	*/
	dev_err = spi_transfer(pDC->slave[slave_index].handle, vector, count);

	if (dev_err != SPI_SUCCESS) {
		TPT_ERROR(STR("spi_transfer returned error 0x%x", dev_err));
		err_code = SPI_OTHER_ERROR;
	}
#endif
handle_tx_rx_sig_exit:
	if (err_code == SPI_OK) {
		send_tx_rx_cfm_sig(rx_msg, client_ref);

		if (tx_rx_req->command_mode != SPI_CMD_MODE_WRITE) {
			/*
			  FIXME:
			  Should replace TRACE_BUS_RECEIVE with suitable funtion
			 */
			/*
			TRACE_BUS_RECEIVE(STR("slave=%d, length=%d", sendRecReq->slave,
			                      pDC->ind->length),
			                  (uint8_t *) pDC->ind +
			                  XPAI_SENDRECEIVE_SPI_IND_DATA_OFFSET,
			                  ((pDC->ind->length + 31) & ~31) >> 3);
			*/
		}
		//this sig is used to app spi interface
		itc_send((union itc_msg **)&pDC->ind,
		         itc_sender(rx_msg),
		         ITC_MY_MBOX);
	} else {
		send_tx_rx_rej_sig(rx_msg, client_ref, err_code);
		if (pDC->ind != NULL) {
			itc_free((union itc_msg **) &pDC->ind);
		}
	}
}
/**
 * Initialize spi_controller
 */
static int spid_init(uint32_t spid_index)
{
	struct spid_context *pDC = &spid_context;
	char mail_box_name[MAILBOX_NAME_LEN] = {0};

	/* Initialize ITC */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if (snprintf(mail_box_name,
	             sizeof(mail_box_name),
	             "%s_%d",
	             RHD_SPI_MAILBOX,
	             spid_index) < 0) {
		TPT_ERROR(STR("snprintf mailbox %s_%d error",
		              RHD_SPI_MAILBOX, spid_index));
		goto init_error;
	}
	/* Create our mailbox. */
	spi_mbox = itc_create_mailbox(mail_box_name, 0);
	if (spi_mbox == ITC_NO_ID)
		goto init_error;

	LIST_INIT(&head);
#ifndef __MACHINE_ZYNQMP_MIMO
	pDC->master = spi_init();
	if(pDC->master == NULL) {
		TPT_ERROR("spi_init - Memory allocation failed");
		goto init_error;
	}
#else
pthread_mutex_init(&pDC->mutex, NULL);

#endif
	pDC->spid_idx = spid_index;

	return 0;
init_error:
#ifndef __MACHINE_ZYNQMP_MIMO	
	if(pDC->master != NULL) {
		free(pDC->master);
		pDC->master = NULL;
	}
#endif	
	if(spi_mbox != ITC_NO_ID) {
		itc_delete_mailbox(spi_mbox);
	}
	return -EFAULT;
}
/**
 * The interrupt handler for spi.
 */
 
#ifndef __MACHINE_ZYNQMP_MIMO
static void spid_int_notifier(void *data)
{
	spi_int_handler(data);
	uio_enable_irq(spid_context.uio.handle);
}
#endif
/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{
	TPT_TRACE(2, STR("Client with mailbox 0x%08x, server_ref 0x%08x and "
	                 "client_ref 0x%08x has %s ",
	                 client_info->connected_mailbox,
	                 client_info->server_ref,
	                 client_info->client_ref,
	                 client_info->state ==
	                 CONN_ESTABLISH_STATUS_DISCONNECTING ?
	                 "disconnected." : "died (or forgot to disconnect.)"));
	remove_client_from_list(client_info->server_ref, false);

}

/**
 * Function conn_server_init
 */
static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	SPI_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	uint32_t supported_versions[] = {SPI_SERVER_VERSIONS};
	struct conn_event_callbacks cb = { NULL, client_disconnect,
		       client_disconnect, NULL
	};

	int conn_result = conn_establish_server_init( &handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &conn_messages, 0, &cb);
	if( conn_result != CONN_INIT_OK) {
		TPT_ERROR("Initalization of conn_establish"
		          " mechanism failed.");
		return NULL;
	}

	return handle;
}

/**
 * Main function to start the spi daemon.
 */
static void read_messages(conn_server_handle_t handle)
{
	union itc_msg *msg = NULL;
	itc_mbox_id_t mail_box;
	struct spid_context *pDC = &spid_context;
	struct conn_client_info client_info;

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		if (msg->msgno == EXIT_SIGNAL) {
			TPT_INFO(STR("%s exiting as ordered", daemon_name));
			itc_free(&msg);
			return;
		}

		/*Handle CONN_ESTABLISH... messages (and messages from
		 unknown clients.*/
		if(!conn_check_client(handle, &msg, &client_info)) {
			continue;
		}

		/* Store mail_box */
		mail_box = itc_sender(msg);

		switch (msg->msgno) {
		case RHD_SPI_CFG_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_SPI_CFG_REQ, sender: %d",
			                mail_box));
			handle_config_signal(pDC, msg, client_info);
			break;
		}
		case RHD_SPI_SENDRECEIVE_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_SPI_SENDRECEIVE_REQ,"
			                " sender: %d",
			                mail_box));
			handle_tx_rx_signal(pDC, msg, client_info.client_ref);
			break;
		}
		default:
			TPT_ERROR(STR("Receive Unexpected message: %d",
			              msg->msgno));
			break;
		}
		itc_free(&msg);

	}

}
/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, spi_mbox, ITC_MY_MBOX);
}
/**
 * Main function to start the spi daemon.
 */
int main(int argc, char **argv)
{
	static char short_options[] = "hd";
	int daemonize = 0;
	uint32_t spid_index = 0;
	int32_t ret = 0;
	void *handle = NULL;
	char *endptr;
	int c;

	while ((c = getopt(argc, argv, short_options)) != -1) {
		switch (c) {
		case 'h':
			print_usage();
			goto main_end;
		case 'd':
			daemonize = 1;
			break;
		default:
			print_usage();
			ret = -EINVAL;
			goto main_end;
		}
	}

	if (argc - optind != 1) {
		printf("No controller supplied to rhd-spid\n");
		print_usage();
		ret = -EINVAL;
		goto main_end;
	}

	spid_index = strtol(argv[argc - 1], &endptr, 10);
	if (*endptr != '\0' || spid_index >= SPI_NOF_CONTROLLERS) {
		printf("Invalid spi controller: %s\n", argv[argc - 1]);
		print_usage();
		ret = -EINVAL;
		goto main_end;
	}

	daemon_name = malloc(DAEMON_NAME_LEN);
	if (!daemon_name) {
		printf("malloc: failed\n");
		ret = -ENOMEM;
		goto main_end;
	}

	snprintf(daemon_name, DAEMON_NAME_LEN, "%s-%d",
	         DAEMON_NAME, spid_index);
	if (rhd_try_lock(daemon_name)) {
		printf("failed to obtain lock: %s\n", daemon_name);
		ret = -EFAULT;
		goto main_end;
	}

	if (!daemonize || !daemon(0, 0)) {

		TPT_INFO(STR("Starting %s %s",
		             daemonize ? "daemon" : "foreground process",
		             daemon_name));
		handle = conn_server_init();

		if (!spid_init(spid_index) && (handle != NULL)) {

			if (signal(SIGTERM, exit_handler) == SIG_ERR) {
				TPT_ERROR("Failed to install"
				          " signal exit handler");
				exit(1);
			}
			/* Start processing ITC messages.
			 * No return.
			 */
			read_messages(handle);

		} else {
			TPT_ERROR(STR("Failed to intialize spi%d", spid_index));
			ret = -EFAULT;
			goto main_end;
		}
	} else {
		TPT_ERROR(STR("Failed to start daemon %s", daemon_name));
		ret = -EFAULT;
	}

main_end:
	if(daemon_name != NULL) {
		free(daemon_name);
	}

	return ret;
}

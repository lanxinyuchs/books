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
#ifndef __LIB_SPI_H
#define __LIB_SPI_H

#ifdef __cplusplus
extern "C" {
#endif

/*FIXME
  board parameters will be used to get this clk value.
*/
#define SPI_CLK_IN_HZ              245720000
#define SPI_SLAVES_PER_DEV         16  /* The actual number slaves per device     */

#define SPI_EFATAL_MASK            0x80000000
#define SPI_ECODE_MASK             0x0000ffff /* Main error code mask            */
#define SPI_ESUBCODE_MASK          0x7fff0000 /* Error subcode mask              */
#define SPI_ESUB_UNKNOWN_INT       0x00010000
#define SPI_ESUB_RX_OR             0x00020000 /* RX overrun occurred.  */
#define SPI_ESUB_TX_UR             0x00040000 /* TX underrun occurred. */
/**
 * @macro SPI_SUCCESS
 * @brief Operation was performed successfully.
 */
#define SPI_SUCCESS                0
/**
 * @macro SPI_EINVALID_PARA
 * @brief Invalid parameter supplied.
 */
#define SPI_EINVALID_PARA          1

/**
 * @macro SPI_EOTHER
 * @brief Operation failed due to miscellaneous error.
 */
#define SPI_EOTHER                 2

/**
 * @macro SPI_EWRONG_STATE
 * @brief Operation was requested in wrong state.
 */
#define SPI_EWRONG_STATE           3

#define SPI_TAGC_BITRATE           0
#define SPI_TAGC_PHASE             1
#define SPI_TAGC_POLARITY          2
#define SPI_TAGC_AUTO_SS           3
#define SPI_TAGC_SEND_FIRST        4
#define SPI_TAGC_INVERT_CLK        5
#define SPI_TAGC_RX_DELAY          6
#define SPI_TAGC_SS                7
#define SPI_TAGC_SS_BIDIR          8
#define SPI_TAGC_PORT              9
#define SPI_TAGC_WIRE              10
#define SPI_TAGC_SS_POL            11
#define SPI_TAGC_TRANSFER          12
#define SPI_TAGC_TAGEND            -1L /*This tag signifies the end of a tag list.*/

/* Data type for SPI_TAGC_RX_DELAY (SPI_CTRL CLK_IDLE). */
typedef enum {
	/* 0 One half cycle */
	SPI_RX_DELAY_HALF_CYCLE  = 0,
	/* 1 One cycle */
	SPI_RX_DELAY_ONE_CYCLE = 1
} spi_rx_delay;

/* Data type for SPI_TAGC_AUTO_SS (SPI_CTRL_SS_MODE). */
typedef enum {
	/* No automatic Slave Select generation. */
	SPI_AUTO_SS_OFF  = 0,
	/* Automatic Slave Select generation. */
	SPI_AUTO_SS_ON = 1
} spi_auto_slave_select;

/* Data type for SPI_TAGC_SEND_FIRST (SPI_CTRL_LSBFE). */
typedef enum {
	/* Send each 32 bits word with MSB first. */
	SPI_SEND_FIRST_MSB = 0,
	/* Send each 32 bits word with LSB first. */
	SPI_SEND_FIRST_LSB = 1
} spi_send_first;


/* Data type for SPI_TAGC_PHASE (SPI_CTRL_CPHA). */
typedef enum {
	/* Sampling on leading edges. */
	SPI_PHASE_LEADING  = 0,
	/* Sampling on trailing edges. */
	SPI_PHASE_TRAILING = 1
} spi_phase;

/* Data type for SPI_TAGC_POLARITY (SPI_CTRL_CPOL). */
typedef enum {
	/* In idle state spi_m_clk is low. */
	SPI_POLARITY_LOW  = 0,
	/* In idle state spi_m_clk is high. */
	SPI_POLARITY_HIGH = 1
} spi_polarity;

/* Data type for SPI_TAGC_INVERT_CLK (SPI_HUB CLK_INV). */
typedef enum {
	/* DO not invert SPI clock */
	SPI_INVERT_CLK_OFF  = 0,
	/* Invert SPI clock */
	SPI_INVERT_CLK_ON = 1
} spi_invert_clock;

/* Data type for SPI_TAGC_SS (SPI_CTRL SS). */
typedef enum {
	/* 0 Slave Select is inactive (low). */
	SPI_SS_LOW  = 0,
	/* 1 Slave Select is active (high). */
	SPI_SS_HIGH = 1
} spi_slave_select;

/* Data type for SPI_TAGC_SS_BIDIR (SPI_CTRL SS_BIDIR). */
typedef enum {
	/* 0 Normal mode. Slave select active during whole SPI transfer. */
	SPI_SS_BIDIR_NORMAL = 0,
	/* 1 Switch mode. Slave select inactivates at direction switch. */
	SPI_SS_BIDIR_SWITCH = 1
} spi_slave_select_bidir;

/* Data type for SPI_TAGC_WIRE (SPI_HUB 4W). */
typedef enum {
	/* 0 = 3-wire SPI. */
	SPI_WIRE_3 = 0,
	/* 1 = 4-wire SPI. */
	SPI_WIRE_4 = 1
} spi_wire;

/* Data type for SPI_TAGC_SS_POL (SPI_HUB SS_HIGH). */
typedef enum {
	/* 0 = Slave Select is active low. */
	SPI_SS_POL_LOW  = 0,
	/* 1 = Slave Select is active high. */
	SPI_SS_POL_HIGH = 1
} spi_slave_select_pol;

struct spi_buffer_des {
	void  *base_addr;    /* Starting address */
	size_t len;     /* Number of bytes to transfer */
};
/* Data type for SPI_TAGC_TRANSFER. */
struct  spi_xfer {
	/* SPI command mode (SPI_CTRL MODE). */

	enum spi_xfer_mode {
		/* 00 Write */
		SPI_MODE_WRITE  = 0,
		/* 01 Read */
		SPI_MODE_READ = 1,
		/* 10 Bidir */
		SPI_MODE_BIDIR = 2,
		/* 11 Duplex */
		SPI_MODE_DUPLEX = 3
	} mode;

	/* SPI transfer length in bits (SPI_LENGTH SPI_LENGTH_VAL). */
	uint32_t length_in_bits;
	/*
	 * SPI number of bits until direction change(SPI_LENGTH SPI_LENGTH_DIR).
	 * Only valid in bidir mode.
	 */
	uint32_t length_bidir_in_bits;

	struct spi_buffer_des *rd_vector;
	uint32_t rd_vector_count;
	struct spi_buffer_des *wr_vector;
	uint32_t wr_vector_count;
};

struct spi_master;
struct spi_dev;

/**
 * Open a device unit
 *   @param   master        a pointer to a structure that is specific for a spi master
 *   @param   unit          the unit number of the device
 *   @return                a pointer that is specific for a spi slave device
 *   @remark                spi_install should be called first to get the spi master pointer
 */
struct spi_dev *spi_open(struct spi_master *master, uint32_t unit);

/**
 * close a device unit
 *   @param   dev           a pointer to a structure that is specific for a spi slave device
 *   @return                SPI_SUCCESS or error code. The meaning of the error refer to the
 *                          comments.
 */
uint32_t spi_close(struct spi_dev *dev);

/**
 * interrupt service function
 *
 *   @param   dev        a pointer to a structure that is specific for a spi slave device
 *   @return
 */
void spi_int_handler(struct spi_master *master);

/**
 * Submit an vector of buffers for transmission as one frame.
 *   @param   dev           a pointer to a structure that is specific for a spi slave device
 *   @param   vector        point to the array of buffers
 *   @param   count         number of buffer
 *   @return                SPI_SUCCESS or error code
 *   @remark                This function is not thread safe. If multiple threads try to use
 *                          this interface simutaneously, then the user must guarantee that the
 *			    shared resource is protected correctly.
 */
uint32_t spi_transfer(struct spi_dev *dev, struct spi_buffer_des *vector,
                      uint32_t count);

/**
 * get the configuration of the device.
 *   @param   dev           a pointer to a structure that is specific for a spi slave device
 *   @param   taglist       point to the configuration list
 *   @return                SPI_SUCCESS or error code
 *   @remark                tagdList points to an array of tags, which must be termi-nated
 *                          with the terminator tag SPI_TAGC_TAGEND.Each tagspecifies which
 *                          option to retrieve and is followed by extra storage for the return
 *                          value of the option, a tag parameter.
 */
uint32_t spi_get_conf(struct spi_dev *dev, int32_t *tagList);

/**
 * set the configuration of the device.
 *   @param   dev           a pointer to a structure that is specific for a spi slave device
 *   @param   taglist       point to the configuration list
 *   @return                SPI_SUCCESS or error code
 */
uint32_t spi_set_conf(struct spi_dev *dev, int32_t *tagList);

/**
 *   allocate and init spi master
 *
 *   @return                a pointer to a structure that is specific for a spi master
 *   @remark                should be called first if you use this library
 */
struct spi_master *spi_init(void);

/**
 * initialize the registers of spi controller.
 *
 *   @param   master        a pointer to spi master
 *   @param   addr          the base address of spi register
 *   @return                SPI_SUCCESS or error code
 *   @remark
 */
uint32_t spi_init_regs(struct spi_master *master, void *addr);

#ifdef __cplusplus
}
#endif

#endif /*__LIB_SPI_H*/

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

#ifndef LCS_LIBSPI_H_
#define LCS_LIBSPI_H_

#include <stdint.h>
#include <linux/types.h>
#include <linux/spi/spidev.h>


struct lib_spi_config {
   uint8_t mode;
   uint8_t bits_per_word;
   uint32_t speed_hz;
};

#define SPI_SLAVES_PER_DEV         3  /* The actual number slaves per device     */

#define SPI_SUCCESS                0

/* Data type for SPI_TAGC_POLARITY (SPI_CTRL_CPOL). */
typedef enum {
	/* In idle state spi_m_clk is low. */
	SPI_POLARITY_LOW  = 0,
	/* In idle state spi_m_clk is high. */
	SPI_POLARITY_HIGH = 1
} spi_polarity;

/* Data type for SPI_TAGC_PHASE (SPI_CTRL_CPHA). */
typedef enum {
	/* Sampling on leading edges. */
	SPI_PHASE_LEADING  = 0,
	/* Sampling on trailing edges. */
	SPI_PHASE_TRAILING = 1
} spi_phase;

/* Data type for SPI_TAGC_SEND_FIRST (SPI_CTRL_LSBFE). */
typedef enum {
	/* Send each 32 bits word with MSB first. */
	SPI_SEND_FIRST_MSB = 0,
	/* Send each 32 bits word with LSB first. */
	SPI_SEND_FIRST_LSB = 1
} spi_send_first;

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



void *libspi_config(uint32_t master,
                    uint32_t cs,
                    struct lib_spi_config *cfg);

void libspi_release_slave(void *handle);

int libspi_transfer(void *handle,
                    uint32_t len,
                    struct spi_ioc_transfer spi_ioc_tr[]);

#endif

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
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pthread.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/types.h>
#include <linux/spi/spidev.h>

#include "libspi_mimo.h"

#define MAX_NO_OF_SPI_MASTERS   2
#define MAX_NO_OF_SELECTS       3
#define MAX_BUF                 64

struct spi_slave {
        int32_t fd;
        uint32_t master;
        uint32_t cs;
};

#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_libspi_mimo
#include "tpt_create.h"
#include "tpt.h"

/******************************************************************************
 * Configure the SPI dev.
 *****************************************************************************/
void *libspi_config(uint32_t master,
                    uint32_t cs,
                    struct lib_spi_config *cfg)
{
        int ret = 0;
        struct spi_slave *slave = malloc(sizeof(struct spi_slave));

        slave->master = master;
        slave->cs = cs;
        char buf[MAX_BUF];

        if (snprintf(buf, sizeof(buf),
                     "/dev/spidev%u.%u",
                     slave->master, slave->cs) < 0) {
                     
				TPT_ERROR(STR("snprintf error master"));
                goto out_error;
        }
        slave->fd = open(buf, O_RDWR);
        if (slave->fd == -1) {
                errno = ENXIO;
				TPT_ERROR(STR("open error master"));
                goto out_error;
        }
        /*
         * spi mode
         */
        ret = ioctl(slave->fd, SPI_IOC_WR_MODE, &cfg->mode);
        if (ret == -1) {
			
			    TPT_ERROR(STR("wr mode ioctl error"));
                perror("ioctl");
                goto out_error;
        }

        /*
         * bits per word
         */
        ret = ioctl(slave->fd, SPI_IOC_WR_BITS_PER_WORD, &cfg->bits_per_word);
        if (ret == -1) {
			
				TPT_ERROR(STR("wr bit per word ioctl error"));
                perror("ioctl");
                goto out_error;
        }
        /*
         * max speed hz
         */
        ret = ioctl(slave->fd, SPI_IOC_WR_MAX_SPEED_HZ, &cfg->speed_hz);
        if (ret == -1) {
			
				TPT_ERROR(STR("wr speed ioctl error"));
                perror("ioctl");
                goto out_error;
        }
        return slave;
out_error:
        free(slave);
        return NULL;
}

/******************************************************************************
 *****************************************************************************/
void libspi_release_slave (void *handle)
{
        struct spi_slave *slave = (struct spi_slave *)handle;
        if (slave) {
                close(slave->fd);
                free(slave);
        }
}


/******************************************************************************
 * Transfer SPI data stream.
 *****************************************************************************/
int libspi_transfer(void *handle,
                    uint32_t len,
                    struct spi_ioc_transfer spi_ioc_tr[])
{
        int ret = 0;
        struct spi_slave *slave = (struct spi_slave *) handle;
        if (!slave) {
                errno = EINVAL;
				TPT_ERROR(STR("libspi transfer slave error"));
                goto out_error;
        }
        ret = ioctl(slave->fd,
                    SPI_IOC_MESSAGE(len),
                    spi_ioc_tr);
        if (ret == -1) {
				TPT_ERROR(STR("libspi transfer ioctl error"));
                perror("ioctl");
                goto out_error;
        }
        
        return 0;

out_error:
        return -errno;
}

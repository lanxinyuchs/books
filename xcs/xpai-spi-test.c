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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "itc.h"
#include "common.h"
#include "xpai_xsp_if.h"
#include "gpio.h"

#define MAX_MAILBOX_NUM    32
#ifndef __MACHINE_ZYNQMP_MIMO	
#define MAX_SLAVE_PER      16
#define LTU_SLAVE_SEL      72
#else
#define MAX_SLAVE_PER      3
#define LTU_SLAVE_SEL      0
#endif

#define CMD_READ           (1<<23)
#define CMD_WRITE          (0)
#define CMD_LEN            24

#define AMC7891_REG_ADDR  (0x40 << 16)
#define TEST_TIMES         50

#define AMC7891_0          0
#define AMC7891_1          32
#define AMC7891_DEVICE_ID  0x44

#define LTU_PORT_NOR       2
#define LTU_PRODUCT_ID_LSB 91
#define LTU_PRODUCT_ID_MSB 208
union itc_msg {
	uint32_t msg_id;
	struct XPAI_SendReceiveSPIIndS sendReceiveInd;
};

static int amc7891_config(uint32_t slave)
{
	struct XPAI_ConfigSPI_S spi_config = {0};

	spi_config.clockPolarity = 0;
	spi_config.clockPhase    = 0;
	spi_config.lsbFirst      = 0;
	spi_config.mode          = 1;
	spi_config.biDir         = 0;
	spi_config.rate          = 24;
	spi_config.idle          = 0;
	spi_config.busWidth      = 1;
	spi_config.port          = 0;
	spi_config.ssPolarity    = 1;

	return XPAI_ConfSPI(slave, &spi_config);
}

static int send_cmd_to_amc7891(uint32_t slave, uint32_t buffer[], int *value)
{
	int32_t ret = EXIT_SUCCESS;
	union itc_msg *receive_msg = NULL;
	uint32_t rx_filter[] = {1, XPAI_SENDRECEIVE_SPI_IND};

	ret = XPAI_SendReceiveSPI(slave, XPAI_XSP_SPI_CMD_MODE_DUPLEX, buffer, CMD_LEN,
	                          0);
	if(ret != 0) {
		printf("XPAI_SendReceiveSPI failed\n");
		return -1;
	}
	receive_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);

	switch ( receive_msg->msg_id ) {
	case XPAI_SENDRECEIVE_SPI_IND: {
		struct XPAI_SendReceiveSPIIndS *ind = (struct XPAI_SendReceiveSPIIndS *)
		                                      receive_msg;
		*value = ind->buffer[0] & 0xFF;
		break;
	}
	default:
		ret = -1;
		printf("receive unexpected signal 0x%08x received\n", receive_msg->msg_id);
		break;
	}
	itc_free(&receive_msg);

	return ret;
}
static int amc7891_read(uint32_t slave, int *value)
{
	uint32_t buffer[32] = {0};
	int32_t ret = EXIT_SUCCESS;

	buffer[0] = CMD_READ | AMC7891_REG_ADDR;

	ret = send_cmd_to_amc7891(slave, buffer, value);

	return ret;
}

static int amc7891_duplex_op(uint32_t slave, int *value)
{
	int32_t ret = EXIT_SUCCESS;
	int dummy = 0;

	ret = amc7891_read(slave, &dummy);
	if (ret) {
		printf("Firt amc7891_read failed\n");
		return ret;
	}

	ret = amc7891_read(slave, value);
	if (ret) {
		printf("Second amc7891_read failed\n");
		return ret;
	}

	if (*value != AMC7891_DEVICE_ID) {
		printf("value = %x\n", *value);
		return -1;
	}

	return ret;

}

static int config_test_cases(uint32_t tc)
{
	int ret = 0;
	struct XPAI_ConfigSPI_S spi_config = {0};

	spi_config.clockPolarity = 0;
	spi_config.clockPhase    = 0;
	spi_config.lsbFirst      = 0;
	spi_config.mode          = 1;
	spi_config.biDir         = 0;
	spi_config.rate          = 24;
	spi_config.idle          = 0;
	spi_config.busWidth      = 1;
	spi_config.port          = 0;
	spi_config.ssPolarity    = 1;

	switch(tc) {
	case 0:
		/*Invalid spi daemon index*/
		if (xpai_spi_slave_init(7, 1) != INIT_OK)
			ret = 0;
		else
			ret = -1;
		break;

	case 1:
		/*Invalid parameter, null spi_config*/
		if(XPAI_ConfSPI(0, 0) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;

	case 2:
		/*Invalid parameter, invalid clockPolarity*/
		spi_config.clockPolarity = 2;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 3:
		/*Invalid parameter, invalid clockPhase*/
		spi_config.clockPhase = 4;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 4:
		/*Invalid parameter, invalid mode*/
		spi_config.mode = 3;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 5:
		/*Invalid parameter, invalid lsbFirst*/
		spi_config.lsbFirst = 3;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 6:
		/*Invalid parameter, invalid bidir*/
		spi_config.biDir = 3;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 7:
		/*Invalid parameter, invalid idle*/
		spi_config.idle = 3;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 8:
		/*Invalid parameter, invalid port no*/
		spi_config.port = 3;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 9:
		/*Invalid parameter, invalid ssPolarity*/
		spi_config.ssPolarity = 3;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	case 10:
		/*Invalid parameter, invalid bus width*/
		spi_config.busWidth = 3;
		if(XPAI_ConfSPI(0, &spi_config) == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;
	}
	return ret;

}
static int config_negative_test()
{
	int i = 0;
	if(itc_create_mailbox("spi_config_negative_test", 0) == ITC_NO_ID) {
		printf("spi_config_negative_test mailbox is failed to create\n");
		return -1;
	}

	if (xpai_spi_slave_init(0, 1) != INIT_OK){
		printf("failed to xpai_spi_slave_init\n");
		return -1;
	}

	for(i = 0; i < 11; i++) {
		if(config_test_cases(i)) {
			printf("configure negative test case %d failed\n", i);
			return -1;
		}
	}
	return 0;
}
static int tx_rx_test_cases(uint32_t tc)
{
	int ret = 0;
	uint32_t buffer[32] = {0};

	switch(tc) {
	case 0:
		/*Invalid parameter, invalid slave*/
		ret = XPAI_SendReceiveSPI(97, XPAI_XSP_SPI_CMD_MODE_DUPLEX, buffer, CMD_LEN,
		                          0);
		if (ret == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;
		break;

	case 1:
		/*Invalid parameter, invalid length*/
		ret = XPAI_SendReceiveSPI(0, XPAI_XSP_SPI_CMD_MODE_DUPLEX, buffer, 0,
		                          0);
		if (ret == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;

		break;
	case 2:
		/*Invalid parameter, invalid bidir len*/
		ret = XPAI_SendReceiveSPI(0, XPAI_XSP_SPI_CMD_MODE_BIDIR, buffer, CMD_LEN,
		                          0);
		if (ret == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;

		break;
	case 3:
		/*Invalid parameter, null buffer*/
		ret = XPAI_SendReceiveSPI(0, XPAI_XSP_SPI_CMD_MODE_WRITE, 0, CMD_LEN,
		                          0);
		if (ret == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;

		break;
	case 4:
		/*Invalid parameter, invalid mode*/
		ret = XPAI_SendReceiveSPI(0, 4, buffer, CMD_LEN, 0);
		if (ret == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;

		break;
	case 5:
		/*Invalid parameter, invalid bidir len*/
		ret = XPAI_SendReceiveSPI(0, XPAI_XSP_SPI_CMD_MODE_BIDIR, buffer, 24,
		                          32);
		if (ret == XPAI_XSP_SPI_ILLEGAL_PARAMETER)
			ret = 0;
		else
			ret = -1;

		break;

	}
	return ret;

}
static int tx_rx_negative_test()
{
	int i = 0;
	struct XPAI_ConfigSPI_S spi_config = {0};

	spi_config.clockPolarity = 0;
	spi_config.clockPhase    = 0;
	spi_config.lsbFirst      = 0;
	spi_config.mode          = 0;
	spi_config.biDir         = 0;
	spi_config.rate          = 24;
	spi_config.idle          = 0;
	spi_config.busWidth      = 1;
	spi_config.port          = 0;
	spi_config.ssPolarity    = 1;

	if(itc_create_mailbox("tx_rx_negative_test", 0) == ITC_NO_ID) {
		printf("tx_rx_negative_test mailbox is failed to create\n");
		return -1;
	}

	if (xpai_spi_slave_init(0, 1) != INIT_OK){
		printf("failed to xpai_spi_slave_init\n");
		return -1;
	}

	if(XPAI_ConfSPI(0, &spi_config)){
		printf("failed to XPAI_ConfSPI\n");
		return -1;
	}
	for(i = 0; i < 6; i++) {
		if(tx_rx_test_cases(i)) {
			printf("tx_rx negative test case %d failed\n", i);
			return -1;
		}
	}
	return 0;
}
static int amc7891_duplex_mode_test(uint32_t slave)
{
	int read_value = 0;
	int i;

	if(itc_create_mailbox("spi_amc7891_test", 0) == ITC_NO_ID) {
		printf("spi_amc7891_test mailbox is failed to create\n");
		return -1;
	}

	if (xpai_spi_slave_init(slave / MAX_SLAVE_PER, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", slave);
		return -1;
	}

	if (amc7891_config(0)) {
		printf("XPAI_ConfSPI failed\n");
		return -1;
	}

	for (i = 0; i < TEST_TIMES; i++) {
		if(amc7891_duplex_op(slave, &read_value) != 0) {
			printf("amc7891_duplex_op failed\n");
			return -1;
		}
	}
	return 0;
}

static int set_ltu_gpio(void)
{
#ifndef __MACHINE_ZYNQMP_MIMO
	gpio_status_t gpio_status;
	uint8_t dir_values[] = {1};
	uint32_t gpio_pins[] = {316};
	gpio_handle_t gpio_handle;
	uint8_t write_value[] = {0};

	gpio_status = gpio_reserve(sizeof(gpio_pins) / sizeof(gpio_pins[0]), gpio_pins,
	                           &gpio_handle);
	if(gpio_status != GPIO_STATUS_SUCCESS) {
		printf("gpio is not reserved correctly\n");
		return -1;
	}

	gpio_status = gpio_write(gpio_handle, sizeof(gpio_pins) / sizeof(gpio_pins[0]),
	                         gpio_pins, write_value);
	if(gpio_status != GPIO_STATUS_SUCCESS) {
		printf("gpio is not written correctly\n");
		return -1;
	}

	gpio_status = gpio_set_dir(gpio_handle,
	                           sizeof(gpio_pins) / sizeof(gpio_pins[0]),
	                           gpio_pins, dir_values);
	if(gpio_status != GPIO_STATUS_SUCCESS) {
		printf("gpio is not set dir correctly\n");
		return -1;

	}
#endif
	return 0;
}

static int ltu_rw(uint32_t data,
                  uint32_t len,
                  uint32_t bdir_len,
                  uint32_t mode,
                  uint32_t *value)
{
	uint32_t buffer[1];
	union itc_msg *receive_msg = NULL;
	uint32_t ret;
	uint32_t rx_filter[] = {1, XPAI_SENDRECEIVE_SPI_IND};

	buffer[0] = data;
#ifdef __MACHINE_ZYNQMP_MIMO
	ret = XPAI_SendReceiveSPI(3, mode,
	                          &buffer[0], len, bdir_len);
	if(ret != 0) {
		printf("XPAI_SendReceiveSPI failed\n");
		return -1;
	}
#else
	ret = XPAI_SendReceiveSPI(LTU_SLAVE_SEL, mode,
	                          &buffer[0], len, bdir_len);
	if(ret != 0) {
		printf("XPAI_SendReceiveSPI failed\n");
		return -1;
	}
#endif
	receive_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);

	switch ( receive_msg->msg_id ) {
	case XPAI_SENDRECEIVE_SPI_IND: {
		struct XPAI_SendReceiveSPIIndS *ind = (struct XPAI_SendReceiveSPIIndS *)
		                                      receive_msg;
		*value = ind->buffer[0];
		break;
	}
	default:
		ret = -1;
		printf("receive unexpected signal 0x%08x received\n", receive_msg->msg_id);
		break;
	}
	itc_free(&receive_msg);
	return ret;
}
static int ltu_test()
{
	struct XPAI_ConfigSPI_S spi_config = {0};
	uint32_t value;

	if(itc_create_mailbox("ltu-test", 0) == ITC_NO_ID) {
		printf("ltu-test mailbox is failed to create\n");
		return -1;
	}

	if (set_ltu_gpio()) {
		printf("Failed to init ltu gpio settings\n");
		return -1;
	}
#ifdef __MACHINE_ZYNQMP_MIMO
	if (xpai_spi_slave_init(0, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}

	if (xpai_spi_slave_init(1, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}
#else
	if (xpai_spi_slave_init(LTU_SLAVE_SEL / MAX_SLAVE_PER, 1) != INIT_OK) {
	  printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
	  return -1;
	}
#endif
	spi_config.clockPolarity = 0;
	spi_config.clockPhase    = 0;
	spi_config.lsbFirst      = 0;
	spi_config.biDir         = 0;
#ifndef __MACHINE_ZYNQMP_MIMO	
	spi_config.rate          = 0x7A;
	spi_config.busWidth 	 = 0;
	spi_config.mode          = 1;
	spi_config.idle          = 0;
	spi_config.port          = LTU_PORT_NOR;
#else
	spi_config.rate          = 2500000;
	spi_config.busWidth 	 = 1;
	spi_config.mode          = 0;
	spi_config.idle          = 1;
	spi_config.port          = 1;
#endif

	spi_config.ssPolarity    = 0;

#ifdef __MACHINE_ZYNQMP_MIMO	
   	if(XPAI_ConfSPI(3, &spi_config)) {
		printf("XPAI_ConfSPI failed\n");
		return -1;
	}

//write ltu4808 r13 register for setting readback
	if(ltu_rw(0x1B0C01AC, 24, 0, XPAI_XSP_SPI_CMD_MODE_WRITE, &value))
		printf("write r13 reg error, reg addr is: 0x1B0C01AC\n");
	//readback r13.
	/*uint32_t ctrl = (13 << 16)|0x1f;

	if(ltu_rw(ctrl, 24, 0, XPAI_XSP_SPI_CMD_MODE_READ, &value))
	{
		printf("read r13 reg error, ctrl is:0x%x\n", ctrl);
		return -1;
	}
    	printf("value is: %x\n", value);*/

#else
	if(XPAI_ConfSPI(LTU_SLAVE_SEL, &spi_config)) {
		printf("XPAI_ConfSPI failed\n");
		return -1;
	}

	if(ltu_rw(0x014902, 24, 0, XPAI_XSP_SPI_CMD_MODE_WRITE, &value))
		return -1;

	if(ltu_rw(0x00008005, 24, 16, XPAI_XSP_SPI_CMD_MODE_BIDIR, &value))
		return -1;

	if (value !=  LTU_PRODUCT_ID_LSB) {
		printf("Read register 5, get wrong data: 0x%x\n", value);
		return -1;
	}
	printf("Product ID LSB: %x\n", value);
	if(ltu_rw(0x00008004, 24, 16, XPAI_XSP_SPI_CMD_MODE_BIDIR, &value))
		return -1;

	if (value !=  LTU_PRODUCT_ID_MSB) {
		printf("Read register 4, get wrong data: 0x%x\n", value);
		return -1;
	}
	printf("Product ID MSB: %x\n", value);
#endif	
	return 0;
}

static int ltu_config(void)
{
	struct XPAI_ConfigSPI_S spi_config = {0};

	if(itc_create_mailbox("ltu-config", 0) == ITC_NO_ID) {
		printf("ltu-config mailbox is failed to create\n");
		return -1;
	}

	if (xpai_spi_slave_init(0, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}

	if (xpai_spi_slave_init(1, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}

	spi_config.clockPolarity = 0;
	spi_config.clockPhase	 = 0;
	spi_config.lsbFirst 	 = 0;
	spi_config.biDir		 = 0;
	spi_config.rate 		 = 2500000;
	spi_config.busWidth 	 = 1;
	spi_config.mode 		 = 0;
	spi_config.idle 		 = 1;
	//todo,yan, the port num will change follow hw iwd
	spi_config.port 		 = 1;
	spi_config.ssPolarity	 = 0;
	//todo,yan, the slave num will change follow hw iwd
	if(XPAI_ConfSPI(3, &spi_config)) {
		printf("XPAI_ConfSPI failed\n");
		return -1;
	}
	printf("XPAI_ConfSPI ok\n");
	return 0;

}

static int ltu_read(char **argv)
{
	uint32_t reg_addr = strtoul(argv[3], NULL, 0);

	uint32_t value;

	if(itc_create_mailbox("ltu-read", 0) == ITC_NO_ID) {
		printf("ltu-test mailbox is failed to create\n");
		return -1;
	}

	if (xpai_spi_slave_init(0, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}

	if (xpai_spi_slave_init(1, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}

//todo,yan, first need write 0x149 register to readback.
	uint32_t ctrl = ((1 << 23)|(reg_addr << 8)); 

	if(ltu_rw(ctrl, 24, 0, XPAI_XSP_SPI_CMD_MODE_READ, &value))
	{
		printf("read r13 reg error, ctrl is:0x%x\n", ctrl);
		return -1;
	}
		printf("value is: %x\n", value);

	return 0;

}

static int ltu_write(char **argv)
{
	uint32_t reg_value = strtoul(argv[3], NULL, 0);

	uint32_t value;

	if(itc_create_mailbox("ltu-write", 0) == ITC_NO_ID) {
		printf("ltu-test mailbox is failed to create\n");
		return -1;
	}

	if (xpai_spi_slave_init(0, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}

	if (xpai_spi_slave_init(1, 1) != INIT_OK) {
		printf("failed to xpai_spi_slave_init with slave 0x%x\n", LTU_SLAVE_SEL);
		return -1;
	}

	if(ltu_rw(reg_value, 24, 0, XPAI_XSP_SPI_CMD_MODE_WRITE, &value))
		printf("write reg error,\n");
		
	printf("write value sucess\n");

	return 0;

}
static void printUsage()
{
	printf("Usage: xpai_spi_test -tc <test case number>\n\n"
	       "\twhere <test case nbr> is one of:\n"
	       "\t1: spi configuration negative test\n"
	       "\t2: spi tx rx negative test\n"
	       "\t3: amc7891 test.\n"
	       "\t4: ltu test\n"
   		   "\t5: ltu config\n"
   		   "\t6: ltu write wrte_value\n"
   		   "\t7: ltu read reg_num\n"
	      );
}
int main(int argc, char **argv)
{
	int tc = 0, result;

	if( argc < 3) {
		printUsage();
		exit(-1);
	}

	if (argc >= 3) {
		if (strstr(argv[1], "-tc") != NULL) {
			tc = atoi(argv[2]);
		}
	}

	if(itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!\n", __func__);
		return -1;
	}

	switch (tc) {
	case 1:
		printf("spi configuration negative test...\n");
		result = config_negative_test();
		break;
	case 2:
		printf("spi tx rx negative test...\n");
		result = tx_rx_negative_test();
		break;

	case 3:
		printf("amc7891 duplex mode test...\n");
		result = amc7891_duplex_mode_test(AMC7891_0);
		break;
	case 4:
		/*doesn't support on the SVP*/
		printf("LTU test...\n");
		result = ltu_test();
		break;
	case 5:
		/*doesn't support on the SVP*/
		printf("LTU config...\n");
		result = ltu_config();
		break;	
	case 6:
		/*doesn't support on the SVP*/
		printf("LTU write...\n");
		result = ltu_write(argv);
		break;
	case 7:
		/*doesn't support on the SVP*/
		printf("LTU read...\n");
		result = ltu_read(argv);
	break;
	default:
		printf("Wrong tc number %d\n", tc);
		printUsage();
		exit(-1);
	}

	if (result) {
		printf("\nSPI Test %d FAILED \n", tc);
		return -1;
	} else {
		printf("\nSPI Test %d PASSED \n", tc);
	}
	return 0;
}


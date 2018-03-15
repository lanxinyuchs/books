#include "xpai_hdr_i2c_if.h"
#include "common.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "itc.h"
#include <stdbool.h>
#include "ose.h"
#include "osetypes.h"
#include "time.h"
#include <unistd.h>
#include "ctype.h"
#include "gpio.h"


#define MAX_MAILBOX_NUM    32

static U32 client_ref = 1234;


#define PRINT_USAGE() print_usage()
#define PRINT_ARGUMENT_INVALID() print_argument_invalid()

#ifndef __MACHINE_ZYNQMP_MIMO
static int power_on_sfp_by_gpio(void)
{
	gpio_status_t status;
	gpio_handle_t handle;


	const uint32_t pins[] =      {233/*sfp port 0*/, 235/*sfp port 1*/};
	const uint8_t values[] =     {0,                 0};
	const uint8_t dir_values[] = {1,                 1};

	status = gpio_reserve(sizeof(pins) / sizeof(pins[0]), pins,
	                      &handle);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("power up sfp pins failed at gpio_reserve\n");
		return -1;
	}

	status = gpio_write(handle, sizeof(pins) / sizeof(pins[0]),
	                    pins, values);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("power up sfp pins failed at gpio write\n");
		return -1;
	}

	// Set direction
	status = gpio_set_dir(handle, sizeof(pins) / sizeof(pins[0]),
	                      pins, dir_values);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("power up sfp pins failed at gpio set dir\n");
		return -1;
	}

	usleep(300000); /* Wait for SFP power up */
	return 0;
}
#endif
static int handle_init(U32 portId)
{
	S32 ret;
	ret = xpai_i2c_port_init(portId, client_ref);
	if (ret != INIT_OK) {
		printf("I2C init failed on port %u, return %d\n", portId, ret);
		return 1;
	}
	return 0;
}

static void print_usage(void)
{
	printf( "-h\n"
	        "       Display usage information.\n\n"
	        "init\n"
	        "       XPAI I2C init.\n\n"
	        "read <portId> <address> <subAddress | NONE> <bytes no>\n"
	        "       Read data\n\n"
	        "write <portId> <address> <subAddress | NONE> <bytes no>\n"
	        "      <data0 data1 data2 ... (byte data)>\n"
	        "       Write data\n"
	        "sfptest1 <portId> <bytes no> \n"
	        "       Write addr area [128, 255] on SFP A2.\n"
	        "       Verify the writing data is correct.\n"
	        "sfptest2 <portId> <bytes no>\n"
	        "       Write 0x55 on SFP A2.\n"
	        "       Verify the data is correct in [128, 255].\n"
	        "sfptest3\n"
	        "       Verify read out 1byte is as expected on SFP A0h.\n"
	        "netest1 <portId> \n"
	        "       portId should be >=3 .\n"
	        "       Verify portId is not valid in init.\n"
	        "netest2 <portId>\n"
	        "       portId should be >=3 .\n"
	        "       Verify portId is not valid.\n"
	        "       The difference with netest1 is this test verified XPAI_XX function\n"
	        "netest3\n"
	        "       Verify the address is out of [0 - 0x7f] .\n"
	        "sfpdump <portId>\n"
	        "       Dump SFP A0 and A2 data\n"
	      );
}


static void print_argument_invalid(void)
{
	printf("argument is invalid.\n");
	printf("type -h for help\n");

}
static int init_itc(void)
{
	/* Initialize ITC and create mailbox, initialize XPAI I2C connection */
	if(itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!\n", __func__);
		return 1;
	}
	if(itc_create_mailbox("i2c-test", 0) == ITC_NO_ID) {
		printf("i2c-test mailbox is failed to create\n");
		return 1;
	}
	return 0;
}

static void delete_itc(void)
{
	itc_mbox_id_t mbox = itc_current_mbox();
	if( mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}
}

static void print_dump_data(U8 buf[])
{
	U8 line;
	U8 *ptr = buf;
	U8 length = 16;
	U8 address = 0;

	for (line = 0; line < length; line++) {
		// print address and data in hex format (grouped in 4 bytes chunks)
		printf("%04X   ", (unsigned int) address);
		for (U32 i = 0; i < length; i++) {
			if ((i % 4) == 0) {
				printf(" ");
			}
			printf("%02X", *ptr);
			ptr++;
		}

		// print data in ASCII format, replace non-printable characters with a dot
		ptr -= length;
		printf(" \"");
		for (U32 i = 0; i < length; i++) {
			printf("%c", !isprint((int)*ptr) ? '.' : *ptr);
			++ptr;
		}
		printf("\"\n");
		address += length;
	}
	printf("\n");
}


static void dump_data(U32 port_id, U32 addr, U32 size)
{
	U8 *buf;
	U8 *ptr;
	U32 read_bytes_num = 4;

	buf = (U8 *) malloc(sizeof(U8) * size);
	if(buf == NULL) {
		printf("malloc failed\n");
		return;
	}
	memset(buf, size, sizeof(U8));
	ptr = buf;

	printf("Dump 0x%x is starting\n", addr);
	for(U32 subAddr = 0; subAddr < size; subAddr += read_bytes_num) {
		S32 ret = XPAI_I2CReadSubPort(port_id, addr, ptr, read_bytes_num, subAddr);
		if(ret != (S32)read_bytes_num) {
			printf("fail to read %d\n", subAddr);
			free(buf);
			return;
		}
		ptr += read_bytes_num;
	}

	printf("Hex     Hex                                 Bin\n");
	printf("----    -------- -------- -------- -------- ------------------\n");
	print_dump_data(buf);

	free(buf);

}
static int sfpdump(int argc, char **argv)
{
	U32 port_id = strtoul(argv[2], NULL, 0);
	U32 addr_A0 = 0x50;
	U32 addr_A2 = 0x51;
	U32 size_A0 = 256;
	U32 size_A2 = 256;
	(void)argc;

	printf("sfp dump is starting ...\n");

	if(handle_init(port_id)) {
		return 1;
	}

	dump_data(port_id, addr_A0, size_A0);
	dump_data(port_id, addr_A2, size_A2);

	return 0;
}

static int sfpwrite(U32 pattern, U32 start, U32 end,
                    U32 num_byte_once, U32 port_id, bool addr_mode)
{
	U32 dev_addr = 0x51;
	U32 num_fail = 0;
	U8 *buffer;
	U32 j = 0;
	S32 rv = 0;
	U32 buffer_size = 0;
	bool subwrite = false;

	printf("Writing [%d, %d) on port%d is starting\n", start, end, port_id);

	for (U32 sub_addr = start; sub_addr < end; sub_addr += num_byte_once) {
		if(sub_addr % 2) {
			/* This part is for testing XPAI_I2CWritePort
			             * which put sub_addr as the first data. */
			subwrite = false;
			buffer_size = num_byte_once + 1;
			buffer = malloc(buffer_size * sizeof(U8));
			if(buffer == NULL) {
				printf("malloc failed\n");
				return 1;
			}
			buffer[0] = sub_addr;
			j = 1;
		} else {
			subwrite = true;
			buffer_size = num_byte_once;
			buffer = malloc(buffer_size * sizeof(U8));
			if(buffer == NULL) {
				printf("malloc failed\n");
				return 1;
			}
			j = 0;
		}

		for(; j < buffer_size; j++) {
			if(addr_mode && (subwrite == false)) {
				buffer[j] = sub_addr + j - 1;
			} else if(addr_mode && (subwrite == true)) {
				buffer[j] = sub_addr + j;
			} else {
				buffer[j] = pattern;
			}
			if(subwrite) {
				printf("writing %d @ %d ", buffer[j], sub_addr + j);
			} else {
				printf("writing %d @ %d ", buffer[j], sub_addr + j - 1);
			}
		}

		if(subwrite == false) {
			rv = XPAI_I2CWritePort(port_id, dev_addr, buffer, buffer_size);
		} else {
			rv = XPAI_I2CWriteSubPort(port_id, dev_addr, buffer, buffer_size, sub_addr);
		}

		usleep(40000);/* wait 40ms according to sfp datasheet */
		if(rv <= 0) {
			printf("write failed with %d\n", rv);
			num_fail++;
		} else if(rv == (S32)buffer_size) {
			printf("write %d bytes successfully\n", rv);
		} else {
			printf("write part of data %d successfully\n", rv);
			num_fail += buffer_size - rv;
		}
		free(buffer);
	}

	if(!num_fail) {
		printf("Writing successfully\n");
		return 0;
	} else {
		printf("Total: %d fail\n", num_fail);
		return 1;
	}
}


static int sfpread(U32 pattern, U32 start, U32 end,
                   U32 num_byte_once, U32 port_id, bool addr_mode)
{
	U32 dev_addr = 0x51;
	U8 *buffer;
	U8 *buffer_read;
	S32 rv = 0;
	U32 num_fail = 0;
	U32 ret = 0;

	buffer = malloc(num_byte_once * sizeof(U8));
	if(buffer == NULL) {
		printf("malloc failed\n");
		return 1;
	}
	buffer_read = malloc(num_byte_once * sizeof(U8));
	if(buffer_read == NULL) {
		free(buffer);
		printf("malloc failed\n");
		return 1;
	}
	printf("Reading [%d, %d) on port%d is starting\n", start, end, port_id);
	for(U32 sub_addr = start; sub_addr < end; sub_addr += num_byte_once) {
		for(U32 j = 0; j < num_byte_once; j++) {
			if(addr_mode) {
				buffer[j] = sub_addr + j; /* expect value */
			} else {
				buffer[j] = pattern;
			}
		}

		rv = XPAI_I2CReadSubPort(port_id, dev_addr,
		                         buffer_read, num_byte_once, sub_addr);

		usleep(40000);/* wait 40ms according to sfp datasheet */
		if(rv != (S32)num_byte_once) {
			printf("read failed at 0x%x, read %d bytes\n", sub_addr, rv);
			free(buffer);
			free(buffer_read);
			return 1;
		}

		if(memcmp(buffer, buffer_read, num_byte_once * sizeof(U8))) {
			printf("not equal \n");
			num_fail++;
			for(U32 j = 0; j < num_byte_once; j++) {
				printf("0x%x vs 0x%x at 0x%x\n ",
				       buffer[j], buffer_read[j], sub_addr);
			}
		}
	}
	if(!num_fail) {
		printf("Reading successfully\n");
	} else {
		printf("Total: %d fail\n", num_fail);
		ret = 1;
	}
	free(buffer);
	free(buffer_read);
	return ret;
}

static int sfptestcase1(int argc, char **argv)
{
	U32 port_id = strtoul(argv[2], NULL, 0);
	U32 num_byte_once = strtoul(argv[3], NULL, 0);

	(void)argc;
#if 0
	if(num_byte_once >= 4) {
		printf("Error: currently i2c only support write less than 4 bytes once\n");
	}
#endif
	printf("-- sfp test case1 is starting ...\n");

	if(handle_init(port_id)) {
		goto sfptestcase1_err;

	}
	if(sfpwrite(0, 128, 256, num_byte_once, port_id, true)) {
		goto sfptestcase1_err;
	}
	if(sfpread(0, 128, 256, num_byte_once, port_id, true)) {
		goto sfptestcase1_err;
	}

	if(sfpwrite(0, 128, 256, num_byte_once, port_id, false)) {
		goto sfptestcase1_err;
	}
	if(sfpread(0, 128, 256, num_byte_once, port_id, false)) {
		goto sfptestcase1_err;
	}

	printf("-- test case with port%d and byte number%d passed\n",
	       port_id, num_byte_once);
	return 0;

sfptestcase1_err:
	printf("-- test case with port%d and byte number%d failed\n",
	       port_id, num_byte_once);
	return 1;
}

static int sfptestcase2(int argc, char **argv)
{
	U32 port_id = strtoul(argv[2], NULL, 0);
	U32 num_byte_once = strtoul(argv[3], NULL, 0);
	(void)argc;
#if 0
	if(num_byte_once >= 4) {
		printf("Error: currently i2c only support write less than 4 bytes once\n");
	}
#endif
	printf("-- sfp test case2 is starting ...\n");

	if(handle_init(port_id)) {
		goto sfptestcase2_err;
	}

	/* init 0 into SFP */
	if(sfpwrite(0, 128, 256, 2, port_id, false)) {
		goto sfptestcase2_err;
	}
	if(sfpread(0, 128, 256, 2, port_id, false)) {
		goto sfptestcase2_err;
	}
	/* write 1/2/3 0x55 @ 128/129/130 */
	if(sfpwrite(0x55, 128, 128 + num_byte_once, num_byte_once, 0, false)) {
		goto sfptestcase2_err;
	}
	/* read data from 128/129/130 */
	if(sfpread(0x55, 128, 128 + num_byte_once, num_byte_once, 0, false)) {
		goto sfptestcase2_err;
	}
	/* read the left data */
	if(sfpread(0, 128 + num_byte_once, 256, num_byte_once, 0, false)) {
		goto sfptestcase2_err;
	}

	/* restore 0 into SFP */
	if(sfpwrite(0, 128, 128 + num_byte_once, num_byte_once, 0, false)) {
		goto sfptestcase2_err;
	}
	if(sfpread(0, 128, 128 + num_byte_once, num_byte_once, 0, false)) {
		goto sfptestcase2_err;
	}

	printf("-- test case with port%d and byte number%d passed\n",
	       port_id, num_byte_once);

	return 0;

sfptestcase2_err:
	printf("-- test case with port%d and byte number%d failed\n",
	       port_id, num_byte_once);
	return 1;
}

static int sfptestcase3(int argc, char **argv)
{
	U32 port_id = 0;
	U32 address = 0x50;
	U8 data = 0x0; /* sub address */
	S32 ret;

	(void)argc;
	(void)argv;

	printf("-- sfp test case3 is starting ...\n");

	if(handle_init(port_id)) {
		return 1;
	}

	ret = XPAI_I2CWritePort(port_id, address, &data, 1);
	if(ret <= 0) {
		printf("write failed return %d\n", ret);
		printf("-- test case failed\n");

		return 1;
	} else {
		printf("write %d byte successfully\n", ret);
	}

	/* According to SFP datasheet, the 1 byte@0xA0h value should be 0x03 on SFP
	*  What means SFP or SFP+ */
	ret = XPAI_I2CReadPort(port_id, address, &data, 1);
	if(ret <= 0) {
		printf("read failed return %d\n", ret);
		return 1;
	} else {
		if(data != 0x03) {
			printf("read data %d is not equal to 0x03\n", data);
			printf("-- test case failed\n");
			return 1;
		} else {
			printf("read data %d is equal to 0x03\n", data);
			printf("-- test case passed\n");
			return 0;
		}
	}

}

/* Test port number is bigger than MAX_PORT_NUMBER(3) */
static int negativetestcase1(int argc, char **argv)
{
	U32 port_id = strtoul(argv[2], NULL, 0);

	(void)argc;

	printf("-- negative test case1 is starting ...\n");

	if(handle_init(port_id)) {
		printf("-- test case pass\n");
		return 0;
	} else {
		printf("-- test case failed\n");
		return 1;
	}

}

/* Test port number is bigger than MAX_PORT but pass connection establishment */
static int negativetestcase2(int argc, char **argv)
{
	U32 port_id = strtoul(argv[2], NULL, 0);
	U8 data;
	S32 ret;
	U32 address = 0x50;
	(void)argc;


	printf("-- negative test case2 is starting ...\n");

	/* Hard code port number to 0 to force handle_init() return true,
	 * Then the test can be continued executing to verify XPAI_XX function
	 * when the port number bigger than MAX_PORT_NUMBER(3) */

	if(handle_init(0)) {
		goto negativetestcase2_err;
	}

	ret = XPAI_I2CReadPort(port_id, address, &data, 1);
	if(ret == XPAI_I2C_WRONG_PORT) {
		printf("readPort func with port %d case pass\n", port_id);
	} else {
		printf("readPort func with port %d case failed\n", port_id);
		goto negativetestcase2_err;

	}


	ret = XPAI_I2CReadSubPort(port_id, address, &data, 1, 0);
	if(ret == XPAI_I2C_WRONG_PORT) {
		printf("readSubPort func with port %d case pass\n", port_id);
	} else {
		printf("readSubPort func with port %d case failed\n", port_id);
		goto negativetestcase2_err;

	}


	data = 0xAA;
	ret = XPAI_I2CWritePort(port_id, address, &data, 1);
	if(ret == XPAI_I2C_WRONG_PORT) {
		printf("writePort func with port %d case pass\n", port_id);
	} else {
		printf("writePort func with port %d case failed\n", port_id);
		goto negativetestcase2_err;

	}


	ret = XPAI_I2CWriteSubPort(port_id, address, &data, 1, 0x80);
	if(ret == XPAI_I2C_WRONG_PORT) {
		printf("writeSubPort func with port %d case pass\n", port_id);
	} else {
		printf("writeSubPort func with port %d case failed\n", port_id);
		goto negativetestcase2_err;

	}

	printf("-- test case passed.\n");
	return 0;
negativetestcase2_err:
	printf("-- test case failed.\n");
	return 1;

}

/* Test device address is not 7 bit */
static int negativetestcase3(int argc, char **argv)
{
	U32 port_id = 0;
	U8 data;
	S32 ret;
	U32 address = 0xF2;  /* not 7 bit device address */
	(void)argc;
	(void)argv;

	printf("negative test case3 is starting ...\n");

	ret = XPAI_I2CReadPort(port_id, address, &data, 1);
	if(ret == XPAI_I2C_OTHER_ERROR ) {
		printf("readPort func with address %d case pass\n", address);
	} else {
		printf("readPort func with address %d case failed\n", address);
		goto negativetestcase3_err;

	}


	ret = XPAI_I2CReadSubPort(port_id, address, &data, 1, 0x0);
	if(ret == XPAI_I2C_OTHER_ERROR) {
		printf("readSubPort func with address %d case pass\n", address);
	} else {
		printf("readSubPort func with address %d case failed\n", address);
		goto negativetestcase3_err;

	}


	data = 0xAA;
	ret = XPAI_I2CWritePort(port_id, address, &data, 1);
	if(ret == XPAI_I2C_OTHER_ERROR) {
		printf("writePort func with address %d case pass\n", address);
	} else {
		printf("writePort func with address %d case failed\n", address);
		goto negativetestcase3_err;

	}


	ret = XPAI_I2CWriteSubPort(port_id, address, &data, 1, 0x80);
	if(ret == XPAI_I2C_OTHER_ERROR) {
		printf("writeSubPort func with address %d case pass\n", address);
	} else {
		printf("writeSubPort func with address %d case failed\n", address);
		goto negativetestcase3_err;

	}

	printf("-- test case passed.\n");
	return 0;

negativetestcase3_err:
	printf("-- test case failed.\n");
	return 1;

}

static int i2c_read_test(int argc, char **argv)
{
	bool use_sub = false;
	U32 port_id;
	U32 address;
	U32 sub_addr;
	U32 length;
	U32 i;
	U8 *buffer;
	S32 ret;
	(void)argc;
	port_id = strtoul(argv[2], NULL, 0);
	address = strtoul(argv[3], NULL, 0);
	if(!strcmp(argv[4], "NONE")) {
		use_sub = false;
	} else {
		use_sub = true;
		sub_addr = strtoul(argv[4], NULL, 0);
	}
	length = strtoul(argv[5], NULL, 0);

	printf("test i2c starting ...\n");


	if(handle_init(port_id)) {
		return 1;
	}

	buffer = (U8 *)malloc(length * sizeof(U8));
	if(buffer == NULL) {
		printf("malloc failed\n");
		return 1;
	}
	if(use_sub == false) {
		ret = XPAI_I2CReadPort(port_id, address, buffer, length);
	} else {
		ret = XPAI_I2CReadSubPort(port_id, address, buffer, length, sub_addr);
	}

	if (ret <= 0) {
		free(buffer);
		printf("read failed return %d\n", ret);
		return 1;
	}

	for(i = 0; i < length; i++) {
		if(i == 0) {
			printf("read port value 0x%x", buffer[i]);
		} else {
			printf(" 0x%x", buffer[i]);
		}
	}
	printf("\n");
	free(buffer);
	return 0;
}

static int i2c_write_test(int argc, char **argv)
{
	bool use_sub = false;
	U32 port_id;
	U32 address;
	U32 sub_addr;
	U32 length;
	U32 i;
	U8 *buffer;
	S32 ret;

	port_id = strtoul(argv[2], NULL, 0);
	address = strtoul(argv[3], NULL, 0);
	if(!strcmp(argv[4], "NONE")) {
		use_sub = false;
	} else {
		use_sub = true;
		sub_addr = strtoul(argv[4], NULL, 0);
	}
	length = strtoul(argv[5], NULL, 0);
	if((argc - 6) != (int)length) {
		PRINT_ARGUMENT_INVALID();
		return 1;
	}

	buffer = (U8 *)malloc(length * sizeof(U8));
	if(buffer == NULL) {
		printf("malloc failed\n");
		return 1;
	}
	for(i = 0; i < length; i++) {
		buffer[i] = strtoul(argv[i + 6], NULL, 0);
	}
	printf("test i2c starting ...\n");

	if(handle_init(port_id)) {
		free(buffer);
		return 1;
	}

	if(use_sub == false) {
		ret = XPAI_I2CWritePort(port_id, address, buffer, length);
	} else {
		ret = XPAI_I2CWriteSubPort(port_id, address, buffer, length, sub_addr);

	}
	if (ret <= 0) {
		printf("write failed return %d\n", ret);
		free(buffer);
		return 1;
	} else {
		printf("write %d bytes successfully\n", ret);
		free(buffer);
		return 0;
	}
}



int main(int argc, char **argv)
{
	int ret = EXIT_FAILURE;

	if(argc == 1) {
		PRINT_USAGE();
		return EXIT_FAILURE;
	}

	if(init_itc()) {
		return EXIT_FAILURE;
	}
#ifndef __MACHINE_ZYNQMP_MIMO
	if (power_on_sfp_by_gpio()) {
		goto main_end;
	}
#endif
	if (!strcmp(argv[1], "sfptest1")) {
		if(argc != 4) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(sfptestcase1(argc, argv)) {
			goto main_end;
		}

	} else if(!strcmp(argv[1], "sfptest2")) {
		if(argc != 4) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(sfptestcase2(argc, argv)) {
			goto main_end;
		}
	}  else if(!strcmp(argv[1], "sfptest3")) {
		if(argc != 2) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(sfptestcase3(argc, argv)) {
			goto main_end;
		}
	} else if(!strcmp(argv[1], "netest1")) {
		if(argc != 3) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(negativetestcase1(argc, argv)) {
			goto main_end;
		}
	} else if(!strcmp(argv[1], "netest2")) {
		if(argc != 3) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(negativetestcase2(argc, argv)) {
			goto main_end;
		}
	} else if(!strcmp(argv[1], "netest3")) {
		if(argc != 2) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(negativetestcase3(argc, argv)) {
			goto main_end;
		}
	} else if(!strcmp(argv[1], "sfpdump")) {
		if(argc != 3) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(sfpdump(argc, argv)) {
			goto main_end;
		}
	} else if(!strcmp(argv[1], "read")) {
		if(argc != 6) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(i2c_read_test(argc, argv)) {
			goto main_end;
		}
	} else if(!strcmp(argv[1], "write")) {
		if(argc < 6) {
			PRINT_ARGUMENT_INVALID();
			goto main_end;
		}
		if(i2c_write_test(argc, argv)) {
			goto main_end;
		}
	} else {
		PRINT_USAGE();
		goto main_end;
	}
	ret = EXIT_SUCCESS;

main_end:

	delete_itc();
	return ret;

}

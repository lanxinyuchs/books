
/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "itc.h"
#include "gpio.h"

#define MAX_MAILBOX_NUM    32


#define USAGE() printf("testcase1  - verify gpio_reserve\n"		\
		       "testcase2  - verify gpio_set_dir\n"		\
		       "testcase3  - a, verify gpio_write\n"		\
		       "             b, verify gpio_read \n"		\
		       "testcase4  - a, verify gpio_reserve with invalid param\n" \
		       "             b, verify gpio_reserve with the pin has been reserved \n" \
		       "testcase5  - a, verify gpio_set_dir with invalid param\n" \
		       "             b, verify gpio_set_dir with wrong state\n" \
		       "testcase6  - a, verify gpio_write with invalid param\n" \
		       "             b, verify gpio_write with wrong state\n" \
		       "testcase7  - a, verify gpio_read with invalid param\n" \
		       "             b, verify gpio_read with wrong state\n" \
		       )



static int testcase1(int argc, char **argv)
{

	(void)argc;
	(void)argv;

	gpio_status_t status;
	gpio_handle_t handle = NULL;
        uint32_t my_pins[] = {0, 31, 53, 54, 117};

         status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
         if(status != GPIO_STATUS_SUCCESS) {
			printf("reserve failed %d\n", status);
                        return 1;
         }
         status = gpio_unreserve(handle);
         if(status != GPIO_STATUS_SUCCESS) {
			printf("unreserve failed %d\n", status);
                        return 1;
         }

	 printf("\n*** gpio test case 1 have passed ***\n");

         return 0;
}

static int testcase2(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	gpio_status_t status;
	gpio_handle_t handle = NULL;
        const uint32_t my_pins[] = {0, 31, 32};
	const uint8_t my_values[] = {1,  1,   1};
	uint8_t *read_values = NULL;
	int ret = 1;

	read_values = calloc(sizeof(my_pins)/sizeof(my_pins[0]), sizeof(uint8_t));
	if(read_values == NULL) {
		printf("calloc failed\n");
		return 1;
	}

	status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("reserve failed %d\n", status);
		goto testcase2_end;

	}

	/* set dir to OUT */
	status = gpio_set_dir(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			      my_pins, my_values);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("Failed to set GPIO direction with status %d.\n", status);
		goto testcase2_end;

	}

	/* get direction */
	/*
	status = gpio_get_dir(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			      my_pins, read_values);
	 if(status != GPIO_STATUS_SUCCESS) {
		 printf("Failed to get GPIO directions with status %d.\n", status);
		 goto testcase2_end;
	 }
	*/

         ret = 0;

testcase2_end:
	 free(read_values);
	 if(handle != NULL) {
		 status = gpio_unreserve(handle);
		 if(status != GPIO_STATUS_SUCCESS) {
			 printf("unreserve failed %d\n", status);
			 ret = 1;
		 }
	 }

	 if (ret == 0)
		printf("\n*** gpio test case 2 have passed ***\n");

	 return ret;
}


static int testcase3(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	gpio_status_t status;
	gpio_handle_t handle = NULL;
        const uint32_t my_pins[] = {26, 27, 28};
	const uint8_t my_values[] = {1,  1,   1};
	uint8_t *read_values = NULL;
	int ret = 1;

	read_values = calloc(sizeof(my_pins)/sizeof(my_pins[0]), sizeof(uint8_t));
	if(read_values == NULL) {
		printf("calloc failed.\n");
		return 1;
	}

	status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("reserve failed %d\n", status);
		goto testcase3_end;
	}

	status = gpio_set_dir(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			      my_pins, my_values);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("Failed to set GPIO direction with status %d.\n", status);
		goto testcase3_end;
	}

	status = gpio_write(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			    my_pins, my_values);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("Failed to write GPIO pin with status %d.\n", status);
		goto testcase3_end;
	}

	status = gpio_read(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			   my_pins, read_values);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("Failed to read GPIO values with status %d.\n", status);
		goto testcase3_end;
	}

	for(uint32_t i = 0; i < sizeof(my_pins)/sizeof(my_pins[0]); i++) {
		if(my_values[i] != read_values[i]) {
			printf("read out pin %d value %d is not as expected value %d.\n",
			       my_pins[i], read_values[i], my_values[i]);
			goto testcase3_end;

		}
	}

	ret = 0;

testcase3_end:
	free(read_values);
	if(handle != NULL) {
		status = gpio_unreserve(handle);
		if(status != GPIO_STATUS_SUCCESS) {
			printf("unreserve failed %d\n", status);
			ret = 1;
		}
	}

	 if (ret == 0)
		printf("\n*** gpio test case 3 have passed ***\n");

	return ret;
}

static int testcase4(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	gpio_status_t status;
	gpio_handle_t handle1 = NULL;
	gpio_handle_t handle2 = NULL;
#ifdef __MACHINE_ZYNQMP_MIMO
	const uint32_t pin1[] = {174};
#else
	const uint32_t pin1[] = {118};
#endif
	const uint32_t pin2[] = {31};
	int ret = 1;

	status = gpio_reserve(sizeof(pin1)/sizeof(pin1[0]), pin1, &handle1);
	if(status != GPIO_STATUS_INVALID_PARAM) {
		printf("Failed: status %d is not as expected GPIO_STATUS_INVALID_PARAM\n",
		       status);
		goto testcase4_end;
	}
	printf("reserve pin %d, handle1 0x%x\n", pin1[0], (uint32_t)handle1);

	status = gpio_reserve(sizeof(pin2)/sizeof(pin2[0]), pin2, &handle1);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("reserve failed %d", status);
		goto testcase4_end;
	}
	printf("reserve pin %d, handle1 0x%x\n", pin2[0], (uint32_t)handle1);

	status = gpio_reserve(sizeof(pin2)/sizeof(pin2[0]), pin2, &handle2);
	if(status != GPIO_STATUS_WRONG_STATE) {
		printf("Failed: status %d is not as expected GPIO_STATUS_WRONG_STATE\n",
		       status);
		goto testcase4_end;
	}
	printf("reserve pin %d, handle2 0x%x\n", pin2[0], (uint32_t)handle2);

	ret = 0;

testcase4_end:
	if(handle1 != NULL) {
		printf("unreserve handle1 0x%x\n", (uint32_t)handle1);
		status = gpio_unreserve(handle1);
		if(status != GPIO_STATUS_SUCCESS) {
			printf("unreserve failed %d\n", status);
			ret = 1;
		}
	}
	if(handle2 != NULL) {
		printf("unreserve handle2 0x%x\n", (uint32_t)handle2);
		status = gpio_unreserve(handle2);
		if(status != GPIO_STATUS_SUCCESS) {
			printf("unreserve failed %d\n", status);
			ret = 1;
		}
	}

	if (ret == 0)
		printf("\n*** gpio test case 4 have passed ***\n");

	return ret;
}

static int testcase5(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	gpio_status_t status;
	gpio_handle_t handle = NULL;
#ifdef __MACHINE_ZYNQMP_MIMO
	const uint32_t my_pins[] = {0, 173};
	const uint32_t invalid_pins[] = {174, 175};
	const uint32_t valid_pins[] = {1, 172};
#else
        const uint32_t my_pins[] = {0, 117};
	const uint32_t invalid_pins[] = {118, 119};
	const uint32_t valid_pins[] = {1, 116};
#endif
	const uint8_t my_values[] = {1,  1};

	int ret = 1;

	/* invalid handle */
	status = gpio_set_dir(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			      my_pins, my_values);
	if(status != GPIO_STATUS_INVALID_PARAM) {
		printf("Failed: set GPIO direction with status %d"
		       " is not as expected GPIO_STATUS_INVALID_PARAM.\n", status);
		return 1;

	}

	status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("reserve failed %d\n", status);
		goto testcase5_end;

	}

	/* invalid pins */
	status = gpio_set_dir(handle, sizeof(invalid_pins)/sizeof(invalid_pins[0]),
			      invalid_pins, my_values);
	if(status != GPIO_STATUS_INVALID_PARAM) {
		printf("Failed: set GPIO directions with status %d"
		       " is not as expected as GPIO_STATUS_INVALID_PARAM.\n", status);
		goto testcase5_end;
	}

	/* valid pins but not reserved */
	status = gpio_set_dir(handle, sizeof(valid_pins)/sizeof(valid_pins[0]),
			      valid_pins, my_values);
	if(status != GPIO_STATUS_WRONG_STATE) {
		printf("Failed: set GPIO directions with status %d"
		       " is not as expected as GPIO_STATUS_WRONG_STATE.\n", status);
		goto testcase5_end;
	}

	ret = 0;

testcase5_end:
	if(handle != NULL) {
		status = gpio_unreserve(handle);
		if(status != GPIO_STATUS_SUCCESS) {
			printf("unreserve failed %d\n", status);
			ret = 1;
		}
	}

	if (ret == 0)
		printf("\n*** gpio test case 5 have passed ***\n");

	return ret;
}

static int testcase6(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	gpio_status_t status;
	gpio_handle_t handle = NULL;
	const uint32_t my_pins[] = {0, 32, 33};
#ifdef __MACHINE_ZYNQMP_MIMO
        const uint32_t invalid_pins[] = {174, 175};
        const uint32_t valid_pins[] = {1, 173};
#else
        const uint32_t invalid_pins[] = {118, 119};
        const uint32_t valid_pins[] = {1, 117};
#endif
	const uint8_t my_values[] = {1,  1};
	int ret = 1;

	/* write my_pins value to 1 */
	status = gpio_write(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			    my_pins, my_values);
	if(status != GPIO_STATUS_INVALID_PARAM) {
		printf("Failed: gpio_write with status %d"
		       " is not as expected GPIO_STATUS_INVALID_PARAM.\n", status);
		return 1;
	}

	/* reserve my_pins  */
	status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("reserve failed %d\n", status);
		return 1;
	}

	/* write invalid_pins value to 1 */
	status = gpio_write(handle, sizeof(invalid_pins)/sizeof(invalid_pins[0]),
			    invalid_pins, my_values);
	if(status != GPIO_STATUS_INVALID_PARAM) {
		printf("Failed: gpio write with status %d "
		       "is not as expected as GPIO_STATUS_INVALID_PARAM.\n", status);
		goto testcase6_end;
	}

	/* write invalid_pins value to 1 */
	status = gpio_write(handle, sizeof(valid_pins)/sizeof(valid_pins[0]),
			    valid_pins, my_values);
	if(status != GPIO_STATUS_WRONG_STATE) {
		printf("Failed: gpio write with status %d "
		       "is not as expected as GPIO_STATUS_WRONG_STATE.\n", status);
		goto testcase6_end;
	}

	ret = 0;

testcase6_end:
	status = gpio_unreserve(handle);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("unreserve failed %d\n", status);
		ret = 1;
	}

	if (ret == 0)
		printf("\n*** gpio test case 6 have passed ***\n");

	return ret;
}

static int testcase7(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	gpio_status_t status;
	gpio_handle_t handle = NULL;
        const uint32_t my_pins[] = {0, 31};
#ifdef __MACHINE_ZYNQMP_MIMO
        const uint32_t invalid_pins[] = {174, 175};
        const uint32_t valid_pins[] = {1, 173};
#else
	const uint32_t invalid_pins[] = {118, 119};
	const uint32_t valid_pins[] = {1, 117};
#endif
	uint8_t *my_values = NULL;
	int ret = 1;
	my_values = calloc(sizeof(my_pins)/sizeof(my_pins[0]), sizeof(uint8_t));

	if(my_values == NULL) {
		printf("calloc failed\n");
		return 1;
	}

	status = gpio_read(handle, sizeof(my_pins)/sizeof(my_pins[0]),
			   my_pins, my_values);
	if(status != GPIO_STATUS_INVALID_PARAM) {
		printf("Failed: gpio_read with status %d"
		       " is not as expected GPIO_STATUS_INVALID_PARAM.\n", status);
		goto testcase7_end;
	}

	status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != GPIO_STATUS_SUCCESS) {
		printf("reserve failed %d\n", status);
		goto testcase7_end;
	}

	status = gpio_read(handle, sizeof(invalid_pins)/sizeof(invalid_pins[0]),
			   invalid_pins, my_values);
	if(status != GPIO_STATUS_INVALID_PARAM) {
		printf("Failed: gpio read with status %d "
		       "is not as expected as GPIO_STATUS_INVALID_PARAM.\n", status);
		goto testcase7_end;
	}

	status = gpio_read(handle, sizeof(valid_pins)/sizeof(valid_pins[0]),
			   valid_pins, my_values);
	if(status != GPIO_STATUS_WRONG_STATE) {
		printf("Failed: gpio read with status %d "
		       "is not as expected as GPIO_STATUS_WRONG_STATE.\n", status);
		goto testcase7_end;
	}

	ret = 0;

testcase7_end:
	free(my_values);
	if(handle != NULL) {
		status = gpio_unreserve(handle);

		if(status != GPIO_STATUS_SUCCESS) {
			printf("unreserve failed %d\n", status);
			ret = 1;

		}
	}

	if (ret == 0)
		printf("\n*** gpio test case 7 have passed ***\n");

	return ret;
}

static void delete_itc(void)
{
	itc_mbox_id_t mbox = itc_current_mbox();
	if( mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}
}

int main(int argc, char **argv)
{
	int ret = 1;
	itc_mbox_id_t mailbox;


	/* Initialize ITC and create mailbox */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	mailbox = itc_create_mailbox("gpio-testcase", 0);
	if(mailbox == ITC_NO_ID) {
		printf("gpio-test mailbox is failed to create\n");
		return ret;
	}
	if(argc != 2) {
		USAGE();
		return ret;
	}

	if(!strcmp(argv[1], "testcase1")) {
               ret = testcase1(argc, argv);
	} else if(!strcmp(argv[1], "testcase2")) {
		ret = testcase2(argc, argv);
	} else if(!strcmp(argv[1], "testcase3")) {
		ret = testcase3(argc, argv);
	} else if(!strcmp(argv[1], "testcase4")) {
		ret = testcase4(argc, argv);
	} else if(!strcmp(argv[1], "testcase5")) {
		ret = testcase5(argc, argv);
	} else if(!strcmp(argv[1], "testcase6")) {
		ret = testcase6(argc, argv);
	} else if(!strcmp(argv[1], "testcase7")) {
		ret = testcase7(argc, argv);
	} else {
		printf("not valid argument\n");
	}

	delete_itc();
	return ret;
}

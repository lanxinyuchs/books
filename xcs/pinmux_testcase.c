/* ---------------------------------------------------------------------------
 *
 * Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include "pinmux.h"
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "itc.h"


#define MAX_MAILBOX_NUM    32

#define PRINT_USAGE()	printf( "-h\n"					\
				"       Display usage information.\n\n"	\
				"testcase1\n"				\
				"       test case to verify pinmux reserve\n" \
				"testcase2\n"				\
				"       test case to verify pinmux set func\n" \
				"testcase3\n"				\
				"       test case to verify pinmux set cfg\n" \
				"testcase4\n"				\
				"       test case to verify pinmux reserve with invalid param\n" \
				"testcase5\n"				\
				"       test case to verify pinmux reserve when the pins have been reserved\n" \
				"testcase6\n"				\
				"       test case to verify pinmux_set_func with invalid param\n" \
				"testcase7\n"				\
				"       test case to verify pinmux_set_cfg with invalid param\n" \
				"testcase8\n"				\
				"       test case to verify pinmux_get_func with invalid param\n" \
				)


static int testcase1(int argc, char **argv)
{

	(void)argc;
	(void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
        uint32_t my_pins[] = {0, 107, 108, 215, 216, 278, 279, 336};

         status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
         if(status != PINMUX_STATUS_SUCCESS) {
			printf("reserve failed %d\n", status);
                        return 1;
         }
         status = pinmux_unreserve(handle);
         if(status != PINMUX_STATUS_SUCCESS) {
			printf("unreserve failed %d\n", status);
                        return 1;
         }

         return 0;
}

static int testcase2(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
	const uint32_t my_pins[] = {0, 107, 108, 215, 216, 278, 279, 336};
	pinmux_func_type_t func_type = PINMUX_FUNC_TYPE_GPIO;

	status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("reserve failed %d\n", status);
		return 1;
	}

	/* set pin 0, 107, 108 func as PINMUX_FUNC_TYPE_ALTF2 */
	status = pinmux_set_func(handle, 3, &my_pins[0], PINMUX_FUNC_TYPE_ALTF2);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: Pin 0 and 107 are set to PINMUX_FUNC_TYPE_ALTF2.\n");
		return 1;
	}

	/* set pin 278, 279, 336 func as PINMUX_FUNC_TYPE_ALTF1 */
	status = pinmux_set_func(handle, sizeof(my_pins)/sizeof(my_pins[0]) - 5,
	                         &my_pins[5], PINMUX_FUNC_TYPE_ALTF1);
	if(status == PINMUX_STATUS_SUCCESS) {
		printf("Failed: Pins are set to PINMUX_FUNC_TYPE_ALTF1.\n");
		return 1;
	}

	status = pinmux_unreserve(handle);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("unreserve failed %d\n", status);
		return 1;
	}

	status = pinmux_get_func(handle, 0, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_ALTF2) {
		printf("pin 0 function type is not ALTF2 as expected\n");
		return 1;
	}

	status = pinmux_get_func(handle, 107, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_ALTF2) {
		printf("pin 0 function type is not ALTF2 as expected\n");
		return 1;
	}

	status = pinmux_get_func(handle, 108, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_ALTF2) {
		printf("pin 0 function type is not ALTF2 as expected\n");
		return 1;
	}

	status = pinmux_get_func(handle, 215, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_GPIO) {
		printf("pin 0 function type is not GPIO as expected\n");
		return 1;
	}
	status = pinmux_get_func(handle, 216, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_GPIO) {
		printf("pin 0 function type is not GPIO as expected\n");
		return 1;
	}

	status = pinmux_get_func(handle, 278, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_ALTF1) {
		printf("pin 0 function type is not ALTF1 as expected\n");
		return 1;
	}
	status = pinmux_get_func(handle, 279, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_ALTF1) {
		printf("pin 0 function type is not ALTF1 as expected\n");
		return 1;
	}
	status = pinmux_get_func(handle, 336, &func_type);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("Failed: get pin0 function failed\n");
		return 1;
	}

	if(func_type != PINMUX_FUNC_TYPE_ALTF1) {
		printf("pin 0 function type is not ALTF1 as expected\n");
		return 1;
	}

	return 0;
}

static int testcase3(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
        const uint32_t my_pins[] = {0, 107, 108, 215, 216, 278, 279, 336};

	status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("reserve failed %d\n", status);
		return 1;
	}

	status = pinmux_set_cfg(handle, 2, &my_pins[0],
				PINMUX_CFG_TYPE_PULLSEL,
				PINMUX_CFG_VALUE_PULLSEL_DOWN);

	if(status != PINMUX_STATUS_SUCCESS){
		printf("My pin's configuration PULLSEL failed to set.\n");
		return 1;
	}

	status = pinmux_set_cfg(handle, 2, &my_pins[2],
				PINMUX_CFG_TYPE_IMPSEL,
				PINMUX_CFG_VALUE_IMPSEL_11);

	if(status != PINMUX_STATUS_SUCCESS){
		printf("My pin's configuration IMPSEL failed to set.\n");
		return 1;
	}

	status = pinmux_set_cfg(handle, 4, &my_pins[4],
				PINMUX_CFG_TYPE_SLEW,
				PINMUX_CFG_VALUE_SLEW_1);

	if(status != PINMUX_STATUS_SUCCESS){
		printf("My pin's configuration SLEW failed to set.\n");
		return 1;
	}

	status = pinmux_unreserve(handle);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("unreserve failed %d\n", status);
		return 1;
	}

         return 0;
}

static int testcase4(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
        const uint32_t my_pins[] = {337};


	status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status == PINMUX_STATUS_INVALID_PARAM) {
		return 0;
	}
	printf("status %d is not as expected as PINMUX_STATUS_INVALID_PARAMS\n", status);
	return 1;
}

static int testcase5(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
        const uint32_t my_pins[] = {0, 107, 108};


	status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != PINMUX_STATUS_SUCCESS) {
		printf("status %d is not as expected as PINMUX_STATUS_SUCCESS\n", status);
		return 1;
	}

	status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins, &handle);
	if(status != PINMUX_STATUS_WRONG_STATE) {
		printf("status %d is not as expected as PINMUX_STATUS_WRONG_STATE\n", status);
		return 1;
	}
	return 0;
}

static int testcase6(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
        const uint32_t my_pins[] = {0, 107, 108};

	status = pinmux_set_func(handle, 3, &my_pins[0], PINMUX_FUNC_TYPE_ALTF2);
	if(status != PINMUX_STATUS_INVALID_PARAM) {
		printf("status %d is not as expected as PINMUX_STATUS_INVALID_PARAM\n", status);
		return 1;
	}
	return 0;

}

static int testcase7(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
        const uint32_t my_pins[] = {0, 107};

	status = pinmux_set_cfg(handle, 2, &my_pins[0],
				PINMUX_CFG_TYPE_PULLSEL,
				PINMUX_CFG_VALUE_PULLSEL_DOWN);

	if(status != PINMUX_STATUS_INVALID_PARAM) {
		printf("status %d is not as expected as PINMUX_STATUS_INVALID_PARAM\n", status);
		return 1;
	}
	return 0;

}

static int testcase8(int argc, char **argv)
{
        (void)argc;
        (void)argv;

	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
	pinmux_func_type_t func_type;

	status = pinmux_get_func(handle, 107, &func_type);
	if(status != PINMUX_STATUS_INVALID_PARAM) {
		printf("status %d is not as expected as PINMUX_STATUS_INVALID_PARAM\n", status);
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

int main(int argc, char **argv)
{
	int ret = 1;

	/* Initialize ITC and create mailbox  */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	itc_mbox_id_t mailbox = itc_create_mailbox("pinmux-testcase", 0);
	if(mailbox == ITC_NO_ID) {
		printf("pinmux-testcase mailbox is failed to create\n");
		return ret;
	}
	if(argc != 2) {
		PRINT_USAGE();
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
	} else if(!strcmp(argv[1], "testcase8")) {
		ret = testcase8(argc, argv);
	} else {
		printf("not valid argument\n");
	}

	delete_itc();
	return ret;


}

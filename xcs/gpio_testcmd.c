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

#define USAGE() printf("argument is invalid\n"                          \
                       "Usage:\n"                                       \
                       "./gpio_test "                                   \
                       "-r <num_of_pins> <p1> [<p2> ...] \n"            \
                       "[<-ur>]\n"                                      \
                       " [<-sd> <num_of_pins> <p1> [<p2> ...]"          \
                       " <p1 dir value> [<p2 dir value> ...]] \n"       \
                       " [<-gd> <num_of_pins> <p1> [<p2> ...]]\n"       \
                       " [<-w> <num_of_pins> <p1> [<p2> ...] "          \
                       "<p1 write value> [<p2 write value> ...]]\n"     \
                       " [<-read> <num_of_pins> <p1> [<p2> ...]]\n\n"   \
                       "Operands:\n"                                    \
                       "-r: reserve\n-sd: set dir\n-gd: get dir\n-w: "\
                       "write\n-ur:unreserve\n"                         \
                       "p1, p2, ...: pin1 number, pin2 number, ...\n\n")

#define PRINT_VALUE(pin_num, pin, value)  print_value(pin_num, pin, value)


static bool parse_param(int argc,
                        char **argv,
                        int *opt_ind,
                        uint32_t *pin_num,
                        uint32_t **pin,
                        uint8_t **value,
                        bool value_from_opt)
{
	uint32_t j;
	if((*opt_ind) >= argc) {
		USAGE();
		return false;
	}
	*pin_num = strtoul(argv[(*opt_ind)++], 0, 0);
	if(*pin_num > (uint32_t)(argc - (*opt_ind))) {
		USAGE();
		return false;
	}
	*pin = (uint32_t *)calloc((*pin_num), sizeof(uint32_t));

	if(*pin == NULL) {
		printf("calloc my_pins failed\n");
		return false;
	}

	for(j = 0; j < *pin_num; j++) {
		(*pin)[j] = strtoul(argv[(*opt_ind)++], 0, 0);
	}

	if(value == NULL) {
		return true;
	}

	if(value_from_opt &&
	    ((*pin_num) > (uint32_t)(argc - (*opt_ind)))) {
		USAGE();
		goto parse_param_error;
	}

	*value = (uint8_t *)calloc(*pin_num, sizeof(uint8_t));
	if(*value == NULL) {
		printf("calloc my_values failed \n");
		return false;
	}
	if(value_from_opt == false) {
		return true;
	}

	for(j = 0; j < *pin_num; j++) {
		(*value)[j] = strtoul(argv[(*opt_ind)++], 0, 0);
	}

	return true;
parse_param_error:
	free(*pin);
	free(*value);
	*pin = NULL;
	*value = NULL;

	return false;
}

static void print_value(uint32_t pin_num, uint32_t pin[], uint8_t value[])
{
	for(uint32_t j = 0; j < pin_num; j++) {
		printf("  pin%d : 0x%x\n", pin[j], (uint32_t)value[j]);
	}
	return;
}

/* This function is not necessary since handle has been
 * checked in gpio_xx function.
 * It's only for cleaning coverity error */
static bool check_handle_valid(gpio_handle_t handle)
{
	if(handle == NULL) {
		printf("handle is NULL");
		return false;
	}
	return true;
}

int main(int argc, char **argv)
{
	gpio_status_t status;
	gpio_handle_t handle = NULL;
	uint32_t pin_num;
	uint32_t *my_pins = NULL;
	uint8_t *my_values = NULL;
	int opt_ind = 1; /* always pointer to next option */
	int ret = EXIT_FAILURE;
	itc_mbox_id_t mailbox;

	/* Initialize ITC and create mailbox */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	mailbox = itc_create_mailbox("gpio-test", 0);
	if(mailbox == ITC_NO_ID) {
		printf("gpio-test mailbox is failed to create\n");
		return ret;
	}
	while(argc > opt_ind) {
		if(!strcmp(argv[opt_ind], "-r")) {
			opt_ind++;
			if(parse_param(argc, argv, &opt_ind,
			               &pin_num, &my_pins,
			               NULL, false) == false) {
				goto main_end;
			}
			status = gpio_reserve(pin_num, my_pins, &handle);
			if(status != GPIO_STATUS_SUCCESS) {
				printf("reserve failed %d\n", status);
				goto main_end;
			}
			printf("pins are reserved 0x%x\n", (uint32_t)handle);
		} else if(!strcmp(argv[opt_ind], "-sd")) {

			opt_ind++;
			if(check_handle_valid(handle) == false) {
				goto main_end;
			}
			if(parse_param(argc, argv, &opt_ind,
			               &pin_num, &my_pins,
			               &my_values, true) == false) {
				goto main_end;
			}

			status = gpio_set_dir(handle, pin_num,
			                      my_pins, my_values);

			if(status != GPIO_STATUS_SUCCESS) {
				printf("set dir failed\n");
				goto main_end;
			}
			printf("pins are set directions successfully\n");
		} else if(!strcmp(argv[opt_ind], "-w")) {
			opt_ind++;
			if(check_handle_valid(handle) == false) {
				goto main_end;
			}
			if(parse_param(argc, argv, &opt_ind,
			               &pin_num, &my_pins,
			               &my_values, true) == false) {
				goto main_end;
			}
			status = gpio_write(handle, pin_num,
			                    my_pins, my_values);
			if(status != GPIO_STATUS_SUCCESS) {
				printf("write failed\n");
				goto main_end;
			}
			printf("pins are written successfully\n");
		} else if((!strcmp(argv[opt_ind], "-gd"))) {
			opt_ind++;
			if(check_handle_valid(handle) == false) {
				goto main_end;
			}
			if(parse_param(argc, argv, &opt_ind,
			               &pin_num, &my_pins,
			               &my_values, false) == false) {
				goto main_end;
			}

			status = gpio_get_dir(handle, pin_num,
			                      my_pins, my_values);
			if(status != GPIO_STATUS_SUCCESS) {
				printf("get dir failed\n");
				goto main_end;
			}
			printf("get pin dir value:\n");
			PRINT_VALUE(pin_num, my_pins, my_values);
		} else if(!strcmp(argv[opt_ind], "-read")) {
			opt_ind++;

			if(check_handle_valid(handle) == false) {
				goto main_end;
			}
			if(parse_param(argc, argv, &opt_ind,
			               &pin_num, &my_pins,
			               &my_values, false) == false) {
				goto main_end;
			}
			status = gpio_read(handle, pin_num, my_pins, my_values);
			if(status != GPIO_STATUS_SUCCESS) {
				printf("read failed\n");
				goto main_end;
			}
			printf("read pin value:\n");
			PRINT_VALUE(pin_num, my_pins, my_values);
		} else if(!strcmp(argv[opt_ind], "-ur")) {

			if(check_handle_valid(handle) == false) {
				goto main_end;
			}
			opt_ind++;
			status = gpio_unreserve(handle);
			handle = NULL;
			if(status != GPIO_STATUS_SUCCESS) {
				printf("unreserve of handle failed\n");
				goto main_end;
			}
			printf("handle is unreserved\n");

		} else {
			USAGE();
			goto main_end;
		}
		free(my_values);
		free(my_pins);
		my_values = NULL;
		my_pins = NULL;
	}
	ret = EXIT_SUCCESS;

main_end:
	if (handle)
		(void) gpio_unreserve(handle);

	itc_delete_mailbox(mailbox);
	free(my_pins);
	free(my_values);
	return ret;
}

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
#include "pinmux.h"
#include "conn-establish-helper.h"
#include "rhd-pinmux-if.h"

#define MAX_MAILBOX_NUM    32

#define CONN_ESTABLISH_TMO      1000
union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_PINMUX_STRUCTS
};

static struct conn_establish_msg_numbers  conn_messages = {
	PINMUX_CONN_ESTABLISH_REQ,
	PINMUX_CONN_ESTABLISH_CFM,
	PINMUX_CONN_ESTABLISH_REJ,
	PINMUX_CONN_DISCONNECT_REQ,
	PINMUX_CONN_DISCONNECT_CFM,
	PINMUX_CONN_DISCONNECT_REJ,
	PINMUX_CONN_MONITOR_FWD
};

static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} pinmux_conn;

static uint32_t client_ref = 0x998;

#define USAGE() printf("argument is invalid.\n"                         \
                       "Usage:\n"                                       \
                       "pinmux_testcmd\n"                               \
                       " [-r <num_of_pins> <p1> [<p2> ...]] \n"         \
                       " [<-ur> <handle>]\n"				\
                       " [<-sfunc> <handle> <num_of_pins> <p1> [<p2> ...]" \
                       " <func_type>]\n"                                \
                       " [<-scfg> <handle> <num_of_pins> <p1> [<p2> ...]" \
                       " <cfg_type> <cfg_value>]\n"                     \
                       " [<-gfunc> <handle> <pin>]\n"			\
                       " [<-gcfg> <handle> <pin> <cfg_type>] \n"	\
                       " [-d] \n\n"					\
                       "Operands:\n"                                    \
                       "-r: reserve\n"                                  \
                       "-ur: unreserve\n"                               \
                       "-sfunc: set function type. \n"                  \
                       "    <func_type>: PINMUX_FUNC_TYPE_GPIO\n"       \
                       "                 PINMUX_FUNC_TYPE_ALTF1\n"      \
                       "                 PINMUX_FUNC_TYPE_ALTF2\n"      \
                       "-scfg: set configuration. \n"                   \
                       "        <cfg_type>        - <cfg_value>: \n"    \
                       "  PINMUX_CFG_TYPE_PULLSEL - PINMUX_CFG_VALUE_PULLSEL_NONE\n" \
                       "                            PINMUX_CFG_VALUE_PULLSEL_UP\n" \
                       "                            PINMUX_CFG_VALUE_PULLSEL_DOWN\n" \
                       "  PINMUX_CFG_TYPE_IMPSEL  - PINMUX_CFG_VALUE_IMPSEL_00\n" \
                       "                            PINMUX_CFG_VALUE_IMPSEL_01\n" \
                       "                            PINMUX_CFG_VALUE_IMPSEL_10\n" \
                       "                            PINMUX_CFG_VALUE_IMPSEL_11\n" \
                       "  PINMUX_CFG_TYPE_SLEW    - PINMUX_CFG_VALUE_SLEW_0\n" \
                       "                            PINMUX_CFG_VALUE_SLEW_1\n" \
                       "-gfunc: get pin's function type\n"              \
                       "-gcfg: get pin's configuration\n"               \
                       "-d: dump all the pinmux related registers' value\n" \
                       " stored at /tmp/pinmux_dump.txt\n")


/* This function is not necessary since handle has been
 * checked in pinmux_xx function.
 * It's only for cleaning coverity error */
static bool check_handle_valid(pinmux_handle_t handle)
{
	if(handle == NULL) {
		printf("handle is NULL");
		return false;
	}
	return true;
}

static bool check_argc(uint32_t argc, uint32_t opt_ind, uint32_t exp_left)
{
	if((argc - opt_ind) < exp_left) {
		USAGE();
		return false;
	}
	return true;
}

static bool parse_pin_from_param(int argc,
                                 char **argv,
                                 int *opt_ind,
                                 uint32_t *pin_num,
                                 uint32_t **pin)
{
	uint32_t j;
	uint32_t tmp_pin_num;
	if((*opt_ind) >= argc) {
		USAGE();
		return false;
	}
	if(pin_num != NULL) {
		*pin_num = strtoul(argv[(*opt_ind)++], 0, 0);
		tmp_pin_num = *pin_num;
		if(*pin_num > (uint32_t)(argc - (*opt_ind))) {
			USAGE();
			return false;
		}
	} else {
		tmp_pin_num = 1;
	}

	*pin = (uint32_t *)calloc((tmp_pin_num), sizeof(uint32_t));

	if(*pin == NULL) {
		printf("calloc my_pins failed\n");
		return false;
	}

	for(j = 0; j < tmp_pin_num; j++) {
		(*pin)[j] = strtoul(argv[(*opt_ind)++], 0, 0);
	}

	return true;
}

static bool parse_func_type(char *argv_str, pinmux_func_type_t *func_type)
{
	bool ret = true;
	if(!strcmp(argv_str, "PINMUX_FUNC_TYPE_GPIO")) {
		*func_type = PINMUX_FUNC_TYPE_GPIO;
	} else if(!strcmp(argv_str, "PINMUX_FUNC_TYPE_ALTF1")) {
		*func_type = PINMUX_FUNC_TYPE_ALTF1;
	} else if(!strcmp(argv_str, "PINMUX_FUNC_TYPE_ALTF2")) {
		*func_type = PINMUX_FUNC_TYPE_ALTF2;
	} else {
		ret = false;
		printf("func_type %s is not valid", argv_str);
	}
	return ret;
}

static bool parse_cfg_type(char *argv_str, pinmux_cfg_type_t *cfg_type)
{
	bool ret = true;
	if(!strcmp(argv_str, "PINMUX_CFG_TYPE_PULLSEL")) {
		*cfg_type = PINMUX_CFG_TYPE_PULLSEL;
	} else if(!strcmp(argv_str, "PINMUX_CFG_TYPE_IMPSEL")) {
		*cfg_type = PINMUX_CFG_TYPE_IMPSEL;
	} else if(!strcmp(argv_str, "PINMUX_CFG_TYPE_SLEW")) {
		*cfg_type = PINMUX_CFG_TYPE_SLEW;
	} else {
		ret = false;
		printf("cfg_type %s is not valid", argv_str);
	}
	return ret;
}

static bool parse_cfg_value(char *argv_str,
                            pinmux_cfg_type_t cfg_type,
                            pinmux_cfg_value_t *cfg_value)
{
	bool ret = true;
	if(cfg_type == PINMUX_CFG_TYPE_PULLSEL) {
		if(!strcmp(argv_str, "PINMUX_CFG_VALUE_PULLSEL_NONE")) {
			*cfg_value = PINMUX_CFG_VALUE_PULLSEL_NONE;
		} else if(!strcmp(argv_str, "PINMUX_CFG_VALUE_PULLSEL_UP")) {
			*cfg_value = PINMUX_CFG_VALUE_PULLSEL_UP;
		} else if(!strcmp(argv_str, "PINMUX_CFG_VALUE_PULLSEL_DOWN")) {
			*cfg_value = PINMUX_CFG_VALUE_PULLSEL_DOWN;
		} else {
			ret = false;
			printf("cfg_value %s is not valid", argv_str);
		}
	} else if(cfg_type == PINMUX_CFG_TYPE_IMPSEL) {
		if(!strcmp(argv_str, "PINMUX_CFG_VALUE_IMPSEL_00")) {
			*cfg_value = PINMUX_CFG_VALUE_IMPSEL_00;
		} else if(!strcmp(argv_str, "PINMUX_CFG_VALUE_IMPSEL_01")) {
			*cfg_value = PINMUX_CFG_VALUE_IMPSEL_01;
		} else if(!strcmp(argv_str, "PINMUX_CFG_VALUE_IMPSEL_10")) {
			*cfg_value = PINMUX_CFG_VALUE_IMPSEL_10;
		} else if(!strcmp(argv_str, "PINMUX_CFG_VALUE_IMPSEL_11")) {
			*cfg_value = PINMUX_CFG_VALUE_IMPSEL_11;
		} else {
			ret = false;
			printf("cfg_value %s is not valid", argv_str);
		}
	} else if(cfg_type == PINMUX_CFG_TYPE_SLEW) {
		if(!strcmp(argv_str, "PINMUX_CFG_VALUE_SLEW_0")) {
			*cfg_value = PINMUX_CFG_VALUE_SLEW_0;
		} else if(!strcmp(argv_str, "PINMUX_CFG_VALUE_SLEW_1")) {
			*cfg_value = PINMUX_CFG_VALUE_SLEW_1;
		} else {
			ret = false;
			printf("cfg_value %s is not valid", argv_str);
		}
	}
	return ret;
}

#define FUNC_TYPE_STR(func_type) func_type_str(func_type)
#define CFG_TYPE_STR(cfg_type) cfg_type_str(cfg_type)
#define CFG_VALUE_STR(cfg_value) cfg_value_str(cfg_value)


static char *cfg_type_str(pinmux_cfg_type_t cfg_type)
{
	switch(cfg_type) {
	case PINMUX_CFG_TYPE_PULLSEL:
		return "PINMUX_CFG_TYPE_PULLSEL";
	case PINMUX_CFG_TYPE_IMPSEL:
		return "PINMUX_CFG_TYPE_IMPSEL";
	case PINMUX_CFG_TYPE_SLEW:
		return "PINMUX_CFG_TYPE_SLEW";
	default:
		return "INVALID";
	}
}

static char *func_type_str(pinmux_func_type_t func_type)
{
	switch(func_type) {
	case PINMUX_FUNC_TYPE_GPIO:
		return "PINMUX_FUNC_TYPE_GPIO";
	case PINMUX_FUNC_TYPE_ALTF1:
		return "PINMUX_FUNC_TYPE_ALTF1";
	case PINMUX_FUNC_TYPE_ALTF2:
		return "PINMUX_FUNC_TYPE_ALTF2";
	default:
		return "INVALID";
	}
}

static char *cfg_value_str(pinmux_cfg_value_t cfg_value)
{
	switch(cfg_value) {
	case PINMUX_CFG_VALUE_PULLSEL_NONE:
		return "PINMUX_CFG_VALUE_PULLSEL_NONE";
	case PINMUX_CFG_VALUE_PULLSEL_UP:
		return "PINMUX_CFG_VALUE_PULLSEL_UP";
	case PINMUX_CFG_VALUE_PULLSEL_DOWN:
		return "PINMUX_CFG_VALUE_PULLSEL_DOWN";
	case PINMUX_CFG_VALUE_IMPSEL_00:
		return "PINMUX_CFG_VALUE_IMPSEL_00";
	case PINMUX_CFG_VALUE_IMPSEL_01:
		return "PINMUX_CFG_VALUE_IMPSEL_01";
	case PINMUX_CFG_VALUE_IMPSEL_10:
		return "PINMUX_CFG_VALUE_IMPSEL_10";
	case PINMUX_CFG_VALUE_IMPSEL_11:
		return "PINMUX_CFG_VALUE_IMPSEL_11";
	case PINMUX_CFG_VALUE_SLEW_0:
		return "PINMUX_CFG_VALUE_SLEW_0";
	case PINMUX_CFG_VALUE_SLEW_1:
		return "PINMUX_CFG_VALUE_SLEW_1";

	default:
		return "INVALID";
	}
}

/****
 *
 *      Function get_mbox
 *
 *****/
static int32_t get_mbox(itc_mbox_id_t *pinmux_mbox)
{
	/* client mailbox */
	if (itc_current_mbox() == ITC_NO_ID) {
		printf("Mailbox doesn't exist.\n");
		return 1;
	}

	/* server mailbox */
	*pinmux_mbox = itc_locate(RHD_PINMUX_MAILBOX);
	if (*pinmux_mbox == ITC_NO_ID) {
		printf("pinmux server is not exist or mailbox %s "
		       "doesn't exist\n",
		       RHD_PINMUX_MAILBOX);
		return 1;
	}
	return 0;
}

static uint32_t pinmux_conn_establish(void)
{
	uint32_t requested_versions[] = {PINMUX_SERVER_VERSIONS};
	uint32_t procedure_ref = 0;

	int ret;
	/* Find client and server mailboxes */
	if (get_mbox(&pinmux_conn.server_mbox) != 0) {
		printf("Client:Cannot find mailbox\n");
		return 1;
	}

	/*Connect to the server*/
	ret = conn_establish(
	            /*input parameters*/
	            pinmux_conn.server_mbox,
	            ++procedure_ref,
	            client_ref,
	            sizeof(requested_versions) / sizeof(requested_versions[0]),
	            requested_versions,
	            &conn_messages,
	            CONN_ESTABLISH_TMO,
	            /*returned values*/
	            &pinmux_conn.server_ref,
	            &pinmux_conn.selected_version);
	if (ret != CONN_ESTABLISH_SUCCESS) {
		printf("Connection establish failed with reason:0x%x\n",
		       ret);
		return 1;

	}


	return 0;
}

static uint32_t msg_check(union itc_msg *msg)
{
	if((msg->any_msg.connection_ref != client_ref)) {
		printf("Client:Server replied with invalid ...._ref; \n"
		       "connection_ref: expected 0x%08x, received 0x%08x\n",
		       client_ref, msg->any_msg.connection_ref);
		itc_free(&msg);
		return 1;
	}
	return 0;
}

/****
 *
 *      Function pinmux_dump
 *
 *****/
static void pinmux_dump(void)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2, RHD_PINMUX_DUMP_CFM, RHD_PINMUX_DUMP_REJ};
	if(pinmux_conn_establish()) {
		return;
	}

	sendmsg = itc_alloc(sizeof(struct pinmux_dump_req),
	                    RHD_PINMUX_DUMP_REQ);

	sendmsg->dump_req.connection_ref = pinmux_conn.server_ref;


	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
	                         pinmux_conn.server_mbox);

	if(msg_check(receivemsg)) {
		return;
	}
	switch(receivemsg->msgno) {
	case RHD_PINMUX_DUMP_CFM:
		printf("Dump successfully.\n"
		       "Result is in /tmp/pinmux_dump.txt\n");
		break;
	case RHD_PINMUX_DUMP_REJ:
		printf("Dump failed.\n");
		break;
	default:
		printf("receive unexpected msg 0x%x\n", receivemsg->msgno);
		break;
	}
	itc_free(&receivemsg);
}

/****
 *
 *      Function pinmux_get_cfg
 *
 *****/
static pinmux_status_t pinmux_get_cfg(pinmux_handle_t handle,
                                      uint32_t pin,
                                      pinmux_cfg_type_t cfg_type,
                                      pinmux_cfg_value_t *cfg_value)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2,
	                        RHD_PINMUX_GET_CFG_CFM,
	                        RHD_PINMUX_GET_CFG_REJ
	                       };
	int32_t ret = 0;

	if(pinmux_conn_establish()) {
		return PINMUX_STATUS_OTHER;
	}

	sendmsg = itc_alloc(sizeof(struct pinmux_get_cfg_req),
	                    RHD_PINMUX_GET_CFG_REQ);

	sendmsg->get_cfg_req.connection_ref = pinmux_conn.server_ref;
	sendmsg->get_cfg_req.handle = handle;
	sendmsg->get_cfg_req.pin = pin;
	sendmsg->get_cfg_req.cfg_type = cfg_type;

	printf("RHD_PINMUX_GET_CFG_REQ, handle:0x%x, pin: %u, cfg_type:%d,"
	       " destination: %u\n", (uint32_t)handle, pin,
	       cfg_type, pinmux_conn.server_mbox);

	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
	                         pinmux_conn.server_mbox);
	if(msg_check(receivemsg)) {
		return PINMUX_STATUS_OTHER;
	}
	switch(receivemsg->msgno) {
	case RHD_PINMUX_GET_CFG_CFM:
		*cfg_value = receivemsg->get_cfg_cfm.cfg_value;
		printf("RHD_PINMUX_GET_CFG_CFM cfg_value: %u from: %u\n",
		       receivemsg->get_cfg_cfm.cfg_value,
		       pinmux_conn.server_mbox);
		ret = PINMUX_STATUS_SUCCESS;
		break;
	case RHD_PINMUX_GET_CFG_REJ:
		ret = receivemsg->get_cfg_rej.error_code;
		printf("RHD_PINMUX_GET_CFG_REJ(Error code: %d) from: %u\n",
		       receivemsg->get_cfg_rej.error_code,
		       pinmux_conn.server_mbox);
		break;

	}
	itc_free(&receivemsg);

	return ret;
}


int main(int argc, char **argv)
{
	pinmux_status_t status;
	pinmux_handle_t handle = NULL;
	uint32_t pin_num;
	uint32_t *my_pins = NULL;
	int opt_ind = 1; /* always pointer to next option */
	int ret = EXIT_FAILURE;
	itc_mbox_id_t mailbox;
	uint32_t need_dead_loop = 0;
	if(daemon(0, 1)) {
		printf("fail to start daemon\n");
		return EXIT_FAILURE;
	}
	/* Initialize ITC and create mailbox  */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	mailbox = itc_create_mailbox("pinmux-test", 0);
	if(mailbox == ITC_NO_ID) {
		printf("pinmux-test mailbox is failed to create\n");
		return ret;
	}
	if(argc < 2) {
		USAGE();
		return EXIT_FAILURE;
	}
	if(!strcmp(argv[opt_ind], "-r")) {
		opt_ind++;
		if(parse_pin_from_param(argc, argv, &opt_ind,
		                        &pin_num, &my_pins) == false) {
			goto main_end;
		}
		status = pinmux_reserve(pin_num, my_pins, &handle);
		if(status != PINMUX_STATUS_SUCCESS) {
			printf("reserve failed %d\n", status);
			goto main_end;
		}
		need_dead_loop = 1;
		printf("pins are reserved 0x%04x\n", (uint32_t)handle);
	} else if(!strcmp(argv[opt_ind], "-sfunc")) {
		pinmux_func_type_t func_type;
		opt_ind++;
		if(check_argc(argc, opt_ind, 4) == false) {
			goto main_end;
		}
		handle = (pinmux_handle_t)strtoul(argv[opt_ind++], 0, 0);
		if(check_handle_valid(handle) == false) {
			goto main_end;
		}
		if(parse_pin_from_param(argc, argv, &opt_ind,
		                        &pin_num, &my_pins) == false) {
			goto main_end;
		}

		if(parse_func_type(argv[opt_ind++], &func_type) == false) {
			goto main_end;
		}
		status = pinmux_set_func(handle, pin_num,
		                         my_pins, func_type);

		if(status != PINMUX_STATUS_SUCCESS) {
			printf("set dir failed\n");
			goto main_end;
		}
		printf("pins are set func type successfully\n");
	} else if(!strcmp(argv[opt_ind], "-scfg")) {
		pinmux_cfg_type_t cfg_type;
		pinmux_cfg_value_t cfg_value = 0;
		opt_ind++;
		if(check_argc(argc, opt_ind, 5) == false) {
			goto main_end;
		}
		handle = (pinmux_handle_t)strtoul(argv[opt_ind++], 0, 0);

		if(check_handle_valid(handle) == false) {
			goto main_end;
		}
		if(parse_pin_from_param(argc, argv, &opt_ind,
		                        &pin_num, &my_pins) == false) {
			goto main_end;
		}


		if(parse_cfg_type(argv[opt_ind++], &cfg_type) == false) {
			goto main_end;
		}
		if(parse_cfg_value(argv[opt_ind++], cfg_type, &cfg_value) ==
		    false) {
			goto main_end;
		}
		status = pinmux_set_cfg(handle, pin_num,
		                        my_pins, cfg_type, cfg_value);

		if(status != PINMUX_STATUS_SUCCESS) {
			printf("set dir failed\n");
			goto main_end;
		}
		printf("pins are set cfg successfully\n");
	} else if((!strcmp(argv[opt_ind], "-gfunc"))) {
		pinmux_func_type_t func_type = PINMUX_FUNC_TYPE_GPIO;
		opt_ind++;
		if(check_argc(argc, opt_ind, 2) == false) {
			goto main_end;
		}
		handle = (pinmux_handle_t)strtoul(argv[opt_ind++], 0, 0);

		if(check_handle_valid(handle) == false) {
			goto main_end;
		}
		if(parse_pin_from_param(argc, argv, &opt_ind,
		                        NULL, &my_pins) == false) {
			goto main_end;
		}
		status = pinmux_get_func(handle, my_pins[0], &func_type);

		if(status != PINMUX_STATUS_SUCCESS) {
			printf("get func failed\n");
			goto main_end;
		}

		printf("pin's func type is %s\n", FUNC_TYPE_STR(func_type));
	} else if((!strcmp(argv[opt_ind], "-gcfg"))) {
		pinmux_cfg_type_t cfg_type;
		pinmux_cfg_value_t cfg_value = 0;
		opt_ind++;
		if(check_argc(argc, opt_ind, 3) == false) {
			goto main_end;
		}
		handle = (pinmux_handle_t)strtoul(argv[opt_ind++], 0, 0);

		if(check_handle_valid(handle) == false) {
			goto main_end;
		}
		if(parse_pin_from_param(argc, argv, &opt_ind,
		                        NULL, &my_pins) == false) {
			goto main_end;
		}

		if(parse_cfg_type(argv[opt_ind++], &cfg_type) == false) {
			goto main_end;
		}
		status = pinmux_get_cfg(handle, my_pins[0],
		                        cfg_type, &cfg_value);

		if(status != PINMUX_STATUS_SUCCESS) {
			printf("get func failed\n");
			goto main_end;
		}

		printf("pin's cfg is %s:%s\n",
		       CFG_TYPE_STR(cfg_type),
		       CFG_VALUE_STR(cfg_value));
	} else if(!strcmp(argv[opt_ind], "-ur")) {
		opt_ind++;
		if(check_argc(argc, opt_ind, 1) == false) {
			goto main_end;
		}
		handle = (pinmux_handle_t)strtoul(argv[opt_ind++], 0, 0);
		if(check_handle_valid(handle) == false) {
			goto main_end;
		}
		status = pinmux_unreserve(handle);
		if(status != PINMUX_STATUS_SUCCESS) {
			printf("unreserve 0x%x failed\n",
			       (uint32_t)handle);
			goto main_end;
		}
		printf("handle 0x%x is unreserved\n", (uint32_t)handle);

	} else if(!strcmp(argv[opt_ind], "-d")) {
		pinmux_dump();
	} else {
		USAGE();
	}
main_end:
	free(my_pins);
	if(need_dead_loop) {
		while(1) {
			sleep(1000);
		}
	}
	itc_delete_mailbox(mailbox);
}

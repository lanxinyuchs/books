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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <itc.h>
#include <uio_helper.h>
#include <pthread.h>
#include "pinmux.h"
#include "rhd-pinmux-if.h"
#include "rhd-common.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER               com_ericsson_xcs_rhd_pinmux
#include "tpt_create.h"
#include "tpt.h"

#define PINMUX_0_LOCK_OFFSET              0x800
#define PINMUX_0_0_SEL_OFFSET             0x840
#define PINMUX_0_0_ALT_FUNC_OFFSET        0x880
#define PINMUX_0_0_PULL_EN_OFFSET         0x8C0
#define PINMUX_0_0_PULL_SEL_OFFSET        0x900
#define PINMUX_0_0_IMPSEL1_OFFSET         0x940
#define PINMUX_0_0_IMPSEL2_OFFSET         0x980
#define PINMUX_0_0_SLEW_OFFSET            0x9C0
#define PINMUX_0_IE                       0xA00

#define EXIT_SIGNAL                       0xDEADBEEF


#define PINMUX_X_LOCK_ADDR(base)                \
	((base) + PINMUX_0_LOCK_OFFSET )
#define PINMUX_X_X_SEL_ADDR(base, subgroup)                     \
	((base) + (subgroup) * 4 + PINMUX_0_0_SEL_OFFSET)
#define PINMUX_X_X_ALT_FUNC_ADDR(base, subgroup)                \
	((base) + (subgroup) * 4 + PINMUX_0_0_ALT_FUNC_OFFSET)
#define PINMUX_X_X_PULL_EN_ADDR(base, subgroup)                 \
	((base) + (subgroup) * 4 + PINMUX_0_0_PULL_EN_OFFSET)
#define PINMUX_X_X_PULL_SEL_ADDR(base, subgroup)                \
	((base) + (subgroup) * 4 + PINMUX_0_0_PULL_SEL_OFFSET)
#define PINMUX_X_X_IMPSEL1_ADDR(base, subgroup)                 \
	((base) + (subgroup) * 4 + PINMUX_0_0_IMPSEL1_OFFSET)
#define PINMUX_X_X_IMPSEL2_ADDR(base, subgroup)                 \
	((base) + (subgroup) * 4 + PINMUX_0_0_IMPSEL2_OFFSET)
#define PINMUX_X_X_SLEW_ADDR(base, subgroup)                    \
	((base) + (subgroup) * 4 + PINMUX_0_0_SLEW_OFFSET)
#define PINMUX_X_IE_ADDR(base)                  \
	((base) + PINMUX_0_IE)


#define _STRINGIFY(s)  #s
#define STRINGIFY(s)   _STRINGIFY(s)

#define NUM_OF_PINS             337
#define NUM_OF_GROUPS           4
#define UIO_PINMUX_PREFIX       "gpio-pinmux"
#define UIO_DEV_NAME_LEN        sizeof(UIO_PINMUX_PREFIX) +             \
	sizeof(STRINGIFY(NUM_OF_GROUPS))                /*e.g "gpio-pinmux0"*/
#define MAX_MAILBOX_NAME_LEN 32

#define GENERATE_PIN_MASK(pin)  1 << ((pin) & 0x1F)
#define GENERATE_PIN_VALUE(register_value, pin)         \
	((register_value) >> ((pin) & 0x1F)) & 0x01

#define MAX(a, b) (((a) > (b)) ? (a) : (b))

#define LOCK_REG_WRITE(lock_reg_addr)  (*(lock_reg_addr) = 0x0)
#define UNLOCK_REG_WRITE(lock_reg_addr)  (*(lock_reg_addr) = 0xDEADBEEF)

#ifdef DEBUG
#define DEBUG_TRACE(reg_addr) debug_trace(reg_addr)
#else /* DEBUG */
#define DEBUG_TRACE(reg_addr) ((void) 0)
#endif /* DEBUG */

/** structure and typedef */
union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_PINMUX_STRUCTS
};

struct conn_establish_msg_numbers pinmux_conn_messages = {
	PINMUX_CONN_ESTABLISH_REQ,
	PINMUX_CONN_ESTABLISH_CFM,
	PINMUX_CONN_ESTABLISH_REJ,
	PINMUX_CONN_DISCONNECT_REQ,
	PINMUX_CONN_DISCONNECT_CFM,
	PINMUX_CONN_DISCONNECT_REJ,
	PINMUX_CONN_MONITOR_FWD
};

struct reg_addr {
	uint32_t *lock;
	uint32_t *sel;
	uint32_t *alt_func;
	uint32_t *pull_en;
	uint32_t *pull_sel;
	uint32_t *impsel1;
	uint32_t *impsel2;
	uint32_t *slew;
	uint32_t *ie;
};

struct pinmux_handle {
	struct pinmux_handle *next;
	volatile struct reg_addr *reg_addrs;
	uint32_t *reserved_masks;
	uint32_t conn_server_ref;
	itc_mbox_id_t client_mbox;
};

struct hw_pin {
	uint32_t key;
	uint32_t group;
	uint32_t subgroup;
	uint32_t pin_num;
	uint32_t pin_mask;
};

struct uio_map_storage {
	UIO_HANDLE_ handle;
	void *base;
};

struct pin_map {
	uint32_t key;
	uint32_t group;
	uint32_t subgroup;
	uint32_t first_pin_number; /* pin number named in sw interface */
	uint32_t last_pin_number;  /* pin number named in sw interface */
	uint32_t first_pin_number_in_group; /* pin number named in hw */
	uint32_t last_pin_number_in_group;  /* pin number named in hw */
};
/*
 * Local Variable Definition
 */

/* the table content meaning see struct gpio_subgroup_map_s
 * Column 0 - C0: key
 * Column 1 - C1: group
 * Column 2 - C2: subgroup
 * Column 3 - C3: first pin number in the subgroup (named in sw interface)
 * Column 4 - C4: last pin number in the subgroup (named in sw interface)
 * Column 5 - C5: first pin number in the subgroup (named in hw)
 * Column 6 - C6: last pin number in the subgroup (named in hw)
 **/
static struct pin_map map_table[] =
/*C0    C1    C2    C3      C4     C5     C6*/
{ {0,    0,    0,     0,     31,     0,     31}, /* GPIO_0_0  - GPIO_0_31 */
  {1,    0,    1,    32,     63,    32,     63}, /* GPIO_0_32 - GPIO_0_63 */
  {2,    0,    2,    64,     95,    64,     95}, /* GPIO_0_64 - GPIO_0_95 */
  {3,    0,    3,    96,    107,    96,    107}, /* GPIO_0_96 - GPIO_0_107 */
  {4,    1,    0,    108,   139,     0,     31}, /* GPIO_1_0  - GPIO_1_31 */
  {5,    1,    1,    140,   171,    32,     63}, /* GPIO_1_32 - GPIO_1_63 */
  {6,    1,    2,    172,   203,    64,     95}, /* GPIO_1_64 - GPIO_1_95 */
  {7,    1,    3,    204,   215,    96,    107}, /* GPIO_1_96 - GPIO_1_107 */
  {8,    2,    0,    216,   247,     0,     31}, /* GPIO_2_0  - GPIO_2_31 */
  {9,    2,    1,    248,   278,    32,     62}, /* GPIO_2_32 - GPIO_2_62 */
  {10,   3,    0,    279,   310,     0,     31}, /* GPIO_3_0  - GPIO_3_31 */
  {11,   3,    1,    311,   336,    32,     57}  /* GPIO_3_32 - GPIO_3_57 */
};

static struct uio_map_storage uio_map_storage[NUM_OF_GROUPS] = {
	{NULL, NULL},
	{NULL, NULL},
	{NULL, NULL},
	{NULL, NULL}
};

static struct pinmux_handle_list {
	uint32_t *all_reserved_masks;  /* all the reserved pins' masks */
	struct pinmux_handle *head;
} pinmux_handle_list = {NULL, NULL};

static itc_mbox_id_t pinmux_mbox = ITC_NO_ID;


static pinmux_status_t validate_param(uint32_t number_of_pins,
                                      uint32_t pin[],
                                      pinmux_handle_t handle,
                                      bool check_handle,
                                      pinmux_func_type_t func_type,
                                      bool check_func_type,
                                      pinmux_cfg_type_t cfg_type,
                                      bool check_cfg_type,
                                      pinmux_cfg_value_t cfg_value,
                                      bool check_cfg_value);
static bool validate_handle(pinmux_handle_t handle,
                            pinmux_handle_t *pre_handle);
static void free_handle(pinmux_handle_t handle);
static void add_handle_to_list(pinmux_handle_t handle);
static void remove_handle_from_list(pinmux_handle_t handle,
                                    pinmux_handle_t pre_handle);
static pinmux_status_t generate_all_pin_masks(pinmux_handle_t handle,
                uint32_t number_of_pins,
                uint32_t pin[],
                uint32_t **pin_mask);
static bool pin_to_hw_pin(uint32_t pin, struct hw_pin *hw_pin);
static bool setup_reg_addr(void *base,
                           uint32_t subgroup,
                           volatile struct reg_addr *reg_addrs);
static void enable_ie(volatile struct reg_addr reg_addr[]);
static void set_func_type_reg(volatile struct reg_addr reg_addr,
                              uint32_t pin_mask,
                              pinmux_func_type_t func_type);
static pinmux_status_t set_func_type(pinmux_handle_t handle,
                                     uint32_t number_of_pins,
                                     uint32_t pin[],
                                     pinmux_func_type_t func_type);
static pinmux_func_type_t
read_func_type_from_reg(volatile struct reg_addr reg_addr,
                        uint32_t pin);
static pinmux_status_t get_func_type(pinmux_handle_t handle,
                                     uint32_t pin,
                                     pinmux_func_type_t *func_type);
static  pinmux_status_t
read_cfg_value_from_reg(volatile struct reg_addr reg_addr,
                        uint32_t pin,
                        pinmux_cfg_type_t cfg_type,
                        pinmux_cfg_value_t *cfg_value);
static pinmux_status_t get_cfg_value(pinmux_handle_t handle,
                                     uint32_t pin,
                                     pinmux_cfg_type_t cfg_type,
                                     pinmux_cfg_value_t *cfg_value);
static void set_pullsel_value(volatile struct reg_addr reg_addr,
                              uint32_t pin_mask,
                              pinmux_cfg_value_t cfg_value);
static void set_impsel_value(volatile struct reg_addr reg_addr,
                             uint32_t pin_mask,
                             pinmux_cfg_value_t cfg_value);
static void set_slew_value(volatile struct reg_addr reg_addr,
                           uint32_t pin_mask,
                           pinmux_cfg_value_t cfg_value);
static void set_cfg_value(volatile struct reg_addr reg_addr,
                          uint32_t pin_mask,
                          pinmux_cfg_type_t cfg_type,
                          pinmux_cfg_value_t cfg_value);
static pinmux_status_t set_cfg(pinmux_handle_t handle,
                               uint32_t number_of_pins,
                               uint32_t pin[],
                               pinmux_cfg_type_t cfg_type,
                               pinmux_cfg_value_t cfg_value);
static pinmux_status_t do_reserve(uint32_t number_of_pins,
                                  uint32_t pin[],
                                  pinmux_handle_t handle,
                                  itc_mbox_id_t client_mbox);
static void get_mailbox_name(uint32_t hw_pin,
                             uint32_t key,
                             char mbox_name[],
                             pinmux_handle_t *handle);

#ifdef DEBUG
static void debug_trace(volatile struct reg_addr reg_addrs)
{
	TPT_TRACE(4, STR("register sel 0x%x: 0x%x",
	                 reg_addrs.sel, *(reg_addrs.sel)));
	TPT_TRACE(4, STR("register alt_func 0x%x: 0x%x",
	                 reg_addrs.alt_func, *(reg_addrs.alt_func)));
	TPT_TRACE(4, STR("register pull_en 0x%x: 0x%x",
	                 reg_addrs.pull_en, *(reg_addrs.pull_en)));
	TPT_TRACE(4, STR("register pull_sel 0x%x: 0x%x",
	                 reg_addrs.pull_sel, *(reg_addrs.pull_sel)));
	TPT_TRACE(4, STR("register impsel1 0x%x: 0x%x",
	                 reg_addrs.impsel1, *(reg_addrs.impsel1)));
	TPT_TRACE(4, STR("register impsel2 0x%x: 0x%x",
	                 reg_addrs.impsel2, *(reg_addrs.impsel2)));
	TPT_TRACE(4, STR("register slew 0x%x: 0x%x",
	                 reg_addrs.slew, *(reg_addrs.slew)));
	TPT_TRACE(4, STR("register ie 0x%x: 0x%x",
	                 reg_addrs.ie, *(reg_addrs.ie)));
	TPT_TRACE(4, STR("register lock 0x%x: 0x%x",
	                 reg_addrs.lock, *(reg_addrs.lock)));
	return;
}
#endif

static void pin_str(uint32_t number_of_pins,
                    const uint32_t pin[],
                    char *trace_str)
{
	char *tmp_str = NULL;
	int size = 0;
	tmp_str = trace_str;
	for(uint32_t i = 0; i < number_of_pins; i++) {
		size = snprintf(tmp_str, sizeof("999 "), "%d ", pin[i]);
		tmp_str = tmp_str + size;
	}
}

static pinmux_status_t trace_pin_number(uint32_t number_of_pins, uint32_t pin[])
{
	char *pinstr = NULL;

	pinstr = calloc(number_of_pins, sizeof("999 "));
	if(pinstr == NULL) {
		TPT_ERROR(STR("calloc size %d failed",
		              number_of_pins * sizeof("999 ")));
		return PINMUX_STATUS_OTHER;

	}
	pin_str(number_of_pins, pin, pinstr);
	TPT_TRACE(1, STR("request pin: %s", pinstr));
	free(pinstr);
	return PINMUX_STATUS_SUCCESS;
}

static pinmux_status_t validate_param(uint32_t number_of_pins,
                                      uint32_t pin[],
                                      pinmux_handle_t handle,
                                      bool check_handle,
                                      pinmux_func_type_t func_type,
                                      bool check_func_type,
                                      pinmux_cfg_type_t cfg_type,
                                      bool check_cfg_type,
                                      pinmux_cfg_value_t cfg_value,
                                      bool check_cfg_value)
{
	pinmux_cfg_value_t max_cfg_value = PINMUX_CFG_VALUE_PULLSEL_NONE;
	pinmux_cfg_value_t min_cfg_value = PINMUX_CFG_VALUE_PULLSEL_NONE;

	TPT_TRACE(1, STR("request number_of_pins: %u", number_of_pins));

	if(number_of_pins == 0 || number_of_pins > NUM_OF_PINS) {
		TPT_ERROR(STR("request number_of_pins %d is "
		              "out of range [1, %d]",
		              number_of_pins, NUM_OF_PINS));
		return PINMUX_STATUS_INVALID_PARAM;
	}

	if(trace_pin_number(number_of_pins, pin) != PINMUX_STATUS_SUCCESS) {
		return PINMUX_STATUS_OTHER;
	}

	for(uint32_t i = 0; i < number_of_pins; i++) {
		if(pin[i] > (NUM_OF_PINS - 1)) {
			TPT_ERROR(STR("request pin %d is out of range [0, %d]",
			              pin[i], NUM_OF_PINS - 1));
			return PINMUX_STATUS_INVALID_PARAM;
		}
	}

	if(check_handle == true) {
		TPT_TRACE(1, STR("request handle: 0x%x", (unsigned int)handle));
		if(validate_handle(handle, NULL) == false) {
			TPT_ERROR("request handle is invalid");
			return PINMUX_STATUS_INVALID_PARAM;
		}
	}


	if(check_func_type == true) {
		TPT_TRACE(1, STR("request func_type: %d", func_type));

		if(func_type > PINMUX_FUNC_TYPE_ALTF2) {
			TPT_ERROR(STR("request function type %d is invalid",
			              func_type));
			return PINMUX_STATUS_INVALID_PARAM;
		}
	}

	if(check_cfg_type == true) {
		TPT_TRACE(1, STR("request cfg_type: %d", cfg_type));

		switch(cfg_type) {
		case PINMUX_CFG_TYPE_PULLSEL:
			min_cfg_value = PINMUX_CFG_VALUE_PULLSEL_NONE;
			max_cfg_value = PINMUX_CFG_VALUE_PULLSEL_DOWN;
			break;
		case PINMUX_CFG_TYPE_IMPSEL:
			min_cfg_value = PINMUX_CFG_VALUE_IMPSEL_00;
			max_cfg_value = PINMUX_CFG_VALUE_IMPSEL_11;
			break;
		case PINMUX_CFG_TYPE_SLEW:
			min_cfg_value = PINMUX_CFG_VALUE_SLEW_0;
			max_cfg_value = PINMUX_CFG_VALUE_SLEW_1;
			break;
		default:
			TPT_ERROR(STR("request invalid configuration type %d",
			              cfg_type));
			return PINMUX_STATUS_INVALID_PARAM;
		}
	}

	if(check_cfg_value == true) {
		TPT_TRACE(1, STR("request cfg_value: %d", cfg_value));

		if(cfg_value > max_cfg_value || cfg_value < min_cfg_value) {
			TPT_ERROR(STR("request invalid configuration value %d "
			              "for type %d", cfg_value, cfg_type));
			return PINMUX_STATUS_INVALID_PARAM;
		}
	}

	return PINMUX_STATUS_SUCCESS;
}

static bool validate_handle(pinmux_handle_t handle,
                            pinmux_handle_t *pre_handle)
{
	pinmux_handle_t tmp_pre_handle = NULL;
	pinmux_handle_t tmp_handle = pinmux_handle_list.head;

	while(tmp_handle != NULL) {
		if(tmp_handle == handle) {
			break;
		}

		tmp_pre_handle = tmp_handle;
		tmp_handle = tmp_handle->next;
	}

	if(tmp_handle != NULL) {
		if(pre_handle != NULL) {
			/* return predecessor handle */
			*pre_handle = tmp_pre_handle;
		}
		return true;
	}
	TPT_INFO(STR("Warning: handle 0x%x does not exist in the list",
	             (unsigned int)handle));
	return false;
}

static pinmux_status_t init_handle(pinmux_handle_t *handle)
{
	uint32_t number_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);

	*handle = (pinmux_handle_t)malloc(sizeof(struct pinmux_handle));
	if(*handle == NULL) {
		TPT_ERROR(STR("malloc handle failed %d",
		              sizeof(struct pinmux_handle)));
		goto init_handle_end;
	}

	/** init reg_addrs */
	(*handle)->reg_addrs = (volatile struct reg_addr *)
	           calloc(number_of_subgroups, sizeof(struct reg_addr));

	if((*handle)->reg_addrs == NULL) {
		TPT_ERROR(STR("calloc reg_addrs failed %d",
		              number_of_subgroups * sizeof(struct reg_addr)));
		goto init_handle_end;
	}

	/** init reserved_masks */
	(*handle)->reserved_masks =
	        (uint32_t *)calloc(number_of_subgroups, sizeof(uint32_t));

	if((*handle)->reserved_masks == NULL) {
		TPT_ERROR(STR("calloc reserved_masks failed %d",
		              number_of_subgroups * sizeof(uint32_t)));
		goto init_handle_end;
	}

	return PINMUX_STATUS_SUCCESS;
init_handle_end:
	free_handle(*handle);
	*handle = NULL;
	return PINMUX_STATUS_OTHER;
}


static void free_handle(pinmux_handle_t handle)
{
	if(handle != NULL) {
		free((void *)handle->reg_addrs);
		free(handle->reserved_masks);
		free(handle);
	}
}

static void add_handle_to_list(pinmux_handle_t handle)
{
	/* add handle to the tail of the list */
	handle->next = NULL;

	if(pinmux_handle_list.head == NULL) {
		pinmux_handle_list.head = handle;
	} else {
		pinmux_handle_t tmp_handle;

		for(tmp_handle = pinmux_handle_list.head;
		    tmp_handle->next != NULL;
		    tmp_handle = tmp_handle->next) {
			;
		}

		tmp_handle->next = handle;
	}
	/* add reserved masks into list all_reserved_masks */
	for(uint32_t i = 0; i < sizeof(map_table) / sizeof(map_table[0]); i++) {
		pinmux_handle_list.all_reserved_masks[i] |=
		        handle->reserved_masks[i];
	}

}

static void remove_handle_from_list(pinmux_handle_t handle,
                                    pinmux_handle_t pre_handle)
{
	if(handle == pinmux_handle_list.head) {
		pinmux_handle_list.head = handle->next;
	} else {
		pre_handle->next = handle->next;
	}
	handle->next = NULL;

	/* remove reserved masks from list all_reserved_masks */
	for(uint32_t i = 0; i < sizeof(map_table) / sizeof(map_table[0]); i++) {
		pinmux_handle_list.all_reserved_masks[i] &=
		        ~handle->reserved_masks[i];
	}
}


static pinmux_status_t generate_all_pin_masks(pinmux_handle_t handle,
                uint32_t number_of_pins,
                uint32_t pin[],
                uint32_t **pin_mask)
{
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;
	struct hw_pin hw_pin;
	uint32_t num_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);

	/** init pin_mask */
	*pin_mask = (uint32_t *)calloc(num_of_subgroups, sizeof(uint32_t));
	if(*pin_mask == NULL) {
		TPT_ERROR(STR("calloc pin_mask failed %d",
		              num_of_subgroups * sizeof(uint32_t)));
		ret = PINMUX_STATUS_OTHER;
		goto generate_all_pin_masks_error;
	}

	for(uint32_t i = 0; i < number_of_pins; i++) {
		uint32_t tmp_mask = 0;

		if(pin_to_hw_pin(pin[i], &hw_pin) == false) {
			ret = PINMUX_STATUS_INVALID_PARAM;
			goto generate_all_pin_masks_error;
		}

		tmp_mask = GENERATE_PIN_MASK(hw_pin.pin_num);

		if((handle->reserved_masks[hw_pin.key] & tmp_mask) == 0) {
			TPT_ERROR(STR("pin %d is not reserved", pin[i]));
			ret =  PINMUX_STATUS_WRONG_STATE;
			goto generate_all_pin_masks_error;
		}

		(*pin_mask)[hw_pin.key] |= tmp_mask;

		TPT_TRACE(3, STR("pin_mask 0x%x", (*pin_mask)[hw_pin.key]));

	}
	return ret;
generate_all_pin_masks_error:
	free(*pin_mask);
	return ret;
}

static bool pin_to_hw_pin(uint32_t pin, struct hw_pin *hw_pin)
{
	for(uint32_t i = 0; i < sizeof(map_table) / sizeof(map_table[0]); i++) {
		if(map_table[i].last_pin_number >= pin) {
			hw_pin->group = map_table[i].group;
			hw_pin->subgroup = map_table[i].subgroup;
			hw_pin->pin_num = pin - map_table[i].first_pin_number +
			                 map_table[i].first_pin_number_in_group;
			hw_pin->pin_mask = GENERATE_PIN_MASK(hw_pin->pin_num);
			hw_pin->key = map_table[i].key;
			return true;
		}
	}
	return false;
}

static void enable_ie(volatile struct reg_addr reg_addr[])
{
	for(uint32_t i = 0; i < sizeof(map_table) / sizeof(map_table[0]); i++) {
		if((reg_addr[i].ie != 0) && (*(reg_addr[i].ie) == 0)) {
			UNLOCK_REG_WRITE(reg_addr[i].lock);
			*(reg_addr[i].ie) = 1;
			LOCK_REG_WRITE(reg_addr[i].lock);
			DEBUG_TRACE(reg_addr[i]);
		}

	}
}

static void set_func_type_reg(volatile struct reg_addr reg_addr,
                              uint32_t pin_mask,
                              pinmux_func_type_t func_type)
{
	UNLOCK_REG_WRITE(reg_addr.lock);

	switch(func_type) {

	case PINMUX_FUNC_TYPE_GPIO:
		*(reg_addr.sel) |= pin_mask;
		break;
	case PINMUX_FUNC_TYPE_ALTF1:
		*(reg_addr.sel) &= ~pin_mask;
		*(reg_addr.alt_func) &= ~pin_mask;
		break;
	case PINMUX_FUNC_TYPE_ALTF2:
		*(reg_addr.sel) &= ~pin_mask;
		*(reg_addr.alt_func) |= pin_mask;
		break;
	default:
		/* impossible to happen,
		 * because it has been checked in validate_param */
		break;
	}
	LOCK_REG_WRITE(reg_addr.lock);

}

static pinmux_status_t set_func_type(pinmux_handle_t handle,
                                     uint32_t number_of_pins,
                                     uint32_t pin[],
                                     pinmux_func_type_t func_type)
{
	uint32_t *pin_mask = NULL;
	uint32_t num_of_subgroups = 0;
	pinmux_status_t ret;

	ret = generate_all_pin_masks(handle, number_of_pins, pin, &pin_mask);
	if(ret != PINMUX_STATUS_SUCCESS) {
		return ret;
	}
	num_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);
	for(uint32_t i = 0; i < num_of_subgroups; i++) {
		if(pin_mask[i] != 0) {
			set_func_type_reg(handle->reg_addrs[i],
			                  pin_mask[i], func_type);
			DEBUG_TRACE(handle->reg_addrs[i]);
		}
	}


	free(pin_mask);
	return PINMUX_STATUS_SUCCESS;
}

static pinmux_func_type_t
read_func_type_from_reg(volatile struct reg_addr reg_addr,
                        uint32_t pin)
{
	uint32_t sel_value = GENERATE_PIN_VALUE(*(reg_addr.sel), pin);
	uint32_t alt_func_value = GENERATE_PIN_VALUE(*(reg_addr.alt_func), pin);
	if(sel_value == 1) {
		return PINMUX_FUNC_TYPE_GPIO;
	} else {
		if(alt_func_value == 1) {
			return PINMUX_FUNC_TYPE_ALTF2;
		} else {
			return PINMUX_FUNC_TYPE_ALTF1;
		}
	}
}

static pinmux_status_t get_func_type(pinmux_handle_t handle,
                                     uint32_t pin,
                                     pinmux_func_type_t *func_type)
{
	uint32_t pin_mask;
	struct hw_pin hw_pin;

	if(pin_to_hw_pin(pin, &hw_pin) == false) {
		return PINMUX_STATUS_INVALID_PARAM;
	}

	pin_mask = GENERATE_PIN_MASK(hw_pin.pin_num);

	if((handle->reserved_masks[hw_pin.key] & pin_mask) == 0) {
		TPT_ERROR(STR("pin %d is not reserved", pin));
		return  PINMUX_STATUS_WRONG_STATE;
	}

	*func_type = read_func_type_from_reg(handle->reg_addrs[hw_pin.key],
	                                     hw_pin.pin_num);
	DEBUG_TRACE(handle->reg_addrs[hw_pin.key]);


	return PINMUX_STATUS_SUCCESS;
}

static  pinmux_status_t
read_cfg_value_from_reg(volatile struct reg_addr reg_addr,
                        uint32_t pin,
                        pinmux_cfg_type_t cfg_type,
                        pinmux_cfg_value_t *cfg_value)
{
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;
	switch(cfg_type) {
	case PINMUX_CFG_TYPE_PULLSEL: {
		uint32_t pull_en_value =
		        GENERATE_PIN_VALUE(*(reg_addr.pull_en), pin);
		uint32_t pull_sel_value =
		        GENERATE_PIN_VALUE(*(reg_addr.pull_sel), pin);
		if (pull_en_value == 0) {
			*cfg_value = PINMUX_CFG_VALUE_PULLSEL_NONE;
		} else if(pull_sel_value == 1) {
			*cfg_value = PINMUX_CFG_VALUE_PULLSEL_UP;
		} else {
			*cfg_value = PINMUX_CFG_VALUE_PULLSEL_DOWN;
		}
		break;
	}
	case  PINMUX_CFG_TYPE_IMPSEL: {
		uint32_t impsel1_value =
		        GENERATE_PIN_VALUE(*(reg_addr.impsel1), pin);
		uint32_t impsel2_value =
		        GENERATE_PIN_VALUE(*(reg_addr.impsel2), pin);

		if((impsel1_value == 0) && (impsel2_value == 0)) {
			*cfg_value =  PINMUX_CFG_VALUE_IMPSEL_00;
		} else if((impsel1_value == 0) && (impsel2_value == 1)) {
			*cfg_value = PINMUX_CFG_VALUE_IMPSEL_10;
		} else if((impsel1_value == 1) && (impsel2_value == 0)) {
			*cfg_value = PINMUX_CFG_VALUE_IMPSEL_01;
		} else {
			*cfg_value = PINMUX_CFG_VALUE_IMPSEL_11;
		}
		break;
	}
	case PINMUX_CFG_TYPE_SLEW: {
		uint8_t slew_value = GENERATE_PIN_VALUE(*(reg_addr.slew), pin);
		if(slew_value == 0) {
			*cfg_value =  PINMUX_CFG_VALUE_SLEW_0;
		} else {
			*cfg_value =  PINMUX_CFG_VALUE_SLEW_1;
		}
		break;
	}
	default: {
		/* Impossible to happen since validate_param
		 * has done the check */
		ret = PINMUX_STATUS_INVALID_PARAM;
		break;
	}
	}
	return ret;
}

static pinmux_status_t get_cfg_value(pinmux_handle_t handle,
                                     uint32_t pin,
                                     pinmux_cfg_type_t cfg_type,
                                     pinmux_cfg_value_t *cfg_value)
{
	uint32_t pin_mask;
	struct hw_pin hw_pin;


	if(pin_to_hw_pin(pin, &hw_pin) == false) {
		return PINMUX_STATUS_INVALID_PARAM;
	}

	pin_mask = GENERATE_PIN_MASK(hw_pin.pin_num);

	if((handle->reserved_masks[hw_pin.key] & pin_mask) == 0) {
		TPT_ERROR(STR("pin %d is not reserved", pin));
		return  PINMUX_STATUS_WRONG_STATE;
	}
	DEBUG_TRACE(handle->reg_addrs[hw_pin.key]);

	return read_cfg_value_from_reg(handle->reg_addrs[hw_pin.key],
	                               hw_pin.pin_num, cfg_type, cfg_value);

}

static void set_pullsel_value(volatile struct reg_addr reg_addr,
                              uint32_t pin_mask,
                              pinmux_cfg_value_t cfg_value)
{
	UNLOCK_REG_WRITE(reg_addr.lock);

	switch(cfg_value) {
	case PINMUX_CFG_VALUE_PULLSEL_NONE:
		*(reg_addr.pull_en) &= ~pin_mask;
		break;
	case  PINMUX_CFG_VALUE_PULLSEL_UP:
		*(reg_addr.pull_en) |= pin_mask;
		*(reg_addr.pull_sel) |= pin_mask;
		break;
	case PINMUX_CFG_VALUE_PULLSEL_DOWN:
		*(reg_addr.pull_en) |= pin_mask;
		*(reg_addr.pull_sel) &= ~pin_mask;
		break;
	default:
		break; /* Impossible to happen */
	}
	LOCK_REG_WRITE(reg_addr.lock);
}

static void set_impsel_value(volatile struct reg_addr reg_addr,
                             uint32_t pin_mask,
                             pinmux_cfg_value_t cfg_value)
{
	UNLOCK_REG_WRITE(reg_addr.lock);

	switch(cfg_value) {
	case PINMUX_CFG_VALUE_IMPSEL_00:
		*(reg_addr.impsel1) &= ~pin_mask;
		*(reg_addr.impsel2) &= ~pin_mask;
		break;
	case  PINMUX_CFG_VALUE_IMPSEL_10:
		*(reg_addr.impsel1) &= ~pin_mask;
		*(reg_addr.impsel2) |= pin_mask;
		break;
	case PINMUX_CFG_VALUE_IMPSEL_01:
		*(reg_addr.impsel1) |= pin_mask;
		*(reg_addr.impsel2) &= ~pin_mask;
		break;
	case PINMUX_CFG_VALUE_IMPSEL_11:
		*(reg_addr.impsel1) |= pin_mask;
		*(reg_addr.impsel2) |= pin_mask;
	default:
		break; /* Impossible to happen */
	}
	LOCK_REG_WRITE(reg_addr.lock);
}

static void set_slew_value(volatile struct reg_addr reg_addr,
                           uint32_t pin_mask,
                           pinmux_cfg_value_t cfg_value)
{
	UNLOCK_REG_WRITE(reg_addr.lock);

	switch(cfg_value) {
	case PINMUX_CFG_VALUE_SLEW_0:
		*(reg_addr.slew) &= ~pin_mask;
		break;
	case  PINMUX_CFG_VALUE_SLEW_1:
		*(reg_addr.slew) |= pin_mask;
		break;
	default:
		break; /* Impossible to happen */
	}
	LOCK_REG_WRITE(reg_addr.lock);
}

static void set_cfg_value(volatile struct reg_addr reg_addr,
                          uint32_t pin_mask,
                          pinmux_cfg_type_t cfg_type,
                          pinmux_cfg_value_t cfg_value)
{

	switch(cfg_type) {

	case PINMUX_CFG_TYPE_PULLSEL:
		set_pullsel_value(reg_addr,
		                  pin_mask,
		                  cfg_value);
		break;
	case PINMUX_CFG_TYPE_IMPSEL:
		set_impsel_value(reg_addr,
		                 pin_mask,
		                 cfg_value);
		break;
	case PINMUX_CFG_TYPE_SLEW:
		set_slew_value(reg_addr,
		               pin_mask,
		               cfg_value);
		break;
	default:
		/* impossible to happen,
		 * because it has been checked in validate_param */
		break;
	}
}

static pinmux_status_t set_cfg(pinmux_handle_t handle,
                               uint32_t number_of_pins,
                               uint32_t pin[],
                               pinmux_cfg_type_t cfg_type,
                               pinmux_cfg_value_t cfg_value)
{
	uint32_t *pin_mask = NULL;
	uint32_t num_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);
	pinmux_status_t ret;

	ret = generate_all_pin_masks(handle, number_of_pins, pin, &pin_mask);
	if(ret != PINMUX_STATUS_SUCCESS) {
		return ret;
	}
	for(uint32_t i = 0; i < num_of_subgroups; i++) {
		if(pin_mask[i] == 0) {
			continue;
		}
		set_cfg_value(handle->reg_addrs[i],
		              pin_mask[i],
		              cfg_type,
		              cfg_value);
		DEBUG_TRACE(handle->reg_addrs[i]);

	}
	free(pin_mask);

	return PINMUX_STATUS_SUCCESS;
}

static pinmux_status_t do_reserve(uint32_t number_of_pins,
                                  uint32_t pin[],
                                  pinmux_handle_t handle,
                                  itc_mbox_id_t client_mbox)
{
	struct hw_pin hw_pin;

	for(uint32_t i = 0; i < number_of_pins; i++) {
		uint32_t tmp_mask = 0;
		if(pin_to_hw_pin(pin[i], &hw_pin) == false) {
			return PINMUX_STATUS_INVALID_PARAM;
		}
		tmp_mask = GENERATE_PIN_MASK(hw_pin.pin_num);
		if(pinmux_handle_list.all_reserved_masks[hw_pin.key] &
		    tmp_mask) {
			char mbox_name[MAX_MAILBOX_NAME_LEN];
			char reserved_mbox_name[MAX_MAILBOX_NAME_LEN];
			mbox_name[0] = '\0';
			reserved_mbox_name[0] = '\0';
			itc_get_name(client_mbox, mbox_name,
			             sizeof(mbox_name));
			get_mailbox_name(hw_pin.pin_num, hw_pin.key,
			                 reserved_mbox_name, NULL);
			TPT_ERROR(STR("Failed to reserve pin %d for %s, "
			              "it is already reserved by %s.",
			              pin[i], mbox_name, reserved_mbox_name));
			return PINMUX_STATUS_WRONG_STATE;
		}

		if(!setup_reg_addr(uio_map_storage[hw_pin.group].base,
		                   hw_pin.subgroup,
		                   &handle->reg_addrs[hw_pin.key])) {
			return PINMUX_STATUS_OTHER;
		}
		handle->reserved_masks[hw_pin.key] |=
		        GENERATE_PIN_MASK(hw_pin.pin_num);
	}
	/* According to ASIC designer,
	 * it doesn't matter to enable IE register before configuration
	 * setting */
	enable_ie(handle->reg_addrs);
	return PINMUX_STATUS_SUCCESS;
}

static void handle_reserve_req(union itc_msg *rec_msg,
                               struct conn_client_info client_info,
                               char *sender_mbox_name)
{
	pinmux_handle_t handle = NULL;;
	union itc_msg *send_msg = NULL;
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_reserve_rej),
	                         sizeof(struct pinmux_reserve_cfm)),
	                     RHD_PINMUX_RESERVE_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;

	if (itc_size(rec_msg) < sizeof(struct pinmux_reserve_req)) {
		TPT_ERROR(STR("pinmux reserve req message is corrupted "
		              "with size %d.", itc_size(rec_msg)));
		ret = PINMUX_STATUS_OTHER;
		goto reserve_end;
	}

	ret = validate_param(rec_msg->reserve_req.number_of_pins,
	                     rec_msg->reserve_req.pin, NULL, false,
	                     0, false, 0, false, 0, false);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto reserve_end;
	}
	ret = init_handle(&handle);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto reserve_end;
	}
	handle->conn_server_ref = client_info.server_ref;
	handle->client_mbox = client_info.sender;

	ret = do_reserve(rec_msg->reserve_req.number_of_pins,
	                 rec_msg->reserve_req.pin,
	                 handle,
                         client_info.sender);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto reserve_end;
	}
	add_handle_to_list(handle);

reserve_end:
	if(ret != PINMUX_STATUS_SUCCESS) {
		free_handle(handle);
		send_msg->reserve_rej.error_code = ret;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_RESERVE_REJ to %s with error code %d",
		                 sender_mbox_name, ret));

	} else {
		send_msg->reserve_cfm.msgno = RHD_PINMUX_RESERVE_CFM;
		send_msg->reserve_cfm.handle = handle;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_RESERVE_CFM to %s",
		                 sender_mbox_name));

	}
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

}

static void handle_unreserve_req(union itc_msg *rec_msg,
                                 struct conn_client_info client_info,
                                 char *sender_mbox_name)
{
	union itc_msg *send_msg = NULL;
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;
	pinmux_handle_t handle = rec_msg->unreserve_req.handle;
	pinmux_handle_t pre_handle;

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_unreserve_rej),
	                         sizeof(struct pinmux_unreserve_cfm)),
	                     RHD_PINMUX_UNRESERVE_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;

	if (itc_size(rec_msg) != sizeof(struct pinmux_unreserve_req)) {
		TPT_ERROR(STR("pinmux unreserve req message is corrupted "
		              "with size %d.", itc_size(rec_msg)));
		ret = PINMUX_STATUS_OTHER;
		goto handle_pinmux_unreserve_req_end;
	}

	if(validate_handle(handle, &pre_handle) == false) {
		ret = PINMUX_STATUS_INVALID_PARAM;
		goto handle_pinmux_unreserve_req_end;
	}
	remove_handle_from_list(handle, pre_handle);
	free_handle(handle);

handle_pinmux_unreserve_req_end:
	if(ret != PINMUX_STATUS_SUCCESS) {
		send_msg->unreserve_rej.error_code = ret;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_UNRESERVE_REJ to %s with error "
		                 "code %d", sender_mbox_name, ret));
	} else {
		send_msg->unreserve_cfm.msgno = RHD_PINMUX_UNRESERVE_CFM;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_UNRESERVE_CFM to %s",
		                 sender_mbox_name));

	}
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

}

static void handle_set_func_req(union itc_msg *rec_msg,
                                struct conn_client_info client_info,
                                char *sender_mbox_name)
{
	union itc_msg *send_msg = NULL;
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_set_func_rej),
	                         sizeof(struct pinmux_set_func_cfm)),
	                     RHD_PINMUX_SET_FUNC_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;

	if (itc_size(rec_msg) < sizeof(struct pinmux_set_func_req)) {
		TPT_ERROR(STR("pinmux set func req message is corrupted "
		              "with size %d.", itc_size(rec_msg)));
		ret = PINMUX_STATUS_OTHER;
		goto handle_set_func_req_end;
	}

	ret = validate_param(rec_msg->set_func_req.number_of_pins,
	                     rec_msg->set_func_req.pin,
	                     rec_msg->set_func_req.handle, true,
	                     rec_msg->set_func_req.func_type, true,
	                     0, false, 0, false);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_set_func_req_end;
	}

	ret = set_func_type(rec_msg->set_func_req.handle,
	                    rec_msg->set_func_req.number_of_pins,
	                    rec_msg->set_func_req.pin,
	                    rec_msg->set_func_req.func_type);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_set_func_req_end;
	}

handle_set_func_req_end:
	if(ret != PINMUX_STATUS_SUCCESS) {
		send_msg->set_func_rej.error_code = ret;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_SET_FUNC_REJ to %s with error "
		                 "code %d", sender_mbox_name, ret));

	} else {
		send_msg->set_func_cfm.msgno = RHD_PINMUX_SET_FUNC_CFM;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_SET_FUNC_CFM to %s",
		                 sender_mbox_name));
	}
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

}

static void handle_get_func_req(union itc_msg *rec_msg,
                                struct conn_client_info client_info,
                                char *sender_mbox_name)
{
	union itc_msg *send_msg = NULL;
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;
	pinmux_func_type_t func_type;

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_get_func_rej),
	                         sizeof(struct pinmux_get_func_cfm)),
	                     RHD_PINMUX_GET_FUNC_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;

	if (itc_size(rec_msg) != sizeof(struct pinmux_get_func_req)) {
		TPT_ERROR(STR("pinmux get func req message is corrupted with "
		              "size %d.", itc_size(rec_msg)));
		ret = PINMUX_STATUS_OTHER;
		goto handle_get_func_req_end;
	}

	ret = validate_param(1, &rec_msg->get_func_req.pin,
	                     rec_msg->get_func_req.handle, true,
	                     0, false, 0, false, 0, false);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_get_func_req_end;
	}

	ret = get_func_type(rec_msg->get_func_req.handle,
	                    rec_msg->get_func_req.pin,
	                    &func_type);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_get_func_req_end;
	}

handle_get_func_req_end:
	if(ret != PINMUX_STATUS_SUCCESS) {
		send_msg->get_func_rej.error_code = ret;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_GET_FUNC_REJ to %s with error "
		                 "code %d", sender_mbox_name, ret));
	} else {
		send_msg->get_func_cfm.msgno = RHD_PINMUX_GET_FUNC_CFM;
		send_msg->get_func_cfm.func_type = func_type;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_GET_FUNC_CFM to %s with "
		                 "func type %d", sender_mbox_name, func_type));
	}
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

}

static void handle_set_cfg_req(union itc_msg *rec_msg,
                               struct conn_client_info client_info,
                               char *sender_mbox_name)
{
	union itc_msg *send_msg = NULL;
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_set_cfg_rej),
	                         sizeof(struct pinmux_set_cfg_cfm)),
	                     RHD_PINMUX_SET_CFG_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;

	if (itc_size(rec_msg) < sizeof(struct pinmux_set_func_req)) {
		TPT_ERROR(STR("pinmux set cfg req message is corrupted"
		              " with size %d.", itc_size(rec_msg)));
		ret = PINMUX_STATUS_OTHER;
		goto handle_set_cfg_req_end;
	}

	ret = validate_param(rec_msg->set_cfg_req.number_of_pins,
	                     rec_msg->set_cfg_req.pin,
	                     rec_msg->set_cfg_req.handle, true,
	                     0, false, rec_msg->set_cfg_req.cfg_type, true,
	                     rec_msg->set_cfg_req.cfg_value, true);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_set_cfg_req_end;
	}

	ret = set_cfg(rec_msg->set_cfg_req.handle,
	              rec_msg->set_cfg_req.number_of_pins,
	              rec_msg->set_cfg_req.pin,
	              rec_msg->set_cfg_req.cfg_type,
	              rec_msg->set_cfg_req.cfg_value);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_set_cfg_req_end;
	}

handle_set_cfg_req_end:
	if(ret != PINMUX_STATUS_SUCCESS) {
		send_msg->set_cfg_rej.error_code = ret;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_SET_CFG_REJ to %s with error "
		                 "code %d", sender_mbox_name, ret));
	} else {
		send_msg->set_cfg_cfm.msgno = RHD_PINMUX_SET_CFG_CFM;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_SET_CFG_CFM to %s",
		                 sender_mbox_name));
	}
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

}

static void handle_get_cfg_req(union itc_msg *rec_msg,
                               struct conn_client_info client_info,
                               char *sender_mbox_name)
{
	union itc_msg *send_msg = NULL;
	pinmux_status_t ret = PINMUX_STATUS_SUCCESS;
	pinmux_cfg_value_t cfg_value;

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_get_cfg_rej),
	                         sizeof(struct pinmux_get_cfg_cfm)),
	                     RHD_PINMUX_GET_CFG_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;

	if (itc_size(rec_msg) != sizeof(struct pinmux_get_cfg_req)) {
		TPT_ERROR(STR("pinmux get cfg req message is corrupted"
		              " with size %d.", itc_size(rec_msg)));
		ret = PINMUX_STATUS_OTHER;
		goto handle_get_cfg_req_end;
	}

	ret = validate_param(1, &rec_msg->get_cfg_req.pin,
	                     rec_msg->get_cfg_req.handle, true, 0, false,
	                     rec_msg->get_cfg_req.cfg_type, true, 0, false);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_get_cfg_req_end;
	}

	ret = get_cfg_value(rec_msg->get_cfg_req.handle,
	                    rec_msg->get_cfg_req.pin,
	                    rec_msg->get_cfg_req.cfg_type,
	                    &cfg_value);
	if(ret != PINMUX_STATUS_SUCCESS) {
		goto handle_get_cfg_req_end;
	}

handle_get_cfg_req_end:
	if(ret != PINMUX_STATUS_SUCCESS) {
		send_msg->get_cfg_rej.error_code = ret;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_GET_CFG_REJ to %s with error "
		                 "code %d", sender_mbox_name, ret));
	} else {
		send_msg->get_cfg_cfm.msgno = RHD_PINMUX_GET_CFG_CFM;
		send_msg->get_cfg_cfm.cfg_value = cfg_value;
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_GET_CFG_CFM to %s with cfg "
		                 "value %d", sender_mbox_name, cfg_value));
	}
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

}
/*************************************************************************/
/** Below two signal handling functions are for debugging purpose */

#define FUNC_TYPE_STR(func_type) func_type_str(func_type)
#define CFG_VALUE_STR(cfg_value) cfg_value_str(cfg_value)
#define PIN_INFO_TXT(pin, func_type, pull, impsel, slew, mbox_name,\
                     handle_str)                                   \
        "%*u%*s%-*s%-*s%-*s%-*s%-*s%-*s\r\n",                      \
                MAX_LIST_INFO_PIN_LEN, pin,                            \
                MAX_LIST_INFO_PIN_MARGIN_LEN, "",                      \
                MAX_LIST_INFO_FUNC_LEN, FUNC_TYPE_STR(func_type),      \
                MAX_LIST_INFO_PULLSEL_LEN, CFG_VALUE_STR(pull),        \
                MAX_LIST_INFO_IMPSEL_LEN, CFG_VALUE_STR(impsel),       \
                MAX_LIST_INFO_SLEW_LEN, CFG_VALUE_STR(slew),           \
                MAX_LIST_INFO_MBOX_NAME_LEN, mbox_name,                \
                MAX_LIST_INFO_HANDLE_LEN, handle_str

static char *func_type_str(pinmux_func_type_t func_type)
{
	switch(func_type) {
	case PINMUX_FUNC_TYPE_GPIO:
		return "GPIO";
	case PINMUX_FUNC_TYPE_ALTF1:
		return "ALTF1";
	case PINMUX_FUNC_TYPE_ALTF2:
		return "ALTF2";
	default:
		return "INVALID";
	}
}

static char *cfg_value_str(pinmux_cfg_value_t cfg_value)
{
	switch(cfg_value) {
	case PINMUX_CFG_VALUE_PULLSEL_NONE:
		return "PULLSEL_NONE";
	case PINMUX_CFG_VALUE_PULLSEL_UP:
		return "PULLSEL_UP";
	case PINMUX_CFG_VALUE_PULLSEL_DOWN:
		return "PULLSEL_DOWN";
	case PINMUX_CFG_VALUE_IMPSEL_00:
		return "IMPSEL_00";
	case PINMUX_CFG_VALUE_IMPSEL_01:
		return "IMPSEL_01";
	case PINMUX_CFG_VALUE_IMPSEL_10:
		return "IMPSEL_10";
	case PINMUX_CFG_VALUE_IMPSEL_11:
		return "IMPSEL_11";
	case PINMUX_CFG_VALUE_SLEW_0:
		return "SLEW_0";
	case PINMUX_CFG_VALUE_SLEW_1:
		return "SLEW_1";
	default:
		return "INVALID";
	}
}

static void read_all_cfg_value(uint32_t hw_pin,
                               volatile struct reg_addr reg_addr,
                               pinmux_func_type_t *func_type,
                               pinmux_cfg_value_t *pull_value,
                               pinmux_cfg_value_t *impsel_value,
                               pinmux_cfg_value_t *slew_value)
{
	*func_type = read_func_type_from_reg(reg_addr, hw_pin);
	read_cfg_value_from_reg(reg_addr, hw_pin,
	                        PINMUX_CFG_TYPE_PULLSEL, pull_value);
	read_cfg_value_from_reg(reg_addr, hw_pin,
	                        PINMUX_CFG_TYPE_IMPSEL, impsel_value);
	read_cfg_value_from_reg(reg_addr, hw_pin,
	                        PINMUX_CFG_TYPE_SLEW, slew_value);
}

static void get_mailbox_name(uint32_t hw_pin,
                             uint32_t key,
                             char mbox_name[],
                             pinmux_handle_t *handle)
{
	pinmux_handle_t tmp_handle = pinmux_handle_list.head;
	while(tmp_handle != NULL) {
		if(tmp_handle->reserved_masks[key] &
		   GENERATE_PIN_MASK(hw_pin)) {
			break;
		}
		tmp_handle = tmp_handle->next;
	}

	if(tmp_handle != NULL) {
		itc_get_name(tmp_handle->client_mbox, mbox_name,
		             MAX_MAILBOX_NAME_LEN);
		if(handle != NULL) {
			*handle = tmp_handle;
		}
	} else {
		/* This case should not happen */
		mbox_name[0] = '\0';
		if(handle != NULL) {
			*handle = (pinmux_handle_t)0xdeadbeef;
		}
		TPT_ERROR("No reserved pin found");

	}
}

static void generate_each_subgroup_pins_info(
	uint32_t key,
	uint32_t mask,
	char *str,
	uint32_t size,
	uint32_t verbose_mode,
	uint32_t type)
{
	uint32_t pin, hw_pin, tmpsize = 0;
	pinmux_func_type_t func_type;
	pinmux_cfg_value_t pull_value, impsel_value, slew_value;
	char mbox_name[MAX_MAILBOX_NAME_LEN];
	pinmux_handle_t handle = NULL;
	char *tmpstr = str;
	char handle_str[sizeof("4294967295")];
	volatile struct reg_addr reg_addrs;
	if(!setup_reg_addr(uio_map_storage[map_table[key].group].base,
	                   map_table[key].subgroup,
	                   &reg_addrs)) {
		strncpy(tmpstr, "setup register addr failed\r\n", size);
		return;
	}


	for(uint32_t i = 0; i < 32; i++) {
		handle_str[0] = '\0';
		mbox_name[0] = '\0';
		pin = map_table[key].first_pin_number + i;
		if(pin > map_table[key].last_pin_number) {
			break;
		}
		hw_pin = map_table[key].first_pin_number_in_group
		        + i;
		read_all_cfg_value(hw_pin, reg_addrs,
		                   &func_type,  &pull_value,
		                   &impsel_value, &slew_value);

		if(mask & (1 << i)) {
			/* the case of reseved pin */
			get_mailbox_name(hw_pin, key, mbox_name, &handle);

			if(verbose_mode) {
				snprintf(handle_str, sizeof("4294967295"),
				         "0x%08x", (uint32_t)handle);
			}

		} else if(!(mask & (1 << i)) &&
			  type == PINMUX_LIST_TYPE_SHORT) {
			continue;
		}
		tmpsize =
			snprintf(tmpstr, size,
			         PIN_INFO_TXT(pin,
			                      func_type,
			                      pull_value,
			                      impsel_value,
			                      slew_value,
			                      mbox_name,
			                      handle_str));

		tmpstr += tmpsize;
		size -= tmpsize;
	}
}

static void list_pins_info(uint32_t *reserved_masks, uint32_t type,
			   uint32_t verbose_mode,
			   struct conn_client_info client_info)
{
	for(uint32_t i = 0; i < sizeof(map_table) / sizeof(map_table[0]); i++) {
		union itc_msg *send_msg =
		        itc_alloc(sizeof(struct pinmux_list_ind),
		                  RHD_PINMUX_LIST_IND);
		uint32_t size = sizeof(send_msg->list_ind.str);
		send_msg->list_ind.str[0] = '\0';
		send_msg->any_msg.connection_ref = client_info.client_ref;

		generate_each_subgroup_pins_info(i, reserved_masks[i],
		                                 send_msg->list_ind.str,
		                                 size, verbose_mode, type);
		if(send_msg->list_ind.str[0] != '\0') {
			send_msg->list_ind.str[size - 1] = '\0';
			TPT_SEND_SIG(send_msg->msgno, client_info.sender,
			             "RHD_PINMUX_LIST_IND");

			itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);
		} else {
			itc_free(&send_msg);
		}
	}
}

#define DUMP_REG(fp, reg_addrs, group, subgroup) \
	dump_reg(fp, reg_addr, group, subgroup)

static void dump_reg(FILE *fp,
                     volatile struct reg_addr reg_addr,
                     uint32_t group,
                     uint32_t subgroup)
{

	fprintf(fp, "Group %u Subgroup %u Register dump:\n"
	        "        Addr          : Value\n"
	        "   lock     0x%08x    : 0x%08x\n"
	        "   sel      0x%08x    : 0x%08x\n"
	        "   alt_func 0x%08x    : 0x%08x\n"
	        "   pull_en  0x%08x    : 0x%08x\n"
	        "   pull_sel 0x%08x    : 0x%08x\n"
	        "   impsel1  0x%08x    : 0x%08x\n"
	        "   impsel2  0x%08x    : 0x%08x\n"
	        "   slew     0x%08x    : 0x%08x\n"
	        "   ie       0x%08x    : 0x%08x\n\n\n",
	        group, subgroup,
	        (uint32_t)reg_addr.lock, *(reg_addr.lock),
	        (uint32_t)reg_addr.sel, *(reg_addr.sel),
	        (uint32_t)reg_addr.alt_func, *(reg_addr.alt_func),
	        (uint32_t)reg_addr.pull_en, *(reg_addr.pull_en),
	        (uint32_t)reg_addr.pull_sel, *(reg_addr.pull_sel),
	        (uint32_t)reg_addr.impsel1, *(reg_addr.impsel1),
	        (uint32_t)reg_addr.impsel2, *(reg_addr.impsel2),
	        (uint32_t)reg_addr.slew, *(reg_addr.slew),
	        (uint32_t)reg_addr.ie, *(reg_addr.ie));
}

static void dump_reg_value(FILE *fp)
{
	volatile struct reg_addr reg_addr;
	uint32_t number_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);
	for(uint32_t i = 0; i < number_of_subgroups; i++) {
		uint32_t group = map_table[i].group;
		uint32_t subgroup = map_table[i].subgroup;
		if(setup_reg_addr(uio_map_storage[group].base,
		                  subgroup, &reg_addr)) {
			DUMP_REG(fp, reg_addr, group, subgroup);
		}
	}
}

static void handle_list_req(union itc_msg *rec_msg,
                            struct conn_client_info client_info,
                            char *sender_mbox_name)
{

	union itc_msg *send_msg = NULL;

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_list_rej),
	                         sizeof(struct pinmux_list_cfm)),
	                     RHD_PINMUX_LIST_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;
	if (itc_size(rec_msg) != sizeof(struct pinmux_list_req)) {
		TPT_ERROR(STR("pinmux list req message is "
		              "corrupted with size %d",
		              itc_size(rec_msg)));
		TPT_SEND_SIG(send_msg->msgno,
		             client_info.sender,
		             STR("RHD_PINMUX_LIST_REJ to %s",
		                 sender_mbox_name));
		itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

	} else {
		send_msg->list_cfm.msgno = RHD_PINMUX_LIST_CFM;
		TPT_SEND_SIG(send_msg->msgno,
		             client_info.sender,
		             STR("RHD_PINMUX_LIST_CFM to %s",
		                 sender_mbox_name));
		itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);
	}

	send_msg = itc_alloc(sizeof(struct pinmux_list_ind),
	                     RHD_PINMUX_LIST_IND);
	send_msg->any_msg.connection_ref = client_info.client_ref;
	if(rec_msg->list_req.verbose_mode) {
		strncpy(send_msg->list_ind.str,
		        "---pin--func---pullsel-------impsel-----slew----"
		        "mailbox_name----------------------handle----\r\n",
		        sizeof(send_msg->list_ind.str));
	} else{
		strncpy(send_msg->list_ind.str,
		        "---pin--func---pullsel-------impsel-----slew----"
		        "mailbox_name--------------------\r\n",
		        sizeof(send_msg->list_ind.str));
	}
	send_msg->list_ind.str[sizeof(send_msg->list_ind.str) -1] = '\0';
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);

	if(pinmux_handle_list.all_reserved_masks != NULL) {
		list_pins_info(pinmux_handle_list.all_reserved_masks,
		               rec_msg->list_req.type,
		               rec_msg->list_req.verbose_mode,
		               client_info);
	}
	send_msg = itc_alloc(sizeof(struct pinmux_list_end_ind),
	                     RHD_PINMUX_LIST_END_IND);
	send_msg->any_msg.connection_ref = client_info.client_ref;
	send_msg->list_end_ind.msgno = RHD_PINMUX_LIST_END_IND;
	TPT_SEND_SIG(send_msg->msgno, client_info.sender,
	             STR("RHD_PINMUX_LIST_END_IND to %s", sender_mbox_name));
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);
}

static void handle_dump_req(union itc_msg *rec_msg,
                            struct conn_client_info client_info,
                            char *sender_mbox_name)
{

	union itc_msg *send_msg = NULL;
	FILE *fp;
	char *filename = "/tmp/pinmux_dump.txt";

	send_msg = itc_alloc(MAX(sizeof(struct pinmux_dump_rej),
	                         sizeof(struct pinmux_dump_cfm)),
	                     RHD_PINMUX_DUMP_REJ);
	send_msg->any_msg.connection_ref = client_info.client_ref;
	if (itc_size(rec_msg) != sizeof(struct pinmux_dump_req)) {
		TPT_ERROR(STR("pinmux dump req message is "
		              "corrupted with size %d.",
		              itc_size(rec_msg)));
		TPT_SEND_SIG(send_msg->msgno, client_info.sender,
		             STR("RHD_PINMUX_DUMP_REJ to %s",
		                 sender_mbox_name));
		goto handle_dump_req_end;
	}

	fp = fopen(filename, "w+");
	if(fp == NULL) {
		TPT_ERROR(STR("open %s is failed.", filename));
		goto handle_dump_req_end;
	}
	dump_reg_value(fp);
	fclose(fp);
	send_msg->dump_cfm.msgno = RHD_PINMUX_DUMP_CFM;

	TPT_SEND_SIG(send_msg->msgno, client_info.sender,
	             STR("RHD_PINMUX_DUMP_CFM to %s",
	                 sender_mbox_name));
handle_dump_req_end:
	itc_send(&send_msg, client_info.sender, ITC_MY_MBOX);
}

/******************************************************************************/
/**
 * Function read_messages
 * Start reading received messages through ITC and handle them.
 */
static void read_messages(conn_server_handle_t handle)
{
	union itc_msg *msg;
	struct conn_client_info client_info;
	char sender_mbox_name[MAX_MAILBOX_NAME_LEN];

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		if (msg->msgno == EXIT_SIGNAL) {
			TPT_INFO(STR("%s exiting as ordered", DAEMON_NAME));
			itc_free(&msg);
			return;
		}

		/*Handle CONN_ESTABLISH... messages (and messages from
		 unknown clients.*/
		if(!conn_check_client(handle, &msg, &client_info))
			continue;

		itc_get_name(client_info.sender, sender_mbox_name,
		             MAX_MAILBOX_NAME_LEN);

		switch (msg->msgno) {
		case RHD_PINMUX_RESERVE_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_RESERVE_REQ, "
			                "sender: %d(%s).", client_info.sender,
			                sender_mbox_name));
			handle_reserve_req(msg, client_info, sender_mbox_name);
			break;
		}
		case RHD_PINMUX_UNRESERVE_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_UNRESERVE_REQ, "
			                "sender: %d(%s).", client_info.sender,
			                sender_mbox_name));
			handle_unreserve_req(msg, client_info, sender_mbox_name);
			break;
		}
		case RHD_PINMUX_SET_FUNC_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_SET_FUNC_REQ, "
			                "sender: %d(%s).", client_info.sender,
			                sender_mbox_name));
			handle_set_func_req(msg, client_info, sender_mbox_name);
			break;
		}
		case RHD_PINMUX_GET_FUNC_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_GET_FUNC_REQ, "
			                "sender: %d(%s).", client_info.sender,
			                sender_mbox_name));
			handle_get_func_req(msg, client_info, sender_mbox_name);
			break;
		}
		case RHD_PINMUX_SET_CFG_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_SET_CFG_REQ, "
			                "sender: %d(%s).", client_info.sender,
			                sender_mbox_name));
			handle_set_cfg_req(msg, client_info, sender_mbox_name);
			break;
		}
		case RHD_PINMUX_GET_CFG_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_GET_CFG_REQ, "
			                "sender: %d.", client_info.sender));
			handle_get_cfg_req(msg, client_info, sender_mbox_name);
			break;
		}
		case RHD_PINMUX_LIST_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_LIST_REQ, "
			                "sender: %d(%s).", client_info.sender,
			                sender_mbox_name));
			handle_list_req(msg, client_info, sender_mbox_name);
			break;
		}
		case RHD_PINMUX_DUMP_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_PINMUX_DUMP_REQ, "
			                "sender: %d(%s).",
			                client_info.sender, sender_mbox_name));
			handle_dump_req(msg, client_info, sender_mbox_name);
			break;
		}
		default:
			TPT_ERROR(STR("Receive unexpected message: %d"
			              " from sender %d(%s)",
			              msg->msgno, client_info.sender, sender_mbox_name));
			break;
		}
		itc_free(&msg);
	}
}


static bool setup_reg_addr(void *base,
                           uint32_t subgroup,
                           volatile struct reg_addr *reg_addrs)
{
	if(base == NULL) {
		TPT_ERROR("base address is NULL");
		return false;
	}

	reg_addrs->sel = (uint32_t *)
	                 PINMUX_X_X_SEL_ADDR((uintptr_t)base, subgroup);
	reg_addrs->alt_func = (uint32_t *)
	                      PINMUX_X_X_ALT_FUNC_ADDR((uintptr_t)base,
	                                      subgroup);
	reg_addrs->pull_en = (uint32_t *)
	                     PINMUX_X_X_PULL_EN_ADDR((uintptr_t)base, subgroup);
	reg_addrs->pull_sel = (uint32_t *)
	                      PINMUX_X_X_PULL_SEL_ADDR((uintptr_t)base,
	                                      subgroup);
	reg_addrs->impsel1 = (uint32_t *)
	                     PINMUX_X_X_IMPSEL1_ADDR((uintptr_t)base, subgroup);
	reg_addrs->impsel2 = (uint32_t *)
	                     PINMUX_X_X_IMPSEL2_ADDR((uintptr_t)base, subgroup);
	reg_addrs->slew = (uint32_t *)
	                  PINMUX_X_X_SLEW_ADDR((uintptr_t)base, subgroup);
	reg_addrs->ie = (uint32_t *)
	                PINMUX_X_IE_ADDR((uintptr_t)base);
	reg_addrs->lock = (uint32_t *)
	                  PINMUX_X_LOCK_ADDR((uintptr_t)base);

	DEBUG_TRACE(*reg_addrs);

	return true;
}

static int do_uio_mmap(uint32_t group)
{
	UIO_HANDLE_ uio_handle = NULL;
	void *mmap_base = NULL;
	char uio_dev_name[UIO_DEV_NAME_LEN];

	snprintf(uio_dev_name, sizeof(uio_dev_name),
	         "%s%d", UIO_PINMUX_PREFIX, group);
	uio_handle = uio_open(uio_dev_name);
	if (uio_handle == UIO_OPEN_FAILED) {
		TPT_ERROR(STR("Failed to open uio %d %d", group, errno));
		goto do_uio_mmap_error;
	}

	mmap_base = uio_mmap(uio_handle);

	if (mmap_base == MAP_FAILED) {
		TPT_ERROR("Failed to peform UIO memory mapping");
		goto do_uio_mmap_error;
	}

	uio_map_storage[group].handle = uio_handle;
	uio_map_storage[group].base = mmap_base;
	return 0;
do_uio_mmap_error:
	uio_close(uio_handle);

	return -EFAULT;
}

static void do_uio_unmap(uint32_t group)
{
	uio_close(uio_map_storage[group].handle);
	uio_map_storage[group].handle = NULL;
	uio_map_storage[group].base = NULL;
}

static int pinmux_init(void)
{
	int i = 0, j = 0;

	/* Initialize ITC */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	/* Create our mailbox. */
	pinmux_mbox = itc_create_mailbox(RHD_PINMUX_MAILBOX, 0);
	if (pinmux_mbox == ITC_NO_ID) {
		return -EFAULT;
	}
	/* map uio device for each group */
	for(i = 0; i < NUM_OF_GROUPS; i++) {
		if(do_uio_mmap(i)) {
			goto init_error;
		}
	}

	/* Intitialize all_reserved_masks */
	/* It's an 32bits array with size 12.
	 * Each 32bits is storing 32 pins reserved information.
	 * 1 is reserved, 0 is not reserved.
	 */
	pinmux_handle_list.all_reserved_masks = (uint32_t *)
	     calloc(sizeof(map_table) / sizeof(map_table[0]), sizeof(uint32_t));

	return 0;
init_error:
	for(j = 0; j < i; j++) {
		do_uio_unmap(j);
	}
	return -EFAULT;
}

static void remove_handle_owned_by_same_client(uint32_t server_ref)
{
	struct pinmux_handle *handle, *pre_handle, *next_handle;
	handle = pinmux_handle_list.head;
	pre_handle = NULL;

	while(handle != NULL) {
		next_handle = handle->next;
		if(handle->conn_server_ref == server_ref) {
			remove_handle_from_list(handle, pre_handle);
			free_handle(handle);
			handle = next_handle;
		} else {
			pre_handle = handle;
			handle = handle->next;
		}
	}
}

/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{
	/* Do unreserve pins */
	remove_handle_owned_by_same_client(client_info->server_ref);

}

/**
 * Function conn_server_init
 */
static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	struct conn_event_callbacks cb = { NULL, client_disconnect,
		       client_disconnect, NULL
	};

	uint32_t supported_versions[] = {PINMUX_SERVER_VERSIONS};
	int conn_result = conn_establish_server_init( &handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &pinmux_conn_messages, 0, &cb);

	if( conn_result != CONN_INIT_OK) {
		TPT_ERROR("Initalization of conn_establish "
		          "mechanism failed.");
		return NULL;
	}

	return handle;
}


static void print_usage()
{
	printf("Usage: rhd-pinmux <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, pinmux_mbox, ITC_MY_MBOX);
}

int main(int argc, char **argv)
{
	int daemonize = 0;
	int32_t ret = 0;
	void *conn_handle;

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		} else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		} else {
			print_usage();
			exit(-EINVAL);
		}
	}

	if (rhd_try_lock(DAEMON_NAME)) {
		printf("failed to obtain lock: %s\n", DAEMON_NAME);
		ret = -EFAULT;
		return ret;
	}


	if (!daemonize || !daemon(0, 0)) {

		TPT_INFO(STR("Starting %s %s",
		             daemonize ? "daemon" : "foreground process",
		             DAEMON_NAME));

		conn_handle = conn_server_init();

		if (!pinmux_init() && (conn_handle != NULL)) {
			/* Start processing ITC messages.
			 * No return.
			 */
			if (signal(SIGTERM, exit_handler) == SIG_ERR) {
				TPT_ERROR("Failed to install signal exit handler");
				exit(-EFAULT);
			}

			read_messages(conn_handle);

		} else {
			TPT_ERROR("Failed to intialize pinmux");
			ret = -EFAULT;
		}
	} else {
		TPT_ERROR(STR("Failed to start daemon %s", DAEMON_NAME));
		ret = -EFAULT;
	}

	return ret;

}

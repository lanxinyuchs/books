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
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <net/if.h>
#include <uio_helper.h>
#include <gpio.h>
#include <pthread.h>
#include <errno.h>
#include "pinmux.h"


/*
 * MACROS
 */

#define TRACEPOINT_PROVIDER    com_ericsson_xcs_rhd_libgpio
#include "tpt_create.h"
#include "tpt.h"

#define GPIO_PAD_OFFSET        0x400
#define GPIO_OUT_OFFSET        0x440
#define GPIO_DIR_OFFSET        0x480
#define GPIO_OUT_SET_OFFSET    0x500
#define GPIO_OUT_CLR_OFFSET    0x540
#define GPIO_DIR_SET_OFFSET    0x580
#define GPIO_DIR_CLR_OFFSET    0x5C0

#define GPIO_X_X_PAD_ADDR(base, subgroup)               \
	((base) + (subgroup) * 4 + GPIO_PAD_OFFSET)
#define GPIO_X_X_OUT_ADDR(base, subgroup)               \
	((base) + (subgroup) * 4 + GPIO_OUT_OFFSET)
#define GPIO_X_X_DIR_ADDR(base, subgroup)               \
	((base) + (subgroup) * 4 + GPIO_DIR_OFFSET)
#define GPIO_X_X_OUT_SET_ADDR(base, subgroup)           \
	((base) + (subgroup) * 4 + GPIO_OUT_SET_OFFSET)
#define GPIO_X_X_OUT_CLR_ADDR(base, subgroup)           \
	((base) + (subgroup) * 4 + GPIO_OUT_CLR_OFFSET)
#define GPIO_X_X_DIR_SET_ADDR(base, subgroup)           \
	((base) + (subgroup) * 4 + GPIO_DIR_SET_OFFSET)
#define GPIO_X_X_DIR_CLR_ADDR(base, subgroup)           \
	((base) + (subgroup) * 4 + GPIO_DIR_CLR_OFFSET)


#define _STRINGIFY(s)  #s
#define STRINGIFY(s)   _STRINGIFY(s)

#define NUM_OF_PINS             337
#define NUM_OF_GROUPS           4
#define UIO_GPIO_PREFIX         "gpio-pinmux"
#define MAGIC_NUM               0xEEEEEEEE
#define UIO_DEV_NAME_LEN        sizeof(UIO_GPIO_PREFIX) + \
	sizeof(STRINGIFY(NUM_OF_GROUPS))                /*e.g "gpio-pinmux0"*/


#define GENERATE_PIN_MASK(pin)  1 << ((pin) & 0x1F)
#define GENERATE_PIN_VALUE(register_value, pin) \
	((register_value) >> ((pin) & 0x1F)) & 0x01

#define REQUIRE(x, s) if (!(x)) do {                                 \
     TPT_ERROR("Expected '" #x "' to be true, returning: " #s);        \
     return s;							      \
} while(0)

/** structure and typedef */

struct reg_addr {
	uint32_t *pad;
	uint32_t *out_set;
	uint32_t *out_clr;
	uint32_t *dir;
	uint32_t *dir_set;
	uint32_t *dir_clr;
#ifdef DEBUG
	uint32_t *out;
#endif
};

struct gpio_handle {
	pinmux_handle_t pinmux_handle;
	volatile struct reg_addr *reg_addrs;   /* Store each subgroup register addr */
	uint32_t *reserved_masks;              /* Store each subgroup reserved pin masks */
	uint32_t magic_num;                    /* check magic_num to avoid to use freed handle */
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
	uint32_t num_of_users;
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

/* the table content meaning see struct pin_map
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

static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

static struct uio_map_storage uio_map_storage[NUM_OF_GROUPS] = {
	{NULL, NULL, 0},
	{NULL, NULL, 0},
	{NULL, NULL, 0},
	{NULL, NULL, 0}
};

/*
 * Local Function Declaration
 */
static void free_handle(gpio_handle_t handle);
static gpio_status_t check_param_valid(uint32_t number_of_pins,
                                       const uint32_t pin[],
                                       gpio_handle_t handle,
                                       bool check_handle);
static gpio_status_t init_handle(gpio_handle_t *handle);
static int do_uio_mmap(uint32_t group);
static gpio_status_t set_uio_mmap_and_register_addr(struct hw_pin hw_pin,
                                                    gpio_handle_t handle);
static bool pin_to_hw_pin(uint32_t pin, struct hw_pin *hw_pin);
static bool setup_reg_addr(void *base,
                           uint32_t subgroup,
                           volatile struct reg_addr *reg_addrs);
static gpio_status_t
pinmux_status_to_gpio_status(pinmux_status_t pinmux_status);
static gpio_status_t reserve_pins(uint32_t number_of_pins,
                                  const uint32_t pin[],
                                  gpio_handle_t *handle);
static gpio_status_t unmap_uio(gpio_handle_t handle);
static gpio_status_t unreserve_pins(pinmux_handle_t pinmux_handle);
static gpio_status_t do_unreserve(gpio_handle_t handle);
static gpio_status_t do_reserve(uint32_t number_of_pins,
                                const uint32_t pin[],
                                gpio_handle_t *handle);

#ifdef DEBUG
#define DEBUG_TRACE(reg_addr) debug_trace(reg_addr)
static void debug_trace(volatile struct reg_addr reg_addrs)
{
	TPT_TRACE(4, STR("register pad 0x%x: 0x%x",
	                 reg_addrs.pad, *(reg_addrs.pad)));
	TPT_TRACE(4, STR("register out_set 0x%x: 0x%x",
	                 reg_addrs.out_set,
	                 *(reg_addrs.out_set)));
	TPT_TRACE(4, STR("register out clr 0x%x: 0x%x",
	                 reg_addrs.out_clr,
	                 *(reg_addrs.out_clr)));
	TPT_TRACE(4, STR("register dir 0x%x: 0x%x",
	                 reg_addrs.dir, *(reg_addrs.dir)));
	TPT_TRACE(4, STR("register dir set 0x%x: 0x%x",
	                 reg_addrs.dir_set,
	                 *(reg_addrs.dir_set)));
	TPT_TRACE(4, STR("register dir clr 0x%x: 0x%x",
	                 reg_addrs.dir_clr,
	                 *(reg_addrs.dir_clr)));
	TPT_TRACE(4, STR("register out 0x%x: 0x%x",
	                 reg_addrs.out, *(reg_addrs.out)));
	return;
}
#else /* DEBUG */
#define DEBUG_TRACE(reg_addr) ((void) 0)
#endif /* DEBUG */

static gpio_status_t check_param_valid(uint32_t number_of_pins,
                                       const uint32_t pin[],
                                       gpio_handle_t handle,
                                       bool check_handle)
{
	if(number_of_pins == 0 || number_of_pins > NUM_OF_PINS) {

		TPT_ERROR(STR("number_of_pins %d is out of range [1, %d]",
		              number_of_pins, NUM_OF_PINS));
		return GPIO_STATUS_INVALID_PARAM;
	}
	for(uint32_t i = 0; i < number_of_pins; i++) {

		if(pin[i] > (NUM_OF_PINS - 1)) {
			TPT_ERROR(STR("pin %d is out of range [0, %d]",
			              pin[i], NUM_OF_PINS - 1));
			return GPIO_STATUS_INVALID_PARAM;
		}
	}


	if(check_handle == true &&
	    (handle->pinmux_handle == NULL ||
	     handle->reserved_masks == NULL ||
	     handle->reg_addrs == NULL ||
	     handle->magic_num != MAGIC_NUM)) {
		TPT_ERROR("handle is invalid");
		return GPIO_STATUS_INVALID_PARAM;
	}

	return GPIO_STATUS_SUCCESS;
}


static gpio_status_t init_handle(gpio_handle_t *handle)
{
	uint32_t number_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);

	*handle = (gpio_handle_t)malloc(sizeof(struct gpio_handle));
	if(*handle == NULL) {
		TPT_ERROR(STR("malloc handle failed %d",
		              sizeof(struct gpio_handle)));
		goto init_handle_end;
	}

	/** init reg_addrs */
	(*handle)->reg_addrs = (struct reg_addr *)
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


	/* magic num to avoid use freed handle */
	(*handle)->magic_num = MAGIC_NUM;

	return GPIO_STATUS_SUCCESS;
init_handle_end:
	free_handle(*handle);
	*handle = NULL;
	return GPIO_STATUS_OTHER;
}

static void free_handle(gpio_handle_t handle)
{
	if(handle != NULL) {
		free((void *)handle->reg_addrs);
		free(handle->reserved_masks);
		handle->magic_num = 0;
		free(handle);
	}
	return;
}


static int do_uio_mmap(uint32_t group)
{
	UIO_HANDLE_ uio_handle = NULL;
	void *mmap_base = NULL;
	char uio_dev_name[UIO_DEV_NAME_LEN];

	snprintf(uio_dev_name, sizeof(uio_dev_name),
	         "%s%d", UIO_GPIO_PREFIX, group);
	uio_handle = uio_open(uio_dev_name);
	if (uio_handle == UIO_OPEN_FAILED) {
		TPT_ERROR(STR("Failed to open uio %d %d",
		              group, errno));
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

static void do_unmap(bool counter_increased, bool map_done,
                     struct hw_pin hw_pin)
{

	if(counter_increased == true) {
		uio_map_storage[hw_pin.group].num_of_users--;
	}
	if(map_done == true) {
		uio_map_storage[hw_pin.group].base = NULL;
		uio_close(uio_map_storage[hw_pin.group].handle);
		uio_map_storage[hw_pin.group].handle = NULL;
	}
}

static gpio_status_t set_uio_mmap_and_register_addr(struct hw_pin hw_pin,
                                                    gpio_handle_t handle)
{

	uint32_t group = hw_pin.group;
	uint32_t key = hw_pin.key;
	bool counter_increased = false;
	bool map_done = false;
	int pthread_ret;


	pthread_ret = pthread_mutex_lock(&lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}

	if(uio_map_storage[group].base != NULL &&
	    handle->reg_addrs[key].pad != NULL) {
		/* Has already mapped and setup register addrs */
		goto set_uio_mmap_and_register_addr_end;
	}

	if(uio_map_storage[group].base != NULL &&
	    handle->reg_addrs[key].pad == NULL) {
		/* Has already mapped but not setup register addrs */
		uio_map_storage[group].num_of_users++;
		counter_increased = true;
		goto set_uio_mmap_and_register_addr_end;
	}

	if(!do_uio_mmap(group)) {
		uio_map_storage[group].num_of_users++;
		counter_increased = true;
		map_done = true;
	}

set_uio_mmap_and_register_addr_end:
	pthread_ret = pthread_mutex_unlock(&lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
		do_unmap(counter_increased, map_done, hw_pin);
		return GPIO_STATUS_OTHER;
	}
	/* setup register address */
	if(setup_reg_addr(uio_map_storage[group].base,
			  hw_pin.subgroup,
			  &handle->reg_addrs[key]) == false) {
		do_unmap(counter_increased, map_done, hw_pin);
		return GPIO_STATUS_OTHER;
	}

	return GPIO_STATUS_SUCCESS;
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

static bool setup_reg_addr(void *base,
                           uint32_t subgroup,
                           volatile struct reg_addr *reg_addrs)
{
	if(base == NULL) {
		TPT_ERROR("base address is NULL");
		return false;
	}

	reg_addrs->pad = (uint32_t *)GPIO_X_X_PAD_ADDR((uintptr_t)base,
	                             subgroup);
	reg_addrs->out_set = (uint32_t *)GPIO_X_X_OUT_SET_ADDR((uintptr_t)base,
	                     subgroup);
	reg_addrs->out_clr = (uint32_t *)GPIO_X_X_OUT_CLR_ADDR((uintptr_t)base,
	                     subgroup);
	reg_addrs->dir = (uint32_t *)GPIO_X_X_DIR_ADDR((uintptr_t)base,
	                             subgroup);
	reg_addrs->dir_set = (uint32_t *)GPIO_X_X_DIR_SET_ADDR((uintptr_t)base,
	                     subgroup);
	reg_addrs->dir_clr = (uint32_t *)GPIO_X_X_DIR_CLR_ADDR((uintptr_t)base,
	                     subgroup);
#ifdef DEBUG
	reg_addrs->out = (uint32_t *)GPIO_X_X_OUT_ADDR((uintptr_t)base,
	                             subgroup);
	DEBUG_TRACE(*reg_addrs);
#endif

	return true;
}

static gpio_status_t generate_all_pin_masks(gpio_handle_t handle,
                uint32_t number_of_pins,
                const uint32_t pin[],
                const uint8_t value[],
                uint32_t **pin_mask,
                uint32_t **value_mask)
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	struct hw_pin hw_pin;
	uint32_t num_of_subgroups = 0;

	num_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);
	/** init pin_mask */
	*pin_mask = (uint32_t *)calloc(num_of_subgroups, sizeof(uint32_t));
	if(*pin_mask == NULL) {
		TPT_ERROR(STR("calloc pin_mask failed %d",
		              num_of_subgroups * sizeof(uint32_t)));
		ret = GPIO_STATUS_OTHER;
		goto generate_all_pin_masks_error;
	}

	/** init value_mask */
	*value_mask = (uint32_t *)calloc(num_of_subgroups, sizeof(uint32_t));
	if(*value_mask == NULL) {
		TPT_ERROR(STR("calloc value_mask failed %d",
		              num_of_subgroups * sizeof(uint32_t)));
		ret = GPIO_STATUS_OTHER;
		goto generate_all_pin_masks_error;
	}

	for(uint32_t i = 0; i < number_of_pins; i++) {
		uint32_t tmp_mask = 0;

		if(pin_to_hw_pin(pin[i], &hw_pin) == false) {
			ret = GPIO_STATUS_INVALID_PARAM;
			goto generate_all_pin_masks_error;
		}

		tmp_mask = GENERATE_PIN_MASK(hw_pin.pin_num);

		if((handle->reserved_masks[hw_pin.key] & tmp_mask) == 0) {
			TPT_ERROR(STR("pin %d is not reserved", pin[i]));
			ret =  GPIO_STATUS_WRONG_STATE;
			goto generate_all_pin_masks_error;
		}

		(*pin_mask)[hw_pin.key] |= tmp_mask;
		if(value[i] != 0) {
			(*value_mask)[hw_pin.key] |= tmp_mask;
		}
		TPT_TRACE(3, STR("value_mask 0x%x pin_mask 0x%x",
		                 (*value_mask)[hw_pin.key],
		                 (*pin_mask)[hw_pin.key]));

	}
	return ret;
generate_all_pin_masks_error:
	free(*value_mask);
	free(*pin_mask);
	return ret;
}

static gpio_status_t
pinmux_status_to_gpio_status(pinmux_status_t pinmux_status)
{
	gpio_status_t ret_status;
	switch(pinmux_status) {
	case PINMUX_STATUS_SUCCESS:
		ret_status = GPIO_STATUS_SUCCESS;
		break;
	case PINMUX_STATUS_INVALID_PARAM:
		ret_status = GPIO_STATUS_INVALID_PARAM;
		break;
	case PINMUX_STATUS_WRONG_STATE:
		ret_status = GPIO_STATUS_WRONG_STATE;
		break;
	case PINMUX_STATUS_UNSUPPORTED:
		ret_status = GPIO_STATUS_UNSUPPORTED;
		break;
	case PINMUX_STATUS_OTHER:
	default:
		ret_status = GPIO_STATUS_OTHER;
		break;
	}
	return ret_status;
}

static gpio_status_t
gpio_to_pinmux_cfg_type(gpio_cfg_type_t gpio_cfg_type,
                        pinmux_cfg_type_t *pinmux_cfg_type)
{
	switch(gpio_cfg_type) {
	case GPIO_CFG_TYPE_PULLSEL:
		*pinmux_cfg_type = PINMUX_CFG_TYPE_PULLSEL;
		break;
	case GPIO_CFG_TYPE_IMPSEL:
		*pinmux_cfg_type = PINMUX_CFG_TYPE_IMPSEL;
		break;
	case GPIO_CFG_TYPE_SLEW:
		*pinmux_cfg_type = PINMUX_CFG_TYPE_SLEW;
		break;
	default:
		TPT_ERROR(STR("cfg type %d is invalid", gpio_cfg_type));
		return GPIO_STATUS_INVALID_PARAM;

	}
	return GPIO_STATUS_SUCCESS;
}

static gpio_status_t
gpio_to_pinmux_cfg_value(gpio_cfg_value_t gpio_cfg_value,
                         pinmux_cfg_value_t *pinmux_cfg_value)
{
	switch(gpio_cfg_value) {
	case GPIO_CFG_VALUE_PULLSEL_NONE:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_PULLSEL_NONE;
		break;
	case GPIO_CFG_VALUE_PULLSEL_UP:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_PULLSEL_UP;
		break;
	case GPIO_CFG_VALUE_PULLSEL_DOWN:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_PULLSEL_DOWN;
		break;
	case GPIO_CFG_VALUE_IMPSEL_00:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_IMPSEL_00;
		break;
	case GPIO_CFG_VALUE_IMPSEL_01:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_IMPSEL_01;
		break;
	case GPIO_CFG_VALUE_IMPSEL_10:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_IMPSEL_10;
		break;
	case GPIO_CFG_VALUE_IMPSEL_11:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_IMPSEL_11;
		break;
	case GPIO_CFG_VALUE_SLEW_0:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_SLEW_0;
		break;
	case GPIO_CFG_VALUE_SLEW_1:
		*pinmux_cfg_value = PINMUX_CFG_VALUE_SLEW_1;
		break;
	default:
		TPT_ERROR(STR("cfg value %d is invalid", gpio_cfg_value));
		return GPIO_STATUS_INVALID_PARAM;

	}
	return GPIO_STATUS_SUCCESS;
}

static gpio_status_t reserve_pins(uint32_t number_of_pins,
                                  const uint32_t pin[],
                                  gpio_handle_t *handle)
{
	pinmux_status_t status;

	status = pinmux_reserve(number_of_pins, pin,
	                        &((*handle)->pinmux_handle));

	if(status == PINMUX_STATUS_SUCCESS) {
		status = pinmux_set_func((*handle)->pinmux_handle,
		                         number_of_pins,
		                         pin,
		                         PINMUX_FUNC_TYPE_GPIO);
		if(status != PINMUX_STATUS_SUCCESS) {
			(void) pinmux_unreserve((*handle)->pinmux_handle);
		}
	}

	return pinmux_status_to_gpio_status(status);
}


static gpio_status_t do_reserve(uint32_t number_of_pins,
                                const uint32_t pin[],
                                gpio_handle_t *handle)
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	struct hw_pin hw_pin;

	ret = check_param_valid(number_of_pins, pin, NULL, false);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	ret = init_handle(handle);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	ret = reserve_pins(number_of_pins, pin, handle);
	if( ret !=  GPIO_STATUS_SUCCESS) {
		TPT_ERROR(STR("Reserve pins failed with error %d",
		              ret));
		goto do_reserve_end;
	}

	for(uint32_t i = 0; i < number_of_pins; i++) {
		if(pin_to_hw_pin(pin[i], &hw_pin) == false) {
			ret = GPIO_STATUS_INVALID_PARAM;
			goto do_reserve_end;
		}
		ret = set_uio_mmap_and_register_addr(hw_pin, *handle);
		if(ret != GPIO_STATUS_SUCCESS) {
			goto do_reserve_end;
		}

		(*handle)->reserved_masks[hw_pin.key] |=
		        GENERATE_PIN_MASK(hw_pin.pin_num);

	}

do_reserve_end:
	if(ret != GPIO_STATUS_SUCCESS) {
		free_handle(*handle);
		*handle = NULL;
	}
	return ret;
}

static gpio_status_t unmap_uio(gpio_handle_t handle)
{
	int pthread_ret;

	pthread_ret = pthread_mutex_lock(&lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}

	for(uint32_t i = 0; i < sizeof(map_table) / sizeof(map_table[0]); i++) {
		if(handle->reg_addrs[i].pad != 0) {
			uint32_t group = map_table[i].group;
			uio_map_storage[group].num_of_users--;

			if(uio_map_storage[group].num_of_users == 0) {
				uio_map_storage[group].base = NULL;
				uio_close(uio_map_storage[group].handle);
				uio_map_storage[group].handle = NULL;
			}
		}
	}

	pthread_ret = pthread_mutex_unlock(&lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}
	return GPIO_STATUS_SUCCESS;
}

static gpio_status_t unreserve_pins(pinmux_handle_t pinmux_handle)
{
	pinmux_status_t pinmux_status;
	gpio_status_t ret_status;

	pinmux_status = pinmux_unreserve(pinmux_handle);
	ret_status = pinmux_status_to_gpio_status(pinmux_status);

	return ret_status;
}
static gpio_status_t do_unreserve(gpio_handle_t handle)
{
	gpio_status_t ret_status;

	unmap_uio(handle);
	ret_status = unreserve_pins(handle->pinmux_handle);

	free_handle(handle);

	return ret_status;
}

gpio_status_t gpio_reserve(uint32_t number_of_pins,
                           const uint32_t pin[],
                           gpio_handle_t *handle)
{
	return do_reserve(number_of_pins, pin, handle);
}

gpio_status_t gpio_unreserve(gpio_handle_t handle)
{
	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);
	return do_unreserve(handle);
}


gpio_status_t gpio_set_dir(gpio_handle_t handle,
                           uint32_t number_of_pins,
                           const uint32_t pin[],
                           const uint8_t value[])
{

	uint32_t *pin_mask = NULL;
	uint32_t *value_mask = NULL;
	uint32_t num_of_subgroups = 0;
	gpio_status_t ret = GPIO_STATUS_SUCCESS;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	ret = generate_all_pin_masks(handle, number_of_pins,
	                             pin, value, &pin_mask, &value_mask);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}
	num_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);
	for(uint32_t i = 0; i < num_of_subgroups; i++) {
		if(pin_mask[i] != 0) {
			*(handle->reg_addrs[i].dir_set) =
			        value_mask[i] & pin_mask[i];
			*(handle->reg_addrs[i].dir_clr) =
			        (~value_mask[i]) & pin_mask[i];
			DEBUG_TRACE(handle->reg_addrs[i]);
		}
	}
	free(pin_mask);
	free(value_mask);
	return ret;
}

gpio_status_t gpio_get_dir(gpio_handle_t handle,
                           uint32_t number_of_pins,
                           const uint32_t pin[],
                           uint8_t  value[])
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	struct hw_pin hw_pin;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	for(uint32_t i = 0; i < number_of_pins; i++) {
		uint32_t tmp_mask = 0;
		if(pin_to_hw_pin(pin[i], &hw_pin) == false) {
			ret = GPIO_STATUS_INVALID_PARAM;
			return ret;
		}
		tmp_mask = GENERATE_PIN_MASK(hw_pin.pin_num);
		if((handle->reserved_masks[hw_pin.key] & tmp_mask) == 0) {
			TPT_ERROR(STR("pin %d is not reserved", pin[i]));
			ret =  GPIO_STATUS_WRONG_STATE;
			return ret;
		}
		value[i] =
		        GENERATE_PIN_VALUE((*(handle->reg_addrs[hw_pin.key].dir)),
		                           hw_pin.pin_num);
		TPT_TRACE(3, STR("get dir value %d", value[i]));

	}

	return ret;
}


gpio_status_t gpio_write(gpio_handle_t handle,
                         uint32_t number_of_pins,
                         const uint32_t pin[],
                         const uint8_t value[])
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	uint32_t *pin_mask = NULL;
	uint32_t *value_mask = NULL;
	uint32_t num_of_subgroups = 0;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	ret = generate_all_pin_masks(handle, number_of_pins,
	                             pin, value, &pin_mask, &value_mask);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	num_of_subgroups = sizeof(map_table) / sizeof(map_table[0]);
	for(uint32_t i = 0; i < num_of_subgroups; i++) {
		if(pin_mask[i] != 0) {
			*(handle->reg_addrs[i].out_set) =
			        value_mask[i] & pin_mask[i];
			*(handle->reg_addrs[i].out_clr) =
			        (~value_mask[i]) & pin_mask[i];
			DEBUG_TRACE(handle->reg_addrs[i]);
		}
	}
	free(pin_mask);
	free(value_mask);
	return ret;
}

gpio_status_t gpio_read(gpio_handle_t handle,
                        uint32_t number_of_pins,
                        const uint32_t pin[],
                        uint8_t value[])
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	struct hw_pin hw_pin;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	for(uint32_t i = 0; i < number_of_pins; i++) {
		uint32_t tmp_mask = 0;
		if(pin_to_hw_pin(pin[i], &hw_pin) == false) {
			ret = GPIO_STATUS_INVALID_PARAM;
			return ret;
		}
		tmp_mask = GENERATE_PIN_MASK(hw_pin.pin_num);
		if((handle->reserved_masks[hw_pin.key] & tmp_mask) == 0) {
			TPT_ERROR(STR("pin %d is not reserved", pin[i]));
			ret =  GPIO_STATUS_WRONG_STATE;
			return ret;
		}
		value[i] =
		        GENERATE_PIN_VALUE(*(handle->reg_addrs[hw_pin.key].pad),
		                           hw_pin.pin_num);
		TPT_TRACE(3, STR("read value %d", value[i]));
		DEBUG_TRACE(handle->reg_addrs[hw_pin.key]);
	}
	return ret;
}

gpio_status_t gpio_set_cfg(gpio_handle_t handle,
                           uint32_t number_of_pins,
                           const uint32_t pin[],
                           gpio_cfg_type_t cfg_type,
                           gpio_cfg_value_t cfg_value)
{
	pinmux_status_t pinmux_status;
	pinmux_cfg_type_t pinmux_cfg_type;
	pinmux_cfg_value_t pinmux_cfg_value;
	gpio_status_t ret;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}
	ret = gpio_to_pinmux_cfg_value(cfg_value, &pinmux_cfg_value);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}
	ret = gpio_to_pinmux_cfg_type(cfg_type, &pinmux_cfg_type);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}
	pinmux_status = pinmux_set_cfg(handle->pinmux_handle,
	                               number_of_pins,
	                               pin,
	                               pinmux_cfg_type,
	                               pinmux_cfg_value);
	return pinmux_status_to_gpio_status(pinmux_status);
}

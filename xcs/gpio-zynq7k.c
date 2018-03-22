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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <gpio.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>

/*
 * MACROS
 */
#define TRACEPOINT_PROVIDER    com_ericsson_xcs_rhd_libgpio
#include "tpt_create.h"
#include "tpt.h"

#ifdef __MACHINE_ZYNQMP_MIMO
#define NUM_OF_PINS             174
#define NUM_OF_GROUPS           6
#else
#define NUM_OF_PINS             118
#define NUM_OF_GROUPS           4
#endif

#define MAX_BUF                 64
#define SYSFS_GPIO_DIR          "/sys/class/gpio"
#define MAGIC_NUM               0xEEEEEEEE
#define GPIO_IN_DIR             0

#define GENERATE_PIN_MASK(pin)  (1 << ((pin) & 0x1F))

#define REQUIRE(x, s) if (!(x)) do {                                 \
	TPT_ERROR("Expected '" #x "' to be true, returning: " #s);        \
	return s;                                                        \
} while(0)

/** structure and typedef */
struct gpio_handle {
	uint32_t *reserved_masks;              /* Store each subgroup reserved pin masks */
	uint32_t magic_num;                    /* check magic_num to avoid to use freed handle */
};

struct hw_pin {
	uint32_t key;
	uint32_t pin_num;
	uint32_t pin_mask;
};

struct pin_map {
	uint32_t first_pin_number; /* pin number named in sw interface */
	uint32_t last_pin_number;  /* pin number named in sw interface */
	uint32_t first_pin_number_in_group; /* pin number named in each bank */
	uint32_t last_pin_number_in_group;  /* pin number named in each bank */
};

/*
 * Local Variable Definition
 */

/* the table content meaning see struct pin_map
 * Column 0 - C0: first pin number (named in sw interface)
 * Column 1 - C1: last pin number (named in sw interface)
 * Column 2 - C2: first pin number in group (named in bank)
 * Column 3 - C3: last pin number in group (named in bank)
 **/
static struct pin_map map_table[NUM_OF_GROUPS] =
/*C0    C1    C2    C3*/
#ifdef __MACHINE_ZYNQMP_MIMO
{ {0,      25,    0,     25}, /* GPIO_0  - GPIO_25 */
  {26,     51,    0,     25}, /* GPIO_26 - GPIO_51 */
  {52,     77,    0,     25}, /* GPIO_52 - GPIO_77 */
  {78,    109,    0,     31}, /* GPIO_78 - GPIO_109 */
  {110,   141,    0,     31}, /* GPIO_110 - GPIO_141 */
  {142,	  173,     0,     31} /* GPIO_142 - GPIO_173 */
};
#else
{ {0,      31,    0,     31}, /* GPIO_0  - GPIO_31 */
  {32,     53,    0,     21}, /* GPIO_32 - GPIO_53 */
  {54,     85,    0,     31}, /* GPIO_54 - GPIO_85 */
  {86,    117,    0,     31} /* GPIO_86 - GPIO_117 */
};
#endif
static uint32_t gpio_reserved_masks[NUM_OF_GROUPS] = {0, 0};

static pthread_mutex_t gpio_mutex = PTHREAD_MUTEX_INITIALIZER;

/*
 * Local Function Declaration
 */
static gpio_status_t check_param_valid(uint32_t number_of_pins,
                                       const uint32_t pin[],
                                       gpio_handle_t handle,
                                       bool check_handle);
static gpio_status_t init_handle(gpio_handle_t *handle);
static void free_handle(gpio_handle_t handle);
static bool pin_to_hw_pin(uint32_t pin, struct hw_pin *hw_pin);
static gpio_status_t do_reserve(uint32_t pin, uint32_t key, uint32_t pin_mask);
static gpio_status_t do_unreserve(uint32_t pin, uint32_t key, uint32_t pin_mask);
static gpio_status_t do_set_dir(uint32_t pin, uint8_t dir);
static gpio_status_t do_read(uint32_t pin, uint8_t *value);
static gpio_status_t do_write(uint32_t pin, uint8_t value);

static gpio_status_t check_handle_valid(gpio_handle_t handle)
{
	if (handle->reserved_masks == NULL ||
	    handle->magic_num != MAGIC_NUM) {
		TPT_ERROR("handle is invalid");
		return GPIO_STATUS_INVALID_PARAM;
	}

	return GPIO_STATUS_SUCCESS;
}

static gpio_status_t check_pin_reserved(uint32_t number_of_pins,
                                        const uint32_t pin[],
                                        gpio_handle_t handle)
{
	struct hw_pin hw_pin;

	for (uint32_t i = 0; i < number_of_pins; i++) {
		if (pin_to_hw_pin(pin[i], &hw_pin) == false) {
			return GPIO_STATUS_INVALID_PARAM;
		}

		if ((handle->reserved_masks[hw_pin.key] &
		     hw_pin.pin_mask) == 0) {
			TPT_ERROR(STR("pin %d is not reserved", pin[i]));
			return GPIO_STATUS_WRONG_STATE;
		}
	}

	return GPIO_STATUS_SUCCESS;
}

static gpio_status_t check_param_valid(uint32_t number_of_pins,
                                       const uint32_t pin[],
                                       gpio_handle_t handle,
                                       bool check_handle)
{
	gpio_status_t ret;

	if (number_of_pins == 0 || number_of_pins > NUM_OF_PINS) {
		TPT_ERROR(STR("number_of_pins %d is out of range [1, %d]",
		              number_of_pins, NUM_OF_PINS));
		return GPIO_STATUS_INVALID_PARAM;
	}

	for (uint32_t i = 0; i < number_of_pins; i++) {
		if (pin[i] > (NUM_OF_PINS - 1)) {
			TPT_ERROR(STR("pin %d is out of range [0, %d]",
			              pin[i], NUM_OF_PINS - 1));
			return GPIO_STATUS_INVALID_PARAM;
		}
	}

	if (check_handle == true) {
		ret = check_handle_valid(handle);
		if (ret != GPIO_STATUS_SUCCESS) {
			return ret;
		}

		ret = check_pin_reserved(number_of_pins, pin, handle);
		if (ret != GPIO_STATUS_SUCCESS) {
			return ret;
		}
	}

	return GPIO_STATUS_SUCCESS;
}

static gpio_status_t init_handle(gpio_handle_t *handle)
{
	*handle = (gpio_handle_t)malloc(sizeof(struct gpio_handle));
	if (*handle == NULL) {
		TPT_ERROR(STR("malloc handle failed %d",
		              sizeof(struct gpio_handle)));
		goto init_handle_end;
	}

	/** init reserved_masks */
	(*handle)->reserved_masks =
	        (uint32_t *)calloc(NUM_OF_GROUPS, sizeof(uint32_t));

	if ((*handle)->reserved_masks == NULL) {
		TPT_ERROR(STR("calloc reserved_masks failed %d",
		              NUM_OF_GROUPS * sizeof(uint32_t)));
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
	if (handle != NULL) {
		free(handle->reserved_masks);
		handle->magic_num = 0;
		free(handle);
	}
	return;
}

static bool pin_to_hw_pin(uint32_t pin, struct hw_pin *hw_pin)
{
	for (uint32_t i = 0; i < NUM_OF_GROUPS; i++) {
		if (map_table[i].last_pin_number >= pin) {
			hw_pin->pin_num = pin - map_table[i].first_pin_number;
			hw_pin->pin_mask = GENERATE_PIN_MASK(hw_pin->pin_num);
			hw_pin->key = i;
			return true;
		}
	}
	return false;
}

static gpio_status_t do_reserve(uint32_t pin, uint32_t key, uint32_t pin_mask)
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	int fd = -1;
	char buf[MAX_BUF];
	int pthread_ret;

	pthread_ret = pthread_mutex_lock(&gpio_mutex);
	if (pthread_ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}

	if (gpio_reserved_masks[key] & pin_mask) {
		TPT_ERROR(STR("pin %d already reserved, key %d, masks 0x%x",
			      pin, key, gpio_reserved_masks[key]));
		ret = GPIO_STATUS_WRONG_STATE;
		goto out_error;
	}

	fd = open(SYSFS_GPIO_DIR "/export", O_WRONLY);
	if (fd == -1) {
		TPT_ERROR(STR("open %s/export error %d", SYSFS_GPIO_DIR, errno));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	if (snprintf(buf, sizeof(buf), "%d", pin) < 0) {
		TPT_ERROR(STR("snprintf %d error", pin));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	if (write(fd, buf, strlen(buf)) < 0) {
		TPT_ERROR(STR("write %d to export error %d", pin, errno));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	gpio_reserved_masks[key] |= pin_mask;
	TPT_INFO(STR("reserved pin %d, key %d, masks 0x%x",
		     pin, key, gpio_reserved_masks[key]));

out_error:
	if (fd != -1) {
		if (close(fd) < 0) {
			TPT_ERROR(STR("close %s/export error %d", SYSFS_GPIO_DIR, errno));
			ret = GPIO_STATUS_OTHER;
		}
	}
	pthread_mutex_unlock(&gpio_mutex);
	return ret;
}

static gpio_status_t do_unreserve(uint32_t pin, uint32_t key, uint32_t pin_mask)
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	int fd = -1;
	char buf[MAX_BUF];
	struct stat status;
	int pthread_ret;

	pthread_ret = pthread_mutex_lock(&gpio_mutex);
	if (pthread_ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}

	/* First check for directory existence. */
	if (snprintf(buf, sizeof(buf), SYSFS_GPIO_DIR "/gpio%d/value", pin) < 0) {
		TPT_ERROR(STR("snprintf %s/gpio%d/value error",
		              SYSFS_GPIO_DIR, pin));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	/**
	 * If the directory does not exist, then do not even try to release the GPIO.
	 */
	if (!stat(buf, &status)) {
		fd = open(SYSFS_GPIO_DIR "/unexport", O_WRONLY);
		if (fd == -1) {
			TPT_ERROR(STR("open %s/unexport error %d", SYSFS_GPIO_DIR, errno));
			ret = GPIO_STATUS_OTHER;
			goto out_error;
		}

		if (snprintf(buf, sizeof(buf), "%d", pin) < 0) {
			TPT_ERROR(STR("snprintf %d error", pin));
			ret = GPIO_STATUS_OTHER;
			goto out_error;
		}

		if (write(fd, buf, strlen(buf)) < 0) {
			TPT_ERROR(STR("write %d to unexport error %d", pin, errno));
			ret = GPIO_STATUS_OTHER;
			goto out_error;
		};
	}

	gpio_reserved_masks[key] &= ~pin_mask;
	TPT_INFO(STR("unreserved pin %d, key %d, masks 0x%x",
		     pin, key, gpio_reserved_masks[key]));

out_error:
	if (fd != -1) {
		if (close(fd) < 0) {
			TPT_ERROR(STR("close %s/unexport error %d", SYSFS_GPIO_DIR, errno));
			ret = GPIO_STATUS_OTHER;
		}
	}
	pthread_mutex_unlock(&gpio_mutex);
	return ret;
}

static gpio_status_t do_set_dir(uint32_t pin, uint8_t dir)
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	int fd = -1;
	char buf[MAX_BUF];
	int pthread_ret;

	pthread_ret = pthread_mutex_lock(&gpio_mutex);
	if (pthread_ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}

	/* set direction */
	if (snprintf(buf, sizeof(buf), SYSFS_GPIO_DIR "/gpio%d/direction", pin)
	    < 0) {
		TPT_ERROR(STR("snprintf %s/gpio%d/direction error",
		              SYSFS_GPIO_DIR, pin));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	fd = open(buf, O_WRONLY);
	if (fd == -1) {
		TPT_ERROR(STR("open %s error %d", buf, errno));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	if (dir == GPIO_IN_DIR) {
		if (write(fd, "in", sizeof("in") - 1) < 0) {
			TPT_ERROR(STR("write in to %s error %d", buf, errno));
			ret = GPIO_STATUS_OTHER;
			goto out_error;
		}
	} else {
		if (write(fd, "out", sizeof("out") - 1) < 0) {
			TPT_ERROR(STR("write out to %s error %d", buf, errno));
			ret = GPIO_STATUS_OTHER;
			goto out_error;
		}
	}

out_error:
	if (fd != -1) {
		if (close(fd) < 0) {
			TPT_ERROR(STR("close %s error %d", buf, errno));
			ret = GPIO_STATUS_OTHER;
		}
	}
	pthread_mutex_unlock(&gpio_mutex);
	return ret;
}

static gpio_status_t do_read(uint32_t pin, uint8_t *value)
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	char cval = 0;
	char buf[MAX_BUF];
	int fd = -1;
	int pthread_ret;

	pthread_ret = pthread_mutex_lock(&gpio_mutex);
	if (pthread_ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}

	if (snprintf(buf, sizeof(buf), SYSFS_GPIO_DIR "/gpio%d/value", pin)
	    < 0) {
		TPT_ERROR(STR("snprintf %s/gpio%d/value error",
		              SYSFS_GPIO_DIR, pin));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	fd = open(buf, O_RDONLY);
	if (fd == -1) {
		TPT_ERROR(STR("open %s error %d", buf, errno));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	if (read(fd, &cval, 1) < 0) {
		TPT_ERROR(STR("read %s error %d", buf, errno));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	*value = (cval == '0' ? 0 : 1);

out_error:
	if (fd != -1) {
		if (close(fd) < 0) {
			TPT_ERROR(STR("close %s error %d", buf, errno));
			ret = GPIO_STATUS_OTHER;
		}
	}
	pthread_mutex_unlock(&gpio_mutex);
	return ret;
}

static gpio_status_t do_write(uint32_t pin, uint8_t value)
{
	gpio_status_t ret = GPIO_STATUS_SUCCESS;
	char cval = (value > 0 ? '1' : '0');
	char buf[MAX_BUF];
	int fd = -1;
	int pthread_ret;

	pthread_ret = pthread_mutex_lock(&gpio_mutex);
	if (pthread_ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return GPIO_STATUS_OTHER;
	}

	if (snprintf(buf, sizeof(buf), SYSFS_GPIO_DIR "/gpio%d/value", pin)
	    < 0) {
		TPT_ERROR(STR("snprintf %s/gpio%d/value error",
		              SYSFS_GPIO_DIR, pin));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	fd = open(buf, O_WRONLY);
	if (fd == -1) {
		TPT_ERROR(STR("open %s error %d", buf, errno));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

	if (write(fd, &cval, 1) < 0) {
		TPT_ERROR(STR("write %d to %s error %d", value, buf, errno));
		ret = GPIO_STATUS_OTHER;
		goto out_error;
	}

out_error:
	if (fd != -1) {
		if (close(fd) < 0) {
			TPT_ERROR(STR("close %s error %d", buf, errno));
			ret = GPIO_STATUS_OTHER;
		}
	}
	pthread_mutex_unlock(&gpio_mutex);
	return ret;
}

gpio_status_t gpio_reserve(uint32_t number_of_pins,
                           const uint32_t pin[],
                           gpio_handle_t *handle)
{
	gpio_status_t ret;
	struct hw_pin hw_pin;
	bool reserved = false;

	ret = check_param_valid(number_of_pins, pin, NULL, false);
	if (ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	ret = init_handle(handle);
	if (ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	for (uint32_t i = 0; i < number_of_pins; i++) {
		if (pin_to_hw_pin(pin[i], &hw_pin) == false) {
			ret = GPIO_STATUS_INVALID_PARAM;
			goto reserve_end;
		}

		ret = do_reserve(pin[i], hw_pin.key, hw_pin.pin_mask);
		if (ret != GPIO_STATUS_SUCCESS) {
			goto reserve_end;
		}

		reserved = true;
		(*handle)->reserved_masks[hw_pin.key] |= hw_pin.pin_mask;
	}

reserve_end:
	if(ret != GPIO_STATUS_SUCCESS) {
		if (reserved == true) {
			gpio_unreserve(*handle);
		} else {
			free_handle(*handle);
		}
		*handle = NULL;
	}
	return ret;
}

gpio_status_t gpio_unreserve(gpio_handle_t handle)
{
	gpio_status_t ret;
	uint32_t masks;
	uint32_t pin;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_handle_valid(handle);
	if (ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	for (uint32_t i = 0; i < NUM_OF_GROUPS; i++) {
		masks = handle->reserved_masks[i];
		if (masks == 0) {
			continue;
		}

		for (uint32_t j = 0; j <= map_table[i].last_pin_number_in_group; j++) {
			if ((masks >> j) & 0x1) {
				pin = j + map_table[i].first_pin_number;
				ret = do_unreserve(pin, i, 1 << j);
				if (ret != GPIO_STATUS_SUCCESS) {
					goto unreserve_end;
				}
			}
		}
	}

unreserve_end:
	free_handle(handle);
	return ret;
}

gpio_status_t gpio_set_dir(gpio_handle_t handle,
                           uint32_t number_of_pins,
                           const uint32_t pin[],
                           const uint8_t value[])
{
	gpio_status_t ret;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	for (uint32_t i = 0; i < number_of_pins; i++) {
		ret = do_set_dir(pin[i], value[i]);
		if (ret != GPIO_STATUS_SUCCESS) {
			return ret;
		}
	}

	return ret;
}

gpio_status_t gpio_write(gpio_handle_t handle,
                         uint32_t number_of_pins,
                         const uint32_t pin[],
                         const uint8_t value[])
{
	gpio_status_t ret;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	for (uint32_t i = 0; i < number_of_pins; i++) {
		ret = do_write(pin[i], value[i]);
		if (ret != GPIO_STATUS_SUCCESS) {
			return ret;
		}
	}

	return ret;
}

gpio_status_t gpio_read(gpio_handle_t handle,
                        uint32_t number_of_pins,
                        const uint32_t pin[],
                        uint8_t value[])
{
	gpio_status_t ret;

	REQUIRE(handle, GPIO_STATUS_INVALID_PARAM);

	ret = check_param_valid(number_of_pins, pin, handle, true);
	if(ret != GPIO_STATUS_SUCCESS) {
		return ret;
	}

	for (uint32_t i = 0; i < number_of_pins; i++) {
		ret = do_read(pin[i], &value[i]);
		if (ret != GPIO_STATUS_SUCCESS) {
			return ret;
		}
	}

	return ret;
}

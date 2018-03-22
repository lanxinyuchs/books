/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <fcntl.h>
#include <sys/file.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <unistd.h>
#include <signal.h>
#include <getopt.h>
#include <errno.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/signalfd.h>
#include <sys/queue.h>

#include <itc.h>

#include <conn-establish-helper.h>
#include <conn-establish.h>

#include <nvpi3.h>

#include "atfmi_internal.h"
#include "atfmi.h"
#include "atfmi.sig"

#include "pinmux.h"

#include <sys/mman.h>

/* Logging */
#include "com_ericsson_atfmi_server.h"

#define ATFMI_MAX_NUM_MBOX 16
#define ATFMI_DAEMON_NAME  "atfmid"
#define ATFMI_LOCK_PREFIX  "/LCK.."
#define ATFMI_LOCK_PATH    "/var/lock"
#define ATFMI_LOCK_FILE    ATFMI_LOCK_PATH ATFMI_LOCK_PREFIX ATFMI_DAEMON_NAME

#define ATFMI_RECEIVE_TMO  1000

#define UIO_ROOT "/sys/class/uio"

union itc_msg {
	uint32_t msgno;
	ATFMI_MESSAGES;
};

static struct daemon_head llist;
static pinmux_t *pinmux_p = NULL;

static nvpi3_db_group_handle
open_db(char *group_name, atfmi_result_t *code);

static void
close_db(nvpi3_db_group_handle db_group_handle, atfmi_result_t *code);

static void
close_node(nvpi3_node_handle node_handle, atfmi_result_t *code);

static nvpi3_node_handle db_find_node(nvpi3_db_group_handle db_handle,
                                      const char* node,
                                      atfmi_result_t *code);

static uint32_t
get_value_size(nvpi3_node_handle node_handle,
		char *key_name,
		uint32_t value_type,
		atfmi_result_t *res);

static void
get_value(nvpi3_node_handle node_handle,
          char *key_name,
          uint32_t value_type,
          uint32_t value_buff_size,
          union nvpi3_key_value *value_buff,
          uint32_t *res);

static void
create_init_rsp_msg(union itc_msg **reply,
                    struct conn_client_info *ci,
                    atfmi_result_t res);

static void
create_shutdown_rsp_msg(union itc_msg **reply,
                        struct conn_client_info *ci,
                        atfmi_result_t res);

static inline bool is_profile_used(struct daemon_head *llist,
                                   const char* profile)
{
	struct daemon *dm;
	TAILQ_FOREACH(dm, llist, lentry)
	{
		if (strcmp(profile, dm->profile) == 0) {
			TPT_TRACE(1, STR(
			       "profile %s used with pid %d \n",
			       dm->profile,
			       dm->pid));
			return true;
		}
	}

	return false;
}

static inline struct daemon *child_get_from_pid(struct daemon_head *llist,
                                                pid_t pid)
{
	struct daemon *dm;
	TAILQ_FOREACH(dm, llist, lentry)
	{
		if (dm->pid == pid)
			return dm;
	}
	return 0;
}

static inline bool send_sys_sig(struct daemon *dm, int sig)
{
	dm->state = STATE_TERMINATED;
	int ret = kill(dm->pid, sig);

	if (ret == 0) {
		TPT_TRACE(1, STR("sent signal %d to pid %d", sig, dm->pid));
		return OK;
	}
	else if (ret == -1 && errno == ESRCH) {
		TPT_INFO(STR(
		       "the daemon with pid %d had already quit",
		       dm->pid));
		return OK;
	}
	else {
		TPT_ERROR(STR("failed to send %d to child with pid %d (%s)",
		       sig,
		       dm->pid,
		       strerror(errno)));
	}
	return FAIL;

}

static inline void daemon_free_all(struct daemon_head *llist)
{
	struct daemon *next;
	struct daemon *dm = TAILQ_FIRST(llist);

	while (dm) {
		if (dm->argv) {
			int i;
			const char *arg;
			for (i = 0, arg = dm->argv[0]; arg; i++, arg =
			     dm->argv[i])
				free((void *) arg);

			free(dm->argv);
		}

		next = TAILQ_NEXT(dm, lentry);
		free(dm);
		dm = next;
	}
}

static inline void daemon_free_single(struct daemon *dm)
{
	if (dm->argv) {
		int i;
		const char *arg;
		for (i = 0, arg = dm->argv[0]; arg; i++, arg = dm->argv[i])
			free((void *) arg);

		free(dm->argv);
	}

	free(dm);
}

static bool launch_daemon(struct daemon *dm)
{
	pid_t daemon_pid = fork();

	if (daemon_pid == -1) {
		TPT_ERROR("fork failed");
		return FAIL;
	}
	else if (daemon_pid == 0) {
		/*we don't want our child
		 * has the parent's sigmask, so reset it*/
		sigset_t mask;
		sigemptyset(&mask);
		if (pthread_sigmask(SIG_SETMASK, &mask, NULL) == -1) {
			TPT_ERROR("sigprocmask failed");
			return FAIL;
		}
		execv(dm->argv[0], dm->argv);
		/*if returns here exit*/
		return FAIL;
	}
	/*parent*/
	dm->ppid = getpid();
	dm->pid = daemon_pid;
	dm->state = STATE_STARTED;
	TPT_TRACE(1, STR(
	       "launched %s pid %d ppid %d",
	       dm->argv[0],
	       daemon_pid,
	       dm->ppid));

	return OK;
}


static const char *get_dev_name(char (*dev)[], const char *name)
{
	DIR           *dir;
	FILE          *fdd;
	struct dirent *dir_e;
	char           name_path[256], name_buf[64];

	dir = opendir(UIO_ROOT);
	dir_e = readdir(dir);
	while ((dir_e = readdir(dir))) {
		memset(name_path, 0, sizeof(name_path));
		memset(name_buf, 0, sizeof(name_buf));
		if (dir_e) {
			if (strstr(dir_e->d_name, "."))
				continue;
			sprintf(name_path, UIO_ROOT "/%s/name",
			        dir_e->d_name);
			fdd = fopen(name_path, "r");
			if (fdd == NULL) {
				closedir(dir);
				return NULL;
			}
			if (fscanf(fdd, "%s\n", name_buf) == 1 &&
			    !strcmp(name, name_buf)) {
				sprintf(*dev, "%s", dir_e->d_name);
				closedir(dir);
				fclose(fdd);
				return *dev;
			}
			fclose(fdd);
		}
	}
	closedir(dir);

	return NULL;
}

static const char *get_map_property(char (*prop_out)[], const char *prop,
                                    const char *dev, int map_no)
{
	FILE *fp;
	char  path[256];

	snprintf(path, sizeof(path), UIO_ROOT "/%s/maps/map%u/%s", dev,
	         map_no, prop);
	fp = fopen(path, "r");
	if (fp == NULL)
		return NULL;
	fgets(*prop_out, 32, fp);
	fclose(fp);

	return *prop_out;
}

static nvpi3_db_group_handle open_db(char *group_name, atfmi_result_t *code)
{
	nvpi3_result_t res;
	nvpi3_db_group_handle db_handle = NULL;
	*code = ATFMI_SUCCESS;

	res = nvpi3_open_db_group(group_name, &db_handle);

	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("failed to open database result %u", res));
		*code = ATFMI_INVALID_PARAMETER;
		return NULL;
	}
	return db_handle;
}

static void close_db(nvpi3_db_group_handle db_group_handle,
                     atfmi_result_t *code)
{
	uint32_t res;
	*code = ATFMI_SUCCESS;
	res = nvpi3_close_db_group(db_group_handle);

	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("failed to close DB result %u", res));
		*code = ATFMI_INVALID_PARAMETER;
	}
	return;
}

static nvpi3_node_handle db_find_node(nvpi3_db_group_handle db_handle,
                                      const char* node,
                                      atfmi_result_t *code)
{
	nvpi3_node_handle node_handle;
	nvpi3_result_t res;
	*code = ATFMI_SUCCESS;

	res = nvpi3_open_node(db_handle, node, &node_handle);

	if (res == NVPI3_RESULT_NOT_FOUND) {
		TPT_TRACE(1, STR(
		       "node %s wasn't found in DB result %u",
		       node, res));
		*code = ATFMI_PROFILE_NOT_FOUND;
	}
	else if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("find node failed for %s result %u", node, res));
		*code = ATFMI_INVALID_PARAMETER;
	}

	return node_handle;
}

static void close_node(nvpi3_node_handle node_handle, atfmi_result_t *code)
{
	nvpi3_result_t res;
	*code = ATFMI_SUCCESS;

	res = nvpi3_close_node(node_handle);
	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("failed to close node result %u", res));
		*code = ATFMI_INVALID_PARAMETER;
	}
	return;
}

static void get_value(nvpi3_node_handle node_handle,
                      char *key_name,
                      uint32_t value_type,
                      uint32_t value_buff_size,
                      union nvpi3_key_value *value_buff,
                      atfmi_result_t *res)
{

	*res = nvpi3_get_value(node_handle,
	                       key_name,
	                       value_type,
	                       &value_buff_size,
	                       value_buff);

}

static uint32_t get_value_size(nvpi3_node_handle node_handle,
		char *key_name,
		uint32_t value_type,
		atfmi_result_t *res)
{
	uint32_t value_size;

	*res = nvpi3_get_value_size(node_handle,
			key_name,
			value_type,
			&value_size);
	return value_size;
}


static bool launch_ATF(char *dev_file, uint32_t port, bool isXXP,
                       bool mux, const char* profile)
{
	struct daemon *dm = NULL;
	uint8_t args = 6; /*min number of args to start atf*/
	char buff[8 + 1], thread[] = ATF_SERVER_NAME_ANP;

	dm = calloc(1, sizeof(struct daemon));
	if (!dm) {
		TPT_ERROR("calloc failed");
		return FAIL;
	}

	TAILQ_INSERT_TAIL(&llist, dm, lentry);

	/*/usr/bin/atfid -n -d /dev/tty/ECB -s MXP_0*/
	/*-m libmux.so wcdma 7*/

	if(mux)
		args = 10;

	dm->argv = malloc((args + 1) * sizeof(char *));
	if (!(dm->argv)) {
		TPT_ERROR(STR("malloc failed %s", dev_file));
		free(dm);
		return FAIL;
	}

	if (!isXXP)
		strcpy(thread, ATF_SERVER_NAME_ECB);
	snprintf(buff, sizeof(buff), "%s_%u", thread, port);

	dm->argv[5] = strdup(buff);
	dm->argv[4] = strdup(ATFMI_SERVER_OPTION);
	dm->argv[3] = strdup(dev_file);
	dm->argv[2] = strdup(ATFMI_DEVICE_OPTION);
	dm->argv[1] = strdup(ATFMI_NOT_DAEMON_OPTION);
	dm->argv[0] = strdup(ATFID_DAEMON_PATH);

	if (mux) {
		TPT_TRACE(1, "mux exists in DTB");

		char str_num[4 + 1];
		snprintf(str_num, sizeof(str_num), "%u", port);

		dm->argv[6] = strdup(ATFMI_MUX_OPTION);
		dm->argv[7] = strdup(ATF_MUX_LIB);
		dm->argv[8] = strdup(profile);
		dm->argv[9] = strdup(str_num);
		TPT_TRACE(1, "populated mux array");
	}
	dm->argv[args] = 0; /*delimiter*/

	if (launch_daemon(dm)) {
		TPT_ERROR(STR("unable to launch %s", dm->argv[0]));
		/*TODO: how should we handle this, just return err
		 * code to app and board will go restart?
		 * are any other cleaning need to be performed?*/
		daemon_free_all(&llist);
		return FAIL;
	}
	strcpy(dm->profile, profile);

	return OK;
}

static bool launch_A4CI(char* dev_file, const char* profile)
{
	struct daemon *dm = NULL;
	uint8_t args = 4; /*min number of args to start A4CI*/

	dm = calloc(1, sizeof(struct daemon));
	if (!dm) {
		TPT_ERROR(STR("calloc failed for %s", dev_file));
		return FAIL;
	}

	TAILQ_INSERT_TAIL(&llist, dm, lentry);

	/*/usr/bin/a4cid -n -d /dev/tty/ECB*/

	dm->argv = malloc((args + 1) * sizeof(char *));
	if (!dm->argv) {
		TPT_ERROR(STR("malloc failed for %s", dev_file));
		free(dm);
		return FAIL;
	}
	dm->argv[4] = 0; /*delimiter*/
	dm->argv[3] = strdup(dev_file);
	dm->argv[2] = strdup(ATFMI_DEVICE_OPTION);
	dm->argv[1] = strdup(ATFMI_NOT_DAEMON_OPTION);
	dm->argv[0] = strdup(A4CID_DAEMON_PATH);

	if (launch_daemon(dm)) {
		TPT_ERROR("couldn't launch daemon");
		daemon_free_all(&llist);
		return FAIL;
	}
	strcpy(dm->profile, profile);
	return OK;
}

static inline bool parse_tty_path(char *target, char *device, char *tty)
{
	char delim[] = "/";
	char *token, *keep_last_tkn = NULL, *prs;
	/*/sys/devices/88009000.anp_uart/tty/ttyS2*/
	prs = strstr(target, ".");

	if(prs) {

		token = strtok(prs, delim);
		/*strip dot from token*/
		if (strcmp(token + 1, device) == 0) {
			/*loop till the end and store last token*/
			for (; token; token = strtok(NULL, delim))
				keep_last_tkn = token;
			strcpy(tty, keep_last_tkn);
			return true;
		}
	}
	return false;
}

static void find_dev_tty(DIR *dir, char* dev, atfmi_result_t *code)
{
	/*TODO: use PATH_MAX from limits.h?*/
	char resolved_path[ATFMI_MAX_PATH_SIZE];
	char link_name[ATFMI_MAX_PATH_SIZE], tty[ATFMI_MAX_PATH_SIZE];

	struct dirent *dp;
	char *ret;

	while ((dp = readdir(dir)) != NULL) {

		snprintf(link_name,
		         sizeof(link_name),
		         "/sys/class/tty/%s",
		         dp->d_name);

		ret = realpath(link_name, resolved_path);
		if (!ret) {
			TPT_ERROR(STR("realpath failed (%d)", errno));
			*code = ATFMI_OTHER_ERROR;
			return;
		}

		if (parse_tty_path(resolved_path, dev, tty)) {
			snprintf(dev,
			         ATFMI_MAX_DEV_SIZE,
			         "/dev/%s", tty);
			TPT_TRACE(1, STR("found dev tty: %s\n", dev));
			*code = ATFMI_SUCCESS;
			return;
		}
	}
	*code = ATFMI_OTHER_ERROR;
}
static void prepare_and_launch_ecb_daemons(nvpi3_node_handle db_port_handle,
                                           char *dev,
                                           uint32_t port,
                                           const char *profile,
                                           atfmi_result_t *code)
{
	uint32_t res;
	char token_to_cmp[] = "/dev/";
	DIR *dir;

	get_value(db_port_handle, "device-file", NVPI3_KEY_TYPE_STR,
	          ATFMI_MAX_DEV_SIZE, (union nvpi3_key_value *) dev, &res);
	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("get_value error %u ", res));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	if (strncmp(dev, token_to_cmp, 5) != 0) {
		dir = opendir("/sys/class/tty/");
		if (!dir) {
			TPT_ERROR("unable to open directory");
			*code = ATFMI_OTHER_ERROR;
			return;
		}
		find_dev_tty(dir, dev, code);
		if (*code != ATFMI_SUCCESS) {
			TPT_ERROR("unable to find tty for provided dev");
			closedir(dir);
			return;
		}
		/*TODO: maybe goto for this need to be created when
		 * new dtb will be merged*/
		if (closedir(dir)) {
			TPT_ERROR("unable to close directory");
			*code = ATFMI_OTHER_ERROR;
			return;
		}

	}

	if (launch_ATF(dev,
	               port,
	      /*isXXP*/false,
	   /*have_mux*/false,
	               profile)) {
		*code = ATFMI_OTHER_ERROR;
		return;
	}

	if (launch_A4CI(dev, profile)) {
		*code = ATFMI_OTHER_ERROR;
		return;
	}
	*code = ATFMI_SUCCESS;
	return;
}


static void prepare_and_launch_atf_daemon(nvpi3_node_handle db_port_handle,
                                          char *dev,
                                          uint32_t port,
                                          bool have_mux,
                                          const char* profile,
                                          atfmi_result_t *code)
{
	uint32_t res;
	char token_to_cmp[] = "/dev/";

	DIR *dir;

	get_value(db_port_handle, "device-file", NVPI3_KEY_TYPE_STR,
	          ATFMI_MAX_DEV_SIZE, (union nvpi3_key_value *) dev, &res);
	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("get_value error %u ", res));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	if (strncmp(dev, token_to_cmp, 5) != 0) {
		dir = opendir("/sys/class/tty/");
		if (!dir) {
			TPT_ERROR("unable to open directory");
			*code = ATFMI_OTHER_ERROR;
			return;
		}
		find_dev_tty(dir, dev, code);
		if (*code != ATFMI_SUCCESS) {
			TPT_ERROR("unable to find tty for provided dev");
			closedir(dir);
			return;
		}
		/*TODO: maybe goto for this need to be created when
		 * new dtb will be merged*/
		if(closedir(dir)) {
			TPT_ERROR("unable to close directory");
			*code = ATFMI_OTHER_ERROR;
			return;
		}

	}

	if (launch_ATF(dev,
	               port,
	               true, /*isXXP*/
	               have_mux,
	               profile)) {
		*code = ATFMI_OTHER_ERROR;
		return;
	}
	*code = ATFMI_SUCCESS;
}

static void unres_pins(pinmux_handle_t handle, atfmi_result_t *code)
{
	pinmux_status_t status;

	status = pinmux_unreserve(handle);
	if (status != PINMUX_STATUS_SUCCESS) {
		TPT_ERROR(STR("Failed(%u) to unreserve pinmux", status));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	*code = ATFMI_SUCCESS;
}
static void set_pins(uint32_t pins[],
                     uint32_t mode[],
                     uint32_t no_of_pins,
                     pin_action_t act,
                     atfmi_result_t *code)
{

	pinmux_status_t status;

	do {
		no_of_pins--;
		if (act == ACTION_SET) {
			/*reserve pin only once*/
			if (pinmux_p->refcnt[pins[no_of_pins]] == 0) {
				status = pinmux_reserve(1,
				                        &pins[no_of_pins],
				                        &(pinmux_p->handle[pins[no_of_pins]]));
				if (status != PINMUX_STATUS_SUCCESS) {
					TPT_ERROR(STR(
					       "Failed(%u) to reserve pinmux",
					       status));
					*code = ATFMI_INVALID_PARAMETER;
					return;
				}
			}
			status = pinmux_set_func(pinmux_p->handle[pins[no_of_pins]],
			                         1,
			                         &pins[no_of_pins],
			                         mode[no_of_pins]);

			if (status == PINMUX_STATUS_SUCCESS) {
				TPT_TRACE(1, STR(
				       "Success. Pin %u set to mode %u",
				       pins[no_of_pins],
				       mode[no_of_pins]));
			} else {
				TPT_ERROR(STR(
				       "Failed(%u) to setfunc pinmux",
				       status));
				*code = ATFMI_INVALID_PARAMETER;
				return;
			}

			pinmux_p->refcnt[pins[no_of_pins]]++;

			*code = ATFMI_SUCCESS;

		} else { /*unset */

			if (pinmux_p->refcnt[pins[no_of_pins]] > 0) {
				unres_pins(pinmux_p->handle[pins[no_of_pins]],
				           code);
				if (*code != ATFMI_SUCCESS)
					return;
				pinmux_p->refcnt[pins[no_of_pins]]--;
				TPT_TRACE(1, STR(
				       "Success. pin (%u) unreserved",
				       pins[no_of_pins]));

			} else {
				TPT_ERROR(STR(
				       "pin(%u) was never set",
				       pins[no_of_pins]));
				*code = ATFMI_INVALID_PARAMETER;
				return;
			}
		}
	} while (no_of_pins);

}

static void get_pin_mux_param(nvpi3_node_handle node, pin_action_t action,
                              uint32_t *code)
{
	uint32_t pin_size = 0, mode_size = 0, res, nof_entries = 0;
	uint32_t pin_val[ATFMI_MAX_UART_PINS] = { 0 };
	uint32_t mode_val[ATFMI_MAX_UART_PINS] = { 0 };

	/* Check size of parameters */
	pin_size = get_value_size(node, "pin", NVPI3_KEY_TYPE_U32, &res);
	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("Failed (%u) to get size of PMUX 'pin'", res));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}
	/* Check size of parameters */
	mode_size = get_value_size(node, "mode", NVPI3_KEY_TYPE_U32, &res);
	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("Failed (%u) to get size of PMUX 'mode'", res));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	if (pin_size != mode_size) {
		TPT_ERROR("Mismatch PIN_MUX 'pin' and 'mode' size");
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}
	get_value(node,
	          "pin",
	          NVPI3_KEY_TYPE_U32,
	          pin_size,
	          (union nvpi3_key_value *) pin_val,
	          &res);
	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("Failed (%u) to get PIN_MUX 'pin' value", res));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	get_value(node,
	          "mode",
	          NVPI3_KEY_TYPE_U32,
	          mode_size,
	          (union nvpi3_key_value *) mode_val,
	          &res);
	if (res != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("Failed (%u) to get PIN_MUX 'mode' value", res));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}
	/*if type of entry in DTB changes this wont fly*/
	nof_entries = mode_size / sizeof(uint32_t);

	set_pins(pin_val, mode_val, nof_entries, action, code);
	if (*code != ATFMI_SUCCESS) {
		TPT_ERROR(STR("Failed (%u) to set func for pins", *code));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}
}

static void handle_pin_mux(nvpi3_db_group_handle db_group_handle,
                        const char* profile, pin_action_t action,
                        atfmi_result_t *code)
{
	nvpi3_node_handle db_pin_mux_handle;
	char pin_node[256];
	uint32_t res, i = 0;

	while (1) {
		snprintf(pin_node, sizeof(pin_node), "%s/pmux%d/", profile, i);

		db_pin_mux_handle = db_find_node(db_group_handle,
		                                 pin_node,
		                                 code);
		if (*code == ATFMI_PROFILE_NOT_FOUND && !i) {
			TPT_INFO("no pinmux configured");
			*code = ATFMI_SUCCESS;
			return;
		} else if (*code == ATFMI_PROFILE_NOT_FOUND) {
			TPT_TRACE(1, STR("didn't find node %s", pin_node));
			*code = ATFMI_SUCCESS;
			return;
		} else if (*code != ATFMI_SUCCESS) {
			TPT_ERROR(STR(
			       "error in DB while checking %s",
			       pin_node));
			goto err_close;
		}
		get_pin_mux_param(db_pin_mux_handle, action, code);
		if (*code != ATFMI_SUCCESS) {
			TPT_ERROR(STR(
			       "failed to get/set (%u) pin mux param",
			       *code));
			goto err_close;
		}
		close_node(db_pin_mux_handle, code);
		if (*code != ATFMI_SUCCESS)
			return;

		i++;
	}
	return;
err_close:
	close_node(db_pin_mux_handle, &res);
	return;
}

static void write_uart_reg(struct uio *uio)
{
	*uio->addr = (*uio->addr & ~uio->mask) | (uio->value & uio->mask);
	return;
}

static void init_uart_uio(struct uio *uart_uio,
                          char *name,
                          atfmi_result_t *code)
{
	char dev_name[32], dev_path[32], tmp[32];
	int fd, page_size, page_offset;
	uint32_t base, size;
	void *map_addr;
	size_t map_size;

	if (get_dev_name(&dev_name, name) == NULL) {
		TPT_ERROR(STR("Failed to get device name for \"%s\"", name));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	snprintf(dev_path, sizeof(dev_path), "/dev/%s", dev_name);

	if (get_map_property(&tmp, "addr", dev_name, 0) == NULL) {
		TPT_ERROR(STR("Failed get map property for \"%s\"", name));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}
	base = strtoul(tmp, NULL, 16);

	if (get_map_property(&tmp, "size", dev_name, 0) == NULL) {
		TPT_ERROR(STR("Failed get map property for \"%s\"", name));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}
	size = strtoul(tmp, NULL, 16);

	page_size = getpagesize();
	page_offset = base % page_size;

	/* Calculate actual size spanning over the whole required page range */
	map_size = (page_offset + size + page_size - 1) & ~(page_size - 1);
	map_addr = (void*) (base - page_offset);

	fd = open(dev_path, O_RDWR | O_SYNC);
	if (fd == -1) {
		TPT_ERROR(STR("Failed (%d) to open \"%s\"", errno, dev_path));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	uart_uio->map_addr = mmap(map_addr,
	                     map_size,
	                     PROT_READ | PROT_WRITE,
	                     MAP_SHARED,
	                     fd,
	                     0);
	close(fd);

	if (uart_uio->map_addr == MAP_FAILED) {
		TPT_ERROR(STR("Failed (%d) to map %u bytes for UIO at 0x%x",
		errno, map_size, (int) map_addr));
		*code = ATFMI_INVALID_PARAMETER;
		return;
	}

	uart_uio->addr = &((uint32_t*) map_addr)[uart_uio->offset >> 2];

	return;

}

static void handle_uart_uio(nvpi3_db_group_handle db_group_handle,
                            const char* profile,
                            atfmi_result_t *code)
{
	char node_name[256], uart_name[ATFMI_MAX_DEV_SIZE];
	nvpi3_node_handle db_uart_handle;
	int i = 0;
	struct uio uart_uio = { 0 };
	uint32_t res;

	uart_uio.map_addr = MAP_FAILED;
	while (1) {
		snprintf(node_name,
		         sizeof(node_name),
		         "%s/uart%d/",
		         profile,
		         i);
		db_uart_handle = db_find_node(db_group_handle, node_name, code);
		if (*code == ATFMI_PROFILE_NOT_FOUND && !i) {
			TPT_INFO("no uart ctrl configured");
			*code = ATFMI_SUCCESS;
			return;
		} else if (*code == ATFMI_PROFILE_NOT_FOUND) {
			TPT_TRACE(1, STR("didn't find node %s", node_name));
			*code = ATFMI_SUCCESS;
			break;
		} else if (*code != ATFMI_SUCCESS) {
			TPT_ERROR(STR(
			       "error in DB while checking %s",
			       node_name));
			goto err_close;
		}

		get_value(db_uart_handle, "name", NVPI3_KEY_TYPE_STR,
		          ATFMI_MAX_DEV_SIZE,
		          (union nvpi3_key_value *) uart_name, &res);

		if (res != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("get_value error %u", res));
			*code = ATFMI_INVALID_PARAMETER;
			goto err_close;
		}

		get_value(db_uart_handle,
		          "offset",
		          NVPI3_KEY_TYPE_U32,
		          sizeof(uart_uio.offset),
		          (union nvpi3_key_value *) &(uart_uio.offset),
		          &res);

		if (res != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("get_value error %u", res));
			*code = ATFMI_INVALID_PARAMETER;
			goto err_close;
		}

		get_value(db_uart_handle,
		          "mask",
		          NVPI3_KEY_TYPE_U32,
		          sizeof(uart_uio.mask),
		          (union nvpi3_key_value *) &(uart_uio.mask),
		          &res);

		if (res != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("get_value error %u", res));
			*code = ATFMI_INVALID_PARAMETER;
			goto err_close;
		}

		get_value(db_uart_handle,
		          "value",
		          NVPI3_KEY_TYPE_U32,
		          sizeof(uart_uio.value),
		          (union nvpi3_key_value *) &(uart_uio.value),
		          &res);

		if (res != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("get_value error %u", res));
			*code = ATFMI_INVALID_PARAMETER;
			goto err_close;
		}

		init_uart_uio(&uart_uio, uart_name, code);
		if (*code != ATFMI_SUCCESS) {
			TPT_ERROR(STR("initiate uart failed (%u)", *code));
			goto unmap;
		}

		write_uart_reg(&uart_uio);
		if(uart_uio.map_addr != MAP_FAILED) {
			munmap(uart_uio.map_addr, uart_uio.map_size);
		}

		close_node(db_uart_handle, code);
		if (*code != ATFMI_SUCCESS)
			return;
		i++;
	}
	return;

unmap:
	if (uart_uio.map_addr != MAP_FAILED) {
		munmap(uart_uio.map_addr, uart_uio.map_size);
	}
err_close:
	close_node(db_uart_handle, &res);
	return;
}

static void find_atf_params(nvpi3_db_group_handle db_group_handle,
                                const char* profile,
                                atfmi_result_t *code)
{
	nvpi3_node_handle db_port_handle, anp_mux_handle;
	char node_name[256], port_type[64];
	bool mux = true;
	uint32_t res;
	struct program p = { 0 };


	while (1) {
		snprintf(node_name,
		         sizeof(node_name),
		         "%s/port%d/",
		         profile,
		         p.cnt);
		db_port_handle = db_find_node(db_group_handle, node_name, code);
		/*we need to check node port at least once*/
		if (*code == ATFMI_PROFILE_NOT_FOUND && !p.cnt) {
			TPT_ERROR("dtb corrupted");
			*code = ATFMI_INVALID_PARAMETER;
			return;
		}
		else if (*code == ATFMI_PROFILE_NOT_FOUND) {
			TPT_TRACE(1, STR("didn't find node %s", node_name));
			*code = ATFMI_SUCCESS;
			break;
		}
		else if (*code != ATFMI_SUCCESS) {
			TPT_ERROR(STR("error in DB while checking %s",
			       node_name));
			goto err_close;
		}
		get_value(db_port_handle, "port-type", NVPI3_KEY_TYPE_STR,
		ATFMI_MAX_DEV_SIZE, (union nvpi3_key_value *) port_type, &res);
		if (res != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("get_value error %u ", res));
			*code = ATFMI_INVALID_PARAMETER;
			return;
		}

		if (strcmp(port_type, "ANP") == 0) {
			TPT_TRACE(1, STR("anp type found"));
			p.prof[p.cnt].type = TYPE_ANP;

			snprintf(node_name,
			         sizeof(node_name),
			         "%s/port%d/mux-control0/",
			         profile,
			         p.cnt);
			anp_mux_handle = db_find_node(db_group_handle,
			                              node_name,
			                              code);
			if (*code == ATFMI_PROFILE_NOT_FOUND) {
				mux = false;
			}
			else if (*code != ATFMI_SUCCESS
			                && *code != ATFMI_PROFILE_NOT_FOUND) {
				goto err_close;
			}
			/*we were just checking if mux exists*/
			close_node(anp_mux_handle, code);
			if (*code != ATFMI_SUCCESS)
				goto err_close;
			prepare_and_launch_atf_daemon(db_port_handle,
			                              p.prof[p.cnt].dev_file,
			                              p.cnt,
			                              mux,
			                              profile,
			                              code);

			if (*code != ATFMI_SUCCESS) {
				TPT_ERROR(STR(
				       "atf launch failed code %u",
				       *code));
				goto err_close;
			}
		}
		else if (strcmp(port_type, "ECB") == 0) {
			TPT_TRACE(1, STR("ecb type found"));
			p.prof[p.cnt].type = TYPE_ECB;

			prepare_and_launch_ecb_daemons(db_port_handle,
			                               p.prof[p.cnt].dev_file,
			                               p.cnt,
			                               profile,
			                               code);
			if (*code != ATFMI_SUCCESS) {
				TPT_ERROR(STR(
				       "ecb launch failed code %u",
				       *code));
				goto err_close;
			}

		}
		else {
			TPT_ERROR(STR(
			       "Unknown type: \"%s\" in \"%s/%s\"",
			       port_type,
			       node_name,
			       "type"));
			*code = ATFMI_INVALID_PARAMETER;
			goto err_close;
		}

		close_node(db_port_handle, code);
		if (*code != ATFMI_SUCCESS)
			return;
		p.cnt++;
	}

	return;
err_close:
	close_node(db_port_handle, &res);
	return;
}



static void handle_init_req(const char* profile, atfmi_result_t *code)
{
	nvpi3_db_group_handle db_group_handle;
	atfmi_result_t ret;

	TPT_INFO(STR("profile \"%s\"", profile));

	*code = ATFMI_SUCCESS;

	if (is_profile_used(&llist, profile)) {
		return;
	}

	db_group_handle = open_db("sys_bpar", code);
	if (*code != ATFMI_SUCCESS)
		return;

	handle_uart_uio(db_group_handle, profile, code);
	if (*code != ATFMI_SUCCESS)
		return;

	handle_pin_mux(db_group_handle, profile, ACTION_SET, code);
	if (*code != ATFMI_SUCCESS)
		return;

	find_atf_params(db_group_handle, profile, code);
	if (*code != ATFMI_SUCCESS) {
		/*error already happened no need to check result*/
		handle_pin_mux(db_group_handle, profile, ACTION_UNSET, &ret);
		return;
	}

	close_db(db_group_handle, code);
	if (*code != ATFMI_SUCCESS)
		return;

}

static void check_profile_kill_daemon(struct daemon_head *llist,
                                      const char* profile,
                                      atfmi_result_t *code)
{
	struct daemon *dm;
	*code = ATFMI_INVALID_PARAMETER;
	TAILQ_FOREACH(dm, llist, lentry)
	{
		if (strcmp(profile, dm->profile) == 0) {
			if (send_sys_sig(dm, SIGTERM)) {
				TPT_ERROR(STR("unable to send %d to %d",
				SIGTERM, dm->pid));
				*code = ATFMI_OTHER_ERROR;
				return;
			}
			*code = ATFMI_SUCCESS;
		}
	}
}


static void handle_shutdown_req(const char* profile, atfmi_result_t *code)
{
	nvpi3_db_group_handle db_group_handle;

	check_profile_kill_daemon(&llist, profile, code);
	if (*code != ATFMI_SUCCESS)
		return;

	db_group_handle = open_db("sys_bpar", code);
	if (*code != ATFMI_SUCCESS)
		return;
	/*TODO: if pin unset fail should we try to kill daemons? */
	handle_pin_mux(db_group_handle, profile, ACTION_UNSET, code);
	if (*code != ATFMI_SUCCESS)
		return;

	close_db(db_group_handle, code);
	if (*code != ATFMI_SUCCESS)
		return;

}

static bool daemon_died(struct daemon_head *llist)
{
	/* Multiple daemons may have died, loop to make sure we get all */
	bool exit = false;
	while (1) {
		int status;
		pid_t pid = waitpid(-1, &status, WNOHANG);
		if (pid <= 0)
			break;

		struct daemon *dm = child_get_from_pid(llist, pid);
		if (dm) {

			if (WIFEXITED(status)) {
				TPT_TRACE(1, STR(
				       "daemon %s with pid %d completed "
				       "successfully status %d state %u",
				       dm->argv[0],
				       dm->pid,
				       status,
				       dm->state));

				if (dm->state != STATE_TERMINATED) {
					/*somebody else killed the child
					 * or it exited unexpectedly,
					 * bad behavior...
					 * free resources but not before
					 * you deal with zombies*/
					exit = true;
				}
			}
			else if (WIFSIGNALED(status)) {
				TPT_INFO(STR(
				       "daemon %s (pid %d) died of signal %d",
				       dm->argv[0],
				       pid,
				       WTERMSIG(status)));
				/*id child is killed  by any other
				 * signal than SIGTERM free resources*/
				if (dm->state != STATE_TERMINATED) {
					exit = true;
				}
			}

			TAILQ_REMOVE(llist, dm, lentry);
			daemon_free_single(dm);

		}
		else {
			TPT_ERROR(STR(
			       "failed to get child from pid %d",
			       pid));
			exit = true;
		}
	}

	if (exit) {
		TPT_INFO("atfmid will exit");
		return FAIL;
	}

	return OK;
}

static int get_signal(int fd,
                      struct signalfd_siginfo *siginfo)
{
	fd_set set;
	int ret;

	FD_ZERO(&set);
	FD_SET(fd, &set);

	struct timeval tv;

	/*no timeout, return immediately*/
	tv.tv_sec = 0;
	tv.tv_usec = 0;

	ret = select(fd + 1, &set, NULL, NULL, &tv);

	if (ret == -1 && errno != EINTR) {
		TPT_ERROR(STR(
		       "Failed to select on pipe (%s)",
		       strerror(errno)));
		return 2;
	}
	if (FD_ISSET(fd, &set)) {
		ret = read(fd, siginfo, sizeof(struct signalfd_siginfo));
		if (ret != sizeof(struct signalfd_siginfo)) {
			TPT_ERROR(STR(
			       "Failed to read from signalfd (%s)",
			       strerror(errno)));
			return -1;
		}
		return 1;
	}

	return 0;
}

static void create_init_rsp_msg(union itc_msg **reply,
                                struct conn_client_info *ci,
                                atfmi_result_t res)
{

	if (res == ATFMI_SUCCESS) {
		*reply = itc_alloc(sizeof(struct atfmi_init_cfm),
		                   ATFMI_INIT_CFM);
		(*reply)->atfmi_init_cfm.procedure_ref = ci->procedure_ref;
		(*reply)->atfmi_init_cfm.connection_ref = ci->client_ref;
	}
	else {
		*reply = itc_alloc(sizeof(struct atfmi_init_rej),
		                   ATFMI_INIT_REJ);
		(*reply)->atfmi_init_rej.procedure_ref = ci->procedure_ref;
		(*reply)->atfmi_init_rej.connection_ref = ci->client_ref;
		(*reply)->atfmi_init_rej.errorCode = res;
	}
}

static void create_shutdown_rsp_msg(union itc_msg **reply,
                                    struct conn_client_info *ci,
                                    atfmi_result_t res)
{
	if (res == ATFMI_SUCCESS) {
		*reply = itc_alloc(sizeof(struct atfmi_shutdown_cfm),
		                   ATFMI_SHUTDOWN_CFM);
		(*reply)->atfmi_shutdown_cfm.procedure_ref = ci->procedure_ref;
		(*reply)->atfmi_shutdown_cfm.connection_ref = ci->client_ref;
	}
	else {
		*reply = itc_alloc(sizeof(struct atfmi_shutdown_rej),
		                   ATFMI_SHUTDOWN_REJ);
		(*reply)->atfmi_shutdown_rej.procedure_ref = ci->procedure_ref;
		(*reply)->atfmi_shutdown_rej.connection_ref = ci->client_ref;
		(*reply)->atfmi_shutdown_rej.errorCode = res;
	}
}


static void *atfmi_thread(void *arg)
{
	uint32_t res = 0;
	atfmi_result_t code;
	int sfd;
	sigset_t *set = arg;
	struct signalfd_siginfo fdsi;

	itc_mbox_id_t my_mbox = ITC_NO_ID;
	union itc_msg *msg = NULL;
	union itc_msg *reply = NULL;

	ATFMI_CONN_ESTABLISH_MESSAGES_STRUCT(atfmi_conn_messages);
	uint32_t supported_versions[] = { ATFMI_SERVER_SUPPORTED_VERSIONS };
	conn_server_handle_t handle;
	struct conn_client_info ci;

	TAILQ_INIT(&llist);

	pinmux_p = calloc(1, sizeof(pinmux_t));

	sfd = signalfd(-1, set, 0);
	if (sfd == -1) {
		TPT_ERROR("signalfd failed");
		goto exit;
	}

	res = conn_establish_server_init(&handle,
	                                 sizeof(supported_versions)
	                                 / sizeof(supported_versions[0]),
	                                 supported_versions,
	                                 &atfmi_conn_messages,
	                                 ATFMI_MAX_CLIENTS,
	                                 NULL); /*do we need callback?*/

	if (res) {
		TPT_ERROR("Failed to init conn_establish server");
		goto exit;
	}

	my_mbox = itc_create_mailbox(ATFMI_SERVER_MBOX_NAME, 0);

	if (my_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("unable to create mbox \"%s\"",
		              ATFMI_SERVER_MBOX_NAME));
		goto exit;
	}

	while (1) {
		msg = itc_receive(ITC_NOFILTER,
		                  ATFMI_RECEIVE_TMO,
		                  ITC_FROM_ALL);

		if (msg && conn_check_client(handle, &msg, &ci)) {

			/*Here we will always have a valid message.*/
			switch (msg->msgno) {

				case ATFMI_INIT_REQ:
					TPT_REC_SIG((uintptr_t)msg, "ATFMI_INIT_REQ");
					handle_init_req(
					           msg->atfmi_init_req.profile,
					           &code);
					TPT_TRACE(1, STR("init code to app %u",
					          code));
					create_init_rsp_msg(&reply, &ci, code);
					break;

				case ATFMI_SHUTDOWN_REQ:
					TPT_REC_SIG((uintptr_t)msg, "ATFMI_SHUTDOWN_REQ");
					handle_shutdown_req(
					       msg->atfmi_shutdown_req.profile,
					       &code);
					TPT_TRACE(1, STR(
					       "shutdown code to app %u",
					       code));
					create_shutdown_rsp_msg(&reply,
					                        &ci,
					                        code);
					break;

				default:
					TPT_ERROR(STR(
					       "Received an unexpected message"
					       "0x%x sender: 0x%x",
					       msg->msgno, itc_sender(msg)));
					break;
			}

			if (reply)
				itc_send(&reply, ci.sender, ITC_MY_MBOX);
		}

		if(msg) {
			itc_free(&msg);
		}
		/*handle signal fd */
		int ret = get_signal(sfd, &fdsi);

		if (ret == 0) {
			/* nothing to read on FD*/
			continue;
		}
		else if (ret == 2 || ret == -1) {
			break;
		}

		if (fdsi.ssi_signo == SIGCHLD) {
			TPT_TRACE(1,STR(
			       "Received SIGCHLD sender %x",
			       fdsi.ssi_pid));
			if (daemon_died(&llist))
				break;
		}
		else if (fdsi.ssi_signo == SIGINT ||
		         fdsi.ssi_signo == SIGTERM) {
			break;
		}
	}

	itc_delete_mailbox(my_mbox);
exit:
	free(pinmux_p);
	pthread_exit(0);
	return NULL;
}

static void run()
{
	FILE *fp;
	int fd, res = 0;
	pthread_t thread;

	sigset_t mask;

	/* Create, check and lock HDB UUCP lock file. */
	fd = open(ATFMI_LOCK_FILE, O_WRONLY | O_CREAT, S_IWUSR);
	if (fd == -1) {
		return;
	}
	if (flock(fd, LOCK_EX | LOCK_NB) == -1) {
		TPT_ERROR(STR("flock(%s) failed, errno: %d "
		       " (lock not obtained)",
		       ATFMI_LOCK_FILE, errno));
		goto err_lock;
	}
	fp = fdopen(fd, "w");
	if (!fp) {
		TPT_ERROR(STR("fdopen() failed, errno: %d", errno));
		goto err_lock;
	}
	fprintf(fp, "%10d\n", getpid());
	fflush(fp);

	sigemptyset(&mask);
	sigaddset(&mask, SIGCHLD);
	sigaddset(&mask, SIGINT);
	sigaddset(&mask, SIGTERM);

	/* block these signals and use signalfd instead */
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1) {
		TPT_ERROR("pthread_sigmask failed");
		goto err_create;
	}

	res = itc_init(ATFMI_MAX_NUM_MBOX,
	               ITC_MALLOC,
	               NULL,
	               ITC_NO_NAMESPACE,
	               0);

	if (res) {
		TPT_ERROR(STR("unable to init ITC for %s, result %d",
		ATFMI_DAEMON_NAME, res));
		goto err_create;
	}

	/* Create ATFMI Server thread. */
	if (pthread_create(&thread, NULL, atfmi_thread, (void *) &mask) != 0) {
		TPT_ERROR("Failed to create ATFMI thread");
		goto err_create;
	}

	pthread_join(thread, NULL);

err_create:
	fclose(fp); /* Will close(fd) */
	return;

err_lock:
	close(fd);
}

int main(int argc, char *argv[])
{

	const char * const string_opt = "hd";
	const struct option l_opt[] = { { "help", 0, NULL, 'h' },
	                                { "daemonize", 0, NULL, 'd' },
	                                { 0, 0, 0, 0 } };

	int c, daemonize = 0;

	while ((c = getopt_long(argc, argv, string_opt, l_opt, NULL)) != -1) {
		switch (c) {
			default:
			case 'h':
				printf("Usage %s [option]\n"
				       "where option is one of:\n"
				       "-h|--help  Display this help message\n"
				       "-d|--daemonize   Disconnect from"
				       " the terminal, create daemon \"%s\"\n",
				       ATFMI_DAEMON_NAME,
				       ATFMI_DAEMON_NAME);
				return 0;
			case 'd':
				/* Auto-backgrounding should be avoided */
				daemonize = 1;
				break;
		}
	}

	TPT_INIT();
	TPT_INFO("Start initializing");

	if (daemonize) {
		if (daemon(0, 0)) {
			perror("daemon");
			return -1;
		}
		TPT_INFO(STR("daemon starting (%s)", ATFMI_DAEMON_NAME));
	}

	run();

	if (daemonize) {
		TPT_INFO(STR( "daemon exiting (%s)", ATFMI_DAEMON_NAME));
	}

	return 0;
}

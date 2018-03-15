/******************************************************************************
 *
 *      COPYRIGHT (C)Ericsson Radio Systems AB, Sweden
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
#ifndef __ATFMI_INTERNAL_H
#define __ATFMI_INTERNAL_H

#include <atfmi.h>
#include <stdbool.h>
#include <nvpi3.h>
#include <signal.h>
#include <errno.h>
#include <sys/queue.h>

#include <conn-establish-helper.h>
#include <conn-establish.h>

#include "pinmux.h"

#define ATFMI_SERVER_MBOX_NAME "atfmi_server"
#define ATFMI_SERVER_SUPPORTED_VERSIONS      1
#define ATFMI_REQ_VERSION                    1
#define ATFMI_NO_OF_SUPPORTED_VERSIONS       1

#define XCS_DAEMON_PATH   "/usr/bin/"
#define A4CID_DAEMON_NAME "a4cid"
#define A4CID_DAEMON_PATH XCS_DAEMON_PATH A4CID_DAEMON_NAME

#define ATFID_DAEMON_NAME "atfid"
#define ATFID_DAEMON_PATH XCS_DAEMON_PATH ATFID_DAEMON_NAME

#define ATFMI_MAX_CLIENTS 16

#define ATF_MUX                     "mux-control"

#define ATFMI_NOT_DAEMON_OPTION    "-n"
#define ATFMI_DEVICE_OPTION        "-d"
#define ATFMI_SERVER_OPTION        "-s"
#define ATFMI_MUX_OPTION           "-m"
#define ATF_SERVER_NAME_ANP        "XXP"
#define ATF_SERVER_NAME_ECB        "MXP"
#define ATF_MUX_LIB                "libatfmi-mux.so.0"

#define ATFMI_MAX_DEV_SIZE 64

#define ATFMI_MAX_PORTS 8
#define ATFMI_MAX_UART_PINS 8
#define ATFMI_MAX_PATH_SIZE 100

#define ATFMI_PROFILE_NOT_FOUND 1

#define ATFMI_NOF_PINS 337

#define OK 0
#define FAIL 1

typedef struct
{
	itc_mbox_id_t mbox;
	uint32_t data;
} cd_t;

typedef enum
{
	STATE_STOPPED = 0,
	STATE_STARTED,
	STATE_TERMINATED,
	STATE_ABORTED,
	STATE_KILLED
} state_t;

typedef enum
{
	ACTION_UNSET = 0,
	ACTION_SET
} pin_action_t;

typedef struct
{
	pinmux_handle_t handle[ATFMI_NOF_PINS];
	uint32_t 	refcnt[ATFMI_NOF_PINS];
} pinmux_t;

struct daemon
{
	char **argv;
	char profile[ATFMI_MAX_PROFILE_NAME_SIZE];
	state_t state;
	pid_t pid;
	pid_t ppid;
	TAILQ_ENTRY(daemon) lentry; /* entry in the launch list */
};

TAILQ_HEAD(daemon_head, daemon);

enum type
{
	TYPE_ANP = 0,
	TYPE_ECB = 1
};

struct program
{
	int cnt;
	struct
	{
		enum type type;
		char dev_file[ATFMI_MAX_DEV_SIZE];
	} prof[ATFMI_MAX_PORTS];
};

struct uio {
	uint32_t offset;
	uint32_t mask;
	uint32_t value;
	void *map_addr;
	uint32_t map_size;
	volatile uint32_t *addr;
};
#endif

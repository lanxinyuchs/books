/******************************************************************************
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 *****************************************************************************/

/**
 * @file libmama.h
 * @brief Interface to mama for system daemon.
 *
 * This file is the interface to Mama used by the daemon which manage the
 * system. For the client interface, see @ref mama.h.
 *
 *******************************************************************************/

#ifndef __LIBMAMA_H
#define __LIBMAMA_H

#include <sys/types.h>
#include <unistd.h>

typedef enum {
	/** Mama exited because of orders (SIGTERM) */
	MAMA_EXIT_REASON_ORDERED = 0,
	/** Mama exited because it received a command to exit */
	MAMA_EXIT_REASON_COMMAND,
	/** Mama exited because the ordered exit exceeded timeout */
	MAMA_EXIT_REASON_TIMEOUT
} mama_exit_reason_t;

/**
 * @brief Child launch function callback.
 *
 * @param user_data  User supplied data returned in the callback.
 * @param name       The command which launched, which is the command with full
 *                   path.
 * @param pid        The pid of the process launched.
 */
typedef void (*mama_child_launch_cb)(void *user_data, char *name, pid_t pid);

/**
 * @brief Child exit function callback.
 *
 * @param user_data  User supplied data returned in the callback.
 * @param name       The command which exited, which is the command with full
 *                   path.
 * @param pid        The pid of the process which exited.
 * @param status     The status of the command when it exited. This is the
 *                   status returned by waitpid(), and can be used to determine
 *                   the cause of the exit, see waitpid(2) for macros.
 */
typedef void (*mama_child_exit_cb)(void *user_data, char *name,
                                   pid_t pid, int status);

/**
 * @brief Domain start function callback.
 *
 * @param user_data  User supplied data returned in the callback.
 * @param name       The name of the domain started as given in the config file.
 */
typedef void (*mama_domain_start_cb)(void *user_data, char *name);

/**
 * @brief Domain stop function callback.
 *
 * @param user_data  User supplied data returned in the callback.
 * @param name       The name of the domain stopped as given in the config file.
 */
typedef void (*mama_domain_stop_cb)(void *user_data, char *name);

/**
 * @brief Reboot function callback.
 *
 * @param user_data User supplied data returned in the callback.
 * @param reason    The reason for the reboot being called. Typically used for
 *                  logging purposes.
 *
 * This callback is passed to mama_run() and will be called when Mama
 * determines it is time to restart the system.
 */
typedef void (*mama_reboot_cb)(void *user_data, char *reason);

/**
 * @brief Exit function callback.
 *
 * @param user_data User supplied data returned in the callback.
 *
 * This callback is passed to mama_run() and will be called when Mama
 * has been told to exit and is ready to do so.
 */
typedef void (*mama_exit_cb)(void *user_data, mama_exit_reason_t reason);

/**
 * @brief Holding callback functions called by mama when events occur.
 *        If left to NULL, no callback will be called, but at least the
 *        reboot_cb must be supplied.
 */
struct mama_callbacks {
	/** Callback function when a child is launched */
	mama_child_launch_cb child_launch;
	/** Callback function when a child exits */
	mama_child_exit_cb   child_exit;
	/** Callback function when a domain is started */
	mama_domain_start_cb domain_start;
	/** Callback function when a domain is stopped */
	mama_domain_stop_cb  domain_stop;
	/** Callback function when mama wants a reboot to occur */
	mama_reboot_cb       reboot;
	/** Callback function when mama wants to exit */
	mama_exit_cb         exit;
};

/**
 * @brief Starts mama.
 *
 * @param cfg_file    The file containing the Mama configuration, see
 *                    @ref mamaglobal.
 * @param cbs         The callbacks, see @ref mama_callbacks.
 * @param user_data   User data which will be passed to each callback.
 *
 * This function will never return. It will take the signals SIGTERM and SIGCHLD,
 * as well as the signals used by @ref SIGALIVE and @ref SIGSYNC. The calling
 * process is supposed to set up the callbacks and call this function to give
 * control over to Mama.
 */
extern void mama_run(char *cfg_file, struct mama_callbacks *cbs, void *user_data);

#endif /* __LIBMAMA_H */

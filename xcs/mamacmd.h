/******************************************************************************
 * @copyright
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 *****************************************************************************/

/**
 * @file mamacmd.h
 * @brief Mama API for managing children during runtime.
 *
 ******************************************************************************/

#ifndef _MAMACMD_H
#define _MAMACMD_H

#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <sys/types.h>

/**
 * Max string size for mamacmd entry cmd.
 */
#define MAMACMD_MAX_ENTRY_STRING_SIZE 100

/** Opaque child id type */
typedef uint64_t mamacmd_id_t;

/**
 * @brief Represents status of the child (executable launched by Mama).
 */
struct mamacmd_status_entry {
	/** The executable to launch with full path specified. */
	char cmd[MAMACMD_MAX_ENTRY_STRING_SIZE];
	/** The name of the domain, which is used as a domain identifier. */
	char domain[MAMACMD_MAX_ENTRY_STRING_SIZE];
	/** Specifies if the child is stopped. */
	bool cmdstopped;
	/** Specifies if a domain of the child is stopped. Domains are groups of
	 *  children which are related. */
	bool domain_cmdstopped;
	/** Specifies if the child is a part of a domain. */
	bool in_domain;
	/** Specifies if the child is a persistent process
	 * or command which runs to completion. */
	bool run_until_completion;
	/** The child id set by mama. */
	mamacmd_id_t id;
	/** The chid pid returned by fork. */
	pid_t pid;
};

/**
 * @brief Config struct for launching the child.
 */
struct mamacmd_launch_config {
	int size;
	/** The *retries* parameter tells Mama how many times to relaunch a
	 * child before going to the next step. A relaunch is triggered if the
	 * child quits. The child can quit on its own, or it can be interrupted
	 * by a signal which it does not catch.
	 * The next step depends on whether or not the child is part of a
	 * *domain*. If the child is not part of a domain, and it has been
	 * retried the number of times specified by this argument the
	 * [mama_reboot_cb](@ref mama_reboot_cb) is called, which typically
	 * restarts the system. If the argument is omitted the default is 0. */
	int retries;
	/** The *retry_timeout_ms* parameter is specified in milliseconds and
	 * must be given if you give #retries parameter. This tells Mama the
	 * maximum time between when the child is restarted until it is alive
	 * (determined by seeing if the launched *pid* exists). If this timeout
	 * is exceeded, Mama will call [mama_reboot_cb](@ref mama_reboot_cb). */
	int retry_timeout_ms;
	/** Timeout for sending *SIGABRT* after something bad happens. */
	int abort_timeout_ms;
	/** The *alive_timeout_ms* parameter tells Mama the time in milliseconds
	 * which can pass between two *alive* signals from the child. If this
	 * parameter is omitted or set to 0, Mama will not be monitoring this
	 * child for hanging. */
	int alive_timeout_ms;
	/** The *wait_timeout_ms* parameter tells mama how long to wait for a
	 * *sync* signal from the child. Until Mama has received this sync
	 * signal, the next child in the list will not be launched. */
	int wait_timeout_ms;
	/** The *run_until_completion* parameter tells Mama that this child is
	 * not persistent in the system, and that it should run to completion
	 * before any other child is launched.
	 * If this parameter is set, the child is expected to exit with an error
	 * code of 0, otherwise it is considered an error and the rules for
	 * retries and domains are applied as for persistent children.*/
	int run_until_completion;
	/** The *affinity* parameters tells Mama which processor core to run
	 * this child on, with the first core being 0. If setting the affinity
	 * fails, the [mama_reboot_cb](@ref mama_reboot_cb) will be called. The
	 * default is no affinity, meaning the child can run on any processor
	 * core. */
	int affinity;
};

/** Reference used for current instance of mamacmd. */
typedef struct mamacmd_instance * mamacmd_handle;

/** @defgroup group2 API Functions in mamacmd.h
 *  @{
 */

/**
 * @brief Obtains a handle for mamacmd instance.
 *
 * @return Handle when successful, otherwise NULL.
 */
extern mamacmd_handle mamacmd_connect(void);

/**
 * @brief Starts a previously stopped child with specified id.
 *
 * @param[in] h The handle for mamacmd instance.
 * @param[in] id Child id.
 *
 * @return 0 when successful, otherwise value different from 0.
 */
extern int mamacmd_child_start(mamacmd_handle h, mamacmd_id_t id);

/**
 * @brief Stops the child with specified id.
 *
 * @param[in] h The handle for mamacmd instance.
 * @param[in] id Child id.
 *
 * @return 0 when successful, otherwise value different from 0.
 */
extern int mamacmd_child_stop(mamacmd_handle h, mamacmd_id_t id);

/**
 * @brief Starts a previously stopped group of children that are part of the
 * domain with the given name.
 *
 * @param[in] h The handle for mamacmd instance.
 * @param[in] domain The name of the domain to start.
 *
 * @return 0 when successful, otherwise value different from 0.
 */
extern int mamacmd_domain_start(mamacmd_handle h, char *domain);

/**
 * @brief Stops all the children that are part of a domain with the given name.
 *
 * @param[in] h The handle for mamacmd instance.
 * @param[in] domain The name of the domain to stop.
 *
 * @return 0 when successful, otherwise value different from 0.
 */
extern int mamacmd_domain_stop(mamacmd_handle h, char *domain);

/**
 * @brief Gets a list of status entries for all children managed by mama.
 *
 * @param[in] h The handle for mamacmd instance.
 * @param[out] entries List of children's status entries.
 *
 * @return Number of entries in the list of managed children.
 */
extern int mamacmd_status(mamacmd_handle h,
                          struct mamacmd_status_entry ** entries);

/**
 * @brief Launches an executable and adds it to the list of managed children.
 *
 * @param[in]  h The handle for mamacmd instance.
 * @param[in]  argv Path to executable and arguments to launch it with.
 * @param[in]  env Environment variables for the child. The variable and value
 *                 come in a pair, where env[0] is a variable and env[1] is a
 *                 value, env[2] is a variable and env[3] is a value etc. The
 *                 array needs to be null terminated.
 * @param[in]  domain The name of the domain.
 * @param[in]  cfg Launch configuration struct.
 * @param[out] id The id assigned to the launched child.
 *
 * @return 0 when successful, otherwise value different from 0.
 *
 * An example of argv with supplied executable and arguments:
 *
 @verbatim
 // /usr/bin/a4cid -n -d /dev/tty/ECB
char *argv[] = {
    "/usr/bin/a4cid",
    "-n",
    "-d",
    "/dev/tty/ECB",
    NULL
};
 @endverbatim
 *
 * An example of env:
 @verbatim
// The environment variables VAR=VAL and FOO=BAR are being set for this child
char *env[] = {
    "VAR", "VAL",
    "FOO", "BAR",
    NULL
};
 */
extern int mamacmd_launch(mamacmd_handle h, char * const argv[],
                          char * const env[], char *domain,
                          struct mamacmd_launch_config *cfg,
                          mamacmd_id_t *id);

/**
 * @brief Stop monitoring a child managed by mama and terminate it.
 *
 * @param[in] h The handle for mamacmd instance.
 * @param[in] id The id of the child to terminate.
 *
 * @return 0 when successful, otherwise value different from 0.
 */
extern int mamacmd_terminate(mamacmd_handle h, mamacmd_id_t id);

/**
 * @brief Frees and returns the resources.
 *
 * @param[in] h The handle for mamacmd instance.
 * @return 0 when successful, otherwise value different from 0.
 */
extern int mamacmd_disconnect(mamacmd_handle h);

/**
 * @brief Shuts down mama and all her children.
 *
 * @param[in] h The handle for mamacmd instance.
 * @return 0 when successful, otherwise value different from 0.
 */
extern int mamacmd_shutdown(mamacmd_handle h);

/** @} */

#endif /* _MAMACMD_H */

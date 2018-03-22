/**
 *   Handling of remote commands.
 *
 *   @file
 *
 *   This file is a part of the COLI shell.
 *
 *   Copyright (C) 2011 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2014-10-29 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed reset of std stream.
 *             They are never changed (redirected), hence no need to
 *             reset them.
 *
 *   Revised : 2014-10-14 Ravineet Singh EAB/FJP/HB
 *   Change  : Replaced dupstr with strdup
 *
 *   Revised : 2014-09-10 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed colish as a command line shell
 *             Removed usleep, again :), root cause fixed in LITS.
 *
 *   Revised : 2014-06-09 Stanislav Vovk
 *   Change  : Back with usleep as WA for lits_daemon crash
 *
 *   Revised : 2014-05-22 Henrik Wallin
 *   Change  : Removed usleep. Moved the handling of the problem case
 *             to lib/std_stream.c:set_std_stream()
 *
 *   Revised : 2014-01-21 Ravineet Singh EAB/FJP/HB
 *   Change  : Replaced, in place,  save/restore of std streams with new
 *             std stream api calls.
 *
 *   Revised : 2013-12-20 Ravineet Singh EAB/FJP/HB
 *   Change  : Changed IPC mechanism from linx to OSE(LITS).
 *             Command is now executed in a OSE process
 *             instead of forked UNIX process.
 *
 *   Revised : 2011-11-30 Lars Jönsson EAB/FJP/TB
 *   Change  : All command names are now allocated dynamically.
 *
 *   Revised : 2011-11-29 Lars Jönsson EAB/FJP/TB
 *   Change  : All signals are now using variable sized string instead of
 *             fix sized strings.
 *
 *   Revised : 2011-11-28 Lars Jönsson EAB/FJP/TB
 *   Change  : Cleanup of help, usage and description strings retrieval.
 *
 *   Revised : 2011-11-25 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <argz.h>

#include <coli.sig>
#include <fdh.h>
#include <ose.h>
#include <fdh.h>
#include <std_stream.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define OS_HUNT_SIG     ((SIGSELECT)251)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
union SIGNAL {
	SIGSELECT                         sig_no;
	struct coli_find_cmd_sig          coli_find_cmd;
	struct coli_find_cmd_r_sig        coli_find_cmd_r;
	struct coli_part_cmd_sig          coli_part_cmd;
	struct coli_part_cmd_r_sig        coli_part_cmd_r;
	struct coli_exec_cmd_sig          coli_exec_cmd;
	struct coli_exec_cmd_r_sig        coli_exec_cmd_r;
	struct coli_cmd_exit_sig          coli_cmd_exit;
	struct coli_help_cmd_sig          coli_help_cmd;
	struct coli_help_cmd_r_sig        coli_help_cmd_r;
	struct coli_usage_cmd_sig         coli_usage_cmd;
	struct coli_usage_cmd_r_sig       coli_usage_cmd_r;
	struct coli_descr_cmd_sig         coli_descr_cmd;
	struct coli_descr_cmd_r_sig       coli_descr_cmd_r;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

static volatile int interrupt_signal_caught;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Locates the coli server.
 *
 *   @param wait       Waits for the coli server to be started is set to 1
 *
 *   @return           Process id if found, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static PROCESS find_coli_srv(int wait)
{
	SIGSELECT     sel_hunt[] = {1, OS_HUNT_SIG};
	union SIGNAL  *sig;
	PROCESS        pid;

	if ( wait ) {
		sig = alloc(sizeof(SIGSELECT), OS_HUNT_SIG);
		hunt("coli_server", 0, NULL, &sig);
		sig = receive(sel_hunt);
		pid = sender(&sig);
		free_buf(&sig);
	}
	else {
		if (hunt("coli_server", 0, &pid, NULL) == 0)
			return 0;
	}

	return pid;
}

/** ==================================================================== */
/**
 *   Returns the needed size if a buffer, where argc and argv are packed
 *   into and each string is null terminated.
 *
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 *
 *   @return           Needed buffer size
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int pack_args_len(int argc, char **argv)
{
	int   i;
	int   count;

	/*
	 **  Add size of c string end marks and a trailing endmark
	 */
	count = argc + 1;

	/*
	 **  Add size of c strings
	 */
	for (i = 0; i < argc; i++)
		count += strlen(argv[i]);

	return count;
}

/** ==================================================================== */
/**
 *   Packs argc and argv into a buffer, where each string is null
 *   terminated.
 *
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 *   @param buf        Buffer for result. Must be big enoungh to hold the
 *                     result.
 *   @param size       The used space of buf, including null terminations
 *
 *   @return           O if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int pack_args(int argc, char **argv, char *buf, int *size)
{
	int   i;
	char  *ptr;
	int   count;

	count = pack_args_len(argc, argv);

	*size = count <= *size ? count : 0;

	if ( *size == 0 )
		return -1;

	ptr = buf;

	for (i = 0; i < argc; i++) {
		strcpy(ptr, argv[i]);
		ptr = &ptr[strlen(ptr)];
		*ptr++ = '\0';
	}

	*ptr++ = '\0';

	return 0;
}

/** ==================================================================== */
/**
 *   Retrives command server.
 *
 *   @param name       Command name
 *
 *   @return           Process id if found, otherwise 0.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static PROCESS get_cmd_pid(char *name)
{
	SIGSELECT     sel_find[] = {2, COLI_FIND_CMD_R, OS_ATTACH_SIG};
	union SIGNAL *sig;
	PROCESS       cmd_pid = 0;
	PROCESS       colid_pid = find_coli_srv(1);
	OSATTREF      ref;

	sig = alloc(sizeof(struct coli_find_cmd_sig) + strlen(name), COLI_FIND_CMD);
	strcpy(sig->coli_find_cmd.name, name);
	send(&sig, colid_pid);
	ref = attach(NULL, colid_pid);

	sig = receive(sel_find);
	detach(&ref);

	if ( sig->sig_no == COLI_FIND_CMD_R ) {
		cmd_pid = sig->coli_find_cmd_r.status == 0 ?
			sig->coli_find_cmd_r.pid : 0;
	}

	free_buf(&sig);

	return cmd_pid;
}

/** ==================================================================== */
/**
 *   Sends a start of command request to the command server.
 *
 *   @param cmd_pid    Process id of command server.
 *
 *   @return           Name of Unix socket
 *
 *   @par Globals:
 *                     --
 *
 * A Unix socket name is returned for standard streams.
 */
/* ===================================================================== */
static char * start_exec_cmd(union SIGNAL *sig, PROCESS cmd_pid)
{
	SIGSELECT     sel_exec[] = {2, COLI_EXEC_CMD_R, OS_ATTACH_SIG};
	char          *sock_name = NULL;
	OSATTREF      ref;

	send(&sig, cmd_pid);
	ref = attach(NULL, cmd_pid);

	sig = receive(sel_exec);
	detach(&ref);

	if ( (sig->sig_no == COLI_EXEC_CMD_R) &&
	     (sig->coli_exec_cmd_r.status == COLI_OK) ) {
		sock_name = malloc(strlen(sig->coli_exec_cmd_r.socket) + 1);

		if ( sock_name != NULL )
			strcpy(sock_name, sig->coli_exec_cmd_r.socket);
	}

	free_buf(&sig);

	return sock_name;
}

/** ==================================================================== */
/**
 *   Wait for the current command to exit.
 *
 *   @param cmd_pid    Process id of command server.
 *
 *   @return           Result from command execution.0 if OK, otherwise
 *                     non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */

static void sig_handler(int signo)
{
	if (signo == SIGINT) {
		interrupt_signal_caught = 1;
	}
}

static int wait_cmd_exit(PROCESS cmd_pid)
{
	SIGSELECT     sel_exit[] = {2, COLI_CMD_EXIT, OS_ATTACH_SIG};
	union SIGNAL *sig;
	int           ret = -1;
	OSATTREF      ref;

	ref = attach(NULL, cmd_pid);

	interrupt_signal_caught = 0;

	signal(SIGINT, sig_handler);

	do {
		sig = receive_w_tmo(100, sel_exit);
	} while (sig == NIL && !interrupt_signal_caught);

	signal(SIGINT, SIG_IGN);

	detach(&ref);

	if (sig != NIL) {
		if (sig->sig_no == COLI_CMD_EXIT) {
			ret = sig->coli_cmd_exit.status;
		}
		free_buf(&sig);
	} else {
		ret = 0;
	}

	return ret;
}

/** ==================================================================== */
/**
 *   Executes the remote command in argv[0].
 *
 *   @param argc       Standard argc
 *   @param argv       Standard argv
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */

OS_PROCESS(icolish_cmd_server)
{
	SIGSELECT     sel_coli_exec_cmd[] = { 1, COLI_EXEC_CMD };
	union SIGNAL *sig;
	char         *sock_name, *str_cmd_pid;
	PROCESS       cmd_pid;

	str_cmd_pid = get_env(current_process(), "CMD_PID");
	if (str_cmd_pid == NULL ||
	    sscanf(str_cmd_pid, "%u", &cmd_pid) != 1) {
		goto complete;
	}

	sig = receive(sel_coli_exec_cmd);

	sock_name = start_exec_cmd(sig, cmd_pid);
	if (NULL == sock_name) {
		goto complete;
	}

	send_fd(sock_name);
	free(sock_name);

	(void) wait_cmd_exit(cmd_pid);

complete:
	kill_proc(current_process());
}

int run_remote_command(int argc, char **argv)
{
	int           ret;
	union SIGNAL *sig;
	int           len;
	SIGSELECT     sel_attach[] = { 1, OS_ATTACH_SIG };
	PROCESS       cmd_pid, cmd_server_pid;
	char          str_cmd_pid[20];

	len = pack_args_len(argc, argv);
	sig = alloc(sizeof(struct coli_exec_cmd_sig) + len, COLI_EXEC_CMD);
	sig->coli_exec_cmd.len = len;

	ret = pack_args(argc, argv, sig->coli_exec_cmd.cmd, &len);
	if (ret != 0) {
		free_buf(&sig);
		return ret;
	}

	cmd_pid = get_cmd_pid(sig->coli_exec_cmd.cmd);
	if (0 == cmd_pid) {
		free_buf(&sig);
		return -2;
	}

	cmd_server_pid = create_process(OS_PRI_PROC,
	                                "icolish_cmd_server",
	                                icolish_cmd_server,
	                                16384,
	                                16,
	                                0, 0, NULL, 0, 0);
	snprintf(str_cmd_pid, sizeof(str_cmd_pid), "%u", cmd_pid);
	set_env(cmd_server_pid, "CMD_PID", str_cmd_pid);
	start(cmd_server_pid);

	send(&sig, cmd_server_pid);

	/* Wait for the command server to complete and die. */
	(void) attach(NULL, cmd_server_pid);
	sig = receive(sel_attach);
	free_buf(&sig);

	return 0;
}

/** ==================================================================== */
/**
 *   Returns the next remote command where the name starts with expr.
 *
 *   @param expr       Start of command name
 *   @param prev       Previous command name found. Empty string if this
 *                     is the first search
 *
 *   @return           Next command if found, otherwise NULL (must be
 *                     freed after use)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
char * next_remote_command(const char *expr, char *prev)
{
	static PROCESS     colid_pid = 0;
	SIGSELECT       sel[] = {2, COLI_PART_CMD_R, OS_ATTACH_SIG};
	union SIGNAL    *sig;
	OSATTREF        ref;
	int             offset;
	int             name_size = strlen(expr) + 1;
	int             prev_size = strlen(prev) + 1;
	char            *next_cmd;

	if ( *prev == '\0' )
		colid_pid = find_coli_srv(0);

	if ( !colid_pid )
		return NULL;

	sig = alloc(sizeof(struct coli_part_cmd_sig) - 1 +
			name_size + prev_size, COLI_PART_CMD);

	sig->coli_part_cmd.name = (offset  = 0);
	sig->coli_part_cmd.prev = (offset += name_size);

	strcpy(&sig->coli_part_cmd.str[sig->coli_part_cmd.name], expr);
	strcpy(&sig->coli_part_cmd.str[sig->coli_part_cmd.prev], prev);

	send(&sig, colid_pid);
	ref = attach(NULL, colid_pid);

	sig = receive(sel);
	detach(&ref);

	next_cmd = NULL;

	if ( sig->sig_no == COLI_PART_CMD_R ) {
		if ( sig->coli_part_cmd_r.status == 0 )
			next_cmd = strdup(sig->coli_part_cmd_r.name);
	}

	free_buf(&sig);

	return next_cmd;
}

/** ==================================================================== */
/**
 *   Return the short help string for the specified command.
 *
 *   @param name       Command name
 *
 *   @return           The help string if found, otherwise NULL (must be
 *                     freed after use)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
char * remote_command_help(char *name)
{
	SIGSELECT     sel[] = {2, COLI_HELP_CMD_R, OS_ATTACH_SIG};
	union SIGNAL  *sig;
	PROCESS       srv_pid = find_coli_srv(0);
	OSATTREF      ref;
	char          *str = NULL;

	if ( srv_pid == 0 )
		return NULL;

	sig = alloc(sizeof(struct coli_help_cmd_sig) + strlen(name), COLI_HELP_CMD);
	strcpy(sig->coli_help_cmd.name, name);
	send(&sig, srv_pid);

	ref = attach(NULL, srv_pid);

	sig = receive(sel);
	detach(&ref);

	if ( (sig->sig_no != OS_ATTACH_SIG) &&
	     (sig->coli_help_cmd_r.status == 0) ) {
		str = strdup(sig->coli_help_cmd_r.help);
	}

	free_buf(&sig);
	return str;
}

/** ==================================================================== */
/**
 *   Return the usage string for the specified command.
 *
 *   @param name       Command name
 *
 *   @return           The usage string if found, otherwise NULL (must be
 *                     freed after use)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
char * remote_command_usage(char *name)
{
	SIGSELECT     sel[] = {2, COLI_USAGE_CMD_R, OS_ATTACH_SIG};
	union SIGNAL  *sig;
	PROCESS        srv_pid = find_coli_srv(0);
	OSATTREF       ref;
	char           *str = NULL;

	if ( srv_pid == 0 )
		return NULL;

	sig = alloc(sizeof(struct coli_usage_cmd_sig) + strlen(name), COLI_USAGE_CMD);
	strcpy(sig->coli_usage_cmd.name, name);

	send(&sig, srv_pid);
	ref = attach(NULL, srv_pid);

	sig = receive(sel);
	detach(&ref);

	if ( (sig->sig_no != OS_ATTACH_SIG) &&
	     (sig->coli_usage_cmd_r.status == 0) ) {
		str = strdup(sig->coli_usage_cmd_r.usage);
	}

	free_buf(&sig);
	return str;
}

/** ==================================================================== */
/**
 *   Return the descr string for the specified command.
 *
 *   @param name       Command name
 *
 *   @return           The descr string if found, otherwise NULL (must be
 *                     freed after use)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
char * remote_command_descr(char *name)
{
	SIGSELECT     sel[] = {2, COLI_DESCR_CMD_R, OS_ATTACH_SIG};
	union SIGNAL  *sig;
	PROCESS       srv_pid = find_coli_srv(0);
	OSATTREF      ref;
	char          *str = NULL;

	if ( srv_pid == 0 )
		return NULL;

	sig = alloc(sizeof(struct coli_descr_cmd_sig) + strlen(name), COLI_DESCR_CMD);
	strcpy(sig->coli_descr_cmd.name, name);

	send(&sig, srv_pid);
	ref = attach(NULL, srv_pid);

	sig = receive(sel);
	detach(&ref);

	if ( (sig->sig_no != OS_ATTACH_SIG) &&
	     (sig->coli_descr_cmd_r.status == 0) ) {
		str = strdup(sig->coli_descr_cmd_r.descr);
	}

	free_buf(&sig);
	return str;
}

int run_remote_linux_command(int argc, char *argv[], char *largv[])
{
	char  **xargv = NULL;
	int     idx, size = 0, res = 0;
	pid_t   pid;


	pid = fork();
	if (pid < 0) { /* Error */

		return -1;

	} else if (pid == 0) { /* Child executes the command */

		/* Enable Control-C */
		signal(SIGINT, SIG_DFL);

		for (idx = 0; largv[idx] != NULL; idx++) {
			xargv = realloc(xargv, (++size) * sizeof(char*));
			if (xargv == NULL) {
				return -1;
			}
			xargv[size - 1] = largv[idx];
		}
		for (idx = 1; idx < argc; idx++) {
			xargv = realloc(xargv, (++size) * sizeof(char*));
			if (xargv == NULL) {
				return -1;
			}
			xargv[size - 1] = argv[idx];
		}
		xargv = realloc(xargv, (++size) * sizeof(char*));
		if (xargv == NULL) {
			return -1;
		}
		xargv[size - 1] = NULL;

		exit(-execv(largv[0], xargv));

	} else { /* Parent waits for child to complete command */

		while (waitpid(pid, &res, 0) == -1) {
			if (errno == EINTR) {
				/* Interrupted system call, retry */
				continue;
			} else {
				printf("waitpid() failed, errno: %d\n",
				       errno);
				return -1;
			}
		}

		if (WIFEXITED(res)) {
			res = -WEXITSTATUS(res); /* 8 LSB from exit */
		} else if (!WIFSIGNALED(res)) {
			res = -1;
		}

	}

	return res;
}

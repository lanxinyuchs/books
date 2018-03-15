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
#include <shell.h>

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

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static U32 run_native_cmd(const char *cmd, const OSADDRESS *taglist);
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
static int wait_cmd_exit(PROCESS cmd_pid)
{
	SIGSELECT     sel_exit[] = {2, COLI_CMD_EXIT, OS_ATTACH_SIG};
	union SIGNAL  *sig;
	int           ret = -1;
	OSATTREF      ref;

	ref = attach(NULL, cmd_pid);

	sig = receive(sel_exit);
	detach(&ref);

	if ( sig->sig_no == COLI_CMD_EXIT )
		ret = sig->coli_cmd_exit.status;

	free_buf(&sig);
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
int run_remote_command(int argc, char **argv)
{
	int           ret;
	union SIGNAL *sig;
	int           len;
	PROCESS       cmd_pid;
	char          *sock_name;

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
		/* fprintf(stderr, "%s: command not found\n", sig->coli_exec_cmd.cmd);*/
		free_buf(&sig);
		return -2;
	}

	sock_name = start_exec_cmd(sig, cmd_pid);

	if (NULL == sock_name)
		return -1;

	send_fd(sock_name);
	free(sock_name);
	ret = wait_cmd_exit(cmd_pid);

	return ret;
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

/** ==================================================================== */
/**
 *   Creates the socket name for the local command server and allocates
 *   memory for the string.
 *
 *   @param            -
 *
 *   @return           Socket name string (must be freed after use)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static char * get_sock_name(void)
{
	char *name;
	char *dir = P_tmpdir;

	name = malloc(strlen(dir) + 1 + strlen(SOCK_PREFIX) + 1 + 4 + 1 + 6);

	if ( name != NULL )
		sprintf(name, "%s/%s.%u.XXXXXX", dir, SOCK_PREFIX, 1099);

	return name;
}

/** ==================================================================== */
/**
 *   Executes the command in the linux execute command signal. The
 *   exit command signal is sent when the command is finished.
 *
 *   @param sig        COLI execute command signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void exec_linux_command(union SIGNAL *sig)
{
	union SIGNAL  *reply, *sig_cont;
	int           ret;
	int           sock_fd;
	stdiofd_t     fds;
	char          *sock_name = get_sock_name();
	SIGSELECT     sel_continue[] = {1, 999};
	const OSADDRESS taglist[] = { RUNCMD_EFS_CLONE, RUNCMD_CLONE_ENV, RUNCMD_TAGEND };

	if (!sock_name)
		goto exit;

	sock_fd = open_socket(sock_name);
	if (sock_fd == -1)
		goto exit;

	reply = alloc(sizeof(struct coli_exec_cmd_r_sig) + strlen(sock_name),
	              COLI_EXEC_CMD_R);

	strcpy(reply->coli_exec_cmd_r.socket, sock_name);
	reply->coli_exec_cmd_r.status = COLI_OK;

	send(&reply, sender(&sig));

	save_std_stream(&fds);

	sig_cont = receive(sel_continue);
	free_buf(&sig_cont);

	ret = set_std_stream_socket(sock_fd);
	if( 0 != ret)
		goto exit;

	if (run_native_cmd(sig->coli_exec_cmd.cmd, taglist) != SHELL_SUCCESS)
		ret = 1;
	else
		ret = 0;

	/* fflush and restore fd:s*/
	fflush(stdout);
	fflush(stderr);
	set_std_stream(&fds);

	reply = alloc(sizeof(struct coli_cmd_exit_sig), COLI_CMD_EXIT);
	reply->coli_cmd_exit.status = ret;
	send(&reply, sender(&sig));

exit:
	if (sock_name)
		free(sock_name);
}

/** ==================================================================== */
/**
 *   Linux Command process.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(coli_command_server)
{
	SIGSELECT     selAll[] = {0};
	union SIGNAL  *sig;
	int           connected = 1;

	while (connected) {
		sig = receive(selAll);

		switch ( sig->sig_no ) {
			case COLI_EXEC_CMD:
				exec_linux_command(sig);
				break;
			case 0xdeadbeef: /* Keep coverity happy */
				goto exit;
			default:
				break;
		}
		free_buf(&sig);
	}

exit:
	kill_proc(current_process());
}

/** ==================================================================== */
/**
 *   Create the COLI Command server.
 *
 *   @return           - Pid of the server.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static PROCESS register_coli_cmd_server(void)
{
	PROCESS pid;

	start(pid = create_process(OS_PRI_PROC,
	                           "coli_command_server",
	                           coli_command_server,
	                           16384,
	                           16,
	                           0, 0, NULL, 0, 0));

	return pid;
}

static void quote(char *ds, char *ss)
{
	int len = strlen(ss);

	ds[0] = '"';
	strcpy(&ds[1], ss);
	ds[len+1] = '"';
	ds[len+2] = '\0';
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
int run_remote_linux_command(int argc, char *argv[], char *largv[])
{
	char          *sock_name = NULL;
	union SIGNAL  *sig;
	OSATTREF      ref;
	static PROCESS ccmd_pid = 0;
	SIGSELECT     sel_exec[] = {2, COLI_EXEC_CMD_R, OS_ATTACH_SIG};
	char         *argz;
	size_t        argz_len;

	if (argz_create(largv, &argz, &argz_len)) {
		fprintf(stderr, "Failed to create argz\n");
		return -1;
	}

	for (int i = 1; i < argc; i++) {
		char *quoted = malloc(strlen(argv[i]) + 3);
		if (!quoted) {
			free(argz);
			return -1;
		}
		quote(quoted, argv[i]);

		if (argz_append(&argz, &argz_len, quoted, strlen(argv[i]) + 3)) {
			fprintf(stderr, "Failed to append %s to argz\n", argv[i]);
			free(quoted);
			return -1;
		}
		free(quoted);
	}

	argz_stringify(argz, argz_len, ' ');

	sig = alloc(sizeof(struct coli_exec_cmd_sig) + argz_len, COLI_EXEC_CMD);
	memcpy(sig->coli_exec_cmd.cmd, argz, argz_len);
	sig->coli_exec_cmd.len = argz_len;
	free(argz);

	if (!ccmd_pid)
		ccmd_pid = register_coli_cmd_server();

	send(&sig, ccmd_pid);
	ref = attach(NULL, ccmd_pid);

	sig = receive(sel_exec);
	detach(&ref);

	if ( (sig->sig_no == COLI_EXEC_CMD_R) &&
			(sig->coli_exec_cmd_r.status == COLI_OK) ) {
		sock_name = malloc(strlen(sig->coli_exec_cmd_r.socket) + 1);

		if ( sock_name != NULL )
			strcpy(sock_name, sig->coli_exec_cmd_r.socket);
	}

	free_buf(&sig);

	if (NULL == sock_name)
		return -1;

	send_fd(sock_name);

	sig = alloc(sizeof(U32), 999);
	send(&sig, ccmd_pid);

	free(sock_name);
	wait_cmd_exit(ccmd_pid);

	return 0;
}

/** ==================================================================== */
/**
 *   Executes the specified linux shell command.
 *   @param name       Command name
 *   @param tagname    Discarded.
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 */
/* ===================================================================== */
static U32 run_native_cmd(const char *cmd, const OSADDRESS *taglist)
{
	char buf[256];
	FILE *ptr;
	int status;

	(void)taglist;

	ptr = popen(cmd, "r");
	if ( ptr== NULL) {
		perror("popen failed:");
		return SHELL_EUNKNOWN_CMD;
	}

	while (fgets(buf, 256, ptr) != NULL) {
		(void) fprintf(stdout, "%s", buf);
	}

	status = pclose(ptr);
	status =  WEXITSTATUS(status);
	return status == 0 ? SHELL_SUCCESS : SHELL_EUNKNOWN_CMD;
}

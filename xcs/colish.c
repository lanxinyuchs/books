/**
 *   Handling of local commands.
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
 *   Revised : 2013-11-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Added support for coli commands integrated into host shell.
 *             If colish is called via a soft link, execute it directly.
 *
 *   Revised : 2012-03-08 Zabihullah Bayat EAB/FJP/RK
 *   Change  : -c option added to just execute commands without starting
 *             shell.
 *
 *   Revised : 2011-11-30 Lars Jönsson EAB/FJP/TB
 *   Change  : All command names are now allocated dynamically.
 *
 *   Revised : 2011-11-25 Lars Jönsson EAB/FJP/TB
 *   Change  : Moved the remaining stuff from run_cmd.c into this file.
 *
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <ctype.h>
#include <editline/readline.h>
#include <histedit.h>
#include <signal.h>
#include <libgen.h>

#include <local_cmd.h>
#include <remote_cmd.h>

#include "whitelist.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define DEFAULT_PROMPT "$ "

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static char  cmd_name[256];

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Executes the command in argv[0].
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
static int exec_command(int argc, char **argv)
{
	int res;
	char **largv;

	if(!strcmp(argv[0], "colish")) {
		fprintf(stderr, "%s: command not allowed\n", argv[0]);
		return -1;
	}

	if ( argc < 1 )
		return -1;

	if ( local_command(argv[0]) )
		return run_local_command(argc, argv);

	largv = whitelist_get_cmd(argv[0]);
	if ( largv )
		return run_remote_linux_command(argc, argv, largv);

	res = run_remote_command(argc, argv);

	return res;
}

/** ==================================================================== */
/**
 *   Finds the next command that starts with the specified command name.
 *
 *   @param expr       Command name or start of a command name
 *   @param prev       Previous command name found. Empty string if this
 *                     is the first search
 *
 *   @return           Pointer to updated prev. Note that prev has to be
 *                     large enough to hold the result.
 *
 *   @par Globals:
 *                     --
 *
 *   This function is used for searching for all commands that starts
 *   with the specified name. One (or none) command is retrieved at each
 *   call. prev should be an empty string the first time. It should be
 *   set to the previuos found command name on all other calls.
 */
/* ===================================================================== */
static char * find_next_command(const char *expr, char *prev)
{
	char  *next_local;
	char  *next_remote;
	char  *next;

	next_local  = next_local_command(expr, prev);
	next_remote = next_remote_command(expr, prev);

	if ( !next_local )
		next = next_remote;
	else if ( !next_remote )
		next = next_local;
	else if ( strcmp(next_local, next_remote) < 0 )
		next = next_local;
	else
		next = next_remote;

	if ( next && (next == next_local) ) {
		free(next_remote);
		next = strdup(next_local);
	}

	return next;
}

/** ==================================================================== */
/**
 *   Generator function for command completion.
 *
 *   @param text       Command name or start of a command name
 *   @param state      Start from scratch if 0, otherwise continue from
 *                     last search
 *
 *   @return           Next command tha matches
 *
 *   @par Globals:
 *                     --
 *
 *   STATE lets us know whether to start from scratch; without any state
 *   (i.e. STATE == 0), then we start at the top of the list.
 */
/* ===================================================================== */
static char * command_generator(const char *text, int state)
{
	char *next;

	/*
	 ** First time
	 */
	if ( !state )
	{
		if ( *text == '\0' )
			return NULL;

		cmd_name[0] = '\0';
	}

	if ( (next = find_next_command(text, cmd_name)) == NULL )
		return NULL;

	/*
	 **  Save for next check
	 */
	strcpy(cmd_name, next);

	return next;
}

/** ==================================================================== */
/**
 *   Attempt to complete on the contents of TEXT.
 *
 *   @param text       Command name or start of a command name
 *   @param start      See description below
 *   @param end        See description below
 *
 *   @return           1 if found, otherwise 0
 *
 *   @par Globals:
 *                     --
 *   START and END bound the region of rl_line_buffer that contains the
 *   word to complete.  TEXT is the word to complete. We can use the
 *   entire contents of rl_line_buffer in case we want to do some simple
 *   parsing. Return the array of matches, or NULL if there aren't any.
 */
/* ===================================================================== */
static char ** fileman_completion(const char* text, int start, int end)
{
	char **matches;

	matches = (char **)NULL;

	/* If this word is at the start of the line, then it is a command
	   to complete.  Otherwise it is the name of a file in the current
	   directory. */
	if (start == 0)
		/* TODO */
		matches = completion_matches(text, command_generator);
	/* matches = rl_completion_matches (text, command_generator); */

	return (matches);
}

/** ==================================================================== */
/**
 *   Initialize readline/editline library.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   Tell the GNU Readline library how to complete.  We want to try to
 *   complete on command names if this is the first word in the line, or
 *   on filenames if not.
 */
/* ===================================================================== */
static void initialize_readline(void)
{
	/* Allow conditional parsing of the ~/.inputrc file. */
	rl_readline_name = "COLI";

	/* Tell the completer that we want a crack first. */
	rl_attempted_completion_function = fileman_completion;
}

/** ==================================================================== */
/**
 *   Strip whitespace from the start and end of a string.
 *
 *   @param string     The string
 *
 *   @return           Pointer to the updated string
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static char * stripwhite (char *string)
{
	register char *s, *t;

	for (s = string; isspace (*s); s++)
		;

	if (*s == 0)
		return (s);

	t = s + strlen (s) - 1;
	while (t > s && isspace (*t))
		t--;
	*++t = '\0';

	return s;
}

static int tty_init()
{
	if (getenv("COLI_SH")) {
		fprintf(stderr, "icolish: recursive invocation not allowed\n");
		return -1;
	}

	if (getenv("LHSH_TTY")) {
		char *ps1 = getenv("PS1");
		if (ps1) {
			setenv("PS2", ps1, 1);
			setenv("PS1", "admin@\\h:\\w\\$", 1);
		}
	}
	else {
		setenv("PS2", "$ ", 1);
	}

	setenv("COLI_SH", "1", 0);

	return 0;
}

/** ==================================================================== */
/**
 *   Main entry point of COLI shell (colish).
 *
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 *
 *   @return           0 if OK, otherwise non-zero (result from command)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int main (int argc, char **argv)
{
	char *line, *s;
	Tokenizer *tok;
	int   local_argc;
	char  **local_argv;
	int ret = 0;
	char *prompt;

	signal(SIGINT, SIG_IGN);
	if (whitelist_init()) {
		fprintf(stderr, "Failed to parse whitelist\n");
		return -1;
	}

	if (tty_init())
		return -1;

	initialize_readline();
	tok = tok_init(NULL);

	if ((argc > 2) && (!strcmp(argv[1], "-c")))
	{
		if ( tok_str(tok, argv[2], &local_argc,
		             (const char ***)&local_argv) == 0 ) {
			ret = exec_command(local_argc, local_argv);

			if (ret == -2) {
				fprintf(stderr, "Invalid command: %s\n", argv[2]);
			}
		}
		tok_end(tok);
		return ret;
	}

	stifle_history(100);

	prompt = getenv("PS2");
	if (!prompt)
		prompt = DEFAULT_PROMPT;

	/* Loop reading and executing lines until the user quits. */
	while ( 1 ) {
		line = readline(prompt);

		if (!line) {
			printf("\n");
			continue;
		}

		/* Remove leading and trailing whitespace from the line.
		   Then, if there is anything left, add it to the history list
		   and execute it. */
		s = stripwhite(line);

		if (*s) {
			char* expansion;
			int result;

			result = history_expand(s, &expansion);

			if (result < 0 || result == 2) {
				fprintf(stderr, "%s\n", expansion);
			}
			else {
				add_history(expansion);

				/*
				 **  Maybe implement and call shell_run_cmd() instead
				 */
				if ( tok_str(tok, line, &local_argc,
				             (const char ***)&local_argv) == 0 ) {
					if (exec_command(local_argc, local_argv) == -2) {
						fprintf(stderr, "Invalid command: %s\n"
						        "Execute 'help' for available commands\n",
						        local_argv[0]);
					}
				}
			}

			free(expansion);
		}

		free(line);
		tok_reset(tok);
	}

	tok_end(tok);

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <sys/stat.h>
#include <malloc.h>

#ifndef __COVERITY__
/* Not using plain tpt to keep this as lightweight as possible. */
#  define TRACEPOINT_CREATE_PROBES
#  define TRACEPOINT_DEFINE
#  include "com_ericsson_xcs_shell_trace.h"
#  define TRACE_ERROR(msg)   \
	tracepoint(TRACEPOINT_PROVIDER, ERROR, file, (int) line, msg);
#  define TRACE_INFO(msg) \
	tracepoint(TRACEPOINT_PROVIDER, INFO, file, (int) line, msg);
#  define TRACE_GROUP(group, msg)    \
	tracepoint(TRACEPOINT_PROVIDER, TRACE ##group, file, (int) line, "-", msg);
#else
#  define TRACE_ERROR(msg) printf("ERROR: { file = \"%s\", line = %d, msg = \"%s\" }\n", file, (int) line, msg)
#  define TRACE_INFO(msg)  printf("INFO: { file = \"%s\", line = %d, msg = \"%s\" }\n", file, (int) line, msg)
#  define TRACE_GROUP(group, msg) printf("TRACE%d: { file = \"%s\", line = %d, obj = \"-\", msg = \"%s\" }\n", group, file, (int) line, msg)
#endif

char *link_names[] = {"error-trace", "info-trace", "group-trace", "tracecmd"};
enum link_type {
        error_trace = 0,
        info_trace,
        group_trace,
        cmd_trace
} link_type;

static void usage(int exit_code)
{
	switch(link_type) {
	case error_trace:
		printf("Usage: %s <message>\n"
		       "  Writes an error trace to the trace and error log.\n"
		       "\n",
		       link_names[error_trace]);
		break;
	case info_trace:
		printf("Usage: %s <message>\n"
		       "  Writes an info trace to the trace and error log.\n"
		       "\n",
		       link_names[info_trace]);
		break;
	case group_trace:
		printf("Usage: %s <group> <message>\n"
		       "  Traces with TRACE<group> to the trace and error log.\n"
		       "\n",
		       link_names[group_trace]);
		break;
	default:
		printf("Usage: %s [options] <message>\n\n"
		       "Options:\n"
		       "  -h | --help                     Print usage information (this message)\n"
		       "  -i | --info                     Trace an INFO trace. (Default)\n"
		       "  -e | --error                    Trace an ERROR trace.\n"
		       "  -g <group> | --group <group>    Trace using a trace group.\n"
		       "  -f <filename> --file <filename> Specify a file name.\n"
		       "  -l <line> --line <line>         Specify a line number.\n"
		       " Note: \"error\", \"info\" and \"group\" are mutually exclusive.\n"
		       "\n",
		       link_names[cmd_trace]);
	}
	exit( exit_code );
}

static int get_script_name(char *name, int name_size)
{
	char fn[sizeof("/proc/xxxxxxxxxxxxxxxxxxxx/cmdline")];
	FILE *f;
	char *cmdline;
	char *script;
	int found;

	snprintf(fn, sizeof(fn), "/proc/%d/cmdline", getppid());
	f = fopen(fn, "r");
	if(!f)
		return 0;
	/* /proc/<pid>/cmdline is a list of nul terminated strings.
	   fscanf %ms reads the entire content of cmdline into the buffer
	   and adds an extra nul at the end, thus ending it with
	   two nul chararcters */
	found = __extension__ fscanf(f, "%ms", &cmdline);
	fclose(f);
	if(!found)
		return 0;
	/* Skipping first paramter which is the interpreter (shell name).
	   Not doing any further investigation about which interpreter is used
	   or even if it's a valid one. */
	script = cmdline + strlen(cmdline) + 1;
	if(!strlen(script)) { /* If no interpreter then executed from command line.*/
		free(cmdline);
		return 0;
	}
	strncpy(name, script, name_size);
	name[name_size - 1] = 0; /* ensure nul termination */
	free(cmdline);
	return 1;
}

static int get_trace_group( char *arg )
{
	char *end  = NULL;
	unsigned long group = strtoul(arg, &end, 0);
	if (*end == '\0' && group >= 1 && group <= 7)
		return group;
	printf("ERROR: \"%s\" is an invalid trace group.\n", arg);
	usage(1);
	return -1; /*just to keep compiler happy, exits in usage().*/
}

int main(int argc, char *argv[])
{
	static struct option options[] = {
		{ "help",  no_argument,       NULL, 'h'},
		{ "info",  no_argument,       NULL, 'i'},
		{ "error", no_argument,       NULL, 'e'},
		{ "group", required_argument, NULL, 'g'},
		{ "file",  required_argument, NULL, 'f'},
		{ "line",  required_argument, NULL, 'l'},
		{ 0, 0, 0, 0 }
	};
	static int opt;
	int next_arg;
	int info  = 0;
	int error = 0;
	int trace = 0;
	unsigned long trace_group = 0;
	unsigned long line = 0;
	char *file = NULL;
	char *base;
	char *end;
	char script_name[100];

	/* a poor mans "basename" */
	char *my_name = strrchr(argv[0], '/');
	my_name = my_name ? my_name + 1 : argv[0];

	/* First check if invoked using one of the symbolic links */
	if( strcmp(my_name, link_names[error_trace] ) == 0 ) {
		link_type = error_trace;
		error = 1;
		next_arg = 1;
	} else if( strcmp(my_name, link_names[info_trace] ) == 0 ) {
		link_type = info_trace;
		info = 1;
		next_arg = 1;
	} else if( strcmp(my_name, link_names[group_trace] ) == 0 ) {
		link_type = group_trace;
		trace = 1;
		if(argc != 3) {
			usage(1);
		}
		trace_group = get_trace_group( argv[1] );
		next_arg = 2;
	} else {
		/* No sym link - use full options */
		link_type = cmd_trace;
		for (;;) {
			opt = getopt_long(argc, argv, "heig:f:l:", options, NULL);
			if(opt == -1)
				break;
			switch( opt ) {
			case 'i':
				info = 1;
				break;
			case 'e':
				error = 1;
				break;
			case 'g':
				trace = 1;
				trace_group = get_trace_group( optarg );
				break;
			case 'f':
				file = optarg;
				break;
			case 'l':
				line = strtoul(optarg, &end, 0);
				if (*end == '\0')
					break;
				printf("ERROR: \"%s\" is an invalid line number.\n", optarg);
				usage(1);
			case 'h':
				usage(0);
			default:
				usage(1);
			}
		}
		next_arg = optind;
	}

	if(argc - next_arg < 1) {
		printf("A message must be specified.\n");
		usage(1);
	}
	if(argc - next_arg > 1) {
		printf("The message must be a single string.\n");
		usage(1);
	}
	if( info + error + trace > 1 ) {
		usage(1);
	}

	if( !file ) {
		if(get_script_name(script_name, sizeof(script_name))) {
			file = script_name;
		} else {
			file = "-";
		}
	}

	base = strrchr(file, '/');
	file = base ? base + 1 : file;

	if( error ) {
		TRACE_ERROR( argv[next_arg] );
		return 0;
	}
	if( trace ) {
		switch( trace_group ) {
		case 1:
			TRACE_GROUP( 1, argv[next_arg] );
			return 0;
		case 2:
			TRACE_GROUP( 2, argv[next_arg] );
			return 0;
		case 3:
			TRACE_GROUP( 3, argv[next_arg] );
			return 0;
		case 4:
			TRACE_GROUP( 4, argv[next_arg] );
			return 0;
		case 5:
			TRACE_GROUP( 5, argv[next_arg] );
			return 0;
		case 6:
			TRACE_GROUP( 6, argv[next_arg] );
			return 0;
		case 7:
			TRACE_GROUP( 7, argv[next_arg] );
			return 0;
		}
	}
	TRACE_INFO( argv[next_arg] );
	return 0;
}

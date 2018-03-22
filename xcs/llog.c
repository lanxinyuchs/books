/******************************************************************************
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 *****************************************************************************/

#include <stdio.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <libgen.h>
#include <errno.h>
#include <ctype.h>

#define LLOG_NUMBER_OF_PARAMETERS 8
#define LLOG_FILE_PATH "/var/log/llog/llog"

enum options {
	LLOG_PRINT_SHORT,
	LLOG_PRINT_LONG,
};

struct command_info {
	int print_option;
	long number;
	char *file;
};

static void print_help()
{
	printf("Syntax: llog [-l][-n][-c][-h][file1 file2 ...]\n"
	       "  llog displays or clears llog entries in the llog(s) or file(s) provided.\n"
	       "  Options:\n"
	       "  -c | --clear   Clear llog after printing\n"
	       "  -h | --help    Prints this help text\n"
	       "  -l | --long    Long format\n"
	       "  -n | --number  Which entry to print\n"
	       "  -f | --file    Specify which log file to use\n");
}

/*
 * This function extracts relevant information from a line and adds
 * them to an array. Relevant information are separated
 * by dollar signs.
 */
static int get_values(char *line, char *values[])
{
	int i = 0;
	char *start;
	char *end;

	strtok(line, "$");

	while (i < LLOG_NUMBER_OF_PARAMETERS) {

		start = strtok(NULL, "$");
		if (start == NULL) {
			return -EINVAL;
		}
		while (isspace(*start)) start++;

		end = start + strlen(start) - 1;
		while (((end > start) &&  isspace(*end)) || (*end == '\n' )) {
			*end = '\0';
			end--;
		}

		values[i] = start;
		i++;
	}
	return 0;
}

/*
 * Function prints the information from llog file in long format.
 */
static void print_long(char *line, int lineNo)
{
	int i = 0;
	char *values[LLOG_NUMBER_OF_PARAMETERS] = { 0, };

	if (!get_values(line, values)) {
		printf("---------------------------------------------------");
		printf("\n");
		printf("No:       %d\n", lineNo);
		printf("Reason:   %s\n", values[i++]);
		printf("Time:     %s\n", values[i++]);
		printf("Program:  %s\n", values[i++]);
		printf("Pid:      %s\n", values[i++]);
		printf("Rank:     %s\n", values[i++]);
		printf("Signal:   %s\n", values[i++]);
		printf("PMD:      %s\n", values[i++]);
		printf("Extra:    %s\n", values[i++]);
	} else {
		printf("---------------------------------------------------");
		printf("\n");
		printf("No:       %d\n", lineNo);
		printf("Failed to retrieve information!\n");
	}

}

/*
 * Print header for short format
 */
static void print_header()
{
	printf("%-4s%-30s%-29s%-21s%-6s%s\n", "NO",
			"REASON/EXTRA",
			"TIME/PMD",
			"PROGRAM",
			"PID",
			"RANK");
	printf("-----------------------------------------------------------------------------------------------");
	printf("\n");

}

/*
 * This function prints the information from llog file in short format.
 */
static void print_short(char *line, int lineNo)
{
	char *values[LLOG_NUMBER_OF_PARAMETERS] = { 0, };
	char *program = NULL;

	if (!get_values(line, values)) {

		program = strtok(values[2], " ");
		program = basename(program);

		printf("%-4d%-30.30s%-29.29s%-21.21s%-6.6s%s\n",
		       lineNo, values[0], values[1], program, values[3], values[4]);
		printf("%-4s%-30.30s%s\n", "", values[7], values[6]);
		printf("-----------------------------------------------------------------------------------------------");
		printf("\n");

	} else {
		printf("%d%3s%s", lineNo, "", "Failed to retrieve information!\n");
		printf("-----------------------------------------------------------------------------------------------");
		printf("\n");
	}

}

/*
 * This function opens llog file and reads the contents line by line.
 * Print functions are called based on the information
 * in command_info structure for each successfully read line.
 */
static int load_and_print(struct command_info *info)
{
	FILE *fd;
	long lineNo = 1;
	size_t len = 0;
	char *line = NULL;

	fd = fopen(info->file, "r");
	if (fd == NULL)
		return -errno;

	while (getline(&line, &len, fd) != -1) {
		if (!strcmp(line, "\n") || line[0] == '\0')
			continue;

		if ( info->number > 0 ) {
			if ( lineNo == info->number )  {
				if ( info->print_option == LLOG_PRINT_SHORT )
					print_short( line, lineNo );
				else
					print_long( line, lineNo );
			} else {
				lineNo++;
				continue;
			}
			break;
		} else if ( info->print_option == LLOG_PRINT_LONG  )
			print_long( line, lineNo );

		else
			print_short( line , lineNo );

		lineNo++;
	}

	fclose(fd);
	return 0;
}

/*
 * Clear the llog
 */
static int clear_log(char *file)
{
	FILE *fd;

	fd = fopen(file, "w");
	if (fd == NULL)
		return -errno;

	fclose(fd);

	return 0;
}

/*
 * Main function
 */
#define LONG_OPTS_END_ {NULL, 0, NULL, 0}
int main(int argc, char *argv[])
{
	int c;
	int res;
	struct command_info info;
	char *end_ptr;

	info.file = LLOG_FILE_PATH;
	info.number = 0;
	info.print_option = LLOG_PRINT_SHORT;

	struct option longopts[] = {
		{"help", no_argument, NULL, 'h'},
		{"long", no_argument, NULL, 'l'},
		{"clear", no_argument, NULL, 'c'},
		{"file", required_argument, NULL, 'f'},
		{"number", required_argument, NULL, 'n'},
		LONG_OPTS_END_
	};

	while ((c = getopt_long(argc, argv, "f:hcln:", longopts, NULL)) != -1) {
		switch (c) {
			case 'h' :
				print_help();
				return 0;
			case 'c' :
				if ((res = clear_log(info.file))) {
					printf("llog: failed to clear llog file: %s.\n", strerror(-res));
					return 1;
				}
				return 0;
			case 'l' :
				info.print_option = LLOG_PRINT_LONG;
				break;
			case 'n' :
				info.number = strtol(optarg, &end_ptr, 0);
				if (*end_ptr) {
					print_help();
					return 1;
				}
				break;
			case 'f' :
				info.file = optarg;
				break;
			default :
				return 0;
		}
	}

	if (info.print_option == LLOG_PRINT_SHORT) {
		print_header();
		if ((res = load_and_print(&info))) {
			printf("llog: failed to load llog file: %s.\n", strerror(-res));
			return 1;
		}
	} else {
		if ((res = load_and_print(&info))) {
			printf("llog: failed to load llog file: %s.\n", strerror(-res));
			return 1;
		}
	}

	return 0;
}

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "llog.h"

#define OP(x) ((x) ? (x) : "-")

int main(int argc, char *argv[])
{
	char *reason = NULL;
	char *datestring = NULL;
	char *program = NULL;
	pid_t pid = 0;
	char *rank = NULL;
	int signal = 0;
	char *pmd = NULL;
	char *extra = NULL;

	if (argc > 1) reason = argv[1];
	if (argc > 2) program = argv[2];
	if (argc > 3) pid = atoi(argv[3]);
	if (argc > 4) rank = argv[4];
	if (argc > 5) signal = atoi(argv[5]);
	if (argc > 6) pmd = argv[6];
	if (argc > 7) extra = argv[7];

	printf("writing to llog: reason %s program %s pid %d "
	       "rank %s signal %d pmd %s extra %s\n", OP(reason),
	       OP(program), pid, OP(rank), signal, OP(pmd), OP(extra));

	llog_write(reason, program, pid, rank, signal, pmd, extra);
}

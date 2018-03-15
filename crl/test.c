#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define TRACEPOINT_DEFINE
#include "com_ericsson_system_start.h"

int main(int argc, char **argv)
{
	int d = 0;
	if (argc == 2)
		d = strtol(argv[1], NULL, 10);

	char buf[24] = {0};
	printf("Start tracing...\n");
	for (int i = 0; i < 1000; i++) {
		sprintf(buf, "System booting(%d), %d", d, i);
		event_system_start(buf);
		usleep(1000);
	}
	printf("Done tracing...\n");
	return 0;
}

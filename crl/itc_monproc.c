#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>

#include "itc.h"
#include "itc_system.h"

#include "itc_ct.h"
#include "itc_loopthreads.h"

int main(int argc, char *argv[])
{
        int ret;

        if(argc < 2 || argc > 3) {
                abort();
        }

        if(argc == 2) {
                ret = procthread(ITC_NO_NAMESPACE, argv[1]);
        } else {
                ret = procthread(argv[1], argv[2]);
        }

        return ret;
}

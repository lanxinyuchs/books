#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "itc_ct.h"

int main(int argc, char *argv[])
{
        int result, i;
        uint32_t tests=0;

        for(i=1 ; i<argc ; i++) {
                tests |= 1 << strtol(argv[i], NULL, 0);
        }

        if(tests == 0) {
                tests = 0xFFFFFFFF;
        }

        if(tests & (1<<0)) {
                result = run_sched_params_test();
                if(result != 0) {
                        printf("Scheduling policy parameters test failed: %d\n",
                               result);
                        return result;
                }
        }

        if(tests & (1<<1)) {
                result = run_pooltest();
                if(result != 0) {
                        printf("Pool test failed: %d\n", result);
                        return result;
                }
        }

        if(tests & (1<<2)) {
                result = run_mbox_tests();
                if(result != 0) {
                        printf("Mbox test failed: %d\n", result);
                        return result;
                }
        }

        if(tests & (1<<3)) {
                result = loc_mon_test();
                if(result != 0) {
                        printf("Locate/monitor test failed: %d\n", result);
                        return result;
                }
        }

        if(tests & (1<<4)) {
                result = run_lnhtest();
                if(result != 0) {
                        printf("lnh test failed: %d\n", result);
                        return result;
                }
        }

        printf("ITC test PASSED\n");

        return 0;
}

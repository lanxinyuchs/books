
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define MAX_DEV_NAME 128
#define TEST_PROFILE "/sys/atf/anp-profile-standard"

int ecb_dl_mux_prog(void **prog, const char *profile, int channel);
int ecb_dl_mux_set(void *prog);

struct mux_test_prog {
	char profile_name[MAX_DEV_NAME];
	int channel;
};

static struct mux_test_prog test_prog;

int ecb_dl_mux_prog(void **prog, const char *profile, int channel)
{
	printf("ENTER: mux prog\n");

	strcpy(test_prog.profile_name, TEST_PROFILE);
	test_prog.channel = 0;

	if((strcmp(profile, test_prog.profile_name) != 0) ||
			(channel != test_prog.channel)) {
		printf("received channel or profile different than hardcoded \n"
		       "received: %s %d \n"
		       "hardcoded: %s %d \n"
		       "prog %p\n", profile, channel, test_prog.profile_name,
		       test_prog.channel, *prog);
		return -1;
	}

	return 0;

}

int ecb_dl_mux_set(void *prog)
{
	printf("ecb_dl_mux_set prog %p \n", prog);

	return 0;
}

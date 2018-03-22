
#include <stdio.h>
#include <stdlib.h>

struct mux_test_prog {
	int channel;
};

int ecb_dl_mux_prog(void **prog, const char *profile, int channel)
{
	struct mux_test_prog *test_prog;

	printf("ecb_dl_mux_prog(\"%s\", %d)\n", profile, channel);

	if (*prog) {
		free(*prog);
	}

	test_prog = *prog = calloc(1, sizeof(struct mux_test_prog));

	if (test_prog) {
		test_prog->channel = channel;
		return 0;
	}

	return -1;
}

int ecb_dl_mux_set(void *prog)
{
	struct mux_test_prog *test_prog = (struct mux_test_prog*) prog;

	printf("ecb_dl_mux_set(%d)\n", test_prog->channel);

	return 0;
}

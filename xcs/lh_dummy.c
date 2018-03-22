
#include <stdio.h>
#include <stdlib.h>

#include <itc.h>
#include <itc_system.h>


static itc_mbox_id_t lnh_assign(char *path)
{
	itc_mbox_id_t mbox;

	mbox = itc_create_mailbox(path, 0);
	if (mbox == ITC_NO_ID) {
		exit(EXIT_FAILURE);
	}
	itc_assign_linkhandler(path, mbox);

	return mbox;
}

static void lnh_deassign(char *path, itc_mbox_id_t mbox)
{
	itc_deassign_linkhandler(path, mbox);
	itc_delete_mailbox(mbox);
}

int main(int argc, char *argv[])
{
	itc_mbox_id_t mbox;
	int           res;

	if (argc != 2) {
		printf("Syntax: %s <path>\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	res = itc_init(4, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if (res != 0) {
		exit(EXIT_FAILURE);
	}

	mbox = lnh_assign(argv[1]);

	printf("Please press enter\n");
	(void) getchar();

	lnh_deassign(argv[1], mbox);

	return 0;
}

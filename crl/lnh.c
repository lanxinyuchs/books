#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <itc.h>

#include <ulh_transport.h>
#include <ulh_lnh.h>
#include <ulh_lnh_msg.h>
#include <ulh_cm.h>
#include <ulh_rio.h>

int main(int argc, char **argv)
{
	struct ulh_lnh *lnh;
	itc_mbox_id_t me;
	int ret;

	itc_init(32, ITC_MALLOC, NULL, NULL, 0);
	me = itc_create_mailbox("main", 0);

	ret = ulh_trans_init(32);
	if (ret) {
		printf("ulh_trans_init() failed, %d\n", ret);
		exit(-1);
	}
	ret = ulh_cm_init();
	if (ret) {
		printf("ulh_cm_init() failed, %d\n", ret);
		exit(-1);
	}
	ret = ulh_rio_init("srio");
	if (ret) {
		printf("ulh_rio_init() failed, %d\n", ret);
		exit(-1);
	}
	lnh = ulh_lnh_create("first_lnh");
	if (!lnh) {
		printf("ulh_lnh_create() failed, %d\n", ret);
		exit(-1);
	}

	for (;;) {
		sleep(60);
	}

	ulh_lnh_destroy(lnh);
	return 0;
}

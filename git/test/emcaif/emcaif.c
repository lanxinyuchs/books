#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <signal.h>
#include <pthread.h>

#include <itc.h>

#include <emca-link-api.h>

union itc_msg {
	uint32_t               msgno;
	struct emca_link_event event;
};

struct test_data {
	pthread_t                 tid;
	uint32_t                  snid;
	int                       wait_event;
	int                       test;
	int                       res;
};

int run_in_single_thread(uint32_t *snids, int num, int wait_event, int test)
{
	itc_mbox_id_t mbox;
	struct emca_link_if *handle;
	struct emca_link_config cfg;
	uint32_t link_instance[4];
	int i, rc = 0;

	mbox = itc_create_mailbox("emca_if_test", 0);
	if (mbox == ITC_NO_ID) {
		printf("itc_create_mailbox() failed\n");
		return -1;
	}
	if (emca_link_init(&handle)) {
		printf("emca_link_init() failed\n");
		return -1;
	}
	printf("emca_link_init() OK\n");

	for (i=0; i<num; i++) {
		cfg.owner_mbox = mbox;
		cfg.snid = snids[i];
		if (emca_link_create(handle, &cfg, &link_instance[i])) {
			printf("emca_link_create() failed\n");
			rc = -1;
			goto done;
		}
	}
	if (!wait_event)
		goto destroy_link;

	printf("emca_link_create() OK : sleep....\n");

	for (;;) {
		union itc_msg *msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {
		case EMCA_LINK_UP:
			printf("LINK_UP: link_name %s instance_id %d\n",
			       msg->event.link_name, msg->event.instance);
			break;
		case EMCA_LINK_DOWN:
			printf("LINK_DOWN: link_name %s instance_id %d\n",
			       msg->event.link_name, msg->event.instance);
			break;
		default:
			printf("Unexpected sig %u\n", msg->msgno);
			rc = -1;
			goto done;
		}
		if (test == 1)
			goto destroy_link;
		if (test == 2)
			goto done;
	}

destroy_link:
	for (i=0; i<num; i++) {
		if (emca_link_destroy(handle, link_instance[i])) {
			rc = -1;
			printf("emca_link_destroy() failed\n");
		} else
			printf("emca_link_destroy() ok\n");
	}
done:
	emca_link_shutdown(&handle);

	itc_delete_mailbox(mbox);
	return rc;
}
void *test_thread(void *data)
{
	struct test_data *t = data;
	uint32_t          snid[1];

	snid[0]= t->snid;
	int rc = run_in_single_thread(snid, 1, t->wait_event, t->test);

	if (!rc)
		t->res = 1;

	pthread_exit(0);
}
int run_in_threads(uint32_t *snids, int num, int wait_event, int test)
{
	struct test_data t[4];
	int i, x, res = 0;

	for (i=0; i<num; i++) {
		t[i].snid = snids[i];
		t[i].wait_event = wait_event;
		t[i].test = test;
		t[i].res = -1;
		if (pthread_create(&t[i].tid, NULL, test_thread, &t[i]))
			goto cancel;
	}
	goto done;
cancel:
	for (x = 0; x<i; x++)
		pthread_cancel(t[x].tid);
done:
	for (x = 0; x<i; x++) {
		pthread_join(t[x].tid, NULL);
		res += t[x].res;
	}
	if (res < num)
		return -1;
	return 0;
}
int main(int argc, char **argv)
{
	uint32_t snids[4];
	int      id = 0, wait_event = 0, test = 0, multi_thread = 0, rc;

        for (;;) {
                static struct option loptions[] = {
                        /* These options set a verb flag. */
                        {"mthrd",   no_argument,       0, 'm'},
                        {"snid",    required_argument, 0, 's'},
                        {"test",    required_argument, 0, 't'},
                        {"wait",    no_argument,       0, 'w'},
                        {0, 0, 0, 0}
                };
                int opti = 0;
		int c = getopt_long(argc, argv, "ms:t:w",
				    loptions, &opti);
                if (c == -1)
                        break;

                switch (c) {
		case 'm':
			multi_thread = 1;
			break;
		case 's':
			if (id < 4)
				snids[id++] = atoi(optarg);
			break;
		case 't':
			test = atoi(optarg);
			break;
		case 'w':
			wait_event = 1;
			break;
		case '?':
			printf("Opt %c ignored\n", c);
			break;
		}
	}

	itc_init(32, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if (multi_thread)
		rc = run_in_threads(snids, id, wait_event, test);
	else
		rc = run_in_single_thread(snids, id, wait_event, test);

	if (rc)
		printf("Test failed\n");
	else
		printf("Test OK\n");
	itc_exit();
	return 0;
}

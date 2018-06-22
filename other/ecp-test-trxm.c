#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <pthread.h>
#include <sys/wait.h>
#include <sys/poll.h>
#include <sys/queue.h>
#include <arpa/inet.h>
#include <itc.h>
#include "bptrxm.sig"
#define TRXM_EXIT_SIGNAL     (EFS_SIGBASE + 25)

static   itc_mbox_id_t test_mailbox[256];
static   itc_mbox_id_t trxm_part = ITC_NO_ID;
static   uint32_t exitstate = 0;
static void *test(void * arg)
{

	char ml_name[32];

	union itc_msg *sig = NULL;
	uint32_t numof = *((uint32_t*)arg);
	snprintf(ml_name, sizeof(ml_name), "trxmd%d", numof);
	test_mailbox[numof] = itc_create_mailbox(ml_name, 0);
	if (test_mailbox[numof] == ITC_NO_ID) {
		printf("test%d create mailbox failure: %d\n", numof,test_mailbox[numof]);
		itc_exit();
		return NULL;
	}
#if 1
	/* block exit signal */
	sigset_t mask;
	sigemptyset(&mask);
	sigaddset(&mask, SIGTERM);
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1) {
		printf("pthread_sigmask failed\n");
		return NULL;
	}
#endif
	printf("trxm_test thread start , mail name is %s \n",ml_name);
	 while (1) {
		itc_mbox_id_t sender;

 		sig = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                sender = itc_sender(sig);

                switch (sig->sigNo) {
                case THREAD_EXIT:
                        itc_free(&sig);
                        return NULL;
		case BP_SEND_SIGNAL:
			printf("test%d BP_SEND_SIGNAL from %d , count is %d\n", numof,sig->if_req.testNo,sig->if_req.count);
			union itc_msg *reply;
			reply = itc_alloc(sizeof(struct bptrxmsendsignal) + sig->if_req.datasize,
                          TRXM_REPLY_SIGNAL);
			reply->if_reply.testNo = sig->if_req.testNo;
			reply->if_reply.count = sig->if_req.count;
                        reply->if_reply.datasize = sig->if_req.datasize;
                        memcpy(reply->if_reply.buffer,sig->if_req.buffer,sig->if_req.datasize);

			itc_send(&reply,sender,ITC_MY_MBOX);
			printf("test%d send count %d back\n", numof,sig->if_req.count);
			break;
		default:
                        printf("Unrecognized signal 0x%08x from 0x%08x", sig->sigNo, sender);
			break;
		}
	        if(sig != NULL)
		   itc_free(&sig);
	}
	return NULL;
}

static int trxm(uint32_t num_thread)
{
	int res;
	uint32_t i = 0;
	union itc_msg *msg;
	pthread_t message_thread[256];
	uint32_t numof[256];
	void * (*trxm_test)(void *) = test;
	printf("start %d trxm_test thread\n",num_thread);
	for(i = 0; i < num_thread; i++) {
		numof[i] = i;
		res = pthread_create(&message_thread[i], NULL, trxm_test, &numof[i]);
		if(res) {
			printf("Failed to create message thread (error:%d)", res);
			return -1;
		}
	}

	for (;;) {
		msg = itc_receive(ITC_NOFILTER,
		                  1000,
		                  ITC_FROM_ALL);
                if (exitstate == 1) {
			for(i = 0; i < num_thread; i++) {
				union itc_msg *sig;
				sig = itc_alloc(sizeof(struct threadexitsignal),THREAD_EXIT);
				itc_send(&sig, test_mailbox[i], ITC_MY_MBOX);
				(void) pthread_join(message_thread[i], NULL);
			}
			if (msg != NULL)
                        	itc_free(&msg);
			return 0;
		}
		if (msg != NULL)
			itc_free(&msg);
	}
	return 0;
}
/**
 * Function exit_handler
 */
static void trxm_exit(int sig)
{
        if (sig == SIGTERM)
		exitstate = 1;

}
static void print_usage()
{
	printf("Built: %s %s\n", __DATE__ , __TIME__);
	printf("Run with arguments trxm_part [<thread number>]\n");
	printf("Example,  setup 10 thread: trxm_part 10 \n");
	printf("Note: the thread number must equal to or bigger than BP thread number \n");
}
int main(int argc, char **argv)
{
	uint32_t num_of_thread = 1;
	int ret;

	struct sigaction exit_act;

        exitstate = 0;
	ret = itc_init(32, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n", ret);
		return -1;
 	}
	trxm_part = itc_create_mailbox("trxm_part", 0);
	if (trxm_part == ITC_NO_ID) {
		printf("trxm_part: Unable to create ITC mailbox!\n");
		return -1;
	}

	if (argc < 2) {
		print_usage();
		return 0;
	}
	if(argc == 2) {
		num_of_thread = atoi(argv[1]); //number of create thread
		if(num_of_thread > 256) {
			printf("Usage: Max thread number is 256 \n");
			return -1;
		}
	}
	memset(&exit_act, '\0', sizeof(exit_act));
	exit_act.sa_handler = &trxm_exit;
        exit_act.sa_flags = 0;
	if (sigaction(SIGTERM, &exit_act, NULL) < 0) {
		printf("Failed to install signal exit handler\n");
		exit(1);
	}
	trxm(num_of_thread);

	return 0;
}


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

#define RECEIVE_TMO 15000
struct thread_param {
	char link[32];
	uint32_t threadnum;
	uint32_t sendnum;
	uint32_t datasize;
};

static   itc_mbox_id_t test_mailbox[256];

static inline void check_recv_signal(uint32_t threadnum,union itc_msg *sig,
		uint32_t expect,uint32_t expect_count,uint32_t expect_datasize)
{
        uint32_t i;
	if (sig->sigNo == ITC_MONITOR_DEFAULT_NO) {
		printf("remote died\n");
		exit(-1);
	}
	if (sig->sigNo == expect && sig->if_reply.count == expect_count && sig->if_reply.datasize == expect_datasize)
        {
		for(i = 0; i < expect_datasize; i++ ) {
			if(sig->if_reply.buffer[i] != '1')
			goto error;
		}
		printf("test%d receive: count is %d, testno is %d , datasize is %d , data is %s success!!!!!\n",
			threadnum,sig->if_reply.count,sig->if_reply.testNo,sig->if_reply.datasize,sig->if_reply.buffer);
		return;
        }
error:
	printf("ERROR:unexpected recevie result!!!! \n");
}
static itc_mbox_id_t hunt_trxmd(itc_mbox_id_t mybox,char *link,uint32_t  num)
{
	char name[128];
	union itc_msg *sig;
	itc_mbox_id_t spid;
	char str_shelld[] = {"trxmd"};

	if (link && strlen(link)) {
		snprintf(name, sizeof(name), "%s/%s%d", link, str_shelld, num);
	} else {
		snprintf(name, sizeof(name), "%s", str_shelld);
	}
	printf("hunt link is %s \n",name);
	itc_locate_async(name, NULL, ITC_MY_MBOX);

	sig = itc_receive(ITC_NOFILTER, 3000, ITC_FROM_ALL);

	if (sig == NULL)
		 return 0;

	if (sig->sigNo != ITC_LOCATE_DEFAULT_NO) {
		printf("unexpected signal 0x%x\n", sig->sigNo);
		return 0;
	}

	spid = itc_sender(sig);
	itc_free(&sig);

	return spid;
}
static void *test(void *arg)
{

	itc_mbox_id_t trxm_pid;
	struct thread_param * p_param = arg;
	uint32_t i = 0;
	union itc_msg *sig;
	char ml_name[32];
	snprintf(ml_name, sizeof(ml_name), "bp_trxm__%d", p_param->threadnum);
	printf("create mailbox %s \n", ml_name);
	test_mailbox[p_param->threadnum] = itc_create_mailbox(ml_name, 0);
	if (test_mailbox[p_param->threadnum] == ITC_NO_ID) {
		printf("test%d create mailbox failure: %d\n",p_param->threadnum, test_mailbox[p_param->threadnum]);
		itc_exit();
		return NULL;
	}
 	printf("me mailbox is %d \n",test_mailbox[p_param->threadnum]);
	trxm_pid = hunt_trxmd(test_mailbox[p_param->threadnum],p_param->link,p_param->threadnum);
	if (!trxm_pid) {
		printf("unable to hunt %s/trxmd%d\n", p_param->link,p_param->threadnum);
		return NULL;
	}
	printf("hunt trxmd%d success \n",p_param->threadnum);
	itc_monitor(trxm_pid, NULL);

	for (i = 0; i < p_param->sendnum; i++) {
		sig = itc_alloc(sizeof(struct bptrxmsendsignal) + p_param->datasize,
			BP_SEND_SIGNAL);
		if (!sig) {
			printf("test%d itc alloc BP_SEND_SING ERROR \n",p_param->threadnum);
		}
		sig -> if_req.testNo = p_param->threadnum;
		sig -> if_req.count = i;
		sig -> if_req.datasize = p_param->datasize;
		memset(sig->if_req.buffer,'1',p_param->datasize);
		itc_send(&sig, trxm_pid, test_mailbox[p_param->threadnum]);
		sig = itc_receive(ITC_NOFILTER, RECEIVE_TMO, ITC_FROM_ALL);
		if (sig == NULL) {
			printf("test%d  count %d timeout\n",p_param->threadnum,i);

		}
		check_recv_signal(p_param->threadnum,sig,TRXM_REPLY_SIGNAL,i,p_param->datasize);
		//check receive
		itc_free(&sig);
	}
	printf("bp test%d completed\n",p_param->threadnum);
	return NULL;
}


static int bp(char *link,uint32_t num_thread,uint32_t sendnum,uint32_t data_size)
{
 	int res;
	uint32_t i = 0;
	pthread_t message_thread[256];
	struct thread_param *t_param;

	void * (*bp_test)(void *) = test;
	printf("start %d bp_test thread\n",num_thread);
	for(i = 0; i < num_thread; i++) {
		t_param = calloc(sizeof(struct thread_param),1);
		strncat(t_param->link,link,32);
		t_param->threadnum = i;
		t_param->sendnum = sendnum;
		t_param->datasize = data_size;
		printf("pthread create : thread num is %d ,send data number is%d,data size is%d\n",
			t_param->threadnum,t_param->sendnum,t_param->datasize );
		res = pthread_create(&message_thread[i], NULL, bp_test, t_param);
		if(res) {
			printf("Failed to create message thread (error:%d)", res);
			return -1;
		}
	}
	for(i =0; i < num_thread; i++) {
		pthread_join(message_thread[i], NULL);
	}
	return 0;
}
static void print_usage()
{
	printf("Built: %s %s\n", __DATE__ , __TIME__);
	printf("Run with arguments bp_part <link> [<thread number> <send number> <data size>]\n");
	printf("Example, send data to trxm3, setup 10 thread, each thread send data to trxm 50 times, data size is 100: bp_part trxm3 10 50 100 \n");
	printf("Note: the thread number must equal to or smaller than trxm thread number \n");
}
int main(int argc, char **argv)
{
	uint32_t num_of_thread = 1;
	uint32_t send_num = 10;
	uint32_t data_size = 10;
	int ret;

	ret = itc_init(32, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n", ret);
		return -1;
	}

	if (argc < 2) {
		print_usage();
		return 0;
	}
	if( argc >= 3 ) {
		num_of_thread = atoi(argv[2]); //number of create thread
		if(num_of_thread > 256) {
			printf("Usage: Max thread number is 256 \n");
			return -1;
		}
	}
	if(argc >= 4) {
		send_num = atoi( argv[3] );
	}
	if( argc >= 5 ) {
		data_size = atoi( argv[4] );
	}

	bp(argv[1],num_of_thread,send_num,data_size);

	return 0;
}

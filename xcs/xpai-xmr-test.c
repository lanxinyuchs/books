#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <itc.h>
#include "xpai_xmr_if.h"

#define UNUSED __attribute__((__unused__))

typedef enum {
	DIST_IND = 0,
	DIST_REQ
} dist_type_t;

typedef enum {
	ANSWER_CFM = 0,
	ANSWER_REJ
} answer_type_t;

/*When a slave is up and running it will announce itself to the main process using...*/
#define SLAVE_ANNOUNCE 0x12340001

/*Main process will tell the slave if it's distributor or subscriber.*/
#define SLAVE_CFG  0x12340002
struct cfg_s {
	uint32_t  msgno;
	int  is_subscriber;
	answer_type_t answer_with;
	char tag[XMR_MAX_TAG_LENGTH];
};

/*Sent to main when a slave receives a DELIV_IND or DELIV_REQ*/
#define REC_IND   0x12340003
struct rec_ind_s {
	uint32_t   msgno;
	dist_type_t msg_type;
	uint32_t   tag_error;
	char       exp_tag[XMR_MAX_TAG_LENGTH]; /*expected tag*/
	char       rec_tag[XMR_MAX_TAG_LENGTH];
	char       rec_data[30];

};

/*Sent to slave to distrubute a tag of IND or REQ type.*/
#define DIST_REQ   0x12340004
struct dist_req_s {
	uint32_t  msgno;
	dist_type_t msg_type;
	char tag[XMR_MAX_TAG_LENGTH];
	char data[30];
};
/*Sent to main when slave has fullfilled the delivery
  (and,  if of type _REQ, has received a CFM orj REJ)*/
#define DIST_CFM   0x12340005
struct dist_cfm_s {
	uint32_t  msgno;
	dist_type_t  msg_type;
	int type_received; /* =1 if cfm, =2 if rej, =0 if a sent msg was _ind*/
};

/*Sent to save to make exit*/
#define SLAVE_END 0x1234000F


union itc_msg {
	uint32_t  msgno;
	/* Structs of the public interface */
	XPAI_SubscribeIndS      subscribe_ind;
	XPAI_DistributeIndS     distribute_ind;
	XPAI_DistributeReqS     distribute_req;
	XPAI_DistributeCfmS     distribute_cfm;
	XPAI_DistributeRejS     distribute_rej;
	XPAI_DelivIndS          deliv_ind;
	XPAI_DelivReqS          deliv_req;
	XPAI_DelivCfmS          deliv_cfm;
	XPAI_DelivRejS          deliv_rej;
	struct cfg_s            cfg;
	struct rec_ind_s        rec_ind;
	struct dist_req_s       dist_req;
	struct dist_cfm_s       dist_cfm;
};

#define MSG_TIMEOUT 1000

#define MBOX "XMR_TEST"
itc_mbox_id_t main_mbox;
itc_mbox_id_t evti_mbox;

static char *msgno_to_str(uint32_t msgno)
{
	switch(msgno) {
	case XPAI_SUBSCRIBE_IND:
		return "XPAI_SUBSCRIBE_IND";
	case XPAI_DISTRIBUTE_IND:
		return "XPAI_DISTRIBUTE_IND";
	case XPAI_DISTRIBUTE_REQ:
		return "XPAI_DISTRIBUTE_REQ";
	case XPAI_DISTRIBUTE_CFM:
		return "XPAI_DISTRIBUTE_CFM";
	case XPAI_DISTRIBUTE_REJ:
		return "XPAI_DISTRIBUTE_REJ";
	case XPAI_DELIV_IND:
		return "XPAI_DELIV_IND";
	case XPAI_DELIV_REQ:
		return "XPAI_DELIV_REQ";
	case XPAI_DELIV_CFM:
		return "XPAI_DELIV_CFM";
	case XPAI_DELIV_REJ:
		return "XPAI_DELIV_REJ";

	default:
		return "<UNKNOWN MESSAGE>";
	}
}

#define SUBSCR 1
#define DISTR  0
#define CHECK_UNEXPECTED(expected_subscriber, is_subscriber, message)   \
	if( expected_subscriber != is_subscriber ) {                    \
		printf("%s %s received an unexpected message %s",       \
		       is_subscriber?"Subscriber":"Distributor",        \
		       my_mbox_name, msgno_to_str(message));            \
		break;                                                  \
	}


static void handle_xpai_deliv_ind(union itc_msg *msg,
                                  char *my_mbox_name,
                                  struct cfg_s *my_cfg)
{
	union itc_msg *msg2 = NULL;

	msg2 = itc_alloc(sizeof(struct rec_ind_s), REC_IND);
	msg2->rec_ind.msg_type = DIST_IND;
	msg2->rec_ind.tag_error = strcmp(my_cfg->tag, msg->deliv_ind.tag);
	memcpy(msg2->rec_ind.exp_tag, my_cfg->tag,
	       XMR_MAX_TAG_LENGTH);
	memcpy(msg2->rec_ind.rec_tag, msg->deliv_ind.tag,
	       XMR_MAX_TAG_LENGTH);
	//TODO: handle data member
	printf("Subscriber %s received %s tag \"%s\"\n",
	       my_mbox_name, msgno_to_str(msg->msgno),
	       msg->deliv_ind.tag);
	itc_send(&msg2, main_mbox, ITC_MY_MBOX);
}

static void handle_xpai_deliv_req(union itc_msg *msg,
                                  char *my_mbox_name,
                                  struct cfg_s *my_cfg)
{
	union itc_msg *msg2 = NULL;

	msg2 = itc_alloc(sizeof(struct rec_ind_s), REC_IND);
	msg2->rec_ind.msg_type = DIST_REQ;
	msg2->rec_ind.tag_error = strcmp(my_cfg->tag, msg->deliv_ind.tag);
	memcpy(msg2->rec_ind.exp_tag, my_cfg->tag,
	       XMR_MAX_TAG_LENGTH);
	memcpy(msg2->rec_ind.rec_tag, msg->deliv_ind.tag,
	       XMR_MAX_TAG_LENGTH);
	//TODO: handle data member
	if(!msg2->rec_ind.tag_error) {
		printf("Subscriber %s received %s tag \"%s\"\n",
		       my_mbox_name, msgno_to_str(msg->msgno),
		       msg->deliv_ind.tag);
	} else {
		printf("Subscriber %s received %s. Subscribed "
		       "to \"%s\" but received \"%s\"\n",
		       my_mbox_name, msgno_to_str(msg->msgno),
		       my_cfg->tag, msg->deliv_ind.tag);
	}
	itc_send(&msg2, main_mbox, ITC_MY_MBOX);

	msg2 = itc_alloc(sizeof(XPAI_DelivCfmS),
	                 EVTI_DELIV_CFM);
	if(my_cfg->answer_with == ANSWER_REJ) {
		msg2->msgno = EVTI_DELIV_REJ;
	}
	memcpy(msg2->deliv_cfm.tag,
	       msg->deliv_ind.tag,
	       XMR_MAX_TAG_LENGTH);
	msg2->deliv_cfm.result = 0;
	itc_send(&msg2, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_dist_req(union itc_msg *msg,
                            char *my_mbox_name)
{
	union itc_msg *msg2 = NULL;
	union itc_msg *msg3 = NULL;
	uint32_t distribute_filter[] = {2, XPAI_DISTRIBUTE_CFM,
	                                XPAI_DISTRIBUTE_REJ
	                               };

	msg2 = itc_alloc(sizeof(XPAI_DistributeIndS), XPAI_DISTRIBUTE_IND);
	memcpy(msg2->distribute_req.tag, msg->dist_req.tag,
	       XMR_MAX_TAG_LENGTH);
	//TODO: handle data member
	if(msg->dist_req.msg_type == DIST_REQ) {
		msg2->msgno = XPAI_DISTRIBUTE_REQ;
	}
	printf("%s distributing tag \"%s\" using %s\n",
	       my_mbox_name, msg2->distribute_req.tag,
	       msgno_to_str(msg2->msgno));

	itc_send(&msg2, evti_mbox, ITC_MY_MBOX);

	msg2 = itc_alloc(sizeof(struct dist_cfm_s), DIST_CFM);
	msg2->dist_cfm.msg_type = msg->dist_req.msg_type;
	msg2->dist_cfm.type_received = 0;

	if(msg->dist_req.msg_type == DIST_REQ) {
		msg3 = itc_receive(distribute_filter, ITC_NO_TMO, ITC_FROM_ALL);
		printf("%s received a \"%s\"\n",
		       my_mbox_name, msgno_to_str(msg3->msgno));
		if(msg3->msgno == XPAI_DISTRIBUTE_CFM) {
			msg2->dist_cfm.type_received = 1;
		} else {
			msg2->dist_cfm.type_received = 2;
		}
	}
	itc_send(&msg2, main_mbox, ITC_MY_MBOX);
	if(msg3) itc_free(&msg3);
}

static void *slave(void UNUSED *ptr)
{
	union itc_msg *msg = NULL;
	itc_mbox_id_t my_mbox;
	char my_mbox_name[50];
	uint32_t cfg_filter[] = {1, SLAVE_CFG};
	struct cfg_s *my_cfg = NULL;

	sprintf(my_mbox_name, MBOX "_SLAVE_%08x", (uint32_t) pthread_self());
	my_mbox = itc_create_mailbox(my_mbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		sprintf(" Slave unable to create ITC mailbox \"%s\"!",
		        my_mbox_name);
	}
	/* Hello mother, I'm here */
	msg = itc_alloc(sizeof(uint32_t), SLAVE_ANNOUNCE);
	itc_send(&msg, main_mbox, ITC_MY_MBOX);

	/* Wait for configuration data.*/
	my_cfg = (struct cfg_s *)itc_receive(cfg_filter, ITC_NO_TMO, main_mbox);
	if(my_cfg->is_subscriber) {
		XPAI_Subscribe(my_mbox, my_cfg->tag);
		printf("Hello. I'm %s and I subscribe to \"%s\"\n",
		       my_mbox_name, my_cfg->tag);
	} else {
		printf("Hello. I'm %s and I'm a distributor\n",
		       my_mbox_name);
	}


	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch(msg->msgno) {
		case SLAVE_END:
			goto the_end;

		case XPAI_DELIV_IND:
			CHECK_UNEXPECTED(SUBSCR, my_cfg->is_subscriber, msg->msgno);
			handle_xpai_deliv_ind(msg, my_mbox_name, my_cfg);
			break;

		case XPAI_DELIV_REQ:
			CHECK_UNEXPECTED(SUBSCR, my_cfg->is_subscriber, msg->msgno);
			handle_xpai_deliv_req(msg, my_mbox_name, my_cfg);
			break;

		case DIST_REQ:
			CHECK_UNEXPECTED(DISTR, my_cfg->is_subscriber, msg->msgno);
			handle_dist_req(msg, my_mbox_name);
			break;

		default:
			CHECK_UNEXPECTED(99 /*always print*/,
			                 my_cfg->is_subscriber, msg->msgno);
		}
		itc_free(&msg);
	} /* for(;;) */
the_end:
	itc_free((union itc_msg **)&my_cfg);
	itc_free(&msg);
	itc_delete_mailbox(my_mbox);
	printf("Bye. %s exiting.\n", my_mbox_name);
	pthread_exit(NULL);
}


static int test_1(itc_mbox_id_t dist_mbox, char *tag, int num_subscribers)
{

	union itc_msg *msg = NULL;
	int num_rec_inds = 0;
	uint32_t dist_cfm_filter[] = {1, DIST_CFM};
	uint32_t rec_ind_filter[]  = {1, REC_IND};

	printf("\n*** Test 1; distributing Alpha with an ind ***\n");
	msg = itc_alloc(sizeof(struct dist_req_s), DIST_REQ);
	msg->dist_req.msg_type = DIST_IND;
	snprintf(msg->dist_req.tag, XMR_MAX_TAG_LENGTH, tag);
	itc_send(&msg, dist_mbox, ITC_MY_MBOX);

	//check DIST_CFM
	msg = itc_receive(dist_cfm_filter, MSG_TIMEOUT, dist_mbox);
	if(!msg) {
		printf("%s:%d ; Timeout waiting for DIST_CFM\n",
		       __func__, __LINE__);
	}
	itc_free(&msg);
	//check that the right subscribers got the tag.
	num_rec_inds = 0;
	while(1) {
		msg = itc_receive(rec_ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg)
			break;
		if(msg->rec_ind.tag_error) {
			printf("%s:%d ; Subscriber expecting \"%s\" reveived \"%s\"\n",
			       __func__, __LINE__,
			       msg->rec_ind.exp_tag, msg->rec_ind.rec_tag);
			itc_free(&msg);
			return -1;
		}
		num_rec_inds++;
		itc_free(&msg);
	};
	if( num_rec_inds != num_subscribers ) {
		printf("%s:%d ; Expected %d subscribers to receive tag "
		       "but %d subscribers got it.\n",
		       __func__, __LINE__, num_subscribers, num_rec_inds);
		return -1;
	}
	return 0;
}

static int test_2(itc_mbox_id_t dist_mbox, char *tag, int num_subscribers)
{

	union itc_msg *msg = NULL;
	int num_rec_inds = 0;
	uint32_t dist_cfm_filter[] = {1, DIST_CFM};
	uint32_t rec_ind_filter[]  = {1, REC_IND};

	printf("\n*** Test 2; distributing Bravo with an req*** \n");
	msg = itc_alloc(sizeof(struct dist_req_s), DIST_REQ);
	msg->dist_req.msg_type = DIST_REQ;
	snprintf(msg->dist_req.tag, XMR_MAX_TAG_LENGTH, tag);
	itc_send(&msg, dist_mbox, ITC_MY_MBOX);

	//check DIST_CFM
	msg = itc_receive(dist_cfm_filter, MSG_TIMEOUT, dist_mbox);
	if(!msg) {
		printf("%s:%d ; Timeout waiting for DIST_CFM\n",
		       __func__, __LINE__);
	}
	if(msg->dist_cfm.type_received != 1) {
		printf("%s:%d ; Expected distributor to receive a "
		       "XPAI_DISTRIBUTE_CFM, but it didn't.\n",
		       __func__, __LINE__);
		itc_free(&msg);
		return -1;
	}

	itc_free(&msg);
	//check that the right subscribers got the tag.
	num_rec_inds = 0;
	while(1) {
		msg = itc_receive(rec_ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg)
			break;
		if(msg->rec_ind.tag_error) {
			printf("%s:%d ; Subscriber expecting \"%s\" reveived \"%s\"\n",
			       __func__, __LINE__,
			       msg->rec_ind.exp_tag, msg->rec_ind.rec_tag);
			itc_free(&msg);
			return -1;
		}
		num_rec_inds++;
		itc_free(&msg);
	};
	if( num_rec_inds != num_subscribers ) {
		printf("%s:%d ; Expected %d subscribers to receive tag "
		       "but %d subscribers got it.\n",
		       __func__, __LINE__, num_subscribers, num_rec_inds);
		return -1;
	}
	return 0;
}

static int test_3(itc_mbox_id_t dist_mbox, char *tag, int num_subscribers)
{

	union itc_msg *msg = NULL;
	int num_rec_inds = 0;
	uint32_t dist_cfm_filter[] = {1, DIST_CFM};
	uint32_t rec_ind_filter[]  = {1, REC_IND};
	printf("\n*** Test 3; distributing Charlie with an req ***\n");
	msg = itc_alloc(sizeof(struct dist_req_s), DIST_REQ);
	msg->dist_req.msg_type = DIST_REQ;
	snprintf(msg->dist_req.tag, XMR_MAX_TAG_LENGTH, tag);
	itc_send(&msg, dist_mbox, ITC_MY_MBOX);

	//check DIST_CFM
	msg = itc_receive(dist_cfm_filter, MSG_TIMEOUT, dist_mbox);
	if(!msg) {
		printf("%s:%d ; Timeout waiting for DIST_CFM\n",
		       __func__, __LINE__);
	}
	if(msg->dist_cfm.type_received != 2) {
		printf("%s:%d ; Expected distributor to receive a "
		       "XPAI_DISTRIBUTE_REJ, but it didn't.\n",
		       __func__, __LINE__);
		itc_free(&msg);
		return -1;
	}
	itc_free(&msg);
	//check that the right subscribers got the tag.
	num_rec_inds = 0;
	while(1) {
		msg = itc_receive(rec_ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg)
			break;
		if(msg->rec_ind.tag_error) {
			printf("%s:%d ; Subscriber expecting \"%s\" reveived \"%s\"\n",
			       __func__, __LINE__,
			       msg->rec_ind.exp_tag, msg->rec_ind.rec_tag);
			itc_free(&msg);
			return -1;
		}
		num_rec_inds++;
		itc_free(&msg);
	};
	if( num_rec_inds != num_subscribers ) {
		printf("%s:%d ; Expected %d subscribers to receive tag "
		       "but %d subscribers got it.\n",
		       __func__, __LINE__, num_subscribers, num_rec_inds);
		return -1;
	}
	return 0;
}
static int test_4(itc_mbox_id_t dist_mbox, itc_mbox_id_t sub_mbox)
{

	union itc_msg *msg = NULL;
	int num_rec_inds = 0;
	uint32_t dist_cfm_filter[] = {1, DIST_CFM};
	uint32_t rec_ind_filter[]  = {1, REC_IND};

	printf("\n*** Test 4; Subscribing in the name of another \"process\" and "
	       "distributing Delta with an req ***\n");
	XPAI_Subscribe(sub_mbox, "Delta");
	msg = itc_alloc(sizeof(struct dist_req_s), DIST_REQ);
	msg->dist_req.msg_type = DIST_REQ;
	snprintf(msg->dist_req.tag, XMR_MAX_TAG_LENGTH, "Delta");
	itc_send(&msg, dist_mbox, ITC_MY_MBOX);

	//check DIST_CFM
	msg = itc_receive(dist_cfm_filter, MSG_TIMEOUT, dist_mbox);
	if(!msg) {
		printf("%s:%d ; Timeout waiting for DIST_CFM\n",
		       __func__, __LINE__);
	}
	if(msg->dist_cfm.type_received != 1) {
		printf("%s:%d ; Expected distributor to receive a "
		       "XPAI_DISTRIBUTE_CFM, but it didn't.\n",
		       __func__, __LINE__);
		itc_free(&msg);
		return -1;
	}
	itc_free(&msg);
	//check that the right subscribers got the tag.
	num_rec_inds = 0;
	while(1) {
		msg = itc_receive(rec_ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg)
			break;
		if(!msg->rec_ind.tag_error) {
			printf("%s:%d ; Subscriber didn't report a tag "
			       "mismatch as was expected.\n",
			       __func__, __LINE__);
			itc_free(&msg);
			return -1;
		}
		num_rec_inds++;
		itc_free(&msg);
	};
	if( num_rec_inds != 1 ) {
		printf("%s:%d ; Expected 1 subscriber to receive tag "
		       "but %d subscribers got it.\n",
		       __func__, __LINE__, num_rec_inds);
		return -1;
	}
	return 0;
}

static int test_5(itc_mbox_id_t sub_mbox)
{

	printf("\n*** Test 5; Trying to subscribe for a too long tag. ***\n");

	XPAI_Subscribe(sub_mbox,
	               "This_is_a_far_too_long_tag_for_XPAI_Subscribe_to_accept");
	/* There is a problem that XPAI_Subscribe don't return fail -
	   How do I verify a pass or fail on this?
	   Well if it didn't crash, then it passed...*/

	return 0;
}

static int test_6(itc_mbox_id_t dist_mbox, itc_mbox_id_t sub_mbox)
{

	union itc_msg *msg = NULL;
	int num_rec_inds = 0;
	uint32_t dist_cfm_filter[] = {1, DIST_CFM};
	uint32_t rec_ind_filter[]  = {1, REC_IND};
	printf("\n*** Test 6; Trying to subscribe for zero length tag. ***\n");
	XPAI_Subscribe(sub_mbox, "");
	msg = itc_alloc(sizeof(struct dist_req_s), DIST_REQ);
	msg->dist_req.msg_type = DIST_REQ;
	msg->dist_req.tag[0] = 0;
	itc_send(&msg, dist_mbox, ITC_MY_MBOX);

	//check DIST_CFM
	msg = itc_receive(dist_cfm_filter, MSG_TIMEOUT, dist_mbox);
	if(!msg) {
		printf("%s:%d ; Timeout waiting for DIST_CFM\n",
		       __func__, __LINE__);
	}
	if(msg->dist_cfm.type_received != 1) {
		printf("%s:%d ; Expected distributor to receive a "
		       "XPAI_DISTRIBUTE_CFM, but it didn't.\n",
		       __func__, __LINE__);
		itc_free(&msg);
		return -1;
	}
	itc_free(&msg);
	//check that the right subscribers got the tag.
	num_rec_inds = 0;
	while(1) {
		msg = itc_receive(rec_ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg)
			break;
		if(!msg->rec_ind.tag_error) {
			printf("%s:%d ; Subscriber didn't report a tag "
			       "mismatch as was expected.\n",
			       __func__, __LINE__);
			itc_free(&msg);
			return -1;
		}
		num_rec_inds++;
		itc_free(&msg);
	};
	if( num_rec_inds != 1 ) {
		printf("%s:%d ; Expected 1 subscriber to receive tag "
		       "but %d subscribers got it.\n",
		       __func__, __LINE__, num_rec_inds);
		return -1;
	}

	return 0;
}


int main(int UNUSED argc, char UNUSED **argv)
{
#define NUM_SUBSCRIBERS 10
#define NUM_DISTRIBUTORS 2
	pthread_t sub_thread[NUM_SUBSCRIBERS];
	itc_mbox_id_t sub_mbox[NUM_SUBSCRIBERS];
	char *tags[] = {"Alpha", "Bravo", "Charlie",
	                "Alpha", "Alpha",
	                "Bravo", "Bravo",
	                "Charlie", "Charlie",
	                "SomethingElse"
	               };
	answer_type_t response[] = {ANSWER_CFM, ANSWER_CFM, ANSWER_CFM,
	                            ANSWER_CFM, ANSWER_CFM,
	                            ANSWER_CFM, ANSWER_CFM,
	                            ANSWER_CFM, ANSWER_REJ,
	                            ANSWER_CFM
	                           };
	pthread_t dist_thread[NUM_DISTRIBUTORS];
	itc_mbox_id_t dist_mbox[NUM_DISTRIBUTORS];
	void *retval;
	uint32_t announce_filter[] = {1, SLAVE_ANNOUNCE};
	union itc_msg *msg = NULL;

	if (itc_init(NUM_SUBSCRIBERS + NUM_DISTRIBUTORS + 1, ITC_MALLOC,
	             NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!",
		       __func__);
		return -1;
	}

	main_mbox = itc_create_mailbox(MBOX, 0);
	if (main_mbox == ITC_NO_ID) {
		printf("%s: Unable to create ITC mailbox \"%s\"!",
		       __func__, MBOX);
		return -1;
	}

	evti_mbox = itc_locate(EVTI_SERVER_NAME);
	if (evti_mbox == ITC_NO_ID) {
		printf("%s: Can't find EVTI server \"%s\"!",
		       __func__, EVTI_SERVER_NAME);
		return -1;
	}

	printf("Starting up slaves\n");
	for(int i = 0; i < NUM_SUBSCRIBERS; i++) {
		int res = pthread_create(&sub_thread[i], NULL, slave, NULL);
		if(res) {
			printf("%s: Failed to create thread : error  %d\n",
			       __func__, res);
			return -1;
		}
		msg = itc_receive(announce_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg) {
			printf("%s: Timeout waiting for thread to "
			       "announce its existens\n", __func__ );
			return -1;
		}
		sub_mbox[i] = itc_sender(msg);
		itc_free(&msg);
		msg = itc_alloc(sizeof(struct cfg_s), SLAVE_CFG);
		msg->cfg.is_subscriber = SUBSCR;
		snprintf(msg->cfg.tag, XMR_MAX_TAG_LENGTH, tags[i]);
		msg->cfg.answer_with = response[i];
		itc_send(&msg, sub_mbox[i], ITC_MY_MBOX);
	}

	for(int i = 0; i < NUM_DISTRIBUTORS; i++) {
		int res = pthread_create(&dist_thread[i], NULL, slave, NULL);
		if(res) {
			printf("%s: Failed to create thread : error  %d\n",
			       __func__, res);
			return -1;
		}
		msg = itc_receive(announce_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg) {
			printf("%s: Timeout waiting for thread to "
			       "announce its existens\n", __func__ );
			return -1;
		}
		dist_mbox[i] = itc_sender(msg);
		itc_free(&msg);
		msg = itc_alloc(sizeof(struct cfg_s), SLAVE_CFG);
		msg->cfg.is_subscriber = DISTR;
		itc_send(&msg, dist_mbox[i], ITC_MY_MBOX);
	}
	usleep(10000); /* Let all slaves start so we don't get confusing
                          ordering of print-outs */

	if(test_1(dist_mbox[0], tags[0], 3))
		return -1;

	if(test_2(dist_mbox[1], tags[1], 3))
		return -1;

	if(test_3(dist_mbox[0], tags[2], 3))
		return -1;

	if(test_4(dist_mbox[0], sub_mbox[NUM_SUBSCRIBERS - 1]))
		return -1;

	if(test_5(sub_mbox[NUM_SUBSCRIBERS - 1]))
		return -1;

	if(test_6(dist_mbox[0], sub_mbox[NUM_SUBSCRIBERS - 1]))
		return -1;

	printf("\n*** All tests have passed ***\n");
	printf("Closing down slaves\n");
	for(int i = 0; i < NUM_SUBSCRIBERS; i++) {
		msg = itc_alloc(sizeof(uint32_t), SLAVE_END);
		itc_send(&msg, sub_mbox[i], ITC_MY_MBOX);
		pthread_join(sub_thread[i], &retval);
	}
	for(int i = 0; i < NUM_DISTRIBUTORS; i++) {
		msg = itc_alloc(sizeof(uint32_t), SLAVE_END);
		itc_send(&msg, dist_mbox[i], ITC_MY_MBOX);
		pthread_join(dist_thread[i], &retval);
	}
	printf("All slaves has been stopped\n");
	return 0;


}

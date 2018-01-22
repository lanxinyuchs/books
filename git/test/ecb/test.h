/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */



#define TEST_DONE						(0xDEADBABE)

struct test_done {
	uint32_t              sigNo;
	uint32_t              ecode;
};



struct thread_data {
	uint32_t		address;
	uint32_t		count;
	uint32_t		size;
	int				result;
	volatile int	run;
	char			*pp_type;

};


extern void *ping_thread(void *param);
extern void *pong_thread(void *param);
extern void *a4ci_thread(void *param);
extern void *a4ci_mcab_thread(void *param);
extern void *g2_a4ci_thread(void *param);

extern itc_mbox_id_t hunt_peer(char *peer_name);



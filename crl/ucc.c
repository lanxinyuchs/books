
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>

#include <itc.h>
#include <math.h>

#include "ucc_sim.h"

#define UCC_SIM_MIN_TMO_SEC 1
#define UCC_SIM_MAX_TMO_SEC 300

#define A4CID_HDLC_ADDR_MIN    1
#define A4CID_HDLC_ADDR_MAX  254

static void print_usage()
{
	printf("Usage:\n"
	       "ucc -set <instance> <behavior>\n"
	       "     behavior bit 1 -- UART overrun\n"
	       "     behavior bit 2 -- CRC error\n"
	       "     behavior bit 3 -- CTRL error\n"
	       "     behavior bit 4 -- timeout error\n"
	       "     behavior bit 5 -- address error\n"
	       "ucc -rec <instance> <address> <timeout>\n"
	       "ucc -send <instance> <address> <length> <1:st byte> "
	                 "<2:nd byte>...\n");
}

static void terminate(int return_code)
{
	exit(return_code);
}

static void init(char *name)
{
	setbuf(stdout, 0);
	itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if (itc_create_mailbox(name, 0) == ITC_NO_ID) {
		printf("Could not create mailbox \"%s\"\n", name);
		terminate(EXIT_FAILURE);
	}
}

static itc_mbox_id_t hunt(char *name)
{
	uint32_t       sel[] = { 1, ITC_LOCATE_DEFAULT_NO };
	itc_mbox_id_t  pid;
	union itc_msg *sig;

	itc_locate_async(name, NULL, ITC_MY_MBOX);

	sig = itc_receive(sel, 5000, ITC_FROM_ALL);
	if (sig) {
		pid = itc_sender(sig);
		itc_free(&sig);
	} else {
		pid = 0;
	}

	return pid;
}

static itc_mbox_id_t hunt_simulator(char *name, const char *instance)
{
	(void) sprintf(name, "UCC_Simulator_%s", instance);
	return hunt(name);
}

static int ucc_set_behavior(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t Mid;
	char          name[64];
	int           status;

	if (argc < 4) {
		printf("Too few parameters\n");
		print_usage();
		return 1;
	}

	Mid = hunt_simulator(name, argv[2]);
	if (Mid == 0) {
		printf("\"%s\" does not exist\n", name);
		return 1;
	}

	sig = itc_alloc(sizeof(struct ucc_simSetBehaviorS), UCC_SIM_SET_BEHAVIOR);

	status = sscanf(argv[3], "%u", &sig->ucc_simSetBehavior.behaviorBits);
	if ((status != 1) || (status > (pow(2, UCC_COUNTERS) - 1))) {
		printf("Invalid value \"%s\" for behavior bits\n", argv[4]);
		itc_free(&sig);
		return 1;
	}

	itc_send(&sig, Mid, ITC_MY_MBOX);

	return 0;
}

static int ucc_rec_msg(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  Mid;
	int            status;
	uint32_t       tmp;
	char           name[64];

	if (argc < 5) {
		printf("Too few parameters\n");
		print_usage();
		return 1;
	}

	Mid = hunt_simulator(name, argv[2]);
	if (Mid == 0) {
		printf("\"%s\" does not exist\n", name);
		return 1;
	}

	sig = itc_alloc(sizeof(struct ucc_simRecMsgS), UCC_SIM_REC_MSG);

	/* read and check HDLC address */
	status = sscanf(argv[3], "0x%x", &tmp);
	if (status != 1) {
		status = sscanf(argv[3], "%u", &tmp);
		if (status != 1) {
			printf("Invalid value \"%s\" for HDLC address\n", argv[3]);
			itc_free(&sig);
			return 1;
		}
	}
	sig->ucc_simRecMsg.hdlcAddr = (uint8_t) tmp;
	if (sig->ucc_simRecMsg.hdlcAddr < A4CID_HDLC_ADDR_MIN ||
	    sig->ucc_simRecMsg.hdlcAddr > A4CID_HDLC_ADDR_MAX) {
		printf("Invalid value \"%s\" for HDLC address, min = %d, max=%d \n",
		       argv[3], A4CID_HDLC_ADDR_MIN, A4CID_HDLC_ADDR_MAX);
		itc_free(&sig);
		return 1;
	}

	memset(sig->ucc_simRecMsg.data, 0, sizeof(sig->ucc_simRecMsg.data));
	sig->ucc_simRecMsg.length = A4CI_MAX_DATA_SIZE;

	/* read and check timeout value */
	status = sscanf(argv[4], "%u", &tmp);
	if (status != 1) {
		printf("Invalid value \"%s\" for timeout\n", argv[4]);
		itc_free(&sig);
		return 1;
	}
	sig->ucc_simRecMsg.tmo_sec = (time_t) tmp;

	if (sig->ucc_simRecMsg.tmo_sec < UCC_SIM_MIN_TMO_SEC ||
	    sig->ucc_simRecMsg.tmo_sec > UCC_SIM_MAX_TMO_SEC) {
		printf("Invalid value \"%s\" for timeout, min = %ds, max=%ds \n",
		       argv[4], UCC_SIM_MIN_TMO_SEC, UCC_SIM_MAX_TMO_SEC);
		itc_free(&sig);
		return 1;
	}

	itc_send(&sig, Mid, ITC_MY_MBOX);

	return 0;
}

static int ucc_reply_msg(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  Mid;
	int            status;
	uint32_t       tmp;
	char           name[64];

	if (argc < 5) {
		printf("Too few parameters\n");
		print_usage();
		return 1;
	}

	Mid = hunt_simulator(name, argv[2]);
	if (Mid == 0) {
		printf("\"%s\" does not exist\n", name);
		return 1;
	}

	sig = itc_alloc(sizeof(struct ucc_simReplyMsgS), UCC_SIM_REPLY_MSG);

	/* read and check HDLC address */
	status = sscanf(argv[3], "0x%x", &tmp);
	if (status != 1) {
		status = sscanf(argv[3], "%u", &tmp);
		if (status != 1) {
			printf("Invalid value \"%s\" for HDLC address\n", argv[3]);
			itc_free(&sig);
			return 1;
		}
	}
	sig->ucc_simReplyMsg.hdlcAddr = (uint8_t) tmp;
	if (sig->ucc_simReplyMsg.hdlcAddr < A4CID_HDLC_ADDR_MIN ||
	    sig->ucc_simReplyMsg.hdlcAddr > A4CID_HDLC_ADDR_MAX) {
		printf("Invalid value \"%s\" for HDLC address, min = %d, max=%d \n",
		       argv[3], A4CID_HDLC_ADDR_MIN, A4CID_HDLC_ADDR_MAX);
		itc_free(&sig);
		return 1;
	}

	memset(sig->ucc_simReplyMsg.data, 0, sizeof(sig->ucc_simReplyMsg.data));
	sig->ucc_simReplyMsg.length = A4CI_MAX_DATA_SIZE;

	/* read and check timeout value */
	status = sscanf(argv[4], "%u", &tmp);
	if (status != 1) {
		printf("Invalid value \"%s\" for timeout\n", argv[4]);
		itc_free(&sig);
		return 1;
	}
	sig->ucc_simReplyMsg.tmo_sec = (time_t) tmp;

	if (sig->ucc_simReplyMsg.tmo_sec < UCC_SIM_MIN_TMO_SEC ||
	    sig->ucc_simReplyMsg.tmo_sec > UCC_SIM_MAX_TMO_SEC) {
		printf("Invalid value \"%s\" for timeout, min = %ds, max=%ds \n",
		       argv[4], UCC_SIM_MIN_TMO_SEC, UCC_SIM_MAX_TMO_SEC);
		itc_free(&sig);
		return 1;
	}

	itc_send(&sig, Mid, ITC_MY_MBOX);

	return 0;
}

static int ucc_send_msg(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  Mid;
	int            status;
	uint32_t       tmp;
	uint16_t       idx;
	char           name[64];

	if (argc < 6) {
		printf("Too few parameters\n");
		print_usage();
		return 1;
	}

	Mid = hunt_simulator(name, argv[2]);
	if (Mid == 0) {
		printf("\"%s\" does not exist\n", name);
		return 1;
	}

	sig = itc_alloc(sizeof(struct ucc_simSendMsgS), UCC_SIM_SEND_MSG);

	/* read and check HDLC address */
	status = sscanf(argv[3], "0x%x", &tmp);
	if (status != 1) {
		status = sscanf(argv[3], "%u", &tmp);
		if (status != 1) {
			printf("Invalid value \"%s\" for HDLC address\n", argv[3]);
			itc_free(&sig);
			return 1;
		}
	}
	sig->ucc_simSendMsg.hdlcAddr = (uint8_t) tmp;
	if (sig->ucc_simSendMsg.hdlcAddr < A4CID_HDLC_ADDR_MIN ||
	    sig->ucc_simSendMsg.hdlcAddr > A4CID_HDLC_ADDR_MAX) {
		printf("Invalid value \"%s\" for HDLC address, min = %d, max=%d \n",
		       argv[3], A4CID_HDLC_ADDR_MIN, A4CID_HDLC_ADDR_MAX);
		itc_free(&sig);
		return 1;
	}

	/* read and check length */
	status = sscanf(argv[4], "%hu", &sig->ucc_simSendMsg.length);
	if (status != 1) {
		printf("Invalid value \"%s\" for length\n", argv[4]);
		itc_free(&sig);
		return 1;
	}
	if (sig->ucc_simSendMsg.length == 0 ||
	    sig->ucc_simSendMsg.length > A4CI_MAX_DATA_SIZE) {
		printf("Invalid value \"%s\" for length, min = 1, max=%d \n",
		       argv[4], A4CI_MAX_DATA_SIZE);
		itc_free(&sig);
		return 1;
	}

	/* read data */
	if (argc < (5 + sig->ucc_simSendMsg.length)) {
		printf("Too few data parameters\n");
		itc_free(&sig);
		return 1;
	}
	for (idx = 0; idx < sig->ucc_simSendMsg.length; idx++) {
		status = sscanf(argv[5 + idx], "0x%x", &tmp);
		if (status != 1) {
			status = sscanf(argv[5 + idx], "%u", &tmp);
			if (status != 1) {
				printf("Invalid value \"%s\" for data\n", argv[5 + idx]);
				itc_free(&sig);
				return 1;
			}
		}
		sig->ucc_simSendMsg.data[idx] = (uint8_t) tmp;
	}

	itc_send(&sig, Mid, ITC_MY_MBOX);

	return 0;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		goto ucc_cmd_fail;
	}

	init("UCC_Simulator_Command");

	if (strcmp(argv[1], "-set") == 0) {
		return ucc_set_behavior(argc, argv);
	} else if (strcmp(argv[1], "-rec") == 0) {
		return ucc_rec_msg(argc, argv);
	} else if (strcmp(argv[1], "-reply") == 0) {
		return ucc_reply_msg(argc, argv);
	} else if (strcmp(argv[1], "-send") == 0) {
		return ucc_send_msg(argc, argv);
	}

ucc_cmd_fail:
	print_usage();
	return 1;

}

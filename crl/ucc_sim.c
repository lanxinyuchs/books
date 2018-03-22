#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>

#include <signal.h>
#include <itc.h>

#include <ecb_dev.h>
#include <ecb_ucc.h>
#include "ecb_ucc_sim.h"

#include "ucc_sim.h"

static struct {
	char                 name[64];
	ucc_sim_status       behavior;
	void                *handle;
} ucc_sim;

static void terminate(int return_code)
{
	if (ucc_sim.handle != NULL) {
		ecb_dev_shutdown(ucc_sim.handle);
	}
	exit(return_code);
}

static void sigexit_handler(int signo)
{
	printf("\"%s\" successfully terminated with signo %d\n",
	       ucc_sim.name, signo);
	terminate(EXIT_SUCCESS);
}

static void init_signal_handler (void)
{
	struct sigaction act;

	/* Set up a signal handlers */
	act.sa_handler = sigexit_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	sigaction(SIGINT, &act, NULL);
	sigaction(SIGTERM, &act, NULL);
	sigaction(SIGKILL, &act, NULL);

}

static int ucc_start(char *instance)
{
	itc_mbox_id_t client_mbox;
	char          tty_name[16] = "/dev/ttyECB0";

	/* Reset the UCC Simulator state. */
	memset(&ucc_sim, 0, sizeof(ucc_sim));
	setbuf(stdout, 0);
	init_signal_handler();
	itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	snprintf(ucc_sim.name, sizeof(ucc_sim.name), "UCC_Simulator_%s", instance);

	client_mbox = itc_create_mailbox(ucc_sim.name, 0);
	if (client_mbox == ITC_NO_ID) {
		printf("Could not create client mailbox \"%s\"\n", ucc_sim.name);
		terminate(EXIT_FAILURE);
	}

	/* Initiate ECB for ucc_sim. */
	if (ecb_dev_init(&ucc_sim.handle, tty_name) != 0) {
		printf("Failed to initate device \"%s\"", tty_name);
		terminate(EXIT_FAILURE);
	}

	printf("\"%s\" (0x%x) successfully started\n",
	       ucc_sim.name, client_mbox);
	printf("Press Control-C to exit\n"
	       "If running in background use: pkill -n ucc_sim\n");

	return 0;
}

void hexdump(char *prefix, uint8_t *data, uint32_t size)
{
	uint32_t idx0, idx1;

	printf(prefix);
	for (idx0 = 0; idx0 <= size; idx0 += 16) {
		for (idx1 = idx0; idx1 < (idx0 + 16) && idx1 < size; idx1++) {
			printf("%02x ", data[idx1]);
		}
		putchar('\n');
	}
}

static void ucc_handle_message(void)
{
	union itc_msg        *msg;
	uint8_t              data[A4CI_MAX_DATA_SIZE*UART_FRAME_MUL];
	uint32_t             length;
	uint32_t             cmd_info_size;

	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

	switch (msg->msgno) {
	case UCC_SIM_SET_BEHAVIOR:
		ucc_sim.behavior.status = msg->ucc_simSetBehavior.behaviorBits;
		printf("Behavior of %s set to\n"
		       "UART overrun %u\n"
		       "CRC errror %u\n"
		       "CTRL errror %u\n"
		       "timeout errror %u\n"
		       "address errror %u\n",
		       ucc_sim.name, ucc_sim.behavior.bit.uart,
		       ucc_sim.behavior.bit.crc, ucc_sim.behavior.bit.ctrl,
		       ucc_sim.behavior.bit.timeout, ucc_sim.behavior.bit.address);
		break;

	case UCC_SIM_REC_MSG:
		printf("\"%s\" expecting msg(a:0x%x, t:%u)\n",
		       ucc_sim.name,
		       msg->ucc_simRecMsg.hdlcAddr,
		       (uint32_t) msg->ucc_simRecMsg.tmo_sec);
		cmd_info_size = (uint32_t) msg->ucc_simRecMsg.length;
		if (ecb_ucc_sim_receive(ucc_sim.handle,
		                        msg->ucc_simRecMsg.hdlcAddr,
		                        msg->ucc_simRecMsg.data,
		                        &cmd_info_size,
		                        msg->ucc_simRecMsg.tmo_sec) == 1) {
			msg->ucc_simRecMsg.length = (uint16_t) cmd_info_size;
			hexdump("msg.data:", msg->ucc_simRecMsg.data,
			        msg->ucc_simRecMsg.length);
		} else {
			printf("\"%s\" data receive failed", ucc_sim.name);
		}
		break;

	case UCC_SIM_REPLY_MSG:
		printf("\"%s\" expecting msg(a:0x%x, t:%u)\n",
		       ucc_sim.name,
		       msg->ucc_simReplyMsg.hdlcAddr,
		       (uint32_t) msg->ucc_simReplyMsg.tmo_sec);
		cmd_info_size = (uint32_t) msg->ucc_simReplyMsg.length;
		if (ecb_ucc_sim_receive(ucc_sim.handle,
		                        msg->ucc_simReplyMsg.hdlcAddr,
		                        msg->ucc_simReplyMsg.data,
		                        &cmd_info_size,
		                        msg->ucc_simReplyMsg.tmo_sec) == 1) {
			msg->ucc_simReplyMsg.length = (uint16_t) cmd_info_size;
			hexdump("msg.data:", msg->ucc_simReplyMsg.data,
			        msg->ucc_simReplyMsg.length);
			printf("Sent msg(a:0x%x, l:%u) from %s\n",
			       msg->ucc_simReplyMsg.hdlcAddr,
			       msg->ucc_simReplyMsg.length,
			       ucc_sim.name);
			if (ecb_ucc_sim_send(ucc_sim.handle,
			                     msg->ucc_simReplyMsg.hdlcAddr,
			                     msg->ucc_simReplyMsg.data,
			                     msg->ucc_simReplyMsg.length,
			                     ucc_sim.behavior) == -1) {
				printf("\"%s\" data send failed", ucc_sim.name);
			}
		} else {
			printf("\"%s\" data receive failed", ucc_sim.name);
		}
		break;

	case UCC_SIM_SEND_MSG:
		/* Copy data to internal array */
		memcpy(data, msg->ucc_simSendMsg.data, msg->ucc_simSendMsg.length);
		if (ucc_sim.behavior.bit.uart == 0){
			length = (uint32_t) msg->ucc_simSendMsg.length;
		} else {
			/* Send packet of double size to provoke uart*/
			memset(data + msg->ucc_simSendMsg.length, 1,
			       sizeof(data)-msg->ucc_simSendMsg.length);
			length = sizeof(data);
		}
		printf("Sent msg(a:0x%x, l:%u) from %s\n",
		       msg->ucc_simSendMsg.hdlcAddr,
		       length,
		       ucc_sim.name);
		hexdump("msg.data:", data, length);
		if (ecb_ucc_sim_send(ucc_sim.handle,
		                     msg->ucc_simSendMsg.hdlcAddr,
		                     data,
		                     length,
		                     ucc_sim.behavior) == -1) {
			printf("\"%s\" data send failed", ucc_sim.name);
		}
		break;

	default:
		printf("Received unexpected signal 0x%x from 0x%x",
		        msg->msgno, itc_sender(msg));
		break;
	}
	itc_free(&msg);
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		printf("Syntax: ./ucc_sim <instance>\n");
		return EXIT_FAILURE;
	}

	ucc_start(argv[1]);

	for (;;) {
		ucc_handle_message();
	}
}

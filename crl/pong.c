
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include <itc.h>
#include <itc_system.h>


static const char *help = {
	"Start server process: pong -s <name>\n"
	"Start client process: pong -c <name> <path> <min=4> <max=4> "
	"<burst=1> <signo_tx=0> <signo_rx=signo_tx>\n"
	"Examples:\n"
	"pong -s clayman\n"
	"pong -c aluvian port_0_dev_1/clayman\n"
	"pong -c halaster clayman\n"
};

static struct timespec t0, t1;

union itc_msg {
	uint32_t sigNo;
};

static void send_pong(char *path, itc_mbox_id_t pid,
                      uint32_t size, uint32_t burst, uint32_t signo_tx)
{
	uint32_t       idx0, idx1;
	union itc_msg *sig;

	printf("Sending %u bytes %u times to \"%s\"\n", size, burst, path);

	clock_gettime(CLOCK_MONOTONIC, &t0);

	for (idx0 = 1; idx0 <= burst; idx0++) {
		sig = itc_alloc(size, signo_tx);
		for (idx1 = 4; idx1 < size; idx1++) {
			((uint8_t*) sig)[idx1] = (uint8_t) (idx1 & 0xff);
		}
		itc_send(&sig, pid, ITC_MY_MBOX);
	}
}

static int recv_pong(char *path, itc_mbox_id_t pid,
                     uint32_t size, uint32_t burst, uint32_t signo_rx)
{
	uint32_t         sel_mon[] = { 2, signo_rx, ITC_MONITOR_DEFAULT_NO };
	uint32_t         idx0, idx1;
	union itc_msg   *sig;
	struct timespec  t;

	for (idx0 = 1; idx0 <= burst; idx0++) {

		sig = itc_receive(sel_mon, ITC_NO_TMO, pid);

		if (sig->sigNo == ITC_MONITOR_DEFAULT_NO) {
			printf("Server \"%s\" was killed\n", path);
			return 1;
		} else if (itc_size(sig) != size) {
			printf("Received bad signal size: %d  "
			       "Correct signal size: %d\n",
			       size, itc_size(sig));
			return -1;
		} else {
			for (idx1 = 4; idx1 < size; idx1++) {
				if (((uint8_t*) sig)[idx1] !=
				    (uint8_t) (idx1 & 0xff)) {
					printf("Received bad data in "
					       "signal of size: %d", size);
					return -1;
				}
			}
			itc_free(&sig);
		}
	}

	clock_gettime(CLOCK_MONOTONIC, &t1);

	if (t1.tv_nsec < t0.tv_nsec) {
		t.tv_sec = t1.tv_sec - t0.tv_sec - 1;
		t.tv_nsec = 1000000000 + t1.tv_nsec - t0.tv_nsec;
	} else {
		t.tv_sec = t1.tv_sec - t0.tv_sec;
		t.tv_nsec = t1.tv_nsec - t0.tv_nsec;
	}

	printf("Received %u bytes %u times from \"%s\" (~RTD: %ld.%09ld)\n",
	       size, burst, path, t.tv_sec, t.tv_nsec);

	return 0;
}


static void init(char *name)
{
	setbuf(stdout, 0);
	itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if (itc_create_mailbox(name, 0) == ITC_NO_ID) {
		printf("Could not create mailbox \"%s\"\n", name);
		exit(-1);
	}
}

static int server(void)
{
	uint32_t       all[] = { 0 };
	union itc_msg *sig;

	for (;;) {
		sig = itc_receive(all, ITC_NO_TMO, ITC_FROM_ALL);
		itc_send(&sig, itc_sender(sig), ITC_MY_MBOX);
	}

	return 0;
}

static int client(char *path, char *min_str, char *max_str, char *burst_str,
                  char *signo_tx_str, char *signo_rx_str)
{
	uint32_t       sel_loc[] = { 1, ITC_LOCATE_DEFAULT_NO };
	uint32_t       min, max, burst, signo_tx, signo_rx, idx;
	union itc_msg *sig;
	itc_mbox_id_t  pid;
	int            ret;

	if (sscanf(min_str, "0x%x", &min) != 1) {
		if (sscanf(min_str, "%u", &min) != 1) {
			printf("Invalid minimum signal size \"%s\"\n",
			       min_str);
			return -1;
		}
	}
	if (min < 4) {
		printf("Invalid minimum signal size \"%s\"\n", min_str);
		return -1;
	}

	if (sscanf(max_str, "0x%x", &max) != 1) {
		if (sscanf(max_str, "%u", &max) != 1) {
			printf("Invalid maximum signal size \"%s\"\n",
			       max_str);
			return -1;
		}
	}
	if (max < 4) {
		printf("Invalid maximum signal size \"%s\"\n", max_str);
		return -1;
	}

	if (sscanf(burst_str, "0x%x", &burst) != 1) {
		if (sscanf(burst_str, "%u", &burst) != 1) {
			printf("Invalid burst size \"%s\"\n",
			       burst_str);
			return -1;
		}
	}
	if (burst == 0 || burst > 1000) {
		printf("Invalid burst size \"%s\"\n", burst_str);
		return -1;
	}

	if (sscanf(signo_tx_str, "0x%x", &signo_tx) != 1) {
		if (sscanf(signo_tx_str, "%u", &signo_tx) != 1) {
			printf("Invalid signal number (tx) \"%s\"\n",
			       signo_tx_str);
			return -1;
		}
	}

	if (sscanf(signo_rx_str, "0x%x", &signo_rx) != 1) {
		if (sscanf(signo_rx_str, "%u", &signo_rx) != 1) {
			printf("Invalid signal number (rx) \"%s\"\n",
			       signo_rx_str);
			return -1;
		}
	}

	if (max < min) {
		idx = max;
		max = min;
		min = idx;
	}

	printf("Minimum signal size...: %u\n"
	       "Maximum signal size...: %u\n"
	       "Signals per burst.....: %u\n"
	       "Signal number (tx/rx).: 0x%08x/0x%08x\n",
	       min, max, burst, signo_tx, signo_rx);

lost_server:
	printf("Locating \"%s\"...\n", path);
	itc_locate_async(path, NULL, ITC_MY_MBOX);
	sig = itc_receive(sel_loc, ITC_NO_TMO, ITC_FROM_ALL);
	pid = itc_sender(sig);
	itc_free(&sig);
	printf("Located \"%s\"\n", path);

	(void) itc_monitor(pid, NULL);

	for (;;) {
		for (idx = min; idx <= max; idx++) {
			send_pong(path, pid, idx, burst, signo_tx);
			ret = recv_pong(path, pid, idx, burst, signo_rx);
			if (ret < 0) {
				return ret;
			} else if (ret > 0) {
				goto lost_server;
			}
		}
	}

	return 0;
}

int main(int argc, char **argv)
{
	int ret;

	if (argc == 3 && strcmp(argv[1], "-s") == 0) {

		init(argv[2]);
		ret = server();

	} else if ((argc >= 4 && argc <= 9) && strcmp(argv[1], "-c") == 0) {

		init(argv[2]);
		if (argc == 4) {
			ret = client(argv[3],     "4",     "4",     "1",
			             "0",         "0");
		} else if (argc == 5) {
			ret = client(argv[3], argv[4],     "4",     "1",
			             "0",         "0");
		} else if (argc == 6) {
			ret = client(argv[3], argv[4], argv[5],     "1",
			             "0",         "0");
		} else if (argc == 7) {
			ret = client(argv[3], argv[4], argv[5], argv[6],
			             "0",         "0");
		} else if (argc == 8) {
			ret = client(argv[3], argv[4], argv[5], argv[6],
			             argv[7], argv[7]);
		} else {
			ret = client(argv[3], argv[4], argv[5], argv[6],
			             argv[7], argv[8]);
		}

	} else {
		printf(help);
		ret = -1;
	}

	return ret;
}

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include <itc.h>

#include "eqmhi_api.h"
#include "eqmhi_ru.h"


#define TEST_FAILURE   -1
#define TEST_SUCCESS    0

#define MAILBOX_SIZE    2
#define MAILBOX_NAME   "cfg_dp_links"

#define EXPECT(expr, res) do {                                                \
	if ((expr) != (res)) {                                                    \
		result = TEST_FAILURE;                                                \
		fprintf(stderr, "Failed [ " #expr " ] at %s:%d: expected %d\n",       \
		        __FILE__, __LINE__, res);                                     \
		goto exit;                                                            \
	}                                                                         \
} while (0)


#define EQM_ID_1        1
#define EQM_ID_2        2
#define EQM_ID_3        3
#define EQM_ID_4        4
#define EQM_ID_5        5
#define EQM_ID_6        6
#define EQM_ID_7        7
#define EQM_ID_8        8

#define TEST_LMC   "/root/dp_tlnh_shmem_cm_test.lmc"
#define TEST_LM_ID "DP"

#define N_ENTITIES_MIN  1
#define N_ENTITIES_MAX  8


static const uint32_t eqm_id[N_ENTITIES_MAX] = {
	EQM_ID_1,
	EQM_ID_2,
	EQM_ID_3,
	EQM_ID_4,
	EQM_ID_5,
	EQM_ID_6,
	EQM_ID_7,
	EQM_ID_8,
};

static struct eqmhi_load_entity load_entities[N_ENTITIES_MAX] = {
	{ .eqm_id = EQM_ID_1,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
	{ .eqm_id = EQM_ID_2,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
	{ .eqm_id = EQM_ID_3,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
	{ .eqm_id = EQM_ID_4,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
	{ .eqm_id = EQM_ID_5,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
	{ .eqm_id = EQM_ID_6,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
	{ .eqm_id = EQM_ID_7,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
	{ .eqm_id = EQM_ID_8,
	  .load.lmc = { EQMHI_LOAD_TYPE_LMC, TEST_LMC, TEST_LM_ID } },
};


#define N_CFGS(n_entities) ((n_entities) * 7)

static const struct eqmhi_cfg_entity cfg_entities[N_CFGS(N_ENTITIES_MAX)] = {

	{
		.eqm_id = EQM_ID_1,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir1"
		}
	}, {
		.eqm_id = EQM_ID_1,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_1,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_1,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 0
		}
	}, {
		.eqm_id = EQM_ID_1,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 8
		}
	}, {
		.eqm_id = EQM_ID_1,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x86000000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_1,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x86001000,
			.size = 4096
		}
	},

	{
		.eqm_id = EQM_ID_2,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir2"
		}
	}, {
		.eqm_id = EQM_ID_2,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_2,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_2,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 1
		}
	}, {
		.eqm_id = EQM_ID_2,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 9
		}
	}, {
		.eqm_id = EQM_ID_2,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x86002000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_2,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x86003000,
			.size = 4096
		}
	},

	{
		.eqm_id = EQM_ID_3,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir3"
		}
	}, {
		.eqm_id = EQM_ID_3,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_3,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_3,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 2
		}
	}, {
		.eqm_id = EQM_ID_3,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 10
		}
	}, {
		.eqm_id = EQM_ID_3,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x86004000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_3,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x86005000,
			.size = 4096
		}
	},

	{
		.eqm_id = EQM_ID_4,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir4"
		}
	}, {
		.eqm_id = EQM_ID_4,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_4,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_4,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 3
		}
	}, {
		.eqm_id = EQM_ID_4,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 11
		}
	}, {
		.eqm_id = EQM_ID_4,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x86006000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_4,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x86007000,
			.size = 4096
		}
	},

	{
		.eqm_id = EQM_ID_5,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir5"
		}
	}, {
		.eqm_id = EQM_ID_5,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_5,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_5,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 4
		}
	}, {
		.eqm_id = EQM_ID_5,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 12
		}
	}, {
		.eqm_id = EQM_ID_5,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x86008000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_5,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x86009000,
			.size = 4096
		}
	},

	{
		.eqm_id = EQM_ID_6,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir6"
		}
	}, {
		.eqm_id = EQM_ID_6,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_6,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_6,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 5
		}
	}, {
		.eqm_id = EQM_ID_6,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 13
		}
	}, {
		.eqm_id = EQM_ID_6,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x8600A000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_6,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x8600B000,
			.size = 4096
		}
	},

	{
		.eqm_id = EQM_ID_7,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir7"
		}
	}, {
		.eqm_id = EQM_ID_7,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_7,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_7,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 6
		}
	}, {
		.eqm_id = EQM_ID_7,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 14
		}
	}, {
		.eqm_id = EQM_ID_7,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x8600C000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_7,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x8600D000,
			.size = 4096
		}
	},

	{
		.eqm_id = EQM_ID_8,
		.cfg.dump_file = {
			.type = EQMHI_CFG_DUMP_FILE,
			.max_num_of_dumps = 1,
			.dump_location = "my_dump_dir8"
		}
	}, {
		.eqm_id = EQM_ID_8,
		.cfg.watchdog_interval = {
			.type = EQMHI_CFG_RU_WATCHDOG_INTERVAL,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_8,
		.cfg.watchdog_timeout = {
			.type = EQMHI_CFG_RU_WATCHDOG_TIMEOUT,
			.ms = 0
		}
	}, {
		.eqm_id = EQM_ID_8,
		.cfg.tx_mbox = {
			.type = EQMHI_CFG_RU_TX_MBOX,
			.id = 7
		}
	}, {
		.eqm_id = EQM_ID_8,
		.cfg.rx_mbox = {
			.type = EQMHI_CFG_RU_RX_MBOX,
			.id = 15
		}
	}, {
		.eqm_id = EQM_ID_8,
		.cfg.tx_shmem = {
			.type = EQMHI_CFG_RU_TX_SHMEM,
			.start = 0x8600E000,
			.size = 4096
		}
	}, {
		.eqm_id = EQM_ID_8,
		.cfg.rx_shmem = {
			.type = EQMHI_CFG_RU_RX_SHMEM,
			.start = 0x8600F000,
			.size = 4096
		}
	},
};

static eqmhi_status_t cb_status;


static void cb(eqmhi_instance_t instance, void *user_data, eqmhi_status_t status)
{
	printf("%s status %d\n", (char*) user_data, status);
	cb_status = status;
}

struct eqmhi_callback callbacks = {
	.load_callback = cb,
	.boot_callback = cb,
	.stop_callback = cb,
	.dump_callback = cb,
	.set_cfg_callback = cb,
	.get_cfg_callback = NULL
};


union itc_msg {
	uint32_t msgno;
};

static int setup_dp_links(uint32_t n_entities)
{
	eqmhi_instance_t  instance = 0;
	eqmhi_status_t    status;
	int               instance_fd;
	int               result = TEST_SUCCESS;
	int               idx;
	union itc_msg    *reply = NULL;
	uint32_t          sel[] = { 1, ITC_LOCATE_DEFAULT_NO };
	char             *msg_str;
	char              dp_mbox_name[ITC_NAME_MAXLEN];

	/* Create our EQMHI instance. */
	status = eqmhi_create(&instance, n_entities, eqm_id, &callbacks, &instance_fd);
	EXPECT(status, EQMHI_STATUS_SUCCESS);


	/* Load entities in our EQMHI instance. */
	msg_str = "Load";
	status = eqmhi_load(instance, msg_str, n_entities, load_entities);
	EXPECT(status, EQMHI_STATUS_SUCCESS);

	status = eqmhi_dispatch_callback(instance);
	EXPECT(status, EQMHI_STATUS_SUCCESS);
	EXPECT(cb_status, EQMHI_STATUS_SUCCESS);


	/* Set configuration for entities in out EQMHI instance. */
	msg_str = "Set configuration";
	status = eqmhi_set_cfg(instance, msg_str, N_CFGS(n_entities), cfg_entities);
	EXPECT(status, EQMHI_STATUS_SUCCESS);

	status = eqmhi_dispatch_callback(instance);
	EXPECT(status, EQMHI_STATUS_SUCCESS);
	EXPECT(cb_status, EQMHI_STATUS_SUCCESS);


	/* Boot entities in our EQMHI instance. */
	msg_str = "Boot";
	status = eqmhi_boot(instance, msg_str);
	EXPECT(status, EQMHI_STATUS_SUCCESS);

	status = eqmhi_dispatch_callback(instance);
	EXPECT(status, EQMHI_STATUS_SUCCESS);
	EXPECT(cb_status, EQMHI_STATUS_SUCCESS);


	/* Wait until all links are established. */
	for (idx = 0; idx < n_entities; idx++) {
		snprintf(dp_mbox_name, sizeof(dp_mbox_name),
		         "dp%d/dp", eqm_id[idx]);
		itc_locate_async(dp_mbox_name, NULL, ITC_MY_MBOX);
		reply = itc_receive(sel, ITC_NO_TMO, ITC_FROM_ALL);
		printf("Located \"%s\" as mailbox id 0x%08x\n",
		       dp_mbox_name, itc_sender(reply));
		itc_free(&reply);
	}

	printf("Links between CPU and device processors were established\n");
	printf("Please, press enter to stop the device processors\n");
	(void) getchar();


	msg_str = "Stop";
	status = eqmhi_stop(instance, msg_str);
	EXPECT(status, EQMHI_STATUS_SUCCESS);

	status = eqmhi_dispatch_callback(instance);
	EXPECT(status, EQMHI_STATUS_SUCCESS);
	EXPECT(cb_status, EQMHI_STATUS_SUCCESS);


	msg_str = "Dump";
	status = eqmhi_dump(instance, msg_str);
	EXPECT(status, EQMHI_STATUS_SUCCESS);

	status = eqmhi_dispatch_callback(instance);
	EXPECT(status, EQMHI_STATUS_SUCCESS);
	EXPECT(cb_status, EQMHI_STATUS_SUCCESS);


	status = eqmhi_destroy(instance);
	instance = 0;
	EXPECT(status, EQMHI_STATUS_SUCCESS);

exit:
	if (instance)
		(void) eqmhi_destroy(instance);

	if (reply)
		itc_free(&reply);

	return result;
}

int main(int argc, char *argv[])
{
	itc_mbox_id_t  mbox;
	uint32_t       idx, n_entities = 8;

	if (argc >= 2) {

		if (strcmp(argv[1], "-help") == 0 ||
		    strcmp(argv[1], "--h") == 0 ||
		    strcmp(argv[1], "-h") == 0) {
			fprintf(stderr,
			        "Load DP and configure DP link handlers:\n"
			        "%s <number of DPs> <LMC> <LM ID>\n",
			        argv[0]);
			return TEST_FAILURE;
		}

		n_entities = (uint32_t) strtoul(argv[1], NULL, 0);
		if (n_entities < N_ENTITIES_MIN ||
		    n_entities > N_ENTITIES_MAX) {
			fprintf(stderr,
			        "Invalid number of device processors (%s)\n",
			        argv[1]);
			return TEST_FAILURE;
		}
	}

	if (argc >= 3) {
		for (idx = 0; idx < n_entities; idx++) {
			snprintf(load_entities[idx].load.lmc.name,
			         sizeof(load_entities[idx].load.lmc.name),
			         argv[2]);
		}
	}

	if (argc >= 4) {
		for (idx = 0; idx < n_entities; idx++) {
			snprintf(load_entities[idx].load.lmc.lm_id,
			         sizeof(load_entities[idx].load.lmc.lm_id),
			         argv[3]);
		}
	}

	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		fprintf(stderr, "Failed to initialize itc\n");
		return TEST_FAILURE;
	}

	mbox = itc_create_mailbox(MAILBOX_NAME, 0);
	if (mbox == ITC_NO_ID) {
		fprintf(stderr, "Failed to create mailbox %s\n", MAILBOX_NAME);
		return TEST_FAILURE;
	}

	return setup_dp_links(n_entities);
}

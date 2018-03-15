#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>

#include <itc.h>
#include <uio_helper.h>
#include <ulh_lnh.h>
#include <ulh_lnh_msg.h>

#include "ulh-shmem-cm.h"
#include "eqm.h"
#include "eqmh_msg.h"
#include "dp-cfg.h"
#include "lmc.h"

#define TRACEPOINT_PROVIDER com_ericsson_eqmh_xenon_dp
#include <tpt_create.h>
#include <tpt.h>

#define RS(dp, no, s) TPT_REC_SIG(no, STR("dp %d:" s, (dp)->id))
#define SS(dp, no, pid, s) TPT_SEND_SIG(no, pid, STR("dp %d:" s, (dp)->id))
#define I(dp, fmt, ...) TPT_INFO(STR("dp %d:" fmt, (dp)->id, ##__VA_ARGS__))
#define E(dp, fmt, ...) TPT_ERROR(STR("dp %d:" fmt, (dp)->id, ##__VA_ARGS__))

#define MBOX_NAME_BASE        "EQM_XENON_DP_"
#define MBOX_MAX_NAME_SIZE    (sizeof(MBOX_NAME_BASE) + 10)

#define UIO_DEV_MISC          "MISC"
#define MISC_DP_CTRL_0        (384 * 4)
#define MISC_DP_STATUS_0      (385 * 4)
#define MISC_DP_CTRL_1        (386 * 4)
#define MISC_DP_STATUS_1      (387 * 4)
#define MISC_DP_CTRL_2        (388 * 4)
#define MISC_DP_STATUS_2      (389 * 4)
#define MISC_DP_CTRL_3        (390 * 4)
#define MISC_DP_STATUS_3      (391 * 4)
#define MISC_DP_CTRL_4        (392 * 4)
#define MISC_DP_STATUS_4      (393 * 4)
#define MISC_DP_CTRL_5        (394 * 4)
#define MISC_DP_STATUS_5      (395 * 4)
#define MISC_DP_CTRL_6        (396 * 4)
#define MISC_DP_STATUS_6      (397 * 4)
#define MISC_DP_CTRL_7        (398 * 4)
#define MISC_DP_STATUS_7      (399 * 4)

#define MAX_NUMBER_OF_LINKS   8
#define CM_NAME               "dp-shmem-cm"
#define ULNH_NAME             "shmem-lnh"

static UIO_HANDLE_ misc_uio_handle = (UIO_HANDLE_) -1;
static void *misc_base;
static itc_mbox_id_t link_mbox = ITC_NO_ID;
static void *lnh = NULL;

typedef enum {
	DP_STATE_STOPPED = 0,
	DP_STATE_LOADED,
	DP_STATE_RUNNING,
	DP_STATE_DUMPED
} dp_state_t;

union itc_msg {
	uint32_t msgno;
	struct ulh_lnhmsg_createcm_req     cmreq;
	struct ulh_lnhmsg_createcm_rsp     cmrsp;
	struct ulh_lnhmsg_createlink_req   lcreq;
	struct ulh_lnhmsg_createlink_rsp   lcrsp;
	struct ulh_lnhmsg_destroylink_req  ldreq;
	struct ulh_lnhmsg_destroylink_rsp  ldrsp;
	struct ulh_lnhmsg_notify           ulind;
	EQMH_MESSAGE_STRUCTS;
};

struct dp_params {
	uint32_t ctrl_misc_offset;
	uint32_t code_ram_start;
	uint32_t code_ram_size;
	uint32_t data_ram_start;
	uint32_t data_ram_size;
};

struct dp {
	uint32_t id;
	uint32_t lid;
	dp_state_t state;
	bool link_created;
	itc_mbox_id_t mbox;
	itc_mbox_id_t server_mbox;
	itc_mbox_id_t sender_mbox;
	union itc_msg save_msg;
	struct eqmhi_cfg_dump_file dump_file;
	bool dump_file_configured;
	bool disable_link;
	uint32_t watchdog_interval;
	uint32_t watchdog_timeout;
	uint32_t tx_mbox_id;
	uint32_t rx_mbox_id;
	struct ulh_shmem_cm_shmem_config tx_shmem;
	struct ulh_shmem_cm_shmem_config rx_shmem;
	uint32_t dp_size;
	uint32_t dp_offset;
	struct dp_params params;
};

static struct dp_params params[8] = {
	{ MISC_DP_CTRL_0, 0x80240000, 128 * 1024, 0x80260000, 64 * 1024 },
	{ MISC_DP_CTRL_1, 0x88240000, 128 * 1024, 0x88260000, 64 * 1024 },
	{ MISC_DP_CTRL_2, 0x82440000, 128 * 1024, 0x82460000, 64 * 1024 },
	{ MISC_DP_CTRL_3, 0x8a440000, 128 * 1024, 0x8a460000, 64 * 1024 },
	{ MISC_DP_CTRL_4, 0x86640000, 128 * 1024, 0x86660000, 64 * 1024 },
	{ MISC_DP_CTRL_5, 0x88040000, 128 * 1024, 0x88060000, 64 * 1024 },
	{ MISC_DP_CTRL_6, 0x88080000, 128 * 1024, 0x880a0000, 64 * 1024 },
	{ MISC_DP_CTRL_7, 0x880c0000, 128 * 1024, 0x880e0000, 64 * 1024 },
};

static void dp_reset(struct dp *dp, bool release)
{
	volatile uint32_t *dp_ctrl = misc_base + dp->params.ctrl_misc_offset;
	if (release) {
		*dp_ctrl = 0x3;
		*dp_ctrl = 0x2;
	}
	else {
		*dp_ctrl = 0x3;
	}
}

static eqmhi_status_t write_buffer(struct dp *dp, uint32_t start, uint32_t size,
                                   uint32_t offset,
                                   size_t buf_size, void *buf)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	void *ptr = (void *) MAP_FAILED;
	int fd;

	fd = open("/dev/mem", O_RDWR | O_SYNC);
	if (fd == -1) {
		E(dp, "Failed to open /dev/mem");
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	ptr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, start);
	if (ptr == (void *) MAP_FAILED) {
		E(dp, "Failed to map memory at 0x%x of size %d bytes", start, size);
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	memcpy(ptr + offset, buf, buf_size);

exit:
	if (ptr != (void *) MAP_FAILED)
		munmap(ptr, size);

	if (fd != -1)
		close(fd);

	return status;
}

static eqmhi_status_t read_buffer(struct dp *dp, uint32_t start, uint32_t size,
                                  uint32_t offset,
                                  size_t buf_size, void *buf)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	void *ptr = (void *) MAP_FAILED;
	int fd;

	fd = open("/dev/mem", O_RDWR | O_SYNC);
	if (fd == -1) {
		E(dp, "Failed to open /dev/mem");
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	ptr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, start);
	if (ptr == (void *) MAP_FAILED) {
		E(dp, "Failed to map memory at 0x%x of size %d bytes", start, size);
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	memcpy(buf, ptr + offset, buf_size);

exit:
	if (ptr != (void *) MAP_FAILED)
		munmap(ptr, size);

	if (fd != -1)
		close(fd);

	return status;
}

static bool validate_config_for_boot(struct dp *dp)
{
	if (!dp->dump_file_configured) {
		I(dp, "Cannot boot - dump file not configured");
		return false;
	}

	if (!dp->tx_mbox_id) {
		I(dp, "Cannot boot - tx mailbox id not configured");
		return false;
	}

	if (!dp->rx_mbox_id) {
		I(dp, "Cannot boot - rx mailbox id not configured");
		return false;
	}

	if (!dp->tx_shmem.start || !dp->tx_shmem.size) {
		I(dp, "Cannot boot - tx shmem not configured properly");
		return false;
	}

	if (!dp->rx_shmem.start || !dp->rx_shmem.size) {
		I(dp, "Cannot boot - rx shmem not configured properly");
		return false;
	}

	return true;
}

static eqmhi_status_t unpack_lmc (struct eqmhi_load_entity *e, uint8_t **lmc,
		int32_t * numbytes, struct dp *dp)
{
	int32_t bytes_read;
	FILE * lmc_file = fopen(e->load.lmc.name, "r");
	if (!lmc_file){
		E(dp, "LMC file could not be open.");
		return EQMHI_STATUS_OTHER;
	}

	/* Get the number of bytes */
	fseek(lmc_file, 0L, SEEK_END);
	*numbytes = ftell(lmc_file);
	if(*numbytes < 0){
		E(dp, "LMC file file size error.");
		fclose(lmc_file);
		return EQMHI_STATUS_OTHER;
	}

	/* reset the file position indicator to
	   the beginning of the file */
	fseek(lmc_file, 0L, SEEK_SET);

	*lmc = (uint8_t *) malloc(*numbytes);
	if(!(*lmc)){
		E(dp, "No memory to allocate for LMC.");
		fclose(lmc_file);
		return EQMHI_STATUS_OTHER;
	}
	bytes_read = fread(*lmc, sizeof(uint8_t), *numbytes, lmc_file);
	if (bytes_read != *numbytes) {
		E(dp, "LMC file bytes read mismatch.");
		fclose(lmc_file);
		free(*lmc);
		return EQMHI_STATUS_OTHER;
	}
	fclose(lmc_file);
	return EQMHI_STATUS_SUCCESS;
}

static eqmhi_status_t find_ru_dp_lm(struct lmc_lm **xenon_dp_lm,
                                    struct lmc_lm *lm_list,
                                    int32_t num_of_lms,
                                    const char *identity,
                                    uint32_t *xenon_dp_lm_size)
{
	int i;

	*xenon_dp_lm = lm_list;
	for (i = 0; i < num_of_lms; i++) {
		if ((*xenon_dp_lm)->header.hdr_short.LMFV == LMC_RU_DP_FORMAT &&
		    (*xenon_dp_lm)->header.hdr_short.LMT == LMC_DSP &&
		    strncmp((*xenon_dp_lm)->header.hdr_ru_dp.IDENTITY, identity,
		            LMC_MAX_NO_OF_BYTES_IN_IDENTITY) == 0) {
			/* get size of excess fields in lmdc */
			*xenon_dp_lm_size =
				((*xenon_dp_lm)->header.hdr_ru_dp.DCL * 2) -
				sizeof((*xenon_dp_lm)->lmdc.dsp.L) -
				sizeof((*xenon_dp_lm)->lmdc.dsp.A) -
				sizeof((*xenon_dp_lm)->lmdc.dsp.FFU) -
				sizeof((*xenon_dp_lm)->lmdc.dsp.CRC);
			return EQMHI_STATUS_SUCCESS;
		} else {
			(*xenon_dp_lm) = (*xenon_dp_lm)->next;
		}
	}
	return EQMHI_STATUS_OTHER;
}

static int init_link(struct dp *dp, void **lnh)
{
	uint32_t sel[] = { 1, ITC_LOCATE_DEFAULT_NO };
	union itc_msg *reply;

	if (ulh_lnh_init(MAX_NUMBER_OF_LINKS)) {
		E(dp, "Failed to initialize link handler");
		return -1;
	}

	if (ulh_shmem_init(CM_NAME)) {
		E(dp, "Failed to initialize shmem cm: %s", CM_NAME);
		return -1;
	}

	*lnh = ulh_lnh_create(ULNH_NAME);
	if (!*lnh) {
		E(dp, "Failed to create link handler %s", ULNH_NAME);
		return -1;
	}

	itc_locate_async(ULNH_NAME, NULL, ITC_MY_MBOX);
	reply = itc_receive(sel, ITC_NO_TMO, ITC_FROM_ALL);
	RS(dp, reply->msgno, "ITC_LOCATE_DEFAULT_NO");
	link_mbox = itc_sender(reply);
	itc_free(&reply);

	return 0;
}

static eqmhi_status_t configure_dp(struct dp *dp)
{
	struct dp_cfg cfg;

	/* Tx for CPU becomes Rx for DP and vice versa */
	cfg.link_cfg.tx_shmem_start = dp->rx_shmem.start;
	cfg.link_cfg.tx_shmem_size = dp->rx_shmem.size;
	cfg.link_cfg.tx_mbox_id = dp->rx_mbox_id - 1;
	cfg.link_cfg.rx_shmem_start = dp->tx_shmem.start;
	cfg.link_cfg.rx_shmem_size = dp->tx_shmem.size;
	cfg.link_cfg.rx_mbox_id = dp->tx_mbox_id - 1;

	return write_buffer(dp,
	                    dp->params.data_ram_start,
	                    dp->params.data_ram_size,
	                    DP_CFG_OFFSET,
	                    sizeof(struct dp_cfg),
	                    &cfg);
}

static eqmhi_status_t init_shmem(struct dp *dp)
{
	eqmhi_status_t  status;
	void           *buf;
	uint32_t        size;

	size = MAX(dp->rx_shmem.size, dp->tx_shmem.size);
	buf = malloc(size);
	if (buf == NULL) {
		E(dp, "Failed malloc for %u bytes", size);
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	memset(buf, 0, size);

	status = write_buffer(dp,
	                      dp->rx_shmem.start,
	                      dp->rx_shmem.size,
	                      0,
	                      dp->rx_shmem.size,
	                      buf);

	if (status == EQMHI_STATUS_SUCCESS) {
		status = write_buffer(dp,
		                      dp->tx_shmem.start,
		                      dp->tx_shmem.size,
		                      0,
		                      dp->tx_shmem.size,
		                      buf);
	}

	free(buf);
exit:
	return status;
}

static eqmhi_status_t create_link(struct dp *dp)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	struct ulh_shmem_cm_config cfg;
	uint32_t filter1[2] = { 1, ULH_LNHMSG_CREATECM_RSP };
	uint32_t filter2[2] = { 1, ULH_LNHMSG_CREATELINK_RSP };

	cfg.common.cfg_size = sizeof(struct ulh_shmem_cm_config);
	cfg.watchdog_interval_ms = dp->watchdog_interval;
	cfg.watchdog_ms = dp->watchdog_timeout;
	cfg.tx_mbox = dp->tx_mbox_id - 1;
	cfg.rx_mbox = dp->rx_mbox_id - 1;
	cfg.tx = dp->tx_shmem;
	cfg.rx = dp->rx_shmem;

	uint32_t cmid;
	union itc_msg *reply;
	union itc_msg *msg = itc_alloc(sizeof(struct ulh_lnhmsg_createcm_req) +
	                               sizeof(struct ulh_shmem_cm_config),
	                               ULH_LNHMSG_CREATECM_REQ);

	msg->cmreq.seq = 0;
	strcpy(msg->cmreq.cm_name, CM_NAME);
	strcpy(msg->cmreq.cm_instance, "shmem-cm-0");
	memcpy(msg->cmreq.config, &cfg, sizeof(struct ulh_shmem_cm_config));
	SS(dp, ULH_LNHMSG_CREATECM_REQ, link_mbox, "ULH_LNHMSG_CREATECM_REQ");
	itc_send(&msg, link_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter1, ITC_NO_TMO, link_mbox);
	RS(dp, reply->msgno, "ULH_LNHMSG_CREATECM_RSP");
	if (reply->cmrsp.result) {
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	cmid = reply->cmrsp.cmid;
	itc_free(&reply);

	msg = itc_alloc(sizeof(struct ulh_lnhmsg_createlink_req),
	                ULH_LNHMSG_CREATELINK_REQ);
	msg->lcreq.prio = 0;
	msg->lcreq.cmid = cmid;
	snprintf(msg->lcreq.name, ULH_LNHNAMESIZ, "dp%d/", dp->id);

	SS(dp, ULH_LNHMSG_CREATELINK_REQ, link_mbox, "ULH_LNHMSG_CREATELINK_REQ");
	itc_send(&msg, link_mbox, ITC_MY_MBOX);
	reply = itc_receive(filter2, ITC_NO_TMO, link_mbox);
	RS(dp, reply->msgno, "ULH_LNHMSG_CREATELINK_RSP");
	if (reply->lcrsp.result) {
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	dp->lid = reply->lcrsp.lid;
	dp->link_created = true;

exit:
	itc_free(&reply);

	return status;
}

eqmhi_status_t set_dp_cfg(struct dp *dp, struct eqmhi_cfg_ru_dp *dpcfg)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	status = write_buffer(dp,
	                      dp->params.data_ram_start,
	                      dp->params.data_ram_size,
	                      dpcfg->offset,
	                      dpcfg->size,
	                      dpcfg->buf);
	return status;
}

eqmhi_status_t get_dp_cfg(struct dp *dp, void **buf)
{
	void *b = malloc(dp->dp_size);
	if (!b)
		return EQMHI_STATUS_OTHER;

	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	status = read_buffer(dp,
	                     dp->params.data_ram_start,
	                     dp->params.data_ram_size,
	                     dp->dp_offset,
	                     dp->dp_size,
	                     b);

	if (status == EQMHI_STATUS_SUCCESS)
		*buf = b;

	return status;
}

static void set_cfg(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;
	struct eqmh_set_cfg_req *req = &msg->eqmh_set_cfg_req;
	struct eqmhi_cfg_entity *e = &req->entities[0];
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	if (dp->state != DP_STATE_LOADED) {
		I(dp, "dp state is %d, must be in state 'LOADED' to config",
		      dp->state);
		status = EQMHI_STATUS_WRONG_STATE;
		goto exit;
	}

	if (e->eqm_id != dp->id) {
		E(dp, "Received set cfg for eqmid %d in eqmid %d thread",
		      e->eqm_id, dp->id);
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	switch (e->cfg.type) {
		case EQMHI_CFG_DUMP_FILE:
			dp->dump_file = e->cfg.dump_file;
			dp->dump_file_configured = true;
			break;
		case EQMHI_CFG_RU_DISABLE_LINK:
			dp->disable_link = e->cfg.disable_link.disable;
			break;
		case EQMHI_CFG_RU_WATCHDOG_INTERVAL:
			dp->watchdog_interval = e->cfg.watchdog_interval.ms;
			break;
		case EQMHI_CFG_RU_WATCHDOG_TIMEOUT:
			dp->watchdog_timeout = e->cfg.watchdog_timeout.ms;
			break;
		case EQMHI_CFG_RU_TX_MBOX:
			dp->tx_mbox_id = e->cfg.tx_mbox.id + 1;
			break;
		case EQMHI_CFG_RU_RX_MBOX:
			dp->rx_mbox_id = e->cfg.rx_mbox.id + 1;
			break;
		case EQMHI_CFG_RU_TX_SHMEM:
			dp->tx_shmem.start = e->cfg.tx_shmem.start;
			dp->tx_shmem.size = e->cfg.tx_shmem.size;
			break;
		case EQMHI_CFG_RU_RX_SHMEM:
			dp->rx_shmem.start = e->cfg.rx_shmem.start;
			dp->rx_shmem.size = e->cfg.rx_shmem.size;
			break;
		case EQMHI_CFG_RU_DP:
			status = set_dp_cfg(dp, &e->cfg.dp);
			if (status != EQMHI_STATUS_SUCCESS)
				goto exit;

			dp->dp_size = e->cfg.dp.size;
			dp->dp_offset = e->cfg.dp.offset;
			break;
		default:
			E(dp, "Unknown config type %d", e->cfg.type);
			status = EQMHI_STATUS_INVALID_PARAM;
	}

exit:
	if (status == EQMHI_STATUS_SUCCESS) {
		reply = itc_alloc(sizeof(struct eqmh_set_cfg_cfm), EQMH_SET_CFG_CFM);
		reply->eqmh_set_cfg_cfm.user_data = (void *)
		    msg->eqmh_set_cfg_req.user_data;
		SS(dp, EQMH_SET_CFG_CFM, dp->server_mbox, "EQMH_SET_CFG_CFM");
	}
	else {
		reply = itc_alloc(sizeof(struct eqmh_set_cfg_rej), EQMH_SET_CFG_REJ);
		reply->eqmh_set_cfg_rej.status = status;
		reply->eqmh_set_cfg_rej.user_data = (void *)
		    msg->eqmh_set_cfg_req.user_data;
		SS(dp, EQMH_SET_CFG_REJ, dp->server_mbox, "EQMH_SET_CFG_REJ");
	}

	reply->any_msg.connection_ref = msg->any_msg.connection_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	itc_send(&reply, dp->server_mbox, itc_sender(msg));
}

static void get_cfg(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;
	struct eqmh_get_cfg_req *req = &msg->eqmh_get_cfg_req;
	struct eqmhi_cfg_entity *e = &req->entities[0];
	size_t size = sizeof(struct eqmh_get_cfg_cfm);
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	void *buf;

	if (e->eqm_id != dp->id) {
		E(dp, "Received set cfg for eqmid %d in eqmid %d thread",
		      e->eqm_id, dp->id);
		status = EQMHI_STATUS_OTHER;
		goto exit;
	}

	switch(e->cfg.type) {
		case EQMHI_CFG_DUMP_FILE:
			e->cfg.dump_file = dp->dump_file;
			break;
		case EQMHI_CFG_RU_DISABLE_LINK:
			e->cfg.disable_link.disable = dp->disable_link;
			break;
		case EQMHI_CFG_RU_WATCHDOG_INTERVAL:
			e->cfg.watchdog_interval.ms = dp->watchdog_interval;
			break;
		case EQMHI_CFG_RU_WATCHDOG_TIMEOUT:
			e->cfg.watchdog_timeout.ms = dp->watchdog_timeout;
			break;
		case EQMHI_CFG_RU_TX_MBOX:
			e->cfg.tx_mbox.id = dp->tx_mbox_id - 1;
			break;
		case EQMHI_CFG_RU_RX_MBOX:
			e->cfg.rx_mbox.id = dp->rx_mbox_id - 1;
			break;
		case EQMHI_CFG_RU_TX_SHMEM:
			e->cfg.tx_shmem.start = dp->tx_shmem.start;
			e->cfg.tx_shmem.size = dp->tx_shmem.size;
			break;
		case EQMHI_CFG_RU_RX_SHMEM:
			e->cfg.rx_shmem.start = dp->rx_shmem.start;
			e->cfg.rx_shmem.size = dp->rx_shmem.size;
			break;
		case EQMHI_CFG_RU_DP:
			if (!dp->dp_size) {
				status = EQMHI_STATUS_WRONG_STATE;
				goto exit;
			}

			status = get_dp_cfg(dp, &buf);
			if (status != EQMHI_STATUS_SUCCESS)
				goto exit;
			e->cfg.dp.size = dp->dp_size;
			e->cfg.dp.offset = dp->dp_offset;
			size += dp->dp_size;
			break;
		default:
			I(dp, "Received unknown config type %d", e->cfg.type);
			status = EQMHI_STATUS_INVALID_PARAM;
	}

exit:
	if (status == EQMHI_STATUS_SUCCESS) {
		reply = itc_alloc(size, EQMH_GET_CFG_CFM);
		reply->eqmh_get_cfg_cfm.entities[0] = *e;
		reply->eqmh_get_cfg_cfm.num_of_entities = 1;

		if (e->cfg.type == EQMHI_CFG_RU_DP) {
			void * dp_data = (((char *) reply) +
		                      sizeof(struct eqmh_get_cfg_cfm));
			memcpy(dp_data, buf, e->cfg.dp.size);
			free(buf);
			reply->eqmh_get_cfg_cfm.entities[0].cfg.dp.buf = dp_data;
			reply->eqmh_get_cfg_cfm.entities[0].cfg.dp.size = e->cfg.dp.size;
		}

		reply->eqmh_get_cfg_cfm.user_data = (void *)
		    msg->eqmh_get_cfg_req.user_data;
		SS(dp, EQMH_GET_CFG_CFM, dp->server_mbox, "EQMH_GET_CFG_CFM");
	}
	else {
		reply = itc_alloc(sizeof(struct eqmh_get_cfg_rej), EQMH_GET_CFG_REJ);
		reply->eqmh_get_cfg_rej.status = status;
		reply->eqmh_get_cfg_rej.user_data = (void *)
		    msg->eqmh_set_cfg_req.user_data;
		SS(dp, EQMH_GET_CFG_REJ, dp->server_mbox, "EQMH_GET_CFG_REJ");
	}

	reply->any_msg.connection_ref = msg->any_msg.connection_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	itc_send(&reply, dp->server_mbox, itc_sender(msg));
}

static void load(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	if (dp->state != DP_STATE_STOPPED) {
		I(dp, "dp state is %d, must be in state 'STOPPED' to load",
		  dp->state);
		status = EQMHI_STATUS_WRONG_STATE;
		goto exit;
	}

	for (int i = 0; i < msg->eqmh_load_req.num_of_entities; i++) {
		struct eqmhi_load_entity *e =
				&msg->eqmh_load_req.entities[i];

		if (e->load.type == EQMHI_LOAD_TYPE_LMC) {
			uint8_t * lmc;
			int32_t numbytes;
			struct lmc_hdr * lmc_header = NULL;
			int32_t num_of_lms;
			struct lmc_lm * lm_list = NULL;

			status = unpack_lmc(e, &lmc, &numbytes, dp);

			if (status != EQMHI_STATUS_SUCCESS){
				I(dp, "LMC unpacking failed.");
				goto exit;
			}

			num_of_lms = lmc_run_parser((uint8_t *) lmc,
			                            numbytes,
			                            &lmc_header,
			                            &lm_list);

			if (num_of_lms < 1) {
				I(dp, "No LMs found in lmc!");
				status = EQMHI_STATUS_OTHER;
				lmc_free_lm_list(&lm_list);
				free(lmc);
				free(lmc_header);
				goto exit;
			}

			struct lmc_lm * xenon_dp_lm;
			uint32_t xenon_dp_lm_size;

			status = find_ru_dp_lm(&xenon_dp_lm, lm_list, num_of_lms,
			                       e->load.lmc.lm_id,
			                       &xenon_dp_lm_size);
			if (status != EQMHI_STATUS_SUCCESS) {
				I(dp, "No RU DP LM found in LM list.");
				lmc_free_lm_list(&lm_list);
				free(lmc);
				free(lmc_header);
				goto exit;
			}

			status = write_buffer(dp,
			                      dp->params.code_ram_start,
			                      dp->params.code_ram_size +
			                      dp->params.data_ram_size,
			                      0,
			                      xenon_dp_lm_size,
			                      xenon_dp_lm->lmdc.dsp.data);

			free(lmc_header);
			xenon_dp_lm = NULL;
			lmc_free_lm_list(&lm_list);
			free(lmc);

		} else {
			I(dp, "Only load type lmc supported");
			status = EQMHI_STATUS_INVALID_PARAM;
			goto exit;
		}

		if (status != EQMHI_STATUS_SUCCESS)
			goto exit;
	}

	dp->state = DP_STATE_LOADED;

exit:
	if (status == EQMHI_STATUS_SUCCESS) {
		reply = itc_alloc(sizeof(struct eqmh_load_cfm), EQMH_LOAD_CFM);
		SS(dp, EQMH_LOAD_CFM, dp->server_mbox, "EQMH_LOAD_CFM");
	}
	else {
		reply = itc_alloc(sizeof(struct eqmh_load_rej), EQMH_LOAD_REJ);
		reply->eqmh_load_rej.status = status;
		SS(dp, EQMH_LOAD_REJ, dp->server_mbox, "EQMH_LOAD_REJ");
	}

	reply->any_msg.connection_ref = msg->any_msg.connection_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	itc_send(&reply, dp->server_mbox, itc_sender(msg));
}

static void boot(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	if (dp->state != DP_STATE_LOADED) {
		I(dp, "dp state is %d, must be in state 'LOADED' to boot", dp->state);
		status = EQMHI_STATUS_WRONG_STATE;
		goto exit;
	}

	if (!validate_config_for_boot(dp)) {
		status = EQMHI_STATUS_WRONG_STATE;
		goto exit;
	}

	if (!dp->disable_link) {
		status = init_shmem(dp);
		if (status != EQMHI_STATUS_SUCCESS) {
			goto exit;
		}
		status = configure_dp(dp);
		if (status != EQMHI_STATUS_SUCCESS) {
			goto exit;
		}
	}

	dp_reset(dp, true);
	dp->state = DP_STATE_RUNNING;

	if (!dp->disable_link) {
		status = create_link(dp);
		if (status != EQMHI_STATUS_SUCCESS)
			goto exit;

		/* Save the message for when we get link up */
		memcpy(&dp->save_msg, msg, itc_size(msg));
		dp->sender_mbox = itc_sender(msg);
		return;
	}

exit:
	if (status == EQMHI_STATUS_SUCCESS) {
		reply = itc_alloc(sizeof(struct eqmh_boot_cfm), EQMH_BOOT_CFM);
		SS(dp, EQMH_BOOT_CFM, dp->server_mbox, "EQMH_BOOT_CFM");
	}
	else {
		reply = itc_alloc(sizeof(struct eqmh_boot_rej), EQMH_BOOT_REJ);
		reply->eqmh_boot_rej.status = status;
		SS(dp, EQMH_BOOT_REJ, dp->server_mbox, "EQMH_BOOT_REJ");
	}

	reply->any_msg.connection_ref = msg->any_msg.connection_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	itc_send(&reply, dp->server_mbox, itc_sender(msg));
}

static void dump(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	if (dp->state != DP_STATE_STOPPED && dp->state != DP_STATE_DUMPED) {
		I(dp, "dp state is %d, must be in state 'STOPPED' to dump", dp->state);
		status = EQMHI_STATUS_WRONG_STATE;
		goto exit;
	}

	dp->state = DP_STATE_DUMPED;

exit:
	if (status == EQMHI_STATUS_SUCCESS) {
		reply = itc_alloc(sizeof(struct eqmh_dump_cfm), EQMH_DUMP_CFM);
		SS(dp, EQMH_DUMP_CFM, dp->server_mbox, "EQMH_DUMP_CFM");
	}
	else {
		reply = itc_alloc(sizeof(struct eqmh_dump_rej), EQMH_DUMP_REJ);
		reply->eqmh_dump_rej.status = status;
		SS(dp, EQMH_DUMP_REJ, dp->server_mbox, "EQMH_DUMP_REJ");
	}

	reply->any_msg.connection_ref = msg->any_msg.connection_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	itc_send(&reply, dp->server_mbox, itc_sender(msg));
}

static void stop(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;

	dp_reset(dp, false);
	dp->state = DP_STATE_STOPPED;

	reply = itc_alloc(sizeof(struct eqmh_stop_cfm), EQMH_STOP_CFM);
	SS(dp, EQMH_STOP_CFM, dp->server_mbox, "EQMH_STOP_CFM");

	reply->any_msg.connection_ref = msg->any_msg.connection_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	itc_send(&reply, dp->server_mbox, itc_sender(msg));
}

static void destroy(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;
	reply = itc_alloc(sizeof(struct eqmh_destroy_cfm), EQMH_DESTROY_CFM);
	reply->any_msg.connection_ref = msg->any_msg.connection_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	SS(dp, EQMH_DESTROY_CFM, dp->server_mbox, "EQMH_DESTROY_CFM");
	itc_send(&reply, dp->server_mbox, itc_sender(msg));
}

static void link_status(struct dp *dp, union itc_msg *msg)
{
	union itc_msg *reply;

	I(dp, "notify state %d", msg->ulind.state);

	if (msg->ulind.state == ULH_LINK_UP) {
		reply = itc_alloc(sizeof(struct eqmh_boot_cfm), EQMH_BOOT_CFM);
		reply->any_msg.connection_ref = dp->save_msg.any_msg.connection_ref;
		reply->any_msg.procedure_ref = dp->save_msg.any_msg.procedure_ref;
		SS(dp, EQMH_BOOT_CFM, dp->server_mbox, "EQMH_BOOT_CFM");
		itc_send(&reply, dp->server_mbox, dp->sender_mbox);
	}

	/* TODO: What to do on link down? */
}

static void destroy_link(struct dp *dp)
{
	uint32_t filter[2] = { 1, ULH_LNHMSG_DESTROYLINK_RSP };
	union itc_msg *reply;
	union itc_msg *msg = itc_alloc(sizeof(struct ulh_lnhmsg_destroylink_req),
	                               ULH_LNHMSG_DESTROYLINK_REQ);
	msg->ldreq.seq = 0;
	msg->ldreq.lid = dp->lid;
	SS(dp, ULH_LNHMSG_DESTROYLINK_REQ, link_mbox, "ULH_LNHMSG_DESTROYLINK_REQ");
	itc_send(&msg, link_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, link_mbox);
	RS(dp, reply->msgno, "ULH_LNHMSG_DESTROYLINK_RSP");
	if (reply->ldrsp.result)
		E(dp, "Failed to destroy shmem dp link");
	itc_free(&reply);
}

void *xenon_dp_thrfxn(void *arg)
{
	struct eqm_args *args = (struct eqm_args *) arg;
	char mbox_name[MBOX_MAX_NAME_SIZE];
	union itc_msg *msg;
	struct dp dp = { 0, };
	bool quit = false;

	snprintf(mbox_name, MBOX_MAX_NAME_SIZE, MBOX_NAME_BASE "%d", args->id);
	dp.server_mbox = args->server_mbox;
	dp.id = args->id;

	dp.mbox = itc_create_mailbox(mbox_name, 0);
	if (dp.mbox == ITC_NO_ID) {
		E(&dp, "Failed to create mailbox %s", mbox_name);
		quit = true;
		goto init_exit;
	}

	if (!lnh) {
		if (init_link(&dp, &lnh) < 0) {
			quit = true;
			goto init_exit;
		}
	}

	dp.params = params[dp.id-1];

	if (misc_uio_handle == (UIO_HANDLE_ *) -1) {
		misc_uio_handle = uio_open(UIO_DEV_MISC);
		if (misc_uio_handle == (UIO_HANDLE_ *) -1) {
			E(&dp, "Failed to open uio device %s", UIO_DEV_MISC);
			quit = true;
			goto init_exit;
		}

		misc_base = uio_mmap(misc_uio_handle);
		if (misc_base == MAP_FAILED) {
			E(&dp, "Failed to map misc memory");
			quit = true;
			goto init_exit;
		}
	}

	dp_reset(&dp, false);

init_exit:
	if (quit) {
		msg = itc_alloc(sizeof(uint32_t), EQM_INSTANCE_CREATED_FAIL);
		SS(&dp, EQM_INSTANCE_CREATED_FAIL, args->server_mbox,
		   "EQM_INSTANCE_CREATED_FAIL");
	}
	else {
		msg = itc_alloc(sizeof(uint32_t), EQM_INSTANCE_CREATED_SUCCESS);
		SS(&dp, EQM_INSTANCE_CREATED_SUCCESS, args->server_mbox,
		   "EQM_INSTANCE_CREATED_SUCCESS");
	}
	itc_send(&msg, args->server_mbox, ITC_MY_MBOX);

	while (!quit) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {
			case EQMH_SET_CFG_REQ:
				RS(&dp, msg->msgno, "EQMH_SET_CFG_REQ");
				set_cfg(&dp, msg);
				break;
			case EQMH_GET_CFG_REQ:
				RS(&dp, msg->msgno, "EQMH_GET_CFG_REQ");
				get_cfg(&dp, msg);
				break;
			case EQMH_LOAD_REQ:
				RS(&dp, msg->msgno, "EQMH_LOAD_REQ");
				load(&dp, msg);
				break;
			case EQMH_BOOT_REQ:
				RS(&dp, msg->msgno, "EQMH_BOOT_REQ");
				boot(&dp, msg);
				break;
			case EQMH_DUMP_REQ:
				RS(&dp, msg->msgno, "EQMH_DUMP_REQ");
				dump(&dp, msg);
				break;
			case EQMH_STOP_REQ:
				RS(&dp, msg->msgno, "EQMH_STOP_REQ");
				stop(&dp, msg);
				break;
			case EQMH_DESTROY_REQ:
				RS(&dp, msg->msgno, "EQMH_DESTROY_REQ");
				destroy(&dp, msg);
				quit = true;
				break;
			case ULH_LNHMSG_NOTIFY:
				RS(&dp, msg->msgno, "ULH_LNHMSG_NOTIFY");
				link_status(&dp, msg);
				break;
			default:
				I(&dp, "Received unsupported msgno %d", msg->msgno);
				break;
		}

		itc_free(&msg);
	}

	if (dp.link_created)
		destroy_link(&dp);

#if 0
	ulh_lnh_destroy(lnh);
#endif

	if (dp.mbox != ITC_NO_ID)
		itc_delete_mailbox(dp.mbox);

	return (void *) EQMHI_STATUS_SUCCESS;
}

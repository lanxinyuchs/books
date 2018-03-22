
#include <stdio.h>
#include <assert.h>

#include <ecb_dev.h>
#include <ecb_unc.h>

#define HDLC_SNRM         0x83
#define HDLC_UA           0x63
#define HDLC_PF           0x10

#define HDLC_RESPONSE_TMO  100

/* 1, SNRM P
 * little endian FCS: { 7E 01 93 8D B0 7E }
 * big endian FCS:    { 7E 01 93 B0 8D 7E }
 *
 * 1, UA F
 * little endian FCS: { 7E 01 73 83 57 7E }
 * big endian FCS:    { 7E 01 73 57 83 7E }
 */


static struct timespec rsp_tmo = {
        .tv_sec = 0,
        .tv_nsec = HDLC_RESPONSE_TMO * 1000000
};

static uint8_t prefix[] = { 1, 2, 3 };

static struct ecb_unc_opt_prefix p = {
  .size = sizeof(prefix),
  .data = prefix
};


int main(int argc, char *argv[])
{
	void    *handle;
	uint8_t  rsp_ctrl;
	uint8_t  rsp_info[12];
	uint32_t rsp_info_size = sizeof(rsp_info);
	uint32_t n;

	ecb_dev_init(&handle, "/dev/ttyUSB3");
	assert(handle);

        /* Use a prefix of 3 bytes */
        assert(ecb_unc_opt(handle, ECB_UNC_OPT_PREFIX, &p) == 0);

	n = 1; /* Use one opening flag */
	assert(ecb_unc_opt(handle, ECB_UNC_OPT_N_OFLAGS, &n) == 0);

	n = 0; /* Use little endian */
	assert(ecb_unc_opt(handle, ECB_UNC_OPT_FCS_BIG_ENDIAN, &n) == 0);
	assert(ecb_unc_cmd_rsp(handle, 0, 1,
			       HDLC_SNRM | HDLC_PF, 0, 0,
			       &rsp_ctrl, rsp_info, &rsp_info_size,
			       &rsp_tmo) != -1);
	assert(ecb_unc_cmd_rsp(handle, 0, 1,
			       HDLC_UA | HDLC_PF, 0, 0,
			       &rsp_ctrl, rsp_info, &rsp_info_size,
			       &rsp_tmo) != -1);

	n = 1; /* Use big endian */
	assert(ecb_unc_opt(handle, ECB_UNC_OPT_FCS_BIG_ENDIAN, &n) == 0);
	assert(ecb_unc_cmd_rsp(handle, 0, 1,
			       HDLC_SNRM | HDLC_PF, 0, 0,
			       &rsp_ctrl, rsp_info, &rsp_info_size,
			       &rsp_tmo) != -1);
	assert(ecb_unc_cmd_rsp(handle, 0, 1,
			       HDLC_UA | HDLC_PF, 0, 0,
			       &rsp_ctrl, rsp_info, &rsp_info_size,
			       &rsp_tmo) != -1);

	assert(ecb_dev_shutdown(handle) == 0);

	return 0;
}

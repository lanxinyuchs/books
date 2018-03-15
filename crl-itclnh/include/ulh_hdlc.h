#ifndef ULH_HDLC_H__
#define ULH_HDLC_H__

#include <stdint.h>

#include <ulh_lnh.h>

#define ULH_HDLC_CFGVER_SWITCHING 1

struct ulh_cm_hdlc_config {
	struct ulh_cm_config cmn;
	int primary;
	uint32_t hdlc_addr;

	/* applicable if cfg_version >= ULH_HDLC_CFGVER_SWITCHING */
	uint32_t redundant;		/* used if primary */
	uint32_t port;			/* local port id */
	uint32_t ecp_addr;		/* local ecp address */
	uint32_t notify_mbox;	/* mbox for notifications (e.g. remote port id) */
};

int ulh_hdlc_init(const char *name);

/* instruct HDLC CM to start getting remote port id */
int ulh_hdlc_initiate_get_remote_port_id(struct ulh_lnh *lnh,
		uint32_t link_id);
/* instruct HDLC CM to start link switch procedure */
int ulh_hdlc_initiate_linkswitch(struct ulh_lnh *lnh,
		uint32_t redundant_link_id, uint32_t active_link_id,
		uint32_t active_remote_port_id);

/* notification about received remote port id */
#define ULH_HDLCMSG_REMOTEPORTID_IND		(0x01900438)
struct ulh_hdlcmsg_remoteportid_ind {
	uint32_t msg_no;
	uint32_t result;
	uint32_t link_id;
	uint32_t port_id;
};

/* notification about successful link switch */
#define ULH_HDLCMSG_LINKSWITCH_IND			(0x01900439)
struct ulh_hdlcmsg_linkswitch_ind {
	uint32_t msg_no;
	uint32_t result;
	uint32_t link_id;
	uint32_t to_link_id;
};

#endif /* !ULH_HDLC_H__ */

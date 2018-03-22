#ifndef ULH_RLNH_H__
#define ULH_RLNH_H__

/*
 * RLNH proto
 */

#define RLNH_VERSION	1

#define RLNH_INIT		5
#define RLNH_INIT_REPLY		6
#define RLNH_PUBLISH		2
#define RLNH_QUERY_NAME		1
#define RLNH_UNPUBLISH		3
#define RLNH_UNPUBLISH_ACK	4
#define RLNH_PUBLISH_PEER	7 /* XXX RLNH version 2 message
				   * which we don't support */

struct rlnh_header {
	uint16_t reserved;
	uint16_t type;
};

struct rlnh_msg_init {
	struct rlnh_header hdr;
	uint32_t version;
};

struct rlnh_msg_initreply {
	struct rlnh_header hdr;
	uint32_t status;

	/*char features[1];   XXX RLNH Version 2 not supported */
};

struct rlnh_msg_publish {
	struct rlnh_header hdr;
	uint32_t laddr;

	char name[1];
};

struct rlnh_msg_unpublish {
	struct rlnh_header hdr;
	uint32_t laddr;
};

struct rlnh_msg_unpublish_ack {
	struct rlnh_header hdr;
	uint32_t laddr;
};

struct rlnh_msg_queryname {
	struct rlnh_header hdr;
	uint32_t laddr;

	char name[1];
};

struct ulh_cm_msghdr;

#ifdef ULH_RLNH_DEBUG
void ulh_rlnh_print_header(const char *name, const char *prfx,
		struct ulh_cm_msghdr *hdr, uint16_t type);
#else
#define ulh_rlnh_print_header(_n, _p, _h, _t)
#endif /* ULH_RLNH_DEBUG */

#endif /* ULH_RLNH_H__ */

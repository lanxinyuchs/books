#ifndef _MHP3_PROTO_H_
#define _MHP3_PROTO_H_

#define MHP_CONN	0x43
#define MHP_DISC	0x44
#define MHP_DATA	0x40
#define MHP_ACK		0x41
#define MHP_NACK	0x4e

struct mhp_header {
	uint16_t feature;
	uint8_t prio;
	uint8_t type;
	uint16_t ns;
	uint16_t nr;
};

struct mhp_conn {
	struct mhp_header cmn;
	uint16_t local_cid;
	uint16_t remote_cid;
};

struct mhp_disc {
	struct mhp_header cmn;
	uint16_t local_cid;
	uint16_t remote_cid;
};

#define MHP_DATA_F_FRAG		(1 << 11)
#define MHP_DATA_FRAGNO_MASK	(0x7ff)
#define MHP_DATA_MAXFRAGS	(0x7ff)

struct mhp_data {
	struct mhp_header cmn;
	uint16_t size;
	uint16_t frag;
};

struct mhp_data_hdr {
	uint32_t src;
	uint32_t dst;
	uint32_t extra;
};

#endif /* _MHP3_PROTO_H_ */

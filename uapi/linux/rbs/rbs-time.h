#ifndef UAPI_RBS_TIME_H__
#define UAPI_RBS_TIME_H__

/* smem */

/*
 * ppc offset storage format
 */
struct rbs_time_shared {
	/* BFN-offset */
	int64_t bfn_offset;
	unsigned bfn_seq;
};

/*
 * arm offset storage format
 */
struct rbs_time_cpm1_offset_slot {
	/* AXM offset in axxia/nuevo time counter steps */
	int64_t offset_atc;
	/* AXM offset in ns */
	int64_t offset_kernel;

	/* AXM offset in pico seconds */
	int64_t offset_seconds;
	uint64_t offset_fraction;

	uint8_t pad[256];
};

struct rbs_time_cpm1_dto_slot {
	/* device-CBFN offset */
	int64_t offset_bfn_ns;

	/* device-AXM offset */
	int64_t offset_cpm_ns;
};

#define RBS_TIME_DTOSIZ	32

/* device time offset */
struct rbs_time_cpm1_dto {
	uint32_t ptr;
	uint32_t status;

	/* Note, we don't protect delay updates because
	 * we can do it "atomically" (32-bit) and delays are
	 * changed only when line rate is changed (should not be
	 * any packets) */
	int32_t ingress_delay_ns;
	int32_t egress_delay_ns;

	struct rbs_time_cpm1_dto_slot slots[2];
};

struct rbs_time_cpm1_offset {
	uint32_t ptr;
	uint32_t status;
	uint8_t pad[252];
	struct rbs_time_cpm1_offset_slot slots[2];
	/* device time offsets */
	struct rbs_time_cpm1_dto dto[RBS_TIME_DTOSIZ];
};

/*
 * Create single shot timer
 */
#define RBS_TIMEIOC_CREATESST	_IOR('t', 5, int)

/*
 * Single shot timer operations
 */
#define RBS_SSTIOC_CANCEL	_IOW('S', 101, int)
#define RBS_SSTIOC_WAIT		_IOW('S', 102, uint64_t)

/*
 * TS ports
 */

/* TS port types */
#define RBS_TIMETSPORT_NTP		0x1
#define RBS_TIMETSPORT_PTP_EPHY		0x2
#define RBS_TIMETSPORT_PTP_C2I		0x4
#define RBS_TIMETSPORT_PTP_C2I_TP	0x8
#define RBS_TIMETSPORT_PTP		(RBS_TIMETSPORT_PTP_EPHY)

/* TS port statistics */
struct rbs_time_ts_port_stats {
	uint32_t rx_ptp;
	uint32_t rx_ptp_no_ets;
	uint32_t rx_ptp_not_evt; /* obsolete */
	uint32_t rx_cpmts;

	uint32_t tx_ptp;
	uint32_t tx_ptp_ts;
	uint32_t tx_ptp_match;
	uint32_t tx_ptp_pktdrop;
	uint32_t tx_ptp_tsdrop;
	uint32_t tx_cpmts;
};

struct rbs_time_ts_port_pktstamp {
	uint32_t flags;
	uint32_t ptp_msgtype;
	uint32_t ptp_msgseq;
	uint64_t tstamp;
};

struct rbs_time_ts_ioc_add_port {
	uint32_t chan; /* in */
	uint32_t type; /* in */
	uint32_t offset_location; /* in */
	uint32_t pidx; /* out */
};

struct rbs_time_ts_ioc_rm_port {
	uint32_t chan; /* in */
	uint32_t type; /* in */
};

struct rbs_time_ts_ioc_getstats {
	uint32_t chan; /* in */
	struct rbs_time_ts_port_stats stats; /* out */
};

struct rbs_time_ts_ioc_pktstamp {
	uint32_t pidx; /* in */
	struct rbs_time_ts_port_pktstamp stamp; /* in */
};

#define RBS_TIMEIOC_ADD_PORT	_IOWR('t', 10, struct rbs_time_ts_ioc_add_port)
#define RBS_TIMEIOC_RM_PORT	_IOW('t', 11, struct rbs_time_ts_ioc_rm_port)
#define RBS_TIMEIOC_GETSTATS	_IOWR('t', 12, struct rbs_time_ts_ioc_getstats)
#define RBS_TIMEIOC_PKTSTAMP	_IOW('t', 13, struct rbs_time_ts_ioc_pktstamp)

#define RBS_TIMEVERSION_2	2

/* PTP modes */
#define RBS_TIMEPTPMODE_ETHER	0x1
#define RBS_TIMEPTPMODE_UDP4	0x2
#define RBS_TIMEPTPMODE_UDP6	0x4

/* C2I port magic */
#define RBS_TIMEC2I_MAGIC_PID(_v)	(((_v) >> 8) & 0x7f)
#define RBS_TIMEC2I_MAGIC_RND_MASK	0xf0f700ff
#define RBS_TIMEC2I_MAGIC_STC		0x07088000
#define RBS_TIMEC2I_MAGIC_STC_MASK	0x0f088000

/* add C2I port */
struct rbs_time_ts_ioc_add_c2i_port {
	uint32_t port_id; /* in */
	uint32_t offset_location; /* in */
	uint32_t mac_chan; /* in */
	uint32_t tp_mac_chan; /* in */
	uint32_t magic_rnd; /* in */
	uint8_t port_mac[6]; /* in */

	uint32_t reserved[16]; /* zero */
};
#define RBS_TIMEIOC_ADD_C2I_PORT	_IOWR('t', 20, \
		struct rbs_time_ts_ioc_add_c2i_port)

/* remove C2I port */
struct rbs_time_ts_ioc_rm_c2i_port {
	uint32_t port_id; /* in */

	uint32_t reserved[16]; /* zero */
};
#define RBS_TIMEIOC_RM_C2I_PORT		_IOW('t', 21, \
		struct rbs_time_ts_ioc_rm_c2i_port)

/* add port (extended version) */
struct rbs_time_ts_ioc_add_port_e {
	uint32_t chan; /* in */
	uint32_t type; /* in */
	uint32_t offset_location; /* in */
	uint32_t pidx; /* out */
	uint32_t ptp_mode; /* in */

	uint32_t reserved[16];
};

#define RBS_TIMEIOC_ADD_PORT_E		_IOWR('t', 22, \
		struct rbs_time_ts_ioc_add_port_e)

/* set port PTP mode */
struct rbs_time_ts_ioc_set_ptpmode {
	uint32_t chan; /* in */
	uint32_t ptp_mode; /* in */

	uint32_t reserved[16];
};

#define RBS_TIMEIOC_SET_PTPMODE		_IOW('t', 23, \
		struct rbs_time_ts_ioc_set_ptpmode)

#endif /* UAPI_RBS_TIME_H__ */

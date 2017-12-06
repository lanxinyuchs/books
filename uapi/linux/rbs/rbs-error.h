#ifndef UAPI_RBS_ERROR_H__
#define UAPI_RBS_ERROR_H__

/* Error sources */
#define RBS_ERRSRC_SYS		0x00000000
#define RBS_ERRSRC_CBF		0x00000100
#define RBS_ERRSRC_DUXI		0x00000101
#define RBS_ERRSRC_SRIO		0x00000102
#define RBS_ERRSRC_PL		0x00000103
#define RBS_ERRSRC_WD		0x00000104

#define RBS_SYS_RESTART		0x00000001

/* flags */
#define RBS_ERRENT_COMMITTED	0x1
#define RBS_ERRENT_BINDATA	0x2
#define RBS_ERRENT_TRUNCATED	0x4

typedef unsigned long long rbs_error_seq_t;

struct rbs_error {
	rbs_error_seq_t seq;
	short size; /* error data size */
	short cpu;
	unsigned int source;
	unsigned int code;
	unsigned long long ts; /* timestamp */
	unsigned short flags;
	unsigned short dropped; /* num. of entries dropped */
	/* followed by error data */
};

#define rbs_error_data(_error_ptr) ((void *) ((_error_ptr) + 1))

/*
 * IOCTL
 */

/* get error buffer state */
struct rbs_error_state {
	int recovered;
	int entry_size;
	rbs_error_seq_t restart_seq; /* last entry from previous restart */
};
#define RBS_ERRIOC_STATE	_IOR('e', 0, struct rbs_error_state)

/* acknowledge entry(ies) */
#define RBS_ERRACK_THIS		0
#define RBS_ERRACK_TO		1
struct rbs_error_ack {
	int what; /* see RBS_ERRACK_XXX */
	rbs_error_seq_t seq;
};
#define RBS_ERRIOC_ACK		_IOW('e', 1, struct rbs_error_ack)

/* read entry */
#define RBS_ERRREAD_THIS	0
#define RBS_ERRREAD_NEXT	1
struct rbs_error_read {
	int what; /* see RBS_ERRREAD_XXX */
	rbs_error_seq_t seq;
	struct rbs_error error;
	/* here should be enough space to hold entry data */
};
#define RBS_ERRIOC_READ		_IOWR('e', 2, struct rbs_error_read)

#endif /* UAPI_RBS_ERROR_H__ */

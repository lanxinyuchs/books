#ifndef ULH_CM_H__
#define ULH_CM_H__

#include <stdint.h>
#include <itc.h>
#include <stdbool.h>
#include <ulh_transport.h>
#include <ulh_timer.h>

struct ulh_cm;

typedef enum ulh_cm_info {
	CM_HEADING = 0,
	CM_SUMMARY,
	CM_DETAILED,
	CM_THROUGHPUT
} ulh_cm_info;

typedef enum lnh_cm_act {
	LNH_THROUGHPUT_ENABLE,
	LNH_THROUGHPUT_DISABLE,
	LNH_THROUGHPUT_UPDATE
} lnh_cm_act_on;

struct ulh_cm_msghdr {
	uint32_t src;
	uint32_t dst;
	uint32_t size;
};

struct ulh_cm_uc_ops {
	void (*uc_connected)(void *);
	void (*uc_disconnected)(void *);
	/* note, itc_msg is consumed */
	void (*uc_delivery)(void *, struct ulh_cm_msghdr *,
			union itc_msg *);
};

struct ulh_cm_dc_ops {
	int (*dc_init)(void *, struct ulh_cm_uc_ops *, void *, uint32_t);
	int (*dc_fini)(void *, uint32_t);
	int (*dc_connect)(void *, uint32_t);
	int (*dc_disconnect)(void *, uint32_t);
	/* note, itc_msg is not consumed */
	int (*dc_transmit)(void *, uint32_t, struct ulh_cm_msghdr *,
			union itc_msg *);
	/* note, ulh_tbuff is consumed */
	void (*dc_receive)(void *, uint32_t, struct ulh_tbuff *);
        int (*dc_info)(void *, ulh_cm_info, char *, int);
        void (*dc_config)(void *, lnh_cm_act_on);
};

struct ulh_cm_instance {
	struct ulh_cm_dc_ops *ops;
	void *instance;
	/* internal */
	void *cm_ref;
};

struct ulh_cm_config {
	uint32_t cfg_size;
	uint32_t cfg_version;
	uint32_t cid; /* transport connection */
	unsigned long uref; /* UC ref */
	itc_mbox_id_t mbox; /* UC mbox */
};

int ulh_cm_create_instance(const char *cm, const char *name,
		struct ulh_cm_instance *instance,
		struct ulh_cm_config *config, struct ulh_timerqueue *tqueue);
int ulh_cm_destroy_instance(struct ulh_cm_instance *);


/* ................... */
struct ulh_cm_ops {
	int (*create_instance)(void *, const char *, struct ulh_cm_instance *,
			struct ulh_cm_config *, struct ulh_timerqueue *tqueue);
	int (*destroy_instance)(void *, struct ulh_cm_instance *);

	void (*destroy)(void *);
};

int ulh_cm_init(void);
int ulh_cm_register(const char *name, struct ulh_cm_ops *ops, void *param);
int ulh_cm_unregister(const char *name);

#endif /* ULH_CM_H__ */

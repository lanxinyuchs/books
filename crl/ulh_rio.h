#ifndef ULH_RIO_H__
#define ULH_RIO_H__

#include <stdint.h>
#include <linux/types.h>
#include <linux/rbs/rbs-rio-cm.h>

#define ULH_RIO_MTU 0x10000

struct ulh_cm_rio_config {
        struct ulh_cm_config    cmn;
	struct sriocm_ioctl_dbg dbg;
        __u16                   peer_id;
        __u8                    mbox;
        __u8                    letter;
        __u8                    channel;
        __u8                    src_mac[6];
        __u8                    dst_mac[6];
	char                    ifname[16];
};

int ulh_rio_init(const char *name);

#endif /* !ULH_RIO_H__ */

#ifndef UAPI_AF_RBS_H__
#define UAPI_AF_RBS_H__

#include <linux/if_ether.h>

/* note, AF_DECnet is taken firstly because it's unlikely that we will need it
 * in RBS and secondly - support of DECnet in the kernel is discontinued */
#define AF_RBS	12 /* AF_DECnet */
#define PF_RBS	AF_RBS

/* protocols */
#define RBS_PROTO_SYNC		1
#define RBS_PROTO_TNPORT	2
#define RBS_PROTO_BB_DSP	3
#define RBS_PROTO_ERR		4
#define RBS_PROTO_TIME		5
#define RBS_PROTO_GPIO		6
#define RBS_PROTO_RIO		7 /* ETH_P_WAN_PPP - NIC disguised as Ethernet device */
#define RBS_PROTO_ECPX		8
#define RBS_PROTO_GPAMUX	9
#define RBS_PROTO_ICMCTRL	10
#define RBS_PROTO_RIOCM		11
#define RBS_PROTO_IOMAP 	12
#define RBS_PROTO_SMEM		13
#define RBS_PROTO_MPORT		14
#define RBS_PROTO_RIO_MSG	15 /* ETH_P_MOBITEX - NIC disguised as Ethernet device */
#define RBS_PROTO_SYS		16
#define RBS_PROTO_DHAI		17
#define RBS_PROTO_DHAI_UNIT	18 /* ..33 */
#define RBS_PROTO_HWL		34
#define RBS_PROTO_MTDPART	35

#define RBS_PROTO_MAX		36


#endif /* UAPI_AF_RBS_H__ */

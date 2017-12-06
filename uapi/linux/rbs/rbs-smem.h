#ifndef UAPI_RBS_SMEM_H__
#define UAPI_RBS_SMEM_H__

/*
 * RBS SMEM interface
 *
 * This interface is somewhat similar to CPP MSI interface.
 *
 * Usecases for userspace application:
 * - Allocate physically contigous memory region that survives application
 * restart (actually surives any type of restart that doesn't restart
 * the kernel)
 * - Allocate semi-persistent physically contigous memory region that survives
 * all type of restarts, except power-on restart
 *
 * Userspace application should use libsmem library to access this
 * functionality
 *
 * Usecases for kernel drivers:
 * - Allocate semi-persistent physically contigous memory region
 *
 */

#define RBS_SMEM_RGNNAMESIZE	16

/* flags */
#define RBS_SMEM_PERSIST	0x00000001
#define RBS_SMEM_ZERO		0x00000002

/*
 * IOCTL
 */

/* allocate region */
struct rbs_smem_alloc {
	char name[RBS_SMEM_RGNNAMESIZE]; /* in */
	unsigned int size; /* in */
	int flags; /* in */
	int recovered; /* out */
	unsigned long long base; /* out */
};
#define RBS_SMEMIOC_ALLOC	_IOWR('s', 0, struct rbs_smem_alloc)

#endif /* UAPI_RBS_SMEM_H__ */

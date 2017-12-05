#ifndef RBS_SMEM_H__
#define RBS_SMEM_H__

#include <uapi/linux/rbs/rbs-smem.h>

/**
 * rbs_smem_alloc_kernel
 * @name - name of the region
 * @size - size, should be page aligned
 * @addr - base address of the allocated region
 * @flags -
 *
 * Return 0 - new region allocated
 *        1 - region has been recovered
 *        <0 - error
 */
extern int rbs_smem_alloc_kernel(const char *name, phys_addr_t size,
		void **addr, int flags);

#endif /* RBS_SMEM_H__ */

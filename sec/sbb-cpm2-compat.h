#ifndef RBS_SBB_COMPAT_H__
#define RBS_SBB_COMPAT_H__

#include <asm/compat.h>

union ioc_params;
typedef union ioc_params ioc_params_t;

struct sbb_compat_sdo_set {
	compat_uptr_t src_p;
	compat_uptr_t dst_p;
	uint32_t data_length;
	uint32_t sdo_length;  /* output */
	uint32_t owner_info_hi;
	uint32_t owner_info_lo;
	uint16_t object_info;
	uint16_t srk_index;
	uint8_t  iv[SDO_IV_LENGTH];
};

struct sbb_compat_sdo_get {
	compat_uptr_t src_p;
	compat_uptr_t dst_p;
	uint32_t data_length;
	uint32_t sdo_length;
	uint32_t owner_info_hi;
	uint32_t owner_info_lo;
	uint16_t object_info; /* output */
};

struct sbb_compat_img_verify {
	compat_uptr_t img_p;
	uint32_t img_length;
	uint32_t failed; /* output, 0 = verify OK, 1 = verify NOK */
};

#endif /* RBS_SBB_COMPAT_H__ */

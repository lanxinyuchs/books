/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef NVPI3_IO
#define NVPI3_IO

#include <stdint.h>
#include "nvpi3_common.h"
#include "nvpi3_cfg_common.h"

nvpi3_result_t nvpi3_get_storage_size(const union nvpi3_db_storage *storage,
                                      uint32_t *size);

nvpi3_result_t nvpi3_read(void *buf, uint32_t size,
                          const union nvpi3_db_storage *db_storage);

nvpi3_result_t nvpi3_write(const void *buf, uint32_t size,
                           const union nvpi3_db_storage *db_storage);

#endif /* NVPI3_IO */

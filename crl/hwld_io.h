/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef HWLD_IO__
#define HWLD_IO__

#include <stdint.h>
#include <mtd/mtd-user.h>

#define ERASED_FLASH_CONTENT (0xff)

typedef struct {
	int (*init)(char *dev, mtd_info_t *pinfo);
	int (*eraselog)(int fd, uint32_t erasesize, uint32_t size);
	int (*erasesector)(int fd, uint32_t start, uint32_t erasesize);
} hwld_io;

extern hwld_io hwld_io_mtd;
extern hwld_io hwld_io_file;

#endif // HWLD_IO__

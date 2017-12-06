/*
 * ICM control socket API
 *
 * Copyright 2013 Ericsson AB
 * Andrey Panteleev <andrey.xx.panteleev@ericsson.com>
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
  *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#ifndef UAPI_RBS_ICMCTRL_H__
#define UAPI_RBS_ICMCTRL_H__

#define RBS_ICMCTRL_CTRLPINS	0x001 /* ctrl pins available */
#define RBS_ICMCTRL_SPI		0x002 /* spi available */
#define RBS_ICMCTRL_PCIPRFAIL	0x004 /* PCI probe failed */
#define RBS_ICMCTRL_PCIREADY	0x008 /* PCI ready */
#define RBS_ICMCTRL_PERIPHERAL	0x010 /* peripeheral loaded (io-ring) */
#define RBS_ICMCTRL_FABRIC	0x020 /* fabric loaded */
#define RBS_ICMCTRL_USER	0x040 /* user mode */

/* load */
struct rbs_icmctrl_ioc_load {
	int mode; /* 0=ps; 1=cvp */ /* in */
	int fd; /* in */
};
#define RBS_ICMCTRLIOC_LOAD	_IOR('i', 0, struct rbs_icmctrl_ioc_load)

#define RBS_ICMCTRLIOC_GETSTATE	_IOW('i', 1, int)

#define RBS_ICMCTRLIOC_PCISCAN	_IOR('i', 2, int)


#endif /* UAPI_RBS_ICMCTRL_H__ */


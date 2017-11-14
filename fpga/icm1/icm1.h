/*
 * ICM1 - ICM1 Driver
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

#ifndef ICM1_H__
#define ICM1_H__

/*
 * CVP 
 */
struct cvp_dev;

struct cvp_dev *icm1_cvp_init(struct pci_dev *pdev, void __iomem *wr_addr);
int icm1_cvp_send(struct cvp_dev *cvp, char *buf, int len);
int icm1_cvp_check_status(struct cvp_dev *cvp);
int icm1_cvp_finish(struct cvp_dev *cvp, int abort);

#endif /* ICM1_H__ */


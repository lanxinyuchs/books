/*
 * HWL socket API
 *
 * Copyright (C) 2015 Ericsson
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef UAPI_RBS_HWL_H__
#define UAPI_RBS_HWL_H__

/* --------------------------------------------------------------------------
 *   CONSTANT DEFINITIONS
 * --------------------------------------------------------------------------
 */

/**
 * Interface info
 */
#define RBS_HWL_ID_SZ           (3)   /* Not null terminated */
#define RBS_HWL_TIMEDATE_SZ     (12)  /* Not null terminated */
#define RBS_HWL_MESSAGE_SZ      (111) /* Not null terminated */

/* --------------------------------------------------------------------------
 *   TYPE DEFINITIONS
 * --------------------------------------------------------------------------
 */

/* Old format
**    4        14        3
** ! seq ! date time ! logID + ; + free text !
**
** 9/15517-CSX10179 entry format
**      1             1            12         3         111
** ! adm state ! filter value ! date time ! log ID ! free text !
*/

#define RBS_HWLMODE_NONBLOCK		(0x0)
#define RBS_HWLMODE_BLOCKING		(0x1)

#define RBS_HWLMODE_MSG_FILTER(_n)	((_n << 1) & 0x7F)
#define RBS_HWL_FILTER_LOGID(_n)	((_n) & 0xFFFF)
#define RBS_HWL_FILTER_MSG(_n, _m)	\
	RBS_HWL_FILTER_LOGID(_n) | (1 << 16 | (((_m) << 18) & (0x7F << 18)))
#define RBS_HWL_GET_LOGID_FILTER(_n)	((_n) & 0x7F)
#define RBS_HWL_GET_MSG_FILTER(_n)	((_n >> 18) & 0x7F)

struct rbs_hwl_ioc_write {
	int  mode;
	int  filter;
	char id[RBS_HWL_ID_SZ + 1];         /* Null terminated */
	char msg[RBS_HWL_MESSAGE_SZ + 1];   /* Null terminated */
};

struct rbs_hwl_ioc_read {
	unsigned int offset;
	int filter;
	char id[RBS_HWL_ID_SZ + 1];
	char time[RBS_HWL_TIMEDATE_SZ + 1];
	char msg[RBS_HWL_MESSAGE_SZ + 1];
};

#define RBS_HWLIOC_ERASE	_IO('h', 0)
#define RBS_HWLIOC_WRITE	_IOR('h', 1, struct rbs_hwl_ioc_write)
#define RBS_HWLIOC_READ		_IOWR('h', 2, struct rbs_hwl_ioc_read)

#define RBS_HWLTIME_UNTRUSTED	0x0
#define RBS_HWLTIME_TRUSTED	0x1 /* time was synchronized with a
					valid clock source */
#define RBS_HWLIOC_TIME		_IOR('g', 3, int)


#endif /* UAPI_RBS_HWL_H__ */

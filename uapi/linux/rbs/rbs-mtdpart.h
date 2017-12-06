/*
 * MTD partition permissions socket API
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

#ifndef UAPI_RBS_MTDPART_H__
#define UAPI_RBS_MTDPART_H__

/* ---------------------------------------------------------------------------
 *   CONSTANT DEFINITIONS
 * ---------------------------------------------------------------------------
 */

#define RBS_MAX_PARTNAME_LENGTH	30

/* ---------------------------------------------------------------------------
 *   TYPE DEFINITIONS
 * ---------------------------------------------------------------------------
 */

#define RBS_MTDPART_ENABLE_WP	_IOW('m', 1, char[RBS_MAX_PARTNAME_LENGTH])
#define RBS_MTDPART_DISABLE_WP	_IOW('m', 2, char[RBS_MAX_PARTNAME_LENGTH])

#endif /* UAPI_RBS_MTDPART_H__ */

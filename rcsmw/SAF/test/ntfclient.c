/*      -*- OpenSAF  -*-
 *
 * (C) Copyright 2011 The OpenSAF Foundation
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. This file and program are licensed
 * under the GNU Lesser General Public License Version 2.1, February 1999.
 * The complete license can be accessed from the following location:
 * http://opensource.org/licenses/lgpl-license.php
 * See the Copying file included with the OpenSAF distribution for full
 * licensing terms.
 *
 * Author(s): Ericsson AB
 *
 */
/**
 *   Client command line program utilities for the NTF Service
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <getopt.h>
#include <limits.h>

#include <saNtf.h>

int get_long_digit(char *str, long *val)
{
  char *endptr;
  errno = 0;              /* To distinguish success/failure after call */
  *val = strtol(str, &endptr, 0);
  /* Check for various possible errors */
  if ((errno == ERANGE && (*val == LONG_MAX || *val == LONG_MIN))
      || (errno != 0 && *val == 0)) {
    perror("strtol");
    return 0;
  }
  if (endptr == str) {
    fprintf(stderr, "No digits were found\n");
    return 0;
  }
  if (*endptr != '\0')    /* other chars than digits */
    return 0;
  return 1;
}

void getVendorId(SaNtfClassIdT * notificationClassId)
{
  long val;
  char *p = strdup(optarg);
  char *vendorId = strtok(p, ",");
  char *majorId = strtok(NULL, ",");
  char *minorId = strtok(NULL, ",");
  if (NULL == vendorId || NULL == majorId || NULL == minorId) {
    fprintf(stderr, "notificationClassId wrong format\n");
    exit(EXIT_FAILURE);
  }
  if (get_long_digit(vendorId, &val)) {
    notificationClassId->vendorId = (SaUint32T) val;
  } else {
    fprintf(stderr, "notificationClassId vendorId wrong format\n");
    exit(EXIT_FAILURE);
  }
  if (get_long_digit(majorId, &val) && (0 <= val && val <= USHRT_MAX)) {
    notificationClassId->majorId = (SaUint16T) val;
  } else {
    fprintf(stderr, "notificationClassId majorId wrong format\n");
    exit(EXIT_FAILURE);
  }
  if (get_long_digit(minorId, &val) && (0 <= val && val <= USHRT_MAX)) {
    notificationClassId->minorId = (SaUint16T) val;
  } else {
    fprintf(stderr, "notificationClassId minorId wrong format\n");
    exit(EXIT_FAILURE);
  }
  free(p);
}

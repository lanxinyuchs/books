/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2012. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *  Purpose : SAF common functions
 * ----------------------------------------------------------------------
 *
 */

#include <sys/time.h>

#include "saAis.h"
#include "safc_ais_common.h"

SaTimeT safc_current_time(void)
{
  struct timeval currentTime;

  gettimeofday(&currentTime, 0);

  return (unsigned)currentTime.tv_sec * 1000000000ULL +
    (unsigned)currentTime.tv_usec * 1000ULL;
}

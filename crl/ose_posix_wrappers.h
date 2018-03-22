/**
 * Copyright Ericsson AB 2009 - All Rights Reserved
 *
 * No part of this software may be reproduced in any form without the written
 * permission of the copyright owner.
 */

/**
 * This file includes macros and functions that wrap POSIX like OSE functions
 * to correct POSIX prototypes.
 *
 * Use this by including this file in OSE program where OSE's POSIX like
 * functions are used. This will automatically recognize if we are in POSIX
 * environment and won't redefine functions if we are compiling for OSE.
 *
 * #include <ose_posix_wrappers.h>
 */

#ifndef _LITS_OSE_POSIX_WRAPPERS_H
#define _LITS_OSE_POSIX_WRAPPERS_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <string.h>
#include <time.h>
#include <assert.h>

#ifdef _POSIX_SOURCE

static inline
char* litswrap_ctime_r(const time_t* time, char* buffer, size_t buf_size)
{
    assert(buf_size >= 26);
    return ctime_r(time, buffer);
}

#define ctime_r(time, buffer, buf_size) litswrap_ctime_r(time, buffer, buf_size)

#endif /* _POSIX_SOURCE */

/* There is no strlcpy in GNU libc. */
static inline
size_t litswrap_strlcpy(char *destination, const char *source, size_t dest_size)
{
    size_t src_size = strlen(source);
    size_t copy_size = dest_size;
    if (src_size < copy_size)
    {
        copy_size = src_size;
    }
    if (copy_size > 0)
    {
        strncpy(destination, source, copy_size);
        destination[copy_size - 1] = 0;
    }
    return src_size;
}

#define strlcpy(destination, source, size) \
    litswrap_strlcpy(destination, source, size)

#ifdef  __cplusplus
}
#endif

#endif /* _LITS_OSE_POSIX_WRAPPERS_H */

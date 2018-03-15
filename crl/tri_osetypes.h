/**
 *   @COPYRIGHT
 *   COPYRIGHT (C) 2015 BY ERICSSON AB. ALL RIGHTS RESERVED. THE
 *   INFORMATION IN THIS DOCUMENT IS THE PROPERTY OF ERICSSON. EXCEPT
 *   AS SPECIFICALLY AUTHORIZED IN WRITING BY ERICSSON, THE RECEIVER
 *   OF THIS DOCUMENT SHALL KEEP THE INFORMATION CONTAINED HEREIN
 *   CONFIDENTIAL AND SHALL PROTECT THE SAME IN WHOLE OR IN PART FROM
 *   DISCLOSURE AND DISSEMINATION TO THIRD PARTIES. DISCLOSURE AND
 *   DISSEMINATIONS TO THE RECEIVER'S EMPLOYEES SHALL ONLY BE MADE ON
 *   A STRICT NEED TO KNOW BASIS.
 */

/**
 *   @FILE TRI_OSETYPES.H
 *   THE CONTENTS OF THIS FILE DECLARES OSE TYPES USED WHEN USING TRI
 *   WITHOUT LITS SUPPORT.
 */

/* ========================================================================
 *   HISTORY OF DEVELOPMENT:
 *   -----------------------
 *   REVISED : 2015-11-04 ANETTE SCHÖTT
 *   CHANGE  : FIRST VERSION.
 * ========================================================================
 */

#ifdef NO_LITS

#ifndef TRI_OSETYPES_H
#define TRI_OSETYPES_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

typedef enum { False = 0, True = 1 } Boolean;
typedef uint32_t U32;
typedef uint16_t U16;
typedef uint8_t U8;
typedef uint32_t SIGSELECT;

#ifdef __cplusplus
}
#endif

#endif /* TRI_OSETYPES_H */

#endif /* NO_LITS */

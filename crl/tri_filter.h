/**
 *   Header file for TRI filter functionality.
 *
 *   ******* THIS FILE IS TRI INTERNAL.***************
 *   ******* TRI CLIENTS SHALL NOT INCLUDE THIS HEADER FILE.******
 *
 * 
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */ 

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2014-11-25  K.V.Ranganadh
 *   Change  : First version.
 * ========================================================================
 */
#ifndef __TRI_FILTER_H 
#define __TRI_FILTER_H


#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdbool.h>
#include <stdint.h>

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
/*
** The different kind of symbols in an infix or RPN expression
*/
typedef enum
{
  TOK_NONE,
  TOK_NULL,
  TOK_NUM,
  TOK_LEN,
  TOK_LEFT_BRACKET,
  TOK_RIGHT_BRACKET,
  TOK_LEFT_PAR,
  TOK_RIGHT_PAR,
  TOK_COLON,
  TOK_RANGE,
  TOK_AND,
  TOK_OR,
  TOK_LESS,
  TOK_GRT,
  TOK_LESS_EQU,
  TOK_GRT_EQU,
  TOK_EQU,
  TOK_NOT_EQU,
  TOK_ELEMENT
} tokKindE;

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/*
** Structure containing information during compilation of an expression
*/
struct parseInfoS
{
  char    *str;    /* Pointer to the next character in the input buffer      */
  int      tok;    /* Last token found in the getNext function               */
  int      value;  /* The numeric value of the last token if it was a number */
  int      level;  /* Number of recursive calls                              */
  int     *rpn_p;  /* Pointer to last position in the rpn expression         */
};

/** ==================================================================== */
/*
**   This Function compiles the filter request recieved from busLogFilter
**   and compiled resultant expression will be stored in compiledFilter.
**
**   @param busLogFilter   : Holds the filter requested
**          compiledFilter : Holds the buffer of compiled filte
** 
**   @return  True  :If filter requested is supported
**            False : If filter requested is incompatible to compile
**
**
*/
/** ==================================================================== */
Boolean compileFilter(char *busLogFilter,int *compiledFilter);

#ifdef __cplusplus
}
#endif
#endif /* __TRI_FILTER_H */

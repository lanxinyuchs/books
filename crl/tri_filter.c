/**
 *   Filter functions to handle te filter command
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
 *   Revised : 2014-11-24 K.V.Ranganadh
 *   Change  : support for filter handling
 *
 *   Revised : 2015-01-14 Nils Carlson
 *   Change  : Fix function loop without header involvement.
 *
 *   Revised : 2015-09-09 Anette Schött
 *   Change  : Add inclusion of tri_osetypes.h to be used when not using
 *             LITS.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#ifdef NO_LITS
#include <tri_osetypes.h>
#else
#include <osetypes.h>
#endif
#include <stdint.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <malloc.h>
#include <errno.h>
#include <syslog.h>
#include <pthread.h>
#include <ctype.h>
#include "tri_filter.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/*
** Maximum number of recursive calls to the expr function
*/
#define MAX_LEVEL      5

/*
** Maximum length of a token symbol
*/
#define MAX_TOK_LEN    4

/*
** Macro to insert an element in an rpn buffer
*/
#define PUSH(RPN_P,I)  (*(RPN_P)++ = (I))

/*
** Break the function loop
*/
static Boolean expr1(struct parseInfoS *p);


/* ===================================================================== */
/*
**   Get next token from input buffer.
**
** @param : structure which holds all the parsing information
**
** @return:
**   The token found.
**
**  @par Globals : -
*/
/* ===================================================================== */
static int
getNext(
  struct parseInfoS *p)  /* I: Pointer to current parse information */
{
   char     str[MAX_TOK_LEN + 1];
   int      kind = TOK_NONE;
   char    *s    = p->str;
   int      val  = 0;
   int      i    = 0;

  /*
  ** Skip spaces
  */
  while (*s == ' ')
  {
    s++;
  }

  /*
  ** Check next character in input buffer
  */
  switch (*s++)
  {
    case 0:
      kind = TOK_NULL;
      break;

    case '(':
      kind = TOK_LEFT_PAR;
      break;

    case ')':
      kind = TOK_RIGHT_PAR;
      break;

    case '[':
      kind = TOK_LEFT_BRACKET;
      break;

    case ']':
      kind = TOK_RIGHT_BRACKET;
      break;

    case ':':
      kind = TOK_COLON;
      break;

    case '=':
      kind = TOK_EQU;
      break;

    case '<':
      if (*s == '=')                /* Check for '<=' */
      {
        kind = TOK_LESS_EQU;
        s++;
      }
      else if (*s == '>')        /* or '<>' */
      {
        kind = TOK_NOT_EQU;
        s++;
      }
      else
      {
        kind = TOK_LESS;
      }
      break;

    case '>':
      if (*s == '=')                 /* Check for '>=' */
      {
        kind = TOK_GRT_EQU;
        s++;
      }
      else
      {
        kind = TOK_GRT;
      }
      break;

    case '.':
      if (*s == '.')                 /* Check for '..' */
      {
        kind = TOK_RANGE;
        s++;
      }
      break;

    case '$':
      if (isxdigit(*s))
      {
        kind = TOK_NUM;
        val = 0;
        do
        {
          val = val * 16 + *s - '0';
          if (*s >= 'a')
          {
            val = val - 'a' + '9' + 1;
          }
          else if (*s >= 'A')
          {
            val = val - 'A' + '9' + 1;
          }
          s++;
        } while (isxdigit(*s));
      }
      break;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      kind = TOK_NUM;
      val = *(s - 1) - '0';

      while (*s >= '0' && *s <= '9')         /* Collect all digits */
      {
        val = val * 10 + *s - '0';
        s++;
      }
      break;

    case 'A':                                /* 'AND' */
    case 'O':                                /* 'OR' */
    case 'L':                                /* 'LEN' */
      str[i++] = *(s - 1);
      while (i < MAX_TOK_LEN && *s >= 'A' && *s <= 'Z')
      {
        str[i++] = *s++;
      };
      str[i] = 0;
      if (strcmp(str, "AND") == 0)
      {
        kind = TOK_AND;
      }
      else if (strcmp(str, "OR") == 0)
      {
        kind = TOK_OR;
      }
      else if (strcmp(str, "LEN") == 0)
      {
        kind = TOK_LEN;
      }
      break;

    default:
      break;
  }

  /*
  ** Update parse information
  */
  p->tok = kind;
  p->value = val;
  p->str = s;
  return kind;
}

/* ===================================================================== */
/*
**   Parses an element reference.
**
** @param : structure which holds all the parsing information
**
** @return:
**   True if successful, else False.
**
**  @par Globals : -
*/
/* ===================================================================== */
static Boolean
element(
  struct parseInfoS *p)  /* I: Pointer to current parse information */
{
  int bitNo;

  if (getNext(p) != TOK_NUM)
  {
    return False;
  }

  /*
  ** Save index
  */
  PUSH(p->rpn_p, p->value);

  /*
  ** Check for ':' or ']'
  */
  switch (getNext(p))
  {
    case TOK_RIGHT_BRACKET:
      PUSH(p->rpn_p, 0);    /* Start bit */
      PUSH(p->rpn_p, 7);    /* Stop bit  */
      (void)getNext(p);
      return True;

    case TOK_COLON:
      break;

    default:
      return False;
  }

  /*
  ** Check start bit number
  */
  switch (getNext(p))
  {
    case TOK_NUM:
      bitNo = p->value;
      break;

    default:
      return False;
  }

  /*
  ** Check for '..' or ']'
  */
  switch (getNext(p))
  {
    case TOK_RIGHT_BRACKET:
      PUSH(p->rpn_p, bitNo);
      PUSH(p->rpn_p, bitNo);
      (void)getNext(p);
      return True;

    case TOK_RANGE:
      break;

    default:
      return False;
  }

  /*
  ** Check stop bit number
  */
  switch (getNext(p))
  {
    case TOK_NUM:
      if (bitNo <= p->value)
      {
        PUSH(p->rpn_p, bitNo);
        PUSH(p->rpn_p, p->value);
      }
      else
      {
        PUSH(p->rpn_p, p->value);
        PUSH(p->rpn_p, bitNo);
      }
      break;

    default:
      return False;
  }

  /*
  ** Check for ']'
  */
  switch (getNext(p))
  {
    case TOK_RIGHT_BRACKET:
      (void)getNext(p);
      return True;

    default:
      return False;
  }
}

/* ===================================================================== */
/*
**   Parses a factor.
**
** @param: structure which holds all the parsing data
**
** @return:
**   True if successful, else False.
**
**
**   @par Globals: -
*/
/* ===================================================================== */
static Boolean
factor(
  struct parseInfoS *p)  /* I: Pointer to current parse information */
{

  switch (p->tok)
  {
    case TOK_LEN:
      PUSH(p->rpn_p, (int)TOK_LEN);
      (void)getNext(p);
      return True;

    case TOK_LEFT_BRACKET:
      PUSH(p->rpn_p, (int)TOK_ELEMENT);
      return element(p);

    default:
      return False;
  }
}

/* ===================================================================== */
/*
**   Parses a simple term.
**
** @param: structure which holds all the parsing data
**
** @return:
**   True if successful, else False.
**
**
**  @par Globals : -
*/
/* ===================================================================== */
static Boolean
simpleTerm(struct parseInfoS *p)  /* I: Pointer to current parse information */
{
   int op;

   if (p->tok == TOK_LEFT_PAR)
   {
      (void)getNext(p);
      if ((Boolean)expr1(p) && (p->tok == TOK_RIGHT_PAR))
      {
        (void)getNext(p);
        return True;
      }
      else
      {
        return False;
      }
   }
   else
   {
      if (! factor(p))
      {
        return False;
      }
      switch (p->tok)
      {
        case TOK_LESS:
        case TOK_GRT:
        case TOK_LESS_EQU:
        case TOK_GRT_EQU:
        case TOK_EQU:
        case TOK_NOT_EQU:
          op = p->tok;
          break;

        default:
          return False;
      }

      if (getNext(p) == TOK_NUM)
      {
        PUSH(p->rpn_p, (int)TOK_NUM);
        PUSH(p->rpn_p, p->value);
        PUSH(p->rpn_p, (int)op);
        (void)getNext(p);
        return True;
      }
      else
      {
        return False;
      }
   }
}

/* ===================================================================== */
/*
**   Parses a term.
**
** @param : structure which holds all the parsing information
**
** @return:
**   True if successful, else False.
**
**  @par Globals : -
*/
/* ===================================================================== */
static Boolean
term(struct parseInfoS *p)  /* I: Pointer to current parse information */
{

  if (! simpleTerm(p))
  {
    return False;
  }

  while (p->tok == TOK_AND)
  {
    (void)getNext(p);
    if (! simpleTerm(p))
    {
      return False;
    }
    PUSH(p->rpn_p, (int)TOK_AND);
  }

  return True;
}

/* ===================================================================== */
/* 
**   Parses a complete expression.
**
**  @param Pointer to current parse information
**
**  @return:
**   True if successful, else False.
**
**  @par Globals : -
*/
/* ===================================================================== */
static Boolean expr1(struct parseInfoS *p)
{

   if (++p->level > MAX_LEVEL)
   {
     return False;
   }

   if (! term(p))
   {
     return False;
   }

   while (p->tok == TOK_OR)
   {
     (void)getNext(p);
     if (! term(p))
     {
       return False;
     }
     PUSH(p->rpn_p, (int)TOK_OR);
   }

   return True;
}

/* ===================================================================== */
/** 
 *   Compiles the filter request
 * 
 *   @param busLogFilter   : Holds the filter requested
 *          compiledFilter : Holds the buffer of compiled filter
 * 
 *   @return     True  : If compilation of filter is successful
 *               False : If requested filter is unsupported
 *
 *   @par Globals: -
 */
/* ===================================================================== */
Boolean compileFilter(char *busLogFilter, int *compiledFilter)
{
    struct parseInfoS p;
    Boolean           result;

    p.str   = busLogFilter;
    p.level = 0;
    p.rpn_p = compiledFilter;
    (void)getNext(&p);

    result = (Boolean)(expr1(&p) && p.tok);

    if (result)
    {
      PUSH(p.rpn_p, 0);
    }

    return result;
}

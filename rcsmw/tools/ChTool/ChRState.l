%{
/* ----------------------------------------------------------------------
 * %CCaseFile:	ChRState.l %
 * %CCaseRev:	/main/3 %
 * %CCaseDate:	2012-10-11 %
 * %CCaseDocNo:	38/190 55-LXA 119 334 Ux %
 * Author:	etxbjca
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	DPcsrc.c %
 * %CCaseTemplateId: 21/002 01-FEA 202 714 Ux, Rev: /main/2 %
 *
 * Copyright (C) 2001 by Ericsson Telecom AB
 * S - 126 25  STOCKHOLM
 * SWEDEN, tel int + 46 8 719 0000
 *
 * This program may be used and/or copied only with the written permission
 * from Ericsson Telecom AB, or in accordance with the terms and
 * conditions stipulated in the agreement/contract under which the program
 * has been supplied.
 *
 * All rights reserved.
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * main/x     01-06-13   etxbjca     Created
 * main/3     071210     etxbjca     Bug fixed for P-state
 * main/4     2012-10-11 etxbjca     R-state num extended to 999, 
 *                                   Ver-state extended to 9999
 * ----------------------------------------------------------------------
 */


#include <stdio.h>

/* Buffer that will contain suggestions for next preliminary or              */
/* ane approved R-state values. (Initial setting)                            */

/* Temporary buffer                                                          */
char tmpBuf[1024];

/******************************************************************************
** R-state types (Used internally)
**
** Type:                Description:                                 Example:
** NONE_T               "none"
**                       Not really a valid R-state value but can
**                       be given to get initial R-state values.
** RSTATE_N_T           R-state value with only numbers (digits),    R1
** RSTATE_C_T           R-state value with only characters           RA
** RSTATE_NC_T          R-state value with numbers and characters    R1A
** RSTATE_NCN_T         R-state value with numbers, characters       R1A01
**                       and numbers
** RSTATE_C_SUF_T       R-state value with characters and a suffix   RA/1
** RSTATE_N_SUF_T       R-state value with numbers and a suffix      R1/A
** RSTATE_NC_SUF_T      R-state value with numbers, characters       R1A/A
**                       and suffix
** RSTATE_NCN_SUF_T     R-state value with numbers, characters       R1A01/1
**                       numbers and suffix
** PREL_NC_T            Preliminary R-state value with numbers       P1A
**                       and one character
** PREL_CN_T,           Preliminary R-state value with characters    PA1
**                       and one number
** BAD_RSTATE_T         Invalid R-state value
******************************************************************************/
typedef enum
  {
    NONE_T,
    RSTATE_N_T,
    RSTATE_C_T,
    RSTATE_NC_T,
    RSTATE_NCN_T,
    RSTATE_C_SUF_T,
    RSTATE_N_SUF_T,
    RSTATE_NC_SUF_T,
    RSTATE_NCN_SUF_T,
    PREL_NC_T,
    PREL_CN_T,
    BAD_RSTATE_T
  } CMRStateType;

/* Initial setting of R-state value type                                     */
static CMRStateType rStateType = BAD_RSTATE_T;
%}

RSTATE_NUM         [0-9]
RSTATE_CHAR        [ABCDEFGHJKLMNSTUVXYZ]
INV_RSTATE_CHAR    [IOPQRW]

RSTATE_NUMS        {RSTATE_NUM}|({RSTATE_NUM}{RSTATE_NUM})|({RSTATE_NUM}{RSTATE_NUM}{RSTATE_NUM})
RSTATE_CHARS       {RSTATE_CHAR}|({RSTATE_CHAR}{RSTATE_CHAR})

SUF                ({RSTATE_NUMS})|{RSTATE_CHAR}

RSTATE_N           R({RSTATE_NUMS})
RSTATE_C           R({RSTATE_CHARS})
RSTATE_NC          R({RSTATE_NUMS})({RSTATE_CHARS})
RSTATE_NCN         R({RSTATE_NUMS})({RSTATE_CHARS})({RSTATE_NUM}{RSTATE_NUM})|R({RSTATE_NUMS})({RSTATE_CHARS})({RSTATE_NUM}{RSTATE_NUM}{RSTATE_NUM})|R({RSTATE_NUMS})({RSTATE_CHARS})({RSTATE_NUM}{RSTATE_NUM}{RSTATE_NUM}{RSTATE_NUM})

RSTATE_N_SUF       R({RSTATE_NUMS})\/({SUF})
RSTATE_C_SUF       R({RSTATE_CHARS})\/({SUF})
RSTATE_NC_SUF      R({RSTATE_NUMS})({RSTATE_CHARS})\/({SUF})
RSTATE_NCN_SUF     R({RSTATE_NUMS})({RSTATE_CHARS})({RSTATE_NUM}{RSTATE_NUM})\_({RSTATE_NUMS})

PREL_NC            P({RSTATE_NUMS})({RSTATE_CHARS})
PREL_C             P({RSTATE_CHARS}){RSTATE_NUM}

%%

........           { return 1; }
R....\/.           { return 2; }
R..[A-Z][A-Z]      { return 3; }
P....              { return 4; }

[\n\t ]            { rStateType = BAD_RSTATE_T;    return setNextRState(); }
{RSTATE_N}         { rStateType = RSTATE_N_T;      return setNextRState(); }
{RSTATE_C}         { rStateType = RSTATE_C_T;      return setNextRState(); }
{RSTATE_NC}        { rStateType = RSTATE_NC_T;     return setNextRState(); }
{RSTATE_NCN}       { rStateType = RSTATE_NCN_T;    return setNextRState(); }
{RSTATE_N_SUF}     { rStateType = RSTATE_N_SUF_T;  return setNextRState(); }
{RSTATE_C_SUF}     { rStateType = RSTATE_C_SUF_T;  return setNextRState(); }
{RSTATE_NC_SUF}    { rStateType = RSTATE_NC_SUF_T; return setNextRState(); }
{RSTATE_NCN_SUF}   { rStateType = RSTATE_NCN_SUF_T;return setNextRState(); }
{PREL_NC}          { rStateType = PREL_NC_T;       return setNextRState(); }
{PREL_C}           { rStateType = PREL_CN_T;       return setNextRState(); }

.*                 { return 5; }

%%
int yywrap()
{}

/******************************************************************************
** char getValidChar(char c)
**
** Description:
** Returns the next valid R-state character if possible.
** Invalid characters are I,O,P,Q,R,W.
**
** Arguments:
** c          Character from which this function will try to find
**            next valid one.
**
** Returns:
** The next valid character if possible,
** otherwise it will return FALSE (0).
******************************************************************************/
char getValidChar(c)
char c;
{
  int found = 0;
  while (!found)
    switch(++c)
      {
      case 'I':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'W':
	/* Oops! Reserved character, try next */
	break;
      default:
	/* Found a character, not among the reserved ones! */
	found = 1;
	break;
      }

  /* Verify that the character is in upper case, otherwise return FALSE (0). */
  return (isupper(c) ? c : 0);
}

/******************************************************************************
** int setNextRState()
**
** NOTE!
** This function is quite complex. Sorry about that, but
** the rules regarding product R-states are really complicated and
** full of exceptions.
**
** Description:
** Initializes
**  nextPrelRState  (next preliminary R-state value)
**  nextAppRState   (next approved R-state value)
** based upon the values in
**  yytext          (provided R-state value)
**  rStateType      (type of the provided R-state value)
** if possible.
**
** Arguments:
** None
**
** Returns:
** (0) on success
** (1) on failure
******************************************************************************/
int setNextRState()
{
  int status = 0;

    switch (rStateType)
      {
      case NONE_T:
      case RSTATE_N_T:
      case RSTATE_C_T:
      case RSTATE_NC_T:
      case RSTATE_NCN_T:
      case RSTATE_C_SUF_T:
      case RSTATE_N_SUF_T:
      case RSTATE_NC_SUF_T:
      case RSTATE_NCN_SUF_T:
      case PREL_NC_T:
      case PREL_CN_T:
	status = 0;
	break;
      case BAD_RSTATE_T:
	status = 1;
	break;
      }
  return status;
}

int main(argc, argv)
int argc;
char* argv[];
{
  int status;
  status = yylex();

  if ( status )
    printf("ERROR\n");
  else
    printf("OK\n");

  return (rStateType != BAD_RSTATE_T ? 0 : 1);
}

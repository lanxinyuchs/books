%{
/* ----------------------------------------------------------------------
 * %CCaseFile:	ChRev.l %
 * %CCaseRev:	/main/5 %
 * %CCaseDate:	2014-06-18 %
 * %CCaseDocNo:	39/190 55-LXA 119 334 Ux %
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
 * main/6     01-06-13   etxbjca     Created
 * main/2     2011-05-11 etxbjca     Changed, "U" is an allowed character
 * ----------------------------------------------------------------------
 */

#include <stdio.h>
#include <string.h>

char nextPrelRev[16]  = "NO_PREL_REV";
char nextAppRev[16]   = "NO_APP_REV";
char tmpBuf[1024];

/******************************************************************************
** Revision types (Used internally)
**
** Type:                Description:                                 Example:
** NONE_T               "none"
**                       Not really a valid revision value but can
**                       be given to get initial revision values.
** ORIG_REV_T           Original document revision                   A,BA,ABC, ABCD
** SPEC_DOC_T           Special document version                     A1, A22
** SPEC_SPEC_DOC_T      Special (special) document version           A22B
** LOC_BASE_DOC_T       Local document version                       A/1, A/1B
** PREL_REV_T           Preliminary document version                 PB3
** PREL_REV_T2          Preliminary document version                 PB3/2
** BAD_REV_T            Invalid revision value
******************************************************************************/
typedef enum
  {
    NONE_T,
    ORIG_REV_T,
    SPEC_DOC_T,
    SPEC_SPEC_DOC_T,
    LOC_BASE_DOC_T,
    PREL_REV_T,
    PREL_REV_T2,
    BAD_REV_T
  } CMRevType;

static CMRevType revType = BAD_REV_T;
%}

REV_NUM            [0-9]
REV_CHAR           [ABCDEFGHJKLMNSTUVXYZ]
INV_REV_CHAR       [IOPQRW]
NOT_P              [^P]


ONE_NUM            {REV_NUM}
TWO_NUM            {REV_NUM}{REV_NUM}
TREE_NUM           {REV_NUM}{REV_NUM}{REV_NUM}
FOUR_NUM           {REV_NUM}{REV_NUM}{REV_NUM}{REV_NUM}

ORIG_REV           {REV_CHAR}|({REV_CHAR}{REV_CHAR})|({REV_CHAR}{REV_CHAR}{REV_CHAR})|({REV_CHAR}{REV_CHAR}{REV_CHAR}{REV_CHAR})
SPEC_DOC           ({ORIG_REV})({REV_NUM}|{REV_NUM}{REV_NUM})
SPEC_SPEC_DOC      ({SPEC_DOC}){REV_CHAR}
BASE_DOC           ({ORIG_REV})|({SPEC_DOC})|({SPEC_SPEC_DOC})
LOC_BASE_DOC       ({BASE_DOC})\/({REV_NUM}|{REV_NUM}{REV_CHAR})
PREL_REV           P({ORIG_REV})({REV_NUM})
PREL_REV_PLUS      P({ORIG_REV})({ONE_NUM}|{TWO_NUM}|{TREE_NUM}|{FOUR_NUM})
PREL_REV2          ({PREL_REV})\/({REV_NUM}|{REV_NUM}{REV_NUM})

%%

[\n\t ]            { revType = BAD_REV_T;        return setNextRev(); }
{NOT_P}......      { return 1; }
P.......           { return 2; }
{ORIG_REV}         { revType = ORIG_REV_T;       return setNextRev(); }
{SPEC_DOC}         { revType = SPEC_DOC_T;       return setNextRev(); }
{SPEC_SPEC_DOC}    { revType = SPEC_SPEC_DOC_T;  return setNextRev(); }
{LOC_BASE_DOC}     { revType = LOC_BASE_DOC_T;   return setNextRev(); }
{PREL_REV}         { revType = PREL_REV_T;       return setNextRev(); }
{PREL_REV_PLUS}    { revType = PREL_REV_T;       return setNextRevPlus(); }
{PREL_REV2}        { revType = PREL_REV_T2;      return setNextRev(); }
.*                 { return 3;;  }

%%
int yywrap()
{}

/******************************************************************************
** char getValidChar(char c)
**
** Description:
** Returns the next valid revision character if possible.
** Invalid characters are I,O,P,Q,R,U,W.
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
	break;
      default:
	found = 1;
	break;
      }

  return (isupper(c) ? c : 0);
}

/******************************************************************************
** int setNextRev()
**
** NOTE!
** This function is quite complex. Sorry about that, but
** the rules regarding product revision are really complicated and
** full of exceptions.
**
** Description:
** Suggests new preliminary and approved revision values based upon
** the given revision value.
** Initializes
**  nextPrelRev  (next preliminary revision value)
**  nextAppRev   (next approved revision value)
** based upon the values in
**  yytext       (provided revision value)
**  revType      (type of the provided revision value)
** if possible.
**
** Arguments:
** None
**
** Returns:
** (0) on success
** (1) on failure
******************************************************************************/
int setNextRev()
{
  int tmpInt;
  int status = 0;
  char* tmpP_1;
  char* tmpP0;
  char* tmpP1;
  char* tmpP2;
  char* tmpP3;
  char* tmpP4;

  strcpy(tmpBuf, yytext);
  tmpP_1 = &tmpBuf[yyleng + 1];
  tmpP0  = &tmpBuf[yyleng];
  tmpP1  = &tmpBuf[yyleng - 1];
  tmpP2  = &tmpBuf[yyleng - 2];
  tmpP3  = &tmpBuf[yyleng - 3];
  tmpP4  = &tmpBuf[yyleng - 4];

  if (yyleng == 4)
    {
      if (isupper(*tmpP1) && isupper(*tmpP2) && isupper(*tmpP3) && isupper(*tmpP4))
	{
	  /* Step the character if possible (three characters at the end) */
	  if (*tmpP1 != 'Z')
	    *tmpP1 = getValidChar(*tmpP1);
	  else if (*tmpP2 != 'Z')
	    {
	      *tmpP2 = getValidChar(*tmpP2);
	      *tmpP1 = 'A';
	    }
	  else if (*tmpP3 != 'Z')
	    {
	      *tmpP3 = getValidChar(*tmpP3);
	      *tmpP2 = 'A';
	      *tmpP1 = 'A';
	    }
	  else if (*tmpP4 != 'Z')
	    {
	      *tmpP4 = getValidChar(*tmpP4);
	      *tmpP3 = 'A';
	      *tmpP2 = 'A';
	      *tmpP1 = 'A';
	    }
	  else
	    status = 1;
	}
      else 
	status = 1;
	
    }
  else if (yyleng == 3)
    {
      if (isupper(*tmpP1) && isupper(*tmpP2) && isupper(*tmpP3))
	{
	  /* Step the character if possible (three characters at the end) */
	  if (*tmpP1 != 'Z')
	    *tmpP1 = getValidChar(*tmpP1);
	  else if (*tmpP2 != 'Z')
	    {
	      *tmpP2 = getValidChar(*tmpP2);
	      *tmpP1 = 'A';
	    }
	  else if (*tmpP3 != 'Z')
	    {
	      *tmpP3 = getValidChar(*tmpP3);
	      *tmpP2 = 'A';
	      *tmpP1 = 'A';
	    }
	  else
	    {
	      *tmpP3='A';
	      *tmpP2='A';
	      *tmpP1='A';
	      *tmpP0='A';
	      *tmpP_1='\0';
	    }
	}
      else 
	status = 1;
    }
  else if (yyleng == 2)
    {
      if (isdigit(*tmpP1) && isdigit(*tmpP2))
	{
	  /* Step the number if possible (two digits at the end) */
	  if (*tmpP1 != '9')
	    (*tmpP1)++;
	  else if (*tmpP2 != '9')
	    {
	      (*tmpP2)++;
	      *tmpP1='0';
	    }
	  else if (*tmpP2 == '9' && revType == PREL_REV_T || revType == PREL_REV_T2)
	    status = 2;
	  else
	    status = 1;
	}
      else if (isdigit(*tmpP1) && !isdigit(*tmpP2))
	{
	  /* Step the number if possible (one digits at the end) */
	  if (*tmpP1 != '9')
	    (*tmpP1)++;
	  else if (revType == SPEC_DOC_T || revType == PREL_REV_T ||
			revType == PREL_REV_T2)
	    {
	      /* Only alowed to extend to two digits for            */
	      /* special document versions and preliminary versions */
	      *tmpP1='1';
	      *tmpP0='0';
	      *tmpP_1='\0';
	    }
	  else
	    status = 1;
	}
      else if (isupper(*tmpP1) && isupper(*tmpP2))
	{
	  /* Step the character if possible (two characters at the end) */
	  if (*tmpP1 != 'Z')
	    *tmpP1 = getValidChar(*tmpP1);
	  else if (*tmpP2 != 'Z')
	    {
	      *tmpP2 = getValidChar(*tmpP2);
	      *tmpP1 = 'A';
	    }
	  else
	    {
	      *tmpP2='A';
	      *tmpP1='A';
	      *tmpP0='A';
	      *tmpP_1='\0';
	    }
	}
      else if (isupper(*tmpP1) && !isupper(*tmpP2))
	{
	  /* Step the character if possible (one character at the end) */
	  /* It is not a original document version so two the revision */
	  /* can not be extended to two characters.                    */
	  if (*tmpP1 != 'Z')
	    *tmpP1 = getValidChar(*tmpP1);
	  else
	    status = 1;
	}
    }
  else if (yyleng == 1)
    {
      /* Only one characters, step up */
      if (isupper(*tmpP1))
	{
	  if (*tmpP1 != 'Z')
	    *tmpP1 = getValidChar(*tmpP1);
	  else
	    {
	      *tmpP1='A';
	      *tmpP0='A';
	      *tmpP_1='\0';
	    }
	}
    }
  else
    status = 1;

  /* Suggest preliminary and approved revision values */
  if (status == 0)
    switch (revType)
      {
      case NONE_T:
	strcpy(nextPrelRev, "PA1");
	strcpy(nextAppRev,  "A");
	break;
      case ORIG_REV_T:
	sprintf(nextAppRev, "%s", tmpBuf);
	sprintf(nextPrelRev, "P%s1", tmpBuf);
	break;
      case PREL_REV_T2:
      case PREL_REV_T:
	sprintf(nextPrelRev, "%s", tmpBuf);
	/* Remove P and the number at the end for the approved version */
	strcpy(nextAppRev, strtok(&tmpBuf[1], "0123456789"));
	break;
      default:
	/* Preliminary versions only allowed for original document versions */
	sprintf(nextAppRev, "%s", tmpBuf);
	sprintf(nextPrelRev, "%s", tmpBuf);
	break;
    }
  else if (status == 2)
    {
      strcpy(nextAppRev, strtok(&tmpBuf[1], "0123456789"));
      status = 0;
    }

  return status;
}

int setNextRevPlus()
{
  char *P;
  char appRev[16];
  int  num;
  int  i;

  strcpy(tmpBuf, yytext);
  P = strtok(tmpBuf,"P0123456789");
  strcpy(appRev,P);

  strcpy(tmpBuf, yytext);
  i = 0;
  while(!isdigit(tmpBuf[i]))
    i++;

  sscanf(&tmpBuf[i],"%d",&num);

  sprintf(nextPrelRev, "P%s%d",appRev, num+1);
  sprintf(nextAppRev, "%s", appRev);
  
  return(0);
}



int main(argc, argv)
int argc;
char* argv[];
{
  int status;
  status = yylex();

  if ( revType != BAD_REV_T )
    printf("%s:%s\n", nextPrelRev, nextAppRev);
  else
    printf("Error in Revision!\n");

  return (revType != BAD_REV_T ? 0 : 1);
}

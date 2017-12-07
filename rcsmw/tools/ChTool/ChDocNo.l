%{
/* ----------------------------------------------------------------------
 * %CCaseFile:	ChDocNo.l %
 * %CCaseRev:	/main/2 %
 * %CCaseDate:	2011-09-21 %
 * %CCaseDocNo:	35/190 55-LXA 119 334 Ux %
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
 * main/2      01-06-13   etxbjca     Created
 * ----------------------------------------------------------------------
 */

#include <stdio.h>

%}

REV_NUM         [0-9]
REV_CHAR        [ABCDEFGHJKLMNSTVXYZ]
PREL_CHAR       [P]
INV_REV_CHAR    [IOPQRUW]

NUM             [0-9]
CH              [ABCDEFGHJKLMNSTVXYZ]

REG_INST_PX     {CH}|({CH}{CH})|({CH}{CH}{CH})
REG_INST_SX     {CH}|({CH}{CH})
REG_INST        ({REG_INST_PX})|(({REG_INST_PX})\/({REG_INST_SX}))

CLASS_PX        {NUM}|({NUM}{NUM})|({NUM}{NUM}{NUM})|({NUM}{NUM}{NUM}{NUM})|({NUM}{NUM}{NUM}{NUM}{NUM})|(\%CPX)
DEC_CLASS       ({NUM}{NUM}{NUM}{NUM})|({NUM}{NUM}{NUM}\ {NUM}{NUM})

PX_DC           ({DEC_CLASS})|(({CLASS_PX})\/({DEC_CLASS}))

IND_NR          \%PNR
DOC_NO1         (({PX_DC})|(({REG_INST})\ ({PX_DC})))\-{IND_NR}
DOC_NO          ({PX_DC})|(({REG_INST})\ ({PX_DC}))

DOC_VAR1        ({REV_CHAR}|({REV_CHAR}{REV_CHAR})){NUM}
DOC_VAR         ({DOC_VAR1}|({PREL_CHAR}{DOC_VAR1}))

LANG            (U..)|(U.)
DOC_NO2         (({DOC_NO1})|(({DOC_NO1})\-({DOC_VAR})))\ ({LANG})

%%

{DOC_NO}                 { return 0; }
[\n\t ]                  { return 1; }
.*                       { return 1; }

%%
int yywrap()
{}

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

  return status;
}

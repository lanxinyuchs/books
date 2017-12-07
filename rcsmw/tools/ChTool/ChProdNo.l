%{
/* ----------------------------------------------------------------------
 * %CCaseFile:	ChProdNo.l %
 * %CCaseRev:	/main/2 %
 * %CCaseDate:	2011-09-21 %
 * %CCaseDocNo:	37/190 55-LXA 119 334 Ux %
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
 * main/4      01-06-13   etxbjca     Created
 * ----------------------------------------------------------------------
 */


#include <stdio.h>

%}

SC              [ABCDEFGHJKLMNSTWXYZ0123456789]

NUM             [0-9]
CH              [A-Z]

CLASS           {CH}{CH}{CH}
ORIG            {CH}|({CH}{CH})
TYPENO          {NUM}{NUM}{NUM}
IX              ({NUM}{NUM})|({NUM}{NUM}{NUM})|({NUM}{NUM}{NUM}{NUM})
PX1             {CH}|({CH}{CH})|({CH}{CH}{CH})
PX2             {NUM}|({NUM}{NUM})|({NUM}{NUM}{NUM})
PX              ({PX1})|({PX2})
SX              {SC}|({SC}{SC})|({SC}{SC}{SC})|({SC}{SC}{SC}{SC})|({SC}{SC}{SC}{SC}{SC})

TYPE            (({CLASS})\ ({TYPENO}))|(({CLASS})({ORIG})\ ({TYPENO}))
BASE_NO         ({TYPE})\ ({IX})
PROD_NO_PX      ({PX})\/({BASE_NO})
PROD_NO_SX      ({BASE_NO})\/({SX})
PROD_NO_PXSX	({PX})\/({BASE_NO})\/({SX})

PROD_NO_BASE	(({CLASS})([+]?))|(({TYPE})([+]?))|(({BASE_NO})([+]?))|(({PROD_NO_PX})([+]?))|(({PROD_NO_SX})([+]?))|(({PROD_NO_PXSX})([+]?))

%%
[\n\t ]				{ return 1; }
.......................		{ return 1; }
{PROD_NO_BASE}			{ return 0; }
.*				{ return 1; }

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

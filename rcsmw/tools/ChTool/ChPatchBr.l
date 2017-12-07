%{
/* ----------------------------------------------------------------------
 * %CCaseFile:	ChPatchBr.l %
 * %CCaseRev:	/main/2 %
 * %CCaseDate:	2011-09-21 %
 * %CCaseDocNo:	36/190 55-LXA 119 334 Ux %
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
 * main/3      01-06-13   etxbjca     Created
 * ----------------------------------------------------------------------
 */

#include <stdio.h>

%}

INC_NUM            [1-6]
INC_ILL            [7-9]
DEL_NUM            [1-9]
ONE                [1]
ZERO               [0]
ALPH_CHAR          [A-Za-z]
CHAR_CHAR          [._-]
ILL_SEP            [_-]
STR_ARR            [A-Za-z0-9_.-]*
STR_NUM            [0-9]*

ORIG_PATCH_BR      Patch_
ILL_IND            ({ZERO}|{CHAR_CHAR}|{ALPH_CHAR})
ILL_INC1           ({ILL_IND}|{INC_ILL})
ILL_INC            ({ILL_INC1})|({ILL_INC1}{STR_ARR})
ILL_DEL            ({ILL_IND}|{ILL_IND}{STR_ARR})
OK_DEL             ({DEL_NUM}|{DEL_NUM}({ZERO}|{DEL_NUM}))
ILL_DIV1           ({STR_NUM}|{ALPH_CHAR})
ILL_DIV            ({ILL_DIV1}|{ILL_DIV1}{STR_ARR})
ILL_COMB1          ({INC_NUM}{ILL_SEP}{OK_DEL}|{INC_NUM}{ILL_SEP}{OK_DEL}{CHAR_CHAR}{STR_ARR})
ILL_COMB2          ({INC_NUM}\.{DEL_NUM}{ILL_DIV}|{INC_NUM}\.{ILL_DEL})
ILL_COMB3          ({ILL_INC})
PATCH_NUM          ({INC_NUM}\.{OK_DEL})
BASE_PATCH_BR      ({ORIG_PATCH_BR}{PATCH_NUM})
PATCH_BRANCH       ({BASE_PATCH_BR}|{BASE_PATCH_BR}{CHAR_CHAR}{STR_ARR})
ILL_BRANCH1        ({ORIG_PATCH_BR}{ILL_COMB1})
ILL_BRANCH2        ({ORIG_PATCH_BR}{ILL_COMB2})
ILL_BRANCH3        ({ORIG_PATCH_BR}{ILL_COMB3})

%%

{PATCH_BRANCH}     { return 0; }
[\n\t ]            { return 1; }
{ILL_BRANCH1}      { return 1; }
{ILL_BRANCH2}      { return 2; }
{ILL_BRANCH3}      { return 3; }
.*                 { return 4; }

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

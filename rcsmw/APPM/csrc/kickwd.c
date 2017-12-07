/* ----------------------------------------------------------------------
 * %CCaseFile:	kickwd.c %
 * %CCaseRev:	/main/R5A/2 %
 * %CCaseDate:	2016-04-01 %
 * %CCaseDocNo: %
 * Author: Arto Nummelin
 *
 * Short description:
 * This file contains a small port program 
 * to kick the EE watchdog when ordered from MW
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2016 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R5A/1      2016-03-31 etxarnu     Created
 * ----------------------------------------------------------------------
 */
#include <stdio.h>
#include <unistd.h>

typedef unsigned char byte;

/* ------------erl_comm------------------------ */
int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

/* -------------- kick ---------------------- */
byte kick(FILE *fp)
{
  if ( fputc('x',fp) == EOF)
    return -1;
  fflush(fp);
  return 0;
}

/* -------------- main prog ---------------------- */
int main( int argc, char * argv [] ) {
  if (argc != 2)
    return -1;

  FILE *wdfile = fopen( argv[1], "w" );

  int fn, res;
  byte buf[100];
  
 
  while (read_cmd(buf) > 0) {
    fn = buf[0];
    
    if (fn == 1) {
      res = kick(wdfile);
    } else
      res = -1;

    buf[0] = res;
    write_cmd(buf, 1);
  }
}



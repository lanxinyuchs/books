/**
 *   TRI test program for interface trace item macros.
 * 
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-06-05 Anette Schött
 *   Change  : First version.
 * ========================================================================
 */
#include "ose.h"
#include "osetypes.h"

#define CPP_SIGNAL (0x0000ab)
typedef struct
{
   SIGSELECT  sigNo;
}CppSignal;


void  myIfFunc(void);


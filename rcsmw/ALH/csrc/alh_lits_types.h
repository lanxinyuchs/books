/*
 * 
 * Copyright (c) Ericsson AB  2014-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 */
 
#ifndef _ALH_LITS_TYPES_H_
#define _ALH_LITS_TYPES_H_
#include <stdint.h>

typedef uint8_t  U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef enum { False = 0, True = 1 } Boolean;

#define CELLO_MAX_PRODUCT_NUMBER_LEN   25
#define CELLO_MAX_PRODUCT_REVISION_LEN  8
#define CELLO_MAX_PRODUCT_NAME_LEN     13
#define CELLO_MAX_PRODUCT_DATE_LEN      9
#define CELLO_MAX_SERIAL_NUMBER_LEN    14
#define CELLO_AVLI_MAX_PRODUCT_NAME_LEN     (33)

typedef struct 
{
  U8 productNumber[CELLO_MAX_PRODUCT_NUMBER_LEN];
  U8 productRevision[CELLO_MAX_PRODUCT_REVISION_LEN];
  U8 productName[CELLO_MAX_PRODUCT_NAME_LEN];
  U8 productDate[CELLO_MAX_PRODUCT_DATE_LEN];
  U8 serialNumber[CELLO_MAX_SERIAL_NUMBER_LEN];
} Cello_PidInHW;

/* typedef struct  */
/* { */
/*   U8 productNumber[CELLO_MAX_PRODUCT_NUMBER_LEN]; */
/*   U8 productRevision[CELLO_MAX_PRODUCT_REVISION_LEN]; */
/*   U8 productName[CELLO_AVLI_MAX_PRODUCT_NAME_LEN]; */
/*   U8 productDate[CELLO_MAX_PRODUCT_DATE_LEN]; */
/*   U8 serialNumber[CELLO_MAX_SERIAL_NUMBER_LEN]; */
/* } Cello_Avli_PidInHW; */

typedef struct
{
   U32             smn;
   U32             apn; 
   U32             ern;
} CelloPiuHwAddr;

typedef struct 
{
  U8 productNumber[CELLO_MAX_PRODUCT_NUMBER_LEN];
  U8 productRevision[CELLO_MAX_PRODUCT_REVISION_LEN];
  U8 productName[CELLO_MAX_PRODUCT_NAME_LEN];
  U8 productDate[CELLO_MAX_PRODUCT_DATE_LEN];
}Cello_PidInSW;

#endif /* _ALH_LITS_TYPES_H_ */


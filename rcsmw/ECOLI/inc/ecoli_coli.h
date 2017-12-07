/*
 *
 * Copyright (c) Ericsson AB 2014-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#ifndef ECOLI_COLI_H
#define ECOLI_COLI_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

  /********** EXPORTED TYPES ****************/

#define COLI_SERVER_MBOX    "ecoli_coli"


  typedef uint32_t ColiResult;
#define COLI_RESULT_OK              0
#define COLI_RESULT_NO_FRU_EXISTS   1
#define COLI_RESULT_OTHER_ERROR    99
#define COLI_RESULT_OK_CONTINUE   100


#ifdef __cplusplus
}
#endif

#endif   /* ECOLI_COLI_H */

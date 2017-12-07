/*
 * Copyright (c) Ericsson AB 2017 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

/**
 * @file
 * @brief Header file to glmslib.
 *
 * This headerfile holds the function prototype to start the
 * GLMS component from the GLMS C-library.
 */


#ifndef GLMS_H
#define GLMS_H

#ifdef __cplusplus
extern "C"
{
#endif

/*
 ******************************************************************************
 * FUNCTION PROTOTYPES
 ******************************************************************************
 */

/**
 * @brief Start GLMS server.
 *
 * The GLMS server will be started.
 *
 * @return Function will not return
 */
void runGlms(void);

#ifdef __cplusplus
}
#endif
#endif /* GLMS_H */

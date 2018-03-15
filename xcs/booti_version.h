/* > Description **************************************************************/
/**
 * @file booti_version.h
 * @brief BOOT interface version
 *
 * This file defines the BOOT interface version.
 */

/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef BOOTI_VERSION_H
#define BOOTI_VERSION_H

/* > Includes *****************************************************************/

/* > Defines ******************************************************************/

/**
 * Boot interface major version. Stepped when the interface is NOT backwards
 * compatible.
 */
#define BOOTI_MAJOR_VERSION 1

/**
 * Boot interface minor version. Stepped when the interface is backwards
 * compatible but have been extended.
 */
#define BOOTI_MINOR_VERSION 0

/* > Type Declarations ********************************************************/

/* > Function Declarations ****************************************************/


#endif /* BOOTI_VERSION_H */

#ifdef __cplusplus
}
#endif

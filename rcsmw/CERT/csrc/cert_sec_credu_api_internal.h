#ifndef CERT_SEC_CREDU_API_INTERNAL
#define CERT_SEC_CREDU_API_INTERNAL

/* ----------------------------------------------------------------------
 * %CCaseFile:	cert_sec_credu_api_internal.h %
 * %CCaseRev:	/main/R11A/3 %
 * %CCaseDate:	2017-09-29 %
 * %CCaseDocNo: %
 * Author:	ekurnik
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2017 All rights reserved.
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
 * R11A/1    2017-09-06   ekurnik     Created
 * R11A/2    2017-09-14   estjako     Small change in define
 * R11A/3    2017-09-29   eivmiha     const char to char in SecCreduTrustedCertificate, SecCreduTrustCategory
 * ----------------------------------------------------------------------
 */

#include <stdint.h>
#include <sys/types.h>

/********** IMPORT ************************/

#ifdef __cplusplus
extern "C" {
#endif

#include "sec_credu_api.h"
#include "cec.h"

/********** EXPORTED TYPES ****************/

const char *CEC_REGISTERED_NAME = "SEC_CREDU_API";

#define SEC_CREDU_BASE 0x18A4ED0
#define SEC_CREDU_REQ_SIG SEC_CREDU_BASE
#define SEC_CREDU_RESP_SIG SEC_CREDU_BASE + 1
#define SEC_CREDU_EVENT_SIG SEC_CREDU_BASE + 2

typedef struct SecCreduTrustedCertificate SecCreduTrustedCertificate;

/* Request type */
typedef enum {
    INITIALIZE = 1,
    FINALIZE = 2,
    SELECTION_OBJECT_GET = 3,
    NC_SUBSCRIBE = 4,
    NC_UNSUBSCRIBE = 5,
    NC_CERT_GET = 6,
    NC_KEY_GET = 7,
    TCAT_SUBSCRIBE = 8,
    TCAT_UNSUBSCRIBE = 9,
    TCAT_GET = 10
} SecCreduReqType;

/* Response status */
typedef enum  {
    OK = 0,
    ERROR_ID_NOT_FOUND = 1,
    ERROR_SUB_ID_NOT_FOUND = 2,
    ERROR_MO_REF_NOT_FOUND = 3,
    ERROR_NC_NOT_INSTALLED = 4,
    ERROR_TCAT_EMPTY = 5,
    ERROR_UNKNOWN_REQ_TYPE = 6
} SecCreduResponseStatus;


typedef enum  {
    NC_EVENT = 1,
    TCAT_EVENT = 2,
    FINALIZE_EVENT = 3
} SecCreduEventType;

typedef struct {
    uint32_t ID;
    cec_handle_t *handle; // TBD
    SecCreduNodeCredentialChangeCallback nodeCredentialCallback;
    SecCreduTrustCategoryChangeCallback trustCategoryCallback;
} SecCreduHandleStruct;

/* typedef in sec_credu_api.h*/
struct SecCreduTrustCategory {
    char *dirname;
    uint32_t trustedCertificateCount;
    SecCreduTrustedCertificate **trustedCertificates;
};

struct SecCreduTrustedCertificate {
    char *id;
    char *filename;
    char *pemContent;
};


#ifdef __cplusplus
}
#endif

#endif   /* CERT_SEC_CREDU_API_INTERNAL */

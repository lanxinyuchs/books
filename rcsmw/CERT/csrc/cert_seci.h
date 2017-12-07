/* ----------------------------------------------------------------------
 * %CCaseFile:	cert_seci.h %
 * %CCaseRev:	/main/R3A/R5A/R6A/R7A/2 %
 * %CCaseDate:	2016-10-03 %
 * %CCaseDocNo: %
 * Author:      
 * Author: Andreas Töyrä, anddreas.toyra@ericsson.com
 *
 * Short description:
 * SECI, Definition of the C interface between CS and TN
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
 * Rev        Date        Name        What
 * -----      -------     --------    -----------------------------------
 * R3A/1      2014-10-05  etxasta     Created
 * R3A/5      2014-11-27  etxasta     Added seci_read, seci_write and
 *                                    seci_delete
 * R3A/6      2014-12-03  etxasta     Added seci_log
 * R3A/7      2014-12-04  etxasta     Add P for sec_log parameters
 * R3A/8      2014-12-05  etxasta     Add SECI_SEVERITY_DEBUG
 * R5A        2016-01-25  ehsake      Add seci_get_vc
 * R5A/2      2016-03-24  etxasta     Add seci_get_nc and seci_get_tcat
 * R6A/1      2016-06-15  etxasta     Add seci_verify_peer
 * R7A/1      2016-09-27  etxasta     Add seci_get_vcert, seci_verify_peer_vc,
 *                                    seci_encode and seci_decode
 * ----------------------------------------------------------------------
 */

#ifndef __SECI_H
#define __SECI_H

#ifdef  __cplusplus
extern "C" {
#endif

/*
******************************************************************************
* INCLUDE FILES
******************************************************************************
*/

#include <stdint.h>
#include <stdlib.h>

/*
******************************************************************************
* MACROS
******************************************************************************
*/

/*
******************************************************************************
* TYPES
******************************************************************************
*/

typedef enum {
    /* Node Credential or Trust Category were found. Used for all functions. */
    SECI_OK = 0,
    /* No Node credential, Trust Category were found for the DN string.
       Or no data to read for secure storage.
       Used for seci_get_cert and seci_read */
    SECI_NOT_FOUND,
    /* Some error, used for all functions */
    SECI_ERROR,
    /* Result pointer was not NULL. Used for seci_get_cert and seci_read */
    SECI_INVALID_PARAMETER_RESULTP,
    /* Internal send error for the c-lib. Used for all functions. */
    SECI_SEND_ERROR,
    /* Internal receive error for the c-lib. Used for all functions. */
    SECI_RECEIVE_ERROR,
    /* Wrong identity to remove data from secure storage.
       Used only for seci_delete */
    SECI_DELETE_FAILED_WRONG_ID,
    /* Error, called on uninitialized subscription API */
    SECI_UNINITIALIZED_SUB,
    /* Peer certificate and its chain are valid.
       Only used by seci_verify_peer. */
    SECI_VERIFY_PEER_VALID,
    /* Peer certificate and its chain are NOT valid.
       Only used by seci_verify_peer. */
    SECI_VERIFY_PEER_NOT_VALID,
    /* Status unknown e.g. missing CRLs. Only used by seci_verify_peer. */
    SECI_VERIFY_PEER_UNKNOWN
} SeciResultT;

typedef enum {
    SECI_SECURITY_LOG = 0,
    SECI_AUDIT_TRAIL_LOG
} SeciLogTypeT;

typedef enum {
    SECI_SEVERITY_EMERGANCY = 0,
    SECI_SEVERITY_ALERT,
    SECI_SEVERITY_CRITICAL,
    SECI_SEVERITY_ERROR,
    SECI_SEVERITY_WARNING,
    SECI_SEVERITY_NOTICE,
    SECI_SEVERITY_INFO,
    SECI_SEVERITY_DEBUG
} SeciSeverityT;

typedef int SeciHandleT;

#define SECI_CRL_CHECK_ENABLED  0
#define SECI_CRL_CHECK_DISABLED 1

/*
******************************************************************************
* FUNCTION PROTOTYPES
******************************************************************************
*/

  /****************************************************************************
   *
   *  Name  : seci_get_cert
   *
   *  Descr.: Get node credential or trust category certificates
   *
   *  Args  : IN:  dnP          DN of Node Credential or Trust Category
   *                            Example of DN string:
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,TrustCategory=1"
   *                            or
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,NodeCredential=1"
   *                            
   *
   *          OUT: resultP      Length in bytes (integer, 4 bytes)
   *                            Data of certificate
   *                            ... and so on for each certificate
   *                            In the end there is a length (integer, 4 bytes)
   *                            telling length 0 to indicate nothing more.
   *                            So no null termination.
   *
   *                            If the DN is for a Node Credential the
   *                            certificate is the first data and the second
   *                            the private key.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_get_cert(char      *dnP,
              char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_get_vc
   *
   *  Descr.: Get vendor credentials
   *
   *  Args  : IN:  secretP      Secret identifier
   *
   *          OUT: resultP      Null terminated string buffer.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_get_vc(char      *secretP,
            char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_get_vcert
   *
   *  Descr.: Get vendor certificate
   *
   *  Args  : IN:  -
   *
   *          OUT: resultP      Null terminated string buffer.
   *                            Contains a pem file containing the
   *                            vendor certificate incl. chain.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_get_vcert(char      **resultP);

  /*************************************************************************** 
   *  Name  : seci_get_nc
   *
   *  Descr.: Get node credential, the node certificate will contain the chain.
   *          PEM format is used on the files.
   *
   *  Args  : IN:  dnP          DN of Node Credential
   *                            Example of DN string:
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,NodeCredential=1"
   *                            
   *
   *          OUT: resultP      Length in bytes (integer, 4 bytes)
   *                            Data of certificate
   *                            ... and so on for each certificate
   *                            In the end there is a length (integer, 4 bytes)
   *                            telling length 0 to indicate nothing more.
   *                            So no null termination.
   *
   *                            The Node Credential certificate is the first
   *                            data and the second the private key.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_get_nc(char      *dnP,
            char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_get_tcat
   *
   *  Descr.: Get trust category certificates and the crl check option
   *
   *  Args  : IN:  dnP          DN of Trust Category
   *                            Example of DN string:
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,TrustCategory=1"
   *                            
   *
   *          OUT: resultP      The first 4 bytes (integer) is the value of
   *                            the CRL check parameter, enabled or disabled.
   *                            SECI_CRL_CHECK_ENABLED  0
   *                            SECI_CRL_CHECK_DISABLED 1
   *
   *                            Length in bytes (integer, 4 bytes)
   *                            Data of certificate
   *                            ... and so on for each certificate
   *                            In the end there is a length (integer, 4 bytes)
   *                            telling length 0 to indicate nothing more.
   *                            So no null termination.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for
   *                            the resultP pointer.
   *
   *  Return: SeciResultT
   *
   * TODO This function needs to add the crl_check parameter as well
   ***************************************************************************/
SeciResultT
seci_get_tcat(char      *dnP,
              char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_read
   *
   *  Descr.: Read data from secure storage
   *
   *  Args  : IN:  idP          Identity string to identify user and
   *                            make index unique.
   *                            Example: "tn_ipsec"
   *               indexP       Index of the data so it can be found again.
   *                            Example: "1"
   *                            
   *          OUT: resultP      First an integer (4 bytes) telling the length
   *                            and then the data. No null termination.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_read(char      *idP,
          char      *indexP,
          char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_write
   *
   *  Descr.: Write data to secure storage
   *
   *  Args  : IN:  idP          Identity string to identify user and
   *                            make index unique.
   *                            Example: "tn_ipsec"
   *               indexP       Index of the data so it can be found again
   *                            Example: "1"
   *               dataP        Data to store, first integer (4 bytes) must be
   *                            the length of the data. No null termination.
   *                            
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_write(char      *idP,
           char      *indexP,
           char      *dataP);

  /****************************************************************************
   *
   *  Name  : seci_delete
   *
   *  Descr.: Delete data from secure storage
   *
   *  Args  : IN:  idP          Identity string to identify user and
   *                            make index unique.
   *                            Example: "tn_ipsec"
   *               indexP       Index of the data so it can be found again
   *                            Example: "1"
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_delete(char      *idP,
            char      *indexP);

  /****************************************************************************
   *
   *  Name  : seci_log
   *
   *  Descr.: Log, can be security log or audit trail log
   *
   *  Args  : IN:  type          Security log or audit trail log
   *               facility      Facility of event, 0-23 according rfc5424
   *               severity      Severity of event
   *               messageP      Event message, null terminated string
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_log(SeciLogTypeT   type,
         int            facility,
         SeciSeverityT  severity,
         char           *messageP);


  /****************************************************************************
   *
   *  Name  : seci_sub_initialize
   *
   *  Descr.: Initialize the subcription and give the SeciHandleT to poll.
   *
   *  Args  : IN:  idP          Identity string to identify user.
   *                            Null terminated string.
   *
   *  Return: SeciHandleT       If error -1 is returned.
   *
   ***************************************************************************/
SeciHandleT
seci_sub_initialize(char     *idP);

  /****************************************************************************
   *
   *  Name  : seci_add_sub
   *
   *  Descr.: Add DN string/strings to subscription. 
   *
   *  Args  : IN:  idP          Identity string to identify user.
   *                            Null terminated string.
   *
   *               dnP          DN of node credential or Trust Category.
   *                            Null terminated string.
   *                            Example of DN string:
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,TrustCategory=1"
   *                            or
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,NodeCredential=1"
   *                            
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_add_sub(char      *idP,
             char      *dnP);

  /****************************************************************************
   *
   *  Name  : seci_del_sub
   *
   *  Descr.: Remove DN string/strings from subscription.
   *
   *  Args  : IN:  idP          Identity string to identify user.
   *                            Null terminated string.
   *
   *               dnP          DN of node credential or Trust Category.
   *                            Null terminated string.
   *                            Example of DN string:
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,TrustCategory=1"
   *                            or
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,NodeCredential=1"
   *                            
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_del_sub(char      *idP,
             char      *dnP);

  /****************************************************************************
   *
   *  Name  : seci_get_sub_event
   *
   *  Descr.: Get subscription event. Call when  
   *
   *  Args  : IN:  idP          Identity string to identify user
   *
   *          OUT: resultP      Length in bytes (integer, 4 bytes)
   *                            Data of DN (MoRef)
   *                            ... and so on for each DN
   *                            In the end there is a length (integer, 4 bytes)
   *                            telling length 0 to indicate nothing more.
   *                            So no null termination.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_get_sub_event(char      *idP,
                   char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_verify_peer
   *
   *  Descr.: Verify the peer certificate and chain towards
   *          trusted certificate. Checking CRLs is included.
   *
   *  Args  : IN: dnTcatP       DN of Trust Category
   *                            Example of DN string:
   *                            "ManagedElement=1,SystemFunctions=1,SecM=1,
   *                            CertM=1,TrustCategory=1"
   *
   *          IN: peerP         Peer certificate and the chain together in one
   *                            pem binary. Length in bytes (integer, 4 bytes)
   *                            Data of the pem binary. So no null termination.
   *                            Note that the chain must be in trust order,
   *                            ending with the peer certificate.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_verify_peer(char      *dnTcatP,
                 char      *peerP);

  /****************************************************************************
   *
   *  Name  : seci_verify_peer_vc
   *
   *  Descr.: Verify the peer vendor certificate and chain towards the nodes
   *          vendor certificate.
   *
   *  Args  : IN: peerVcP       Peer vendor certificate and the chain together
   *                            in one pem binary. Null terminated.
   *                            Note that the chain must be in trust order,
   *                            ending with the peer certificate.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_verify_peer_vc(char      *peerVcP);

  /****************************************************************************
   *
   *  Name  : seci_encode
   *
   *  Descr.: Encrypt using peer public key and sign using the nodes vendor
   *          credential.
   *
   *  Args  : IN: peerVcP       Peer vendor certificate and the chain together
   *                            in one pem binary. Null termination.
   *                            Note that the chain must be in trust order,
   *                            ending with the peer certificate.
   *
   *          IN: dataLen       Data length.
   *
   *          IN: dataP         Data to encode.
   *                            
   *          OUT: resultP      Encoded data. Null terminated.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_encode(char      *peerVcP,
            int       dataLen,
            char      *dataP,
            char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_decode
   *
   *  Descr.: Verify using peer vendor certificate public key and decrypt using
   *          the nodes vendor credential.
   *
   *  Args  : IN: peerVcP       Peer vendor certificate and the chain together
   *                            in one pem binary. Null termination.
   *                            Note that the chain must be in trust order,
   *                            ending with the peer certificate.
   *
   *          IN: dataLen       Data length.
   *
   *          IN: dataP         Data to decode. Null terminated.
   *                            
   *          OUT: resultP      Decoded data. Null terminated.
   *
   *                            The memory must be freed after usage
   *                            by a call to seci_free() for the
   *                            resultP pointer.
   *
   *  Return: SeciResultT
   *
   ***************************************************************************/
SeciResultT
seci_decode(char      *peerVcP,
            int       dataLen,
            char      *dataP,
            char      **resultP);

  /****************************************************************************
   *
   *  Name  : seci_free
   *
   *  Descr.: frees the memory space pointed to by ptr.
   *          Used for seci_get_cert and seci_read result pointer  
   *
   *  Args  : IN:     ptr      A pointer to the allocated memory
   *
   ***************************************************************************/
void
seci_free(void *ptr);

#ifdef  __cplusplus
}
#endif

#endif /* __SECI_H */

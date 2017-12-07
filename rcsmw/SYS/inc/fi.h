/* ----------------------------------------------------------------------
 * %CCaseFile:	fi.h %
 * %CCaseRev:	/main/R3A/R6A/R7A/R10A/3 %
 * %CCaseDate:	2017-07-11 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: This file contains the interfaces provided by FI.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
 * R3A/7      2014-09-17 etxpejn
 * R3A/8      2015-01-07 erarafo     fiOpenWrite(), work in progress
 * R3A/9      2015-01-09 erarafo     fiOpenWrite(), further work in progress
 * R3A/10     2015-01-12 erarafo     Result code added
 * R3A/11     2015-01-12 erarafo     Result codes added
 * R6A/1      2016-05-27 eolaand     Add comment about deprecated function
 * R7A/1      2016-10-17 etxpejn     Added fiExportFile2 & fiImportFile2
 * R10A/1     2017-05-03 estjako     Added FTPES in comments
 * ----------------------------------------------------------------------
 */

#ifndef __FI_H
#define __FI_H

#ifdef  __cplusplus
extern "C" {
#endif

/*
******************************************************************************
* INCLUDE FILES
******************************************************************************
*/

#include <stdint.h>
#include <stdio.h>

/*
******************************************************************************
* MACROS
******************************************************************************
*/

#define DN_TI_SIGNAL_REVISION 0


/* This type specifies what physical media to write files to.
 * FI_VOLATILE specifies RAM disk and FI_PERSISTENT specifies
 * the solid-state drive.
 */
typedef uint32_t FiPersistencyT;
#define FI_VOLATILE                         0
#define FI_PERSISTENT                       1

/* This type specifies function return values when using the FI interface */
typedef uint32_t FiResultT;
#define FI_OK                               0
#define FI_INVALID_URI                      1
#define FI_FAILED_TO_CONNECT_TO_SFTP_SERVER 2
#define FI_FAILED_TO_IMPORT_OR_EXPORT_FILE  3
#define FI_RECEIVE_ERROR                    4
#define FI_SEND_ERROR                       5
#define FI_FAILED_TO_OPEN_FILE              6
#define FI_FAILED_TO_OBTAIN_DIRECTORY       7
#define FI_SPACE_FULL                       8
#define FI_NOT_SUPPORTED                    9
#define FI_INVALID_PARAMETER               10
#define FI_OPEN_ERROR                      11
#define FI_SFTP_SYMLINK_INVALID_NAME       12
#define FI_SFTP_SYMLINK_FAILED_TO_CREATE   13
#define FI_SFTP_SYMLINK_FAILED_TO_DELETE   14
#define FI_SFTP_SYMLINK_TOO_MANY_ATTEMPTS  15
#define FI_SFTP_SYMLINK_UNSPECIFIED_ERROR  16
#define FI_FAILED_TO_DECRYPT_PASSWORD      17
#define FI_FILE_SIZE_EXCEED_LIMITATION     18
#define FI_NO_ENOUGH_DISK_SPACE            19

#define FI_DN_MAX_SIZE (400)

/*
******************************************************************************
* TYPES
******************************************************************************
*/


/*
******************************************************************************
* FUNCTION PROTOTYPES
******************************************************************************
*/


 /*****************************************************************************
   *
   *  Name  : fiExportFile
   *
   *  Descr.: A file is exported from the node to an external server.
   *
   *
   *  Args  : IN:     Password      The password belonging to the URI, should be a
   *                                null-terminated string.
   *
   *                  Uri           The (SFTP or FTPES) URI referred to a remote file, e.g.
   *                                sftp://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml.
   *                                If user name is needed to access the referenced file then the
   *                                URI shall include the user name. The URI must not contain
   *                                password. If the URI ends with ‘/’, the file name from the
   *                                fullpath will be used.
   *
   *                  Fullpath      Defines the directory and file where the file to be exported
   *                                is located.
   *
   *  Return: FiResultT
   *
   ****************************************************************************/
FiResultT
fiExportFile(const char *Password, const char *Uri, const char *Fullpath);


 /*****************************************************************************
   *
   *  Name  : fiExportFile2
   *
   *  Descr.: A file is exported from the node to an external server.
   *
   *
   *  Args  : IN: EncryptPassword   The ECIM encrypted password belonging to the URI, should be a
   *                                null-terminated string.
   *
   *               Uri              The (SFTP or FTPES) URI referred to a remote file, e.g.
   *                                sftp://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml.
   *                                If user name is needed to access the referenced file then the
   *                                URI shall include the user name. The URI must not contain
   *                                password. If the URI ends with ‘/’, the file name from the
   *                                fullpath will be used.
   *
   *               Fullpath         Defines the directory and file where the file to be exported
   *                                is located.
   *
   *  Return: FiResultT
   *
   ****************************************************************************/
FiResultT
fiExportFile2(const char *EncryptPassword, const char *Uri, const char *Fullpath);


 /*****************************************************************************
   *
   *  Name  : fiImportFile
   *
   *  Descr.: Imports a file from an external server to the node.
   *
   *
   *  Args  : IN:  Password          The password belonging to the URI, should be a
   *                                 null-terminated string.
   *
   *               Uri               The (SFTP or FTPES) URI referred to a remote file, e.g.
   *                                 sftp://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml.
   *                                 If user name is needed to access the referenced file then the
   *                                 URI shall include the user name. The URI must not contain
   *                                 password.
   *
   *               File              Defines the file name. The file will be saved in the /rcs/fi/.
   *
   *
   *  Return: FiResultT
   *
   ****************************************************************************/
FiResultT
fiImportFile(const char *Password, const char *Uri, const char *File);


 /*****************************************************************************
   *
   *  Name  : fiImportFile2
   *
   *  Descr.: Imports a file from an external server to the node.
   *
   *
   *  Args  : IN:  EncryptPassword   The ECIM encrypted password belonging to the URI, should be a
   *                                 null-terminated string.
   *
   *               Uri               The URI (SFTP or FTPES)referred to a remote file, e.g.
   *                                 sftp://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml.
   *                                 If user name is needed to access the referenced file then the
   *                                 URI shall include the user name. The URI must not contain
   *                                 password.
   *
   *               File              Defines the file name. The file will be saved in the /rcs/fi/.
   *
   *
   *  Return: FiResultT
   *
   ****************************************************************************/
FiResultT
fiImportFile2(const char *EncrytPassword, const char *Uri, const char *File);


/*****************************************************************************
   *
   *  Name  : fiImportFile3
   *
   *  Descr.: Imports a file from an external server to the node.
   *
   *
   *  Args  : IN:  Password   The password belonging to the URI, should be a null-terminated string. 
                                     If last parameter "encrypedPwd" is ture, it is a ECIM encrypted password
   *
   *               Uri               The URI referred to a remote file, e.g.
   *                                 sftp://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml.
   *                                 If user name is needed to access the referenced file then the
   *                                 URI shall include the user name. The URI must not contain
   *                                 password.
   *
   *               File              Defines the file name. The file will be saved in the /rcs/fi/.
   *
   *               MaxSize           Defines the maximum size of the file in bytes, the file to be imported cannot exceed the size limitation.
   *
   *               EncryptedPwd      The flag to indicate if ECIM encrypted password is used or not.
   *                                 If the flag is true, first parameter "password" is a ECIM encrypted password, otherwise it is not.
   *
   *
   *  Return: FiResultT
   *
   ****************************************************************************/
FiResultT
fiImportFile3(const char *EncrytPassword, const char *Uri, const char *File, uint64_t MaxSize, uint8_t EncryptedPwd);



 /*****************************************************************************
   *
   *  Name  : fiFree
   *
   *  Descr.: frees the memory space pointed to by ptr.
   *
   *  Args  : IN:     ptr      A pointer to the allocated memory
   *
   ****************************************************************************/
void
fiFree(void *ptr);


/*****************************************************************************
  *
  *  Name  : fiOpenWrite
  *
  *  Descr.: Deprecated function. NOT to be used. Will be removed in R18.
  *
  *  Args  : IN:     dir       A directory name (with no slashes)
  *                  name      A file name (with no slashes)
  *         OUT:     fp        A pointer to a location for a pointer to FILE
  *
  *  Return: FiResultT
  *
  ****************************************************************************/
FiResultT
fiOpenWrite(
    FiPersistencyT persistence,
    const char *dir,
    const char *name,
    FILE **fp);

#ifdef  __cplusplus
}
#endif

#endif /* __FI_H */

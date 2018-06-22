/* >  CONTENTS
 ******************************************************************************
 ************* Copyright ERICSSON RADIO ACCESS AB *****************************
 ******************************************************************************
 *
 * The copyright to the computer programs herein is the property of
 * ERICSSON RADIO ACCESS AB. The programs may be used and/or copied
 * only with the written permission from ERICSSON RADIO ACCESS AB or
 * in accordance with the terms and conditions stipulated in the
 * agreement/contract under which the prgrams have been supplied.
 *
 ******************************************************************************
        CONTENTS
        --------
        1  Description
        2  Include files
        3  Declarations
        4  Functions
        
 ******************************************************************************
 */


/* >  1  DESCRIPTION
******************************************************************************
*
* <text...>
*
*
* Revision history
* Revided:  04 Nov 2016 EDENTAO
* Change: first revision
*
******************************************************************************
 */


/* >  Double inclusion protection
 ******************************************************************************
 */
#ifndef __LOGFILE_H__
#define __LOGFILE_H__

/* >  2  INCLUDE FILES
 ******************************************************************************
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "ac_defs.h"

/*********        AC Log file definition        *********/

#define MAX_ANTENNA_NUM 128   //can be replaced by the global Macro
#define MAX_CARRIER_NUM 3   //can be replaced by the global Macro
#define FILE_BUFFER_SIZE 65536   //64KB

class LogFile
{
public:
    LogFile(void);
    ~LogFile();
    static LogFile* getInstance();
    bool init();  /**< Driver initialization. */

    void scan_dir(char *dir, int depth);
    bool checkFolderExisted(char *path, char *folderFind);
    bool storeAcData(U16 carrierId, U16 antId, U16 receiveTransmit, S16 * data, U16 dataSize);

private:
    char workPath[128];
    char acPath[128];
    char fileBuffer[FILE_BUFFER_SIZE];

public:
};
/* >  End of double inclusion protection
 ******************************************************************************
 */
#endif

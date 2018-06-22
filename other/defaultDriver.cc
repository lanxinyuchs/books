/* >  CONTENTS
 ******************************************************************************
 ************************** Copyright ERICSSON CBC ****************************
 ******************************************************************************
 *
 * The copyright to the computer programs herein is the property of
 * ERICSSON China (CBC). The programs may be used and/or copied
 * only with the written permission from ERICSSON China (CBC) or
 * in accordance with the terms and conditions stipulated in the
 * agreement/contract under which the prgrams have been supplied.
 *
 ******************************************************************************
 CONTENTS
 --------
 1  Description
 2  Include files
 3  Declarations and Definitions
 4  Signal Definitions
 5  Function prototypes
 6  Functions
 7  Processes
 
  ******************************************************************************
 */


/* >  1  DESCRIPTION
 *******************************************************************************
 *
 * <text...>
 *
 * @(#) ClearCase ID: defaultDriver.cc /main/..  2008-03-01 EZHIBWA
 *
 * Review Info:        2008-03-25         By BP SW team
 *
 * Revision history
 *     Date              Author             Description
 *  2008-03-01           ezhibwa           First Revision
 *
 *******************************************************************************
 */

/* >  2  INCLUDE FILES
 *******************************************************************************
 */

#include <shell.h>
#include <stdio.h>
#include <stdlib.h>

//extern "C"
//{
//#include <xpai_xll_if.h>
//}

#include "defaultDriver.h"
//#include "dataBase.h"
//#include "halPrint.h"

/* >  3  DECLARATIONS AND DEFINITIONS
 ******************************************************************************
 */

/* >  3.1  GLOBAL
 ******************************************************************************
 */

/* >  3.2  LOCAL
 ******************************************************************************
 */
char* DefaultDriver::freqClassUsage  = NULL;

/* >  4  SIGNAL DEFINITIONS
 ******************************************************************************
 */

/* >  5  FUNCTION PROTOTYPES
 ******************************************************************************
 */

/* >  6  FUNCTIONS
 ******************************************************************************
 */

/* >  6.1  GLOBAL
 ******************************************************************************
 */

/* >  6.2  LOCAL
 ******************************************************************************
 */

/* >  DefaultDriver::DefaultDriver
 *****************************************************************************
 *
 *  Description :  Construction function of DefaultDriver Class.
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
DefaultDriver::DefaultDriver()
{
    instanceDrvName = NULL;
}

/* >  DefaultDriver::init
 *****************************************************************************
 *
 *  Description :  This is the init function of class DefaultDriver.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 * DefaultDriver*                  INPUT                 drv
 *     void*                       INPUT                 data
 *     void*                       OUTPUT               result
 *
 *  Return      :
 *  Type                                  Comments
 *  bool             Return true if success, return false if failed.
 *
 *****************************************************************************
 */
bool DefaultDriver::init(const char* instanceDrvName)
{
    static bool doItOnce1 = true;
    //static bool doItOnce1 = DataBase::get("/board/freqClassUsage",  &freqClassUsage);
    return doItOnce1;
}

/* >  DefaultDriver::initSequenceExecution
 *****************************************************************************
 *
 *  Description :  This is the init sequence of class DefaultDriver.
 *
 *  Arguments   :  void
 *
 *  Return      :
 *  Type                                  Comments
 *  bool             Return true if success, return false if failed.
 *
 *****************************************************************************
 */
bool DefaultDriver::initSequenceExecution(void)
{
    return true;
}

/* >  DefaultDriver::registerHalInterface
 *****************************************************************************
 *
 *  Description :  DefaultDriver::registerHalInterface
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void DefaultDriver::registerHalInterface(void)
{
}

/* >  DefaultDriver::testInterfaceInit
 *****************************************************************************
 *
 *  Description :  DefaultDriver::testInterfaceInit
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void DefaultDriver::testInterfaceInit(void)
{
}

const char* DefaultDriver::getInstanceDrvName() const
{
    // If instanceDriver name is not set return string that indicates that and to avoid SW crashes
    return (instanceDrvName != NULL) ? instanceDrvName : "= DefaultDriver: instanceDrvname not initialized =";
}

bool DefaultDriver::show(void *data)
{
    return true;
}

bool PaDriverBranchId::setSuffix(const std::string& newSuffix)
{   
/*
    char key[DB_MAX_KEY_LENGTH];	
    U16 paBoardChNum;
    suffix=newSuffix;
    std::string pa = suffix.substr(0, 3);
    branchId = 'Z';

    sprintf(key,"/hw/pa/board/chNum");
    if(!DataBase::getIfExists(key, paBoardChNum))
    {
        TRACE_ERROR(STR("failed to get \"%s\" from SW DB.", key));
        return false;
    } 
 
    branchId ='A' + (pa[0] - '0')*paBoardChNum + (pa[2] - '0');
    connected = true;    
    //legacy = false;
*/
    return true;
}



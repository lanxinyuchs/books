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
     3  Declarations
     4  Functions
     
 ******************************************************************************
 */


/* >  1  DESCRIPTION
 *******************************************************************************
 *
 * <text...>
 *
 * @(#) ClearCase ID: defaultDriver.h /main/..  2008-03-01 EZHIBWA
 *
 * Review Info:        2008-03-25         By BP SW team
 *
 * Revision history
 *     Date              Author             Description
 *  2008-03-01           ezhibwa           First Revision
 *
 *******************************************************************************
 */

/* >  Double inclusion protection
 ******************************************************************************
 */
#ifndef __DEFAULT_DRIVER_H__
#define __DEFAULT_DRIVER_H__

/* >  2  INCLUDE FILES
 ******************************************************************************
 */
#include <vector>
#include <string>
#include "ac_defs.h"


/* >  3  DECLARATIONS
 ******************************************************************************
 */
#define starts_with(x, y) (!strncmp(x, y, strlen(x)))

/*!
 * The purpose of this class is to keep track of three different parts of a driver:
 *
 *  1. Is this a PA-driver or not.
 *     This is shown by the member variable connected.
 *
 *  2. If it is a PA-driver, then we need to know which antenna-port it connects to.
 *     This is shown by the member variable branchId.
 *
 *  3. Also we need to know whether it is a single-TX (legacy) or multi-TX which is only
 *     relevant if there is a PA-driver.
 *     This is shown by the member variable legacy.
 *
 *  The internal state of this class gives information about which of these three
 *  parts above that are valid for a specific driver.
 *
 *  This class replaces the parameter isPaDriver that in some extent had the same
 *  purpose as this class.
 *
 *  Below is a table including the different values of the state variables
 *  (connected, branchId and legacy) depending on different configuration of the driver:
 *
 *    Type of configuration    | connected |   branchId    |  legacy             |  isPaDriver*
 *    -------------------------|-----------|---------------|---------------------|-------------
 *    multi-TX, PA-driver      | true      | A - Z         | false               | A - B
 *    -------------------------|-----------|---------------|---------------------|-------------
 *    single-TX, PA-driver     | true      | A             | true                | 1
 *    -------------------------|-----------|---------------|---------------------|-------------
 *    not a PA-driver          | false     | 0             | true                | 0
 *                             |           |(undefined)    | (irrelevant         |
 *                             |           |               |  for not PA-driver) |
 *    -------------------------|-----------|---------------|---------------------|-------------
 *
 *  * = This is a mapping to the old implementation, and is only meant to be a help for
 *      designer that has worked with the variable isPaDriver. Designer not familiar with
 *      isPaDriver do not need to bother about this column.
 *
 *  For single-TX some of the state variables are initiated in the class StartUp
 *  and for multi-TX the same initiations are accomplished in the class MultiPa.
 *
 */
class PaDriverBranchId
{
public:

    PaDriverBranchId()
    {
        connected = false;
        //legacy = false;
        branchId = 0;
    };

    /*!
    * @returns true if it is configured as a PA-driver, false if not.
    */
    bool isConnected() const
    {
        return connected;
    };

    /*!
    * @returns true if single-TX (legacy), false if not (which means multi-TX).
    */
    /*bool isLegacy() const
    {
    return legacy;
    };*/
    
    /*!
    * @returns true if this driver uses suffix on its name ("drv:0.0")
    */
    bool hasSuffix() const
    {
        return !suffix.empty();
    };
    /*!
    * Set the status variables depending on character inparameter.
    *
    * @param id the char corresponding to the antenna port.
    * @returns true if the set is OK, false otherwise.
    */
    bool set(char id)
    {
        if(((id >= 'A') && (id <= 'H'))||((id >= '0') && (id <= '9')))
        {
            connected = true;  // True means that it is a PA-driver.
            branchId = id;
            //legacy = false;    // False means multi-TX driver.
            return true;
        }
        return false;
    };

    /*! Use this for Platform 5.2 and later, which use the drv:x[.y[.z]] drivername style.
    *  The suffix is the part after the :
    *  On 5.2 and later the PA branch should be handled through hali1 using instanceTokens,
    *  so this is a temporary fix.
    */
    bool setSuffix(const std::string& newSuffix);
    
    std::string getSuffix() const
    {
        return suffix;
    }

    /*!
    * Make this driver to be a "legacy" driver. For PA-drivers only, this makes
    * the driver a single-TX driver.
    */
    /*void setLegacy()
    {
    legacy = true;
    };*/
    
    /*!
    * Fetch the value of the status variable branchId.
    *
    * @returns char A - Z or 0 if not a PA-driver.
    */
    char get() const
    {
        //if (!suffix.empty())
        //    INFO(STR("ABN: BranchId should not be used when suffix (%s) is used", suffix.c_str()));
        return branchId;
    };

    /*!
    * Fetch the value of branchId as an U8.
    *
    * @returns U8 0 - 7.
    */
    U32  getU32() const
    {
        return (((branchId - 'A') < 0) ? 0:branchId - 'A');
    };
    
    /*!
    * Fetch the value of branchId as an U16.
    *
    * @returns U16 0 - 7.
    */
    U16  getU16() const
    {
        return (((branchId - 'A') < 0) ? 0:branchId - 'A');
    };
    
private:
    
    bool connected; /*!< @brief True means connected as a PA-driver, false means not a PA-driver. */
    char branchId;  /*!< @brief Which antenna port that is connected to the PA-driver, values A - H or 0 when not PA. */
    //bool legacy;    /*!< @brief True means single-TX and false means multi-TX. Irrelevant for non PA-drivers.*/
    std::string suffix;

};


/* The following structure (EqualKey) is used by hash_map<const char*, ...> */
/*struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};
*/
/**
 * Base class for all HW Drivers with implemented default functionality.
 */
class DefaultDriver
{
public:
    DefaultDriver();
    virtual ~DefaultDriver(){}
    virtual bool init(const char* instanceDrvName);
    virtual bool initSequenceExecution(void);
    virtual void registerHalInterface(void);
    virtual void testInterfaceInit(void);
    virtual const char* getInstanceDrvName() const;

    virtual bool show(void *data = NULL);

    char* instanceDrvName;
    PaDriverBranchId paDriverBranchId;
    static char* freqClassUsage;
};

/* >  4  FUNCTIONS
 ******************************************************************************
 */

/* >  End of double inclusion protection
 ******************************************************************************
 */

#endif /* __DEFAULT_DRIVER_H__ */

/*********************************** END **************************************/


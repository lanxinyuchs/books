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
  ******************************************************************************
 */
/* >  1  DESCRIPTION
 *******************************************************************************
 *
 * General ac frequency data
 * Corrected AC data, error data.
 *
 * Revision history
 *     Date                   Author             Description
 *  2016-08-30           edentao           First Revision
 *
 *******************************************************************************
 */


/* >  2  INCLUDE FILES
 *******************************************************************************
 */

/*******   mailbox header  *******/
#include <itc.h>
#include <ac_defs.h>



/* >  3  DECLARATIONS AND DEFINITIONS
 ******************************************************************************
 */
 
/* >  3.1  GLOBAL
 ******************************************************************************
 */

/* >  3.2  LOCAL
 ******************************************************************************
 */
 
/* >  4  SIGNAL DEFINITIONS
 ******************************************************************************
 */

/* >  5  FUNCTION PROTOTYPES
 ******************************************************************************
 */
bool get_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id);

/* >  6  FUNCTIONS
 ******************************************************************************
 */
bool get_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id)
{
    *mbox_id= itc_locate(mbox_name);
    if (*mbox_id== ITC_NO_ID) {
        //TPT_ERROR(STR("%s does not exist", mbox_name));
        return false;
    }
    return true;
}

/* >  6.1  GLOBAL
 ******************************************************************************
 */

/* >  6.2  LOCAL
 ******************************************************************************
 */


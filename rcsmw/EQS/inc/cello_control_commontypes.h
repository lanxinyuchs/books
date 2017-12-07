/* ----------------------------------------------------------------------
 * %CCaseFile:	cello_control_commontypes.h %
 * %CCaseRev:	/main/R1A/1 %
 * %CCaseDate:	2012-10-03 %
 * %CCaseDocNo:	9/190 55-CNX 901 0099 Ux %
 * Author:	eramlan
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	CELLO_template.h %
 * %CCaseTemplateId: 114/002 01-LXA 119 334 Ux, Rev: /main/3 %
 *
 * © Ericsson AB 2006 All rights reserved.
 * The information in this document is the property of Ericsson.
 * Except as specifically authorized in writing by Ericsson, the receiver of this
 * document shall keep the information contained herein confidential and shall protect
 * the same in whole or in part from disclosure and dissemination to third parties.
 * Disclosure and disseminations to the receivers employees shall only be made
 * on a strict need to know basis.
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R60Y/1     2006-02-10 eramlan     Created
 * R60Y/2     2006-02-10 erabegt     Moved types and macros here from PRI.
 * R60Y/3     2006-02-28 erabegt     Moved Cello_VpiVci, Cello_Cep, Cello_Port
 *                                   types here from cello_commontypes.h
 * cppdev/2   2008-05-15 qtxleco     Added type CelloProgramInstance.
 * cppdev/5   2008-10-22 estator     Added type CelloPiuPosition.
 * cppdev/6   2010-04-06 xcssriy     Corrected CELLO_AVAIL2_DEPENDENCY_LOCKED 
 *                                   value.
 * cppdev/7  2010-04-07 xcssriy      rollbacked AVAIL2_DEPENDENCY_LOCKED value.
 * ----------------------------------------------------------------------
 */

#ifndef CELLO_CONTROL_COMMONTYPES_H
#define CELLO_CONTROL_COMMONTYPES_H

/*
 * ---------------------------------------------------------------------------
 * Include files
 * ---------------------------------------------------------------------------
 */

#include "osetypes.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * ---------------------------------------------------------------------------
 * Defines
 * ---------------------------------------------------------------------------
 */

/*
 * The link handler name is a null terminated string containing six characters.
 */
#define CELLO_PIU_LH_NAME_SIZE (7)

/* Maximum number of characters for the PID attributes,
   including the null character. */
#define CELLO_MAX_PRODUCT_NUMBER_LEN   25
#define CELLO_MAX_PRODUCT_REVISION_LEN  8
#define CELLO_MAX_PRODUCT_NAME_LEN     13
#define CELLO_MAX_PRODUCT_DATE_LEN      9
#define CELLO_MAX_SERIAL_NUMBER_LEN    14

/* Invalid id from OEI */
#define CELLO_EMPTY_EVENT_ID                 ((U32)0)

/* Invalid id from OSDI */
#define CELLO_EMPTY_SHUTTINGDOWN_ID          ((U32)0)

/*
 * ---------------------------------------------------------------------------
 * Type declarations and macros for limits when applicable.
 * ---------------------------------------------------------------------------
 */

/*
   Data structure for Product Information Data (PID). 
   The last character in each members of the 
   struct will be the null character('\0').
*/
typedef struct 
{
  U8 productNumber[CELLO_MAX_PRODUCT_NUMBER_LEN];
  U8 productRevision[CELLO_MAX_PRODUCT_REVISION_LEN];
  U8 productName[CELLO_MAX_PRODUCT_NAME_LEN];
  U8 productDate[CELLO_MAX_PRODUCT_DATE_LEN];
  U8 serialNumber[CELLO_MAX_SERIAL_NUMBER_LEN];
} Cello_PidInHW;

/*
 * This type identifies a Plug-In Unit resource. It contains the attributes
 * that together uniqely identifies the resource object instance.
 *
 * smn   - SMN, Switch Module Number.
 *
 * apn   - APN, ASCI-port Number.
 *
 * ern   - ERN, Execution Resource Number. The ERN identifies e.g the GPB, DBM,
 *         SP, custom circuits, etc.
 *
 */
typedef struct
{
   U32             smn;
   U32             apn; 
   U32             ern;
} CelloPiuHwAddr;

#define CELLO_MAX_NUMBER_SMN 256
#define CELLO_MAX_NUMBER_APN 64

/*
 * This type specifies the Plug-In Unit resource's role.
 */
typedef U16 CelloPiuRole;
#define CELLO_PIU_ROLE_MP                1
#define CELLO_PIU_ROLE_BP                2
#define CELLO_PIU_ROLE_NONE              3
#define CELLO_PIU_ROLE_CMXB              4


typedef U16 CelloOperationalState;
#define CELLO_OPER_DISABLED                  0
#define CELLO_OPER_ENABLED                   1

typedef U16 CelloAdministrativeState;
#define CELLO_ADMIN_LOCKED                   0
#define CELLO_ADMIN_UNLOCKED                 1
#define CELLO_ADMIN_SHUTTINGDOWN             2

typedef U16 CelloAvailabilityStatus;
#define CELLO_AVAIL_IN_TEST                  0
#define CELLO_AVAIL_FAILED                   1
#define CELLO_AVAIL_POWER_OFF                2
#define CELLO_AVAIL_OFF_LINE                 3
#define CELLO_AVAIL_OFF_DUTY                 4
#define CELLO_AVAIL_DEPENDENCY               5
#define CELLO_AVAIL_DEGRADED                 6
#define CELLO_AVAIL_NOT_INSTALLED            7
#define CELLO_AVAIL_LOG_FULL                 8
#define CELLO_AVAIL_DEPENDENCY_LOCKED       51
#define CELLO_AVAIL_DEPENDENCY_FAILED       52
#define CELLO_AVAIL_DEPENDENCY_SHUTTINGDOWN 53
#define CELLO_AVAIL_NO_STATUS               99
/* Protocol version 2 of PRI*/
#define CELLO_AVAIL2_IN_TEST                  0x0001
#define CELLO_AVAIL2_FAILED                   0x0002
#define CELLO_AVAIL2_POWER_OFF                0x0004
#define CELLO_AVAIL2_OFF_LINE                 0x0008
#define CELLO_AVAIL2_OFF_DUTY                 0x0010
#define CELLO_AVAIL2_DEPENDENCY               0x0020
#define CELLO_AVAIL2_DEGRADED                 0x0040
#define CELLO_AVAIL2_NOT_INSTALLED            0x0080
#define CELLO_AVAIL2_LOG_FULL                 0x0100
#define CELLO_AVAIL2_DEPENDENCY_LOCKED        0x0200
#define CELLO_AVAIL2_DEPENDENCY_FAILED        0x0400
#define CELLO_AVAIL2_DEPENDENCY_SHUTTINGDOWN  0x0800
#define CELLO_AVAIL2_NO_STATUS                0x0000


/*
   Data structure for Product Information Data (PID). 
   The last character in each members of the 
   struct will be the null character('\0'). 
*/

typedef struct 
{
  U8 productNumber[CELLO_MAX_PRODUCT_NUMBER_LEN];
  U8 productRevision[CELLO_MAX_PRODUCT_REVISION_LEN];
  U8 productName[CELLO_MAX_PRODUCT_NAME_LEN];
  U8 productDate[CELLO_MAX_PRODUCT_DATE_LEN];
}Cello_PidInSW;



/* The internal ingress/egress VPI and VCI value for a connection */
typedef struct
{
   U16 vpi;    /* Virtual Path Identifier    */
   U16 vci;    /* Virtual Channel Identifier */

} Cello_VpiVci;


/* The Connection End Point identifier */
typedef U32 Cello_Cep;


/* The Cello Port Identifier */
typedef U32 Cello_Port;


/*
 * This type identifies a program instance. It can be used in cunjunction
 * with Cello_PidInSW to fully specify a program instance.
 *
 *  instanceId     - Instance identifier. This corresponds to the identifier
 *                   used as hunt path prefix for processes in instantiated
 *                   programs. Identity zero indicates that the program is not 
 *                   instantiated.
 *  hwDistribution - This defines on which processor and/or processor core
 *                   the program instance should execute.
 *                   Distribution zero indicates no/default distribution.
 *  restartGroup   - This controls error escalation for the program instance.
 *
 */
typedef struct
{
  U32 instanceId;
  U32 hwDistribution;
  U32 restartGroup;
} CelloProgramInstance;


/*****************************************************************************
 *
 * This type identifies the position of a PIU in a node.
 *
 *  subrackNumber - Number of the subrack where the PIU is placed.
 *                  For subracks having a SwitchModule MO, the value
 *                  is the same as the SMN. For other subracks,
 *                  it is defined by the Subrack MO.
 *  slotNumber    - Number of the slot where the PIU is placed.
 *                  Slot number is defined by the Slot MO.
 *                  Numbering is left to right, starting with 1.
 *
 ****************************************************************************/
typedef struct 
{
  U32 subrackNumber;
  U32 slotNumber;
} CelloPiuPosition;


/*
 * ---------------------------------------------------------------------------
 * Function prototypes
 * ---------------------------------------------------------------------------
 */

/*
 * Name         :
 *
 * Description  :
 *
 * Input par.   :
 *
 * Output par.  :
 *
 * Return       :
 *
 * Side effects :
 */

#ifdef __cplusplus
}
#endif

#endif /* CELLO_CONTROL_COMMONTYPES_H */

/******************************************************************************
 *
 *      COPYRIGHT (C) Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/******************************************************************************
 *
 * Product name:
 *      XSPRAY,  CAH 109 2191
 *
 * File:
 *      xspray_serverint.c
 *
 * Author:
 *      Göran Nordin (egornor)
 *
 * Description:
 *      This file implements the xsprayTxSrvRxSuperv process.
 *
 * Revision history:
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 First version.
 *      2013-04-12 Göran Nordin (egornor)
 *                 HQ99347, xspray crashes on OSE4
 *      2015-06-25 Göran Nordin (egornor)
 *                 Changed to only use funtions that are supported in lits
 *                 emulation.
 */

/*----------------------------  Include files  ------------------------------*/

#include "ose.h"
#include "xspray.h"
#include "xspray_server.h"
/*----------------------------  CONSTANTS  ----------------------------------*/

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/


/*
 * The union below defines signals received and sent by xspray monitor.
 */
union SIGNAL
{
  SIGSELECT                     sigNo;
  struct XSPRAY_AttachKillReqS  attachKillReq;
};

/*----------------------------  Declaration of External Functions  ----------*/

/*----------------------------  Declaration of Local Functions  -------------*/


/*----------------------------  Definition of Global Variables  -------------*/

/*----------------------------  Definition of Local Variables  --------------*/


/*----------------------------  Function Definitions  -----------------------*/

/******************************************************************************
 *
 * Process name:
 *      xsprayTxSrvRxSuperv
 *
 * Process type:
 *      OSE5: Interrupt process without HW vector, i.e. created with OSVECTOR -1.
 *      OSE4: Prioritized process.
 *
 * Description:
 *      xspray process that supervise connection to receiving process.
 *      It is an interrupt process without HW vector because we want to be
 *      able to stop the tx server regardless of which priority it is
 *      executing on.
 *      If connection is lost then the current block is ordered to be killed.
 *      Because this interrupt process cannot use get_envp to get pointer to
 *      status area it relies on that redirection tables are setup on the tx
 *      server that handles transmission. The OS_ATTACH_SIG should have tx
 *      server as addressee. The XSPRAY_KILL_REQ is sent to the tx server but
 *      but is redirected to xsprayTxSrvKill.
 *
 *****************************************************************************/
OS_PROCESS(xsprayTxSrvRxSuperv)
{
  union SIGNAL *sig;
  OSATTREF attRef;

#if defined(_LITS_OSE_H) || !(defined(OSE_VERSION) && (OSE_VERSION >= 500))
  /*
   * OSE4 does not allow attach from interrupt process therefore a
   * prioritized process need to be created.
   */
  /*
   * lits does currently not support interrupt process without HW vector,
   * ((OSVECTOR) -1),  process therefore a prioritized process need to be
   * created.
   */
  for (;;)
#endif
  if ((sig = receive(xsprayAllSig)) != NIL)
  {
    /*
     * We just ignore signals other than because this interrupt process cannot
     * use get_envp to get pointer to status area and we don't want to spam
     * error.
     */
    if (sig->sigNo == XSPRAY_ATTACH_REQ)
    {
      PROCESS pidToAttachTo = sig->attachKillReq.pidToAttachToOrKill;

      sig->sigNo = XSPRAY_KILL_REQ;
      sig->attachKillReq.pidToAttachToOrKill = sender(&sig);
      attRef = attach(&sig, pidToAttachTo); /* Ok to ignore OSATTREF return value */
      (void)attRef;
    }
    else if (sig->sigNo == XSPRAY_KILL_REQ)
    {
         /* Stop tx server */
         /* get_ptype needed because Lits does not support stopping zoombie. */
         if (get_ptype(sig->attachKillReq.pidToAttachToOrKill) !=  OS_ZOOMBIE)
	   stop(sig->attachKillReq.pidToAttachToOrKill);
	 /* Send signal to xsprayTxSrvKill */
	 send(&sig, sig->attachKillReq.killSrvPid);
    }
    else
    {
      free_buf(&sig);
    }
  }
}

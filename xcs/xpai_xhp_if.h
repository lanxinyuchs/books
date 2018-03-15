/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
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
 *      XPAI/XHP
 *
 * File:
 *      xpai_xhp_if.h
 *
 * Author:
 *      Johnny Aberg (qrajabe)
 *
 * Description:
 *      This file defines XPAI XHP external interface functions.
 *
 * Reviewed:
 *      2000-10-02, Johnny Aberg (qrajabe)
 *
 * Revision history:
 *      2000-10-02, Johnny Aberg (qrajabe)
 *              Implementation started.
 *
 *      2005-11-28, Sven Löfgren (qlofsve)
 *              HDLC UCC support added.
 *
 *      2005-12-05, Sven Löfgren (qlofsve)
 *              HDLC UCC support uppdated after review.
 *
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *      2006-06-27, Sven Löfgren (xedsven)
 *              TR WRNad21205 (HDLC_T1 does not specify minimum time) and
 *              CR WRNad24942 (WendyP5ad21901WP4ad21796: Add
 *              XPAI_ConfigECPResponseDelay due...).
 *
 *      2006-10-24, Sven Löfgren (xedsven)
 *              CR WRNad23807 (WendyP5 WRNad10824) (Improve availabilty on link
 *              between RUIF and RU, otherwis EUL won't work) (= prio mess).
 *              XPAI_UNC_PRIORITIZED_SIGNAL added.
 *
 *      2008-06-30, Sven Löfgren (xedsven)
 *              Tags XPAI_XHP_LINKUP0, XPAI_XHP_LINKDOWN0, XPAI_XHP_LINKUP1,
 *              and XPAI_XHP_LINKDOWN1 added.
 *              XPAI_ConfigECPResponseDelay: 2 ms changed to 3 ms.
 *
 *      2011-01-31, Henrik Stöckel (qrahsto)
 *              WP nci066, multiple ports and channels.
 *
 *****************************************************************************/
#ifndef _XPAI_XHP_IF_H
#define _XPAI_XHP_IF_H

/*----------------------------  Include Files  ------------------------------*/
#include <ose.h>
#include <osetypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  Constants  ----------------------------------*/

/*
 * XHP tags that may be subscribed with the function XPAI_Subscribe
 * (see xpai_xmr_if.h).
 *
 * XPAI_XHP_LINKUP
 *     Distributed when HDLC link with path bp is established.
 *     This is the RS485 bus. This bus is used when running CPRI version 1.
 *     The procedure HDLC UNC has entered Normal Response Mode, i.e.
 *     the HDLC UNC data link is operational.
 *     Valid size: 1 < strlen(XPAI_XHP_LINKUP) < XMR_MAX_TAG_LENGTH
 *
 * XPAI_XHP_LINKUP_0
 *     Distributed when CPRI link #0 with path bp0 is established.
 *     The CPRI version is 2.
 *     The procedure HDLC UAC has entered Asynchronous Response Mode, i.e.
 *     the HDLC UAC data link is operational.
 *     Valid size: 1 < strlen(XPAI_XHP_LINKUP_0) < XMR_MAX_TAG_LENGTH
 *
 * XPAI_XHP_LINKUP_1
 *     Distributed when CPRI link #1 with path bp1 is established.
 *     The CPRI version is 2.
 *     The procedure HDLC UAC has entered Asynchronous Response Mode, i.e.
 *     the HDLC UAC data link is operational.
 *     Valid size: 1 < strlen(XPAI_XHP_LINKUP_1) < XMR_MAX_TAG_LENGTH
 *
 * XPAI_XHP_LINKDOWN
 *     Distributed when HDLC link with path bp is released or lost.
 *     This is the RS485 bus. This bus is used when running CPRI version 1.
 *     The procedure HDLC UNC has entered Normal Disconnected Mode, i.e.
 *     the HDLC UNC data link is non-operational.
 *     Valid size: 1 < strlen(XPAI_XHP_LINKDOWN) < XMR_MAX_TAG_LENGTH
 *
 * XPAI_XHP_LINKDOWN_0
 *     Distributed when CPRI link #0 with path bp0 is released or lost.
 *     The CPRI version is 2.
 *     The procedure HDLC UAC has entered Asynchronous Disconnected Mode, i.e.
 *     the HDLC UAC data link is non-operational.
 *     Valid size: 1 < strlen(XPAI_XHP_LINKDOWN_0) < XMR_MAX_TAG_LENGTH
 *
 * XPAI_XHP_LINKDOWN_1
 *     Distributed when CPRI link #1 with path bp1 is released or lost.
 *     The CPRI version is 2.
 *     The procedure HDLC UAC has entered Asynchronous Disconnected Mode, i.e.
 *     the HDLC UAC data link is non-operational.
 *     Valid size: 1 < strlen(XPAI_XHP_LINKDOWN_1) < XMR_MAX_TAG_LENGTH
 *
 */
#define XPAI_XHP_LINKUP    "XHP_LinkUp"
#define XPAI_XHP_LINKUP_0  "XHP_LinkUp0"
#define XPAI_XHP_LINKUP_1  "XHP_LinkUp1"

#define XPAI_XHP_LINKDOWN   "XHP_LinkDown"
#define XPAI_XHP_LINKDOWN_0 "XHP_LinkDown0"
#define XPAI_XHP_LINKDOWN_1 "XHP_LinkDown1"

/* HDLC UNC low and high priority signals.
 *
 * Signals that are transmitted on the HDLC link, from the HDLC secondary
 * station to the HDLC primary station, may be sent with low or high
 * priority. To send a signal with high priority on the HDLC link, set a bit,
 * XPAI_UNC_PRIORITIZED_SIGNAL, in the signal number. The bit is cleared
 * before the signal is transmitted on the HDLC link.
 *
 * A signal that is sent with high priority pauses any ongoing transmission of
 * frames of a low priority signal on the HDLC link. When the high priority
 * signal is sent completely the transmission of frames of the paused low
 * priority signal is continued.
 *
 * The high priority signal is also compressed by removing the high priority
 * signal header (OSE header and signal number) when a signal with the same
 * high priority signal header is sent more than once. Maximum 8 different
 * high priority signal headers are stored in the high priority signal header
 * lists in the HDLC primary and secondary stations. Bits in the HDLC
 * fragmentation header are used to encode which of the stored headers belongs
 * to a compressed signal. If more than 8 different high priority signal headers
 * are received, the high priority signal header list is cleared and has to be
 * rebuild again (when new high prioritized signals are received), which means
 * that the compression will be less effective. To minimize the number of stored
 * high priority signal headers, it is recommended that a high priority signal
 * is always sent from the same process. The high priority signal header list
 * is also cleared if the HDLC link enters Normal Disconnected Mode (link down)
 * and rebuild again when the HDLC link enters Normal Response Mode (link up).
 *
 * The shell command 'linkstat -state' shows the stored high priority signal
 * headers in the high priority signal header list.
 *
 * Example: allocate signal to be sent with low priority
 *   sendP = alloc(sizeof(struct mySigS), MY_SIG);
 *
 * Example: allocate signal to be sent with high priority
 *   sendP = alloc(sizeof(struct mySigS), MY_SIG | XPAI_UNC_PRIORITIZED_SIGNAL);
 */
#define XPAI_UNC_PRIORITIZED_SIGNAL  0x00000000 /* Currently not supported */


/* Values for UCC hdlcAddr. */
#define XPAI_UCC_HDLC_ADDR_MIN  1
#define XPAI_UCC_HDLC_ADDR_MAX  254


/* Values for UCC length. */
#define XPAI_UCC_LENGTH_MIN     1
#define XPAI_UCC_LENGTH_MAX     78


/* Return codes for XPAI_UCCSubscribe.
 * Success:
 *   XPAI_UCC_SUBSCRIBE_OK
 * Error codes:
 *   XPAI_UCC_SUBSCRIBE_NOK_ADDR_RANGE: The HDLC address is out of range.
 *   XPAI_UCC_SUBSCRIBE_NOK_ADDR_SUB:   The HDLC address is already subscribed to.
 *   XPAI_UCC_SUBSCRIBE_NOK_SUPPORT:    The UCC functionality is not supported.
 *   XPAI_UCC_SUBSCRIBE_NOK_OTHER:      Other (internal) error.
 */
#define XPAI_UCC_SUBSCRIBE_OK              0
#define XPAI_UCC_SUBSCRIBE_NOK_ADDR_RANGE  -1
#define XPAI_UCC_SUBSCRIBE_NOK_ADDR_SUB    -2
#define XPAI_UCC_SUBSCRIBE_NOK_SUPPORT     -3
#define XPAI_UCC_SUBSCRIBE_NOK_OTHER       -4


/* Return codes for XPAI_UCCSend.
 * Success:
 *   XPAI_UCC_SEND_OK
 * Error codes:
 *   XPAI_UCC_SEND_NOK_ADDR_RANGE: The HDLC address is out of range.
 *   XPAI_UCC_SEND_NOK_ADDR_SUB:   The HDLC address is not subscribed to.
 *   XPAI_UCC_SEND_NOK_LENGTH:     The length is out of range.
 *   XPAI_UCC_SEND_NOK_SUPPORT:    The UCC functionality is not supported.
 *   XPAI_UCC_SEND_NOK_OTHER:      Other (internal) error.
 */
#define XPAI_UCC_SEND_OK              0
#define XPAI_UCC_SEND_NOK_ADDR_RANGE  -1
#define XPAI_UCC_SEND_NOK_ADDR_SUB    -2
#define XPAI_UCC_SEND_NOK_LENGTH      -3
#define XPAI_UCC_SEND_NOK_SUPPORT     -4
#define XPAI_UCC_SEND_NOK_OTHER       -5


/* Return codes for XPAI_ConfigECPResponseDelay
 * Success:
 *   XPAI_CONFIG_ECP_RESPONSE_DELAY_OK
 * Error codes:
 *   XPAI_CONFIG_ECP_RESPONSE_DELAY_NOK_SERVER  Server not found.
 *   XPAI_CONFIG_ECP_RESPONSE_DELAY_NOK_OTHER   Other error.
 */
#define XPAI_CONFIG_ECP_RESPONSE_DELAY_OK          0
#define XPAI_CONFIG_ECP_RESPONSE_DELAY_NOK_SERVER  -1
#define XPAI_CONFIG_ECP_RESPONSE_DELAY_NOK_OTHER   -2


/* Return codes for XPAI_StartLink
 * Success:
 *   XPAI_START_LINK_OK
 * Error codes:
 *   XPAI_START_LINK_NOK_PORT Wrong port number
 *   XPAI_START_LINK_NOK_CHANNEL Wrong channel number
 *   XPAI_START_LINK_NOK_OTHER Other error
 */
#define XPAI_START_LINK_OK          0
#define XPAI_START_LINK_NOK_PORT    -1
#define XPAI_START_LINK_NOK_CHANNEL -2
#define XPAI_START_LINK_NOK_OTHER   -3


/* XPSIM stub XPAI functions. */
#ifdef SOFT
#define XPAI_FNO_UCC_SUBSCRIBE              ((U32)XPAI_UCCSubscribe)
#define XPAI_FNO_UCC_SEND                   ((U32)XPAI_UCCSend)
#define XPAI_FNO_CONFIG_ECP_RESPONSE_DELAY  ((U32)XPAI_ConfigECPResponseDelay)
#define XPAI_FNO_START_LINK                 ((U32)XPAI_StartLink)
#endif

/*
 * Signal numbers.
 * 0x0100E900 = XHP_SIGBASE. Redefined here to avoid dependency to xp.h
 */
#define XPAI_UCC_DELIVER_IND  (0x0100E900 + 0x80) /* !-SIGNO( struct XPAI_UccDeliverIndS )-! */


/*----------------------------  Macros  -------------------------------------*/

/*----------------------------  Structs and Typedefs  -----------------------*/

/******************************************************************************
 * Signal:
 *      XPAI_UCC_DELIVER_IND
 *
 * Parameters:
 *      hdlcAddr  HDLC address of the HDLC Tributary station which received
 *                the data.
 *                Value: XPAI_UCC_HDLC_ADDR_MIN - XPAI_UCC_HDLC_ADDR_MAX.
 *      length    Number of valid U8 in the data array, starting at offset 0.
 *                Value: XPAI_UCC_LENGTH_MIN - XPAI_UCC_LENGTH_MAX.
 *      data      Array containing data received from the HDLC Control station.
 *
 * Description:
 *      Indication with user data sent to subscriber when data is received by
 *      a HDLC Tributary station.
 *
 *****************************************************************************/
struct XPAI_UccDeliverIndS {
	SIGSELECT sigNo;
	U8        hdlcAddr;
	U16       length;
	U8        data[XPAI_UCC_LENGTH_MAX];
};

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_UCCSubscribe
 *
 * Parameters:
 *      pid       Pid of the subscriber.
 *      hdlcAddr  HDLC address of the HDLC Tributary station.
 *                Value: XPAI_UCC_HDLC_ADDR_MIN - XPAI_UCC_HDLC_ADDR_MAX.
 *
 * Return value:
 *      XPAI_UCC_SUBSCRIBE_OK
 *      XPAI_UCC_SUBSCRIBE_NOK_ADDR_RANGE
 *      XPAI_UCC_SUBSCRIBE_NOK_ADDR_SUB
 *      XPAI_UCC_SUBSCRIBE_NOK_SUPPORT
 *      XPAI_UCC_SUBSCRIBE_NOK_OTHER
 *
 * Description:
 *      This function is used by the client to subscribe for indications with
 *      user data received by a HDLC UCC Tributary station. The HDLC UCC
 *      Tributary station (slave) will forward any data received from the HDLC
 *      UCC Control station (master) to the subscriber in the indication signal
 *      XPAI_UCC_DELIVER_IND.
 *
 *      The physical layer for the HDLC UCC protocol is the XP RS485 link.
 *
 * Restriction:
 *      The HDLC Tributary station is only present on AUM2.
 *      On AUM1 this function will only return XPAI_UCC_SUBSCRIBE_NOK_SUPPORT.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
S32 XPAI_UCCSubscribe(U32 pid, U8 hdlcAddr); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_UCCSend
 *
 * Parameters:
 *      hdlcAddr  HDLC address of the HDLC Tributary station to send the user
 *                data from.
 *                Value: XPAI_UCC_HDLC_ADDR_MIN - XPAI_UCC_HDLC_ADDR_MAX.
 *      length    Length of the user data to be sent.
 *                Value: XPAI_UCC_LENGTH_MIN - XPAI_UCC_LENGTH_MAX.
 *      data      Pointer to the user data to be sent to the HDLC Control
 *                station.
 *
 * Return value:
 *      XPAI_UCC_SEND_OK
 *      XPAI_UCC_SEND_NOK_ADDR_RANGE
 *      XPAI_UCC_SEND_NOK_ADDR_SUB
 *      XPAI_UCC_SEND_NOK_LENGTH
 *      XPAI_UCC_SEND_NOK_SUPPORT
 *      XPAI_UCC_SEND_NOK_OTHER
 *
 * Description:
 *      This function is used by the client to send user data to the HDLC
 *      UCC Control station (master). The data is sent by a HDLC UCC Tributary
 *      station (slave). Note that HDLC UCC is an unreliable protocol and the
 *      client will therefore not know whether the data is successfully
 *      transferred or not.
 *
 *      The physical layer for the HDLC UCC protocol is the XP RS485 link.
 *
 * Restriction:
 *      The HDLC address must be subscribed to before this function is called.
 *
 *      The HDLC Tributary station is only present on AUM2.
 *      On AUM1 this function will only return XPAI_UCC_SEND_NOK_SUPPORT.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
S32 XPAI_UCCSend(U8 hdlcAddr, U16 length, U8 *data); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_ConfigECPResponseDelay
 *
 * Parameters:
 *      None.
 *
 * Return value:
 *      XPAI_CONFIG_ECP_RESPONSE_DELAY_OK
 *      XPAI_CONFIG_ECP_RESPONSE_DELAY_NOK_SERVER
 *      XPAI_CONFIG_ECP_RESPONSE_DELAY_NOK_OTHER
 *
 * Description:
 *      This function introduces a delay of at least 3 ms between receiving
 *      and sending characters on the HDLC link (minimum time for HDLC_T1
 *      in the HDLC Secondary station).
 *
 *      The delay may also be introduced on new produced units with the optional
 *      board parameter SYS_ECP_RESPONSE_DELAY set to YES.
 *
 *      NOTE: The delay depends on the system tick. If the system tick is
 *      4 ms the delay will be 4-8 ms.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern S32 XPAI_ConfigECPResponseDelay(void); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_StartLink
 *
 * Parameters:
 *      port     Port number.
 *      channel  Channel number in the range 1..4. For xenon1.0 it is also used
 *               as buffer index.
 *
 * Return value:
 *      XPAI_START_LINK_OK
 *      XPAI_START_LINK_NOK_PORT
 *      XPAI_START_LINK_NOK_CHANNEL
 *      XPAI_START_LINK_NOK_OTHER
 *
 * Description:
 *      This function is used by the client to start links on channels 1 to 4.
 *
 * Restriction:
 *      The HW needs to support the ports and channels e.g. if the FPGA needs to
 *      be loaded.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
S32 XPAI_StartLink(U32 port, U32 channel); /* !- FUNC -! */

#ifdef __cplusplus
}
#endif

#endif /* _XPAI_XHP_IF_H */

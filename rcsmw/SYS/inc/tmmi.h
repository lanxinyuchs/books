/* ----------------------------------------------------------------------
 * %CCaseFile:	tmmi.h %
 * %CCaseRev:	/main/R4A/3 %
 * %CCaseDate:	2015-09-18 %
 * %CCaseDocNo: %
 * Author:      etxpeno
 *
 * Short description: Implementation of the tmmi interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015 All rights reserved.
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
 * R4A/3      2015-09-17 etxarnu     Added CS_mpIdMismatch
* ----------------------------------------------------------------------
 */


#ifndef _TMMI_H
#define _TMMI_H

#ifdef __cplusplus 
extern "C" {
#endif


#include <stdint.h>

/** @addtogroup TopologyMetaMacInterface
 * @brief Topology Meta MAC Interface
 * @details TMMI handles Board availability and meta mac addresses.
 * This IWD describes how IELL can propagate board state based on
 * connectivity availability and configured meta MACs to RBS CS.
 *
 * Topology describes both availability and reachability of
 * all known boards in the node.
 * Topology data is updated when:
 *     * Any board restarts cold.
 *     * MOM is updated with additional or removed boards.
 *     * All cables towards a board are unplugged, disabled or broken.
 *
 * Meta MAC is a mac address which is used for establishing
 * and maintaining ethernet connectivity between boards.
 * Macs are pre-generate and unique for each board.
 *
 * Each traffic type has an interface name, meta MAC
 * and a vlan for receiving (ex. <interfaceName>.<vlan>, "vei0.10").
 * PCP bits (priority) should also be set when sending.

 * @{
 */

/** @addtogroup TopologyMetaMacInterfaceDefines
 * @{
 */

/**
 * @name Defines
 * @{
 */
#define TMMI_IF_NAME_LEN 16
#define TMMI_MAC_LEN 6

typedef enum
  {
    TMMI_NORMAL,
    TMMI_EMERGENCY
  } TmmiTrafficType;

typedef enum
  {
    TMMI_BOARD_UNAVAILABLE,
    TMMI_BOARD_AVAILABLE
  } TmmiBoardAvailState;

/**
 * @name Data types
 * @{
 */

struct CS_BoardMapEntry
{
  uint8_t             mpId;
  TmmiBoardAvailState availState;
  uint8_t             numMacEntries;
  struct CS_MacEntry* macEntries;
};

struct CS_MacEntry
{
  TmmiTrafficType trafficType;
  char            interfaceName[TMMI_IF_NAME_LEN];
  uint16_t        vlan;
  uint8_t         prio;
  uint8_t         metaMacAddress[TMMI_MAC_LEN];
};

/**
 * @brief Set available board map
 * @details Set or Update board availability for all
 * boards in the node
 *
 * @param[in] numBoards number of boards in the node.
 * @param[in] presentBoards Array with boardMap
 * information of length numBoards.
 * @retval -
 */
void CS_setAvailBoardMap(uint8_t                  numBoards,
			 struct CS_BoardMapEntry* presentBoards);

/**
 * @brief Mismatch in MPID.
 * @details This call will result in escalation to Network Loader
 * 
 *
 * @retval -
 */
void CS_mpIdMismatch();

#ifdef __cplusplus 
}
#endif


#endif /* _TMMI_H */

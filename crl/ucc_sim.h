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

#ifndef UCC_SIM_H_
#define UCC_SIM_H_

#include <a4ci.h>
#include "ucc_sim.sig"

union itc_msg {
	uint32_t                      msgno;
	struct ucc_simSetBehaviorS    ucc_simSetBehavior;
	struct ucc_simRecMsgS         ucc_simRecMsg;
	struct ucc_simReplyMsgS       ucc_simReplyMsg;
	struct ucc_simSendMsgS        ucc_simSendMsg;
};

typedef union
{
  struct
  {
    uint32_t uart : 1; /* UART_OVERRUN_ERROR */
    uint32_t crc : 1; /* CRC_ERROR */
    uint32_t ctrl : 1; /* CTRL_ERROR */
    uint32_t timeout : 1; /* TIMEOUT_ERROR */
    uint32_t address : 1; /* ADDRESS_ERROR */
  }bit;
  uint32_t status;
} ucc_sim_status;

#define UCC_COUNTERS           5
#define UART_FRAME_MUL       100

/**
 * @brief
 *
 * Dump data to standard output.
 *
 * @param prefix         Prefix to be used in printout.
 * @param data           Data for printout.
 * @param size           Printout size.
  *
 * @return
 *
 */
void hexdump(char *prefix,
             uint8_t *data,
             uint32_t size);

#endif

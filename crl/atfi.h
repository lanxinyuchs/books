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

#ifndef ATFI_H_
#define ATFI_H_

/* For interoperability reasons definitions here are the same as in legacy. */

/* 6.7  Protocol revision. */
#define ATFI_PROTOCOL_REV                    3

/* 6.8  Error codes. */
#define ATFI_SIGNAL_IN_WRONG_STATE           0x01
#define ATFI_UNEXPECTED_PARAMETER_VALUE      0x02
#define ATFI_UNSUPPORTED_CAPABILITY          0x03
#define ATFI_RESOURCE_SHORTAGE               0x04
#define ATFI_WRONG_CONFIGURATION_DATA        0x05
#define ATFI_OTHER_ERROR                     0x06

/* 6.9  AU and NPU Connection states. */
#define ATFI_CONNECTED                       0x00
#define ATFI_DISCONNECTED                    0x01

/* 6.10 Type of unit. */
#define ATFI_XP                              0x00
#define ATFI_EP                              0x01
#define ATFI_NPU                             0x02
#define ATFI_AU3                             0x03
#define ATFI_XP_CPRI_I2                      0x04
#define ATFI_DU_SECONDARY                    0x05
#define ATFI_DU_PRIMARY                      0x06

/* 6.11 Thread Names. */
#define ATFI_MXP_PHYSICAL_THREAD             "MXP_"
#define ATFI_BXP_PHYSICAL_THREAD             "BXP_"
#define ATFI_EPP_PHYSICAL_THREAD             "EPP_"
#define ATFI_BXP_XCU_MAIN_PHYSICAL_THREAD    "BXP_XCU"
#define ATFI_BXP_XCU_PHYSICAL_THREAD         "BXP_XCU_"

/* 6.14 General constants. */
#define ATFI_MAX_NUMBER_OF_CLIENTS            8
#define ATFI_MAX_NUMBER_OF_CONNECTIONS       16

/* 6.15 Procedure maximum execution time constants. */
#define ATFI_CONN_ESTABLISH_RSP_MAXTIME      10
#define ATFI_ADD_CONN_MAP_RSP_MAXTIME        10
#define ATFI_REMOVE_CONN_MAP_RSP_MAXTIME     10
#define ATFI_CONNECT2_RSP_MAXTIME            10
#define ATFI_DISCONNECT_RSP_MAXTIME          10
#define ATFI_RESET_RSP_MAXTIME               10
#define ATFI_AUDIT_RSP_MAXTIME               10
#define ATFI_ADD_AU3_MAP_RSP_MAXTIME         10
#define ATFI_GET_AU3_PORT_RSP_MAXTIME        10
#define ATFI_GET_PHY_ENDPOINT_RSP_MAXTIME    10
#define ATFI_CONFIG_IDCP_A3_RSP_MAXTIME      20
#define ATFI_CONFIG_IDCP_B3_RSP_MAXTIME      20
#define ATFI_ADD_AISG_MAP2_RSP_MAXTIME       10
#define ATFI_ADD_CPRI_MAP2_RSP_MAXTIME       10

/* 6.17 String. */
#define ATFI_MAX_PRODUCT_ID_STRINGLENGTH     33
#define ATFI_MAX_UNIQUE_HW_ID_LENGTH         19

#endif

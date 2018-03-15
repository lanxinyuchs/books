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

#ifndef ATFI_HDLC_H_
#define ATFI_HDLC_H_

/* HDLC Address Field definitions. */
#define HDLC_ADDR_NO            0x00
#define HDLC_ADDR_BC            0xff

/* HDLC Control Field definitions. */
#define HDLC_SNRM               0x83
#define HDLC_DISC               0x43
#define HDLC_UA                 0x63
#define HDLC_DM                 0x0f
#define HDLC_XID                0xaf
#define HDLC_FRMR               0x87
#define HDLC_RNR                0x05
#define HDLC_RR                 0x01
#define HDLC_I                  0x00
#define HDLC_PF                 0x10

/* HDLC Control Field macros. */
#define HDLC_U_FORMAT(c)        (((c) & 0x03) == 0x03)
#define HDLC_S_FORMAT(c)        (((c) & 0x03) == 0x01)
#define HDLC_I_FORMAT(c)        (((c) & 0x01) == 0x00)
#define HDLC_U_FUNCTION(c)      ((c) & 0xef)
#define HDLC_S_FUNCTION(c)      ((c) & 0x0f)
#define HDLC_NS(c)              (((c) & 0x0e) >> 1)
#define HDLC_NR(c)              (((c) & 0xe0) >> 5)
#define HDLC_VS_VR(vs, vr)      (((vs) << 1) | ((vr) << 5))
#define HDLC_VR(vr)             ((vr) << 5)

/* HDLC Window mask. */
#define HDLC_WINDOW_MASK        0x7

#endif

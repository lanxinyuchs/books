/*
 * RBS_SIGBASE, BCP_SIGBASE and BCP_XCS_SIGBASE can be found in:
 * AI Common Signal Base for RBS 6000, 2/155 18-HRB 105 602.
 */

#ifndef XCS_SIGBASE_H_
#define XCS_SIGBASE_H_

#ifndef RBS_SIGBASE
#define RBS_SIGBASE        (0x01000000)
#endif

#ifndef BCP_SIGBASE
#define BCP_SIGBASE        (RBS_SIGBASE + 0xC000)
#endif

/* XCS signal base (0x0100E000 - 0x0100F9FF) */
#define BCP_XCS_SIGBASE    (BCP_SIGBASE + 0x2000)
#define XCS_SIGBASE        (BCP_XCS_SIGBASE)

/* Signal ranges for XCS components in signal range order */

/*
 * These numbers are reserved for XPAI and XDAI:
 *
 * XPAI_SELF_TEST_IND         (0x0100E080)
 * XPAI_FAULT_IND             (0x0100E090)
 * XPAI_LOAD_FILE_DATA_IND    (0x0100E0A0)
 * XPAI_LOAD_FILE_DELETE_IND  (0x0100E0A1)
 * XPAI_SUBSCRIBE_IND         (0x0100E200)
 * XPAI_DISTRIBUTE_IND        (0x0100E201)
 * XPAI_DELIV_IND             (0x0100E205)
 * XPAI_SENDRECEIVE_SPI_IND   (0x0100EB82)
 * XDAI_MAINTENANCE_STATE_IND (0x0100F700)
 *
 */

/* 0x0100E000 - 0x0100E0FF, excluding the ones reserved for XPAI. */
#define LMC_MSG_BASE       (XCS_SIGBASE + 0x0000)

/* 0x0100E100 - 0x0100E1FF */
#define EQMH_MSG_BASE      (XCS_SIGBASE + 0x0100)

/* 0x0100E200 - 0x0100E2FF, excluding the ones reserved for XPAI. */
#define EVTI_MSG_BASE      (XCS_SIGBASE + 0x0200)

/* 0x0100E300 - 0x0100E3FF */
#define NVPI3_MSG_BASE     (XCS_SIGBASE + 0x0300)

/* 0x0100E400 - 0x0100E4FF */
#define ATFMI_MSG_BASE     (XCS_SIGBASE + 0x0400)

/* 0x0100E500 - 0x0100E5FF */
#define NODEID_MSG_BASE    (XCS_SIGBASE + 0x0500)

/* 0x0100E600 - 0x0100E6FF */
#define ELOG_MSG_BASE      (XCS_SIGBASE + 0x0600)

/* 0x0100E700 - 0x0100E7FF */
/* Unused */

/* 0x0100E800 - 0x0100E8FF */
/* Unused */

/* 0x0100E900 - 0x0100E9FF */
/* Unused */

/* 0x0100EA00 - 0x0100EAFF */
/* Unused */

/* 0x0100EB00 - 0x0100EBFF, excluding the one reserved for XPAI. */
/* Unused */

/* 0x0100EC00 - 0x0100ECFF */
/* Unused */

/* 0x0100ED00 - 0x0100EDFF */
/* Unused */

/* 0x0100EE00 - 0x0100EEFF */
/* Unused */

/* 0x0100EF00 - 0x0100EFFF */
/* Unused */

/* 0x0100F000 - 0x0100F9FF, excluding the one reserved for XDAI. */
#define RHD_MSG_BASE       (XCS_SIGBASE + 0x1000)

#endif

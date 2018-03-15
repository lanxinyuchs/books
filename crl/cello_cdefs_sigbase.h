/**
 *    Signal base values for Radio Monitor Software
 * 
 *   @file
 *   @version @(#) ClearCase ID: 
 *
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */ 

/* ========================================================================
 *   History of development:
 *   -----------------------
 *
 *   Revised : 1997-04-02
 *   Change  : First version.
 *
 *   Revised : 1997-04-17
 *   Change  : Added SIGBASE for RMDL.
 *
 *   Revised : 1997-05-13
 *   Change  : Adding BBTT_SIGBASE.
 *
 *   Revised : 1997-05-23
 *   Change  : Changed RMMH to OMMH.
 *
 *   Revised : 1997-05-30
 *   Change  : BSSW sigbase changed for use with decimal numbers.
 *
 *   Revised : 1997-06-04
 *   Change  : Name change race.
 *
 *   Revised : 1997-06-23
 *   Change  : New base adresses for BS Subsystems.
 *
 *   Revised : 1997-10-10
 *   Change  : Added signal base for REPDC.
 *
 *   Revised : 1997-10-21
 *   Change  : Added signal domain for proxy stubs.
 *
 *   Revised : 1997-10-26
 *   Change  : Added BBSIM_SIGBASE.
 *
 *   Revised : 199711-11
 *   Change  : Moved the SOFTIF SIGBASE within the correct signal domain.
 *
 *   Revised : 1997-11-27
 *   Change  : Added sigbase for OMAOC, TTIPCS, REPCS and YATT.
 *
 *   Revised : 1997-11-28
 *   Change  : Added sigbase for OMAOM. Changed name of TTIPCS to TTDSH. 
*              Removed not needed block REPCS.
 *
 *   Revised : 1998-02-16
 *   Change  : Adding MSSIM_SIGBASE for the mobile station simulator.
 *
 *   Revised : 1998-02-20
 *   Change  : Increase sigbase for mS Control SW block MSSYH.
 *
 *   Revised : 1998-03-11 
 *   Change  : Added sigbase for REIFXH and TTMMXH.
 *
 *   Revised : 1998-05-29 qplmija
 *   Change  : Extended signal base for APIM and added signal space for BBUDP.
 *             APIM_SIGBASE is now 400 signals.
 *             BBUDP_SIGBASE is 100 signals.
 *
 *   Revised : 1998-06-18 eplent
 *   Change  : TMH_SIGBASE has been added.
 *
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

#ifndef CELLO_CDEFS_SIGBASE_H
#define CELLO_CDEFS_SIGBASE_H
 
#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/** 
 * Signal domain for BS applications is according to document: 
 * 5/1013-FEA 202 1016 Uen defined to: 0x300000 to 0x3FFFFF.
 *
 * This signal domain has been further divided among the BS 
 * subsystems as follows:
 * RES 0x300000-0x301FFF i.e. 3 145 728 - 3 153 919
 * TTS 0x302000-0x302FFF i.e. 3 153 920 - 3 158 015
 * OMS 0x303000-0x303FFF i.e. 3 158 016 - 3 162 111
 * THS 0x304000-0x304FFF i.e. 3 162 112 - 3 166 207
 */


/** 
 * Base address for subsystem OM's signal domains.
 *
 * The DDS shows the signal numbers as decimal, which means
 * that they are easier to understand if they are related to a
 * round figure. This means that the OM's sigbase have been set to 
 * decimal 3 160 000 (instead of 0x303000).
 *
 * Note that this reduces the original space of 4096 (0xFFF) signals.
 */

#define OM_SIGBASE     (3160000)

#define OMMH_SIGBASE   (OM_SIGBASE + 200)   /* 200 signals */
#define OMCM_SIGBASE   (OM_SIGBASE + 400)   /* 200 signals */
#define OMFM_SIGBASE   (OM_SIGBASE + 600)   /* 200 signals */
#define OMMAX_SIGBASE  (OM_SIGBASE + 800)   /* 200 signals */
#define OMDL_SIGBASE   (OM_SIGBASE + 1000)  /* 200 signals */
#define UNUSED_SIGBASE (OM_SIGBASE + 1200)  /* 200 signals, unused, old APIM */
#define OMCSF_SIGBASE  (OM_SIGBASE + 1400)  /* 200 signals */
#define OMAOC_SIGBASE  (OM_SIGBASE + 1600)  /* 200 signals */
#define OMAOM_SIGBASE  (OM_SIGBASE + 1800)  /* 200 signals */
#define OMAPIM_SIGBASE (OM_SIGBASE + 2000)  /* 400 signals */

/** 
 * Base address for subsystem TT's signal domains.
 *
 * The DDS shows the signal numbers as decimal, which means
 * that they are easier to understand if they are related to a
 * round figure. This means that the OM's sigbase have been set to 
 * decimal 3 154 000 (instead of 0x302000).
 *
 * Note that this reduces the original space of 4096 (0xFFF) signals.
 */

#define TT_SIGBASE     (3154000)

#define TTDH_SIGBASE   (TT_SIGBASE + 200)     /* 200 signals */
#define TTSMXH_SIGBASE (TT_SIGBASE + 400)     /* 200 signals */
#define TTTUH_SIGBASE  (TT_SIGBASE + 600)     /* 200 signals */
#define TTTUC_SIGBASE  (TT_SIGBASE + 800)     /* 200 signals */
#define TTAH_SIGBASE   (TT_SIGBASE + 1000)    /* 200 signals */
#define TTARH_SIGBASE  (TT_SIGBASE + 1200)    /* 200 signals */
#define TTDSH_SIGBASE  (TT_SIGBASE + 1400)    /* 200 signals */
#define TTMMXH_SIGBASE (TT_SIGBASE + 1600)    /* 200 signals */
#define TTMH_SIGBASE   (TT_SIGBASE + 1800)    /* 200 signals */


/** 
 * Base address for subsystem RE's signal domains.
 *
 * The DDS shows the signal numbers as decimal, which means
 * that they are easier to understand if they are related to a
 * round figure. This means that the OM's sigbase have been set to 
 * decimal 3 145 000 (instead of 0x300000).
 *
 * Note that this reduces the original space of 8192 (0x1FFF) signals.
 */

#define RE_SIGBASE     (3145000)

#define REREC_SIGBASE  (RE_SIGBASE + 200)   /* 400 signals */
#define RERH_SIGBASE   (RE_SIGBASE + 600)   /* 200 signals */
#define RESH_SIGBASE   (RE_SIGBASE + 800)   /* 200 signals */
#define RECPCH_SIGBASE (RE_SIGBASE + 1000)  /* 200 signals */
#define REDPCH_SIGBASE (RE_SIGBASE + 1200)  /* 200 signals */
#define REPDC_SIGBASE  (RE_SIGBASE + 1400)  /* 200 signals */
#define REIFXH_SIGBASE (RE_SIGBASE + 1600)  /* 200 signals */


/**
 * Base address for the Mobile Station Simulator
 */

#define MSSIM_SIGBASE (3300000)

#define MSMH_SIGBASE  (MSSIM_SIGBASE + 200)   /* 200 signals */
#define MSSYH_SIGBASE (MSSIM_SIGBASE + 400)   /* 200 signals */


/** 
 * Base addresses for other signal domains
 */

/**
 * Yet Another Test Tool signal domain
 */
#define YATT_SIGBASE   (0x3C0000)

/**
 *  Proxy stubs signal domain
 */
#define SOFTIF_SIGBASE (0x3D0000)

/**
 *  Test Engine signal domain 
 */
#define OMTE_SIGBASE   (0x3E0000)

/**
 *  Base Band Test Tool signal domain 
 */
#define BBTT_SIGBASE   (0x3F0000)

/**
 *  Base Band simulator signal domain 
 */
#define BBSIM_SIGBASE  (BBTT_SIGBASE + 100) /* 100 signals */

/**
 *  Base Band User Data Protocol signal domain
 */
#define BBUDP_SIGBASE  (BBTT_SIGBASE + 200) /* 100 Signals */

/** 
 * End of double inclusion protection
 */

#ifdef __cplusplus
}
#endif

#endif /* CELLO_CDEFS_SIGBASE_H */

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */

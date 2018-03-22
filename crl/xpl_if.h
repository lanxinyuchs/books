typedef unsigned int U32;
typedef unsigned short U16;
typedef unsigned char U8;

/******************************************************************************
 *
 *	COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *    	The copyright to the computer program(s) herein is the property
 *    	of Ericsson Radio Systems AB.
 *
 *    	The program(s) may be used and/or copied only with the written
 *    	permission from Ericsson Radio Systems AB or in accordance with
 *    	the terms and conditions stipulated in the agreement/contract
 *    	under which the program(s) have been supplied.
 *
 *****************************************************************************/

/******************************************************************************
 *
 * Product name:         
 *     	XPL
 *
 * File:         
 *	xpl_if.h
 *	
 * Author:       
 *	Rickard Fahlquist (QRAFAST)    23 May 2000          
 *
 * Description:  
 *	The xpl interface.
 *
 * Reviewed:
 * 
 *
 * Revision history:  
 *		
 *****************************************************************************/
 
#ifndef XPL_IF_H
#define XPL_IF_H

/*----------------------------  Include files  ------------------------------*/
#include "iboot_if.h"
/*----------------------------  CONSTANTS  ----------------------------------*/
/* Types used by XPL to label different parts of memory. 
 * The labels are passed on to, and used by, XOS. 
 */
#define XPL_PROG_DATA_AREA_TYPE         ((U32)1)
#define XPL_RPDOUT_OFFSET_TYPE          ((U32)5)

#define XPL_ALIGN_MASK                  (U32)0xff
#define XPL_SW_PID_LENGTH               ((U32)33)

/*----------------------------  MACROS  -------------------------------------*/
 
/*----------------------------  Structs and typedefs  -----------------------*/
/******************************************************************************
 * Type:
 *       XplDramElemS_t
 *
 * Description:
 *              Node in the linked list holding the info to be stored in
 *              the XPL/Program data area.
 *              The info consists of the RAM memory map and pointers to the
 *              RPDOUT-files loaded in RAM by XPL.
 *              What the different nodes describe is determined by the value 
 *              of the entry 'type' which is one of the defines above.
 *              To find e.g. offset and size of the Core dump area, the list 
 *              must be traversed until the type "XPL_CORE_DUMP_AREA_TYPE" is  
 *              found.
 *
 *****************************************************************************/
typedef struct XPL_DramElemS {
  U32                  size;
  U32                  type; 
  U8                   *offset;
  struct XPL_DramElemS *nextElem;
} XPL_DramElemS_t; 
 
/* The struct that holds the information passed on as argument to the
 * loaded file
 */
typedef struct XPL_ExportDataS { 
  U32       revNo;    /* Xpl revision number. Used for distinguishing
                       * different versions of XPL interfaces. */
  U32       auType;                                       /* From IbootHdr */
  U32       reserved1;
  U32       ibootStartRestartCause;                       /* From iboot */
  U32       startRestartCause;                            /* From iboot */ 
  U32       protectBootStatus;                            /* From iboot */
  U32       abootStatus;                                  /* From iboot */
  U32       abootReturnValue;                             /* From iboot */
  U8        reserved2;
  U8        reserved3;
  U8        ibootSwPid[IBOOT_SW_PID_LENGTH];              /* From iboot */
  U8        xplSwPid[XPL_SW_PID_LENGTH];
  U8        *pointerToStartedProg;
  char      loadFileSwPid[XLF_IBOOT_HDR_SUID_LEN];        /* From IbootHdr */
  IBOOT_XpMemMapElementS_t xpMemMap[XP_MEM_MAP_ELEMENTS]; /* From iboot */
  XPL_DramElemS_t          *ramMapList;
  
} XPL_ExpDataS_t;
 
/* The rpdout file header */
typedef struct XPL_RpdoutHeaderS{
  U16 hPsVolume;
  U16 hDsVolume;   
  U16 hCategory;
  U16 hState;   
  U32 hCode;
  U32 hDs;
  U32 hEntry;
  U32 hFormat;
  U32 hVar;
  U32 hNovram;
  U32 hText;
  U32 hData;
  U32 hBss;
  U32 hRsize;
  U32 hSymb;
} XPL_RpdoutHeaderS_t;  /*lint !e768*/

/* The rpdout file tail */
typedef struct XPL_RpdoutTailS{
  U16 tDummy;
  U8  tSuid[32];
  U16 tChsum;
} XPL_RpdoutTailS_t;  

typedef void XPL_ApplicProgP_t(XPL_ExpDataS_t *xplImportData);

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/


#endif /* XPL_IF_H */
 

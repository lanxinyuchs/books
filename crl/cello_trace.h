/*
 *
 *   Interface to the trace functions.
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
 *   Revised: 1997-04-26 Göran Nordin
 *   Change:  First version
 *
 *   Revised: 2009-05-07 Lennart Cormin EAB/FJP/HM
 *   Change : Added new bios code CS_ALLOC_MULTIPLE_TRACE_V. This function
 *            can only be used internally by trace so there is no new proxy
 *            function added.
 *
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

#ifndef CELLO_TRACE_H
#define CELLO_TRACE_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/**
 * Bios fuction codes for the different calls.
 */
#define CS_CREATE_TRACE_PC          1
#define CS_KILL_TRACE_I             2
#define CS_CLEAR_TRACE_I            3
#define CS_ENABLE_TRACE_I           4
#define CS_DISABLE_TRACE_I          5 
#define CS_ALLOC_TRACE_PV           6
#define CS_GET_TRACE_LIST_PR        7
#define CS_FIND_TRACE_I             8
#define CS_GET_TRACE_I              9
#define CS_ALLOC_MULTIPLE_TRACE_V  10

/**
 * Constants for trace status.
 */
#define CS_TRACE_NFOUND   0
#define CS_TRACE_EMPTY    1
#define CS_TRACE_NEMPTY   2

#define CS_MAX_TRACE_NAME_LEN 16
				/* Max number of bytes in the trace	*/
				/* name string.				*/
#define CS_MAX_TRACE_STATUS_LEN 8
				/* Max number of bytes in the trace	*/
				/* status string.			*/

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

struct Cs_traceTStamp
    {
    unsigned long ticks_ul;
    unsigned long usecs_ul;
    };
				/* This structure is used to hold the	*/
				/* time stamp for each trace entry.	*/
				/* It is present in the status struct.	*/
				/* returned by Cs_findTrace_i() and	*/
				/* Cs_getTrace_i().			*/
				/* The Cs_traceTStamp contains the	*/
				/* number of system ticks and micro	*/
				/* seconds since system restart.       	*/
				/* The OSE Delta system call 		*/
				/* system_tick can be used to convert	*/
				/* system ticks to micro seconds.	*/

typedef unsigned short Cs_traceESize;
				/* The type is used to define the	*/
				/* entry_size field in the trace_state	*/
				/* structure.				*/

/**
 * This structure is used when reading the trace.
 * A user must allocate such a structure and pass it to the Cs_findTrace_i()
 * and CS_getTrace_i() functions.
 * The structure need not be initialized by the caller.
 * The traceEntry_pc, entrySize_t, Cs_timeStamp_r, stampValid_uc and base_pc
 * fields contain data for the caller to read.
 * Actual trace entry contents must be fetched by the caller by
 * using the trace entry address traceEntry_pc, as a pointer.
 * The Pointer to trace base base_pc can be used in susequent calls to
 * Cs_killTrace_i, Cs_clearTrace_i and Cs_enableTrace_i. 
 */

struct Cs_traceState
    {
    char *traceEntry_pc;       	  /* Pointer to current trace entry.	  */
    Cs_traceESize entrySize_t;    /* No of bytes in current entry.	  */
    struct Cs_traceTStamp timeStamp_r;
                                  /* Time when current entry was stored.  */
    unsigned char stampValid_uc;  /* Non zero when timeStamp_r is present.*/
    char *base_pc;		  /* Pointer to trace base.	  	  */
    char traceName_z[CS_MAX_TRACE_NAME_LEN + 1];
                                  /* Name of the trace buffer.	  	  */
    };

/**
 * This structure is used when retrieving the names of all trace buffers.
 */
struct Cs_traceList
{
  unsigned long area_ul;          /* The number of bytes in memory	  */
	  	  	  	  /* reserved for trace.		  */
  unsigned long freeSpace_ul;     /* The number of bytes that is not yet  */
	  	  	  	  /* occupied by trace buffers.		  */
  unsigned long maxFreeSize_ul;	  /* The maximum free size that can be	  */
	 	 	 	  /* used when creating a trace buffer.	  */
	 	 	 	  /* This size can be smaller than	  */
 	  	  	  	  /* freeSpace_ul due to fragmentation.	  */
  unsigned long count_ul;	  /* The number of trace buffers.	  */

  struct
  {
    unsigned long size_ul;	  /* The size of trace buffers.		  */
    char status_z[CS_MAX_TRACE_STATUS_LEN + 1];
                                  /* status of the trace buffer.  	  */
    char name_z[CS_MAX_TRACE_NAME_LEN + 1];
                                  /* Name of the trace buffer.	  	  */
  } buffer_ar[1];
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/************************************************************************
 *
 *  Function name : Cs_createTrace_pc
 *
 *  Description   : Allocates and initializes a trace buffer.
 *                  The trace is cleared but not enabled.
 *                  If a trace buffer with the same name, as given in name_z,
 *                  already exists then that trace buffer is freed and a new
 *                  one is allocated.
 *
 *  Arguments     : size_ul  The number of bytes in the trace buffer.
 *		    name_z   Name_z is an ascii string used to identify this
 *                           trace buffer. The name may be set to "" if
 *                           several buffers are not needed.
 *                           The name must not be longer than
 *                           CS_MAX_TRACE_NAME_LEN.
 *                
 *  Return        : Returns the base of the trace buffer. NULL is returned
 *                  if memory cannot be allocated for the trace buffer.
 *
 ************************************************************************/
char *Cs_createTrace_pc(unsigned long size_ul, const char *name_z);


/************************************************************************
 *
 *  Function name : Cs_killTrace_i
 *
 *  Description   : Frees a trace buffer.
 *
 *  Arguments     : base_pc  The base of the trace buffer.
 *
 *  Return        : Returns a zero value if base_pc is a valid base of a
 *		    trace buffer, otherwise a non zero value is returned.
 *
 ************************************************************************/
int Cs_killTrace_i(char *base_pc);


/************************************************************************
 *
 *  Function name : Cs_clearTrace_i
 *
 *  Description   : Removes all data from the trace buffer.
 *                  Trace enable status is not affected.
 *                  The trace buffer must not be re-created or killed while
 *                  this call is in progress.
 *
 *  Arguments     : base_pc  The base of the trace buffer.
 *
 *  Return        : Returns a zero value if base_pc is a valid base of a
 *		    trace buffer, otherwise a non zero value is returned.
 *
 ************************************************************************/
int Cs_clearTrace_i(char *base_pc);


/************************************************************************
 *
 *  Function name : Cs_enableTrace_i
 *
 *  Description   : Enables a trace buffer for trace data storage.
 *                  Subsequent calls to Cs_allocTrace_pv and Cs_freeTrace_v
 *                  causes data to be stored in the trace buffer.
 *                  The trace buffer must not be re-created or killed while
 *                  this call is in progress.
 *
 *  Arguments     : base_pc  The base of the trace buffer.
 *
 *  Return        : Returns a zero value if base_pc is a valid base of a
 *		    trace buffer, otherwise a non zero value is returned.
 *
 ************************************************************************/
int Cs_enableTrace_i(char *base_pc);


/************************************************************************
 *
 *  Function name : Cs_disableTrace_i
 *
 *  Description   : Disables a trace buffer for trace data storage.
 *                  Subsequent calls to Cs_allocTrace_pv returns NULL
 *                  The trace buffer must not be re-created or killed while
 *                  this call is in progress.
 *
 *  Arguments     : base_pc  The base of the trace buffer.
 *
 *  Return        : Returns a zero value if base_pc is a valid base of a
 *		    trace buffer, otherwise a non zero value is returned.
 *
 ************************************************************************/
int Cs_disableTrace_i(char *base_pc);


/************************************************************************
 *
 *  Function name : Cs_allocTrace_pv
 *
 *  Description   : Reserves space in the trace for a trace entry
 *                  if trace data storage is enabled.
 *                  If the trace buffer is enabled then it must not be
 *                  re-created or killed while this call is in progress.
 *
 *  Arguments     : base_pc      The base of the trace buffer.
 *                  entrySize_t  Is the number of bytes to be allocated.
 *
 *  Return        : Returns a pointer to allocated entry or NULL if trace
 *                  data storage is disabled.
 *
 ************************************************************************/
void *Cs_allocTrace_pv(char *base_pc, Cs_traceESize entrySize_t);


/************************************************************************
 *
 *  Function name : Cs_freeTrace_v
 *
 *  Description   : Returns a previously allocated trace entry to
 *                  the trace. The trace entry becomes visible for subsequent
 *                  trace unpacking via calls Cs_findTrace_i and Cs_getTrace_i.
 *                  The trace buffer must not be re-created or killed while
 *                  this call is in progress.
 *
 *  Arguments     : entry_pv  Address of pointer to previously allocated trace
 *                            entry.The pointer is set to NULL by this call. 
 *
 *  Return        : None.
 *
 ************************************************************************/
void Cs_freeTrace_v(void **entry_pv);


/************************************************************************
 *
 *  Function name : Cs_getTraceList_pr
 *
 *  Description   : Lists the names of all trace buffers in the system
 *                  and if they are enabled.
 *                  The caller is responsible of freeing the returned
 *                  buffer via the call free_buf.
 *
 *  Arguments     : None.
 *                
 *  Return        : Returns a list containing names of all trace buffers
 *		    in the system.
 *                  if there are no trace buffers then
 *                  Cs_traceList.count_ul is set to zero.
 *
 ************************************************************************/
struct Cs_traceList *Cs_getTraceList_pr(void);


/************************************************************************
 *
 *  Function name : Cs_findTrace_i
 *
 *  Description   : If the trace is found and not empty then data storage is
 *		    disabled and the oldest trace entry is located.
 *                  Information for the oldest trace entry is read into the
 *                  firstState_pr structure.
 *		    The trace must explicitly be enabled, via Cs_enableTrace_i,
 *		    after it has been examined through Cs_findTrace_i, i e if
 *                  Cs_findTrace_i returns CS_TRACE_NEMPTY or CS_TRACE_EMPTY,
 *                  and Cs_getTrace_i.
 *
 *  Arguments     : name_z         Name_z is an ascii string used to identify
 *                                 this trace buffer. The name may be set to ""
 *                                 if several buffers are not needed.
 *                                 The name must not be longer than
 *                                 CS_MAX_TRACE_NAME_LEN.
 *
 *		    firstState_pr  Is a pointer to the control structure used
 *				   when unpacking the trace.
 *
 *  Return        : CS_TRACE_NFOUND  when a trace buffer with name name_z
 *                                   could not be found. firstState_pr does 
 *                                   not contain any usefull data when
 *                                   CS_TRACE_NFOUND is returned.
 *                  CS_TRACE_EMPTY   when an empty recovered trace buffer is
 *                                   found. Fields base_pc and traceName_z
 *                                   in firstState_pr contains valid data
 *                                   when CS_TRACE_EMPTY is returned.
 *                  CS_TRACE_NEMPTY  when a recovered trace buffer that is not
 *                                   empty is found. The trace is
 *                                   automatically disabled and all fields in
 *                                   firstState_pr contains valid data when
 *                                   CS_TRACE_NEMPTY is returned.
 *
 ************************************************************************/
int Cs_findTrace_i(const char *name_z, struct Cs_traceState *firstState_pr);


/************************************************************************
 *
 *  Function name :  Cs_getTrace_i
 *
 *  Description   : Reads the next trace entry into the currentState_pr
 *		    structure.
 *
 *  Arguments     : currentState_pr  Is a pointer to the control structure
 *				     used when unpacking the trace.
 *				     This is the same structure as in the
 * 				     Cs_findTrace_i call.
 *
 *  Return        : Returns a zero value if there are no more entries.
 *
 ************************************************************************/
int Cs_getTrace_i(struct Cs_traceState *currentState_pr);


#endif /* CELLO_TRACE_H */

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */

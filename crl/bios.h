#ifndef _BIOS_H
#define _BIOS_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 ****************************************************************************
 * MANIFEST CONSTANTS.
 ****************************************************************************
 */
#define BIOS_NAME_SIZE          24 

/*
 *---------------------------------------------------------------------------
 * The biosInstall flags
 *---------------------------------------------------------------------------
 */
#define BIOS_SUPERVISOR_ONLY    0
#define BIOS_USERMODE_ALLOWED   1 
#define BIOS_RESERVED_BY_KERNEL 2 
#define BIOS_FORCE_INSTALL      4 
#define BIOS_SUPPORTS_FCDEVCLASS_CONVENTION 8 

/*
 *---------------------------------------------------------------------------
 * The biosInstall and biosList success and error codes.
 *---------------------------------------------------------------------------
 */
#define BIOS_SUCCESS            0
#define BIOS_ENO_TABLE_SPACE    1 
#define BIOS_EDUPLICATE_NAME    2 
#define BIOS_ENAME_TOO_LONG     3
#define BIOS_EUSER_MODE         4 
#define BIOS_ENO_ENTRY          5 

/*
 ****************************************************************************
 * 6  TYPES.
 ****************************************************************************
 */

typedef long (BiosFunction)(long, long, long, long, long, long, long);

struct BiosList {
  BiosFunction *entrypoint;
  long runlevel;
  unsigned long handle;
  unsigned long flags;
  char name[BIOS_NAME_SIZE];
};

/*
 ****************************************************************************
 * 7 GLOBAL FUNCTIONS
 ****************************************************************************
 */
long 
biosInstall(const char *name, BiosFunction *entrypoint, unsigned long flags);

unsigned long 
biosOpen(const char *name);

long 
biosCall(unsigned long handle, ... /* arg1..arg7 */);

#ifdef __cplusplus
}
#endif

#endif /* # ifndef _BIOS_H */

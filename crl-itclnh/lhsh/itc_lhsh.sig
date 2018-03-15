#ifndef _LHSH_SIG
#define _LHSH_SIG
  
#include <itc.h>

/* From ose.h */
#define OS_ATTACH_SIG (252)

#define FM_SUCCESS 0

typedef unsigned long U32;
typedef long S32;
typedef unsigned short U16;
typedef unsigned char U8;
typedef U32 FmHandle;
typedef U32 EfsEnum;
typedef U32 EfsBoolean;
typedef U32 EfsStatus;
typedef U32 EfsBitmask;

#define EFS_SIGBASE 30300 /* Start of internal EFS signals. */
#define FM_SIGBASE 30100 /* Start of FM/VM signal numbers. */

/* Types of operations (bitmask). Used when cancelling pending (blocking) operations. */
#define FM_OPER_READ       0x01 /* Cancel all pending read operations. */
#define FM_OPER_WRITE      0x02 /* Cancel all pending write operations. */
#define FM_OPER_WAIT       0x04 /* Cancel all pending wait operations. */
#define FM_OPER_DRAIN      0x08 /* Cancel all pending drain operations. */
#define FM_OPER_UPDATES    0x10 /* Cancel all pending (unsynched) writes */ 

/***********************************************************
 *
 * SIGNAL INTERFACE
 *
 ***********************************************************/
/* Sent from child to parent; completion signal with status and error codes. */
#define CHILD_DONE     (EFS_SIGBASE + 1)    /* !-SIGNO(struct ChildDone)-! */
struct ChildDone
{
    uint32_t sigNo;
    EfsStatus status;                   /* The status of the executed command. */
    long error;                      /* Errno value when the child terminated. */
};  
  
#define SHELL_INTERFACE_REQUEST   (EFS_SIGBASE + 40)       /* !-SIGNO(struct ShellInterfaceRequest)-! */
#define SHELL_INTERFACE_REPLY     (EFS_SIGBASE + 41)       /* !-SIGNO(struct ShellInterfaceReply)-! */
  struct ShellInterfaceRequest
  {
      uint32_t sigNo;
  };
  struct ShellInterfaceReply
  {
      uint32_t sigNo;
      EfsStatus status;           /* O: Status of the operation. */
      char whatStr[32];           /* O: What string of product. */
      uint32_t biosHandle;             /* O: BIOS handle for trap interface. */
      uint32_t sigs[1];          /* O: Supported signals. Variable size. */
  };  
  
#define SHELL_CREATE_CHILD_REQUEST  (EFS_SIGBASE + 42) \
    /* !-SIGNO(struct ShellCreateChild)-! */
#define SHELL_CREATE_CHILD_REPLY    (EFS_SIGBASE + 43) \
    /* !-SIGNO(struct ShellCreateChild)-! */

struct ShellCreateChild
{
  uint32_t sigNo;
  EfsStatus status;    /* O: Status of the operation. */
  uint32_t child_pid;  /* O: Local pid of the created process. */
  uint32_t att_ref;    /* O: Attref to be specified in start req. */
  uint32_t options;    /* I: Options (none are yet defined). */
  uint32_t proc_type;  /* I: Default process type. */
  uint32_t priority;   /* I: Default priority. */
  uint32_t user_id;    /* I: User ID. */
  uint32_t cmd_name;   /* I: command name (offset into string pool). */
  uint32_t proc_name;  /* I: process name (offset into string pool). */
  uint32_t argv;       /* I: argument vector (offset into string pool). */
  uint32_t wdir;       /* I: working directory (offset into string pool). */
  uint32_t user_name;  /* I: user name (offset into string pool). */
  uint32_t terminal ;  /* I: terminal (offset into string pool). */
  char s[6];           /* IO: String pool (variable size). */
};

  /* Set an environment variable on the specified process. */
  
#define SHELL_SETENV_CHILD (EFS_SIGBASE + 44) /* !-SIGNO(struct ShellSetenvChild)-! */
  struct ShellSetenvChild
  {
      uint32_t sigNo;
      uint32_t child_pid;          /* I: Local pid of process to receive environment variable. */
      char assignment[1];         /* I: "var=name" (variable size). */
  };
 
  /* Set an environment variable on the specified process. */
  
#define SHELL_SETENV_CHILD_SENDER (EFS_SIGBASE + 45) /* !-SIGNO(struct ShellSetenvChildSender)-! */
  struct ShellSetenvChildSender
  {
      uint32_t sigNo;
      uint32_t child_pid;          /* I: Local pid of process to receive environment variable. */
      char assignment[1];         /* I: "var=name" (variable size). */
  };
  
  /* Start the specified process. */
#define SHELL_START_CHILD          (EFS_SIGBASE + 46) /* !-SIGNO(struct ShellStartChild)-! */
  struct ShellStartChild
  {
      uint32_t sigNo;
      uint32_t child_pid;          /* I: Local pid of child process to start. */
      uint32_t att_ref;           /* I: Attref returned in create child reply. */
  };
  
#define FM_INTERFACE_REQUEST  (30100) /* !-SIGNO(struct FmInterfaceRequest)-! */
#define FM_INTERFACE_REPLY    (30101)

struct FmInterfaceReply
{
   uint32_t sigNo;
   EfsStatus status;
   char      whatStr[32];
   U32       biosHandle;
   uint32_t sigs[1];
};

#define FM_DUP_HANDLE_REQUEST    (30170)    /* !-SIGNO(struct FmDupHandle)-! */

#define FM_DUP_HANDLE_REPLY      (30171)    /* !-SIGNO(struct FmDupHandle)-! */

struct FmDupHandle
{
   uint32_t sigNo;
   EfsStatus status;
   FmHandle  oldHandle;
   FmHandle  newHandle;
};

#define FM_READ_REQUEST (FM_SIGBASE + 58) /* !-SIGNO(struct FmReadRequest)-! */
#define FM_READ_REPLY   (FM_SIGBASE + 59) /* !-SIGNO(struct FmReadReply)-! */
 
 struct FmReadRequest
 {
   uint32_t sigNo;
   FmHandle handle;   /* I: Handle of open object. */
   EfsEnum position;  /* I: Position in file to read from (FM_POSITION_XXX). */
   EfsBoolean block;  /* I: Block until satisfied. */
   U32 requested;     /* I: Number of bytes to read. */
 };
 
 struct FmReadReply
 {
   uint32_t sigNo;
   EfsStatus status;  /* O: Status of the operation. */
   FmHandle handle;   /* O: Handle of open object. */
   U32 actual;        /* O: Number of bytes actually read.*/
   char buffer[1];    /* O: The data read. Variable size. */
 };
 
#define FM_WRITE_REQUEST (FM_SIGBASE+56) /* !-SIGNO(struct FmWriteRequest)-! */
#define FM_WRITE_REPLY   (FM_SIGBASE+57) /* !-SIGNO(struct FmWriteReply)-! */
 struct FmWriteRequest
 {
   uint32_t sigNo;
   FmHandle handle;     /* I: Handle of open object. */
   EfsEnum position;    /* I: Position in file to write at (FM_POSITION_XXX). */
   U32 requested;       /* I: Number of bytes to write. */
   char buffer[1];      /* I: The data to write. Variable size. */
 };
 struct FmWriteReply
 {
   uint32_t sigNo;
   EfsStatus status;    /* O: Status of the operation. */
   FmHandle handle;     /* O: Handle of open object. */
   U32 actual;          /* O: Number of bytes actually written. */
 };


#define FM_SET_OWNER_PROC_REQUEST (FM_SIGBASE+54) /* !-SIGNO(struct FmSetOwnerProc)-! */
#define FM_SET_OWNER_PROC_REPLY   (FM_SIGBASE+55) /* !-SIGNO(struct FmSetOwnerProc)-! */

struct FmSetOwnerProc
{
   uint32_t  sigNo;
   EfsStatus status;
   FmHandle  handle;
};



#define FM_SET_POS_REQUEST             (30168)
#define FM_SET_POS_REPLY               (30169)

struct FmSetPos
{
   uint32_t sigNo;
   EfsStatus status;
   FmHandle  handle;
   EfsEnum   whence;
   S32       offset;
   U32       position;
};


#define FM_CANCEL_REQUEST		(FM_SIGBASE + 66)	/* !-SIGNO(struct FmCancel)-! */
#define FM_CANCEL_REPLY			(FM_SIGBASE + 67)	/* !-SIGNO(struct FmCancel)-! */
struct FmCancel
{
	uint32_t sigNo;
	EfsStatus status;	/* O: Status of the operation. */
	FmHandle handle;	/* I: Handle of open object. */
	U32 operations;	/* I: Operations to cancel (FM_OPER_XXX). */
	EfsBoolean global;	/* I: Cancel operations requested by all processes. */
};

#define FM_CLOSE_REQUEST		(FM_SIGBASE + 52)	/* !-SIGNO(struct FmClose)-! */
#define FM_CLOSE_REPLY			(FM_SIGBASE + 53)	/* !-SIGNO(struct FmClose)-! */
struct FmClose
{
	uint32_t sigNo;
	EfsStatus status;	/* O: Status of the operation. */
	FmHandle handle;	/* I: Handle of open object. */
	EfsBoolean cancel;	/* I: Cancel (and reply) pending read/write/wait operations. */
};

#define FM_SUBSCRIBE_SIGNAL_REQUEST (30196) /* !-SIGNO(struct FmSubscribeSignal)-! */

#define FM_SUBSCRIBE_SIGNAL_REPLY (30197) /* !-SIGNO(struct FmSubscribeSignal)-! */

struct FmSubscribeSignal
{
   uint32_t  sigNo;
   EfsStatus  status;
   FmHandle   handle;
   EfsBitmask signals;
};

#define FM_TCC_SIZE             0x20

struct FmTerminalInfo
{
   EfsBitmask    valid;
   U32           readSizeMin;
   U32           readTimeOut;
   U32           rxBuffer;
   U32           rxSpeed;
   U32           txSpeed;
   EfsBoolean    rxFlow;
   EfsBoolean    txFlow;
   U8            charBits;
   U8            stopBits;
   U8            parity;
   U8            flowCtrl;
   EfsBitmask    flags;
   U16           width;
   U16           height;
   unsigned char ctrlChars[FM_TCC_SIZE];
   U16           volumeName;
   U16           deviceName;
   U16           termType;
   char          s[4];      /*Aligned to 4 bytes*/
   U32           idleTmo;
   char          actualStringPool[3];
};

#define FM_EXAMINE_TERM_REQUEST (30192) /* !-SIGNO(struct FmExamineTermRequest)-! */

#define FM_EXAMINE_TERM_REPLY   (30193) /* !-SIGNO(struct FmExamineTermReply)-! */

struct FmExamineTermRequest
{
   uint32_t sigNo;
   FmHandle  handle;
};

struct FmExamineTermReply
{
   uint32_t             sigNo;
   EfsStatus             status;
   FmHandle              handle;
   struct FmTerminalInfo info;
};


#endif


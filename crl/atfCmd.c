/* -*- c -*- ******************************************************************
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

/******************************************************************************
 *
 * Product name:
 *      BP Test
 *
 * File:
 *      atfCmd.c
 *
 * Author:
 *      Peter Marchall
 *
 * Description:
 *      .
 *
 * Reviewed:
 *
 * Revision history:
 *      2001-12-05, Peter Marchall (QPETMAR)
 *          File created.
 *      2002-03-06, Peter Marchall (QPETMAR)
 *          Added command for auxiliary transport.
 *      2003-08-11, Peter Marchall (QPETMAR)
 *          Rewrite.
 *      2005-10-21, Peter Marchall (QPETMAR)
 *          BCP ATM.
 *      2008-07-30, Björn Svensson (XEDBJSV)
 *          Updated according to 4/15519-CEH10169/1 Revision AC.
 *      2008-08-05, Peter Marchall (XPETMAR)
 *          Updated according to 4/15519-CEH10169/1 Revision AG.
 *      2010-05-04, Peter Marchall (QPETMAR)
 *          Updated according to 4/15519-CEH10169/1 Revision PAK2.
 *
 *****************************************************************************/

/*----------------------------  Include files  ------------------------------*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "ose.h"
#include "osetypes.h"

#include "cello_cri.h"
#include "cello_te_ose.h"
#include "cello_te_trace.h"

#include "bcp_atfi.h"
#include "bcp_atfi.sig"


/*----------------------------  CONSTANTS  ----------------------------------*/

static const char *atfHelp = {
  "ATF commands:\r\n"
  "atf -start <instance>\r\n"
  "atf -kill <instance>\r\n"
  "atf -ce <instance> <server> <address> <protocol revision>\r\n"
  "atf -acm <instance> <server> <address> <logical address> <physical address>\r\n"
  "atf -aau3m <instance> <server> <address> <logical address> <au type>\r\n"
  "atf -rcm <instance> <server> <address> <logical address>\r\n"
  "atf -co <instance> <server> <address> <logical address> <type of unit>\r\n"
  "atf -dc <instance> <server> <address> <logical address>\r\n"
  "atf -reset <instance> <server> <address>\r\n"
  "atf -audit <instance> <server> <address> <logical address>\r\n"
  "atf -ads <instance> <server> <address> <baudrate> <timeout> <maskM> <mb0 ... mbM> <uidN> <ub0 ... ubN>\r\n"
  "atf -gau3p <instance> <server> <address> <logical address>\r\n"
  "atf -gauid <instance> <server> <address> <logical address>\r\n"
  "atf -aam <instance> <server> <address> <logical address> <device type> <uidN> <ub0 ... ubN>\r\n"
  "atf -acprim <instance> <server> <address> <logical address> <cascade number> <physical address>\r\n"
  "atf -gpep <instance> <server> <address>\r\n"
  "atf -gsidcpa <instance> <server> <address> <snidA> <snidB> <supa>\r\n"
  "atf -gsidcpb <instance> <server> <address> <snidA> <supa> <snidB> <cepIdB>\r\n"
  "atf -gridcpa <instance> <server> <address> <snidA> <snidB> <supa>\r\n"
  "atf -gridcpb <instance> <server> <address> <snidA> <supa> <snidB>\r\n"
};

static const char *atfLazy = {
  "ATF command examples:\r\n"
  "atf -start 0\r\n"
  "atf -kill 0\r\n"
  "atf -ce 0 MXP_0 0 3\r\n"
  "atf -acm 0 MXP_0 0 1 1\r\n"
  "atf -rcm 0 MXP_0 0 1\r\n"
  "atf -co 0 MXP_0 0 1 0\r\n"
  "atf -dc 0 MXP_0 0 0\r\n"
  "atf -reset 0 MXP_0 0\r\n"
};

/*----------------------------  MACROS  -------------------------------------*/
/*----------------------------  Structs and typedefs  -----------------------*/

union SIGNAL {
  SIGSELECT                    sigNo;
#include "bcpAtfiUnionContent.h"
};


/*----------------------------  Definition of Global Variables  -------------*/

extern OSENTRYPOINT atfClient;


/*----------------------------  Definition of Local Variables  --------------*/
/*----------------------------  Declaration of Local Functions  -------------*/
/*----------------------------  Function Definitions  -----------------------*/

static PROCESS
atfHuntClient(char *name, const char *instance)
{
  PROCESS pid = 0;

  (void) sprintf(name, "ATF_Client_%s", instance);
  (void) hunt(name, 0, &pid, 0);

  return pid;
}

static PROCESS
atfHuntServer(char *name)
{
  SIGSELECT     sel[] = { 1, 0xcafebabe };
  PROCESS       pid;
  union SIGNAL *sig;

  sig = alloc(sizeof(SIGSELECT), 0xcafebabe);
  (void) hunt(name, 0, 0, &sig);

  sig = receive_w_tmo((OSTIME) 5000, sel);
  if (sig) {
    pid = sender(&sig);
    free_buf(&sig);
  } else {
    pid = 0;
  }

  return pid;
}

static int
atfStart(int argc, char **argv)
{
  PROCESS       cPid;
  char          name[64];

  if (argc != 3) {
    printf("The Start command takes 1 parameter\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    cPid = create_process(OS_PRI_PROC, name, atfClient,
                          1024, 16, 0, 0, 0, 0, 0);
    start(cPid);
    printf("Client \"%s\" (0x%x) successfully started\r\n", name, cPid);
  } else {
    printf("Client \"%s\" (0x%x) already started\r\n", name, cPid);
  }

  return 0;
}

static int
atfKill(int argc, char **argv)
{
  PROCESS       cPid;
  char          name[64];

  if (argc != 3) {
    printf("The Kill command takes 1 parameter\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid != 0) {
    kill_proc(cPid);
    printf("Client \"%s\" (0x%x) successfully killed\r\n", name, cPid);
  } else {
    printf("Client \"%s\" does not exist\r\n", name);
  }

  return 0;
}

static int
atfConnectionEstablish(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 6) {
    printf("The Connection Establish command takes 4 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiConnEstablishReqS), ATFI_CONN_ESTABLISH_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiConnEstablishReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiConnEstablishReq.protocolRev);
  if (status != 1) {
    printf("Invalid value \"%s\" for protocol revision\r\n", argv[5]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfAddConnectionMap(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 7) {
    printf("The Add Connection Map command takes 5 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiAddConnMapReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiAddConnMapReq.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%hu", &sig->atfiAddConnMapReq.physicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for physical address\r\n", argv[6]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfAddAu3Map(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 7) {
    printf("The Add AU3 Map command takes 5 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiAddAu3MapReqS), ATFI_ADD_AU3_MAP_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiAddAu3MapReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiAddAu3MapReq.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%s", sig->atfiAddAu3MapReq.auType);
  if (status != 1) {
    printf("Invalid value \"%s\" for au type\r\n", argv[6]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfRemoveConnectionMap(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 6) {
    printf("The Remove Connect Map command takes 4 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiRemoveConnMapReqS), ATFI_REMOVE_CONN_MAP_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiRemoveConnMapReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiRemoveConnMapReq.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfConnect(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 7) {
    printf("The Connect command takes 5 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiConnect2ReqS), ATFI_CONNECT2_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiConnect2Req.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiConnect2Req.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%hu", &sig->atfiConnect2Req.typeOfUnit);
  if (status != 1) {
    printf("Invalid value \"%s\" for type of unit\r\n", argv[6]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfDisconnect(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 6) {
    printf("The Disconnect command takes 4 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiDisconnectReqS), ATFI_DISCONNECT_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiDisconnectReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiDisconnectReq.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfReset(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 5) {
    printf("The Reset command takes 3 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiResetReqS), ATFI_RESET_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiResetReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfAudit(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 6) {
    printf("The Audit command takes 4 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiAuditReqS), ATFI_AUDIT_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiAuditReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiAuditReq.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfAisgDeviceScan(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status, cnt, idx;
  U16           dummy;

  if (argc < 10) {
    printf("The AISG Device Scan command takes some parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiAisgDeviceScanReqS),
	      ATFI_AISG_DEVICE_SCAN_REQ);
  memset(sig->atfiAisgDeviceScanReq.uniqueHwIdMask, 0,
	 BCP_ATFI_MAX_UNIQUE_HW_ID_LENGTH);
  memset(sig->atfiAisgDeviceScanReq.uniqueHwId, 0,
	 BCP_ATFI_MAX_UNIQUE_HW_ID_LENGTH);

  status = sscanf(argv[4], "%lu",
		  &sig->atfiAisgDeviceScanReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu",
		  &sig->atfiAisgDeviceScanReq.baudrate);
  if (status != 1) {
    printf("Invalid value \"%s\" for baudrate\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%hu",
		  &sig->atfiAisgDeviceScanReq.timeout);
  if (status != 1) {
    printf("Invalid value \"%s\" for timeout\r\n", argv[6]);
    return 1;
  }

  status = sscanf(argv[7], "%hu", &dummy);
  if (status != 1) {
    printf("Invalid value \"%s\" for mask length\r\n", argv[7]);
    return 1;
  }
  sig->atfiAisgDeviceScanReq.uniqueHwIdMaskLength = (U8) dummy;

  for (idx = 0, cnt = 8;
       idx < sig->atfiAisgDeviceScanReq.uniqueHwIdMaskLength && cnt < argc;
       idx++, cnt++) {
	  status = sscanf(argv[cnt], "%hu", &dummy);
	  if (status != 1) {
		  printf("Invalid value \"%s\" for mask\r\n",
			 argv[cnt]);
		  return 1;
	  }
	  sig->atfiAisgDeviceScanReq.uniqueHwIdMask[idx] = dummy;
  }

  status = sscanf(argv[cnt], "%hu", &dummy);
  if (status != 1) {
    printf("Invalid value \"%s\" for unique identifier length\r\n", argv[cnt]);
    return 1;
  }
  sig->atfiAisgDeviceScanReq.uniqueHwIdLength = (U8) dummy;
  cnt++;

  for (idx = 0;
       idx < sig->atfiAisgDeviceScanReq.uniqueHwIdLength && cnt < argc;
       idx++, cnt++) {
	  status = sscanf(argv[cnt], "%hu", &dummy);
	  if (status != 1) {
		  printf("Invalid value \"%s\" for unique identifier\r\n",
			 argv[cnt]);
		  return 1;
	  }
	  sig->atfiAisgDeviceScanReq.uniqueHwId[idx] = (U8) dummy;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfGetAu3Port(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 6) {
    printf("The Get AU3 Port command takes 4 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiGetAu3PortReqS), ATFI_GET_AU3_PORT_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiGetAu3PortReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiGetAu3PortReq.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfGetAisgUniqueIdentifier(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 6) {
    printf("The Get AISG Unique Identifier command takes 4 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiGetAisgUniqueIdReqS), ATFI_GET_AISG_UNIQUE_ID_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiGetAisgUniqueIdReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiGetAisgUniqueIdReq.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfAddAisgMap(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status, i;
  U16           dummy;

  if (argc < 8 && argc > 27) {
    printf("The Add AISG Map command takes 6 to 25 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiAddAisgMap2ReqS), ATFI_ADD_AISG_MAP2_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiAddAisgMap2Req.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiAddAisgMap2Req.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%hu", &sig->atfiAddAisgMap2Req.deviceType);
  if (status != 1) {
    printf("Invalid value \"%s\" for device type\r\n", argv[6]);
    return 1;
  }

  status = sscanf(argv[7], "%hu", &dummy);
  sig->atfiAddAisgMap2Req.uniqueHwIdLength = (U8) dummy;
  if (status != 1 ||
      argc + 8 < sig->atfiAddAisgMap2Req.uniqueHwIdLength ||
      sig->atfiAddAisgMap2Req.uniqueHwIdLength > BCP_ATFI_MAX_UNIQUE_HW_ID_LENGTH) {
    printf("Invalid value \"%s\" for length\r\n", argv[7]);
    return 1;
  }

  memset(&sig->atfiAddAisgMap2Req.uniqueHwId, 0, BCP_ATFI_MAX_UNIQUE_HW_ID_LENGTH);
  for (i = 0; i < sig->atfiAddAisgMap2Req.uniqueHwIdLength; i++) {
    status = sscanf(argv[8 + i], "%hu", &dummy);
    sig->atfiAddAisgMap2Req.uniqueHwId[i] = (U8) dummy;
    if (status != 1) {
      printf("Invalid value \"%s\" for HW Identifier\r\n", argv[8 + i]);
      return 1;
    }
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfAddCpriMap(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 8) {
    printf("The Add CPRI Map command takes 6 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiAddCpriMap2ReqS), ATFI_ADD_CPRI_MAP2_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiAddCpriMap2Req.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%hu", &sig->atfiAddCpriMap2Req.logicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for logical address\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%hu", &sig->atfiAddCpriMap2Req.cascadeNo);
  if (status != 1) {
    printf("Invalid value \"%s\" for cascade number\r\n", argv[6]);
    return 1;
  }

  status = sscanf(argv[7], "%hu", &sig->atfiAddCpriMap2Req.physicalAddress);
  if (status != 1) {
    printf("Invalid value \"%s\" for physical address\r\n", argv[7]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfGetPhyEndpoint(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 5) {
    printf("The Get Phyiscal Endpoint command takes 3 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiGetPhyEndpointReqS),  ATFI_GET_PHY_ENDPOINT_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiGetPhyEndpointReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfSetupIdcpA(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 8) {
    printf("The Setup IDCP A command takes 6 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiSetupIdcpA3ReqS), ATFI_SETUP_IDCP_A3_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiSetupIdcpA3Req.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%lu", &sig->atfiSetupIdcpA3Req.snidA);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidA\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%lu", &sig->atfiSetupIdcpA3Req.snidB);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidB\r\n", argv[6]);
    return 1;
  }

  sig->atfiSetupIdcpA3Req.phyEndPointAddrB.transportType = TRANS_TYPE_ATM;

  status = sscanf(argv[7], "%lu", &sig->atfiSetupIdcpA3Req.phyEndPointAddrB.address.atm.supa);
  if (status != 1) {
    printf("Invalid value \"%s\" for supa\r\n", argv[7]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfSetupIdcpB(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 9) {
    printf("The Setup IDCP B command takes 7 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiSetupIdcpB3ReqS), ATFI_SETUP_IDCP_B3_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiSetupIdcpB3Req.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%lu", &sig->atfiSetupIdcpB3Req.snidA);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidA\r\n", argv[5]);
    return 1;
  }

  sig->atfiSetupIdcpB3Req.phyEndPointAddrA.transportType = TRANS_TYPE_ATM;

  status = sscanf(argv[6], "%lu", &sig->atfiSetupIdcpB3Req.phyEndPointAddrA.address.atm.supa);
  if (status != 1) {
    printf("Invalid value \"%s\" for supa\r\n", argv[6]);
    return 1;
  }

  status = sscanf(argv[7], "%lu", &sig->atfiSetupIdcpB3Req.snidB);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidB\r\n", argv[7]);
    return 1;
  }

  status = sscanf(argv[8], "%lu", &sig->atfiSetupIdcpB3Req.cepIdB);
  if (status != 1) {
    printf("Invalid value \"%s\" for cepIdB\r\n", argv[8]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfReleaseIdcpA(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 8) {
    printf("The Release IDCP A command takes 6 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiReleaseIdcpAReqS), ATFI_RELEASE_IDCP_A_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiReleaseIdcpAReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%lu", &sig->atfiReleaseIdcpAReq.snidA);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidA\r\n", argv[5]);
    return 1;
  }

  status = sscanf(argv[6], "%lu", &sig->atfiReleaseIdcpAReq.snidB);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidB\r\n", argv[6]);
    return 1;
  }

  sig->atfiReleaseIdcpAReq.phyEndPointAddrB.transportType = TRANS_TYPE_ATM;

  status = sscanf(argv[7], "%lu", &sig->atfiReleaseIdcpAReq.phyEndPointAddrB.address.atm.supa);
  if (status != 1) {
    printf("Invalid value \"%s\" for supa\r\n", argv[7]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
atfReleaseIdcpB(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 8) {
    printf("The Release IDCP B command takes 6 parameters\r\n");
    return 1;
  }

  cPid = atfHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = atfHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct atfiReleaseIdcpBReqS), ATFI_RELEASE_IDCP_B_REQ);

  status = sscanf(argv[4], "%lu", &sig->atfiReleaseIdcpBReq.addrInfo.linkHandle);
  if (status != 1) {
    printf("Invalid value \"%s\" for address\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "%lu", &sig->atfiReleaseIdcpBReq.snidA);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidA\r\n", argv[5]);
    return 1;
  }

  sig->atfiReleaseIdcpBReq.phyEndPointAddrA.transportType = TRANS_TYPE_ATM;

  status = sscanf(argv[6], "%lu", &sig->atfiReleaseIdcpBReq.phyEndPointAddrA.address.atm.supa);
  if (status != 1) {
    printf("Invalid value \"%s\" for supa\r\n", argv[6]);
    return 1;
  }

  status = sscanf(argv[7], "%lu", &sig->atfiReleaseIdcpBReq.snidB);
  if (status != 1) {
    printf("Invalid value \"%s\" for snidB\r\n", argv[7]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

int
atfCmd(int argc, char **argv)
{
  char cmd[64];
  int  status;

  if (argc < 2) {
    goto atf_cmd_fail;
  }

  status = sscanf(argv[1], "%s", cmd);
  if (status != 1) {
    goto atf_cmd_fail;
  }

  if (strcmp(cmd, "-start") == 0) {
    return atfStart(argc, argv);
  } else if (strcmp(cmd, "-kill") == 0) {
    return atfKill(argc, argv);
  } else if (strcmp(cmd, "-ce") == 0) {
    return atfConnectionEstablish(argc, argv);
  } else if (strcmp(cmd, "-acm") == 0) {
    return atfAddConnectionMap(argc, argv);
  } else if (strcmp(cmd, "-aau3m") == 0) {
    return atfAddAu3Map(argc, argv);
  } else if (strcmp(cmd, "-rcm") == 0) {
    return atfRemoveConnectionMap(argc, argv);
  } else if (strcmp(cmd, "-co") == 0) {
    return atfConnect(argc, argv);
  } else if (strcmp(cmd, "-dc") == 0) {
    return atfDisconnect(argc, argv);
  } else if (strcmp(cmd, "-reset") == 0) {
    return atfReset(argc, argv);
  } else if (strcmp(cmd, "-audit") == 0) {
    return atfAudit(argc, argv);
  } else if (strcmp(cmd, "-ads") == 0) {
    return atfAisgDeviceScan(argc, argv);
  } else if (strcmp(cmd, "-gau3p") == 0) {
    return atfGetAu3Port(argc, argv);
  } else if (strcmp(cmd, "-gauid") == 0) {
    return atfGetAisgUniqueIdentifier(argc, argv);
  } else if (strcmp(cmd, "-aam") == 0) {
    return atfAddAisgMap(argc, argv);
  } else if (strcmp(cmd, "-acprim") == 0) {
    return atfAddCpriMap(argc, argv);
  } else if (strcmp(cmd, "-gpep") == 0) {
    return atfGetPhyEndpoint(argc, argv);
  } else if (strcmp(cmd, "-gsidcpa") == 0) {
    return atfSetupIdcpA(argc, argv);
  } else if (strcmp(cmd, "-gsidcpb") == 0) {
    return atfSetupIdcpB(argc, argv);
  } else if (strcmp(cmd, "-gridcpa") == 0) {
    return atfReleaseIdcpA(argc, argv);
  } else if (strcmp(cmd, "-gridcpb") == 0) {
    return atfReleaseIdcpB(argc, argv);
  } else if (strcmp(cmd, "-help") == 0) {
    printf("%s", atfHelp);
    return 0;
  } else if (strcmp(cmd, "-lazy") == 0) {
    printf("%s", atfLazy);
    return 0;
  }

atf_cmd_fail:
  printf("atf -help\r\n");
  return 1;
}

void
addCmd_atf(void)
{
  CelloCri_addShellCommand("atf",
                           "atf <cmd> <...> <...> ...",
                           "Execute ATF command",
                           atfCmd);
}


/******************************************************************************
 *
 * Process name:
 *      atfClient
 *
 * Process type:
 *      Prioritized process
 *
 * Description:
 *      .
 *
 * Used global variables:
 *      None.
 *
 *****************************************************************************/

OS_PROCESS(atfClient)
{
  static const SIGSELECT  all[] = { 0 };
  union SIGNAL           *sig;
  struct OS_pcb          *pcb;
  char                   *name;
  PROCESS                 pid;

  for (;;) {

    sig = receive((SIGSELECT*) all);
    pid = sender(&sig);

    pcb = get_pcb(pid);
    if (pcb == NULL) {
      TRACE_ERROR("No PCB");
      free_buf(&sig);
      continue;
    }
    name = &pcb->strings[pcb->name];

    switch (sig->sigNo) {

      /*
      ** CLIENT TO SERVER SIGNALS.
      */

      case ATFI_CONN_ESTABLISH_REQ:
        INFO(STR("Sent ATFI_CONN_ESTABLISH_REQ(lh:%u,pr:%u) to %s",
                 sig->atfiConnEstablishReq.addrInfo.linkHandle,
                 sig->atfiConnEstablishReq.protocolRev,
                 name));
        send(&sig, pid);
        break;

      case ATFI_ADD_CONN_MAP_REQ:
        INFO(STR("Sent ATFI_ADD_CONN_MAP_REQ(lh:%u,la:%u,pa:%u) to %s",
                 sig->atfiAddConnMapReq.addrInfo.linkHandle,
                 sig->atfiAddConnMapReq.logicalAddress,
                 sig->atfiAddConnMapReq.physicalAddress,
                 name));
        send(&sig, pid);
        break;

      case ATFI_ADD_AU3_MAP_REQ:
        INFO(STR("Sent ATFI_ADD_AU3_MAP_REQ(lh:%u,la:%u,at:\"%s\") to %s",
                 sig->atfiAddAu3MapReq.addrInfo.linkHandle,
                 sig->atfiAddAu3MapReq.logicalAddress,
                 sig->atfiAddAu3MapReq.auType,
                 name));
        send(&sig, pid);
        break;

      case ATFI_REMOVE_CONN_MAP_REQ:
        INFO(STR("Sent ATFI_REMOVE_CONN_MAP_REQ(lh:%u,la:%u) to %s",
                 sig->atfiRemoveConnMapReq.addrInfo.linkHandle,
                 sig->atfiRemoveConnMapReq.logicalAddress,
                 name));
        send(&sig, pid);
        break;

      case ATFI_CONNECT2_REQ:
        INFO(STR("Sent ATFI_CONNECT2_REQ(lh:%u,la:%u,tu:%u) to %s",
                 sig->atfiConnect2Req.addrInfo.linkHandle,
                 sig->atfiConnect2Req.logicalAddress,
                 sig->atfiConnect2Req.typeOfUnit,
                 name));
        send(&sig, pid);
        break;

      case ATFI_DISCONNECT_REQ:
        INFO(STR("Sent ATFI_DISCONNECT_REQ(lh:%u,la:%u) to %s",
                 sig->atfiDisconnectReq.addrInfo.linkHandle,
                 sig->atfiDisconnectReq.logicalAddress,
                 name));
        send(&sig, pid);
        break;

      case ATFI_RESET_REQ:
        INFO(STR("Sent ATFI_RESET_REQ(lh:%u) to %s",
                 sig->atfiResetReq.addrInfo.linkHandle,
                 name));
        send(&sig, pid);
        break;

      case ATFI_AUDIT_REQ:
        INFO(STR("Sent ATFI_AUDIT_REQ(lh:%u,la:%u) to %s",
                 sig->atfiAuditReq.addrInfo.linkHandle,
                 sig->atfiAuditReq.logicalAddress,
                 name));
        send(&sig, pid);
        break;

      case ATFI_GET_AU3_PORT_REQ:
        INFO(STR("Sent ATFI_GET_AU3_PORT_REQ(lh:%u,la:%u) to %s",
                 sig->atfiGetAu3PortReq.addrInfo.linkHandle,
                 sig->atfiGetAu3PortReq.logicalAddress,
                 name));
        send(&sig, pid);
        break;

      case ATFI_GET_AISG_UNIQUE_ID_REQ:
        INFO(STR("Sent ATFI_GET_AISG_UNIQUE_ID_REQ(lh:%u,la:%u) to %s",
                 sig->atfiGetAisgUniqueIdReq.addrInfo.linkHandle,
                 sig->atfiGetAisgUniqueIdReq.logicalAddress,
                 name));
        send(&sig, pid);
        break;

      case ATFI_AISG_DEVICE_SCAN_REQ:
        INFO(STR("Sent ATFI_AISG_DEVICE_SCAN_REQ("
		 "lh:%u,br:%u,to:%u,"
		 "ml:%u,m:%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
                 "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x,"
		 "ul:%u,u:%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
                 "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x"
		 ") to %s",
                 sig->atfiAisgDeviceScanReq.addrInfo.linkHandle,
                 sig->atfiAisgDeviceScanReq.baudrate,
		 sig->atfiAisgDeviceScanReq.timeout,
		 sig->atfiAisgDeviceScanReq.uniqueHwIdMaskLength,
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[0],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[1],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[2],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[3],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[4],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[5],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[6],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[7],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[8],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[9],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[10],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[11],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[12],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[13],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[14],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[15],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[16],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[17],
                 sig->atfiAisgDeviceScanReq.uniqueHwIdMask[18],
		 sig->atfiAisgDeviceScanReq.uniqueHwIdLength,
                 sig->atfiAisgDeviceScanReq.uniqueHwId[0],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[1],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[2],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[3],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[4],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[5],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[6],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[7],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[8],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[9],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[10],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[11],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[12],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[13],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[14],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[15],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[16],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[17],
                 sig->atfiAisgDeviceScanReq.uniqueHwId[18],
		 name));
        send(&sig, pid);
        break;

      case ATFI_ADD_AISG_MAP2_REQ:
        INFO(STR("Sent ATFI_ADD_AISG_MAP2_REQ(lh:%u,la:%u,dt:%u,len:%u,id:"
                 "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
                 "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x) to %s",
                 sig->atfiAddAisgMap2Req.addrInfo.linkHandle,
                 sig->atfiAddAisgMap2Req.logicalAddress,
                 sig->atfiAddAisgMap2Req.deviceType,
                 sig->atfiAddAisgMap2Req.uniqueHwIdLength,
                 sig->atfiAddAisgMap2Req.uniqueHwId[0],
                 sig->atfiAddAisgMap2Req.uniqueHwId[1],
                 sig->atfiAddAisgMap2Req.uniqueHwId[2],
                 sig->atfiAddAisgMap2Req.uniqueHwId[3],
                 sig->atfiAddAisgMap2Req.uniqueHwId[4],
                 sig->atfiAddAisgMap2Req.uniqueHwId[5],
                 sig->atfiAddAisgMap2Req.uniqueHwId[6],
                 sig->atfiAddAisgMap2Req.uniqueHwId[7],
                 sig->atfiAddAisgMap2Req.uniqueHwId[8],
                 sig->atfiAddAisgMap2Req.uniqueHwId[9],
                 sig->atfiAddAisgMap2Req.uniqueHwId[10],
                 sig->atfiAddAisgMap2Req.uniqueHwId[11],
                 sig->atfiAddAisgMap2Req.uniqueHwId[12],
                 sig->atfiAddAisgMap2Req.uniqueHwId[13],
                 sig->atfiAddAisgMap2Req.uniqueHwId[14],
                 sig->atfiAddAisgMap2Req.uniqueHwId[15],
                 sig->atfiAddAisgMap2Req.uniqueHwId[16],
                 sig->atfiAddAisgMap2Req.uniqueHwId[17],
                 sig->atfiAddAisgMap2Req.uniqueHwId[18],
                 name));
        send(&sig, pid);
        break;

      case ATFI_ADD_CPRI_MAP2_REQ:
        INFO(STR("Sent ATFI_ADD_CPRI_MAP2_REQ(lh:%u,la:%u,cn:%u,pa:%u) to %s",
                 sig->atfiAddCpriMap2Req.addrInfo.linkHandle,
                 sig->atfiAddCpriMap2Req.logicalAddress,
                 sig->atfiAddCpriMap2Req.cascadeNo,
                 sig->atfiAddCpriMap2Req.physicalAddress,
                 name));
        send(&sig, pid);
        break;

      case ATFI_GET_PHY_ENDPOINT_REQ:
        INFO(STR("Sent ATFI_GET_PHY_ENDPOINT_REQ(lh:%u) to %s",
                 sig->atfiGetPhyEndpointReq.addrInfo.linkHandle,
                 name));
        send(&sig, pid);
        break;

      case ATFI_SETUP_IDCP_A3_REQ:
        INFO(STR("Sent ATFI_SETUP_IDCP_A3_REQ(lh:%u,snidA:%u,snidB:%u,supa:%u) to %s",
                 sig->atfiSetupIdcpA3Req.addrInfo.linkHandle,
                 sig->atfiSetupIdcpA3Req.snidA,
                 sig->atfiSetupIdcpA3Req.snidB,
                 sig->atfiSetupIdcpA3Req.phyEndPointAddrB.address.atm.supa,
                 name));
        send(&sig, pid);
        break;

      case ATFI_SETUP_IDCP_B3_REQ:
        INFO(STR("Sent ATFI_SETUP_IDCP_B3_REQ(lh:%u,snidA:%u,"
                 "supa:%u,snidB:%u,cepIdB:0x%x) to %s",
                 sig->atfiSetupIdcpB3Req.addrInfo.linkHandle,
                 sig->atfiSetupIdcpB3Req.snidA,
                 sig->atfiSetupIdcpB3Req.phyEndPointAddrA.address.atm.supa,
                 sig->atfiSetupIdcpB3Req.snidB,
                 sig->atfiSetupIdcpB3Req.cepIdB,
                 name));
        send(&sig, pid);
        break;

      case ATFI_RELEASE_IDCP_A_REQ:
        INFO(STR("Sent ATFI_RELEASE_IDCP_A_REQ(lh:%u,snidA:%u,snidB:%u,supa:%u) to %s",
                 sig->atfiReleaseIdcpAReq.addrInfo.linkHandle,
                 sig->atfiReleaseIdcpAReq.snidA,
                 sig->atfiReleaseIdcpAReq.snidB,
                 sig->atfiReleaseIdcpAReq.phyEndPointAddrB.address.atm.supa,
                 name));
        send(&sig, pid);
        break;

      case ATFI_RELEASE_IDCP_B_REQ:
        INFO(STR("Sent ATFI_RELEASE_IDCP_B_REQ(lh:%u,snidA:%u,"
                 "supa:%u,snidB:%u) to %s",
                 sig->atfiReleaseIdcpBReq.addrInfo.linkHandle,
                 sig->atfiReleaseIdcpBReq.snidA,
                 sig->atfiReleaseIdcpBReq.phyEndPointAddrA.address.atm.supa,
                 sig->atfiReleaseIdcpBReq.snidB,
                 name));
        send(&sig, pid);
        break;


      /*
      ** SERVER TO CLIENT SIGNALS.
      */

      case ATFI_CONN_ESTABLISH_CFM:
        INFO(STR("Received ATFI_CONN_ESTABLISH_CFM(lh:%u) from %s",
                 sig->atfiConnEstablishCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_CONN_ESTABLISH_REJ:
        INFO(STR("Received ATFI_CONN_ESTABLISH_REJ(lh:%u,ec:%u,pr:%u) from %s",
                 sig->atfiConnEstablishRej.addrInfo.linkHandle,
                 sig->atfiConnEstablishRej.errorCode,
                 sig->atfiConnEstablishRej.highestSupportedProtocolRev,
                 name));
        break;

      case ATFI_ADD_CONN_MAP_CFM:
        INFO(STR("Received ATFI_ADD_CONN_MAP_CFM(lh:%u) from %s",
                 sig->atfiAddConnMapCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_ADD_CONN_MAP_REJ:
        INFO(STR("Received ATFI_ADD_CONN_MAP_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiAddConnMapRej.addrInfo.linkHandle,
                 sig->atfiAddConnMapRej.errorCode,
                 name));
        break;

      case ATFI_ADD_AU3_MAP_CFM:
        INFO(STR("Received ATFI_ADD_AU3_MAP_CFM(lh:%u) from %s",
                 sig->atfiAddAu3MapCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_ADD_AU3_MAP_REJ:
        INFO(STR("Received ATFI_ADD_AU3_MAP_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiAddAu3MapRej.addrInfo.linkHandle,
                 sig->atfiAddAu3MapRej.errorCode,
                 name));
        break;

      case ATFI_REMOVE_CONN_MAP_CFM:
        INFO(STR("Received ATFI_REMOVE_CONN_MAP_CFM(lh:%u) from %s",
                 sig->atfiRemoveConnMapCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_REMOVE_CONN_MAP_REJ:
        INFO(STR("Received ATFI_REMOVE_CONN_MAP_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiRemoveConnMapRej.addrInfo.linkHandle,
                 sig->atfiRemoveConnMapRej.errorCode,
                 name));
        break;

      case ATFI_CONNECT2_CFM:
        INFO(STR("Received ATFI_CONNECT2_CFM(lh:%u,st:%u) from %s",
                 sig->atfiConnect2Cfm.addrInfo.linkHandle,
                 sig->atfiConnect2Cfm.state,
                 name));
        break;

      case ATFI_CONNECT2_REJ:
        INFO(STR("Received ATFI_CONNECT2_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiConnect2Rej.addrInfo.linkHandle,
                 sig->atfiConnect2Rej.errorCode,
                 name));
        break;

      case ATFI_CONNECT2_IND:
        INFO(STR("Received ATFI_CONNECT2_IND(lh:%u,la:%u) from %s",
                 sig->atfiConnect2Req.addrInfo.linkHandle,
                 sig->atfiConnect2Req.logicalAddress,
                 name));
        break;

      case ATFI_DISCONNECT_CFM:
        INFO(STR("Received ATFI_DISCONNECT_CFM(lh:%u) from %s",
                 sig->atfiDisconnectCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_DISCONNECT_REJ:
        INFO(STR("Received ATFI_DISCONNECT_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiDisconnectRej.addrInfo.linkHandle,
                 sig->atfiDisconnectRej.errorCode,
                 name));
        break;

      case ATFI_DISCONNECT_IND:
        INFO(STR("Received ATFI_DISCONNECT_IND(lh:%u,la:%u) from %s",
                 sig->atfiDisconnectInd.addrInfo.linkHandle,
                 sig->atfiDisconnectInd.logicalAddress,
                 name));
        break;

      case ATFI_RESET_CFM:
        INFO(STR("Received ATFI_RESET_CFM(lh:%u) from %s",
                 sig->atfiResetCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_RESET_REJ:
        INFO(STR("Received ATFI_RESET_REJ(lh:%u) from %s",
                 sig->atfiResetRej.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_AUDIT_CFM:
        INFO(STR("Received ATFI_AUDIT_CFM(lh:%u,st:%u) from %s",
                 sig->atfiAuditCfm.addrInfo.linkHandle,
                 sig->atfiAuditCfm.state,
                 name));
        break;

      case ATFI_AUDIT_REJ:
        INFO(STR("Received ATFI_AUDIT_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiAuditRej.addrInfo.linkHandle,
                 sig->atfiAuditRej.errorCode,
                 name));
        break;

      case ATFI_GET_AU3_PORT_CFM:
        INFO(STR("Received ATFI_GET_AU3_PORT_CFM(lh:%u,pi:%u) from %s",
                 sig->atfiGetAu3PortCfm.addrInfo.linkHandle,
                 sig->atfiGetAu3PortCfm.portId,
                 name));
        break;

      case ATFI_GET_AU3_PORT_REJ:
        INFO(STR("Received ATFI_GET_AU3_PORT_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiGetAu3PortRej.addrInfo.linkHandle,
                 sig->atfiGetAu3PortRej.errorCode,
                 name));
        break;

      case ATFI_GET_AISG_UNIQUE_ID_CFM:
        INFO(STR("Received ATFI_GET_AISG_UNIQUE_ID_CFM(lh:%u,len:%u,id:"
                 "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
                 "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x) from %s",
                 sig->atfiGetAisgUniqueIdCfm.addrInfo.linkHandle,
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwIdLength,
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[0],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[1],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[2],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[3],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[4],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[5],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[6],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[7],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[8],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[9],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[10],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[11],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[12],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[13],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[14],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[15],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[16],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[17],
                 sig->atfiGetAisgUniqueIdCfm.uniqueHwId[18],
                 name));
        break;

      case ATFI_GET_AISG_UNIQUE_ID_REJ:
        INFO(STR("Received ATFI_GET_AISG_UNIQUE_ID_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiGetAisgUniqueIdRej.addrInfo.linkHandle,
                 sig->atfiGetAisgUniqueIdRej.errorCode,
                 name));
        break;

      case ATFI_AISG_DEVICE_SCAN_CFM:
        INFO(STR("Received ATFI_AISG_DEVICE_SCAN_CFM(lh:%u,re:%u,dt:%u,"
		 "ul:%u,u:%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
                 "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x) from %s",
                 sig->atfiAisgDeviceScanCfm.addrInfo.linkHandle,
		 sig->atfiAisgDeviceScanCfm.result,
		 sig->atfiAisgDeviceScanCfm.deviceType,
		 sig->atfiAisgDeviceScanCfm.uniqueHwIdLength,
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[0],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[1],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[2],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[3],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[4],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[5],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[6],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[7],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[8],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[9],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[10],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[11],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[12],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[13],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[14],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[15],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[16],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[17],
                 sig->atfiAisgDeviceScanCfm.uniqueHwId[18],
		 name));
        break;

      case ATFI_AISG_DEVICE_SCAN_REJ:
        INFO(STR("Received ATFI_AISG_DEVICE_SCAN_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiAisgDeviceScanRej.addrInfo.linkHandle,
                 sig->atfiAisgDeviceScanRej.errorCode,
		 name));
        break;

      case ATFI_ADD_AISG_MAP_CFM:
        INFO(STR("Received ATFI_ADD_AISG_MAP_CFM(lh:%u) from %s",
                 sig->atfiAddAisgMapCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_ADD_AISG_MAP_REJ:
        INFO(STR("Received ATFI_ADD_AISG_MAP_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiAddAisgMapRej.addrInfo.linkHandle,
                 sig->atfiAddAisgMapRej.errorCode,
                 name));
        break;

      case ATFI_ADD_CPRI_MAP_CFM:
        INFO(STR("Received ATFI_ADD_CPRI_MAP_CFM(lh:%u) from %s",
                 sig->atfiAddCpriMapCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_ADD_CPRI_MAP_REJ:
        INFO(STR("Received ATFI_ADD_CPRI_MAP_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiAddCpriMapRej.addrInfo.linkHandle,
                 sig->atfiAddCpriMapRej.errorCode,
                 name));
        break;

      case ATFI_GET_PHY_ENDPOINT_CFM:
        INFO(STR("Received ATFI_GET_PHY_ENDPOINT_CFM(lh:%u,supa:%u,snid:%u) from %s",
                 sig->atfiGetPhyEndpointCfm.addrInfo.linkHandle,
                 sig->atfiGetPhyEndpointCfm.phyEndPointAddr.address.atm.supa,
                 sig->atfiGetPhyEndpointCfm.snid,
                 name));
        break;

      case ATFI_GET_PHY_ENDPOINT_REJ:
        INFO(STR("Received ATFI_GET_PHY_ENDPOINT_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiGetPhyEndpointRej.addrInfo.linkHandle,
                 sig->atfiGetPhyEndpointRej.errorCode,
                 name));
        break;

      case ATFI_SETUP_IDCP_A3_CFM:
        INFO(STR("Received ATFI_SETUP_IDCP_A3_CFM(lh:%u,cepIdB:0x%x) from %s",
                 sig->atfiSetupIdcpA3Cfm.addrInfo.linkHandle,
                 sig->atfiSetupIdcpA3Cfm.cepIdB,
                 name));
        break;

      case ATFI_SETUP_IDCP_A3_REJ:
        INFO(STR("Received ATFI_SETUP_IDCP_A3_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiSetupIdcpA3Rej.addrInfo.linkHandle,
                 sig->atfiSetupIdcpA3Rej.errorCode,
                 name));
        break;

      case ATFI_SETUP_IDCP_B3_CFM:
        INFO(STR("Received ATFI_SETUP_IDCP_B3_CFM(lh:%u) from %s",
                 sig->atfiSetupIdcpB3Cfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_SETUP_IDCP_B3_REJ:
        INFO(STR("Received ATFI_SETUP_IDCP_B3_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiSetupIdcpB3Rej.addrInfo.linkHandle,
                 sig->atfiSetupIdcpB3Rej.errorCode,
                 name));
        break;

      case ATFI_RELEASE_IDCP_A_CFM:
        INFO(STR("Received ATFI_RELEASE_IDCP_A_CFM(lh:%u) from %s",
                 sig->atfiReleaseIdcpACfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_RELEASE_IDCP_A_REJ:
        INFO(STR("Received ATFI_RELEASE_IDCP_A_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiReleaseIdcpARej.addrInfo.linkHandle,
                 sig->atfiReleaseIdcpARej.errorCode,
                 name));
        break;

      case ATFI_RELEASE_IDCP_B_CFM:
        INFO(STR("Received ATFI_RELEASE_IDCP_B_CFM(lh:%u) from %s",
                 sig->atfiReleaseIdcpBCfm.addrInfo.linkHandle,
                 name));
        break;

      case ATFI_RELEASE_IDCP_B_REJ:
        INFO(STR("Received ATFI_RELEASE_IDCP_B_REJ(lh:%u,ec:%u) from %s",
                 sig->atfiReleaseIdcpBRej.addrInfo.linkHandle,
                 sig->atfiReleaseIdcpBRej.errorCode,
                 name));
        break;

      default:
        TRACE_ERROR(STR("Received unexpected signal 0x%08x from %s",
                        sig->sigNo, name));
        break;
    }

    if (sig) {
      free_buf(&sig);
    }

    free_buf((union SIGNAL**)(void*) &pcb);

  }
}

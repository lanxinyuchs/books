/* ----------------------------------------------------------------------
 * %CCaseFile:	namespace_simulator.c %
 * %CCaseRev:	/main/R3A/R9A/R10A/1 %
 * %CCaseDate:	2017-05-11 %
 * %CCaseDocNo: %
 * Author:      etxlg
 * Author: <name>, <e-mail address>
 *
 * This terminates the CTN protocol (see IWD in the OOT block).
 * The normal user of this protocol is TN, code here enables running the
 * system without TN.
 * Lookup of OaM namespace configured for the IP in OaMaccesspoint is always
 * done at start. I.e. all OaM services wait for TN to connect and then the
 * lookup is performed, only exception is if the OaMaccesspoint is empty.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and disseminations to the receivers employees shall
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R3A/1      2014-11-17 etxlg       Created from demo code in IWD
 * R3A/2      2014-11-24 etxlg       Development support for NS.
 *				     parked for now...
 * R10A/1     2017-05-11 etxpeno     Using CsTnInitialize2()
 *                                   Needs a lot of cleanup
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <syslog.h>
#include <errno.h>
#include <search.h>

#include "cstnn.h"

#include <saAis.h>
#include <saImm.h>
#include <saImmOm.h>
#include <saImmOi.h>

/*
  #define NS_CTRL_FILE "ns_control_file.sh"
  #define NS_FILE "ns_name.txt"
*/
#define NS_CTRL_FILE "/home/sirpa/dev_patches/ns_control_file.sh"
#define NS_FILE "/tmp/ns_name.txt"

struct data {
  SaStringT immDn;
  SaStringT ecimDn;
  SaStringT address;
  int subscriber;
};

struct ccb {
  SaStringT immDn;
  SaStringT ecimDn;
  SaStringT address;
  int action;
};

static SaAisErrorT recover_imm();
static SaAisErrorT setup_oi(SaImmOiHandleT *handle,
			    SaSelectionObjectT *selectionObject);

static SaAisErrorT create_AddressIP(SaImmOiHandleT immOiHandle,
				    SaImmOiCcbIdT ccbId,
				    const SaImmClassNameT className,
				    const SaNameT *parentName,
				    const SaImmAttrValuesT_2 **attr);
static SaAisErrorT delete_AddressIP(SaImmOiHandleT immOiHandle,
				    SaImmOiCcbIdT ccbId,
				    const SaNameT *objectName);
static SaAisErrorT modify_AddressIP(SaImmOiHandleT immOiHandle,
				    SaImmOiCcbIdT ccbId,
				    const SaNameT *objectName,
				    const SaImmAttrModificationT_2 **attrMods);
static SaAisErrorT completed_AddressIP(SaImmOiHandleT immOiHandle,
				       SaImmOiCcbIdT ccbId);
static void apply_AddressIP(SaImmOiHandleT immOiHandle,
			    SaImmOiCcbIdT ccbId);
static void abort_AddressIP(SaImmOiHandleT immOiHandle,
			    SaImmOiCcbIdT ccbId);

static SaStringT fetch_value(const SaStringT attrName,
			     const SaImmAttrValuesT_2 **attributes);

static void insert(SaNameT *object_name, const SaImmAttrValuesT_2 **attributes);

static void handle_apply(const void *, const VISIT, const int);
static void create_struct_data(const SaStringT, const SaStringT,
			       const SaStringT);

static int cmp_ecimDn_struct_data(const void *pa, const void *pb);
static int cmp_immDn_struct_ccb(const void *pa, const void *pb);
static int cmp_immDn_struct_data(const void *pa, const void *pb);
static void print_struct_data(const void *nodep, const VISIT which,
			      const int depth);
static void print_struct_ccb(const void *nodep, const VISIT which,
			     const int depth);

static void free_struct_ccb(void *);
static void free_struct_data(struct data *);

#define NO_SUBSCRIBER 0
#define SUBSCRIBER 1

#define CREATE 0
#define DELETE 1
#define MODIFY 2

#define NO_CCB 0

static void *dataRoot = NULL;
static void *ccbRoot = NULL;
static SaImmOiCcbIdT ongoing_ccbId = NO_CCB;

char*
make_fpath(char *name)
{
  /*
    not set by appm - hardcode it!
    char *dir = getenv("DEV_PATCHES");
    if(dir == NULL) {
    syslog(LOG_DEBUG, "DEV_PATCHES not set");
    return NULL;
    }
  */
  char *dir = "/tmp";
  char *path;


  path = malloc(strlen(dir) + strlen(name) + 1 + 1);
  if(!path) {
    syslog(LOG_DEBUG, "malloc: %m");
    return NULL;
  }
  strcpy(path, dir);
  strcat(path, "/");
  strcat(path, name);
  syslog(LOG_DEBUG, "make_fpath() -> %s", path);
  return path;
}

void
init_development_ns(void)
{
  struct stat st;
  int res;

  syslog(LOG_DEBUG, "init_development_ns()");

  syslog(LOG_DEBUG, "Checking for file: %s", NS_CTRL_FILE);
  if(stat(NS_CTRL_FILE, &st) == 0){
    syslog(LOG_DEBUG, "Found controlfile - executing");
    res = system(NS_CTRL_FILE);
    syslog(LOG_DEBUG, "Command returned: %d", WEXITSTATUS(res));
  }
  return;
}

CsTnResult
dn_to_tn2(const char *dn, CsTnInfoT *info)
{
  printf("dn_to_tn2: dn = %s\n", dn);

  struct data ptr;
  ptr.ecimDn = (const SaStringT) dn;
  void *val;
  val = tfind((void *)&ptr, &dataRoot, cmp_ecimDn_struct_data);

  if (val == NULL)
    goto EXIT;

  struct data *found = *(struct data **)val;

  if (found->address)
    strcpy(info->IpAddress, found->address);

  if (found->subscriber == NO_SUBSCRIBER) {
    tdelete((void *)&ptr, &dataRoot, cmp_ecimDn_struct_data);

    found->subscriber = SUBSCRIBER;
    val = tsearch((void *)found, &dataRoot, cmp_ecimDn_struct_data);
    if (val == NULL)
      exit(EXIT_FAILURE);
  }

 EXIT:
  twalk(dataRoot, print_struct_data);
  printf("dn_to_tn2: dn = %s -> IpAddress = %s\n", dn, info->IpAddress);

  return CSTN_OK;
}

/* example function using the callback API
 * parameters to this function are used both for input and output */
void
dn_to_tn(int *length, char *dn)
{
  /* this function is slightly borked but hey it's not for real */
  int fd;
  int ret;
  int inlen = *length;

  /* The DN received is NULL terminated (NULL included in *length), thus
   * it is alright to print it out as a string */
  syslog(LOG_DEBUG, "Request for NS of [%d]: %s", *length, dn);

  *length = 1; memcpy(dn, "", 1); /*start by setting up for empty ns*/
  fd = open(NS_FILE, O_RDONLY);
  if(fd != -1){
    ret = read(fd, dn, inlen);
    close(fd);
    syslog(LOG_DEBUG, "namespace length from file: %d", ret);
    if(ret != -1){
      *length = ret;
      *(dn + ret) = '\0';  /* for printing */
      syslog(LOG_DEBUG, "namespace from file: %s", dn);
    } else {
      syslog(LOG_DEBUG, "failed to read file: %s", NS_FILE);
    }
  } else {
    syslog(LOG_DEBUG, "failed to open file: %s", NS_FILE);
  }
  return;


  /* Check that the NS to be returned is NOT to long to fit in the buffer,
   * this is realy unlikely but if it ever happens this breaks */
  /*
   *length = sizeof ns <= (unsigned)*length ? sizeof ns : (unsigned)*length;
   */

  /* a NULL terminated return string is NOT required
   * it works in either case (i.e. with or without '\0'),
   * this example will return the string with NULL termination,
   *** memcpy(dn, ns, *length);
   * There are two special cases
   * 1. Signalling that namespacing is NOT in use:
   *  *length = 1, memcpy(dn, "", 1), i.e. return a single NULL
   * 2. Error return, the code was unable to return anything sensible
   *  *length = 0, dn - don't care*/

}

int
main(void)
{
  openlog("namespace_simulator", LOG_PID, LOG_USER);

  init_development_ns();

  SaAisErrorT rc = SA_AIS_OK;
  SaImmOiHandleT handle_oi;
  SaSelectionObjectT selectionObject;

  printf("Setting up IMM object implementer\n");
  rc = setup_oi(&handle_oi, &selectionObject);
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Failed to setup IMM object implementer");
    return rc;
  }

  printf("Recover data from IMM\n");
  rc = recover_imm();
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Failed to recover from IMM");
    return rc;
  }

  while(1){ /* runs in a loop to do retry */
    syslog(LOG_DEBUG, "Calling CsTnInitialize2()");
    printf("Calling CsTnInitialize2()\n");
    CsTnCallbacks2T cb;
    cb.OamDNtoNsNameAndIp = dn_to_tn2;

    CsTnHandleT handle;
    handle = CsTnInitialize2(&cb);
    if(handle == -1) {
      syslog(LOG_DEBUG, "Failed to initialize - retrying");
      break;
    }

    struct pollfd pfd[2];
    pfd[0].fd = handle;
    pfd[0].events = POLLIN;
    pfd[1].fd = selectionObject;
    pfd[1].events = POLLIN;
    while(1){
      int ret;
      syslog(LOG_DEBUG, "Doing poll");
      ret = poll(pfd, 2, -1);
      if(ret == -1) {
	syslog(LOG_DEBUG, "poll: %m");
	break;
      }

      if (pfd[0].revents != 0) {
	syslog(LOG_DEBUG, "Calling CsTnDispatch()");
	ret = CsTnDispatch();
	if(ret == -1) {
	  syslog(LOG_DEBUG, "Failure in dispatch");
	  break;
	}
      }

      if (pfd[1].revents != 0) {
	SaDispatchFlagsT dispatchFlag = SA_DISPATCH_ONE;
	syslog(LOG_DEBUG, "Calling saImmOiDispatch()");
	rc = saImmOiDispatch(handle_oi, dispatchFlag);

	if (rc != SA_AIS_OK) {
	  syslog(LOG_DEBUG, "Failure in dispatch");
	  break;
	}
      }
    } /* end of inner while */
    /* something failed, wait a bit, then try again */
    sleep(1);
  } /* end of outer while */
} /* end of main */

static SaAisErrorT
recover_imm()
{
  SaAisErrorT rc = SA_AIS_OK;
  SaImmHandleT handle;
  SaImmCallbacksT *callbacks = NULL;
  SaVersionT version = {'A', 0x02, 0x0b};

  rc = saImmOmInitialize(&handle, callbacks, &version);
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't initialize Imm OM");
    return rc;
  }

  SaImmSearchHandleT searchHandle;
  SaStringT classNames[3];

  classNames[0] = "AddressIPv4";
  classNames[1] = "AddressIPv6";
  classNames[2] = NULL;

  SaImmAttrNameT attributeNames[3];
  attributeNames[0] = "address";
  attributeNames[1] = "RcsImmAttrEcimDn";
  attributeNames[2] = NULL;
  rc = saImmOmSearchClassInitialize_s2(handle, NULL, SA_IMM_SUBTREE,
				       SA_IMM_SEARCH_GET_SOME_ATTR,
				       classNames, attributeNames,
				       &searchHandle);

  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't initialize Imm OM Search");
    return rc;
  }

  while(1) {
    SaImmSearchObjectsT_s2 **objects;
    rc = saImmOmSearchNextN_s2(searchHandle, 5, &objects);
    if (rc == SA_AIS_ERR_NOT_EXIST)
      break;

    if (rc != SA_AIS_OK) {
      syslog(LOG_DEBUG, "Couldn't get next search result");
      return rc;
    }

    for (int i=0; objects[i]; i++)
      insert(&objects[i]->objectName,
	     (const SaImmAttrValuesT_2 **)objects[i]->attributes);
  }

  /* Cleanup */
  rc = saImmOmFinalize(handle);
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't finalize Imm OM");
    return rc;
  }

  twalk(dataRoot, print_struct_data);

  return rc;
}

static void
insert(SaNameT *object_name, const SaImmAttrValuesT_2 **attributes)
{
  SaStringT ecimDn, address;

  char name[SA_MAX_NAME_LENGTH+1];
  strncpy(name, (char*) object_name->value, object_name->length);
  name[object_name->length] = '\0';

  ecimDn = fetch_value("RcsImmAttrEcimDn", attributes);
  address = fetch_value("address", attributes);

  if (ecimDn == NULL)
    return;

  create_struct_data(ecimDn, name, address);
}

static SaAisErrorT
setup_oi(SaImmOiHandleT *handle,
	 SaSelectionObjectT *selectionObject)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaImmOiCallbacksT_2 callbacks =
    {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
  SaVersionT version = {'A', 0x02, 0x0b};

  callbacks.saImmOiCcbObjectCreateCallback = &create_AddressIP;
  callbacks.saImmOiCcbObjectDeleteCallback = &delete_AddressIP;
  callbacks.saImmOiCcbObjectModifyCallback = &modify_AddressIP;
  callbacks.saImmOiCcbCompletedCallback = &completed_AddressIP;
  callbacks.saImmOiCcbApplyCallback = &apply_AddressIP;
  callbacks.saImmOiCcbAbortCallback = &abort_AddressIP;

  rc = saImmOiInitialize_2(handle, &callbacks, &version);
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't initialize Imm OI");
    return rc;
  }

  rc = saImmOiSelectionObjectGet(*handle, selectionObject);
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't get Selection Object");
    return rc;
  }

  rc = saImmOiImplementerSet(*handle, "namespace_simulator");
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't set ImplementerName");
    return rc;
  }

  rc = saImmOiClassImplementerSet(*handle, "AddressIPv4");
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't set ClassImplementerName");
    return rc;
  }

  rc = saImmOiClassImplementerSet(*handle, "AddressIPv6");
  if (rc != SA_AIS_OK) {
    syslog(LOG_DEBUG, "Couldn't set ClassImplementerName");
    return rc;
  }

  return rc;
}

static SaAisErrorT
create_AddressIP(SaImmOiHandleT immOiHandle,
		 SaImmOiCcbIdT ccbId,
		 const SaImmClassNameT className,
		 const SaNameT *parentName,
		 const SaImmAttrValuesT_2 **attr)
{
  (void) immOiHandle;
  SaAisErrorT rc = SA_AIS_OK;

  if (ongoing_ccbId != NO_CCB && ccbId != ongoing_ccbId)
    return SA_AIS_ERR_BAD_OPERATION;

  ongoing_ccbId = ccbId;

  SaStringT ecimDn, address, idx;
  ecimDn = fetch_value("RcsImmAttrEcimDn", attr);
  address = fetch_value("address", attr);

  SaStringT immDn = NULL;
  SaStringT rdn = NULL;
  char* ptr;
  size_t immDnlen, rdnlen;

  if (strcmp("AddressIPv4", className) == 0) {
    rdn = "addressIPv4Id";
    rdnlen = strlen(rdn);
  } else if (strcmp("AddressIPv6", className) == 0) {
    rdn = "addressIPv6Id";
    rdnlen = strlen(rdn);
  }

  idx = fetch_value(rdn, attr);
  if (idx) {
    size_t idxlen = strlen(idx);
    immDnlen = rdnlen+1+idxlen+1+parentName->length+1;
    ptr = immDn = malloc(immDnlen);
    memcpy(ptr, rdn, rdnlen);
    ptr += rdnlen;
    *ptr++ = '=';
    memcpy(ptr, idx, idxlen);
    ptr += idxlen;
    *ptr++ = ',';
    memcpy(ptr, parentName->value, parentName->length);
    ptr += parentName->length;
    *ptr = '\0';
  };


  printf("Executing Object Create callback\n");
  printf("ccbId: %d\n", (int) ccbId);
  printf("immDn: %s\n", immDn);
  printf("RcsImmAttrEcimDn: %s\n", ecimDn);
  printf("address: %s\n", address);

  struct ccb *ccbptr;
  ccbptr = malloc(sizeof *ccbptr);

  ccbptr->action = CREATE;
  ccbptr->immDn = immDn;
  ccbptr->ecimDn = strdup(ecimDn);
  ccbptr->address = address?strdup(address):NULL;

  void *val;
  val = tsearch((void *)ccbptr, &ccbRoot, cmp_immDn_struct_ccb);

  struct ccb *found;
  found = *(struct ccb **)val;

  if (found == NULL)
    exit(EXIT_FAILURE);

  if (found != ccbptr) {
    found->action = ccbptr->action;

    free(found->immDn);
    found->immDn = ccbptr->immDn;

    free(found->ecimDn);
    found->ecimDn = ccbptr->ecimDn;

    free(found->address);
    found->address = ccbptr->address;

    free(ccbptr);
  }

  twalk(ccbRoot, print_struct_ccb);

  return rc;
}

static SaAisErrorT
delete_AddressIP(SaImmOiHandleT immOiHandle,
		 SaImmOiCcbIdT ccbId,
		 const SaNameT *objectName)
{
  (void) immOiHandle;
  SaAisErrorT rc = SA_AIS_OK;

  if (ongoing_ccbId != NO_CCB && ccbId != ongoing_ccbId)
    return SA_AIS_ERR_BAD_OPERATION;

  ongoing_ccbId = ccbId;

  printf("Executing Object Delete callback\n");
  printf("ccbId: %d\n", (int) ccbId);
  printf("immDn: %s\n", (char*) objectName->value);

  struct ccb *ccbptr;
  ccbptr = malloc(sizeof *ccbptr);

  ccbptr->action = DELETE;
  ccbptr->immDn = strdup((char*) objectName->value);
  ccbptr->ecimDn = NULL;
  ccbptr->address = NULL;

  void *val;
  val = tsearch((void *)ccbptr, &ccbRoot, cmp_immDn_struct_ccb);

  struct ccb *found;
  found = *(struct ccb **)val;

  if (found == NULL)
    exit(EXIT_FAILURE);
  if (found != ccbptr) {
    /*
     * Delete of an object which is created in this CCB
     */
    found->action = ccbptr->action;

    free(found->ecimDn);
    found->ecimDn = ccbptr->ecimDn;

    free(found->address);
    found->address = ccbptr->address;

    free(ccbptr->immDn);
    free(ccbptr);
  } else {
    /*
     * Delete of an object which is not created in this CCB
     *
     * Try to find the ecim dn
     */
    struct data ptr;
    ptr.immDn = (const SaStringT) objectName->value;

    val = tfind((void *)&ptr, &dataRoot, cmp_immDn_struct_data);
    if (val)
      found->ecimDn = strdup((*(struct data **)val)->ecimDn);
  }

  twalk(ccbRoot, print_struct_ccb);

  return rc;
}

static SaAisErrorT
modify_AddressIP(SaImmOiHandleT immOiHandle,
		 SaImmOiCcbIdT ccbId,
		 const SaNameT *objectName,
		 const SaImmAttrModificationT_2 **attrMods)
{
  (void) immOiHandle;
  SaAisErrorT rc = SA_AIS_OK;

  if (ongoing_ccbId != NO_CCB && ccbId != ongoing_ccbId)
    return SA_AIS_ERR_BAD_OPERATION;

  ongoing_ccbId = ccbId;

  SaStringT address = NULL;
  int address_found = 0;

  for(int i=0; attrMods[i]; i++) {
    if (strcmp("address", attrMods[i]->modAttr.attrName) == 0 &&
	attrMods[i]->modAttr.attrValueType == SA_IMM_ATTR_SASTRINGT &&
	attrMods[i]->modAttr.attrValuesNumber == 1) {
      address = *(SaStringT *)attrMods[i]->modAttr.attrValues[0];
      address_found = 1;
      break;
    }
  }

  printf("Executing Object Modify callback\n");
  printf("ccbId: %d\n", (int) ccbId);
  printf("immDn: %s\n", (char*) objectName->value);
  if (!address_found)
    goto EXIT;
  printf("address: %s\n\n", address);

  struct ccb *ccbptr;
  ccbptr = malloc(sizeof *ccbptr);

  ccbptr->action = MODIFY;
  ccbptr->immDn = strdup((char*) objectName->value);
  ccbptr->ecimDn = NULL;
  ccbptr->address = address?strdup(address):NULL;

  void *val;
  val = tsearch((void *)ccbptr, &ccbRoot, cmp_immDn_struct_ccb);

  struct ccb *found;
  found = *(struct ccb **)val;

  if (found == NULL)
    exit(EXIT_FAILURE);

  if (found != ccbptr) {
    /*
     * Modify of an object which is created in this CCB
     */
    free(found->address);
    found->address = ccbptr->address;

    free(ccbptr->immDn);
    free(ccbptr);
  } else {
    /*
     * Modify of an object which is not created in this CCB
     *
     * Try to find the ecim dn
     */
    struct data ptr;
    ptr.immDn = (const SaStringT) objectName->value;

    val = tfind((void *)&ptr, &dataRoot, cmp_immDn_struct_data);
    if (val)
      found->ecimDn = strdup((*(struct data **)val)->ecimDn);
  }

 EXIT:
  twalk(ccbRoot, print_struct_ccb);

  return rc;
}
static SaAisErrorT
completed_AddressIP(SaImmOiHandleT immOiHandle, SaImmOiCcbIdT ccbId)
{
  (void) immOiHandle;
  (void) ccbId;
  SaAisErrorT rc = SA_AIS_OK;

  if (ongoing_ccbId != NO_CCB && ccbId != ongoing_ccbId)
    return SA_AIS_ERR_BAD_OPERATION;

  return rc;
}

static void
apply_AddressIP(SaImmOiHandleT immOiHandle, SaImmOiCcbIdT ccbId)
{
  (void) immOiHandle;

  printf("Executing CCB Apply callback\n");
  printf("ccbId: %d\n", (int) ccbId);

  ongoing_ccbId = NO_CCB;

  twalk(ccbRoot, print_struct_ccb);
  twalk(ccbRoot, handle_apply);

  tdestroy(ccbRoot, free_struct_ccb);
  ccbRoot = NULL;
  twalk(dataRoot, print_struct_data);
}

static void
abort_AddressIP(SaImmOiHandleT immOiHandle, SaImmOiCcbIdT ccbId)
{
  (void) immOiHandle;

  printf("Executing CCB Abort callback\n");
  printf("ccbId: %d\n", (int) ccbId);

  ongoing_ccbId = NO_CCB;

  tdestroy(ccbRoot, free_struct_ccb);
  ccbRoot = NULL;
}

static SaStringT
fetch_value(const SaStringT attrName, const SaImmAttrValuesT_2 **attributes)
{
  if (attrName == NULL)
    return NULL;

  for(int i=0; attributes[i]; i++) {
    if (strcmp(attrName, attributes[i]->attrName) == 0 &&
	attributes[i]->attrValueType == SA_IMM_ATTR_SASTRINGT &&
	attributes[i]->attrValuesNumber == 1)
      return *(SaStringT *)attributes[i]->attrValues[0];
  }
  return NULL;
}

static void
handle_apply(const void *nodep, const VISIT which, const int depth)
{
  (void)depth;
  struct ccb *ptr = *(struct ccb **) nodep;

  switch (which) {
  case postorder:
  case leaf:

    switch (ptr->action) {
    case CREATE:
      printf("Create\n"
	     "immDn: %s\n"
	     "ecimDn: %s\n"
	     "address: %s\n\n", ptr->immDn, ptr->ecimDn, ptr->address);
      create_struct_data(ptr->ecimDn, ptr->immDn, ptr->address);
      break;
    case DELETE:
      {
	printf("Delete\n"
	       "ecimDn: %s\n\n", ptr->ecimDn);
	struct data search;
	search.ecimDn = ptr->ecimDn;
	void *val;
	val = tfind((void *)&search, &dataRoot, cmp_ecimDn_struct_data);
	if (val == NULL)
	  break;

	struct data *found = *(struct data **)val;

	if (found->subscriber == SUBSCRIBER) {
	  CsTnInfoT cstnInfo;
	  memset(cstnInfo.NsName, 0, CSTN_NSNAME_STRLEN);
	  memset(cstnInfo.IpAddress, 0, INET6_ADDRSTRLEN);
	  printf("CsTnUpdate(%s, {%s, %s})\n\n", ptr->ecimDn,
		 cstnInfo.NsName, cstnInfo.IpAddress);
	  CsTnUpdate(ptr->ecimDn, &cstnInfo);
	}
	tdelete((void *)&search, &dataRoot, cmp_ecimDn_struct_data);
	free_struct_data(found);
	break;
      }
    case MODIFY:
      {
	printf("Modify\n"
	       "ecimDn: %s\n"
	       "address: %s\n\n", ptr->ecimDn, ptr->address);
	struct data search;
	search.ecimDn = ptr->ecimDn;
	void *val;
	val = tfind((void *)&search, &dataRoot, cmp_ecimDn_struct_data);
	if (val == NULL)
	  break;

	struct data *found = *(struct data **)val;

	printf("Found\n"
	       "immDn: %s\n"
	       "ecimDn: %s\n"
	       "address: %s\n"
	       "subscriber: %u\n\n", found->immDn, found->ecimDn,
	       found->address, found->subscriber);
 	if (ptr->address == NULL ||
	    found->address == NULL ||
	    strcmp(ptr->address, found->address)) {
	  if (found->subscriber == SUBSCRIBER) {
	    CsTnInfoT cstnInfo;
	    memset(cstnInfo.NsName, 0, CSTN_NSNAME_STRLEN);
	    if(ptr->address == NULL)
	      memset(cstnInfo.IpAddress, 0, INET6_ADDRSTRLEN);
	    else
	      strcpy(cstnInfo.IpAddress, ptr->address);
	    printf("CsTnUpdate(%s, {%s, %s})\n\n", ptr->ecimDn,
		   cstnInfo.NsName, cstnInfo.IpAddress);
	    CsTnUpdate(ptr->ecimDn, &cstnInfo);
	  }
	  free(found->address);
	  found->address = ptr->address?strdup(ptr->address):NULL;
	}

	break;
      }
    }
  case preorder:
  case endorder:
    break;
  }
}

static void
create_struct_data(const SaStringT ecimDn, const SaStringT immDn,
		   const SaStringT address)
{
  struct data *ptr;

  ptr = malloc(sizeof *ptr);
  ptr->ecimDn = strdup(ecimDn);
  ptr->immDn = strdup(immDn);
  ptr->address = address?strdup(address):NULL;
  ptr->subscriber = NO_SUBSCRIBER;

  void *val;
  val = tsearch((void *) ptr, &dataRoot, cmp_ecimDn_struct_data);

  if (val == NULL)
    exit(EXIT_FAILURE);

  struct data *found;
  found = *(struct data **)val;

  if (found != ptr) {
    found->subscriber = ptr->subscriber;

    free(found->immDn);
    found->immDn = ptr->immDn;

    free(found->ecimDn);
    found->ecimDn = ptr->ecimDn;

    free(found->address);
    found->address = ptr->address;

    free(ptr);
  }

}

static int
cmp_ecimDn_struct_data(const void *pa, const void *pb)
{
  return strcmp(((struct data*)pa)->ecimDn, ((struct data*)pb)->ecimDn);
}

static int
cmp_immDn_struct_ccb(const void *pa, const void *pb)
{
  return strcmp(((struct ccb*)pa)->immDn, ((struct ccb*)pb)->immDn);
}

static int
cmp_immDn_struct_data(const void *pa, const void *pb)
{
  return strcmp(((struct data*)pa)->immDn, ((struct data*)pb)->immDn);
}

static void
print_struct_data(const void *nodep, const VISIT which, const int depth)
{
  (void)depth;
  struct data *ptr;

  switch (which) {
  case preorder:
  case endorder:
    break;
  case postorder:
  case leaf:
    ptr = *(struct data **) nodep;
    printf("ecimDn: %s\n", ptr->ecimDn);
    printf("immDn: %s\n", ptr->immDn);
    printf("address: %s\n", ptr->address);
    printf("subscriber: %d\n\n", ptr->subscriber);
    break;
  }
}

static void
print_struct_ccb(const void *nodep, const VISIT which, const int depth)
{
  (void)depth;
  struct ccb *ptr;

  switch (which) {
  case postorder:
  case leaf:
    ptr = *(struct ccb **) nodep;
    printf("action: %u\n"
	   "immDn: %s\n"
	   "ecimDn: %s\n"
	   "address: %s\n\n", ptr->action, ptr->immDn, ptr->ecimDn,
	   ptr->address);
    break;

  case preorder:
  case endorder:
    break;
  }
}

static void
free_struct_ccb(void *nodep)
{
  struct ccb *ptr = nodep;

  if (ptr == NULL)
    return;

  free(ptr->immDn);
  free(ptr->ecimDn);
  free(ptr->address);
  free(ptr);
}

static void
free_struct_data(struct data *ptr)
{
  if (ptr == NULL)
    return;

  free(ptr->immDn);
  free(ptr->ecimDn);
  free(ptr->address);
  free(ptr);
}

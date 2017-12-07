/* ----------------------------------------------------------------------
 * %CCaseFile:	tmmi.c %
 * %CCaseRev:	/main/R4A/R5A/3 %
 * %CCaseDate:	2016-03-04 %
 * %CCaseDocNo: %
 * Author:      etxpeno
 *
 * Short description: Implementation of the tmmi interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
 * R4A/1      2015-04-23 etxpeno     Created
 * R4A/5      2015-08-26 etxarnu     Write file before trying to set up cec
 * R4A/8      2015-09-17 etxarnu     Added CS_mpIdMismatch
 * R5A/1      2016-02-01 etxarnu     TR HU54831, don't send to MW if not started
 * R5A/2      2016-02-02 etxarnu     TR HU54831, working solution
 * R5A/3      2016-03-04 etxarnu     Check if CEC if started before calling cec_open
 * ----------------------------------------------------------------------
 */

#include <stdio.h>
#include <string.h>
#include <cec.h>
#include <unistd.h>

#include "tmmi.h"

/*
******************************************************************************
* MACROS
******************************************************************************
*/

#define TMMI_SET 0
#define CEC_TIMEOUT_MILLIS 43000

static char signature[] = {'T', 'M', 'M', 'I'};

static size_t buffer_size(uint8_t numBoards,
			  struct CS_BoardMapEntry* presentBoards);
static int write2file(FILE *stream, uint8_t numBoards,
		      struct CS_BoardMapEntry* presentBoards);

void
CS_setAvailBoardMap(uint8_t numBoards, struct CS_BoardMapEntry* presentBoards)
{
  cec_handle_t *handle;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;
  int send_length;
  char *ptr;
  uint8_t i, j;
  FILE *stream;

  stream = fopen("/tmp/presentBoards.txt.2", "w");
  if (stream == NULL)
    return;

  write2file(stream, numBoards, presentBoards);
  fclose(stream);

  /* If CEC is not up, no point in sending to erlang. */
  if ( access("/tmp/cec_ready",F_OK) == -1 ) 
    {
      system(" logger 'tmmi.c: CEC not ready, no message to sysTmmiServer' ");
      return;
    }


  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL)
    return;

  /* Build the send signal */
  send_length = buffer_size(numBoards, presentBoards);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = TMMI_SET;
  ptr += sizeof(uint32_t);

  for (i = 0; i < numBoards; i++) {
    *(uint8_t*)ptr = presentBoards[i].mpId;
    ptr += sizeof(uint8_t);
    *(TmmiBoardAvailState*)ptr = presentBoards[i].availState;
    ptr += sizeof(TmmiBoardAvailState);
    *(uint8_t*)ptr = presentBoards[i].numMacEntries;
    ptr += sizeof(uint8_t);

    for (j = 0; j < presentBoards[i].numMacEntries; j++) {
      *(TmmiTrafficType*)ptr = presentBoards[i].macEntries[j].trafficType;
      ptr += sizeof(TmmiTrafficType);
      memcpy(ptr,
	     presentBoards[i].macEntries[j].interfaceName,
	     TMMI_IF_NAME_LEN);
      ptr += TMMI_IF_NAME_LEN;
      *(uint16_t*)ptr = presentBoards[i].macEntries[j].vlan;
      ptr += sizeof(uint16_t);
      *(uint8_t*)ptr = presentBoards[i].macEntries[j].prio;
      ptr += sizeof(uint8_t);
      memcpy(ptr,
	     presentBoards[i].macEntries[j].metaMacAddress,
	     TMMI_MAC_LEN*sizeof(uint8_t));
      ptr += TMMI_MAC_LEN*sizeof(uint8_t);
    }
  }

  /* Send the request and wait for the reply */
  if (cec_send_with_pid(handle, &send_packet) == -1)
    goto ERROR;

  if (cec_receive_w_tmeout(handle, &recv_packet, CEC_TIMEOUT_MILLIS) == -1)
    goto ERROR;

  free(recv_packet.data);

 ERROR:
  free(send_packet.data);
  cec_close(handle);
}

void
CS_mpIdMismatch()
{
  FILE *stream;

  stream = fopen("/tmp/presentBoards.txt.2", "w");
  if (stream == NULL)
    return;
  fprintf(stream, "MPID_MISMATCH\n");
  fclose(stream);

}

static size_t
buffer_size(uint8_t numBoards, struct CS_BoardMapEntry* presentBoards)
{
  size_t size = 0;
  uint8_t i, j;

  size += sizeof(uint32_t);
  for (i = 0; i < numBoards; i++) {
    size += sizeof(uint8_t);
    size += sizeof(TmmiBoardAvailState);
    size += sizeof(uint8_t);

    for (j = 0; j < presentBoards[i].numMacEntries; j++) {
      size += sizeof(TmmiTrafficType);
      size += TMMI_IF_NAME_LEN;
      size += sizeof(uint16_t);
      size += sizeof(uint8_t);
      size += TMMI_MAC_LEN * sizeof(uint8_t);
    }
  }

  return size;
}

static int
write2file(FILE *stream, uint8_t numBoards,
	   struct CS_BoardMapEntry* presentBoards)
{
  int size = 0;
  uint8_t i, j;

  size += fprintf(stream, "%u\n", numBoards);

  for (i = 0; i < numBoards; i++) {
    size += fprintf(stream, "%u\n", presentBoards[i].mpId);
    size += fprintf(stream, "%u\n", presentBoards[i].availState);
    size += fprintf(stream, "%u\n", presentBoards[i].numMacEntries);
    for (j = 0; j < presentBoards[i].numMacEntries; j++) {
      char interfaceName[TMMI_IF_NAME_LEN];

      size += fprintf(stream, "%u\n",
		      presentBoards[i].macEntries[j].trafficType);
      strncpy(interfaceName, presentBoards[i].macEntries[j].interfaceName,
	      TMMI_IF_NAME_LEN);
      size += fprintf(stream, "%.*s\n", TMMI_IF_NAME_LEN, interfaceName);
      size += fprintf(stream, "%u\n", presentBoards[i].macEntries[j].vlan);
      size += fprintf(stream, "%u\n", presentBoards[i].macEntries[j].prio);
      size += fprintf(stream, "%.2x:%.2x:%.2x:%.2x:%.2x:%.2x\n",
		      presentBoards[i].macEntries[j].metaMacAddress[0],
		      presentBoards[i].macEntries[j].metaMacAddress[1],
		      presentBoards[i].macEntries[j].metaMacAddress[2],
		      presentBoards[i].macEntries[j].metaMacAddress[3],
		      presentBoards[i].macEntries[j].metaMacAddress[4],
		      presentBoards[i].macEntries[j].metaMacAddress[5]);
    }
  }

  return size;
}

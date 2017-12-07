/* ----------------------------------------------------------------------
 * %CCaseFile:  cert_sbb.h %
 * %CCaseRev:   /main/R3A/1 %
 * %CCaseDate:  2016-01-25 %
 * %CCaseDocNo: %
 * Author:      ehsake
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB %CCaseTemplateCopyrightYear% All rights reserved.
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
 * Rev        Date        Name        What
 * -----      ----------  --------    --------------------------
 *            20160125    ehsake      SECI update for eIDL encryption
 *            20161012    ehsake      Fixed Coverity warnings
 *            20170523    etomist     HV90099, fix for hard-coded mtd
 * ----------------------------------------------------------------------
 */


/* Copied and adapted for SECI from Network Loader */

#define _GNU_SOURCE /* required to remove warning for asprintf */

#include "cert_vc.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>

#include <rhai-sys.h>

#include <cert_trace.h>
#include <cert_file_vc.h>
#include <cert_crc.h>
#include <cert_sbb.h>



/*
 * Table copied from ETXLG
 *
 **************************************
 *        Magic pattern               *    4 bytes, "vecr" or "novc"
 **************************************
 *        Version number              *    4 bytes, 0, 0, 0, 2 in G2
 *                                    *
 **************************************
 *                                    *
 *        Containers length           *    4 bytes (Calculated from
 *                                    *    first length to last
 *                                    *    length inclusive. (Excludes
 **************************************    header and crc).
 //////       Container          //////
 **************************************
 *        Length of this              *    4 bytes (Calculated over
 *           container                *    the full container, length
 **************************************    and name.)
 *        Name of the this            *
 *            container               *    32 bytes, "Ericsson-vc-1" on
 *            container               *    VC boards, hash of serial number
 **************************************    on NO_VC boards
 *                                    *
 *        Securely stored data        *    Certificates on VC boards,
 *             section                *    same hash as name on NO_VC boards.
 *                                    *    Padding to write next length
 *                                    *    at 4byte alignment
 **************************************
 ////   End of Generic Container   ////
 **************************************
 *        Length of next              *    4 bytes
 *        generic container           *    NULL, if no more containers
 **************************************
 *        CRC for Vendor Credentials  *
 *        from MAGIC to Last length   *
 *        field which is NULL         *    4 bytes (CRC32 algorithm)
 **************************************

*/


static uint32_t pad_to_32bit_alignment(uint32_t value)
{
    return ((value + 4 - 1) / 4) * 4;
}

#define VC_SECRET_PREFIX "Ericsson-vc-1 "
#define MAX_DEVNAME_LEN 20
#define VENDOR_CREDENTIALS_PART "vendor_credentials"

static int compute_owner_secret(uint32_t *secret)
{
    struct rhai_sys_hwinfo hw_info;
    uint32_t *i, accumulator = 0, n, words_len;
    char padded[strlen(VC_SECRET_PREFIX) + sizeof(hw_info.serialnumber)];


    if (rhai_sys_hwinfo(&hw_info) < 0) {
        DEBUG("Failed to retrieve serial number%s","");
        return -1;
    }

    /* Initialize the whole padded secret length */
    memset(padded, 0, sizeof(padded));

    /* Create the secret as ascii*/
    strcpy(padded, VC_SECRET_PREFIX);
    strcat(padded, hw_info.serialnumber);

    /*
     * Compute the number of words to use when generating
     * the hashed secret
     */
    words_len = pad_to_32bit_alignment(strlen(padded)) / 4;

    /*
     * Consume the ascii secret one word at a time to create
     * the hashed secret.
     */
    for (n = 0, i = (uint32_t *)padded; n < words_len; n++, i++)
        accumulator = accumulator ^ ntohl(*i);

    *secret = accumulator;

    return 0;
}

static int decrypt_vc_container(struct vc_container *container)
{
    uint32_t secret, decrypted_len;
    uint16_t object_info;
    uint8_t *decrypted_data;
    int result = -1;

    if (compute_owner_secret(&secret) < 0)
        return result;

    decrypted_data = malloc(container->data_len);
    if (!decrypted_data) {
        DEBUG("Failed to allocate memory for vendor credentials secure storage data: %s","");
        return -1;
    }

    if (get_sdo_data(container->data, container->data_len,
             (uint64_t)secret,
             decrypted_data, &decrypted_len,
             &object_info)) {
        DEBUG("Failed to decrypt vendor credentials object in secure storage %s","");
    } else {
      memcpy(container->data, decrypted_data, decrypted_len);
      container->data_len = decrypted_len;
      result = 0;
    }

    free(decrypted_data);
    return result;
}


static struct vc_container **realloc_containers(struct vc_container **containers,
                           int new_count)
{
    struct vc_container **new_containers;

    new_containers = realloc(containers, new_count * sizeof(struct vc_container*));
    if (!new_containers) {
        DEBUG("Failed to allocate containers for trusted anchor: %s","");
        free(containers);
        return NULL;
    }

    new_containers[new_count - 1] = NULL;

    return new_containers;
}

static struct vc_container **read_vc_containers(void *data) {
    uint32_t container_len;
    int count = 0;
    struct vc_container **containers = NULL;

    while ((container_len = ntohl(*(uint32_t *)data))) {

        containers = realloc_containers(containers, (count + 2));
        if (!containers) {

            return NULL;
        }
        containers[count] = malloc(container_len);
        if (!containers[count]) {
            free(containers);
            return NULL;
        }


        memcpy(containers[count], data, container_len);

        containers[count]->data_len = container_len
            - sizeof(containers[count]->name)
            - sizeof(containers[count]->data_len);

        container_len = pad_to_32bit_alignment(container_len);

        if (decrypt_vc_container(containers[count]) < 0) {
          /* free previously allocated containers */
          for(int i = 0; i< count; i++)
            free(containers[count]);
          free(containers);
          return NULL;
        }

        data += container_len;
        count++;
    }



    return containers;
}

static
void format_str_field(char *field)
{
    char *read;
    char *write;

    read = field;
    write = field;
    while (*read != '\0') {
        if(!(*read == '"')) {
            *write++ = (*read);
        }
        read++;
    }
    *write = '\0';
}

static
int get_mtd_devname(const char *partname, char *devname)
{
    FILE *fp;
    int minor;
    int res;
    unsigned long int size;
    unsigned long int erasesize;
    char name[100];
    char line[255];

    if (partname == NULL) {
        //errno = EINVAL;
        return -1;
    }

    fp = fopen("/proc/mtd", "r");

    if (fp == NULL)
        return -1;

    while (fgets(line, sizeof(line), fp)) {
        memset(name, '\0', sizeof(name));
        res = sscanf(line, "mtd%d: %lx %lx %s",
                 &minor,
                 &size,
                 &erasesize,
                 name);
        if (res == 4) {
            format_str_field(name);
            if (strcmp(name, partname) == 0) {
                memset(devname, '\0', MAX_DEVNAME_LEN);
                /* Sanity check. */
                if (minor < 0 || minor > 10000) {
                    fclose(fp);
                    return -2;
                }
                sprintf(devname, "/dev/mtd%d", minor);
                fclose(fp);
                return 0;
            }
        }
    }
    fclose(fp);
    // errno = ENODEV;
    return -3;
}

static
int get_real_vc(char** resultP)
{
    int fd, ret = 0, vc_type = ERROR_READING_VC, bytes_to_read = 0, count=0;
    struct vc_hdr *hdr, *tmp_hdr;
    uint32_t len, image_crc, computed_crc;
    struct vc_container **vc_containers = NULL, *container = NULL;

    char mtd[MAX_DEVNAME_LEN] = {0};
    char *datap;
    int datalen;


    hdr = malloc(sizeof(*hdr));
    if (!hdr) {
        DEBUG("Could not allocate memory for Vendor Credentials header %s","");
        return ERROR_READING_VC;
    }

    ret = get_mtd_devname(VENDOR_CREDENTIALS_PART, mtd);
    if (ret) {
        DEBUG("Could not find mtd device for %s",
            VENDOR_CREDENTIALS_PART);
        return ERROR_READING_VC;
    }

    fd = open(mtd, O_RDONLY);
    if(fd < 0) {
        DEBUG("Could not open mtd device %s: %m", mtd);
        goto out_free;
    }

    /* Read the initial header */
    bytes_to_read = sizeof(*hdr);
    ret = read(fd, hdr, bytes_to_read);
    if (ret < 0) {
        DEBUG("Failed to read from vendor credentials: %s","");
        goto out_close;
    } else if (ret != bytes_to_read) {
        DEBUG("Failed to read %d bytes from vendor credentials, read %d",
            bytes_to_read, ret);
        goto out_close;
    }

    DEBUG("Verify Magic",NULL);

    /*
     * Verify the magic
     * if "vecr" - VC_EXISTS
     * if "novc" - NO_VC_EXISTS
     */
    if (strncmp(hdr->magic, "vecr", sizeof(hdr->magic)) == 0) {
        vc_type = VC_EXISTS;
    } else if (strncmp(hdr->magic, "novc", sizeof(hdr->magic)) == 0) {
        vc_type = NO_VC_EXISTS;
        goto out_close;
    } else {
        DEBUG("Bad magic in Vendor Credentials%s","");
        goto out_close;
    }


    DEBUG("Magic is %d",vc_type);

    /* Verify we have a known version number */
    if (hdr->version[0] != 0 ||
        hdr->version[1] != 0 ||
        hdr->version[2] != 0 ||
        hdr->version[3] != 2) {
        DEBUG("Unhandled version of vendor credentials%s","");
        goto out_close;
    }

    /* Convert length to correct endianness and add header and crc */
    len = sizeof(*hdr) + ntohl(hdr->containers_length) + 4;

    /* Reallocate space for all the data */
    tmp_hdr = realloc(hdr, len);
    if (!tmp_hdr) {
        DEBUG("Could not allocate memory for vendor credentials data: %s","");
        goto out_close;
    }
    hdr = tmp_hdr;

    /* Read all the data */
    bytes_to_read = len - sizeof(*hdr);
    ret = read(fd, hdr->data, bytes_to_read);
    if (ret < 0) {
        DEBUG("Failed to read from vendor credentials %s","");
        goto out_close;
    } else if (ret != bytes_to_read) {
        DEBUG("Failed to read %d bytes from vendor credentials, read %d",
            bytes_to_read, ret);
        goto out_close;
    }

    close(fd);
    fd = 0;

    image_crc = ntohl(*(uint32_t *)(hdr->data + len - sizeof(*hdr) - 4));
    computed_crc = crc((char *)hdr, len - 4);
    if (computed_crc != image_crc) {
      DEBUG("Bad CRC, computed CRC 0x%04x did not match Vendor Credentials CRC 0x%04x",
          computed_crc, image_crc);
      goto out_free;
    }

    vc_containers = read_vc_containers(hdr->data);
    if (!vc_containers)
      goto out_free;

    for (count = 0; vc_containers[count]; count++);

    if (count > 1) {
      DEBUG("Unsupported number of vendor credentials containers: %d",
          count);
      goto out_free;
    }

    container = vc_containers[0];

    /* TODO: Validate that we find valid PEM keys */
    datalen = (container->data_len);
    datap = (char*)(&container->data);
    *resultP = malloc(datalen+1); /* result should be null terminated */
    if(*resultP == NULL)
    {
      DEBUG("Could not allocate memory for output buffer %s","");
      goto out_free;
    } else {
      memset(*resultP,0,datalen+1);
      memcpy(*resultP,datap,datalen);
    }



out_close:
        if(fd)
          close(fd);

out_free:
    free(hdr);
    if(vc_containers) {
      for (count = 0; vc_containers[count]; count++) {
        free(vc_containers[count]);
      }
      free(vc_containers);
    }

    return vc_type;
}


SeciResultT get_vc(char** resultP)
{
  SeciResultT result = SECI_OK;
  if(get_real_vc(resultP) != VC_EXISTS) {
     DEBUG("No VC. Attempt to read from file%s","");
    result = get_vc_from_file(resultP);
  }

  return result;

}


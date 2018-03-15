/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include "lmc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define DEV_BUILD_FALSE
#define TEST_LMC "dp_tlnh_shmem_cm_test.lmc"
#define TEST_BIN "dp_tlnh_shmem_cm_test.bin"
#define TEST_BIN2 "dp_tlnh_shmem_cm_test2.bin"

/*
 * A lot of stuff in this file is either multiplied or divided by 2
 * The reason behind this is that most values in headers are measured in
 * 16-bit words.
 */

/**
 * swap is needed because header is in big endian, and linux
 * uses little endian
 */
#define byte_swap_2(A)  ((((uint16_t)(A) & 0xff00) >> 8) | \
                   (((uint16_t)(A) & 0x00ff) << 8))
#define byte_swap_4(A)  ((((uint32_t)(A) & 0xff000000) >> 24) | \
                   (((uint32_t)(A) & 0x00ff0000) >> 8)  | \
                   (((uint32_t)(A) & 0x0000ff00) << 8)  | \
                 (((uint32_t)(A) & 0x000000ff) << 24))

static unsigned int crc16_table[256] = {
0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50a5, 0x60c6, 0x70e7,
0x8108, 0x9129, 0xa14a, 0xb16b, 0xc18c, 0xd1ad, 0xe1ce, 0xf1ef,
0x1231, 0x0210, 0x3273, 0x2252, 0x52b5, 0x4294, 0x72f7, 0x62d6,
0x9339, 0x8318, 0xb37b, 0xa35a, 0xd3bd, 0xc39c, 0xf3ff, 0xe3de,
0x2462, 0x3443, 0x0420, 0x1401, 0x64e6, 0x74c7, 0x44a4, 0x5485,
0xa56a, 0xb54b, 0x8528, 0x9509, 0xe5ee, 0xf5cf, 0xc5ac, 0xd58d,
0x3653, 0x2672, 0x1611, 0x0630, 0x76d7, 0x66f6, 0x5695, 0x46b4,
0xb75b, 0xa77a, 0x9719, 0x8738, 0xf7df, 0xe7fe, 0xd79d, 0xc7bc,
0x48c4, 0x58e5, 0x6886, 0x78a7, 0x0840, 0x1861, 0x2802, 0x3823,
0xc9cc, 0xd9ed, 0xe98e, 0xf9af, 0x8948, 0x9969, 0xa90a, 0xb92b,
0x5af5, 0x4ad4, 0x7ab7, 0x6a96, 0x1a71, 0x0a50, 0x3a33, 0x2a12,
0xdbfd, 0xcbdc, 0xfbbf, 0xeb9e, 0x9b79, 0x8b58, 0xbb3b, 0xab1a,
0x6ca6, 0x7c87, 0x4ce4, 0x5cc5, 0x2c22, 0x3c03, 0x0c60, 0x1c41,
0xedae, 0xfd8f, 0xcdec, 0xddcd, 0xad2a, 0xbd0b, 0x8d68, 0x9d49,
0x7e97, 0x6eb6, 0x5ed5, 0x4ef4, 0x3e13, 0x2e32, 0x1e51, 0x0e70,
0xff9f, 0xefbe, 0xdfdd, 0xcffc, 0xbf1b, 0xaf3a, 0x9f59, 0x8f78,
0x9188, 0x81a9, 0xb1ca, 0xa1eb, 0xd10c, 0xc12d, 0xf14e, 0xe16f,
0x1080, 0x00a1, 0x30c2, 0x20e3, 0x5004, 0x4025, 0x7046, 0x6067,
0x83b9, 0x9398, 0xa3fb, 0xb3da, 0xc33d, 0xd31c, 0xe37f, 0xf35e,
0x02b1, 0x1290, 0x22f3, 0x32d2, 0x4235, 0x5214, 0x6277, 0x7256,
0xb5ea, 0xa5cb, 0x95a8, 0x8589, 0xf56e, 0xe54f, 0xd52c, 0xc50d,
0x34e2, 0x24c3, 0x14a0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
0xa7db, 0xb7fa, 0x8799, 0x97b8, 0xe75f, 0xf77e, 0xc71d, 0xd73c,
0x26d3, 0x36f2, 0x0691, 0x16b0, 0x6657, 0x7676, 0x4615, 0x5634,
0xd94c, 0xc96d, 0xf90e, 0xe92f, 0x99c8, 0x89e9, 0xb98a, 0xa9ab,
0x5844, 0x4865, 0x7806, 0x6827, 0x18c0, 0x08e1, 0x3882, 0x28a3,
0xcb7d, 0xdb5c, 0xeb3f, 0xfb1e, 0x8bf9, 0x9bd8, 0xabbb, 0xbb9a,
0x4a75, 0x5a54, 0x6a37, 0x7a16, 0x0af1, 0x1ad0, 0x2ab3, 0x3a92,
0xfd2e, 0xed0f, 0xdd6c, 0xcd4d, 0xbdaa, 0xad8b, 0x9de8, 0x8dc9,
0x7c26, 0x6c07, 0x5c64, 0x4c45, 0x3ca2, 0x2c83, 0x1ce0, 0x0cc1,
0xef1f, 0xff3e, 0xcf5d, 0xdf7c, 0xaf9b, 0xbfba, 0x8fd9, 0x9ff8,
0x6e17, 0x7e36, 0x4e55, 0x5e74, 0x2e93, 0x3eb2, 0x0ed1, 0x1ef0
};

/**
 * @brief
 * Parses LMs in LMC.
 *
 * @param lm_start          Pointer to first LM
 * @param lm_list           Output list of LMs
 *
 * @return                  0 if OK, or error.
 */
static int run_lm_parser (uint8_t * lm_start, struct lmc_lm ** lm_list);

/**
 * @brief
 * Calculates CRC16 of given data for given length.
 *
 * @param init              Initial value of CRC
 * @param data              Pointer to data that needs to be CRCed
 * @param length            Length of data
 *
 * @return                  CRC value
 */
static short unsigned int calc_crc16 (uint16_t init, char* data, uint32_t length);

/**
 * @brief
 * Calculates CRC16 of given data for given length.
 *
 * @param data              Pointer to data that needs to be CRCed
 * @param length            Length of data
 *
 * @return                  CRC value
 */
static short unsigned int mcalc_crc16 (char* data, uint32_t length);

/**
 * @brief
 * Swaps Endianes of integers in LMC header.
 *
 * @param lmc_header        Pointer to LMC header that needs byte swaping
 */
static void swap_lmc_endianess (struct lmc_hdr *lmc_header);

/**
 * @brief
 * Swaps Endianes of integers in RU LM header.
 *
 * @param lmc_header        Pointer to LM header that needs byte swaping
 */
static void swap_ru_lm_endianess (struct lmc_lm *lm_header);

static short unsigned int calc_crc16 (uint16_t init,
                               char* data,
                               uint32_t length)
{
  uint16_t crc = init, tmp;
  unsigned char dtmp;
  uint64_t i;

  for(i = 0; i < length; i++)
  {
    dtmp = (unsigned char) ((unsigned char) data[i]);
    tmp = (uint16_t) crc16_table[(crc>>8)];
    crc &= 0x00FF;
    crc |= ((tmp>>8)^(crc&0x00FF))<<8;
    crc &= 0xFF00;
    crc |= (tmp&0x00FF)^(dtmp);
  }
  /* printf("CRC=%04x\n", byteSwap2(crc)); */
  return (crc);
}

static short unsigned int mcalc_crc16 (char* data, uint32_t length)
{
  return calc_crc16(0, data, length);
}

static void swap_lmc_endianess (struct lmc_hdr *lmc_header)
{
    lmc_header->FV = byte_swap_2(lmc_header->FV);
    lmc_header->HLEN = byte_swap_2(lmc_header->HLEN);
    lmc_header->LMCT = byte_swap_2(lmc_header->LMCT);
    lmc_header->HWP = byte_swap_2(lmc_header->HWP);
    lmc_header->RHWF = byte_swap_2(lmc_header->RHWF);
    lmc_header->NSLM = byte_swap_2(lmc_header->NSLM);
    lmc_header->CLEN = byte_swap_4(lmc_header->CLEN);
    lmc_header->CRC = byte_swap_2(lmc_header->CRC);
    lmc_header->LCRC = byte_swap_2(lmc_header->LCRC);
    lmc_header->NBPN = byte_swap_4(lmc_header->NBPN);
}

static void swap_ru_lm_endianess (struct lmc_lm *lm_header)
{
    lm_header->header.hdr_ru_dp.LMFV =
            byte_swap_2(lm_header->header.hdr_ru_dp.LMFV);
    lm_header->header.hdr_ru_dp.LMT =
            byte_swap_2(lm_header->header.hdr_ru_dp.LMT);
    lm_header->header.hdr_ru_dp.NLM =
            byte_swap_4(lm_header->header.hdr_ru_dp.NLM);
    lm_header->header.hdr_ru_dp.HWARCH =
            byte_swap_4(lm_header->header.hdr_ru_dp.HWARCH);
    lm_header->header.hdr_ru_dp.HWTYPE =
            byte_swap_4(lm_header->header.hdr_ru_dp.HWTYPE);
    lm_header->header.hdr_ru_dp.DCL =
            byte_swap_4(lm_header->header.hdr_ru_dp.DCL);
    lm_header->header.hdr_ru_dp.CA =
            byte_swap_2(lm_header->header.hdr_ru_dp.CA);
    lm_header->header.hdr_ru_dp.CRC =
            byte_swap_2(lm_header->header.hdr_ru_dp.CRC);
}

static int run_lm_parser (uint8_t * lm_start, struct lmc_lm ** lm_list) {
#ifdef DEV_BUILD
    printf("Parsing LM list\n");
#endif

    struct lmc_lm * lm_local = (struct lmc_lm *) malloc(sizeof(struct lmc_lm));
    if (!lm_local){
        return LMC_ERROR_MALLOC;
    }
    (void)memset((void *)lm_local, 0, sizeof(struct lmc_lm));
    uint16_t lm_header_size;

    if (byte_swap_2(((struct lmc_lm *)lm_start)->header.hdr_short.LMFV) == LMC_RU_DP_FORMAT &&
        byte_swap_2(((struct lmc_lm *)lm_start)->header.hdr_short.LMT) == LMC_DSP   )
    {
        uint16_t crc;
        int lmdc_NS_L_A;
        lm_header_size =
                byte_swap_4(((struct lmc_lm *)lm_start)->header.hdr_ru_dp.NLM) * 2 -
                byte_swap_4(((struct lmc_lm *)lm_start)->header.hdr_ru_dp.DCL) * 2;
        memcpy(lm_local,lm_start,lm_header_size);

        crc = mcalc_crc16((char *)lm_local, lm_header_size -
                           sizeof(lm_local->header.hdr_ru_dp.CRC));
        swap_ru_lm_endianess(lm_local);

        if (lm_local->header.hdr_ru_dp.CRC != crc) {
            free(lm_local);
            return LMC_ERROR_CRC;
        }

        /* size of fields that precede data*/
        lmdc_NS_L_A = sizeof(lm_local->lmdc.dsp.NS) +
                sizeof(lm_local->lmdc.dsp.L) +
                sizeof(lm_local->lmdc.dsp.A);
        lm_local->lmdc.dsp.data =
                (char* )malloc(lm_local->header.hdr_ru_dp.DCL*2 - lmdc_NS_L_A);
        if(!lm_local->lmdc.dsp.data){
            free(lm_local);
            return LMC_ERROR_MALLOC;
        }

        memcpy(lm_local->lmdc.dsp.data,
               lm_start + lm_header_size + lmdc_NS_L_A,
               lm_local->header.hdr_ru_dp.DCL * 2 - lmdc_NS_L_A);

        *lm_list = lm_local;
    }

    else
    {
        free(lm_local);
        return LMC_ERROR_VER;
    }

    return 0;
}

int lmc_run_parser (uint8_t * lmc, int32_t lmc_size,
                    struct lmc_hdr **lmc_header,
                    struct lmc_lm ** lm_list) {

#ifdef DEV_BUILD
    printf("Parsing LMC header\n");
#endif

    struct lmc_hdr * lmc_local =
            (struct lmc_hdr *) malloc(sizeof(struct lmc_hdr));
    if(!lmc_local) {
        return LMC_ERROR_MALLOC;
    }
    (void)memset((void *)lmc_local, 0, sizeof(struct lmc_hdr));

    uint16_t lmc_header_size =
            byte_swap_2(((struct lmc_hdr *)lmc)->HLEN) * 2; /* size in bytes */

    memcpy(lmc_local, lmc, lmc_header_size);

    /* CRC of LMC header WITHOUT both CRC fields */
    uint16_t crc =
            mcalc_crc16((char *)lmc_local, lmc_header_size -
            (uint32_t)sizeof(lmc_local->CRC) - (uint32_t)sizeof(lmc_local->LCRC));
    /* CRC of LMs WITHOUT LMC header */
    uint16_t lcrc =
            mcalc_crc16((char *)lmc + lmc_header_size,
                    lmc_size - lmc_header_size);

    swap_lmc_endianess(lmc_local);

    if (crc != lmc_local->CRC) {
        free(lmc_local);
        return LMC_ERROR_CRC;
    }

    if (lcrc != lmc_local->LCRC) {
        free(lmc_local);
        return LMC_ERROR_LCRC;
    }
    if (lmc_local->NSLM < 1) {
        free(lmc_local);
        return  LMC_ERROR_ZEROLMS;
    }
    if (lmc_local->LMCT != LMC_LMC) {
        free(lmc_local);
        return LMC_ERROR_LMCT;
    }
    if ((lmc_local->HLEN + lmc_local->CLEN)*2 !=  lmc_size) {
        free(lmc_local);
        return LMC_ERROR_LMSZ;
    }

#ifdef DEV_BUILD
    printf ("Number of LMs detected: %d\n", lmc_local->NSLM);
#endif

    /* save pointer to the begining of LMs */
    uint8_t * lm_start = (uint8_t *)lmc;
    lm_start += lmc_header_size;

    int lmc_ret;
    int i;
    struct lmc_lm * lm_list_start;
    /* save first LM */
    lm_list_start = *lm_list;
    if(lmc_local->NSLM <= 0) {
        free(lmc_local);
        return LMC_ERROR_ZEROLMS;
    }

    for (i = 0; i < lmc_local->NSLM; i++) {
        lmc_ret = run_lm_parser(lm_start, lm_list);
        if (lmc_ret) {
            free(lmc_local);
            return lmc_ret;
        }
        /* move to next LM */
        lm_start += (*lm_list)->header.hdr_short.NLM * 2;
        lm_list = &(*lm_list)->next;

    }

    /* restore lm_list to first lm */
    *lm_list = lm_list_start;
    *lmc_header = lmc_local;
    return lmc_local->NSLM;
}

void lmc_free_lm_list(struct lmc_lm ** lm_list) {
    struct lmc_lm * next_node;
    int i;

    if((*lm_list) == NULL) {
        return;
    }

    for (i = 0;; i++)
    {
        next_node = (*lm_list)->next;
        free((*lm_list)->lmdc.dsp.data);
        free((*lm_list));
        *lm_list = next_node;
        if(next_node == NULL)
            return;

    }
}

#ifdef DEV_BUILD
/* FOR DEV only
 *
 * A lot of this stuff will go into run_lm* functions.
 * This is just to show that it works.
 * */
int main ( void ) {
    FILE * lmc_file = fopen(TEST_LMC, "r");
    uint32_t numbytes;

    /* Get the number of bytes */
    fseek(lmc_file, 0L, SEEK_END);
    numbytes = ftell(lmc_file);

    /* reset the file position indicator to
    the beginning of the file */
    fseek(lmc_file, 0L, SEEK_SET);

    uint8_t * lmc = (uint8_t *) malloc(numbytes);
    (void)memset((void *)lmc, 0, sizeof(struct lmc_hdr));
    fread(lmc, sizeof(uint8_t), numbytes, lmc_file);

    struct lmc_hdr * lmc_header;

    int32_t num_of_lms = 0;
    uint32_t lmc_header_size = 0;
    struct lmc_lm * lm_list;
    num_of_lms = lmc_run_parser((uint8_t *) lmc, numbytes, &lmc_header, &lm_list);

    if(num_of_lms < 1)
        return num_of_lms;

    printf ("\nLMC HEADER PARSED\n"
            "FV = %d\n"
            "HLEN = %d\n"
            "LMCT = %d\n"
            "CPN = %s\n"
            "CRS = %s\n"
            "HWP = %d\n"
            "RHWF = %d\n"
            "NSLM = %d\n"
            "CLEN = %d\n"
            "CRC = %d\n"
            "LCRC = %d\n"
            "NBPN = %x\n",
            (lmc_header->FV),
            (lmc_header->HLEN),
            (lmc_header->LMCT),
            lmc_header->CPN,
            lmc_header->CRS,
            (lmc_header->HWP),
            (lmc_header->RHWF),
            (lmc_header->NSLM),
            (lmc_header->CLEN),
            (lmc_header->CRC),
            (lmc_header->LCRC),
            (lmc_header->NBPN));

    printf ("\nRU LM HEADER PARSED\n"
            "LMFV = %d\n"
            "LMT = %d\n"
            "NLM = %d\n"
            "LMPN = %s\n"
            "LMRS = %s\n"
            "LMDAT = %.*s\n"
            "IDENTITY = %s\n"
            "HWARCH = %d\n"
            "HWTYPE = %d\n"
            "DCL = %d\n"
            "CA = %d\n"
            "CRC = %d\n",
            lm_list->header.hdr_ru_dp.LMFV,
            lm_list->header.hdr_ru_dp.LMT,
            lm_list->header.hdr_ru_dp.NLM,
            lm_list->header.hdr_ru_dp.LMPN,
            lm_list->header.hdr_ru_dp.LMRS,
            LMC_NO_OF_BYTES_IN_DAT,lm_list->header.hdr_ru_dp.LMDAT,
            lm_list->header.hdr_ru_dp.IDENTITY,
            lm_list->header.hdr_ru_dp.HWARCH,
            lm_list->header.hdr_ru_dp.HWTYPE,
            lm_list->header.hdr_ru_dp.DCL,
            lm_list->header.hdr_ru_dp.CA,
            lm_list->header.hdr_ru_dp.CRC);
#if 0
    printf ("\nRU LM HEADER PARSED\n"
               "LMFV = %d\n"
               "LMT = %d\n"
               "NLM = %d\n"
               "LMPN = %s\n"
               "LMRS = %s\n"
               "LMDAT = %.*s\n"
               "IDENTITY = %s\n"
               "HWARCH = %d\n"
               "HWTYPE = %d\n"
               "DCL = %d\n"
               "CA = %d\n"
               "CRC = %d\n",
               lm_list->next->header.hdr_ru_dp.LMFV,
               lm_list->next->header.hdr_ru_dp.LMT,
               lm_list->next->header.hdr_ru_dp.NLM,
               lm_list->next->header.hdr_ru_dp.LMPN,
               lm_list->next->header.hdr_ru_dp.LMRS,
               LMC_NO_OF_BYTES_IN_DAT,lm_list->next->header.hdr_ru_dp.LMDAT,
               lm_list->next->header.hdr_ru_dp.IDENTITY,
               lm_list->next->header.hdr_ru_dp.HWARCH,
               lm_list->next->header.hdr_ru_dp.HWTYPE,
               lm_list->next->header.hdr_ru_dp.DCL,
               lm_list->next->header.hdr_ru_dp.CA,
               lm_list->next->header.hdr_ru_dp.CRC);
#endif
    FILE *fp;

    fp = fopen(TEST_BIN, "w+");
    int i;
    int lmdc_NS_L_A_FFU_CRC;
    lmdc_NS_L_A_FFU_CRC = sizeof(lm_list->lmdc.dsp.NS) + sizeof(lm_list->lmdc.dsp.L) +
            sizeof(lm_list->lmdc.dsp.A) + sizeof(lm_list->lmdc.dsp.FFU) + sizeof(lm_list->lmdc.dsp.CRC);
    for (i = 0; i < (lm_list->header.hdr_ru_dp.DCL * 2) - lmdc_NS_L_A_FFU_CRC; i++) { // 12 for lmdc hdr and 4 for CRC
        fputc( *(lm_list->lmdc.dsp.data + i) , fp );
    }
    fclose(fp);
#if 0
    fp = fopen(TEST_BIN2, "w+");
    lmdc_NS_L_A_FFU_CRC = sizeof(lm_list->next->lmdc.dsp.NS) + sizeof(lm_list->next->lmdc.dsp.L) +
                sizeof(lm_list->next->lmdc.dsp.A) + sizeof(lm_list->next->lmdc.dsp.FFU) + sizeof(lm_list->next->lmdc.dsp.CRC);
    for (i = 0; i < (lm_list->next->header.hdr_ru_dp.DCL * 2) - lmdc_NS_L_A_FFU_CRC; i++) { // 12 for lmdc hdr and 4 for CRC
            fputc( *(lm_list->next->lmdc.dsp.data + i) , fp );
        }
    fclose(fp);
#endif
    lmc_free_lm_list(num_of_lms, &lm_list);
    free(lmc_header);
    fclose(lmc_file);
    return 0;
}
#endif /* DEV_BUILD */

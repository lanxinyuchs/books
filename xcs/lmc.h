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

#ifndef LMC_PARSER_H_
#define LMC_PARSER_H_

#include <stdint.h>

#define LMC_MAX_NO_OF_BYTES_IN_DP_PN        24
#define LMC_MAX_NO_OF_BYTES_IN_DP_RS        24
#define LMC_NO_OF_BYTES_IN_DAT              8
#define LMC_MAX_NO_OF_BYTES_IN_IDENTITY     32
#define LMC_MAX_NO_OF_BYTES_IN_PN           22
#define LMC_MAX_NO_OF_BYTES_IN_RS           8

typedef enum
{
    LMC_ERROR_MALLOC = -6,
    LMC_ERROR_LMCT = -5,
    LMC_ERROR_VER = -4,
    LMC_ERROR_LCRC = -3,
    LMC_ERROR_CRC = -2,
    LMC_ERROR_LMSZ = -1,
    LMC_ERROR_ZEROLMS = 0
} lmc_error_t;

typedef enum
{
    LMC_LMC = 21,
    LMC_TAP = 22
}lmc_lm_container_t;

typedef enum
{
    LMC_DSP  = 1,
    LMC_FPGA = 2,
    LMC_DSP_W_SEG = 3,
    LMC_DSP_W_ROLE = 4,
    LMC_FPGA_W_ROLE = 5,
    LMC_IMG = 6,
    LMC_SVL  = 1001
} lmc_lm_type_t;

typedef enum
{
    LMC_SVL_ADDED = 1,
    LMC_NEW_FORMAT = 2,
    LMC_LOADING_USING_IDENTITY = 3,
    LMC_KDU_SPECIFIC = 4,
    LMC_RU_DP_FORMAT = 5
} lmc_lm_format_t;

/* New LM header format supporting DP */
struct lmc_lm_hdr_ru_dp
{
    int16_t LMFV; /* LM format version - 5 */
    int16_t LMT;  /* LM type - 1 */
    int32_t NLM; /* number of 16-bit words measuring offset */
    char LMPN[LMC_MAX_NO_OF_BYTES_IN_DP_PN]; /* product number */
    char LMRS[LMC_MAX_NO_OF_BYTES_IN_DP_RS]; /* R-state of LM */
    char LMDAT[LMC_NO_OF_BYTES_IN_DAT]; /* format YYYYMMDD, not null terminated */
    char IDENTITY[LMC_MAX_NO_OF_BYTES_IN_IDENTITY];
    int32_t HWARCH; /* HW architecture */
    int32_t HWTYPE; /* HW type */
    int32_t DCL; /* number of 16-bit words in Data Container without CRC */
    int16_t CA; /* compression algorithm */
    uint16_t CRC; /* CRC of LM header */
};

struct lmc_lm_short
{
    int16_t LMFV; /* LM format version */
    int16_t LMT; /* LM type */
    int32_t NLM; /* offset measured in 16-bit words */
};

struct lmc_dsp_lmdc
{
    int32_t NS;
    uint32_t L; /* length of data measured in 16-bit words */
    uint32_t A;
    char * data;
    uint16_t FFU;   /* goes after lmdc blob */
    uint16_t CRC;   /* goes after lmdc blob */
};

struct lmc_lm
{
    union {
        struct lmc_lm_short    hdr_short;
        struct lmc_lm_hdr_ru_dp hdr_ru_dp; /* supported */
    } header;
    union
    {
        struct lmc_dsp_lmdc dsp;
    } lmdc;
    struct lmc_lm *next;
};

/* Struct containing all entries except BPNL for all versions. */
struct lmc_hdr
{
    uint16_t FV;    /* LMC format version */
    uint16_t HLEN;  /* Length of LMC header in 16-bit words */
    uint16_t LMCT;  /* Type of LMC */
    char CPN[LMC_MAX_NO_OF_BYTES_IN_PN];    /* Product number of LMC */
    char CRS[LMC_MAX_NO_OF_BYTES_IN_RS];    /* R-state of LMC */
    uint16_t HWP;
    uint16_t RHWF;
    uint16_t FFU;
    uint16_t NSLM;
    uint32_t CLEN;  /* measured in 16-bit words */
    uint16_t CRC;
    uint16_t LCRC;
    uint32_t NBPN;
};

/**
 * @brief
 * Frees memory taken for lm_list structure.
 * Both lm and data are freed.
 *
 * @param lm_list           List of LMs that needs to be freed.
 *
 */
void lmc_free_lm_list(struct lmc_lm ** lm_list);


/**
 * @brief
 * Gives out LMC header, and list of included LMs.
 *
 * @param lmc               Pointer to LMC.
 * @param lmc_size          Size of LMC in bytes.
 * @param lmc_header        Pointer to space where LMC header is stored.
 *                          Allocated memory needs to be freed!
 * @param lm_list           Output list of LMs
 *                          Allocated memory needs to be freed, see description
 *                          for free_lm_list above.
 *
 * @return                  Number of LMs in LMC returned through lm_list, or error.
 */
int lmc_run_parser (uint8_t * lmc, int32_t lmc_size, struct lmc_hdr **lmc_header, struct lmc_lm ** lm_list);

#endif /* LMC_PARSER_H_ */

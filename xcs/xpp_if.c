/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <stdio.h>
#include "itc.h"
/* XPP includes */
#include "log_tpt.h"

#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

/* XPP includes */
#include "xpp_init.h"
#include "xpai_xll_if.h"
#include "xpai_xht_if.h"
#include "xpai_xmh_if.h"
#include "xpai_xcbc_basic_if.h"
#include "xpai_xhl_if.h"

/* XDP */
#include "xdp_if.h"

union itc_msg {
	uint32_t msgno;
	struct XPAI_SelfTestIndS self_test_ind;
};

#define _UNUSED_ __attribute__((__unused__))

uint32_t XPAI_GenIOPortConf(uint32_t _UNUSED_ port,    /* !- FUNC -! */
                uint32_t _UNUSED_ mask,
                uint32_t _UNUSED_ IO_dir)
{
	return 0;
}

uint32_t XPAI_GenIOPortRead(uint32_t _UNUSED_ port, uint32_t _UNUSED_ mask) /* !- FUNC -! */
{
	return 0;
}

uint32_t XPAI_GenIOPortWrite(uint32_t _UNUSED_ port, /* !- FUNC -! */
                uint32_t _UNUSED_ mask,
                uint32_t _UNUSED_ data)
{
	return 0;
}

uint32_t XPAI_SetBankRegister(uint32_t _UNUSED_ number,  /* !- FUNC -! */
                uint32_t _UNUSED_ low,
                uint32_t _UNUSED_ high)
{
        return 0;
}

uint32_t X2_GetBankRegister(uint32_t _UNUSED_ number,
                uint32_t _UNUSED_ *lowP,
                uint32_t _UNUSED_ *highP)
{
        return 0;
}

/* xpai_xht_if.h */

uint16_t XPAI_Checksum(uint8_t _UNUSED_ *startAddr, uint32_t _UNUSED_ length) /* !- FUNC -! */
{
        return (uint16_t) 0;
}

/* Flash stub xpai_xmh_if.h */
int32_t XPAI_FlashErase(uint8_t _UNUSED_ *startAddr,
                uint32_t _UNUSED_ length) /* !- FUNC -! */
{
        return 0;
}
int32_t XPAI_FlashWrite(uint8_t _UNUSED_ *startAddr, /* !- FUNC -! */
                uint8_t _UNUSED_ *buffer,
                uint32_t _UNUSED_ length)
{
        return 0;
}
int32_t XPAI_FlashRead(uint8_t _UNUSED_ *startAddr, /* !- FUNC -! */
                uint8_t _UNUSED_ *buffer,
                uint32_t _UNUSED_ length)
{
        return 0;
}
struct XPAI_SubFileListS  *XPAI_LoadSubFileList(void) /* !- FUNC -! */
{
        return NULL;
}
int32_t XPAI_LoadSubFileOpen(uint32_t _UNUSED_ loadSubFileIx) /* !- FUNC -! */
{
        return 0;
}
int32_t XPAI_LoadSubFileRead(int32_t _UNUSED_ handle, /* !- FUNC -! */
                uint8_t _UNUSED_ *buffer,
                uint32_t _UNUSED_ pos,
                uint32_t _UNUSED_ size)
{
        return 0;
}

/* xpai_xcbc_basic_if.h */
uint32_t XPAI_GetBoardInfo(uint8_t _UNUSED_ *loadable,          /* !- FUNC -! */
                uint8_t _UNUSED_ *flash,
                uint8_t _UNUSED_ *filesystemAccess)
{
        return 0;
}

/* XDP Stuff
   xdai_xdmmi_if.h */
void XDAI_initXDPInterface(void)
{
        return;
}
uint32_t XPAI_HWLogWrite(char _UNUSED_ *dateTimeStr,  /* !- FUNC -! */
                    uint32_t   _UNUSED_ logId,
                    char _UNUSED_ *logStr)
{
    return 0;
}

void XPAI_RestartBoard(char _UNUSED_ *loadModule) /* !- FUNC -! */
{
	return;
}

uint32_t XPAI_SelfTest(uint32_t pid)
{
	union itc_msg *msg_p = NULL;

	TPT_TRACE(1, STR("XPAI_SelfTest(pid=0x%x)", pid));

	msg_p = itc_alloc(sizeof(struct XPAI_SelfTestIndS), XPAI_SELF_TEST_IND);
	msg_p->self_test_ind.result = XPAI_SELF_TEST_RESULT_PASSED;

	TPT_SEND_SIG(msg_p->msgno, pid, "XPAI_SELF_TEST_IND");

	itc_send(&msg_p, (itc_mbox_id_t)pid, ITC_MY_MBOX);

        return XPAI_SELF_TEST_OK;
}

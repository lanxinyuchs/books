/* ---------------------------------------------------------------------------
 *
 * @copyright Ericsson AB 2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#ifndef BUFPARAMS_H
#define BUFPARAMS_H

/*Buffer related data*/

#define XPLINK_BUFMODE_FULL_BUFFERING   0
#define XPLINK_BUFMODE_LINE_BUFFERING   1
#define XPLINK_BUF_SIZE1 80
#define XPLINK_BUF_SIZE2 900
#define XPLINK_BUF_SIZE3 948 /*prevent fragmentation*/

#define ITC_BUFMODE 1
#define ITC_BUF_SIZE 80

#define MINISHELL_BUFMODE 0
#define MINISHELL_BUF_SIZE 0


#define RCMD_REQ_BUFFER_SIZE 512 /*used only in test app*/

#endif /* BUFPARAMS_H */

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
#include <sys/types.h>
#include <itc.h>

struct ose_file {
        itc_mbox_id_t spid;
        uint32_t handle;
        uint32_t mode;
        uint32_t size;
        uint32_t append;
};

struct child_data {
        const char *path_prefix;
        const char *prompt;
        itc_mbox_id_t parent;
        itc_mbox_id_t mbox;
        itc_mbox_id_t child;
        const char *cmd_str;
        struct ose_file in;
        struct ose_file out;
        struct ose_file err;
	uint32_t num_of_trxm;
	const char *linkhandler_name;
};

void create_child(struct child_data *cd);

#ifndef TEST_AVC_H
#define TEST_AVC_H
/* ----------------------------------------------------------------------
 * %CCaseFile:	test_avc.h %
 * %CCaseRev:	/main/R6A/2 %
 * %CCaseDate:	2016-07-11 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Constants for test_avc.c.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2016 All rights reserved.
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
 * R6A/1      2016-06-19 erarafo     First version
 * ----------------------------------------------------------------------
 */

#define AVC_INIT                           1
#define AVC_FIN                            2

#define AVC_IMPL_SET                     101
#define AVC_IMPL_CLEAR                   102
#define AVC_SET_UINT32                   111
#define AVC_CLEAR_UINT32                 112
#define AVC_SET_32                       113
#define AVC_ADD_STRING                   121
#define AVC_UPDATE_STRING                122
#define AVC_REMOVE_STRING                123

#define AVC_ADD_STRINGS                  221
#define AVC_REPLACE_STRINGS              222
#define AVC_CLEAR_STRINGS                223
#define AVC_DELETE_STRINGS               224
#define AVC_CREATE_INSTANCE_SINGLE_ATTR  201
#define AVC_DELETE_INSTANCE              202

#define AVC_DISP                         901

#endif

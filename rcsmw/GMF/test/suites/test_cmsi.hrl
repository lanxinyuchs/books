%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
%%%
%%% The information in this document is the property of Ericsson.
%%%
%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%

%% must match definitions in cmsi.h
-define(CMSI_OK,                           0).
-define(CMSI_INVALID_PARAMETER_DIRECTION,  1).
-define(CMSI_INVALID_PARAMETER_TODNP,      2).
-define(CMSI_SEND_ERROR,                   3).
-define(CMSI_RECEIVE_ERROR,                4).
-define(CMSI_OBJECT_CLASS_NOT_FOUND,       5).
-define(CMSI_CLASS_NOT_FOUND,              6).
-define(CMSI_ATTRIBUTE_NOT_FOUND,          7).
-define(CMSI_ATTRIBUTE_NO_STRUCT_REF,      8).
-define(CMSI_LDN_NOT_FOUND,                9).
-define(CMSI_ACTION_NOT_FOUND,            10).

%% TODO; rename to match .h file
-define(MIM_TO_IMM, 1).
-define(IMM_TO_MIM, 2).



%% must match definition in ift_app: master.h
-define(CMSI, 10).


%% must match definitions in ift_app: test_cmsi.c
-define(CMSI_TRANSFORM,                      1).
-define(CMSI_PASS_INVALID_TODNP_A,           2).
-define(CMSI_CODE_EXAMPLE,                   3).
-define(CMSI_INITIATE_SERVICE,               4).
-define(CMSI_TERMINATE_SERVICE,              5).
-define(CMSI_CLEANUP,                        6).
-define(CMSI_TERMINATE_SERVICE_KEEP_HANDLE,  7).
-define(CMSI_PASS_INVALID_TODNP_B,           8).
-define(CMSI_IS_MO_CLASS_STRUCT,             9).
-define(CMSI_IS_ATTR_REF_2_STRUCT,           10).
-define(CMSI_GET_STRUCTREF_FOR_ATTR,         11).
-define(CMSI_GET_OBJECT_ID,                  12).
-define(CMSI_GET_LDN,                        13).
-define(CMSI_GET_OBJECT_IDS,                 14).
-define(CMSI_GET_LDNS,                       15).
-define(CMSI_GET_ME_ID,                      16).
-define(CMSI_GET_ADM_OP_ID,                  17).
-define(CMSI_GET_ACTION_NAME,                18).
-define(CMSI_GET_OAP_DATA,                   19).
-define(CMSI_GET_ME_USER_LABEL,              20).
-define(CMSI_GET_IMM_CLASS_NAME,             21).
-define(CMSI_GET_OAP_DATA2,                  22).
-define(CMSI_GET_BIDIR_ASS_FOR_CLIENT,       23).
-define(CMSI_GET_DN_PREFIX,                  24).

-define(ONESHOT, false).
-define(LONGLIVING, true).
-define(IGNORED_TRANSPORT, false).


-define(IGNORED_FUNCTION, 0).
-define(IGNORED_DIRECTION, 1).
-define(IGNORED_DN, "").


-define(REPEAT, 30).

-define(CLIENT_0, 0).
-define(CLIENT_1, 1).
-define(CLIENT_2, 2).


-define(CMSI_INTERACTIVE_HTTP_PORT, 8888).

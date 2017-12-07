%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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


%% must match definition in master.h of ift_app
-define(AVLI, 6).


%% must match definitions in cello_avli.sig
-define(CELLO_AVLI_SERVER_UP_IND,          16#1055E).
-define(CELLO_AVLI2_INITIATE_SERVICE_CFM,  16#10826).
-define(CELLO_AVLI_WRITE_CFM,              16#1055C).


%% must match definitions in $RCT_TOP/IFT/../csrc/test_avli.c
-define(AVLI_INITIATE_MEMORY,          1).
-define(AVLI2_FREE_MEMORY,             9).
-define(AVLI2_INITIATE_SERVICE,        10).
-define(AVLI2_WRITE_NODE_EVENT,        11).
-define(AVLI2_WRITE_PIU_EVENT,         12).
-define(AVLI2_WRITE_HW_EVENT,          13).
-define(AVLI2_WRITE_SERVICE_EVENT,     14).
-define(AVLI2_WRITE_OTHER_EVENT,       15).
-define(AVLI3_WRITE_PGM_EVENT,         16).
-define(AVLI4_WRITE_NODE_EVENT,        17).
-define(AVLI5_WRITE_HW_EVENT,          18).
-define(NONEXISTENT_FUNCTION_CODE,     99).

%% must match definitions in cello_avli.h
-define(CELLO_AVLI_SUCCESS,                      0).
-define(CELLO_AVLI_SERVICE_UNAVAIL,              1).
-define(CELLO_AVLI_TOO_LONG_STRING,              2).
-define(CELLO_AVLI_OUT_OF_MEMORY,                3).
-define(CELLO_AVLI_ILLEGAL_SIGNAL,               4).
-define(CELLO_AVLI_ILLEGAL_EVENT,                5).
-define(CELLO_AVLI_LOG_NOT_CREATED,              6).
-define(CELLO_AVLI_ILLEGAL_PARAM,                7).
-define(CELLO_AVLI_MEMORY_NOT_INITIATED,         8).
-define(CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV, 9).

-define(CELLO_AVLI_TIME_BY_AVLI, 16#FFFFFFFF).

-define(CELLO_AVLI_EVENT_NOT_USED, 0).
-define(CELLO_AVLI_IN_SERVICE, 1).
-define(CELLO_AVLI_OUT_OF_SERVICE, 2).
-define(CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE, 3).

-define(CELLO_AVLI_REASON_NOT_USED, 0).
-define(CELLO_AVLI_SHUTDOWN_COMMAND, 1).
-define(CELLO_AVLI_UNOPERATIONAL, 2).
-define(CELLO_AVLI_STARTING, 3).
-define(CELLO_AVLI_OPERATIONAL, 4).

-define(CELLO_AVLI_NONE, 0).
-define(CELLO_AVLI_MP, 1).
-define(CELLO_AVLI_BP, 2).

-define(CLIENT_1, 1).
-define(CLIENT_2, 2).


-define(AVLI_INTERACTIVE_HTTP_PORT, 8888).

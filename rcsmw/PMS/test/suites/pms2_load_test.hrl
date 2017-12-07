%%% --------------------------------------------------------
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
%%% --------------------------------------------------------


%% ===========================================
%% Shortcuts to other libraries.
%% ===========================================
-define(LIB,        pms2_test_lib).
-define(LOAD_LIB,   pms2_load_test_lib).
-define(XML,        xml_parse_lib).
-define(LOAD,       pmsLoadApp).

%% ===========================================
%% Buffer that will be added to timeout when
%% waiting for pms2ReportRop signal. Defined
%% in seconds.
%% ===========================================
-define(RP_TO_BUFFER, 5).

%% ===========================================
%% Timeout for ROP subscribe.
%% ===========================================
-define(ROP_SUB_TO, 10 * 60 * 1000).

%% ===========================================
%% Mapper for converting RP to GP.
%% ===========================================
-define(RP_TO_GP(RP), 
  case RP of
    ?TEN_SECONDS    -> ?GP_10_SEC;
    ?THIRTY_SECONDS -> ?GP_30_SEC;
    ?ONE_MIN        -> ?GP_1_MIN;
    ?FIVE_MIN       -> ?GP_5_MIN;
    ?FIFTEEN_MIN    -> ?GP_15_MIN;
    _ -> undef
  end
).

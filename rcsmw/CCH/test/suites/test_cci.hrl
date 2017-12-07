%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	test_cci.hrl %
%%% @author enekdav
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R10A/9

%%% @doc ==Local authorization==
%%% @end

-hrl_vsn('/main/R5A/R10A/9').
-hrl_date('2017-07-10').
-hrl_author('enekdav').

%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2016-02-19 ekurnik     Created
%%% R5A/2      2016-02-22 ekurnik     Changed CCI_DELTA_TOO_BIG 
%%% R5A/3      2016-03-01 etomist     Changed CCI_SIG_BASE
%%% R10A/1     2017-06-08 eivmiha     Added CCI_TINKER_STEP
%%% R10A/3     2017-06-26 evadumb     Added CCI_PV1, CCI_PV2 enum
%%% R10A/5     2017-06-27 evadumb     Added CCI2_SUBSCRIBE_REQ
%%% R10A/8     2017-06-28 ekurnik     Added CCI2_NTP_STATE_IND
%%% R10A/9     2017-07-10 enekdav     Added CCI_REASON_SLEW and CCI_REASON_SLEW_DISCREPANCY
%%% ----------------------------------------------------------

-define(CCI, 24).

-define(CCI_SIG_BASE,        16#18A4FD0).

-define(CCI_SUBSCRIBE_REQ,   ?CCI_SIG_BASE + 0).
-define(CCI_UNSUBSCRIBE_REQ, ?CCI_SIG_BASE + 1).
-define(CCI_TIME_UPDATE_IND, ?CCI_SIG_BASE + 2).
-define(CCI_SUBSCRIBE_CFM,   ?CCI_SIG_BASE + 3).
-define(CCI_SUBSCRIBE_REJ,   ?CCI_SIG_BASE + 4).
-define(CCI_UNSUBSCRIBE_CFM, ?CCI_SIG_BASE + 5).
-define(CCI_UNSUBSCRIBE_REJ, ?CCI_SIG_BASE + 6).
-define(CCI_SERVER_DOWN_IND, ?CCI_SIG_BASE + 7).
-define(CCI2_SUBSCRIBE_REQ,  ?CCI_SIG_BASE + 8).
-define(CCI2_NTP_STATE_IND,  ?CCI_SIG_BASE + 9).

%% CciRejectReason enum
-define(CCI_REJ_SERVICE_UNAVAIL, 0).
-define(CCI_REJ_ALREADY_SUBSCRIBED, 1).
-define(CCI_REJ_ALREADY_UNSUBSCRIBED, 2).
-define(CCI_REJ_MAX_SUBSCRIBERS_REACHED, 3).
-define(CCI_REJ_UNSUPPORTED_PV, 4).

%% CciTimeUpdateReason enum
-define(CCI_REASON_INITIAL, 0).
-define(CCI_REASON_STEP, 1).
-define(CCI_REASON_SLEW, 2).
-define(CCI_REASON_SLEW_DISCREPANCY, 3).

-define(CCI_DELTA_TOO_BIG, -1). %% 0xffffffff will be interpreted as -1
-define(CCI_MAX_DELTA, 60 * 60 * 1000000). %% 1h in us
-define(CCI_TINKER_STEP, 3000000).

%% CciProtocolVersion enum
-define(CCI_PV1, 1).
-define(CCI_PV2, 2).

%% CciNtpState enum
-define(CCI_NTP_OUT_OF_SYNC, 0).
-define(CCI_NTP_IN_SYNC, 1).

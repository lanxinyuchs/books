%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ComsaAlarm.hrl %
%%% Author:	etxpeno
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('11/190 55-CNA 113 348 Ux').
-hrl_vsn('/main/R11A/R12A/1').
-hrl_date('2017-10-25').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% -----      -------    --------    ------------------------
%%% R11A/2     2017-10-04  etxpeno     Created
%%% R12A/1     2017-10-26  etxpeno     Dialyzer fix
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description:
%%% ----------------------------------------------------------
-define(SA_NTF_INFO_ID_CORRELATION_INFO,  1000).
-define(SA_NTF_INFO_ID_ALARM_ID,          1001).
-define(SA_NTF_INFO_ID_UUID,              1002).

%% DNx where x is in the range 1-99
%% example: infoId for DN3 = NTF_INFOID_DN_BASE + 3
-define(SA_NTF_INFO_ID_DN_BASE,           1100).
-define(SA_NTF_INFO_ID_DN_MAX,            1199).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           comsaAppAlarm
%%% Description: Storing the alarms originating from the applications.
%%% ----------------------------------------------------------
-record(comsaAppAlarm, {key :: {MajorType :: non_neg_integer(),
			        MinorType :: non_neg_integer(),
			        DN        :: binary()} | '_',
			program_group :: binary() | undefined | '_'}).

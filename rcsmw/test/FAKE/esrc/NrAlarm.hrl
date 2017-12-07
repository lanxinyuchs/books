%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	NrAlarm.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R9A/1').
-hrl_author('etxjotj').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R9A/1      2017-04-10 etxjotj     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: Defining macros for binaries because binaries look ugly
%%% ----------------------------------------------------------

%% Defining macros for binaries because binaries look ugly
-define(registrar, <<"registrar">>).
-define(fmAlarmTypes, <<"fmAlarmTypes">>).
-define(fmId, <<"fmId">>).
-define(majorType, <<"majorType">>).
-define(minorType, <<"minorType">>).
-define(moClasses, <<"moClasses">>).
-define(specificProblem, <<"specificProblem">>).
-define(eventType, <<"eventType">>).
-define(probableCause, <<"probableCause">>).
-define(isStateful, <<"isStateful">>).
-define(additionalText, <<"additionalText">>).
-define(defaultSeverity, <<"defaultSeverity">>).
-define(action, <<"action">>).
-define(source, <<"source">>).

-define(sender, <<"sender">>).
-define(severity, <<"severity">>).
-define(text, <<"text">>).
-define(info, <<"info">>).



%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------
%-define(CONSTANT_NAME,               Value).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Description: Alarm types
%%% ----------------------------------------------------------

-record(nrAlarmType, {key :: {Registrar::binary(), FmKey::binary()},
		      majorType :: integer(),
		      minorType :: integer(),
		      moClasses :: binary(),
		      specificProblem :: binary(),
		      eventType :: binary(), 
		      probableCause :: integer(),
		      isStateful :: boolean(),
		      additionalText :: binary(),
		      defaultSeverity :: binary()}).




%%% ----------------------------------------------------------
%%% Description: Alarms
%%% ----------------------------------------------------------

-record(nrAlarm, 
	{key :: {Registrar::binary(), FmId::binary(), Sender::binary()},
	 severity :: binary(),
	 text :: binary(),
	 info :: [map()]}).


%%% ----------------------------------------------------------
%%% Description: Boards of hwcategory="OTHER" reported by CAT via LMHI
%%% ----------------------------------------------------------

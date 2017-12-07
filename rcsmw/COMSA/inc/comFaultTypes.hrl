%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comFaultTypes.hrl %
%%% Author:     etxjotj
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('').
-hrl_vsn('/main/R1A/1').
-hrl_date('2012-01-18').
-hrl_author('etxjotj').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2010 All rights reserved.
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
%%% R1A/1      2011-01-18 etxjotj     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

%% COM FM Severity levels taken from ComOamSpiNotificationFm_1.h

%%
%% Indicates that the problem has disappeared.
%% Only the stateful alarms can be cleared.
%% The stateless alarms (alerts) can not be cleared.
%%
-define(FmSeverityCleared,0).
%%
%% If the severity level is not specified, this level
%% will be used by default.
%%
-define(FmSeverityIndeterminate,1).
%%
%% Indicates that there is a problem.
%% The system may work without serious problems,
%% but the problem must be solved.
%%
-define(FmSeverityWarning,2).
%%
%% Indicates that there is a problem.
%% It must be solved.
%%
-define(FmSeverityMinor,3).
%%
%% Indicates that there is a problem.
%% It can lead to a partial or a complete
%% unfunctional system.
%%
-define(FmSeverityMajor,4).
%%
%% Indicates that there is a serious problem.
%% Immediate actions must be taken by the operator,
%% otherwise the system will be completely unfunctional.
%%
-define(FmSeverityCritical,5).

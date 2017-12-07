%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_fm.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_fm).
-vsn('/main/R1A/R2A/R3A/1').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2014 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/3      2012-05-02 etxpeno     Added spec().
%%% R2A/2      2013-11-12 erarafo     Adapted to change in comsaI:clear_alarm/2
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([register/0]).

-export([request_LKF_fault_alarm/0,
	 cancel_LKF_fault_alarm/0,
	 request_LKF_missing_alarm/0,
	 cancel_LKF_missing_alarm/0,
	 request_emergency_unlock_alarm/0,
	 cancel_emergency_unlock_alarm/0,
	 request_emergency_unlock_expired_alarm/0,
	 cancel_emergency_unlock_expired_alarm/0,
	 request_integration_unlock_alarm/0,
	 cancel_integration_unlock_alarm/0,
	 request_integration_unlock_expired_alarm/0,
	 cancel_integration_unlock_expired_alarm/0,
	 request_production_unlock_alarm/0,
	 cancel_production_unlock_alarm/0,
	 request_production_unlock_expired_alarm/0,
	 cancel_production_unlock_expired_alarm/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-define('EriProbableCause_timingProblemX733', 999).
-define(licensingDN, [<<"ManagedElement">>, <<"1">>, <<"SystemFunctions">>,
		      <<"1">>, <<"Licensing">>, <<"1">>]).
-define(majorType, 193).
-define(rcsMinorTypeOffset, 101). %FIXME
-define(minorType(Local),
	((?rcsMinorTypeOffset bsl 16) bor (Local band 16#ffff))).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec register() -> 'ok'.
register() ->
    %% comsaI:register_alarm(lihLKFFaultAlarm, ?majorType, ?minorType(100),
    %% 			  "Licensing", "lihLKFFaultAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    %% comsaI:register_alarm(lihLKFMissingAlarm, ?majorType, ?minorType(101),
    %% 			  "Licensing", "lihLKFMissingAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    %% comsaI:register_alarm(lihEmergencyAlarm, ?majorType, ?minorType(102),
    %% 			  "Licensing", "lihEmergencyAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    %% comsaI:register_alarm(lihEmergencyExpiredAlarm, ?majorType, ?minorType(103),
    %% 			  "Licensing", "lihEmergencyExpiredAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    %% comsaI:register_alarm(lihIntegrationAlarm, ?majorType, ?minorType(104),
    %% 			  "Licensing", "lihIntegrationAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    %% comsaI:register_alarm(lihIntegrationExpiredAlarm, ?majorType,
    %% 			  ?minorType(105), "Licensing",
    %% 			  "lihIntegrationExpiredAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    %% comsaI:register_alarm(lihProductionAlarm, ?majorType, ?minorType(106),
    %% 			  "Licensing", "lihProductionAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    %% comsaI:register_alarm(lihProductionExpiredAlarm, ?majorType,
    %% 			  ?minorType(107), "Licensing",
    %% 			  "lihProductionExpiredAlarm", processing,
    %% 			  ?'EriProbableCause_timingProblemX733', alarm, ""),
    ok.

-spec request_LKF_fault_alarm() -> 'ok'.
request_LKF_fault_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihLKFFaultAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_LKF_fault_alarm() -> 'ok'.
cancel_LKF_fault_alarm() ->
    %% comsaI:clear_alarm(lihLKFFaultAlarm, ?licensingDN, "Cleared"),
    ok.

-spec request_LKF_missing_alarm() -> 'ok'.
request_LKF_missing_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihLKFMissingAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_LKF_missing_alarm() -> 'ok'.
cancel_LKF_missing_alarm() ->
    %% comsaI:clear_alarm(lihLKFMissingAlarm, ?licensingDN, "Cleared"),
    ok.

-spec request_emergency_unlock_alarm() -> 'ok'.
request_emergency_unlock_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihEmergencyAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_emergency_unlock_alarm() -> 'ok'.
cancel_emergency_unlock_alarm() ->
    %% comsaI:clear_alarm(lihEmergencyAlarm, ?licensingDN, "Cleared"),
    ok.

-spec request_emergency_unlock_expired_alarm() -> 'ok'.
request_emergency_unlock_expired_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihEmergencyExpiredAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_emergency_unlock_expired_alarm() -> 'ok'.
cancel_emergency_unlock_expired_alarm() ->
    %% comsaI:clear_alarm(lihEmergencyExpiredAlarm, ?licensingDN, "Cleared"),
    ok.

-spec request_integration_unlock_alarm() -> 'ok'.
request_integration_unlock_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihIntegrationAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_integration_unlock_alarm() -> 'ok'.
cancel_integration_unlock_alarm() ->
    %% comsaI:clear_alarm(lihIntegrationAlarm, ?licensingDN, "Cleared"),
    ok.

-spec request_integration_unlock_expired_alarm() -> 'ok'.
request_integration_unlock_expired_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihIntegrationExpiredAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_integration_unlock_expired_alarm() -> 'ok'.
cancel_integration_unlock_expired_alarm() ->
    %% comsaI:clear_alarm(lihIntegrationExpiredAlarm, ?licensingDN, "Cleared"),
    ok.

-spec request_production_unlock_alarm() -> 'ok'.
request_production_unlock_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihProductionAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_production_unlock_alarm() -> 'ok'.
cancel_production_unlock_alarm() ->
    %% comsaI:clear_alarm(lihProductionAlarm, ?licensingDN, "Cleared"),
    ok.

-spec request_production_unlock_expired_alarm() -> 'ok'.
request_production_unlock_expired_alarm() ->
    %% Msg = "FIXME",
    %% comsaI:send_alarm(lihProductionExpiredAlarm, critical, ?licensingDN, Msg),
    ok.

-spec cancel_production_unlock_expired_alarm() -> 'ok'.
cancel_production_unlock_expired_alarm() ->
    %% comsaI:clear_alarm(lihProductionExpiredAlarm, ?licensingDN, "Cleared"),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

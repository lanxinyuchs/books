%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxjotj
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/1

%%% @doc ==Header==
%%% This library module handles the legacy interface towards COM/RCS
%%% @end

-module(nrComAlarm).
-author('etxjotj').

%%% ----------------------------------------------------------
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Date       Name        What
%%% ---------  --------    ------------------------
%%% R9A/1      2017-04-05   etxjotj     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([register_alarm/2, raise_and_cease/1, unregister_alarm/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%% 	 code_change/3, terminate/2]).

%-include("ComFm.hrl").
-include("NrAlarm.hrl").

%% From ComFm.hrl
-record(fmAlarmType, {fmAlarmTypeId,
                      majorType,
                      minorType,
                      moClasses,
                      specificProblem,
                      eventType,
                      probableCause,
                      isStateful,
                      additionalText,
                      configuredSeverity,
                      defaultSeverity}).
		    
-define(EventType_OTHER, 1).
-define(EventType_COMMUNICATIONSALARM, 2).
-define(EventType_QUALITYOFSERVICEALARM, 3).
-define(EventType_PROCESSINGERRORALARM, 4).
-define(EventType_EQUIPMENTALARM, 5).
-define(EventType_ENVIRONMENTALALARM, 6).
-define(EventType_INTEGRITYVIOLATION, 7).
-define(EventType_OPERATIONALVIOLATION, 8).
-define(EventType_PHYSICALVIOLATION, 9).
-define(EventType_SECURITYSERVICEORMECHANISMVIOLATION, 10).
-define(EventType_TIMEDOMAINVIOLATION, 11).

-define(SeverityLevel_CRITICAL, 3).
-define(SeverityLevel_MAJOR, 4).
-define(SeverityLevel_MINOR, 5).
-define(SeverityLevel_WARNING, 6).




%% -define(sender,<<"sender">>).
%% -define(severity,<<"severity">>).
%% -define(text,<<"text">>).
%% -define(info,<<"info">>).
			   

%-include("template.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Register alarm types towards COM
%%% Must run within the scope of a transaction
%%% @end
%%% ----------------------------------------------------------

-spec register_alarm(Registrar::binary(), FmAlarmType::map()) -> ok.

register_alarm(Registrar, FmAlarmType) ->
    FmId = maps:get(?fmId, FmAlarmType),
    %% Assuming Registrar and FmId are something that can be treated as a string
    FmAlarmTypeId = make_id(Registrar, FmId),
    FmAlarmTypeObj = 
	#fmAlarmType{fmAlarmTypeId = {"1","1","1","1",FmAlarmTypeId},
		     majorType = maps:get(?majorType, FmAlarmType),
		     minorType = maps:get(?minorType, FmAlarmType),
		     moClasses = mapgetstr(?moClasses, FmAlarmType),
		     specificProblem = mapgetstr(?specificProblem, FmAlarmType),
		     eventType = event_type(maps:get(?eventType, FmAlarmType)),
		     probableCause = maps:get(?probableCause, FmAlarmType),
		     isStateful = maps:get(?isStateful, FmAlarmType),
		     additionalText = mapgetstr(?additionalText, FmAlarmType),
		     defaultSeverity = severity(maps:get(?defaultSeverity, FmAlarmType))
		     },
    case lists:member(fmAlarmType, mnesia:system_info(tables)) of
	true ->
	    mnesia:write(FmAlarmTypeObj);
	false ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Raise and cease alarms towards COM
%%% @end
%%% ----------------------------------------------------------

-type snmp_action()::{raise, NrAlarm::#nrAlarm{}, NrAlarmType::#nrAlarmType{}}|
		     {cease, NrAlarm::#nrAlarm{}, NrAlarmType::#nrAlarmType{}}.

-spec raise_and_cease(SnmpActions::[snmp_action()]) -> ok.

raise_and_cease(SnmpActions) when is_list(SnmpActions) ->
    [raise_and_cease(Action)||Action<-SnmpActions],
    ok;
raise_and_cease({raise, NrAlarm, NrAlarmType}) ->
    AlarmName = list_to_atom(
		  make_id(element(1, NrAlarm#nrAlarm.key),
			  element(2, NrAlarm#nrAlarm.key))),
    Severity = list_to_atom(NrAlarm#nrAlarm.severity),
    DN = split_dn(element(3, NrAlarm#nrAlarm.key)),
    AdditionalText = get_at(NrAlarm, NrAlarmType),
    AdditionalInfo = [], % figure this out later
    comsaI:send_alarm(AlarmName, Severity, DN, AdditionalText, AdditionalInfo);

raise_and_cease({cease, NrAlarm, NrAlarmType}) ->
    AlarmName = list_to_atom(
		  make_id(element(1, NrAlarm#nrAlarm.key),
			  element(2, NrAlarm#nrAlarm.key))),
    DN = split_dn(element(3, NrAlarm#nrAlarm.key)),
    AdditionalText = get_at(NrAlarm, NrAlarmType),
    AdditionalInfo = [], % figure this out later
    comsaI:clear_alarm(AlarmName, DN, AdditionalText, AdditionalInfo).

%%% ----------------------------------------------------------
%%% @doc Unregister alarm types towards COM
%%% @end
%%% ----------------------------------------------------------

-spec unregister_alarm(NrAlarmTypeKeys::[tuple()]|tuple()) -> ok.

unregister_alarm(NrAlarmTypeKeys) when is_list(NrAlarmTypeKeys) ->
    Fun = fun() ->
		  [unregister_alarm(NrAlarmTypeKey)||
		      NrAlarmTypeKey<-NrAlarmTypeKeys],
		  ok
	  end,
    case lists:member(fmAlarmType, mnesia:system_info(tables)) of
	true -> 
	    case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		{aborted, Reason} ->
		    error_logger:error_report([{?MODULE, com_unregister_alarm,
						[NrAlarmTypeKeys]},
					       {aborted, Reason}]),
		    {error, {internal_server_error, <<"Software error">>}}
	    end;
	false -> 
	    ok
    end;
unregister_alarm(NrAlarmTypeKey) ->
    FmAlarmTypeId = make_id(NrAlarmTypeKey),
    mnesia:delete({fmAlarmType, {"1","1","1","1",FmAlarmTypeId}}).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------





make_id({Registrar, FmId}) ->
    make_id(Registrar, FmId).
make_id(Registrar, FmId) ->
    binary_to_list(<<Registrar/binary, <<".">>/binary, FmId/binary>>).

    
%% Returns a binary string as a native string.
mapgetstr(Key, Map) ->
    binary_to_list(maps:get(Key, Map)).

event_type(<<"other">>) -> ?EventType_OTHER;
event_type(<<"communicationsAlarm">>) -> ?EventType_COMMUNICATIONSALARM;
event_type(<<"qualityOfServiceAlarm">>) -> ?EventType_QUALITYOFSERVICEALARM;
event_type(<<"processingErrorAlarm">>) -> ?EventType_PROCESSINGERRORALARM;
event_type(<<"equipmentAlarm">>) -> ?EventType_EQUIPMENTALARM;
event_type(<<"environmentalAlarm">>) -> ?EventType_ENVIRONMENTALALARM;
event_type(<<"integrityViolation">>) -> ?EventType_INTEGRITYVIOLATION;
event_type(<<"operationalViolation">>) -> ?EventType_OPERATIONALVIOLATION;
event_type(<<"physicalViolation">>) -> ?EventType_PHYSICALVIOLATION;
event_type(<<"securityServiceOrMechanismViolation">>) -> 
    ?EventType_SECURITYSERVICEORMECHANISMVIOLATION;
event_type(<<"timeDomainViolation">>) -> ?EventType_TIMEDOMAINVIOLATION.

severity(<<"critical">>) -> ?SeverityLevel_CRITICAL;
severity(<<"major">>) -> ?SeverityLevel_MAJOR;
severity(<<"minor">>) -> ?SeverityLevel_MINOR;
severity(<<"warning">>) -> ?SeverityLevel_WARNING.
		


split_dn(FullDN) ->
    [list_to_binary(X)||X<-string:tokens(binary_to_list(FullDN), ",")].


    
get_at(NrAlarm, NrAlarmType) ->
    case size(NrAlarm#nrAlarm.text) of
	0 ->
	    NrAlarmType#nrAlarmType.additionalText;
	_ ->
	    NrAlarm#nrAlarm.text
    end.


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% ----------------------------------------------------------
%%% info_msg
%%% ----------------------------------------------------------

%% info_msg(Format) ->
%%     info_msg(Format, []).

%% info_msg(Format, Args) ->
%%     error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% warning_msg
%%% ----------------------------------------------------------
%% warning_msg(Format, Args) ->
%%     error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% error_msg
%%% ----------------------------------------------------------
%% error_msg(Format, Args) ->
%%     error_logger:error_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

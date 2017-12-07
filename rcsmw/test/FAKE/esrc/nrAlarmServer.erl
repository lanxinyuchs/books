%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxjotj
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/3

%%% @doc ==FM service main server==
%%% This is the main server module responsible for executing the logic in
%%% the FM service. It gets its input from the REST API and outputs results
%%% to the SNMP stack.
%%% @end

-module(nrAlarmServer).
-behaviour(gen_server).
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

-export([start/0]).
-export([attach/2, detach/1, raise_and_cease/2, getAAL/1, getAAL/2]).

%% Test functions
-export([run_attach/0, run_raise/0, run_cease/0, run_detach/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-include("NrAlarm.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the NR service
%%% @end
%%% ----------------------------------------------------------

start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Attach a registrar to the FM service
%%% The Fm alarm type is a JSONE decoded structure
%%% @end
%%% ----------------------------------------------------------

-type attach_ret()::ok |
		    {error, {conflict, Reason::string()}} |
		    {error, {internal_server_error, Reason::string()}}.

-spec attach(Registrar::binary(), FmAlarmTypes::[map()]) -> attach_ret().


attach(Registrar, FmAlarmTypes) ->
    gen_server:call(?MODULE, {attach, Registrar, FmAlarmTypes}).


%%% ----------------------------------------------------------
%%% @doc Detach a registrar to the FM service
%%% @end
%%% ----------------------------------------------------------

-type detach_ret() :: ok | {error, {internal_server_error, Reason::binary()}}.
-spec detach(Registrar::binary()) -> detach_ret().

detach(Registrar) ->
    gen_server:call(?MODULE, {detach, Registrar}).

%%% ----------------------------------------------------------
%%% @doc Raise or cease one or more alarms
%%% @end
%%% ----------------------------------------------------------

-type raise_and_cease_ret()::ok |
			     {error, {bad_request, Reason::string()}} |
			     {error, {not_found, Reason::string()}} |
			     {error, {gone, Reason::string()}} | 
			     {error, {internal_server_error, Reason::string()}}.

-spec raise_and_cease(Registrar::binary(), AlarmActions::[map()]) -> 
			     raise_and_cease_ret().

raise_and_cease(Registrar, AlarmActions) ->
    gen_server:call(?MODULE, {raise_and_cease, Registrar, AlarmActions}).


%%% ----------------------------------------------------------
%%% @doc Get info about current alarms
%%% @end
%%% ----------------------------------------------------------

getAAL(Registrar) ->
    gen_server:call(?MODULE, {getAAL, Registrar}).

getAAL(Registrar, FmId) ->
    gen_server:call(?MODULE, {getAAL, Registrar, FmId}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init(Args) ->
    gen_server:cast(?MODULE, {initialize, Args}),
    {ok, initializing}.

initialize(Args) ->
    info_msg("Initialize ~p~n",[Args]),

    %% Mnesia initialization
    setup_mnesia(),
    create_types_table(),
    create_alarms_table(),

    #{}.

handle_call({attach, Registrar, FmAlarmTypes}, _From, State) 
  when is_map(State) ->
    Response = handle_attach(Registrar, FmAlarmTypes),
    {reply, Response, State};
handle_call({detach, Registrar}, _From, State) when is_map(State)  ->
    Response = handle_detach(Registrar),
    {reply, Response, State};

handle_call({raise_and_cease, Registrar, AlarmActions}, _From, State) 
  when is_map(State) ->
    Response = handle_raise_and_cease(Registrar, AlarmActions),
    {reply, Response, State};

handle_call(Request, From, State) when is_map(State)  ->
    info_msg("call ~p ~p ~p",[Request, From, State]),
    {reply, ok, State};

handle_call(Request, From, State)  ->
    info_msg("call ~p ~p ~p",[Request, From, State]),
    {reply, ok, State}.

handle_cast({initialize, Args}, initializing) ->
    {noreply, initialize(Args)};
handle_cast(Request, State) ->
    info_msg("cast ~p ~p~n",[Request, State]),
    {noreply, State}.

handle_info(Request, State) ->
    info_msg("info ~p ~p~n",[Request, State]),
    {noreply, State}.

code_change(OldVersion, State, Extra) ->
    info_msg("Code change ~p ~p ~p ~n",[OldVersion, State, Extra]),
    {ok, State}.

terminate(Reason, State) ->
    info_msg("Terminate ~p ~p ~n",[Reason, State]),
    ok.



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% Description: Start mnesia if in standalone mode

setup_mnesia() ->
    case mnesia:system_info(is_running) of
	yes ->
	    ok;
	no ->
	    application:set_env(mnesia, dir, "."),
	    mnesia:start()
    end.

create_types_table() ->
    case lists:member(nrAlarmType, mnesia:system_info(tables)) of
	true ->
	    ok;
	false ->
	    info_msg("Creating nrAlarmType~n"),
	    %% DbNodes = mnesia:system_info(db_nodes),
	    mnesia:create_table(
	      nrAlarmType, 
	      [{type, set},
	       %% {disc_copies, DbNodes},
	       {attributes, record_info(fields, nrAlarmType)}])
    end.

create_alarms_table() ->
    case lists:member(nrAlarm, mnesia:system_info(tables)) of
	true ->    
	    ok;
	false ->
	    info_msg("Creating nrAlarm~n"),
	    %% DbNodes = mnesia:system_info(db_nodes),
	    mnesia:create_table(
	      nrAlarm,
	      [{type, set},
	       %% {disc_copies, DbNodes},
	       {attributes, record_info(fields, nrAlarm)}])
    end.

handle_attach(Registrar, FmAlarmTypes) ->
    Fun = 
	fun() ->
		%% Check if registrar is known
		WP = mnesia:table_info(nrAlarmType, wild_pattern),
		Pattern = WP#nrAlarmType{key={Registrar, '_'}},
		case mnesia:match_object(Pattern) of
		    [] -> ok;
		    _ -> mnesia:abort(conflict)
		end,
		%% Registrar alarm types
		[begin
		     register_alarm(Registrar, FmAlarmType),
		     com_register_alarm(Registrar, FmAlarmType)
		 end||
		    FmAlarmType <- FmAlarmTypes],
		ok
	end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, conflict} ->
	    Msg = binary_to_list(Registrar) ++ " is already attached",
	    {error, {conflict, Msg}};
	{aborted, Reason} ->
	    error_logger:error_report([{?MODULE, handle_attach, 
					[Registrar, FmAlarmTypes]},
				       {aborted, Reason}]),
	    {error, {internal_server_error, <<"Software error">>}}
    end.
				
register_alarm(Registrar, FmAlarmType) ->
    NrAlarmType = 
	#nrAlarmType{key = {Registrar, maps:get(?fmId, FmAlarmType)},
		     majorType = maps:get(?majorType, FmAlarmType),
		     minorType = maps:get(?minorType, FmAlarmType),
		     moClasses = maps:get(?moClasses, FmAlarmType),
		     specificProblem = maps:get(?specificProblem, FmAlarmType),
		     eventType = maps:get(?eventType, FmAlarmType),
		     probableCause = maps:get(?probableCause, FmAlarmType),
		     isStateful = maps:get(?isStateful, FmAlarmType),
		     additionalText = maps:get(?additionalText, FmAlarmType),
		     defaultSeverity = maps:get(?defaultSeverity, FmAlarmType)
		     },
    mnesia:write(NrAlarmType),
    error_logger:info_report(lists:zip([new|record_info(fields, nrAlarmType)],
				       tuple_to_list(NrAlarmType))).


handle_raise_and_cease(Registrar, AlarmActions) ->
    %% All validations happens in the mnesia transaction
    %% After it passes then issue snmp traps, becuase that is a side effect
    %% that should not be in the transaction
    Fun = fun() -> {ok, [execute_action(Registrar, Action)||
			    Action<-AlarmActions]}
	  end,
    case mnesia:transaction(Fun) of
	{atomic, {ok, SnmpActions}} ->
	    execute_snmp(SnmpActions),
	    com_raise_and_cease(SnmpActions);
	{aborted, {bad_request, Reason}} ->
	    {error, {bad_request, Reason}};
	{aborted, {not_found, Reason}} ->
	    {error, {not_found, Reason}};
	{aborted, {gone, Reason}} ->
	    {error, {gone, Reason}};
	{aborted, Reason} ->
	    error_logger:error_report([{?MODULE, handle_raise_and_cease},
				       [Registrar, AlarmActions],
				       {aborted, Reason}])
    end.

execute_action(Registrar, Action) ->
    case mapgetatom(?action, Action) of
	raise ->
	    Parsed = 
		{raise,
		 maps:get(?fmId, Action),
		 maps:get(?severity, Action),
		 maps:get(?sender, Action),
		 maps:get(?text, Action),
		 maps:get(?info, Action)},
	    execute_raise(Registrar, Parsed);
	cease ->
	    Parsed = 
		{cease, maps:get(?fmId, Action), maps:get(?source, Action)},
	    execute_cease(Registrar, Parsed);
	Type ->
	    Msg = atom_to_list(Type)++" is not allowed",
	    mnesia:abort({bad_request, list_to_binary(Msg)})
    end.

mapgetatom(Key, Map) ->
    list_to_atom(binary_to_list(maps:get(Key, Map))).

execute_raise(Registrar, {raise, FmId, Severity, Sender, Info, Text}) ->
    case mnesia:read({nrAlarmType, {Registrar, FmId}}) of
	[NrAlarmType] ->
	    assert_severity(Severity),
	    Key = {Registrar, FmId, Sender},
	    NrAlarm = #nrAlarm{key=Key,
			       severity=Severity,
			       text=Text,
			       info=Info},
	    if NrAlarmType#nrAlarmType.isStateful -> mnesia:write(NrAlarm);
	       true -> ok %Do not record alerts in mnesia
	    end,
	    {raise, NrAlarm, NrAlarmType};
	[] ->
	    Msg = <<" is not known">>,
	    WP = mnesia:table_info(nrAlarmType, wild_pattern),
	    Pattern = WP#nrAlarmType{key={Registrar, '_'}},
	    case mnesia:match_object(Pattern) of
		[] -> 
		    mnesia:abort({gone, <<Registrar/binary, Msg/binary>>});
		_ ->
		    mnesia:abort({not_found, <<FmId/binary, Msg/binary>>})
	    end
    end.

execute_cease(Registrar, {cease, FmId, Source}) ->
    Key = {Registrar, FmId, Source},
    case mnesia:read({nrAlarm, Key}) of
	[NrAlarm] ->
	    mnesia:delete({nrAlarm, Key}),
	    [NrAlarmType] = mnesia:read({nrAlarmType, {Registrar, FmId}}),
	    {cease, NrAlarm, NrAlarmType};
	[] ->	    
	    Msg = <<" is not known">>,
	    case mnesia:read({nrAlarmType, {Registrar, FmId}}) of
		[] ->
		    WP = mnesia:table_info(nrAlarmType, wild_pattern),
		    Pattern = WP#nrAlarmType{key={Registrar, '_'}},
		    case mnesia:match_object(Pattern) of
			[] -> 
			    mnesia:abort({gone, <<Registrar/binary, Msg/binary>>});
			_ ->
			    mnesia:abort({not_found, <<FmId/binary, Msg/binary>>})
		    end;
		_ ->
		    mnesia:abort({not_found, <<Source/binary, Msg/binary>>})
	    end
    end.
		    
assert_severity(<<"critical">>) -> ok;
assert_severity(<<"major">>) -> ok;
assert_severity(<<"minor">>) -> ok;
assert_severity(<<"warning">>) -> ok;
assert_severity(Unknown) -> 
    Msg = <<" is not a valid severity">>,
    mnesia:abort({bad_request, <<Unknown/binary, Msg/binary>>}).
    

execute_snmp(SnmpActions) ->
    [execute_snmp_action(SnmpAction)||SnmpAction<-SnmpActions],
    ok.

execute_snmp_action({raise, NrAlarm, NrAlarmType}) ->
    SequenceNo = 12345,
    AT = case size(NrAlarm#nrAlarm.text) of
	     0 ->
		 NrAlarmType#nrAlarmType.additionalText;
	     _ ->
		 NrAlarm#nrAlarm.text
	 end,
    AlarmData = 
	[{eriAlarmActiveManagedObject, element(3, NrAlarm#nrAlarm.key)},
	 {eriAlarmActiveMajorType, NrAlarmType#nrAlarmType.majorType},
	 {eriAlarmActiveMinorType, NrAlarmType#nrAlarmType.minorType},
	 {eriAlarmActiveSpecificProblem, NrAlarmType#nrAlarmType.specificProblem},
	 {eriAlarmActiveLastSequenceNo, SequenceNo},
	 {eriAlarmActiveEventType, NrAlarmType#nrAlarmType.eventType},
	 {eriAlarmActiveEventTime, snmp:date_and_time()},
	 {eriAlarmActiveProbableCause, NrAlarmType#nrAlarmType.probableCause},
	 {eriAlarmNObjAdditionalText, AT},
	 {eriAlarmNObjMoreAdditionalText, false}, % Figure thus out later
	 {eriAlarmNObjResourceId, false}], % figure this out later
    Severity = case {NrAlarm#nrAlarm.severity,
		     NrAlarmType#nrAlarmType.isStateful} of
		   {<<"critical">>, true} -> eriAlarmCritical;
		   {<<"major">>, true} -> eriAlarmMajor;
		   {<<"minor">>, true} -> eriAlarmMinor;
		   {<<"warning">>, true} -> eriAlarmWarn;
		   {<<"critical">>, false} -> eriAlarmCriticalAlert;
		   {<<"major">>, false} -> eriAlarmMajorAlert;
		   {<<"minor">>, false} -> eriAlarmMinorAlert;
		   {<<"warning">>, false} -> eriAlarmWarnAlert
	       end,

    error_logger:info_report([{notification, Severity}|AlarmData]);
execute_snmp_action({cease, NrAlarm, NrAlarmType}) ->
    SequenceNo = 12345,
    AT = get_at(NrAlarm, NrAlarmType),
    AlarmData = 
	[{eriAlarmActiveManagedObject, element(3, NrAlarm#nrAlarm.key)},
	 {eriAlarmActiveMajorType, NrAlarmType#nrAlarmType.majorType},
	 {eriAlarmActiveMinorType, NrAlarmType#nrAlarmType.minorType},
	 {eriAlarmActiveSpecificProblem, NrAlarmType#nrAlarmType.specificProblem},
	 {eriAlarmActiveLastSequenceNo, SequenceNo},
	 {eriAlarmActiveEventType, NrAlarmType#nrAlarmType.eventType},
	 {eriAlarmActiveEventTime, snmp:date_and_time()},
	 {eriAlarmActiveProbableCause, NrAlarmType#nrAlarmType.probableCause},
	 {eriAlarmNObjAdditionalText, AT},
	 {eriAlarmNObjMoreAdditionalText, false}, % Figure thus out later
	 {eriAlarmNObjResourceId, false}], % figure this out later
    Severity = eriAlarmCleared,

    error_logger:info_report([{notification, Severity}|AlarmData]).
    
get_at(NrAlarm, NrAlarmType) ->
    case size(NrAlarm#nrAlarm.text) of
	0 ->
	    NrAlarmType#nrAlarmType.additionalText;
	_ ->
	    NrAlarm#nrAlarm.text
    end.

handle_detach(Registrar) ->	  
    WP1 = mnesia:table_info(nrAlarm, wild_pattern),
    Pattern1 = WP1#nrAlarm{key = {Registrar, '_', '_'}},
    WP2 = mnesia:table_info(nrAlarmType, wild_pattern),
    Pattern2 = WP2#nrAlarmType{key = {Registrar, '_'}},
    Fun = fun() ->
		  NrAlarms = mnesia:match_object(Pattern1),
		  NrAlarmTypes = mnesia:match_object(Pattern2),
		  SnmpActions = 
		      case {NrAlarms, NrAlarmTypes} of
			  {[], []} ->
			      mnesia:abort(gone);
			  _ -> 
			      [begin
				   Key = {element(1, NrAlarm#nrAlarm.key),
					  element(2, NrAlarm#nrAlarm.key)},
				   NrAlarmType = mnesia:read(nrAlarmType, Key),
				   {cease, NrAlarm, NrAlarmType}
			       end||NrAlarm<-NrAlarms]
		      end,
		  [mnesia:delete_object(X)||X<-NrAlarms++NrAlarmTypes],
		  {ok, SnmpActions, 
		   [X#nrAlarmType.key||X<-NrAlarmTypes]}
	  end,
    case mnesia:transaction(Fun) of
	{atomic, {ok, SnmpActions, NrAlarmTypeKeys}} ->
	    execute_snmp(SnmpActions),
	    com_unregister_alarm(NrAlarmTypeKeys);
	{aborted, gone} ->
	    Msg = <<" is not known">>,
	    {error, {gone, <<Registrar/binary, Msg/binary>>}};
	{aborted, Reason} ->
	    error_logger:error_report([{?MODULE, handle_detach, 
					[Registrar]},
				       {aborted, Reason}]),
	    {error, {internal_server_error, <<"Software error">>}}
    end.


%%% ----------------------------------------------------------------------
%%% LEGACY SECTION FOR INTEROP WITH COM/RCS
%%%
%%% These functions only execute if the nrComAlarm module is loaded
%%% In those cases, interactions with COM/COMSA occurs within that module
%%% ----------------------------------------------------------------------


com_register_alarm(Registrar, FmAlarmType) ->
    case erlang:function_exported(nrComAlarm, register_alarm, 2) of
	true ->
	    nrComAlarm:register_alarm(Registrar, FmAlarmType);
	false ->
	    ok
    end.

com_raise_and_cease(SnmpActions) ->
    case erlang:function_exported(nrComAlarm, raise_and_cease, 1) of
	true ->
	    nrComAlarm:raise_and_cease(SnmpActions);
	false ->
	    ok
    end.
		
com_unregister_alarm(NrAlarmTypeKeys) ->
    case erlang:function_exported(nrComAlarm, com_unregister_alarm, 1) of
	true ->
	    nrComAlarm:com_unregister_alarm(NrAlarmTypeKeys);
	false ->
	    ok
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

info_msg(Format) ->
    info_msg(Format, []).

info_msg(Format, Args) ->
    error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

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

run_attach() ->
    JSON = generate_json([{"{",[]},
			  {"~p:~p,~n",["registrar","johan"]},
			  {"~p:[",["fmAlarmTypes"]},
			  {"{~p:~p,~n",["fmId","myalarm"]},
			  {"~p:~p,~n",["majorType",193]},
			  {"~p:~p,~n",["minorType",123456]},
			  {"~p:~p,~n",["moClasses","SystemFunctions"]},
			  {"~p:~p,~n",["specificProblem", "MyAlarm"]},
			  {"~p:~p,~n",["eventType", "communicationsAlarm"]},
			  {"~p:~p,~n",["probableCause", 193]},
			  {"~p:~p,~n",["isStateful",true]},
			  {"~p:~p,~n",["additionalText","Fm service test"]},
			  {"~p:~p}",["defaultSeverity","warning"]},
			  {"]}",[]}]),
    
    io:format("JSON:~n~s~n-----------------------------------------~n",[JSON]),
    Map = jsone:decode(list_to_binary(JSON)),
    Registrar = maps:get(?registrar, Map),
    FmAlarmTypes = maps:get(?fmAlarmTypes, Map),
    attach(Registrar, FmAlarmTypes).
    
run_raise() ->
    JSON = generate_json([{"[{~p:~p,~n",["action","raise"]},
			  {"~p:~p,~n",["fmId","myalarm"]},
			  {"~p:~p,~n",["severity", "major"]},
			  {"~p:~p,~n",["sender", "ManagedElement=1"]},
			  {"~p:~p,~n",["text", "Alarm test"]},
			  {"~p:[]}]",["info"]}]),
			  %% {"~p:[{~p:~p}]}]",["info", "key1", "info1"]}]),
    io:format("JSON:~n~s~n-----------------------------------------~n",[JSON]),
    Alarms = jsone:decode(list_to_binary(JSON)),
    Registrar = <<"johan">>,
    raise_and_cease(Registrar, Alarms).

run_cease() ->
    JSON = generate_json([{"[{~p:~p,~n",["action","case"]},
			  {"~p:~p,~n",["fmId","myalarm"]},
			  {"~p:~p",["source", "ManagedElement=1"]},
			  {"}]",["info"]}]),
    io:format("JSON:~n~s~n-----------------------------------------~n",[JSON]),
    Alarms = jsone:decode(list_to_binary(JSON)),
    Registrar = <<"johan">>,
    raise_and_cease(Registrar, Alarms).
    

    

run_detach() ->
    detach(<<"johan">>).

generate_json(Data) ->
    lists:flatten([io_lib:format(F,A)||{F,A}<-Data]).

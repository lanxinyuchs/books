%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logStreamServer.erl %
%%% Author:	etxasta
%%% Description: Syslog originator implementation used for the
%%%              stream option in the LogM fragment,
%%%              mainly intended for the security event log.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(logStreamServer).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/2').
-date('2015-09-24').
-author('etxasta').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R1A/1      2012-05-22 etxasta     Created
%%% R2A/4      2013-06-10 etxkols     Commented out "Event received"
%%% R2A/5      2013-06-11 etxasta     Bug fix and removed io:format
%%% R2A/14     2013-12-12 uabesvi     changed LogM to RcsLogM
%%% R2A/15     2014-09-15 etxlg       DSCP set on udp packets, TR HS73150
%%% R3A/1      2014-10-09 etxpejn     Added license_state/1 and #state.license_rtsel
%%% R3A/2      2014-10-28 etxpejn     Added license handling for the security log
%%% R3A/5      2014-11-05 etxpejn     Added AuditTrail log for license controll RTSEL
%%% R3A/19     2015-05-27 etxpejn     Corr HT78791 in new_index
%%% R3A/20     2015-06-08 etxpejn     Corr HT81770 in new_index
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([event/5]).
-export([event/6]).
-export([event/7]).
-export([init_tables/1]).
-export([get_name/1, find_streams/0]).
-export([license_state/1]).
-export([get_severity/1]).
-export([update_state/1]).
-export([get_test/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-export([check_uri/1]).

-include("RcsLogM.hrl").

-define(REMOTE_UDP_PORT,  514). % Standard syslog port for UDP
-define(REMOTE_TLS_PORT, 6514). % Standard syslog port for TLS

-record(state, {state,
		license_rtsel = inoperable}).



%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start() ->
    gen_server:start_link({local, logStreamServer}, ?MODULE, [], []).

event(Name, _User, Severity, TimeStamp, Msg) ->
    event(Name, 4, Severity, TimeStamp, Msg).

event(Name, _SourceIp, _User, Severity, TimeStamp, Msg) ->
    event(Name, 4, Severity, TimeStamp, Msg).

event(Name, _SourceIp, _User, Facility, Severity, TimeStamp, Msg) ->
    gen_server:cast(
        logStreamServer,
        {event, Name, Facility, Severity, TimeStamp, Msg}).

init_tables(_) ->
       ok.

%% Temp debug functions
get_name(Name) ->
    gen_server:call(logStreamServer, {getName, Name}).

find_streams() ->
    gen_server:cast(logStreamServer, find_streams).

license_state(LicenseState) ->
gen_server:cast(logStreamServer, {license_state, LicenseState}).

get_severity(Log) ->
    gen_server:call(logStreamServer, {get_severity, Log}).

update_state(ConnState) ->
    gen_server:cast(logStreamServer, {update_state, ConnState}).

get_test() ->
    gen_server:call(logStreamServer, get_test).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(Args) ->
    put(send_processes, []),
    gen_server:cast(self(), {initialize, Args}),
    {ok, initializing}.

handle_call({getName, Name}, _, State) ->
    {reply, {Name, get(Name)}, State};
handle_call({get_severity, Log}, _, State) ->
    {reply, get({severity, Log}), State};
handle_call(get_test, _, State) ->
    io:format("~nPrint all process variables:~n~p~n", [get()]),
    {reply, get(send_processes), State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({initialize, _Args}, initializing) ->
    %% Scan logPushTransfer for stream logs
    find_all_streams(),
    %% Set the severity filter
    set_severity(),
    %% Set up mnesia subscriptions
    mnesia:subscribe({table, log, detailed}),
    mnesia:subscribe({table, logPushTransfer, detailed}),
    {noreply, #state{state = up}};
handle_cast(find_streams, State) ->
    %% Scan logPushTransfer for stream logs
    find_all_streams(),
    {noreply, State};
handle_cast({event, Name, Facility, Severity, TimeStamp, Msg},
	    #state{license_rtsel = LicenseState} = State) ->
    CS = check_severity(Name, Severity),
    case {LicenseState, CS} of
        {operable, send} ->
    	    stream(Name, Facility, Severity, TimeStamp, Msg);
        _ ->
	    %% No streaming is allowed when license is inoperable,
            %% or thrown by severity filter
	    do_nada
    end,
    {noreply, State};
handle_cast({license_state, LicenseState}, State) ->
    LogPushTransferKeys = mnesia:dirty_all_keys(logPushTransfer),
    %% Change operationalState and availabilityStatus if needed.
    SecKeys   = find_push_transfer_key(LogPushTransferKeys, "SecurityLog", []),
    AuditKeys = find_push_transfer_key(LogPushTransferKeys, "AuditTrailLog", []),
    mnesia:transaction(
        fun() ->
                change_op_state(SecKeys, [], LicenseState),
                change_op_state(AuditKeys, [], LicenseState)
        end),
    {noreply, State#state{license_rtsel = LicenseState}};
handle_cast({update_state, ConnState}, State) ->
    handle_update_state(ConnState),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, Event}, #state{license_rtsel = LicenseState} = State) ->
    handle_mnesia_table_event(Event, LicenseState),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% # stream(Name, SourceIp, User, Facility, Severity, TimeStamp, Msg)
%%% Input:     Name
%%%            SourceIp
%%%            User
%%%            Facility
%%%            Severity
%%%            TimeStamp
%%%            Msg
%%% Output:    ok
%%% Exceptions:
%%% Description: Handle a log event incase of the stream option.
%%% ----------------------------------------------------------
stream(Name, Facility, Severity, TimeStamp, Msg) ->
    case get(Name) of
        undefined ->
            ok;
        StreamList ->
            Data =
            format_stream(Facility, Severity, TimeStamp, Msg),
            send_stream(Data, StreamList, Name)
    end.

%%% ----------------------------------------------------------
%%% # send_stream(Data, StreamList, Name) ->
%%% Input:     Data - encoded message to send
%%%            StreamList
%%%            Name
%%% Output:    
%%% Exceptions:
%%% Description: Send a stream to a syslog server
%%% ----------------------------------------------------------
send_stream(_, [], _) ->
    ok;
send_stream(Data, [{_,_,Index}|T], Name) ->
    logStreamSendServer:send(Index, Data),
    send_stream(Data, T, Name);
send_stream(Data, [_|T], Name) ->
    send_stream(Data, T, Name).


%%% ----------------------------------------------------------
%%% # format_stream(Facility, Severity, TimeStamp, Msg)
%%% Input:     Facility
%%%            Severity
%%%            TimeStamp
%%%            Msg
%%% Output:    Log string() in syslog format
%%% Exceptions:
%%% Description: Format a syslog stream
%%% ----------------------------------------------------------
format_stream(Facility, Severity, TimeStamp, Msg) ->
    %% Priority and syslog version 1
    Priority = "<" ++ create_priority(Facility, Severity) ++ ">1",
    
    %% Timestamp header using UTC
    TimeS = create_time(TimeStamp),

    %% Host header (FQDN)
    Host = host_ip(),
    
    %% App-name header
    AppName = app_name(),

    %% Proc ID header
    ProcId = "-",
    
    %% Message ID header
    MsgId = "-",

    %% Structured-Data Header
    SD = "-",

    %% Message Header, message in UTF-8 format
    %% Msg

    %% Syslog stream
    string:join([Priority, TimeS, Host, AppName, ProcId, MsgId, SD, Msg], " ").


app_name() ->
    case lists:keyfind(networkManagedElementId,
            1, comsaI:get_managed_element_data()) of
        {networkManagedElementId, undefined} ->
            "1";
        {networkManagedElementId, ME} ->
            ME
    end.

host_ip() ->
    case ootI:get_oap_ip_addr() of
        [] ->
            "-";
        IpString ->
            % Assume this is on the form "IP1.IP2.IP3.IP4",
            % i.e. not to be run through resolver
            IpString
    end.

%%% ----------------------------------------------------------
%%% # create_priority(User, Severity)
%%% Input:     Facility
%%%            Severity
%%% Output:    Priority - string()
%%% Exceptions:
%%% Description: Format and create a syslog priority value
%%% ----------------------------------------------------------
create_priority(Facility, Severity) ->
    Sev =
    case Severity of
        Severity when is_list(Severity) ->
            list_to_integer(Severity);
        emergency -> ?LogSeverity_EMERGENCY;
        alert     -> ?LogSeverity_ALERT;
        critical  -> ?LogSeverity_CRITICAL;
        error     -> ?LogSeverity_ERROR;
        warning   -> ?LogSeverity_WARNING;
        notice    -> ?LogSeverity_NOTICE;
        info      -> ?LogSeverity_INFO
    end,
    integer_to_list(Facility*8 + Sev).
 
%%% ----------------------------------------------------------
%%% # create_time(TimeStamp)
%%% Input:     Time
%%% Output:    string()
%%% Exceptions:
%%% Description: Format and create a syslog UTC time value
%%% ----------------------------------------------------------
create_time(TimeStamp) ->
    case TimeStamp of
        {{Year, Month, Day}, {Hour, Minute, Second}} ->
            integer_to_list(Year) ++ "-" ++ format_number(Month) ++ "-" ++
            format_number(Day) ++ "T" ++ format_number(Hour) ++ ":" ++
            format_number(Minute) ++ ":" ++ format_number(Second) ++ "Z";
        TimeStamp ->
            {{Year, Month, Day}, {Hour, Minute, Second}} =
            calendar:now_to_datetime(TimeStamp),
            format_number(Year) ++ "-" ++ format_number(Month) ++ "-" ++
            format_number(Day) ++ "T" ++ format_number(Hour) ++ ":" ++
            format_number(Minute) ++ ":" ++ format_number(Second) ++ "Z"
    end.

format_number(Number) when is_integer(Number) ->
    case Number of
        Number when Number < 10 ->
            "0" ++ integer_to_list(Number);
        _ ->
            integer_to_list(Number)
    end;
format_number(Number) when is_float(Number) ->
    case Number of
        Number when Number < 10 ->
            "0" ++ float_to_list(Number);
        _ ->
            float_to_list(Number)
    end.

%%% ----------------------------------------------------------
%%% # find_push_transfer_key(KeyList, Table, Keys)
%%% Input:     list(), string(), list()
%%% Output:    list()
%%% Exceptions:
%%% Description: 
%%% ----------------------------------------------------------
find_push_transfer_key([], _Table, Keys) ->
    Keys;
find_push_transfer_key([Key|T], Table, Keys) ->
    case lists:nth(4, tuple_to_list(Key)) of
	Table ->
            find_push_transfer_key(T, Table, lists:append(Keys, [Key]));
	_Else ->
	    find_push_transfer_key(T, Table, Keys)
    end.
	
%%% ----------------------------------------------------------
%%% #           handle_mnesia_table_event(MnesiaEvent)
%%% Input: MnesiaEvent - according to mnesia doc
%%% Output:
%%% Exceptions:
%%% Description: Handle any updates to the models owned by COMSA, which have
%%%              side effects
%%% ----------------------------------------------------------
handle_mnesia_table_event({write, logPushTransfer, New, [], _ActivityId},
    LicenseState) ->
    case New#logPushTransfer.transferType of
        ?TransferType_STREAM ->
	    addUri(New#logPushTransfer.logPushTransferId,
		   New#logPushTransfer.uri),
	    mnesia:transaction(
	      fun() -> change_op_state(New#logPushTransfer.logPushTransferId,
				       New, LicenseState) end);
	_ ->
	    mnesia:transaction(
		  fun() -> change_op_state(New#logPushTransfer.logPushTransferId,
					   New, operable) end)
    end;
handle_mnesia_table_event({write, logPushTransfer, New, [Old], _ActivityId},
    LicenseState) ->
    case {New#logPushTransfer.transferType,
            Old#logPushTransfer.transferType} of
        {?TransferType_STREAM, ?TransferType_STREAM} ->
            case {New#logPushTransfer.uri, Old#logPushTransfer.uri} of
                {Same, Same} ->
                    ok;
                _ ->
                    %% Stream uri changed
		    removeUri(New#logPushTransfer.logPushTransferId,
			      Old#logPushTransfer.uri),
                    addUri(New#logPushTransfer.logPushTransferId,
                        New#logPushTransfer.uri)
            end;
        {?TransferType_STREAM, ?TransferType_BULK} ->
            %% Changed to stream, start streaming
            addUri(New#logPushTransfer.logPushTransferId,
                New#logPushTransfer.uri),
	    mnesia:transaction(
		  fun() -> change_op_state(New#logPushTransfer.logPushTransferId,
					   New, LicenseState) end);
        {?TransferType_BULK, ?TransferType_STREAM} ->
            %% Changed to bulk, stop streaming
	    removeUri(New#logPushTransfer.logPushTransferId,
		      Old#logPushTransfer.uri),
	    mnesia:transaction(
	      fun() -> change_op_state(New#logPushTransfer.logPushTransferId,
				       New, operable) end);
        _ ->
            ok
    end;
handle_mnesia_table_event({delete, logPushTransfer, _, [Old], _ActivityId},
    _LicenseState) ->
    removeUri(Old#logPushTransfer.logPushTransferId, Old#logPushTransfer.uri);
handle_mnesia_table_event({write, log, New, [Old], _ActivityId}, _) ->
    case {New#log.severityFilter, Old#log.severityFilter} of
        {Same, Same} ->
            ok;
        {List,_} ->
            {_,_,_, Log} = New#log.logId,
            put({severity, Log}, List),
            ok
    end;
handle_mnesia_table_event(_,_) ->
    ok.


addUri(PushTransferId, Uri) ->
    case check_uri(Uri) of
        {Type, Ip, Port, CN} ->
            Stream  = {Type, Ip, Port, CN},
            LogName = element(4, PushTransferId),
            Id      = element(5, PushTransferId),
	    case get(LogName) of
                undefined -> % New push transfer id
                    Index = new_index(),
                    put(LogName, [{Stream, [Id], Index}]),
                    new_process(Index, Type, Ip, Port, CN);
                StreamList -> 
                    case lists:keyfind(Stream, 1, StreamList) of
                        {Stream, IdList, Index} ->
                            case lists:member(Id, IdList) of
                                true ->
                                    ok;
                                false ->
                                    NewStreamList =
                                    lists:keyreplace(Stream, 1, StreamList,
                                        {Stream, IdList ++ [Id], Index}),
                                    put(LogName, NewStreamList)
                            end;
                        false -> % New push transfer id
                            Index = new_index(),
                            put(LogName,
                                StreamList ++ [{Stream, [Id], Index}]),
                            new_process(Index, Type, Ip, Port, CN)
                    end
            end;
        _ ->
            ok
    end.

new_process(Index, Type, Ip, Port, CN) ->
    List = get(send_processes),
    Name = logStreamSendServer:make_process_name(Index),
    case lists:keyfind(Name, 1, List) of
        false ->
	    ChildSpec =
		logStreamSendServer:make_child(Index, Type, Ip, Port, CN),
            put(send_processes, lists:append(List, [{Name, 1}])),
            case supervisor:start_child(logSuper, ChildSpec) of
                {ok, _} ->
                    ok;
                {ok, _, _} ->
                    ok;
                {error, Reason} ->
                    error_msg("Start new process failed, ~w", [Reason])
            end;
        {Name, Number} ->
            NewList = lists:keyreplace(Name, 1, List, {Name, Number + 1}),
            put(send_processes, NewList)
    end.


removeUri(PushTransferId, Uri) ->
    case check_uri(Uri) of
        {Type, Ip, Port, CN} ->
            Stream  = {Type, Ip, Port, CN},
            LogName = element(4, PushTransferId),
            Id      = element(5, PushTransferId),
            case get(LogName) of
                undefined ->
                    ok;
                [{Stream, IdList, Index}] ->
                    case lists:delete(Id, IdList) of
                        [] ->
                            erase(LogName),
                            remove_process(Index);
                        NewIdList ->
                            put(LogName, [{Stream, NewIdList, Index}])
                    end;
                StreamList ->
                    case lists:keyfind(Stream, 1, StreamList) of
                        {Stream, IdList, Index} ->
                            case lists:delete(Id, IdList) of
                                [] ->
                                    NewStreamList =
                                    lists:keydelete(Stream, 1, StreamList),
                                    put(LogName, NewStreamList),
                                    remove_process(Index);
                                NewIdList ->
                                    NewStreamList =
                                    lists:keyreplace(Stream, 1, StreamList,
                                        {Stream, NewIdList, Index}),
                                   put(LogName, NewStreamList)
                            end;
                        false ->
                            ok
                    end
            end;
        _ ->
            ok
    end.

remove_process(Index) ->
    List = get(send_processes),
    Name = logStreamSendServer:make_process_name(Index),
    case lists:keyfind(Name, 1, List) of
        false ->
            ok;
        {Name, 1} ->
	    ok = supervisor:terminate_child(logSuper, Name),
            ok = supervisor:delete_child(logSuper, Name),
            NewList = lists:keydelete(Name, 1, List),
            put(send_processes, NewList);
        {Name, Number} ->
            NewList = lists:keyreplace(Name, 1, List, {Name, Number - 1}),
            put(send_processes, NewList)
    end,
    return_index(Index).


new_index() ->
    case get(stream_indexes) of
        undefined ->
            put(stream_indexes, [1]),
            "1";
	[] ->
	    %% HT78791
	    put(stream_indexes, [1]),
            "1";
        List ->
	    %% HT81770
	    Max = lists:max(List),
	    Index = do_new_index(true, List, Max + 1),
	    put(stream_indexes, lists:append(List, [Index])),
            integer_to_list(Index)
    end.

do_new_index(false, _, Index) ->
    Index; 
do_new_index(true, List, Index) ->
    do_new_index(lists:member(Index, List), List, Index).

return_index(Index) ->
    case get(stream_indexes) of
        undefined ->
            ok;
        List ->
            put(stream_indexes,
                lists:delete(list_to_integer(Index), List))
    end.


check_uri(Uri) ->
    case http_uri:parse(Uri, [{scheme_defaults,[{syslog, 1}]}]) of
        {ok, {syslog, _,Ip, 1,_,_}} -> %% No port, use 514 default
            {syslog, Ip, ?REMOTE_UDP_PORT, undefined};
        {ok, {syslog, _,Ip,Port,_,_}} ->
            {syslog, Ip, Port, undefined};
        _ ->
            case http_uri:parse(Uri, [{scheme_defaults,[{syslogs, 1}]}]) of
                {ok, {syslogs, _,Ip, 1,_,Opt}} ->
                    %% No port, use 6514 default
                    CN = fetch_cn(Opt),
                    {syslogs, Ip, ?REMOTE_TLS_PORT, CN};
                {ok, {syslogs, _,Ip,Port,_,Opt}} ->
                    CN = fetch_cn(Opt),
                    {syslogs, Ip, Port, CN};
                _ ->
                    error
            end
    end.


fetch_cn(Opt) ->
    do_fetch_cn(string:tokens(Opt, "?")).

do_fetch_cn([]) ->
    {no_cn, undefined};
do_fetch_cn([H|T]) ->
    case string:tokens(H, "=") of
        ["CN", CN] ->
            CN;
        _ ->
            do_fetch_cn(T)
    end.



change_op_state([], _, _) ->
    ok;
change_op_state([Key|T], [], LicenseState) ->
    [Obj] = mnesia:read(logPushTransfer, Key),
    change_op_state(Key, Obj, LicenseState),
    change_op_state(T, [], LicenseState);
change_op_state(Id, [], LicenseState) ->
    [Obj] = mnesia:read(logPushTransfer, Id),
    change_op_state(Id, Obj, LicenseState);
change_op_state({_,_,_,Log,_}, LogPushTransfer, operable) when Log == "SecurityLog" ;
Log == "AuditTrailLog" ->
    ok = mnesia:write(LogPushTransfer#logPushTransfer
		      {operationalState = ?OperState_ENABLED,
		       availabilityStatus = []});
change_op_state({_,_,_,Log,_}, LogPushTransfer, inoperable) when Log == "SecurityLog" ;
Log == "AuditTrailLog"->
    ok = mnesia:write(LogPushTransfer#logPushTransfer
		      {operationalState = ?OperState_DISABLED,
		       availabilityStatus = [?AvailStatus_DEPENDENCY]});
change_op_state(_Id, LogPushTransfer, error) ->
    ok = mnesia:write(LogPushTransfer#logPushTransfer
		      {operationalState = ?OperState_DISABLED,
		       availabilityStatus = [?AvailStatus_DEPENDENCY]});
change_op_state(_Id, LogPushTransfer, ok) ->
    %% Set the operationalState to ENABLED 
    ok = mnesia:write(LogPushTransfer#logPushTransfer
		      {operationalState = ?OperState_ENABLED,
		       availabilityStatus = []});
change_op_state(_Id, LogPushTransfer, _) ->
    %% This is not the SecurityLog or the AuditTrailLog so the liceseState is
    %% not valid, set the operationalState to ENABLED
    ok = mnesia:write(LogPushTransfer#logPushTransfer
		      {operationalState = ?OperState_ENABLED}).



handle_update_state({Index, Type, Status}) ->
    info_msg("Index: ~p, Type: ~p, Status: ~p", [Index, Type, Status]),
    update_status("SecurityLog", Index, Status),
    update_status("AuditTrailLog", Index, Status).
    
update_status(Table, Index, Status) ->
    case get(Table) of
        undefined ->
            ok;
        StreamList ->
            case lists:keyfind(Index, 3, StreamList) of
                {_Stream, IdList, Index} ->
                    lists:foreach(
                        fun(Id) ->
                                Key = {"1", "1", "1", Table, Id},
				mnesia:transaction(fun() ->
							   change_op_state(Key, [], Status)
                                    end)
                        end, IdList);
                _ ->
                    ok
            end
    end.


%%% ----------------------------------------------------------
%%% #           find_all_streams()
%%% Input: -
%%% Output:
%%% Exceptions:
%%% Description: Find and search for logs with stream option
%%% ----------------------------------------------------------
find_all_streams() ->
    find_all_streams(mnesia:dirty_all_keys(logPushTransfer)).

find_all_streams([]) ->
    ok;
find_all_streams([Id|T]) ->
    [Obj] = mnesia:dirty_read({logPushTransfer, Id}),
    case Obj#logPushTransfer.transferType of
	?TransferType_STREAM -> 
            addUri(Obj#logPushTransfer.logPushTransferId,
                Obj#logPushTransfer.uri);
        ?TransferType_BULK ->
            ok
    end,
    find_all_streams(T).

%%% ----------------------------------------------------------
%%% #           set_severity()
%%% Input: -
%%% Output:
%%% Exceptions:
%%% Description: Set the severity process data at startup
%%% ----------------------------------------------------------
set_severity() ->
    [S] = mnesia:dirty_read(log, {"1","1","1","SecurityLog"}),
    [AT] = mnesia:dirty_read(log, {"1","1","1","AuditTrailLog"}),
    case S#log.severityFilter of
        undefined ->
            put({severity, "SecurityLog"}, []);
        _ ->
            put({severity, "SecurityLog"}, S#log.severityFilter)
    end,
    case AT#log.severityFilter of
        undefined ->
            put({severity, "AuditTrailLog"}, []);
        _ ->
            put({severity, "AuditTrailLog"}, AT#log.severityFilter)
    end.

%%% ----------------------------------------------------------
%%% #           check_severity()
%%% Input: -
%%% Output:
%%% Exceptions:
%%% Description: Check if it should be filtered out by
%%%              the severity filter
%%% ----------------------------------------------------------
check_severity(Name, Severity) ->
    case get({severity, Name}) of
        undefined ->
            drop; %% Someone is to early
        List ->
            case lists:member(Severity, List) of
                true ->
                    drop;
                false ->
                    send
            end
    end.

%%% ----------------------------------------------------------
%%% INFO, WARNING and ERROR MESSAGE HANDLING
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



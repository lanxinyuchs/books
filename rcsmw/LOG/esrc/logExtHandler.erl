%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logExtHandler.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(logExtHandler).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R4A/1').
-date('2015-09-01').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
%%% R1A/1      2012-02-21 etxjotj     Created
%%% R2A/2      2013-12-12 uabesvi     changed LogM to RcsLogM
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-record(state, {socket}).
-include("RcsLogM.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start(Socket) ->
    Id = {?MODULE, Socket},
    GenArgs = [?MODULE, [Socket], []], % Module, Args, Options
    StartFunc = {gen_server, start , GenArgs},
    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [?MODULE],
    ChildSpec = {Id, StartFunc, Restart, Shutdown, Type, Modules},
    supervisor:start_child(logSuper, ChildSpec).

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init([Socket]) ->
    inet:setopts(Socket, [{active, once}]),
    {ok, #state{socket=Socket}}.
    
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    case Msg of
	{tcp, Socket, Data} ->
	    <<_:32,Bencode/binary>> = Data,
	    LogMsg = bencode:decode(binary_to_list(Bencode)),
	    case handle_log_msg(LogMsg) of
		ok -> ok;
		{ok, Ret} -> 
		    RetMsg = list_to_binary(bencode:encode(Ret)),
		    Size = size(RetMsg)+4,
		    SendMsg = <<3,0,Size:16, RetMsg/binary>>,
		    gen_tcp:send(Socket, SendMsg)
	    end,
	    {noreply, Socket};
	{tcp_closed, Socket} ->
	    info_msg("{tcp_closed, ~p]~n",[Socket]),
	    {stop, normal, State};
	{tcp_error, Socket, Reason} ->
	    error_msg("~p~n",[{tcp_error, Socket, Reason}]),
	    {stop, normal, State}
    end.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.


%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

handle_log_msg([<<"write_log">>, Dict]) ->
    [{<<"logName">>, NameB},
     {<<"logSvcUsrName">>, UserB},
     {<<"logSeverity">>, SeverityI},
     {<<"logTimeStamp">>, TS},
     {<<"logAck">>, LA},
     {<<"logBuffer">>, LogMsgB}] = Dict,
    Name = binary_to_list(NameB),
    User = binary_to_list(UserB),
    LogMsg = binary_to_list(LogMsgB),
    Timestamp = 
	case TS of
	    <<"unknown">> -> undefined;
	    _ -> TS
	end,
    LogAck = 
	case LA of
	    <<"NULL">> -> undefined;
	    _ -> binary_to_list(LA)
	end,

    Severity = 
	case SeverityI of
	    ?LogSeverity_EMERGENCY -> emergency;
	    ?LogSeverity_ALERT -> alert;
	    ?LogSeverity_CRITICAL -> critical;
	    ?LogSeverity_ERROR -> error;
	    ?LogSeverity_WARNING -> warning;
	    ?LogSeverity_NOTICE -> notice;
	    ?LogSeverity_INFO -> info
	end,

    case {LogAck, Timestamp} of
	{undefined, undefined} ->
	    logI:awrite_log(Name, User, Severity, LogMsg);
	{_, undefined} ->
	    Result = logI:write_log(Name, User, Severity, LogMsg),
	    {ok, format_result(NameB, LA, Result)};
	{undefined, _} ->
	    logI:awrite_log(Name, User, Severity, LogMsg);
	_ ->
	    Result = logI:write_log(Name, User, Severity, Timestamp, LogMsg),
	    {ok, format_result(NameB, LA, Result)}
    end;
	

handle_log_msg(<<"get-version">>) ->
    Version = logI:get_version(),
    {ok, [<<"version">>, Version]}.


format_result(Name, Ack, Result) ->
    Dict = [{<<"logName">>, Name},
	    {<<"logAck">>, Ack},
	    {<<"logResult">>, list_to_binary(atom_to_list(Result))}],
    [<<"log_ack">>, Dict].
    

%% info_msg(Format) ->
%%     info_msg(Format, []).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certAlarm.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R5A/R6A/1
%%% 
%%% @doc == This module implements the key handling ==
-module(certAlarm).
-vsn('/main/R2A/R5A/R6A/1').
-behaviour(gen_server).
-date('2016-05-31').
-author('etxasta').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/1      2014-06-23 etxasta     Created
%%% R5A/1      2016-04-14 uabhgma     Added check before clear alarm
%%% R6A/1      2016-05-31 etxasta     Changed to a gen_server
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% Called from supervisor
-export([start/0]).

%% API
-export([send/5, clear/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the certSecStore server process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% #          send(Mo, AlarmType, Key, Level, Msg) ->
%%% Input: Mo        - atom(nc|tc)
%%%        AlarmType - atom(cert_not_available|cert_to_expire|
%%%                         cert_enroll_failed)
%%%        Key       - tuple(Nc or Tc key)
%%%        Level     - atom(warning|minor|major|critical)
%%%        Msg       - string()
%%% Output: - 
%%% Exceptions: 
%%% Description: Send alarm. Will only send alarm if not
%%%              already sent.
%%% ----------------------------------------------------------
send(Mo, AlarmType, Key, Level, Msg) ->
    gen_server:cast(?MODULE, {send, Mo, AlarmType, Key, Level, Msg}),
    ok.

%%% ----------------------------------------------------------
%%% #          clear(Mo, AlarmType, Key) ->
%%% Input: Mo        - atom(nc|tc)
%%%        AlarmType - atom(cert_not_available|cert_to_expire|
%%%                         cert_enroll_failed)
%%%        Key       - tuple(Nc or Tc key)
%%% Output: - 
%%% Exceptions: 
%%% Description: Clear alarm. Will only clear alarm if sent.
%%% ----------------------------------------------------------
clear(Mo, AlarmType, Key) ->
    gen_server:cast(?MODULE, {clear, Mo, AlarmType, Key}),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(_Args) ->
    {ok, up}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Mo, AlarmType, Key, Level, Msg}, State) ->
    handle_send(Mo, AlarmType, Key, Level, Msg),
    {noreply, State};
handle_cast({clear, Mo, AlarmType, Key}, State) ->
    handle_clear(Mo, AlarmType, Key),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
handle_send(Mo, AlarmType, Key, Level, Msg) ->
    send_if_exist(Mo, alarm_type(AlarmType), Key, Level, Msg).

handle_clear(Mo, AlarmType, Key) ->
    clear_if_exist(Mo, alarm_type(AlarmType), Key).

alarm_type(cert_not_available) ->
    'CertMCertificateNotAvailable';
alarm_type(cert_to_expire) ->
    'CertMCertificateToExpire';
alarm_type(cert_enroll_failed) ->
    'CertMAutomaticEnrollmentFailed'.

send_if_exist(Mo, AlarmType, Key, Level, Msg) ->
    case get({Mo, AlarmType, Key}) of
        {Level, Msg} ->
            ok;
        _ ->
            put({Mo, AlarmType, Key}, {Level, Msg}),
            comsaI:send_alarm(AlarmType, Level, mk_dn(Mo, Key), Msg)
    end.

clear_if_exist(Mo, AlarmType, Key) ->
    case get({Mo, AlarmType, Key}) of
        {_Level,_Msg} ->
            put({Mo, AlarmType, Key}, undefined),
            comsaI:clear_alarm(AlarmType, mk_dn(Mo, Key));
        _ ->
            ok
    end.

mk_dn(nc, {_,_,_,_,Index}) ->
    [list_to_binary("ManagedElement=1"),
     list_to_binary("SystemFunctions=1"),
     list_to_binary("SecM=1"),
     list_to_binary("CertM=1"),
     list_to_binary("NodeCredential=" ++ Index)];
mk_dn(tc, {_,_,_,_,Index}) ->
    [list_to_binary("ManagedElement=1"),
     list_to_binary("SystemFunctions=1"),
     list_to_binary("SecM=1"),
     list_to_binary("CertM=1"),
     list_to_binary("TrustedCertificate=" ++ Index)].
     


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


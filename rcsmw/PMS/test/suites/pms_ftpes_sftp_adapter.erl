%%% %CCaseFile:	pms_ftpes_sftp_adapter.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/1

%%% @doc == Test server which acts as adapter towards sftp and ftpes client ==
%%% This adapter is used in test cases which test sftp and ftpes server
%%% It provides the same output (given the same input) for both ftpes and sftp
%%% @end

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

%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R9A/1      2017-01-23 ekurnik     Created
%%%

-module(pms_ftpes_sftp_adapter).
-behaviour(gen_server).

%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start/0, start/1, stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([set_rct_client/1, 
         start_channel/0, 
         stop_channel/0,
         command/2]).

-record(state, {rct_client}).

%% Start adapter process
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%% Stop adapter process
stop() ->
    gen_server:call(?MODULE, stop).

%% Set rct_client ct_hook
%% Currently supported: rct_ftpes_client and rct_sftp_client
set_rct_client(RctClient) ->
    gen_server:call(?MODULE, {set_rct_client, RctClient}).

%% Start a connection towards server
start_channel() ->
    gen_server:call(?MODULE, start_channel).

%% Stop a connection
stop_channel() ->
    gen_server:call(?MODULE, stop_channel).

%% Triggers a command towards rct_client (list_dir, read_file, delete_file...)
command(Command, Args) ->
    gen_server:call(?MODULE, {command, Command, Args}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    init(undefined);

init(RctClient) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{rct_client = RctClient}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({set_rct_client, RctClient}, _From, State) ->
    {reply, ok, State#state{rct_client = RctClient}};

handle_call(_Request, _From, #state{rct_client = undefined} = State) ->
    {reply, {error, "rct_client is undefined"}, State};

handle_call(start_channel, _From, #state{rct_client = RctClient} = State) ->
    {reply, do_start_channel(RctClient), State};

handle_call(stop_channel, _From, #state{rct_client = RctClient} = State) ->
    {reply, do_stop_channel(RctClient), State};

handle_call({command, list_dir, Args}, _From, #state{rct_client = rct_ftpes_client} = State) ->
    Resp = 
    case erlang:apply(rct_ftpes_client, list_dir, Args) of
        {ok, FileListing} ->
            {ok, [filename:basename(string:strip(File)) || File <- string:tokens(FileListing, "\r\n")]};
        Error ->
            Error
    end,
    {reply, Resp, State};

handle_call({command, delete_file, Args}, _From, #state{rct_client = rct_ftpes_client} = State) ->
    Resp = 
    case erlang:apply(rct_ftpes_client, delete_file, Args) of
        {error, epath} ->
            {error, permission_denied}; %% error 550 access denied translates into epath in ftp client
        Other ->
            Other
    end,
    {reply, Resp, State};

handle_call({command, Command, Args}, _From, #state{rct_client = RctClient} = State) ->
    {reply, erlang:apply(RctClient, Command, Args), State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_start_channel(rct_sftp_client) ->
    rct_sftp_client:start_channel();
do_start_channel(rct_ftpes_client) ->
    rct_ftpes_client:open().

do_stop_channel(rct_sftp_client) ->
    rct_sftp_client:stop_channel();
do_stop_channel(rct_ftpes_client) ->
    rct_ftpes_client:close().



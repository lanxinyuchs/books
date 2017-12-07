%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysTftpd.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2015
%%% @version /main/R4A/R5A/1
%%% @doc == tftp handling ==
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(sysTftpd).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/1').
-date('2015-11-17').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% Rev     Date     Name      What
%%% -----   -------  --------  ------------------------
%%% R4A/1   150327   etxpeno   First try
%%% R4A/3   150526   etxlg     got rid of debug - too slow
%%% R4A/4   150825   etxarnu   New tftp root path
%%% R4A/6   150902   etxarnu   Bind tftpd to own dus cluster IP address
%%% R4A/7   150910   etxarnu   Moved start of TLS to activate
%%% ----------------------------------------------------------
%%% R5A/1   151117   etxpeno   do not use sysEnv:role/1
%%% ----------------------------------------------------------

%% API
-export([start_link/0]).
-export([activate/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
	{
	  pid
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    TftpRoot = filename:join([sysEnv:home_dir(),"tftproot"]),
    OwnIp = clhI:ip_address(),
    case os:cmd("ip address show |grep " ++ to_str(OwnIp)) of
	[] ->
	    ignore; %Own IP address not configured
	_ ->
	    Config = [%%%%%{debug, all},
		      {callback,{"",tftp_file,[{root_dir, TftpRoot}]}},
		      {reject, write},
		      {udp,[{ip,OwnIp}]}],
	    case inets:start(tftpd, Config) of
		{ok, Pid} ->
		    sysInitI:info_msg("~p: Starting tftp server~n", [?MODULE]),
		    {ok, #state{pid = Pid}};
		Reason ->
		    sysInitI:info_msg("~p: Not starting tftp server~n"
				      " Reason= ~p~n", [?MODULE, Reason]),
		    ignore
	    end
    end.

to_str({A,B,C,D}) ->
    io_lib:format("~p.~p.~p.~p",[A,B,C,D]).
%%--------------------------------------------------------------------
activate() ->
    CoreState = clhI:core_state(),
    IsDus = sysEnv:is_dus(),

    case CoreState of
	active when IsDus ->
	    OwnIp = clhI:ip_address(),
	    case os:cmd("ip address show |grep " ++ to_str(OwnIp)) of
		[] ->
		    ok; %Own IP address not configured
		_ ->
		    init_nl_tls(OwnIp),
		    ok
	    end;
	_ ->
	    ok
    end.

%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_nl_tls(OwnIp) ->
    Dir = swmI:get_current_archive_dir(),
    case file:list_dir(Dir) of
	{ok, Filelist} ->
	    ParamList = [{root, Dir}, {default_contents, Filelist}],
	    sysNetloaderTls:set_params(ParamList),
	    sysNetloaderTls:run_server(OwnIp),
	    sysInitI:info_msg("~p: Configuring TLS fileserver~n", [?MODULE]);
	{error, Reason} ->
	    sysInitI:info_msg("~p: No configuring of TLS fileserver~n"
			      "Reason: ~p~n", [?MODULE, Reason]),
	    ok
    end.

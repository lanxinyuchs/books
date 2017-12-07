%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_rop_hook.erl %
%%% Author:	eolaand
%%% 
%%% Description: Reads and logs PM ROP files after each test case.
%%% Functions for verification of ROP content and number of files.  
%%%
%%% CTH dependency: pms_pmi_proxy
%%%
%%% ----------------------------------------------------------
-module(pms_rop_hook).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R11A/1').
-date('2017-10-20').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R3A/1    2014-10-16 eolaand     Created
%%% ----------------------------------------------------------


%% ROP functions
-export([expected/1,
	 expected/2,
	 list_rop_files/0,
	 list_rop_files/1,
	 get_rop_files/0,
	 get_rop_files/1,
	 get_rop_file/1,
	 get_rop_file/2]).

%% Server API
-export([start/0, 
	 start/1, 
	 stop/0, 
	 stop/1]).

%% CT hook callback functions
-export([
	 id/1,
	 init/2,
	 pre_init_per_suite/3,
	 post_init_per_suite/4,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 post_end_per_suite/4,
	 on_tc_fail/3,
	 terminate/1
	]).

%% Debug functions
-export([dump/0,
	 dump/1]).

%%%===================================================================
%%% Include files
%%%===================================================================
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Macros and records
%%%===================================================================
-define(SERVER, ?MODULE).
-define(PROXY_SERVER, pms_pmi_proxy).
-define(SFTP_CLIENT, rct_sftp_client).
-define(HOOK_PRIO, 10).

-record(cth_state, {
	  opts = [], 
	  name = ?MODULE, 
	  delete_rop_files = on_end_tc,
	  cs_node,
	  priv_dir
	 }).

-record(loop, {
	  name = ?SERVER,
	  cs_node,
	  t_ref,
	  expected = [],
	  exp_ref,
	  sftp = ?SFTP_CLIENT
	 }).

%%%===================================================================
%%% ROP functions
%%%===================================================================
expected(Exp) ->
    call({expected, Exp}).    


expected(Server, Exp) ->
    call(Server, {expected, Exp}).


list_rop_files() ->
    list_rop_files(?SERVER).


list_rop_files(Server) ->
    call(Server, {pms_db_call, rop_files_list, []}).


get_rop_files() ->
    get_rop_files(?SERVER).
	

get_rop_files(Server) ->
    call(Server, {pms_db_call, rop_data_get_all, []}).


get_rop_file(FileName) ->
    get_rop_file(?SERVER, FileName).


get_rop_file(Server, FileName) ->
    call(Server, {pms_db_call, rop_data_get, [FileName]}).


%%%===================================================================
%%% Support functions
%%%===================================================================

%%%===================================================================
%%% CT Hooks callbacks
%%%===================================================================
%% @private
id(Opts) ->
    proplists:get_value(instance_name, Opts, ?SERVER).

%% @private
init(Id, Opts) ->
    ct:pal("~w:init(~p, ~p)", [?MODULE, Id, Opts]),
    Name = lta(Id),
    CTHState = #cth_state{name = Name, opts = Opts},
    case start(Name, Opts) of
	{ok, {_Pid, CsNode}} ->
	    NewCTHState = 
		init_cth_state(Opts, CTHState#cth_state{cs_node = CsNode}),
	    {ok, NewCTHState, ?HOOK_PRIO};
	_Error ->
	    {ok, CTHState, ?HOOK_PRIO}
    end.


%% @private
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_suite(_SuiteName, InitData, CTHState) ->
    PrivDir = ?config(priv_dir, InitData),
    NewCTHState = CTHState#cth_state{priv_dir = PrivDir},
    case check_env(CTHState) of
	ok ->
	    delete_rop_files(CTHState),
	    expected(CTHState#cth_state.name, []),
	    {InitData, NewCTHState};
	{error, Reason} ->
	    ct:pal("~w: Skipping test suite due to ~p", [?MODULE, Reason]),
	    {{skip, Reason}, NewCTHState}
    end.

%% @private
post_init_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, CTHState}.


%% @private
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_testcase(_TC, Config, CTHState) ->
    case check_env(CTHState) of
	ok ->
	    {Config, CTHState};
	{error, Reason} ->
	    ct:pal("~w: Skipping test case due to ~p", [?MODULE, Reason]),
	    {{skip, Reason}, CTHState}
    end.

%% @private
%% post_end_per_testcase(TC, Config, {SkipOrFail, _Reason} = Ret, CTHState)
%%   when SkipOrFail =:= skip; SkipOrFail =:= fail ->
%%     catch log_te_log(TC, Config, CTHState),
%%     {Ret, CTHState};

post_end_per_testcase(_TC, Config, Return, CTHState) ->
    try
	expected(CTHState#cth_state.name, []),
	FileDir = get_file_dir(CTHState, Config),
	log_rop_files(CTHState#cth_state.cs_node, FileDir),
	delete_rop_files(CTHState)
    catch _:E ->
	    ct:log(lightred,
		   "~p: Failed to clean up and log ROP files~n~p",
		   [?MODULE, E]),
	    ok
    end,
    {Return, CTHState}.


%% @private
post_end_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, CTHState}.


%% @private
on_tc_fail(_TC, _Reason, CTHState) ->
    CTHState.
    

%% @private
terminate(CTHState) ->
    Server = CTHState#cth_state.name,
    Pid = whereis(Server),
    ct:pal("~w: terminate, server ~p pid = ~p", [?MODULE, Server, Pid]),
    case Pid of
	undefined ->
	    ok;
	_ ->
	    call(Pid, stop),
	    catch exit(Pid, kill)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start([]).

%% @private
start(Opts) ->
    start(lta(id(Opts)), Opts).


start(Server, Opts) ->
    Proxy = proplists:get_value(?PROXY_SERVER, Opts, ?PROXY_SERVER),
    Sftp = proplists:get_value(?SFTP_CLIENT, Opts, ?SFTP_CLIENT),
    case catch pms_pmi_proxy:get_cs_node(Proxy) of
	{ok, CSNode} ->
	    LoopData = #loop{name = Server, cs_node = CSNode, sftp = Sftp},
	    Pid = spawn(fun() -> server_loop(LoopData) end),
	    erlang:register(Server, Pid),
	    ct:pal("pms_rop_hook (~p) started: ~p", [Server, Pid]),
	    {ok, {Pid, CSNode}};
	Error ->
	    ct:log("Failed to start pms_rop_hook (~p): ~p", [Server, Error]),
	    {error, failed_to_start}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stop the server.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    stop(?SERVER).


%% @private
stop(Server) ->
    call(Server, stop).
    

%%%===================================================================
%%% Debug functions
%%%===================================================================
%% @private
dump() ->
    call(dump).


%% @private
dump(Server) ->
    call(Server, dump).


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% Server loop
%%%===================================================================
server_loop(#loop{expected = Exp, exp_ref = ExpRef} = LoopData) ->
    receive
	{timeout, _Ref, {check_expected, From, ExpRef}} when Exp =/= [] ->
	    NewLoopData = check_expected(Exp, From, ExpRef, LoopData),
	    server_loop(NewLoopData);
	{timeout, _Ref, {check_expected, _From, _ExpRef}} ->
	    server_loop(LoopData);
	{call, From, {expected, NewExp}} ->
	    cancel_timer(LoopData#loop.t_ref),
	    ct:pal("~p: Received expect call ~p", [?MODULE, NewExp]),
	    NewExpRef = make_ref(),
	    From ! {call_reply, {ok, NewExpRef}},
	    NewLoopData = check_expected(NewExp, From, NewExpRef, LoopData),
	    ct:pal("~p: NewLoopData = ~p", [?MODULE, NewLoopData]),
	    server_loop(NewLoopData);
	{call, From, {pms_db_call, Func, Arg}} ->
	    Reply = rpc_call(LoopData#loop.cs_node, pmsDb, Func, Arg),
	    From ! {call_reply, Reply},
	    server_loop(LoopData);
	{call, From, dump} ->
	    From ! {call_reply, {ok, LoopData}},
	    server_loop(LoopData);
	{call, From, stop} ->
	    From ! {call_reply, ok}
    end.


check_expected(Exp, From, ExpRef, LoopData) ->
    case check_expected_rop(Exp, From, ExpRef, LoopData) of
	[] ->
	    LoopData#loop{expected = [], 
			  exp_ref = undefined,
			  t_ref = undefined};
	RemExp ->
	    TRef = start_exp_timer(From, ExpRef),
	    LoopData#loop{expected = RemExp, 
			  exp_ref = ExpRef,
			  t_ref = TRef}
    end.


check_expected_rop([] = Exp, _From, _ExpRef, _LoopData) ->
    Exp;

check_expected_rop(Exp, From, ExpRef, LoopData) ->
    case rpc_call(LoopData#loop.cs_node, pmsDb, rop_data_get_all, []) of
        {ok, RopData} when RopData =/= [] ->
	    %% ct:pal("~p: Found new ROP Files: ~p~n .", 
	    %% 	   [?MODULE, [Name || {Name, _} <- RopData]]),
	    check_exp_contents(Exp, RopData, From, ExpRef, LoopData);
	_ ->
	    Exp
    end.


check_exp_contents({wait_until, ERExp, URExp} = Exp, RopData, From, ExpRef, 
		   LoopData) ->
    case check_rop_contents_until(RopData, ERExp, URExp) of
	{ok, _N} = Res ->
	    send_result(From, ExpRef, Res, LoopData);
	_Error ->
	    Exp
    end;

check_exp_contents(Exp, RopData, From, ExpRef, LoopData) ->
    NRopData = lists:zip(lists:seq(1, length(RopData)), RopData),
    try
	check_rop_contents(Exp, NRopData, From, ExpRef, LoopData)
    catch _:Reason ->
	    ct:pal("~p: ~p", [?MODULE, Reason]),
	    send_result(From, ExpRef, {error, Reason}, LoopData)
    end. 


%% If N is an integer, 
%% in which case the expected result must be in the N:th ROP file
%%
%% If N is an list of integers,
%% the expected result must be in one of the ROP files defined in N
check_rop_contents([{N, ERExp, URExp} | T] = Exp, NRopData, From, ExpRef, 
		   LoopData) ->
    case crc_search(N, NRopData) of
	false ->
	    Exp;
	{_N, RopData} ->
	    check_rop_cont(RopData, ERExp, URExp),
	    check_rop_contents(T, NRopData, From, ExpRef, LoopData)
    end;
check_rop_contents([], _NRopData, From, ExpRef, LoopData) ->
    send_result(From, ExpRef, ok, LoopData).


crc_search(N, NRopData) when is_integer(N) -> 
    crc_search([N], NRopData);
crc_search([], _NRopData) ->
    false;
crc_search([H | T], NRopData) -> 
    case lists:keyfind(H, 1, NRopData) of
	false -> crc_search(T, NRopData);
	Res   -> Res
    end.
    




check_rop_contents_until(RopData, RegExpI, RegExpE) ->
    check_rop_contents_until(RopData, RegExpI, RegExpE, 1).


check_rop_contents_until([RopData | T], RegExpI, RegExpE, N) ->
    case catch check_rop_cont(RopData, RegExpI, RegExpE) of
	ok ->
	    {ok, N};
	_Error ->
	    check_rop_contents_until(T, RegExpI, RegExpE, N + 1)
    end;

check_rop_contents_until([], _RegExpI, _RegExpE, _N) ->
    {error, failed_to_match}.


check_rop_cont({FileName, ZROP}, ERegExps, UERegExps) ->
    ROP = zlib:gunzip(ZROP),
    EF = fun({ERegExp, EOpts}) ->
		 case re:run(ROP, ERegExp, EOpts) of
		     {match, _} ->
			 ok;
		     _NoMatch ->
			 ct:log("Failed to match {~s, ~p} in ROP file ~s:"
				"~n~p~n~p", 
				[ERegExp, EOpts, FileName, _NoMatch, ROP]),
			 throw({"Failed to match in ROP file:", ERegExp, 
				FileName})
		 end
	 end,
    lists:foreach(EF, add_re_opts(ERegExps)),	 
    UEF = fun({UERegExp, UEOpts}) ->
		  case re:run(ROP, UERegExp, UEOpts) of
		      {match, _} when UERegExp =/= [] ->
			  ct:log("Unexpected match on ~s in ROP file ~s", 
				 [UERegExp, FileName]),
			  throw({"Unexpected match in ROP file:", UERegExp,
				 FileName});
		      _Nomatch ->
			  ok
		  end
	  end,
    lists:foreach(UEF, add_re_opts(UERegExps)).	 


add_re_opts(RegExps) ->
    lists:map(fun({_RegExp, _Opts} = RO) ->
		      RO;
		 (RegExp) when is_list(RegExp) ->
		      {RegExp, []}
	      end, RegExps).


start_exp_timer(From, ExpRef) ->
    erlang:start_timer(1000, self(), {check_expected, From, ExpRef}).    


send_result(From, Ref, Res, LoopData) ->
    From ! {rop_result, Ref, LoopData#loop.name, Res},
    [].
    
%%%===================================================================
%%% Init state
%%%===================================================================
init_cth_state(Opts, CTHState) ->
    DRF = proplists:get_value(delete_rop_files, Opts, 
			      CTHState#cth_state.delete_rop_files),
    CTHState#cth_state{delete_rop_files = DRF}.

%%%===================================================================
%%% Check that test environment is ok.
%%%===================================================================
check_env(CTHState) ->
    case whereis(CTHState#cth_state.name) of
	Pid when is_pid(Pid) ->
	    ping_cs_node(CTHState#cth_state.cs_node);	 
	_Undefined ->
	    {error, "pms_rop_hook server DOWN"}
    end.


ping_cs_node(CSNode) ->
    ping_cs_node(30, CSNode).


ping_cs_node(N, CSNode) when N > 0 ->
    case net_adm:ping(CSNode) of
	pong ->
	    ok;
	_Pang ->
	    timer:sleep(1000),
	    ping_cs_node(N-1, CSNode)
    end;

ping_cs_node(_N, _CSNode) ->
    {error, "CS Node is DOWN"}.



%%%===================================================================
%%% Call server
%%%===================================================================
call(Arg) ->
    call(?SERVER, Arg).


call(Server, Arg) ->
    Server ! {call, self(), Arg},
    receive
	{call_reply, Reply} ->
	    Reply
    after 20000 ->
	    {error, timeout}
    end. 


%% cast(Arg) ->
%%     cast(?SERVER, Arg).


%% cast(Server, Arg) ->
%%     Server ! {cast, Arg},
%%     ok.

%%%===================================================================
%%% Init test environment
%%%===================================================================

%%%===================================================================
%%% lib functions
%%%===================================================================
%%========================================================================
%% log_rop_files() -> any().
%% 
%% 
%%========================================================================
get_file_dir(CTHState, Config) ->
    case catch ?config(priv_dir, Config) of
	FileDir when is_list(FileDir) ->
	    FileDir;
	_ ->
	    CTHState#cth_state.priv_dir
    end.
    

log_rop_files(CSNode, FileDir) 
  when CSNode =/= undefined, FileDir =/= undefined ->
    {ok, RopData} = rpc_call(CSNode, pmsDb, rop_data_get_all, []),
    {ok, RelPath} =  relative_file_path(FileDir),
    F = fun({ZipFile, ZipData}, {F, D}) ->
		Tokens   = string:tokens(ZipFile, "."),
		Data     = zlib:gunzip(ZipData),
		Name     = string:join(Tokens -- ["gz"], "."),
		FileName = Name ++ ".txt",
		File     = filename:join(FileDir, FileName),
		ok       = file:write_file(File, Data),
		{"~n<a href=\"~s\">~s</a>" ++ F, 
		 [filename:join(RelPath, FileName), Name | D]}
	end,
    {Format, OutData} = lists:foldl(F, {[], []}, lists:reverse(RopData)),
    ct:log("ROP files:" ++ Format, OutData);

log_rop_files(CSNode, FileDir) ->
    ct:pal("Can't log ROP files. CS Node is ~p, priv_dir is ~p", 
	   [CSNode, FileDir]),
    ok.


delete_rop_files(CTHState) ->
    rpc_call(CTHState#cth_state.cs_node, pmsDb, rop_file_delete_all, []).


relative_file_path(FileDir) ->
    {ok, CWD} = file:get_cwd(),    
    SplitPath = filename:split(FileDir),
    SplitCWD = filename:split(CWD),
    relative_file_path(SplitCWD, SplitPath).


relative_file_path([H | T1], [H | T2]) ->
    relative_file_path(T1, T2);

relative_file_path([], [_TC, _Run, RelFilePath]) ->
    {ok, RelFilePath};

relative_file_path(_, _) ->
    {error, abspath}.


%% get_config(Par) ->
%%     case ct:get_config(Par) of
%% 	undefined ->
%% 	    ct:log(lightred,
%% 		   "~p ~p Could not read config parameter ~p for "
%% 		   "pms_pmi_proxy, Reason: undefined",
%% 		   [?MODULE, get_config, Par]),
%% 	    throw({?MODULE, {{fail, {undefined, Par}}}});
%% 	Val ->	    
%% 	    Val
%%     end.


cancel_timer(undefined) ->
    ok;

cancel_timer(Ref) ->
    erlang:cancel_timer(Ref),
    receive
	{timeout, Ref, _} ->
	    ok
    after 0 ->
	    ok
    end.


rpc_call(Node, M, F, A) ->
    rpc:call(Node, M, F, A, 20000).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.


%% atl(A) when is_atom(A) ->
%%     atom_to_list(A);
%% atl(L) when is_list(L) ->
%%     L.



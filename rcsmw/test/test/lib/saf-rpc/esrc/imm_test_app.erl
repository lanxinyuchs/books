%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	imm_test_app.erl %
%% @private
%% @author etxarnu
%% @copyright Ericsson AB 2012-2015
%% @doc 
%% @end
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% R2A/1      2012-10-23 eolaand     Created
%%% ----------------------------------------------------------
-module(imm_test_app).

%% API
-export([main/1, load_driver/2]).

-define(IMM_TEST_APP_NAME, atom_to_list(?MODULE)).
-define(IMM_TEST_APP, ?IMM_TEST_APP_NAME ++ ".escript").
-define(SAFE_IMM_SO, "safe_imm_drv.so").
-define(SAFE, safe).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec main(Arg::list()) -> ok | {error, Reason::atom()}
%% @end
%%--------------------------------------------------------------------
main(Arg) ->
    fix_code_path(),
    AppScript = get_app_script(),
    {ok,[_,_,_,{archive,ArchBin}]} = escript:extract(AppScript,[]),
%    {ok,[_,_,_,{archive,ArchBin}]} = escript:extract(?IMM_TEST_APP,[]),
    LibPath = get_arch_lib_dir(),
    SoFileRelPath = filename:join([LibPath,?SAFE_IMM_SO]),
    [TmpDir] = string:tokens(os:cmd("mktemp -d"),"\n"),
    {ok, [SoFilePath]} = zip:extract(ArchBin, [{file_list, [SoFileRelPath]},
					       {cwd,TmpDir}]),
    true = os:putenv("SAFE_LIB_DIR", filename:dirname(SoFilePath)),
    NodeName = node_name(Arg),
    {ok, _} = net_kernel:start([NodeName,shortnames]),
    case os:getenv("SIM_TGT") of
	"tgt_i686" ->
	    ok;
	_ ->
	    erlang:set_cookie(node(),NodeName)
    end,
    application:set_env(safe,services,[imm]),
    io:format("Starting Safe IMM on node ~p~n", [node()]),
    StartRes = safe:start(),
    if 
	StartRes =:= ok ->
	    io:format("Safe IMM started successfully!~n", []);
	true ->
	    ok
    end,
    file:delete(TmpDir),
    ok = StartRes,
    receive
	forever ->
	    ok
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_app_script() ->
    PathComps = filename:split(code:lib_dir(safe)),
    Path = 
	lists:takewhile(fun(Comp) -> Comp =/= ?IMM_TEST_APP end, PathComps),
    filename:join(Path ++ [?IMM_TEST_APP]).
    

fix_code_path() ->
    Archive = ?IMM_TEST_APP,
    F = fun(Path) ->
		case lists:reverse(filename:split(Path)) of
		    [Archive | _] ->
			false;
		    _ ->
			true
		end
	end,
    [TopPath | _] = lists:dropwhile(F, code:get_path()),
    true = code:add_patha(TopPath).


get_arch_lib_dir() ->
    PathComps = filename:split(code:priv_dir(safe)),
    [_ | LibPath] = 
	lists:dropwhile(fun(Comp) -> Comp =/= ?IMM_TEST_APP end, PathComps),
    filename:join(LibPath ++ ["lib"]).
	
			 
node_name([]) ->
    list_to_atom(?IMM_TEST_APP_NAME ++ add_user_name());

node_name([Name | _]) ->
    list_to_atom(Name).


add_user_name() ->    
    case os:getenv("USER") of
	false ->
	    "";    
	Name ->
	    "_" ++ Name
    end.


load_driver(Driver, Arg) ->
    application:load(safe),
    Dir = get_so_lib_dir(),
    case erl_ddll:try_load(Dir, Driver, []) of
	{ok, _} ->
	    safe_lib:port_open(Driver, Arg);
	{ok, _, _} ->
	    safe_lib:port_open(Driver, Arg);
	{error, permanent} ->
	    safe_lib:port_open(Driver, Arg);
	{error, Reason} ->
	    Str = erl_ddll:format_error(Reason),
	    erlang:error({load_driver, Str}, [Dir, Driver])
    end.


get_so_lib_dir() ->
    case os:getenv("SAFE_LIB_DIR") of
	false ->
	    Type = erlang:system_info(system_architecture),
	    LibDir = filename:join([code:priv_dir(safe), "lib"]),
	    ArchDir = filename:join([LibDir, Type]),
	    get_so_lib_dir(ArchDir, LibDir);
	Dir ->
	    Dir
    end.


get_so_lib_dir(ArchDir, LibDir) ->
    case filelib:is_dir(ArchDir) of
	true  -> 
	    ArchDir;
	false -> 
	    LibDir
    end.
    

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cover_hook.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R4A/1
%%%
%%% @doc == A hook to load cover.erl to node, use when get test coverage ==
%%%  see <a href="http://www.erlang.org/doc/man/cover.html">cover</a><br/>
%%%
%%% To be able to run cover you have to define a .coverspec file. <br/>
%%% Also collect .beam and .erl to same directory. <br/>
%%% or link to escr to src, like
%%% 
%%% Start testsuite with -cover xyz.coverspec -pa /directoury/to/beam_and_erl. <br/>
%%% Coverage data shows in html_log. <br/>
%%% > ln -s esrc src % will do a softlink from esrc to a fake src dir. <br/>
%%% Note! you need rct_rpc hook befor this cover_hook in your test_SUITE.
%%% Hook formats:
%%%    ``` [{ct_hooks, [{rct_rpc, rpc_1},
%%%                     {cover_hook, [{TargHost, Sname}]}]} ].
%%%        [{ct_hooks, [{rct_rpc, rpc_1},
%%%                     {cover_hook,[{du1, username}]}]}]. '''
%%%
%%% Argument description:
%%% '''TargHost    = atom()                      Used to build up Node name.
%%%    Sname   = atom()                          Used in simulated env as node name,
%%%                                              atom username, will result that your username will be used as sim nodename.'''
%%%
%%%
%%% Testcase example:  $RCT_TOP/test/suites/LIH/lici_erlang_SIUTE.erl <br/>
%%% .coverspec example:  $RCT_TOP/test/suites/LIH/lih_erlang.coverspec <br/>
%%%
%%% example to start test_SUITE on sim env using cover flag:<br/>
%%%
%%% $RCT_TOP/test/bin/rct_run.sh -sim mystp <br/>
%%% -dir $RCT_TOP/test/suites/LIH -suite lici_erlang_SUITE <br/>
%%% -case all -noshell <br/>
%%% -cover $RCT_TOP/test/suites/LIH/lih_erlang.coverspec <br/>
%%% -pa $RCT_TOP/LIH/LIH_CNX9012615/LIH_CAX1033067/out <br/>
%%%
%%%
%%% @end


-module(cover_hook). 
%%% Except as specifically authorized in writing by Ericsson, the receiver of
%%% this document shall keep the information contained herein confidential and
%%% shall protect the same in whole or in part from disclosure and dissemination
%%% to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall only be made
%%% on a strict need to know basis.
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev         Date         Name        What
%%% -----       ---------    --------    ------------------------
%%% R1A/1       2012-10-02   etxivri     Created
%%% R4A/1      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% -----------------------------------------------------------------------------

-export([init/2]).
-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).

-include_lib("common_test/include/ct.hrl").

-define(TargetDefaultUser,sirpa).


%% @hidden
init(_Id, Opts) ->
    {ok, Opts}.


%%--------------------------------------------------------------------
%% @doc
%% load cover on SUT (no need on sim node) and add nodes to current cover test. <br/>
%% @spec pre_init_per_suite(_Suite, Config, States) -> {Config, States}
%% @end
%%--------------------------------------------------------------------
pre_init_per_suite(_Suite, Config , States) ->
    Module = cover, %$OTP/suse_x86/lib/tools-0/cover.erl
    {Module, Binary, Filename} = code:get_object_code(Module),
    SimOrTarget = os:getenv("SIM_OR_TARGET"),
    lists:foreach(fun({_TargHost, Sname}) when SimOrTarget == "sim" ->
			  {ok, SimHostName} = inet:gethostname(),
			  SimUserName =  case Sname of
					     username ->
						 os:getenv("USER");
					     noname ->
						 os:getenv("USER");
					     _ ->
						 atom_to_list(Sname)
					 end,
			  SimErlNode = list_to_atom(lists:concat([SimUserName,"@", SimHostName])),
			  ct:pal("# SimErlNode: ~p",[SimErlNode]),
			  %% cover.erl is already loaded on the simulated node.
			  %% rpc:call(SimErlNode, code, load_binary, [Module, Filename, Binary]),
			  ct_cover:add_nodes([SimErlNode]);
		     ({TargHost, _Sname}) ->
			  ErlNode = list_to_atom(lists:concat([?TargetDefaultUser,"@", TargHost])),
			  %% ct:pal("# ErlNode: ~p",[ErlNode]),
			  rpc:call(ErlNode, code, load_binary, [Module, Filename, Binary]),	  
			  ct_cover:add_nodes([ErlNode])
		  end, States),

    {Config, States}.


%%--------------------------------------------------------------------
%% @doc
%% Remove nodes from current cover test. Call this function to stop cover test on nodes. <br/>
%% @spec post_end_per_suite(_Suite, _Config, Return , States) -> {Return, States}
%% @end
%%--------------------------------------------------------------------
post_end_per_suite( _SUITE, _Config, Return , States) ->
    lists:foreach(fun({TargHost, Sname})->
			  case os:getenv("SIM_OR_TARGET") of
			      "sim" -> 
				  {ok, SimHostName} = inet:gethostname(),
				  SimUserName =  case Sname of
						     username ->
							 os:getenv("USER");
						     noname ->
							 os:getenv("USER");
						     _ ->
							 atom_to_list(Sname)
						 end,
				  SimErlNode = list_to_atom(lists:concat([SimUserName,"@", SimHostName])),
				  ct_cover:remove_nodes([SimErlNode]);
			      "target" ->
				  ErlNode = list_to_atom(lists:concat([?TargetDefaultUser,"@", TargHost])),
				  ct_cover:remove_nodes([ErlNode])
			  end
		  end, States),

    {Return, States}.

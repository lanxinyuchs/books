%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rob_coli_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015
%%% @version /main/R4A/5
%%%
%%% @doc == Several users using cli operations.==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%%
%%% @end

-module(rob_coli_SUITE).
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2015-10-07 etxivri     Created
%%% R4A/3      2015-10-07 etxivri     Increased testsuites timetrap.
%%% R4A/4      2015-10-20 etxivri     Changed ct:pal to ct:log.
%%% R4A/5      2015-10-22 etxivri     Changed ct:pal to ct:log.
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 loop_coli_help/1
	]).

-define(COLI, coli).  

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_cli hook for each user, to be able to use cli for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 240}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {cth_conn_log, []},
		 {rct_core,[]},
		 {rct_coli, {coli, [manual_connect, {connect_timeout, 60}]}},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       []}}]}}
     		]}].

%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:log("Testcase failed due to: ~p.  \nCleanup.", 
		   [Reason])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     loop_coli_help
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].

%%--------------------------------------------------------------------
%% @doc
%% @spec loop_coli_help(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
loop_coli_help(_Config) ->
    A = lists:seq(1, 10000),
    %% A = lists:seq(1, 1),
    B = length(A),
    ct:pal("### Loop help coli cmd, ~p times.",[B]),
    lists:foreach(fun(X) ->
			  ct:pal("## Nr : ~p", [X]),
			  wait_for_coli_connect(),
			  send_help_cmd_check_answ(),
			  wait_for_coli_disconnect()
		  end, A).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
send_help_cmd_check_answ() ->
    {ok,Answer} = rct_coli:send(coli,"help coli"),
    %% ct:pal("## : ~p", [Answer]),
    %% Answ = string:tokens(Answer, "\r\n\""),
    case re:run(Answer, "Coli cmd shell usage") of
	{match, _} ->
	    ok;
	nomatch ->
	    ct:pal("## Nomatched Answer: ~p", [Answer]),
	    ct:fail("TC will fail due to unexpected answer.")
    end.

wait_for_coli_connect() ->
    wait_for_coli_connect(30000).
wait_for_coli_connect(Timeout) when Timeout < 0 ->
    ct:fail("Coli connect not ok within timeout!");
wait_for_coli_connect(Timeout) ->
    case rct_coli:connect(coli) of
	ok ->
	    ok;
	_Other ->
	    ct:log("Connect coli failed: ~p ~n "
		   "Sleep and try again",
		   [_Other]),
	    timer:sleep(5000),
	    wait_for_coli_connect(Timeout-5000)
    end.
	    
wait_for_coli_disconnect() ->
    wait_for_coli_disconnect(30000).
wait_for_coli_disconnect(Timeout) when Timeout < 0 ->
    ct:fail("Coli disconnect not ok within timeout!");
wait_for_coli_disconnect(Timeout) ->
    case rct_coli:disconnect(coli) of
	ok ->
	    ok;
	_Other ->
	    ct:log("Disconnect coli failed: ~p ~n "
		   "sleep and try again",
		   [_Other]),
	    timer:sleep(5000),
	    wait_for_coli_disconnect(Timeout-5000)
    end.

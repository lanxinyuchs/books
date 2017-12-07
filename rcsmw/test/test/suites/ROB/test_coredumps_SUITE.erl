%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	test_coredumps_SUITE.erl %
%%% @author etxjovp
%%% @copyright Ericsson AB 2013-2015
%%% @version /main/R2A/R3A/1
-module(test_coredumps_SUITE).
%%% ----------------------------------------------------------
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
%%% R2A/1      2013-01-21 etxkols     created
%%% R2A/2      2013-01-28 etxkols     com testcase added
%%% R2A/3      2013-01-28 etxkols     Robustness fix
%%% R2A/4      2013-01-28 etxkols     More fixes
%%% R2A/5      2013-01-28 etxkols     Removed com from all/0
%%% R2A/6      2013-04-18 etxpeno     Support for new APPM implementation
%%% R2A/7      2013-07-04 etxarnu     Support for crash printout
%%% R2A/8      2014-02-03 etxkols     Increased rpc:call timeout
%%% R3A/1      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0,
	 test_oi/1,
	 com/1]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_rpc, rct_netconf, cth_conn_log, rct_logging, rct_tlib
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_core, []},
		 {rct_htmllink,[]},
		 {rct_rpc, rpc},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       ["\"test_oi\" died with Exit code",
						"Program ID [0-9]+ has terminated",
						"Program ID [0-9]+ has crashed"]}}]}}
		]}].

%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.
%% @hidden
groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__dus__1__group, [], []},
     {sbc__qual__tcu__1__group, [], []},
     {sbc__def__dus__1__group, [], [{group, default__group}]},
     {sbc__def__tcu__1__group, [], [{group, default__group}]},
     {sbc__upgrade__dus__1__group, [], [{group, default__group}]},
     {sbc__upgrade__tcu__1__group, [], [{group, default__group}]},
     {sdc__def__dus__1__group, [], [{group, default__group}]},
     {sdc__def__tcu__1__group, [], [{group, default__group}]},
     {sdc__qual__dus__1__group, [], []},
     {sdc__qual__tcu__1__group, [], []}
    ].
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
%    [test_oi, com].
    [test_oi].

%%--------------------------------------------------------------------
%% @doc
%% Tests that coredump can be generated on target and sim by doing kill -6 on test_oi.
%% Downloaded coredump is deleted<br/><br/>
%% @end
%%--------------------------------------------------------------------
test_oi(_) ->
    ok = rct_core:coredump_test(),
    case rct_rpc:call(rpc, appmServer, get_apps, [], 10000) of
	{badrpc, Reason} ->
	    ct:log(lightred,"Could not connect to erlang node with rpc, Reason: ~p",[{badrpc, Reason}]),
	    ct:fail("Could not rpc node, Reason: ~p", [{badrpc, Reason}]);
	Apps ->
	    case lists:keysearch("test_oi",1,Apps) of
		{value,{"test_oi",Pid}} ->
		    case rct_rpc:call(rpc, os, cmd, ["kill -6 " ++ Pid], 10000) of
			{badrpc, Reason} ->
			    ct:log(lightred,"Could not connect to erlang node with rpc, Reason: ~p",[{badrpc, Reason}]),
			    ct:fail("Could not rpc node, Reason: ~p", [{badrpc, Reason}]);
			_ ->
			    timer:sleep(10000)
		    end;
		false ->
		    ct:log(lightred,"Application test_oi not found",[]),
		    ct:fail("Application test_oi not found, Applications: ", [Apps])
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Tests that coredump can be generated on target and sim by doing kill -6 on com.
%% Downloaded coredump is deleted<br/><br/>
%% @end
%%--------------------------------------------------------------------
com(_) ->
    ok = rct_core:coredump_test(),
    case rct_rpc:call(rpc, comte_com, get_pid, [], 10000) of
	{badrpc, Reason} ->
	    ct:log(lightred,"Could not connect to erlang node with rpc, Reason: ~p",[{badrpc, Reason}]),
	    ct:fail("Could not rpc node, Reason: ~p", [{badrpc, Reason}]);
	Pid ->
	    Pid2 = string:strip(Pid,right,$\n),
	    case re:run(Pid2,"^[0-9]+$",[]) of
		{match,_} ->
		    case rct_rpc:call(rpc, os, cmd, ["kill -6 " ++ Pid2], 10000) of
			{badrpc, Reason} ->
			    ct:log(lightred,"Could not connect to erlang node with rpc, Reason: ~p",[{badrpc, Reason}]),
			    ct:fail("Could not rpc node, Reason: ~p", [{badrpc, Reason}]);
			_ ->
			    timer:sleep(10000)
		    end;
		_ ->
		    ct:log(lightred,"rpc comte_com:get_pid() did not return Pid, Returned: ",[Pid2]),
		    ct:fail("rpc comte_com:get_pid() did not return Pid")
	    end
    end.

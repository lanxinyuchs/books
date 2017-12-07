%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	netconf_users_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R2A/R3A/R4A/R5A/R7A/1
%%% 
%%% @doc == Several users using netconf operations.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%% 
%%% @end

-module(netconf_users_SUITE).
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2012-12-14 etxivri     Created
%%% R2A/3      2013-01-11 etxivri     added TCs
%%% R2A/4      2013-01-11 etxivri     added memory check after open,add,delete,close
%%% R2A/5      2013-01-18 etxivri     Removed memory check, this will be done in /MEAS. 
%%%                                   Updates in clean up if tc fail.
%%% R2A/6      2013-01-23 etxivri     Added sync in end per testcase.
%%% R2A/7      2013-02-01 etxivri     Added new TCs nc_users_add_rbsUnit_paralell,
%%%                                   nc_users_add_delete_rbsUnit_paralell,
%%%                                   Updated paralell requests TCs. Updated end per tc.
%%% R2A/8      2013-02-08 etxivri     Minor updates in kill_com tc.
%%% R2A/9      2013-02-18 etxivri     Minor updates in nc_sessions_idle_timeout and
%%%                                   nc_users_add_rbsUnit_paralell_commit.
%%% R2A/10     2013-03-01 etxivri     Updated cleanup in end_per_testcase.
%%% R2A/11     2013-03-08 etxivri     Updated cleanup session after reboot and powe off/on.
%%%                                   Added rct_core hook.
%%% R2A/12     2013-04-23 etxivri     Added a sleep before init restart, a try to make it more robust!
%%% R2A/13     2013-05-07 etxivri     Added wait_for_appl_started, to make TC more robust.
%%%                                   Improved cleanup.
%%% R2A/14     2013-05-24 etxivri     Decreased nr of netconf sessions in init_restar TC,
%%%                                   Due to problem in OTP. They take down ssh sessions in sequense
%%%                                   that take 4 sec each.
%%% R2A/15     2013-05-27 etxivri     Updated kill com. Added $USER in "pgrep".
%%% R2A/16     2013-06-27 etxivri     Changed timetrap to 30 min.
%%% R2A/16     2013-06-28 etxivri     Added TCs, that add several rbsUnits in one commit, and some fail cases.
%%% R2A/18     2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/19     2013-08-27 etxivri     added a filter when kill com is used.
%%% R2A/20     2013-09-02 etxivri     Changed filtered string in ERROR hook.
%%% R2A/21     2013-09-05 etxivri     Changed to a better pgrep com.
%%% R2A/22     2013-09-10 etxivri     Increased timeout value in TC nc_users_paralell_get_config.
%%% R2A/23     2013-09-16 etxivri     Kill com TC is ok to run. Removed group
%%% R2A/24     2013-09-17 etxivri     Updated restart tc to use all NC sessions.
%%%                                   Added init:reboot TC.
%%% R2A/25     2013-10-09 etxivri     Update to make init:reboot more robust.
%%% R2A/26     2013-10-22 etxivri     Added a check to make sure node restarts after init_rboot
%%% R2A/27     2014-01-16 etxkols     Support for dus5201. 
%%% R3A/1      2015-05-27 etxivri     Remove compile warning.
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warning and copyright
%%% R7A/1      2016-09-08 etxivri     Changed random to rand.
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
	 nc_sessions_idle_timeout/1,
	 nc_sessions_subscribe_notificataion/1,
	 nc_users_paralell_get_config/1,
	 nc_users_random_get_config/1,
	 nc_users_add_rbsUnit/1,
	 nc_users_add_rbsUnit_commit/1,
	 nc_users_add_rbsUnit_commit_reversed_order/1,
	 nc_users_add_rbsUnit_paralell/1,
	 nc_users_add_delete_rbsUnit_paralell/1,
	 nc_users_add_rbsUnit_paralell_commit/1,
	 nc_users_add_rbsUnit_restart/1,
	 nc_users_add_rbsUnit_init_reboot/1,
	 nc_users_add_rbsUnit_reboot/1,
	 nc_users_add_rbsUnit_power/1,
	 nc_users_add_rbsUnit_kill_com/1,
	 nc_user_add_10_rbsUnits_one_commit/1,
	 nc_user_fail_to_add_attribute/1,
	 nc_user_fail_to_add_struct_attribute/1
	]).

%-define(NrOfNetConfUsers, 1).
%% -define(NrOfNetConfUsers, 5).
%-define(NrOfNetConfUsers, 20).
-define(NrOfNetConfUsers, 50).

-define(NC_SessionNameList, [list_to_atom("nc"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfNetConfUsers)]).

-define(NrOfMoInstList, lists:seq(1,10) ).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    %% ct:pal("NetconfHooks: ~p",[NetconfHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_core,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       ["The job was brutally killed - exiting"]}}]}} | 
		 %% {rct_logging, {all, [{erlang,{[],[]}}]}} |
		 NetconfHooks
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added rbs units", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 5000, noprint),
	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf -u $USER"], 5000, noprint),
	    CC = length(string:tokens(C,"\n")),
	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Netconf: ~p",[CC]),	    

	    %%%%
	    %% Open a session and clean up rbsUnits, close all session.
            %%%%
	    try
		lists:foreach(fun(Name) -> nc_open(ct_netconfc:open(Name, []), Name) end, ?NC_SessionNameList)
	    catch throw:{?MODULE, found} ->
		    ok
	    end,

            %%%%
	    %% Commit, close sessions.
            %%%%
	    lists:foreach(fun(Name1) ->
				  %% ct:pal("### Cleanup after fail TC, Close session: ~p", [Name1]),
				  ct_netconfc:close_session(Name1)
			  end, 
			  ?NC_SessionNameList)
	    		 
    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.

nc_open({error,{connection_exists,_}}, Name) ->
    ct_netconfc:close_session(Name), % Clean up at our client, if netconf process was killed on node, 
    ct_netconfc:open(Name, []), % Then set up again.
    nc_delete_rbsunits(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    nc_delete_rbsunits(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     nc_sessions_idle_timeout,
     nc_sessions_subscribe_notificataion,
     nc_users_paralell_get_config,
     nc_users_random_get_config,
     nc_users_add_rbsUnit,
     nc_users_add_rbsUnit_commit,
     nc_users_add_rbsUnit_commit_reversed_order,
     nc_users_add_rbsUnit_paralell,
     nc_users_add_delete_rbsUnit_paralell,
     nc_users_add_rbsUnit_paralell_commit,
     nc_users_add_rbsUnit_restart,
     nc_users_add_rbsUnit_init_reboot,
     nc_users_add_rbsUnit_reboot,
     nc_users_add_rbsUnit_power,
     nc_users_add_rbsUnit_kill_com,
     nc_user_add_10_rbsUnits_one_commit,
     nc_user_fail_to_add_attribute,
     nc_user_fail_to_add_struct_attribute
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
%% Verify that a session that not subscribes on event notifications <br/>
%% will be closed after 5min if no data has been sent.  <br/>
%% @spec nc_sessions_idle_timeout(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_sessions_idle_timeout(_Config) ->
    ct:pal("### Check that a session is closed after 5min, if idle!",[]),

    %%%%
    %% Open Netconf sessions. 10 sessions vill be used for this test.
    %% Otherwise sessions could be released before check, due to timeout.
    %%%%
    {UsedSessionNames,_} = lists:split(10, ?NC_SessionNameList),
    ok = nc_open_session(UsedSessionNames),

    ct:pal("### All nc sessions opened.",[]),
    ct:pal("### sleep 4 min. Check that sessions still exist",[]),
    ct:sleep({minutes, 4}), %% this is not a thorough check!

    lists:foreach(fun(Name1) ->
    			  ct:pal("### Check that session still exist: ~p", [Name1]),
    			  {error,{connection_exists,_}} = ct_netconfc:open(Name1, [])
    		  end, 
    		  UsedSessionNames),

    ct:pal("### sleep 1 min and 30 sec.",[]),
    ct:sleep({seconds, 90}), % add 30 sec to make sure sessions is closed.
    ct:pal("### sleep 5 min and 30 sec has passed. Sessions shall be closed",[]),

    lists:foreach(fun(Name2) ->
    			  ct:pal("### Open Name: ~p", [Name2]),
    			  {ok, _} = ct_netconfc:open(Name2, [])
    		  end, 
    		  UsedSessionNames),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(UsedSessionNames),

    ok.
    
    
%%--------------------------------------------------------------------
%% @doc
%% Verify that a session that subscribes on notifications <br/>
%% shall not be closed after 5 min, even if no data has been sent.  <br/>
%% @spec nc_sessions_subscribe_notificataion(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_sessions_subscribe_notificataion(_Config) ->
    ct:pal("### Check that a session is still up after 5min, if idle!",[]),

    lists:foreach(fun(Name) ->
			  ct:pal("### Open Name: ~p", [Name]),
    			  {ok, _} = ct_netconfc:open(Name, []),
			  ok = ct_netconfc:create_subscription(Name)
    		  end, 
    		  ?NC_SessionNameList),
    ct:pal("### nc sessions opened.",[]),

    ct:pal("### sleep 6 min. Session shall not be closed",[]),
    ct:sleep({minutes, 6}), 

    lists:foreach(fun(Name2) ->
			  ct:pal("### Check that session still exist: ~p", [Name2]),
    			  {error,{connection_exists,_}} = ct_netconfc:open(Name2, [])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),
 
    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% Check several netconf users do paralell request on get_config. <br/>
%% @spec nc_users_paralell_get_config(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_paralell_get_config(_Config) ->
    ct:pal("### Check several netconf users do paralell request on get_config!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% send get config req.
    %%%%    
    Self = self(),    
    PidList = lists:map(fun(Name1) ->
				spawn(fun() -> ct:pal("### Get Cofig with Name: ~p", [Name1]),
					       ct_netconfc:get_config(Name1,running,{'ManagedElement',
										     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
										     [{managedElementId,[],["1"]}]}),
					       Self ! ok
				      end)
			end,
			?NC_SessionNameList),

    %%%%
    %% Wait on answer from add  request.
    %%%%
    lists:foreach(fun(_Pid) ->
			 receive
    			      ok -> 			      
				 ok
			 after 90000 -> 
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList),

    %%%%
    %% Close session
    %%%%
    ct:pal("All get_config is done ok. Close sessions",[]),
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several netconf users do random request on get_config. <br/>
%% @spec nc_users_random_get_config(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_random_get_config(_Config) ->
    ct:pal("### Check several netconf users do random request on get_config!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
			 
    lists:foreach(fun(N) ->
			  RandomNr = rand:uniform(?NrOfNetConfUsers),
    			  Name1 = list_to_atom("nc"++integer_to_list(RandomNr)),
			  ct:pal("### Nr: ~p, Get Cofig with Name: ~p", [N, Name1]),
    			  {ok,_} = ct_netconfc:get_config(Name1,running,{'ManagedElement',
							       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							       [{managedElementId,[],["1"]}]})
    		  end, 
    		  lists:seq(1,?NrOfNetConfUsers)),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add rbs units. Do commit after each rbsUnit is added. <br/>
%% @spec nc_users_add_rbsUnit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units. Do commit after each rbsUnit is added!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %% %%%%
    %% Add rbs units
    %%%%
    lists:foreach(fun(Name1) ->
    			  ok = nc_add_rbsunit(Name1),
    			  ct:pal("### Close session: ~p", [Name1]),
    			  ok = ct_netconfc:close_session(Name1),
    			  ct:pal("### Open session: ~p", [Name1]),
    			  {ok,_} = ct_netconfc:open(Name1,[]),
			  ok = check_rbs_units_is_added([Name1])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name2) ->
			  ok = nc_delete_rbsunit(Name2),
			  ct:pal("### Close session: ~p", [Name2]),
			  ok = ct_netconfc:close_session(Name2),
			  ct:pal("### Open session: ~p", [Name2]),
			  {ok,_} = ct_netconfc:open(Name2,[]) ,
			  ok = check_rbs_units_is_deleted([Name2])
    		  end, 
    		  ?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add rbs units. <br/>
%% Do commit after all rbsUnits has been added.  <br/>
%% Check that not commited rbsUnits is not commited, until their sessions is closed.<br/>
%% @spec nc_users_add_rbsUnit_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_commit(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units. Do commit after all rbsUnits has been added!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    lists:foreach(fun(Name1) ->
    			  nc_add_rbsunit(Name1),
			  % Check that rbsUnit is deleted, data in COMTE cash. 
			  check_rbs_units_is_not_commited([Name1])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name2) ->
    			  ct:pal("### Close session: ~p", [Name2]),
    			  ok = ct_netconfc:close_session(Name2),
    			  ct:pal("### Open session: ~p", [Name2]),
    			  {ok,_} = ct_netconfc:open(Name2,[]),
			  % Check rbs unit is created.
			  check_rbs_units_is_added([Name2]),
			  % Check that ohter rbs unit is not commited.
                          % create a list with the not commited rbsUnits.
			  NotCommitedList = lists:dropwhile(fun(X) -> 
								    X=/=Name2
							    end, ?NC_SessionNameList),
			  CompleteNotCommitedList = lists:delete(Name2, NotCommitedList),
			  %% ct:pal("~p", [CompleteNotCommitedList]),
			  check_rbs_units_is_not_commited(CompleteNotCommitedList)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name3) ->
    			  nc_delete_rbsunit(Name3),
			  % Check that rbsUnit is deleted, data in COMTE cash. 
			  check_rbs_units_is_deleted([Name3])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name4) ->
    			  ct:pal("### Close session: ~p", [Name4]),
    			  ok = ct_netconfc:close_session(Name4),
    			  ct:pal("### Open session: ~p", [Name4]),
    			  {ok,_} = ct_netconfc:open(Name4,[]),
			  % Check rbs unit is deleted.
			  check_rbs_units_is_deleted([Name4]),
			  % Check that ohter rbs unit is deleted, data from COMTE cash.
			  % create a list with the remaining not deleted rbsUnits.
			  NotDeletedList = lists:dropwhile(fun(Y) -> 
			  					    Y=/=Name4
			  				    end, ?NC_SessionNameList),
			  CompleteNotDeletedList = lists:delete(Name4, NotDeletedList),
			  %% ct:pal("~p", [CompleteNotDeletedList]),
			  check_rbs_units_is_deleted(CompleteNotDeletedList)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add rbs units.<br/>
%% After all rbsUnits is added, do commit in reverse order. <br/>
%% Check that not commited rbsUnits is not commited, until their sessions is closed.<br/>
%% @spec nc_users_add_rbsUnit_commit_reversed_order(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_commit_reversed_order(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units. after all rbsUnits has been added do commit in reverse order!",[]),

    Reversed_SessionNameList = lists:reverse(?NC_SessionNameList),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units, no commit
    %%%%
    lists:foreach(fun(Name1) ->
    			  nc_add_rbsunit(Name1),
			  % Check that rbsUnit is deleted, data in COMTE cash. 
			  check_rbs_units_is_not_commited([Name1])
    		  end, 
		  ?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name2) ->
    			  ct:pal("### Close session: ~p", [Name2]),
    			  ok = ct_netconfc:close_session(Name2),
    			  ct:pal("### Open session: ~p", [Name2]),
    			  {ok,_} = ct_netconfc:open(Name2,[]),
			  % Check rbs unit is created.
			  check_rbs_units_is_added([Name2]),
			  % Check that ohter rbs unit is not commited.
                          % create a list with the not commited rbsUnits.
			  NotCommitedList = lists:dropwhile(fun(X) -> 
			  					    X=/=Name2
			  				    end, Reversed_SessionNameList),
			  CompleteNotCommitedList = lists:delete(Name2, NotCommitedList),
			  %% ct:pal("~p", [CompleteNotCommitedList]),
			  check_rbs_units_is_not_commited(CompleteNotCommitedList)
    		  end, 
		  Reversed_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name3) ->
    			  nc_delete_rbsunit(Name3),
			  % Check that rbsUnit is deleted, data in COMTE cash. 
			  check_rbs_units_is_deleted([Name3])
    		  end, 
		  ?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name4) ->
    			  ct:pal("### Close session: ~p", [Name4]),
    			  ok = ct_netconfc:close_session(Name4),
    			  ct:pal("### Open session: ~p", [Name4]),
    			  {ok,_} = ct_netconfc:open(Name4,[]),
			  % Check rbs unit is deleted.
			  check_rbs_units_is_deleted([Name4]),
			  % Check that ohter rbs unit is deleted, data from COMTE cash.
			  % create a list with the remaining not deleted rbsUnits.
			  NotDeletedList = lists:dropwhile(fun(Y) -> 
			  					    Y=/=Name4
			  				    end, Reversed_SessionNameList),
			  CompleteNotDeletedList = lists:delete(Name4, NotDeletedList),
			  %% ct:pal("~p", [CompleteNotDeletedList]),
			  check_rbs_units_is_deleted(CompleteNotDeletedList)
    		  end, 
		  Reversed_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add rbs units and commit, paralell req. <br/>
%% @spec nc_users_add_rbsUnit_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_paralell(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units and commit, paralell request!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    Self = self(),    
    PidList_Add = lists:map(fun(Name1) ->
				    spawn(fun() -> nc_add_rbsunit(Name1),
						   %% nc_commit([Name1]),
						   ct:pal("### Close session: ~p", [Name1]),
						   ok = ct_netconfc:close_session(Name1),
						   
						   Self ! done
					  end)
			    end,
			    ?NC_SessionNameList),

    %%%%
    %% Wait on answer from add request.
    %%%%
    lists:foreach(fun(_Pid) ->
			 receive
    			      done -> 			      
				 ok
			 after 50000 -> 
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList_Add),

    timer:sleep(30000),
    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    PidList_Delete = lists:map(fun(Name2) ->
				    spawn(fun() -> nc_delete_rbsunit(Name2),
						   %% nc_commit([Name2]),
						   ct:pal("### Close session: ~p", [Name2]),
						   ok = ct_netconfc:close_session(Name2),

						   Self ! done
					  end)
			    end,
			    ?NC_SessionNameList),

    %%%%
    %% Wait on answer from delete request.
    %%%%
    lists:foreach(fun(_Pid_1) ->
    			 receive
    			      done -> 			      
    				 ok
    			 after 50000 -> 
    				 ct:fail("Error no answer within time!", [])
    			 end
    		  end, PidList_Delete),

    timer:sleep(30000),
    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok =nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add, delete rbs units and commit, paralell req. <br/>
%% No rbsUnits should be created. <br/>
%% @spec nc_users_add_delete_rbsUnit_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_delete_rbsUnit_paralell(_Config) ->
    ct:pal("### Check several Netconf users, add delete rbs units and commit, paralell request!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    Self = self(),    
    PidList_Add = lists:map(fun(Name1) ->
				    spawn(fun() -> nc_add_rbsunit(Name1),
						   nc_delete_rbsunit(Name1),
						   %% nc_commit([Name1]),
						   ct:pal("### Close session: ~p", [Name1]),
						   ok = ct_netconfc:close_session(Name1),
						   
						   Self ! done
					  end)
			    end,
			    ?NC_SessionNameList),

    %%%%
    %% Wait on answer from add/delete request.
    %%%%
    lists:foreach(fun(_Pid) ->
			 receive
    			      done -> 			      
				 ok
			 after 50000 -> 
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList_Add),

    timer:sleep(30000),
    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok =nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add rbs units paralell. <br/>
%% Do commit after all rbsUnit is added. <br/>
%% @spec nc_users_add_rbsUnit_paralell_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_paralell_commit(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units paralell. Do commit after all rbsUnit is added!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    Self = self(),

    Slave = fun(Name, Loop) ->
		    receive
			add_cmd -> nc_add_rbsunit(Name),
				   Self ! add_done;
			%% commit_cmd -> nc_commit([Name]),
			%% 	      Self ! commit_done;
			close_cmd -> ct:pal("### Close session: ~p", [Name]),
				      ok = ct_netconfc:close_session(Name),
				      Self ! close_done;
			delete_cmd -> nc_delete_rbsunit(Name),
				      Self ! delete_done;
			exit_slave -> exit(normal)
		    after 300000 -> % If nothing happens within 5min .
			    ct:pal("Exit slave due to timeout",[]),
			    exit(normal)
		    end,
		    Loop(Name, Loop)
	    end,
    
    PidList = lists:map(fun(Name) ->
				spawn(fun() -> 
					      Slave(Name, Slave)
				      end)
				    
			end,
			?NC_SessionNameList),

    %%%%
    %% Send Add rbsUnit aralell request
    %%%%
    lists:foreach(fun(Pid1) ->
			  Pid1 ! add_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid2) ->
			  %% Pid2 ! commit_cmd
			  Pid2 ! close_cmd
		  end, PidList),
    
    %%%%
    %% Wait on answer from the request.
    %%%%
    lists:foreach(fun(_Pid_1) ->
			 receive
    			      add_done -> 			      
				 ok
			 after 50000 -> 
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList),

    lists:foreach(fun(_Pid_2) ->
			  receive
    			      %% commit_done -> 			      
			      %% 	 ok
			      close_done ->
				  ok
			  after 50000 -> 
				  ct:fail("Error no answer within time! ", [])
			  end
		  end, PidList),

    timer:sleep(30000),
    %% [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Send Delete rbs units paralell request
    %%%%
    lists:foreach(fun(Pid3) ->
			  Pid3 ! delete_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid4) ->
    			  %% Pid4 ! commit_cmd
			  Pid4 ! close_cmd
    		  end, PidList),

    %%%%
    %% Wait on answer from the Delete request.
    %%%%
    lists:foreach(fun(_Pid_3) ->
			 receive
    			      delete_done -> 			      
				 ok
			 after 50000 -> 
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList),

    %% Wait on answer from the Commit request.
    lists:foreach(fun(_Pid_4) ->
			  receive
    			      %% commit_done -> 
    			      %% 	 ok
			      close_done -> 
				  ok
			  after 50000 -> 
				  ct:fail("Error no answer within time!", [])
			  end
    		  end, PidList),

    timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    lists:foreach(fun(Pid5) ->
    			  Pid5 ! exit_slave
    		  end, PidList),

    %%%%
    %% Close session
    %%%%
    ok =nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  NC users add rbsUnits, then restart.<br/>
%% - Add rbs units, No commit.<br/>
%% - restart.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit<br/>
%% - restart again.<br/>
%% - Check rbsUnits still exist.<br/>
%% @spec nc_users_add_rbsUnit_restart(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_restart(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units. Do restart!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    
    %%%%
    %% Add rbs units
    %%%%
    lists:foreach(fun(Name1) ->
    			  nc_add_rbsunit(Name1)
    		  end, ?NC_SessionNameList),

    timer:sleep(5000),
    %%%%
    %% restart
    %%%%
    ct:pal("### restart!",[]),
    rct_rpc:call(rpc, init, restart, [], 2000),
    timer:sleep(10000),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Add rbs units again
    %%%%
    lists:foreach(fun(Name2) ->
    			  ok = nc_add_rbsunit(Name2),
    			  ct:pal("### Close session: ~p", [Name2]),
    			  ok = ct_netconfc:close_session(Name2),
    			  ct:pal("### Open session: ~p", [Name2]),
    			  {ok,_} = ct_netconfc:open(Name2,[])
    		  end, ?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    timer:sleep(5000),
    %%%%
    %% restart again
    %%%%
    ct:pal("### restart again!",[]),
    rct_rpc:call(rpc, init, restart, [], 2000),
    timer:sleep(10000),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name3) ->
    			  ok = nc_delete_rbsunit(Name3),
    			  ct:pal("### Close session: ~p", [Name3]),
    			  ok = ct_netconfc:close_session(Name3),
    			  ct:pal("### Open session: ~p", [Name3]),
    			  {ok,_} = ct_netconfc:open(Name3,[])
    		  end, ?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%%  NC users add rbsUnits, then perform init:reboot.<br/>
%% - Add rbs units, No commit.<br/>
%% - init:reboot.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit<br/>
%% - init:reboot again.<br/>
%% - Check rbsUnits still exist.<br/>
%% @spec nc_users_add_rbsUnit_init_reboot(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_init_reboot(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units. Do init:reboot!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    
    %%%%
    %% Add rbs units
    %%%%
    lists:foreach(fun(Name1) ->
    			  nc_add_rbsunit(Name1)
    		  end, ?NC_SessionNameList),

    %%%%
    %% init:reboot
    %%%%
    ct:pal("### init:reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    wait_for_node_go_down(ErlNode),
    check_for_node_restart(),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Add rbs units again
    %%%%
    lists:foreach(fun(Name2) ->
    			  ok = nc_add_rbsunit(Name2),
    			  ct:pal("### Close session: ~p", [Name2]),
    			  ok = ct_netconfc:close_session(Name2),
    			  ct:pal("### Open session: ~p", [Name2]),
    			  {ok,_} = ct_netconfc:open(Name2,[])
    		  end, ?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% init:reboot again
    %%%%
    ct:pal("### init:reboot again!",[]),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    wait_for_node_go_down(ErlNode),
    check_for_node_restart(),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name3) ->
    			  ok = nc_delete_rbsunit(Name3),
    			  ct:pal("### Close session: ~p", [Name3]),
    			  ok = ct_netconfc:close_session(Name3),
    			  ct:pal("### Open session: ~p", [Name3]),
    			  {ok,_} = ct_netconfc:open(Name3,[])
    		  end, ?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% One Netconf users, add, get, delete 10 rbs units. Do commit after each operation is done. <br/>
%% - Add rbs units, No commit.<br/>
%% - Commit.<br/>
%% - Delete all rbs units, No commit<br/>
%% - Commit.<br/>
%% @spec nc_user_add_10_rbsUnits_one_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_user_add_10_rbsUnits_one_commit(_Config) ->
    ct:pal("### One Netconf users, add, delete 10 rbsUnits. One commit after each operation is done !",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    [NC_session|_T]=?NC_SessionNameList,
    ok = nc_open_session([NC_session]),

    %%%%
    %% Add rbs units
    %%%%
    
    RbsUnits_Instances = lists:map(fun(Nr) ->
					   RbsUnitId = "nc" ++ integer_to_list(Nr),
					   UserLabel = "userLabel_" ++ RbsUnitId,
					   {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],[{'rbsUnitId',[],[RbsUnitId]}, {userLabel,[],[UserLabel]} ] }
					       
				   end, 
				   ?NrOfMoInstList),
    %% ct:pal("~p", [RbsUnits_Instances]),
    
    All_rbsUnits_List = lists:append([{'equipmentId',[],["1"]}], RbsUnits_Instances),
    %% ct:pal("~p", [All_rbsUnits_List]),

    ct:pal("Add 10 MO instances, commit after all is added.",[]),
    ok = ct_netconfc:edit_config(NC_session,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    						     [{managedElementId,[],["1"]},
    						      {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],    						       
    						       All_rbsUnits_List
    						      }]}),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session([NC_session]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session([NC_session]),
    lists:foreach(fun(X) ->
			  RbsUnitId = "nc" ++ integer_to_list(X),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  ct:pal("Check rbs unit created: ~p, Userlabel: ~p.",[RbsUnitId, UserLabel]),
			  
			  {ok,[{'ManagedElement',
			  	[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  	[{managedElementId,[],["1"]},
			  	 {'Equipment',
			  	  [{xmlns,
			  	    "urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
			  	  [{equipmentId,[],["1"]},
			  	   {'RbsUnit',
			  	    [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
			  	    [{rbsUnitId,[],[RbsUnitId]},
			  	     {userLabel,[],[UserLabel]},
			  	     {hwInstallStatus,[], [_]},
			  	     {administrativeState,[], [_]},
			  	     {operationalState,[], [_]},
			  	     {availState,[], [_]},
			  	     {detectedProductData, _, _},
			  	     {operationalLed, _, _},
			  	     {faultLed, _, _},				    
			  	     {statusLed, _, _},
			  	     {maintenanceLed, _, _}]
			  	   }]}]}]} =
			      ct_netconfc:get(NC_session, {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							    [{managedElementId,[],["1"]},
							     {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							      [{'equipmentId',[],["1"]},
							       {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
								[{rbsUnitId,[],[RbsUnitId]}]}
							      ]}]})
		  end, 
		  ?NrOfMoInstList),

    %%%%
    %% Delete rbs units
    %%%%
    RbsUnitsInstances = lists:map(fun(Nr1) ->
					  RbsUnitId = "nc" ++ integer_to_list(Nr1),
					  UserLabel = "userLabel_" ++ RbsUnitId,
					  {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"},
						      {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"}, 
						      {'nc:operation',"delete"}],
					   [{'rbsUnitId',[],[RbsUnitId]}, {userLabel,[],[UserLabel]} ] }
					      
				  end, 
				  ?NrOfMoInstList),
    %% ct:pal("~p", [RbsUnitsInstances]),
    
    AllrbsUnitsList = lists:append([{'equipmentId',[],["1"]}], RbsUnitsInstances),
    %% ct:pal("~p", [AllrbsUnitsList]),

    ct:pal("Delete 10 MO instances, commit after all is deleted.",[]),
    ok = ct_netconfc:edit_config(NC_session,running,{'ManagedElement',
    						 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    						     [{managedElementId,[],["1"]},
    						      {'Equipment',
    						       [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
    						       AllrbsUnitsList
    						      }]}),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session([NC_session]),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint),

    %%%%
    %% Just a check to se that no rbsUnits exist.
    %%%%
    G = rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 5000, noprint),
    case length(G) of
    	0 ->
    	    ok;
    	_ ->
    	    ct:pal("~p",[length(G)]),
    	    ct:fail("some RbsUnits still exist after delete!!")
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% One Netconf users, add rbs units. One with bad format. <br/>
%% Close session will result in that nothing is added, due to failed to add all rbsUnit. <br/>
%% - Add 10 rbs units and one rbsUnit with bad xml format.<br/>
%% - the operation will fail and nc session will be closed.<br/>
%% @spec nc_user_fail_to_add_attribute(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_user_fail_to_add_attribute(_Config) ->

    ct:pal("### One Netconf users, add, 11 rbsUnits. One with bad xml format, NO rbsUnits shall exist after commit !",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    [NC_session|_T]=?NC_SessionNameList,
    ok = nc_open_session([NC_session]),

    %%%%
    %% Add rbs units
    %%%%
    RbsUnits_Instances = lists:map(fun(Nr) ->
					   RbsUnitId = "nc" ++ integer_to_list(Nr),
					   UserLabel = "userLabel_" ++ RbsUnitId,
					   {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],[{'rbsUnitId',[],[RbsUnitId]}, {userLabel,[],[UserLabel]} ] }
					       
				   end, 
				   ?NrOfMoInstList),
    %% ct:pal("~p", [RbsUnits_Instances]),
    
    All_rbsUnits_List = lists:append([{'equipmentId',[],["1"]}], RbsUnits_Instances),
    %% ct:pal("~p", [All_rbsUnits_List]),

    Fail_rbsUnits_List=lists:append(All_rbsUnits_List, [{'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
					       [{rbsUnitId,[],["nc11"]},{userLabel,[],[userLabel_nc11]}]}]),
    %% ct:pal("~p", [Fail_rbsUnits_List]),

    ct:pal("Add 10 correct MO instances and 1 bad attribute.",[]),

    {error,[{'error-type',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
	     ["application"]},
	    {'error-tag',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
	     ["unknown-attribute"]},
	    {'error-severity',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
	     ["error"]},
	    {'error-info',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
	     [{'bad-attribute',[],["userLabel_nc11"]},
	      {'bad-element',[],["RbsUnit"]}]},
	    {'error-message',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"},
			      {'xml:lang',"en"}],
	     ["Failed to add attribute"]}]} =
	ct_netconfc:edit_config(NC_session,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    						     [{managedElementId,[],["1"]},
    						      {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],    						       
						       Fail_rbsUnits_List
						      }]}),

 
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint),

    %%%%
    %% Just a check to se that no rbsUnits exist.
    %%%%
    G = rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 5000, noprint),
    case length(G) of
    	0 ->
    	    ok;
    	_ ->
    	    ct:pal("~p",[length(G)]),
    	    ct:fail("some RbsUnits exist !!")
    end,

    ok.


%%--------------------------------------------------------------------
%% @doc
%% One Netconf users, add rbs units. One with bad instanse name. <br/>
%% Close session will result in that nothing is added, due to failed to add all rbsUnit. <br/>
%% - Add 10 rbs units and one bad.<br/>
%% - Nothing shall be added.<br/>
%% - the operation will fail and nc session will be closed.<br/>
%% @spec nc_user_fail_to_add_struct_attribute(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_user_fail_to_add_struct_attribute(_Config) ->

    ct:pal("### One Netconf users, add 10 rbsunits and 1 with bad instanse name in xml, NO rbsUnits shall exist after commit !",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    [NC_session|_T]=?NC_SessionNameList,
    ok = nc_open_session([NC_session]),

    %%%%
    %% Add rbs units
    %%%%
    RbsUnits_Instances = lists:map(fun(Nr) ->
					   RbsUnitId = "nc" ++ integer_to_list(Nr),
					   UserLabel = "userLabel_" ++ RbsUnitId,
					   {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],[{'rbsUnitId',[],[RbsUnitId]}, {userLabel,[],[UserLabel]} ] }
					       
				   end, 
				   ?NrOfMoInstList),
    %% ct:pal("~p", [RbsUnits_Instances]),
    
    All_rbsUnits_List = lists:append([{'equipmentId',[],["1"]}], RbsUnits_Instances),
    %% ct:pal("~p", [All_rbsUnits_List]),
    
    %%%%
    %% The fault is 'RbsUnitt' is bad instance name.
    %%%%
    Fail_rbsUnits_List=lists:append(All_rbsUnits_List, [{'RbsUnitt',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
					       [{rbsUnitId,[],["nc11"]},{userLabel,[],[userLabel_nc11]}]}]),
    %% ct:pal("~p", [Fail_rbsUnits_List]),

    ct:pal("Add 10 correct MO instances and 1 bad instance name.",[]),

    {error,[{'error-type',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
                      ["application"]},
        {'error-tag',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
                     ["unknown-attribute"]},
        {'error-severity',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
                          ["error"]},
        {'error-info',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
                      [{'bad-attribute',[],["RbsUnitt"]},
                       {'bad-element',[],["Equipment"]}]},
        {'error-message',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"},
                          {'xml:lang',"en"}],
	 ["Failed to add struct attribute"]}]} =
	ct_netconfc:edit_config(NC_session,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    						     [{managedElementId,[],["1"]},
    						      {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],    						       
						       Fail_rbsUnits_List
						      }]}),

 
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 5000, noprint),

    %%%%
    %% Just a check to se that no rbsUnits exist.
    %%%%
    G = rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 5000, noprint),
    case length(G) of
    	0 ->
    	    ok;
    	_ ->
    	    ct:pal("~p",[length(G)]),
    	    ct:fail("some RbsUnits exist !!")
    end,

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  NC users add rbsUnits, then reboot.<br/>
%% - Add rbs units, No commit.<br/>
%% - Reboot.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit<br/>
%% - Reboot again.<br/>
%% - Check rbsUnits still exist.<br/>
%% @spec nc_users_add_rbsUnit_reboot(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_reboot(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units. Do reboot!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    lists:foreach(fun(Name1) ->
    			  nc_add_rbsunit(Name1)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up session on client side, after reboot.
    %%%%
    lists:foreach(fun(Name2) ->
			  ct:pal("### Close hanging session on client: ~p", [Name2]),
			  ct_netconfc:close_session(Name2)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),


    %%%%
    %% Add rbs units again
    %%%%
    lists:foreach(fun(Name3) ->
    			  ok = nc_add_rbsunit(Name3),
    			  ct:pal("### Close session: ~p", [Name3]),
    			  ok = ct_netconfc:close_session(Name3),
    			  ct:pal("### Open session: ~p", [Name3]),
    			  {ok,_} = ct_netconfc:open(Name3,[])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% reboot again
    %%%%
    ct:pal("### reboot again!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up session on client side, after reboot.
    %%%%
    lists:foreach(fun(Name4) ->
			  ct:pal("### Close hanging session on client: ~p", [Name4]),
			  ct_netconfc:close_session(Name4)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name5) ->
    			  ok = nc_delete_rbsunit(Name5),
    			  ct:pal("### Close session: ~p", [Name5]),
    			  ok = ct_netconfc:close_session(Name5),
    			  ct:pal("### Open session: ~p", [Name5]),
    			  {ok,_} = ct_netconfc:open(Name5,[])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.




%%--------------------------------------------------------------------
%% @doc
%%  NC users add rbsUnits, then power off/on.<br/>
%% - Add rbs units, No commit.<br/>
%% - Power off/on.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check rbsUnits exist or no exist. 
%%   It depends if data has been written to disc before power off.<br/>
%% - Add rbs units, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check rbsUnits exist.<br/>
%% @spec nc_users_add_rbsUnit_power(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_power(_Config) ->
    ct:pal("### Check several Netconf users, add rbs units. Do power off/on!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    lists:foreach(fun(Name1) ->
    			 ok = nc_add_rbsunit(Name1)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Power off/on 
    %%%%
    ct:pal("### power off/on!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up session on client side, after power off/on.
    %%%%
    lists:foreach(fun(Name2) ->
    			  ct:pal("### Close hanging session on client: ~p", [Name2]),
			  ct_netconfc:close_session(Name2)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Add RbsUnits and commit, power off directly, then data could exist in disc. depends on how many rbsUnits.
    %%%%

    %%%%
    %% Add rbs units again
    %%%%
    lists:foreach(fun(Name3) ->
    			  ok = nc_add_rbsunit(Name3),
    			  ct:pal("### Close session: ~p", [Name3]),
    			  ok = ct_netconfc:close_session(Name3),
    			  ct:pal("### Open session: ~p", [Name3]),
    			  {ok,_} = ct_netconfc:open(Name3,[])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Power off/on again
    %%%%
    ct:pal("### power off/on again!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up session on client side, after power off/on.
    %%%%
    lists:foreach(fun(Name4) ->
			  ct:pal("### Close hanging session on client: ~p", [Name4]),
			  ct_netconfc:close_session(Name4)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    check_if_rbs_units_exist_or_removed(?NC_SessionNameList),

    %%%%
    %% Add RbsUnits and commit, power off after a few seconds so data has been written to disc.
    %%%%

    %%%%
    %% Add rbs units again
    %%%%
    lists:foreach(fun(Name5) ->
    			  ok = nc_add_rbsunit(Name5),
    			  ct:pal("### Close session: ~p", [Name5]),
    			  ok = ct_netconfc:close_session(Name5),
    			  ct:pal("### Open session: ~p", [Name5]),
    			  {ok,_} = ct_netconfc:open(Name5,[])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),
    %% sleep to be sure that data exist on disc.
    timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),

    %%%%
    %% Power off/on again, 3:rd time.
    %%%%
    ct:pal("### power off/on, 3:rd time!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up session on client side, after power off/on.
    %%%%
    lists:foreach(fun(Name6) ->
			  ct:pal("### Close hanging session on client: ~p", [Name6]),
			  ct_netconfc:close_session(Name6)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name7) ->
    			  ok = nc_delete_rbsunit(Name7),
    			  ct:pal("### Close session: ~p", [Name7]),
    			  ok = ct_netconfc:close_session(Name7),
    			  ct:pal("### Open session: ~p", [Name7]),
    			  {ok,_} = ct_netconfc:open(Name7,[])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  NC users add rbsUnits, then kill_com.<br/>
%% - Add rbs units, No commit.<br/>
%% - kill com.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit<br/>
%% - kill com again.<br/>
%% - Check rbsUnits still exist.<br/>
%% @spec nc_users_add_rbsUnit_kill_com(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_rbsUnit_kill_com(_Config) ->
  
    ct:pal("### Check several Netconf users, add rbs units. Kill com!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    lists:foreach(fun(Name1) ->
    			  nc_add_rbsunit(Name1)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Get COM pid.
    %%%%
    ComPid = wait_for_com_to_start(),
    ct:pal("ComPid: ~p",[ComPid]),

    %%%%
    %% Kill Com.
    %%%%
    ct:pal("kill Com. ",[]),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ ComPid], 200),
    timer:sleep(5000),
    %%%%
    %% Check that com is restarted with a new pid.
    %%%%
    NewComPid = wait_for_com_to_restart(ComPid),
    ct:pal("NewComPid: ~p",[NewComPid]), 

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),
    %% test_server:break("break"),

    %%%%
    %% Add rbs units again
    %%%%
    lists:foreach(fun(Name2) ->
    			  ok = nc_add_rbsunit(Name2),
    			  ct:pal("### Close session: ~p", [Name2]),
    			  ok = ct_netconfc:close_session(Name2),
    			  ct:pal("### Open session: ~p", [Name2]),
    			  {ok,_} = ct_netconfc:open(Name2,[])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Kill Com again
    %%%%
    ct:pal("kill com again. ",[]),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ NewComPid], 200),

    %%%%
    %% Check that com is restarted with a new pid.
    %%%%
    New_ComPid = wait_for_com_to_restart(NewComPid),
    ct:pal("NewComPid: ~p",[New_ComPid]),    

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = check_rbs_units_is_added(?NC_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    lists:foreach(fun(Name3) ->
    			  ok = nc_delete_rbsunit(Name3),
    			  ct:pal("### Close session: ~p", [Name3]),
    			  ok = ct_netconfc:close_session(Name3),
    			  ct:pal("### Open session: ~p", [Name3]),
    			  {ok,_} = ct_netconfc:open(Name3,[])
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = check_rbs_units_is_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%% Internal functions
%%--------------------------------------------------------------------
%% @doc 
%% Test netconf create by creating a rbsUnit in FEQM.<br/>
%% @end
%%--------------------------------------------------------------------
nc_add_rbsunit(NC_hookName) ->
    RbsUnitId = atom_to_list(NC_hookName),
    UserLabel = "userLabel_" ++ RbsUnitId,
    ct:pal("### Create RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),

    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							  [{rbsUnitId,[],[RbsUnitId]},
							   {userLabel,[],[UserLabel]} ]}]}]}),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Test netconf delete by deleteing a rbsUnit in FEQM.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_delete_rbsunit(NC_hookName) ->
    RbsUnitId =atom_to_list(NC_hookName),
    UserLabel = "userLabel_" ++ RbsUnitId,
    ct:pal("### Delete RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),

    ok = ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
						      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						      [{managedElementId,[],["1"]},
						       {'Equipment',
							[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							[{'equipmentId',[],["1"]},
							 {'RbsUnit',
							  [{xmlns,"urn:com:ericsson:ecim:rbsunit"},
							   {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
							   {'nc:operation',"delete"}],
							  [{rbsUnitId,[],[RbsUnitId]},
							   {userLabel,[],[UserLabel]}]}]}]}),
    ok.

%%%%%%%%%%%%%%%%%
%% Use in cleanup
%% Need to open/close after each delete.
%%%%%%%%%%%%%%%%%
nc_delete_rbsunits(NC_hookName, NC_SessionNameList) ->
    lists:foreach(fun(SesionName) ->
			  RbsUnitId =atom_to_list(SesionName),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  ct_netconfc:open(NC_hookName,[]),
			  %% ct:pal("### Delete RBS Unit id: ~p, userlabel: ~p", [RbsUnitId, UserLabel]),
			  ct_netconfc:edit_config(NC_hookName,running,{'ManagedElement',
								       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
								       [{managedElementId,[],["1"]},
									{'Equipment',
									 [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
									      [{'equipmentId',[],["1"]},
									       {'RbsUnit',
										[{xmlns,"urn:com:ericsson:ecim:rbsunit"},
										 {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
										 {'nc:operation',"delete"}],
										[{rbsUnitId,[],[RbsUnitId]},
										 {userLabel,[],[UserLabel]}]}]}]}),
			  ct_netconfc:close_session(NC_hookName)
		  end,
		  NC_SessionNameList).


%%--------------------------------------------------------------------
%% @doc 
%% Open netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_open_session(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### Open Name: ~p", [Name]),
    			  {ok,_} = ct_netconfc:open(Name, [])
    		  end, 
		  NC_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Close netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_close_session(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### CloseName: ~p", [Name]),
			  ok = ct_netconfc:close_session(Name)
    		  end, 
    		  NC_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Commit, close sessions and open sessions. <br/>
%% @end
%%--------------------------------------------------------------------
%% nc_commit(NC_SessionNameList) ->
%%     lists:foreach(fun(Name) ->
%% 			  ct:pal("### Close session: ~p", [Name]),
%% 			  ok = ct_netconfc:close_session(Name),
%% 			  ct:pal("### Open session: ~p", [Name]),
%% 			  {ok,_} =  ct_netconfc:open(Name,[])
%% 		  end,
%% 		  NC_SessionNameList),
%%     ok.
    
%%--------------------------------------------------------------------
%% @hidden
%% delete_RbsUnits_List(NC_SessionNameList)
%%--------------------------------------------------------------------
delete_RbsUnits_List(NC_SessionNameList) ->
    lists:foreach(fun(Client) ->
		     ok = nc_delete_rbsunit(Client),
		     ct:pal("### Close session: ~p", [Client]),
		     ok = ct_netconfc:close_session(Client),
		     ct:pal("### Open session: ~p", [Client]),
		     {ok,_} = ct_netconfc:open(Client,[])
    		  end, 
    		  NC_SessionNameList).

%%--------------------------------------------------------------------
%% @hidden
%% check_rbs_units_is_added(NC_SessionNameList)
%%--------------------------------------------------------------------
check_rbs_units_is_added(NC_SessionNameList) ->
    lists:foreach(fun(NC_hookName) ->
			  ct:pal("Check rbs unit created, id: ~p.",[NC_hookName]),

			  RbsUnitId = atom_to_list(NC_hookName),
			  UserLabel = "userLabel_" ++ RbsUnitId,

			  {ok,[{'ManagedElement',
			  	[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  	[{managedElementId,[],["1"]},
			  	 {'Equipment',
			  	  [{xmlns,
			  	    "urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
			  	  [{equipmentId,[],["1"]},
			  	   {'RbsUnit',
			  	    [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
			  	    [{rbsUnitId,[],[RbsUnitId]},
			  	     {userLabel,[],[UserLabel]},
			  	     {hwInstallStatus,[], [_]},
			  	     {administrativeState,[], [_]},
			  	     {operationalState,[], [_]},
			  	     {availState,[], [_]},
			  	     {detectedProductData, _, _},
			  	     {operationalLed, _, _},
			  	     {faultLed, _, _},				    
			  	     {statusLed, _, _},
			  	     {maintenanceLed, _, _}]
			  	   }]}]}]} =
			      ct_netconfc:get(NC_hookName, {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							    [{managedElementId,[],["1"]},
							     {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							      [{'equipmentId',[],["1"]},
							       {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
								[{rbsUnitId,[],[RbsUnitId]}]}
							      ]}]})
		  end, 
		  NC_SessionNameList),
    ok.


%%-------------------------------------------------------------------- 
%% @hidden
%% check_rbs_units_is_not_commited(NC_SessionNameList)
%%-------------------------------------------------------------------- 
check_rbs_units_is_not_commited(NC_SessionNameList) ->
    lists:foreach(fun(NC_hookName) ->
			  ct:pal("Check rbs unit id: ~p, is deleted.",[NC_hookName]),
			  RbsUnitId = atom_to_list(NC_hookName),

			  {ok,[{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  	[{managedElementId,[],["1"]},
			  	 {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
			  	  [{equipmentId,[],["1"]},
			  	   {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
			  	    [{rbsUnitId,[],[RbsUnitId]},
			  	     {administrativeState,[{unset,"true"}],[]} 
			  	   ]}]}]}]} =
			      ct_netconfc:get(NC_hookName,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							   [{managedElementId,[],["1"]},
							    {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							     [{'equipmentId',[],["1"]},
							      {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							       [{rbsUnitId,[],[RbsUnitId]}, 
								{administrativeState,[],[]} ]}
							     ]}]})
    		  end, 
    		  NC_SessionNameList),
    ok.


%%-------------------------------------------------------------------- 
%% @hidden
%% check_rbs_units_is_deleted(NC_SessionNameList)
%%-------------------------------------------------------------------- 
check_rbs_units_is_deleted(NC_SessionNameList) ->
    lists:foreach(fun(NC_hookName) ->
			  ct:pal("Check rbs unit id: ~p, is deleted.",[NC_hookName]),
			  RbsUnitId = atom_to_list(NC_hookName),

			  {error, [_, 
			  	   _, 
			  	   _,
			  	   {'error-message', _,  [Res]}
			  	  ]} =
			      ct_netconfc:get(NC_hookName,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							   [{managedElementId,[],["1"]},
							    {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							     [{'equipmentId',[],["1"]},
							      {'RbsUnit',  [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							       [{rbsUnitId,[],[RbsUnitId]}]}
							     ]}]}),

			  case re:run(Res, "MO: ManagedElement=1,Equipment=1,RbsUnit=" ++ RbsUnitId ++ " is not available [(]no instance[)].") of
			      {match, _} ->
			  	  ct:pal("RbsUnitId: ~p is deleted." ,[RbsUnitId]);
			      nomatch ->
			  	  ct:fail("rbs units still exist")
			  end
    		  end, 
    		  NC_SessionNameList),
    ok.


%%-------------------------------------------------------------------- 
%% @hidden
%% open_session_after_restart(NC_SessionNameList)
%%-------------------------------------------------------------------- 
open_session_after_restart(NC_SessionNameList) ->
    open_session_after_restart(NC_SessionNameList, 60000).

open_session_after_restart(_NC_SessionNameList, Timeout) when Timeout < 2000 ->
    ct:fail("Netconf not started within max timeout after restart.");

open_session_after_restart([H|T], Timeout) ->
    case ct_netconfc:open(H, []) of
	{error,_Cause} ->
	    %% ct:pal("### After restart, Fail to open Name: ~p, Cause: ~p", [H,Cause]),
	    timer:sleep(1000),
	    open_session_after_restart([H|T], Timeout-1000);
	{ok,_} ->
	    ct:pal("### Open Name: ~p, is done.", [H]),
	    lists:foreach(fun(Client) ->
				  ct:pal("### Open Name: ~p", [Client]),
				  {ok,_} = ct_netconfc:open(Client, [])
			  end, T)
    end.

%%-------------------------------------------------------------------- 
%% @hidden
%% check_if_rbs_units_exist_or_removed(NC_SessionNameList)
%%-------------------------------------------------------------------- 
check_if_rbs_units_exist_or_removed(NC_SessionNameList) ->
    %% Nr_NC_SessionName = length(NC_SessionNameList),
    NrOfRbsUnits_afterPowerOffOn = length(rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 100)),
    ct:pal("ets: ~p", [rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 100)]),

    %% case ?NrOfNetConfUsers of
    %% 	Val when Val < 5 -> %% RbsUnits Always deleted from disc after power off/on
    %% 	    ct:pal("### RbsUnits shall not exist on disc.", []),
    %% 	    check_rbs_units_is_deleted(NC_SessionNameList);
    %% 	Val when Val < 20 -> %% This numbers of RbsUnits might result in deleted or existing on disc after power off/on
    %% 	    ct:pal("### RbsUnits could exist on disc.", []),
    %% 	    case rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 100) of
    %% 		[] ->
    %% 		    check_rbs_units_is_deleted(NC_SessionNameList);
    %% 		_ -> 
    %% 		    ExistingRbsUnitsList = lists:sublist(NC_SessionNameList, NrOfRbsUnits_afterPowerOffOn),
    %% 		    ct:pal("ExistingRbsUnitsList: ~p",[ExistingRbsUnitsList]),
		    
    %% 		    DeletedRbsUnitsList = lists:sublist(NC_SessionNameList, NrOfRbsUnits_afterPowerOffOn+1, ?NrOfNetConfUsers),
    %% 		    ct:pal("DeletedRbsUnitsList: ~p",[DeletedRbsUnitsList]),
		    
    %% 		    check_rbs_units_is_added(ExistingRbsUnitsList),
    %% 		    check_rbs_units_is_deleted(DeletedRbsUnitsList),
    %% 		    %% Clean up.
    %% 		    delete_RbsUnits_List(ExistingRbsUnitsList)
    %% 	    end;
    %% 	_ -> %% This numbers of RbsUnits always result in deleted and existing on disc after power off/on
	    ct:pal("### Some RbsUnits should exist on disc.", []),
	    ExistingRbsUnitsList = lists:sublist(NC_SessionNameList, NrOfRbsUnits_afterPowerOffOn),
	    ct:pal("ExistingRbsUnitsList: ~p",[ExistingRbsUnitsList]),
    
	    DeletedRbsUnitsList = lists:sublist(NC_SessionNameList, NrOfRbsUnits_afterPowerOffOn+1, ?NrOfNetConfUsers),
	    ct:pal("DeletedRbsUnitsList: ~p",[DeletedRbsUnitsList]),
    
	    check_rbs_units_is_added(ExistingRbsUnitsList),
	    check_rbs_units_is_deleted(DeletedRbsUnitsList),
	    
	    %% Clean up.
	    delete_RbsUnits_List(ExistingRbsUnitsList). 
    %% end.

%%-------------------------------------------------------------------- 
%% @hidden
%% wait_for_com_to_restart(ComPid)
%%-------------------------------------------------------------------- 
wait_for_com_to_restart(ComPid)->
    wait_for_com_to_restart(ComPid, 60000).

wait_for_com_to_restart(_ComPid, Timeout) when Timeout < 3000 ->
    ct:fail("New COM process not started up within max timeout after restart.");

wait_for_com_to_restart(ComPid, Timeout)->
    NewComPid = wait_for_com_to_start(),
    %% ct:pal("### NewComPid: ~p, old com pid: ~p",[NewComPid, ComPid]), 
    case ComPid == NewComPid of
    	true ->
	    timer:sleep(2000),
	    wait_for_com_to_restart(ComPid, Timeout-2000);
    	false ->
	    NewComPid
    end.

%%-------------------------------------------------------------------- 
%% @hidden
%% wait_for_com_to_start()
%%-------------------------------------------------------------------- 
wait_for_com_to_start() ->
    wait_for_com_to_start(90000).

wait_for_com_to_start(Timeout) when Timeout < 50000 ->
    ct:fail("COM not started within max timeout.");

wait_for_com_to_start(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 1000)  of
	[] ->
	    timer:sleep(5000),
	    wait_for_com_to_start(Timeout - 5000);
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(5000),
	    wait_for_com_to_start(Timeout - 5000);
	Com ->
	    %% remove \n from the received list.
	    [ComPid|_] = string:tokens(Com, "\n "),
	    ComPid
    end.


%% ===========================================================================
%% @doc
%% Check for Application to be started. <br/>
%% Wait for test_app is started. <br/>
%% @spec wait_for_appl_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_appl_started() ->
    wait_for_appl_started(60000).

wait_for_appl_started(Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout after restart.");

wait_for_appl_started(Timeout) ->
    case rct_rpc:call(rpc, appmServer, get_apps, [], 250) of
    	{badrpc, _} ->
	    timer:sleep(250),
	    wait_for_appl_started(Timeout - 250);
	AppProplist ->
    	    case proplists:lookup("test_app", AppProplist) of
    		{"test_app", _} ->
		    End = erlang:timestamp(),
		    End;
    		_ ->
		    timer:sleep(250),
		    wait_for_appl_started(Timeout - 250)
    	    end
    end.

%%--------------------------------------------------------------------
%% @doc 
%% wait_for_node_go_down. <br/>
%% @end
%%--------------------------------------------------------------------
wait_for_node_go_down(ErlNode) ->
    wait_for_node_go_down(ErlNode, 60000).
wait_for_node_go_down(_ErlNode, Timeout) when Timeout < 500 ->
    ct:fail("Node has not gone down !");
wait_for_node_go_down(ErlNode, Timeout) ->
        case net_adm:ping(ErlNode) of
	pang ->
	    ok;
	_Res ->
	    %% ct:pal("Ping res: ~p", [Res]),
	    timer:sleep(3000),
	    wait_for_node_go_down(ErlNode, Timeout-3000)
    end.

%%--------------------------------------------------------------------
%% @doc 
%% do_init_reboot. <br/>
%% @end
%%--------------------------------------------------------------------
do_init_reboot() ->
    do_init_reboot(3).
do_init_reboot(0) ->
    ct:fail("Node has not rebooted, tried 3 times!!");
do_init_reboot(Nr) ->
    case rct_rpc:call(rpc, init, reboot, [], 2000) of
	ok ->
	    ok;
	{badrpc,nodedown} ->
	    ct:pal("init:reboot() did not take effect! try again",[]),
	    timer:sleep(2000),
	    do_init_reboot(Nr-1)
    end.


%% ===========================================================================
%% @doc
%% Check that node restats. <br/>
%% Wait for a specific string arrives in the consol printouts. <br/>
%% @spec check_for_node_restart() -> ok
%% @end
%% ===========================================================================
check_for_node_restart() ->
    ct:pal("### Check that node restarts.", []),
    %% Check that node has restarted.
    BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    RestartStr = case BoardType of
		     BoardType when BoardType == "dus4101";
				    BoardType == "duw4101" ->
			 "2:nd Stage Boot Loader";
		     _ ->
			 "Ericsson Version: 2/CXC1736593/" % tcu03, dus5201, dus3201?
		       end,
    ok = check_node_is_starting(RestartStr).

check_node_is_starting(RestartStr)->
    ct:log("RestartStr: ~p", [RestartStr]),
    case ct_telnet:expect(console, RestartStr, [{timeout,30000}, no_prompt_check]) of
	{ok, _} ->
	    ok;
	Res ->
	    ct:pal("### Res: ~w", [Res]),
	    ct:fail("Node has not restarted within expected time!.")
    end.

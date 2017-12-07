%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	netconf_users_testclass1_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R10A/1
%%% 
%%% @doc == Several users using netconf operations.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%% 
%%% @end

-module(netconf_users_testclass1_SUITE).
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
%%% R2A/2      2013-11-14 etxivri     Created, Ported TC from netconf_users_SUIT
%%%                                   Due to model in FEQM shall be removed.
%%% R2A/3      2013-11-20 etxivri     Clean up printouts of rbs units.
%%% R2A/4      2013-11-21 etxivri     Skip a match when open session due to 
%%%                                   it could already be open.
%%% R2A/5      2013-12-06 etxivri     Minor update to make a TC mor robust.
%%% R2A/6      2013-12-13 etxivri     Incresed NrOfNetConfUsers due sometimes 
%%%                                   power and kill com TCs fails due to 
%%%                                   check expect some instances to be created.
%%% R2A/7      2014-09-01 etxivri     Update in tcu power tc, to handle no 
%%%                                   instances exist after second power off/on.
%%%                                   Incr timeout when wait for appl to star.
%%% R2A/8      2014-01-16 etxkols     Support for dus5201.
%%% R2A/9      2014-01-28 etxivri     Update a TC to handle sa_ais_err_exist.
%%% R2A/9      2014-01-30 etxivri     Increased timeout when check netconf 
%%%                                   is started after restart.
%%% R2A/11     2014-02-07 etxivri     Increased some of the timeouts in receive.
%%% R2A/12     2014-02-09 etxivri     Update to make it more robust.
%%% R2A/13     2014-02-27 etxivri     Updatate paralell_get_config to make it 
%%%                                   more robust.
%%% R2A/14     2014-03-13 eransbn     New test case nc_users_add_testclass1_kill_session
%%% R2A/16     2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/17      2014-07-02 etxivri    fix a compile warning and make it mor robust.
%%% R2A/19      2014-07-08 etxivri    Update due to limit of nr off ssh session
%%%                                   is set to 5 at same time. 
%%%                                   One session is check session.
%%% R2A/20      2014-07-10 etxivri    A try to make it more robust.
%%% R2A/21      2014-09-03 etxivri    A try to make it more robust.
%%% R2A/22      2014-09-11 etxivri    A try to make it more robust.
%%% R3A/1       2015-01-28 etxivri    Update error filter
%%% R4A/1       2015-07-02 etxmlar    Updated nc_user_fail_to_add_attribute,
%%%                                   changed 'error-message'
%%% R4A/2       2015-07-14 etxivri    Update idle_timeout tc to be more robust.
%%% R4A/3       2015-08-27 etxivri    Adaption to new testmom
%%% R4A/4       2015-09-08 etxivri    Update idle tc due to changed timeout.
%%% R4A/5       2015-09-24 etxivri    Update check that node starts to  restart.
%%% R4A/6       2015-09-30 etxmlar    now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/7      2015-10-20 etxivri    Add sleep 35 sec in restart tc due to new
%%%                                  behaviour when write conf to disc.
%%% R4A/8      2015-10-23 etxivri    Remove init restart to be used.
%%% R5A/1      2016-02-03 etxivri    Update in power tc to be more robust.
%%% R5A/2      2016-02-05 etxivri    Update to restart using appm instead for init:.
%%% R5A/3      2016-02-19 etxkols    Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R5A/4      2016-05-02 etxivri    Update due to new behaviour.   
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% R10A/1     2017-06-29 etxivri    Update reboot cmd.
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
	 nc_users_add_testclass1/1,
	 nc_users_add_testclass1_commit/1,
	 nc_users_add_testclass1_commit_reversed_order/1,
	 nc_user_fail_to_add_attribute/1,
	 nc_user_fail_to_add_struct_attribute/1,
	 nc_users_add_testclass1_paralell/1,
	 nc_users_add_delete_testclass1_paralell/1,
	 nc_users_add_testclass1_paralell_commit/1,
	 nc_users_add_testclass1_restart/1,
	 nc_users_add_testclass1_init_reboot/1,
	 nc_users_add_testclass1_reboot/1,
	 nc_users_add_testclass1_power/1,
	 nc_users_add_testclass1_kill_com/1,
	 nc_users_add_testclass1_kill_session/1,
	 get_me_id/1
	]).

%-define(NrOfNetConfUsers, 1).
-define(NrOfNetConfUsers, 5). %% One session is check session.
%% -define(NrOfNetConfUsers, 20).
%% -define(NrOfNetConfUsers, 30).
%% -define(NrOfNetConfUsers, 50).

-define(NC_SessionNameList, 
	[list_to_atom(
	   "nc"++integer_to_list(N)) || N <- lists:seq(1,?NrOfNetConfUsers)]).

-define(NrOfMoInstList, lists:seq(1,10) ).

-define(ATTR, "12345").
%% -define(CHECK_SESSION, check_session).
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of NetconfHooks touples with differents Names.
    NetconfHooks = 
	[{rct_netconf, 
	  list_to_atom(
	    "nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    %% ct:pal("NetconfHooks: ~p",[NetconfHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_core,[]},
		 %% {rct_netconf, check_session},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       ["The job was brutally killed - "
						"exiting",
						"sa_ais_err_exist"]}}]}} | 
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
    	    ct:pal("Testcase failed due to: ~p.  \n"
    		   "Clean up added instances units", [Reason]),

    	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 
    			     10000, noprint),
    	    C = rct_rpc:call(rpc, os, cmd, ["pgrep netconf -u $USER"], 
    			     10000, noprint),
    	    CC = length(string:tokens(C,"\n")),
    	    ct:pal("### Com: ~p",[A]),
    	    ct:pal("### Netconf: ~p",[CC]),	    

    	    %%%%
    	    %% Open a session and clean up instances, close all session.
            %%%%
    	    try
    	    	lists:foreach(fun(Name) -> nc_open(ct_netconfc:open(Name, []), 
    	    					   Name) 
    	    		      end, ?NC_SessionNameList)
    	    catch throw:{?MODULE, found} ->
    	    	    ok
    	    end,

            %%%%
    	    %% Commit, close sessions.
            %%%%
    	    lists:foreach(fun(Name1) ->
    	    			  ct_netconfc:close_session(Name1)
    	    		  end, 
    	    		  ?NC_SessionNameList)
	    		 
    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),

    ok.

nc_open({error,{connection_exists,_}}, Name) ->
    % Clean up at our client, if netconf process was killed on node, 
    ct_netconfc:close_session(Name), 
    % Then set up again.
    ct_netconfc:open(Name, []), 
    nc_cleanup_testclass1(Name, ?NC_SessionNameList),
    throw({?MODULE, found});
nc_open({ok,_}, Name) ->
    nc_cleanup_testclass1(Name, ?NC_SessionNameList),
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
     nc_users_add_testclass1,
     nc_users_add_testclass1_commit,
     nc_users_add_testclass1_commit_reversed_order,
     nc_user_fail_to_add_attribute,
     nc_user_fail_to_add_struct_attribute,
     nc_users_add_testclass1_paralell,
     nc_users_add_delete_testclass1_paralell,
     nc_users_add_testclass1_paralell_commit,
     %% nc_users_add_testclass1_restart,
     nc_users_add_testclass1_init_reboot,
     nc_users_add_testclass1_reboot,
     nc_users_add_testclass1_power,
     nc_users_add_testclass1_kill_com,
     nc_users_add_testclass1_kill_session
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].

get_me_id(_Config) ->
    {ok, _} = ct_netconfc:open(nc1, []),

    A=	ct_netconfc:get(nc1,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],[]}]
			}),

    ok = ct_netconfc:close_session(nc1),
    ct:pal("A: ~p", [A]),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Verify that a session that not subscribes on event notifications <br/>
%% will be closed after 5min if no data has been sent.  <br/>
%% @spec nc_sessions_idle_timeout(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_sessions_idle_timeout(_Config) ->
    ct:pal("### Check that a session is closed after 5min, if idle!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),

    %%%%
    %% Set connectionTimeOut in libcom_netconf_agent.cfg from 3600 to 300 sec,
    %% if it match.
    %%%%
    ct:pal("# Set netconf connectionTimeOut to 5min in libcom_netconf_agent.cfg"),
    A = rct_rpc:call(rpc, os, cmd,
		     ["cat /home/sirpa/releases/R*/comte/libcom_netconf_agent.cfg"], 10000, noprint),
    ct:log("# Orginal libcom_netconf_agent.cfg:~n~p",[clean_up_str(A)]),

    SedCmd = "sed -i 's/3600/300/g' /home/sirpa/releases/R*/comte/libcom_netconf_agent.cfg",
    rct_rpc:call(rpc, os, cmd, [SedCmd], 10000, noprint),

    timer:sleep(5000),    
    ct:pal("Reboot node to make the changes take effekt."),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    wait_for_node_go_down(ErlNode),
    check_for_node_restart(),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    wait_for_appl_started(),

    timer:sleep(60000),

    B = rct_rpc:call(rpc, os, cmd, ["cat /home/sirpa/releases/R*/comte/libcom_netconf_agent.cfg"], 10000, noprint),
    ct:log("# Modified libcom_netconf_agent.cfg:~n~p",[clean_up_str(B)]),

    %%%%
    %% Open Netconf sessions. 5 sessions vill be used for this test.
    %% Otherwise sessions could be released before check, due to timeout.
    %%%%
    {UsedSessionNames,_} = lists:split(5, ?NC_SessionNameList),
    ok = nc_open_session(UsedSessionNames),

    ct:pal("### All nc sessions opened.",[]),
    ct:pal("### sleep 4 min. Check that sessions still exist",[]),
    ct:sleep({minutes, 4}), %% this is not a thorough check!

    lists:foreach(fun(Name1) ->
    			  ct:pal("### Check that session still exist: ~p", 
				 [Name1]),
    			  {error,{connection_exists,_}} = 
			      ct_netconfc:open(Name1, [])
    		  end, 
    		  UsedSessionNames),

    ct:pal("### sleep 2 min.",[]),
    ct:sleep({seconds, 120}), % add 60 sec to make sure sessions is closed.
    ct:pal("### sleep 6 min has passed.Sessions shall be closed",[]),

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
    
clean_up_str(XmlStr) ->
    string:tokens(XmlStr, " <>?\n").


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
			  ct:pal("### Check that session still exist: ~p", 
				 [Name2]),
    			  {error,{connection_exists,_}} = 
			      ct_netconfc:open(Name2, [])
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
    ct:pal("### Check several netconf users do paralell request on get_config!",
	   []),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% send get config req.
    %%%%    
    Self = self(),    
    PidList = 
	lists:map(
	  fun(Name1) ->
		  spawn(fun() -> 
				ct:pal("### Get Config with Name: ~p", 
				       [Name1]),
				case get_config(Name1) of
				    error ->
					Self ! error;
				    SessName ->
					Self ! {done, SessName}
				end
			end)
	  end,
	  ?NC_SessionNameList),

    %%%%
    %% Wait on answer from get_config request.
    %%%%
    NC_Sess = lists:map(fun(_Pid) ->
				receive
				    error -> 			      
					error;
				    {done, NcSess} ->
					NcSess
				after 120000 -> 
					ct:fail("Error no answer within time!",
						[])
				end
			end, PidList),

    GetConfRes = lists:filter(fun(X) -> 
				       case X of 
					   error -> false; 
					   _ -> true 
				       end 
			       end, NC_Sess),
    ct:pal("sessions that get_config result in ok  : ~p",[GetConfRes]),
    A = length(GetConfRes),
    B = length(?NC_SessionNameList),
    ct:pal("Nr sessions that get_config result in ok : ~p",[A]),

    
    lists:foreach(fun(PidA) ->
   			  exit(PidA, kill)
    		  end, PidList),

    %%%%
    %% Close session
    %%%%
    ct:pal("All get_config is done ok. Close sessions",[]),
    ok = nc_close_session(?NC_SessionNameList),

    A = B,

    ok.

get_config(SessionName) ->
    case ct_netconfc:
	get_config(SessionName,running,
		   {'ManagedElement',
		    [{xmlns,"urn:com:ericsson:"
		      "ecim:ComTop"}],
		    [{managedElementId,[],["1"]}]}) of
	{ok, _} ->
	    SessionName;
	{error, _Error} ->
	    ct:pal("Rec Error from get-config : ~p , ~p", 
		   [_Error, SessionName]),
	    error;
	Other ->
	    ct:pal("Rec unexpected answer from get-config : ~p , ~p", 
		   [Other, SessionName]),
	    ct:fail("Unexpected answer from get_config")
    end.

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
			 
    lists:foreach(
      fun(N) ->
	      RandomNr = rand:uniform(?NrOfNetConfUsers),
	      Name1 = list_to_atom("nc"++integer_to_list(RandomNr)),
	      ct:pal("### Nr: ~p, Get Cofig with Name: ~p", [N, Name1]),
	      {ok,_} = 
		  ct_netconfc:get_config(Name1,running,
					 {'ManagedElement',
					  [{xmlns,"urn:com:ericsson:"
					    "ecim:ComTop"}],
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
%% Check several Netconf users, add TestClass1 instances. <br/>
%% Do commit after each instances is added. <br/>
%% @spec nc_users_add_testclass1(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1(_Config) ->
    ct:pal("### Check several Netconf users, add testclass1 instance. "
	   "Do commit after each instance is added!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %% %%%%
    %% Add instances
    %%%%
    lists:foreach(fun(Name1) ->
			  InstName1 = atom_to_list(Name1),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(Name1, 
							  InstName1, 
							  ?ATTR),
    			  ct:pal("### Close session: ~p", [Name1]),
    			  ok = ct_netconfc:close_session(Name1),
    			  ct:pal("### Open session: ~p", [Name1]),
    			  %% {ok,_} = ct_netconfc:open(Name1,[]),
			  nc_open_session([Name1]),
			  ok = rct_nc_testclass1_lib:
			      nc_get_mo_instance_check_attr(Name1,
							    InstName1, 
							    ?ATTR)
		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    lists:foreach(fun(Name2) ->
			  InstName2 = atom_to_list(Name2),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(Name2, InstName2),
			  ct:pal("### Close session: ~p", [Name2]),
			  ok = ct_netconfc:close_session(Name2),
			  ct:pal("### Open session: ~p", [Name2]),
			  %% {ok,_} = ct_netconfc:open(Name2,[]) ,
			  nc_open_session([Name2]),
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(Name2, InstName2)
    		  end, 
    		  ?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add TestClass1 instances . <br/>
%% Do commit after all instances has been added.  <br/>
%% Check that not commited instances is not commited, until their sessions is closed.<br/>
%% @spec nc_users_add_testclass1_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_commit(_Config) ->
    ct:pal("### Check several Netconf users, add TestClass1. "
	   "Do commit after all TestClass1 instances has been added!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instances.
    %%%%
    ok = add_inst_and_attr_no_commit(?NC_SessionNameList),

    %% %%%%
    %% %% Check that instances not commited.
    %% %%%%
    %% ok = check_testclass1_is_deleted(?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name1) ->
			  InstName1 = atom_to_list(Name1),
			  ct:pal("### Close session: ~p", [Name1]),
    			  ok = ct_netconfc:close_session(Name1),
    			  ct:pal("### Open session: ~p", [Name1]),
    			  %% {ok,_} = ct_netconfc:open(Name1,[]),
			  nc_open_session([Name1]),
			  %% Check testclass1 is created.
			  ok = rct_nc_testclass1_lib:
			      nc_get_mo_instance_check_attr(Name1,
							    InstName1, 
							    ?ATTR),
			  %% Check that ohter testclass1 is not commited.
			  %% create a list with the not commited instances.
			      NotCommitedList = 
			      lists:dropwhile(fun(X) -> 
						      X=/=Name1
					      end, 
					      ?NC_SessionNameList),
			  _CompleteNotCommitedList = 
			      lists:delete(Name1, NotCommitedList)
			  %% %% ct:pal("~p", [CompleteNotCommitedList]),
			  %% check_testclass1_is_deleted(CompleteNotCommitedList)
    		  end,
		  ?NC_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(?NC_SessionNameList),

    %%%%
    %% Delete testclass1
    %%%%
    lists:foreach(fun(Name2) ->
			  InstName2 = atom_to_list(Name2),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(Name2, InstName2),
			  %% Check that testclass1 is deleted,
			  %% data in COMTE cash. 
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(Name2, InstName2)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name3) ->
			  InstName3 = atom_to_list(Name3),
    			  ct:pal("### Close session: ~p", [Name3]),
    			  ok = ct_netconfc:close_session(Name3),
    			  ct:pal("### Open session: ~p", [Name3]),
    			  %% {ok,_} = ct_netconfc:open(Name3,[]),
			  nc_open_session([Name3]),
			  %% Check testclass1 is deleted.
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(Name3, InstName3),
			  %% Check that ohter testclass1 is deleted, 
			  %% data from COMTE cash.
			  %% create a list with the remaining
			  %% not deleted testclass1.
			  NotDeletedList = 
			      lists:dropwhile(fun(Y) -> 
						      Y=/=Name3
					      end, ?NC_SessionNameList),
			  _CompleteNotDeletedList = lists:delete(Name3, 
			  					NotDeletedList)
			  %% %% ct:pal("~p", [CompleteNotDeletedList]),
			  %% check_testclass1_is_not_deleted(
			  %%   CompleteNotDeletedList)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Check testclass1 is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add TestClass1 instances.<br/>
%% After all instances is added, do commit in reverse order. <br/>
%% Check that not commited instances is not commited, <br/>
%% until their sessions is closed.<br/>
%% @spec nc_users_add_testclass1_commit_reversed_order(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_commit_reversed_order(_Config) ->
    ct:pal("### Check several Netconf users, add testclass1 instances. "
	   "after all instances has been added do commit in reverse order!",[]),

    Reversed_SessionNameList = lists:reverse(?NC_SessionNameList),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instances, no commit
    %%%%
    ok = add_inst_and_attr_no_commit(?NC_SessionNameList),

    %% %%%%
    %% %% Check that instances not commited.
    %% %%%%
    %% ok = check_testclass1_is_deleted(?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name1) ->
			  InstName1 = atom_to_list(Name1),
    			  ct:pal("### Close session: ~p", [Name1]),
    			  ok = ct_netconfc:close_session(Name1),
			  timer:sleep(2000),
    			  ct:pal("### Open session: ~p", [Name1]),
    			  %% {ok,_} = ct_netconfc:open(Name1,[]),
			  nc_open_session([Name1]),
			  %% Check instance is created.
			  ok = rct_nc_testclass1_lib:
			      nc_get_mo_instance_check_attr(Name1,
							    InstName1, 
							    ?ATTR),
			  %% Check that ohter instances is not commited.
                          %% create a list with the not commited instances.
			  NotCommitedList = 
			      lists:dropwhile(fun(X) -> 
						      X=/=Name1
					      end, Reversed_SessionNameList),
			  _CompleteNotCommitedList = lists:delete(Name1, 
								 NotCommitedList)
			  %% %% ct:pal("~p", [CompleteNotCommitedList]),
			  %% check_testclass1_is_deleted(CompleteNotCommitedList)
    		  end, 
		  Reversed_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(?NC_SessionNameList),

    %%%%
    %% Delete instances, no commit.
    %%%%
    lists:foreach(fun(Name2) ->
			  InstName2 = atom_to_list(Name2),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(Name2, InstName2),
			  %% Check that testclass1 is deleted,
			  %% data in COMTE cash. 
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(Name2, InstName2)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name3) ->
			  InstName3 = atom_to_list(Name3),
    			  ct:pal("### Close session: ~p", [Name3]),
    			  ok = ct_netconfc:close_session(Name3),
			  timer:sleep(2000),
    			  ct:pal("### Open session: ~p", [Name3]),

    			  %% {ok,_} = ct_netconfc:open(Name3,[]),
			  nc_open_session([Name3]),
			  % Check instance is deleted.
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(Name3, InstName3),
			  %% Check that ohter instances is deleted, 
			  %% data from COMTE cash.
			  %% create a list with the remaining not deleted inst.
			  NotDeletedList = 
			      lists:dropwhile(fun(Y) -> 
						      Y=/=Name3
					      end, Reversed_SessionNameList),
			  _CompleteNotDeletedList = lists:delete(Name3, 
								NotDeletedList)
			  %% %% ct:pal("~p", [CompleteNotDeletedList]),
			  %% check_testclass1_is_not_deleted(
			  %%   CompleteNotDeletedList)
    		  end, 
		  Reversed_SessionNameList),

    %%%%
    %% Check instance is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),
    
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add TestClass1 instances <br/>
%%  and commit, paralell req. <br/>
%% Note that there could be conflicts if operations is exct on same time.<br/>
%% Then operation could fail due to another is doing opeartion.<br/>
%% @spec nc_users_add_testclass1_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_paralell(_Config) ->
    ct:pal("### Check several Netconf users, add testclass1 units and commit, "
	   "paralell request!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instances
    %%%%
    Self = self(),    
    PidList_Add = 
	lists:map(
	  fun(Name1) ->
		  spawn(fun() -> 
				InstName1 = atom_to_list(Name1),
				ct:pal(" Create testclass1: ~p", [InstName1]),
				ok = rct_nc_testclass1_lib:
				    nc_add_mo_instance_and_attr(Name1, 
								InstName1, 
								?ATTR),
				ct:pal("### Close session: ~p", [Name1]),
				case close_sess_when_add_paralell(Name1) of
				    error ->
					Self ! done;
				    SessName ->
					Self ! {done, SessName}
				end
			end)
	  end,
	  ?NC_SessionNameList),

    %%%%
    %% Wait on answer from add request.
    %%%%
    NC_Sess_Add = lists:map(fun(_Pid) ->
				    receive
					done -> 			      
					    error;
					{done, NcSess} ->
					    NcSess
				    after 90000 -> 
					    ct:fail("Error no answer within "
						    "time, when create!", [])
				    end
			    end, PidList_Add),
    
    %% Create a list of sessions there MO instance where created.
    %% Filter out error from list. 
    %% error could be returned if operation is same time.
    CreatedInst = lists:filter(fun(X) -> 
				       case X of 
					   error -> false; 
					   _ -> true 
				       end 
			       end, NC_Sess_Add),
    ct:pal("Created Instances : ~p",[CreatedInst]),

    lists:foreach(fun(PidA) ->
   			  exit(PidA, kill)
    		  end, PidList_Add),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),
    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(CreatedInst),

    %%%%
    %% Delete instances
    %%%%
    PidList_Delete = 
    	lists:map(
    	  fun(Name2) ->
    		  spawn(fun() -> 
    				InstName2 = atom_to_list(Name2),
    				ct:pal(" Delete testclass1: ~p", [InstName2]),
				case delete_mo_inst(Name2) of
				    error ->
					Self ! done;
				    SessionName2 ->
					Self ! {done, SessionName2}
				end
			end)
    	  end,
    	  ?NC_SessionNameList),


    %%%%
    %% Wait on answer from delete request.
    %%%%
    NC_Sess_Del = lists:map(fun(_Pid_1) ->
    				 receive
    				     done -> 			      
    					 error;
    				     {done, NcSess2} ->
    					 NcSess2
    				 after 90000 -> 
    					 ct:fail("Error no answer within "
						 "time, when delete!", [])
    				 end
    			 end, PidList_Delete),

    %% Create a list of sessions there MO instance where deleted.
    %% Filter out error from list. 
    %% error could be returned if operation is same time.
    DeletedInst = lists:filter(fun(Y) -> 
    					   case Y of 
					       error -> false; 
    					       _ -> true 
    					   end 
    				   end, NC_Sess_Del),
    ct:pal("Deleted instances : ~p",[DeletedInst]),

    lists:foreach(fun(PidB) ->
    			  exit(PidB, kill)
    		  end, PidList_Delete),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),
    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(DeletedInst),
 
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    %%%%
    %% Just a cleanup to ensure all created instances is deleted
    %%%%
    lists:foreach(fun(Name3) ->
    			  InstName3 = atom_to_list(Name3),
    			  ct_netconfc:open(Name3, []),
    			  rct_nc_testclass1_lib:
    			      nc_delete_mo_instance_no_check(Name3,
    							     InstName3),
    			  ct_netconfc:close_session(Name3)     
    		  end, 
    		  CreatedInst),
    
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),

    ok.

%%%%%%%%%%%%%%
%%%%%%%%%%%%%%
close_sess_when_add_paralell(SessionName) ->
    case ct_netconfc:close_session(SessionName) of
	ok ->
	    SessionName;
	{error,
	 [{'error-type', _, ["application"]},
	  {'error-tag', _, ["operation-failed"]},
	  {'error-severity', _,["error"]},
	  {'error-message', _,
	   _}]} ->
	    error;
	{error,{no_connection_found, _}} ->
	    error;
	Other ->
	    ct:pal("Rec unexpected answer when "
		   "create : ~p ", [Other]),
	    ct:fail("Unexpected answer when create")
    end.

close_sess_when_delete_paralell(SessionName) ->
    InstName = atom_to_list(SessionName),
    MatchStr = "MO: ManagedElement=1,TestRoot=1,"
	"TestClass1="++InstName++" is not "
	"available (no instance).",
    case ct_netconfc:close_session(SessionName) of
	ok ->
	    SessionName;
	{error,
	 [{'error-type', _, ["application"]},
	  {'error-tag', _, ["data-missing"]},
	  {'error-severity', _, ["error"]},
	  {'error-message', _, [MatchStr]}]} ->
	    error;
	{error,{no_connection_found, _}} ->
	    error;
	Other ->
	    ct:pal("Rec unexpected answer when "
		   "delete : ~p ", [Other]),
	    ct:fail("Unexpected answer when delete")
    end.
	
delete_mo_inst(SessionName) ->
    InstName = atom_to_list(SessionName),
    MatchStr = "MO: ManagedElement=1,TestRoot=1,"
	"TestClass1="++InstName++" is not "
	"available (no instance).",
    case rct_nc_testclass1_lib:
	nc_delete_mo_instance_no_check(SessionName, 
				       InstName) of
	ok ->
	    ok = ct_netconfc:close_session(SessionName),
	    %% ct:pal("AAA close : ~p, ~p",[SessionName, A]),
	    SessionName;
	{error,
	 [{'error-type', _, ["application"]},
	  {'error-tag', _, ["data-missing"]},
	  {'error-severity', _, ["error"]},
	  {'error-message', _, [MatchStr]}]} ->
	    _B = ct_netconfc:close_session(SessionName),
	    %% ct:pal("BBB close : ~p, ~p",[SessionName, _B]),
	    error;
    	Other ->
	    ct:pal("Rec unexpected answer when "
		   "delete : ~p ", [Other]),
	    ct:fail("Unexpected answer when delete")
    end.
	
%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add, delete TestClass1 and commit, <br/>
%% paralell req. <br/>
%% No instances should be created. <br/>
%% @spec nc_users_add_delete_testclass1_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_delete_testclass1_paralell(_Config) ->
    ct:pal("### Check several Netconf users, add delete testclass1 "
	   "and commit, paralell request!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instances
    %%%%
    Self = self(),    
    PidList = 
	lists:map(
	  fun(Name1) ->
		  spawn(fun() -> InstName1 = atom_to_list(Name1),
		  		 ct:pal("Create instance : ~p",[InstName1]),
		  		 ok = rct_nc_testclass1_lib:
		  		     nc_add_mo_instance_and_attr(Name1, 
		  						 InstName1, 
		  						 ?ATTR),
		  		 ct:pal("Delete instance : ~p",[InstName1]),
		  		 ok = rct_nc_testclass1_lib:
		  		     nc_delete_mo_instance(Name1, 
		  					   InstName1),
		  		 ct:pal("### Close session: ~p", [Name1]),
				 %% There could be sa_ais_err_exist when "
				 %% operate on same admin owner ar same tile.
				 %% Use same check as previus TC and don't care 
				 %% if sa_ais exeit or not.
				 case close_sess_when_add_paralell(Name1) of
				     error ->
					 Self ! done;
				     SessName ->
					 Self ! {done, SessName}
				 end
			end)
		      
	  end,
	  ?NC_SessionNameList),

    %%%%
    %% Wait on answer from add/delete request.
    %%%%
    lists:foreach(fun(_Pid) ->
			 receive
    			      done -> 			      
				 ok;
			     {done, NcSess2} ->
				 ct:log("####### ~p #########",[NcSess2]),
				 NcSess2
			 after 90000 -> 
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList),

    lists:foreach(fun(PidB) ->
    			  exit(PidB, kill)
    		  end, PidList),


    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),
    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Netconf users, add TestClass1 paralell. <br/>
%% Do commit after all instances is added. <br/>
%% @spec nc_users_add_testclass1_paralell_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_paralell_commit(_Config) ->
    ct:pal("### Check several Netconf users, add testclass1 paralell. "
	   "Do commit after all instance is added!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instances
    %%%%
    Self = self(),

    Slave = fun(Name, Loop) ->
		    InstName = atom_to_list(Name),
		    receive
			add_cmd ->
			    ok = rct_nc_testclass1_lib:
				nc_add_mo_instance_and_attr(Name, 
							    InstName, 
							    ?ATTR),
			    Self ! add_done;
			close_cmd_when_add -> 
			    ct:pal("### Close session after add: ~p", [Name]),
			    case close_sess_when_add_paralell(Name) of
				error ->
				    Self ! close_done_add;
				SessName ->
				    Self ! {close_done_add, SessName}
			    end;
			close_cmd_when_delete -> 
			    ct:pal("### Close session after del: ~p", [Name]),
			    case close_sess_when_delete_paralell(Name) of
				error ->
				    Self ! close_done_delete;
				SessionName ->
				    Self ! {close_done_delete, SessionName}
			    end;
			delete_cmd ->
			    rct_nc_testclass1_lib:
				nc_delete_mo_instance_no_check(Name, 
							       InstName),
			    %% ct:pal("BBB : ~p , ~p ", [B, InstName]),
			    Self ! delete_done;
			exit_slave -> exit(normal)
		    after 300000 -> % If nothing happens within 5min .
			    ct:pal("Exit slave due to timeout",[]),
			    exit(normal)
		    end,
		    Loop(Name, Loop)
	    end,
    
    PidList = lists:map(fun(Name1) ->
				spawn(fun() -> 
					      Slave(Name1, Slave)
				      end)
				    
			end,
			?NC_SessionNameList),

    %%%%
    %% Send Add instance paralell request
    %%%%
    lists:foreach(fun(Pid1) ->
			  Pid1 ! add_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid2) ->
			  Pid2 ! close_cmd_when_add
		  end, PidList),
    
    %%%%
    %% Wait on answer from the request.
    %%%%
    lists:foreach(fun(_Pid_1) ->
			  receive
			      add_done -> 			      
				  ok
			  after 90000 -> 
				  ct:fail("Error no answer within "
					  "time When Create inst", [])
			  end
		  end, PidList),

    NC_Sess_Add = lists:map(fun(_Pid_2) ->
				    receive
					close_done_add ->
					    error;
					{close_done_add, SessName} ->
					    SessName
				    after 30000 -> 
					    ct:log("no answer within time!"
						   " when add. No need to fail!")
					    %% ct:fail("Error no answer within "
					    %% 	    "time, close when add ", [])
				    end
			    end, PidList),

    %% Create a list of sessions there MO instance where created.
    %% Filter out error from list. 
    %% error could be returned if operation is same time.
    CreatedInst = lists:filter(fun(X) -> 
				       case X of 
					   error -> false; 
					   _ -> true 
				       end 
			       end, NC_Sess_Add),
    ct:pal("Created Instances : ~p",[CreatedInst]),

    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),

    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(CreatedInst),
    
    %%%%
    %% Send Delete instances paralell request
    %%%%
    lists:foreach(fun(Pid3) ->
			  Pid3 ! delete_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid4) ->
			  Pid4 ! close_cmd_when_delete
    		  end, PidList),

    %%%%
    %% Wait on answer from the Delete request.
    %%%%
    lists:foreach(fun(_Pid_3) ->
			 receive
    			      delete_done -> 			      
				 ok
			 after 30000 -> 
				 ct:log("no answer within "
					"time! when delete. No need to fail!")
				 %% ct:fail("Error no answer within "
				 %% 	 "time! when delete", [])
			 end
		  end, PidList),

    %% Wait on answer from the Commit request.
    NC_Sess_Del = lists:map(fun(_Pid_4) ->
				   receive
				       close_done_delete -> 
					   error;
				       {close_done_delete, NcSess2} ->
					   NcSess2
				   after 90000 -> 
					   ct:fail("Error no answer within "
						   "time, when close del", [])
				   end
			   end, PidList),

    %% Create a list of sessions there MO instance where deleted.
    %% Filter out error from list. 
    %% error could be returned if operation is same time.
    DeletedInst = lists:filter(fun(Y) -> 
				       case Y of 
					   error -> false; 
					   _ -> true 
				       end 
			       end, NC_Sess_Del),
    ct:pal("Deleted instances : ~p",[DeletedInst]),

    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),

    lists:foreach(fun(Pid5) ->
    			  exit(Pid5, kill)
    		  end, PidList),

    %%%%
    %% Open sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(DeletedInst),

    lists:foreach(fun(Pid5) ->
    			  Pid5 ! exit_slave
    		  end, PidList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    %%%%
    %% Just a cleanup to ensure all created instances is deleted
    %%%%
    lists:foreach(fun(Name5) ->
    			  InstName5 = atom_to_list(Name5),
    			  ct_netconfc:open(Name5, []),
    			  rct_nc_testclass1_lib:
    			      nc_delete_mo_instance_no_check(Name5,
    							     InstName5),
    			  ct_netconfc:close_session(Name5)     
    		  end, 
    		  CreatedInst),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  NC users add TestClass1, then restart.<br/>
%% - Add instances, No commit.<br/>
%% - restart.<br/>
%% - Check that no instances exist.<br/>
%% - Add instances, do commit<br/>
%% - restart again.<br/>
%% - Check instances still exist.<br/>
%% @spec nc_users_add_testclass1_restart(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_restart(_Config) ->
    ct:pal("### Check several Netconf users, add testclass1 instances. "
	   "Do restart!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    
    %%%%
    %% Add instancess, no commit
    %%%%
    ok = add_inst_and_attr_no_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart
    %%%%
    ct:pal("### restart!",[]),
    ok = rct_rpc:call(rpc, appmI, restart_piu_warm, ['_'], 100000, print),
    timer:sleep(10000),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    ok = add_inst_and_attr_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart again
    %%%%
    ct:pal("### restart again!",[]),
    ok = rct_rpc:call(rpc, appmI, restart_piu_warm, ['_'], 100000, print),
    timer:sleep(10000),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(?NC_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    ok = delete_inst_commit(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%%  NC users add TestClass1, then perform restart Cold.<br/>
%% - Add instances, No commit.<br/>
%% - Restart Cold.<br/>
%% - Check no instances exist.<br/>
%% - Add instances, do commit<br/>
%% - Restart Cold again.<br/>
%% - Check instances still exist.<br/>
%% @spec nc_users_add_testclass1_init_reboot(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_init_reboot(_Config) ->
    ct:pal("### Check several Netconf users, add testclass1 instances. "
	   "Do appmI:restart_piu_cold",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instancess, no commit
    %%%%
    ok = add_inst_and_attr_no_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart cold
    %%%%
    ct:pal("### restart cold!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    wait_for_node_go_down(ErlNode),
    check_for_node_restart(),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    ok = add_inst_and_attr_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart cold again
    %%%%
    ct:pal("### restart cold again!",[]),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    wait_for_node_go_down(ErlNode),
    check_for_node_restart(),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),
    wait_for_appl_started(),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(?NC_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    ok = delete_inst_commit(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% One Netconf users, add TestClass1. One with bad format. <br/>
%% Will result in that nothing is added, <br/>
%% due to edit_config failed and session will be closed automatic. <br/>
%% - Add 10 instances and one instance with bad xml format.<br/>
%% - the operation will fail and nc session will be closed.<br/>
%% @spec nc_user_fail_to_add_attribute(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_user_fail_to_add_attribute(_Config) ->

    ct:pal("### One Netconf users, add, 11 testclass instances. ""One with "
	   "bad xml format, NO instances shall exist after commit !",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    [NC_session|_T]=?NC_SessionNameList,
    ok = nc_open_session([NC_session]),
    NrOfInstList = lists:seq(1, 11),
    
    %%%%
    %% Add instances
    %%%%
    Instances = 
	lists:map(fun(Nr) ->
			  InstName = atom_to_list(NC_session)++"_"++
			      integer_to_list(Nr),
			  {'TestClass1',
			   [],
			   [{'testClass1Id',[],[InstName]},
			    {struct1,[{struct,"Struct1"}],
			     [{struct1mem2,[],["init"]},
			      {struct1mem1,[],[?ATTR]}]},
			    {int32,[],[?ATTR]}
			   ]}
		  end, 
		  ?NrOfMoInstList),
    %% ct:pal("Instances: ~p", [Instances]),
    
    TestRootList = lists:append([{testRootId,[],["1"]}], 
				Instances),
    %% ct:pal("TestRootList: ~p", [TestRootList]),
    
    FailInstAttrList = lists:append(TestRootList, 
				     [{'TestClass1',
				       [],
				       [{'testClass1Id',[],["nc1_11"]},
					{struct1,[{struct,"Struct1"}],
					 [{struct1mem2,[],["init"]},
					  {struct1mem1,[],[?ATTR]}]},
					{int32,[],[badAttr]}
				       ]}]),
    
    ct:log("~p", [FailInstAttrList]),
    
    ct:pal("Add 10 correct MO instances and 1 with bad attribute.",[]),

   
    %%   {'error-message',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"},
    %% 			{'xml:lang',"en"}],
    %%    ["Assertion failed at /repo/etxarnu/com/com51_pra_cand1_2/clone/git/com-main/src/netconf_agent/src/SetStructOp.cc:19 - SetStructOp"]}]} =
   
    ErrorMsgString = "Non-struct attribute int32 in MO ManagedElement=1,TestRoot=1,TestClass1=nc1_11 should not have any child elements.",
    {error,
     [{'error-type',
       [{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
       ["application"]},
      {'error-tag',
       [{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
       ["unknown-element"]},
      {'error-severity',
       [{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
       ["error"]},
      {'error-info',
       [{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"}],
       [{'bad-element',[],["badAttr"]}]},
      {'error-message',
       [{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"},
	{'xml:lang',"en"}],
       [ErrorMsgString]}]} =
    	ct_netconfc:edit_config(NC_session,running,
    				{'ManagedElement',
    				 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    				 [{managedElementId,[],["1"]},
    				  {'TestRoot',
    				   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
    				   FailInstAttrList
    				  }]}),

    %% case lists:prefix("Assertion failed", ErrorMsgString) of
    %% 	true ->
    %% 	    continue;
    %% 	false ->
    %% 	    ct:fail("Unexpected error-message:  ~p", [ErrorMsgString])
    %% end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),

    %%%%
    %% Check instances is deleted
    %%%%
    nc_open_session([NC_session]),
    lists:foreach(fun(X) ->
			  InstName2 = atom_to_list(NC_session)++"_"++
			      integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(NC_session, 
							   InstName2)
		  end, NrOfInstList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),
    ok = nc_close_session([NC_session]),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% One Netconf users, add TestClass1 instances. <br/>
%% One with bad instanse name. <br/>
%% Will result in that nothing is added, <br/>
%% due to edit_config failed, session will be closed automatic. <br/>
%% - Add 10 instances and one bad.<br/>
%% - Nothing shall be added.<br/>
%% - the operation will fail and nc session will be closed.<br/>
%% @spec nc_user_fail_to_add_struct_attribute(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_user_fail_to_add_struct_attribute(_Config) ->

    ct:pal("### One Netconf users, add 10 testclass1 instances and 1 with bad "
	   "instanse name in xml, NO inst shall exist after commit !",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    [NC_session|_T]=?NC_SessionNameList,
    ok = nc_open_session([NC_session]),
    NrOfInstList = lists:seq(1, 11),
    
    %%%%
    %% Add instances
    %%%%
    Instances = 
	lists:map(fun(Nr) ->
			  InstName = atom_to_list(NC_session)++"_"++
			      integer_to_list(Nr),
			  {'TestClass1',
			   [],
			   [{'testClass1Id',[],[InstName]},
			    {struct1,[{struct,"Struct1"}],
			     [{struct1mem2,[],["init"]},
			      {struct1mem1,[],[?ATTR]}]},
			    {int32,[],[?ATTR]}
			   ]}
		  end, 
		  ?NrOfMoInstList),
    %% ct:pal("Instances: ~p", [Instances]),
    
    TestRootList = lists:append([{testRootId,[],["1"]}], 
				Instances),
    %% ct:pal("TestRootList: ~p", [TestRootList]),

    
    %%%%
    %% The fault is TestClass1 with bad instance name.
    %%%%
    FailInstList = lists:append(TestRootList, 
				[{'TestClassss1',
				  [],
				  [{'testClass1Id',[],["nc1_11"]},
				   {struct1,[{struct,"Struct1"}],
				    [{struct1mem2,[],["init"]},
				     {struct1mem1,[],[?ATTR]}]},
				   {int32,[],[?ATTR]}
				  ]}]),
    ct:log("~p", [FailInstList]),

    ct:pal("Add 10 correct MO instances and 1 bad instance name.",[]),

    {error,
     [{'error-type',
       [{xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"}],
       ["application"]},
      {'error-tag',
       [{xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"}],
       ["unknown-attribute"]},
      {'error-severity', [{xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"}],
       ["error"]},
      {'error-info', [{xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"}],
       [{'bad-attribute',[],["TestClassss1"]},
	{'bad-element',[],["TestRoot"]}]},
      {'error-message', [{xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"},
	{'xml:lang',"en"}],
       ["Failed to add struct attribute"]}]} =
	ct_netconfc:edit_config(NC_session,running,
				{'ManagedElement',
				 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				 [{managedElementId,[],["1"]},
				  {'TestRoot',
				   [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
				   FailInstList
				  }]}),
 
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = nc_open_session([NC_session]),
    lists:foreach(fun(X) ->
			  InstName2 = atom_to_list(NC_session)++"_"++
			      integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(NC_session, 
							   InstName2)
		  end, NrOfInstList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, noprint),
    ok = nc_close_session([NC_session]),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  NC users add TestClass1, then reboot.<br/>
%% - Add instances, No commit.<br/>
%% - Reboot.<br/>
%% - Check no instances exist.<br/>
%% - Add instances, do commit<br/>
%% - Reboot again.<br/>
%% - Check instances still exist.<br/>
%% @spec nc_users_add_testclass1_reboot(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_reboot(_Config) ->
    ct:pal("### Check several Netconf users, add instances. Do reboot!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instancess, no commit
    %%%%
    ok = add_inst_and_attr_no_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    %% ok = ct_telnet:send(console, "reboot"),
    ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board"),
    {ok,_} = ct_telnet:expect(console, "Ericsson Version:", 
			      [{timeout,60000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up session on client side, after reboot.
    %%%%
    lists:foreach(fun(Name2) ->
    			  ct:pal("### Close hanging session on client: ~p", 
    				 [Name2]),
    			  ct_netconfc:close_session(Name2)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    ok = add_inst_and_attr_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% reboot again
    %%%%
    ct:pal("### reboot again!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    %% ok = ct_telnet:send(console, "reboot"),
    ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board"),
    {ok,_} = ct_telnet:expect(console, "Ericsson Version:", 
			      [{timeout,60000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(?NC_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    ok = delete_inst_commit(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.




%%--------------------------------------------------------------------
%% @doc
%%  NC users add TestClass1, then power off/on.<br/>
%% - Add instances, No commit.<br/>
%% - Power off/on.<br/>
%% - Check no instances exist.<br/>
%% - Add instances, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check instances exist or no exist. 
%%   It depends if data has been written to disc before power off.<br/>
%% - Add instances, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check instances exist.<br/>
%% @spec nc_users_add_testclass1_power(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_power(_Config) ->
    ct:pal("### Check several Netconf users, add testclass1 instances. "
	   "Do power off/on!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% %% Add instancess, no commit
    %% %%%%
    ok = add_inst_and_attr_no_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% Power off/on 
    %%%%
    ct:pal("### power off/on!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "login:", 
    			      [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up session on client side, after power off/on.
    %%%%
    lists:foreach(fun(Name2) ->
    			  ct:pal("### Close hanging session on client: ~p", 
    				 [Name2]),
    			  ct_netconfc:close_session(Name2)
    		  end, 
    		  ?NC_SessionNameList),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Add instances again and commit. power off directly, 
    %% then data could exist in disc. depends on how many instances.
    %%%%
    ok = add_inst_and_attr_commit(?NC_SessionNameList),

    %%%%
    %% Power off/on again
    %%%%
    ct:pal("### power off/on again!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%% Remove this check due to sometimes power off/on takes some times,
    %%%% then it could result that instances exist.
    %% %%%%
    %% %% Check instance is created or deleted.
    %% %%%%
    %% %% check_if_inst_exist_or_removed(?NC_SessionNameList),
    %% ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %% power off after a few seconds so data has been written to disc.
    %%%%
    ok = add_inst_and_attr_commit(?NC_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% Check instances is added
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    ok = check_inst_added(?NC_SessionNameList),
    ok = nc_close_session(?NC_SessionNameList),

    %% %% sleep to be sure that data exist on disc.
    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 20000, print),

    %%%%
    %% Power off/on again, 3:rd time.
    %%%%
    ct:pal("### power off/on, 3:rd time!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Open sessions after restart.
    %%%%
    open_session_after_restart(?NC_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = check_inst_added(?NC_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    ok = delete_inst_commit(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  NC users add TestClass1, then kill_com.<br/>
%% - Add instances, No commit.<br/>
%% - kill com.<br/>
%% - Check no instances exist.<br/>
%% - Add instances, do commit<br/>
%% - kill com again.<br/>
%% - Check instances still exist.<br/>
%% @spec nc_users_add_testclass1_kill_com(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_kill_com(_Config) ->
  
    ct:pal("### Check several Netconf users, add testclass1 instance. "
	   "Kill com!",[]),

    %%%%
    %% Open Netconf sessions.
    %%%%
    ok = nc_open_session(?NC_SessionNameList),

    %%%%
    %% Add instancess, no commit
    %%%%
    ok = add_inst_and_attr_no_commit(?NC_SessionNameList),
    timer:sleep(5000),

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
    %% Check instances is deleted
    %%%%
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    ok = add_inst_and_attr_commit(?NC_SessionNameList),

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
    %% Check instances is added
    %%%%
    ok = check_inst_added(?NC_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    ok = delete_inst_commit(?NC_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = nc_open_session(?NC_SessionNameList),
    ok = check_inst_deleted(?NC_SessionNameList),

    %%%%
    %% Close session
    %%%%
    ok = nc_close_session(?NC_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%%  NC users add TestClass1, then kill_session.<br/>
%% - Add instances, No commit.<br/>
%% - kill the sessions .<br/>
%% - Check no instances exist.<br/>
%% @spec nc_users_add_testclass1_kill_session(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
nc_users_add_testclass1_kill_session(_Config) ->
  
    %% Create new List, because the last will be used to kill the other 
    %% sessions (can't kill own session)
    KillList =
	lists:delete(list_to_atom("nc"++integer_to_list(length(?NC_SessionNameList))),?NC_SessionNameList),

    %% Open Netconf sessions.
    ok = nc_open_session(?NC_SessionNameList),
 
    %%%%
    %% Add instancess, no commit
    %%%%
    ok = add_inst_and_attr_no_commit(KillList),
    timer:sleep(5000),

    %%
    %% kill sessions
    %%
    kill_session(KillList, lists:last(?NC_SessionNameList)),
   
    %%%%
    %% Close session
    %%%%
    ok = nc_close_session([lists:last(?NC_SessionNameList)]),
    ok.


%%% Internal functions
%%%%%%%%%%%%%%%%%
%% Kill sessions 
%% Check that the sessions are killed 
%% KillList = Session List to be killed
%% KillerSession = Session used to kill other sessions
%%%%%%%%%%%%%%%%%
kill_session(KillList, KillerSession)->
    lists:foreach(fun(Name) ->
			  SessionId = ct_netconfc:get_session_id(Name),	
			  ct:pal("### killing sessionId: ~p Name: ~p",[SessionId, Name]),
			  case  ct_netconfc:kill_session(KillerSession, SessionId) of
			      ok->
				  ct:pal("ok session killed"),
				  case ct_netconfc:get_session_id(Name) of
				      {error,{no_connection_found,Name}} -> ok;
				      _Reason -> poll_session_id_removed(Name, 6, [])
				  end;
			      Result ->  ct:fail("kill_sesion cause: ~p",[Result])
			  end 
		  end,KillList).
%%%%%%%%%%%%%%%%%
%% Check that the session is removed
%%%%%%%%%%%%%%%%%
poll_session_id_removed(Name, NrOfTry, Reason) when NrOfTry =:= 0->  
    ct:fail("Session id exist after session_kill SessionId: ~p Name: ~p",[Reason, Name]);
poll_session_id_removed(Name, NrOfTry, _Reason)->
    timer:sleep(500),   
    case ct_netconfc:get_session_id(Name) of
	{error,{no_connection_found,Name}} -> ok;
	Result -> poll_session_id_removed(Name, NrOfTry - 1, Result)
    end. 


 
%%%%%%%%%%%%%%%%%
%% Use in cleanup
%% Need to open/close after each delete.
%%%%%%%%%%%%%%%%%
nc_cleanup_testclass1(NC_hookName, NC_SessionNameList) ->
    lists:foreach(fun(SesionName) ->
			  InstName =atom_to_list(SesionName),
			  ct_netconfc:open(NC_hookName,[]),
			  %% ct:pal("### Delete instance id: ~p", [InstName]),
			  rct_nc_testclass1_lib:
			      nc_delete_mo_instance_no_check(NC_hookName,
							     InstName),
			  ct_netconfc:close_session(NC_hookName)
		  end,
		  NC_SessionNameList).


%%--------------------------------------------------------------------
%% @doc 
%% Open netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
%% nc_open_session(NC_SessionNameList) ->
%%     lists:foreach(fun(Name) ->
%% 			  timer:sleep(2000),
%% 			  ct:pal("### Open Name: ~p", [Name]),
%%     			  {ok,_} = ct_netconfc:open(Name, [])
%%     		  end, 
%% 		  NC_SessionNameList),
%%     ok.

nc_open_session(NC_SessionNameList) ->
    nc_open_session(NC_SessionNameList, 120000).
nc_open_session([], _Timeout) ->
    ct:pal("all nc session is up");
nc_open_session(_NC_SessionNameList, Timeout) when Timeout < 0 ->
    ct:fail("Session could not be  started within max timeout after restart.");

nc_open_session([H|T], Timeout) ->
    case ct_netconfc:open(H, []) of
	{error,Cause} ->
	    timer:sleep(1000),
	    ct:log("### Could not Open Name: ~p, try again. cause: ~p.", 
		   [H, Cause]),
	    nc_open_session([H|T], Timeout-1000);
	{ok,_} ->
	    ct:pal("### Open Name: ~p, is done.", [H]),
	    nc_open_session(T, Timeout)
    end.

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

%% %%-------------------------------------------------------------------- 
%% %% @hidden
%% %% check_testclass1_is_not_deleted(NC_SessionNameList)
%% %%-------------------------------------------------------------------- 
%% check_testclass1_is_not_deleted(NC_SessionNameList) ->
%%     {ok,_} = ct_netconfc:open(?CHECK_SESSION,[]),
%%     lists:foreach(fun(NC_hookName) ->
%% 			  ct:pal("Check testclass1 id: ~p, not deleted.",
%% 				 [NC_hookName]),
%% 			  InstName = atom_to_list(NC_hookName),
%% 			  rct_nc_testclass1_lib:
%% 			      nc_check_mo_instance_created(?CHECK_SESSION,
%% 							   InstName)
%% 		  end, 
%% 		  NC_SessionNameList),
%%     ok = ct_netconfc:close_session(?CHECK_SESSION),
%%     ok.

%% %%-------------------------------------------------------------------- 
%% %% @hidden
%% %% check_testclass1_is_deleted(NC_SessionNameList)
%% %%-------------------------------------------------------------------- 
%% check_testclass1_is_deleted(NC_SessionNameList) ->
%%     {ok,_} = ct_netconfc:open(?CHECK_SESSION,[]),
%%     lists:foreach(fun(NC_hookName) ->
%% 			  ct:pal("Check TestClass1 id: ~p, not exist.",
%% 				 [NC_hookName]),
%% 			  InstName = atom_to_list(NC_hookName),
			  
%% 			  ok = rct_nc_testclass1_lib:
%% 			      nc_check_mo_instance_deleted(?CHECK_SESSION, 
%% 							   InstName)
%% 		  end, 
%%     		  NC_SessionNameList),
%%     ok = ct_netconfc:close_session(?CHECK_SESSION),

%%     ok.

%%-------------------------------------------------------------------- 
%% @hidden
%% open_session_after_restart(NC_SessionNameList)
%%-------------------------------------------------------------------- 
open_session_after_restart(NC_SessionNameList) ->
    open_session_after_restart(NC_SessionNameList, 120000).

open_session_after_restart(_NC_SessionNameList, Timeout) when Timeout < 2000 ->
    ct:fail("Netconf not started within max timeout after restart.");

open_session_after_restart([H|T], Timeout) ->
    case ct_netconfc:open(H, []) of
	{error,_Cause} ->
	    %% ct:pal("### After restart, Fail to open Name: ~p, Cause: ~p", 
	    %% 	   [H,_Cause]),
	    timer:sleep(1000),
	    open_session_after_restart([H|T], Timeout-1000);
	{ok,_} ->
	    ct:pal("### Open Name: ~p, is done.", [H]),
	    lists:foreach(fun(Client) ->
				  timer:sleep(2000),
				  ct:pal("### Open Name: ~p", [Client]),
				  {ok,_} = ct_netconfc:open(Client, [])
			  end, T)
    end.

%% %%-------------------------------------------------------------------- 
%% %% @hidden
%% %% check_if_inst_exist_or_removed(NC_SessionNameList)
%% %%-------------------------------------------------------------------- 
%% check_if_inst_exist_or_removed(NC_SessionNameList) ->
%%     ExistOrRemList =
%% 	lists:map(
%% 	  fun(SessName) ->
%% 		  InstName = atom_to_list(SessName),
%% 		  case rct_nc_testclass1_lib:nc_get_mo_instance_no_check(
%% 			 SessName, InstName) of
%% 		  {ok,[{'ManagedElement',[{xmlns,"urn:com:ericsson:"
%% 					   "ecim:ComTop"}],
%% 			[{managedElementId,[],["1"]},
%% 			 {'TestRoot',[{xmlns,"urn:com:ericsson:"
%% 				       "ecim:TESTMOM"}],
%% 			  [{testRootId,[],["1"]},
%% 			   {'TestClass1',[],
%% 			    [{struct1,[{struct,"Struct1"}],AttrStructList},
%% 			     {int32,[],[?ATTR]},
%% 			     {testClass1Id,[],[InstName]}
%% 			     ]
%% 			   }]}]}]}  ->
%% 			  %% %%Check expected attributes exist
%% 			  true = lists:member({struct1mem1,[],[?ATTR]}, 
%% 					      AttrStructList),
%% 			  true = lists:member({struct1mem2,[],["init"]}, 
%% 					      AttrStructList),
%% 			  exist;
%% 		      {error, _} ->
%% 			  not_exist
%% 		  end
%% 	  end, NC_SessionNameList),
    
%%     ct:pal("### ExistOrRemList : ~p", [ExistOrRemList]),
%%     %% Filter out not_exist from list list.
%%     ExistList = lists:filter(fun(X) ->
%% 				     case X of
%% 					 not_exist ->
%% 					     false;
%% 					 exist -> 
%% 					     true
%% 				     end
%% 			     end, ExistOrRemList),
    
%%     ct:pal("### ExistList : ~p", [ExistList]),
%%     NrOfCreatedInst = length(ExistList),
    
%%     %% %% %%% Just a esay chek to se if instances is created or not as expected.
%%     %% %% case ?NrOfNetConfUsers of
%%     %% %% 	%% Instances Always deleted from disc after power off/on
%%     %% %% 	Val when Val < 5 -> 
%%     %% %% 	    ct:pal("### Instances shall not exist on disc.", []),
%%     %% %% 	    NrOfCreatedInst = 0;
%%     %% %% 	%% This numbers of instances could result in deleted or 
%%     %% %% 	%% existing on disc after power off/on
%%     %% %%     Val when Val < 20 -> 
%%     %% %% 	    ct:pal("### instances could exist or not existon disc.", []),    
%%     %% %% 	    %% ct:pal("### ExistOrRemList : ~p", [ExistOrRemList]),
%%     %% %% 	    ct:pal("NrOfCreatedInst: ~p of Nr of create inst operation"
%%     %% %% 		   " befor power: ~p",
%%     %% %% 		   [NrOfCreatedInst, length(ExistOrRemList)]);
%%     %% %% 	%% This numbers of instances always result in deleted and 
%%     %% %% 	%% existing on disc after power off/on
%%     %% %% 	_ -> 
%%     %% %% 	    BoardType = get_board_type(),
%%     %% %% 	    case BoardType of
%%     %% %% 		BoardType when BoardType == "dus4101";
%%     %% %% 			       BoardType == "duw4101" ->
%%     %% %% 		    ct:pal("### Some instances shall exist on disc.\n"
%%     %% %% 			   "### NrOfCreatedInst: ~p", [NrOfCreatedInst]),
%%     %% %% 		    case NrOfCreatedInst of
%%     %% %% 			Nr when Nr > 0 ->
%%     %% %% 			    ok;
%%     %% %% 			_ ->
%%     %% %% 			    ct:fail("Expected some instances created, "
%%     %% %% 				    "but it was not!")
%%     %% %% 		    end;
%%     %% %% 		_ ->
%%     %% %% 		    ct:pal("### Results in no instances exist due to fast execution on TCU.\n"
%%     %% %% 			   "### NrOfCreatedInst: ~p", [NrOfCreatedInst]),
%%     %% %% 		    ok
%%     %% %% 	    end
%%     %% %% end,       

 
%%     %% %% %% Using the above check results in warning when compile.
%%     %% %% %% This is a quick fix that shall be used when ? NrOfNetConfUsers
%%     %% %% %% is larger than 20.
%%     %% BoardType = get_board_type(),
%%     %% case BoardType of
%%     %% 	BoardType when BoardType == "dus4101";
%%     %% 		       BoardType == "duw4101" ->
%%     %% 	    ct:pal("### Some instances shall exist on disc.\n"
%%     %% 		   "### NrOfCreatedInst: ~p", [NrOfCreatedInst]),
%%     %% 	    case NrOfCreatedInst of
%%     %% 		Nr when Nr > 0 ->
%%     %% 		    ok;
%%     %% 		_ ->
%%     %% 		    ct:fail("Expected some instances created, "
%%     %% 			    "but it was not!")
%%     %% 	    end;
%%     %% 	_ ->
%%     %% 	    ct:pal("### Results in no instances exist due to fast execution on TCU.\n"
%%     %% 		   "### NrOfCreatedInst: ~p", [NrOfCreatedInst]),
%%     %% 	    ok
%%     %% end,

%%     ct:pal("### Nr of Instances that exist on disc: ~p", [NrOfCreatedInst]),
%%     %% NrOfCreatedInst = 0,

%%     ok.

%%-------------------------------------------------------------------- 
%% @hidden
%% add_inst_and_attr_no_commit(NC_SessionNameList)
%%--------------------------------------------------------------------
add_inst_and_attr_no_commit(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  ct:pal("# Create instance: ~p", [InstName]),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(Name, 
							  InstName, 
							  ?ATTR)
    		  end, NC_SessionNameList),
    ok.


%%%% NOTE nc sessions shall be up.
add_inst_and_attr_commit(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  ct:pal("## Create instance: ~p", [InstName]),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(Name, 
							  InstName, 
							  ?ATTR),
    			  ct:pal("### Close session: ~p", [Name]),
    			  ok = ct_netconfc:close_session(Name)
    			  %% ct:pal("### Open session: ~p", [Name3]),
    			  %% {ok,_} = ct_netconfc:open(Name3,[])
    		  end, NC_SessionNameList),
    ok.

%%-------------------------------------------------------------------- 
%% @hidden
%% delete_inst_commit(NC_SessionNameList) 
%%--------------------------------------------------------------------
%%%% NOTE nc sessions shall be up.
delete_inst_commit(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  ct:pal("## Delete instance: ~p", [InstName]),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(Name, InstName),
    			  ct:pal("### Close session: ~p", [Name]),
    			  ok = ct_netconfc:close_session(Name)
    			  %% ct:pal("### Open session: ~p", [Name]),
    			  %% {ok,_} = ct_netconfc:open(Name,[])
    		  end, NC_SessionNameList),
    ok.

check_inst_added(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  timer:sleep(2000),
			  InstName = atom_to_list(Name),
			  ok = rct_nc_testclass1_lib:
   			      nc_get_mo_instance_check_attr(Name,
   							    InstName, 
   							    ?ATTR)
		  end, 
		  NC_SessionNameList),
    ok.

%%-------------------------------------------------------------------- 
%% @hidden
%% check_inst_deleted(NC_SessionNameList)
%%--------------------------------------------------------------------
check_inst_deleted(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  timer:sleep(2000),
    			  InstName = atom_to_list(Name),
    			  ok = rct_nc_testclass1_lib:
    			      nc_check_mo_instance_deleted(Name, InstName)
    		  end, 
    		  NC_SessionNameList),
    ok.

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
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 10000)  of
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
    wait_for_appl_started(120000).

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
		    End = os:timestamp(),
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
    case rct_rpc:call(rpc, appmI, restart_piu_cold, ['_'], 100000, print) of
	ok ->
	    ok;
	{badrpc,nodedown} ->
	    ct:pal("restart cold, did not take effect! try again",[]),
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
    BoardType = get_board_type(),
    RestartStr = case BoardType of
		     BoardType when BoardType == "dus4101";
				    BoardType == "duw4101" ->
			 "2:nd Stage Boot Loader";
		     _ ->
			 "Ericsson Version:"
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


get_board_type() ->
    BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    BoardType.

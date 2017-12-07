%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	rob_alarm_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/R3A/4
%%% 
%%% @doc == Robustness when alarm is active , Check ecpected traps is recieved==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%% @end

-module(rob_alarm_SUITE).
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
%%% R2A/1      2013-10-01 etxivri     Created
%%% R2A/3      2013-10-01 etxivri     Update due to over 50 pmJobs then alarm is
%%%                                   raised. 
%%% R3A/1      2013-10-17 etxivri     Minor update.
%%% R3A/2      2013-10-21 etxivri     Minor update.
%%% R3A/3      2013-10-21 etxivri     Minor update.
%%% R3A/4      2014-12-18 etxivri     Minor update.
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_ntf.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 pm_alarm_max_nr_of_jobs/1
	]).

-define(NrOfCliUsers, 1). % Only one is needed to get active alarms.
-define(Cli_sess, cli1). % Only one is needed to get active alarms.
-define(NrOfNetConfUsers, 1).

-define(CLI_SessionNameList, 
	[list_to_atom(
	   "cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(ActPmAlarmMaxNrJobs,  [{type,eriAlarmWarning},
				{eriAlarmActiveManagedObject,
				 "ManagedElement=1,SystemFunctions=1,Pm=1"},
				{eriAlarmActiveMinorType,9175056},
				{eriAlarmActiveSpecificProblem,
				 "Large Number Of Counters"}
			       ] ).
-define(ClearPmAlarmMaxNrJobs, [{type,eriAlarmCleared},
				{eriAlarmActiveManagedObject,
				 "ManagedElement=1,SystemFunctions=1,Pm=1"},
				{eriAlarmActiveMinorType,9175056},
				{eriAlarmActiveSpecificProblem,
				 "Large Number Of Counters"}
			       ] ).
-define(AlarmListRebuilt, [{type,eriAlarmAlarmListRebuilt}]).
-define(AllowedTraps,[any_order, 
		      {allowed_traps,
		       [
			[{type,eriAlarmCritical}, 
			 {eriAlarmActiveSpecificProblem,
			  "License Key File Fault"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,Lm=1"}],
			[{type,eriAlarmMinor}, 
			 {eriAlarmActiveSpecificProblem,
			  "All NTP Servers Unreachable"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,SysM=1"}],
			[{type,eriAlarmCleared}, 
			 {eriAlarmActiveSpecificProblem,
			  "All NTP Servers Unreachable"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,SysM=1"}],
			[{type,eriAlarmHeartBeatNotif}],
			[{type,nsNotifyShutdown}]
		       ]
		      }] ).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, <br/>
%% to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    %% build up a list of CliHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    CliHooks =[{rct_cli, {list_to_atom("cli"++integer_to_list(N)), 
			  [manual_connect]}} || N <- lists:seq(1,
							       ?NrOfCliUsers)] ,

    %% ct:pal("CliHooks: ~p",[CliHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_scp, [{1, node1}]},
		 {rct_core,[]},
		 {rct_snmpmgr,snmp1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}} | 
		 lists:append(NetconfHooks, CliHooks)
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
end_per_testcase(pm_alarm_max_nr_of_jobs, Config) ->
    case proplists:get_value(tc_status, Config) of
	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \nClean up pmJobs", 
		   [Reason]),
	    delete_pmjob(lists:seq(1, 51), ?Cli_sess)
    end,
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [pm_alarm_max_nr_of_jobs].

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
%% Create max nr of PmJobs. Max is 50 pm jobs.
%% - Check expected alarm is generated. max number of jobs running<br/>
%% Delete one PmJob, checl alaram is ceased.
%% @spec pm_alarm_max_nr_of_jobs(Config) -> ok
%% @end
%%--------------------------------------------------------------------   
pm_alarm_max_nr_of_jobs(_Config) ->
    Cli_session = ?Cli_sess,

    %%%%
    %% Set expected traps when generate pm alarm.
    %%%%
    ok = rct_snmpmgr:wait_for_traps([?ActPmAlarmMaxNrJobs],
				    ?AllowedTraps,
    				    180),
    %%%%
    %% Craete 50 pmJobs this will rsult in alarm.
    %%%%
    create_pmjob(lists:seq(1,51), Cli_session),
    timer:sleep(5000), %% Alarm takes two sec before acktive.
    ok = rct_snmpmgr:check_traps(),

    %%%%
    %% Again, Set expected traps when generate pm alarm.
    %%%%
    ok = rct_snmpmgr:wait_for_traps([?AlarmListRebuilt, 
				     ?ActPmAlarmMaxNrJobs],
				    ?AllowedTraps,
    				    300),
    %%%%
    %% Reboot node
    %%%%
    reboot_node(),
    %% test_server:break("A"),
    timer:sleep(10000),
    ok = rct_snmpmgr:check_traps(),

    %%%%
    %% Set expected traps when cease pm alarm.
    %%%%
    ok = rct_snmpmgr:wait_for_traps([?ClearPmAlarmMaxNrJobs],
				    ?AllowedTraps,
    				    180),
    %%%%
    %% delete two pmJob. This shall result in alarm ceased.
    %%%%
    delete_pmjob([51,50], Cli_session),
    timer:sleep(5000), %% Alarm takes two sec before acktive.
    ok = rct_snmpmgr:check_traps(),

    delete_pmjob(lists:seq(1, 49), Cli_session),

    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
create_pmjob(NrList, Cli_session) ->
    PrintOpt = print,
    ok = rct_cli:connect(Cli_session),
    rct_cli:send(Cli_session, "configure", PrintOpt),

    lists:foreach(fun(X) ->
    			  Nr = integer_to_list(X),
    			  ct:pal("Nr: ~p", [Nr]),
			  rct_cli:send(Cli_session, "top", PrintOpt),
    			  rct_cli:send(Cli_session, 
    				       "ManagedElement=1,"
    				       "SystemFunctions=1,"
    				       "Pm=1,"
    				       "PmJob="++Nr, 
    				       PrintOpt), 
    			  rct_cli:send(Cli_session, 
    				       "MeasurementReader="++Nr, 
    				       PrintOpt),
    			  rct_cli:send(Cli_session, 
    				       "measurementSpecification", 
    				       PrintOpt),
    			  rct_cli:send(Cli_session, 
    				       "groupRef=\"ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=Group1\"", 
    				       PrintOpt)			  			      
    			  end, NrList),

    rct_cli:send(Cli_session, "commit", PrintOpt),
    ok = rct_cli:disconnect(Cli_session).


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
delete_pmjob(NrList, Cli_session) ->
    PrintOpt = print,
    rct_cli:connect(Cli_session),

    lists:foreach(fun(X) ->
			  Nr = integer_to_list(X),
			  ct:pal("Delete PmJob Nr: ~p", [Nr]),
			  rct_cli:send(Cli_session, "top", PrintOpt),
			  rct_cli:send(Cli_session, "configure", PrintOpt),

			  rct_cli:send(Cli_session, 
				       "ManagedElement=1,"
				       "SystemFunctions=1,"
				       "Pm=1,"
				       "PmJob="++Nr, 
				       PrintOpt), 
			  rct_cli:send(Cli_session, 
				       "requestedJobState=STOPPED", 
				       PrintOpt),
			  rct_cli:send(Cli_session, "top", PrintOpt),
			  rct_cli:send(Cli_session, 
				       "no ManagedElement=1,"
				       "SystemFunctions=1,"
				       "Pm=1,"
				       "PmJob="++Nr, 
				       PrintOpt),

			  rct_cli:send(Cli_session, "commit", PrintOpt)
			  end, NrList),

    rct_cli:disconnect(Cli_session).


%%--------------------------------------------------------------------
%% @doc
%% reboot the node.  <br/>
%% @spec reboot_node() -> ok
%% @end
%%--------------------------------------------------------------------
reboot_node() ->
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    %% net_kernel:disconnect(ErlNode),
    %% %% This is just to make sure that no ERROR in ct shell from testnode,
    %% %% When run suite in seq with other suites.
    %% Nodes = nodes(),
    %% ct:pal("# Nodes: ~p",[Nodes]),
    %% [ net_kernel:disconnect(TestNode) || TestNode <- Nodes],
    ok = ct_telnet:send(console, "reboot"),
    net_kernel:disconnect(ErlNode),
    timer:sleep(5000),
    {ok,_} = ct_telnet:expect(console, "login:", [{timeout,60000}, 
						     no_prompt_check]),
    net_kernel:connect(ErlNode).

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pri_c_SUITE.erl %
%%% @author etxarnu
%%%
%%% @doc ==Basic Test Suite for the PRI legacy C interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(pri_c_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/1').
-date('2017-01-27').
-author('etxarnu').
%%% ----------------------------------------------------------
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
%%% -------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------
%%% Rev     Date       Name     What
%%% R2A/10  2013-02-29 etxkols  Added rct_core hook
%%% R2A/11  2013-03-18 etxkols  Added retry when start_proxy times
%%%                             out in wait_for_app_up
%%% R2A/12  2013-04-09 etxkols  Temporary added 30 sec sleep in restart_own_piu
%%%                             to give EE time to start sshd
%%% R2A/13  2013-04-17 etxjovp  change timetrap to 30
%%% R3A/2   2014-07-14 etxarnu  Increased wait_for_app_up timeout to 120 sec
%%% R3A/3   2014-10-03 etxkols  Added console logging
%%% R3A/4   2014-12-01 etxarnu  Increased wait_for_app_down timeout to 20 sec
%%% R3A/4   2014-12-02 etxarnu  Handle case where service alreday terminated
%%% R3A/5   2015-02-19 etxarnu  Removed WARM and REFRESH TCs
%%% R3A/6   2015-04-24 etxkols  Increased wait_for_app_up timeout to 135 sec
%%% R4A/1   2015-07-02 etxarnu  Increased wait_for_app_up timeout to 600 sec
%%% R4A/2   2015-07-02 etxarnu  Added wait_for_com_started
%%% R4A/3   2015-07-10 etxjovp  Add group definitions used by CS CI
%%% R4A/4   2015-09-29 etxivri  Activate warm restarts tests in restart_own_piu.
%%%                             Warm restarts TCs runs on target env.
%%%                             Note, on sim then start_proxy is needed.
%%% R4A/5   2015-10-23 etxivri  Update wait or app started to be more robust.
%%% R4A/6   2015-10-23 etxivri  Update due to warm restart is enabled on TCU
%%% R4A/7   2015-11-18 etxivri  Update due to warm restart is enabled on DUS
%%% R5A/1   2016-03-05 etxarnu  Added warm restart after warm restart
%%% R7A/1   2016-09-16 etxarnu  Removed warm restart for vrcs
%%% R7A/2   2016-10-17 egjknpa  add FAKE_VNFM for vrcs 
%%% R7A/3   2016-10-31 etxaldu  Reset the FAKE_VNFM for vrcs after restart
%%% R8A/1   2016-12-13 etxkols  Removing rs232 hook for cloud
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0,
	 initiate_service/1,
	 terminate_service/0,
	 incor_prot_vers/1,
	 cor_prot_vers/1,
	 get_hunt_path/1,
	 get_pid/1,
	 get_own_identity3/1,
	 get_own_identity8/1,
	 get_own_identity9/1,
	 get_piuOrDevice_identity8/1,
	 get_piuOrDevice_identity9/1,
	 restart_own_piu/1,
	 restart_other_piu/1,
	 get_linkHandler_name3/1
	]).

-include_lib("common_test/include/ct.hrl").
-include("test_pri.hrl").

-define(TENV,"pri_c_suite").
-define(TVAL,"coldtest").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    Serial = case os:getenv("SIM_OR_TARGET") of
		 "cloudish" -> [{cth_conn_log,[]}];
		 _ -> [{rct_rs232,console},{cth_conn_log,[]}]
	     end,
   [{timetrap, {minutes, 30}},
    {ct_hooks, [{rct_htmllink,[]},
		{rct_rpc, rpc},
                {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
                {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		{rct_core,[]}
	       ] ++ Serial}].

%% @hidden
init_per_suite(Config) ->
    {ok, client_started} =
        rct_proxy:start_proxy(node1, pri1, ?PRI),
    {ok, memory_initiated} =
        rct_proxy:send_proxy(node1, pri1, ?CelloPri_initiateMemory),
    %% NewConfig.
    %% fake VNFM for vrcs
    fake_vnfm(set),
    Config.

%% @hidden
end_per_suite(_Config) ->
    timer:sleep(2000),
    {ok, memory_freed} =
        rct_proxy:send_proxy(node1, pri1, ?CelloPri_freeMemory),

    {ok, client_stopped} = rct_proxy:stop_proxy(node1, pri1),
    %% fake VNFM for vrcs
    fake_vnfm(unset),
    ok.


%% @hidden
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),
    Config.

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {cover__group, [], AllGroup -- [restart_own_piu,restart_other_piu]},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, cover__group}]},
     {sdc__nocover__sim__1__group, [], [restart_own_piu,restart_other_piu]},
     {sdc__qual__all__1__group, [], []} 
   ].
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [cor_prot_vers,
     get_pid,
     get_hunt_path,
     get_own_identity3,
     get_own_identity8,
     get_own_identity9,
     get_piuOrDevice_identity8,
     get_piuOrDevice_identity9,
     restart_own_piu,
     restart_other_piu%% ,
     %% get_linkHandler_name3
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request connection to the PRI Server, using incorrect protocols.<br/>
%% @spec incor_prot_vers(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
incor_prot_vers(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_REJ, _, _} = initiate_service({4, 3, 2}),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request connection to the PRI Server, using correct protocols.<br/>
%% @spec cor_prot_vers(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cor_prot_vers(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),
    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request the hunt path.<br/>
%% @spec get_hunt_path(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_hunt_path(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1, ?CelloPiu3_getHuntPath,
			     {1,"xxxxxx", 0}),

    {ok, {?CELLO_PIU3_GET_HUNT_PATH_CFM,
	  "xxxxxx", ?CELLO_PIU_RO_OK, 0}} = rct_proxy:receive_proxy(),
    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request actual Product info of PIU.<br/>
%% @spec get_pid(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_pid(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1, ?CelloPiu10_getPid,
			     {1, 0}),

    {ok, {?CELLO_PIU10_OPERATIONAL_PID_CFM,
	  1,
	  ProductNumber, ProductRevision,
	  ProductName, ProductDate, SerialNumber,
	  ?CELLO_PIU_RO_OK, 0}} = rct_proxy:receive_proxy(),

    ct:pal("GetPid returns: ~n"
	   "ProductNumber= ~p~n"
	   "ProductRevision= ~p~n"
	   "ProductName= ~p~n"
	   "ProductDate= ~p~n"
	   "SerialNumber= ~p~n"
	  , [ProductNumber, ProductRevision,
	     ProductName, ProductDate, SerialNumber]),

    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request identity of own PIU.<br/>
%% @spec get_own_identity3(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_own_identity3(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1, ?CelloPiu3_getOwnIdentity),

    {ok, {?CELLO_PIU3_GET_OWN_ID_CFM,
	  _PiuInstanceId, ?CELLO_PIU_RO_OK}} = rct_proxy:receive_proxy(),


    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% The function is used to request identity of own PIU.<br/>
%% @spec get_own_identity9(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_own_identity9(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1, ?CelloPri9_getOwnIdentity, {4711}),

    {ok, {?CELLO_PRI9_GET_IDENTITY_CFM,
	  _PiuInstanceId,
	  _PiuOrDeviceId,
	  _SrNo,
	  _Smn,
	  _Slot,
	  _Apn,
	  _DevId,
	  _HuntPrefix,
	  ?CELLO_PIU_RO_OK,
	  4711}} = rct_proxy:receive_proxy(),


    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% The function is used to request identity of other PIU.<br/>
%% @spec get_piuOrDevice_identity9(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_piuOrDevice_identity9(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1,
			     ?CelloPri9_getPiuOrDeviceIdentity, {1,4711}),

    {ok, {?CELLO_PRI9_GET_IDENTITY_CFM,
	  _PiuInstanceId,
	  _PiuOrDeviceId,
	  _SrNo,
	  _Smn,
	  _Slot,
	  _Apn,
	  _DevId,
	  _HuntPrefix,
	  ?CELLO_PIU_RO_OK,
	  4711}} = rct_proxy:receive_proxy(),


    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% The function is used to request identity of own PIU.<br/>
%% @spec get_own_identity8(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_own_identity8(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1, ?CelloPri8_getOwnIdentity, {4711}),

    {ok, {?CELLO_PRI8_GET_IDENTITY_CFM,
	  _PiuInstanceId,
	  _PiuOrDeviceId,
	  _Smn,
	  _Slot,
	  _Apn,
	  _DevId,
	  _HuntPrefix,
	  ?CELLO_PIU_RO_OK,
	  4711}} = rct_proxy:receive_proxy(),


    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request identity of other PIU.<br/>
%% @spec get_piuOrDevice_identity8(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_piuOrDevice_identity8(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1,
			     ?CelloPri8_getPiuOrDeviceIdentity, {1,4711}),

    {ok, {?CELLO_PRI8_GET_IDENTITY_CFM,
	  _PiuInstanceId,
	  _PiuOrDeviceId,
	  _Smn,
	  _Slot,
	  _Apn,
	  _DevId,
	  _HuntPrefix,
	  ?CELLO_PIU_RO_OK,
	  4711}} = rct_proxy:receive_proxy(),


    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% The function is used to request restart of own PIU.<br/>
%% @spec restart_own_piu(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
restart_own_piu(_) ->
    IsVrcs = rct_rpc:call(rpc, sysEnv, vrcs, [], 10000),
    case IsVrcs of
	true ->
	    %% warm not used in vrcs
	    ok;
	false ->


	    Pid0 = rct_rpc:call(rpc, os, getpid, [], 10000),
	    ct:pal("Pid0 = ~p", [Pid0]),
	    ct:pal("restart_own_piu RESTART_WARM", []),
	    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

	    %%{ok, ?CELLO_PRI_OK} = this might not come since the ift_app is killed
	    rct_proxy:send_proxy(node1, pri1,
				 ?CelloPiu4_restartOwnPiu, {0,true,"crash",2560}),
	    timer:sleep(5000),
	    ok = wait_for_app_up(node1, pri1),

	    Pid1 = rct_rpc:call(rpc, os, getpid, [], 10000),
	    ct:pal("Pid1 = ~p", [Pid1]),

	    case Pid1 of
		Pid0 ->
		    ok;
		_ ->
		    ct:fail("Cold restart after request of warm restart")
	    end,

	    rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),


	    ct:pal("restart_own_piu RESTART_WARM  Second time", []),
	    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

	    %%{ok, ?CELLO_PRI_OK} = this might not come since the ift_app is killed
	    rct_proxy:send_proxy(node1, pri1,
				 ?CelloPiu4_restartOwnPiu, {0,true,"crash",2560}),
	    timer:sleep(5000),
	    ok = wait_for_app_up(node1, pri1),

	    Pid1 = rct_rpc:call(rpc, os, getpid, [], 10000),
	    ct:pal("Pid1 = ~p", [Pid1]),

	    case Pid1 of
		Pid0 ->
		    ok;
		_ ->
		    ct:fail("Cold restart after second request of warm restart")
	    end,

	    rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000)

    end,

    ct:pal("restart_own_piu RESTART_COLD", []),
    rct_rpc:call(rpc, os,putenv, [?TENV,?TVAL], 10000),

    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),
    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1,
			     ?CelloPiu4_restartOwnPiu, {2,true,"crash",2560}),

    ct:pal(" RESTART_COLD, wait for applications to start", []),
    wait_for_app_restart(node1, pri1),
    wait_for_com_started(),

    fake_vnfm(set),

    case rct_rpc:call(rpc, os,getenv, [?TENV], 10000) of
	?TVAL ->
	    ct:fail("Warm restart after request of cold restart");
	_ ->
	    ok
    end,

    ct:pal("restart_own_piu RESTART_COLD_WITH_TEST", []),
    rct_rpc:call(rpc, os,putenv, [?TENV,?TVAL], 10000),

    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),
    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1,
			     ?CelloPiu4_restartOwnPiu, {3,true,"crash",2560}),

    ct:pal(" RESTART_COLD_WITH_TEST: wait for applications to start", []),
    wait_for_app_restart(node1, pri1),
    wait_for_com_started(),

    fake_vnfm(set),

    case rct_rpc:call(rpc, os,getenv, [?TENV], 10000) of
	?TVAL ->
	    ct:fail("Warm restart after request of cold restart");
	_ ->
	    ok
    end,

    %% {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),
    timer:sleep(30000),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% The function is used to request restart of other PIU.<br/>
%% @spec restart_other_piu(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
restart_other_piu(_) ->
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1,
			     ?CelloPiu4_restartOtherPiu,
			     {2,1,true,"crash",2560}),

    {ok, {?CELLO_PIU4_RESTART_PIU_CFM,
	  2,2560}} = rct_proxy:receive_proxy(),


    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec get_linkHandler_name3(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_linkHandler_name3(_) ->
    ClientId = 42,
    {?CELLO_PRI_INITIATE_SERVICE_CFM, 0, 10} = initiate_service({10,9,8}),

    {ok, ?CELLO_PRI_OK} =
	rct_proxy:send_proxy(node1, pri1, ?CelloPiu3_getLinkHandlerName,
			     {1, ClientId}),

    LinkHandlerName = "",
    {ok, {?CELLO_PIU3_GET_LH_NAME_CFM, ?CELLO_PIU_RO_OK, LinkHandlerName,
	  ClientId}} = rct_proxy:receive_proxy(),

    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% initiate_service
%%
%% @spec initiate_service(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
initiate_service(ProtocolVersion) ->
    {?CELLO_PRI_TERMINATE_SERVICE_CFM} = terminate_service(),

    {ok, ?CELLO_PRI_OK} =
        rct_proxy:send_proxy(node1, pri1, ?CelloPri_initiateService,
                             ProtocolVersion),
    {ok, {?CELLO_PRI_SERVER_UP_IND}} = rct_proxy:receive_proxy(),

    {ok, ?CELLO_PRI_OK} =
        rct_proxy:send_proxy(node1, pri1, ?CelloPri_internal),
    {ok, Answer} = rct_proxy:receive_proxy(),
    {ok, ?CELLO_PRI_OK} =
        rct_proxy:send_proxy(node1, pri1, ?CelloPri_internal),

    Answer.

%%--------------------------------------------------------------------
%% @doc
%% terminate_service
%%
%% @spec terminate_service() -> ok
%% @end
%%--------------------------------------------------------------------
terminate_service() ->
    case rct_proxy:send_proxy(node1, pri1, ?CelloPri_terminateService) of
	    {ok, ?CELLO_PRI_OK} ->
	    {ok, Answer} = rct_proxy:receive_proxy(),

	    {ok, ?CELLO_PRI_OK} =
		rct_proxy:send_proxy(node1, pri1, ?CelloPri_internal),

	    Answer;
	{ok, client_not_started} ->
	    {?CELLO_PRI_TERMINATE_SERVICE_CFM}
    end.

wait_for_app_restart(Node, Name) ->
    ok = wait_for_app_down(Node),
    timer:sleep(5000),
    ok = wait_for_app_up(Node, Name),
    timer:sleep(10000).

wait_for_app_down(Node) ->
    wait_for_app_down(Node, 20).

wait_for_app_down(_Node, 0) ->
    ct:fail("ift_app not down");
wait_for_app_down(Node, Cnt) ->
    [disconnect_node(N) || N <- nodes(connected)],
    case rct_proxy:is_app_running(Node) of
	false ->
	    ct:pal("Application down"),
	    ok;
	true ->
	    timer:sleep(1000),
	    wait_for_app_down(Node, Cnt-1)
    end.

wait_for_app_up(Node, Name) ->
    wait_for_app_up(Node, Name, 600).

wait_for_app_up(_Node, _Name, 0) ->
    ct:fail("ift_app not up");
wait_for_app_up(Node, Name, Cnt) ->
    case rct_proxy:is_app_running(Node) of
	true ->
	    case rct_proxy:start_proxy(Node, Name, ?PRI) of
		{error, timeout} ->
		    ct:pal("rct_proxy:start_proxy timeout in wait_for_app_up at count ~p, Retrying",[Cnt]),
		    timer:sleep(1000),
		    wait_for_app_up(Node, Name, Cnt-1);
		{ok, client_started} ->
		    ct:pal("Application up"),
		    {ok, memory_initiated} =
			rct_proxy:send_proxy(Node, Name, ?CelloPri_initiateMemory),
		    ok;
		{ok,clientId_already_in_use} ->
		    ct:log("client not restarted. Sleep and try again"),
		    timer:sleep(1000),
		    wait_for_app_up(Node, Name, Cnt-1);
		_Other ->
		    ct:log("Rcvd unexpected answer: ~p, sleep and try again.",
			  [_Other]),
		    timer:sleep(1000),
		    wait_for_app_up(Node, Name, Cnt-1)
	    end;
%	    {ok, client_started} =
%		rct_proxy:start_proxy(Node, Name, ?PRI),
%	    {ok, memory_initiated} =
%		rct_proxy:send_proxy(Node, Name, ?CelloPri_initiateMemory),
%	    ok;
	false ->
	    timer:sleep(1000),
	    wait_for_app_up(Node, Name, Cnt-1)
    end.


%% ===========================================================================
%% @doc
%% Set or Unset the FAKE_VNFM env variable for vrcs. <br/>
%% @spec fake_vnfm(Action)
%% Action: atom() set|unset
%% @end
%% ===========================================================================
fake_vnfm(set) ->
    case rct_rpc:call(rpc, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc, os, putenv, ["FAKE_VNFM", ""], 10000);
        _ ->
            rct_rpc:call(rpc, os, unsetenv, ["FAKE_VNFM"], 10000)
    end;
fake_vnfm(_) ->
    case rct_rpc:call(rpc, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc, os, unsetenv, ["FAKE_VNFM"], 10000);
        _ ->
            ok
    end.



%% ===========================================================================
%% @doc
%% Check for COM started. <br/>
%% @spec wait_for_com_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_com_started() ->
    wait_for_com_started(90000).

wait_for_com_started(Timeout) when Timeout < 500 ->
    %% io:get_line("### COM Check,  press return\r\n");
    ct:fail("COM not started within max timeout.");

wait_for_com_started(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 1000)  of
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(1000),
	    wait_for_com_started(Timeout - 1000);
	[] ->
	    timer:sleep(1000),
	    wait_for_com_started(Timeout - 1000);
	Data ->
	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
		{match, _Result} ->
		    Data;
		_ ->
		    timer:sleep(1000),
		    wait_for_com_started(Timeout - 1000)
	    end
    end.

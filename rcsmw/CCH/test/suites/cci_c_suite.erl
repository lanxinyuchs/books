%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cci_c_suite.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R10A/24
%%%
%%% @doc ==Basic Test Suite for the CCI interface on SUT.==
%%% This Test Suite can be used on target enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(cci_c_suite).

%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R5A/1	2016-02-17	ekurnik		Created
%%% R5A/2	2016-02-19  ekurnik		Added new testcases
%%% R5A/3 	2016-02-22	ekurnik		Fixed failing testcases
%%% R5A/4 	2016-02-26  ekurnik		Added SIM test group
%%% R5A/5   2016-02-29  ekurnik	    Added 1 minute wait in cci_time_ind to be sure
%%%									that the time wasn't changed by previous TC
%%% R5A/6	2016-02-29  ekurnik		Using rct_ssh hook instead of rct_ssh script
%%% R5A/7   2016-03-16  ekurnik     Suppressing CalendarClockMisaligned alarm with
%%%                                 cold restart after cci_time_ind test
%%% R10A/1  2017-06-06  ekurnik     Created skeleton for time_slew tests
%%% R10A/2  2017-06-08  eivmiha     Added time_slew tests
%%% R10A/3  2017-06-12  evadumb     Added CCI ntpd polling tests
%%% R10A/4  2017-06-14  eivmiha     Added comsa_ntpd_polling tests
%%% R10A/6  2017-06-19  evadumb     Updated comsa_ntpd_polling tests
%%% R10A/10 2017-06-27  evadumb     Added tests for CCI subscribtion with PV2 protocol version and PV2 signal
%%% R10A/12 2017-06-28  ekurnik     Added TC-US2-1, TC-US2-1a, TC-US2-1b
%%% R10A/13 2017-06-28  estjako     Added groups to all()
%%% R10A/14 2017-06-29  ekurnik     Added TC-US2-4
%%% R10A/15 2017-06-30  evadumb     Added tests for ntp server state indication
%%% R10A/16 2017-07-03  emirbos     Added TC-US2-2, TC-US2-2a, TC-US2-2b, TC-US2-2c, TC-US2-2d
%%% R10A/17 2017-07-03  estjako     Small changes in waitForNtpState
%%% R10A/18 2017-07-04  ekurnik     Refactoring
%%% R10A/19 2017-07-05  ekurnik     Ignoring clock slew indication in timestep tests
%%% R10A/20 2017-07-06  estjako     Added groups to all()
%%% R10A/21 2017-07-10  enekdav     Added tests for clock slew group
%%% R10A/23 2017-07-12  enekdav     Making ntpd_state_ind_initial test more robust
%%% R10A/23 2017-07-14  enekdav     Making ntpd_state_ind_server_group tests more robust
%%% ----------------------------------------------------------

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0,
         all/0
        ]).

-export([cci_subscribe_unsubscribe/1,
         cci_time_ind/1,
         cci_server_down/1,
         cci_client_down/1
        ]).

-export([cci_subscribe_2_PV2/1,
         cci_subscribe_2_PV1/1,
         cci_subscribe_1_PV1/1,
         cci_subscribe_2_PV2_already/1,
         cci_subscribe_2_PV1_already/1,
         cci_subscribe_1_unsuccesful/1,
         cci_subscribe_not_supported/1
        ]).

-export([ntp_state_ind_no_server_conf/1,
         ntp_state_ind_server_conf/1,
         ntp_state_ind_server_re_conf/1
        ]).

-export([cci_time_slew_alg_undefined/1,
         cci_time_slew_alg_init_offset/1,
         cci_time_slew_alg_same_offset/1,
         cci_time_slew_alg_diff_offset/1,
         cci_time_slew_alg_notification/1,
         cci_time_slew_alg_over_tinker_step/1
        ]).

-export([ntpd_polling_started/1
		]).

-export([ntpd_polling/1,
         ntpd_polling_no_server_configured/1,
         ntpd_polling_server_configured_no_reach/1,
         ntpd_polling_server_configured/1,
         ntpd_polling_mutiple_servers_configured/1
        ]).

-export([ntp_state_ind_initial/1,
         ntp_state_ind_timestep/1]).

-export([ntp_state_out_of_sync_undefined_offset/1,
		 ntp_state_in_sync_undefined_offset/1,
		 ntp_state_out_of_sync_defined_offset/1,
		 ntp_state_in_sync_same_offset/1,
		 ntp_state_in_sync_different_offset/1]).

-export([cci_pv2_clock_slew/1,
		 cci_pv2_clock_slew_discrepancy/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("test_cci.hrl").

-define(SYSM_DN, [<<"ManagedElement=1">>,
                  <<"SystemFunctions=1">>,
                  <<"SysM=1">>]).
-define(CALENDAR_CLOCK_ALARM, 'CalendarClockMisaligned').


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

-spec suite() -> [tuple()].

suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
                 {rct_netconf, nc1},
                 {rct_htmllink,[]},
                 {rct_logging, {all,
                                [{erlang,
                                  {["ERROR REPORT","CRASH REPORT"],
                                   ["exception exit: unexpected_exit",
                                    "\\*\\* unexpected_exit"]}
                                 }]}},
                 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
                 {rct_core,[]},
                 {rct_ssh,{ssh,[manual_connect]}}
                ]}].

%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok = rct_proxy:exit_master(node1).

init_per_group(cci_time_slew_group, Config) ->
    case cci_c_lib:is_target() of
        true ->
            {skip, "Can't run test on target."};
        false ->
            Config
    end;

init_per_group(comsa_ntpd_polling_group, Config) ->
    case cci_c_lib:is_target() of
        false ->
            {skip, "Can't run test on SIM"};
        true ->  
            Config
    end;

init_per_group(cci_state_change_ntp_state_group, Config) ->
    case cci_c_lib:is_target() of
        true ->
            {skip, "Can't run test on target."};
        false ->
            Config
    end;

init_per_group(cci_pv2_clock_slew_group, Config) ->
    case cci_c_lib:is_target() of
        true ->
            {skip, "Can't run test on target."};
        false ->
            Config
    end;

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.


%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(ntp_state_ind_no_server_conf, Config) ->
    {ok, client_started} =
        rct_proxy:start_proxy(node1, cci1, ?CCI), 
    
    ct:log("Unlocking just in case of locked administrativeState"),
    ok = cci_c_lib:set_ntp_element(administrativeState, "UNLOCKED"),
    
    init_subscribe(ntp_state_ind_no_server_conf, Config),

    Config;

init_per_testcase(ntp_state_ind_server_conf, Config) ->
    {ok, client_started} =
        rct_proxy:start_proxy(node1, cci1, ?CCI),
    
    ct:log("Setting administrativeState to 'LOCKED?"),

    ok = cci_c_lib:set_ntp_element(administrativeState, "LOCKED"),
    
    init_subscribe(ntp_state_ind_server_conf, Config),
    
    Config;

init_per_testcase(ntp_state_ind_server_re_conf, Config) ->
    {ok, client_started} =
        rct_proxy:start_proxy(node1, cci1, ?CCI),
    
    ct:log("Unlocking just in case of locked administrativeState"),
    ok = cci_c_lib:set_ntp_element(administrativeState, "UNLOCKED"),
    
    init_subscribe(ntp_state_ind_server_re_conf, Config),

    Config;

%% @hidden
init_per_testcase(TestCase, Config) ->
    {ok, client_started} =
        rct_proxy:start_proxy(node1, cci1, ?CCI),
    init_subscribe(TestCase, Config),
    Config.



%% helper function, subscribes to CCI for some TCs
init_subscribe(TestCase, _Config) ->
    subscribe({lists:member(TestCase, cci_time_slew()),
               lists:member(TestCase, ntp_state_ind_server() ++ 
                                cci_state_change_ntp_state() ++ cci_pv2_clock_slew())}).
%% PV1
subscribe({true, _}) ->
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    ok;

%% PV2
subscribe({_, true}) ->
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}), 
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    {_, _} = waitForNtpStateInd(),
    ok;

subscribe(_) ->
    ok.

%% @hidden
end_per_testcase(cci_time_ind, _Config) ->
    %% There is a chance that cci_time_ind TC will raise CalendarClockMisaligned alarm
    %% The node should be restarted if that is the case
    case rct_rpc:call(rpc, comsaI, get_alarms, [?CALENDAR_CLOCK_ALARM], 10000) of
        [] ->
            ok;
        _ ->
            rct_rpc:call(rpc, appmI, restart_node, [cold, "Clearing CalendarClockMisaligned alarm"], 10000),
            timer:sleep(60000),
            waitForNodeUp()
    end,
    %% ift_app stops working after setting the time back
    %% so restart is neccessary
    ok = rct_proxy:exit_master(node1);


%% @hidden
end_per_testcase(TestCase, Config) ->
    timer:sleep(2000),
    end_subscribe(TestCase, Config),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, cci1, 10000),
    ok.

%% helper function, unsubscribes to CCI for some TCs
end_subscribe(TestCase, _Config) ->
    unsubscribe(lists:member(TestCase, cci_time_slew() ++ 
                                       ntp_state_ind_server() ++ 
                                       cci_state_change_ntp_state() ++ cci_pv2_clock_slew())).

unsubscribe(true) ->
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),
    waitForUnsubscribeConfirm(),
    ok;

unsubscribe(false) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    CciTimeSlewGroup = cci_time_slew(),
    CciNtpdPollingGroup = cci_ntpd_polling(),
    ComsaNtpdServerPollingGroup = comsa_ntpd_polling(),
    CciPV2SubscribeGroup = cci_pv2_subscribe(),
    NtpStateIndServer = ntp_state_ind_server(),
    CciStateChangeNtpStateGroup = cci_state_change_ntp_state(),
    CciPV2ClockSlewGroup = cci_pv2_clock_slew(),
    [
     {cci_sim_test_group, [], [cci_subscribe_unsubscribe,
                               cci_server_down,
                               cci_client_down,
                               ntpd_polling,
                               ntp_state_ind_initial,
                               {group, cci_time_slew_group},
                               {group, cci_ntpd_polling_group},
                               {group, cci_pv2_subscribe_group},
                               {group, cci_state_change_ntp_state_group},
                               {group, cci_pv2_clock_slew_group}
                              ]},
     {cci_time_slew_group, [], CciTimeSlewGroup},
     {cci_ntpd_polling_group, [], CciNtpdPollingGroup},
     {cci_pv2_subscribe_group, [], CciPV2SubscribeGroup},
     {ntp_state_ind_server_group, [], NtpStateIndServer},
     {cci_state_change_ntp_state_group, [], CciStateChangeNtpStateGroup},
     {comsa_ntpd_polling_group, [], ComsaNtpdServerPollingGroup},
     {cci_pv2_clock_slew_group, [], CciPV2ClockSlewGroup},
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade_short__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []},
     %% This suite can be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     cci_subscribe_unsubscribe,
     cci_time_ind,
     cci_server_down,
     cci_client_down,
     ntp_state_ind_initial,
     {group, cci_ntpd_polling_group},
     {group, comsa_ntpd_polling_group},
     {group, cci_pv2_subscribe_group}, 
     {group, ntp_state_ind_server_group}
    ].

%%--------------------------------------------------------------------
%% @doc
%% CCI Time slew test group
%% @end
%%--------------------------------------------------------------------
cci_time_slew() ->
    [
     cci_time_slew_alg_undefined,
     cci_time_slew_alg_init_offset,
     cci_time_slew_alg_same_offset,
     cci_time_slew_alg_diff_offset,
     cci_time_slew_alg_notification
    ].



%%--------------------------------------------------------------------
%% @doc
%% CCI ntpd polling test group
%% @end
%%--------------------------------------------------------------------
cci_ntpd_polling() ->
	[
     ntpd_polling_started
	].

%%--------------------------------------------------------------------
%% @doc
%% COMSA ntpd polling group
%% @end
%%--------------------------------------------------------------------
comsa_ntpd_polling() ->
    [
     ntpd_polling,
     ntpd_polling_no_server_configured,
     ntpd_polling_server_configured_no_reach,
     ntpd_polling_server_configured,
     ntpd_polling_mutiple_servers_configured
    ].

%%--------------------------------------------------------------------
%% @doc
%% CCI PV2 subscribe group
%% @end
%%--------------------------------------------------------------------
cci_pv2_subscribe() ->
    [
     cci_subscribe_2_PV2,
     cci_subscribe_2_PV1,
     cci_subscribe_1_PV1,
     cci_subscribe_2_PV2_already,
     cci_subscribe_2_PV1_already,
     cci_subscribe_1_unsuccesful,
     cci_subscribe_not_supported
    ].


ntp_state_ind_server() ->
    [
     ntp_state_ind_no_server_conf,
     ntp_state_ind_server_conf,
     ntp_state_ind_server_re_conf
    ].

%%--------------------------------------------------------------------
%% @doc
%% CCI state change ntp_state_ind
%% @end
%%--------------------------------------------------------------------
cci_state_change_ntp_state() ->
    [
     ntp_state_out_of_sync_undefined_offset,
	 ntp_state_in_sync_undefined_offset,
	 ntp_state_out_of_sync_defined_offset,
	 ntp_state_in_sync_same_offset,
	 ntp_state_in_sync_different_offset
    ].

%%--------------------------------------------------------------------
%% @doc
%% CCI PV2 clock slew new enum values
%% @end
%%--------------------------------------------------------------------
cci_pv2_clock_slew() ->
	[
	 cci_pv2_clock_slew,
	 cci_pv2_clock_slew_discrepancy
	].
%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc
%% Tests CCI subscribe and unsubscribe
%% @spec cci_subscribe_unsubscribe(_Config) -> ok
%% @end
%%--------------------------------------------------------------------

cci_subscribe_unsubscribe(_Config) ->
    
    %% Subscribe
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),	
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %% Subscribe again - expected to fail
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),	
    waitForSubscribeReject(),
    
    %% Unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),	
    waitForUnsubscribeConfirm(),
    
    %% Unsubscribe again - expected to fail
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),	
    waitForUnsubscribeReject(),
    
    ok.

cci_time_ind(_Config) ->
    
    %% Subscribe
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %% Wait to be sure that the time has not been changed in the last minute
    waitForTimestepInd(0),
    ok = rct_ssh:connect(ssh),
    
    %% Set time 5 seconds in future
    rct_ssh:exec(ssh, "date -s '5 seconds'", 5000, "(.*?)", [global, {capture, all, list}]),
    ok = waitForTimestepIndIgnoreSlew(5 * 1000000),
    
    %% Set time 2 hours in future
    rct_ssh:exec(ssh, "date -s '7200 seconds'", 5000, "(.*?)", [global, {capture, all, list}]),
    ok = waitForTimestepIndIgnoreSlew(7200 * 1000000, 65000), %% extend waiting since it arrives few ms late in some cases
    
    %% Unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),
    waitForUnsubscribeConfirm(),
    
    %% Set time to correct value
    rct_ssh:exec(ssh, "date -s '7205 seconds ago'", 5000, "(.*?)", [global, {capture, all, list}]),
    
    %% Must timeout since we're not subscribed
    timeout = waitForTimestepInd(7205 * 1000000),
    
    rct_ssh:disconnect(ssh),
    
    ok.

%% special function for testing timestep and ignore possible slew notifications
waitForTimestepIndIgnoreSlew(Miliseconds) ->
  waitForTimestepIndIgnoreSlew(Miliseconds, 60000).

waitForTimestepIndIgnoreSlew(Miliseconds, Timeout) ->
    case waitForTimestepInd(Miliseconds, Timeout) of
        ok ->
            ok;
        nok -> %% this could be due to slew, try again
            ct:log("Received time update due to slew, try one more time~n"),
            waitForTimestepInd(Miliseconds, Timeout);
        timeout ->
            timeout
    end.

cci_server_down(_Config) ->
    
    %%subscribe
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %% Stop cch_service (expected to be restarted)
    rct_rpc:call(rpc, gen_server, stop, [cch_service, unexpected_exit, infinity], 10000),
    
    waitForServerDownInd(),
    
    %% Wait to be sure service is restarted
    timer:sleep(5000),
    
    %% Subscribe again
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %% Unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),
    waitForUnsubscribeConfirm(),
    
    ok.

cci_client_down(_Config) ->
    
    %% No subscribers
    Subscribers_empty = rct_rpc:call(rpc, cch_service, get_subscribers, [], 10000),
    0 = maps:size(Subscribers_empty),
    
    %% Subscribe
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %% One registered subscriber
    Subscribers_1 = rct_rpc:call(rpc, cch_service, get_subscribers, [], 10000),
    ct:pal("Subscribers: ~p", [Subscribers_1]),
    1 = maps:size(Subscribers_1),
    
    %% Stop client
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, cci1, 20000),
    timer:sleep(5000),
    
    %% The subscriber should be removed from the list
    Subscribers_empty = rct_rpc:call(rpc, cch_service, get_subscribers, [], 10000),
    0 = maps:size(Subscribers_empty),
    
    %% Start client
    {ok, client_started} = rct_proxy:start_proxy(node1, cci1, ?CCI),
    timer:sleep(5000),
    
    %% Subscribe
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %% One registered subscriber
    Subscribers_2 = rct_rpc:call(rpc, cch_service, get_subscribers, [], 10000),
    ct:pal("Subscribers: ~p", [Subscribers_2]),
    1 = maps:size(Subscribers_2),
    
    %% Unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),
    waitForUnsubscribeConfirm(),
    
    %% The subscriber should be removed from the list
    Subscribers_empty = rct_rpc:call(rpc, cch_service, get_subscribers, [], 10000),
    0 = maps:size(Subscribers_empty),
    
    ok.


%%--------------------------------------------------------------------
%% 
%% Tests CCI subscribe and unsubscribe with PV2 protocol version and PV2 signal.
%% 
%%--------------------------------------------------------------------

%% TC-US1-1
%% Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol version.
%% Check that confirm and time_ind is received. Check that application exists in subscriber list and PV2 is
%% stored. Unsubscribe from CCH.
cci_subscribe_2_PV2(_Config) ->
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    waitForNtpStateInd(),

    %check that pv2 is stored
    ok = check_pv_is_stored(?CCI_PV2),
    
    %unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),   
    waitForUnsubscribeConfirm(),
    
    ok.

%% TC-US1-1a
%% Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV1 protocol version.
%% Check that confirm and time_ind is received. Check that application exists in subscriber list and PV1 is
%% stored. Unsubscribe from CCH.
cci_subscribe_2_PV1(_Config) ->
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV1}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),

    %check that pv1 is stored
    ok = check_pv_is_stored(?CCI_PV1),
    
    %unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),   
    waitForUnsubscribeConfirm(),
    
    ok.

%% TC-US1-1b
%% Node is started. Application subscribes to cch_service with legacy subscribe signal. Check that confirm and
%% time_ind is received. Check that application exists in subscriber list and PV1 is stored. Unsubscribe from CCH.
cci_subscribe_1_PV1(_Config) ->
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %check that pv1 is stored
    ok = check_pv_is_stored(?CCI_PV1),
    
    %unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),   
    waitForUnsubscribeConfirm(),
    
    ok.

%% TC-US1-2
%% Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol version.
%% Check that confirm and time_ind is received. Check that application exists in subscriber list and PV2 is stored.
%% Try to subscribe to PV2 again. Confirm that subscription fails. Try to subscribe to PV1. Confirm that
%% subscription fails. Try to subscribe via legacy PV1. Confirm subscription fails. Unsubscribe from CCH.
cci_subscribe_2_PV2_already(_Config) ->
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    waitForNtpStateInd(),
    
    %check that app exists in subscriber list and pv2 is stored
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}),   
    waitForSubscribeReject(),
    
    %expected to fail on pv1
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV1}),   
    waitForSubscribeReject(),
    
    %expected to fail with pv1 signal
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),   
    waitForSubscribeReject(),
    
    %unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),   
    waitForUnsubscribeConfirm(),
    
    ok.

%% TC-US1-2a
%% Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV1 protocol version.
%% Check that confirm and time_ind is received. Check that application exists in subscriber list and PV1 is stored.
%% Try to subscribe to PV1 again. Confirm that subscription fails. Try to subscribe to PV2. Confirm that
%% subscription fails. Try to subscribe via legacy PV1. Confirm subscription fails. Unsubscribe from CCH.
cci_subscribe_2_PV1_already(_Config) ->
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV1}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %check that app exists in subscriber list and pv1 is stored
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV1}),   
    waitForSubscribeReject(),
    
    %expected to fail on pv2
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}),   
    waitForSubscribeReject(),
    
    %expected to fail with pv1 signal
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),   
    waitForSubscribeReject(),
    
    %unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),   
    waitForUnsubscribeConfirm(),
    
    ok.

%% TC-US1-2b
%% Node is started. Application subscribes to cch_service with legacy subscribe signal. Check that confirm and
%% time_ind is received. Check that application exists in subscriber list and PV1 is stored. Try to subscribe to PV1
%% again. Confirm that subscription fails. Try to subscribe to PV2. Confirm that subscription fails. Unsubscribe from CCH.
cci_subscribe_1_unsuccesful(_Config) ->
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    %check that app exists in subscriber list and pv1 is stored
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),   
    waitForSubscribeReject(),
    
    %expected to fail on pv2
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}),   
    waitForSubscribeReject(),
    
    %expected to fail on pv1
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV1}),   
    waitForSubscribeReject(),
    
    %unsubscribe
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),   
    waitForUnsubscribeConfirm(),
    
    ok.

%% TC-US1-3
%% Node is started. Application subscribes to cch_service with PV2 subscribe signal and unsupported PV.
%% Confirm that subscription is rejected with PV_unsupported error reason.
cci_subscribe_not_supported(_Config) ->
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {6}),   
    waitForUnsupportedPVReject(),
    
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {7}),   
    waitForUnsupportedPVReject(),
    
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {23}),   
    waitForUnsupportedPVReject(),
    
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {31}),   
    waitForUnsupportedPVReject(),
    
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {101}),   
    waitForUnsupportedPVReject(),
    
    ok.


check_pv_is_stored(PV) ->
    
    Map = rct_rpc:call(rpc, cch_service, get_subscribers, [], 10000),
    [Key]= maps:keys(Map),

    %has only one element
    #{Key := Subscriber} = Map,
    
    ct:pal("pv: ~p , subscriber: ~p", [PV, Subscriber]),
    {_,_,_, PV} = Subscriber,

    ok.

%%--------------------------------------------------------------------
%% TEST CASES - ntp_state_ind testcases
%%--------------------------------------------------------------------

%% TC-US2-1    CCI PV2 notification on SIM Node is started. 
%% Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol version. 
%% Check that ntp_state_ind is received with OUT_OF_SYNC state.

%% TC-US2-1a   CCI PV2 notification on target PV1  
%% Node is started. Application subscribes to cch_service with PV1 subscribe signal. 
%% Check that ntp_state_ind is not received

%% TC-US2-1b   CCI PV2 notification on target PV2  
%% Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol version. 
%% Check that ntp_state_ind is received.

ntp_state_ind_initial(_Config) ->
    
    %% Wait to be sure that the time has not been changed in the last minute
    waitForTimestepInd(0),
    
    ct:log("Subscribe with PV1, expect to get confirm and initial timestep indication"),
    {ok, ?CCI_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_SUBSCRIBE_REQ, {}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    ct:log("Expect timeout while waiting for ntp_state_ind"),
    timeout = waitForNtpStateInd(5000),
    
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),
    waitForUnsubscribeConfirm(),
    
    
    ct:log("Subscribe with PV2, expect to get confirm and initial timestep indication"),
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    
    ct:log("Expect to get ntp_state_ind"),
    case cci_c_lib:is_target() of
        true ->
            {_NtpSyncState, _Offset} = waitForNtpStateInd();
        false ->
            {?CCI_NTP_OUT_OF_SYNC, 0} = waitForNtpStateInd() %% SIM will always be OUT_OF_SYNC
    end,
    
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),
    waitForUnsubscribeConfirm(),
    
    ok.

%%--------------------------------------------------------------------
%% TEST CASES - ntp_state_ind timestep
%%--------------------------------------------------------------------

%% TC-US2-4    Update offset after timestep
%% Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol version.
%% CCH is IN_SYNC. Change time on node. Check that new offset is obtained after time_ind is received.
%% Check that IN_SYNC notification is sent.
ntp_state_ind_timestep(_Config) ->
    
    ct:log("Subscribe with PV2, expect to get confirm, initial timestep indication and initial NTP state indication"),
    {ok, ?CCI2_SUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI2_SUBSCRIBE_REQ, {?CCI_PV2}),
    waitForSubscribeConfirm(),
    waitForInitialTimestepInd(),
    {?CCI_NTP_IN_SYNC, _} = waitForNtpStateInd(),
    
    %% Wait to be sure that the time has not been changed in the last minute
    waitForTimestepInd(0),
    waitForNtpStateInd(1000),
    ok = rct_ssh:connect(ssh),
    
    %% Set time 5 seconds in future
    rct_ssh:exec(ssh, "date -s '5 seconds'", 5000, "(.*?)", [global, {capture, all, list}]),
    ok = waitForTimestepIndIgnoreSlew(5 * 1000000),
    
    ct:log("When we get timestep we expect ntp_state_ind to be sent as well~n"),
    {?CCI_NTP_IN_SYNC, _} = waitForNtpStateInd(1000), 
    
    %% Set time 5 seconds back
    rct_ssh:exec(ssh, "date -s '5 seconds ago'", 5000, "(.*?)", [global, {capture, all, list}]),
    ok = waitForTimestepIndIgnoreSlew(5 * 1000000, 65000),
    
    ct:log("When we get timestep we expect ntp_state_ind to be sent as well~n"),
    {?CCI_NTP_IN_SYNC, _} = waitForNtpStateInd(1000), 
    
    {ok, ?CCI_UNSUBSCRIBE_REQ} = rct_proxy:send_proxy(node1, cci1, ?CCI_UNSUBSCRIBE_REQ, {}),
    waitForUnsubscribeConfirm(),
    
    rct_ssh:disconnect(ssh),
    
    ok.

%%--------------------------------------------------------------------
%% TEST CASES - time slew group
%%--------------------------------------------------------------------

%% TC-US0-2: CCI time slew algorithm - undefined 
%% Node is started. Test algorithm with 'undefined' value.
%% Check that CCI ignores the value and continues to work as intended
cci_time_slew_alg_undefined(_Config) ->
    CchState = get_cch_service_state(),
    
    %% Undefined value is ignored, state stays the same 
    ct:log("New offset value: undefined, expect state to stay the same"),
    CchState = test_cci_time_slew_alg(undefined, CchState),
    
    ct:log("Notification shouldn't be sent, expect timeout"),
    timeout = waitForTimestepInd(undefined, 5000),
    
    ok.

%% TC-US0-2a: CCI time slew algorithm - initial offset
%% Node is started. Test algorithm with initial valid value. 
%% Check that this is setup as initial offset.
cci_time_slew_alg_init_offset(_Config) ->
    CchState = get_initial_state(),
    
    _CchState2 = set_check_offset(2000, CchState),
    
    ct:log("Notification shouldn't be sent, expect timeout"),
    timeout = waitForTimestepInd(undefined, 5000),
    
    ok.

%% TC-US0-2b: CCI time slew algorithm - same offset
%% Node is started. Test algortihm by providing same value as initial. 
%% Check that value is ignored.
cci_time_slew_alg_same_offset(_Config) ->
    CchState = get_initial_state(),
    
    CchState2 = set_check_offset(30000, CchState),
    
    CchState2 = set_check_offset(30000, CchState2),
    
    ct:log("Notification shouldn't be sent, expect timeout"),
    timeout = waitForTimestepInd(undefined, 5000),
    
    ok.

%% TC-US0-2c: CCI time slew algorithm - different offset
%% Node is started. Test algorithm with value different from the initial. 
%% Check that diff is accumulated and new offset is set as initial.
cci_time_slew_alg_diff_offset(_Config) ->
    CchState = get_initial_state(),
    
    CchState2 = set_check_offset(2000, CchState),
    
    Delta1 = get_state_param(cumulativeDelta, CchState2),
    
    CchState3 = set_check_offset(3000, CchState2),
    
    Delta2 = get_state_param(cumulativeDelta, CchState3),
    
    ct:log("Accumulated diff should be 1000"),
    1000 = Delta2 - Delta1,

    ct:log("Notification shouldn't be sent, expect timeout"),
    timeout = waitForTimestepInd(undefined, 5000),
    
    ok.

%% TC-US0-2d   CCI time slew algorithm - notification sent 
%% Node is started. Test algorithm with value different from the initial offset by more than 50 ms. 
%% Check that notification is sent, accumulated diff is 0 and new offset is set as initial.
cci_time_slew_alg_notification(_Config) ->
    CchState = get_initial_state(),
    
    CchState2 = set_check_offset(2000, CchState),
    
    CchState3 = set_check_offset(60000, CchState2),
    
    ct:log("Notification should be sent"),
    ok = waitForTimestepInd(58000, 5000),
    
    ct:log("Accumulated diff should be 0"),
    0 = get_state_param(cumulativeDelta, CchState3),
    
    ok.

%% TC-US0-2e: CCI time slew algorithm - offset over tinker step
%% Node is started.Test algorithm with value different than initial which difference is over tinker step. 
%% Check that notification is not sent since this is handled by timestep, and new offset is set as initial.
cci_time_slew_alg_over_tinker_step(_Config) ->
    CchState = get_initial_state(),
    
    CchState2 = set_check_offset(?CCI_TINKER_STEP, CchState),
    
    timeout = waitForTimestepInd(undefined, 5000),
    
    _CchState3 = set_check_offset(-20000, CchState2),
    
    ct:log("Notification shouldn't be sent, expect timeout"),
    timeout = waitForTimestepInd(undefined, 5000),
    
    ok.




%%--------------------------------------------------------------------
%% TEST CASES - CCI ntpd polling test group
%%--------------------------------------------------------------------

%% TC-US0-1: CCI ntpd polling started on target/SIM
%% Node is started. Check that CCI has started on target to poll ntpd via comsaNtpServer.
%% Check that CCI has not started to poll ntpd via comsaNtpServer.
ntpd_polling_started(_Config) ->

    CchState = get_cch_service_state(),

    case cci_c_lib:is_target() of
        %% runs on target
        true ->
            case get_state_param(timer, CchState) of
                undefined -> 
                    ct:fail("CCI has not started to poll ntpd on target.");
                _ ->
                    ct:pal("CCI has started to poll ntpd on target.")
            end;
        %% runs on sim
        false ->
            case get_state_param(timer, CchState) of
                undefined -> 
                    ct:pal("CCI has not started to poll ntpd on SIM.");
                _ ->
                    ct:fail("CCI has started to poll ntpd on SIM.")
            end
    end,

    ok.

%%--------------------------------------------------------------------
%% TEST CASES - COMSA ntpd polling group
%%--------------------------------------------------------------------

%% TC-US0-3: Node is started. Poll ntpd via comsaNtpServer. Check that the result is returned on target. Check
%% that the result is not returned on sim.
ntpd_polling(Config) ->

    Result = rct_rpc:call(rpc, comsaNtpServer, cci_get_ntpd_sync_source_state, [], 10000),

    case cci_c_lib:is_target() of
        %% runs on target
        true ->
            case Result of
                {sync_source, _NtpdVars} ->
                    ok;
                no_sync_source ->
                    ok
            end;
        %% runs on sim
        false ->
            case Result of
                {badrpc, {'EXIT', _}} ->
                    ok
            end
    end,

    Config.


%% TC-US0-3b: Node is started. Poll ntpd via comsaNtpServer. Check that the result holds 
%% no information.
ntpd_polling_no_server_configured(Config) ->
    ct:log("Setting administrativeState to 'LOCKED?"),

    ok = cci_c_lib:set_ntp_element(administrativeState, "LOCKED"),

    no_sync_source = rct_rpc:call(rpc, comsaNtpServer, cci_get_ntpd_sync_source_state, [], 10000),

    ct:log("Reverting to old administrativeState"),
    ok = cci_c_lib:set_ntp_element(administrativeState, "UNLOCKED"),

    Config. 

%% TC-US0-3c: Node is started. Configure invalid NTP server address. Poll ntpd via comsaNtpServer.
%% Check that the result holds no information.
ntpd_polling_server_configured_no_reach(Config) ->
    ct:log("Fetching serverAddress"),

    ServerAddress = cci_c_lib:get_ntp_element(serverAddress),
    [A, B, C, D] = string:tokens(ServerAddress, "."),

    NewServerAddress = string:join([A, B, integer_to_list(list_to_integer(C) + 2), D], "."),

    ct:log("Setting serverAddress to ~p", [NewServerAddress]),

    ok = cci_c_lib:set_ntp_element(serverAddress, NewServerAddress),
    no_sync_source = rct_rpc:call(rpc, comsaNtpServer, cci_get_ntpd_sync_source_state, [], 10000),

    ct:log("Reverting to old serverAddress"),
    ok = cci_c_lib:set_ntp_element(serverAddress, ServerAddress),

    Config.

%% TC-US0-3d: Node is started. Configure valid NTP server address. Poll ntpd via comsaNtpServer. 
%% Check that the result is returned as expected
ntpd_polling_server_configured(Config) ->
    {sync_source, _NtpdVars} = rct_rpc:call(rpc, comsaNtpServer, cci_get_ntpd_sync_source_state, [], 10000),

    Config.

%% TC-US0-3e: Node is started. Configure multiple valid NTP server addresses. Poll ntpd via 
%% comsaNtpServer. Check that the result is returned as expected.
ntpd_polling_mutiple_servers_configured(Config) ->
    ct:log("Fetching serverAddress"),

    ServerAddress = cci_c_lib:get_ntp_element(serverAddress),
    [A, B, C, D] = string:tokens(ServerAddress, "."),
    NewServerAddress = string:join([A, B, C, integer_to_list(list_to_integer(D) + 1)], "."),

    ct:log("Creating new NtpServer"),

    ok = cci_c_lib:create_ntp_server_mo("UNLOCKED", "2", NewServerAddress, "nesto"),

    ServerAddress2 = cci_c_lib:get_ntp_element(serverAddress, "2"),
    ct:pal("Second server address is: ~p", [ServerAddress2]),

    {sync_source, _NtpdVars} = rct_rpc:call(rpc, comsaNtpServer, cci_get_ntpd_sync_source_state, [], 10000),

    ct:log("Deleting second NtpServer"),
    ok = cci_c_lib:delete_ntp_server_mo("2"),

    Config.


%%--------------------------------------------------------------------
%% TEST CASES - ntp state indications for server
%%--------------------------------------------------------------------

%% TC-US2-3: Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol
%% version. CCH is IN_SYNC. LOCK NtpServer. Check that OUT_OF_SYNC notification is sent.
ntp_state_ind_no_server_conf(_Config) ->
    State = get_cch_service_state(),
    %check that CCH is IN_SYNC
    ?CCI_NTP_IN_SYNC = get_state_param(sync, State),

    ct:log("Setting administrativeState to 'LOCKED?"),

    ok = cci_c_lib:set_ntp_element(administrativeState, "LOCKED"),

    %check that CCH is OUT_OF_SYNC
    {?CCI_NTP_OUT_OF_SYNC, _} = waitForNtpStateInd(),

    ct:log("Reverting to old administrativeState"),
    ok = cci_c_lib:set_ntp_element(administrativeState, "UNLOCKED"),
    
    %ignoring notification
    waitForNtpStateInd(),

    ok.


%% TC-US2-3a: Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol
%% version. CCH is OUT_OF_SYNC. UNLOCK NtpServer. Check that IN_SYNC notification is sent.
ntp_state_ind_server_conf(_Config) ->
    State = get_cch_service_state(),
    %check that CCH is OUT_OF_SYNC
    ?CCI_NTP_OUT_OF_SYNC = get_state_param(sync, State),

    ct:log("Reverting to old administrativeState"),
    ok = cci_c_lib:set_ntp_element(administrativeState, "UNLOCKED"),

    %check that CCH is IN_SYNC
    {?CCI_NTP_IN_SYNC, _} = waitForNtpStateInd(),

    ok.


%% TC-US2-3b: Node is started. Application subscribes to cch_service with PV2 subscribe signal and PV2 protocol
%% version. CCH is IN_SYNC. Add new NtpServer. Check that OUT_OF_SYNC is first received. Check that IN_SYNC notification
%% is sent after reconfiguration.
ntp_state_ind_server_re_conf(_Config) ->
    State = get_cch_service_state(),
    %check that CCH is IN_SYNC
    ?CCI_NTP_IN_SYNC = get_state_param(sync, State),

    ct:log("Fetching serverAddress"),

    ServerAddress = cci_c_lib:get_ntp_element(serverAddress),
    [A, B, C, D] = string:tokens(ServerAddress, "."),
    NewServerAddress = string:join([A, B, C, integer_to_list(list_to_integer(D) + 1)], "."),
    
    ct:log("Creating new NtpServer"),
    ok = cci_c_lib:create_ntp_server_mo("UNLOCKED", "2", NewServerAddress, "nesto", 1000),

    ServerAddress2 = cci_c_lib:get_ntp_element(serverAddress, "2"),
    ct:pal("Second server address is: ~p", [ServerAddress2]),

    %check that CCH is OUT_OF_SYNC
    {?CCI_NTP_OUT_OF_SYNC, _} = waitForNtpStateInd(),

    timer:sleep(10000),
    %check that CCH is IN_SYNC
    {?CCI_NTP_IN_SYNC, _} = waitForNtpStateInd(),

    ct:log("Deleting second NtpServer"),
    ok = cci_c_lib:delete_ntp_server_mo("2"),

    %waiting for out/in sync notification, ignoring it
    waitForNtpStateInd(),
    waitForNtpStateInd(),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES - CCI state change ntp_state_ind group
%%--------------------------------------------------------------------

%% TC-US2-2: ntp_state_ind OUT_OF_SYNC -> OUT_OF_SYNC
%% Node is started. Application subscribes to cch_service with PV2 
%% subscribe signal and PV2 protocol version. CCH is already OUT_OF_SYNC.
%% CCH receives undefined offset. Check that no notification is sent.


ntp_state_out_of_sync_undefined_offset(_Config) ->

    CchState = get_initial_state(),
    CchState2 = erlang:setelement(9, CchState, ?CCI_NTP_OUT_OF_SYNC),

    rct_rpc:call(rpc, cch_service, send_ntp_state_ind, [undefined, CchState2], 10000),
    ct:log("Notification shouldn't be sent, expect timeout"),
    timeout = waitForNtpStateInd(5000),

    ok.

%% TC-US2-2a: ntp_state_ind IN_SYNC -> OUT_OF_SYNC
%% Node is started. Application subscribes to cch_service with PV2
%% subscribe signal and PV2 protocol version. CCH is IN_SYNC. CCH 
%% receives undefined offset. Check that OUT_OF_SYNC notification
%% is sent.

ntp_state_in_sync_undefined_offset(_Config) ->

    CchState = get_initial_state(),
    CchState2 = erlang:setelement(9, CchState, ?CCI_NTP_IN_SYNC),

    rct_rpc:call(rpc, cch_service, send_ntp_state_ind, [undefined, CchState2], 10000),
    ct:log("NTP_OUT_OF_SYNC notification should be sent"),
    {?CCI_NTP_OUT_OF_SYNC, _} = waitForNtpStateInd(),

    ok.

%% TC-US2-2b: ntp_state_ind OUT_OF_SYNC -> IN_SYNC
%% Node is started. Application subscribes to cch_service with PV2 
%% subscribe signal and PV2 protocol version. CCH is OUT_OF_SYNC. 
%% CCH receives offset. Check that IN_SYNC notification is sent with
%% that offset.

ntp_state_out_of_sync_defined_offset(_Config) ->

    CchState = get_initial_state(),
    CchState2 = erlang:setelement(9, CchState, ?CCI_NTP_OUT_OF_SYNC),

    rct_rpc:call(rpc, cch_service, send_ntp_state_ind, [2000, CchState2], 10000),
    ct:log("NTP_IN_SYNC notification should be sent"),
    {?CCI_NTP_IN_SYNC, _} = waitForNtpStateInd(),

    ok.

%% TC-US2-2c: ntp_state_ind IN_SYNC -> IN_SYNC, same offset
%% Node is started. Application subscribes to cch_service with PV2 
%% subscribe signal and PV2 protocol version. CCH is IN_SYNC. CCH 
%% receives the same offset. Check that notification is not sent.

ntp_state_in_sync_same_offset(_Config) ->

    CchState = get_initial_state(),
                
    ct:log("Setting sync attribute in state to NTP_IN_SYNC"),
    CchState2 = erlang:setelement(9, CchState, ?CCI_NTP_IN_SYNC), % state has 8 elements in cch_service, sync is last

    ct:log("Setting ntp_offset attribute in state to 2000"),
    CchState3 = erlang:setelement(5, CchState2, 2000),
                
    ct:log("Sending ntp_state_ind with same offset"),
    rct_rpc:call(rpc, cch_service, send_ntp_state_ind, [2000, CchState3], 10000),
                
    ct:log("Notification shouldn't be sent, expect timeout"),
    timeout= waitForNtpStateInd(5000),

    ok.

%% TC-US2-2d: ntp_state_ind IN_SYNC -> IN_SYNC, diff offset
%% Node is started. Application subscribes to cch_service with PV2 
%% subscribe signal and PV2 protocol version. CCH is IN_SYNC. CCH 
%% receives different offset. Check that IN_SYNC notification is 
%% sent with new offset.

ntp_state_in_sync_different_offset(_Config) ->
    CchState = get_initial_state(),
                
    ct:log("Setting sync attribute in state to NTP_IN_SYNC"),
    CchState2 = erlang:setelement(9, CchState, ?CCI_NTP_IN_SYNC), % state has 8 elements in cch_service, sync is last
    
    ct:log("Setting ntp_offset attribute in state to 2000"),
    CchState3 = erlang:setelement(5, CchState2, 2000),
               
    ct:log("Sending ntp_state_ind with different offset"),
    rct_rpc:call(rpc, cch_service, send_ntp_state_ind, [3000, CchState3], 10000),
                
    ct:log("Notification is sent, expecting NTP_IN_SYNC and offset"),
    {?CCI_NTP_IN_SYNC, _} = waitForNtpStateInd(),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES - CCI PV2 clock slew new enum group
%%--------------------------------------------------------------------

%% TC-US3-1: CCI PV2 clock slew
%% Node is started. Application subscribes to cch_service with PV2 subscribe
%% signal and PV2 protocol version. Provide initial offset value of 0. After
%% 180 seconds provide new offset value of 60. Check that time_update_ind is
%% sent with reason SLEW.

cci_pv2_clock_slew(_Config) ->

    CchState = get_initial_state(),
	
	ct:pal("Setting ntp_offset attribute in state to 0"),
    CchState2 = test_cci_time_slew_alg(0, CchState),

	ct:pal("Waiting 180 seconds"),
	timer:sleep(180 * 1000),
	
	ct:pal("Setting ntp_offset attribute in state to 60"),
	test_cci_time_slew_alg(60 * 1000, CchState2),

	ct:pal("Expecting time_update_ind is sent with reason SLEW"),
	ok = waitForTimestepInd(60000, 5000, ?CCI_REASON_SLEW),

    ok.

%% TC-US3-2: CCI PV2 clock slew discrepancy
%% Node is started. Application subscribes to cch_service with PV2 subscribe
%% signal and PV2 protocol version. Provide initial offset value of 0. After
%% 60 seconds provide new offset value of 60. Check that time_update_ind is
%% sent with reason SLEW_DISCREPANCY.

cci_pv2_clock_slew_discrepancy(_Config) ->

    CchState = get_initial_state(),
	
	ct:pal("Setting ntp_offset attribute in state to 0"),
	CchState2 =  test_cci_time_slew_alg(0, CchState),

	ct:pal("Waiting 60 seconds"),
	timer:sleep(60 * 1000),
	
	ct:pal("Setting ntp_offset attribute in state to 60"),
	test_cci_time_slew_alg(60 * 1000, CchState2),

	ct:pal("Expecting time_update_ind is sent with reason SLEW_DISCREPANCY"),
	ok = waitForTimestepInd(60000, 5000, ?CCI_REASON_SLEW_DISCREPANCY),

    ok.

%%---------------------------------------------------------------------------------------------------------

receive_msg(Pattern) ->
    case rct_proxy:receive_proxy() of 
        Pattern ->
            {ok, pattern_matched};
        {ok, {ok, ?CCI_TIME_UPDATE_IND, Reason, Timediff}} ->
            ct:pal("Received time update with reason ~p: [~p us]~n", [Reason, Timediff]),
            receive_msg(Pattern);
        {ok, {ok, ?CCI2_NTP_STATE_IND, NtpSyncState, Offset}} ->
            ct:pal("Received ntp_state_ind with state [~p] and offset [~p ms]~n", [NtpSyncState, Offset]),
            receive_msg(Pattern);
        Received ->
            {nok, Received}
    end.

waitForSubscribeConfirm() ->
    {ok, pattern_matched} = receive_msg({ok, {ok, ?CCI_SUBSCRIBE_CFM}}),
    ct:pal("Received CCI subscribe confirm").

waitForInitialTimestepInd() ->
    {ok, {ok, ?CCI_TIME_UPDATE_IND, ?CCI_REASON_INITIAL, 0}} = rct_proxy:receive_proxy(),
    ct:pal("Received CCI initial timestep indication").

waitForSubscribeReject() ->
    {ok, pattern_matched} = receive_msg({ok, {ok, ?CCI_SUBSCRIBE_REJ, ?CCI_REJ_ALREADY_SUBSCRIBED}}),
    ct:pal("Received CCI subscribe reject").

waitForUnsupportedPVReject() ->
    {ok, pattern_matched} = receive_msg({ok, {ok, ?CCI_SUBSCRIBE_REJ, ?CCI_REJ_UNSUPPORTED_PV}}),
    ct:pal("Received CCI subscribe reject").

waitForUnsubscribeConfirm() ->
    {ok, pattern_matched} = receive_msg({ok, {ok, ?CCI_UNSUBSCRIBE_CFM}}),
    ct:pal("Received CCI unsubscribe confirm").

waitForUnsubscribeReject() ->
    {ok, pattern_matched} = receive_msg({ok, {ok, ?CCI_UNSUBSCRIBE_REJ, ?CCI_REJ_ALREADY_UNSUBSCRIBED}}),
    ct:pal("Received CCI unsubscribe reject").

waitForTimestepInd(MicroSeconds) ->
    waitForTimestepInd(MicroSeconds, 60000).

waitForTimestepInd(MicroSeconds, Timeout) ->
	waitForTimestepInd(MicroSeconds, Timeout, ?CCI_REASON_STEP).

waitForTimestepInd(MicroSeconds, Timeout, Reason) ->
    case rct_proxy:receive_proxy(Timeout) of 
        {ok, {ok, ?CCI_TIME_UPDATE_IND, Reason, Timediff}} ->
            ct:pal("Received time update with reason ~p: [~p us]~n", [Reason, Timediff]),
            evaluateTimediff(MicroSeconds , Timediff);
        {error, timeout} ->
            timeout;
        _ ->
            nok
    end.

waitForServerDownInd() ->
    {ok, pattern_matched} = receive_msg({ok, {ok, ?CCI_SERVER_DOWN_IND}}),
    ct:pal("Received CCI server down indication").

waitForNtpStateInd() ->
    waitForNtpStateInd(60000).

waitForNtpStateInd(Timeout) ->
    case rct_proxy:receive_proxy(Timeout) of 
        {ok, {ok, ?CCI2_NTP_STATE_IND, NtpSyncState, Offset}} ->
            ct:log("Received ntp_state_ind with state [~p] and offset [~p ms]~n", [NtpSyncState, Offset]),
            {NtpSyncState, Offset};
        {error, timeout} ->
            timeout;
        _ ->
            nok
    end.

%% Max diff is 1 hour
evaluateTimediff(Expected, Received) when Expected > ?CCI_MAX_DELTA ->
    if
        Received == ?CCI_DELTA_TOO_BIG ->
            ok;
        true ->
            nok
    end;

evaluateTimediff(Expected, Received) ->
    if 
        Expected - Expected * 0.01 < Received, Expected + Expected * 0.01 > Received ->
            ok;
        true ->
            nok
    end.

waitForNodeUp() ->
    waitForNodeUp(600000).

waitForNodeUp(Timeout) when Timeout < 0 ->
    ct:fail("The node is still down");

waitForNodeUp(Timeout) ->
    timer:sleep(5000),
    case rct_rpc:call(rpc, appmServer, get_apps, [], 10000) of
        {error, _Reason} ->
            waitForNodeUp(Timeout - 5000);
        {badrpc, _Reason} ->
            waitForNodeUp(Timeout - 5000);
        _ ->
            ct:pal("The node is up!")
    end.


%% Gets state record from cch_service
get_cch_service_state() ->
    get_cch_service_state(rpc).

get_cch_service_state(Rpc) ->
    rct_rpc:call(Rpc, gen_server, call, [cch_service, {cch_service, get_state}], 10000).

%% Calls time slew algorithm and returns state
test_cci_time_slew_alg(NewOffset, State) ->
    test_cci_time_slew_alg(rpc, NewOffset, State).

test_cci_time_slew_alg(Rpc, NewOffset, State) ->
    %% FIXME: Uncomment when implemented
    NewState = rct_rpc:call(Rpc, cch_service, calculate_clock_slew, [NewOffset, State], 10000),
    NewState.

%% get_state_param(Parameter) ->
%%     get_state_param(Parameter, get_cch_service_state()).

%% Gets parameter from cch_service state
%% FIXME: This is currently implemented a bit hacky, 
%% when record is completely defined in PV2
%% define record in test_cci.hrl
get_state_param(cumulativeDelta, State) ->
    element(6, State);
get_state_param(ntpOffset, State) ->
    element(5, State);
get_state_param(timer, State) ->
    element(8, State);
get_state_param(sync, State) ->
    element(9, State);
get_state_param(Param, _State) ->
    ct:fail("No ~p parameter defined in state or not implemented yet", [Param]).

set_check_offset(Offset, State) ->
    CchState = test_cci_time_slew_alg(Offset, State),
    
    Offset = get_state_param(ntpOffset, CchState),
    
    CchState.

get_initial_state() ->
    CchState = get_cch_service_state(),
    
    undefined = get_state_param(ntpOffset, CchState),

    CchState.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	vii_c_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/1
%%%
%%% @doc ==Basic Test Suite for the VII legacy C interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(vii_c_SUITE).
-vsn('/main/R2A/R3A/R4A/R6A/1').
-date('2016-09-02').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R2A/3      2013-02-29 etxkols     Added rct_core hook
%%% R2A/4      2013-04-17 etxjovp     change timetrap to 30
%%% R3A/1      2014-11-15 erarafo     WP 3722: Status LED, work in progress
%%% R3A/2      2014-11-16 erarafo     WP 3722: Status LED, work in progress
%%% R3A/3      2015-02-18 etxarnu     Added {rct_rpc, rpc} to ct_hooks
%%% R3A/4      2015-02-19 erarafo     Reverted to R3A/2
%%% R3A/5      2015-02-19 erarafo     Trigger another release attempt
%%% R3A/6      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-09-17 etxpejn     Added cluster sim groups
%%% R6A/1      2016-09-02 etxpen0     Remove test case vii_fault_indicator/1
%%% ----------------------------------------------------------

%%% TODO, the implementation of WP3722 causes the status LED to be
%%% steady_on for a freshly installed node, since there is a
%%% license key alarm raised. The four individual test cases may
%%% need rework due to this, especially so the vii_status_indicator/1
%%% test case.

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0,
         all/0]).

-export([vii_operational_indicator/1,
	 vii_status_indicator/1,
	 vii_maintenance_indicator/1]).

-include_lib("common_test/include/ct.hrl").
-include("test_vii.hrl").

suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]
					      }}]}},
		 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
                 {rct_core,[]}
		]}].

%% @hidden
init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, vii1, ?VII),
    Config.

%% @hidden
end_per_suite(_Config) ->
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, vii1),
    ok = rct_proxy:exit_master(node1).

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
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade_short__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},
     {sdc__qual__all__1__group, [], []},
     %% This suite can be run on both MPs within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]},
     {sbc__cluster__dual_sim_3__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     vii_operational_indicator,
     vii_status_indicator,
     vii_maintenance_indicator
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%
%% @vii_operational_indicator(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
vii_operational_indicator(_Config) ->
    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_NO_POWER}),
    ok = check_leds([{operational, off},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_POWER}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_BACKUP_START}),
    ok = check_leds([{operational, double_flash_off},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_BACKUP_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_BOOTTEST_START}),
    ok = check_leds([{operational, fast_blink},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_BOOTTEST_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),
    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_LOADTEST_START}),
    ok = check_leds([{operational, fast_blink},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_LOADTEST_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_MISSING_RESOURCE_START}),
    ok = check_leds([{operational, slow_blink},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_MISSING_RESOURCE_END}),

    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%%
%% @vii_status_indicator(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
vii_status_indicator(_Config) ->
    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_REMOTE_UNIT_FAULT_START}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      slow_blink}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_REMOTE_UNIT_FAULT_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_NODE_FAULT_START}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      steady_on}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_NODE_FAULT_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%
%% @vii_maintenance_indicator(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
vii_maintenance_indicator(_Config) ->
    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_MEDIUM_BUTTON_PRESS_START}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, slow_blink},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_MEDIUM_BUTTON_PRESS_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_SHORT_BUTTON_PRESS_START}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, fast_blink},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_SHORT_BUTTON_PRESS_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_BOARD_LOCKED}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, steady_on},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_BOARD_UNLOCKED}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_SHUTDOWN_START}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, slow_blink},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_SHUTDOWN_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_ALARM_SUPPRESS_START}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, fast_blink},
		     {status,      off}]),

    {ok, ?CELLO_VII_SUCCESS} = rct_proxy:send_proxy(node1, vii1,
						    ?Vii_visualIndRequest,
						    {?CELLO_VII_ALARM_SUPPRESS_END}),
    ok = check_leds([{operational, steady_on},
		     {fault,       off},
		     {maintenance, off},
		     {status,      off}]),

    ok.

%%% ----------------------------------------------------------
%%% @doc Clause 2 is provisional: Accept any behavior for the
%%% status indicator while WP 3722 (Status LED) is work in
%%% progress.
%%% @end
%%% ----------------------------------------------------------

check_leds([]) ->
    ok;

check_leds([{Indicator, WantedLedBehavior}|T]) when Indicator =:= status ->
    {ok, ActualBehaviorCode} =
	rct_proxy:send_proxy(node1, vii1, ?Vii_visualIndGet, {indicatorCode(Indicator)}),
    ct:pal("indicator: ~w, behavior wanted: ~w, actual: ~w",
	   [Indicator, WantedLedBehavior, behaviorName(ActualBehaviorCode)]),
    check_leds(T);

check_leds([{Indicator, WantedLedBehavior}|T]) ->

    ViiIndicator = indicatorCode(Indicator),

    ViiWantedLedBehavior =
	case WantedLedBehavior of
	    off              -> ?CELLO_VII_LED_OFF;
	    steady_on        -> ?CELLO_VII_LED_ON;
	    slow_blink       -> ?CELLO_VII_LED_SLOW_BLINK;
	    fast_blink       -> ?CELLO_VII_LED_FAST_BLINK;
	    double_flash_off -> ?CELLO_VII_LED_DOUBLE_FLASH_OFF;
	    double_flash_on  -> ?CELLO_VII_LED_DOUBLE_FLASH_ON
	end,

    case rct_proxy:send_proxy(node1, vii1, ?Vii_visualIndGet, {ViiIndicator}) of
	{ok, ViiWantedLedBehavior} ->
	    check_leds(T);
	{ok, ViiLedBehavior} ->
	    ct:pal("mismatch: indicator: ~w, behavior: ~w",
		   [Indicator, behaviorName(ViiLedBehavior)]),
	    {Indicator, behaviorName(ViiLedBehavior)}
    end.


indicatorCode(operational) ->
    ?CELLO_VII_LED_OPERATIONAL;
indicatorCode(fault) ->
    ?CELLO_VII_LED_FAULT;
indicatorCode(status) ->
    ?CELLO_VII_LED_STATUS;
indicatorCode(maintenance) ->
    ?CELLO_VII_LED_MAINTENANCE.


behaviorName(?CELLO_VII_LED_OFF) ->
    off;
behaviorName(?CELLO_VII_LED_ON) ->
    steady_on;
behaviorName(?CELLO_VII_LED_SLOW_BLINK) ->
    slow_blink;
behaviorName(?CELLO_VII_LED_FAST_BLINK) ->
    fast_blink;
behaviorName(?CELLO_VII_LED_DOUBLE_FLASH_OFF) ->
    double_flash_off;
behaviorName(?CELLO_VII_LED_DOUBLE_FLASH_ON) ->
    double_flash_on.

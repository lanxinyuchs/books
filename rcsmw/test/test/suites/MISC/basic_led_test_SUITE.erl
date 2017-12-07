%% coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	basic_led_test_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/2
-module(basic_led_test_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/2').
-date('2014-10-02').
-author('etxivri').
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
%%% R1A/2      2014-10-02  etxivri     Created

 
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
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0,
	 turn_off_leds/1,
	 yellow_test/1,
	 blue_test/1,
	 green_test/1,
	 red_test/1,
	 blink_slow_yellow_test/1,
	 blink_slow_blue_test/1,
	 blink_slow_green_test/1
	]).
-include_lib("common_test/include/ct.hrl").

-define(TIME, 5000).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks:
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_consserv,cs1},
                 {rct_rs232,console}, 
		 {led_rs232,led_console},
		 {rct_rpc, rpc},
		 {rct_ssh,{ssh,[manual_connect]}},
		 %% {cth_conn_log,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],[]}}]}},
                 {rct_core,[]}]}].


%% @hidden
init_per_suite(Config) ->
    led_rs232:login(led_console),
    led_analyze_lib:load_led_client_to_testnode(rpc),
    Config.


%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Client = led_analyze_lib:start_led_client(rpc),
    [{clients,[Client]}|Config].

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    Clients = ?config(clients, _Config),
    led_analyze_lib:stop_led_client(rpc, Clients) ,
    ok.

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------

all() -> 
    [yellow_test, 
     blue_test, 
     green_test, 
     red_test,
     blink_slow_yellow_test,
     blink_slow_blue_test,
     blink_slow_green_test].
     %% [all_led_test].

%%--------------------------------------------------------------------
%% @doc 
%% Groups.<br/><br/>
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].



%%%%%%%%%%%%%
%% Led Test
%%%%%%%%%%%%%

%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec yellow_test(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
yellow_test(Config) ->
    ct:pal("# Yellow Test"),
    [Client| _] = ?config(clients, Config),
    ct:pal("# Check Yellow Led, on"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[status,steady_on]], 10000, noprint),
    timer:sleep(?TIME),
    yellow_on = led_analyze_lib:getrgbi_fiber_1(),
    ct:pal("Yellow: On",[]),

    %% test_server:break("A"),

    ct:pal("Check Yellow Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[status,off]], 10000, noprint),
    timer:sleep(?TIME),
    yellow_off = led_analyze_lib:getrgbi_fiber_1(),
    ct:pal("Yellow: Off",[]),
    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec blue_test(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
blue_test(Config) ->
    ct:pal("# blue Test"),
    [Client| _] = ?config(clients, Config),
    ct:pal("# Check Blue Led, on"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[maintenance,steady_on]], 10000, noprint),
    timer:sleep(?TIME),
    blue_on = led_analyze_lib:getrgbi_fiber_2(),
    ct:pal("Blue: On",[]),

    ct:pal("Check Blue Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[maintenance,off]], 10000, noprint),
    timer:sleep(?TIME),
    blue_off = led_analyze_lib:getrgbi_fiber_2(),
      ct:pal("Blue: Off",[]),
    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec green_test(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
green_test(Config) ->
    [Client| _] = ?config(clients, Config),
    ct:pal("# Check Green Led, on"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[operational,steady_on]], 10000, noprint),
    timer:sleep(?TIME),
    green_on = led_analyze_lib:getrgbi_fiber_3(),
    ct:pal("Green: On",[]),

    ct:pal("# Green Test"),
    [Client| _] = ?config(clients, Config),
    ct:pal("Check Green Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[operational,off]], 10000, noprint),
    timer:sleep(?TIME),
    green_off = led_analyze_lib:getrgbi_fiber_3(),
    ct:pal("Green: Off",[]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec red_test(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
red_test(Config) ->
    ct:pal("# Red Test"),
    [Client| _] = ?config(clients, Config),
    ct:pal("# Check Red Led, on"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[fault,steady_on]], 10000, noprint),
    timer:sleep(?TIME),
    red_on = led_analyze_lib:getrgbi_fiber_4(),
    ct:pal("Red: On",[]),

    ct:pal("Check Red Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[fault,off]], 10000, noprint),
    timer:sleep(?TIME),
    red_off = led_analyze_lib:getrgbi_fiber_4(),
    ct:pal("Red: Off",[]),
    ok.


%%%%%%%%%%%%%
%% Slow blink
%%%%%%%%%%%%%
%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec blink_slow_yellow_test(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
blink_slow_yellow_test(Config) ->
    [Client| _] = ?config(clients, Config),
    rct_rpc:call(rpc, led_client, 
    		 set_led_behavior, 
    		 [Client,[status, slow_blink]], 10000, noprint),
    
    %% led_rs232:login(led_console),
    timer:sleep(5000),

    led_analyze_lib:check_slow_blink(yellow),
    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec blink_slow_blue_test(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
blink_slow_blue_test(Config) ->
    [Client| _] = ?config(clients, Config),
    rct_rpc:call(rpc, led_client, 
    		 set_led_behavior, 
    		 [Client,[maintenance, slow_blink]], 10000, noprint),
    
    %% led_rs232:login(led_console),
    timer:sleep(5000),
   
    led_analyze_lib:check_slow_blink(blue),   
    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec blink_slow_green_test(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
blink_slow_green_test(Config) ->
    [Client| _] = ?config(clients, Config),
    rct_rpc:call(rpc, led_client, 
    		 set_led_behavior, 
    		 [Client,[operational, slow_blink]], 10000, noprint),
    %% test_server:break("A"),
    
    %% led_rs232:login(led_console),
    timer:sleep(5000),
   
    led_analyze_lib:check_slow_blink(green),
    ok.


%% %%%%%%%%%%%%%
%% %% Slow blink Red Test . Note this can't be done, only off and on can be used
%% %%%%%%%%%%%%%
%% blink_slow_red_test(Config) ->
%%     [Client| _] = ?config(clients, Config),
%%     rct_rpc:call(rpc, led_client, 
%%     		 set_led_behavior, 
%%     		 [Client,[fault, slow_blink]], 10000, noprint),
    
%%     led_rs232:login(led_console),
%%     timer:sleep(5000),
   
%%     led_analyze_lib:check_slow_blink(red),
%%     ok.






%%%--------------------------------------------------------------------
%%% @doc 
%%% @spec turn_off_leds(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
turn_off_leds(Config) ->
    ct:pal("# turn off leds"),
    [Client| _] = ?config(clients, Config),

    ct:pal("Check Yellow Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[status,off]], 10000, noprint),

    ct:pal("Check Blue Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[maintenance,off]], 10000, noprint),

    ct:pal("Check Green Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[operational,off]], 10000, noprint),

    ct:pal("Check Red Led, off"),
    rct_rpc:call(rpc, led_client, 
		 set_led_behavior, 
		 [Client,[fault,off]], 10000, noprint),
    ok.

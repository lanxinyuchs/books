%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ai_gui_test_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R7A/R9A/1
%%%
%%% @doc == AI gui tests.==
%%%
%%%
%%% @end

-module(ai_gui_test_SUITE).
-author('etxmlar').
-vsn('/main/R3A/R4A/R7A/R9A/1').
-date('2017-02-22').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2015-03-18 etxivri     Created
%%% R3A/3      2015-03-31 etxivri     Update due to new behaviour.
%%% R3A/4      2015-05-11 etxivri     Update due to new behaviour.
%%% R3A/5      2015-05-12 etxivri     New search str for node start up.
%%% R4A/1      2015-07-01 etxivri     Update to be more robust.
%%% R4A/2      2015-10-01 etxmlar     now() depricated in OTP 18 changed to os:timestamp()
%%% R7A/1      2016-09-06 etxmlar     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% R9A/1      2017-02-22 etxmlar     Changed expect string to make TC c_6_1 more
%%%                                   robust
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 groups/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 c_5_1/1,
	 c_5_2/1,
	 c_6_1/1
	]).


-define(NC, nc1).
%% -define(Protocol, "http").
%% -define(Port, "8080").
-define(Protocol, "https").
%% -define(Port, "443"). %% Not needed
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    [{timetrap,{minutes,60}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_power,power},
			 {rct_consserv,cs1},
			 {rct_netconf, {nc1, man_auth}},
			 {rct_ssh,{ssh,[manual_connect]}},
			 {rct_power,node},
			 {rct_rs232,console},
			 {cth_conn_log,[]}]}];
	_Other ->
	    [{timetrap, {minutes, 600}}, % 10 hours
	     {ct_hooks, [{rct_rpc, rpc},
			 {rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,node},
			 {rct_netconf, nc1},
			 {cth_conn_log, []},
			 {rct_logging, {all,
			 		[{erlang,
			 		  {["ERROR REPORT","CRASH REPORT"],
			 		   []}
			 		 }]}},
			 {rct_core,[]},
			 {rct_cli, {cli, [manual_connect]}}
			]}]
    end.


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
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
	    nl_lib:export_ai_log(Config, ?Protocol),
	    nl_lib:export_esi(Config, ?Protocol)
    end,
    ok.
%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [c_5_1,
     c_5_2,
     c_6_1
    ].

%%%--------------------------------------------------------------------
%%% @doc Download repeatedly -> integrate.
%%% @spec c_5_1(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_5_1(Config) ->
    ct:pal("C_5_1: Download repeatedly -> integrate."),

    %%%%
    %% Precondition. Board restore.
    %%%%
    board_restore(Config),

    %%%%
    %% Download. First time.
    %%%%
    ct:pal("##: Download first time."),
    download(Config),

    %%%% 
    %% Download. Second time.
    %%%%
    ct:pal("##: Download second time."),
    download(Config),

    %%%%
    %% Integrate
    %%%%   
    integrate(Config),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Download -> power off/on -> integrate.
%%% @spec c_5_2(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_5_2(Config) ->
    ct:pal("C_5_2: Download -> power off/on -> integrate.", []),

    
    %% Precondition. Board restore.
    %%%%
    board_restore(Config),

    %%%%
    %% Download.
    %%%%
    download(Config),
    timer:sleep(10000),    

    %%%%
    %% Power off
    %%%%
    ok = ct:pal("### power off!",[]),
    ok = rct_power:off(node),
    timer:sleep(10000),    

    %%%%
    %% Power on
    %%%%    
    ct:pal("### power on!",[]),
    ok = rct_power:on(node),

    %% New behaviour! The node shall automatic perform integrate after power on.
    {ok, _} = ct_telnet:expect(console,
    			       "Ericsson Version:", 
    			       [{timeout,60000},no_prompt_check]),
    ct_telnet:expect(console,
    		     "Mounting /sys",
    		     [{timeout,300000},no_prompt_check]),

    timer:sleep(30000),
    nl_lib:wait_for_netconf_started(?NC),
    timer:sleep(30000),
    nl_lib:site_config_complete(?NC, 60000),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Download -> Cancel during download then download + integrate.
%%% @spec c_6_1(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_6_1(Config) ->
    ct:pal("C_6_1: Cancel during download then download + integrate.", []),

    %%%%
    %% Precondition. Board restore.
    %%%%
    board_restore(Config),

    %%%%
    %% Cancel test 1
    %%%%
    ct:pal("start cancel test 1. Cancel during download progess."),
    start_download(Config),
    random_sleep(50),
    nl_lib:cancel(Config, console, ?Protocol),

    %%%%
    %% Cancel test 2
    %%%%
    ct:pal("start cancel test 2. Cancel Prepare filesystem."),
    start_download(Config),
    {ok, _} = ct_telnet:expect(console,
    			       "Prepare filesystem : Started", 
    			       [{timeout,120000},no_prompt_check]),

    timer:sleep(5000), %% Wait a 5 sec before random sleep
    random_sleep(5),
    nl_lib:cancel(Config, console, ?Protocol),

    %%%%
    %% Cancel test 3
    %%%%
    ct:pal("start cancel test 3. Unpack and install software."),
    start_download(Config),
    {ok, _} = ct_telnet:expect(console,
    			       "mounted filesystem with ordered data mode", 
    			       [{timeout,120000},no_prompt_check]),
    random_sleep(5),
    nl_lib:cancel(Config, console, ?Protocol),

    timer:sleep(5000),

    %%%%
    %% Download.
    %%%%
    download(Config),
    timer:sleep(10000),   

    %%%%
    %% Integrate
    %%%%   
    integrate(Config),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
board_restore(Config) ->
    ct:pal("Precondition: perform board restore."),
    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
    ct:pal("Board i restored. Now test can start."),
    timer:sleep(5000).

download(Config) ->
    ct:pal("# Download."),
    nl_lib:download_files(Config, console, ?Protocol),
    ct:pal("# Done, download."),
    timer:sleep(5000).


integrate(Config) ->
    ct:pal("### Integrate."),
    nl_lib:integrate(Config, console, ?NC, ?Protocol),
    ct:pal("### Done Integrate."),
	timer:sleep(5000).

start_download(Config) ->
    ct:pal("# Start Download."),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol),
    ct:pal("# download Started.").


random_sleep(MaxNr) ->
    %%%%
    %% Get a random nr.
    %%%%
    RandomNr = rand:uniform(MaxNr),
    ct:pal("# Start Sleep for: ~p sec.", [RandomNr]),
    ct:sleep({seconds, RandomNr}),
    RandomNr.

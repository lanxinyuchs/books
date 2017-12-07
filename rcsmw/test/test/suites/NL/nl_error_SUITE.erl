%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_error_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R5A/R7A/1
%%%
%%% @doc == NL / AI Error testcases.==
%%%
%%%
%%% @end

-module(nl_error_SUITE).
-author('etxmlar').
-vsn('/main/R3A/R5A/R7A/1').
-date('2016-09-06').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R3A/1      2015-02-25 etxivri     Created
%%% R3A/3      2015-02-25 etxivri     Update for unsecure.
%%% R3A/4      2015-03-03 etxivri     Update with https.
%%% R3A/7      2015-03-20 etxivri     Update a consol math.
%%% R3A/8      2015-03-31 etxivri     Update.
%%% R3A/9      2015-04-09 etxivri     Minor update
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warning
%%% R7A/1      2016-09-06 etxmlar     Changed random to rand due to random is
%%%                                   depricated in OTP19.
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
	 power_off_at_download/1,
	 power_off_at_prepare_filesystem/1,
	 power_off_at_unpack_install_sw/1, %% could result in destroyed disc.
	 power_off_at_finishing_and_storing/1
	]).


-define(NC, nc1).
-define(Protocol, "https").
%% -define(Port, "8080").

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
			 {rct_rs232,console},
			 {cth_conn_log,[]}]}];
	_Other ->
	    [{timetrap, {minutes, 600}}, % 10 hours
	     {ct_hooks, [{rct_rpc, rpc},
			 {rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,power},
			 {rct_netconf, nc1},
			 {cth_conn_log, []},
			 {rct_cli, {cli, [manual_connect]}}
			]}]
    end.


%% @hidden
init_per_suite(Config) ->
    %% board_restore(Config),
    Config.
%% @hidden
end_per_suite(_Config) ->
    %% ct:pal("## Make sure node is up after tests.  Download and integrate."),
    %% download(Config),
    %% integrate(Config),
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    board_restore(Config),
    Config.
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    install_node(Config),
    ok.


board_restore(Config) ->
    case nl_lib:open_netconf(?NC) of
	{ok, _} ->
	    ct:pal("Precondition: perform board restore."),
	    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
	    ct:pal("Board i restored. Now test can start."),
	    timer:sleep(5000);
	_Other ->
	    ct:log("Board Restored not needed !. ~p", [_Other])
    end.

install_node(Config) ->
    ct:pal("## Make sure node is up after tests.  Download and integrate."),
    nl_lib:download_files(Config, console, ?Protocol),
    nl_lib:integrate(Config, console, ?NC, ?Protocol).

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
    [
     power_off_at_download,
     power_off_at_prepare_filesystem
     %% power_off_at_unpack_install_sw %% could result in disc error
     %% %% power_off_at_finishing_and_storing
    ].


%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec power_off_at_download(Config) -> ok
%% @end
%%--------------------------------------------------------------------
power_off_at_download(Config) ->
    ct:pal("F_1_2 : Power off at: Download UP"),

    start_download(Config),
    ct:pal("## Start Download"),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol),

    case nl_lib:check_if_vc_board() of
    	"yes" -> 
    	    ok;
    	_ ->
    	    {ok,_} = 
		ct_telnet:expect(console, 
				 "Download of Software package : Started",
				 [{timeout,30000}, no_prompt_check])
    end,

    random_sleep(35),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),
    %% %% test_server:break("A"),
    %% power_off(Config),
    %% timer:sleep(10000),
    %% power_on(),

    check_after_power_on(),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec power_off_at_prepare_filesystem(Config) -> ok
%% @end
%%--------------------------------------------------------------------
power_off_at_prepare_filesystem(Config) ->
    ct:pal("F_1_3 : Power off at: Prepare filesystem"),

    start_download(Config),

    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    {ok,_} = 
		ct_telnet:expect(console, 
				 %% "ioremap empty pramfs filesystem image :",
				 "nl: This is the placeholder script:",
				 [{timeout, 120000}, no_prompt_check]);
	_ ->
	    {ok,_} = ct_telnet:expect(console, 
				      "Prepare filesystem : Started",
				      [{timeout,90000}, no_prompt_check])
    end,
    random_sleep(5),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),
    %% power_off(Config),
    %% timer:sleep(10000),
    %% power_on(),

    check_after_power_on(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% -
%% @spec power_off_at_unpack_install_sw(Config) -> ok
%% @end
%%--------------------------------------------------------------------
power_off_at_unpack_install_sw(Config) ->
    ct:pal("F_1_4 : Power off at: Unpack and install software"),

    start_download(Config),
    
    case nl_lib:check_if_vc_board() of
    	"yes" -> 
    	    {ok,_} = ct_telnet:expect(console, 
    				      "bio: create slab <bio-2> at 2",
    				      [{timeout,120000}, no_prompt_check]);
    	_ ->
    	    {ok,_} = ct_telnet:expect(console, 
				      "Unpack and install software : Started",
				      [{timeout,90000}, no_prompt_check])
    end,

    random_sleep(60),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),
    %% power_off(Config),
    %% timer:sleep(10000),
    %% power_on(),

    check_after_power_on(),
    ok.

%% %% Efter Bosses optimeringar gar detta for sbnabbt so man hamnar i 
%% %% Download Completed  innan man hinner gora power off.
%%--------------------------------------------------------------------
%% @doc
%% -
%% @spec power_off_at_finishing_and_storing(Config) -> ok
%% @end
%%--------------------------------------------------------------------
power_off_at_finishing_and_storing(Config) ->
    ct:pal("F_1_4x : Power off at: Finishing and storing log files"),

    start_download(Config),
   
    case nl_lib:check_if_vc_board() of
    	"yes" -> 
    	    {ok,_} = ct_telnet:expect(console, 
    				      "bio: create slab <bio-2> at 2",
    				      [{timeout,120000}, no_prompt_check]),
	    timer:sleep(20000); %% No trig point on this.
    	_ ->
    	    {ok,_} = 
		ct_telnet:expect(console, 
				 "Finishing and storing log files : Started",
				 [{timeout,90000}, no_prompt_check])
    end,

    %% random_sleep(5),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),
    %% power_off(Config),
    %% timer:sleep(10000),
    %% power_on(),

    check_after_power_on(),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
start_download(Config) ->
    ct:pal("## Start Download"),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol),
    ct:pal("## Download started").

%% power_on() ->
%%     ct:pal("### power on!",[]),
%%     ok = rct_power:on(power).

check_after_power_on() ->
    case ct_telnet:expect(console, 
			  "Ericsson Version:",
			  [{timeout,30000}, no_prompt_check]) of
	{ok,_} ->
	    case ct_telnet:expect(console, 
				  "Network Loader Ready:",
				  [{timeout,120000}, no_prompt_check]) of
		{ok,_} -> 
		    ct_telnet:expect(console, 
				     "Build date:",
				     [{timeout,120000}, no_prompt_check]),
		    ok;
		_Other ->
		    test_server:break("Check if disc fault!!")
	    end;
	_Else -> %% Power did not work! cleanu for nest TC.
	    ct:pal("### TC will fail ### due to power off on did not work!!!"),
	    ct:pal("### Perorm a extra power to clean up for next TC!"),
	    rct_power:cycle(power),
	    ct_telnet:expect(console, 
			     "Build date:",
			     [{timeout,120000}, no_prompt_check]),
	    ct:fail("Power did not work, Node has not restarted !!")
    end,
    timer:sleep(15000).


%% power_off(Config) ->
%%     ct:pal("## Start Power off"),
%%     case rct_power:off(power, no_retries) of
%% 	ok ->
%% 	    ct:pal("# 1 # Done Power off, first try");
%% 	_A ->
%% 	    case rct_power:off(power, no_retries) of
%% 		ok ->
%% 		    ct:pal("# 2 # Done Power off, Second try");
%% 		_B ->
%% 		    case rct_power:off(power, no_retries) of
%% 			ok ->
%% 			    ct:pal("# 3 # Done Power off, third try");
%% 			_C ->
%% 			    nl_lib:httpc_request(Config, post, "Cancel", ?Protocol)
%% 		    end
%% 	    end
%%     end.    

random_sleep(MaxNr) ->
    %%%%
    %% Get a random nr.
    %%%%
    RandomNr = rand:uniform(MaxNr),
    ct:pal("# Start Sleep for: ~p sec.", [RandomNr]),
    ct:sleep({seconds, RandomNr}),
    RandomNr.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_on_site_conf_fast_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016
%%% @version /main/R7A/2
%%%
%%% @doc == AI lmt integration trusted network IPv4 download as fast as possible after board restore. ==
%%%
%%%
%%% @end

-module(lmt_integration_on_site_conf_fast_SUITE).
-author('etxmlar').
-vsn('/main/R7A/2').
-date('2016-11-07').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% Rev       Date        Name        What
%%% -----     ---------   --------    ------------------------
%%% R7A/1     2016-11-02  etxmlar     Created
%%% R7A/2     2016-11-02  etxmlar     Updated and checked in
%%% ----------------------------------------------------------

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
	 nodeUC640_N_009/1
	]).

-define(NC, nc1).
-define(Protocol, "https").
-define(DefaultPort, "8080").
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

    HW = atom_to_list(ct:get_config({test_nodes,1})),
    TftpBootDir = "/proj/rcs-tmp/tftpboot/"++HW++"/",
    ct:pal("TftpBootDir: ~p ", [TftpBootDir]),

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:pal("ls : ~p ", [Ls]),

    [{tftpboot_dir, TftpBootDir},
     {hw, HW}| Config].


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
init_per_testcase(TestCase, Config)->
    ct:log("init_per_testcase: TestCase ~p~n", [TestCase]),  
    Config.


%%@hidden
end_per_testcase(TestCase, Config) ->
    ct:log("end_per_testcase: TestCase ~p~n", [TestCase]),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:log("Testcase failed due to: ~p. Export logs. \n", [Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol),
    	    nl_lib:export_esi(Config, ?Protocol)
    end,
    ok.


string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

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
    [nodeUC640_N_009
    ].


%%%--------------------------------------------------------------------
%%% @doc  Download and Integrate with check
%%% @spec nodeUC640_N_009(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_N_009(Config) ->
    ct:log("nodeUC640_N_009:~n" 
	   "Run on  both Sec/UnSec board. ~n"
	   "Board restore ->  Download as fast as possible after board restore -> Integrate. ~n"),


    %%Prepare for download 
    XmlFile = "RbsSummaryFile.xml",
   
    HW = atom_to_list(_Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    %%BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    MyTime = integer_to_list(ms_since_1970()),

    %%IP = get_node_ip(),
    Method = post,

    Body = 
	"host="++SftpHost++"&"++
	"username="++Username++"&"++
	"password="++Password++"&"++
	%% "filename=boards/"++HW++"/RbsSummaryFile.xml"++"&"++
	"filename=boards/"++HW++"/"++XmlFile++"&"++
	"utcMs="++MyTime++"&"++
	"DoDownload=Download",

    Timeout = 600000,
    Url_https = "https://"++IP++"/cgi-bin/nl_gui:post",

    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Download. 
    %%%%
    download(Config, Method, Url_https, Body, Timeout),
   
    %%%%
    %% Integrate
    %%%%  
    integrate(Config),

    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
%% hard_factory_reset(Config) ->
%%     aic_httpc:hard_factory_reset(Config, console),
%%     ct:log("Board is hard factory reset. Now test can start."),
%%     ok.

board_restore(Config) ->
    aic_httpc:board_restore(Config, console).

download(Config, Method, Url_https, Body, Timeout) ->

    start_needed_applications("https"),
    aic_httpc:send_httpc_request_no_check(Method, Url_https, Body, Timeout),
    stop_needed_applications("https"),
    aic_httpc:wait_for_download_completed(Config, "https"),
    ok.

integrate(Config) ->
    aic_httpc:integrate(Config, console, ?NC),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_needed_applications(Protocol) ->	
    inets:start(), %% För httpc
    case Protocol of
	"https" ->
	    crypto:start(),
	    ssl:start();
	_Other ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stop_needed_applications(Protocol) ->	
    inets:stop(), %% För httpc
    case Protocol of
	"https" ->
	    crypto:stop(),
	    ssl:stop();
	_Other ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
ms_since_1970() ->
    list_to_integer(string:strip(os:cmd("date -u +%s"),right,$\n)) * 1000.

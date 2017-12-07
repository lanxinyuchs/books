%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_ai_actions_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R7A/R9A/R11A/1
%%%
%%% @doc == Escalate to NL testcases.==
%%%
%%%
%%% @end

-module(nl_ai_actions_SUITE).
-author('etxivri').
-vsn('/main/R3A/R4A/R5A/R7A/R9A/R11A/1').
-date('2017-10-09').

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
%%% R3A/1      2015-02-24 etxivri     Created
%%% R3A/2      2015-03-03 etxivri     Update for https
%%% R3A/3      2015-03-05 etxivri     Add request_download_pogress
%%% R3A/4      2015-05-26 etxivri     Add hard_factory_reset.
%%% R3A/5      2015-05-28 etxivri     Update in generic_download request.
%%% R3A/6      2015-06-01 etxivri     Update to use action from lib/lib
%%% R4A/1      2015-09-18 etxivri     Add timer sleep after integrate done to
%%%                                   make sure everything is up before 
%%%                                   other tests starts.
%%% R7A/1      2015-10-04 etxivri     Update hard_factory_reset to handle if old
%%%                                   NL on dus exist on sda1.
%%% R7A/2      2014-10-05 etxivri     Update to use RbsSummaryFile.xml.nl15B
%%% R7A/3      2016-10-14 etxivri     Update if nl on tcu is to old.
%%% R7A/4      2017-01-11 etxmlar     Update the check, if nl is to old
%%% ----------------------------------------------------------
%%% R9A/1      2017-03-28 etxderb     R7 NL can install X3 UPs from R9A
%%%                                   as soon as boot files are signed so now
%%%                                   inhibit hardfactory reset for this case
%%% R9A/2      2017-04-21 etxmlar     small correction 
%%% R11A/1     2017-10-09 etxivri     Update httc req due to OTP20
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
	 %% wait_for_netconf_started/0,
	 all/0,
	 export_ai_log/1,
	 download_files/1,
	 request_download_pogress/1,
	 integrate/1,
	 board_restore/1,
	 export_esi/1,
	 generic_export_esi/1,
	 cancel/1,
	 hard_factory_reset/1,
	 download_after_hfr/1
	]).


-define(APP4, "restart").
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
    case aic_httpc:check_if_vc_board() of
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
			 {rct_power,node},
			 {rct_netconf, nc1},
			 {cth_conn_log, []},
			 %% {rct_logging, {all,
			 %% 		[{erlang,
			 %% 		  {["ERROR REPORT","CRASH REPORT"],
			 %% 		   ["Program ID [0-9]+ has crashed",
			 %% 		    "Program ID [0-9]+ has terminated",
			 %% 		    "has failed to start after 3 attempts within 300 seconds"]}
			 %% 		 }]}},
			 %% {rct_core,[]}
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
	    export_ai_log(Config),
	    export_esi(Config)
    end,
    ok.
%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
     {all_actions,[],[board_restore,
		      export_ai_log,
		      download_files,
		      integrate,
		      export_esi
		     ]},
     {hard_factory_reset,[sequence],[hard_factory_reset,
				     download_after_hfr,
				     integrate
				    ]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     board_restore,
     %% export_ai_log,
     download_files,
     integrate
     %% export_esi
    ].


board_restore(Config) ->
    aic_httpc:board_restore(Config, console),
    ok.

export_ai_log(Config) ->
    aic_httpc:export_ai_log(Config),
    ok.

download_files(Config) ->
    aic_httpc:download_files(Config, console),
    ok.

request_download_pogress(Config) ->
    aic_httpc:request_download_pogress(Config, ?Protocol),
    ok.

integrate(Config) ->
    aic_httpc:integrate(Config, console, ?NC),
    ct:log("sleep 60sec to make sure node is up and ready for next tests!"),
    timer:sleep(60000),
    ok.

export_esi(Config) ->
    aic_httpc:export_esi(Config),
    ok.

generic_export_esi(Config) ->
    aic_httpc:export_esi_check_nl_state(Config, console),
    ok.

cancel(Config) ->
    aic_httpc:cancel(Config, console),
    ok.

hard_factory_reset(Config) ->
    case is_hard_factory_reset_ok() of
        false ->
            N = length(ct:get_config(test_nodes)),
            Hwa = ct:get_config({test_nodes,N}),
            BoardType = ct:get_config({Hwa,board_type}),
            Prodno = ct:get_config({Hwa, product_no}),
            case is_r_state(Prodno) of
                false ->
                    ct:pal("Notice, this board seems to be a P revision (~p) X3 board (~p) ~n"
                           " => this board most likely dont support to install R9A bootfiles (signed)~n"
                           " => so performing BoardRestore instead of hardFactoryReset", [Prodno, BoardType]),
                    aic_httpc:board_restore(Config, console),
                    ok;
                true ->
                    aic_httpc:hard_factory_reset(Config, console),
                    ok
            end;
        true ->
            aic_httpc:hard_factory_reset(Config, console),
            ok
    end.

is_hard_factory_reset_ok() ->
    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    BoardType = ct:get_config({Hwa,board_type}),
    Prodno = ct:get_config({Hwa, product_no}),
    is_hard_factory_reset_ok(BoardType, Prodno).
    
is_hard_factory_reset_ok(BoardType, Prodno) ->
    case {is_x3_board(BoardType), is_r_state(Prodno)} of
        {true, false} ->
            false;
        _ ->
            true
    end.

is_r_state(Prodno) ->
    case string:tokens(Prodno, "R") of
        [Prodno]  -> false;
        [_, _] -> true
    end.

-define(X3_BOARDS, ["dus5301","dus3301", "dus6303", "dus6502"]).
is_x3_board(BoardType) ->
    lists:member(BoardType, ?X3_BOARDS).

download_after_hfr(Config) ->
    Console = console,
    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    ct:log("# Hwa: ~p", [Hwa]),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("# BoardType: ~p", [BoardType]),
    case {aic_httpc:check_if_vc_board(), is_hard_factory_reset_ok()} of
	{"yes", _} -> 
	    download_files(Config);
	{_, false} ->
	    download_files(Config);
	_Other ->
	    %% Could only be runed on unsec.
	    ct:pal("check rev on nl type2"),
	    NlStatus = get_rev_on_nl_type2(Console, Config),
	    case NlStatus of
		nl_is_ok ->
		    download_files(Config);
		{to_old_nl, NLUpgradeFile}  ->
		    ct:pal("Do NL upgrade before install."),
		    aic_httpc:nl_upgrade(Config, Console, NLUpgradeFile),
		    ct:pal("Install SW."),
		    download_files(Config)
	    end
    end,
    ok.


get_rev_on_nl_type2(Console, Config) -> 
    ct:pal("cat nl_log"),
    %% ok = ct_telnet:send(console, "cat /rcs/networkloader/nl_log.1"),
    ok = ct_telnet:send(console, "cat /nl/log/nl_log.1 "),	    
    {ok, _} = ct_telnet:expect(Console,
			       "Running version:", 
			       [{timeout,30000},no_prompt_check]),

    %% Get NL version on node
    NodeNLVersion = get_nl_version_node(Config),
    ct:log("Network Loader Version on Node: ~p", [NodeNLVersion]),


    NlStatus =
	%% first check for Multi NL support
	case support_multi_nl() of
	    true ->
		ct:pal("grep for Networkloader type2 in nl_log"),
		ok = ct_telnet:send(console, "grep 'Networkloader type2' /nl/log/nl_log.1"),
		{ok, _} = ct_telnet:expect(Console,
					   "Networkloader type2 booted from partition /dev/sda1", 
					   [{timeout,5000},no_prompt_check]),

		NLVerNotOk = "CNX9012629-R4F10", %%TR HU14731

		case do_nl_upgrade(NodeNLVersion, NLVerNotOk) of
		    false ->
			ct:pal("The NL on sda1 is to old."
			       " NL can not handle large lkf/config files."),
			%% After NL version CNX9012629-R4B18 nl upgrade is done 
			%% with UP not with the CXP
			NLUpgradeFile = 
			    get_what_nl_upgrade_file_to_use(NodeNLVersion, "CNX9012629-R4B18"),
			{to_old_nl, NLUpgradeFile};
		    true ->
			ct:pal("The NL on sda1 is OK. NL ug supported."),
			nl_is_ok
		end;	 

	    false ->
		ct:pal("The NL on sda1 is to old."
		       " NL Upgrade is not supported."),
		%% After NL version CNX9012629-R4B18 nl upgrade is done with UP not with the CXP
		NLUpgradeFile = get_what_nl_upgrade_file_to_use(NodeNLVersion, "CNX9012629-R4B18"),
		{to_old_nl, NLUpgradeFile}
	end,

    ct:pal("NlStatus : ~p", [NlStatus]),
    NlStatus.

%%--------------------------------------------------------------------
support_multi_nl() ->

    Cmd = "ls /proc/device-tree/rcs_nodes/",
    ok = ct_telnet:send(console, Cmd),

    %% Ans = 
    %% 	ct_telnet:expect(console, "boot_nl3_partition",[{timeout, 30000}, no_prompt_check]),

    {ok, DataList} = ct_telnet:get_data(console),

    Ans = 
	case re:run(DataList, "boot_nl3_partition") of
	    {match, Value} ->
		ct:log("Value: ~p ~n",[Value]),
		ok;
	    nomatch ->
		nok
	end,

    Is_Multi_nl =
	case Ans of
	    ok -> 
		true;
	    nok -> 
		false
	end,

    ct:log("Is_Multi_nl: ~p", [Is_Multi_nl]),
    Is_Multi_nl.
%%--------------------------------------------------------------------
get_nl_version_node(_Config) ->
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),

    start_needed_applications(?Protocol),

    HttpsUrl= "https://"++IP++"/help.html",
    HttpUrl= "http://"++IP++":"++?Protocol++"/help.html",

    UrlRes =  
%%	case httpc:request(HttpsUrl) of
        case httpc:request(get,{HttpsUrl,[]},[{ssl,[{server_name_indication,"test_vc"}]}],[]) of
	    {ok,{{_,200,"OK"}, _ReplyHttpsA, ReplyHttpsB}} ->
		%%ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		ReplyHttpsB;
	    {error, Reason} ->
		%% case httpc:request(HttpUrl) of
		case httpc:request(get,{HttpUrl,[]},[{ssl,[{server_name_indication,"test_vc"}]}],[]) of
		    {ok,{{_,200,"OK"}, _ReplyHttpA, ReplyHttpB}} ->
			%%ct:log("ReplyHttpB: ~p ~n",[ReplyHttpB]),
			ReplyHttpB; 
		    {error, Reason} ->
			ct:fail("TC will fail due to unknown protocol.")
		end
	end,

    stop_needed_applications(?Protocol),

    UrlListRes = string:tokens(UrlRes, " \n\t"),  
    NLNodeVerStr = 
	lists:flatten([VersionStr||VersionStr<-UrlListRes, lists:prefix("CNX", VersionStr)]),

    %% NLNodeVerStr = "CNX9012629-R4AC07<p>"
    NLNodeVersionStr = lists:flatten(string:tokens(NLNodeVerStr, "<p>")),
    NLNodeVersionStr.
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

stop_needed_applications(Protocol) ->	
    inets:stop(), %% För httpc
    case Protocol of
	"https" ->
	    crypto:stop(),
	    ssl:stop();
	_Other ->
	    ok
    end.

do_nl_upgrade(UpNLVer, NodeNLVer)->

    %%NL Version in UP: "CNX9012629-R4AA02"
    %%NL on Node: "CNX9012629-R4U02

    [CNX, UpNLLabel] = string:tokens(UpNLVer, "/-"),         
    [CNX, NodeNLLabel] = string:tokens(NodeNLVer, "/-"),

    gt(UpNLLabel, NodeNLLabel).

%%--------------------------

gt(A,B) ->
    {N1,L1,M1} = split_version(string:to_lower(A)),
    {N2,L2,M2} = split_version(string:to_lower(B)),

    (gt_(N1, N2)) orelse
    (eq_(N1, N2) andalso gt_(L1, L2)) orelse
    (eq_(N1, N2) andalso eq_(L1, L2) andalso gt_(M1, M2)).


gt_(A,B) when length(A) == length(B) -> A > B;
gt_(A,B) when length(A) > length(B) -> true;
gt_(_A,_B) -> false.

eq_(A,B) -> A == B.

%% split_version("r1234abcde5678")->{1234, "abcde", 5678}
split_version([$r | NLM]) -> 
    {N, LM} = consume(NLM, integers),
    {L, M} = consume(LM, lc_letters),
    case consume(M, integers) of
	{M, _} ->
	    %% Normal
	    ok;
	{_Real_m, Extra} ->
	    ct:log("split_version contains extra info ~p - ignoring~n",[Extra]),
	    ok
    end,
    {N, L, M}.

%% --------------------------------------------------------------------------
%% consume(S, Type) ->
%% 			{Hit, Res} | {error, version_parse_error}
%% 			 
%%	where
%%		S = string()
%%		Type = Must be defined in regexp(Type) -> regexp()
%%		Hit = Consecutive pattern found in S matching Type
%%		Rest = The rest of the string so that Hit ++ Rest == S.
%%      
%% --------------------------------------------------------------------------
consume(S, Type) ->
    case re:run(S, regexp(Type)) of
	{match, [{0, No_hits_in_a_row}]} ->
	    Hit = string:substr(S, 1, No_hits_in_a_row),
	    Rest = string:substr(S, No_hits_in_a_row+1),
	    {Hit, Rest};
	No_correct_match ->
	    ct:log("Incorrect int match ~p in version string ~p to pattern ~p~n", 
		   [No_correct_match, S, regexp(Type)]),
	    {error, version_parse_error}
    end.

regexp(integers) -> "[0-9]+";
regexp(lc_letters) -> "[a-z]+".


%%--------------------------------------------------------------------
get_what_nl_upgrade_file_to_use(NodeNLVersion, NLVerNotOk)->

    %%NL Version in UP: "CNX9012629-R4AA02"
    %%NL on Node: "CNX9012629-R4U02

    [CNX, UpNLLabel] = string:tokens(NodeNLVersion, "/-"),         
    [CNX, NodeNLLabel] = string:tokens(NLVerNotOk, "/-"),

    case gt(UpNLLabel, NodeNLLabel) of
	false ->
	    ct:pal("The NL on sda1 supports NL upgrade with cxp. Use RbsSummaryFile.xml.nl15B"),
	    "RbsSummaryFile.xml.nl15B";
	true ->
	    ct:pal("The NL on sda1 supports NL upgrade with UP. Use RbsSummaryFile.xml.nl"),
	    "RbsSummaryFile.xml.nl"
    end.	 

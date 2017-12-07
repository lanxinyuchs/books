%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ai_user_interface_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R7A/R9A/1
%%%
%%% @doc == AI user interface tests.==
%%%
%%%
%%% @end

-module(ai_user_interface_SUITE).
-author('etxmlar').
-vsn('/main/R3A/R4A/R7A/R9A/1').
-date('2017-04-21').

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
%%% R3A/1      2015-05-26 etxivri     Created
%%% R3A/2      2015-05-28 etxivri     Update in generic_download request.
%%% R4A/1      2015-07-01 etxivri     Make it more robust.
%%% R7A/1      2016-10-24 etxmlar     Update if nl on dus/tcu is to old 
%%% R7A/2      2017-01-11 etxmlar     Update the check, if nl is to old
%%% R9A/1      2017-04-21 etxmlar     R7 NL can install X3 UPs from R9A
%%%                                   as soon as boot files are signed so now
%%%                                   inhibit hardfactory reset for this case
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
	 c_1_1/1
	]).


-define(NC, nc1).
-define(Protocol, "https").
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
    [c_1_1
    ].

%%%--------------------------------------------------------------------
%%% @doc Download repeatedly -> integrate.
%%% @spec c_1_1(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_1_1(Config) ->
    ct:pal("C_1_1: AI with sda1 NL."),

    %%%%
    %% Precondition. Hard factory reset. to make nl on sda1 active.
    %%%%
    ct:pal("Precondition: perform hard_factory_reset."),
    hard_factory_reset(Config),

    %%%%
    %% Download.
    %%%%
    download_after_hard_factory_reset(Config),

    %%%%
    %% Integrate
    %%%%   
    generic_integrate(Config),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hard_factory_reset(Config) ->
%%     %% nl_lib:board_restore(Config, console, ?NC, ?Protocol),
%%     nl_lib:hard_factory_reset(Config, console, ?NC, ?Protocol),
%%     ct:pal("Board is HardFactoryReset. Now test can start."),
%%     timer:sleep(5000).

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
		    nl_lib:hard_factory_reset(Config, console, ?NC, ?Protocol),
		    ct:pal("Board is HardFactoryReset. Now test can start."),
		    timer:sleep(5000),
                    ok
            end;
        true ->
	    nl_lib:hard_factory_reset(Config, console, ?NC, ?Protocol),
	    ct:pal("Board is HardFactoryReset. Now test can start."),
	    timer:sleep(5000),
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


generic_download(Config) ->
    ct:pal("# Start Download."),
    nl_lib:generic_download_files(Config, console),
    ct:pal("# Done download."),
    timer:sleep(5000).

generic_integrate(Config) ->
    ct:pal("### Start Integrate."),
    nl_lib:generic_integrate(Config, console, ?NC),
    ct:pal("### Done Integrate."),
    timer:sleep(60000).


download_after_hard_factory_reset(Config) ->
    Console = console,
    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    ct:log("# Hwa: ~p", [Hwa]),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("# BoardType: ~p", [BoardType]),
    case {aic_httpc:check_if_vc_board(), 
	  is_hard_factory_reset_ok(), 
	  is_x3_board(BoardType)} of
	{"yes", _, _} -> 
	    generic_download(Config);
	{_, false, _} ->
	    generic_download(Config);
	{_, _, true} ->
	    %%No need to check NL version on dusX3 boards
	    generic_download(Config);
	_Other ->
	    %% Could only be runed on unsec.
	    ct:pal("check rev on nl type2"),
	    NlStatus = get_rev_on_nl_type2(Config, Console),
	    case NlStatus of
		nl_is_ok ->
		    generic_download(Config);
		{to_old_nl, NLUpgradeFile} ->
		    ct:pal("Do NL upgrade before install."),
		    aic_httpc:nl_upgrade(Config, Console, NLUpgradeFile),
		    ct:pal("Install SW."),
		    generic_download(Config)
	    end
    end,
    ok.


get_rev_on_nl_type2(Config, Console) -> 
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
	case httpc:request(HttpsUrl) of
	    {ok,{{_,200,"OK"}, _ReplyHttpsA, ReplyHttpsB}} ->
		%%ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		ReplyHttpsB;
	    {error, Reason} ->
		case httpc:request(HttpUrl) of
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

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_escalate_3_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R7A/R9A/2
%%%
%%% @doc == Escalate NL from type2 to type 3 testcase.==
%%%
%%%
%%% @end

-module(nl_escalate_3_SUITE).
-author('etxmlar').
-vsn('/main/R5A/R6A/R7A/R9A/2').
-date('2017-05-04').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% Rev        Date        Name        What
%%% -----      ---------   --------    ------------------------
%%% R7A/1      2016-10-18  etxmlar     Updated to run correct test  
%%% R7A/2      2016-11-15  etxmlar     Updated for TCU04    
%%% R7A/3      2016-12-01  etxmlar     Updated TC be more robust  
%%% R7A/4      2017-01-11  etxmlar     Update the check, if nl is to old    
%%% R9A/1      2017-02-01  etxmlar     Update to support sec boards    
%%% R9A/2      2017-05-02  etxmlar     R7 NL can install X3 UPs from R9A
%%%                                    as soon as boot files are signed so now
%%%                                    inhibit hardfactory reset for this case           
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
	 board_restore/1,
	 board_restore_hard/1,
	 export_ai_log/1,
	 download_files/1,
	 integrate/1
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
init_per_group(d_1_4, Config) ->
    ct:log("d_1_4: Board restore -> board restore hard (hard_factory_reset) -> download -> integrate. ~n"
	   "1. Perform board restore to get type 3 nl to be used. ~n"
	   "2. Perform hard factory reset to get type2 nl to be used. ~n"
	   "3. Download. ~n"
	   "4. Integrate. ~n"),

    Config;

init_per_group(GroupName, Config) ->
    ct:pal("GroupName: ~p",[GroupName]),
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    Config.

end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
	    aic_httpc:export_ai_log(Config),
	    aic_httpc:export_esi(Config)
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
     {d_1_4, [], [board_restore,
		  board_restore_hard,
		  export_ai_log,
		  download_files,
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
    ].

%%--------------------------------------------------------------------
%% @doc
%% @spec board_restore(Config) -> ok
%% @end
%%--------------------------------------------------------------------
board_restore(Config) ->

    ct:log("Board restore. ~n"
	   "Then check type3 booted from partition sda2 NL. ~n"),
    aic_httpc:board_restore(Config, console),
    check_type3_booted_from_sda2(console),

    ok.


board_restore_hard(Config) ->

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
		    ct:log(" => NOTE! But in this case ignore board restore, no board restore x2"),  	   
                    %%aic_httpc:board_restore(Config, console),

                    ok;
                true ->
		    do_board_restore_hard(Config)
            end;
        true ->
            do_board_restore_hard(Config)
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



do_board_restore_hard(_Config) ->
    ct:log("Board restore hard (hard_factory_reset). ~n"  
	   "Then check type2 booted from partition sda1 NL. ~n"),


    NLState = check_node_is_in_nl(console), %% no_nl | nl

    case NLState of
	nl -> %% Board is in NL state.

	    IP = get_node_ip(),
	    Method = post,
	    Body = "DoHardFactoryReset=HardFactoryReset",
	    Timeout = 600000,
	    Url_https = "https://"++IP++"/cgi-bin/nl_gui:post",

	    ct:log("Url https: ~p", [Url_https]),
	    start_needed_applications("https"),
	    timer:sleep(5000),
	    aic_httpc:send_httpc_request_no_check(Method, Url_https, Body, Timeout),
	    timer:sleep(5000),
	    stop_needed_applications("https"),	
	    timer:sleep(20000),

	    check_type2_booted_from_sda1(console);

	_Else -> 

	    ct:log("_Else : ~p", [_Else ]),
	    ct:fail("Board restore hard failed, faulty nl state!") 
    end,

    ok.



export_ai_log(Config) ->
    aic_httpc:export_ai_log(Config),
    ok.

download_files(Config) ->

    ct:pal("# Download."),
    NLStatus = check_if_upgrade_nl_after_hfr(Config),

    case NLStatus of
	nl_is_ok ->
	    ct:pal("NL is ok."),
	    aic_httpc:download_files(Config, console);
	{to_old_nl, NLUpgradeFile} ->
	    ct:pal("Do NL upgrade before download."),
	    aic_httpc:nl_upgrade(Config, console, NLUpgradeFile),
	    aic_httpc:download_files(Config, console);
	{sec_nl, NLUpgradeFile}  -> 
	    ct:pal("It is a secure board."),
	    aic_httpc:nl_upgrade(Config, console, NLUpgradeFile),
	    aic_httpc:download_files(Config, console)

    end,

    ct:pal("# Done, download."),
    timer:sleep(5000).



integrate(Config) ->
    aic_httpc:integrate(Config, console, ?NC),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_type3_booted_from_sda2(Console) ->

    ct:log("check_type3, first sleep 10 sec", []),
    timer:sleep(10000),

    case aic_httpc:check_if_vc_board() of
	"yes" -> 
	    ct:log("No type3 check for secure board");
	_Other ->	      
	    ct_telnet:send(Console, "cat /nl/log/nl_log.1"),
	    {ok, _} = ct_telnet:expect(Console,
				       "Networkloader type3 booted from partition /dev/sda2", 
				       [{timeout,300000},no_prompt_check])
    end,

    ok.
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_type2_booted_from_sda1(Console) ->

    ct:log("check_type2, first check Network Loader Ready:", []),


    case ct_telnet:expect(Console,
			  "Network Loader Ready:", 
			  [{timeout,300000},no_prompt_check]) of
	{ok, _} ->
	    ok;
	_ -> % try again.
	    ok = ct_telnet:send(Console, ""),
	    {ok, _} = ct_telnet:expect(Console,
				       "Network Loader Ready:", 
				       [{timeout,300000},no_prompt_check])
    end,
    %%
    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout,30000},no_prompt_check]),


    case aic_httpc:check_if_vc_board() of
	"yes" -> 
	    ct:log("No type2 check for secure board");
	_Other ->	      

	    ct_telnet:send(Console, "cat /nl/log/nl_log.1"),
	    {ok, _} = ct_telnet:expect(Console,
				       "Networkloader type2 booted from partition /dev/sda1", 
				       [{timeout,300000},no_prompt_check])

    end,

    ok.
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_node_ip() ->
    HW = atom_to_list(ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    ct:pal("Node IP : ~p", [IP]),
    IP.

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
check_node_is_in_nl(Console) ->

    case aic_httpc:check_if_vc_board() of
	"yes" -> 
	    ct:log("No nl check for secure board"),
	    timer:sleep(10000),
	    nl;
	_Other1 ->
	    ct_telnet:send(Console, ""),
	    case ct_telnet:expect(Console, "networkloader]#", 
				  [{timeout,5000},no_prompt_check]) of
		{ok, _} ->
		    nl;
		_ -> %% prompt can be corrupted, try second time
		    timer:sleep(5000),
		    ct_telnet:send(Console, ""),
		    case ct_telnet:expect(Console, "networkloader]#", 
					  [{timeout,5000},no_prompt_check]) of
			{ok, _} ->
			    nl;
			_Other2 ->
			    no_nl
		    end
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_if_upgrade_nl_after_hfr(Config)->
    ct:pal("# Check nl and upgrade if it is to old."),
    NlStatus = download_after_hfr(Config),
    NlStatus.

%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
download_after_hfr(Config) ->
    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    ct:log("# Hwa: ~p", [Hwa]),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("# BoardType: ~p", [BoardType]),

    NlStatus = 
	case {aic_httpc:check_if_vc_board(), is_hard_factory_reset_ok(), is_x3_board(BoardType)} of
	    {"yes", _, _} -> 
		SecNodeNLVersion = get_nl_version_node(Config),
		ct:log("Network Loader Version on Secure Node: ~p", [SecNodeNLVersion]),

		%% After NL version CNX9012629-R4B18 nl upgrade is done with UP not with the CXP
		NLUpgradeFile = get_what_nl_upgrade_file_to_use(SecNodeNLVersion, "CNX9012629-R4B18"),
		{sec_nl, NLUpgradeFile}; 
	    {_, false, _} ->
		nl_is_ok;
	    {_, _, true} ->
		%%No need to check NL version on dusX3 boards
	    	nl_is_ok;
	    _Other ->
		%% Could only be runed on unsec.
		ct:pal("check rev on nl type2"),
		get_rev_on_nl_type2(Config, console)
	end,
    NlStatus.


get_rev_on_nl_type2(Config, Console) -> 
    ct:pal("cat nl_log"),
    %% ok = ct_telnet:send(console, "cat /rcs/networkloader/nl_log.1"),
    ok = ct_telnet:send(console, "cat /nl/log/nl_log.1 "),	    
    {ok, _} = ct_telnet:expect(console,
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

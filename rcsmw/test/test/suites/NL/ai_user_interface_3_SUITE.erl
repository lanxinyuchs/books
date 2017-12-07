%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ai_user_interface_3_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R7A/R9A/2
%%%
%%% @doc == AI user interface tests.==
%%%
%%%
%%% @end

-module(ai_user_interface_3_SUITE).
-author('etxmlar').
-vsn('/main/R4A/R5A/R7A/R9A/2').
-date('2017-04-19').

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
%%% R4A/1      2015-09-18 etxmlar     Created 
%%% R4A/2      2015-09-21 etxmlar     Updated 
%%% R4A/3      2015-09-23 etxmlar     Added check for multi_nl 
%%% R4A/4      2015-09-24 etxmlar     Corrected check for multi_nl 
%%% R4A/5      2015-10-20 etxmlar     Corrected nl vesion check
%%% R4A/6      2015-10-20 etxmlar     Removed a printout
%%% R4A/7      2015-10-21 etxmlar     Small fix
%%% R4A/8      2015-10-30 etxmlar     Bug fix
%%% R5A/1      2016-02-10 etxmlar     Added an extra check after download 
%%% R7A/1      2016-10-28 etxmlar     Update if nl on tcu is to old.
%%% R7A/2      2016-11-06 etxmlar     Updated get_nl_version_up
%%% R7A/3      2016-12-06 etxmlar     Temp. Update to support BPU
%%% R7A/4      2017-01-11 etxmlar     Update the check, if nl is to old
%%% R9A/1      2017-01-26 etxmlar     Added support for new boardtypes
%%% R9A/2      2017-04-19 etxmlar     R7 NL can install X3 UPs from R9A
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
	 c_1_4/1
	]).


-define(NC, nc1).
-define(DefaultProt, "https").
-define(DefaultPort, "8080").
-define(TCU_UP_WITH_NL_VERSION_R6Y02, "https://arm110-eiffel001.seli.gic.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_5/R2JBS/CXP9024419_5-R2JBS.zip"). %%NL version CNX9012629-R6Y02
-define(DUS_UP_WITH_NL_VERSION_R6Y01, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_5/R2EGN/CXP9024418_5-R2EGN.zip"). %%NL version CNX9012629-R6Y01

-define(C608_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").  %%NL version CNX9012629-R8N04

-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R4A241/CXP9024418_6-R4A241.zip").  %%NL version CNX9012629-R8N04

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
			 {rct_rpc, rpc},
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
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    
    TftpUP = get_node_up(TftpBootDir),
    ct:log("Tftp UP : ~p ", [TftpUP]),
    
    [{tftpboot_dir, TftpBootDir},
     {tftp_up, TftpUP},
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

init_per_testcase(TestCase = c_1_4, Config) ->
    ct:pal("init TestCase: ~p",[TestCase]),

    case get_boardtype() of
	"tcu03" ->
	    {skip, "TC not valid for tcu03"};
	_Other ->
	    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
	    TftpUP = proplists:get_value(tftp_up, Config), 

	    %% cp UP to tmp dir in priv_dir
	    TmpPath=?config(priv_dir,Config),
	    os:cmd("chmod 777 "++TmpPath),
	    os:cmd("mkdir "++TmpPath++"tmp"),
	    TmpDir =TmpPath++"tmp/",
	    ct:pal("TmpDir: ~p", [TmpDir]),
	    os:cmd("cp "++TftpBootDir++TftpUP++" "++TmpDir),
	    LS_TmpDir = string_tokens(os:cmd("ls "++TmpDir)),
	    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),
	    [{tmp_dir, TmpDir} | Config]
    end;

init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
end_per_testcase(TestCase = c_1_4, Config) ->
    
    ct:log("end TestCase: ~p",[TestCase]),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
    	    nl_lib:export_ai_log(Config, ?DefaultProt),
    	    %%nl_lib:export_esi(Config, ?DefaultProt),	  		
	    ct:log("## Make sure node is up after tests. Integrate."),
	    nl_lib:integrate(Config, console, ?NC, ?DefaultProt)
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
    [c_1_4].

%%%--------------------------------------------------------------------
%%% @doc Hard factory reset (sda1) -> Download (Upgrade NL) -> integrate.
%%% @spec c_1_4(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_1_4(Config) ->
    ct:pal("C_1_4: AI with sda1, Upgrade NL."),
    
    %%%%
    %% Precondition. Hard factory reset. to make nl on sda1 active.
    %%%%
    ct:pal("Precondition: perform hard_factory_reset."),
    hard_factory_reset(Config),

    %%%%
    %% Check NL version before download
    %%%%
    NLStrList = check_nl_version(Config),
    
    %%%%
    %% Download.
    %%%%
    BoardType = get_board_type(),
    HW =  proplists:get_value(hw, Config), 
 
    NLStatus = check_if_upgrade_nl_after_hfr(Config),

    case NLStatus of
	nl_is_ok ->
	    ct:pal("NL is ok."),
	    generic_download(Config);
	{to_old_nl, NLUpgradeFile} ->

	    ct:pal("NL is to old. Do NL upgrade."),

	    case BoardType of	 
		BoardType when 	BoardType == "tcu04";				
				BoardType == "tcu0401" ->
		    download_config_in_tftpboot_dir(HW, ?TCU_UP_WITH_NL_VERSION_R6Y02);
		BoardType when 	BoardType == "dus5201";				
				BoardType == "dus3201" ->
		    download_config_in_tftpboot_dir(HW, ?DUS_UP_WITH_NL_VERSION_R6Y01);
		BoardType when 	BoardType == "dus5301";				
				BoardType == "dus3301";
				BoardType == "dus6303";
				BoardType == "dus6502" ->
 		    download_config_in_tftpboot_dir(HW, ?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04);
		BoardType when 	BoardType == "c608"->
		    download_config_in_tftpboot_dir(HW, ?C608_UP_WITH_NL_VERSION_R8N04)
	    end,

	    aic_httpc:nl_upgrade(Config, console, NLUpgradeFile),

	    TmpDir = proplists:get_value(tmp_dir, Config), 
	    Org_UP =  proplists:get_value(tftp_up, Config), 
	    Orignal_UP = filename:join(TmpDir, Org_UP),
	    download_config_in_tftpboot_dir(HW, Orignal_UP),

	    generic_download(Config);
	sec_nl -> 
	    %% update ??
	    ct:pal("It is a secure board."),
	    generic_download(Config)
    end,

    %%%%
    %% Check Ailog after download
    %%%%
    check_after_download(Config, NLStrList),

    %%%%
    %% Integrate
    %%%%   
    generic_integrate(Config),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hard_factory_reset(Config) ->
%%     nl_lib:hard_factory_reset(Config, console, ?NC, ?DefaultProt),
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
		    nl_lib:hard_factory_reset(Config, console, ?NC, ?DefaultProt),
		    ct:pal("Board is HardFactoryReset. Now test can start."),
		    timer:sleep(5000),
                    ok
            end;
        true ->
	    nl_lib:hard_factory_reset(Config, console, ?NC, ?DefaultProt),
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

check_nl_version(Config) ->
    NodeNLVer = get_nl_version_node(Config),
    UpNLVer = get_nl_version_up(Config),
    
    ct:log("Network Loader Version on Node: ~p",[NodeNLVer]),
    ct:log("Network Loader Version in UP: ~p",[UpNLVer]),
    UpVerStr = lists:last(string:tokens(UpNLVer, "\- ")),

    NLStrList =
	case do_nl_upgrade(UpNLVer, NodeNLVer)  of
	    true ->
		ct:log("Upgrading Network Loader to version: ~p ~n",[UpVerStr]),
		["Upgrading Network Loader to version "++UpVerStr,
		 "Running version: \""++UpNLVer++"\""];	
	    false ->
		ct:log("Network Loader up to date. Version: ~p ~n",[NodeNLVer]),
		["Network Loader up to date"]
	end,

    NLStrList.

get_nl_version_node(_Config) ->
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    
    start_needed_applications(?DefaultProt),

    HttpsUrl= "https://"++IP++"/help.html",
    HttpUrl= "http://"++IP++":"++?DefaultPort++"/help.html",
   
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
    
    stop_needed_applications(?DefaultProt),

    UrlListRes = string:tokens(UrlRes, " \n\t"),  

    NLNodeVerStr = 
	lists:flatten([VersionStr||VersionStr<-UrlListRes, lists:prefix("CNX", VersionStr)]),
    
    %% NLNodeVerStr = "CNX9012629-R4AC07<p>"
    NLNodeVersionStr = lists:flatten(string:tokens(NLNodeVerStr, "<p>")),
    NLNodeVersionStr.

get_nl_version_up(Config) ->
    TftpUP = proplists:get_value(tftp_up, Config), 
    TmpDir = proplists:get_value(tmp_dir, Config), 

    %% cd tempdir to find NL version in UP (nl_product_version.txt)
    ok = file:set_cwd(TmpDir),
    FileExt = filename:extension(TftpUP),

    %% Check for NL version in product DUS or BPU i.e first try DUS then BPU
    %% Temp check Remove later when Product is the same
    NL_Version_Str = check_for_nl_version_in_dus_up(FileExt, TftpUP),
    
    NLUpVersionStr = 
	case re:run(NL_Version_Str, "No such file or directory") of
	    {match,_}->
		check_for_nl_version_in_bpu_up(FileExt, TftpUP);
	    _ ->
		NL_Version_Str
	end,
    NLUpVersionStr.

check_for_nl_version_in_dus_up(FileExt, TftpUP)->

    CMD_prefix = 
	case FileExt of
	    ".cxs" ->
		%% "RCP*cxs "; 
		"tar -Ozxf "++TftpUP++" --wildcards RCS-*.cxp";
	    ".tgz" ->
		%%"DC*tgz "; %% 
		"tar -Ozxf "++TftpUP++" --wildcards RCS-*.cxp";
	    ".zip" -> 
		%%"CXP*zip "  %% 
		"unzip -p "++TftpUP++" RCS-*.cxp"
	end,

    %% Unpack UP to get file nl_product_version.txt from UP in tmpDir
    NL_CMD = CMD_prefix++" | tar -Ozxf - --wildcards RCSEE*cxp | tar -zxf - --wildcards NL3*/nl*/*version* --strip=3",
    ct:log("NL_CMD for DUS and TCU: ~p", [NL_CMD]),
    os:cmd(NL_CMD),

    NL_up_version = os:cmd("egrep CNX* nl_product_version.txt"), 
    NL_Version_Str= lists:flatten(string:tokens(NL_up_version, "\n")),
    NL_Version_Str.


check_for_nl_version_in_bpu_up(FileExt, TftpUP)->

    CMD_prefix = 
	case FileExt of
	    ".cxs" ->
		%% "RCP*cxs "; 
		"tar -Ozxf "++TftpUP++" --wildcards BRCS_*.cxp";
	    ".tgz" ->
		%%"DC*tgz "; %% 
		"tar -Ozxf "++TftpUP++" --wildcards BRCS_*.cxp";
	    ".zip" -> 
		%%"CXP*zip "  %% 
		"unzip -p "++TftpUP++" BRCS_*.cxp"
	end,

    %% Unpack UP to get file nl_product_version.txt from UP in tmpDir
    NL_CMD = CMD_prefix++" | tar -Ozxf - --wildcards BRCS-EE*cxp | tar -zxf - --wildcards NL3*/nl*/*version* --strip=3",
    ct:log("NL_CMD for BPU: ~p", [NL_CMD]),
    os:cmd(NL_CMD),

    NL_up_version = os:cmd("egrep CNX* nl_product_version.txt"), 
    NL_Version_Str= lists:flatten(string:tokens(NL_up_version, "\n")),
    NL_Version_Str.

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

check_after_download(_Config, ExpectStrList) ->

    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    
    start_needed_applications(?DefaultProt),

    HttpsUrl= "https://"++IP++"/ailog.txt",
    HttpUrl= "http://"++IP++":"++?DefaultPort++"/ailog.txt",
 
    UrlResStr =  
	case httpc:request(HttpsUrl) of
	    {ok,{{_,200,"OK"}, _ReplyHttpsA, ReplyHttpsB}} ->
		ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		ReplyHttpsB;
	    {error, Reason} ->
		case httpc:request(HttpUrl) of
		    {ok,{{_,200,"OK"}, _ReplyHttpA, ReplyHttpB}} ->
			ct:log("ReplyHttpB: ~p ~n",[ReplyHttpB]),
			ReplyHttpB; 
		    {error, Reason} ->
			ct:fail("TC will fail due to unknown protocol.")
		end
	end,
    
    stop_needed_applications(?DefaultProt),
    
    Options = [global,{capture, all, binary}],

    lists:foreach(fun(String) ->    
			  case re:run(UrlResStr, String, Options) of
			      {match, [[Value]]} ->
				  ct:log("Value: ~p ~n",[Value]),
				  ok;
			      nomatch ->
				  ct:fail("TC will fail due to nomatch in ailog.txt.")	
			  end 
		  end, 
		  ExpectStrList).

string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

string_tokens(Str) ->
    string:tokens(Str, " \n").

get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 

get_boardtype() ->
    BoardType = proplists:get_value(board_type,
				    ct:get_config(
				      ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    BoardType.


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
download_config_in_tftpboot_dir(HW, UP)->
    ct:pal("Insert UP in tftpboot dir: ~p ",  [UP]),

    %%%%
    %% Insert faulty/old UP tftpboot dir
    %%%%
   
    CMD = "rcstprep.sh "++HW++" "++UP,
    ct:log("CMD:~n~p~n",[CMD]),

    ExpectStr = "Downloading of UP and preparations for installation OK",
    RcstprepStr = os:cmd(CMD),

    Options = [global,{capture, all, binary}],

    case re:run(RcstprepStr, ExpectStr, Options) of
	{match, [[Value]]} ->
	    ct:log("Value: ~p~n", [Value]),
	    ok;
	_Other ->
	    ct:fail("TC will fail due to NOT expected answere from rcstprep.sh")
    end,

    timer:sleep(10000). % Just in case!

get_board_type()->
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:log("BoardType: ~p", [BoardType]),
    BoardType.

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
		sec_nl; 
	    {_, false, _} ->
		nl_is_ok;
	    {_, _, true} ->
		%%No need to check NL version on dusX3 boards
		nl_is_ok;
	    _Other ->
		%% Could only be runed on unsec 
		ct:pal("check rev on nl type2"),
		get_rev_on_nl_type2(Config, console)
	end,
    NlStatus.


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

%%--------------------------------------------------------------------
support_multi_nl() ->

    Cmd = "ls /proc/device-tree/rcs_nodes/",
    ok = ct_telnet:send(console, Cmd),
    timer:sleep(10000),
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

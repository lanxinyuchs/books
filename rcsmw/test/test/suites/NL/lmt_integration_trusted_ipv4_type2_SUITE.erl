%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_trusted_ipv4_type2_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R7A/R8A/1
%%%
%%% @doc == AI lmt integration trusted network IPv4. ==
%%%
%%%
%%% @end

-module(lmt_integration_trusted_ipv4_type2_SUITE).
-author('etxmlar').
-vsn('/main/R7A/R8A/1').
-date('2017-01-24').

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
%%% Rev       Date        Name        What
%%% -----     ---------   --------    ------------------------
%%% R6A/1     2016-09-02  etxmlar     Created
%%% R6A/2     2016-09-02  etxmlar     Updated and checked in
%%% R8A/1     2017-01-20  etxmlar     Added support for new boardtypes
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
	 nodeUC640_A4_001/1,
	 nodeUC640_A4_005/1
	]).


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

    TftpUP = get_node_up(TftpBootDir),
    ct:log("Tftp UP : ~p ", [TftpUP]),

    BoardType = get_boardtype(),

    [{tftp_up, TftpUP},
     {tftpboot_dir, TftpBootDir},
     {hw, HW},
     {board_type, BoardType}| Config].

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
init_per_testcase(TestCase, Config) 
  when TestCase == nodeUC640_A4_001 ->

    %% cp UP to tmp dir in priv_dir
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    TftpUP = proplists:get_value(tftp_up, Config), 
    BoardType = proplists:get_value(board_type, Config),

    TmpPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++TmpPath),
    os:cmd("mkdir "++TmpPath++"tmp"),
    TmpDir =TmpPath++"tmp/",
    ct:pal("TmpDir: ~p", [TmpDir]),
    os:cmd("cp "++TftpBootDir++TftpUP++" "++TmpDir),
    LS_TmpDir = string_tokens(os:cmd("ls "++TmpDir)),
    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),

    %% Move the original RbsSummaryFile
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Hw = proplists:get_value(hw, Config),
    mv_RbsSummaryFile_to_org(TftpBootDir),

    %% Generat a new RbsSummaryFile for smrs use
    nl_lib:gen_and_write_RbsSummaryFile("RbsSummaryFile.xml", Hw, TftpBootDir),
    SiteInstallationFile = "SiteInstallationFileIpv4.xml",

    %% Check what TN_port to use depending on boardtype
    TN_port = nl_lib:get_tn_port_capitel(BoardType),

    [{tmp_dir, TmpDir},
     {sif, SiteInstallationFile},
     {tn_port, TN_port}| Config];

init_per_testcase(TestCase, Config) 
  when TestCase == nodeUC640_A4_005  ->

    %% First move the original RbsSummaryFile
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Hw = proplists:get_value(hw, Config),
    BoardType = proplists:get_value(board_type, Config),

    mv_RbsSummaryFile_to_org(TftpBootDir),

    %% Generat a new RbsSummaryFile for smrs use
    nl_lib:gen_and_write_RbsSummaryFile("RbsSummaryFile.xml", Hw, TftpBootDir),
    SiteInstallationFile = "SiteInstallationFileIpv4.xml",

    %% Check what TN_port to use depending on boardtype
    TN_port = nl_lib:get_tn_port_capitel(BoardType),

    [{sif, SiteInstallationFile},
     {tn_port, TN_port}| Config].

%% @hidden


end_per_testcase(TestCase, Config)
  when TestCase == nodeUC640_A4_005;
       TestCase == nodeUC640_A4_001 ->

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    %% Remove RbsSummaryFile in tftpboot dir
    RmCmd = "rm "++TftpBootDir++"RbsSummaryFile.xml",
    Rm = os:cmd(RmCmd),
    ct:log("Rm: ~p ", [Rm]),

    %% Move back orig RbsSummaryFilein tftpboot dir
    MvCmd = "mv "++TftpBootDir++"org_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),

    %%remove SIF in priv dir
    SiteInstallationFile = proplists:get_value(sif, Config),
    remove_sif_in_priv_dir(Config, SiteInstallationFile),

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

string_tokens(Str) ->
    string:tokens(Str, " \n").

mv_RbsSummaryFile_to_org(TftpBootDir)->
   
    %% mv RbsSummaryFile to org_RbsSummaryFile.
    MvCmd = "mv "++TftpBootDir++"RbsSummaryFile.xml "++TftpBootDir++"org_RbsSummaryFile.xml",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    true = lists:member("org_RbsSummaryFile.xml" ,Ls),
    false = lists:member("RbsSummaryFile.xml", Ls).

get_boardtype() ->
    BoardType = proplists:get_value(board_type,
				    ct:get_config(
				      ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    BoardType.
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
    [nodeUC640_A4_005,
     nodeUC640_A4_001
    ].

%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nodeUC640_A4_005(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_A4_005(Config) ->
    ct:log("nodeUC640_A4_006:~n"
	   "Run on both Sec/UnSec board, IPv4, default LMT IP adress, SMRS address = IP address~n"
	   "Board restore hard -> Download -> Integrate"),
    
    %%%%
    %% Precondition: Board restore hard.
    %%%%
    ct:log("Precondition: perform board restore hard."),
    board_restore_hard(Config),

    %%%%
    %% Download
    %%%%
    
    %% SIF file = "SiteInstallationFileIpv4.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port = proplists:get_value(tn_port, Config),

    generate_sif_ipv4_trusted(Config, SiteInstallationFile, TN_port),    
    download_no_check(Config, SiteInstallationFile),

    %%%%
    %% Integrate
    %%%%   
    integrate(Config),
    
    ok.

%%%--------------------------------------------------------------------
%%% @doc  Download and Integrate with check
%%% @spec nodeUC640_A4_001(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_A4_001(Config) ->
    ct:log("nodeUC640_A4_002:~n" 
	   "Run on UnSec board, IPv4, default LMT IP adress, SMRS address = IP address~n"
	   "Board restore hard -> Download -> Integrate. With expected checkpoints during download"),

    %%%%
    %% Precondition: Board restore hard.
    %%%%
    ct:log("Precondition: perform board restore hard."),
    board_restore_hard(Config),

    %%%%
    %% Check for NL upgrade
    %%%%
    IsUpgradeTuple = is_nl_upgrade(Config),

    %%%%
    %% Download. 
    %%%%

    %% SIF file = "SiteInstallationFileIpv4.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port = proplists:get_value(tn_port, Config),

    generate_sif_ipv4_trusted(Config, SiteInstallationFile, TN_port),
    download_with_check(Config, SiteInstallationFile, IsUpgradeTuple),
   
    %%%%
    %% Integrate
    %%%%  
    integrate(Config),

    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
board_restore_hard(Config) ->
    aic_httpc:hard_factory_reset(Config, console),
    ct:log("Board restore hard (hard_factory_reset). Now test can start."),
    timer:sleep(5000).

download_no_check(Config, SiteInstallationFile) ->
   
    ct:log("# Start Download LMT Integration."),
    aic_httpc:download_files_lmt_integration(Config, console, SiteInstallationFile),
    ct:log("# Done download LMT Integration."),
    timer:sleep(5000).


download_with_check(Config, SiteInstallationFile, IsUpgradeTuple) ->
   
    ct:log("# Start Download: LMT Integration."),

    nl_lib:httpc_request_lmt_integration(Config, post, "Download", ?Protocol, 
					 ?DefaultPort, SiteInstallationFile),

    %%Get the boards TN port
    BoardType = proplists:get_value(board_type, Config),
    ct:log("BoardType: ~p", [BoardType]),

    TN_port =  nl_lib:get_tn_port_lower_case(BoardType),

    case  ct_telnet:expect(console,
			   "Enabling Northbound interface: "++TN_port, 
    			   [{timeout,120000},no_prompt_check]) of	    
    	{ok, _} ->
    	    {ok, _} = ct_telnet:expect(console,
				       TN_port++" enabled", 
    				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Vlan Interface Added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Default router added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       TN_port++" configured", 
				       [{timeout,60000},no_prompt_check]);
    	_Else ->
	    ct:fail(TN_port++" not enabled when Download files")
    end,

    nl_lib:wait_for_download_completed(Config, ?Protocol),
    ct:log("# Download done: LMT Integration."),
    timer:sleep(5000),

    case IsUpgradeTuple of
	{true, UpNLVer} ->
	    UpVerStr = lists:last(string:tokens(UpNLVer, "\- ")),
	    ct:log("Network Loader is upgraded to: ~p ~n",[UpVerStr]),
	    check_after_download(["Upgrading Network Loader to version "++UpVerStr,
				  "Running version: \""++UpNLVer++"\""]);	
	{false, NodeNLVer} ->
	    ct:log("Network Loader is up to date: ~p ~n",[NodeNLVer]),
	    check_after_download(["Network Loader up to date"])
    end,
    ok.
	
check_after_download(ExpStringList)->

    lists:foreach(fun(String) -> 
			  nl_lib:check_for_printout_in_ailog(String)
		  end, 
		  ExpStringList).	      

integrate(Config) ->

    ct:log("# Start Integrate: LMT integration."),
    aic_httpc:integrate_lmt_integration(Config, console, nc1),
    ct:log("# Integrate done: LMT integration."),
    ct:log("sleep 60sec to make sure node is up and ready for next tests!"),
    timer:sleep(60000).


generate_sif_ipv4_trusted(Config, SiteInstallationFile, TN_port)->

    ct:log("Generate SIF and write to priv dir"),  
    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    ok = nl_lib:gen_and_write_sif_ipv4_trusted(Config,
					       SiteInstallationFile, 
					       "RbsSummaryFile.xml", 
					       HwId, TN_port),  

    %% Check SIF exist in priv dir
    Priv_dir = ?config(priv_dir, Config),
    Ls = string_token(os:cmd("ls "++Priv_dir)),

    ct:pal("ls : ~p ", [Ls]),
    true = lists:member(SiteInstallationFile, Ls).

remove_sif_in_priv_dir(Config, SiteInstallationFile)->

    Priv_dir = ?config(priv_dir, Config),
    RmCmd = "rm "++Priv_dir++SiteInstallationFile,
    Rm = os:cmd(RmCmd),
    ct:log("Rm: ~p ", [Rm]),

    Ls = string_token(os:cmd("ls "++Priv_dir)),
    ct:log("ls : ~p ", [Ls]),							
    ok.

get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 

is_nl_upgrade(Config)->
    NodeNLVer = get_nl_version_node(Config),
    UpNLVer = get_nl_version_up(Config),
    
    ct:log("Network Loader Version on Node: ~p",[NodeNLVer]),
    ct:log("Network Loader Version in UP: ~p",[UpNLVer]),

    IsUpgradeTuple =
	case do_nl_upgrade(UpNLVer, NodeNLVer)  of
	    true ->
		{true, UpNLVer};
	    false ->
		{false, NodeNLVer}
	end,

    IsUpgradeTuple.

get_nl_version_node(_Config) ->
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    
    start_needed_applications(?Protocol),

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
    
    stop_needed_applications(?Protocol),

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
    
    CMD_prefix = 
	case FileExt of
	    ".cxs" ->
		%% "RCP*cxs "; 
		"tar -Ozxf "++TftpUP++" --wildcards RCS*.cxp";
	    ".tgz" ->
		%%"DC*tgz "; %% 
		"tar -Ozxf "++TftpUP++" --wildcards RCS*.cxp";
	    ".zip" -> 
		%%"CXP*zip "  %% 
		"unzip -p "++TftpUP++" RCS*.cxp"
	end,
    
    %% Unpack UP to get file nl_product_version.txt from UP in tmpDir
    NL_CMD = CMD_prefix++" | tar -Ozxf - --wildcards RCSEE*cxp | tar -zxf - --wildcards NL3*/nl*/*version* --strip=3",
    ct:log("NL_CMD: ~p", [NL_CMD]),
    os:cmd(NL_CMD),
 
    NL_up_version = os:cmd("egrep CNX* nl_product_version.txt"), 
    NLUpVersionStr= lists:flatten(string:tokens(NL_up_version, "\n")),
    NLUpVersionStr.

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



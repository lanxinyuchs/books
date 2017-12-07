%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_no_downgrade_2_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015
%%% @version /main/R4A/4
%%%
%%% @doc == AI user interface tests. Only for dus ==
%%%
%%%
%%% @end

-module(nl_no_downgrade_2_SUITE).
-author('etxmlar').
-vsn('/main/R4A/4').
-date('2015-10-30').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% R4A/1      2015-10-12 etxmlar     Created 
%%% R4A/2      2015-10-12 etxmlar     Updated 
%%% R4A/3      2015-10-20 etxmlar     Corrected CXS UP check
%%% R4A/5      2015-10-30 etxmlar     Bug fix
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
	 c_1_5/1,
	 install_node/1
	]).


-define(NC, nc1).
-define(DefaultProt, "https").
-define(DefaultPort, "8080").
-define(OLD_DUS_UP_URL, "$RDE_TOP/tools/jenkins/rcs_query.sh -N MSRBS -T 15B -q url_path").
-define(DUS_UP_URL, "$RDE_TOP/tools/jenkins/rcs_query.sh -N MSRBS -q url_path").
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

    NodeUP = get_node_up(TftpBootDir),
    ct:log("Node UP : ~p ", [NodeUP]),
    
    CxsUP = check_if_cxs_up(NodeUP, TftpBootDir),
    ct:log("CXS UP : ~p ", [CxsUP]),
    
    [{tftpboot_dir, TftpBootDir},
     {node_up, NodeUP},
     {cxs_up, CxsUP},
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

init_per_testcase(TestCase = c_1_5, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),

    case get_boardtype() of
	"tcu03" ->
	    {skip, "TC not valid for tcu03"};
	_Other ->
	    
	    %% Create tmp dir in priv_dir
	    TmpPath=?config(priv_dir,Config),
	    os:cmd("chmod 777 "++TmpPath),
	    os:cmd("mkdir "++TmpPath++"tmp"),
	    TmpDir =TmpPath++"tmp/",
	    ct:pal("TmpDir: ~p", [TmpDir]),
	    [{tmp_dir, TmpDir} | Config]
    end;


init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(TestCase, Config) when TestCase == c_1_5 ->

    ct:log("End TestCase: ~p",[TestCase]),

    HW =  proplists:get_value(hw, Config),  
    NodeUp =  proplists:get_value(node_up, Config), 
    CxsUP =  proplists:get_value(cxs_up, Config),
    FileExt = filename:extension(NodeUp),
    %%ct:log("FileExt: ~p",[FileExt]),

    case FileExt of
	".cxs" -> 
	    download_config_in_tftpboot_dir(HW, CxsUP);
	FileExt when FileExt == ".tgz";
		     FileExt == ".zip" ->
	    case lists:prefix("dus",HW) of
		true ->
		    DUS_URL = os:cmd(?DUS_UP_URL),
		    DUS_UP_URL = lists:flatten(string:tokens(DUS_URL,"\n ")),
		    download_config_in_tftpboot_dir(HW, DUS_UP_URL);		   
		false ->
		    %% Not working for 16A maybe in 16B. 
		    %% TCU_URL = os:cmd(?TCU_UP_URL),
		    %% TCU_UP_URL = lists:flatten(string:tokens(TCU_URL,"\n ")),
		    %% download_config_in_tftpboot_dir(HW, TCU_UP_URL)
		    continue
	    end		
    end,


    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
    	    nl_lib:export_ai_log(Config, ?DefaultProt),
    	    nl_lib:export_esi(Config, ?DefaultProt)	  		
    end,
    ok;

end_per_testcase(TestCase, _Config) ->
    ct:log("End TestCase: ~p",[TestCase]),
    ok.

check_if_cxs_up(NodeUP, TftpBootDir)->
    FileExt = filename:extension(NodeUP),
    ct:log("FileExt: ~p",[FileExt]),

    CxsUP = 
	case FileExt of
	    ".cxs" ->
		FileList = filelib:wildcard(TftpBootDir++"cxslabel_*"),
		CxsFileStr = lists:flatten(filename:basename(FileList)),
		UP = lists:subtract(CxsFileStr, "cxslabel_"),
		ct:log("CXS UP:~n~p~n",[UP]),
		UP;
	    FileExt when FileExt == ".tgz";
			 FileExt == ".zip" ->
		[]
	end,
    CxsUP.

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
    [c_1_5,
     install_node].



install_node(Config) ->
    ct:log("## Make sure node is up after tests.  Download and integrate."),
    nl_lib:download_files(Config, console, ?DefaultProt),
    nl_lib:integrate(Config, console, ?NC, ?DefaultProt).
%%%--------------------------------------------------------------------
%%% @doc Hard factory reset (sda1) -> Download (Upgrade NL) -> integrate.
%%% @spec c_1_5(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_1_5(Config) ->
    ct:pal("C_1_5: No NL downgrade when remove nl dir in sda2."),
    
    %%%%
    %% Precondition. Hard factory reset. to make nl on sda1 active.
    %%%%
    ct:pal("Precondition: Hard Board restore + Remove nl in sda2."),
    hard_factory_reset(Config),

    %%%%
    %% Remove dir nl on sda2 
    rm_nl_sda2(console),
    %%%%

    %%%%
    %% Check NL version on node
    %%%%
    NodeNLVerBefore = get_nl_version_node(Config),
    ct:log("Network Loader Version on Node: ~p", [NodeNLVerBefore]),

    %%%%
    %% Before Download add R15 UP in boot dir
    %%%%
    HW = proplists:get_value(hw, Config), 
    UP = os:cmd(?OLD_DUS_UP_URL),
    UP_URL = lists:flatten(string:tokens(UP,"\n ")),
    download_config_in_tftpboot_dir(HW, UP_URL),

    %%%%
    %% Check NL version in up
    %%%%
    UPversion = get_nl_version_up(Config),
    ct:log("Network Loader Version in UP: ~p", [UPversion]),

    %%%%
    %% Download.
    %%%%
    generic_download(Config),

    %%%%
    %% Integrate
    %%%%   
    generic_integrate(Config),

    %%%%
    %% Board restore
    %%%%   
    board_restore(Config),
    
    %%%%
    %% Check NL version on node
    %%%%
    NodeNLVerAfter = get_nl_version_node(Config),
    ct:log("Network Loader Version on Node: ~p", [NodeNLVerAfter]),

    %%%%
    %% Check correct NL version on node + booted from sda2
    %%%%
    check_nl_version(NodeNLVerBefore, NodeNLVerAfter),

    %%%%
    %% Download.
    %%%%
    generic_download(Config),

    %%%%
    %% Integrate
    %%%%   
    generic_integrate(Config),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hard_factory_reset(Config) ->

    nl_lib:hard_factory_reset(Config, console, ?NC, ?DefaultProt),
    ct:pal("Board is HardFactoryReset."),
    timer:sleep(5000).


rm_nl_sda2(Console) ->

    A = ct_telnet:send(Console, "mount -t ext2 /dev/sda2 /mnt"),
    ct:pal("A: ~p",[A]),
    timer:sleep(2000), % Just in case!
    B = ct_telnet:send(Console, "rm -rf /mnt/nl"),
    ct:pal("B: ~p",[B]),
    timer:sleep(2000), % Just in case!
    C = ct_telnet:send(Console, "sync;sync"),
    ct:pal("C: ~p",[C]),
    timer:sleep(2000), % Just in case!
    D = ct_telnet:send(Console, "umount /mnt"),
    ct:pal("D: ~p",[D]),
    timer:sleep(2000), % Just in case!
    E = ct_telnet:send(Console, "sync;sync"),
    ct:pal("E: ~p",[E]),

    ct:pal("Remove nl in sda2 (Factory mode). Now test can start."),
    timer:sleep(10000),
    ok.

board_restore(Config) ->
    ct:pal("Perform board restore."),
    nl_lib:board_restore(Config, console, ?NC, ?DefaultProt),
    ct:pal("Board is restored."),
    timer:sleep(5000).

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

    TmpDir = proplists:get_value(tmp_dir, Config),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    TftpUP = get_node_up(TftpBootDir),
    ct:log("Tftp UP : ~p ", [TftpUP]),

    %%cp UP from tftp-dir to tmp-dir
    os:cmd("cp "++TftpBootDir++TftpUP++" "++TmpDir),
    LS_TmpDir = string_tokens(os:cmd("ls "++TmpDir)),
    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),

    TmpDir = proplists:get_value(tmp_dir, Config), 

    %% cd to tmp-dir to find NL version in UP (nl_product_version.txt)
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



download_config_in_tftpboot_dir(HW, UP)->
    ct:pal("Insert UP in tftpboot dir: ~p ",  [UP]),

    %%%%
    %% Insert faulty/old UP tftpboot dir
    %%%%
   
    CMD = "rcstprep.sh "++HW++" "++UP,
    ct:log("CMD:~n~p~n",[CMD]),
    os:cmd(CMD),
    timer:sleep(10000). % Just in case!

check_nl_version(NodeNLVerBefore, NodeNLVerAfter) ->
    case NodeNLVerBefore == NodeNLVerAfter of
	true ->
	    check_type3_booted_from_sda2(console);
	false ->
	    ct:pal("Testcase failed due to NL version missmatch:\n~p == ~p.", [NodeNLVerBefore, NodeNLVerAfter]),
	    ct:fail("Wrong Network Loader version on node: ~p",[NodeNLVerAfter])

    end.


check_type3_booted_from_sda2(Console) ->
    ct_telnet:send(Console, "cat /nl/log/nl_log.1"),
    {ok, _} = ct_telnet:expect(Console,
			       "booted from partition /dev/sda2", 
			       [{timeout,300000},no_prompt_check]),
    
    ok.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ai_user_interface_2_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R6A/R7A/1
%%%
%%% @doc == AI user interface tests. ==
%%%
%%%
%%% @end

-module(ai_user_interface_2_SUITE).
-author('etxmlar').
-vsn('/main/R4A/R6A/R7A/1').
-date('2016-11-07').

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
%%% R4A/1      2015-09-18 etxmlar     Created
%%% R4A/2      2015-09-22 etxmlar     Updated
%%% R4A/3      2015-10-20 etxmlar     Corrected nl vesion check
%%% R4A/4      2015-10-20 etxmlar     Removed a printout
%%% R4A/5      2015-10-21 etxmlar     Small fix
%%% R4A/6      2015-10-30 etxmlar     Bug fix
%%% R4A/7      2015-10-30 etxmlar     Small fix
%%% R4A/8      2015-11-06 etxmlar     Removed a testline
%%% R7A/1      2016-11-06 etxmlar     Updated get_nl_version_up
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
	 c_1_3/1
	]).


-define(NC, nc1).
-define(DefaultProt, "https").
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
    %% run rcstprep.sh to prepare for NL upgrade
    HW = atom_to_list(ct:get_config({test_nodes,1})),

    %% ============ cxs UP
    %%UP = "CXS101549_5-R4AC37", %%For test
    UP = get_UP(),
    RcstPrep_CMD = "rcstprep.sh "++HW++" "++UP,
    ct:log("RcstPrep_CMD: ~p", [RcstPrep_CMD]),
    os:cmd(RcstPrep_CMD),

    %% ============ Full UP
    %%UP = ct:get_config({jenkins_config, container}), %%Full UP
  
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
init_per_testcase(TestCase = c_1_3, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),

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
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
    	    nl_lib:export_ai_log(Config, ?DefaultProt),
    	    nl_lib:export_esi(Config, ?DefaultProt)	  		
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
    [c_1_3
    ].

%%%--------------------------------------------------------------------
%%% @doc Board restore (sda2) -> Download (Upgrade NL) -> integrate.
%%% @spec c_1_3(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_1_3(Config) ->
    ct:pal("C_1_3: AI with sda2, Upgrade NL."),
    
    %%%%
    %% Precondition. Board restore.
    %%%%
    ct:pal("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Check NL version before download
    %%%%
    NLStr = check_nl_version(Config),
    
    %%%%
    %% Download.
    %%%%
    generic_download(Config),

    %%%%
    %% Check Ailog after download
    %%%%
    check_after_download(Config, NLStr),

    %%%%
    %% Integrate
    %%%%   
    generic_integrate(Config),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
board_restore(Config) ->
    nl_lib:board_restore(Config, console, ?NC, ?DefaultProt),
    ct:pal("Board is restored. Now test can start."),
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

check_nl_version(Config) ->
    NodeNLVer = get_nl_version_node(Config),
    UpNLVer = get_nl_version_up(Config),

    ct:log("Network Loader Version on Node: ~p",[NodeNLVer]),
    ct:log("Network Loader Version in UP: ~p",[UpNLVer]),
    UpVerStr = lists:last(string:tokens(UpNLVer, "\- ")),
    
    NLStr =
	case do_nl_upgrade(UpNLVer, NodeNLVer) of
	    true ->
		ct:log("Upgrading Network Loader to version: ~p ~n",[UpVerStr]),
		"Upgrading Network Loader to version "++UpVerStr;		
	    false ->
		ct:log("Network Loader up to date. Version: ~p ~n",[NodeNLVer]),
		"Network Loader up to date"
	end,
    NLStr.

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

check_after_download(_Config, ExpectStr) ->

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
		%%ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		ReplyHttpsB;
	    {error, Reason} ->
		case httpc:request(HttpUrl) of
		    {ok,{{_,200,"OK"}, _ReplyHttpA, ReplyHttpB}} ->
			%ct:log("ReplyHttpB: ~p ~n",[ReplyHttpB]),
			ReplyHttpB; 
		    {error, Reason} ->
			ct:fail("TC will fail due to unknown protocol.")
		end
	end,
    
    stop_needed_applications(?DefaultProt),

    
    Options = [global,{capture, all, binary}],
    
    case re:run(UrlResStr, ExpectStr, Options) of
	{match, [[Value]]} ->
	    ct:log("Value: ~p ~n",[Value]),
	    ok;
	nomatch ->
	    ct:fail("TC will fail due to nomatch in ailog.txt.")
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

do_nl_upgrade(UpNLVer, NodeNLVer)->

    %%NL Version in UP: "CNX9012629-R4AA02"
    %%NL on Node: "CNX9012629-R4U02"

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

get_UP() ->
    UP = ct:get_config({jenkins_config, cxp}),
    case UP of
        undefined ->
            get_latest_UP(get_boardtype());
        _ ->
        	UP
    end.

get_latest_UP(BoardType) ->
    Label = case string:to_upper(string:substr(BoardType, 1, 3)) of
        "DUS" -> "DUS2Label";
        "TCU" -> "TLabel"
    end,
    OS_cmd = "wget --no-check-certificate -q https://rbs-rde-ci.rnd.ki.sw.ericsson.se/view/stable_label/job/stable_prep4TestInfo.txt/lastSuccessfulBuild/artifact/prep4TestInfo.txt -O - | grep "++Label++" | awk -F ':' '{printf \"%s\", $2}'",
    ct:pal("Getting latest stable UP: ~p~n", [OS_cmd]),
    os:cmd(OS_cmd).

%% install_node(Config)->
%%     board_rerestore(Config),
%%     reinstall_node(Config).
    
%% board_rerestore(Config) ->
%%     case nl_lib:open_netconf(?NC) of
%% 	{ok, _} ->
%% 	    ct:pal("Precondition: perform board restore."),
%% 	    nl_lib:board_restore(Config, console, ?NC, ?DefaultProt),
%% 	    ct:pal("Board i restored. Now test can start."),
%% 	    timer:sleep(5000);
%% 	_Other ->
%% 	    ct:log("Board Restored not needed !. ~p", [_Other])
%%     end.

%% reinstall_node(Config) ->
%%     nl_lib:download_files(Config, console, ?DefaultProt),
%%     nl_lib:integrate(Config, console, ?NC, ?DefaultProt).



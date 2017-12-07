%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ai_gui_test_2_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R7A/R9A/4
%%%
%%% @doc == AI gui tests.==
%%%
%%%
%%% @end

-module(ai_gui_test_2_SUITE).
-author('etxmlar').
-vsn('/main/R4A/R5A/R7A/R9A/4').
-date('2017-04-20').

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
%%% R4A/1      2015-09-25 etxmlar     Created
%%% R4A/2      2015-09-25 etxmlar     Updated 
%%% R4A/3      2015-09-28 etxmlar     Added testcase c_6_4, c_6_5
%%% R4A/4      2015-10-01 etxmlar     now() depricated in OTP 18 changed to os:timestamp()
%%% R4A/5      2015-10-20 etxmlar     Corrected nl vesion check
%%% R4A/6      2015-10-20 etxmlar     Removed a printout
%%% R4A/7      2015-10-21 etxmlar     Small fix
%%% R4A/8      2015-10-30 etxmlar     Bug fix
%%% R4A/9      2015-11-13 etxmlar     Random time was on the edge
%%% R5A/1      2016-02-12 etxmlar     Added printout
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% R7A/2      2016-10-25 etxmlar     Updated to handle old NL + large LKF for TCU
%%% R7A/3      2016-10-25 etxmlar     Removed a line
%%% R7A/4      2016-11-06 etxmlar     Updated get_nl_version_up
%%% R7A/5      2016-12-05 etxmlar     Temp update? To support BPU
%%% R7A/6      2017-01-11 etxmlar     Update the check, if nl is to old
%%% R9A/1      2017-01-26 etxmlar     Added support for new boardtypes
%%% R9A/2      2017-03-13 etxmlar     Updated to make it more robust
%%% R9A/3      2017-03-20 etxmlar     Updated to make it more reliable
%%% R9A/4      2017-04-20 etxmlar     R7 NL can install X3 UPs from R9A
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
	 c_6_3/1,
	 c_6_4/1,
	 c_6_5/1
	]).


-define(NC, nc1).
%% -define(Protocol, "http").
%% -define(Port, "8080").
-define(Protocol, "https").
%% -define(Port, "443"). %% Not needed
-define(DUS_UP_WITH_NL_VERSION_R6Y01, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_5/R2EGN/CXP9024418_5-R2EGN.zip"). %%NL version CNX9012629-R6Y01
-define(TCU_UP_WITH_NL_VERSION_R6Y02, "https://arm110-eiffel001.seli.gic.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_5/R2JBS/CXP9024419_5-R2JBS.zip"). %%NL version CNX9012629-R6Y02

-define(C608_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").  %%NL version CNX9012629-R8N04

%%-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R4A241/CXP9024418_6-R4A241.zip").  %%NL version CNX9012629-R8N04

-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_DETECTING_AXM5600_BOOTSIG, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R11A122/CXP9024418_6-R11A122.zip"). %% NL R9U04

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
init_per_testcase(TestCase, Config) when TestCase == c_6_3;
					 TestCase == c_6_4;
					 TestCase == c_6_5 ->

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
    [c_6_3,
     c_6_4,
     c_6_5
    ].


%%%--------------------------------------------------------------------
%%% @doc Download -> Cancel during download in request phase then download + integrate.
%%% @spec c_6_3(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_6_3(Config) ->
    ct:pal("C_6_3: Cancel during download in request phase then download + integrate.", []),

    %%%%
    %% Precondition. Do board restore hard, to perform cancel
    %% when NL upgrade (when NL version is newer).
    %%%%   
    hard_factory_reset(Config),

    {NlUg, _NodeNLVer, _UpNLVer} = check_nl_version(Config),
    
    case NlUg of
	yes ->

	    %% Cancel test 1
	    ct:pal("start cancel test 1. Cancel during download progess in request phase"),
	    start_download(Config),
	    random_sleep(40),
	    nl_lib:cancel(Config, console, ?Protocol),

	    %% Cancel test 2      
	    ct:pal("start cancel test 2. Cancel Prepare filesystem, request phase."),
	    start_download(Config),
	    {ok, _} = ct_telnet:expect(console,
				       "Prepare filesystem : Started", 
				       [{total_timeout,480000}, {idle_timeout,180000}, no_prompt_check]),
	    %%[{timeout,120000},no_prompt_check]),

	    random_sleep(5),
	    nl_lib:cancel(Config, console, ?Protocol);

	no ->
	    continue
    end,


    %% Download.
    download_after_cancel_when_hrf(Config),
    timer:sleep(10000),   

    %% Integrate 
    integrate(Config),

    %% Cancel test 3

    %% Precondition. Do board restore, to perform cancel
    %% when NL upgrade in sda2 and request phase (when NL version is newer).

    board_restore(Config),
    modify_nl_product_version(),

    ct:pal("start cancel test 3. Unpack and install software, request phase."),
    start_download(Config),
    {ok, _} = ct_telnet:expect(console,
			       "Unpack and install software : Started", 
			       [{total_timeout,480000}, {idle_timeout,180000}, no_prompt_check]),
  
    nl_lib:cancel(Config, console, ?Protocol),
    timer:sleep(5000),

    %% Power off
    ok = ct:pal("### power off!",[]),
    ok = rct_power:off(node),
    timer:sleep(10000),    

    %% Power on  
    ct:pal("### power on!",[]),
    ok = rct_power:on(node),

    check_after_power_on(),

    %% Download.
    download(Config),
    timer:sleep(10000),   

    %% Integrate  
    integrate(Config),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Download -> Cancel during download in reboot phase then download + integrate.
%%% @spec c_6_4(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_6_4(Config) ->
    ct:pal("C_6_4: Cancel during download in reboot phase then download + integrate.", []),

    %%%%
    %% Precondition. Do board restore hard, to perform cancel
    %% when NL upgrade (when NL version is newer).
    %%%%   
    hard_factory_reset(Config),
   
    {NlUg, _NodeNLVer, UpNLVer} = check_nl_version(Config),
    
    case NlUg of
	yes ->
            %%%%
	    %% Cancel test 1
            %%%%
	    ct:pal("Start cancel test. Prepare board with an old NL"),
	    BoardType = get_board_type(),
	    HW =  proplists:get_value(hw, Config), 

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
		    download_config_in_tftpboot_dir(HW, ?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_DETECTING_AXM5600_BOOTSIG);
		BoardType when 	BoardType == "c608"->
		    download_config_in_tftpboot_dir(HW, ?C608_UP_WITH_NL_VERSION_R8N04)
	    end,

	    NLStatus = check_if_upgrade_nl_after_hfr(Config),

	    case NLStatus of
		nl_is_ok ->
		    ct:pal("NL is ok."),
		    continue;
		{to_old_nl, NLUpgradeFile} ->
		    ct:pal("NL is to old. Do NL upgrade."),
		    aic_httpc:nl_upgrade(Config, console, NLUpgradeFile);	  
		sec_nl -> 
		    %% update ??
		    ct:pal("It is a secure board."),
		    continue
	    end,

	    ct:pal("Start cancel test. Cancel when Prepare filesystem, reboot phase."),
	    TmpDir = proplists:get_value(tmp_dir, Config), 
	    Org_UP =  proplists:get_value(tftp_up, Config), 
	    Orignal_UP = filename:join(TmpDir, Org_UP),

	    download_config_in_tftpboot_dir(HW, Orignal_UP),
	    start_download(Config),
	    case  ct_telnet:expect(console,
				   "Saved configuration loaded", 
				   [{total_timeout, 480000}, {idle_timeout,120000},no_prompt_check]) of	    
		{ok, _} ->
		    {ok, _} = ct_telnet:expect(console,
					       "Prepare filesystem : Started", 
					       [{total_timeout,480000}, {idle_timeout,120000},no_prompt_check]);
		_Else ->
		    ct:fail("Cancel when Prepare filesystem in reboot phase")
	    end,	

	    random_sleep(5),
	    nl_lib:cancel(Config, console, ?Protocol),
	    ok = check_after_cancel(UpNLVer),

	    timer:sleep(5000);

	no ->   
	    continue
    end,
    
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

%%%--------------------------------------------------------------------
%%% @doc Download -> Cancel during download in reboot phase then download + integrate.
%%% @spec c_6_5(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
c_6_5(Config) ->
    ct:pal("C_6_4: Cancel during download in reboot phase then download + integrate.", []),

    %%%%
    %% Precondition. Do board restore hard, to perform cancel
    %% when NL upgrade (when NL version is newer).
    %%%%   
    hard_factory_reset(Config),
   
    {NlUg, _NodeNLVer, UpNLVer} = check_nl_version(Config),
    
    case NlUg of
	yes ->
            %%%%
	    %% Cancel test 1
            %%%%
	    ct:pal("Start cancel test. Prepare board with an old NL"),
	    BoardType = get_board_type(),
	    HW =  proplists:get_value(hw, Config), 

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
		    download_config_in_tftpboot_dir(HW, ?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_DETECTING_AXM5600_BOOTSIG);
		BoardType when 	BoardType == "c608"->
		    download_config_in_tftpboot_dir(HW, ?C608_UP_WITH_NL_VERSION_R8N04)
	    end,

	    NLStatus = check_if_upgrade_nl_after_hfr(Config),

	    case NLStatus of
	    	nl_is_ok ->
	    	    ct:pal("NL is ok."),
	    	    continue;
		{to_old_nl, NLUpgradeFile}  ->
	    	    ct:pal("NL is to old. Do NL upgrade."),
	    	    aic_httpc:nl_upgrade(Config, console, NLUpgradeFile);	  
	    	sec_nl -> 
	    	    %% update ??
	    	    ct:pal("It is a secure board."),
	    	    continue
	    end,

	    ct:pal("Start cancel test. Cancel when Unpack and install software, reboot phase."),
	    TmpDir = proplists:get_value(tmp_dir, Config), 
	    Org_UP =  proplists:get_value(tftp_up, Config), 
	    Orignal_UP = filename:join(TmpDir, Org_UP),

	    download_config_in_tftpboot_dir(HW, Orignal_UP),
	    start_download(Config),
	    case  ct_telnet:expect(console,
				   "Saved configuration loaded", 
				   [{total_timeout,480000},{idle_timeout,120000},no_prompt_check]) of	    
		{ok, _} ->
		    {ok, _} = ct_telnet:expect(console,
					       "Unpack and install software : Started", 
					       [{total_timeout,480000},{idle_timeout,120000},no_prompt_check]);
		_Else ->
		    ct:fail("Cancel when Unpack and install software in reboot phase")
	    end,	

	    random_sleep(5),
	    nl_lib:cancel(Config, console, ?Protocol),
	    ok = check_after_cancel(UpNLVer),

	    timer:sleep(5000);

	no ->   
	    continue
    end,
    
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
%% hard_factory_reset(Config) ->
%%     ct:pal("Precondition: perform board restore hard."),
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

download_after_cancel_when_hrf(Config) ->
    ct:pal("# Download."),
    NlStatus = download_after_hfr(Config),

    case NlStatus of
	nl_is_ok ->
	    ct:pal("# download Started."),
	    nl_lib:download_files(Config, console, ?Protocol);
	{to_old_nl, NLUpgradeFile} ->
	    ct:pal("Do NL upgrade before download."),
	    aic_httpc:nl_upgrade(Config, console, NLUpgradeFile),
	    ct:pal("# download Started."),
	    nl_lib:download_files(Config, console, ?Protocol);
	sec_nl ->
	    ct:pal("# download Started secure board."),
	    aic_httpc:download_files(Config, console)
    end,
    ct:pal("# Done, download."),
    timer:sleep(5000).


integrate(Config) ->
    ct:pal("### Integrate."),
    nl_lib:integrate(Config, console, ?NC, ?Protocol),
    ct:pal("### Done Integrate."),
	timer:sleep(5000).

start_download(Config)->
    ct:pal("# Start Download."),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol),
    ct:pal("# download Started.").

%% start_download_after_hfr(Config) ->
%%     ct:pal("# Start Download."),
%%     NlStatus = download_after_hfr(Config),
%%     case NlStatus of
%% 	nl_is_ok ->
%% 	    ct:pal("# download Started."),
%% 	    nl_lib:httpc_request(Config, post, "Download", ?Protocol);
%% 	to_old_nl ->
%% 	    ct:pal("Do NL upgrade before download."),
%% 	    aic_httpc:nl_upgrade(Config, console,"RbsSummaryFile.xml.nl15B"),
%% 	    ct:pal("# download Started."),
%% 	    nl_lib:httpc_request(Config, post, "Download", ?Protocol);
%% 	sec_nl -> 
%% 	    ct:pal("# download Started secure board."),
%% 	    aic_httpc:download_files(Config, console)
%% 	end,
%%     ok.

check_if_upgrade_nl_after_hfr(Config)->
    ct:pal("# Check nl and upgrade if it is to old."),
    NlStatus = download_after_hfr(Config),
    NlStatus.
    

random_sleep(MaxNr) ->
    %%%%
    %% Get a random nr.
    %%%%
    RandomNr = rand:uniform(MaxNr),
    ct:pal("# Start Sleep for: ~p sec.", [RandomNr]),
    ct:sleep({seconds, RandomNr}),
    RandomNr.

check_nl_version(Config) ->
    NodeNLVer = get_nl_version_node(Config),
    UpNLVer = get_nl_version_up(Config),
    ct:log("Network Loader Version on Node: ~p",[NodeNLVer]),
    ct:log("Network Loader Version in UP: ~p",[UpNLVer]),
    
    NlUg =
	case do_nl_upgrade(UpNLVer, NodeNLVer) of
	    true ->
		ct:log("Upgrading Network Loader. ~n",[]),
		yes;		
	    false ->
		ct:log("Network Loader up to date. Skip testcase ~n",[]),
		no
	end,
    
    {NlUg, NodeNLVer, UpNLVer}.

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

check_after_cancel(UpNLVer) ->

    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    
    start_needed_applications(?Protocol),

    HttpsUrl= "https://"++IP++"/ailog.txt",
    HttpUrl= "http://"++IP++":"++?Protocol++"/ailog.txt",
 
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
    
    stop_needed_applications(?Protocol),

    
    Options = [global,{capture, all, binary}],
    ct:log("NL version to find in ailog.txt: UpNLVer: ~p ~n",[UpNLVer]),
    ct:log("Ailog.txt, UrlResStr: ~p ~n",[UrlResStr]),
    
    case re:run(UrlResStr, UpNLVer, Options) of
	{match, [[Value]]} ->
	    ct:log("Value: ~p ~n",[Value]),
	    ok;
	nomatch ->
	    ct:fail("TC will fail due to nomatch in ailog.txt.")
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
    {ok, _} = ct_telnet:expect(console,
			       "Running version:", 
			       [{total_timeout,30000},no_prompt_check]),

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
					   [{total_timeout,5000},no_prompt_check]),

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


check_after_power_on() ->
    case ct_telnet:expect(console, 
			  "Ericsson Version:",
			  [{total_timeout,30000}, no_prompt_check]) of
	{ok,_} ->

	    {ok,_} = ct_telnet:expect(console, 
				      "Network Loader Ready:",
				      [{total_timeout,180000}, {idle_timeout,180000}, no_prompt_check]),

	    ct_telnet:expect(console, "Build date:",
			     [{total_timeout,180000}, {idle_timeout,120000}, no_prompt_check]);

	_Else -> %% Power did not work! clean for next TC.
	    ct:pal("### TC will fail ### due to power off on did not work!!!"),
	    ct:pal("### Perorm a extra power to clean up for next TC!"),
	    rct_power:cycle(node),
	    ct_telnet:expect(console, 
			     "Build date:",
			     [{total_timeout,120000}, no_prompt_check]),
	    ct:fail("Power did not work, Node has not restarted !!")
    end,
    timer:sleep(15000).

modify_nl_product_version()->

    NL_product_version = "CNX9012629-R1A01",

    CmdCommand = "echo "++NL_product_version++" > /nl/nl_product_version.txt",
    ct:log("CmdCommand:~p ~n",[CmdCommand]),
    ct_telnet:cmd(console, CmdCommand),
    ok.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	fetch_5g_esi_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R10A/R12A/3
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using cli.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end


-module(fetch_5g_esi_SUITE).
%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2017-05-08 etxivri     Created
%%% R10A/2     2017-05-12 etxivri     Update to check cli is up before fetch esi.
%%% R10A/3     2017-05-12 etxivri     Removed unused hooks.
%%% R10A/4     2017-05-15 etxivri     Removed logging and core hook.
%%% R12A/1     2017-11-20 etxivri     Update to not use rpc towards vnfm
%%% R12A/2     2017-11-24 etxivri     Update to fetch esi from vnfm using logMo
%%% ----------------------------------------------------------
%%%

						% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 fetch_esi_from_all_nodes/1,
	 fetch_vnfm_esi_rest_api/1,
	 break/1
	 %% transfer_esi_unpacked/1
	]).


-define(ERROR_STRING, "\"ERROR REPORT|CRASH REPORT\"").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    TestNodesList = ct:get_config(test_nodes),
    ct:pal("TestNodesList : ~p", [TestNodesList]),

    CliHooks = 
	[{N, list_to_atom("cli"++integer_to_list(N)), ssh_lmt_ipv4, [manual_connect]} || {N, _A} <- TestNodesList],
    ct:log("CliHooks : ~p", [CliHooks]),

    [{ct_hooks, 
      [{rct_htmllink,[]},
       {rct_vnfm,[vnfm1]},
       %% {rct_node_state,NodeStateHooks},
       %% {rct_rpc, rpc1},
       %% {rct_netconf, NetconfHooks},
       {cth_conn_log,[]},
       {rct_consserv,cs1},
       {rct_cli, CliHooks}
       %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
       %% {rct_core,[]}
      ]}].


    

%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
	ok -> 
	    ct:log("status ~p",[proplists:get_value(tc_status, Config)]);
	R ->
	    ct:log("test case ~p",[R])
	    %% aic_curl:poll_all_ai_esi(Config, 900)
    end,
    Config.

break(_Config) ->
    test_server:break("AA").

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [].


%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
fetch_esi_from_all_nodes(Config) ->
    TestNodesList = ct:get_config(test_nodes),
    ct:pal("TestNodesList : ~p", [TestNodesList]),
    EsiDataList = lists:map(fun({N, _Type}) ->
				%% case N of
				%%     1 -> 
				%%         %% Is an VNFM, use rest api to fetch esi
				%% 	ct:log("################ Fetch vnfm esi ################"),
				%% 	fetch_vnfm_esi_rest_api(Config, N);
				%%     _Other ->
					ct:log("################ Fetch esi from node nr : ~p ################", [N]),
					transfer_esi_unpacked_cli(Config, N)
				%% end
			end, TestNodesList),
    ct:pal("# # EsiDataList: ~p", [EsiDataList]),

    ct:pal("# Unpack all fetched esi.", []),
    UnpackEsiDirList =  lists:map(fun(EsiData) ->
					 case EsiData of
					     {skiped, dummy, TestNodeNr} ->
						{skiped, TestNodeNr};
					     {EsiName, EsiLogPath, TestNodeNr} ->
						 ct:pal("# EsiName: ~p , # TestNodeNr: ~p", [EsiName, TestNodeNr]),
						 UnpackEsiDir = unpack_esi(EsiName, EsiLogPath, TestNodeNr),
						 {UnpackEsiDir, TestNodeNr}
						 %% ct:pal("# # UnpackEsiDir: ~p", [UnpackEsiDir])
					 end
				 end, EsiDataList),
    ct:log("# # UnpackEsiDirList: ~p", [UnpackEsiDirList]),
    check_for_pmd(UnpackEsiDirList),
    ok.

check_for_pmd(UnpackEsiDirList) ->
    lists:foreach(fun({UnpackEsiDir, TestNodeNr}) ->
			  case UnpackEsiDir of
			      skiped -> 
				  ct:log("# TestNodeNr :~p. No esi fetched, skip to check pmd", [TestNodeNr]);
			      _Other ->
				  case TestNodeNr of
				      1 ->
					  ct:log("# TestNodeNr :~p." 
						 "~nThis is an vnfm,  No pmd dir exist in esi. skip to check pmd.", 
						 [TestNodeNr]);
				      _Node ->
					  ct:log("# TestNodeNr :~p.~nUnpackEsiDir: ~p", [TestNodeNr, UnpackEsiDir]),
					  ok = check_esi_for_pmd(UnpackEsiDir, ["rcs/dumps/appdump/","rcs/dumps/pmd/"])
					  %% %% ok = read_esi_log_file(UnpackEsiDir, "rcs/erlang/*", ?ERROR_STRING),
				  end
			  end
		  end,UnpackEsiDirList).

%%--------------------------------------------------------------------
%% @doc
%% fetch_vnfm_esi_rest_api
%% @end
%%--------------------------------------------------------------------
fetch_vnfm_esi_rest_api(Config) ->
    fetch_vnfm_esi_rest_api(Config, 1).
fetch_vnfm_esi_rest_api(Config, Nr) ->
    ct:pal("Create a esi name using the os cmd date.", []),
    {{Y,Mo,D},{H,Mi,S}}=calendar:local_time(),
    VnfmTimeStamp = lists:flatten(io_lib:format("date_~4.4.0w-~2.2.0w-~2.2.0w_time_~2.2.0w-~2.2.0w-~2.2.0w",[Y,Mo,D,H,Mi,S])),

    ct:log("VnfmTimeStamp: ~n~p", [VnfmTimeStamp]),
    VnfmEsiName = VnfmTimeStamp++"_vnfm_esi.tgz",
    ct:log("VnfmEsiName: ~n~p", [VnfmEsiName]),

    ct:log("# Get esi log path.", []),
    EsiLogPath=proplists:get_value(priv_dir,Config),
    ct:log("EsiLogPath: ~p", [EsiLogPath]),

    ct:pal("# Get vnfm esi.", []),
    Res = rct_vnfm:fetch_vnfm_esi_rest_api(vnfm1, VnfmEsiName, EsiLogPath),
    ct:log("Res : ~n~p", [Res]),
    
    ok = wait_for_vnfm_esi_exist(VnfmEsiName, EsiLogPath, 120000),
    ct:pal("# Unpack esi.", []),

    B = os:cmd("ls "++EsiLogPath),
    ct:log("# # B: ~p", [string_tokens(B, " \n")]),
	    
    {VnfmEsiName, EsiLogPath, Nr}.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
transfer_esi_unpacked_cli(Config, Nr)->
    ct:pal("### Fetch ESI from testnode Nr : ~p", [Nr]),
    Cli = list_to_atom(atom_to_list(cli)++integer_to_list(Nr)),
    case wait_for_cli(Cli) of
	cli_not_up ->
	    ct:pal("TestNode Nr: ~p can not be reached with cli. Skip fetch esi. ", [Nr]),
	    {skiped, dummy, Nr};
	_Other ->
	    ct:log("### _Other : ~p", [_Other]),
	    MeId = get_integer_attribute(Cli, "show verbose ManagedElement=1", "managedElementId"),
	    EsiLogPath=?config(priv_dir,Config),
	    ct:log("# EsiLogPath: ~p", [EsiLogPath]),
	    os:cmd("chmod 777 "++EsiLogPath), % else permission.
	    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
	    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
	    Uri = SFTP_URL++EsiLogPath,
	    ok = rct_cli:connect(Cli),
	    {ok,A} = rct_cli:send(Cli,"ManagedElement="++MeId++",SystemFunctions=1,LogM=1,exportEsi "++Uri++" "++Password, print),
	    ct:pal("# A: ~p", [A]),
	    ok = rct_cli:disconnect(Cli),
	    timer:sleep(5000),
	    
	    JobId = get_progress_report_id(A),
	    wait_for_export_is_finished(Cli, MeId, JobId),
	    
	    EsiName = get_esi_name(Cli, MeId, JobId),
	    ct:pal("# # EsiName: ~p", [EsiName]),

	    %% ct:pal("# # EsiLogPath: ~p", [EsiLogPath]),
	    B = os:cmd("ls "++EsiLogPath),
	    ct:log("# # B: ~p", [string_tokens(B, " \n")]),
	    
	    {EsiName, EsiLogPath, Nr}
    end.


get_esi_name(Cli, MeId, JobId) ->
    rct_cli:connect(Cli),
    timer:sleep(2000),
    EsiName = case rct_cli:send(Cli," show ManagedElement="++MeId++",SystemFunctions=1,LogM=1,progressReport="++JobId++",resultInfo", print) of
		  {ok,A} ->
		      ct:pal("# resultInfo : ~p", [A]),
		      AnswList = string:tokens(A, "\r\n \">"),
		      lists:last(AnswList);
		  _Other ->
		      not_found
	      end,
    ct:pal("# EsiName : ~p", [EsiName]),
    EsiName.

string_tokens(Str, What) ->
    AnswList = string:tokens(Str,What ),
    ct:pal("# # AnswList: ~p", [AnswList]),
    AnswList.

get_progress_report_id(A) ->
    AnswList = string_tokens(A, " \r\n>"),
    JobId = lists:last(AnswList),
    ct:pal("# # JobId: ~p", [JobId]),
    JobId.

wait_for_export_is_finished(Cli, MeId, JobId) ->
    wait_for_export_is_finished(Cli, MeId, JobId, 300000).

wait_for_export_is_finished(_Cli, _MeId, _JobId, Timeout) when Timeout < 0 ->
    ct:pal("Export Esi did not finished within expected time!"),
    ct:fail("Export Esi did not finished within expected time!");

wait_for_export_is_finished(Cli, MeId, JobId, Timeout) ->
    rct_cli:connect(Cli),
    timer:sleep(2000),
    case rct_cli:send(Cli," show ManagedElement="++MeId++",SystemFunctions=1,LogM=1,progressReport="++JobId, print) of
	{ok,A} ->
	    ct:log("# Export esi,progressReport: ~n~p", [A]),
	    case re:run(A, "SUCCESS") of
		{match, _} ->
		    rct_cli:disconnect(Cli),
		    ct:pal("# Export esi is SUCCESS.", []);
		nomatch ->
		    ct:pal("# Export esi not finished, sleep ant check again.", []),
		    timer:sleep(10000),
		    wait_for_export_is_finished(Cli, MeId, JobId, Timeout-10000)
	    end;
	_Other ->
	    rct_cli:disconnect(Cli),
	    ct:log("# _Other: ~p", [_Other]),
	    ct:log("# # Export esi not finished, sleep ant check again.", []),
	    timer:sleep(10000),
	    wait_for_export_is_finished(Cli, MeId, JobId, Timeout-10000)
    end.    


wait_for_vnfm_esi_exist(_VnfmEsiName, _EsiLogPath, Timeout) when Timeout < 0 -> 
    ct:log("### Did not foun esi within expected timeout", []),
    nok;
wait_for_vnfm_esi_exist(VnfmEsiName, EsiLogPath, Timeout) ->
    Ls = os:cmd("ls "++EsiLogPath),
    ct:log("### ls ~p : ~n~p", [EsiLogPath, Ls]),
    %% Check esi exist in logdir
    case re:run(Ls, VnfmEsiName) of
	{match, _} ->
	    ct:pal("esi : ~p fetched ok. ~nExist under : ~p", [VnfmEsiName, EsiLogPath]),
	    ok;
	nomatch ->
	    ct:pal("Esi not found! Sleep and try check again!"),
	    timer:sleep(10000),
	    wait_for_vnfm_esi_exist(VnfmEsiName, EsiLogPath, Timeout-10000)
    end.

unpack_esi(EsiName, EsiLogPath, TestNodeNr) ->
    %% Construct a dir name where to unpack the esi
    UnpackDir = atom_to_list(ct:get_config({test_nodes, TestNodeNr})),
    UnpackEsiDir = filename:join(EsiLogPath, UnpackDir),
    ct:log("UnpackEsiDir : ~p", [UnpackEsiDir]),
    os:cmd("mkdir " ++ UnpackEsiDir),
    os:cmd("tar -xf " ++ EsiLogPath ++ EsiName ++" --gunzip --directory=" ++ UnpackEsiDir),
    format_html("<a href=\"~s\">~s</a>", [UnpackEsiDir, "Esi unpacked dir "++UnpackDir]),
    format_html("<a href=\"~s\">~s</a>", [filename:join(EsiLogPath,EsiName), EsiName]),
    UnpackEsiDir.


format_html(String,Args) ->
     ct:log(default, 1, String, Args, [no_css]).

get_integer_attribute(Cli,Command, Attribute) ->
    rct_cli:connect(Cli),
    ct:pal("# Cli connect. OK to continue!",[]),
    Re =  ".*" ++ Attribute ++"=\\\""++"(\\d+).*",
    {_,Reply} =  rct_cli:send(Cli, Command),
    ct:log("Reply ~p",[Reply]),
    
    case  re:run(Reply, Re, [global,{capture, all, binary}]) of
	{match,[[_,Value]]} -> ok = rct_cli:disconnect(Cli),
			       binary:bin_to_list(Value);
	nomatch  -> rct_cli:disconnect(Cli),
		    "1"
    end.


%% %% check_if_vc_board()->
%% %%     Hw = atom_to_list(ct:get_config({test_nodes,1})),
%% %%     Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
%% %%     Secure_board.

%% read_esi_log_file(EsiLogPath, LogFileDir, ErrorString) ->
%%     case os:cmd("egrep " ++  ErrorString  ++ " " ++  filename:join(EsiLogPath, LogFileDir))  of
%% 	[]->
%% 	    ok;
%% 	Error -> ct:fail("Error in erlang log: ~n~p",[Error])
%%     end.

check_esi_for_pmd(_EsiLogPath, []) ->
    ok;
check_esi_for_pmd(EsiLogPath, [DumpDir|Rest]) ->
    PmdPath = filename:join(EsiLogPath, DumpDir),
    ct:pal("# PmdPath: ~p", [PmdPath]),
    case os:cmd("ls " ++ PmdPath) of
	[] ->  ct:pal("# No coredump found #",[]),
	       check_esi_for_pmd(EsiLogPath, Rest);
	Dump -> case string:str(Dump,"cannot access") > 0 of
		    true -> ct:pal("WARNING:~p no such file or directory",[DumpDir]),
			    check_esi_for_pmd(EsiLogPath, Rest);
		    false ->  ct:fail("Dumps found in dir ~p: ~p",[DumpDir, Dump])
		end
    end.


wait_for_cli(Cli) ->
    wait_for_cli(Cli, 120000).
wait_for_cli(_Cli, Timeout) when Timeout < 0 ->
    cli_not_up;
wait_for_cli(Cli, Timeout) ->
    case rct_cli:connect(Cli) of
	ok ->
	    rct_cli:disconnect(Cli),
	    ct:pal("# Cli connect. OK to continue!",[]),
	    cli_up;
	_Other ->
	    rct_cli:disconnect(Cli),
	    ct:log("# Cli NOT connected. Sleep and try again! ~n~p",[_Other]),
	    timer:sleep(10000),
	    wait_for_cli(Cli, Timeout-10000)
    end.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ai_httpc_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/6
%%% @doc ==Installs du board using httpc==
%%% @end

-module(ai_httpc_SUITE).
-id('Updated by CCase').
-vsn('/main/R3A/6').
-date('2015-05-29').
-author('etxkols').
-include_lib("common_test/include/ct.hrl").
%%% ----------------------------------------------------------
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
%%% R3B/1      2015-02-017 etxivri     Created
%%% R3B/3      2015-02-17  etxivri     Removed a timeout.
%%% R3B/4      2015-02-18  etxivri     Add some checks in some TCs.
%%% R3B/5      2015-02-27  etxkols     Preparations for 2 labs
%%% R3A/6      2015-05-28  etxkols     Changed rct_netconf hook format 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
	 download_files/1,
	 integrate/1,
	 board_restore/1,
	 factory_reset/1,
	 export_log/1,
	 export_esi/1
	 ]).

-define(NcSess, nc1).
-define(FileName, "fakeEsi").
-define(FileNameExportEsi, "exportEsi").

%%--------------------------------------------------------------------
%% @doc
%% Used hooks:
%% @end
%%--------------------------------------------------------------------
suite() ->
    case check_if_vc_board() of
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
	    [{timetrap,{minutes,60}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_power,power},
			 {rct_consserv,cs1},
			 {rct_netconf,nc1},
			 {rct_ssh,{ssh,[manual_connect]}},
			 {rct_rs232,console},
			 %% {rct_logging, {all,[{erlang,
			 %% 		      {["ERROR REPORT","CRASH REPORT"],
			 %% 		       []}}]}},
			 {cth_conn_log,[]}]}]
    end.

%% @hidden
init_per_suite(Config) ->
    %% inets:start(),
    Config.
%% @hidden
end_per_suite(_Config) ->
    %% inets:stop(),
    ok.

%% @hidden
%% init_per_testcase(download_files = TC, Config) ->
%%     inets:stop(),   %% Need this for this TC on secure boards
%%     inets:start(),  %% Otherwise you get {error,socket_closed_remotely}
%%     init_per_tc_check_logs(TC, Config),
%%     Config;
init_per_testcase(integrate = TC, Config) ->
    inets:start(),
    init_per_tc_check_logs(TC, Config),
    Config;
init_per_testcase(export_esi = TC, Config) ->
    inets:start(),
    init_per_tc_check_logs(TC, Config),
    Config;
init_per_testcase(_TestCase, Config) ->
    inets:start(),
    Config.


%% @hidden
end_per_testcase(integrate = TC, Config) ->
    inets:stop(),
    end_per_tc_check_logs(TC, Config),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    tc_failed_try_to_export_esi_and_fakelog(Config, Reason)
    end;
end_per_testcase(export_esi = TC, Config) ->
    inets:stop(),
    end_per_tc_check_logs(TC, Config),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    tc_failed_try_to_export_esi_and_fakelog(Config, Reason)
    end;
end_per_testcase(_TestCase, Config) ->
    inets:stop(),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    tc_failed_try_to_export_esi_and_fakelog(Config, Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Try to export logs if tc fails on not secure borads.
%% @end
%%--------------------------------------------------------------------
tc_failed_try_to_export_esi_and_fakelog(Config, Reason) -> 
   case check_if_vc_board() of
	"yes" -> 
	    ok;
	_ ->
	   ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
	   ct_telnet:send(console, ""),
	   case ct_telnet:expect(console, 
				 "login:", [{timeout,10000},no_prompt_check]) of
	       {ok, _} ->
		   ct:pal("Try to export ESI:", []),
		   export_esi(Config);
	       _Other ->
		   ct:pal("Try to export fakeEsi if stuck in nl", []),
		   export_log(Config)
	   end
   end.

%%--------------------------------------------------------------------
%% @doc
%% Use this to get logs after some of the TCs has been runed.
%% TCs that are in NL state can not use this.
%% @end
%%--------------------------------------------------------------------
init_per_tc_check_logs(TC, Config) ->
    case check_if_vc_board() of
	"yes" -> 
	    ok;
	_ ->
	    rct_logging:pre_init_per_suite(TC, 
					   Config, 
					   {all,[{erlang,
						  {["ERROR REPORT",
						    "CRASH REPORT"],
						   []}}]} ),
	    rct_logging:pre_init_per_testcase(TC, 
					      Config, 
					      {all,[{erlang,
						     {["ERROR REPORT",
						       "CRASH REPORT"],
						      []}}]} )
    end.

end_per_tc_check_logs(TC, Config) ->
    case check_if_vc_board() of
	"yes" -> 
	    ok;
	_ ->
	    rct_logging:post_end_per_testcase(TC,
					      Config, 
					      ok, 
					      {all,[{erlang,
						     {["ERROR REPORT",
						       "CRASH REPORT"],
						      []}}]} )
    end.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [board_restore, 
     download_files, 
     export_log,
     integrate, 
     export_esi].

%%--------------------------------------------------------------------
%% @doc
%% @spec download_files(Config) -> ok
%% @end
%%--------------------------------------------------------------------
download_files(Config) ->
    case check_node_is_in_nl() of
	nl ->
	    ok;
	_Else ->
	    ct:fail("TC will wail due to board is not in NL. "
    		    "Not possible to do download_files")
    end,

    httpc_request(Config, post, "Download"),
    timer:sleep(20000),
    case check_if_vc_board() of
    	"yes" -> 
    	    %% check_expect_consol("Secure Boot Enabled", 300000, Config),
    	    %% check_expect_consol("Network Loader Ready:", 300000, Config);
    	    wait_for_download_done();
    	_Other ->
    	    {ok, _} = ct_telnet:expect(console, 
    				       "The board can be powered off", 
    				       [{timeout,120000},no_prompt_check]),
    	    ct:pal("Rcvd : The board can be powered off. OK to continue",[])
    end.

wait_for_download_done() ->
    wait_for_download_done(300000).
wait_for_download_done(Timeout) when Timeout < 0 ->
    ct:fail("TC will fail due to Download not done within expected time.");
wait_for_download_done(Timeout) ->
    ct_telnet:send(console, "cat /nl/ServerRoot/htdocs/progress.txt"),
    case ct_telnet:expect(console, "DownloadReady", 
			   [{timeout,20000},no_prompt_check]) of
	{ok, _} ->
	    ok;
	_Other ->
	    ct:pal("download is not done, sleep and try again."),
	    timer:sleep(10000),
	    wait_for_download_done(Timeout-10000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec integrate(Config) -> ok
%% @end
%%--------------------------------------------------------------------
integrate(Config) ->
    case check_node_is_in_nl() of
	nl ->
	    ok;
	_Else ->
	    ct:fail("TC will wail due to board is not in NL. "
    		    "Not possible to do integrate")
    end,

    httpc_request(Config, post, "Integrate"),
    
    case check_if_vc_board() of
	"yes" -> 
	    check_expect_consol("Secure Boot Enabled", 300000, Config),
	    check_expect_consol("Running rcs_start", 300000, Config);
	_Other ->
	    case ct_telnet:expect(console, "login:", 
				  [{timeout,120000},no_prompt_check]) of
		{ok, _} ->
		    ok;
		_ -> % login: prompt can be corrupted on dus2
		    ok = ct_telnet:send(console, ""),
		    {ok, _} = 
			ct_telnet:expect(console, 
					 "login:", 
					 [{timeout,20000},no_prompt_check])
	    end,
	    ct:pal("Rcvd : login prompt.",[])
    end,

    wait_for_netconf_started(),
    site_config_complete(),
    ok.


check_expect_consol(ExpStr, Timeout, Config) ->
   case  ct_telnet:expect(console, ExpStr, 
			  [{timeout, Timeout}, no_prompt_check]) of
       {ok, _} -> 
	   ok;
       _ ->
	   ct:log("### TC will fail die to timeout when check consol logs"),
	   export_log(Config), %% If type 2 sw is used
	   export_esi(Config), %% If type 3 sw is used
	   ct:fail("No match in consol log within expected time.")
   end.

%%--------------------------------------------------------------------
%% @doc
%% @spec board_restore(Config) -> ok
%% @end
%%--------------------------------------------------------------------
board_restore(Config) ->
    %% ct_telnet:send(console, ""),
    %% case ct_telnet:expect(console, "networkloader]#", 
    %% 			  [{timeout,5000},no_prompt_check]) of
    %% 	{ok, _} ->
    %% 	    ct:fail("TC will wail due to board is in NL. "
    %% 		    "Not possible to do board restore");
    %% 	_ ->
    %% 	    wait_for_netconf_started()
    %% end,
	    
    %% wait_for_netconf_started(),
    site_config_complete(?NcSess, 120000),
    httpc_request(Config, post, "BoardRestore"),
    case check_if_vc_board() of
	"yes" -> 
	    check_expect_consol("Secure Boot Enabled", 300000, Config),
	    check_expect_consol("Network Loader Ready:", 300000, Config),
	    timer:sleep(20000);
	_Other ->
	    ExpectStr = "Welcome to Autointegration with laptop", 
	    {ok, _} = ct_telnet:expect(console,
				       ExpectStr, 
				       [{timeout,300000},no_prompt_check]),
	    ct:pal("Rcvd : ~p, Now you are in NL. ",[ExpectStr])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec factory_reset(Config) -> ok
%% @end
%%--------------------------------------------------------------------
factory_reset(Config) ->
    wait_for_netconf_started(),
    site_config_complete(),
    httpc_request(Config, post, "FactoryReset"),
    case check_if_vc_board() of
	"yes" -> 
	    check_expect_consol("Secure Boot Enabled", 300000, Config),
	    check_expect_consol("Network Loader Ready:", 300000, Config);
	_Other ->
	    ExpectStr = "Welcome to Autointegration with laptop", 
	    {ok, _} = ct_telnet:expect(console,
				       ExpectStr, 
				       [{timeout,300000},no_prompt_check]),
	    ct:pal("Rcvd : ~p, Now you are in NL. ",[ExpectStr])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec export_log(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_log(Config) ->
    FileName = ?FileName,
    EsiLogPath = ?config(priv_dir,Config),
    httpc_request(Config, post, "Export"),
    timer:sleep(5000),
    wait_log_exist_in_log_path(FileName, EsiLogPath),

    create_fake_esi_dir(FileName ++ "_ericsson", EsiLogPath),
    create_fake_esi_dir(FileName ++ "_prev_ericsson", EsiLogPath),
  
    io:format("<a href=\"~s\">~s</a>",
	      [EsiLogPath, "Fake Esi dir"]),
    ct:pal("log esi : ~p, exist in ~n path: ~n~p ", [FileName, EsiLogPath]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec export_esi(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_esi(Config) ->
    wait_for_netconf_started(),
    FileName = ?FileNameExportEsi,
    EsiLogPath = ?config(priv_dir,Config),
    httpc_request(Config, post, "ESI"),

    timer:sleep(5000),
    wait_log_exist_in_log_path(FileName, EsiLogPath),

    ct:pal("esi: ~p, exist in ~n path: ~n~p ", [FileName, EsiLogPath]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
httpc_request(Config, Method, DoAction) ->
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
%    [{host, SftpHost},{username, Username},{password, Password}] = 
%	ct:get_config({Hw,sftp_server}),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    SftpAiInstallDir = ct:get_config({Hw,sftp_ai_install_dir}),
    MyTime = integer_to_list(ms_since_1970()),
    
    ct:log("BoardType: ~p~n"
	   "HW: ~p~n"
	   "IP: ~p~n"
	   "sftp Host: ~p~n"
	   "Username: ~p~n"
	   "Password: ~p~n"
	   "SftpAiInstallDir: ~p~n"
	   "MyTime: ~p~n",
	   [BOARDTYPE,
	    HW,
	    IP,
	    SftpHost,
	    Username,
	    Password,
	    SftpAiInstallDir,
	    MyTime]),
    
    case DoAction of
	Val when Val == "Download";
		 Val == "Integrate" ->
	    Url = "http://"++IP++":8080/cgi-bin/nl_gui:post",
	    Body = "host="++SftpHost++"&"++
		"username="++Username++"&"++
		"password="++Password++"&"++
		"filename=boards/"++HW++"/RbsSummaryFile.xml"++"&"++
		"utcMs="++MyTime++"&"++
		"Do"++DoAction++"="++DoAction;
	%% %%%%
	%% %% BoardRestore and FactoryReset
	%% %%%%
	Val when Val == "BoardRestore";
		 Val == "FactoryReset" ->
	    Url = "http://"++IP++":8080/cgi-bin/aicGui:post",
	    Body = "Do"++DoAction++"="++DoAction;
	%% %%%%
	%% %% Export and ESI
	%% %%%%
	Val when Val == "Export";
		 Val == "ESI" ->
	    case DoAction of
		"Export" ->
		    Url = "http://"++IP++":8080/cgi-bin/nl_gui:post",
		    FileName = ?FileName;
		"ESI" ->
		    Url = "http://"++IP++":8080/cgi-bin/aicGui:post",
		    FileName = ?FileNameExportEsi
	    end,
	    EsiLogPath = ?config(priv_dir,Config),
	    os:cmd("chmod 777 "++ EsiLogPath),
	    ct:pal("EsiLogPath : ~p", [EsiLogPath]),
	    Body = "host="++SftpHost++"&"++
		"username="++Username++"&"++
		"password="++Password++"&"++
		"filename="++EsiLogPath++FileName++"&"++
		"Do"++DoAction++"="++DoAction;

	_Other ->
	    Url = dummy,
	    Body = dummy,
	    ct:fail("### No Valid DoAction not found")
    end,

    ct:pal("DoAction : ~p", [DoAction]),
    ct:pal("url : ~p", [Url]),
    ct:pal("Body : ~p", [Body]),

    {ok, Reply} = httpc:request(Method, {Url, [], [], Body}, [], []),
    ct:log("Reply : ~p", [Reply]),
    {{_,200,"OK"}, _ReplyA, _ReplyB} = Reply,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
site_config_complete() ->
    ct:log("Wait for netconf rbsConfigLevel: SITE_CONFIG_COMPLETE"),
    site_config_complete(?NcSess, 180000).
site_config_complete(_NC, Timeout) when Timeout < 0 ->
    ct:fail("Could not get SITE_CONFIG_COMPLETE with netconf "
	    "within expected time");
site_config_complete(NC, Timeout) ->
    open_netconf(NC),
	    {ok,[{'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],["1"]},
		   {'NodeSupport',
		    [{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
		    [{nodeSupportId,[],["1"]},
		     {'AutoProvisioning',
		      [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
		      [{autoProvisioningId,[],["1"]},
		       {rbsConfigLevel,[],[SITE_CONFIG_COMPLETE]}]}]}]}]} =
		ct_netconfc:get(NC,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'NodeSupport',[],
				       [{nodeSupportId,[],["1"]},
					{'AutoProvisioning',[],
					 [{autoProvisioningId,[],["1"]}]}]}]}),
	    case SITE_CONFIG_COMPLETE of
		"SITE_CONFIG_COMPLETE" ->
		    ct:log("rbsConfigLevel: SITE_CONFIG_COMPLETE"),
		    ok = ct_netconfc:close_session(NC);
		_ ->
		    ct_netconfc:close_session(NC),
		    ct:log(yellow,"Wrong rbsConfigLevel: ~s, "
			   "Retrying in 5 seconds",[SITE_CONFIG_COMPLETE]),
		    timer:sleep(5000),
		    site_config_complete(NC, Timeout-5000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
ms_since_1970() ->
    list_to_integer(string:strip(os:cmd("date -u +%s"),right,$\n)) * 1000.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
%%Create dir and decrypt
create_fake_esi_dir(FileName, EsiLogPath)->
    case filelib:is_file(EsiLogPath ++ FileName) of
	true ->
	    FakeEsiDir = EsiLogPath ++  FileName ++ "_dir", 
	    os:cmd("mkdir " ++ FakeEsiDir),
	    ct:log("Create dir  ~p",[FakeEsiDir]),
	    Out = EsiLogPath ++ FileName ++".tgz",
	    FakeEsiDir = EsiLogPath ++ FileName  ++ "_dir", 
	    %%Crypro is started from hook
	    case  
		file:write_file(Out, 
				crypto:block_decrypt(aes_cfb128,
						     <<"1234567890ABCDEF">>,
						     <<"1234567890ABCDEF">>,
						     element(2, 
							     file:read_file(
							       EsiLogPath ++
								   FileName))))
	    of
		ok -> 
		    os:cmd("tar -xf " ++ 
			       Out ++
			       " --gunzip --directory=" ++ 
			       FakeEsiDir );
		{error, Reason} -> 
		    ct:log("Error ~p",[Reason]);
		Other -> 
		    ct:log("Other ~p",[Other])
	    end;
	false -> ct:log("No file ~p",[FileName])
    end,
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
wait_for_netconf_started() ->
    ct:pal("### Check Netconf",[]),
    wait_for_netconf_started(180000).
wait_for_netconf_started(Timeout) when Timeout < 0 ->
    ct:fail("Netconf not started within max timeout.");
wait_for_netconf_started(Timeout) ->
    case open_netconf(?NcSess, []) of
	{ok,_} ->
	    ok = ct_netconfc:close_session(?NcSess),
	    ok;
	Res  ->
	    ct:log("Res: ~p", [Res]),
	    timer:sleep(5000),
	    wait_for_netconf_started(Timeout - 5000)
    end.

open_netconf(NC) ->
    open_netconf(NC, []).
open_netconf(NC, Param) ->
    case check_if_vc_board()  of
	"yes" ->
	    ct_netconfc:open(NC, [{user, "SysAdminTest"}, 
				  {password, "SysAdminTest"} | Param]);
	_Oter ->  
	    ct_netconfc:open(NC, Param)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
wait_log_exist_in_log_path(FileName, EsiLogPath) ->
    wait_log_exist_in_log_path(FileName, EsiLogPath, 60000).
wait_log_exist_in_log_path(FileName, EsiLogPath, Timeout) when Timeout < 0 ->
    ct:pal("Tc will fail due to log does NOT exist within expected time.~n"
	   "filename : ~p~n"
	   "LogPath : ~p", [FileName, EsiLogPath]),
    ct:fail("log does NOT exist within expected time");
wait_log_exist_in_log_path(FileName, EsiLogPath, Timeout) ->
    LS = os:cmd("ls "++EsiLogPath),
    LsList = string:tokens(LS, " .\n"),
    ct:log("ls : ~p", [LsList]),
    
    case re:run(LsList, FileName) of
	{match, _} ->
	    ct:log("Log exist in log path", []),
	    timer:sleep(5000);
	_Other ->
	    ct:log("Log NOT exist in log path, sleep and try again.", []),
	    timer:sleep(5000),
	    wait_log_exist_in_log_path(FileName, EsiLogPath, Timeout-5000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_node_is_in_nl() ->
    ct_telnet:send(console, ""),
    case ct_telnet:expect(console, "networkloader]#", 
			  [{timeout,5000},no_prompt_check]) of
	{ok, _} ->
	    nl;
	_ -> %% prompt can be corrupted, try second time
	    timer:sleep(5000),
	    ct_telnet:send(console, ""),
	    case ct_telnet:expect(console, "networkloader]#", 
			  [{timeout,5000},no_prompt_check]) of
		{ok, _} ->
		    nl;
		_Other ->
		    no_nl
	    end
    end.

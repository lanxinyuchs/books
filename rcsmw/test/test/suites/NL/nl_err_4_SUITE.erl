%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_err_4_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R4A/R5A/R6A/R7A/1
%%%
%%% @doc == NL / AI Error testcases. Fault in SiteBasic.xml ( our name is config_initial.netconf) ==
%%%
%%%
%%% @end

-module(nl_err_4_SUITE).
-author('etxmlar').
-vsn('/main/R3A/R4A/R5A/R6A/R7A/1').
-date('2016-10-24').

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
%%% R3A/1      2015-05-18 etxivri     Created.
%%% R3A/2      2015-05-25 etxivri     Add new TCs.
%%% R3A/4      2015-06-17 etxivri     Make search for expected dtr more robust.
%%% R4A/1      2015-09-08 etxivri     Changed a ct:pal to ct:log
%%% R5A/1      2016-01-08 etxmlar     ExpectStr updated because of
%%%                                   split_files in aicServer.erl   
%%% R5A/3      2016-02-04 etxmlar     Updated TC site_basic_with_several_session  
%%% R5A/4      2016-02-09 etxmlar     Changed to ct:log because of
%%%                                   parsing error in ct:log  
%%% R7A/1      2016-10-21 etxmlar     Added support to decrypt ESI         
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
	 install_node/1,
	 syntax_fault_in_SiteBasic/1,
	 wrong_content_in_SiteBasic/1,
	 err_site_basic_with_several_session/1,
	 site_basic_with_several_session/1 
	]).


-define(NC, nc1).
-define(Cli, cli).
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
			 {cth_conn_log,[]},
			 {rct_cli, {cli,
				    [{user, "SysAdminTest"}, 
				     {password, "SysAdminTest"},
				     manual_connect]}}
			]}];
	_Other ->
	    [{timetrap, {minutes, 60}}, % 1 hours
	     {ct_hooks, [{rct_rpc, rpc},
			 {rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,power},
			 {rct_netconf, nc1},
			 {cth_conn_log, []},
			 {rct_cli, {cli, [manual_connect]}}
			]}]
    end.


%% @hidden
init_per_suite(Config) ->
    HW = atom_to_list(ct:get_config({test_nodes,1})),
    TftpBootDir = "/proj/rcs-tmp/tftpboot/"++HW++"/",
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:pal("ls : ~p ", [Ls]),
    [{tftpboot_dir, TftpBootDir},
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
init_per_testcase(TestCase, Config) ->

    ct:pal("TestCase: ~p",[TestCase]),
    cp_config_initial_to_org(Config),
    Config.

end_per_testcase(_TestCase, Config) ->
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    %% Move org_RbsSummaryFile to RbsSummaryFile.
    MvCmd = "mv "++TftpBootDir++"org_config_initial.netconf "++
    	TftpBootDir++"config_initial.netconf",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),

    false = lists:member("org_config_initial.netconf", Ls),
    true = lists:member("config_initial.netconf", Ls),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    ok.


string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"), 
    ListOfStr.


cp_config_initial_to_org(Config) ->
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    ct:pal("our name on SiteBasic.xml is config_initial.netconf."),
    %% copy config_initial.netconf to org_config_initial.netconf.
    CpCmd = "cp "++TftpBootDir++"config_initial.netconf "++
	TftpBootDir++"org_config_initial.netconf",
    Cp = os:cmd(CpCmd),
    ct:log("Cp : ~p ", [Cp]),
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    true = lists:member("org_config_initial.netconf" ,Ls),
    true = lists:member("config_initial.netconf", Ls).

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
    [board_restore,
     syntax_fault_in_SiteBasic,
     wrong_content_in_SiteBasic,
     err_site_basic_with_several_session,
     site_basic_with_several_session, 
     install_node
    ].

board_restore(Config) ->
    case nl_lib:open_netconf(?NC) of
	{ok, _} ->
	    ct:pal("Precondition: perform board restore."),
	    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
	    ct:pal("Board is restored. Now test can start."),
	    timer:sleep(5000);
	_Other ->
	    ct:log("Board Restored not needed !. ~p", [_Other])
    end.

install_node(Config) ->
    ct:pal("## Make sure node is up after tests.  Download and integrate."),
    nl_lib:download_files(Config, console, ?Protocol),
    nl_lib:integrate(Config, console, ?NC, ?Protocol).

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec syntax_fault_in_SiteBasic(Config) -> ok
%% @end
%%--------------------------------------------------------------------
syntax_fault_in_SiteBasic(Config) ->
    ct:pal("F_2_1 : Syntax fault, SiteBasic.xml. "
	   "Our file is config_initial.netconf"),

    %% Create a syntax fault in config_initial.netconf.
    %% 1. Remove first " character from config_initial.netconf 
    %%    and copy to tmp file.
    SedCmd = "sed 's/[\"]//'",  %% org sed cmd is sed 's/"//'
    create_fault(Config, "config_initial.netconf", SedCmd),

    %% Download
    nl_lib:download_files(Config, console, ?Protocol),
    %% Integrate that shall not be ok.
    nl_lib:httpc_request(Config, post, "Integrate", ?Protocol),

    check_after_integrate_fail_with_decrypt_esi(Config),
    %%check_after_integrate_fail(Config),

    ct:pal("Check failed loading siteBasicFile.netconf.1 exist in erl log."),
    GrepStr = "'aicServer: Failed - loading'",
    ExpectStr = "\"/rcs/networkloader/siteBasicFile.netconf_nc.1",
    check_expected_error_exist(Config, GrepStr, ExpectStr, yes),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec wrong_content_in_SiteBasic(Config) -> ok
%% @end
%%--------------------------------------------------------------------
wrong_content_in_SiteBasic(Config) ->
    ct:pal("F_2_2 : Wrong Content, SiteBasic.xml. "
    	   "Our file is config_initial.netconf"),

    %% 1. Insert a content fault. replace managedElementId to managedElement_Id.
    SedCmd = "sed 's/managedElementId/managedElement_Id/g'",
    create_fault(Config, "config_initial.netconf", SedCmd),

    %% Download
    nl_lib:download_files(Config, console, ?Protocol),
    %% Integrate that shall not be ok.
    nl_lib:httpc_request(Config, post, "Integrate", ?Protocol),

    check_after_integrate_fail_with_decrypt_esi(Config),

    %%check_after_integrate_fail(Config),

    ct:pal("Check failed loading siteBasicFile.netconf.1 exist in erl log."),
    GrepStr = "'aicServer: Failed - loading'",
    ExpectStr = "\"/rcs/networkloader/siteBasicFile.netconf_nc.1",
    check_expected_error_exist(Config, GrepStr, ExpectStr, yes),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec err_site_basic_with_several_session(Config) -> ok
%% @end
%%--------------------------------------------------------------------
err_site_basic_with_several_session(Config) ->
    ct:pal("F_2_3 : Site Basic with several Session. "
	   "Fault in site basic shall result in escalate to NL. "
    	   "Our file is config_initial.netconf"),

    %% sed cmd to add new not valid xml cmds in config_initial.netconf.
    %% In this case just add TrustedCertificate=1 and this is not valid.
    %% To create TrustedCertificate needs a coplete MO path.
    SedAddXml = "sed '/<\\/UserManagement>/a <CertM><certMId>1<\\/certMId><TrustCategory><trustCategoryId>1<\\/trustCategoryId><trustedCertificates>TrustedCertificate=1<\\/trustedCertificates><\\/TrustCategory><\\/CertM>' ",
    
    update_xml_using_two_edit_and_insert_xml(Config, 
					     "config_initial.netconf", 
					     SedAddXml),
    %% Download
    nl_lib:download_files(Config, console, ?Protocol),
    %% Integrate that shall not be ok.
    nl_lib:httpc_request(Config, post, "Integrate", ?Protocol),

    check_after_integrate_fail(Config),
    ct:pal("Check failed loading siteBasicFile.netconf exist in erl log."),
    GrepStr = "'aicServer: Failed - loading'",
    ExpectStr = "\"/rcs/networkloader/siteBasicFile.netconf",
    check_expected_error_exist(Config, GrepStr, ExpectStr, no),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec site_basic_with_several_session(Config) -> ok
%% @end
%%--------------------------------------------------------------------
site_basic_with_several_session(Config) ->
    ct:pal("F_2_X : Site Basic with several Session. "
	   "At the moment when no fault is inserted in site basic, "
	   "this result in ok installation. "
	   "A question to Andreas Toyro regarding this due to before this "
	   "result in rollback to NL when add TrustedCertificate."
    	   "Our file is config_initial.netconf"),

    %% sed cmd to add new validxml cmds in config_initial.netconf.
    SedAddXml = "sed '/<\\/UserManagement>/a <CertM><certMId>1<\\/certMId><TrustCategory><trustCategoryId>1<\\/trustCategoryId><trustedCertificates>ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=1<\\/trustedCertificates><\\/TrustCategory><\\/CertM>' ",
    
    update_xml_using_two_edit_and_insert_xml(Config, 
					     "config_initial.netconf", 
					     SedAddXml),

    %% Download
    aic_httpc:download_files(Config, console),
   
    %% Integrate that shall not be ok.
    nl_lib:httpc_request(Config, post, "Integrate", ?Protocol),

    check_after_integrate_fail(Config),
    ct:log("Check failed loading siteBasicFile.netconf exist in erl log."),
    GrepStr = "'aicServer: Failed - loading'",
    ExpectStr = "\"/rcs/networkloader/siteBasicFile.netconf",
    check_expected_error_exist(Config, GrepStr, ExpectStr, no),
    %% TrustedCertificate is missing
    ct:log("Check, TrustedCertificate is missing, exist in erl log."),
    GrepStr1 = "'Transaction commit failed'",
    ExpectStr1 = "TrustedCertificate=1 is missing",
    check_expected_error_exist(Config, GrepStr1, ExpectStr1, no),
    
    ok.

%%--------------------------------------------------------------------
update_xml_using_two_edit_and_insert_xml(Config, File, SedAddXml) ->
    ct:pal("Update : ~p to edit two times and add new xml cmd",  [File]),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    LogPath = ?config(priv_dir,Config),
    ct:pal("LogPath:~n~p~n",[LogPath]),

    %% 1. Remove sec mo and add rest to a new file. 
    %% Note, sed cmd need \ before / and using er str then an extar \ is needed.
    %% sed '/<SecM/,/<\/SecM>/d' config_initial.netconf > file1
    Cmd1 = "sed '/<SecM/,/<\\/SecM>/d' "++TftpBootDir++File++" > "++
    	LogPath++"file1",
    ct:log("Cmd1:~n~p~n",[Cmd1]),
    os:cmd(Cmd1),

    %% 2. Remove SysM and add rest to a new file. 
    %% sed '/<SysM/,/<\/SysM>/d' config_initial.netconf > file2
    Cmd2 = "sed '/<SysM/,/<\\/SysM>/d' "++TftpBootDir++File++" > "++
    	LogPath++"file2",
    ct:log("Cmd2:~n~p~n",[Cmd2]),
    os:cmd(Cmd2),


    %% 3. Add new xml cmds in file from 2. Outcome is a new file.
    Cmd3 = SedAddXml ++LogPath++"file2"++" > " ++ LogPath++"file3",
    ct:log("Cmd3:~n~p~n",[Cmd3]),
    os:cmd(Cmd3),


    %% 4. Add content from file1 and file3 in a new file, file4.
    %% cat file1 file3 >> file4
    Cmd4 = "cat "++LogPath++"file1"++" "++LogPath++"file3 >> "++
	LogPath++"file4",
    ct:log("Cmd4:~n~p~n",[Cmd4]),
    os:cmd(Cmd4),

    
    %% 5. cp file4 to config_initial.netconf in TftpBootDir
    Cmd5 = "cp "++LogPath++"file4"++" "++TftpBootDir++File,
    ct:log("Cmd5:~n~p~n",[Cmd5]),
    os:cmd(Cmd5),

    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
create_fault(Config, File, SedCmd) ->
    ct:pal("Insert syntax fault in file: ~p ",  [File]),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    %% 1. Create a fault
    CMD1 = SedCmd ++ " " ++
    	TftpBootDir ++ 
    	File ++ " > " ++ 
    	TftpBootDir ++ 
    	"/tmp_file.xml",
    ct:log("CMD:~n~p~n",[CMD1]),
    os:cmd(CMD1),

    %% 2. mv tmp file to RbsSummaryFile.
    CMD2 = "mv " ++ 
    	TftpBootDir ++ 
    	"/tmp_file.xml " ++ 
    	TftpBootDir ++ File,
    ct:log("CMD:~n~p~n",[CMD2]),
    os:cmd(CMD2).

%%--------------------------------------------------------------------
check_after_integrate_fail_with_decrypt_esi(Config) ->
    ct:pal("Wait for rollback to NL due to fault in SiteBasic file"),

    {ok,_} = 
    	ct_telnet:expect(console, 
    			 "Version: CNX901",
    			 [{timeout, 900000}, no_prompt_check]),
       	ct_telnet:expect(console, 
    			 "Build date:",
    			 [{timeout, 10000}, no_prompt_check]),


    ct:pal("Check that it is ok to export esi"),
    nl_lib:generic_export_esi(Config, console, ?Protocol, "", nl ),

    ct:pal("Decrypt ESI"),
    EsiLogPath = ?config(priv_dir,Config),
    decrypt_esi(EsiLogPath),

    %% An generic check.
    GrepStr2 = "'giving up'",
    ExpectStr2 = "Loading failed after 90 seconds and 3 retries - giving up",
    check_expected_error_exist(Config, GrepStr2, ExpectStr2, yes),

    ok.

%%--------------------------------------------------------------------
check_after_integrate_fail(Config) ->
    ct:pal("Wait for rollback to NL due to fault in SiteBasic file"),

    {ok,_} = 
    	ct_telnet:expect(console, 
    			 "Version: CNX901",
    			 [{timeout, 900000}, no_prompt_check]),
       	ct_telnet:expect(console, 
    			 "Build date:",
    			 [{timeout, 10000}, no_prompt_check]),


    ct:pal("Check that it is ok to export esi"),
    nl_lib:generic_export_esi(Config, console, ?Protocol, "", nl ),

    %% An generic check.
    GrepStr2 = "'giving up'",
    ExpectStr2 = "Loading failed after 90 seconds and 3 retries - giving up",
    check_expected_error_exist(Config, GrepStr2, ExpectStr2, no),

    ok.

%%--------------------------------------------------------------------
check_expected_error_exist(Config, GrepStr, ExpectStr, DecryptEsi) ->

    EsiLogPath = ?config(priv_dir,Config),

    case DecryptEsi of
	yes ->
	    continue; 
	no ->
	    os:cmd("tar -xzf "++EsiLogPath++"esi.*"++" -C "++EsiLogPath)
    end,

    Ls = os:cmd("ls "++EsiLogPath++"rcs/erlang/"),
    ct:log("# ls: ~p", [string_token(Ls)]),
    GrepErlLog = os:cmd("grep "++GrepStr++" "++
			    EsiLogPath++"rcs/erlang/erlang.log.*"),
    ct:log("# grep result in erl log: ~p", [GrepErlLog]),
    ct:log("## grep result in erl log: ~p", 
	   [string:tokens(GrepErlLog, "\"\r\n")]),

    case re:run(GrepErlLog, ExpectStr) of
	{match, _} ->
	    ct:log("ExpectStr: ~p exist in erlang log", [ExpectStr]);
	_Other ->
	    ct:fail("TC will fail due to, "
		    "Expected str not found in erlang log.!")
    end.


%%--------------------------------------------------------------------
decrypt_esi(EsiLogPath)->

    Ls = os:cmd("ls "++EsiLogPath),
    ct:log("# decrypt_esi ls: ~p", [string_token(Ls)]),
    ok = file:set_cwd(EsiLogPath),
    EsiLogName = lists:flatten(string:tokens(os:cmd("find esi.* "), "\n")),
    
    case filename:extension(EsiLogName) of
	".gpg" ->
	    ct:pal("EsiLogName: ~p", [EsiLogName]),
	    ct:pal("EsiLogPath: ~p", [EsiLogPath]),
	    TestUser = re:replace(os:cmd("sudo -u rcsci1 cat /home/rcsci1/private/rcsci1.LOG_info"),
				  "\\s+", "", [global,{return,list}]),

	    %%todo check that it is not possible to unpack
	    %%check_esi_log_transfered(EsiLogName, EsiLogPath, false),
	    %%Request the AccesToken
	    Cmd = "curl -X POST -k -H \"username:"++TestUser ++"\" -H \"password:" 
		++ TestUser ++"\""++
		" https://main-csdp.internal.ericsson.com/authorization/token/v2/authorize",
	    AccessToken = find_token(string:tokens(send_curl_command(Cmd), ",")),
	    ct:pal("AccessToken: ~p", [AccessToken]),

	    %%Get the fileName before request the decrypted file
	    EsiFile = filename:join(EsiLogPath, EsiLogName),
	    Cmd2 = "curl -k -F \"file=@" ++ EsiFile ++"\"  -X " ++
		"POST https://main-csdp.internal.ericsson.com/crypto/v1/files?token="
		++AccessToken,
	    R2 = send_curl_command(Cmd2),
	    File = find_file(string:tokens(R2, ",")),
	    ct:pal("File: ~p", [File]),

	    %%Request the decrypted ESI
	    DecryptEsiLogName = EsiLogName++".tgz",
	    DecryptFile = filename:join(EsiLogPath, DecryptEsiLogName),
	    Cmd3 = "curl -k -v https://main-csdp.internal.ericsson.com/crypto/v1/files/" ++
		File ++ "?token=" ++ AccessToken ++ " -o" ++ DecryptFile,
	    send_curl_command(Cmd3),
	    %%Check if esi is unpacked and unpack
	    unpack_esi(DecryptEsiLogName, EsiLogPath),
	    check_esi_unpacked(EsiLogPath, "rcs");
	%% R3 should be the decrypted contents of the uploaded file.
	_Else ->
	    ct:fail("The ESI is not ecrypted: ~p", [EsiLogName])
    end,
    ok.


unpack_esi(EsiLogName, EsiLogPath)->

    ct:pal("EsiLogName: ~p", [EsiLogName]),
    ct:pal("EsiLogPath: ~p", [EsiLogPath]),

    Ls = os:cmd("ls "++EsiLogPath),
    ct:log("# unpack_esi ls: ~p", [string_token(Ls)]),
    ct:pal("## Esi log file: ~p, exist in \n path : ~p \n",
	   [EsiLogName, EsiLogPath]),
    timer:sleep(100),

    os:cmd("tar -xf " ++ EsiLogPath ++ EsiLogName ++" --gunzip --directory=" ++ EsiLogPath),
    format_html("<a href=\"~s\">~s</a>", 
		[EsiLogPath, "Esi unpacked dir"]),
    format_html("<a href=\"~s\">~s</a>", 
		[filename:join(EsiLogPath,EsiLogName), EsiLogName]),
    ok.


check_esi_unpacked(EsiLogPath, PathToCheck)->
    DirToCheck =  filename:join(EsiLogPath, PathToCheck),
    case string:str(os:cmd("ls " ++ DirToCheck), "No such file or directory") of
	0 -> ok;
	_Reply -> 
	    ct:fail("Could not find ~p in dir ~p",[PathToCheck, EsiLogPath])
    end.

send_curl_command(Cmd)->
   %% ct:log("~s",[Cmd]),
    Response = os:cmd(Cmd),
    ct:log(re:replace(Response,"<","\\&lt;",[{return,list},global])),
    Response.

find_token([]) ->
    ok;
find_token([ResultList | RestList]) ->
    case string:str(ResultList, "access_token\":") of
	0 ->
	    find_token(RestList);  
	_ ->
	    %% The string contains the access_token
	    %% Remove unwanted caracters
	    find_value("access_token", string:tokens(ResultList, "\""))
    end.

find_file([First | Rest]) ->
    case string:str(First, "fileName\":") of
	0 ->
	    find_file(Rest);
	_ ->
	    find_value("fileName", string:tokens(First, "\""))
    end.


find_value(ValueType, []) ->
    ct:fail("Did not found the ~p value", [ValueType]);
find_value(ValueType, [ValueType, _, Value | _Rest]) ->
    Value;
find_value(ValueType, [_OtherValueType | RestList]) ->
    find_value(ValueType, RestList).


format_html(String,Args) ->
     ct:log(default, 1, String, Args, [no_css]).

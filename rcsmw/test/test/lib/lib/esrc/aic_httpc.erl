%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aic_httpc.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R11A/1
%%%
%%% @doc == ai support lib using httpc. ==
%%% <br/>
%%%
%%%
%%% @end

-module(aic_httpc).
-include_lib("common_test/include/ct.hrl").

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
%%% R3A/2      2015-05-29 etxivri     created
%%% R4A/1      2015-07-02 etxivri     Update check in site_config_complete.
%%% R4A/2      2015-07-02 etxivri     Minor bug fix.
%%% R4A/3      2015-10-22 etxmlar     Updated request_download_pogress to
%%%                                   handle no connection when nl upgrade
%%% R5A/1      2015-11-11 etxivri     Update export esi and ai log for cluster.
%%% R5A/2      2016-05-25 etxmlar     Added new functions for AI testcases.
%%% R6A/1      2016-07-06 etxmlar     Update download_files_lmt_integration
%%%                                   to be more generic
%%% R6A/2      2016-08-26 etxmlar     Increased max timeout in 
%%%                                   wait_for_download_completed because
%%%                                   Full UP download takes longer time
%%%                                   (coutains more CXP:s)
%%% R7A/1      2016-10-04 etxivri     Add nl_upgrade  
%%% R7A/2      2016-11-22 etxmlar     Increased max timeout in 
%%%                                   wait_for_download_completed     
%%% R11A/1     2017-10-09 etxivri     Update httpc request for OTP20
%%% ------------------------------------------------------------
-export([check_if_vc_board/0,
	 send_httpc_request/4,
	 send_httpc_request_no_check/4,
	 board_restore/2,
	 board_restore/3,
	 factory_reset/2,
	 factory_reset/3,
	 hard_factory_reset/2,
	 hard_factory_reset/3,
	 download_files/1,
	 download_files/2,
	 download_files/3,
	 nl_upgrade/1,
	 nl_upgrade/2,
	 nl_upgrade/3,
	 download_files_lmt_integration/3,
	 download_files_lmt_on_site_conf_or_zt_off_site_pre_conf/4,
	 integrate/3,
	 integrate_lmt_integration/3,
	 integrate_lmt_int_on_site_conf/3,
	 export_ai_log/1,
	 export_ai_log/3,
	 export_ai_log/4,
	 export_esi/1,
	 export_esi/2,
	 export_esi/3,
	 export_esi/4,
	 cancel/2,
	 export_esi_check_nl_state/2,
	 wait_for_download_completed/2,
	 wait_for_download_completed/3,
	 request_download_pogress/1,
	 request_download_pogress/2,
	 request_download_pogress/3,
	 wait_for_netconf_started/1,
	 wait_for_netconf_started/2
	]).

-define(DefaultProt, "https").
-define(HttpPort, "8080").
-define(DefaultXmlFile, "RbsSummaryFile.xml").
-define(FileName, "fakeEsi").

%%--------------------------------------------------------------------
%% @doc
%% Return "true" or "false"
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
send_httpc_request(Method, Url, Body, Timeout) ->
    ct:log("# Method: ~p", [Method]),
    ct:log("# Url: ~p", [Url]),
    ct:log("# Body: ~p", [Body]),
    ct:log("# Timeout: ~p", [Timeout]),
    case httpc:request(Method, {Url, [], [], Body}, [{timeout, Timeout},{ssl,[{server_name_indication,"test_vc"}]}], []) of 
	{ok, Reply} ->
    	    ct:log("Reply : ~p", [Reply]),
	    ok;
    	_A -> 
    	    ct:log("Ans from httpc request not expected, try again. ~n "
    		   "Answ: ~p ", [_A]),
    	    case httpc:request(Method, {Url, [], [], Body}, 
			       [{timeout, Timeout}, {ssl,[{server_name_indication,"test_vc"}]}], []) of
    		{ok, Reply} ->
    		    ct:log("Reply second time: ~p", [Reply]),
    		    ok;
    		_B -> 
    		    ct:log("Ans from second httpc request not expected, "
    			   "try again last time. ~n "
    			   "Answ: ~p ", [_B]),
    		    {ok, Reply} = httpc:request(Method, {Url, [], [], Body}, 
    						[{timeout, Timeout}, {ssl,[{server_name_indication,"test_vc"}]}], [])
    	    end
    end,
    Reply.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
send_httpc_request_no_check(Method, Url, Body, Timeout) ->
    ct:log("## Method: ~p", [Method]),
    ct:log("## Url: ~p", [Url]),
    ct:log("## Body: ~p", [Body]),
    ct:log("## Timeout: ~p", [Timeout]),
    case httpc:request(Method, {Url, [], [], Body}, [{timeout, Timeout}, {ssl,[{server_name_indication,"test_vc"}]}],[]) of 
	{ok, Reply} ->
    	    ct:log("Reply : ~p", [Reply]),
	    ok;
    	_A -> 
    	    ct:log("Ans from httpc request not expected, try again. ~n "
    		   "Answ: ~p ", [_A]),
    	    case httpc:request(Method, {Url, [], [], Body}, 
			       [{timeout, Timeout}, {ssl,[{server_name_indication,"test_vc"}]}], []) of
    		{ok, Reply} ->
    		    ct:log("Reply second time: ~p", [Reply]),
    		    ok;
    		_B -> 
    		    ct:log("Ans from second httpc request not expected, "
    			   "try again last time. ~n "
    			   "Answ: ~p ", [_B]),
    		    {_, Reply} = httpc:request(Method, {Url, [], [], Body}, 
    						[{timeout, Timeout}, {ssl,[{server_name_indication,"test_vc"}]}], [])
    	    end
    end,
    Reply.

	    
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

%%--------------------------------------------------------------------
%% @doc
%% @spec board_restore(Config, Console) -> ok
%% @end
%%--------------------------------------------------------------------
board_restore(Config, Console) ->
    board_restore(Config, Console, no_nc_check).
board_restore(Config, Console, NC) ->
    ct:pal("board restore"),
    check_or_no_check_nc(NC),
    
    IP = get_node_ip(),
    Method = post,
    Body = "DoBoardRestore=BoardRestore",
    
    _Prot = generic_send_request_aic_gui(Config, 
					 Method, 
					 "DoBoardRestore", 
					 IP, 
					 Body, 
					 600000),

    ct_telnet:expect(Console,
		     "Ericsson Version:", 
		     [{idle_timeout,120000},no_prompt_check]),

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
		     [{timeout,30000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% @spec factory_reset(Config, Console) -> ok
%% @end
%%--------------------------------------------------------------------
factory_reset(Config, Console) ->
    factory_reset(Config, Console, no_nc_check).
factory_reset(Config, Console, NC) ->
    ct:pal("factory reset"),
    check_or_no_check_nc(NC),

    IP = get_node_ip(),
    Method = post,
    Body = "DoFactoryReset=FactoryReset",
    
    _Prot = generic_send_request_aic_gui(Config, 
					 Method, 
					 "DoFactoryReset", 
					 IP, 
					 Body, 
					 600000),

    ct_telnet:expect(Console,
		     "Ericsson Version:", 
		     [{idle_timeout,120000},no_prompt_check]),

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
		     [{timeout,30000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% @spec hard_factory_reset(Config, Console) -> ok
%% @end
%%--------------------------------------------------------------------
hard_factory_reset(Config, Console) ->
    hard_factory_reset(Config, Console, no_nc_check).
hard_factory_reset(Config, Console, NC) ->
    ct:pal("hard factory reset"),
    check_or_no_check_nc(NC),

    IP = get_node_ip(),
    Method = post,
    Body = "DoHardFactoryReset=HardFactoryReset",
    
    _Prot = generic_send_request_aic_gui(Config, 
					 Method, 
					 "DoHardFactoryReset", 
					 IP, 
					 Body, 
					 600000),

    ct_telnet:expect(Console,
		     "Ericsson Version:", 
		     [{idle_timeout,120000},no_prompt_check]),

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
		     [{timeout,30000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
generic_send_request_aic_gui(_Config, Method, DoAction, IP, Body, Timeout) ->
    Url_https = "https://"++IP++"/cgi-bin/aicGui:post",
    ct:pal("Url https: ~p", [Url_https]),
    ct:pal("Body: ~p", [Body]),

    ct:pal("First try using https.", []),
    timer:sleep(5000),
    start_needed_applications("https"), %% works also for http.
    timer:sleep(5000),
    Reply = send_httpc_request_no_check(Method, Url_https, Body, Timeout),

    Prot = case Reply of
    	{{_,200,"OK"}, _ReplyA, _ReplyB} ->
    	    ct:pal("Reply from ~p using https is ok.", [DoAction]),
    	    "https";
    	_Other ->
    	    ct:log("_Other : ~p", [_Other]),
	    ct:pal("Action failed. Try old way using http and Port 8080.", []),
	    Url_http = "http://"++IP++":"++?HttpPort++"/cgi-bin/aicGui:post",
	    ct:pal("Url http: ~p", [Url_http]),
		   timer:sleep(5000),
		   case send_httpc_request(Method, Url_http, Body, Timeout) of 
		       {{_,200,"OK"}, _, _}  ->
			   ct:pal("Reply from ~p using http with port 8080 "
				  "is ok.", [DoAction]),
			   "http";
		       _Fail ->
			   ct:log("_Fail : ~p", [_Fail ]),
			   ct:fail("Action fail using https or http!")
		   end
	   end,
    timer:sleep(5000),
    stop_needed_applications("https"),	
    timer:sleep(20000),

    Prot.

%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec download_files(Config) -> ok
%% @end
%%--------------------------------------------------------------------
download_files(Config) ->
    download_files(Config, console, "RbsSummaryFile.xml").
download_files(Config, Console) ->
    download_files(Config, Console, "RbsSummaryFile.xml").
download_files(Config, Console, XmlFile) ->
    ct:pal("download files"),

    HW = get_hw(),
    IP = get_node_ip(),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    MyTime = integer_to_list(ms_since_1970()),

    Method = post,
    Body = "host="++SftpHost++"&"++
	"username="++Username++"&"++
	"password="++Password++"&"++
	"filename=boards/"++HW++"/"++XmlFile++"&"++
	"utcMs="++MyTime++"&"++
	"DoDownload=Download",

    Prot = generic_send_request_nl_gui(Config, 
					Method, 
					"DoDownload", 
					IP, 
					Body, 
				       600000),
    
    case Prot of
	"https" ->
	    wait_for_download_completed(Config, Prot);
	"http" ->
	    case ct_telnet:expect(Console, "The board can be powered off", 
    				  [{timeout,180000},no_prompt_check]) of
    		{ok, _} ->
    		    ok;
    		_ -> % login: prompt can be corrupted on dus2
		    ct:pal("old http is used. "
			   "sleep 2min more to ensure download copleted."),
		    timer:sleep(120000)
	    end
    end,

    ok.
%%--------------------------------------------------------------------
%% @doc
%% nl upgrade. using RbsSummaryFile.xml.nl
%% @spec nl_upgrade(Config) -> ok
%% @end
%%--------------------------------------------------------------------
nl_upgrade(Config) ->
    nl_upgrade(Config, console, "RbsSummaryFile.xml.nl").
nl_upgrade(Config, Console) ->
    nl_upgrade(Config, Console, "RbsSummaryFile.xml.nl").
nl_upgrade(Config, Console, XmlFile) ->
    ct:pal("download files"),

    HW = get_hw(),
    IP = get_node_ip(),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    MyTime = integer_to_list(ms_since_1970()),

    Method = post,
    Body = "host="++SftpHost++"&"++
	"username="++Username++"&"++
	"password="++Password++"&"++
	"filename=boards/"++HW++"/"++XmlFile++"&"++
	"utcMs="++MyTime++"&"++
	"DoDownload=Download",

    _Prot = generic_send_request_nl_gui(Config, 
					Method, 
					"Download", 
					IP, 
					Body, 
				       600000),
    ct:pal("Wait for board restarts, due to nl upgrade"),
    {ok, _} = ct_telnet:expect(Console, "Ericsson Version: ", 
		     [{timeout,180000},no_prompt_check]),
    ct:pal("Wait for node to start on new NL"),
    {ok, _} = ct_telnet:expect(Console, "Network Loader Ready:", 
		     [{timeout,180000},no_prompt_check]),
    timer:sleep(30000),
    ct:pal("Just to print info."),
    ct_telnet:send(console, "cat /nl/log/nl_log.1"),
    ct_telnet:expect(Console, "Running version:", 
		     [{timeout,30000},no_prompt_check]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec download_files_lmt_on_site_conf_or_zt_off_site_pre_conf(Config, Console, SifFile, Loaclfile) -> ok
%% @end
%%--------------------------------------------------------------------
download_files_lmt_on_site_conf_or_zt_off_site_pre_conf(Config, Console, SifFile, Loaclfile) ->

    {SifcontentInput, LocalFileInput} =  
	case check_if_local_file(Loaclfile) of
	    true ->
		ct:log("local file used"),

		TftpBootDir = proplists:get_value(tftpboot_dir, Config),
		Sif_path = filename:join(TftpBootDir, SifFile),

		{ok, Binary} = file:read_file(Sif_path),
		SifContentStr = binary_to_list(Binary),

		SifContentHexEncodedURI = http_uri:encode(SifContentStr),
		ct:log("SifContentHexEncodedURI: ~p", [SifContentHexEncodedURI]),

		{SifContentHexEncodedURI, SifFile}; 
	    false ->
		ct:log("No local file used"),
		{"&","&"}
	end,

    HW = get_hw(),
    IP = get_node_ip(),

    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),

    MyTime = integer_to_list(ms_since_1970()),
    Method = post,

    Body = 
	"sifcontent="++SifcontentInput++"&"++
	"host="++SftpHost++"&"++
	"filename=boards/"++HW++"/"++SifFile++"&"++
	"username="++Username++"&"++
	"password="++Password++"&"++
	"localfile="++LocalFileInput++"&"++
	"utcMs="++MyTime++"&"++
	"DoDownload=Download",

    Prot = generic_send_request_nl_gui(Config, 
				       Method, 
				       "DoDownload", 
				       IP, 
				       Body, 
				       600000),

    case Prot of
	"https" ->
	    wait_for_download_completed(Config, Prot);
	"http" ->
	    case ct_telnet:expect(Console, "The board can be powered off", 
    				  [{timeout,180000},no_prompt_check]) of
    		{ok, _} ->
    		    ok;
    		_ -> % login: prompt can be corrupted on dus2
		    ct:pal("old http is used. "
			   "sleep 2min more to ensure download copleted."),
		    timer:sleep(120000)
	    end
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec  download_files_lmt_integration(Config, Console, XmlFile) -> ok
%% @end
%%--------------------------------------------------------------------
download_files_lmt_integration(Config, Console, XmlFile) ->
    
    %%HW = get_hw(),
    IP = get_node_ip(),

    %% [{host, SftpHost},{username, Username},{password, Password}] = 
    %% 	ct:get_config(sftp_server),

    %% Temporary, get from stp.cfg "when new labb"
    {SmrsUsername,SmrsPassword} =
	case XmlFile of
	    "SiteInstallationFileUntrustedIpv4.xml" ->
		{"dustest","dustest"};
	    _Else->
		{"dustest","AAssff222!"}
	end,

    %% Temporary, get from stp.cfg when "new labb"
    %% SmrsUsername = "dustest",    
    %% SmrsPassword = "dustest",
    %% SmrsPassword = "AAssff222!",  

    MyTime = integer_to_list(ms_since_1970()),
    Method = post,

    %%SIF_file =  proplists:get_value(sif, Config),
    Priv_dir = ?config(priv_dir, Config),
    Sif_path = filename:join(Priv_dir, XmlFile),

    {ok, Binary} = file:read_file(Sif_path),
    SifContentStr = binary_to_list(Binary),

    SifContentHexEncodedURI = http_uri:encode(SifContentStr),
    ct:log("SifContentHexEncodedURI: ~p", [SifContentHexEncodedURI]),
  

    Body = 
	"sifcontent="++SifContentHexEncodedURI++"&"++
	"filename="++"&"++
	"smrsusername="++SmrsUsername++"&"++
	"smrspassword="++SmrsPassword++"&"++
	"localfile="++XmlFile++"&"++
	"utcMs="++MyTime++"&"++
	"DoDownload=Download",

    Prot = generic_send_request_nl_gui(Config, 
				       Method, 
				       "DoDownload", 
				       IP, 
				       Body, 
				       600000),
    
    case Prot of
	"https" ->
	    wait_for_download_completed(Config, Prot);
	"http" ->
	    case ct_telnet:expect(Console, "The board can be powered off", 
    				  [{timeout,180000},no_prompt_check]) of
    		{ok, _} ->
    		    ok;
    		_ -> % login: prompt can be corrupted on dus2
		    ct:pal("old http is used. "
			   "sleep 2min more to ensure download copleted."),
		    timer:sleep(120000)
	    end
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec integrate(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
integrate(Config, Console, NC) ->
    ct:pal("integrate"),

    IP = get_node_ip(),
    MyTime = integer_to_list(ms_since_1970()),

    Method = post,
    Body = 
	"host="++"&"++
    	"username="++"&"++
    	"password="++"&"++
    	"filename="++"&"++
    	"utcMs="++MyTime++"&"++
    	"DoIntegrate=Integrate",

    _Prot = generic_send_request_nl_gui(Config, 
    					Method, 
    					"DoIntegrate", 
    					IP, 
    					Body, 
    					600000),
    
    case check_if_vc_board() of
    	"yes" -> 
    	    ok;
    	_Other ->
    	    case ct_telnet:expect(Console, "login:", 
    				  [{timeout,120000},no_prompt_check]) of
    		{ok, _} ->
    		    ok;
    		_ -> % login: prompt can be corrupted on dus2
    		    ok = ct_telnet:send(Console, ""),
    		    {ok, _} = 
    			ct_telnet:expect(Console, 
    					 "login:", 
    					 [{timeout,20000},no_prompt_check])
    	    end,
    	    ct:pal("Rcvd : login prompt.",[])
    end,

    wait_for_netconf_started(NC, 600000),
    site_config_complete(NC),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec integrate_lmt_integration(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
integrate_lmt_integration(Config, Console, NC) ->
    ct:pal("Integrate lmt integration"),

    IP = get_node_ip(),
    MyTime = integer_to_list(ms_since_1970()),

    Method = post,

    Body = 
	"sifcontent="++"&"++
	"filename="++"&"++
    	"smrsusername="++"&"++
    	"smrspassword="++"&"++
    	"localfile="++"&"++
    	"utcMs="++MyTime++"&"++
    	"DoIntegrate=Integrate",

    _Prot = generic_send_request_nl_gui(Config, 
    					Method, 
    					"DoIntegrate", 
    					IP, 
    					Body, 
    					600000),

    case check_if_vc_board() of
    	"yes" -> 
    	    ok;
    	_Other ->
    	    case ct_telnet:expect(Console, "login:", 
    				  [{timeout,120000},no_prompt_check]) of
    		{ok, _} ->
    		    ok;
    		_ -> % login: prompt can be corrupted on dus2
    		    ok = ct_telnet:send(Console, ""),
    		    {ok, _} = 
    			ct_telnet:expect(Console, 
    					 "login:", 
    					 [{timeout,20000},no_prompt_check])
    	    end,
    	    ct:pal("Rcvd : login prompt.",[])
    end,

    wait_for_netconf_started(NC, 600000),
    site_config_complete(NC),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec integrate_lmt_int_on_site_conf(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
integrate_lmt_int_on_site_conf(Config, Console, NC) ->
    ct:pal("Integrate lmt integration"),

    IP = get_node_ip(),
    MyTime = integer_to_list(ms_since_1970()),

    Method = post,

    Body = 
	"host="++"&"++
	"sifcontent="++"&"++
    	"username="++"&"++
    	"password="++"&"++
	"filename="++"&"++
    	"localfile="++"&"++
    	"utcMs="++MyTime++"&"++
    	"DoIntegrate=Integrate",

    _Prot = generic_send_request_nl_gui(Config, 
    					Method, 
    					"DoIntegrate", 
    					IP, 
    					Body, 
    					600000),
    
    case check_if_vc_board() of
    	"yes" -> 
    	    ok;
    	_Other ->
    	    case ct_telnet:expect(Console, "login:", 
    				  [{timeout,120000},no_prompt_check]) of
    		{ok, _} ->
    		    ok;
    		_ -> % login: prompt can be corrupted on dus2
    		    ok = ct_telnet:send(Console, ""),
    		    {ok, _} = 
    			ct_telnet:expect(Console, 
    					 "login:", 
    					 [{timeout,20000},no_prompt_check])
    	    end,
    	    ct:pal("Rcvd : login prompt.",[])
    end,

    wait_for_netconf_started(NC, 600000),
    site_config_complete(NC),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec export_ai_log(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_ai_log(Config) ->
    EsiLogPath = ?config(priv_dir, Config),
    IP = get_node_ip(),
    os:cmd("chmod 777 "++ EsiLogPath),
    export_ai_log(Config, ?FileName, EsiLogPath, IP).

export_ai_log(Config, FileName, IP) ->
    EsiLogPath = ?config(priv_dir, Config),
    os:cmd("chmod 777 "++ EsiLogPath),
    export_ai_log(Config, FileName, EsiLogPath, IP).

export_ai_log(Config, FileName, EsiLogPath, IP) ->
    ct:pal("export ai log, IP: ~p", [IP]),

    %% IP = get_node_ip(),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),

    Method = post,
    Body = "host="++SftpHost++"&"++
	"username="++Username++"&"++
	"password="++Password++"&"++
	"filename="++EsiLogPath++FileName++"&"++
	"DoExport=Export",

    _Prot = generic_send_request_nl_gui(Config, 
					 Method, 
					 "DoExport", 
					 IP, 
					 Body, 
					 600000),

    timer:sleep(5000),
    ct:log("EsiLogPath : ~p", [EsiLogPath]),
    ct:log("FileName: ~p", [FileName]),
    wait_log_exist_in_log_path(FileName, EsiLogPath),

    create_fake_esi_dir(FileName ++ "_ericsson", EsiLogPath),
    create_fake_esi_dir(FileName ++ "_prev_ericsson", EsiLogPath),
  
    %% io:format("<a href=\"~s\">~s</a>",
    %% 	      [EsiLogPath, "Fake Esi dir"]),

    ct:log(default, 1, "<a href=\"~s\">~s</a>",
	   [EsiLogPath, "Fake Esi dir"], [no_css]),

    ct:pal("log esi : ~p, exist in ~n path: ~n~p ", [FileName, EsiLogPath]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check NL state before export. NL state can be in : no_nl | nl
%% @spec export_esi_check_nl_state(Config, Console) -> ok
%% @end
%%--------------------------------------------------------------------
export_esi_check_nl_state(Config, Console) ->
    ct:pal("export ESI, check nl state before export."),
    EsiLogPath = ?config(priv_dir, Config),
    os:cmd("chmod 777 "++ EsiLogPath),
    NL_State = check_node_is_in_nl(Console), %% no_nl | nl
    export_esi(Config, EsiLogPath, NL_State).

%%--------------------------------------------------------------------
%% @doc
%% @spec export_esi(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_esi(Config) ->
    EsiLogPath = ?config(priv_dir, Config),
    %% ct:pal("esi log path: ~p", [EsiLogPath]),
    os:cmd("chmod 777 "++ EsiLogPath),
    export_esi(Config, EsiLogPath, no_nl).

export_esi(Config, IP) ->
    EsiLogPath = ?config(priv_dir, Config),
    ct:log("esi log path: ~p", [EsiLogPath]),
    os:cmd("chmod 777 "++ EsiLogPath),
    export_esi(Config, EsiLogPath, no_nl, IP).

export_esi(Config, EsiLogPath, NL_State) ->
    IP = get_node_ip(),
    export_esi(Config, EsiLogPath, NL_State, IP).

export_esi(Config, EsiLogPath, NL_State, IP) ->
    ct:pal("export ESI, IP: ~p", [IP]),

    %% IP = get_node_ip(),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),

    Method = post,
    Body = "host="++SftpHost++"&"++
	"username="++Username++"&"++
	"password="++Password++"&"++
	"directory="++EsiLogPath++"&"++
	"DoESI=ESI",

    case NL_State of
	nl -> %% Board is in NL state.
	    generic_send_request_nl_gui(Config, 
					Method, 
					"DoEsi", 
					IP, 
					Body, 
					600000);
	_Else -> %% Board is up and running.
	    generic_send_request_aic_gui(Config, 
					 Method, 
					 "DoEsi", 
					 IP, 
					 Body, 
					 600000)
    end,

    timer:sleep(5000),
    ct:log("EsiLogPath : ~p", [EsiLogPath]),
    wait_log_exist_in_log_path(EsiLogPath),

    ct:pal("esi exist in ~n path: ~n~p ", [ EsiLogPath]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec cancel(Config, Console) -> ok
%% @end
%%--------------------------------------------------------------------
cancel(Config, Console) ->
    ct:pal("cancel"),

    IP = get_node_ip(),
    Method = post,
    Body = "DoCancel=Cancel",
    _Prot = generic_send_request_nl_gui(Config, 
					Method, 
					"DoCancel", 
					IP, 
					Body, 
					600000),
    
    {ok, _} = ct_telnet:expect(Console,
			       "Network Loader Ready:", 
			       [{timeout,300000},no_prompt_check]),
    
    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout,30000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
generic_send_request_nl_gui(_Config, Method, DoAction, IP, Body, Timeout) ->
    Url_https = "https://"++IP++"/cgi-bin/nl_gui:post",
    ct:pal("Url https: ~p", [Url_https]),
    ct:pal("Body: ~p", [Body]),

    ct:pal("First try using https.", []),
    timer:sleep(5000),
    start_needed_applications("https"), %% works also for http.
    timer:sleep(5000),
    Reply = send_httpc_request_no_check(Method, Url_https, Body, Timeout),

    Prot = case Reply of
    	{{_,200,"OK"}, _ReplyA, _ReplyB} ->
    	    ct:pal("Reply from ~p using https is ok.", [DoAction]),
    	    "https";
    	_Other ->
    	    ct:log("_Other : ~p", [_Other]),
	    ct:pal("Action failed. Try old way using http and Port 8080.", []),
	    Url_http = "http://"++IP++":"++?HttpPort++"/cgi-bin/nl_gui:post",
	    ct:pal("Url http: ~p", [Url_http]),
		   timer:sleep(5000),
		   case send_httpc_request(Method, Url_http, Body, Timeout) of 
		       {{_,200,"OK"}, _, _}  ->
			   ct:pal("Reply from ~p using http with port 8080 "
				  "is ok.", [DoAction]),
			   "http";
		       _Fail ->
			   ct:log("_Fail : ~p", [_Fail ]),
			   ct:fail("Action fail using https or http!")
		   end
	   end,
    timer:sleep(5000),
    stop_needed_applications("https"),	
    timer:sleep(20000),
    Prot.

%%--------------------------------------------------------------------
%% @doc
%% @spec wait_for_download_completed(Config, Protocol) -> ok
%% @end
%%--------------------------------------------------------------------
wait_for_download_completed(Config, Protocol) ->
    wait_for_download_completed(Config, Protocol, dummy).
wait_for_download_completed(Config, Protocol, Port) ->
    ct:pal("### wait_for_download_completed",[]),
    wait_for_download_completed(Config, Protocol, Port, 600000). %% 10 minutes 


wait_for_download_completed(_Config,_Protocol,_Port,Timeout) when Timeout < 0 ->
    ct:fail("wait_for_download_completed not rcvd within max timeout.");

wait_for_download_completed(Config, Protocol, Port, Timeout) ->
    case request_download_pogress(Config, Protocol, Port) of
	{_, _, "DownloadReady"} ->
	    ct:pal("Download Completed", []);
	_Other ->
	    ct:log("Download not done yet,sleep and check again.~n~p",[_Other]),
	    timer:sleep(20000),
	    wait_for_download_completed(Config, Protocol, Port, Timeout-20000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec request_download_pogress(Config) -> ok
%% @end
%%--------------------------------------------------------------------
request_download_pogress(Config) ->
    request_download_pogress(Config, ?DefaultProt, ?HttpPort).
request_download_pogress(Config, Protocol) ->
    request_download_pogress(Config, Protocol, dummy).
request_download_pogress(Config, Protocol, Port) ->
    ct:pal("request_download_pogress"),

    ct:pal("Protocol: ~p , Port: ~p", [Protocol, Port]),
    ct:log("Config: ~p", [Config]),
    IP = get_node_ip(),
    
    start_needed_applications(Protocol),

    {Answere, Reply} = 
	case Protocol of
	    "http" ->
	        URL = "http://"++IP++":"++Port++"/progress.txt",
		%% case httpc:request("http://"++IP++":"++Port++"/progress.txt") of
                case httpc:request(get,{URL,[]},[{ssl,[{server_name_indication,"test_vc"}]}],[]) of
		    {ok, R} ->
			{ok, R}; 
		    {error, R} ->
			{error, R}
		end;
	    "https" ->
	        URL = "https://"++IP++"/progress.txt",
		%% case httpc:request("https://"++IP++"/progress.txt") of
                case httpc:request(get,{URL,[]},[{ssl,[{server_name_indication,"test_vc"}]}],[]) of
		    {ok, R} ->
			{ok, R}; 
		    {error, R} ->
			{error, R}
		end;
	    _Other ->
		ct:fail("TC will fail due to unknown protocol."),
		{dummy, dummy}
	end,

    stop_needed_applications(Protocol),	

    ct:log("Answer from download progress: ~n~p~n~p",[Answere, Reply]),
    Reply.


%%% Internal
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
site_config_complete(NC) ->
    ct:log("Wait for netconf rbsConfigLevel: SITE_CONFIG_COMPLETE"),
    site_config_complete(NC, 180000).
site_config_complete(_NC, Timeout) when Timeout < 0 ->
    ct:fail("Could not get SITE_CONFIG_COMPLETE with netconf "
	    "within expected time");
%% site_config_complete(NC, Timeout) ->
    %% open_netconf(NC),
    %% 	    {ok,[{'ManagedElement',
    %% 		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 		  [{managedElementId,[],["1"]},
    %% 		   {'NodeSupport',
    %% 		    [{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
    %% 		    [{nodeSupportId,[],["1"]},
    %% 		     {'AutoProvisioning',
    %% 		      [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
    %% 		      [{autoProvisioningId,[],["1"]},
    %% 		       {rbsConfigLevel,[],[SITE_CONFIG_COMPLETE]}]}]}]}]} =
    %% 		ct_netconfc:get(NC,{'ManagedElement',
    %% 				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 				     [{managedElementId,[],["1"]},
    %% 				      {'NodeSupport',[],
    %% 				       [{nodeSupportId,[],["1"]},
    %% 					{'AutoProvisioning',[],
    %% 					 [{autoProvisioningId,[],["1"]}]}]}]}),
    %% 	    case SITE_CONFIG_COMPLETE of
    %% 		"SITE_CONFIG_COMPLETE" ->
    %% 		    ct:log("rbsConfigLevel: SITE_CONFIG_COMPLETE"),
    %% 		    ok = ct_netconfc:close_session(NC);
    %% 		_ ->
    %% 		    ct_netconfc:close_session(NC),
    %% 		    ct:log(yellow,"Wrong rbsConfigLevel: ~s, "
    %% 			   "Retrying in 5 seconds",[SITE_CONFIG_COMPLETE]),
    %% 		    timer:sleep(5000),
    %% 		    site_config_complete(NC, Timeout-5000)
    %% end.

site_config_complete(NC, Timeout) ->
    open_netconf(NC),
    case ct_netconfc:get(NC,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'NodeSupport',[],
				       [{nodeSupportId,[],["1"]},
					{'AutoProvisioning',[],
					 [{autoProvisioningId,[],["1"]}]}]}]}) of
	{ok,[{'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'NodeSupport',
		[{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
		[{nodeSupportId,[],["1"]},
		 {'AutoProvisioning',
		  [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
		  [{autoProvisioningId,[],["1"]},
		   {rbsConfigLevel,[],[SITE_CONFIG_COMPLETE]}]}]}]}]} ->
	    case SITE_CONFIG_COMPLETE of
		"SITE_CONFIG_COMPLETE" ->
		    ct:log("rbsConfigLevel: SITE_CONFIG_COMPLETE"),
		    ok = ct_netconfc:close_session(NC);
		_ ->
		    ct_netconfc:close_session(NC),
		    ct:log(yellow,"Wrong rbsConfigLevel: ~s, "
			   "sleep 10 sec and Try again.",[SITE_CONFIG_COMPLETE]),
		    timer:sleep(10000),
		    site_config_complete(NC, Timeout-10000)
	    end;
	_Other ->
	    ct_netconfc:close_session(NC),
	    ct:log("Unexpected answer rcvd,sleep 10 sec and Try again.",[]),
	    ct:log("Unexpected Answ: ~p", [_Other]),
	    timer:sleep(10000),
	    site_config_complete(NC, Timeout-10000)
    end.

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
get_hw() ->
    HW = atom_to_list(ct:get_config({test_nodes,1})),
    ct:pal("HW : ~p", [HW]),
    HW.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
open_netconf(NC) ->
    open_netconf(NC, []).
open_netconf(NC, Param) ->
    Answ = case check_if_vc_board()  of
	       "yes" ->
		   ct_netconfc:open(NC, [{user, "SysAdminTest"}, 
					 {password, "SysAdminTest"} | Param]);
	       _Oter ->  
		   ct_netconfc:open(NC, Param)
	   end,
    Answ.

%%--------------------------------------------------------------------
%% @doc
%% wait_for_netconf_started(netconf session)
%% @end
%%--------------------------------------------------------------------
wait_for_netconf_started(NC) ->
    ct:pal("### Check Netconf",[]),
    wait_for_netconf_started(NC, 300000).
wait_for_netconf_started(_NC, Timeout) when Timeout < 0 ->
    ct:fail("Netconf not started within max timeout.");
wait_for_netconf_started(NC, Timeout) ->
    case open_netconf(NC, []) of
	{ok,_} ->
	    ct:log("Netconf up. Close session for other use."),
	    ok = ct_netconfc:close_session(NC),
	    ok;
	Res  ->
	    ct:log("Res: ~p", [Res]),
	    ct_netconfc:close_session(NC),
	    timer:sleep(5000),
	    wait_for_netconf_started(NC, Timeout - 5000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_or_no_check_nc(NC) ->
    case NC of
	no_nc_check ->
	    ct:log("# No check of netconf is ok to be used"),
	    ok;
	_Other ->
	    wait_for_netconf_started(NC, 60000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
wait_log_exist_in_log_path(EsiLogPath) ->
    wait_log_exist_in_log_path("esi[.]", EsiLogPath, 6000000).
wait_log_exist_in_log_path(FileName, EsiLogPath) ->
    wait_log_exist_in_log_path(FileName, EsiLogPath, 600000).
wait_log_exist_in_log_path(FileName, EsiLogPath, Timeout) when Timeout < 0 ->
    ct:pal("Tc will fail due to log does NOT exist within expected time.~n"
	   "filename : ~p~n"
	   "LogPath : ~p", [FileName, EsiLogPath]),
    ct:fail("log does NOT exist within expected time");
wait_log_exist_in_log_path(FileName, EsiLogPath, Timeout) ->
    LS = os:cmd("ls "++EsiLogPath),
    %% LsList = string:tokens(LS, " .\n"),
    ct:log("ls : ~p", [LS]),
    
    case re:run(LS, FileName) of
	{match, _} ->
	    ct:pal("Log exist in log path", []);
	_Other ->
	    ct:log("Log NOT exist in log path, sleep and try again.", []),
	    timer:sleep(10000),
	    wait_log_exist_in_log_path(FileName, EsiLogPath, Timeout-5000)
    end.

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
check_node_is_in_nl(Console) ->
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
		_Other ->
		    no_nl
	    end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Return "true" or "false"
%% @end
%%--------------------------------------------------------------------
check_if_local_file(Loaclfile)->
    case Loaclfile of
	no_local_file ->
	    false;
	local_file ->
	    true
    end.


%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_lib.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/1
%%%
%%% @doc == support lib when testing upgrade mechanism. ==
%%% <br/>
%%%
%%%
%%% @end

-module(nl_lib).
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
%%% R3A/1      2015-02-23 eransbn     Created
%%% R3A/2      2015-02-24 etxivri     Update to handle Config from SUITEs.
%%%                                   And handle differents protocols and ports.
%%% R3A/3      2015-02-24 etxivri     Added more funtions.
%%% R3A/4      2015-02-25 etxivri     Add console name as input in some funct.
%%% R3A/5      2015-02-27 etxkols     Preparation for 2 labs,
%%% R3A/6      2015-02-27 etxivri     Add gen_and_write_sif
%%% R3A/7      2015-03-02 etxivri     Update to use https.
%%% R3A/7      2015-03-05 etxivri     Add request_download_pogress.
%%% R3A/10     2015-03-18 etxivri     Update export esi.
%%% R3A/11     2015-03-18 etxivri     bug fix.
%%% R3A/12     2015-03-19 etxivri     Add cancel.
%%% R3A/15     2015-03-30 etxivri     Update httpc request more robust.
%%% R3A/16     2015-03-31 etxivri     Update to make board_restore more robust.
%%% R3A/17     2015-04-08 etxivri     increased some timeouts.
%%% R3A/18     2015-04-08 etxivri     increased timeout in wait for netconf.
%%% R3A/19     2015-04-13 etxivri     Add option to spec xml file to use 
%%%                                   when download is used.
%%% R3A/19     2015-04-13 etxivri     Also changed body for integrate.
%%% R3A/20     2015-05-19 etxivri     Update in generic export esi.
%%% R3A/21     2015-05-26 etxivri     Update when get info about sftp_server.
%%% R3A/21     2015-05-26 etxivri     Add hard_factory_reset. And generic 
%%%                                   Download and integrate.
%%% R3A/23     2015-05-28 etxivri     add send_httpc_request_no_check.
%%% R4A/1      2015-07-01 etxivri     Make check of SITE_CONFIG more robust.
%%% R4A/2      2015-07-01 etxivri     New try to make it more robust.
%%% R4A/3      2015-07-02 etxivri     Minor bugfix.
%%% R4A/4      2015-10-22 etxmlar     Updated request_download_pogress to
%%%                                   handle no connection when nl upgrade.
%%% R5A/1      2016-05-25 etxmlar     Added new functions for AI testcases.
%%% R5A/2      2016-06-02 etxmlar     Added first version of 
%%%                                   download_file_nl_upgrade
%%% R6A/2      2016-06-16 etxmlar     Added cancel_lmt_integration
%%% R6A/3      2016-07-07 etxmlar     Updated the gen_and_write_sif_ipv4_untrusted
%%% R6A/4      2016-09-14 etxmlar     Increased max timeout in 
%%%                                   wait_for_download_completed because
%%%                                   Full UP download takes longer time
%%%                                   (coutains more CXP:s)
%%% R6A/4      2016-09-20 etxmlar     Updated download_file_nl_upgrade and
%%%                                   added check_for_printout_in_ailog
%%% R7A/1      2016-09-20 etxmlar     Merge from R6A
%%% R7A/2      2016-09-20 etxmlar     Increased waittime for nl download completed
%%% R7A/3      2016-10-28 etxmlar     check_for_nl_upgrade_completed updated
%%% R8A/1      2017-01-19 etxmlar     Added interface help functions
%%% R9A/1      2017-02-09 etxmlar     Try to make nl upgrade check more robust
%%% R9A/2      2017-02-13 etxmlar     Try to make nl upgrade check more robust again
%%% R9A/3      2017-02-23 etxmlar     Try to make download_files more robust again
%%% R9A/3      2017-03-20 etxmlar     Added gen_and_write_sif_for_emergency_restore/3,
%%% R10A/1     2017-04-27 etxmlar     Updated export_ai_log
%%% ----------------------------------------------------------


-export([check_if_vc_board/0,
	 create_fake_esi_dir/2,
	 open_netconf/1,
	 open_netconf/2,
	 wait_for_netconf_started/1,
	 wait_for_netconf_started/2,
	 wait_log_exist_in_log_path/1,
	 wait_log_exist_in_log_path/2,
	 wait_log_exist_in_log_path/3,
	 ms_since_1970/0,
	 site_config_complete/1,
	 site_config_complete/2,
	 check_expect_consol/3,
	 check_expect_consol/5,
	 check_node_is_in_nl/1,
	 httpc_request/3,
	 httpc_request/4,
	 httpc_request/5,
	 httpc_request/6,
	 httpc_request/7,
	 httpc_request/8,
	 httpc_request_lmt_integration/6,
	 httpc_request_lmt_integration/7,
	 httpc_request_lmt_integration/8,
	 send_httpc_request/4,
	 send_httpc_request_no_check/4,
	 download_files/1,
	 download_files/3,
	 download_files/4,
	 download_file_nl_upgrade/5,
	 generic_download_files/1,
	 generic_download_files/2,
	 generic_download_files/3,
	 request_download_pogress/1,
	 request_download_pogress/2,
	 request_download_pogress/3,
	 wait_for_download_completed/2,
	 wait_for_download_completed/3,
	 integrate/3,
	 integrate/4,
	 integrate/5,
	 generic_integrate/3,
	 board_restore/3,
	 board_restore/4,
	 board_restore/5,
	 factory_reset/3,
	 factory_reset/4,
	 factory_reset/5,
	 hard_factory_reset/3,
	 hard_factory_reset/4,
	 hard_factory_reset/5,
	 export_ai_log/1,
	 export_ai_log/2,
	 export_ai_log/3,
	 export_ai_log/5,
	 export_esi/1,
	 export_esi/2,
	 export_esi/3,
	 export_esi/5,
	 generic_export_esi/3,
	 generic_export_esi/4,
	 generic_export_esi/5,
	 cancel/2,
	 cancel/3,
	 cancel/4,
	 cancel_lmt_integration/2,
	 cancel_lmt_integration/3,
	 cancel_lmt_integration/4,
	 wait_node_is_in_nl/1,
	 wait_node_is_in_nl/2,
	 gen_and_write_sif/3,
	 gen_and_write_sif_lmt_on_site_or_zt_off_site/3,
	 gen_and_write_sif_ipv4_trusted/5,
	 gen_and_write_sif_ipv6_trusted/5,
	 gen_and_write_sif_ipv4_untrusted/5,
	 gen_and_write_sif_for_emergency_restore/3,
	 gen_and_write_RbsSummaryFile/3,
	 check_for_printout_in_ailog/1,
	 get_tn_port_capitel/1,
	 get_tn_port_lower_case/1,
	 get_interface_id/2,
	 get_board_interfaces/1
	]).


%% -define(DefaultProt, "http").
-define(DefaultProt, "https").
-define(DefaultPort, "8080").
-define(DefaultXmlFile, "RbsSummaryFile.xml").
-define(FileName, "fakeEsi").
%% -define(FileNameExportEsi, "exportEsi").
-define(FileNameExportEsi, ""). %% No name when export ESI.
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

open_netconf(NC) ->
    open_netconf(NC, []).

open_netconf(NC, Param) ->
	open_netconf(NC, Param, check_if_vc_board()).

open_netconf(NC, Param, VcBoard) when VcBoard == "yes" ->
	%% board with vc, open with user name and password
	open_netconf(NC, Param, "SysAdminTest", "SysAdminTest");
open_netconf(NC, Param, _VcBoard) ->
	%% board without vc, try to open without user name and password (legacy)
    %% and, if failed, with username and password
	Ret = open_netconf(NC, Param, "", ""),
	case Ret of
		{ok, _} -> Ret;
		 _ -> open_netconf(NC, Param, "SysAdminTest", "SysAdminTest")
	end.

open_netconf(NC, Param, User, Password) ->
	case (string:len(User)>0) and (string:len(Password)>0) of
		true -> 
			ct_netconfc:open(NC, [{user, User}, {password, Password} | Param]);
		false ->
			ct_netconfc:open(NC, Param)
	end.

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
	    timer:sleep(5000),
	    wait_for_netconf_started(NC, Timeout - 5000)
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
%% @end
%%--------------------------------------------------------------------
wait_node_is_in_nl(Console) ->
    ct:pal("### Start wait for NL",[]),
    wait_node_is_in_nl(Console, 180000).
wait_node_is_in_nl(_Console, Timeout) when Timeout < 0 ->
    ct:fail("Node is not in NL within max timeout.");
wait_node_is_in_nl(Console, Timeout) ->
    ct_telnet:send(Console, ""),
    case ct_telnet:expect(Console, "networkloader]#",
			  [{timeout,10000},no_prompt_check]) of
	{ok,_} ->
	    ct:pal("### Node is in NL",[]),
	    ok;
	_Other  ->
	    ct:log("### Node is NOT in NL, sleep and try again. ~n"
		   "_Other : ~p",[_Other]),
	    %% timer:sleep(10000),
	    wait_node_is_in_nl(Console, Timeout - 10000)
    end.

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
%%     timer:sleep(30000),
%%     open_netconf(NC),
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
%% 			   "Try again.",[SITE_CONFIG_COMPLETE]),
%% 		    site_config_complete(NC, Timeout-30000)
%%     end.


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
httpc_request(Config, Method, DoAction) ->
    httpc_request(Config, Method, DoAction, ?DefaultProt, ?DefaultPort, 
		  dummy, dummy, ?DefaultXmlFile).
httpc_request(Config, Method, DoAction, Protocol) ->
    httpc_request(Config, Method, DoAction, Protocol, dummy, dummy, 
		  dummy, ?DefaultXmlFile).
httpc_request(Config, Method, DoAction, Protocol, Port) ->
    httpc_request(Config, Method, DoAction, Protocol, Port, dummy, dummy, 
		  ?DefaultXmlFile).
httpc_request(Config, Method, DoAction, Protocol, Port, XmlFile) ->
    httpc_request(Config, Method, DoAction, Protocol, Port, dummy, dummy, 
		  XmlFile).
httpc_request(Config, Method, DoAction, Protocol, Port, EsiLogPath, EsiName) ->
    httpc_request(Config, Method, DoAction, Protocol, Port, EsiLogPath, 
		  EsiName, ?DefaultXmlFile).
httpc_request(Config, Method, DoAction, Protocol, Port, EsiLogPath, EsiName, 
	     XmlFile) ->
    ct:log("Config: ~p", [Config]),
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    SftpAiInstallDir = ct:get_config({Hw,sftp_ai_install_dir}),
    MyTime = integer_to_list(ms_since_1970()),

    timer:sleep(2000),
    start_needed_applications(Protocol),
    timer:sleep(2000),

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

    ct:log("Protocol: ~p, Port: ~p", [Protocol, Port]),
    
    case DoAction of
	%% %%%%
	%% %% Download and Integrate
	%% %%%%
	%% Val when Val == "Download";
	%% 	 Val == "Integrate" ->
	"Download" ->
	    Body = "host="++SftpHost++"&"++
		"username="++Username++"&"++
		"password="++Password++"&"++
		%% "filename=boards/"++HW++"/RbsSummaryFile.xml"++"&"++
		"filename=boards/"++HW++"/"++XmlFile++"&"++
		"utcMs="++MyTime++"&"++
		"Do"++DoAction++"="++DoAction;
	"Integrate" ->
	    %% Body = "Do"++DoAction++"="++DoAction;
	    Body = "host="++"&"++
		"username="++"&"++
		"password="++"&"++
		"filename="++"&"++
		"utcMs="++MyTime++"&"++
		"Do"++DoAction++"="++DoAction;
	%%%%
	%% BoardRestore and FactoryReset and HardFactoryReset
	%%%%
	Val when Val == "BoardRestore";
		 Val == "FactoryReset";
		 Val == "HardFactoryReset";
		 Val == "Cancel"->
	    Body = "Do"++DoAction++"="++DoAction;
	%%%%
	%% Export and ESI
	%%%%
	Val when Val == "Export";
		 Val == "ESI" ->
	    case DoAction of
		"Export" ->
		    Path = "filename=";
		"ESI" -> 
		    Path = "directory="
	    end,
	    FileName = EsiName,
	    %% EsiLogPath = ?config(priv_dir,Config),
	    os:cmd("chmod 777 "++ EsiLogPath),
	    ct:pal("EsiLogPath : ~p", [EsiLogPath]),
	    Body = "host="++SftpHost++"&"++
		"username="++Username++"&"++
		"password="++Password++"&"++
		Path++EsiLogPath++FileName++"&"++
		"Do"++DoAction++"="++DoAction;
	    
	_Other ->
	    %% Url = dummy,
	    Body = dummy,
	    ct:fail("### No Valid DoAction not found")
    end,

    Url = get_url(Protocol, Port, IP, DoAction),

    ct:pal("DoAction : ~p", [DoAction]),
    ct:pal("url : ~p", [Url]),
    ct:pal("Body : ~p", [Body]),

    timer:sleep(2000),

    %% {ok, Reply} = httpc:request(Method, {Url, [], [], Body}, [], []),
    %% ct:log("Reply : ~p", [Reply]),
    %% {{_,200,"OK"}, _ReplyA, _ReplyB} = Reply,

    %% A try to make request more robust
    Reply = send_httpc_request(Method, Url, Body, 600000),
    {{_,200,"OK"}, _ReplyA, _ReplyB} = Reply,

    timer:sleep(2000),
    %% inets:stop(), %% För httpc
    stop_needed_applications(Protocol),
    timer:sleep(2000),

    ok.

send_httpc_request(Method, Url, Body, Timeout) ->
    case httpc:request(Method, {Url, [], [], Body}, [{timeout, Timeout}], []) of 
	{ok, Reply} ->
    	    ct:log("Reply : ~p", [Reply]),
	    ok;
    	_A -> 
    	    ct:log("Ans from httpc request not expected, try again. ~n "
    		   "Answ: ~p ", [_A]),
    	    case httpc:request(Method, {Url, [], [], Body}, 
			       [{timeout, Timeout}], []) of
    		{ok, Reply} ->
    		    ct:log("Reply second time: ~p", [Reply]),
    		    ok;
    		_B -> 
    		    ct:log("Ans from second httpc request not expected, "
    			   "try again last time. ~n "
    			   "Answ: ~p ", [_B]),
    		    {ok, Reply} = httpc:request(Method, {Url, [], [], Body}, 
    						[{timeout, Timeout}], [])
    	    end
    end,
    Reply.


send_httpc_request_no_check(Method, Url, Body, Timeout) ->
    case httpc:request(Method, {Url, [], [], Body}, [{timeout, Timeout}],[]) of 
	{ok, Reply} ->
    	    ct:log("Reply : ~p", [Reply]),
	    ok;
    	_A -> 
    	    ct:log("Ans from httpc request not expected, try again. ~n "
    		   "Answ: ~p ", [_A]),
    	    case httpc:request(Method, {Url, [], [], Body}, 
			       [{timeout, Timeout}], []) of
    		{ok, Reply} ->
    		    ct:log("Reply second time: ~p", [Reply]),
    		    ok;
    		_B -> 
    		    ct:log("Ans from second httpc request not expected, "
    			   "try again last time. ~n "
    			   "Answ: ~p ", [_B]),
    		    {_, Reply} = httpc:request(Method, {Url, [], [], Body}, 
    						[{timeout, Timeout}], [])
    	    end
    end,
    Reply.


get_url(Protocol, Port, IP, DoAction) ->
    get_url(Protocol, Port, IP, DoAction, dummy).
get_url(Protocol, Port, IP, DoAction, BoardState) ->
    case Protocol of
	"https" ->
	    URL = Protocol++"://"++IP++"/cgi-bin/";
	 "http"-> 
	    URL = Protocol++"://"++IP++":"++Port++"/cgi-bin/";
	_Other ->
	    URL = dummy,
	    ct:pal("Rcvd protocol : ~p ", [_Other]),
	    ct:fail("TC will fail, Not defined protocol")
    end,
     
    case DoAction of
	Val when Val == "Download";
		 Val == "Integrate";
		 Val == "Export";
		 Val == "Cancel"->
	    Url = URL++"nl_gui:post";
	Val when Val == "BoardRestore";
		 Val == "FactoryReset";
		 Val == "HardFactoryReset"->
	    Url = URL++"aicGui:post";
	"ESI" ->
	    case BoardState of
		nl ->
		    Url = URL++"nl_gui:post";
		_Else ->
		    Url = URL++"aicGui:post"
	    end;
	_Otheer ->
	    Url = dummy,
	    ct:fail("TC will fail, Not defined DoAction")
    end,
    Url.
	    
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
%% @spec export_esi(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_esi(Config) ->
    FileName = ?FileNameExportEsi,
    EsiLogPath = ?config(priv_dir,Config),
    export_esi(Config, ?DefaultProt, ?DefaultPort, EsiLogPath, FileName).
export_esi(Config, Protocol) ->
    FileName = ?FileNameExportEsi,
    EsiLogPath = ?config(priv_dir,Config),
    export_esi(Config, Protocol, dummy, EsiLogPath, FileName).
export_esi(Config, Protocol, Port) ->
    FileName = ?FileNameExportEsi,
    EsiLogPath = ?config(priv_dir,Config),
    export_esi(Config, Protocol, Port, EsiLogPath, FileName).
export_esi(Config, Protocol, Port, EsiLogPath, FileName) ->
    ct:pal("export_esi"),
    httpc_request(Config, post, "ESI", Protocol, Port, EsiLogPath, FileName),

    timer:sleep(5000),
    wait_log_exist_in_log_path(EsiLogPath),

    ct:pal("esi exist in ~n path: ~n~p ", [ EsiLogPath]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec generic_export_esi(Config, Console, Protocol) -> ok
%% @end
%%--------------------------------------------------------------------
generic_export_esi(Config, Console, Protocol) ->
    generic_export_esi(Config, Console, Protocol, "").
generic_export_esi(Config, Console, Protocol, Port) ->
    %% BoardState is either nl or no_nl
    BoardState = check_node_is_in_nl(Console),
    generic_export_esi(Config, Console, Protocol, Port, BoardState).
generic_export_esi(Config, _Console, Protocol, Port, BoardState) ->
    ct:pal("generic_export_esi_no_check.",[]),
    EsiLogPath = ?config(priv_dir,Config),
    os:cmd("chmod 777 "++EsiLogPath),
    HW = atom_to_list(ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),

    ct:pal("BoardState: ~p",[BoardState]),

    DoAction = "ESI",
    Url = get_url(Protocol, Port, IP, DoAction, BoardState),
    ct:pal("Url: ~p",[Url]),

    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),

    Body = "host="++SftpHost++"&"++
    	"username="++Username++"&"++
    	"password="++Password++"&"++
	"directory="++EsiLogPath++"&"++
    	"Do"++DoAction++"="++DoAction,
    ct:pal("Body: ~p",[Body]),

    start_needed_applications(Protocol),
    {ok, Reply} = httpc:request(post, {Url, [], [], Body}, [], []),
    stop_needed_applications(Protocol),
    ct:pal("Reply: ~p", [Reply]),

    %% Simple check to se esi exist.
    timer:sleep(60000),
    ct:pal("EsiLogPath: ~p~n",[EsiLogPath]),

    Result = check_if_esi_exist_or_not(EsiLogPath),

    {Result, Reply}.


check_if_esi_exist_or_not(EsiLogPath) ->
    check_if_esi_exist_or_not(EsiLogPath, 120000).
check_if_esi_exist_or_not(_EsiLogPath, Timeout) when Timeout < 0 ->
    ct:pal("Result: esi_not_exist", []),
    esi_not_exist;
check_if_esi_exist_or_not(EsiLogPath, Timeout) -> 
    Ls = os:cmd("ls "++EsiLogPath),
    ct:pal("ls: ~p", [Ls]),
    case re:run(Ls, "esi[.]") of
	{match,_} ->
	    ct:pal("Result: esi_exist", []),
	    esi_exist;
	nomatch ->
	    timer:sleep(10000),
	    check_if_esi_exist_or_not(EsiLogPath, Timeout-10000)
    end.    

%%--------------------------------------------------------------------
%% @doc
%% @spec export_ai_log(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_ai_log(Config) ->
    FileName = ?FileName,
    EsiLogPath = ?config(priv_dir, Config),
    export_ai_log(Config, ?DefaultProt, ?DefaultPort, EsiLogPath, FileName).
export_ai_log(Config, Protocol) ->
    FileName = ?FileName,
    EsiLogPath = ?config(priv_dir, Config),
    export_ai_log(Config, Protocol, dummy, EsiLogPath, FileName).
export_ai_log(Config, Protocol, Port) ->
    FileName = ?FileNameExportEsi,
    EsiLogPath = ?config(priv_dir,Config),
    export_ai_log(Config, Protocol, Port, EsiLogPath, FileName).
export_ai_log(Config, Protocol, Port, EsiLogPath, FileName) ->
    ct:pal("export_ai_log"),
    httpc_request(Config, post, "Export", Protocol, Port, EsiLogPath, FileName),
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
%% @spec factory_reset(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
factory_reset(Config, Console, NC) ->
    factory_reset(Config, Console, NC, ?DefaultProt, ?DefaultPort).
factory_reset(Config, Console, NC, Protocol) ->
    factory_reset(Config, Console, NC, Protocol, dummy).
factory_reset(Config, Console, NC, Protocol, Port) ->
    ct:pal("factory_reset"),
    wait_for_netconf_started(NC),
    site_config_complete(NC),
    httpc_request(Config, post, "FactoryReset", Protocol, Port),
    case check_if_vc_board() of
	"yes" -> 
	    check_expect_consol(Console, 
				NC,
				"Secure Boot Enabled", 
				300000, 
				Config),
	    check_expect_consol(Console, 
				NC, 
				"Network Loader Ready:", 
				300000, 
				Config);
	_Other ->
	    ExpectStr = "Welcome to Autointegration with laptop", 
	    {ok, _} = ct_telnet:expect(Console,
				       ExpectStr, 
				       [{timeout,300000},no_prompt_check]),
	    ct:pal("Rcvd : ~p, Now you are in NL. ",[ExpectStr])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec hard_factory_reset(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
hard_factory_reset(Config, Console, NC) ->
    hard_factory_reset(Config, Console, NC, ?DefaultProt, ?DefaultPort).
hard_factory_reset(Config, Console, NC, Protocol) ->
    hard_factory_reset(Config, Console, NC, Protocol, dummy).
hard_factory_reset(Config, Console, NC, Protocol, Port) ->
    ct:pal("HardFactoryReset"),
    %% wait_for_netconf_started(NC),
    site_config_complete(NC,  10000),
    httpc_request(Config, post, "HardFactoryReset", Protocol, Port),

    case ct_telnet:expect(Console,
			  "Ericsson Version:", 
			  [{idle_timeout,120000},no_prompt_check]) of
	{ok, _} ->
	    ok;
	_A ->
	    ct:log("HardFactoryReset not performed, try again.~n~p", [_A]),
	    httpc_request(Config, post, "HardFactoryReset", Protocol, Port)
    end,

    {ok, _} = ct_telnet:expect(Console,
    			       "Network Loader Ready:", 
    			       [{timeout,300000},no_prompt_check]),

    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout,30000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% @spec board_restore(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
board_restore(Config, Console, NC) ->
    board_restore(Config, Console, NC, ?DefaultProt, ?DefaultPort).
board_restore(Config, Console, NC, Protocol) ->
    board_restore(Config, Console, NC, Protocol, dummy).
board_restore(Config, Console, NC, Protocol, Port) ->
    ct:pal("board_restore"),
    %% wait_for_netconf_started(NC),
    site_config_complete(NC,  10000),
    httpc_request(Config, post, "BoardRestore", Protocol, Port),

    case ct_telnet:expect(Console,
			  "Ericsson Version:", 
			  [{idle_timeout,120000},no_prompt_check]) of
	{ok, _} ->
	    ok;
	_A ->
	    ct:log("board restore not performed, try again.~n~p", [_A]),
	    httpc_request(Config, post, "BoardRestore", Protocol, Port)
    end,

    {ok, _} = ct_telnet:expect(Console,
    			       "Network Loader Ready:", 
    			       [{timeout,300000},no_prompt_check]),
%%
    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout,30000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% @spec check_expect_consol(ExpStr, Timeout, Config) -> ok
%% @end
%%--------------------------------------------------------------------
check_expect_consol(ExpStr, Timeout, Config) ->
    check_expect_consol(console, nc1, ExpStr, Timeout, Config).
check_expect_consol(Console, NC, ExpStr, Timeout, Config) ->
   case  ct_telnet:expect(Console, ExpStr, 
			  [{timeout, Timeout}, no_prompt_check]) of
       {ok, _} -> 
	   ok;
       _ ->
	   ct:log("### TC will fail due to timeout when check consol logs"),
	   export_ai_log(Config), %% If type 2 sw is used
	   case open_netconf(NC, []) of
	       {ok,_} -> 
		   ct_netconfc:close_session(NC),
		   export_esi(Config, NC); %% If type 3 sw is used
	       _Else ->
		   skip
	   end,
	   ct:fail("No match in consol log within expected time.")
   end.
%%--------------------------------------------------------------------
%% @doc
%% @spec integrate(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
%%%%
%%%% Note! could result in ERROR in ct shell.
%%%%       Received unexpected ssl data on {sslsocket
%%%%       Ingela at OTP is informed.
%%%%
integrate(Config, Console, NC) ->
    integrate(Config, Console, NC, ?DefaultProt, ?DefaultPort).
integrate(Config, Console, NC, Protocol) ->
    integrate(Config, Console, NC, Protocol, dummy).
integrate(Config, Console, NC, Protocol, Port) ->
    ct:pal("integrate"),

    httpc_request(Config, post, "Integrate", Protocol, Port),
    
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
%% @spec cancel(Config, Console) -> ok
%% @end
%%--------------------------------------------------------------------
cancel(Config, Console) ->
    cancel(Config, Console, ?DefaultProt, ?DefaultPort).
cancel(Config, Console, Protocol) ->
    cancel(Config, Console, Protocol, dummy).
cancel(Config, Console, Protocol, Port) ->
    ct:pal("Cancel"),
    httpc_request(Config, post, "Cancel", Protocol, Port),

    {ok, _} = ct_telnet:expect(Console,
			       "Network Loader Ready:", 
			       [{timeout,300000},no_prompt_check]),
    
    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout,30000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% @spec cancel_lmt_integration(Config, Console) -> ok
%% @end
%%--------------------------------------------------------------------
cancel_lmt_integration(Config, Console) ->
    cancel_lmt_integration(Config, Console, ?DefaultProt, ?DefaultPort).
cancel_lmt_integration(Config, Console, Protocol) ->
    cancel_lmt_integration(Config, Console, Protocol, dummy).
cancel_lmt_integration(Config, Console, Protocol, Port) ->
    ct:pal("Cancel"),
    httpc_request_lmt_integration(Config, post, "Cancel", Protocol, Port),

    {ok, _} = ct_telnet:expect(Console,
			       "Network Loader Ready:", 
			       [{timeout,300000},no_prompt_check]),
    
    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout,30000},no_prompt_check]).



%%--------------------------------------------------------------------
%% @doc
%% @spec download_files(Config) -> ok
%% @end
%%--------------------------------------------------------------------
download_files(Config) ->
    download_files(Config, console, ?DefaultProt, ?DefaultPort).
download_files(Config, Console, Protocol) ->
    download_files(Config, Console, Protocol, dummy).
download_files(Config, Console, Protocol, Port) ->
    ct:pal("download_files"),

    httpc_request(Config, post, "Download", Protocol, Port),

    timer:sleep(20000),

    case check_if_vc_board() of
    	"yes" -> 
	    ok;
    	_Other ->
    	    {ok, _} = ct_telnet:expect(Console, 
    				       "Download Completed", 
    				       [{timeout,300000},no_prompt_check]), 
    	    ct:pal("Rcvd : Download Completed. OK to continue",[])
    end,
    
    wait_for_download_completed(Config, Protocol, Port),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec generic_download_files(Config) -> ok
%% @end
%%--------------------------------------------------------------------
generic_download_files(Config) ->
    generic_download_files(Config, console, "RbsSummaryFile.xml").
generic_download_files(Config, Console) ->
    generic_download_files(Config, Console, "RbsSummaryFile.xml").
generic_download_files(Config, Console, XmlFile) ->
    ct:pal("generic download_files"),

    HW = atom_to_list(ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
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
%% @spec  download_file_nl_upgrade(Config, Console, SifFile, Loaclfile, UpNLVerStr) -> ok
%% @end
%%--------------------------------------------------------------------
download_file_nl_upgrade(Config, Console, NL_Upgrade_File, Loaclfile, UpNLVerStr) ->

  {SifcontentInput, LocalFileInput} =  
	case check_if_local_file(Loaclfile) of
	    true ->
		ct:log("local file used"),

		TftpBootDir = proplists:get_value(tftpboot_dir, Config),
		File_path = filename:join(TftpBootDir, NL_Upgrade_File),

		{ok, Binary} = file:read_file(File_path),
		SifContentStr = binary_to_list(Binary),

		SifContentHexEncodedURI = http_uri:encode(SifContentStr),
		ct:log("SifContentHexEncodedURI: ~p", [SifContentHexEncodedURI]),

		{SifContentHexEncodedURI, NL_Upgrade_File}; 
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
	"filename=boards/"++HW++"/"++NL_Upgrade_File++"&"++
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
	    check_for_nl_upgrade_completed(Config, Console, UpNLVerStr);
	"http" ->
	    ct:log("Fail to upgrade nl, using http"),
	    ct:fail("Action fail using http!")

    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec check_for_nl_upgrade_completed(Config, Console, UpNLVerStr) -> ok
%% @end
%%--------------------------------------------------------------------
check_for_nl_upgrade_completed(_Config, Console, UpNLVerStr)->
    ct:pal("### check_for_nl_upgrade_completed",[]),
    check_for_download_completed(_Config, Console, 300000, UpNLVerStr).

check_for_download_completed(_Config, Console, TotalTimeout, UpNLVerStr) ->
    NLVerStr = lists:last(string:tokens(UpNLVerStr, "\- ")),
    %%check_for_printout_in_ailog("Upgrading networkloader to version "++ NLVerStr),

    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    check_for_printout_in_ailog("Upgrading networkloader to version "++ NLVerStr);
	_ ->
	    {ok,_} = ct_telnet:expect(Console, 
				      "File and software installation successful",
	    			      %%"Rebooting on new Network Loader",
	    			      [{timeout,300000}, no_prompt_check]),

	    {ok,_} = ct_telnet:expect(Console, 
				      ["RBS Sys: restart ordered by sysreboot",
				       "RBS Sys: Restart ordered by sysreboot"],
				      [{timeout,300000}, no_prompt_check])
    end,
    
    {ok,_} = 
	ct_telnet:expect(Console, 
			 "Network Loader Ready",
			 [{timeout, TotalTimeout}, no_prompt_check]),
    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout, TotalTimeout},no_prompt_check]),

    check_for_printout_in_ailog("Running version: \""++UpNLVerStr++"\""),

    ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @spec wait_for_nl_upgrade_completed(Config, Protocol) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% wait_for_nl_upgrade_completed(Config, Protocol)->
%%     ct:pal("### wait_for_nl_upgrade_completed",[]),
%%     wait_for_download_completed(Config, Protocol, 300000).

%% wait_for_download_completed(_Config, Protocol, Timeout) when Timeout < 0 ->
%%     ct:fail("wait_for_nl_upgrade_completed not rcvd within max timeout.");

%% wait_for_download_completed(Config, Protocol, Timeout) ->

%%     case request_download_progress_for_nl_upgrade(Config, Protocol) of
%% 	%% Do not know yet, just an example: "NLDownloadReady"
%% 	{_, _, "NLDownloadReady"} ->
%% 	    ct:pal("Download Completed", []);
%% 	_Other ->
%% 	    ct:pal("Download not done yet, sleep and check again.~n~p",[_Other]),
%% 	    timer:sleep(20000),
%% 	    wait_for_download_completed(Config, Protocol,Timeout-20000)
%%     end.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @spec request_download_progress_for_nl_upgrade(Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% request_download_progress_for_nl_upgrade(Config, Protocol) ->
%%     ct:pal("request_download_progress_for_nl_upgrade"),

%%     ct:pal("Protocol: ~p ", [Protocol]),
%%     ct:log("Config: ~p", [Config]),

%%     HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
%%     Hw = list_to_atom(HW),
%%     _BOARDTYPE = ct:get_config({Hwa,board_type}),
%%     [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),

%%     start_needed_applications(Protocol),

%%     {Answere, Reply} = 
%% 	%% Do not know yet, just an example: nl_progress.txt
%% 	case httpc:request("https://"++IP++"/nl_progress.txt") of
%% 	    {ok, R} ->
%% 		{ok, R}; 
%% 	    {error, R} ->
%% 		{error, R}
%% 	end,

%%     stop_needed_applications(Protocol),	

%%     ct:log("Answer from download progress: ~n~p~n~p",[Answere, Reply]),
%%     Reply.


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

%%--------------------------------------------------------------------
%% @doc
%% Start using https and that will fail it will try old way, http port 8080.
%% @spec generic_integrate(Config, Console, NC) -> ok
%% @end
%%--------------------------------------------------------------------
generic_integrate(Config, Console, NC) ->
    ct:pal("generic integrate"),

    HW = atom_to_list(ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    %% [{host, SftpHost},{username, Username},{password, Password}] = 
    %% 	ct:get_config(sftp_server),
    MyTime = integer_to_list(ms_since_1970()),

    Method = post,
    
    Body = "host="++"&"++
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
	    Url_http = "http://"++IP++":"++?DefaultPort++"/cgi-bin/nl_gui:post",
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
    wait_for_download_completed(Config, Protocol, Port, 480000). %%8 minutes

wait_for_download_completed(_Config,_Protocol,_Port,Timeout) when Timeout < 0 ->
    ct:fail("wait_for_download_completed not rcvd within max timeout.");

wait_for_download_completed(Config, Protocol, Port, Timeout) ->
    case request_download_pogress(Config, Protocol, Port) of
	{_, _, "DownloadReady"} ->
	    ct:pal("Download Completed", []);
	_Other ->
	    ct:pal("Download not done yet, sleep and check again.~n~p",[_Other]),
	    timer:sleep(20000),
	    wait_for_download_completed(Config, Protocol, Port, Timeout-20000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec request_download_pogress(Config) -> ok
%% @end
%%--------------------------------------------------------------------
request_download_pogress(Config) ->
    request_download_pogress(Config, ?DefaultProt, ?DefaultPort).
request_download_pogress(Config, Protocol) ->
    request_download_pogress(Config, Protocol, dummy).
request_download_pogress(Config, Protocol, Port) ->
    ct:pal("request_download_pogress"),

    ct:pal("Protocol: ~p , Port: ~p", [Protocol, Port]),
    ct:log("Config: ~p", [Config]),
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),

    start_needed_applications(Protocol),

    {Answere, Reply} = 
	case Protocol of
	    "http" ->
		case httpc:request("http://"++IP++":"++Port++"/progress.txt") of
		    {ok, R} ->
			{ok, R}; 
		    {error, R} ->
			{error, R}
		end;
	    "https" ->
		case httpc:request("https://"++IP++"/progress.txt") of
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


%%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_sif(Sifname, Summaryname, Node) -> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_sif(Sifname, Summaryname, Node) ->
    ct:pal("Sifname: ~p~n"
	   "Summaryname: ~p~n"
	   "Node: ~p~n", [Sifname, Summaryname, Node]),
    Tftpdir = ct:get_config({Node,tftpboot}),
    ct:pal("Tftpdir: ~p", [Tftpdir]),
    Tftp_name = filename:join(Tftpdir, Sifname),
    ct:pal("Tftp_name: ~p", [Tftp_name]),
    RbsSumFilePath = add_base_dir(Summaryname),
    ct:pal("RbsSumFilePath: ~p", [RbsSumFilePath]),
    SIF_str = sif_string(RbsSumFilePath),
    ct:log("SIF_str: ~p", [SIF_str]),
    file:write_file(Tftp_name, SIF_str),
    ct:pal("Path to SiteInstallationFile.xml : ~n~p~n", [Tftp_name]).

sif_string(Rbs_sum_path) ->
    "<RbsSiteInstallationFile
        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
        xsi:noNamespaceSchemaLocation=\"SiteInstallation.xsd\">
        <Format revision=\"K\"/>
        <InstallationData
	logicalName=\"%Node Name%\"
                vlanId=\"%3100%\"
                rbsIntegrationCode=\"%rbsIntegrationCode%\">
                <OamIpConfigurationData
	ipAddress=\"%Integration OaM IP Address%\"
                        subnetMask=\"255.255.240.0\"
                        defaultRouter0=\"10.200.0.1\">
                        <DnsServer
	ipAddress=\"10.212.100.10\"/>
                </OamIpConfigurationData>
	<SmrsData
	address=\"%SMRS Data address%\"
                        userName=\"%user_name%\"
                        password=\"%password%\"
                        summaryFilePath=\"" ++ Rbs_sum_path ++ "\"
        />
        </InstallationData>
	</RbsSiteInstallationFile>".

%%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_sif_lmt_on_site_or_zt_off_site(Sifname, Summaryname, Node) -> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_sif_lmt_on_site_or_zt_off_site(Sifname, Summaryname, Node) ->

    ct:pal("Sifname: ~p~n"
	   "Summaryname: ~p~n"
	   "Node: ~p~n", [Sifname, Summaryname, Node]),

    Tftpdir = ct:get_config({Node,tftpboot}),
    ct:pal("Tftpdir: ~p", [Tftpdir]),
    Tftp_name = filename:join(Tftpdir, Sifname),
    ct:pal("Tftp_name: ~p", [Tftp_name]),

    RbsSumFilePath = add_base_dir(Summaryname),
    ct:pal("RbsSumFilePath: ~p", [RbsSumFilePath]),

    [{host, SftpHost},{username, _Username},{password, _Password}] = 
	ct:get_config(sftp_server),
    
    SIF_str = sif_string(RbsSumFilePath, SftpHost),
    ct:log("SIF_str: ~n", []),
    ct:log(re:replace(SIF_str,"<","\\&lt;",[{return,list},global])),

    file:write_file(Tftp_name, SIF_str),
    ct:pal("Path to SiteInstallationFile.xml : ~n~p~n", [Tftp_name]).



sif_string(Rbs_sum_path, SMRS_Data_address) ->
    "<RbsSiteInstallationFile
        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
        xsi:noNamespaceSchemaLocation=\"SiteInstallation.xsd\">
        <Format revision=\"K\"/>
        <InstallationData
	logicalName=\"%Node Name%\"
                vlanId=\"%3100%\"
                rbsIntegrationCode=\"%rbsIntegrationCode%\">
                <OamIpConfigurationData
	ipAddress=\"%Integration OaM IP Address%\"
                        subnetMask=\"255.255.240.0\"
                        defaultRouter0=\"10.200.0.1\">
                        <DnsServer
	ipAddress=\"10.212.100.10\"/>
                </OamIpConfigurationData>
	<SmrsData
	address=\"" ++ SMRS_Data_address ++ "\"
                        userName=\"%user_name%\"
                        password=\"%password%\"
                        summaryFilePath=\"" ++ Rbs_sum_path ++ "\"
        />
        </InstallationData>
	</RbsSiteInstallationFile>".

%%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_sif_for_emergency_restore(Sifname, Summaryname, Node) -> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_sif_for_emergency_restore(Sifname, Summaryname, Node) ->

    ct:pal("Sifname: ~p~n"
	   "Summaryname: ~p~n"
	   "Node: ~p~n", [Sifname, Summaryname, Node]),

    Tftpdir = ct:get_config({Node,tftpboot}),
    ct:pal("Tftpdir: ~p", [Tftpdir]),
    Tftp_name = filename:join(Tftpdir, Sifname),
    ct:pal("Tftp_name: ~p", [Tftp_name]),

    RbsSumFilePath = add_base_dir(Summaryname),
    ct:pal("RbsSumFilePath: ~p", [RbsSumFilePath]),

    [{host, SftpHost},{username, _Username},{password, _Password}] = 
	ct:get_config(sftp_server),

    SIF_str = sif_emergency_restore_string(RbsSumFilePath, SftpHost),
    ct:log("SIF_str: ~n", []),
    ct:log(re:replace(SIF_str,"<","\\&lt;",[{return,list},global])),

    file:write_file(Tftp_name, SIF_str),
    ct:pal("Path to SiteInstallationFile.xml : ~n~p~n", [Tftp_name]).

sif_emergency_restore_string(Rbs_sum_path, SMRS_Data_address) ->
    "<RbsSiteInstallationFile
        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
        xsi:noNamespaceSchemaLocation=\"SiteInstallation.xsd\">
        <Format revision=\"K\"/>
        <InstallationData>
	<SmrsData
	address=\"" ++ SMRS_Data_address ++ "\"
                        userName=\"%user_name%\"
                        password=\"%password%\"
                        summaryFilePath=\"" ++ Rbs_sum_path ++ "\"
        />
        </InstallationData>
	</RbsSiteInstallationFile>".

%%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_sif_ipv4_trusted(Config, Sifname, Summaryname, Node, TN_port)-> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_sif_ipv4_trusted(Config, Sifname, Summaryname, Node, TN_port) ->
    ct:log("Sifname: ~p~n"
	   "Summaryname: ~p~n"
	   "Node: ~p~n", [Sifname, Summaryname, Node]),

    Priv_dir = ?config(priv_dir, Config),
    ct:pal("Priv_dir: ~p", [Priv_dir]),
    os:cmd("chmod 777 "++Priv_dir), % else permission.

    Priv_dir_path_to_sif = filename:join(Priv_dir, Sifname),
    ct:pal("Priv path to SIF: ~p", [Priv_dir_path_to_sif]),

    %% Temporary, change when "new labb"??
    RbsSumFilePath = get_summary_dir(Summaryname),
    ct:pal("RbsSumFilePath: ~p", [RbsSumFilePath]),

    SIF_str = sif_ipv4_string(RbsSumFilePath, Node, TN_port),
    ct:pal("SIF ipv4: ~n", []),
    ct:log(re:replace(SIF_str,"<","\\&lt;",[{return,list},global])),

    file:write_file(Priv_dir_path_to_sif, SIF_str),
    ct:pal("Path to ~p: ~n~p~n", [Sifname, Priv_dir_path_to_sif]).
    

sif_ipv4_string(RbsSumFilePath, Node, TN_port) ->

    [{ssh, OAM_IpAddress}, {vlan, VlanId}, 
     {netmask, SubnetMask}, {gateway, DefaultRouter0}] = ct:get_config({Node, ssh_TN_A_ipv4}),

    Node_Str = atom_to_list(Node),
    VlanId_Str = integer_to_list(VlanId),

    %% Temporary, get from stp.cfg when "new labb"
    %% for all config data below
    DNS_IpAddress = "10.68.101.148", 
    SMRS_address= "10.68.101.157",   
    %% SMRS_User_Name= "dustest",       
    %% SMRS_password= "dustest",        
    SMRS_User_Name= "",       
    SMRS_password= "",        
    

    ct:pal("Node: ~p~n"
	   "TN_port:~p~n"
	   "VlanId: ~p~n"
	   "OAM_IpAddress: ~p~n"
	   "SubnetMask: ~p~n"
	   "DefaultRouter0: ~p~n"
	   "DNS_IpAddress: ~p~n"
	   "SMRS_address: ~p~n"
	   "SMRS_User_Name: ~p~n"
	   "SMRS_password: ~p~n"
	   "RbsSumFilePath: ~p~n", 
	   [Node_Str, TN_port, VlanId_Str, OAM_IpAddress,SubnetMask,DefaultRouter0,
	    DNS_IpAddress, SMRS_address,SMRS_User_Name,SMRS_password,RbsSumFilePath]),


    "<RbsSiteInstallationFile
         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
         xsi:noNamespaceSchemaLocation=\"SiteInstallation.xsd\">
       <Format revision=\"L\"/>
       <InstallationData
	logicalName=\""++Node_Str++"\"
            vlanId=\""++VlanId_Str++"\"
            rbsIntegrationCode=\"%rbsIntegrationCode%\"
            tnPort=\""++TN_port++"\">
          <OamIpConfigurationData
	ipAddress=\""++OAM_IpAddress++"\"
              subnetMask=\""++SubnetMask++"\"
              defaultRouter0=\""++DefaultRouter0++"\">
            <DnsServer
	ipAddress=\""++DNS_IpAddress++"\"/>
          </OamIpConfigurationData>
	<SmrsData
	address=\""++SMRS_address++"\"
              userName=\""++SMRS_User_Name++"\"
              password=\""++SMRS_password++"\"
             summaryFilePath=\"" ++ RbsSumFilePath ++ "\"
             />
	</InstallationData>
	</RbsSiteInstallationFile>".


%%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_sif_ipv6_trusted(Config, Sifname, Summaryname, Node, TN_port) -> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_sif_ipv6_trusted(Config, Sifname, Summaryname, Node, TN_port) ->
    ct:log("Sifname: ~p~n"
	   "Summaryname: ~p~n"
	   "Node: ~p~n", [Sifname, Summaryname, Node]),

    Priv_dir = ?config(priv_dir, Config),
    ct:pal("Priv_dir: ~p", [Priv_dir]),
    os:cmd("chmod 777 "++Priv_dir), % else permission.

    Priv_dir_path_to_sif = filename:join(Priv_dir, Sifname),
    ct:pal("Priv path to SIF: ~p", [Priv_dir_path_to_sif]),

    %% Temporary, change when "new labb"??
    RbsSumFilePath = get_summary_dir(Summaryname),
    ct:pal("RbsSumFilePath: ~p", [RbsSumFilePath]),

    SIF_str = sif_ipv6_string(RbsSumFilePath, Node, TN_port),
    ct:pal("SIF ipv6:~n", []),
    ct:log(re:replace(SIF_str,"<","\\&lt;",[{return,list},global])),

    file:write_file(Priv_dir_path_to_sif, SIF_str),
    ct:pal("Path to ~p: ~n~p~n", [Sifname, Priv_dir_path_to_sif]).

sif_ipv6_string(RbsSumFilePath, Node, TN_port) ->

    [{ssh, OAM_IpAddress}, {vlan, VlanId}, 
     {netmask, SubnetMask}, {gateway, DefaultRouter0}] = ct:get_config({Node, ssh_TN_A_ipv6}),

    Node_Str = atom_to_list(Node),
    VlanId_Str = integer_to_list(VlanId),

    %% Temporary, get from stp.cfg when "new labb"
    %% for all config data below
    DNS_IpAddress = "2001:1b70:6282:b280::148", 
    SMRS_address= "2001:1b70:6282:b280::157" ,   
    %%SMRS_User_Name= "dustest",       
    %%SMRS_password= "dustest",        
    SMRS_User_Name= "",      
    SMRS_password= "",        

    ct:pal("Node: ~p~n"
	   "TN_port:~p~n"
	   "VlanId: ~p~n"
	   "OAM_IpAddress: ~p~n"
	   "SubnetMask: ~p~n"
	   "DefaultRouter0: ~p~n"
	   "DNS_IpAddress: ~p~n"
	   "SMRS_address: ~p~n"
	   "SMRS_User_Name: ~p~n"
	   "SMRS_password: ~p~n"
	   "RbsSumFilePath: ~p~n", 
	   [Node_Str, TN_port, VlanId_Str, OAM_IpAddress,SubnetMask,DefaultRouter0,
	    DNS_IpAddress, SMRS_address,SMRS_User_Name,SMRS_password,RbsSumFilePath]),


    "<RbsSiteInstallationFile
         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
         xsi:noNamespaceSchemaLocation=\"SiteInstallation.xsd\">
       <Format revision=\"L\"/>
       <InstallationData
	logicalName=\""++Node_Str++"\"
            vlanId=\""++VlanId_Str++"\"
            rbsIntegrationCode=\"%rbsIntegrationCode%\"
            tnPort=\""++TN_port++"\">
          <OamIpConfigurationData
	ipAddress=\""++OAM_IpAddress++"\"
              networkPrefixLength=\""++SubnetMask++"\"
              defaultRouter0=\""++DefaultRouter0++"\">
            <DnsServer
	ipAddress=\""++DNS_IpAddress++"\"/>
          </OamIpConfigurationData>
	<SmrsData
	address=\""++SMRS_address++"\"
              userName=\""++SMRS_User_Name++"\"
              password=\""++SMRS_password++"\"
             summaryFilePath=\"" ++ RbsSumFilePath ++ "\"
             />
	</InstallationData>
	</RbsSiteInstallationFile>".


%%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_sif_ipv4_untrusted(Config, Sifname, Summaryname, Node, TN_port) -> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_sif_ipv4_untrusted(Config, Sifname, Summaryname, Node, TN_port) ->

    ct:log("Sifname: ~p~n"
	   "Summaryname: ~p~n"
	   "Node: ~p~n", [Sifname, Summaryname, Node]),

    Priv_dir = ?config(priv_dir, Config),
    ct:pal("Priv_dir: ~p", [Priv_dir]),
    os:cmd("chmod 777 "++Priv_dir), % else permission.

    Priv_dir_path_to_sif = filename:join(Priv_dir, Sifname),
    ct:pal("Priv path to SIF: ~p", [Priv_dir_path_to_sif]),

    %% Temporary, change when "new labb"??
    Node_Str = atom_to_list(Node),
    RbsSumFilePath = "boards/"++Node_Str++"/RbsSummaryFile.xml",
    %%RbsSumFilePath = get_summary_dir(Summaryname),
    ct:pal("RbsSumFilePath: ~p", [RbsSumFilePath]),

    SIF_str = sif_ipv4_untrusted_string(RbsSumFilePath, Node, TN_port),
    ct:pal("SIF untrusted ipv4: ~n", []),
    ct:log(re:replace(SIF_str,"<","\\&lt;",[{return,list},global])),

    file:write_file(Priv_dir_path_to_sif, SIF_str),
    ct:pal("Path to ~p: ~n~p~n", [Sifname, Priv_dir_path_to_sif]).


sif_ipv4_untrusted_string(RbsSumFilePath, Node, TN_port) ->

    [{ssh,Inner_IpAddress}, {ssh_outer, Out_IpAddress}, {netmask_outer, _Netmask_Outer}, 
     {vlan, VlanId}, {gateway_outer, Gateway_Outer}] = 
	ct:get_config({Node, ssh_TN_A_ipv4_ipsec}),

    Node_Str = atom_to_list(Node),
    VlanId_Str = integer_to_list(VlanId),

    %% Temporary, get from stp.cfg when "new labb"
    %% for all config data below
    Out_SubnetMask= "255.255.255.0",  

    Outer_Dns_Server = "192.168.10.10",   
    Secure_Gateway = "192.168.226.1",     
    FQDN = "dummySecGw.rnd.ericsson.se",
    Inner_Dns_Server = "10.68.101.148", 

    SMRS_address= "10.68.101.150",      
    %% SMRS_User_Name= "dustest",       
    %% SMRS_password= "dustest",        

    %%SMRS_User_Name= "dustest",       
    %%SMRS_password= "AAssff222",        

    SMRS_User_Name= "",       
    SMRS_password= "",        


    ct:pal("Node: ~p~n"
	   "TN_port:~p~n"
	   "VlanId: ~p~n"
	   "Out_IpAddeess: ~p~n"
	   "Netmask_Outer: ~p~n"
	   "Gateway_Outer: ~p~n"
	   "Outer_Dns_Server: ~p~n"
	   "Secure_Gateway: ~p~n"
	   "FQDN: ~p~n"
	   "Inner_IpAddeess: ~p~n"
	   "Inner_Dns_Server: ~p~n"
	   "SMRS_address: ~p~n"
	   "SMRS_User_Name: ~p~n"
	   "SMRS_password: ~p~n"
	   "RbsSumFilePath: ~p~n", 
	   [Node_Str, TN_port, VlanId_Str, Out_IpAddress, Out_SubnetMask, Gateway_Outer,
	    Outer_Dns_Server, Secure_Gateway,FQDN, Inner_IpAddress, Inner_Dns_Server,
	    SMRS_address,SMRS_User_Name,SMRS_password,RbsSumFilePath]),

    "<RbsSiteInstallationFile
         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
         xsi:noNamespaceSchemaLocation=\"SiteInstallation.xsd\">
    <Format revision=\"L\"/>
    <InstallationData
	logicalName=\""++Node_Str++"\"
            vlanId=\""++VlanId_Str++"\"
            rbsIntegrationCode=\"%rbsIntegrationCode%\"
            tnPort=\""++TN_port++"\">
	  <SmrsData
	address=\""++SMRS_address++"\"
              userName=\""++SMRS_User_Name++"\"
              password=\""++SMRS_password++"\"
              summaryFilePath=\"" ++ RbsSumFilePath ++ "\"/>
	</InstallationData>
	<UntrustedNetworkTemporaryConfigurationData>
	<OuterIpConfigurationData 
	ipAddress=\""++Out_IpAddress++"\"
           subnetMask=\""++Out_SubnetMask++"\"
           defaultRouter0=\""++Gateway_Outer++"\">
         <OuterDnsServer 
	ipAddress=\""++Outer_Dns_Server++"\"/>
         <SecGW 
	ipAddress=\""++Secure_Gateway++"\"
	     FQDN=\""++FQDN++"\"/>
        </OuterIpConfigurationData>
	<InnerIpConfigurationData 
	ipAddress=\""++Inner_IpAddress++"\">
           <InnerDnsServer 
	ipAddress=\""++Inner_Dns_Server++"\"/>
         </InnerIpConfigurationData>
	</UntrustedNetworkTemporaryConfigurationData>
	</RbsSiteInstallationFile>".

%%--------------------------------------------------------------------
%% @doc
%% @spec get_summary_dir(File) -> ok
%% @end
%%--------------------------------------------------------------------
get_summary_dir(File) ->
    Nodedir = atom_to_list(ct:get_config({test_nodes,1})),
    ct:log("Nodedir: ~p", [Nodedir]),
    SFTP_path = filename:join(["smrs", Nodedir, File]),
    SFTP_path.

%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_RbsSummaryFile(RbsSummaryFile, Node, TftpBootDir) -> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_RbsSummaryFile(RbsSummaryFile, Node, TftpBootDir) ->
    ct:log("RbsSummaryFilename: ~p~n"
	   "Node: ~p~n"
	   "TftpBootDir: ~p~n", [RbsSummaryFile, Node, TftpBootDir]),

    Tftp_path_to_RbsSummaryFile= filename:join(TftpBootDir, RbsSummaryFile),
    ct:pal("Tftp path to : ~p", [Tftp_path_to_RbsSummaryFile]),

    RbsSummaryFile_str = rbsSummaryFile_string(TftpBootDir),
    ct:pal("RbsSummaryFile: ~n", []),
    ct:log(re:replace(RbsSummaryFile_str,"<","\\&lt;",[{return,list},global])),
    
    file:write_file(Tftp_path_to_RbsSummaryFile, RbsSummaryFile_str),
    ct:pal("Path to ~p: ~n~p~n", [RbsSummaryFile, Tftp_path_to_RbsSummaryFile]).
    
    
rbsSummaryFile_string(TftpBootDir) ->

    SiteBasicFilePath = get_config_files_dir("config_initial.netconf"),
    SiteEquipmentFilePath = get_config_files_dir("config_eqm_dummy.netconf"),
    LicensingKeyFilePath = get_config_files_dir("LKF.xml"),
    LabConfigFilePath = get_config_files_dir("config_lab.sh"),
    InitialSecurityConfigurationFilePath = "%ForIPsec_path, not supported yet%",

    UpgradePackage = get_node_up(TftpBootDir),
    ct:pal("UpgradePackage: ~p ~n", [UpgradePackage]),
    UpgradePackageFilePath = get_config_files_dir(UpgradePackage),


    ct:pal("SiteBasicFilePath: ~p~n"
	   "SiteEquipmentFilePath:~p~n"
	   "LicensingKeyFilePath: ~p~n"
	   "UpgradePackageFilePath: ~p~n"
	   "LabConfigFilePath: ~p~n",
	   [SiteBasicFilePath, SiteEquipmentFilePath, LicensingKeyFilePath, 
	    UpgradePackageFilePath, LabConfigFilePath]),


    "<summary:AutoIntegrationRbsSummaryFile
	xmlns:summary=\"http://www.ericsson.se/RbsSummaryFileSchema\"
	xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
	xsi:schemaLocation=\"http://www.ericsson.se/RbsSummaryFileSchemaSummaryFile.xsd\">
     <Format revision=\"F\"/>
     <ConfigurationFiles
	siteBasicFilePath=\"" ++ SiteBasicFilePath ++ "\"
        siteEquipmentFilePath=\"" ++ SiteEquipmentFilePath ++ "\"
        licensingKeyFilePath=\"" ++ LicensingKeyFilePath ++ "\"
        upgradePackageFilePath=\"" ++ UpgradePackageFilePath ++ "\"
        labConfigFilePath=\"" ++ LabConfigFilePath ++ "\"
        initialSecurityConfigurationFilePath=\"" ++ InitialSecurityConfigurationFilePath ++ "\"/>
      </summary:AutoIntegrationRbsSummaryFile>"
	.


%%--------------------------------------------------------------------
%% @doc
%% @spec get_config_files_dir(File)-> ok
%% @end
%%--------------------------------------------------------------------
get_config_files_dir(File) ->
    Nodedir = atom_to_list(ct:get_config({test_nodes,1})),
    ct:log("Nodedir: ~p", [Nodedir]),
    SFTP_path = filename:join(["smrs", Nodedir, File]),
    SFTP_path.

%%--------------------------------------------------------------------
%% @doc
%% @spec get_node_up(TftpBootDir)-> Up
%% @end
%%--------------------------------------------------------------------
get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 

%%--------------------------------------------------------------------
%% @doc
%% @spec add_base_dir(File)-> Sftppath
%% @end
%%--------------------------------------------------------------------
add_base_dir(File) ->
    Nodedir = atom_to_list(ct:get_config({test_nodes,1})),
    ct:pal("Nodedir: ~p", [Nodedir]),
    Sftppath = filename:join(["boards", Nodedir, File]),
    Sftppath.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
httpc_request_lmt_integration(Config, Method, DoAction, Protocol, Port)->
    httpc_request_lmt_integration(Config, Method, DoAction, Protocol, Port, dummy, dummy, 
				   dummy).
httpc_request_lmt_integration(Config, Method, DoAction, Protocol, Port, XmlFile) ->
    httpc_request_lmt_integration(Config, Method, DoAction, Protocol, Port, dummy, dummy, 
				  XmlFile).
httpc_request_lmt_integration(Config, Method, DoAction, Protocol, Port, EsiLogPath, EsiName) ->
    httpc_request_lmt_integration(Config, Method, DoAction, Protocol, Port, EsiLogPath, 
				  EsiName, ?DefaultXmlFile).
httpc_request_lmt_integration(Config, Method, DoAction, Protocol, Port, EsiLogPath, EsiName, 
			      XmlFile) ->

    ct:log("Config: ~p", [Config]),
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),

    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),


    MyTime = integer_to_list(ms_since_1970()),

    %% Temporary, get from stp.cfg when "new labb"
    {SmrsUsername, SmrsPassword} =
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

    timer:sleep(2000),
    start_needed_applications(Protocol),
    timer:sleep(2000),

    ct:log("BoardType: ~p~n"
	   "HW: ~p~n"
	   "IP: ~p~n"
	   "Username: ~p~n"
	   "Password: ~p~n"
	   "MyTime: ~p~n",
	   [BOARDTYPE,
	    HW,
	    IP,
	    SmrsUsername,
	    SmrsPassword,
	    MyTime]),


    ct:log("Protocol: ~p, Port: ~p", [Protocol, Port]),
    
    case DoAction of

	%%%%
	%% Download, Integrate and cancel
	%%%%

	"Download" ->
       
	    %%SIF_file =  proplists:get_value(sif, Config),
	    Priv_dir = ?config(priv_dir, Config),
	    Sif_path = filename:join(Priv_dir, XmlFile),

	    {ok, Binary}=file:read_file(Sif_path),
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
	    	"DoDownload=Download"; 

	"Integrate" ->

	    Body = 
		"sifcontent="++"&"++
		"filename="++"&"++
		"smrsusername="++"&"++
		"smrspassword="++"&"++
		"localfile="++"&"++
		"utcMs="++MyTime++"&"++
		"Do"++DoAction++"="++DoAction;

	"Cancel"->

	    Body = 
		"sifcontent="++"&"++
		"filename="++"&"++
		"smrsusername="++"&"++
		"smrspassword="++"&"++
		"localfile="++"&"++
		"utcMs=0"++"&"++		
		"Do"++DoAction++"="++DoAction;

        %%%%
	%% BoardRestore and FactoryReset and HardFactoryReset
        %%%%
	Val when Val == "BoardRestore";
		 Val == "FactoryReset";
		 Val == "HardFactoryReset"->
	    Body = "Do"++DoAction++"="++DoAction;
	%%%%
	%% Export and ESI
	%%%%
	Val when Val == "Export";
		 Val == "ESI" ->
	    case DoAction of
		"Export" ->
		    Path = "filename=";
		"ESI" -> 
		    Path = "directory="
	    end,
	    FileName = EsiName,
	    %% EsiLogPath = ?config(priv_dir,Config),
	    os:cmd("chmod 777 "++ EsiLogPath),
	    ct:pal("EsiLogPath : ~p", [EsiLogPath]),
	    Body = "host="++SftpHost++"&"++
		"username="++Username++"&"++
		"password="++Password++"&"++
		Path++EsiLogPath++FileName++"&"++
		"Do"++DoAction++"="++DoAction;
	    
	_Other ->
	    %% Url = dummy,
	    Body = dummy,
	    ct:fail("### No Valid DoAction not found")
    end,

    Url = get_url(Protocol, Port, IP, DoAction),

    ct:pal("DoAction : ~p", [DoAction]),
    ct:pal("url : ~p", [Url]),
    ct:pal("Body : ~p", [Body]),

    timer:sleep(2000),

    %% {ok, Reply} = httpc:request(Method, {Url, [], [], Body}, [], []),
    %% ct:log("Reply : ~p", [Reply]),
    %% {{_,200,"OK"}, _ReplyA, _ReplyB} = Reply,

    %% A try to make request more robust
    Reply = send_httpc_request(Method, Url, Body, 600000),
    {{_,200,"OK"}, _ReplyA, _ReplyB} = Reply,

    timer:sleep(2000),
    %% inets:stop(), %% För httpc
    stop_needed_applications(Protocol),
    timer:sleep(2000),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec check_for_printout_in_ailog(ExpectStr)-> ok
%% @end
%%--------------------------------------------------------------------
check_for_printout_in_ailog(ExpectStr)->
    ct:log("### check for printout in ailog.txt",[]),
    check_for_printout_in_ailog(ExpectStr, 480000).

check_for_printout_in_ailog(_ExpectStr, Timeout) when Timeout < 0 ->
    ct:fail("check for printout in ailog.txt not rcvd within max timeout.");

check_for_printout_in_ailog(ExpectStr, Timeout)->
    case request_for_ailog() of
	{ok, UrlResStr} ->	  
	    Options = [global, {capture, all, binary}],
	    case re:run(UrlResStr, ExpectStr, Options) of
		{match, [[Value]]} ->
		    ct:log("match value: ~p ~n",[Value]),
		    ok;
		nomatch ->
		    ct:log("nomatch ~n",[]),
		    timer:sleep(3000),
		    check_for_printout_in_ailog(ExpectStr, Timeout - 3000)  
	    end;
	{error, _UrlResStr} ->
	    ct:pal("Could not request ailog.txt, sleep and check again.~n~p",[_UrlResStr]),
	    timer:sleep(3000),
	    check_for_printout_in_ailog(ExpectStr, Timeout - 3000)
    end.


request_for_ailog()-> 

    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    
    start_needed_applications(?DefaultProt),

    HttpsUrl= "https://"++IP++"/ailog.txt",

    {Answere, UrlResStr} =  
	case httpc:request(HttpsUrl) of
	    {ok,{{_,200,"OK"}, _ReplyHttpsA, ReplyHttpsB}} ->
		ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		{ok, ReplyHttpsB};
	    {error, R} ->
		{error, R}
	end,

    stop_needed_applications(?DefaultProt),
    ct:log("Answer from request ailog: ~n~p",[Answere]),
    {Answere, UrlResStr}.
 
%%--------------------------------------------------------------------
%% @doc
%% @spec get_tn_port_capitel(BoardType)-> ok
%% @end
%%--------------------------------------------------------------------
get_tn_port_capitel(BoardType)->
    TN_port = 
	case  BoardType of
	    BoardType when BoardType == "dus5301";
			   BoardType == "dus3301";
			   BoardType == "dus6303";
			   BoardType == "dus6502" ->
		"TN_C";
	    BoardType when BoardType == "dus5201";
			   BoardType == "dus3201";
			   BoardType == "tcu0401";
			   BoardType == "c608"->
		"TN_A"
	end,
    ct:log("TN_port capitel: ~p", [TN_port]),
    TN_port.

%%--------------------------------------------------------------------
%% @doc
%% @spec get_tn_port_lower_case(BoardType)-> ok
%% @end
%%--------------------------------------------------------------------
get_tn_port_lower_case(BoardType)->
    TN_port = 
	case  BoardType of
	    BoardType when BoardType == "dus5301";
			   BoardType == "dus3301";
			   BoardType == "dus6303";
			   BoardType == "dus6502" ->
		"tnc";
	    BoardType when BoardType == "dus5201";
			   BoardType == "dus3201";
			   BoardType == "tcu0401";
			   BoardType == "c608"->
		"tna"
	end,
    ct:log("TN_port lower case: ~p", [TN_port]),
    TN_port.

%%--------------------------------------------------------------------
%% @doc
%% @spec  get_interface_id(BoardType, TN_port)-> ok
%% @end
%%--------------------------------------------------------------------
get_interface_id(BoardType, TN_port)->

    Eth_mpping_list = get_eth_mpping_list(),

    {value, {BoardType, MappingList}} = lists:keysearch(BoardType,1, Eth_mpping_list),
    {value,{TN_port, InterfaceId}} = lists:keysearch(TN_port,1, MappingList),

    ct:log("InterfaceId: ~p", [InterfaceId]),
    InterfaceId.


%%--------------------------------------------------------------------
%% @doc
%% @spec get_board_interfaces(BoardType)->  ok
%% @end
%%--------------------------------------------------------------------
get_board_interfaces(BoardType)->

    Eth_mpping_list = get_eth_mpping_list(),
    {value, {BoardType, MappingList}} = lists:keysearch(BoardType,1, Eth_mpping_list),

    MappingList.


%%--------------------------------------------------------------------
%% @doc
%% @spec  get_eth_mpping_list()-> ok
%% @end
%%--------------------------------------------------------------------
get_eth_mpping_list()->

    Eth_mpping_list = [{"dus3201", [{"tna", "adkgmac17"},
				    {"tnb", "adkxgmac16"},
				    {"tnc", "adkgmac1"}]},
		       {"dus5201", [{"tna", "adkgmac17"},
				    {"tnb", "adkxgmac16"},
				    {"tnc", "adkxgmac1"}]},
		       {"dus5301", [{"tna", "adkxgmac16"},
				    {"tnb", "adkxgmac17"},
				    {"tnc", "adkgmac4"},
				    {"tnd", "adkgmac2"}]},
		       {"dus3301", [{"tna", "adkxgmac16"},
				    {"tnb", "adkxgmac17"},
				    {"tnc", "adkgmac4"},
				    {"tnd", "adkgmac2"}]},
		       {"dus6303", [{"tna", "adkxgmac16"},
				    {"tnb", "adkxgmac17"},
				    {"tnc", "adkgmac2"}]},
		       {"dus6502", [{"tna", "adkxgmac16"},
				    {"tnb", "adkxgmac17"},
				    {"tnc", "adkgmac2"}]},
		       {"tcu0401", [{"tna", "adkxgmac0"},
				    {"tnb", "adkxgmac1"},
				    {"tnc", "adkxgmac16"},
				    {"tnd", "adkxgmac17"},
				    {"tne", "adkxgmac32"},
				    {"tnf", "adkgmac33"},
				    {"tng", "adkgmac48"},
				    {"tnh", "adkgmac49"},
				    {"tnj", "adkgmac65"},
				    {"tnk", "adkgmac80"},
				    {"tnl", "adkgmac81"},
				    {"tnm", "adkgmac96"},
				    {"tnn", "adkgmac97"}]},
		       {"c608", [{"tna", "adkxgmac0"},
				 {"tnb", "adkxgmac1"},
				 {"tnc", "adkxgmac16"},
				 {"tnd", "adkxgmac17"},
				 {"tne", "adkxgmac32"},
				 {"tnf", "adkgmac33"},
				 {"tng", "adkgmac48"},
				 {"tnh", "adkgmac49"},
				 {"tnj", "adkgmac64"},
				 {"tnk", "adkgmac65"},
				 {"tnl", "adkgmac80"},
				 {"tnm", "adkgmac81"},
				 {"tnn", "adkgmac96"},
				 {"tnp", "adkgmac97"},
				 {"tnr", "adkgmac112"},
				 {"tns", "adkgmac113"}]}
		      ],

    Eth_mpping_list.

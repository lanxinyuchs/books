%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aic_curl.erl %
%%% @author eransbn
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R4A/5
-module(aic_curl).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/5').
-date('2016-03-01').
-author('eransbn').
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2015-05-06 eransbn     Created
%%% R3A/3      2015-05-28 eransbn     fetch_esi corrected
%%% R3A/4      2015-06-02 eransbn     fetch_esi improvement
%%% R3A/4      2015-12-14 eransbn     Add poll_all_ai_esi
%%% ----------------------------------------------------------
-export([factory_reset/0,
	 hard_factory_reset/0,
	 board_restore/0,
	 fetch_fake_esi/1,
	 fetch_esi/1,
	 fetch_esi_nl/1,
	 install_sw/0,
	 nl_upgrade/0,
	 poll_all_ai_esi/2
%%	 take_out_node_from_pool()
]).
-define(CURLRSPOK, "HTTP/1.1 200").
-define(CURLRSPLIST, [?CURLRSPOK, "HTTP/1.1 403 Forbidden","couldn't connect to host"]).

%% @doc factory_reset() -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host"| "unknown"
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end
factory_reset()->
    ct:log("Sending Factory Reset"),
    Hw = ct:get_config({test_nodes,1}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    CmdHttps =  "curl -v -k --noproxy " ++ IP ++ " --data \"DoFactoryReset=FactoryReset"++ "\" "	
	"https://" ++ IP ++ "/cgi-bin/aicGui:post",
    CmdHttp =  "curl -v -k --noproxy " ++ IP ++ " --data \"DoFactoryReset=FactoryReset"++ "\" "	
	"http://" ++ IP ++ ":8080/cgi-bin/aicGui:post",

    timer:sleep(1000),
    Response = send_curl_comand(CmdHttps),
    case  check_curl_response(Response) of
	{error, "couldn't connect to host"} -> 
	    ct:log("Try with http instead of https"),
	    Response2 = send_curl_comand(CmdHttp),
	    check_curl_response(Response2);
	Response3 -> Response3
    end.
%% @doc hard_factory_reset() -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host"| "unknown"
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end
hard_factory_reset()->
    ct:log("Sending hard Factory Reset"),
    Hw = ct:get_config({test_nodes,1}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    CmdHttps =  "curl -v -k --noproxy " ++ IP ++ " --data \"DoHardFactoryReset=HardFactoryReset"++ "\" "	
	"https://" ++ IP ++ "/cgi-bin/aicGui:post",
    CmdHttp =  "curl -v -k --noproxy " ++ IP ++ " --data \"DoHardFactoryReset=HardFactoryReset"++ "\" "	
	"http://" ++ IP ++ ":8080/cgi-bin/aicGui:post",

    timer:sleep(1000),
    Response = send_curl_comand(CmdHttps),
    case  check_curl_response(Response) of
	{error, "couldn't connect to host"} -> 
	    ct:log("Try with http instead of https"),
	    Response2 = send_curl_comand(CmdHttp),
	    check_curl_response(Response2);
	Response3 -> Response3
    end.
%% @doc board_restore() -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host"| "unknown"
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end
board_restore()->
    ct:log("Sending Board Restore"),
    Hw = ct:get_config({test_nodes,1}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    CmdHttps =  "curl -v -k --noproxy " ++ IP ++ " --data \"DoBoardRestore=BoardRestore"++ "\" "	
	"https://" ++ IP ++ "/cgi-bin/aicGui:post",
    CmdHttp =  "curl -v -k --noproxy " ++ IP ++ " --data \"DoBoardRestore=BoardRestore"++ "\" "	
	"http://" ++ IP ++ ":8080/cgi-bin/aicGui:post",
    timer:sleep(1000),
    Response = send_curl_comand(CmdHttps),
    case  check_curl_response(Response) of
	{error,"couldn't connect to host"} -> 
	    ct:log("Try with http instead of https"),
	    Response2 = send_curl_comand(CmdHttp),
	    check_curl_response(Response2);
	Response3 -> Response3
    end.

%% @doc install_sw() -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host" | "unknown"
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end       
install_sw()->
    ct:log("Install SW"),
    Hw = ct:get_config({test_nodes,1}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    SftpAiInstallDir = ct:get_config({Hw,sftp_ai_install_dir}),
    SftpAiInstallDir2 = re:replace(SftpAiInstallDir,"/","%2F",[{return,list},global]),
    MyTime = integer_to_list(ms_since_1970()),
    CmdHttps = "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&filename=" ++ SftpAiInstallDir2 ++ "%2F"++ "RbsSummaryFile.xml" ++ "&DoIntegrate=Integrate" ++
	"&utcMs="    ++ MyTime ++ "\" "
	"https://" ++ IP ++ "/cgi-bin/nl_gui:post",
    CmdHttp = "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&filename=" ++ SftpAiInstallDir2 ++ "%2F"++ "RbsSummaryFile.xml" ++ "&DoIntegrate=Integrate" ++
	"&utcMs="    ++ MyTime ++ "\" "
	"http://" ++ IP ++ ":8080/cgi-bin/nl_gui:post",

    timer:sleep(1000),
    Response = send_curl_comand(CmdHttps),
    case  check_curl_response(Response) of
	{error, "couldn't connect to host"} -> 
	    ct:log("Try with http instead of https"),
	    Response2 = send_curl_comand(CmdHttp),
	    check_curl_response(Response2);
	Response3 -> Response3
    end.

%% @doc install_upgrade() -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host"| "unknown"
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end     
nl_upgrade()->
    ct:log("Update Network Loader from UP"),
    Hw = ct:get_config({test_nodes,1}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),					
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    SftpAiInstallDir = ct:get_config({Hw,sftp_ai_install_dir}),
    SftpAiInstallDir2 = re:replace(SftpAiInstallDir,"/","%2F",[{return,list},global]),
    MyTime = integer_to_list(ms_since_1970()),
    CmdHttps = "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&filename=" ++ SftpAiInstallDir2 ++ "%2F"++"RbsSummaryFile.xml.nl"  ++ "&DoIntegrate=Integrate" ++
	"&utcMs="    ++ MyTime ++ "\" "
	"https://" ++ IP ++ "/cgi-bin/nl_gui:post",
    CmdHttp = "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&filename=" ++ SftpAiInstallDir2 ++ "%2F"++"RbsSummaryFile.xml.nl"  ++ "&DoIntegrate=Integrate" ++
	"&utcMs="    ++ MyTime ++ "\" "
	"http://" ++ IP ++ ":8080/cgi-bin/nl_gui:post",
    timer:sleep(1000),
   
    Response = send_curl_comand(CmdHttps),
    case  check_curl_response(Response) of
	{error, "couldn't connect to host"} ->
	    ct:log("Try with http instead of https"),
	    Response2 = send_curl_comand(CmdHttp),
	    check_curl_response(Response2);
	Response3 -> Response3
    end.
%% @doc fetch_fake_esi(Config) -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host"| "unknown"| "no esi""
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end     
fetch_fake_esi(Config)->
    ct:log("Export AutoIntegration log's"),
    Hw = ct:get_config({test_nodes,1}),
    FileName = "fakeEsi",
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    EsiLogPath = ?config(priv_dir,Config),
    os:cmd("chmod 777 "++ EsiLogPath),
    EsiLogPath2 = re:replace(EsiLogPath,"/","%2F",[{return,list},global]),
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    Cmd =  "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&filename=" ++ EsiLogPath2 ++ FileName ++ "&DoExport=Export"++ "\" "
	"https://" ++ IP ++ "/cgi-bin/nl_gui:post",
    timer:sleep(1000),
    ct:log("~s",[Cmd]),
    Response = os:cmd(Cmd),
    ct:log(re:replace(Response,"<","\\&lt;",[{return,list},global])),

    case check_curl_response(Response) of
	ok ->
	    case  poll_esi_transfer(EsiLogPath,FileName, true) of
		ok -> create_fake_esi_dir(FileName ++ "_ericsson", EsiLogPath),
		      create_fake_esi_dir(FileName ++ "_prev_ericsson", EsiLogPath),
		      S =  io_lib:format("<a href=\"~s\">~s</a>", [EsiLogPath, "Fake Esi dir"]),
		      ct:log(S,[]),
		      ok;
		R -> ct:log("~p",[R])
	    end;
	Rsp ->
	    ct:log("No fake esi because curl response ~p",[Rsp]), 
	    Rsp
    end.
%% @doc fetch_esi(Config) -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host"| "unknown | "no esi""
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end           
fetch_esi(Config)->
    ct:log("Export ESI"),
    Hw = ct:get_config({test_nodes,1}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    EsiLogPath = ?config(priv_dir,Config),
    os:cmd("chmod 777 "++ EsiLogPath),
    EsiLogPath2 = re:replace(EsiLogPath,"/","%2F",[{return,list},global]),
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    Cmd =  "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&directory=" ++ EsiLogPath2  ++ "&DoESI=ESI"++ "\" "
	"https://" ++ IP ++ "/cgi-bin/aicGui:post",
    timer:sleep(1000),
    ct:log("~s",[Cmd]),
    Response = os:cmd(Cmd),
    ct:log(re:replace(Response,"<","\\&lt;",[{return,list},global])),
    case check_curl_response(Response) of 
	ok ->
	   case  poll_esi_transfer(EsiLogPath,"", false) of
	       ok ->
		   unpack_esi(EsiLogPath),
		   ok;
	       R -> ct:log("~p",[R]),
		    R
	   end;
	Rsp ->
	    ct:log("No esi because curl response ~p",[Rsp]), 
	    Rsp
    end.
%% @doc fetch_esi_nl(Config) -> ok | {error, Reason}
%% atom = atom()
%% String = string()
%% Reason = "HTTP/1.1 403 Forbidden"| "couldn't connect to host"| "unknown | "no esi""
%% "HTTP/1.1 403 Forbidden" = wrong state can't do factory reset (probably in NL state).
%% "unknown" = Conntact lib author.
%% Example: 
%% @end           
fetch_esi_nl(Config)->
    ct:log("Export ESI form NL"),
    Hw = ct:get_config({test_nodes,1}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    EsiLogPath = ?config(priv_dir,Config),
    os:cmd("chmod 777 "++ EsiLogPath),
    EsiLogPath2 = re:replace(EsiLogPath,"/","%2F",[{return,list},global]),
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    Cmd =  "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&directory=" ++ EsiLogPath2  ++ "&DoESI=ESI"++ "\" "
	"https://" ++ IP ++ "/cgi-bin/nl_gui:post",
    timer:sleep(1000),
    ct:log("~s",[Cmd]),
    Response = os:cmd(Cmd),
    ct:log(re:replace(Response,"<","\\&lt;",[{return,list},global])),
    case check_curl_response(Response) of 
	ok ->
	   case  poll_esi_transfer(EsiLogPath,"", false) of
	       ok ->
		   unpack_esi(EsiLogPath),
		   ok;
	       R -> ct:log("~p",[R]),
		    R
	   end;
	Rsp ->
	    ct:log("No esi because curl response ~p",[Rsp]), 
	    Rsp
    end.
poll_esi_transfer(EsiLogPath, FileName, FileNameBool)->
    poll_esi_transfer(EsiLogPath, FileName, FileNameBool, 100).
poll_esi_transfer(EsiLogPath, _FileName, _FileNameBool, 0) ->
    ct:log("No esi found in dir:~n~p",[EsiLogPath]),
    {error, "no esi"};
poll_esi_transfer(EsiLogPath, FileName, FileNameBool, NrRepeat)->
    timer:sleep(5000),
    case FileNameBool of
	true -> case  string:str(os:cmd("ls " ++ filename:join([EsiLogPath , FileName])),"No such file or directory") of
		    0 -> ok;
		    _ -> poll_esi_transfer(EsiLogPath, FileName, FileNameBool, NrRepeat -1)
		    
		end;
	false -> case string:str(os:cmd("ls " ++  filename:join([EsiLogPath , "esi*tar*"])),"No such file or directory") of
		     0 -> ok, timer:sleep(10000); %%Improve this
		     _ -> poll_esi_transfer(EsiLogPath, FileName, FileNameBool, NrRepeat -1)
		 end
    end.
			       
			     
%% %%this function should be somewhere else
%% take_out_node_from_pool(Config)->
%%     Hw = ct:get_config({test_nodes,1}),
%%     LogPath = ?config(priv_dir,Config),
%% Cmd =   
%% "curl https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job/set_faulty_node_offline/buildWithParameters?token=nisse\&NODE=" ++ Hw ++"\&STATE=offline\&TEST_PATH="https://rbs-rde.rnd.ki.sw.ericsson.se/proj/rcs-tmp/stps/tcu018/CXS101553-R3H22/pre_dc_wr6_sec_tcu03_spec_1/config_pre_dc_wr6_sec_tcu03_spec_1/"


%%========================================================================
%%       Internal functions
%%========================================================================

check_curl_response(Response)->  
    check_curl_response(Response, ?CURLRSPLIST).
check_curl_response(_Response, []) ->
   {error, "unknown"};
check_curl_response(Response, [Check|Rest])->
    case  string:str(Response, Check) >= 1 of
	true->
	   case string:str(Response, ?CURLRSPOK) > 1 of
	      true -> ok;
	       false -> {error, Check}
	   end;
	false -> check_curl_response(Response, Rest)
    end.


%%   Ahora = calendar:datetime_to_gregorian_seconds({date(), time()}),
%%   Jimilives = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
%%   1000*(Ahora-Jimilives).
ms_since_1970() ->
    list_to_integer(string:strip(os:cmd("date -u +%s"),right,$\n)) * 1000.

%%Create dir and decrypt
create_fake_esi_dir(FileName, EsiLogPath)->
    case filelib:is_file(filename:join([EsiLogPath , FileName])) of
	true ->
	    FakeEsiDir = EsiLogPath ++  FileName ++ "_dir", 
	    os:cmd("mkdir " ++ FakeEsiDir),
	    ct:log("Create dir  ~p",[FakeEsiDir]),
	    Out = EsiLogPath ++ FileName ++".tgz",
	    FakeEsiDir = EsiLogPath ++ FileName  ++ "_dir", 
	    %%Crypro is started from hook
	    case  file:write_file(Out, crypto:block_decrypt(aes_cfb128,<<"1234567890ABCDEF">>,<<"1234567890ABCDEF">>, 
							    element(2, file:read_file(EsiLogPath ++ FileName)))) of
		ok -> os:cmd("tar -xf " ++ Out ++" --gunzip --directory=" ++ FakeEsiDir );
		{error, Reason} -> ct:log("Error ~p",[Reason]);
		Other -> ct:log("Other ~p",[Other])
		end;
	false -> ct:log("No file ~p",[FileName])
    end,
    ok.

unpack_esi(EsiLogPath)->
    Cmd = "tar -xf " ++  EsiLogPath ++ "esi*tar*" ++ " --gunzip --directory=" ++ EsiLogPath,
    ct:log("Cmd ~s",[Cmd]),
    case os:cmd(Cmd) of
	[]-> 
	    S =  io_lib:format("<a href=\"~s\">~s</a>", [EsiLogPath, "Esi log path"]),
	    ct:log(S,[]);
	%%improve this
	Reply -> ct:log("Not able to unpack esi because ~s",[Reply]), 
		  S =  io_lib:format("<a href=\"~s\">~s</a>", [EsiLogPath, "Esi log path"]),
		 ct:log(S,[])
    end.
		 

send_curl_comand(Cmd)->
    ct:log("~s",[Cmd]),
    Response = os:cmd(Cmd),
    ct:log(re:replace(Response,"<","\\&lt;",[{return,list},global])),
    Response.

poll_all_ai_esi(_Config, [])->
    ok;
poll_all_ai_esi(Config, TimerToPolSec) ->
    case fetch_esi(Config) of
	ok -> ok;
	_RespEsi  -> 
	    case fetch_esi_nl(Config) of
		ok -> ok;
		_RespEsiNl  ->
		    case fetch_fake_esi(Config) of
			ok -> ok;
			_RespEsiFake -> 
			    timer:sleep(10000),
			    poll_all_ai_esi(Config, TimerToPolSec - 10 )
		    end
	    end
    end.

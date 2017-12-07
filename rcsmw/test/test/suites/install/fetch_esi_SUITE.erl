%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	fetch_esi_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R11A/1
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end


-module(fetch_esi_SUITE).
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
%%% R1A/1      2014-03-25 eransbn     Created
%%% R1A/5      2014-03-25 eransbn     Suport for vc card
%%% R2A/9      2015-02-12 eransbn     Read error and crash report from erlang log
%%% R3A/1      2015-03-11 etxkols     Increased netconf poll time to 5 minutes
%%% R3A/2      2015-03-12 etxkols     Fixed netconf polling
%%% R3A/3      2015-03-12 etxkols     Prepare for two labs
%%% R3A/3      2015-03-26 eransbn     More check for vc cards
%%% R3A/4      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-09-25 etxarnu     Updated check_esi_for_pmd
%%% R4A/2      2015-09-25 eransbn     More try when open netconf
%%% R4A/3      2015-10-05 etxkols     now/0 depricated, replaced with os:timestamp/0
%%% R4A/5      2015-10-20 eransbn     Updated check of pmds for vc cards (2 dir in /rcs/dumps)
%%% R4A/6      2015-10-21 eransbn     Updated check of pmds for vc cards 
%%% R4A/7      2015-11-12 etxpejn     Updated transfer_esi_unpacked to fit also for dual
%%% R4A/8      2015-12-14 eransbn     Add poll of ai esi (end_per_testcase)
%%% R4A/9      2016-02-01 eransbn     Add poll of ai esi (in poll_connection)
%%% R4A/10     2016-02-12 etxkols     ipv6
%%% R5A/2      2016-04-19 eransbn     Html tags changed in CT
%%% R6A/2      2016-04-19 eransbn     Load LKF on sim to disable encrypted esi
%%% R6A/3      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% R6A/4      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
%%% R11A/1     2017-10-17 etxkivri    A try to make it more robust when open nc.
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
	 transfer_esi_unpacked/1]).

%% -define(SFTP_HOST, "10.68.200.11").
%% -define(USER, "mauve").
%% -define(PSWD, "dilbert").
%% -define(SFTP_URL, "sftp://"++?USER++"@"++?SFTP_HOST).
-define(ERROR_STRING, "\"ERROR REPORT|CRASH REPORT\"").
-define(LKF_DIR, "/proj/rcs/LKF_hands_off/").
-define(LKF_FILE, "licensingKeyFile.xml").
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
   
    case check_if_vc_board() of
	"yes" ->
	    [{ct_hooks, [{rct_htmllink,[]},
			 {cth_conn_log,[]},
			 {rct_consserv,cs1},
			 {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}},
			 {rct_netconf, {nc1, man_auth}}
			]}];
	_  ->
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_netconf,nc1},
			 {cth_conn_log,[]},
			 {rct_consserv,cs1},
			 {rct_cli, {cli, [manual_connect]}},
			 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
			 {rct_core,[]}
			]}]
    end.


%% @hidden
init_per_suite(Config) ->
    poll_connection(nc1, 180, Config),
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc_1, os, putenv, ["FAKE_VNFM", ""], 10000);
        _ ->
            rct_rpc:call(rpc_1, os, unsetenv, ["FAKE_VNFM"], 10000)
    end,
    Config.
%% @hidden
end_per_suite(_Config) ->
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc_1, os, unsetenv, ["FAKE_VNFM"], 10000);
        _ ->
            ok
    end,
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    %%Need to load lkf to disable encrypted esi
    case os:getenv("SIM_OR_TARGET") of
	"sim" -> load_lkf(Config);
	_ -> ok
    end,
    
	    
    
    
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
	ok -> 
	    ct:log("status ~p",[proplists:get_value(tc_status, Config)]);
	R ->
	    ct:log("test case ~p",[R]),
	    aic_curl:poll_all_ai_esi(Config, 900)

    end,
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [transfer_esi_unpacked].
%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a transfer ESI log from SUT to a sftp server<br/>
%% This TC transfer ESI log from SUT to a sftp server using netconf.<br/>
%% The log file will be stored .../log_private/ and unpacked, that will be created when TC starts.<br/>
%% TC will check progressreport using netconf and also check that correct file is transfered correct.
%%
%% @spec transfer_esi_unpacked(Config) -> ok
%% @end
%%--------------------------------------------------------------------

transfer_esi_unpacked(Config)->
    MeId = get_integer_attribute(cli, "show verbose ManagedElement=1", "managedElementId"),
    ct:log("MeId ~p",[MeId]),
    %% MeId = proplists:get_value(meId, Config),
    EsiLogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++EsiLogPath), % else permission.
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [SFTP_URL++EsiLogPath]},
		     {password, [], [Password]}]}
		  ]}]}]},
    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'LogM',
			  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			  [{logMId,[],["1"]},
			   {progressReport,[],[]}
			  ]}]}]},
    %%Check secure card or not
    %% netconf_open(nc1, []),
    ct:log("Check for nc open"),
    do_poll_connection_nc_open(nc1,60),

    ct:log("trig the action transfer_esi using netconf"),
    %% trig the action transfer_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
	{ok, A} ->
	    Return = extract_element(returnValue, A),
	    ct:pal("transfer_esi ~p~n",[Return]);
	Error ->
	    ct:pal("~p~n",[Error]),
	    ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),
    timer:sleep(500),
    case wait_for_progress(progressReport, ProgressFilter,Config) of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    check_esi_log_transfered(EsiLogName, EsiLogPath, true);
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]).



%% @hidden
%% Will check that esi log is tranfered to expected path and also the size of the file.
%% If the size is to small, then maybe it does not consist on anything!
check_esi_log_transfered(EsiLogName, EsiLogPath, UnpackEsiFile) ->
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    {SftpHost2,Inet} = case re:run(SftpHost,":") of
			  {match,_} -> {string:strip(string:strip(SftpHost,right,$]),left,$[), inet6};
			  _         -> {SftpHost,inet}
		      end,
    {ok,ChPid,_ChRef} =
	ssh_sftp:start_channel(SftpHost2,
			       [{inet,Inet},
				{user, Username},
				{password, Password},
				{silently_accept_hosts, true},
				{timeout, 180000}]),

    {ok, DirData} = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
    %% DirData = lists of strings from the directory.

    %% In case dual DU EsiLogName will be several files divided by "; "
    ok = check_esi_log_transfered_cont(string:tokens(EsiLogName, "; "), DirData, EsiLogPath, UnpackEsiFile, ChPid),
    
    ssh_sftp:stop_channel(ChPid),
    ok.


check_esi_log_transfered_cont([], _DirData, _EsiLogPath, _UnpackEsiFile, _ChPid) ->
    ok;
check_esi_log_transfered_cont([Log | RestLog], DirData, EsiLogPath, UnpackEsiFile, ChPid) ->
    case lists:member(Log, DirData) of
	true ->
	    ct:pal("### Esi log file: ~p, exist in \n path : ~p \n",
		   [Log, EsiLogPath]),
	    timer:sleep(100),
	    case UnpackEsiFile of
		true -> 
		    DuDir = lists:nth(2, string:tokens(Log, ".")),
		    UnpackEsiDir = filename:join(EsiLogPath, DuDir),
		    os:cmd("mkdir " ++ UnpackEsiDir),
		    os:cmd("tar -xf " ++ EsiLogPath ++ Log ++" --gunzip --directory=" ++ UnpackEsiDir),
		   %% io:format("<a href=\"~s\">~s</a>", [UnpackEsiDir, "Esi unpacked dir"]),
		  %%  io:format("<a href=\"~s\">~s</a>", [filename:join(EsiLogPath,Log), Log]),
		   format_html("<a href=\"~s\">~s</a>", [UnpackEsiDir, "Esi unpacked dir"]),
		   format_html("<a href=\"~s\">~s</a>", [filename:join(EsiLogPath,Log), Log]),
		    %%check esi log files for errors and pmd  on vc card
		    case check_if_vc_board() of
			"yes" -> 
			    ok = check_esi_for_pmd(UnpackEsiDir, ["rcs/dumps/appdump/","rcs/dumps/pmd/"]),
			    read_esi_log_file(UnpackEsiDir, "rcs/erlang/*", ?ERROR_STRING);
			_-> ok
		    end;
		_-> 
		    io:format("<a href=\"~s\">~s</a>", [filename:join(EsiLogPath, Log), Log])
	    end,
	    {ok, FileInfo} = ssh_sftp:read_file_info(ChPid, EsiLogPath++Log, 3000),
	    %%ct:pal("### Recieved FileInfo: ~p", [FileInfo]),
	    
	    Size = lists:nth(2, tuple_to_list(FileInfo)),
	    %%{file_info, Size, _, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,
	    
	    if Size > 10000 ->
		    true;
	       true  ->
		    ct:pal("### Size of the esi tar file is: ~p. ~n "
			   "It is smaller than expected. ~n "
			   "Unpack the file and ckeck that it look OK. \n",[Size]),
		    ct:fail("Size of the esi log file is to small! check if it "
			    "looks ok after unpack!.")
	    end,
	    check_esi_log_transfered_cont(RestLog, DirData, EsiLogPath, UnpackEsiFile, ChPid);
	false ->
	    ct:fail(" Could not find the ESI log file, on sftp server.")
    end.

%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------

wait_for_progress(Attribute, ProgressFilter, Config) ->
    do_poll_connection_nc_open(nc1, 10),

    {ok, A} = ct_netconfc:get(nc1, ProgressFilter),
    ct_netconfc:close_session(nc1),
    {ok, Report} = extract_element(Attribute, A),
    {ok, State} = extract_element(state, [Report]),
    timer:sleep(1000),
    case State of
	{state, _, ["FINISHED"]} ->
	    ct:log("~p~n",[Report]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} =
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, ["CANCELLED"]} ->
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} =
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, [Current]} ->
	    ct:log("State: ~s~n",[Current]),
	    wait_for_progress(Attribute, ProgressFilter, Config)
    end.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

%%%--------------------------------------------------------------------
%%Node = target
%%Time in second
%%%--------------------------------------------------------------------
poll_connection(NC, Time, Config) ->
    ct:pal("Trying to connect to the target for ~p seconds",[Time]),
    Expire = secs_since_1970() + Time,
    do_poll_connection(NC, Expire, Config).
do_poll_connection(NC, Expire, Config) ->
    case  netconf_open(NC,[]) of
	{ok,_} -> 
	    ct_netconfc:close_session(NC);
	Result-> 
	    case secs_since_1970() > Expire of
		true ->
		    aic_curl:poll_all_ai_esi(Config, 900),
		    ct:fail("Could not connect with netconf, Reason: ~p", [Result]);
		false ->
		    ct:log("Could not connect with netconf, retrying in 5 seconds"),
		    timer:sleep(5000),
		    do_poll_connection(NC, Expire, Config)
	    end
    end.
do_poll_connection_nc_open(NC, Time) ->
    Expire = secs_since_1970() + Time,
    poll_connection_nc_open(NC, Expire).
poll_connection_nc_open(NC, Expire) ->
    case  netconf_open(NC,[]) of
	{ok,_} ->
	    ok ;
	Result-> 
	    case secs_since_1970() > Expire of
		true ->
		    ct:fail("Could not connect with netconf, Reason: ~p", [Result]);
		false ->
		    ct:log("Could not connect with netconf, retrying in 5 seconds"),
		    timer:sleep(5000),
		    poll_connection_nc_open(NC, Expire)
	    end
    end.
secs_since_1970() ->
    {MS,S,_} = os:timestamp(), 
    (MS*1000000) + S.

netconf_open(Session, Param)->
    case check_if_vc_board()  of
	"yes"->  ct_netconfc:open
		   (Session, [{user, "SysAdminTest"},
			      {password, "SysAdminTest"}|Param]);
	_-> ct_netconfc:open(Session,Param)

    end.

	    
get_integer_attribute(Cli,Command, Attribute)->
    ok = rct_cli:connect(Cli),
    Re =  ".*" ++ Attribute ++"=\\\""++"(\\d+).*",
    {_,Reply} =  rct_cli:send(Cli, Command),
    ct:log("Reply ~p",[Reply]),

    case  re:run(Reply, Re, [global,{capture, all, binary}]) of
	{match,[[_,Value]]} -> ok = rct_cli:disconnect(Cli),
			       binary:bin_to_list(Value);
	nomatch  -> rct_cli:disconnect(cli),
		   "1"
    end.

check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

read_esi_log_file(EsiLogPath, LogFileDir, ErrorString) ->
    case os:cmd("egrep " ++  ErrorString  ++ " " ++  filename:join(EsiLogPath, LogFileDir))  of
	[]->
	    ok;
	Error -> ct:fail("Error in erlang log: ~n~p",[Error])
    end.

check_esi_for_pmd(_EsiLogPath, []) ->
    ok;
check_esi_for_pmd(EsiLogPath, [DumpDir|Rest]) ->
    case os:cmd("ls " ++ filename:join(EsiLogPath, DumpDir)) of
	[] -> check_esi_for_pmd(EsiLogPath, Rest);
	Dump -> case string:str(Dump,"cannot access") > 0 of
		    true -> ct:pal("WARNING:~p no such file or directory",[DumpDir]),
			    check_esi_for_pmd(EsiLogPath, Rest);
		    false ->  ct:fail("Dumps found in dir ~p: ~p",[DumpDir, Dump])
		end
    end.

format_html(String,Args) ->
     ct:log(default, 1, String, Args, [no_css]).



load_lkf(_Config) ->
    case rct_rpc:call(rpc_1, lmaI, encrypt_esi_log, [], 10000) of
	false ->
	    ok;
	true  -> [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
		 SFTP_URL = "sftp://"++Username++"@"++SftpHost,
		 install_lkf(SFTP_URL, Password, ?LKF_FILE),
		 LicenseInfo = rct_rpc:call(rpc_1, lmaI, encrypt_esi_log, [], 10000),
		 ct:pal("LicenseInfo: ~p", [LicenseInfo])
    end.



install_lkf(SFTP_URL, Password, LKF) -> 
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    SFTP_DIR = "/proj/rcs-tmp/stps/",
    ct:log("Loading LKF ~p",[?LKF_DIR++LKF]),
    {ok,_} = file:copy(?LKF_DIR++LKF, 
		       SFTP_DIR++JenkinsNode++"/"++LKF),
    FingerprintUpdateable= {'ManagedElement',
			    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			    [{managedElementId,[],["1"]},
			     {'SystemFunctions',
			      [{systemFunctionsId,[],["1"]},
			       {'Lm',
				[{xmlns,"urn:com:ericsson:ecim:LM"}],
				[{lmId,[],["1"]},
				 {fingerprintUpdateable,[],[]}
				]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, A} = ct_netconfc:get(nc1, FingerprintUpdateable),
    ct_netconfc:close_session(nc1),
    {ok,{fingerprintUpdateable,_,FPUpdateable}} = extract_element(fingerprintUpdateable, A),
    
    case FPUpdateable of
	["true"] ->
	    %% Set fingerprint
	    B = {'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],["1"]},
		  {'SystemFunctions',[],
		   [{systemFunctionsId,[],["1"]},
		    {'Lm',
		     [{xmlns,"urn:com:ericsson:ecim:LM"}],
		     [{lmId,[],["1"]},
		      {fingerprint,[],["RCS_MSR"]}
		     ]}]}]},
	    {ok,_} = ct_netconfc:open(nc1,[]),
	    ok = ct_netconfc:edit_config(nc1, running, B),
	    ct_netconfc:close_session(nc1);
	_ ->
	    %% Fingerprint is already set
	    do_nada
    end,
        
    %% Install LKF
    C =  {'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'SystemFunctions',[],
	    [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{xmlns,"urn:com:ericsson:ecim:LM"}],
	      [{lmId,[],["1"]},
	       {'KeyFileManagement', [],
		[{keyFileManagementId,[],["1"]},
		 {installKeyFile,[],
		  [{uri, [], [SFTP_URL++SFTP_DIR++JenkinsNode
			      ++"/"++LKF]},
		   {password, [], [Password]}]}
		]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    ct_netconfc:action(nc1, C),
    ct_netconfc:close_session(nc1),
    ok = wait_until_install_complete(0).

wait_until_install_complete(30) ->
    nok;
wait_until_install_complete(No) ->
    InstallResult = {'ManagedElement',
		     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		     [{managedElementId,[],["1"]},
		      {'SystemFunctions',
		       [{systemFunctionsId,[],["1"]},
			{'Lm',
			 [{xmlns,"urn:com:ericsson:ecim:LM"}],
			 [{lmId,[],["1"]},
			  {'KeyFileManagement',
			   [{keyFileManagementId,[],["1"]},
			    {reportProgress,[],[]}
			   ]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, C2} = ct_netconfc:get(nc1, InstallResult),
    ct_netconfc:close_session(nc1),
    {ok,{_,_,Report}} = extract_element(reportProgress, C2),
    ct:pal("Report: ~p", [Report]),

    {ok,{_,_,Result}} = extract_element(result, Report),
    ct:pal("Result: ~p", [Result]),

    case Result of
	 ["SUCCESS"] ->
	    ok;
	_Else ->
	    timer:sleep(1000),
	    wait_until_install_complete(No +1)
    end.



%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_esi_SUITE.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R9A/R10A/2
%%% 
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end


-module(log_esi_SUITE).
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
%%% R1A/1      2012-08-17 etxivri     Created
%%% R1A/2      2012-08-20 etxivri     increased mem size in the check.
%%% R1A/3      2012-08-20 etxivri     Added a negative test (transfer a esi to non existing url).
%%% R1A/4      2012-08-20 etxivri     corrected a edoc error.
%%% R1A/5      2012-09-11 eblakes     Extended scope of testing and added checks
%%% R1A/6      2012-10-05 etxivri     SFTP_HOST changed to 10.68.200.11
%%% R2A/1      2012-02-11 etxjotj     Removed crypto:stop in end per case
%%% R2A/2      2012-02-11 etxjotj     Updated and changed parameter to uri
%%% R2A/3      2012-03-19 etxkols     Changed ct:pal to ct:log because of Jenkins
%%% R2A/4      2013-03-23 etxkols     Wrong ct:pal format
%%% R2A/5      2013-03-23 etxkols     Added {silently_accept_hosts, true} to ssh_sftp:start_channel
%%% R1A/6      2013-06-17 etxjovp     Add hook rct_logging
%%% R2A/7      2013-03-23 etxkols     Added 500 ms sleep before starting to poll prog reports
%%% R2A/8      2013-10-07 etxkols     Added link to ESI in html log
%%% R2A/11     2014-02-24 eransbn     Added tc transfer_esi_unpacked
%%% R2A/13     2014-03-06 eransbn     added ct_core and netconf pollfunction
%%% R3A/1      2015-02-28 etxkols     Preparation for 2 labs
%%% R3A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-10-09 etxjotj     Granularity TCs
%%% R4A/4      2015-10-12 etxjotj     Logging errors
%%% R4A/5      2015-10-13 etxkols     Change ct:pal to ct:log
%%% R4A/6      2015-11-23 etxjotj     Check if backup remains after transfer
%%% R5A/6      2016-05-12 eransbn     Add tc transfer_encrpted_esi_and_decrypt
%%% R5A/7      2016-08-22 etxkols     Git migration requires that CC paths is not used 
%%% R5A/8      2016-08-22 etxkols     Reverted Git migration changes 
%%% R6A/2      2016-09-13 etxpejn     Removed loading of old patch

%%% R9A/1      2017-02-06 ekurnik     Added ftpes_group
%%% R9A/2      2017-02-15 emarnek     Added wrong_port test
%%% R9A/4      2017-02-28 estjako     Added test folder for ftpes tests
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 transfer_esi/1,
	 transfer_esi_small/1,
	 transfer_esi_large/1,
	 transfer_esi_static/1,
	 transfer_esi_nisse/1,
	 transfer_esi_fail/1,
	 transfer_esi_fail_port/1,
	 transfer_esi_fail_password/1,
	 transfer_esi_unpacked/1,
	 transfer_encrpted_esi_and_decrypt/1,
	 groups/0]).

%% -define(SFTP_HOST, "10.68.200.11").
%% -define(USER, "mauve").
%% -define(PSWD, "dilbert").
%% -define(SFTP_URL, "sftp://"++?USER++"@"++?SFTP_HOST).
-define(LMA_DIR, rct_cc_git_path:find("RCS_TOP", ["LMA/LMA_CNX9013077/test/suites/", "LMA/test/suites/"])).
-define(LOG_ESI, "/home/eransbn/cs/dont_touch/logEsi.beam"). %%TODO should be removed when functionality are in place
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,{nc1, html}},
                 {cth_conn_log,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		 {rct_scp, scp1},
		 {rct_ssh,node1}, %%should be removed when encrypt EsiLog delivered
		 {rct_core,[]},
         ftpes_test_lib:ftpes_hook()
		]}].


%% @hidden
init_per_suite(Config) ->
    poll_connection(nc1, 180),
    crypto:start(),
    ssh:start(),
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    %% sftp by default
    [{ftp_protocol, sftp}, {meId, MeId}|Config].
%% @hidden
end_per_suite(_Config) ->
    ok.

init_per_group(ftpes_group, Config) ->
    rct_ftpes_client:open(), 
    ftpes_test_lib:create_test_folder("LOG"),
    {ok, Folder} = rct_ftpes_client:pwd(),
    FilePath = Folder ++ "/",
    NewConfig = lists:keyreplace(ftp_protocol, 1, Config, {ftp_protocol, ftpes}),
    
    %% enable ftp over tls on the node
    ftpes_test_lib:start_server(rpc_1),
    NewConfig2 = ftpes_test_lib:initialize_nc_tc(NewConfig),
    ftpes_test_lib:enable_ftpes_tls(NewConfig2),
    
    [{file_path, FilePath} | NewConfig2];
init_per_group(_Group, Config) ->
    Config.

end_per_group(ftpes_group, Config) ->
    rct_ftpes_client:rmdir(?config(file_path, Config)),
    ftpes_test_lib:disable_ftpes_tls(),
    ftpes_test_lib:clean_nc_tc(Config),
    rct_ftpes_client:close(),
    ok;
end_per_group(_Group, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    FtpProtocol = ?config(ftp_protocol, Config),
    [{host, Host},{username, Username},{password, Password}] = 
        ftpes_test_lib:get_ftp_config(FtpProtocol),
    Url = atom_to_list(FtpProtocol) ++ "://"++Username++"@"++Host,
    EsiLogPath= get_ftp_file_path(FtpProtocol, Config),
    if FtpProtocol =:= sftp ->
           os:cmd("chmod 777 "++EsiLogPath); % else permission.
    true ->
        ok
    end,
    ct:pal("Executing ~w over ~p~n",[TestCase, FtpProtocol]),
    [{url, Url}, {password, Password}, {esi_log_path, EsiLogPath} | Config].
%% @hidden
end_per_testcase(TestCase, _Config) when TestCase =:= transfer_encrpted_esi_and_decrypt ->
    %%put the default LKF
    Hw = ct:get_config({test_nodes,1}),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
   %% SFTP = "sftp://"++Username++"@"++SftpHost,
    TftpDir = ct:get_config({Hw,tftpboot}),
    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    ct:pal("Need to install LKF with test license"),
    LKF = "LKF.xml",
    %% Add PKI verification for LKF
    rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [off], 10000),
    install_default_lkf(SFTP_URL, Password, LKF,TftpDir),
    LicenseInfo = rct_rpc:call(rpc_1, lmaI, encrypt_esi_log, [], 10000),
    ct:pal("LicenseInfo: ~p", [LicenseInfo]),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {ftpes_group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []} 
    ].
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [transfer_esi, 
     transfer_esi_small, 
     transfer_esi_large, 
     transfer_esi_static,
     transfer_esi_nisse, 
     transfer_esi_fail, 
     transfer_esi_fail_port, 
     transfer_esi_fail_password].

%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a transfer ESI log from SUT to a sftp server<br/>
%% This TC transfer ESI log from SUT to a sftp server using netconf.<br/>
%% The log file will be stored .../log_private/ , that will be created when TC starts.<br/>
%% TC will check progressreport using netconf and also check that correct file is transfered correct.
%%
%% @spec transfer_esi(Config) -> ok
%% @end
%%--------------------------------------------------------------------
transfer_esi(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),
    
    case do_transfer_esi(MeId, Url, Password, EsiLogPath, undefined) of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    Backups = get_all_backups(MeId),
	    [case extract_element(backupName, [Backup]) of
		 {ok, {_, _, ["EsiBackup"++A]}} ->
		     ct:pal("Esi~s present",[A]),
		     ct:fail(esi_backup_present);
		 
		 _ ->
		     ok
	     end||Backup<-Backups],
	    check_esi_log_transfered(EsiLogName, false, Config);
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]).

%%--------------------------------------------------------------------
%% @doc Export esi with granularity small
%% @end
%%--------------------------------------------------------------------


transfer_esi_small(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),
    
    case do_transfer_esi(MeId, Url, Password, EsiLogPath, "small") of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    check_esi_log_transfered(EsiLogName, false, Config);
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]).

%%--------------------------------------------------------------------
%% @doc Export esi with granularity large
%% @end
%%--------------------------------------------------------------------

transfer_esi_large(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),
    
    case do_transfer_esi(MeId, Url, Password, EsiLogPath, "large") of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    check_esi_log_transfered(EsiLogName, false, Config);
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]).

%%--------------------------------------------------------------------
%% @doc Export esi with granularity static
%% @end
%%--------------------------------------------------------------------

transfer_esi_static(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),
    
    case do_transfer_esi(MeId, Url, Password, EsiLogPath, "static") of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    check_esi_log_transfered(EsiLogName, false, Config);
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]).

%%--------------------------------------------------------------------
%% @doc Export esi with non-existent granularity 
%% @end
%%--------------------------------------------------------------------

transfer_esi_nisse(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),
    
    case do_transfer_esi(MeId, Url, Password, EsiLogPath, "nisse") of
	{["SUCCESS"], [_]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    ct:fail("ESI accepted erroneous input");
	{error, _} ->
	    ct:print("transfer_esi ended with failure~n",[])
    end,
    ct:pal("Test case complete~n",[]).

do_transfer_esi(MeId, Url, Password, EsiLogPath, Granularity) ->

    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [Url++EsiLogPath]},
		     {password, [], [Password]}|
		     case Granularity of
			 undefined -> [];
			 _ -> [{granularity, [], [Granularity]}]
		     end]
		    }]}]}]},
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
    {ok,_} = ct_netconfc:open(nc1,[]),
    %% trig the action transfer_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
	{ok, A} ->
	    Return = extract_element(returnValue, A),
	    ct:pal("transfer_esi ~p~n",[Return]),
	    ct_netconfc:close_session(nc1),
	    timer:sleep(3000),
	    wait_for_progress(progressReport, ProgressFilter);
	Error ->
	    ct:log("~p~n",[Error]),
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a transfer ESI log from SUT to a sftp server using a non existing url.<br/>
%% This TC will trig a transfer ESI log from SUT to a non existing url.<br/>
%% The progressreport shall indicate FAILURE of the transfer.<br/>
%%
%% @spec transfer_esi_fail(Config) -> ok
%% @end
%%--------------------------------------------------------------------
transfer_esi_fail(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),

    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [Url++EsiLogPath++"/non_existing_dir"]},
		     {password, [], [Password]}]}
		    %% [{uri, [], [?SFTP_URL++EsiLogPath++"/non_existing_dir"]},
		    %%  {password, [], [?PSWD]}]}
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

    {ok,_} = ct_netconfc:open(nc1,[]),
    %% trig the action transfer_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
	{ok, A} ->
	    Return = extract_element(returnValue, A),
	    ct:log("transfer_esi ~p~n",[Return]);
	Error ->
	    ct:log("~p~n",[Error]),
	    ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),

    case wait_for_progress(progressReport, ProgressFilter) of
	{["SUCCESS"], ResultInfo} ->
	    ct:log("transfer_esi: SUCCESS ~p~n",[ResultInfo]),
	    ct:fail("Failed operation resulted in success report.~n",[]);
	{["FAILURE"], _} ->
	    ct:log("Got expected result 'FAILURE'~n",[]),
	    ok;
	{Result, _} ->
	    ct:log("transfer_esi: ~s~n",[Result]),
	    ct:fail(Result)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a transfer ESI log from SUT to a sftp server using a url with wrong port.<br/>
%% This TC will trig a transfer ESI log from SUT to a non existing url.<br/>
%% The progressreport shall indicate FAILURE of the transfer.<br/>
%%
%% @spec transfer_esi_fail_port(Config) -> ok
%% @end
%%--------------------------------------------------------------------
transfer_esi_fail_port(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),

    Action = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'LogM',
          [{xmlns,"urn:com:ericsson:ecim:LogM"}],
          [{logMId,[],["1"]},
           {'exportEsi',[],
            [{uri, [], [Url++":9921"++EsiLogPath]},
             {password, [], [Password]}]}
            %% [{uri, [], [?SFTP_URL":9921"++EsiLogPath]},
            %%  {password, [], [?PSWD]}]}
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

    {ok,_} = ct_netconfc:open(nc1,[]),
    %% trig the action transfer_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
    {ok, A} ->
        Return = extract_element(returnValue, A),
        ct:log("transfer_esi ~p~n",[Return]);
    Error ->
        ct:log("~p~n",[Error]),
        ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),

    case wait_for_progress(progressReport, ProgressFilter) of
    {["SUCCESS"], ResultInfo} ->
        ct:log("transfer_esi: SUCCESS ~p~n",[ResultInfo]),
        ct:fail("Failed operation resulted in success report.~n",[]);
    {["FAILURE"], _} ->
        ct:log("Got expected result 'FAILURE'~n",[]),
        ok;
    {Result, _} ->
        ct:log("transfer_esi: ~s~n",[Result]),
        ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a transfer ESI log from SUT to a sftp server using wrong password.<br/>
%% This TC will trig a transfer ESI log from SUT using the wrong password.<br/>
%% The progressreport shall indicate FAILURE of the transfer.<br/>
%%
%% @spec transfer_esi_fail_password(Config) -> ok
%% @end
%%--------------------------------------------------------------------
transfer_esi_fail_password(Config)->
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    WrongPassword = "notapassword",

    Action = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'LogM',
          [{xmlns,"urn:com:ericsson:ecim:LogM"}],
          [{logMId,[],["1"]},
           {'exportEsi',[],
            [{uri, [], [Url++EsiLogPath]},
             {password, [], [WrongPassword]}]}
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

    {ok,_} = ct_netconfc:open(nc1,[]),
    %% trig the action transfer_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
    {ok, A} ->
        Return = extract_element(returnValue, A),
        ct:log("transfer_esi ~p~n",[Return]);
    Error ->
        ct:log("~p~n",[Error]),
        ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),

    case wait_for_progress(progressReport, ProgressFilter) of
    {["SUCCESS"], ResultInfo} ->
        ct:log("transfer_esi: SUCCESS ~p~n",[ResultInfo]),
        ct:fail("Failed operation resulted in success report.~n",[]);
    {["FAILURE"], _} ->
        ct:log("Got expected result 'FAILURE'~n",[]),
        ok;
    {Result, _} ->
        ct:log("transfer_esi: ~s~n",[Result]),
        ct:fail(Result)
    end.
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
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),

    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [Url++EsiLogPath]},
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
    {ok,_} = ct_netconfc:open(nc1,[]),
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
    case wait_for_progress(progressReport, ProgressFilter) of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    check_esi_log_transfered(EsiLogName, true, Config);
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]).



%% @hidden
%% Will check that esi log is tranfered to expected path and also the size of the file.
%% If the size is to small, then maybe it does not consist on anything!
check_esi_log_transfered(EsiLogName, UnpackEsiFile, Config) ->
    check_esi_log_transfered(?config(ftp_protocol, Config), 
                             EsiLogName, 
                             ?config(esi_log_path, Config),
                             UnpackEsiFile, 
                             Config).
    
check_esi_log_transfered(sftp, EsiLogName, EsiLogPath, UnpackEsiFile, _Config) ->
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    {ok,ChPid,_ChRef} = 
	ssh_sftp:start_channel(SftpHost, 
			       [{user, Username},
				{password,Password },
				{silently_accept_hosts, true},
				{timeout, 30000}]),
	%% ssh_sftp:start_channel(?SFTP_HOST, 
	%% 		       [{user, ?USER},
	%% 			{password, ?PSWD},
	%% 			{silently_accept_hosts, true},
	%% 			{timeout, 30000}]),

    {ok, DirData} = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
    % DirData = lists of strings from the directory.
    {ok, FileInfo} = ssh_sftp:read_file_info(ChPid, EsiLogPath++EsiLogName, 3000),
    Size = lists:nth(2, tuple_to_list(FileInfo)),
    ssh_sftp:stop_channel(ChPid),
    
    check_esi_log_transfered(EsiLogName, EsiLogPath, UnpackEsiFile, DirData, Size),
    ok;

check_esi_log_transfered(ftpes, EsiLogName, EsiLogPath, UnpackEsiFile, Config) ->

    {ok, RawDirData} = rct_ftpes_client:list_dir(EsiLogPath),
    DirData = [filename:basename(string:strip(File)) || File <- string:tokens(RawDirData, "\r\n")],
    ct:pal("DirData: ~p~n", [DirData]),
    
    %% transfer file locally so it can be accessed 
    {ok, Bin} = rct_ftpes_client:read_file(filename:join(EsiLogPath, EsiLogName)),
    LocalDir = ?config(priv_dir,Config),
    file:write_file(filename:join(LocalDir, EsiLogName), Bin, [write]),
    {ok, FileInfo} = file:read_file_info(filename:join(LocalDir, EsiLogName)),
    Size = lists:nth(2, tuple_to_list(FileInfo)),

    check_esi_log_transfered(EsiLogName, LocalDir, UnpackEsiFile, DirData, Size),
    
    %% delete exported ESI from FTPES server
    rct_ftpes_client:delete_file(filename:join(EsiLogPath, EsiLogName)),
    ok;


check_esi_log_transfered(EsiLogName, EsiLogPath, UnpackEsiFile, DirData, Size) when is_list(EsiLogName)->
    Pred= fun(Str) ->
		  if Str == EsiLogName ->
			  true;
		     true ->
			  false
		  end
	  end,

    case lists:any(Pred,DirData) of
	true -> 
	    ct:pal("### Esi log file: ~p, exist in \n path : ~p \n",
		   [EsiLogName, EsiLogPath]),
	    timer:sleep(100),
	    case UnpackEsiFile of
		true -> os:cmd("tar -xf " ++ EsiLogPath ++ EsiLogName ++" --gunzip --directory=" ++ EsiLogPath),
			%% io:format("<a href=\"~s\">~s</a>", 
			%% 	  [EsiLogPath, "Esi unpacked dir"]),
			%% io:format("<a href=\"~s\">~s</a>", 
			%% 	  [filename:join(EsiLogPath,EsiLogName), EsiLogName]);
			format_html("<a href=\"~s\">~s</a>", 
				  [EsiLogPath, "Esi unpacked dir"]),
			format_html("<a href=\"~s\">~s</a>", 
				  [filename:join(EsiLogPath,EsiLogName), EsiLogName]);

		_-> %% io:format("<a href=\"~s\">~s</a>", 
		    %% 	      [filename:join(EsiLogPath,EsiLogName), EsiLogName])
		    format_html("<a href=\"~s\">~s</a>", 
		    	      [filename:join(EsiLogPath,EsiLogName), EsiLogName])

	    end;
	false -> 
	    ct:fail(" Could not find the ESI log file, on server.")
    end,

    if Size > 10000 ->
	    true;
       true  ->
	    ct:pal("### Size of the esi tar file is: ~p. ~n "
		   "It is smaller than expected. ~n "
		   "Unpack the file and ckeck that it look OK. \n",[Size]),
	    ct:fail("Size of the esi log file is to small! check if it "
		    "looks ok after unpack!.")
    end,
 
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This TC makes sure that the test license to disable the encrypt ESI functionlaity is not 
%% loaded on the node and then transfer ESI log from SUT to a sftp server using netconf.<br/>
%% The log file will be stored .../log_private/ , that will be created when TC starts.<br/>
%% TC will check progressreport using netconf and also check that correct file is transfered 
%% correct.
%%
%% @spec transfer_encrpted_esi_and_decrypt(Config) -> ok
%% @end
%%--------------------------------------------------------------------
transfer_encrpted_esi_and_decrypt(Config)->   
    Url = ?config(url, Config),
    MeId = ?config(meId, Config),
    EsiLogPath = ?config(esi_log_path, Config),
    Password = ?config(password, Config),
 
    case rct_rpc:call(rpc_1, lmaI, encrypt_esi_log, [], 10000) of
	false ->
	    ct:pal("Need to install LKF without test license to run this TC"),
	    LKF = "RCS_MSR_160111_154117_without_encryption_key.xml",
	    %% Remove PKI verification for LKF
	    rct_rpc:call(rpc_1, lmaGlms, change_test_mode, [on], 10000),
	    log_license_support:install_lkf(Url, Password, LKF),
	    LicenseInfo = rct_rpc:call(rpc_1, lmaI, encrypt_esi_log, [], 10000),
	    ct:pal("LicenseInfo: ~p", [LicenseInfo]);
	true ->
	    %% Already possible to fetch a enctypted ESI
	    do_nada
    end,
    	    
    %% test_server:break("Break"),	
    case do_transfer_esi(MeId, Url, Password, EsiLogPath, undefined) of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    Backups = get_all_backups(MeId),
	    [case extract_element(backupName, [Backup]) of
		 {ok, {_, _, ["EsiBackup"++A]}} ->
		     ct:pal("Esi~s present",[A]),
		     ct:fail(esi_backup_present);
		 
		 _ ->
		     ok
	     end||Backup<-Backups],
	    %% Make sure that the ESI ends with .gpg
	    case filename:extension(EsiLogName) of
		".gpg" ->
		    ct:pal("EsiLogName: ~p", [EsiLogName]),
		    ct:pal("EsiLogPath: ~p", [EsiLogPath]),
		    TestUser = re:replace(os:cmd("sudo -u rcsci1 cat /home/rcsci1/private/rcsci1.LOG_info"),
					  "\\s+", "", [global,{return,list}]),

		    %%todo check that it is not possible to unpack
		    check_esi_log_transfered(EsiLogName, false, Config),
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
		    check_esi_log_transfered(DecryptEsiLogName, true, Config),
		    check_esi_unpacked(EsiLogPath, "rcs");
		    %% R3 should be the decrypted contents of the uploaded file.
		_Else ->
		    ct:fail("The ESI is not ecrypted: ~p", [EsiLogName])
	    end;
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]),
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

    

%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------

wait_for_progress(Attribute, ProgressFilter) ->
    {ok, _} = ct_netconfc:open(nc1, []),
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
	    wait_for_progress(Attribute, ProgressFilter)
    end.

get_all_backups(MeId) ->
    Get =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]}]}]}]}]},
    {ok, _} = ct_netconfc:open(nc1, []),
    {ok, Result} = ct_netconfc:get(nc1, Get),
    ct_netconfc:close_session(nc1),
    {ok, {_, _, Contents}} = extract_element('BrmBackupManager', Result),
    [BrmBackupE||BrmBackupE<-Contents,
		 element(1, BrmBackupE) == 'BrmBackup'].


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
poll_connection(Node, Time) ->
    ct:pal("Trying to connect to the target for ~p seconds",[Time]),
    poll_connection(Node, Time, []).
poll_connection(_Node, Time, Reason) when Time =:= 0 ->
    ct:fail(Reason);
poll_connection(Node, Time, _Reason) ->
    timer:sleep(1000),
    case  ct_netconfc:open(Node,[]) of
	{ok,_} -> ct_netconfc:close_session(Node),
		  ok;
	Result-> poll_connection(Node, Time-1, Result)
    end.

	 
 format_html(String,Args) ->
     ct:log(default, 1, String, Args, [no_css]).
   
install_default_lkf(SFTP_URL, Password, LKF, LKF_DIR) ->
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    SFTP_DIR = "/proj/rcs-tmp/stps/",

    {ok,_} = file:copy(LKF_DIR++"/"++LKF, 
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
wait_until_install_complete(No) -> %%export this function in log_sec_event_SUITE.erl
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

get_ftp_file_path(sftp, Config) ->
    ?config(priv_dir, Config);
get_ftp_file_path(ftpes, _Config) ->
    %% should always point to relevant directory
    {ok, LogDir} = rct_ftpes_client:pwd(),
    LogDir.

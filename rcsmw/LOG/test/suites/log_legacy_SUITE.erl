%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_legacy_SUITE.erl %
%%% @author estjako
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R9A/6
%%% @doc Example test suite for one sim or target env

-module(log_legacy_SUITE).
-include_lib("common_test/include/ct.hrl").

-define(FTPES_SERVER_LOG_DIR, "LOG").

-vsn('/main/R2A/R3A/R4A/R9A/6').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R1A/1      2012-03-14 etxjotj     Created
%%% R1A/2      2012-09-20 eblakes     New test suite for logexport
%%% R1A/4      2012-09-20 etxkols     edoc and exports fixed
%%% R1A/5      2012-10-04 etxkols     SFTP_HOST changed to 10.68.200.11
%%% R2A/6      2013-02-29 etxkols     Added rct_core hook
%%% R2A/7      2013-03-23 etxkols     Wrong ct:pal format
%%% R1A/8      2013-06-17 etxjovp     Add hook rct_logging
%%% R3A/2      2015-02-28 etxkols     Preparation for 2 labs
%%% R3A/3      2015-03-09 etxjotj     Severity filter test
%%% R4A/1      2015-05-20 etxpejn     Changed SystemLog to LicensingLog
%%% R4A/2      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/3      2016-02-12 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%%
%%% R9A/1      2017-02-03 eivmiha     ftpes_group tests added
%%% R9A/3      2017-02-14 emarnek     Added wrong_port test
%%% R9A/4      2017-02-16 etxkols     Change ct:pal to ct:log to avoid Jenkins console parsing failure
%%% R9A/5      2017-03-01 ekurnik     Added sleep to export logs which caused test to fail sometimes
%%% R9A/6      2017-02-28 estjako     Changed delete file logic for ftpes
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% CT infrastructure
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
     init_per_group/2,
     end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0]).

%%% Test cases
-export([write_log/1,
	 transfer_logs/1,
     transfer_logs_wrong_uri/1,
     transfer_logs_wrong_port/1,
     transfer_logs_wrong_pass/1,
	 password/1, 
	 transfer_avli/1,
     transfer_avli_wrong_uri/1,
     transfer_avli_wrong_port/1,
     transfer_avli_wrong_pass/1,
	 severity_filter/1
	]).
 
%% @hidden
groups() ->
    LegacyGroup = legacy_group(),
    FtpesGroup = ftpes_group(),
    [
     {legacy_group, [], LegacyGroup },
     {ftpes_group, [], FtpesGroup},
     {default__group, [], LegacyGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]}, 
     {sbc__upgrade_short__all__1__group, [], [{group, default__group}]}, 
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []} 
    ].
%%--------------------------------------------------------------------
%% @doc Runs all testcases in SUITE.
%% @end
%%-------------------------------------------------------------------

all() ->
    [{group, legacy_group}].


legacy_group() ->
    [write_log,
     transfer_logs,
     transfer_logs_wrong_uri,
     transfer_logs_wrong_port,
     transfer_logs_wrong_pass,
     password,
     transfer_avli,
     transfer_avli_wrong_uri,
     transfer_avli_wrong_port,
     transfer_avli_wrong_pass].

ftpes_group() ->
    [transfer_logs,
     transfer_logs_wrong_uri,
     transfer_logs_wrong_port,
     transfer_logs_wrong_pass,
     transfer_avli,
     transfer_avli_wrong_uri,
     transfer_avli_wrong_port,
     transfer_avli_wrong_pass].


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_rpc, rct_netconf
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,{nc1, html}},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging, 
		  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
          ftpes_test_lib:ftpes_hook()
		]}].

%% @hidden
init_per_suite(Config) ->
    LogPath = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", LogPath]),
    [{ftp_protocol, sftp}, {sftp_root, LogPath}|Config].

%% @hidden
end_per_suite(_Config) ->
    ok.

init_per_group(ftpes_group, Config) ->
    ok = rct_ftpes_client:open(),
    ftpes_test_lib:create_test_folder("LOG"),
    {ok, Folder} = rct_ftpes_client:pwd(),
    Path = Folder ++ "/",
    NewConfig = lists:keyreplace(ftp_protocol, 1, Config, {ftp_protocol, ftpes}),
    
    %% Create NodeCredential and activate ftpes feature
    Started = ftpes_test_lib:start_server(rpc_1),
    NewConfig2 = ftpes_test_lib:initialize_nc_tc(NewConfig),
    ftpes_test_lib:enable_ftpes_tls(NewConfig2),
    
    [{started, Started}, {log_path, Path}| NewConfig2];

init_per_group(_GroupName, Config) ->
    FtpProtocol = ?config(ftp_protocol, Config),
    LogPath = ftpes_test_lib:get_ftp_file_path(FtpProtocol, Config),
    
    [{log_path, LogPath} | Config].

end_per_group(ftpes_group, Config) ->
    rct_ftpes_client:rmdir(?config(log_path, Config)),
    ftpes_test_lib:disable_ftpes_tls(),
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_server(rpc_1);
        yes -> ok
    end,
    ftpes_test_lib:clean_nc_tc(Config),
    
    ok = rct_ftpes_client:close();

end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    FtpProtocol = ?config(ftp_protocol, Config),
    [{host, Host},{username, Username},{password, Password}] = 
        ftpes_test_lib:get_ftp_config(FtpProtocol),
    
    Uri = atom_to_list(FtpProtocol) ++ "://"++Username++"@"++Host,
    [{uri, Uri}, {password, Password} | Config].

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%%-------------------------------------------------------------------
%% @doc Writes a log entry.<br/><br/>
%% @end
%%--------------------------------------------------------------------
write_log(_) ->
    Args = ["LicensingLog", "Test", info, "Automated test"],
    rct_rpc:call(rpc_1, logI, write_log, Args, 10000),
    ok.

%%--------------------------------------------------------------------
%% @doc Test if password can be set in cleartext and encrypted
%% @end
%%--------------------------------------------------------------------

password(_config) ->
    A = {'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',[],
	   [{systemFunctionsId,[],["1"]},
	    {'LogM',[{xmlns,"urn:com:ericsson:ecim:LogM"}],
	     [{logMId,[],["1"]},
	      {'Log',[],
	       [{logId,[],["SecurityLog"]},
		{'LogPushTransfer',[],
		 [{logPushTransferId,[],["1"]},
		  {uri,[],["sftp://apa@1.2.3.4/banan"]},
		  {transferType,[],["BULK"]},
		  {password,[], %{struct, "EcimPassword"}],
		   [{cleartext, [], []},
		    {password, [], ["dilbert"]}]
		   }]}]},
	      {'Log',[],
	       [{logId,[],["SecurityLog"]},
		{'LogPushTransfer',[],
		 [{logPushTransferId,[],["1"]},
		  {uri,[],["sftp://apa@1.2.3.4/banan"]},
		  {transferType,[],["BULK"]},
		  {password,[],%{struct, "EcimPassword"}],
		   [%{cleartext, [], ["false"]},
		    {password, [], ["1:hhWoDtjP5KaxP5bGUdC9IQcB4TXeywU="]}]
		   }]}]}
	      ]}]}]},
    ok = netconf(edit_config, [nc1, running, A]).


%%--------------------------------------------------------------------
%% @doc Transfer logs.<br/><br/>
%% @end
%%--------------------------------------------------------------------
transfer_logs(Config)->
    Get = {'ManagedElement',                  
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],["1"]},
		  {'SystemFunctions',
		   [{systemFunctionsId,[],["1"]},
		    {'LogM',
		     [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		     [{logMId,[],["1"]}
		     ]}]}]},

    {ok, ME} = netconf(get, [nc1, Get]),
    {ok, {'LogM', _, LogList}} = extract_element('LogM', ME),
    LogNameList = extract_logName(LogList),
    
    NewConfig = [{success, yes} |Config],
    DID_IT_FAIL = [export_log(LogName, NewConfig) || LogName <- LogNameList],
    check_for_failure([Yes || Yes <- DID_IT_FAIL, Yes /= ok], DID_IT_FAIL).

transfer_logs_wrong_uri(Config)->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ "/WrongLogPath"}),
    transfer_logs_fail([{success, no}|NewConfig]).

transfer_logs_wrong_port(Config)->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ ":9921"}),
    transfer_logs_fail([{success, no}|NewConfig]).

transfer_logs_wrong_pass(Config)->
    NewConfig = lists:keyreplace(password, 1, Config, {password, "wrongpassword"}),
    transfer_logs_fail([{success, no}|NewConfig]).
    
transfer_logs_fail(Config)->
    Get = {'ManagedElement',                  
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],["1"]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'LogM',
             [{xmlns,"urn:com:ericsson:ecim:LogM"}],
             [{logMId,[],["1"]}
             ]}]}]},

    {ok, ME} = netconf(get, [nc1, Get]),
    {ok, {'LogM', _, LogList}} = extract_element('LogM', ME),
    LogNameList = extract_logName(LogList),
    [export_log(LogName, Config) || LogName <- LogNameList].

check_for_failure([], _) -> 
    [];
check_for_failure(_, DID_IT_FAIL) -> 
    ct:fail(DID_IT_FAIL).


%%--------------------------------------------------------------------
%% @doc Export logs
%% @end
%%--------------------------------------------------------------------
   	
export_log(LogName, Config)->
    ProgressFilter = {'ManagedElement',   
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'LogM',
			  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			  [{logMId,[],["1"]},
			   {'Log',[],
			    [{logId,[],[LogName]},
			     {progressReport,[],[]}]}]}]}]},

    Success = ?config(success, Config),
    Uri = ?config(uri, Config) ++ ?config(log_path, Config),
    Pass = ?config(password, Config),
    
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'Log',[],
		    [{logId,[],[LogName]},
		     {'export',[],
		      [{uri, [], [Uri]},
		       {password, [], [Pass]}]}]}]}]}]},
    {ok, ActionId} = netconf(action, [nc1, Action]),
    ct:pal("Wait for progress"),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    	{["SUCCESS"],["The log is empty"]} ->
	    %%ct:pal(" The " ++ LogName ++ " is empty"),
	    ok;
    	{["SUCCESS"], _ResultInfo} ->
	    check_log_transfered(LogName, Config);
        {["FAILURE"], ResultInfo} ->
            case Success of 
                yes -> ct:fail("~w failed:  ~p",[export_log, ResultInfo]);
                _ -> ct:log("~w failed:  ~p",[export_log, ResultInfo])
            end;
    	Result ->
    	    ct:pal("~w: ~s ~p",[export_log, LogName, Result]),
    	    ct:fail(Result)
    end,
    
    timer:sleep(100), %% sleep due to timing issue
    ok.

%%--------------------------------------------------------------------
%% @doc Transfer avli.<br/><br/>
%% @end
%%--------------------------------------------------------------------

transfer_avli(Config)-> transfer_export_avli([{success, yes}|Config]).

transfer_avli_wrong_uri(Config)->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ "/WrongLogPath"}),
    transfer_export_avli([{success, no}|NewConfig]).

transfer_avli_wrong_port(Config)->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ ":9921"}),
    transfer_export_avli([{success, no}|NewConfig]).

transfer_avli_wrong_pass(Config)->
    NewConfig = lists:keyreplace(password, 1, Config, {password, "wrongpassword"}),
    transfer_export_avli([{success, no}|NewConfig]).

transfer_export_avli(Config)->
    LogName = "RBS_CS_AVAILABILITY",
    ProgressFilter = {'ManagedElement',   
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'LogM',
			  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			  [{logMId,[],["1"]},
			   {progressReport,[],[]}
			   ]}]}]},

    Success = ?config(success, Config),
    Uri = ?config(uri, Config) ++ ?config(log_path, Config),
    Pass    = ?config(password, Config),

    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportAvailabilityLog',[],
		    [{uri, [], [Uri]},
		     {password, [], [Pass]}]}
		  ]}]}]},
    {ok, ActionId} = netconf(action, [nc1, Action]),
    ct:pal("Wait for progress"),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    	{["SUCCESS"], ["The log is empty"]} ->
	    ct:pal(" The " ++ LogName ++ " is empty"),
	    ok;
    	{["SUCCESS"], _ResultInfo} ->
	    check_log_transfered(LogName, Config);
        {["FAILURE"], ResultInfo} ->
           case Success of 
               yes -> ct:fail("~w failed:  ~p",[export_log, ResultInfo]);
               _ -> ct:log("~w failed:  ~p",[export_log, ResultInfo])
           end;
    	Result ->
    	    ct:pal("~w: ~s ~p", [export_log, LogName, Result]),
    	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc Set and delete severity filter
%% @end
%%--------------------------------------------------------------------

severity_filter(_) ->
    Edit1 = {'ManagedElement',[],
	     [{managedElementId,[],["1"]},
	      {'SystemFunctions',
	       [{systemFunctionsId,[],["1"]},
		{'LogM',	[],
		 [{logMId,[],["1"]},
		  {'Log',[],
		   [{logId,[], ["FakeLog"]},
		    {severityFilter,[], ["WARNING"]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit1]),
    Edit2 = {'ManagedElement',
	     [{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	     [{managedElementId,[],["1"]},
	      {'SystemFunctions',
	       [{systemFunctionsId,[],["1"]},
		{'LogM',	[],
		 [{logMId,[],["1"]},
		  {'Log',[],
		   [{logId,[], ["FakeLog"]},
		    {severityFilter,[{'xc:operation', "delete"}], []}
		    ]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit2]).
    
    



%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Execute an netconf command and close the session
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    case ct_netconfc:close_session(nc1) of
	ok ->
	    Res;
	Fail ->
	    ct:pal("apply(ct_netconfc, ~p ~p) =~n~p~n",[F,A, Res]),
	    ct:fail(Fail)
    end.
%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------

wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(1000),
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter), 
	    ct_netconfc:close_session(nc1),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    ct:pal("Waiting for updated progress~n"),
			    wait_for_progress(Attribute, OldActionId, 
					      ProgressFilter);
			_ ->
			    case check_progress_elements(Attribute, Report) of
				loop ->
				    wait_for_progress(Attribute, OldActionId, 
						      ProgressFilter);
				{ok, Result, ResultInfo} ->
				    {Result, ResultInfo}
			    end
		    end;
		{error, Error} ->
		    ct:pal("~p",[Error]),
		    wait_for_progress(Attribute, OldActionId, ProgressFilter)
	    end;
	{error, Reason} ->
	    ct:pal("~p~n",[Reason]),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.

check_progress_elements(Attribute, ProgressReport) ->
    ct:log("#### netconf ~p~n", [{Attribute, ProgressReport}]),
    {ok, Report} = extract_element(Attribute, ProgressReport),
    ct:log("#### netconf ~p~n", [Report]),
    {ok, State}  = extract_element(state, [Report]),
    case State of
	{state, _, ["FINISHED"]} ->
	    format_progress_report(Report),
	    ct:log("~s",[format_progress_report(Report)]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} = 
		extract_element(resultInfo, [Report]),
	    {ok, Result, ResultInfo};
	{state, _, [Current]} ->
	    {ok, {progressPercentage, _, [Percent]}} =
		extract_element(progressPercentage, [Report]),
	    {ok, {progressInfo, _, Info}} = 
		extract_element(progressInfo, [Report]),
	    ct:pal("State: ~s ~s % ready~n~s",[Current, Percent, info(Info)]),
	    loop
    end.

info([])     -> "";
info([Info]) -> Info.

format_progress_report({progressReport, _, Members}) ->
    [io_lib:format("reportProgress:~n",[])|format_progress_report(Members)];
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) -> 
    ct:pal("~p~n",[X]),
    ct:fail(unknown).
			      

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
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------


%%Returns list of all the Logs present                  
extract_logName(LogList)-> 
    case lists:keytake('Log',1,LogList) of	  
	{value,_,_}->  
	    {value,Log1,Restlogs}=lists:keytake('Log',1,LogList),
	    {'Log',[],
	     [{logId,[],[LogName]}|_]}
		=Log1,
	    [LogName|extract_logName(Restlogs)];
	_-> 
	    []
    end.


%% @hidden
%% Will check that log is tranfered to expected path and also the size of the file.
%% If the size is to small, then maybe it does not consist on anything!
check_log_transfered("AlarmLog", Config) ->
    check_log_transfered("saLogAlarm",Config);
check_log_transfered(LogName, Config) ->
    Proto = ?config(ftp_protocol, Config),
    LogPath = ?config(log_path, Config),
    {ok, DirData} = case Proto of
                        sftp -> file:list_dir(LogPath);
                        ftpes -> {ok, RawDirData} =rct_ftpes_client:list_dir(LogPath),
                                 {ok, [filename:basename(string:strip(File)) || File <- string:tokens(RawDirData, "\r\n")]}
                    end,

    FileNames = finder_file_name(DirData, LogName),
    
    case FileNames of
	[] -> 
	    "Could not find the " ++ LogName ++ " file, on " ++ atom_to_list(Proto) ++" server.";
	_ -> 
	    ct:pal("### log file: ~p, exist in \n path : ~p \n",
		   [lists:nth(1, FileNames), LogPath]),
	    check_log_transfered_cont(lists:nth(1, FileNames), LogName, Config),
        delete_files(Proto, FileNames)
    end.

%% extracts the name of the file where the log is saved from DirData
finder_file_name([H|T], LogName)->
    case lists:prefix(LogName, H) of
	true->
	    [H | finder_file_name(T, LogName)];
	false->
	    finder_file_name(T, LogName)
    end;
finder_file_name([], _) ->
    [].

%%Checks the size of the file and notifies if it's to small
check_log_transfered_cont(FileName, LogName, Config)->
  
    Size = case ?config(ftp_protocol, Config) of
               sftp -> filelib:file_size(filename:join(?config(log_path, Config), FileName));
               ftpes -> {ok, Bin} = rct_ftpes_client:read_file(filename:join(?config(log_path, Config), FileName)),
                        LocalDir = ftpes_test_lib:get_ftp_file_path(sftp, Config),
                        file:write_file(filename:join(LocalDir, LogName), Bin, [write]),
                        filelib:file_size(filename:join(LocalDir, FileName))
           end,

    if Size > 10000 ->
	    true;
       true  ->
	    ct:pal(("### Size of the  ")++LogName++(" tar file is: ~p. \n It is smaller than expected. \n Unpack the file and check that it look OK. \n" ) ,[Size])
    end,
    ok.

delete_files(sftp, _FileNames) ->
    ok;
delete_files(ftpes, FileNames) ->
    lists:foreach(fun(File) -> rct_ftpes_client:delete_file(File) end, FileNames).


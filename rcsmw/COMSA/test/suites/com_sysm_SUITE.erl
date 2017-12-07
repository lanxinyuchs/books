%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	com_sysm_SUITE.erl %
%%% @author estjako
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R9A/4
%%% 
%%% @doc == Test Suite for testing the ECIM SysM branch==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end

-module(com_sysm_SUITE).
-vsn('/main/R2A/R3A/R9A/4').

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
%%% R1A/1      2012-09-20 etxjotj     Created
%%% R2A/1      2013-01-17 etxkols     Change ct:pal to ct:log since
%%%                                   Jenkins detects error in console
%%% R2A/5      2013-02-28 etxkols     Added rct_core hook
%%% R3A/1      2015-02-27 etxkols     Preparation for 2 labs
%%% R3A/2      2015-02-28 etxkols     Preparation for cluster
%%% R3A/3      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% 
%%% R9A/1      2016-02-13 enekdav     FTPES test group added
%%% R9A/2      2016-03-15 emarnek     Removed file delete from FTPES
%%% R9A/3      2017-04-03 estjako     ftpes_group added in all()
%%% R9A/4      2017-04-04 estjako     Removed ftpes_group from all()- fails on 5G environment
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
     init_per_suite/1,
     end_per_suite/1,
     init_per_group/2,
     end_per_group/2,
     init_per_testcase/2,
     end_per_testcase/2,
     groups/0,
     all/0]).
-export([export_models/1,
         export_models_wrong_uri/1,
         export_models_wrong_pass/1,
	     configure_ntp/1]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, legacy_group}].

legacy_group() ->
    [export_models].

ftpes_group() ->
    [export_models,
     export_models_wrong_uri,
     export_models_wrong_pass].

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
                 {rct_core,[]},
                 {rct_logging, 
          {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
          ftpes_hook()
        ]}].


%% @hidden
init_per_suite(Config) ->
       PrivDir = proplists:get_value(priv_dir, Config),
       os:cmd(["chmod a+rwx ", PrivDir]),
     %% This is because due to errors in COM there is no proper way to
     %% read the set managedElementId if it has changed from "1"
       MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
       MeId = case proplists:get_value(networkManagedElementId, MeData) of
                  undefined -> "1";
                  NMEI -> NMEI
              end,
       [{host, NtpHost}] = ct:get_config(ntp_server),
       [{meId, MeId}, {ntp_host, NtpHost}, {ftp_protocol, sftp}, {sftp_root, PrivDir}|Config].
%% @hidden
end_per_suite(_Config) ->
    ok.

init_per_group(ftpes_group, Config) ->
    ok = rct_ftpes_client:open(),
    ok = rct_ftpes_client:cd("COMSA"),
    NewConfig = lists:keyreplace(ftp_protocol, 1, Config, {ftp_protocol, ftpes}),
    
    %% Create NodeCredential and activate ftpes feature
    Started = ftpes_test_lib:start_server(rpc_1),
    NewConfig2 = ftpes_test_lib:initialize_nc_tc(NewConfig),
    ftpes_test_lib:enable_ftpes_tls(NewConfig2),
    
    [{started, Started} | NewConfig2];

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(ftpes_group, Config) ->
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
        get_ftp_config(FtpProtocol),
    Path = get_ftp_file_path(FtpProtocol, Config),
    
    Uri = atom_to_list(FtpProtocol) ++ "://"++Username++"@"++Host ++ Path,
     
    [{uri, Uri}, {password, Password}, {log_path, Path} | Config].

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.
%% @hidden
groups() ->
    AllGroup = all(),
    LegacyGroup = legacy_group(),
    FtpesGroup = ftpes_group(),
    [
     {legacy_group, [], LegacyGroup},
     {ftpes_group, [], FtpesGroup},
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []}
    ].


%%--------------------------------------------------------------------
%% @doc Schema file transfer 
%% Find out which model schemas there are and transfer them to the specified
%% remote host via the export action
%% Sect 3.1.3 and 3.1.4 of 
%% Use Case description 12/155 56-FAE 151 01 Uen Rev A 2013-03-07
%% @end
export_models(Config) -> 
    MeId = proplists:get_value(meId, Config),
    export_models_test([{schemas, get_all_schemas(MeId)} | Config]).

export_models_wrong_uri(Config) ->
    MeId = proplists:get_value(meId, Config),
    OldUri = ?config(uri, Config),
    Uri = OldUri ++ "/WrongComPath",
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri}),
    export_models_test([{error, "efnamena"}, {schemas, [hd(get_all_schemas(MeId))]} | NewConfig]).

export_models_wrong_pass(Config) ->
    MeId = proplists:get_value(meId, Config),
    NewConfig = lists:keyreplace(password, 1, Config, {password, "wrongpassword"}),
    export_models_test([{error, "euser"}, {schemas, [hd(get_all_schemas(MeId))]}| NewConfig]).

export_models_test(Config)->        
    MeId = proplists:get_value(meId, Config),
    Schemas = ?config(schemas, Config),
    ExpectedError = ?config(error, Config),
    Uri = ?config(uri, Config),
    Password = ?config(password, Config),
    Results = [begin
		   {ok, SchemaId} = extract_element(schemaId, [Schema]),
		   Action = 
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],[MeId]},
			 {'SystemFunctions',
			  [{systemFunctionsId,[],["1"]},
			   {'SysM',
			    [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
			    [{sysMId,[],["1"]},
			     {'Schema', [], 
			      [SchemaId, 
			       {export, 
				[{uri, [Uri]},
				 {password, [Password]}]}]}]}]}]},
		   netconf(action, [nc1, Action])
	       end
	       ||Schema<-Schemas],
    ct:log("Results: ~p~n", [Results]),
    lists:map(fun(X) -> check_results(X, ExpectedError) end, Results),
%%     [case extract_element(returnValue, Result) of
%% 	 {ok, {returnValue, _, ["Exported"]}} -> ok;
%% 	 {ok, {returnValue, _, [Unexpected]}} ->
%% 	     {ok, {schemaId, _, [SchemaId]}} = 
%% 		 extract_element(schemaId, Result),
%% 	     ct:fail({SchemaId, Unexpected})
%%      end||{ok, Result}<-Results],
%%     PrivDir = proplists:get_value(priv_dir, Config),
%%     {ok, Files} = file:list_dir(PrivDir),
%%     ct:pal("Files: ~p~n",[Files]),
    ok.

check_results(X, ExpectedError) ->
    ct:log("Reason: ~p~n", [X]),
    case X of
        ok -> ok;
        {error, Reason} -> StringReason = lists:flatten(io_lib:format("~p", [Reason])),
                           case string:str(StringReason, ExpectedError) of
                            0 -> ct:fail("Unexpected error, expected ~p~nGot ~p~n", [ExpectedError, Reason]);
                            _Other -> ok
                           end
    end.

%%--------------------------------------------------------------------
%% @doc Configure NTP
%% Using netconf to cause NTP to be configured
%% Sect 3.1.2 of Use Case description 12/155 56-FAE 151 01 Uen Rev A 2013-03-07
%% @end

configure_ntp(Config)->        
    MeId = proplists:get_value(meId, Config),
    Server = proplists:get_value(ntp_host, Config),
    
    Create = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'SysM',
		  [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
		  [{sysMId,[],["1"]},
		   {'NtpServer', [],
		    [{ntpServerId, ["LabNtpServer"]},
		     {serverAddress , [Server]},
		     {administrativeState, ["LOCKED"]}]}]}]}]},
    
    ok = netconf(edit_config, [nc1, running, Create]),

    Unlock = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'SysM',
		  [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
		  [{sysMId,[],["1"]},
		   {'NtpServer', [],
		    [{ntpServerId, ["LabNtpServer"]},
		     {administrativeState, ["UNLOCKED"]}]}]}]}]},
    
    ok = netconf(edit_config, [nc1, running, Unlock]),

    UserLabel = {'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],[MeId]},
		  {'SystemFunctions',
		   [{systemFunctionsId,[],["1"]},
		    {'SysM',
		     [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
		     [{sysMId,[],["1"]},
		      {'NtpServer', [],
		       [{ntpServerId, ["LabNtpServer"]},
			{userLabel, ["LabNtpServer"]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, UserLabel]),
    
    RemoveLabel =  {'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
		  {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}
		 ],
		 [{managedElementId,[],[MeId]},
		  {'SystemFunctions',
		   [{systemFunctionsId,[],["1"]},
		    {'SysM',
		     [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
		     [{sysMId,[],["1"]},
		      {'NtpServer', [],
		       [{ntpServerId, ["LabNtpServer"]},
			{userLabel, [{'xc:operation', "delete"}], []}
		       ]}]}]}]},

    ok = netconf(edit_config, [nc1, running, RemoveLabel]),

    RemoveServer =  {'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
		  {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}
		 ],
		 [{managedElementId,[],[MeId]},
		  {'SystemFunctions',
		   [{systemFunctionsId,[],["1"]},
		    {'SysM',
		     [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
		     [{sysMId,[],["1"]},
		      {'NtpServer', [{'xc:operation', "delete"}],
		       [{ntpServerId, ["LabNtpServer"]}
		       ]}]}]}]},
    ok = netconf(edit_config, [nc1, running, RemoveServer]).



%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    ok = ct_netconfc:close_session(nc1),
    Res.

%%%--------------------------------------------------------------------
%%% Description: Returns a list of all schema elements
%%%--------------------------------------------------------------------

get_all_schemas(MeId) ->
    Get =  
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SysM',
	     [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	     [{sysMId,[],["1"]}]}]}]},

    {ok, Result} = netconf(get, [nc1, Get]),
    {ok, {_, _, Contents}} = extract_element('SysM', Result),
    [Element||Element<-Contents, 
		 element(1, Element) == 'Schema'].

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

get_ftp_config(sftp) ->
    ct:get_config(sftp_server);
get_ftp_config(ftpes) ->
    [{host, ftpes_test_lib:get_ftpes_test_server_address(ipv4)},
     {username, ftpes_test_lib:get_ftpes_test_server_username()},
     {password, ftpes_test_lib:get_ftpes_test_server_password()}].

get_ftp_file_path(sftp, Config) ->
    ?config(sftp_root, Config);
get_ftp_file_path(ftpes, _Config) ->
    %% should always point to relevant directory
    {ok, LogDir} = rct_ftpes_client:pwd(),
    LogDir.

ftpes_hook() ->
    {rct_ftpes_client, [{port, 21}, {ip, ftpes_test_lib:get_ftpes_test_server_address(ipv4)},
                        {user, ftpes_test_lib:get_ftpes_test_server_username()},
                        {password, ftpes_test_lib:get_ftpes_test_server_password()},
                        {start_session_per_tc, false}]}.

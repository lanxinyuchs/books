%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	com_sysm_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/2
%%% 
%%% @doc == Test Suite for testing the ECIM SysM branch==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(com_sysm_SUITE).
-vsn('/main/R2A/R3A/2').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/6      2014-05-20 etxjotj     Support for ECIM TimeM
%%% R3A/1      2015-05-29 etxkols     Changed rct_netconf hook format 
%%% R3A/2      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([export_models/1,
	 configure_ntp/1,
	 configure_time/1]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [export_models, configure_ntp, configure_time].

%%--------------------------------------------------------------------
%% @doc 
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,nc1},
                 {cth_conn_log,[]},
                 {rct_core,[]}]}].

%% @hidden
init_per_suite(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", PrivDir]),
    %% This is because due to errors in COM there is no proper way to
    %% read the set managedElementId if it has changed from "1"
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
     [{meId, MeId},
      {ntp_host, "10.68.200.3"},
      {sftp_host, "10.68.200.11"},
      {sftp_user, "mauve"},
      {sftp_pass, "dilbert"},
      {sftp_root, PrivDir} | Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:print("Now running ~w~n",[TestCase]),
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.




%%--------------------------------------------------------------------
%% @doc Schema file transfer 
%% Find out which model schemas there are and transfer them to the specified
%% remote host via the export action
%% Sect 3.1.3 and 3.1.4 of 
%% Use Case description 12/155 56-FAE 151 01 Uen Rev A 2013-03-07
%% @end

export_models(Config)->        
    MeId = proplists:get_value(meId, Config),
    Schemas = get_all_schemas(MeId),
    Uri = sftp_uri(Config),
    Password = proplists:get_value(sftp_pass, Config),
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
    [case extract_element(returnValue, Result) of
	 {ok, {returnValue, _, ["Exported"]}} -> ok;
	 {ok, {returnValue, _, [Unexpected]}} ->
	     {ok, {schemaId, _, [SchemaId]}} = 
		 extract_element(schemaId, Result),
	     ct:fail({SchemaId, Unexpected})
     end||{ok, Result}<-Results],
    PrivDir = proplists:get_value(priv_dir, Config),
    {ok, Files} = file:list_dir(PrivDir),
    ct:pal("~p~n",[Files]),
    ok.

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

%%--------------------------------------------------------------------
%% @doc Configure timezone
%% Timezone is a configurable value, but it has no effect on system local time
%% @end

configure_time(Config)->        
    MeId = proplists:get_value(meId, Config),
    
    Set = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'SysM',
		  [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
		  [{sysMId,[],["1"]},
		   {'TimeM', [], 
		    [{timeMId, ["1"]},
		     {'DateAndTime', [], 
		      [{dateAndTimeId, ["1"]},
		       {timeZone, ["Europe/Stockholm"]}]}]}]}]}]},
    
    ok = netconf(edit_config, [nc1, running, Set]),

    Clear = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	       {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'SysM',
		  [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
		  [{sysMId,[],["1"]},
		   {'TimeM', [], 
		    [{timeMId, ["1"]},
		     {'DateAndTime', [], 
		      [{dateAndTimeId, ["1"]},
		       {timeZone, [{'xc:operation', "delete"}], []}]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Clear]).

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
%%% Description: Create a uri from config values
%%%--------------------------------------------------------------------
sftp_uri(Config) ->
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    Dir = proplists:get_value(sftp_root, Config),
    "sftp://"++User++"@"++Host++Dir.

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


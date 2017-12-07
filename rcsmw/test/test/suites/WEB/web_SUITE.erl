%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	web_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R6A/R10A/R11A/2
%%% 
%%% @doc == Test Suite for the RBS web service ==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(web_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R6A/R10A/R11A/2').
-date('2017-10-10').
-author('etxkols').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/3      2013-02-08 etxjotj     Remade
%%% R2A/4      2013-08-09 etxkols     Added printout of downloaded MOMs
%%% R2A/8      2013-08-09 etxkols     Added search for ERROR & CRASH
%%% R3A/1      2015-02-28 etxkols     Preparation for 2 labs
%%% R4A/1      2015-06-02 etxkols     https
%%% R4A/2      2015-07-14 etxjovp     Add group definitions used by CS CI
%%% R6A/1      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% R11A/1     2017-10-09 etxkols     Harder TLS checking in OTP20
%%%                                   -compile([Commented out export_all]).
%%% R11A/2     2017-10-10 etxkols     exported extract_element/2
%%% ----------------------------------------------------------
%%% 

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0]).
-export([get_mim_files_web/1]).
-export([extract_element/2]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_http, www1},
		 {rct_netconf,{nc1, html}},
		 {cth_conn_log, []},
%		 {rct_logging, {all, []}}
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
		]}].



%% @hidden
init_per_suite(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    %% These are for the upcoming test case for retriving models using a
    %% netconf action
    RootDir = filename:join(PrivDir, "sftp_root"),
    os:cmd(["mkdir -p ", RootDir, " ; chmod a+rwx ", RootDir]),
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    [{meId, MeId},
     {sftp_host, SftpHost},
     {sftp_user, Username},
     {sftp_pass, Password},
     %% {sftp_host, "10.68.200.11"},
     %% {sftp_user, "mauve"},
     %% {sftp_pass, "dilbert"},
     {sftp_root, RootDir}| Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []}  
     ].
%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [get_mim_files_web].

%%%--------------------------------------------------------------------
%%% @doc List the known model files and extract them using the onboard webserver
%%% @end
%%%--------------------------------------------------------------------

get_mim_files_web(Config) ->
    MeId = proplists:get_value(meId, Config),
    Dir = proplists:get_value(sftp_root, Config),
    ct:pal("Writing files to: ~p~n",[Dir]),

    {ok, _} = ct_netconfc:open(nc1, []),
    Get = {'ManagedElement',
	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId, [MeId]},
	    {'SystemFunctions', 
	     [{systemFunctionsId, ["1"]},
	      {'SysM',
	       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	       [{sysMId,[],["1"]}]}]}]},

    {ok, SysM} = ct_netconfc:get(nc1, Get),
    ct:pal("~p~n",[SysM]),

    SchemaIds = extract_elements('schemaId', SysM),
    inets:start(),
    [begin
	 {ok, Url} = rct_http:https_url(www1, "/models/"++SchemaName++".xml"),
	 case httpc:request(get,{Url,[]},[{ssl,[{server_name_indication,"test_vc"}]}],[]) of
	     {ok, {_, _, Body}} ->
		 Path = filename:join(Dir, SchemaName++".xml"),
		 ok = file:write_file(Path,Body);
	     AnyHttp ->
		 ct:pal("Unexpected http return on ~p: ~n~p~n",[Url, AnyHttp]),
		 ct:fail(unexpected_http_return)
	 end
		 
     end||{_, _, [SchemaName]}<-SchemaIds],
    Cmd = "ls " ++ Dir,
    ct:pal(Cmd ++ "~n~s",[os:cmd(Cmd)]),
    ok.

%% %%%--------------------------------------------------------------------
%% %%% LIBRARY FUNCTIONS
%% %%%--------------------------------------------------------------------


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


extract_elements(Element, [{Element, Attribute, Content}|Contents]) ->
    [{Element, Attribute, Content}|extract_elements(Element, Contents)];
extract_elements(Element, [{Element, Content}|Contents]) ->
    [{Element, Content}|extract_elements(Element, Contents)];
extract_elements(Element, [{_, _, Content}|Elements]) ->
    extract_elements(Element, Content)++
	extract_elements(Element, Elements);    
extract_elements(Element, [{_, Content}|Elements]) ->
    extract_elements(Element, Content)++
	extract_elements(Element, Elements);
extract_elements(Element, [_|Elements]) ->
    extract_elements(Element, Elements);
extract_elements(_, []) ->
    [].





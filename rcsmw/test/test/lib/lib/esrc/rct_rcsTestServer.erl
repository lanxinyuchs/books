%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	rct_rcsTestServer.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/1

%%% @doc ==Support functions for RCS Test Server==
%%% 
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(rct_rcsTestServer).
-id('Updated by CCase').
-vsn('/main/R3A/1').
-date('2015-01-22').
-author('etxberb').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ------- ---------- -------  ------------------------------------------------
%% R3A/1   2015-01-22 etxberb  Created.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([start/1,
	 stop/1,
	 run/2]).

-export([ct_hook/1,
	 getPid/1]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
%% identifiers used by RCT hooks
-define(RCS, rcs1).

%% General
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Start the sysTestServer process on the RCS node.
%%%
%%% @end
%%% ###=====================================================================###
start(Config) ->
    T = 10000,
    try rct_rpc:call(?RCS, sysTestServer, start, [], T) of
	{ok, TsPid} ->
	    RcsGL = rct_rpc:call(?RCS, sysServer, get_group_leader, [], T),
	    %% IO from the RcsTestServer process is channeled to the Common Test
	    %% node because it is started from 'rct_rpc' and thus is assigned
	    %% the group_leader of the CT node.
	    %% We need to change to the RCS Node group_leader so that e.g.
	    %% faults indicated by ERROR REPORTs are logged on the RCS node as
	    %% they should. Such faults are then detected by the test system in
	    %% the normal way via the 'rct_logging' hook.
	    true = rct_rpc:call(?RCS, erlang, group_leader, [RcsGL, TsPid], T),
	    [{'RcsTestServerPid', TsPid} | Config];
	Error ->
	    ct:fail("~nsysTestServer not started:~n~p", [Error])
    catch
	ErrClass : ErrReason ->
	    ct:fail("~nrct_rpc:call failed:~n{~p, ~p}", [ErrClass, ErrReason])
    end.

%%% ###########################################################################
%%% @doc Stop the sysTestServer process on the RCS node.
%%%
%%% @end
%%% ###=====================================================================###
stop(Config) ->
    T = 10000,
    CommonTestGL = erlang:group_leader(),
    TsPid = getPid(Config),
    %% Redirect IO from the RcsTestServer to CommonTest:
    rct_rpc:call(?RCS, erlang, group_leader, [CommonTestGL, TsPid], T),
    rct_rpc:call(?RCS, sysTestServer, stop, [], T).

%%% ###########################################################################
%%% @doc Run a function in the sysTestServer process on the RCS node.
%%%
%%% @end
%%% ###=====================================================================###
run(MFA, Config) ->
    TsPid = getPid(Config),
    T = 10000,
    try rct_rpc:call(?RCS, sysTestServer, run, [MFA, TsPid, T - 100], T) of
	{ok, Result} ->
	    Result;
	Error ->
	    ct:fail("~n~p", [Error])
    catch
	ErrClass : ErrReason ->
	    ct:fail("~nErrClass: ~p~nErrReason: ~p", [ErrClass, ErrReason])
    end.

%%% ###########################################################################
%%% @doc Get the value of a specified 'ct_hook'.
%%%
%%% @end
%%% ###=====================================================================###
ct_hook(rct_rpc) ->
    ?RCS;
ct_hook(_) ->
    undefined.

%%% ###########################################################################
%%% @doc Get the process identity of the sysTestServer process on the RCS node.
%%%
%%% @end
%%% ###=====================================================================###
getPid(Config) ->
    proplists:get_value('RcsTestServerPid', Config).

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###

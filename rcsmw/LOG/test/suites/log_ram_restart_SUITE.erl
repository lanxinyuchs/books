%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_ram_restart_SUITE.erl %
%%% @author edamkon
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/6
%%%
%%% @doc == Test Suite for testing application logs.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(log_ram_restart_SUITE).

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R5V/1      2016-03-15 edamkon     Created
%%% ----------------------------------------------------------
%%%

-include_lib("common_test/include/ct.hrl").
-include("log_ram_test.hrl").

-export([
  suite/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2,
  all/0,
  groups/0
]).

-export([
  node_restart_file_check/1
]).

-define(LIB, log_ram_test_lib).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
  [{ct_hooks, [
    {rct_rpc, rpc_1},
    {rct_netconf, {nc1, html}}
  ]}].

%% @hidden
init_per_suite(Config) ->
  Config.

%% @hidden
end_per_suite(_Config) ->
  ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
  delete_logs(),
  Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
  print_ram_log_files(Config),
  cleanup(Config).

%% @hidden
groups() ->
  [{default__group, [], all()}].

all() -> [
  node_restart_file_check
].


%% ===========================================================================
%% TEST CASES
%% ===========================================================================

%%--------------------------------------------------------------------
%% @doc
%% - Write RAM Log to disk
%% - Restart the node
%% - Check if the written files are still there
%% @end
%%--------------------------------------------------------------------
node_restart_file_check(_Config) ->
  LogMessage = "written before restart",
  ok = create_log([{zip, false}]),
  ok = write_log({1, LogMessage}),

  %% --------------------------------------
  %% Writing to file is not manually
  %% triggered, since the files should be
  %% written to disk automatically before
  %% the node is restarted.
  %% --------------------------------------
  ok = restart_node(?NODE_RESTART_TIMEOUT),

  %% --------------------------------------
  %% After restarting the node, check if
  %% the logs really were written to disk
  %% before the restart to the node's disk.
  %% --------------------------------------
  ok = check_log([LogMessage]).


%% ===========================================================================
%% HELPER FUNCTION ALIASES
%% ===========================================================================

cleanup(A)                 -> ?LIB:cleanup(A).
write_log(A)               -> ?LIB:write_log(A).
check_log(A)               -> ?LIB:check_log(A).
create_log(A)              -> ?LIB:create_log(A).
delete_logs()              -> ?LIB:delete_logs().
restart_node(A)            -> ?LIB:restart_node(A).
print_ram_log_files(A)     -> ?LIB:print_ram_log_files(A).

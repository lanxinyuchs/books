%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	install_sim_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/2
%%% @doc ==Installs du board and checks software version==
%%% @end
-module(install_sim_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/2').
-date('2014-03-03').
-author('etxkols').
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
%%% R2A/1      2014-02-18 etxkols     Created
%%% R2A/2      2014-03-03 etxkols     Added rct_logging option sim_no_u_flag
%%% ----------------------------------------------------------
%%% JenkinsNode
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         logs/1]).

suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all,sim_no_u_flag]}}]}].

%% @hidden
init_per_suite(Config) -> 
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [logs].

logs(_) ->
    ok.

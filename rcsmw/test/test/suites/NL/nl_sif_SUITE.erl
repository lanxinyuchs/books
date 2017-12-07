%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_sif_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/2

%%% @doc ==Header==
%%% Testcases for semi-auto integration with laptop
%%% @end
-module(nl_sif_SUITE).
-vsn('/main/R3A/2').
-date('2015-02-27').
-author('etxivri').
%% -------------------------------------------------------------------
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2015 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%
%% -------------------------------------------------------------------
%% R3A/1   150227  etxivri  Copied from CAX/test/src and adapted.
%% -------------------------------------------------------------------


-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([gen_and_write_sif/1]).



suite() ->
   [{timetrap, {minutes, 10}},
     {ct_hooks, [{rct_power, power}, %for powering on/off
                 {rct_consserv, consserv}, %reset console
                 {rct_rs232, console}, %console boot cmds
		 %% rct_semi, %% Need to solve how to generate site_basic
                 {rct_scp, scp}]}
    ].


init_per_suite(Config) ->
   Config.
end_per_suite(Config) ->
    Config.



all() ->
  [].
  

%%--------------------------------------------------------------------
%% @doc
%% @spec gen_and_write_sif(Config) -> ok
%% @end
%%--------------------------------------------------------------------
gen_and_write_sif(Config) ->
    ct:pal("TC config ~p~n", [Config]),
    Hwid = ct:get_config({test_nodes,1}),
    ct:pal("Nodename ~p~n", [Hwid]),
    
    ok = nl_lib:gen_and_write_sif("SiteInstallationFile.xml", "RbsSummaryFile.xml", Hwid),
    ok.

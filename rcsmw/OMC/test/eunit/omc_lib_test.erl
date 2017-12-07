
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: omc_lib_test.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%% -----      -------    --------    ------------------------
%%% R7A/1      2016-10-18 ehsake      Created
%%% ----------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(omc_lib_test).
-id('Updated by CCase').
-author('ehsake').
-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



setup() ->
    meck:new(omc_server,[unstick, passthrough]),
    meck:new(omc_tls_server,[unstick, passthrough]),
    meck:new(omc_ldap_server,[unstick, passthrough]),
    Config = [],
    Config.

tear_down(_Config) ->
    meck:unload(omc_server),
    meck:unload(omc_tls_server),
    meck:unload(omc_ldap_server),

    ok.


omc_lib_test_() ->
    { foreach,
      fun setup/0,
      fun tear_down/1,
      [
       {"Test generate esi info", fun test_generate_esi_log/0}
      ]}.


%% ====================================================================
%% Test Cases
%% ====================================================================
    

test_generate_esi_log() ->
    
    
    meck:expect(omc_server,get_sessions, fun() -> "some sessions" end),
    meck:expect(omc_server,get_daemon_refs, fun() -> "some refs" end),
    meck:expect(omc_tls_server,info, fun() -> "some info" end),
    meck:expect(omc_ldap_server,ecoli_aatrace_lookup,1,fun(_) ->"some ldap info" end),
    Result = omc_lib:generate_rollback_esi(),
    ?assertMatch({omcEsi,"ok","/tmp/omc/omc.log"},Result),
    ?assertMatch(true,filelib:is_file("/tmp/omc/omc.log")).

    
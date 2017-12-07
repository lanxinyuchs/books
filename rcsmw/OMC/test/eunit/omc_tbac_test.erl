
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: omc_tbac_test.erl %
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

-module(omc_tbac_test).
-id('Updated by CCase').
-author('ehsake').
-include_lib("eunit/include/eunit.hrl").
-include("omc.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



setup() ->
    meck:new(sysInitI,[unstick, passthrough]),
    meck:expect(sysInitI,warning_msg,2,ok),
    [].

tear_down(_Config) ->
    meck:unload(sysInitI),
    ok.


omc_tbac_test_() ->
    { foreach,
      fun setup/0,
      fun tear_down/1,
      [
       {"TBAC disabled", fun test_tbac_off/0},
       %%{"TBAC enabled wildcard", fun test_tbac_on_wildcard/0},
       {"TBAC enabled", fun test_tbac_on/0}
             
      ]}.


%% ====================================================================
%% Test Cases
%% ====================================================================
    

test_tbac_off() ->
    
    Authentication = ["*"],
    TargetType = [],
    TBAC = false,
    Roles = omc_tbac:authorize(#ldap_params{},TBAC,TargetType,
                               Authentication,get_ldap_roles()),
    ?debugFmt("Roles = ~p ", [Roles]), 
    ?assertEqual(2,length(Roles)),
    ?assert(lists:member("expert1",Roles)),
    ?assert(lists:member("expert",Roles)).
                                

test_tbac_on() ->
    
    Authentication = ["rbs"],
    TargetType = ["rbs"],
    TBAC = true,
    Roles = omc_tbac:authorize(#ldap_params{},TBAC,TargetType,
                               Authentication,get_ldap_roles()),
    ?debugFmt("Roles = ~p ", [Roles]), 
    ?assertEqual(2,length(Roles)),
    ?assert(lists:member("expert1",Roles)),
    ?assert(lists:member("coli",Roles)).


test_tbac_on_wildcard() ->
    
    Authentication = ["*"],
    TargetType = ["rbs"],
    TBAC = true,
    Roles = omc_tbac:authorize(#ldap_params{},TBAC,TargetType,
                               Authentication,get_ldap_roles()),
    ?debugFmt("Roles = ~p ", [Roles]), 
    ?assertEqual(2,length(Roles)),
    ?assert(lists:member("expert1",Roles)),
    ?assert(lists:member("coli",Roles)).



get_ldap_roles() ->
    ["rbs_alias:operator",
     "lookup:what_is_this",
     "rbs:coli",
     "expert",
     "*:expert1"].
    
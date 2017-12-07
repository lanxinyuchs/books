%% @author ehsake
%% @doc @todo Add description to comSnmp_test.


-module(comSnmp_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).




-include("RcsSnmp.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(mnesia,[passthrough,unstick]),
    Config = [],
    Config.

tear_down(_Config) ->
    meck:unload(mnesia),
    ok.


snmp_test_() ->
    { foreach,
      fun setup/0,
      fun tear_down/1,
      [
       {"Test case 1, normal eunit", fun test_snmp_agent_to_prop_list/0},
       {"Test case 2, with meck", fun test_get_agent_params/0}
      ]}.


%% ====================================================================
%% Test Cases
%% ====================================================================

%% Example of testing 
%% Normally it would be extended with more interesting scenarios

test_snmp_agent_to_prop_list() ->
    
    %% Logging to console during test is performed with ?debugMsg or
    %% ?debugFmt macros. io:format(..) will not be printed.

    Res = comSnmp:snmp_agent_to_prop_list(#snmp{agentAddress=undefined}),
    ?debugFmt("Got result ~p", [Res]),

    ?assert(length(Res) == 0).
    

test_get_agent_params() ->
    
    %% mnesia:transaction normally will execute a read or write operation,
    %% since we mock transaction, it is not needed to mock read as it will
    %% never be called. 
    %% In case of mnesia:dirty_read being used the mocking would be
    %% meck:expect(mnesia,dirty_read,fun(_Table,_Key) -> [Rec] end).

    Rec = #snmp{agentAddress=undefined},
    meck:expect(mnesia,transaction,fun(_) -> {atomic,[Rec]} end),
    Params = comSnmp:get_agent_params(),
    ?assert(length(Params) == 0).
    
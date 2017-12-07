%%% %CCaseFile:	uniAndBiDirMoRef_SUITE.erl %
%%% @author ecaiyan
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/4

%%% @doc ==Tests of XYZ==
%%% This test suite exercises the CMSI interface.
%%%
%%% To run the dialyzer on this module:
%%%
%%% dialyzer UniAndBiDirMoRef_SUITE.erl $RCT_TOP/test/lib/rct-proxy/esrc/rct_proxy.erl
%%% @end

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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


-module(uniAndBiDirMoRef_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R11A/1      2017-09-11 ecaiyan     First version
%%% ----------------------------------------------------------

-export([
	 suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0
	]).

-export([
	 setNonExternalUniDirMoRefAttrCli/1,
	 setExternalUniDirMoRefAttrCli/1,
	 setNonExternalUniDirMoRefAttrNetConf/1,
	 setExternalUniDirMoRefAttrNetConf/1
	]).

-include_lib("common_test/include/ct.hrl").

-include("test_cmsi.hrl").

%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].

-define(COMTOP, {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(NETCNF, nc1).


%% @hidden
-spec suite() -> config().

suite() ->
    [
     {timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_netconf,nc1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}},
		 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		 {rct_core,[]},
		 {rct_cli, cli}
		]}
    ].


%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    Config.


%% @hidden
-spec end_per_suite(config()) -> any().

end_per_suite(_Config) ->
    ok.


%% @hidden
-spec init_per_group(atom(), config()) -> config().

init_per_group(_GroupName, Config) ->
    Config.


%% @hidden
-spec end_per_group(atom(), config()) -> any().

end_per_group(_GroupName, _Config) ->
    ok.


%% @hidden
-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    Config.


%% @hidden
-spec end_per_testcase(atom(), config()) -> any().

end_per_testcase(_TestCase, _Config) ->
    ok.



%% @hidden
-spec all() -> list() | {skip, term()}.

all() ->
    [
     setNonExternalUniDirMoRefAttrCli,
     setExternalUniDirMoRefAttrCli,
     setNonExternalUniDirMoRefAttrNetConf,
     setExternalUniDirMoRefAttrNetConf
    ].

%%% ----------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

setNonExternalUniDirMoRefAttrCli(_Config)->
    rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
    rct_cli:send(cli, "configure"),
    rct_cli:send(cli, "LeafConfigReservedMo=1"),
    rct_cli:send(cli, "commit"),
   
    rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
    rct_cli:send(cli, "configure"),
    rct_cli:send(cli, "LeafConfigReserver=1"),
    rct_cli:send(cli, "uniRefToLocalConfigReservedMo1=\"ManagedElement=1,TestRoot=1,LeafConfigReservedMo=1\""),
    rct_cli:send(cli, "commit"),
    

    rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
    rct_cli:send(cli, "LeafConfigReserver=1"),
    {ok, Res} =  rct_cli:send(cli, "show uniRefToLocalConfigReservedMo1"),

    RE = ["\\s*ManagedElement=1,TestRoot=1,LeafConfigReservedMo=1\\s*"],
    re_match(Res, RE).

setExternalUniDirMoRefAttrCli(_Config)->
    rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
    rct_cli:send(cli, "configure"),
    rct_cli:send(cli, "LeafConfigReserver=1"),
    rct_cli:send(cli, "externalUniRefSeqToAny1=[\"ManagedElement=AnotherNode,NonValidMoClass=1\"]"),
    %% rct_cli:send(cli, "externalUniRefSeqToAny1=[\"ManagedElement=AnotherNode,Transport=1\"]"),
    rct_cli:send(cli, "commit"),
    

    rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
    rct_cli:send(cli, "LeafConfigReserver=1"),
    {ok, Res} = rct_cli:send(cli, "show externalUniRefSeqToAny1"),

    RE = ["\\s*ManagedElement=AnotherNode,NonValidMoClass=1\\s*"],
    re_match(Res, RE).

setNonExternalUniDirMoRefAttrNetConf(_Config)->
    {ok, _} = ct_netconfc:open(nc1, []),

    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'TestRoot', [],
				    [{testRootId, [], ["1"]},
				     {'LeafConfigReserver', [], 
				      [{leafConfigReserverId, [], ["1"]}]}
				    ]
                                   }
				  ]
                                 }),
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'TestRoot', [],
				    [{testRootId, [], ["1"]},
				     {'LeafConfigReserver', [], 
				      [{leafConfigReserverId, [], ["1"]},
				       {uniRefToLocalConfigReservedMo1, [], ["ManagedElement=1,TestRoot=1,LeafConfigReservedMo=1"]}]
				     }
				    ]
                                   }
				  ]
                                 }),
    {ok,[{'ManagedElement',
               [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
               [{managedElementId,[],["1"]},
                {'TestRoot',
                    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                    [{testRootId,[],["1"]},
                     {'LeafConfigReserver',[],
                         [{leafConfigReserverId,[],["1"]},
                          {uniRefToLocalConfigReservedMo1,[],
                              ["ManagedElement=1,TestRoot=1,LeafConfigReservedMo=1"]}]}]}]}]} 
	= ct_netconfc:get_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'TestRoot', [],
				    [{testRootId, [], ["1"]},
				     {'LeafConfigReserver', [], 
				      [{leafConfigReserverId, [], ["1"]},
				       {uniRefToLocalConfigReservedMo1,[]}]}
				    ]
                                   }
				  ]
                                 }),
    ok = ct_netconfc:close_session(nc1).

setExternalUniDirMoRefAttrNetConf(_Config)->
    {ok, _} = ct_netconfc:open(nc1, []),

    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'TestRoot', [],
				    [{testRootId, [], ["1"]},
				     {'LeafConfigReserver', [], 
				      [{leafConfigReserverId, [], ["1"]}]}
				    ]
                                   }
				  ]
                                 }),
    %Val=["ManagedElement=AnotherNode,NonValidMoClass=1"],
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'TestRoot', [],
				    [{testRootId, [], ["1"]},
				     {'LeafConfigReserver', [], 
				      [{leafConfigReserverId, [], ["1"]},
				       {uniRefToLocalConfigReservedMo1, [], ["ManagedElement=AnotherNode,NonExistingMoClassOnThisNode=1"]}]
				     }
				    ]
                                   }
				  ]
                                 }),
    {ok,[{'ManagedElement',
               [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
               [{managedElementId,[],["1"]},
                {'TestRoot',
                    [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                    [{testRootId,[],["1"]},
                     {'LeafConfigReserver',[],
                         [{leafConfigReserverId,[],["1"]},
                          {externalUniRefSeqToAny1,[],
                              ["ManagedElement=AnotherNode,NonValidMoClass=1"]}]}]}]}]} 
	= ct_netconfc:get_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'TestRoot', [],
    				    [{testRootId, [], ["1"]},
    				     {'LeafConfigReserver', [], 
    				      [{leafConfigReserverId, [], ["2"]},
    				       {externalUniRefSeqToAny1,[]}]}
    				    ]
                                   }
    				  ]
                                 }),
    ok = ct_netconfc:close_session(?NETCNF).



re_match(Input, RE) ->
    case re:run(Input, RE, [multiline,{newline,any}]) of
	{match,_} ->
	    ct:log("Success. Input: ~p~n  Matched:~p", [Input,RE]),
	    ok;
	nomatch ->
	    ct:log("Failure. Input: ~p~n  Did not match:~p", [Input,RE]),
	    ct:fail({nomatch,Input,RE})
    end.

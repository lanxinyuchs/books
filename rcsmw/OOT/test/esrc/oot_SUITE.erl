%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oot_SUITE.erl %
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/2

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(oot_SUITE).
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
%%% R1A/1      2014-01-28 uabesvi     Created
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0]).


-export([address_1/1]).
-export([address_2/1]).
-export([ldn_1/1]).
-export([ldn_2/1]).
-export([cli_1/1]).
-export([cli_2/1]).
-export([netconf_1/1]).
-export([netconf_2/1]).
-export([cli_netconf_1/1]).


-define(COMTOP,  {xmlns, "urn:com:ericsson:ecim:ComTop"}).


-define(L2B(__L), list_to_binary(__L)).


-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
		  {'nc:operation', "delete"}]).



%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks,
      [
       {rct_netconf, nc1},
       {cth_conn_log,[]}
      ]}].


%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_group(_Group, Config) ->
    Config.

%% @hidden
end_per_group(_Group, _Config) ->
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
    [
     address_1,
     address_2,
     cli_1,
     cli_2,
     netconf_1,
     netconf_2
	  ].


groups() ->
    [].


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------

%%========================================================================
%% address_1(Config) -> ok.
%% 
%% @doc 
%% change IP address
%% @end
%%========================================================================
address_1(_Config) ->
    change_address("100.11.22.33/32").

address_2(_Config) ->
    change_address("200.0.0.44/32").

ldn_1(_Config) ->
    change_ldn("ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1").

ldn_2(_Config) ->
    change_ldn("ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=2").

cli_1(_Config) ->
    change_port(sshPort, 15304).

cli_2(_Config) ->
    change_port(sshPort, 12345).

netconf_1(_Config) ->
    change_port(netconfPort, 15303).

netconf_2(_Config) ->
    change_port(netconfPort, 11111).


cli_netconf_1(_Config) ->
    change_ports([{netconfPort, 11111 }, {sshPort, 12345}]).



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


change_address(Addr) ->
    
    open_trans(),

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], ["1"]},
			      {'Transport',
			       [], 
			       [{transportId, [], ["1"]},
				{'Router',
				 [],
				 [{routerId, [], ["1"]},
				  {'InterfaceIPv4', 
				   [],
				   [{interfaceIPv4Id, [], ["1"]},
				    {'AddressIPv4', 
				     [],
				     [{addressIPv4Id, [], ["1"]}, 
				      {address, [], [Addr]}]}]
				  }]
				}]}]
			    }),

    close_trans().



change_ldn(Ldn) ->
    
    open_trans(),

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], ["1"]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], ["1"]},
				{'SysM',
				 [],
				 [{sysMId, [], ["1"]},
				  {'OamAccessPoint', 
				   [],
				   [{oamAccessPointId, [], ["1"]},
				    {ipv4address, [], [Ldn]}]
				  }]
				}]
			      }]
			    }),

    close_trans().


change_port(Port, Val) ->
    
    open_trans(),

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], ["1"]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], ["1"]},
				{'SysM',
				 [],
				 [{sysMId, [], ["1"]},
				  {'OamAccessPoint', 
				   [],
				   [{oamAccessPointId, [], ["1"]},
				    {Port, [], [integer_to_list(Val)]}]
				  }]
				}]
			      }]
			    }),

    close_trans().


change_ports(PortVals) ->

    NCPortVals = 
	[{Port, [], [integer_to_list(Val)]} || {Port, Val} <- PortVals],
    
    open_trans(),

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], ["1"]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], ["1"]},
				{'SysM',
				 [],
				 [{sysMId, [], ["1"]},
				  {'OamAccessPoint', 
				   [],
				   [{oamAccessPointId, [], ["1"]} |
				    NCPortVals]
				  }]
				}]
			      }]
			    }),

    close_trans().



open_trans()  -> ct_netconfc:open(nc1, []).
close_trans() -> ct_netconfc:close_session(nc1).

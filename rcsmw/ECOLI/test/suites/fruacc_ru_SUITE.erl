%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	fruacc_ru_SUITE.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R5A/R7A/1
-module(fruacc_ru_SUITE).
-id('Updated by CCase').
-vsn('/main/R4A/R5A/R7A/1').
-date('2016-10-27').
-author('uabesvi').
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
%%% R4A/1      2015-07-22 uabesvi     Created
%%% ----------------------------------------------------------
%%% 
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

-export([coli/1]).
-export([tab/1]).
-export([tab_fru/1]).
-export([slash/1]).
-export([local_cmd/1]).
-export([ru_cmd/1]).
-export([ru_cmds/1]).
-export([switch_rus/1]).
-export([root/1]).
-export([root_cmd/1]).
-export([prompt/1]).
-export([one_ldn_many_fru_types/1]).


-include_lib("common_test/include/ct.hrl").


-define(RU,  "ru").
-define(XMU, "xmu").


-define(DU1,  "ManagedElement=1,Equipment=1,FieldReplaceableUnit=Mp1").

-define(RU1,  "ManagedElement=1,Equipment=1,FieldReplaceableUnit=2,DeviceGroup=1").
-define(RU2,  "ManagedElement=1,Equipment=1,FieldReplaceableUnit=3,DeviceGroup=1").
-define(XMU1, "ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,DeviceGroup=7").

-define(DEFAULT_DU,     "'$default_fru_id' MpId = 1").
-define(DEFAULT_REPLY,  "Please, do not disturb ").
-define(DEFAULT_PROMPT, "coli [/]").

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_rpc, rct_netconf, cth_conn_log, rct_logging, rct_tlib
%% @end
%%--------------------------------------------------------------------
suite() -> 
    suite_vc(is_vc_board()).


suite_vc(true) ->
    [{ct_hooks,
      [
       {rct_htmllink, []},
       {rct_consserv, cs1},
       {cth_conn_log, []},
       {rct_ssh,      {ssh,  [manual_connect]}},
       {rct_coli,     {coli, [manual_connect]}},
       {rct_netconf,  {nc1,  man_auth}},
       {rct_cli,      {cli,  [{user,     "SysAdminTest"}, 
			      {password, "SysAdminTest"},
			      manual_connect]}}
      ]}];
suite_vc(false) -> 
    CpuMemory = suite_mem(ct:get_config(node_type), os:getenv("SIM_OR_TARGET")),
    
    [{ct_hooks, 
      [
       {rct_htmllink, []},
       {rct_consserv, cs1},
       {rct_rs232,    console},                 
       {rct_power,    node},
       {rct_snmpmgr,  snmp1},
       {rct_rpc,      rpc},
       {rct_netconf,  nc1},
       {rct_cli,      {cli,  [manual_connect]}},
       {rct_ssh,      {ssh,  [manual_connect]}},
       {rct_coli,     {coli, [manual_connect]}},
       {cth_conn_log, []},
       {rct_tlib,     {kalle, [{cpumemory, CpuMemory}, {cpuload,100}]}},
       {rct_core,     []},
       {rct_logging,  {all, [{erlang,
			      {["ERROR REPORT","CRASH REPORT"], []}}]}}
      ]}].



suite_mem(undefined, "sim") ->
    300;
suite_mem(undefined, _) ->
    Nodes = ct:get_config(ct:get_config({test_nodes,1})),
    case proplists:get_value(board_type, Nodes) of
	"tcu03" ->
	    300;
	"tcu0401" ->
	    300;
	"dus5201" ->  
	    300;
	"dus5301" -> 
	    300;
	"dus3201" ->  
	    300;
	"dus4101" ->
	    260;
	"duw4101" ->
	    260;
	_ ->
	    300
    end;
suite_mem(node_ci_dc_dus, _) ->
    900; %% Remove when node CI changes curl call
suite_mem(node_ci_dc_rbs, _) ->
    900;
suite_mem(node_ci_hc_rbs, _) ->
    900;
suite_mem(node_ci_dc_tcu03, _) ->
    400;
suite_mem(node_ci_hc_tcu03, _) ->
    400.


%% @hidden
init_per_suite(Config) -> 
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(one_ldn_many_fru_types, Config) ->
  Config;

%% @hidden
init_per_testcase(_TestCase, Config) ->
    start(sune),
    ok = hunt(sune),
    ok = register_frus(sune, "xmu, ru"),
    ok = add_fru(sune, ?RU, ?RU1),
    ok = add_fru(sune, ?RU, ?RU2),
    connect(),
    send(?RU1, ?RU1),
    send("\t", ?RU1),
    Config.

%% @hidden
end_per_testcase(one_ldn_many_fru_types, _Config) ->
  ok;

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    disconnect(),
    ok = delete_fru(sune, ?RU, ?RU1),
    ok = delete_fru(sune, ?RU, ?RU2),
    stop(sune),
    ok.

%% @hidden
init_per_group(no_fru_id = Group, Config) ->
    ct:pal("Running group ~p~n", [Group]),
    [{du, {?DEFAULT_DU, "default_fru_id"}} | Config];
init_per_group(du1 = Group, Config) ->
    ct:pal("Running group ~p~n", [Group]),
    du_name(1, list_to_binary(?DU1), primary),
    [{du, {?DU1, ?DU1}} | Config].
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------

all() -> 

    %%=================================================================
    %% If DU1 has a fru id then the no_fru_id group cannot be executed
    %% because that group is to test when there is no fru id for DU1.
    %%=================================================================
    NoFruId = 
	case fruacc_lib:rpc(clhI, fru_id, []) of
	    ?DEFAULT_DU -> 
		[{group, no_fru_id}];
	    _ -> 
		ct:pal("Skipping NoFruId group, Fru name already set~n"),
		[]
	end,


    Sim = NoFruId ++ [{group, du1}],
    Sec = [],

    choose(is_vc_board(), Sim ++ Sec, Sim).




groups() ->
    All = [
	   coli,
	   tab,
	   tab_fru,
	   slash,
	   local_cmd,
	   ru_cmd,
	   ru_cmds,
	   switch_rus,
	   root,
	   root_cmd,
     one_ldn_many_fru_types
	  ],
    [
     {no_fru_id, [], All},
     {du1, [], All}
%%     {no_fru_id, [], All}
    ].

%%--------------------------------------------------------------------
%% @doc 
%% Test a coli command.<br/><br/>
%% @end
%%--------------------------------------------------------------------
coli(_) ->
    send("help coli", "Coli cmd shell usage").



%%--------------------------------------------------------------------
%% @doc 
%% Test tab.<br/><br/>
%% @end
%%--------------------------------------------------------------------
tab(_) ->
    send("\t", "misc/").


%%--------------------------------------------------------------------
%% @doc 
%% Test tab.<br/><br/>
%% @end
%%--------------------------------------------------------------------
tab_fru(_) ->
    send("ManagedElement=1,Equipment=1,FieldReplaceableUnit=\t", "Managed"),
    send("3\t", ?RU2),
    send("\t", ?RU2).
    
%%--------------------------------------------------------------------
%% @doc 
%% Test slash.<br/><br/>
%% @end
%%--------------------------------------------------------------------
slash(_) ->
    send("/", "").


%%--------------------------------------------------------------------
%% @doc 
%% Test tab.<br/><br/>
%% @end
%%--------------------------------------------------------------------
local_cmd(_) ->
    send("dabrowsky/stina", "no such command").


%%--------------------------------------------------------------------
%% @doc 
%% Test tab.<br/><br/>
%% @end
%%--------------------------------------------------------------------
ru_cmd(_) ->
    send(?RU1, ?RU1),
    send("\t", ?RU1),
    send("wollter/justus", ?DEFAULT_REPLY),
    send(?RU2, ?RU2),
    send("\t", ?RU2),
    send("wollter/justus", ?DEFAULT_REPLY).
    

%%--------------------------------------------------------------------
%% @doc 
%% <br/><br/>
%% @end
%%--------------------------------------------------------------------
ru_cmds(_) ->
    ok = replies(sune, ["first", "second", "third"]),
    send(?RU1, ?RU1),
    send("\t", ?RU1),
    send("wollter/justus", "first"),
    send("wollter/justus", "second"),
    send("wollter/justus", "third"),
    send("wollter/justus",  ?DEFAULT_REPLY).


%%--------------------------------------------------------------------
%% @doc 
%% <br/><br/>
%% @end
%%--------------------------------------------------------------------
switch_rus(_) ->
    ok = replies(sune, ["first", "second", "third"]),
    send(?RU1, ?RU1),
    send("\t", ?RU1),
    send("wollter/justus", "first"),
    send(?RU2, ?RU2),
    send("\t", ?RU2),
    send("wollter/justus", "second"),
    send(?RU1, ?RU1),
    send("\t", ?RU1),
    send("wollter/justus",  "third").


%%--------------------------------------------------------------------
%% @doc 
%%.<br/><br/>
%% @end
%%--------------------------------------------------------------------
root(_) ->
    send("/", ""),
    send("\t", "misc").

%%--------------------------------------------------------------------
%% @doc 
%%.<br/><br/>
%% @end
%%--------------------------------------------------------------------
root_cmd(_) ->
    send("wollter", "wollter"),
    send("justus", ?DEFAULT_REPLY),
    send("/misc/info", "Report RC: false"),
    send("/misc/reportrc true", "RC"),
    send("/misc/info", "Report RC: true").


%%--------------------------------------------------------------------
%% @doc 
%%.<br/><br/>
%% @end
%%--------------------------------------------------------------------
prompt(_) ->
    send("/misc", ?RU1),
    send("prompt -s stina>", "stina>"),
    send("prompt -d ", ?RU1).


%%--------------------------------------------------------------------
%% @doc 
%% Test attaching a single LDN to any fru-type.
%% @end
%%--------------------------------------------------------------------
one_ldn_many_fru_types(_Config) ->
    Name      = justus,
    CommonLDN = "ManagedElement=1,"
                "Equipment=1,"
                "FieldReplaceableUnit=both,"
                "DeviceGroup=1",
  
    start(Name),
    ok = hunt(Name),
    ok = register_frus(Name, "xmu, ru"),
    ok = add_fru(Name, ?XMU, CommonLDN),
    ok = add_fru(Name, ?RU,  CommonLDN),

    connect(),

    %% Go to the LDN and check if all the commands
    %% that are configured for both RU and XMU are
    %% valid commands.
    ok = send(CommonLDN, CommonLDN),
    ok = send("\t", "(?=.*lassgaard/)(?=.*nyman/)(?=.*wollter/)"),

    disconnect(),

    ok = delete_fru(Name, ?RU,  CommonLDN),
    ok = delete_fru(Name, ?XMU, CommonLDN),

    stop(Name),
    ok.

%%========================================================================
%% help functions
%%========================================================================

connect() ->
    ok = rct_coli:connect(coli).

disconnect() ->
    ok = rct_coli:disconnect(coli).


send(Cmd, Expected) ->
    {ok, _} = rct_coli:send(coli, Cmd, Expected),
    ok.
   


is_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    ct:get_config({list_to_atom(Hw),secure_board}) == "yes".




start(N)            -> fruacc_lib:start(N).
stop(N)             -> fruacc_lib:stop(N).
hunt(N)             -> fruacc_lib:hunt(N).
register_frus(N, F) -> fruacc_lib:register_frus(N, F).
add_fru(N, T, L)    -> fruacc_lib:add_fru(N, T, L).
delete_fru(N, T, L) -> fruacc_lib:delete_fru(N, T, L).
replies(N, R)       -> fruacc_lib:replies(N, R).

    
choose(true,  T, _) -> T;
choose(false, _, F) -> F.



%% no_fru_id() ->
%%     ok.

du_name(Du, Name, Type) ->
    fruacc_lib:rpc(clhI, associate_mp, [Du, Name, Type]).

%% goto_fru(Fru) ->
%%     send(Fru, Fru).


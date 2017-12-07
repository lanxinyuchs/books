%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	basic_rat_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R9A/R10A/R11A/R12A/2
-module(basic_rat_SUITE).
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
%%% R1A/1      2013-11-18 eransbn     Created
%%% R1A/5      2013-11-18 eransbn     Added tc_wrat_nodeci_configure
%%% R1A/7      2014-05-14 eransbn     Added tc_lrat_nodeci_configure and tc_grat_nodeci_configure
%%%                                   Changed name to basic_rat_SUITE from basic_wrat_SUITE
%%% R1A/12      2014-05-14 eransbn    Added tc_tcu_nodeci_configure
%%% R1A/12      2014-11-27 eransbn    Support for vc card
%%% R3A/4       2014-11-28 etxivri    added a new generic configure. 
%%%                                   Remove reinstall
%%% R3A/7       2015-02-19 eransbn    Added MO:s CustomRule and CustomRole for vc cards
%%% R3A/8       2015-05-21 etxkols    Removed "has terminated" from rct_logging hook
%%% R3A/9       2015-05-26 eransbn    workaround for config sector
%%% R4A/1       2015-06-26 etxivri    update expected answer when asking
%%%                                   jenkins config for installed_type on tcu
%%% R4A/1       2015-10-16  eransbn   Remove reinstall from tcu test 
%%% R4A/4       2015-10-16  eransbn   workaround until this file is fixed at node-ci
%%%                                   netconf_xml/tcu_config.xml
%%% R4A/5       2016-01-20  eransbn   Change info-bank call get_netconf_xml_version()
%%% R5A/2       2016-01-27  etxkols   Remove primaryCoreRef from xml if already configured
%%% R5A/3       2016-01-27  etxkols   Reverting to R5A/1
%%% R5A/4       2016-01-28  etxkols   Try again
%%% R9A/1       2017-04-12  etxivri   Skip if it is cloud.
%%% R10A/6      2017-09-04  etxkols   get_netconf_xml_version() will always pick LATEST xml so we hardcode to LATEST
%%% R11A/1      2017-10-05  etxivri   Increase time tom wait for ENABLED state.
%%% R12A/2      2017-11-22  etxivri   More update for git env
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
	 all/0,
	 tc_reinstall/1,
	 tc_wrat_configure/1,
	 nodeci_configure/1,
	 tc_wrat_nodeci_configure/1,
	 tc_lrat_nodeci_configure/1,
	 tc_grat_nodeci_configure/1,
	 tc_tcu_nodeci_configure/1,
	 tc_bpu_c2_configure/1,
	 get_netconf_xml_version/0 % Just to avoid compiler warning
	]).


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_rpc, rct_netconf, cth_conn_log, rct_logging, rct_tlib
%% @end
%%--------------------------------------------------------------------
suite() ->
    case rat_lib:check_if_vc_board()  of
	"yes"->  [{ct_hooks, [{rct_htmllink,[]},
			      {rct_consserv,cs1},
			      {rct_consserv,cs1},
			      {cth_conn_log,[]},
			      {rct_netconf, {nc1,man_auth}},
			      {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}}
			     ]}];
	_ ->
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,node},
			 {rct_snmpmgr,snmp1},
			 {rct_rpc, rpc},
			 {rct_netconf, nc1},
			 {rct_cli, {cli, [manual_connect]}},
			 {cth_conn_log,[]},
			 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],["has failed to start after 3 attempts within 300 seconds","error: non_empty_attribute_list_expected"]}}]}},
			 {rct_core,[]}]}]
    end.

%%% ----------------------------------------------------------
%%% Definitions
%%% ----------------------------------------------------------
-define(NodeCiXmlVerison, "R1A988").
-define(XmlDir,"/proj/rbs-g2-ci/GIT_repo/" ++ ?NodeCiXmlVerison ++ "/ci-support/netconf_xml/").
%%-define below will always pick latest xml regardless of track, so we may just as well use LATEST since 0 link in get_netconf_xml_version() is broken
%%-define(XmlDirDw2,"/proj/rbs-g2-ci/GIT_repo/" ++ get_netconf_xml_version() ++ "/ci-support/netconf_xml/").
%%-define(XmlDirDw2,"/proj/rbs-g2-ci/GIT_repo/LATEST/ci-support/netconf_xml/").

%%-define(XmlDirDw2,"/vobs/rcs/test/RCT_CRX901275/test/suites/RAT/netconf_xml/").
-define(XmlDirDw2, ct_or_git()).
ct_or_git() ->

    case os:getenv("CLEARCASE_ROOT") of
	false -> ct:log("Git env"), "/proj/rcs-tmp/netconf_xml/";
	Return -> ct:log("clearcase env ~p",[Return]),"/vobs/rcs/test/RCT_CRX901275/test/suites/RAT/netconf_xml/"
    end.

    

%% @hidden
init_per_suite(Config) ->
    case length(ct:get_config(test_nodes)) > 1 of
	true ->
	    {skip,"This is Dual stp"};
	false ->
	    Config
    end. 
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [tc_reinstall,tc_wrat_configure].

tc_reinstall(_)->
    ok = rat_lib:reinstall(rpc, nc1).

nodeci_configure(_Config) ->
    %% %% Get rat type installed on node.
    case os:getenv("SIM_OR_TARGET") of
	"cloudish" ->
	    ct:pal("No nodeci_configure not for Cloud");
	_ ->
	    RatType = ct:get_config({jenkins_config, installed_type}),
	    ct:log("RatType: ~p", [RatType]),
	    case RatType of
		grat ->
		    tc_grat_nodeci_configure(_Config);
		wrat ->
		    tc_wrat_nodeci_configure(_Config);
		lrat ->
		    tc_lrat_nodeci_configure(_Config);
		tcu ->
		    tc_tcu_nodeci_configure(_Config);
		%%New Jenkins
		rat -> case ct:get_config({jenkins_config, pre_bdc_config}) of
			   "lratc2" ->
			       tc_bpu_c2_configure(_Config) ;
			   _Undef -> tc_lrat_nodeci_configure(_Config)
		       end;

		_Undef ->
		    ct:log("No installed_type found: ~p", [_Undef]),
		    ok
	    end
    end,
    ok.

tc_wrat_nodeci_configure(_)->
    case rat_lib:check_if_vc_board() of
    	"yes" -> 
    	    create_mo_CustomRule(nc1),
    	    create_mo_CustomRole(nc1);
    	_ -> ok
    end,
    rat_lib:send_netconf_xml(nc1, ?XmlDirDw2 ++ "config_1_ru_1_sector/WRAT_basic_fru_netconf_create.xml"),
    case is_primaryCoreRef_defined(nc1) of
	false ->
	    ct:pal("primaryCoreRef not configured"),
	    rat_lib:send_netconf_xml(nc1, ?XmlDirDw2 ++ "RBSNC_2_basic_fru_netconf_create_common.xml");
	true ->
	    ct:pal("primaryCoreRef already configured, modifying XML"),
	    XML=remove_primaryCoreRef(?XmlDirDw2 ++ "RBSNC_2_basic_fru_netconf_create_common.xml"),
	    rat_lib:send_netconf_filebinary(nc1, list_to_binary(XML))
    end,
    Result0 =  poll_attribute
     		 (cli,
     		  "operationalState=ENABLED",
     		  "show ManagedElement=1,NodeSupport=1,SectorEquipmentFunction=1,operationalState", 600),
    case Result0 of
     	{ok,_}-> ok;
     	_ -> ct:fail(Result0)
    end,
    ok.

tc_lrat_nodeci_configure(_)->
    case rat_lib:check_if_vc_board() of
    	"yes" -> 
    	    create_mo_CustomRule(nc1),
    	    create_mo_CustomRole(nc1);
    	_ -> ok
    end,
    rat_lib:send_netconf_xml(nc1, ?XmlDirDw2 ++ "config_1_ru_1_sector/LRAT_basic_fru_netconf_create.xml"),
    case is_primaryCoreRef_defined(nc1) of
	false ->
	    ct:pal("primaryCoreRef not configured"),
	    rat_lib:send_netconf_xml(nc1, ?XmlDirDw2 ++ "RBSNC_2_basic_fru_netconf_create_common.xml");
	true ->
	    ct:pal("primaryCoreRef already configured, modifying XML"),
	    XML=remove_primaryCoreRef(?XmlDirDw2 ++ "RBSNC_2_basic_fru_netconf_create_common.xml"),
	    rat_lib:send_netconf_filebinary(nc1, list_to_binary(XML))
    end,
    Result0 =  poll_attribute
     		 (cli,
     		  "operationalState=ENABLED",
     		  "show ManagedElement=1,NodeSupport=1,SectorEquipmentFunction=1,operationalState", 600),
    case Result0 of
     	{ok,_}-> ok;
     	_ -> ct:fail(Result0)
    end,
    timer:sleep(60000),
    ok.
tc_bpu_c2_configure(_)->
    case rat_lib:check_if_vc_board() of
    	"yes" -> 
    	    create_mo_CustomRule(nc1),
    	    create_mo_CustomRole(nc1);
    	_ -> ok
    end,
    rat_lib:send_netconf_xml(nc1, "/home/eransbn/cs/mib/script/config_c2_bpu.xml"),
    
    Result0 =  poll_attribute
     		 (cli,
     		  "operationalState=ENABLED",
     		  "show ManagedElement=1,Equipment=1,FieldReplaceableUnit=AAS-1,RiPort=DATA_2,operationalState", 1300),
    case Result0 of
     	{ok,_}-> ok;
     	_ -> ct:fail(Result0)
    end,
    timer:sleep(60000),
    ok.

tc_grat_nodeci_configure(_)->
    case rat_lib:check_if_vc_board() of
    	"yes" -> 
    	    create_mo_CustomRule(nc1),
    	    create_mo_CustomRole(nc1);
    	_ -> ok
    end,
    rat_lib:send_netconf_xml(nc1, ?XmlDirDw2 ++ "config_1_ru_1_sector/GRAT_basic_fru_netconf_create.xml"),
    case is_primaryCoreRef_defined(nc1) of
	false ->
	    ct:pal("primaryCoreRef not configured"),
	    rat_lib:send_netconf_xml(nc1, ?XmlDirDw2 ++ "RBSNC_2_basic_fru_netconf_create_common.xml");
	true ->
	    ct:pal("primaryCoreRef already configured, modifying XML"),
	    XML=remove_primaryCoreRef(?XmlDirDw2 ++ "RBSNC_2_basic_fru_netconf_create_common.xml"),
	    rat_lib:send_netconf_filebinary(nc1, list_to_binary(XML))
    end,
    Result0 =  poll_attribute
		 (cli,
		  "operationalState=ENABLED",
		  "show ManagedElement=1,NodeSupport=1,SectorEquipmentFunction=1,operationalState", 300),
    case Result0 of
    	{ok,_}-> ok;
    	_ -> ct:fail(Result0)
    end,
    ok.

tc_tcu_nodeci_configure(_)->
    case rat_lib:check_if_vc_board() of
    	"yes" -> 
    	    create_mo_CustomRule(nc1),
    	    create_mo_CustomRole(nc1);
    	_ -> ok
    end,
    case is_primaryCoreRef_defined(nc1) of
	false ->
	    ct:pal("primaryCoreRef not configured"),
	    rat_lib:send_netconf_xml(nc1, ?XmlDirDw2 ++ "tcu_config.xml");
	true ->
	    ct:pal("primaryCoreRef already configured, modifying XML"),
	    XML=remove_primaryCoreRef(?XmlDirDw2 ++ "tcu_config.xml"),
	    rat_lib:send_netconf_filebinary(nc1, list_to_binary(XML))
    end,
    Result0 =  poll_attribute
    		 (cli,
    		  "operationalState=ENABLED",
    		  "show ManagedElement=1,Equipment=1,FieldReplaceableUnit=1", 300),
    case Result0 of
    	{ok,_}-> ok;
    	_ -> ct:fail(Result0)
    end,
    ok.

remove_primaryCoreRef(XML) ->
    os:cmd("cat " ++ XML ++ " | grep -v primaryCoreRef").

is_primaryCoreRef_defined(NC) ->
    {ok,_Reason} = case rat_lib:check_if_vc_board() of
    	"yes" -> ct_netconfc:open(NC, [{user, "SysAdminTest"}, {password, "SysAdminTest"}]);
	    _ -> ct_netconfc:open(NC,[])
    end,
    {ok, Reply} = ct_netconfc:get_config(NC,running,
					 {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					  [{managedElementId,[],["1"]},
					   {'NodeSupport',
					    [{nodeSupportId,[],["1"]},
					     {'MpClusterHandling',
					      [{mpClusterHandlingId,[],["1"]}]}]}]}),
    ok = ct_netconfc:close_session(NC),
    case Reply of
	[{'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'NodeSupport',
	    [{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
	    [{nodeSupportId,[],["1"]},
	     {'MpClusterHandling',
	      [{xmlns,"urn:com:ericsson:ecim:RmeMpClusterHandling"}],
	      [{mpClusterHandlingId,[],["1"]},
	       {primaryCoreRef,[],
		["ManagedElement=1,Equipment=1,FieldReplaceableUnit=1"]}]}]}]}] ->
	    true;
	[{'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'NodeSupport',
	    [{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
	    [{nodeSupportId,[],["1"]},
	     {'MpClusterHandling',
	      [{xmlns,"urn:com:ericsson:ecim:RmeMpClusterHandling"}],
	      [{mpClusterHandlingId,[],["1"]}]}]}]}] ->
	    false;
	Error ->
	    ct:pal("Error ~n~p",[Error]),
	    error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Runs Create mo:s and check that NodeBLocallCell get enabled and then
%%      delete the created mo:s
%% @end
%%--------------------------------------------------------------------
tc_wrat_configure(_)->
    {ok,_Reason} = ct_netconfc:open(nc1,[]),


    {ok,Conf} = ct_netconfc:get_config(nc1,running,
    				       {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    					[{managedElementId,[],["1"]}]}),
    ok = ct_netconfc:close_session(nc1),
    ct:log("Config ~n~p",[Conf]),

    %%Initial config
    %% /proj/rbs-g2-ci/GIT_repo/R1A040/ci-support/netconf_xml/basic_netconf_create.xml
    create_mo_pluginunit(nc1),
%%test_server:break("A"),
    create_mo_auxpluginunit(nc1),
%%test_server:break("A"),
    create_mo_antennaUnitGroup(nc1),
%%test_server:break("A"),
    create_mo_ResAllocFunc(nc1),
    create_mo_SectorEqFunc(nc1),
    Result0 =  poll_attribute
    		(cli,
    		 "operationalState=ENABLED",
    		 "show ManagedElement=1,SectorEquipmentFunction=1,operationalState", 80),
    case Result0 of
    	{ok,_}-> ok;
    	_ -> ct:fail(Result0)
    end,

    Fqband =  get_integer_attribute
    		(cli,"show ManagedElement=1,SectorEquipmentFunction=1, fqBand","fqBand"),
    ct:log("Fqband ~p",[Fqband]),
    UarfcnDl =  get_DlFreq_from_band(Fqband),
    ct:log("UarfcnDl ~p",[UarfcnDl]),
    %% Create cell
    create_mo_NodeBLocalCellGroup(nc1),
    create_mo_NodeBLocalCell(nc1,Fqband ,UarfcnDl),

    %%UnlockCell ENABLED
    change_adm_state_NodebLocalCellGroup(nc1,"UNLOCKED"),
    change_adm_state_NodebLocalCell(nc1,"UNLOCKED"),

    %%Check operationalstate NodebLocalCell
    Result =  poll_attribute
    		(cli,
    		 "operationalState=ENABLED",
    		 "show ManagedElement=1,NodeBFunction=1,NodeBLocalCellGroup=1"
    		 ",NodeBLocalCell=1,operationalState", 80),
    case Result of
    	{ok,_}-> ok;
    	_ -> ct:fail(Result)
    end,

    %%LockCell to be able to delete the mo:s
    change_adm_state_NodebLocalCell(nc1,"LOCKED"),
    change_adm_state_NodebLocalCellGroup(nc1,"LOCKED"),

    %% Delete all Mo:s
    delete_NodebLocalCell(nc1),
    delete_NodeBLocalCellGroup(nc1),
    delete_SectorEqFunc(nc1),
    delete_ResAllocFun(nc1),
    delete_antennaUnitGroup(nc1),
    delete_auxpluginunit(nc1),
    delete_pluginUnit(nc1),

ok.
%%----------------------------------------------------%%
%%           Internal functions     TODO: Comment the functions                   %%
%%----------------------------------------------------%%

%This will always pick latest xml regardless of track, changing 
get_netconf_xml_version()->
    %%Return latest version of xml files from dw2
    Version = os:cmd("/env/rbsg2/bin/blgen -file /env/rbsg2/app/build/0/recipe/TOOL/BL_cis-git.xml --track G2_MAIN --timeout 600"),
    Option = [global, {capture, all, list}],
    RE =  ".version=.([A-Z0-9]+).*",
    case re:run(Version, RE, Option) of
	{match,[[_ALL,Value]]} -> Value;
	{nomatch}->ct:fail("NoMatch  ~p",[Version]);
	{error, ErrType} -> ct:fail(ErrType);
	_ ->ct:fail("Unknown response from excecution from"
		    "\n/env/rbsg2/bin/blgen -file /env/rbsg2/app/build/0/recipe/TOOL/BL_cis-git.xml --track G2_MAIN --timeout 600")
    end.

get_integer_attribute(Cli,Command, Attribute)->
    ok = rct_cli:connect(Cli),
    Re =  ".*\\s+" ++ Attribute ++"="++"(\\d+).*",
    {_,Reply} =  rct_cli:send(Cli, Command),
    ct:log("Reply ~p",[Reply]),
    case  re:run(Reply, Re, [global,{capture, all, binary}]) of
	{match,[[_,Value]]} -> ok = rct_cli:disconnect(Cli),
			       binary:bin_to_list(Value);
	nomatch  -> rct_cli:disconnect(cli),
		    false
    end.

poll_attribute(Cli,ExpectedValue, Command, Timeout) ->
    ct:log("Start poll_attribute ~p ~n poll time ~ps",[Command, Timeout]),
    ok = rct_cli:connect(Cli),%%TODO: if Timeout == 0
    Return = poll_attribute(Cli, ExpectedValue, Command, Timeout, []),
    ok = rct_cli:disconnect(cli),
    ct:log("Poll result ~p",[Return]),
    Return.
poll_attribute(Cli,ExpectedValue, Command, Timeout, _Return) when Timeout > 0 ->
    Received = rct_cli:send(cli,Command , ExpectedValue, noprint),
    case Received  of
	{ok,{_,ExpectedValue}}  -> {ok, ExpectedValue};
	_->
	    %% ct:log("sleep 10sec and poll angain"),
	    timer:sleep(10000),
	    poll_attribute(Cli,ExpectedValue, Command, Timeout-10, Received)
    end;
poll_attribute(_Cli,_ExpectedValue, _Command, Timeout, Return) when Timeout =:= 0->
    Return.
change_adm_state_NodebLocalCellGroup(Nc1,AdmState)->
    ct:log("#-------- ~p NodebLocalCellGroup  --------#",[AdmState]),

 F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		ok =
		       ct_netconfc:edit_config(
			 Nc1,running,
			 {'ManagedElement',
			  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId,[],["1"]},
			   {'NodeBFunction',
			    [{xmlns,"urn:com:ericsson:ecim:NodeBFunction"}],
			    [{nodeBFunctionId,[],["1"]},
			     {'NodeBLocalCellGroup',[],
			      [{nodeBLocalCellGroupId,[],["1"]},
			       {administrativeState,[],[AdmState]}]}]}]}),
		ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
change_adm_state_NodebLocalCell(Nc1,AdmState)->
     ct:log("#-------- ~p NodeBLocalCell --------#",[AdmState]),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],["1"]},
			 {'NodeBFunction',
			  [{xmlns,"urn:com:ericsson:ecim:WratEcimMom"}],
			  [{nodeBFunctionId,[],["1"]},
			   {'NodeBLocalCellGroup',[],
			    [{nodeBLocalCellGroupId,[],["1"]},
			     {'NodeBLocalCell',[],
			      [{nodeBLocalCellId,[],["1"]},
			       {administrativeState,[],[AdmState]}
			      ]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

create_mo_pluginunit(Nc1)->

    ct:log("#-------- Create pluginUnit --------#"),
    F = fun() -> {ok,_} = ct_netconfc:open(nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],["1"]},
			 {'Equipment',
			  [{xmlns,"urn:com:ericsson:ecim:Equipment"}],
			  [{equipmentId,[],["1"]},
			   {userLabel,[],["Equip_1"]},
			   {'PlugInUnit',
			    [{xmlns,"urn:com:ericsson:ecim:PlugInUnit"}],
			    [{plugInUnitId,[],["1"]},
			     {userLabel,[],[]},
			     {administrativeState,[],["UNLOCKED"]},
			     {'SubDeviceGroup',[],
			      [{subDeviceGroupId,[],["1"]},
			       {'CellDeviceSet',[],[{cellDeviceSetId,[],["1"]}]}]},
			     {'SubDeviceGroup',[],
			      [{subDeviceGroupId,[],["2"]},
			       {'RicmDeviceSet',[],[{ricmDeviceSetId,[],["1"]}]}]},
			     {'DeviceGroup',
			      [{xmlns,"urn:com:ericsson:ecim:DeviceGroup"}],
			      [{deviceGroupId,[],["1"]}]},
			     {'RiPort',
			      [{xmlns,"urn:com:ericsson:ecim:RiPort"}],
			      [{riPortId,[],["A"]},
			       {remoteRiPortRef,[],
				["ManagedElement=1,Equipment=1,AuxPlugInUnit=1,RiPort=DATA_1"]}]}]}]}]}),
		 %%check operationalstate not implemented yet
		 %% {ok,_} = poll_attribute
		 %% 	    (cli,
		 %% 	     "operationalState=ENABLED",
		 %% 	     "show ManagedElement=1,Equipment=1,PlugInUnit=1"
		 %% 	     ",operationalState", 80),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

delete_pluginUnit(Nc1)->
    ct:log("#-------- Delete pluginUnit --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       nc1,running,
		       {'ManagedElement',[],
			[{managedElementId,[],["1"]},
			 {'Equipment',[],
			  [{'equipmentId',[],["1"]},
			   {'PlugInUnit',[{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
						{'nc:operation',"delete"}],
			    [{plugInUnitId,[],["1"]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

create_mo_auxpluginunit(Nc1)->
    ct:log("#-------- Create AuxPlugInUnit --------#"),
    F = fun() -> {ok,_} = ct_netconfc:open(nc1,[]),
		 ok =
			ct_netconfc:edit_config(
			  Nc1,running,
			  {'ManagedElement',
			   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			   [{managedElementId,[],["1"]},
			    {'Equipment',
			     [{xmlns,"urn:com:ericsson:ecim:Equipment"}],
			     [{equipmentId,[],["1"]},
			      {userLabel,[],["Equip_1"]},
			      {'AuxPlugInUnit',
			       [{xmlns,"urn:com:ericsson:ecim:AuxPlugInUnit"}],
			       [{auxPlugInUnitId,[],["1"]},
				{administrativeState,[],["UNLOCKED"]},
				{position,[],["0"]},
				{positionInformation,[],["NOT_NULL"]},
				{'DeviceGroup',
				 [{xmlns,"urn:com:ericsson:ecim:DeviceGroup"}],
				 [{deviceGroupId,[],["1"]},
				  {'TrDeviceSet',[],[{trDeviceSetId,[],["1"]}]},
				  {'RfPort',
				   [{xmlns,"urn:com:ericsson:ecim:RfPort"}],
				   [{rfPortId,[],["A"]},
				    {maxCurrent,[],["180"]},
				    {minCurrent,[],["50"]},
				    {userLabel,[],[]},
				    {vswrSupervisionActive,[],["false"]},
				    {vswrSupervisionSensitivity,[],["1"]},
				    {administrativeState,[],["UNLOCKED"]}]},
				  {'RfPort',
				   [{xmlns,"urn:com:ericsson:ecim:RfPort"}],
				   [{rfPortId,[],["B"]},
				    {maxCurrent,[],["180"]},
				    {minCurrent,[],["50"]},
				    {userLabel,[],[]},
				    {vswrSupervisionActive,[],["false"]},
				    {vswrSupervisionSensitivity,[],["1"]},
				    {administrativeState,[],["UNLOCKED"]}]}]},
				{'RiPort',
				 [{xmlns,"urn:com:ericsson:ecim:RiPort"}],
				 [{riPortId,[],["DATA_1"]},
				  {remoteRiPortRef,[],
				   ["ManagedElement=1,Equipment=1,PlugInUnit=1,RiPort=A"]}]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)%% ,
		 %% %%check operationalstate
		 %% {ok,_} = poll_attribute
		 %% 	    (cli,
		 %% 	     "operationalState=ENABLED",
		 %% 	     "show ManagedElement=1,Equipment=1,AuxPlugInUnit=1"
		 %% 	     ",operationalState", 100)
	end,
    try F()
      catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
delete_auxpluginunit(Nc1)->
    ct:log("#-------- Delete AuxPlugInUnit --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       nc1,running,
		       {'ManagedElement',[],
			[{managedElementId,[],["1"]},
			 {'Equipment',[],
			  [{'equipmentId',[],["1"]},
			   {'AuxPlugInUnit',[{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
						{'nc:operation',"delete"}],
			    [{auxPlugInUnitId,[],["1"]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

create_mo_antennaUnitGroup(Nc1)->
    ct:log("#-------- Create AntennaUnitGroup --------#"),
    F = fun() ->  {ok,_} = ct_netconfc:open(nc1,[]),
		  ok =
		      ct_netconfc:edit_config(
			Nc1,running,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],["1"]},
			  {'Equipment',
			   [{xmlns,"urn:com:ericsson:ecim:Equipment"}],
			   [{equipmentId,[],["1"]},
			    {userLabel,[],["Equip_1"]},
			    {'AntennaUnitGroup',
			     [{xmlns,"urn:com:ericsson:ecim:AntennaUnitGroup"}],
			     [{antennaUnitGroupId,[],["1"]},
			      {'RfBranch',[],
			       [{rfBranchId,[],["1"]},
				{dlAttenuation,[],["1"]},
				{rfPortRef,[],
				 ["ManagedElement=1,Equipment=1,AuxPlugInUnit=1,DeviceGroup=1,RfPort=A"]},
				{ulAttenuation,[],["1"]},
				{userLabel,[],[]}]},
			      {'RfBranch',[],
			       [{rfBranchId,[],["2"]},
				{dlAttenuation,[],["1"]},
				{rfPortRef,[],
				 ["ManagedElement=1,Equipment=1,AuxPlugInUnit=1,DeviceGroup=1,RfPort=B"]},
				{ulAttenuation,[],["1"]},
				{userLabel,[],[]}]}]}]}]}),
		  ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
delete_antennaUnitGroup(Nc1)->
    ct:log("#-------- Delete AntennaUnitGroup --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       nc1,running,
		       {'ManagedElement',[],
			[{managedElementId,[],["1"]},
			 {'Equipment',[],
			  [{'equipmentId',[],["1"]},
			   {'AntennaUnitGroup',[{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
						{'nc:operation',"delete"}],
			    [{antennaUnitGroupId,[],["1"]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

create_mo_ResAllocFunc(Nc1)->
    ct:log("#-------- Create ResourceAllocationFunction --------#"),
     F = fun() ->  {ok,_} = ct_netconfc:open(nc1,[]),
		   ok =
		       ct_netconfc:edit_config(
			 Nc1,running,
			 {'ManagedElement',
			  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId,[],["1"]},
			   {'ResourceAllocationFunction',
			    [{xmlns,"urn:com:ericsson:ecim:ResourceAllocationFunction"}],
			    [{resourceAllocationFunctionId,[],["1"]}]}]}),
		   	ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
      catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
delete_ResAllocFun(Nc1)->
  ct:log("#-------- Delete ResourceAllocationFunction --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',[],
			[{managedElementId,[],["1"]},
			 {'ResourceAllocationFunction',
			    [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
				{'nc:operation',"delete"}],
			    [{resourceAllocationFunctionId,["1"]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

create_mo_SectorEqFunc(Nc1)->
    ct:log("#-------- Create SectorEquipmentFunction --------#"),
    %%TODO: Check op state, if unlocked lockit
    F = fun() ->   {ok,_} = ct_netconfc:open(nc1,[]),
		   ok =
		       ct_netconfc:edit_config(
			 Nc1,running,
			 {'ManagedElement',
			  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId,[],["1"]},
			   {'SectorEquipmentFunction',
			    [{xmlns,"urn:com:ericsson:ecim:SectorEquipmentFunction"}],
			    [{sectorEquipmentFunctionId,[],["1"]},
			     {administrativeState,[],["UNLOCKED"]},
			    %% {confOutputPower,[],["20"]},
			     {mixedModeRadio,[],["false"]},
			     {rfBranchRef,[],
			      ["ManagedElement=1,Equipment=1,AntennaUnitGroup=1,RfBranch=1"]},
			     {rfBranchRef,[],
			      ["ManagedElement=1,Equipment=1,AntennaUnitGroup=1,RfBranch=2"]},
			     {userLabel,[],[]}]}]}),
		   ok = ct_netconfc:close_session(Nc1, 10000)
    	end,
    try F()
      catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
delete_SectorEqFunc(Nc1)->
  ct:log("#-------- Delete SectorEquipmentFunction --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',[],
			[{managedElementId,[],["1"]},
			 {'SectorEquipmentFunction',
			    [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
				{'nc:operation',"delete"}],
			    [{sectorEquipmentFunctionId,["1"]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

create_mo_NodeBLocalCellGroup(Nc1)->

    ct:log("#-------- Create NodeBLocalCellGroup --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),

		 ok =
		       ct_netconfc:edit_config(
			 Nc1,running,
			 {'ManagedElement',
			  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId,[],["1"]},
			   {'NodeBFunction',
			    [{xmlns,"urn:com:ericsson:ecim:NodeBFunction"}],
			    [{nodeBFunctionId,[],["1"]},
			     {'NodeBLocalCellGroup',[],
			      [{nodeBLocalCellGroupId,[],["1"]},
			       {administrativeState,[],["LOCKED"]}]}]}]}),
		ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
delete_NodeBLocalCellGroup(Nc1)->
  ct:log("#-------- Delete NodeBLocalCellGroup --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',[],
			[{managedElementId,[],["1"]},
			 {'NodeBFunction',[],
			  [{nodeBFunctionId,[],["1"]},
			   {'NodeBLocalCellGroup',
			    [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
				{'nc:operation',"delete"}],
			    [{nodeBLocalCellGroupId,["1"]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.

create_mo_NodeBLocalCell(Nc1, Fqband, UarfcnDl)->
    ct:log("#-------- Create NodeBLocalCell --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],["1"]},
			 {'NodeBFunction',
			  [{xmlns,"urn:com:ericsson:ecim:WratEcimMom"}],
			  [{nodeBFunctionId,[],["1"]},
			   {'NodeBLocalCellGroup',[],
			    [{nodeBLocalCellGroupId,[],["1"]},
			     {administrativeState,[],["LOCKED"]},
			     {'NodeBLocalCell',[],
			      [{nodeBLocalCellId,[],["1"]},
			       {administrativeState,[],["LOCKED"]},
			       {bandwidthDl,[],["50"]},
			       {bandwidthUl,[],["50"]},
			       {cellRange,[],["35000"]},
			       {localCellId,[],["1"]},
			       {numOfRxAntennas,[],["2"]},
			       {numOfTxAntennas,[],["1"]},
			       {operatingBand,[],[Fqband]},
			       {partOfSectorPower,[],["100"]},
			       {sectorEquipmentFunctionRef,[],
				["ManagedElement=1,SectorEquipmentFunction=1"]},
			       {uarfcnDl,[],[UarfcnDl]},
			       {coherenceMode,[],["2"]}]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
delete_NodebLocalCell(Nc1)->
  ct:log("#-------- Delete NodeBLocalCell --------#"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',[],
			[{managedElementId,[],["1"]},
			 {'NodeBFunction',[],
			  [{nodeBFunctionId,[],["1"]},
			   {'NodeBLocalCellGroup',[],
			    [{nodeBLocalCellGroupId,[],["1"]},
			     {'NodeBLocalCell',
				[{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
				{'nc:operation',"delete"}],
			       [{nodeBLocalCellId,["1"]}]}]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
get_DlFreq_from_band(Fqband)->
    BandFreqList = [{"1","10700"},{"2","9800"},{"3","1338"},{"4","1638"},
		    {"5","4408"},{"6","4400"},{"7","2400"},{"8","3013"},
		    {"9","9312"},{"10","3250"},{"11","3750"},{"12","3873"},
		    {"13","4030"},{"14","4130"},{"19","738"},{"20","4575"},
		    {"21","887"},{"22","4850"},{"25","5263"},{"26","5838"}],
    case proplists:get_value(Fqband,BandFreqList, undefined) of
	undefined -> ct:fail("Radio Band ~p not defined",[Fqband]);
	UarfcnDl -> UarfcnDl
    end.


create_mo_CustomRule(Nc1)->
    ct:log("#-------- Create CustomRule --------#"),
    %%TODO: Check op state, if unlocked lockit
    F = fun() ->   {ok,_} = rat_lib:netconf_open(nc1,[]),
		   ok =
		       ct_netconfc:edit_config(
			 Nc1,running,
			 {'ManagedElement',
			  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId,[],["1"]},
			   {'SystemFunctions',[],
			    [{systemFunctionsId,[],["1"]},
			   {'SecM',
			    [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
			    [{secMId,[],["1"]},
			     {'UserManagement',[],
			      [{userManagementId,[],["1"]},
			       {'LocalAuthorizationMethod',
				[{xmlns,"urn:com:ericsson:ecim:ComLocalAuthorization"}],
				[{localAuthorizationMethodId,[],["1"]},
				 {'CustomRule',[],
				  [{customRuleId,[],["expert"]},
				   {ruleName,[],["expert"]},
				   {permission,[],["RWX"]},
				   {ruleData,[],["ManagedElement,*"]},
				   {userLabel,[],
				    ["Temporary solution for application lab testing on secure boards"]}
			    ]}]}]}]}]}]}),
		   ok = ct_netconfc:close_session(Nc1, 10000)
    	end,
    try F()
      catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
      end.

create_mo_CustomRole(Nc1)->
    ct:log("#-------- Create CustomRole --------#"),

    F = fun() ->   {ok,_} = rat_lib:netconf_open(nc1,[]),
		   ok =
		       ct_netconfc:edit_config(
			 Nc1,running,
			 {'ManagedElement',
			  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId,[],["1"]},
			   {'SystemFunctions',[],
			    [{systemFunctionsId,[],["1"]},
			   {'SecM',
			    [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
			    [{secMId,[],["1"]},
			     {'UserManagement',[],
			      [{userManagementId,[],["1"]},
			       {'LocalAuthorizationMethod',
				[{xmlns,"urn:com:ericsson:ecim:ComLocalAuthorization"}],
				[{localAuthorizationMethodId,[],["1"]},
				 {'CustomRole',[],
				  [{customRoleId,[],["1"]},
				   {roleName,[],["expert"]},
				   {rules,[],
				    ["ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,LocalAuthorizationMethod=1,CustomRule=expert"]},
				   {userLabel,[],
				    ["Temporary solution for application lab testing on secure boards"]}
			    ]}]}]}]}]}]}),
		   ok = ct_netconfc:close_session(Nc1, 10000)
    	end,
    try F()
      catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
      end.




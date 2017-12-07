%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootDataInit.erl %
%%% Author:	etxbjca
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ootDataInit).
%%-behaviour(rcmMnesia). % This ensures that the mandatory callbacks are present
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R9A/R10A/R11A/R12A/2').
-date('2017-11-01').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-01-15   etxbjca   Created
%%% R3A/1      2014-11-10   etxlg     Register in CEC
%%% R3A/2      2014-11-12   etxlg     Added missing ~n to activate info
%%% R3A/3      2014-12-08   etxlg     activate() removed
%%% R3A/4      2015-02-03   etxlg     Run server ootSnmpProxy
%%% R3A/4      2015-02-05   etxlg     Run server ootResolver
%%% R4A/1      2015-09-02   eolaand   Create alternate OamAccessPoint(=2)
%%% R4A/2      2015-09-03   etxpeno   Add extra attributes i OamAccessPoint
%%% R4A/3      2015-09-07   eolaand   Use macros and generalize some fcns.
%%% R4A/6      2015-09-23   eolaand   Don't create OamAccessPoint at upgrade
%%% R4A/7      2015-10-01   eolaand   Rename OamAccessPoint=2 to Alternative
%%% R4A/8      2015-10-13   etxlg     CSTN broken out of ootServer
%%% R4A/9      2015-10-26   eolaand   Add restart_strategy callback
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% ----    ---------- -------  ------------------------------------------------
%%% R9A/1   2017-02-20 etxpeno  Support for ootModel
%%% ----    ---------- -------  ------------------------------------------------
%%% R10A/2  2017-04-25 eolaand  Do not create OamAccessPoint=Alternative in vrcs
%%% ----    ---------- -------  ------------------------------------------------
%%% R11A/1  2017-08-09 eolaand  Remove support of deprecated OAP attributes
%%% R11A/2  2017-08-15 eolaand  Restore support of deprecated OAP attributes
%%% ----    ---------- -------  ------------------------------------------------
%%% R12A/1  2017-10-31 eolaand  Remove support of deprecated OAP attributes 2
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([instPhParallel_init_data/0,
	 instPhParallel_init/1,
	 instPhParallel_post_init/0]).
-export([restart_strategy/0,
	 children/0]).

%-export([upgrade_init/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("oot.hrl").


-define(SAFS_VSN, {safsVersion, $A, 2, 11}).
-define(SIGNATURE, <<"CSTNNETNS">>).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type init_tables()->                                   %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Create the Mnesia tables needed for XXX.
%%% ----------------------------------------------------------
instPhParallel_init(DbNodes)->
    ok = ootModel:init(DbNodes).


%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type init_data()->                                     %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Initiate Mnesia data tables for XXX.
%%% ----------------------------------------------------------
instPhParallel_init_data()->
    %%ok = cec:register(?SIGNATURE, ootServer).

    %% Logs
    %% The internal log is intended as insurance for other block interface
    %% failures
%%     case swmI:is_upgrade_ongoing() of
%% 	true ->
%% 	    ok;
%% 	false ->
%% %	    OptList2 = [{maxSizeKb,        1024},
%% 	    OptList2 = [{maxSizeKb,        1},
%% 			{rotatingSegments, 3},
%% 			{public,           false},
%% 		        {local,            false},
%% 		        {encrypted,        false}],
%% 	    logI:create_log("OotLog", OptList2)
%%     end,

    ok = ootModel:init_data(),

    ok = cec:register(?SIGNATURE, ootCstn).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type post_init()->                                     %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Register things for XXX that require init_data phase
%%%              to have been done.
%%% ----------------------------------------------------------
instPhParallel_post_init()->
    case swmI:is_upgrade_ongoing() of
        false ->
	    imm_init_oap();
        true ->
	    sysInitI:info_msg("~p: Upgrade is ongoing, no need to create "
			      "OamAccessPoint~n", [?MODULE]),
	    ok
    end,

    ok = ootModel:post_init().


imm_init_oap() ->
    {OmHandle, AOHandle, CcbHandle} = ootImmLib:initialize_om_ao_ccb(),
    ok = create_sys_fnc(CcbHandle),
    ok = create_sys_m(CcbHandle),
    case sysEnv:vrcs() of
	false ->
	    ok = create_oap_alt(CcbHandle),
	    sysInitI:info_msg("~p: Created OamAccessPoint ~s~n",
			      [?MODULE, ootLib:to_list(?ALT_OAP_DN)]);
	_ ->
	    ok
    end,
    case ootLib:is_rvnfm() of
	false ->
	    ok = create_oap(CcbHandle),
	    sysInitI:info_msg("~p: Created OamAccessPoint ~s~n",
			      [?MODULE, ootLib:to_list(?OAP_DN)]),
	    ok = safs_imm_om:ccb_apply(CcbHandle);
	_True ->
	    ok
    end,
    ootImmLib:finalize_om_ao_ccb({OmHandle, AOHandle, CcbHandle}),
    ok.

%%% ----------------------------------------------------------
%%% -type upgrade_init()->                                  %#
%%%     ok                                                  %#
%%% Input: Type: atom()
%%%        OldVersion: string()
%%% Output:
%%% Exceptions:
%%% Description: Application code change for XXX.
%%% ----------------------------------------------------------
%%%upgrade_init()->
%%    merge_data_from_old_node(),
%%    ok.

%%% ----------------------------------------------------------
%%% -type activate()->                                      %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions: No longer needed. There is an explicit wait for IMM
%%%		done in ootServer instead.
%%% Description:
%%% ----------------------------------------------------------
%activate() ->
%    sysInitI:info_msg("~p:activate()~n", [?MODULE]),
%    ootServer:init_imm(),
%    ok.
%%% ----------------------------------------------------------
%%% -type restart_strategy()->                              %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: OOT supervisor restart strategy. A crashing child
%%%              will escalate to a warm restart.
%%%              This is a bit hard and should be handled in a nicer way,
%%%              but at the moment a system restart is needed to make
%%%              absolutely sure that OOT is returned to a known state after
%%%              a crash..
%%%
%%% ----------------------------------------------------------
restart_strategy() ->
    {ok, {one_for_one, 0, 1}}.

%%% ----------------------------------------------------------
%%% -type children()->                                      %#
%%%     {ok, ChildSpec}                                     %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
children() ->
    {ok, [{ootServer, {ootServer, start_link, []},
	   permanent, 1000, worker, [ootServer]},
          %%% ootCstn should be rigged to cause warmstart at crash!
	  %%% CSTN need the tn-oam-agent to restart
	  {ootCstn, {ootCstn, start_link, []},
	   permanent, 1000, worker, [ootCstn]},
	  {ootResolver, {ootResolver, start_link, []},
	   permanent, 1000, worker, [ootResolver]},
	  {ootSnmpProxy, {ootSnmpProxy, start_link, []},
	   permanent, 1000, worker, [ootSnmpProxy]}
    ]}.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%===========================================================================
%% create OamAccessPoint instance in imm
%%===========================================================================
create_sys_fnc(CcbHandle) ->
    Attrs = [get_val_string(?SYS_FUNC_ID, "1")],
    ootImmLib:create_imm_object(CcbHandle, ?SYS_FUNC, Attrs).


create_sys_m(CcbHandle) ->
    Attrs = [get_val_string(?SYS_M_ID, "1")],
    ootImmLib:create_imm_object(CcbHandle, ?SYS_M, ?SYS_M_PARENT_DN, Attrs).


create_oap(CcbHandle) ->
    CliPort = get_port(cli),
    NCPort = get_port(netconf),
    ootImm:create_oap(CcbHandle, ?OAP_ID_VAL, CliPort, NCPort).
    %% ootImm:create_oap(CcbHandle, ?OAP_ID_VAL).


create_oap_alt(CcbHandle) ->
    ootImm:create_oap(CcbHandle, ?ALT_OAP_ID_VAL).


get_val_string(Attr, Val) ->
    ootImmLib:build_attr_val(Attr, string, Val).


get_port(Port) ->
    sysEnv:get_initial_port_conf(Port).


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmSchedBuLib.erl %
%%% @author eransbn
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/4

%%% @doc ==Scheduled backup library==
%%% This module contains library functions for doing scheduled backup test
%%% @end

-module(swmSchedBuLib).
-vsn('/main/R2A/4').
-date('2014-10-15').
-author('eransbn').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-04-11   etxjotj     Created
%%% R2A/1      2014-10-09   etxivri     Added is_alarm. To prepare some suites
%%%                                     to be moved to block SWM.
%%% R2A/3      2014-10-09   etxivri     Added new functions.
%%% R2A/4      2014-10-15 eransbn     Updated for vc card
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([set_scheduled_backup_name/2,
	 create_single_event/2,
	 create_periodic_event/2,
	 create_calendar_event/2,
	 is_alarm/3,
	 check_nc_session/1,
	 build_to_up/5,
	 get_sw_version/2
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


set_scheduled_backup_name(MeId, Name) ->
    Set =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {scheduledBackupName, [Name]}]}]}]}]}]},
    netconf(edit_config, [nc1, running, Set]).


%%% ----------------------------------------------------------
%%% @doc Create a BrmSingleEvent
%%% @end
%%% ----------------------------------------------------------

create_single_event(MeId, Attributes) ->
    Config = [{Key, [Value]}||{Key, Value}<-Attributes],
    Set =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {'BrmSingleEvent', [], Config}]}]}]}]}]},

    netconf(edit_config, [nc1, running, Set]).

%%% ----------------------------------------------------------
%%% @doc Create a BrmPeriodicEvent
%%% @end
%%% ----------------------------------------------------------

create_periodic_event(MeId, Attributes) ->
    Config = [{Key, [Value]}||{Key, Value}<-Attributes],
    Set =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {'BrmPeriodicEvent', Config}]}]}]}]}]},

    netconf(edit_config, [nc1, running, Set]).

%%% ----------------------------------------------------------
%%% @doc Create a BrmCalendarBasedPeriodicEvent
%%% @end
%%% ----------------------------------------------------------

create_calendar_event(MeId, Attributes) ->
    Config = [{Key, [Value]}||{Key, Value}<-Attributes],
    Set =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {'BrmCalendarBasedPeriodicEvent', Config}]}]}]}]}]},

    netconf(edit_config, [nc1, running, Set]).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           netconf(Function, Args)
%%% Input: Function:atom()
%%%        Args:[term()]
%%% Output:
%%% Exceptions:
%%% Description: Opens a nc1 channel, and calls a ct_netconfc function
%%% ----------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ct_netconfc:close_session(nc1);
	{ok, Res} ->
	    case ct_netconfc:close_session(nc1) of
		ok ->
		    {ok, Res};
		Other2 ->
		    {error, Other2}
	    end;
	Other ->
	    {error, Other}
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


%%%--------------------------------------------------------------------
%%% Description: Read the value of BrmBackupScheduler.nextScheduledTime
%%%--------------------------------------------------------------------
is_alarm(NcSess, MeId, MinorType) ->
    FmAlarms = get_all_alarms(NcSess, MeId),
    Res = lists:foldr(
	    fun(_FmAlarm, true) ->
		    true;
	       (FmAlarm, false) ->
		    case extract_element(minorType, [FmAlarm]) of
			{ok, {_, _, [MinorType]}} ->
			    ct:pal("FmAlarm MinorType : ~p, Exist.",
				   [MinorType]),
			    true;
			_ ->
			    ct:pal("FmAlarm MinorType : ~p, Not Exist.",
				   [MinorType]),
			    false
		    end
	    end, false, FmAlarms),
    ct:log("Res: ~p",[Res]),
    Res.

get_all_alarms(Nc, MeId) ->
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'Fm',
	       [{fmId,[],["1"]}]}]}]},
    {ok, Result} = netconf(get, [Nc, Get]),
    {ok, {_, _, Contents}} = extract_element('Fm', Result),
    [FmAlarmE||FmAlarmE<-Contents,
	       element(1, FmAlarmE) == 'FmAlarm'].


%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.


%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
check_nc_session(Session) ->
    check_nc_session(Session, 180000).
check_nc_session(_Session, Timeout) when Timeout < 0 ->
    ct:fail("NC session not up within max timeout.");
check_nc_session(Session, Timeout) ->

    %%case ct_netconfc:open(Session, [{timeout, 30000}]) of
case netconf_open(Session, [{timeout, 30000}]) of
	{ok,_} ->
	    ok;
	{error,{connection_exists, _}} ->
	    ok;
	_Err ->
	    ct:log("nc_session not open: ~p. Sleep and try again",[_Err]),
	    timer:sleep(5000),
	    check_nc_session(Session, Timeout-5000)
    end.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
%%% ===========================================================================
%%% @doc
%%% Build upgrade packages using same deleivery that is installed on Node.<br/>
%%% CXS rev is steped. Using  rcs_upmod_and_upgradeprep script. <br/>
%%% NC_Session = atom() <br/>
%%% ModScriptPath = string() , path to mod script <br/>
%%% ModScriptName = string() , mod script name <br/>
%%% @spec build_to_up(NC_Session,ModScriptPath,ModScriptName,MeId,UgHook) -> ok
%%% @end
%%% ===========================================================================
build_to_up(NC_Session, ModScriptPath, ModScript, MeId, UgHook) ->
    {StpName, CXS_LABEL} = get_info_to_build_up(NC_Session, MeId, UgHook),
    ct:pal("Active ProductRevision: ~p , before upgrade.",[CXS_LABEL]),

    ct:pal("Remove existing files in upgrade dir",[]),
    C_M_D = "chmod 777 /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    ct:pal("CMD:~n~p~n",[C_M_D]),
    os:cmd(C_M_D),
    CMD = "rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    ct:pal("CMD:~n~p~n",[CMD]),
    os:cmd(CMD),

    timer:sleep(5000),

    ct:log("# Run script that step unpack CXS and step REV nr with 1, ~n"
	   "Also update DUMMY CXP REV with 1. ~n"
    	   "then build CXS again. ~n"
    	   "After that it will be moved to UGPath unpacked.",[]),

    CMD1 = "rcs_upmod_and_upgradeprep.sh"
    	" -n " ++ StpName ++
    	" -m " ++ ModScriptPath ++ ModScript ++
    	" -r " ++ CXS_LABEL,
    ct:pal("CMD1:~n~p~n",[CMD1]),

    os:cmd(CMD1),

    B = os:cmd("ls -l /proj/rcs-tmp/upgrade/"++StpName),
    C = string:tokens(B,"\n"),
    ct:log("ls -l ~p:~n~p~n",["/proj/rcs-tmp/upgrade/"++StpName, C]).

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
get_info_to_build_up(NC_Session, MeId, UgHook) ->
    SW_Vers = get_sw_version(NC_Session, MeId),
    ct:log("SW_Vers:~n~p~n",[SW_Vers]),

    %% Construct a CXS label.
    [CXS, Index, Label] = string:tokens(SW_Vers, "/-"),
    ct:log("CXS:~p, Index: ~p, Label: ~p ~n",[CXS, Index, Label]),
    CXS_LABEL = CXS++"_"++Index++"-"++Label,

    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(UgHook),
    ct:log("UGPath:~n~p~n",[UGPath]),
    StpName = lists:last(string:tokens(UGPath, "/")),
    ct:log("StpName: ~p", [StpName]),

    %% B = os:cmd("ls -l "++UGPath),
    %% C = string:tokens(B,"\n"),
    %% ct:log("ls -l ~p:~n~p~n",[UGPath, C]),

    {StpName, CXS_LABEL}.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
get_sw_version(Session, MeId) ->
    Get_SwInventory = {'ManagedElement',
    		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    		       [{managedElementId,[],[MeId]},
    			{'SystemFunctions',
    			 [{systemFunctionsId,[],["1"]},
    			  {'SwInventory',[],
    			   [{swInventoryId,[],["1"]},
    			    {'SwVersion',[],[]}
    			   ]}]}]},

    case  netconf(get_config, [Session, running, Get_SwInventory]) of
	{ok, [{'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],["1"]},
          {'SystemFunctions',[],
              [{systemFunctionsId,[],["1"]},
               {'SwInventory',
                   [{xmlns,"urn:com:ericsson:ecim:RcsSwIM"}],
		SwInventory
	       }]}]}]} ->
	    ct:pal("### ~p", [SwInventory]),

	    SwVersions = lists:keydelete(swInventoryId, 1 , SwInventory),
	    ct:pal("SwVersions = ~p~n",[SwVersions]),
	    Sw_Versions =
		[X || {'SwVersion',[],[{swVersionId,[],[X]}]} <- SwVersions],
	    [SwVersion|_T] = Sw_Versions,
	    ct:pal("### SwVersion ~p", [SwVersion]),
	    SwVersion;
	{error, Error} ->
	    ct:pal("get_sw_version Error = ~p~n",[Error]),
	    ct:fail("get_sw_version Error"),
	    _SwVersion = dummy
    end.


%% Return sim,not_sec_card or sec_card
%% Prepare SUITE to be moved to block SWM,
%% exist also in swm_test_lib.
check_kdu()->
    case os:getenv("SIM_OR_TARGET") of
	"sim" -> sim;
	_-> case string:str(ct:get_config({list_to_atom(atom_to_list(ct:get_config({test_nodes,1}))),product_no}),"/11") of
		0 -> not_sec_card;
		_-> sec_card
	    end
    end.

netconf_open(Session, Param)->
    case check_kdu()  of
	TARGET when TARGET == sim;
		    TARGET == not_sec_card -> ct_netconfc:open(Session,Param);
	sec_card ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param])
    end.

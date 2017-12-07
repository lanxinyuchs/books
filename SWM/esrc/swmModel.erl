%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmModel.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R9A/R10A/R11A/R12A/4
%%%
%%% @doc ==Software management model==
%%% This module holds the agent implementation of the ECIM SwM model.
%%% The actual action implementation is made in swmServer.
%%% @end

-module(swmModel).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R9A/R10A/R11A/R12A/4').
-date('2017-11-22').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-01-10 etxbjca     Created
%% ----    ---------- -------  ------------------------------------------------
%%% R2A/18     2013-10-17 erarafo     Support for new Oam Mgmt SPI in COM
%%% R2A/30     2014-04-16 etxjotj     Added removeSoftwareVersion
%%% R2A/31     2014-04-28 etxjotj     Reset uri and password at update tables
%%% R2A/34     2014-05-13 etxjotj     Bugfix for up state handling
%%% R2A/35     2014-05-16 etxjotj     Handle corrupt metadata files
%%%                                   Remove support for cxs*.xml pattern
%%% R2A/36     2014-05-22 erarafo     HS62272
%%% R2A/37     2014-06-05 etxjotj     Support for ee split detection
%%% R2A/38     2014-06-12 etxjotj     EE split, progress reporting fix
%%% R2A/39     2014-07-16 etxjotj     Fix of HS67412, prevent incomplete MoRefs
%%% R2A/40     2014-07-21 etxjotj     Handling of optional meta elements
%%% R2A/41     2014-07-22 etxjotj     Fix of HS76792: synchronous confirm
%%% R2A/42     2014-07-24 etxjotj     Dialyzer fix
%%% R2A/43     2014-10-10 etxjotj     Cleaning out old commit code
%%% R2A/44     2014-11-07 etxjotj     Fix of HT21368: Node release 14B
%% ----    ---------- -------  ------------------------------------------------
%%% R3A/1      2014-11-07 etxjotj     HT21368: Set node release value from UP
%%% R3A/2      2014-11-28 etxberb     Added values/1.
%%% R3A/3      2014-11-28 etxjotj     ECIM SwM 3.0
%%% R3A/5      2015-06-11 etxjotj     UP init state fix
%% ----    ---------- -------  ------------------------------------------------
%%% R4A/3      2015-07-22 etxjotj     HT84852 Update release info correctly
%%% R4A/4      2015-08-21 etxpejn     Changed logI:write_log to swmLib:write_swm_log
%%% R4A/5      2015-08-28 etxjotj     Dialyzer fix
%%% R4A/6      2015-08-31 etxjotj     Use sysInitI for printouts
%%% R4A/7      2015-11-26 etxjotj     Max UP limit
%%% R4A/8      2015-11-27 etxjotj     HU37957 Active UP is always committed
%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1      2016-03-11 etomist     Added ActionCapability lock/unlock
%%% R5A/2      2016-03-16 etomist     Add lock_fail exception handling to action()
%%% R5A/4      2016-03-28 etxberb     Corrected unsafe try clause in
%%%                                   update_sw_version/2.
%%% R5A/5      2016-04-04 etxpejn     Added io:format for deuging 
%%% R5A/6      2016-04-06 etxpejn     Removed io:format 
%%% R5A/7      2016-04-14 etxjotj     Don't use try for mnesia ops
%%% ----    ---------- -------  ------------------------------------------------
%%% R9A/2   2017-05-10 etxberb  Added merge_with_dets/1.
%%% ------- ---------- -------  ------------------------------------------------
%%% R10A/1  2017-06-07 etxjotj  Removed obsolete removeSwVersion
%%% R10A/2  2017-07-03 etxjotj  HV97842 Look at boardlist for UpComplete test
%%% R10A/3  2017-07-05 etxjotj  Don't do complete check in vrcs  
%%% R10A/4  2017-08-11 etxjotj  Merge from R11A/1
%%% R10A/5  2017-08-28 etxjotj  HW23495 Fix UP complete check when HAL is used
%%% ------- ---------- -------  ------------------------------------------------
%%% R11A/1  2017-07-26 etxberb  HV97842 & HW14932: Adding boardTypesOTH.
%%% ------- ---------- -------  ------------------------------------------------
%%% R12A/1  2017-11-15 eivirad  HW44673 Node was crashing in prepare UP with different board type
%%% R12A/2  2017-11-17 etxpejn  Changed get_stacktrace order in action
%%% R12A/3  2017-11-22 etxberb  HW46038: Match on 'Uri /= undefined' in
%%%                             merge_with_dets/1.
%%% ------- ---------- -------  ------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).
						%-export([action/3]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 action/4,
	 createMo/5]).

-export([update_tables/0, update_sw_version/1]).
%% -export([set_active_version/1]).
-export([get_matching_up/1]).
-export([backup/0]).
						%-export([package_state_test/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("comte_types.hrl").
-include("RcsSwM.hrl").
-include("RcsBrM.hrl").
-include("SwmInternal.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% @doc Returns true if the specified instance exists.
%%% @end
%%% ----------------------------------------------------------

-spec existsMo([binary()], integer()) -> boolean().

existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%%% ----------------------------------------------------------
%%% @doc Returns the number of MO instances of given class directly below the specified parent.
%%% ----------------------------------------------------------

-spec countMoChildren([binary()], binary(), integer()) -> non_neg_integer().

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).


%%% ----------------------------------------------------------
%%% @doc Gets MO attribute values. 
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% ----------------------------------------------------------

getMoAttributes(AttrNames, DnRev, TxHandle) ->
    [getMoAttribute([AttrName|DnRev], TxHandle)||AttrName<-AttrNames].



						%getMoAttribute([Attribute|DnRev], TxHandle) ->

getMoAttribute([<<"timeRemainingBeforeFallback">>|_DnRev], _) ->
    case swmLib:get_ram_variable(timeRemainingBeforeFallback) of
	undefined -> comsaEcimModelAdaptor:type(int16,-1);
	Value -> comsaEcimModelAdaptor:type(int16, Value)
    end;    

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).




						%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).


setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).


setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


%%% ----------------------------------------------------------
%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.
%%% ----------------------------------------------------------

-spec createMo([binary()], 
	       mo_attribute_name(), 
	       binary(), 
	       [com_named_attribute()], 
	       integer()) -> 
		      {ok, tuple()}.

createMo([ClassName | ParentDnRev],
	 _KeyAttrName,
	 KeyValue,
	 InitAttrs,
	 _TransId) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).


deleteMo(_, _) ->
    ok.

table("SwM") -> swM;
table("UpgradePackage") -> upgradePackage.
%% table("SwVersionMain") -> swVersionMain.

types(swM) -> ?swM_types;
types(upgradePackage) -> ?upgradePackage_types.
%% types(swVersionMain) -> ?swVersionMain_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

action(Name = <<"createUpgradePackage">>, DnRev, Params, TransId) ->
    try do_action(Name, DnRev, Params, TransId) 
    catch 
	throw:{fail, Reason} ->
	    %% Indicates a fault on behalf of the operator
	    swmLib:unlock_action_capable(?CREATE_UP_ACTION_CAPABLE_ID),
	    {error, list_to_binary(Reason)};
	Type:Reason ->
	    Stacktrace = erlang:get_stacktrace(),
	    swmLib:unlock_action_capable(?CREATE_UP_ACTION_CAPABLE_ID),
	    sysInitI:error_report(
	      [{Type, Reason}, {stacktrace, Stacktrace}]),
	    ?UINT16(0)
    end;
action(Name, DnRev, Params, TransId) ->
    try do_action(Name, DnRev, Params, TransId)
    catch 
	throw:{fail, Reason} ->
	    swmLib:unlock_action_capable([?REMOVE_UP_ACTION_CAPABLE_ID,
					  ?PREPARE_UP_ACTION_CAPABLE_ID,
					  ?VERIFY_UP_ACTION_CAPABLE_ID,
					  ?ACTIVATE_UP_ACTION_CAPABLE_ID,
					  ?CONFIRM_UP_ACTION_CAPABLE_ID]),
	    %% Indicates a fault on behalf of the operator
	    {error, list_to_binary(Reason)};
	throw:{lock_fail, Reason} ->
	    {error, list_to_binary(Reason)};
	Type:Reason ->
	    Stacktrace = erlang:get_stacktrace(),
	    swmLib:unlock_action_capable([?REMOVE_UP_ACTION_CAPABLE_ID,
					  ?PREPARE_UP_ACTION_CAPABLE_ID,
					  ?VERIFY_UP_ACTION_CAPABLE_ID,
					  ?ACTIVATE_UP_ACTION_CAPABLE_ID,
					  ?CONFIRM_UP_ACTION_CAPABLE_ID]),
	    sysInitI:error_report(
	      [{Type, Reason}, {stacktrace, Stacktrace}]),
	    {error, <<"A software error occurred">>}
    end.


do_action(<<"createUpgradePackage">>, _, Params, _) ->
    UBin = get_value("uri", Params),
    PBin = get_value("password", Params, ?STRING(<<"">>)),
    Passwd = binary_to_list(PBin),
    Uri = binary_to_list(UBin),
    allow_create(),
    swmLib:mo_lock_action_capable(?CREATE_UP_ACTION_CAPABLE_ID,
                                  ?CREATE_UP_ACTION_CAPABLE_INFO),
    ActionId = swmServer:create_package(Uri, Passwd),
    ?UINT16(ActionId);
do_action(<<"createUpgradePackageFromUri">>, _, _, _) ->
    swmServer:update_progress(
      [{additionalInfoClear, 
	"The createUpgradePackageFromUri action is not supported"},
       {result, ?ActionResultType_FAILURE},
       {resultInfo, "The action was not executed"},
       {state, ?ActionStateType_FINISHED}]),
    ?BOOL(true);
do_action(<<"createUpgradePackageLocal">>, _, _, _) ->
    swmServer:update_progress(
      [{additionalInfoClear, 
	"The createUpgradePackageLocal action is not supported"},
       {result, ?ActionResultType_FAILURE},
       {resultInfo, "The action was not executed"},
       {state, ?ActionStateType_FINISHED}]),
    ?BOOL(true);
do_action(<<"createUpgradePackageRemote">>, _, _, _) ->
    swmServer:update_progress(
      [{additionalInfoClear, 
	"The createUpgradePackageRemote action is not supported"},
       {result, ?ActionResultType_FAILURE},
       {resultInfo, "The action was not executed"},
       {state, ?ActionStateType_FINISHED}]),
    ?BOOL(true);
%% HS67412 fix
do_action(<<"removeUpgradePackage">>, _, Params, _) ->
    swmLib:mo_lock_action_capable(?REMOVE_UP_ACTION_CAPABLE_ID,
                                  ?REMOVE_UP_ACTION_CAPABLE_INFO),
    UpDn = binary_to_list(get_value("upgradePackage", Params)),
    case UpDn of
	"ManagedElement"++_ ->
	    swmServer:remove_package(UpDn),
	    ?BOOL(true);
	_ ->
	    throw({fail, "Incomplete reference: "++UpDn})
    end;    
%% HS51877 fix
do_action(<<"removeSoftwareVersion">>, _, Params, _) ->
    VsnDn = binary_to_list(get_value("swVersion", Params)),
    case VsnDn of
	"ManagedElement"++_ ->
	    swmServer:remove_software_version(VsnDn),
	    ?BOOL(true);
	_ ->
	    throw({fail, "Incomplete reference: "++VsnDn})
    end;
%% HS51877 ends here
%% HS67412 ends here
%% We do not care about incomplete references in this clause because
%% it is deprecated and we will not act upon it
%% do_action(<<"removeSwVersion">>, _, Params, _) ->
%%     VsnDnBin = get_value("swVersion", Params),
%%     swmServer:remove_sw_version(binary_to_list(VsnDnBin)),
%%     ?BOOL(true);
do_action(<<"prepare">>, DnRev, _, _) ->
    swmLib:mo_lock_action_capable(?PREPARE_UP_ACTION_CAPABLE_ID,
                                  ?PREPARE_UP_ACTION_CAPABLE_INFO),
    swmServer:prepare_package(comsaGeneric:dnrev_to_key(DnRev)),
    ?BOOL(true);
do_action(<<"verify">>, DnRev, _, _) ->
    swmLib:mo_lock_action_capable(?VERIFY_UP_ACTION_CAPABLE_ID,
                                  ?VERIFY_UP_ACTION_CAPABLE_INFO),
    swmServer:verify_package(comsaGeneric:dnrev_to_key(DnRev)),
    ?BOOL(true);
do_action(<<"activate">>, DnRev, _, _) ->
    swmLib:mo_lock_action_capable(?ACTIVATE_UP_ACTION_CAPABLE_ID,
                                  ?ACTIVATE_UP_ACTION_CAPABLE_INFO),
    swmServer:activate_package(comsaGeneric:dnrev_to_key(DnRev)),
    ?BOOL(true);
%% SwM 4.0
%% do_action(<<"commit">>, DnRev, _, _) ->
%%     swmServer:confirm_package(comsaGeneric:dnrev_to_key(DnRev)),
%%     ?BOOL(true);
do_action(<<"confirm">>, DnRev, _, _) ->
    swmLib:mo_lock_action_capable(?CONFIRM_UP_ACTION_CAPABLE_ID,
                                  ?CONFIRM_UP_ACTION_CAPABLE_INFO),
    swmServer:confirm_package(comsaGeneric:dnrev_to_key(DnRev)),
    ?BOOL(true);
do_action(<<"cancel">>, DnRev, _, _) ->
    swmServer:cancel(comsaGeneric:dnrev_to_key(DnRev)),
    ?BOOL(true).

allow_create() ->
    case length(ets:tab2list(upgradePackage)) of
	NoOfUps when NoOfUps < 3 -> 
	    ok;
	_ ->
	    case swmLib:get_ram_variable(disable_max_up_check) of
		true ->
		    ok;
		_ ->
		    Msg = set_create_fail_msg(),
		    throw({fail, Msg})
	    end
    end.

set_create_fail_msg() ->
    Msg1 = "Remove an upgrade package before creating another one.",
    Msg2 = "Proceed with AutoProvisioning::rbsConfigLevel to "
	"READY_FOR_SERVICE or beyond.",
    Protected = aicI:get_backup_name(),
    WP = mnesia:table_info(brmBackup, wild_pattern),
    Pattern = WP#brmBackup{backupName = Protected},
    case mnesia:transaction(fun() -> mnesia:match_object(Pattern) end) of
	{atomic, []} -> 
	    throw({fail, Msg1});
	{atomic, BuList} when length(BuList) > 0 ->
	    throw({fail, Msg1++" "++Msg2});
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [])
    end.



get_value(Key, Params) ->
    case proplists:get_value(list_to_binary(Key), Params) of
	{_, Value} ->
	    Value;
	_ ->
	    swmLib:write_swm_log("SwM", error, "Missing parameter "++Key),
	    throw(error)
    end.
get_value(Key, Params, Default) ->
    case proplists:get_value(list_to_binary(Key), Params, Default) of
	{_, Value} ->
	    Value;
	_ ->
	    swmLib:write_swm_log("SwM", error, "Missing parameter "++Key),
	    throw(error)
    end.

%% set_active_version(Key) -> 
%%     swmLib:db_op(fun() -> do_set_active_version(Key) end).

%% do_set_active_version(Key) ->
%%     Active = 
%% 	["ManagedElement=1,SystemFunctions=1,SwM=1,SwVersionMain="++Key],
%%     [SwM] = mnesia:read({swM, {"1","1","1"}}),
%%     mnesia:write(SwM#swM{activeSwVersion = Active}).


%%% ----------------------------------------------------------
%%% @doc 
%%%  Update the upgradePackage and swVersionMain tables
%%%  The upgradePackage table describes the contents of 
%%%  $RCS_ROOT/rcs/swm/archive
%%%  The swVersionMain table describes the top level of 
%%%  $RCS_ROOT/home/$USER/swm and
%%%  $RCS_ROOT/home/$USER/software
%%% @end
%%% ----------------------------------------------------------
-spec update_tables() -> string(). 

update_tables() ->
    case mnesia:transaction(fun() -> do_update_tables() end) of
	{atomic, Active} -> 
	    backup(),
	    Active;
	{aborted, Reason} -> 
	    erlang:error({aborted, Reason})
    end.

do_update_tables() ->
    %% scratch_table(swVersionMain),
    SwVsnPattern = filename:join(swmLib:software_dir(), "*-up.xml"),
    [ActiveKey] = update_sw_versions(filelib:wildcard(SwVsnPattern)),

						%    scratch_table(upgradePackage),
    AllUPs = mnesia:all_keys(upgradePackage),
    UpPattern = filename:join([swmLib:archive_dir(), "*", "*-up.xml"]),
    update_packages(ActiveKey, AllUPs, filelib:wildcard(UpPattern)),
    ActiveKey.

%% The design of the software currently allows only one active UP
update_sw_versions([UpFile|UpFiles]) ->
    case update_sw_version(UpFile, active) of
	undefined ->
	    update_sw_versions(UpFiles);
	Key ->
	    [Key|update_sw_versions(UpFiles)]
    end;
update_sw_versions([]) ->
    [].

update_sw_version(UpFile) ->
    update_sw_version(UpFile, other).

update_sw_version(UpFile, ActiveState) ->
    case xmerl_scan:file(UpFile) of
	{ConfigurationE, []} ->
	    ProductE = find_element(product, ConfigurationE),
	    ProdId = find_attribute(id, ProductE),
	    Version = find_attribute(version, ProductE),
	    TypeE = try find_element(type, ConfigurationE) 
		    catch _:_ -> ""
		    end,
	    %% HT21368
	    ReleaseE = try find_element(release, ConfigurationE)
		       catch _:_ -> ""
		       end,
	    %%HT84852 Update release only only for the active UP
	    case ActiveState of
		active -> 
		    info_msg("Setting node type to ~p (~p)~n",
			     [find_text(TypeE), find_text(ReleaseE)]),
		    comsaI:set_node_type(find_text(TypeE), find_text(ReleaseE));
		other ->
		    ok
	    end,

	    swmInventory:make_mom_key(ProdId, Version);
	ScanRes ->

     	    Dir = filename:basename(filename:dirname(UpFile)),
     	    logI:write_log("SwM", "SwM", error, 
     			   "Corrupt UP file in "++Dir),
	    UpFiles =
		filelib:wildcard(filename:join(swmLib:software_dir(),
					       "*-up.xml")),
	    sysInitI:error_report([{xmerl_scan, file, [UpFile]},
				   {result, ScanRes},
				   {upFiles, UpFiles},
				   file:read_file_info(UpFile)]),
	    UpDir = filename:dirname(UpFile),
	    ArchiveDir = filename:dirname(UpDir),
	    LsCmds = ["ls -la " ++ ArchiveDir,
		      "ls -la " ++ UpDir],
	    [io:format("~s~n~s~n", [Ls, os:cmd(Ls)]) || Ls <- LsCmds],
	    undefined
    end.

format_product_number(ProductNumber) ->
    [case X of
	 $_ -> $/;
	 _ -> X
     end||X<-ProductNumber].


update_packages(ActiveKey, AllUPs, PackageFiles) ->
    UntreatedUPs = 

	lists:foldl(fun(PackageFile, UPs) ->
			    update_package(ActiveKey, UPs, PackageFile)
		    end, AllUPs, PackageFiles),
    [mnesia:delete({upgradePackage, Key})||Key<-UntreatedUPs],
    ok.

update_package(ActiveKey, RemainingUPs, PackageFile) ->
    case init_upgrade_package(ActiveKey, PackageFile) of
	undefined ->
	    RemainingUPs;
	Up ->
	    mnesia:write(Up),
	    lists:delete(Up#upgradePackage.upgradePackageId, RemainingUPs)
    end.

init_upgrade_package(ActiveKey, UpFile) ->
    case xmerl_scan:file(UpFile) of
	{ConfigurationE, []} ->

	    ProductE = find_element(product, ConfigurationE),
	    Name = find_attribute(name, ProductE),
	    ProdId = find_attribute(id, ProductE),
	    Version = find_attribute(version, ProductE),
	    DateE = find_element(date, ConfigurationE),
	    DescriptionE = try find_element(description, ConfigurationE)
			   catch _:_ -> ""
			   end,
	    TypeE = try find_element(type, ConfigurationE) 
		    catch _:_ -> ""
		    end,

	    ProductData = 
		#'ProductData'{productName = Name,
			       productNumber = format_product_number(ProdId),
			       productRevision = Version,
			       productionDate = find_text(DateE),
			       description = find_text(DescriptionE),
			       type = find_text(TypeE)},
	    UpKey = swmInventory:make_mom_key(ProdId, Version),
	    UpgradePackageId = {"1","1","1",UpKey},

	    IsUpgrade = swmI:is_upgrade_ongoing(),

	    UpDir = filename:dirname(UpFile),
	    %% ContentInfoE = find_element(contentinfo, ConfigurationE),
	    %% IsComplete = is_package_complete(UpDir, ContentInfoE),
	    IsComplete = case sysEnv:rcs_mode_2() of
			     vrcs -> true;
			     _ -> is_updir_complete(UpDir) %HV97842
			 end,
	    Obj = 
		case mnesia:read({upgradePackage, UpgradePackageId}) of
		    [UP] -> 
			{State, Created} = 
			    do_determine_package_state([UP], UpKey, ActiveKey,
						       IsUpgrade, IsComplete),
			UP#upgradePackage{state=State,
					  created=Created};
		    [] -> 
			{State, Created} = 
			    do_determine_package_state([], UpKey, ActiveKey,
						       IsUpgrade, IsComplete),
			#upgradePackage{upgradePackageId=UpgradePackageId,
					state=State,
					created=Created}
		end,

	    ActivationStep = 
		#'ActivationStep'{serialNumber=1,
				  name="Activate",
				  description="Activates this upgrade package"},

	    Obj2 = merge_with_dets(Obj),

	    Obj2#upgradePackage{
	      ignoreBreakPoints=true,
	      administrativeData = [ProductData],
	      activationStep = [ActivationStep],
	      creatorActionId = 0};
		
	ScanRes ->
	    Dir = filename:basename(filename:dirname(UpFile)),
	    swmLib:write_swm_log("SwM", error, "Corrupt UP file in "++Dir),
	    UpFiles =
		filelib:wildcard(filename:join([swmLib:archive_dir(),
						"*",
						"*-up.xml"])),
	    sysInitI:error_report([{xmerl_scan, file, [UpFile]},
				   {result, ScanRes},
				   {upFiles, UpFiles},
				   file:read_file_info(UpFile)]),
	    UpDir = filename:dirname(UpFile),
	    ArchiveDir = filename:dirname(UpDir),
	    LsCmds = ["ls -la " ++ ArchiveDir,
		      "ls -la " ++ UpDir],
	    [io:format("~s~n~s~n", [Ls, os:cmd(Ls)]) || Ls <- LsCmds],
	    RmCmd = "rm -rf " ++ UpDir,
	    info_msg("~s~n~s~n",[RmCmd, os:cmd(RmCmd)]),
	    undefined
    end.

merge_with_dets(#upgradePackage{upgradePackageId = UPId} = Obj) ->
    try
	[#upgradePackage{uri = Uri,
			 password = Pwd}] = dets:lookup(upgradePackage, UPId),
	case Uri of
	    undefined ->
		Obj;
	    _ ->
		Obj#upgradePackage{uri = Uri, password = Pwd}
	end
    catch
	_ : _ ->
	    Obj
    end.

do_determine_package_state([], ActiveKey, ActiveKey, _, _) ->
    %% No previous entry exists. 
    %% If the Up is the active, then it must already be confirmed
    info_msg("~p is in COMMIT_COMPLETED because no previous entry exists~n",
	     [ActiveKey]),
    {?UpgradePackageState_COMMIT_COMPLETED,
     comsaI:iso_time(os:timestamp(), extended)};
do_determine_package_state([], UpKey, _, _, IsComplete) ->
    %% No previous entry exists. 
    %% If the up is not the active, then it must be a new UP. 
    
    State = if IsComplete -> 
		    info_msg("~p is in PREPARE_COMPLETED because no previous "
			     "entry exists, it is not the current UP, but it "
			     "is complete~n",[UpKey]),
		    ?UpgradePackageState_PREPARE_COMPLETED;
	       true -> 
		    info_msg("~p is in INITIALIZED because no previous "
			     "entry exists, it is not the current UP, and it "
			     "is not complete~n",[UpKey]),
		    ?UpgradePackageState_INITIALIZED
	    end,

    {State, comsaI:iso_time(os:timestamp(), extended)};
do_determine_package_state([OldObj], UpKey, ActiveKey, IsUpgrade, IsComplete) ->
    %% A previous entry exist
    State = OldObj#upgradePackage.state,
    %% For certain states, which are in transient states
    %% they should be returned to a suitable stable state
    %% HU37957 Make sure the active UP always ends up in COMMIT_COMPLETED
    NewState = 
	case State of
	    ?UpgradePackageState_ACTIVATION_IN_PROGRESS ->
		case IsUpgrade of
		    true ->
			info_msg("~p is in ACTIVATION_IN_PROGRESS because "
				 "it was in the same state earlier and an "
				 "upgrade is ongoing~n",[UpKey]),
			?UpgradePackageState_ACTIVATION_IN_PROGRESS;
		    false ->
			info_msg("~p is in PREPARE_COMPLETED because "
				 "it was in ACTIVATION_IN_PROGRESS earlier "
				 "and an upgrade is not ongoing~n",[UpKey]),
			?UpgradePackageState_PREPARE_COMPLETED
		end;
	    _ when UpKey == ActiveKey->
		info_msg("~p is in COMMIT_COMPLETED because "
			 "it is the running UP~n",[UpKey]),
		?UpgradePackageState_COMMIT_COMPLETED;
	    ?UpgradePackageState_INITIALIZED when IsComplete ->
		info_msg("~p is in PREPARE_COMPLETED because "
			 "it was in INITIALIZED earlier "
			 "and the UP is complete~n",[UpKey]),
		?UpgradePackageState_PREPARE_COMPLETED;
	    ?UpgradePackageState_PREPARE_IN_PROGRESS ->
		info_msg("~p is in INITIALIZED because "
			 "it was in PREPARE_IN_PROGRESS earlier~n",[UpKey]),
		?UpgradePackageState_INITIALIZED;
	    ?UpgradePackageState_WAITING_FOR_COMMIT ->
		info_msg("~p is in PREPARE_COMPLETED because "
			 "it was in WAITING_FOR_COMMIT earlier "
			 "and it is not the running UP~n",[UpKey]),
		%% If a restart occurs in this state, 
		%% the system shall automatically revert to the previous version
		?UpgradePackageState_PREPARE_COMPLETED;
	    ?UpgradePackageState_COMMIT_COMPLETED ->
		info_msg("~p is in PREPARE_COMPLETED because "
			 "it was in COMMIT_COMPLETED earlier "
			 "and it is not the running UP~n",[UpKey]),
		%% If a package has been been running, return it to prepare
		%% so it can be activated again. The active UP has already
		%% been taken care of above. (This isn't exactly ECIM but
		%% it is practical.)
		?UpgradePackageState_PREPARE_COMPLETED;
	    ?UpgradePackageState_DEACTIVATION_IN_PROGRESS ->
		info_msg("~p is in PREPARE_COMPLETED because "
			 "it was in DEACTIVATION_IN_PROGRESS earlier and "
			 "it is not the running UP~n",[]),
		?UpgradePackageState_PREPARE_COMPLETED;
	    ?UpgradePackageState_PREPARE_COMPLETED when not IsComplete ->
		info_msg("~p is in INITIALIZEED because "
			 "it was in PREPARE_COMPLETE but is not complete~n",
			 [UpKey]),
		?UpgradePackageState_INITIALIZED;
	    _ -> %% All other states are kept as is
		info_msg("~p keeps it previous state~n",[UpKey]),
		State
	end,
    {NewState, OldObj#upgradePackage.created}.

%% is_package_complete(UpDir, ContentInfoE) ->
%%     do_is_package_complete(UpDir, 
%% 			   [{try find_attribute(filename, P)
%% 			     catch _:_ -> ""
%% 			     end,
%% 			     find_attribute(id, P)
%% 			    }||P<-ContentInfoE#xmlElement.content,
%% 			       P#xmlElement.name == product]).

%% The filename attribute is optional, and if it's not there we cannot
%% determine completeness
%% do_is_package_complete(UpDir, [{Name,Pid}|CXPs]) ->
%%     case filelib:is_file(filename:join(UpDir, Name)) of
%% 	true ->
%% 	    do_is_package_complete(UpDir, CXPs);
%% 	false ->
%% 	    %% Check for bundle
%% 	    %% This is technically not correct, as there is no design
%% 	    %% rule stating that name convention presumed here.
%% 	    %% However, since RCS is the only bundle in 15B it is ok for
%% 	    %% now.
%% 	    Bundle = filename:join(UpDir, string:to_lower(Pid)++".xml"),
%% 	    case filelib:is_file(Bundle) of
%% 		true ->
%% 		    do_is_package_complete(UpDir, CXPs);
%% 		false ->
%% 		    info_msg("Package ~p is not complete. Missing ~p~n",
%% 			     [filename:basename(UpDir), Name]),
%% 		    false
%% 	    end
%%     end;
%% do_is_package_complete(UpDir, []) ->
%%     info_msg("Package ~p is complete.~n",
%% 	     [filename:basename(UpDir)]),
%%     true.

%% HV97842 & HW14932 fix
is_updir_complete(UpDir) ->
    case
        swmBoardList:swp([products],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {boardTypesOTH, swmBoardList:boardTypes_oth()},
                          {options, [{global, UpDir}]}])
    of
        {ok, [{products, Selected}]} ->
            do_is_updir_complete(UpDir, Selected);
        {error, Reason} ->
            info_msg("Failed to get board list: ~p~n", [Reason]),
            false
    end.

do_is_updir_complete(UpDir, [{{_,Pid,_}, {global, Name}}|CXPs]) ->
    case filelib:is_file(filename:join(UpDir, Name)) of
	true ->
	    do_is_updir_complete(UpDir, CXPs);
	false ->
	    %% Check for bundle
	    %% This is technically not correct, as there is no design
	    %% rule stating that name convention presumed here.
	    %% However, since RCS is the only bundle in 15B it is ok for
	    %% now.
	    Bundle = filename:join(UpDir, string:to_lower(Pid)++".xml"),
	    case filelib:is_file(Bundle) of
		true ->
		    do_is_updir_complete(UpDir, CXPs);
		false ->
		    info_msg("Package ~p is not complete. Missing ~p~n",
			     [filename:basename(UpDir), Name]),
		    false
	    end
    end;
do_is_updir_complete(UpDir, [_|CXPs]) -> % HW23495
    do_is_updir_complete(UpDir, CXPs);
do_is_updir_complete(_, []) ->
    true.

%% Fix ends here

    


%% scratch_table(Table) ->
%%     swmLib:db_op(
%%       fun() -> 
%% 	      [mnesia:delete({Table, Key})||Key<-mnesia:all_keys(Table)]
%%       end),
%%     ok.


get_matching_up(Keys) ->
    Fun = fun() -> do_get_matching_up(Keys) end,
    swmLib:db_op_dirty(Fun).

do_get_matching_up(ProductData) ->    
    try 
	[begin
	     Id = PD#'ProductData'.productNumber,
	     Vsn = PD#'ProductData'.productRevision,
	     Key = {"1", "1", "1", swmInventory:make_mom_key(Id, Vsn)},
	     case mnesia:read({upgradePackage, Key}) of
		 [Obj] ->
		     Obj;
		 [] ->
		     throw(nomatch)
	     end
	 end||PD<-ProductData]
    catch throw:nomatch ->
	    nomatch
    end.

%%% ----------------------------------------------------------
%%% @doc Stores the MOM info about the package on the file system
%%% This information contains status which is not part of the metadata
%%% @end
%%% ----------------------------------------------------------

backup() ->
    %% It is assumed that the table has been opened by swmServer
    Objects = ets:tab2list(upgradePackage),
    try dets:insert(upgradePackage, Objects) of
	ok ->
	    ok;
	{error, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, backup},
	       {mfa, {dets, insert, [upgradePackage, Objects]}},
	       {error, Reason}])
    catch _:_ ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
						%some_method(Parameter)->
						%   nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
						%internal_function1(One, Two)->
						%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
        lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value;
find_text("") -> "".


%% info_msg(Format) ->
%%     info_msg(Format, []).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

						%error_msg(Format) ->
						%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
						%even_more_internal_function1(One, Two)->
						%   nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%% package_state_test() ->
%%     ActiveKey = "CXS12345-R2A",
%%     [begin
%% 	 io:format("----------------------------------------~n"
%% 		   "OldState = ~p SameKey = ~p IsUpgrade = ~p~n",
%% 		   [state(OldState), UpKey==ActiveKey, IsUpgrade]),
%% 	 UP = #upgradePackage{upgradePackageId={"1","1","1",UpKey},
%% 			      state=OldState},
%% 	 {State, Created} = 
%% 	     do_determine_package_state([UP], UpKey, ActiveKey, IsUpgrade),
%% 	 io:format("State = ~s Created= ~s~n",[state(State),Created])
%%      end||OldState<-lists:seq(1,8), UpKey<-["CXS12345-R1A","CXS12345-R2A"],
%% 	  IsUpgrade <-[true,false]],
%%     ok.

%% state(?UpgradePackageState_INITIALIZED) -> "INITIALIZED";
%% state(?UpgradePackageState_PREPARE_IN_PROGRESS) -> "PREPARE_IN_PROGRESS";
%% state(?UpgradePackageState_PREPARE_COMPLETED) -> "PREPARE_COMPLETED";
%% state(?UpgradePackageState_ACTIVATION_IN_PROGRESS) -> "ACTIVATION_IN_PROGRESS";
%% state(?UpgradePackageState_ACTIVATION_STEP_COMPLETED) -> "ACTIVATION_STEP_COMPLETED";
%% state(?UpgradePackageState_WAITING_FOR_COMMIT) -> "WAITING_FOR_COMMIT";
%% state(?UpgradePackageState_COMMIT_COMPLETED) -> "COMMIT_COMPLETED";
%% state(?UpgradePackageState_DEACTIVATION_IN_PROGRESS) -> "DEACTIVATION_IN_PROGRESS".



%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmLib.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/R5A/12
%%%
%%% @doc ==Library functions for software management==
%%% This module contains various library functions for software management
%%% 1. Initialization for internal services
%%% 2. Official swmI API function implementations
%%% 3. Directory environment functions to location various SWM resources
%%% 4. ECIM CommonLibrary AsyncActionProgress updating for SWM classes
%%% 5. SWM internal support functions

-module(swmLib).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/12').
-date('2016-03-18').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R1A/1      2012-02-02 etxjotj     Created
%%% R1A/3      2012-03-07 etxpeno     Add find_file/1
%%% R1A/5      2012-04-18 etxjotj     Rewrote find_file/1
%%% R1A/9      2012-07-05 etxjotj     Added swmVariables
%%% R1A/15     2012-07-15 etxjotj     Added the permanent data dir
%%% R2A/14     2014-02-18 etxberb     Added delete_upgrWindow_data/0,
%%%                                   activate_upgrWindow_tables/0,
%%%                                   copy_upgrWindow_table/1,
%%%                                   write_upgrWindow_table/2.
%%% R2A/17     2014-02-26 etxberb     Changed a few 'catch' to 'try - catch'.
%%% R2A/18     2014-02-26 etxberb     Robustness update of upgrWindow_tab2file.
%%% R2A/23     2014-03-24 erarafo     Robust registration of upgrade callbacks
%%% R2A/24     2014-03-24 erarafo     Cleanup
%%% R2A/26     2014-04-04 erarafo     HS45726, part of proposed solution
%%% R2A/27     2014-04-08 etxberb     Added write_upgrWindow_table/1.
%%% R2A/28     2014-04-15 erarafo     Refactored match of *-up.xml filename
%%% R2A/30     2014-05-13 erarafo     "." and ".." dropped from dir listings
%%% R2A/31     2014-05-14 erarafo     Added a -spec
%%% R2A/32     2014-06-05 etxjotj     EE split
%%% R2A/33     2014-06-12 etxjotj     EE split again
%%% R2A/34     2014-07-29 etxjotj     Bugfix
%%% R2A/35     2014-08-05 etxjotj     Don't give out more percent than 100
%%% R2A/36     2014-08-05 etxjotj     ESI data from SWM
%%% R2A/37     2014-10-07 etxarnu     updated generate_esi to get dev_patches
%%%                                   content even if symlinked
%%% R3A/1      2014-10-07 etxpejn     Changed error to warning in copy_old_table
%%% R3A/2      2014-10-24 etxjotj     Don't crash on copy_old_table errors
%%% R3A/3      2014-11-21 etxjotj     Better fault identification in copy_old_t
%%% R3A/4      2014-12-15 etxjotj     Copy old table schema check
%%% R3A/5      2014-12-19 etxjotj     Upgrade prep
%%% R3A/6      2015-01-20 etxjotj     Extended esi info
%%% R3A/7      2015-01-22 etxjotj     Added get_new_cxp_path/2
%%% R3A/8      2015-02-06 etxjotj     Moved get_free_disk/0 here
%%% R3A/9      2015-02-17 etxberb     Added an '{error, badfile}' clause in
%%%                                   upgrWindow_file2tab/1.
%%% R3A/10     2015-02-19 etxberb     Added get_file_info/1.
%%% R3A/11     2015-03-27 etxberb     TR HT59089: Added call to
%%%                                   alhI:swm_upgrWindow_active/0.
%%% R3A/12     2015-03-31 etxberb     Changed to warning in upgrWindow_file2tab.
%%% R3A/13     2015-04-14 etxberb     Added file_rename/2.
%% ----    ---------- -------  ------------------------------------------------
%% R4A/1   2015-04-16 etxpejn  Added create ESI dir in generate_esi/0
%% R4A/2   2015-04-30 etxberb  Changed error to warning in
%%                             upgrWindow_tab2file/1.
%% R3A/14  2015-04-28 etxjotj  Add backup to esi
%% R3A/15  2015-04-30 etxberb  Changed error to warning in
%%                             upgrWindow_tab2file/1.
%% R3A/16  2015-05-06 etxjotj  Dialyzer fault fix
%% R4A/4   2015-05-13 etxjotj  Handled shuffled attributes
%% R3A/18  2015-05-30 etxjotj  Upgrade_init backup in esi
%% R3A/19  2015-06-09 etxjotj  Make cxp list
%% R4A/8   2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%% R4A/9   2015-07-08 etxjotj  Added new upg functions
%% R4A/10  2015-07-09 etxjotj  HT91333 Remove stuff from disk when asked
%% R4A/11  2015-07-10 etxjotj  Dialyzer fix
%% R4A/13  2015-07-21 etxjotj  Clean disk
%% R4A/14  2015-08-21 etxpejn  Added write_swm_log
%% R4A/16  2015-09-03 etxjotj  Return current archive dir
%% R4A/18  2015-09-09 etxjotj  HU15807 Always use link solution for fallback
%% R4A/19  2015-09-17 etxjotj  Check for tables in old db
%% R4A/20  2015-09-21 etxjotj  Action id handling
%% R4A/21  2015-09-25 etxpejn  Moved rpc:call to logI:write_log
%% R4A/22  2015-10-01 etxtory  Use sysServer for checking 
%% R4A/23  2015-10-01 etxjotj  Support for compressed backups
%% R4A/25  2015-10-12 etxjotj  Mnesia on tmp fallback fix
%% R4A/28  2015-10-23 etxjotj  Backup for cluster
%% R4A/29  2015-10-30 etxjotj  Uninstall fallback
%% R4A/30  2015-11-03 etxjotj  Unlock and lock backups
%% R4A/31  2015-11-04 etxjotj  Spell check fix
%% R4A/32  2015-11-05 etxjotj  Updated backup locking mechanism
%% R4A/33  2015-11-23 etxjotj  HU37344 Remove EsiBackup MO after ESI
%% R5A/1   2016-02-26 etxberb  Added sda1_dir/0 & software_hal_dir/0.
%% R5A/2   2016-03-04 etomist  Added lock_action_capable, unlock_action_capable
%% R5A/3   2016-03-04 etomist  Changed lock_action_capable to return exact
%%                             reason in case of failure
%% R5A/5   2016-03-07 etomist  Updated lock_action_capable, unlock_action_capable
%%                             Added update_action_capable_info, get_action_capable_info
%% R5A/6   2016-03-08 etomist  Fixed dialyzer error, updated get_action_capable_info
%% R5A/7   2016-03-11 etomist     Added ActionCapability lock/unlock
%% R5A/8   2016-03-14 etxberb  Added hwSwCompatibilityIndex in
%%                             get_current_up_metadata/0.
%% R5A/9   2016-03-15 etxjotj  Identify CXP source
%% R5A/10  2016-03-16 etomist  Update to action capability lock/unlock functions
%% R5A/11  2016-03-18 etomist  Update printout in action capability lock/unlock functions
%%% R4A/34  2016-03-18 etxjotj  Action id handling fix
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% Initialization
-export([init/1]).
-export([delete_upgrWindow_data/0]).

%%% swmI external API functions
-export([get_cxp_path/2, get_new_cxp_path/2]).
-export([find_file/1]).
-export([register_upg_callback/1,
	 remove_upg_callback/1,
	 get_upg_callbacks/0,
	 clear_upg_callbacks/0]).

-export([get_current_up_metadata/0]).
-export([clean_files/0, clean_disk/1]).

-export([get_cxp_source/1]).

%%% Directory environment functions
-export([archive_dir/0, 
	 appdata_dir/0,
	 backup_dir/0, backup_dir/1,
	 data_dir/0,
	 esi_dir/0,
	 sda1_dir/0,
	 swm_dir/0,
	 software_dir/0,software_dir/1,software_dir_other/0,software_hal_dir/0,
	 squash_fs_dir/0,
	 sw_version_dir/0,
	 upgrade_prep_dir/0, upgrade_prep_dir/1
	]).

%%% Filename pattern matching
-export([is_up_abspath/1]).

%%% AsyncActionProgress and AsyncActionProgressWithSteps handling
-export([update/2, update_up/2]).

%%% Upgrade functions
-export([copy_old_table/1, all_objects/1, all_keys/1, read/2, first/1, next/2]).
-export([is_attributes_in_old_record/2, transform_obj/2]).
-export([is_old_table/1]).
-export([activate_upgrWindow_tables/0,
	 copy_upgrWindow_table/1,
	 write_upgrWindow_table/1, write_upgrWindow_table/2]).
-export([make_cxp_list/1]).

%%% SWM internal support
-export([get_variable/1, set_variable/2, erase_variable/1]).
-export([get_ram_variable/1, set_ram_variable/2, erase_ram_variable/1]).
-export([db_op/1, db_op_dirty/1]).
-export([parse_date/1, parse_date_simple/1]).
-export([upgrWindow_dir/0, print_ls/1]).
-export([write_swm_log/3]).
-export([sync/0]).
-export([os_cxp/0]).
-export([get_free_disk/0, get_free_disk/1]).
-export([init_action_id/1, init_action_id/2, get_new_action_id/1]).
-export([lock_action_capable/2, lock_action_capable/3,
         unlock_action_capable/1, unlock_action_capable/2,
         update_action_capable_info/2, update_action_capable_info/3,
         get_action_capable_info/0, get_action_capable_info/1,
         mo_lock_action_capable/2]).

%%% Backup handling
-export([mnesia_backup/1]).
-export([install_fallback/1, uninstall_fallback/0]).
-export([lock_backup/1, unlock_backup/1]).

%%% ESI
-export([generate_esi/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
-include("RcsSwM.hrl").
-include("SwmInternal.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

%%% ----------------------------------------------------------
-define(Tbl_upgrWindow,        olddb_upgrWindow).
-define(Tbl_upgrWindow_active, olddb_upgrWindow_active).

%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Adds a callback module for upgrade triggers
%%% @end
%%% ----------------------------------------------------------

register_upg_callback(Module) ->
    Fun =
	fun() ->
		case get_variable(upgCallback) of
		    undefined ->
			set_variable(upgCallback,
				     ordsets:from_list([Module]));
		    ModulesSet ->
			set_variable(upgCallback,
				     ordsets:add_element(Module, ModulesSet))
		end
	end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [Module])
    end.

%%% ----------------------------------------------------------
%%% @doc Removes a callback module for upgrade triggers
%%% @end
%%% ----------------------------------------------------------

remove_upg_callback(Module) ->
    Fun = fun() ->
		  case get_variable(upgCallback) of
		      undefined ->
			  ok;
		      ModulesSet ->
			  set_variable(upgCallback,
				       ordsets:del_element(Module, ModulesSet))
		  end
	  end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [Module])
    end.

%%% ----------------------------------------------------------
%%% @doc Returns the list of callback modules for upgrade triggers
%%% @end
%%% ----------------------------------------------------------

-spec get_upg_callbacks() -> [module()].

get_upg_callbacks() ->
    case get_variable(upgCallback) of
	undefined ->
	    [];
	ModulesSet ->
	    ordsets:to_list(ModulesSet)
    end.

%%% ----------------------------------------------------------
%%% @doc Clears the list of callback modules for upgrade triggers
%%% @end
%%% ----------------------------------------------------------

-spec clear_upg_callbacks() -> ok.

clear_upg_callbacks() ->
    set_variable(upgCallback, ordsets:new()),
    ok.

%%% ----------------------------------------------------------
%%% @doc Returns metadata about the currently running upgrade package
%%% This function works before the MOM has been updated so it can be used
%%% in early start phases
%%% @end
%%% ----------------------------------------------------------


get_current_up_metadata() ->

    CxsPattern = filename:join(swmLib:software_dir(), "*-up.xml"),
    CxsFile =
	case filelib:wildcard(CxsPattern) of
	    [] ->
		CxsPatternB = filename:join(swmLib:software_dir(), "cxs*.xml"),
		%% We assume only one such file for now
		hd(filelib:wildcard(CxsPatternB));
	    [Path] -> Path
	end,

    {ConfigurationE, []} = xmerl_scan:file(CxsFile),

    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    DateE = find_element(date, ConfigurationE),
    DescriptionE = find_element(description, ConfigurationE),
    TypeE = find_element(type, ConfigurationE),
    HwSwCompIx = swmBoardList:hwSwCompatibilityIndex(ConfigurationE),

    Date = case parse_date(find_text(DateE)) of
	       {local, DT} -> DT;
	       {absolute, DT} ->
		   calendar:universal_time_to_local_time(DT)
	   end,

    [{productName, Name},
     {productNumber, ProdId},
     {productRevision, Version},
     {productionDate, Date},
     {description, find_text(DescriptionE)},
     {type, find_text(TypeE)},
     {hwSwCompatibilityIndex, HwSwCompIx}].

%%% ----------------------------------------------------------
%%% @doc Creates SWM internal database tables
%%% @end
%%% ----------------------------------------------------------
init(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(swmRamVariables,
				 [{type, set},
				  {ram_copies, DbNodes},
				  {attributes, record_info(fields,
							   swmRamVariables)} |
				  swmDataInit:add_clh_option(swmRamVariables)]),
    {atomic, ok} =
	clhI:mnesia_create_table(swmVariables,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   swmVariables)} |
				  swmDataInit:add_clh_option(swmVariables)]),
 
   case swmI:is_upgrade_ongoing() of
	true ->
	    ok = swmI:copy_old_table(swmVariables);
	false ->
	    ok
    end,
    ok.

%%% ----------------------------------------------------------
%%% @doc Implementation of the official swmI API (see swmI for detailed info)
%%% @end
%%% ----------------------------------------------------------

get_cxp_path(CxpProdId, CxpProdVsn) ->
    do_get_cxp_path(CxpProdId, CxpProdVsn, software_dir()).

get_new_cxp_path(CxpProdId, CxpProdVsn) ->
    do_get_cxp_path(CxpProdId, CxpProdVsn, software_dir_other()).

do_get_cxp_path(CxpProdId, CxpProdVsn, SoftwareDir) ->
    CxpDirName = "*"++CxpProdId++"_"++CxpProdVsn,
    Pattern = filename:join(SoftwareDir, CxpDirName),
    case filelib:wildcard(Pattern) of
	[] -> {error, not_found};
	[Path] -> {ok, Path};
	Dirs -> {error, {many_dirs_found, Dirs}}
    end.

%%% ----------------------------------------------------------
%%% @doc Implementation of the official swmI API (see swmI for detailed info)
%%% @end
%%% ----------------------------------------------------------

find_file(File) ->
    Base = filename:basename(File),
    Patch = filename:join(sysEnv:dev_patches_dir(), Base),
    case filelib:is_regular(Patch) of
	true ->
	    Patch;
	false ->
	    File
    end.

%%% ----------------------------------------------------------
%%% @doc Upgrade package storage area $RCS_ROOT/rcs/swm/archive
%%% @end
%%% ----------------------------------------------------------

archive_dir() ->
    filename:join(swm_dir(), "archive").

%%% ----------------------------------------------------------
%%% @doc Temporary appdata storage area $RCS_ROOT/rcs/swm/appdata
%%% @end
%%% ----------------------------------------------------------

appdata_dir() ->
    filename:join(swm_dir(), "appdata").

%%% ----------------------------------------------------------
%%% @doc Backup storage area $RCS_ROOT/rcs/swm/backup
%%% @end
%%% ----------------------------------------------------------

backup_dir() ->
    filename:join(swm_dir(), "backup").

%%% ----------------------------------------------------------
%%% @doc Individual backup storage area $RCS_ROOT/rcs/swm/backup/[Index]
%%% @end
%%% ----------------------------------------------------------

-spec backup_dir(Index::string()) -> string().

backup_dir(Index) ->
    filename:join(backup_dir(), Index).

%%% ----------------------------------------------------------
%%% @doc Individual backup storage area $RCS_ROOT/rcs/swm/backup/[Index]
%%% @end
%%% ----------------------------------------------------------

-spec data_dir() -> string().

data_dir() ->
    filename:join(swm_dir(), "data").

%%% ----------------------------------------------------------
%%% @doc Storage area for esi information
%%% @end
%%% ----------------------------------------------------------

esi_dir() ->
    filename:join(swm_dir(), "esi").
%%% ----------------------------------------------------------
%%% @doc Active software storage area $RCS_ROOT/home/$USER/software
%%% @end
%%% ----------------------------------------------------------

software_dir() ->
    software_dir(sysEnv:home_dir()).

software_dir(HomeDir) ->
    filename:join(HomeDir, "software").

%%% ----------------------------------------------------------
%%% @doc Alternate software storage area 
%%% In simulated $RCS_ROOT/{home,home2}/$USER/software
%%% In classic /disk/homepartition{1,2}/$USER/software
%%% In splitee /rcs/swm/home{1,2}/$USER/software

software_dir_other() ->
    HomeOther = swmOs:home_dir_other(),		    
    software_dir(HomeOther).

%%% ###########################################################################
%%% software_hal_dir
%%%
%%% ###=====================================================================###
software_hal_dir() ->
    filename:join(sda1_dir(), "halswp").

%%% ###########################################################################
%%% sda1_dir
%%%
%%% ###=====================================================================###
sda1_dir() ->
    filename:join(swm_dir(), "sda1").

%%% ----------------------------------------------------------
%%% @doc Mountpoint for squash fs images

squash_fs_dir() ->
    filename:join(sysEnv:rcs_root(), "software").


%%% ----------------------------------------------------------
%%% @doc NOT USED!
%%% @end
%%% ----------------------------------------------------------

sw_version_dir() ->
    filename:join(sysEnv:home_dir(), "swm").

%%% ----------------------------------------------------------
%%% @doc SWM permanent storage area $RCS_ROOT/rcs/swm
%%% Storage areas on home and home2 do not survive upgrades
%%% ----------------------------------------------------------

swm_dir() ->
    filename:join(sysEnv:rcs_dir(), "swm").

%%% ----------------------------------------------------------
%%% @doc Storage area for pre-upgrade produced files
%%% ----------------------------------------------------------

upgrade_prep_dir() ->
    upgrade_prep_dir(sysEnv:home_dir()).

upgrade_prep_dir(HomeDir) ->
    filename:join(HomeDir, "upgradePrep").
    

%%% ----------------------------------------------------------
upgrWindow_dir() ->
    filename:join(swm_dir(), "upgrWindow").

%%% ----------------------------------------------------------
%%% @doc Writes in the swmLog on the core MP.
%%% @end
%%% ----------------------------------------------------------
write_swm_log(User, Severity, Msg) ->
    logI:write_log("SwmLog", User, Severity, Msg).

%%% ----------------------------------------------------------
%%% @doc Returns true if the given string matches "*-up.xml".
%%% @end
%%% ----------------------------------------------------------

is_up_abspath(AbsPath) ->
    case re:run(AbsPath, "-up\\.xml$", []) of
	nomatch ->
	    false;
	_ ->
	    true
    end.

%%% ----------------------------------------------------------
%%% @doc AsyncActionProgress update support function
%%% By using this function to update the 'AsyncActionProgress' you
%%% get an easier syntax for the individual specific updates that
%%% must be done.
%%%
%%% Progress information should be given as additionalInfo. The latest
%%% update of additionalInfo is copied to progressInfo
%%%
%%% Using the additionalInfoClear "field" the additionalInfo sequence
%%% is cleared. This is not a real field, but updates the additionalInfo
%%%
%%% Each user must have its own functions for writing the record to the
%%% proper mnesia object.
%%%
%%% Side effect: If the given keylist contains additionalInfo or
%%% additionalInfoClear an INFO message is written to the Erlang log.
%%% @end
%%% ----------------------------------------------------------

-spec update([{atom(), integer()|string()}], #'AsyncActionProgress'{}) ->
	  #'AsyncActionProgress'{}.

update([{actionId, ActionId}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{actionId = ActionId});
update([{actionName, Name}|Data], Progress) ->
    update(Data, Progress#'AsyncActionProgress'{actionName = Name});
update([{additionalInfo, Info}|Data], Progress) ->
    info_msg("additionalInfo: ~s~n",[Info]),
    NewAI = case Progress#'AsyncActionProgress'.additionalInfo of
		AI when is_list(AI) -> AI++[Info];
		_ -> [Info]
	    end,
    update(Data, Progress#'AsyncActionProgress'{additionalInfo = NewAI,
					      progressInfo = Info});
update([{additionalInfoClear, Info}|Data], Progress) ->
    info_msg("additionalInfo: ~s~n",[Info]),
    update(Data, Progress#'AsyncActionProgress'{additionalInfo = [Info],
					      progressInfo = Info});
update([{progressInfo, Info}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{progressInfo = Info});
update([{progressPercentage, Percent}|Data], P) when Percent =< 100 ->
    update(Data, P#'AsyncActionProgress'{progressPercentage = Percent});
update([{result, Result}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{result = Result});
update([{resultInfo, Info}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{resultInfo = Info});
update([{state, State}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{state = State});
update([{timeActionStarted, Date}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{timeActionStarted = Date});
update([{timeActionCompleted, Date}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{timeActionCompleted = Date});
update([X|Data], P) ->
    error_msg("Unknown progress header: ~p~n",[X]),
    update(Data, P);
update([], P) ->
    Time = comsaI:iso_time(os:timestamp(), extended),
    P#'AsyncActionProgress'{timeOfLastStatusUpdate = Time}.

-spec update_up([{atom(), integer()|string()}], #'AsyncActionProgressWithSteps'{}) ->
	  #'AsyncActionProgressWithSteps'{}.

update_up([{actionId, ActionId}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{actionId = ActionId});
update_up([{actionName, Name}|Data], Progress) ->
    update_up(Data, Progress#'AsyncActionProgressWithSteps'{actionName = Name});
update_up([{additionalInfo, Info}|Data], Progress) ->
    info_msg("additionalInfo: ~s~n",[Info]),
    NewAI = case Progress#'AsyncActionProgressWithSteps'.additionalInfo of
		AI when is_list(AI) -> AI++[Info];
		_ -> [Info]
	    end,
    update_up(Data, Progress#'AsyncActionProgressWithSteps'{additionalInfo = NewAI,
					      progressInfo = Info});
update_up([{additionalInfoClear, Info}|Data], Progress) ->
    info_msg("additionalInfo: ~s~n",[Info]),
    update_up(Data, Progress#'AsyncActionProgressWithSteps'{additionalInfo = [Info],
					      progressInfo = Info});
update_up([{progressInfo, Info}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{progressInfo = Info});
update_up([{progressPercentage, Percent}|Data], P) when Percent =< 100 ->
    update_up(Data,P#'AsyncActionProgressWithSteps'{
		     progressPercentage = Percent,
		     stepProgressPercentage = Percent});
update_up([{result, Result}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{result = Result});
update_up([{resultInfo, Info}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{resultInfo = Info});
update_up([{state, State}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{state = State});
update_up([{step, Step}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{step = Step});
update_up([{timeActionStarted, Date}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{timeActionStarted = Date});
update_up([{timeActionCompleted, Date}|Data], P) ->
    update_up(Data, P#'AsyncActionProgressWithSteps'{timeActionCompleted = Date});
update_up([X|Data], P) ->
    error_msg("Unknown progress header: ~p~n",[X]),
    update_up(Data, P);
update_up([], P) ->
    Time = comsaI:iso_time(os:timestamp(), extended),
    P#'AsyncActionProgressWithSteps'{timeOfLastStatusUpdate = Time}.

%%% ----------------------------------------------------------
%%% @doc SWM variable storage access.
%%% Returns 'undefined' if the variable is not set


get_variable(Key) ->
    Fun = fun() -> mnesia:read({swmVariables, Key}) end,
    case db_op_dirty(Fun) of
	[Obj] ->
	    Obj#swmVariables.value;
	[] ->
	    undefined
    end.

%%% ----------------------------------------------------------
%%% @doc SWM variable storage update

set_variable(Key, Value) ->
    Fun = fun() -> mnesia:write(#swmVariables{key=Key, value=Value}) end,
    db_op_dirty(Fun).


%%% ----------------------------------------------------------
%%% @doc SWM variable storage update

erase_variable(Key) ->
    Fun = fun() -> mnesia:delete({swmVariables, Key}) end,
    db_op_dirty(Fun).

%%% ----------------------------------------------------------
%%% @doc SWM ram variable storage access.
%%% Returns 'undefined' if the variable is not set


get_ram_variable(Key) ->
    Fun = fun() -> mnesia:read({swmRamVariables, Key}) end,
    case db_op_dirty(Fun) of
	[Obj] ->
	    Obj#swmRamVariables.value;
	[] ->
	    undefined
    end.

%%% ----------------------------------------------------------
%%% @doc SWM ram variable storage update

set_ram_variable(Key, Value) ->
    Fun = fun() -> mnesia:write(#swmRamVariables{key=Key, value=Value}) end,
    db_op_dirty(Fun).


%%% ----------------------------------------------------------
%%% @doc SWM ram variable storage update

erase_ram_variable(Key) ->
    Fun = fun() -> mnesia:delete({swmRamVariables, Key}) end,
    db_op_dirty(Fun).

%%% ----------------------------------------------------------
%%% @doc Ensures that the Fun() is executed within the scope of a transaction

db_op(Fun) ->
    case mnesia:is_transaction() of
	true ->
	    Fun();
	false ->
	    mnesia:transaction(Fun)
    end.

%%% ----------------------------------------------------------
%%% @doc Encapsulates database Funs in a dirty operation

db_op_dirty(Fun) ->
    case mnesia:is_transaction() of
	true ->
	    Fun();
	false ->
	    mnesia:async_dirty(Fun)
    end.


%%% ----------------------------------------------------------
%%% -type copy_old_table(Tab) ->                            %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: copy_old_table(Tab)
%%% ----------------------------------------------------------

copy_old_table(Tab) ->
    try
	begin
	    [OldSchema] = read(schema, Tab),
	    OldAttrs = proplists:get_value(attributes, element(3,OldSchema)),
	    [NewSchema] = ets:lookup(schema, Tab),
	    case proplists:get_value(attributes, element(3, NewSchema)) of
		OldAttrs -> 
		    [{Tab, Ets}] = ets:lookup(olddb, Tab),
		    [mnesia:dirty_write(Obj) || Obj <- ets:tab2list(Ets)],
		    ok;
		NewAttrs when length(NewAttrs) /= length(OldAttrs) ->
		    throw({schema_inconsistency, Tab, OldAttrs, NewAttrs});
		NewAttrs ->
		    case is_same_attributes(OldAttrs, NewAttrs) of
			true ->
			    Sort = make_sort_fun(OldAttrs, NewAttrs),
			    [{Tab, Ets}] = ets:lookup(olddb, Tab),
			    [mnesia:dirty_write(Sort(Obj)) || 
				Obj <- ets:tab2list(Ets)],
			    ok;
			false ->
			    throw({schema_inconsistency, Tab, 
				   OldAttrs, NewAttrs})
		    end
			
	    end
	end
    catch
	throw:{schema_inconsistency, Tab, Old, New} ->
	    sysInitI:error_report([{schema_inconsistency, Tab},
				       {old, Old},
				       {new, New},
				       erlang:get_stacktrace()]),
	    erlang:error(schema_inconsistency,[Tab]);
	error:{badmatch,[]} ->
	    sysInitI:warning_report([{mfa, {?MODULE,copy_old_table,[Tab]}},
					 {error, no_such_table},
					 erlang:get_stacktrace()]),
	    {error, no_such_table};
	ErrClass : ErrReason ->
	    sysInitI:warning_report([{mfa, {?MODULE,copy_old_table,[Tab]}},
					 {ErrClass, ErrReason},
					 erlang:get_stacktrace()]),
	    {error, ErrReason}
		
    end.

is_same_attributes(Old, New) ->
    case {lists:sort(Old), lists:sort(New)} of
	{Same, Same} ->
	    true;
	_ ->
	    false
    end.

%% Resort the fields of a record to a different order

make_sort_fun(OldAttrs, NewAttrs) ->
    fun(Obj) ->
	    [Type|Values] = tuple_to_list(Obj),
	    KeyedValues = lists:zip(OldAttrs, Values),
	    NewValues = [proplists:get_value(NewAttr, KeyedValues)||
			    NewAttr<-NewAttrs],
	    list_to_tuple([Type|NewValues])
    end.
	    

%%% ----------------------------------------------------------
%%% Called by swmServer just before taking an mnesia backup during upgrade.
%%% This activates the upgrade window.
%%%
%%% The upgrade window is a period during upgrade between SWM taking a snapshot
%%% of the mnesia database and reboot. During this window, applications can
%%% write data that needs to come along with the backed up data to be restored
%%% after reboot.
%%% ----------------------------------------------------------
activate_upgrWindow_tables() ->
    os:cmd("mkdir " ++ upgrWindow_dir()),
    swmServer:ets_new(?Tbl_upgrWindow_active, [public, named_table]),
    try
	alhI:swm_upgrWindow_active()
    catch
	ErrClass : ErrReason ->
	    sysInitI:warning_report([{?MODULE, activate_upgrWindow_tables},
					 {ErrClass, ErrReason} |
					 erlang:get_stacktrace()])
    end.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
copy_upgrWindow_table(Tab) ->
    upgrWindow_files2tabs(),
    try ets:lookup(?Tbl_upgrWindow, Tab) of
	[] ->
	    %% No data on file for the requested table.
	    ok;
	[{Tab, UpgrWinTabName}] ->
	    try ets:tab2list(UpgrWinTabName) of
		List when is_list(List) ->
		    Result = [(catch mnesia:dirty_write(Obj)) || Obj <- List],
		    case lists:keymember('EXIT', 1, Result) of
			false ->
			    ok;
			true ->
			    ErrInfo =
				[{?MODULE, copy_upgrWindow_table, [Tab]},
				 {result, Result},
				 {tableObjs, List},
				 {stacktrace, erlang:get_stacktrace()}],
			    sysInitI:error_report(ErrInfo),
			    {error, Result}
		    end
	    catch
		ErrClass : ErrReason ->
		    Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
		    ErrInfo =
			[{?MODULE, copy_upgrWindow_table, [Tab]},
			 "Application table not found. Internal SWM mismatch?",
			 Err],
		    sysInitI:warning_report(ErrInfo),
		    ok
	    end
    catch
	error : badarg ->
	    %% No data on file.
	    ok
    end.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
copy_to_upgrWindow_table(Tab, UpgrWinTabName) ->
    [ets:insert(UpgrWinTabName, Obj) || Obj <- ets:tab2list(Tab)].

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
write_upgrWindow_table(Tab) ->
    case ets:info(?Tbl_upgrWindow_active) of
	undefined ->
	    ok;
	_ ->
	    case create_upgrWindow_table(Tab) of
		{Action, UpgrWinTabName} ->
		    copy_to_upgrWindow_table(Tab, UpgrWinTabName),
		    upgrWindow_tab2file(UpgrWinTabName),
		    print_ls(Action, upgrWindow_dir(), atom_to_list(Tab));
		undefined ->
		    timer:apply_after(100,
				      ?MODULE,
				      write_upgrWindow_table,
				      [Tab])
	    end
    end.

write_upgrWindow_table(Tab, Obj) ->
    case ets:info(?Tbl_upgrWindow_active) of
	undefined ->
	    ok;
	_ ->
	    case create_upgrWindow_table(Tab) of
		{Action, UpgrWinTabName} ->
		    ets:insert(UpgrWinTabName, Obj),
		    upgrWindow_tab2file(UpgrWinTabName),
		    print_ls(Action, upgrWindow_dir(), atom_to_list(Tab));
		undefined ->
		    timer:apply_after(100,
				      ?MODULE,
				      write_upgrWindow_table,
				      [Tab, Obj])
	    end
    end.

create_upgrWindow_table(Tab) ->
    try ets:lookup(?Tbl_upgrWindow, Tab) of
	[] ->
	    UpgrWinTabName = get_upgrWindow_tab_name(Tab),
	    ets:insert(?Tbl_upgrWindow, {Tab, UpgrWinTabName}),
	    OrigMainOpts = [{keypos, ets:info(Tab, keypos)},
			    ets:info(Tab, type)],
	    swmServer:ets_new(UpgrWinTabName,
			      [public, named_table | OrigMainOpts]),
	    upgrWindow_tab2file(?Tbl_upgrWindow),
	    {create, UpgrWinTabName};
	[{Tab, UpgrWinTabName}] ->
	    {update, UpgrWinTabName}
    catch
	error : badarg ->
	    case swmServer:ets_new(?Tbl_upgrWindow, [public, named_table]) of
		ok ->
		    create_upgrWindow_table(Tab);
		{error, _} ->
		    undefined
	    end
    end.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
print_ls(Dir) ->
    print_ls(Dir, "").

print_ls(Dir, File) ->
    print_ls(create, Dir, File).

print_ls(create, Dir, File) ->
    Cmd = "ls -la ",
    Ls = os:cmd(Cmd ++ Dir),
    info_msg(print_ls_file(File) ++ Cmd ++ Dir ++ "~n" ++ Ls);
print_ls(_, _, _) ->
    ok.

print_ls_file("") ->
    "";
print_ls_file(File) ->
    "File:  " ++ File ++ "~n".

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
delete_upgrWindow_data() ->
    os:cmd("rm " ++ upgrWindow_dir() ++ "/*"),
    try ets:tab2list(?Tbl_upgrWindow) of
	List when is_list(List) ->
	    catch ets:delete(?Tbl_upgrWindow),
	    [(catch ets:delete(UpgrWinTabName)) || {_, UpgrWinTabName} <- List]
    catch
	_ : _ ->
	    ok
    end,
    ok.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
get_upgrWindow_tab_name(Tab) ->
    list_to_atom(atom_to_list(?Tbl_upgrWindow) ++ "_" ++ atom_to_list(Tab)).

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
upgrWindow_files2tabs() ->
    MyTabResult = upgrWindow_file2tab(?Tbl_upgrWindow),
    TblsResult =
	try ets:tab2list(?Tbl_upgrWindow) of
	    Tbls when is_list(Tbls) ->
		[{?Tbl_upgrWindow, MyTabResult} |
		 [{UpgrWinTabName, upgrWindow_file2tab(UpgrWinTabName)} ||
		     {_, UpgrWinTabName}
			 <- Tbls]]
	catch
	    _ : _ ->
		%% No data on file.
		[{?Tbl_upgrWindow, MyTabResult}]
	end,
    sysInitI:info_report([{?MODULE, upgrWindow_files2tabs} | TblsResult]),
    ok.

upgrWindow_file2tab(Tab) ->
    try ets:file2tab(upgrWindow_dir() ++ "/" ++ atom_to_list(Tab)) of
	{ok, Tab} ->
	    ok;
	{error, {read_error, {file_error, _File, enoent}}} ->
	    sysInitI:warning_report([{?MODULE, upgrWindow_file2tab},
					 {file_not_found, Tab}]),
	    print_ls(upgrWindow_dir()),
	    no_file;
	{error, cannot_create_table} ->
	    table_already_created;
	{error, badfile} ->
	    TabTmp = atom_to_list(Tab) ++ ".tmp",
	    %% The file became corrupt during system shutdown probably because
	    %% it was not closed properly before system went down. Maybe it is
	    %% possible to fix in OTP (ets), but no time to test a patch with
	    %% extra disk sync. Also, a workaround is implemented in block ALH
	    %% for TR HT50597 that solves the consequences of this fault.
	    %% Finally, the affected case in APPM / ALH is anyway a temporary
	    %% solution that needs to be replaced by another way of keeping an
	    %% accurate time stamp to be used for System Down Time logging.
	    %% For all these reasons, this case is changed to a WARNING :-)
	    sysInitI:warning_report([{?MODULE, upgrWindow_file2tab},
					 {badfile, Tab} |
					 get_file_info(Tab)]),
	    print_ls(upgrWindow_dir(), TabTmp),
	    upgrWindow_file2tab(list_to_atom(TabTmp));
	Error ->
	    print_ls(upgrWindow_dir()),
	    Error
    catch
	ErrClass : ErrReason ->
	    ErrInfo = [{?MODULE, upgrWindow_file2tab, Tab},
		       {ErrClass, [ErrReason | erlang:get_stacktrace()]}],
	    sysInitI:error_report(ErrInfo),
	    {ErrClass, ErrReason}
    end.

%%% ----------------------------------------------------------
get_file_info(FileName) when is_atom(FileName) ->
    File = upgrWindow_dir() ++ "/" ++ atom_to_list(FileName),
    FileInfo = file:read_file_info(File),
    Content =
	case file:read_file(File) of
	    {ok, FileBin} ->
		{content, binary_to_list(FileBin)};
	    FileError ->
		{content, FileError}
	end,
    [FileInfo, Content].

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
upgrWindow_tab2file(Tab) ->
    %% Since 'ets:tab2file' deletes the file and then writes it again, there is
    %% a time gap where the file can disappear if the process is killed between
    %% delete and write. (This has been proven on target). In order to prevent
    %% this problem, the write operation is divided in two steps by first
    %% writing to a temporary file and then renaming it. Renaming is atomic.
    File = upgrWindow_dir() ++ "/" ++ atom_to_list(Tab),
    TmpFile = File ++ ".tmp",
    try
	begin
	    ok = ets:tab2file(Tab, TmpFile),
	    ok = file_rename(TmpFile, File)
	end
    catch
	ErrClass : ErrReason ->
	    ErrInfo = [{?MODULE, upgrWindow_tab2file, Tab},
		       {ErrClass, [ErrReason | erlang:get_stacktrace()]}],
	    sysInitI:warning_report(ErrInfo),
	    ErrReason
    end.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
%% 'ets:tab2file' is not always completely done on disk when it returns. This
%% fault appears during system shutdown. Let's try to wait for maximum one
%% second for the disk to finish the tab2file operation and see if it helps...
file_rename(Source, Destination) ->
    file_rename(Source, Destination, 1).

file_rename(Source, Destination, Cnt) when Cnt =< 20 ->
    case file:read_file_info(Source) of
	{ok, #file_info{size = Size}} when is_integer(Size) andalso Size > 0 ->
	    file_rename_warning(Cnt, Source, Destination),
	    file:rename(Source, Destination);
	_ ->
	    timer:sleep(50),
	    file_rename(Source, Destination, Cnt + 1)
    end;
file_rename(Source, Destination, _) ->
    Reason =
	{fileRename_timeout, {{source_file, Source},
			      {source_file_info, file:read_file_info(Source)},
			      {destination_file, Destination}}},
    throw(Reason).

file_rename_warning(1, _, _) ->
    ok;
file_rename_warning(Cnt, Source, Destination) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 'late disk sync of ets:tab2file on disk',
				 {number_of_checks, Cnt},
				 {source_file, Source},
				 {source_file_info,file:read_file_info(Source)},
				 {destination_file, Destination}]).

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

all_objects(Tab) ->
    [{Tab, Ets}] = ets:lookup(olddb, Tab),
    ets:tab2list(Ets).


%%% ----------------------------------------------------------
%%% -type is_old_db(Tab)                                  %#
%%% Input: Tab::atom()
%%% Output: boolean()
%%% Exceptions:
%%% Description: 
%%% ----------------------------------------------------------

is_old_table(Tab) ->
    case ets:lookup(olddb, Tab) of
	[{Tab, _}] ->
	    true;
	[] ->
	    false
    end.


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

all_keys(Tab) ->
    [{Tab, Ets}] = ets:lookup(olddb, Tab),
    all_keys(ets:first(Ets), Ets).

all_keys('$end_of_table', _) ->
    [];
all_keys(Key, Ets) ->
    [Key|all_keys(ets:next(Ets, Key))].

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

read(Tab, Key) ->
    [{Tab, Ets}] = ets:lookup(olddb, Tab),
    ets:lookup(Ets, Key).

%%% ----------------------------------------------------------
%%% -type first(Tab)->                                       %#
%%%
%%% Input: Tab:atom()
%%% Output: '$end_of_table' | term()
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

first(Tab) ->
    [{Tab, Ets}] = ets:lookup(olddb, Tab),
    ets:first(Ets).


%%% ----------------------------------------------------------
%%% -type next(Tab, Key)->                                  %#
%%%
%%% Input: Tab:atom(), Key:term()
%%% Output: '$end_of_table' | term()
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

next(Tab, Key) ->
    [{Tab, Ets}] = ets:lookup(olddb, Tab),
    ets:next(Ets, Key).

%%% ----------------------------------------------------------
%%% @doc Check if the given attributes are in the attribute list
%%% @end
%%% ----------------------------------------------------------

is_attributes_in_old_record(Table, Attributes) ->
    [OldSchema] = read(schema, Table),
    OldAttrs = proplists:get_value(attributes, element(3, OldSchema)),
    lists:foldl(fun(Attr, Accu) ->
			lists:member(Attr, OldAttrs) and Accu
		end, true, Attributes).

%%% ----------------------------------------------------------
%%% @doc Transform an object
%%% @end
%%% ----------------------------------------------------------

transform_obj(Record, Added) ->
    [Tab|Values] = tuple_to_list(Record),
    [OldSchema] = read(schema, Tab),
    OldAttrs = proplists:get_value(attributes, element(3,OldSchema)),
    [NewSchema] = ets:lookup(schema, Tab),
    NewAttrs = proplists:get_value(attributes, element(3, NewSchema)),
    Zipped = lists:zip(OldAttrs, Values),
    NewValues = 
	[begin
	     Default = proplists:get_value(NewAttr, Added, undefined),
	     proplists:get_value(NewAttr, Zipped, Default)
	 end||NewAttr<-NewAttrs],
    list_to_tuple([Tab|NewValues]).
    

%%% ----------------------------------------------------------
%%% @doc Parses the date and time part of a datetime string

parse_date_simple(DateString)   ->
    {ok, [Year, Month, Day, Hour, Minute, Second], _} =
	io_lib:fread("~d-~d-~dT~d:~d:~d", DateString),

    {{Year, Month, Day}, {Hour, Minute, Second}}.

%%% ----------------------------------------------------------
%%% @doc Returns true if we run split architecture

os_cxp() ->
    get_variable(os_cxp).

%%% ----------------------------------------------------------
%%% @doc Returns the free disk on /rcs as byte multiple of kb
%%% @end

%%% Return free disk in bytes
get_free_disk() ->
    FreeKb = sysServer:get_free_disk(),
    FreeKb * 1024.

%%% Original implementation: left for debugging
get_free_disk(orig) ->
    DfRes = os:cmd(["df -kP ", sysEnv:rcs_dir()]),
    Values = hd(lists:reverse(string:tokens(DfRes, "\n"))),
    %% [Device, Blocks, Used, Available, UsePercent, Mounted]
    [_, _, _, Available, _, _] = string:tokens(Values, "\t "),
    list_to_integer(Available)*1024.


%%% ----------------------------------------------------------
%%% @doc Parses a date time string with compensation for timezone if given


parse_date(DateString) ->
    {ok, [Year, Month, Day, Hour, Minute, Second], Continuation} =
	io_lib:fread("~d-~d-~dT~d:~d:~d", DateString),
    NextC =
	case Continuation of
	    "."++Rest -> % Fractions are currently ignored
		{ok, [_], Cont2} =
		    io_lib:fread("~d", Rest),
		Cont2;
	    _ ->
		Continuation
	end,
    case NextC of
	"" ->
	    {local, {{Year, Month, Day}, {Hour, Minute, Second}}};
	"Z" ->
	    {absolute, {{Year, Month, Day}, {Hour, Minute, Second}}};
	_ ->
	    {ok, [Sign, DH, DM], ""} =
		io_lib:fread("~c~d:~d",NextC),
	    Offset = DH*3600+DM,
	    DT  = {{Year, Month, Day}, {Hour, Minute, Second}},
	    Secs = calendar:datetime_to_gregorian_seconds(DT),
	    UT = calendar:gregorian_seconds_to_datetime(
		   case Sign of
		       [$+] -> Secs-Offset;
		       [$-] -> Secs+Offset
		   end),
	    {absolute, UT}
    end.


%%% ----------------------------------------------------------
%%% doc Unmounts file systems that are no longer needed once
%%% the UP corresponding to the given archive directory has been
%%% removed.
%%% 
%%% Since a CXP may be shared between several UPs it is necessary
%%% to analyze the total collection of UPs present: CXPs that are
%%% shared by another UP must not be unmounted.
%%% end
%%% ----------------------------------------------------------

%% spec unmount_unused_filesystems(string()) -> any().

%% unmount_unused_filesystems(ArchiveDir) ->
%%     AllArchiveDirs = filelib:wildcard(filename:join(archive_dir(), "*")),    
%%     OtherArchiveDirs = AllArchiveDirs--[ArchiveDir],
%%     UpNivs = get_cxp_nivs(ArchiveDir),
%%     OtherNivs = union_cxp_nivs(OtherArchiveDirs),
%%     CandidateNivs =
%% 	lists:append(
%% 	  [case ordsets:is_element(Niv, OtherNivs) of
%% 	       false ->
%% 		   [Niv];
%% 	       true ->
%% 		   []
%% 	   end
%% 	   || Niv <- UpNivs]),
%%     [begin
%% 	 MountPoint = filename:join(squash_fs_dir(), N++"_"++I++"_"++V),
%% 	 case filelib:is_dir(MountPoint) of
%% 	     false ->
%% 		 ok;
%% 	     true ->
%% 		 info_msg("attempt to unmount: ~s~n", [MountPoint]),
%% 		 RcsMode = sysEnv:rcs_mode(),
%% 		 swmOs:unmount(RcsMode, MountPoint)
%% 	 end
%%      end
%%      || {N,I,V} <- CandidateNivs].

%%% ----------------------------------------------------------
%%% doc Wraps the ssh_sftp:list_dir/3 function, dropping the
%%% "." and ".." directory names from a filename listing.
%%% end
%%% ----------------------------------------------------------

%% -spec sftp_list_dir(pid(), string(), timeout()) ->
%% 	  {ok, [string()]} | {error, any()}.

%% sftp_list_dir(ChannelPid, Path, TimeoutMillis) ->
%%     case ssh_sftp:list_dir(ChannelPid, Path, TimeoutMillis) of
%% 	{error, _}=Error ->
%% 	    Error;
%% 	{ok, Listing} ->
%% 	    {ok, Listing--[".", ".."]}
%%     end.    


%%% ----------------------------------------------------------
%%% doc Returns the list of {CxpName, CxpId, CxpVersion} triplets
%%% for the UP present in the given directory (the directory is
%%% typically /rcs/swm/archive/CxsName_CxsId_CxsVersion).
%%% end
%%% ----------------------------------------------------------

%% spec get_cxp_nivs(string()) ->  [{string(), string(), string()}].

%% get_cxp_nivs(ArchiveDir) ->
%%     % get all basenames in the given directory: expect one
%%     % UP XML file and any number of .cxp files
%%     {ok, UpFiles} = case file:list_dir(ArchiveDir) of
%% 			{error, Reason} ->
%% 			    erlang:error(Reason, [ArchiveDir]);
%% 			ListDir -> 
%% 			    ListDir
%% 		    end,
    
%%     % expect exactly one *-up.xml file
%%     [UpMetadataFileBasename] =
%% 	[UpFile || UpFile <- UpFiles, is_up_abspath(UpFile)],
    
%%     case xmerl_scan:file(filename:join(ArchiveDir, UpMetadataFileBasename)) of
%% 	{ConfigurationE, []} ->
%% 	    ContentInfoE = find_element(contentinfo, ConfigurationE),
%% 	    ProductEs = find_elements(product, ContentInfoE),
%% 	    [{find_attribute(name, ProductE),
%% 	      find_attribute(id, ProductE),
%% 	      find_attribute(version, ProductE)}
%% 	     ||ProductE <- ProductEs];
%% 	{error, enoent} ->
%% 	    []
%%     end.


%%% ----------------------------------------------------------
%%% doc Returns the union of {CxpName, CxpId, CxpVersion} triplets
%%% for the given archive directories.
%%% end
%%% ----------------------------------------------------------

%% spec union_cxp_nivs([string()]) ->  [{string(), string(), string()}].

%% union_cxp_nivs(ArchiveDirs) ->
%%     Unite =
%% 	fun (ArchiveDir, Acc) ->
%% 		 ordsets:union(Acc, ordsets:from_list(get_cxp_nivs(ArchiveDir)))
%% 	end,
%%     lists:foldl(Unite, ordsets:new(), ArchiveDirs).

%%% ----------------------------------------------------------
%%% @doc Generate Ericsson support information
%%% @end
%%% ----------------------------------------------------------
generate_esi() ->
    case file:make_dir(swmLib:esi_dir()) of
	ok ->
	    ok;
	{error,eexist} ->
	    ok;
	{error, Reason} ->
	    erlang:error(Reason, [swmLib:esi_dir()])
    end,
    {0, _} = swmOs:cmdres(["rm -rf ", esi_dir(), "/*"]),
    {0, _} = swmOs:cmdres(["ls -l ", sysEnv:dev_patches_dir(), "/ > ", 
			   esi_dir(), "/dev_patches_content.txt"]),
    {0, _} = swmOs:cmdres(["ls -l ", squash_fs_dir(), " > ", 
			   esi_dir(), "/lvlist.txt"]),
    {0, _} = swmOs:cmdres(["ls -l ", archive_dir(), " > ", 
			   esi_dir(), "/archive.txt"]),

    MnesiaPath = filename:join(swmLib:swm_dir(), "upgrade_init*"),
    swmOs:cmdres(["cp ",MnesiaPath, " ", esi_dir()]),

    ModVsnFile = filename:join(esi_dir(), "module_versions.txt"),
    {ok, MVFd} = file:open(ModVsnFile, [write]),
    [begin
	 Vsn = proplists:get_value(vsn, Mod:module_info(attributes), unknown),
	 io:format(MVFd, "~w: ~p~n",[Mod, Vsn])
     end||Mod<-lists:sort(erlang:loaded())],
    file:close(MVFd),

    InvList = filename:join(esi_dir(), "swInventory.txt"),
    {ok, InvFd} = file:open(InvList, [write]),
    swmInventory:print_inventory(InvFd),
    file:close(InvFd),

    ExportTime = os:timestamp(),
    Name = list_to_binary("EsiBackup."++comsaI:iso_time(ExportTime, basic)),
    MgrKey = {"1","1","1","1"},
    Type = system,
    Progress = undefined,

    MoRef = swmBackup:create_backup_common(Name, MgrKey, Type, Progress),
    Index = lists:last(string:tokens(MoRef, "=,")),

    %% Store backup in ESI dir
    BuDir = backup_dir(Index),
    EsiBuDir = filename:join(esi_dir(), "backup"),
    file:rename(BuDir, EsiBuDir),
    TmpBuPath = swmBackup:compress_file(EsiBuDir, ExportTime),
    file:rename(TmpBuPath, filename:join(esi_dir(), "backup.zip")),

    os:cmd("rm -rf "++EsiBuDir),
    %% HU37344 Remove MO
    BuKey = {"1","1","1","1",Index},
    {atomic, ok} = 
	mnesia:transaction(fun() -> mnesia:delete({brmBackup, BuKey}) end),
    unlock_backup(Index),

    file:copy(filename:join(sysEnv:home_dir(), ".cxp_list"),
	      filename:join(esi_dir(), "cxp_list")),

    ok.

%%% ----------------------------------------------------------
%%% @doc Make cxp list for mounting and store it in the given home directory
%%% This presumes that the $HOME/software directory has been populated first
%%% @end
%%% ----------------------------------------------------------

make_cxp_list(Dir) ->
    Path = filename:join(Dir, ".cxp_list"),
    {ok, Files} = file:list_dir(filename:join(Dir, "software")),
    {ok, Fd} = file:open(Path, [write]),
    [io:format(Fd,"~s~n",[Cxp])||Cxp<-lists:sort(Files),
				 filename:extension(Cxp) /= ".xml"],
    file:close(Fd),
    Md5Path = filename:join(Dir, filename:basename(Path)++".md5sum"),
    os:cmd(["cd ", filename:dirname(Path), " ; "
	    "md5sum ", filename:basename(Path), " > ", Md5Path]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Clean files
%%% Remove all unnecessary stuff from the disk
%%% @end
%%% ----------------------------------------------------------

clean_files() ->
    MnesiaPath = filename:join(swmLib:swm_dir(), "upgrade_init*"),
    os:cmd(["rm -f ", MnesiaPath]),
    os:cmd(["rm -rf ", esi_dir(), "/*"]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Clean disk
%%% Remove all unnecessary stuff from the disk
%%% @end
%%% ----------------------------------------------------------

clean_disk(_) ->
    clean_files().

%%% ----------------------------------------------------------
%%% @doc Make a mnesia backup adopted for cluster
%%% @end
%%% ----------------------------------------------------------

%%% mnesia:backup/1 uses a max configuration which means it tries to
%%% include data form all mnesia nodes. This is not necessary, we can do
%%% with data stored on the main core MP only

mnesia_backup(Path) ->
    Tables = mnesia:table_info(schema, tables),
    CpArgs = [{ram_overrides_dump, false}, {min, Tables}],
    case mnesia:activate_checkpoint(CpArgs) of
	{ok, Name, _Nodes} ->
	    Res = mnesia:backup_checkpoint(Name, Path),
	    mnesia_checkpoint:deactivate(Name),
	    Res;
	{error, Reason} ->
	    {error, Reason}
    end.

%%% ----------------------------------------------------------
%%% @doc Create a link to a mnesia backup file
%%% The regular mnesia commands copies the file, and that may fill up the
%%% home volume
%%% @end
%%% ----------------------------------------------------------

install_fallback(BuPath) ->
    RestoreDir = filename:join(sysEnv:home_dir(), "restore"),
    info_msg("Making symlink ~p -> ~p~n",
	     [RestoreDir, filename:dirname(BuPath)]),
    ok = file:make_symlink(filename:dirname(BuPath), RestoreDir),
    file:delete(filename:join(sysEnv:home_dir(), "install_complete")),
    sync().

%%% ----------------------------------------------------------
%%% @doc Remove the link to a mnesia backup file
%%% @end
%%% ----------------------------------------------------------

uninstall_fallback() ->
    RestoreDir = filename:join(sysEnv:home_dir(), "restore"),
    os:cmd(["rm -rf ", RestoreDir]),
    sync().

%%% ----------------------------------------------------------
%%% @doc Preserve exclusivity for managing the backup area
%%% @end
%%% ----------------------------------------------------------

lock_backup(Index) ->
    LockId = {{backup, Index},self()},
    info_msg("Obtaining lock: ~p~n",[LockId]),
    case global:set_lock(LockId, [node()], 10) of
	true ->
	    ok;
	false ->
	    erlang:error(set_lock_failed, [Index])
    end.

unlock_backup(Index) ->
    LockId = {{backup, Index},self()},
    info_msg("Releasing lock: ~p~n",[LockId]),
    global:del_lock(LockId, [node()]).




%%% ----------------------------------------------------------
%%% @doc Sync disk cache to permanent storage
%%% Syncs the disk cache and waits for the cache to be committed to disk
%%% @end
%%% ----------------------------------------------------------
sync() ->
    %% This is an important file change. Make sure it gets to the disk
    os:cmd("sync"),
    timer:sleep(5000).


%%% ----------------------------------------------------------
%%% @doc Set counter value for progress report action id
%%% The counter value is set to a random value but not zero (0)
%%% to avoid the confusion, that happens if the counter starts
%%% at zero after restart
%%% @end
%%% ----------------------------------------------------------
-spec init_action_id(Key::term()) -> ok.

init_action_id(Key) ->
    {_, X, _} = os:timestamp(),
    Value = (X rem 65534)+1,
    set_ram_variable({actionId, Key}, Value).

-spec init_action_id(Key::term(), Excluded::[integer()]) -> ok.

init_action_id(Key, []) ->
    init_action_id(Key);
init_action_id(Key, Excluded) ->
    Value = (lists:max(Excluded) rem 65534)+1,
    set_ram_variable({actionId, Key}, Value).

%%% ----------------------------------------------------------
%%% @doc Update and return counter value for progress report action id
%%% The return should be a 16-bit unsigned integer. Never return 0 (zero).
%%% @end
%%% ----------------------------------------------------------

-spec get_new_action_id(Key::term()) -> 0..65535.

get_new_action_id(Key) ->
    AKey = {actionId, Key},
    case mnesia:dirty_update_counter(swmRamVariables, AKey, 1) rem 65536 of
	0 ->
	    get_new_action_id(Key);
	NewActionId ->
	    NewActionId
    end.

-spec lock_action_capable(LockId::atom(), Text::string()) ->
    ok | {nok, atom()} | {aborted, string()}.
lock_action_capable(LockId, Text) ->
    lock_action_capable(LockId, {"1", "1", "1"}, Text).

-spec lock_action_capable(LockId::atom(), Key::term(), Text::string()) ->
    ok | {nok, atom()} | {aborted, string()}.
lock_action_capable(LockId, Key, Text) ->
    Fun = fun() ->
        Lock = get_variable(action_capable_lock),
        case Lock of 
            undefined ->
                set_variable(action_capable_lock, LockId),
                [Value | _] = mnesia:read(swM, Key),
                NewValue = Value#swM{
                            actionCapable = ?ActionCapabilityState_WAIT,
                            actionCapableInfo = Text},
                mnesia:write(swM, NewValue, write);
            LockId ->
                {nok, LockId};
            _ ->
                {nok, Lock}
        end
    end,
    case db_op(Fun) of 
        {atomic, ok} ->
            sysInitI:info_msg("swmLib: lock_action_capable succeeded: ~p~n",
                              [LockId]),
            ok;
        {atomic, {nok, LockId}} ->
            sysInitI:info_msg("swmLib: lock_action_capable with LockId "
                              "~p unsuccessful, already locked to the same LockId~n",
                              [LockId]),
            {nok, LockId};
        {atomic, {nok, Value}} ->
            sysInitI:info_msg("swmLib: lock_action_capable with LockId "
                              "~p unsuccessful, already locked to ~p~n",
                              [LockId, Value]),
            {nok, Value};
        {aborted, Reason} ->
            sysInitI:info_msg("swmLib: lock_action_capable with description "
                              "~p failed, transaction aborted with reason ~p~n",
                              [Text, Reason]),
            {aborted, Reason}
    end.

-spec unlock_action_capable
    (LockId::atom()) -> ok | {nok, atom()} | {aborted, string()};
    (LockIdList::list()) -> ok | {nok, atom()} | {aborted, string()}.
unlock_action_capable(LockId) ->
    unlock_action_capable(LockId, {"1", "1", "1"}).

-spec unlock_action_capable
    (LockId::atom(), Key::term()) -> ok | {nok, list()} | {aborted, string()};
    (LockIdList::list(), Key::term()) ->
        ok | {nok, list()} | {aborted, string()}.
unlock_action_capable(LockId, Key) when is_atom(LockId) ->
    unlock_action_capable([LockId], Key);
unlock_action_capable(LockIdList, Key) when is_list(LockIdList) ->
    Fun = fun() ->
        Lock = get_variable(action_capable_lock),
        case Lock of
            undefined ->
                {nok, LockIdList};
            _ ->
                case lists:member(Lock, LockIdList) of
                    true ->
                        erase_variable(action_capable_lock),
                        [Value | _] = mnesia:read(swM, Key),
                        NewValue = Value#swM{
                                        actionCapable =
                                                ?ActionCapabilityState_CAPABLE,
                                        actionCapableInfo = undefined},
                        mnesia:write(swM, NewValue, write);
                    false ->
                        {nok, [Lock]}
                end
        end
    end,
    case db_op(Fun) of 
        {atomic, ok} ->
            sysInitI:info_msg("swmLib: unlock_action_capable succeeded: ~p~n",
                              [LockIdList]),
            ok;
        {atomic, {nok, LockIdList}} ->
            sysInitI:info_msg("swmLib: unlock_action_capable unsuccessful for ~p, "
                              "currently in CAPABLE state~n", [LockIdList]),
            {nok, LockIdList};
        {atomic, {nok, Value}} ->
            sysInitI:info_msg("swmLib: unlock_action_capable unsuccessful, "
                              "unlock requested by ~p, "
                              "currently locked to ~p~n", [LockIdList, Value]),
            {nok, Value};
        {aborted, Reason} ->
            sysInitI:info_msg("swmLib: unlock_action_capable failed, "
                              "transaction aborted with reason ~p~n", [Reason]),
            {aborted, Reason}
    end.

-spec update_action_capable_info(LockId::atom(), Text::string()) ->
    ok | {nok, atom()} | {aborted, string()}.
update_action_capable_info(LockId, Text) ->
    update_action_capable_info(LockId, {"1", "1", "1"}, Text).

-spec update_action_capable_info
    (LockId::atom(), Key::term(), Text::string()) ->
        ok | {nok, atom()} | {aborted, string()};
    (LockIdList::list(), Key::term(), Text::string()) ->
        ok | {nok, atom()} | {aborted, string()}.
update_action_capable_info(LockId, Key, Text) when is_atom(LockId) ->
    update_action_capable_info([LockId], Key, Text);
update_action_capable_info(LockIdList, Key, Text) when is_list(LockIdList)->
    Fun = fun() ->
        Lock = get_variable(action_capable_lock),
        case Lock of
            undefined ->
                {nok, LockIdList};
            _ ->
                case lists:member(Lock, LockIdList) of
                    true ->
                        [Value | _] = mnesia:read(swM, Key),
                        NewValue = Value#swM{actionCapableInfo = Text},
                        mnesia:write(swM, NewValue, write);
                    false ->
                        {nok, [Lock]}
                end
        end
    end,
    case db_op(Fun) of 
        {atomic, ok} ->
            sysInitI:info_msg("swmLib: update_action_capable_info succeeded: ~p~n",
                              [LockIdList]),
            ok;
        {atomic, {nok, LockIdList}} ->
            sysInitI:info_msg("swmLib: update_action_capable_info unsuccessful, "
                              "currently in CAPABLE state~n"),
            {nok, LockIdList};
        {atomic, {nok, Value}} ->
            sysInitI:info_msg("swmLib: update_action_capable_info unsuccessful, "
                              "update requested by ~p, "
                              "currently locked to ~p~n", [LockIdList, Value]),
            {nok, Value};
        {aborted, Reason} ->
            sysInitI:info_msg("swmLib: update_action_capable_info failed, "
                              "transaction aborted with reason ~p~n", [Reason]),
            {aborted, Reason}
    end.

-spec get_action_capable_info() -> string().
get_action_capable_info() ->
    get_action_capable_info({"1", "1", "1"}).
-spec get_action_capable_info(Key::term()) -> string().
get_action_capable_info(Key) ->
    [Val | _] = mnesia:dirty_read(swM, Key),
    Val#swM.actionCapableInfo.

-spec mo_lock_action_capable(LockId::atom(), LockString::string()) -> ok.
mo_lock_action_capable(LockId, LockString) ->
    case lock_action_capable(LockId,
                             LockString) of
        ok ->
            ok;
        {nok, Lock} ->
            Str = io_lib:format("Lock for ~p unsuccessful, reason ~p",
                                [Lock, swmLib:get_action_capable_info()]),
            throw({lock_fail, Str});
        {aborted, Reason} ->
            throw({lock_fail, Reason})
    end.


%%% ----------------------------------------------------------
%%% @doc Return the CXP info of the CXP that is the source of a file
%%% Input is a path in /home/sirpa/software
%%% @end
%%% ----------------------------------------------------------

get_cxp_source(Path) ->
    CxpRoot = get_cxp_root(Path),
    XmlPath = filelib:wildcard(filename:join(CxpRoot, "cxp*.xml")),
    {ConfigurationE, []} = xmerl_scan:file(XmlPath),
    ProductE = find_element(product, ConfigurationE),
    {find_attribute(name, ProductE),
     find_attribute(id, ProductE),
     find_attribute(version, ProductE)}.

get_cxp_root("/") ->
    throw(path_not_in_software);
get_cxp_root(Path) ->
    ParentDir = filename:dirname(Path),
    case filename:basename(ParentDir) of
	"software" ->
	    Path;
	_ ->
	    get_cxp_root(ParentDir)
    end.

    
    
    

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

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


%% find_elements(ElementName, #xmlElement{content=ContentList}) ->
%%     find_elements(ElementName, ContentList);

%% find_elements(ElementName, ContentList) ->
%%     [E|| #xmlElement{name=Name}=E <- ContentList, Name =:= ElementName].


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

%%% ----------------------------------------------------------
%%% #           find_text(Element)
%%% Input: Element:#xmlElement{}
%%% Output:
%%% Exceptions:
%%% Description: Returns the text content of an element which contains text
%%% ----------------------------------------------------------

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value.

info_msg(Format) ->
   info_msg(Format, []).
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%warning_msg(Format) ->
%    warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

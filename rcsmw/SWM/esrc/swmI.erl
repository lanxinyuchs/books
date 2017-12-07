%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmI.erl %
%%% Author:	etxjotj
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2
%%%
%%% @doc ==Software management interface==
%%% This module contains the erlang internal interface for software 
%%% management. Other parts of the interface like callback modules for
%%% COMTE is not included here.

-module(swmI).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-28').
-author('etxjotj').

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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-02-14 etxjotj     Created
%%% R1A/2      2012-03-07 etxpeno     Add find_file/1
%%% -----      ---------  --------    ------------------------
%%% R2A/5      2013-09-16 erarafo     Edoc only.
%%% R2A/7      2014-02-18 etxberb     Added copy_upgrWindow_table/1 and
%%%                                   write_upgrWindow_table/2.
%%% R2A/8      2014-02-18 etxarnu     Added revert/1
%%% R2A/12     2014-04-08 etxberb     Added write_upgrWindow_table/1
%%% R2A/13     2014-05-16 erarafo     Edoc elaborated
%%% R2A/15     2014-05-22 erarafo     HS62272: Edoc adjusted
%%% R2A/16     2014-08-04 etxjotj     Manage disk space
%%% R2A/17     2014-08-05 etxjotj     ESI data from SWM
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2014-10-24 etxjotj     Changed semantics on copy_old_table
%%% R3A/2      2015-01-20 etxjotj     Added is_failsafe_restart/0
%%% R3A/3      2015-01-22 etxjotj     Added get_new_cxp_path/2
%%% R3A/4      2015-03-20 etxderb     Added reset_boot/0, used by sysNetloader
%%% R3A/5      2015-03-26 etxberb     Added is_restore_backup_active/0.
%%% R3A/6      2015-05-08 etxderb     Added reset_boot/1
%%% -----      ---------  --------    ------------------------
%%% R4A/1      2015-07-08 etxjotj     Added is_attributes_in_old_record/2 and
%%%                                   transform_obj/2
%%% R4A/2      2015-07-09 etxjotj     HT91333 Added clean_files/0
%%% R4A/4      2015-07-21 etxjotj     Clean disk
%%% R4A/5      2015-09-03 etxjotj     Return current archive dir
%%% R4A/8      2015-09-10 erarafo     Dialyzer warnings fixed
%%% R4A/9      2015-09-17 etxjotj     Check for old table
%%% R4A/10     2015-09-24 etxjotj     Exemption handling for mnesia monitoring
%%% R4A/11     2015-10-02 etxjotj     Force auto backup
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2016-03-14 etxjotj     Identify the CXP source of any file
%%% R5A/3      2016-04-11 etxjotj     Added function to calculate cxp dir      
%%% -----      ---------  --------    ------------------------
%%% R6A/1      2016-06-03 etxpejn     Removed dns and arp functions    
%%% R6A/2   2016-09-14 etxberb  Added get_bootfallback_up_metadata/0.
%%% -----   ---------  -------- ------------------------
%%% R7A/1   2016-10-13 etxjotj  Interface for AIC
%%% -----   ---------  -------- ------------------------
%%% R8A/1   2016-10-25 etxberb  Added node_type/0.
%%% R8A/2   2016-11-16 etxberb  Added moGet_SwM/0.
%%% R8A/3   2016-12-16 etxberb  Added add_board/3.
%%% R8A/4   2016-12-29 etxberb  MR37329 HW Sensitive Install implemented.
%%% -----   ---------  -------- ------------------------
%%% R9A/1   2017-02-20 etxjotj  Check $NODE_TYPE
%%% R9A/2   2017-02-28 etxberb  Added node_types_valid/0, is_node_type_valid/1.
%%% R9A/3   2017-03-06 etxberb  Added node_types_deprecated- physical- &
%%%                             virtual/0.
%%% R9A/4   2017-03-13 etxberb  Moved "5GRadioNode" from node_types_virtual/0,
%%%                             to node_types_physical/1.
%%% R9A/6   2017-03-21 etxberb  Added hsi_state check in add_board/3.
%%% R9A/7   2017-08-14 etxberb  Added is_hsi_active/0, is_hsi_enabled/0.
%%% -----   ---------  -------- ------------------------
%%% R10A/1  2017-05-09 etxtory  Added get_autobackup_path for AIC
%%% R10A/2  2017-08-14 etxberb  Merge from R9A/7.
%%% -----   ---------  -------- ------------------------
%%% R11A/1  2017-09-04 etxjotj  Added restore_backup
%%% R11A/2  2017-09-04 etxjotj  Added remount_tmpfs_size_cmd
%%% R11A/3  2017-09-04 etxjotj  Added quarantine_reset
%%% R11A/6  2017-09-29 etxpejn  Added hw_log
%%% R11A/7  2017-10-11 etxberb  HW32389: Added validate_current_installed_LMCs,
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/1  2017-11-28 etxjotj  Callback functions for create backup
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% For all blocks
-export([register_appdata_receiver/2]).
-export([is_upgrade_ongoing/0,
	 is_swRestore_ongoing/0]).   % Only valid in 5G (vRCS)
-export([is_failsafe_restart/0]).
-export([copy_old_table/1, all_objects/1, all_keys/1, read/2, 
	 first/1, next/2, is_attributes_in_old_record/2, transform_obj/2,
	 is_old_table/1]).
-export([register_upg_callback/1, remove_upg_callback/1]).
-export([find_file/1]).
-export([get_current_up_metadata/0, get_bootfallback_up_metadata/0]).
-export([register_exemption/2, force_auto_backup/0]).
-export([get_cxp_source/1]).
-export([get_appdata_files/0, get_appdata_files/1, get_appdata_files/2]).
-export([get_cxp_root/1]).
-export([node_type/0]).
-export([is_node_type_valid/1]).
-export([node_types_deprecated/0]).
-export([node_types_physical/0]).
-export([node_types_valid/0]).
-export([node_types_virtual/0]).
-export([is_hsi_active/0, is_hsi_enabled/0]).
-export([register_bu_callback/2, remove_bu_callback/2]).
%% AIC interface
-export([make_ai_backup/0, clear_ai_backup/0]).
-export([get_autobackup_path/0]).
-export([restore_backup/1]).

%% APPM interface
-export([clear_application_logs/0, clear_application_tmp/0]).
-export([add_board/3, add_board_blocked/3]).
-export([remount_tmpfs_size_cmd/2]).

%% COMSA interface
-export([quarantine_reset/0, format_next_scheduled_time/1]).
-export([hw_log/1]).

%% LOG interface
-export([generate_esi/0]).

%% SYS interface
-export([clean_files/0]).
-export([clean_disk/1]).
-export([initial_auto_backup/0]).
-export([init_config_prepare/0]).

%% For test SUITEs
-export([moGet_SwM/0]).

%% internal interface
-export([activation_complete/0]).

%% Not categorized. Move to other heading asap
-export([get_cxp_path/2, get_new_cxp_path/1, get_new_cxp_path/2]).
-export([is_restore_backup_active/0]).
-export([copy_upgrWindow_table/1,
	 write_upgrWindow_table/1, write_upgrWindow_table/2]).
-export([revert/1]).
-export([reset_boot/0, reset_boot/1]).
-export([get_current_archive_dir/0]).

-include("RcsSwM.hrl").

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	sysInitI:error_report(?RepInfo(__ReportInfo))).
-define(LOG_INFO(__ReportInfo),
	sysInitI:info_report(?RepInfo(__ReportInfo))).
-define(LOG_WARN(__ReportInfo),
	sysInitI:warning_report(?RepInfo(__ReportInfo))).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc This function registers an appdata receiver.
%%%      At first install and in subsequent upgrades, SWM will call the
%%%      registered modules appdata/1 function with an #xmlElement{} as
%%%      defined in xmerl/include/xmerl.hrl containing the contents of a
%%%      registration data file. The receiver is responsible for specifying
%%%      the internal xml format, as well as decoding the contents.
%%%      Non parsable content will not be forwarded, but an error report will
%%%      be printed.
%%% @end
%%% ----------------------------------------------------------

-spec register_appdata_receiver(Tag::string(), Module::atom()) -> ok.

register_appdata_receiver(Tag, Module) ->
    swmAppData:register_appdata_receiver(Tag, Module).

%%%-----------------------------------------------------------------------------
%%% @doc Returns the path to the directory which holds LMC content
%%% @end
%%%-----------------------------------------------------------------------------

-spec get_cxp_path(CxpProdId::string(), CxpProdVsn::string()) -> {ok, string()} | {error, not_found | {many_dirs_found, [string()]}}.

get_cxp_path(CxpProdId, CxpProdVsn) ->
    swmLib:get_cxp_path(CxpProdId, CxpProdVsn).

%%%-----------------------------------------------------------------------------
%%% @doc Returns the path to the directory which holds new LMC content
%%% Valid during upgrade only
%%% @end
%%%-----------------------------------------------------------------------------

-spec get_new_cxp_path({LmName::string(), LmId::string()}) -> string().

get_new_cxp_path({_, LmId}) ->
    swmServer:get_new_cxp_path(LmId).


-spec get_new_cxp_path(CxpProdId::string(), CxpProdVsn::string()) ->
			      {ok, string()} |  {error, not_found | {many_dirs_found, [string()]}}.

get_new_cxp_path(CxpProdId, CxpProdVsn) ->
    swmLib:get_new_cxp_path(CxpProdId, CxpProdVsn).

%%%-----------------------------------------------------------------------------
%%% @doc Returns the full filepath to File, by searching first in dev_patches
%%%      directory. If the file is found in the dev_patches directory, the path
%%%      to that file is returned, otherwise the original file path is returned.
%%% @end
%%%-----------------------------------------------------------------------------

-spec find_file(File::file:name()) -> file:name().

find_file(File) ->
    swmLib:find_file(File).

%%%-----------------------------------------------------------------------------
%%% @doc Tells if upgrade is ongoing. 
%%% Returns 'true' after the operator calls "activate", and 'false" again
%%% after the operator calls "confirm"
%%% @end
%%%-----------------------------------------------------------------------------

-spec is_upgrade_ongoing() -> boolean().

is_upgrade_ongoing() ->
    swmServer:is_upgrade_ongoing().

%%%-----------------------------------------------------------------------------
%%% @doc Tells if software restore is ongoing. 
%%% Returns 'true' after the new VNF instance is started and false again
%%% after the whole "confirm" sequence is finished.
%%% NOTE! This function is only valid in 5G (vRCS).
%%% @end
%%%-----------------------------------------------------------------------------

-spec is_swRestore_ongoing() -> boolean().

is_swRestore_ongoing() ->
    swmvBuRestore:is_ongoing().

%%%-----------------------------------------------------------------------------
%%% @doc Tells if restore backup is activated. 
%%% @end
%%%-----------------------------------------------------------------------------
-spec is_restore_backup_active() -> boolean().

is_restore_backup_active() ->
    case swmLib:get_ram_variable(restore_backup_active) of
	undefined ->
	    false;
	Value ->
	    Value
    end.

%%% ----------------------------------------------------------
%%% @doc Register a module that implements upgrade callback
%%% functions. A module may implement one or more of the
%%% functions verify_precondition/0, verify_upgrade/0,
%%% activate_start/0, preload/0, activate/0 and confirm/0.
%%% 
%%% The verify_precondition/0 function must have the return type
%%% ok | {ok, string()}.
%%%
%%% The other callback functions must have the return type
%%% ok | {ok, string()} | {error, string()}.
%%% @end
%%% ----------------------------------------------------------

-spec register_upg_callback(Module::atom()) -> ok.

register_upg_callback(Module) ->
    swmLib:register_upg_callback(Module).

-spec remove_upg_callback(Module::atom()) -> ok.

remove_upg_callback(Module) ->
    swmLib:remove_upg_callback(Module).

%%%-----------------------------------------------------------
%%% @doc Copy the contents of the old table to the new db
%%% @end
%%% ----------------------------------------------------------

-spec copy_old_table(Tab::atom()) -> ok | {error, Reason::term()}.

copy_old_table(Tab) ->
    swmLib:copy_old_table(Tab).

%%%-----------------------------------------------------------
%%% @doc Copy the contents of the old upgrade phase table to the new db.
%%% @end
%%% ----------------------------------------------------------

-spec copy_upgrWindow_table(Tab::atom()) -> ok.

copy_upgrWindow_table(Tab) ->
    swmLib:copy_upgrWindow_table(Tab).

%%%-----------------------------------------------------------
%%% @doc Write to a temporary table during the upgrade phase.
%%% @end
%%% ----------------------------------------------------------

-spec write_upgrWindow_table(Tab::atom()) -> ok.

write_upgrWindow_table(Tab) ->
    swmLib:write_upgrWindow_table(Tab).

%%%-----------------------------------------------------------
%%% @doc Write to a temporary table during the upgrade phase.
%%% @end
%%% ----------------------------------------------------------

-spec write_upgrWindow_table(Tab::atom(), Obj::tuple()) -> ok.

write_upgrWindow_table(Tab, Obj) ->
    swmLib:write_upgrWindow_table(Tab, Obj).

%%%-----------------------------------------------------------
%%% @doc Return the records of a table
%%% @end
%%% ----------------------------------------------------------

-spec all_objects(Tab::atom()) -> [tuple()].

all_objects(Tab) ->
    swmLib:all_objects(Tab).

%%%-----------------------------------------------------------
%%% @doc Retrieve all keys for all records in a table
%%% @end
%%% ----------------------------------------------------------

-spec all_keys(Tab::atom()) -> [term()].

all_keys(Tab) ->
    swmLib:all_keys(Tab).

%%%-----------------------------------------------------------
%%% @doc Read a record from the old table
%%% @end
%%% ----------------------------------------------------------

-spec read(Tab::atom(), Key::term()) -> [tuple()].

read(Tab, Key) ->
    swmLib:read(Tab, Key).

%%%-----------------------------------------------------------
%%% @doc Check if a table is present in the old database
%%% @end
%%% ----------------------------------------------------------

-spec is_old_table(Tab::atom()) -> boolean().

is_old_table(Tab) ->
    swmLib:is_old_table(Tab).

%%%-----------------------------------------------------------
%%% @doc Return the first key of the table
%%% @end
%%% ----------------------------------------------------------

-spec first(Tab::atom()) -> term().

first(Tab) ->
    swmLib:first(Tab).

%%%-----------------------------------------------------------
%%% @doc Return the next key of the table
%%% @end
%%% ----------------------------------------------------------

-spec next(Tab::atom(), Key::term()) -> '$end_of_table' | term().

next(Tab, Key) ->
    swmLib:next(Tab, Key).

%%%-----------------------------------------------------------
%%% @doc Determine if a set of attributes are included in the old record
%%% @end
%%% ----------------------------------------------------------

-spec is_attributes_in_old_record(Table::atom(), Attributes::[atom()]) -> boolean().

is_attributes_in_old_record(Table, Attributes) ->
    swmLib:is_attributes_in_old_record(Table, Attributes).

%%%-----------------------------------------------------------
%%% @doc Transform an old db object
%%% New attributes must be supplied with default values
%%% Old attributes will be automatically removed
%%% @end
%%% ----------------------------------------------------------

-spec transform_obj(Object::tuple(), Added::[{Key::atom(), Value::any()}]) -> 
			   NewObject::tuple().

transform_obj(Object, Added) ->
    swmLib:transform_obj(Object, Added).

%%%-----------------------------------------------------------
%%% @doc Signal that all activation activities are complete
%%% @end
%%% ----------------------------------------------------------

-spec activation_complete() -> ok.

activation_complete() ->
    swmServer:activation_complete().

%%%-----------------------------------------------------------
%%% @doc get_current_up_metadata()
%%% @end
%%% ----------------------------------------------------------

-type up_metadata_key()::productName|proxductNumber|productRevision|description|type.
-type datetime()::{{Year::integer(), Month::integer(), Day::integer()},
		   {Hour::integer(), Minute::integer(), Second::integer()}}.
-type up_metadata_element()::{Key::up_metadata_key(), Value::string()} | 
			     {Key::productionDate, Value::datetime()}.
-type up_metadata_type()::[up_metadata_element()].
-spec get_current_up_metadata() -> up_metadata_type().

get_current_up_metadata() ->
    swmLib:get_current_up_metadata().

get_bootfallback_up_metadata() ->
    swmLib:get_bootfallback_up_metadata().

%%%-----------------------------------------------------------
%%% @doc Revert to a specified backup. 
%%% Used if escalation has exhausted all more immediate recovery options
%%% for example cold with test does not remedy the problem.
%%% Three options exists
%%% latest: Use the latest made scheduled backup
%%% after_ug: Use the backup made immediately after an upgrade
%%% before_ug: Use the backup made immediately before an upgrade
%%%
%%% If either of these backups are not available or if a failure occurs this
%%% function returns nok
%%% The function returns ok upon initiating the backup restore process, however
%%% it is not an indication that the restore is complete, because that will
%%% occur after a restart
%%% @end
%%% ----------------------------------------------------------

-spec revert(Type::latest|after_ug|before_ug) -> ok | nok.

revert(Type) ->
    swmFallbackList:revert(Type).

%%%-----------------------------------------------------------
%%% @doc Clean files
%%% Reduce the usage of the /rcs partition by removing items that can be 
%%% saftely removed
%%% @end 
%%%-----------------------------------------------------------

-spec clean_files() -> any().

clean_files() ->
    swmLib:clean_files().

%%%-----------------------------------------------------------
%%% @doc Clean disk
%%% Callback for sysServer when there is a need to clean the disk
%%% @end 
%%%-----------------------------------------------------------

-spec clean_disk(Severity::minor|major) -> any().

clean_disk(Severity) ->
    swmLib:clean_disk(Severity).

%%%-----------------------------------------------------------
%%% @doc Get MO attributes for 'SwM'
%%% 
%%% @end 
%%%-----------------------------------------------------------

-spec moGet_SwM() -> list().

moGet_SwM() ->
    try mnesia:dirty_read(swM, mnesia:dirty_first(swM)) of
	[Obj] ->
	    moGet_SwM_swMId(Obj) ++
		moGet_SwM_reportProgress(Obj) ++
		moGet_SwM_fallbackTimer(Obj) ++
		moGet_SwM_timeRemainingBeforeFallback(Obj) ++
		moGet_SwM_localFileStorePath(Obj) ++
		moGet_SwM_userLabel(Obj) ++
		moGet_SwM_timeoutFallbackCapability(Obj) ++
		moGet_SwM_actionCapable(Obj) ++
		moGet_SwM_actionCapableInfo(Obj);
	[] ->
	    []
    catch
	_ : _ ->
	    []
    end.

%%%-----------------------------------------------------------
moGet_SwM_swMId(#swM{swMId = {_, _, SwMId}}) ->
    [{swMId, [], [SwMId]}].

%%%-----------------------------------------------------------
moGet_SwM_reportProgress(#swM{reportProgress = undefined}) ->
    [];
moGet_SwM_reportProgress(#swM{reportProgress = Val}) ->
    ?LOG_INFO([{not_implemented_yet, Val}]),
    [{reportProgress, [], []}].

%%%-----------------------------------------------------------
moGet_SwM_fallbackTimer(#swM{fallbackTimer = Val}) ->
    [{fallbackTimer, [], [sysUtil:term_to_string(Val)]}].

%%%-----------------------------------------------------------
moGet_SwM_timeRemainingBeforeFallback(#swM{timeRemainingBeforeFallback = Val})->
    [{timeRemainingBeforeFallback, [], [sysUtil:term_to_string(Val)]}].

%%%-----------------------------------------------------------
moGet_SwM_localFileStorePath(#swM{localFileStorePath = undefined}) ->
    [];
moGet_SwM_localFileStorePath(#swM{localFileStorePath = Val}) ->
    [{localFileStorePath, [], [sysUtil:term_to_string(Val)]}].

%%%-----------------------------------------------------------
moGet_SwM_userLabel(#swM{userLabel = undefined}) ->
    [];
moGet_SwM_userLabel(#swM{userLabel = Val}) ->
    [{userLabel, [], [sysUtil:term_to_string(Val)]}].

%%%-----------------------------------------------------------
moGet_SwM_timeoutFallbackCapability(#swM{timeoutFallbackCapability = Enum}) ->
    [{timeoutFallbackCapability,
      [],
      [enum2Str_timeoutFallbackCapability(Enum)]}].

enum2Str_timeoutFallbackCapability(?SwMTimeoutFallbackCapability_SUPPORTED) ->
    "SUPPORTED";
enum2Str_timeoutFallbackCapability(?SwMTimeoutFallbackCapability_NOT_SUPPORTED) ->
    "NOT_SUPPORTED";
enum2Str_timeoutFallbackCapability(_) ->
    "UNKNOWN".

%%%-----------------------------------------------------------
moGet_SwM_actionCapable(#swM{actionCapable = Enum}) ->
    [{actionCapable, [], [enum2Str_actionCapable(Enum)]}].

enum2Str_actionCapable(?ActionCapabilityState_CAPABLE) ->
    "CAPABLE";
enum2Str_actionCapable(?ActionCapabilityState_WAIT) ->
    "WAIT";
enum2Str_actionCapable(_) ->
    "UNKNOWN".

%%%-----------------------------------------------------------
moGet_SwM_actionCapableInfo(#swM{actionCapableInfo = undefined}) ->
    [];
moGet_SwM_actionCapableInfo(#swM{actionCapableInfo = Val}) ->
    [{actionCapableInfo, [], [sysUtil:term_to_string(Val)]}].


%%%-----------------------------------------------------------
%%% @doc Generate Ericsson support information
%%% Causes SWM to generate support information to be included in the ESI
%%% package
%%% @end
%%% ----------------------------------------------------------

-spec generate_esi() -> ok.

generate_esi() ->
    swmLib:generate_esi().

%%%-----------------------------------------------------------
%%% @doc Returns true if this restart was caused by the failsafe mechanism
%%% It remains 'true' from the init phase to the activate phase
%%% @end
%%% ----------------------------------------------------------

-spec is_failsafe_restart() -> boolean().

is_failsafe_restart() ->
    swmFailsafe:is_failsafe_restart().

%%%-----------------------------------------------------------
%%% @doc Resets the boot partition to only be capable of booting
%%% to networkloader
%%% @end
%%% ----------------------------------------------------------
-spec reset_boot(Type::soft|hard) -> ok.
reset_boot(Type) when Type == soft; Type == hard ->
    swmOs:reset_boot(Type).

%% Kept for backward compability
reset_boot() ->
    reset_boot(soft).

%%%-----------------------------------------------------------
%%% @doc Return the path to the current software archive dir
%%% @end
%%% ----------------------------------------------------------

-spec get_current_archive_dir() -> Path::string().

get_current_archive_dir() ->
    swmServer:get_current_archive_dir().

%%%%%%%%%%%%%% commented because of dialyzer warnings, erarafo 2015-09-10
%% %%%-----------------------------------------------------------
%% %%% doc Run 'ldconfig' as root
%% %%% end
%% %%% ----------------------------------------------------------
%% -spec ldconfig(Argv::string()) -> string().
%% 
%% ldconfig(Argv) ->
%%     swmOs:argv(Argv).

-spec clear_application_logs() -> string().

%%%-----------------------------------------------------------
%%% @doc Remove files
%%% @end
%%% ----------------------------------------------------------

clear_application_logs() ->
    swmOs:clear_application_logs().

%%%-----------------------------------------------------------
%%% @doc Remove files
%%% @end
%%% ----------------------------------------------------------

-spec clear_application_tmp() -> string().

clear_application_tmp() ->
    swmOs:clear_application_tmp().

%%% ###########################################################################
%%% @doc Request from LMHI (CAT) to download SW for a boardType of
%%%   hwcategory="OTHER".
%%%
%%% @end
%%% ----------------------------------------------------------

-spec add_board(ProductNumber   :: string(),
		ProductRevision :: string(),
		CallbackModule  :: atom()) ->
    ok |
	wait |
	{error, Reason :: string()}.

%%% ###=====================================================================###
add_board(ProductNumber, ProductRevision, CbModule) ->
    add_board(swmLib:get_variable(hsi_state),
	      swmLib:get_ram_variable(add_board_STATE),
	      ProductNumber,
	      ProductRevision,
	      CbModule).

%%% ###=====================================================================###
add_board(on, blocked, ProductNumber, ProductRevision, CbModule) ->
    add_board_blocked_send(ProductNumber, ProductRevision, CbModule),
    wait;
add_board(on, AbState, ProductNumber, ProductRevision, CbModule) ->
    BoardType = {ProductNumber, ProductRevision},
    case swmBoardList:boardTypeOth_add(BoardType) of
	ok ->
	    add_board_reply(AbState,
			    ProductNumber,
			    ProductRevision,
			    CbModule,
			    ok);
	_ ->
	    swmServer:add_board(AbState, BoardType, CbModule)
    end;
add_board(_, _, ProductNumber, ProductRevision, CbModule) ->
    ?LOG_WARN(["HSI not enabled",
	       {productNumber, ProductNumber},
	       {productRevision, ProductRevision},
	       {cbModule, CbModule}]),
    ok.

%%% ###########################################################################
%%% add_board_blocked
%%%
%%% ###=====================================================================###
add_board_blocked(ProductNumber, ProductRevision, CbModule) ->
    case swmLib:get_ram_variable(add_board_STATE) of
	blocked ->
	    add_board_blocked_send(ProductNumber, ProductRevision, CbModule);
	_ ->
	    add_board(on, unblocked, ProductNumber, ProductRevision, CbModule)
    end.

%%% ###########################################################################
%%% add_board_blocked_send
%%%
%%% ###=====================================================================###
add_board_blocked_send(ProductNumber, ProductRevision, CbModule) ->
    timer:apply_after(1000,
		      swmDiskSrv,
		      request,
		      [#{mfaFun => {?MODULE,
				    add_board_blocked,
				    [ProductNumber, ProductRevision, CbModule]},
			 reply => false}]).

%%% ###########################################################################
%%% add_board_reply
%%%
%%% ###=====================================================================###
add_board_reply(unblocked, CbModule, ProdNo, ProdRev, Res) ->
    CbModule:board_added(ProdNo, ProdRev, Res);
add_board_reply(_, _, _, _, Res) ->
    Res.

%%%-----------------------------------------------------------
%%% @doc Register an exemption for mnesia monitoring
%%% An exemption will cause the db monitoring no to react when 
%%% a specific field of a table is changed. Multiple fields can be registered.
%%% Make sure it is the right thing to do
%%% @end
%%% ----------------------------------------------------------

-spec register_exemption(Table::atom(), Field::integer()) -> ok.

register_exemption(Table, Field) ->
    swmDbMonitor:register_exemption(Table, Field).

%%%-----------------------------------------------------------
%%% @doc Make the mnesia monitor do an autobackup immediately
%%% This function can be used if there is an urgent need to store things 
%%% permanently in the database
%%% @end
%%% ----------------------------------------------------------

-spec force_auto_backup() -> ok | {error, busy}.

force_auto_backup() ->
    swmDbMonitor:force_auto_backup().

%%% ----------------------------------------------------------
%%% @doc Return the CXP info of the CXP that is the source of a file
%%% Input is a path in $ROOT/home/sirpa/software or $ROOT/software
%%% @end
%%% ----------------------------------------------------------

-spec get_cxp_source(Path::string()) -> 
			    {ProdName::string(), Id::string(), Vsn::string()}.

get_cxp_source(Path) ->
    swmLib:get_cxp_source(Path).

%%% ----------------------------------------------------------
%%% @doc Return the paths of all appdata files in the current running up
%%% @end
%%% ----------------------------------------------------------

-spec get_appdata_files() -> [string()].

get_appdata_files() ->
    get_appdata_files(current, any).

%%% ----------------------------------------------------------
%%% @doc Return the paths of a subset of appdata files
%%% @end
%%% ----------------------------------------------------------

-spec get_appdata_files(Target::string()|any) -> [string()].

get_appdata_files(Target) ->
    get_appdata_files(current, Target).

%%% ----------------------------------------------------------
%%% @doc Return the paths of all or a subset of appdata files of any UP
%%% Up is the currently running, the other installed UP in the upgrade case
%%% or a path to a directory where an UP is unpacked
%%% @end
%%% ----------------------------------------------------------

-spec get_appdata_files(Up::current|other|string(), 
			Target::string()|any) -> [string()].

get_appdata_files(Up, Target) ->
    swmAppData:get_appdata_files(Up, Target).

%%% ----------------------------------------------------------
%%% @doc Returns the CXP root directory if given a path that's within a CXP
%%% @end
%%% ----------------------------------------------------------

-spec get_cxp_root(Path::string()) -> {ok, string()} | {error, path_not_in_cxp}.

get_cxp_root(Path) ->
    swmLib:get_cxp_root(Path).

%%-----------------------------------------------------------------------------
%% @doc Returns the node type that is specified in the UP metadata file
%%      (*-up.xml).
%% @end
%%-----------------------------------------------------------------------------
-spec node_type() ->
    string().
node_type() ->
    proplists:get_value(type,
			get_current_up_metadata(), 
			case os:getenv("NODE_TYPE") of
			    false -> "undefined";
			    Type -> Type
			end).

%%-----------------------------------------------------------------------------
%% @doc Validates a NodeType.
%% @end
%%-----------------------------------------------------------------------------

-spec is_node_type_valid(NodeType :: string()) ->
    boolean() | deprecated.

is_node_type_valid(NodeType) ->
    case lists:member(NodeType, node_types_deprecated()) of
	false ->
	    lists:member(NodeType, node_types_valid());
	true ->
	    deprecated
    end.

%%-----------------------------------------------------------------------------
%% @doc Returns a list of node types that are valid and recognized by SWM, but
%%   deprecated.
%% @end
%%-----------------------------------------------------------------------------

-spec node_types_deprecated() ->
    list(string()).

node_types_deprecated() ->
    [
     "RCP-SIM",
     "RCS-DUS2",
     "RCS-T"
    ].

%%-----------------------------------------------------------------------------
%% @doc Returns a list of physical node types that are valid and recognized by
%%   SWM.
%% @end
%%-----------------------------------------------------------------------------

-spec node_types_physical() ->
    list(string()).

node_types_physical() ->
    [
     "BPU",
     "RadioNode",    % Legacy
     "RadioTNode",   % Legacy
     "RnNode",
     "VtfRadioNode", % Verizon TF
     "5GRadioNode"   % NR
    ].

%%-----------------------------------------------------------------------------
%% @doc Returns a list of node types that are valid and recognized by SWM.
%% @end
%%-----------------------------------------------------------------------------

-spec node_types_valid() ->
    list(string()).

node_types_valid() ->
    node_types_physical() ++
	node_types_virtual() ++
	node_types_deprecated().

%%-----------------------------------------------------------------------------
%% @doc Returns a list of virtual node types that are valid and recognized by
%%   SWM.
%% @end
%%-----------------------------------------------------------------------------

-spec node_types_virtual() ->
    list(string()).

node_types_virtual() ->
    [
     "R-VNFM",
     "vPP",
     "vRC",
     "vSD"
    ].

%%% ----------------------------------------------------------
%%% @doc Creates an AI backup and places it on the restoreEscalationList
%%% Only if escalation list is empty is the backup added to list
%%% @end
%%% ----------------------------------------------------------

-spec make_ai_backup() -> ok | {error, Reason::term()}.

make_ai_backup() ->
    swmBackup:make_ai_backup().

%%% ----------------------------------------------------------
%%% @doc Clears the AI backup and removes it from restoreEscalationList
%%% @end
%%% ----------------------------------------------------------

-spec clear_ai_backup() -> ok.

clear_ai_backup() ->
    swmBackup:clear_ai_backup().

%%% ----------------------------------------------------------
%%% @doc Gets the autobackup path and filename.
%%% @end
%%% ----------------------------------------------------------

-spec get_autobackup_path() -> string().

get_autobackup_path() ->
    swmDbMonitor:get_autobackup_path().

%%% ----------------------------------------------------------
%%% @doc Checks if HSI has been activated.
%%%   HSI is 'not_applicable' when HSI is not enabled or the UP does not contain
%%%   any HSI-applicable (OTHER-categorized) LMCs.
%%% @end
%%% ----------------------------------------------------------

-spec is_hsi_active() -> boolean() | not_applicable.

is_hsi_active() ->
    is_hsi_active(sysEnv:rcs_mode_2()).

is_hsi_active(target) ->
    case swmLib:get_ram_variable(hsi_active) of
	undefined ->
	    ArchiveDir = get_current_archive_dir(),
	    {ok, [{products, AllHsi_Products}]} =
		swmBoardList:swp([products],
				 [{boardTypesOTH, all},
				  {options, [{global, ArchiveDir}]}]),
	    %% Assuming no bundled products in boardTypesOTH lists.
	    AllHsi_LmcFileNames =
		[File || {_, {global, File}} <- AllHsi_Products],
	    set_hsi_active(AllHsi_LmcFileNames, ArchiveDir);
	{false, AllHsi_LmcFileNames} ->
	    set_hsi_active(AllHsi_LmcFileNames, get_current_archive_dir());
	HsiActive ->
	    HsiActive
    end;
is_hsi_active(_) ->
    not_applicable.

%%% ----------------------------------------------------------
%%% @doc Checks if HSI has been enabled.
%%% @end
%%% ----------------------------------------------------------

-spec is_hsi_enabled() -> boolean().

is_hsi_enabled() ->
    case swmLib:get_variable(hsi_state) of
	on ->
	    true;
	_ ->
	    false
    end.

%%% ----------------------------------------------------------
%%% @doc Initiate a backup restore
%%% Used by AIC to trigger an restore in case of AI failure
%%% @end
%%% ----------------------------------------------------------

-spec restore_backup(BuName::string()) -> ok | {error, term()}.
    
restore_backup(BuName) ->
    swmBackup:restore_backup(BuName).

%%% ----------------------------------------------------------
%%% @doc Remount tmpfs with another size
%%% Returns the exit code and any response from cup
%%% The caller must act on any unforeseen exits
%%% @end
%%% ----------------------------------------------------------

-spec remount_tmpfs_size_cmd(Size::string(), Path::string()) -> {ExitCode::integer(), Return::string()}.

remount_tmpfs_size_cmd(Size, Path) ->
    swmOs:remount_tmpfs_size_cmd(Size, Path).

%%% ----------------------------------------------------------
%%% @doc Reset the db write quarantine
%%% Called by COMSA to enable immediate DB write after a reconfiguration
%%% @end
%%% ----------------------------------------------------------

-spec quarantine_reset() -> ok.

quarantine_reset() ->
    swmDbMonitor:quarantine_reset().

%%% ----------------------------------------------------------
%%% @doc New networkManagedElementId
%%% Called by COMSA when the networkManagedElementId has changed to write 
%%% in the HW Log.
%%% @end
%%% ----------------------------------------------------------

-spec hw_log(NetworkManagedElementId::string()) -> ok.

hw_log(NetworkManagedElementId) ->
    swmInventory:hw_log(NetworkManagedElementId).
   
%%% ----------------------------------------------------------
%%% @doc Formats the next scheduled time of the BrmBackupScheduler MO
%%% The stored format of BrmBackupScheduler::nextScheduledTime is internal to
%%% SWM and can be converted to a readable string with this function
%%% @end
%%% ----------------------------------------------------------

-spec format_next_scheduled_time(Opaque::any()) -> Date::string() | undefined.

format_next_scheduled_time(NextScheduledTime) ->
    swmBackupModel:format_next_scheduled_time(NextScheduledTime).


%% select_apply(M, F, A) ->
%%     NewM = select_apply(sysEnv:rcs_mode_2(), M),
%%     apply(NewM, F, A).

%% select_apply(vrcs, swmBackup) -> swmcBackup;
%% select_apply(_, M) -> M.
    
%%% ----------------------------------------------------------
%%% @doc Make a db backup before the db monitor starts
%%% Returns error if there is a disk fault, checksum generating fault
%%% or encryption fault
%%% @end
%%% ----------------------------------------------------------

-spec initial_auto_backup() -> ok | error.

initial_auto_backup() ->
    swmDbMonitor:initial_auto_backup().

%%% ----------------------------------------------------------
%%% @doc Install a ug database as upgrade_init db
%%% @end
%%% ----------------------------------------------------------

-spec init_config_prepare() -> ok.

init_config_prepare() ->
    swmvBackup:init_config_prepare().


-spec register_bu_callback(Module::atom(), Function::atom()) -> ok.
register_bu_callback(Module, Function) ->
    swmLib:register_bu_callback({Module, Function}).

-spec remove_bu_callback(Module::atom(), Function::atom()) -> ok.
remove_bu_callback(Module, Function) ->
    swmLib:remove_bu_callback({Module, Function}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%% set_hsi_active
%%% 
%%% ###=====================================================================###
set_hsi_active([], _) ->
    swmLib:set_ram_variable(hsi_active, not_applicable),
    not_applicable;
set_hsi_active(AllHsi_LmcFileNames, ArchiveDir) ->
    {ok, Archive_FileNames} = file:list_dir(ArchiveDir),
    case AllHsi_LmcFileNames -- Archive_FileNames of
	[] ->
	    swmLib:set_ram_variable(hsi_active, {false, AllHsi_LmcFileNames}),
	    false;
	[_ | _] ->
	    swmLib:set_ram_variable(hsi_active, true),
	    true
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


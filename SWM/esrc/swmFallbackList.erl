%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmFallbackList.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/1

%%% @doc ==Fallback list==
%%% This module contains the logic for handling the backup fallback list used
%%% to provide the catastrophic failure recovery mechanism with proper backups
%%% for system recovery.
%%% The swmFallbackList table is not visible to the operator. Instead there
%%% exists another object in the BrM model which is handled by swmBackup.
%%% The reason for this list is that we need more data.
%%% @end

-module(swmFallbackList).
-vsn('/main/R2A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/1').
-date('2017-12-06').
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-03-24 etxjotj     Created
%%% R2A/2      2014-03-24 etxarnu     Select proper backup at revert
%%% R2A/3      2014-03-25 etxjotj     Audit fallback list
%%% R2A/4      2014-03-31 etxarnu     Corrected brefore->before in revert/1
%%% R2A/5      2014-04-09 etxjotj     Some refactoring to find a fault
%%% R2A/6      2014-04-09 etxjotj     Backup not being present when demoting
%%% R2A/7      2014-04-25 etxjotj     Fix of HS51856
%%% R2A/8      2014-04-25 etxjotj     Added history
%%% R4A/1      2015-07-10 etxjotj     HT88822 Filter duplicate backups in list
%%% R4A/2      2015-08-24 etxpejn     Changed logI:write_log to swmLib:write_swm_log
%%% R5A/1      2016-01-26 etxjotj     HU52423 Backup promotion fix
%% R6A/1   2016-09-14 etxberb  Added get_bootfallback_up/0,
%%                             audit_bootfallback/0, is_bootfallback_complete/0,
%%                             remove_bootfallback_complete/0,
%%                             set_bootfallback_complete/0.
%% R6A/2   2016-09-15 etxberb  Fixed bug in audit_bootfallback/1 for vrcs.
%% R7A/1   2016-10-07 etxjotj  Added clear list option
%%                             Backups don't change creationType 
%% R8A/1   2016-10-25 etxjotj  Escalation list default backup
%% R8A/2   2016-11-03 etxjotj  Avoid escalation fallback creation during upgrade
%% R8A/3   2016-12-01 etxjotj  HV46202 Don't make escalation list default backup
%%                             if there is a AI backup present
%% R8A/4   2016-12-01 etxjotj  Bugfix
%% R8A/5   2017-01-16 etxjotj  HV55838 Improved AI backup handling
%% R9A/2   2017-02-17 etomist  HV61068, clear pending_ug_backup in add_pending
%% R11A/1  2017-08-03 etxberb  Bootfallback enabled:
%%                             * Using --no-mount option.
%%                             * Aligned with Fast Restore.
%%                             * Added bootfallback_sw/0,
%%                               remove_bootfallback_flag/0.
%% R11A/2  2017-08-21 etxberb  Added is_bootfallback_enabled/0.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([add_backup/2]).
-export([add_pending/0, cancel_pending/0, pending_ug_backup/1]).
-export([revert/1]).
-export([get_bootfallback_up/0, get_fallback_list/0]).
-export([audit_fallback_list/0, clear_fallback_list/0]).
-export([audit_bootfallback/0,
	 bootfallback_sw/0,
	 is_bootfallback_complete/0,
	 is_bootfallback_enabled/0,
	 remove_bootfallback_complete/0,
	 remove_bootfallback_flag/0,
	 set_bootfallback_complete/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("SwmInternal.hrl").
-include("RcsBrM.hrl").

-define(BFbInstComplFileName, "bootfallback_install_complete").
-define(BFbInstComplFile, filename:join(sysEnv:home_dir(),
					?BFbInstComplFileName)).
-define(BFbInstComplFile_other, filename:join(swmOs:home_dir_other(),
					      ?BFbInstComplFileName)).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-type escalation_type()::latest | before_ug | after_ug.

%%% ----------------------------------------------------------
%%% @doc Add a backup to the rollback list
%%% @end
%%% ----------------------------------------------------------

-spec add_backup(Type::escalation_type(), Name::string()) -> ok.
add_backup(Type, Name) ->
    add_backup(Type, Name, sysEnv:rcs_mode_2()).

add_backup(_, _, vrcs) -> ok;
add_backup(Type, Name, _) ->
    case mnesia:transaction(fun() -> do_add_backup(Type,Name) end) of
	{atomic, _} ->
	    %% HV55838
	    %% Housekeeping of Escalation default backups
	    case Type of 
		latest ->
		    %% Clear escalation defaults, except the one we're adding
		    %% Name can actually be any backup name, but that doesn't 
		    %% matter because we should not clear the backup we've just
		    %% added 
		    clear_escalation_default(Name);
		_ -> 
		    ok
	    end,
	    %% Creation type should no longer be changed
	    %% [swmBackup:set_creation_type(Index, NewCT)||
	    %% 	{Index, NewCT}<-FileList],
	    ok;
	{aborted, no_such_backup} ->
	    error_msg("Backup ~p does not exist~n",[Name]);
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [Type, Name])
    end.


%%% ----------------------------------------------------------
%%% @doc The backup before upgrade is added later but save the name
%%% @end
%%% ----------------------------------------------------------

-spec pending_ug_backup(BuName::string()) -> ok.

pending_ug_backup(BuName) ->
    case sysEnv:rcs_mode_2() of
	vrcs -> ok;
	_ ->
	    swmLib:set_variable(pending_ug_backup, BuName)
    end.

%%% ----------------------------------------------------------
%%% @doc A pending backup is removed as pending
%%% @end
%%% ----------------------------------------------------------

%% HS51856
-spec cancel_pending() -> ok.

cancel_pending() ->
    swmLib:erase_variable(pending_ug_backup).

%%% ----------------------------------------------------------
%%% @doc The backup before upgrade is added to the fallback list after upgrade
%%% @end
%%% ----------------------------------------------------------

-spec add_pending() -> ok.

add_pending() ->
    IsVrcs = sysEnv:rcs_mode_2() == vrcs,
    case swmLib:get_variable(pending_ug_backup) of
	BuName when is_list(BuName), not IsVrcs ->
	    add_backup(before_ug, BuName),
            swmLib:erase_variable(pending_ug_backup); % HV61068
	_ ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Reset the bootfallback_install_complete flag on the configured home dir
%%%      and initiate install of a bootfallback on other dir if not already in
%%%      place.
%%% @end
%%% ----------------------------------------------------------
-spec audit_bootfallback() -> ok | {error, Reason :: term()}.

audit_bootfallback() ->
    audit_bootfallback(sysEnv:rcs_mode_2()).

audit_bootfallback(target) ->
    case file:read_file_info(?BFbInstComplFile) of
	{ok, _FileInfo} ->
	    info_msg("Starting up on BootFallback instance~n", []),
	    file:delete(?BFbInstComplFile),
	    FbInstance = swmOs:get_fallback_instance(),
	    swmOs:set_boot_instance(configured, FbInstance);
	_ ->
	    %% Normal start on configured instance.
	    ok
    end,
    case is_bootfallback_complete() of
	true ->
	    ok;
	false ->
	    swmOs:homes(mount),
	    swmBackup:install_bootfallback(),
	    swmOs:homes(umount)
    end;
audit_bootfallback(_) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Checks if there is a fallback UP installed on the other home dir.
%%% @end
%%% ----------------------------------------------------------
-spec is_bootfallback_complete() -> boolean().

is_bootfallback_complete() ->
    case swmOs:get_fallback_instance() /= swmOs:get_active_instance() of
	true ->
	    case swmOs:is_home_mounted(inactive) of
		true ->
		    is_bootfallback_file();
		false ->
		    swmOs:homes(mount),
		    Res = is_bootfallback_file(),
		    swmOs:homes(umount),
		    Res
	    end;
	false ->
	    false
    end.

is_bootfallback_file() ->
    case file:read_file_info(?BFbInstComplFile_other) of
	{ok, _FileInfo} ->
	    true;
	_ ->
	    false
    end.

%%% ----------------------------------------------------------
%%% @doc Checks if the bootfallback feature is enabled.
%%% @end
%%% ----------------------------------------------------------
-spec is_bootfallback_enabled() -> boolean().

is_bootfallback_enabled() ->
    swmLib:get_variable(bootfallback_enabled).

%%% ----------------------------------------------------------
%%% @doc Returns a list of installed bootfallback software.
%%% @end
%%% ----------------------------------------------------------
-spec bootfallback_sw() -> list(string()).

bootfallback_sw() ->
    case is_bootfallback_complete() of
	true ->
	    {ok, Software} = file:list_dir(swmLib:software_dir_other()),
	    Software;
	false ->
	    []
    end.

%%% ----------------------------------------------------------
%%% @doc Remove the installed bootfallback on the other home dir completely.
%%%   NOTE: Called only from swmOs:clear/0.
%%% @end
%%% ----------------------------------------------------------
-spec remove_bootfallback_complete() -> ok | {error, Reason :: term()}.

remove_bootfallback_complete() ->
    remove_bootfallback_complete(sysEnv:rcs_mode_2()).

remove_bootfallback_complete(target) ->
    file:delete(?BFbInstComplFile_other),
    swmOs:remove_installation_inactive(),
    Remove = bootfallback_sw() -- swmServer:current_all_sw(),
    info_msg("Removing BootFallback~nThe following will be removed: ~p~n",
	     [Remove]),
    swmServer:software_remove(Remove),
    info_msg("BootFallback installation removed~n", []);
remove_bootfallback_complete(_) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Remove the flag for bootfallback on the other home dir. By doing this,
%%%   the bootfallback installation is available to use for other purposes, such
%%%   as restore.
%%% @end
%%% ----------------------------------------------------------
-spec remove_bootfallback_flag() -> ok | {error, Reason :: term()}.

remove_bootfallback_flag() ->
    remove_bootfallback_flag(sysEnv:rcs_mode_2()).

remove_bootfallback_flag(target) ->
    file:delete(?BFbInstComplFile_other),
    info_msg("BootFallback flag removed~n", []);
remove_bootfallback_flag(_) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Set a flag that a bootfallback UP is installed on the other home dir.
%%% @end
%%% ----------------------------------------------------------
-spec set_bootfallback_complete() -> ok | {error, Reason :: term()}.

set_bootfallback_complete() ->
    info_msg("BootFallback installation complete~n", []),
    file:write_file(?BFbInstComplFile_other, <<>>).

%%% ----------------------------------------------------------
%%% @doc Return the UP best fit for becoming bootfallback
%%% @end
%%% ----------------------------------------------------------
-spec get_bootfallback_up() -> [string()] | undefined.

get_bootfallback_up() ->
    case mnesia:transaction(fun do_get_bootfallback_up/0) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {aborted, Reason}]),
	    undefined
    end.

do_get_bootfallback_up() ->
    case mnesia:read(swmFallbackList, before_ug) of
	[#swmFallbackList{name = Name}] ->
	    Name;
	[] ->
	    undefined
    end.

%%% ----------------------------------------------------------
%%% @doc Return the list of escalation backups, newest first
%%% @end
%%% ----------------------------------------------------------
-spec get_fallback_list() -> [string()] | undefined.

get_fallback_list() ->
    case mnesia:transaction(fun do_get_fallback_list/0) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, get_fallback_list},
	       {aborted, Reason}]),
	    undefined
    end.
      
do_get_fallback_list() ->
    WP = mnesia:table_info(swmFallbackList, wild_pattern),
    case mnesia:match_object(WP) of
	[] ->
	    undefined;
	Objs ->
	    Sorted = lists:keysort(#swmFallbackList.date, Objs),
	    %% HT88822 Backups should not appear twice
	    %% After upgrade the 'latest' slot needs to be filled with a 
	    %% different value, therefore, the same backup may appear twice
	    %% but the escalation list should not show that

	    %% 'Sorted' should give us the oldest backup first
	    %% But in the escalation list we need the newest backup first
	    lists:foldl(fun(Obj, List) ->
				Name = Obj#swmFallbackList.name,
				case lists:member(Name, List) of
				    true ->
					List;
				    false -> 
					%% Make names come in reverse order
					[Name|List]
				end
			end, [], Sorted)
    end.
	    

%%%-----------------------------------------------------------
%%% @doc Revert to a specified backup
%%% Makes the system initiate a backup restore operation with the backup
%%% given by type. The exact backup name is retrieved from the swmFallbackList
%%% @end
%%% ----------------------------------------------------------

-spec revert(Type::escalation_type()) -> ok | nok.

revert(Type) ->
    Msg = 
	case Type of
	    latest -> 
		"Reverting to the latest scheduled backup";
	    after_ug ->
		"Reverting to the latest post upgrade backup";
	    before_ug ->
		"Reverting to the latest pre upgrade backup"
	end,
    swmLib:write_swm_log("BrM", info, Msg),
    {atomic,Name} = mnesia:transaction(
		      fun()->
			      case mnesia:read({swmFallbackList,Type}) of
				  [#swmFallbackList{name=N}] ->
				      N;
				  [] ->
				      ""
			      end
		      end  ),
    case swmBackup:restore_backup(Name) of
	ok ->
	    ok;
	{error, Reason} ->
	    info_msg("Revert failed ~p~n",[Reason]),
	    nok
    end.
	
%%% ----------------------------------------------------------
%%% @doc Check if all the backups in the rollback list are still there
%%% @end
%%% ----------------------------------------------------------

-spec audit_fallback_list() -> ok.
    
audit_fallback_list() ->
    case mnesia:transaction(fun do_audit_fallback_list/0) of
	{atomic, ok} ->
	    ok;
	{atomic, empty} -> 
	    %% HV46202 The AI backup takes precedence before the default bu
	    IsUpgradeOngoing = swmI:is_upgrade_ongoing(),
	    IsAiBuFile = filelib:is_file(swmLib:ai_bu_file()),
	    case {IsUpgradeOngoing, IsAiBuFile} of
		{true, _ } -> 
		    info_msg("Escalation list is empty but upgrade is "
			     "ongoing~n");
		{_, true} ->
		    info_msg("Escalation list is empty but there is an "
			     "AI backup~n");
		_ ->
		    create_escalation_default()
	    end;
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{mfa, {?MODULE, audit_fallback_list, []}},
	       {aborted, Reason}])
    end.

do_audit_fallback_list() ->
    do_audit_fallback_list(mnesia:first(swmFallbackList)),
    %% Note: the two calls to mnesia:first may not give the same result
    case mnesia:first(swmFallbackList) of
	'$end_of_table' -> 
	    empty;
	_ ->
	    ok
    end.

do_audit_fallback_list('$end_of_table') ->
    ok;
do_audit_fallback_list(Key) ->
    [Obj] = mnesia:read({swmFallbackList, Key}),
    case swmBackupModel:get_backup_by_name(Obj#swmFallbackList.name) of
	[] ->
	    mnesia:delete({swmFallbackList, Key}),
	    do_audit_fallback_list(mnesia:next(swmFallbackList, Key));
	[_] ->
	    do_audit_fallback_list(mnesia:next(swmFallbackList, Key))
    end.

create_escalation_default() ->
    case sysEnv:rcs_mode_2() of
	vrcs -> ok;
	_ ->
	    do_create_escalation_default()
    end.

do_create_escalation_default() ->
    Mgr = {"1","1","1","1"},    
    Time = comsaI:iso_time(os:timestamp(), basic),
    Name = "Escalation_default_"++Time,
    BuName = list_to_binary(Name),
    try swmBackup:create_backup_common(BuName,Mgr,system,undefined) of
	MoRef ->
	    Index = lists:last(string:tokens(MoRef, "=")),
	    swmLib:unlock_backup(Index),
	    ok = add_backup(latest, Name)
    catch Type:Error ->
	    error_msg("Creating of fallback list backup failed: ~p~n",
		      [{Type, Error}])
    end.

%% HV55838 Housekeeping of escalation default
clear_escalation_default(ReservedName) ->    
    [begin
	 BackupName = BrmBackup#brmBackup.backupName,
	 swmBackup:delete_system_created_backup(BackupName, undefined),
	 BackupName
     end||BrmBackup<-ets:tab2list(brmBackup),
	  is_removable(BrmBackup, ReservedName)].

is_removable(BrmBackup, Reserved) ->
    BackupName = BrmBackup#brmBackup.backupName,
    case BackupName of
	Reserved ->
	    false;
	"Escalation_default_" ++_ ->
	    case BrmBackup#brmBackup.creationType of
		?BrmBackupCreationType_SYSTEM_CREATED ->
		    true;
		_ -> 
		    %% This backup has been exported and imported again
		    %% and is part of manual housekeeping
		    false
	    end;
	_ ->
	    false
    end.



%%% ----------------------------------------------------------
%%% @doc Clear the rollbackEcalationList
%%% @end
%%% ----------------------------------------------------------

-spec clear_fallback_list() -> ok.

clear_fallback_list() ->
    {atomic, ok} = mnesia:transaction(fun do_clear_fallback_list/0),
    ok.

do_clear_fallback_list() ->
    Keys = mnesia:all_keys(swmFallbackList),
    [mnesia:delete({swmFallbackList, Key})||Key<-Keys],
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           do_add_backup(Type, Name)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Runs within a transaction
%%% ----------------------------------------------------------

do_add_backup(Type, Name) ->
    Obj = get_BrmBackup_object(Name),
    %% HU52423 
    %% Update metadatafile for promoted backups also
    %% PromotedBackups = promote_backup(Obj),
    %% DemotedBackups = demote_backup(Type),

    CreationType = Obj#brmBackup.creationType,
    CreationTime = Obj#brmBackup.creationTime,
    {absolute, Time} = swmLib:parse_date(CreationTime),
    mnesia:write(#swmFallbackList{key = Type,
				  name = Name,
				  creationType = CreationType,
				  date = Time}),
    %% PromotedBackups++DemotedBackups.
    [].

get_BrmBackup_object(Name) ->
     case swmBackupModel:get_backup_by_name(Name) of
	 [O] -> O;
	 [] -> mnesia:abort(no_such_backup)
     end.
    
%% promote_backup(Obj) ->
%%     CreationType = Obj#brmBackup.creationType,
%%     SC = ?BrmBackupCreationType_SYSTEM_CREATED,
%%     Index = lists:last(tuple_to_list(Obj#brmBackup.brmBackupId)),
%%     %% Promote to system created status for delete protection
%%     case CreationType of
%% 	?BrmBackupCreationType_MANUAL ->
%% 	    mnesia:write(Obj#brmBackup{creationType = SC}),
%% 	    %%HU52423 Data to set_creation_type call
%% 	    [{Index, SC}];
%% 	?BrmBackupCreationType_SCHEDULED ->
%% 	    mnesia:write(Obj#brmBackup{creationType = SC}),
%% 	    %%HU52423 Data to set_creation_type call
%% 	    [{Index, SC}];
%% 	?BrmBackupCreationType_SYSTEM_CREATED -> 
%% 	    []
%%     end.    

%% demote_backup(Type) ->
%%     case mnesia:read({swmFallbackList, Type}) of
%% 	[] -> [];
%% 	[Current]  ->
%% 	    CName = Current#swmFallbackList.name,
%% 	    case swmBackupModel:get_backup_by_name(CName) of
%% 		[CBu] ->
%% 		    NewCT = Current#swmFallbackList.creationType,
%% 		    mnesia:write(CBu#brmBackup{creationType = NewCT}),
%% 		    Key = CBu#brmBackup.brmBackupId,
%% 		    Index = lists:last(tuple_to_list(Key)),
%% 		    [{Index, NewCT}];
%% 		[] -> 
%% 		    []
%% 	    end
%%     end.

info_msg(Format) ->
   info_msg(Format, []).
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%    warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

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


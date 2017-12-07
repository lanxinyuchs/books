%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmFallbackList.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R4A/R5A/1

%%% @doc ==Fallback list==
%%% This module contains the logic for handling the backup fallback list used
%%% to provide the catastrophic failure recovery mechanism with proper backups
%%% for system recovery.
%%% The swmFallbackList table is not visible to the operator. Instead there
%%% exists another object in the BrM model which is handled by swmBackup.
%%% The reason for this list is that we need more data.
%%% @end

-module(swmFallbackList).
-vsn('/main/R2A/R4A/R5A/1').
-date('2016-01-26').
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([add_backup/2]).
-export([add_pending/0, cancel_pending/0, pending_ug_backup/1]).
-export([revert/1]).
-export([get_fallback_list/0]).
-export([audit_fallback_list/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("SwmInternal.hrl").
-include("RcsBrM.hrl").

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
					      
    case mnesia:transaction(fun() -> do_add_backup(Type,Name) end) of
	{atomic, FileList} ->
	    [swmBackup:set_creation_type(Index, NewCT)||
		{Index, NewCT}<-FileList],
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
    swmLib:set_variable(pending_ug_backup, BuName).

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
    case swmLib:get_variable(pending_ug_backup) of
	BuName when is_list(BuName) ->
	    add_backup(before_ug, BuName);
	_ ->
	    ok
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
    
audit_fallback_list() ->
    case mnesia:transaction(fun do_audit_fallback_list/0) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{mfa, {?MODULE, audit_fallback_list, []}},
	       {aborted, Reason}])
    end.

do_audit_fallback_list() ->
    do_audit_fallback_list(mnesia:first(swmFallbackList)).

do_audit_fallback_list('$end_of_table') ->
    ok;
do_audit_fallback_list(Key) ->
    [Obj] = mnesia:read({swmFallbackList, Key}),
    case swmBackup:get_backup_by_name(Obj#swmFallbackList.name) of
	[] ->
	    mnesia:delete({swmFallbackList, Key}),
	    do_audit_fallback_list(mnesia:next(swmFallbackList, Key));
	[_] ->
	    do_audit_fallback_list(mnesia:next(swmFallbackList, Key))
    end.
	    

    

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
    PromotedBackups = promote_backup(Obj),
    DemotedBackups = demote_backup(Type),

    CreationType = Obj#brmBackup.creationType,
    CreationTime = Obj#brmBackup.creationTime,
    {absolute, Time} = swmLib:parse_date(CreationTime),
    mnesia:write(#swmFallbackList{key = Type,
				  name = Name,
				  creationType = CreationType,
				  date = Time}),
    PromotedBackups++DemotedBackups.

get_BrmBackup_object(Name) ->
     case swmBackup:get_backup_by_name(Name) of
	 [O] -> O;
	 [] -> mnesia:abort(no_such_backup)
     end.
    
promote_backup(Obj) ->
    CreationType = Obj#brmBackup.creationType,
    SC = ?BrmBackupCreationType_SYSTEM_CREATED,
    Index = lists:last(tuple_to_list(Obj#brmBackup.brmBackupId)),
    %% Promote to system created status for delete protection
    case CreationType of
	?BrmBackupCreationType_MANUAL ->
	    mnesia:write(Obj#brmBackup{creationType = SC}),
	    %%HU52423 Data to set_creation_type call
	    [{Index, SC}];
	?BrmBackupCreationType_SCHEDULED ->
	    mnesia:write(Obj#brmBackup{creationType = SC}),
	    %%HU52423 Data to set_creation_type call
	    [{Index, SC}];
	?BrmBackupCreationType_SYSTEM_CREATED -> 
	    []
    end.    

demote_backup(Type) ->
    case mnesia:read({swmFallbackList, Type}) of
	[] -> [];
	[Current]  ->
	    CName = Current#swmFallbackList.name,
	    case swmBackup:get_backup_by_name(CName) of
		[CBu] ->
		    NewCT = Current#swmFallbackList.creationType,
		    mnesia:write(CBu#brmBackup{creationType = NewCT}),
		    Key = CBu#brmBackup.brmBackupId,
		    Index = lists:last(tuple_to_list(Key)),
		    [{Index, NewCT}];
		[] -> 
		    []
	    end
    end.

%% info_msg(Format) ->
%%    info_msg(Format, []).
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


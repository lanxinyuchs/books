%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmServer.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/R5A/18

%%% @doc ==The software management service==
%%% This model contains software for upgrade
%%% The actions are modelled in the ECIM SwM model, and the instrumentation
%%% is implemented in the swmModel module
%%%
%%% ==File overview==
%%% SWM manages software and backup places in a lot of places
%%% ===The software archive===
%%% The archive is located at /rcs/swm/software
%%% In there the original container files are stored unaltered. The container
%%% files are organized in directories corresponding to upgrade packages.
%%% ===The active software===
%%% The active software is located at /home/$USER/software
%%% This directory contains the *-up
%%% ==Functions overview==
%%% ===Startup functions===
%%%
%%% start/0, activate/0
%%% These are general to the framework of the system startup sequence
%%%
%%% ===SWM actions===
%%%
%%% create_package/2, remove_package/1, prepare_package/1,
%%% verify_package/1, activate_package/1, commit_package/1, cancel/1
%%%
%%% These functions correspond to the actions in the ECIM SwM model
%%% and generally creates an asynchronous job executed by the
%%% swmServer process.
%%%
%%% ===Other functions===
%%%
%%% These functions are support functions which need to be exported for a reason
%%%
%%% activation_complete/0, update_progress/1, install_on_other/1,
%%% is_upgrade_ongoing/0, open_dets/2
%%%
%%% Documentation is available on the indiviual functions
%%%
%%% ==Asynchronous actions==
%%%
%%% Most sequences here is regarded as asynchronous actions, which
%%% means that they should report the result in as progress
%%% information. Because of this the swmserver may not have
%%% uncontrolled crashes which may lead to a progress not being
%%% reported. The action_handler/1 function takes an action fun and
%%% catches all crashes so that the action result is properly
%%% reported. This function can also take care of closing of
%%% connections and other clean up activities.
%%%
%%% Sequence: action_handler/1 -> handle_cancelled/0 -> cleanup/0
%%% Sequence: action_handler/1 -> handle_fail/1 -> cleanup/0
%%%
%%% ==Progress updates==
%%%
%%% In ECIM progress reporting of asynchronous actions are standardized
%%% through the ECIM common library. A common implementation for updating
%%% the #'AsyncActionProgress' record is available in swmLib.
%%% Progress data is received as a list of key-value tuples.
%%%
%%% This is done within the scope of a transaction.
%%%
%%% Sequence: update_progress/1 -> mnesia:read/1 -> swmLib:update/2 ->
%%%           mnesia:write/1
%%%
%%% The default_progress_report/2 creates a default record if the attribute
%%% is not set.
%%%
%%% ==SwM Actions==
%%% Generally the functions enabling actions begin with "handle_", so
%%% the create_package/1 call will be implemented in the swmServer as
%%% handle_create_package and so on. That's the point of entry for all modules.
%%%
%%% ===Create upgrade package===
%%%
%%% This action should download the cxs metadata file of an upgrade
%%% package, create the UpgradePackage MO and set the state to
%%% initialized. However, since we ourselves build UP containers, it
%%% can also handle that the user points to such a file instead. In
%%% that case it should go to state prepared instead.
%%%
%%% The CXS metadata file is named *-up.xml (earlier cxs*.xml)
%%% The CXP metadata file is named cxp*.xml
%%%
%%% Sequence: handle_create_package/3 -> start_channel/4 -> download_up_info/6
%%% Alt 1: download_up_info/6
%%% Alt 2: download_up_info/6 -> download_up_file/2 -> loop_download/7
%%%
%%% The alternative depends on whether there is a file named *.cxs, or
%%% we find a file with extension .xml with recognizable xml content.
%%%
%%% The loop_download function can be stopped by a cancel message. It also
%%% accepts progress update messages, so if fed with those the download
%%% progress will be updated
%%%
%%% ===Prepare upgrade package===
%%%
%%% Based on the information in the CXS metadata file it should
%%% download the CXP files and place it in the archive
%%%
%%% Sequence: handle_prepare_package/1 -> download_packages/1 ->
%%%           extract_passwd/1 -> start_channel/4 -> loop_download/7
%%%
%%% ===Verify upgrade package===
%%%
%%% This action should make verifications on the UP. Nothing is done now
%%%
%%% Sequence: handle_verify_package/1 -> ok
%%%
%%% ===Activate upgrade package===
%%%
%%% The activation is where the new software is brought to use.
%%% In order to do that there are two home directory areas, the standard
%%% /home/$USER and /upgrade/home which is the alternate area.
%%% During the activate phase the /upgrade/home should be populated with
%%% all the necessary files for starting the new software.
%%%
%%% Registered upgrade triggers should be called for both erlang applications
%%% and APPM-controlled applications.
%%%
%%% A special install script from Linux EE shall be called to install the
%%% new version of the Linux OS when necessary.
%%%
%%% A late backup is taken which forms the basis of data conversion and
%%% initialization of the new database. (Mneisa is started from scratch.)
%%% Lastly, the current and the new areas should switch places and the system
%%% shall be restarted.
%%%
%%% The action is separated into two parts. One which is executed within the
%%% scope of a transaction, and a second, post_activate, which is done only
%%% if the transaction succeeds, because it has side effects due to activation
%%% of timers.
%%%
%%% Sequence: handle_activate_package/1 -> do_activate_package/1 ->
%%%           mount_software/1 -> install_on_other/1 -> call_mw_triggers/1 ->
%%%           appmI:call_upg_triggers/1 ->
%%% Sequence alt 1: soft simulated (
%%%           post_activate/0

%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(swmServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/18').
-date('2016-03-28').
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
%%% -----      -------    --------    ------------------------
%%% Early history in the bottom of the file
%%% R4A/1      2015-05-26 erarafo     Adapted to change in GMF upgrade
%%% R4A/2      2015-06-11 etxjotj     ECIM SWM 4.0 "commit" action removed
%%% R4A/3      2015-06-11 etxjotj     Compiler error fix
%%% R3A/32     2015-06-04 etxjotj     Log when CXP unmount fails
%%%                                   Set flag when auditing software
%%% R3A/33     2015-06-10 etxjotj     Mount list adoptions
%%% R3A/34     2015-06-11 etxjotj     Synchronous model updates
%%% R4A/7      2015-06-17 etxjotj     HT85211 Better sftp fault printouts
%%% R4A/8      2015-06-22 etxjotj     SFTP error handling improved
%%% R4A/9      2015-06-22 erarafo     Adjustment
%%% R4A/11     2015-07-09 etxjotj     HT91333 Remove upgrade_init_* at confirm
%%% R4A/12     2015-07-20 etxjotj     HT91333 Again + some refactoring 
%%% R4A/13     2015-07-20 etxjotj     Adding consistency check
%%% R4A/14     2015-07-21 etxjotj     Dialyzer fix
%%% R4A/15     2015-07-22 etxjotj     Added root user check in software audit
%%% R4A/16     2015-07-29 etxderb     Temporary changed error_msg=>warning_msg
%%% R4A/17     2015-08-21 etxpejn     Changed logI:write_log to swmLib:write_swm_log
%%% R4A/16     2015-07-29 etxderb     New cup in, warning_msg=>error_msg.
%%% R4A/21     2015-09-04 etxjotj     HU14586 Upgrade marker
%%% R4A/22     2015-09-04 etxjotj     HU14586 Run fsck
%%% R4A/24     2015-09-08 etxjotj     Removed unwanted error printout with info
%%% R4A/25     2015-09-09 etxjotj     HU15807 Always use link solution for fb
%%% R4A/26     2015-09-09 etxjotj     HU14513 Cancel when no op is ongoing
%%% R4A/28     2015-09-15 etxjotj     HU17751 Cancel when waiting for commit
%%% R4A/29     2015-09-15 etxjotj     Fixed revision history
%%% R4A/30     2015-09-21 etxjotj     Action id handling
%%% R4A/31     2015-09-22 etxjotj     HU19769 Use init:reboot at failed upgrade
%%% R4A/32     2015-09-28 etxjotj     HU21335 All cleanup before EE confirm
%%% R4A/33     2015-10-01 erarafo     HU22829 POST_REBOOT_ACTIVATE_TIMEOUT up by 50%
%%% R4A/34     2015-10-06 etxjotj     Improved fault handling at prepare
%%% R4A/38     2015-10-16 etxjotj     HU26187 Changed takedown procedure and
%%%                                   upgrade-init handling
%%% R4A/41     2015-10-15 etxjotj     Improved fault indications
%%% R4A/42     2015-10-16 etxjotj     Bugfix in linking
%%% R4A/44     2015-10-22 etxjotj     Make autobackup at post_activate end
%%% R4A/50     2015-11-23 etxjotj     HU37957 Autobackup at confirm
%%% R5A/1      2016-01-11 etxjotj     HU47600 Improved confirm lock mechanism
%%% R5A/2      2016-01-12 etxjotj     HU49624 Cancel flag remade
%%% R5A/3      2016-02-11 etxpejn     HU53464 Handle cancel on SwM level
%%% R5A/4      2016-02-22 etxjotj     Reduce progress reporting in prepare
%%% R5A/5      2016-02-23 etxjotj     HU59680 Stop subscribing to UP events
%%% R5A/6      2016-02-24 etxjotj     Improved printout if remote sftp close
%%% R5A/7      2016-03-01 etxberb     Going live with Hw/Sw decoupling routines,
%%%                                   using swmBoardList:products/2.
%%% R5A/8      2016-03-09 etxjotj     Remade the download procedure
%%% R5A/10     2016-03-09 etxberb     Improvements of HAL functionality.
%%% R5A/11     2016-03-11 etomist     Added ActionCapability lock/unlock
%%% R5A/12     2016-03-13 erarafo     Removed doubled LTTng trace of 'confirm'
%%% R5A/13     2016-03-14 etomist     Fixed error on upgrade caused by 
%%%                                   ActionCapability lock/unlock
%%% R5A/14     2016-03-15 etomist     Added lock_action_capable_after_upgrade
%%% R5A/15     2016-03-16 etomist     unlock_action_capable moved to before post_activate
%%% R5A/16     2016-03-18 etomist     Fixed audit_software() to work with action capability lock/unlock
%%% R5A/18     2016-03-28 etxberb     * Bug fix in download_up_info/6 at
%%%                                     xmerl_scan..
%%%                                   * Added swmBoardList:read_swp_selected in
%%%                                     dir/2.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% Applications must finalize all ICTI sessions before
%% this many seconds have passed.
-define(POST_REBOOT_ACTIVATE_TIMEOUT, 900).

%% This filename is used by some module-external entity
%% (from the initial install sequence maybe?)
-define(SQUASHFS_TMPDIR_NAME, "tmp").

%% This size is defined to be compliant to most sftp servers out there
-define(CHUNK_SIZE, 32768).

%% action ID used for lock/unlock actionCapable
-define(AUDIT_SW_ACTION_CAPABLE_ID, swmAuditSwActionCapableId).

%% This string defines actionCapableInfo when action is performed
-define (AUDIT_SW_ACTION_CAPABLE_INFO, "The system is performing software audit.").

%% -define(UP_METADATA_FILENAME_PATTERNS, ["*-up.xml", "cxs*.xml"]).

-export([start/0]).
-export([activate/0]).

%% SWM Actions
-export([create_package/2, remove_package/1, prepare_package/1,
     verify_package/1, activate_package/1, %commit_package/1,
     confirm_package/1]).
-export([remove_sw_version/1, remove_software_version/1]).
-export([cancel/1]).

%% Other functions
-export([activation_complete/0]).
-export([update_progress/1]).
%-export([install_on_other/1]).
-export([is_upgrade_ongoing/0]).
-export([open_dets/2]).
-export([ets_new/2]).
-export([get_new_cxp_path/1]).
-export([check_up_states_for_restore/0]).
-export([audit_software/0]).
-export([get_current_archive_dir/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     code_change/3, terminate/2]).

-export([countdown/2]).
-export([send_fallback_alarm/2, fallback/3]).
-export([restart_node/2]).
-export([prepare_progress/2]).

-export([start_test_event_server/0, stop_test_event_server/0]).

-include("RcsSwM.hrl").
-include("RcsSwIM.hrl").
-include("alhI.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("SwmInternal.hrl").

-type fallbackContext() :: 
      activating_before_restart | 
      data_conversion |
      waiting_for_confirm | 
      cancel.

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the swmServer server process
%%% @end
%%% ----------------------------------------------------------

start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Creates an upgrade package MO
%%% Downloading of the cxsNNNN.xml metadata info file or if
%%% present, a complete upgrade package .cxs file.
%%% In the first case the UP will be initialized and a prepare
%%% bust be invoked.
%%% In the second case the UP will go directly to prepare complete
%%% @end
%%% ----------------------------------------------------------
create_package(Uri, Password) ->
    case action_handler(fun() -> create_package_fun(Uri, Password) end) of
    {ok, ActionId} when is_integer(ActionId) ->
        ActionId;
    _ ->
        0
    end.

create_package_fun(Uri, Password) ->
    case actionId_new() of
    ActionId when ActionId /= 0 ->
        case http_uri:parse(Uri) of   % Check Uri format
        {ok, {sftp, [_ | _], [_ | _], Port, [_ | _], _}}
        when is_integer(Port) ->
            %% ======= Normal case here! =======
            ok = gen_server:cast(
               swmServer,{create_package, Uri, Password, ActionId}),
            {ok, ActionId};
        {ok, {file, _, _, _, _, _}} ->
            %% This is for basic testing only
            ok = gen_server:cast(
               swmServer,{create_package, Uri, Password, ActionId}),
            {ok, ActionId};
        {ok, {sftp, User, Host, Port, RemoteDir, _}} ->
            PortStr =
            try integer_to_list(Port)
            catch _ : _ -> make_string(Port) ++ "<-NOT AN INTEGER"
            end,
            ParsingProblem =
            "Missing or faulty value(s):" ++
            " User=" ++ User ++
            " Host=" ++ Host ++
            " Port=" ++ PortStr ++
            " RemoteDir=" ++ RemoteDir,
            ResultInfo = "Uri fault: " ++ ParsingProblem,
            create_package_logFault(ActionId, Uri),
            throw({fail, ResultInfo});
        {ok, _} ->
            create_package_logFault(ActionId, Uri),
            ResultInfo = "Unknown Uri format: " ++ Uri,
            throw({fail, ResultInfo});
        {error, ParsingProblem} ->
            ResultInfo = "Uri fault: " ++ make_string(ParsingProblem),
            create_package_logFault(ActionId, Uri),
            throw({fail, ResultInfo})
        end;
    ActionId ->   % == 0
        ResultInfo = "Too early, start phases not completed.",
        create_package_logFault(ActionId, Uri),
        erlang:error([{?MODULE, create_package}, ResultInfo, {uri, Uri}])
    end.

create_package_logFault(ActionId, Uri) ->
    set_default_progress_report("createUpgradePackage", ActionId),
    swmLib:write_swm_log("SwM", info, "createUpgradePackage " ++ Uri).

%%% ----------------------------------------------------------
%%% @doc Removes the upgrade package from the SWM archive
%%% But not if it is used
%%% @end
%%% ----------------------------------------------------------

remove_package(UpDn) ->
    ok = gen_server:cast(swmServer, {remove_package, UpDn}).

%%% ----------------------------------------------------------
%%% @doc Removes a software version 
%%% Deprected in ECIM SwM 2.1
%%% @end
%%% ----------------------------------------------------------

remove_sw_version(VsnDn) ->
    ok = gen_server:cast(swmServer, {remove_sw_version, VsnDn}).

%%% ----------------------------------------------------------
%%% @doc Removes a software version 
%%% Unmounts the software of the inventory SwVersion if it is still mounted
%%% @end
%%% ----------------------------------------------------------

remove_software_version(VsnDn) ->
    ok = gen_server:cast(swmServer, {remove_software_version, VsnDn}).

%%% ----------------------------------------------------------
%%% @doc Download any *.cxp file specified at the given location
%%% @end
%%% ----------------------------------------------------------

prepare_package(UpKey) ->
    %% HS87440
    case set_up_state(UpKey, prepareInProgress) of
    {ok, _} ->
        ok = gen_server:cast(swmServer, {prepare_package, UpKey});
    {error, {wrong_state, State}} ->
        Msg = "Prepare can not be executed in state " ++ state(State),
        throw({fail, Msg})
    end.

%%% ----------------------------------------------------------
%%% @doc Verify a package
%%% @end
%%% ----------------------------------------------------------
verify_package(UpKey) ->
    %% HS87440
    case get_up_state(UpKey) of
    ?UpgradePackageState_PREPARE_COMPLETED -> 
        ok = gen_server:cast(swmServer, {verify_package, UpKey});
    State ->
        Msg = "Verify can not be executed in state " ++ state(State),
        throw({fail, Msg})
    end.


%%% ----------------------------------------------------------
%%% @doc Unpacks a package onto a the new home partition
%%% Depending on the upgrade type (which should be determined automatically
%%% depending on the contents of the upgrade package) but is now regulated
%%% manually through mnesia.
%%% @end
%%% ----------------------------------------------------------

activate_package(UpKey) ->
    %% HS87440
    case swmFailsafe:get_usage_state() of
    idle ->
        ok;
    _ ->
        FsMsg = "Activate can not be executed when failsafe is active",
        throw({fail, FsMsg})
    end,   
     
    case set_up_state(UpKey, activationInProgress) of
    {ok, _} ->
        sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: activate"),
        ok = gen_server:cast(swmServer, {activate_package, UpKey});
        {error, {wrong_state, State}} ->
        SMsg = "Activate can not be executed in state " ++ state(State),
        throw({fail, SMsg});
    {error, Reason} ->
        throw({fail, Reason}) 
    end.


%%% ----------------------------------------------------------
%%% doc Confirm the upgrade once the activation is finished
%%% This method retains the old non ECIM-compliant asynchronous handling
%%% end
%%% ----------------------------------------------------------
%% commit_package(UpKey) ->
%%     sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: confirm"),
%%     ok = gen_server:cast(swmServer, {commit_package, UpKey}).

%%% ----------------------------------------------------------
%%% @doc Confirm the upgrade once the activation is finished
%%% This method implements the ECIM-compliant synchronous handling
%%% @end
%%% ----------------------------------------------------------
confirm_package(UpKey) ->
    %% HU47600 Improved locking mechanism
    case set_confirm_lock() of
    go ->
        try do_confirm_package(UpKey)
        catch throw:{fail, Reason} ->
            throw({fail, Reason});
          Type:Reason ->
            sysInitI:error_report(
              [{Type, Reason}, {stacktrace, erlang:get_stacktrace()}]),
            erlang:Type(Reason)
        after
        release_confirm_lock()
        end;
    busy ->
        throw({fail, "Confirm is already ongoing"})
    end.

%% HU47600 Improved locking mechanism
%% RAM table is chosen because we want this lock reset at reboot anyway
set_confirm_lock() ->
    Fun = fun() ->
          case swmLib:get_ram_variable(confirm_lock) of
              busy -> 
              busy;
              _ ->
              swmLib:set_ram_variable(confirm_lock, busy),
              go
          end
      end,
    case mnesia:transaction(Fun) of
    {atomic, busy} ->
        busy;
    {atomic, go} ->
        go;
    {aborted, Reason} ->
        erlang:error({aborted, Reason}, [])
    end.

release_confirm_lock() ->
    swmLib:erase_ram_variable(confirm_lock).
    
        


do_confirm_package(UpKey) ->
    Sender = sender(UpKey),
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: confirm"),
    swmLib:write_swm_log(Sender, info, "confirm"),
    [Obj] = mnesia:dirty_read({upgradePackage, UpKey}),
    case Obj#upgradePackage.state of
    ?UpgradePackageState_WAITING_FOR_COMMIT ->
        %% HS87470
        swmLib:erase_ram_variable(activationStarted),
        %% HS67532, part 2 of 2
        %% Handle poweroff during commit
        HeartPath = filename:join(sysEnv:home_dir(), "heart_fallback"),
        CFR = filename:join(sysEnv:home_dir(), "confirm_failed_rollback"),
        case {file:copy(HeartPath, CFR), sysEnv:rcs_mode()} of
        {{ok, _}, _} -> 
            cmd(["chmod a+x ", CFR, " ; sync"]),
            ok;
        {{error, enoent}, simulated} -> ok;
        {{error, Reason}, _} ->
            erlang:error(Reason, [UpKey])
        end,
        NewObj = Obj#upgradePackage{reportProgress = undefined},
        mnesia:dirty_write(NewObj),
        % HS34720
        send_command(UpKey, confirm, "", confirm_failed, Sender),
        % HS34720 end
        ok = gen_server:call(swmServer, {confirm_package, UpKey}, 300000),
        swmLib:erase_variable(activating_up),
        FinalObj = NewObj#upgradePackage{state = state(commitCompleted)},
        mnesia:dirty_write(FinalObj),
        ok = gen_server:cast(
           swmServer, {post_confirm_package,element(4,UpKey)}),
        file:delete(CFR), % HS67532
        ok;
    State ->
        throw({fail, 
           "Confirm can not be executed in state " ++ state(State)})
    end.
    

%%% ----------------------------------------------------------
%% @doc cancel download or activation.
%%% Although you can issue the command elsewhere it will not have any effect.
%%% @end
%%% ----------------------------------------------------------
cancel(UpKey) ->
    Fun = fun() -> reboot_if_cancel(UpKey) end,
    Sender = sender(UpKey),
    case mnesia:transaction(Fun) of
    {atomic, reboot} ->
        info_msg("Activate cancellation ordered. Rebooting...~n"),
        swmLib:write_swm_log(Sender, critical, 
                 "Activate cancellation ordered. Rebooting..."),
        swmLib:sync(),
        restart_node(cold, ?ALH_TAG_UpgradeCancelled);
    {atomic, no_reboot} ->
        info_msg("Activate cancellation ordered.~n"),
        swmLib:write_swm_log(Sender, info, 
                 "Activate cancellation ordered."),
        swmServer!{cancel, UpKey};
    {atomic, not_ongoing} -> %HU14513
        swmLib:write_swm_log(Sender, info, 
                 "Cancel ordered. No ongoing action."),
        ok;
    {aborted, Reason} ->
        erlang:error({aborted, Reason}, [UpKey])
    end.

reboot_if_cancel({"1","1","1"}) ->
    %% TODO Find out if any action is ongoing on any UP
    not_ongoing;
reboot_if_cancel(UpKey) ->
    [UP] =  mnesia:read({upgradePackage, UpKey}),
    Progress = UP#upgradePackage.reportProgress,
    %% HU14513 Handle cancel when no ongoing action
    case Progress of 
    undefined -> 
        not_ongoing;
    Progress ->
        #'AsyncActionProgressWithSteps'{actionName = ActionName,
                        state = State}  = Progress,
        case {ActionName, State} of
        {"activate", ?ActionStateType_RUNNING} ->
            do_update_progress(
              UpKey, [{state, ?ActionStateType_CANCELLING}]),
            %% HU49624 Cancel flag fix
            case swmLib:get_ram_variable(activate_cancel) of
            reboot -> reboot;
            _ -> no_reboot
            end;
        %% HU17751
        {_, ?ActionStateType_FINISHED} 
          when UP#upgradePackage.state == 
               ?UpgradePackageState_WAITING_FOR_COMMIT ->
            reboot;
        {_, ?ActionStateType_FINISHED} ->
            not_ongoing;
        {_, ?ActionStateType_CANCELLED} ->
            not_ongoing;
        _ -> 
            no_reboot
        end
    end.

%%% ----------------------------------------------------------
%%% @doc Automatically called after the activation
%%% At the end of the activation this function must be called to
%%% complete the upgrade
%%% This function is called by gmfImmUgMaster when the data conversion phase
%%% is complete
%%% @end
%%% ----------------------------------------------------------

activation_complete() ->
    info_msg("Got activation_complete~n"),
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: activate complete"),
    ok = gen_server:cast(swmServer, activation_complete).

%%% ----------------------------------------------------------
%%% @doc Called after complete startup
%%% This function is called by sysApp after a complete startup.
%%% @end
%%% ----------------------------------------------------------

activate() ->
    case swmLib:get_variable(ug_failed) of
    undefined ->
        case is_upgrade_ongoing() of
        true ->
            lock_action_capable_after_upgrade(),
            ok;
        false ->
            %% Normal startup
            unlock_action_capable_after_restart(),
            clear_all_progress_info()
        end;
    BuName ->
        swmLib:erase_variable(ug_failed),
        report_on_reboot(BuName)
    end,
    ok.

%%% ----------------------------------------------------------
%%% Description: This function is entered at system startup if it is
%%%              detected that the rollback backup has been loaded
%%% ----------------------------------------------------------

report_on_reboot(BuName) ->
    info_msg("Detecting restore of rollback backup ~p~n",[BuName]),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    case swmLib:get_ram_variable(bu_progress_report_exists) of
    true ->
        %% This means there was a deliberate restore ordered
        %% No need to update UP progress, only BrM progress
        clear_all_progress_info();
    false -> 
        %% This means any ongoing upgrade has failed
        %% Restore the links and set boot counter to 0
        unlock_action_capable_after_restart(),
        case sysEnv:board_type() of
        tcu03 ->
            sysRhai:setbootcounter(0),
            Other = case swmOs:get_active_instance() of
                   1 -> "2";
                   2 -> "1"
               end,
            swmOs:run_swm_wrapper("activate_ee_tcu03 -i "++Other);
        _ ->
            ok
        end,
 
        [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
        report_failure_restart(SwM#swM.reportProgress, CompleteTime),
        [report_failure_restart(Up#upgradePackage.upgradePackageId,
                    Up#upgradePackage.reportProgress,
                    CompleteTime)||
        Up<-ets:tab2list(upgradePackage)];
    undefined ->
        %% Can happen if there is a power cycle immediately after 
        %% activate, and perhaps other cases
        unlock_action_capable_after_restart(),
        clear_all_progress_info()
    end,
    swmLib:erase_ram_variable(bu_progress_report_exists).

clear_all_progress_info() ->
    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
    AAP = #'AsyncActionProgress'
    {actionName="",
     additionalInfo=[""],
     progressInfo = "Default message",
     progressPercentage=100,
     result=?ActionResultType_NOT_AVAILABLE,
     resultInfo="",
     state=?ActionStateType_FINISHED,
     actionId=0,
     timeActionStarted = "",
     timeActionCompleted= "",
     timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)},
    mnesia:dirty_write(SwM#swM{reportProgress=AAP}),
    AAPwS = #'AsyncActionProgressWithSteps'
    {actionName="",
     additionalInfo=[""],
     progressInfo = "Default message",
     progressPercentage=100,
     result=?ActionResultType_NOT_AVAILABLE,
     resultInfo="",
     state=?ActionStateType_FINISHED,
     actionId=0,
     timeActionStarted = "",
     timeActionCompleted= "",
     timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended),
     step=-1,
     stepProgressPercentage=100},
    [mnesia:dirty_write(UP#upgradePackage{reportProgress=AAPwS})||
    UP<-ets:tab2list(upgradePackage)].


    

report_failure_restart(undefined, _) ->
    ok;
report_failure_restart(Progress, CompleteTime) 
  when is_record(Progress, 'AsyncActionProgress')->
    case Progress#'AsyncActionProgress'.state of
    ?ActionStateType_RUNNING ->
        swmFallbackList:cancel_pending(), %HS51856
        Msg = "The action was interrupted by a restart",
        Info = "More info may be available in SwmLog",
        update_progress([{result, ?ActionResultType_FAILURE},
                 {additionalInfo, Info},
                 {progressPercentage, 100},
                 {resultInfo, Msg},
                 {state, ?ActionStateType_FINISHED},
                 {timeActionCompleted, CompleteTime}]);
    _ ->
        ok
    end.

report_failure_restart(_, undefined, _) ->
    ok;
report_failure_restart(Key, Progress, CompleteTime) 
  when is_record(Progress, 'AsyncActionProgressWithSteps')->
    case Progress#'AsyncActionProgressWithSteps'.state of
    ?ActionStateType_RUNNING ->
        swmFallbackList:cancel_pending(), %HS51856
        Msg = "The action was interrupted by a restart",
        update_progress(Key, [{result, ?ActionResultType_FAILURE},
                  {additionalInfo, Msg},
                  {progressPercentage, 100},
                  {resultInfo, Msg},
                  {state, ?ActionStateType_FINISHED},
                  {timeActionCompleted, CompleteTime}]);
    _ ->
        ok
    end.


%%% ----------------------------------------------------------
%%% @doc Called after complete startup
%%% This function is called by sysApp after a complete startup.
%%% @end
%%% ----------------------------------------------------------

is_upgrade_ongoing() ->
    filelib:is_file(filename:join(sysEnv:home_dir(), "upgrade_init")).

%%% ----------------------------------------------------------
%%% @doc Audit software
%%% Unmounts all software that is not part of the running UP
%%% @end
%%% ----------------------------------------------------------

audit_software() ->
    case swmLib:lock_action_capable(?AUDIT_SW_ACTION_CAPABLE_ID, 
                                    ?AUDIT_SW_ACTION_CAPABLE_INFO) of
    ok ->
        try do_audit_software()
        after
          ok = swmLib:unlock_action_capable(?AUDIT_SW_ACTION_CAPABLE_ID)
        end;
    _ ->
        %% Silently fails to execute
        ok
    end.

do_audit_software() ->
    Installed = swmOs:get_installed_cxps(),
    {ok, Software} = file:list_dir(swmLib:software_dir()),
    Remove = Installed--Software,
    info_msg("Auditing software~nThe following will be removed: ~p~n",[Remove]),
    do_audit_software(Remove).

do_audit_software([Cxp|RemoveList]) ->
    case swmOs:remove_cxp(Cxp) of
    ok -> ok;
    error ->
        swmLib:write_swm_log("SwM", alert, 
                 "Software audit failed for "++Cxp)
    end,
    do_audit_software(RemoveList);
do_audit_software([]) ->
    audit_root_users(),
    info_msg("Software audit complete~n"),
    swmLib:write_swm_log("SwM", info, "Software audit complete").
  
%% Addition to pinpoint problems reported in HT94058
audit_root_users() ->
    SquashFsDir = swmLib:squash_fs_dir(),
    {ok, Files} = file:list_dir(SquashFsDir),
    audit_root_users(Files).    
    
audit_root_users([File|Files]) ->
    SquashFsDir = swmLib:squash_fs_dir(),
    Path = filename:join(SquashFsDir, File),
    case file:read_file_info(Path) of
    {ok, FIO} ->
        case FIO#file_info.uid of
        0 -> 
            error_msg("Owner root on at least one item in /software~n"
                  "~s~n", [os:cmd(["ls -l ", SquashFsDir])]),
            swmLib:write_swm_log("SwM", alert, 
                     "Software audit failed for "++File);
        _ -> 
            audit_root_users(Files)
        end;
    {error, _} ->
        audit_root_users(Files)
    end;
audit_root_users([]) ->
    ok.

    

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
-record(state, 
    {state              :: undefined|activationInProgress|waitingForCommit,
     fallbackTimer,     % TimerRef
     alarmTimer,        % TimerRef
     countdownTimer     % TimerRef
        }).

init(Args) ->
    ok = gen_server:cast(self(), {initialize, Args}),
    {ok, initializing}.

initialize(_Args) ->
    actionId_init(),

    %% This part is to set the env which is normally done by APPM for all
    %% other applications
    {_, CxpProdNr, CxpRev} = swmLib:get_cxp_source(code:lib_dir(swm)),
    os:putenv("CXP_NO", CxpProdNr),
    os:putenv("CXP_REV", CxpRev),

    mnesia:subscribe({table, swVersion, detailed}),

    cmd(["df ",sysEnv:home_dir()]),
    info_msg("Active instance is ~w~n",[swmOs:get_active_instance()]),

    DataDir = swmLib:data_dir(),
    SwFile = filename:join(DataDir, "swdata.dets"),
    filelib:ensure_dir(SwFile),

    open_dets(swVersion, [{file, SwFile}, {keypos, 2}]),

    swmModel:update_tables(),
    swmInventory:update_tables(),

    %% HS67532, part 1 of 2
    CFR = filename:join(sysEnv:home_dir(), "confirm_failed_rollback"),
    case filelib:is_file(CFR) of
    true -> 
        error_msg("Confirm failed. Rolling back.~n"),
        swmLib:write_swm_log("SwM",info,"Confirm failed. Rolling back"),
        cmd(CFR);
    false -> 
        ok
    end,
    %% HS67532 fix ends here
    case swmLib:get_variable(bu_restore) of
    undefined ->
        info_msg("This is not a backup fallback restart~n"),
        case is_upgrade_ongoing() of
        true ->
            info_msg("upgrade is ongoing~n"),
            UP = swmLib:get_variable(activating_up),
            Key = UP#upgradePackage.upgradePackageId,
            update_progress(
              Key, [{additionalInfo, "Converting database."},
                {progressPercentage, 76}]),
            
            %% This is to cause a fallback before data conversions
            %% are ready if that task hangs
            ok = gen_server:cast(swmServer, set_global_fallback),

            %% If there is a heart_fallback file, then this is
            %% a reboot upgrade. 
            Heart = filename:join(sysEnv:home_dir(), "heart_fallback"),
            case filelib:is_file(Heart) of
            true ->
                info_msg("Setting heart fallback cmd~n"),
                heart:set_cmd(Heart),
                %% HS87470
                swmLib:set_ram_variable(activationStarted, Key);
            false ->
                ok
            end,
            {noreply, #state{state = activationInProgress}};
        false ->
            info_msg("upgrade is not ongoing~n"),
            %% HU29568 upgrade marker removed
            do_audit_software(),
            audit_progress_state(),%% HS27033
            swmLib:erase_ram_variable(activationStarted),%%HT18288
            {noreply, #state{}}
            %% case is_upgrade_marker() of
            %%  true ->
            %%      Listing = os:cmd(["ls -la ", sysEnv:home_dir()]),
            %%      error_msg("Upgrade marker is present. "
            %%            "This is a failed upgrade.~n~s~n",
            %%            [Listing]),
            %%      erase_upgrade_marker(),
            %%      %% HU19769 Don't use APPM for reboot in this case
            %%      swmLib:sync(),
            %%      %% HU26178 Don't use init:reboot either
            %%      info_msg("Halting system~n"),
            %%      Heart = "pgh_restart_board -e -r "
            %%      "'Failed upgrade: Missing up file'",
            %%      heart:set_cmd(Heart),
            %%      erlang:halt(),
            %%      {stop, upgrade_maker_fail, #state{}};
            
            %%  false -> 
            %%      %% HU26178 Only do cleanup if ok
            %%      info_msg("Normal startup, starting audit~n"),
            %%      audit_software(),
            %%      audit_progress_state(),%% HS27033
            %%      swmLib:erase_ram_variable(activationStarted),%%HT18288
            %%      {noreply, #state{}}
            %% end
        end;
    BuRestore ->
        info_msg("bu_restore = ~p~n",[BuRestore]),
        do_audit_software(),
        swmLib:set_ram_variable(bu_progress_report_exists, 
                    swmBackup:is_progress_report()),
        swmLib:erase_ram_variable(activationStarted),
        Heart = filename:join(sysEnv:home_dir(), "heart_fallback"),
        case filelib:is_file(Heart) of
        true -> 
            file:delete(Heart);
        false ->
            ok
        end,
        {noreply, #state{}}
    end.

%%% ----------------------------------------------------------
handle_call({confirm_package, UpKey}, _, State) ->
    cancel_timer(State#state.fallbackTimer),
    cancel_timer(State#state.alarmTimer),
    cancel_timer(State#state.countdownTimer),

    [ets:delete(OldTab)||{_, OldTab} <- ets:tab2list(olddb)],
    ets:delete(olddb),

    {_, _, _, Up} = UpKey,
    Dn = [<<"ManagedElement=1">>, 
      <<"SystemFunctions=1">>,
      <<"SwM=1">>,
      list_to_binary("UpgradePackage="++Up)],
    comsaI:clear_alarm('FallbackOperationStartingSoon', Dn),

    %% HU21335
    %% All cleaning up must be made before confirming to EE
    %% HU29568 
    %% New upgrade_init handling
    %% Db file no longer stored in /rcs/swm but retain this cleaning
    %% for backwards compatiblity

    cmd(["rm -f ",filename:join(swmLib:swm_dir(), "upgrade_init*")]),
    cmd(["rm -f ",filename:join(sysEnv:home_dir(), "upgrade_init*")]),
    cmd(["rm -rf ",swmLib:upgrade_prep_dir()]),
    update_count(-1),
    swmDbMonitor:force_auto_backup(), % HU37957
    swmOs:commit(),

    {reply, ok, State#state{countdownTimer = undefined,
                fallbackTimer = undefined,
                alarmTimer = undefined,
                state = undefined}};
handle_call(_Request, _From, State) ->
    {reply, undefined, State}.


%%% ----------------------------------------------------------
%%% Description: This is to avoid blocking other applications
%%% ----------------------------------------------------------

handle_cast({initialize, Args}, initializing) ->
    initialize(Args); 

 
%%% ----------------------------------------------------------
%%% Description: This is called to start a timer before it has
%%% been activation.
%%% ----------------------------------------------------------

handle_cast(set_global_fallback, State) ->
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,
    WaitTime = case swmLib:get_variable(swm_test_rollback_timer) of
           undefined ->
               info_msg(
             "Enable data conversion timeout in ~w seconds~n",
             [?POST_REBOOT_ACTIVATE_TIMEOUT]),
               ?POST_REBOOT_ACTIVATE_TIMEOUT*1000;
           WT ->
               warning_msg(
             "Data conversion timeout in ~w seconds (lab config)~n",
             [WT/1000]),
               WT
           end,
    {ok, FallbackTimer} =
    timer:apply_after(WaitTime,
              swmServer,
              fallback,
              [Key, 
               data_conversion,
               "Timeout before completion of data conversion"]),
    {noreply, State#state{fallbackTimer=FallbackTimer}};

handle_cast(audit_software, State) ->
    case is_upgrade_ongoing() of
    true ->  % no audit during upgrade
        ok;
    false ->
        audit_software()
    end,
    {noreply, State};

handle_cast({create_package, Uri, Password, Id}, State) ->
    set_default_progress_report("createUpgradePackage", Id),
    action_handler(fun() -> handle_create_package(Id, Uri, Password) end),
    ok = swmLib:unlock_action_capable(?CREATE_UP_ACTION_CAPABLE_ID),
    {noreply, State};
handle_cast({remove_package, UpDn}, State) ->
    Id = actionId_new(),
    set_default_progress_report("removeUpgradePackage", Id),
    action_handler(fun() -> handle_remove_package(UpDn) end),
    ok = swmLib:unlock_action_capable(?REMOVE_UP_ACTION_CAPABLE_ID),
    {noreply, State};


handle_cast({remove_sw_version, VsnDn}, State) ->
    Id = actionId_new(),
    set_default_progress_report("removeSwVersion", Id),
    action_handler(fun() -> handle_remove_sw_version(VsnDn) end),
    {noreply, State};

handle_cast({remove_software_version, VsnDn}, State) ->
    Id = actionId_new(),
    set_default_progress_report("removeSoftwareVersion", Id),
    action_handler(fun() -> handle_remove_software_version(VsnDn) end),
    {noreply, State};

handle_cast({prepare_package, UpKey}, State) ->
    Id = actionId_new(),
    set_default_up_progress_report(UpKey, "prepare", Id),
    action_handler(fun() -> handle_prepare_package(UpKey) end, UpKey),
    ok = swmLib:unlock_action_capable(?PREPARE_UP_ACTION_CAPABLE_ID),
    {noreply, State};

handle_cast({verify_package, UpKey}, State) ->
    Id = actionId_new(),
    set_default_up_progress_report(UpKey, "verify", Id),
    action_handler(fun() -> handle_verify_package(UpKey) end, UpKey),
    ok = swmLib:unlock_action_capable(?VERIFY_UP_ACTION_CAPABLE_ID),
    {noreply, State};

handle_cast({activate_package, UpKey}, State) ->
    Id = actionId_new(),
    set_default_up_progress_report(UpKey, "activate", Id),
    Result=action_handler(fun() -> handle_activate_package(UpKey) end,UpKey),
    case Result of
    {ok, activationInProgress} ->
        {noreply, State#state{state=activationInProgress}};
    _ ->
        {noreply, State}
    end;
handle_cast(activation_complete, State) 
  when State#state.state==activationInProgress ->
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,
    update_progress(
      Key, [{additionalInfo, "Database conversion complete"},
        {progressPercentage, 80}]),
    %% Cancelling the post reboot activate timer
    cancel_timer(State#state.fallbackTimer),

    %% This is obviously a fake, we cannot know when the applications
    %% are started. We should get a ping from APPM or some way to monitor
    %% the startup
    timer:sleep(60000),
    update_progress(
      Key, [{additionalInfo, "System startup complete"},
        {progressPercentage, 95}]),
    
    case handle_activation_complete() of
        {ok, {CountdownTimer, FallbackTimer, AlarmTimer}} ->
                        
            {noreply, State#state{state = waitingForCommit,
                      fallbackTimer = FallbackTimer,
                      alarmTimer = AlarmTimer,
                      countdownTimer = CountdownTimer}};
        Error ->
            {stop, Error, State}
     end;
%% handle_cast({commit_package, UpKey}, State) ->
%%     cancel_timer(State#state.fallbackTimer),
%%     cancel_timer(State#state.alarmTimer),
%%     cancel_timer(State#state.countdownTimer),
%%     Id = actionId_new(),
%%     set_default_up_progress_report(UpKey, "confirm", Id),
%%     action_handler(fun() -> handle_commit_package(UpKey) end, UpKey),
%%     {noreply, State#state{countdownTimer=undefined,
%%            fallbackTimer=undefined,
%%            alarmTimer=undefined,
%%            state=undefined}}; %HS76792

handle_cast({post_confirm_package, _}, State) ->
    swmFallbackList:add_pending(),
    [UP] = swmInventory:get_current_sw_version(),
    %% HT47248 fix
    UpStr = 
    UP#'ProductData'.productName++"_"++
    format_product_number(UP#'ProductData'.productNumber)++"_"++
    UP#'ProductData'.productRevision,
    BuName = "Final_backup_for_"++UpStr++"_"++
    comsaI:iso_time(os:timestamp(), basic),
    %% Fix ends here
    ManagerKey = {"1","1","1","1"},
    Id = swmLib:get_new_action_id(mgr),
    swmBackup:set_default_progress_report(mgr, "CREATE", Id),
    create_backup(list_to_binary(BuName), ManagerKey, system, mgr),
    swmFallbackList:add_backup(after_ug, BuName),
    swmFallbackList:add_backup(latest, BuName),
    ok = swmLib:unlock_action_capable(?CONFIRM_UP_ACTION_CAPABLE_ID),
    audit_software(),
    {noreply, State};

handle_cast(Request, State) ->
    error_msg("Unknown request: ~p~nState = ~p~n",[Request, State]),
    {noreply, State}.
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------

handle_info({cancel, _UpKey}, State)
  when State#state.state == activationInProgress ->
    info_msg("'cancel' not supported while 'activate' in progress~n"),
    {noreply, State};

%% HS37544 solution: For the time being do not support 'cancel' at
%% all while 'activate' is in progress; neither before nor after the
%% restart. Note that COM is not enabled anyway before the state
%% changes to 'waitingForCommit'.

%% handle_info({cancel, UpKey}, State) 
%%   when State#state.state == activationInProgress ->
%% %%% FIXME! Find out if the UpKey matches the ongoing activation!!! jotj
%%     [Obj] = mnesia:dirty_read({upgradePackage, UpKey}),
%%     mnesia:dirty_write(Obj#upgradePackage{state = state(prepareCompleted)}),
%% 
%%     case swmLib:get_variable(set_unpacked) of
%%  Vsn when Vsn /= undefined->
%%      release_handler:remove_release(Vsn),
%%      swmLib:erase_variable(set_unpacked);
%%  undefined ->
%%      ok
%%     end,
%%     cmd("rm -rf "++sysEnv:home_dir_other()),
%%     swmInventory:update_tables(),
%% 
%%     CompleteTime = comsaI:iso_time(os:timestamp(), extended),
%%     update_progress([{result, ?ActionResultType_FAILURE},
%%           {state, ?ActionStateType_CANCELLED},
%%                   {progressPercentage, 100},
%%           {timeActionCompleted, CompleteTime}]),
%%     {noreply, State#state{state=undefined}};
handle_info({cancel, Key}, State) when State#state.state==waitingForCommit ->
    case global:set_lock({busy, cancel}, [node()], 0) of
    true ->
        case swmLib:get_ram_variable(activationStarted) of
        Key ->
            mnesia:transaction(fun() -> swmLib:erase_ram_variable(activationStarted) end);
        _Else ->
            do_nada
        end,
        try handle_cancel_after_activate(Key) of
        busy ->
            {noreply, State};
        _ -> 
            {noreply, State#state{state=undefined}}
        catch Type:Reason ->
            sysInitI:error_report(
              [{Type, Reason}, {stacktrace, erlang:get_stacktrace()}]),
            {noreply, State}
        after 
        global:del_lock({busy, cancel})
        end;
    false -> 
        {noreply, State}
    end;
handle_info({cancel, _}, State) ->
    %% HS44329 Cancel was performed out of order
    {noreply, State};
handle_info({mnesia_table_event, Event}, State) -> 
    try handle_mnesia_table_event(Event)
    catch Type:Error ->
            sysInitI:error_report(
              [{mfa, {?MODULE, handle_mnesia_table_event, [Event]}},
               {Type, Error},
           {stacktrace, erlang:get_stacktrace()}])
    end,
    {noreply, State};
handle_info({'ETS-TRANSFER' = Msg, TabId, FromPid, HeirData}, State) ->
    sysInitI:info_report([{?MODULE, handle_info, Msg},
                  {tabId, TabId},
                  {fromPid, FromPid} | format_HeirData(HeirData)]),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.



        
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
terminate(_Reason, _State) ->
    dets:close(upgradePackage),
    dets:close(swVersion),
    ok.

format_HeirData(List) when is_list(List) ->
    List;
format_HeirData(Other) ->
    [Other].

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

%% HU30730
%% Release housekeeping lock after creating a system created backup
create_backup(Name, BrmBackupManagerKey, Type=system, Progress) ->
    MoRef = swmBackup:create_backup_common(Name, BrmBackupManagerKey,
                       Type, Progress),
    Index = lists:last(string:tokens(MoRef, "=")),
    swmLib:unlock_backup(Index),
    MoRef.


%%% ----------------------------------------------------------
%%% #           audit_progress_state()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: This function is called if swmServer is restarted for any
%%%              other than an ongoing activation, whereby the progress is
%%%              set to a final state
%%% ----------------------------------------------------------

%% HS27033
audit_progress_state() ->
    {atomic, [SwM]} =
    mnesia:transaction(fun() -> mnesia:read(swM, {"1","1","1"}) end),
    case SwM#swM.reportProgress of
    undefined -> 
        ok;
    Progress ->
        case Progress#'AsyncActionProgress'.state of
        ?ActionStateType_RUNNING ->
            swmFallbackList:cancel_pending(), % HS51856
            CompleteTime =
            comsaI:iso_time(os:timestamp(), extended),
            update_progress(
              [{result, ?ActionResultType_FAILURE},
               {progressPercentage, 100},
               {state, ?ActionStateType_FINISHED},
               {resultInfo, "The action was interrupted by a restart"},
               {timeActionCompleted, CompleteTime}]);
        ?ActionStateType_CANCELLING ->
            swmFallbackList:cancel_pending(), %HS51856
            %% Since the swmServer has restarted any
            %% cancellation would have been handled by
            %% initialization cleanup, hence sucess
            CompleteTime =
            comsaI:iso_time(os:timestamp(), extended),
            %% HS76784
            update_progress([{result, ?ActionResultType_FAILURE},
                     {state, ?ActionStateType_CANCELLED},
                     {progressPercentage, 100},
                     {timeActionCompleted, CompleteTime}]),
            ok;
        _ ->
            %% Check reportProgress for all UPs
            {atomic, UPKey} =
            mnesia:transaction(fun() -> mnesia:first(upgradePackage) end),
            audit_progress_state_ups(UPKey),
            ok
        end
    end.

%% TODO, Johan/Rabbe, behover SWM nivan kollas eller racker det med UP?
audit_progress_state_ups('$end_of_table') ->
    %% No more Ups to search
    ok;
audit_progress_state_ups(UPKey) ->
    {atomic, [UP]} =
    mnesia:transaction(fun() -> mnesia:read(upgradePackage, UPKey) end),
    case UP#upgradePackage.reportProgress of
        undefined -> 
        ok;
        Progress ->
        case Progress#'AsyncActionProgressWithSteps'.state of
            ?ActionStateType_RUNNING ->
            swmFallbackList:cancel_pending(), % HS51856
                CompleteTime =
                comsaI:iso_time(os:timestamp(), extended),
                update_progress(UPKey,
                        [{result, ?ActionResultType_FAILURE},
                     {progressPercentage, 100},
                         {state, ?ActionStateType_FINISHED},
                         {resultInfo, "The action was interrupted by a restart"},
                         {timeActionCompleted, CompleteTime}]);
            ?ActionStateType_CANCELLING ->
            swmFallbackList:cancel_pending(), %HS51856
                %% Since the swmServer has restarted any
                %% cancellation would have been handled by
                %% initialization cleanup, hence sucess
                CompleteTime =
                comsaI:iso_time(os:timestamp(), extended),
            %% HS76784
                update_progress(UPKey, [{result, ?ActionResultType_FAILURE},
                            {state, ?ActionStateType_CANCELLED},
                        {progressPercentage, 100},
                            {timeActionCompleted, CompleteTime}]),
                ok;
            _ ->
                ok
            end
    end,
    {atomic, NextUPKey} =
    mnesia:transaction(fun() -> mnesia:next(upgradePackage, UPKey) end),
    audit_progress_state_ups(NextUPKey).

%%% ----------------------------------------------------------
%%% #           handle_create_package(ActionId, Uri, Password)
%%% Input: ActionId:integer() - Used in progress reporting
%%%        Uri:string() - Location of the UP
%%%        Password:string() - Password to the service
%%% Output:
%%% Exceptions:
%%% Description: Open sftp and download either the metadata file or entire
%%%              upgrade package container. The given URI is expected to be
%%%              of type sftp, specifying an sftp user and a server directory,
%%%              holding the UP metadata file and the CXP files.
%%% ----------------------------------------------------------

-spec handle_create_package(non_neg_integer(), string(), string()) -> ok.

handle_create_package(ActionId, Uri, Password) ->
    swmLib:write_swm_log("SwM", info, "createUpgradePackage "++Uri),
    update_progress([{additionalInfoClear, "createUpgradePackage commenced"}]),
    case http_uri:parse(Uri) of
    {error, ParsingProblem} ->
        throw({fail, "Uri fault: " ++ make_string(ParsingProblem)});
    {ok, {sftp, User, Host, Port, RemoteDir, _Query}} ->
        {ok, Pid, _CRef} = 
        start_channel(Host, Port, User, Password),
        {ok, FIO} = 
        case ssh_sftp:read_file_info(Pid, RemoteDir, 10000) of
            {error, no_such_file} ->
            update_progress([{additionalInfo,
                      "No such file: " ++ RemoteDir}]),
            throw({fail, "No such file: "++RemoteDir});
            {ok, FIO1} ->  {ok, FIO1}
        end,
        {SourceDir, Listing} =
        case FIO#file_info.type of
            directory ->
            {ok, BaseNames} = sftp_list_dir(Pid, RemoteDir, 10000), % HT85211
            {RemoteDir, BaseNames};
            regular ->
            {filename:dirname(RemoteDir),
             [filename:basename(RemoteDir)]}
        end,
        info_msg("Files found at remote dir: ~p~n",[Listing]),
        DN = download_up_info(ActionId, Pid, SourceDir, Uri, Password, 
                  Listing),
        swmLib:write_swm_log("SwM", info, "Action create complete"),

        CompleteTime = comsaI:iso_time(os:timestamp(), extended),

        update_progress([{additionalInfo,
                  "createUpgradePackage complete"},
                 {timeActionCompleted, CompleteTime},
                 {progressPercentage, 100},
                 {result, ?ActionResultType_SUCCESS},
                 {resultInfo, DN},
                 {state, ?ActionStateType_FINISHED}]),

        ok;
    {ok, {file, _, "localhost", _, RemoteDir, _}} ->
        %% This clause is for testing purposes only, and assumes a 
        %% correctly formatted up
        File = filename:basename(RemoteDir),
        {ok, Data} = file:read_file(RemoteDir),
        {Xml, _} = xmerl_scan:string(binary_to_list(Data)),
        ProductData = extract_product_data(Xml),
        ArchivePath = get_up_archive_dir(ProductData),
        DN = 
        case filelib:is_dir(ArchivePath) of
            true ->
            #'ProductData'{productName = Name,
                       productNumber = Id,
                       productRevision = Vsn} = ProductData,
            Msg = "UP "++Name++" ("++Id++" "++Vsn++
                ") already exists",
            throw({fail, Msg});
            false ->
            create_up(ActionId, File, Uri, Password, 
                  ProductData, ArchivePath, Data)
        end,

        swmLib:write_swm_log("SwM", info, "Action create complete"),
        CompleteTime = comsaI:iso_time(os:timestamp(), extended),

        update_progress([{additionalInfo,
                  "createUpgradePackage complete"},
                 {timeActionCompleted, CompleteTime},
                 {progressPercentage, 100},
                 {result, ?ActionResultType_SUCCESS},
                 {resultInfo, DN},
                 {state, ?ActionStateType_FINISHED}]),

        ok
    end.

%%% ----------------------------------------------------------
%%% #         download_up_info(ActionId, pid, SourceDir, Uri, Password, Files)
%%% Input: ActionId:integer() - Used in progress reporting
%%%        Pid:pid() - Ssh channel server
%%%        SourceDir:string() - Location of the up, one up only in each dir
%%%        Uri:string() - Compelte location of the up to be stored in MO
%%%        Password:string() - Passsword to be saved in MO
%%%        Files:[string()] - List of cxp files, or one up container file
%%% Output: DN:string()
%%% Exceptions:
%%% Description: Download up metadata and create UpgradePackage MO
%%%              Metadata is an xml file with recognizable format
%%%              Should there be a file named .cxs assume its a up container
%%%              and download the whole thing
%%% ----------------------------------------------------------

-spec download_up_info(non_neg_integer(), 
               pid(), 
               string(), 
               string(), 
               string(), 
               [string()]) -> string().

download_up_info(ActionId, Pid, SourceDir, Uri, Password, [File|Files]) ->
    Path = filename:join(SourceDir, File),
    case string:str(lists:reverse(File), "lmx.pu-") of
    1 ->
        %% Match towards the reversed file to avoid that a -up.xml~ is picked
        %% Find out if this is a up metadata file
        {ok, Data} = case ssh_sftp:read_file(Pid, Path) of
                 {ok, D} -> {ok, D};
                 {error, no_such_file} ->
                 throw({fail, "No such file: "++Path});
                 {error, permission_denied} -> 
                 throw({fail, "Permission denied: "++Path});
                 {error, ReadFileReason} ->
                 EMsg = "SFTP error: "++
                     make_string_flat(ReadFileReason)++
                     " for "++Path,
                 throw({fail, EMsg})
             end,
        {Xml, []} = xmerl_scan:string(binary_to_list(Data)),
        case check_up_metadata(Xml) of
        ok ->
            ProductData = extract_product_data(Xml),
            ArchivePath = get_up_archive_dir(ProductData),
            case filelib:is_dir(ArchivePath) of
            true ->
                #'ProductData'{productName = Name,
                       productNumber = Id,
                       productRevision = Vsn} = ProductData,
                Msg = "UP "++Name++" ("++Id++" "++Vsn++
                ") already exists",
                throw({fail, Msg});
            false ->
                create_up(ActionId, File, Uri, Password, 
                      ProductData, ArchivePath, Data)
            end;
        {error, Reason} ->
            
            Msg = lists:flatten(io_lib:format(
              "Ill-formed UP metadata file: ~s, ~s",
              [File, Reason])),
            warning_msg("~s~n",[Msg]),
            swmLib:write_swm_log("SwM", error, Msg),
            download_up_info(
              ActionId, Pid, SourceDir, Uri, Password, Files)
        end;
    _Else -> % Filename does not contain "-up.xml"
        download_up_info(ActionId, Pid, SourceDir, Uri, Password, Files)
    end;
download_up_info(_, _, _, _, _, []) ->
%    update_progress([{resultInfo, "Could not find an upgrade package"}]),
    throw({fail, "Could not find an upgrade package"}).

%%% ----------------------------------------------------------
%%% @doc Checks that the given XML element appears to be that
%%% of a well-formed UP metadata file.
%%% ----------------------------------------------------------

-spec check_up_metadata(#xmlElement{}) -> ok | {error, string()}.

check_up_metadata(Xml) ->
    case Xml#xmlElement.name of
    configuration ->
        try find_attribute(type, Xml) of
        "MSRBS-UP" ->
            ok;
        Other ->
            {error,
             lists:flatten(
               io_lib:format(
             "bad value of 'type' attribute: ~s", [Other]))}
        catch
        _:_ ->
            % Allowing metadata when type="MSRBS-UP" is missing;
            % this is not documented in the UP IWD and so can
            % be considered deprecated
            try find_element(product, Xml) of
            UpProdE ->
                try find_attribute(id, UpProdE) of
                Id ->
                    case Id of
                    [$C, $X, $S|_] ->
                        ok;
                    _ ->
                        {error, "product id not 'CXS*'"}
                    end
                catch
                _:_ ->
                    {error, "missing 'id' attribute in 'product'"}
                end
            catch
            _:_ ->
                {error, "missing 'product' element"}
            end
        end;
    _ ->
        {error, "missing 'configuration' element"}
    end.
%%% ----------------------------------------------------------
%%% @doc Create a new UP. It is trusted that the new UP does not
%%% clash with existing ones (its name+id+version is unique).
%%% @end
%%% ----------------------------------------------------------

-spec create_up(ActionId::non_neg_integer(), 
        MetaFile::string(), 
        Uri::string(), 
        Password::string(), 
%               ConfigurationE::#xmlElement{},
        Productdata::#'ProductData'{},
        ArchivePath::string(), 
        Data::binary()) -> string().

create_up(ActionId, MetaFile, Uri, Password, 
      %ConfigurationE, 
      ProductData, 
      ArchivePath, 
      Data) ->
    Msg = "Reading UP data",
    update_progress([{additionalInfo, Msg }]),
    EnumState = state(initialized),
    EcimPassword = #'EcimPassword'{
              password =
              comsaI:encrypt_password(list_to_binary(Password))},
%    ProductData = extract_product_data(ConfigurationE),
    Key = swmInventory:make_mom_key(ProductData#'ProductData'.productNumber,
                    ProductData#'ProductData'.productRevision),
    NewIndex = {"1","1","1", Key},
    CreateTime = comsaI:iso_time(os:timestamp(), extended),
    ActivationStep =
    #'ActivationStep'{serialNumber=1,
              name="Activate",
              description="Activates this upgrade package"},
    UP = #upgradePackage{upgradePackageId = NewIndex,
             state = EnumState,
             ignoreBreakPoints = true,
             administrativeData = [ProductData],
             activationStep = [ActivationStep],
             created = CreateTime,
             %% directoryPathExtension = undefined,
             %% remoteFileStore = undefined,
             %% isLocallyStored=false,
             uri = Uri,
             creatorActionId = ActionId,
             password = EcimPassword},
    Fun = fun() -> mnesia:write(UP) end,
    case mnesia:transaction(Fun) of
    {atomic, ok} ->
        cmd("mkdir -p "++ArchivePath),
        ok = file:write_file(filename:join(ArchivePath, MetaFile), Data),
        swmLib:sync(),
        "UpgradePackage="++Key;
    {aborted, Reason} ->
        throw(Reason)
    end.

%%% ----------------------------------------------------------
%%% #           extract_product_data()
%%% Input: ConfigurationE:#xmlElement{}
%%% Output: #'ProductData'{}
%%% Exceptions:
%%% Description: Get product data from a up metadata file
%%% ----------------------------------------------------------
-spec extract_product_data(#xmlElement{}) -> #'ProductData'{}.

extract_product_data(ConfigurationE) ->
    UpProdE = find_element(product, ConfigurationE),
    Name = find_attribute(name, UpProdE),
    Id = find_attribute(id, UpProdE),
    Version = find_attribute(version, UpProdE),
    DateE = find_element(date, ConfigurationE),
    Description = case find_optional_element(description, ConfigurationE) of
              false -> "";
              DescriptionE -> find_text(DescriptionE)
          end,
               
    Type = case find_optional_element(type, ConfigurationE) of
        false -> "";
        TypeE -> find_text(TypeE)
        end,
    #'ProductData'{productName = Name,
           productNumber = swmInventory:format_product_number(Id),
           productRevision = Version,
           productionDate = find_text(DateE),
           description = Description,
           type = Type}.

%%% ----------------------------------------------------------
%%% #           loop_download(Pid, Handle, Fd, Size, BSize)
%%% Input: Pid:pid() - Ssh channel service
%%%        Handle - Sftp file handle
%%%        Fd:file_descriptor() - Local file handle
%%%       Size: integer() - Filesize
%%%       BSize:integer() - The selected blocksize
%%% Output: complete|{error, Reason}
%%% Exceptions:
%%% Description: Fetch a file piece by piece and update the progress info
%%%              so that it reads 90 % ready when the file is downloaded
%%% ----------------------------------------------------------


loop_download(Key, Pid, Handle, Fd, Size, BSize, Accu, {ok, Data}) ->

    %% HS28267
    garbage_collect(),

    %% HS23685
    case file:write(Fd, Data) of
    ok ->
        ok;
    {error, enospc} ->
        throw({fail, "No disk space left"});
    {error, Reason} ->
        update_progress(Key, [{additionalInfo, file:format_error(Reason)}]),
        erlang:error(Reason)
    end,
    %% end HS23685

    New = Accu+size(Data),
    receive
    update_progress ->
        update_progress(Key, [{progressPercentage, New*98 div Size}]);
    {cancel, _} -> % {cancel, UpKey}
        ssh_sftp:close(Pid, Handle),
        file:close(Fd),
        throw(cancelled)
    after 0 ->
        ok
    end,
    loop_download(Key, Pid, Handle, Fd, Size, BSize, New,
          ssh_sftp:read(Pid, Handle, BSize));
loop_download(_, Pid, Handle, Fd, _Size, _BSize, _Accu, eof) ->
    file:close(Fd),
    ssh_sftp:close(Pid, Handle),
    complete;
loop_download(_, Pid, Handle, Fd, _Size, _BSize, _Accu, {error, Reason}) ->
    error_msg("loop_download ~p~n",[{error, Reason}]),
    file:close(Fd),
    ssh_sftp:close(Pid, Handle),
    {error, Reason}.


%%% ----------------------------------------------------------
%%% #           handle_remove_package(UpDn)
%%% Input: UpDn:[binary()] Dn identifying an UpgradePackage MO
%%% Output: ok
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------


handle_remove_package(UpDn) ->
    Key = dn_to_key(UpDn),
    try element(4, Key) of
    PkgName ->
        AddInfo = "Removing package " ++ PkgName,
        update_progress([{additionalInfoClear, AddInfo}])
    catch
    _ : _ ->
        throw({fail, "Incomplete reference: " ++ UpDn})
    end,
    Up =
    case mnesia:dirty_read({upgradePackage, Key}) of
        [Obj] ->
        Obj;
        [] ->
        throw({fail, "Upgrade package does not exist"})
    end,
    case Up#upgradePackage.state of
    State when State =:= ?UpgradePackageState_INITIALIZED orelse 
           State =:= ?UpgradePackageState_PREPARE_COMPLETED orelse 
           State =:= ?UpgradePackageState_COMMIT_COMPLETED ->
        Fun = fun() -> do_remove_package(Key) end,
        case mnesia:transaction(Fun) of
        {atomic, ok} ->
            Path = get_up_archive_dir(
                 Up#upgradePackage.administrativeData),
            cmd("rm -rf "++Path),
            audit_software(),
            swmLib:write_swm_log("SwM", info, 
                     "UpgradePackage="++element(4,Key)++" removed"),
            CompleteTime = comsaI:iso_time(os:timestamp(), extended),
            update_progress([{additionalInfo, "Action complete"},
                     {progressPercentage, 100},
                     {timeActionCompleted, CompleteTime},
                     {result, ?ActionResultType_SUCCESS},
                     {state, ?ActionStateType_FINISHED}]);
        {atomic, stop} ->
            swmLib:write_swm_log("SwM", info, "Action remove failed"),
            CompleteTime = comsaI:iso_time(os:timestamp(), extended),
            update_progress([{additionalInfo, "Action complete"},
                     {progressPercentage, 100},
                     {timeActionCompleted, CompleteTime},
                     {result, ?ActionResultType_FAILURE},
                     {state, ?ActionStateType_FINISHED}])
        end;
    State ->
        ResultInfo = 
        "Upgrade package cannot be removed in state "++state(State),
        swmLib:write_swm_log("SwM", info, ResultInfo),
        CompleteTime = comsaI:iso_time(os:timestamp(), extended),
        update_progress([{resultInfo, ResultInfo},
                 {additionalInfo, "Action complete"},
                 {progressPercentage, 100},
                 {timeActionCompleted, CompleteTime},
                 {result, ?ActionResultType_FAILURE},
                 {state, ?ActionStateType_FINISHED}])
    end,
    ok.

get_up_archive_dir([ProductData]) ->
    get_up_archive_dir(ProductData);
get_up_archive_dir(ProductData) ->
    #'ProductData'{productName = Name,
           productNumber = Id,
           productRevision = Version} = ProductData,
    get_up_archive_dir(Name, Id, Version).

get_up_archive_dir(Name, Id, Version) ->
    filename:join(swmLib:archive_dir(),
          Name++"_"++no_slash(Id)++"_"++Version).

no_slash(Str) ->
    [case X of
     $/ -> $_;
     _ -> X
     end || X<-Str].


get_current_archive_dir() ->
    UP = swmLib:get_current_up_metadata(),
    Name = proplists:get_value(productName, UP),
    ProdId = proplists:get_value(productNumber, UP),
    Version = proplists:get_value(productRevision, UP),
    get_up_archive_dir(Name, ProdId, Version).

%%% ----------------------------------------------------------
%%% @doc Removes an UP. This functions runs within a mnesia
%%% transaction.
%%% @end
%%% ----------------------------------------------------------

-spec do_remove_package(tuple()) -> ok | stop.

do_remove_package(Key) ->
    Up =
    case mnesia:read({upgradePackage, Key}) of
        [Obj] ->
        Obj;
        [] ->
        throw({fail, "Upgrade package does not exist"})
    end,

    %% Check if it is the active version

    %% First, get the active version
    %% We're assuming that there is only one active sw version at a time
    [SwInventory] = mnesia:read({swInventory, {"1","1","1"}}),
    ActiveKey = dn_to_key(hd(SwInventory#swInventory.active)),
    [SwVersion] = mnesia:read({swVersion, ActiveKey}),
    ActiveProductData = SwVersion#swVersion.administrativeData,

    %% Then compare with UP product data
    [UpProductData] = Up#upgradePackage.administrativeData,

    case is_same_product(ActiveProductData, UpProductData) of
    true ->
        Info =
        "This upgrade package contains active software. "
        "It cannot be removed at this time.",
        do_update_progress(swm, [{resultInfo, Info}]),
        info_msg("UP not removed; contains active software~n"),
        stop;
    false ->
        case swmBackup:has_dependent_backups(UpProductData) of
        true -> 
            % HS62185, added case
            Info =
            "This upgrade package is referred by a backup. "
                    "It cannot be removed at this time.",
            do_update_progress(swm, [{resultInfo, Info}]),
            info_msg("UP not removed; referred by a backup~n"),
            stop;
        false ->
            mnesia:delete_object(Up),
            ok
        end
    end.


-spec is_same_product(#'ProductData'{},#'ProductData'{}) -> boolean(). 

is_same_product(P1, P2) ->
    P1#'ProductData'.productNumber == P2#'ProductData'.productNumber andalso
    P1#'ProductData'.productRevision == P2#'ProductData'.productRevision.

%%% ----------------------------------------------------------
%%% #           handle_remove_sw_version(VsnDn)
%%% Input: VsnDn: [binary()] - Dn identifying an SwVersionMain MO
%%% Output: ok
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

handle_remove_sw_version(_) ->

    %% Update tables
    swmModel:update_tables(),
    swmInventory:update_tables(),

    %% And report 
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress([{additionalInfoClear, "This action is deprecated"},
             {progressPercentage, 100},
             {timeActionCompleted, CompleteTime},
             {result, ?ActionResultType_SUCCESS},
             {state, ?ActionStateType_FINISHED}]),
    ok.

%% HS51877 fix
handle_remove_software_version(VsnDn) ->
    Key = dn_to_key(VsnDn),
    try element(4, Key) of
    SwVsn ->
        AddInfo = "Removing swVersion="++SwVsn,
        update_progress([{additionalInfoClear, AddInfo}])
    catch _:_ ->
        throw({fail, "Incomplete reference: "++VsnDn})
    end,

    %% Check that it isn't the active version
    Actives = [swmInventory:make_mom_key(PD#'ProductData'.productNumber,
                     PD#'ProductData'.productRevision)
           ||PD<-swmInventory:get_current_sw_version()],

    case lists:member(element(4,Key), Actives) of
        true ->
        info_msg("Currently running: ~p~n",[Actives]),
        throw({fail, "Cannot remove active version "++element(4,Key)});
        false ->
            ok
    end,

    DnRev = lists:reverse([list_to_binary(X)||X<-string:tokens(VsnDn, ",=")]),
    {atomic, ExistMo} = mnesia:transaction(
              fun() -> 
                  swmInventory:existsMo(DnRev, 0)
              end),
    case ExistMo of
    true ->
        %% This will remove all unused versions, but it will do for now
        %% because there should only be one other swVersion so it will work 
        %% as expected
        audit_software(),
        %% Update tables
        swmModel:update_tables(),
        swmInventory:update_tables(),
        swmLib:write_swm_log("SwM", info, "Software version removed"),
        update_progress([{resultInfo, "Software version removed"}]);
    false ->
        swmLib:write_swm_log("SwM", info, "No such software"),
        update_progress([{resultInfo, "No such software"}])
    end,

    %% And report complete success
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress([{progressPercentage, 100},
             {timeActionCompleted, CompleteTime},
             {result, ?ActionResultType_SUCCESS},
             {state, ?ActionStateType_FINISHED}]),
    ok.
%% HS51877 fix ends here


%%% ----------------------------------------------------------
%%% #           handle_prepare_package(Key)
%%% Input: Key:{string(), string(), string(), string()
%%%            A tuple which is a key in the upgradePackage table
%%% Output: ok
%%% Exceptions:
%%% Description: Downloads remaining parts of an upgrade package which
%%%              should leave the package in state PREPARE COMPLETED
%%% ----------------------------------------------------------

handle_prepare_package(Key) ->
    AddInfo = "Preparing package "++element(4,Key),
    Sender = sender(Key),
    swmLib:write_swm_log(Sender, info, "prepare"),
    update_progress(Key, [{additionalInfoClear, AddInfo}]),
    info_msg("~s~n",[AddInfo]),

    [Up] = mnesia:dirty_read({upgradePackage, Key}),
    case Up#upgradePackage.uri of
    undefined ->
        throw({fail, "Missing value for uri"});
    _ ->
        ok
    end,

    try do_download_packages(Up) of
    ok ->
        Key = Up#upgradePackage.upgradePackageId,
        mnesia:transaction(
          fun() -> 
              [UP] = mnesia:read({upgradePackage, Key}),
              NewUp = UP#upgradePackage{state=state(prepareCompleted)},
                         %% isLocallyStored=true},
              mnesia:write(NewUp) 
          end),
        swmLib:write_swm_log(Sender, info, "Action prepare complete"),
        CompleteTime = comsaI:iso_time(os:timestamp(), extended),    
        update_progress(Key, [{additionalInfo, "Preparation complete"},
                  {timeActionCompleted, CompleteTime},
                  {progressPercentage, 100},
                  {result, ?ActionResultType_SUCCESS},
                  {state, ?ActionStateType_FINISHED}]),
        ok
    catch Type:Reason ->

        %% HS23697: We should remove all files that
        %% were downloaded and caused this crash,
        %% except for the UP metadata file.
        ArchiveDir = get_up_archive_dir(
                   Up#upgradePackage.administrativeData),
        {ok, Files} = file:list_dir(ArchiveDir),
        [case swmLib:is_up_abspath(File) of
             true ->
             ok;
             false ->
             file:delete(filename:join(ArchiveDir, File))
         end   
         || File <- Files],
        %% end of fix HS23697

        NewUp = Up#upgradePackage{state=state(initialized)},
        mnesia:transaction(fun() -> mnesia:write(NewUp) end),
        case {Type, Reason} of
            {throw, cancelled} -> % HS37375
            ok;
            _ ->
            sysInitI:error_report(
              [{?MODULE, download_packages, [Up]},
               {mfa, {swmServer, do_download_packages, [Up]}},
               {Type, Reason},
               erlang:get_stacktrace()])
        end,
        erlang:Type(Reason)
    end.
%%% ----------------------------------------------------------
%%% #           do_download_packages(Key, Up)
%%% Input: Up:#upgradePackage{}
%%% Output: ok
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

do_download_packages(Up) ->
    case http_uri:parse(Up#upgradePackage.uri) of
    {ok, ParsedUri} when element(1, ParsedUri) == sftp ->
        do_download_packages(Up, ParsedUri);
    {ok, ParsedUri} when element(1, ParsedUri) == file ->
        test_download_packages(Up, ParsedUri)
    end.

do_download_packages(Up, Sftp) when element(1, Sftp) == sftp ->
    ArchiveDir = get_up_archive_dir(Up#upgradePackage.administrativeData), 
    BoardType_BB = swmBoardList:boardType(),
    BoardTypes_RADIO = all,
    case
    swmBoardList:products({BoardType_BB, BoardTypes_RADIO},
                  {global, ArchiveDir})
    of
    {ok, Products} ->
        named_download(Up, ArchiveDir, Products, Sftp);
    {uc_error, Reason} ->
        throw({uc_error, Reason})
    end.

named_download(Up, ArchiveDir, Products, Sftp) ->
    case swmLib:get_variable(use_legacy_download) of
        true ->
            legacy_download(Up, ArchiveDir, Sftp);
    _  ->
            do_named_download(Up, Products, Sftp) 
    end.

do_named_download(Up, Products, Sftp) ->
    info_msg("Initiating named download~n"),
    {sftp, User, Host, Port, RemoteFile, _Query} = Sftp,
        
    Password = comsaI:decrypt_password(Up#upgradePackage.password),
    {ok, ChannelPid, _CRef} = start_channel(Host, Port, User, Password),

    RemoteDir = get_remote_dir(ChannelPid, RemoteFile),

    %% Set up progress reporting by calculating the total size
    %% of all files in the directory

    %% FileSizes = [{{Name, ProdId, Version}, File, Path, Size}]

    FileSizes = get_file_sizes(ChannelPid, RemoteDir, Products),
    info_msg("File sizes: ~n~p~n",[FileSizes]),

    TotalSize = lists:foldl(fun({_, _, _, S}, A) -> S+A end, 0, FileSizes),
    info_msg("TotalSize: ~w bytes ~n",[TotalSize]),

    UpKey = Up#upgradePackage.upgradePackageId,
    {ok, Tref} = 
    timer:apply_interval(30000, ?MODULE,prepare_progress,[UpKey,TotalSize]),
    put(progress_timer, Tref),
    
    swmLib:set_ram_variable(download_progress, 0),

    info_msg("do_named_download/3, n. of CXP files: ~w~n", [length(FileSizes)]),

    ArchiveDir = get_up_archive_dir(Up#upgradePackage.administrativeData), 
    download_package(Up, ArchiveDir, ChannelPid, FileSizes),
    unpack_package(ArchiveDir, FileSizes),
    timer:cancel(Tref),
    ok.

download_package(Up, ArchiveDir, ChannelPid, FileSizes) ->
    %% Sanity checks
    UpKey = Up#upgradePackage.upgradePackageId,
    NeededSize = 
    lists:foldl(
      fun({_, _, _, Size}, Accu) when Size > 0 ->
          Size+Accu;
         ({_, Basename, _, _}, _) ->
    Msg = Basename++" is empty",
          update_progress(UpKey, [{additionalInfo, Msg}]),
          throw({fail, Msg})
      end, 0, FileSizes),
    FreeDisk = swmLib:get_free_disk(),
    info_msg("Free disk:  ~w bytes~n",[FreeDisk]),
    case FreeDisk of
    FreeDisk when FreeDisk > NeededSize -> 
        ok;
    FreeDisk -> 
        throw({fail, "Download failed due to disk space shortage. "++
               integer_to_list(NeededSize div 1024)++" k needed. "++
               integer_to_list(FreeDisk div 1024)++" k available"})
    end,
    download_package(UpKey, ArchiveDir, ChannelPid, 0, 0, FileSizes).

download_package(UpKey, ArchiveDir, ChannelPid, AccuDownloadTime, 
         AccuWriteTime, [{_, Basename, Path, _}|FileSizes]) ->
    update_progress(UpKey, [{additionalInfo,"Downloading "++Basename}]),
    {ok, Handle} = ssh_sftp:open(ChannelPid, Path, [read, binary]),
    LocalPath = filename:join(ArchiveDir, Basename),
    {ok, Fd} = file:open(LocalPath, [write, binary, raw]),
    case chunk_download(UpKey, ChannelPid, Handle, Fd) of
    {ok, {D,W}} -> 
        download_package(UpKey, ArchiveDir, ChannelPid, 
                 AccuDownloadTime+D, AccuWriteTime+W,
                 FileSizes);
    {error, Reason} ->
        sysInitI:error_report([{?MODULE, download_package},
                   {path, Path},
                   {upKey, UpKey}]),
        erlang:error(Reason)
    end;
download_package(UpKey, _, _, Download, Write, []) ->
    Msg = "Upgrade package download complete. Unpacking...",
    update_progress(UpKey, [{additionalInfoClear, Msg}]),
    info_msg("Upgrade package download complete.~n"
         "Download time: ~p s~n"
         "Disk write time: ~p s~n",
         [Download/1000000, Write/1000000]).

chunk_download(UpKey, ChannelPid, Handle, Fd) ->
    FirstRead = sftp_read(ChannelPid, Handle),
    chunk_download(UpKey, ChannelPid, Handle, Fd, 0, 0, FirstRead).

chunk_download(UpKey, ChannelPid, Handle, Fd, Download, Write, {D,{ok,Data}}) ->
    garbage_collect(),
    ThisWrite = 
    case file_write(Fd, Data) of
        {ThisW, ok} ->
        ThisW;
        {_, {error, enospc}} ->
        throw({fail, file:format_error(enospc)});
        {_, {error, Reason}}  ->
        update_progress(UpKey, [{additionalInfo, 
                     file:format_error(Reason)}]),
        throw({error, file:format_error(Reason)})
    end,
    mnesia:dirty_update_counter(swmRamVariables, download_progress, size(Data)),
    receive
    {cancel, _} ->
        ssh_sftp:close(ChannelPid, Handle),
        file:close(Fd),
        throw(cancelled)
    after 0 ->
        chunk_download(UpKey, ChannelPid, Handle, Fd, Download+D, 
               Write+ThisWrite, sftp_read(ChannelPid, Handle))
    end;
chunk_download(_, ChannelPid, Handle, Fd, Download, Write, {D, eof}) ->
    file:close(Fd),
    ssh_sftp:close(ChannelPid, Handle),
    {ok, {Download+D, Write}};
chunk_download(_, ChannelPid, Handle, Fd, _, _,  {_, {error, Reason}}) ->
    error_msg("chunk_download ~p~n",[{error, Reason}]),
    file:close(Fd),
    ssh_sftp:close(ChannelPid, Handle),
    {error, Reason}.

   
sftp_read(ChannelPid, Handle) ->
    timer:tc(fun() -> ssh_sftp:read(ChannelPid, Handle, ?CHUNK_SIZE) end).
    
file_write(Fd, Data) ->
    timer:tc(fun() -> file:write(Fd, Data) end).

unpack_package(ArchiveDir, FileSizes) ->    
    Paths = [filename:join(ArchiveDir, Basename)||
        {_, Basename, _, _} <- FileSizes],
    lists:foldl(fun(Path, Backlog) -> 
            receive
                {cancel, _} ->
                throw(cancelled)
            after 0 ->
                ok
            end,
            swmOs:mount_cxp_deferring(Path, Backlog)
        end,
        #mountBacklog{nofCxpsLeft=length(FileSizes)},
        Paths).


prepare_progress(UpKey, Size) ->
    Current = swmLib:get_ram_variable(download_progress),
    update_progress(UpKey, [{progressPercentage, Current*30 div Size}]).

    
get_remote_dir(Pid, RemoteFile) ->
    {ok, FIO} = ssh_sftp:read_file_info(Pid, RemoteFile),
    case FIO#file_info.type of
    directory ->
        RemoteFile;
    _ ->
        filename:dirname(RemoteFile)
    end.



get_file_sizes(Pid, RemoteDir, [{ProdId, {Source, File}} | Products]) ->
    Path = filename:join(dir(Source, RemoteDir), File),
    {ok, FileInfo} = file_info(Source, Path, Pid),
    case FileInfo#file_info.type of
    regular ->
        [{ProdId, File, Path, FileInfo#file_info.size}|
         get_file_sizes(Pid, RemoteDir, Products)];
    OtherType ->
        Reason =
        Path ++
        " is not a regular file. It is " ++
        atom_to_list(OtherType),
        throw({fail, Reason})
    end;
get_file_sizes(_, _, []) ->
    [].

%%% ----------------------------------------------------------
dir(global, GlobalDir) ->
    GlobalDir;
dir(hal, _) ->
    [HalSwpId] = swmBoardList:read_swp_selected(hal_swp_id),
    filename:join(swmLib:software_hal_dir(), HalSwpId).

%%% ----------------------------------------------------------
file_info(global, Path, Pid) ->
    case ssh_sftp:read_file_info(Pid, Path, 100000) of
    {ok, FIO} ->
        {ok, FIO};
    {error, Reason} ->
        throw({fail, make_string_flat(Reason) ++ " " ++ Path})
    end;
file_info(hal, Path, _) ->
    case file:read_file_info(Path) of
    {ok, FIO} ->
        {ok, FIO};
    {error, Reason} ->
        throw({fail, make_string_flat(Reason) ++ " " ++ Path})
    end.

%%% Description: Legacy download is used when the UP metadata has not been 
%%%              furnished with file names for package files 
          
legacy_download(Up, ArchiveDir, {sftp, User, Host, Port, RemoteFile, _Query})->
    Uri = Up#upgradePackage.uri,
    Password = comsaI:decrypt_password(Up#upgradePackage.password),
    {ok, Pid, _CRef} = start_channel(Host, Port, User, Password),

    {ok, FIO} = ssh_sftp:read_file_info(Pid, RemoteFile),
    RemoteDir = case FIO#file_info.type of
            directory ->
            RemoteFile;
            _ ->
            filename:dirname(RemoteFile)
        end,
    
    %% HS23701 fix part 1
    FreeDisk = swmLib:get_free_disk(),
    info_msg("Free disk:  ~w bytes~n",[FreeDisk]),

    %% We only handle files in a flat directory.
    {ok, Files} = sftp_list_dir(Pid, RemoteDir, 10000), % HT85211
    info_msg("Uri: ~p~nRemoteDir: ~p~nFiles: ~p~n",[Uri , RemoteDir, Files]),

    %% Set up progress reporting by calculating the total size
    %% of all files in the directory

    FileSizes =
    [begin
         Path = filename:join(RemoteDir, File),
         {ok, ThisFIO} = case ssh_sftp:read_file_info(Pid, Path, 10000) of
                 {ok, TFIO} -> {ok, TFIO};
                 {error, Reason} ->
                     {fail, make_string_flat(Reason)}
                 end,
         case {ThisFIO#file_info.type, filename:extension(File)}  of
         {regular, ".xml"} ->
             {Path, 0};
         {regular, ".cxp"} ->
             ThisSize = ThisFIO#file_info.size,
             {Path, ThisSize};
         _ ->
             {Path, 0}
         end
     end||File<-Files],

    info_msg("File sizes: ~n~p~n",[FileSizes]),
    TotalSize = lists:foldl(fun({_, S}, A) -> S+A end, 0, FileSizes),
    info_msg("TotalSize: ~p~n",[TotalSize]),
    %% HS23701 fix part 2
    if TotalSize > FreeDisk ->
        throw({fail, "Download failed due to disk space shortage. "++integer_to_list(TotalSize div 1024)++" k needed. "++integer_to_list(FreeDisk div 1024)++" k available"});
       true ->
        ok
    end,
    %% HS23701 ends here

    {ok, Tref} = timer:send_interval(1000, update_progress),
    put(progress_timer, Tref),

    lists:foldl(
      fun({_Path, 0}, AccuSize) ->
          AccuSize;
     ({Path, Size}, AccuSize) ->
          Basename = filename:basename(Path),
          Key = Up#upgradePackage.upgradePackageId,
          update_progress(Key, [{additionalInfo,"Downloading "++Basename}]),
          LocalPath = filename:join(ArchiveDir, Basename),
          {ok, Handle} = sftp_open(Pid, Path, [read, binary]), % HT85211
          {ok, Fd} = file:open(LocalPath, [write, binary, raw]),
          Firstread = ssh_sftp:read(Pid, Handle, ?CHUNK_SIZE),
          info_msg("Path: ~p~nSize: ~p~nTotalSize: ~p~nAccuSize= ~p~n",
               [Path, Size, TotalSize, AccuSize]),
          case loop_download(Key, Pid, Handle, Fd, TotalSize, ?CHUNK_SIZE, 
                 AccuSize, Firstread) of
          complete ->
              Size + AccuSize;
          {error, ReadReason} ->
              throw(
            {fail, make_string_flat(ReadReason)})
          end
              
%         spawn(swmOs, mount_cxp, [LocalPath]),
      end, 0, FileSizes),
    timer:cancel(Tref),
    ok.




%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

handle_verify_package(Key) ->
    AddInfo = "Verifying package "++element(4,Key),
    Sender = sender(Key),
    swmLib:write_swm_log(Sender, info, "verify"),
    update_progress(Key, [{additionalInfoClear, AddInfo}]),
    
    %% The trigger action for verify is somewhat different than the others
    %% because it should not fail, just report warnings and concerns.
    update_progress(Key, [{additionalInfo, "Starting CS verification"}]),

    swmOs:clear(),
    [UP] = mnesia:dirty_read({upgradePackage, Key}),
    try swmOs:install_up(UP)
    catch error:{exit_code, 1} ->
        throw({fail, "UP signature failure"});
      error:{exit_code, 2} ->
        throw({fail, "Failed to read package. It may be corrupt or a software error occured."})
    end,

    CbModules = swmLib:get_upg_callbacks(),
    [case has_callback(CbModule, verify_precondition) of
     true ->
         try apply(CbModule, verify_precondition, []) of
         ok ->
             ok;
         {ok, Msg} ->
             swmLib:write_swm_log(Sender, warning, Msg),
             update_progress(Key, [{additionalInfo, Msg}])
         catch T:E ->
             sysInitI:error_report(
               [{?MODULE, handle_verify_package, [Key]},
            {line, ?LINE},
            {mfa, {CbModule, verify_precondition, []}},
            {T,E}]),
             Msg2 = "Action verifyPreconditions failed on a software "
             "error",
             swmLib:write_swm_log(Sender, error, Msg2),
             update_progress(Key, [{additionalInfo, Msg2}])
         end;
     false ->
         ok
     end||CbModule<-CbModules],

    update_progress(Key, [{additionalInfo, "CS verification complete"}]),

    update_progress(Key, [{additionalInfo, "Sending application triggers"}]),
    case appmI:call_upi("verifyPreconditions") of
    {ok, []} ->
        swmLib:write_swm_log(Sender, info, "All verification complete"),
        update_progress(Key, [{additionalInfo, "All verification complete"}]);
    {ok, Responses} ->
        [begin
         Msg4 = LmName++"("++ProdNr++"): "++Result--"\n",
         update_progress(Key, [{additionalInfo, Msg4}]),
         case Result of
             "ERROR: "++_ ->
             swmLib:write_swm_log(Sender, info, Msg4);
             _  ->
             swmLib:write_swm_log(Sender, info, Msg4)
         end
         end||{{LmName, ProdNr}, Result}<-Responses];
    {error, Reasons} ->
        swmLib:write_swm_log(Sender, info, "Participants not ok"),
        update_progress(Key, [{additionalInfo, "Participants not ok"}]),
        throw(Reasons)

    end,

    swmLib:write_swm_log(Sender, info, "Action verify complete"),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),   
    update_progress(Key, [{additionalInfo, "Verification complete"},
              {progressPercentage, 100},
              {timeActionCompleted, CompleteTime},
              {result, ?ActionResultType_SUCCESS},
              {state, ?ActionStateType_FINISHED}]),
    ok.

get_up_state(Key) ->
    Fun = fun() -> 
          [Up] = mnesia:read({upgradePackage, Key}),
          Up#upgradePackage.state
      end,
    {atomic, State} = mnesia:transaction(Fun),
    State.


-spec handle_activate_package(tuple()) -> {ok, activationInProgress}.

handle_activate_package(Key) ->
    AddInfo = "Activating package "++element(4, Key),
    Sender = sender(Key),
    swmLib:write_swm_log(Sender, info, "activate"),
    update_progress(Key, [{additionalInfoClear, AddInfo},
              {progressPercentage, 1}]),

    RevertStateFun = fun() -> do_state_change(Key, prepareCompleted) end,

    try pre_activate_package(Key)
    catch
    throw:Throw ->
        mnesia:transaction(RevertStateFun),
        throw(Throw);
    T:E ->
        mnesia:transaction(RevertStateFun),
        info_msg("Activate stopped at pre_activate_package ~p~n~p~n",
             [{T,E}, erlang:get_stacktrace()]),
        erlang:T(E, [Key])
    end,

    %% This part should NOT be executed within the scope of a mnesia
    %% transaction
    try do_activate_package(Key) of
    ok ->
        {ok, activationInProgress}
    catch Type:Error ->
        info_msg("Activate stopped at do_activate_package ~p~n~p~n",
             [{Type,Error}, erlang:get_stacktrace()]),
        %% Soft upgrade cleanup
        case swmLib:get_variable(set_unpacked) of
        Vsn when Vsn /= undefined->
            release_handler:remove_release(Vsn),
            swmLib:erase_variable(set_unpacked);
        undefined ->
            ok
        end,
        %% General cleanup
        mnesia:transaction(RevertStateFun),
        %% This may produce unwanted
        %% case Error of
        %%  {fail, _} -> 
        %%      %% This indicates a fault by the operator = 
        %%      %% no error report
        %%      sysInitI:warning_report(
        %%        [{Type, Error},
        %%         {stacktrace, erlang:get_stacktrace()}]),
        %%      ok;
        %%  _ ->
        %%      sysInitI:error_report(
        %%        [{Type, Error}, 
        %%         {stacktrace, erlang:get_stacktrace()}])
        %% end,

        erlang:Type(Error)
    end.
%% case mnesia:transaction(fun() -> do_activate_package(Up) end) of
%%  {atomic, ok} ->
%%      {ok, activationInProgress};
%%  {aborted, Reason} ->
%%      %% Soft upgrade cleanup
%%      case swmLib:get_variable(set_unpacked) of
%%      Vsn when Vsn /= undefined->
%%          release_handler:remove_release(Vsn),
%%          swmLib:erase_variable(set_unpacked);
%%      undefined ->
%%          ok
%%      end,
%%      %% General cleanup
%%      mnesia:dirty_write(
%%        Up#upgradePackage{state = state(prepareCompleted)}),
%%      throw(Reason)
%% end;

pre_activate_package(Key) ->
    %% Create the rollback backup
    %% HT47248 fix
    [UP] = swmInventory:get_current_sw_version(),
    UpStr = 
    UP#'ProductData'.productName++"_"++
    format_product_number(UP#'ProductData'.productNumber)++"_"++
    UP#'ProductData'.productRevision,
    BuName = "Rollback_backup_"++UpStr++"_"++
    comsaI:iso_time(os:timestamp(), basic),     
    %% Fix ends here
    %% This flag will be detected if the system restarts on rollback
    swmLib:set_variable(ug_failed, BuName),
    ManagerKey = {"1","1","1","1"},
    swmBackup:set_default_progress_report(mgr, "CREATE", 0),
    BuRef = create_backup(list_to_binary(BuName), ManagerKey, system, mgr),
    swmLib:erase_variable(ug_failed),
    {ok, MnesiaPath} = swmBackup:get_mnesia_file(BuRef),
    swmLib:activate_upgrWindow_tables(),
    update_progress(
      Key, [{additionalInfo, "Last chance to cancel without restart"}]),
    receive
    {cancel, Key} ->
        throw(cancelled)
    after 10000 ->
        %% HU49624 Cancel flag fix
        {atomic, _} = 
        mnesia:transaction(
          fun() -> swmLib:set_ram_variable(activate_cancel, reboot) 
          end),
        ok
    end,
    update_progress(
      Key, [{additionalInfo, "Cancel will now cause a restart"}]),
    
    swmLib:install_fallback(MnesiaPath), %HU15807
    swmFallbackList:pending_ug_backup(BuName),

    %% Activate the fallback timer before reboot
    %% This is actually not in accordance with ECIM, but we want
    %% some kind of time limited action and this is how we do it.

    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
    FallbackTimeout = SwM#swM.fallbackTimer,
    {ok, CountdownTimer} =
    timer:apply_interval(1000, swmServer, countdown,
                 [FallbackTimeout, os:timestamp()]),
    {ok, FallbackTimer} =
    timer:apply_after(FallbackTimeout*1000, swmServer,fallback,
              [Key,
               activating_before_restart,
               "Activate timeout"]),
    put(countdown_timer, CountdownTimer),
    put(fallback_timer, FallbackTimer),
    info_msg("Fallback timer started. Fallback in ~w seconds ~n",
         [FallbackTimeout]).

format_product_number(ProductNumber) ->
    [case X of
         $_ -> $/;
         _ -> X
     end||X<-ProductNumber].



-spec do_activate_package(tuple()) -> ok.

do_activate_package(Key) ->
    [UP] = mnesia:dirty_read({upgradePackage, Key}),
    %% Remember what are doing in case of restart
    swmLib:set_variable(activating_up, UP),
    Sender = sender(Key),

    mount_homes(),

    swmOs:clear(),
    Key = UP#upgradePackage.upgradePackageId,
    %% Progress percentage is based on measurements of time elapsed in TCU
    %% Just keep it rising in some fashion
    update_progress(Key, [{additionalInfo, "Installing upgrade package"},
              {progressPercentage, 4}]),

    %% Prepare to generate necessary files

    swmOs:install_up(UP),

    sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: new UP installed"),
    update_progress(
      Key, [{additionalInfo, "Software installed. Backing up data."},
        {progressPercentage, 42}]),

    %% HU29568
    %% New upgrade_init handling
    %% Remove the soft link solution and use a compressed backup on 
    %% OtherHomeDir instead

    OtherHomeDir = swmOs:home_dir_other(),
    UgDb = filename:join(OtherHomeDir, "upgrade_init"),
    TmpFile = filename:join(sysEnv:tmp_dir(), "upgrade_init"),
    TmpFileGz = TmpFile++".gz",

    swmLib:activate_upgrWindow_tables(),
    swmLib:set_variable(upgrade, true),
    ok = swmLib:mnesia_backup(TmpFile),
    swmLib:erase_variable(upgrade),
    
    {0, _} = swmOs:cmdres(["cd ",sysEnv:tmp_dir(), " ; gzip ",TmpFile]),
    {0, _} = swmOs:cmdres(["mv -f ",TmpFileGz, " ", UgDb]),

    update_progress(Key, [{additionalInfo, "Database backup complete"},
              {progressPercentage, 43}]),

    find_new_paths(OtherHomeDir),
    %% HS23704 fix
    send_command(Key, verify_upgrade, "", verify_upgrade_failed, Sender), 
    %% Fix ends here

    send_command(Key, activate_start, "", reboot, Sender),

    make_cs_preload(OtherHomeDir),

    send_command(Key, preload, "", reboot, Sender),
    try ets:delete(swmNewPaths)
    catch _:_ ->
        ok
    end,

    send_command(Key, activate, "", reboot, Sender),

    case {swmLib:get_variable(upgrade_type), sysEnv:rcs_mode()} of
    {Type, simulated} when Type==reboot; Type==undefined ->
        update_progress(Key,[{additionalInfo, "Reboot upgrade simulated"}]),
        swmOs:preload(),
        swmOs:activate(),
        %% The "Switching over" message is used by node test cases
        %% Don't change it
        update_progress(Key, [{additionalInfo, "Switching over"}]),
        unmount_homes(),
        timer:apply_after(1000,
                  ?MODULE,
                  restart_node,
                  [cold, ?ALH_TAG_UpgradeNormal]);
    {Type, target} when Type==reboot; Type==undefined ->
        update_progress(Key, [{additionalInfo, "Reboot upgrade"}]),
        update_progress(Key, [{additionalInfo, "Loading OS"},
                  {progressPercentage, 43}]),
        swmOs:preload(),
        update_progress(Key, [{additionalInfo, "Activating"},
                  {progressPercentage, 50}]),
        swmOs:activate(),

        %% The "Switching over" message is used by node test cases
        %% Don't change it
        update_progress(Key, [{additionalInfo, "Switching over"},
                  {progressPercentage, 56}]),
        unmount_homes(),
        swmLib:sync(),
        run_fsck(),
        timer:apply_after(1000,
                      ?MODULE,
                      restart_node,
                      [cold, ?ALH_TAG_UpgradeNormal])
    %% {soft, simulated} ->
    %%     update_progress_dirty([{additionalInfo, "Soft upgrade simulated"}]),
    %%     %% Clear the release handler of garbage
    %%     [begin
    %%   release_handler:remove_release(Vsn),
    %%   cmd("rm -rf "++sysEnv:releases_vsn_dir(Vsn))
    %%      end||
    %%  {_, Vsn, _, State} <-release_handler:which_releases(),
    %%  State /= permanent],

    %%     %% Install the backup
    %%     mnesia:install_fallback(MnesiaPath),


    %%     %% Create a run file for make_release.escript using the new OTP
    %%     UgPatches = filename:join(swmLib:swm_dir(), "ug_patches"),
    %%     MrDevPattern = filename:join(UgPatches, "make_release.escript"),
    %%     filelib:ensure_dir(MrDevPattern),
    %%     [MakeRelease] =
    %%  case filelib:wildcard(MrDevPattern) of
    %%      [] ->
    %%      MrPattern = filename:join(
    %%                [SwVersionDir, "*CXP*", "*CXC*",
    %%                 "sys-*", "priv", "bin",
    %%                 "make_release.escript"]),
    %%      filelib:wildcard(MrPattern);
    %%      Dev ->
    %%      Dev
    %%  end,

    %%     %% TODO: The use of OtpDir is problematic here. It should be the
    %%     %% the path to an unpacked OTP tree, but with the RBS CXP from the
    %%     %% CI web it will be the atom 'undefined'. This is so because the
    %%     %% code unpack_cxp_finalize/3 expects to find a tar.gz OTP package
    %%     %% which does not happen (the package appears to be expanded
    %%     %% already when unpack_cxp_finalize/3 executes).
    %%     Cmd = "export OTP_ROOT="++OtpDir++"\n"++
    %%  "export PATH=${OTP_ROOT}/bin:${PATH}\n"++
    %%  MakeRelease++" "++SwVersionDir++" "++OtpDir++" "++OtherHomeDir++
    %%  " upgrade\n",
    %%     ExePath = filename:join(OtherHomeDir, "run_make_rel"),
    %%     ok = file:write_file(ExePath, list_to_binary(Cmd)),
    %%     cmd("chmod u+x "++ExePath++" && "++ExePath),

    %%     %% Install other ug dev patches
    %%     DevPatches = filename:join(OtherHomeDir, "dev_patches"),
    %%     cmd("cp -R "++UgPatches++"/* "++DevPatches),

    %%     %% Check if the make_release.escript program finished. There
    %%     %% should be a data file
    %%     UgCompletePath =
    %%  filename:join(sysEnv:home_dir_other(), "make_release_complete"),
    %%     case file:consult(UgCompletePath) of
    %%  {ok, Data} ->
    %%      [{relpath, RelPath},{appdirs, AppDirs}] = Data,
    %%      update_progress_dirty([{additionalInfo, "New release created"}]),
    %%      run_upgrade(RelPath, AppDirs);
    %%  {error, Reason} ->
    %%      error_msg("Cannot read ~p: ~p~n",[UgCompletePath, Reason]),
    %%      throw(make_release_failed)
    %%     end
    end,
    ok.

mount_homes() ->
    case filelib:wildcard("/software/RCSEE*")  of
    [] -> % classic
        ok;
    _ ->
        swmOs:homes(mount)
    end.

unmount_homes() ->
    case filelib:wildcard("/software/RCSEE*") of
    [] -> % classic
        ok;
    _ ->
        swmOs:homes(umount)
    end.
         

run_fsck() ->
    case sysEnv:rcs_mode() of
    target ->
        swmOs:run_swm_wrapper_res("fcsk -fpv /dev/rootvg/hfs1-lv"),
        swmOs:run_swm_wrapper_res("fcsk -fpv /dev/rootvg/hfs2-lv");
    _ ->
        ok
    end.


make_cs_preload(OtherHomeDir) ->
    swmAppData:push_appdata_upgrade(OtherHomeDir),
    %% swmUpgradeCb:make_releases(OtherHomeDir),
    ok.


%%% ----------------------------------------------------------
%%% @doc Returns a trigger string for APPM and a human-readable
%%% string for use as MO attribute values.
%%% @end
%%% ----------------------------------------------------------

-spec trigger(atom()) -> {string(), string()}.

trigger(verify_upgrade) -> {"verifyUpgrade", "verifyUpgrade"};
trigger(activate_start) -> {"activateStart", "activateStart"};
trigger(preload) ->  {"preload", "preload"};
trigger(activate) -> {"activate", "activate"};
trigger(confirm) -> {"commit", "confirm"}.


-spec send_command(
    Key::swm|{string(), string(), string(), string()},
    Action::verify_upgrade|activate_start|preload|activate|confirm,
    Args::string(),
    FailAction::reboot|confirm_failed|verify_upgrade_failed,
    Sender::string()) -> any().

send_command(Key, Action, _Args, FailAction, Sender) ->
    {AppmIAction, OperatorAction} = trigger(Action),
    CbModules = swmLib:get_upg_callbacks(),
    [case has_callback(CbModule, Action) of
     true ->
         try apply(CbModule, Action, []) of
         ok ->
             ok;
         {ok, Msg} ->
             swmLib:write_swm_log(Sender, info, Msg),
             case Action of
             confirm ->  ok;
             _ ->
                 update_progress(Key, [{additionalInfo, Msg}])
             end;
             
         {error, Msg} ->
             swmLib:write_swm_log(Sender, error, Msg),
             case Action of
             confirm -> ok;
             _ ->
                 update_progress(Key, [{additionalInfo, Msg}])
             end,
             throw(FailAction)
         catch 
         T:E ->
             sysInitI:error_report(
               [{?MODULE, send_command, [Action]},
            {line, ?LINE},
            {mfa, {CbModule, Action, []}},
            {T,E}]),
             Msg3 = "Action "++OperatorAction++
                " failed on a software error",
             swmLib:write_swm_log(Sender, error, Msg3),
             update_progress(Key, [{additionalInfo, Msg3}])
         end;
     false ->
         ok
     end
     || CbModule <- CbModules],

    case Action of
    confirm -> ok;
    _ ->
        update_progress(
          Key, [{additionalInfo, "CS "++OperatorAction++" complete"}]),
        
        update_progress(
          Key, [{additionalInfo, 
             "Sending application trigger "++OperatorAction}])
    end,
    %% Phase = case Args of
    %%      ""-> AppmIAction;
    %%      _ -> AppmIAction++" "++Args
    %%      end,
    case appmI:call_upi(AppmIAction) of
    {ok, []} ->
        swmLib:write_swm_log(Sender, info, "All participants ok"),
        case Action of
        confirm -> ok;
        _ ->
            update_progress(
              Key, [{additionalInfo, "All participants ok"}])
        end;
    {ok, Responses} ->
        [begin
         Msg4 = LmName++"("++ProdNr++"): "++Result--"\n",
         update_progress(Key, [{additionalInfo, Msg4}]),
         case Result of
             "ERROR: "++_ ->
             swmLib:write_swm_log(Sender, error, Msg4),
             throw(FailAction);
             _  ->
             swmLib:write_swm_log(Sender, info, Msg4),
             ok
         end
         end||{{LmName, ProdNr}, Result}<-Responses];
    {error, Reasons} ->
        swmLib:write_swm_log(Sender, error, "Participants not ok"),
        case Action of
        confirm -> ok;
        _ -> 
            update_progress(
              Key, [{additionalInfo, "Participants not ok"}])
        end,
        throw(Reasons)

    end.

%%% ----------------------------------------------------------
%%% @doc Wraps an erlang:function_exported/3 call, providing
%%% zero for the arity argument and ensuring that the referred
%%% module is also loaded.
%%% @end
%%% ----------------------------------------------------------

-spec has_callback(module(), atom()) -> boolean().

has_callback(Module, Action) ->
    case code:ensure_loaded(Module) of
    {error, Reason} ->
        sysInitI:error_report(
          [{mfa, {code, ensure_loaded, [Module, Action]}}, 
           {reason, Reason}]),
        erlang:error(cannot_load_module, [Module, Action]);
    _ ->
        erlang:function_exported(Module, Action, 0)
    end.

find_new_paths(HomeOther) ->
    ets:new(swmNewPaths, [set, public, named_table]),
    Pattern = filename:join([HomeOther, "software", "*", "cxp*.xml"]),
    CxpFiles = filelib:wildcard(Pattern),
    [register_paths(CxpFile)||CxpFile<-CxpFiles],
    ok.

register_paths(CxpFile) ->
    CxpPath = filename:join(swmLib:squash_fs_dir(),
                filename:basename(filename:dirname(CxpFile))),
    {ConfigurationE, _} = xmerl_scan:file(CxpFile, []),
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    [begin
     ProdId = find_attribute(id, ProductE),
     ets:insert_new(swmNewPaths, {ProdId, CxpPath})
     end||ProductE<-ContentInfoE#xmlElement.content,
      ProductE#xmlElement.name == product],
    ok.

get_new_cxp_path(LmId) ->
    case ets:lookup(swmNewPaths, LmId) of
    [{_, Path}] -> Path;
    [] -> erlang:error(unknown_lm_id,[LmId])
    end.


%% run_upgrade(RelFile, AppDirs) ->

%%     NewVsn =
%%  case release_handler:set_unpacked(RelFile, AppDirs) of
%%      {ok, Vsn} ->
%%      swmLib:set_variable(set_unpacked, Vsn),
%%      Vsn;
%%      {error, Reason} ->
%%      sysInitI:error_report(
%%        [{mfa, {release_handler, set_upnacked, [RelFile, AppDirs]}},
%%         {error, Reason}]),
%%      erlang:error(Reason, [])
%%  end,
%%     case release_handler:check_install_release(NewVsn, [purge]) of
%%  {ok, _OtherVsn, _Descr} ->
%%      ok;
%%  {error, Reason2} ->
%%      sysInitI:warning_report(
%%        [{mfa, {release_handler, check_install_release, [NewVsn, purge]}},
%%         {error, Reason2}])
%%     end,
%%     Opts = [{error_action, reboot},
%%      {code_change_timeout, 10000},
%%      {suspend_timeout, 10000},
%%      {update_paths, true}],
%%     spawn(release_handler, install_release, [NewVsn, Opts]).


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

handle_activation_complete() -> 
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,
    
    update_progress(Key, [{additionalInfo, "Finishing activation"},
              {progressPercentage, 99}]),
    
    Fun = fun() -> do_activation_complete() end,
    case mnesia:transaction(Fun) of
    {atomic, ok} ->
        %% HT12440
        ok = swmLib:unlock_action_capable(?ACTIVATE_UP_ACTION_CAPABLE_ID),
        swmLib:write_swm_log(sender(Key), info, "Activation complete"),
        CompleteTime = comsaI:iso_time(os:timestamp(), extended),       
        update_progress(Key, [{additionalInfo, "Activation complete"},
                  {timeActionCompleted, CompleteTime},
                  {progressPercentage, 100},
                  {result, ?ActionResultType_SUCCESS},
                  {state, ?ActionStateType_FINISHED}]),
        try post_activate() of
        {ok, Result} -> {ok, Result}
        catch T:E ->
            sysInitI:error_report(
              [{T,E}, erlang:get_stacktrace()]),
            handle_fail(Key, "Post activation procedure failed", 
                undefined),
            restart_node(cold, ?ALH_TAG_UpgradeFailure),
            {T,E}
        end;

    Reason ->
        sysInitI:error_report([Reason, erlang:get_stacktrace()]),
        handle_fail(Key, "Activation failed", undefined),
        restart_node(cold, ?ALH_TAG_UpgradeFailure),
        Reason
    end.

do_activation_complete() ->
    UP = swmLib:get_variable(activating_up),
    mnesia:write(UP#upgradePackage{state = state(waitingForCommit)}).

post_activate() ->
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,
    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
    FallbackTimeout = SwM#swM.fallbackTimer,
    {ok, CountdownTimer} = 
    timer:apply_interval(1000, swmServer, countdown,
                 [FallbackTimeout, os:timestamp()]),
    {ok, FallbackTimer} =
    timer:apply_after(FallbackTimeout*1000, swmServer, fallback,
              [Key,
               waiting_for_confirm,
               "Confirm timeout"]),
    {ok, AlarmTimer} =
    case FallbackTimeout of
        FallbackTimeout when FallbackTimeout < 300 ->
        send_fallback_alarm(FallbackTimeout, Key),
        undefined;
        _ ->
        timer:apply_after(1000*(FallbackTimeout-300),
                  swmServer, send_fallback_alarm, [300, Key])
    end,
    erase_upgrade_marker(),
    swmDbMonitor:force_auto_backup(),
    {ok, {CountdownTimer, FallbackTimer, AlarmTimer}}.


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ---------------------------------------------------------

%% handle_commit_package(Key) ->
%%     AddInfo = "Confirming package "++element(4,Key),
%%     Sender = sender(Key),
%%     logI:write_log("SwmLog", Sender, info, "confirm"),
%%     update_progress(Key, [{additionalInfoClear, AddInfo}]),

%%     [Up] = mnesia:dirty_read({upgradePackage, Key}),
%%     case Up#upgradePackage.state of
%%  ?UpgradePackageState_WAITING_FOR_COMMIT ->
%%      % HS34720
%%      send_command(Key, confirm, "", confirm_failed, Sender),
%%      % HS34720 end

%%      [ets:delete(OldTab)||{_, OldTab} <- ets:tab2list(olddb)],
%%      ets:delete(olddb),
%%      OldDb = filename:join(sysEnv:home_dir(), "upgrade_init"),
%%      file:delete(OldDb),
%%      mnesia:uninstall_fallback(),
%%      mnesia:dirty_write(
%%        Up#upgradePackage{state = state(commitCompleted)}),
%%      {ME, SF, SwM, UpVsn} = Key,
%%      Dn = [<<"ManagedElement">>, list_to_binary(ME),
%%        <<"SystemFunctions">>, list_to_binary(SF),
%%        <<"SwM">>, list_to_binary(SwM),
%%        <<"UpgradePackage">>, list_to_binary(UpVsn)],
%%      comsaI:clear_alarm('FallbackOperationStartingSoon', Dn),
%%      update_count(-1),
%%      swmOs:commit(),
%%      swmFallbackList:add_pending(),
%%      BuName = "Final_backup_for_"++UpVsn++"_"++
%%      comsaI:iso_time(os:timestamp(), basic),
%%      ManagerKey = {"1","1","1","1"},
%%      swmBackup:set_default_progress_report(mgr, "CREATE", 0),
%%      swmBackup:create_backup_common(
%%        list_to_binary(BuName), ManagerKey, system, mgr),
%%      swmFallbackList:add_backup(after_ug, BuName),
%%      swmFallbackList:add_backup(latest, BuName),
%%      audit_software(),
%%      ok;
%%  State ->
%%      throw("Confirm can not be executed in state " ++ state(State))
%%     end,
%%     CompleteTime = comsaI:iso_time(os:timestamp(), extended),
%%     update_progress(Key, [{additionalInfo, "Confirm complete"},
%%            {timeActionCompleted, CompleteTime},
%%            {progressPercentage, 100},
%%            {result, ?ActionResultType_SUCCESS},
%%            {state, ?ActionStateType_FINISHED}]),
%%     swmLib:erase_variable(activating_up),
%%     ok.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

handle_mnesia_table_event({write, swVersion, Obj, _, _}=E) ->
    case dets:insert(swVersion, Obj) of
    ok -> ok;
    {error, Reason} ->
        sysInitI:error_report(
          [{?MODULE, handle_mnesia_table_event, [E]},
           {mfa, {dets, insert, [swVersion, Obj]}},
           {error, Reason}])
    end;
handle_mnesia_table_event({delete, swVersion, _, _, _}) ->
    ok;

handle_mnesia_table_event(_) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Maps the given DN to a tuple containing the RDN values
%%% only. For example, ManagedElement=1,Chimney=3 would map to
%%% {"1", "3"}.
%%%
%%% TODO: No caller passes a binarized string as input? Or is it
%%% just dialyzer that does not detect it?
%%% @end
%%% ----------------------------------------------------------

-spec dn_to_key(string()|binary()) -> tuple().

dn_to_key(DnBin) when is_binary(DnBin) ->
    dn_to_key(binary_to_list(DnBin));

dn_to_key(Dn) when is_list(Dn) ->
    Parts = string:tokens(Dn, ","),
    KeyList =
    [begin
         [_, KeyPart] = string:tokens(Part, "="),
         KeyPart
     end||Part<-Parts],
    list_to_tuple(KeyList).

%%% ----------------------------------------------------------
%%% #           action_handler(Fun)
%%% Input: Fun:fun() - Action to be called under supervision
%%% Output:
%%% Exceptions:
%%% Description: Performs the Fun and catches all errors with proper progress
%%%              reporting. It should also close any open sftp connections and
%%%              do whatever clean up that's necessary. In terms of error
%%%              reporting, that is a section that can be much developed.
%%% ----------------------------------------------------------

action_handler(Fun) ->
    action_handler(Fun, swm).

action_handler(Fun, Key) ->
    NoSuccess = "The action could not be completed",
    SwError = "A software related error occured",
    try Fun() of
    ok -> ok;
    {ok, Result} -> {ok, Result}
    catch
    throw : cancelled ->
        swmLib:write_swm_log(sender(Key), info, "Operation cancelled"),
        handle_cancelled(Key);
    throw : Reason when Reason =:= verify_upgrade_failed ->
        %% HS62261: This is considered a non-CS fault.
        Msg = "UP verification failed",
        swmLib:write_swm_log(sender(Key), error, Msg),
        warning_msg("throw ~w~n", [Reason]),
        handle_fail(Key, NoSuccess, Reason);
    throw : Reason when Reason =:= confirm_failed ->
        %% Unlikely to happen; enforce restart.
        swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
        error_msg("throw ~w~n", [Reason]),
        handle_fail(Key, NoSuccess, Reason);
    throw : {fail, Reason} ->
        %% Not a SW error. Command failed.
        swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
        warning_msg("External reason: ~s~n",[make_string_flat(Reason)]),
        handle_fail(Key, NoSuccess, Reason);
    throw : {uc_error, Reason} ->
        %% Not a SW error. Command failed.
        swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
        handle_fail(Key, NoSuccess, Reason);
    throw : Reason ->
        swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
        error_msg("throw ~p~n", [Reason]),
        handle_fail(Key, NoSuccess, Reason);
    Type : Reason ->
        swmLib:write_swm_log(sender(Key), error, "Software error"),
        sysInitI:error_report([{Type, Reason},
                       {stacktrace, erlang:get_stacktrace()}]),
        handle_fail(Key, NoSuccess, SwError)
    end.

sender(swm) ->
    "SwM";
sender({"1","1","1"}) ->
    "SwM";
sender({_, _, _, UpId}) ->
    "UP="++UpId.


%%% ----------------------------------------------------------
%%% #           handle_fail(ProgressInfo)
%%% Input: ProgressInfo:string()
%%% Output:
%%% Exceptions:
%%% Description: This function reports actions that have failed
%%% ----------------------------------------------------------

-record(cleanupContext, {action}).

handle_fail(Key, ProgressInfo, Reason) ->
    Msg = make_string_flat(Reason),
    swmLib:write_swm_log(sender(Key), info, Msg),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(Key, [{result, ?ActionResultType_FAILURE},
              {resultInfo, Msg},
              {progressInfo, ProgressInfo},
              {progressPercentage, 100},
              {state, ?ActionStateType_FINISHED},
              {timeActionCompleted, CompleteTime}]),
    cleanup(#cleanupContext{action=Reason}, ?ALH_TAG_UpgradeFailure).

%%% ----------------------------------------------------------
%%% #           handle_cancelled()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: This function reports actions that have been cancelled
%%% ----------------------------------------------------------

handle_cancelled(Key) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(Key, [{result, ?ActionResultType_FAILURE}, % HS76784
              {state, ?ActionStateType_CANCELLED},
              {progressPercentage, 100},
              {timeActionCompleted, CompleteTime}]),
    cleanup(#cleanupContext{}, ?ALH_TAG_UpgradeCancelled).

%%% ----------------------------------------------------------
actionId_init() ->
    swmLib:init_action_id(ug).

    %% ets:new(?MODULE, [named_table, public, ordered_set, {keypos, 1}]),
    %% %% Start at a quasi random place in the number series, but never at 0
    %% {_, X, _} = os:timestamp(),
    %% ets:insert(?MODULE, {actionId, (X rem 65534)+1}).

%%% ----------------------------------------------------------
actionId_new() ->
    swmLib:get_new_action_id(ug).
    %% try ets:update_counter(?MODULE, actionId, 1)
    %% catch
    %%  _ : _ ->
    %%      0
    %% end.

%%% ----------------------------------------------------------
%% actionId_get() ->
%%     ets:update_counter(?MODULE, actionId, 0).

%%% ----------------------------------------------------------
%%% #           cleanup()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: This function cleans up anything that needs cleaning up
%%% ----------------------------------------------------------

cleanup(#cleanupContext{action=Action}, Cause) ->
    case get(progress_timer) of
    undefined -> ok;
    Tref -> timer:cancel(Tref)
    end,
    case get(channelPid) of
    Pid when is_pid(Pid) ->
        case get(handle) of
        undefined ->
            ok;
        Handle ->
            ssh_sftp:close(Pid, Handle)
        end,
        ssh_sftp:stop_channel(Pid);
    undefined -> ok
    end,
    case get(connectionRef) of
    CRef when is_pid(CRef) -> ssh:close(CRef);
    undefined -> ok
    end,

    case get(countdown_timer) of
    undefined -> ok;
    TimerRef ->
        timer:cancel(TimerRef)
    end,
    try update_count(-1)
    catch _:_ -> ok
    end,
    try ets:delete(swmNewPaths) 
    catch _:_ -> ok
    end,
    %% HS51856
    try swmBackup:internal_housekeeping()
    catch T:E ->
        sysInitI:error_report(
          [{mfa, {swmBackup, internal_housekeeping, []}},
           {T,E},
           erlang:get_stacktrace()]),
        ok
    end,
    %% HS51856 ends here
    case get(fallback_timer) of
    undefined ->
        ok;
%%% HS23704 fix 
    FallbackTimer when Action == verify_upgrade_failed ->
        timer:cancel(FallbackTimer);    
%%% HS23704 fix ends here
    _ ->
        restart_node(cold, Cause)
    end,

    if
    Action =:= reboot ->
        restart_node(cold, Cause);
    Action =:= confirm_failed ->
        restart_node(cold, Cause);
    true->
        swmLib:uninstall_fallback(),
        %% ActiveKey = 
        swmModel:update_tables(),
        %% swmModel:set_active_version(ActiveKey),
        swmInventory:update_tables(),
        %% swmInventory:set_active_version(ActiveKey),
        %% HS87470
        swmLib:erase_ram_variable(activationStarted)
    end.


%%% ----------------------------------------------------------
%%% @doc Update progress info. The given keys are attribute
%%% names of the SwM MO. Some attributes are handled specially.
%%% The timeOfLastStatusUpdate attribute is updated automatically
%%% on each invocation.
%%% @end
%%% ----------------------------------------------------------

-spec update_progress([{atom(), string()|integer()}]) -> any().

update_progress(ProgressData) ->
    %% info_msg("Update progress: ~n~p~n",[ProgressData]),
    mnesia:transaction(fun() -> do_update_progress(swm, ProgressData) end).

update_progress(Key, ProgressData) ->
    mnesia:transaction(fun() -> do_update_progress(Key, ProgressData) end).
                    
do_update_progress(swm, ProgressData) ->
    [Obj] = mnesia:read({swM, {"1","1","1"}}),
    Old = case Obj#swM.reportProgress of
          undefined ->
          warning_msg("No default progress report!~n"),
          default_progress_report("swm", 0);
          PR -> PR
      end,
    Progress = swmLib:update(ProgressData, Old),
    mnesia:write(Obj#swM{reportProgress=Progress});

do_update_progress(Key, ProgressData) ->
    [Obj] = mnesia:read({upgradePackage, Key}),
    Old = case Obj#upgradePackage.reportProgress of
          undefined ->
          default_up_progress_report("up", 1);
          PR -> PR
      end,
    Progress = swmLib:update_up(ProgressData, Old),
    mnesia:write(Obj#upgradePackage{reportProgress=Progress}).
    


set_default_progress_report(Action, Id) ->
    mnesia:transaction(fun() ->
                   [Obj] = mnesia:read({swM, {"1","1","1"}}),
                   Progress = default_progress_report(Action, Id),
                   mnesia:write(Obj#swM{reportProgress=Progress})
               end).

set_default_up_progress_report(Key, Action, Id) ->
    mnesia:transaction(
      fun() ->
          [Obj] = mnesia:read({upgradePackage, Key}),
          Progress = default_up_progress_report(Action, Id),
          mnesia:write(Obj#upgradePackage{reportProgress=Progress})
      end).
    

default_progress_report(Action, Id) ->
    #'AsyncActionProgress'
    {actionName=Action,
     additionalInfo=[""],
     progressInfo = "Action started",
     progressPercentage=0,
     result=?ActionResultType_NOT_AVAILABLE,
     resultInfo="",
     state=?ActionStateType_RUNNING,
     actionId=Id,
     timeActionStarted = comsaI:iso_time(os:timestamp(), extended),
     timeActionCompleted= "",
     timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)}.
default_up_progress_report(Action, Id) ->
    #'AsyncActionProgressWithSteps'
    {actionName=Action,
     additionalInfo=[""],
     progressInfo = "Action started",
     progressPercentage=0,
     result=?ActionResultType_NOT_AVAILABLE,
     resultInfo="",
     state=?ActionStateType_RUNNING,
     actionId=Id,
     step=0,
     stepProgressPercentage=0,
     timeActionStarted = comsaI:iso_time(os:timestamp(), extended),
     timeActionCompleted= "",
     timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)}.




%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

open_dets(Name, Args) ->
    case dets:open_file(Name, [{auto_save,0}|Args]) of
    {ok, Name} ->
        {ok, Name};
    {error, Reason} ->
        sysInitI:error_report(
          [{?MODULE, open_dets},
           {mfa, {dets, open_file, [Name, Args]}},
           {error, Reason}])
    end.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
ets_new(TableName, Opts) ->
    case whereis(?MODULE) of
    Pid when Pid == self() ->
        catch ets:new(TableName, Opts),
        ok;
    Pid when is_pid(Pid) ->
        HeirData =
        [{tblName, TableName}, {tblOpts, Opts} | process_info(self())],
        catch ets:new(TableName, Opts ++ [{heir, Pid, HeirData}]),
        ok;
    undefined ->
        {error, server_process_not_found}
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

-spec state(atom()|integer()) -> integer()|string().

state(initialized) -> ?UpgradePackageState_INITIALIZED;
state(prepareInProgress) -> ?UpgradePackageState_PREPARE_IN_PROGRESS;
state(prepareCompleted) -> ?UpgradePackageState_PREPARE_COMPLETED;
state(activationInProgress) -> ?UpgradePackageState_ACTIVATION_IN_PROGRESS;
state(activationStepCompleted) ->
    ?UpgradePackageState_ACTIVATION_STEP_COMPLETED;
state(waitingForCommit) -> ?UpgradePackageState_WAITING_FOR_COMMIT;
state(commitCompleted) -> ?UpgradePackageState_COMMIT_COMPLETED;
state(deactivationInProgress) -> ?UpgradePackageState_DEACTIVATION_IN_PROGRESS;

state(?UpgradePackageState_INITIALIZED) -> "INITIALIZED";
state(?UpgradePackageState_PREPARE_IN_PROGRESS) -> "PREPARE_IN_PROGRESS";
state(?UpgradePackageState_PREPARE_COMPLETED) -> "PREPARE_COMPLETED";
state(?UpgradePackageState_ACTIVATION_IN_PROGRESS) -> "ACTIVATION_IN_PROGRESS";
state(?UpgradePackageState_ACTIVATION_STEP_COMPLETED) ->
    "ACTIVATION_STEP_COMPLETED";
state(?UpgradePackageState_WAITING_FOR_COMMIT) -> "WAITING_FOR_COMMIT";
state(?UpgradePackageState_COMMIT_COMPLETED) -> "COMMIT_COMPLETED";
state(?UpgradePackageState_DEACTIVATION_IN_PROGRESS) ->
    "DEACTIVATION_IN_PROGRESS".

%%% os:cmd wrapper with printout
cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), Res = os:cmd(Cmd)]),
    Res.

handle_cancel_after_activate(Key) ->
    Fun = fun() -> do_handle_cancel_after_activate(Key) end,
    case mnesia:transaction(Fun) of
    {atomic, goahead} ->
        CompleteTime = comsaI:iso_time(os:timestamp(), extended),
        %% HS76784
        update_progress(Key,[{result, ?ActionResultType_FAILURE},
                 {state, ?ActionStateType_CANCELLED},
                 {progressPercentage, 100},
                 {timeActionCompleted, CompleteTime}]),
        fallback(Key, 
             cancel, 
             "The activation was cancelled by the user");
    {atomic, busy} ->
        busy
    end.

do_handle_cancel_after_activate(Key) ->
    case mnesia:read({upgradePackage, Key}) of
    [] ->
        busy;
    [UP] ->
        case UP#upgradePackage.state of
        ?UpgradePackageState_WAITING_FOR_COMMIT ->
            mnesia:write(UP#upgradePackage{state=state(commitCompleted)}),
            goahead;
        _ ->
            busy
        end
    end.

%%% ----------------------------------------------------------
%%% @doc Perform fallback. This function is called directly by the
%%% 'cancel' action, and also from timers created in 'activate',
%%% in the init phase of swmServer when an upgrade restart is
%%% taking place, and while waiting for the 'confirm' in upgrade.
%%%
%%% The given UP key refers to the To-UP of the upgrade operation
%%% in progress.
%%% @end
%%% ----------------------------------------------------------

-spec fallback(any(), fallbackContext(), string()) -> any().

fallback(UpKey, Context, Msg) ->
    case Context of 
    data_conversion ->
        NotReceived = gmfImmUgInserter:getNotReceived(),
        error_msg("data conversion taking too long, "
              "classes not handled: ~p~n",
              [ordsets:to_list(NotReceived)]);
    activating_before_restart ->
        %% HU24092 Catch potentially hanging trigger commands
        %% These are potentially long messages, but it is nevertheless
        %% necessary
        error_logger:info_report(
          [{P,erlang:port_info(P)}||P<-erlang:ports()]),
        error_logger:info_msg("ps -ef ~n",[]),
        error_logger:info_msg("~s~n",[os:cmd("ps -ef ")]),
        ok;
    
    _->
        ok
    end,
    try do_fallback(UpKey, Msg)
    catch T:E ->
        sysInitI:error_report(
          [{?MODULE, fallback, [UpKey, Msg]},
           {T,E},
           erlang:get_stacktrace()])
    end.

do_fallback(Key, Msg) ->
    swmLib:write_swm_log(sender(Key), info, Msg),
    Fun = fun() ->
          [Obj] = mnesia:read({upgradePackage, Key}),
          mnesia:write(
            Obj#upgradePackage{state=state(deactivationInProgress)})
      end,
    {atomic, ok} = mnesia:transaction(Fun),
    info_msg("Fallback timer: ~s~n",[Msg]),
    %% HT47289 Make sure reason gets written to log
    swmLib:sync(),
    %% Fix ends here
    restart_node(cold, ?ALH_TAG_UpgradeTimeout).

cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    timer:cancel(TimerRef).

send_fallback_alarm(TimeRemaining, {ME, SF, SwM, Up}) ->
    try do_send_fallback_alarm(TimeRemaining, {ME, SF, SwM, Up})
    catch T:E ->
        sysInitI:error_report(
          [{?MODULE, send_fallback_alarm, [TimeRemaining, {ME,SF,SwM,Up}]},
           {T,E},
           erlang:get_stacktrace()])
    end.

do_send_fallback_alarm(TimeRemaining, {ME, SF, SwM, Up}) ->
    Dn = [list_to_binary(X)||X<-["ManagedElement="++ME,
                 "SystemFunctions="++SF,
                 "SwM="++SwM,
                 "UpgradePackage="++Up]],

    Msg = lists:flatten(
        io_lib:format(
          "The upgrade operation has not been confirmed yet. "
          "A fallback operation will be started in ~w seconds if the "
          "upgrade is not confirmed.",[TimeRemaining])),
    %% HT47289 Log in SwmLog that alarm is raised
    swmLib:write_swm_log(
      "SwM", alert, 
      "Raising the 'FallbackOperationStartingSoon' alarm. Time remaining: "++
      integer_to_list(TimeRemaining)),
    comsaI:send_alarm('FallbackOperationStartingSoon', warning, Dn, Msg).

countdown(FallbackTimeout, Now) ->
    try do_countdown(FallbackTimeout, Now)
    catch T:E ->
        sysInitI:error_report(
          [{?MODULE, countdown, [FallbackTimeout, Now]},
           {T,E},
           erlang:get_stacktrace()])
    end.

do_countdown(FallbackTimeout, Now) ->
    Elapsed = time_elapsed(os:timestamp(), Now),
    Remaining = FallbackTimeout - Elapsed,
    case Remaining of
    Remaining when Remaining > -1 ->
        update_count(Remaining);
    _ ->
        update_count(-1)
    end.

update_count(Count) ->
    swmLib:set_ram_variable(timeRemainingBeforeFallback, Count).

time_elapsed(Now, Start) ->
    timer:now_diff(Now, Start) div 1000000.

%% Check if the state transition is allowed, and if so update the state
set_up_state(Key, WantedState) ->
    Fun = fun() -> db_set_up_state(Key, WantedState) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

db_set_up_state(Key, Wanted) ->
    [Obj] = case mnesia:read({upgradePackage, Key}) of
        [O] -> [O];
        [] -> mnesia:abort({no_such_up, Key})
        end,
    case {Wanted, Obj#upgradePackage.state} of
    {prepareInProgress, ?UpgradePackageState_INITIALIZED} ->
        do_state_change(Obj, Wanted);
    {activationInProgress, ?UpgradePackageState_PREPARE_COMPLETED} ->
        case swmLib:get_ram_variable(activationStarted) of
        undefined ->
            swmLib:set_ram_variable(activationStarted, Key),
            do_state_change(Obj, Wanted);
        _UpKey ->
            %% An activation is already ongoing, abort this one
            {error, "An activation is already ongoing, this action will be ignored"}
        end;
    {_, State} ->
        {error, {wrong_state, State}}
    end.

do_state_change(Obj, Wanted) when is_record(Obj, upgradePackage) ->
    NewObj = Obj#upgradePackage{state = state(Wanted)},
    mnesia:write(NewObj),
    {ok, NewObj};
do_state_change(Key, Wanted) ->
    [UP] = mnesia:read({upgradePackage, Key}),
    do_state_change(UP, Wanted).

%%% ----------------------------------------------------------
restart_node(Rank, AvliCause) ->
    swmLib:activate_upgrWindow_tables(),
    appmI:restart_node(Rank, AvliCause).


%%% ----------------------------------------------------------
%%% #           check_up_states_for_restore()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Restore backup is not allowed, while an UP is in a transient
%%%              state
%%% ----------------------------------------------------------

%% HT12440 fix main part
check_up_states_for_restore() ->
    Fun = fun() -> do_check_up_states_for_restore() end,
    case mnesia:transaction(Fun) of
    {atomic, Result} ->
        Result;
    {aborted, Reason} ->
        erlang:error({aborted, Reason}, [])
    end.

do_check_up_states_for_restore() ->
    do_check_up_states_for_restore(mnesia:all_keys(upgradePackage)).

do_check_up_states_for_restore([Key|Keys]) ->
    [UP] = mnesia:read({upgradePackage, Key}),
    UnallowedStates = [?UpgradePackageState_ACTIVATION_IN_PROGRESS,
               ?UpgradePackageState_ACTIVATION_STEP_COMPLETED,
               ?UpgradePackageState_WAITING_FOR_COMMIT,
               ?UpgradePackageState_DEACTIVATION_IN_PROGRESS],
    case lists:member(UP#upgradePackage.state, UnallowedStates) of
    true ->
        {error, {element(4, Key), state(UP#upgradePackage.state)}};
    false ->
        do_check_up_states_for_restore(Keys)
    end;
do_check_up_states_for_restore([]) ->
    ok.

%% Fix ends here


%%% ----------------------------------------------------------
%%% #           start_channel(Host, Port, User, Password)
%%% Input:
%%% Output: Handle- sftp file handle
%%% Exceptions: throw(Error) as returned from ssh_sftp:start_channel/3
%%% Description: Open a sftp connection and start a channel
%%%              Store info as process variables for graceful shutdown
%%%              when there are problems
%%% ----------------------------------------------------------


start_channel(Host, Port, User, Password) ->
    case sysSftp:start_channel(Host, Port, User, Password) of
    {ok, SP, C} ->
        info_msg("~p~n",[ssh:connection_info(
                   C, [client_version, server_version, peer])]),
        put(channelPid, SP),
        put(connectionRef, C),
        {ok, SP, C};
    {error, E1} ->
        Info1 =
        case E1 of
            etimedout ->
            "Cannot establish a connection to remote server";
            closed -> 
            "The remove server closed the connection";
            String when is_list(String) ->
            String;
            _ ->
            lists:flatten(io_lib:format("~p",[E1]))
        end,
        update_progress([{resultInfo, Info1}]),
        throw(E1)
    end.


%%% ----------------------------------------------------------
%%% #           sftp_list_dir(ChannelPid, Path, Timeout)
%%% Input: ChannellPid::pid() - From sftp
%%%        Path::string() - The directory path
%%%        Timeout::integer() - Timeout in millisecs
%%% Output:
%%% Exceptions:
%%% Description: Wraps the ssh_sftp:list_dir/3 function, dropping the
%%% "." and ".." directory names from a filename listing.
%%% end
%%% ----------------------------------------------------------
-spec sftp_list_dir(pid(), string(), integer()) -> {ok, [string()]}.

%% HT85211 Better error handling
sftp_list_dir(ChannelPid, Path, TimeoutMillis) ->
    case ssh_sftp:list_dir(ChannelPid, Path, TimeoutMillis) of
    {ok, Listing} ->
        {ok, Listing--[".", ".."]};
    {error, permission_denied} ->
        throw({fail, "Permission denied"});
    {error, timeout} ->
        throw({fail,"The server stopped sending data when reading "++Path});
    {error, Reason} ->
        throw({fail, "SFTP error: "++make_string_flat(Reason)})
    end.

sftp_open(ChannelPid, Path, Opts) ->
    case ssh_sftp:open(ChannelPid, Path, Opts) of
    {ok, Handle} ->
        {ok, Handle};
    {error, permission_denied} ->
        throw({fail, "Permission denied"});
    {error, timeout} ->
        throw({fail,"The server stopped sending data when opening "++Path});
    {error, Reason} ->
        throw({fail, "SFTP error: "++make_string_flat(Reason)})
    end.

%% HU14586
%% Use upgrade marker as failsafe for detecting failed upgrades

upgrade_marker_path() ->
    filename:join(swmLib:swm_dir(), "upgrade_marker").

%% HU29568
%% Upgrade marker is no longer needed
%% Erase function retained for backwards compatibility reasons

%% set_upgrade_marker() ->
%%     ok = file:write_file(upgrade_marker_path(), <<"abcd">>).

erase_upgrade_marker() ->
    case file:delete(upgrade_marker_path()) of
    ok ->
        ok;
    {error, enoent} ->
        ok
    end.

%% is_upgrade_marker() ->
%%     filelib:is_file(upgrade_marker_path()).

%%% ----------------------------------------------------------
%%% #           unlock_action_capable_after_restart()
%%% Input: 
%%% Output:
%%% Exceptions:
%%% Description: Unlocks action capable after restart
%%% ----------------------------------------------------------
unlock_action_capable_after_restart() ->
    swmLib:erase_variable(action_capable_lock),
    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
    mnesia:dirty_write(SwM#swM{actionCapable = ?ActionCapabilityState_CAPABLE,
                               actionCapableInfo = undefined}),
    ok.


%%% ----------------------------------------------------------
%%% #           lock_action_capable_after_upgrade()
%%% Input: 
%%% Output:
%%% Exceptions:
%%% Description: Locks action capable after upgrade to enable
%%%              upgrading from older versions that do not 
%%%              support actionCapability locking
%%% ----------------------------------------------------------
lock_action_capable_after_upgrade() ->
    case swmLib:get_variable(action_capable_lock) of
        undefined ->
            swmLib:lock_action_capable(?ACTIVATE_UP_ACTION_CAPABLE_ID,
                                       ?ACTIVATE_UP_ACTION_CAPABLE_INFO);
        _ ->
            ok
    end.

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions: A badmatch occurs if the element does not exist
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

-spec find_optional_element(ElementName::atom(), 
                Element::#xmlElement{}|list()) -> 
                   #xmlElement{} | false.

find_optional_element(ElementName, Element) 
  when is_record(Element, xmlElement) ->
    ContentList = Element#xmlElement.content,
%%     find_element(ElementName, );
%% find_optional_element(ElementName, ContentList) ->
    case lists:keysearch(ElementName, #xmlElement.name, ContentList) of
    {value, Value}  ->
        Value;
    false ->
        false
    end.


%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions: erlang:error invoked if attribute does not exist
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
    Text#xmlText.value.

%%% ----------------------------------------------------------
make_string(Term) ->
    make_string("~p", Term).

make_string_flat(Term) ->
    make_string("~s", Term).

make_string(Format, Term) ->
    try lists:flatten(io_lib:format(Format, [Term]))
    catch
    _ : _ ->
        lists:flatten(io_lib:format("~p", [Term]))
    end.


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%even_more_internal_function1(One, Two)->
%   nnn.

info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format) ->
    error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format) ->
    warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%%% TEST CASE SUPPORT


%%% HU59680 Move to separate process to avoid messages getting queued up in
%%% swmServer
%%% Needed for swm_basic_test_suite
start_test_event_server() ->
    proc_lib:spawn(
      fun() ->
          register(swmTestEventServer, self()),
          mnesia:subscribe({table, swM, detailed}),
          mnesia:subscribe({table, upgradePackage, detailed}),
          test_event_server_loop()
      end).

stop_test_event_server() ->
    swmTestEventServer!stop.

test_event_server_loop() ->
    receive
    {mnesia_table_event, Event} ->
        case swmLib:get_variable(swm_basic_tests_return_pid) of
        Pid when is_pid(Pid) -> Pid!{mnesia_table_event, Event};
        _ -> ok
        end,
        test_event_server_loop();
    stop ->
        ok
    end.



test_download_packages(Up, {file, _, "localhost", _, RemoteFile, _}) ->
    Uri = Up#upgradePackage.uri,
    ArchiveDir = get_up_archive_dir(Up#upgradePackage.administrativeData), 

    {ok, FIO} = file:read_file_info(RemoteFile),
    RemoteDir = case FIO#file_info.type of
            directory ->
            RemoteFile;
            _ ->
            filename:dirname(RemoteFile)
        end,


    %% We only handle files in a flat directory.
    {ok, Files} = file:list_dir(RemoteDir),
    info_msg("Uri: ~p~nRemoteDir: ~p~nFiles: ~p~n",[Uri , RemoteDir, Files]),
    %% Set up progress reporting by calculating the total size
    %% of all files in the directory
    FileSizes =
    [begin
         Path = filename:join(RemoteDir, File),
         {ok, ThisFIO} = file:read_file_info(Path),
         case ThisFIO#file_info.type of
         regular ->
             case filename:extension(File) of
             ".xml" -> {Path, 0};
             _ ->
                 {Path, ThisFIO#file_info.size}
             end;
         _ ->
             {Path, 0}
         end
     end||File<-Files],

    info_msg("File sizes: ~n~p~n",[FileSizes]),
    TotalSize = lists:foldl(fun({_, S}, A) -> S+A end, 0, FileSizes),
    info_msg("TotalSize: ~p~n",[TotalSize]),
    {ok, Tref} = timer:send_interval(1000, update_progress),
    put(progress_timer, Tref),

    lists:foldl(
      fun({_Path, 0}, AccuSize) ->
          AccuSize;
     ({Path, Size}, AccuSize) ->
          Basename = filename:basename(Path),
          Key = Up#upgradePackage.upgradePackageId,
          update_progress(Key, [{additionalInfo,"Downloading "++Basename}]),
          LocalPath = filename:join(ArchiveDir, Basename),
          ok = file:make_link(Path, LocalPath),
          receive
          {cancel, _} ->
              throw(cancelled)
          after 0 -> ok
          end,
%         spawn(swmOs, mount_cxp, [LocalPath]),
          Size + AccuSize
      end, 0, FileSizes),
    timer:cancel(Tref),
    ok.
    
    
    

    
                   

%% erl_call(N, M, F, A) ->
%%     OtpRoot = os:getenv("OTP_ROOT"),
%%     Pattern = filename:join(
%%      [OtpRoot, "lib", "erl_interface-*", "bin","erl_call"]),
%%     [ErlCall] = filelib:wildcard(Pattern),
%%     sysOs:cmd(ErlCall++" -c bs_SIS_1 -sname "++
%%     make_string("~w -a '~w ~w ~w'", [N, M, F, A])).



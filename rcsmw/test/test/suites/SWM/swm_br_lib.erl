%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_br_lib.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/2
%%% 
%%% @doc == support lib when testing bacup, restore mechanism. ==
%%% <br/>
%%%
%%% 
%%% @end

-module(swm_br_lib).
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2014-03-13 etxivri     Created
%%% R2A/3      2014-03-17 etxivri     Update to handle if old and new actionId
%%%                                   is same. Could be same after node restart.
%%% R2A/5      2014-04-01 etxivri     Added function used for failsafe and 
%%%                                   schedule bu. Also cleanup.
%%% R2A/6      2014-06-27 etxivri     Added coding: latin-1
%%% R2A/7      2014-08-21 etxivri     Added new fuctions.
%%% R3A/1      2015-02-19 etxivri     Update to remove error printouts 
%%%                                   in ct shell.
%%% R3A/2      2015-06-04 etxivri     Update for secured boards.
%%% R4A/1      2015-09-02 etxivri     Update to be more robust.
%%% R6A/1      2016-07-11 etxivri     Update get_action_id.
%%% R7A/1      2016-11-14 etxivri     Add clear_system_bu using coli.
%%% R8A/1      2016-11-30 etxivri     Add clear SITE_CONFIG_COMPLETE bu.
%%% R8A/1      2016-12-13 etxivri     check after restore more robust.
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).

-export([
	 create_backup/3,
	 create_backup_successful/4,
	 delete_backup/3,
	 delete_backup_successful/4,
	 export_backup/5,
	 import_backup/4,
	 restore_backup/3,
	 restore_backup_successful/5,
	 conf_single_schedule_bu/5,
	 activate_failsafe_bu/2,
	 deactivate_failsafe_bu/2,
	 get_correct_buid/2,
	 get_action_id/2,
	 get_action_id/3,
	 wait_for_expecting_state/4,
	 wait_for_expecting_state/5,
	 wait_for_expecting_state/6,
	 %% check_progress_elements/1,
	 %% check_progress_elements/2,
	 %% get_result_info/2,
	 get_progress_report/2,
	 get_progress_report/3,
	 get_all_backups/2,
	 set_failsafe_attribute/4,
	 add_time/2,
	 iso_time/2,
	 clear_system_bu/1,
	 clear_scc_bu_cli/1
	]).


-define(MANAGED_ELEMENT, 'ManagedElement').
-define(COM_TOP, [{xmlns,"urn:com:ericsson:ecim:ComTop"}]).
%% -define(ELEMENT_ID_1, {managedElementId,[],["1"]}).
-define(ELEMENT_ID_1, {managedElementId,[],[MeId]}).
-define(SYSTEMFUNCTIONS, 'SystemFunctions').
-define(SYSTEMFUNCTIONS_ID_1, {systemFunctionsId,[],["1"]}).
-define(BRM, 'BrM').
-define(BRM_ID, {brMId,[],["1"]}).
-define(BRMBACKUPMANAGER, 'BrmBackupManager').
-define(BRMBACKUPMANAGER_ID, {brmBackupManagerId,[],["1"]}).

-define(REC_BRM_XLMNS, [{xmlns,"urn:com:ericsson:ecim:RcsBrM"}]).

-define(BRM_MO, {?MANAGED_ELEMENT,
		 ?COM_TOP,
		 [?ELEMENT_ID_1,
		  {?SYSTEMFUNCTIONS,
		   [?SYSTEMFUNCTIONS_ID_1,
		    {?BRM,
		     [?BRM_ID,
		      {?BRMBACKUPMANAGER,
		       [?BRMBACKUPMANAGER_ID]}
		     ]}]}]} ).


%%% ===========================================================================
%%% @doc
%%% Clear system BUs <br/>
%%% @spec clear_system_bu(Coli) -> ok
%%% @end
%%% ==========================================================================
clear_system_bu(Coli) ->
    ct:log("## cleanup system created BUs, coli swm/housekeep", []),
    ok = rct_coli:connect(Coli),
    {ok,Answer} = rct_coli:send(Coli,"/swm/housekeep"),
    ok = rct_coli:disconnect(Coli),
    timer:sleep(1000),
    ct:log("## coli answer: ~p", [Answer]),
    timer:sleep(30000),
    ok.

%%% ===========================================================================
%%% @doc
%%% Clear SITE_CONFIG_COMPLETE backup <br/>
%%% @spec clear_scc_bu_cli(Cli) -> ok
%%% @end
%%% ==========================================================================
clear_scc_bu_cli(Cli) ->
    ct:log("## clear SITE_CONFIG_COMPLETE backup by setting rbsConfigLevel=READY_FOR_SERVICE using cli.", []),
    rct_cli:connect(Cli),
    rct_coli:send(Cli,"configure"),
    rct_coli:send(Cli,"ManagedElement=1,NodeSupport=1,AutoProvisioning=1"),
    rct_coli:send(Cli,"rbsConfigLevel=READY_FOR_SERVICE"),
    {ok,Answer} = rct_coli:send(Cli,"commit"),
    rct_coli:disconnect(Cli),
    timer:sleep(1000),
    ct:log("## cli answer: ~p", [Answer]),
    ok.

%%% ===========================================================================
%%% @doc
%%% Create a backup <br/>
%%% - Name = string() <br/>
%%% @spec create_backup(NC_Session, MeId, Name) -> ok
%%% @end
%%% ===========================================================================
create_backup(NC_sess, MeId, Name)->
    ct:pal("Executing action createBackup: ~p", [Name]),
    bu_action(NC_sess, 'createBackup', MeId, Name),
    ok.

create_backup_successful(NC_sess, MeId, Name, ActionId) ->
    ct:pal("Executing action createBackup: ~p", [Name]),
    bu_action(NC_sess, 'createBackup', MeId, Name),

    case wait_for_expecting_state(NC_sess, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  "CREATE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  "FINISHED", 
	  ProgressReport}] ->
	    ct:pal("Backup succesfully created.~n~p", [ProgressReport]),
	    ok;
	_ErrRes ->
	    ct:pal("createBackup: ~p~n",[_ErrRes]),
	    ct:fail(_ErrRes)
    end.


%%% ===========================================================================
%%% @doc
%%% delete a backup <br/>
%%% - Name = string() <br/>
%%% @spec delete_backup(NC_Session, MeId, Name) -> ok
%%% @end
%%% ===========================================================================
delete_backup(NC_sess, MeId, Name) ->    
    ct:pal("Executing action deleteBackup ~s~n",[Name]),
    bu_action(NC_sess, 'deleteBackup', MeId, Name),
    ok.


delete_backup_successful(NC_sess, MeId, Name, ActionId) ->    
    ct:pal("Executing action deleteBackup ~s~n",[Name]),
    bu_action(NC_sess, 'deleteBackup', MeId, Name),

   case wait_for_expecting_state(NC_sess, MeId, ActionId, "FINISHED") of
    	[{"SUCCESS", 
    	  "DELETE", 
    	  _ProgressInfo, 
    	  _ResultInfo, 
    	  _State, 
    	  ProgressReport}] ->
    	    ct:pal("Backup deleted successfully.~n~p", [ProgressReport]),
    	    ok;
    	 _ErrRes ->
    	    ct:pal("deleteBackup: ~p~n",[_ErrRes]),
    	    ct:fail(_ErrRes)
    end.


%%% ===========================================================================
%%% @doc
%%% export a backup <br/>
%%% - BuId = BackupId,  exampel ["1"] .  <br/>
%%% @spec export_backup(NC_Session, MeId, BuId, Uri, Password) -> ok
%%% @end
%%% ===========================================================================
export_backup(NC_sess, MeId, BuId, Uri, Password) -> 
    Action = construct_nc_message(MeId, {'BrmBackup', 
					 [{brmBackupId, BuId},
					  {'export',[],
					   [{uri, [], [Uri]},
					    {password, [Password]}]}]} ),

    ct:pal("Executing action export, brmBackupId : ~p ~n",[BuId]),
    Ares = netconf(NC_sess, action, [Action]),
    ct:pal("Ares: ~p",[Ares]),
    case Ares of
	{ok, _} -> ok;
	Error ->
	    ct:pal("backup Export action failed: ~p~n",[Error]),
	    ct:fail(Error)
    end,
    ok.

%%% ===========================================================================
%%% @doc
%%% Import a backup <br/>
%%% @spec import_backup(NC_Session, MeId, Uri, Password) -> ok
%%% @end
%%% ===========================================================================
import_backup(NC_sess, MeId, Uri, Password) -> 
    Action = construct_nc_message(MeId, {'importBackup',[],
					 [{uri, [], [Uri]},
					  {password, [], [Password]}]} ),

    ct:pal("Executing action importBackup"),
    {ok, _A} = netconf(NC_sess, action, [Action]),

    ok.

%%% ===========================================================================
%%% @doc
%%% restore a backup <br/>
%%% - BuId = BackupId,  exampel ["1"] .  <br/>
%%% @spec restore_backup(NC_Session, MeId, BuId) -> ok
%%% @end
%%% ===========================================================================
restore_backup(NC_sess, MeId, BuId) -> 
    Action = construct_nc_message(MeId, {'BrmBackup', 
					 [{brmBackupId, [], BuId},
					  {restore, []}]} ),

    ct:pal("Executing action restore on backup: ~p", [BuId]),
    {ok, ActionResponse} = netconf(NC_sess, action, [Action]),
    ct:pal("#### : ~p", [ActionResponse]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} -> 
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    
    ok.

restore_backup_successful(NC_sess, MeId, BuId, ActionId, BuId) -> 
    restore_backup(NC_sess, MeId, BuId),
    
    %% %%%%
    %% %% Get BuId that belongs to BU_name.
    %% %%%%
    %% BuId = get_correct_buid(?BU_Name_1, Backups), %% in a list.
    %% ct:pal("Selected backup ~p to restore.~n",[BuId]),

    ct_telnet:expect(console, "Ericsson",
		     [{timeout,60000}, no_prompt_check]),

    case wait_for_expecting_state(NC_sess, 
				  MeId, 
				  ActionId, 
				  "FINISHED", 
				  {brmBackup, BuId}) of
	[{"SUCCESS", 
	  "RESTORE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  ProgressReport}] ->
	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("restoreBackup: ~p~n",[ErrResult]),
	    ct:fail(ErrResult)
    end.

%%% ===========================================================================
%%% @doc
%%% Will get the buId where the backup name exist. <br/>
%%% Returns the buId in a list. <br/>
%%% - BuName = str(). Name of the backup <br/>
%%% - AllBackupList = list of data of the brmBackupId. <br/>
%%% @spec get_correct_buid(BuName, AllBackupList) -> ok
%%% @end
%%% ===========================================================================
get_correct_buid(BuName, AllBackupList) ->
    List = lists:dropwhile(fun({_BrmBackup, _, Data}) ->
				   case lists:keysearch(backupName, 1, Data) of
				       {value,{backupName,[],[BuName]}} ->
				   	   false;
				       _ ->
				   	   true
				   end
			   end, AllBackupList),
    ct:pal("BuIdList: ~p", [List]),
    [WantedList | _] = List,
    {_BrmBackup, _, WantedData} = WantedList,
    {value,{brmBackupId,[],[BuId]}} = 
	lists:keysearch(brmBackupId, 1, WantedData),
    [BuId].


%%% ===========================================================================
%%% @doc
%%% Configure a single event to occur XX seconds from now<br/>
%%% AddTime = integer(), seconds to start backup.
%%% - SchedId = BackupId,  exampel ["1"] .  <br/>
%%% @spec conf_single_schedule_bu(NC_sess, MeId, Name, AddTime, SchedId) -> ok
%%% @end
%%% ===========================================================================
conf_single_schedule_bu(NC_sess, MeId, Name, AddTime, SchedId) ->
    TS = timestamp(),
    FutureTime = add_time(AddTime, TS),
    LegalTime = iso_time(FutureTime, extended),

    Set = construct_nc_message(MeId, {'BrmBackupScheduler',
				      [{brmBackupSchedulerId,[], [SchedId]},
				       {scheduledBackupName, [Name]},
				       {'BrmSingleEvent', [],
					[{brmSingleEventId, [Name++"_sct"]},
					 {scheduledTime,[LegalTime]}]}]} ),
    
    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    ok = netconf(NC_sess, edit_config, [running, Set]),
    set_admin_state(NC_sess, MeId, "UNLOCKED", SchedId),
    timer:sleep(5000),
    ct:pal("Announced time is ~p~n", [get_next_scheduled_time(NC_sess, 
							      MeId, 
							      SchedId)]),

    ok.

%%% ===========================================================================
%%% @doc
%%% Activate failsafe backup. <br/>
%%% @spec activate_failsafe_bu(NC_sess, MeId) -> ok
%%% @end
%%% ===========================================================================
activate_failsafe_bu(NC_sess, MeId) ->
    Activate = construct_nc_message(MeId, {'BrmFailsafeBackup',
					   [{brmFailsafeBackupId, [], ["1"]},
					    {'activate',[]}]} ),

    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(NC_sess, action, [Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} -> 
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    ok.

%%% ===========================================================================
%%% @doc
%%% Deactivate failsafe backup. <br/>
%%% @spec deactivate_failsafe_bu(NC_sess, MeId) -> ok
%%% @end
%%% ===========================================================================
deactivate_failsafe_bu(NC_sess, MeId) ->
    Deactivate = construct_nc_message(MeId, {'BrmFailsafeBackup',
					     [{brmFailsafeBackupId, [], ["1"]},
					      {'deactivate',[]}]} ),
    ct:pal("Executing action deactivate"),

    {ok, ActionResponse} = netconf(NC_sess, action, [Deactivate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} -> 
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    ok.

%%% ===========================================================================
%%% @doc
%%% get action id <br/>
%%% Two differents actionIds can be read. <br/>
%%% - brmBackupManager or brmBackup <br/>
%%% @spec get_action_id(NC_sess, MeId) -> ok
%%% @end
%%% ===========================================================================
get_action_id(NC_sess, MeId) ->
    %% ct_netconfc:open(NC_sess, [{timeout, 30000}]),
    get_action_id(NC_sess, MeId, {brmBackupManager, dummy}).


get_action_id(NC_sess, MeId, WhichBU) ->
    case WhichBU of
	{brmBackup, _BuId} ->
	    Progress_Report = get_progress_report(NC_sess, MeId, WhichBU);
	{brmBackupScheduler, _Id} ->
	    Progress_Report = get_progress_report(NC_sess, MeId, WhichBU);
	{brmFailsafeBackup, _Id} ->
	    Progress_Report = get_progress_report(NC_sess, MeId, WhichBU);
	_ ->
	   Progress_Report =  get_progress_report(NC_sess, MeId)
    end,
	    
    %% ct:pal("## Progress_Report: ~p ",[Progress_Report]),
    case Progress_Report of
	{progressReport, [{struct,"AsyncActionProgress"},
			  {unset, "true"}], []} ->
	    ActionId = undefined;
	{progressReport, [{unset, "true"}], []} ->
	    ActionId = undefined;
	{progressReport,
	 [{struct,"AsyncActionProgress"}],
	 ProgressReport} ->
	    {actionId,_,[ActionId]} = 
		lists:keyfind(actionId, 1, ProgressReport),
	    ActionId
    end,
    
    %% ct_netconfc:close_session(NC_sess, 30000),
    ct:pal("### ActionId: ~p", [ActionId]),
    
    ActionId.

%%% ===========================================================================
%%% ===========================================================================
netconf_open(Session, Param)->
    case aic_httpc:check_if_vc_board()  of
	"yes" -> ct_netconfc:open(Session, 
				  [{user, "SysAdminTest"},
				   {password, "SysAdminTest"}|Param]);
	_ ->ct_netconfc:open(Session,Param)
    end.
%%% ===========================================================================
%%% @doc
%%% get progress report <br/>
%%% Differents progress report can be read. <br/>
%%% - example brmBackupManager or brmBackup <br/>
%%% @spec get_progress_report(NC_sess, MeId) -> ok
%%% @end
%%% ===========================================================================
get_progress_report(NC_sess, MeId) ->
    get_progress_report(NC_sess, MeId, {brmBackupManager, dummy}, 300000).
get_progress_report(NC_sess, MeId, WhichBU) ->
    get_progress_report(NC_sess, MeId, WhichBU, 300000).
get_progress_report(NC_sess, _MeId, WhichBU, Timeout) when Timeout < 0 ->
    ct_netconfc:close_session(NC_sess, 30000),
    ct:pal("BU: ~p",[WhichBU]),
    ct:fail("No progress report rceived within max timeout.");
get_progress_report(NC_sess, MeId, WhichBU, Timeout) ->
    A = ct_netconfc:open(NC_sess, [{timeout, 30000}]),
    ct:log("# A # : ~p",[A]),

    %% netconf_open(NC_sess, [{timeout, 30000}]),

    case WhichBU of
	{brmBackup, BuId} ->
	    NC_Message = 
		construct_nc_message(MeId, {'BrmBackup',[],
					    [{brmBackupId,[],BuId},
					     {progressReport, []}]}),
	    case ct_netconfc:get(NC_sess, NC_Message) of
		{ok, [Ans]} ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ReportProgress = 
			get_progress(MeId, {brmBackup, BuId}, Ans),
		    ct:pal("# ReportProgress # : ~p",[ReportProgress]),
		    %% ct_netconfc:close_session(NC_sess, 30000),
		    ReportProgress;
		_Err ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ct:log("# No brmBackup progress report recived # : ~p ~n"
			   "# Check nc session #. ~n",[_Err]),
		    timer:sleep(5000),
		    %% swm_test_lib:check_nc_session(NC_sess),
		    get_progress_report(NC_sess, MeId, WhichBU, Timeout-5000)
	    end;
	{brmBackupScheduler, Id} ->
	    NC_Message = 
		construct_nc_message(MeId, {'BrmBackupScheduler',
					    [{brmBackupSchedulerId, [], [Id]},
					     {progressReport, []}]}),
	    case ct_netconfc:get(NC_sess, NC_Message) of
		{ok, [Ans]} ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ProgressReport = 
			get_progress(MeId, {brmBackupScheduler, Id}, Ans),
		    ct:log("# ProgressReport: ~p",[ProgressReport]),
		    %% ct_netconfc:close_session(NC_sess, 30000),
		    ProgressReport;
		_Err ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ct:log("# No brmBackupScheduler progress report "
			   "recived: ~p ~n"
			   "# Check nc session. ~n",[_Err]),
		    timer:sleep(5000),
		    %% swm_test_lib:check_nc_session(NC_sess),
		    get_progress_report(NC_sess, MeId, WhichBU, Timeout-5000)
	    end;
	{brmFailsafeBackup, Id} ->
	    NC_Message = 
		construct_nc_message(MeId, {'BrmFailsafeBackup',
					    [{brmFailsafeBackupId, [], [Id]},
					     {progress, []}]}),
	    case ct_netconfc:get(NC_sess, NC_Message) of
		{ok, [Ans]} ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ProgressReport = 
			get_progress(MeId, {brmFailsafeBackup, Id}, Ans),
		    ct:log("# Progress: ~p",[ProgressReport]),
		    ct_netconfc:close_session(NC_sess, 30000),
		    ProgressReport;
		_Err ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ct:log("# No BrmFailsafeBackup progress report "
			   "recived: ~p ~n"
			   "# Check nc session. ~n",[_Err]),
		    timer:sleep(5000),
		    %% swm_test_lib:check_nc_session(NC_sess),
		    get_progress_report(NC_sess, MeId, WhichBU, Timeout-5000)
	    end;
	{brmBackupManager, _} ->
	    NC_Message = 
		construct_nc_message(MeId, {progressReport, []}),
	    case ct_netconfc:get(NC_sess, NC_Message) of
		{ok, [Ans]} ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ReportProgress = get_progress(MeId, brmBackupManager, Ans),
		    ct:pal("## ReportProgress: ~p",[ReportProgress]),
		    %% ct_netconfc:close_session(NC_sess, 30000),
		    ReportProgress;
		_Err1 ->
		    ct_netconfc:close_session(NC_sess, 30000),
		    ct:log("## No progress report recived: ~p ~n"
			   "## Check nc session. ~n",[_Err1]),
		    timer:sleep(5000),
		    %% swm_test_lib:check_nc_session(NC_sess),
		    get_progress_report(NC_sess, MeId, WhichBU, Timeout-5000)
	    end;
	_ ->
	    ct:fail("TC will fail due to no defined bu progress "
		    "that shall be used")
    end.



get_progress(MeId, WhatProgress, Ans) ->
    {?MANAGED_ELEMENT,
     ?COM_TOP,
     [?ELEMENT_ID_1,
      {?SYSTEMFUNCTIONS, [],
       [?SYSTEMFUNCTIONS_ID_1,
	{?BRM,
	 ?REC_BRM_XLMNS,
	 [?BRM_ID,
	  {?BRMBACKUPMANAGER, [],
	   [?BRMBACKUPMANAGER_ID,
	    Progress]}]}]}]} = Ans,
    case WhatProgress of
	 {brmBackup, BuId} ->
	    {'BrmBackup',[],
	     [{brmBackupId,[],BuId},
	      ReportProgress]} = Progress,
	    ReportProgress;
	{brmBackupScheduler, Id} ->
	    {'BrmBackupScheduler',[],
	     [{brmBackupSchedulerId,[],[Id]},
	      ProgressReport]} = Progress,
	    ProgressReport;
	{brmFailsafeBackup, Id} ->
	    {'BrmFailsafeBackup',[_],
	     [{brmFailsafeBackupId,[],[Id]},
	      ProgressReport]} = Progress,
	    ProgressReport;
	brmBackupManager ->
	    Progress
    end.
%%% ===========================================================================
%%% @doc
%%% Description: Returns a list of all backup elements <br/>
%%% @spec get_all_backups(NC_sess, MeId) -> ok
%%% @end
%%% ===========================================================================
get_all_backups(NC_sess, MeId) ->
    %% Get = construct_nc_message(MeId),
    Get = ?BRM_MO,

    {ok, Result} = netconf(NC_sess, get, [Get]),
    ct:log("Get: ~p",[Result]),
    {ok, {_, _, Contents}} = extract_element('BrmBackupManager', Result),
    [BrmBackupE||BrmBackupE<-Contents, 
		 element(1, BrmBackupE) == 'BrmBackup'].

%% %%%--------------------------------------------------------------------
%% %%% Description: Read the resultinfo info of the last progress report
%% %%%--------------------------------------------------------------------
%% get_result_info(NC_Sess, ProgressFilter) ->
%%     get_progress_report_member(NC_Sess, resultInfo, ProgressFilter).

%% %%%--------------------------------------------------------------------
%% %%% Description: Read a member of the progress report struct
%% %%%--------------------------------------------------------------------
%% get_progress_report_member(NC_Sess, Member, ProgressFilter) ->
%%     {ok, A} = netconf(NC_Sess, get, [NC_Sess, ProgressFilter]),
%%     case extract_element(progressReport, A) of
%% 	{ok, {progressReport, [{unset, "true"}], []}}  ->
%% 	    undefined;
%% 	_ ->
%% 	    {ok, {Member, [], [Value]}} = 
%% 		extract_element(Member, A),
%% 	    Value
%%     end.

set_failsafe_attribute(NC_sess, MeId, Attribute, Value) ->
    EditConfig = construct_nc_message(MeId, {'BrmFailsafeBackup',
					     [{brmFailsafeBackupId,[],["1"]},
					      {Attribute, [Value]}]} ),
    ok = netconf(NC_sess, edit_config, [running, EditConfig]).

    %% netconf(edit_config, [nc1, running, EditConfig]).


%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------
bu_action(NC_sess, ActionCmd, MeId, Name) ->
    Action = construct_nc_message(MeId, {ActionCmd,[],
					 [{name, [], [Name]}]}),

    ct:pal("Executing action: ~p", [ActionCmd]),
    {ok, ActionResponse} = netconf(NC_sess, action, [Action]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} -> 
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    Action.

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------
netconf(NC_sess, F, A) ->
    %% {ok, _} = ct_netconfc:open(NC_sess, []),
    netconf_open(NC_sess, []),
    Res = apply(ct_netconfc, F, [NC_sess | A]),
    ok = ct_netconfc:close_session(NC_sess),
    Res.


%%% ===========================================================================
%%% @doc
%%% Description: wait for an certain expected state in progress report. <br/>
%%% Two differents state can be used. brmBackupManager or brmBackup.  <br/>
%%% Session = atom() <br/>
%%% MeId = string() <br/>
%%% OldActionId = string() <br/>
%%% ExpState = string(),  "FINISHED" | "CANCELLED"<br/>
%%% @spec wait_for_expecting_state(Session, MeId, OldActionId, ExpState) -> ok
%%% @end
%%% ===========================================================================
wait_for_expecting_state(Session, MeId, OldActionId, ExpState) ->
    wait_for_expecting_state(Session, MeId, OldActionId, ExpState, {brmBackupManager, dummy}, 60000). 
wait_for_expecting_state(Session, MeId, OldActionId, ExpState, WhichBU) ->
    wait_for_expecting_state(Session, MeId, OldActionId, ExpState, WhichBU, 300000). 

wait_for_expecting_state(Session, MeId, OldActionId, ExpState, WhichBU, Timeout) 
  when Timeout < 0 ->
    case WhichBU of
	{brmBackup, _BuId} ->
	    {progressReport,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_progress_report(Session, 
						   MeId, 
						   WhichBU);
	{brmBackupScheduler, _Id} ->
	    {progressReport,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_progress_report(Session, 
						   MeId, 
						   WhichBU);
	{brmFailsafeBackup, _Id} ->
	    {progressReport,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_progress_report(Session, 
						   MeId, 
						   WhichBU);
	_ ->
	    {progressReport,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_progress_report(Session, 
						   MeId)
    end,

    {actionId,_,[ActionId]} = lists:keyfind(actionId, 1, ProgressReport),
    case ActionId of
	Val when Val == OldActionId -> %%After a n restart old and new actionid could be same, then return Result and match in TC.
	    
	    {ok, Result} = check_progress_elements(ProgressReport, ExpState),
	    ct_netconfc:close_session(Session, 30000),
	    Result;
	_ ->
	    ct_netconfc:close_session(Session, 30000),
	    ct:pal("WhichBU : ~p",[WhichBU]),
	    ct:pal("# ActionId: ~p, OldActionId: ~p",[ActionId, OldActionId]),
	    ct:fail("Not received Expected state within max timeout.")
    end;
	
wait_for_expecting_state(Session, MeId, OldActionId, ExpState, WhichBU, Timeout) ->
    case WhichBU of
	{brmBackup, _BuId} ->
	    {progressReport,
	     %% [{struct,"AsyncActionProgress"}],
	     _,
	     ProgressReport} = get_progress_report(Session, 
						   MeId, 
						   WhichBU);
	{brmBackupScheduler, _Id} ->
	    {progressReport,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_progress_report(Session, 
						   MeId, 
						   WhichBU);
	{brmFailsafeBackup, _Id} ->
	    {progress,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_progress_report(Session, 
						   MeId, 
						   WhichBU);
	_ ->
	    {progressReport,
	     [{struct,"AsyncActionProgress"}],
	     ProgressReport} = get_progress_report(Session, 
						   MeId)
    end,

    ct:log("###いい ProgressReport: ~p",[ProgressReport]),
    %% {actionId,_,[ActionId]} = lists:keyfind(actionId, 1, ProgressReport),
    case ProgressReport of
	[] -> 
	    ct:log("# ProgressReport is empty !"),
	    ActionId = undefined;
	_NotEmpty ->
	    {actionId,_,[ActionId]} = lists:keyfind(actionId, 1, ProgressReport)
    end,

    %ct:pal("###いい ProgressReport: ~p",[ProgressReport]),
    ct:pal("### ActionId: ~p, OldActionId: ~p",[ActionId, OldActionId]),
    case ActionId of
	OldActionId ->
	%% Val when Val == OldActionId ->
	    ct:pal("Waiting for updated progress~n",[]),
	    timer:sleep(5000),
	    wait_for_expecting_state(Session, MeId, OldActionId, ExpState, 
				     WhichBU, Timeout-5000);
	no_check -> 
	    case check_progress_elements(ProgressReport, ExpState) of
		loop ->
		    timer:sleep(5000),
		    wait_for_expecting_state(Session, MeId, OldActionId, 
					     ExpState, WhichBU, Timeout-5000);
		{ok, Result} ->
		    ct:log("Result: ~p", [Result]),
		    ct_netconfc:close_session(Session, 30000),
		    ct:pal("New ActionId: ~p, OldActionId: ~p", [ActionId,
								 OldActionId]),
		    Result
	    end;
	_ ->
	    case check_progress_elements(ProgressReport, ExpState) of
		loop ->
		    timer:sleep(5000),
		    wait_for_expecting_state(Session, MeId, OldActionId, 
					     ExpState, WhichBU, Timeout-5000);
		{ok, Result} ->
		    ct:log("Result: ~p", [Result]),
		    ct_netconfc:close_session(Session, 30000),
		    ct:pal("New ActionId: ~p, OldActionId: ~p", [ActionId,
								 OldActionId]),
		    Result
	    end
    end.

%%%--------------------------------------------------------------------
%%% Description: wait forFINISHED in  progress results and return results.
%%%--------------------------------------------------------------------
%% check_progress_elements(ProgressReport) ->
%%     check_progress_elements(ProgressReport, "FINISHED").

check_progress_elements(ProgressReport, ExpState) ->
    {state,[],[State]} = 
	lists:keyfind(state, 1, ProgressReport),
    case State of
	ExpState->
	    ct:log("# ~p~n",[ProgressReport]),
	    {result,[],[Result]} = 
		lists:keyfind(result, 1, ProgressReport),
	    {actionName,[],[ActionName]} = 
		lists:keyfind(actionName, 1, ProgressReport),
	    {progressInfo,[],[ProgressInfo]} = 
		lists:keyfind(progressInfo, 1, ProgressReport),
	    case lists:keyfind(resultInfo, 1, ProgressReport) of
		{resultInfo,[],[ResultInfo]} ->
		    ResultInfo;
		{resultInfo,[],[]} ->
		    ResultInfo = []
	    end,
	    {state,[],[State]} = 
    		lists:keyfind(state, 1, ProgressReport),
	    {ok, [{Result, ActionName, ProgressInfo, ResultInfo, State,
		   ProgressReport}]};
	CurrentState -> %% Ej klar
	    {actionName,[],[ActionName]} = 
		lists:keyfind(actionName, 1, ProgressReport),
	    {progressPercentage,[],[Percent]} = 
		lists:keyfind(progressPercentage, 1, ProgressReport),
	    {progressInfo,[],[Info]} = 
		lists:keyfind(progressInfo, 1, ProgressReport),
	    %% ct:pal("# State: ~s ~p % ready~n~s",[CurrentState, 
	    %% 					 list_to_integer(Percent), 
	    %% 					 Info]),
	    %% loop
	    ct:pal("Action: ~s~nState: ~s ~s % ready~n~s",
		   [ActionName, CurrentState, Percent, Info]),
	    loop
    end.

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
%%% Description: Convert an backup action error code to a meaningful value
%%%--------------------------------------------------------------------
decode_return_value("0") -> actionStarted;
decode_return_value("1") -> nameValidationFailure;
decode_return_value("2") -> duplicateName;
decode_return_value("3") -> housekeepingRequired;
decode_return_value("4") -> backupNotFound;
decode_return_value("98") -> missingParameter;
decode_return_value("99") -> softwareFault.




%%% Functions used for schedule backup.

%%%--------------------------------------------------------------------
%%% Description: Adds seconds to a now-tuple
%%%--------------------------------------------------------------------

add_time(N, {N1,N2,N3}) ->
    case N2+N of
	F2 when F2 >= 1000000 ->
	    {N1+F2 div 1000000, F2 rem 1000000, N3};
	F2 ->
	    {N1, F2, N3}
    end.

%%%--------------------------------------------------------------------
%%% Description: Get node time
%%%--------------------------------------------------------------------

timestamp() ->
    rct_rpc:call(rpc_1, os, timestamp, [], 10000).


%%% ----------------------------------------------------------
%%% @doc Convert an os:timestamp() tuple to an ISO 8601 string
%%%
%%% Input: Now  - An os:timestamp() tuple
%%%        Type - basic|extended|extended_zonefree|extended_z
%%% Output: string()
%%% @end
%%% ----------------------------------------------------------

iso_time(Now, Type) ->
    fn_date(Now, Type)++"T"++fn_time(Now, Type).

%% time_offset(Now) ->
%%     DT = calendar:now_to_local_time(Now),
%%     UTC = calendar:now_to_universal_time(Now),
%%     DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
%%         calendar:datetime_to_gregorian_seconds(UTC),
%%     [Sign, DH, DM] = diff(DiffSecs),
%%     lists:append([[Sign], padzero(DH), ":", padzero(DM)]).


fn_date(Now, Type) ->
    {{Y,M,D}, _} = calendar:now_to_local_time(Now),
    case Type of
        basic ->
            lists:append([integer_to_list(Y),
                          padzero(M),
                          padzero(D)]);
	extended_z ->
	    {{YU,MU,DU}, _} = calendar:now_to_universal_time(Now),
	    
            lists:append([integer_to_list(YU), "-",
                          padzero(MU), "-", padzero(DU)]);
	    
        Extended when Extended==extended; Extended==extended_zonefree ->
            lists:append([integer_to_list(Y), "-",
                          padzero(M), "-", padzero(D)])
    end.

fn_time(Now, Type) ->
    DT={_, {H, M, S}} = calendar:now_to_local_time(Now),
    UTC = calendar:now_to_universal_time(Now),
    DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC),
    [Sign, DH, DM] = diff(DiffSecs),
    case Type of
        basic ->
            lists:append([padzero(H),
                          padzero(M),
                          padzero(S),
                          [Sign],
                          padzero(DH),
                          padzero(DM)]);
	extended_z ->
	    {_, {HU, MU, SU}} = UTC,
            lists:append([padzero(HU), ":",padzero(MU), ":",padzero(SU),"Z"]); 
        extended ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S),
                          [Sign], padzero(DH), ":", padzero(DM)]);
        extended_zonefree ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S)])
    end.

%% format_date({{Y,M,D},{H,Mi,S}}) ->
%%     lists:append([integer_to_list(Y), "-", padzero(M), "-", padzero(D),"T",
%% 		  padzero(H), ":", padzero(Mi), ":", padzero(S)]).
    

padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.


diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
        {0, {H, M,_}} ->
                [$+, H, M];
        {-1, _} ->
                {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
                [$-, H, M]
    end.

%%%--------------------------------------------------------------------
%%% Description: Set the BrmBackupScheduler.adminState attribute
%%%--------------------------------------------------------------------

set_admin_state(NC_sess, MeId, Value, SchedId) ->
    Set = construct_nc_message(MeId, {'BrmBackupScheduler',
				      [{brmBackupSchedulerId,[], [SchedId]},
				       {adminState, [Value]}]} ),
    ok = netconf(NC_sess, edit_config, [running, Set]).

%%%--------------------------------------------------------------------
%%% Description: Read the value of BrmBackupScheduler.nextScheduledTime
%%%--------------------------------------------------------------------
get_next_scheduled_time(NC_sess, MeId, SchedId) ->
    Get = construct_nc_message(MeId, {'BrmBackupScheduler',
				      [{brmBackupSchedulerId, [], [SchedId]},
				       {nextScheduledTime, []}]} ),

    {ok, A} = netconf(NC_sess, get, [Get]),

    case extract_element(nextScheduledTime, A) of
	{ok, {nextScheduledTime, [{unset, "true"}], []}} ->
	    undefined;
	{ok, {nextScheduledTime, _, [Time]}} -> 
	    Time
    end.


%%%--------------------------------------------------------------------
%%% Description: Construct a nc message. Add top levels on the message.
%%%--------------------------------------------------------------------
%% construct_nc_message(MeId) ->
%%     %% Mess = {'ManagedElement',
%%     %% 	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%%     %% 	    [{managedElementId,[],[MeId]},
%%     %% 	     {'SystemFunctions',
%%     %% 	      [{systemFunctionsId,[],["1"]},
%%     %% 	       {'BrM',
%%     %% 		[{brMId,[],["1"]},
%%     %% 		 {'BrmBackupManager',
%%     %% 		  [{brmBackupManagerId,[],["1"]}
%%     %% 		  ]}]}]}]},
%%     Mess = ?BRM_MO,
%%     ct:log("# Message : ~p", [Mess]),
%%     Mess.

construct_nc_message(MeId, Message) ->
    %% Mess = {'ManagedElement',
    %% 	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 	    [{managedElementId,[],[MeId]},
    %% 		{'SystemFunctions',
    %% 		 [{systemFunctionsId,[],["1"]},
    %% 		  {'BrM',
    %% 		   [{brMId,[],["1"]},
    %% 		    {'BrmBackupManager',
    %% 		     [{brmBackupManagerId,[],["1"]},
    %% 		      Message
    %% 		     ]}]}]}]},

    Mess = {?MANAGED_ELEMENT,
	    ?COM_TOP,
	    [?ELEMENT_ID_1,
	     {?SYSTEMFUNCTIONS,
	      [?SYSTEMFUNCTIONS_ID_1,
	       {?BRM,
		[?BRM_ID,
		 {?BRMBACKUPMANAGER,
		  [?BRMBACKUPMANAGER_ID,
		   Message]}]}]}]},
    ct:log("## Message : ~p", [Mess]),
    Mess.
    

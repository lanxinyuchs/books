%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	post_ug_lib.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R4A/R5A/R6A/3
%%%
%%% @doc == support lib when testing upgrade mechanism. ==
%%% <br/>
%%%
%%%
%%% @end

-module(post_ug_lib).
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
%%% R2A/2      2013-12-13 etxivri     Created.
%%% R3A/1      2015-04-29 etxmlar     Added functions
%%% R3A/2      2015-05-27 etxmlar     Corrected edoc fault
%%% R3A/3      2015-06-02 etxmlar     Updated 
%%% R3A/4      2015-06-03 etxmlar     Updated fingerprint check after UG
%%% R3A/5      2015-06-15 etxmlar     Added CXP UP build 
%%% R4A/1      2015-06-26 etxmlar     Added backup functions and CERT
%%% R4A/2      2015-06-30 etxmlar     Modify check_data_from_cli/3
%%% R4A/3      2015-11-19 etxmlar     Corrected tc after changes
%%%                                   in LogM progressReport
%%% R4A/4      2015-11-19 etxmlar     Updated edoc
%%% R4A/5      2015-11-25 etxmlar     Added wait time in reinstall_rbs
%%% R5A/1      2016-05-23 etxmlar     Corrected a list of tokens 
%%% R5A/2      2016-05-26 etxmlar     Corrected, check for CXS UP
%%% R5A/3      2016-05-26 etxmlar     Forgotten revision log
%%% R5A/4      2016-06-02 etxmlar     Added case clause in re:run to handle 
%%%                                   CorruptRecivedData in check_data
%%% R6A/2      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R6A/3      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------

% -compile([export_all]).
-include_lib("lihi.hrl").

-export([
	 create_and_install_lkf/0,
	 reinstall_rbs/2,
	 check_if_fingerprint_updateable/0,
	 change_attribute_from_cli/2,
	 check_data_from_cli/3,
	 change_mo_from_cli/1,
	 create_pmjob_from_cli/4,
	 create_pmjob_from_cli/5,
	 create_pmevent_from_cli/5,
	 find_mo_from_cli/5,
	 change_attributes_from_cli/2,
	 create_backup_mo_from_cli/3,
	 find_pmjob_from_cli/7,
	 find_pmevent_from_cli/7,
	 find_single_scheduler_backup_event/4,
	 find_periodic_scheduler_backup_event_calender/4,
	 find_periodic_scheduler_backup_event/4,
	 create_enrollmentauthority_mo_from_cli/3,
	 create_enrollmentservergroup_mo_from_cli/3,
	 create_enrollmentserver_mo_from_cli/3,
	 create_nodecredential_mo_from_cli/3,
	 install_trustcertificate_mo_from_cli/5,
	 installcredentialfromuri_from_cli/6,
	 fetch_esi/4,
	 fetch_avli/4, 
	 fetch_log/4, 
	 create_trustcategory_mo_from_cli/3,
	 check_progress_data_from_cli/3,
	 check_progress_data_when_reboot_from_cli/4,
	 create_customrule_mo_from_cli/3,
	 create_customrole_mo_from_cli/3,
	 create_snmptargetv3_mo_from_cli/3,
	 create_pushlog_mo_from_cli/3,
	 check_sec_mo_and_data_from_cli/4,
	 check_sec_mo_object_and_data_from_cli/5,
	 unpack_esi_log_data_from_cli/6,
	 unpack_avli_log_data_from_cli/6,
	 check_log_progress_data_from_cli/3,
	 check_avli_log_data_from_cli/1,
	 check_esi_log_data_from_cli/1,
	 check_push_log_mo_and_data_from_cli/4,
	 find_swm_log_data/2,
	 configure_tnport/2,
	 configure_ethernetport/2,
	 configure_vlanport/2,
	 configure_ipv4address/5,
	 configure_routetableIPv4static/6,
	 get_current_up_from_cli/0,
	 change_fingerprint_attribute_from_cli/2,
	 build_valid_full_ug_package/4,
	 create_backup_manually/3,
	 restore_backup_manually/2,
	 startonlineenrollment_from_cli/3,
	 startofflineenrollment_from_cli/4
	]).

-define(ERROR_STRING, "\"ERROR REPORT|CRASH REPORT\"").

%%% ===========================================================================
%%% @doc
%%% Create and install LKF. <br/>
%%% @spec create_and_install_lkf() -> ok
%%% @end
%%% ===========================================================================
create_and_install_lkf() ->
    compile:file(rct_cc_git_path:find("RCS_TOP", ["LMA/LMA_CNX9013077/test/suites/lmaTestLib.erl", "LMA/test/suites/lmaTestLib.erl"])),
   
    case lmaTestLib:check_if_fingerprint_updateable() of
 	ok ->
	    ct:pal("Change the fingerprint from CLI"),
	    lmaTestLib:changing_fingerprint_from_cli(?Fingerprint),
	    ct:pal("Install LKF"),
	    lmaTestLib:install_lkf(),
	    ok = lmaTestLib:check_data_from_cli(["result=SUCCESS", "state=FINISHED"], 
						?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL);
	nok ->
	    ct:pal("The LKF is already installed")
    end,
    ok.
%%% ===========================================================================
%%% @doc
%%% Check if LKF is instlled i.e, check if fingerprint is updateable. <br/>
%%% @spec check_if_fingerprint_updateable()-> ok|nok
%%% @end
%%% ===========================================================================
check_if_fingerprint_updateable()->

    compile:file(rct_cc_git_path:find("RCS_TOP", ["LMA/LMA_CNX9013077/test/suites/lmaTestLib.erl",
				       "LMA/test/suites/lmaTestLib.erl"])),
 
    Result = 
	case lmaTestLib:check_if_fingerprint_updateable() of
	    ok ->
		ok;
	    nok ->
		ct:pal("The LKF is already installed"),
		nok
	end,
    Result.


%%% ===========================================================================
%%% @doc
%%% Try to chhange fingerprint attribute from cli ater upgrade. <br/>
%%% @spec change_fingerprint_attribute_from_cli(AttString, LdnString) -> {ok,RecievedData2}
%%% @end
%%% ===========================================================================
change_fingerprint_attribute_from_cli(AttString, LdnString) ->

    Options = [global,{capture, all, binary}],
    
    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, LdnString),
    {ok,RecievedData} = rct_cli:send(cli, AttString),

    case re:run(RecievedData, "ERROR", Options) of
	{match, [[_Value]]} ->
	    ct:log("Attribute configuration error = ~p~n",[RecievedData]),
	    ct:fail("Attribute configuration error");
	nomatch ->
	    ok
    end,
    
    {ok,RecievedData2} = rct_cli:send(cli,"commit"), 
    
    case re:run(RecievedData2, "ERROR", Options) of
	{match, [[_Value2]]} ->
	    ct:log("OK, Fingerprint not possible to update = ~p~n",[RecievedData2]),
	    rct_cli:send(cli, "abort");
	nomatch ->
	    ct:log("ERROR Fingerprint possible to update = ~p~n",[RecievedData2]),
	    ct:fail("ERROR Fingerprint possible to update")
    end,
    
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Change attribute from cli. <br/>
%%% @spec change_attribute_from_cli(AttString, LdnString) -> ok
%%% @end
%%% ===========================================================================
change_attribute_from_cli(AttString, LdnString) ->

    Options = [global,{capture, all, binary}],
    
    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, LdnString),
    {ok,RecievedData} = rct_cli:send(cli, AttString),

    case re:run(RecievedData, "ERROR", Options) of
	{match, [[_Value]]} ->
	    ct:log("Attribute configuration error = ~p~n",[RecievedData]),
	    ct:fail("Attribute configuration error");
	nomatch ->
	    ok
    end,
    
    {ok,RecievedData2} =  rct_cli:send(cli,"commit"), 

    case re:run(RecievedData2, "ERROR", Options) of
	{match, [[_Value2]]} ->
	    ct:log("commit error = ~p~n",[RecievedData2]),
	    ct:fail("commit error");
	nomatch ->
	    ok
    end,
    
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Change mo from cli. <br/>
%%% @spec change_mo_from_cli( LdnString) -> ok
%%% @end
%%% ===========================================================================
change_mo_from_cli( LdnString) ->
    
    rct_cli:send(cli, LdnString),
    rct_cli:send(cli,"activate"),
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Create PM job without attribute change from cli. <br/>
%%% @spec create_pmjob_from_cli(PmJobString, MeasurementReader, MeasurementSpec, GroupRef) -> ok
%%% @end
%%% ===========================================================================
create_pmjob_from_cli(PmJobString, MeasurementReader, MeasurementSpec, GroupRef) ->
    
    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, PmJobString),
    rct_cli:send(cli, MeasurementReader),
    rct_cli:send(cli, MeasurementSpec),
    rct_cli:send(cli, GroupRef),
    rct_cli:send(cli,"commit"), 
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Create PM job with attribute change from cli. <br/>
%%% @spec create_pmjob_from_cli(PmJobString, AttStringList, MeasurementReader, MeasurementSpec, GroupRef) -> ok
%%% @end
%%% ===========================================================================
create_pmjob_from_cli(PmJobString, AttStringList, MeasurementReader, MeasurementSpec, GroupRef) ->
    
    Options = [global,{capture, all, binary}],

    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, PmJobString),
    
    lists:foreach(fun(AttString) ->    
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end 
		  end, 
		  AttStringList),

    rct_cli:send(cli, MeasurementReader),
    rct_cli:send(cli, MeasurementSpec),
    rct_cli:send(cli, GroupRef),
    rct_cli:send(cli,"commit"), 
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Change attributs from cli. <br/>
%%% @spec change_attributes_from_cli(AttStringList, LdnString) -> ok
%%% @end
%%% ===========================================================================
change_attributes_from_cli(AttStringList, LdnString) ->

    Options = [global,{capture, all, binary}],

    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, LdnString),
    
    lists:foreach(fun(AttString) ->
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end
		  end, 
		  AttStringList),
    
    {ok,RecievedData2} = rct_cli:send(cli,"commit"), 
    case re:run(RecievedData2, "ERROR", Options) of
	{match, [[_Value2]]} ->
	    ct:log("commit error = ~p~n",[RecievedData2]),
	    ct:fail("commit error");
	nomatch ->
	    continue
    end,
    
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Create PM event from cli. <br/>
%%% @spec create_pmevent_from_cli(EventProcedurString, EventJob, AttStringList, EventGroupRef, EventTypeRef) -> ok
%%% @end
%%% ===========================================================================
create_pmevent_from_cli(EventProcedurString, EventJob, AttStringList, EventGroupRef, EventTypeRef) ->

    Options = [global,{capture, all, binary}],

    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, EventProcedurString),
    rct_cli:send(cli, EventJob),
    
    lists:foreach(fun(AttString) -> 
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end
		  end, 
		  AttStringList),
    
    rct_cli:send(cli, "up"),
    rct_cli:send(cli, EventGroupRef),
    rct_cli:send(cli, EventTypeRef),
    
    rct_cli:send(cli,"commit"), 
    rct_cli:send(cli, "top").
	
%%% ===========================================================================
%%% @doc
%%% Create backup mo from cli. <br/>
%%% @spec create_backup_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_backup_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create enrollmentauthority mo from cli. <br/>
%%% @spec create_enrollmentauthority_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_enrollmentauthority_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create enrollmentservergroup mo from cli. <br/>
%%% @spec create_enrollmentservergroup_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_enrollmentservergroup_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create enrollmentserver mo from cli. <br/>
%%% @spec create_enrollmentserver_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_enrollmentserver_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create nodecredential mo from cli. <br/>
%%% @spec create_nodecredential_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_nodecredential_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create trustcategory mo from cli. <br/>
%%% @spec create_trustcategory_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_trustcategory_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create customrule mo from cli. <br/>
%%% @spec create_customrule_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_customrule_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create customrole mo from cli. <br/>
%%% @spec create_customrole_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_customrole_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create snmptargetv3 mo from cli. <br/>
%%% @spec create_snmptargetv3_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_snmptargetv3_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Create pushlog mo from cli. <br/>
%%% @spec create_pushlog_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_pushlog_mo_from_cli(MoString, AttStringList, LdnString)->
    configur_mo_from_cli(MoString, AttStringList, LdnString).

%%% ===========================================================================
%%% @doc
%%% Configure mo from cli. <br/>
%%% @spec configur_mo_from_cli(MoString, AttStringList, LdnString)-> ok
%%% @end
%%% ===========================================================================
configur_mo_from_cli(MoString, AttStringList, LdnString)->

    Options = [global,{capture, all, binary}],
    
    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, LdnString),
    rct_cli:send(cli, MoString),
    
    lists:foreach(fun(AttString) ->  
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end	 
		  end, 
		  AttStringList),
    
    {ok,RecievedData2} = rct_cli:send(cli,"commit"), 
    case re:run(RecievedData2, "ERROR", Options) of
	{match, [[_Value2]]} ->
	    ct:log("commit error = ~p~n",[RecievedData2]),
	    ct:fail("commit error");
	nomatch ->
	    continue
    end,
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% startOnlineEnrollment from cli. <br/>
%%% @spec startonlineenrollment_from_cli(Action, ChallengePassword, LdnString)-> ok
%%% @end
%%% ===========================================================================
startonlineenrollment_from_cli(Action, ChallengePassword, LdnString)->

    rct_cli:send(cli, LdnString),
    rct_cli:send(cli, Action++" --challengePassword "++ChallengePassword).

% ===========================================================================
%%% @doc
%%% startOfflineCsrEnrollment from cli. <br/>
%%% @spec startofflineenrollment_from_cli(Action, Uri, UriPassword, LdnString)-> ok
%%% @end
%%% ===========================================================================
startofflineenrollment_from_cli(Action, Uri, UriPassword, LdnString)->

    rct_cli:send(cli, LdnString),
    rct_cli:send(cli, Action++" --uri "++Uri++" --uriPassword "++UriPassword).

%%% ===========================================================================
%%% @doc
%%% installCredentialFromUri from cli. <br/>
%%% @spec installcredentialfromuri_from_cli(Action, Uri, UriPassword, CredentialPassword, Fingerprint, LdnString)-> ok
%%% @end
%%% ===========================================================================
installcredentialfromuri_from_cli(Action, Uri, UriPassword, CredentialPassword, Fingerprint, LdnString)->

    rct_cli:send(cli, LdnString),
    rct_cli:send(
      cli, Action++" --uri "++Uri++" --uriPassword "++UriPassword++" --credentialPassword "++CredentialPassword++" --fingerprint "++Fingerprint).

%%% ===========================================================================
%%% @doc
%%% Install Trustcertificate mo from cli. <br/>
%%% @spec install_trustcertificate_mo_from_cli(Action, Uri, UriPassword, Fingerprint, LdnString)->ok
%%% @end
%%% ===========================================================================
install_trustcertificate_mo_from_cli(Action, Uri, UriPassword, Fingerprint, LdnString)->

    rct_cli:send(cli, LdnString),
    rct_cli:send(
      cli, Action++" --uri "++Uri++" --uriPassword "++UriPassword++" --fingerprint "++Fingerprint).

%%% ===========================================================================
%%% @doc
%%% Export ESI from cli. <br/>
%%% @spec fetch_esi(Action, Uri, UriPassword, LdnString)-> ok
%%% @end
%%% ===========================================================================
fetch_esi(Action, Uri, UriPassword, LdnString)->
    
    rct_cli:send(cli, LdnString),
    rct_cli:send(cli, Action++" --uri "++Uri++" --password "++UriPassword).

%%% ===========================================================================
%%% @doc
%%% Export Availibility Log from cli. <br/>
%%% @spec fetch_avli(Action, Uri, UriPassword, LdnString)-> ok
%%% @end
%%% ===========================================================================
fetch_avli(Action, Uri, UriPassword, LdnString)->
    
    rct_cli:send(cli, LdnString),
    rct_cli:send(cli, Action++" --uri "++Uri++" --password "++UriPassword).

%%% ===========================================================================
%%% @doc
%%% Export  Log from cli. <br/>
%%% @spec fetch_log(Action, Uri, UriPassword, LdnString)-> ok
%%% @end
%%% ===========================================================================
fetch_log(Action, Uri, UriPassword, LdnString)->
    
    rct_cli:send(cli, LdnString),
    rct_cli:send(cli, Action++" --uri "++Uri++" --password "++UriPassword).

%%% ===========================================================================
%%% @doc
%%% Create backup manually <br/>
%%% @spec create_backup_manually(Action, Name, LdnString)-> ok
%%% @end
%%% ===========================================================================
create_backup_manually(Action, Name, LdnString)->
    
    rct_cli:send(cli, LdnString),
    {ok, RecievedData} = rct_cli:send(cli, Action++" --name "++Name),

    Data = string:tokens(lists:flatten(RecievedData), "\r\n "),
    ReturnValue = lists:nth(4, Data),

    case decode_return_value(ReturnValue) of
	actionStarted ->
	    ok;
	ErrorValue ->
	    ct:log("Create Backup error. Error: ~p~n",[ErrorValue]),
	    ct:fail(ErrorValue)
    end.   

%%% ===========================================================================
%%% @doc
%%% Restore backup manually <br/>
%%% @spec restore_backup_manually(Action, LdnString)-> ok
%%% @end
%%% ===========================================================================
restore_backup_manually(Action, LdnString)->
    
    rct_cli:send(cli, LdnString),
    {ok, RecievedData} = rct_cli:send(cli, Action),

    Data = string:tokens(lists:flatten(RecievedData), "\r\n "),
    ReturnValue = lists:nth(2, Data),

    case decode_return_value(ReturnValue) of
    	actionStarted ->
    	    ok;
    	ErrorValue ->
    	    ct:log("Restore Backup error. Error: ~p~n",[ErrorValue]),
    	    ct:fail(ErrorValue)
    end.   


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

%%% ===========================================================================
%%% @doc
%%% Check data from cli. <br/>
%%% @spec check_data_from_cli(AttributeList, Mo, NoOfTries) -> ok|nok
%%% @end
%%% ===========================================================================
check_data_from_cli(AttributeList, Mo, NoOfTries) ->
    {ok, RecievedData} = rct_cli:send(cli, "show verbose "++Mo, print),
    check_data_from_cli(AttributeList, Mo, NoOfTries, RecievedData).

check_data_from_cli([], _Mo, _NoOfTries, _RecievedData) ->
    ok;


check_data_from_cli([WantedString | RestWantedList], Mo, NoOfTries, RecievedData) ->
    
    case check_data(WantedString, RecievedData) of
	ok ->
	    check_data_from_cli(RestWantedList, Mo, NoOfTries, RecievedData);
	nok ->
	    case NoOfTries of
		0 ->
		    nok;
		_Else ->
		    ct:pal("Did not find ~p, trying again in 1 sec", [WantedString]),
		    timer:sleep(1000),
		    check_data_from_cli([WantedString] ++ RestWantedList, Mo, NoOfTries -1)
	    end
    end.

check_data(WantedString, RecievedData) ->

    Options = [global, %% check all rows
               {capture, all, binary}],
    case re:run(RecievedData, WantedString, Options) of
	{match, [[_Value]]} ->
	    ok;
	nomatch ->
	    nok;
	_CourruptRecievedData ->
	    nok
    end.
%%% ===========================================================================
%%% @doc
%%% Check progress data from CLI <br/>
%%% @spec check_progress_data_from_cli([], _Mo, _NoOfTries) -> ok
%%% @end
%%% ===========================================================================
check_progress_data_from_cli([], _Mo, _NoOfTries) ->
    ok;

check_progress_data_from_cli([WantedString | RestWantedList], Mo, NoOfTries) ->

    {ok, RecievedData} = rct_cli:send(cli, "show verbose "++Mo, print),
    case check_data(WantedString, RecievedData) of
	ok ->
	    check_progress_data_from_cli(RestWantedList, Mo, NoOfTries);
	nok ->
	    case NoOfTries of
		0 ->
		    nok;
		_Else ->
		    ct:pal("Did not find ~p, trying again in 3 sec", [WantedString]),
		    timer:sleep(3000),
		    check_progress_data_from_cli([WantedString] ++ RestWantedList, Mo, NoOfTries -1)
	    end
    end.

%%% ===========================================================================
%%% @doc
%%% Check progress data when reboot from CLI <br/>
%%% @spec check_progress_data_when_reboot_from_cli([], _Mo, _NoOfTries, _CliSession) -> ok
%%% @end
%%% ===========================================================================
check_progress_data_when_reboot_from_cli([], _Mo, _NoOfTries, _CliSession) ->
    ok;

check_progress_data_when_reboot_from_cli([WantedString | RestWantedList], Mo, NoOfTries, CliSession) ->

    %% {ok, RecievedData} = rct_cli:send(cli, "show verbose "++Mo, print), 
    RecievedData = rct_cli:send(cli, "show verbose "++Mo, print), 

    case RecievedData of
	{ok, Data} ->
	    case check_data(WantedString, Data) of
		ok ->
		    check_progress_data_when_reboot_from_cli(RestWantedList, Mo, NoOfTries, CliSession);
		nok ->
		    case NoOfTries of
			0 ->
			    nok;
			_Else ->
			    ct:pal("Did not find ~p, trying again in 5 sec", [WantedString]),
			    timer:sleep(5000),
			    check_progress_data_when_reboot_from_cli([WantedString] ++ RestWantedList, 
								     Mo, NoOfTries -1, CliSession)
		    end
	    end;
	{error,closed} ->
	    ok = rct_cli:disconnect(CliSession),
	    ct:pal("Reboot, Sleep 120 sec before check progress report again"),
	    timer:sleep(120000), 
	    case rct_cli_connect(CliSession, 30) of
		ok ->
		    check_progress_data_when_reboot_from_cli([WantedString] ++ RestWantedList, 
							     Mo, NoOfTries -1, CliSession);
		nok ->
		    nok
	    end	
    end.

rct_cli_connect(CliSession, Tries)->

    Return =
	case rct_cli:connect(CliSession) of
	    ok ->  
		ok;
	    _ ->
		case Tries of
		    0 ->
			nok;
		    _Else ->
			ct:pal("rct_cli connect after reboot trying again in 5 sec"),
			timer:sleep(5000),
			rct_cli_connect(CliSession, Tries-1)
		end		    
	end,
    Return.

%%% ===========================================================================
%%% @doc
%%% Check log progress data from CLI <br/>
%%% @spec check_log_progress_data_from_cli([], Mo, _NoOfTries) -> {ok, RecievedData}
%%% @end
%%% ===========================================================================
check_log_progress_data_from_cli([], Mo, _NoOfTries) ->
    {ok, RecievedData} = rct_cli:send(cli, "show verbose "++Mo, print),
    {ok, RecievedData};

check_log_progress_data_from_cli([WantedString | RestWantedList], Mo, NoOfTries) ->

    timer:sleep(2000),
    {ok, RecievedData} = rct_cli:send(cli, "show verbose "++Mo, print),

    case check_data(WantedString, RecievedData) of
	ok ->
	    check_log_progress_data_from_cli(RestWantedList, Mo, NoOfTries);
	nok ->
	    case NoOfTries of
		0 ->
		    nok;
		_Else ->
		    ct:pal("Did not find ~p, trying again in 4 sec", [WantedString]),
		    timer:sleep(2000),
		    check_log_progress_data_from_cli([WantedString] ++ RestWantedList, Mo, NoOfTries -1)
	    end
    end.

%%% ===========================================================================
%%% @doc
%%% Unpack ESI <br/>
%%% @spec unpack_esi_log_data_from_cli(EsiLogData, EsiLogPath, DataPrefix, SftpHost, SftpUser, SftpPassword)-> ok
%%% @end
%%% ===========================================================================
unpack_esi_log_data_from_cli(EsiLogData, EsiLogPath, DataPrefix, SftpHost, SftpUser, SftpPassword)->
    
    Data = string:tokens(EsiLogData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
   
    [LogData] = find_object_data_from_cli(DataPrefix, Data2),
    EsiLogName = string:substr(LogData, 12),

    {ok,ChPid,_ChRef} =
	ssh_sftp:start_channel(SftpHost,
			       [{user, SftpUser},
				{password, SftpPassword},
				{silently_accept_hosts, true},
				{timeout, 180000}]),
    
    {ok, DirData} = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
    %% DirData = lists of strings from the directory.

    Pred =  
	fun(Str) ->
		if Str == EsiLogName ->
			true;
		   true ->
			false
		end
	end,
    
    case lists:any(Pred,DirData) of
	true ->
	    ct:pal("### Esi log file: ~p, exist in \n path : ~p \n",
		   [EsiLogName, EsiLogPath]),
	    timer:sleep(100),
	    os:cmd("tar -xf " ++ EsiLogPath ++ EsiLogName ++" --gunzip --directory=" ++ EsiLogPath),
	    io:format("<a href=\"~s\">~s</a>",
		      [EsiLogPath, "Esi unpacked dir"]),
	    io:format("<a href=\"~s\">~s</a>",
		      [filename:join(EsiLogPath,EsiLogName), EsiLogName]);
	false ->
	    ct:fail("Could not find the ESI log file, on sftp server.")
    end,

    {ok, FileInfo} =
	ssh_sftp:read_file_info(ChPid, EsiLogPath++EsiLogName, 3000),
    %%ct:pal("### Recieved FileInfo: ~p", [FileInfo]),
    
    Size = lists:nth(2, tuple_to_list(FileInfo)),
    %%{file_info, Size, _, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,
    
    if Size > 10000 ->
	    true;
       true  ->
	    ct:pal("### Size of the esi tar file is: ~p. ~n "
		   "It is smaller than expected. ~n "
		   "Unpack the file and ckeck that it look OK. \n",[Size]),
	    ct:fail("Size of the esi log file is to small! check if it "
		    "looks ok after unpack!.")
    end,

    ssh_sftp:stop_channel(ChPid),
    ok.
%%% ===========================================================================
%%% @doc
%%% Check ESI <br/>
%%% @spec check_esi_log_data_from_cli(EsiLogPath)->  {ok,[]}
%%% @end
%%% ===========================================================================
check_esi_log_data_from_cli(EsiLogPath)->
    
    check_esi_for_pmd(EsiLogPath, "rcs/dumps/"),
    read_esi_log_file(EsiLogPath, "rcs/erlang/*", ?ERROR_STRING).
   
%%% ===========================================================================
%%% @doc
%%% Unpack Availibility Log <br/>
%%% @spec unpack_avli_log_data_from_cli(AvliLogData, AvliLogPath, DataPrefix, SftpHost, SftpUser, SftpPassword)-> ok
%%% @end
%%% ===========================================================================
unpack_avli_log_data_from_cli(AvliLogData, AvliLogPath, DataPrefix, SftpHost, SftpUser, SftpPassword)->

    Data = string:tokens(AvliLogData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
       
    DataList = find_object_data_from_cli(DataPrefix, Data2),
    %% lists:usort(DataList) depends on progressInfo return
    LogData = lists:flatten(lists:usort(DataList)),

    AvliLogFile = filename:basename(LogData),
    
    {ok,ChPid,_ChRef} =
	ssh_sftp:start_channel(SftpHost,
			       [{user, SftpUser},
				{password, SftpPassword},
				{silently_accept_hosts, true},
				{timeout, 180000}]),
    
    {ok, DirData} = ssh_sftp:list_dir(ChPid, AvliLogPath, 2000),
    %% DirData = lists of strings from the directory.
    
    Pred =  
	fun(Str) ->
		if Str == AvliLogFile ->
			true;
		   true ->
			false
		end
	end,
    
    case lists:any(Pred,DirData) of
	true ->
	    ct:pal("### Avli log file: ~p, exist in \n path : ~p \n",
		   [AvliLogFile, AvliLogPath]),
	    timer:sleep(100),
	    io:format("<a href=\"~s\">~s</a>",
		      [AvliLogPath, "Avli unpacked dir"]),
	    io:format("<a href=\"~s\">~s</a>",
		      [filename:join(AvliLogPath,AvliLogFile), AvliLogFile]),
	    copy_file_to_dir_and_unpack(AvliLogPath, AvliLogFile);
	false ->
	    ct:fail(" Could not find the Avli log file, on sftp server.")
    end,

    ssh_sftp:stop_channel(ChPid),
    
    ok.

%%% ===========================================================================
%%% @doc
%%% Check Availibility Log <br/>
%%% @spec check_avli_log_data_from_cli(AvliLogPath)-> {ok, UPAfterUG}
%%% @end
%%% ===========================================================================
check_avli_log_data_from_cli(AvliLogPath)->

    StripPath = string:strip(AvliLogPath, right, $/),
    CheckDir = filename:dirname(StripPath),

    {ok, FileList} = file:list_dir(CheckDir),
    AvliLogFiles = find_object_data_from_cli("RBS_CS_AVAILABILITY_LOG", FileList),			 
    SortAvliLogFiles = lists:sort(AvliLogFiles),

    Avlilog1 = hd(SortAvliLogFiles),
    Avlilog2 = tl(SortAvliLogFiles),

  
    AvliData1 = os:cmd("egrep " ++  "UpgradePackage"  ++ " " ++  filename:join(CheckDir, Avlilog1)),
    AvliData2 = string:tokens(lists:flatten(AvliData1), "\n "), 

    UPListBefore = lists:filter(fun(X) -> 
				      case X of  
					  "<UpgradePackage>" -> 
					      false;  
					  "</UpgradePackage>" -> 
					      false;
					  _ -> 
					      true 
				      end 
			      end, AvliData2),

    %%Check if CXS or CXP
    UPBeforeUG = 
	case lists:prefix("CXS",hd(UPListBefore)) of
	    true->
		swm_test_lib:get_highest_label(UPListBefore);
	    false ->
		get_highest_label(UPListBefore)
	end,
 
    AvliData3 = os:cmd("egrep " ++  "UpgradePackage"  ++ " " ++  filename:join(CheckDir, Avlilog2)),
    AvliData4 = string:tokens(AvliData3, "\n "),
    
    UPListAfter = lists:filter(fun(X) -> 
				     case X of  
					 "<UpgradePackage>" -> 
					     false;  
					 "</UpgradePackage>" -> 
					     false; 
					 _ -> 
					     true 
				     end 
			     end, AvliData4),

    %%Check if CXS or CXP
    UPAfterUG = 
	case lists:prefix("CXS",hd(UPListAfter)) of
	    true->
		swm_test_lib:get_highest_label(UPListAfter);
	    false ->
		get_highest_label(UPListAfter)
	end,

    ct:pal("UP Before UG: ~p - UP After UG: ~p ~n", [UPBeforeUG, UPAfterUG]),

    case UPAfterUG > UPBeforeUG of
	true ->
	    {ok, UPAfterUG};
	false ->
	    ct:pal("UP Before UG: ~p - UP After UG: ~p ~n", [UPBeforeUG, UPAfterUG]),
	    ct:fail("Faulty UP RevNr in AVAILABILITY LOG after Upgrade")
    end.
	
%%% ===========================================================================
%%% @doc
%%% Find object data from CLI <br/>
%%% @spec find_object_data_from_cli(Prefix, Data2) -> Result
%%% @end
%%% ===========================================================================
find_object_data_from_cli(Prefix, Data2)->
    find_object_data_from_cli(Prefix, Data2, []).
    
find_object_data_from_cli(_Prefix, [], Result) ->
    Result;

find_object_data_from_cli(Prefix, [Element | RestElement], Result) ->
    case lists:prefix(Prefix, Element) of
	true -> 
	    find_object_data_from_cli(Prefix, RestElement, Result ++ [Element]);
	false ->
	    find_object_data_from_cli(Prefix, RestElement, Result)
    end.

%%% ===========================================================================
%%% @doc
%%% Find mo from CLI <br/>
%%% @spec find_mo_from_cli(PrefixMo, ParentMo, WantedAttribute, NoOfTries, WantedAnswer) ->  {LDN, WantedAnswer}
%%% @end
%%% ===========================================================================
find_mo_from_cli(PrefixMo, ParentMo, WantedAttribute, NoOfTries, WantedAnswer) ->
    timer:sleep(1000),
    MOs = find_data_from_cli(PrefixMo, ParentMo),
    ct:pal("Checking these MOs: ~p", [MOs]),
    find_mo(PrefixMo, ParentMo, MOs, WantedAttribute, NoOfTries, WantedAnswer).

find_mo(PrefixMo, ParentMo, [], AttrList, NoOfTries, WantedAnswer) ->
    case NoOfTries of
	0 ->
	    ct:pal("MO not found with attribute: ~p", [AttrList]),
	    {no_mo, nok};
	_Else ->
	    find_mo_from_cli(PrefixMo, ParentMo, AttrList, NoOfTries-1, WantedAnswer)
    end;
find_mo(PrefixMo, ParentMo, [MO | RestMo], AttrList, NoOfTries, WantedAnswer) ->
    LDN = ParentMo ++ "," ++ MO,
    case check_data_from_cli(AttrList, LDN, 0) of
	WantedAnswer ->
	    {LDN, WantedAnswer};
	_ ->
	    find_mo(PrefixMo, ParentMo, RestMo, AttrList, NoOfTries, WantedAnswer)
    end.

%%% ===========================================================================
%%% @doc
%%% reinstall_rbs. <br/>
%%% @spec reinstall_rbs(Rpc, Nc1)-> ok
%%% @end
%%% ===========================================================================

%% Reinstall the rbs and check when its done.
reinstall_rbs(Rpc, Nc1)->

    case check_if_sec_board() of
	"yes" ->
	    ct:log("This is secure board skip reinstall"),
	    ok;
	_ ->
	    
	    ct:pal("Reinstall"),
	    rct_rpc:call(Rpc,sysNetloader,coli_reinstall,[asdf],10000),

	    {ok, ErlNode} = rct_rpc:get_erlnode(Rpc),
	    net_kernel:disconnect(ErlNode),
	    
	    %%poll ManagedElement
	    Time = 10000,
	    timer:sleep(Time),
	    ct:pal("Wait ~p s before poll_reinstall",[Time/1000]),

	    poll_reinstall(Nc1)
    end.

poll_reinstall(Nc1)->

    Timer = 15000,
    timer:sleep(Timer),
    ct:pal("Wait ~p s before checking if node is up",[Timer/1000]),
    Response = netconf_open(Nc1,[]),
    case Response of
	{ok,_} ->  
	    case  ct_netconfc:get_config(
		    Nc1,running,
		    {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		     [{managedElementId,[],["1"]}]}) of
		{ok,_}-> 
		    ct:pal("Reinstall done"),
		    ok = ct_netconfc:close_session(Nc1, 10000),
		    ok;
		_-> 
		    ok = ct_netconfc:close_session(Nc1, 10000),
		    poll_reinstall(Nc1)
	    end;
	_-> 
	    poll_reinstall(Nc1)
    end.


netconf_open(Session, Param)->

    Return=
	case check_if_sec_board()  of
	    "yes" ->  
		ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param]);
	    _ ->
		ct_netconfc:open(Session,Param)
	end,
    Return.

check_if_sec_board()->

    Hw = ct:get_config({test_nodes,1}),
    Res = ct:get_config({Hw,secure_board}),
    Res.


%%% ===========================================================================
%%% @doc
%%% Find PM job from CLI. <br/>
%%% @spec find_pmjob_from_cli(MO, PmJob, AttributeList, MeasurementReader, MeasurementSpec, GroupRef, NoOfTries)-> {Ref, ok}	
%%% @end
%%% ===========================================================================
find_pmjob_from_cli(MO, PmJob, AttributeList, MeasurementReader, MeasurementSpec, GroupRef, NoOfTries)->
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
   
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
   
    
    ObjectAnswer =
	case find_pmjob(PmJob, AttributeList, MO, NoOfTries, Data2) of
	    ok ->
		find_pmjob_objects(MO++","++PmJob, MeasurementReader, MeasurementSpec, GroupRef);
	    nok ->		
		{no_pmjob, nok}
	end,
    ObjectAnswer.
    
%%% ===========================================================================
%%% @doc
%%% Find PM event from CLI. <br/>
%%% @spec find_pmevent_from_cli(MO, EventJob, AttributeList, EventGroupRef, EventTypeRef, EventFilerList, NoOfTries)-> 	{pm_event, ok}
%%% @end
%%% ===========================================================================
find_pmevent_from_cli(MO, EventJob, AttributeList, EventGroupRef, EventTypeRef, EventFilerList, NoOfTries)->  
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
    
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    
    ObjectAnswer =
	case find_pmevent(EventJob, AttributeList, MO, NoOfTries, Data2) of
	    ok ->
		find_pmevent_objects(MO, EventJob, EventGroupRef, EventTypeRef, EventFilerList, NoOfTries);
	    nok ->		
		{EventJob, nok}
	end,
    ObjectAnswer.

%%% ===========================================================================
%%% @doc
%%% Find single scheduler backup event from CLI. <br/>
%%% @spec find_single_scheduler_backup_event(MO, SingleEvent, AttributeList, NoOfTries)-> ok
%%% @end
%%% ===========================================================================
find_single_scheduler_backup_event(MO, SingleEvent, AttributeList, NoOfTries)->
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
    
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    
    case find_single_event(SingleEvent, Data2) of
	ok ->
	    case check_data_from_cli(AttributeList, MO++","++SingleEvent, NoOfTries) of
		ok->
		    ok;
		nok->
		    {no_scheduledTime, nok}
	    end;
	nok ->		
	    {no_singleEvent, nok}
    end.

%%% ===========================================================================
%%% @doc
%%% Find periodic scheduler backup event calender from CLI. <br/>
%%% @spec find_periodic_scheduler_backup_event_calender(MO, CalendarBasedPeriodicEvent,AttributeList, NoOfTries)-> ok
%%% @end
%%% ===========================================================================
find_periodic_scheduler_backup_event_calender(MO, CalendarBasedPeriodicEvent, 
					      AttributeList, NoOfTries)->
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    
    case find_calendar_based_periodic_event(CalendarBasedPeriodicEvent, Data2) of
	ok ->
	    case check_data_from_cli(AttributeList, MO++","++CalendarBasedPeriodicEvent, NoOfTries) of
		ok->
		    ok;
		nok->
		    {no_calendarBasedPeriodicEventCiteria, nok}
	    end;
	nok ->		
	    {no_calendarBasedPeriodicEvent, nok}
    end.

%%% ===========================================================================
%%% @doc
%%% Find periodic scheduler backup event CLI. <br/>
%%% @spec find_periodic_scheduler_backup_event(MO, PeriodicEvent,AttributeList, NoOfTries)-> ok
%%% @end
%%% ===========================================================================
find_periodic_scheduler_backup_event(MO, PeriodicEvent, 
				     AttributeList, NoOfTries)->
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    
    case find_periodic_event(PeriodicEvent, Data2) of
	ok ->
	    case check_data_from_cli(AttributeList, MO++","++PeriodicEvent, NoOfTries) of
		ok->
		    ok;
		nok->
		    {no_periodicEventCiteria, nok}
	    end;
	nok ->		
	    {no_periodicEvent, nok}
    end.
    
%%% ===========================================================================
%%% @doc
%%% Check sec mo and data event CLI. <br/>
%%% @spec check_sec_mo_and_data_from_cli(MO, SecMo, AttributeList, NoOfTries)-> ok
%%% @end
%%% ===========================================================================
check_sec_mo_and_data_from_cli(MO, SecMo, AttributeList, NoOfTries)->
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    
    case find_sec_mo(SecMo, Data2) of
	ok ->
	    case check_data_from_cli(AttributeList, MO++","++SecMo, NoOfTries) of
		ok->
		    ok;
		nok->
		    {attribut_missmatch, nok}
	    end;
	nok ->		
	    {no_sec_mo, nok}
    end.

%%% ===========================================================================
%%% @doc
%%% Check sec mo object and data event CLI. <br/>
%%% @spec check_sec_mo_object_and_data_from_cli(MO, SecMo, SecObjectList, AttributeList, NoOfTries)-> ok
%%% @end
%%% ===========================================================================
check_sec_mo_object_and_data_from_cli(MO, SecMo, SecObjectList, AttributeList, NoOfTries)->
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),

    DataMO =
	case SecMo of
	    noMo ->
		find_sec_object(SecObjectList, MO),
		MO;
	    SecMo ->
		case find_sec_mo(SecMo, Data2) of
		    ok ->
			find_sec_object(SecObjectList, MO++","++SecMo),
			MO++","++SecMo;
		    nok ->	
			ct:fail("Sec MO not found")
		end
	end,
    
    case check_data_from_cli(AttributeList, DataMO, NoOfTries) of
	ok->
	    ok;
	nok->
	    {attribut_missmatch, nok}
    end.

%%% ===========================================================================
%%% @doc
%%% Find swm log data. <br/>
%%% @spec find_swm_log_data(Mo, DataList)-> ok
%%% @end
%%% ===========================================================================
find_swm_log_data(Mo, DataList)->
    {ok, RecievedData} = rct_cli:send(cli,"show verbose "++Mo, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    find_data(DataList, Data2).

%%% ===========================================================================
%%% @doc
%%% Check push log mo and data from CLI. <br/>
%%% @spec check_push_log_mo_and_data_from_cli(Mo, PushLogMo, AttributeList, NoOfTries)-> ok
%%% @end
%%% ===========================================================================
check_push_log_mo_and_data_from_cli(Mo, PushLogMo, AttributeList, NoOfTries)->	   		    
    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli, "show "++Mo, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    
    case find_pushlog_mo(PushLogMo, Data2) of
	ok ->
	    case check_data_from_cli(AttributeList, Mo++","++PushLogMo, NoOfTries) of
		ok->
		    ok;
		nok->
		    {attribut_missmatch, nok}
	    end;
	nok ->		
	    {no_push_log_mo, nok}
    end.


%%% ===========================================================================
%%% @doc
%%% Configure TnPort from CLI. <br/>
%%% @spec configure_tnport(FieldReplaceableUnitMo, TnPortMo)-> {ok,RecievedData}
%%% @end
%%% ===========================================================================
configure_tnport(FieldReplaceableUnitMo, TnPortMo)->
    rct_cli:send(cli,"configure"),
    {ok,_RecievedData1} = rct_cli:send(cli, FieldReplaceableUnitMo), 
    {ok,RecievedData2} = rct_cli:send(cli, TnPortMo), 

    case check_recieved_data(RecievedData2) of
	ok ->
	    continue;
	nok ->
	    ct:log("TnPort MO configuration error = ~p~n",[RecievedData2]),
	    ct:fail("TnPort MO configuration error")
    end,

    
    {ok,RecievedData3} = rct_cli:send(cli,"commit"), 
    case check_recieved_data(RecievedData3) of
	ok ->
	    continue;
	nok ->
	    ct:log("TnPort MO commit error = ~p~n",[RecievedData3]),
	    ct:fail("TnPort MO commit error")
    end,
    
    rct_cli:send(cli, "top").


%%% ===========================================================================
%%% @doc
%%% Configure EthernetPort from CLI. <br/>
%%% @spec configure_ethernetport(MO, AttributeList)-> {ok,RecievedData} 
%%% @end
%%% ===========================================================================
configure_ethernetport(MO, AttributeList)->

    Options = [global,{capture, all, binary}],

    rct_cli:send(cli,"configure"),
    {ok,RecievedData1} = rct_cli:send(cli, MO), 
    case check_recieved_data(RecievedData1) of
	ok ->
	    continue;
	nok ->
	    ct:log("EthernetPort MO configuration error = ~p~n",[RecievedData1]),
	    ct:fail("EthernetPort MO configuration error")
    end,

    lists:foreach(fun(AttString) ->    
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end 
		  end, 
		  AttributeList),



    {ok,RecievedData2} = rct_cli:send(cli,"commit"), 
    case check_recieved_data(RecievedData2) of
	ok ->
	    continue;
	nok ->
	    ct:log("EthernetPort MO commit error = ~p~n",[RecievedData2]),
	    ct:fail("EthernetPort MO commit error")
    end,

    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% configure Vlan portfrom CLI. <br/>
%%% @spec configure_vlanport(MO, AttributeList)-> {ok,RecievedData} 
%%% @end
%%% ===========================================================================
configure_vlanport(MO, AttributeList)->

    Options = [global,{capture, all, binary}],

    rct_cli:send(cli,"configure"),
    {ok,RecievedData1} = rct_cli:send(cli, MO), 
    case check_recieved_data(RecievedData1) of
	ok ->
	    continue;
	nok ->
	    ct:log("VlanPort MO configuration error = ~p~n",[RecievedData1]),
	    ct:fail("VlanPort MO configuration error")
    end,

    lists:foreach(fun(AttString) ->    
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end 
		  end, 
		  AttributeList),



    {ok,RecievedData2} = rct_cli:send(cli,"commit"), 
    case check_recieved_data(RecievedData2) of
	ok ->
	    continue;
	nok ->
	    ct:log("VlanPort MO commit error = ~p~n",[RecievedData2]),
	    ct:fail("VlanPort MO commit error")
    end,

    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Configure Ipv4address from CLI. <br/>
%%% @spec configure_ipv4address(RouterMo, InterfaceIPv4Mo, InterfaceAttList, AddressIPv4Mo, AddressAttList)-> {ok,RecievedData} 
%%% @end
%%% ===========================================================================
configure_ipv4address(RouterMo, InterfaceIPv4Mo, InterfaceAttList, AddressIPv4Mo, AddressAttList)-> 

    Options = [global,{capture, all, binary}],

    rct_cli:send(cli,"configure"),
    {ok, _RecievedData1} = rct_cli:send(cli, RouterMo), 
    {ok, _RecievedData2} = rct_cli:send(cli, InterfaceIPv4Mo), 

    lists:foreach(fun(AttString) ->    
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end 
		  end, 
		  InterfaceAttList),

    {ok,RecievedData3} = rct_cli:send(cli, AddressIPv4Mo), 

    case check_recieved_data(RecievedData3) of
	ok ->
	    continue;
	nok ->
	    ct:log("ipv4address MO configuration error = ~p~n",[RecievedData3]),
	    ct:fail("ipv4address MO configuration error")
    end,

    lists:foreach(fun(AttString) ->    
			  {ok,RecievedData} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end 
		  end, 
		  AddressAttList),

    {ok,RecievedData4} = rct_cli:send(cli,"commit"), 
    case check_recieved_data(RecievedData4) of
	ok ->
	    continue;
	nok ->
	    ct:log("ipv4address MO commit error = ~p~n",[RecievedData4]),
	    ct:fail("ipv4address MO commit error")
    end,

    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Configure routetableIPv4static from CLI. <br/>
%%% @spec configure_routetableIPv4static(RouterMo, RouteTableIPv4StaticMo,Dst1Mo, Dst1AttList, NextHop1Mo,NextHop1AttList)->  {ok,RecievedData} 
%%% @end
%%% ===========================================================================
configure_routetableIPv4static(RouterMo, RouteTableIPv4StaticMo, 
			       Dst1Mo, Dst1AttList, NextHop1Mo,
			       NextHop1AttList 
			       %%Dst2Mo, Dst2AttList, NextHop2Mo,
			       %%NextHop2AttList
			      )-> 

    Options = [global,{capture, all, binary}],

    rct_cli:send(cli,"configure"),
    {ok, _RecievedData10} = rct_cli:send(cli, RouterMo), 
    {ok, _RecievedData1} = rct_cli:send(cli, RouteTableIPv4StaticMo), 
    {ok, _RecievedData2} = rct_cli:send(cli, Dst1Mo), 


    lists:foreach(fun(AttString) ->    
			  {ok,RecievedData3} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData3, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData3]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end 
		  end, 
		  Dst1AttList),

    {ok, _RecievedData4} = rct_cli:send(cli, NextHop1Mo), 

    lists:foreach(fun(AttString) ->    
			  {ok,RecievedData5} = rct_cli:send(cli, AttString), 
			  case re:run(RecievedData5, "ERROR", Options) of
			      {match, [[_Value]]} ->
				  ct:log("Attribute configuration error = ~p~n",[RecievedData5]),
				  ct:fail("Attribute configuration error");
			      nomatch ->
				  continue
			  end 
		  end, 
		  NextHop1AttList),

    rct_cli:send(cli, "up"),
    rct_cli:send(cli, "up"), 

    %% {ok, _RecievedData6} = rct_cli:send(cli, Dst2Mo), 
    %% lists:foreach(fun(AttString) ->    
    %% 			  {ok,RecievedData7} = rct_cli:send(cli, AttString), 
    %% 			  case re:run(RecievedData7, "ERROR", Options) of
    %% 			      {match, [[_Value]]} ->
    %% 				  ct:log("Attribute configuration error = ~p~n",[RecievedData7]),
    %% 				  ct:fail("Attribute configuration error");
    %% 			      nomatch ->
    %% 				  continue
    %% 			  end 
    %% 		  end, 
    %% 		  Dst2AttList),


    %% {ok, _RecievedData8} = rct_cli:send(cli, NextHop2Mo), 
    
    %% lists:foreach(fun(AttString) ->    
    %% 			  {ok,RecievedData9} = rct_cli:send(cli, AttString), 
    %% 			  case re:run(RecievedData9, "ERROR", Options) of
    %% 			      {match, [[_Value]]} ->
    %% 				  ct:log("Attribute configuration error = ~p~n",[RecievedData9]),
    %% 				  ct:fail("Attribute configuration error");
    %% 			      nomatch ->
    %% 				  continue
    %% 			  end 
    %% 		  end, 
    %% 		  NextHop2AttList),
    
    {ok,RecievedData11} = rct_cli:send(cli,"commit"), 
    case check_recieved_data(RecievedData11) of
	ok ->
	    continue;
	nok ->
	    ct:log("ipv4address MO commit error = ~p~n",[RecievedData11]),
	    ct:fail("ipv4address MO commit error")
    end,
    
    rct_cli:send(cli, "top").

%%% ===========================================================================
%%% @doc
%%% Get current UP from CLI. <br/>
%%% @spec get_current_up_from_cli() -> UpVersion
%%% @end
%%% ===========================================================================
get_current_up_from_cli() ->
   
    {ok ,RecievedData} = 
	rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,SwM=1"),
    string:tokens(RecievedData, "=\r\n ").

%%% ===========================================================================
%%% @doc
%%% Description: Increase version in cxp xml on the previous label. <br/>
%%%              This is used by create. ex dir /proj/rcs-tmp/upgrade/tcu021/
%%%              Increase the PrevLabel with IncreseNr.
%%% PrevLabel = string() , ex "CXS101549/3-R2A2956" <br/>
%%% IncreseNr = integer() <br/>
%%% @spec build_valid_full_ug_package(UgSession, DcUpName, SwVersion, IncreseNr) -> ok
%%% @end
%%% ===========================================================================
build_valid_full_ug_package(UgSession, DcUpName, SwVersion, IncreseNr) ->
 
    %% Get stp upgrade dir 
    {ok, UGPath} = rct_upgrade:get_up_dir(UgSession),

    ct:pal("Remove existing files in upgrade dir:~n~p~n",[UGPath]),
    ChmodDir= "chmod 777 "++UGPath++"/*",
    ct:pal("Chmod upgrade dir:~n~p~n",[ChmodDir]),
    os:cmd(ChmodDir),
    RemoveFiles = "rm -rf "++UGPath++"/*",
    ct:pal("Remove files in upgrade dir:~n~p~n",[RemoveFiles]),
    os:cmd(RemoveFiles),

    %% Get CXP from stp tftpboot dir
    Node = ct:get_config({test_nodes,1}),
    TftpBootdir =  ct:get_config({Node, tftpboot}),
    ct:pal("tftpboot dir:~n~p~n",[TftpBootdir]),

    CXPfile = TftpBootdir++DcUpName,
    ct:pal("CXP file: ~p", [CXPfile]),

    %% Run upgradeprep on CXP file
    NodeString = atom_to_list(Node),
    Prep_Cmd = "upgradeprep.sh -stp "++NodeString++" "++CXPfile,
    ct:pal("UpgradePrep_Cmd: ~p", [Prep_Cmd]),
    os:cmd(Prep_Cmd),
    timer:sleep(10000), % just in case!

    %% Modify cxp .xml. Construct a CXP label.
    [_DC, CXP, Index, Label, _NR] = string:tokens(SwVersion, "/-"),
    ct:pal("CXP:~p, Index: ~p, Label: ~p ~n",[CXP, Index, Label]),
 
    %% Increase label version with 1.
    ToVersionNr = integer_to_list(IncreseNr),

    LabelLenght = length(Label),
    LastElementNr = LabelLenght - 1,
    {Pre, LastElement} =  lists:split(LastElementNr,Label),

    ToVer =   
	case string:to_integer(LastElement) of
	    {error,no_integer} ->
		Label++ToVersionNr;
	    {Integer, []} ->
		Pre++integer_to_list(Integer + IncreseNr)
	end,
    
    ct:pal("UP ToVersion: ~p ", [ToVer]),

    DcCXPxml = "DC-*-up.xml",
    ct:pal("DC-CXP up .xml: ~p ", [DcCXPxml]),
   
    %%CMD = "grep "++CXP++" "++UGPath++"/"++DcCXPxml++ "| awk '{print $4}'|sed 's/version=//'|tr -d '""/>\n'",
    CMD = "grep "++CXP++" "++UGPath++"/"++DcCXPxml++ "| awk '{print $3}'|sed 's/version=//'|tr -d '""/>\n'",
    ct:pal("CMD:~n~p~n",[CMD]),
    Cmd_return = os:cmd(CMD),

    [_Id, DC_CXP] = string:tokens(Cmd_return, " \""),
    [_DC, _CXPnr, DcCXPLabel] = string:tokens(DC_CXP, "-"),
    
    %%[DcCXPLabel] = string:tokens(CXPLabel, " \""),

    ct:pal("DC-CXP up Label: ~p ", [DcCXPLabel]),

    %%%% This sed cmd change version on line that match CXP.
    %%%% Changes is done directly in DC-CXP-xml when using -i flag in sed cmd.
    CMD2 = "sed -i -e '/"++CXP++"/s/"++DcCXPLabel++"/"++ToVer++"/' "++UGPath++"/"++DcCXPxml,

    ct:pal("CMD2:~n~p~n",[CMD2]),
    os:cmd(CMD2).

%%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%%%--------------------------------------------------------------------
%%% Description: read ERROR string from CLI RecievedData
%%%--------------------------------------------------------------------
check_recieved_data(RecievedData)->
    Options = [global,{capture, all, binary}],

    case re:run(RecievedData, "ERROR", Options) of
	{match, [[_Value]]} ->
	    nok;
	nomatch ->
	    ok
    end.

%%%--------------------------------------------------------------------
%%% Description: read ERROR string in ESI
%%%--------------------------------------------------------------------
read_esi_log_file(EsiLogPath, LogFileDir, ErrorString) ->
    case os:cmd("egrep " ++  ErrorString  ++ " " ++  filename:join(EsiLogPath, LogFileDir))  of
	[]->
	    {ok,[]};
	Error -> 
	    ct:log("Error in erlang log:~p ~n",[Error]),
	    ct:fail("ESI Log: Error in erlang log")
    end.

%%%--------------------------------------------------------------------
%%% Description: check for pmd in ESI
%%%--------------------------------------------------------------------
check_esi_for_pmd(EsiLogPath, PmdDir) ->
    case os:cmd("ls " ++ filename:join(EsiLogPath,PmdDir)) of
	[] -> 
	    {ok, []};
	Pmd -> 
	    ct:log("Pmd: ~p ~n",[Pmd]),
	    ct:fail("ESI Log: Failed Pmd")
    end.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
copy_file_to_dir_and_unpack(Path, File)->

    NewPath = string:strip(Path, right, $/),
    NewDir = filename:dirname(NewPath),

    FileDistination =  filename:join(NewDir, File),
    FileSource =  filename:join(Path, File),
    
    {ok, _} = file:copy(FileSource, FileDistination),
    os:cmd("gunzip " ++ FileDistination).

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_data_from_cli(Prefix, MO) ->
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),

    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    find_mo(Prefix, Data2, []).
    
find_mo(_Prefix, [], MOs) ->
    MOs;
find_mo(Prefix, [Element | RestElement], MOs) ->
    case lists:prefix(Prefix, Element) of
	true ->
	    find_mo(Prefix, RestElement, MOs ++ [Element]);
	false ->
	    find_mo(Prefix, RestElement, MOs)
    end.
%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_pmjob(PmJob, AttributeList, MO, NoOfTries, Data2)->
    case find_mom_object(PmJob, Data2) of
	[]->
	    ct:pal("PmJob not found: ~p", [PmJob]), 
	    nok;
	[PmJob] ->
	    case check_data_from_cli(AttributeList, MO++","++PmJob, NoOfTries) of
		ok->
		    ok;
		nok->
		    ct:pal("Did not find one of this Attributes: ~p", [AttributeList]),
		    nok
	    end
    end.
%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
find_pmjob_objects(MO, MeasurementReader, MeasurementSpec, GroupRef)->
    
    {ok, RecievedData} = rct_cli:send(cli,"show "++MO++","++MeasurementReader, print),
    
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
       
    case find_mom_object(MeasurementReader, Data2) of
	[]->
	    ct:pal("MeasurementReader not found: ~p", [MeasurementReader]),
	    {no_measurementreader, nok};
	[MeasurementReader] ->
	    case find_mom_object(MeasurementSpec, Data2) of
		[]->
		    ct:pal("MeasurementSpec not found: ~p", [MeasurementSpec]),
		    {no_measurementspec, nok};
		[MeasurementSpec] ->
		    case find_mom_object(GroupRef, Data2) of
			[]->
			    ct:pal("GroupRef not found: ~p", [GroupRef]),
			    {no_ref, nok};
			[Ref] ->
			    {Ref, ok}	
		    end
	    end
    end.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_mom_object(WandtedObj, ObjectList) ->
    case lists:member(WandtedObj, ObjectList) of
	true ->
	    [WandtedObj];
	false ->
	    []
    end.
%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_pmevent(EventJob, AttributeList, MO, NoOfTries, Data2)->
    case find_mom_object(EventJob, Data2) of
	[]->
	    ct:pal("EventJob not found: ~p", [EventJob]),
	    nok;
	[EventJob] ->
	    case check_data_from_cli(AttributeList, MO++","++EventJob, NoOfTries) of
		ok->
		    ok;
		nok->
		    ct:pal("Did not find one of this Attributes: ~p", [AttributeList]),
		    nok
	    end
    end.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_pmevent_objects(MO, EventJob, EventGroupRef, EventTypeRef, EventFilterList, NoOfTries)->
    
    {ok, RecievedData} = rct_cli:send(cli,"show verbose "++MO++","++EventJob, print),
    
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),

    GroupRefAnswere = find_ref(EventGroupRef, Data2),
    TypeRefAnswere = find_ref(EventTypeRef, Data2),
    EventFilterAnswere = check_event_filter(EventFilterList, MO, EventJob, Data2, NoOfTries),

    case {GroupRefAnswere, TypeRefAnswere, EventFilterAnswere} of
	{ok, ok, ok} ->
	    {pm_event, ok};
	{{Object, nok}, ok, ok} ->
	    {Object, nok};
	{ok, {Object, nok}, ok} ->
	    {Object, nok};
	{ok, ok, {Object, nok}} ->
	    {Object, nok}
    end.

find_ref(ObjectList, Data2)->

    RefAnswere = 
	lists:foreach(fun(Object)->
			      case find_mom_object(Object, Data2) of
				  []->
				      ct:pal("Ref Object not found: ~p", [Object]),
				      {Object, nok};
				  [Object] ->
				      ok
			      end	      
		      end, 
		      ObjectList),
    RefAnswere.


check_event_filter([EventFilter|RestList], MO, EventJob, Data2, NoOfTries)->
    
    case find_mom_object(EventFilter, Data2) of
	[]->
	    ct:pal("EventFilter not found: ~p", [EventFilter]),
	    {EventFilter, nok};
	[EventFilter] ->
	    case check_data_from_cli(RestList, MO++","++EventJob, NoOfTries) of
		ok->
		    ok;
		nok->
		    {no_filterCriteria, nok}
	    end
    end.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_single_event(SingleEvent, Data2)->
    case find_mom_object(SingleEvent, Data2) of
	[]->
	    ct:pal("SingleEvent not found: ~p", [SingleEvent]), 
	    nok;
	[SingleEvent] ->
	    ok
    end.
    
%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_calendar_based_periodic_event(CalendarBasedPeriodicEvent, Data2)->
    case find_mom_object(CalendarBasedPeriodicEvent, Data2) of
	[]->
	    ct:pal("CalendarBasedPeriodicEvent not found: ~p", [CalendarBasedPeriodicEvent]), 
	    nok;
	[CalendarBasedPeriodicEvent] ->
	    ok
    end.
%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------   
find_periodic_event(PeriodicEvent, Data2)->
    case find_mom_object(PeriodicEvent, Data2) of
	[]->
	    ct:pal("PeriodicEvent not found: ~p", [PeriodicEvent]), 
	    nok;
	[PeriodicEvent] ->
	    ok
    end.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_sec_mo(SecMo, Data2)->
    case find_mom_object(SecMo, Data2) of
	[]->
	    ct:pal("Sec MO not found: ~p", [SecMo]), 
	    nok;
	[SecMo] ->
	    ok
    end.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_sec_object(SecObjectList, Mo) ->

    timer:sleep(1000),
    {ok, RecievedData} = rct_cli:send(cli,"show verbose "++Mo, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    find_object(SecObjectList, Data2).


find_object([], _Data2)->
    ok;

find_object([Object |RestObjectList], Data2)->
    case find_mom_object(Object, Data2) of
	[]->
	    ct:pal("Sec Object not found: ~p", [Object]), 
	    ct:fail("Sec Object not found");
	[Object] ->
	    find_object(RestObjectList, Data2)
    end.
%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_data([], _Data2)->
    ok;

find_data([Data |RestDataList], Data2)->
    case find_mom_object(Data, Data2) of
	[]->
	    ct:pal("MoM data not found: ~p", [Data]), 
	    ct:fail("MoM data not found");
	[Data] ->
	    find_data(RestDataList, Data2)
    end.
%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
find_pushlog_mo(PushLogMo, Data2)->
    case find_mom_object(PushLogMo, Data2) of
	[]->
	    ct:pal("Push Log MO not found: ~p", [PushLogMo]), 
	    nok;
	[PushLogMo] ->
	    ok
    end.


%%% ===========================================================================
%%% @doc
%%% Get highest cxs label  <br/>
%%% UPs = list of CXp strings, from get_ups(?NC_Session),
%%% HighestUP = string , ex "DC-CXP9024418/4-R9HM-1". <br/>
%%% @spec get_highest_label(UPs) -> HighestUP
%%% @end
%%% ===========================================================================
get_highest_label(UPs) ->
    StartUP = lists:nth(1, UPs),
    get_highest_label(UPs, 0, StartUP).
get_highest_label([], _HighestNr, HighestUP) ->
    ct:log("### The Highest UP Label : ~p", [HighestUP]),
    HighestUP;
get_highest_label([UP | T], HighestNr, HighestUP) ->

    [_DC, CXP , Rev, _NR] = string:tokens(UP, "-"),
    ct:pal("CXP:~p, Label: ~p ~n",[CXP, Rev]),

    %% [CXP, Rev] = string:tokens(UP,"-"),
    %% ct:log("### CXP: ~p, Rev: ~p", [CXP, Rev]),

    %% This construck a integer from the version.
    %% Check what integer is the highest to get latest created up label.
    {ok, Number} = version_to_integer(Rev),

    case Number > HighestNr of
    	true ->
	    NewHighestNr = Number,
	    ct:log("### Number: ~p ,  "
		   "NewHighestNr: ~p ,  "
		   "NewHighestUP: ~p", [Number, NewHighestNr, UP]),
	    get_highest_label(T, NewHighestNr, UP);
    	false ->
    	    get_highest_label(T, HighestNr, HighestUP)
    end.


%%%--------------------------------------------------------------------
%%% Description: Cosntruct a integer from the version str.
%%% The integer will increase when newer version is used.
%%% Then this is used to get the latest UP.
%%% In a version string such as R15XYZ23 the "XYZ" part may
%%% consist of 1, 2 or 3 uppercase letters.
%%%--------------------------------------------------------------------
version_to_integer(R) ->
    RE = "(R[1-9][0-9]*)([A-Z]+)([0-9]*)",

  case re:run(R, RE, [anchored]) of
    nomatch ->
      {nomatch, unspecified};
    {match, [{_, Length}, {Rpos, Rlen}, {Apos, Alen}, {Npos, Nlen}]} ->
      if
        Length < length(R) ->
          {nomatch, garbage_at_end};
        Alen > 3 ->
          {nomatch, too_many_letters};
        true ->
	  % Alen is in the range 1..3
          Rpart = string:substr(R, Rpos+2, Rlen-1),
          Apart = string:substr(R, Apos+1, Alen),
          NpartValue =
            if Nlen =:= 0 ->
              0;
            true ->
              1 + list_to_integer(string:substr(R, Npos+1, Nlen))
            end,
          if
            NpartValue > 9999 ->
              {error, out_of_range};
            true ->
	      RpartValue = list_to_integer(Rpart)*100*100*100*10000,
	      ApartValue =
		case Apart of
		  [Z] ->
		    (Z-64)*10000;
		  [Y, Z] ->
		    ((Y-64)*100 + (Z-64))*10000;
		  [X, Y, Z] ->
		    (((X-64)*100 + (Y-64))*100 + (Z-64))*10000
		end,
	      {ok, RpartValue + ApartValue + NpartValue}
          end
      end
  end.



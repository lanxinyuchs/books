%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaLicenseSupportModel.erl %
%%% @author qselert
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/4
%%%
%%% @doc ==ECIM model callback module for LM==
%%% This module implements the callback functions for the LicenseSupport model
-module(lmaLicenseSupportModel).
-vsn('/main/R11A/4').
-date('2017-09-28').
-author('qselert').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R11A/1     2017-07-13   qselert   SP086, LicenseSupport callback functions. 
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/0]).

%% These functions are needed for new COMTE API.

-export([setMoAttributes/3]).
-export([getMoAttributes/3]).
-export([getMoAttribute/2, nextMo/3]).
-export([action/4]).
-export([existsMo/2]).
-export([countMoChildren/3]).
-export([createMo/5]).
-export([deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsLM.hrl").
-include("lma.hrl").
-include("RmeLicenseSupport.hrl").

-define(SET_TIMEOUT, 2000).
-define(TIMEOUT, 5000).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @private
init() ->
    Root = ["ManagedElement", "NodeSupport", "LicenseSupport"],
    comsaLib:register_callback(Root,lmaLicenseSupportModel).

%%% @doc Returns true if the specified instance exists.
-spec existsMo([binary()], integer()) -> boolean().
existsMo(DnRev, _TransId) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
%%% -spec countMoChildren([binary()], binary(), integer()) -> non_neg_integer().
countMoChildren([DnRev], ClassName, _TransId) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(ClassName))).
  

%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% -spec getMoAttributes([binary()], [binary()], integer()) -> [com_value()].
getMoAttributes(AttrNameList, DnRev, TransId) ->
    case existsMo(DnRev, TransId) of
	false ->
	    [];
	true ->
	    [getMoAttribute([AttrName|DnRev], TransId)||AttrName<-AttrNameList]
    end.

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, Tx) ->
    case mnesia:dirty_read(lmaModel, 1) of
	[Obj] ->
	    case Obj#lmaModel.transId of 
		Tx ->
		    %% Fingerprint is set during AI, make sure that the install LKF is
		    %% finished before returning OK to avoid race condition with 
		    %% seting featureState when MOs are not created yet.
		    %% Added initial sleep due to problems seen in HV36751
		    timer:sleep(200),
		    [KeyFileObj] = mnesia:dirty_read(keyFileManagement, {"1","1","1","1"}),
		    ReportProgress = KeyFileObj#keyFileManagement.reportProgress,
		    wait_for_install_complete(ReportProgress, 2),
		    mnesia:dirty_delete(lmaModel, 1),
		    ok;
		SavedTx ->
		    logI:write_log("LicensingLog", "lmaModel", warning, "Transaction Id in db: " ++
				       integer_to_list(SavedTx) ++ " does not match transaction id in "
				   " lmaModel:finished: " ++ integer_to_list(Tx)),
		    ok
	    end;
	[] ->
	    ok
    end.

wait_for_install_complete(undefined, 0) ->
    logI:write_log("LicensingLog", "lmaModel", warning, "AI ongoing - reportProgress undefined"),
    ok;
wait_for_install_complete(undefined, No) ->
    logI:write_log("LicensingLog", "lmaModel", warning, "AI ongoing - reportProgress undefined,"
		  " try to wait 200ms"),
    timer:sleep(200),
    [KeyFileObj] = mnesia:dirty_read(keyFileManagement, {"1","1","1","1"}),
    wait_for_install_complete(KeyFileObj#keyFileManagement.reportProgress, No-1),
    ok;
wait_for_install_complete(ReportProgress, No) ->
    case ReportProgress#'AsyncActionProgress'.state of
	?ActionStateType_FINISHED ->
	    logI:write_log("LicensingLog", "lmaModel", info, "AI ongoing - LKF installation ready!"),
	    ok;
	_Else ->
	    logI:write_log("LicensingLog", "lmaModel", info, "AI ongoing - wait for the LKF to be "
			   "installed...."),
	    timer:sleep(200),
	    [KeyFileObj] = mnesia:dirty_read(keyFileManagement, {"1","1","1","1"}),
	    NewReportProgress = KeyFileObj#keyFileManagement.reportProgress,
	    wait_for_install_complete(NewReportProgress, No)
    end.
	    
	    



%%% setMoAttributes(AttrNames::[com_named_attribute()], ReverseDn::[binary()],
%%%                 TransId:integer()) -> ok |{error, Reason}
%%% Set a list of attribute values (instead of only one).
setMoAttributes(NamedAttributes = [{AttrName, ComValue}], DnRev, _TransId) ->
	    {9, LicenseAreaId} = ComValue,
	    case lmaGlms:set_licenseAreaId(LicenseAreaId) of
		{ok, licenseareaid_updated} ->
		    logI:write_log("LicensingLog", "lmaLicenseSupport", info, "setMoAttribute licenseAreaId "
				   "to: "++ binary_to_list(LicenseAreaId)),
		    %ok = lmaModel:store_transId_if_ai(TransId),
		    Table = table(comsaGeneric:class(DnRev)),
		    comsaGeneric:set(DnRev, Table, types(Table), NamedAttributes),
		    ok;
		{error, Reason} ->
		    logI:write_log("LicensingLog", "lmaLicenseSupport", warning,
				   "Set licenseAreaId failed"),
		    {abort, Reason}
	    end.
    


%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.
%%% -spec createMo([binary()],
%%% 	       mo_attribute_name(),
%%% 	       binary(),
%%% 	       [com_named_attribute()],
%%% 	       integer()) ->
%%% 	  {ok, tuple()}.
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

deleteMo(DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table).

%%% @doc Gets the next MO
-spec nextMo([binary()], undefined|tuple(), integer()) -> {ok, undefined|tuple()}.
nextMo(Dn, Key, _TxHandle) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).



%%% action(Name::binary(), ReverseDn::[binary()], NamedParams::[com_named_parameter()], TransId:integer())
%%% Execute an action.
%%% Because the list of parameter is named, optional parameters may be excluded in the list, and the order
%%% of parameters is no longer fixed.
action(<<"initialize">>, [<<"1">>,<<"LicenseSupport">>,<<"1">>,
			<<"NodeSupport">>,<<"1">>,<<"ManagedElement">>],
       _NamedParams, _TransId) ->
    logI:write_log("LicensingLog", "lmaLicenseSupportModel", info, "initialize action");
action(<<"installAreaLicenseKeys">>, _DnRev, NamedParams, _TransId) ->
    [{<<"keys">>,{9, KeysBin}}] = NamedParams, %%  keys is a string. 
    logI:write_log("LicensingLog", "lmaLicenseSupportModel", info, "installAreaLicenseKeys action"),
	try lmaGlms:install_area_license_keys(KeysBin) of
	    {ok, ResultCode} ->
	        io:format("IALKF Result received ResultCode ~p~n",[ResultCode]),
		    logI:write_log("LicensingLog", "lmaLicenseSupportModel", info, "installAreaLicenseKeys OK"),
		    {12, ResultCode};	
		{error, Reason} ->
		    io:format("IALKF Result ERROR ~n"),
		    logI:write_log("LicensingLog", "lmaLicenseSupportModel", info, "installAreaLicenseKeys ERROR"),
		    {error, Reason}
	catch _:Reason ->
		    io:format("IALKF Result NOT OK ~n"),
		    logI:write_log("LicensingLog", "lmaLicenseSupportModel", warning,
				   "Install area LKF failed"),
		    lmaLib:error_msg("Install area LKF failed: ~p~n", [Reason]),
		    {error, <<"Timeout for install area keys">>}
	end.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
table("LicenseSupport") -> licenseSupport.

types(licenseSupport) -> ?licenseSupport_types.


values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

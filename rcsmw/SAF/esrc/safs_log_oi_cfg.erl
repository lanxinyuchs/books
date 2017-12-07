%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_log_oi_cfg.erl
%%
%% Description:
%%    This file handles the IMM OI functions for LogStream configuration
%%    objects.
%%
%%--------------------------------------------------------------------
-module(safs_log_oi_cfg).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_log.hrl").
%%-include("safs_imm_ct_p.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------
-export([set_creation_timestamp/2,
	 set_creation_timestamp/3]).

-export([get_imm_cfg/1]).

%%----------------------------------------------------------------------
%% Life-cycle
%%----------------------------------------------------------------------
-export([
         initialize/0,
         finalize/1
        ]).

%%----------------------------------------------------------------------
%% OI callback functions
%%----------------------------------------------------------------------
-export([ccb_object_create_2/5,
	 ccb_object_delete/3,
	 ccb_object_modify_2/4,
	 ccb_completed/2,
	 ccb_apply/2,
	 ccb_abort/2,
	 rt_attr_update/3]).

%%--------------------------------------------------------------------
%% Support functions
%%--------------------------------------------------------------------
-export([bitmap_to_severity/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(LOG_OI_CCB_NAME, <<"safLogServiceCfg">>).
-define(SA_LOG_STREAM_CFG, 'SaLogStreamConfig').
-define(SYSTEM_LOG, <<"safLgStrCfg=saLogSystem,safApp=safLogService">>).
-define(SA_SEV_FILTER, "saLogStreamSeverityFilter").

-define(CFG_ATTRS, [<<"saLogStreamFileName">>,
		    <<"saLogStreamPathName">>,
		    <<"saLogStreamMaxLogFileSize">>,
		    <<"saLogStreamFixedLogRecordSize">>,
		    <<"saLogStreamLogFullAction">>,
		    <<"saLogStreamMaxFilesRotated">>,
		    <<"saLogStreamLogFileFormat">>,
		    <<"saLogStreamSeverityFilter">>]).

%% Temporary macro definitions until conflict between IMM an LOG hrl is fixed.
-define(SAF_VSN, #safsVersion{releaseCode  = $A,
			      majorVersion = 2,
			      minorVersion = 11}).

-define(SA_TIME, sa_imm_attr_satimet).
-define(SA_UINT_32, sa_imm_attr_sauint32t).
-define(SA_ATTR_REP, sa_imm_attr_values_replace).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------



%%====================================================================
%% External API
%%====================================================================
initialize() ->
    try
	{ok, OiHandle, _Vsn} = safs_imm_oi:initialize(?MODULE, ?SAF_VSN),
	ok = safs_imm_oi:implementer_set(OiHandle, ?LOG_OI_CCB_NAME),
	case safs_imm_oi:class_implementer_set(OiHandle, ?SA_LOG_STREAM_CFG) of
	    ok ->
		{ok, OiHandle};
	    Error ->
		finalize(OiHandle),
		Error
	end
    catch _:_ ->
	    {error, oi_cfg_initialize_failed}
    end.


finalize(undefined) ->
    ok;

finalize(OiHandle) ->
    safs_imm_oi:class_implementer_release(OiHandle, ?SA_LOG_STREAM_CFG),
    safs_imm_oi:implementer_clear(OiHandle),
    safs_imm_oi:finalize(OiHandle).


set_creation_timestamp(OiHandle, ObjectName) ->
    set_creation_timestamp(OiHandle, ObjectName, timestamp()).


set_creation_timestamp(OiHandle, ObjectName, Timestamp) ->
    AttrMods = get_timestamp_attr_mods(Timestamp),
    safs_imm_oi:rt_object_update_2(OiHandle, ObjectName, [AttrMods]).


get_imm_cfg(StreamName) ->
    get_imm_cfg(StreamName, ?CFG_ATTRS).


get_imm_cfg(StreamName, Attrs) ->
    try
	{ok, OmHandle, _} = safs_imm_om:initialize(undefined, ?SAF_VSN),
	{ok, AH} = safs_imm_om:accessor_initialize(OmHandle),
	{ok, AttrVals} =
	    safs_imm_om:accessor_get_2(AH, to_bin(StreamName), Attrs),
	safs_imm_om:accessor_finalize(AH),
	safs_imm_om:finalize(OmHandle),
	lists:flatmap(fun attr_val_to_cfg/1, AttrVals)
    catch _:E ->
	    ST = erlang:get_stacktrace(),
	    error_logger:format("Failed to fetch IMM data:\n\t~p\n"
				"StreamName:\n\t~p\n"
				"Stacktrace:\n\t~p\n",
				[E, StreamName, ST]),
	    []
    end.

%%====================================================================
%% Callback functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: ccb_object_create_2/5
%% Description:
%%--------------------------------------------------------------------
ccb_object_create_2(_OiHandle, _CcbId, _ClassName, _ParentName, _Attr) ->
    %% {error, ?SA_ERR_BAD_OPERATION}.
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_object_delete/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_delete(_OiHandle, _CcbId, _ObjectName) ->
    {error, ?SA_ERR_BAD_OPERATION}.

%%--------------------------------------------------------------------
%% Function: ccb_object_modify_2/4
%% Description:
%%--------------------------------------------------------------------
ccb_object_modify_2(_, CcbId, ObjectName,
		    [#safsImmAttrModification_2{modType = Type,
						modAttr = ModAttr}])
  when ObjectName =:= ?SYSTEM_LOG,
       Type =:= ?SA_ATTR_REP,
       is_record(ModAttr, safsImmAttrValues_2) ->
    case to_list(ModAttr#safsImmAttrValues_2.attrName) of
	?SA_SEV_FILTER ->
	    filter_modify(ModAttr, ObjectName, CcbId);
	_Other ->
	    {error, ?SA_ERR_BAD_OPERATION}
    end;

ccb_object_modify_2(_OiHandle, _CcbId, _ObjectName, _AttrMods) ->
    {error, ?SA_ERR_BAD_OPERATION}.

%%--------------------------------------------------------------------
%% Function: ccb_completed/2
%% Description:
%%--------------------------------------------------------------------
ccb_completed(_OiHandle, _CcbId) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_apply/2
%% Description:
%%--------------------------------------------------------------------
ccb_apply(_OiHandle, CcbId) ->
    call_server({filter_apply, CcbId}).

%%--------------------------------------------------------------------
%% Function: ccb_abort/2
%% Description:
%%--------------------------------------------------------------------
ccb_abort(_OiHandle, CcbId) ->
    call_server({filter_abort, CcbId}).

%%--------------------------------------------------------------------
%% Function: rt_attr_update/3
%% Description:
%%--------------------------------------------------------------------
rt_attr_update(OiHandle, ObjectName, AttributeNames) ->
    safs_log_oi_rt:rt_attr_update(OiHandle, ObjectName, AttributeNames).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
filter_modify(#safsImmAttrValues_2{attrValues = [AttrVal]} = ModAttr,
	      ObjectName, CcbId)
  when ModAttr#safsImmAttrValues_2.attrValueType =:= ?SA_UINT_32,
       is_record(AttrVal, safsImmAttrValue) ->
    case AttrVal#safsImmAttrValue.sauint32 of
	Val when 0 =< Val, Val =< 127 ->
	    Filter = bitmap_to_severity(Val),
	    Call = {filter_modify, CcbId, to_list(ObjectName), Filter},
	    Res = call_server(Call),
	    ccb_cb_res(Res);
	_Invalid ->
	    {error, ?SA_ERR_BAD_OPERATION}
    end;

filter_modify(_ModAttr, _ObjectName, _CcbId) ->
    {error, ?SA_ERR_BAD_OPERATION}.


bitmap_to_severity(Val) ->
    Map = [16#1, 16#2, 16#4, 16#8, 16#10, 16#20, 16#40],
    [Em, Al, Cr, Er, Wa, No, In] = [(Val band B) =:= B || B <- Map],
    #safsLogSeverityFlags{saLogSevFlagEmergency = Em,
			  saLogSevFlagAlert = Al,
			  saLogSevFlagCritical = Cr,
			  saLogSevFlagError = Er,
			  saLogSevFlagWarning = Wa,
			  saLogSevFlagNotice = No,
			  saLogSevFlagInfo = In}.


get_timestamp_attr_mods(Val) ->
    AttrVal = #safsImmAttrValue{satime = Val},
    AttrVals = #safsImmAttrValues_2{attrName = saLogStreamCreationTimestamp,
				    attrValueType = ?SA_TIME,
				    attrValuesNumber = 1,
				    attrValues = [AttrVal]
				   },
    #safsImmAttrModification_2{modType = ?SA_ATTR_REP,
			       modAttr = AttrVals}.


attr_val_to_cfg({<<"saLogStreamFileName">>, _, [Name]}) ->
    [{logFileName, to_list(Name)}];
attr_val_to_cfg({<<"saLogStreamPathName">>, _, [Path]}) ->
    [{logFilePathName, to_list(Path)}];
attr_val_to_cfg({<<"saLogStreamMaxLogFileSize">>, _, [Size]}) ->
    [{maxLogFileSize, Size}];
attr_val_to_cfg({<<"saLogStreamFixedLogRecordSize">>, _, [Size]}) ->
    [{maxLogRecordSize, Size}];
attr_val_to_cfg({<<"saLogStreamLogFullAction">>, _, [Val]}) ->
    [{logFileFullAction, to_full_action(Val)}];
attr_val_to_cfg({<<"saLogStreamMaxFilesRotated">>, _, [Val]}) ->
    [{maxFilesRotated, Val}];
attr_val_to_cfg({<<"saLogStreamLogFileFormat">>, _, [Format]}) ->
    [{logFileFmt, to_list(Format)}];
attr_val_to_cfg({<<"saLogStreamSeverityFilter">>, _, [Val]}) ->
    [{severityFilter, bitmap_to_severity(Val)}];
attr_val_to_cfg(_) ->
    [].


to_full_action(A) ->
    safs_log_oi_rt:to_full_action(A).


ccb_cb_res(ok) ->
    ok;
ccb_cb_res(_Error) ->
    {error, ?SA_ERR_BAD_OPERATION}.


call_server(Request) ->
    gen_server:call(?SERVER, Request, infinity).


to_list(B) when is_binary(B) ->
    binary_to_list(B);
to_list(L) when is_list(L) ->
    L;
to_list(A) when is_atom(A) ->
    atom_to_list(A).


to_bin(L) when is_list(L) ->
    list_to_binary(L);
to_bin(B) when is_binary(B) ->
    B;
to_bin(A) when is_atom(A) ->
    to_bin(atom_to_list(A)).


timestamp() ->
    safs_log_oi_rt:timestamp().

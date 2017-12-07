%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_log_oi_rt.erl
%%
%% Description:
%%    This file handles the IMM OI functions for LogStream runtime objects.
%%
%%--------------------------------------------------------------------
-module(safs_log_oi_rt).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_log.hrl").
%% -include("safs_imm_ct_p.hrl").
%% -include("om.hrl").
%% -include("safs_imm_om.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
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
-export([admin_operation_2/5]).
-export([rt_attr_update/3]).

%%----------------------------------------------------------------------
%% RT Object API
%%----------------------------------------------------------------------
-export([
         stream_create/4,
	 stream_delete/2
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 to_full_action/1,
	 timestamp/0
        ]).

%%--------------------------------------------------------------------
%% Test exports
%%--------------------------------------------------------------------
%%-compile(export_all).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

-define(SAF_VSN, #safsVersion{releaseCode  = $A,
			      majorVersion = 2,
			      minorVersion = 11}).

-define(LOG_OI_RT_NAME, <<"safLogServiceRt">>).
%% -define(SA_LOG_STREAM, <<"SaLogStream">>).
-define(SA_LOG_STREAM, "SaLogStream").

-define(LOG_PARENT, <<"safApp=safLogService">>).

-define(SA_LOG_ADMIN_CHANGE_FILTER, 1).

-define(SA_STR, sa_imm_attr_sastringt).
-define(SA_UINT_64, sa_imm_attr_sauint64t).
-define(SA_UINT_32, sa_imm_attr_sauint32t).
-define(SA_TIME, sa_imm_attr_satimet).
-define(SA_ATTR_REP, sa_imm_attr_values_replace).

-define(SA_NUM_OPENERS, <<"saLogStreamNumOpeners">>).
-define(SA_SEV_FILTER, <<"saLogStreamSeverityFilter">>).

-define(KIL, 1000).
-define(MEG, ?KIL * ?KIL).
-define(TER, ?KIL * ?MEG).

-define(LOG_WRAP_INT,   1).
-define(LOG_HALT_INT,   2).
-define(LOG_ROTATE_INT, 3).

-define(TRUE_INT,  1).
-define(FALSE_INT, 0).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External API
%%====================================================================
initialize() ->
    try
	{ok, OiHandle, _Vsn} = safs_imm_oi:initialize(?MODULE, ?SAF_VSN),
	ok = safs_imm_oi:implementer_set(OiHandle, ?LOG_OI_RT_NAME),
	{ok, OiHandle}
    catch _:_ ->
	    {error, oi_rt_initialize_failed}
    end.


finalize(undefined) ->
    ok;

finalize(OiHandle) ->
    safs_imm_oi:implementer_clear(OiHandle),
    safs_imm_oi:finalize(OiHandle).


stream_create(OiHandle, ObjectName, CA, Severity) ->
    StreamName = to_stream_name(ObjectName),
    RDN = to_imm_attr({safLgStr, ?SA_STR, ltb(StreamName)}),
    Attrs = convert_attributes(CA, Severity),
    ok = safs_imm_oi:rt_object_create_2(OiHandle,
					ltb(?SA_LOG_STREAM),
					?LOG_PARENT,
					[RDN | Attrs]).


stream_delete(OiHandle, ObjectName) ->
    safs_imm_oi:rt_object_delete(OiHandle, ObjectName).


%%====================================================================
%% Callback functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: rt_attr_update/3
%% Description:
%%--------------------------------------------------------------------
rt_attr_update(OiHandle, ObjectName, [?SA_NUM_OPENERS]) ->
    case call_server({get_num_openers, btl(ObjectName)}, 5000) of
	{ok, N} ->
	    AttrMods = get_num_openers_attr_mods(N),
	    safs_imm_oi:rt_object_update_2(OiHandle, ObjectName, [AttrMods]);
	_Error ->
	    {error, ?SA_ERR_FAILED_OPERATION}
    end;

rt_attr_update(_OiHandle, _ObjectName, _AttributeNames) ->
    {error, ?SA_ERR_FAILED_OPERATION}.

%%--------------------------------------------------------------------
%% Function: admin_operation_2/5
%% Description:
%%--------------------------------------------------------------------
admin_operation_2(OiHandle, Invocation, ObjectName, OperationId, Params)
  when OperationId =:= ?SA_LOG_ADMIN_CHANGE_FILTER,
       0 =< Params, Params =< 127 ->
    Res = change_filter(OiHandle, ObjectName, Params),
    safs_imm_oi:admin_operation_result(OiHandle, Invocation, adm_op_res(Res));

admin_operation_2(OiHandle, Invocation, _ObjectName, _OperationId, _Params) ->
    Res = ?SA_ERR_INVALID_PARAM,
    safs_imm_oi:admin_operation_result(OiHandle, Invocation, Res).

%%%===================================================================
%%% Internal functions
%%%===================================================================
change_filter(OiHandle, ObjectName, Params) ->
    Filter = safs_log_oi_cfg:bitmap_to_severity(Params),
    case call_server({change_filter, btl(ObjectName), Filter}, 10000) of
	ok ->
	    AttrMods = get_filter_attr_mods(Params),
	    safs_imm_oi:rt_object_update_2(OiHandle, ObjectName, [AttrMods]);
	{error, timeout} ->
	    {error, ?SA_ERR_TIMEOUT};
	_Error ->
	    {error, ?SA_ERR_LIBRARY}
    end.


get_filter_attr_mods(Val) ->
    AttrVal = #safsImmAttrValue{sauint32 = Val},
    AttrVals = #safsImmAttrValues_2{attrName = ?SA_SEV_FILTER,
				    attrValueType = ?SA_UINT_32,
				    attrValuesNumber = 1,
				    attrValues = [AttrVal]
				   },
    #safsImmAttrModification_2{modType = ?SA_ATTR_REP,
			       modAttr = AttrVals}.


get_num_openers_attr_mods(Val) ->
    AttrVal = #safsImmAttrValue{sauint32 = Val},
    AttrVals = #safsImmAttrValues_2{attrName = ?SA_NUM_OPENERS,
				    attrValueType = ?SA_UINT_32,
				    attrValuesNumber = 1,
				    attrValues = [AttrVal]
				   },
    #safsImmAttrModification_2{modType = ?SA_ATTR_REP,
			       modAttr = AttrVals}.



% call_server(Request) ->
%     call_server(Request, infinity).

call_server(Request, Timeout) ->
    case catch gen_server:call(?SERVER, Request, Timeout) of
	{'EXIT', {timeout, _}} ->
	    {error, timeout};
	{'EXIT', _Reason} ->
	    {error, call_failed};
	Res ->
	    Res
    end.

adm_op_res(ok) ->
    ok;

adm_op_res({error, Res}) ->
    Res.


to_stream_name(ObjectName) ->
    case string:tokens(btl(ObjectName), "=,") of
	["safLgStr", StreamName, "safApp", "safLogService"] ->
	    StreamName;
	_ ->
	    btl(ObjectName)
    end.


convert_attributes(CA, Severity) ->
    TS = timestamp(),
    #safsLogFileCreateAttributes_2{logFileName     = FileName,
				   logFilePathName = FilePath,
				   maxLogFileSize = FileSize,
				   maxLogRecordSize = RecSize,
				   haProperty = HaProp,
				   logFileFullAction = FFA,
				   maxFilesRotated = MaxRot,
				   logFileFmt = Fmt} = CA,
    SevFilter = severity_to_bitmap(Severity),
    AttrList = [{saLogStreamFileName, ?SA_STR, ltb(FileName)},
		{saLogStreamPathName, ?SA_STR, ltb(FilePath)},
		{saLogStreamMaxLogFileSize, ?SA_UINT_64, FileSize},
		{saLogStreamFixedLogRecordSize, ?SA_UINT_32, RecSize},
		{saLogStreamHaProperty, ?SA_UINT_32, bool_to_int(HaProp)},
		{saLogStreamLogFullAction, ?SA_UINT_32, to_full_action(FFA)},
		{saLogStreamMaxFilesRotated, ?SA_UINT_32, MaxRot},
		{saLogStreamLogFileFormat, ?SA_STR, ltb(Fmt)},
		{saLogStreamSeverityFilter, ?SA_UINT_32, SevFilter},
		{saLogStreamCreationTimestamp, ?SA_TIME, TS}],
    [to_imm_attr(Attr) || Attr <- AttrList].


severity_to_bitmap(Sev) ->
    #safsLogSeverityFlags{saLogSevFlagEmergency = Em,
			  saLogSevFlagAlert = Al,
			  saLogSevFlagCritical = Cr,
			  saLogSevFlagError = Er,
			  saLogSevFlagWarning = Wa,
			  saLogSevFlagNotice = No,
			  saLogSevFlagInfo = In} = Sev,
    Flags = [bool_to_int(Flag) || Flag <- [Em, Al, Cr, Er, Wa, No, In]],
    Map = [16#1, 16#2, 16#4, 16#8, 16#10, 16#20, 16#40],
    lists:foldl(fun({Fl, B}, Acc) ->
			Fl * B + Acc
		end, 0, lists:zip(Flags, Map)).


bool_to_int(true) ->
    ?TRUE_INT;
bool_to_int(false) ->
    ?FALSE_INT;
bool_to_int(?TRUE_INT) ->
    ?TRUE_INT;
bool_to_int(?FALSE_INT) ->
    ?FALSE_INT.


to_full_action(?LOG_WRAP_INT) ->
    ?LOG_WRAP;
to_full_action(?LOG_HALT_INT) ->
    ?LOG_HALT;
to_full_action(?LOG_ROTATE_INT) ->
    ?LOG_ROTATE;
to_full_action(Other) ->
    Other.


to_imm_attr({Name, Type, Val}) ->
    #safsImmAttrValues_2{attrName = Name,
			 attrValueType = Type,
			 attrValues = [to_imm_attr_val(Type, Val)]}.

to_imm_attr_val(?SA_STR, Val) ->
    #safsImmAttrValue{sastring = Val};
to_imm_attr_val(?SA_UINT_64, Val) ->
    #safsImmAttrValue{sauint64 = Val};
to_imm_attr_val(?SA_UINT_32, Val) ->
    #safsImmAttrValue{sauint32 = Val};
to_imm_attr_val(?SA_TIME, Val) ->
    #safsImmAttrValue{satime = Val}.



ltb(L) when is_list(L) ->
    list_to_binary(L);
ltb(B) when is_binary(B) ->
    B.


btl(B) when is_binary(B) ->
    binary_to_list(B);
btl(L) when is_list(L) ->
    L.


timestamp() ->
    {MeS, S, MiS} = os:timestamp(),
    ?TER * (?MEG * MeS + S) + ?KIL * MiS.

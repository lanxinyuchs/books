%%% ------------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ------------------------------------------------------------------------
%%% %CCaseFile:	pmsSession2.erl %
%%% Author:	
%%% @private
%%% Description:
%%%
%%% Session module for PMI2.
%%% 
%%% 
%%% 
%%% ------------------------------------------------------------------------
-module(pmsSession2).
-behaviour(gen_server).
-vsn('/main/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-30').
-author('eolaand').
-shaid('d72ab94947b83b0675b19cea84fbe5442686de2a').

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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    --------------------------------------
%%% R1A/1      2014-08-18 uabesvi     Created
%%% R9A/1      2017-02-06 uabesvi     Added CEC proc to log
%%% ------------------------------------------------------------------------
%%%
%%% ------------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ------------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ------------------------------------------------------------------------

%%====================================================
%% Real interface
%%====================================================

%% CEC interfaces
-export([cec_setup/1]).

%% gen_server interfaces
-export([init/1,
	 handle_cast/2,
	 handle_call/3,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsPm.hrl").
-include("pms.hrl").


-define(SERVER, ?MODULE).
-define(PMI2_MOD, pmsPmI2).

-define(INITIALIZE,     1).
-define(INITIALIZE_RES, 2).
-define(COUNTER_MAP,    3).
-define(SUBSCRIBE_ROP,  4).
-define(REPORT_ROP,     5).
-define(REPORT_SC,      6).
-define(DATA_ROP,       7).
-define(DATA_SC,        8).
-define(FINALIZE,       9).

-define(END_TAG,   <<0>>).
-define(END_TAG_4, <<0,0,0,0>>).

-define(SUBSCRIBE_TO,  5000).

-define(PMI2_OK, 1).
-define(PMI2_UNKNOWN_COUNTER_MAP_ID, 3).
-define(PMI2_BAD_PARAMETER, 4).
-define(PMI2_INTERNAL_ERROR, 5).

-define(DEFAULT_BL_TO, 10).
-define(LONG_BL_TO, 30).

-record(state, {socket, 
		pms_handle}).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%========================================================================
%% cec_setup(Socket) -> SessionPid
%% 
%% CEC setup request.
%% Start a Session process handling the socket.
%%========================================================================
cec_setup(Socket) ->
    ?LOG_RAM(?SEV_1, {"cec_setup handling socket: ~p~n", [Socket]}),
    %% start a per-session genserver
    {ok, SessionPid} = gen_server:start(?MODULE, Socket, []),
    %% and return so that the connection can be transferred too
    SessionPid.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% init(Socket) -> {ok, State}
%%========================================================================
init(Socket) ->
    ?LOG_RAM(?SEV_1, "Started~n"),
    {ok, #state{socket = Socket}}.


%%========================================================================
%% handle_call(Command, From, State) -> {reply, Res, State}
%%========================================================================
handle_call(get_proc_info, _From, State) ->
    {reply, 
     {session2, 
      [{loop_size, erts_debug:flat_size(State)}, 
       {proc_info, pmsLib:get_proc_info(self())}]}, 
     State};

handle_call(Command, From, S) ->
    ?LOG_RAM(?SEV_5, 
	     {"Received unknown handle_call msg ~p~n", [{Command, From}]}),
    {reply, Command, S}.

%%========================================================================
%% handle_cast(Message, State) -> {noreply, State}
%%========================================================================
handle_cast(Msg, S) ->
    ?LOG_RAM(?SEV_5, {"Received unknown handle_cast msg ~p~n", [Msg]}),
    {noreply, S}.

%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
handle_info({pmi2SubscribeRop, {GP, SubscribeSpec}}, S) ->
    Data = encode_subscribe_rop(GP, SubscribeSpec),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_info({pmi2ReportRop, {GP, ReportId, MaxReportingTime}}, S) ->
    Data = encode_report_rop(GP, ReportId, MaxReportingTime),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_info({pmi2ReportShowCounters,
	     {ReportId, MoInstLDN, ShowCountersSpec, MaxReportingTime}},
	    S) ->
    Data = encode_report_sc(ReportId, 
			    MoInstLDN, 
			    ShowCountersSpec,
			    MaxReportingTime),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_info({tcp, _, <<Msg:1/native-unsigned-integer-unit:32, Data/binary>>},
	    #state{socket = Socket} = S) ->
    inet:setopts(Socket, [{active, once}]),
    ?LOG_RAM(?SEV_5, "activate once ~n"),
    case handle_app_msg(Msg, Data, S) of
	ok              -> {noreply, S};
	{ok, Handle}    -> {noreply, S#state{pms_handle = Handle}};
	{error, Reason} -> {stop, Reason, S};
	{stop, Reason}  -> {stop, Reason, S}
    end;

handle_info({tcp_closed, Reason}, S) 
  when S#state.pms_handle =/= undefined ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"tcp_closed ~p~nFinalize and terminate.", [Reason]}),
    ?PMI2_MOD:pmi2Finalize(S#state.pms_handle),
    {stop, normal, S};

handle_info({tcp_closed, Reason}, S) ->
    ?LOG_RAM(?SEV_WARNING, {"tcp_closed ~p~n", [Reason]}),
    {stop, tcp_closed, S};

handle_info(Info, S) ->
    ?LOG_RAM(?SEV_5, {"Received unknown handle_info msg ~p~n", [Info]}),
    {noreply, S}.


%%========================================================================
%% code_change(OldVsn, State, Extra) -> {ok, State}
%%========================================================================
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%========================================================================
%% terminate(Reason, State) -> Reason
%%========================================================================
terminate(Reason, _S) ->
    ?LOG_RAM(?SEV_1, {"Terminated. Reason = ~p~n", [Reason]}),
    Reason.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% handle_app_msg(Msg, Subscriber) -> ok | {stop, Reason}
%% 
%% Check the message and take actions.
%%========================================================================
handle_app_msg(?INITIALIZE, Data, S) ->
    case decode_init_data(Data) of
	{ok, {Callbacks, CounterMap, CecProcName}} -> 
	    ?LOG_RAM(?SEV_1, 
		     {" INITIALIZE PMI2 <=== ~n"
		      " CounterMap ~p~n",
		      [CounterMap]}),
	    Result = ?PMI2_MOD:pmi2Initialize(Callbacks, CounterMap),
	    send_init_result(S, Result, CecProcName);
	Error ->
	    send_init_result(S, Error, undefined)
    end;

handle_app_msg(?COUNTER_MAP, Maps, #state{pms_handle = Handle}) ->
    DecodedMaps = decode_counter_maps(Maps),
    ?LOG_RAM([{?SEV_1, " COUNTER_MAP PMI2 <=== ~n"},
	      {?SEV_5, 
	       ?LFUN({" COUNTER_MAP PMI2 <=== ~n  Maps = ~p~n", 
		      [pmsLib:log_trunc_cm(DecodedMaps)]})}]),
    ?PMI2_MOD:pmi2CounterMap(Handle, DecodedMaps);

handle_app_msg(?DATA_ROP, Data, S) ->
    {GP, RequestId, ValBuns, FinalFragment} = decode_data_rop(Data),
    Res = ?PMI2_MOD:pmi2DataRop(S#state.pms_handle, 
				GP, 
				RequestId, 
				ValBuns, 
				FinalFragment),
    %% ?LOG_RAM(?SEV_1, {"DATA_ROP PMI2 ~n  Result = ~p~n", [Res]}),
    ?LOG_RAM(?SEV_1, {"DATA_ROP PMI2 ~n  Result = ~p~n", [Res]}, 
    	     ?DEFAULT_BL_TO),
    Res;

handle_app_msg(?DATA_SC, Data, S) ->
    {RequestId, Result, ErrorStr, Vals} = decode_data_sc(Data),
    ?LOG_RAM(?SEV_1, " DATA_SC PMI2 <=== ~n"),
    ?PMI2_MOD:pmi2DataShowCounters(S#state.pms_handle, 
				   RequestId, 
				   Result, 
				   ErrorStr, 
				   Vals);

handle_app_msg(?FINALIZE, Data, S) ->
    _DecodedData = decode_finalize(Data),
    ?LOG_RAM(?SEV_5, {" FINALIZE PMI2 <===  ~p~n", [Data]}),

    ?PMI2_MOD:pmi2Finalize(S#state.pms_handle),
    {stop, normal};

handle_app_msg(Msg, Data, _S) ->
    ?LOG_RAM(?SEV_WARNING, {" UNKNOWN MESSAGE <=== ~n ~p  ~p~n", [Msg, Data]}).


%%========================================================================
%% send_init_result(State, Result, CecProcId) -> 
%%    Result.
%%
%% Result = {ok, Handle} | {error, Reason}
%% 
%%========================================================================
send_init_result(S, Result, CecProcName) ->
    FormatedResult = format_init_result(Result, CecProcName),
    DataRes = <<?INITIALIZE_RES:1/native-unsigned-integer-unit:32, 
	       FormatedResult:1/native-unsigned-integer-unit:32>>,
    ?LOG_RAM(?SEV_1, 
	     {" ===> INITIALIZE RES_PMI2  ~n"
	      " DataREs ~p~n",
	      [DataRes]}),
    gen_tcp:send(S#state.socket, DataRes),
    Result.
    

format_init_result({ok, AppJobPid}, CecProcName) ->
    pmsAppJob:cec_proc_name(AppJobPid, CecProcName),
    ?PMI2_OK;

format_init_result({error, ?ERR_UNKNOWN_MAP_ID}, _) ->
    ?PMI2_UNKNOWN_COUNTER_MAP_ID;

format_init_result({error, ?ERR_INCONSISTENT_CALLBACKS}, _) ->
    ?PMI2_BAD_PARAMETER;

format_init_result({error, _}, _) ->
    ?PMI2_INTERNAL_ERROR.
    

%%========================================================================
%% decode_init_data(Data) -> 
%%    {{SubscrCb, ReportRopCb, ReportScCb}, CounterMap} 
%%
%% CounterMap = string() | undefined | error
%% 
%%========================================================================
decode_init_data(Data) ->
    <<ProcPid:1/native-unsigned-integer-unit:32,
     SubscrCb:1/native-unsigned-integer-unit:32,
     ReportRopCb:1/native-unsigned-integer-unit:32,
     ReportScCb:1/native-unsigned-integer-unit:32,
     R/binary>> = Data,
    CecProcName = cec:get_program_name(ProcPid),
    ?LOG_RAM(?SEV_1, {"Calling process: ~p~n", [CecProcName]}),
    case did(decode_32(R)) of
	{error, _} = E ->
	    E;
	CounterMap ->
	    {ok, {[{?SUBSCRIBE_CB, SubscrCb /= 0}, 
		   {?REPORT_ROP_CB, ReportRopCb /= 0}, 
		   {?REPORT_SC_CB, ReportScCb /= 0}],
		  CounterMap,
		  CecProcName
		  }}
    end.
  

did({0, _}) ->  
    undefined;
did({_, R}) ->
    case decode_string(R) of
	{CounterMap, ?END_TAG_4} -> 
	    CounterMap;
	Rem->
	    ?LOG_RAM(?SEV_5, 
		     {"~p:decode_initialize unexpected remaining ~p~n",
		      [?MODULE, Rem]}),
	    {error, {"Invalid CounterMapId", R}}
    end.

%%========================================================================
%% decode_counter_maps(Maps) -> 
%%    [{GrpId, GrpIdAlias, [{MtId, MtIdAlias}]}] | {error, Reason}
%% 
%%========================================================================
decode_counter_maps(Maps) ->
    {NoOfGrps, Rem} = decode_32(Maps),
    dcm_group(NoOfGrps, Rem, []).

%%---------------------------------------------------------------
%% decode a PmGroup
%%
%% [PmGroupId, PmGroupIdAlias, NoofMeasurementTypes, MeasurementTypes]
%%
%% PmGroupId            = string()
%% PmGroupIdAlias       = uint32
%% NoofMeasurementTypes = uint32
%% MeasurementTypes     = see dcm_meas_type/3
%%---------------------------------------------------------------
dcm_group(0, ?END_TAG_4, Acc) ->
    lists:reverse(Acc);
dcm_group(0, Rem, Acc) ->
    ?LOG_RAM(?SEV_5, 
	     {"~p:decode_counter_maps unexpected remaining ~p~n",
	      [?MODULE, Rem]}),
    Acc;
dcm_group(NoOfGrps, Maps, Acc) ->
    {Group, Rem} = dcm_g(Maps),
    dcm_group(NoOfGrps - 1, Rem, [Group | Acc]).

dcm_g(Maps) ->
    %% {GrpId,      Rem1} = decode_string(Maps),
    {GrpId,      Rem1} = decode_binary(Maps),
    {GrpIdAlias, Rem2} = decode_32(Rem1),
    {NoMt,       Rem3} = decode_32(Rem2),
    {Mts,        Rem4} = dcm_meas_types(NoMt, Rem3, []),
    {{GrpId, GrpIdAlias, Mts}, Rem4}.

%%---------------------------------------------------------------
%% decode MeasurementTypes
%% 
%% MeasurementTypes     = [MtId, MtIdAlias]
%% MtId                 = string()
%% MtIdAlias            = uint32
%%---------------------------------------------------------------
dcm_meas_types(0, Rem, Acc) ->
    {Acc, Rem};
dcm_meas_types(NoOfMts, Maps, Acc) ->
    {Mt, Rem} = dcm_mt(Maps),
    dcm_meas_types(NoOfMts - 1, Rem, [Mt | Acc]).

dcm_mt(Maps) ->
    %% {MtId,      Rem1} = decode_string(Maps),
    {MtId,      Rem1} = decode_binary(Maps),
    {MtIdAlias, Rem2} = decode_32(Rem1),
    {{MtId, MtIdAlias}, Rem2}.

%%========================================================================
%% decode_data_rop(Data) -> 
%%    {GP, RequestId, ValueBundles, FinalFragment} | {error, Reason}
%% 
%% [GP, RequestId, ValBunLen, ValBuns, FinalFragment]
%%
%% GP            = uint32
%% RequestId     = uint64
%% ValBunLen     = uint32
%% ValBuns       = see ddr_value_bundles/3
%% FinalFragment = uint8
%% 
%% 
%%========================================================================
decode_data_rop(Data) ->
    ?LOG_RAM([{?SEV_1, {" DATA_ROP PMI2 <=== ~n", []}, ?DEFAULT_BL_TO},
	      {?SEV_5, {" DATA_ROP PMI2 <=== ~n", []}}]),
    {GP,        R1} = decode_32(Data),
    {ReqId,     R2} = decode_32(R1),
    {ValBunLen, R3} = decode_32(R2),

    {ValBuns, R4}   = decode_rop_vb(ValBunLen, R3),
    {FF, R}         = decode_32(R4),
    FinalFragment   = (FF /= 0),

    ddr_end(R),
    ?LOG_RAM([{?SEV_2, " DATA_ROP PMI2 decoded~n"},
	      {?SEV_5, 
	       ?LFUN({" DATA_ROP PMI2 <=== ~n"
		      "   GP      ~p~n"
		      "   ReqId   ~p~n" 
		      "   VbLen   ~p~n"
		      "   ValBuns ~p~n"
		      "   FF      ~p~n"
		      "   Rem     ~p~n",
		      [GP, 
		       ReqId, 
		       ValBunLen,
		       pmsLib:log_trunc_bundle(ValBuns), 
		       FinalFragment, 
		       R]})}]),
    {GP, ReqId, ValBuns, FinalFragment}.

 
ddr_end(?END_TAG_4) ->
    ok;
ddr_end(Rem) ->
    ?LOG_RAM(?SEV_5, 
	     {"~p:decode_data_rop unexpected remaining ~p~n",
	      [?MODULE, Rem]}).


%%---------------------------------------------------------------
%% decode ROP value bundle
%% 
%% [GroupBundleSize, GroupIdAlias, MoInstanceBundleLength, MoInstances]
%% 
%% GroupBundleSize        = uint32
%% GroupIdAlias           = uint32
%% MoInstanceBundleLength = uint32
%% MoInstanceBundleSize   = uint32
%% MoInstances            = [MoInstanceAlias, MeasValueLength, MeasValues]
%% 
%% MoInstanceAlias      = uint32
%% MeasValueLength      = uint32
%% MeasValues           = [MeasurementTypeAlias, Multiplicity, [Value]]
%% 
%% MeasurementTypeAlias = uint32
%% Multiplicity         = uint32
%% Value                = uint64
%%---------------------------------------------------------------
decode_rop_vb(NValBuns, Bin) ->
    %% TS1 = os:timestamp(),
    Res = decode_rop_vb_mb(NValBuns, Bin, fun fun_decode_vb/1),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_1, {"Execution time decode_rop_vb: ~p ms", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    Res.

decode_rop_vb_mb(N, Bin, F) ->
    {SplitBin, Rem} = split_vb_mb_bin(N, Bin, []),
    %% TS1 = os:timestamp(),
    Decoded = pmsLib:pmapr_delay(F, SplitBin, 0),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_1, {"Execution time ~p: ~p", 
    %% 		      [F, timer:now_diff(TS2, TS1)]}),
    {Decoded, Rem}.

split_vb_mb_bin(0, Rem, Acc) ->
    {Acc, Rem};

split_vb_mb_bin(N, Bin, Acc) ->
    {Len, Rem} = decode_32(Bin),
    <<HBin:Len/binary, Rem1/binary>> = Rem, 
    split_vb_mb_bin(N - 1, Rem1, [HBin | Acc]).


fun_decode_vb(VBin) ->
    {GidAlias, R1} = decode_32(VBin),
    {InstBunLen, R2} = decode_32(R1),
    {InstBuns, _R} = decode_rop_mb(InstBunLen, R2),
    {GidAlias, InstBuns}.


decode_rop_mb(NInstBuns, Bin) ->
    decode_rop_vb_mb(NInstBuns, Bin, fun fun_decode_mb/1).


fun_decode_mb(MBin) ->
    {InstAlias,  R1} = decode_32(MBin),
    {MeasValLen, R2} = decode_32(R1),
    {MeasVals,   _R}  = ddr_meas_values(MeasValLen, R2, []),
    {InstAlias, MeasVals}.

%%---------------------------------------------------------------
%% decode SC value bundle
%% 
%% [GroupIdAlias, MoInstanceBundleLength, MoInstances]
%% 
%% GroupIdAlias           = uint32
%% MoInstanceBundleLength = uint32
%% MoInstances            = [MoInstanceAlias, MeasValueLength, MeasValues]
%% 
%% MoInstanceAlias      = uint32
%% MeasValueLength      = uint32
%% MeasValues           = [MeasurementTypeAlias, Multiplicity, [Value]]
%% 
%% MeasurementTypeAlias = uint32
%% Multiplicity         = uint32
%% Value                = uint64
%%---------------------------------------------------------------
ddr_value_bundles(0, Rem, Acc) ->
    {lists:reverse(Acc), Rem};
ddr_value_bundles(ValBunLen, Rem, Acc) ->
    {GidAlias,   R1} = decode_32(Rem),
    {InstBunLen, R2} = decode_32(R1),
    {InstBuns,   R}  = ddr_mo_bundles(InstBunLen, R2, []),
    ddr_value_bundles(ValBunLen - 1, R, [{GidAlias, InstBuns} | Acc]).
    

ddr_mo_bundles(0, Rem, Acc) ->
    {lists:reverse(Acc), Rem};
ddr_mo_bundles(InstBunLen, Rem, Acc) ->
    {InstAlias,  R1} = decode_32(Rem),
    {MeasValLen, R2} = decode_32(R1),
    {MeasVals,   R}  = ddr_meas_values(MeasValLen, R2, []),
    ddr_mo_bundles(InstBunLen - 1, R, [{InstAlias, MeasVals} | Acc]).
    

ddr_meas_values(0, Rem, Acc) ->
    {lists:reverse(Acc), Rem};
ddr_meas_values(MeasValsLen, Rem, Acc) ->
    {MtAlias, R1} = decode_32(Rem),
    {Mult,    R2} = decode_32(R1),
    {Values,  R}  = ddr_values(Mult, R2, []),
    ddr_meas_values(MeasValsLen - 1, R, [{MtAlias, Values} | Acc]).

ddr_values(0, Rem, Acc) ->
    {lists:reverse(Acc), Rem};
ddr_values(Mult, Rem, Acc) ->
    {V, R1} = decode_s64(Rem),
    ddr_values(Mult - 1, R1, [V | Acc]).


%%========================================================================
%% decode_data_sc(Data) -> 
%%   [{RequestId, Result, ErrorString, [Value]}] | {error, Reason}
%% 
%% [RequestId, Result, ErrorStr, [Value]]
%% 
%% RequestId    = uint32
%% Result       = uint32
%% ErrorStr     = string()
%% Value        = uint64
%%========================================================================
decode_data_sc(Data) ->
    {RequestId, R1} = decode_32(Data),
    {Result,    R2} = decode_32(R1),
    {ErrorStr,  R3} = decode_string(R2),

    {ValBunLen, R4} = decode_32(R3),
    {ValBuns, R}    = ddr_value_bundles(ValBunLen, R4, []),
    ddsc_end(R),

    ?LOG_RAM([{?SEV_1, {" DATA SHOW COUNTERS <=== ~n", []}, ?DEFAULT_BL_TO},
	      {?SEV_5, 
	       ?LFUN({" DATA SHOW COUNTERS <=== ~n"
		      "   ReqId   ~p~n" 
		      "   Result  ~p~n"
		      "   Error   ~p~n"
		      "   Multi   ~p~n"
		      "   Vals    ~p~n"
		      "   Rem     ~p~n",
		      [RequestId,
		       Result,
		       ErrorStr,
		       ValBunLen,
		       pmsLib:log_trunc_bundle(ValBuns), 
		       R]})}]),
    {RequestId, Result, ErrorStr, ValBuns}.

 
ddsc_end(?END_TAG) ->
    ok;
ddsc_end(?END_TAG_4) ->
    ok;
ddsc_end(Rem) ->
    ?LOG_RAM(?SEV_5, 
	     {"~p:decode_data_sc unexpected remaining ~p~n",
	      [?MODULE, Rem]}).

%%========================================================================
%% decode_finalize(Data) -> 
%%    [{GP}}] | {error, Reason}
%% 
%% Check the message and take actions.
%%========================================================================
decode_finalize(?END_TAG_4) ->
    ok;
decode_finalize(Rem) ->
    ?LOG_RAM(?SEV_5, 
	     {"~p:decode_finalize unexpected remaining ~p~n",
	      [?MODULE, Rem]}),
    error.

%%========================================================================
%% common decode functions
%%========================================================================
decode_binary(Maps) ->
    {Len, Rem1} = decode_32(Maps),
    <<Bin:Len/binary, Rem2/binary>> = Rem1, 
    {Bin, Rem2}.

decode_string(Maps) ->
    {Len, Rem1} = decode_32(Maps),
    <<Str:Len/binary, Rem2/binary>> = Rem1, 
    {binary_to_list(Str), Rem2}.

decode_32(<<Int:1/native-unsigned-integer-unit:32, Rest/binary>>) ->
    {Int, Rest}.

decode_s64(<<Int:1/native-signed-integer-unit:64, Rest/binary>>) ->
    {Int, Rest}.

%%========================================================================
%% common endcode functions
%%========================================================================
encode_32(Int) ->
    <<Int:1/native-unsigned-integer-unit:32>>.


encode_subscribe_rop(GP, SubscribeSpec) ->
    iolist_to_binary([<<?SUBSCRIBE_ROP:1/native-unsigned-integer-unit:32>>,
		      encode_32(GP),
		      encode_32(length(SubscribeSpec)),
		      encode_sr(SubscribeSpec, [])]).
    
    
encode_sr([], Acc) ->
    lists:sort(Acc);
encode_sr([{GidAlias, MidAliases} | T], Acc) ->
    SR = iolist_to_binary([encode_32(GidAlias),
			   encode_32(length(MidAliases)),
			   [encode_32(MidA) || MidA <- MidAliases]]),
    encode_sr(T, [SR | Acc]).


encode_report_rop(GP, ReportId, MaxReportingTime) ->
    iolist_to_binary([<<?REPORT_ROP:1/native-unsigned-integer-unit:32>>,
		      encode_32(GP),
		      encode_32(ReportId),
		      encode_32(MaxReportingTime)]).
    

encode_report_sc(ReportId, MoInstLdn, ShowCountersSpec, MaxReportingTime) ->
    iolist_to_binary([<<?REPORT_SC:1/native-unsigned-integer-unit:32>>,
		      encode_32(ReportId),
		      encode_32(MoInstLdn),
		      encode_32(length(ShowCountersSpec)),
		      encode_sr(ShowCountersSpec, []),
		      encode_32(MaxReportingTime)]).
    
%%===========================================================================
%% log truncate functions
%%===========================================================================


%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



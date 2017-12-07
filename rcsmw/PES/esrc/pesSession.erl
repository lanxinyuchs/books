%%% ------------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ------------------------------------------------------------------------
%%% %CCaseFile:	pesSession.erl %
%%% Author:	
%%% @private
%%% Description:
%%%
%%% Session module for PEI.
%%% 
%%% 
%%% 
%%% ------------------------------------------------------------------------
-module(pesSession).
-behaviour(gen_server).
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-04-05').
-author('uabesvi').

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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    --------------------------------------
%%% R3A/1      2014-11-10 eolaand     Created
%%% R4A/1      2015-09-08 uabesvi     error_logger -> sysInitI
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

%% PEI callbacks 
-export([peiEventJobCallback/8,
	 peiMEAttrUpdateCallback/3]).
    

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsPMEventM.hrl").
-include("pes.hrl").


-define(PEI_API, pesPeI).

-define(INITIALIZE,     1).
-define(INITIALIZE_RES, 2).
-define(ME_ATTR_UPDATE, 3).
-define(EVENT_JOB,      4).
-define(FINALIZE,       5).

-define(SUBSCRIBE_TO,  5000).

-define(PEI_OK, 1).
-define(PEI_UNKNOWN_EVENT_MAP_ID, 3).
-define(PEI_BAD_PARAMETER, 4).
-define(PEI_INTERNAL_ERROR, 5).

-record(state, {socket, 
		pei_handle}).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
peiEventJobCallback(AppPid, ProducerId, JobId, ReqJobstate, Types, FilterIds, 
		    FileCtrl, StreamCtrl) -> 
    gen_server:cast(AppPid, 
		    {peiEventJob,
		     {ProducerId, JobId, ReqJobstate, Types, FilterIds, 
		      FileCtrl, StreamCtrl}}).


peiMEAttrUpdateCallback(AppPid, UserlLabel, LogicalName) ->
    gen_server:cast(AppPid, {peiMEAttrUpdate, {UserlLabel, LogicalName}}).

    
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
    ?LOG_RAM(?SEV_1, "started~n"),
    {ok, #state{socket = Socket}}.


%%========================================================================
%% handle_call(Command, From, State) -> {reply, Res, State}
%%========================================================================
handle_call(Command, From, S) ->
    ?LOG_RAM(?SEV_1, 
	     {"Received unknown handle_call msg ~p~n", [{Command, From}]}),
    {reply, Command, S}.


%%========================================================================
%% handle_cast(Message, State) -> {noreply, State}
%%========================================================================
handle_cast({peiEventJob, Params}, S) ->
    Data = encode_event_job(Params),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_cast({peiMEAttrUpdate, Params}, S) ->
    Data = encode_me_attr_update(Params),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_cast(Msg, S) ->
    ?LOG_RAM(?SEV_1, {"Received unknown handle_cast msg ~p~n", [Msg]}),
    {noreply, S}.


%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
handle_info({tcp, _, <<Msg, Data/binary>>}, #state{socket = Socket} = S) ->
    inet:setopts(Socket, [{active, once}]),
    case handle_app_msg(Msg, Data, S) of
	ok -> 
	    {noreply, S};
	{ok, Handle} -> 
	    {noreply, S#state{pei_handle = Handle}};
	{error, Reason} -> 
	    {stop, Reason, S};
	{stop, Reason} -> 
	    {stop, Reason, S}
    end;

handle_info({tcp_closed, Reason}, S) 
  when S#state.pei_handle =/= undefined ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"tcp_closed ~p~nFinalize and terminate.", [Reason]}),
    ?PEI_API:peiFinalize(S#state.pei_handle),
    {stop, normal, S};

handle_info({tcp_closed, Reason}, S) ->
    ?LOG_RAM(?SEV_WARNING, {"tcp_closed ~p~n", [Reason]}),
    {stop, tcp_closed, S};

handle_info(Info, S) ->
    ?LOG_RAM(?SEV_1, {"Received unknown handle_info msg ~p~n", [Info]}),
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
    ?LOG_RAM(?SEV_1, {"terminated ~p~n", [Reason]}),
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
    {EventMapId, Rem1} = decode_string(Data),
    {EventJobCb, Rem2} = decode_bool(Rem1),
    {MEAttrUpdateCb, _} = decode_bool(Rem2),
    Callbacks = [{peiEventJobCallback, EventJobCb},
		 {peiMEAttrUpdateCallback, MEAttrUpdateCb}],
    ?LOG_RAM(?SEV_1, {" INITIALIZE PEI <=== ~n Callbacks ~p~n", [Callbacks]}),
    Result = ?PEI_API:peiInitialize(EventMapId, Callbacks, ?MODULE),
    FormatedResult = format_init_result(Result),
    ReplyData = <<?INITIALIZE_RES:8, FormatedResult:8>>,
    gen_tcp:send(S#state.socket, ReplyData),
    app_msg_res(Result);

handle_app_msg(?FINALIZE, Data, S) ->
    _DecodedData = decode_finalize(Data),
    ?LOG_RAM(?SEV_1, {" FINALIZE PEI <===  ~p~n", [Data]}),
    ?PEI_API:peiFinalize(S#state.pei_handle),
    {stop, normal};

handle_app_msg(Msg, Data, _S) ->
    ?LOG_RAM(?SEV_1, {" UNKNOWN MESSAGE <=== ~n ~p  ~p~n", [Msg, Data]}),
    ok.


app_msg_res({error, ?ERR_UNKNOWN_MAP_ID}) ->
    {stop, normal};

app_msg_res(Other) ->
    Other.


format_init_result({ok, _}) ->
    ?PEI_OK;

format_init_result({error, ?ERR_UNKNOWN_MAP_ID}) ->
    ?PEI_UNKNOWN_EVENT_MAP_ID;

format_init_result({error, _}) ->
    ?PEI_INTERNAL_ERROR.


%%========================================================================
%% decode_finalize(Data) -> 
%%    ok | {error, Reason}
%% 
%% Check the message and take actions.
%%========================================================================
decode_finalize(<<0>>) ->
    ok;
decode_finalize(Rem) ->
    sysInitI:info_msg("~p:decode_finalize unexpected remaining ~p~n",
		      [?MODULE, Rem]),
    {error, bad_finalize}.


%%========================================================================
%% encode_me_attr_update() -> 
%%    Data::binary()
%% 
%% Encode callback
%%========================================================================
encode_me_attr_update({UserLabel, NeMEId}) ->
    iolist_to_binary([<<?ME_ATTR_UPDATE>>,
		      encode_bool(is_valid(UserLabel)),
		      encode_opt_string(UserLabel),
		      encode_bool(is_valid(NeMEId)),
		      encode_opt_string(NeMEId)
		     ]).

%%========================================================================
%% encode_event_job() -> 
%%    Data::binary()
%% 
%% Encode callback
%%========================================================================
encode_event_job({ProducerId, JobId, ReqJobState, Types, Filters, 
		  FileCtrl, StreamCtrl}) ->
    iolist_to_binary([<<?EVENT_JOB>>,
		      encode_string(ProducerId),
		      encode_string(JobId),
		      encode_job_state(ReqJobState),
		      encode_32(length(Types)),
		      encode_32_list(Types),
		      encode_filters(Filters),
		      encode_bool(is_valid(FileCtrl)),
		      encode_file_ctrl(FileCtrl),
		      encode_bool(is_valid(StreamCtrl)),
		      encode_stream_ctrl(StreamCtrl)
		     ]).
    

encode_job_state(JobState) when is_integer(JobState) ->
    encode_32(JobState);

encode_job_state(_) ->
    encode_32(0).
    

encode_filters(Filters) when is_list(Filters), Filters =/= [] ->
    [encode_32(length(Filters)) | 
     [[encode_string(Name), encode_string(Value)] || 
			   {Name, Value} <- Filters]]; 

encode_filters(_) ->
    encode_32(0).


encode_compr_type(CompressionType) 
  when is_integer(CompressionType) ->    
    encode_32(CompressionType);

encode_compr_type(_) -> 
    <<>>.

encode_file_ctrl({ReportingPeriod, CompressionType}) ->    
    [encode_32(ReportingPeriod), 
     encode_bool(is_valid(CompressionType)),
     encode_compr_type(CompressionType)];

encode_file_ctrl(_) -> 
    <<>>.

encode_stream_ctrl({CompressionType, DestinationIpAddress, DestinationPort}) ->
    [encode_bool(is_valid(CompressionType)),
     encode_compr_type(CompressionType),
     encode_string(DestinationIpAddress),
     encode_32(DestinationPort)];

encode_stream_ctrl(_) ->
    <<>>.

%%========================================================================
%% common decode functions
%%========================================================================

decode_string(Maps) ->
    {Len, Rem1} = decode_32(Maps),
    <<Str:Len/binary, Rem2/binary>> = Rem1, 
    {binary_to_list(Str), Rem2}.

decode_bool(Bin) ->
    case decode_8(Bin) of
	{0, Rem} ->
	    {false, Rem};
	{_, Rem} ->
	    {true, Rem}
    end.

decode_8(<<Int:1/native-unsigned-integer-unit:8, Rest/binary>>) ->
    {Int, Rest}.

decode_32(<<Int:1/native-unsigned-integer-unit:32, Rest/binary>>) ->
    {Int, Rest}.

%% decode_64(<<Int:1/native-unsigned-integer-unit:64, Rest/binary>>) ->
%%     {Int, Rest}.

%% decode_s64(<<Int:1/native-signed-integer-unit:64, Rest/binary>>) ->
%%     {Int, Rest}.



%%========================================================================
%% common encode functions
%%========================================================================
encode_string(String) ->
    [encode_32(length(String)), String].


encode_opt_string(String) when is_list(String) ->
    encode_string(String);

encode_opt_string(_) ->
    <<>>.


%% encode_string_list(Strings) ->
%%     [encode_string(String) || String <- Strings].


encode_8(Int) ->
    <<Int:1/native-unsigned-integer-unit:8>>.


encode_32(Int) ->
    <<Int:1/native-unsigned-integer-unit:32>>.


encode_32_list(Ints) ->
    [encode_32(Int) || Int <- Ints].


encode_bool(true) ->
    encode_8(1);
    
encode_bool(_False) ->
    encode_8(0).
    

%% encode_64(Int) ->
%%     <<Int:1/native-unsigned-integer-unit:64>>.

%% encode_sr([], Acc) ->
%%     Acc;
%% encode_sr([{GidAlias, MidAliases} | T], Acc) ->
%%     SR = iolist_to_binary([encode_32(GidAlias),
%% 			   encode_32(length(MidAliases)),
%% 			   [encode_32(MidA) || MidA <- MidAliases]]),
%%     encode_sr(T, [SR | Acc]).

%%========================================================================
%% commonly used functions
%%========================================================================
is_valid(undefined) ->
    false;
is_valid(_) ->
    true.


%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



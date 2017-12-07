%%% ------------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ------------------------------------------------------------------------
%%% %CCaseFile:	pmsSession.erl %
%%% Author:	
%%% @private
%%% Description:
%%%
%%% 
%%% 
%%% 
%%% 
%%% ------------------------------------------------------------------------
-module(pmsSession).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/2').
-date('2016-12-15').
-author('eolaand').
-shaid('6db43bc0e876a6eddc21b3f647a24640e61aa38f').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R1A/1      2012-01-18 uabesvi     Created
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
%%-export([start/0]).
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
-define(PMI_MOD, pmsPmI).
-define(CEC_PORT, 2345).

-define(SUBSCRIBE, 1).
-define(REPORT, 2).
-define(REPORT_SC, 3).

-define(SUBSCRIBE_TO,  5000).


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
    ?LOG_RAM(?SEV_1, "Started.~n"),
    {ok, #state{socket = Socket}}.


%%========================================================================
%% handle_call(Command, From, State) -> {reply, Res, State}
%%========================================================================
handle_call(get_proc_info, _From, State) ->
    {reply, 
     {session, 
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
handle_info({pmiReport, {GP, TimeSpec, Deadline}}, S) ->
    Data = term_to_binary({?REPORT, GP, TimeSpec, Deadline}),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_info({pmiReportShowCounters, {ReqId, MeasObjLDN, Timeout}}, S) ->
    Data = term_to_binary({?REPORT_SC, ReqId, MeasObjLDN, Timeout}),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_info({pmiSubscribe, {GP, CounterSpecs}}, S) ->
    Data = term_to_binary({?SUBSCRIBE, GP, CounterSpecs}),
    gen_tcp:send(S#state.socket, Data),
    {noreply, S};

handle_info({tcp, _, Binary}, #state{socket = Socket} = S) ->
    inet:setopts(Socket, [{active, once}]),
    ?LOG_RAM(?SEV_1, "activate once ~n"),
    case handle_app_msg(binary_to_term(Binary), S) of
	ok              -> {noreply, S};
	{ok, Handle}    -> {noreply, S#state{pms_handle = Handle}};
	{error, Reason} -> {stop, Reason, S};
	{stop, Reason}  -> {stop, Reason, S}
    end;

handle_info({tcp_closed, Reason}, S) 
  when S#state.pms_handle =/= undefined ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"tcp_closed ~p~nFinalize and terminate.", [Reason]}),
    ?PMI_MOD:pmiFinalize(S#state.pms_handle),
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
handle_app_msg({"initialize", PMGroups}, _S) ->
    ?LOG_RAM([{?SEV_1, " INITIALIZE <=== ~n"},
	      {?SEV_5, 
	       {" INITIALIZE <=== ~n"
		" Groups ~p~n", [PMGroups]}}]),
    ?PMI_MOD:pmiInitialize(PMGroups);


handle_app_msg({"finalize"}, S) ->
    ?LOG_RAM(?SEV_1, {" FINALIZE <=== ~n", []}),
    ?PMI_MOD:pmiFinalize(S#state.pms_handle),
    {stop, normal};

handle_app_msg({"data", GP, TimeSpec, MeasObjLDN, ValBundle}, S) ->
    Handle = S#state.pms_handle,
    ?LOG_RAM([{?SEV_1, " PM DATA PMI <===~n"},
	      {?SEV_5, 
	       ?LFUN({" PM DATA <===~n"
		      "  Handle = ~p~n"
		      "  GP     = ~p~n"
		      "  TS     = ~p~n"
		      "  LDN    = ~p~n"
		      "  Data   = ~p~n", 
		      [Handle, 
		       GP, 
		       TimeSpec, 
		       MeasObjLDN, 
		       pmsLib:log_trunc_pmi_vals(ValBundle)]})}]),
	      
    ConvValueBundle = [{to_binary(Group), [{to_binary(MT), to_list(Vals)} || 
					      {MT, _, Vals} <- MTVals]} || 
			  {Group, MTVals} <- ValBundle],
    Res = pmsAppJob:pmi_data(Handle, 
			     GP,
			     TimeSpec,
			     to_binary(MeasObjLDN),
			     ConvValueBundle),
    ?LOG_RAM(?SEV_1, {"PM DATA PMI ~n  Result = ~p~n", [Res]}),
    Res;

handle_app_msg({"data_show_counters", RequestId, Result, ErrorStr, Values}, S) ->
    Handle = S#state.pms_handle,
    ?LOG_RAM([{?SEV_1, " PM DATA SHOW COUNTERS <===~n"},
	      {?SEV_5, 
	       {" PM DATA SHOW COUNTERS <===~n"
		"  Handle   = ~p~n"
		"  ReqId    = ~p~n"
		"  Result   = ~p~n"
		"  ErrorStr = ~p~n"
		"  Data     = ~p~n", 
		[Handle, RequestId, Result, ErrorStr, Values]}}]),

    ValuesB = [{to_binary(MT), to_list(Vals)} || 
		  {MT, _, Vals} <- Values],

    pmsAppJob:pmi_data_show_counters(Handle, 
				     RequestId, 
				     Result, 
				     ErrorStr, 
				     ValuesB),
    ok.


to_list(Term) ->
    pmsLib:to_list(Term).
    

to_binary(Term) ->
    pmsLib:to_binary(Term).
    

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_itc_server.erl %
%%% Author:     etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ecoli_itc_server).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/R7A/R9A/R11A/1').
-date('2017-09-13').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R4A/1      2015-06-01 uabesvi     Created
%%% R4A/15     2015-11-10 uabesvi     do not crash if several mboxes are found
%%%                                   when sending a coli command request
%%% R4A/16     2015-11-10 uabesvi   error_logger -> sysInitI
%%% R9A/1      2017-04-04 uabesvi   added logs
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0,
         start/1,
         start_link/0,
         start_link/1,
         stop/0]).

-export([activate/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
	 format_status/2]).


-export([send/4]).
-export([do_it/1]).

-record(state, 
	{
	  itc_port,
	  request_id = 0,
	  requests   = [],   %% [{RequestId, UserPid}]
	  replies    = []
	 }).

-include("ecoli.hrl").

-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).

-define(SERVER, ?MODULE).
-define(COMMA, 44).

-define(MAX_REPLY_LEN, 100).
 

start() ->
    start([]).
start(Opts) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

stop() ->
    gen_server:cast(?SERVER, stop).

init(_Opts) ->
    ?LOG_RAM(?SEV_1, "Started.~n"),
    ItcPort = itc:open("ecoli_coli"),
    itc:listen(ItcPort),
    State = #state{itc_port = ItcPort},
    ets:new(ecoli_fruacc, [named_table, {keypos, 2}]),
    {ok, State}.

activate() ->
    ok.

send(FruType, FruId, Command, Args) ->
    MeFruId = ecoli_lib:update_me_id(FruId, "1"),
    gen_server:cast(?SERVER, {send, self(), FruType, MeFruId, Command, Args}),
    ok.

do_it(Mbox) ->
    gen_server:cast(?SERVER, {do_it, Mbox}),
    ok.

handle_call(Command, {Pid, _}, S) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(Pid)] ++
	[{unrecognized_msg, Command} | ?STATE_INFO(S)],
    sysInitI:warning_report(Info),
    {reply, Command, S}.


%%-----------------------------------------------------------------
%% COLI_COMMAND_REQUEST
%%-----------------------------------------------------------------
handle_cast({send, 
	     UserPid, 
	     _FruType, 
	     FruId,
	     #coli_cmd{cli_pname = {Path, Cmd}, 
		       cli_type  = CliType},
	     Args},
	    #state{itc_port   = ItcPort,
		   request_id = ReqId,
		   requests   = Requests} = S) ->
    ?LOG_RAM(?SEV_1, {"ITC sending command~n"
		      "  FruId   = ~p~n"
		      "  Path    = ~p~n"
		      "  Cmd     = ~p~n"
		      "  ItcPort = ~p~n"
		      "  ReqId   = ~p~n",
		      [FruId, Path, Cmd, ItcPort, ReqId]}),
    handle_send_itc(find_mbox(atom_to_list(CliType), FruId), 
		    ItcPort,
		    FruId,
		    string:join([Path, Cmd], "/") ++ " " ++ string:join(Args, " "),
		    ReqId),
    {noreply, S#state{request_id = ReqId + 1,
		      requests   = Requests ++ [{ReqId, UserPid}]}};
handle_cast({do_it, MboxId}, #state{itc_port = ItcPort} = S) ->
    itc:send(ItcPort,
	     MboxId,
	     1,
	     <<0>>),
    {noreply, S};
handle_cast(Request, S) ->
    ?LOG_RAM(?SEV_WARNING, {"ITC send. Unrecognized command~n"
			    "  Request = ~p~n",
			    [Request]}),
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, Request} |
				 ?STATE_INFO(S)]),
    {noreply, S}.



%%===================================================================================
%% handle_info
%%===================================================================================
%%-----------------------------------------------------------------
%% COLI_INTERFACE_REGISTRATION
%%-----------------------------------------------------------------
handle_info({message,
	     _FromPid,
	     {FromMbox, _ToMbox, ?COLI_INTERFACE_REGISTRATION, Data}},
	    #state{itc_port = MyPort} = S) ->
    FruTypes = decode_reg(Data, []),
    [ets:insert(ecoli_fruacc,
		#fruacc{key = {lower(FT), FromMbox}}) || FT <- FruTypes],
    _Ref = itc:attach(MyPort, FromMbox),
    ?LOG_RAM(?SEV_1, {"ITC IF registration~n"
		      "  FromBox  = ~p~n"
		      "  FruTypes = ~p~n",
		      [FromMbox, FruTypes]}),
    {noreply, S};
%%-----------------------------------------------------------------
%% COLI_ADD_FRU
%%-----------------------------------------------------------------
handle_info({message,
	     _FromPid,
	     {FromMbox, _ToMbox, ?COLI_ADD_FRU, Data}},
	    S) ->
    {FruType, Ldn} = decode_add_fru(Data),
    Exists = [Key || #fruacc{key  = Key,
			     ldns = LDNS} <- ets:tab2list(ecoli_fruacc),
		    lists:member(Ldn, LDNS)],
    add_fru(Exists, 
	    ets:lookup(ecoli_fruacc, {FruType, FromMbox}),
	    {FruType, FromMbox},
	    Ldn),
    {noreply, S};
%%-----------------------------------------------------------------
%% COLI_DELETE_FRU
%%-----------------------------------------------------------------
handle_info({message,
	     _FromPid,
	     {FromMbox, _ToMbox, ?COLI_DELETE_FRU, Data}},
	    S) ->
    {FruType, Ldn} = decode_add_fru(Data),
    delete_fru(ets:lookup(ecoli_fruacc, {FruType, FromMbox}),
	       {FruType, FromMbox},
	       Ldn),
    {noreply, S};
%%-----------------------------------------------------------------
%% COLI_COMMAND_REPLY
%%-----------------------------------------------------------------
handle_info({message,
	     _FromPid,
	     {FromMbox, _ToMbox, ?COLI_COMMAND_REPLY, Data}},
	    S) ->
    {noreply, handle_command_reply(Data, FromMbox, S)};
%%-----------------------------------------------------------------
%% MBOX DOWN
%%-----------------------------------------------------------------
handle_info({mailbox_down,
	     _Port,
	     _Ref,
	     Mbox},
	    S) ->
    ets:match_delete(ecoli_fruacc, {'_', {'_', Mbox}, '_'}),
    {noreply, S};
%%-----------------------------------------------------------------
%% Unknown message
%%-----------------------------------------------------------------
handle_info(Info, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, Info},
				 {state, S}]),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

format_status(_Opt, [_Pdict, S]) ->
     [{data, [{"State", [{itc_port, S#state.itc_port}]}]}].

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.



find_mbox(CliType, FruId) ->
    f_mbox(ets:match(ecoli_fruacc, {fruacc, {CliType, '$1'}, '$2'}), FruId).

f_mbox([], _) ->
    {error, cli_type_not_found};
f_mbox([[Mbox, Ldns] | _],
	FruId) ->
    case lists:member(FruId, Ldns) of
	true  -> {ok, {Mbox, FruId}};
	false -> {error, fru_id_not_found}
    end.

    
handle_send_itc({ok, {MboxId, _}}, ItcPort, Ldn, Cmd, Rid) ->
    CmdBin = create_max_bin(Cmd, ?MAX_COMMAN_LENGTH), 
    LdnBin = create_max_bin(Ldn, ?MAX_LDN_LENGTH), 
    Data   = <<Rid:1/native-unsigned-integer-unit:32,
	      CmdBin/binary,
	      LdnBin/binary
	      >>,
    ?LOG_RAM(?SEV_WARNING, {"ITC send~n"
			    "  MboxId  = ~p~n"
			    "  FruId   = ~p~n"
			    "  Data    = ~p~n",
			    [MboxId, Ldn, Data]}),
    itc_send(ItcPort, MboxId, ?COLI_COMMAND_REQUEST, Data);
handle_send_itc(Error, ItcPort, Ldn, Cmd, Rid) ->
    ?LOG_RAM(?SEV_WARNING, {"ITC sending failed~n"
			    "  Result  = ~p~n"
			    "  FruId   = ~p~n"
			    "  Cmd     = ~p~n"
			    "  ItcPort = ~p~n"
			    "  ReqId   = ~p~n",
			    [Error, Ldn, Cmd, ItcPort, Rid]}),
    Error.


itc_send(ItcPort, MboxId, SigNo, Data) ->
    itc:send(ItcPort, MboxId, SigNo, Data).
   


add_fru([], 
	[],
	{FruType, FromMbox},
	Ldn) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"ITC Discarding add fru request. "
	      "Fru type not registered. ~n"
	      "  FruType     = ~p~n"
	      "  Ldn         = ~p~n"
	      "  FromMailBox = ~p~n",
	      [FruType, Ldn, FromMbox]}),
    sysInitI:warning_msg("COLI: Discarding add fru request. "
			     "Fru type not registered. ~n"
			     "FruType     = ~p~n"
			     "Ldn         = ~p~n"
			     "FromMailBox = ~p~n",
			     [FruType, Ldn, FromMbox]),
    {error, no_fru};
add_fru(_,
	[#fruacc{ldns = OldFrus}],
	{FruType, FromMbox} = Key,
	Ldn) ->
    ?LOG_RAM(?SEV_1, 
	     {"ITC Add fru request.~n "
	      "  FruType     = ~p~n"
	      "  Ldn         = ~p~n"
	      "  FromMailBox = ~p~n",
	      [FruType, Ldn, FromMbox]}),
    ets:insert(ecoli_fruacc, #fruacc{key  = Key,
				     ldns = (OldFrus -- [Ldn]) ++ [Ldn]
				    }).


delete_fru([],
	   {FruType, FromMbox},
	   Ldn) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"ITC Discarding delete fru request. "
	      "Fru type not registered. ~n"
	      "  FruType     = ~p~n"
	      "  Ldn         = ~p~n"
	      "  FromMailBox = ~p~n",
	      [FruType, Ldn, FromMbox]}),
    sysInitI:warning_msg("COLI: Discarding delete fru request. "
			     "Fru type not registered. ~n"
			     "FruType     = ~p~n"
			     "Ldn         = ~p~n"
			     "FromMailBox = ~p~n",
			     [FruType, Ldn, FromMbox]),
    {error, no_fru};
delete_fru([#fruacc{ldns = OldFrus}],
	   {FruType, FromMbox} = Key,
	   Ldn) ->
    ?LOG_RAM(?SEV_1, 
	     {"ITC Delete fru request. ~n"
	      "  FruType     = ~p~n"
	      "  Ldn         = ~p~n"
	      "  FromMailBox = ~p~n",
	      [FruType, Ldn, FromMbox]}),
    ets:insert(ecoli_fruacc, #fruacc{key  = Key,
				     ldns = OldFrus -- [Ldn]
				    }).
    


lower(Str) ->
    string:to_lower(Str).


create_max_bin(Str, MaxLen) ->
    {MaxStr, _} = lists:split(MaxLen, Str ++ lists:duplicate(MaxLen, 16#0)),   
    list_to_binary(MaxStr).




handle_command_reply(Data,
		     FromMbox,
		     #state{requests = Requests} = S) ->
    {Result, ReqId, Reply} = decode_cmd_reply(Data),
    hcr(proplists:get_value(ReqId, Requests),
	FromMbox,
	{Result, ReqId, Reply},
	S).


hcr(undefined,
    FromMbox,
    {Result, ReqId, Reply},
    S) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"ITC Command reply. Unknown requestId. ~n"
	      "  FromBox   = ~p~n"
	      "  Result    = ~p~n"
	      "  RequestId = ~p~n"
	      "  Reply     = ~p~n",
	      [FromMbox, Result, ReqId, lists:sublist(Reply, ?MAX_REPLY_LEN)]}),
    sysInitI:info_msg("~p: Received message with unknown requestId"
		      " (~p) from mbox (~p)~n"
		      "  Result = ~p~n"
		      "  Msg = ~p~n",
		      [?MODULE, ReqId, FromMbox, Result, Reply]),
    S;
hcr(UserPid,
    _,
    {?COLI_RESULT_OK_CONTINUE = Result, ReqId, Reply},
    #state{requests = _Requests, 
	   replies  = Replies} = S) ->
    ?LOG_RAM(?SEV_1, 
	     {"ITC Command reply. Continue. ~n"
	      "  Result    = ~p~n"
	      "  RequestId = ~p~n"
	      "  Reply     = ~p~n",
	      [Result, ReqId, lists:sublist(Reply, ?MAX_REPLY_LEN)]}),
    TotalReplies = lists:append(lists:reverse([Reply | Replies])),
    UserPid ! {request_reply, {Result, ReqId, TotalReplies}},
    S#state{replies = []};
hcr(UserPid,
    _,
    {Result, ReqId, Reply},
    #state{requests = Requests, 
	   replies  = Replies} = S) ->
    ?LOG_RAM(?SEV_1, 
	     {"ITC Command reply. Final reply. ~n"
	      "  Result    = ~p~n"
	      "  RequestId = ~p~n"
	      "  Reply     = ~p~n",
	      [Result, ReqId, lists:sublist(Reply, ?MAX_REPLY_LEN)]}),
    TotalReplies = lists:append(lists:reverse([Reply | Replies])),
    UserPid ! {request_reply, {Result, ReqId, TotalReplies}},
    S#state{requests = lists:keydelete(ReqId, 1, Requests),
	    replies  = []}.


%%========================================================================
%% common decode functions
%%========================================================================


decode_reg(<<>>, Acc) ->
    lists:reverse([string:strip(A) || A <- Acc]);
decode_reg(Data, Acc) ->
    case decode_comma_string(Data) of
	{[], <<>>} ->
	    lists:reverse([string:strip(A) || A <- Acc]);
	{FruType, <<>>} ->
	    lists:reverse([FruType | Acc]);
	{FruType, Rem} ->
	    decode_reg(Rem, [FruType | Acc])
    end.

decode_add_fru(Data) ->
    {FruType, R1} = decode_string(Data),
    {FruId,   _}  = decode_string(R1),
    {lower(FruType), ecoli_lib:update_me_id(FruId, "1")}.

decode_cmd_reply(Data) ->
    {Result, R1} = decode_32(Data),
    {ReqId,  R2} = decode_32(R1),
    {Reply,  _}  = decode_string(R2),
    {Result, ReqId, Reply}.



decode_string(String) ->
    dec_str(String, []).

dec_str(Str, Acc) ->
    case decode_8(Str) of
	{0, Rem}  when Acc == [] -> dec_str(Rem, Acc);
	{0, Rem} -> {lists:reverse(Acc), Rem};
	{S, Rem} -> dec_str(Rem, [S | Acc])
    end.

decode_comma_string(Data) ->
    dec_cs(Data, []).

dec_cs(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
dec_cs(Data, Acc) ->
    case decode_8(Data) of
	{0,      Rem} when Acc == [] -> dec_cs(Rem, Acc);
	{?COMMA, Rem} when Acc == [] -> dec_cs(Rem, Acc);
	{0,      Rem} -> {lists:reverse(Acc), Rem};
	{?COMMA, Rem} -> {lists:reverse(Acc), Rem};
	{S,      Rem} -> dec_cs(Rem, [S | Acc])
    end.

decode_8(<<Int:1/native-unsigned-integer-unit:8, Rest/binary>>) ->
    {Int, Rest}.

decode_32(<<Int:1/native-unsigned-integer-unit:32, Rest/binary>>) ->
    {Int, Rest}.


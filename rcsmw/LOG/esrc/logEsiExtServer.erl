%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logEsiExtServer.erl %
%%% Author:	
%%% Description:
%%%
%%% 
%%%
%%% ----------------------------------------------------------
-module(logEsiExtServer).
-behaviour(gen_server).
-vsn('/main/R10A/R11A/8').
-date('2017-10-17').
-author('uabesvi').
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
%%% R10A/1-6   2016-06-22 uabesvi     Created
%%% R11A/1-7   2017-08-30 uabesvi     RU ESI additions
%%% R11A/8     2017-10-17 uabesvi     Format IP address added
%%% ----------------------------------------------------------

%%% ---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ---------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


-include("log.hrl").
-include("RcsLogM.hrl").
-include_lib("kernel/include/file.hrl").

%% gen_server calls
-export([start/0,
         start/1,
         start_link/0,
         start_link/1,
         stop/1]).

-export([get_ext_boards/0]).

-export([get_ext_board_info/3]).
-export([ext_esi_ready/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



%% ESI handling
-export([generate_esi/2]).

-define(SERVER(__RuId), ?MODULE).
%%-define(SERVER(__RuId), list_to_atom(atom_to_list(?MODULE) ++ "_" ++ __RuId)).

-define(COMMA,     44).
-define(SEMICOLON, 59).

-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).



-define(S_IDLE,        idle).
-define(S_ESTABLISHED, established).
-define(S_GET_BOARDS,  get_boards).

-record(state, 
	{
	  name,
	  state     = ?S_IDLE,
	  itc_port,
	  cat_mbox, 
	  request_id = 0,
	  requests   = [],   %% [{RequestId, UserPid}]
	  user_id 
	 }).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------



%%============================================================================
%% gen_server handling
%%============================================================================
start() ->
    start([]).

start(RuId) when is_integer(RuId) ->
    start(integer_to_list(RuId));
start(RuId) ->
    Name = ?SERVER(RuId),
    gen_server:start({local, Name}, ?MODULE, [Name, RuId], []).

start_link() ->
    start_link([]).

start_link(RuId) ->
    Name = ?SERVER(RuId),
    gen_server:start_link({local, Name}, ?MODULE, [Name, RuId], []).


stop(RuId) when is_integer(RuId) ->
    stop(integer_to_list(RuId));
stop(_RuId) ->
    gen_server:cast(whereis(?SERVER(_RuId)), stop).

init([Name, _RuId]) ->
    ItcPort = itc:open(?ITC_LOG_ESI),
    itc:listen(ItcPort),
    {ok, #state{name     = Name,
		state    = idle,
		itc_port = ItcPort}}.

get_ext_boards() ->
    gen_server:call(?SERVER(RuId),
		    get_ext_boards, 
		    10000).

get_ext_board_info(RuId, Mbox, Timeout) when is_integer(RuId) ->
    get_ext_board_info(integer_to_list(RuId), Mbox, Timeout);
get_ext_board_info(RuId, Mbox, Timeout) ->
    gen_server:call(?SERVER(RuId),
		    {get_ext_board_info, self(), RuId, Mbox}, 
		    Timeout).

ext_esi_ready(RuId, Mbox) when is_integer(RuId) ->
    ext_esi_ready(integer_to_list(RuId), Mbox);
ext_esi_ready(RuId, Mbox) ->
    gen_server:cast(?SERVER(RuId), {ext_esi_ready, self(), RuId, Mbox}).

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Reason, _S) ->
    ok.




handle_call(get_ext_boards, 
	    _From,
	    #state{cat_mbox = undefined} = S) ->
    {reply, {ok, [], undefined}, S};
handle_call(get_ext_boards, 
	    From,
	    #state{itc_port = ItcPort,
		   cat_mbox = Mbox} = S) ->
    sysInitI:info_msg("~p Send LOG_ESI_EXT_GET_BOARDS_RREQ. ~p ~n", 
		      [?MODULE, Mbox]),
    handle_send_itc(?LOG_ESI_EXT_GET_BOARDS_RREQ, Mbox, ItcPort, ""),
    {noreply, S#state{user_id = From,
		      state   = ?S_GET_BOARDS}};

handle_call({get_ext_board_info, _UserId, RuId, Mbox}, 
	    From,
	    #state{itc_port = ItcPort} = S) ->
    sysInitI:info_msg("~p Send LOG_ESI_EXT_GET_BOARD_INFO_RREQ. ~p ~n", 
		      [?MODULE, Mbox]),
    handle_send_itc(?LOG_ESI_EXT_GET_BOARD_INFO_RREQ, Mbox, ItcPort, RuId),
    {noreply, S#state{user_id = From}};
handle_call(Command, {Pid, _}, S) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(Pid)] ++
	[{unrecognized_msg, Command} | ?STATE_INFO(S)],
    sysInitI:warning_report(Info),
    {reply, Command, S}.


handle_cast({ext_esi_ready, _UserId, RuId, Mbox}, 
	    #state{itc_port = ItcPort} = S) ->
    sysInitI:info_msg("~p Send LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ. ~p ~n", 
		      [?MODULE, Mbox]),
    handle_send_itc(?LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ, Mbox, ItcPort, RuId),
    {noreply, S};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {unrecognized_msg, Request} | ?STATE_INFO(S)]),
    {noreply, S}.


%%=======================================================================
%% Establish request
%%=======================================================================
handle_info({message,
	     _FromPid,
	     {FromMbox, _ToMbox, ?LOG_ESI_EXT_CONN_ESTABLISH_UREQ, _Data}},
	    #state{itc_port = ItcPort} = S) ->
    sysInitI:info_msg("~p Received LOG_ESI_EXT_CONN_ESTABLISH_UREQ. ~p ~n", 
		      [?MODULE, FromMbox]),
    _Ref = itc:attach(ItcPort, FromMbox),
    {noreply, S#state{cat_mbox = FromMbox,
		      state    = ?S_ESTABLISHED}};

%%=======================================================================
%% Boards info confirm
%%=======================================================================
handle_info({message,
	     _FromPid,
	     {FromMbox, _ToMbox, ?LOG_ESI_EXT_GET_BOARDS_RCFM, Data}},
	    #state{user_id = From} = S) ->
    sysInitI:info_msg("~p Received LOG_ESI_EXT_GET_BOARDS_RCFM. ~p ~n", 
		      [?MODULE, FromMbox]),
    {RUs, _Rem} = decode_boards_cfm(Data),
    gen_server:reply(From, {ok, RUs, FromMbox}),
    {noreply, S#state{cat_mbox = FromMbox,
		      state    = ?S_ESTABLISHED}};

%%=======================================================================
%% Board info
%%=======================================================================
handle_info({message,
	     _FromPid,
	     {FromMbox, _ToMbox, ?LOG_ESI_EXT_GET_BOARD_INFO_RCFM, Data}},
	    #state{user_id = From} = S) ->
    sysInitI:info_msg("~p Received LOG_ESI_EXT_GET_BOARD_INFO_RCFM. ~p ~n", 
		      [?MODULE, FromMbox]),
    {RuId, IpAddr, Port, _NameSpace, Dirs} = decode_board_info(Data),
    gen_server:reply(From, {ok, {RuId, IpAddr, Port, Dirs}}),
    {noreply, S};

%%=======================================================================
%% MBOX DOWN
%%=======================================================================
handle_info({mailbox_down, _Port, _Ref, Mbox},
	    #state{cat_mbox = Mbox} = S) ->
    sysInitI:info_msg("~p Mailbox down. ~p ~n", [?MODULE, Mbox]),
    {noreply, S#state{cat_mbox = undefined}};
handle_info({mailbox_down, _Port, _Ref, Mbox},
	    #state{cat_mbox = CatMbox} = S) ->
    sysInitI:warning_msg("~p Mailbox down. Wrong Mailbox.~n"
			 "  Got  ~p, ~n"
			 "  Had  ~p, ~n",
			 [?MODULE, Mbox, CatMbox]),
    {noreply, S#state{cat_mbox = undefined}};

handle_info(Info, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {unrecognized_msg, Info},
			     {state, S}]),
    {noreply, S}.



%%============================================================================
%% @doc esi handling for external boards
%%
%% 
%% @end
%%============================================================================
generate_esi({_RuId, _Mbox}, _Timeout) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
handle_send_itc(?LOG_ESI_EXT_GET_BOARDS_RREQ = SigNo, MboxId, ItcPort, _RuId) ->
    itc_send(ItcPort, MboxId, SigNo, <<>>);
handle_send_itc(SigNo, MboxId, ItcPort, RuId) ->
    Bid  = create_max_bin(RuId, ?LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH), 
    Data = <<Bid/binary >>,
    itc_send(ItcPort, MboxId, SigNo, Data).



itc_send(ItcPort, MboxId, SigNo, Data) ->
    itc:send(ItcPort, MboxId, SigNo, Data).


create_max_bin(Str, MaxLen) ->
    {MaxStr, _} = lists:split(MaxLen, Str ++ lists:duplicate(MaxLen, 16#0)),   
    list_to_binary(MaxStr).




%%=======================================================================
%% Decode Boards info confirm
%%=======================================================================
decode_boards_cfm(Data) ->
    {Noof,  R1}   = decode_32(Data),
    {_BoNo, _Rest} = decode_boards(Noof, R1, []).


decode_boards(Noof, Data, Acc) when Noof =< 0 ->
    {Acc, Data};
decode_boards(_Noof, <<>>, Acc) ->
    {Acc, []};
decode_boards(Noof, Data, Acc) ->
    <<NoB:?LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH/binary, R1/binary>> = Data,
    {No, _} = decode_string(NoB),
    decode_boards(Noof - 1, R1, [No | Acc]).



%%=======================================================================
%% Decode Board info
%%=======================================================================
decode_board_info(Data) ->
    <<RuIdB:?LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH/binary, R1/binary>> = Data,
    <<IpAddB:?LOG_ESI_EXT_MAX_IP_ADD_LENGTH/binary,     R2/binary>> = R1,
    {RuId,    _} = decode_string(RuIdB),
    {IpAddr,  _} = decode_string(IpAddB),
    {Port,   R3} = decode_32(R2),
    <<NsB:?LOG_ESI_EXT_NAMESPACE_MAX_LENGTH/binary, R4/binary>> = R3,
    {NameSp, _} = decode_string(NsB),
    {NoDirs, R5} = decode_32(R4),
    {RuId, format_ip(IpAddr), Port, NameSp, decode_bi_dirs(NoDirs, R5, [])}.

format_ip(IpAddr) ->
    case inet:parse_ipv6_address(IpAddr) of
	{ok, _} -> "[" ++ IpAddr ++ "]";
	_       -> IpAddr
    end.

decode_bi_dirs(Noof, Rest, Acc) when Noof =< 0 ->
    {Acc, Rest};
decode_bi_dirs(Noof, Rest, Acc) ->
    {Size, R1} = decode_32(Rest),
    <<DirB:?LOG_ESI_EXT_DIR_PATH_MAX_LENGTH/binary, R2/binary>> = R1,
    {Dir,  _} = decode_string(DirB),
    decode_bi_dirs(Noof - 1, R2, [{Dir, Size} | Acc]).
    




decode_8(<<Int:1/native-unsigned-integer-unit:8, Rest/binary>>) ->
    {Int, Rest}.

decode_32(<<Int:1/native-unsigned-integer-unit:32, Rest/binary>>) ->
    {Int, Rest}.

decode_string(String) ->
    dec_str(String, []).

dec_str(<<>>, Acc) ->
    {Acc, <<>>};
dec_str(Str, Acc) ->
    case decode_8(Str) of
%% 	{0, Rem}  when Acc == [] -> dec_str(Rem, Acc);
	{0, Rem} -> {lists:reverse(Acc), Rem};
	{S, Rem} -> dec_str(Rem, [S | Acc])
    end.




%% warning_msg(Line, Format, Args) ->
%%     sysInitI:warning_msg("~w:~p " ++ Format, [?MODULE, Line | Args]).




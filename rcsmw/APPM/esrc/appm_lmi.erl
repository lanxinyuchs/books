%%%-------------------------------------------------------------------
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2017
%%% @doc
%%%
%%% @end
%%% Created :  8 Aug 2012 by Arto Nummelin
%%%-------------------------------------------------------------------
-module(appm_lmi).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R10A/R11A/1').
-date('2017-09-28').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseFile:	appm_lmi.erl %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R2A/1     20121205    etxarnu     Updated for CEC
%%% R2A/8     20131101    etxarnu     Removed incorrect ERROR REPORT
%%% R2A/9     20140110    etxarnu     Added signPgm
%%% R2A/10    20140411    etxpejn     Added heartbeat signal
%%% R2A/11    20140417    etxberb     Removed return signal for heartbeat.
%%% R3A/1     20141128    etxarnu     Added phase to get_lms
%%%                                   Added ns to start_pgm
%%% R4A/1     20150812    etxarnu     Added printout if error in getLms
%%% R4A/2     20150812    etxarnu     Changed to info printout
%%% R4A/3     20150901    etxarnu     Reduced info printouts
%%% R4A/4     20150911    etxarnu     Handle DuId correctly (cluster)
%%% R5A/1     20160309    etxarnu     Added LmiGetPids
%%% ----    ---------- -------  ------------------------------------------------
%%% R6A/1   2016-06-16 etxberb  Added HwCategory & HwModel
%%% R6A/2   2016-07-08 etxberb  Added HwType in search functions.
%%% R8A/1   2016-12-15 etxarnu  Added LmiAddBoard
%%% R8A/2   2016-12-17 etxarnu  Added set_fake_resp/clear_fake_resp for add_board
%%% R10A/1  2017-06-30 uabesvi  Added LmiAddExtBoard
%%% R11A/1  2017-09-06 etxarnu  Added mboxId to startPgm
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%% API
-export([message/1]).

%%% Callback from SWM that board has been added (or addition failed)
-export([board_added/3]).

%%% LOG asks for all external boards. Used to generate ESI from external boards
-export([get_ext_boards/0]).

%%% For test on can set a fake swmI:add_board response
-export([set_fake_resp/3,clear_fake_resp/2]).

-define(LMHI_MAX_PRODUCT_NUMBER_LEN,   25).
-define(LMHI_MAX_PRODUCT_REVISION_LEN,  8).
-define(LMHI_MAX_REASON_LEN,           64).

-define(LMHI_ADD_BOARD_RSP,       16#01860000).
-define(LMHI_ADD_BOARD_REJ,       16#01860001).
-define(LMHI_ADD_EXT_BOARD_RSP,   16#01860002).

-include("appm.hrl").
-include("lmi.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Handle a LMI message
%% @spec message(Bytes) -> ReturnedBytes
%% @end
%%--------------------------------------------------------------------
message(Bytes) ->
    Msg = lmi:decode_msg(Bytes,'LmiMessage'),
%%%    case Msg of
%%% 	#'LmiMessage'{getLms = GetLms} when GetLms /= undefined ->
%%% 	    io:format("~p: Decoded message:\n~p\n\n", [?MODULE, Msg]);
%%% 	_ ->
%%% 	    ok
%%%    end,
    result_msg(Msg).


%%%===================================================================
%%% Internal functions
%%%===================================================================

result_msg(#'LmiMessage'{addBoard = AddBoard}) when AddBoard =/= undefined ->
    BoardNo = binary_to_list(AddBoard#'LmiAddBoard'.boardNo),
    BoardRev = binary_to_list(AddBoard#'LmiAddBoard'.boardRev),
    MboxId = AddBoard#'LmiAddBoard'.mboxId,
    R = case add_board( BoardNo, BoardRev, MboxId) of
	    ok ->
		#'LmiAddBoardRet'{result = 'OK'};
	    wait ->
		#'LmiAddBoardRet'{result = 'WAIT'};
	    {error, Error} ->
		appmLib:log(info,
			    ?MODULE_STRING ++ " - add_board with NOK, result ~p ~n"
			    "BoardNo=~p~n"
			    "BoardRev=~p~n"
			    "MboxId=~p~n",
			    [Error,
			     BoardNo,
			     BoardRev,
			     MboxId]),
		#'LmiAddBoardRet'{result = 'NOK'}
	end,

    %% io:format("Result message:\n~p\n\n", [R]),
    lmi:encode_msg(R);

result_msg(#'LmiMessage'{addExtBoard = AddExtBoard}) when AddExtBoard =/= undefined ->
    BoardNo = binary_to_list(AddExtBoard#'LmiAddExtBoard'.boardNo),
    BoardRev = binary_to_list(AddExtBoard#'LmiAddExtBoard'.boardRev),
    MboxId = AddExtBoard#'LmiAddExtBoard'.mboxId,
    add_ext_board( BoardNo, BoardRev, MboxId),
    R = #'LmiAddExtBoardRet'{result = 'OK'},
    %% io:format("Result message:\n~p\n\n", [R]),
    lmi:encode_msg(R);

result_msg(#'LmiMessage'{deleteExtBoard = DelExtBoard}) when DelExtBoard =/= undefined ->
    BoardNo = binary_to_list(DelExtBoard#'LmiDeleteExtBoard'.boardNo),
    BoardRev = binary_to_list(DelExtBoard#'LmiDeleteExtBoard'.boardRev),
    MboxId = DelExtBoard#'LmiDeleteExtBoard'.mboxId,
    delete_ext_board( BoardNo, BoardRev, MboxId),
    R = #'LmiDeleteExtBoardRet'{result = 'OK'},
    %% io:format("Result message:\n~p\n\n", [R]),
    lmi:encode_msg(R);

result_msg(#'LmiMessage'{getLms = GetLms}) when GetLms =/= undefined ->
    %% Handle = GetLms#'LmiGetLMs'.handle,
    HwCategory = GetLms#'LmiGetLMs'.hwCategory,
    HwModel = GetLms#'LmiGetLMs'.hwModel,
    BoardType = GetLms#'LmiGetLMs'.boardType,
    BoardRev = GetLms#'LmiGetLMs'.boardRev,
    Tag = GetLms#'LmiGetLMs'.tag,
    Phase = is_preload(GetLms#'LmiGetLMs'.phase),
    R = case get_lms(Phase, HwCategory, HwModel, BoardType, BoardRev, Tag) of
	    {ok,Data} ->
		#'LmiGetLMsResult'{result = 'OK',
				   lmdata = Data};
	    {error, Error} ->
		appmLib:log(info,
			    ?MODULE_STRING ++ " - get_lms with NOK result ~p ~n"
			    "HwCategory=~p~n"
			    "HwModel=~p~n"
			    "BoardType=~p~n"
			    "BoardRev=~p~n"
			    "Tag=~p~n"
			    "Phase=~p~n",
			    [Error,
			     HwCategory,
			     HwModel,
			     BoardType,
			     BoardRev,
			     Tag,
			     Phase]),
		#'LmiGetLMsResult'{result = 'NOK',
				   lmdata = []}
	end,

    %% io:format("Result message:\n~p\n\n", [R]),
    lmi:encode_msg(R);

result_msg(#'LmiMessage'{getPids = GetPids}) when GetPids =/= undefined ->
    DuId = GetPids#'LmiGetPids'.duId,
    PgmId = GetPids#'LmiGetPids'.pgmId,
    R = case get_pids(DuId,PgmId) of
	    {ok,[{pids,Pids}]} ->
		appmLib:log(info, ?MODULE_STRING ++ 
				" - get_pids(~p) -> Pids=~p ~n",
			    [ PgmId, Pids]),
		#'LmiGetPidsResult'{result = 'OK',
		 		    pidData = Pids};
	    {error, Error} ->
		appmLib:log(info,
			    ?MODULE_STRING ++ " - get_pids with NOK result ~p ~n"
			    "PgmId=~p~n",
			    [Error, PgmId]),
		#'LmiGetPidsResult'{result = 'NOK',
				   pidData = []}
	end,

    lmi:encode_msg(R);

result_msg(#'LmiMessage'{startPgm = StartPgm}) when StartPgm =/= undefined ->
    PgmName = StartPgm#'LmiStartPgm'.pgmName, 
    LmId = StartPgm#'LmiStartPgm'.lmId,
    Ns = StartPgm#'LmiStartPgm'.ns,
    DuId = StartPgm#'LmiStartPgm'.duId,
    CpuSet = StartPgm#'LmiStartPgm'.cpuSet,
    Args = StartPgm#'LmiStartPgm'.arg,
    MboxId = StartPgm#'LmiStartPgm'.mboxId,

    R =	case appmServer:start_dynamic_lm(
	       {PgmName, LmId, DuId, CpuSet, Ns, Args, MboxId}) of
	    {ok, PgmId} ->
		#'LmiStartPgmRet'{result = 'OK',
				  pgmId  = PgmId};
	    {error, _Error} ->
		#'LmiStartPgmRet'{result = 'NOK',
				  pgmId  = 0}
	end,

    lmi:encode_msg(R);

result_msg(#'LmiMessage'{stopPgm = StopPgm}) when StopPgm =/= undefined ->
    PgmId = StopPgm#'LmiStopPgm'.pgmId,
    DuId = StopPgm#'LmiStopPgm'.duId,

    R = case appmServer:stop_dynamic_lm(DuId,PgmId) of
	    ok ->
		#'LmiStopPgmRet'{result = 'OK'};
	    {error, _Reason} ->
		#'LmiStopPgmRet'{result = 'NOK'}
	end,

    lmi:encode_msg(R);

result_msg(#'LmiMessage'{signPgm = SignPgm}) when SignPgm =/= undefined ->
    PgmId = SignPgm#'LmiSignPgm'.pgmId,
    DuId = SignPgm#'LmiSignPgm'.duId,
    SigNo = SignPgm#'LmiSignPgm'.sigNo,

    R = case appmServer:signal_to_pgm(DuId,PgmId,SigNo) of
	    ok ->
		#'LmiSignPgmRet'{result = 'OK'};
	    {error, _Reason} ->
		#'LmiSignPgmRet'{result = 'NOK'}
	end,

    lmi:encode_msg(R);

result_msg(#'LmiMessage'{heartbeat = Heartbeat}) when Heartbeat =/= undefined ->
    PgmName = Heartbeat#'LmiHeartbeat'.pgmName,
    LmId = Heartbeat#'LmiHeartbeat'.lmId,
    
    %% TODO, is return value needed?
    ok = appmHbServer:heartbeat_signal({PgmName, LmId}),
    undefined.

%%%===================================================================
add_board(BoardNo, BoardRev, MboxId) ->
    case add_board_int(BoardNo, BoardRev) of
	ok ->
	    ok;
	wait ->
	    store_board_adder(BoardNo, BoardRev,MboxId),
	    wait;
	{error,R} ->
	    {error,R}
    end.

add_board_int(BoardNo, BoardRev) ->
    case ets:lookup(appm_add_board,{fake_resp, {BoardNo, BoardRev}}) of
	[{_,Resp}] ->
	    Resp;
	_ ->
	    swmI:add_board(BoardNo, BoardRev, ?MODULE)
    end.


%%%===================================================================
get_ext_boards() ->
    ets:tab2list(appm_add_ext_board).

%%%===================================================================
add_ext_board(BoardNo, BoardRev, MboxId) ->
    ets:insert(appm_add_ext_board, {{BoardNo, BoardRev}, MboxId}).


%%%===================================================================
delete_ext_board(BoardNo, BoardRev, _MboxId) ->
    ets:delete(appm_add_ext_board, {BoardNo, BoardRev}).



%%% test functions for add_board to set fake response
set_fake_resp(BoardNo, BoardRev,Resp = {error,_}) ->
    ets:insert(appm_add_board, {{fake_resp, {BoardNo, BoardRev}}, Resp});
set_fake_resp(BoardNo, BoardRev,Resp)
  when  Resp == ok; Resp == wait ->
    ets:insert(appm_add_board, {{fake_resp, {BoardNo, BoardRev}}, Resp});
set_fake_resp(_BoardNo, _BoardRev,Resp) ->
    io:format("Response not acceptable ~p~n",[Resp]).


clear_fake_resp(BoardNo, BoardRev)  ->
    ets:delete(appm_add_board, {fake_resp, {BoardNo, BoardRev}}).


%%%
%%%
	
%%% Callback from SWM that board has been added (or addition failed)
board_added(BoardNo, BoardRev, Result) ->
    case find_board_adder(BoardNo, BoardRev) of
	{ok,MboxId} ->	    
	    send_rsp(MboxId, BoardNo, BoardRev,Result);
	{error,Reason} ->
	    appmLib:log(info,
			?MODULE_STRING ++ " - find_board_adder failed with reason ~p ~n"
			    "BoardNo=~p~n"
			    "BoardRev=~p~n"
			    "ResultFromSWM=~p~n",
			    [Reason,
			     BoardNo,
			     BoardRev,
			     Result])
    end.

 
store_board_adder(BoardNo, BoardRev,MboxId)  ->
    ets:insert(appm_add_board, {{BoardNo, BoardRev}, MboxId}).

find_board_adder(BoardNo, BoardRev) ->
    case ets:lookup(appm_add_board, {BoardNo, BoardRev}) of
	[] ->
	    {error, board_not_found};
	[{_,MboxId}] ->
	    ets:delete(appm_add_board, {BoardNo, BoardRev}),
	    {ok,MboxId}
    end.


send_rsp(MboxId, BoardNo, BoardRev,ok) ->
    ItcPort = get_itc_port(),
    Data = [to_c_string(BoardNo, ?LMHI_MAX_PRODUCT_NUMBER_LEN),
	    to_c_string(BoardRev, ?LMHI_MAX_PRODUCT_REVISION_LEN)],
    itc:send(ItcPort, MboxId, ?LMHI_ADD_BOARD_RSP, Data);
send_rsp(MboxId, BoardNo, BoardRev,{error,Reason}) ->
    ItcPort = get_itc_port(),
    Data = [to_c_string(BoardNo, ?LMHI_MAX_PRODUCT_NUMBER_LEN),
	    to_c_string(BoardRev, ?LMHI_MAX_PRODUCT_REVISION_LEN),
	    to_c_string(Reason, ?LMHI_MAX_REASON_LEN)],
    itc:send(ItcPort, MboxId, ?LMHI_ADD_BOARD_REJ, Data).

get_itc_port() ->
    appmServer:get_itc_port().



%%%===================================================================
get_lms(Phase, HC, HM, BT, BR, Tag) ->
    BoardType =
	case BR of
	    <<>> ->
		{binary_to_list(BT), ?DEFAULT_BoardRev};
	    _ ->
		{binary_to_list(BT), binary_to_list(BR)}
	end,
    HwType =
	case HM of
	    <<>> ->
		{binary_to_list(HC), ?DEFAULT_HwModel};
	    _ ->
		{binary_to_list(HC), binary_to_list(HM)}
	end,
	    
    case appmAppData:get_lm_files(Phase,
				  HwType,
				  BoardType,
				  binary_to_list(Tag)) of
	{error,Reason} ->
	    {error,Reason};
	Data ->
	    {ok,restruct_lms(Data,[])}
    end.

restruct_lms([],Acc) ->
    Acc;

restruct_lms([{Name,Id,Rev,Files}|T],Acc) ->
    restruct_lms(T, [#'LmiLmData'
		     {id=#'LmiProdId'{name=Name,id=Id, rev=Rev},
		      file=restruct_files(Files,[])} | Acc]);
restruct_lms([{error,_Reason}|T],Acc) ->
% Ignore files in lmlist not matching Tag
    restruct_lms(T, Acc).

restruct_files([],Acc) ->
    Acc;

restruct_files([{Type,Path}|T],Acc) ->
    restruct_files(T, [#'LmiFileInfo'{type=Type,path=Path} | Acc]).

%%%===================================================================
to_c_string(Text, Length) when is_list(Text) ->
    to_c_string(list_to_binary(Text), Length);
to_c_string(Text, Length) ->
    case byte_size(Text) of
        TextLength when TextLength >= Length ->
            [binary:part(Text, 0, Length-1), 0];
        TextLength ->
            [Text, binary:copy(<<0>>, Length-TextLength)]
    end.


%%%===================================================================
get_pids(DuId,PgmId) ->   
    appmServer:get_pids(DuId,PgmId).

%%%===================================================================
is_preload(0) ->
    false;
is_preload(1) ->
    true.

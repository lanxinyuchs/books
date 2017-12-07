%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logDbg.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(logDbg).
-vsn('/main/R1A/R10A/R11A/4').
-date('2017-10-02').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-02-27   etxjotj     Created
%%% R10A/2     2017-06-22   uabesvi     Tests for RU ESI
%%% R11A/1     2017-08-30   uabesvi     Tests for RU ESI
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([fill/2, fill/5]).

-export([itc_start/1]).
-export([itc_stop/1]).
-export([itc_loop/1]).
-export([itc_hunt/2]).
%% -export([itc_register/2]).
%% -export([itc_add_fru/3]).
%% -export([itc_del_fru/3]).
%% -export([itc_reply/2]).

-export([generate_esi/0]).

-include("log.hrl").


-define(LOG_ESI_EXT_CALLBACK,       16#0191001).
-define(ESI_EXT_CALLBACK_DATA,      16#0191002).
-define(LOG_ESI_EXT_CALLBACK_READY, 16#0191003).

-record(itc_data, {name, 
		   own_port,
		   coli_port,
		   coli_ref,
		   coli_pid,
		   coli_mboxid,
		   hunt_ref,
		   user_pid,
		   replies = []
		   }).

-define(RU_NAME, justus).		   

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

fill(Name, N) ->
    fill(Name, "logDbg", info, "TestTestTestTestTest", N).

fill(Name, User, Severity, Msg, N) when N>0->
    logI:write_log(Name, User, Severity, Msg),
    fill(Name, User, Severity, Msg, N-1);
fill(_, _, _, _, _) -> ok.





itc_start(_Name) ->
    Self = self(),
    spawn(fun() -> itc_init(?RU_NAME, Self) end),
    receive
	{inited, Mbox} ->
	    Mbox
    after 3000 ->
	    io:format("ITC_LOG_EXT_APP itc_start ~p~n", [timeout]),
	    timeout
    end.
    

itc_stop(_Name) ->
    stop(?RU_NAME),
    ok.

stop(Name) ->
    Name ! {stop, Name, self()},
    receive
	stopped ->
	    ok
    after 3000 ->
	    unregister(Name),
	    io:format("ITC_LOG_EXT_APP itc_stop ~p~n", [timeout]),
	    timeout
    end.
   
generate_esi() ->
    ?RU_NAME ! generate_esi.

    

itc_init(Name, UserPid) ->
    logEsi:register_esi_cb(?MODULE),
    io:format("ITC_LOG_EXT_APP Process started ~p~n", [Name]),
    ii(whereis(Name), Name),
    ItcPort = itc:open(atom_to_list(Name)),
    io:format("ITC_LOG_EXT_APP ITC port ~p~n", [itc:get_id(ItcPort)]),
    itc:listen(ItcPort),
    UserPid ! {inited, itc:get_id(ItcPort)},
    itc_loop(#itc_data{name     = Name,
		       own_port = ItcPort}).

ii(undefined, Name) ->
    register(Name, self());
ii(_Pid, Name) ->
    stop(Name),
    register(Name, self()).
   
    

%% itc_register(Name, FruType) ->
%%     Name ! {itc_register, FruType},
%%     ok.

%% itc_add_fru(Name, FruType, Ldn) ->
%%     Name ! {itc_add_fru, FruType, Ldn},
%%     ok.

%% itc_del_fru(Name, FruType, Ldn) ->
%%     Name ! {itc_del_fru, FruType, Ldn},
%%     ok.

itc_hunt(Name, Hunt) ->
    Name ! {itc_hunt, self(), Hunt},
    receive
	hunted ->
	    ok;
	5000 ->
	    {error, hunt_timeout}
    end.

%% itc_reply(Name, Reply) ->
%%     Name ! {itc_reply, Reply},
%%     ok.



itc_loop(#itc_data{name        = Name,
		   own_port    = OwnPort,
%%		   coli_port   = ColiPort,
		   coli_mboxid = ColiMboxId,
		   coli_ref    = _ColiRef,
		   hunt_ref    = HuntRef,
		   user_pid    = _UserPid
%%		   replies     = Replies
		  } = Loop) ->
    io:format("ITC_LOG_EXT_APP Loop ~p~n", [Loop]),
    receive
	{itc_hunt, NewUserPid, Hunt} ->
	    NewHuntRef = itc:hunt(OwnPort, Hunt),
	    io:format("ITC_EXT_LOG_APP Hunt ~p ~p~n~p~n", 
		      [OwnPort, Hunt, NewHuntRef]),
	    itc_loop(Loop#itc_data{hunt_ref = NewHuntRef,
				   user_pid = NewUserPid});
	generate_esi ->
	    io:format("ITC_EXT_LOG_APP generate_esi~n", []),
	    NewHuntRef = itc:hunt(OwnPort, ?ITC_LOG_ESI),
	    io:format("ITC_EXT_LOG_APP Hunt ~p ~p~n~p~n", 
		      [OwnPort, ?ITC_LOG_ESI, NewHuntRef]),
	    itc_loop(Loop#itc_data{hunt_ref = NewHuntRef,
				   user_pid = self()});
	{message,
	 _, 
	 {FrMboxId, _ToMboxId, ?LOG_ESI_EXT_GET_BOARD_INFO_RREQ, MsgData} = _M} ->
	    BoardId = decode_cmd_req(MsgData),
   	    io:format("ITC_EXT_LOG_APP ~p: MESSAGE ~p~n",
                    [BoardId, "LOG_ESI_EXT_GET_BOARD_INFO_RREQ"]),
	    timer:sleep(1000),


	    IpA = "10.68.97.237",

	    BoardNo = create_max_bin(BoardId, 
				     ?LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH),
	    IpAddr  = create_max_bin(IpA, 
				     ?LOG_ESI_EXT_MAX_IP_ADD_LENGTH),
	    Port    = 4443,
	    NameSpace = create_max_bin("", 
				       ?LOG_ESI_EXT_NAMESPACE_MAX_LENGTH),
	    NoofDirs = 2,

%% 	    Dir1 = create_max_bin("/home/uabesvi/tmp/kemi", 
%% 				      ?LOG_ESI_EXT_DIR_PATH_MAX_LENGTH),
%% 	    Dir2 = create_max_bin("/home/uabesvi/tmp/kalle", 
%% 				      ?LOG_ESI_EXT_DIR_PATH_MAX_LENGTH),

	    Dir1 = create_max_bin("/sune", 
				      ?LOG_ESI_EXT_DIR_PATH_MAX_LENGTH),
	    Dir2 = create_max_bin("/kalle", 
				      ?LOG_ESI_EXT_DIR_PATH_MAX_LENGTH),

	    Dir1Size = 30011,
	    Dir2Size = 99999,

	    Data = <<BoardNo/binary,
		    IpAddr/binary,
		    Port:1/native-unsigned-integer-unit:32,
		    NameSpace/binary,
		    NoofDirs:1/native-unsigned-integer-unit:32,
		    Dir1Size:1/native-unsigned-integer-unit:32,
		    Dir1/binary,
		    Dir2Size:1/native-unsigned-integer-unit:32,
		    Dir2/binary,
		    0>>,
	    io:format("ITC_EXT_LOG_APP ~p: reply directories ~p~n",
		      [Name, Data]),
	    itc:send(OwnPort, FrMboxId, ?LOG_ESI_EXT_GET_BOARD_INFO_RCFM, Data),
	    itc_loop(Loop#itc_data{replies = []});

	{message,
	 _, 
	 {FrMboxId, _ToMboxId, ?LOG_ESI_EXT_GET_BOARDS_RREQ, _MsgData} = _M} ->
    	    io:format("ITC_EXT_LOG_APP ~p: MESSAGE ~p~n",
		      [Name, "LOG_ESI_EXT_GET_BOARDS_RREQ"]),
	    timer:sleep(1000),

	    NoofBoards = 2,
	    BoardNo1 = create_max_bin("sune", 
				      ?LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH),
	    BoardNo2 = create_max_bin("berit", 
				      ?LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH),

	    io:format("ITC_EXT_LOG_APP BoardNo1 ~p~n", [BoardNo1]),
	    io:format("ITC_EXT_LOG_APP BoardNo2 ~p~n", [BoardNo2]),


	    Data = <<NoofBoards:1/native-unsigned-integer-unit:32,
		    BoardNo1/binary,
		    BoardNo2/binary>>,
	    io:format("ITC_EXT_LOG_APP ~p: reply boards ~p~n",
		      [Name, Data]),
	    itc:send(OwnPort, FrMboxId, ?LOG_ESI_EXT_GET_BOARDS_RCFM, Data),
	    itc_loop(Loop#itc_data{replies = []});
	{message,
	 _, 
	 {_FrMboxId, _ToMboxId, ?LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ, MsgData} = _M} ->
	    BoardId = decode_cmd_req(MsgData),
%%    	    io:format("ITC_EXT_LOG_APP ~p: MESSAGE ~p~n", [Name, _M]),
   	    io:format("ITC_EXT_LOG_APP ~p: MESSAGE ~p~n",
                    [BoardId, "LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ"]),
	    itc_loop(Loop#itc_data{replies = []});
	{mailbox_up, OwnPort, HuntRef, Pid} = M ->
	    io:format("ITC_EXT_LOG_APP ~p: mailbox_up ~p~n", [Name, M]),
	    NewColiRef = itc:attach(OwnPort, Pid),
	    io:format("ITC_EXT_LOG_APP ~p: mailbox_up ~p ~p~n", 
		      [Name, Pid, NewColiRef]),
%%	    UserPid ! hunted,
	    send_conn_est(OwnPort, Pid),
	    itc_loop(Loop#itc_data{coli_mboxid = Pid,
				   hunt_ref    = undefined,
				   user_pid    = undefined});
	{mailbox_down, OwnPort, _ColiRef, ColiMboxId} ->
	    io:format("ITC_EXT_LOG_APP mailbox down ~p~n", [Name]),
	    itc_loop(Loop);
	{stop, Name, User} ->
	    itc:close(OwnPort),
	    unregister(Name),
	    User ! stopped,
	    %%io:format("ITC_EXT_LOG_APP Process stopped ~p~n", [Name]),
	    stopped;
	Unknown ->
	    io:format("ITC_EXT_LOG_APP Unknown message ~p~n", [Unknown]),
	    itc_loop(Loop)
    end.


%% get_reply() ->
%%     Reply = "/home/uabesvi/rcs,123;/home/uabesvi/test/r6;/home/uabesvi/test/r8, 22;",
%%     list_to_binary(Reply).


decode_cmd_req(Data) ->
    {BoardId,  _} = decode_string(Data),
    BoardId.



decode_string(String) ->
    dec_str(String, []).

dec_str(Str, Acc) ->
    case decode_8(Str) of
	{0, Rem} -> {lists:reverse(Acc), Rem};
	{S, Rem} -> dec_str(Rem, [S | Acc])
    end.


decode_8(<<Int:1/native-unsigned-integer-unit:8, Rest/binary>>) ->
    {Int, Rest}.

%% decode_32(<<Int:1/native-unsigned-integer-unit:32, Rest/binary>>) ->
%%     {Int, Rest}.

    

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

send_conn_est(OwnPort, RcsMB) ->
    io:format("ITC_EXT_LOG_APP sending ~p ~n", 
	      ["LOG_ESI_EXT_CONN_ESTABLISH_UREQ"]),
    Data = <<>>,
    itc:send(OwnPort, RcsMB, ?LOG_ESI_EXT_CONN_ESTABLISH_UREQ, Data).
    

create_max_bin(Str, MaxLen) ->
    {MaxStr, _} = lists:split(MaxLen, Str ++ lists:duplicate(MaxLen, 16#0)),   
    list_to_binary(MaxStr).

    

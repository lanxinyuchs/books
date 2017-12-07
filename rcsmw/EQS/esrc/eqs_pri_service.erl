%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	eqs_pri_service.erl %
%%% Author:	etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(eqs_pri_service).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R6A/R9A/R11A/1').
-date('2017-09-21').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-10-03   etxbjca     Created
%%% R1A/3      2014-04-01   etxarnu     Added get_serial_number/0
%%% R2A/17     2014-05-07   etxpejn     Added COLI board_hwpid/1
%%% R2A/19     2014-06-13   etxtory     Added interface for EA in type 3
%%% R2A/20     2014-08-19   etxarnu     Convert productDate to pri format YYYYMMDD
%%% R3A/1      2014-08-28   etxarnu     Print HW ProdData at start
%%% R3A/3      2014-09-25   etxarnu     Exported get_product_information_data/0
%%% R2A/21     2014-10-03   etxarnu     Merged R3A stuff
%%% R2A/22     2014-10-05   etxpeno     Correct get_product_information_data/1
%%%                                     for non-ARM
%%% R2A/23     2015-01-11   etxarnu     Added printout showing who does a RestartOwnPiu
%%% R3A/4      2014-10-06   etxpeno     Merged R2A stuff
%%% R3A/5-6    2014-10-20   etxpeno     Use rhai-sys to read product info
%%% R3A/8      2015-01-12   etxarnu     Add more printouts at restarts
%%% R3A/9      2015-01-27   etxarnu     Add better printouts at restarts
%%% R3A/10     2015-02-27   etxberb     Added calls to appmI in
%%%                                     'CelloPiu4_restartOtherPiu'/2.
%%% R3A/11     2015-02-03   etxarnu     Add marketName to productInfo
%%% R4A/2      2015-09-14   etxpeno     Cluster corrections
%%% R4A/3      2015-10-08   etxarnu     Added cec_takeover
%%% R4A/5      2015-10-12   etxpeno     increase timeout in 'CelloPiu10_getPid'/2
%%% R6A/1      2016-04-12   etxarnu     Added get_product_information_data("i686_32")
%%% R6A/2      2016-08-29   etxkols     Change fake prodno for simulator
%%% R9A/1      2017-01-27   etxarnu     Fix to handle both vrcs64 and rcssim
%%% R9A/3      2017-04-07   etxpeno     Sanitizing product information data
%%% R11A/1     2017-09-21   etxarnu     Fix for new rcs-sim
%%% ----------------------------------------------------------

-export([start/0,
	 start/1,
	 start_link/0,
	 start_link/1,
	 stop/0]).

-export([activate/0]).

-export([cec_setup/1]).
-export([cec_takeover/1]).

-export([get_serial_number/0]).
-export([get_product/0]).
-export([get_product_number/0]).
-export([get_product_information_data/0]).

%% Function used by COLI
-export([board_hwpid/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% Functions for test purposes:
-export([get_state/0,
	 info/0,
	 info_all/0]).

-define(SERVER, ?MODULE).

-define(PRI_SIGNAL_REVISION, 0).

-define(CELLO_PRI_RESTART_WARM,           0).
-define(CELLO_PRI_RESTART_REFRESH,        1).
-define(CELLO_PRI_RESTART_COLD,           2).
-define(CELLO_PRI_RESTART_COLD_WITH_TEST, 3).

-define(CELLO_PRI_INITIATE_SERVICE_CFM,  16#105C9).
-define(CELLO_PRI_INITIATE_SERVICE_SUS,  16#105CA).
-define(CELLO_PRI_INITIATE_SERVICE_REJ,  16#105CB).
-define(CELLO_PRI_SERVER_UP_IND,         16#105CC).
-define(CELLO_PRI_SERVER_DOWN_IND,       16#105CD).
-define(CELLO_PRI_SERVER_UNPUBLISH_IND,  16#105CE).
-define(CELLO_PRI_TERMINATE_SERVICE_CFM, 16#105D0).
-define(CELLO_PIU3_GET_HUNT_PATH_CFM,    16#10738).
-define(CELLO_PIU3_GET_LH_NAME_CFM,      16#1069B).
-define(CELLO_PIU3_GET_OWN_ID_CFM,       16#1073C).
-define(CELLO_PIU4_RESTART_PIU_CFM,      16#1077C).
-define(CELLO_PIU4_RESTART_PIU_REJ,      16#1077D).
-define(CELLO_PRI8_GET_IDENTITY_CFM,     16#10A5F).
-define(CELLO_PRI9_GET_IDENTITY_CFM,     16#10B0A).
-define(CELLO_PIU10_OPERATIONAL_PID_CFM, 16#10B63).

-define(CELLO_PIU_HUNT_PATH_SIZE, 80).

-define(CELLO_PIU_LH_NAME_SIZE, 7).

-define(CELLO_MAX_PRODUCT_NUMBER_LEN,   25).
-define(CELLO_MAX_PRODUCT_REVISION_LEN,  8).
-define(CELLO_MAX_PRODUCT_NAME_LEN,     13).
-define(CELLO_MAX_PRODUCT_DATE_LEN,      9).
%%% Pad 3 extra bytes to align to the struct CelloPiu3PidInHwCfm
-define(CELLO_MAX_SERIAL_NUMBER_LEN,    14+3).



-define(CELLO_PIU_RO_OK,                          1).
-define(CELLO_PIU_RO_WRONG_HW_ADDRESS,            2).
-define(CELLO_PIU_RO_ERROR_PIU_NOT_FOUND,         3).
-define(CELLO_PIU_RO_HW_PID_NOT_FETCHED,         14).

-define(INIT_SERVICE,                    0).
-define(TERM_SERVICE,                    1).
-define(PIU3_GET_HUNT_PATH,              2).
-define(PIU10_GET_PID,                   3).
-define(PRI8_GET_OWN_IDENTITY,           4).
-define(PRI9_GET_OWN_IDENTITY,           5).
-define(PIU4_RESTART_OWN_PIU,            6).
-define(PIU4_RESTART_OTHER_PIU,          7).
-define(PIU3_GET_OWN_IDENTITY,           8).
-define(PRI9_GET_PIU_OR_DEVICE_IDENTITY, 9).
-define(PRI8_GET_PIU_OR_DEVICE_IDENTITY, 10).
-define(PIU3_GET_LINK_HANDLER_NAME,      11).

%% General
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).

-record(state, {data,
		piu_instance_id,
		product_information_data = []
	       }).

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Get the server process state.
%%%
%%% @end
%%% ###=====================================================================###
get_state() ->
    gen_server:call(?SERVER, {?MODULE, get_state}).

%%% ###########################################################################
%%% @doc Print the server process state and information.
%%%
%%% @end
%%% ###=====================================================================###
info() ->
    Msg = {?MODULE, info},
    gen_server:cast(?SERVER, Msg).

%%% ###=====================================================================###
info_all() ->
    [rpc:cast(Node, ?MODULE, info, []) || Node <- clhI:erlang_nodes(all)].

%%% ###########################################################################
%%% @doc Start the server process.
%%%
%%% @end
%%% ###=====================================================================###
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
    erlang:process_flag(trap_exit, true),
    Data = gb_trees:empty(),
    ProductInformationData = get_product_information_data(),
    sysInitI:info_msg("~p: HW product data: ~p~n",
		      [?MODULE, ProductInformationData]),
    State = #state{data                     = Data,
		   piu_instance_id          = clhI:mp_id(),
		   product_information_data = ProductInformationData},
    {ok, State}.

activate() ->
    gen_server:cast(?SERVER, activate).

cec_setup(Socket) ->
    gen_server:call(?SERVER, {cec_setup, Socket}).

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

board_hwpid(_) ->
    print(gen_server:call(?SERVER, board_hwpid)).

print([]) ->
    done;
print([{productNumber, ProdNo} | Rest]) ->
    io:format("Product Number: ~s~n", [ProdNo]),
    print(Rest);
print([{productRevision, ProdRev}| Rest]) ->
    io:format("Product Revision: ~s~n", [ProdRev]),
    print(Rest);
print([{productName, ProdName}| Rest]) ->
    io:format("Product Name: ~s~n", [ProdName]),
    print(Rest);
print([{marketName, MarketName}| Rest]) ->
    io:format("Market Name: ~s~n", [MarketName]),
    print(Rest);
print([{productDate, Date}| Rest]) ->
    io:format("Product Date: ~s~n", [Date]),
    print(Rest);
print([{serialNumber, SerialNo}| Rest]) ->
    io:format("Product Serial Number: ~s~n", [SerialNo]),
    print(Rest);
print([_Unknown | Rest]) ->
    print(Rest).

get_product() ->
    gen_server:call(?SERVER, get_product).

get_serial_number() ->
    gen_server:call(?SERVER, get_serial_number).

get_product_number() ->
    gen_server:call(?SERVER, get_product_number).

handle_call({cec_setup, Socket}, _From, S) ->
    Reply = self(),
    NewData = gb_trees:insert(Socket, {undefined, undefined}, S#state.data),

    {reply, Reply, S#state{data = NewData}};

handle_call(board_hwpid, _From, State) ->
    ProductInformationData = State#state.product_information_data,
    {reply, ProductInformationData, State};

handle_call(get_product, _From, State) ->
    ProductInformationData = State#state.product_information_data,
    Result =
	try begin
		{ok, Name} = get_product_info(productName,
					      ProductInformationData),
		{ok, No} = get_product_info(productNumber,
					    ProductInformationData),
		{ok, Rev} = get_product_info(productRevision,
					     ProductInformationData),
		Prod = Name ++ " " ++ No ++ " " ++ Rev,
		{ok, Prod}
	    end of
	    {ok, Product} ->
		{ok, Product}
	catch
	    _:Reason ->
		{error, Reason}
	end,
    {reply, Result, State};

handle_call(get_serial_number, _From, State) ->
    ProductInformationData = State#state.product_information_data,
    SerialNumber =
	case get_product_info(serialNumber, ProductInformationData) of
	    {ok,Ser} when is_list(Ser) ->
		Ser;
	    _ ->
		""
	end,
    {reply, SerialNumber, State};

handle_call(get_product_number, _From, State) ->
    ProductInformationData = State#state.product_information_data,
    ProductNumber =
	case get_product_info(productNumber, ProductInformationData) of
	    {ok,Pno} when is_list(Pno) ->
		Pno;
	    _ ->
		""
	end,
    {reply, ProductNumber, State};

handle_call(Command, _From, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {from, _From},
			     {unrecognized_msg, Command} |
			     ?STATE_INFO(S)]),
    {reply, Command, S}.

handle_cast({cec_takeover, Socket}, S) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, S};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(activate, S) ->
    {noreply, S};

handle_cast({?MODULE, info}, S) ->
    sysInitI:info_report(process_info(self()) ++ ?STATE_INFO(S)),
    {noreply, S};
handle_cast(_Msg, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {unrecognized_msg, _Msg} |
			     ?STATE_INFO(S)]),
    {noreply, S}.

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?INIT_SERVICE:4/native-unsigned-integer-unit:8,
	       Spid:4/native-unsigned-integer-unit:8,
	       PvFirstWanted:4/native-unsigned-integer-unit:8,
	       PvSecondWanted:4/native-unsigned-integer-unit:8,
	       PvThirdWanted:4/native-unsigned-integer-unit:8>>},
	    S) ->
    NewState = 'CelloPri_initiateService'({Socket,
					   Spid,
                                           PvFirstWanted,
                                           PvSecondWanted,
                                           PvThirdWanted}, S),
    {noreply, NewState};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?TERM_SERVICE:4/native-unsigned-integer-unit:8>>},
            S) ->
    NewState = 'CelloPri_terminateService'(Socket, S),
    {noreply, NewState};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PIU3_GET_HUNT_PATH:4/native-unsigned-integer-unit:8,
	       PiuInstanceId:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8,
	       ProcessName/binary>>},
            S) ->
    'CelloPiu3_getHuntPath'({Socket, PiuInstanceId, ProcessName, ClientId}, S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PIU10_GET_PID:4/native-unsigned-integer-unit:8,
	       PiuInstanceId:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8>>},
            S) ->
    'CelloPiu10_getPid'({Socket, PiuInstanceId, ClientId}, S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PIU3_GET_OWN_IDENTITY:4/native-unsigned-integer-unit:8>>},
            S) ->
    'CelloPiu3_getOwnIdentity'(Socket, S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PRI8_GET_OWN_IDENTITY:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8>>},
            S) ->
    'CelloPri8_getOwnIdentity'({Socket, ClientId}, S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PRI9_GET_OWN_IDENTITY:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8>>},
            S) ->
    'CelloPri9_getOwnIdentity'({Socket, ClientId}, S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?PIU4_RESTART_OWN_PIU:4/native-unsigned-integer-unit:8,
	       RestartRank:4/native-unsigned-integer-unit:8,
	       RestartEscalation:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8,
	       RestartCause/binary>>},
            S) ->
    sysInitI:info_msg("~p:~n"
		      "~p~n"
		      "call to CelloPiu4_restartOwnPiu~n"
		      "with RestartRank=~p, RestartEscalation=~p, RestartCause=~s~n",
		      [?MODULE, cec:get_program_name(ClientPid),
		       get_rank_str(RestartRank), RestartEscalation =/= 0,
		       get_str(RestartCause)]),
    'CelloPiu4_restartOwnPiu'({Socket, RestartRank, RestartEscalation,
                               RestartCause, ClientId}, S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?PIU4_RESTART_OTHER_PIU:4/native-unsigned-integer-unit:8,
	       PiuInstanceId:4/native-unsigned-integer-unit:8,
	       RestartRank:4/native-unsigned-integer-unit:8,
	       RestartEscalation:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8,
	       RestartCause/binary>>},
            S) ->
    sysInitI:info_msg("~p:~n"
		      "~p~n"
		      "call to CelloPiu4_restartOtherPiu~n"
		      "with PiuInstanceId=~p, RestartRank=~p, RestartEscalation=~p, RestartCause=~s~n",
		      [?MODULE, cec:get_program_name(ClientPid),
		       PiuInstanceId, get_rank_str(RestartRank),
		       RestartEscalation =/= 0, get_str(RestartCause)]),
    'CelloPiu4_restartOtherPiu'({Socket, PiuInstanceId, RestartRank,
				 RestartEscalation,  RestartCause, ClientId},
				S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PRI9_GET_PIU_OR_DEVICE_IDENTITY:4/native-unsigned-integer-unit:8,
	       PiuOrDeviceId:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8>>},
            S) ->
    'CelloPri9_getPiuOrDeviceIdentity'({Socket, PiuOrDeviceId, ClientId}, S),

    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PIU3_GET_LINK_HANDLER_NAME:4/native-unsigned-integer-unit:8,
	       PiuInstanceId:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8>>},
            S) ->
    'CelloPiu3_getLinkHandlerName'({Socket, PiuInstanceId, ClientId}, S),

    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?PRI8_GET_PIU_OR_DEVICE_IDENTITY:4/native-unsigned-integer-unit:8,
	       PiuOrDeviceId:4/native-unsigned-integer-unit:8,
	       ClientId:4/native-unsigned-integer-unit:8>>},
            S) ->
    'CelloPri8_getPiuOrDeviceIdentity'({Socket, PiuOrDeviceId, ClientId}, S),

    {noreply, S};

handle_info({tcp_closed, Socket}, S) ->
    {_Spid, ItcPort} = gb_trees:get(Socket, S#state.data),
    NewData = gb_trees:delete(Socket, S#state.data),
    itc_close(ItcPort),
    NewState = S#state{data = NewData},
    {noreply, NewState};

handle_info(_Msg, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {unrecognized_msg, _Msg} |
			     ?STATE_INFO(S)]),
    {noreply, S}.

terminate(_Reason, S) ->
    ok = data_foreach(
	   fun({Socket, {Spid, ItcPort}}) ->
		   send_pri_server_down_ind({ItcPort, Spid}),
		   itc_close(ItcPort),
		   gen_tcp:close(Socket)
	   end, S#state.data),

    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

'CelloPri_initiateService'({Socket, Spid, PvFirstWanted,
			    _PvSecondWanted, _PvThirdWanted}, State) ->
    ItcPort = itc_open(Spid),

    send_pri_server_up_ind({ItcPort, Spid}),

    SignalRevision = ?PRI_SIGNAL_REVISION,
    SelectedPV = PvFirstWanted,
    send_pri_initiate_service_cfm({ItcPort, Spid},
                                  {SignalRevision, SelectedPV}),

    NewData = gb_trees:update(Socket, {Spid, ItcPort}, State#state.data),
    inet:setopts(Socket, [{active, once}]),
    State#state{data = NewData}.

'CelloPri_terminateService'(Socket, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),
    send_pri_terminate_service_cfm({ItcPort, Spid}),
    NewData = gb_trees:delete(Socket, State#state.data),
    itc_close(ItcPort),
    gen_tcp:close(Socket),
    State#state{data = NewData}.

'CelloPiu3_getHuntPath'({Socket, PiuInstanceId, ProcessName, ClientId},
			State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    HuntPath =
	case get_huntPathPrefix(PiuInstanceId) of
	    "" ->
		ProcessName;
	    HuntPathPrefix ->
		HuntPathPrefix ++ ProcessName
	end,
    Result = ?CELLO_PIU_RO_OK,

    inet:setopts(Socket, [{active, once}]),

    send_piu3_get_hunt_path_cfm({ItcPort, Spid}, {HuntPath, Result, ClientId}).


'CelloPiu10_getPid'({Socket, PiuInstanceId, ClientId}, State) ->
    ProductInformationData =
	case State#state.piu_instance_id of
	    PiuInstanceId ->
		State#state.product_information_data;
	    _ ->
		ErlangNode = clhI:erlang_node(PiuInstanceId),
		try
		    gen_server:call({?SERVER, ErlangNode}, board_hwpid)
		catch
		    _:_ ->
			[]
		end
	end,

    {R1, ProductNumber} =
	get_product_info(productNumber, ProductInformationData),
    {R2, ProductRevision} =
	get_product_info(productRevision, ProductInformationData),
    {R3, ProductName} =
	get_product_info(productName, ProductInformationData),
    {R4, ProductDate} =
	get_product_info(productDate, ProductInformationData),
    {R5, SerialNumber} =
	get_product_info(serialNumber, ProductInformationData),

    Result =
	case lists:any(fun(Res)->Res =:= error end, [R1,R2,R3,R4,R5]) of
	    false -> ?CELLO_PIU_RO_OK;
	    true  -> ?CELLO_PIU_RO_HW_PID_NOT_FETCHED
	end,

    inet:setopts(Socket, [{active, once}]),

    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    send_piu10_operational_pid_cfm({ItcPort, Spid},
                                   {PiuInstanceId,
                                    ProductNumber, ProductRevision,
                                    ProductName, ProductDate, SerialNumber,
                                    Result, ClientId}).

'CelloPiu3_getOwnIdentity'(Socket, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    PiuInstanceId = State#state.piu_instance_id,
    Result = ?CELLO_PIU_RO_OK,

    inet:setopts(Socket, [{active, once}]),

    send_piu3_get_own_id_cfm({ItcPort, Spid}, {PiuInstanceId, Result}).

'CelloPri8_getOwnIdentity'({Socket, ClientId}, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    PiuInstanceId = State#state.piu_instance_id,
    PiuOrDeviceId = PiuInstanceId,
    Apn = get_apn(PiuInstanceId),
    DeviceId = get_deviceId(PiuInstanceId),
    HuntPathPrefix = get_huntPathPrefix(PiuInstanceId),
    SlotNumber = get_slot_number(PiuInstanceId),
    Smn = get_smn(PiuInstanceId),
    Result = ?CELLO_PIU_RO_OK,

    inet:setopts(Socket, [{active, once}]),

    send_pri8_get_identity_cfm({ItcPort, Spid},
                               {PiuInstanceId, PiuOrDeviceId,
                                Smn, SlotNumber, Apn, DeviceId, HuntPathPrefix,
                                Result, ClientId}).

'CelloPri9_getOwnIdentity'({Socket, ClientId}, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    PiuInstanceId = State#state.piu_instance_id,
    PiuOrDeviceId = PiuInstanceId,
    SubrackNumber = get_subrack_number(PiuInstanceId),
    Apn = get_apn(PiuInstanceId),
    DeviceId = get_deviceId(PiuInstanceId),
    HuntPathPrefix = get_huntPathPrefix(PiuInstanceId),
    SlotNumber = get_slot_number(PiuInstanceId),
    Smn = get_smn(PiuInstanceId),
    Result = ?CELLO_PIU_RO_OK,

    inet:setopts(Socket, [{active, once}]),

    send_pri9_get_identity_cfm({ItcPort, Spid},
                               {PiuInstanceId, PiuOrDeviceId, SubrackNumber,
                                Smn, SlotNumber, Apn, DeviceId, HuntPathPrefix,
                                Result, ClientId}).

'CelloPiu4_restartOwnPiu'({Socket, RestartRank, RestartEscalation,
                           RestartCause, _ClientId}, _State) ->
    inet:setopts(Socket, [{active, once}]),

    appmI:restart_own_piu(get_rank(RestartRank),
			  RestartEscalation =/= 0,
			  binary_to_list(RestartCause)),
    ok.

'CelloPiu4_restartOtherPiu'({Socket, PiuInstanceId, RestartRank,
			     RestartEscalation, RestartCause, ClientId},
			    State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    inet:setopts(Socket, [{active, once}]),

    appmI:restart_piu(PiuInstanceId,   % MpId
		      get_rank(RestartRank),
		      RestartEscalation =/= 0,
		      binary_to_list(RestartCause)),
    send_piu4_restart_piu_cfm({ItcPort, Spid}, {PiuInstanceId, ClientId}).

'CelloPri8_getPiuOrDeviceIdentity'({Socket, PiuOrDeviceId, ClientId}, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    PiuInstanceId = PiuOrDeviceId,
    Apn = get_apn(PiuInstanceId),
    DeviceId = get_deviceId(PiuInstanceId),
    HuntPathPrefix = get_huntPathPrefix(PiuInstanceId),
    SlotNumber = get_slot_number(PiuInstanceId),
    Smn = get_smn(PiuInstanceId),
    Result = ?CELLO_PIU_RO_OK,

    inet:setopts(Socket, [{active, once}]),

    send_pri8_get_identity_cfm({ItcPort, Spid},
                               {PiuInstanceId, PiuOrDeviceId,
                                Smn, SlotNumber, Apn, DeviceId, HuntPathPrefix,
                                Result, ClientId}).

'CelloPri9_getPiuOrDeviceIdentity'({Socket, PiuOrDeviceId, ClientId}, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),

    PiuInstanceId = PiuOrDeviceId,
    SubrackNumber = get_subrack_number(PiuInstanceId),
    Smn = get_smn(PiuInstanceId),
    SlotNumber = get_slot_number(PiuInstanceId),
    Apn = get_apn(PiuInstanceId),
    DeviceId = get_deviceId(PiuInstanceId),
    HuntPathPrefix = get_huntPathPrefix(PiuInstanceId),
    Result = ?CELLO_PIU_RO_OK,

    inet:setopts(Socket, [{active, once}]),

    send_pri9_get_identity_cfm({ItcPort, Spid},
                               {PiuInstanceId, PiuOrDeviceId, SubrackNumber,
                                Smn, SlotNumber, Apn, DeviceId, HuntPathPrefix,
                                Result, ClientId}).

'CelloPiu3_getLinkHandlerName'({Socket, PiuInstanceId, ClientId}, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),
    Result = ?CELLO_PIU_RO_OK,
    LinkHandlerName = get_link_handler_name(PiuInstanceId),

    inet:setopts(Socket, [{active, once}]),

    send_piu3_get_lh_name_cfm({ItcPort, Spid},
			      {Result, LinkHandlerName, ClientId}).

send_pri_initiate_service_cfm({ItcPort, Spid}, {SignalRevision, SelectedPV}) ->
    SigNo = ?CELLO_PRI_INITIATE_SERVICE_CFM,
    Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
             SelectedPV:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

%% send_pri_initiate_service_rej({ItcPort, Spid}, {SignalRevision, HighestPV}) ->
%%     SigNo = ?CELLO_PRI_INITIATE_SERVICE_REJ,
%%     Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
%%              HighestPV:4/native-unsigned-integer-unit:8>>,
%%     itc_send(ItcPort, Spid, SigNo, Data).

send_pri_server_up_ind({ItcPort, Spid}) ->
    SigNo = ?CELLO_PRI_SERVER_UP_IND,
    Data = <<>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_pri_server_down_ind({ItcPort, Spid}) ->
    SigNo = ?CELLO_PRI_SERVER_DOWN_IND,
    Data = <<>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_pri_terminate_service_cfm({ItcPort, Spid}) ->
    SigNo = ?CELLO_PRI_TERMINATE_SERVICE_CFM,
    Data = <<>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_piu3_get_hunt_path_cfm({ItcPort, Spid}, {HuntPath, Result, ClientId}) ->
    SigNo = ?CELLO_PIU3_GET_HUNT_PATH_CFM,
    Data = [to_c_string(HuntPath, ?CELLO_PIU_HUNT_PATH_SIZE),
            <<Result:4/native-unsigned-integer-unit:8,
              ClientId:4/native-unsigned-integer-unit:8>>],
    itc_send(ItcPort, Spid, SigNo, Data).

send_piu10_operational_pid_cfm({ItcPort, Spid},
                               {PiuInstanceId,
                                ProductNumber, ProductRevision, ProductName,
                                ProductDate, SerialNumber,
                                Result, ClientId}) ->
    SigNo = ?CELLO_PIU10_OPERATIONAL_PID_CFM,
    Data = [<<PiuInstanceId:4/native-unsigned-integer-unit:8>>,
            to_c_string(ProductNumber, ?CELLO_MAX_PRODUCT_NUMBER_LEN),
            to_c_string(ProductRevision, ?CELLO_MAX_PRODUCT_REVISION_LEN),
            to_c_string(ProductName, ?CELLO_MAX_PRODUCT_NAME_LEN),
            to_c_string(pri_date(ProductDate), ?CELLO_MAX_PRODUCT_DATE_LEN),
            to_c_string(SerialNumber, ?CELLO_MAX_SERIAL_NUMBER_LEN),
            <<Result:4/native-unsigned-integer-unit:8,
              ClientId:4/native-unsigned-integer-unit:8>>],
    itc_send(ItcPort, Spid, SigNo, Data).

%% This function handles both old date format YYYYMMDD and new format YYY-MM-DDThh:mm:ss.SSS
%% and converts it to PRI format YYYYMMDD
pri_date(ProductDate) ->
    case string:tokens(ProductDate,"T") of
	[Date1,_] ->
	    case string:tokens(Date1,"-") of
		[Yr,Mo,Dt] ->
		    Yr++Mo++Dt;
		X ->
		    sysInitI:error_msg("~p: wrong date format in HW ~p~n",
				       [?MODULE,X]),
		    X
	    end;
	Date2 ->
	    Date2
    end.



send_piu3_get_own_id_cfm({ItcPort, Spid}, {PiuInstanceId, Result}) ->
    SigNo = ?CELLO_PIU3_GET_OWN_ID_CFM,
    Data = <<PiuInstanceId:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_pri9_get_identity_cfm({ItcPort, Spid},
                           {PiuInstanceId, PiuOrDeviceId, SubrackNumber,
                            Smn, SlotNumber, Apn, DeviceId, HuntPathPrefix,
                            Result, ClientId}) ->
    SigNo = ?CELLO_PRI9_GET_IDENTITY_CFM,
    Data = [<<PiuInstanceId:4/native-unsigned-integer-unit:8,
              PiuOrDeviceId:4/native-unsigned-integer-unit:8,
              SubrackNumber:4/native-unsigned-integer-unit:8,
              Smn:4/native-unsigned-integer-unit:8,
              SlotNumber:4/native-unsigned-integer-unit:8,
              Apn:4/native-unsigned-integer-unit:8,
              DeviceId:4/native-unsigned-integer-unit:8>>,
            to_c_string(HuntPathPrefix, ?CELLO_PIU_HUNT_PATH_SIZE),
            <<Result:4/native-unsigned-integer-unit:8,
              ClientId:4/native-unsigned-integer-unit:8>>],
    itc_send(ItcPort, Spid, SigNo, Data).

send_pri8_get_identity_cfm({ItcPort, Spid},
                           {PiuInstanceId, PiuOrDeviceId,
                            Smn, SlotNumber, Apn, DeviceId, HuntPathPrefix,
                            Result, ClientId}) ->
    SigNo = ?CELLO_PRI8_GET_IDENTITY_CFM,
    Data = [<<PiuInstanceId:4/native-unsigned-integer-unit:8,
              PiuOrDeviceId:4/native-unsigned-integer-unit:8,
              Smn:4/native-unsigned-integer-unit:8,
              SlotNumber:4/native-unsigned-integer-unit:8,
              Apn:4/native-unsigned-integer-unit:8,
              DeviceId:4/native-unsigned-integer-unit:8>>,
            to_c_string(HuntPathPrefix, ?CELLO_PIU_HUNT_PATH_SIZE),
            <<Result:4/native-unsigned-integer-unit:8,
              ClientId:4/native-unsigned-integer-unit:8>>],
    itc_send(ItcPort, Spid, SigNo, Data).

send_piu4_restart_piu_cfm({ItcPort, Spid}, {PiuInstanceId, ClientId}) ->
    SigNo = ?CELLO_PIU4_RESTART_PIU_CFM,
    Data = <<PiuInstanceId:4/native-unsigned-integer-unit:8,
             ClientId:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

%% send_piu4_restart_piu_rej({ItcPort, Spid},
%%                           {Result, PiuInstanceId, ClientId}) ->
%%     SigNo = ?CELLO_PIU4_RESTART_PIU_REJ,
%%     Data = <<Result:4/native-unsigned-integer-unit:8,
%%              PiuInstanceId:4/native-unsigned-integer-unit:8,
%%              ClientId:4/native-unsigned-integer-unit:8>>,
%%     itc_send(ItcPort, Spid, SigNo, Data).

send_piu3_get_lh_name_cfm({ItcPort, Spid},
			  {Result, LinkHandlerName, ClientId}) ->
    SigNo = ?CELLO_PIU3_GET_LH_NAME_CFM,
    Data = [<<Result:4/native-unsigned-integer-unit:8>>,
	    to_c_string(LinkHandlerName, ?CELLO_PIU_LH_NAME_SIZE),
	    <<ClientId:4/native-unsigned-integer-unit:8>>],
    itc_send(ItcPort, Spid, SigNo, Data).

to_c_string(Text, Length) when is_list(Text) ->
    to_c_string(list_to_binary(Text), Length);
to_c_string(Text, Length) ->
    case byte_size(Text) of
        TextLength when TextLength >= Length ->
            [binary:part(Text, 0, Length-1), 0];
        TextLength ->
            [Text, binary:copy(<<0>>, Length-TextLength)]
    end.


get_str(S) when size(S) > 0 ->
    [R|_]=binary:split(S,<<0>>),
    binary_to_list(R);
get_str(_S) ->
    "".

get_rank(?CELLO_PRI_RESTART_WARM) ->
    warm;
get_rank(?CELLO_PRI_RESTART_REFRESH) ->
    warm;
get_rank(?CELLO_PRI_RESTART_COLD) ->
    cold;
get_rank(?CELLO_PRI_RESTART_COLD_WITH_TEST) ->
    cold_with_test;
get_rank(_) ->
    %% Do a warm restart if RestartRank is invalid since it is not
    %% possible to reject such request according to PRI API
    warm.

get_rank_str(?CELLO_PRI_RESTART_WARM) -> "WARM";
get_rank_str(?CELLO_PRI_RESTART_REFRESH) -> "REFRESH";
get_rank_str(?CELLO_PRI_RESTART_COLD) -> "COLD";
get_rank_str(?CELLO_PRI_RESTART_COLD_WITH_TEST) -> "COLD_WITH_TEST";
get_rank_str(I) -> "Unknown:" ++ integer_to_list(I).


itc_open(Spid) ->
    Name = "PRI" ++ integer_to_list(Spid),

    itc:open(Name).

itc_send(ItcPort, Spid, SigNo, Data) ->
    itc:send(ItcPort, Spid, SigNo, iolist_to_binary(Data)).

itc_close(undefined) ->
    ok;
itc_close(ItcPort) ->
    itc:close(ItcPort).

data_foreach(Fun, Data) ->
    Iter = gb_trees:iterator(Data),
    data_foreach1(Fun, Iter).

data_foreach1(Fun, Iter) ->
    case gb_trees:next(Iter) of
	none ->
	    ok;
	{Key, Val, Iter2} ->
	    Fun({Key, Val}),
	    data_foreach1(Fun, Iter2)
    end.

get_apn(_PiuInstanceId) ->
    0.

get_slot_number(_PiuInstanceId) ->
    0.

get_subrack_number(_PiuInstanceId) ->
    0.

get_smn(_PiuInstanceId) ->
    0.


get_product_information_data() ->
    {Arch, _} = sysEnv:architecture(),
    case {Arch,sysEnv:vrcs()} of
	{Arch,	true} ->
	    get_product_information_data(Arch);
	{"x86_64",false} ->
	    get_product_information_data("rcssim");
	{"i686",false} ->
	    get_product_information_data("rcssim");
	{Arch,	false} ->
	    get_product_information_data(Arch)
    end.


get_product_information_data("arm") ->
    AppName = "prodinfo",
    PrivDir = code:priv_dir(eqs),
    BinDir = sysEnv:target_bin_dir(),
    Prog = filename:join([PrivDir, BinDir, AppName]),
    ExtProg = swmI:find_file(Prog),
    case string:tokens(os:cmd(ExtProg ++ " -short"),"\n") of
	[ProdName, ProdNo, ProdRev, ProdDate, SerNo] ->
	    [{productNumber,   normalize_pid(ProdNo, productNumber)},
	     {productRevision, normalize_pid(ProdRev, productRevision)},
	     {productName,     normalize_pid(ProdName, productName)},
	     {marketName,      ""},
	     {productDate,     normalize_pid(ProdDate, productDate)},
	     {serialNumber,    normalize_pid(SerNo, serialNumber)}];
	[ProdName, MarketName,ProdNo, ProdRev, ProdDate, SerNo] ->
	    [{productNumber,   normalize_pid(ProdNo, productNumber)},
	     {productRevision, normalize_pid(ProdRev, productRevision)},
	     {productName,     normalize_pid(ProdName, productName)},
	     {marketName,      normalize_pid(MarketName, marketName)},
	     {productDate,     normalize_pid(ProdDate, productDate)},
	     {serialNumber,    normalize_pid(SerNo, serialNumber)}];
	Other ->
	    sysInitI:error_msg("~p: Failed to read HW info, reason: ~p~n",
			       [?MODULE, Other]),
	    false
    end;
get_product_information_data("i686_32") ->
    [
     {productNumber,   "KDU VRCS"},
     {productRevision, "R6A"},
     {productName,     "vRCS"},
     {productDate,     "20160421"},
     {serialNumber,    "A123456789"}
    ];
get_product_information_data("x86_64") ->
    [
     {productNumber,   "KDU VRCS64"},
     {productRevision, "R8A"},
     {productName,     "vRCS64"},
     {productDate,     "20161207"},
     {serialNumber,    "A123456789"}
    ];
get_product_information_data(_) ->
    Type = get_type(),
    ProductRevision = "R5A",
    ProductDate = "20130101",
    SerialNumber = "C823848081",

    [
     {productNumber,   get_product_number(Type)},
     {productRevision, ProductRevision},
     {productName,     get_product_name(Type)},
     {productDate,     ProductDate},
     {serialNumber,    SerialNumber}
    ].

get_product_info(Type, ProductInformationData) ->
    case lists:keyfind(Type, 1, ProductInformationData) of
	{Type, ProductInfo} ->
	    {ok, ProductInfo};
	false ->
	    {error, ""}
    end.

normalize_pid(ProdNo, productNumber) when is_list(ProdNo) ->
    %% Remove space and all characters outside 32 to 127
    lists:filter(fun($ ) -> false;
		    (C) -> C >= 32 andalso C =< 127 end, ProdNo);
normalize_pid(Data, _) when is_list(Data) ->
    %% Remove all characters outside 32 to 127
    lists:filter(fun(C) -> C >= 32 andalso C =< 127 end, Data).

get_product_number(dus) -> "KDU137925/4";
get_product_number(tcu) -> "KDU137926/1".

get_product_name(dus) -> "DUS52";
get_product_name(tcu) -> "TCU03".

get_type() ->
    IsTcu = is_tcu(),
    if
	IsTcu ->
	    tcu;
	true ->
	    case os:getenv("BT") of
		"dus" ->
		    dus;
		"duw" ->
		    duw;
		"duw2" ->
		    duw;
		"tcu" ->
		    tcu;
		"tcu03" ->
		    tcu;
		_ ->
		    dus
	    end
    end.

is_tcu() ->
    ManagedElementType = get_managedElementType(),
    lists:prefix("TCU", ManagedElementType).

get_managedElementType() ->
    proplists:get_value(managedElementType, comsaI:get_managed_element_data()).

get_link_handler_name(PiuInstanceId) ->
    clhI:hunt_path_prefix(PiuInstanceId).

get_huntPathPrefix(PiuInstanceId) ->
    case get_link_handler_name(PiuInstanceId) of
	"" ->
	    "";
	HuntPathPrefix ->
	    HuntPathPrefix ++ "/"
    end.

get_deviceId(_PiuInstanceId) ->
    0.

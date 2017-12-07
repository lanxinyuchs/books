%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysOei.erl %
%%% @author elarrun
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R4A/R11A/12

%%% @doc =Object Event Identity (OEI) implementation==
%%% This module contains the support for the Object Event Identity
%%% implementation.

-module(sysOei).
-vsn('/main/R2A/R4A/R11A/12').
-date('2017-10-19').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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

%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2013-02-21 etxpeno     First real version
%% ----    ---------- -------  ------------------------------------------------
%% R4A/1   2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%% ----    ---------- -------  ------------------------------------------------
%% R11A/6  2017-08-25 elarrun  Added support for correlationId
%% R11A/8  2017-08-25 etxpeno  mnesia table sys_oei is persistant
%% R11A/9  2017-08-29 etxpeno  Add missing legacy code
%% R11A/10 2017-08-30 etxpeno  fix upgrade scenario
%%% ----------------------------------------------------------

%%% ----------------------------
%%% Exported interface functions
%%% ----------------------------

%% Called from sysDataInit.erl
-export([init/1]).
-export([init_data/0]).

-export([register_cec/0, cec_setup/1]).

-export([get_id/0]).
-export([get_correlation_uuid/0]).
-export([get_correlation_uuid/2]).

%%% ---------------------------
%%% Exported internal functions
%%% ---------------------------
-export([handle_tcp/1]).

%%% -------
%%% Records
%%% -------
-record(sys_oei, {key, integer}).

%%% -------
%%% Defines
%%% -------
-define(CELLO_OEI_UNSPECIFIED_EVENT_ID, 0).
-define(OEI_FIRST_EVENT_ID, 1).
-define(OEI_LAST_EVENT_ID, 4294967295).

-define(OEI_GET, 0).

-define(OEI_GET_EVENT_ID, 1).
-define(OEI_GET_CORR_ID, 2).
-define(OEI_GET_CORR_ID_2, 3).
-define(OEI_UNSPECIFIED_CORR_ID, "00000000-0000-0000-0000-000000000000").

%%% -------------------------------------
%%% Code for exported interface functions
%%% -------------------------------------

%%% ----------------------------------------------------------
%%% @doc Creates mnesia table sys_oei
%%% @end
%%% ----------------------------------------------------------
-spec init(DbNodes::[node()]) -> ok.
init(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(sys_oei,
				 [{attributes,
				   record_info(fields, sys_oei)},
				  {disc_copies, DbNodes} |
				  sysDataInit:add_clh_option(sys_oei)]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Populates the mnesia table sys_oei
%%% @end
%%% ----------------------------------------------------------
-spec init_data() -> ok.
init_data() ->
    IsUpgradeOngoing = swmI:is_upgrade_ongoing(),
    ok = init_data(IsUpgradeOngoing).

init_data(false) ->
    Rec = #sys_oei{key     = id,
		   integer = ?OEI_FIRST_EVENT_ID},
    ok = mnesia:dirty_write(Rec);
init_data(true) ->
    swmI:copy_old_table(sys_oei),
    ok.

%%% ----------------------------------------------------------
%%% @doc Registers a callback module to CEC
%%% @end
%%% ----------------------------------------------------------
-spec register_cec() -> ok.
register_cec() ->
    ok = cec:register(<<"OEI">>, ?MODULE).

%%% ----------------------------------------------------------
%%% @doc Spawns a process that will handle all OEI related TCP messages
%%% @end
%%% ----------------------------------------------------------
-spec cec_setup(Socket::inet:socket()) -> pid().
cec_setup(Socket) ->
    spawn(?MODULE, handle_tcp, [Socket]).


%%% ----------------------------------------------------------
%%% @doc Gives a unique EventId.
%%% @end
%%% ----------------------------------------------------------
-spec get_id() -> pos_integer().
get_id() ->
    F = fun() ->
		CurrentId = get_current_id(),
		NextId = get_next_id(CurrentId),
		Rec = #sys_oei{key = id, integer = NextId},
		ok = mnesia:write(Rec),
		CurrentId
	end,

    case mnesia:transaction(F) of
	{atomic, Id} ->
	    Id;
	{aborted, _Reason} ->
	    ?CELLO_OEI_UNSPECIFIED_EVENT_ID
    end.

get_current_id() ->
    case mnesia:wread({sys_oei, id}) of
	[] ->
	    ?OEI_FIRST_EVENT_ID;
	[#sys_oei{integer = Id}] ->
	    Id
    end.

get_next_id(?OEI_LAST_EVENT_ID) -> ?OEI_FIRST_EVENT_ID;
get_next_id(Id)                 -> Id+1.


%%% ----------------------------------------------------------
%%% @doc Returns a UUID that identifies an Event uniquely
%%%      across nodes, Virtual Machines and Physical Machines.
%%%      
%%%      For backwards compatibility, this UUID includes 
%%%      an old school EventId in such a way that the EventId 
%%%      can later be extracted.
%%% @end
%%% ----------------------------------------------------------
-spec get_correlation_uuid() -> binary().
get_correlation_uuid() ->
    EventId     = get_id(),
    SystemUUID  = sysEnv:get_systemUUID(),
    get_correlation_uuid(EventId, SystemUUID).

    
-spec get_correlation_uuid(pos_integer(),string()) -> binary().
get_correlation_uuid(EventId,SystemUUID) ->
    case EventId of 
        ?CELLO_OEI_UNSPECIFIED_EVENT_ID ->
            CorrId = ?OEI_UNSPECIFIED_CORR_ID;
    _ ->
        HexEventId = lists:flatten(io_lib:format("~8.16.0b",[EventId])),

        {Corr2,Corr4} = lists:split(4,HexEventId), % Corr2 (5-8), Corr4 (10-13)
        {Corr1,Rem1} = lists:split(4, SystemUUID), % Corr1 (1-4), Rem1 (5-)
        {_,Rem2} = lists:split(4, Rem1),           % _ (5-8), Rem2 (9-)
        {Corr3,Rem3} = lists:split(1, Rem2),       % Corr3 (9), Rem3 (10-)
        {_,Corr5} = lists:split(4, Rem3),          % - (10-13), Corr5 (14-)

        CorrId = lists:concat([Corr1, Corr2, Corr3, Corr4, Corr5])
    end,
    list_to_binary(CorrId).

%%% -------------------------------------
%%% Code for exported internal functions
%%% -------------------------------------

%%% ----------------------------------------------------------
%%% @doc Handles all OEI related TCP messages
%%% @end
%%% ----------------------------------------------------------
handle_tcp(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            oei_get(Socket, Bin);
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, _Reason} ->
            ok;
        _ ->
            handle_tcp(Socket)
    end.

oei_get(Socket, <<?OEI_GET:4/native-unsigned-integer-unit:8>>) ->
    Id = get_id(),
    gen_tcp:send(Socket, <<Id:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}]),
    handle_tcp(Socket);

oei_get(Socket, <<?OEI_GET_CORR_ID_2:4/native-unsigned-integer-unit:8,
		  EventId:4/native-unsigned-integer-unit:8,
		  SystemUUIDLength:4/native-unsigned-integer-unit:8,
		  SystemUUID:SystemUUIDLength/binary>>) ->
    CorrId = get_correlation_uuid(EventId,binary_to_list(SystemUUID)),
    gen_tcp:send(Socket, <<SystemUUIDLength:4/native-unsigned-integer-unit:8,
			   CorrId/binary>>),
    inet:setopts(Socket, [{active, once}]),
    handle_tcp(Socket);

oei_get(Socket, <<?OEI_GET_CORR_ID:4/native-unsigned-integer-unit:8,
		  EventId:4/native-unsigned-integer-unit:8>>) ->
    case sysEnv:get_systemUUID() of
	%% {nok, _Reason} ->
	%%     %% abort if no SYSTEM_UUID in environment??  elarrun 20170824
        %%     gen_tcp:send(Socket, <<>>);
	SystemUUID ->
            SystemUUIDLength = string:len(SystemUUID),
            CorrelationUUID = get_correlation_uuid(EventId,SystemUUID),
            gen_tcp:send(Socket, <<SystemUUIDLength:4/native-unsigned-integer-unit:8,
				   CorrelationUUID/binary>>)
    end,
    inet:setopts(Socket, [{active, once}]),
    handle_tcp(Socket);

oei_get(Socket, <<?OEI_GET_EVENT_ID:4/native-unsigned-integer-unit:8>>) ->
    Id = get_id(),
    gen_tcp:send(Socket, <<Id:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}]),
    handle_tcp(Socket);

oei_get(Socket, _) ->
    gen_tcp:send(Socket, <<>>),
    inet:setopts(Socket, [{active, once}]),
    handle_tcp(Socket).

%%% -------------------------------------
%%% Code for internal functions
%%% -------------------------------------

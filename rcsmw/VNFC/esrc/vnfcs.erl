%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxaldu
%%% @copyright Ericsson AB 2016
%%% @doc == VNFC backend server module ==
%%% Modules used as a worker under supervisor sup
%%%
%%% "[http://www.erlang.org/doc/design_principles/gen_server_concepts.html]"
%%%@end
%%%
%%% ----------------------------------------------------------
-module(vnfcs).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%% ----------------------------------------------------------
%%%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%%            16-09-13   etxaldu     Created
%%%            17-09-06   uabesvi     Fixed new heartbeat interface
%%%            17-09-20   etxpejn     Added info print in volumebackup
%%%            17-09-27   uabesvi     Fix for sending json error
%%%            17-11-14   etxaldu     Added is_heartbeat_rcvd()
%%%            17-11-17   etxaldu     Don't reply to hb until
%%%                                   appmServer is active. Correct
%%%                                   some HB error replies.
%%%            17-12-05   etxaldu     Change is_heartbeat_rcvd() so
%%%                                   that it reflects that the VNF
%%%                                   has received a heartbeat and
%%%                                   replied with 200 OK at least
%%%                                   once since the last vnfcs
%%%                                   process restart.
%%% ----------------------------------------------------------

-include("vnfcI.hrl").
-include("vnfcs.hrl").

%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0, stop/0]).
-export([heartbeat/3]).
-export([volumebackup/3]).
-export([restart_ind/1]).
-export([get_vnfm_server/0]).
-export([stop_heartbeat/0]).
-export([start_heartbeat/0]).
-export([hb_state/0]).
-export([is_standalone/0]).
-export([is_heartbeat_rcvd/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([queue_msg/2, vnfcs_queue/2, safe_call/2]).

%%% These functions should only be called for test/debug purposes
-export([dbg_last_hb/0, dbg_show_hb/1, dbg_reset_vnfcs/0]).
-export([dbg_status/0, dbg_show_pd/0]).

%%% ----------------------------------------------------------
%%% #2.3 IMPORTED DEFINITIONS
%%% ----------------------------------------------------------
-include_lib("kernel/include/file.hrl").

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% heartbeat macros
-define(HB_STARTING,    "STARTING").
-define(HB_OPERATIONAL, "OPERATIONAL").
-define(HB_FAILED,      "FAILED").

-define(HTTP_OK,                  200).
-define(HTTP_SERVICE_ACCEPTED,    202).
-define(HTTP_BAD_REQUEST,         400).
-define(HTTP_SERVICE_UNAVAILABLE, 503).


-define(JSON_HEAD, "Content-Type: application/json\r\n\r\n").



%%% ------------------------------------------------------------------
%%% API Function Definitions
%%% ------------------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the vnfcs process
%%%      ----> Module:init/1
%%% @end
%%% ----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% ----------------------------------------------------------
%%% @doc If the gen_server is not part of a supervision tree,
%%%      a stop function may be useful
%%%
%%% "[http://www.erlang.org/doc/design_principles/gen_server_concepts.html]"
%%% @end
%%% ----------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).


%%% ----------------------------------------------------------
%%% @doc REST query entry method
%%%      handling rest query "post" for the VNF<->VNFM heartbeat
%%% @end
%%% ----------------------------------------------------------
-spec heartbeat(SessionID::pid(),EnvList::list({any(),any()}),Input::string()) ->
    ok.
%%% call back rest query 'POST'
heartbeat(SessionID, Env, Body) ->
    {ok, Method}=get_method(Env),
    case Method of
        "POST" ->
            gen_server:call(?SERVER,{post, {SessionID, Env, Body}});
        _ ->
            info_msg("Heartbeat received with unsupported method~n"),
            gen_server:call(?SERVER,{hb_method_err, {SessionID}})
    end.

%%% ----------------------------------------------------------
%%% @doc REST query entry method
%%%      handling rest query "post" for the VNF<->VNFM volume-backup.
%%% @end
%%% ----------------------------------------------------------
-spec volumebackup(SessionID::pid(),EnvList::list({any(),any()}),Input::string()) ->
    ok.
%%% call back rest query 'POST'
volumebackup(SessionID, Env, Body) ->
    {ok, Method}=get_method(Env),
    case Method of
        "POST" ->
            Result = os:cmd("/etc/vrcs/backup_volume.py '"++Body++"'"),
            JKey = encode({{json, "result"}}),
            JValue = encode({{json, Result}}),
            JsonBin = encode([{JKey, JValue}]),
            Data = ["Content-Type: application/json\r\n\r\n", JsonBin],
            info_msg("Volumebackup received and answered with result: ~p~n",
                     [Result]),
            mod_esi:deliver(SessionID, Data);
        _ ->
            info_msg("Volumebackup received with unsupported method~n"),
            Data = ["Status: 405 Method Not Allowed\r\n\r\n"],
            mod_esi:deliver(SessionID, Data)
    end.

%%% ----------------------------------------------------------
%%% @doc restart_ind(RestartType)
%%% Store the last vnf restart type in the vnfcs process dictionary.
%%% The next heartbeat reply will have the restart type in JSON format
%%% in the heartbeat body
%%% Input: RestartType::atom() - ?RESTART_USR|?RESTART_APP|?RESTART_OSMW
%%% Output: -
%%% @end
%%% ----------------------------------------------------------
restart_ind(RestartType) ->
    gen_server:call(?SERVER, {restart_ind, RestartType}).


%%% ----------------------------------------------------------
%%% @doc get_vnfm_server()
%%% Get the vnfm server data and VnfId
%%% Input: -
%%% Output: {ok, {IpAddress, Port, VnfId} | {error, Reason}
%%%      IpAddress: string()  e.g. "147.214.13.196"
%%%      Port: integer()
%%%      VnfId: string() e.g. "92ba5b8e-8a74-11e6-ad11-fa163e13b2f0"
%%% @end
%%% ----------------------------------------------------------
get_vnfm_server() ->
    gen_server:call(?SERVER, get_vnfm_server).


%%% ----------------------------------------------------------
%%% @doc stop_heartbeat()
%%% Stop replying to the VNFM heartbeats.
%%% Called when the heal request fails, heartbeat replies
%%% resume after the next restart.
%%% Input: -
%%% Output: -
%%% @end
%%% ----------------------------------------------------------
stop_heartbeat() ->
    gen_server:call(?SERVER, stop_heartbeat).


%%% ----------------------------------------------------------
%%% @doc start_heartbeat()
%%% Start replying to the VNFM heartbeats.
%%% Called when ok to reply to heartbeats (e.g. database ready)
%%% Input: -
%%% Output: -
%%% @end
%%% ----------------------------------------------------------
start_heartbeat() ->
    gen_server:call(?SERVER, start_heartbeat).


%%% ----------------------------------------------------------
%%% @doc hb_state()
%%% Get the status of the heartbeat.
%%% Input: -
%%% Output: {ok, State, LastHb}
%%%    State  : {disabled, integer()} | enabled
%%%    LastHb : {last_hb, integer()}  | not_received
%%% @end
%%% ----------------------------------------------------------
hb_state() ->
    gen_server:call(?SERVER, hb_state).


%%% ----------------------------------------------------------
%%% @doc is_standalone()
%%% Check in the VNF is standalone (for legacy tests).
%%% (no VNF_ID env is present for standalone)
%%% Input:
%%% Output: true|false
%%% @end
%%% ----------------------------------------------------------
is_standalone() ->
    case os:getenv("VNF_ID") of
        false ->
            true;
        _ ->
            false
    end.


%%% ----------------------------------------------------------
%% @doc Check if the VNF has at anytime received a heartbeat and
%%      replied with 200 OK since the last vnfcs process restart.
%% @end
%%% ----------------------------------------------------------
-spec is_heartbeat_rcvd() ->
    boolean().
is_heartbeat_rcvd() ->
    case whereis(vnfcs) of
        Pid1 when is_pid(Pid1) ->
            gen_server:call(?SERVER, is_hb_rcvd);
        _ ->
            false
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ------------------------------------------------------------------
%%% gen_server Function Definitions
%%% ------------------------------------------------------------------
%%% @doc init method call by
%%%      start_link/0
%%% @end
init(_Args) ->
    get_my_vnf_id(),
    case mnesia:dirty_read(vnfcs, vnfm_data) of
        [#vnfcs{value = VnfmData}] ->
            Ip = VnfmData#vnfm_data.vnfm_http_ip,
            Port = VnfmData#vnfm_data.vnfm_http_port,
            Id = VnfmData#vnfm_data.vnf_instance_id,
            info_msg("VNFM server data~n"
                     "    Ip:       ~p~n"
                     "    Port:     ~p~n"
                     "    VnfId:    ~p~n",
                     [Ip, Port, Id]),
            put(vnfm_server, VnfmData);
        _ ->
            ok
    end,

    %% Only stop heartbeat at system restart, not local process restart
    case {is_vnf(), mnesia:dirty_read(vnfc, system_restart)} of
        {_, []} ->
            %% Node restart, stop heart beat and wait for appm
            put(wait_for_appm, true),
            put(stop_heartbeat, erlang:system_time()),
            mnesia:dirty_write(#vnfc{key = system_restart,
                                     value = erlang:system_time()});
        {false, _} ->
            %% Process restart, a VNFM, stop heart beat, don't wait for appm
            put(stop_heartbeat, erlang:system_time()),
            put(wait_for_appm, false);
        {true, _}->
            %% Process restart, don't stop heartbeats, don't wait for appm
            put(wait_for_appm, false)
    end,
    %% Default the aic_state to "STARTING"
    put(aic_state, ensure_bin(?HB_STARTING)),
    info_msg("Starting vnfcs ~n"),
    {ok, []}.


%%% @doc Synchronous Call  activated by
%%%      gen_server:call(gen_server:multi_call)
%%%      ---> Module:handle_call/3
%%%      <br/>return rest query response
%%% @end
handle_call({post, _Post = {SessionID, Env, Body}}, _From, State) ->
    HbStop = get(stop_heartbeat),
    Wait4Appm = get(wait_for_appm),
    show_hb(get(dbg_show_hb), Env, Body, HbStop, Wait4Appm),
    log_heart_beat(),
    send_hb_reply(SessionID, Body, HbStop, get(aic_state), Wait4Appm),
    {reply, ok, State};

handle_call({hb_method_err, {SessionID}}, _From, State) ->
    log_heart_beat(),
    Data = ["Status: 405 Method Not Allowed\r\n\r\n"],
    log_heart_beat_reply("405 Method Not Allowed"),
    mod_esi:deliver(SessionID, Data),
    {reply, ok, State};

handle_call({restart_ind, undefined}, _From, State) ->
    handle_call({restart_ind, recovery}, _From, State);

handle_call({restart_ind, RestartType}, _From, State) ->
    restart_ind(RestartType, mnesia:dirty_read(vnfcs, vnfm_data)),
    {reply, ok, State};

handle_call(get_vnfm_server, _From, State) ->
    Result = case get(vnfm_server) of
                 undefined ->
                     {error, not_found};
                 VnfmData ->
                     Ip = VnfmData#vnfm_data.vnfm_http_ip,
                     Port = VnfmData#vnfm_data.vnfm_http_port,
                     Id = VnfmData#vnfm_data.vnf_instance_id,
                     {ok, {Ip, Port, Id}}
                 end,
    {reply, Result, State};

handle_call(stop_heartbeat, _From, State) ->
    put(stop_heartbeat, erlang:system_time()),
    {reply, ok, State};

handle_call(start_heartbeat, _From, State) ->
    erase(stop_heartbeat),
    info_msg("start_heartbeat~n"),
    {reply, ok, State};

handle_call(hb_state, _From, State) ->
    SysTime = erlang:system_time(),
    HbState = case get(stop_heartbeat) of
                  undefined ->
                      enabled;
                  StateTs ->
                      {disabled,
                       erlang:convert_time_unit(SysTime-StateTs, native, second)}
              end,
    LastHb = case get(last_hb) of
                 undefined ->
                     not_received;
                 HbTs ->
                     {last_hb,
                      erlang:convert_time_unit(SysTime-HbTs, native, second)}
             end,
    {reply, {ok, HbState, LastHb}, State};

handle_call(is_hb_rcvd, _From, State) ->
    Res = case get(last_hb_reply_ok) of
              undefined ->
                  false;
              _ ->
                  true
          end,
    {reply, Res, State};

handle_call(dbg_last_hb, _From, State) ->
    SysTime = erlang:system_time(),
    case get(last_hb) of
        undefined ->
            info_msg("dbg_last_hb: No heartbeat received~n");
        LastHb ->
            Secs = erlang:convert_time_unit(SysTime-LastHb, native, second),
            info_msg("dbg_last_hb~n"
                     "Last heartbeat received "
                     "~p seconds ago~n",[Secs])
    end,
    {reply, ok, State};

handle_call({dbg_show_hb, NumHb}, _From, State) when is_integer(NumHb) ->
    info_msg("dbg_last_hb: Show next ~p heartbeats~n",[NumHb]),
    put(dbg_show_hb, NumHb),
    {reply, ok, State};

handle_call(dbg_reset_vnfcs, _From, State) ->
    info_msg("dbg_reset_vnfcs~n"),
    erase(stop_heartbeat),
    erase(vnfm_server),
    erase(last_hb),
    erase(last_hb_reply),
    erase(last_hb_reply_ok),
    erase(last_hb_reply_error),
    erase(vnf_id),
    get_my_vnf_id(),
    {reply, ok, State};

handle_call(dbg_show_pd, _From, State) ->
    info_msg("~p~n", [process_info(self(), dictionary)]),
    {reply, ok, State};

handle_call(dbg_status, _From, State) ->
    SysTime = erlang:system_time(),
    {F1, A1} = {"dbg_status~n",[]},

    {F2, A2} = case get(last_hb) of
                   undefined ->
                       {"  HeartBeat        - No heartbeat received~n", []};
                   LastHb ->
                       Interval = erlang:convert_time_unit(SysTime-LastHb,
                                                           native, second),
                       {"  HeartBeat        - Last received ~p seconds ago~n",
                        [Interval]}
               end,

    {F3, A3} = case get(stop_heartbeat) of
                   undefined ->
                       {"  HeartBeat reply  - Service Available~n", []};
                   HbStop ->
                       Secs = erlang:convert_time_unit(SysTime-HbStop,
                                                       native, second),
                       {"  HeartBeat reply  - Service Unavailable set ~p seconds ago~n",
                        [Secs]}
               end,

    {F4, A4} = case get(last_hb_reply) of
                   undefined ->
                       {"                     No heartbeat reply sent~n", []};
                   {LastReply, OldReply} ->
                       {LastHbReply, Resp} = get(LastReply),
                       ReplyInt = erlang:convert_time_unit(SysTime-LastHbReply,
                                                           native, second),
                       F4x = "                     Last Reply sent ~p seconds"
                           " ago. Response ~p~n",
                       A4x = [ReplyInt, Resp],
                       case get(OldReply) of
                           undefined ->
                               {F4x, A4x};
                           {OldHbReply, OldResp} ->
                               OldReplyT =
                                   erlang:convert_time_unit(SysTime-OldHbReply,
                                                            native, second),
                               {F4x ++ "                     Last ~p sent ~p"
                                " seconds ago~n",
                                A4x ++ [OldResp, OldReplyT]}
                       end;
                   last_hb_reply_ok ->
                       {LastHbReply, "200 OK"} = get(last_hb_reply_ok),
                       ReplyInt = erlang:convert_time_unit(SysTime-LastHbReply,
                                                           native, second),
                       F4A = "                     Last Reply sent ~p seconds"
                           " ago. Response ~p~n",
                       A4A = [ReplyInt, "200 OK"],
                       case get(last_hb_reply_error) of
                           undefined ->
                               {F4A, A4A};
                           {LastHbReErr, StatusErr} ->
                               ReplyErr =
                                   erlang:convert_time_unit(SysTime-LastHbReErr,
                                                            native, second),
                               {F4A ++ "                     Last error (~p)"
                                " sent ~p seconds ago~n",
                                A4A ++ [StatusErr, ReplyErr]}
                       end;
                   _last_hb_reply_error ->
                       {LastHbReply, ErrResp} = get(last_hb_reply_error),
                       ReplyInt = erlang:convert_time_unit(SysTime-LastHbReply,
                                                           native, second),
                       F4B = "                     Last Reply sent ~p seconds"
                           " ago. Response ~p~n",
                       A4B = [ReplyInt, ErrResp],
                       case get(last_hb_reply_ok) of
                           undefined ->
                               {F4B, A4B};
                           {LastHbReOk, StatusOk} ->
                               ReplyOk =
                                   erlang:convert_time_unit(SysTime-LastHbReOk,
                                                            native, second),
                               {F4B ++ "                     Last ~p sent ~p"
                                " seconds ago~n",
                                A4B ++ [StatusOk, ReplyOk]}
                       end
               end,

    {F5, A5} = case get(vnfm_server) of
                   undefined ->
                       {"  VNFM Server      - No data received~n", []};
                   VnfmData ->
                       {"  VNFM Server IP   - ~p~n"
                        "  VNFM Server Port - ~p~n"
                        "  VNFId(HeartBeat) - ~p~n",
                        [VnfmData#vnfm_data.vnfm_http_ip,
                         VnfmData#vnfm_data.vnfm_http_port,
                         VnfmData#vnfm_data.vnf_instance_id]}
               end,

    {F6, A6} = case get(vnf_id) of
                   undefined ->
                       {"  VnfId(config)    - VnfId not configured~n", []};
                   VnfId ->
                       {"  VnfId(config)    - ~p~n", [VnfId]}
               end,

    {F7, A7} = {"  Restart Ind      - ~p~n", [read_restart_type()]},

    {F8, A8} = {"  AIC Status       - ~p~n", [get(aic_state)]},

    {F9, A9} = case get(wait_for_appm) of
                   true ->
                       {"  APPM             - Waiting~n", []};
                   _ ->
                       {"  APPM             - Ready~n", []}
               end,

    Format = F1++F2++F3++F4++F5++F6++F7++F8++F9,
    Args   = A1++A2++A3++A4++A5++A6++A7++A8++A9,
    info_msg(Format, Args),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.


%%% @doc Asynchronous requests -Cast  activated by
%%%      gen_server:cast
%%%      ---> Module:handle_cast/2
%%%      <br/>not expected any return
%%% @end
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%%% @doc Handling other messages
%%%
%%% gen_server should be able to receive other messages than
%%% requests when needed
%%% @end
handle_info(_Info, State) ->
    {noreply, State}.


%%% @doc If the gen_server is part of a supervision tree, it will
%%%      automaticaly be terminated by its supervisor
%%%
%%% "[http://www.erlang.org/doc/design_principles/gen_server_concepts.html]"
%%% @end
terminate(_Reason, _State) ->
    ok.


%%% @doc Handling other messages when needed
%%%
%%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% ----------------------------------------------------------
%%% #       dbg_last_hb()
%%% Input: -
%%% Output: -
%%% Exceptions:
%%% Description: Debug Function: Show when the last heartbeat
%%%              was received
%%% ----------------------------------------------------------
dbg_last_hb() ->
    gen_server:call(?SERVER, dbg_last_hb).


%%% ----------------------------------------------------------
%%% #       dbg_show_hb(NumHb)
%%% Input: NumHb : integer()
%%% Output:
%%% Exceptions:
%%% Description: Debug function to log next NumHb heart beats
%%% ----------------------------------------------------------
dbg_show_hb(NumHb) ->
    gen_server:call(?SERVER, {dbg_show_hb, NumHb}).

%%% ----------------------------------------------------------
%%% #       dbg_reset_vnfcs()
%%% Input: NumHb : integer()
%%% Output:
%%% Exceptions:
%%% Description: Debug function reset the vnfcs process.
%%% ----------------------------------------------------------
dbg_reset_vnfcs() ->
    gen_server:call(?SERVER, dbg_reset_vnfcs).

%%% ----------------------------------------------------------
%%% #       dbg_show_pd()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Debug function to show the process dict
%%% ----------------------------------------------------------
dbg_show_pd() ->
    gen_server:call(?SERVER, dbg_show_pd).


%%% ----------------------------------------------------------
%%% #       dbg_status()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Debug function show the vnfcs status
%%% ----------------------------------------------------------
dbg_status() ->
    gen_server:call(?SERVER, dbg_status).


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% used to find 'GET' 'PUT' ... method from the query
get_method(Arg)->
    Method = proplists:get_value(request_method,Arg,[]),
    {ok, Method}.



%%% ----------------------------------------------------------
%%% #       put_vnfm_data(VnfmData)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Store VNFM data in mnesia and the process
%%%              dictionary.
%%% ----------------------------------------------------------
put_vnfm_data(VnfmData) ->
    OldVnfmData = put(vnfm_server, VnfmData),
    case {OldVnfmData, VnfmData} of
        {VnfmData, VnfmData} ->
            ok;
        _ ->
            mnesia:dirty_write(#vnfcs{key = vnfm_data,
                                      value = VnfmData}),
            Ip = VnfmData#vnfm_data.vnfm_http_ip,
            Port = VnfmData#vnfm_data.vnfm_http_port,
            Id = VnfmData#vnfm_data.vnf_instance_id,
            info_msg("New VNFM server data received~n"
                     "    Ip:    ~p~n"
                     "    Port:  ~p~n"
                     "    vnfId: ~p~n", [Ip, Port, Id])
    end.


%%% ----------------------------------------------------------
%%% #       info_msg(InfoStr, Args)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Wrapper for sysInitI:info_msg
%%% ----------------------------------------------------------
info_msg(InfoStr) ->
    info_msg(InfoStr, []).
info_msg(InfoStr, Args) ->
    sysInitI:info_msg("~w: "++InfoStr, [?SERVER|Args]).


%%% ----------------------------------------------------------
%%% #       decode(JsonValue)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Wrapper for the jsone decode function
%%%              See jsone.erl for api
%%% ----------------------------------------------------------
decode(Body) ->
    case catch jsone:decode(list_to_binary(Body)) of
        {'EXIT', {Reason, _Error}} ->
            {error, Reason};
        JsonMap ->
            JsonMap
    end.


%%% ----------------------------------------------------------
%%% #       encode(JsonValue)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Wrapper for the jsone encode function
%%%              See jsone.erl for api
%%% ----------------------------------------------------------
encode(JsonValue) ->
    case catch jsone:encode(JsonValue) of
        {'EXIT', {Reason, _Error}} ->
            {error, Reason};
        JsonBin ->
            JsonBin
    end.


%%% ----------------------------------------------------------
%%% #       find(Key, Map)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Find a Key in the decoded Json map
%%% ----------------------------------------------------------
find(Key, Map) ->
    BinKey = encode({{json, Key}}),
    Value = case maps:is_key(BinKey, Map) of
                true -> maps:get(BinKey, Map);
                false -> undefined
            end,
    case {BinKey, Value} of
        {BinKey, Value} when is_binary(BinKey),
                             is_binary(Value)->
            binary_to_list(Value);
        {BinKey, Value} when is_binary(BinKey) ->
            Value;
        {{error, Reason}, _} ->
            {error, Reason}
    end.


%%% ----------------------------------------------------------
%%% #       log_heart_beat()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Log the time of the last heartbeat
%%% ----------------------------------------------------------
log_heart_beat() ->
    put(last_hb, erlang:system_time()),
    ok.


%%% ----------------------------------------------------------
%%% #       log_heart_beat_reply()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Log the time of the last heartbeat reply
%%% ----------------------------------------------------------
log_heart_beat_reply(Response) ->
    SysTime = erlang:system_time(),
    case Response of
        "200 OK" ->
            put(last_hb_reply, {last_hb_reply_ok, last_hb_reply_error}),
            put(last_hb_reply_ok, {SysTime, Response});
        _ ->
            put(last_hb_reply, {last_hb_reply_error, last_hb_reply_ok}),
            put(last_hb_reply_error, {SysTime, Response})
    end,
    ok.


%%% ----------------------------------------------------------
%%% #       show_hb()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Log the received heartbeat
%%% ----------------------------------------------------------
show_hb(Val, Env, Body, HbStop, Wait4Appm) when is_integer(Val),
                                               Val > 0 ->
    SysTime = erlang:system_time(),
    {F1, A1} = case get(last_hb) of
                   undefined ->
                       {"Heartbeat received~n"
                        "    Env:    ~p~n"
                        "    Body:  ~p~n", [Env, Body]};
                   LastHb ->
                       Interval = erlang:convert_time_unit(SysTime-LastHb,
                                                           native, second),
                       {"Heartbeat received (Interval: ~ps)~n"
                        "    Env:    ~p~n"
                        "    Body:  ~p~n", [Interval, Env, Body]}
               end,
    {F2, A2} = case HbStop of
                   undefined ->
                       {"HeartBeat reply  - Service Available~n", []};
                   _ ->
                       Secs = erlang:convert_time_unit(SysTime-HbStop,
                                                       native, second),
                       {"HeartBeat reply  - Service Unavailable ~p seconds ago~n",
                        [Secs]}
               end,
    {F3, A3} = case Wait4Appm of
                   true ->
                       {"APPM             - Waiting~n", []};
                   _ ->
                       {"APPM             - Ready~n", []}
               end,
    Format = F1++F2++F3,
    Args = A1++A2++A3,
    info_msg(Format, Args),
    put(dbg_show_hb,  Val-1),
    ok;
show_hb(_, _, _, _,_) ->
    erase(dbg_show_hb),
    ok.


%%% ----------------------------------------------------------
%%% #           restart_ind(RestartType, VnfcsRec)
%%% Input: RestartType::string()
%%%            ?RESTART_USR_STR|?RESTART_APP_STR|?RESTART_OSMW_STR
%%%        VnfcsRec::#vnfcs{}
%%% Output: -
%%% Exceptions:
%%% Description: Store the restart type in the vnfcs process
%%%              dictionary. Will be sent in next hb reply
%%% ----------------------------------------------------------
restart_ind(?RESTART_OSMW, []) ->
    %% Don't send indication at startup
    put(wait_for_appm, false),
    ok;
restart_ind(RestartType, _) ->
    put(wait_for_appm, false),
    put(restart_ind, RestartType).


%%% ----------------------------------------------------------
%%% #           get_my_vnf_id()
%%% Input: -
%%% Output: VnfId: string() e.g. "92ba5b8e-8a74-11e6-ad11-fa163e13b2f0"
%%%         undefined
%%% Exceptions:
%%% Description: Read the VNF_ID environmental variable and store
%%%              it in the process dictionary and return the value
%%% ----------------------------------------------------------
get_my_vnf_id() ->
    case get(vnf_id) of
        undefined ->
            case os:getenv("VNF_ID") of
                false ->
                    undefined;
                OsVnfId ->
                    put(vnf_id, OsVnfId),
                    OsVnfId
            end;
        ProcVnfId ->
            ProcVnfId
    end.


%%% ----------------------------------------------------------
%%% #           queue_msg(Function, Args)
%%% Input: Function: atom()
%%%        Args: list()
%%% Output: -
%%% Exceptions:
%%% Description: The vnfcs process is starting up, queue the
%%%              messages
%%% ----------------------------------------------------------
queue_msg(Func, Args) ->
    case whereis(vnfcs_queue) of
        Pid when is_pid(Pid) ->
            Pid ! {Func, Args};
        _ ->
            Pid = start_vnfcs_queue(),
            Pid ! {Func, Args}
    end,
    ok.


%% --------------------------------------------------------------------
%% @doc
%% Start the vnfcs_queue process.<br/>
%% <br/>
%% @spec start_vnfcs_queue() -> ok
%%  Ip, Port : string()
%%  LSock :gen_tcp listerner socket : term()
%% @end
%% --------------------------------------------------------------------
start_vnfcs_queue() ->
    Pid = spawn_link(?MODULE, vnfcs_queue, [[], 10]),
    register(vnfcs_queue, Pid),
    Pid.


%% --------------------------------------------------------------------
%% @doc
%% The main loop of the vnfcs_queue process.<br/>
%% <br/>
%% @spec vnfcs_queue()
%% @end
%% --------------------------------------------------------------------
vnfcs_queue(MsgQueue, Timeout) ->
    receive
        FuncArgs ->
            vnfcs_queue(MsgQueue ++ [FuncArgs], Timeout)
    after Timeout ->
            case whereis(vnfcs) of
                Pid when is_pid(Pid) ->
                    handle_queue(MsgQueue);
                _ ->
                    NewTimeout = adjust_timeout(Timeout),
                    vnfcs_queue(MsgQueue, NewTimeout)
            end
    end.


%% --------------------------------------------------------------------
%% @doc
%% Initialize the vnfmServer web server process.<br/>
%% <br/>
%% @spec vnfcs_queue(MsgQueue)
%%  MsgQueue : string()
%% @end
%% --------------------------------------------------------------------
handle_queue([]) ->
    case proplists:get_value(message_queue_len, process_info(self())) of
        0 ->
            ok; %% Message queue empty so safe to exit
        _ ->
            vnfcs_queue([], 10)
    end;
handle_queue([{Func, Args}|MsgQueue]) ->
    apply(?MODULE, Func, Args),
    handle_queue(MsgQueue).


%% --------------------------------------------------------------------
%% @doc
%% Adjust the Timeout. Double it until max 1sec.<br/>
%% <br/>
%% @spec adjust_timeout(Timeout)
%%  Timeout : integer()
%% @end
%% --------------------------------------------------------------------
adjust_timeout(Timeout) when Timeout >= 1000 ->
    Timeout;
adjust_timeout(Timeout) ->
    Timeout*2.


%% --------------------------------------------------------------------
%% @doc
%% Check the vnfcs process is ready, if not buffer the message<br/>
%% <br/>
%% @spec safe_call(Func, Args) -> ok
%% @end
%% --------------------------------------------------------------------
safe_call(Func, Args) ->
    %% There's a chance that appmServer calls this fuction before the
    %% vnfcs process has started so we need to check before calling.
    case {whereis(vnfcs), whereis(vnfcs_queue)} of
        {Pid1, undefined} when is_pid(Pid1) ->
            apply(?MODULE, Func, Args);
        _ ->
            %% Queue the message temporarily
            queue_msg(Func, Args),
            queued
    end.


%%% ----------------------------------------------------------
%%% #       send_hb_reply(SessionID, Body, HbState, AicState, Wait4Appm)
%%% Input: SessionID, Body, HbState
%%% Output:
%%% Exceptions: Send 503 Service Unavailable is hb disabled or waiting for appm
%%% Description: Send heart beat reply 200 ok if hb enabled
%%% ----------------------------------------------------------
send_hb_reply(SessionID, Body, undefined, AicState, false) ->
    try
        InputData = decode_input(Body),
        ok = store_vnfm_server(get_my_vnf_id(), InputData, Body),
        ReplyData = check_aic_state(AicState),
        put(aic_state, proplists:get_value(status, ReplyData)),
        hb_reply_rc(SessionID, ReplyData)
    catch
        throw:{vnfcs, {_Error, ErrorData}} ->
            hb_reply_rc(SessionID, ErrorData)
    end;
send_hb_reply(SessionID, _, _, _, _) ->
    Data = ["Status: 503 Service Unavailable\r\n\r\n"],
    log_heart_beat_reply("503 Service Unavailable"),
    mod_esi:deliver(SessionID, Data).

%%-----------------------------------------------------
%% send the heartbeat reply back to VNFM
%%-----------------------------------------------------
hb_reply_rc(SessionID, Data) ->
    Encoded = jsone:encode(mapify([get_restart_type() | Data])),
    mod_esi:deliver(SessionID, [?JSON_HEAD, Encoded]).


%%% ----------------------------------------------------------
%%% #       decode_input(Body)
%%% Input:  Body
%%% Output: {Ip, Port, VnfId}
%%%         throws an exception in error cases
%%% Description:
%%% ----------------------------------------------------------
decode_input(Body) ->
    di(decode(Body), Body).

di(Map, _Body) when is_map(Map) ->
    di_get_input_data(Map);
di(Error, Body) ->
    Msg = "Failed to decode the heartbeat JSON body",
    info_msg(Msg ++ "~n"
             "    Error: ~p~n"
             "    JSON:  ~p~n", [Error, Body]),
    ErrorRep   = [{status,           ?HTTP_BAD_REQUEST},
                  {detail,           ensure_bin("Bad request")},
                  {additionalDetail, ensure_bin(Msg)}],
    Data       = [{status, ensure_bin(?HB_FAILED)},
                  {error, ErrorRep}],
    throw({vnfcs, {"400 Bad Request", Data}}).

di_get_input_data(Map) ->
    {find("vnfm_http_ip",    Map),
     find("vnfm_http_port",  Map),
     find("vnf_instance_id", Map)}.


%%% ----------------------------------------------------------
%%% #       store_vnfm_server(MyVnfId, {Ip, Port, VnfId}, Body)
%%% Input:
%%% Output: ok
%%%         throws an exception in error cases
%%% Description:
%%% ----------------------------------------------------------
store_vnfm_server(MyVnfId, {Ip, Port, MyVnfId}, _)
  when is_list(Ip),
       is_integer(Port),
       is_list(MyVnfId) ->
    {ok, IpTuple} = inet:parse_address(Ip),
    IpString      = format_ip(IpTuple),
    put_vnfm_data(#vnfm_data{vnfm_http_ip    = IpString,
                 vnfm_http_port  = Port,
                 vnf_instance_id = MyVnfId}),
    ok;
store_vnfm_server(_, {Ip, Port, Id}, _)
  when Ip   == undefined,
       Port == undefined,
       Id   == undefined ->
    ok;
store_vnfm_server(MyVnfId, {Ip, Port, Id}, Body)
  when is_list(Ip),
       is_integer(Port),
       is_list(Id) ->
    Msg = "Heartbeat received with an unexpected VnfId~n",
    info_msg(Msg ++ "~n"
             "    Ip:             ~p~n"
             "    Port:           ~p~n"
             "    Received vnfId: ~p~n"
             "    Expected vnfId: ~p~n"
             "    JSON:           ~p~n",
             [Ip, Port, Id, MyVnfId, Body]),
    Error   = [{status,           ?HTTP_BAD_REQUEST},
               {detail,           ensure_bin("Bad request")},
               {additionalDetail, ensure_bin(Msg)}],
    Data    = [{status, ensure_bin(?HB_FAILED)},
               {error, Error}],
    throw({vnfcs, {"400 Bad Request", Data}});
store_vnfm_server(_, {Ip, Port, Id}, Body) ->
    Msg = "Incomplete VNFM server data received",
    info_msg(Msg ++ "~n"
             "    Ip:    ~p~n"
             "    Port:  ~p~n"
             "    VnfId: ~p~n"
             "    JSON:  ~p~n", [Ip, Port, Id, Body]),
    Error   = [{status,           ?HTTP_BAD_REQUEST},
               {detail,           ensure_bin("Bad request")},
               {additionalDetail, ensure_bin(Msg)}],
    Data    = [{status, ensure_bin(?HB_FAILED)},
               {error, Error}],
    throw({vnfcs, {"400 Bad Request", Data}}).


format_ip(IpTuple) when tuple_size(IpTuple) == 4 -> %% IPv4
    inet:ntoa(IpTuple);
format_ip(IpTuple) when tuple_size(IpTuple) == 8 -> %% IPv6
    "[" ++ inet:ntoa(IpTuple) ++ "]". %% Use brackets to handle port


%%% ----------------------------------------------------------
%%% #       get_restart_type()
%%% Input:
%%% Output: JSON encoded RestartType
%%%
%%% Description:
%%% ----------------------------------------------------------
get_restart_type()->
    grt(get_restart_type(erase(restart_ind))).

grt(undefined) ->
    [];
grt(Type) ->
    {restart_type, ensure_bin(Type)}.

get_restart_type(?RESTART_USR)  -> ?RESTART_USR_STR;
get_restart_type(?RESTART_APP)  -> ?RESTART_APP_STR;
get_restart_type(?RESTART_OSMW) -> ?RESTART_OSMW_STR;
get_restart_type(_)             -> undefined.


%%% ----------------------------------------------------------
%%% #       check_aic_state()
%%% Input:
%%% Output: {AicState, AicDetail, AicError}
%%%
%%% Description:
%%% ----------------------------------------------------------
check_aic_state(<<"OPERATIONAL">> = AicState) ->
    log_heart_beat_reply("200 OK"),
    [{status, AicState}];
check_aic_state(_) ->
    {LogMsg, Data} = shbr_caic(aicI:heartbeat()),
    log_heart_beat_reply(LogMsg),
    Data.

shbr_caic({Status, undefined, _ErrorResponse})
  when Status == ?HB_STARTING ->
    {"200 OK", [{status, ensure_bin(Status)}]};
shbr_caic({Status, Detail, _ErrorResponse})
  when Status == ?HB_STARTING ->
    DetailLen = string:sub_string(Detail, 1, ?HB_DETAIL_LEN),
    {"200 OK", [{status, ensure_bin(Status)},
                {detail, ensure_bin(DetailLen)}]};
shbr_caic({Status, _Detail, _ErrorResponse})
  when Status == ?HB_OPERATIONAL ->
    {"200 OK", [{status, ensure_bin(Status)}]};
shbr_caic({Status, _Detail, {HttpCode, ErrorDetail, AddDetail}})
  when Status == ?HB_FAILED ->
    DetailLen    = string:sub_string(ErrorDetail, 1, ?HB_ERROR_DETAIL_LEN),
    AddDetailLen = string:sub_string(AddDetail,   1, ?HB_ERROR_ADD_DETAIL_LEN),
    Error   = [{status,           HttpCode},
               {detail,           ensure_bin(DetailLen)},
               {additionalDetail, ensure_bin(AddDetailLen)}],
    {"503 Service Unavailable", [{status, ensure_bin(Status)},
                                 {error, Error}]}.


%%% ----------------------------------------------------------
%%% #       mapify(List)
%%% Input:
%%% Output: #map{}
%%%
%%% Description:
%%% Format data as a map to be able to JSON encode it
%%% ----------------------------------------------------------
mapify(List) ->
    mapify(List, maps:new()).

mapify([], Map) ->
    Map;
mapify([[] | T], Map) ->
    mapify(T, Map);
mapify([{Key, Value} | T], Map)
  when is_atom(Key) andalso
       (is_binary(Value) orelse is_integer(Value)) ->
    mapify(T, maps:put(ensure_bin(Key), Value, Map));
mapify([{Key, Value} | T], Map)
  when is_atom(Key) andalso
       is_list(Value) ->
    ValueMap = mapify(Value, maps:new()),
    mapify(T, maps:put(ensure_bin(Key), ValueMap, Map)).


read_restart_type()->
    case get(restart_ind) of
        ?RESTART_USR  -> ?RESTART_USR_STR;
        ?RESTART_APP  -> ?RESTART_APP_STR;
        ?RESTART_OSMW -> ?RESTART_OSMW_STR;
        _ -> undefined
    end.


ensure_bin(Term) when is_binary(Term) ->
    Term;
ensure_bin(Term) ->
    list_to_binary(ensure_str(Term)).

ensure_str(Term) ->
    catch sysUtil:term_to_string(Term).


is_vnf()  ->
    is_vnf(swmI:node_type()).

is_vnf("vRC") -> true;
is_vnf("vPP") -> true;
is_vnf("vSD") -> true;
is_vnf(_) -> false.

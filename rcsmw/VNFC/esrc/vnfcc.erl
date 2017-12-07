%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxaldu
%%% @copyright Ericsson AB 2016-2017
%%% @doc == VNFC backend server module ==
%%% Modules used as a worker under supervisor sup
%%%
%%% "[http://www.erlang.org/doc/design_principles/gen_server_concepts.html]"
%%%@end
%%%
%%% ----------------------------------------------------------
-module(vnfcc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%% ----------------------------------------------------------
%%%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%%            16-12-08   etxaldu     Created
%%%            17-03-15   etxarnu     Added Reason to Heal request
%%%            17-04-06   edanlef     Aligned to /lcm/v0 API.
%%%            17-08-23   etxaldu     Use https in URL 
%%% ----------------------------------------------------------

-include("vnfcI.hrl").


%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0, stop/0]).
-export([send_heal/1]).
-export([send_heal/2]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([dbg_status/0]).

%%% ----------------------------------------------------------
%%% #2.3 IMPORTED DEFINITIONS
%%% ----------------------------------------------------------

%%% ------------------------------------------------------------------
%%% API Function Definitions
%%% ------------------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Starts the vnfcc process
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
%%% @doc send_heal(Initiator)
%%% Send a heal request to the VNFM.
%%% Input: Initiator - atom() (calling module)
%%% Output: ok | {error, Reason}
%%% @end
%%% ----------------------------------------------------------
send_heal(Initiator) ->
    gen_server:call(?SERVER,{send_heal, Initiator,"No reason"}, infinity).
%%% ----------------------------------------------------------
%%% @doc send_heal(Initiator,Reason)
%%% Send a heal request to the VNFM.
%%% Input: Initiator - atom() (calling module)
%%%        Reason - string() (Reason for heal)
%%% Output: ok | {error, Reason}
%%% @end
%%% ----------------------------------------------------------
send_heal(Initiator,Reason) ->
    gen_server:call(?SERVER,{send_heal, Initiator,Reason}, infinity).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ------------------------------------------------------------------
%%% gen_server Function Definitions
%%% ------------------------------------------------------------------
%%% @doc init method call by
%%%      start_link/0
%%% @end
init(Args) ->
    {ok, Args}.


%%% @doc Synchronous Call  activated by
%%%      gen_server:call(gen_server:multi_call)
%%%      ---> Module:handle_call/3
%%%      <br/>return rest query response
%%% @end
handle_call({send_heal, Healer, Reason}, _From, State) ->
    Result = case {vnfcs:hb_state(), get(heal_state)} of
                 {{ok, enabled, _}, undefined} ->
                     start_heal(Healer,Reason);
                 {_, undefined} ->
                     {error, hb_reply_disabled};
                 _HealData ->
                     {error, already_started}
             end,
    {reply, Result, State};

handle_call(dbg_status, _From, State) ->
    SysTime = erlang:system_time(),
    {F1, A1} = {"dbg_status~n",[]},

    {F2, A2} = case get(heal_state) of
                   undefined ->
                       {"  Heal Request     - Not initiated~n", []};
                   {Healer, Time} ->
                       HealSecs = erlang:convert_time_unit(SysTime-Time,
                                                           native, second),
                       {"  Heal Request     - Healing initiated by ~p ~p seconds ago~n",
                        [Healer, HealSecs]}
               end,
    Format = F1++F2,
    Args = A1++A2,
    info_msg(Format, Args),
    {reply, ok, State};

handle_call(_Request, _From, _State) ->
    {reply, undefined, error}.


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
%%% ----------------------------------------------------------
%%% #           start_heal()
%%% Input: -
%%% Output: ok | {error, Reason}
%%% Exceptions:
%%% Description: Send a heal request to the VNFM
%%% ----------------------------------------------------------
start_heal(Healer,HealReason) ->
    case vnfcI:get_vnfm_server() of
        {ok, {VnfmIp, VnfmPort, VnfId}} ->
            info_msg("Sending Heal Request~n"
                     "    VnfId:     ~p~n"
                     "    VNFM Ip:   ~p~n"
                     "    VNFM Port: ~p~n"
                     "    Reason:    ~p~n",
		     [VnfId, VnfmIp, VnfmPort, HealReason]),
            VnfmPortStr = integer_to_list(VnfmPort),
            VnfmProtoStr = get_protocol_str(VnfmPort),
            Url = VnfmProtoStr ++ VnfmIp ++ ":" ++ VnfmPortStr ++ 
                "/vnflcm/v1/vnf_instances/" ++ VnfId ++ "/heal",
            put(heal_state, {Healer, erlang:system_time()}),
            JKey = encode({{json, "cause"}}),
            JValue = encode({{json, map_heal_reason_to_string(HealReason)}}),
            JsonBin = encode([{JKey, JValue}]),
            Maps = maps:put(data, {Url, [], "application/json", JsonBin }, maps:new()),
            case vnfcHttps:send_receive(Maps) of
                {ok, _} ->
                    ok;
                {error, Error} ->
                    info_msg("Unexpected http reply from VNFM. ~n "
                        "reply: ~p  ", [Error]),
                    {error, unexpected_reply}
            end;
        {error, Reason} ->
            info_msg("Unable to send Heal Request~n "
                "Reason: VNFM server ~p ", [Reason]),
            {error, Reason}
    end.


info_msg(InfoStr, Args) ->
    sysInitI:info_msg("~w: "++InfoStr, [?SERVER|Args]).


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


map_heal_reason_to_string("'Manual restart'") ->
    "Operator initiated restart";
map_heal_reason_to_string("'Manual COLI restart'") ->
    "Operator initiated restart";
map_heal_reason_to_string(_Str) ->
    "Application failure".


%%% ----------------------------------------------------------
%%% #       get_protocol_str(Port)
%%% Input:  Port - integer() 
%%% Output: "http://" | "https://"
%%% Exceptions:
%%% Description: 
%%% ----------------------------------------------------------
get_protocol_str(Port) ->
    case Port of
        4443 ->
            "https://";
        _ ->
            "http://"
    end.

%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootResolver.erl %
%%% Author:	etxlg
%%% Description: Performs name to IPv4 lookup in the relevant namespace.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ootResolver).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/R7A/R8A/R9A/R11A/R12A/3').
-date('2017-10-27').
-author('eolaand').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%% R3A/1      2015-02-04 etxlg       Created
%%% R3A/3      2015-02-09 etxlg       Prepare for new API in APPM
%%% R3A/4      2015-02-11 etxlg       default to inet:getaddr if simulated
%%% R3A/6      2015-03-31 etxlg       corrected find_private_binary()
%%% R3A/7      2015-04-01 etxlg       handle appm not yet started
%%% R4A/1      2015-08-28 etxlg       wait also for ootServer
%%% R4A/2      2015-09-24 eolaand     Change error_logger to sysInitI
%%% R4A/3      2015-10-12 etxlg       Trying to please the new OTP18 compiler
%%% R4A/4      2015-11-09 etxlg       IPv6
%%% R4A/5      2015-12-07 etxlg       Make TC work (without TN/OamAP)
%%% R4A/6      2015-12-08 etxlg       and for SIM
%%% R5A/1      2015-12-15 etxlg       detect OamAP:  IPv4 <-> IPv6
%%% R7A/1      2016-09-16 etxpeno     Dialyzer fixes
%%% R9A/1      2017-02-02 uabesvi     Added logging
%%% R9A/4      2017-02-07 uabesvi     Use sysEnv:rcs_mode_2 to determine if sim
%%% R9A/5      2017-02-10 eolaand     Temporarily revert correction in previous
%%%                                   version
%%% R9A/6      2017-02-14 eolaand     Revert back to corrected version 4
%%% R9A/8      2017-02-24 eolaand     Add a catch when calling server in order
%%%                                   to handle the case when a call is made
%%%                                   before the server is started.
%%% R11A/2     2017-10-05 emarnek     HW33598
%%% R12A/1     2017-10-24 etxlg       SP338? resolve in arbitrary NS
%%% R12A/2     2017-10-26 etxlg       Clean tmp_res at restart
%%% R12A/3     2017-10-27 eolaand     Fix strange dialyzer error
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).
-export([getaddr/1, getaddr/2, getaddr/3]).

-export([get_addresses/3]). %SP338

%%called in through fun by ootServer
-export([config_update/1]).

%%callback from APPM
-export([resolv_exit/1]).

%%called from APPM through ootI
-export([restart_complete/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%for debug
-export([stop/0]).


%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("oot.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SERVER, ?MODULE).
-define(LOOPBACK, {127,0,0,1}).
-define(GARB_TIME, 13). %seconds, temporary resolvers are stopped 
%%%matching macros in resolv.c
-define(DUMMY, 0).
-define(REPLY_NS, 1).
-define(RES_REP_4, 2).
-define(RES_REP_6, 3).
-define(RES_REP_ALL_4, 4).
-define(RES_REP_ALL_6, 5).
-define(ERR_NO_RET, 128).
-define(ERR_RETURN, 129).



%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------
-record(res, {pgm_id, nns, sock, lport, rport, started}).
%%state:		init -> complete -> running
%%meaning:	first -> appm ready -> in operation
-record(st, {state		:: atom(),
	     oam_nns		:: binary() | 'undefined',
	     lmt_nns		:: binary() | 'undefined',
	     oam_res		:: #res{} | 'undefined',
	     lmt_res		:: #res{} | 'undefined',
	     tmp_res = []	:: [#res{}] | [],
	     jobs = []		:: list(),
	     seq_no		:: integer(),
	     is_sim,
	     is_ipv6,
	     dummy}).


%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%% @doc
%%% Starts the SNMP proxy server
%%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

getaddr(Host) ->
    getaddr(Host, any, oam).

getaddr(Host, Inet) when Inet =:= inet; Inet =:= inet6 ->
    getaddr(Host, Inet, oam).

getaddr([_|_] = Host, Inet, NetD)
	when Inet =:= inet; Inet =:= inet6; Inet =:= any,
	     NetD =:= oam; NetD =:= lmt ->
%%  emarnek: to replace with trim when OTP 20 comes out
%%  RawHost = string:trim(Host, both, "[]"),
    RawHost = string:strip(string:strip(Host, left, $[), right, $]),
    case inet:parse_address(RawHost) of
	{error, einval} ->
	    %% ?LOG_INFO("Call server to resolve ~p (~p, ~p)", 
	    %% 	      [Host, Inet, NetD]),  
	    Res = getaddr_from_resolver(RawHost, Inet, NetD),
	    %% ?LOG_INFO("Resolve result: ~p", [Res]), 
	    Res;
	{ok, _} = Answer ->
	    Answer
    end;
getaddr({_, _, _, _} = Ip, _, Net_d)
	when Net_d =:= oam; Net_d =:= lmt ->
    inet:getaddr(Ip, inet); %try to be like inet:getaddr, which accepts this
getaddr({_,_,_,_,_,_,_,_} = Ip, _, Net_d)
	when Net_d =:= oam; Net_d =:= lmt ->
    inet:getaddr(Ip, inet6). %try to be like inet:getaddr, which accepts this

get_addresses([_|_] = Host, I, [_|_] = Nns) when I =:= inet; I =:= inet6 ->
    try
	gen_server:call(?SERVER, {get_addresses, Host, I, list_to_binary(Nns)})
    of
	{ok, Addresses} ->
	    {ok, unique(Addresses)};
	Any ->
	    Any
    catch _:_Reason ->
	    ?LOG_INFO("Call to server failed: ~p", [_Reason]),  
	    {error, eagain}
    end.

getaddr_from_resolver(Host, Inet, NetD) ->
    try
	gen_server:call(?SERVER, {getaddr, Host, Inet, NetD})
    catch _:_Reason ->
	    ?LOG_INFO("Call to server failed: ~p", [_Reason]),  
	    {error, eagain}
    end.

resolv_exit(List) ->
    gen_server:cast(?SERVER, {resolv_exit, hd(List)}).
restart_complete() ->
    gen_server:cast(?SERVER, restart_complete).

stop() ->
    gen_server:cast(?SERVER, stop).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR UNEXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% @private
%%called by fun registered in ootServer
%%HERE FIXME also look for OamIP address (IPv6 or IPv4)
config_update(Prop_list) ->
    case {proplists:get_value(oap_namespace, Prop_list),
	  proplists:get_value(access_point_address, Prop_list)} of
	{undefined, undefined} ->
	    ok;
	{_New_ns, _New_oam_ip} = Maybe_changed ->
	    gen_server:cast(?SERVER, {config_update, Maybe_changed})
    end.
%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%% ----------------------------------------------------------
%%% #           init(Args)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    case sysInitI:restart_type() of
        local -> %must have crashed, restart everything
	    self() ! async_init, %release and do all work outside of init
	    {ok, #st{state = complete,
		     seq_no = 0,
		     is_sim = sysEnv:rcs_mode_2() == simulated}};
        vm_restart -> %wait for restart_complete
	    self() ! async_init, %release and do all work outside of init
	    {ok, #st{state = init,
		     seq_no = 0,
		     is_sim = sysEnv:rcs_mode_2() == simulated}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({getaddr, Host, any, Oam_or_LMT}, _, #st{is_sim = true} = S) ->
    ?LOG_WARNING("Simulated, resolving for: ~p on ~p to IPv4",
		 [Host, Oam_or_LMT]),
    sysInitI:warning_msg(
      "~w: Simulated, resolving for: ~p on ~p to IPv4~n",
      [?MODULE, Host, Oam_or_LMT]),
    {reply, inet:getaddr(Host, inet), S};
handle_call({getaddr, Host, Inet, _Oam_or_LMT}, _, #st{is_sim = true} = S) ->
    {reply, inet:getaddr(Host, Inet), S};
handle_call({getaddr, _, _, _}, _, #st{state = Ps} = S) when Ps =/= running ->
    {reply, {error, enavail}, S};
handle_call({getaddr, Host, any, Oam_or_LMT}, From, #st{is_ipv6 = true} = S) ->
    handle_call({getaddr, Host, inet6, Oam_or_LMT}, From, S);
handle_call({getaddr, Host, any, Oam_or_LMT}, From, #st{is_ipv6 = false} = S) ->
    handle_call({getaddr, Host, inet, Oam_or_LMT}, From, S);
handle_call({getaddr, Host, Inet, oam}, _From, #st{oam_nns = <<>>} = S) ->
    %HERE FIXME this is quick workaround for testcase, better would be to run
    %the full resolver implemenation also in default namespace, but it gets
    %tricky if we (temporarily) end up with two resolvers in the same namespace
    ?LOG_WARNING("No OamAP (namespace), resolving: ~p in default namespac",
		 [Host]),
    sysInitI:warning_msg(
      "~w: No OamAP (namespace), resolving: ~p in default namespace~n",
      [?MODULE, Host]),
    {reply, inet:getaddr(Host, Inet), S};
handle_call({getaddr, Host, Inet, oam}, From, S) ->
    Job = initiate_resolve(S, S#st.oam_res, Host, Inet, oam, From),
    {noreply, bump_seq(S#st{jobs = Job ++ S#st.jobs})};
handle_call({getaddr, Host, Inet, lmt}, From, S) ->
    Job = initiate_resolve(S, S#st.lmt_res, Host, Inet, lmt, From),
    {noreply, bump_seq(S#st{jobs = Job ++ S#st.jobs})};

handle_call({getaddr, _, _, _}, _, #st{state = Ps} = S) when Ps =/= running ->
    {reply, {error, enavail}, S};
handle_call({get_addresses, Host, Inet, Nns}, _, #st{is_sim = true} = S) ->
    ?LOG_WARNING("Simulated, resolving for: ~p on ~p to ~p",
		 [Host, Nns, Inet]),
    sysInitI:warning_msg(
      "~w: Simulated, resolving for: ~p on ~p to ~p~n",
      [?MODULE, Host, Nns, Inet]),
    {reply, inet:getaddr(Host, Inet), S};
handle_call({get_addresses, Host, Inet, Nns}, From, S) ->
    New_s = initiate_resolve(S, Host, Inet, Nns, From),
    {noreply, bump_seq(New_s)};

handle_call(Call, _From, S) ->
    Reply = {error, {unknown_call, Call}},
    {reply, Reply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, S) ->
    warn_msg("Stopping by command"),
    {stop, normal, S};
handle_cast(restart_complete, S) ->
    case S#st.state of
	init ->
	    {noreply, S#st{state = complete}};
	_ ->
	    {noreply, S}
    end;
handle_cast({resolv_exit, Appm_id}, S) ->
    ?LOG_WARNING("Unexpected resolver exit~n"
		 "appm_id   = ~p~n"
		 "Resolvers = ~p~n"
		 "Jobs      = ~p",
		 [Appm_id, [S#st.oam_res, S#st.lmt_res], S#st.jobs]),
    sysInitI:warning_msg(
      "~w: Unexpected resolver exit, appm_id: ~p~n"
      "Resolvers: ~p~n"
      "Jobs: ~p~n",
      [?MODULE, Appm_id, [S#st.oam_res, S#st.lmt_res], S#st.jobs]),
						%Just restart the whole thing
    {noreply, restart_all_resolvers(S)};
handle_cast({config_update, {Ns, _Ip} = Cnf}, #st{is_ipv6 = Is_ipv6, oam_nns = Nns} = S) ->
    ?LOG_INFO("Received config_update: ~p~n", [Cnf]),
    Real_ns = case Ns of undefined -> Nns; _ -> Ns end,
    case {Real_ns, is_ipv6(ootI:get_oap_ip_addr())} of
	{Nns, Is_ipv6} -> %%%nothing changed
	    {noreply, S};
	_ -> %%%one, or both changed
	    ?LOG_INFO("Config changed, restart resolvers", []),
	    {noreply, restart_all_resolvers(S)}
    end;
handle_cast(_Msg, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({udp, _Sock, ?LOOPBACK, Rport, Data}, S) ->
    New_s = handle_udp(Rport, Data, S),
    {noreply, garb_resolvers(New_s)};
handle_info({timeout, seq_no, Seq}, #st{jobs = Jobs} = S) ->
    New_jobs = find_and_reply(Seq, Jobs, {error, eagain}),
    {noreply, garb_resolvers(S#st{jobs = New_jobs})};
handle_info(async_init, S) ->
    case S#st.state of
	complete ->
	    case ootI:register_cfg_upd_cb(fun ?MODULE:config_update/1) of
		ok ->
		    {noreply, run_async_init()};
		_ -> %sleep a bit waiting for ootServer(IMM)
		    ?LOG_INFO("OOT not ready, wait 2 more seconds"),
		    sysInitI:info_msg(
		      "~p: OOT not ready, wait 2 more seconds~n", [?MODULE]),
		    erlang:send_after(2000, self(), async_init),
		    {noreply, S}
	    end;
	init -> %sleep a bit waiting for APPM
	    sysInitI:info_msg("Restart not done, wait 2 more seconds"),
	    sysInitI:info_msg(
	      "~p: Restart not done, wait 2 more seconds~n", [?MODULE]),
	    erlang:send_after(2000, self(), async_init),
	    {noreply, S};
	_ -> %not forseen - an errror
	    ?LOG_ERROR("handle_info: ~p", [async_init]),
	    sysInitI:error_msg("~p: handle_info: ~p~n",
			       [?MODULE, async_init]),
	    {noreply, S}
    end;
handle_info(_Info, S) ->
    ?LOG_INFO("handle_info: ~p", [_Info]),
    sysInitI:info_msg("~p: handle_info: ~p~n", [?MODULE, _Info]),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _S) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(test, S, test) when size(S) =:= 11 ->
    {st, State, Oam_nns, Lmt_nns, Oam_res, Lmt_res, _Jobs,
	Seq_no, Is_sim, Is_ipv6, _Dummy} = S,
New_s = #st{state = State,
	     oam_nns = Oam_nns,
	     lmt_nns = Lmt_nns,
	     oam_res = cc(Oam_res),
	     lmt_res = cc(Lmt_res),
	     tmp_res = [],
	     jobs = [],	
	     seq_no = Seq_no,
	     is_sim = Is_sim,
	     is_ipv6 = Is_ipv6,
	     dummy = undefined},
    {ok, New_s};
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

cc(undefined) -> undefined;
cc(Tuple) ->
    List = tuple_to_list(Tuple),
    list_to_tuple( List ++ [erlang:monotonic_time(seconds)]).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%return: plain #st{}
handle_udp(Rport, <<?REPLY_NS:8, Ns/binary>> = Duh, S) ->
    case Ns of
	Match when Match =:= S#st.oam_nns ->
	    S#st{oam_res = update_rport(S#st.oam_res, Rport)};
	Match when Match =:= S#st.lmt_nns ->
	    S#st{lmt_res = update_rport(S#st.lmt_res, Rport)};
	_ -> %not suppose to happen
	    ?LOG_INFO("unexpected packet: ~p", [Duh]),
	    sysInitI:info_msg("~w: unexpected packet: ~p~n",
			      [?MODULE, Duh]),
	    S
    end;
handle_udp(_, <<?RES_REP_4:8, Seq:32/big, A:8, B:8, C:8, D:8>>,
	   #st{jobs = Jobs} = S) ->
    New_jobs = find_and_reply(Seq, Jobs, {ok, {A, B, C, D}}),
    S#st{jobs = New_jobs};
handle_udp(_, <<?RES_REP_6:8, Seq:32/big,
		A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>,
	   #st{jobs = Jobs} = S) ->
    New_jobs = find_and_reply(Seq, Jobs, {ok, {A, B, C, D, E, F, G, H}}),
    S#st{jobs = New_jobs};
handle_udp(_, <<?RES_REP_ALL_4:8, Seq:32/big, IPv4s/binary>>,
	   #st{jobs = Jobs} = S) ->
    Addresses = mk_ipv4_list(IPv4s),
    New_jobs = find_and_reply(Seq, Jobs, {ok, Addresses}),
    S#st{jobs = New_jobs};
handle_udp(_, <<?RES_REP_ALL_6:8, Seq:32/big, IPv6s/binary>>,
	   #st{jobs = Jobs} = S) ->
    Addresses = mk_ipv6_list(IPv6s),
    New_jobs = find_and_reply(Seq, Jobs, {ok, Addresses}),
    S#st{jobs = New_jobs};
handle_udp(_, <<?ERR_NO_RET:8, Seq:32/big>>, #st{jobs = Jobs} = S) ->
    warn_msg("Unexpected response[~b] from resolver, mapped to: ~p",
	     [?ERR_NO_RET, eproto]),
    New_jobs = find_and_reply(Seq, Jobs, {error, eproto}),
    S#st{jobs = New_jobs};
handle_udp(_, <<?ERR_RETURN:8, Seq:32/big, Error/binary>>,
            #st{jobs = Jobs} = S) ->
    Posix_error = map_error(Error),
    warn_msg("Error response[~b] from resolver, Error: ~p, mapped to: ~p",
	     [?ERR_RETURN, Error, Posix_error]),
    New_jobs = find_and_reply(Seq, Jobs, {error, Posix_error}),
    S#st{jobs = New_jobs}.

% initiate_resolve/6
%% return: #res{}
initiate_resolve(_, #res{rport = undefined}, _Host, _Inet, Type, From) ->
    warn_msg("Resolve attempted, but no running resolver for: ~p", [Type]),
    gen_server:reply(From, {error, eagain}),
    [];
initiate_resolve(#st{seq_no = Seq}, #res{} = Res, Host, Inet, _Type, From) ->
    udp_enc_send(Seq, Res, Host, encode_inet(Inet)),
    [{Seq, From, erlang:send_after(1000, self(), {timeout, seq_no, Seq}),
      Res#res.nns}].

% initiate_resolve/5
% return: New_s
initiate_resolve(#st{oam_nns = Oam_nns, seq_no = Seq} = S,
		 Host, Inet, Oam_nns, From) ->
    Inet_enc = encode_inet_many(Inet),
    udp_enc_send(Seq, S#st.oam_res, Host, Inet_enc),
    Job = {Seq, From,
	   erlang:send_after(1000, self(), {timeout, seq_no, Seq}),
	   Oam_nns},
    S#st{jobs = [Job | S#st.jobs]};
initiate_resolve(#st{seq_no = Seq} = S,
		 Host, Inet, Nns, From) ->
    Inet_enc = encode_inet_many(Inet),
    {New_tmp_res, Res} = find_start_tmp_resolver(S, Nns),
    case Res of
	#res{} ->
	    udp_enc_send(Seq, Res, Host, Inet_enc),
	    Job = {Seq, From,
		   erlang:send_after(1000, self(), {timeout, seq_no, Seq}),
		   Nns},
	    S#st{jobs = [Job | S#st.jobs], tmp_res = New_tmp_res};
	error ->
	    gen_server:reply(From, {error, enavail}),
	    S
    end.

encode_inet_many(inet)  -> ?RES_REP_ALL_4;
encode_inet_many(inet6) -> ?RES_REP_ALL_6.

encode_inet(inet)  -> ?RES_REP_4;
encode_inet(inet6) -> ?RES_REP_6.

udp_enc_send(Seq, #res{sock = Sock, rport = Rport}, Host, Inet_enc) ->
    gen_udp:send(Sock, ?LOOPBACK, Rport, [<<Inet_enc:8, Seq:32/big>>, Host]).

%return:  {New_tmp_res, Res}
find_start_tmp_resolver(#st{tmp_res = Tmp_res}, Nns) ->
    case lists:keytake(Nns, #res.nns, Tmp_res) of
	false ->
	    case ensure_start_resolver(Nns) of
		{ok, Res} ->
		    {[Res | Tmp_res], Res};
		{error, _} ->
		    {Tmp_res, error}
	    end;
	{value, #res{} = Res, Resolvers} ->
	    New_res = Res#res{started = erlang:monotonic_time(seconds)},
	    {[New_res | Resolvers], New_res}
    end.

%udp_send(Seq, #res{sock = Sock, rport = Rport}, Host, Inet) ->
%    Inet_enc = case Inet of inet -> ?RES_REP_4; inet6 -> ?RES_REP_6 end,
%    gen_udp:send(Sock, ?LOOPBACK, Rport, [<<Inet_enc:8, Seq:32/big>>, Host]).

%%Jobs is a list of {Seq, From, Tref, inet|inet6}
%%%for now we do not check if correct inet is returned for the relevant query
%%%(I think it may be impossible that this happens in resolv.c:getaddrinfo)
find_and_reply(Seq, Jobs, Reply) ->
    find_and_reply(Seq, Jobs, Reply, []).
find_and_reply(Seq, [J | T], Reply, Acc) when element(1, J) =:= Seq ->
    erlang:cancel_timer(element(3, J)), %it may be gone already - no matter
    gen_server:reply(element(2, J), Reply),
    T ++ Acc;
find_and_reply(Seq, [J | T], Reply, Acc)  ->
    find_and_reply(Seq, T, Reply, [J | Acc]);
find_and_reply(_, [], _, Acc)  ->
    Acc.

cancel_job(J) ->
    erlang:cancel_timer(element(3, J)), %it may be gone already - no matter
    gen_server:reply(element(2, J), {error, eagain}).

update_rport(R, Rport) ->
    R#res{rport = Rport}.

restart_all_resolvers(#st{oam_res = O_res,
			  lmt_res = L_res,
			  tmp_res = Tmp_res,
			  jobs = Jobs} = _S) ->
    [cancel_job(J) || J <- Jobs],
    stop_resolver(O_res),
    stop_resolver(L_res),
    [stop_resolver(T_res) || T_res <- Tmp_res],
    run_async_init().

run_async_init() ->
    Oam_ns =
	case ootI:get_oap_namespace() of
	    {ok, Ns} -> Ns;
	    {error, _} -> <<>>
	end,
    Lmt_ns = sysInitI:get_lmt_ns(),
    #st{state = running,
	oam_res = start_resolver(Oam_ns),
	lmt_res = start_resolver(Lmt_ns),
	seq_no = 0,
	is_sim  = sysEnv:rcs_mode_2() == simulated,
	is_ipv6 = is_ipv6(ootI:get_oap_ip_addr()),
	oam_nns = Oam_ns,
	lmt_nns = Lmt_ns}.

is_ipv6("") ->
    warn_msg("No IP assigned to OamAccessPoint - name resolver defaulted "
	     "to IPv4"),
    false;
is_ipv6(IP_string) ->
    case inet:parse_address(IP_string) of
	{ok, Tuple} when size(Tuple) =:= 8 ->
	    true;
	{ok, Tuple} when size(Tuple) =:= 4 ->
	    false;
	_ ->
	    warn_msg("OamAccessPoint IP: ~p, cannot make sense - "
		     "name resolver defaulted to IPv4", [IP_string]),
	    false
    end.

find_resolver() ->
    try  sysEnv:find_private_binary(oot, "resolv", []) of
	 Res_bin -> Res_bin
    catch
	A:B ->
	    io:format("~w: Crashed: ~p:~p~n~p~n",
		      [?MODULE, A, B, erlang:get_stacktrace()]),
	    []
    end.

%not optimal, server may block for 2 seconds here
ensure_start_resolver(Nns) ->
    case start_resolver(Nns) of
	#res{pgm_id = PgmId} when PgmId =:= undefined ->
	    {error, "not started"};
	Res -> 
	    receive
		{udp, _Sock, ?LOOPBACK, Rport, <<?REPLY_NS:8, _/binary>>} ->
		{ok, update_rport(Res, Rport)}
	    after
		2000 -> {error, "timed out"}
	    end
   end.

start_resolver(Nns) ->
    start_resolver(Nns, find_resolver()).

start_resolver(Ns, Res_bin) when Ns =:= <<>>; Res_bin =:= [] ->
    #res{};
start_resolver(Ns, Res_bin) ->
    Sock_result = gen_udp:open(0, [inet, binary, {active, true},
				   {ip, ?LOOPBACK},
				   {netns, <<"/run/netns/", Ns/binary>>}]),
    start_resolver(Ns, Res_bin, Sock_result).

start_resolver(Ns, Res_bin, {ok, Sock}) ->
    {ok, {_, Lport}} = inet:sockname(Sock),
    Start_args_list =
	[Res_bin, "-p", integer_to_list(Lport), "-n", binary_to_list(Ns)],
    %[Res_bin, "-d" ,"-p", integer_to_list(Lport), "-n", binary_to_list(Ns)],
    appm_start_resolver(Start_args_list, Ns, Sock, Lport);
start_resolver(Ns, _, Error) ->
    sysInitI:error_msg("~w: Failed to access namespace: ~p -> ~p~n",
			       [?MODULE, Ns, Error]),
    #res{}.

appm_start_resolver(Start_args_list, Ns, Sock, Lport) ->
    try appmServer:start_internal_lm([{name, "oot running resolv"},
                                      {mfa, {?MODULE, resolv_exit,[]}},
                                      {args, Start_args_list},
                                      {ns, Ns},
                                      {owner, self()},
				      {autoclean, true}]) of
        {ok, Appm_id} ->
	    #res{pgm_id = Appm_id,
		 nns = Ns,
		 sock = Sock,
		 lport = Lport,
		 started = erlang:monotonic_time(second)};
        {error, Reason} -> %FIXME HERE what to do now
            ?LOG_ERROR("Failed to run resolver: ~p", [Reason]),
            sysInitI:error_msg("~w: Failed to run resolver: ~p~n",
			       [?MODULE, Reason]),
	    gen_udp:close(Sock),
	    #res{}
    catch
        error:undef ->
	    case appmPghServer:req_spawn_pgm([{args, Start_args_list},
					      {ns, Ns}]) of
                {ok, Pgh_res} ->
                    Pgh_id = proplists:get_value(pgm_id, Pgh_res),
		    #res{pgm_id = Pgh_id,
			 nns = Ns,
			 sock = Sock,
			 lport = Lport,
			 started = erlang:monotonic_time(second)};
                Badness ->
		    ?LOG_ERROR("Failed to run resolver: ~p", [Badness]),
		    sysInitI:error_msg("~w: Failed to run resolver: ~p~n",
				       [?MODULE, Badness]),
		    gen_udp:close(Sock),
		    #res{}
            end
    end.

stop_resolver(#res{pgm_id = undefined, sock = Sock}) ->
    catch gen_udp:close(Sock),
    #res{};
stop_resolver(#res{pgm_id = Appm_id, sock = Sock}) ->
    appm_stop_resolver(Appm_id),
    gen_udp:close(Sock),
    #res{}.

appm_stop_resolver(Appm_id) ->
    _Res =
        try appmServer:stop_internal_lm(Appm_id) of
            _ -> ok
        catch
            error:undef ->
                appmPghServer:req_destroy_pgm([{pgm_id, [Appm_id]}])
        end.

mk_ipv4_list(IPv4s) ->
    mk_ipv4_list(IPv4s, []).
mk_ipv4_list(<<>>,  Acc) ->
    lists:reverse(Acc);
mk_ipv4_list(<<A:8, B:8, C:8, D:8, Rest/binary>>, Acc) ->
    mk_ipv4_list(Rest, [{A, B, C, D} | Acc]).

mk_ipv6_list(IPv6s) ->
    mk_ipv6_list(IPv6s, []).
mk_ipv6_list(<<>>,  Acc) ->
    lists:reverse(Acc);
mk_ipv6_list(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Rest/binary>>,
	     Acc) ->
    mk_ipv6_list(Rest, [{A, B, C, D, E, F, G, H} | Acc]).

% return: #st{}
garb_resolvers(#st{tmp_res = []} = S) ->
    S;
garb_resolvers(#st{tmp_res = Tmp_res, jobs = Jobs} = S) ->
    Tm = erlang:monotonic_time(seconds),
    New_res =
	lists:foldl(
	fun(#res{started = St} = Res, Acc) when Tm - St > ?GARB_TIME ->
	    case is_running(Res, Jobs) of
		true ->
		    [Res | Acc];
		false ->
		    stop_resolver(Res),
		    Acc
	    end;
	   (Res, Acc) ->
		[Res | Acc]
	end, [], Tmp_res),
    S#st{tmp_res = New_res}.

is_running(#res{nns = Nns}, Jobs) ->
    lists:keymember(Nns, 4, Jobs). %jobs should be made into record()!

%remove duplicates but retain order
unique(List) ->
    unique(List, []).

unique([], Result) ->
    lists:reverse(Result);
unique([H | T], Acc) ->
    case lists:member(H, Acc) of
	true ->
	    unique(T, Acc);
	false ->
	    unique(T, [H | Acc])
    end.

map_error(<<"EAI_AGAIN">>) -> nxdomain;
map_error(<<"EAI_MEMORY">>) -> enomem;
map_error(_) -> einval.		%just anything will have to do

bump_seq(#st{seq_no = 16#FFFFFFFF} = S) ->
    S#st{seq_no = 0};
bump_seq(#st{seq_no = Seq} = S) ->
    S#st{seq_no = Seq + 1}.

%%close_sockets(Resolvers) ->
%%    [gen_udp:close(S) || #res{sock = S} <- Resolvers, S =/= undefined].

warn_msg(Format) ->
    warn_msg(Format, []).
warn_msg(Format, Args) ->
    ?LOG_WARNING(Format, Args),
    sysInitI:warning_msg("~w: " ++ Format ++ "~n",
			 [?MODULE | Args]).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

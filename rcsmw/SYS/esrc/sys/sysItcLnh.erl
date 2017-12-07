%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysItcLnh.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R5A/6
%%% @doc == ITC link handler ==
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(sysItcLnh).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/6').
-date('2016-03-18').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% Rev     Date     Name      What
%%% -----   -------  --------  ------------------------
%%% R4A/1   150417   etxpeno   First try
%%% R4A/5   150918   etxpeno   print result from "pgrep mdulnh" when itc_link_handler has crashed
%%% ----------------------------------------------------------
%%% R5A/1   151117   etxpeno   remove dead code
%%% R5A/2   151120   etxpeno   add warmstart callbacks
%%% R5A/3   151201   etxarnu   Call sysTmmiServer:update_own_ip at warm_done
%%% R5A/4   151204   etxarnu   Call sysDhcpd:reopen_if at warm_done
%%% R5A/5   160302   etxpeno   update the internal signal numbers
%%% R5A/6   160318   etxarnu   Inform sysTmmiServer at warm 
%%% ----------------------------------------------------------

%% API
-export([start_link/0]).

-export([create_link/3,
	 destroy_link/1]).

%% warmstart callbacks
-export([prep_warm/0, warm/0, warm_done/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(ITC_LNH_CTL_CREATE_LINK,      16#01800100).
-define(ITC_LNH_CTL_CREATE_LINK_RES,  16#01800101).
-define(ITC_LNH_CTL_DESTROY_LINK,     16#01800102).
-define(ITC_LNH_CTL_DESTROY_LINK_RES, 16#01800103).
-define(ITC_LNH_CTL_LINKSTATE_IND,    16#01800104).

-define(LINK_UP,   1).
-define(LINK_DOWN, 0).

-record(state,
	{
	  itc_port,
	  mbox_id_c,
	  attach_ref,
	  links = orddict:new(),
	  pending = queue:new()
	}).

-record(link ,
	{
	  link_id,
	  name,
	  vlan_id,
	  peer_mac,
	  link_state = down
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_link(MpId, VlanId, PeerMac) ->
    sysInitI:info_msg("~p:create_link(~p,~p,~p) called~n",
		      [?MODULE, MpId, VlanId, PeerMac]),
    Res =
	case clhI:hunt_path_prefix(MpId) of
	    "" -> {error, "Same MpId"};
	    Name  ->
		gen_server:call(?SERVER, {create_link, MpId,
					  transform(name, Name),
					  VlanId,
					  transform(mac, PeerMac)})
	end,
    sysInitI:info_msg("~p:create_link() returns ~p~n",
		      [?MODULE, Res]),
    Res.

destroy_link(MpId) ->
    sysInitI:info_msg("~p:destroy_link(~p) called~n", [?MODULE, MpId]),
    gen_server:call(?SERVER, {destroy_link, MpId}).

prep_warm() ->
    gen_server:cast(?SERVER, prep_warm),
    ok.

warm() ->
    sysTmmiServer:warm_restart(),
    ok.

warm_done() ->
    sysTmmiServer:update_own_ip(),
    sysDhcpd:reopen_if(),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
    sysInitI:info_msg("~p: Starting ITC link handler~n", [?MODULE]),
    ItcPort = itc:open("itc_lnh_ctl_erl"),
    start_prog(),
    HuntRef = itc:hunt(ItcPort, "itc_lnh_ctl_c"),
    MboxId = receive
		 {mailbox_up, _ItcPort, HuntRef, S} ->
		     S
	     after 10000 ->
		     undefined
	     end,
    ok = itc:listen(ItcPort),
    AttachRef = itc:attach(ItcPort, MboxId),
    {ok, #state{mbox_id_c  = MboxId,
		itc_port   = ItcPort,
		attach_ref = AttachRef}}.

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
handle_call({create_link, _MpId, Name, VlanId, PeerMac} = Req, From, State) ->
    send_create_link(State#state.itc_port, State#state.mbox_id_c, Name, VlanId,
		     PeerMac),
    NewReq = erlang:append_element(Req, From),
    NewPending = queue:in(NewReq, State#state.pending),
    {noreply, State#state{pending = NewPending}};
handle_call({destroy_link, MpId} = Req, From, State) ->
    case orddict:find(MpId, State#state.links) of
	error ->
	    {reply, ok, State};
	{ok, Link} ->
	    send_destroy_link(State#state.itc_port, State#state.mbox_id_c,
			      Link#link.link_id),
	    NewReq = erlang:append_element(Req, From),
	    NewPending = queue:in(NewReq, State#state.pending),
	    {noreply, State#state{pending = NewPending}}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(prep_warm, State) ->
    ItcPort = State#state.itc_port,
    Mbox = State#state.mbox_id_c,
    F = fun(_MpId, Link) ->
		send_destroy_link(ItcPort, Mbox, Link#link.link_id),
		false
	end,
    NewLinks = orddict:filter(F, State#state.links),
    {noreply, State#state{links = NewLinks}};
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info({message, ItcPort, {FromMboxId, _ToMboxId, SigNo, Data}},
	    #state{itc_port   = ItcPort,
		   mbox_id_c  = FromMboxId} = State) ->
    NewState = handle_itc_msg(SigNo, Data, State),
    {noreply, NewState};
handle_info({mailbox_down, ItcPort, AttachRef, MboxId},
	    #state{itc_port   = ItcPort,
		   mbox_id_c  = MboxId,
		   attach_ref = AttachRef} = State) ->
    Pids = os:cmd("pgrep mdulnh"),
    sysInitI:error_msg("~p: The program itc_link_handler has crashed~n"
		       "pgrep mdulnh => ~p~n",
		       [?MODULE, Pids]),
    Reason = "Crash of itc_link_handler",
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

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
terminate(_Reason, #state{itc_port = ItcPort}) ->
    itc_close(ItcPort),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_prog() ->
    App = "itc_link_handler",
    PrivDir = code:priv_dir(sys),
    BinDir = sysEnv:target_bin_dir(),
    Prog = filename:join([PrivDir, BinDir, App]),
    RealProg = swmI:find_file(Prog),
    open_port({spawn_executable, RealProg}, []).

itc_close(undefined) ->
    ok;
itc_close(ItcPort) ->
    itc:close(ItcPort).

send_create_link(ItcPort, MboxId, Name, VlanId, PeerMac) ->
    Data = <<VlanId:2/native-unsigned-integer-unit:8,
	     PeerMac/binary,
	     Name/binary>>,
    ok = itc:send(ItcPort, MboxId, ?ITC_LNH_CTL_CREATE_LINK, Data).

send_destroy_link(ItcPort, MboxId, LinkId) ->
    Data = <<LinkId:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, MboxId, ?ITC_LNH_CTL_DESTROY_LINK, Data).

transform(mac, MAC) ->
    list_to_binary([list_to_integer(A, 16) || A <- string:tokens(MAC, ":")]);
transform(name, Name) ->
    iolist_to_binary([Name, 0]).

handle_itc_msg(?ITC_LNH_CTL_CREATE_LINK_RES,
	       <<LinkId:4/native-unsigned-integer-unit:8,
		 Result:4/native-signed-integer-unit:8>>, State) ->
    {{value, {create_link, MpId, Name, VlanId, PeerMac, From}},
     NewPending} = queue:out(State#state.pending),
    NewLinks =
	case Result of
	    0 ->
		gen_server:reply(From, ok),
		Link = #link{link_id  = LinkId,
			     name     = Name,
			     vlan_id  = VlanId,
			     peer_mac = PeerMac},
		orddict:store(MpId, Link, State#state.links);
	    _ ->
		gen_server:reply(From, {error, Result}),
		State#state.links
	end,
    State#state{links   = NewLinks,
		pending = NewPending};
handle_itc_msg(?ITC_LNH_CTL_DESTROY_LINK_RES,
	       <<Result:4/native-signed-integer-unit:8>>, State) ->

    case queue:out(State#state.pending) of
	{empty, NewPending} ->
	    NewLinks = State#state.links;
	{{value, {destroy_link, MpId, From}}, NewPending} ->
	    NewLinks =
		case Result of
		    0 ->
			gen_server:reply(From, ok),
			orddict:erase(MpId, State#state.links);
		    _ ->
			gen_server:reply(From, {error, Result}),
			State#state.links
		end
    end,
    State#state{links   = NewLinks,
		pending = NewPending};
handle_itc_msg(?ITC_LNH_CTL_LINKSTATE_IND,
	       <<LinkId:4/native-unsigned-integer-unit:8,
		 LinkState:4/native-signed-integer-unit:8>>, State) ->
    F = fun(_MpId, #link{link_id = LinkId0} = Link) when LinkId0 =/= LinkId ->
		Link;
	   (_MpId, Link) when LinkState == ?LINK_UP ->
		Link#link{link_state = up};
	   (_MpId, Link) when LinkState == ?LINK_DOWN ->
		Link#link{link_state = down}
	end,
    NewLinks = orddict:map(F, State#state.links),
    State#state{links = NewLinks};
handle_itc_msg(_SigNo, _Data, State) ->
    State.

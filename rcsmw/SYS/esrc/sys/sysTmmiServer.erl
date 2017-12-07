%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysTmmiServer.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R11A/1

%%% @doc ==TMMI interface==
%%% This module implements a server process that handles the TMMI interface.
%%% @end

-module(sysTmmiServer).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/R11A/1').
-date('2017-09-05').
-author('etxjotj').

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
%%% Rev        Date         Name        What
%%% -----      -------      --------    ------------------------
%%% R4A/1      2015-04-24   etxpeno     Created
%%% R4A/13     2015-10-13   etxarnu     Added update_own_ip
%%% R4A/14     2015-11-02   etxarnu     Handle case when OWN_CLUSTER_IF not defined
%%% ----------------------------------------------------------
%%% R5A/1      2015-11-17   etxpeno     remove dead code
%%% R5A/2      2015-12-01   etxarnu     exported update_own_ip/
%%% R5A/3      2016-03-01   etxarnu     Use appmPghServer instead of sudo
%%% R5A/4      2016-03-09   etxarnu     Corrected OwnIp string
%%% R5A/5      2016-03-15   etxarnu     Wait for OwnIf
%%% R5A/6      2016-03-17   etxarnu     fixed update_own_ip
%%% R11A/1     2017-09-05   etxjotj     Copied swmOs:cmdres to avoid dependency
%%% ----------------------------------------------------------

%% API
-export([start_link/0]).

%% CEC callbacks
-export([register_cec/0, cec_setup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([get_present_boards/1, pp_presentBoards/1]).
-export([update_own_ip/0]).
-export([warm_restart/0]).

-define(SERVER, ?MODULE).
-define(TMMI_SET, 0).

-define(TMMI_IF_NAME_LEN, 16).
-define(TMMI_MAC_LEN, 6).

-define(TMMI_NORMAL,    0).
-define(TMMI_EMERGENCY, 1).

-define(TMMI_BOARD_UNAVAILABLE, 0).
-define(TMMI_BOARD_AVAILABLE,   1).

-record(boardMapEntry,
	{
	  availState,
	  macEntries = []
	}).

-record(macEntry,
	{
	  trafficType,
	  interfaceName,
	  vlan,
	  prio,
	  metaMacAddress
	}).

-record(state,
	{
	  ownIf         = "",
	  sockets       = [],
	  presentBoards = orddict:new(),
	  ownIpUpdated = false
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

%%% ----------------------------------------------------------
%%% @doc Registers a callback module to CEC
%%% @end
%%% ----------------------------------------------------------
-spec register_cec() -> ok.
register_cec() ->
    ok = cec:register(<<"TMMI">>, ?MODULE).

-spec cec_setup(Socket::inet:socket()) -> pid().
cec_setup(Socket) ->
    gen_server:call(?SERVER, {cec_setup, Socket}).

warm_restart() ->
    gen_server:call(?SERVER, warm_restart).

%%% ----------------------------------------------------------
update_own_ip() ->
    case os:getenv("OWN_CLUSTER_IF") of
	false ->
	    ok;
	_ ->
	    gen_server:call(?SERVER, update_own_ip)
    end.

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
    Filename = <<"/tmp/presentBoards.txt.2">>,
    {OwnIf, PresentBoards} =
	try
	    get_present_boards(Filename)
	catch
	    _:_ ->
		sysInitI:info_msg("~p: Couldn't parse the file ~p~n"
				  "Trying to delete the file~n",
				  [?MODULE, Filename]),
		file:delete(Filename),
		{"", orddict:new()}
	end,
    sysInitI:info_msg("~p: PresentBoards: ~p~n",
		      [?MODULE, pp_presentBoards(PresentBoards)]),

    ok = update_itcLnh(PresentBoards),

    {ok, #state{ownIf = OwnIf, presentBoards = PresentBoards}}.

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
handle_call({cec_setup, Socket}, _From, #state{sockets = Sockets} = S) ->
    {reply, self(), S#state{sockets = [Socket | Sockets]}};

handle_call(update_own_ip, _From, State) ->
    NewState=update_own_ip(State),
    Reply = ok,
    {reply, Reply, NewState};
handle_call(warm_restart, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{ownIpUpdated=false}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    sysInitI:info_msg("~p  ~p~n",[#state{},State]),
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
handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?TMMI_SET:4/native-unsigned-integer-unit:8,
	       Data/binary>>}, State) ->
    Client = cec:get_program_name(ClientPid),
    sysInitI:info_msg("~p: request from ~p~n", [?MODULE, Client]),
    PresentBoards = decode_present_boards(Data, orddict:new()),
    NewState=handle_updates(State, PresentBoards),
    gen_tcp:send(Socket, <<0:4/native-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState#state{presentBoards = PresentBoards}};
handle_info({tcp_closed, Socket}, #state{sockets = Sockets} = State) ->
    {noreply, State#state{sockets = lists:delete(Socket, Sockets)}};
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
terminate(_Reason, State) ->
    [gen_tcp:close(Socket) || Socket <- State#state.sockets],
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
handle_updates(State, PresentBoards) ->
    OldPresentBoards = State#state.presentBoards,
    AddedPresentBoards =
	orddict:fold(
	  fun(MpId, OldBoardMapEntry, Acc) ->
		  case orddict:find(MpId, Acc) of
		      error ->
			  %% Removed MP
		          handle_remove_mp(MpId, Acc);
		      {ok, BoardMapEntry} ->
			  %% Update
			  handle_update_mp(MpId, OldBoardMapEntry,
					   BoardMapEntry, Acc)
		  end
	  end, PresentBoards, OldPresentBoards),

    ok =
	orddict:fold(
	  fun(MpId, BoardMapEntry, ok) ->
		  handle_add_mp(MpId, BoardMapEntry)
	  end, ok, AddedPresentBoards),
    update_own_ip(State).



update_own_ip(#state{ownIf=OwnIf, ownIpUpdated=false} = State) ->
    wait_for_if(OwnIf,10), %to give IELL some time to start HW
    OwnIp = lists:flatten(to_str(clhI:ip_address())),
    case cmdres("/sbin/ip address show |grep " ++ OwnIp) of
	{_,[]} ->
	    Cmd = ["ip","addr", "add",  OwnIp ++"/24", "dev", OwnIf],
	    sysInitI:info_msg("sysTmmiServer:update_own_ip cmd  ~p~n", [Cmd]),
	    appmPghServer:req_spawn_pgm([{args, Cmd},  {flag,1} ]),
	    State#state{ownIpUpdated=true};
	_ ->
	    State
    end;
update_own_ip(State) ->
    State.

wait_for_if(_OwnIf,0) -> ok;
wait_for_if(OwnIf,N) ->
    case cmdres("/sbin/ip address show |grep " ++ OwnIf) of
	{0,_} -> ok;
	_ ->
	    timer:sleep(500),
	    wait_for_if(OwnIf,N-1)
    end.

cmdres(CmdList) ->
    Cmd = lists:flatten(CmdList),
    CmdR = Cmd++" ; echo -n \"Res=$?\"",
    sysInitI:info_msg("sysTmmiServer: ~s~n~s~n",[Cmd, Res = os:cmd(CmdR)]),
    Code = lists:last(string:tokens(Res, "\n=")),
    Rev = lists:reverse(Res),
    Result =
	case string:str(Rev, "\n") of
	    0 -> "";
	    Pos -> lists:reverse(string:sub_string(Rev, Pos))
	end,
    {list_to_integer(Code), Result}.
 
to_str({A,B,C,D}) ->
    io_lib:format("~p.~p.~p.~p",[A,B,C,D]).

handle_remove_mp(MpId, Dict) ->
    sysInitI:info_msg("~p: MP ~p is removed~n", [?MODULE, MpId]),
    sysItcLnh:destroy_link(MpId),
    Dict.

handle_update_mp(MpId, OldBoardMapEntry, OldBoardMapEntry, Dict) ->
    %% No change
    orddict:erase(MpId, Dict);
handle_update_mp(MpId, OldBoardMapEntry, BoardMapEntry, Dict) ->
    %% Change
    sysInitI:info_msg("~p: MP ~p is changed~n"
			  "OldBoardMapEntry: ~p~n"
			  "   BoardMapEntry: ~p~n",
			  [?MODULE, MpId,
			   pp_boardMapEntry(OldBoardMapEntry),
			   pp_boardMapEntry(BoardMapEntry)]),
    orddict:erase(MpId, Dict).

handle_add_mp(MpId, BoardMapEntry) ->
    sysInitI:info_msg("~p: MP ~p is added~n"
			  "BoardMapEntry: ~p~n",
			  [?MODULE, MpId, pp_boardMapEntry(BoardMapEntry)]),
    ClusterIf = os:getenv("OWN_CLUSTER_IF"),
    update_arp(MpId, ClusterIf, BoardMapEntry#boardMapEntry.macEntries),
    update_itcLnh(MpId, BoardMapEntry#boardMapEntry.macEntries),
    ok.

decode_present_boards(<<>>, Dict) ->
    Dict;
decode_present_boards(<<MpId:1/native-unsigned-integer-unit:8,
			AvailState:4/native-unsigned-integer-unit:8,
			NumMacEntries:1/native-unsigned-integer-unit:8,
			Data/binary>>, Dict) ->
    {NewData, MacEntries} = decode_mac_entries(Data, NumMacEntries, []),

    BoardMapEntry = #boardMapEntry{availState = AvailState,
				   macEntries = MacEntries},
    NewDict = orddict:store(MpId, BoardMapEntry, Dict),
    decode_present_boards(NewData, NewDict).

decode_mac_entries(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_mac_entries(Data, N, Acc) ->
    {MacEntry, NewData} = decode_mac_entry(Data),
    decode_mac_entries(NewData, N-1, [MacEntry|Acc]).

decode_mac_entry(<<TrafficType:4/native-unsigned-integer-unit:8,
		   InterfaceName:?TMMI_IF_NAME_LEN/binary,
		   Vlan:2/native-unsigned-integer-unit:8,
		   Prio:1/native-unsigned-integer-unit:8,
		   MetaMacAddress:?TMMI_MAC_LEN/binary,
		   Data/binary>>) ->

    MacEntry = #macEntry{trafficType    = pp_trafficType(TrafficType),
			 interfaceName  = decode_interfaceName(InterfaceName),
			 vlan           = Vlan,
			 prio           = Prio,
			 metaMacAddress = pp_metaMacAddress(MetaMacAddress)},
    {MacEntry, Data}.

decode_interfaceName(Data) ->
    [InterfaceName|_] = binary:split(Data, <<0>>),
    pp_interfaceName(InterfaceName).

get_present_boards(Filename) ->
    Open = open_present_boards(Filename),
    PresentBoards = read_present_boards(Open),
    close_present_board(Open),
    PresentBoards.

open_present_boards(Filename) ->
    file:open(Filename, [read, raw, read_ahead]).

close_present_board({error, _Reason}) ->
    ok;
close_present_board({ok, IoDevice}) ->
    file:close(IoDevice).

read_present_boards({error, _Reason}) ->
    {"",orddict:new()};
read_present_boards({ok, IoDevice}) ->
    NumBoards = read_line(IoDevice, int),
    MpId = read_line(IoDevice, int),
    AvailState = read_line(IoDevice, int),
    NumMacEntries = read_line(IoDevice, int),
    MacEntries = read_mac_entries(NumMacEntries, IoDevice, []),
    BoardMapEntry = #boardMapEntry{availState = AvailState,
				   macEntries = MacEntries},
    NewAcc = orddict:store(MpId, BoardMapEntry, orddict:new()),
    OwnIf = read_own_if(MacEntries),
    {OwnIf,read_present_boards(NumBoards-1, IoDevice, NewAcc)}.

read_present_boards(0, _IoDevice, Acc) ->
    Acc;
read_present_boards(N, IoDevice, Acc) ->
    MpId = read_line(IoDevice, int),
    AvailState = read_line(IoDevice, int),
    NumMacEntries = read_line(IoDevice, int),
    MacEntries = read_mac_entries(NumMacEntries, IoDevice, []),
    BoardMapEntry = #boardMapEntry{availState = AvailState,
				   macEntries = MacEntries},
    NewAcc = orddict:store(MpId, BoardMapEntry, Acc),
    read_present_boards(N-1, IoDevice, NewAcc).

read_mac_entries(0, _IoDevice, Acc) ->
    lists:reverse(Acc);
read_mac_entries(N,  IoDevice, Acc) ->
    TrafficType = pp_trafficType(read_line(IoDevice, int)),
    InterfaceName = read_line(IoDevice, string),
    Vlan = read_line(IoDevice, int),
    Prio = read_line(IoDevice, int),
    MetaMacAddress = read_line(IoDevice, string),
    MacEntry = #macEntry{trafficType    = TrafficType,
			 interfaceName  = InterfaceName,
			 vlan           = Vlan,
			 prio           = Prio,
			 metaMacAddress = MetaMacAddress},
    read_mac_entries(N-1, IoDevice, [MacEntry|Acc]).

read_own_if([]) ->
    sysInitI:info_msg("sysTmmiServer: ownIf not found"),
    "";
read_own_if([M|_]) when M#macEntry.trafficType == normal ->
    M#macEntry.interfaceName ++ "." ++ integer_to_list( M#macEntry.vlan);
read_own_if([_H|T]) ->
    read_own_if(T).

read_line(IoDevice, int) ->
    {ok, Data} = file:read_line(IoDevice),
    list_to_integer(lists:droplast(Data));
read_line(IoDevice, string) ->
    {ok, Data} = file:read_line(IoDevice),
    lists:droplast(Data).

pp_presentBoards(PresentBoards) ->
    [pp_presentBoard(PresentBoard) || PresentBoard <- PresentBoards].

pp_presentBoard({MpId, BoardMapEntry}) ->
    [{mpId, MpId} | pp_boardMapEntry(BoardMapEntry)].

pp_boardMapEntry(BoardMapEntry) ->
    [{availState, pp_availState(BoardMapEntry#boardMapEntry.availState)},
     {macEntries, pp_macEntries(BoardMapEntry#boardMapEntry.macEntries)}].

pp_availState(?TMMI_BOARD_UNAVAILABLE) -> board_unavailable;
pp_availState(?TMMI_BOARD_AVAILABLE)   -> board_available;
pp_availState(AvailState)              -> {unknown, AvailState}.

pp_macEntries(MacEntries) ->
    [pp_macEntry(MacEntry) || MacEntry <- MacEntries].

pp_macEntry(MacEntry) ->
    [{trafficType, MacEntry#macEntry.trafficType},
     {interfaceName, MacEntry#macEntry.interfaceName},
     {vlan, MacEntry#macEntry.vlan},
     {prio, MacEntry#macEntry.prio},
     {metaMacAddress, MacEntry#macEntry.metaMacAddress}].

pp_trafficType(?TMMI_NORMAL)    -> normal;
pp_trafficType(?TMMI_EMERGENCY) -> emergency;
pp_trafficType(TrafficType)     -> {unknown, TrafficType}.

pp_interfaceName(InterfaceName) -> binary_to_list(InterfaceName).

pp_metaMacAddress(<<A,B,C,D,E,F>>) ->
    hex(A) ++ ":" ++ hex(B) ++ ":" ++ hex(C) ++ ":" ++ hex(D) ++ ":" ++
	hex(E) ++ ":" ++ hex(F);
pp_metaMacAddress(MetaMacAddress)  -> MetaMacAddress.

hex(I) -> [hex1(I div 16), hex1(I rem 16)].

hex1(I) when I >= 0, I =< 9 -> I+$0;
hex1(I) when I > 9,  I =< 15-> I-10+$a.

update_arp(_MpId, false, _MacEntries) ->
    ok;
update_arp(MpId, ClusterIf, MacEntries) ->
    [arp_macEntry(MpId, ClusterIf, MacEntry) || MacEntry <- MacEntries].

arp_macEntry(MpId, ClusterIf, #macEntry{trafficType = normal,
					metaMacAddress = Mac}) ->
    Ip = "169.254.3." ++ integer_to_list(MpId),
    Cmd = ["arp", "-i", ClusterIf, "-s", Ip, Mac],
    sysInitI:info_msg("sysTmmiServer: arp-cmd ~p~n", [Cmd]),
    appmPghServer:req_spawn_pgm([{args, Cmd},  {flag,1} ]);

arp_macEntry(_MpId, _ClusterIf, _MacEntry) ->
    ok.

update_itcLnh(PresentBoards) ->
    OwnMpId = clhI:mp_id(),
    ok = orddict:fold(
	   fun(MpId, _, ok) when MpId == OwnMpId ->
		   ok;
	      (MpId, #boardMapEntry{macEntries = MacEntries}, ok) ->
		   update_itcLnh(MpId, MacEntries)
	   end, ok, PresentBoards).

update_itcLnh(MpId, MacEntries) ->
    [itcLnh_macEntry(MpId, MacEntry) || MacEntry <- MacEntries],
    ok.

itcLnh_macEntry(MpId, #macEntry{trafficType    = normal,
				vlan           = VlanId,
				metaMacAddress = PeerMac}) ->
    sysItcLnh:create_link(MpId, VlanId, PeerMac),
    ok;
itcLnh_macEntry(_MpId, _MacEntry) ->
    ok.

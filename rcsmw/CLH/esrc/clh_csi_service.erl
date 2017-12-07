%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	clh_csi_service.erl %
%%% Author:     etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(clh_csi_service).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/1').
-date('2016-07-08').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R2A/1      2012-11-08   etxbjca     Created
%%% R3A/1      2015-02-04   etxpejn     Added create_mp & delete_mp
%%% R3A/12     2015-02-25   etxberb     Added restart_own_piu at nodedown.
%%% R4A/1      2015-04-17 etxberb     Updated according to changed interfaces.
%%% R4A/2      2015-04-20 etxberb     Bug fix and additional functionality.
%%% R4A/3      2015-04-22 etxberb     Removed side effects from
%%%                                   mnesia-transaction.
%%% R4A/4      2015-04-24 etxberb     Added CHI-messages.
%%% R4A/5      2015-04-24 etxberb     Added CSI-messages.
%%% R4A/6      2015-04-27 etxberb     Added CSI_GET_HUNT_PATH_PREFIX.
%%% R4A/7      2015-04-29 etxberb     Added cluster_config file.
%%% R4A/8      2015-05-08 etxberb     Added subscribe functionality.
%%% R4A/9      2015-05-19 etxberb     Bug fix.
%%% R4A/10     2015-05-20 etxberb     Added get_coreRank/1, get_fruId/1 &
%%%                                   get_mpRole/1.
%%% R4A/11     2015-05-21 etxberb     Implemented HUNT_PATH_PREFIX.
%%% R4A/12     2015-05-28 etxberb     Changed 'clhI:role' to 'clhI:mp_role'.
%%% R4A/13     2015-06-05 etxberb     Changed undef("") FruId to default string.
%%% R4A/16     2015-07-07 etxberb     Added install_begin/0.
%%% R4A/17     2015-08-11 etxberb     Moved cluster_config file to home_dir.
%%% R4A/18     2015-08-20 etxberb     Added more info in INFO REPORTs.
%%% R4A/22     2015-09-11 etxpeno     add dhcp at associate_mp, etc
%%% R4A/23     2015-09-18 etxberb     The cluster_complete file now created on
%%%                                   both target and sim. Created when a second
%%%                                   MP gets associated.
%%% R4A/24     2015-10-20 etxberb     Added reset_MpState/1.
%%% R5A/1      2015-12-03 etxpejn     Revert to NL at disassociate_mp on regular
%%% R5A/2      2015-12-07 etxpejn     Changed error to warning in disassociate_mp on core
%%% R5A/3      2015-12-18 etxpejn     MpId = 2 reserved for core, added in fun_associate_mp
%%% R6A/1      2016-07-08 etxarnu    Changed file:get_cwd to sysEnv:home_dir
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([install_begin/0]).
-export([start/0,
         start/1,
         start_link/0,
         start_link/1,
         stop/0]).

-export([activate/0]).
-export([cec_setup/1]).
-export([cluster_restart/4]).
-export([soft_cluster_restart_request/1]).
-export([soft_cluster_restart_reply/1]).
-export([associate_mp/3]).
-export([disassociate_mp/1]).
-export([get_node_op_state/0]).

-export([mnesia_stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
	 format_status/2]).

-export([file_name_cluster_config/0,
	 file_content_cluster_config/0]).

-export([revert_to_nl/0]).

%% Used only in test purposes
-export([trigger_ChiOpStateChangeInd/1,
	 trigger_CsiCoreStateChangeInd/1,
	 connect_to_mp/1]).

-export([get_coreRank/1,
	 get_fruId/1,
	 get_mpRole/1,
	 get_state/0,
	 info/0,
	 info_all/0]).

-define(CSI_OK,                         0).
-define(CSI_ERROR_SERVER_NOT_AVAILABLE, 1).
-define(CSI_NO_EXIST,                   2).

-define(CSI_ROLE_ACTIVE,  0).   % Deprecated
-define(CSI_ROLE_STANDBY, 1).   % Deprecated
-define(CSI_ROLE_REGULAR, 2).   % Deprecated

-define(CSI_CORE_STATE_ACTIVE,    0).
-define(CSI_CORE_STATE_STANDBY,   1).
-define(CSI_CORE_STATE_UNDEFINED, 2).

-define(CSI_CLUSTER_RESTART_HARD, 0).
-define(CSI_CLUSTER_RESTART_SOFT, 1).

-define(CLUSTER_RESTART_TO,      15000). 


-define(CHI_CORE_RANK_PRIMARY,   0).
-define(CHI_CORE_RANK_SECONDARY, 1).
-define(CHI_CORE_RANK_UNDEFINED, 2).

-define(CHI_RESTART_RANK_COLD,           0).
-define(CHI_RESTART_RANK_COLD_WITH_TEST, 1).
-define(CHI_RESTART_RANK_WARM,           2).

-define(CSI_RESTART_TYPE_HARD, 0).
-define(CSI_RESTART_TYPE_SOFT, 1).

-define(CHI_RESULT_OK,                         0).
-define(CHI_RESULT_ERROR_SERVER_NOT_AVAILABLE, 1).
-define(CHI_RESULT_NO_EXIST ,                  2).
-define(CHI_RESULT_BAD_PARM ,                  3).
-define(CHI_RESULT_NO_ITC_MBOX,                4).   % Not applicable in erlang

-define(CHI_OPSTATE_DISABLED, 0).
-define(CHI_OPSTATE_ENABLED,  1).

-define(CSI_GET_STATE,    0).   % Deprecated
-define(CSI_SUBSCRIBE,    1).   % Deprecated
-define(CSI_UNSUBSCRIBE,  2).   % Deprecated
-define(CSI_GET_ROLE,     3).   % Deprecated
-define(CSI_SUBSCRIBE2,   4).   % Deprecated
-define(CSI_UNSUBSCRIBE2, 5).   % Deprecated
-define(CHI_GET_STATE,    6).   % Deprecated
-define(CHI_SUBSCRIBE,    7).   % Deprecated
-define(CHI_UNSUBSCRIBE,  8).   % Deprecated
-define(CSI_GET_OWN_MPID,                 9).
-define(CSI_SUBSCRIBE_CORE_STATE,        10).
-define(CSI_UNSUBSCRIBE_CORE_STATE,      11).
-define(CHI_SUBSCRIBE_OP_STATE,          12).
-define(CHI_UNSUBSCRIBE_OP_STATE,        13).
-define(CHI_ASSOCIATE_MP,                14).
-define(CHI_DISASSOCIATE_MP,             15).
-define(CHI_CLUSTER_RESTART,             16).
-define(CSI_GET_HUNT_PATH_PREFIX,        17).
-define(CSI_SUBSCRIBE_CLUSTER_RESTART,   18).
-define(CSI_UNSUBSCRIBE_CLUSTER_RESTART, 19).
-define(CSI_CLUSTER_RESTART_REPLY,       20).

-define(SERVER, ?MODULE).

-define(CSI_CHANGE_OPER_STATE_IND, 16#0180000).   % Deprecated
-define(CHI_CHANGE_STATE_IND,      16#0180001).   % Deprecated
-define(CSI_CHANGE_ROLE_IND,       16#0180002).   % Deprecated
-define(CHI_OP_STATE_CHANGE_IND,   16#0180003).
-define(CSI_CORE_STATE_CHANGE_IND, 16#0180004).
-define(CSI_CLUSTER_RESTART_IND,   16#0180006).

-define(CSI_SUBSCRIPTION1, csi_subscription1).   % Deprecated
-define(CSI_SUBSCRIPTION2, csi_subscription2).   % Deprecated
-define(CHI_SUBSCRIPTION,  chi_subscription).    % Deprecated

-define(DIR_HOME, sysEnv:home_dir()).
-define(FILENAME_ClusterConfig, "cluster_config").

-define(PROC_INFO(Pid), sysUtil:pid_info(Pid, {all, [error_handler]})).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).

%% General
-define(ELSE, true).

-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

-include("clhTables.hrl").




%%===========================================================================
%% #state
%% 
%% subscrs_clusterRestart         = apps subscribing to soft cluster restart
%% args_clusterRestart            = cluster restart arguments
%% waiting_subscrs_clusterRestart = these apps have not yet replied on
%%                                  soft cluster restart request
%% waiting_nodes_clusterRestart   = these nodes have not yet replied on
%%                                  soft cluster restart request
%% core_node_clusterRestart       = used by regular nodes. The core node
%%                                  where to send the soft restart reply
%% timer_ref_clusterRestart       = reference to the time out message
%%===========================================================================
-record(state,
	{
	  mpId,
	  myConf,
	  myState,
	  subscrs_coreState = [],
	  subscrs_opState = [],
	  subscrs_clusterRestart = [], 
	  args_clusterRestart,         
	  waiting_subscrs_clusterRestart = [],
	  waiting_nodes_clusterRestart = [],  
	  core_node_clusterRestart,           
	  timer_ref_clusterRestart,
	  mp_list = [],
	  itc_port
	 }).

-record(mbox,
	{
	  id,
	  monRef,
	  pid_name
	 }).

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Get the CoreRank for an Mp.
%%%
%%% @end
%%% ###=====================================================================###
get_coreRank(MpId) ->
    case mnesia:dirty_read(clhMpConf, MpId) of
	[#clhMpConf{coreRank = CoreRank}] ->
	    {ok, CoreRank};
	[] ->
	    {error, mp_undefined}
    end.

%%% ###########################################################################
%%% @doc Get the FruId for an Mp.
%%%
%%% @end
%%% ###=====================================================================###
get_fruId(MpId) ->
    case mnesia:dirty_read(clhMpConf, MpId) of
	[#clhMpConf{fruId = FruId}] ->
	    {ok, FruId};
	[] ->
	    {error, mp_undefined}
    end.

%%% ###########################################################################
%%% @doc Get the MpRole for an Mp.
%%%
%%% @end
%%% ###=====================================================================###
get_mpRole(MpId) ->
    case mnesia:dirty_read(clhMpConf, MpId) of
	[#clhMpConf{mpRole = MpRole}] ->
	    {ok, MpRole};
	[] ->
	    {error, mp_undefined}
    end.

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
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
install_begin() ->
    OwnMpId = clhI:mp_id(),
    {atomic, _} =
	mnesia:transaction(fun() ->
				   fun_tbl_init_myself(OwnMpId)
			   end),
    ok.

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
    ok = net_kernel:monitor_nodes(true, [{node_type, visible},
    					 nodedown_reason]),
    OwnMpId = clhI:mp_id(),
    mnesia:subscribe({table, clhMpState, detailed}),
    {atomic, {MyMpConf, MyMpState}} =
	mnesia:transaction(fun() ->
				   fun_tbl_init_myself(OwnMpId)
			   end),
    case file:read_file_info(file_name_cluster_config()) of
	{error, enoent} ->
	    nl_file_write(?FILENAME_ClusterConfig, nl_file_config_info());
	_ ->
	    ok
    end,
    MpList = [{MpId, clhI:erlang_node(MpId)} || MpId <- clhI:mp_id(all)],
    ItcPort = itc:open("clh_csi"),
    State = #state{mpId     = OwnMpId,
		   myConf   = MyMpConf,
		   myState  = MyMpState,
		   mp_list  = MpList,
		   itc_port = ItcPort},
    {ok, State}.

activate() ->
    gen_server:cast(?SERVER, activate).

cec_setup(_Socket) ->
    gen_server:call(?SERVER, cec_setup).

cluster_restart(MpId, _RestartType, _RestartRank, _RestartCause) ->
    sysInitI:info_msg("~p:cluster_restart called for MpId: ~p~n", [?MODULE, MpId]),
    %%TODO what?
    ok.

soft_cluster_restart_request(CoreNode) ->
    gen_server:call(?SERVER, {soft_cluster_restart_request, CoreNode}).
    
soft_cluster_restart_reply(Node) ->
    gen_server:call(?SERVER, {soft_cluster_restart_reply, Node}).
    
associate_mp(MpId, FruId, CoreRank) ->
    gen_server:call(?SERVER, {associate_mp, MpId, FruId, CoreRank}).

disassociate_mp(MpId) ->
    gen_server:call(?SERVER, {disassociate_mp, MpId}).

get_node_op_state() ->
    gen_server:call(?SERVER, get_node_op_state).

trigger_CsiCoreStateChangeInd(MpId) ->
    gen_server:cast(?SERVER, {trigger_CsiCoreStateChangeInd, MpId}).

trigger_ChiOpStateChangeInd(MpId) ->
    gen_server:cast(?SERVER, {trigger_ChiOpStateChangeInd, MpId}).

mnesia_stop() ->
    %% TODO, why error on node when using mnesia:stop/0?
    %% mnesia:stop(),
    ok.

handle_call(cec_setup, _From, S) ->
    Reply = self(),
    {reply, Reply, S};

handle_call({associate_mp, MpId, FruId, CoreRank}, From, S) ->
    {Result, NewS} =
	transaction_associate_mp(MpId, FruId, CoreRank, From, S),
    result_info(log, associate_mp),
    {reply, Result, NewS};

handle_call({disassociate_mp, MpId}, From, S) ->
    {Result, NewS} = transaction_disassociate_mp(MpId, From, S),
    result_info(log, disassociate_mp),
    {reply, Result, NewS};

handle_call(get_node_op_state, _From, S) ->
    %% Collect all MP operational state and provide a node
    %% operational state.
    %% All MPs working => "Node is working"
    %% One or more MPs are not working => "Node is partly working"
    Reply = {ok, "Node is working"},
    {reply, Reply, S};

handle_call({soft_cluster_restart_request, CoreNode}, _From, S) ->
    NewS = soft_cluster_restart_request(CoreNode, S),
    {reply, ok, NewS};

handle_call({soft_cluster_restart_reply, Node}, _From, S) ->
    NewS = soft_cluster_restart_reply(Node, S),
    {reply, ok, NewS};




handle_call({?MODULE, get_state}, _From, State) ->
    {reply, State, State};
handle_call(Command, From, S) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(From)] ++
	[{unrecognized_msg, Command} |
	 ?STATE_INFO(S)],
    sysInitI:warning_report(Info),
    {reply, Command, S}.

handle_cast({trigger_CsiCoreStateChangeInd, MpId}, S) ->
    case check_mp_id(MpId) of
	true ->
	    send_csi_core_state_change_ind(S, MpId);
	false ->
	    ok
    end,
    {noreply, S};

handle_cast({trigger_ChiOpStateChangeInd, MpId}, S) ->
    case check_mp_id(MpId) of
	true ->
	    send_chi_op_state_change_ind(S, MpId);
	false ->
	    ok
    end,
    {noreply, S};

handle_cast({cluster_restart, Data}, S) ->
    NewS = cluster_restart_inform_subscribers(Data, clhI:erlang_nodes(), S),
    {noreply, NewS};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(activate, S) ->
    {atomic, NewS} =
	mnesia:transaction(fun() ->
				   fun_activate(S)
			   end),
    {noreply, NewS};

handle_cast({?MODULE, info}, State) ->
    sysInitI:info_report(?PROC_INFO(self()) ++ ?STATE_INFO(State)),
    {noreply, State};
handle_cast(_Request, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, _Request} |
				 ?STATE_INFO(S)]),
    {noreply, S}.

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CSI_GET_STATE:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result =
	case check_mp_id(MpId) of
	    true ->  ?CSI_OK;
	    false -> ?CSI_NO_EXIST
	end,

    OpState = get_op_state(MpId),
    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8,
			   OpState:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CSI_SUBSCRIBE:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8,
	       MboxId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result = add_csi_subscriber(check_mp_id(MpId), MpId, MboxId,
				?CSI_SUBSCRIPTION1),
    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CSI_UNSUBSCRIBE:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8,
	       MboxId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result = remove_csi_subscriber(check_mp_id(MpId), MpId, MboxId,
				   ?CSI_SUBSCRIPTION1),
    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CSI_GET_ROLE:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result =
	case check_mp_id(MpId) of
	    true ->  ?CSI_OK;
	    false -> ?CSI_NO_EXIST
	end,
    Role = get_role(MpId),

    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8,
			   Role:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CSI_SUBSCRIBE2:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8,
	       MboxId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result = add_csi_subscriber(check_mp_id(MpId), MpId, MboxId,
				?CSI_SUBSCRIPTION2),
    send_csi_change_role_ind(Result, S#state.itc_port, MpId, MboxId),

    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CSI_UNSUBSCRIBE2:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8,
	       MboxId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result = remove_csi_subscriber(check_mp_id(MpId), MpId, MboxId,
				   ?CSI_SUBSCRIPTION2),
    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CHI_GET_STATE:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result =
	case check_mp_id(MpId) of
	    true ->  ?CSI_OK;
	    false -> ?CSI_NO_EXIST
	end,
    OpState = get_op_state(MpId),
    Role = get_role(MpId),

    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8,
			   OpState:4/native-unsigned-integer-unit:8,
			   Role:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CHI_SUBSCRIBE:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8,
	       MboxId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result = add_chi_subscriber(check_mp_id(MpId), MpId, MboxId),
    send_chi_change_state_ind(Result, S#state.itc_port, MpId, MboxId),

    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CHI_UNSUBSCRIBE:4/native-unsigned-integer-unit:8,
	       MpId:4/native-unsigned-integer-unit:8,
	       MboxId:4/native-unsigned-integer-unit:8>>},
	    S) ->
%%% ########################### Deprecated ###########################
    Result = remove_chi_subscriber(check_mp_id(MpId), MpId, MboxId),
    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

%%% =================================== CHI ===================================
handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CHI_ASSOCIATE_MP:4/native-unsigned-integer-unit:8,
 	      MpId:4/native-unsigned-integer-unit:8,
 	      FruIdLen:4/native-unsigned-integer-unit:8,
	      FruId:FruIdLen/binary,
 	      CoreRank:4/native-unsigned-integer-unit:8,
 	      _MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, NewS} =
	try
	    msg_CHI_ASSOCIATE_MP(ClientPid,
				 MpId,
				 FruId,
				 c2erl_CoreRank(CoreRank, ClientPid),
				 S)
	catch
	    _ : Result ->
		{erl2c_ChiResult(Result), S}
	end,
    result_info(put, [{cResult, CResult}]),
    TcpRes =
	gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    InetRes = inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    result_info(put, [{tcp_send, TcpRes}, {inet_setopts, InetRes}]),
    result_info(log, msg_CHI_ASSOCIATE_MP),
    {noreply, NewS};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CHI_DISASSOCIATE_MP:4/native-unsigned-integer-unit:8,
 	      MpId:4/native-unsigned-integer-unit:8,
 	      _MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, NewS} =
	try
	    msg_CHI_DISASSOCIATE_MP(ClientPid, MpId, S)
	catch
	    _ : Result ->
		{erl2c_ChiResult(Result), S}
	end,
    result_info(put, [{cResult, CResult}]),
    TcpRes =
	gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    InetRes = inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    result_info(put, [{tcp_send, TcpRes}, {inet_setopts, InetRes}]),
    result_info(log, msg_CHI_DISASSOCIATE_MP),
    {noreply, NewS};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CHI_CLUSTER_RESTART:4/native-unsigned-integer-unit:8,
 	      RestartType:4/native-unsigned-integer-unit:8,
 	      RestartRank:4/native-unsigned-integer-unit:8,
 	      RestartCauseLen:4/native-unsigned-integer-unit:8,
	      RestartCause:RestartCauseLen/binary,
 	      _MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    try
	msg_CHI_CLUSTER_RESTART(Socket,
				ClientPid,
				c2erl_RestartType(RestartType, ClientPid),
				c2erl_RestartRank(RestartRank, ClientPid),
				sysUtil:term_to_string(RestartCause))
    catch
	_ : Result ->
	    CResult = erl2c_ChiResult(Result),
	    gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
	    inet:setopts(Socket, [{active, once}, {nodelay, true}])
    end,
    {noreply, S};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CHI_SUBSCRIBE_OP_STATE:4/native-unsigned-integer-unit:8,
 	      MpId:4/native-unsigned-integer-unit:8,
 	      MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, COpState, NewS} =
	try
	    msg_CHI_SUBSCRIBE_OP_STATE(ClientPid, MpId, MboxId, S)
	catch
	    _ : Result ->
		{erl2c_ChiResult(Result), -1, S}
	end,
    result_info(put, [{cResult, CResult}, {cOpState, COpState}]),
    case CResult of
	?CHI_RESULT_OK ->
	    Data =
		<<MpId:4/native-unsigned-integer-unit:8,
		 COpState:4/native-unsigned-integer-unit:8>>,
	    ItcRes =
		itc:send(S#state.itc_port,
			 MboxId,
			 ?CHI_OP_STATE_CHANGE_IND,
			 Data),
	    result_info(put, [{itc_send, ItcRes}]);
	_ ->
	    ok
    end,
    TcpRes =
	gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    InetRes = inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    result_info(put, [{tcp_send, TcpRes}, {inet_setopts, InetRes}]),
    result_info(log, msg_CHI_SUBSCRIBE_OP_STATE),
    {noreply, NewS};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CHI_UNSUBSCRIBE_OP_STATE:4/native-unsigned-integer-unit:8,
 	      MpId:4/native-unsigned-integer-unit:8,
 	      MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, NewS} =
	try
	    msg_CHI_UNSUBSCRIBE_OP_STATE(ClientPid, MpId, MboxId, S)
	catch
	    _ : Result ->
		{erl2c_ChiResult(Result), S}
	end,
    result_info(put, [{cResult, CResult}]),
    TcpRes =
	gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    InetRes = inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    result_info(put, [{tcp_send, TcpRes}, {inet_setopts, InetRes}]),
    result_info(log, msg_CHI_UNSUBSCRIBE_OP_STATE),
    {noreply, NewS};


%%% =================================== CSI ===================================
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?CSI_GET_OWN_MPID:4/native-unsigned-integer-unit:8>>},
	    S) ->
    Result = ?CSI_OK,
    MpId = S#state.mpId,
    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8,
			   MpId:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp,
	     Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	      ?CSI_GET_HUNT_PATH_PREFIX:4/native-unsigned-integer-unit:8,
 	      MpId:4/native-unsigned-integer-unit:8>>},
	    S) ->
    Result = ?CSI_OK,
    HuntPathPrefix = list_to_binary(clhI:hunt_path_prefix(MpId)),
    HuntPathPrefixLen = erlang:size(HuntPathPrefix),
    gen_tcp:send(Socket, <<Result:4/native-unsigned-integer-unit:8,
			  HuntPathPrefixLen:4/native-unsigned-integer-unit:8,
			  HuntPathPrefix/binary>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CSI_SUBSCRIBE_CORE_STATE:4/native-unsigned-integer-unit:8,
 	      MpId:4/native-unsigned-integer-unit:8,
 	      MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, CCoreState, NewS} =
	try
	    msg_CSI_SUBSCRIBE_CORE_STATE(ClientPid, MpId, MboxId, S)
	catch
	    _ : Result ->
		{erl2c_CsiResult(Result), -1, S}
	end,
    case CResult of
	?CSI_OK ->
	    Data =
		<<MpId:4/native-unsigned-integer-unit:8,
		 CCoreState:4/native-unsigned-integer-unit:8>>,
	    itc:send(S#state.itc_port,
		     MboxId,
		     ?CSI_CORE_STATE_CHANGE_IND,
		     Data);
	_ ->
	    ok
    end,
    gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, NewS};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CSI_UNSUBSCRIBE_CORE_STATE:4/native-unsigned-integer-unit:8,
 	      MpId:4/native-unsigned-integer-unit:8,
 	      MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, NewS} =
	try
	    msg_CSI_UNSUBSCRIBE_CORE_STATE(ClientPid, MpId, MboxId, S)
	catch
	    _ : Result ->
		{erl2c_CsiResult(Result), S}
	end,
    gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, NewS};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CSI_SUBSCRIBE_CLUSTER_RESTART:4/native-unsigned-integer-unit:8,
 	      MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, NewS} =
	try
	    msg_CSI_SUBSCRIBE_CLUSTER_RESTART(ClientPid, MboxId, S)
	catch
	    _ : Result ->
		{erl2c_ChiResult(Result), -1, S}
	end,
    result_info(put, [{cResult, CResult}]),
    TcpRes =
	gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    InetRes = inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    result_info(put, [{tcp_send, TcpRes}, {inet_setopts, InetRes}]),
    result_info(log, msg_CSI_SUBSCRIBE_CLUSTER_RESTART),
    {noreply, NewS};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CSI_UNSUBSCRIBE_CLUSTER_RESTART:4/native-unsigned-integer-unit:8,
 	      MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, NewS} =
	try
	    msg_CSI_UNSUBSCRIBE_CLUSTER_RESTART(ClientPid, MboxId, S)
	catch
	    _ : Result ->
		{erl2c_ChiResult(Result), S}
	end,
    result_info(put, [{cResult, CResult}]),
    TcpRes =
	gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    InetRes = inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    result_info(put, [{tcp_send, TcpRes}, {inet_setopts, InetRes}]),
    result_info(log, msg_CSI_UNSUBSCRIBE_CLUSTER_RESTART),
    {noreply, NewS};

handle_info({tcp,
	     Socket,
 	     <<ClientPid:4/native-unsigned-integer-unit:8,
 	      ?CSI_CLUSTER_RESTART_REPLY:4/native-unsigned-integer-unit:8,
 	      MboxId:4/native-unsigned-integer-unit:8>>},
 	    S) ->
    {CResult, NewS} =
	try
	    msg_CSI_CLUSTER_RESTART_REPLY(ClientPid, MboxId, S)
	catch
	    _ : Result ->
		{erl2c_ChiResult(Result), S}
	end,
    result_info(put, [{cResult, CResult}]),
    TcpRes =
	gen_tcp:send(Socket, <<CResult:4/native-unsigned-integer-unit:8>>),
    InetRes = inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    result_info(put, [{tcp_send, TcpRes}, {inet_setopts, InetRes}]),
    result_info(log, msg_CSI_CLUSTER_RESTART_REPLY),
    {noreply, NewS};

%%% =================================== * ===================================
handle_info({tcp_closed, _Socket}, S) ->
    {noreply, S};

handle_info({nodeup, N, InfoList}, S) ->
    sysInitI:info_report([{?MODULE, nodeup},
			      {node, N} |
			      InfoList]),
    {noreply, S};
handle_info({nodedown, N, InfoList}, S) ->
    sysInitI:info_report([{?MODULE, nodedown},
			      {node, N} |
			      InfoList]),
    set_opState(clhI:mp_id(N), disabled),
    case is_core_alive() of
	true ->
	    ok;
	false ->
	    appmI:restart_own_piu(cold, false, "Core MP down")
    end,
    {noreply, S};
handle_info({mnesia_table_event, Event}, S) ->
    mnesia_table_event_log(Event, S),
    NewS = mnesia_table_event(Event, S),
    {noreply, NewS};
handle_info({mailbox_down, _Port, _MonRef, MboxId}, S) ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			      {mailbox_down, MboxId},
			      {port, _Port},
			      {monRef, _MonRef} |
			      ?STATE_INFO(S)]),
    NewS = subscr_remove_all(MboxId, S),
    {noreply, NewS};
handle_info(cluster_restart_to, #state{args_clusterRestart = Args} = S) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			      {reason, cluster_restart_timeout} |
			      ?STATE_INFO(S)]),
    NewS = do_cluster_restart(Args, S),
    {noreply, NewS};
handle_info(Info, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, Info} |
				 ?STATE_INFO(S)]),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

format_status(_Opt, [_Pdict, S]) ->
     [{data, [{"State", [{own_mp, S#state.mpId},
			 {mp_list, S#state.mp_list},
			 {itc_port, S#state.itc_port}]}]}].

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

result_info(put, List) ->
    case get(result_info) of
	undefined ->
	    put(result_info, List);
	RI ->
	    put(result_info, RI ++ List)
    end;
result_info(log, Func) ->
    Info =
	case erase(result_info) of
	    undefined ->
		[{?MODULE, {?FUNCTION, Func}}];
	    RI ->
		RI
	end,
    sysInitI:info_report(Info).


%%% ----------------------------------------------------------
%%% @doc This function will show the file name of cluster_config.
%%% @end
%%% ----------------------------------------------------------
file_name_cluster_config() ->
    filename:join(?DIR_HOME, ?FILENAME_ClusterConfig).

%%% ----------------------------------------------------------
%%% @doc This function will show the content of file cluster_config.
%%% @end
%%% ----------------------------------------------------------
file_content_cluster_config() ->
    case file:read_file(file_name_cluster_config()) of
	{ok, Bin} ->
	    binary_to_term(Bin);
	_ ->
	    undefined
    end.

send_chi_change_state_ind(?CHI_RESULT_OK, ItcPort, MpId, MboxId) ->
    Data = get_chi_change_state_ind(MpId),
    itc:send(ItcPort, MboxId, ?CHI_CHANGE_STATE_IND, Data);
send_chi_change_state_ind(_, _, _, _) ->
    ok.

%%% ########################### Deprecated ###########################
get_chi_change_state_ind(MpId) ->
    OpState = get_op_state(MpId),
    Role = get_role(MpId),
    <<MpId:4/native-unsigned-integer-unit:8,
      OpState:4/native-unsigned-integer-unit:8,
      Role:4/native-unsigned-integer-unit:8>>.

%% -----------------------------------------------------------------------------
send_csi_core_state_change_ind(S, MpId) ->
    CoreState = get_core_state(MpId),
    subscr_coreState_notify(MpId, CoreState, S),
    ok.

%% -----------------------------------------------------------------------------
send_chi_op_state_change_ind(S, MpId) ->
    OpState = get_op_state(MpId),
    subscr_opState_notify(MpId, OpState, S),
    ok.

send_csi_change_role_ind(?CSI_OK, ItcPort, MpId, MboxId) ->
    Data = get_csi_change_role_ind(MpId),
    itc:send(ItcPort, MboxId, ?CSI_CHANGE_ROLE_IND, Data);
send_csi_change_role_ind(_, _, _, _) ->
    ok.

get_csi_change_role_ind(MpId) ->
    Role = get_role(MpId),
    <<MpId:4/native-unsigned-integer-unit:8,
      Role:4/native-unsigned-integer-unit:8>>.

add_csi_subscriber(true, MpId, MboxId, Subscription) ->
    clh_db:add_subscriber(MpId, MboxId, Subscription),
    ?CSI_OK;
add_csi_subscriber(false, _MpId, _MboxId, _Subscription) ->
    ?CSI_NO_EXIST.

remove_csi_subscriber(true, MpId, MboxId, Subscription) ->
    clh_db:remove_subscriber(MpId, MboxId, Subscription),
    ?CSI_OK;
remove_csi_subscriber(false, _MpId, _MboxId, _Subscription) ->
    ?CSI_NO_EXIST.

%%% ########################### Deprecated ###########################
add_chi_subscriber(true, MpId, MboxId) ->
    clh_db:add_subscriber(MpId, MboxId, ?CHI_SUBSCRIPTION),
    ?CHI_RESULT_OK;
add_chi_subscriber(false, _MpId, _MboxId) ->
    ?CHI_RESULT_NO_EXIST.

%%% ########################### Deprecated ###########################
remove_chi_subscriber(true, MpId, MboxId) ->
    clh_db:remove_subscriber(MpId, MboxId, ?CHI_SUBSCRIPTION),
    ?CHI_RESULT_OK;
remove_chi_subscriber(false, _MpId, _MboxId) ->
    ?CHI_RESULT_NO_EXIST.

get_op_state(MpId) ->
    case clhI:op_state(MpId) of
	enabled ->
	    ?CHI_OPSTATE_ENABLED;
	_ ->
	    ?CHI_OPSTATE_DISABLED
    end.

get_core_state(MpId) ->
    case clhI:core_state(MpId) of
	active ->
	    ?CSI_CORE_STATE_ACTIVE;
	standby ->
	    ?CSI_CORE_STATE_STANDBY;
	_ ->
	    ?CSI_CORE_STATE_UNDEFINED
    end.

%%% ########################### Deprecated ###########################
get_role(1) -> ?CSI_ROLE_ACTIVE;
get_role(_) -> ?CSI_ROLE_REGULAR.

check_mp_id(MpId) ->
    case mnesia:dirty_read(clhMpConf, MpId) of
	[] ->
	    false;
	_Obj ->
	    true
    end.

%% check_mp_id(MpId, S) ->
%%     S#state.own_mp == MpId orelse
%% 	lists:keyfind(MpId, 1, S#state.mp_list) /= false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd(Cmd) ->
    info_msg("~s~n~s~n", [Cmd, os:cmd(Cmd)]).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: " ++ Format, [?MODULE|Args]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% connect_to_mp
%%%
%%% ###=====================================================================###
connect_to_mp(MpId) ->
    ErlangNode = sysEnv:get_erlang_node(MpId),
    net_kernel:connect_node(ErlangNode).

%%% ###########################################################################
%%% coreRank_to_mpRole
%%%
%%% ###=====================================================================###
coreRank_to_mpRole(primary) ->
    core;
coreRank_to_mpRole(secondary) ->
    core;
coreRank_to_mpRole(_) ->
    regular.

%%% ###########################################################################
%%% coreRank_to_coreState
%%%
%%% ###=====================================================================###
coreRank_to_coreState(primary) ->
    active;
coreRank_to_coreState(secondary) ->
    standby;
coreRank_to_coreState(_) ->
    undefined.

%%% ###########################################################################
%%% c2erl_CoreRank
%%%
%%% ###=====================================================================###
c2erl_CoreRank(?CHI_CORE_RANK_PRIMARY, _) ->
    primary;
c2erl_CoreRank(?CHI_CORE_RANK_SECONDARY, _) ->
    secondary;
c2erl_CoreRank(?CHI_CORE_RANK_UNDEFINED, _) ->
    undefined;
c2erl_CoreRank(Value, From) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION} | sysUtil:pid_name(From)] ++
			      [{unrecognized_value, Value}]),
    throw({error, bad_parm}).

%%% ###########################################################################
%%% c2erl_RestartRank
%%%
%%% ###=====================================================================###
c2erl_RestartRank(?CHI_RESTART_RANK_WARM, _) ->
    warm;
c2erl_RestartRank(?CHI_RESTART_RANK_COLD, _) ->
    cold;
c2erl_RestartRank(?CHI_RESTART_RANK_COLD_WITH_TEST, _) ->
    cold_with_test;
c2erl_RestartRank(Value, From) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION} | sysUtil:pid_name(From)] ++
			      [{unrecognized_value, Value}]),
    throw({error, bad_parm}).

%%% ###########################################################################
%%% c2erl_RestartType
%%%
%%% ###=====================================================================###
c2erl_RestartType(?CSI_RESTART_TYPE_HARD, _) ->
    hard;
c2erl_RestartType(?CSI_RESTART_TYPE_SOFT, _) ->
    soft;
c2erl_RestartType(Value, From) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION} | sysUtil:pid_name(From)] ++
			      [{unrecognized_value, Value}]),
    throw({error, bad_parm}).

%%% ###########################################################################
%%% erl2c_ChiOpState
%%%
%%% ###=====================================================================###
erl2c_ChiOpState(disabled) ->
    ?CHI_OPSTATE_DISABLED;
erl2c_ChiOpState(enabled) ->
    ?CHI_OPSTATE_ENABLED;
erl2c_ChiOpState(_) ->
    -1.

%%% ###########################################################################
%%% erl2c_ChiResult
%%%
%%% ###=====================================================================###
erl2c_ChiResult(ok) ->
    ?CHI_RESULT_OK;
erl2c_ChiResult({error, no_exist}) ->
    ?CHI_RESULT_NO_EXIST;
erl2c_ChiResult({error, bad_parm}) ->
    ?CHI_RESULT_BAD_PARM;
erl2c_ChiResult(Value) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			       {unrecognized_value, Value}]),
    -1.

%%% ###########################################################################
%%% erl2c_CsiCoreState
%%%
%%% ###=====================================================================###
erl2c_CsiCoreState(active) ->
    ?CSI_CORE_STATE_ACTIVE;
erl2c_CsiCoreState(standby) ->
    ?CSI_CORE_STATE_STANDBY;
erl2c_CsiCoreState(undefined) ->
    ?CSI_CORE_STATE_UNDEFINED;
erl2c_CsiCoreState(_) ->
    -1.

%%% ###########################################################################
%%% erl2c_CsiResult
%%%
%%% ###=====================================================================###
erl2c_CsiResult(ok) ->
    ?CSI_OK;
erl2c_CsiResult({error, no_exist}) ->
    ?CSI_NO_EXIST;
erl2c_CsiResult(Value) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			       {unrecognized_value, Value}]),
    -1.

%%% ###########################################################################
%%% fun_activate
%%%
%%% ###=====================================================================###
fun_activate(#state{myConf = #clhMpConf{mpRole = undefined}} = S) ->
    S;
fun_activate(#state{mpId = MpId} = S) ->
    [MpState] = mnesia:read(clhMpState, MpId),
    NewMpState = MpState#clhMpState{opState = enabled},
    mnesia:write(NewMpState),
    S#state{myState = NewMpState}.

%%% ###########################################################################
%%% fun_associate_mp
%%%
%%% ###=====================================================================###
fun_associate_mp(MpId, FruId, CoreRank, S) ->
    ErlNode = clhI:make_erlang_node(MpId),
    MpRole = coreRank_to_mpRole(CoreRank),

    CoreState = coreRank_to_coreState(CoreRank),
    MatchHead = #clhMpConf{mpId = '$1',
			   fruId = clhI:fruId_to_binary(FruId),
			   coreRank = '$2',
			   _ = '_'},
    MatchResult = ['$1', '$2'],
    fun_associate_mp(mnesia:select(clhMpConf, [{MatchHead, [], [MatchResult]}]),
		     mnesia:read(clhMpConf, MpId),
		     MpId,
		     FruId,
		     CoreRank,
		     ErlNode,
		     MpRole,
		     CoreState,
		     S).

%%% ###=====================================================================###
fun_associate_mp([[MpId, CoreRank]],   % Both Match
		 _,
		 MpId,                 % Match
		 FruId,
		 CoreRank,             % Match
		 _,
		 _,
		 _,
		 S) ->
    %% No change!
    Info =
	[no_change,
	 {mpId, MpId},
	 {fruId, FruId},
	 {coreRank, CoreRank} |
	 ?STATE_INFO(S)],
    {{ok, S}, Info, {true, MpId}};
fun_associate_mp([[MpId, OldCoreRank]],   % MpId Match
		 _,
		 MpId,                    % Match
		 FruId,
		 CoreRank,
		 _,
		 _,
		 _,
		 S) ->
    %% TODO: Implement this case if required
    Info =
	[change_of_coreRank,
	 {mpId, MpId},
	 {fruId, FruId},
	 {coreRank, CoreRank},
	 {old_coreRank, OldCoreRank} |
	 ?STATE_INFO(S)],
    mnesia:abort({nok, {{error, bad_parm}, S}, Info});
fun_associate_mp([[OldMpId, _]],
		 [MpConf],
		 MpId,
		 FruId,
		 CoreRank,
		 _,
		 _,
		 _,
		 S) when OldMpId /= MpId ->
    %% TODO: Send a message to the CLH server to reinstall itself
    %% TODO: Mark the old MpId <-> FruId association as faulty
    %% TODO: Make sure the old data is erased at appropriate time
    Info =
	[{change_of_MpId, new_MpId_already_used},
	 {mpId, MpId},
	 {fruId, FruId},
	 {coreRank, CoreRank},
	 {old_MpConf, MpConf} |
	 ?STATE_INFO(S)],
    mnesia:abort({nok, {{error, bad_parm}, S}, Info});
fun_associate_mp([[OldMpId, _]],
		 [],
		 MpId,
		 FruId,
		 CoreRank,
		 _,
		 _,
		 _,
		 S) when OldMpId /= MpId ->
    %% TODO: Send a message to the CLH server to reinstall itself
    %% TODO: Mark the old MpId <-> FruId association as faulty
    %% TODO: Make sure the old data is erased at appropriate time
    Info =
	[{change_of_MpId, new_MpId_not_used},
	 {mpId, MpId},
	 {fruId, FruId},
	 {coreRank, CoreRank},
	 {old_MpId, OldMpId} |
	 ?STATE_INFO(S)],
    mnesia:abort({nok, {{error, bad_parm}, S}, Info});
fun_associate_mp(_,
		 _,
		 MpId,
		 FruId,
		 undefined,
		 _,
		 MpRole,
		 _,
		 S) when MpId == 1 ; MpId == 2 ->
    Info =
	[{coreRank_not_allowed_for_MpId, mpId_1_or_2_is_reserved_for_core},
	 {mpId, MpId},
	 {fruId, FruId},
	 {coreRank, undefined},
	 {mpRole, MpRole} |
	 ?STATE_INFO(S)],
    mnesia:abort({nok, {{error, bad_parm}, S}, Info});
fun_associate_mp([],   % No old FruId association found
		 [],   % The requested MpId is not used before
		 MpId,
		 FruId,
		 CoreRank,
		 ErlNode,
		 MpRole,
		 CoreState,
		 #state{mp_list = MpList} = S) ->
    Info =
	[fruId_associated_to_new_Mp,
	 {mpId, MpId},
	 {fruId, FruId},
	 {coreRank, CoreRank} |
	 ?STATE_INFO(S)],
    NewMpConf = #clhMpConf{mpId = MpId,
			   fruId = FruId,
			   erlNode = ErlNode,
			   mpRole = MpRole,
			   coreRank = CoreRank},
    NewMpState = #clhMpState{mpId = MpId,
			     coreState = CoreState},
    ok = mnesia:write(NewMpConf),
    ok = mnesia:write(NewMpState),
    %% NewMpList = [{sysEnv:get_mp_id(Node), Node} || Node <- nodes()],
    NewMpList = MpList ++ [{MpId, ErlNode}],
    NewS = update_my_state([NewMpConf, NewMpState, {mpList, NewMpList}], S),
    {{ok, NewS}, Info, {true, MpId}};
fun_associate_mp([],
		 [#clhMpConf{fruId = OldFruId} = MpConf],
		 MpId,
		 FruId,
		 CoreRank,
		 _,
		 MpRole,
		 _,
		 S) ->
    case ?FruId_default(MpId) of
	OldFruId ->
	    Info =
		[fruId_associated_to_existing_Mp,
		 {mpId, MpId},
		 {fruId, FruId},
		 {coreRank, CoreRank} |
		 ?STATE_INFO(S)],
	    NewMpConf = MpConf#clhMpConf{fruId = clhI:fruId_to_binary(FruId),
					 mpRole = MpRole,
					 coreRank = CoreRank},
	    ok = mnesia:write(NewMpConf),
	    NewS = update_my_state([NewMpConf], S),
	    {{ok, NewS}, Info, {true, MpId}};
	_ ->
	    Info =
		[{newFruId_association, mpId_associated_to_another_fruId},
		 {mpId, MpId},
		 {fruId, FruId},
		 {coreRank, CoreRank},
		 {old_MpConf, MpConf} |
		 ?STATE_INFO(S)],
	    mnesia:abort({nok, {{error, bad_parm}, S}, Info})
    end;
fun_associate_mp(FruId_Match,
		 MpConf,
		 MpId,
		 FruId,
		 CoreRank,
		 _,
		 _,
		 _,
		 S) ->
    Info =
	[unexpected_case,
	 {mpId, MpId},
	 {fruId, FruId},
	 {coreRank, CoreRank},
	 {fruId_Match, FruId_Match},
	 {old_Mp_data, MpConf} |
	 ?STATE_INFO(S)],
    mnesia:abort({nok, {{error, bad_parm}, S}, Info}).

%%% ###########################################################################
%%% fun_disassociate_mp
%%%
%%% ###=====================================================================###
fun_disassociate_mp(MpId, S) when MpId == 1 ->
    %% Should the primary MP be possible to delete?
    %% Only remove FruId at the moment.
    [Obj] = mnesia:read(clhMpConf, MpId),
    NewObj = Obj#clhMpConf{fruId = ?FruId_default(MpId)},
    mnesia:write(NewObj),
    {ok, S#state{myConf = NewObj}};
fun_disassociate_mp(MpId, #state{mp_list = MpList} = S) ->
    mnesia:delete({clhMpConf, MpId}),
    mnesia:delete({clhMpState, MpId}),
    {ok, S#state{mp_list = lists:keydelete(MpId, 1, MpList)}}.

%%% ###########################################################################
%%% fun_tbl_init_myself
%%%
%%% ###=====================================================================###
fun_tbl_init_myself(MpId) ->
    case mnesia:read(clhMpConf, MpId) of
	[MpConf] ->
	    ok;
	_ ->
	    MpConf = #clhMpConf{mpId = MpId,
				fruId = ?FruId_default(MpId),
				erlNode = clhI:make_erlang_node(MpId)},
	    mnesia:write(MpConf)
    end,
    reset_MpState(MpId),
    case mnesia:read(clhMpState, MpId) of
	[MpState] ->
	    ok;
	_ ->
	    MpState = #clhMpState{mpId = MpId},
	    mnesia:write(MpState)
    end,
    {MpConf, MpState}.

%%% ###=====================================================================###
reset_MpState(1) ->
    [begin
	 [Obj] = mnesia:read(clhMpState, Key),
	 mnesia:write(Obj#clhMpState{opState = disabled})
     end
     || Key <- mnesia:all_keys(clhMpState)];
reset_MpState(_) ->
    ok.

%%% ###########################################################################
%%% is_core_alive
%%%
%%% ###=====================================================================###
is_core_alive() ->
    is_core_alive(ets:tab2list(clhMpState)).

%%% ###=====================================================================###
is_core_alive([#clhMpState{opState = enabled, coreState = CoreState} | _])
  when CoreState /= undefined ->
    true;
is_core_alive([_ | Tail]) ->
    is_core_alive(Tail);
is_core_alive([]) ->
    false.

%%% ###########################################################################
%%% mnesia_table_event
%%%
%%% ###=====================================================================###
mnesia_table_event({write,
		    clhMpState,
		    #clhMpState{mpId = MpId,
				opState = NewOpState,
				coreState = NewCoreState},
		    [#clhMpState{mpId = MpId,
				 opState = OldOpState,
				 coreState = OldCoreState}],
		    _ActivityId},
		   S) ->
    subscr_opState_notify(MpId, NewOpState, OldOpState, S),
    subscr_coreState_notify(MpId, NewCoreState, OldCoreState, S),
    S;
mnesia_table_event({write,
		    clhMpState,
		    #clhMpState{mpId = MpId,
				opState = NewOpState,
				coreState = NewCoreState},
		    [],
		    _ActivityId},
		   S) ->
    subscr_opState_notify(MpId, NewOpState, disabled, S),
    subscr_coreState_notify(MpId, NewCoreState, undefined, S),
    S;
mnesia_table_event({delete,
		    clhMpState,
		    {clhMpState, MpId},
		    [#clhMpState{mpId = MpId,
				 opState = OldOpState,
				 coreState = OldCoreState}],
		    _ActivityId},
		   S) ->
    subscr_opState_notify(MpId, disabled, OldOpState, S),
    subscr_coreState_notify(MpId, undefined, OldCoreState, S),
    S;
mnesia_table_event(_, S) ->
    S.

%%% ###########################################################################
%%% mnesia_table_event_log
%%%
%%% ###=====================================================================###
mnesia_table_event_log({write, _Tbl, NewObj, [OldObj], _} = Event,
		       S) when NewObj /= OldObj ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			      Event |
			      ?STATE_INFO(S)]);
mnesia_table_event_log({write, _Tbl, _NewObj, [], _} = Event,
		       S) ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			      Event |
			      ?STATE_INFO(S)]);
mnesia_table_event_log({delete, _Tbl, _TblAndKey, [], _}, _S) ->
    ok;
mnesia_table_event_log({delete, _Tbl, _TblAndKey, _OldObjs, _} = Event,
		       S) ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			      Event |
			      ?STATE_INFO(S)]);
mnesia_table_event_log(_, _) ->
    ok.

%%% ###########################################################################
%%% msg_CHI_ASSOCIATE_MP
%%%
%%% ###=====================================================================###
msg_CHI_ASSOCIATE_MP(ClientPid, MpId, FruId, CoreRank, S) ->
    {Result, NewS} =
	transaction_associate_mp(MpId, FruId, CoreRank, ClientPid, S),
    {erl2c_ChiResult(Result), NewS}.

%%% ###########################################################################
%%% msg_CHI_DISASSOCIATE_MP
%%%
%%% ###=====================================================================###
msg_CHI_DISASSOCIATE_MP(ClientPid, MpId, S) ->
    {Result, NewS} = transaction_disassociate_mp(MpId, ClientPid, S),
    {erl2c_ChiResult(Result), NewS}.

%%% ###########################################################################
%%% msg_CHI_CLUSTER_RESTART
%%%
%%% ###=====================================================================###
msg_CHI_CLUSTER_RESTART(Socket,
			ClientPid,
			RestartType,
			RestartRank,
			RestartCause) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(ClientPid)] ++
	[{restartType, RestartType},
	 {restartRank, RestartRank},
	 {restartCause, RestartCause}],
    sysInitI:info_report(Info),
    gen_tcp:send(Socket, <<?CHI_RESULT_OK:4/native-unsigned-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),

    gen_server:cast(?SERVER, {cluster_restart, {RestartType, RestartRank, RestartCause}}).

%%     cluster_restart_inform_subscribers(RestartType),

%%     appmI:restart_node(RestartType, RestartRank, RestartCause).

%%% ###########################################################################
%%% msg_CHI_SUBSCRIBE_OP_STATE
%%%
%%% ###=====================================================================###
msg_CHI_SUBSCRIBE_OP_STATE(ClientPid, MpId, MboxId, S) ->
    PidName = sysUtil:pid_name(ClientPid),
    Info =
	[{?MODULE, ?FUNCTION} | PidName] ++
	[{mpId, MpId},
	 {mboxId, MboxId},
	 {subscrs, S#state.subscrs_opState}],
    result_info(put, Info),
    case clhI:op_state(MpId) of
	OpState when OpState == enabled orelse OpState == disabled ->
	    NewS = subscr_opState_add(MpId, MboxId, PidName, S),
	    Result = ok;
	OpState ->
	    NewS = S,
	    Result = {error, no_exist}
    end,
    {erl2c_ChiResult(Result), erl2c_ChiOpState(OpState), NewS}.

%%% ###########################################################################
%%% msg_CHI_UNSUBSCRIBE_OP_STATE
%%%
%%% ###=====================================================================###
msg_CHI_UNSUBSCRIBE_OP_STATE(ClientPid, MpId, MboxId, S) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(ClientPid)] ++
	[{mpId, MpId}],
    result_info(put, Info),
    NewS = subscr_opState_remove(MpId, MboxId, S),
    Result = ok,
    {erl2c_ChiResult(Result), NewS}.

%%% ###########################################################################
%%% msg_CSI_SUBSCRIBE_CORE_STATE
%%%
%%% ###=====================================================================###
msg_CSI_SUBSCRIBE_CORE_STATE(ClientPid, MpId, MboxId, S) ->
    PidName = sysUtil:pid_name(ClientPid),
    Info =
	[{?MODULE, ?FUNCTION} | PidName] ++
	[{mpId, MpId},
	 {mboxId, MboxId},
	 {subscrs, S#state.subscrs_coreState}],
    sysInitI:info_report(Info),
    case clhI:mp_role(MpId) of
	Role when Role /= undefined ->
	    NewS = subscr_coreState_add(MpId, MboxId, PidName, S),
	    Result = ok;
	_ ->
	    NewS = S,
	    Result = {error, no_exist}
    end,
    CoreState = clhI:core_state(MpId),
    {erl2c_CsiResult(Result), erl2c_CsiCoreState(CoreState), NewS}.

%%% ###########################################################################
%%% msg_CSI_UNSUBSCRIBE_CORE_STATE
%%%
%%% ###=====================================================================###
msg_CSI_UNSUBSCRIBE_CORE_STATE(ClientPid, MpId, MboxId, S) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(ClientPid)] ++
	[{mpId, MpId}],
    sysInitI:info_report(Info),
    NewS = subscr_coreState_remove(MpId, MboxId, S),
    Result = ok,
    {erl2c_ChiResult(Result), NewS}.

%%% ###########################################################################
%%% msg_CSI_SUBSCRIBE_CLUSTER_RESTART
%%%
%%% ###=====================================================================###
msg_CSI_SUBSCRIBE_CLUSTER_RESTART(ClientPid, MboxId, S) ->
    PidName = sysUtil:pid_name(ClientPid),
    Info =
	[{?MODULE, ?FUNCTION} | PidName] ++
	[{mboxId, MboxId},
	 {subscrs, S#state.subscrs_opState}],
    result_info(put, Info),
    NewS = subscr_clusterRestart_add(undefined, MboxId, PidName, S),
    Result = ok,
    {erl2c_ChiResult(Result), NewS}.

%%% ###########################################################################
%%% msg_CSI_UNSUBSCRIBE_CLUSTER_RESTART
%%%
%%% ###=====================================================================###
msg_CSI_UNSUBSCRIBE_CLUSTER_RESTART(ClientPid, MboxId, S) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(ClientPid)] ++
	[],
    result_info(put, Info),
    NewS = subscr_clusterRestart_remove(undefined, MboxId, S),
    Result = ok,
    {erl2c_ChiResult(Result), NewS}.

%%% ###########################################################################
%%% msg_CSI_CLUSTER_RESTART_REPLY
%%%
%%% ###=====================================================================###
msg_CSI_CLUSTER_RESTART_REPLY(ClientPid, MboxId, S) ->
    Info =
	[{?MODULE, ?FUNCTION} | sysUtil:pid_name(ClientPid)] ++
	[],
    result_info(put, Info),
    NewS = clusterRestart_reply(undefined, MboxId, S),
    Result = ok,
    {erl2c_ChiResult(Result), NewS}.

%%% ###########################################################################
%%% nl_file_config_info
%%%
%%% ###=====================================================================###
nl_file_config_info() ->
    RecordTags = record_info(fields, clhMpConf),
    [begin
	 [_RecordName | RecordValues] = tuple_to_list(Obj),
	 {Obj#clhMpConf.mpId, sysUtil:pairElems(RecordTags, RecordValues)}
     end
     || Obj <- ets:tab2list(clhMpConf)].

%%% ###########################################################################
%%% nl_file_write
%%%
%%% ###=====================================================================###
nl_file_write(FileName, Content) ->
    File = filename:join(?DIR_HOME, FileName),
    Bytes = erlang:term_to_binary(Content),
    case file:write_file(File, Bytes) of
	ok ->
	    ok;
	Error ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				       {file, File},
				       {content, Bytes},
				       Error]),
	    Error
    end.

%%% ###########################################################################
%%% set_opState
%%%
%%% ###=====================================================================###
set_opState(MpId, OpState) ->
    mnesia:transaction(fun() ->
			       [MpState] = mnesia:read(clhMpState, MpId),
			       NewMpState =
				   MpState#clhMpState{opState = OpState},
			       mnesia:write(NewMpState)
		       end).

%%% ###########################################################################
%%% subscr_add
%%%
%%% ###=====================================================================###
subscr_add([{Key, Subscrs} | Tail], Key, Mbox) ->
    [{Key, [Mbox | Subscrs]} | Tail];
subscr_add([Other | Tail], Key, Mbox) ->
    [Other | subscr_add(Tail, Key, Mbox)];
subscr_add([], Key, Mbox) ->
    [{Key, [Mbox]}].

%%% ###########################################################################
%%% subscr_coreState_add
%%%
%%% ###=====================================================================###
subscr_coreState_add(MpId,
		     MboxId,
		     PidName,
		     #state{subscrs_coreState = Subscrs} = S) ->
    Mbox = #mbox{id = MboxId,
		 monRef = itc:attach(S#state.itc_port, MboxId),
		 pid_name = PidName},
    S#state{subscrs_coreState = subscr_add(Subscrs, MpId, Mbox)}.

%%% ###########################################################################
%%% subscr_coreState_get
%%%
%%% ###=====================================================================###
subscr_coreState_get(MpId, #state{subscrs_coreState = Subscrs}) ->
    subscr_get(Subscrs, MpId).

%%% ###########################################################################
%%% subscr_coreState_notify
%%%
%%% ###=====================================================================###
subscr_coreState_notify(MpId, NewCoreState, OldCoreState, S)
  when NewCoreState/= OldCoreState ->
    subscr_coreState_notify(MpId, NewCoreState, S);
subscr_coreState_notify(_, _, _, _) ->
    ok.

%%% ###=====================================================================###
subscr_coreState_notify(MpId, CCoreState, S) when is_integer(CCoreState) ->
    subscr_coreState_notify_send(subscr_coreState_get(MpId, S),
				 MpId,
				 CCoreState,
				 S);
subscr_coreState_notify(MpId, CoreState, S) ->
    subscr_coreState_notify(MpId, erl2c_CsiCoreState(CoreState), S).

%%% ###=====================================================================###
subscr_coreState_notify_send([#mbox{id = MboxId} | Tail],
			     MpId,
			     CCoreState,
			     S) ->
    Data =
	<<MpId:4/native-unsigned-integer-unit:8,
	 CCoreState:4/native-unsigned-integer-unit:8>>,
    itc:send(S#state.itc_port, MboxId, ?CSI_CORE_STATE_CHANGE_IND, Data),
    subscr_coreState_notify_send(Tail, MpId, CCoreState, S);
subscr_coreState_notify_send([], _, _, _) ->
    ok.

%%% ###########################################################################
%%% subscr_coreState_remove
%%%
%%% ###=====================================================================###
subscr_coreState_remove(MpId, MboxId, #state{subscrs_coreState = Subscrs} =S) ->
    S#state{subscrs_coreState = subscr_remove(Subscrs, MpId, MboxId, S)}.

%%% ###########################################################################
%%% subscr_get
%%%
%%% ###=====================================================================###
subscr_get([{Key, Subscrs} | _], Key) ->
    Subscrs;
subscr_get([_ | Tail], Key) ->
    subscr_get(Tail, Key);
subscr_get([], _) ->
    [].

%%% ###########################################################################
%%% subscr_opState_add
%%%
%%% ###=====================================================================###
subscr_opState_add(MpId,
		   MboxId,
		   PidName,
		   #state{subscrs_opState = Subscrs} = S) ->
    Mbox = #mbox{id = MboxId,
		 monRef = itc:attach(S#state.itc_port, MboxId),
		 pid_name = PidName},
    S#state{subscrs_opState = subscr_add(Subscrs, MpId, Mbox)}.

%%% ###########################################################################
%%% subscr_opState_get
%%%
%%% ###=====================================================================###
subscr_opState_get(MpId, #state{subscrs_opState = Subscrs}) ->
    subscr_get(Subscrs, MpId).

%%% ###########################################################################
%%% subscr_opState_notify
%%%
%%% ###=====================================================================###
subscr_opState_notify(MpId, NewOpState, OldOpState, S)
  when NewOpState /= OldOpState ->
    subscr_opState_notify(MpId, NewOpState, S);
subscr_opState_notify(_, _, _, _) ->
    ok.

%%% ###=====================================================================###
subscr_opState_notify(MpId, COpState, S) when is_integer(COpState) ->
    subscr_opState_notify_send(subscr_opState_get(MpId, S),
			       MpId,
			       COpState,
			       S);
subscr_opState_notify(MpId, OpState, S) ->
    subscr_opState_notify(MpId, erl2c_ChiOpState(OpState), S).

%%% ###=====================================================================###
subscr_opState_notify_send([#mbox{id = MboxId} | Tail], MpId, COpState, S) ->
    Data =
	<<MpId:4/native-unsigned-integer-unit:8,
	 COpState:4/native-unsigned-integer-unit:8>>,
    itc:send(S#state.itc_port, MboxId, ?CHI_OP_STATE_CHANGE_IND, Data),
    subscr_opState_notify_send(Tail, MpId, COpState, S);
subscr_opState_notify_send([], _, _, _) ->
    ok.

%%% ###########################################################################
%%% subscr_opState_remove
%%%
%%% ###=====================================================================###
subscr_opState_remove(MpId, MboxId, #state{subscrs_opState = Subscrs} = S) ->
    S#state{subscrs_opState = subscr_remove(Subscrs, MpId, MboxId, S)}.

%%% ###########################################################################
%%% subscr_clusterRestart_add
%%%
%%% ###=====================================================================###
subscr_clusterRestart_add(_MpId,
			  MboxId,
			  PidName,
			  #state{subscrs_clusterRestart = Subscrs,
				 itc_port               = ItcPort} = S) ->
    Mbox = #mbox{id       = MboxId,
		 monRef   = itc:attach(ItcPort, MboxId),
		 pid_name = PidName},
    case lists:keysearch(MboxId, 2, Subscrs) of
	false -> S#state{subscrs_clusterRestart = [Mbox | Subscrs]};
	_     -> S
    end.

%%% ###########################################################################
%%% subscr_clusterRestart_notify
%%%
%%% No MpId exists in cluster restart,
%%% MboxId is used instead as the application identity
%%% ###=====================================================================###
subscr_clusterRestart_notify(Mboxes, ItcPort) ->
    [subscr_clusterRestart_notify_send(Mbox, ItcPort) || Mbox <- Mboxes].

%%% ###=====================================================================###
subscr_clusterRestart_notify_send(#mbox{id = MboxId}, ItcPort) ->
    Type = ?CSI_CLUSTER_RESTART_SOFT,
    Data = <<Type:4/native-unsigned-integer-unit:8>>,
    itc:send(ItcPort, MboxId, ?CSI_CLUSTER_RESTART_IND, Data).

%%% ###########################################################################
%%% subscr_clusterRestart_remove
%%%
%%% No MpId exists in cluster restart,
%%% MboxId is used instead as the application identity
%%% ###=====================================================================###
subscr_clusterRestart_remove(_MpId,
			     MboxId,
			     #state{subscrs_clusterRestart = Subscrs} = S) ->
    NewSubscrs = lists:keydelete(MboxId, 2, Subscrs),
    S#state{subscrs_clusterRestart = NewSubscrs}.

%%% ###########################################################################
%%% subscr_clusterRestart_remove
%%%
%%% No MpId exists in cluster restart,
%%% MboxId is used instead as the application identity
%%% ###=====================================================================###
clusterRestart_reply(_MpId,
		     MboxId,
		     #state{waiting_subscrs_clusterRestart = Subscrs,
			    waiting_nodes_clusterRestart   = Nodes,
			    args_clusterRestart = {RestartType, 
						   RestartRank, 
						   RestartCause},
			    timer_ref_clusterRestart = TimerRef,
			    core_node_clusterRestart = CoreNode} = S) ->

    
    
    case {lists:keydelete(MboxId, 2, Subscrs), Nodes, CoreNode} of
	%% Active core, request node restart
	{[], [], undefined} ->
	    cancel_timer(TimerRef),
	    appmI:restart_node(RestartType, RestartRank, RestartCause),
	    S#state{waiting_subscrs_clusterRestart = [],
		    args_clusterRestart            = undefined,
		    timer_ref_clusterRestart       = undefined};
	%% Regular node, inform active core
	{[], [], _} ->
	    cancel_timer(TimerRef),
	    rpc:cast(CoreNode, 
		     clh_csi_service, 
		     soft_cluster_restart_reply,
		     [clhI:erlang_node()]),
	    S#state{waiting_subscrs_clusterRestart = [],
		    args_clusterRestart            = undefined,
		    timer_ref_clusterRestart       = undefined, 
		    core_node_clusterRestart       = undefined};
	%% Still waiting on other apps
	{NewSubscrs, _, _} ->
	    S#state{waiting_subscrs_clusterRestart = NewSubscrs}
    end.






%%% ###########################################################################
%%% subscr_remove
%%%
%%% ###=====================================================================###
subscr_remove([{Key, Subscrs} | Tail], Key, MboxId, S) ->
    case lists:keyfind(MboxId, #mbox.id, Subscrs) of
	#mbox{monRef = MonRef} ->
	    itc:detach(S#state.itc_port, MonRef);
	_ ->
	    ok
    end,
    [{Key, lists:keydelete(MboxId, #mbox.id, Subscrs)} | Tail];
subscr_remove([Other | Tail], Key, MboxId, S) ->
    [Other | subscr_remove(Tail, Key, MboxId, S)];
subscr_remove([], _, _, _) ->
    [].

%%% ###=====================================================================###
subscr_remove_all([{Key, Subscrs} | Tail], MboxId) when is_integer(MboxId) ->
    [{Key, lists:keydelete(MboxId, #mbox.id, Subscrs)} |
     subscr_remove_all(Tail, MboxId)];
subscr_remove_all([], _) ->
    [];
subscr_remove_all(MboxId, #state{} = S) when is_integer(MboxId) ->
    New_subscrs_opState =
	subscr_remove_all(S#state.subscrs_opState, MboxId),
    New_subscrs_coreState =
	subscr_remove_all(S#state.subscrs_coreState, MboxId),
    S#state{subscrs_opState = New_subscrs_opState,
	    subscrs_coreState = New_subscrs_coreState}.

%%% ###########################################################################
%%% transaction_associate_mp
%%%
%%% ###=====================================================================###
transaction_associate_mp(MpId, FruId, CoreRank, From, S)
  when CoreRank == primary orelse
       CoreRank == secondary orelse
       CoreRank == undefined ->
    InfoHeader = [{?MODULE, ?FUNCTION} | sysUtil:pid_name(From)],
    case
	mnesia:transaction(fun() ->
				   fun_associate_mp(MpId, FruId, CoreRank, S)
			   end)
	of
	{atomic, {Return, Info, NewMp}} ->
	    case CoreRank of
		undefined when MpId =/= undefined->
		    sysDbServer:copy_mnesia_content(clhI:erlang_node(MpId));
		_ ->
		    ok
	    end,
	    nl_file_write(?FILENAME_ClusterConfig, nl_file_config_info()),
	    trigger_start_mp(NewMp),
	    add_dhcp(NewMp),
	    result_info(put, InfoHeader ++ Info);
	{aborted, {nok, Return, Info}} ->
	    result_info(put, InfoHeader),
	    sysInitI:warning_report(InfoHeader ++ Info);
	{aborted, Reason} ->
	    Return = {{error, bad_parm}, S},
	    Info =
		[transaction_error,
		 {reason, Reason},
		 {mpId, MpId},
		 {fruId, FruId},
		 {coreRank, CoreRank} |
		 ?STATE_INFO(S)],
	    result_info(put, InfoHeader),
	    sysInitI:error_report(InfoHeader ++ Info)
    end,
    Return;
transaction_associate_mp(MpId, FruId, CoreRank, From, S) ->
    InfoHeader = [{?MODULE, ?FUNCTION} | sysUtil:pid_name(From)],
    Info =
	InfoHeader ++
	[{{unrecognized_value, 'CoreRank'}, CoreRank},
	 {mpId, MpId},
	 {fruId, FruId}],
    result_info(put, InfoHeader),
    sysInitI:warning_report(Info),
    {{error, bad_parm}, S}.

%%% ###########################################################################
%%% transaction_disassociate_mp
%%%
%%% ###=====================================================================###
transaction_disassociate_mp(MpId, From, #state{mp_list = MpList} = S) ->
    InfoHeader = [{?MODULE, ?FUNCTION} | sysUtil:pid_name(From)],
    Info = [{mpId, MpId} | ?STATE_INFO(S)],
    result_info(put, InfoHeader ++ Info),
    {atomic, Return} =
	mnesia:transaction(fun() ->
				   fun_disassociate_mp(MpId, S)
			   end),
    nl_file_write(?FILENAME_ClusterConfig, nl_file_config_info()),
    remove_dhcp(MpId),
    case lists:keyfind(MpId, 1, MpList) of
	{1, _Node} ->
	    sysInitI:warning_report(InfoHeader 
				    ++ [disassociate_mp_on_core, {mpId, MpId}]);
	{MpId, Node} ->
	    case sysEnv:architecture() of
		{"arm",_} ->
		    sysInitI:info_report(InfoHeader ++ Info),
		    sysInitI:info_msg("clh_csi_service:transaction_disassociate_mp"
				      "~n#################################################~n"
				      "########## Revert to networkloader ############~n", []),
		    rpc:call(Node, ?MODULE, revert_to_nl, []);
		{Arch,_} ->
		    Info2 =
			InfoHeader ++ Info ++
			[{{no_sup_arch, 'Arch'}, Arch}],
		    sysInitI:info_report(Info2)
		    %% rpc:call(Node, ?MODULE, mnesia_stop, []),
		    %% erlang:disconnect_node(Node)
	    end;
	false ->
	    sysInitI:warning_report(InfoHeader ++
					[mpId_not_found, {mpId, MpId}])
    end,
    Return.

revert_to_nl() ->
    sysRhai:setbootptr(nl),
    catch aicI:clean_up(soft),
    swmI:force_auto_backup(),
    appmI:restart_own_piu(cold, false, atom_to_list(soft)).

%%% ###########################################################################
%%% trigger_start_mp
%%%
%%% ###=====================================================================###
trigger_start_mp(false) ->
    ok;
trigger_start_mp({true, 1}) ->
    ok;
trigger_start_mp({true, MpId}) ->
    HomeDir = sysEnv:home_dir(),
    case filelib:is_file(filename:join(HomeDir, "cluster_complete")) of
	true ->
	    %% The regular node has already been created once
	    %% TODO: Check if we really need to do this here...
	    net_kernel:connect_node(clhI:make_erlang_node(MpId));
	false ->
	    %% Inital startup
	    cmd(["touch ", HomeDir, "/cluster_complete"])
    end.

%%% ###########################################################################
%%% update_my_state
%%%
%%% ###=====================================================================###
update_my_state([#clhMpConf{mpId = MyMpId} = MpConf | Tail],
		#state{mpId = MyMpId} = S) ->
    update_my_state(Tail, S#state{myConf = MpConf});
update_my_state([#clhMpState{mpId = MyMpId} = MpState | Tail],
		#state{mpId = MyMpId} = S) ->
    update_my_state(Tail, S#state{myState = MpState});
update_my_state([{mpList, MpList} | Tail], S) ->
    update_my_state(Tail, S#state{mp_list = MpList});
update_my_state([_ | Tail], S) ->
    update_my_state(Tail, S);
update_my_state([], S) ->
    S.

add_dhcp(false) ->
    ok;
add_dhcp({true, MpId}) ->
    ClientId = mpId_to_clientId(MpId),
    IpAddress = clhI:ip_address(MpId),
    sysDhcpd:add_client(ClientId, IpAddress).

remove_dhcp(MpId) ->
    ClientId = mpId_to_clientId(MpId),
    sysDhcpd:remove_client(ClientId).

mpId_to_clientId(MpId) ->
    iolist_to_binary(io_lib:format("~3.10.0b", [MpId])).



%%% ###########################################################################
%%% cluster_restart_inform_subscribers
%%%
%%% ###=====================================================================###
%% Hard restart. Just do as requested.
cluster_restart_inform_subscribers({hard, _, _} = Data,
				   _,
				   State) ->
    do_cluster_restart(Data, State);
%% Soft restart, no subscribers. Do restart.
cluster_restart_inform_subscribers({soft, _, _} = Data,
				   [],
				   #state{subscrs_clusterRestart = []} = State) ->
    do_cluster_restart(Data, State);
%% Soft restart already ongoing. Ignore request.
cluster_restart_inform_subscribers({soft, _, _},
				   _,
				   #state{timer_ref_clusterRestart = TimerRef
					 } = State)
  when TimerRef =/= undefined ->
    State;
%% Request soft restart.
cluster_restart_inform_subscribers({soft, _, _} = Data,
				   Nodes,
				   #state{subscrs_clusterRestart = Subscrs,
					  itc_port               = ItcPort
					 } = State) ->
    subscr_clusterRestart_notify(Subscrs, ItcPort),
    nodes_clusterRestart_notify(Nodes),
    TimerRef = timer:send_after(?CLUSTER_RESTART_TO, cluster_restart_to),
    State#state{waiting_subscrs_clusterRestart = Subscrs,
		waiting_nodes_clusterRestart   = Nodes,
		args_clusterRestart            = Data,
	        timer_ref_clusterRestart       = TimerRef}.



nodes_clusterRestart_notify(Nodes) ->
    [rpc:cast(Node, 
	      clh_csi_service, 
	      soft_cluster_restart_request,
	      [clhI:erlang_node()]) || Node <- Nodes].



do_cluster_restart({RestartType, RestartRank, RestartCause},
		   #state{timer_ref_clusterRestart = TimerRef} = State) ->
    cancel_timer(TimerRef),
    appmI:restart_node(RestartType, RestartRank, RestartCause),
    State#state{waiting_subscrs_clusterRestart = [],
		waiting_nodes_clusterRestart   = [],
		args_clusterRestart            = undefined,
		timer_ref_clusterRestart       = undefined}.


cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    timer:cancel(TimerRef).



%%% ###########################################################################
%%% soft_cluster_restart_request
%%%
%%% This is called from the active core node.
%%% The current node is a regular node.
%%% ###=====================================================================###
soft_cluster_restart_request(CoreNode,
			     #state{subscrs_clusterRestart = []} = State) ->
    rpc:cast(CoreNode, 
	     clh_csi_service, 
	     soft_cluster_restart_reply,
	     [clhI:erlang_node()]),
    State#state{waiting_subscrs_clusterRestart = [],
		core_node_clusterRestart       = undefined,
	        timer_ref_clusterRestart       = undefined};
soft_cluster_restart_request(CoreNode,
			     #state{subscrs_clusterRestart = Subscrs,
				    itc_port               = ItcPort
				   } = State) ->
    subscr_clusterRestart_notify(Subscrs, ItcPort),
    State#state{waiting_subscrs_clusterRestart = Subscrs,
		core_node_clusterRestart       = CoreNode}.


%%% ###########################################################################
%%% soft_cluster_restart_reply
%%%
%%% 
%%% 
%%% ###=====================================================================###
soft_cluster_restart_reply(Node, 
			   #state{waiting_subscrs_clusterRestart = Subscrs,
				  waiting_nodes_clusterRestart   = Nodes,
				  args_clusterRestart      = {RestartType, 
							      RestartRank, 
							      RestartCause},
				  timer_ref_clusterRestart = TimerRef
				 } = State) ->
    case {Nodes -- [Node], Subscrs}  of
	{[], []} ->
	    cancel_timer(TimerRef),
	    appmI:restart_node(RestartType, RestartRank, RestartCause),
	    State#state{waiting_subscrs_clusterRestart = [],
			args_clusterRestart            = undefined,
			timer_ref_clusterRestart       = undefined};
	{NewNodes, _} ->
	    State#state{waiting_nodes_clusterRestart = NewNodes}
    end.

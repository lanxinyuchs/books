%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_sftp_client.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R4A/3
%%%
%%% @doc == SFTP Client CT Hook ==
%%% This module provides a simplified and limited sftp client API. 
%%%
%%% The default target is a CS node but the client may be configured to 
%%% connect to any sftp server. 
%%% The module is implemented as a ct_hook in order to facilitate its use in
%%% Common Test suites. The ct_hook determines what target the client
%%% should connect to and sets up IP address and port number accordingly.
%%%
%%% The API functions in this module corresponds to functions in the
%%% OTP module ssh_sftp. See OTP documentation for a detailed description
%%% of these functions.
%%%
%%% The `rct_sftp_client' hook is specified in the `suite/0' function of the 
%%% test suite as described below:
%%%
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_sftp_client, Opts}]}].
%%%
%%%    Opts = [Opt]
%%%    Opt = {du_no, No} | 
%%%          {sim_sname, SName} | 
%%%          {instance_name, InstanceName} | 
%%%          {ip, IpAddress} | 
%%%          {port, PortNumber} | 
%%%          {user, User} | 
%%%          {password, Password} | 
%%%          {start_channel_per_tc, SCpTCFlag} | 
%%%          {stop_channel_on_fail, SCoFFlag}'''
%%%
%%%  `No = integer()' <br/>
%%%    - Used to indicate testnode (DU) number when testing towards a node in
%%%      a cluster. Note that only the active node runs sftp.<br/>
%%%      Default is 1
%%%
%%%  `SName = atom()' <br/>
%%%    - Used to provide a different sim node name than the default 
%%%      when running tests towards a simulator.<br/>
%%%      Default is `$USER' <br/>
%%%
%%%  `InstanceName = atom()' <br/>
%%%    - Used to provide an identity of the CTH instance. Also used as the 
%%%      registered name of the server handling the connection towards the test 
%%%      node. The naming is useful when running multinode tests. 
%%%      See example below.<br/>
%%%      Default is `rct_sftp_client' <br/>
%%%
%%%  `IpAddress = string()' <br/>
%%%    - Used when connecting to another node than a CS node. May also be
%%%      used to override the default IP address in CS tests.
%%%
%%%  `PortNumber = integer()' <br/>
%%%    - Used to override the default port number.<br/>
%%%      Default port is 2024 in CS target and 22 in non CS target.
%%%      In a simulated CS environment the port number is fetched from
%%%      the CS node.
%%%
%%%  `User = string()' <br/>
%%%    - User name for the sftp server. 
%%%
%%%  `Password = string()' <br/>
%%%    - Password for the sftp server. 
%%%
%%%  `SCpTCFlag = boolean()' <br/>
%%%    - Determines if the sftp channel should be automatically started before, 
%%%      and stopped after, each test case. <br/>
%%%      Default is `true'<br/>
%%%
%%%  `SCoFFlag = boolean()' <br/>
%%%    - Determines if the sftp channel should be automatically stopped at 
%%%      test case failure. Note that this flag is only relevant in case 
%%%      the `SCpTCFlag' is set to `false'. <br/>
%%%      Default is `false'<br/>
%%%
%%% All parameters described above are optional.
%%%
%%% In a multinode scenario several instances may be specified using different
%%% instance names as in this example.
%%%
%%% ```suite() -> 
%%%     [{ct_hooks, [{rct_sftp_client, [{instance_name, sftp_1},
%%%                                     {du_no, 1}]},
%%%                  {rct_sftp_client, [{instance_name, sftp_2}
%%%                                     {du_no, 2}]}].'''
%%%
%%% @end
%%% ----------------------------------------------------------
-module(rct_sftp_client).
-vsn('/main/R2A/R4A/3').
-date('2016-03-08').
-author('etxkols').
-shaid('f1bc62d29802c1bc8c6c524977164e4b49129734').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R2A/1      2013-08-23 eolaand     Created
%%% R2A/3      2014-08-13 eolaand     Make sure to close the connection and not
%%%                                   just the channel.
%%% R4A/3      2016-03-08 etxkols     5G
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API
-export([start_channel/0,
	 start_channel/1,
	 stop_channel/0,
	 stop_channel/1,
	 list_dir/1,
	 list_dir/2,
	 read_file/1,
	 read_file/2,
	 read_files/1,
	 read_files/2,
	 write_file/2,
	 write_file/3,
	 delete_file/1,
	 delete_file/2,
	 delete_files/1,
	 delete_files/2]).

%% ct_hooks callbacks
-export([id/1, 
	 init/2, 
	 pre_init_per_suite/3, 
	 pre_init_per_testcase/3, 
	 post_end_per_testcase/4, 
	 on_tc_fail/3,
	 terminate/1]).

%% Server API
-export([start/0, 
	 start/1, 
	 stop/0, 
	 stop/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% Debug
-export([dump/0, 
	 dump/1,
	 get_ch_pid/0,
	 get_ch_pid/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SERVER, ?MODULE).
-define(DEFAULT_USER, "expert").
-define(DEFAULT_PWD, "expert").
-define(DEFAULT_TARGET_SFTP_PORT, 2024).
-define(DEFAULT_IP_TYPE, ssh_lmt_ipv4).
-define(DEFAULT_DU_NO, 1).

%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------
-record(cth_state, {name,
		    opts,
		    start_channel_per_tc,
		    stop_channel_on_fail}).

-record(state, {server,
		test_env, 
		ip, 
		port, 
		user, 
		password, 
		ch_pid,
		conn_ref,
		cs_sim_node,
		opts}).
 
%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type server() :: atom() | pid().
-type file_data() :: {FileName::string, Data::binary()}.

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%%===================================================================
%%% API
%%%===================================================================
%% @equiv start_channel(rct_sftp_client)
-spec start_channel() -> ok | {error, Reason::term()}.
start_channel() ->
    start_channel(?SERVER).


-spec start_channel(Server::server()) -> ok | {error, Reason::term()}.
start_channel(Server) ->
    call(Server, start_channel).


%% @equiv stop_channel(rct_sftp_client)
-spec stop_channel() -> ok | {error, Reason::term()}.
stop_channel() ->
    stop_channel(?SERVER).


-spec stop_channel(Server::server()) -> ok | {error, Reason::term()}.
stop_channel(Server) ->
    call(Server, stop_channel).


%% @equiv list_dir(rct_sftp_client, Path)
-spec list_dir(Path::string()) -> 
		      {ok, [FileName::string()]} | {error, Reason::term()}.
list_dir(Path) ->
    list_dir(?SERVER, Path).


-spec list_dir(Server::server(), Path::string()) -> 
		      {ok, [FileName::string()]} | {error, Reason::term()}.
list_dir(Server, Path) ->
    call(Server, {list_dir, Path}).
    

%% @equiv read_file(rct_sftp_client, FilePath)
-spec read_file(FilePath::string()) -> 
		       {ok, Data::binary()} | {error, Reason::term()}.
read_file(FilePath) ->
    read_file(?SERVER, FilePath).


-spec read_file(Server::server(), FilePath::string()) -> 
		       {ok, Data::binary()} | {error, Reason::term()}.
read_file(Server, FilePath) ->
    call(Server, {read_file, FilePath}).
    

%% @equiv read_files(rct_sftp_client, Path)
-spec read_files(Path::string()) -> 
			{ok, [file_data()]} | {error, Reason::term()}.
read_files(Path) ->
    read_files(?SERVER, Path).


-spec read_files(Server::server(), Path::string()) -> 
			{ok, [file_data()]} | {error, Reason::term()}.
read_files(Server, Path) ->
    call(Server, {read_files, Path}).
    

%% @equiv write_file(rct_sftp_client, FilePath)
-spec write_file(FilePath::string(), IoList::iolist()) ->  
			ok | {error, Reason::term()}.
write_file(FilePath, IoList) ->
    write_file(?SERVER, FilePath, IoList).


-spec write_file(Server::server(), FilePath::string(), IoList::iolist()) ->  
			ok | {error, Reason::term()}.
write_file(Server, FilePath, IoList) ->
    call(Server, {write_file, {FilePath, IoList}}).
    

%% @equiv delete_file(rct_sftp_client, FilePath)
-spec delete_file(FilePath::string()) ->  
			 ok | {error, Reason::term()}.
delete_file(FilePath) ->
    delete_file(?SERVER, FilePath).


-spec delete_file(Server::server(), FilePath::string()) ->  
			 ok | {error, Reason::term()}.
delete_file(Server, FilePath) ->
    call(Server, {delete_file, FilePath}).


%% @equiv delete_files(rct_sftp_client, Path)
-spec delete_files(Path::string()) -> ok.
delete_files(Path) ->
    delete_files(?SERVER, Path).


-spec delete_files(Server::server(), Path::string()) -> ok.
delete_files(Server, Path) ->
    call(Server, {delete_files, Path}).


%%%===================================================================
%%% CT Hooks callbacks
%%%===================================================================
%% @private
id(Opts) ->
    proplists:get_value(instance_name, Opts, ?SERVER).


%% @private
init(Id, Opts) ->
    case start(Opts) of
	{ok, _} ->
	    ok;
	Error ->
	    pal("Failed to start sftp client (~p): ~p", [Id, Error])
    end,
    Scope = proplists:get_value(start_channel_per_tc, Opts, true),
    Cof = proplists:get_value(stop_channel_on_fail, Opts, false),
    {ok, #cth_state{name = lta(Id), 
		    opts = Opts,
		    start_channel_per_tc = Scope,
		    stop_channel_on_fail = Cof}}.


%% @private
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};

pre_init_per_suite(_Suite, Config, State) ->
    case whereis(State#cth_state.name) of
	Pid when is_pid(Pid) ->
	    {Config, State};
	undefined ->
	    {{fail, "Failed to start sftp client."}, State}
    end.
	    
%% @private
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};

pre_init_per_testcase(_TC, Config, State) 
  when State#cth_state.start_channel_per_tc =:= true ->
    case catch start_channel(State#cth_state.name) of
	ok ->
	    {Config, State};
	Error ->
	    log(lightred, "Failed to start sftp channel: ~p", [Error]),
	    {{fail, "Failed to start sftp channel."}, State}
    end;

pre_init_per_testcase(_TC, Config, State) ->
    {Config, State}.


%% @private
post_end_per_testcase(_TC, _Config, Return, State)
  when State#cth_state.start_channel_per_tc =:= true ->
    catch stop_channel(State#cth_state.name),
    {Return, State};

post_end_per_testcase(_TC, _Config, Return, State) ->
    {Return, State}.


%% @private
on_tc_fail(_TC, _Reason, State) ->
    case State#cth_state.stop_channel_on_fail of
	true ->    
	    catch stop_channel(State#cth_state.name);
	_False ->
	    ok
    end,
    State.


%% @private
terminate(State) ->
    catch stop(State#cth_state.name).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start([]).

%% @private
start(Opts) ->
    crypto:start(),
    ssh:start(),
    case init_state(Opts) of
	{ok, State} ->
	    Server = State#state.server,
	    Res = gen_server:start({local, Server}, ?MODULE, State, []),
	    pal("sftp client (~p) started: ~p", [State#state.server, Res]),
	    Res;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stop the server.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    stop(?SERVER).


%% @private
stop(Server) ->
    call(Server, stop).
    

%%%===================================================================
%%% Debug functions
%%%===================================================================
%% @private
dump() ->
    dump(?SERVER).


%% @private
dump(Server) ->
    call(Server, dump).


%% @private
get_ch_pid() ->
    get_ch_pid(?SERVER).


%% @private
get_ch_pid(Server) ->
    call(Server, get_ch_pid).
    

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
init(State) ->
    {ok, State}.

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
handle_call(start_channel, _From, State) 
  when State#state.ch_pid =:= undefined ->
    Ip = State#state.ip,
    Port = State#state.port,
    SshOpts = get_ssh_opts(State),
    case ssh_sftp:start_channel(Ip, Port, SshOpts) of
	{ok, ChPid, ConnRef} ->
	    erlang:monitor(process, ChPid),
	    {reply, ok, State#state{ch_pid = ChPid, conn_ref = ConnRef}};
	Error ->
	    {reply, Error, State}
    end;

handle_call(start_channel, _From, State) ->
    {reply, ok, State};

handle_call(stop_channel, _From, #state{ch_pid = ChPid} = State)
  when State#state.ch_pid =/= undefined ->
    ssh_sftp:stop_channel(ChPid),
    Res = ssh:close(State#state.conn_ref),
    {reply, Res, State#state{ch_pid = undefined, conn_ref = undefined}};

handle_call(stop_channel, _From, State) ->
    {reply, ok, State};

handle_call({Cmd, _}, _From, #state{ch_pid = undefined} = State) 
  when Cmd =:= list_dir;
       Cmd =:= read_file;
       Cmd =:= read_files;
       Cmd =:= write_file;
       Cmd =:= delete_file;
       Cmd =:= delete_files ->
    {reply, {error, channel_not_open}, State};

handle_call({list_dir, Path}, _From, State) ->
    Res = ssh_sftp:list_dir(State#state.ch_pid, Path),
    {reply, Res, State};

handle_call({read_file, Path}, _From, State) ->
    Res = ssh_sftp:read_file(State#state.ch_pid, Path),
    {reply, Res, State};

handle_call({read_files, Path}, _From, State) ->
    ChPid = State#state.ch_pid,
    Res = read_all_files(Path, ChPid),
    {reply, Res, State};

handle_call({write_file, {Path, IoList}}, _From, State) ->
    Res = ssh_sftp:write_file(State#state.ch_pid, Path, IoList),
    {reply, Res, State};

handle_call({delete_file, Path}, _From, State) ->
    Res = ssh_sftp:delete(State#state.ch_pid, Path),
    {reply, Res, State};

handle_call({delete_files, Path}, _From, State) ->
    ChPid = State#state.ch_pid,
    Res = delete_all_files(Path, ChPid),
    {reply, Res, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(dump, _From, State) ->
    {reply, {ok, State}, State};

handle_call(get_ch_pid, _From, State) ->
    {reply, {ok, State#state.ch_pid}, State};

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
handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) 
  when Object =:= State#state.ch_pid ->
    pal("Sftp channel ~p is down: ~p", [Object, _Info]),
    {noreply, State#state{ch_pid = undefined}};

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    {noreply, State}; 

handle_info({_Ref, {error, _Reason} = Error}, State) ->
    log("Sftp channel info received: ~p", [Error]),
    {noreply, State}; 

handle_info(_Info, State) ->
    pal("Received unexpected Info: ~p", [_Info]),
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
    pal("sftp_client ~p terminating: ~p", [State#state.server, _Reason]),
    case State#state.ch_pid of
	ChPid when is_pid(ChPid) ->
	    ssh_sftp:stop_channel(ChPid),
	    ssh:close(State#state.conn_ref);
	_ ->
	    ok
    end,
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


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
call(Server, Call) ->
    gen_server:call(Server, Call, 20000).


init_state(Opts) ->
    try
	Server = proplists:get_value(instance_name, Opts, ?SERVER),
	User = proplists:get_value(user, Opts, ?DEFAULT_USER),
	Pwd = proplists:get_value(password, Opts, ?DEFAULT_PWD),
	Env = os:getenv("SIM_OR_TARGET"),
	CSNode = get_cs_sim_node(Env, Opts),
	Ip = get_ip(Env, Opts),
	Port = get_port(Env, Opts, CSNode),
	{ok, #state{server = Server,
		    test_env = Env,
		    cs_sim_node = CSNode,
		    user = User,
		    password = Pwd,
		    ip = Ip,
		    port = Port,
		    opts = Opts}}
    catch _:Error -> 
	    log(lightred,"~p: Failed to get sftp ip and port: ~p",
		   [?MODULE, Error]),
	    {error, Error}
    end.


get_ip(Env, Opts) ->
    case proplists:get_value(ip, Opts) of
	undefined when Env =:= "target" ->
	    get_target_ip(Opts);
	undefined when Env =:= "cloudish" ->
	    get_target_ip(Opts);
	undefined ->
	    "localhost";
	Ip when is_list(Ip) ->
	    Ip
    end.


get_target_ip(Opts) ->
    N = proplists:get_value(du_no, Opts, ?DEFAULT_DU_NO),
    Site = ct:get_config({test_nodes, N}),
    IpType = proplists:get_value(target_ip_type, Opts, ?DEFAULT_IP_TYPE),
    SSH = ct:get_config({Site, IpType}),
    Ip = proplists:get_value(ssh, SSH),
    Ip =/= undefined 
	orelse throw({"Invalid ssh data:", SSH}),
    Ip.


get_port(Env, Opts, CSNode) ->
    case proplists:get_value(port, Opts) of
	undefined when Env =:= "sim" ->
	    get_cs_sim_port(CSNode);
	undefined when Env =:= "target" ->
	    ?DEFAULT_TARGET_SFTP_PORT;
	undefined when Env =:= "cloudish" ->
	    ?DEFAULT_TARGET_SFTP_PORT;
	Port when is_integer(Port) ->
	    Port;
	Port ->
	    throw({"Invalid sftp port:", Port})
    end.
    

get_cs_sim_port(ErlNode) ->
    case rpc:call(ErlNode, sysEnv, get_port_conf, [sftp]) of
	Port when is_integer(Port) ->
	    Port;
	Other ->
	    throw({"Invalid sftp port from CS sim:", {ErlNode, Other}})
    end.


get_cs_sim_node("sim", Opts) ->
    get_cs_sim_node(Opts);

get_cs_sim_node(_Other, _Opts) ->
    undefined.


get_cs_sim_node(Opts) ->
    User = get_sim_sname(Opts),
    {ok, HostName} = inet:gethostname(),          
    lta(atl(User) ++ "@" ++ HostName).
    

get_sim_sname(Opts) ->
    case init:get_argument(sim_sname) of
	{ok, [[Name]]} ->
	    Name;
	_Other ->
	    get_sim_sname_opt(Opts)
    end.


get_sim_sname_opt(Opts) ->
    case proplists:get_value(sim_sname, Opts) of
	undefined ->
	    User = os:getenv("USER"),
	    get_sim_sname_opt_du(User, Opts);
	SName -> 
	    SName
    end.


get_sim_sname_opt_du(User, Opts) ->
    case proplists:get_value(du_no, Opts) of
	N when is_integer(N) ->
	    "du" ++ itl(N) ++ "_" ++ User; 
	_Other ->
	    get_sim_sname_du(User)
    end.


get_sim_sname_du(User) ->
    case init:get_argument(mp) of
	{ok, [[DU]]} ->
	    DU ++ "_" ++ User;
	_Other ->
	    User
    end.


get_ssh_opts(State) ->
    User = State#state.user,
    Pwd = State#state.password,
    [
     {user, User}, 
     {password, Pwd}, 
%%     {timeout, 30000},
     {silently_accept_hosts, true}
    ].

read_all_files(Path, ChPid) ->
    case ssh_sftp:list_dir(ChPid, Path) of
	{ok, Files} ->
	    read_all_files(Path, Files, ChPid);
	Error ->
	    Error
    end.


read_all_files(Path, Files, ChPid) ->
    try
	{ok, [begin
		  {ok, Data} = 
		      ssh_sftp:read_file(ChPid, filename:join(Path, File)),
		  {File, Data} 
	      end || File <- Files]}
    catch _:Error ->
	    pal("Failed to read files from ~p: ~p", [Path, Error]),
	    {error, failed_to_read_files}
    end.


delete_all_files(Path, ChPid) ->
    case ssh_sftp:list_dir(ChPid, Path) of
	{ok, Files} ->
	    delete_all_files(Path, Files, ChPid);
	Error ->
	    Error
    end.


delete_all_files(Path, Files, ChPid) ->
    %% This is best effort. No error result.
    lists:foreach(fun(File) ->
			  ssh_sftp:delete(ChPid, filename:join(Path, File))
		  end, Files).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.


atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) when is_list(L) ->
    L.
    

itl(I) when is_integer(I) ->
    integer_to_list(I);
itl(L) when is_list(L) ->
    L.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%% print(Fmt, Arg) ->
%%     ppl(print, Fmt, Arg).

%% The following functions are w/a to avoid crashing in erlang shell
%% due to a bug in R16B01.

pal(Fmt, Arg) ->
    ppl(pal, Fmt, Arg).


log(Fmt, Arg) ->
    ppl(log, Fmt, Arg).


log(Cat, Fmt, Arg) ->
    case init:get_argument(shell) of
	{ok, _} ->
	    io:format(Fmt ++ "~n", Arg);
	_ ->
	    ct:log(Cat, Fmt, Arg)
    end.


ppl(Func, Fmt, Arg) ->
    case init:get_argument(shell) of
	{ok, _} ->
	    io:format(Fmt ++ "~n", Arg);
	_ ->
	    ct:Func(Fmt, Arg)
    end.

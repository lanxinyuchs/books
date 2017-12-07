%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_ftpes_client.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/R9A/3
%%%
%%% @doc == FTPES Client CT Hook ==
%%% This module provides a simplified and limited ftpes client API. 
%%%
%%% The default target is a CS node but the client may be configured to 
%%% connect to any ftpes server. 
%%% The module is implemented as a ct_hook in order to facilitate its use in
%%% Common Test suites. The ct_hook determines what target the client
%%% should connect to and sets up IP address and port number accordingly.
%%%
%%% The API functions in this module corresponds to functions in the
%%% OTP module ftp. See OTP documentation for a detailed description
%%% of these functions.
%%%
%%% The `rct_ftpes_client' hook is specified in the `suite/0' function of the 
%%% test suite as described below:
%%%
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_ftpes_client, Opts}]}].
%%%
%%%    Opts = [Opt]
%%%    Opt = {du_no, No} | 
%%%          {sim_sname, SName} | 
%%%          {instance_name, InstanceName} | 
%%%          {ip, IpAddress} | 
%%%          {port, PortNumber} | 
%%%          {user, User} | 
%%%          {password, Password} | 
%%%          {tls_options, TlsOpts} | 
%%%          {mode, Mode} | 
%%%          {start_session_per_tc, SCpTCFlag} | 
%%%          {stop_session_on_fail, SCoFFlag}'''
%%%
%%%  `No = integer()' <br/>
%%%    - Used to indicate testnode (DU) number when testing towards a node in
%%%      a cluster. Note that only the active node runs ftpes.<br/>
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
%%%      Default is `rct_ftpes_client' <br/>
%%%
%%%  `IpAddress = string()' <br/>
%%%    - Used when connecting to another node than a CS node. May also be
%%%      used to override the default IP address in CS tests.
%%%
%%%  `PortNumber = integer()' <br/>
%%%    - Used to override the default port number.<br/>
%%%      Default port is 9921 in CS target and 21 in non CS target.
%%%
%%%  `User = string()' <br/>
%%%    - User name for the ftpes server. 
%%%
%%%  `Password = string()' <br/>
%%%    - Password for the ftpes server. 
%%%
%%%  `TlsOpts = list()' <br/>
%%%    - List of TLS options such as cert, key, cacert...
%%%      Default tls_options are cert and key file from FTPES block
%%%
%%%  `Mode = atom()' <br/>
%%%    - Data transport mode can be active or passive
%%%      Default is passive
%%%
%%%  `SCpTCFlag = boolean()' <br/>
%%%    - Determines if the ftpes session should be automatically started before, 
%%%      and stopped after, each test case. <br/>
%%%      Default is `true'<br/>
%%%
%%%  `SCoFFlag = boolean()' <br/>
%%%    - Determines if the ftpes session should be automatically stopped at 
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
%%%     [{ct_hooks, [{rct_ftpes_client, [{instance_name, ftpes_1},
%%%                                     {du_no, 1}]},
%%%                  {rct_ftpes_client, [{instance_name, ftpes_2}
%%%                                     {du_no, 2}]}].'''
%%%
%%% @end
%%% ----------------------------------------------------------
-module(rct_ftpes_client).
-vsn('/main/R8A/R9A/3').
-date('2017-02-28').
-author('ekurnik').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R8A/1   2017-01-18   ekurnik     Created
%%% R8A/2   2017-01-20   ekurnik     Fixed issue with file paths
%%% R9A/1   2017-01-31   ekurnik     Added new calls: cd, pwd, mkdir, rmdir
%%% R9A/2   2017-02-06   ekurnik     Minor fix
%%% R9A/3   2017-02-28   ekurnik     Binary mode by default
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API
-export([open/0,
     open/1,
     close/0, close/1,
     list_dir/1,list_dir/2,
     read_file/1, read_file/2, read_files/1, read_files/2,
     write_file/2, write_file/3,
     delete_file/1, delete_file/2, delete_files/1, delete_files/2,
     pwd/0, pwd/1,
     cd/1, cd/2,
     mkdir/1, mkdir/2,
     rmdir/1, rmdir/2]).

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

-define(DATA_DIR, ftpes_test_lib:get_ftpes_data_dir()).
-define(CERTFILE, ?DATA_DIR ++ "/user_expert.crt").
-define(KEYFILE, ?DATA_DIR ++ "/user_expert.key").

-define(DEFAULT_USER, "dustest").
-define(DEFAULT_PWD, "dustest").
-define(DEFAULT_TARGET_FTPES_PORT, 9921).
-define(DEFAULT_IP_TYPE, ssh_lmt_ipv4).
-define(DEFAULT_TLS_OPTS, [{certfile, ?CERTFILE}, {keyfile, ?KEYFILE}]).
-define(DEFAULT_MODE, passive).
-define(DEFAULT_DU_NO, 1).



%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------
-record(cth_state, {name,
            opts,
            start_session_per_tc,
            stop_session_on_fail}).

-record(state, {server,
        test_env, 
        ip, 
        port, 
        user, 
        password,
        tls_options, 
        mode,
        ch_pid,
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
%% @equiv open(rct_ftpes_client)
-spec open() -> ok | {error, Reason::term()}.
open() ->
    open(?SERVER).


-spec open(Server::server()) -> ok | {error, Reason::term()}.
open(Server) ->
    call(Server, open).


%% @equiv close(rct_ftpes_client)
-spec close() -> ok | {error, Reason::term()}.
close() ->
    close(?SERVER).


-spec close(Server::server()) -> ok | {error, Reason::term()}.
close(Server) ->
    call(Server, close).


%% @equiv list_dir(rct_ftpes_client, Path)
-spec list_dir(Path::string()) -> 
              {ok, [FileName::string()]} | {error, Reason::term()}.
list_dir(Path) ->
    list_dir(?SERVER, Path).


-spec list_dir(Server::server(), Path::string()) -> 
              {ok, [FileName::string()]} | {error, Reason::term()}.
list_dir(Server, Path) ->
    call(Server, {list_dir, Path}).
    

%% @equiv read_file(rct_ftpes_client, FilePath)
-spec read_file(FilePath::string()) -> 
               {ok, Data::binary()} | {error, Reason::term()}.
read_file(FilePath) ->
    read_file(?SERVER, FilePath).


-spec read_file(Server::server(), FilePath::string()) -> 
               {ok, Data::binary()} | {error, Reason::term()}.
read_file(Server, FilePath) ->
    call(Server, {read_file, FilePath}).
    

%% @equiv read_files(rct_ftpes_client, Path)
-spec read_files(Path::string()) -> 
            {ok, [file_data()]} | {error, Reason::term()}.
read_files(Path) ->
    read_files(?SERVER, Path).


-spec read_files(Server::server(), Path::string()) -> 
            {ok, [file_data()]} | {error, Reason::term()}.
read_files(Server, Path) ->
    call(Server, {read_files, Path}).
    

%% @equiv write_file(rct_ftpes_client, FilePath)
-spec write_file(FilePath::string(), IoList::iolist()) ->  
            ok | {error, Reason::term()}.
write_file(FilePath, IoList) ->
    write_file(?SERVER, FilePath, IoList).


-spec write_file(Server::server(), FilePath::string(), IoList::iolist()) ->  
            ok | {error, Reason::term()}.
write_file(Server, FilePath, IoList) ->
    call(Server, {write_file, {FilePath, IoList}}).
    

%% @equiv delete_file(rct_ftpes_client, FilePath)
-spec delete_file(FilePath::string()) ->  
             ok | {error, Reason::term()}.
delete_file(FilePath) ->
    delete_file(?SERVER, FilePath).


-spec delete_file(Server::server(), FilePath::string()) ->  
             ok | {error, Reason::term()}.
delete_file(Server, FilePath) ->
    call(Server, {delete_file, FilePath}).


%% @equiv delete_files(rct_ftpes_client, Path)
-spec delete_files(Path::string()) -> ok.
delete_files(Path) ->
    delete_files(?SERVER, Path).


-spec delete_files(Server::server(), Path::string()) -> ok.
delete_files(Server, Path) ->
    call(Server, {delete_files, Path}).

%% @equiv pwd(rct_ftpes_client)
-spec pwd() -> {ok, Dir :: string()}.
pwd() ->
    pwd(?SERVER).


-spec pwd(Server::server()) -> {ok, Dir :: string()}.
pwd(Server) ->
    call(Server, {pwd, []}).

%% @equiv cd(rct_ftpes_client, Path)
-spec cd(Dir::string()) -> ok | {error, Reason::term()}.
cd(Dir) ->
    cd(?SERVER, Dir).


-spec cd(Server::server(), Dir::string()) -> ok | {error, Reason::term()}.
cd(Server, Dir) ->
    call(Server, {cd, Dir}).

%% @equiv mkdir(rct_ftpes_client, Path)
-spec mkdir(Dir::string()) -> ok | {error, Reason::term()}.
mkdir(Dir) ->
    mkdir(?SERVER, Dir).


-spec mkdir(Server::server(), Dir::string()) -> ok | {error, Reason::term()}.
mkdir(Server, Dir) ->
    call(Server, {mkdir, Dir}).

%% @equiv rmdir(rct_ftpes_client, Path)
-spec rmdir(Dir::string()) -> ok | {error, Reason::term()}.
rmdir(Dir) ->
    rmdir(?SERVER, Dir).


-spec rmdir(Server::server(), Dir::string()) -> ok | {error, Reason::term()}.
rmdir(Server, Dir) ->
    call(Server, {rmdir, Dir}).



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
        pal("Failed to start ftpes client (~p): ~p", [Id, Error])
    end,
    Scope = proplists:get_value(start_session_per_tc, Opts, true),
    Cof = proplists:get_value(stop_session_on_fail, Opts, false),
    {ok, #cth_state{name = lta(Id), 
            opts = Opts,
            start_session_per_tc = Scope,
            stop_session_on_fail = Cof}}.


%% @private
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};

pre_init_per_suite(_Suite, Config, State) ->
    case whereis(State#cth_state.name) of
    Pid when is_pid(Pid) ->
        {Config, State};
    undefined ->
        {{fail, "Failed to start ftpes client."}, State}
    end.
        
%% @private
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};

pre_init_per_testcase(_TC, Config, State) 
  when State#cth_state.start_session_per_tc =:= true ->
    case catch open(State#cth_state.name) of
    ok ->
        {Config, State};
    Error ->
        log(lightred, "Failed to start ftpes session: ~p", [Error]),
        {{fail, "Failed to start ftpes session."}, State}
    end;

pre_init_per_testcase(_TC, Config, State) ->
    {Config, State}.


%% @private
post_end_per_testcase(_TC, _Config, Return, State)
  when State#cth_state.start_session_per_tc =:= true ->
    catch close(State#cth_state.name),
    {Return, State};

post_end_per_testcase(_TC, _Config, Return, State) ->
    {Return, State}.


%% @private
on_tc_fail(_TC, _Reason, State) ->
    case State#cth_state.stop_session_on_fail of
    true ->    
        catch close(State#cth_state.name);
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
    ssl:start(),
    case init_state(Opts) of
    {ok, State} ->
        Server = State#state.server,
        Res = gen_server:start({local, Server}, ?MODULE, State, []),
        pal("ftpes client (~p) started: ~p", [State#state.server, Res]),
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
handle_call(open, _From, State) 
  when State#state.ch_pid =:= undefined ->
    Ip = State#state.ip,
    Port = State#state.port,
    Mode = State#state.mode,
    TlsOpts = State#state.tls_options,
    User = State#state.user,
    Pass = State#state.password,
    case ftp:open(Ip, [{port, Port}, {mode, Mode}, {tls, TlsOpts}]) of
    {ok, ChPid} ->
        case ftp:user(ChPid, User, Pass) of
            ok ->
                erlang:monitor(process, ChPid),
                ftp:type(ChPid, binary),
                {reply, ok, State#state{ch_pid = ChPid}};
            Error ->
                {reply, Error, State}
        end;
    Error ->
        {reply, Error, State}
    end;

handle_call(open, _From, State) ->
    {reply, ok, State};

handle_call(close, _From, #state{ch_pid = ChPid} = State)
  when State#state.ch_pid =/= undefined ->
    ftp:close(ChPid),
    {reply, ok, State#state{ch_pid = undefined}};

handle_call(close, _From, State) ->
    {reply, ok, State};

handle_call({Cmd, _}, _From, #state{ch_pid = undefined} = State) 
  when Cmd =:= list_dir;
       Cmd =:= read_file;
       Cmd =:= read_files;
       Cmd =:= write_file;
       Cmd =:= delete_file;
       Cmd =:= delete_files ->
    {reply, {error, session_not_open}, State};

handle_call({list_dir, Path}, _From, State) ->
    Res = ftp:nlist(State#state.ch_pid, Path),
    {reply, Res, State};

handle_call({read_file, Path}, _From, State) ->
    Res = ftp:recv_bin(State#state.ch_pid, Path),
    {reply, Res, State};

handle_call({read_files, Path}, _From, State) ->
    ChPid = State#state.ch_pid,
    Res = read_all_files(Path, ChPid),
    {reply, Res, State};

handle_call({write_file, {Path, IoList}}, _From, State) ->
    Res = ftp:send_bin(State#state.ch_pid, IoList, Path),
    {reply, Res, State};

handle_call({delete_file, Path}, _From, State) ->
    Res = ftp:delete(State#state.ch_pid, Path),
    {reply, Res, State};

handle_call({delete_files, Path}, _From, State) ->
    ChPid = State#state.ch_pid,
    Res = delete_all_files(Path, ChPid),
    {reply, Res, State};

handle_call({cd, Dir}, _From, State) ->
    Res = ftp:cd(State#state.ch_pid, Dir),
    {reply, Res, State};

handle_call({pwd, _}, _From, State) ->
    Res = ftp:pwd(State#state.ch_pid),
    {reply, Res, State};

handle_call({mkdir, Dir}, _From, State) ->
    Res = ftp:mkdir(State#state.ch_pid, Dir),
    {reply, Res, State};

handle_call({rmdir, Dir}, _From, State) ->
    Res = ftp:rmdir(State#state.ch_pid, Dir),
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
    pal("Ftpes session ~p is down: ~p", [Object, _Info]),
    {noreply, State#state{ch_pid = undefined}};

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    {noreply, State}; 

handle_info({_Ref, {error, _Reason} = Error}, State) ->
    log("Ftpes session info received: ~p", [Error]),
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
    pal("ftpes_client ~p terminating: ~p", [State#state.server, _Reason]),
    case State#state.ch_pid of
    ChPid when is_pid(ChPid) ->
        ftp:close(ChPid);
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
    TlsOpts = proplists:get_value(tls_options, Opts, ?DEFAULT_TLS_OPTS),
    Mode = proplists:get_value(mode, Opts, passive),
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
            tls_options = TlsOpts,
            mode = Mode,
            opts = Opts}}
    catch _:Error -> 
        log(lightred,"~p: Failed to get ftpes ip and port: ~p",
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
        ?DEFAULT_TARGET_FTPES_PORT;
    undefined when Env =:= "cloudish" ->
        ?DEFAULT_TARGET_FTPES_PORT;
    Port when is_integer(Port) ->
        Port;
    Port ->
        throw({"Invalid ftpes port:", Port})
    end.
    

get_cs_sim_port(ErlNode) ->
    case rpc:call(ErlNode, sysEnv, get_port_conf, [ftpes]) of
    Port when is_integer(Port) ->
        Port;
    Other ->
        throw({"Invalid ftpes port from CS sim:", {ErlNode, Other}})
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

read_all_files(Path, ChPid) ->
    case ftp:nlist(ChPid, Path) of
    {ok, FileListing} ->
        Files = [filename:basename(string:strip(File)) || 
                   File <- string:tokens(FileListing, "\r\n")],
        read_all_files(Path, Files, ChPid);
    Error ->
        Error
    end.


read_all_files(Path, Files, ChPid) ->
    try
    {ok, [begin
          {ok, Data} = 
              ftp:recv_bin(ChPid, filename:join(Path, File)),
          {File, Data} 
          end || File <- Files]}
    catch _:Error ->
        pal("Failed to read files from ~p: ~p", [Path, Error]),
        {error, failed_to_read_files}
    end.


delete_all_files(Path, ChPid) ->
    case ftp:nlist(ChPid, Path) of
    {ok, FileListing} ->
        Files = [filename:basename(string:strip(File)) || 
                   File <- string:tokens(FileListing, "\r\n")],
        delete_all_files(Path, Files, ChPid);
    Error ->
        Error
    end.


delete_all_files(Path, Files, ChPid) ->
    %% This is best effort. No error result.
    lists:foreach(fun(File) ->
              ftp:delete(ChPid, filename:join(Path, File))
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

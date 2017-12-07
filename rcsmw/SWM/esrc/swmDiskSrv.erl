%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmDiskSrv.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R9A/R11A/1

%% @doc == Disk Server ==
%% Server process for synchronous writing on disk.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmDiskSrv).
-vsn('/main/R9A/R11A/1').
-date('2017-10-11').
-author(etxberb).

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:	module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%

%% ###=======================================================================###
%% # 1.4   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R9A/1   2017-04-19 etxberb  Created.
%% R11A/1  2017-10-11 etxberb  Added logging with time consumption for request.
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    INTERNAL DEFINITIONS
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 2.1.1 Interface functions
%% ###-----------------------------------------------------------------------###
%% Interface
-export([request/1]).

%% Basic
-export([child_specs/0,
	 start/0,
	 start_link/0,
	 stop/0]).

%% Queries
-export([get_state/0,
	 get_state/1,
	 info/0]).

%%% ###----------------------------------------------------------------------###
%%% # 2.1.2 Callbacks
%%% ###----------------------------------------------------------------------###
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###
%% Basic
-define(CHILD_Server, {?SERVER,
		       {?MODULE, start_link, []},
		       permanent,
		       1000,
		       worker,
		       [?MODULE]}).
-define(MyMsgTag, ?MODULE).
-define(SERVER, ?MODULE).
-define(SERVER_STR, sysUtil:term_to_string(?SERVER)).

%% Misc
-define(CALL_TIMEOUT, 10000).   % Default timeout in ms for syncronous calls.
-define(MANDATORY_mfaFun(__FromMod),
	fun() ->
		erlang:error("Mandatory #{mfaFun => } missing in " ++
			     sysUtil:term_to_string(__FromMod))
	end).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	sysInitI:error_report(?RepInfo(__ReportInfo))).
-define(LOG_ERR_ALL(__ReportInfo),
	error_logger:error_report(?RepInfo(__ReportInfo))).
-define(LOG_INFO(__ReportInfo),
	sysInitI:info_report(?RepInfo(__ReportInfo))).
-define(LOG_WARN(__ReportInfo),
	sysInitI:warning_report(?RepInfo(__ReportInfo))).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).

%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(LD_INFO(__Record),   % LoopData
	sysUtil:record_format(record_info(fields, ld), __Record)).
-define(MODULE_STR, sysUtil:term_to_string(?MODULE)).
-define(MonoTime, erlang:monotonic_time()).
-define(PROC_INFO(__Pid), sysUtil:pid_info(__Pid)).
-define(STATE_INFO(__Record),
	sysUtil:record_format(record_info(fields, state), __Record)).

%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###
-record(state, {state = operational,
		parent}).

%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###
-type child_id()       :: term().               % Not a pid() !
						% Identifies the child spec
						% internally in the supervisor.
-type child_modules()  :: [module()] | dynamic.
-type child_restart()  :: permanent | transient | temporary.
-type child_shutdown() :: brutal_kill | timeout().
-type child_spec()     :: {Id        :: child_id(),
			   StartFunc :: mfargs(),
			   Restart   :: child_restart(),
			   Shutdown  :: child_shutdown(),
			   Type      :: child_type(),
			   Modules   :: child_modules()}.
-type child_type()     :: worker | supervisor.

-type error_reason() :: term().

-type mfargs() :: {module(), atom(), list()}.

-type request() :: #{mfaFun   := mfargs() | function(),
		     reply    => boolean(),
		     shutdown => boolean(),
		     timeout  => timeout()}.

-type user_defined_reply() :: term().

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 3.    CODE
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.1.1 Interface functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% @doc Request for executing in the server process.
%%   The requested 'mfaFun' will be sent to the swmDiskSrv process for
%%   execution. The default behaviour is to wait for a reply from the requested
%%   'mfaFun' and then either return the 'mfaFun' return value or re-generate
%%   any exception that occurred within the 'mfaFun'.
%%
%%   Exceptions will be mirrored (re-generated) in the 'from' process as they
%%   were caught by swmDiskSrv. They will also be logged as an ERROR REPORT,
%%   except the 'throw' class which is considered to be a controlled way of
%%   returning fail cases and thus part of expected scenarios.
%%
%%   Asyncronous request is also available as an option. In that case, all
%%   exception classes including 'throw' will be ERROR logged.
%%
%%   Also, a 'shutdown' option is available, meaning that the user intends to
%%   shutdown (cold restart) the node either within the requested 'mfaFun' or
%%   after returning to the requesting function. The swmDiskSrv process will
%%   then go into 'shutting_down' state and thus ignoring all subsequent
%%   requests.
%%
%% @end
%% ###=======================================================================###
-spec request(Request :: request()) ->
    user_defined_reply() |         % When #{reply => true} (default)
	ok |                       % When #{reply => false}
	{error, error_reason()}.   % When #{reply => false}
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
request(Request) ->
    FromMod = sysUtil:get_previous_module(),
    Default = #{mfaFun   => ?MANDATORY_mfaFun(FromMod),
		reply    => true,
		shutdown => false,
		timeout  => ?CALL_TIMEOUT},
    FullReq = maps_put([{fromPid, self()},
			{fromMod, FromMod}],
		       maps:merge(Default, Request)),
    case maps:get(reply, FullReq) of
	true ->
	    case call(request,
		      fun do_request/1,
		      [FullReq],
		      maps:get(timeout, FullReq))
		of
		{req_exception, {ExcClass, ExcReason}} ->
		    erlang:ExcClass(ExcReason);
		Reply ->
		    Reply
	    end;
	false ->
	    cast(request, [FullReq])
    end.

%% #############################################################################
%% @doc Return specifications of children processes in the supervisor tree.
%%
%% @end
%% ###=======================================================================###
-spec child_specs() ->
    list(child_spec()).
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
child_specs() ->
    [?CHILD_Server].

%% #############################################################################
%% @doc Fetch the process state.
%%
%% @end
%% ###=======================================================================###
-spec get_state() ->
    {ok, State :: #state{}} |
	{error, error_reason()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_state() ->
    get_state(?CALL_TIMEOUT).

%% ###=======================================================================###
-spec get_state(Timeout :: timeout()) ->
    {ok, State :: #state{}} |
	{error, error_reason()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_state(Timeout) ->
    gen_server:call(?SERVER, {?MyMsgTag, get_state}, [{timeout, Timeout}]).

%% #############################################################################
%% @doc Report process info to the erlang log.
%%
%% @end
%% ###=======================================================================###
-spec info() ->
    ok.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
info() ->
    gen_server:cast(?SERVER, {?MyMsgTag, info}).

%% #############################################################################
%% @doc Start the server process.
%%
%% @end
%% ###=======================================================================###
-spec start() ->
    {ok, pid()} | ignore |
	{error, error_reason()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start() ->
    ServerName = {local, ?SERVER},
    Module = ?MODULE,
    InitArg = #state{parent = self()},
    Options = [],
    gen_server:start(ServerName, Module, InitArg, Options).

%% #############################################################################
%% @doc Start the server process and link the calling process to it.
%%
%% @end
%% ###=======================================================================###
-spec start_link() ->
    {ok, pid()} | ignore |
	{error, error_reason()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link() ->
    ServerName = {local, ?SERVER},
    Module = ?MODULE,
    InitArg = #state{parent = self()},
    Options = [],
    gen_server:start_link(ServerName, Module, InitArg, Options).

%% #############################################################################
%% @doc Stop the server process.
%%
%% @end
%% ###=======================================================================###
-spec stop() ->
    ok.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop() ->
    gen_server:stop(?SERVER).

%%% ###----------------------------------------------------------------------###
%%% # 3.1.2 'gen_server' call back functions
%%% ###----------------------------------------------------------------------###
%% #############################################################################
%% @doc Initialize the server process.
%%
%% @end
%% ###=======================================================================###
init(State) ->
    {ok, do(fun do_init/1, [State])}.

%% #############################################################################
%% @doc handle_call
%%
%% @end
%% ###=======================================================================###
handle_call(Req, From, #state{state = shutting_down} = State) ->
    ?LOG_INFO(["In shutting_down state, message ignored",
	       {req, Req},
	       {from, sysUtil:pid_name(From)}]),
    {reply, shutting_down, State};
handle_call({?MyMsgTag, request, [Request] = Args}, _From, State) ->
    T1 = ?MonoTime,
    Result = do(fun do_request/1, Args),
    T_end = ?MonoTime,
    ?LOG_INFO([{request, Request},
	       {total, sysUtil:time_to_string(T_end - T1)}]),
    {reply, Result, new_state(Request, State)};
handle_call({?MyMsgTag, get_state}, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    ?LOG_WARN([{unrecognized, _Request} | ?PROC_INFO(_From)]),
    {reply, undefined, State}.

%% #############################################################################
%% @doc handle_cast
%%
%% @end
%% ###=======================================================================###
handle_cast(Req, #state{state = shutting_down} = State) ->
    ?LOG_INFO(["In shutting_down state, message ignored",
	       {req, Req}]),
    {noreply, State};
handle_cast({?MyMsgTag, request, [Request] = Args}, State) ->
    T1 = ?MonoTime,
    case do(fun do_request/1, Args) of
	{req_exception, {throw = ExcClass, ExcReason}} ->
	    %% The user dosen't care (cast), so it means that this is a fault.
	    %% The 'throw' class is not handled in the general function
	    %% 'do_request', so it needs to be handled here:
	    Stacktrace = erlang:get_stacktrace(),
	    ?LOG_ERR([{ExcClass, ExcReason},
		      {request, Request},
		      {stacktrace, Stacktrace} |
		      fromPid_info(Request)]);
	_ ->
	    ok
    end,
    T_end = ?MonoTime,
    ?LOG_INFO([{request, Request},
	       {total, sysUtil:time_to_string(T_end - T1)}]),
    {noreply, new_state(Request, State)};
handle_cast({?MyMsgTag, info}, State) ->
    ?LOG_INFO(?STATE_INFO(State) ++ ?PROC_INFO(self())),
    {noreply, State};
handle_cast(_Request, State) ->
    ?LOG_WARN([{unrecognized, _Request}]),
    {noreply, State}.

%% #############################################################################
%% @doc handle_info
%%
%% @end
%% ###=======================================================================###
handle_info(Msg, #state{state = shutting_down} = State) ->
    ?LOG_INFO(["In shutting_down state, message ignored",
	       {msg, Msg}]),
    {noreply, State};
handle_info(_Msg, State) ->
    ?LOG_WARN([{unrecognized, _Msg}]),
    {noreply, State}.

%% #############################################################################
%% @doc code_change
%%
%% @end
%% ###=======================================================================###
code_change(_OldVsn, State, _Extra) ->
    ?LOG_INFO([{oldVsn, _OldVsn}, {extra, _Extra} | ?STATE_INFO(State)]),
    {ok, State}.

%% #############################################################################
%% @doc terminate
%%
%% @end
%% ###=======================================================================###
terminate(_Reason, _State) ->
    ?LOG_INFO([{reason, _Reason} | ?STATE_INFO(_State)]),
    ok.

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Do Functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% do_init
%%
%% ###=======================================================================###
do_init(State) ->
    State#state{state = operational}.

%% #############################################################################
%% do_request
%%
%% ###=======================================================================###
do_request(Request) ->
    %% The req_exception return value is applicable only when the user has
    %% requested #{reply => true}, which means that this result is sent back to
    %% the user process and the exception is re- generated. I.e. the reply will
    %% be the exception that we caught down here.
    case maps:get(mfaFun, Request) of
	{M, F, A} ->
	    try
		apply(M, F, A)
	    catch
		throw : Reason ->
		    %% This is regarded as a normal failure use case that the
		    %% user recognizes and will handle in a 'catch'.
		    {req_exception, {throw, Reason}};
		ExcClass : ExcReason ->
		    Stacktrace = erlang:get_stacktrace(),
		    ?LOG_ERR([{ExcClass, ExcReason},
			      {request, Request},
			      {stacktrace, Stacktrace} |
			      fromPid_info(Request)]),
		    {req_exception, {ExcClass, ExcReason}}
	    end;
	Fun when is_function(Fun) ->
	    try
		Fun()
	    catch
		throw : Reason ->
		    %% This is regarded as a normal failure use case that the
		    %% user recognizes and will handle in a 'catch'.
		    {req_exception, {throw, Reason}};
		ExcClass : ExcReason ->
		    Stacktrace = erlang:get_stacktrace(),
		    ?LOG_ERR([{ExcClass, ExcReason},
			      {request, Request},
			      {stacktrace, Stacktrace} |
			      fromPid_info(Request)]),
		    {req_exception, {ExcClass, ExcReason}}
	    end
    end.

%% ###-----------------------------------------------------------------------###
%% # 3.3.2 Help Functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% call
%%
%% ###=======================================================================###
call(MsgName, MsgFun, Args, Timeout) ->
    Self = self(),
    case whereis(?SERVER) of
	Self ->
	    do(MsgFun, Args);
	_ ->
	    gen_server:call(?SERVER, {?MyMsgTag, MsgName, Args}, Timeout)
    end.

%% #############################################################################
%% cast
%%
%% ###=======================================================================###
cast(MsgName, Args) ->
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    gen_server:cast(?SERVER, {?MyMsgTag, MsgName, Args});
	_ ->
	    {error, ?SERVER_STR ++ " process does not exist"}
    end.

%% #############################################################################
%% do
%%
%% ###=======================================================================###
do(Fun, Args) ->
    {module, Module} = erlang:fun_info(Fun, module),
    {name, FunctionName} = erlang:fun_info(Fun, name),
    do(Fun, Module, FunctionName, Args).

do(Fun, Module, Function, Args) ->
    try
	erlang:apply(Fun, Args)
    catch
	ExcClass : ExcReason ->
	    Stacktrace = erlang:get_stacktrace(),
	    ?LOG_ERR([{ExcClass, ExcReason},
		      {module, Module},
		      {function, Function}
		      | format_args(Args)] ++
		     [{stacktrace, Stacktrace}
		      | ?PROC_INFO(self())]),
	    {ExcClass, ExcReason}
    end.

%% #############################################################################
%% format_args
%%
%% ###=======================================================================###
format_args(Args) ->
    format_args(Args, 1).

format_args([#state{} = State | Tail], Cnt) ->
    [{"arg." ++ integer_to_list(Cnt), "state - see below"} |
     ?STATE_INFO(State)] ++
	format_args(Tail, Cnt);
format_args([Arg | Tail], Cnt) ->
    [{"arg." ++ integer_to_list(Cnt), Arg} | format_args(Tail, Cnt + 1)];
format_args([], _) ->
    [].

%% #############################################################################
%% fromPid_info
%%
%% ###=======================================================================###
fromPid_info(Request) ->
    FromPid = maps:get(fromPid, Request),
    ["--- From process ---" | sysUtil:pid_name(FromPid)].

%% #############################################################################
%% maps_put
%%
%% ###=======================================================================###
maps_put([{Key, Value} | Tail], Map) ->
    maps_put(Tail, maps:put(Key, Value, Map));
maps_put([], Map) ->
    Map.

%% #############################################################################
%% new_state
%%
%% ###=======================================================================###
new_state(Request, State) ->
    case maps:get(shutdown, Request) of
	false ->
	    State;
	true ->
	    State#state{state = shutting_down}
    end.

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###

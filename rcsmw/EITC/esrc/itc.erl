%%% -*- Erlang -*-
%%% %EricssonCopyright%
%%% %CopyrightBegin%
%%%
%%% Copyright Ericsson AB 2013-2016.  All Rights Reserved.
%%%
%%% The program may be used and/or copied only with the written permission
%%% from Ericsson AB, or in accordance with the terms and conditions
%%% stipulated in the agreement/contract under which the program
%%% has been supplied.
%%%
%%% %CopyrightEnd%
%%%
%%% @doc Interface module for ITC from Erlang
%%%
%%%
%%% == Erlang Driver ==
%%%
%%% This interface module uses an Erlang driver to manage ITC.
%%% One port instance is created for each ITC mailbox that is created.
%%% This port instance creates one process thread to own and handle
%%% the mailbox.  Creating many mailboxes hence creates many process
%%% threads which may degrade system performance.
%%%
%%% The mailbox handler thread directly sends received ITC messages
%%% to the Erlang process that created the mailbox port.  This is
%%% only possible in the SMP Erlang VM so the driver will refuse
%%% to be loaded unless in such a VM.
%%%
%%% ITC messages that are sent from an Erlang process are first
%%% sent from the Erlang VM scheduler thread the Erlang process
%%% runs on to the mailbox handler thread, and then from that
%%% thread to the destination.  This extra step is required to
%%% ensure message order.  The ITC message does not suffer any
%%% extra copying due to this.  Instead a small wrapper message
%%% is used to hold the real message over the first hop.
%%%
%%%
%%% == Initialization ==
%%%
%%% The driver must start with initializing ITC.  So the first function
%%% to call must be {@link init/1}.  It should not be needed to call
%%% {@link init/1} explicitly since it is done at application start
%%% with parameters from the application environment having the same names
%%% as the options to {@link init/1}, that is: {@link init_opt()}.
%%% See also {@link start/1}.
%%%
%%% @end
-module(itc).

-type driver_path() :: [string()].
%% List of directories where to search for the dynamically linked driver
%% that manages the ITC API.  If empty (the default) a list of directories
%% is used that contains the default installation directory as well
%% as the source build directory.  This option should only be necessary
%% if you have to place the driver file in an unusual location.

-type mailbox_count() :: non_neg_integer().
%% Mailbox count.  Default is `1000'.
-define(MAILBOX_COUNT, 1000).

-type alloc_scheme() ::
	malloc |
	{pool,non_neg_integer()} |
	{pool_flex,
	 non_neg_integer(),
	 {non_neg_integer(),non_neg_integer(),
	  non_neg_integer(),non_neg_integer(),
	  non_neg_integer(),non_neg_integer(),
	  non_neg_integer(),non_neg_integer()}} |
	driver_alloc.
%% Memory allocation scheme for ITC.  Default is `driver_alloc',
%% which will use the Erlang VM memory allocation through
%% the `ITC_USER_DEFINED' allocation scheme.
%% The others are ITC's allocation schemes.
-define(ALLOC_SCHEME, driver_alloc).

-type namespace() :: no_namespace | iodata().
%% ITC Namespace for the VM node.  Default is `no_namespace'.
-define(NAMESPACE, no_namespace).

%%==========================================================================
%% Exported API
%%==========================================================================
-export([start/0,start/1,stop/0,open/1,close/1]).
-export([get_id/1,get_name/2,hunt/2,dehunt/2,attach/2,detach/2]).
-export([send/4,send/5,listen/1,listen/2]).

%%==========================================================================
%% Application internal API
%%==========================================================================
-export([open/2,init/0,init/1,init/2,exit/0,exit/1]).
-compile({no_auto_import,[exit/1]}).
-export([open_port/1,close_port/1]).
-export([default_driver_path/0,load_driver/0,load_driver/1,unload_driver/0]).

%%==============================================================================
%% Types
%%==============================================================================

-type mailbox() :: port(). %% -opaque
%% Mailbox handle.  Implementead as an erlang port.

-type mailbox_id() :: integer(). %% -opaque
%% ITC Mailbox ID.  An integer.

-type message_number() :: 0..4294967295. %% -opaque
%% ITC Message number

-type hunt_ref() :: {'locate_ref',port(),integer()}. %% -opaque
%% Reference from a hunt request.  This term will be included
%% in a successful hunt response.

-type attach_ref() :: {'monitor_ref',port(),integer()}. %% -opaque
%% Reference from an attach request.  This term will be included
%% in the term returned when the attached mailbox disappears.

-export_type([mailbox/0,mailbox_id/0,message_number/0]).
-export_type([hunt_ref/0,attach_ref/0]).

-define(IS_UINT32(X), (X) band 16#FFFFFFFF =:= (X)).

%%==============================================================================
%% External API
%%
%% These throw exceptions from the API functions themselves, not from below.
%%==============================================================================

%%------------------------------------------------------------------------------
%% @equiv start([])
%% @end
%%------------------------------------------------------------------------------
-spec start() -> ok | {error,term()}.

start() ->
    start([]).

%%------------------------------------------------------------------------------
%% @doc Start the eITC application with given options.
%%
%% The application is first loaded unless already loaded,
%% then the given options are set in the application environment,
%% and then the application is started.
%%
%% When the application starts the options to {@link init/1} are
%% taken from the application environment variables with the same
%% names as the options {@link init_opt()}.
%%
%% Before calling {@link init/1} a call to {@link exit/0} is done
%% to ensure initializing will succeed e.g after `init:restart()'.
%%
%% @see stop/0
%% @end
%%------------------------------------------------------------------------------
-spec start(Opts) -> ok | {error,term()} when
      Opts :: [init_opt()].

start(Opts) when is_list(Opts) ->
    case application:load(?APP) of
	ok ->
	    start(Opts, ?APP);
	{error,{already_loaded,?APP}} ->
	    start(Opts, ?APP)
    end;
start(Opts) ->
    erlang:error(badarg, [Opts]).



start([], App) ->
    application:start(App);
start([{Opt,Val}|Opts], App) ->
    ok = application:set_env(App, Opt, Val),
    start(Opts, App).

%%------------------------------------------------------------------------------
%% @doc Stop the eITC application.
%%
%% The same as `application:stop(eitc)'.
%% Will call {@link exit/0} while stopping.
%%
%% @see start/1
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> ok | {error,term()}.

stop() ->
    application:stop(?APP).

%%------------------------------------------------------------------------------
%% @doc Create an ITC mailbox with the given name name and return a port
%% that handles the mailbox.
%%
%% The caller gets linked to the created port.  The caller also gets
%% received ITC messages from the port.  Note that changing the port owner
%% using `erlang:port_connect/2' does not change the ITC message receiver.
%% This might work one day and then a suitable API function will appear
%% in this module.
%%
%% One dedicated process thread is created to own and handle the mailbox,
%% since ITC mandates this.  Creating many mailboxes can therefore degrade
%% system performance.
%%
%% raises: `badarg' | `system_limit'
%%
%% @see listen/2
%% @end
%%------------------------------------------------------------------------------
-spec open(Name) -> Port when
      Name :: iodata(),
      Port :: mailbox().

open(Name) ->
    Port = do_open_port(),
    try open(Port, Name) of
	ok ->
	    Port
    catch
	error:Reason ->
	    close_port(Port),
	    erlang:raise(error, Reason, erlang:get_stacktrace())
    end.

%%------------------------------------------------------------------------------
%% @doc Delete an ITC mailbox.
%%
%% Will also consume any ``{'EXIT',Port,_}'' message from the port that comes
%% due to the port closing when the calling process traps exits.
%%
%% raises: `badarg'
%% @end
%%------------------------------------------------------------------------------
-spec close(Port) -> ok when
      Port :: mailbox().

close(Port) when is_port(Port) -> % itc_delete_mbox
    close_port(Port);
close(Port) -> 
    erlang:error(badarg, [Port]).

%%------------------------------------------------------------------------------
%% @doc Get the ITC mailbox id for the given port.
%%
%% raises: `badarg'
%% @end
%%------------------------------------------------------------------------------
-spec get_id(Port) -> Pid when
      Port :: mailbox(),
      Pid :: mailbox_id().

get_id(Port) when is_port(Port) -> % itc_get_id
    control(Port, $g, <<>>);
get_id(Port) -> 
    erlang:error(badarg, [Port]).

%%------------------------------------------------------------------------------
%% @doc Get the ITC mailbox name for the given mailbox id.
%%
%% raises: `badarg'
%% @end
%%------------------------------------------------------------------------------
-spec get_name(Port, MboxId) -> Name | undefined when
      Port :: mailbox(),
      MboxId :: mailbox_id(),
      Name :: binary().

get_name(Port, MboxId) when is_port(Port), ?IS_UINT32(MboxId) -> % itc_get_name
    Ref = control(Port, $n, <<MboxId:32>>),
    receive
	{Ref,Reply} ->
	    if
		is_list(Reply) ->
		    list_to_existing_atom(Reply);
		is_binary(Reply) ->
		    Reply
	    end
    end;
get_name(Port, MboxId) ->
    erlang:error(badarg, [Port,MboxId]).

%%------------------------------------------------------------------------------
%% @doc Hunt for mailbox by name.
%%
%% Will send `{mailbox_up, Port, Ref, Pid}'
%% to the calling process when the mailbox becomes available.
%%
%% Returns a reference term that can be used to cancel the hunt
%% using {@link dehunt/2}.
%%
%% raises: `badarg'
%%
%% @end
%%------------------------------------------------------------------------------
-spec hunt(Port, Name) -> Ref when
      Port :: mailbox(),
      Name :: iodata(),
      Ref :: hunt_ref().

hunt(Port, Name) when is_port(Port) -> % itc_locate_async
    Ref = control(Port, $l, Name),
    receive
	Ref ->
	    Ref
    end;
hunt(Port, Name) ->
    erlang:error(badarg, [Port,Name]).

%%------------------------------------------------------------------------------
%% @doc Stop hunting for mailbox.
%%
%% If a message for this hunt has been sent but not received
%% by the calling process, it is removed from the message queue.
%% Note that this only works if the same process that did
%% the hunt does the dehunt.
%%
%% raises: `badarg'
%%
%% @see hunt/2
%% @end
%%------------------------------------------------------------------------------
-spec dehunt(Port, Ref) -> ok when
      Port :: mailbox(),
      Ref :: hunt_ref().

dehunt(Port, {locate_ref,Port,R}=Ref) % remove from ref list
  when is_port(Port), ?IS_UINT32(R) ->
    Result = control(Port, $L, <<R:32>>),
    receive
	{mailbox_up,Port,Ref,true} ->
	    %% Found and deleted; had not fired
	    Result;
	{mailbox_up,Port,Ref,false} ->
	    %% Not found; may have fired
	    %% Read out the fired hunt reply
	    receive {mailbox_up,Port,Ref,_} ->
		    Result
	    after 0 ->
		    Result
	    end
    end;
dehunt(Port, Ref) ->
    erlang:error(badarg, [Port,Ref]).

%%------------------------------------------------------------------------------
%% @doc Attach to (Monitor a) mailbox.
%%
%% Will send `{mailbox_down, Port, Ref, MboxId}'
%% to the calling process if the mailbox disappears.
%%
%% Returns a reference that can be used to cancel the attachment
%% (monitor) using {@link detach/2}.
%%
%% raises: `badarg' | `enomem'
%%
%% @end
%%------------------------------------------------------------------------------
-spec attach(Port, MboxId) -> Ref when
      Port :: mailbox(),
      MboxId :: mailbox_id(),
      Ref :: attach_ref().

attach(Port, MboxId) when is_port(Port), ?IS_UINT32(MboxId) -> % itc_monitor
    Ref = control(Port, $m, <<MboxId:32>>),
    receive
	Ref ->
	    Ref
    end;
attach(Port, MboxId) -> 
    erlang:error(badarg, [Port,MboxId]).

%%------------------------------------------------------------------------------
%% @doc Remove attachment to (Demonitor a) mailbox.
%%
%% If a message for this monitor has been sent but not received
%% by the calling process, it is removed from the message queue.
%% Note that this only works of the same process
%% that did the attach does the detach.
%%
%% raises: `badarg'
%%
%% @see attach/2
%% @end
%%------------------------------------------------------------------------------
-spec detach(Port, Ref) -> ok when
      Port :: mailbox(),
      Ref :: attach_ref().

detach(Port, {monitor_ref,Port,R}=Ref) % itc_unmonitor and remove from ref list
  when is_port(Port), ?IS_UINT32(R) ->
    Result = control(Port, $M, <<R:32>>),
    receive
	{mailbox_down,Port,Ref,true} ->
	    %% Found and deleted; had not fired
	    Result;
	{mailbox_down,Port,Ref,false} ->
	    %% Not found; may have fired
	    %% Read out the fired hunt reply
	    receive {mailbox_down,Port,Ref,_} ->
		    Result
	    after 0 ->
		    Result
	    end
    end;
detach(Port, Ref) ->
    erlang:error(badarg, [Port,Ref]).

%%------------------------------------------------------------------------------
%% @doc Send an ITC message.
%%
%% The message is sent from the mailbox's own ID that is: `get_id(Port)'.
%%
%% raises: `badarg'
%%
%% @see send/5
%% @end
%%------------------------------------------------------------------------------
-spec send(Port, ToMboxId, MsgNo, MsgData) -> ok when
      Port :: mailbox(),
      ToMboxId :: mailbox_id(),
      MsgNo :: message_number(),
      MsgData :: iodata().

send(Port, ToMboxId, MsgNo, MsgData) % itc_send
  when is_port(Port), ?IS_UINT32(ToMboxId), ?IS_UINT32(MsgNo) ->
    Ref = control(Port, $s, [<<ToMboxId:32,MsgNo:32>>,MsgData]),
    receive
	Ref ->
	    ok
    end;
send(Port, ToMboxId, MsgNo, MsgData) ->
    erlang:error(badarg, [Port,ToMboxId,MsgNo,MsgData]).

%%------------------------------------------------------------------------------
%% @doc Send an ITC message with faked sender.
%%
%% As {@link send/4} but fake the sender to be `FromMboxId'.
%%
%% raises: `badarg'
%%
%% @see send/4
%% @end
%%------------------------------------------------------------------------------
-spec send(Port, ToMboxId, FromMboxId,MsgNo, MsgData) -> ok when
      Port :: mailbox(),
      ToMboxId :: mailbox_id(),
      FromMboxId :: mailbox_id(),
      MsgNo :: message_number(),
      MsgData :: iodata().

send(Port, ToMboxId, FromMboxId, MsgNo, MsgData) % itc_send
  when ?IS_UINT32(ToMboxId), ?IS_UINT32(FromMboxId), ?IS_UINT32(MsgNo) ->
    Ref =
	control(Port, $S, [<<ToMboxId:32,FromMboxId:32,MsgNo:32>>|MsgData]),
    receive
	Ref ->
	    ok
    end;
send(Port, ToMboxId, FromMboxId, MsgNo, MsgData) ->
    erlang:error(badarg, [Port,ToMboxId,FromMboxId,MsgNo,MsgData]).

%%------------------------------------------------------------------------------
%% @doc Start listening for any ITC message.
%%
%% The mailbox will send `{message,Port,{FromMboxId,ToMboxId,MsgNo,MsgData}}'
%% to the process that created the mailbox when an ITC message arrives.
%%
%% raises: `badarg' | `enomem'
%%
%% @end
%%------------------------------------------------------------------------------
-spec listen(Port) -> ok when
      Port :: mailbox().

listen(Port) when is_port(Port) ->
    %% Block nothing - listen to all
    Ref = control(Port, $T, <<>>),
    receive
	Ref ->
	    ok
    end;
listen(Port) ->
    erlang:error(badarg, [Port]).

%%------------------------------------------------------------------------------
%% @doc Start listening for specified ITC message numbers.
%%
%% The mailbox Will send `{message,Port,{FromMboxId,ToMboxId,MsgNo,MsgData}}'
%% to the process that created the mailbox when an ITC message with any
%% of the specified `MsgNo's arrives.
%%
%% The message numbers specification ``{'not',MsgNos}'' means receive
%% any message with a message number other than the listed.
%%
%% raises: `badarg' | `enomem'
%%
%% @end
%%------------------------------------------------------------------------------
-spec listen(Port, MsgNos) -> ok when
      Port :: mailbox(),
      MsgNos :: list(MsgNo) | {'not',list(MsgNo)},
      MsgNo :: message_number().

listen(Port, MsgNos) when is_port(Port), is_list(MsgNos) ->
    %% MsgNos == [] listens to nothing - block all
    case check_list_of_msgnos(MsgNos) of
	true ->
	    Ref = control(Port, $t, << <<MsgNo:32>> || MsgNo <- MsgNos >>),
	    receive
		Ref ->
		    ok
	    end;
	false ->
	    erlang:error(badarg, [Port,MsgNos])
    end;
listen(Port, {'not',NotMsgNos}=MsgNos)
  when is_port(Port), is_list(NotMsgNos) ->
    %% MsgNos == [] block nothing - listen to all
    case check_list_of_msgnos(NotMsgNos) of
	true ->
	    Ref = control(Port, $T, << <<MsgNo:32>> || MsgNo <- NotMsgNos >>),
	    receive
		Ref ->
		    ok
	    end;
	false ->
	    erlang:error(badarg, [Port,MsgNos])
    end;
listen(Port, MsgNos) ->
    erlang:error(badarg, [Port,MsgNos]).



%%==========================================================================
%% Application internal API
%%==========================================================================

-type init_opt() ::
	{driver_path,driver_path()} |
	{mailbox_count,mailbox_count()} |
	{alloc_scheme,alloc_scheme()} |
	{namespace,namespace()}.

%% @private
-spec open(Port, Name) -> ok when
      Port :: port(),
      Name :: iodata().
open(Port, Name) -> %% itc_create_mbox
    control(Port, $c, Name).

%% @equiv init([])
-spec init() -> already_initialized | ok.
init() ->
    init([]).

%% @doc Initialize ITC by calling `itc_init()'
%%
%% First loads the driver and then instructs it to call `itc_init()'.
%% This function is automatically called when the application starts,
%% with options from the application  environment, so it should not be
%% necessary to call it explicitly.
%%
%% Returns `already_initialized' if ITC already is initialized.
%%
%% Fails for bad arguments, if the driver can not lock itself into the VM,
%% or if the underlying `itc_init()' fails.
%%
%% raises: `badarg'
%%
%% @see start/1
%% @end
-spec init(Opts) -> already_initialized | ok when
      Opts :: [init_opt()];
	  (MailboxCount) -> already_initialized | ok when
      MailboxCount :: mailbox_count().
init(Opts) when is_list(Opts) ->
    Port = open_port(proplists:get_value(driver_path, Opts, [])),
    try init(Port, Opts)
    after
	close_port(Port)
    end;
init(MailboxCount) when is_integer(MailboxCount) ->
    init([{mailbox_count,MailboxCount}]).

%% @private
-spec init(Port, Opts) -> ok | already_initialized when
      Port :: port(),
      Opts :: [init_opt()];
	  (MailboxCount, NameSpace) -> ok | already_initialized when
      MailboxCount :: mailbox_count(),
      NameSpace :: namespace().
init(Port, Opts) when is_port(Port) -> % itc_init
    MailboxCount = proplists:get_value(mailbox_count, Opts, ?MAILBOX_COUNT),
    NS =
	case proplists:get_value(namespace, Opts, ?NAMESPACE) of
	    no_namespace ->
		[];
	    NameSpace ->
		[NameSpace,0]
	end,
    AllocScheme = proplists:get_value(alloc_scheme, Opts, ?ALLOC_SCHEME),
    Data =
	[case AllocScheme of
	     malloc ->
		 <<MailboxCount:32,$m>>;
	     {pool,Size} ->
		 <<MailboxCount:32,$p,Size:32>>;
	     {pool_flex,Size,{S0,S1,S2,S3,S4,S5,S6,S7}} ->
		<<MailboxCount:32,$f,Size:32,
		  S0:32,S1:32,S2:32,S3:32,S4:32,S5:32,S6:32,S7:32>>;
	     driver_alloc ->
		 <<MailboxCount:32,$d>>
	 end|NS],
    control(Port, $i, Data);
init(MailboxCount, NameSpace) when is_integer(MailboxCount) ->
    init([{mailbox_count,MailboxCount},{namespace,NameSpace}]).

%% @doc Exit ITC by calling `itc_exit()'
%%
%% This function is automatically called when the application stops,
%% so it should not be necessary to call it explicitly.  It blocks
%% opening of new mailboxes, stops all existing mailboxes and then
%% calls `itc_exit()'.
%%
%% Returns `ok' if successful.
%% If an exit is already in progress no mailboxes are stopped and
%% `already_exiting' is returned.
%% If `itc_exit()' already has been called no mailboxes are stopped and
%% `already_exited' is returned.
%%
%% Fails if the driver is not loaded or if the underlying `itc_exit()' fails.
%%
%% raises: `badarg'
%%
%% @see stop/0
%% @end
-spec exit() -> ok | already_exiting | already_exited.
exit() ->
    Port = do_open_port(),
    try exit(Port)
    after
	close_port(Port)
    end.

%% @private
-spec exit(port()) -> ok | already_exiting | already_exited.
exit(Port) when is_port(Port) ->
    case control(Port, $X, <<>>) of
	Result when is_atom(Result) ->
	    Result;
	{exiting_ref,_,_} = Ref ->
	    receive
		{exiting,Port,Ref,Ports} ->
		    [close_port(P) || P <- Ports, P =/= Port],
		    ok = control(Port, $x, <<>>)
	    end
    end.

%% @private
-spec open_port([string()]) -> port().
open_port(DriverPath) ->
    try do_open_port()
    catch
	error:badarg ->
	    load_driver(DriverPath),
	    try do_open_port()
	    catch
		error:badarg ->
		    unload_driver(),
		    erlang:raise(error, badarg, erlang:get_stacktrace())
	    end
    end.

%% @private
-spec close_port(Port) -> ok when
      Port :: port().
close_port(Port) ->
    catch erlang:port_close(Port),
    receive {'EXIT',Port,_} ->
	    ok
    after 0 ->
	    ok
    end.

%% @private
-spec unload_driver() -> ok.
unload_driver() ->
    case erl_ddll:unload_driver(?DRIVER_NAME_STRING) of
	ok ->
	    ok;
	{error,UnloadErr} ->
	    erlang:error({ddll_error,UnloadErr})
    end.

%% @private
-spec load_driver() -> ok.
load_driver() ->
    load_driver([]).

%% @private
-spec load_driver([string()]) -> ok.
load_driver(DriverPath) ->
    load_driver(
      case DriverPath of
	  [] ->
	      default_driver_path();
	  _ ->
	      DriverPath
      end,
      []).

%% @private
-spec default_driver_path() -> [string()].
default_driver_path() ->
    SourceDir =
	%% Locate the source build .libs directory via this module's
	%% ebin directory since there maybe is no priv dir
	%% so code:lib_dir/1 can go wrong.
	case code:which(?MODULE) of
	    Problem when is_atom(Problem) ->
		[];
	    ModuleFilename ->
		[filename:absname_join(
		   filename:absname(filename:dirname(ModuleFilename)),
		   filename:join(["..","c_src",".libs"]))]
	end,
    PrivDir = code:priv_dir(?APP),
    Arch = erlang:system_info(system_architecture),
    SourceDir ++
	[filename:join([PrivDir,Arch,"lib"]),
	 filename:join([PrivDir,"lib"])].



%%==========================================================================
%% Internal
%%==========================================================================

do_open_port() ->
    erlang:open_port(
      {spawn_driver,?DRIVER_NAME_STRING},
      [binary]).

load_driver([], PEs) ->
    Spec =
	{?DRIVER_NAME_STRING,
	 [{Path,erl_ddll:format_error(Error)} ||
	     {Path,Error} <- lists:reverse(PEs)]},
    erlang:error({no_driver,Spec});
load_driver([Path|Paths], PEs) ->
    case erl_ddll:load(Path, ?DRIVER_NAME_STRING) of
	ok ->
	    ok;
	{error,LoadErr} when is_atom(LoadErr) ->
	    erlang:error({ddll_error,LoadErr});
	{error,Error} ->
	    load_driver(Paths, [{Path,Error}|PEs])
    end.

control(Port, Command, Data) ->
    case erlang:port_control(Port, Command, Data) of
	<<>> ->
	    ok;
	<<$a,Atom/binary>> ->
	    erlang:binary_to_existing_atom(Atom, latin1);
	<<$b,Bin/binary>> ->
	    Bin;
	<<$r,Ref:32,RefName/binary>> ->
	    {erlang:binary_to_existing_atom(RefName, latin1),Port,Ref};
	<<$u,Uint:32>> ->
	    Uint
    end.

check_list_of_msgnos([]) ->
    true;
check_list_of_msgnos([MsgNo|MsgNos]) when ?IS_UINT32(MsgNo) ->
    check_list_of_msgnos(MsgNos);
check_list_of_msgnos(_) ->
    false.

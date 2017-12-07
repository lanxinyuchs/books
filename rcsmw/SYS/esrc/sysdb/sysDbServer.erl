%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysDbServer.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R11A/2

%%% @doc ==The DB service==
%%% Carries out init calls to each application which
%%% has exported an init function in its registered module

-module(sysDbServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R11A/2').
-date('2017-10-17').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-01-27 etxbjca     Created
%%% R1A/4      2012-02-03 etxjotj     Added the install_complete file
%%% -----      -------    --------   ------------------------
%%% R2A/12     2014-01-17 erarafo     Eliminate broken lines in Erlang log
%%% R2A/13     2014-01-17 erarafo     More informative log entries
%%% -----      -------    --------   ------------------------
%%% R3A/1      2015-01-14 etxpeno     Support for regular role
%%% R3A/2      2015-01-21 etxpeno     Support for init_board callbacks
%%% R3A/3      2015-01-27 etxpeno     Support disc_copies in regular
%%% R3A/4      2015-01-28 etxpeno     Remove support for standby role
%%% R3A/5      2015-02-03 etxpejn     Added net_kernel:connect_node in do_init
%%% R3A/6      2015-06-01 etxjotj     HT79394 Handle removed records
%%% R3A/7      2015-06-17 etxjotj     HT84003 Remove olddb after read
%%% -----      -------    --------   ------------------------
%%% R4A/1      2015-07-03 etxberb     Added install_begin and install_end.
%%% R4A/2      2015-07-07 etxberb     Replaced sysEnv with clhI.
%%% R4A/3      2015-08-07 etxberb     Corrections for installation of additional
%%%                                   MPs in a cluster.
%%% R4A/4      2015-08-21 etxjotj     Mnesia on tmp
%%% R4A/7      2015-09-15 etxarnu     wait_for_tables in do_init for regular 
%%% R5A/1      2015-10-07 etxjotj     Mnesia on tmp again
%%% R4A/8      2015-10-08 etxjotj     Mnesia move not necessary any longer
%%% R4A/9      2015-10-15 etxjotj     HU25078 Don't remove olddb
%%% -----      -------    --------   ------------------------
%%% R5A/3      2015-12-17 etxberb     Restructured installation phases, added
%%%                                   parallel execution phases and logging of
%%%                                   elapsed time per module, phase & total.
%%% R5A/4      2016-01-11 etxberb     Removed deprecated installation phases.
%%% R5A/5      2016-01-18 etxberb     Removed obsolete installation loggings.
%%% R5A/6      2016-03-04 etxtory     init_board never call on regular
%%% R5A/7      2016-04-11 etxtory     Take backup before install_complete
%%% -----      -------    --------   ------------------------
%%% R6A/1      2016-05-20 etxjotj     Removed legacy upgrade cases
%%% -----      -------    --------   ------------------------
%%% R7A/1      2016-10-12 etxberb     Added is_install_complete/0.
%%% R7A/2      2016-10-14 etxberb     Added swmBackup:init_config_prepare in
%%%                                   handle_upgrade/0.
%%% -----      -------    --------   ------------------------
%%% R8A/2      2017-01-24 etxberb     Exported get_init_modules/0.
%%% -----      -------    --------   ------------------------
%%% R11A/1     2017-09-05 etxjotj     Replaced swmDbMonitor, swmBackup with swmI
%%% R11A/2     2017-10-17 etxberb     Adaptions to OTP20.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([copy_mnesia_content/1]).
-export([is_install_complete/0]).
-export([vmstat/0]).
-export([get_init_modules/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-export([instPh_apply/3]).

-include_lib("xmerl/include/xmerl.hrl").

-define(FILE_install_complete, "install_complete").
-define(PATH_install_complete,
	filename:join(home_dir(), ?FILE_install_complete)).

-define(MonoTime, erlang:monotonic_time()).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(STACKTRACE_C,   % Current stacktrace
	element(2, process_info(self(), current_stacktrace))).
-define(STACKTRACE_E,   % Stacktrace at Exception
	erlang:get_stacktrace()).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start() ->
    gen_server:start_link({local, sysDbServer}, ?MODULE, [], []).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init(_) ->
    %% io:format("~p: Starting fprof~n", [?MODULE]),

    %% {ok, Tracer} = fprof:profile(start),
    %% fprof:trace([start,
    %% 		 {tracer, Tracer},
    %% 		 {procs, [self(),whereis(mnesia_controller)]}
    %% 		]),
    %% Code to profile

    Tables = mnesia:system_info(tables),
    ok = mnesia:wait_for_tables(Tables, 120000),

    %% fprof:trace(stop),
    %% fprof:analyse([{dest,"/tmp/fprof.anal"}]),
    %% io:format("~p:  fprof stopped~n", [?MODULE]),

    %% case timer:tc(mnesia, wait_for_tables, [Tables, 120000]) of
    %% 	{Time, ok} ->
    %% 	    io:format("~p: wait_for_tables took ~p secs~n",
    %% 		      [?MODULE, Time/1000000]);
    %% 	{Time, Res} ->
    %% 	    io:format("~p: wait_for_tables took ~p secs~n",
    %% 		      [?MODULE, Time/1000000]),
    %% 	    erlang:error(Res)
    %% end,

    handle_upgrade(),

    case is_install_complete() of
	true ->
	    ok;
	false ->
	    do_init(clhI:core_state())
    end,
    {ok, state}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
vmstat() ->
    Cmd = "vmstat -n 1 40",
    {Y, Mo, D} = date(),
    {H, Mi, S} = time(),
    Info =
	"~p collected memory and process information~n" ++
	"from start of installation phases, " ++
	"~4B-~2.10.0B-~2.10.0B::~2.10.0B:~2.10.0B:~2.10.0B~n" ++
	"os:cmd(~p)~n~s",
    Args = [?MODULE, Y, Mo, D, H, Mi, S, Cmd, os:cmd(Cmd)],
    error_logger:info_msg(Info, Args),
    exit(normal).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  Phases:
%%  * init
%%  * init_data
%%  * init_board
%%  * post_init
%%  Sub-phases within each phase above:
%%  * instPhSeqBeg
%%  * instPhParallel
%%  * instPhSeqEnd
%%% ###=====================================================================###
do_init(active) ->
    spawn(?MODULE, vmstat, []),
    T0 = ?MonoTime,
    Modules = get_init_modules(),
    sysInitI:info_report([{?MODULE, ?FUNCTION}, {modules, Modules}]),
    %% -------------- Installation phase -------------- begin
    info_msg("Running install_begin functions~n", []),
    T1 = ?MonoTime,
    instPh_runSeq(Modules, instPhSeqBeg_begin, ?MonoTime),
    instPh_runParallel(Modules, instPhParallel_begin, ?MonoTime),
    instPh_runSeq(Modules, instPhSeqEnd_begin, ?MonoTime),
    %% -------------- Installation phase -------------- init
    info_msg("Running init functions~n", []),
    T2 = ?MonoTime,
    instPh_runSeq(Modules, instPhSeqBeg_init, ?MonoTime),
    instPh_runParallel(Modules, instPhParallel_init, ?MonoTime),
    instPh_runSeq(Modules, instPhSeqEnd_init, ?MonoTime),
    %% -------------- Installation phase -------------- init_data
    info_msg("Running init_data functions~n", []),
    T3 = ?MonoTime,
    instPh_runSeq(Modules, instPhSeqBeg_init_data, ?MonoTime),
    instPh_runParallel(Modules, instPhParallel_init_data, ?MonoTime),
    instPh_runSeq(Modules, instPhSeqEnd_init_data, ?MonoTime),
    %% -------------- Installation phase -------------- init_board
    info_msg("Running init_board functions~n", []),
    T4 = ?MonoTime,
    instPh_runSeq(Modules, instPhSeqBeg_init_board, ?MonoTime),
    instPh_runParallel(Modules, instPhParallel_init_board, ?MonoTime),
    instPh_runSeq(Modules, instPhSeqEnd_init_board, ?MonoTime),
    %% -------------- Installation phase -------------- post_init
    info_msg("Running post_init functions~n", []),
    T5 = ?MonoTime,
    instPh_runSeq(Modules, instPhSeqBeg_post_init, ?MonoTime),
    instPh_runParallel(Modules, instPhParallel_post_init, ?MonoTime),
    instPh_runSeq(Modules, instPhSeqEnd_post_init, ?MonoTime),
    %% -------------- Installation phase -------------- end
    info_msg("Running install_end functions~n", []),
    T6 = ?MonoTime,
    instPh_runSeq(Modules, instPhSeqBeg_end, ?MonoTime),
    instPh_runParallel(Modules, instPhParallel_end, ?MonoTime),
    instPh_runSeq(Modules, instPhSeqEnd_end, ?MonoTime),
    %% -------------- Installation complete --------------
    T7 = ?MonoTime,
    Date = lists:flatten(io_lib:format("~w.~n", [calendar:local_time()])),
    
    %% Make sure that there is backup created before creating install_complete.
    info_msg("Creating autobackup ~n", []),
    ok = swmI:initial_auto_backup(),
    ok = file:write_file(?PATH_install_complete, list_to_binary(Date)),

    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  "-------------- Summary per phase --------------",
			  {'get modules', sysUtil:time_to_string(T1 - T0)},
			  {'Phase begin', sysUtil:time_to_string(T2 - T1)},
			  {'Phase init', sysUtil:time_to_string(T3 - T2)},
			  {'Phase init_data', sysUtil:time_to_string(T4 - T3)},
			  {'Phase init_board', sysUtil:time_to_string(T5 - T4)},
			  {'Phase post_init', sysUtil:time_to_string(T6 - T5)},
			  {'Phase end', sysUtil:time_to_string(T7 - T6)},
			  "-------------- Summary all phases --------------",
			  {'TOTAL', sysUtil:time_to_string(T7 - T0)}]),
    T8 = ?MonoTime,
    info_msg("Data initialization complete!~nGRAND TOTAL TIME: ~s~n",
	     [sysUtil:time_to_string(T8 - T0)]);
do_init(_CoreState) ->
    CoreNodes = clhI:erlang_nodes(core) -- [node()],
    info_msg("Connecting to active core node...~n\ttrying with core nodes ~p~n",
	     [CoreNodes]),
    ActiveNode = connect_to_active_node(CoreNodes),
    info_msg("...connected to active core node ~p~n", [ActiveNode]),

    info_msg("Connecting mnesia to node ~p...~n", [ActiveNode]),
    {ok, _} = rpc:call(ActiveNode,
		       mnesia,
		       change_config,
		       [extra_db_nodes, [node()]]),
    info_msg("...mnesia is connected,  waiting for tables~n"),
    Tables = mnesia:system_info(tables),
    ok = mnesia:wait_for_tables(Tables, 120000),
    
    Modules = get_init_modules(),
    %% -------------- Installation phase -------------- init_board
    info_msg("Running init_board functions~n",[]),
    instPh_runSeq(Modules, instPhSeqBeg_init_board, ?MonoTime),
    instPh_runParallel(Modules, instPhParallel_init_board, ?MonoTime),
    instPh_runSeq(Modules, instPhSeqEnd_init_board, ?MonoTime),

    Date = lists:flatten(io_lib:format("~w.~n",[calendar:local_time()])),
    ok = file:write_file(?PATH_install_complete, list_to_binary(Date)),
    info_msg("Data initialization complete!~n").

%%% ###########################################################################
%%% connect_to_active_node
%%%
%%% ###=====================================================================###
connect_to_active_node(ExtNodes) when is_list(ExtNodes) ->
    case connect_to_any_node(ExtNodes) of
	not_connected ->
	    ConnectedNode = wait_for_connection(ExtNodes),
	    connect_to_active_node(ConnectedNode);
	ConnectedNode ->
	    connect_to_active_node(ConnectedNode)
    end;
connect_to_active_node(ConnectedNode) when is_atom(ConnectedNode) ->
    case rpc:call(ConnectedNode, clhI, erlang_node, [active]) of
	[ConnectedNode] ->   % == ActiveNode
	    ConnectedNode;
	[ActiveNode] ->
	    net_kernel:connect_node(ActiveNode),
	    ActiveNode
    end.

%%% ###########################################################################
%%% connect_to_any_node
%%%
%%% ###=====================================================================###
connect_to_any_node([ExtNode | Tail]) ->
    case net_kernel:connect_node(ExtNode) of
	true ->
	    ExtNode;
	_ ->
	    connect_to_any_node(Tail)
    end;
connect_to_any_node([]) ->
    not_connected.

%%% ----------------------------------------------------------
%%% #           get_init_modules(Applications)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Finds out init modles from each loaded app
%%% ----------------------------------------------------------
get_init_modules([{App, _, _}|Loaded]) ->
    case application:get_key(App, mod) of
	{ok, {_, Data}} when is_list(Data) ->
	    case proplists:get_value(initmodule, Data) of
		undefined ->
		    get_init_modules(Loaded);
		Module ->
		    [Module|get_init_modules(Loaded)]
	    end;
	_ ->
	    get_init_modules(Loaded)
    end;
get_init_modules([]) -> [].

get_init_modules() ->
    Apps = get_apps(),
    get_init_modules(Apps).

get_apps() ->
    CxsFileP = filename:join([home_dir(), "software", "*-up.xml"]),
    [CxsFile] =
	case filelib:wildcard(CxsFileP) of
	    [] ->
		CxsFileP2 = filename:join([home_dir(), "software", "cxs*.xml"]),
		filelib:wildcard(CxsFileP2);
	    R -> R
	end,
    {CxsConfigurationE, []} = xmerl_scan:file(CxsFile),

    CxsProductE = find_element(product, CxsConfigurationE),
    CxsVersion = find_attribute(version, CxsProductE),

    RelFileP = filename:join([home_dir(), "releases", CxsVersion, "*.rel"]),
    [RelFile] = filelib:wildcard(RelFileP),
    %% case filelib:wildcard(RelFileP) of
    %%     [] ->
    %% 	warning_msg("Hardcoded version used when making the .rel file~n"),
    %% 	NewP = filename:join([home_dir(), "releases", "R1A", "*.rel"]),
    %% 	[F] = filelib:wildcard(NewP),
    %% 	F;
    %%     [RFile] ->
    %% 	RFile
    %% end,

    {ok, [{release, _, _, Apps}]} = file:consult(RelFile),
    Apps.

%%% ###########################################################################
%%% instPh_args
%%%
%%% ###=====================================================================###
instPh_args(Phase) when Phase == instPhSeqBeg_init orelse
			Phase == instPhParallel_init  orelse
			Phase == instPhSeqEnd_init orelse
			Phase == init ->
    Nodes = mnesia:system_info(running_db_nodes),
    [Nodes];
instPh_args(_) ->
    [].

%%% ###########################################################################
%%% instPh_runParallel
%%%
%%% ###=====================================================================###
instPh_runParallel(Modules, Func, T0) ->
    Args = instPh_args(Func),
    {ok, Results} =
	sysUtil:parallel_call([{?MODULE, instPh_apply, [M, Func, Args]} ||
				  M <- Modules]),
    T1 = ?MonoTime,
    OkResults = [case Result of
		    {ok, OkResult} ->
			OkResult
		 end
		 || Result <- Results],
    error_logger:info_report([{{?MODULE, phase}, Func} | OkResults] ++
			     [{'TOTAL', sysUtil:time_to_string(T1 - T0)}]).

%%% ----------------------------------------------------------
%%% #           instPh_runSeq(Modules::[atom()])
%%% Input: [Module] - A list of modules
%%% Output: ok
%%% Exceptions:
%%% Description: Call the function init/1 in the specified modules
%%% ----------------------------------------------------------

instPh_runSeq(Modules, Func, T0) ->
    instPh_runSeq(Modules, Func, [], T0).

instPh_runSeq([Module | Tail], Func, Results, T0) ->
    Args = instPh_args(Func),
    {ok, Result} = instPh_apply(Module, Func, Args),
    instPh_runSeq(Tail, Func, Results ++ [Result], T0);
instPh_runSeq([], Func, Results, T0) ->
    T1 = ?MonoTime,
    error_logger:info_report([{{?MODULE, phase}, Func} | Results] ++
			     [{'TOTAL', sysUtil:time_to_string(T1 - T0)}]),
    ok.

%%% ###########################################################################
%%% instPh_apply
%%%
%%% ###=====================================================================###
instPh_apply(Module, Func, Args) ->
    T0 = ?MonoTime,
    try apply(Module, Func, Args) of
	ok ->
	    T1 = ?MonoTime,
	    {ok, {Module, sysUtil:time_to_string(T1 - T0)}};
	Any ->
	    erlang:error({unknown_response, Any}, [Module])
    catch
	error : undef ->
	    case ?STACKTRACE_E of
		[{Module, _Func, _, _}|_] ->
		    {ok, {Module, undefined}};
		StackTrace ->
		    erlang:error({undef,StackTrace}, [Module])
	    end;
	Type : Reason ->
	    Stacktrace = ?STACKTRACE_E,
	    io:format("~n"),
	    error_logger:error_report([{mfa, {Module, Func, Args}},
				       {Type, Reason},
				       Stacktrace]),
	    erlang:Type(Reason, [Module])
    end.

%%% ----------------------------------------------------------
%%% #           handle_upgrade
%%% Input: 
%%% Output:
%%% Exceptions:
%%% Description: Search for upgrade database and read it
%%% ----------------------------------------------------------
handle_upgrade() ->
    swmI:init_config_prepare(),   % search for and move any downlaoded
 						% initial configuration to
 						% upgrade_init.
    %% HU29568
    %% The upgrade backup may be compressed. Check for file type, before using

    OldDb = filename:join(home_dir(), "upgrade_init"),

    case filelib:is_file(OldDb) of
	true ->
	    handle_upgrade_compressed(OldDb);
	false ->
	    ok
    end.    

handle_upgrade_compressed(Compressed) ->
    info_msg("Reading compressed old db ~p~n", [Compressed]),
    TmpPath = filename:join(sysEnv:tmp_dir(), "upgrade_init"),
    os:cmd(["gunzip -c ",Compressed, " > ",TmpPath]),
    read_old_db(TmpPath).

%%% ----------------------------------------------------------
%%% #           read_old_db
%%% Input: Path:string(), complete path to the old backup file
%%% Output:
%%% Exceptions:
%%% Description: For upgrade, this function reads a
%%%              backup created using the mnesia_backup module, and
%%%              stores the data in the ets tabl2 olddb.
%%% ----------------------------------------------------------
read_old_db(Path) ->
    info_msg("Reading old db: ~p~n", [Path]),
    Options = [{file, Path},
	       {name, make_ref()},
	       {repair, false},
	       {mode, read_only}],
    case disk_log:open(Options) of
	{ok, Fd} ->
	    ets:new(olddb, [set, public, named_table]),
	    Tab = ets:new(undefined, [set, public, {keypos,2}]),
	    ets:insert(olddb, {schema, Tab}),
	    do_read_old_db(Fd, start);
	{repaired, Fd, _, _} ->
	    ets:new(olddb, [set, public, named_table]),
	    Tab = ets:new(undefined, [set, public, {keypos,2}]),
	    ets:insert(olddb, {schema, Tab}),
	    do_read_old_db(Fd, start);
	{error, Reason} ->
	    erlang:error({error, Reason}, [Path])
    end.

do_read_old_db(Fd, Cont) ->
    case disk_log:chunk(Fd, Cont) of
	eof ->
	    disk_log:close(Fd);
	{error, Reason} ->
	    io:format(":-( ~p could not read config db file: ~p~n",
		      [?MODULE, {error, Reason}]),
	    erlang:error({error, Reason}, [Fd, Cont]);
	{Cont2, Terms} ->
	    insert_terms(Terms),
	    do_read_old_db(Fd, Cont2);
	{Cont2, Terms, _ } ->
	    insert_terms(Terms),
	    do_read_old_db(Fd, Cont2)
    end.

insert_terms(Terms) ->
    [begin
	 OrigTabName = element(1, Term),
	 case OrigTabName of
	     schema ->
		 TabName = element(2, Term),
		 case ets:lookup(olddb, TabName) of
		     [] ->
			 Tab2 = ets:new(undefined, [set, public, {keypos,2}]),
			 ets:insert(olddb, {TabName, Tab2});
		     [_] ->
			 ok
		 end,
		 [{_, Tab}] = ets:lookup(olddb, OrigTabName);
	     _ ->
		 [{_, Tab}] = ets:lookup(olddb, OrigTabName)
	 end,
	 %% HT79394
	 case Term of
	     {X, Key} -> % This object has been recorded as deleted;
		 info_msg("Deleting ~p~n", [{X,Key}]),
		 ets:delete(Tab, Key);
	     _ ->
		 ets:insert(Tab, Term)
	 end
     end||Term<-Terms, element(1, Term) =/= log_header],
    ok.


%%% ----------------------------------------------------------
%%% #           home_dir()
%%% Input: -
%%% Output: string()
%%% Exceptions:
%%% Description: Returns the local home directory
%%% ----------------------------------------------------------
home_dir() ->
    filename:join([root_dir(), "home", getenv("USER")]).

root_dir() ->
    getenv("RCS_ROOT").

getenv(Env) ->
    try os:getenv(Env) of
	false ->
	    ?LOG_ERR([{unknown_env, Env}]),
	    erlang:error(unknown_env,[Env]);
	Result ->
	    Result
    catch
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{env, Env},
		      {ErrClass, ErrReason},
		      {callstack, Stacktrace}]),
	    erlang:ErrClass(ErrReason)
    end.

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
        lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------
find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

wait_for_connection(ExtNodes) ->
    MyNodes = erlang:nodes(),
    ConnectedNodes =
	[{lists:member(ExtNode, MyNodes), ExtNode} || ExtNode <- ExtNodes],
    case lists:keyfind(true, 1, ConnectedNodes) of
	false ->
	    timer:sleep(250),
	    wait_for_connection(ExtNodes);
	{true, ConnectedNode} ->
	    ConnectedNode
    end.


%%% ----------------------------------------------------------
copy_mnesia_content(Node) ->
    case lists:member(Node,mnesia:table_info(clhMpConf,ram_copies)) of
	 false ->
	    info_msg("sysDbServer:copy_mnesia_content(~p)~n", [Node]),
	    lists:foreach(
	      fun(T) ->
		      {atomic, ok} = mnesia:add_table_copy(T, Node, ram_copies)
	      end,
	      mnesia:system_info(tables));
	true ->
	    ok % table already copied to Node
    end.

%%% ----------------------------------------------------------
is_install_complete() ->
    filelib:is_file(?PATH_install_complete).

info_msg(Format) ->
    info_msg(Format, []).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs.erl
%%
%% Description:
%%    This file contains the safs application interface
%%
%%--------------------------------------------------------------------
-module(safs).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/0,
	 start/1,
	 stop/0,
	 trace_start/0,
	 trace_stop/0,
	 trace_services/0,
	 trace_internal_services/0,
	 trace_services/1,
	 trace_remove_services/0,
	 trace_remove_internal_services/0,
	 trace_remove_services/1,
	 set_tp/1,
	 clear_tp/1,
	 trace_groups/0,
	 get_env/2,
	 set_env/2,
	 format_error/1
	]).

%%--------------------------------------------------------------------
%% Special exports
%%--------------------------------------------------------------------
-export([
	 rcs_prep_warm/0,
	 rcs_warm/0,
	 dump_imm_info/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 start/2,
	 init/1
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the application
%%--------------------------------------------------------------------
start() ->
    start(temporary).

%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the application
%%--------------------------------------------------------------------
start(Type) when Type == permanent; Type == temporary ->
    application:start(safs, Type).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stops the application
%%--------------------------------------------------------------------
stop() ->
    application:stop(safs).

%%====================================================================
%% Tracing
%%====================================================================
%%--------------------------------------------------------------------
%% Function: trace_start/0
%% Description:
%%--------------------------------------------------------------------
trace_start() ->
    safs_trace:start().
%%--------------------------------------------------------------------
%% Function: trace_stop/0
%% Description:
%%--------------------------------------------------------------------
trace_stop() ->
    safs_trace:stop().

%%--------------------------------------------------------------------
%% Function: trace_services/0/1
%% Description:
%%--------------------------------------------------------------------
trace_services() ->
    trace_services([{imm_om, all}, {imm_oi, all}, {ntf, all}]).

trace_internal_services() ->
    trace_services([{imm_db, all}, {com, all}]).

trace_services([]) ->
    ok;
trace_services([Svc |Svcs]) ->
    safs_trace:service_trace_points(Svc),
    trace_services(Svcs).

%%--------------------------------------------------------------------
%% Function: trace_remove_services/0/1
%% Description:
%%--------------------------------------------------------------------
trace_remove_services() ->
    trace_remove_services([{imm_om, all}, {imm_oi, all}, {ntf, all}]).

trace_remove_internal_services() ->
    trace_remove_services([{imm_db, all}, {com, all}]).

trace_remove_services([]) ->
    ok;
trace_remove_services([Svc |Svcs]) ->
    safs_trace:remove_service_trace_points(Svc),
    trace_remove_services(Svcs).

%%--------------------------------------------------------------------
%% Function: set_tp/1
%% Description:
%%--------------------------------------------------------------------
set_tp(TL) ->
    safs_trace:tp(TL).

%%--------------------------------------------------------------------
%% Function: clear_tp/1
%% Description:
%%--------------------------------------------------------------------
clear_tp(TL) ->
    safs_trace:ctp(TL).

%%--------------------------------------------------------------------
%% Function: trace_group/0
%% Description:
%%--------------------------------------------------------------------
trace_groups() ->
    [{imm_om, safs_imm_om:trace_groups()},
     {imm_oi, safs_imm_oi:trace_groups()},
     {ntf, safs_ntf:trace_groups()},
     {imm_db, safs_imm_db:trace_groups()},
     {com, safs_trace:com_trace_groups()},
     {log, safs_log:trace_groups()}].

%%--------------------------------------------------------------------
%% Function: format_error(SaAisError) -> Result
%%       SaAisError        = ais_error()
%%       Result            = string()
%% Description: Return a diagnostic error string.
%%--------------------------------------------------------------------
format_error(sa_ais_err_library) ->
    <<"An unexpected problem occurred in the library (such as corruption)."
	" The library cannot be used anymore.">>;
format_error(sa_ais_err_version) ->
    <<"The version parameter is not compatible with the version of the"
	" implementation of the Availability Management Framework or particular"
	" service.">>;
format_error(sa_ais_err_init) ->
    <<"A callback function that is required for this API has not been supplied"
	" in a previous call of 'SaArea':initialize().">>;
format_error(sa_ais_err_timeout) ->
    <<"An implementation-dependent timeout or the timeout specified in the API"
	" call occurred before the call could complete. It is unspecified"
	" whether the call succeeded or whether it did not.">>;
format_error(sa_ais_err_try_again) ->
    <<"The service cannot be provided at this time. The component"
	" or process might try again later.">>;
format_error(sa_ais_err_invalid_param) ->
    <<"A parameter is not set correctly.">>;
format_error(sa_ais_err_no_memory) ->
    <<"Either the service library or the provider of the service is out of"
	" memory and cannot provide the service.">>;
format_error(sa_ais_err_bad_handle) ->
    <<"A handle is invalid.">>;
format_error(sa_ais_err_busy) ->
    <<"Resource is already in use.">>;
format_error(sa_ais_err_access) ->
    <<"Access is denied.">>;
format_error(sa_ais_err_not_exist) ->
    <<"An entity, referenced by the invoker, does not exist.">>;
format_error(sa_ais_err_name_too_long) ->
    <<"Name exceeds maximum length.">>;
format_error(sa_ais_err_exist) ->
    <<"An entity, referenced by the invoker, already exists.">>;
format_error(sa_ais_err_no_space) ->
    <<"The buffer provided by the component or process is too small.">>;
format_error(sa_ais_err_interrupt) ->
    <<"The request was canceled by a timeout or other interrupt.">>;
format_error(sa_ais_err_no_resources) ->
    <<"Not enough resources.">>;
format_error(sa_ais_err_not_supported) ->
    <<"The requested function is not supported.">>;
format_error(sa_ais_err_bad_operation) ->
    <<"The requested operation is not allowed.">>;
format_error(sa_ais_err_failed_operation) ->
    <<"The requested operation failed.">>;
format_error(sa_ais_err_message_error) ->
    <<"A communication error occurred.">>;
format_error(sa_ais_err_queue_full) ->
    <<"The destination queue does not contain enough space for the"
     " entire message.">>;
format_error(sa_ais_err_queue_not_available) ->
    <<"The destination queue is not available.">>;
format_error(sa_ais_err_bad_flags) ->
    <<"The flags are invalid.">>;
format_error(sa_ais_err_too_big) ->
    <<"A value is larger than the maximum value permitted.">>;
format_error(sa_ais_err_no_sections) ->
    <<"There are no or no more sections matching the specified sections"
	" in the saCkptSectionIteratorInititialize() call.">>;
format_error(sa_ais_err_no_op) ->
    <<"The requested operation had no effect.">>;
format_error(sa_ais_err_repair_pending) ->
    <<"The administrative operation is only partially"
	" completed as some targeted components must be repaired.">>;
format_error(sa_ais_err_no_bindings) ->
    <<"There are no more bindings in the context.">>;
format_error(sa_ais_err_unavailable)->
    <<"The operation requested in this call is unavailable on this cluster"
        " node as the cluster node is not a member node or the handle was" 
        " acquired before the node left the cluster membership.">>;
format_error(sa_ais_err_access_denied) ->
    <<"The required access to a particular function of the AIS Service is"
        " denied due to a security violation.">>;
format_error(sa_ais_err_not_ready) ->
    <<"The readiness state can not be set.">>;
format_error(sa_ais_err_deployment) ->
    <<"The requested operation was accepted and applied at"
        " the information model level. However, its complete deployment"
        " in the running system may not be guaranteed at the moment.">>;
format_error(SaAisError) ->
    unicode:characters_to_binary(lists:flatten(io_lib:format("SAFS - Invalid error code: ~p", [SaAisError]))).


%%--------------------------------------------------------------------
%% Function: rcs_prep_warm/0 and rcs_warm/0
%% Description: These functions are used to get RT data cleared when an 
%%          application restarts (not just when IMM cluster is down) if
%%          the variable imm_clean_rt_on_oi_finalize' is set to true
%%--------------------------------------------------------------------
rcs_prep_warm() ->
    spawn(fun() -> dump_imm_info(), ok end),
    set_env(rcs_warm_restart, true).

rcs_warm() ->
    set_env(rcs_warm_restart, false).

%%--------------------------------------------------------------------
%% Function: dump_imm_info/0
%% Description: This function is used to get dump info about all 
%%              SAFS processes.
%%--------------------------------------------------------------------
dump_imm_info() ->
    case safs:get_env(info_dump_dir, undefined) of
	undefined ->
	    ok;
	DumpDir ->
	    {{Y, Mo, D}, {H, Mi, Sec}} = calendar:now_to_universal_time(os:timestamp()),
	    FileName = lists:flatten(io_lib:format("~w~2..0w~2..0w-~2..0w:~2..0w:~2..0w-imm_proc_info-2.log", 
						   [Y,Mo,D,H,Mi,Sec])), 
	    Path = filename:join(DumpDir, FileName),
	    %% Delete old imm_proc_info logs
	    delete_old_logs(DumpDir),
	    case file:open(Path, [write]) of
		{ok, F} ->
		    NamedProcs = [safs_imm_om, safs_imm_oi, safs_imm_special_applier,
				  safs_log_srv, safs_ntf_srv],
		    dump_named_proc_info(F, NamedProcs),
		    FilterFun = fun({_,X,_,_}) -> X end,
		    S = supervisor:which_children(safs_com_socketsup),
		    io:format(F, "\nSupervised Sockets:\n~p\n\n", [S]),
		    dump_proc_info(F, lists:map(FilterFun, S)),
		    P = supervisor:which_children(safs_com_insup),
		    io:format(F, "\nSupervised Proxys:\n~p\n\n", [P]),
		    dump_proc_info(F, lists:map(FilterFun, P)),
		    C = supervisor:which_children(safs_imm_om_ccb_sup),
		    io:format(F, "\nSupervised Ccbs:\n~p\n\n", [C]),
		    dump_proc_info(F, lists:map(FilterFun, C)),
		    io:format(F, "\n\nOI table:\n~p\n\n", [ets:tab2list(safs_imm_oi_user)]),
		    ok;
		E ->
		    error_logger:warning_msg("Couldn't open dump file: ~p, ~p\n", [Path, E])
	    end
    end.

dump_named_proc_info(_, []) ->
    ok;
dump_named_proc_info(F, [Name |Names]) ->
    io:format(F, "\nProcess Name: ~p\n", [Name]),
    dump_proc_info(F, [erlang:whereis(Name)]),
    dump_named_proc_info(F, Names).

dump_proc_info(_, []) ->
    ok;
dump_proc_info(F, [Pid |Pids]) ->
    dump_proc_info(F, Pid),
    dump_proc_info(F, Pids);
dump_proc_info(F, Pid) when is_pid(Pid) ->	 
    io:format(F, "Pid: ~p\nProcess Info:\n~p\nBacktrace:\n~p\n\n", 
	      [Pid, 
	       erlang:process_info(Pid),
	       erlang:process_info(Pid, [backtrace])]).

%%--------------------------------------------------------------------
%% Function: delete_old_imm/1
%% Description: Delete old imm_proc_info logs except recent ones
%%--------------------------------------------------------------------
delete_old_logs(DumpDir) ->
    %% Delete logs of old format (pre safs < 0.9), can be removed in the future.
    delete_old_logs_1(DumpDir, filelib:wildcard("*imm_proc_info.log", DumpDir)),
    
    LogFiles = filelib:wildcard("*imm_proc_info-2.log", DumpDir),
    SortedFiles = lists:reverse(lists:sort(LogFiles)),
    %% Number of logs to keep is info_max_logs(kept logs plus the new) - 1
    LogsToKeep = get_env(info_max_logs, 5) - 1, 
    if
	length(SortedFiles) > LogsToKeep ->
	    delete_old_logs_1(DumpDir, lists:nthtail(LogsToKeep, SortedFiles));
	true  ->
	    ok
    end.
    
delete_old_logs_1(_, []) ->
    ok;
delete_old_logs_1(Dir, [FileName|Fs]) ->
    file:delete(filename:join(Dir, FileName)),
    delete_old_logs_1(Dir, Fs).

%%====================================================================
%% Server functions
%%====================================================================
start(_, _) ->
    supervisor:start_link({local, safs_sup}, safs, safs_init).

init(safs_init) ->
    % ?INFO("safs starting\n", []),
    
    SupFlags = get_env(restart_policy, {one_for_one, 5, 1000}),

    ChildSpec = [
		 {safs_com_sup, {safs_com, start, [[]]},
		  permanent,
		  10000, supervisor, [safs_com]},
		 {safs_imm_oi, {safs_imm_oi, start_link, []},
		  permanent,
		  10000, worker, [safs_imm_oi]},
		 {safs_imm_om, {safs_imm_om, start_link, []},
		  permanent,
		  10000, worker, [safs_imm_om]},
		 {safs_imm_om_ccb_sup, {safs_imm_om_ccb_sup, start, [[]]},
		  permanent,
		  10000, supervisor, [safs_imm_om_ccb_sup]},
		 {safs_ntf_srv, {safs_ntf_srv, start, [{}]},
		  permanent,
		  10000, worker, [safs_ntf_srv]},
		 {safs_log_srv, {safs_log_srv, start_link, []},
		  permanent,
		  10000, worker, [safs_log_srv]},
		 {safs_imm_special_applier, {safs_imm_special_applier, start_link, []},
		  permanent,
		  10000, worker, [safs_imm_special_applier]}
		],
    {ok, {SupFlags, ChildSpec}}.

%%====================================================================
%% Internal utility functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: get_env/2
%% Description:
%%--------------------------------------------------------------------
get_env(Parameter, Default) ->
    check_value_with_default(Parameter, application:get_env(safs, Parameter), Default).


check_value_with_default(Par, Value, Default) when Par == imm_om_port;  
						   Par == imm_oi_port;  
						   Par == ntf_port;  
						   Par == log_port;
						   Par == imm_ct_port ->
    case Value of
	{ok, TcpPort} when TcpPort > 0,
	                   TcpPort < 65536 ->
	    TcpPort;
	{ok, {Ip, TcpPort}} when TcpPort > 0,
	                         TcpPort < 65536 ->
	    {ok, IpVal} = inet:parse_address(Ip),
	    {IpVal, TcpPort};
	_ ->
	    Default
    end;
check_value_with_default(imm_ct_cb, Value, Default) ->
     case Value of
	 {ok, CbModule} when is_atom(CbModule) ->
	     CbModule;
	  _ ->
	     Default
     end;
check_value_with_default(Par, Value, Default) when Par == log_root;
						   Par == log_cfg ->
    case Value of
	{ok, List} when is_list(List) ->
	    List;
	_ ->
	    Default
    end;
check_value_with_default(log_close, Value, Default) ->
    case Value of
	{ok, Bool} when is_boolean(Bool) ->
	    Bool;
	_ ->
	    Default
    end;
check_value_with_default(imm_notification_category, Value, Default) ->
    case Value of
	{ok, Category} when Category == all; 
			    Category == none;
			    Category == runtime;
			    Category == config ->
	    Category;
	_ ->
	    Default
    end;
check_value_with_default(imm_cb_timeout, Value, Default) ->
    case Value of
	{ok, TimeOut} when is_integer(TimeOut) ->
	    TimeOut;
	{ok, infinity} ->
	    infinity;
	_ ->
	    Default
    end;
check_value_with_default(Var, Value, Default) when Var == imm_ccb_completed_cb_timeout;
						   Var == imm_ccb_apply_cb_timeout;
						   Var == imm_ccb_abort_cb_timeout ->
    case Value of
	{ok, TimeOut} when is_integer(TimeOut) ->
	    TimeOut;
	{ok, infinity} ->
	    infinity;
	_ ->
	    safs:get_env(imm_cb_timeout, Default)
    end;
check_value_with_default(imm_sync_cb, Value, Default) ->
    case Value of
	{ok, {M, F}} when is_atom(M), is_atom(F) ->
	    {M, F};
	_ ->
	    Default
    end;
check_value_with_default(imm_clean_rt_on_oi_finalize, Value, Default) ->
    case Value of
	{ok, Bool} when Bool =:= true; Bool =:= false ->
	    Bool;
	_ ->
	    Default
    end;
check_value_with_default(rcs_warm_restart, Value, Default) -> 
    case Value of
	{ok, Bool} when Bool =:= true; Bool =:= false ->
	    Bool;
	_ ->
	    Default
    end;
check_value_with_default(restart_policy, Value, Default) -> 
    case Value of
	{ok, T} when is_tuple(T) ->
	    T;
	_ ->
	    Default
    end;
check_value_with_default(info_dump_dir, Value, Default) -> 
    case Value of
	{ok, T} when is_list(T) ->
	    T;
	_ ->
	    Default
    end;
check_value_with_default(info_max_logs, Value, Default) -> 
    case Value of
	{ok, T} when is_list(T) ->
	    T;
	_ ->
	    Default
    end;
check_value_with_default(_Par, _Value, _Default) ->
    {error, sa_ais_err_not_exist}.


%%--------------------------------------------------------------------
%% Function: set_env/2
%% Description:
%%--------------------------------------------------------------------
set_env(Parameter, Value) ->
    case check_value(Parameter, Value) of
	ok ->
	    application:set_env(safs, Parameter, Value);
	_ ->
	    {error, sa_ais_err_invalid_param}
    end.

check_value(imm_notification_category, Value) when Value == all; 
						   Value == none;
						   Value == runtime;
						   Value == config ->
    ok;
check_value(rcs_warm_restart, Value) when Value =:= true; 
					  Value =:= false ->
    ok;
check_value(_ ,_) ->
    error.

%%----------------------------------------------------------------------
%% END OF MODULE
%%----------------------------------------------------------------------



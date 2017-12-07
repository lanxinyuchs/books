%% ===========================================================================
%% Copyright (c) Ericsson AB 2010 All rights reserved.
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
%% ===========================================================================
%%
%% @copyright Ericsson AB 2013-2017
%% @doc
%% This module provides a simple wrapper for Erlang/OTP application 'dbg'
%% tp and tpl functions.
%%
%% For further details, refer to dbg reference guide.
%% 
%% == Usage ==
%% 
%% This module provides Erlang tracing on any combination of 
%% Module, Function, and Arity. Further it is possible to define
%% if return value and/or calling function will be printed.
%% 
%% The traced module will be loaded if not already loaded,
%% if the module is included in the path. 
%% (Otherwise refer to code:add_path/1).
%% 
%%
%% == Examples ==
%% === tp and tpl ===
%% A chronological example of usage
%%
%% Star a trace on pmsJob, external calls only
%% <pre>
%% pmsDebug:tp(d, pmsJob).
%% </pre>
%%
%% Stop the trace on pmsJob
%% <pre>
%% pmsDebug:tp(s, pmsJob).
%% </pre>
%%
%% Add a trace on pmsJob:create, all arities. Print also the return trace
%% <pre>
%% pmsDebug:tp(r, pmsJob, create).
%% </pre>
%%
%% Stop trace pmsJob:create
%% <pre>
%% pmsDebug:tp(s, pmsJob, create).
%% </pre>
%%
%% Add a trace on pmsJob:create/1, i.e. arity one only.
%% Print the return trace and the calling function
%% <pre>
%% pmsDebug:tp(cr, pmsJob, create, 1).
%% </pre>
%%
%% Stop all tracing on pmsJob
%% <pre>
%% pmsDebug:tp(s, pmsJob).
%% </pre>
%% 
%% Stop all tracing on all modules.
%% <pre>
%% pmsDebug:stop_clear().
%% </pre>
%% 
%% === using match spec ===
%% 
%% ms/3 takes a match_spec() as the third parameter, see further
%% <url href="http://www.erlang.org/doc/apps/erts/match_spec.html">Match specifications in Erlang</url>
%% <pre>
%% pmsDebug:ms(my_mod, my_fnc, [{{'_', '$2'}, [{'==', false, {element, 1, '$2'}}], [{message, {exception_trace}}]}]).
%% </pre>
%% 
%%
%% To create simple match_specs create_ms/5 or create_ms/6 can be used.
%% By simple is meant only matching on integers, atoms, fully specified tuples,
%% etc; no complicated structures using '_'.
%% 
%% Add a trace on osegw_server:send/4 when the 3rd parameter (SigNo) is not 123.
%% First use create_ms/6 to create a fun to be used in ms/3.
%% Use the second printed line (the one with 4 parameters) to start the trace.
%% <pre>
%% > pmsDebug:create_ms(r, osegw_server, send, 3, '/=', 123).
%% pmsDebug:ms(osegw_server, send, fun([P1,P2,P3,P4,P5]) when P3 /= 123 -> exception_trace() end).
%% pmsDebug:ms(osegw_server, send, fun([P1,P2,P3,P4]) when P3 /= 123 -> exception_trace() end).
%% ok
%% </pre>
%% 
%% Add a trace on cpp_telnet:open/4 when the 1st parameter (Host) 
%% is not "cppemu12", Note the extra \" to make the value to a string.
%% First use create_ms/6 to create a fun to be used in ms/3.
%% Use the second printed line (the one with 4 parameters) to start the trace.
%% <pre>
%% > pmsDebug:create_ms(r, cpp_telnet, open, 1, '==', "\"cppemu12\"").
%% pmsDebug:ms(cpp_telnet, open, fun([P1,P2,P3,P4,P5,P6]) when P1 == "cppemu12" -> exception_trace() end).
%% pmsDebug:ms(cpp_telnet, open, fun([P1,P2,P3,P4]) when P1 == "cppemu12" -> exception_trace() end).
%% pmsDebug:ms(cpp_telnet, open, fun([P1,P2]) when P1 0= "cppemu12" -> exception_trace() end).
%% ok
%% </pre>
%% 
%% === ts ===
%% Using this function it is possible to toggel timestamp printouts
%% when debugging with tp and tpl
%%
%% === compile and versions ===
%% Use these functions to printout the compile times and versions of 
%% Erlang modules
%%
%%
%% == Bugs ==
%% NOTE: If you reload a module all tracing will be stopped on that module.
%% 
%% @type bool_func() = atom().
%%
%% Refer to BoolFunction in Match specifications in Erlang:
%% <url href="http://www.erlang.org/doc/apps/erts/match_spec.html">Match Spec</url>, 
%% all is_*/1 are valid bool_func terms.
%% 
%% @type guard_func() = '>' | '>=' | '<' | '=<' | '=:=' | '==' | '=/=' | '/='
%%
%%
%% @end

-module(pmsDebug).
-vsn('/main/R2A/R3A/R5A/R6A/R7A/R8A/1').
-date('2017-01-05').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    --------------------------------------
%%% R1A/1      2012-01-18 uabesvi     Created
%%% ------------------------------------------------------------------------
%%%
%%% ------------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ------------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ------------------------------------------------------------------------

%% interface
-export([compiled/0]).
-export([compiled/1]).
-export([create_ms/5]).
-export([create_ms/6]).
-export([ts/1]).
-export([log_dbg/0]).
-export([ms/3]).
-export([stop_clear/0]).
-export([tm/3]).
-export([tm/4]).
-export([tp/2]).
-export([tp/3]).
-export([tp/4]).
-export([tpl/2]).
-export([tpl/3]).
-export([tpl/4]).
-export([versions/0]).
-export([versions/1]).
-export([get_versions/0]).
-export([get_versions/1]).
-export([get_versions/2]).
-export([show_counters/1]).

-export([fragment/0]).
-export([procs/0]).
-export([tables/0]).
-export([tables/1]).
-export([aliases/0]).
-export([get_aliases/0]).

-export([set_log_truncate_level/0]).
-export([set_log_truncate_level/1]).

-export([counter_filter/1]).
-export([modules/1]).

-export([memory/0]).
-export([memory/1]).

-export([help/1]).

-define(VALID_DBG_OPT, [caller, return]).
-define(RETURN, {message, {exception_trace}}).
-define(CALLER, {message, {caller}}).

-define(ECIM_TABS, [pmMeasurementCapabilities,
		    pmGroup,
		    measurementType,
		    pmJob,
		    measurementReader]).

-define(INTERNAL_TABS, [pmsAppsInfo,
			pmsAppRegistry,
			pmsEnv,
			pmsScMoClasses,
			pmsScAppMoLdns,
			pmsCounterAliases]).



-include("pms.hrl").
-include("RcsPm.hrl").

%% ===========================================================================
%% EXPORTED FUNCTIONS
%% ===========================================================================

help(fragment) ->
    p("~nfragment prints the Pm instances. ~n"),
    p("~n");
help(procs) ->
    p("~nprocs prints information about the PMS internal processes. ~n"),
    p("~n");
help(tables) ->
    p("~nprocs prints information about the PMS internal tables. ~n"),
    p("~n");
help(aliases) ->
    p("~naliases prints information about the aliases specified by the application. ~n"),
    p("~n");
help(versions) ->
    p("~nversions prints the PMS module versions. ~n"),
    p("~n");
help(compiled) ->
    p("~ncompiled prints the compile time of the PMS modules. ~n"),
    p("~n");
help(set_log_truncate_level) ->
    p("~nSet number of counters/counter values that are printed in RcsPmCounters logs.~n"
      "Arity 0 sets the default value. Level must be less than 100.~n"),
    p("~n");

help(X) ->
    p("~nUnknown function ~p~n", [X]).
    
p(S)    -> io:format(S).
p(S, A) -> io:format(S, A).


%% ===========================================================================
%% @spec compiled() -> ok
%%
%% 
%% @doc
%% 
%% Prints the compile time for pms modules.
%% 
%% @end
%% ===========================================================================	
compiled() ->
    [code:ensure_loaded(M) || M <- ?PMS_MODS],
    compiled(pms).

%% ===========================================================================
%% @spec compiled(Modules) -> ok
%%
%% where
%%     Modules = [Module] | Prefix
%%     Module  = atom()
%%     Prefix  = atom()
%% 
%% @doc
%% 
%% Prints the compile time for listed modules or modules starting with Prefix.
%% versions(pms) will print all the pms modules.
%% 
%% @end
%% ===========================================================================	
compiled(Modules)
  when is_list(Modules) ->
    get_attributes(Modules, fun compiled/2);
compiled(Prefix) ->
    compiled(modules(Prefix)).


compiled(Width, Mod) ->
    io:format(": ~*s~19s  ~s~n", [-Width,
                                  Mod,
                                  attr(Mod, time),
                                  opt(attr(Mod, date))]).

opt("-") ->
    "";
opt(D) ->
    "(" ++ D ++ ")".


%% ===========================================================================	
%% @spec versions() -> ok
%%
%% 
%% @doc
%% 
%% Prints the versions of pms modules.
%% @end
%% =========================================================================== 
versions() ->
    [code:ensure_loaded(M) || M <- ?PMS_MODS],
    versions(pms).

%% ===========================================================================	
%% @spec versions(Modules) -> ok
%%
%% where
%%     Modules = [Module] | Prefix
%%     Module  = atom()
%%     Prefix  = atom()
%% 
%% @doc
%% 
%% Prints the versions of the specified modules.
%% versions(pms) will print all the pms modules.
%% @end
%% =========================================================================== 
versions(Modules) ->
    get_attributes(Modules, fun(W,M) -> attr(W, M, vsn, fun vsn/1) end),
    ok.

%% vsn/1

vsn("/main/" ++ V) ->
    V;
vsn(T) when is_atom(T) ->
    vsn(atom_to_list(T));
vsn(T) ->
    attr(T).

is_char(C) ->
    0 =< C andalso C < 256.


%%========================================================================
%% get_versions() -> ok.
%% 
%%========================================================================
get_versions() ->
    get_versions(pms, ?PMS_MODS).

%%========================================================================
%% get_versions(Prefix) -> ok.
%% 
%% This wont guarantee that all modules will be displayed,
%% only those which have been loaded.
%%========================================================================
get_versions(Prefix) when is_atom(Prefix) ->
    All  = code:all_loaded(), 
    Mods = [M || {M, _} <- All, lists:prefix(atom_to_list(Prefix), 
					     atom_to_list(M))],
    get_versions(Prefix, Mods).

%%========================================================================
%% get_versions(Prefix, Modules) -> ok.
%% 
%%========================================================================
get_versions(Prefix, Modules) when is_atom(Prefix) ->
    [code:ensure_loaded(M) || M <- Modules],
    Versions = pmsLib:get_version_attributes(Prefix),
    Text = "Versions:~n" ++ string:join(lists:reverse(Versions), "~n") ++ "~n",
    io_lib:format(Text, []).


%% ===========================================================================
%% @spec show_counters(LDN) -> ok
%%
%% @doc
%% Show all PmGroups and MeasurementTypes for a LDN
%% @end
%% ===========================================================================	
show_counters(Ldn) ->
    [_, MoClass | _] = lists:reverse(string:tokens(Ldn, "=,")),
    Grps     = get_grps(pmsDb:pm_group_find_mo_class(MoClass)),
    Counters = get_counters(Grps),
    Pids     = get_pids(Grps),
    Jobs     = get_jobs(Counters, []),
    io:format("==================================~n"),
    sc_print(Counters, Pids, Jobs).

   

sc_print([], [], []) ->
    ok;
sc_print([{Grp, Cs} | TG], [{P, Meas} | TP], [Job | TJ]) ->
    Pid = case P of
	       {ok, Pidens} -> Pidens;
	       Error        -> Error
	   end,


    io:format("~s~s~s==================================~n",
	      [scp_grp(Grp, Cs), scp_pid(Pid, Meas, Grp), scp_job(Job, Grp)]),
    sc_print(TG, TP, TJ).


scp_grp(Grp, Cs) ->
    io:format("Group      : ~p~n"
	      "  Counters : ~p~n~n",
	      [Grp, lists:sort(Cs)]),
    "".

scp_pid(Pid, Meas, Grp) ->
    Cs = proplists:get_value(Grp, Meas),

    io:format("AppJobPid  : ~p~n"
	      "  Counters : ~p~n~n",
	      [Pid, lists:sort(Cs)]),
    "".

scp_job(Job, Grp) ->

    {ok, MeasReaders} = pmsDb:measurement_reader_match(Job),

    MSs = [MS || #measurementReader{measurementSpecification = MS} 
		     <- MeasReaders],

    GRs = [lists:reverse(string:tokens(binary_to_list(GR), "=,")) || 
	      #'MeasurementSpecification'{groupRef = GR} 
		  <- MSs, is_binary(GR)],
    
    GrpRef = [{"1", "1", "1", G} || [G | _] <- GRs, G == Grp],

 
    TRs = [string:tokens(binary_to_list(TR), "=,") || 
	      #'MeasurementSpecification'{measurementTypeRef = TR} 
		  <- MSs, is_binary(TR)],
    
    TypeRefs = [G || [_, _, G | _] <- TRs, G == Grp],
    
    MrTypesG = 
	case GrpRef of
	    [] -> 
		[];
	    [Ref] ->
		{ok, MtG} = pmsDb:measurement_type_match(Ref),
		
		MtG
	end,
    
    MrTypesT = [pmsDb:measurement_type_get(Grp, T) || T <- TypeRefs],

    io:format("JobIds     : ~p~n"
	      "  Counters : ~p~n",
	      [Job, spc_j(MrTypesG ++ MrTypesT)]),
    "".


spc_j(MTs) ->
    [C || #measurementType{measurementTypeId = {_, _, _, _, C}} <- MTs].


    

get_grps({ok, Recs}) ->
    {ok, [Grp || #pmGroup{pmGroupId = {_, _, _, Grp}} <- Recs]};
get_grps(_Error) ->
    {error, no_pm_groups}.

get_counters({ok, Grps}) ->
    get_counters(Grps, []);
get_counters(Error) ->
    Error.

get_counters([], Acc) ->
    Acc;
get_counters([PmGroup | T], Acc) ->
    {ok, MTs} = pmsDb:measurement_type_match({"1", "1", "1", PmGroup}),
    MT = [Mid || #measurementType{measurementTypeId = {_,_,_,_,Mid}} <- MTs],
    get_counters(T, [{PmGroup, MT} | Acc]).
    

get_pids({ok, Grps}) ->
    lists:append([gp(Grp) || Grp <- Grps]);
get_pids(Error) ->
    Error.

gp(Grp) ->
    case pmsDb:app_reg_get([Grp]) of
	{ok, AppRegs} ->
	    Pids = [Pid || 
		       #pmsAppRegistry{job_pid = Pid} = AppReg <- AppRegs,
		       filter_reg_pids(AppReg)],
	    PidSort = lists:usort(Pids),
	    [{Pid, gp_loop_data(Pid)} || Pid <- PidSort];
	_ ->
	    {error, ?SC_NO_COUNTERS}
    end.

gp_loop_data(Pid) ->
    gp_loop_data([pmsAppJob], Pid, {error, not_found}).
gp_loop_data([], _Pid, Res) ->
    Res;
gp_loop_data([Mod | T], Pid, _) ->
    Meas = try
	       Mod:gld(Pid, self()),
	       receive
		   {loop_data, LD} ->
		       gp_ld(proplists:get_value(measurements, LD))
	       after 5000 ->
		       {error, {timeout, Pid}}
	       end
	   catch error:X ->
		   {error, X}
	   end,
    case Meas of
	{error, _} = Error ->
	    gp_loop_data(T, Pid, Error);
	Meas ->
	    Meas
    end.


gp_ld([{_, Grps}]) ->
    gp_ld(Grps, []).

gp_ld([], Acc) ->
    Acc;
gp_ld([{Grp, MTs} | T], Acc) ->
    Cs = [element(2, MT) || MT <- MTs],
    gp_ld(T, [{Grp, Cs} | Acc]).


filter_reg_pids(#pmsAppRegistry{pmi_cb = {_CbMod, Callbacks}}) ->
    proplists:get_value(?REPORT_SC_CB, Callbacks, false);
filter_reg_pids(#pmsAppRegistry{pmi_cb = PmiCb}) when is_atom(PmiCb) ->
    true.



get_jobs([], Acc) ->
    lists:append(lists:reverse(Acc));
get_jobs([{PmGrp, Types} | Rem], Acc) ->
    MrG    = pmsDb:measurement_reader_match_pm_group(PmGrp),
    MrTemp = [pmsDb:measurement_reader_match_meas_type(PmGrp, T) || T <- Types],
    MrT    = lists:append([MR || {ok, MR} <- MrTemp]),
    get_jobs(Rem, [lists:usort(gvd(MrG, MrT)) | Acc]).

    
gvd({ok, MrG}, MrT) ->
    Mids = [Mid || #measurementReader{measurementReaderId = Mid} <- MrG ++ MrT],
    [{ME, SF, PM, J} || {ME, SF, PM, J, _} <- Mids].



	    

%% ===========================================================================
%% @spec stop_clear() -> ok
%%
%% @doc
%% Removes all tracing and kills the trace process.
%% @end
%% ===========================================================================	
stop_clear() ->
    dbg:stop_clear(),
    ok.


%% ===========================================================================
%% @spec tm(Module, StartFnc, StopFnc) 
%%
%%       -> ok
%% 
%% @doc
%% @equiv tm(Module, StartFnc, StopFnc, [])
%% @end
%% ===========================================================================	
tm(Module, StartFnc, StopFnc) -> tm(Module, StartFnc, StopFnc, []).

%% ===========================================================================
%% @spec tm(Module, StartFnc, StopFnc, FilteredFncs) 
%%
%%       -> ok
%% 
%% where
%%   Moudle     = atom()
%%   StartFnc   = atom() 
%%   StopFnc    = atom()
%%   FilterFncs = [FilterFnc]
%%   FilterFnc  = atom()
%%   
%% @doc
%%    Start tracing in Module when StartFnc is called and stop
%%    the tracing when StopFnc returns.
%%    It is possible to filter functions to not to be traced
%%    by defining them in FilteredFncs.
%%
%%    The traces contains the function call and its return value.
%%    NOTE: tm stops all other ongoing tracing.
%%    
%% @end
%% ===========================================================================	
tm(Module, StartFnc, StopFnc, FilteredFnc) -> 
    stop_clear(),
    dbg:tracer(process, 
	       {fun tm_dbg/2, {{StartFnc, StopFnc, FilteredFnc}, false}}),
    dbg:p(all, [c, timestamp]),
    dbg:tpl(Module, '_', '_', [{'_', [], [{message, {exception_trace}}]}]),
    ok.
    



%% ===========================================================================
%% @spec tp(Cmd, Module) 
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd    = d | r | c | cr | s
%%   Module = atom() | '_'
%% 
%% @doc
%% Equivalent to tp(Cmd, Module, '_', '_').
%%
%% For Cmd see tp/4
%% @end
%% ===========================================================================	
tp(Cmd, Module) -> dbg(tp, Cmd, [Module]).

%% ===========================================================================
%% @spec tp(Cmd, Module, Function) 
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd      = d | r | c | cr | s
%%   Module   = atom() 
%%   Function = atom() | '_'
%% 
%% @doc
%% Equivalent to tp(Cmd, Module, Function, '_').
%% 
%% For Cmd see tp/4
%% @end
%% ===========================================================================	
tp(Cmd, Module, Function) -> dbg(tp, Cmd, [Module, Function]).

%% ===========================================================================
%% @spec tp(Cmd, Module, Function, Arity) 
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd      =  d | r | c | cr | s
%%   Module   = atom() 
%%   Function = atom() | '_'
%%   Arity    = integer() | '_'
%% 
%% @doc
%% This function enables call trace for one or more functions. 
%% All exported functions matching the {Module, Function, Arity} 
%% argument will be concerned.
%%
%% The Module, Function and/or Arity parts of the tuple may be specified as
%% the atom '_' which is a "wild-card" matching all modules/functions/arities. 
%% Note, if the Module is specified as '_', the Function and Arity parts have 
%% to be specified as '_' too. The same holds for the Functions relation to 
%% the Arity.
%%
%% If Module is not '_'  the module will be loaded.
%%
%% The tag can have the following values:
%% <pre>
%%   d  - start debugging on function calls specified
%%   r  - same as d, but also shows the return value of the function calls
%%   c  - same as d, but also shows the calling function
%%   cr - combination of r and c
%%   s  - stop tracing on funcion calls specified
%% </pre>
%% 
%% @end
%% ===========================================================================	
tp(Cmd, Module, Function, Arity) -> dbg(tp, Cmd, [Module, Function, Arity]).


%% ===========================================================================
%% @spec  tpl(Cmd, Module) 
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd    = d | r | c | cr | s
%%   Module = atom() 
%% 
%% @doc
%% Equivalent to tpl(Cmd, Module, '_', '_') 
%% 
%% For Cmd see tp/4
%% @end
%% ===========================================================================	
tpl(Cmd, Module) -> dbg(tpl, Cmd, [Module]).

%% ===========================================================================
%% @spec tpl(Cmd, Module, Function) 
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd      = d | r | c | cr | s
%%   Module   = atom() 
%%   Function = atom() | '_'
%% 
%% @doc
%% Equivalent to tpl(Cmd, Module, Function, '_') 
%% 
%% For Cmd see tp/4
%% @end
%% ===========================================================================	
tpl(Cmd, Module, Function) -> dbg(tpl, Cmd, [Module, Function]).

%% ===========================================================================
%% @spec tpl(Cmd, Module, Function, Arity) 
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd      = d | r | c | cr | s
%%   Module   = atom() 
%%   Function = atom() | '_'
%%   Arity    = integer() | '_'
%% 
%% @doc
%% This function works as tp/4, but enables tracing for local calls 
%% (and local functions) as well as for global calls (and functions).
%% 
%% For Cmd see tp/4
%% @end
%% ===========================================================================	
tpl(Cmd, Module, Function, Arity) -> dbg(tpl, Cmd, [Module, Function, Arity]).




%% ===========================================================================
%% @spec ms(Module, Function, MS)
%%
%%       -> ok | error
%%
%% where
%%
%%   Module   = atom() 
%%   Function = atom() | '_'
%%   MS       = match_spec() | function()
%% 
%% @doc
%% Start debugging using a match spec. 
%% 
%% Refer to create_ms/5/6 how to create a call to this function.
%% Refer further to dbg:tpl/3 and erts user's guide for match specs.
%% @end
%% ===========================================================================	
ms(Module, Function, MS) 
  when is_atom(Module),
       is_atom(Function),
       is_function(MS) -> 
    dbg_init(),
    %% This is a workaround because caller does not work otherwise
    MS2 = case dbg:fun2ms(MS) of
	      [{X, Y, [{caller}]}] -> [{X, Y, [{message, {caller}}]}];
	      MS1 -> MS1
	  end,
    dbg:tpl(Module, Function, MS2);

ms(Module, Function, MS) 
  when is_atom(Module),
       is_atom(Function),
       is_list(MS) -> 
    
    dbg_init(),
    dbg:tpl(Module, Function, MS);



ms(_, _, _) ->
    io:format("Illegal parameters~n", []).




%% ===========================================================================
%% @spec create_ms(Cmd, Module, Function, Parameter, BoolFunc)
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd       = d | r | c | cr | s
%%   Module    = atom() 
%%   Function  = atom() | '_'
%%   Parameter = integer()
%%   BoolFunc  = bool_func()
%% 
%% @doc
%% Prints a function call to be used when invoking ms/3.
%% 
%% This command will not start any tracing but is a help function to
%% create match specs to be used in ms/3.
%% It will print one command for each arity of Function.
%% The print-out can be pasted into an Erlang window to start the trace.
%% 
%% For the parameters, refer to create_ms/6
%%<pre> 
%% Cmd       = See tp/4
%% Parameter = The parameter possision for which Test is controlled 
%%</pre> 
%% @end
%% ===========================================================================
create_ms(Action, Module, Function, Parameter, BoolFunc) 
  when is_atom(Module),
       is_atom(Function),
       Parameter > 0 ->
    case create_ms_test(stringify(BoolFunc), Action) of
	{bool_f, T2, _CR} -> 
	    Act = create_ms_test_cr(Action),
	    create_msX(bool_f, Module, Function, Parameter, T2, not_valid, Act);
	{guard_f, _T2} ->
	    io:format("Sorry, Value missing.~n",[]), error;
	_ ->
	    error
    end.
	    
%% ===========================================================================
%% @spec create_ms(Cmd, Module, Function, Parameter, GuardFunc, Value)
%%
%%       -> ok | error
%%
%% where
%%
%%   Cmd       = d | r | c | cr | s
%%   Module    = atom() 
%%   Function  = atom() | '_'
%%   Parameter = integer()
%%   GuardFunc = guard_func()
%%   Value     = integer() | atom() | tuple() | binary() | string()
%% 
%% @doc
%% Prints a function call to be used when invoking ms/3.
%% 
%% This command will not start any tracing but is a help function to
%% create match specs to be used in ms/3.
%% It will print one command for each arity of Function.
%% The print-out can be pasted into an Erlang window to start the trace.
%% 
%%<pre> 
%% Cmd       = See tp/4
%% Parameter = The position for which Test and Value are controlled 
%% Value     = Value of the Parameter
%%</pre> 
%% 
%% 
%% @end
%% ===========================================================================	
create_ms(Action, Module, Function, Parameter, GuardFunc, Value) 
  when is_atom(Module),
       is_atom(Function),
       Parameter > 0 ->
    ValueStr = stringify(Value),
    case create_ms_test(stringify(GuardFunc), ValueStr) of
	{bool_f, T2, _CR} -> 
	    Act = create_ms_test_cr(Action),
	    create_msX(bool_f, Module, Function, Parameter, T2, not_valid, Act);
	{guard_f, T2} ->
	    Act = create_ms_test_cr(Action),
	    create_msX(guard_f, Module, Function, Parameter, T2, ValueStr, Act);
	_ ->
	    error
    end.
	    
	    
create_ms_test(Test, Action)
  when Test == "is_atom";
       Test == "is_binary";
       Test == "is_constant";
       Test == "is_float";
       Test == "is_function";
       Test == "is_integer";
       Test == "is_list";
       Test == "is_number";
       Test == "is_pid";
       Test == "is_port";
       Test == "is_reference";
       Test == "is_tuple" ->
    case create_ms_test_cr(Action) of
	error -> error;
	X     -> {bool_f, Test, X}
    end;

create_ms_test(Test, Value)
     when Test == "==";
	  Test == "/=";
	  Test == "=<";
	  Test == "<";
	  Test == ">=";
	  Test == ">";
	  Test == "=:=";
	  Test == "=/=" ->
    case is_list(Value) of
	true  -> {guard_f, Test};
	false -> io:format("Sorry, Value must be a string ~p~n",[Value]),
		 error
    end;


create_ms_test("is_record", _) ->
    io:format("Sorry, 'is_record' not valid parameter.~n",[]),
    error;
create_ms_test(Test, _) ->
    io:format("Sorry, illegal parameter Test ~p~n",[Test]),
    error.
        
create_ms_test_cr(Action) ->
    case Action of 
	d  -> "ok";
	c  -> "caller()";
	r  -> "exception_trace()";
	cr -> "caller(), exception_trace()";
	_  -> 
	    io:format("Sorry, illegal Action value: ~p~n"
		      "valid values: [d | c | r | cr]",
		      [Action]), 
	      error
    end.

create_msX(_, _, _, _, _, _, error) ->
    error;
%% Not all functions are exported in ets
create_msX(Type, ets = M, lookup = F, Para, Test, Value, Action) ->
    create_msX(Type, M, F, 2, Para, Test, Value, Action),
    ok;
create_msX(Type, M, F, Para, Test, Value, Action) ->
    FL = M:module_info(functions),
    Farity = lists:foldl(fun({Fnc,Arity}, Acc) when Fnc == F -> [Arity | Acc];
			    (_, Acc) -> Acc end,
			 [],
			 FL),
    case {Farity, lists:filter(fun(X) -> X >= Para end, Farity)} of
	{[], _} -> 
	    io:format("Function '~p' not found in module '~p'~n",[F,M]);
	{_, []} ->
	    io:format("'~p:~p/~p' not found. Available arities ~p~n",
		      [M,F,Para,Farity]);
	{_, X} ->
	    [create_msX(Type, M, F, Arity, Para, Test, Value, Action)
	     || Arity <- X],
	    ok
    end.
 
create_msX(guard_f, M, F, Arity, Para, Test, Value, Action) ->
    L = create_ms_create_l(Arity),
    P = create_ms_create_p(Para),
    S = "pmsDebug:ms(" ++ atom_to_list(M) ++ ", " ++ atom_to_list(F) ++ 
	", fun([" ++ L ++ "]) when " ++ P ++ " " ++ Test ++ " " ++ Value ++
	" -> " ++ Action ++ " end).",
    io:format("~s~n",[S]);
create_msX(bool_f, M, F, Arity, Para, Test, not_valid, Action) -> 
    L = create_ms_create_l(Arity),
    P = create_ms_create_p(Para),
    S = "pmsDebug:ms(" ++ atom_to_list(M) ++ ", " ++ atom_to_list(F) ++ 
	", fun([" ++ L ++ "]) when " ++ Test ++ "(" ++ P ++ ")" ++
	" -> " ++ Action ++ " end).",
    io:format("~s~n",[S]).

create_ms_create_l(X)    ->
    create_ms_create_l(X-1, ["P", integer_to_list(X)]).
create_ms_create_l(0, L) -> 
    lists:flatten(L);
create_ms_create_l(X, L) -> 
    create_ms_create_l(X-1, "P" ++ integer_to_list(X) ++ "," ++ L).

create_ms_create_p(P) ->
    "P" ++ integer_to_list(P).


%% ===========================================================================
%% @spec log_dbg()
%%
%%       -> ok | {error, Reason}
%%
%% where
%%       Reason = term()
%% 
%% @doc
%% Interface for printing erlang dbg info in uddalog.
%% 
%% @end
%% ===========================================================================
log_dbg() ->
    stop_clear(),
    TraceFun = fun(Mess, _State) ->
		       udda_env ! {erlang_trace, Mess}
	       end,
    Res = dbg:tracer(process, {TraceFun, start}),
    ld(Res).

ld({ok,_}) ->
    dbg:p(all, [c, timestamp]),
    ok;
ld(Res) ->
    Res.


%%% ===========================================================================
%%% # ts(Boolean) ->
%% 
%% @doc
%% Toggels the timestamp printouts when debugging using tp and tpl.
%% NOTE: Will kill all tracing already started.
%% 
%% @end
%%% ===========================================================================
ts(true)  ->
    io:format("~n================================ ~n"),
    io:format("NOTE: Any old tracing was killed"),
    io:format("~n================================ ~n~n"),
    dbg:stop_clear(),

    %% start the tracer with time stamps
    dbg:tracer(process, {fun dbg_it/2, ok}),
    dbg:p(all,c),
    ok;

ts(false) -> 
    io:format("~n================================ ~n"),
    io:format("NOTE: Any old tracing was killed"),
    io:format("~n================================ ~n~n"),

    %% start the tracer without time stamps
    dbg:stop_clear(),
    dbg_init(),
    ok.








%%% ----------------------------------------------------------
%%% @doc 
%%% Filter setting for RcsPmCounter log
%%% 
%%% @end
%%% ----------------------------------------------------------
counter_filter(_) ->
    io:format("Deprecated. Use /log/ramlog.~n"),
    {error, "Deprecated~n"}.



%%% ----------------------------------------------------------
%%% @doc 
%%% set log truncate level for RcsPmCounters log
%%% i.e how many counter values will be printed in the log
%%% @end
%%% ----------------------------------------------------------
%% set default value
set_log_truncate_level() ->
    set_log_truncate_level(?LOG_MAX).

set_log_truncate_level(Level) when is_integer(Level) andalso 
				   Level >= 0 andalso
				   Level < 100 ->
    mnesia:dirty_write(pmsEnv, {pmsEnv, log_max, Level});
set_log_truncate_level(_) ->
    io:format("Level must be an integer between 0 and 99~n").


%% ===========================================================================
%% INTERNAL FUNCTIONS
%% ===========================================================================

%%=======================================================
%% Transform the call to dbg calls
%%=======================================================
dbg(DbgCmd, Command, Args) ->
    try 
	Cmd = format_cmd(Command),
	dbg_init(),
	dbg_do(DbgCmd, Cmd, Args),
	ok
    catch
	throw:{udda_error, _} -> error
    end.


%%=======================================================
%% Init dbg tracer
%%=======================================================
dbg_init() -> 
    dbgi(dbg:get_tracer()).

dbgi({error, _}) ->
    tracer(),
    dbg:p(all, [c, timestamp]);
dbgi({ok, _}) ->
    ok.


%%=======================================================
%% Execute the request
%%=======================================================
dbg_do(DbgCmd, s, Attr) -> 
    dbg_stop(is_valid_attr(Attr, DbgCmd));
dbg_do(DbgCmd, Cmd, Attr) -> 
    dbg_start(DbgCmd, is_valid_attr(Attr, DbgCmd), udda_to_dbg_form(Cmd), []).






%%% ===========================================================================
%%% # udda_to_dbg_form(Cmd) ->
%%% Input:       Cmd - atom()
%%%            
%%% Output:      [] | [DbgArg] | error
%%%              DbgArg = caller | return
%%%             
%%% Description: dbg wrapper.
%%% ===========================================================================
udda_to_dbg_form(d)  -> [];
udda_to_dbg_form(s)  -> [];
udda_to_dbg_form(c)  -> [caller];
udda_to_dbg_form(r)  -> [return];
udda_to_dbg_form(cr) -> [caller, return];
udda_to_dbg_form(X)  ->
    io:format(
      "Illegal Action ~p.~n"
      "~n"
      "~n"
      "The following Actions are allowed:~n"
      "~n"
      "debug         - start debugging;~n"
      "stop          - stop debugging;~n"
      "caller        - as 'debug' but shows also the calling function;~n"
      "return        - as 'debug' but shows also the function return value;~n"
      "caller_return - as 'debug' but shows also the calling function and~n"
      "                the function return value.~n"
      "~n"
      "There are also short notations for the actions:~n"
      "d, s, c, r, cr respectively.~n"
      "~n"
      "~n"
      "~n"
      ,[X]),
    throw({udda_error, udda_to_dbg_form}).

%%% ===========================================================================
%%% # is_valid_attr(Attr) ->
%%% Input:       Attr = [M] | [M, F] | [M, F, A]
%%%            
%%% Output:      [] | [DbgArg] | error
%%%              DbgArg = caller | return
%%%             
%%% Description: dbg wrapper.
%%% ===========================================================================
is_valid_attr([M], _)       -> iva_is_module_loaded(M);
is_valid_attr([M, F], C)    -> iva_is_valid_function(M, {F, '_'}, C);
is_valid_attr([M, F, A], C) -> iva_is_valid_function(M, {F, A}, C);
is_valid_attr(Attr, _)      -> io:format("Illegal Parameter: ~p.~n~n", [Attr]),
			       throw({udda_error, is_valid_attr}).
    



iva_is_valid_function(Mod, {Func, Arity}, C) ->
    iva_is_module_loaded(Mod),
    ModFuncs = Mod:module_info(choose(C == tp, exports, functions)),
    FindL = [choose(Arity == '_', F, {F, A}) || {F, A} <- ModFuncs],
    Find  =  choose(Arity == '_', Func, {Func, Arity}),
    iva_ivf(lists:member(Find, FindL), 
	    Mod, 
	    Func, 
	    Arity, 
	    choose(C == tp, "exported", "such")).



%% OK
iva_ivf(true, Mod, Func, Arity, _) ->
    {Mod, Func, Arity};
%% Some functions in ets are not defined in module_info
iva_ivf(false, ets, Func, Arity, _) ->
    {ets, Func, Arity};
%% Error
iva_ivf(false, Mod, Func, '_', Exports) ->
    io:format("No ~s function ~p:~p.~n",[Exports, Mod, Func]),
    throw({udda_error, is_valid_fucntion});
iva_ivf(false, Mod, Func, Arity, Exports) ->
    io:format("No ~s function/arity ~p:~p/~p.~n", 
	      [Exports, Mod, Func, Arity]),
    throw({udda_error, is_valid_function}).



iva_is_module_loaded(Mod) ->
    case module_loaded(Mod) of
	true  -> {Mod, '_', '_'};
	false -> iva_iml(code:load_file(Mod), Mod)
    end.

iva_iml({module, _},     Mod) -> {Mod, '_', '_'};
iva_iml({error, nofile}, Mod) -> io:format("Module ~p not loaded.~n",[Mod]),
				 throw({udda_error, iva_is_module_loaded}).








%%% ---------------------------------------------------------------------------
%%% # dbg_start(DbgCmd, Mods, Options, MatchSpec)
%%% Input:       Modes    - atom | List of Modules
%%%              Options  - list 
%%% Output:      
%%% Description: Start logging
%%% ---------------------------------------------------------------------------
dbg_start(DbgCmd, Mods, Opts, MatchSpec) -> 
    Caller = lists:member(caller, Opts), 
    Return = lists:member(return, Opts), 
    UnrecOpts = Opts -- ?VALID_DBG_OPT,

    start_logging(DbgCmd, 
		  UnrecOpts,
		  Mods,
		  [Caller, Return],
		  MatchSpec).


%%% ---------------------------------------------------------------------------
%%% # dbg_stop(List of Modules | Block)
%%% # dbg_stop(Block, SubBlock)
%%% Input:       
%%% Output:      
%%% Description: Stop debugging modules
%%% ---------------------------------------------------------------------------
dbg_stop({M, F, _}) -> dbgs(dbg:ctp(M, F)).


dbgs({ok, [{_ ,_, 0}]} = Res) ->
    io:format("WARNING. Nothing matched the trace.~n"),
    Res;
dbgs(Res) ->
    Res.



%%% ---------------------------------------------------------------------------
%%% # start_logging(DbgFun, UnrecOpts, ValidBlocks, Options, MatchSpecInput)
%%% Input:       UnrecOpts   - list of unrecognized options
%%%              ValidBlocks - {true, List of modules} | false
%%%              Options     - list of Options [caller, return]
%%% Output:      
%%% Description: Check the parameters to find the wanted logging
%%% ---------------------------------------------------------------------------
%% Unknown Options required
start_logging(_DbgFun, UnrecOpts, _ValidBlocks, _Opts, _MS)
 when UnrecOpts /= [] ->
    io:format("Sorry. Unrecognized Options: ~p~n", [UnrecOpts]),
    throw({udda_error, start_logging});
%% No options given
start_logging(DbgFun, [], Mods, [false, false], MSI) ->
    MS = get_match_spec(MSI, []),
    call_dbg(DbgFun, Mods, MS);
%% Option = caller
start_logging(DbgFun, [], Mods, [true, false], MSI) ->
    MS = get_match_spec(MSI, [?CALLER]),
    call_dbg(DbgFun, Mods, [MS]);
%% Option = return
start_logging(DbgFun, [], Mods, [false, true], MSI) ->
    MS = get_match_spec(MSI, [?RETURN]),
    call_dbg(DbgFun, Mods, [MS]);
%% Option = caller and return
start_logging(DbgFun, [], Mods, [true, true], MSI) ->
    MS = get_match_spec(MSI, [?RETURN, ?CALLER]),
    call_dbg(DbgFun, Mods, [MS]).



%%% ---------------------------------------------------------------------------
%%% # get_match_spec(MatchSpecHeadConds, Body)
%%% Input:       MatchSpecHeadConds - [] | {Head, Conds}
%%%              
%%% Output:      {Head, Conds, Body} | []
%%% Description: Create the match spec
%%% ---------------------------------------------------------------------------
get_match_spec([], [])              -> [];
get_match_spec([], Body)            -> {'_', [], Body}.
%get_match_spec({Head, Conds}, Body) -> {Head, Conds, Body}.


%%% ---------------------------------------------------------------------------
%%% # call_dbg(DbgFun, Mods, MatchSpec)
%%% Input:       
%%%              
%%% Output:      
%%% Description: Call dbg using the requested input
%%% ---------------------------------------------------------------------------
call_dbg(DbgFun, {Mod, Fcn, Arity}, MatchSpec) 
  when is_atom(Mod), 
       is_atom(Fcn),
       (is_integer(Arity) orelse Arity == '_'),
       is_list(MatchSpec) ->
    call_dbg(DbgFun, Mod, Fcn, Arity, MatchSpec).


call_dbg(DbgFun, Mod, Fcn, Arity, MatchSpec)
  when is_atom(Mod), is_atom(Fcn), is_list(MatchSpec) ->
%%    io:format("#### Mod ~p~n     Fcn ~p~n     Arity ~p~n     MatchSpec ~p~n",
%%	      [Mod, Fcn, Arity, MatchSpec]),
    calldbg(dbg:DbgFun(Mod, Fcn, Arity, MatchSpec), Mod).

calldbg({ok, [{_,_,0}]} = X, _) ->
    io:format("WARNING. Nothing matched the trace.~n~p~n",[X]),
    throw({udda_error, call_dbg});
calldbg({error, X}, _) ->
    io:format("ERROR. ~p~n", [X]),
    throw({udda_error, call_dbg});
calldbg({ok, X}, Mod) ->
    io:format("Trace started: ~p ~p~n", [Mod, X]),
    ok.






%% ===========================================================================
%% MISC FUNCTIONS
%% ===========================================================================


format_cmd(debug)         -> d;
format_cmd(stop)          -> s;
format_cmd(caller)        -> c;
format_cmd(return)        -> r;
format_cmd(rc)            -> cr;
format_cmd(caller_return) -> cr;
format_cmd(return_caller) -> cr;
format_cmd(Cmd)           -> Cmd.




choose(true,  T, _) -> T;
choose(false, _, F) -> F.

stringify(A) when is_atom(A)    -> atom_to_list(A);
stringify(I) when is_integer(I) -> integer_to_list(I);
stringify(T) when is_tuple(T)   -> tuple_to_list(T);
stringify(B) when is_binary(B)  -> binary_to_list(B);
stringify(S)                    -> S.
    

%%===========================================================================
%% tm_dbg(TraceMsg, State)
%% 
%%   TraceMsg = trace message from dbg
%%   State    = {{StartFunction, StopFunction, FilterFunctions}, Print}
%%   StartFunction   = atom() 
%%   StopFunction    = atom()
%%   FilterFunctions = [FilterFunction]
%%   FilterFunction  = atom()
%%   Print           = boolean()
%%   
%% Print the trace message if Print == true and the function is
%% not defined in FilterFunctions.
%% Set Print to true if F is StartFunction and false if it is StopFunction.
%%   
%%===========================================================================
tm_dbg({trace_ts, Pid, call, {M, F, A}, _TS}, 
    {{F, _, Filter} = State, _}) ->
    td_call(lists:member(F, Filter), "(~p) call ~p:~p", [Pid, M, F, A]),
    {State, true};
tm_dbg({trace_ts, Pid, call, {M, F, A}, _TS},
    {{_, _, Filter}, true} = State) ->
    td_call(lists:member(F, Filter), "(~p) call ~p:~p", [Pid, M, F, A]),
    State;
tm_dbg({trace_ts, Pid, return_from, {M, F, A}, Ret, _TS},
    {{_, F, Filter} = State, _}) ->
    td_return(lists:member(F, Filter), 
	      "(~p) returned from ~p:~p/~p -> ~n  ~p~n", 
	      [Pid, M, F, A, Ret]),
    {State, false};
tm_dbg({trace_ts, Pid, return_from, {M, F, A}, Ret, _TS}, 
    {{_, _, Filter}, true} = State) ->
    td_return(lists:member(F, Filter), 
	      "(~p) returned from ~p:~p/~p -> ~n  ~p~n",
	      [Pid, M, F, A, Ret]),
    State;
tm_dbg(_Trejs, State) ->
    State.


td_call(true, _, _) ->
    ok;
td_call(false, Str, [Pid, M, F, []]) ->
    io:format(Str ++ "().~n", [Pid, M, F]);
td_call(false, Str, [Pid, M, F, A]) ->
    StrA = string:join(lists:duplicate(length(A), "~p"), ","),
    io:format(Str ++ "(" ++ StrA ++ ").~n", [Pid, M, F] ++ A).

td_return(false, Str, Args) -> io:format(Str, Args);
td_return(_, _, _)          -> ok.
    

%% ===========================================================================
%% CODE STOLEN FROM dbg
%% ===========================================================================
%%% ---------------------------------------------------------------------------
%% tracer/0,tracer/1
%%
%% tracer(Devices) -> {ok,TracerPid} | {error,Reason}
%%
%% Devices = Device | [Device]
%% Device = user | FilePath
%% FilePath = string()
%%
%% This function is used if you want to output the trace report 
%% with record definitions.
%%
%% Code stolen, and slightly modified, from dbg.erl
%%% ---------------------------------------------------------------------------
-record(dbgParams, {devices, print_records, print_args = true}).

tracer() ->
    tracer([user]).
tracer(Devices) ->
    %% Make sure that no tracer running right now.
    IOs = lists:map(fun tracer_devices/1,
		    lists:zip(lists:seq(1, length(Devices)), Devices)),
    %% start the new tracer
    dbg:tracer(process, {fun dhandler/2, #dbgParams{devices       = IOs,
						    print_records = true,
						    print_args    = true}}).

tracer_devices({Id, user}) ->
    {Id, shell, user};
tracer_devices({Id, FileName}) ->
    {ok, FileHandle} = file:open(FileName, [write]),
    {Id, FileName, FileHandle}.




%% Code stolen and slightly modified from dbg.erl
dhandler(end_of_trace, Params) ->
    ioformat(Params, "end_of_trace\n", []),
    Params;
dhandler({trace, set_params, #dbgParams{} = Params}, _OldParams) ->
    Params;
dhandler({trace, get_params, Pid}, Params) ->
    Pid ! {ok, Params},
    Params;
dhandler(Trace, Params) 
  when element(1, Trace) == trace, tuple_size(Trace) >= 3 ->
    dhandler1(Trace, tuple_size(Trace), Params);
dhandler(Trace, Params) 
  when element(1, Trace) == trace_ts, tuple_size(Trace) >= 4 ->
    dhandler1(Trace, tuple_size(Trace) - 1, Params);
dhandler(Trace, Params)
  when element(1, Trace) == drop, tuple_size(Trace) =:= 2 ->
    ioformat(Params,
	     "*** Dropped ~s messages.~n",
	     [pretty(element(2, Trace), Params)]),
    Params;
dhandler(Trace, Params)
  when element(1, Trace) == seq_trace, tuple_size(Trace) >= 3 ->
    SeqTraceInfo = case Trace of
		       {seq_trace, Lbl, STI, TS} ->
			   ioformat(Params,
				    "SeqTrace ~s [~s]: ",
				    [pretty(TS, Params), pretty(Lbl, Params)]),
			   STI;
		       {seq_trace, Lbl, STI} ->
			   ioformat(Params, 
				    "SeqTrace [~s]: ",
				    [pretty(Lbl, Params)]),
			   STI 
		   end,
    case SeqTraceInfo of
	{send, Ser, Fr, To, Mes} ->
	    ioformat(Params,
		     "(~s) ~s ! ~s [Serial: ~s]~n",
		     [pretty(Fr,  Params),
		      pretty(To,  Params),
		      pretty(Mes, Params),
		      pretty(Ser, Params)]);
	{'receive', Ser, Fr, To, Mes} ->
	    ioformat(Params,
		     "(~s) << ~s [Serial: ~s, From: ~s]~n",
		     [pretty(To,  Params),
		      pretty(Mes, Params),
		      pretty(Ser, Params),
		      pretty(Fr,  Params)]);
	{print, Ser, Fr, _, Info} ->
	    ioformat(Params, 
		     "-> ~s [Serial: ~s, From: ~s]~n",
		     [pretty(Info, Params),
		      pretty(Ser,  Params),
		      pretty(Fr,   Params)]);
	Else ->
	    ioformat(Params, "~s~n", [pretty(Else, Params)])
    end,
    Params;
dhandler(Trace, Params) ->
    ioformat(Params, "Tracer got unknown message! ~p~n", [Trace]),
    Params.

dhandler1(Trace, Size, Params) ->
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message -> ioformat(Params,
				    "(~s) << ~s~n", 
				    [pretty(From,    Params),
				     pretty(Message, Params)])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    case element(5, Trace) of
%%%! This causes messages to disappear when used by ttb (observer). Tests
%%%! so far show that there is no difference in results with dbg even if I
%%%! comment it out, so  I hope this is only some old code which isn't
%%%! needed anymore... /siri
%%%!		Self -> ok;
		To -> ioformat(Params, 
			       "(~s) ~s ! ~s~n", 
			       [pretty(From,    Params),
				pretty(To,      Params),
				pretty(Message, Params)])
	    end;
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    ioformat(Params,
			     "(~s) call ~s (~s)~n",
			     [pretty(From,    Params),
			      ffunc(MFA,      Params),
			      pretty(Message, Params)]);
		MFA ->
		    ioformat(Params,
			     "(~s) call ~s~n",
			     [pretty(From, Params), ffunc(MFA, Params)])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    ioformat(Params, 
			     "(~s) old_ret ~s -> ~s~n",
			     [pretty(From, Params),
			      ffunc(MFA,   Params),
			      pretty(Ret,  Params)]);
		MFA ->
		    ioformat(Params,
			     "(~s) old_ret ~s~n",
			     [pretty(From, Params), ffunc(MFA, Params)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    ioformat(Params, 
		     "(~s) returned from ~s -> ~s~n",
		     [pretty(From, Params),
		      ffunc(MFA, Params),
		      pretty(Ret, Params)]);
	return_to ->
	    MFA = element(4, Trace),
	    ioformat(Params,
		     "(~s) returning to ~s~n",
		     [pretty(From, Params), ffunc(MFA, Params)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    ioformat(Params,
		     "(~s) spawn ~s as ~s~n", 
		     [pretty(From, Params),
		      pretty(Pid, Params),
		      ffunc(MFA, Params)]);
	Op ->
	    ioformat(Params,
		     "(~s) ~s ~s~n",
		     [pretty(From, Params),
		      pretty(Op, Params),
		      ftup(Trace, 4, Size, Params)])
    end,
    Params.

%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M, F, Args}, #dbgParams{print_args = false} = Params)
  when is_list(Args) ->
    ffunc({M, F, length(Args)}, Params);
ffunc({M, F, Argl},Params) when is_list(Argl) ->
    io_lib:format("~s:~s(~s)", [pretty(M, Params),
				pretty(F, Params),
				fargs(Argl, Params)]);
ffunc({M, F, Arity}, Params) ->
    io_lib:format("~s:~s/~p", [pretty(M, Params), pretty(F, Params), Arity]);
ffunc(X, Params) ->
    io_lib:format("~s", [pretty(X, Params)]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity, _Params) when is_integer(Arity) -> 
    integer_to_list(Arity);
fargs([], _Params) ->
    [];
fargs([A], Params) -> 
    io_lib:format("~s", [pretty(A, Params)]);  %% last arg
fargs([A|Args], Params) ->
    [io_lib:format("~s,", [pretty(A, Params)]) | fargs(Args, Params)];
fargs(A,Params) -> 
    io_lib:format("~s", [pretty(A,Params)]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index, Params) -> 
    io_lib:format("~s", [pretty(element(Index, Trace), Params)]);
ftup(Trace, Index, Size, Params) -> 
    [io_lib:format("~s ", [pretty(element(Index, Trace), Params)]) 
     | ftup(Trace, Index+1, Size, Params)].

pretty(Term,#dbgParams{print_records = true}) ->
    io_lib_pretty:print(Term, get_record_fun());
pretty(Term,_Params) ->
    io_lib_pretty:print(Term).

get_record_fun() ->
    record_print_fun(get_rec_table_id()).

record_print_fun(RT) ->
    fun(Tag, NoFields) ->
            case ets:lookup(RT, Tag) of
                [{_, {attribute, _, record, {Tag, Fields}}}] 
		when length(Fields) =:= NoFields ->
                    record_fields(Fields);
                _ ->
                    no
            end
    end.

record_fields([{record_field, _, {atom, _, Field}} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([{record_field, _, {atom, _, Field}, _} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([]) ->
    [].

get_rec_table_id() ->
    get_rec_table_id(ets:all()).
get_rec_table_id([Id|T]) ->
    case ets:info(Id,name) of
 	shell_records -> Id;
 	_Else         -> get_rec_table_id(T)
    end;
get_rec_table_id([]) ->
    undefined.

ioformat(#dbgParams{devices = Devs}, String, Args) ->
    [catch io:format(Dev, String, Args) || {_Id, _MetaData, Dev} <- Devs].



%%% ----------------------------------------------------------
%%% # attrs(Modules|Prefix, Attr|FormatFun)
%%%
%%% Output: Number of modules listed.
%%%
%%% Description: List an attribute from module_info.
%%% ----------------------------------------------------------

%% attr/1

attr(T) when is_atom(T) ->
    atom_to_list(T);
attr(N) when is_integer(N) ->
    integer_to_list(N);
attr(V) ->
    case is_list(V) andalso lists:all(fun is_char/1, V) of
        true ->  %% string
            V;
        false ->
            io_lib:format("~p", [V])
    end.

%% attr/4

attr(Width, Mod, Attr, VFun) ->
    io:format(": ~*s~s~n", [-Width, Mod, attr(Mod, Attr, VFun)]).

attr(Mod, Attr, VFun) ->
    Key = key(Attr),
    try
        VFun(val(Attr, keyfetch(Attr, Mod:module_info(Key))))
    catch
        _:_ ->
            "-"
    end.

attr(Mod, Attr) ->
    attr(Mod, Attr, fun attr/1).

key(time) -> compile;
key(_)    -> attributes.

val(time, {_,_,_,_,_,_} = T) ->
    lists:flatten(io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                tuple_to_list(T)));
val(_, [V]) ->
    V.


%%% ----------------------------------------------------------
%%% # modules(Prefix|Prefixes)
%%%
%%% Input: Prefix = atom()
%%%
%%% Description: Return the list of all loaded modules with the
%%%              specified prefix.
%%% ----------------------------------------------------------

modules(Prefix)
  when is_atom(Prefix) ->
    lists:sort(mods(Prefix));
modules(Prefixes)
  when is_list(Prefixes) ->
    lists:sort(lists:flatmap(fun modules/1, Prefixes)).

mods(Prefix) ->
    P = atom_to_list(Prefix),
    lists:filter(fun(M) ->
                         lists:prefix(P, atom_to_list(M))
                 end,
                 erlang:loaded()).


%% keyfetch/2

keyfetch(Key, List) ->
    {Key,V} = lists:keyfind(Key, 1, List),
    V.




dbg_it({trace, Pid, T, {M, F, A}}, _) ->
    begin
	io:format("~n~s  ~p ~p ~p:~p(", [get_time(), Pid, T, M, F]),
	dbg_it_p(A)
    end;
dbg_it({trace, _Pid, return_from, {M ,F, A}, Res}, _) ->
    io:format("~n~s  returned from ~p:~p/~p -> ~n~p~n", 
	      [get_time(), M, F, A, Res]);
dbg_it({trace, _Pid, exception_from, {M ,F, A}, Res}, _) ->
    io:format("~n***** ~s  EXCEPTION from *****~n~p:~p/~p -> ~n~p~n", 
	      [get_time(), M, F, A, Res]);
dbg_it(T, _) -> 
    io:format("===== ~s  ~n~p~n", [get_time(), T]).

    
dbg_it_p([])    -> io:format("~n");
dbg_it_p([X])   -> io:format("~p).~n",[X]);
dbg_it_p([X|T]) -> io:format("~p,",   [X]), dbg_it_p(T).

    

%%========================================================================
%% fragment() -> ok
%%
%% print PM fragment
%%========================================================================
fragment() ->
    io:format("~n   PM - ~p~n",[{"1", "1", "1"}]),
    {ok, PmGrps} = pmsDb:pm_group_match({"1", "1", "1"}),
    [pm_grp(PmGrp) || PmGrp <- PmGrps],
    io:format("~n"),
    {ok, PmJobs} = pmsDb:pm_job_match({"1", "1", "1"}),
    [pm_job(PmJob) || PmJob <- PmJobs],
    io:format("~n~n"),
    ok.

pm_grp(#pmGroup{pmGroupId = Id}) ->
    io:format("      PmGroup - ~p~n", [Id]),
    {ok, MTs} = pmsDb:measurement_type_match(Id),
    [mt(MT) || MT <- MTs],
    ok.

mt(#measurementType{measurementTypeId = Id}) ->
    io:format("         MeasurementType - ~p~n", [Id]).


pm_job(#pmJob{pmJobId = Id}) ->
    io:format("      PmJob - ~p~n", [Id]),
    {ok, PJs} = pmsDb:measurement_reader_match(Id),
    [mr(PJ) || PJ <- PJs],
    ok.

mr(#measurementReader{measurementReaderId      = Id,
		      measurementSpecification = Spec}) ->
    #'MeasurementSpecification'{groupRef           = GrpRef,
				measurementTypeRef = MtRef} = Spec,
    io:format("         MeasurementReader - ~p~n", [Id]),
    case GrpRef of
	undefined ->
	    ok;
	_ -> 
	    io:format("            GroupRef    - ~p~n", 
		       [binary_to_list(GrpRef)])
    end,
    case MtRef of
	undefined -> 
	    ok;
	_ ->
	    io:format("            MeasTypeRef - ~p~n",
		      [binary_to_list(MtRef)])
    end.
    

%%========================================================================
%% procs() -> ok
%%
%% print PM fragment
%%========================================================================
procs() ->
    procs(erlang:whereis(pmsServer)).

procs(Server) when is_pid(Server) ->
    io:format("~n=========================~n"),
    LD      = gen_server:call(Server, get_loop_data),
    GrpIds2 = proplists:get_value(job_groups, LD),
    Gids2   = maps:fold(fun get_job_group_data/3, [], GrpIds2),

    io:format("~n =====  Server  =====  ~p~n", [Server]),
    PP = [begin
	      io:format("   GroupId: ~p~n"
		    "      GP     = ~p~n"
		    "      JobIds = ~p~n", 
		    [Gid, GP, Jid]), 
	      P
	  end
	  || {Gid, GP, Jid, P} <- Gids2],
    [procs_pm_grp(Pid, Server) || Pid <- PP],

    io:format("~n=========================~n"),
    io:format("~n"),

    AppRegPid = erlang:whereis(pmsAppRegistry),
    io:format("~n =====  AppReg  =====  ~p~n", [AppRegPid]),
    AppRegs = ets:tab2list(pmsAppRegistry),
    proc_app_reg(AppRegs),
    ok;
procs(_) ->
    io:format("~n   PMS Server not found. ~n").


procs_pm_grp(GrpPid, Server) ->
    {MonitorsAll, GrpId} = get_monitors(GrpPid, pmsJobGroup),

    Monitors = [M || {process, Pid} = M <- MonitorsAll, Pid /= Server],

    LD = proc_loop(GrpPid),
    
    GrpId       = proplists:get_value(group_id,        LD), 
    RP          = proplists:get_value(rp,              LD), 
    Jobs        = proplists:get_value(pm_jobs,         LD), 
    Grps        = proplists:get_value(pm_groups,       LD), 
    Add         = proplists:get_value(pm_groups_add,   LD), 
    Rem         = proplists:get_value(pm_groups_rem,   LD), 
    MiPids      = proplists:get_value(mi_pids,         LD), 
    JgPids      = proplists:get_value(jg_pids,         LD), 
    ToJobs      = proplists:get_value(to_jobs,         LD), 
    Timer       = proplists:get_value(timer_ref,       LD), 
    RopTab      = proplists:get_value(rop_tab,         LD), 
    BegTime     = proplists:get_value(rop_begin_time,  LD), 
    Mi2Com      = proplists:get_value(mi_2_common,     LD), 
    Sw          = proplists:get_value(sw_data,         LD), 
    {Server, _} = proplists:get_value(server,          LD),
    FSP         = proplists:get_value(file_state_prev, LD),
    FSN         = proplists:get_value(file_state_new,  LD),
    MiExp       = proplists:get_value(n_exp_meas_info, LD),
    JgExp       = proplists:get_value(n_exp_job_group, LD),
    Deleted     = proplists:get_value(deleted,         LD),
    FM          = proplists:get_value(file_method,     LD),
    
    io:format("~n    =====  PmsJobGroup ~p  =====~n"
	      "            GroupId = ~p~n"
	      "            RP      = ~p (~s)~n"
	      "            Jobs    = ~p~n"
	      "            Grps    = ~p~n"
	      "            Add     = ~p~n"
	      "            Rem     = ~p~n"
	      "            MiPids  = ~p~n"
	      "            JgPids  = ~p~n"
	      "            ToJobs  = ~p~n"
	      "            Timer   = ~p~n"
	      "            BegTime = ~p~n"
	      "            RopTab  = ~p~n"
	      "            Mi2Com  = ~p~n"
	      "            FileM   = ~p~n"
	      "            Sw      = ~p~n"
	      "            Server  = ~p~n"
	      "            FSP     = ~p~n"
	      "            FSN     = ~p~n"
	      "            MiExp   = ~p~n"
	      "            JgExp   = ~p~n"
	      "            Deleted = ~p~n",
	      [GrpPid, GrpId, RP, p_period(RP), 
	       [J || {J, _} <- Jobs],
	       [G || {G, _} <- Grps],
	       [A || {A, _} <- Add],
	       [R || {R, _} <- Rem],
	       MiPids, JgPids, ToJobs,
	       Timer, BegTime, 
	       RopTab, 
	       Mi2Com,
	       FM,
	       Sw, 
	       Server, 
	       FSP, FSN, 
	       MiExp, JgExp, 
	       Deleted]),
    
    remain(LD, [group_id, 
		rp,
		pm_jobs,
		pm_groups,
		mi_pids,
		jg_pids,
		to_jobs,
		pm_groups_add,
		pm_groups_rem,
		timer_ref,
		rop_tab,
		rop_begin_time,
		mi_2_common,
		file_method,
		sw_vsn,
		server,
		deleted,
		file_state_prev,
		file_state_new,
		n_exp_meas_info,
		n_exp_job_group]),
    
    [procs_pm_job(Pid) || {process, Pid} <- Monitors].



procs_pm_job(JobPid) ->
    case get_monitors(JobPid, pmsJob) of
	not_same_module -> ok;
	_               -> pmj(proc_loop(JobPid), JobPid)
    end.

pmj(LD, JobPid) ->
    JobId    = proplists:get_value(job_id,         LD), 
    CurState = proplists:get_value(job_state,      LD), 
    ReqState = proplists:get_value(req_state,      LD), 
    MeasTab  = proplists:get_value(meas_tab,       LD), 
    GP       = proplists:get_value(gp,             LD), 
    RP       = proplists:get_value(rp,             LD), 
    Timer    = proplists:get_value(timer_ref,      LD), 
    Delta    = proplists:get_value(cspec_gp_delta, LD), 
    JobGroup = proplists:get_value(job_group,      LD),
    AppPids  = proplists:get_value(app_pids,       LD),
    Deleted  = proplists:get_value(deleted,        LD),
    GpEnd    = proplists:get_value(gp_end,         LD),
    PreGpEnd = proplists:get_value(prev_gp_end,    LD),
    AppJobs  = proplists:get_value(n_app_jobs,     LD),
    NoPmGrps = proplists:get_value(n_pm_groups,    LD),
    Slaves   = proplists:get_value(mi_slave_tab,   LD),
    
    io:format("~n       =====  PmsJob ~p  =====~n"
	      "               JobId     = ~p~n"
	      "               CurState  = ~p ~s~n"
	      "               ReqState  = ~p ~s~n"
	      "               MeasTab   = ~p~n"
	      "               GP        = ~p ~s~n"
	      "               RP        = ~p ~s~n"
	      "               Timer     = ~p~n"
	      "               CS_delta  = ~p~n"
	      "               JobGroup  = ~p~n"
	      "               AppPids   = ~p~n"
	      "               Deleted   = ~p~n"
	      "               GpEnd     = ~p~n"
	      "               PrevGpEnd = ~p~n"
	      "               NoAppJobs = ~p~n"
	      "               NoPmGrps  = ~p~n"
	      "               Slaves    = ~p~n",
	      [JobPid, JobId, CurState, p_state(CurState),
	       ReqState, p_state(ReqState), MeasTab, GP, p_period(GP), 
	       RP, p_period(RP), 
	       Timer, Delta, JobGroup, AppPids, Deleted, GpEnd, PreGpEnd, 
	       AppJobs, NoPmGrps, Slaves]),

    remain(LD, [job_id, 
		job_state,
		req_state,
		meas_tab,
		gp,
		rp,
		timer_ref,
		cspec_gp_delta,
		job_group,
		app_pids,
		deleted,
		gp_end,
		prev_gp_end,
		n_app_jobs,
		n_exp_meas_data,
		n_pm_groups,
		mi_slave_tab
	       ]),

    [procs_pm_mi(Pid) || {_Name, Pid} <- Slaves].


procs_pm_mi(JobMiPid) ->
    case get_monitors(JobMiPid, pmsJobMeasInfo) of
	not_same_module -> ok;
	_               -> pmmi(proc_loop(JobMiPid), JobMiPid)
    end.


pmmi(LD, JobMiPid) ->
    JobId    = proplists:get_value(job_id,      LD), 
    Parent   = proplists:get_value(parent,      LD), 
    PmGrp    = proplists:get_value(pm_group,    LD), 
    PmGrpDn  = proplists:get_value(pm_group_dn, LD), 
    GP       = proplists:get_value(gp,          LD), 
    RP       = proplists:get_value(rp,          LD), 
    GpEnd    = proplists:get_value(gp_end,      LD), 
    MeasTab  = proplists:get_value(meas_tab,    LD), 
    MeasVals = proplists:get_value(meas_vals,   LD),
    MeasInfo = proplists:get_value(meas_info,   LD),
    ProcType = proplists:get_value(proc_type,   LD),
    
    io:format("~n         =====  PmsJobMeasInfo ~p  =====~n"
	      "                 JobId        = ~p~n"
	      "                 Parent       = ~p~n"
	      "                 PmGroup      = ~p~n"
	      "                 PmGroupDn    = ~p~n"
	      "                 GP           = ~p ~s~n"
	      "                 RP           = ~p ~s~n"
	      "                 GpEnd        = ~p~n"
	      "                 MeasTab      = ~p~n"
	      "                 MeasValsLen  = ~p~n"
	      "                 MeasInfoSize = ~p~n"
	      "                 ProcType     = ~p~n",
	      [
	       JobMiPid,
	       JobId,
	       Parent,
	       PmGrp,
	       PmGrpDn,
	       GP, p_period(GP), 
	       RP, p_period(RP), 
	       GpEnd,
	       MeasTab,
	       length(MeasVals),
	       size(iolist_to_binary([MeasInfo])),
	       ProcType
	      ]),

    remain(LD, [job_id, 
		parent,
		pm_group,
		pm_group_dn,
		gp,
		rp,
		gp_end,
		meas_tab,
		meas_vals,
		meas_info,
		proc_type
	       ]).




p_state(?JobState_ACTIVE)  -> "(ACTIVE)"; 
p_state(?JobState_STOPPED) -> "(STOPPED)";
p_state(_)                 -> "".

p_period(?TimePeriod_ONE_MIN)        -> "(ONE_MIN)";
p_period(?TimePeriod_FIVE_MIN)       -> "(FIVE_MIN)";
p_period(?TimePeriod_FIFTEEN_MIN)    -> "(FIFTEEN_MIN)";
p_period(?TimePeriod_THIRTY_MIN)     -> "(THIRTY_MIN)";
p_period(?TimePeriod_ONE_HOUR)       -> "(ONE_HOUR)";
p_period(?TimePeriod_TWELVE_HOUR)    -> "(TWELVE_HOUR)";
p_period(?TimePeriod_ONE_DAY)        -> "(ONE_DAY)";
p_period(?TimePeriod_TEN_SECONDS)    -> "(TEN_SEC)";
p_period(?TimePeriod_THIRTY_SECONDS) -> "(THIRTY_SEC)";
p_period(_)                          -> "".



proc_app_reg(AppRegs) ->
    Pids = [JobPid || {pmsAppRegistry, _, JobPid, _, _} <- AppRegs],
    par(lists:usort(Pids)).

par([]) ->
    ok;
par([JobPid | T]) ->

    LD = proc_loop_host(JobPid),

    Grps     = proplists:get_value(pm_groups,    LD),
    Cb       = proplists:get_value(pmi_cb,       LD),
    SessPid  = proplists:get_value(session_pid,  LD),
    Subs     = proplists:get_value(subscribers,  LD),
    Meas     = proplists:get_value(measurements, LD),
    ComplGp  = proplists:get_value(complete_gp,  LD),
    Timers   = proplists:get_value(gp_timers,    LD),
    EndTime  = proplists:get_value(gp_end_time,  LD),
    ReqId    = proplists:get_value(req_id,       LD),
    ScReqs   = proplists:get_value(sc_reqs,      LD),
    CbPmi2   = proplists:get_value(pmi2_cb,      LD),
    CbMod    = proplists:get_value(pmi2_cb_mod,  LD),
    CouMap   = proplists:get_value(counter_map,  LD),
    AliasTab = proplists:get_value(alias_tab,    LD),
    Prot     = proplists:get_value(protocol,     LD),
    ProcType = proplists:get_value(proc_type,    LD),

    io:format("~n    =====  AppJob ~p  =====~n"
	      "         PmGroups   = ~p~n"
	      "         PmiCb      = ~p~n"
	      "         SessPid    = ~p~n"
	      "         Subscr     = ~p~n"
	      "         Measures   = ~p~n"
	      "         CompleteGp = ~p~n"
	      "         Timers     = ~p~n"
	      "         EndTime    = ~p~n"
	      "         ReqId      = ~p~n"
	      "         ScReqs     = ~p~n"
	      "         Pim2Cb     = ~p~n"
	      "         CbMod      = ~p~n"
	      "         CounterMap = ~p~n"
	      "         Proto      = ~p~n"
	      "         AliasTab   = ~p~n"
	      "         ProcType   = ~p~n",
	      [JobPid, Grps, Cb, SessPid, Subs, Meas, ComplGp, Timers, 
	       EndTime, ReqId, ScReqs, CbPmi2, CbMod, CouMap, Prot, AliasTab,
	       ProcType]),

    remain(LD, [pm_groups,
		pmi_cb,
		session_pid,
		subscribers,
		measurements,
		complete_gp,
		gp_timers,
		gp_end_time,
		req_id,
		sc_reqs,
	        pmi2_cb,
	        pmi2_cb_mod,
	        counter_map,
		protocol,
		alias_tab,
	        callbacks,
	        cb_mod,
	        proc_type]),
    
    par(T).
    

%% -record(loop, {pm_groups    = [],  %% [PmGroupId]
%% 	       pmi_cb,
%% 	       session_pid,
%% 	       subscribers  = [],  %% [{{JobPid, GP}, {CounterSpec, OldCounterSpec}}]
%% 	       measurements = [],  %% []
%% 	       gp_timers    = [],
%% 	       gp_end_time  = [],
%% 	       req_id       = 0,
%% 	       sc_reqs      = []}).

get_monitors(Pid, Module) ->
    X = erlang:process_info(Pid, [monitors, dictionary]),
%%       io:format("~n ----- ~p ~p ----- ~n",[Module, Pid]),   
%%       io:format("~n   monitors: ~p ~n",[X]),
    Dict = proplists:get_value(dictionary, X),
%%      io:format("~n   dict: ~p ~n",[proplists:get_value(name, Dict)]),
	case proplists:get_value(name, Dict) of
	    {Module, Name} ->
		{proplists:get_value(monitors, X), Name};
	    _ ->
		not_same_module
	end.

proc_loop(Pid) ->
    Pid ! {get_loop_data, self()},
    receive
	{loop_data, Loop} -> Loop
    after 5000 ->
	    timeout
    end.

proc_loop_host(Pid) ->
    pmsAppJob:gld(Pid, self()),
    receive
	{loop_data, Loop} -> Loop
    after 5000 ->
	    timeout
    end.


%%========================================================================
%% tables(Tabs) -> ok
%% 
%% Tabs = [atom()]
%%
%% print table contents of the tables in Tabs.
%%========================================================================
tables() ->
    tables([pmJob, 
	    measurementReader, 
	    pmsAppsInfo, 
	    pmsAppRegistry, 
	    pmsScMoClasses,
	    pmsScAppMoLdns]).

tables(Tabs) ->
    io:format("~n"),
    [table(Tab, ets:tab2list(Tab), true) || Tab <- Tabs],
    ok.



table(Tab, [], Print) ->
    table_name(Tab, Print),
    io:format("~n");
table(Tab, [H|T], Print) ->
    [_ | List] = tuple_to_list(H),
    table_name(Tab, Print),
    Fields = table_fields(Tab),
    L = max_length(Fields, 0),
    [io:format("  ~-*s :  ~p~n", [L, K, V]) || {K, V} <- lists:zip(Fields, List)],
    case T of
	[] -> io:format("~n");
	_  -> io:format("  ----------~n")
    end,
    table(Tab, T, false).


table_name(Tab, true) ->
    io:format("=== ~p ===~n", [Tab]);
table_name(_, _) ->   
    ok.

table_fields(pmJob)             -> record_info(fields, pmJob);
table_fields(measurementReader) -> record_info(fields, measurementReader);
table_fields(pmsAppsInfo)       -> record_info(fields, pmsAppsInfo);
table_fields(pmsAppRegistry)    -> record_info(fields, pmsAppRegistry);
table_fields(pmsScMoClasses)    -> record_info(fields, pmsScMoClasses);
table_fields(pmsScAppMoLdns)    -> record_info(fields, pmsScAppMoLdns);
table_fields(_)                 -> unknown_record.
    

max_length([], L) ->
    L;
max_length([H|T], L) ->
    case length(atom_to_list(H)) of
	GR when GR > L -> max_length(T, GR);
	_              -> max_length(T, L)
    end.

remain(LD, List) ->
    rem_p(lists:foldl(fun(K,A) -> proplists:delete(K,A) end, LD, List)).
    
rem_p([])  -> ok;
rem_p(Rem) -> io:format("Remaining loop data: ~p~n", [Rem]).



%%========================================================================
%% aliases() -> ok
%% 
%% print pm group aliases 
%%========================================================================
aliases() ->
    aliases(ets:tab2list(pmsAppRegistry)).

aliases(E) ->
    Pids = [{Pid, Grp} || #pmsAppRegistry{pm_group = Grp, job_pid = Pid} <- E],
    io:format("--- ~p~n", [Pids]),
    aliases_p(asort(Pids, [])).



aliases_p([]) ->
    ok;
aliases_p([{Pid, Grps} | T]) ->
    Pid ! {get_aliases, self()},
    receive 
	{aliases_data, Data} ->
	    io:format("Pid    ~p~nGroups ~p~n", [Pid, Grps]),
	    ap([S || {Int, _} = S    <- Data, is_integer(Int)],
	       [S || {{_, _}, _} = S <- Data])
    after 3000 -> 
	    io:format("aliases timeout ~p~n", [Pid])
    end,
    aliases_p(T).


ap([], _MTs) ->
    ok;
ap([{Alias, GrpId} = Grp | T], MTs) ->
    io:format("  ~p ~p~n", [GrpId, Alias]),
    {_, MTsRem} = lists:foldl(fun ap_mt/2, {Grp, []}, MTs),
    ap(T, MTsRem).
    
    
ap_mt({{GA, MtA}, {GrpId, MtId}}, {{GA, GrpId}, _} = Acc) ->
    io:format("        ~p ~p~n", [MtId, MtA]),
    Acc;
ap_mt(MT, {Grp, Acc}) ->
    {Grp, [MT | Acc]}.
    


asort([], Acc) ->
    Acc;
asort([{Pid, Grp} | T], Acc) ->
    case lists:keytake(Pid, 1, Acc) of
	{value, {Pid, Grps}, RestAcc} ->
	    asort(T, [{Pid, [Grp | Grps]} | RestAcc]);
	_ ->
	    asort(T, [{Pid, [Grp]} | Acc])
    end.


%%========================================================================
%% get_aliases() -> ok
%% 
%% get pm group aliases 
%%========================================================================
get_aliases() ->
    get_aliases(ets:tab2list(pmsAppRegistry)).

get_aliases(E) ->
    Pids = [{Pid, Grp} || #pmsAppRegistry{pm_group = Grp, job_pid = Pid} <- E],
    ga(asort(Pids, []), []).

ga([], Acc) ->
    Acc;
ga([{Pid, _Grps} | T], Acc) ->
    Pid ! {get_aliases, self()},
    Res = receive 
	      {aliases_data, Data} ->
		  {Pid, Data}
    after 3000 -> 
	    {Pid, timeout}
    end,
    ga(T, [Res | Acc]).



%% ===========================================================================
%% @spec memory() 
%%
%%       -> ok
%% 
%% @doc
%% Print PMS memory usage.
%% @end
%% ===========================================================================	
memory() -> 
    memory(disc),
    memory(procs). 



memory(disc) -> 
    EcimStr = a2s(?ECIM_TABS),
    IntStr  = a2s(?INTERNAL_TABS),

    MaxName  = lists:foldl(fun get_max_len/2, 0, EcimStr ++ IntStr),    
    EcimName = mem_fix_left(EcimStr, MaxName),
    IntName  = mem_fix_left(IntStr,  MaxName),

    EcimMem = [mnesia:table_info(T, memory) || T <- ?ECIM_TABS],
    IntMem  = [mnesia:table_info(T, memory) || T <- ?INTERNAL_TABS],
    EcimTot = lists:sum(EcimMem),
    IntTot  = lists:sum(IntMem),
    
    MaxSize  = lists:foldl(fun get_max_len/2, 0, i2s([EcimTot, IntTot])),    
    EcimSize = mem_fix_right(i2s(EcimMem), MaxSize),
    IntSize  = mem_fix_right(i2s(IntMem),  MaxSize),
    
    EcimIO  = mem_format(EcimName, EcimSize), 
    IntIO   = mem_format(IntName,  IntSize), 
    
    io:format("=========================================================~n"
	      " ~s~n"
	      "=========================================================~n"
	      " ECIM MNESIA TABLES                                      ~n"
	      "=========================================================~n"
	      "~s"
	      "~s~n"
	      "=========================================================~n"
	      " INTERNAL MNESIA TABLES                                  ~n"
	      "=========================================================~n"
	      "~s"
	      "~s~n",
	     [get_time(),
	      EcimIO,
	      tot_format(EcimTot, MaxName + MaxSize),
	      IntIO,
	      tot_format(IntTot, MaxName + MaxSize)
	     ]);
memory(procs) ->
    Pids = get_all_procs(),
    try
	memory_procs(get_proc_info(Pids))
    catch 
	error:_ ->
	    memory_error()
    end.

memory_error() ->
    io:format("~n"
	      "=========================================================~n"
	      " PROCSESS INFO                                           ~n"
	      "=========================================================~n"
	      " Timeout while fetching process info~n"
	      "=========================================================~n").


memory_procs({Server, JobGrps, Jobs, AppReg, AppJobs, Sessions}) ->
    WordSize = erlang:system_info(wordsize),
    io:format("~n"
	      "=========================================================~n"
	      " PROCSESS INFO                                      ~n"
	      "=========================================================~n"
	      "                 LoopSize  HeapSize  MsgQueue  Reductions~n"
	      "---------------------------------------------------------~n"
	      "~s"
	      "---------------------------------------------------------~n"
	      "~s"
	      "---------------------------------------------------------~n"
	      "~s"
	      "---------------------------------------------------------~n"
	      "~s"
	      "---------------------------------------------------------~n"
	      "~s"
	      "---------------------------------------------------------~n"
	      "~s"
	      "=========================================================~n"
	      "~n~n"
	      "=========================================================~n"
	      " PROCSESS ETS TABLES                                     ~n"
	      "=========================================================~n"
	      "~s"
	      "---------------------------------------------------------~n"
	      "~s"
	      "---------------------------------------------------------~n"
	      "~s"
	      "=========================================================~n"
	      "~n",
	     [
	      mem_server(Server, WordSize),
	      mem_job_grp(JobGrps, WordSize),
	      mem_job(Jobs, WordSize),
	      mem_app_reg(AppReg, WordSize),
	      mem_app_job(AppJobs, WordSize),
	      mem_session(Sessions, WordSize),

	      mem_job_grp_ets(JobGrps),
	      mem_job_ets(Jobs),
	      mem_app_job_ets(AppJobs)
	     ]).


mem_server({server, Data}, WS) ->
    mem_pi_print({"Server", Data}, WS).

mem_job_grp([], _WS) ->
    "no job groups\n";
mem_job_grp(JobGrps, WS) ->
    [mem_pi_print({choose(Name == undefined, "common", Name), Data}, WS)
     || {Name, Data} <- JobGrps].

mem_job([], _WS) ->
    "no jobs\n";
mem_job(Jobs, WS) ->
    [mem_pi_print({Name, Data}, WS) || {Name, Data} <- Jobs].

mem_app_reg({app_reg, Data}, WS) ->
    mem_pi_print({"AppReg", Data}, WS).

mem_app_job([], _WS) ->
    "no app jobs\n";
mem_app_job(AppJobs, WS) ->
    [mem_pi_print({"AppJob", Data}, WS) 
     || {app_job, Data} <- AppJobs].

mem_session([], _WS) ->
    "no sessions\n";
mem_session(Sessions, WS) ->
    [mem_pi_print({choose(Session == session, "Session", "Session2"),
		   Data}, 
		  WS) 
		  || {Session, Data} <- Sessions].

mem_pi_print({Name, Data}, WS) ->
    Loop = proplists:get_value(loop_size, Data),
    Info = proplists:get_value(proc_info, Data),

    Name ++
	mem_fix_right([i2s(Loop * WS)], 25 - length(Name)) ++
	mem_fix_right([i2s(proplists:get_value(total_heap_size, Info))], 10) ++
	mem_fix_right([i2s(proplists:get_value(message_queue_len, Info))], 10) ++
	mem_fix_right([i2s(proplists:get_value(reductions, Info))], 12) ++
	"\n".
	
    

mem_job_grp_ets([]) ->
    "no job groups\n";
mem_job_grp_ets(JobGrps) ->
    [mem_ets_print({"JobGroup: ", choose(Name == undefined, "common", Name), Data})
     || {Name, Data} <- JobGrps].

mem_job_ets([]) ->
    "no jobs\n";
mem_job_ets(Jobs) ->
    [mem_ets_print({"PmJob: ", Name, Data}) || {Name, Data} <- Jobs].


mem_app_job_ets([]) ->
    "no app jobs\n";
mem_app_job_ets(AppJobs) ->
    [mem_ets_print({"AppJob", "", Data}) || {app_job, Data} <- AppJobs].


mem_ets_print({What, Name, Data}) ->
    io_lib:format("~s~s", [What, Name]) ++
    mem_ep(proplists:get_value(ets_tables, Data)).


mem_ep(undefined) ->
    "";
mem_ep([]) ->
    "";
mem_ep(Ets) ->
    Str = ["\n  " ++ mem_fix_left([a2s(Tab)], 10) ++ 
	   " = " ++
	   mem_fix_right([i2s(Size)], 10) || {Tab, Size} <- Ets],
    Str ++ "\n".
	




a2s(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
a2s(Vals) ->
    [atom_to_list(V)    || V <- Vals].

i2s(Int) when is_integer(Int) ->
    integer_to_list(Int);
i2s(Vals) ->
    [integer_to_list(V) || V <- Vals].


mem_fix_left(List, Len) ->
    [string:left(L, Len) || L <- List].

mem_fix_right(List, Len) ->
    [string:right(L, Len) || L <- List].


get_max_len(L, Max) when length(L) > Max -> length(L);
get_max_len(_, Max) -> Max.


mem_format(Str, Attr) ->
    mem_format(Str, Attr, []).

mem_format([], [], Acc) ->
    lists:flatten(lists:reverse(Acc));
mem_format([Str | S], [Attr | A], Acc) ->
    mem_format(S, A, [io_lib:format(Str ++ " = ~s\n", [Attr]) | Acc]).

tot_format(Size, Len) ->
    Dash  = string:copies("-", Len + 3 ),
    Total = mem_fix_left(["TOTAL"], Len - 4),
    lists:flatten(io_lib:format(Dash ++ "\n" ++
				Total ++ " = ~p\n",
				[Size])).



get_proc_info({Server, JobGrps, Jobs, AppReg, AppJobs, Sessions}) ->
    gpi_flush(),
    {gpi_rc(gpi_gen_server(Server), 100),
     [gpi_rc(pmsJobGroup:get_proc_info(P), 100) || P <- JobGrps],
     [gpi_rc(pmsJob:get_proc_info(P), 100)      || P <- Jobs],
     gpi_rc(gpi_gen_server(AppReg), 100),
     [gpi_rc(pmsAppJob:get_proc_info(P), 100)        || P <- AppJobs],
     [gpi_rc(gpi_gen_server(S), 100) || S <- Sessions]}.

    
gpi_gen_server(Server) ->
    try
	gen_server:call(Server, get_proc_info)
    catch
	_:_ ->
	    {error, {gen_server, timeout}}
    end.
    

gpi_rc({error, _}, N) when N > 0 -> 
    gpi_wait();
gpi_rc(Res, _) ->
    Res.

gpi_wait() ->
    receive
	Msg -> Msg
    after 5000 ->
	    {error, {gpi_wait, timeout}}
    end.
	    
	      

gpi_flush() ->
    receive
	Msg -> 
	    io:format("=========================================================~n"
		      " Flushed message ~n~p~n" 
		      "=========================================================~n",
		      [Msg]),
	    gpi_flush()
    after 0 ->
	    ok
    end.
	    
 

 




get_all_procs() ->
    get_all_procs(erlang:whereis(pmsServer)).

get_all_procs(Server) when is_pid(Server) ->
    ServerLD = gen_server:call(Server, get_loop_data),
    JgIds  = proplists:get_value(job_groups, ServerLD),
    JgPids = [P || {_,_,_,P} <- maps:fold(fun get_job_group_data/3, [], JgIds)],

    JgMon     = [get_monitors(Jg, pmsJobGroup) || Jg <- JgPids],
    JgMonPids = [M || {M, _} <- JgMon],
    JPids     = lus(lf([[P || {_, P} <- L, P /= Server] || L <- JgMonPids])),

    AppRegPid  = erlang:whereis(pmsAppRegistry),
    AppRegLD   = gen_server:call(AppRegPid, get_loop_data),
    AppJobPids = [P || {P, {app_job, _, _}} <- AppRegLD],
    SesionPids = [P || {P, {session, _, _}} <- AppRegLD],

    {Server, JgPids, JPids, AppRegPid, AppJobPids, SesionPids};
get_all_procs(_) ->
    {error, {server, not_found}}.


get_job_group_data({Gid, GP}, {Pid, _, JobIds},  Acc) ->
    [{Gid, GP, JobIds, Pid} | Acc].




lus(List) -> lists:usort(List).
lf(List)  -> lists:flatten(List).
    


%%===========================================================================
%% get_time(GP) -> string()
%% 
%% get a printable time
%%===========================================================================
get_time() ->
    Now = {_, _, MS} = os:timestamp(),
    get_time(calendar:now_to_local_time(Now), MS).
get_time({{Y, M, D}, {H, Mi, S}}, MS) ->
    lists:append([integer_to_list(Y), "-",
		  gt_zero(M),
		  integer_to_list(M), "-",
		  gt_zero(D),
		  integer_to_list(D), "  ",
		  gt_zero(H),
		  integer_to_list(H), ":",
		  gt_zero(Mi),
		  integer_to_list(Mi), ":",
		  gt_zero(S),
		  integer_to_list(S), ".",
		  gt_zero_ms(MS),
		  integer_to_list(MS)]).
 
gt_zero(X) when X < 10 -> "0";
gt_zero(_)             -> "".
    
gt_zero_ms(X) when X < 10     -> "00000";
gt_zero_ms(X) when X < 100    -> "0000";
gt_zero_ms(X) when X < 1000   -> "000";
gt_zero_ms(X) when X < 10000  -> "00";
gt_zero_ms(X) when X < 100000 -> "0";
gt_zero_ms(_)                 -> "".
    



%%% ----------------------------------------------------------
%%% # get_attributes(Modules|Prefix, Attr|FormatFun)
%%%
%%% Output: Number of modules listed.
%%%
%%% Description: List an attribute from module_info.
%%% ----------------------------------------------------------

get_attributes(Modules, Fun)
  when is_list(Modules) ->
    sep(),
    W = 2 + widest(Modules),
    N = lists:foldl(fun(M,A) -> Fun(W,M), A+1 end, 0, Modules),
    sep(),
    N;
get_attributes(Prefix, Fun) ->
    get_attributes(modules(Prefix), Fun).



%% sep/[01]

sep() ->
    sep($#).

sep(Ch) ->
    io:format("~c~65c~n", [Ch, $-]).

%% widest/1

widest(List) ->
    lists:foldl(fun widest/2, 0, List).

widest(T, Max)
  when is_atom(T) ->
    widest(atom_to_list(T), Max);

widest(T, Max)
  when is_integer(T) ->
    widest(integer_to_list(T), Max);

widest(T, Max)
  when is_list(T) ->  %% string
    max(length(T), Max).

